{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemTable.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDEMemTable;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  BDE, DB, DBTables;

type
  TJvBDEMemoryTable = class(TDBDataSet)
  private
    FTableName: TFileName;
    FMoveHandle: HDBICur;
    FEnableDelete: Boolean;
    FDisableEvents: Boolean;
    procedure EncodeFieldDesc(var FieldDesc: FLDDesc;
      const Name: string; DataType: TFieldType; Size, Precision: Word);
    procedure SetTableName(const Value: TFileName);
    function SupportedFieldType(AType: TFieldType): Boolean;
    procedure DeleteCurrentRecord;
  protected
    function CreateHandle: HDBICur; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalDelete; override;
  public
    constructor Create(AOwner: TComponent); override;
    function BatchMove(ASource: TDataSet; AMode: TBatchMode;
      ARecordCount: Longint): Longint;
    procedure CopyStructure(ASource: TDataSet);
    procedure CreateTable;
    procedure DeleteTable;
    procedure EmptyTable;
    procedure GotoRecord(RecordNo: Longint);
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    procedure SetFieldValues(const FieldNames: array of string;
      const Values: array of const);
  published
    property EnableDelete: Boolean read FEnableDelete write FEnableDelete default True;
    property TableName: TFileName read FTableName write SetTableName;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  BDEConst, DBConsts, Math,
  JvDBUtils, JvBdeUtils;

const
 InternalMemTableName1 = '$InMem$';
 InternalMemTableName2 = '$JvInMem$';

{ Memory tables are created in RAM and deleted when you close them. They
  are much faster and are very useful when you need fast operations on
  small tables. Memory tables do not support certain features (like
  deleting records, referntial integrity, indexes, autoincrement fields
  and BLOBs) }

constructor TJvBDEMemoryTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableDelete := True;
end;

function TJvBDEMemoryTable.BatchMove(ASource: TDataSet; AMode: TBatchMode;
  ARecordCount: Longint): Longint;
var
  SourceActive: Boolean;
  MovedCount: Longint;
begin
  if (ASource = nil) or (Self = ASource) or
    not (AMode in [batCopy, batAppend]) then
    _DBError(SInvalidBatchMove);
  SourceActive := ASource.Active;
  try
    ASource.DisableControls;
    DisableControls;
    ASource.Open;
    ASource.CheckBrowseMode;
    ASource.UpdateCursorPos;
    if AMode = batCopy then
    begin
      Close;
      CopyStructure(ASource);
    end;
    if not Active then
      Open;
    CheckBrowseMode;
    if ARecordCount > 0 then
    begin
      ASource.UpdateCursorPos;
      MovedCount := ARecordCount;
    end
    else
    begin
      ASource.First;
      MovedCount := MaxLongint;
    end;
    try
      Result := 0;
      while not ASource.Eof do
      begin
        Append;
        AssignRecord(ASource, Self, True);
        Post;
        Inc(Result);
        if Result >= MovedCount then
          Break;
        ASource.Next;
      end;
    finally
      Self.First;
    end;
  finally
    if not SourceActive then
      ASource.Close;
    Self.EnableControls;
    ASource.EnableControls;
  end;
end;

procedure TJvBDEMemoryTable.CopyStructure(ASource: TDataSet);
var
  I: Integer;

  procedure CreateField(FieldDef: TFieldDef; AOwner: TComponent);
  begin
    FieldDef.CreateField(AOwner, nil, FieldDef.Name, True);
  end;

begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do
    Fields[I].Free;
  if ASource = nil then
    Exit;
  ASource.FieldDefs.Update;
  FieldDefs := ASource.FieldDefs;
  for I := 0 to FieldDefs.Count - 1 do
    if SupportedFieldType(FieldDefs.Items[I].DataType) then
    begin
      if (csDesigning in ComponentState) and (Owner <> nil) then
        CreateField(FieldDefs.Items[I], Owner)
      else
        CreateField(FieldDefs.Items[I], Self);
    end;
end;

procedure TJvBDEMemoryTable.DeleteCurrentRecord;
var
  CurRecNo, CurRec: Longint;
  Buffer: Pointer;
  iFldCount: Word;
  FieldDescs: PFLDDesc;
begin
  CurRecNo := RecNo;
  iFldCount := FieldDefs.Count;
  FieldDescs := AllocMem(iFldCount * SizeOf(FLDDesc));
  try
    Check(DbiGetFieldDescs(Handle, FieldDescs));
    Check(DbiCreateInMemTable(DBHandle, InternalMemTableName1, iFldCount, FieldDescs,
      FMoveHandle));
    try
      DisableControls;
      Buffer := AllocMem(RecordSize);
      try
        First;
        CurRec := 0;
        while not Self.Eof do
        begin
          Inc(CurRec);
          if CurRec <> CurRecNo then
          begin
            DbiInitRecord(FMoveHandle, Buffer);
            Self.GetCurrentRecord(Buffer);
            Check(DbiAppendRecord(FMoveHandle, Buffer));
          end;
          Self.Next;
        end;
        FDisableEvents := True;
        try
          Close;
          Open;
          FMoveHandle := nil;
        finally
          FDisableEvents := False;
        end;
      finally
        FreeMem(Buffer, RecordSize);
      end;
    except
      DbiCloseCursor(FMoveHandle);
      FMoveHandle := nil;
      raise;
    end;
    GotoRecord(CurRecNo - 1);
  finally
    if FieldDescs <> nil then
      FreeMem(FieldDescs, iFldCount * SizeOf(FLDDesc));
    FMoveHandle := nil;
    EnableControls;
  end;
end;


function TJvBDEMemoryTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PChar;
begin
  Result := inherited GetFieldData(Field, Buffer);
  if not Result then
  begin
    RecBuf := nil;
    case State of
      dsBrowse:
        if not IsEmpty then
          RecBuf := ActiveBuffer;
      dsEdit, dsInsert:
        RecBuf := ActiveBuffer;
      dsCalcFields:
        RecBuf := CalcBuffer;
    end;
    if RecBuf = nil then
      Exit;
    with Field do
      if FieldNo > 0 then
      begin
        Check(DbiGetField(Handle, FieldNo, RecBuf, nil, IsBlank));
        Result := not IsBlank;
      end;
  end;
end;

procedure TJvBDEMemoryTable.InternalDelete;
begin
  if EnableDelete then
    DeleteCurrentRecord
  else
    inherited InternalDelete;
end;

function TJvBDEMemoryTable.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then
  begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TJvBDEMemoryTable.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := False;
end;


procedure TJvBDEMemoryTable.DoAfterClose;
begin
  if not FDisableEvents then
    inherited DoAfterClose;
end;

procedure TJvBDEMemoryTable.DoAfterOpen;
begin
  if not FDisableEvents then
    inherited DoAfterOpen;
end;

procedure TJvBDEMemoryTable.DoBeforeClose;
begin
  if not FDisableEvents then
    inherited DoBeforeClose;
end;

procedure TJvBDEMemoryTable.DoBeforeOpen;
begin
  if not FDisableEvents then
    inherited DoBeforeOpen;
end;


procedure TJvBDEMemoryTable.DoBeforeScroll;
begin
  if not FDisableEvents then
    inherited DoBeforeScroll;
end;

procedure TJvBDEMemoryTable.DoAfterScroll;
begin
  if not FDisableEvents then
    inherited DoAfterScroll;
end;

function TJvBDEMemoryTable.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := not (AType in [ftUnknown, ftWideString, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid] + ftNonTextTypes);
end;

function TJvBDEMemoryTable.CreateHandle: HDBICur;
var
  I: Integer;
  FldDescList: TFieldDescList;
  FieldDescs: PFLDDesc;
  iFldCount: Cardinal;
  szTblName: DBITBLNAME;
begin
  if (FMoveHandle <> nil) then
  begin
    Result := FMoveHandle;
    Exit;
  end;
  if FieldCount > 0 then
    FieldDefs.Clear;
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do
    begin
      if not SupportedFieldType(Fields[I].DataType) then
        DatabaseErrorFmt(SUnknownFieldType, [Fields[I].FieldName]);
      with Fields[I] do
        if not (Calculated or Lookup) then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
  iFldCount := FieldDefs.Count;
  SetDBFlag(dbfTable, True);
  try
    if TableName = '' then
      AnsiToNative(Locale, InternalMemTableName1, szTblName, SizeOf(szTblName) - 1)
    else
      AnsiToNative(Locale, TableName, szTblName, SizeOf(szTblName) - 1);
    SetLength(FldDescList, iFldCount);
    FieldDescs := BDE.PFLDDesc(FldDescList);
    for I := 0 to FieldDefs.Count - 1 do
      with FieldDefs[I] do
        EncodeFieldDesc(FldDescList[I], Name, DataType, Size, Precision);
    Check(DbiTranslateRecordStructure(nil, iFldCount, FieldDescs, nil, nil,
      FieldDescs, False));
    Check(DbiCreateInMemTable(DBHandle, szTblName, iFldCount, FieldDescs,
      Result));
  finally
    SetDBFlag(dbfTable, False);
  end;
end;

procedure TJvBDEMemoryTable.CreateTable;
begin
  CheckInactive;
  Open;
end;

procedure TJvBDEMemoryTable.DeleteTable;
begin
  CheckBrowseMode;
  Close;
end;

procedure TJvBDEMemoryTable.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    DisableControls;
    FDisableEvents := True;
    try
      Close;
      Open;
    finally
      FDisableEvents := False;
      EnableControls;
    end;
  end;
end;

procedure TJvBDEMemoryTable.EncodeFieldDesc(var FieldDesc: FLDDesc;
  const Name: string; DataType: TFieldType; Size, Precision: Word);
begin
  with FieldDesc do
  begin
    FillChar(szName, SizeOf(szName), 0);
    AnsiToNative(Locale, Name, szName, SizeOf(szName) - 1);
    iFldType := FieldLogicMap(DataType);
    iSubType := FieldSubtypeMap(DataType);
    if iSubType = fldstAUTOINC then
      iSubType := 0;
    case DataType of
      ftString, ftFixedChar, ftBytes, ftVarBytes, ftBlob..ftTypedBinary:
        iUnits1 := Size;
      ftBCD:
        begin
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then
            iUnits1 := Precision
          else
            iUnits1 := 32;
          iUnits2 := Size; {Scale}
        end;
    end;
  end;
end;

function TJvBDEMemoryTable.GetRecordCount: Integer;
begin
  if State = dsInactive then
    _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(Handle, Result));
end;

procedure TJvBDEMemoryTable.SetRecNo(Value: Integer);
var
  Rslt: DBIResult;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Rslt := DbiSetToSeqNo(Handle, Value);
  if Rslt = DBIERR_EOF then
    Last
  else
  if Rslt = DBIERR_BOF then
    First
  else
  begin
    Check(Rslt);
    Resync([rmExact, rmCenter]);
  end;
end;

function TJvBDEMemoryTable.GetRecNo: Integer;
var
  Rslt: DBIResult;
begin
  Result := -1;
  if State in [dsBrowse, dsEdit] then
  begin
    UpdateCursorPos;
    Rslt := DbiGetSeqNo(Handle, Result);
    if (Rslt <> DBIERR_EOF) and (Rslt <> DBIERR_BOF) then
      Check(Rslt);
  end;
end;

procedure TJvBDEMemoryTable.GotoRecord(RecordNo: Longint);
begin
  RecNo := RecordNo;
end;

function TJvBDEMemoryTable.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

procedure TJvBDEMemoryTable.SetFieldValues(const FieldNames: array of string;
  const Values: array of const);
var
  I: Integer;
  Pos: Longint;
begin
  Pos := RecNo;
  DisableControls;
  try
    First;
    while not Eof do
    begin
      Edit;
      for I := 0 to Max(High(FieldNames), High(Values)) do
        FieldByName(FieldNames[I]).AssignValue(Values[I]);
      Post;
      Next;
    end;
    GotoRecord(Pos);
  finally
    EnableControls;
  end;
end;

procedure TJvBDEMemoryTable.SetTableName(const Value: TFileName);
begin
  CheckInactive;
  FTableName := Value;
  DataEvent(dePropertyChange, 0);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

