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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvMemTable;

interface

uses SysUtils, Classes, Controls, {$IFDEF WIN32} Bde, {$ELSE} DbiTypes,
  DbiProcs, DbiErrs, {$ENDIF} DB, DBTables;

type

{ TJvMemoryTable }

  TJvMemoryTable = class(TDBDataSet)
  private
    FTableName: TFileName;
    FMoveHandle: HDBICur;
    FEnableDelete: Boolean;
    FDisableEvents: Boolean;
    procedure EncodeFieldDesc(var FieldDesc: FLDDesc;
      const Name: string; DataType: TFieldType; Size
      {$IFDEF COMPILER4_UP}, Precision {$ENDIF}: Word);
    procedure SetTableName(const Value: TFileName);
    function SupportedFieldType(AType: TFieldType): Boolean;
    procedure DeleteCurrentRecord;
  protected
    function CreateHandle: HDBICur; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
{$IFDEF COMPILER3_UP}
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;
{$ENDIF}
{$IFDEF WIN32}
    function GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE}
      Integer; override {$ENDIF};
{$ENDIF}
{$IFDEF COMPILER3_UP}
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalDelete; override;
{$ELSE}
    procedure DoBeforeDelete; override;
    function GetRecordNumber: Longint; {$IFNDEF VER80} override; {$ENDIF}
    procedure SetRecNo(Value: Longint);
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    function BatchMove(ASource: TDataSet; AMode: TBatchMode;
      ARecordCount: Longint): Longint;
    procedure CopyStructure(ASource: TDataSet);
    procedure CreateTable;
    procedure DeleteTable;
    procedure EmptyTable;
    procedure GotoRecord(RecordNo: Longint);
{$IFDEF COMPILER3_UP}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
{$ENDIF}
    procedure SetFieldValues(const FieldNames: array of string;
      const Values: array of const);
{$IFNDEF COMPILER3_UP}
{$IFNDEF VER80}
    property RecordCount: Longint read GetRecordCount;
{$ENDIF}
{$ENDIF}
{$IFNDEF COMPILER3_UP}
    property RecNo: Longint read GetRecordNumber write SetRecNo;
{$ENDIF}
  published
    property EnableDelete: Boolean read FEnableDelete write FEnableDelete
      default True;
    property TableName: TFileName read FTableName write SetTableName;
  end;

implementation

uses DBConsts, JvDBUtils, JvBdeUtils, {$IFDEF COMPILER3_UP} BDEConst, {$ENDIF} 
  Forms, JvMaxMin;

{ Memory tables are created in RAM and deleted when you close them. They
  are much faster and are very useful when you need fast operations on
  small tables. Memory tables do not support certain features (like
  deleting records, referntial integrity, indexes, autoincrement fields
  and BLOBs) }

{ TJvMemoryTable }

constructor TJvMemoryTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnableDelete := True;
end;

function TJvMemoryTable.BatchMove(ASource: TDataSet; AMode: TBatchMode;
  ARecordCount: Longint): Longint;
var
  SourceActive: Boolean;
  MovedCount: Longint;
begin
  if (ASource = nil) or (Self = ASource) or
    not (AMode in [batCopy, batAppend]) then _DBError(SInvalidBatchMove);
  SourceActive := ASource.Active;
  try
    ASource.DisableControls;
    DisableControls;
    ASource.Open;
    ASource.CheckBrowseMode;
    ASource.UpdateCursorPos;
    if AMode = batCopy then begin
      Close;
      CopyStructure(ASource);
    end;
    if not Active then Open;
    CheckBrowseMode;
    if ARecordCount > 0 then begin
      ASource.UpdateCursorPos;
      MovedCount := ARecordCount;
    end
    else begin
      ASource.First;
      MovedCount := MaxLongint;
    end;
    try
      Result := 0;
      while not ASource.EOF do begin
        Append;
        AssignRecord(ASource, Self, True);
        Post;
        Inc(Result);
        if Result >= MovedCount then Break;
        ASource.Next;
      end;
    finally
      Self.First;
    end;
  finally
    if not SourceActive then ASource.Close;
    Self.EnableControls;
    ASource.EnableControls;
  end;
end;

procedure TJvMemoryTable.CopyStructure(ASource: TDataSet);

  procedure CreateField(FieldDef: TFieldDef; AOwner: TComponent);
  begin
{$IFDEF COMPILER4_UP}
    FieldDef.CreateField(AOwner, nil, FieldDef.Name, True);
{$ELSE}
    FieldDef.CreateField(AOwner);
{$ENDIF}
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do Fields[I].Free;
  if (ASource = nil) then Exit;
  ASource.FieldDefs.Update;
  FieldDefs := ASource.FieldDefs;
  for I := 0 to FieldDefs.Count - 1 do begin
    if SupportedFieldType(FieldDefs.Items[I].DataType) then begin
      if (csDesigning in ComponentState) and (Owner <> nil) then
        CreateField(FieldDefs.Items[I], Owner)
      else
        CreateField(FieldDefs.Items[I], Self);
    end;
  end;
end;

procedure TJvMemoryTable.DeleteCurrentRecord;
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
    Check(DbiCreateInMemTable(DBHandle, '$InMem$', iFldCount, FieldDescs,
      FMoveHandle));
    try
      DisableControls;
      Buffer := AllocMem(RecordSize);
      try
        First;
        CurRec := 0;
        while not Self.EOF do begin
          Inc(CurRec);
          if CurRec <> CurRecNo then begin
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

{$IFDEF COMPILER3_UP}

function TJvMemoryTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  IsBlank: LongBool;
  RecBuf: PChar;
begin
  Result := inherited GetFieldData(Field, Buffer);
  if not Result then begin
    RecBuf := nil;
    case State of
      dsBrowse: if not IsEmpty then RecBuf := ActiveBuffer;
      dsEdit, dsInsert: RecBuf := ActiveBuffer;
      dsCalcFields: RecBuf := CalcBuffer;
    end;
    if RecBuf = nil then Exit;
    with Field do
      if (FieldNo > 0) then begin
        Check(DbiGetField(Handle, FieldNo, RecBuf, nil, IsBlank));
        Result := not IsBlank;
      end;
  end;
end;

procedure TJvMemoryTable.InternalDelete;
begin
  if EnableDelete then DeleteCurrentRecord
  else inherited;
end;

function TJvMemoryTable.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;

function TJvMemoryTable.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := False;
end;

{$ELSE}

procedure TJvMemoryTable.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  if EnableDelete then begin
    DeleteCurrentRecord;
    DoAfterDelete;
    SysUtils.Abort;
  end;
end;

{$ENDIF}

procedure TJvMemoryTable.DoAfterClose;
begin
  if not FDisableEvents then inherited DoAfterClose;
end;

procedure TJvMemoryTable.DoAfterOpen;
begin
  if not FDisableEvents then inherited DoAfterOpen;
end;

procedure TJvMemoryTable.DoBeforeClose;
begin
  if not FDisableEvents then inherited DoBeforeClose;
end;

procedure TJvMemoryTable.DoBeforeOpen;
begin
  if not FDisableEvents then inherited DoBeforeOpen;
end;

{$IFDEF COMPILER3_UP}

procedure TJvMemoryTable.DoBeforeScroll;
begin
  if not FDisableEvents then inherited DoBeforeScroll;
end;

procedure TJvMemoryTable.DoAfterScroll;
begin
  if not FDisableEvents then inherited DoAfterScroll;
end;

{$ENDIF}

function TJvMemoryTable.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result := not (AType in [ftUnknown {$IFDEF COMPILER4_UP}, ftWideString {$ENDIF}
    {$IFDEF COMPILER5_UP}, ftOraBlob, ftOraClob, ftVariant, ftInterface, 
    ftIDispatch, ftGuid {$ENDIF}] + ftNonTextTypes);
end;

function TJvMemoryTable.CreateHandle: HDBICur;
var
  I: Integer;
{$IFDEF COMPILER4_UP}
  FldDescList: TFieldDescList;
  FieldDescs: PFLDDesc;
{$ELSE}
  FieldDescs: PFLDDesc;
{$ENDIF}
  iFldCount: Cardinal;
  szTblName: DBITBLNAME;
begin
  if (FMoveHandle <> nil) then begin
    Result := FMoveHandle;
    Exit;
  end;
  if FieldCount > 0 then FieldDefs.Clear;
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do begin
      if not SupportedFieldType(Fields[I].DataType) then
{$IFDEF COMPILER3_UP}
 {$IFDEF COMPILER4_UP}
        DatabaseErrorFmt(SUnknownFieldType, [Fields[I].FieldName]);
 {$ELSE}
        DatabaseErrorFmt(SFieldUnsupportedType, [Fields[I].FieldName]);
 {$ENDIF}
{$ELSE}
        DBErrorFmt(SFieldUnsupportedType, [Fields[I].FieldName]);
{$ENDIF}
      with Fields[I] do
        if not (Calculated {$IFDEF WIN32} or Lookup {$ENDIF}) then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
{$IFNDEF COMPILER4_UP}
  FieldDescs := nil;
{$ENDIF}
  iFldCount := FieldDefs.Count;
  SetDBFlag(dbfTable, True);
  try
    if TableName = '' then
      AnsiToNative(Locale, '$RxInMem$', szTblName, SizeOf(szTblName) - 1)
    else
      AnsiToNative(Locale, TableName, szTblName, SizeOf(szTblName) - 1);
{$IFDEF COMPILER4_UP}
    SetLength(FldDescList, iFldCount);
    FieldDescs := BDE.PFLDDesc(FldDescList);
{$ELSE}
    FieldDescs := AllocMem(iFldCount * SizeOf(FLDDesc));
{$ENDIF}
    for I := 0 to FieldDefs.Count - 1 do begin
      with FieldDefs[I] do
{$IFDEF COMPILER4_UP}
        EncodeFieldDesc(FldDescList[I], Name, DataType, Size, Precision);
{$ELSE}
        EncodeFieldDesc(PFieldDescList(FieldDescs)^[I], Name, DataType, Size);
{$ENDIF}
    end;
    Check(DbiTranslateRecordStructure(nil, iFldCount, FieldDescs, nil, nil,
      FieldDescs {$IFDEF WIN32}, False {$ENDIF}));
    Check(DbiCreateInMemTable(DBHandle, szTblName, iFldCount, FieldDescs,
      Result));
  finally
{$IFNDEF COMPILER4_UP}
    if FieldDescs <> nil then FreeMem(FieldDescs, iFldCount * SizeOf(FLDDesc));
{$ENDIF}
    SetDBFlag(dbfTable, False);
  end;
end;

procedure TJvMemoryTable.CreateTable;
begin
  CheckInactive;
  Open;
end;

procedure TJvMemoryTable.DeleteTable;
begin
  CheckBrowseMode;
  Close;
end;

procedure TJvMemoryTable.EmptyTable;
begin
  if Active then begin
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

procedure TJvMemoryTable.EncodeFieldDesc(var FieldDesc: FLDDesc;
  const Name: string; DataType: TFieldType; Size
  {$IFDEF COMPILER4_UP}, Precision {$ENDIF}: Word);
begin
  with FieldDesc do begin
    FillChar(szName, SizeOf(szName), 0);
    AnsiToNative(Locale, Name, szName, SizeOf(szName) - 1);
    iFldType := FieldLogicMap(DataType);
    iSubType := FieldSubtypeMap(DataType);
{$IFDEF WIN32}
    if iSubType = fldstAUTOINC then iSubType := 0;
{$ENDIF WIN32}
    case DataType of
{$IFDEF COMPILER4_UP}
      ftString, ftFixedChar, ftBytes, ftVarBytes, ftBlob..ftTypedBinary:
{$ELSE}
      ftString, ftBytes, ftVarBytes, ftBlob, ftMemo, ftGraphic
      {$IFDEF WIN32}, ftFmtMemo, ftParadoxOle, ftDBaseOle,
      ftTypedBinary {$ENDIF}:
{$ENDIF}
        iUnits1 := Size;
      ftBCD:
        begin
{$IFDEF COMPILER4_UP}
          { Default precision is 32, Size = Scale }
          if (Precision > 0) and (Precision <= 32) then iUnits1 := Precision
          else iUnits1 := 32;
{$ELSE}
          iUnits1 := 32;
{$ENDIF}
          iUnits2 := Size;  {Scale}
        end;
    end;
  end;
end;

{$IFDEF WIN32}
function TJvMemoryTable.GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE} Integer {$ENDIF};
begin
  if State = dsInactive then _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(Handle, Result));
end;
{$ENDIF WIN32}

procedure TJvMemoryTable.SetRecNo(Value: {$IFDEF COMPILER3_UP} Integer {$ELSE} Longint {$ENDIF});
var
  Rslt: DBIResult;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  Rslt := DbiSetToSeqNo(Handle, Value);
  if Rslt = DBIERR_EOF then Last
  else if Rslt = DBIERR_BOF then First
  else begin
    Check(Rslt);
    Resync([rmExact, rmCenter]);
  end;
end;

{$IFDEF COMPILER3_UP}
function TJvMemoryTable.GetRecNo: Integer;
{$ELSE}
function TJvMemoryTable.GetRecordNumber: Longint;
{$ENDIF}
var
  Rslt: DBIResult;
begin
  Result := -1;
  if State in [dsBrowse, dsEdit] then begin
    UpdateCursorPos;
    Rslt := DbiGetSeqNo(Handle, Result);
    if (Rslt = DBIERR_EOF) or (Rslt = DBIERR_BOF) then Exit
    else Check(Rslt);
  end;
end;

procedure TJvMemoryTable.GotoRecord(RecordNo: Longint);
begin
  RecNo := RecordNo;
end;

{$IFDEF COMPILER3_UP}
function TJvMemoryTable.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;
{$ENDIF COMPILER3_UP}

procedure TJvMemoryTable.SetFieldValues(const FieldNames: array of string;
  const Values: array of const);
var
  I: Integer;
  Pos: Longint;
begin
  Pos := RecNo;
  DisableControls;
  try
    First;
    while not EOF do begin
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

procedure TJvMemoryTable.SetTableName(const Value: TFileName);
begin
  CheckInactive;
  FTableName := Value;
  DataEvent(dePropertyChange, 0);
end;

end.
