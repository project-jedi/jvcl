{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvMemDS.PAS, released on 2002-07-04.

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

unit JvMemDS;

interface

{$IFDEF COMPILER3_UP}

uses
  SysUtils, Classes, DB,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvDBUtils;

type
  TMemBlobData = string;
  TMemBlobArray = array [0..0] of TMemBlobData;
  PMemBlobArray = ^TMemBlobArray;
  TJvMemoryRecord = class;
  TLoadMode = (lmCopy, lmAppend);
  TSaveLoadState = (slsNone, slsLoading, slsSaving);
  TCompareRecords = function(Item1, Item2: TJvMemoryRecord): Integer of object;

  TJvMemoryData = class(TDataSet)
  private
    FSaveLoadState: TSaveLoadState;
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBookmarkOfs: Integer;
    FBlobOfs: Integer;
    FRecBufSize: Integer;
    FOffsets: PWordArray;
    FLastID: Integer;
    FAutoInc: Longint;
    FActive: Boolean;
    FRecords: TList;
    FIndexList: TList;
    FCaseInsensitiveSort: Boolean;
    FDescendingSort: Boolean;
    FAutoIncField: TField;
    FSrcAutoIncField: TField;
    function AddRecord: TJvMemoryRecord;
    function InsertRecord(Index: Integer): TJvMemoryRecord;
    function FindRecordID(ID: Integer): TJvMemoryRecord;
    procedure CreateIndexList(const FieldNames: string);
    procedure FreeIndexList;
    procedure QuickSort(L, R: Integer; Compare: TCompareRecords);
    procedure Sort;
    function CalcRecordSize: Integer;
    function FindFieldData(Buffer: Pointer; Field: TField): Pointer;
    function GetMemoryRecord(Index: Integer): TJvMemoryRecord;
    function GetCapacity: Integer;
    function RecordFilter: Boolean;
    procedure SetCapacity(Value: Integer);
    procedure ClearRecords;
    procedure InitBufferPointers(GetProps: Boolean);
    procedure FixReadOnlyFields(MakeReadOnly: boolean);
  protected
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;
    procedure AssignMemoryRecord(Rec: TJvMemoryRecord; Buffer: PChar);
    function GetActiveRecBuf(var RecBuf: PChar): Boolean; virtual;
    procedure InitFieldDefsFromFields;
    procedure RecordToBuffer(Rec: TJvMemoryRecord; Buffer: PChar);
    procedure SetMemoryRecordData(Buffer: PChar; Pos: Integer); virtual;
    procedure SetAutoIncFields(Buffer: PChar); virtual;
    function CompareRecords(Item1, Item2: TJvMemoryRecord): Integer; virtual;
    function GetBlobData(Field: TField; Buffer: PChar): TMemBlobData;
    procedure SetBlobData(Field: TField; Buffer: PChar; Value: TMemBlobData);
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    {$IFNDEF COMPILER5_UP}
    function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean; override;
    function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
      Decimals: Integer): Boolean; override;
    {$ENDIF}
    procedure InternalInitRecord(Buffer: PChar); override;
    procedure ClearCalcFields(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure CloseBlob(Field: TField); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InitRecord(Buffer: PChar); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure OpenCursor(InfoQuery: Boolean); override;
    function IsCursorOpen: Boolean; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    property Records[Index: Integer]: TJvMemoryRecord read GetMemoryRecord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetCurrentRecord(Buffer: PChar): Boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure SortOnFields(const FieldNames: string;
      {$IFDEF COMPILER4_UP}
      CaseInsensitive: Boolean = True; Descending: Boolean = False);
      {$ELSE}
      CaseInsensitive, Descending: Boolean);
      {$ENDIF}
    procedure EmptyTable;
    procedure CopyStructure(Source: TDataSet; UseAutoIncAsInteger: boolean = false);
    function LoadFromDataSet(Source: TDataSet; RecordCount: Integer;
      Mode: TLoadMode;DisableAllControls:boolean=true): Integer;
    function SaveToDataSet(Dest: TDataSet; RecordCount: Integer;DisableAllControls:boolean=true): Integer;
    property SaveLoadState: TSaveLoadState read FSaveLoadState;
  published
    property Capacity: Integer read GetCapacity write SetCapacity default 0;
    property Active;
    property AutoCalcFields;
    property Filtered;
    {$IFDEF COMPILER4_UP}
    property FieldDefs;
    property ObjectView default False;
    {$ENDIF}
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TJvMemBlobStream = class(TStream)
  private
    FField: TBlobField;
    FDataSet: TJvMemoryData;
    FBuffer: PChar;
    FMode: TBlobStreamMode;
    FOpened: Boolean;
    FModified: Boolean;
    FPosition: Longint;
    FCached: Boolean;
    function GetBlobSize: Longint;
    function GetBlobFromRecord(Field: TField): TMemBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

  TJvMemoryRecord = class(TPersistent)
  private
    FMemoryData: TJvMemoryData;
    FID: Integer;
    FData: Pointer;
    FBlobs: Pointer;
    function GetIndex: Integer;
    procedure SetMemoryData(Value: TJvMemoryData; UpdateParent: Boolean);
  protected
    procedure SetIndex(Value: Integer); virtual;
  public
    constructor Create(MemoryData: TJvMemoryData); virtual;
    constructor CreateEx(MemoryData: TJvMemoryData; UpdateParent: Boolean); virtual;
    destructor Destroy; override;
    property MemoryData: TJvMemoryData read FMemoryData;
    property ID: Integer read FID write FID;
    property Index: Integer read GetIndex write SetIndex;
    property Data: Pointer read FData;
  end;

{$ENDIF COMPILER3_UP}

implementation

{$IFDEF COMPILER3_UP}

uses
  Forms, DbConsts, Math;

resourcestring
  SMemNoRecords = 'No data found';
  {$IFNDEF COMPILER4_UP}
  SInvalidFields = 'No fields defined';
  {$ENDIF}

const
  ftBlobTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary {$IFDEF COMPILER5_UP}, ftOraBlob, ftOraClob {$ENDIF}];

  ftSupported = [ftString, ftSmallint, ftInteger, ftWord, ftBoolean, ftFloat,
    ftCurrency, ftDate, ftTime, ftDateTime, ftAutoInc, ftBCD, ftBytes,
    ftVarBytes {$IFDEF COMPILER4_UP}, ftADT, ftFixedChar, ftWideString,
  ftLargeint {$ENDIF} {$IFDEF COMPILER5_UP}, ftVariant, ftGuid {$ENDIF}] +
  ftBlobTypes;

  fkStoredFields = [fkData];

  {$IFDEF COMPILER5_UP}
  GuidSize = 38;
  {$ENDIF}

{ Utility routines }

function CompareFields(Data1, Data2: Pointer; FieldType: TFieldType;
  CaseInsensitive: Boolean): Integer;
begin
  Result := 0;
  case FieldType of
    ftString:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftSmallint:
      if SmallInt(Data1^) > SmallInt(Data2^) then
        Result := 1
      else
      if SmallInt(Data1^) < SmallInt(Data2^) then
        Result := -1;
    ftInteger, ftDate, ftTime, ftAutoInc:
      if Longint(Data1^) > Longint(Data2^) then
        Result := 1
      else
      if Longint(Data1^) < Longint(Data2^) then
        Result := -1;
    ftWord:
      if Word(Data1^) > Word(Data2^) then
        Result := 1
      else
      if Word(Data1^) < Word(Data2^) then
        Result := -1;
    ftBoolean:
      if WordBool(Data1^) and not WordBool(Data2^) then
        Result := 1
      else
      if not WordBool(Data1^) and WordBool(Data2^) then
        Result := -1;
    ftFloat, ftCurrency:
      if Double(Data1^) > Double(Data2^) then
        Result := 1
      else
      if Double(Data1^) < Double(Data2^) then
        Result := -1;
    ftDateTime:
      if TDateTime(Data1^) > TDateTime(Data2^) then
        Result := 1
      else
      if TDateTime(Data1^) < TDateTime(Data2^) then
        Result := -1;
    {$IFDEF COMPILER4_UP}
    ftFixedChar:
      if CaseInsensitive then
        Result := AnsiCompareText(PChar(Data1), PChar(Data2))
      else
        Result := AnsiCompareStr(PChar(Data1), PChar(Data2));
    ftWideString:
      if CaseInsensitive then
        Result := AnsiCompareText(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)))
      else
        Result := AnsiCompareStr(WideCharToString(PWideChar(Data1)),
          WideCharToString(PWideChar(Data2)));
    ftLargeint:
      if Int64(Data1^) > Int64(Data2^) then
        Result := 1
      else
      if Int64(Data1^) < Int64(Data2^) then
        Result := -1;
    {$ENDIF}
    {$IFDEF COMPILER5_UP}
    ftVariant:
      Result := 0;
    ftGuid:
      Result := AnsiCompareText(PChar(Data1), PChar(Data2));
    {$ENDIF}
  end;
end;

function CalcFieldLen(FieldType: TFieldType; Size: Word): Word;
begin
  if not (FieldType in ftSupported) then
    Result := 0
  else
  if FieldType in ftBlobTypes then
    Result := SizeOf(Longint)
  else
  begin
    Result := Size;
    case FieldType of
      ftString:
        Inc(Result);
      ftSmallint:
        Result := SizeOf(SmallInt);
      ftInteger:
        Result := SizeOf(Longint);
      ftWord:
        Result := SizeOf(Word);
      ftBoolean:
        Result := SizeOf(WordBool);
      ftFloat:
        Result := SizeOf(Double);
      ftCurrency:
        Result := SizeOf(Double);
      ftBCD:
        Result := 34;
      ftDate, ftTime:
        Result := SizeOf(Longint);
      ftDateTime:
        Result := SizeOf(TDateTime);
      ftBytes:
        Result := Size;
      ftVarBytes:
        Result := Size + 2;
      ftAutoInc:
        Result := SizeOf(Longint);
      {$IFDEF COMPILER4_UP}
      ftADT:
        Result := 0;
      ftFixedChar:
        Inc(Result);
      ftWideString:
        Result := (Result + 1) * 2;
      ftLargeint:
        Result := SizeOf(Int64);
      {$ENDIF}
      {$IFDEF COMPILER5_UP}
      ftVariant:
        Result := SizeOf(Variant);
      ftGuid:
        Result := GuidSize + 1;
      {$ENDIF}
    end;
  end;
end;

procedure CalcDataSize(FieldDef: TFieldDef; var DataSize: Integer);
{$IFDEF COMPILER4_UP}
var
  I: Integer;
{$ENDIF}
begin
  with FieldDef do
  begin
    if DataType in ftSupported - ftBlobTypes then
      Inc(DataSize, CalcFieldLen(DataType, Size) + 1);
    {$IFDEF COMPILER4_UP}
    for I := 0 to ChildDefs.Count - 1 do
      CalcDataSize(ChildDefs[I], DataSize);
    {$ENDIF}
  end;
end;

procedure Error(const Msg: string);
begin
  DatabaseError(Msg);
end;

procedure ErrorFmt(const Msg: string; const Args: array of const);
begin
  DatabaseErrorFmt(Msg, Args);
end;

type
  TBookmarkData = Integer;
  PMemBookmarkInfo = ^TMemBookmarkInfo;
  TMemBookmarkInfo = record
    BookmarkData: TBookmarkData;
    BookmarkFlag: TBookmarkFlag;
  end;

//=== TJvMemoryRecord ========================================================

constructor TJvMemoryRecord.Create(MemoryData: TJvMemoryData);
begin
  CreateEx(MemoryData, True);
end;

constructor TJvMemoryRecord.CreateEx(MemoryData: TJvMemoryData;
  UpdateParent: Boolean);
begin
  inherited Create;
  SetMemoryData(MemoryData, UpdateParent);
end;

destructor TJvMemoryRecord.Destroy;
begin
  SetMemoryData(nil, True);
  inherited Destroy;
end;

function TJvMemoryRecord.GetIndex: Integer;
begin
  if FMemoryData <> nil then
    Result := FMemoryData.FRecords.IndexOf(Self)
  else
    Result := -1;
end;

procedure TJvMemoryRecord.SetMemoryData(Value: TJvMemoryData; UpdateParent: Boolean);
var
  I: Integer;
  DataSize: Integer;
begin
  if FMemoryData <> Value then
  begin
    if FMemoryData <> nil then
    begin
      FMemoryData.FRecords.Remove(Self);
      if FMemoryData.BlobFieldCount > 0 then
        Finalize(PMemBlobArray(FBlobs)[0], FMemoryData.BlobFieldCount);
      ReallocMem(FBlobs, 0);
      ReallocMem(FData, 0);
      FMemoryData := nil;
    end;
    if Value <> nil then
    begin
      if UpdateParent then
      begin
        Value.FRecords.Add(Self);
        Inc(Value.FLastID);
        FID := Value.FLastID;
      end;
      FMemoryData := Value;
      if Value.BlobFieldCount > 0 then
      begin
        ReallocMem(FBlobs, Value.BlobFieldCount * SizeOf(Pointer));
        Initialize(PMemBlobArray(FBlobs)[0], Value.BlobFieldCount);
      end;
      DataSize := 0;
      for I := 0 to Value.FieldDefs.Count - 1 do
        CalcDataSize(Value.FieldDefs[I], DataSize);
      ReallocMem(FData, DataSize);
    end;
  end;
end;

procedure TJvMemoryRecord.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetIndex;
  if (CurIndex >= 0) and (CurIndex <> Value) then
    FMemoryData.FRecords.Move(CurIndex, Value);
end;

//=== TJvMemoryData ==========================================================

constructor TJvMemoryData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordPos := -1;
  FLastID := Low(Integer);
  FAutoInc := 1;
  FRecords := TList.Create;
end;

destructor TJvMemoryData.Destroy;
begin
  FreeIndexList;
  ClearRecords;
  FRecords.Free;
  ReallocMem(FOffsets, 0);
  inherited Destroy;
end;

function TJvMemoryData.GetCapacity: Integer;
begin
  if FRecords <> nil then
    Result := FRecords.Capacity
  else
    Result := 0;
end;

procedure TJvMemoryData.SetCapacity(Value: Integer);
begin
  if FRecords <> nil then
    FRecords.Capacity := Value;
end;

function TJvMemoryData.AddRecord: TJvMemoryRecord;
begin
  Result := TJvMemoryRecord.Create(Self);
end;

function TJvMemoryData.FindRecordID(ID: Integer): TJvMemoryRecord;
var
  I: Integer;
begin
  for I := 0 to FRecords.Count - 1 do
  begin
    Result := TJvMemoryRecord(FRecords[I]);
    if Result.ID = ID then
      Exit;
  end;
  Result := nil;
end;

function TJvMemoryData.InsertRecord(Index: Integer): TJvMemoryRecord;
begin
  Result := AddRecord;
  Result.Index := Index;
end;

function TJvMemoryData.GetMemoryRecord(Index: Integer): TJvMemoryRecord;
begin
  Result := TJvMemoryRecord(FRecords[Index]);
end;

{$IFNDEF COMPILER5_UP}

function TJvMemoryData.BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
begin
  Move(BCD^, Curr, SizeOf(Currency));
  Result := True;
end;

function TJvMemoryData.CurrToBCD(const Curr: Currency; BCD: Pointer; Precision,
  Decimals: Integer): Boolean;
begin
  Move(Curr, BCD^, SizeOf(Currency));
  Result := True;
end;

{$ENDIF COMPILER5_UP}

procedure TJvMemoryData.InitFieldDefsFromFields;
var
  I: Integer;
  Offset: Word;
begin
  if FieldDefs.Count = 0 then
  begin
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if (FieldKind in fkStoredFields) and not (DataType in ftSupported) then
          ErrorFmt(SUnknownFieldType, [DisplayName]);
    end;
    FreeIndexList;
  end;
  Offset := 0;
  {$IFDEF COMPILER4_UP}
  inherited InitFieldDefsFromFields;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefList.Count * SizeOf(Word));
  for I := 0 to FieldDefList.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefList[I] do
    begin
      if DataType in ftSupported - ftBlobTypes then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
  {$ELSE}
  { Create FieldDefs from persistent fields if needed }
  if FieldDefs.Count = 0 then
    for I := 0 to FieldCount - 1 do
    begin
      with Fields[I] do
        if FieldKind = fkData then
          FieldDefs.Add(FieldName, DataType, Size, Required);
    end;
  { Calculate fields offsets }
  ReallocMem(FOffsets, FieldDefs.Count * SizeOf(Word));
  for I := 0 to FieldDefs.Count - 1 do
  begin
    FOffsets^[I] := Offset;
    with FieldDefs[I] do
    begin
      if DataType in ftSupported - ftBlobTypes then
        Inc(Offset, CalcFieldLen(DataType, Size) + 1);
    end;
  end;
  {$ENDIF}
end;

function TJvMemoryData.FindFieldData(Buffer: Pointer; Field: TField): Pointer;
var
  Index: Integer;
begin
  {$IFDEF COMPILER4_UP}
  Index := FieldDefList.IndexOf(Field.FullName);
  {$ELSE}
  Index := FieldDefs.IndexOf(Field.FieldName);
  {$ENDIF}
  if (Index >= 0) and (Buffer <> nil) and
    {$IFDEF COMPILER4_UP}
    (FieldDefList[Index].DataType in ftSupported - ftBlobTypes) then
    {$ELSE}
    (FieldDefs[Index].DataType in ftSupported - ftBlobTypes) then
    {$ENDIF}
    Result := (PChar(Buffer) + FOffsets[Index])
  else
    Result := nil;
end;

function TJvMemoryData.CalcRecordSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FieldDefs.Count - 1 do
    CalcDataSize(FieldDefs[I], Result);
end;

procedure TJvMemoryData.InitBufferPointers(GetProps: Boolean);
begin
  if GetProps then
    FRecordSize := CalcRecordSize;
  FBookmarkOfs := FRecordSize + CalcFieldsSize;
  FBlobOfs := FBookmarkOfs + SizeOf(TMemBookmarkInfo);
  FRecBufSize := FBlobOfs + BlobFieldCount * SizeOf(Pointer);
end;

procedure TJvMemoryData.ClearRecords;
begin
  while FRecords.Count > 0 do
    TObject(FRecords.Last).Free;
  FLastID := Low(Integer);
  FRecordPos := -1;
end;

function TJvMemoryData.AllocRecordBuffer: PChar;
begin
  Result := StrAlloc(FRecBufSize);
  if BlobFieldCount > 0 then
    Initialize(PMemBlobArray(Result + FBlobOfs)[0], BlobFieldCount);
end;

procedure TJvMemoryData.FreeRecordBuffer(var Buffer: PChar);
begin
  if BlobFieldCount > 0 then
    Finalize(PMemBlobArray(Buffer + FBlobOfs)[0], BlobFieldCount);
  StrDispose(Buffer);
  Buffer := nil;
end;

procedure TJvMemoryData.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[FRecordSize], CalcFieldsSize, 0);
end;

procedure TJvMemoryData.InternalInitRecord(Buffer: PChar);
var
  I: Integer;
begin
  FillChar(Buffer^, FBlobOfs, 0);
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)[I] := '';
end;

procedure TJvMemoryData.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Low(Integer);
    BookmarkFlag := bfInserted;
  end;
end;

function TJvMemoryData.GetCurrentRecord(Buffer: PChar): Boolean;
begin
  Result := False;
  if not IsEmpty and (GetBookmarkFlag(ActiveBuffer) = bfCurrent) then
  begin
    UpdateCursorPos;
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      Move(Records[FRecordPos].Data^, Buffer^, FRecordSize);
      Result := True;
    end;
  end;
end;

procedure TJvMemoryData.RecordToBuffer(Rec: TJvMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  Move(Rec.Data^, Buffer^, FRecordSize);
  with PMemBookmarkInfo(Buffer + FBookmarkOfs)^ do
  begin
    BookmarkData := Rec.ID;
    BookmarkFlag := bfCurrent;
  end;
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Buffer + FBlobOfs)[I] := PMemBlobArray(Rec.FBlobs)[I];
  GetCalcFields(Buffer);
end;

function TJvMemoryData.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Accept: Boolean;
begin
  Result := grOk;
  Accept := True;
  case GetMode of
    gmPrior:
      if FRecordPos <= 0 then
      begin
        Result := grBOF;
        FRecordPos := -1;
      end
      else
      begin
        repeat
          Dec(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos < 0);
        if not Accept then
        begin
          Result := grBOF;
          FRecordPos := -1;
        end;
      end;
    gmCurrent:
      if (FRecordPos < 0) or (FRecordPos >= RecordCount) then
        Result := grError
      else
      if Filtered then
        if not RecordFilter then
          Result := grError;
    gmNext:
      if FRecordPos >= RecordCount - 1 then
        Result := grEOF
      else
      begin
        repeat
          Inc(FRecordPos);
          if Filtered then
            Accept := RecordFilter;
        until Accept or (FRecordPos > RecordCount - 1);
        if not Accept then
        begin
          Result := grEOF;
          FRecordPos := RecordCount - 1;
        end;
      end;
  end;
  if Result = grOk then
    RecordToBuffer(Records[FRecordPos], Buffer)
  else
  if (Result = grError) and DoCheck then
    Error(SMemNoRecords);
end;

function TJvMemoryData.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TJvMemoryData.GetActiveRecBuf(var RecBuf: PChar): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := ActiveBuffer;
    dsEdit, dsInsert:
      RecBuf := ActiveBuffer;
    dsCalcFields:
      RecBuf := CalcBuffer;
    dsFilter:
      RecBuf := TempBuffer;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TJvMemoryData.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf, Data: PChar;
  {$IFDEF COMPILER5_UP}
  VarData: Variant;
  {$ENDIF}
begin
  Result := False;
  if not GetActiveRecBuf(RecBuf) then
    Exit;
  if Field.FieldNo > 0 then
  begin
    Data := FindFieldData(RecBuf, Field);
    if Data <> nil then
    begin
      Result := Boolean(Data[0]);
      Inc(Data);
      if Field.DataType in [ftString {$IFDEF COMPILER4_UP}, ftFixedChar,
      ftWideString {$ENDIF} {$IFDEF COMPILER5_UP}, ftGuid {$ENDIF}] then
        Result := Result and (StrLen(Data) > 0);
      if Result and (Buffer <> nil) then
        {$IFDEF COMPILER5_UP}
        if Field.DataType = ftVariant then
        begin
          VarData := PVariant(Data)^;
          PVariant(Buffer)^ := VarData;
        end
        else
        {$ENDIF}
          Move(Data^, Buffer^, CalcFieldLen(Field.DataType, Field.Size));
    end;
  end
  else
  if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields] then
  begin
    Inc(RecBuf, FRecordSize + Field.Offset);
    Result := Boolean(RecBuf[0]);
    if Result and (Buffer <> nil) then
      Move(RecBuf[1], Buffer^, Field.DataSize);
  end;
end;

procedure TJvMemoryData.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf, Data: PChar;
  {$IFDEF COMPILER5_UP}
  VarData: Variant;
  {$ENDIF}
begin
  if not (State in dsWriteModes) then
    Error(SNotEditing);
  GetActiveRecBuf(RecBuf);
  with Field do
  begin
    if FieldNo > 0 then
    begin
      if State in [dsCalcFields, dsFilter] then
        Error(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        ErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then
      begin
        Data := FindFieldData(RecBuf, Field);
        if Data <> nil then
        begin
          {$IFDEF COMPILER5_UP}
          if DataType = ftVariant then
          begin
            if Buffer <> nil then
              VarData := PVariant(Buffer)^
            else
              VarData := EmptyParam;
            Boolean(Data[0]) := LongBool(Buffer) and not
              (VarIsNull(VarData) or VarIsEmpty(VarData));
            if Boolean(Data[0]) then
            begin
              Inc(Data);
              PVariant(Data)^ := VarData;
            end
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end
          else
          {$ENDIF}
          begin
            Boolean(Data[0]) := LongBool(Buffer);
            Inc(Data);
            if LongBool(Buffer) then
              Move(Buffer^, Data^, CalcFieldLen(DataType, Size))
            else
              FillChar(Data^, CalcFieldLen(DataType, Size), 0);
          end;
        end;
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      Inc(RecBuf, FRecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[0]) then
        Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;

procedure TJvMemoryData.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      inherited SetFiltered(Value);
      First;
    end;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TJvMemoryData.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then
  begin
    CheckBrowseMode;
    inherited SetOnFilterRecord(Value);
    if Filtered then
      First;
  end
  else
    inherited SetOnFilterRecord(Value);
end;

function TJvMemoryData.RecordFilter: Boolean;
var
  SaveState: TDataSetState;
begin
  Result := True;
  if Assigned(OnFilterRecord) then
  begin
    if (FRecordPos >= 0) and (FRecordPos < RecordCount) then
    begin
      SaveState := SetTempState(dsFilter);
      try
        RecordToBuffer(Records[FRecordPos], TempBuffer);
        OnFilterRecord(Self, Result);
      except
        Application.HandleException(Self);
      end;
      RestoreState(SaveState);
    end
    else
      Result := False;
  end;
end;

function TJvMemoryData.GetBlobData(Field: TField; Buffer: PChar): TMemBlobData;
begin
  Result := PMemBlobArray(Buffer + FBlobOfs)[Field.Offset];
end;

procedure TJvMemoryData.SetBlobData(Field: TField; Buffer: PChar;
  Value: TMemBlobData);
begin
  if Buffer = ActiveBuffer then
  begin
    if State = dsFilter then
      Error(SNotEditing);
    PMemBlobArray(Buffer + FBlobOfs)[Field.Offset] := Value;
  end;
end;

procedure TJvMemoryData.CloseBlob(Field: TField);
begin
  if (FRecordPos >= 0) and (FRecordPos < FRecords.Count) and (State = dsEdit) then
    PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] :=
      PMemBlobArray(Records[FRecordPos].FBlobs)[Field.Offset]
  else
    PMemBlobArray(ActiveBuffer + FBlobOfs)[Field.Offset] := '';
end;

function TJvMemoryData.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TJvMemBlobStream.Create(Field as TBlobField, Mode);
end;

function TJvMemoryData.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Bookmark <> nil) and FActive and (TBookmarkData(Bookmark^) > Low(Integer)) and
    (TBookmarkData(Bookmark^) <= FLastID);
end;

function TJvMemoryData.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  if (Bookmark1 = nil) and (Bookmark2 = nil) then
    Result := 0
  else
  if (Bookmark1 <> nil) and (Bookmark2 = nil) then
    Result := 1
  else
  if (Bookmark1 = nil) and (Bookmark2 <> nil) then
    Result := -1
  else
  if TBookmarkData(Bookmark1^) > TBookmarkData(Bookmark2^) then
    Result := 1
  else
  if TBookmarkData(Bookmark1^) < TBookmarkData(Bookmark2^) then
    Result := -1
  else
    Result := 0;
end;

procedure TJvMemoryData.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData, Data^,
    SizeOf(TBookmarkData));
end;

procedure TJvMemoryData.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Data^, PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData,
    SizeOf(TBookmarkData));
end;

function TJvMemoryData.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag;
end;

procedure TJvMemoryData.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkFlag := Value;
end;

procedure TJvMemoryData.InternalGotoBookmark(Bookmark: TBookmark);
var
  Rec: TJvMemoryRecord;
  SavePos: Integer;
  Accept: Boolean;
begin
  Rec := FindRecordID(TBookmarkData(Bookmark^));
  if Rec <> nil then
  begin
    Accept := True;
    SavePos := FRecordPos;
    try
      FRecordPos := Rec.Index;
      if Filtered then
        Accept := RecordFilter;
    finally
      if not Accept then
        FRecordPos := SavePos;
    end;
  end;
end;

procedure TJvMemoryData.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(@PMemBookmarkInfo(Buffer + FBookmarkOfs)^.BookmarkData);
end;

procedure TJvMemoryData.InternalFirst;
begin
  FRecordPos := -1;
end;

procedure TJvMemoryData.InternalLast;
begin
  FRecordPos := FRecords.Count;
end;

procedure TJvMemoryData.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);
begin
  if Field.DataType = ftWideString then
  begin
    if ToNative then
    begin
      Word(Dest^) := Length(PWideString(Source)^) * 2;
      Move(PWideChar(Source^)^, (PWideChar(Dest) + 1)^, Word(Dest^));
    end
    else
      SetString(WideString(Dest^), PWideChar(PChar(Source) + 2), Word(Source^) div 2);
  end
  else
    inherited DataConvert(Field, Source, Dest, ToNative);
end;

procedure TJvMemoryData.AssignMemoryRecord(Rec: TJvMemoryRecord; Buffer: PChar);
var
  I: Integer;
begin
  Move(Buffer^, Rec.Data^, FRecordSize);
  for I := 0 to BlobFieldCount - 1 do
    PMemBlobArray(Rec.FBlobs)[I] := PMemBlobArray(Buffer + FBlobOfs)[I];
end;

procedure TJvMemoryData.SetMemoryRecordData(Buffer: PChar; Pos: Integer);
var
  Rec: TJvMemoryRecord;
begin
  if State = dsFilter then
    Error(SNotEditing);
  Rec := Records[Pos];
  AssignMemoryRecord(Rec, Buffer);
end;

procedure TJvMemoryData.SetAutoIncFields(Buffer: PChar);
var
  I, Count: Integer;
  Data: PChar;
begin
  Count := 0;
  for I := 0 to FieldCount - 1 do
    if (Fields[I].FieldKind in fkStoredFields) and
      (Fields[I].DataType = ftAutoInc) then
    begin
      Data := FindFieldData(Buffer, Fields[I]);
      if Data <> nil then
      begin
        Boolean(Data[0]) := True;
        Inc(Data);
        Move(FAutoInc, Data^, SizeOf(Longint));
        Inc(Count);
      end;
    end;
  if Count > 0 then
    Inc(FAutoInc);
end;

procedure TJvMemoryData.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
  Rec: TJvMemoryRecord;
begin
  if Append then
  begin
    Rec := AddRecord;
    FRecordPos := FRecords.Count - 1;
  end
  else
  begin
    if FRecordPos = -1 then
      RecPos := 0
    else
      RecPos := FRecordPos;
    Rec := InsertRecord(RecPos);
    FRecordPos := RecPos;
  end;
  SetAutoIncFields(Buffer);
  SetMemoryRecordData(Buffer, Rec.Index);
end;

procedure TJvMemoryData.InternalDelete;
var
  Accept: Boolean;
begin
  Records[FRecordPos].Free;
  if FRecordPos >= FRecords.Count then
    Dec(FRecordPos);
  Accept := True;
  repeat
    if Filtered then
      Accept := RecordFilter;
    if not Accept then
      Dec(FRecordPos);
  until Accept or (FRecordPos < 0);
  if FRecords.Count = 0 then
    FLastID := Low(Integer);
end;

procedure TJvMemoryData.InternalPost;
var
  RecPos: Integer;
begin
  if State = dsEdit then
    SetMemoryRecordData(ActiveBuffer, FRecordPos)
  else
  begin
    if State in [dsInsert] then
      SetAutoIncFields(ActiveBuffer);
    if FRecordPos >= FRecords.Count then
    begin
      SetMemoryRecordData(ActiveBuffer, AddRecord.Index);
      FRecordPos := FRecords.Count - 1;
    end
    else
    begin
      if FRecordPos = -1 then
        RecPos := 0
      else
        RecPos := FRecordPos;
      SetMemoryRecordData(ActiveBuffer, InsertRecord(RecPos).Index);
      FRecordPos := RecPos;
    end;
  end;
end;

procedure TJvMemoryData.OpenCursor(InfoQuery: Boolean);
begin
  if not InfoQuery then
  begin
    if FieldCount > 0 then
      FieldDefs.Clear;
    InitFieldDefsFromFields;
  end;
  FActive := True;
  inherited OpenCursor(InfoQuery);
end;

procedure TJvMemoryData.InternalOpen;
begin
  BookmarkSize := SizeOf(TBookmarkData);
  {$IFDEF COMPILER4_UP}
  if DefaultFields then
    CreateFields;
  {$ELSE}
  if DefaultFields then
    Error(SInvalidFields);
  {$ENDIF}
  BindFields(True);
  InitBufferPointers(True);
  InternalFirst;
end;

procedure TJvMemoryData.InternalClose;
begin
  ClearRecords;
  FAutoInc := 1;
  BindFields(False);
  {$IFDEF COMPILER4_UP}
  if DefaultFields then
    DestroyFields;
  {$ENDIF}
  FreeIndexList;
  FActive := False;
end;

procedure TJvMemoryData.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TJvMemoryData.InternalInitFieldDefs;
begin
  InitFieldDefsFromFields;
end;

function TJvMemoryData.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

function TJvMemoryData.GetRecordCount: Integer;
begin
  Result := FRecords.Count;
end;

function TJvMemoryData.GetRecNo: Integer;
begin
  CheckActive;
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FRecordPos + 1;
end;

procedure TJvMemoryData.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= FRecords.Count) then
  begin
    FRecordPos := Value - 1;
    Resync([]);
  end;
end;

function TJvMemoryData.IsSequenced: Boolean;
begin
  Result := not Filtered;
end;

function TJvMemoryData.Locate(const KeyFields: string;
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

procedure TJvMemoryData.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    ClearRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end;
end;

procedure TJvMemoryData.CopyStructure(Source: TDataSet; UseAutoIncAsInteger: boolean = false);

  procedure CheckDataTypes(FieldDefs: TFieldDefs);
  var
    I: Integer;
  begin
    for I := FieldDefs.Count - 1 downto 0 do
    begin
      if (FieldDefs.Items[I].DataType = ftAutoInc) and UseAutoIncAsInteger then
        FieldDefs.Items[I].DataType := ftInteger;
      if not (FieldDefs.Items[I].DataType in ftSupported) then
        FieldDefs.Items[I].Free
      {$IFDEF COMPILER4_UP}
      else
        CheckDataTypes(FieldDefs[I].ChildDefs);
      {$ENDIF}
    end;
  end;

var
  I: Integer;
begin
  CheckInactive;
  for I := FieldCount - 1 downto 0 do
    Fields[I].Free;
  if Source = nil then
    Exit;
  Source.FieldDefs.Update;
  FieldDefs := Source.FieldDefs;
  CheckDataTypes(FieldDefs);
  {$IFDEF COMPILER4_UP}
  CreateFields;
  {$ELSE}
  for I := 0 to FieldDefs.Count - 1 do
    if (csDesigning in ComponentState) and (Owner <> nil) then
      FieldDefs.Items[I].CreateField(Owner)
    else
      FieldDefs.Items[I].CreateField(Self);
  {$ENDIF}
end;

procedure TJvMemoryData.FixReadOnlyFields(MakeReadOnly: boolean);
var
  i: integer;
begin
  if MakeReadOnly then
    for i := 0 to FieldCount - 1 do
      Fields[i].ReadOnly := (Fields[i].Tag = 1)
  else
    for i := 0 to FieldCount - 1 do
    begin
      Fields[i].Tag := Ord(Fields[i].ReadOnly);
      Fields[i].ReadOnly := false;
      if Fields[i].DataType = ftAutoInc then
        FAutoIncField := Fields[i];
    end;
end;

function TJvMemoryData.LoadFromDataSet(Source: TDataSet; RecordCount: Integer;
  Mode: TLoadMode;DisableAllControls:boolean=true): Integer;
var
  SourceActive: Boolean;
  MovedCount, i: Integer;
  SB, DB: TBookmark;
begin
  Result := 0;
  if Source = Self then
    Exit;
  FSaveLoadState := slsLoading;
  SourceActive := Source.Active;
  if DisableAllControls then
    Source.DisableControls;
  SB := Source.GetBookmark;
  try
    if DisableAllControls then
      self.DisableControls;
    DB := GetBookMark;
    try
      Filtered := False;
      with Source do
      begin
        Open;
        CheckBrowseMode;
        UpdateCursorPos;
      end;
      if Mode = lmCopy then
      begin
        Close;
        CopyStructure(Source);
      end;
      FreeIndexList;
      if not Active then
        Open;
      CheckBrowseMode;
      if RecordCount > 0 then
        MovedCount := RecordCount
      else
      begin
        Source.First;
        MovedCount := MaxInt;
      end;
      FAutoIncField := nil;
      // FixReadOnlyFields also sets FAutoIncField if there is any
      FixReadOnlyFields(false);
      // find first source autoinc field
      FSrcAutoIncField := nil;
      if Mode = lmCopy then
        for i := 0 to Source.FieldCount - 1 do
          if Source.Fields[i].DataType = ftAutoInc then
          begin
            FSrcAutoIncField := Source.Fields[i];
            Break;
          end;
      try
        while not Source.Eof do
        begin
          Append;
          AssignRecord(Source, Self, True);
          // assign AutoInc value manually (make usre to keep largest if source isn't sorted by autoinc field)
          if (FAutoIncField <> nil) and (FSrcAutoIncField <> nil) then
            FAutoInc := Max(FAutoInc, FSrcAutoIncField.AsInteger);
          Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Source.Next;
        end;
      finally
        FixReadOnlyFields(true);
        FAutoIncField := nil;
        FSrcAutoIncField := nil;
        First;
      end;
      // move back to where we started from
      if (DB <> nil) and BookMarkValid(DB) then
      begin
        GotoBookMark(DB);
        FreeBookMark(DB);
      end;
    finally
      if DisableAllControls then
        EnableControls;
    end;
  finally
    // move back to where we started from
    if (SB <> nil) and Source.BookmarkValid(SB) then
    begin
      Source.GotoBookmark(SB);
      Source.FreeBookmark(SB);
    end;
    if not SourceActive then
      Source.Close;
    if DisableAllControls then
      Source.EnableControls;
    FSaveLoadState := slsNone;
  end;
end;

function TJvMemoryData.SaveToDataSet(Dest: TDataSet; RecordCount: Integer;DisableAllControls:boolean=true): Integer;
var
  MovedCount: Integer;
  SB, DB: TBookmark;
begin
  Result := 0;
  if Dest = Self then
    Exit;
  CheckBrowseMode;
  UpdateCursorPos;
  if DisableAllControls then
  begin
    DisableControls;
    Dest.DisableControls;
  end;
  FSaveLoadState := slsSaving;
  try
    SB := GetBookmark;
    DB := Dest.GetBookMark;
    try
      if not Dest.Active then
        Dest.Open
      else
        Dest.CheckBrowseMode;
      if RecordCount > 0 then
        MovedCount := RecordCount
      else
      begin
        First;
        MovedCount := MaxInt;
      end;
      try
        while not Eof do
        begin
          Dest.Append;
          AssignRecord(Self, Dest, True);
          Dest.Post;
          Inc(Result);
          if Result >= MovedCount then
            Break;
          Next;
        end;
      finally
        Dest.First;
      end;
    finally
      if (SB <> nil) and BookmarkValid(SB) then
      begin
        GotoBookmark(SB);
        FreeBookMark(SB);
      end;
      if (DB <> nil) and Dest.BookmarkValid(DB) then
      begin
        Dest.GotoBookmark(DB);
        Dest.FreeBookmark(DB);
      end;
    end;
  finally
    if DisableAllControls then
    begin
      EnableControls;
      Dest.EnableControls;
    end;
    FSaveLoadState := slsNone;
  end;
end;

procedure TJvMemoryData.SortOnFields(const FieldNames: string;
  {$IFDEF COMPILER4_UP}
  CaseInsensitive: Boolean = True; Descending: Boolean = False);
  {$ELSE}
  CaseInsensitive, Descending: Boolean);
  {$ENDIF}
begin
  CreateIndexList(FieldNames);
  FCaseInsensitiveSort := CaseInsensitive;
  FDescendingSort := Descending;
  try
    Sort;
  except
    FreeIndexList;
    raise;
  end;
end;

procedure TJvMemoryData.Sort;
var
  Pos: TBookmarkStr;
begin
  if Active and (FRecords <> nil) and (FRecords.Count > 0) then
  begin
    Pos := Bookmark;
    try
      QuickSort(0, FRecords.Count - 1, CompareRecords);
      SetBufListSize(0);
      InitBufferPointers(False);
      try
        SetBufListSize(BufferCount + 1);
      except
        SetState(dsInactive);
        CloseCursor;
        raise;
      end;
    finally
      Bookmark := Pos;
    end;
    Resync([]);
  end;
end;

procedure TJvMemoryData.QuickSort(L, R: Integer; Compare: TCompareRecords);
var
  I, J: Integer;
  P: TJvMemoryRecord;
begin
  repeat
    I := L;
    J := R;
    P := Records[(L + R) shr 1];
    repeat
      while Compare(Records[I], P) < 0 do
        Inc(I);
      while Compare(Records[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        FRecords.Exchange(I, J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, Compare);
    L := I;
  until I >= R;
end;

function TJvMemoryData.CompareRecords(Item1, Item2: TJvMemoryRecord): Integer;
var
  Data1, Data2: PChar;
  F: TField;
  I: Integer;
begin
  Result := 0;
  if FIndexList <> nil then
  begin
    for I := 0 to FIndexList.Count - 1 do
    begin
      F := TField(FIndexList[I]);
      Data1 := FindFieldData(Item1.Data, F);
      if Data1 <> nil then
      begin
        Data2 := FindFieldData(Item2.Data, F);
        if Data2 <> nil then
        begin
          if Boolean(Data1[0]) and Boolean(Data2[0]) then
          begin
            Inc(Data1);
            Inc(Data2);
            Result := CompareFields(Data1, Data2, F.DataType,
              FCaseInsensitiveSort);
          end
          else
          if Boolean(Data1[0]) then
            Result := 1
          else
          if Boolean(Data2[0]) then
            Result := -1;
          if FDescendingSort then
            Result := -Result;
        end;
      end;
      if Result <> 0 then
        Exit;
    end;
  end;
  if Result = 0 then
  begin
    if Item1.ID > Item2.ID then
      Result := 1
    else
    if Item1.ID < Item2.ID then
      Result := -1;
    if FDescendingSort then
      Result := -Result;
  end;
end;

function TJvMemoryData.GetIsIndexField(Field: TField): Boolean;
begin
  if FIndexList <> nil then
    Result := FIndexList.IndexOf(Field) >= 0
  else
    Result := False;
end;

procedure TJvMemoryData.CreateIndexList(const FieldNames: string);
var
  Pos: Integer;
  F: TField;
begin
  if FIndexList = nil then
    FIndexList := TList.Create
  else
    FIndexList.Clear;
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    F := FieldByName(ExtractFieldName(FieldNames, Pos));
    if (F.FieldKind = fkData) and
      (F.DataType in ftSupported - ftBlobTypes) then
      FIndexList.Add(F)
    else
      ErrorFmt(SFieldTypeMismatch, [F.DisplayName]);
  end;
end;

procedure TJvMemoryData.FreeIndexList;
begin
  FIndexList.Free;
  FIndexList := nil;
end;

//=== TJvMemBlobStream =======================================================

constructor TJvMemBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TJvMemoryData;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if not FField.Modified and (Mode <> bmRead) then
  begin
    if FField.ReadOnly then
      ErrorFmt(SFieldReadOnly, [FField.DisplayName]);
    if not (FDataSet.State in [dsEdit, dsInsert]) then
      Error(SNotEditing);
    FCached := True;
  end
  else
    FCached := (FBuffer = FDataSet.ActiveBuffer);
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TJvMemBlobStream.Destroy;
begin
  if FOpened and FModified then
    FField.Modified := True;
  if FModified then
  try
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    Application.HandleException(Self);
  end;
end;

function TJvMemBlobStream.GetBlobFromRecord(Field: TField): TMemBlobData;
var
  Rec: TJvMemoryRecord;
  Pos: Integer;
begin
  Result := '';
  Pos := FDataSet.FRecordPos;
  if (Pos < 0) and (FDataSet.RecordCount > 0) then
    Pos := 0
  else
  if Pos >= FDataSet.RecordCount then
    Pos := FDataSet.RecordCount - 1;
  if (Pos >= 0) and (Pos < FDataSet.RecordCount) then
  begin
    Rec := FDataSet.Records[Pos];
    if Rec <> nil then
      Result := PMemBlobArray(Rec.FBlobs)[FField.Offset];
  end;
end;

function TJvMemBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if FOpened then
  begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then
    begin
      if FCached then
      begin
        Move(PChar(FDataSet.GetBlobData(FField, FBuffer))[FPosition], Buffer,
          Result);
        Inc(FPosition, Result);
      end
      else
      begin
        Move(PChar(GetBlobFromRecord(FField))[FPosition], Buffer, Result);
        Inc(FPosition, Result);
      end;
    end;
  end;
end;

function TJvMemBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: TMemBlobData;
begin
  Result := 0;
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    Temp := FDataSet.GetBlobData(FField, FBuffer);
    if Length(Temp) < FPosition + Count then
      SetLength(Temp, FPosition + Count);
    Move(Buffer, PChar(Temp)[FPosition], Count);
    FDataSet.SetBlobData(FField, FBuffer, Temp);
    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
  end;
end;

function TJvMemBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    0:
      FPosition := Offset;
    1:
      Inc(FPosition, Offset);
    2:
      FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TJvMemBlobStream.Truncate;
begin
  if FOpened and FCached and (FMode <> bmRead) then
  begin
    FDataSet.SetBlobData(FField, FBuffer, '');
    FModified := True;
  end;
end;

function TJvMemBlobStream.GetBlobSize: Longint;
begin
  Result := 0;
  if FOpened then
    if FCached then
      Result := Length(FDataSet.GetBlobData(FField, FBuffer))
    else
      Result := Length(GetBlobFromRecord(FField))
end;

{$ENDIF COMPILER3_UP}

end.

