{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBdeUtils.PAS, released on 2002-07-04.

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

unit JvBdeUtils;

interface

uses
  Windows, SysUtils, Bde, Registry,
  {$IFDEF COMPILER6_UP}
  RTLConsts,
  {$ENDIF}
  Classes, DB, DBTables, IniFiles,
  JvDBUtils;

type
  {$IFNDEF WIN32}
  TLocateFilter = (lfTree, lfCallback);
  {$ENDIF}

  {$IFNDEF COMPILER3_UP}
  TBDEDataSet = TDataSet;
  {$ENDIF}

  {$IFNDEF COMPILER5_UP}
  TDatabaseLoginEvent = TLoginEvent;
  {$ENDIF}

  TJvDBLocate = class(TJvLocateObject)
  private
    {$IFNDEF WIN32}
    FFilterHandle: HDBIFilter;
    FTree: PChar;
    FTreeSize: Integer;
    FFilterKind: TLocateFilter;
    procedure ActivateFilter;
    procedure DeactivateFilter;
    procedure DropFilter;
    procedure CheckFilterKind;
    procedure ChangeBookmark;
    procedure BuildFilterHeader(var Rec);
    procedure BuildFilterTree;
    procedure FreeTree;
    function RecordFilter(RecBuf: Pointer; RecNo: Longint): Smallint;
    {$IFDEF WIN32} stdcall;
    {$ENDIF}
    {$ELSE}
    function LocateCallback: Boolean;
    procedure RecordFilter(DataSet: TDataSet; var Accept: Boolean);
    {$ENDIF WIN32}
  protected
    {$IFDEF WIN32}
    function LocateFilter: Boolean; override;
    {$ELSE}
    procedure ActiveChanged; override;
    function LocateFilter: Boolean; override;
    {$ENDIF WIN32}
    procedure CheckFieldType(Field: TField); override;
    function LocateKey: Boolean; override;
    function UseKey: Boolean; override;
    function FilterApplicable: Boolean; override;
  public
    destructor Destroy; override;
  end;

  TJvCloneDataset = class(TBDEDataSet)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    property SourceHandle: HDBICur read FSourceHandle write SetSourceHandle;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

  TJvCloneDbDataset = class(TDBDataSet)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    procedure InitFromDataSet(Source: TDBDataSet; Reset: Boolean);
    property SourceHandle: HDBICur read FSourceHandle write SetSourceHandle;
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

  TJvCloneTable = class(TTable)
  private
    FSourceHandle: HDBICur;
    FReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    procedure SetSourceHandle(ASourceHandle: HDBICur);
  protected
    function CreateHandle: HDBICur; override;
  public
    procedure InitFromTable(SourceTable: TTable; Reset: Boolean);
  published
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  end;

{ Utility routines }

function CreateDbLocate: TJvLocateObject;
{$IFNDEF WIN32}
function CheckOpen(Status: DBIResult): Boolean;
{$ENDIF}
procedure FetchAllRecords(DataSet: TBDEDataSet);
function TransActive(Database: TDatabase): Boolean;
function AsyncQrySupported(Database: TDatabase): Boolean;
{$IFDEF WIN32}
function GetQuoteChar(Database: TDatabase): string;
{$ENDIF}
procedure ExecuteQuery(const DbName, QueryText: string);
procedure ExecuteQueryEx(const SessName, DbName, QueryText: string);
procedure BdeTranslate(Locale: TLocale; Source, Dest: PChar; ToOem: Boolean);
function FieldLogicMap(FldType: TFieldType): Integer;
function FieldSubtypeMap(FldType: TFieldType): Integer;
procedure ConvertStringToLogicType(Locale: TLocale; FldLogicType: Integer;
  FldSize: Word; const FldName, Value: string; Buffer: Pointer);
function GetAliasPath(const AliasName: string): string;
function IsDirectory(const DatabaseName: string): Boolean;
function GetBdeDirectory: string;
function BdeErrorMsg(ErrorCode: DBIResult): string;
function LoginToDatabase(Database: TDatabase; OnLogin: TDatabaseLoginEvent): Boolean;
function DataSetFindValue(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
function DataSetFindLike(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
function DataSetRecNo(DataSet: TDataSet): Longint;
function DataSetRecordCount(DataSet: TDataSet): Longint;
function DataSetPositionStr(DataSet: TDataSet): string;
procedure DataSetShowDeleted(DataSet: TBDEDataSet; Show: Boolean);
function CurrentRecordDeleted(DataSet: TBDEDataSet): Boolean;
function IsFilterApplicable(DataSet: TDataSet): Boolean;
function IsBookmarkStable(DataSet: TBDEDataSet): Boolean;
function BookmarksCompare(DataSet: TBDEDataSet; Bookmark1,
  Bookmark2: TBookmark): Integer;
function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
procedure SetIndex(Table: TTable; const IndexFieldNames: string);
procedure RestoreIndex(Table: TTable);
procedure DeleteRange(Table: TTable; IndexFields: array of const;
  FieldValues: array of const);
procedure PackTable(Table: TTable);
procedure ReindexTable(Table: TTable);
procedure BdeFlushBuffers;
function GetNativeHandle(Database: TDatabase; Buffer: Pointer;
  BufSize: Integer): Pointer;
procedure ToggleDebugLayer(Active: Boolean; const DebugFile: string);
procedure DbNotSupported;

{ Export/import DataSet routines }

procedure ExportDataSet(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; MaxRecordCount: Longint);
procedure ExportDataSetEx(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; AsciiDelimiter, AsciiSeparator: Char;
  MaxRecordCount: Longint);
procedure ImportDataSet(Source: TBDEDataSet; DestTable: TTable;
  MaxRecordCount: Longint; Mappings: TStrings; Mode: TBatchMode);

{ ReportSmith initialization }

procedure InitRSRUN(Database: TDatabase; const ConName: string;
  ConType: Integer; const ConServer: string);

implementation

uses
  Forms, Controls, Dialogs, Consts, DBConsts, Math,
  {$IFDEF COMPILER3_UP}
  BDEConst, DBCommon,
  {$ENDIF}
  {$IFNDEF WIN32}
  JvStr16,
  {$ENDIF}
  JvxRConst, JvVCLUtils, JvFileUtil, JvStrUtils, JvDateUtil;

{ Utility routines }

{$IFDEF COMPILER5_UP}

procedure DBError(const Ident: string);
begin
  DatabaseError(Ident);
end;
{$ENDIF}

function IsBookmarkStable(DataSet: TBDEDataSet): Boolean;
var
  Props: CURProps;
begin
  with DataSet do
    Result := Active and (DbiGetCursorProps(Handle, Props) = DBIERR_NONE) and
      Props.bBookMarkStable;
end;

function SetToBookmark(ADataSet: TDataSet; ABookmark: TBookmark): Boolean;
begin
  Result := False;
  {$IFDEF COMPILER3_UP}
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and Eof) and
      BookmarkValid(ABookmark) then
    try
      ADataSet.GotoBookmark(ABookmark);
      Result := True;
    except
    end;
  {$ELSE}
  with TBDEDataSet(ADataSet) do
    if Active and (ABookmark <> nil) and not (Bof and Eof) then
      if DbiSetToBookmark(Handle, ABookmark) = DBIERR_NONE then
      try
        Resync([rmExact, rmCenter]);
        Result := True;
      except
      end;
  {$ENDIF}
end;

function BookmarksCompare(DataSet: TBDEDataSet; Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt =
  ((2, CMPLess), (CMPGtr, CMPEql));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    {$IFDEF WIN32}
    Check(DbiCompareBookmarks(DataSet.Handle, Bookmark1, Bookmark2,
      Result));
    {$ELSE}
    Check(DbiCompareBookmarks(DataSet.Handle, Bookmark1, Bookmark2,
      Word(Result)));
    {$ENDIF}
    if Result = CMPKeyEql then
      Result := CMPEql;
  end;
end;

function DBGetIntProp(const Handle: Pointer; PropName: Longint): Longint;
var
  Length: Word;
  Value: Longint;
begin
  Value := 0;
  Check(DbiGetProp(HDBIObj(Handle), PropName, @Value, SizeOf(Value), Length));
  Result := Value;
end;

{$IFDEF WIN32}

function GetQuoteChar(Database: TDatabase): string;
{$IFNDEF COMPILER3_UP}
const
  dbQUOTECHAR = $0404000A;
  {$ENDIF}
var
  Q: Char;
  Len: Word;
begin
  Result := '';
  if Database.IsSQLBased then
  begin
    Q := #0;
    DbiGetProp(HDBIObj(Database.Handle), dbQUOTECHAR, @Q, SizeOf(Q), Len);
    if Q <> #0 then
      Result := Q;
  end
  else
    Result := '"';
end;
{$ENDIF}

function AsyncQrySupported(Database: TDatabase): Boolean;
begin
  Result := False;
  if Database.Connected then
    if Database.IsSQLBased then
    try
      Result := BOOL(DBGetIntProp(Database.Handle, dbASYNCSUPPORT));
    except
    end
    else
      {$IFDEF WIN32}
      Result := True;
  {$ELSE}
      Result := False;
  {$ENDIF}
end;

function FieldLogicMap(FldType: TFieldType): Integer;
{$IFNDEF COMPILER3_UP}
{$IFDEF DELPHI1}
const
  FldTypeMap: array [TFieldType] of Integer = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldBLOB, fldBLOB, fldBLOB);
  {$ELSE}
const
  FldTypeMap: array [TFieldType] of Integer = (
    fldUNKNOWN, fldZSTRING, fldINT16, fldINT32, fldUINT16, fldBOOL,
    fldFLOAT, fldFLOAT, fldBCD, fldDATE, fldTIME, fldTIMESTAMP, fldBYTES,
    fldVARBYTES, fldINT32, fldBLOB, fldBLOB, fldBLOB, fldBLOB, fldBLOB,
    fldBLOB, fldBLOB);
  {$ENDIF}
  {$ENDIF}
begin
  Result := FldTypeMap[FldType];
end;

function FieldSubtypeMap(FldType: TFieldType): Integer;
{$IFNDEF COMPILER3_UP}
{$IFDEF DELPHI1}
const
  FldSubtypeMap: array [TFieldType] of Integer = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstBINARY,
    fldstMEMO, fldstGRAPHIC);
  {$ELSE}
const
  FldSubtypeMap: array [TFieldType] of Integer = (
    0, 0, 0, 0, 0, 0, 0, fldstMONEY, 0, 0, 0, 0, 0, 0, fldstAUTOINC,
    fldstBINARY, fldstMEMO, fldstGRAPHIC, fldstFMTMEMO, fldstOLEOBJ,
    fldstDBSOLEOBJ, fldstTYPEDBINARY);
  {$ENDIF}
  {$ENDIF}
begin
  Result := FldSubtypeMap[FldType];
end;

{$IFNDEF WIN32}

function CheckOpen(Status: DBIResult): Boolean;
begin
  case Status of
    DBIERR_NONE:
      Result := True;
    DBIERR_NOTSUFFTABLERIGHTS:
      begin
        if not Session.GetPassword then
          DbiError(Status);
        Result := False;
      end;
  else
    DbiError(Status);
  end;
end;
{$ENDIF}

{ Routine for convert string to IDAPI logical field type }

procedure ConvertStringToLogicType(Locale: TLocale; FldLogicType: Integer;
  FldSize: Word; const FldName, Value: string; Buffer: Pointer);
var
  Allocate: Boolean;
  BCD: FMTBcd;
  E: Integer;
  L: Longint;
  B: WordBool;
  DateTime: TDateTime;
  D: Double;
  Data: Longint;
  {$IFDEF WIN32}
  TimeStamp: TTimeStamp;
  {$ELSE}
  DtData: TDateTime;
  {$ENDIF}
begin
  if Buffer = nil then
  begin
    Buffer := AllocMem(FldSize);
    Allocate := Buffer <> nil;
  end
  else
    Allocate := False;
  try
    case FldLogicType of
      fldZSTRING:
        AnsiToNative(Locale, Value, PChar(Buffer), FldSize);
      fldBYTES, fldVARBYTES:
        Move(Value[1], Buffer^, Min(Length(Value), FldSize));
      fldINT16, fldINT32, fldUINT16:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            Val(Value, L, E);
            if E <> 0 then
              {$IFDEF COMPILER3_UP}
              DatabaseErrorFmt(SInvalidIntegerValue, [Value, FldName]);
              {$ELSE}
              DBErrorFmt(SInvalidIntegerValue, [Value, FldName]);
              {$ENDIF}
            Move(L, Buffer^, FldSize);
          end;
        end;
      fldBOOL:
        begin
          L := Length(Value);
          if L = 0 then
            B := False
          else
            B := Value[1] in ['Y', 'y', 'T', 't', '1'];
          Move(B, Buffer^, SizeOf(WordBool));
        end;
      fldFLOAT, fldBCD:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            D := StrToFloat(Value);
            if FldLogicType <> fldBCD then
              Move(D, Buffer^, SizeOf(Double))
            else
            begin
              DbiBcdFromFloat(D, 32, FldSize, BCD);
              Move(BCD, Buffer^, SizeOf(BCD));
            end;
          end;
        end;
      fldDATE:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            DateTime := StrToDate(Value);
            {$IFDEF WIN32}
            TimeStamp := DateTimeToTimeStamp(DateTime);
            Data := TimeStamp.Date;
            {$ELSE}
            Data := Trunc(DateTime);
            {$ENDIF}
            Move(Data, Buffer^, Min(FldSize, SizeOf(Data)));
          end;
        end;
      fldTIME:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            DateTime := StrToTime(Value);
            {$IFDEF WIN32}
            TimeStamp := DateTimeToTimeStamp(DateTime);
            Data := TimeStamp.Time;
            {$ELSE}
            Data := Round(Frac(DateTime) * MSecsPerDay);
            {$ENDIF}
            Move(Data, Buffer^, Min(FldSize, SizeOf(Data)));
          end;
        end;
      fldTIMESTAMP:
        begin
          if Value = '' then
            FillChar(Buffer^, FldSize, 0)
          else
          begin
            DateTime := StrToDateTime(Value);
            {$IFDEF WIN32}
            TimeStamp := DateTimeToTimeStamp(DateTime);
            D := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
            Move(D, Buffer^, Min(FldSize, SizeOf(D)));
            {$ELSE}
            DtData := DateTime * MSecsPerDay;
            Move(DtData, Buffer^, Min(FldSize, SizeOf(DtData)));
            {$ENDIF}
          end;
        end;
    else
      DbiError(DBIERR_INVALIDFLDTYPE);
    end;
  finally
    if Allocate then
      FreeMem(Buffer, FldSize);
  end;
end;

{ Execute Query routine }

procedure ExecuteQueryEx(const SessName, DbName, QueryText: string);
begin
  with TQuery.Create(Application) do
  try
    DatabaseName := DbName;
    {$IFDEF WIN32}
    SessionName := SessName;
    {$ENDIF}
    SQL.Add(QueryText);
    ExecSQL;
  finally
    Free;
  end;
end;

procedure ExecuteQuery(const DbName, QueryText: string);
begin
  ExecuteQueryEx('', DbName, QueryText);
end;

{ Database Login routine }

function LoginToDatabase(Database: TDatabase; OnLogin: TDatabaseLoginEvent): Boolean;
var
  EndLogin: Boolean;
begin
  Result := Database.Connected;
  if Result then
    Exit;
  Database.OnLogin := OnLogin;
  EndLogin := True;
  repeat
    try
      Database.Connected := True;
      EndLogin := True;
    except
      on E: EDbEngineError do
      begin
        EndLogin := (MessageDlg(E.Message + '. ' + SRetryLogin,
          mtConfirmation, [mbYes, mbNo], 0) <> mrYes);
      end;
      on E: EDatabaseError do
      begin
        { User select "Cancel" in login dialog }
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
    else
      raise;
    end;
  until EndLogin;
  Result := Database.Connected;
end;

{ ReportSmith runtime initialization routine }

procedure InitRSRUN(Database: TDatabase; const ConName: string;
  ConType: Integer; const ConServer: string);
const
  IniFileName = 'RPTSMITH.CON';
  scConNames = 'ConnectNamesSection';
  idConNames = 'ConnectNames';
  idType = 'Type';
  idServer = 'Server';
  idSQLDataFilePath = 'Database';
  idDataFilePath = 'DataFilePath';
  idSQLUserID = 'USERID';
var
  ParamList: TStringList;
  DBPath: string[127];
  TempStr, AppConName: string[127];
  UserName: string[30];
  ExeName: string[12];
  IniFile: TIniFile;
begin
  ParamList := TStringList.Create;
  try
    {$IFDEF WIN32}
    Database.Session.GetAliasParams(Database.AliasName, ParamList);
    {$ELSE}
    Session.GetAliasParams(Database.AliasName, ParamList);
    {$ENDIF}
    if Database.IsSQLBased then
      DBPath := ParamList.Values['SERVER NAME']
    else
      DBPath := ParamList.Values['PATH'];
    UserName := ParamList.Values['USER NAME'];
  finally
    ParamList.Free;
  end;
  AppConName := ConName;
  if AppConName = '' then
  begin
    ExeName := ExtractFileName(Application.ExeName);
    AppConName := Copy(ExeName, 1, Pos('.', ExeName) - 1);
  end;
  IniFile := TIniFile.Create(IniFileName);
  try
    TempStr := IniFile.ReadString(scConNames, idConNames, '');
    if Pos(AppConName, TempStr) = 0 then
    begin
      if TempStr <> '' then
        TempStr := TempStr + ',';
      IniFile.WriteString(scConNames, idConNames, TempStr + AppConName);
    end;
    IniFile.WriteInteger(AppConName, idType, ConType);
    IniFile.WriteString(AppConName, idServer, ConServer);
    if Database.IsSQLBased then
    begin
      IniFile.WriteString(AppConName, idSQLDataFilePath, DBPath);
      IniFile.WriteString(AppConName, idSQLUserID, UserName);
    end
    else
      IniFile.WriteString(AppConName, idDataFilePath, DBPath);
  finally
    IniFile.Free;
  end;
end;

{ BDE aliases routines }

function IsDirectory(const DatabaseName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  if (DatabaseName = '') then
    Exit;
  I := 1;
  while I <= Length(DatabaseName) do
  begin
    {$IFDEF COMPILER3_UP}
    if DatabaseName[I] in LeadBytes then
      Inc(I)
    else
      {$ENDIF COMPILER3_UP}
      if DatabaseName[I] in [':', '\'] then
      Exit;
    Inc(I);
  end;
  Result := False;
end;

function GetAliasPath(const AliasName: string): string;
var
  SAlias: DBINAME;
  Desc: DBDesc;
  Params: TStrings;
begin
  Result := '';
  StrPLCopy(SAlias, AliasName, SizeOf(SAlias) - 1);
  AnsiToOem(SAlias, SAlias);
  Check(DbiGetDatabaseDesc(SAlias, @Desc));
  if StrIComp(Desc.szDbType, szCFGDBSTANDARD) = 0 then
  begin
    OemToAnsi(Desc.szPhyName, Desc.szPhyName);
    Result := StrPas(Desc.szPhyName);
  end
  else
  begin
    Params := TStringList.Create;
    try
      {$IFDEF WIN32}
      Session.Active := True;
      {$ENDIF}
      Session.GetAliasParams(AliasName, Params);
      Result := Params.Values['SERVER NAME'];
    finally
      Params.Free;
    end;
  end;
end;

//=== TJvCloneDataset ========================================================

procedure TJvCloneDataset.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

function TJvCloneDataset.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

procedure TJvCloneDataset.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

//=== TJvCloneDbDataset ======================================================

procedure TJvCloneDbDataset.InitFromDataSet(Source: TDBDataSet; Reset: Boolean);
begin
  with Source do
  begin
    {$IFDEF WIN32}
    Self.SessionName := SessionName;
    {$ENDIF}
    Self.DatabaseName := DatabaseName;
    SetSourceHandle(Handle);
    {$IFDEF WIN32}
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
    {$ENDIF}
  end;
  if Reset then
  begin
    {$IFDEF WIN32}
    Filtered := False;
    {$ENDIF}
    First;
  end;
end;

procedure TJvCloneDbDataset.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

function TJvCloneDbDataset.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

procedure TJvCloneDbDataset.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

//=== TJvCloneTable ==========================================================

procedure TJvCloneTable.InitFromTable(SourceTable: TTable; Reset: Boolean);
begin
  with SourceTable do
  begin
    Self.TableType := TableType;
    Self.TableName := TableName;
    {$IFDEF WIN32}
    Self.SessionName := SessionName;
    {$ENDIF}
    Self.DatabaseName := DatabaseName;
    if not Reset then
    begin
      if IndexName <> '' then
        Self.IndexName := IndexName
      else
        if IndexFieldNames <> '' then
        Self.IndexFieldNames := IndexFieldNames;
    end;
    SetSourceHandle(Handle);
    {$IFDEF WIN32}
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
    {$ENDIF}
  end;
  if Reset then
  begin
    {$IFDEF WIN32}
    Filtered := False;
    {$ENDIF}
    DbiResetRange(Handle);
    IndexName := '';
    IndexFieldNames := '';
    First;
  end;
end;

procedure TJvCloneTable.SetSourceHandle(ASourceHandle: HDBICur);
begin
  if ASourceHandle <> FSourceHandle then
  begin
    Close;
    FSourceHandle := ASourceHandle;
    if FSourceHandle <> nil then
      Open;
  end;
end;

procedure TJvCloneTable.SetReadOnly(Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

function TJvCloneTable.CreateHandle: HDBICur;
begin
  Check(DbiCloneCursor(FSourceHandle, FReadOnly, False, Result));
end;

//=== TJvDBLocate ============================================================

function CreateDbLocate: TJvLocateObject;
begin
  Result := TJvDBLocate.Create;
end;

{$IFNDEF WIN32}
function CallbackFilter(pDBLocate: Longint; RecBuf: Pointer;
  RecNo: Longint): Smallint;
  {$IFDEF WIN32} stdcall;{$ELSE} export; {$ENDIF WIN32}
begin
  Result := TJvDBLocate(pDBLocate).RecordFilter(RecBuf, RecNo);
end;
{$ENDIF WIN32}

destructor TJvDBLocate.Destroy;
begin
  {$IFNDEF WIN32}
  DropFilter;
  {$ENDIF}
  inherited Destroy;
end;

procedure TJvDBLocate.CheckFieldType(Field: TField);
var
  Locale: TLocale;
begin
  if not (Field.DataType in [ftDate, ftTime, ftDateTime]) then
  begin
    if DataSet is TBDEDataSet then
      Locale := TBDEDataSet(DataSet).Locale
    else
      Locale := Session.Locale;
    ConvertStringToLogicType(Locale, FieldLogicMap(Field.DataType),
      Field.DataSize, Field.FieldName, LookupValue, nil);
  end;
end;

function TJvDBLocate.UseKey: Boolean;
var
  I: Integer;
begin
  Result := False;
  if DataSet is TTable then
    with DataSet as TTable do
    begin
      if (not Self.LookupField.IsIndexField) and (not IndexSwitch or
        (not CaseSensitive and Database.IsSQLBased)) then
        Exit;
      if (not LookupExact) and (Self.LookupField.DataType <> ftString) then
        Exit;
      IndexDefs.Update;
      for I := 0 to IndexDefs.Count - 1 do
        with IndexDefs[I] do
          if not (ixExpression in Options) and
            ((ixCaseInsensitive in Options) or CaseSensitive) then
            if AnsiCompareText(Fields, Self.LookupField.FieldName) = 0 then
            begin
              Result := True;
              Exit;
            end;
    end;
end;

function TJvDBLocate.LocateKey: Boolean;
var
  Clone: TJvCloneTable;

  function LocateIndex(Table: TTable): Boolean;
  begin
    with Table do
    begin
      SetKey;
      FieldByName(Self.LookupField.FieldName).AsString := LookupValue;
      if LookupExact then
        Result := GotoKey
      else
      begin
        GotoNearest;
        Result := MatchesLookup(FieldByName(Self.LookupField.FieldName));
      end;
    end;
  end;

begin
  try
    TTable(DataSet).CheckBrowseMode;
    if TTable(DataSet).IndexFieldNames = LookupField.FieldName then
      Result := LocateIndex(TTable(DataSet))
    else
    begin
      Clone := TJvCloneTable.Create(DataSet);
      with Clone do
      try
        ReadOnly := True;
        InitFromTable(TTable(DataSet), True);
        IndexFieldNames := Self.LookupField.FieldName;
        Result := LocateIndex(Clone);
        if Result then
        begin
          Check(DbiSetToCursor(TTable(DataSet).Handle, Handle));
          DataSet.Resync([rmExact, rmCenter]);
        end;
      finally
        Free;
      end;
    end;
  except
    Result := False;
  end;
end;

function TJvDBLocate.FilterApplicable: Boolean;
begin
  Result := IsFilterApplicable(DataSet);
end;

{$IFDEF WIN32}

function TJvDBLocate.LocateCallback: Boolean;
var
  Clone: TJvCloneDbDataset;
begin
  Result := False;
  try
    TBDEDataSet(DataSet).CheckBrowseMode;
    Clone := TJvCloneDbDataset.Create(DataSet);
    with Clone do
    try
      ReadOnly := True;
      InitFromDataset(TDBDataSet(DataSet), True);
      OnFilterRecord := RecordFilter;
      Filtered := True;
      if not (BOF and EOF) then
      begin
        First;
        Result := True;
      end;
      if Result then
      begin
        Check(DbiSetToCursor(TBDEDataSet(DataSet).Handle, Handle));
        DataSet.Resync([rmExact, rmCenter]);
      end;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

procedure TJvDBLocate.RecordFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := MatchesLookup(DataSet.FieldByName(LookupField.FieldName));
end;

function TJvDBLocate.LocateFilter: Boolean;
var
  SaveCursor: TCursor;
begin
  if LookupExact or (LookupField.DataType = ftString) or
    not (DataSet is TDBDataSet) then
    Result := inherited LocateFilter
  else
  begin
    SaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      Result := LocateCallback;
    finally
      Screen.Cursor := SaveCursor;
    end;
  end;
end;

{$ELSE WIN32}

type
  TFilterRec = record { the simple filter tree with one condition }
    Header: CANExpr;
    Condition: CANBinary;
    FieldNode: CANField;
    ConstNode: CANConst;
  end;

function TJvDBLocate.LocateFilter: Boolean;
var
  SaveCursor: TCursor;
  Status: DBIResult;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    ActivateFilter;
    try
      Check(DbiSetToBegin(TBDEDataSet(DataSet).Handle));
      Status := DbiGetNextRecord(TBDEDataSet(DataSet).Handle, dbiNoLock,
        nil, nil);
      if Status = DBIERR_NONE then
      begin
        DataSet.Resync([rmExact, rmCenter]);
        ChangeBookmark;
        Result := True;
      end
      else
        Result := False;
    finally
      DeactivateFilter;
      if Result then
        SetToBookmark(DataSet, Bookmark);
    end;
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TJvDBLocate.BuildFilterHeader(var Rec);
const
  FCondition: array [Boolean] of CANOp = (canGE, canEQ);
  FilterHeaderSize = SizeOf(CANExpr) + SizeOf(CANBinary) +
    SizeOf(CANField) + SizeOf(CANConst);
begin
  with TFilterRec(Rec) do
  begin
    with Header do
    begin
      iVer := CANEXPRVERSION;
      iNodes := 3;
      iNodeStart := SizeOf(CANExpr);
      iLiteralStart := FilterHeaderSize;
    end;
    with Condition do
    begin
      nodeClass := nodeBINARY;
      canOp := FCondition[LookupExact];
      iOperand1 := SizeOf(CANBinary);
      iOperand2 := iOperand1 + SizeOf(CANField);
    end;
    with FieldNode do
    begin
      nodeClass := nodeFIELD;
      canOp := canFIELD2;
      iFieldNum := LookupField.FieldNo;
      iNameOffset := 0;
    end;
    with ConstNode do
    begin
      canOp := canCONST2;
      iType := FieldLogicMap(LookupField.DataType);
      iSize := LookupField.DataSize;
      iOffset := Length(LookupField.FieldName) + 1;
    end;
    Header.iTotalSize := FilterHeaderSize + ConstNode.iSize +
      ConstNode.iOffset;
  end;
end;

procedure TJvDBLocate.BuildFilterTree;
var
  Temp: PChar;
  Rec: TFilterRec;
begin
  if FTree <> nil then
    FreeMem(FTree, FTreeSize);
  FTree := nil;
  BuildFilterHeader(Rec);
  FTreeSize := Rec.Header.iTotalSize;
  FTree := AllocMem(FTreeSize);
  try
    FillChar(FTree^, FTreeSize, 0);
    Temp := FTree;
    Move(Rec, FTree^, SizeOf(TFilterRec));
    Inc(Temp, SizeOf(TFilterRec));
    StrPCopy(PChar(Temp), LookupField.FieldName);
    Inc(Temp, Rec.ConstNode.iOffset);
    ConvertStringToLogicType(DataSet.Locale, FieldLogicMap(LookupField.DataType),
      LookupField.DataSize, LookupField.FieldName, LookupValue, Temp);
  except
    FreeTree;
    raise;
  end;
end;

procedure TJvDBLocate.FreeTree;
begin
  if FTree <> nil then
    FreeMem(FTree, FTreeSize);
  FTree := nil;
  FTreeSize := 0;
end;

procedure TJvDBLocate.CheckFilterKind;
var
  NewKind: TLocateFilter;
begin
  if CaseSensitive and LookupExact then
    NewKind := lfTree
  else
    NewKind := lfCallback;
  if (FFilterKind <> NewKind) or (NewKind = lfTree) then
  begin
    DropFilter;
    FFilterKind := NewKind;
  end;
end;

procedure TJvDBLocate.ActivateFilter;
begin
  CheckFilterKind;
  if FFilterHandle = nil then
  begin
    if FFilterKind = lfCallback then
    begin
      Check(DbiAddFilter(DataSet.Handle, Longint(Self), 0, True, nil,
        CallbackFilter, FFilterHandle));
    end
    else { lfTree }
    begin
      BuildFilterTree;
      Check(DbiAddFilter(DataSet.Handle, 0, 1, False,
        pCANExpr(FTree), nil, FFilterHandle));
    end;
  end;
  DbiActivateFilter(DataSet.Handle, FFilterHandle);
end;

procedure TJvDBLocate.DeactivateFilter;
begin
  DbiDeactivateFilter(DataSet.Handle, FFilterHandle);
end;

procedure TJvDBLocate.DropFilter;
begin
  if FFilterHandle <> nil then
    DbiDropFilter(DataSet.Handle, FFilterHandle);
  FreeTree;
  FFilterHandle := nil;
end;

function TJvDBLocate.RecordFilter(RecBuf: Pointer; RecNo: Longint): Smallint;
var
  Accept: Boolean;
begin
  try
    Move(RecBuf^, DataSet.ActiveBuffer^, DataSet.RecordSize);
    if LookupField <> nil then
      Accept := MatchesLookup(LookupField)
    else
      Accept := True;
    Result := Ord(Accept);
  except
    Application.HandleException(Self);
    Result := ABORT;
  end;
end;

procedure TJvDBLocate.ChangeBookmark;
begin
  if Bookmark <> nil then
    DataSet.FreeBookmark(Bookmark);
  Bookmark := DataSet.GetBookmark;
end;

procedure TJvDBLocate.ActiveChanged;
begin
  DropFilter;
end;

{$ENDIF WIN32}

{ DataSet locate routines }

function IsFilterApplicable(DataSet: TDataSet): Boolean;
var
  Status: DBIResult;
  Filter: hDBIFilter;
begin
  if DataSet is TBDEDataSet then
  begin
    Status := DbiAddFilter(TBDEDataSet(DataSet).Handle, 0, 0, False, nil,
      nil, Filter);
    Result := (Status = DBIERR_NONE) or (Status = DBIERR_INVALIDFILTER);
    if Result then
      DbiDropFilter(TBDEDataSet(DataSet).Handle, Filter);
  end
  else
    Result := True;
end;

function DataSetFindValue(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
begin
  with TJvDBLocate.Create do
  try
    DataSet := ADataSet;
    if ADataSet is TDBDataSet then
      IndexSwitch := not TDBDataSet(DataSet).Database.IsSQLBased;
    Result := Locate(FieldName, Value, True, False);
  finally
    Free;
  end;
end;

function DataSetFindLike(ADataSet: TBDEDataSet; const Value,
  FieldName: string): Boolean;
begin
  with TJvDBLocate.Create do
  try
    DataSet := ADataSet;
    if ADataSet is TDBDataSet then
      IndexSwitch := not TDBDataSet(DataSet).Database.IsSQLBased;
    Result := Locate(FieldName, Value, False, False);
  finally
    Free;
  end;
end;

// (rom) changed to var
var
  SaveIndexFieldNames: TStrings = nil;

procedure UsesSaveIndexies;
begin
  if SaveIndexFieldNames = nil then
    SaveIndexFieldNames := TStringList.Create;
end;

procedure ReleaseSaveIndexies; far;
begin
  if SaveIndexFieldNames <> nil then
  begin
    SaveIndexFieldNames.Free;
    SaveIndexFieldNames := nil;
  end;
end;

procedure SetIndex(Table: TTable; const IndexFieldNames: string);
var
  IndexToSave: string;
begin
  IndexToSave := Table.IndexFieldNames;
  Table.IndexFieldNames := IndexFieldNames;
  UsesSaveIndexies;
  SaveIndexFieldNames.AddObject(IndexToSave, Table.MasterSource);
end;

procedure RestoreIndex(Table: TTable);
begin
  if (SaveIndexFieldNames <> nil) and (SaveIndexFieldNames.Count > 0) then
  begin
    try
      Table.IndexFieldNames :=
        SaveIndexFieldNames[SaveIndexFieldNames.Count - 1];
      Table.MasterSource :=
        TDataSource(SaveIndexFieldNames.Objects[SaveIndexFieldNames.Count - 1]);
    finally
      SaveIndexFieldNames.Delete(SaveIndexFieldNames.Count - 1);
      if SaveIndexFieldNames.Count = 0 then
        ReleaseSaveIndexies;
    end;
  end;
end;

procedure DeleteRange(Table: TTable; IndexFields: array of const;
  FieldValues: array of const);
var
  I: Integer;
  NewIndex: string;
begin
  NewIndex := '';
  for I := Low(IndexFields) to High(IndexFields) do
  begin
    NewIndex := NewIndex + TVarRec(IndexFields[I]).VString^;
    if I <> High(IndexFields) then
      NewIndex := NewIndex + ';';
  end;
  SetIndex(Table, NewIndex);
  try
    Table.SetRange(FieldValues, FieldValues);
    try
      while not Table.EOF do
        Table.Delete;
    finally
      Table.CancelRange;
    end;
  finally
    RestoreIndex(Table);
  end;
end;

procedure ReindexTable(Table: TTable);
var
  WasActive: Boolean;
  WasExclusive: Boolean;
begin
  with Table do
  begin
    WasActive := Active;
    WasExclusive := Exclusive;
    DisableControls;
    try
      if not (WasActive and WasExclusive) then
        Close;
      try
        Exclusive := True;
        Open;
        Check(dbiRegenIndexes(Handle));
      finally
        if not (WasActive and WasExclusive) then
        begin
          Close;
          Exclusive := WasExclusive;
          Active := WasActive;
        end;
      end;
    finally
      EnableControls;
    end;
  end;
end;

procedure PackTable(Table: TTable);
{ This routine copied and modified from demo unit TableEnh.pas
  from Borland Int. }
var
  { FCurProp holds information about the structure of the table }
  FCurProp: CurProps;
  { Specific information about the table structure, indexes, etc. }
  TblDesc: CRTblDesc;
  { Uses as a handle to the database }
  hDb: hDbiDB;
  { Path to the currently opened table }
  TablePath: array [0..dbiMaxPathLen] of Char;
  Exclusive: Boolean;
begin
  if not Table.Active then
    _DBError(SDataSetClosed);
  Check(DbiGetCursorProps(Table.Handle, FCurProp));
  if StrComp(FCurProp.szTableType, szParadox) = 0 then
  begin
    { Call DbiDoRestructure procedure if PARADOX table }
    hDb := nil;
    { Initialize the table descriptor }
    FillChar(TblDesc, SizeOf(CRTblDesc), 0);
    with TblDesc do
    begin
      { Place the table name in descriptor }
      StrPCopy(szTblName, Table.TableName);
      { Place the table type in descriptor }
      StrCopy(szTblType, FCurProp.szTableType);
      bPack := True;
      bProtected := FCurProp.bProtected;
    end;
    { Get the current table's directory. This is why the table MUST be
      opened until now }
    Check(DbiGetDirectory(Table.DBHandle, False, TablePath));
    { Close the table }
    Table.Close;
    try
      { NOW: since the DbiDoRestructure call needs a valid DB handle BUT the
        table cannot be opened, call DbiOpenDatabase to get a valid handle.
        Setting TTable.Active = False does not give you a valid handle }
      Check(DbiOpenDatabase(nil, szCFGDBSTANDARD, dbiReadWrite, dbiOpenExcl, nil,
        0, nil, nil, hDb));
      { Set the table's directory to the old directory }
      Check(DbiSetDirectory(hDb, TablePath));
      { Pack the PARADOX table }
      Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
      { Close the temporary database handle }
      Check(DbiCloseDatabase(hDb));
    finally
      { Re-Open the table }
      Table.Open;
    end;
  end
  else
    if StrComp(FCurProp.szTableType, szDBase) = 0 then
  begin
    { Call DbiPackTable procedure if dBase table }
    Exclusive := Table.Exclusive;
    Table.Close;
    try
      Table.Exclusive := True;
      Table.Open;
      try
        Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, nil, True));
      finally
        Table.Close;
      end;
    finally
      Table.Exclusive := Exclusive;
      Table.Open;
    end;
  end
  else
    DbiError(DBIERR_WRONGDRVTYPE);
end;

procedure FetchAllRecords(DataSet: TBDEDataSet);
begin
  with DataSet do
    if not EOF then
    begin
      CheckBrowseMode;
      Check(DbiSetToEnd(Handle));
      Check(DbiGetPriorRecord(Handle, dbiNoLock, nil, nil));
      CursorPosChanged;
      UpdateCursorPos;
    end;
end;

procedure BdeFlushBuffers;
var
  I, L: Integer;
  {$IFDEF WIN32}
  Session: TSession;
  J: Integer;
  {$ENDIF}
begin
  {$IFDEF WIN32}
  for J := 0 to Sessions.Count - 1 do
  begin
    Session := Sessions[J];
    if not Session.Active then
      Continue;
    {$ENDIF}
    for I := 0 to Session.DatabaseCount - 1 do
    begin
      with Session.Databases[I] do
        if Connected and not IsSQLBased then
        begin
          for L := 0 to DataSetCount - 1 do
          begin
            if DataSets[L].Active then
              DbiSaveChanges(DataSets[L].Handle);
          end;
        end;
    end;
    {$IFDEF WIN32}
  end;
  {$ENDIF}
end;

{$IFNDEF WIN32}
type
  TDbiGetExactRecordCount = function(hCursor: hDBICur;
    var iRecCount: Longint): DbiResult;

const
  DbiGetExactRecCnt: TDbiGetExactRecordCount = nil;

function DbiGetExactRecordCount(hCursor: hDBICur;
  var iRecCount: Longint): DbiResult;
var
  HModule: THandle;
  ErrMode: Cardinal;
begin
  if not Assigned(DbiGetExactRecCnt) then
  begin
    ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
    HModule := LoadLibrary('IDAPI01.DLL');
    SetErrorMode(ErrMode);
    if HModule >= HINSTANCE_ERROR then
    begin
      @DbiGetExactRecCnt := GetProcAddress(HModule, 'DBIGETEXACTRECORDCOUNT');
      FreeLibrary(HModule);
    end;
  end;
  if Assigned(DbiGetExactRecCnt) then
    Result := DbiGetExactRecCnt(hCursor, iRecCount)
  else
    Result := DbiGetRecordCount(hCursor, iRecCount);
end;
{$ENDIF}

function DataSetRecordCount(DataSet: TDataSet): Longint;
var
  IsCount: Boolean;
begin
  {$IFDEF COMPILER3_UP}
  if DataSet is TBDEDataSet then
  begin
  {$ENDIF}
    IsCount := (DbiGetExactRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE) or (DbiGetRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE);
  {$IFDEF COMPILER3_UP}
  end
  else
  try
    Result := DataSet.RecordCount;
    IsCount := True;
  except
    IsCount := False;
  end;
  {$ENDIF}
  if not IsCount then
    Result := -1;
end;

function DataSetRecNo(DataSet: TDataSet): Longint;
var
  FCurProp: CURProps;
  FRecProp: RECProps;
begin
  Result := -1;
  if (DataSet <> nil) and DataSet.Active and (DataSet.State in [dsBrowse,
    dsEdit]) then
  begin
    {$IFDEF COMPILER3_UP}
    if not (DataSet is TBDEDataSet) then
    begin
      Result := DataSet.RecNo;
      Exit;
    end;
    {$ENDIF}
    if DbiGetCursorProps(TBDEDataSet(DataSet).Handle, FCurProp) <> DBIERR_NONE then
      Exit;
    if (StrComp(FCurProp.szTableType, szParadox) = 0) or
      (FCurProp.iSeqNums = 1) then
    begin
      DataSet.GetCurrentRecord(nil);
      if DbiGetSeqNo(TBDEDataSet(DataSet).Handle, Result) <> DBIERR_NONE then
        Result := -1;
    end
    else
      if StrComp(FCurProp.szTableType, szDBase) = 0 then
    begin
      DataSet.GetCurrentRecord(nil);
      if DbiGetRecord(TBDEDataSet(DataSet).Handle, dbiNOLOCK, nil, @FRecProp) = DBIERR_NONE
        then
        Result := FRecProp.iPhyRecNum;
    end;
  end;
end;

function DataSetPositionStr(DataSet: TDataSet): string;
var
  RecNo, RecCount: Longint;
begin
  try
    RecNo := DataSetRecNo(DataSet);
  except
    RecNo := -1;
  end;
  if RecNo >= 0 then
  begin
    RecCount := DataSetRecordCount(DataSet);
    if RecCount >= 0 then
      Result := Format('%d:%d', [RecNo, RecCount])
    else
      Result := IntToStr(RecNo);
  end
  else
    Result := '';
end;

function TransActive(Database: TDatabase): Boolean;
var
  Info: XInfo;
  {$IFDEF WIN32}
  S: hDBISes;
  {$ENDIF}
begin
  {$IFDEF WIN32}
  Result := False;
  if DbiGetCurrSession(S) <> DBIERR_NONE then
    Exit;
  {$ENDIF}
  Result := (Database.Handle <> nil) and
    (DbiGetTranInfo(Database.Handle, nil, @Info) = DBIERR_NONE) and
    (Info.exState = xsActive);
  {$IFDEF WIN32}
  DbiSetCurrSession(S);
  {$ENDIF}
end;

function GetBdeDirectory: string;
const
  Ident = 'DLLPATH';
var
  {$IFDEF WIN32}
  Ini: TRegistry;
const
  BdeKey = 'SOFTWARE\Borland\Database Engine';
  {$ELSE}
  Ini: TIniFile;
  {$ENDIF}
begin
  Result := '';
  {$IFDEF WIN32}
  Ini := TRegistry.Create;
  try
    Ini.RootKey := HKEY_LOCAL_MACHINE;
    if Ini.OpenKey(BdeKey, False) then
      if Ini.ValueExists(Ident) then
        Result := Ini.ReadString(Ident);
  {$ELSE}
  Ini := TIniFile.Create('WIN.INI');
  try
    Result := Ini.ReadString('IDAPI', Ident, '');
  {$ENDIF}
  { Check for multiple directories, use only the first one }
    if Pos(';', Result) > 0 then
      Delete(Result, Pos(';', Result), MaxInt);
    if (Length(Result) > 2) and (Result[Length(Result)] <> '\') then
      Result := Result + '\';
  finally
    Ini.Free;
  end;
end;

procedure ExportDataSetEx(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; AsciiDelimiter, AsciiSeparator: Char;
  MaxRecordCount: Longint);

  function ExportAsciiField(Field: TField): Boolean;
  begin
    Result := Field.Visible and not (Field.Calculated
      {$IFDEF WIN32} or Field.Lookup {$ENDIF}) and not (Field.DataType in
      ftNonTextTypes + [ftUnknown]);
  end;

const
  TextExt = '.TXT';
  SchemaExt = '.SCH';
var
  I: Integer;
  S, Path: string;
  BatchMove: TBatchMove;
  TablePath: array[0..dbiMaxPathLen] of Char;
begin
  if Source = nil then
    _DBError(SDataSetEmpty);
  if DestTable.Active then
    DestTable.Close;
  {$IFDEF WIN32}
  if Source is TDBDataSet then
    DestTable.SessionName := TDBDataSet(Source).SessionName;
  {$ENDIF}
  if (TableType = ttDefault) then
  begin
    if DestTable.TableType <> ttDefault then
      TableType := DestTable.TableType
    else
      if (CompareText(ExtractFileExt(DestTable.TableName), TextExt) = 0) then
      TableType := ttASCII;
  end;
  BatchMove := TBatchMove.Create(Application);
  try
    StartWait;
    try
      BatchMove.Mode := batCopy;
      BatchMove.Source := Source;
      BatchMove.Destination := DestTable;
      DestTable.TableType := TableType;
      BatchMove.Mappings.Clear;
      if (DestTable.TableType = ttASCII) then
      begin
        if CompareText(ExtractFileExt(DestTable.TableName), SchemaExt) = 0 then
          DestTable.TableName := ChangeFileExt(DestTable.TableName, TextExt);
        with Source do
          for I := 0 to FieldCount - 1 do
          begin
            if ExportAsciiField(Fields[I]) then
              BatchMove.Mappings.Add(Format('%s=%0:s',
                [Fields[I].FieldName]));
          end;
        BatchMove.RecordCount := 1;
      end
      else
        BatchMove.RecordCount := MaxRecordCount;
      BatchMove.Execute;
      if (DestTable.TableType = ttASCII) then
      begin
        { ASCII table always created in "fixed" format with "ascii"
          character set }
        with BatchMove do
        begin
          Mode := batAppend;
          RecordCount := MaxRecordCount;
        end;
        S := ChangeFileExt(ExtractFileName(DestTable.TableName), '');
        Path := NormalDir(ExtractFilePath(DestTable.TableName));
        if Path = '' then
        begin
          DestTable.Open;
          try
            Check(DbiGetDirectory(DestTable.DBHandle, False, TablePath));
            Path := NormalDir(OemToAnsiStr(StrPas(TablePath)));
          finally
            DestTable.Close;
          end;
        end;
        with TIniFile.Create(ChangeFileExt(Path + S, SchemaExt)) do
        try
          if AsciiCharSet <> '' then
            WriteString(S, 'CharSet', AsciiCharSet)
          else
            WriteString(S, 'CharSet', 'ascii');
          if AsciiDelimited then
          begin { change ASCII-file format to CSV }
            WriteString(S, 'Filetype', 'VARYING');
            WriteString(S, 'Delimiter', AsciiDelimiter);
            WriteString(S, 'Separator', AsciiSeparator);
          end;
        finally
          Free;
        end;
        { clear previous output - overwrite existing file }
        S := Path + ExtractFileName(DestTable.TableName);
        if Length(ExtractFileExt(S)) < 2 then
          S := ChangeFileExt(S, TextExt);
        I := FileCreate(S);
        if I < 0 then
          raise EFCreateError.CreateFmt(ResStr(SFCreateError), [S]);
        FileClose(I);
        BatchMove.Execute;
      end;
    finally
      StopWait;
    end;
  finally
    BatchMove.Free;
  end;
end;

procedure ExportDataSet(Source: TBDEDataSet; DestTable: TTable;
  TableType: TTableType; const AsciiCharSet: string;
  AsciiDelimited: Boolean; MaxRecordCount: Longint);
begin
  ExportDataSetEx(Source, DestTable, TableType, AsciiCharSet,
    AsciiDelimited, '"', ',', MaxRecordCount);
end;

procedure ImportDataSet(Source: TBDEDataSet; DestTable: TTable;
  MaxRecordCount: Longint; Mappings: TStrings; Mode: TBatchMode);
var
  BatchMove: TBatchMove;
begin
  if Source = nil then
    _DBError(SDataSetEmpty);
  {$IFDEF WIN32}
  if (Source is TDBDataSet) and not Source.Active then
    TDBDataSet(Source).SessionName := DestTable.SessionName;
  {$ENDIF}
  BatchMove := TBatchMove.Create(Application);
  try
    StartWait;
    try
      BatchMove.Mode := Mode;
      BatchMove.Source := Source;
      BatchMove.Destination := DestTable;
      if Mappings.Count > 0 then
        BatchMove.Mappings.AddStrings(Mappings);
      BatchMove.RecordCount := MaxRecordCount;
      BatchMove.Execute;
    finally
      StopWait;
    end;
  finally
    BatchMove.Free;
  end;
end;

function GetNativeHandle(Database: TDatabase; Buffer: Pointer;
  BufSize: Integer): Pointer;
var
  Len: Word;
begin
  Result := nil;
  if Assigned(Database) and Database.Connected then
  begin
    if Database.IsSQLBased then
    begin
      Check(DbiGetProp(HDBIOBJ(Database.Handle), dbNATIVEHNDL,
        Buffer, BufSize, Len));
      Result := Buffer;
    end
    else
      DBError(SLocalDatabase);
  end
  else
    _DBError(SDatabaseClosed);
end;

procedure BdeTranslate(Locale: TLocale; Source, Dest: PChar; ToOem: Boolean);
var
  Len: Cardinal;
begin
  Len := StrLen(Source);
  if ToOem then
    AnsiToNativeBuf(Locale, Source, Dest, Len)
  else
    NativeToAnsiBuf(Locale, Source, Dest, Len);
  if Source <> Dest then
    Dest[Len] := #0;
end;

function TrimMessage(Msg: PChar): PChar;
var
  Blank: Boolean;
  Source, Dest: PChar;
begin
  Source := Msg;
  Dest := Msg;
  Blank := False;
  while Source^ <> #0 do
  begin
    if Source^ <= ' ' then
      Blank := True
    else
    begin
      if Blank then
      begin
        Dest^ := ' ';
        Inc(Dest);
        Blank := False;
      end;
      Dest^ := Source^;
      Inc(Dest);
    end;
    Inc(Source);
  end;
  if (Dest > Msg) and ((Dest - 1)^ = '.') then
    Dec(Dest);
  Dest^ := #0;
  Result := Msg;
end;

function BdeErrorMsg(ErrorCode: DBIResult): string;
var
  I: Integer;
  NativeError: Longint;
  Msg, LastMsg: DBIMSG;
begin
  I := 1;
  DbiGetErrorString(ErrorCode, Msg);
  TrimMessage(Msg);
  if Msg[0] = #0 then
    Result := Format(ResStr(SBDEError), [ErrorCode])
  else
    Result := StrPas(Msg);
  while True do
  begin
    StrCopy(LastMsg, Msg);
    ErrorCode := DbiGetErrorEntry(I, NativeError, Msg);
    if (ErrorCode = DBIERR_NONE) or
      (ErrorCode = DBIERR_NOTINITIALIZED) then
      Break;
    TrimMessage(Msg);
    if (Msg[0] <> #0) and (StrComp(Msg, LastMsg) <> 0) then
      Result := Format('%s. %s', [Result, Msg]);
    Inc(I);
  end;
  for I := 1 to Length(Result) do
    if Result[I] < ' ' then
      Result[I] := ' ';
end;

procedure DataSetShowDeleted(DataSet: TBDEDataSet; Show: Boolean);
begin
  with DataSet do
  begin
    CheckBrowseMode;
    Check(DbiValidateProp(hDBIObj(Handle), curSOFTDELETEON, True));
    DisableControls;
    try
      Check(DbiSetProp(hDBIObj(Handle), curSOFTDELETEON, Ord(Show)));
    finally
      EnableControls;
    end;
    if DataSet is TTable then
      TTable(DataSet).Refresh
    else
    begin
      CursorPosChanged;
      First;
    end;
  end;
end;

function CurrentRecordDeleted(DataSet: TBDEDataSet): Boolean;
var
  FRecProp: RECProps;
begin
  Result := False;
  if (DataSet <> nil) and DataSet.Active then
  begin
    DataSet.GetCurrentRecord(nil);
    if DbiGetRecord(DataSet.Handle, dbiNOLOCK, nil, @FRecProp) = DBIERR_NONE
      then
      Result := FRecProp.bDeleteFlag;
  end;
end;

procedure DbNotSupported;
begin
  DbiError(DBIERR_NOTSUPPORTED);
end;

procedure ToggleDebugLayer(Active: Boolean; const DebugFile: string);
const
  Options: array[Boolean] of Longint = (0, DEBUGON or OUTPUTTOFILE or
    APPENDTOLOG);
var
  FileName: DBIPATH;
begin
  Check(DbiDebugLayerOptions(Options[Active], StrPLCopy(FileName,
    DebugFile, SizeOf(DBIPATH) - 1)));
end;

initialization
  JvDBUtils.CreateLocateObject := CreateDbLocate;
  
{$IFDEF WIN32}
finalization
  ReleaseSaveIndexies;
{$ELSE}
  AddExitProc(ReleaseSaveIndexies);
{$ENDIF}

end.

