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

Contributor(s):
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBdeUtils;

interface

uses
  Windows, SysUtils, Bde, Registry,
  {$IFDEF COMPILER6_UP}
  Variants, RTLConsts,
  {$ENDIF COMPILER6_UP}
  Classes, DB, DBTables, IniFiles,
  JvDBUtils;

type
  TJvDBLocate = class(TJvLocateObject)
  private
    function LocateCallback: Boolean;
    procedure RecordFilter(DataSet: TDataSet; var Accept: Boolean);
  protected
    function LocateFilter: Boolean; override;
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
procedure FetchAllRecords(DataSet: TBDEDataSet);
function TransActive(Database: TDatabase): Boolean;
function AsyncQrySupported(Database: TDatabase): Boolean;
function GetQuoteChar(Database: TDatabase): string;
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

{ begin JvDBUtil }
{ ExecuteSQLScript executes SQL script }

procedure ExecuteSQLScript(Base: TDataBase; const Script: string; const Commit: TCommit; OnProgress: TJvDBProgressEvent; const UserData: Integer);

{ GetQueryResult executes SQL Query and returns Result as Variant }

function GetQueryResult(const DatabaseName, SQL: string): Variant;

{ GetStoredProcResult executes SQL stored procedure and returns
  value of ResultName parameters as Variant }

function GetStoredProcResult(const ADatabaseName, AStoredProcName: string; AParams: array of Variant;
  const AResultName: string): Variant;

{ StrFieldDesc returns field description of given FLDDesc record }

function StrFieldDesc(Field: FLDDesc): string;

function Var2Type(V: Variant; const VarType: Integer): Variant;

procedure CopyRecord(DataSet: TDataSet);

{ AddReference create reference for paradox table,
  RefField and MasterField are field numbers (first field has number 1)
  Tables allready must have indices for this fields }

procedure AddReference(Tbl: TTable; RefName: string; RefField: Word;
  MasterTable: string; MasterField: Word; ModOp, DelOp: RINTQual);

{ AddMasterPassword extracted from "bde.hlp" file }
procedure AddMasterPassword(Table: TTable; pswd: string);

procedure PackEncryptedTable(Table: TTable; pswd: string);

function EncodeQuotes(const S: string): string;

{*********************** from JvStrUtil unit ***********************}

function Cmp(const S1, S2: string): Boolean;

{ SubStr returns substring from string, S,
  separated with Separator string}

function SubStr(const S: string; const Index: Integer; const Separator: string): string;

{ SubStrEnd same to previous function but Index numerated
  from the end of string }

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }

function ReplaceString(S: string; const OldPattern, NewPattern: string): string;

{ GetXYByPos is same to previous function, but
  returns X position in line too}

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);

{####################### from JvStrUtil unit #######################}

{ end JvDBUtil }

implementation

uses
  Forms, Controls, Dialogs, Consts, DBConsts, Math,
  BDEConst, DBCommon,
  JvConsts, JvJVCLUtils, JvJCLUtils, JvTypes, JvResources;

{ Utility routines }

procedure DBError(const Ident: string);
begin
  DatabaseError(Ident);
end;

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
  with ADataSet do
    if Active and (ABookmark <> nil) and not (Bof and Eof) and
      BookmarkValid(ABookmark) then
    try
      ADataSet.GotoBookmark(ABookmark);
      Result := True;
    except
    end;
end;

function BookmarksCompare(DataSet: TBDEDataSet; Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt =
  ((2, CMPLess), (CMPGtr, CMPEql));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
  begin
    Check(DbiCompareBookmarks(DataSet.Handle, Bookmark1, Bookmark2,
      Result));
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

function GetQuoteChar(Database: TDatabase): string;
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
      Result := True;
end;

function FieldLogicMap(FldType: TFieldType): Integer;
begin
  Result := FldTypeMap[FldType];
end;

function FieldSubtypeMap(FldType: TFieldType): Integer;
begin
  Result := FldSubtypeMap[FldType];
end;

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
  TimeStamp: TTimeStamp;
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
              DatabaseErrorFmt(SInvalidIntegerValue, [Value, FldName]);
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
            TimeStamp := DateTimeToTimeStamp(DateTime);
            Data := TimeStamp.Date;
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
            TimeStamp := DateTimeToTimeStamp(DateTime);
            Data := TimeStamp.Time;
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
            TimeStamp := DateTimeToTimeStamp(DateTime);
            D := TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
            Move(D, Buffer^, Min(FldSize, SizeOf(D)));
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
    SessionName := SessName;
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
        EndLogin := (MessageDlg(E.Message + '. ' + RsRetryLogin,
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
    Database.Session.GetAliasParams(Database.AliasName, ParamList);
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
    if DatabaseName[I] in LeadBytes then
      Inc(I)
    else
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
      Session.Active := True;
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
    Self.SessionName := SessionName;
    Self.DatabaseName := DatabaseName;
    SetSourceHandle(Handle);
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
  end;
  if Reset then
  begin
    Filtered := False;
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
    Self.SessionName := SessionName;
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
    Self.Filter := Filter;
    Self.OnFilterRecord := OnFilterRecord;
    if not Reset then
      Self.Filtered := Filtered;
  end;
  if Reset then
  begin
    Filtered := False;
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

destructor TJvDBLocate.Destroy;
begin
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
  SaveIndexFieldNames: TStringList = nil;

procedure UsesSaveIndexies;
begin
  if SaveIndexFieldNames = nil then
    SaveIndexFieldNames := TStringList.Create;
end;

procedure ReleaseSaveIndices;
begin
  FreeAndNil(SaveIndexFieldNames);
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
        ReleaseSaveIndices;
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
  Session: TSession;
  J: Integer;
begin
  for J := 0 to Sessions.Count - 1 do
  begin
    Session := Sessions[J];
    if not Session.Active then
      Continue;
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
  end;
end;

function DataSetRecordCount(DataSet: TDataSet): Longint;
var
  IsCount: Boolean;
begin
  if DataSet is TBDEDataSet then
  begin
    IsCount := (DbiGetExactRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE) or (DbiGetRecordCount(TBDEDataSet(DataSet).Handle,
      Result) = DBIERR_NONE);
  end
  else
  try
    Result := DataSet.RecordCount;
    IsCount := True;
  except
    IsCount := False;
  end;
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
    if not (DataSet is TBDEDataSet) then
    begin
      Result := DataSet.RecNo;
      Exit;
    end;
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
  S: hDBISes;
begin
  Result := False;
  if DbiGetCurrSession(S) <> DBIERR_NONE then
    Exit;
  Result := (Database.Handle <> nil) and
    (DbiGetTranInfo(Database.Handle, nil, @Info) = DBIERR_NONE) and
    (Info.exState = xsActive);
  DbiSetCurrSession(S);
end;

function GetBdeDirectory: string;
const
  Ident = 'DLLPATH';
var
  Ini: TRegistry;
const
  BdeKey = 'SOFTWARE\Borland\Database Engine';
begin
  Result := '';
  Ini := TRegistry.Create;
  try
    Ini.RootKey := HKEY_LOCAL_MACHINE;
    if Ini.OpenKey(BdeKey, False) then
      if Ini.ValueExists(Ident) then
        Result := Ini.ReadString(Ident);
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
      or Field.Lookup) and not (Field.DataType in
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
  if Source is TDBDataSet then
    DestTable.SessionName := TDBDataSet(Source).SessionName;
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
          raise EFCreateError.CreateResFmt(@SFCreateError, [S]);
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
  if (Source is TDBDataSet) and not Source.Active then
    TDBDataSet(Source).SessionName := DestTable.SessionName;
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
      DBError(RsELocalDatabase);
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
    Result := Format(SBDEError, [ErrorCode])
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
{ begin JvDBUtil }

procedure ExecuteSQLScript(Base: TDataBase; const Script: string; const Commit: TCommit; OnProgress: TJvDBProgressEvent; const UserData: Integer);
var
  N: Integer;
  Term: Char;

  function NextQuery: string;
  var
    C: Char;
    Rem: Boolean;
  begin
    Result := '';
    Rem := False;
    while Length(Script) >= N do
    begin
      C := Script[N];
      Inc(N);
      if (C = Term) and not Rem then
        Exit;
      Result := Result + C;
      if (C = '/') and (Length(Script) >= N) and (Script[N] = '*') then
        Rem := True;
      if (C = '*') and (Length(Script) >= N) and (Script[N] = '/') and Rem then
        Rem := False;
    end;
    Result := '';
  end;

  function SetTerm(S: string): Boolean;
  var
    Rem: Boolean;
  begin
    Rem := False;
    while (Length(S) > 0) do
    begin
      if (S[1] in [' ', Cr, Lf]) then
        Delete(S, 1, 1)
      else
      if Rem then
        if (S[1] = '*') and (Length(S) > 1) and (S[2] = '/') then
        begin
          Delete(S, 1, 2);
          Rem := False;
        end
        else
          Delete(S, 1, 1)
      else
      if (S[1] = '/') and (Length(S) > 1) and (S[2] = '*') then
      begin
        Delete(S, 1, 2);
        Rem := True;
      end
      else
        Break;
    end;
    Result := AnsiStrLIComp(PChar(S), 'set term', 8) = 0;
    if Result then
    begin
      S := Trim(Copy(S, 9, 1024));
      if Length(S) = 1 then
        Term := S[1]
      else
        EDatabaseError.Create('Bad term');
      Exit;
    end;
    Result := AnsiStrLIComp(PChar(S), 'commit work', 11) = 0;
    if Result then
    begin
      Base.Commit;
      Base.StartTransaction;
      Exit;
    end;
  end;

var
  Q: string;
  ErrPos: Integer;
  NBeg: Integer;
  X, Y, N2: Integer;
  S1: string;
  Query: TQuery;
  Stop: Boolean;
begin
  if Commit in [ctStep, ctAll] then
    Base.StartTransaction;
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := Base.DatabaseName;
    Query.ParamCheck := False;
    N := 1;
    Term := ';';
    Stop := False;
    NBeg := 1;
    try
      Q := NextQuery;
      while Q <> '' do
      begin
        if not SetTerm(Q) then
        begin
          if Assigned(OnProgress) then
          begin
            S1 := Q;
            N2 := 0;
            while (Length(S1) > 0) and (S1[1] in [' ', Cr, Lf]) do
            begin
              Delete(S1, 1, 1);
              Inc(N2);
            end;
            GetXYByPos(Script, NBeg + N2, X, Y);
            if Assigned(OnProgress) then
              OnProgress(UserData, Stop, Y)
            else
              // (rom) i do not like this
              Application.ProcessMessages;
            if Stop then
              SysUtils.Abort;
          end;
          Query.SQL.Text := Q;
          Query.ExecSQL;
          if Commit = ctStep then
          begin
            Base.Commit;
            Base.StartTransaction;
          end;
          Query.Close;
        end;
        NBeg := N + 1;
        Q := NextQuery;
      end;
      if Commit in [ctStep, ctAll] then
        Base.Commit;
    except
      on E: Exception do
      begin
        if Commit in [ctStep, ctAll] then
          Base.Rollback;
        if E is EDatabaseError then
        begin
          ErrPos := NBeg;
          //..
          raise EJvScriptError.Create(E.Message, ErrPos)
        end
        else
          raise;
      end;
    end;
  finally
    Query.Free;
  end;
end;

function GetQueryResult(const DatabaseName, SQL: string): Variant;
var
  Query: TQuery;
begin
  Query := TQuery.Create(Application);
  try
    Query.DatabaseName := DatabaseName;
    Query.ParamCheck := False;
    Query.SQL.Text := SQL;
    Query.Open;
    Result := Query.Fields[0].AsVariant;
  finally
    Query.Free;
  end;
end;

function GetStoredProcResult(const ADatabaseName, AStoredProcName: string; AParams: array of Variant;
  const AResultName: string): Variant;
var
  I: Integer;
begin
  with TStoredProc.Create(Application) do
    try
      DatabaseName := ADatabaseName;
      ParamBindMode := pbByNumber;
      StoredProcName := AStoredProcName;
      Prepare;
      for I := Low(AParams) to High(AParams) do
        Params[I].Value := AParams[I];
      ExecProc;
      Result := ParamByName(AResultName).Value;
    finally
      Free;
    end;
end;

function StrFieldDesc(Field: FLDDesc): string;

  function SUnits1: string;
  begin
    Result := IntToStr(Field.iUnits1);
  end;

  function SUnits2: string;
  begin
    if Field.iUnits2 < 0 then
      Result := IntToStr(-Field.iUnits2)
    else
      Result := IntToStr(Field.iUnits2);
  end;

begin
  with Field do
    case iFldType of
      fldUNKNOWN:
        Result := 'unknown';
      fldZSTRING:
       Result := 'string'; { Null terminated string }
      fldDATE:
        Result := 'date'; { Date (32 bit) }
      fldBLOB:
        Result := 'blob'; { Blob }
      fldBOOL:
        Result := 'boolean'; { Boolean  (16 bit) }
      fldINT16:
        Result := 'integer'; { 16 bit signed number }
      fldINT32:
        Result := 'long integer'; { 32 bit signed number }

      fldFLOAT:
        Result := 'float'; { 64 bit floating point }
      fldBCD:
        Result := 'BCD'; { BCD }
      fldBYTES:
        Result := 'bytes'; { Fixed number of bytes }
      fldTIME:
        Result := 'time'; { Time (32 bit) }
      fldTIMESTAMP:
        Result := 'timestamp'; { Time-stamp  (64 bit) }
      fldUINT16:
        Result := 'unsigned int'; { Unsigned 16 bit Integer }
      fldUINT32:
        Result := 'unsigned long int'; { Unsigned 32 bit Integer }

      fldFLOATIEEE:
        Result := 'float IEEE'; { 80-bit IEEE float }
      fldVARBYTES:
        Result := 'varbytes'; { Length prefixed var bytes }
      fldLOCKINFO:
        Result := 'lockinfo'; { Look for LOCKINFO typedef }
      fldCURSOR:
        Result := 'Oracle cursor'; { For Oracle Cursor type }

     { Paradox types (Physical) }
      fldPDXCHAR:
        Result := 'alpha(' + SUnits1 + ')'; { Alpha    (string) }
      fldPDXNUM:
        Result := 'numeric(' + SUnits1 + ', ' + SUnits2 + ')'; { Numeric }

      fldPDXMONEY:
        Result := 'money'; { Money }
      fldPDXDATE:
        Result := 'date'; { Date }
      fldPDXSHORT:
        Result := 'smallint'; { Short }
      fldPDXMEMO:
        Result := 'memo blob'; { Text Memo (blob) }
      fldPDXBINARYBLOB:
        Result := 'binary blob'; { Binary data (blob) }
      fldPDXFMTMEMO:
        Result := 'formatted blob'; { Formatted text  (blob) }
      fldPDXOLEBLOB:
        Result := 'OLE blob'; { OLE object (blob) }

      fldPDXGRAPHIC:
        Result := 'graphic blob'; { Graphics object (blob) }
      fldPDXLONG:
        Result := 'long integer'; { Long }
      fldPDXTIME:
        Result := 'time'; { Time }
      fldPDXDATETIME:
        Result := 'date time'; { Time Stamp }
      fldPDXBOOL:
        Result := 'boolean'; { Logical }
      fldPDXAUTOINC:
        Result := 'auto increment'; { Auto increment (long) }
      fldPDXBYTES:
        Result := 'bytes'; { Fixed number of bytes }

      fldPDXBCD:
        Result := 'BCD'; { BCD (32 digits) }

      { xBASE types (Physical) }
      fldDBCHAR:
        Result := 'character'; { Char string }
      fldDBNUM:
        Result := 'number'; { Number }
      fldDBMEMO:
        Result := 'memo blob'; { Memo (blob) }
      fldDBBOOL:
        Result := 'logical'; { Logical }
      fldDBDATE:
        Result := 'date'; { Date }
      fldDBFLOAT:
        Result := 'float'; { Float }

      fldDBLOCK:
        Result := 'LOCKINFO'; { Logical type is LOCKINFO }
      fldDBOLEBLOB:
        Result := 'OLE blob'; { OLE object    (blob) }
      fldDBBINARY:
        Result := 'binary blob'; { Binary data   (blob) }
      fldDBBYTES:
        Result := 'bytes'; { Only for TEMPORARY tables }
      fldDBLONG:
        Result := 'long integer'; { Long (Integer) }
      fldDBDATETIME:
        Result := 'date time'; { Time Stamp }
      fldDBDOUBLE:
        Result := 'double'; { Double }

      fldDBAUTOINC:
        Result := 'auto increment'; { Auto increment (long) }

     { InterBase types (Physical) }
      1026:
        Result := 'integer';
      1028:
        Result := 'numeric(' + SUnits1 + ', ' + SUnits2 + ')'; { Numeric }
      1029:
        Result := 'char(' + SUnits1 + ')';
      1031:
        Result := 'date'; { Date }
    else
      Result := 'unknown type';
    end;
end;

{************************ Variant conversion routines ************************}

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if V = Null then
  begin
    case VarType of
      varString, varOleStr:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
    else
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
end;

procedure CopyRecord(DataSet: TDataSet);
var
  I: Integer;
begin
  with DataSet, TStringList.Create do
  try
    for I := 0 to FieldCount - 1 do
      Add(Fields[I].AsString);
    DataSet.Append;
    for I := 0 to FieldCount - 1 do
      if Fields[I].IsNull then
        Fields[I].AsString := Strings[I];
  finally
    Free;
  end
end;

procedure AddReference(Tbl: TTable; RefName: string; RefField: Word;
  MasterTable: string; MasterField: Word; ModOp, DelOp: RINTQual);
var
  hDb: hDbiDb;
  TblDesc: CRTblDesc;
  RInt: pRINTDesc;
  Dir: string;
  OpType: CROpType;
begin
  SetLength(Dir, dbiMaxNameLen + 1);
  Check(DbiGetDirectory(Tbl.DBHandle, False, PChar(Dir)));
  SetLength(Dir, StrLen(PChar(Dir)));
  RInt := AllocMem(SizeOf(RINTDesc));
  try
    FillChar(TblDesc, SizeOf(CRTblDesc), #0);
    Tbl.DisableControls;
    Tbl.Close;
    Check(DbiOpenDatabase(nil, nil, dbiReadWrite, dbiOpenExcl, nil, 0, nil, nil, hDb));
    Check(DbiSetDirectory(hDb, PChar(Dir)));
    with RInt^ do
    begin
      StrPCopy(szRintName, RefName);
      StrPCopy(szTblName, MasterTable);
      eType := rintDEPENDENT;
      eModOp := ModOp;
      eDelOp := DelOp;
      iFldCount := 1;
      aiThisTabFld[0] := RefField;
      aiOthTabFld[0] := MasterField;
    end;
    TblDesc.iRintCount := 1;
    TblDesc.pRINTDesc := RInt;
    OpType := crADD;
    TblDesc.pecrRintOp := @OpType;
    StrPCopy(TblDesc.szTblName, Tbl.TableName);
    StrCopy(TblDesc.szTblType, szParadox);
    Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
  finally
    Check(DbiCloseDatabase(hDb));
    FreeMem(RInt, SizeOf(RINTDesc));
    Tbl.EnableControls;
    Tbl.Open;
  end;
end;

{
procedure PackTable(Table: TTable);
var
  Props: CURProps;
  hDb: hDBIDb;
  TableDesc: CRTblDesc;
begin
  // Make sure the table is open exclusively so we can get the db handle...
  if not Table.Active then
    raise EDatabaseError.CreateRes(@STableNotOpen);
  if not Table.Exclusive then
    raise EDatabaseError.CreateRes(@STableNotOpenExclusively);

  // Get the table properties to determine table type...
  Check(DbiGetCursorProps(Table.Handle, Props));

  // If the table is a Paradox table, you must call DbiDoRestructure...
  if Props.szTableType = szPARADOX then
  begin
    // Blank out the structure...
    FillChar(TableDesc, SizeOf(TableDesc), 0);
    // Get the database handle from the table's cursor handle...

    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to True...
    TableDesc.bPack := True;
    // Close the table so the restructure can complete...
    Table.Close;
    // Call DbiDoRestructure...
    Check(DbiDoRestructure(hDb, 1, @TableDesc, nil, nil, nil, False));
  end
  else
  // If the table is a dBASE table, simply call DbiPackTable...
  if (Props.szTableType = szDBASE) then
    Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, szDBASE, True))
  else
    // Pack only works on Paradox or dBASE; nothing else...
    raise EDatabaseError.CreateRes(@SNoParadoxDBaseTable);
  Table.Open;
end;
}
//Add a master password to a Paradox table.
//This procedure uses the following input:
//AddMasterPassword(Table1, 'MyNewPassword')

procedure AddMasterPassword(Table: TTable; pswd: string);
const
  RESTRUCTURE_TRUE = WordBool(1);
var
  TblDesc: CRTblDesc;
  hDb: hDBIDb;
begin
  { Make sure that the table is opened and is exclusive }
  if not Table.Active or not Table.Exclusive then
    raise EDatabaseError.CreateRes(@RsETableNotInExclusiveMode);
  { Initialize the table descriptor }
  FillChar(TblDesc, SizeOf(CRTblDesc), #0);
  with TblDesc do
  begin
    { Place the table name in descriptor }
    StrPCopy(szTblName, Table.TableName);
    { Place the table type in descriptor }
    StrCopy(szTblType, szPARADOX);
    { Master Password, Password }
    StrPCopy(szPassword, pswd);
    { Set bProtected to True }
    bProtected := RESTRUCTURE_TRUE;
  end;
  { Get the database handle from the cursor handle }
  Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
  { Close the table }
  Table.Close;

  { Add the master password to the Paradox table }
  Check(DbiDoRestructure(hDb, 1, @TblDesc, nil, nil, nil, False));
  { Add the new password to the session }
  Session.AddPassword(pswd);
  { Re-Open the table }
  Table.Open;
end;

// Pack a Paradox table with Password
// The table must be opened execlusively before calling this function...

procedure PackEncryptedTable(Table: TTable; pswd: string);
const
  RESTRUCTURE_TRUE = WordBool(1);
var
  Props: CURProps;
  hDb: hDBIDb;
  TableDesc: CRTblDesc;
begin
  // Make sure the table is open exclusively so we can get the db handle...
  if not Table.Active then
    raise EDatabaseError.CreateRes(@RsETableNotOpen);
  if not Table.Exclusive then
    raise EDatabaseError.CreateRes(@RsETableNotOpenExclusively);

  // Get the table properties to determine table type...
  Check(DbiGetCursorProps(Table.Handle, Props));

  // If the table is a Paradox table, you must call DbiDoRestructure...
  if Props.szTableType = szPARADOX then
  begin
    // Blank out the structure...
    FillChar(TableDesc, SizeOf(TableDesc), 0);
    // Get the database handle from the table's cursor handle...
    Check(DbiGetObjFromObj(hDBIObj(Table.Handle), objDATABASE, hDBIObj(hDb)));
    // Put the table name in the table descriptor...
    StrPCopy(TableDesc.szTblName, Table.TableName);
    // Put the table type in the table descriptor...
    StrPCopy(TableDesc.szTblType, Props.szTableType);
    // Set the Pack option in the table descriptor to True...
    TableDesc.bPack := True;
    { Master Password, Password }
    StrPCopy(TableDesc.szPassword, pswd);
    { Set bProtected to True }
    TableDesc.bProtected := RESTRUCTURE_TRUE;
    // Close the table so the restructure can complete...
    Table.Close;
    // Call DbiDoRestructure...
    Check(DbiDoRestructure(hDb, 1, @TableDesc, nil, nil, nil, False));
  end
  else
  // If the table is a dBASE table, simply call DbiPackTable...
  if Props.szTableType = szDBASE then
    Check(DbiPackTable(Table.DBHandle, Table.Handle, nil, szDBASE, True))
  else
    // Pack only works on Paradox or dBASE; nothing else...
    raise EDatabaseError.CreateRes(@RsENoParadoxDBaseTable);
  Table.Open;
end;

function EncodeQuotes(const S: string): string;
begin
  Result := S;
  Result := ReplaceString(Result, CrLf, Cr);
  Result := ReplaceString(Result, Cr, '\#13');
  Result := ReplaceString(Result, '"', '\#34');
  Result := ReplaceString(Result, ',', '\#44');
end;

{*********************** from JvStrUtil unit ***********************}

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
// {Вырезает подстроку. Подстроки разделяются символом Sep}
{ SubStr returns substring from string, S, separated with Separator string [translated]}
var
  I: Integer;
  pB, pE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  pB := PChar(S);
  for I := 1 to Index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then
      Exit;
    pB := pB + Length(Separator);
    if pB[0] = #0 then
      Exit;
  end;
  pE := StrPos(pB + 1, PChar(Separator));
  if pE = nil then
    pE := PChar(S) + Length(S);
  if AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) <> 0 then
    SetString(Result, pB, pE - pB);
end;

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
var
  MaxIndex: Integer;
  pB: PChar;
begin
// Not optimal implementation [translated]
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do
  begin
    Inc(MaxIndex);
    pB := StrPos(pB + Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - Index, Separator);
end;

function Cmp(const S1, S2: string): Boolean;
begin
  Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0;
end;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }

function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
var
  LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do
  begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + NewPattern + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

{ GetXYByPos is same to previous function, but
  returns X position in line too}

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
var
  I, iB: Integer;
begin
  X := -1;
  Y := -1;
  iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = Cr then
      begin
        Inc(Y);
        iB := I + 1
      end;
      Inc(I);
    end;
    X := Pos - iB;
  end;
end;
{####################### from JvStrUtil unit #######################}

initialization
  JvDBUtils.CreateLocateObject := CreateDbLocate;
  
finalization
  ReleaseSaveIndices;
  // (rom) i tried deleting the elements created by CreateDbLocate
  // (rom) but that causes crashes

end.

