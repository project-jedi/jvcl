{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLists.PAS, released on 2002-07-04.

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

unit JvDBLists;

interface

uses
  SysUtils, Classes, DB, DBTables,
  {$IFDEF WIN32}
  Bde,
  {$ELSE}
  WinTypes, WinProcs, DbiTypes, DbiProcs, DbiErrs,
  {$ENDIF}
  JvDBUtils;

type
  TBDEItemType = (bdDatabases, bdDrivers, bdLangDrivers, bdUsers
    {$IFDEF WIN32}, bdRepositories {$ENDIF});

  TJvCustomBDEItems = class(TBDEDataSet)
  private
    FItemType: TBDEItemType;
    {$IFDEF WIN32}
    FSessionName: string;
    FSessionLink: TDatabase;
    function GetDBSession: TSession;
    procedure SetSessionName(const Value: string);
    {$ENDIF}
    procedure SetItemType(Value: TBDEItemType);
  protected
    {$IFDEF WIN32}
    function GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE} Integer; override {$ENDIF};
    procedure OpenCursor {$IFDEF COMPILER3_UP} (InfoQuery: Boolean) {$ENDIF}; override;
    procedure CloseCursor; override;
    {$ENDIF}
    function CreateHandle: HDBICur; override;
    property ItemType: TBDEItemType read FItemType write SetItemType
      default bdDatabases;
  public
    {$IFDEF WIN32}
    {$IFDEF COMPILER3_UP}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    {$ENDIF}
    property DBSession: TSession read GetDBSession;
    {$IFNDEF COMPILER3_UP}
    property RecordCount: Longint read GetRecordCount;
    {$ENDIF}
  published
    property SessionName: string read FSessionName write SetSessionName;
    {$ENDIF WIN32}
  end;

  TJvBDEItems = class(TJvCustomBDEItems)
  published
    property ItemType;
  end;

  TJvDBListDataSet = class(TDBDataSet)
  {$IFDEF WIN32}
  protected
    function GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE} Integer; override {$ENDIF};
  public
    {$IFDEF COMPILER3_UP}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    {$ELSE}
    property RecordCount: Longint read GetRecordCount;
    {$ENDIF}
  {$ENDIF}
  end;

  TDBItemType = (dtTables, dtStoredProcs, dtFiles {$IFDEF WIN32},
    dtFunctions {$ENDIF});

  TJvCustomDatabaseItems = class(TJvDBListDataSet)
  private
    FExtended: Boolean;
    FSystemItems: Boolean;
    FFileMask: string;
    FItemType: TDBItemType;
    procedure SetFileMask(const Value: string);
    procedure SetExtendedInfo(Value: Boolean);
    procedure SetSystemItems(Value: Boolean);
    procedure SetItemType(Value: TDBItemType);
  protected
    function CreateHandle: HDBICur; override;
    function GetItemName: string;
    property ItemType: TDBItemType read FItemType write SetItemType
      default dtTables;
    property ExtendedInfo: Boolean read FExtended write SetExtendedInfo
      default False;
    property FileMask: string read FFileMask write SetFileMask;
    property SystemItems: Boolean read FSystemItems write SetSystemItems
      default False;
  public
    property ItemName: string read GetItemName;
  end;

  TJvDatabaseItems = class(TJvCustomDatabaseItems)
  published
    property ItemType;
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

  TTabItemType = (dtFields, dtIndices, dtValChecks, dtRefInt,
    dtSecurity, dtFamily);

  TJvCustomTableItems = class(TJvDBListDataSet)
  private
    FTableName: TFileName;
    FItemType: TTabItemType;
    FPhysTypes: Boolean;
    procedure SetTableName(const Value: TFileName);
    procedure SetItemType(Value: TTabItemType);
    procedure SetPhysTypes(Value: Boolean);
  protected
    function CreateHandle: HDBICur; override;
    property ItemType: TTabItemType read FItemType write SetItemType
      default dtFields;
    property PhysTypes: Boolean read FPhysTypes write SetPhysTypes
      default False; { for dtFields only }
  published
    property TableName: TFileName read FTableName write SetTableName;
  end;

  TJvTableItems = class(TJvCustomTableItems)
  published
    property ItemType;
    property PhysTypes;
  end;

  TJvDatabaseDesc = class(TObject)
  private
    FDescription: DBDesc;
  public
    constructor Create(const DatabaseName: string);
    property Description: DBDesc read FDescription;
  end;

  TJvDriverDesc = class(TObject)
  private
    FDescription: DRVType;
  public
    constructor Create(const DriverType: string);
    property Description: DRVType read FDescription;
  end;

{$IFNDEF CBUILDER}
{ Obsolete classes, for backward compatibility only }

type
  TJvDatabaseList = class(TJvCustomBDEItems);

  TJvLangDrivList = class(TJvCustomBDEItems)
    constructor Create(AOwner: TComponent); override;
  end;

  TJvTableList = class(TJvCustomDatabaseItems)
  public
    function GetTableName: string;
  published
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

  TJvStoredProcList = class(TJvCustomDatabaseItems)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ExtendedInfo;
    property SystemItems;
  end;

  TJvFieldList = class(TJvCustomTableItems);

  TJvIndexList = class(TJvCustomTableItems)
    constructor Create(AOwner: TComponent); override;
  end;

{$ENDIF CBUILDER}

implementation

uses
  DBConsts,
  {$IFDEF COMPILER3_UP}
  BDEConst,
  {$ENDIF}
  JvxRConst;

{ Utility routines }

function dsGetRecordCount(DataSet: TBDEDataSet): Longint;
begin
  if DataSet.State = dsInactive then
    _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(DataSet.Handle, Result));
end;

//=== TJvSessionLink =========================================================

{$IFDEF WIN32}

type
  TJvSessionLink = class(TDatabase)
  private
    FList: TJvCustomBDEItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TJvSessionLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (AOwner <> nil) and (AOwner is TSession) then
    SessionName := TSession(AOwner).SessionName;
  Temporary := True;
  KeepConnection := False;
end;

destructor TJvSessionLink.Destroy;
begin
  if FList <> nil then
  begin
    FList.FSessionLink := nil;
    FList.Close;
  end;
  inherited Destroy;
end;

{$ENDIF}

//=== TJvCustomBDEItems ======================================================

procedure TJvCustomBDEItems.SetItemType(Value: TBDEItemType);
begin
  if ItemType <> Value then
  begin
    CheckInactive;
    FItemType := Value;
  end;
end;

function TJvCustomBDEItems.CreateHandle: HDBICur;
begin
  case FItemType of
    bdDatabases:
      Check(DbiOpenDatabaseList(Result));
    bdDrivers:
      Check(DbiOpenDriverList(Result));
    bdLangDrivers:
      Check(DbiOpenLdList(Result));
    bdUsers:
      Check(DbiOpenUserList(Result));
    {$IFDEF WIN32}
    bdRepositories:
      Check(DbiOpenRepositoryList(Result));
    {$ENDIF}
  end;
end;

{$IFDEF WIN32}

function TJvCustomBDEItems.GetDBSession: TSession;
begin
  Result := Sessions.FindSession(SessionName);
  if Result = nil then
    {$IFDEF COMPILER3_UP}
    Result := DBTables.Session;
    {$ELSE}
    Result := DB.Session;
    {$ENDIF}
end;

procedure TJvCustomBDEItems.SetSessionName(const Value: string);
begin
  CheckInactive;
  FSessionName := Value;
  DataEvent(dePropertyChange, 0);
end;

procedure TJvCustomBDEItems.OpenCursor;
var
  S: TSession;
begin
  S := Sessions.List[SessionName];
  S.Open;
  Sessions.CurrentSession := S;
  FSessionLink := TJvSessionLink.Create(S);
  try
    TJvSessionLink(FSessionLink).FList := Self;
    inherited OpenCursor {$IFDEF COMPILER3_UP} (InfoQuery) {$ENDIF};
  except
    FSessionLink.Free;
    FSessionLink := nil;
    raise;
  end;
end;

procedure TJvCustomBDEItems.CloseCursor;
begin
  inherited CloseCursor;
  if FSessionLink <> nil then
  begin
    TJvSessionLink(FSessionLink).FList := nil;
    FSessionLink.Free;
    FSessionLink := nil;
  end;
end;

function TJvCustomBDEItems.GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;

{$ENDIF WIN32}

{$IFDEF COMPILER3_UP}
function TJvCustomBDEItems.Locate(const KeyFields: string;
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
{$ENDIF COMPILER3_UP}

//=== TJvDBListDataSet =======================================================

{$IFDEF COMPILER3_UP}
function TJvDBListDataSet.Locate(const KeyFields: string;
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
{$ENDIF COMPILER3_UP}

{$IFDEF WIN32}
function TJvDBListDataSet.GetRecordCount: {$IFNDEF COMPILER3_UP} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;
{$ENDIF WIN32}

//=== TJvCustomDatabaseItems =================================================

procedure TJvCustomDatabaseItems.SetItemType(Value: TDBItemType);
begin
  if ItemType <> Value then
  begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvCustomDatabaseItems.SetFileMask(const Value: string);
begin
  if FileMask <> Value then
  begin
    if Active and (FItemType in [dtTables, dtFiles]) then
    begin
      DisableControls;
      try
        Close;
        FFileMask := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else
      FFileMask := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvCustomDatabaseItems.SetExtendedInfo(Value: Boolean);
begin
  if FExtended <> Value then
  begin
    CheckInactive;
    FExtended := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvCustomDatabaseItems.SetSystemItems(Value: Boolean);
begin
  if FSystemItems <> Value then
  begin
    if Active and (FItemType in [dtTables, dtStoredProcs]) then
    begin
      DisableControls;
      try
        Close;
        FSystemItems := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else
      FSystemItems := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TJvCustomDatabaseItems.CreateHandle: HDBICur;
var
  WildCard: PChar;
  Pattern: array [0..DBIMAXTBLNAMELEN] of Char;
begin
  WildCard := nil;
  if FileMask <> '' then
    WildCard := AnsiToNative(DBLocale, FileMask, Pattern, SizeOf(Pattern) - 1);
  case FItemType of
    dtTables:
      Check(DbiOpenTableList(DBHandle, FExtended, FSystemItems, WildCard, Result));
    dtStoredProcs:
      if DataBase.IsSQLBased then
        Check(DbiOpenSPList(DBHandle, FExtended, FSystemItems, nil, Result))
      else
        DatabaseError(SLocalDatabase);
    dtFiles:
      Check(DbiOpenFileList(DBHandle, WildCard, Result));
    {$IFDEF WIN32}
    dtFunctions:
      if DataBase.IsSQLBased then
        Check(DbiOpenFunctionList(DBHandle, DBIFUNCOpts(FExtended), @Result))
      else
        DatabaseError(SLocalDatabase);
    {$ENDIF}
  end;
end;

function TJvCustomDatabaseItems.GetItemName: string;
const
  sObjListNameField = 'NAME';
  sFileNameField = 'FILENAME';
  sTabListExtField = 'EXTENSION';
var
  Temp: string;
  Field: TField;
begin
  Result := '';
  if not Active then
    Exit;
  if FItemType = dtFiles then
    Field := FindField(sFileNameField)
  else
    Field := FindField(sObjListNameField);
  if Field = nil then
    Exit;
  Result := Field.AsString;
  if FItemType in [dtTables, dtFiles] then
  begin
    Field := FindField(sTabListExtField);
    if Field = nil then
      Exit;
    Temp := Field.AsString;
    if Temp <> '' then
    begin
      if Temp[1] <> '.' then
        Temp := '.' + Temp;
      Result := Result + Temp;
    end;
  end;
end;

procedure TJvCustomTableItems.SetItemType(Value: TTabItemType);
begin
  if ItemType <> Value then
  begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvCustomTableItems.SetPhysTypes(Value: Boolean);
begin
  if Value <> PhysTypes then
  begin
    if Active and (ItemType = dtFields) then
    begin
      DisableControls;
      try
        Close;
        FPhysTypes := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else
      FPhysTypes := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvCustomTableItems.SetTableName(const Value: TFileName);
begin
  if Value <> FTableName then
  begin
    if Active then
    begin
      DisableControls;
      try
        Close;
        FTableName := Value;
        if FTableName <> '' then
          Open;
      finally
        EnableControls;
      end;
    end
    else
      FTableName := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TJvCustomTableItems.CreateHandle: HDBICur;
var
  STableName: PChar;
begin
  if FTableName = '' then
    _DBError(SNoTableName);
  STableName := StrAlloc(Length(FTableName) + 1);
  try
    AnsiToNative(DBLocale, FTableName, STableName, Length(FTableName));
    case FItemType of
      dtFields:
        while not CheckOpen(DbiOpenFieldList(DBHandle, STableName, nil,
          FPhysTypes, Result)) do {Retry}
          ;
      dtIndices:
        while not CheckOpen(DbiOpenIndexList(DBHandle, STableName, nil,
          Result)) do {Retry}
          ;
      dtValChecks:
        while not CheckOpen(DbiOpenVchkList(DBHandle, STableName, nil,
          Result)) do {Retry}
          ;
      dtRefInt:
        while not CheckOpen(DbiOpenRintList(DBHandle, STableName, nil,
          Result)) do {Retry}
          ;
      dtSecurity:
        while not CheckOpen(DbiOpenSecurityList(DBHandle, STableName, nil,
          Result)) do {Retry}
          ;
      dtFamily:
        while not CheckOpen(DbiOpenFamilyList(DBHandle, STableName, nil,
          Result)) do {Retry}
          ;
    end;
  finally
    StrDispose(STableName);
  end;
end;

//=== TJvDatabaseDesc ========================================================

constructor TJvDatabaseDesc.Create(const DatabaseName: string);
var
  Buffer: PChar;
begin
  inherited Create;
  Buffer := StrPCopy(StrAlloc(Length(DatabaseName) + 1), DatabaseName);
  try
    Check(DbiGetDatabaseDesc(Buffer, @FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

constructor TJvDriverDesc.Create(const DriverType: string);
var
  Buffer: PChar;
begin
  inherited Create;
  Buffer := StrPCopy(StrAlloc(Length(DriverType) + 1), DriverType);
  try
    Check(DbiGetDriverDesc(Buffer, FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

{$IFNDEF CBUILDER}

//=== TJvLangDrivList ========================================================

constructor TJvLangDrivList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := bdLangDrivers;
end;

//=== TJvTableList ===========================================================

function TJvTableList.GetTableName: string;
begin
  Result := ItemName;
end;

constructor TJvStoredProcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtStoredProcs;
end;

//=== TJvIndexList ===========================================================

constructor TJvIndexList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtIndices;
end;

{$ENDIF CBUILDER}

end.

