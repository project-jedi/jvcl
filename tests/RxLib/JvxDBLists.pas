{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxDBLists.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxDBLists;


interface

uses SysUtils, Classes, DB, DBTables, JvxDBUtils, JvxBdeUtils,
{$IFDEF WIN32} 
  Windows, Bde;
{$ELSE}
  WinTypes, WinProcs, DbiTypes, DbiProcs, DbiErrs;
{$ENDIF}

type

{ TJvxBDEItems }

  TBDEItemType = (bdDatabases, bdDrivers, bdLangDrivers, bdUsers 
    {$IFDEF WIN32}, bdRepositories {$ENDIF});

  TJvxCustomBDEItems = class(TBDEDataSet)
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
    function GetRecordCount: {$IFNDEF Delphi3_Up} Longint {$ELSE}
      Integer; override {$ENDIF};
    procedure OpenCursor {$IFDEF Delphi3_Up}(InfoQuery: Boolean){$ENDIF}; override;
    procedure CloseCursor; override;
{$ENDIF}
    function CreateHandle: HDBICur; override;
    property ItemType: TBDEItemType read FItemType write SetItemType
      default bdDatabases;
  public
{$IFDEF WIN32}
  {$IFDEF Delphi3_Up}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  {$ENDIF}
    property DBSession: TSession read GetDBSession;
  {$IFNDEF Delphi3_Up}
    property RecordCount: Longint read GetRecordCount;
  {$ENDIF}
  published
    property SessionName: string read FSessionName write SetSessionName;
{$ENDIF WIN32}
  end;

  TJvxBDEItems = class(TJvxCustomBDEItems)
  published
    property ItemType;
  end;

{ TJvxDBListDataSet }

  TJvxDBListDataSet = class(TDBDataSet)
{$IFDEF WIN32}
  protected
    function GetRecordCount: {$IFNDEF Delphi3_Up} Longint {$ELSE}
      Integer; override {$ENDIF};
  public
  {$IFDEF Delphi3_Up}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
  {$ELSE}
    property RecordCount: Longint read GetRecordCount;
  {$ENDIF}
{$ENDIF}
  end;

{ TJvxDatabaseItems }

  TDBItemType = (dtTables, dtStoredProcs, dtFiles {$IFDEF WIN32},
    dtFunctions {$ENDIF});

  TJvxCustomDatabaseItems = class(TJvxDBListDataSet)
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

  TJvxDatabaseItems = class(TJvxCustomDatabaseItems)
  published
    property ItemType;
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

{ TJvxTableItems }

  TTabItemType = (dtFields, dtIndices, dtValChecks, dtRefInt,
    dtSecurity, dtFamily);

  TJvxCustomTableItems = class(TJvxDBListDataSet)
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

  TJvxTableItems = class(TJvxCustomTableItems)
  published
    property ItemType;
    property PhysTypes;
  end;

{ TJvxDatabaseDesc }

  TJvxDatabaseDesc = class(TObject)
  private
    FDescription: DBDesc;
  public
    constructor Create(const DatabaseName: string);
    property Description: DBDesc read FDescription;
  end;

{ TJvxDriverDesc }

  TJvxDriverDesc = class(TObject)
  private
    FDescription: DRVType;
  public
    constructor Create(const DriverType: string);
    property Description: DRVType read FDescription;
  end;

{*************************************************************************}

{$IFNDEF CBUILDER}
{ Obsolete classes, for backward compatibility only }

type

  TJvxDatabaseList = class(TJvxCustomBDEItems);

  TJvxLangDrivList = class(TJvxCustomBDEItems)
    constructor Create(AOwner: TComponent); override;
  end;

  TJvxTableList = class(TJvxCustomDatabaseItems)
  public
    function GetTableName: string;
  published
    property ExtendedInfo;
    property FileMask;
    property SystemItems;
  end;

  TJvxStoredProcList = class(TJvxCustomDatabaseItems)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ExtendedInfo;
    property SystemItems;
  end;

  TJvxFieldList = class(TJvxCustomTableItems);

  TJvxIndexList = class(TJvxCustomTableItems)
    constructor Create(AOwner: TComponent); override;
  end;

{$ENDIF CBUILDER}

implementation

uses DBConsts, {$IFDEF Delphi3_Up} BDEConst, {$ENDIF} JvxDConst;

{ Utility routines }

function dsGetRecordCount(DataSet: TBDEDataSet): Longint;
begin
  if DataSet.State = dsInactive then _DBError(SDataSetClosed);
  Check(DbiGetRecordCount(DataSet.Handle, Result));
end;

{$IFDEF WIN32}
type
  TJvxSessionLink = class(TDatabase)
  private
    FList: TJvxCustomBDEItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TJvxSessionLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (AOwner <> nil) and (AOwner is TSession) then
    SessionName := TSession(AOwner).SessionName;
  Temporary := True;
  KeepConnection := False;
end;

destructor TJvxSessionLink.Destroy;
begin
  if FList <> nil then begin
    FList.FSessionLink := nil;
    FList.Close;
  end;
  inherited Destroy;
end;
{$ENDIF}

{ TJvxCustomBDEItems }

procedure TJvxCustomBDEItems.SetItemType(Value: TBDEItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
  end;
end;

function TJvxCustomBDEItems.CreateHandle: HDBICur;
begin
  case FItemType of
    bdDatabases: Check(DbiOpenDatabaseList(Result));
    bdDrivers: Check(DbiOpenDriverList(Result));
    bdLangDrivers: Check(DbiOpenLdList(Result));
    bdUsers: Check(DbiOpenUserList(Result));
{$IFDEF WIN32}
    bdRepositories: Check(DbiOpenRepositoryList(Result));
{$ENDIF}
  end;
end;

{$IFDEF WIN32}
function TJvxCustomBDEItems.GetDBSession: TSession;
begin
  Result := Sessions.FindSession(SessionName);
  if Result = nil then
{$IFDEF Delphi3_Up}
    Result := DBTables.Session;
{$ELSE}
    Result := DB.Session;
{$ENDIF}
end;

procedure TJvxCustomBDEItems.SetSessionName(const Value: string);
begin
  CheckInactive;
  FSessionName := Value;
  DataEvent(dePropertyChange, 0);
end;

procedure TJvxCustomBDEItems.OpenCursor;
var
  S: TSession;
begin
  S := Sessions.List[SessionName];
  S.Open;
  Sessions.CurrentSession := S;
  FSessionLink := TJvxSessionLink.Create(S);
  try
    TJvxSessionLink(FSessionLink).FList := Self;
    inherited OpenCursor{$IFDEF Delphi3_Up}(InfoQuery){$ENDIF};
  except
    FSessionLink.Free;
    FSessionLink := nil;
    raise;
  end;
end;

procedure TJvxCustomBDEItems.CloseCursor;
begin
  inherited CloseCursor;
  if FSessionLink <> nil then begin
    TJvxSessionLink(FSessionLink).FList := nil;
    FSessionLink.Free;
    FSessionLink := nil;
  end;
end;
{$ENDIF WIN32}

{$IFDEF WIN32}
function TJvxCustomBDEItems.GetRecordCount: {$IFNDEF Delphi3_Up} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;
{$ENDIF WIN32}

{$IFDEF Delphi3_Up}
function TJvxCustomBDEItems.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;
{$ENDIF Delphi3_Up}

{ TJvxDBListDataSet }

{$IFDEF Delphi3_Up}
function TJvxDBListDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := DataSetLocateThrough(Self, KeyFields, KeyValues, Options);
  if Result then begin
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
  end;
end;
{$ENDIF Delphi3_Up}

{$IFDEF WIN32}
function TJvxDBListDataSet.GetRecordCount: {$IFNDEF Delphi3_Up} Longint {$ELSE} Integer {$ENDIF};
begin
  Result := dsGetRecordCount(Self);
end;
{$ENDIF WIN32}

{ TJvxCustomDatabaseItems }

procedure TJvxCustomDatabaseItems.SetItemType(Value: TDBItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvxCustomDatabaseItems.SetFileMask(const Value: string);
begin
  if FileMask <> Value then begin
    if Active and (FItemType in [dtTables, dtFiles]) then begin
      DisableControls;
      try
        Close;
        FFileMask := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FFileMask := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvxCustomDatabaseItems.SetExtendedInfo(Value: Boolean);
begin
  if FExtended <> Value then begin
    CheckInactive;
    FExtended := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvxCustomDatabaseItems.SetSystemItems(Value: Boolean);
begin
  if FSystemItems <> Value then begin
    if Active and (FItemType in [dtTables, dtStoredProcs]) then begin
      DisableControls;
      try
        Close;
        FSystemItems := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FSystemItems := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TJvxCustomDatabaseItems.CreateHandle: HDBICur;
var
  WildCard: PChar;
  Pattern: array[0..DBIMAXTBLNAMELEN] of Char;
begin
  WildCard := nil;
  if FileMask <> '' then
    WildCard := AnsiToNative(DBLocale, FileMask, Pattern, SizeOf(Pattern) - 1);
  case FItemType of
    dtTables: Check(DbiOpenTableList(DBHandle, FExtended, FSystemItems, WildCard, Result));
    dtStoredProcs:
      if DataBase.IsSQLBased then
        Check(DbiOpenSPList(DBHandle, FExtended, FSystemItems, nil, Result))
      else DatabaseError(LoadStr(SLocalDatabase));
    dtFiles: Check(DbiOpenFileList(DBHandle, WildCard, Result));
{$IFDEF WIN32}
    dtFunctions:
      if DataBase.IsSQLBased then
        Check(DbiOpenFunctionList(DBHandle, DBIFUNCOpts(FExtended), @Result))
      else DatabaseError(LoadStr(SLocalDatabase));
{$ENDIF}
  end;
end;

function TJvxCustomDatabaseItems.GetItemName: string;
const
  sObjListNameField = 'NAME';
  sFileNameField = 'FILENAME';
  sTabListExtField  = 'EXTENSION';
var
  Temp: string;
  Field: TField;
begin
  Result := '';
  if not Active then Exit;
  if FItemType = dtFiles then Field := FindField(sFileNameField)
  else Field := FindField(sObjListNameField);
  if Field = nil then Exit;
  Result := Field.AsString;
  if FItemType in [dtTables, dtFiles] then begin
    Field := FindField(sTabListExtField);
    if Field = nil then Exit;
    Temp := Field.AsString;
    if Temp <> '' then begin
      if Temp[1] <> '.' then Temp := '.' + Temp;
      Result := Result + Temp;
    end;
  end;
end;

{ TJvxCustomTableItems }

procedure TJvxCustomTableItems.SetItemType(Value: TTabItemType);
begin
  if ItemType <> Value then begin
    CheckInactive;
    FItemType := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvxCustomTableItems.SetPhysTypes(Value: Boolean);
begin
  if Value <> PhysTypes then begin
    if Active and (ItemType = dtFields) then begin
      DisableControls;
      try
        Close;
        FPhysTypes := Value;
        Open;
      finally
        EnableControls;
      end;
    end
    else FPhysTypes := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TJvxCustomTableItems.SetTableName(const Value: TFileName);
begin
  if Value <> FTableName then begin
    if Active then begin
      DisableControls;
      try
        Close;
        FTableName := Value;
        if FTableName <> '' then Open;
      finally
        EnableControls;
      end;
    end
    else FTableName := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

function TJvxCustomTableItems.CreateHandle: HDBICur;
var
  STableName: PChar;
begin
  if FTableName = '' then _DBError(SNoTableName);
  STableName := StrAlloc(Length(FTableName) + 1);
  try
    AnsiToNative(DBLocale, FTableName, STableName, Length(FTableName));
    case FItemType of
      dtFields:
        while not CheckOpen(DbiOpenFieldList(DBHandle, STableName, nil,
          FPhysTypes, Result)) do {Retry};
      dtIndices:
        while not CheckOpen(DbiOpenIndexList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtValChecks:
        while not CheckOpen(DbiOpenVchkList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtRefInt:
        while not CheckOpen(DbiOpenRintList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtSecurity:
        while not CheckOpen(DbiOpenSecurityList(DBHandle, STableName, nil,
          Result)) do {Retry};
      dtFamily:
        while not CheckOpen(DbiOpenFamilyList(DBHandle, STableName, nil,
          Result)) do {Retry};
    end;
  finally
    StrDispose(STableName);
  end;
end;

{ TJvxDatabaseDesc }

constructor TJvxDatabaseDesc.Create(const DatabaseName: string);
var
  Buffer: PChar;
begin
  Buffer := StrPCopy(StrAlloc(Length(DatabaseName) + 1), DatabaseName);
  try
    Check(DbiGetDatabaseDesc(Buffer, @FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

{ TJvxDriverDesc }

constructor TJvxDriverDesc.Create(const DriverType: string);
var
  Buffer: PChar;
begin
  Buffer := StrPCopy(StrAlloc(Length(DriverType) + 1), DriverType);
  try
    Check(DbiGetDriverDesc(Buffer, FDescription));
  finally
    StrDispose(Buffer);
  end;
end;

{*************************************************************************}

{$IFNDEF CBUILDER}

{ TJvxLangDrivList }

constructor TJvxLangDrivList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := bdLangDrivers;
end;

{ TJvxTableList }

function TJvxTableList.GetTableName: string;
begin
  Result := ItemName;
end;

{ TJvxStoredProcList }

constructor TJvxStoredProcList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtStoredProcs;
end;

{ TJvxIndexList }

constructor TJvxIndexList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemType := dtIndices;
end;

{$ENDIF CBUILDER}

end.
