{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{*******************************************************}
{                                                       }
{         Borland Delphi Unit                           }
{         TUTILITY.DLL Class Unit                       }
{                                                       }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

Unit TUtil;

{$I jvcl.inc}

interface

{$IFDEF WIN32}
uses Windows, DB, BDE, SysUtils, DBTables;
{$ELSE}
uses WinTypes, WinProcs, DB, DbiTypes, DbiProcs, DbiErrs, SysUtils, DbTables;
{$ENDIF}

type
  HTUses = Word;
  PHTUses = ^HTUses;

{ Verify Callback processes }

  TUVerifyProcess = (TUVerifyHeader, TUVerifyIndex, TUVerifyData,
    TUVerifySXHeader, TUVerifySXIndex, TUVerifySXData, TUVerifySXIntegrity,
    TUVerifyTableName);

{ Call back info for Verify Callback function }

  PUVerifyCallBack = ^TUVerifyCallBack;
  TUVerifyCallBack = packed record
    PercentDone: SmallInt;
    TableName: DBIPath;
    Process: TUVerifyProcess;
    CurrentIndex: Word;
    TotalIndex: Word;
  end;

{ TUtility error }

  ETUtilityError = class(EDBEngineError)
  public
    constructor Create(ErrorCode: DBIResult);
  end;

{ Check and repair modes }

  TCheckRepair = (crNoRepair, crAutoRepair, crConfirmRepair);
  TVerifyOption = (vfAppendErrors, vfBypassSecondaryIndexes,
    vfIgnoreWarnings, vfVerifyHeaderOnly, vfNoLockTable, vfDialogHide);
  TVerifyOptions = set of TVerifyOption;

  TTUAction = procedure of object;

{ TTUtility }

  TTUtility = class(TObject)
  private
    FSession: HTUses;
    FCheckErrorTable, FErrorTable, FProblemTable,
    FKeyViolationTable, FBackupTable,
    FTableName: DBIPATH;
    FPassword: DBINAME;
    FTblDesc: CRTblDesc;
    FOptDataLen: Word;
    FCheckRepair: TCheckRepair;
    FVerifyOptions: TVerifyOptions;
    FShowNoError: Boolean;
    procedure SetTabName(const TabName: string; const Dest: DBIPATH);
    function CheckOpen(Status: DBIResult): Boolean;
    procedure Check(Status: DBIResult);
    function ProgressCallback(CBInfo: Pointer): CBRType;
    function VerifyFlag: Integer;
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetCheckErrorTable: string;
    procedure SetCheckErrorTable(const Value: string);
    function GetErrorTable: string;
    procedure SetErrorTable(const Value: string);
    function GetProblemTable: string;
    procedure SetProblemTable(const Value: string);
    function GetKeyViolationTable: string;
    procedure SetKeyViolationTable(const Value: string);
    function GetBackupTable: string;
    procedure SetBackupTable(const Value: string);
    function GetTableName: string;
    procedure SetTableName(const Value: string);
    function TULastErrorMessage: string;
    procedure CheckBackupTable;
    function ShowPasswordDialog: Boolean;
  protected
    procedure RunTUtility(Action: TTUAction);
    procedure FillTblDesc;
    procedure ClearTblDesc;
    procedure DoCheckTable; virtual;
    procedure DoRepairTable; virtual;
    function VerifyTable: Cardinal;
    property BackupTable: string read GetBackupTable write SetBackupTable;
  public
    constructor Create;
    destructor Destroy; override;
    function ErrorString(ErrorCode: DBIResult): string;
    procedure DefaultBackupNames;
    procedure CheckTable;
    procedure RepairTable;
    procedure DropErrorTable;
    property CheckRepair: TCheckRepair read FCheckRepair write FCheckRepair default crConfirmRepair;
    property CheckErrorTable: string read GetCheckErrorTable write SetCheckErrorTable;
    property ErrorTable: string read GetErrorTable write SetErrorTable;
    property KeyViolationTable: string read GetKeyViolationTable write SetKeyViolationTable;
    property ProblemTable: string read GetProblemTable write SetProblemTable;
    property Password: string read GetPassword write SetPassword;
    property ShowNoError: Boolean read FShowNoError write FShowNoError;
    property TableName: string read GetTableName write SetTableName;
    property VerifyOptions: TVerifyOptions read FVerifyOptions write FVerifyOptions
      default [vfIgnoreWarnings];
  end;

{ Utility routines }

procedure CheckTables(const TablesDir: string; Repair: TCheckRepair);
procedure CheckTable(const TableName: string; Repair: TCheckRepair);

implementation

uses Classes, Controls, Dialogs, Forms, JvDBUtils, JvBdeUtils,  JvBDEProgress, JvJCLUtils, JvJVCLUtils;

const
{ Verify table options }
  TU_APPEND_ERRORS            =  1; { append errors to an existing errors table }
  TU_BYPASS_SECONDARY_INDEXES =  2; { bypass secondary indexes }
  TU_IGNORE_WARNINGS          =  4; { prevents reporting of warning errors }
  TU_VERIFY_HEADER_ONLY       =  8; { verify table header only }
  TU_DIALOG_HIDE              = 16; { hide TUtility dialogs }
  TU_NO_LOCK                  = 32; { lock table being verified (recommended) }

{ Verify table error codes }
  VFE_WARNING           = 0; { warning error }
  VFE_DAMAGE_VERIFY     = 1; { table is damaged, verification can continue }
  VFE_DAMAGE_NOT_VERIFY = 2; { table is damaged; verification cannot continue }
  VFE_REBUILD_MANUALLY  = 3; { table must be rebuilt manually }
  VFE_CANNOT_REBUILD    = 4; { table cannot be rebuilt; restore from a backup }

const
{$IFDEF WIN32}
  TULib = 'TUTIL32.DLL';
{$ELSE}
  TULib = 'TUTILITY.DLL';
{$ENDIF}

var
  TUHandle: THandle = 0;
  TUInit: function (hTUSession: PHTUses): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUVerifyTable: function (hTUSession: HTUses; pszTableName,
    pszDriverType, pszErrTableName, pszPassword: PChar; iOptions: Integer;
    var piErrorLevel: Cardinal): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TURebuildTable: function (hTUSession: HTUses; pszTableName,
    pszDriverType, pszBackupTableName, pszKeyviolName,
    pszProblemTableName: PChar;
    pCrDesc: pCRTblDesc): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUGetCRTblDescCount: function (hTUSession: HTUses;
    pszTableName: PChar; var iFldCount,iIdxCount, iSecRecCount,
    iValChkCount, iRintCount, iOptParams,
    iOptDataLen: Word): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUFillCRTblDesc: function (hTUSession: HTUses; pCrDesc: pCRTblDesc;
    pszTableName,
    pszPassword: PChar): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUFillCURProps: function (hTUSession: HTUses; pszTableName: PChar;
    tblProps: pCURProps): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUExit: function (hTUSession: HTUses): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};
  TUGetErrorString: function (iErrorCode: DBIResult;
    pszError: PChar): DBIResult {$IFDEF WIN32} stdcall {$ENDIF};

{$IFDEF COMPILER3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  STUNotLoaded = 'Unable to load %s library';
  STUNoTables = 'No Paradox tables to verify';
  STUVerifyComplete = 'Verification successful. ';
  STUVerifyOk = 'Table %s verify complete. No errors found.';
  STUDamage = 'Table %s is damaged. Rebuild it.';
  STURebuild = 'Table %s is damaged. Rebuild?';
  STURebuildManual = 'Table %s is damaged and must be rebuilt manually.';
  STUNoRebuild = 'Table %s is damaged and cannot be rebuilt; restore from a backup.';
  STUUnknownError = 'Unknown %s error, code %d';
  STUPwDlgCaption = 'Enter Table Password';
  STUPwDlgPrompt = 'Enter master password for table %s:';

function TUtilityLoaded: Boolean;
begin
  Result := TUHandle >= HINSTANCE_ERROR;
end;

function LoadTUtility: Boolean;
var
  OldError: Word;
  Path: string;
{$IFNDEF WIN32}
  P: array[0..255] of Char;
{$ENDIF}
begin
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    Path := NormalDir(GetBdeDirectory) + TULib;
{$IFDEF WIN32}
    TUHandle := LoadLibrary(PChar(Path));
{$ELSE}
    TUHandle := LoadLibrary(StrPCopy(P, Path));
{$ENDIF}
    if not TUtilityLoaded then begin
      Path := TULib;
{$IFDEF WIN32}
      TUHandle := LoadLibrary(PChar(Path));
{$ELSE}
      TUHandle := LoadLibrary(StrPCopy(P, Path));
{$ENDIF}
    end;
    if TUtilityLoaded then begin
      @TUInit := GetProcAddress(TUHandle, 'TUInit');
      @TUVerifyTable := GetProcAddress(TUHandle, 'TUVerifyTable');
      @TURebuildTable := GetProcAddress(TUHandle, 'TURebuildTable');
      @TUGetCRTblDescCount := GetProcAddress(TUHandle, 'TUGetCRTblDescCount');
      @TUFillCRTblDesc := GetProcAddress(TUHandle, 'TUFillCRTblDesc');
      @TUFillCURProps := GetProcAddress(TUHandle, 'TUFillCURProps');
      @TUExit := GetProcAddress(TUHandle, 'TUExit');
      @TUGetErrorString := GetProcAddress(TUHandle, 'TUGetErrorString');
    end
    else TUHandle := 1;
  finally
    SetErrorMode(OldError);
  end;
  Result := TUtilityLoaded;
end;

procedure FreeTUtility; far;
begin
  if TUtilityLoaded then FreeLibrary(TUHandle);
  TUHandle := 0;
end;

procedure CheckTU;
begin
  if not TUtilityLoaded then
    raise EDatabaseError.CreateFmt(STUNotLoaded, [TULib]);
end;

{ ETUtilityError }

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
    if Source^ <= ' ' then Blank := True else
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
  if (Dest > Msg) and (Dest[Word(-1)] = '.') then Dec(Dest);
  Dest^ := #0;
  Result := Msg;
end;

type
  EDBEngineErrorHack = class(EDatabaseError)
  private
    FErrors: TList;
  end;

constructor ETUtilityError.Create(ErrorCode: DBIResult);
var
  ErrorIndex: Integer;
  NativeError: Longint;
  Msg, LastMsg: DBIMSG;
begin
  inherited Create(0);
{$IFDEF WIN32}
  if not Session.Active then Exit;
{$ENDIF}
  with EDBEngineErrorHack(Self) do begin
    if FErrors <> nil then begin
      for ErrorIndex := FErrors.Count - 1 downto 0 do
        TDBError(FErrors[ErrorIndex]).Free;
      FErrors.Clear;
    end;
  end;
  ErrorIndex := 1;
  try
    TUGetErrorString(ErrorCode, Msg);
    TDBError.Create(Self, ErrorCode, 0, Msg);
    TrimMessage(Msg);
    if Msg[0] = #0 then
      Message := Format(STUUnknownError, [TULib, ErrorCode])
    else Message := StrPas(Msg);
    while True do begin
      StrCopy(LastMsg, Msg);
      ErrorCode := DbiGetErrorEntry(ErrorIndex, NativeError, Msg);
      if (ErrorCode = DBIERR_NONE) or
        (ErrorCode = DBIERR_NOTINITIALIZED) then Break;
      TDBError.Create(Self, ErrorCode, NativeError, Msg);
      TrimMessage(Msg);
      if (Msg[0] <> #0) and (StrComp(Msg, LastMsg) <> 0) then
        Message := Format('%s. %s', [Message, Msg]);
      Inc(ErrorIndex);
    end;
  except
    Message := Format(STUUnknownError, [TULib, ErrorCode]);
  end;
end;

{ TTUtility }

constructor TTUtility.Create;
begin
  inherited Create;
  FCheckRepair := crConfirmRepair;
  FVerifyOptions := [vfIgnoreWarnings];
end;

destructor TTUtility.Destroy;
begin
  ClearTblDesc;
  inherited Destroy;
end;

procedure TTUtility.RunTUtility(Action: TTUAction);
var
  FCallback: TJvDBCallback ;
begin
  CheckTU;
  Check(TUInit(@FSession));
  try
    FCallback := TJvDBCallback .Create(Self, cbGENPROGRESS,
      SizeOf(TUVerifyCallBack), ProgressCallback, dcChain);
    try
      Action;
    finally
      FCallback.Free;
    end;
  finally
    TUExit(FSession);
  end;
end;

function TTUtility.CheckOpen(Status: DBIResult): Boolean;
begin
  Result := True;
  case Status of
    DBIERR_NONE: Result := True;
    DBIERR_NOTSUFFTABLERIGHTS:
      begin
        if not Session.GetPassword then Check(Status);
        Result := False;
      end;
    else if (Status <> 0) then Check(Status);
  end;
end;

procedure TTUtility.Check(Status: DBIResult);
var
  ErrInfo: DBIErrInfo;
begin
  if Status <> 0 then begin
    DbiGetErrorInfo(True, ErrInfo);
    if (ErrInfo.iError = Status) then DbiError(Status)
    else raise ETUtilityError.Create(Status);
  end;
end;

function TTUtility.ProgressCallback(CBInfo: Pointer): CBRType;
begin
  Result := cbrCONTINUE;
  with PUVerifyCallBack(CBInfo)^ do begin
    StrPCopy(TableName, Self.TableName);
    if (PercentDone = 0) then PercentDone := -1;
  end;
end;

function TTUtility.ErrorString(ErrorCode: DBIResult): string;
var
  Msg: DBIMSG;
begin
  CheckTU;
  TUGetErrorString(ErrorCode, Msg);
  TrimMessage(Msg);
  if Msg[0] = #0 then Result := Format(STUUnknownError, [TULib, ErrorCode])
  else Result := StrPas(Msg);
end;

function TTUtility.VerifyFlag: Integer;
const
  VerifyFlags: array[TVerifyOption] of Integer =
    (TU_APPEND_ERRORS, TU_BYPASS_SECONDARY_INDEXES, TU_IGNORE_WARNINGS,
    TU_VERIFY_HEADER_ONLY, TU_NO_LOCK, TU_DIALOG_HIDE);
var
  I: TVerifyOption;
begin
  Result := 0;
  for I := Low(TVerifyOption) to High(TVerifyOption) do
    if I in FVerifyOptions then Result := Result or VerifyFlags[I];
end;

procedure TTUtility.SetTabName(const TabName: string; const Dest: DBIPATH);
var
  P: PChar;
begin
  P := @Dest[0];
  if ChangeFileExt(TabName, '') <> StrPas(Dest) then begin
    if TabName <> '' then
      StrPLCopy(Dest, AnsiUpperCase(ChangeFileExt(TabName, '')),
        SizeOf(Dest) - 1)
    else FillChar(P^, SizeOf(Dest), #0);
  end;
end;

function TTUtility.GetPassword: string;
begin
  Result := StrPas(FPassword);
end;

procedure TTUtility.SetPassword(const Value: string);
begin
  if Value <> Password then begin
    if Value <> '' then
      StrPLCopy(FPassword, Value, SizeOf(FPassword) - 1)
    else FillChar(FPassword, SizeOf(FPassword), 0);
  end;
end;

function TTUtility.GetCheckErrorTable: string;
begin
  Result := StrPas(FCheckErrorTable);
end;

procedure TTUtility.SetCheckErrorTable(const Value: string);
begin
  SetTabName(Value, FCheckErrorTable);
end;

function TTUtility.GetErrorTable: string;
begin
  Result := StrPas(FErrorTable);
end;

procedure TTUtility.SetErrorTable(const Value: string);
begin
  SetTabName(Value, FErrorTable);
end;

function TTUtility.GetProblemTable: string;
begin
  Result := StrPas(FProblemTable);
end;

procedure TTUtility.SetProblemTable(const Value: string);
begin
  SetTabName(Value, FProblemTable);
end;

function TTUtility.GetKeyViolationTable: string;
begin
  Result := StrPas(FKeyViolationTable);
end;

procedure TTUtility.SetKeyViolationTable(const Value: string);
begin
  SetTabName(Value, FKeyViolationTable);
end;

function TTUtility.GetBackupTable: string;
begin
  Result := StrPas(FBackupTable);
end;

procedure TTUtility.SetBackupTable(const Value: string);
begin
  SetTabName(Value, FBackupTable);
end;

function TTUtility.GetTableName: string;
begin
  Result := StrPas(FTableName);
end;

procedure TTUtility.SetTableName(const Value: string);
begin
  SetTabName(Value, FTableName);
end;

function TTUtility.ShowPasswordDialog: Boolean;
var
  S: string;
begin
  S := Password;
  Result := InputQuery(STUPwDlgCaption, Format(STUPwDlgPrompt,
    [ExtractFileName(TableName)]), S);
  if Result then Password := S;
end;

procedure TTUtility.FillTblDesc;
begin
  FillChar(FTblDesc, SizeOf(FTblDesc), 0);
  Check(TUGetCRTblDescCount(FSession, FTableName, FTblDesc.iFldCount,
    FTblDesc.iIdxCount, FTblDesc.iSecRecCount, FTblDesc.iValChkCount,
    FTblDesc.iRintCount, FTblDesc.iOptParams, FOptDataLen));
  StrPCopy(FTblDesc.szTblName, TableName);
  StrCopy(FTblDesc.szTblType, szPARADOX);
  StrPCopy(FTblDesc.szErrTblName, ErrorTable);
  GetMem(FTblDesc.pFldDesc, FTblDesc.iFldCount * SizeOf(FldDesc));
  GetMem(FTblDesc.PIdxDesc, FTblDesc.iIdxCount * SizeOf(IdxDesc));
  GetMem(FTblDesc.pSecDesc, FTblDesc.iSecRecCount * SizeOf(SecDesc));
  GetMem(FTblDesc.pVchkDesc, FTblDesc.iValChkCount * SizeOf(VchkDesc));
  GetMem(FTblDesc.pRintDesc, FTblDesc.iRintCount * SizeOf(RintDesc));
  GetMem(FTblDesc.pfldOptParams, FTblDesc.iOptParams * SizeOf(FldDesc));
  GetMem(FTblDesc.pOptData, FOptDataLen * DBIMAXSCFLDLEN);
  try
    while not CheckOpen(TUFillCRTblDesc(FSession, @FTblDesc, FTableName,
      FPassword)) do {Retry};
  except
    ClearTblDesc;
    raise;
  end;
end;

procedure TTUtility.ClearTblDesc;
begin
  if FTblDesc.pFldDesc <> nil then
    FreeMem(FTblDesc.pFldDesc, FTblDesc.iFldCount * SizeOf(FldDesc));
  if FTblDesc.PIdxDesc <> nil then
    FreeMem(FTblDesc.PIdxDesc, FTblDesc.iIdxCount * SizeOf(IdxDesc));
  if FTblDesc.pSecDesc <> nil then
    FreeMem(FTblDesc.pSecDesc, FTblDesc.iSecRecCount * SizeOf(SecDesc));
  if FTblDesc.pVchkDesc <> nil then
    FreeMem(FTblDesc.pVchkDesc, FTblDesc.iValChkCount * SizeOf(VchkDesc));
  if FTblDesc.pRintDesc <> nil then
    FreeMem(FTblDesc.pRintDesc, FTblDesc.iRintCount * SizeOf(RintDesc));
  if FTblDesc.pFldOptParams <> nil then
    FreeMem(FTblDesc.pFldOptParams, FTblDesc.iOptParams * SizeOf(FldDesc));
  if FTblDesc.pOptData <> nil then
    FreeMem(FTblDesc.pOptData, FOptDataLen * DBIMAXSCFLDLEN);
  FillChar(FTblDesc, SizeOf(FTblDesc), 0);
end;

procedure TTUtility.DoRepairTable;
var
  CurProp: CURProps;
  PasswordEmpty: Boolean;
begin
  if TableName = '' then Exit;
  while not CheckOpen(TUFillCURProps(FSession, FTableName,
    @CurProp)) do {Retry};
  PasswordEmpty := Password = '';
  if CurProp.bProtected and PasswordEmpty then
    if not ShowPasswordDialog then
      Exit; { no password specified - no repair }
  try
    VerifyTable;
    FillTblDesc;
    try
      Screen.Cursor := crHourGlass;
      try
{$IFNDEF WIN32}
        CheckBackupTable;
{$ENDIF}
        while not CheckOpen(TURebuildTable(FSession, FTableName,
          szPARADOX, FBackupTable, FKeyViolationTable, FProblemTable,
          @FTblDesc)) do {Retry};
      finally
        Screen.Cursor := crDefault;
      end;
    finally
      ClearTblDesc;
    end;
  finally
    if PasswordEmpty then Password := '';
  end;
end;

function TTUtility.VerifyTable: Cardinal;
begin
  CheckTU;
  { TUtility must be re-initialized for each verification }
  Check(TUExit(FSession));
  Check(TUInit(@FSession));
  Screen.Cursor := crHourGlass;
  try
    while not CheckOpen(TUVerifyTable(FSession, FTableName, szPARADOX,
      FCheckErrorTable, FPassword, VerifyFlag, Result)) do {Retry};
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TTUtility.DoCheckTable;

  function TabName: string;
  begin
    Result := ExtractFileName(ChangeFileExt(TableName, '.DB'));
  end;

var
  ErrMsg: string;
begin
  if TableName = '' then Exit;
  case VerifyTable of
    VFE_WARNING: if FShowNoError then
      MessageDlg(Format(STUVerifyOk, [TabName]), mtInformation, [mbOk], 0);
    VFE_DAMAGE_VERIFY, VFE_DAMAGE_NOT_VERIFY:
      begin
        ErrMsg := TULastErrorMessage;
        case FCheckRepair of
          crNoRepair: MessageDlg(ErrMsg + Format(STUDamage, [TabName]),
            mtError, [mbOk], 0);
          crAutoRepair: RepairTable;
          crConfirmRepair:
            if MessageDlg(ErrMsg + Format(STURebuild, [TabName]),
              mtError, [mbYes, mbNo], 0) = mrYes then
              RepairTable;
        end;
      end;
    VFE_REBUILD_MANUALLY:
      begin
        ErrMsg := TULastErrorMessage;
        MessageDlg(ErrMsg + Format(STURebuildManual, [TabName]), mtError,
          [mbOk], 0);
      end;
    VFE_CANNOT_REBUILD:
      begin
        ErrMsg := TULastErrorMessage;
        MessageDlg(ErrMsg + Format(STUNoRebuild, [TabName]), mtError,
          [mbOk], 0);
      end;
  end;
end;

procedure TTUtility.CheckTable;
begin
  RunTUtility(DoCheckTable);
end;

procedure TTUtility.RepairTable;
begin
  RunTUtility(DoRepairTable);
end;

function TTUtility.TULastErrorMessage: string;
var
  Table: TTable;
begin
  Result := '';
  if CheckErrorTable = '' then Exit;
  Table := TTable.Create(Application);
  try
    Table.TableName := ChangeFileExt(CheckErrorTable, '.DB');
    try
      Table.Open;
      Table.Last;
      Result := Table.FieldByName('Error Message').AsString;
      if Result <> '' then Result := Result + '. ';
    except
      Result := '';
    end;
  finally
    Table.Free;
  end;
  if Result = '' then Result := STUVerifyComplete;
end;

procedure TTUtility.DropErrorTable;
begin
  if CheckErrorTable = '' then Exit;
  with TTable.Create(Application) do
  try
    TableName := ChangeFileExt(CheckErrorTable, '.DB');
    if FileExists(TableName) then DeleteTable;
  finally
    Free;
  end;
end;

procedure TTUtility.CheckBackupTable;
var
  TabPath: string;
begin
  TabPath := ChangeFileExt(TableName, '');
  if TabPath <> '' then begin
    Delete(TabPath, Length(TabPath), 1);
    BackupTable := TabPath + '_.DB';
  end else BackupTable := '';
end;

procedure TTUtility.DefaultBackupNames;
var
  TabPath: string;
begin
  TabPath := NormalDir(GetEnvVar('TEMP'));
  if (TabPath = '') then
    TabPath := NormalDir(ExtractFilePath(TableName));
  CheckErrorTable := TabPath + 'VERIFY.DB';
  ErrorTable := TabPath + 'REBUILD.DB';
  ProblemTable := TabPath + 'PROBLEM.DB';
  KeyViolationTable := TabPath + 'KEYVIOL.DB';
  CheckBackupTable;
end;

{ Utility routines }

function GetPxTableNames(const DirectoryName: string; List: TStrings): string;
var
  hDB: HDBIDb;
  Cursor: HDBICur;
  DirName: string;
  Desc: FILEDesc;
  DbPath: DBIPATH;
begin
{$IFDEF WIN32}
  Session.Active := True;
{$ENDIF}
  DirName := DirectoryName;
  if not IsDirectory(DirName) then DirName := GetAliasPath(DirName);
  Result := DirName;
  Check(DbiOpenDatabase(nil, nil, dbiREADWRITE, dbiOPENSHARED,
    nil, 0, nil, nil, hDB));
  try
    Check(DbiSetDirectory(hDB, StrPLCopy(DbPath, DirName, SizeOf(DbPath) - 1)));
    List.BeginUpdate;
    try
      List.Clear;
      Check(DbiOpenFileList(hDB, '*.db', Cursor));
      try
        while DbiGetNextRecord(Cursor, dbiNOLOCK, @Desc, nil) = 0 do begin
          if (DirName[Length(DirName)] <> '\') and (Length(DirName) > 1) then
            DirName := DirName + '\'
          else if Length(DirName) = 1 then DirName := DirName + ':\';
          List.Add(Format('%s%s', [DirName, StrPas(Desc.szFileName)]));
        end;
      finally
        DbiCloseCursor(Cursor);
      end;
    finally
      List.EndUpdate;
    end;
  finally
    DbiCloseDatabase(hDB);
  end;
end;

procedure CheckTable(const TableName: string; Repair: TCheckRepair);
var
  TU: TTUtility;
begin
  CheckTU;
  if not FileExists(ChangeFileExt(TableName, '.DB')) then
    DatabaseError(STUNoTables);
  TU := TTUtility.Create;
  try
    TU.CheckRepair := Repair;
    TU.ShowNoError := True;
    try
      TU.TableName := TableName;
      TU.DefaultBackupNames;
      TU.CheckTable;
      TU.DropErrorTable;
    except
      on E: ETUtilityError do
        begin
          if TUtilityLoaded then Application.HandleException(TU)
          else raise;
        end;
      else raise;
    end;
  finally
    TU.Free;
  end;
end;

procedure CheckTables(const TablesDir: string; Repair: TCheckRepair);
var
  List: TStrings;
  TU: TTUtility;
  I: Integer;
begin
  CheckTU;
  TU := TTUtility.Create;
  try
    List := TStringList.Create;
    try
      GetPxTableNames(TablesDir, List);
      if List.Count <= 0 then DatabaseError(STUNoTables);
      TU.CheckRepair := Repair;
      TU.ShowNoError := False;
      for I := 0 to List.Count - 1 do
        try
          TU.TableName := List[I];
          TU.DefaultBackupNames;
          TU.CheckTable;
        except
          on E: ETUtilityError do
            begin
              if TUtilityLoaded then Application.HandleException(TU)
              else raise;
            end;
          else raise;
        end;
      TU.DropErrorTable;
    finally
      List.Free;
    end;
  finally
    TU.Free;
  end;
end;

initialization
  LoadTUtility;
{$IFNDEF WIN32}
  AddExitProc(FreeTUtility);
{$ENDIF}
end.
