{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JvUIB.pas.                                              }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ UIB Visual Components: TUIBDataBase, TUIBComponent, TUIBTransaction,         }
{   TUIBSQL, TUIBQuery.                                                        }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: Jun 08, 2003                                                  }
{                                                                              }
{******************************************************************************}

unit JvUIB;

{$I JCL.INC}
{$I JvUIB.inc}

(*------------------------------------------------------------------------------
  This is a cascading programming style.

..............oOo...............................oOo.........oOo.................
 States        |    Operations                   |  Commands | Components
..............oOo...............................oOo.........oOo.................
 qsDataBase    |  BeginDataBase(L)               |           | TUIBDataBase
--------------------------------------------------------------------------------
 qsTransaction |  |-> BeginTransaction           |           | TUIBTransaction
--------------------------------------------------------------------------------
 qsExecImme    |      |-> BeginExecImme .........|.[ExecSQL] | TUIBQuery
 qsStatement   |      |-> BeginStatement         |           |
 qsPrepare     |      |   |-> BeginPrepare       |           |
 qsExecute     |      |   |   |-> BeginExecute   |.[Execute] |
               |      |   |   |   |-> Next ......|.[Open]    |
               |      |   |   |   |   |          |           |
               | R <- E   E   E   E   E          | [Fields]  |
               |          |   |   |   |          |           |
 qsExecute     |          |   |   |<- EndExecute |.[Close]   |
 qsPrepare     |          |   |<- EndPrepare     |           |
 qsStatement   |          |<- EndStatement       |           |
 -------------------------------------------------------------------------------
 qsTransaction |          EndTransaction         |           | TUIBTransaction
..............oOo...............................oOo.........oOo.................
 LEGEND
   E  = Except
   R  = Raise
   -> = Call
------------------------------------------------------------------------------*)

interface
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UIBJVCL} jvComponent, {$ENDIF}
  Classes, SysUtils, JvUIBLib, JvUIBase, SyncObjs;

type

 {Oo.........................................................................oO
                              TUIBComponent

      Synchronise Databases, Transactions and Queries.

    Library        | TUIBDatabase     | TUIBTransaction  | TUIBQuery
   ==========================================================================
    Lock <---------|------------------|------------------|-----------------o
                   | Lock <-----------|------------------|---------------o
                   |                  | Lock <-----------|-------------o
                   |                  |                  | Lock   <--o
                   |                  |                  | UnLock <--o
                   |                  | UnLock <---------|-------------o
                   | UnLock <---------|------------------|---------------o
    UnLock <-------|------------------|------------------|-----------------o

      Note: With Interbase 7, no need to synchronise anything but removing
      Synchronisation you have exactly the same performance than IB6.01 with
      Synchronisation on a single CPU !

  Oo.........................................................................oO}

{-----------------------------------------------------------------------------
  Class: TJvUIBComponent
  All UIB components inherith from this class to encapsulate Critical Sections.
  Critical Sections make UIB THread Safe.
-----------------------------------------------------------------------------}
{$IFDEF UIBNOCOMPONENT}
  TJvUIBComponent = class(TObject)
{$ELSE}
  TJvUIBComponent = class(TComponent)
{$ENDIF}
  private
    FCriticalsection: TCriticalSection;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; virtual;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    { Lock the critical Section. }
    procedure Lock; virtual;
    { UnLock the critical Section. }
    procedure UnLock; virtual;
  end;

  // Forward declarations
  TJvUIBTransaction = class;
  TJvUIBQuery = class;
  TJvUIBSQL = class;

{-----------------------------------------------------------------------------
  Class: TJvUIBDataBase
  Manage a Database connection.
-----------------------------------------------------------------------------}
  TJvUIBDataBase = class(TJvUIBComponent)
  private
    FDbHandle: IscDbHandle;
    FHandleShared: boolean;
    FParams: TStrings;
    FDatabaseName: TFileName;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FTransactions: TList;
    function ReadParamString(Param: String; Default: String = ''): String;
    procedure WriteParamString(Param: String; Value: String);
    function ReadParamInteger(Param: String; Default: Integer): Integer;
    procedure WriteParamInteger(Param: String; Value: Integer);
    procedure SetParams(const Value: TStrings);
    procedure SetDatabaseName(const Value: TFileName);
    procedure SetConnected(const Value: Boolean);
    function GetConnected: Boolean;
    procedure SetSQLDialect(const Value: Integer);
    function GetSQLDialect: Integer;
    function GetCharacterSet: TCharacterSet;
    procedure SetCharacterSet(const Value: TCharacterSet);
    function GetPassWord: string;
    function GetUserName: string;
    procedure SetPassWord(const Value: string);
    procedure SetUserName(const Value: string);
    procedure AddTransaction(Transaction: TJvUIBTransaction);
    procedure RemoveTransaction(Transaction: TJvUIBTransaction);
    procedure ClearTransactions;
    procedure CloseTransactions;
    procedure SetHandle(const Value: IscDbHandle);
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    property Handle: IscDbHandle read FDbHandle write SetHandle;
    property IsHandleShared : Boolean read FHandleShared;
  published
    { DataBase connection parametters. }
    property Params: TStrings read FParams write SetParams;
    { Database file name. }
    property DatabaseName: TFileName read FDatabaseName write SetDatabaseName;
    { Connect or disconnect a database. }
    property Connected: Boolean read GetConnected write SetConnected default False;
    { The SQL dialect gives access to DSQL features, set the dialect to 1 or 3.
      Dialect 3 gives access to features introduced in InterBase 6. }
    property SQLDialect: Integer read GetSQLDialect write SetSQLDialect default 3;
    { Character set to be utilized. }
    property CharacterSet: TCharacterSet read GetCharacterSet write SetCharacterSet default csNONE;
    { Set the user name. Default = SYSDBA. }
    property UserName: string read GetUserName write SetUserName;
    { Set the Password. Default = masterkey. }
    property PassWord: string read GetPassWord write SetPassWord;
    { This event occur after the component is connected to database. }
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    { This event occur before the component is connected to database. }
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    { This event occur after the component is disconnected from database. }
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    { This event occur before the component is disconnected from database. }
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
  end;

  { Describe how a transaction is closed. }
  TEndTransMode = (
    etmStayIn,           // keep transaction without commit or rollback
    etmCommit,           // commit transaction
    etmCommitRetaining,  // commit transaction and keep transaction handle
    etmRollback,         // rollback transaction
    etmRollbackRetaining // rollback transaction and keep transaction handle
  );

  { Indicate the Query state.
    order is important ! }
  TQueryState = (
    qsDataBase,    // have a database handle
    qsTransaction, // have a transaction handle
    qsStatement,   // have a statement handle
    qsExecImme,    // Query executed
    qsPrepare,     // Query prepared
    qsExecute      // Query opened
  );

  {Oo.......................................................................oO
                                  TUIBTransaction
   Oo.......................................................................oO}

  // Transaction parameters
  TTransParam = (
    tpConsistency,     // prevents a transaction from accessing tables if they are written to by other transactions.
    tpConcurrency,     // allows concurrent transactions to read and write shared data.
    tpShared,          // Concurrent, shared access of a specified table among all transactions.
    tpProtected,       // Concurrent, restricted access of a specified table.
    tpExclusive,       // ???
    tpWait,            // Specifies that the transaction is to wait until the conflicting resource is released before retrying an operation [Default].
    tpNowait,          // Specifies that the transaction is not to wait for the resource to be released, but instead, should return an update conflict error immediately.
    tpRead,            // Read-only access mode that allows a transaction only to select data from tables.
    tpWrite,           // Read-write access mode of that allows a transaction to select, insert, update, and delete table data [Default].
    tpLockRead,        // Read-only access of a specified table. Use in conjunction with tpShared, tpProtected, and tpExclusive to establish the lock option.
    tpLockWrite,       // Read-write access of a specified table. Use in conjunction with tpShared, tpProtected, and tpExclusive to establish the lock option [Default].
    tpVerbTime,        // ???
    tpCommitTime,      // ???
    tpIgnoreLimbo,     // ???
    tpReadCommitted,   // Unlike a concurrency transaction, a read committed transaction sees changes made and committed by transactions that were active after this transaction started.
    tpAutoCommit,      // ???
    tpRecVersion,      // Enables an tpReadCommitted transaction to read only the latest committed version of a record.
    tpNoRecVersion,    // ???
    tpRestartRequests, // ???
    tpNoAutoUndo       // ???
  );

  // Set of transaction parameters
  TTransParams = set of TTransParam;

  TJvUIBTransaction = class(TJvUIBComponent)
  private
    FDataBase: TJvUIBDataBase;
    FTransaction: IscTrHandle;
    FSQLComponent: TList;
    FStatements: Integer;
    FOptions   : TTransParams;
    FLockRead  : string;
    FLockWrite : string;
    FSQLDialect: Integer;
    function GetInTransaction: Boolean;
    function TPB: string;
    function GetOptions: TTransParams;
    procedure SetOptions(const Value: TTransParams);
    function GetLockRead: string;
    function GetLockWrite: string;
    procedure SetLockRead(const Value: string);
    procedure SetLockWrite(const Value: string);
    function GetDataBase: TJvUIBDataBase;
    procedure BeginDataBase;
    procedure BeginTransaction;
    procedure EndTransaction(ETM: TEndTransMode);
    procedure AddSQLComponent(Component: TJvUIBSQL);
    procedure RemoveSQLComponent(Component: TJvUIBSQL);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode = etmStayIn);
  protected
{$IFNDEF UIBNOCOMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
    procedure SetDataBase(const Database: TJvUIBDataBase); virtual;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure Lock; override;
    procedure UnLock; override;

    procedure Commit;
    procedure CommitRetaining;
    procedure RollBack;
    procedure RollBackRetaining;

    property InTransaction: Boolean read GetInTransaction;
    property Handle: IscTrHandle read FTransaction;

  published

    property DataBase  : TJvUIBDataBase read GetDataBase write SetDataBase;
    property Options   : TTransParams   read GetOptions    write SetOptions default [tpConcurrency,tpWait,tpWrite];
    property LockRead  : string         read GetLockRead   write SetLockRead;
    property LockWrite : string         read GetLockWrite  write SetLockWrite;
  end;

  TJvUIBSQL = class(TJvUIBComponent)
  private
    FCurrentState: TQueryState;
    FTransaction: TJvUIBTransaction;
    procedure BeginTransaction;
    procedure EndTransaction(const ETM: TEndTransMode);
  protected
    procedure SetTransaction(const Transaction: TJvUIBTransaction); virtual;
{$IFNDEF UIBNOCOMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
  public
    procedure Close(const Mode: TEndTransMode = etmStayIn); virtual;
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure Lock; override;
    procedure UnLock; override;
    property CurrentState: TQueryState read FCurrentState;
  published
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
  end;

  {Oo.......................................................................oO
                                  TUIBQuery
   Oo.......................................................................oO}
  TJvUIBQuery = class(TJvUIBSQL)
  private
    FStatement: IscStmtHandle;
    FQuickScript: boolean;
    FSQL: TStrings;
    FSQLResult: TSQLResult;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FParameter: TSQLParams;
    FEOF: boolean;
    FBufferChunks: Cardinal;
    FOnError: TEndTransMode;
    FCursorName: string;
    FUseCursor: boolean;
    function GetFields: TSQLResult;
    procedure BeginStatement;
    procedure EndStatement(const ETM: TEndTransMode);
    procedure EndExecImme(const ETM: TEndTransMode);
    procedure OnSQLChange(Sender: TObject);
    procedure SetSQL(const Value: TStrings);
    procedure EndPrepare(const ETM: TEndTransMode);
    procedure EndExecute(const ETM: TEndTransMode);

    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var Value: Variant); overload;

  protected
    procedure BeginPrepare;
    procedure BeginExecute;
    procedure BeginExecImme;
    function  ParamsClass: TSQLParamsClass; virtual;
    function  ResultClass: TSQLResultClass; virtual;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;

    procedure Open;
    procedure ExecSQL;
    procedure Execute;
    procedure Close(const Mode: TEndTransMode = etmStayIn); override;
    procedure Next;
    procedure FetchAll;

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    procedure ReadBlob(const name: string; var Value: Variant); overload;

    function ParamsAddBlob(Stream: TStream): Word; overload;
    function ParamsAddBlob(var Str: String): Word; overload;
    function ParamsAddBlob(Buffer: Pointer; Size: Word): Word; overload;

    procedure ParamsReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ParamsReadBlob(const Index: Word; var str: string); overload;
    procedure ParamsReadBlob(const Index: Word; var Value: Variant); overload;

    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;

    property Eof: boolean read FEOF;
    property Fields: TSQLResult read GetFields;
    property Params: TSQLParams read FParameter;
    property CursorName: string read FCursorName;
  published
    property SQL: TStrings read FSQL write SetSQL;
    property CachedFetch: boolean read FCachedFetch write FCachedFetch default True;
    property FetchBlobs: boolean read FFetchBlobs write FFetchBlobs default False;
    property QuickScript: boolean read FQuickScript write FQuickScript  default False;
    property BufferChunks: Cardinal read FBufferChunks write FBufferChunks default 1000;
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    property UseCursor: boolean read FUseCursor write FUseCursor default False;
  end;

  TUIBProtocol = (
    proLocalHost,
    proTCPIP,
    proNetBUI
  );

  TJvUIBService = class(TJvUIBComponent)
  private
    FUserName: string;
    FPassWord: string;
    FHost    : string;
    FProtocol: TUIBProtocol;
    FHandle  : IscSvcHandle;
    procedure BeginService;
    procedure EndService;
    function CreateSPB: string; virtual;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
  published
    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property Host: string read FHost write FHost;
    property Protocol: TUIBProtocol read FProtocol write FProtocol default proLocalHost;
  end;

  TVerboseEvent = procedure(Sender: TObject; Message: string) of object;


  TJvUIBBackupRestore = class(TJvUIBService)
  private
    FBackupFiles: TStrings;
    FDatabase: TFileName;
    FOnVerbose: TVerboseEvent;
    procedure SetBackupFiles(const Value: TStrings);
    function CreateStartSPB: string; virtual; abstract;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure Run;
  published
    property BackupFiles: TStrings read FBackupFiles write SetBackupFiles;
    property Database: TFileName read FDatabase write FDatabase;
    property OnVerbose: TVerboseEvent read FOnVerbose write FOnVerbose;
  end;

  TBackupOption = (boIgnoreChecksums, boIgnoreLimbo, boMetadataOnly,
    boNoGarbageCollection, boOldMetadataDesc, boNonTransportable,
    boConvertExtTables, boExpand);
  TBackupOptions = set of TBackupOption;

  TJvUIBBackup = class(TJvUIBBackupRestore)
  private
    FOptions: TBackupOptions;
    function CreateStartSPB: string; override;
  published
    property Options: TBackupOptions read FOptions write FOptions default [];
  end;

  TRestoreOption = (roDeactivateIndexes, roNoShadow, roNoValidityCheck,
    roOneRelationAtATime, roReplace, roCreateNewDB, roUseAllSpace);
  TRestoreOptions = set of TRestoreOption;

  TJvUIBRestore = class(TJvUIBBackupRestore)
  private
    FOptions: TRestoreOptions;
    FPageSize: Cardinal;
    function CreateStartSPB: string; override;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
  published
    property Options: TRestoreOptions read FOptions write FOptions default [roCreateNewDB];
    property PageSize: Cardinal read FPageSize write FPageSize default 0;
  end;

implementation
uses JvUIBConst;

{ TJvUIBDataBase }

const
  CharacterSetStr : array[TCharacterSet] of string = (
    'NONE', 'ASCII', 'BIG_5', 'CYRL', 'DOS437', 'DOS850', 'DOS852', 'DOS857',
    'DOS860', 'DOS861', 'DOS863', 'DOS865', 'EUCJ_0208', 'GB_2312', 'ISO8859_1',
    'ISO8859_2', 'KSC_5601', 'NEXT', 'OCTETS', 'SJIS_0208', 'UNICODE_FSS',
    'WIN1250', 'WIN1251', 'WIN1252', 'WIN1253', 'WIN1254'
{$IFDEF FB15_UP}
    ,'DOS737', 'DOS775', 'DOS858', 'DOS862', 'DOS864', 'DOS866', 'DOS869',
    'WIN1255', 'WIN1256', 'WIN1257', 'ISO8859_3', 'ISO8859_4', 'ISO8859_5',
    'ISO8859_6', 'ISO8859_7', 'ISO8859_8', 'ISO8859_9', 'ISO8859_13'
{$ENDIF}
    );

procedure TJvUIBDataBase.AddTransaction(Transaction: TJvUIBTransaction);
begin
  if (FTransactions = nil) then FTransactions := TList.Create;
  FTransactions.Add(Transaction);
end;

procedure TJvUIBDataBase.ClearTransactions;
begin
  while (FTransactions <> nil) do
    TJvUIBTransaction(FTransactions.Last).SetDataBase(nil);
end;

procedure TJvUIBDataBase.CloseTransactions;
var i: Integer;
begin
  if (FTransactions <> nil) then
    for i := 0 to FTransactions.Count - 1 do
      TJvUIBTransaction(FTransactions.Items[i]).Close(etmCommit);
end;

{$IFDEF UIBNOCOMPONENT}
  constructor TJvUIBDataBase.Create;
{$ELSE}
  constructor TJvUIBDataBase.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FDbHandle := nil;
  FHandleShared := False;
  FParams := TStringList.Create;
  SQLDialect := 3;
  CharacterSet := csNONE;
end;

destructor TJvUIBDataBase.Destroy;
begin
  Lock;
  try
    Connected := False;
    ClearTransactions;
    TStringList(FParams).Free;
  finally
    UnLock;
  end;
  inherited;
end;

function TJvUIBDataBase.GetCharacterSet: TCharacterSet;
var
  i: TCharacterSet;
  S: String;
begin
  S := trim(UpperCase(ReadParamString('lc_ctype', 'NONE')));
  Result := csNONE;
  for i := low(TCharacterSet) to high(TCharacterSet) do
    if (S = CharacterSetStr[i]) then
    begin
      Result := i;
      Break;
    end;
end;

function TJvUIBDataBase.GetConnected: Boolean;
begin
  Lock;
  try
    result := FDbHandle <> nil;
  finally
    UnLock;
  end;
end;

function TJvUIBDataBase.GetPassWord: string;
begin
  result := ReadParamString('password');
end;

function TJvUIBDataBase.GetSQLDialect: Integer;
begin
  try
    Result := ReadParamInteger('sql_dialect', 3);
  except
    WriteParamInteger('sql_dialect', 3);
    raise;
  end;
end;

function TJvUIBDataBase.GetUserName: string;
begin
  result := ReadParamString('user_name');
end;

function TJvUIBDataBase.ReadParamInteger(Param: String;
  Default: Integer): Integer;
begin
  Result := StrToInt(ReadParamString(Param, IntToStr(Default)));
end;

function TJvUIBDataBase.ReadParamString(Param, Default: String): String;
var
  I: Integer;
begin
  Lock;
  try
    I := FParams.IndexOfName(Param);
    if I >= 0 then             
    begin
      Result := Copy(FParams[I], Length(Param) + 2, Maxint);
      Exit;
    end;
    Result := Default;
  finally
    UnLock;
  end;
end;

procedure TJvUIBDataBase.RemoveTransaction(Transaction: TJvUIBTransaction);
begin
  if (FTransactions <> nil) then
  begin
    FTransactions.Remove(Transaction);
    if FTransactions.Count = 0 then
      FreeAndNil(FTransactions);
  end;
end;

procedure TJvUIBDataBase.SetCharacterSet(const Value: TCharacterSet);
begin
  WriteParamString('lc_ctype', CharacterSetStr[Value]);
end;

procedure TJvUIBDataBase.SetConnected(const Value: Boolean);
begin
  if (Value = Connected) then Exit;
  Lock;
  try
    case Value of
      True  :
        begin
          if Assigned(BeforeConnect) then BeforeConnect(Self);
          if not FHandleShared then
            AttachDatabase(FDatabaseName, FDbHandle, FParams.Text, #13);
          if Assigned(AfterConnect) then AfterConnect(Self);
        end;
      False :
        begin
          if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
          CloseTransactions;
          if FHandleShared then
          begin
            FDbHandle := nil;
            FHandleShared := False;
          end else
            DetachDatabase(FDbHandle);
          if Assigned(AfterDisconnect) then AfterDisconnect(Self);
        end;
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBDataBase.SetDatabaseName(const Value: TFileName);
begin
  FDatabaseName := Value;
end;

procedure TJvUIBDataBase.SetHandle(const Value: IscDbHandle);
begin
  if FDbHandle = nil then
  begin
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end else
    raise Exception.Create(EUIB_DBHANDLEALLREADYSET);
end;

procedure TJvUIBDataBase.SetParams(const Value: TStrings);
begin
  FParams.Assign(Value);
end;

procedure TJvUIBDataBase.SetPassWord(const Value: string);
begin
  WriteParamString('password', Value);
end;

procedure TJvUIBDataBase.SetSQLDialect(const Value: Integer);
begin
  WriteParamInteger('sql_dialect', Value);
end;

procedure TJvUIBDataBase.SetUserName(const Value: string);
begin
  WriteParamString('user_name', Value);
end;

procedure TJvUIBDataBase.WriteParamInteger(Param: String; Value: Integer);
begin
  WriteParamString(Param, IntToStr(Value));
end;

procedure TJvUIBDataBase.WriteParamString(Param, Value: String);
var
  I: Integer;
  S: string;
begin
  Lock;
  try
    S := Param + '=' + Value;
    I := FParams.IndexOfName(Param);
    if I >= 0 then
      FParams[I] := S
    else
      FParams.Add(S);
  finally
    UnLock;
  end;
end;

{ TJvUIBSQL }

procedure TJvUIBSQL.SetTransaction(const Transaction: TJvUIBTransaction);
begin
  if (FTransaction <> Transaction) then
  begin
    if (FTransaction <> nil) then
    begin
      Close(etmCommit);
      FTransaction.RemoveSQLComponent(Self);
    end;
    FTransaction := Transaction;
    if (Transaction <> nil) then
      Transaction.AddSQLComponent(Self);
  end;
end;

procedure TJvUIBSQL.BeginTransaction;
begin
  if FTransaction <> nil then
    FTransaction.BeginTransaction else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  FCurrentState := qsTransaction;
end;

procedure TJvUIBSQL.Close(const Mode: TEndTransMode);
begin

end;

procedure TJvUIBSQL.EndTransaction(const ETM: TEndTransMode);
begin
  if FTransaction <> nil then
    FTransaction.EndTransaction(ETM) else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  if (ETM in [etmCommit, etmRollback]) then
     FCurrentState := qsDataBase;
end;

procedure TJvUIBSQL.Lock;
begin
  inherited;
    Ftransaction.Lock;
end;

procedure TJvUIBSQL.UnLock;
begin
    Ftransaction.UnLock;
  inherited;
end;

{$IFNDEF UIBNOCOMPONENT}
procedure TJvUIBSQL.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
end;
{$ENDIF}

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBSQL.Create;
{$ELSE}
constructor TJvUIBSQL.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FCurrentState := qsDataBase;
{$IFNDEF UIBNOCOMPONENT}
  if (AOwner is TJvUIBTransaction) then
    Transaction := TJvUIBTransaction(AOwner) else
{$ENDIF}
    FTransaction := nil;
end;

destructor TJvUIBSQL.Destroy;
begin
  SetTransaction(nil);
  inherited;
end;

{ TJvUIBQuery }

//******************************************************************************
// Constructor Destructor                                                      *
//******************************************************************************
{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBQuery.Create;
{$ELSE}
constructor TJvUIBQuery.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FSQL         := TStringList.Create;
  TStringList(FSQL).OnChange := OnSQLChange;
  FEOF         := True;
  FCachedFetch := True;
  FetchBlobs   := False;
  FQuickScript := False;
  FOnError     := etmRollback;
  FParameter   := ParamsClass.Create;
  FCursorName  := '';
  FUseCursor   := False;
  FBufferChunks := 1000;
end;

destructor TJvUIBQuery.Destroy;
begin
  FSQL.Free;
  FreeAndNil(FParameter);
  inherited Destroy;
end;

//******************************************************************************
// Component features
//******************************************************************************

procedure TJvUIBQuery.OnSQLChange(Sender: TObject);
begin
  Close(etmStayIn);
end;

procedure TJvUIBQuery.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

//******************************************************************************
// Statement
//******************************************************************************

procedure TJvUIBQuery.BeginStatement;
begin
  BeginTransaction;
  Lock;
  try
    try
      FStatement := nil;
      DSQLAllocateStatement(FTransaction.FDataBase.FDbHandle, FStatement);
    except
      EndTransaction(FOnError);
      raise;
    end;
    inc(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsStatement;
end;

procedure TJvUIBQuery.EndStatement(const ETM: TEndTransMode);
begin
  Lock;
  try
    if FCursorName <> '' then
    begin
      FCursorName := '';
      DSQLFreeStatement(FStatement, DSQL_drop); 
    end else
      DSQLFreeStatement(FStatement, 0);
    FStatement := nil;
    Dec(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM);
end;

function TJvUIBQuery.GetFields: TSQLResult;
begin
  if (FSQLResult = nil) then
    raise EUIBError.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

//******************************************************************************
// ExecImme                                                                      *
//******************************************************************************

procedure TJvUIBQuery.BeginExecImme;
var
  I: Integer;
  procedure ExecuteQuery(const AQuery: String; Params: TSQLParams);
  begin
    if (Trim(AQuery) = '') then exit;
    Lock;
    try
      try
        DSQLExecuteImmediate(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
          AQuery, FTransaction.FSQLDialect, Params);
      except
        if (FOnError <> etmStayIn) then
          EndExecImme(FOnError);
        raise;
      end;
    finally
      UnLock;
    end;
  end;
begin
  BeginTransaction;
  if FQuickScript then
    for i := 0 to SQL.Count - 1 do
    begin
      ExecuteQuery(SQL.Strings[i], nil);
    end else
      ExecuteQuery(SQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TJvUIBQuery.EndExecImme(const ETM: TEndTransMode);
begin
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM);
end;

//******************************************************************************
// Prepare                                                                   *
//******************************************************************************

procedure TJvUIBQuery.BeginPrepare;
begin
  if (FStatement = nil) then BeginStatement;
  FSQLResult := ResultClass.Create(0, FCachedFetch, FFetchBlobs, FBufferChunks);
  Lock;
  try
    try
      DSQLPrepare(FTransaction.FTransaction, FStatement, FSQL.Text,
        FTransaction.FSQLDialect, FSQLResult);
      if FUseCursor then
      begin
        FCursorName := 'CURSOR'+inttostr(Integer(Self));
        DSQLSetCursorName(FStatement, FCursorName);
      end;
    except
      FreeAndNil(FSQLResult);
      EndStatement(FOnError);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsPrepare;
end;

procedure TJvUIBQuery.EndPrepare(const ETM: TEndTransMode);
begin
  FreeAndNil(FSQLResult);
  FCurrentState := qsStatement;
  EndStatement(ETM);
end;

//******************************************************************************
// Execute
//******************************************************************************

procedure TJvUIBQuery.BeginExecute;
begin
  if (FSQLResult = nil) then BeginPrepare;
  Lock;
  try
    try
      DSQLExecute(FTransaction.FTransaction, FStatement,
        FTransaction.FSQLDialect, FParameter);
    except
      if (FOnError <> etmStayIn) then
        EndPrepare(FOnError);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsExecute;
end;

procedure TJvUIBQuery.EndExecute(const ETM: TEndTransMode);
begin
  FEOF := True;
  FCurrentState := qsPrepare;
  EndPrepare(ETM);
end;

//******************************************************************************
// Other
//******************************************************************************

procedure TJvUIBQuery.Open;
begin
  Close(etmStayIn);
  Next;
end;

procedure TJvUIBQuery.Close(const Mode: TEndTransMode = etmStayIn);
begin
  case FCurrentState of
    qsStatement   : EndStatement(Mode);
    qsExecImme    : EndExecImme(Mode);
    qsPrepare     : EndPrepare(Mode);
    qsExecute     : EndExecute(Mode);
  end;
end;

procedure TJvUIBQuery.Next;
begin
  if (FCurrentState <> qsExecute) then
  begin
    Close;
    BeginExecute;
  end;
  Lock;
  try
    try
      if FSQLResult.FetchBlobs then
        FEOF := not DSQLFetchWithBlobs(FTransaction.FDataBase.FDbHandle,
          FTransaction.FTransaction, FStatement, FTransaction.FSQLDialect, FSQLResult) else
        FEOF := not DSQLFetch(FStatement, FTransaction.FSQLDialect, FSQLResult);
    except
      EndExecute(FOnError);
      raise;
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBQuery.FetchAll;
begin
  while not Eof do Next;
end;

procedure TJvUIBQuery.ExecSQL;
begin
  BeginExecImme;
end;

procedure TJvUIBQuery.Execute;
begin
  BeginExecute;
end;

function TJvUIBQuery.ParamsAddBlob(var Str: String): Word;
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  Result := Params.AddBlob;
  BlobHandle := nil;
  Lock;
  try
    Params.AsQuad[Result] := BlobCreate(FTransaction.FDataBase.FDbHandle,
      FTransaction.FTransaction, BlobHandle);
    try
      BlobWriteString(BlobHandle, Str);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

function TJvUIBQuery.ParamsAddBlob(Stream: TStream): Word;
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  Result := Params.AddBlob;
  BlobHandle := nil;
  Lock;
  try
    Params.AsQuad[Result] := BlobCreate(FTransaction.FDataBase.FDbHandle,
      FTransaction.FTransaction, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

function TJvUIBQuery.ParamsAddBlob(Buffer: Pointer; Size: Word): Word;
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  Result := Params.AddBlob;
  BlobHandle := nil;
  Lock;
  try
    Params.AsQuad[Result] := BlobCreate(FTransaction.FDataBase.FDbHandle,
      FTransaction.FTransaction, BlobHandle);
    try
      BlobWriteSegment(BlobHandle, Size, Buffer);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;


procedure TJvUIBQuery.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    Lock;
    try
      BlobHandle := nil;
      BlobOpen(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobSaveToStream(BlobHandle, Stream);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBQuery.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var str: string);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     str := '' else
  begin
    Lock;
    try
      BlobHandle := nil;
      BlobOpen(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobReadString(BlobHandle, str);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBQuery.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var Value: Variant);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    Lock;
    try
      BlobHandle := nil;
      BlobOpen(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
        BlobHandle, sqlda.AsQuad[Index]);
      try
        BlobReadVariant(BlobHandle, Value);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBQuery.ReadBlob(const Index: Word; var Str: string);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Str) else
    InternalReadBlob(Fields, Index, str);
end;

procedure TJvUIBQuery.ReadBlob(const Index: Word; Stream: TStream);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Stream) else
    InternalReadBlob(Fields, Index, Stream);
end;

procedure TJvUIBQuery.ReadBlob(const Index: Word; var Value: Variant);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Value) else
    InternalReadBlob(Fields, Index, Value);
end;

procedure TJvUIBQuery.ReadBlob(const name: string; Stream: TStream);
begin
  ReadBlob(Fields.GetFieldIndex(name), Stream);
end;

procedure TJvUIBQuery.ReadBlob(const name: string; var str: string);
begin
  ReadBlob(Fields.GetFieldIndex(name), str);
end;

procedure TJvUIBQuery.ReadBlob(const name: string; var Value: Variant);
begin
  ReadBlob(Fields.GetFieldIndex(name), Value);
end;

procedure TJvUIBQuery.ParamsReadBlob(const Index: Word; var Value: Variant);
begin
  InternalReadBlob(Params, Index, Value);
end;

procedure TJvUIBQuery.ParamsReadBlob(const Index: Word; var str: string);
begin
  InternalReadBlob(Params, Index, str);
end;

procedure TJvUIBQuery.ParamsReadBlob(const Index: Word; Stream: TStream);
begin
  InternalReadBlob(Params, Index, Stream);
end;

procedure TJvUIBQuery.ParamsSetBlob(const Index: Word; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  if (Params.FieldCount = Index) then
    ParamsAddBlob(Stream) else
  begin
    if (FCurrentState < qsTransaction) then
      BeginTransaction;
    BlobHandle := nil;
    Lock;
    try
      if Params.IsNull[Index] then
        Params.AsQuad[Index] := BlobCreate(FTransaction.FDataBase.FDbHandle,
          FTransaction.FTransaction, BlobHandle) else
        BlobOpen(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
          BlobHandle, Params.AsQuad[Index]);
      try
        BlobWriteStream(BlobHandle, Stream);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBQuery.ParamsSetBlob(const Index: Word; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (Params.FieldCount = Index) then
    ParamsAddBlob(str) else
  begin
    if (FCurrentState < qsTransaction) then
      BeginTransaction;
    BlobHandle := nil;
    Lock;
    try
      Params.AsQuad[Index] := BlobCreate(FTransaction.FDataBase.FDbHandle,
        FTransaction.FTransaction, BlobHandle);
      try
        BlobWriteString(BlobHandle, str);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBQuery.ParamsSetBlob(const Index: Word; Buffer: Pointer;
  Size: Word);
var BlobHandle: IscBlobHandle;
begin
  if (Params.FieldCount = Index) then
    ParamsAddBlob(Buffer, Size) else
  begin
    if (FCurrentState < qsTransaction) then
      BeginTransaction;
    BlobHandle := nil;
    Lock;
    try
      if Params.IsNull[Index] then
        Params.AsQuad[Index] := BlobCreate(FTransaction.FDataBase.FDbHandle,
          FTransaction.FTransaction, BlobHandle) else
        BlobOpen(FTransaction.FDataBase.FDbHandle, FTransaction.FTransaction,
          BlobHandle, Params.AsQuad[Index]);
      try
        BlobWriteSegment(BlobHandle, Size, Buffer);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

function TJvUIBQuery.ParamsClass: TSQLParamsClass;
begin
  Result := TSQLParams;
end;

function TJvUIBQuery.ResultClass: TSQLResultClass;
begin
  Result := TSQLResult;
end;

{ TJvUIBTransaction }

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBTransaction.Create;
{$ELSE}
constructor TJvUIBTransaction.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FOptions := [tpConcurrency,tpWait,tpWrite];
  FTransaction := nil;
  FStatements  := 0;
end;

destructor TJvUIBTransaction.Destroy;
begin
  ClearSQLComponents;
  SetDataBase(nil);
  inherited;
end;

{$IFNDEF UIBNOCOMPONENT}
procedure TJvUIBTransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FDataBase) and (Operation = opRemove)) then
    SetDataBase(nil);
end;
{$ENDIF}

procedure TJvUIBTransaction.SetDataBase(const Database: TJvUIBDataBase);
begin
  if (FDataBase <> Database) then
  begin
    if (FDataBase <> nil) then
    begin
      Close(etmCommit);
      FDataBase.RemoveTransaction(Self);
    end;
    FDataBase := Database;
    if (Database <> nil) then
    begin
      DataBase.AddTransaction(Self);
      FSQLDialect := Database.SQLDialect;
    end;
  end;
end;

procedure TJvUIBTransaction.Close(const Mode: TEndTransMode = etmStayIn);
var i: Integer;
begin
  if (FSQLComponent <> nil) then
    for i := 0 to FSQLComponent.Count -1 do
      TJvUIBQuery(FSQLComponent.Items[i]).Close(Mode);
  EndTransaction(Mode);
end;

procedure TJvUIBTransaction.BeginDataBase;
begin
  if (FDataBase = nil) then raise Exception.Create(EUIB_DATABASENOTDEF);
  FDataBase.Connected := True;
end;

procedure TJvUIBTransaction.BeginTransaction;
begin
  if not FDataBase.Connected then BeginDataBase;
  Lock;
  try
    if (FTransaction = nil) then
      TransactionStart(FTransaction, FDataBase.FDbHandle, TPB) else
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.EndTransaction(ETM: TEndTransMode);
begin
  Lock;
  try
    if (FTransaction <> nil) then
      try
         { If there is Statements alive I must keep handle.}
          if FStatements > 0 then
            case ETM of
              etmCommit   : ETM := etmCommitRetaining;
              etmRollback : ETM := etmRollbackRetaining;
            end;
          case ETM of
            etmCommit            : TransactionCommit(FTransaction);
            etmCommitRetaining   : TransactionCommitRetaining(FTransaction);
            etmRollback          : TransactionRollback(FTransaction);
            etmRollbackRetaining : TransactionRollbackRetaining(FTransaction);
          end;
      except
        case ETM of
          etmCommit          : TransactionRollback(FTransaction);
          etmCommitRetaining : TransactionRollbackRetaining(FTransaction);
        end;
        raise;
      end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.AddSQLComponent(Component: TJvUIBSQL);
begin
  if (FSQLComponent = nil) then FSQLComponent := TList.Create;
  FSQLComponent.Add(Component);
end;

procedure TJvUIBTransaction.ClearSQLComponents;
begin
  while (FSQLComponent <> nil) do
    TJvUIBQuery(FSQLComponent.Last).SetTransaction(nil);
end;

procedure TJvUIBTransaction.RemoveSQLComponent(Component: TJvUIBSQL);
begin
  if (FSQLComponent <> nil) then
  begin
    FSQLComponent.Remove(Component);
    if (FSQLComponent.Count = 0) then
      FreeAndNil(FSQLComponent);
  end;
end;

procedure TJvUIBTransaction.Lock;
begin
  inherited;
  if (FDataBase <> nil) then
    FDataBase.Lock;
end;

procedure TJvUIBTransaction.UnLock;
begin
  if (FDataBase <> nil) then
    FDataBase.UnLock;
  inherited;
end;

procedure TJvUIBTransaction.Commit;
begin
  EndTransaction(etmCommit);
end;

procedure TJvUIBTransaction.CommitRetaining;
begin
  EndTransaction(etmCommitRetaining);
end;

procedure TJvUIBTransaction.RollBack;
begin
  EndTransaction(etmRollback);
end;

procedure TJvUIBTransaction.RollBackRetaining;
begin
  EndTransaction(etmRollbackRetaining);
end;

function TJvUIBTransaction.GetInTransaction: Boolean;
begin
  Lock;
  try
    Result := (FTransaction <> nil);
  finally
    UnLock;
  end;
end;

//function TJvUIBTransaction.TPB: string;
//var
//  tp: TTransParam;
//begin
//  if FOptions = [tpConcurrency,tpWait,tpWrite] then
//    result := ''
//  else
//    begin
//      Result := isc_tpb_version3;
//      for tp := low(TTransParam) to High(TTransParam) do
//        if (tp in FOptions) then
//        begin
//          Result := Result + Char(Ord(tp)+1);
//          case tp of
//            tpLockRead : Result := Result + Char(Length(FLockRead)) + FLockRead;
//            tpLockWrite: Result := Result + Char(Length(FLockWrite)) + FLockWrite;
//          end;
//        end;
//    end;
//end;

function TJvUIBTransaction.TPB: string;
var
  tp: TTransParam;
procedure ParseStrOption(const code: Char; const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  P := Pointer(Value);
  if P <> nil then
    while P^ <> #0 do
    begin
      Start := P;
      while not (P^ in [#0, ';']) do Inc(P);
      if (P - Start) > 0 then
      begin
        SetString(S, Start, P - Start);
        Result := Result + code + Char(P - Start) + S;
      end;
      if P^ =';' then inc(P);
    end;
end;
begin
  if FOptions = [tpConcurrency,tpWait,tpWrite] then
    result := ''
  else
    begin
      Result := isc_tpb_version3;
      for tp := Low(TTransParam) to High(TTransParam) do
        if (tp in FOptions) then
        begin
          case tp of
            tpLockRead : ParseStrOption(Char(Ord(tp)+1), FLockRead);
            tpLockWrite: ParseStrOption(Char(Ord(tp)+1), FLockWrite);
          else
            Result := Result + Char(Ord(tp)+1);
          end;
        end;
    end;
end;


function TJvUIBTransaction.GetOptions: TTransParams;
begin
  Lock;
  try
    Result := FOptions;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.SetOptions(const Value: TTransParams);
begin
  Lock;
  try
    FOptions := Value;
  finally
    UnLock;
  end;
end;

function TJvUIBTransaction.GetLockRead: string;
begin
  Lock;
  try
    Result := FLockRead;
  finally
    UnLock;
  end;
end;

function TJvUIBTransaction.GetLockWrite: string;
begin
  Lock;
  try
    Result := FLockWrite;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.SetLockRead(const Value: string);
begin
  Lock;
  try
    FLockRead := Value;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.SetLockWrite(const Value: string);
begin
  Lock;
  try
    FLockWrite := Value;
  finally
    UnLock;
  end;
end;

function TJvUIBTransaction.GetDataBase: TJvUIBDataBase;
begin
  Lock;
  try
    Result := FDataBase;
  finally
    UnLock;
  end;
end;

{ TJvUIBComponent }

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBComponent.Create;
begin
{$ELSE}
constructor TJvUIBComponent.Create(AOwner: TComponent);
begin
  inherited;
{$ENDIF}
  FCriticalsection := TCriticalSection.Create;
end;

destructor TJvUIBComponent.Destroy;
begin
  FCriticalsection.Free;
  inherited Destroy;
end;

procedure TJvUIBComponent.Lock;
begin
{$IFDEF UIBTHREADSAFE}
  FCriticalsection.Enter;
{$ENDIF}
end;

procedure TJvUIBComponent.UnLock;
begin
{$IFDEF UIBTHREADSAFE}
  FCriticalsection.Leave;
{$ENDIF}
end;

{ TJvUIBService }

procedure TJvUIBService.BeginService;
begin
  case FProtocol of
    proLocalHost : ServiceAttach('service_mgr', FHandle, CreateSPB);
    proTCPIP     : ServiceAttach(Fhost + ':service_mgr', FHandle, CreateSPB);
    proNetBUI    : ServiceAttach('\\'+ Fhost + '\service_mgr', FHandle, CreateSPB);
  end;
end;

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBService.Create;
{$ELSE}
constructor TJvUIBService.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FProtocol := proLocalHost;
  FHandle := nil;
end;

function TJvUIBService.CreateSPB: string;
procedure AddString(id: char; var Str: string);
begin
  if (Str <> '') then
    Result := Result + id + Char(length(Str)) + Str;
end;
begin
  Result := isc_spb_version + isc_spb_current_version;
  AddString(isc_spb_user_name, FUserName);
  AddString(isc_spb_password, FPassWord);
end;

procedure TJvUIBService.EndService;
begin
  ServiceDetach(FHandle);
end;

{ TJvUIBBackupRestore }

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBBackupRestore.Create;
{$ELSE}
constructor TJvUIBBackupRestore.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FBackupFiles := TStringList.Create;
end;

destructor TJvUIBBackupRestore.Destroy;
begin
  FBackupFiles.Free;
  inherited;
end;

procedure TJvUIBBackupRestore.SetBackupFiles(const Value: TStrings);
begin
  FBackupFiles.Assign(Value);
end;

procedure TJvUIBBackupRestore.Run;
var
  Buffer: string;
  Len: Word;
begin
  BeginService;
  try
    ServiceStart(FHandle, CreateStartSPB);
    if Assigned(FOnVerbose) then
    begin
      SetLength(Buffer, 1024);
      while true do
      begin
        ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
        if (Buffer[1] <> isc_info_svc_line) then
          raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
        Len := PWord(@Buffer[2])^;
        if len > 0 then
          FOnVerbose(self, copy(Buffer, 4, len)) else
          Break;
      end;
    end;
  finally
    EndService;
  end;
end;

{ TJvUIBBackup }

function TJvUIBBackup.CreateStartSPB: string;
var
  Len: Word;
  i: Integer;
  FileName: string;
  FileLength: Integer;
  function GetValue(Index: Integer): string;
  begin
    if Index >= 0 then
      Result := Copy(FBackupFiles.Strings[Index], Length(FBackupFiles.Names[Index]) + 2, MaxInt) else
      Result := '';
  end;
begin
  // backup service   ibservices
  Result := isc_action_svc_backup;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := FBackupFiles.Names[i];
    if FileName = '' then
      FileName := FBackupFiles[i];
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
      Result := Result + FileName;
      // Backup file length
      if TryStrToInt(GetValue(i), FileLength) then
      begin
        Result := Result + isc_spb_bkp_length;
        Result := Result + PChar(@FileLength)[0] + PChar(@FileLength)[1] +
          PChar(@FileLength)[2] + PChar(@FileLength)[3];
      end;
    end;
  end;

  if assigned(FOnVerbose) then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
    Result := Result + isc_spb_options + PChar(@FOptions)^ + #0#0#0;
end;

{ TJvUIBRestore }

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBRestore.Create;
{$ELSE}
constructor TJvUIBRestore.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FOptions := [roCreateNewDB];
  FPageSize := 0;
end;

function TJvUIBRestore.CreateStartSPB: string;
var
  Len: Word;
  i: Integer;
  FileName: string;
  Opts: Cardinal;
begin
  // backup service   ibservices
  Result := isc_action_svc_restore;

  for i := 0 to FBackupFiles.Count - 1 do
  begin
    FileName := FBackupFiles[i];
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
      Result := Result + FileName;
    end;
  end;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  if assigned(FOnVerbose) then
    Result := Result + isc_spb_verbose;

  if (FOptions <> []) then
  begin
    Opts := PByte(@FOptions)^ shl 8;
    Result := Result + isc_spb_options + PChar(@Opts)[0] +
      PChar(@Opts)[1] + PChar(@Opts)[2] + PChar(@Opts)[3];
  end;

  if FPageSize > 0 then
    Result := Result + isc_spb_res_page_size + PChar(@FPageSize)[0] +
      PChar(@FPageSize)[1] + PChar(@FPageSize)[2] + PChar(@FPageSize)[3];
end;

end.
