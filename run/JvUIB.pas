{******************************************************************************}
{                                                                              }
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
{ Last modified: September 21, 2003                                            }
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
  {$IFDEF USEJVCL} JvComponent, {$ENDIF}
  Classes, SysUtils, SyncObjs, JvUIBLib, JvUIBase, JvUIBSQLParser;

type

 {Oo.........................................................................oO
                              TJvUIBComponent

      Synchronise Databases, Transactions and Queries.

    TUIBLibrary    | TUIBDatabase     | TUIBTransaction  | TUIBQuery
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
  {$IFDEF USEJVCL}
  TJvUIBComponent = class(TJvComponent)
  {$ELSE}
  TJvUIBComponent = class(TComponent)
  {$ENDIF}
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
  TJvUIBStatement = class;

{-----------------------------------------------------------------------------
  Class: TJvUIBDataBase
  Manage a Database connection.
-----------------------------------------------------------------------------}
  TJvUIBDataBase = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLiBraryName: string;
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
    procedure SetLibraryName(const Lib: string);
    function GetTransactions(const Index: Cardinal): TJvUIBTransaction;
    function GetTransactionsCount: Cardinal;
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure ExecuteImmediate(const Statement: string);
    procedure CreateDatabase(PageSize: Integer = 4096);
    property Handle: IscDbHandle read FDbHandle write SetHandle;
    property IsHandleShared : Boolean read FHandleShared;
    property Transactions[const Index: Cardinal]: TJvUIBTransaction read GetTransactions;
    property TransactionsCount: Cardinal read GetTransactionsCount;
    property Lib: TUIBLibrary read FLibrary;
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
    { Define wich library the connection use.}
    property LibraryName: string read FLiBraryName write SetLibraryName;
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
    qsExecImme,    // Query executed immediately without the need of statement handle
    qsStatement,   // have a statement handle
    qsPrepare,     // Query prepared
    qsExecute      // Query executed 
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
  TOnEndTransaction = procedure(Sender: TObject; var Mode: TEndTransMode) of object;

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
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TOnEndTransaction;
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
    procedure AddSQLComponent(Component: TJvUIBStatement);
    procedure RemoveSQLComponent(Component: TJvUIBStatement);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode = etmStayIn);
    function GetStatements(const Index: Cardinal): TJvUIBStatement;
    function GetStatementsCount: Cardinal;
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
    {Commit transaction.}
    procedure Commit;
    {Commit transaction but keep transaction handle.}
    procedure CommitRetaining;
    {Rollback transaction.}
    procedure RollBack;
    {Rollback transaction but keep transaction handle.}
    procedure RollBackRetaining;
    {Indicate if the transaction is active.}
{$IFDEF IB71_UP}
    procedure SavepointRelease(const Name: string);
    procedure SavepointRollback(const Name: string; Option: Word = 0);
    procedure SavepointStart(const Name: string);
{$ENDIF}
    property InTransaction: Boolean read GetInTransaction;
    {Transaction handle.}
    property Handle: IscTrHandle read FTransaction;
    property Statements[const Index: Cardinal]: TJvUIBStatement read GetStatements;
    property StatementsCount: Cardinal read GetStatementsCount;
  published
    {Database connection.}
    property DataBase  : TJvUIBDataBase read GetDataBase write SetDataBase;
    {Transaction parametters.}
    property Options   : TTransParams   read GetOptions    write SetOptions default [tpConcurrency,tpWait,tpWrite];
    {List of the tables to lock for read, tpLockRead option must set. ex: 'Table1;Table2'}
    property LockRead  : string         read GetLockRead   write SetLockRead;
    {List of the tables to lock for write, tpLockWrite option must set. ex: 'Table1;Table2'}
    property LockWrite : string         read GetLockWrite  write SetLockWrite;
    {This event occur after a transaction is started.}
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction write FOnStartTransaction;
    {This evenet occur before to end the transaction, you can change the ETM parametter.}
    property OnEndTransaction: TOnEndTransaction read FOnEndTransaction write FOnEndTransaction;
  end;

  TJvUIBStatement = class(TJvUIBComponent)
  private
    FCurrentState: TQueryState;
    FTransaction: TJvUIBTransaction;
    FStatement: IscStmtHandle;
    FOnError: TEndTransMode;
    FCursorName: string;
    FSQLResult: TSQLResult;
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FBufferChunks: Cardinal;
    FQuickScript: boolean;
    FSQL: TStrings;
    FParsedSQL: string;
    FParameter: TSQLParams;
    FParseParams: boolean;
    FOnClose: TNotifyEvent;
    function GetPlan: string;
    function GetStatementType: TUIBStatementType;
    procedure SetSQL(const Value: TStrings);
    procedure DoSQLChange(Sender: TObject);
    function GetFields: TSQLResult;
    function GetEof: boolean;
  protected
    procedure SetTransaction(const Transaction: TJvUIBTransaction); virtual;
{$IFNDEF UIBNOCOMPONENT}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}

    procedure BeginTransaction; virtual;
    procedure BeginStatement; virtual;
    procedure BeginPrepare; virtual;
    procedure BeginExecute; virtual;
    procedure BeginExecImme; virtual;

    procedure EndTransaction(const ETM: TEndTransMode); virtual;
    procedure EndStatement(const ETM: TEndTransMode); virtual;
    procedure EndPrepare(const ETM: TEndTransMode); virtual;
    procedure EndExecute(const ETM: TEndTransMode); virtual;
    procedure EndExecImme(const ETM: TEndTransMode); virtual;

    procedure InternalNext; virtual;
    procedure InternalPrior; virtual;

    function  ParamsClass: TSQLParamsClass; virtual;
    function  ResultClass: TSQLResultClass; virtual;

    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var Value: Variant); overload;

    property QuickScript: boolean read FQuickScript write FQuickScript  default False;

  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
    procedure Lock; override;
    procedure UnLock; override;

    procedure Close(const Mode: TEndTransMode = etmStayIn); virtual;
    procedure FetchAll;
    procedure Open;
    procedure Prepare;
    procedure Execute;
    procedure ExecSQL;
    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlob(const Index: Word; var str: string); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    procedure ReadBlob(const name: string; var str: string); overload;
    procedure ReadBlob(const name: string; var Value: Variant); overload;

    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;

    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    procedure ParamsSetBlob(const Name: string; var str: string); overload;
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word); overload;

    property Fields: TSQLResult read GetFields;
    property Params: TSQLParams read FParameter;
    property CursorName: string read FCursorName;
    property CurrentState: TQueryState read FCurrentState;
    property Eof: boolean read GetEof;
    property ParseParams: boolean read FParseParams write FParseParams;

    property Plan: string read GetPlan;
    property StatementType: TUIBStatementType read GetStatementType;

  published
    property SQL: TStrings read FSQL write SetSQL;
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    property CachedFetch: boolean read FCachedFetch write FCachedFetch default True;
    property FetchBlobs: boolean read FFetchBlobs write FFetchBlobs default False;
    property BufferChunks: Cardinal read FBufferChunks write FBufferChunks default 1000;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  {Oo.......................................................................oO
                                  TUIBQuery
   Oo.......................................................................oO}
  TJvUIBQuery = class(TJvUIBStatement)
  public
    procedure BuildStoredProc(const StoredProc: string);
  published
    property QuickScript;
  end;

  TOnParse = procedure(Sender: TObject; NodeType: TSQLNodeType;
    const Statement: string; Position, Count: Integer) of object;

  TJvUIBScript = class(TJvUIBComponent)
  private
    FQuery: TJvUIBQuery;
    FScript: TStrings;
    FAutoDDL: boolean;
    FOnParse: TOnParse;
    procedure SetTransaction(const Value: TJvUIBTransaction);
    function GetTransaction: TJvUIBTransaction;
    procedure SetScript(const Value: TStrings);
  public
  {$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
  {$ELSE}
    constructor Create(AOwner: TComponent); override;
  {$ENDIF}
    destructor Destroy; override;
    procedure ExecuteScript;
  published
    property Transaction: TJvUIBTransaction read GetTransaction write SetTransaction;
    property Script: TStrings read FScript write SetScript;
    property AutoDDL: boolean read FAutoDDL write FAutoDDL default True;
    property OnParse: TOnParse read FOnParse write FOnParse;
  end;

  TUIBProtocol = (
    proLocalHost,
    proTCPIP,
    proNetBUI
  );

  TJvUIBService = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLiBraryName: string;
    FUserName: string;
    FPassWord: string;
    FHost    : string;
    FProtocol: TUIBProtocol;
    FHandle  : IscSvcHandle;
    procedure BeginService;
    procedure EndService;
    function CreateSPB: string; virtual;
    procedure SetLibraryName(const Lib: String);
  public
{$IFDEF UIBNOCOMPONENT}
    constructor Create; override;
{$ELSE}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
    destructor Destroy; override;
  published
    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property Host: string read FHost write FHost;
    property Protocol: TUIBProtocol read FProtocol write FProtocol default proLocalHost;
    { Define wich library the connection use.}
    property LibraryName: string read FLiBraryName write SetLibraryName;
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
    roOneRelationAtATime, roReplace, roCreateNewDB, roUseAllSpace
    {$IFDEF IB71_UP},roValidate{$ENDIF});

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
{$IFDEF IB71_UP}
    ,'ISO8859_15', 'KOI8R'
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
  FLibrary := TUIBLibrary.Create;
  FLiBraryName := GDS32DLL;
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
    FLibrary.Free;
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

procedure TJvUIBDataBase.ExecuteImmediate(const Statement: string);
begin
  FLibrary.Load(FLiBraryName);
  FLibrary.DSQLExecuteImmediate(Statement, SQLDialect);
end;

procedure TJvUIBDataBase.CreateDatabase(PageSize: Integer = 4096);
var TrHandle: IscTrHandle;
const
  CreateDb = 'CREATE DATABASE ''%s'' USER ''%s'' PASSWORD ''%s'' '+
    'PAGE_SIZE %d DEFAULT CHARACTER SET %s';
begin
  TrHandle := nil;
  Connected := False;
  FLibrary.Load(FLiBraryName);
  FLibrary.DSQLExecuteImmediate(FDbHandle, TrHandle,
    Format(CreateDb, [DatabaseName, UserName, PassWord, PageSize,
    CharacterSetStr[CharacterSet]]), SQLDialect);
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
    with FLibrary do
    case Value of
      True  :
        begin
          if Assigned(BeforeConnect) then BeforeConnect(Self);
          FLibrary.Load(FLiBraryName);
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
  if (FDbHandle = nil) or ((FDbHandle <> nil) and FHandleShared) then
  begin
    FLibrary.Load(FLiBraryName);
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end else
    raise Exception.Create(EUIB_DBHANDLEALLREADYSET);
end;

procedure TJvUIBDataBase.SetLibraryName(const Lib: string);
begin
  SetConnected(False);
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

function TJvUIBDataBase.GetTransactions(const Index: Cardinal): TJvUIBTransaction;
begin
  if FTransactions <> nil then
    Result := FTransactions.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TJvUIBDataBase.GetTransactionsCount: Cardinal;
begin
  if FTransactions <> nil then
    Result := FTransactions.Count else
    Result := 0;
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

{ TJvUIBStatement }

procedure TJvUIBStatement.SetTransaction(const Transaction: TJvUIBTransaction);
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

procedure TJvUIBStatement.BeginTransaction;
begin
  if FTransaction <> nil then
    FTransaction.BeginTransaction else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  FCurrentState := qsTransaction;
end;

procedure TJvUIBStatement.Close(const Mode: TEndTransMode);
begin
  case FCurrentState of
    qsStatement   : EndStatement(Mode);
    qsExecImme    : EndExecImme(Mode);
    qsPrepare     : EndPrepare(Mode);
    qsExecute     : EndExecute(Mode);
  end;
end;

procedure TJvUIBStatement.Open;
begin
  // if you reopen the same query I Close
  // the cursor, clean sql result and
  // execute the query again to save
  // the prepare time !
  if (FCurrentState = qsExecute) then
  begin
    Lock;
    try
      try
        FSQLResult.ClearRecords;
        with FTransaction.FDataBase.FLibrary do
          DSQLFreeStatement(FStatement, DSQL_close);
      except
        Close(FOnError);
        raise;
      end;
      BeginExecute;
    finally
      UnLock;
    end;
  end else
    Close(etmStayIn);
  InternalNext;
end;

procedure TJvUIBStatement.Next;
begin
  InternalNext;
end;

procedure TJvUIBStatement.Prior;
begin
  InternalPrior;
end;

procedure TJvUIBStatement.Last;
begin

  FetchAll;
end;

procedure TJvUIBStatement.First;
begin
  if (FSQLResult <> nil) and
   (FSQLResult.RecordCount > 0) and
    (FSQLResult.CurrentRecord <> 0) then
   FSQLResult.CurrentRecord := 0;
end;

procedure TJvUIBStatement.FetchAll;
begin
  while not Eof do Next;
end;

procedure TJvUIBStatement.Execute;
begin
  BeginExecute;
end;

procedure TJvUIBStatement.ExecSQL;
begin
  BeginExecImme;
end;

procedure TJvUIBStatement.Prepare;
begin
  if (FCurrentState < qsPrepare) then
  BeginPrepare
end;

procedure TJvUIBStatement.InternalNext;
begin
  if (FCurrentState < qsExecute) then
    BeginExecute;
  if ((Fields.CurrentRecord + 1) < Fields.RecordCount) then
  begin
    Fields.CurrentRecord := Fields.CurrentRecord + 1;
  end else
  begin
    Lock;
    try
      with FTransaction.FDataBase.FLibrary do
      try
        if FSQLResult.FetchBlobs then
          DSQLFetchWithBlobs(FTransaction.FDataBase.FDbHandle,
            FTransaction.FTransaction, FStatement, FTransaction.FSQLDialect, FSQLResult) else
          DSQLFetch(FStatement, FTransaction.FSQLDialect, FSQLResult);
      except
        if FOnError <> etmStayIn then
          EndExecute(FOnError);
        raise;
      end;
    finally
      UnLock;
    end;
  end;
  if eof then
    beep;
end;

procedure TJvUIBStatement.InternalPrior;
begin
  if Fields.CachedFetch then
  begin
    if Fields.CurrentRecord > 0 then
      Fields.CurrentRecord := Fields.CurrentRecord - 1;
  end else
    raise EUIBError.Create('CachedFetch property not set to True.');
end;

procedure TJvUIBStatement.EndTransaction(const ETM: TEndTransMode);
begin
  if FTransaction <> nil then
    FTransaction.EndTransaction(ETM) else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  if (ETM in [etmCommit, etmRollback]) then
     FCurrentState := qsDataBase;
end;

procedure TJvUIBStatement.BeginStatement;
begin
  BeginTransaction;
  Lock;
  try
    with FTransaction.FDataBase.FLibrary do
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

procedure TJvUIBStatement.EndStatement(const ETM: TEndTransMode);
begin
  Lock;
  try
    with FTransaction.FDataBase.FLibrary do
      DSQLFreeStatement(FStatement, DSQL_drop);

    FStatement := nil;
    Dec(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM);

  if Assigned(FOnClose) then
    FOnClose(Self);    
end;

procedure TJvUIBStatement.BeginPrepare;
begin
  if (FStatement = nil) then BeginStatement;
  FSQLResult := ResultClass.Create(0, FCachedFetch, FFetchBlobs, FBufferChunks);
  Lock;
  try
    with FTransaction.FDataBase.FLibrary do
    try
    if (FQuickScript or (not FParseParams)) then
      DSQLPrepare(FTransaction.FTransaction, FStatement, FSQL.Text,
        FTransaction.FSQLDialect, FSQLResult) else
      DSQLPrepare(FTransaction.FTransaction, FStatement, FParsedSQL,
        FTransaction.FSQLDialect, FSQLResult);
      FCursorName := 'C'+inttostr(Integer(FStatement));
      DSQLSetCursorName(FStatement, FCursorName);
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

procedure TJvUIBStatement.EndPrepare(const ETM: TEndTransMode);
begin
  FreeAndNil(FSQLResult);
  FCurrentState := qsStatement;
  EndStatement(ETM);
end;

procedure TJvUIBStatement.BeginExecute;
begin
  if (FSQLResult = nil) then BeginPrepare;
  Lock;
  try
    with FTransaction.FDataBase.FLibrary do
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

procedure TJvUIBStatement.EndExecute(const ETM: TEndTransMode);
begin
  FCurrentState := qsPrepare;
  EndPrepare(ETM);
end;

procedure TJvUIBStatement.BeginExecImme;
var
  I: Integer;
  procedure ExecuteQuery(const AQuery: String; Params: TSQLParams);
  begin
    if (Trim(AQuery) = '') then exit;
    Lock;
    try
      with FTransaction.FDataBase.FLibrary do
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
    for i := 0 to FSQL.Count - 1 do
    begin
      ExecuteQuery(FSQL.Strings[i], nil);
    end else
      if FParseParams then
        ExecuteQuery(FParsedSQL, FParameter) else
        ExecuteQuery(FSQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TJvUIBStatement.EndExecImme(const ETM: TEndTransMode);
begin
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
    EndTransaction(ETM);
end;

function TJvUIBStatement.ParamsClass: TSQLParamsClass;
begin
  Result := TSQLParams;
end;

function TJvUIBStatement.ResultClass: TSQLResultClass;
begin
  Result := TSQLResult;
end;

procedure TJvUIBStatement.Lock;
begin
  inherited;
    Ftransaction.Lock;
end;

procedure TJvUIBStatement.UnLock;
begin
    Ftransaction.UnLock;
  inherited;
end;

procedure TJvUIBStatement.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

function TJvUIBStatement.GetPlan: string;
begin
  Lock;
  try
    if (FCurrentState < qsPrepare) then
      Raise EUIBError.Create(EUIB_MUSTBEPREPARED)else
      Result := FTransaction.FDataBase.FLibrary.DSQLInfoPlan(FStatement);
  finally
    UnLock
  end;
end;

function TJvUIBStatement.GetStatementType: TUIBStatementType;
begin
  Result := TUIBStatementType(-1); // invalid
  Lock;
  try
    if (FCurrentState < qsPrepare) then
      Raise EUIBError.Create(EUIB_MUSTBEPREPARED)else
      Result := FTransaction.FDataBase.FLibrary.DSQLInfoStatementType(FStatement);
  finally
    UnLock
  end;
end;

procedure TJvUIBStatement.DoSQLChange(Sender: TObject);
begin
  Close(etmStayIn);
  if (not FQuickScript or FParseParams) then
    FParsedSQL := FParameter.Parse(FSQL.Text);
end;

function TJvUIBStatement.GetFields: TSQLResult;
begin
  if (FSQLResult = nil) then
    raise EUIBError.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

function TJvUIBStatement.GetEof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Eof else
    Result := True;
end;

{$IFNDEF UIBNOCOMPONENT}
procedure TJvUIBStatement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
end;
{$ENDIF}

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBStatement.Create;
{$ELSE}
constructor TJvUIBStatement.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FCurrentState := qsDataBase;
{$IFNDEF UIBNOCOMPONENT}
  if (AOwner is TJvUIBTransaction) then
    Transaction := TJvUIBTransaction(AOwner) else
{$ENDIF}
    FTransaction := nil;
  FSQL         := TStringList.Create;
  TStringList(FSQL).OnChange := DoSQLChange;
  FCachedFetch := True;
  FetchBlobs   := False;
  FQuickScript := False;
  FOnError     := etmRollback;
  FParameter   := ParamsClass.Create;
  FCursorName  := '';
  FBufferChunks := 1000;
  FParseParams := True;
end;

destructor TJvUIBStatement.Destroy;
begin
  FSQL.Free;
  FreeAndNil(FParameter);
  SetTransaction(nil);
  inherited;
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Str: string);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Str) else
    InternalReadBlob(Fields, Index, str);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Stream: TStream);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Stream) else
    InternalReadBlob(Fields, Index, Stream);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Value: Variant);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Value) else
    InternalReadBlob(Fields, Index, Value);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; Stream: TStream);
begin
  ReadBlob(Fields.GetFieldIndex(name), Stream);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; var str: string);
begin
  ReadBlob(Fields.GetFieldIndex(name), str);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; var Value: Variant);
begin
  ReadBlob(Fields.GetFieldIndex(name), Value);
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FTransaction.FDataBase.FDbHandle,
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

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
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

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Buffer: Pointer;
  Size: Word);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FTransaction.FDataBase.FDbHandle,
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

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; Stream: TStream);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FTransaction.FDataBase.FDbHandle,
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

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FTransaction.FDataBase.FDbHandle,
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

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FTransaction.FDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FTransaction.FDataBase.FDbHandle,
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

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    Lock;
    with FTransaction.FDataBase.FLibrary do
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

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
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
    with FTransaction.FDataBase.FLibrary do
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

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA; const Index: Word;
  var Value: Variant);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    Lock;
    with FTransaction.FDataBase.FLibrary do
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

{ TJvUIBQuery }

procedure TJvUIBQuery.BuildStoredProc(const StoredProc: string);
var
  i, r: Integer;
  Str: string;
begin
  Close;
  r := 0;
  TStringList(FSQL).OnChange := nil;
  try
    Params.Clear;
    FParsedSQL :=
      'SELECT RDB$FIELD_TYPE, RDB$PARAMETER_NAME, RDB$FIELD_SCALE, RDB$PARAMETER_TYPE '+
       'FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON '+
       'PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME '+
      'WHERE '+
          'PRM.RDB$PROCEDURE_NAME = ''' + UpperCase(StoredProc) + ''' '+
      'ORDER BY RDB$PARAMETER_TYPE, PRM.RDB$PARAMETER_NUMBER';
    Open;
    try
      while not Eof do
      begin
        with Fields do
        if AsSmallint[3] = 0 then
        begin
          if AsSmallint[2] < 0 then
          begin
            case Fields.AsSmallint[0] of
              blr_short:  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 4);
              blr_long :  Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 7);
              blr_int64,
              blr_quad,
              blr_double: Params.AddFieldType(Trim(AsString[1]), uftNumeric, - AsSmallint[2], 15);
            else
              Raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
            end;
          end else
          case Fields.AsSmallint[0] of
            blr_text,
            blr_text2,
            blr_varying,
            blr_varying2,
            blr_cstring,
            blr_cstring2  : Params.AddFieldType(Trim(AsString[1]), uftChar);
            blr_float,
            blr_d_float   : Params.AddFieldType(Trim(AsString[1]), uftFloat);
            blr_short     : Params.AddFieldType(Trim(AsString[1]), uftSmallint);
            blr_long      : Params.AddFieldType(Trim(AsString[1]), uftInteger);
            blr_quad      : Params.AddFieldType(Trim(AsString[1]), uftQuad);
            blr_double    : Params.AddFieldType(Trim(AsString[1]), uftDoublePrecision);
            blr_timestamp : Params.AddFieldType(Trim(AsString[1]), uftTimestamp);
            blr_blob,
            blr_blob_id   : Params.AddFieldType(Trim(AsString[1]), uftBlob);
            blr_sql_date  : Params.AddFieldType(Trim(AsString[1]), uftDate);
            blr_sql_time  : Params.AddFieldType(Trim(AsString[1]), uftTime);
            blr_int64     : Params.AddFieldType(Trim(AsString[1]), uftInt64);
          {$IFDEF IB7_UP}
            blr_boolean_dtype : Params.AddFieldType(Trim(AsString[1]), uftBoolean);
          {$ENDIF}
          else
            // shouldn't occur but ...
            raise EUIBError.Create('Unknow field type.');
          end
        end else
          inc(r);
        Next;
      end;
      if (Params.FieldCount > 0) then
      begin
        FParsedSQL := ' (';
        Str        := ' (';
        for i := 0 to Params.FieldCount - 1 do
        begin
          FParsedSQL := FParsedSQL + '?,';
          Str        := Str        + ':'+ Params.FieldName[i] +','
        end;
        FParsedSQL[Length(FParsedSQL)] := ')';
        Str[Length(Str)] := ')';
        if r > 0 then
          begin
            FParsedSQL := 'SELECT * FROM ' + StoredProc + FParsedSQL;
            FSQL.Text  := 'SELECT * FROM ' + StoredProc + Str;
          end else
          begin
            FParsedSQL := 'EXECUTE PROCEDURE ' + StoredProc + FParsedSQL;
            FSQL.Text  := 'EXECUTE PROCEDURE ' + StoredProc + Str;
          end;
      end else
        begin
          if r > 0 then
            FParsedSQL := 'SELECT * FROM ' + StoredProc else
            FParsedSQL := 'EXECUTE PROCEDURE ' + StoredProc;
          FSQL.Text := FParsedSQL;
        end;
    except
      FParsedSQL := '';
      Params.Clear;
      Close(FOnError);
      raise;
    end;
  finally
    Close;
    TStringList(FSQL).OnChange := DoSQLChange;
  end;
end;

{ TJvUIBTransaction }

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBTransaction.Create;
{$ELSE}
constructor TJvUIBTransaction.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FOptions       := [tpConcurrency,tpWait,tpWrite];
  FTransaction   := nil;
  FStatements    := 0;
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

function TJvUIBTransaction.GetStatements(const Index: Cardinal): TJvUIBStatement;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TJvUIBTransaction.GetStatementsCount: Cardinal;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Count else
    Result := 0;
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
    with FDataBase.FLibrary do
    if (FTransaction = nil) then
    begin
      TransactionStart(FTransaction, FDataBase.FDbHandle, TPB);
      if Assigned(FOnStartTransaction) then
        FOnStartTransaction(Self);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.EndTransaction(ETM: TEndTransMode);
begin
  Lock;
  try
    if (FTransaction <> nil) then
      with FDataBase.FLibrary do
      try
        if Assigned(FOnEndTransaction) then
          FOnEndTransaction(Self, ETM);
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

procedure TJvUIBTransaction.AddSQLComponent(Component: TJvUIBStatement);
begin
  if (FSQLComponent = nil) then FSQLComponent := TList.Create;
  FSQLComponent.Add(Component);
end;

procedure TJvUIBTransaction.ClearSQLComponents;
begin
  while (FSQLComponent <> nil) do
    TJvUIBQuery(FSQLComponent.Last).SetTransaction(nil);
end;

procedure TJvUIBTransaction.RemoveSQLComponent(Component: TJvUIBStatement);
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

{$IFDEF IB71_UP}
procedure TJvUIBTransaction.SavepointRelease(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRelease(FTransaction, Name);
end;

procedure TJvUIBTransaction.SavepointRollback(const Name: string; Option: Word = 0);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRollback(FTransaction, Name, Option);
end;

procedure TJvUIBTransaction.SavepointStart(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointStart(FTransaction, Name);
end;
{$ENDIF}

function TJvUIBTransaction.GetInTransaction: Boolean;
begin
  Lock;
  try
    Result := (FTransaction <> nil);
  finally
    UnLock;
  end;
end;

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
  FLibrary.Load(FLiBraryName);
  case FProtocol of
    proLocalHost : FLibrary.ServiceAttach('service_mgr', FHandle, CreateSPB);
    proTCPIP     : FLibrary.ServiceAttach(Fhost + ':service_mgr', FHandle, CreateSPB);
    proNetBUI    : FLibrary.ServiceAttach('\\'+ Fhost + '\service_mgr', FHandle, CreateSPB);
  end;
end;

{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBService.Create;
{$ELSE}
constructor TJvUIBService.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FLibrary := TUIBLibrary.Create;
  FLiBraryName := GDS32DLL;
  FProtocol := proLocalHost;
  FHandle := nil;
end;

destructor TJvUIBService.Destroy;
begin
  inherited;
  FLibrary.Free;
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

procedure TJvUIBService.SetLibraryName(const Lib: String);
begin
  FLibrary.UnLoad;
  FLiBraryName := Lib;
end;

procedure TJvUIBService.EndService;
begin
  FLibrary.ServiceDetach(FHandle);
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
    FLibrary.ServiceStart(FHandle, CreateStartSPB);
    if Assigned(FOnVerbose) then
    begin
      SetLength(Buffer, 1024);
      while true do
      begin
        FLibrary.ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
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


{ TJvUIBScript }
{$IFDEF UIBNOCOMPONENT}
constructor TJvUIBScript.Create;
{$ELSE}
constructor TJvUIBScript.Create(AOwner: TComponent);
{$ENDIF}
begin
  inherited;
  FQuery := TJvUIBQuery.Create(nil);
  FQuery.ParseParams := False;
  FScript := TStringList.Create;
  FAutoDDL := True;
end;

destructor TJvUIBScript.Destroy;
begin
  FQuery.Free;
  FScript.Free;
  inherited;
end;

procedure TJvUIBScript.ExecuteScript;
var
  FStream: TStringStream;
  Lexer: TUIBLexer;
  Grammar: TUIBGrammar;
  i, k: Integer;
  j: TCharacterSet;
  Dialect: Integer;
  TrHandle: IscTrHandle;

  procedure CheckDatabase;
  begin
    if (Transaction = nil) then
       raise EUIBError.Create(EUIB_TRANSACTIONNOTDEF);
    if (Transaction.DataBase = nil) then
       raise EUIBError.Create(EUIB_DATABASENOTDEF);
  end;

  function Statement: string;
  var p: Integer;
  begin
    with Grammar.RootNode.Nodes[i] do
    begin
      p := PosFrom.Pos;
      FStream.Seek(p, soFromBeginning);
      Result := FStream.ReadString(PosTo.Pos - p);
    end;
  end;

begin
  FStream := TStringStream.Create(FScript.Text);
  Lexer := TUIBLexer.Create(FStream);
  Grammar := TUIBGrammar.Create(Lexer);
  try
    if (Grammar.yyparse = 0) and (Grammar.RootNode <> nil) then
    begin
      for i := 0 to Grammar.RootNode.NodesCount - 1 do
      begin
        if Assigned(FOnParse) then
          FOnParse(self, Grammar.RootNode.Nodes[i].NodeType, Statement, i,
            Grammar.RootNode.NodesCount);
        case Grammar.RootNode.Nodes[i].NodeType of
          NodeSetSqlDialect:
            begin
              CheckDatabase;
              if TryStrToInt(Grammar.RootNode.Nodes[i].Value, Dialect) then
                Transaction.DataBase.SQLDialect := Dialect else
                raise EUIBError.Create('Parse error: SET SQL DIALECT');
            end;
          NodeSetNames:
            begin
              CheckDatabase;
              for j := low(TCharacterSet) to high(TCharacterSet) do
              begin
                if (CompareText(CharacterSetStr[j], Grammar.RootNode.Nodes[i].Value) = 0) then
                begin
                  Transaction.DataBase.CharacterSet := j;
                  Break;
                end;
                raise EUIBError.Create('Parse error: SET NAMES');
              end;
            end;
          NodeCreateDatabase:
            begin
              CheckDatabase;
              Transaction.DataBase.Connected := False;
              TrHandle := nil;
              with Transaction.DataBase do
              begin
                FLibrary.Load(FLiBraryName);
                // I MUST provide the real DB Handle (not nil)
                // because altering forein key can fail otherwise.
                Transaction.DataBase.Lock;
                try
                  FLibrary.DSQLExecuteImmediate(
                    FDbHandle, TrHandle, Statement, SQLDialect);
                finally
                  Transaction.DataBase.UnLock;
                end;
              end;
              with Grammar.RootNode.Nodes[i].Nodes[0] do
              for k := 0 to NodesCount - 1 do
              case Nodes[k].NodeType of
                NodeName     : Transaction.DataBase.DatabaseName :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
                NodeUsername : Transaction.DataBase.UserName :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
                NodePassWord : Transaction.DataBase.PassWord :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
              end;
            end;
          NodeConnect:
            with Transaction.DataBase do
            begin
              Connected := False;
              with Grammar.RootNode.Nodes[i] do
              begin
                DatabaseName := copy(Nodes[0].Value ,2, Length(Nodes[0].Value) - 2);
                UserName     := copy(Nodes[1].Value ,2, Length(Nodes[1].Value) - 2);
                PassWord     := copy(Nodes[2].Value ,2, Length(Nodes[2].Value) - 2);
              end;
              Connected := True;
            end;
          NodeSetAutoDDL:
            begin
              if UpperCase(Grammar.RootNode.Nodes[i].Value) = 'ON' then  FAutoDDL := True else
              if UpperCase(Grammar.RootNode.Nodes[i].Value) = 'OFF' then FAutoDDL := False else
              raise EUIBError.Create('"SET AUTODDL" must be "ON" or "OFF"');
            end;
          NodeCommit:
            begin
              Transaction.Commit;
            end;
          NodeRollback:
            begin
              Transaction.RollBack;
            end;
        {$IFDEF IB71_UP}
          NodeSavepointSet:
            Transaction.SavepointStart(Grammar.RootNode.Nodes[i].Nodes[0].Value);
          NodeSavepointRelease:
            Transaction.SavepointRelease(Grammar.RootNode.Nodes[i].Nodes[0].Value);
          NodeSavepointUndo:
            Transaction.SavepointRollback(Grammar.RootNode.Nodes[i].Nodes[0].Value);
        {$ENDIF}
          NodeSelect, // perhaps a select statement execute a procedure ...
          NodeInsert,
          NodeDeleteSearched,
          NodeDeletePositioned,
          NodeUpdateSearched,
          NodeUpdatePositioned:
            begin
              FQuery.SQL.Text := trim(Statement);
              FQuery.ExecSQL;
              FQuery.Close(etmStayIn);
            end;
        else
          // DDL ...
          FQuery.SQL.Text := trim(Statement);
          FQuery.ExecSQL; // faster for ddl
          if FAutoDDL then
            FQuery.Close(etmCommit) else
            FQuery.Close(etmStayIn);

        end;
      end;
    end else
      raise EUIBParser.Create(Lexer.yylineno, Lexer.yycolno);
  finally
    FQuery.Close(etmCommit);
    Transaction.Commit;
    Grammar.Free;
    Lexer.yyinput := nil;
    FStream.Free;
    Lexer.Free;
  end;
end;

function TJvUIBScript.GetTransaction: TJvUIBTransaction;
begin
  Result := FQuery.Transaction;
end;

procedure TJvUIBScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TJvUIBScript.SetTransaction(const Value: TJvUIBTransaction);
begin
  FQuery.Transaction := Value;
end;

end.
