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
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

{ @abstract(UIB Visual components.)
  @author(Henri Gourvest: hgourvest@progdigy.com)
  @lastmod(Jan 16, 2003)}

{$I jvcl.inc}
{$I JvUIB.inc}

unit JvUIB;

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
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  Classes, SysUtils, SyncObjs,
  JvUIBLib, JvUIBase, JvUIBSQLParser, JvUIBConst;

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

{ All UIB components inherit from this class to encapsulate Critical Sections.
  Critical Sections make UIB Thread Safe. }
  {$IFDEF USEJVCL}
  TJvUIBComponent = class(TJvComponent)
  {$ELSE}
  TJvUIBComponent = class(TComponent)
  {$ENDIF USEJVCL}
  private
    FCriticalSection: TCriticalSection;
  public
    { @exclude }
    constructor Create(AOwner: TComponent); override;
    { @exclude }
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
  TJvUIBDataBase = class;

  { The list of MetaData Objects returned by TJvUIBDatabase.GetMetadata function. }
  TMetaDataOptions = class(TPersistent)
  private
    FObjects: TOIDDatabases;
    FTables: TOIDTables;
    FViews: TOIDViews;
    FProcedures: TOIDProcedures;
    FUDFs: TOIDUDFs;
    FSysInfos: Boolean;
  public
    { @exclude }
    constructor Create;
  published
    { Metadata objects (Procedure, Generator, Exception, UDF, Role). }
    property Objects: TOIDDatabases read FObjects write FObjects default ALLObjects;
    { Table properties (TableField, Primary, Foreign, TableTrigger, Unique, Index, Check)}
    property Tables: TOIDTables read FTables write FTables default ALLTables;
    { View properties (Fields & Triggers)}
    property Views: TOIDViews read FViews write FViews default ALLViews;
    { Procedure properties (input & output parametters). }
    property Procedures: TOIDProcedures read FProcedures write FProcedures default AllProcedures;
    { UDFs properties (Fields). }
    property UDFs: TOIDUDFs read FUDFs write FUDFs default ALLUDFs;
    { Include System tables, triggers and domains. }
    property SysInfos: Boolean read FSysInfos write FSysInfos default False;
  end;

  TJvUIBDataBase = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLibraryName: TFileName;
    FDbHandle: IscDbHandle;
    FHandleShared: Boolean;
    FParams: TStringList;
    FDatabaseName: TFileName;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FTransactions: TList;
    FOnConnectionLost: TNotifyEvent;
    FExceptions: TList;
    FMetaData: TObject;
    FMetaDataOptions: TMetaDataOptions;
    function ReadParamString(Param: string; Default: string = ''): string;
    procedure WriteParamString(Param: string; Value: string);
    function ReadParamInteger(Param: string; Default: Integer): Integer;
    procedure WriteParamInteger(Param: string; Value: Integer);
    function GetParams: TStrings;
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
    procedure SetDbHandle(const Value: IscDbHandle);
    procedure SetLibraryName(const Lib: TFileName);
    function GetTransactions(const Index: Cardinal): TJvUIBTransaction;
    function GetTransactionsCount: Cardinal;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(const Value: Word);
  protected
    procedure DoOnConnectionLost(Lib: TUIBLibrary); virtual;
    procedure DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass); virtual;
  public
    { Constructor method. }
    constructor Create(AOwner: TComponent); override;
    { Destructor method. }
    destructor Destroy; override;
    { Execute a SQL statement without the need to have the database connected,
      it is usefull to create a database by SQL. }
    procedure ExecuteImmediate(const Statement: string);
    { Remove all Interbase Exception class registered using 'RegistedException'. }
    procedure ClearExceptions;
    { Associate an Interbase Exception with a Delphi exception, ID is the Exception Identifier number. }
    procedure RegisterException(Excpt: EUIBExceptionClass; ID: Integer); overload;
    { Associate an Interbase Exception with a Delphi exception, Name is the Interbase Exception name. }
    function RegisterException(Excpt: EUIBExceptionClass; const Name: string): Integer; overload;
    { Remove the Registered Exception number. }
    procedure UnRegisterException(Number: Integer);
    { Remove the Registered Exception class. }
    procedure UnRegisterExceptions(Excpt: EUIBExceptionClass);
    { Create a database with a default page size of 2048. }
    procedure CreateDatabase(PageSize: Integer = 2048);
    { Return a TMetaDatabase class corresponding to the current connection. }
    function GetMetadata(Refresh: Boolean = False): TObject;
    { The DbHandle can be used to share the current connection with other Interbase components like IBX. }
    property DbHandle: IscDbHandle read FDbHandle write SetDbHandle;
    { Determine if the DbHandle is initialized by another component. }
    property IsHandleShared: Boolean read FHandleShared;
    { List all transactions connected to the database component. }
    property Transactions[const Index: Cardinal]: TJvUIBTransaction read GetTransactions;
    { Number of connected transactions. }
    property TransactionsCount: Cardinal read GetTransactionsCount;
    { Can be used to access the low level API. }
    property Lib: TUIBLibrary read FLibrary;
  published
    { DataBase connection parametters. }
    property Params: TStrings read GetParams write SetParams;
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
    property LibraryName: TFileName read FLibraryName write SetLibraryName;
    { This event occur after the component is connected to database. }
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    { This event occur before the component is connected to database. }
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    { This event occur after the component is disconnected from database. }
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    { This event occur before the component is disconnected from database. }
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    { When connection lost, Database, Transactions and Queries are automatically closed.
      Only one exception is raised to terminate the current stack and this event occur. }
    property OnConnectionLost: TNotifyEvent read FOnConnectionLost write FOnConnectionLost;
    { The blob segment size used to write in database, this parametter depend on hard drive. }
    property SegmentSize: Word read GetSegmentSize write SetSegmentSize default 16 * 1024;
    { The list of MetaData Objects returned by GetMetadata. }
    property MetaDataOptions: TMetaDataOptions read FMetaDataOptions;
  end;

  { Describe how a transaction is closed. }
  TEndTransMode = (
    etmDefault, // Use default Transaction Action
    etmStayIn, // keep transaction without commit or rollback
    etmCommit, // commit transaction
    etmCommitRetaining, // commit transaction and keep transaction handle
    etmRollback, // rollback transaction
    etmRollbackRetaining // rollback transaction and keep transaction handle
    );

  { Indicate the Query state.
    order is important ! }
  TQueryState = (
    qsDataBase, // have a database handle
    qsTransaction, // have a transaction handle
    qsExecImme, // Query executed immediately without the need of statement handle
    qsStatement, // have a statement handle
    qsPrepare, // Query prepared
    qsExecute // Query executed
    );

  {Oo.......................................................................oO
                                  TUIBTransaction
   Oo.......................................................................oO}

  // Transaction parameters
  TTransParam = (
    { prevents a transaction from accessing tables if they are written to by
      other transactions.}
    tpConsistency,
    { allows concurrent transactions to read and write shared data. }
    tpConcurrency,
    { Concurrent, shared access of a specified table among all transactions. }
    tpShared,
    { Concurrent, restricted access of a specified table. }
    tpProtected,
    tpExclusive,
    { Specifies that the transaction is to wait until the conflicting resource
      is released before retrying an operation [Default]. }
    tpWait,
    { Specifies that the transaction is not to wait for the resource to be
      released, but instead, should return an update conflict error immediately. }
    tpNowait,
    { Read-only access mode that allows a transaction only to select data from tables. }
    tpRead,
    { Read-write access mode of that allows a transaction to select, insert,
      update, and delete table data [Default]. }
    tpWrite,
    { Read-only access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option. }
    tpLockRead,
    { Read-write access of a specified table. Use in conjunction with tpShared,
      tpProtected, and tpExclusive to establish the lock option [Default]. }
    tpLockWrite,
    tpVerbTime,
    tpCommitTime,
    tpIgnoreLimbo,
    { Unlike a concurrency transaction, a read committed transaction sees changes
      made and committed by transactions that were active after this transaction started. }
    tpReadCommitted,
    tpAutoCommit,
    { Enables an tpReadCommitted transaction to read only the latest committed
      version of a record. }
    tpRecVersion,
    tpNoRecVersion,
    tpRestartRequests,
    tpNoAutoUndo
    );

  { Set of transaction parameters. }
  TTransParams = set of TTransParam;
  {This evenet occur before to end the transaction, you can change the ETM parametter.}
  TOnEndTransaction = procedure(Sender: TObject; var Mode: TEndTransMode) of object;

  { The Transaction component. }
  TJvUIBTransaction = class(TJvUIBComponent)
  private
    FDataBase: TJvUIBDataBase;
    FDataBases: TList;
    FTrHandle: IscTrHandle;
    FSQLComponent: TList;
    FStatements: Integer;
    FOptions: TTransParams;
    FLockRead: string;
    FLockWrite: string;
    FSQLDialect: Integer;
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TOnEndTransaction;
    FAutoRetain: Boolean;
    FAutoStart: Boolean;
    FAutoStop: Boolean;
    FDefaultAction: TEndTransMode;
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
    procedure BeginTransaction(Auto: Boolean = True);
    function EndTransaction(ETM: TEndTransMode; From: TJvUIBStatement;
      Auto: Boolean): Boolean;
    procedure AddSQLComponent(Component: TJvUIBStatement);
    procedure RemoveSQLComponent(Component: TJvUIBStatement);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode; Auto: Boolean);
    function GetStatements(const Index: Integer): TJvUIBStatement;
    function GetStatementsCount: Integer;
    procedure ClearDataBases;
    function GetDatabases(const Index: Integer): TJvUIBDataBase;
    function GetDatabasesCount: Integer;
    function GetAutoRetain: Boolean;
    procedure SetAutoRetain(const Value: Boolean);
    procedure SetDefaultAction(const Value: TEndTransMode);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataBase(const ADatabase: TJvUIBDataBase); virtual;
  public
    { Constructor method. }
    constructor Create(AOwner: TComponent); override;
    { Destructor method.}
    destructor Destroy; override;
    { cf TjvUIBComponent.Lock }
    procedure Lock; override;
    { cf TjvUIBComponent.UnLock }
    procedure UnLock; override;
    { Add a database to the transaction. }
    procedure AddDataBase(ADataBase: TJvUIBDataBase);
    { Remove a database from a transaction. }
    procedure RemoveDatabase(ADataBase: TJvUIBDataBase); overload;
    { Remove a database from a transaction. }
    procedure RemoveDatabase(Index: Integer); overload;
    {Start Transaction.}
    procedure StartTransaction;
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
    { Interbase 7.1 spceficic, Release a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRelease(const Name: string);
    { Interbase 7.1 spceficic, RollBack a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointRollback(const Name: string; Option: Word = 0);
    { Interbase 7.1 spceficic, Start a savepoint.
      On Firebird 1.5 this must be call by SQL.}
    procedure SavepointStart(const Name: string);
    {$ENDIF IB71_UP}
    property InTransaction: Boolean read GetInTransaction;
    {Transaction handle.}
    property TrHandle: IscTrHandle read FTrHandle;
    { Queries connected to this transaction.}
    property Statements[const Index: Integer]: TJvUIBStatement read GetStatements;
    { Number of Queries connected to this transaction.}
    property StatementsCount: Integer read GetStatementsCount;
    { Get all databases attached to the transaction. }
    property Databases[const Index: Integer]: TJvUIBDataBase read GetDatabases;
    { How many databases attached to the transaction. }
    property DatabasesCount: Integer read GetDatabasesCount;
  published
    {Database connection.}
    property DataBase: TJvUIBDataBase read GetDataBase write SetDataBase;
    {Transaction parametters.}
    property Options: TTransParams read GetOptions write SetOptions default [tpConcurrency, tpWait, tpWrite];
    {List of the tables to lock for read, tpLockRead option must set. ex: 'Table1;Table2'}
    property LockRead: string read GetLockRead write SetLockRead;
    {List of the tables to lock for write, tpLockWrite option must set. ex: 'Table1;Table2'}
    property LockWrite: string read GetLockWrite write SetLockWrite;
    {This event occur after a transaction is started.}
    property OnStartTransaction: TNotifyEvent read FOnStartTransaction write FOnStartTransaction;
    {This evenet occur before to end the transaction, you can change the ETM parametter.}
    property OnEndTransaction: TOnEndTransaction read FOnEndTransaction write FOnEndTransaction;
    {If False, commit and rollback close all connected statements and finally close transaction.
     If True, commit and rollback are modified to commitretaining or rollbackretaining if at least one statement is open.}
    property AutoRetain: Boolean read GetAutoRetain write SetAutoRetain default False;
    {If True, transaction automatically started when needed.
     if False you must explicitely call "starttransaction".}
    property AutoStart: Boolean read FAutoStart write FAutoStart default True;
    {default = False, if True you need to close transaction explicitly.}
    property AutoStop: Boolean read FAutoStop write FAutoStop default True;
    {Transaction default action if closed automaticaly, commit or rollback only.}
    property DefaultAction: TEndTransMode read FDefaultAction write SetDefaultAction default etmCommit;
  end;

  { Simple query component. }
  TJvUIBStatement = class(TJvUIBComponent)
  private
    FCurrentState: TQueryState;
    FTransaction: TJvUIBTransaction;
    FDataBase: TJvUIBDataBase;
    FStHandle: IscStmtHandle;
    FOnError: TEndTransMode;
    FCursorName: string;
    FSQLResult: TSQLResult;
    FCachedFetch: Boolean;
    FFetchBlobs: Boolean;
    FBufferChunks: Cardinal;
    FQuickScript: Boolean;
    FSQL: TStringList;
    FParsedSQL: string;
    FParameter: TSQLParams;
    FParseParams: Boolean;
    FOnClose: TNotifyEvent;
    FStatementType: TUIBStatementType;
    function GetPlan: string;
    function GetStatementType: TUIBStatementType;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure DoSQLChange(Sender: TObject);
    function GetFields: TSQLResult;
    function GetEof: Boolean;
    function FindDataBase: TJvUIBDataBase;
    function GetRowsAffected: Cardinal;
  protected
    procedure SetTransaction(const Transaction: TJvUIBTransaction); virtual;
    procedure SetDataBase(ADataBase: TJvUIBDataBase);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure BeginTransaction; virtual;
    procedure BeginStatement; virtual;
    procedure BeginPrepare; virtual;
    procedure BeginExecute; virtual;
    procedure BeginExecImme; virtual;

    procedure EndTransaction(const ETM: TEndTransMode; Auto: Boolean); virtual;
    procedure EndStatement(const ETM: TEndTransMode; Auto: Boolean); virtual;
    procedure EndPrepare(const ETM: TEndTransMode; Auto: Boolean); virtual;
    procedure EndExecute(const ETM: TEndTransMode; Auto: Boolean); virtual;
    procedure EndExecImme(const ETM: TEndTransMode; Auto: Boolean); virtual;

    procedure InternalNext; virtual;
    procedure InternalPrior; virtual;
    procedure InternalClose(const Mode: TEndTransMode; Auto: Boolean); virtual;

    function ParamsClass: TSQLParamsClass; virtual;
    function ResultClass: TSQLResultClass; virtual;

    procedure InternalGetBlobSize(SqlDa: TSQLDA; const Index: Word; out Size: Cardinal);
    procedure InternalReadBlob(SqlDa: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlob(SqlDa: TSQLDA; const Index: Word; var Str: string); overload;
    procedure InternalReadBlob(SqlDa: TSQLDA; const Index: Word; var Value: Variant); overload;
    procedure InternalReadBlob(SqlDa: TSQLDA; const Index: Word; Buffer: Pointer); overload;

    property QuickScript: Boolean read FQuickScript write FQuickScript default False;
  public
    { Constructor method. }
    constructor Create(AOwner: TComponent); override;
    { Destructor method. }
    destructor Destroy; override;
    { cf TJvUIBComponent.Lock }
    procedure Lock; override;
    { cf TJvUIBComponent.UnLock }
    procedure UnLock; override;
    { Close the statement. You can commit or rollback the transaction when closing. }
    procedure Close(const Mode: TEndTransMode = etmStayIn); virtual;
    { Fetch all records returned by the query. }
    procedure FetchAll;
    { Open the query and fetch the first record if FetchFirst = True. }
    procedure Open(FetchFirst: Boolean = True);
    { Prepare the query. }
    procedure Prepare;
    { Execute the query. }
    procedure Execute;
    { Execute the query or the script (QuickScript = True) immediately. }
    procedure ExecSQL;
    { Get the next record. }
    procedure Next;
    { Get the prior record. }
    procedure Prior;
    { Get the last record. }
    procedure Last;
    { Get the firdt record. }
    procedure First;
    { Read a the blob in a stream by index. }
    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    { Read a the blob in a string by index. }
    procedure ReadBlob(const Index: Word; var Str: string); overload;
    { Read a the blob in a Variant by index. }
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by index. }
    procedure ReadBlob(const Index: Word; Buffer: Pointer); overload;
    { Read a the blob in a stream by name. }
    procedure ReadBlob(const Name: string; Stream: TStream); overload;
    { Read a the blob in a string by name. }
    procedure ReadBlob(const Name: string; var Str: string); overload;
    { Read a the blob in a Variant by name. }
    procedure ReadBlob(const Name: string; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by name. }
    procedure ReadBlob(const Name: string; Buffer: Pointer); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Index: Word; var Str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Name: string; var Str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word); overload;

    { Get the the blob size of the current record. }
    function FieldBlobSize(const Index: Word): Cardinal;
    { Get the blob size of the corresonding parametter. }
    function ParamBlobSize(const Index: Word): Cardinal;

    { The internal statement handle. }
    property StHandle: IscStmtHandle read FStHandle;
    { Use fields to read the current record. }
    property Fields: TSQLResult read GetFields;
    { use Params to set parametters, the param names are set dynamically
      parsing the SQL query, by default the param values are null string.
      The first time you set a parametter value, the field type is defined.  }
    property Params: TSQLParams read FParameter;
    { All UIB statements declare a unique cursor name, another query can use
      this cursor to modify the current cursor, this feature is for unidirectionnal
      statements !!.<br>
      ex: UPDATE proj_dept_budget SET projected_budget = :value WHERE CURRENT OF %s; }
    property CursorName: string read FCursorName;
    { Indicate the current state of the query. }
    property CurrentState: TQueryState read FCurrentState;
    { if True there isn't anymore record to fetch. }
    property Eof: Boolean read GetEof;
    { @exclude }
    property ParseParams: Boolean read FParseParams write FParseParams;
    { The plan used internally by interbase (the query must be prepared). }
    property Plan: string read GetPlan;
    { Get the current statement type (the query must be prepared). }
    property StatementType: TUIBStatementType read GetStatementType;
    { Return the number of rows affected by the query (stInsert, stUpdate or stDelete). }
    property RowsAffected: Cardinal read GetRowsAffected;
  published
    { The sql query. }
    property SQL: TStrings read GetSQL write SetSQL;
    { Transaction of the query. }
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
    { Connected database, in most cases you don't need to set this property, it is
      only needed if the transaction concern more than one database. }
    property DataBase: TJvUIBDataBase read FDataBase write SetDataBase;
    { If an error occur, this action is applied to the connected transaction. }
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    { If True all record are saved in memory. }
    property CachedFetch: Boolean read FCachedFetch write FCachedFetch default True;
    { If True the blob data is fetched with the record. }
    property FetchBlobs: Boolean read FFetchBlobs write FFetchBlobs default False;
    { Use BufferChunks to get or set the number of records for which the query
      allocates buffer space at any time. When the query’s buffer is full,
      trying to fetch an additional record causes the dataset to reallocate
      the buffer so that it has enough memory to hold an additional BufferChunks
      records. <br>
      Note: When CachedFetch is False, BufferChunks has no meaning. }
    property BufferChunks: Cardinal read FBufferChunks write FBufferChunks default 1000;
    { OnClose event. }
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  {Oo.......................................................................oO
                                  TUIBQuery
   Oo.......................................................................oO}
  { The query component. }
  TJvUIBQuery = class(TJvUIBStatement)
  public
    { Helper method to buid the SQL query needed to execute the stored procedure.
      Input data type found using this method. }
    procedure BuildStoredProc(const StoredProc: string);
  published
    { If True you can use this component as a fast script component where each line is a query.
      You must use the ExecSQL method ! }
    property QuickScript;
  end;

  { Parsing event, occur on each query executed. }
  TOnParse = procedure(Sender: TObject; NodeType: TSQLNodeType;
    const Statement: string; Position, Count: Integer) of object;

  { The script component. }
  TJvUIBScript = class(TJvUIBComponent)
  private
    FQuery: TJvUIBQuery;
    FScript: TStringList;
    FAutoDDL: Boolean;
    FOnParse: TOnParse;
    procedure SetTransaction(const Value: TJvUIBTransaction);
    function GetTransaction: TJvUIBTransaction;
    function GetScript: TStrings;
    procedure SetScript(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecuteScript;
  published
    property Transaction: TJvUIBTransaction read GetTransaction write SetTransaction;
    property Script: TStrings read GetScript write SetScript;
    property AutoDDL: Boolean read FAutoDDL write FAutoDDL default True;
    property OnParse: TOnParse read FOnParse write FOnParse;
  end;

  TUIBProtocol = (
    proLocalHost,
    proTCPIP,
    // (rom) should be renamed to proNetBEUI
    proNetBUI
    );

  TJvUIBService = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLibraryName: string;
    FUserName: string;
    FPassWord: string;
    FHost: string;
    FProtocol: TUIBProtocol;
    FHandle: IscSvcHandle;
    procedure BeginService;
    procedure EndService;
    function CreateSPB: string; virtual;
    procedure SetLibraryName(const Lib: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property UserName: string read FUserName write FUserName;
    property PassWord: string read FPassWord write FPassWord;
    property Host: string read FHost write FHost;
    property Protocol: TUIBProtocol read FProtocol write FProtocol default proLocalHost;
    { Define wich library the connection use.}
    property LibraryName: string read FLibraryName write SetLibraryName;
  end;

  TVerboseEvent = procedure(Sender: TObject; Message: string) of object;

  TJvUIBBackupRestore = class(TJvUIBService)
  private
    FBackupFiles: TStringList;
    FDatabase: TFileName;
    FOnVerbose: TVerboseEvent;
    function GetBackupFiles: TStrings;
    procedure SetBackupFiles(const Value: TStrings);
    function CreateStartSPB: string; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  published
    property BackupFiles: TStrings read GetBackupFiles write SetBackupFiles;
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
    {$IFDEF IB71_UP}, roValidate {$ENDIF});

  TRestoreOptions = set of TRestoreOption;

  TJvUIBRestore = class(TJvUIBBackupRestore)
  private
    FOptions: TRestoreOptions;
    FPageSize: Cardinal;
    function CreateStartSPB: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TRestoreOptions read FOptions write FOptions default [roCreateNewDB];
    property PageSize: Cardinal read FPageSize write FPageSize default 0;
  end;

implementation

uses
  Math,
  JvUIBMetaData;

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = record
    ExepClass: EUIBExceptionClass;
    ID: Integer;
  end;

//=== TJvUIBDataBase =========================================================

constructor TJvUIBDataBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibrary := TUIBLibrary.Create;
  FLibraryName := GetClientLibrary;
  FLibrary.OnConnectionLost := DoOnConnectionLost;
  FLibrary.OnGetDBExceptionClass := DoOnGetDBExceptionClass;
  FDbHandle := nil;
  FHandleShared := False;
  FParams := TStringList.Create;
  SQLDialect := 3;
  CharacterSet := csNONE;
  WriteParamString('sql_role_name', '');
  FExceptions := TList.Create;
  FMetaData := nil;
  FMetaDataOptions := TMetaDataOptions.Create;
end;

destructor TJvUIBDataBase.Destroy;
begin
  Lock;
  try
    Connected := False;
    ClearTransactions;
    FParams.Free;
    ClearExceptions;
    FExceptions.Free;
    FLibrary.Free;
    FMetaDataOptions.Free;
  finally
    UnLock;
  end;
  inherited Destroy;
end;

procedure TJvUIBDataBase.AddTransaction(Transaction: TJvUIBTransaction);
begin
  if FTransactions = nil then
    FTransactions := TList.Create;
  FTransactions.Add(Transaction);
end;

procedure TJvUIBDataBase.ClearTransactions;
begin
  while FTransactions <> nil do
    TJvUIBTransaction(FTransactions.Last).RemoveDatabase(Self);
end;

procedure TJvUIBDataBase.CloseTransactions;
var
  I: Integer;
begin
  if FTransactions <> nil then
    for I := 0 to FTransactions.Count - 1 do
      TJvUIBTransaction(FTransactions.Items[I]).Close(etmDefault, True);
end;

procedure TJvUIBDataBase.DoOnConnectionLost(Lib: TUIBLibrary);
begin
  Lib.RaiseErrors := False;
  try
    Connected := False;
  finally
    Lib.RaiseErrors := True;
    if Assigned(FOnConnectionLost) then
      FOnConnectionLost(Self);
  end;
end;

function TJvUIBDataBase.GetCharacterSet: TCharacterSet;
var
  I: TCharacterSet;
  S: string;
begin
  S := Trim(UpperCase(ReadParamString('lc_ctype', 'NONE')));
  Result := csNONE;
  for I := Low(TCharacterSet) to High(TCharacterSet) do
    if S = CharacterSetStr[I] then
    begin
      Result := I;
      Break;
    end;
end;

function TJvUIBDataBase.GetConnected: Boolean;
begin
  Lock;
  try
    Result := FDbHandle <> nil;
  finally
    UnLock;
  end;
end;

function TJvUIBDataBase.GetPassWord: string;
begin
  Result := ReadParamString('password');
end;

function TJvUIBDataBase.GetSQLDialect: Integer;
const
  cSqlDialect = 'sql_dialect';
begin
  try
    Result := ReadParamInteger(cSqlDialect, 3);
  except
    WriteParamInteger(cSqlDialect, 3);
    raise;
  end;
end;

procedure TJvUIBDataBase.ExecuteImmediate(const Statement: string);
begin
  FLibrary.Load(FLibraryName);
  FLibrary.DSQLExecuteImmediate(Statement, SQLDialect);
end;

procedure TJvUIBDataBase.CreateDatabase(PageSize: Integer = 2048);
const
  CreateDb = 'CREATE DATABASE ''%s'' USER ''%s'' PASSWORD ''%s'' ' +
    'PAGE_SIZE %d DEFAULT CHARACTER SET %s';
var
  TrHandle: IscTrHandle;
begin
  TrHandle := nil;
  Connected := False;
  FLibrary.Load(FLibraryName);
  FLibrary.DSQLExecuteImmediate(FDbHandle, TrHandle,
    Format(CreateDb, [DatabaseName, UserName, PassWord, PageSize,
    CharacterSetStr[CharacterSet]]), SQLDialect);
end;

function TJvUIBDataBase.GetUserName: string;
begin
  Result := ReadParamString('user_name');
end;

function TJvUIBDataBase.ReadParamInteger(Param: string;
  Default: Integer): Integer;
begin
  Result := StrToInt(ReadParamString(Param, IntToStr(Default)));
end;

function TJvUIBDataBase.ReadParamString(Param, Default: string): string;
var
  I: Integer;
begin
  Lock;
  try
    I := Params.IndexOfName(Param);
    if I >= 0 then
    begin
      Result := Copy(Params[I], Length(Param) + 2, MaxInt);
      Exit;
    end;
    Result := Default;
  finally
    UnLock;
  end;
end;

procedure TJvUIBDataBase.RemoveTransaction(Transaction: TJvUIBTransaction);
begin
  if FTransactions <> nil then
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
  if Value = Connected then
    Exit;
  Lock;
  try
    with FLibrary do
      case Value of
        False:
          begin
            if Assigned(FBeforeDisconnect) then
              FBeforeDisconnect(Self);
            CloseTransactions;
            if FMetaData <> nil then
              FreeAndNil(FMetaData);
            if FHandleShared then
            begin
              FDbHandle := nil;
              FHandleShared := False;
            end
            else
              DetachDatabase(FDbHandle);
            if Assigned(FAfterDisconnect) then
              FAfterDisconnect(Self);
          end;
        True:
          begin
            if Assigned(FBeforeConnect) then
              FBeforeConnect(Self);
            FLibrary.Load(FLibraryName);
            if not FHandleShared then
              AttachDatabase(FDatabaseName, FDbHandle, Params.Text, BreakLine);
            if Assigned(FAfterConnect) then
              FAfterConnect(Self);
          end;
      end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBDataBase.SetDatabaseName(const Value: TFileName);
begin
  FDatabaseName := Value;
  if csDesigning in ComponentState then
    Connected := False;
end;

procedure TJvUIBDataBase.SetDbHandle(const Value: IscDbHandle);
begin
  if (FDbHandle = nil) or ((FDbHandle <> nil) and FHandleShared) then
  begin
    FLibrary.Load(FLibraryName);
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end
  else
    raise Exception.Create(EUIB_DBHANDLEALLREADYSET);
end;

procedure TJvUIBDataBase.SetLibraryName(const Lib: TFileName);
begin
  SetConnected(False);
  FLibrary.UnLoad;
  FLibraryName := Lib;
end;

function TJvUIBDataBase.GetTransactions(const Index: Cardinal): TJvUIBTransaction;
begin
  if FTransactions <> nil then
    Result := FTransactions.Items[Index]
  else
    raise EListError.CreateFmt(EUIB_INDEXERROR, [Index]);
end;

function TJvUIBDataBase.GetTransactionsCount: Cardinal;
begin
  if FTransactions <> nil then
    Result := FTransactions.Count
  else
    Result := 0;
end;

function TJvUIBDataBase.GetParams: TStrings;
begin
  Result := FParams;
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

procedure TJvUIBDataBase.WriteParamInteger(Param: string; Value: Integer);
begin
  WriteParamString(Param, IntToStr(Value));
end;

procedure TJvUIBDataBase.WriteParamString(Param, Value: string);
var
  I: Integer;
  S: string;
begin
  Lock;
  try
    S := Param + '=' + Value;
    I := Params.IndexOfName(Param);
    if I >= 0 then
      Params[I] := S
    else
      Params.Add(S);
  finally
    UnLock;
  end;
end;

procedure TJvUIBDataBase.ClearExceptions;
var
  I: Integer;
begin
  for I := 0 to FExceptions.Count - 1 do
    FreeMem(FExceptions[I]);
  FExceptions.Clear;
end;

procedure TJvUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  ID: Integer);
var
  ExcepInfo: PExceptionInfo;
  I: Integer;
begin
  for I := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[I]).ID = ID then
      raise Exception.CreateFmt(EUIB_EXPTIONREGISTERED, [ID]);
  GetMem(ExcepInfo, SizeOf(TExceptionInfo));
  ExcepInfo.ExepClass := Excpt;
  ExcepInfo.ID := ID;
  FExceptions.Add(ExcepInfo);
end;

function TJvUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  const Name: string): Integer;
var
  Transaction: TJvUIBTransaction;
  Query: TJvUIBQuery;
begin
  Result := -1;
  Transaction := TJvUIBTransaction.Create(nil);
  Query := TJvUIBQuery.Create(nil);
  try
    Transaction.DataBase := Self;
    Query.Transaction := Transaction;
    Query.CachedFetch := False;
    Query.SQL.Text := 'SELECT RDB$EXCEPTION_NUMBER FROM RDB$EXCEPTIONS WHERE RDB$EXCEPTION_NAME = ?';
    Query.Params.AsString[0] := UpperCase(Name);
    Query.Open;
    if not Query.Eof then
    begin
      Result := Query.Fields.AsInteger[0];
      RegisterException(Excpt, Result);
    end;
    Query.Close(etmCommit);
    if Result = -1 then
      raise Exception.CreateFmt(EUIB_EXCEPTIONNOTFOUND, [Name]);
  finally
    Query.Free;
    Transaction.Free;
  end;
end;

procedure TJvUIBDataBase.UnRegisterException(Number: Integer);
var
  I: Integer;
begin
  for I := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[I]).ID = Number then
    begin
      FreeMem(FExceptions[I]);
      FExceptions.Delete(I);
      Break;
    end;
end;

procedure TJvUIBDataBase.UnRegisterExceptions(Excpt: EUIBExceptionClass);
var
  I: Integer;
begin
  I := 0;
  while I < FExceptions.Count do
  begin
    if PExceptionInfo(FExceptions[I]).ExepClass = Excpt then
    begin
      FreeMem(FExceptions[I]);
      FExceptions.Delete(I);
    end
    else
      Inc(I);
  end;
end;

procedure TJvUIBDataBase.DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass);
var
  I: Integer;
begin
  for I := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[I]).ID = Number then
    begin
      Excep := PExceptionInfo(FExceptions[I]).ExepClass;
      Exit;
    end;
  Excep := EUIBException;
end;

function TJvUIBDataBase.GetMetadata(Refresh: Boolean = False): TObject;
var
  Transaction: TJvUIBTransaction;
begin
  if Refresh and (FMetaData <> nil) then
    FreeAndNil(FMetaData);
  if FMetaData = nil then
  begin
    Transaction := TJvUIBTransaction.Create(nil);
    try
      Transaction.Database := Self;
      FMetaData := TMetaDataBase.Create(nil, -1);
      with TMetaDataBase(FMetaData) do
      begin
        OIDDatabases := FMetaDataOptions.Objects;
        OIDTables := FMetaDataOptions.Tables;
        OIDViews := FMetaDataOptions.Views;
        OIDProcedures := FMetaDataOptions.Procedures;
        OIDUDFs := FMetaDataOptions.UDFs;
        SysInfos := FMetaDataOptions.FSysInfos
      end;
      try
        TMetaDataBase(FMetaData).LoadFromDatabase(Transaction);
        Transaction.Commit;
      except
        FreeAndNil(FMetaData);
        raise;
      end;
    finally
      Transaction.Free;
    end;
  end;
  Result := FMetaData;
end;

function TJvUIBDataBase.GetSegmentSize: Word;
begin
  Result := FLibrary.SegMentSize;
end;

procedure TJvUIBDataBase.SetSegmentSize(const Value: Word);
begin
  FLibrary.SegMentSize := Value;
end;

//=== TJvUIBStatement ========================================================

constructor TJvUIBStatement.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentState := qsDataBase;
  if AOwner is TJvUIBTransaction then
    Transaction := TJvUIBTransaction(AOwner)
  else
    FTransaction := nil;
  FSQL := TStringList.Create;
  FSQL.OnChange := DoSQLChange;
  FCachedFetch := True;
  FetchBlobs := False;
  FQuickScript := False;
  FOnError := etmRollback;
  FParameter := ParamsClass.Create;
  FCursorName := '';
  FBufferChunks := 1000;
  FParseParams := True;
end;

destructor TJvUIBStatement.Destroy;
begin
  FSQL.Free;
  FParameter.Free;
  FParameter := nil;
  SetTransaction(nil);
  inherited Destroy;
end;

procedure TJvUIBStatement.SetTransaction(const Transaction: TJvUIBTransaction);
begin
  if FTransaction <> Transaction then
  begin
    if FTransaction <> nil then
    begin
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True)
      else
        InternalClose(etmStayIn, True);
      FTransaction.RemoveSQLComponent(Self);
    end;
    FTransaction := Transaction;
    if Transaction <> nil then
      Transaction.AddSQLComponent(Self);
    FCurrentState := qsDataBase;
  end;
end;

procedure TJvUIBStatement.SetDataBase(ADataBase: TJvUIBDataBase);
begin
  if FDataBase <> ADataBase then
  begin
    if FTransaction <> nil then
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True)
      else
        InternalClose(etmStayIn, True);
    FDataBase := ADataBase;
  end;
end;

procedure TJvUIBStatement.BeginTransaction;
begin
  if FTransaction <> nil then
    FTransaction.BeginTransaction
  else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  FCurrentState := qsTransaction;
end;

procedure TJvUIBStatement.Close(const Mode: TEndTransMode);
begin
  InternalClose(Mode, False);
end;

procedure TJvUIBStatement.Open(FetchFirst: Boolean = True);
begin
  // if you reopen the same query I Close
  // the cursor, clean sql Result and
  // execute the query again to save
  // the prepare time !
  if FCurrentState = qsExecute then
  begin
    Lock;
    try
      try
        FSQLResult.ClearRecords;
        with FindDataBase.FLibrary do
          DSQLFreeStatement(FStHandle, DSQL_close);
      except
        InternalClose(FOnError, False);
        raise;
      end;
      FCurrentState := qsPrepare;
    finally
      UnLock;
    end;
  end
  else
    InternalClose(etmStayIn, False);
  if FetchFirst then
    InternalNext
  else
    BeginExecute;
end;

procedure TJvUIBStatement.Next;
begin
  if FCurrentState <> qsExecute then
    raise Exception.Create(EUIB_MUSTBEOPEN);
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
  while not Eof do
    Next;
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
  if FCurrentState < qsPrepare then
    BeginPrepare;
end;

procedure TJvUIBStatement.InternalNext;
begin
  if FCurrentState < qsExecute then
    BeginExecute;
  if (Fields.CurrentRecord + 1) < Fields.RecordCount then
    Fields.CurrentRecord := Fields.CurrentRecord + 1
  else
  begin
    Lock;
    try
      with FindDataBase.FLibrary do
      try
        if FSQLResult.FetchBlobs then
          DSQLFetchWithBlobs(FindDataBase.FDbHandle,
            FTransaction.FTrHandle, FStHandle, FTransaction.FSQLDialect, FSQLResult)
        else
          DSQLFetch(FStHandle, FTransaction.FSQLDialect, FSQLResult);
      except
        if FOnError <> etmStayIn then
          EndExecute(FOnError, False);
        raise;
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBStatement.InternalPrior;
begin
  if Fields.CachedFetch then
  begin
    if Fields.CurrentRecord > 0 then
      Fields.CurrentRecord := Fields.CurrentRecord - 1;
  end
  else
    raise Exception.Create(EUIB_CACHEDFETCHNOTSET);
end;

procedure TJvUIBStatement.EndTransaction(const ETM: TEndTransMode; Auto: Boolean);
begin
  if FTransaction <> nil then
  begin
    if FTransaction.EndTransaction(ETM, Self, Auto) then
      FCurrentState := qsDataBase;
  end
  else
    raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
end;

procedure TJvUIBStatement.BeginStatement;
begin
  BeginTransaction;
  Lock;
  try
    with FindDataBase.FLibrary do
    try
      FStHandle := nil;
      DSQLAllocateStatement(FindDataBase.FDbHandle, FStHandle);
    except
      EndTransaction(FOnError, False);
      raise;
    end;
    Inc(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsStatement;
end;

procedure TJvUIBStatement.EndStatement(const ETM: TEndTransMode; Auto: Boolean);
begin
  Lock;
  try
    with FindDataBase.FLibrary do
      DSQLFreeStatement(FStHandle, DSQL_drop);

    FStHandle := nil;
    Dec(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsTransaction;
  if ETM <> etmStayIn then
    EndTransaction(ETM, Auto);

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TJvUIBStatement.BeginPrepare;
begin
  if FStHandle = nil then
    BeginStatement;
  FSQLResult := ResultClass.Create(0, FCachedFetch, FFetchBlobs, FBufferChunks);
  Lock;
  try
    with FindDataBase.FLibrary do
    try
      if FQuickScript or not FParseParams then
        FStatementType := DSQLPrepare(FTransaction.FTrHandle, FStHandle,
          SQL.Text, FTransaction.FSQLDialect, FSQLResult)
      else
        FStatementType := DSQLPrepare(FTransaction.FTrHandle, FStHandle,
          FParsedSQL, FTransaction.FSQLDialect, FSQLResult);
      FCursorName := 'C' + IntToStr(Integer(FStHandle));
      DSQLSetCursorName(FStHandle, FCursorName);
    except
      FreeAndNil(FSQLResult);
      EndStatement(FOnError, False);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsPrepare;
end;

procedure TJvUIBStatement.EndPrepare(const ETM: TEndTransMode; Auto: Boolean);
begin
  FSQLResult.Free;
  FSQLResult := nil;
  FCurrentState := qsStatement;
  EndStatement(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecute;
begin
  if FSQLResult = nil then
    BeginPrepare;
  Lock;
  try
    with FindDataBase.FLibrary do
    try
      if FStatementType = stExecProcedure then
        DSQLExecute2(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter, FSQLResult)
      else
        DSQLExecute(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter);
    except
      if FOnError <> etmStayIn then
        EndPrepare(FOnError, False);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsExecute;
end;

procedure TJvUIBStatement.EndExecute(const ETM: TEndTransMode; Auto: Boolean);
begin
  FCurrentState := qsPrepare;
  EndPrepare(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecImme;
var
  I: Integer;

  procedure ExecuteQuery(const AQuery: string; Params: TSQLParams);
  begin
    if Trim(AQuery) = '' then
      Exit;
    Lock;
    try
      with FindDataBase.FLibrary do
      try
        DSQLExecuteImmediate(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          AQuery, FTransaction.FSQLDialect, Params);
      except
        if FOnError <> etmStayIn then
          EndExecImme(FOnError, False);
        raise;
      end;
    finally
      UnLock;
    end;
  end;
begin
  BeginTransaction;
  if FQuickScript then
    for I := 0 to SQL.Count - 1 do
      ExecuteQuery(SQL.Strings[I], nil)
  else
  if FParseParams then
    ExecuteQuery(FParsedSQL, FParameter)
  else
    ExecuteQuery(SQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TJvUIBStatement.EndExecImme(const ETM: TEndTransMode; Auto: Boolean);
begin
  FCurrentState := qsTransaction;
  if ETM <> etmStayIn then
    EndTransaction(ETM, Auto);
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
  inherited Lock;
  Ftransaction.Lock;
end;

procedure TJvUIBStatement.UnLock;
begin
  Ftransaction.UnLock;
  inherited UnLock;
end;

function TJvUIBStatement.GetSQL: TStrings;
begin
  Result := FSQL;
end;

procedure TJvUIBStatement.SetSQL(const Value: TStrings);
begin
  FSQL.Assign(Value);
end;

function TJvUIBStatement.GetPlan: string;
begin
  Lock;
  try
    if FCurrentState < qsPrepare then
      raise Exception.Create(EUIB_MUSTBEPREPARED)
    else
      Result := FindDataBase.FLibrary.DSQLInfoPlan(FStHandle);
  finally
    UnLock;
  end;
end;

function TJvUIBStatement.GetStatementType: TUIBStatementType;
begin
  if FCurrentState < qsPrepare then
    raise Exception.Create(EUIB_MUSTBEPREPARED)
  else
    Result := FStatementType;
end;

procedure TJvUIBStatement.DoSQLChange(Sender: TObject);
begin
  InternalClose(etmStayIn, True);
  if not FQuickScript or FParseParams then
    FParsedSQL := FParameter.Parse(SQL.Text);
end;

function TJvUIBStatement.GetFields: TSQLResult;
begin
  if FSQLResult = nil then
    raise Exception.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

function TJvUIBStatement.GetEof: Boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Eof
  else
    Result := True;
end;

function TJvUIBStatement.FindDataBase: TJvUIBDataBase;
begin
  if FDataBase <> nil then
    Result := FDataBase
  else
  if FTransaction <> nil then
    Result := FTransaction.FDataBase
  else
    raise Exception.Create(EUIB_DATABASENOTDEF);
end;

procedure TJvUIBStatement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Str: string);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Str)
  else
    InternalReadBlob(Fields, Index, Str);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Stream: TStream);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Stream)
  else
    InternalReadBlob(Fields, Index, Stream);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; var Value: Variant);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Value)
  else
    InternalReadBlob(Fields, Index, Value);
end;

procedure TJvUIBStatement.ReadBlob(const Name: string; Stream: TStream);
begin
  ReadBlob(Fields.GetFieldIndex(Name), Stream);
end;

procedure TJvUIBStatement.ReadBlob(const Name: string; var Str: string);
begin
  ReadBlob(Fields.GetFieldIndex(Name), Str);
end;

procedure TJvUIBStatement.ReadBlob(const Name: string; var Value: Variant);
begin
  ReadBlob(Fields.GetFieldIndex(Name), Value);
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; var Str: string);
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteString(BlobHandle, Str);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; Buffer: Pointer;
  Size: Word);
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
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
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteStream(BlobHandle, Stream);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; var Str: string);
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteString(BlobHandle, Str);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; Buffer: Pointer; Size: Word);
var
  BlobHandle: IscBlobHandle;
begin
  if FCurrentState < qsTransaction then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
    try
      BlobWriteSegment(BlobHandle, Size, Buffer);
    finally
      BlobClose(BlobHandle);
    end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBStatement.InternalReadBlob(SqlDa: TSQLDA; const Index: Word;
  Stream: TStream);
var
  BlobHandle: IscBlobHandle;
begin
  if not SqlDa.IsBlob[Index] then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if not SqlDa.IsNull[Index] then
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, SqlDa.AsQuad[Index]);
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

procedure TJvUIBStatement.InternalReadBlob(SqlDa: TSQLDA; const Index: Word;
  var Str: string);
var
  BlobHandle: IscBlobHandle;
begin
  if not SqlDa.IsBlob[Index] then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if SqlDa.IsNull[Index] then
    Str := ''
  else
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, SqlDa.AsQuad[Index]);
      try
        BlobReadString(BlobHandle, Str);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TJvUIBStatement.InternalReadBlob(SqlDa: TSQLDA; const Index: Word;
  var Value: Variant);
var
  BlobHandle: IscBlobHandle;
begin
  if not SqlDa.IsBlob[Index] then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if not SqlDa.IsNull[Index] then
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, SqlDa.AsQuad[Index]);
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

procedure TJvUIBStatement.InternalClose(const Mode: TEndTransMode;
  Auto: Boolean);
begin
  case FCurrentState of
    qsStatement:
      EndStatement(Mode, Auto);
    qsExecImme:
      EndExecImme(Mode, Auto);
    qsPrepare:
      EndPrepare(Mode, Auto);
    qsExecute:
      EndExecute(Mode, Auto);
  end;
end;

procedure TJvUIBStatement.InternalGetBlobSize(SqlDa: TSQLDA; const Index: Word;
  out Size: Cardinal);
var
  BlobHandle: IscBlobHandle;
begin
  if not SqlDa.IsBlob[Index] then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if not SqlDa.IsNull[Index] then
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, SqlDa.AsQuad[Index]);
      try
        BlobSize(BlobHandle, Size);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

function TJvUIBStatement.FieldBlobSize(const Index: Word): Cardinal;
begin
  if Fields.FetchBlobs then
    Result := Fields.GetBlobSize(Index)
  else
    InternalGetBlobSize(Fields, Index, Result);
end;

function TJvUIBStatement.ParamBlobSize(const Index: Word): Cardinal;
begin
  InternalGetBlobSize(Params, Index, Result);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Buffer: Pointer);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Buffer)
  else
    InternalReadBlob(Fields, Index, Buffer);
end;

procedure TJvUIBStatement.ReadBlob(const Name: string; Buffer: Pointer);
begin
  ReadBlob(Fields.GetFieldIndex(Name), Buffer);
end;

procedure TJvUIBStatement.InternalReadBlob(SqlDa: TSQLDA;
  const Index: Word; Buffer: Pointer);
var
  BlobHandle: IscBlobHandle;
begin
  if not SqlDa.IsBlob[Index] then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if SqlDa.IsNull[Index] then
    Exit
  else
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, SqlDa.AsQuad[Index]);
      try
        BlobReadSizedBuffer(BlobHandle, Buffer);
      finally
        BlobClose(BlobHandle);
      end;
    finally
      UnLock;
    end;
  end;
end;

function TJvUIBStatement.GetRowsAffected: Cardinal;
begin
  Result := 0;
  Lock;
  try
    if FCurrentState < qsPrepare then
      raise Exception.Create(EUIB_MUSTBEPREPARED)
    else
      Result := FindDataBase.FLibrary.DSQLInfoRowsAffected(FStHandle, FStatementType);
  finally
    UnLock
  end;
end;

//=== TJvUIBQuery ============================================================

procedure TJvUIBQuery.BuildStoredProc(const StoredProc: string);
var
  I, R: Integer;
  Str: string;
begin
  InternalClose(etmStayIn, True);
  R := 0;
  FSQL.OnChange := nil;
  try
    Params.Clear;
    FParsedSQL :=
      'SELECT RDB$FIELD_TYPE, RDB$PARAMETER_NAME, RDB$FIELD_SCALE, RDB$PARAMETER_TYPE ' +
      'FROM RDB$PROCEDURE_PARAMETERS PRM JOIN RDB$FIELDS FLD ON ' +
      'PRM.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME ' +
      'WHERE ' +
      'PRM.RDB$PROCEDURE_NAME = ''' + UpperCase(StoredProc) + ''' ' +
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
                blr_short:
                  Params.AddFieldType(Trim(AsString[1]), uftNumeric, -AsSmallint[2], 4);
                blr_long:
                  Params.AddFieldType(Trim(AsString[1]), uftNumeric, -AsSmallint[2], 7);
                blr_int64, blr_quad, blr_double:
                  Params.AddFieldType(Trim(AsString[1]), uftNumeric, -AsSmallint[2], 15);
              else
                raise Exception.Create(EUIB_UNEXPECTEDERROR);
              end;
            end
            else
              case Fields.AsSmallint[0] of
                blr_text, blr_text2, blr_varying, blr_varying2,
                blr_cstring, blr_cstring2:
                  Params.AddFieldType(Trim(AsString[1]), uftChar);
                blr_float, blr_d_float:
                  Params.AddFieldType(Trim(AsString[1]), uftFloat);
                blr_short:
                  Params.AddFieldType(Trim(AsString[1]), uftSmallint);
                blr_long:
                  Params.AddFieldType(Trim(AsString[1]), uftInteger);
                blr_quad:
                  Params.AddFieldType(Trim(AsString[1]), uftQuad);
                blr_double:
                  Params.AddFieldType(Trim(AsString[1]), uftDoublePrecision);
                blr_timestamp:
                  Params.AddFieldType(Trim(AsString[1]), uftTimestamp);
                blr_blob, blr_blob_id:
                  Params.AddFieldType(Trim(AsString[1]), uftBlob);
                blr_sql_date:
                  Params.AddFieldType(Trim(AsString[1]), uftDate);
                blr_sql_time:
                  Params.AddFieldType(Trim(AsString[1]), uftTime);
                blr_int64:
                  Params.AddFieldType(Trim(AsString[1]), uftInt64);
                {$IFDEF IB7_UP}
                blr_boolean_dtype:
                  Params.AddFieldType(Trim(AsString[1]), uftBoolean);
                {$ENDIF IB7_UP}
              else
                // shouldn't occur but ...
                raise Exception.Create('Unknown field type.');
              end
          end
          else
            Inc(R);
        Next;
      end;
      if Params.FieldCount > 0 then
      begin
        FParsedSQL := ' (';
        Str := ' (';
        for I := 0 to Params.FieldCount - 1 do
        begin
          FParsedSQL := FParsedSQL + '?,';
          Str := Str + ':' + Params.FieldName[I] + ','
        end;
        FParsedSQL[Length(FParsedSQL)] := ')';
        Str[Length(Str)] := ')';
        if R > 0 then
        begin
          FParsedSQL := 'SELECT * FROM ' + StoredProc + FParsedSQL;
          SQL.Text := 'SELECT * FROM ' + StoredProc + Str;
        end
        else
        begin
          FParsedSQL := 'EXECUTE PROCEDURE ' + StoredProc + FParsedSQL;
          SQL.Text := 'EXECUTE PROCEDURE ' + StoredProc + Str;
        end;
      end
      else
      begin
        if R > 0 then
          FParsedSQL := 'SELECT * FROM ' + StoredProc
        else
          FParsedSQL := 'EXECUTE PROCEDURE ' + StoredProc;
        SQL.Text := FParsedSQL;
      end;
    except
      FParsedSQL := '';
      Params.Clear;
      InternalClose(FOnError, False);
      raise;
    end;
  finally
    InternalClose(etmStayIn, True);
    FSQL.OnChange := DoSQLChange;
  end;
end;

//=== TJvUIBTransaction ======================================================

constructor TJvUIBTransaction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [tpConcurrency, tpWait, tpWrite];
  FTrHandle := nil;
  FStatements := 0;
  FDataBases := TList.Create;
  FAutoRetain := False;
  FAutoStart := True;
  FAutoStop := True;
  FDefaultAction := etmCommit;
end;

destructor TJvUIBTransaction.Destroy;
begin
  ClearSQLComponents;
  Close(etmDefault, True);
  ClearDataBases;
  FDataBases.Free;
  inherited Destroy;
end;

procedure TJvUIBTransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TJvUIBDataBase) and (Operation = opRemove) then
    RemoveDatabase(TJvUIBDataBase(AComponent));
end;

procedure TJvUIBTransaction.SetDataBase(const ADatabase: TJvUIBDataBase);
begin
  RemoveDatabase(FDataBase);
  AddDataBase(ADatabase);
  FDataBase := ADatabase;
end;

procedure TJvUIBTransaction.Close(const Mode: TEndTransMode; Auto: Boolean);
var
  I: Integer;
begin
  Lock;
  try
    if (FStatements > 0) and (FSQLComponent <> nil) then
      for I := 0 to FSQLComponent.Count - 1 do
        TJvUIBQuery(FSQLComponent.Items[I]).InternalClose(etmStayIn, Auto);
  finally
    UnLock;
  end;
  EndTransaction(Mode, nil, Auto);
end;

function TJvUIBTransaction.GetStatements(const Index: Integer): TJvUIBStatement;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Items[Index]
  else
    raise EListError.CreateFmt(EUIB_INDEXERROR, [Index]);
end;

function TJvUIBTransaction.GetStatementsCount: Integer;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Count
  else
    Result := 0;
end;

procedure TJvUIBTransaction.ClearDataBases;
var
  I: Integer;
begin
  FDataBase := nil;
  for I := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[I]).RemoveTransaction(Self);
  FDataBases.Clear;
end;

function TJvUIBTransaction.GetDatabases(const Index: Integer): TJvUIBDataBase;
begin
  Result := FDataBases[Index];
end;

function TJvUIBTransaction.GetDatabasesCount: Integer;
begin
  Result := FDataBases.Count;
end;

procedure TJvUIBTransaction.BeginDataBase;
var
  I: Integer;
begin
  if FDataBase = nil then
    raise Exception.Create(EUIB_DATABASENOTDEF);
  for I := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[I]).Connected := True;
end;

procedure TJvUIBTransaction.BeginTransaction(Auto: Boolean = True);
type
  TEBDynArray = array of TISCTEB;
var
  Buffer: Pointer;
  I: Integer;
  ATPB: string;
begin
  BeginDataBase;
  Lock;
  try
    with FDataBase.FLibrary do
      if FTrHandle = nil then
      begin
        if Auto and (not FAutoStart) then
          raise EUIBException.Create(EUIB_EXPLICITTRANS);

        if FDataBases.Count = 1 then
          TransactionStart(FTrHandle, FDataBase.FDbHandle, TPB)
        else
        begin
          GetMem(Buffer, SizeOf(TISCTEB) * FDataBases.Count);
          try
            ATPB := TPB;
            for I := 0 to FDataBases.Count - 1 do
              with TEBDynArray(Buffer)[I] do
              begin
                Handle := @TJvUIBDatabase(FDataBases[I]).FDbHandle;
                Len := Length(ATPB);
                Address := PChar(ATPB);
              end;
            TransactionStartMultiple(FTrHandle, FDataBases.Count, Buffer);
          finally
            FreeMem(Buffer);
          end;
        end;
        if Assigned(FOnStartTransaction) then
          FOnStartTransaction(Self);
      end;
  finally
    UnLock;
  end;
end;

function TJvUIBTransaction.EndTransaction(ETM: TEndTransMode;
  From: TJvUIBStatement; Auto: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  // don't lock if it is not necessary
  if ETM = etmStayIn then
    Exit;
  Lock;
  try
    // Default Action
    if ETM = etmDefault then
      ETM := FDefaultAction;
    if FTrHandle <> nil then
      with FDataBase.FLibrary do
      try
        if Assigned(FOnEndTransaction) then
          FOnEndTransaction(Self, ETM);
        { If there are Statements alive I must keep handle only if FAutoRetain = True.}
        if (FStatements > 0) and FAutoRetain then
          case ETM of
            etmCommit:
              ETM := etmCommitRetaining;
            etmRollback:
              ETM := etmRollbackRetaining;
          end
        else
        if ETM in [etmCommit, etmRollback] then
          if (FStatements > 0) and (FSQLComponent <> nil) then
            for I := 0 to FSQLComponent.Count - 1 do
              if From <> FSQLComponent.Items[I] then
                TJvUIBQuery(FSQLComponent.Items[I]).InternalClose(etmStayIn, Auto);

        Assert(FAutoStop or not Auto, EUIB_NOAUTOSTOP);

        case ETM of
          etmCommit:
            begin
              TransactionCommit(FTrHandle);
              Result := True;
            end;
          etmCommitRetaining:
            TransactionCommitRetaining(FTrHandle);
          etmRollback:
            begin
              TransactionRollback(FTrHandle);
              Result := True;
            end;
          etmRollbackRetaining:
            TransactionRollbackRetaining(FTrHandle);
        end;
      except
        case ETM of
          etmCommit, etmRollback:
            TransactionRollback(FTrHandle);
          etmCommitRetaining, etmRollbackRetaining:
            TransactionRollbackRetaining(FTrHandle);
        end;
        raise;
      end;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.AddSQLComponent(Component: TJvUIBStatement);
begin
  if FSQLComponent = nil then
    FSQLComponent := TList.Create;
  FSQLComponent.Add(Component);
end;

procedure TJvUIBTransaction.ClearSQLComponents;
begin
  while (FSQLComponent <> nil) do
    TJvUIBQuery(FSQLComponent.Last).SetTransaction(nil);
end;

procedure TJvUIBTransaction.RemoveSQLComponent(Component: TJvUIBStatement);
begin
  if FSQLComponent <> nil then
  begin
    FSQLComponent.Remove(Component);
    if FSQLComponent.Count = 0 then
      FreeAndNil(FSQLComponent);
  end;
end;

procedure TJvUIBTransaction.Lock;
var
  I: Integer;
begin
  inherited Lock;
  for I := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[I]).Lock;
end;

procedure TJvUIBTransaction.UnLock;
var
  I: Integer;
begin
  for I := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[I]).UnLock;
  inherited UnLock;
end;

procedure TJvUIBTransaction.AddDataBase(ADataBase: TJvUIBDataBase);
var
  I: Integer;
begin
  if ADataBase <> nil then
  begin
    for I := 0 to FDataBases.Count - 1 do
      if FDataBases[I] = ADataBase then
        Exit;
    Close(etmDefault, True);
    FDataBases.Add(ADataBase);
    ADataBase.AddTransaction(Self);
    FSQLDialect := ADatabase.SQLDialect;
  end;
end;

procedure TJvUIBTransaction.RemoveDatabase(ADataBase: TJvUIBDataBase);
var
  I: Integer;
begin
  if ADataBase <> nil then
  begin
    if ADataBase = FDataBase then
      FDataBase := nil;
    for I := 0 to FDataBases.Count - 1 do
      if FDataBases[I] = ADataBase then
      begin
        Close(etmDefault, True);
        ADataBase.RemoveTransaction(Self);
        FDataBases.Delete(I);
        Exit;
      end;
  end;
end;

procedure TJvUIBTransaction.RemoveDatabase(Index: Integer);
begin
  with TJvUIBDataBase(FDataBases[Index]) do
  begin
    Close(etmDefault, True);
    RemoveTransaction(Self);
    FDataBases.Delete(Index);
  end;
end;

procedure TJvUIBTransaction.Commit;
begin
  EndTransaction(etmCommit, nil, False);
end;

procedure TJvUIBTransaction.CommitRetaining;
begin
  EndTransaction(etmCommitRetaining, nil, False);
end;

procedure TJvUIBTransaction.RollBack;
begin
  EndTransaction(etmRollback, nil, False);
end;

procedure TJvUIBTransaction.RollBackRetaining;
begin
  EndTransaction(etmRollbackRetaining, nil, False);
end;

{$IFDEF IB71_UP}

procedure TJvUIBTransaction.SavepointRelease(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRelease(FTrHandle, Name);
end;

procedure TJvUIBTransaction.SavepointRollback(const Name: string; Option: Word = 0);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointRollback(FTrHandle, Name, Option);
end;

procedure TJvUIBTransaction.SavepointStart(const Name: string);
begin
  BeginTransaction;
  FDataBase.FLibrary.SavepointStart(FTrHandle, Name);
end;

{$ENDIF IB71_UP}

function TJvUIBTransaction.GetInTransaction: Boolean;
begin
  Lock;
  try
    Result := (FTrHandle <> nil);
  finally
    UnLock;
  end;
end;

function TJvUIBTransaction.TPB: string;
var
  TP: TTransParam;

  procedure ParseStrOption(const Code: Char; const Value: string);
  var
    P, Start: PChar;
    S: string;
  begin
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, ';']) do
          Inc(P);
        if (P - Start) > 0 then
        begin
          SetString(S, Start, P - Start);
          Result := Result + Code + Char(P - Start) + S;
        end;
        if P^ = ';' then
          Inc(P);
      end;
  end;

begin
  if FOptions = [tpConcurrency, tpWait, tpWrite] then
    Result := ''
  else
  begin
    Result := isc_tpb_version3;
    for TP := Low(TTransParam) to High(TTransParam) do
      if TP in FOptions then
      begin
        case TP of
          tpLockRead:
            ParseStrOption(Char(Ord(TP) + 1), FLockRead);
          tpLockWrite:
            ParseStrOption(Char(Ord(TP) + 1), FLockWrite);
        else
          Result := Result + Char(Ord(TP) + 1);
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

function TJvUIBTransaction.GetAutoRetain: Boolean;
begin
  Lock;
  try
    Result := FAutoRetain;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.SetAutoRetain(const Value: Boolean);
begin
  Lock;
  try
    FAutoRetain := Value;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.StartTransaction;
begin
  BeginTransaction(False);
end;

procedure TJvUIBTransaction.SetDefaultAction(const Value: TEndTransMode);
begin
  Assert(Value in [etmCommit, etmRollBack], 'Commit or Rollback only.');
  FDefaultAction := Value;
end;

//=== TJvUIBComponent ========================================================

constructor TJvUIBComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCriticalSection := TCriticalSection.Create;
end;

destructor TJvUIBComponent.Destroy;
begin
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TJvUIBComponent.Lock;
begin
  {$IFDEF UIBTHREADSAFE}
  FCriticalSection.Enter;
  {$ENDIF UIBTHREADSAFE}
end;

procedure TJvUIBComponent.UnLock;
begin
  {$IFDEF UIBTHREADSAFE}
  FCriticalSection.Leave;
  {$ENDIF UIBTHREADSAFE}
end;

//=== TJvUIBService ==========================================================

constructor TJvUIBService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLibrary := TUIBLibrary.Create;
  FLibraryName := GDS32DLL;
  FProtocol := proLocalHost;
  FHandle := nil;
end;

destructor TJvUIBService.Destroy;
begin
  inherited Destroy;
  FLibrary.Free;
end;

procedure TJvUIBService.BeginService;
begin
  FLibrary.Load(FLibraryName);
  case FProtocol of
    proLocalHost:
      FLibrary.ServiceAttach('service_mgr', FHandle, CreateSPB);
    proTCPIP:
      FLibrary.ServiceAttach(Fhost + ':service_mgr', FHandle, CreateSPB);
    proNetBUI:
      FLibrary.ServiceAttach('\\' + Fhost + '\service_mgr', FHandle, CreateSPB);
  end;
end;

function TJvUIBService.CreateSPB: string;

  procedure AddString(ID: Char; var Str: string);
  begin
    if Str <> '' then
      Result := Result + ID + Char(Length(Str)) + Str;
  end;

begin
  Result := isc_spb_version + isc_spb_current_version;
  AddString(isc_spb_user_name, FUserName);
  AddString(isc_spb_password, FPassWord);
end;

procedure TJvUIBService.SetLibraryName(const Lib: string);
begin
  FLibrary.UnLoad;
  FLibraryName := Lib;
end;

procedure TJvUIBService.EndService;
begin
  FLibrary.ServiceDetach(FHandle);
end;

//=== TJvUIBBackupRestore ====================================================

constructor TJvUIBBackupRestore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackupFiles := TStringList.Create;
end;

destructor TJvUIBBackupRestore.Destroy;
begin
  FBackupFiles.Free;
  inherited Destroy;
end;

function TJvUIBBackupRestore.GetBackupFiles: TStrings;
begin
  Result := FBackupFiles;
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
      while True do
      begin
        FLibrary.ServiceQuery(FHandle, '', isc_info_svc_line, Buffer);
        if Buffer[1] <> isc_info_svc_line then
          raise Exception.Create(EUIB_UNEXPECTEDERROR);
        Len := PWord(@Buffer[2])^;
        if Len > 0 then
          FOnVerbose(Self, Copy(Buffer, 4, Len))
        else
          Break;
      end;
    end;
  finally
    EndService;
  end;
end;

//=== TJvUIBBackup ===========================================================

function TJvUIBBackup.CreateStartSPB: string;
var
  Len: Word;
  I: Integer;
  FileName: string;
  FileLength: Integer;

  function GetValue(Index: Integer): string;
  begin
    if Index >= 0 then
      Result := Copy(BackupFiles.Strings[Index], Length(BackupFiles.Names[Index]) + 2, MaxInt)
    else
      Result := '';
  end;

begin
  // backup service   ibservices
  Result := isc_action_svc_backup;

  // DB Name
  Result := Result + isc_spb_dbname;
  Len := Length(FDatabase);
  // (rom) this is a really strange and bad expression
  // (rom) replace with small helper function
  Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
  Result := Result + FDatabase;

  for I := 0 to BackupFiles.Count - 1 do
  begin
    FileName := BackupFiles.Names[I];
    if FileName = '' then
      FileName := BackupFiles[I];
    if FileName <> '' then
    begin
      // Backup file
      Result := Result + isc_spb_bkp_file;
      Len := Length(FileName);
      Result := Result + PChar(@Len)[0] + PChar(@Len)[1];
      Result := Result + FileName;
      // Backup file length
      if TryStrToInt(GetValue(I), FileLength) then
      begin
        Result := Result + isc_spb_bkp_length;
        Result := Result + PChar(@FileLength)[0] + PChar(@FileLength)[1] +
          PChar(@FileLength)[2] + PChar(@FileLength)[3];
      end;
    end;
  end;

  if Assigned(FOnVerbose) then
    Result := Result + isc_spb_verbose;

  if FOptions <> [] then
    Result := Result + isc_spb_options + PChar(@FOptions)^ + #0#0#0;
end;

//=== TJvUIBRestore ==========================================================

constructor TJvUIBRestore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [roCreateNewDB];
  FPageSize := 0;
end;

function TJvUIBRestore.CreateStartSPB: string;
var
  Len: Word;
  I: Integer;
  FileName: string;
  Opts: Cardinal;
begin
  // backup service   ibservices
  Result := isc_action_svc_restore;

  for I := 0 to BackupFiles.Count - 1 do
  begin
    FileName := BackupFiles[I];
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

  if Assigned(FOnVerbose) then
    Result := Result + isc_spb_verbose;

  if FOptions <> [] then
  begin
    Opts := PByte(@FOptions)^ shl 8;
    Result := Result + isc_spb_options + PChar(@Opts)[0] +
      PChar(@Opts)[1] + PChar(@Opts)[2] + PChar(@Opts)[3];
  end;

  if FPageSize > 0 then
    Result := Result + isc_spb_res_page_size + PChar(@FPageSize)[0] +
      PChar(@FPageSize)[1] + PChar(@FPageSize)[2] + PChar(@FPageSize)[3];
end;

//=== TJvUIBScript ===========================================================

constructor TJvUIBScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FQuery := TJvUIBQuery.Create(nil);
  FQuery.ParseParams := False;
  FScript := TStringList.Create;
  FAutoDDL := True;
end;

destructor TJvUIBScript.Destroy;
begin
  FQuery.Free;
  FScript.Free;
  inherited Destroy;
end;

procedure TJvUIBScript.ExecuteScript;
var
  FStream: TStringStream;
  Lexer: TUIBLexer;
  Grammar: TUIBGrammar;
  I, K: Integer;
  J: TCharacterSet;
  Dialect: Integer;
  TrHandle: IscTrHandle;

  procedure CheckDatabase;
  begin
    if Transaction = nil then
      raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
  end;

  function Statement: string;
  var
    P: Integer;
  begin
    with Grammar.RootNode.Nodes[I] do
    begin
      P := PosFrom.Pos;
      FStream.Seek(P, soFromBeginning);
      Result := FStream.ReadString(PosTo.Pos - P);
    end;
  end;

begin
  FStream := TStringStream.Create(Script.Text);
  Lexer := TUIBLexer.Create(FStream);
  Grammar := TUIBGrammar.Create(Lexer);
  try
    if (Grammar.yyparse = 0) and (Grammar.RootNode <> nil) then
    begin
      for I := 0 to Grammar.RootNode.NodesCount - 1 do
      begin
        if Assigned(FOnParse) then
          FOnParse(Self, Grammar.RootNode.Nodes[I].NodeType, Statement, I,
            Grammar.RootNode.NodesCount);
        case Grammar.RootNode.Nodes[I].NodeType of
          NodeSetSqlDialect:
            begin
              CheckDatabase;
              if TryStrToInt(Grammar.RootNode.Nodes[I].Value, Dialect) then
                FQuery.FindDataBase.SQLDialect := Dialect
              else
                raise Exception.Create(EUIB_PARSESQLDIALECT);
            end;
          NodeSetNames:
            begin
              CheckDatabase;
              // (rom) this loop looks buggy. It throws an exception for the second element
              for J := Low(TCharacterSet) to High(TCharacterSet) do
              begin
                if CompareText(CharacterSetStr[J], Grammar.RootNode.Nodes[I].Value) = 0 then
                begin
                  FQuery.FindDataBase.CharacterSet := J;
                  Break;
                end;
                raise Exception.Create(EUIB_PARSESETNAMES);
              end;
            end;
          NodeCreateDatabase:
            begin
              CheckDatabase;
              FQuery.FindDataBase.Connected := False;
              TrHandle := nil;
              with FQuery.FindDataBase do
              begin
                FLibrary.Load(FLibraryName);
                // I MUST provide the real DB Handle (not nil)
                // because altering foreign key can fail otherwise.
                FQuery.FindDataBase.Lock;
                try
                  FLibrary.DSQLExecuteImmediate(FDbHandle,
                    TrHandle, Statement, SQLDialect);
                finally
                  FQuery.FindDataBase.UnLock;
                end;
              end;
              with Grammar.RootNode.Nodes[I].Nodes[0] do
                for K := 0 to NodesCount - 1 do
                  case Nodes[K].NodeType of
                    NodeName:
                      FQuery.FindDataBase.DatabaseName :=
                        Copy(Nodes[K].Value, 2, Length(Nodes[K].Value) - 2);
                    NodeUsername:
                      FQuery.FindDataBase.UserName :=
                        Copy(Nodes[K].Value, 2, Length(Nodes[K].Value) - 2);
                    NodePassWord:
                      FQuery.FindDataBase.PassWord :=
                        Copy(Nodes[K].Value, 2, Length(Nodes[K].Value) - 2);
                  end;
            end;
          NodeConnect:
            with FQuery.FindDataBase do
            begin
              Connected := False;
              with Grammar.RootNode.Nodes[I] do
              begin
                DatabaseName := Copy(Nodes[0].Value, 2, Length(Nodes[0].Value) - 2);
                UserName := Copy(Nodes[1].Value, 2, Length(Nodes[1].Value) - 2);
                PassWord := Copy(Nodes[2].Value, 2, Length(Nodes[2].Value) - 2);
              end;
              Connected := True;
            end;
          NodeSetAutoDDL:
            begin
              if UpperCase(Grammar.RootNode.Nodes[I].Value) = 'ON' then
                FAutoDDL := True
              else
              if UpperCase(Grammar.RootNode.Nodes[I].Value) = 'OFF' then
                FAutoDDL := False
              else
                raise Exception.Create(EUIB_BADAUTODLL);
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
            Transaction.SavepointStart(Grammar.RootNode.Nodes[I].Nodes[0].Value);
          NodeSavepointRelease:
            Transaction.SavepointRelease(Grammar.RootNode.Nodes[I].Nodes[0].Value);
          NodeSavepointUndo:
            Transaction.SavepointRollback(Grammar.RootNode.Nodes[I].Nodes[0].Value);
          {$ENDIF IB71_UP}
          NodeSelect, // perhaps a select statement execute a procedure ...
          NodeInsert, NodeDeleteSearched, NodeDeletePositioned,
          NodeUpdateSearched, NodeUpdatePositioned:
            begin
              FQuery.SQL.Text := Trim(Statement);
              FQuery.ExecSQL;
              FQuery.Close(etmStayIn);
            end;
        else
          // DDL ...
          FQuery.SQL.Text := Trim(Statement);
          FQuery.ExecSQL; // faster for ddl
          if FAutoDDL then
            FQuery.Close(etmCommit)
          else
            FQuery.Close(etmStayIn);

        end;
      end;
    end
    else
      raise EUIBParser.Create(Lexer.yylineno, Lexer.yycolno);
  finally
    FQuery.Close(etmStayIn);
    //Transaction.Commit;
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

function TJvUIBScript.GetScript: TStrings;
begin
  Result := FScript;
end;

procedure TJvUIBScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TJvUIBScript.SetTransaction(const Value: TJvUIBTransaction);
begin
  FQuery.Transaction := Value;
end;

//=== TMetaDataOptions =======================================================

constructor TMetaDataOptions.Create;
begin
  // (rom) added inherited Create
  inherited Create;
  FObjects := ALLOBjects;
  FTables := ALLTables;
  FViews := ALLViews;
  FProcedures := ALLProcedures;
  FUDFs := ALLUDFs;
  FSysInfos := False;
end;

end.

