{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2003 of these individuals.                                                   }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

{ @abstract(UIB Visual components.)
  @author(Henri Gourvest: hgourvest@progdigy.com)
  @lastmod(Jan 16, 2003)}

unit JvQUIB;

{$I jvcl.inc}
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
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}  JvQComponent, 
  Classes, SysUtils, SyncObjs, JvQUIBLib, JvQUIBase, JvQUIBSQLParser, JvQUIBConst;

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

{ All UIB components inherith from this class to encapsulate Critical Sections.
  Critical Sections make UIB THread Safe. }

TJvUIBComponent = class(TJvComponent)

  private
    FCriticalsection: TCriticalSection;
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
    FSysInfos: boolean;
  public
   { @exclude }
    constructor Create;
  published
    { Metadata objects (Procedure, Generator, Exception, UDF, Role). }
    property Objects: TOIDDatabases read FObjects write FObjects default ALLObjects;
    { Table properties (TableField, Primary, Foreign, TableTrigger, Unique, Index, Check)}
    property Tables: TOIDTables read FTables write FTables default ALLTables;
    { View properties (Fields & Triggers)}
    property Views: TOIDViews read FViews write FViews default AllViews;
    { Procedure properties (input & output parametters). }
    property Procedures: TOIDProcedures read FProcedures write FProcedures default AllProcedures;
    { UDFs properties (Fields). }
    property UDFs: TOIDUDFs read FUDFs write FUDFs default AllUDFs;
    { Include System tables, triggers and domains. }
    property SysInfos: boolean read FSysInfos write FSysInfos default False;
  end;

  TJvUIBDataBase = class(TJvUIBComponent)
  private
    FLibrary: TUIBLibrary;
    FLiBraryName: TFileName;
    FDbHandle: IscDbHandle;
    FHandleShared: boolean;
    FParams: TStrings;
    FDatabaseName: TFileName;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FTransactions: TList;
    FOnConnectionLost: TNotifyEvent;
    FExceptions: TList;
    FMetadata: TObject;
    FMetaDataOptions: TMetaDataOptions;
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
    function GetMetadata(Refresh: boolean = False): TObject;
    { The DbHandle can be used to share the current connection with other Interbase components like IBX. }
    property DbHandle: IscDbHandle read FDbHandle write SetDbHandle;
    { Determine if the DbHandle is initialized by another component. }
    property IsHandleShared : Boolean read FHandleShared;
    { List all transactions connected to the database component. }
    property Transactions[const Index: Cardinal]: TJvUIBTransaction read GetTransactions;
    { Number of connected transactions. }
    property TransactionsCount: Cardinal read GetTransactionsCount;
    { Can be used to access the low level API. }
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
    property LibraryName: TFileName read FLiBraryName write SetLibraryName;
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
    property SegmentSize: Word read GetSegmentSize write SetSegmentSize default 16*1024;
    { The list of MetaData Objects returned by GetMetadata. }
    property MetaDataOptions: TMetaDataOptions read FMetaDataOptions;
  end;

  { Describe how a transaction is closed. }
  TEndTransMode = (
    etmDefault,          // Use default Transaction Action
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
    FOptions   : TTransParams;
    FLockRead  : string;
    FLockWrite : string;
    FSQLDialect: Integer;
    FOnStartTransaction: TNotifyEvent;
    FOnEndTransaction: TOnEndTransaction;
    FAutoRetain: boolean;
    FAutoStart: boolean;
    FAutoStop: boolean;
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
    procedure BeginTransaction(Auto: boolean = True);
    function EndTransaction(ETM: TEndTransMode; From: TJvUIBStatement;
      Auto: boolean): boolean;
    procedure AddSQLComponent(Component: TJvUIBStatement);
    procedure RemoveSQLComponent(Component: TJvUIBStatement);
    procedure ClearSQLComponents;
    procedure Close(const Mode: TEndTransMode; Auto: boolean);
    function GetStatements(const Index: Integer): TJvUIBStatement;
    function GetStatementsCount: Integer;
    procedure ClearDataBases;
    function GetDatabases(const Index: Integer): TJvUIBDataBase;
    function GetDatabasesCount: Integer;
    function GetAutoRetain: boolean;
    procedure SetAutoRetain(const Value: boolean);
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
    Procedure StartTransaction;
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
{$ENDIF}
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
    {If false, commit and rollback close all connected statements and finally close transaction.
     If True, commit and rollback are modified to commitretaining or rollbackretaining if at least one statement is open.}
    property AutoRetain: boolean read GetAutoRetain write SetAutoRetain default False;
    {If True, transaction automatically started when needed.
     if False you must explicitely call "starttransaction".}
    property AutoStart: boolean read FAutoStart write FAutoStart default True;
    {default = false, if True you need to close transaction explicitly.}
    property AutoStop: boolean read FAutoStop write FAutoStop default True;
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
    FCachedFetch: boolean;
    FFetchBlobs: boolean;
    FBufferChunks: Cardinal;
    FQuickScript: boolean;
    FSQL: TStrings;
    FParsedSQL: string;
    FParameter: TSQLParams;
    FParseParams: boolean;
    FOnClose: TNotifyEvent;
    FStatementType: TUIBStatementType;
    FUseCursor: boolean;
    function GetPlan: string;
    function GetStatementType: TUIBStatementType;
    procedure SetSQL(const Value: TStrings);
    procedure DoSQLChange(Sender: TObject);
    function GetFields: TSQLResult;
    function GetEof: boolean;
    function FindDataBase: TJvUIBDataBase;
    function GetRowsAffected: Cardinal;
    function GetBof: boolean;
  protected
    procedure SetTransaction(const Transaction: TJvUIBTransaction); virtual;
    procedure SetDataBase(ADataBase: TJvUIBDataBase);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure BeginTransaction; virtual;
    procedure BeginStatement; virtual;
    procedure BeginPrepare; virtual;
    procedure BeginExecute; virtual;
    procedure BeginExecImme; virtual;

    procedure EndTransaction(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndStatement(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndPrepare(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndExecute(const ETM: TEndTransMode; Auto: boolean); virtual;
    procedure EndExecImme(const ETM: TEndTransMode; Auto: boolean); virtual;

    procedure InternalNext; virtual;
    procedure InternalPrior; virtual;
    procedure InternalClose(const Mode: TEndTransMode; Auto: boolean); virtual;

    function  ParamsClass: TSQLParamsClass; virtual;
    function  ResultClass: TSQLResultClass; virtual;

    procedure InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Stream: TStream); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var str: string); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; var Value: Variant); overload;
    procedure InternalReadBlob(sqlda: TSQLDA; const Index: Word; Buffer: Pointer); overload;

    property QuickScript: boolean read FQuickScript write FQuickScript  default False;

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
    procedure CloseCursor;
    procedure FetchAll;
    { Open the query and fetch the first record if FetchFirst = true. }
    procedure Open(FetchFirst: boolean = True);
    { Prepare the query. }
    procedure Prepare;
    { Execute the query. }
    procedure Execute;
    { Execute the query or the script (QuickScript = true) immediately. }
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
    procedure ReadBlob(const Index: Word; var str: string); overload;
    { Read a the blob in a Variant by index. }
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by index. }
    procedure ReadBlob(const Index: Word; Buffer: Pointer); overload;
    { Read a the blob in a stream by name. }
    procedure ReadBlob(const name: string; Stream: TStream); overload;
    { Read a the blob in a string by name. }
    procedure ReadBlob(const name: string; var str: string); overload;
    { Read a the blob in a Variant by name. }
    procedure ReadBlob(const name: string; var Value: Variant); overload;
    { Read a the blob in a PREALLOCATED buffer by name. }
    procedure ReadBlob(const name: string; Buffer: Pointer); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Index: Word; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Index: Word; var str: string); overload;
    { The the blob value of a parametter using a Buffer. }
    procedure ParamsSetBlob(const Index: Word; Buffer: Pointer; Size: Word); overload;

    { The the blob value of a parametter using a Stream. }
    procedure ParamsSetBlob(const Name: string; Stream: TStream); overload;
    { The the blob value of a parametter using a string. }
    procedure ParamsSetBlob(const Name: string; var str: string); overload;
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
    { if true there isn't anymore record to fetch. }
    property Eof: boolean read GetEof;
    property Bof: boolean read GetBof;
    { @exclude }
    property ParseParams: boolean read FParseParams write FParseParams;
    { The plan used internally by interbase (the query must be prepared). }
    property Plan: string read GetPlan;
    { Get the current statement type (the query must be prepared). }
    property StatementType: TUIBStatementType read GetStatementType;
    { Return the number of rows affected by the query (stInsert, stUpdate or stDelete). }
    property RowsAffected: Cardinal read GetRowsAffected;
    property UseCursor: boolean read FUseCursor write FUseCursor default True;
  published
    { The sql query. }
    property SQL: TStrings read FSQL write SetSQL;
    { Transaction of the query. }
    property Transaction: TJvUIBTransaction read FTransaction write SetTransaction;
    { Connected database, in most cases you don't need to set this property, it is
      only needed if the transaction concern more than one database. }
    property DataBase: TJvUIBDataBase read FDataBase write SetDataBase;
    { If an error occur, this action is applied to the connected transaction. }
    property OnError: TEndTransMode read FOnError write FOnError default etmRollback;
    { If true all record are saved in memory. }
    property CachedFetch: boolean read FCachedFetch write FCachedFetch default True;
    { If true the blob data is fetched with the record. }
    property FetchBlobs: boolean read FFetchBlobs write FFetchBlobs default False;
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
    { If true you can use this component as a fast script component where each line is a query.
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
    FScript: TStrings;
    FAutoDDL: boolean;
    FOnParse: TOnParse;
    procedure SetTransaction(const Value: TJvUIBTransaction);
    function GetTransaction: TJvUIBTransaction;
    procedure SetScript(const Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
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
    proNetBEUI
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
    constructor Create(AOwner: TComponent); override;
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
    constructor Create(AOwner: TComponent); override;
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
    constructor Create(AOwner: TComponent); override;
  published
    property Options: TRestoreOptions read FOptions write FOptions default [roCreateNewDB];
    property PageSize: Cardinal read FPageSize write FPageSize default 0;
  end;

implementation
uses JvQUIBMetaData, Math;

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = record
    ExepClass: EUIBExceptionClass;
    ID: Integer;
  end;

{ TJvUIBDataBase }

procedure TJvUIBDataBase.AddTransaction(Transaction: TJvUIBTransaction);
begin
  if (FTransactions = nil) then FTransactions := TList.Create;
  FTransactions.Add(Transaction);
end;

procedure TJvUIBDataBase.ClearTransactions;
begin
  while (FTransactions <> nil) do
    TJvUIBTransaction(FTransactions.Last).RemoveDatabase(Self); 
end;

procedure TJvUIBDataBase.CloseTransactions;
var i: Integer;
begin
  if (FTransactions <> nil) then
    for i := 0 to FTransactions.Count - 1 do
      TJvUIBTransaction(FTransactions.Items[i]).Close(etmDefault, True);
end;

constructor TJvUIBDataBase.Create(AOwner: TComponent);
begin
  inherited;
  FLibrary := TUIBLibrary.Create;
  FLiBraryName := GetClientLibrary;
  FLibrary.OnConnectionLost := DoOnConnectionLost;
  FLibrary.OnGetDBExceptionClass := DoOnGetDBExceptionClass;
  FDbHandle := nil;
  FHandleShared := False;
  FParams := TStringList.Create;
  SQLDialect := 3;
  CharacterSet := csNONE;
  WriteParamString('sql_role_name', '');
  FExceptions := TList.Create;
  FMetadata := nil;
  FMetaDataOptions := TMetaDataOptions.Create;
end;

destructor TJvUIBDataBase.Destroy;
begin
  Lock;
  try
    Connected := False;
    ClearTransactions;
    TStringList(FParams).Free;
    ClearExceptions;
    FExceptions.Free;
    FLibrary.Free;
    FMetaDataOptions.Free;
  finally
    UnLock;
  end;
  inherited;
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

procedure TJvUIBDataBase.CreateDatabase(PageSize: Integer = 2048);
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
    begin
      FTransactions.Free;
      FTransactions := nil;
    end;
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
            AttachDatabase(FDatabaseName, FDbHandle, FParams.Text, BreakLine);
          if Assigned(AfterConnect) then AfterConnect(Self);
        end;
      False :
        begin
          if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
          CloseTransactions;
          if FMetadata <> nil then
            FreeAndNil(FMetadata);
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
  if (csDesigning in ComponentState) then
    Connected := False;
end;

procedure TJvUIBDataBase.SetDbHandle(const Value: IscDbHandle);
begin
  if (FDbHandle = nil) or ((FDbHandle <> nil) and FHandleShared) then
  begin
    FLibrary.Load(FLiBraryName);
    FDbHandle := Value;
    FHandleShared := (FDbHandle <> nil);
  end else
    raise Exception.Create(EUIB_DBHANDLEALLREADYSET);
end;

procedure TJvUIBDataBase.SetLibraryName(const Lib: TFileName);
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

procedure TJvUIBDataBase.ClearExceptions;
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    FreeMem(FExceptions[i]);
  FExceptions.Clear;
end;

procedure TJvUIBDataBase.RegisterException(Excpt: EUIBExceptionClass;
  ID: Integer);
var
  ExcepInfo: PExceptionInfo;
  i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = ID then
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
    if (Result = - 1) then
      raise Exception.CreateFmt(EUIB_EXCEPTIONNOTFOUND, [Name]);
  finally
    Query.Free;
    Transaction.Free;
  end;
end;

procedure TJvUIBDataBase.UnRegisterException(Number: Integer);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if PExceptionInfo(FExceptions[i]).ID = Number then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
      Break;
    end;
end;

procedure TJvUIBDataBase.UnRegisterExceptions(Excpt: EUIBExceptionClass);
var i: Integer;
begin
  i := 0;
  while i < FExceptions.Count do
  begin
    if (PExceptionInfo(FExceptions[i]).ExepClass = Excpt) then
    begin
      FreeMem(FExceptions[i]);
      FExceptions.Delete(i);
    end else
    inc(i);
  end;
end;

procedure TJvUIBDataBase.DoOnGetDBExceptionClass(Number: Integer; out Excep: EUIBExceptionClass);
var i: Integer;
begin
  for i := 0 to FExceptions.Count - 1 do
    if (PExceptionInfo(FExceptions[i]).ID = Number) then
    begin
      Excep := PExceptionInfo(FExceptions[i]).ExepClass;
      Exit;
    end;
  Excep := EUIBException;
end;

function TJvUIBDataBase.GetMetadata(Refresh: boolean = False): TObject;
var
  Transaction: TJvUIBTransaction;
begin
  if Refresh and (FMetadata <> nil) then
    FreeAndNil(FMetadata);
  if (FMetadata = nil) then
  begin
    Transaction := TJvUIBTransaction.Create(nil);
    try
      Transaction.Database := Self;
      FMetadata := TMetaDataBase.Create(nil, -1);
      with TMetaDataBase(FMetadata) do
      begin
        OIDDatabases := FMetaDataOptions.Objects;
        OIDTables := FMetaDataOptions.Tables;
        OIDViews := FMetaDataOptions.Views;
        OIDProcedures := FMetaDataOptions.Procedures;
        OIDUDFs := FMetaDataOptions.UDFs;
        SysInfos := FMetaDataOptions.FSysInfos
      end;
      try
        TMetaDataBase(FMetadata).LoadFromDatabase(Transaction);
        Transaction.Commit;
      except
        FreeAndNil(FMetadata);
        raise;
      end;
    finally
      Transaction.Free;
    end;
  end;
  Result := FMetadata;
end;

function TJvUIBDataBase.GetSegmentSize: Word;
begin
  Result := FLibrary.SegMentSize;
end;

procedure TJvUIBDataBase.SetSegmentSize(const Value: Word);
begin
  FLibrary.SegMentSize := Value;
end;

{ TJvUIBStatement }

procedure TJvUIBStatement.SetTransaction(const Transaction: TJvUIBTransaction);
begin
  if (FTransaction <> Transaction) then
  begin
    if (FTransaction <> nil) then
    begin
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True) else
        InternalClose(etmStayIn, True);
      FTransaction.RemoveSQLComponent(Self);
    end;
    FTransaction := Transaction;
    if (Transaction <> nil) then
      Transaction.AddSQLComponent(Self);
    FCurrentState := qsDataBase;
  end;
end;

procedure TJvUIBStatement.SetDataBase(ADataBase: TJvUIBDataBase);
begin
  if (FDataBase <> ADataBase) then
  begin
    if (FTransaction <> nil) then
    begin
      if FTransaction.AutoRetain then
        InternalClose(etmDefault, True) else
        InternalClose(etmStayIn, True);
    end;
    FDataBase := ADataBase;
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
  InternalClose(Mode, False);
end;

procedure TJvUIBStatement.Open(FetchFirst: boolean = True);
begin
  // if you reopen the same query I Close
  // the cursor, clean sql result and
  // execute the query again to save
  // the prepare time !
  if (FCurrentState = qsExecute) then
    CloseCursor else
    InternalClose(etmStayIn, False);
  if FetchFirst then
    InternalNext else
    BeginExecute;
end;

procedure TJvUIBStatement.Next;
begin
  if (FCurrentState <> qsExecute) then
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
      with FindDataBase.FLibrary do
      try
        if FSQLResult.FetchBlobs then
          DSQLFetchWithBlobs(FindDataBase.FDbHandle,
            FTransaction.FTrHandle, FStHandle, FTransaction.FSQLDialect, FSQLResult) else
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
  end else
    raise Exception.Create(EUIB_CACHEDFETCHNOTSET);
end;

procedure TJvUIBStatement.EndTransaction(const ETM: TEndTransMode; Auto: boolean);
begin
  if FTransaction <> nil then
  begin
    if FTransaction.EndTransaction(ETM, Self, Auto) then
      FCurrentState := qsDataBase;
  end else
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
    inc(FTransaction.FStatements);
  finally
    UnLock;
  end;
  FCurrentState := qsStatement;
end;

procedure TJvUIBStatement.EndStatement(const ETM: TEndTransMode; Auto: boolean);
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
  if (ETM <> etmStayIn) then
    EndTransaction(ETM, Auto);

  if Assigned(FOnClose) then
    FOnClose(Self);    
end;

procedure TJvUIBStatement.BeginPrepare;
begin
  if (FStHandle = nil) then BeginStatement;
  FSQLResult := ResultClass.Create(0, FCachedFetch, FFetchBlobs, FBufferChunks);
  Lock;
  try
    with FindDataBase.FLibrary do
    try
    if (FQuickScript or (not FParseParams)) then
      FStatementType := DSQLPrepare(FTransaction.FTrHandle, FStHandle,
        FSQL.Text, FTransaction.FSQLDialect, FSQLResult) else
      FStatementType := DSQLPrepare(FTransaction.FTrHandle, FStHandle,
        FParsedSQL, FTransaction.FSQLDialect, FSQLResult);
      FCursorName := 'C' + inttostr(Integer(FStHandle));
      if FUseCursor then
        DSQLSetCursorName(FStHandle, FCursorName);
    except
      FSQLResult.Free;
      FSQLResult := nil;
      EndStatement(FOnError, False);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsPrepare;
end;

procedure TJvUIBStatement.EndPrepare(const ETM: TEndTransMode; Auto: boolean);
begin
  FSQLResult.Free;
  FSQLResult := nil;
  FCurrentState := qsStatement;
  EndStatement(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecute;
begin
  if (FSQLResult = nil) then BeginPrepare;
  Lock;
  try
    with FindDataBase.FLibrary do
    try
      if (FStatementType = stExecProcedure) then
        DSQLExecute2(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter, FSQLResult) else
        DSQLExecute(FTransaction.FTrHandle, FStHandle,
          FTransaction.FSQLDialect, FParameter);
    except
      if (FOnError <> etmStayIn) then
        EndPrepare(FOnError, False);
      raise;
    end;
  finally
    UnLock;
  end;
  FCurrentState := qsExecute;
end;

procedure TJvUIBStatement.EndExecute(const ETM: TEndTransMode; Auto: boolean);
begin
  FCurrentState := qsPrepare;
  EndPrepare(ETM, Auto);
end;

procedure TJvUIBStatement.BeginExecImme;
var
  I: Integer;
  procedure ExecuteQuery(const AQuery: String; Params: TSQLParams);
  begin
    if (Trim(AQuery) = '') then exit;
    Lock;
    try
      with FindDataBase.FLibrary do
      try
        DSQLExecuteImmediate(FindDataBase.FDbHandle, FTransaction.FTrHandle,
          AQuery, FTransaction.FSQLDialect, Params);
      except
        if (FOnError <> etmStayIn) then
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
    for i := 0 to FSQL.Count - 1 do
    begin
      ExecuteQuery(FSQL.Strings[i], nil);
    end else
      if FParseParams then
        ExecuteQuery(FParsedSQL, FParameter) else
        ExecuteQuery(FSQL.Text, FParameter);
  FCurrentState := qsExecImme;
end;

procedure TJvUIBStatement.EndExecImme(const ETM: TEndTransMode; Auto: boolean);
begin
  FCurrentState := qsTransaction;
  if (ETM <> etmStayIn) then
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
      Raise Exception.Create(EUIB_MUSTBEPREPARED)else
        Result := FindDataBase.FLibrary.DSQLInfoPlan(FStHandle);
  finally
    UnLock
  end;
end;

function TJvUIBStatement.GetStatementType: TUIBStatementType;
begin
  if (FCurrentState < qsPrepare) then
    Raise Exception.Create(EUIB_MUSTBEPREPARED)else
    Result := FStatementType;
end;

procedure TJvUIBStatement.DoSQLChange(Sender: TObject);
begin
  InternalClose(etmStayIn, True);
  if (not FQuickScript or FParseParams) then
    FParsedSQL := FParameter.Parse(FSQL.Text);
end;

function TJvUIBStatement.GetFields: TSQLResult;
begin
  if (FSQLResult = nil) then
    raise Exception.Create(EUIB_QUERYNOTOPEN);
  Result := FSQLResult;
end;

function TJvUIBStatement.GetEof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Eof else
    Result := True;
end;

function TJvUIBStatement.GetBof: boolean;
begin
  if Assigned(FSQLResult) then
    Result := FSQLResult.Bof else
    Result := True;
end;

function TJvUIBStatement.FindDataBase: TJvUIBDataBase;
begin
  if FDataBase <> nil then
    result := FDataBase else
     if FTransaction <> nil then
       result := FTransaction.FDataBase else
       raise Exception.Create(EUIB_DATABASENOTDEF);  
end;

procedure TJvUIBStatement.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent = FTransaction) and (Operation = opRemove)) then
    SetTransaction(nil);
end;

constructor TJvUIBStatement.Create(AOwner: TComponent);
begin
  inherited;
  FUseCursor := True;
  FCurrentState := qsDataBase;
  if (AOwner is TJvUIBTransaction) then
    Transaction := TJvUIBTransaction(AOwner) else
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
  FParameter.Free;
  FParameter := nil;
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

procedure TJvUIBStatement.ParamsSetBlob(const Index: Word; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.AsQuad[Index] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
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
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
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

procedure TJvUIBStatement.ParamsSetBlob(const Name: string; var str: string);
var BlobHandle: IscBlobHandle;
begin
  if (FCurrentState < qsTransaction) then
    BeginTransaction;
  BlobHandle := nil;
  Lock;
  with FindDataBase.FLibrary do
  try
    Params.ByNameAsQuad[Name] := BlobCreate(FindDataBase.FDbHandle,
      FTransaction.FTrHandle, BlobHandle);
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
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
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
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
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
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
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

procedure TJvUIBStatement.InternalClose(const Mode: TEndTransMode;
  Auto: boolean);
begin
  case FCurrentState of
    qsStatement   : EndStatement(Mode, Auto);
    qsExecImme    : EndExecImme(Mode, Auto);
    qsPrepare     : EndPrepare(Mode, Auto);
    qsExecute     : EndExecute(Mode, Auto);
  end;
end;

procedure TJvUIBStatement.InternalGetBlobSize(sqlda: TSQLDA; const Index: Word; out Size: Cardinal);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if (not sqlda.IsNull[Index]) then
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
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
    Result := Fields.GetBlobSize(Index) else
    InternalGetBlobSize(Fields, Index, Result);
end;

function TJvUIBStatement.ParamBlobSize(const Index: Word): Cardinal;
begin
  InternalGetBlobSize(Params, Index, Result);
end;

procedure TJvUIBStatement.ReadBlob(const Index: Word; Buffer: Pointer);
begin
  if Fields.FetchBlobs then
    Fields.ReadBlob(Index, Buffer) else
    InternalReadBlob(Fields, Index, Buffer);
end;

procedure TJvUIBStatement.ReadBlob(const name: string; Buffer: Pointer);
begin
  ReadBlob(Fields.GetFieldIndex(name), Buffer);
end;

procedure TJvUIBStatement.InternalReadBlob(sqlda: TSQLDA;
  const Index: Word; Buffer: Pointer);
var
  BlobHandle: IscBlobHandle;
begin
  if (not sqlda.IsBlob[Index]) then
    raise EUIBConvertError.Create(EUIB_CASTERROR);
  if sqlda.IsNull[Index] then
     Exit else
  begin
    Lock;
    with FindDataBase.FLibrary do
    try
      BlobHandle := nil;
      BlobOpen(FindDataBase.FDbHandle, FTransaction.FTrHandle,
        BlobHandle, sqlda.AsQuad[Index]);
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
    if (FCurrentState < qsPrepare) then
      Raise Exception.Create(EUIB_MUSTBEPREPARED)else
      Result := FindDataBase.FLibrary.DSQLInfoRowsAffected(FStHandle, FStatementType);
  finally
    UnLock
  end;
end;

procedure TJvUIBStatement.CloseCursor;
begin
  if (FCurrentState = qsExecute) then
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
  end;
end;

{ TJvUIBQuery }

procedure TJvUIBQuery.BuildStoredProc(const StoredProc: string);
var
  i, r: Integer;
  Str: string;
begin
  InternalClose(etmStayIn, True);
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
              Raise Exception.Create(EUIB_UNEXPECTEDERROR);
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
            raise Exception.Create(EUIB_UNEXPECTEDERROR);
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
      InternalClose(FOnError, False);
      raise;
    end;
  finally
    InternalClose(etmStayIn, True);
    TStringList(FSQL).OnChange := DoSQLChange;
  end;
end;

{ TJvUIBTransaction }

constructor TJvUIBTransaction.Create(AOwner: TComponent);
begin
  inherited;
  FOptions     := [tpConcurrency,tpWait,tpWrite];
  FTrHandle := nil;
  FStatements  := 0;
  FDataBases   := TList.Create;
  FAutoRetain  := False;
  FAutoStart   := True;
  FAutoStop    := True;
  FDefaultAction := etmCommit; 
end;

destructor TJvUIBTransaction.Destroy;
begin
  ClearSQLComponents;
  Close(etmDefault, True);
  ClearDataBases;
  FDataBases.Free;
  inherited;
end;

procedure TJvUIBTransaction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if ((AComponent is TJvUIBDataBase) and (Operation = opRemove)) then
    RemoveDatabase(TJvUIBDataBase(AComponent));
end;

procedure TJvUIBTransaction.SetDataBase(const ADatabase: TJvUIBDataBase);
begin
  RemoveDatabase(FDataBase);
  AddDataBase(ADatabase);
  FDataBase := ADatabase;
end;

procedure TJvUIBTransaction.Close(const Mode: TEndTransMode; Auto: boolean);
var
  i: Integer;
begin
  lock;
  try
    if (FStatements > 0) and (FSQLComponent <> nil) then
      for i := 0 to FSQLComponent.Count -1 do
        TJvUIBQuery(FSQLComponent.Items[i]).InternalClose(etmStayIn, Auto);
  finally
    UnLock;
  end;
  EndTransaction(Mode, nil, Auto);
end;

function TJvUIBTransaction.GetStatements(const Index: Integer): TJvUIBStatement;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Items[Index] else
    raise EListError.CreateFmt(EUIB_INDEXERROR,[Index]);
end;

function TJvUIBTransaction.GetStatementsCount: Integer;
begin
  if FSQLComponent <> nil then
    Result := FSQLComponent.Count else
    Result := 0;
end;

procedure TJvUIBTransaction.ClearDataBases;
var i: Integer;
begin
  FDataBase := nil;
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).RemoveTransaction(Self);
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
var i: Integer;
begin
  if (FDataBase = nil) then raise Exception.Create(EUIB_DATABASENOTDEF);
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).Connected := True;
end;

procedure TJvUIBTransaction.BeginTransaction(Auto: boolean = True);
type
  TEBDynArray = array of TISCTEB;
var
  Buffer: Pointer;
  i: Integer;
  ATPB: string;
begin
  BeginDataBase;
  Lock;
  try
    with FDataBase.FLibrary do
    if (FTrHandle = nil) then
    begin
      If Auto and (not FAutoStart) then
        raise EUIBException.Create(EUIB_EXPLICITTRANS);

      if FDataBases.Count = 1 then
      begin
        TransactionStart(FTrHandle, FDataBase.FDbHandle, TPB);
      end else
      begin
        GetMem(Buffer,  SizeOf(TISCTEB) * FDataBases.Count);
        try
          ATPB := TPB;
          for i := 0 to FDataBases.Count - 1 do
            with TEBDynArray(Buffer)[i] do
            begin
              Handle  := @TJvUIBDatabase(FDataBases[i]).FDbHandle;
              Len     := Length(ATPB);
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

function TJvUIBTransaction.EndTransaction(ETM: TEndTransMode; From: TJvUIBStatement;
  Auto: boolean): boolean;
var i: Integer;
begin
  Result := False;
  // don't lock if it is not necessary
  if (ETM = etmStayIn) then Exit;
  Lock;
  try
    // Default Action
    if (ETM = etmDefault) then ETM := FDefaultAction;
    if (FTrHandle <> nil) then
      with FDataBase.FLibrary do
      try
        if Assigned(FOnEndTransaction) then
          FOnEndTransaction(Self, ETM);
       { If there is Statements alive I must keep handle only if FAutoRetain = True.}
        if (FStatements > 0) and FAutoRetain then
          case ETM of
            etmCommit   : ETM := etmCommitRetaining;
            etmRollback : ETM := etmRollbackRetaining;
          end else
            if (ETM in [etmCommit, etmRollback]) then
            begin
              if (FStatements > 0) and (FSQLComponent <> nil) then
                for i := 0 to FSQLComponent.Count -1 do
                  if (From <> FSQLComponent.Items[i]) then
                    TJvUIBQuery(FSQLComponent.Items[i]).InternalClose(etmStayIn, Auto);
            end;

        Assert( FAutoStop or (not Auto), EUIB_NOAUTOSTOP);

        case ETM of
          etmCommit            :
            begin
              TransactionCommit(FTrHandle);
              Result := True;
            end;
          etmCommitRetaining   : TransactionCommitRetaining(FTrHandle);
          etmRollback          :
            begin
              TransactionRollback(FTrHandle);
              Result := True;
            end;
          etmRollbackRetaining : TransactionRollbackRetaining(FTrHandle);
        end;
      except
        case ETM of
          etmCommit, etmRollback :
            TransactionRollback(FTrHandle);
          etmCommitRetaining, etmRollbackRetaining :
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
  if (FSQLComponent = nil) then
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
  if (FSQLComponent <> nil) then
  begin
    FSQLComponent.Remove(Component);
    if (FSQLComponent.Count = 0) then
    begin
      FSQLComponent.Free;
      FSQLComponent := nil;
    end;
  end;
end;

procedure TJvUIBTransaction.Lock;
var i: Integer;
begin
  inherited;
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).Lock;
end;

procedure TJvUIBTransaction.UnLock;
var i: Integer;
begin
  for i := 0 to FDataBases.Count - 1 do
    TJvUIBDataBase(FDataBases[i]).UnLock;
  inherited;
end;

procedure TJvUIBTransaction.AddDataBase(ADataBase: TJvUIBDataBase);
var i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
        Exit;
    Close(etmDefault, True);
    FDataBases.Add(ADataBase);
    ADataBase.AddTransaction(Self);
    FSQLDialect := ADatabase.SQLDialect;
  end;
end;

procedure TJvUIBTransaction.RemoveDatabase(ADataBase: TJvUIBDataBase);
var
  i: Integer;
begin
  if (ADataBase <> nil) then
  begin
    if ADataBase = FDataBase then
      FDataBase := nil;
    for i := 0 to FDataBases.Count - 1 do
      if FDataBases[i] = ADataBase then
      begin
        Close(etmDefault, True);
        ADataBase.RemoveTransaction(Self);
        FDataBases.Delete(i);
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
{$ENDIF}

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

function TJvUIBTransaction.GetAutoRetain: boolean;
begin
  Lock;
  try
    Result := FAutoRetain;
  finally
    UnLock;
  end;
end;

procedure TJvUIBTransaction.SetAutoRetain(const Value: boolean);
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

{ TJvUIBComponent }

constructor TJvUIBComponent.Create(AOwner: TComponent);
begin
  inherited;
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
    proNetBEUI    : FLibrary.ServiceAttach('\\'+ Fhost + '\service_mgr', FHandle, CreateSPB);
  end;
end;

constructor TJvUIBService.Create(AOwner: TComponent);
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

constructor TJvUIBBackupRestore.Create(AOwner: TComponent);
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
          raise Exception.Create(EUIB_UNEXPECTEDERROR);
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

constructor TJvUIBRestore.Create(AOwner: TComponent);
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
constructor TJvUIBScript.Create(AOwner: TComponent);
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
       raise Exception.Create(EUIB_TRANSACTIONNOTDEF);
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
                FQuery.FindDataBase.SQLDialect := Dialect else
                raise Exception.Create(EUIB_PARSESQLDIALECT);
            end;
          NodeSetNames:
            begin
              CheckDatabase;
              for j := low(TCharacterSet) to high(TCharacterSet) do
              begin
                if (CompareText(CharacterSetStr[j], Grammar.RootNode.Nodes[i].Value) = 0) then
                begin
                  FQuery.FindDataBase.CharacterSet := j;
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
                FLibrary.Load(FLiBraryName);
                // I MUST provide the real DB Handle (not nil)
                // because altering forein key can fail otherwise.
                FQuery.FindDataBase.Lock;
                try
                  FLibrary.DSQLExecuteImmediate(
                    FDbHandle, TrHandle, Statement, SQLDialect);
                finally
                  FQuery.FindDataBase.UnLock;
                end;
              end;
              with Grammar.RootNode.Nodes[i].Nodes[0] do
              for k := 0 to NodesCount - 1 do
              case Nodes[k].NodeType of
                NodeName     : FQuery.FindDataBase.DatabaseName :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
                NodeUsername : FQuery.FindDataBase.UserName :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
                NodePassWord : FQuery.FindDataBase.PassWord :=
                  copy(Nodes[k].Value ,2, Length(Nodes[k].Value) - 2);
              end;
            end;
          NodeConnect:
            with FQuery.FindDataBase do
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

procedure TJvUIBScript.SetScript(const Value: TStrings);
begin
  FScript.Assign(Value);
end;

procedure TJvUIBScript.SetTransaction(const Value: TJvUIBTransaction);
begin
  FQuery.Transaction := Value;
end;

{ TMetaDataOptions }

constructor TMetaDataOptions.Create;
begin
  inherited;
  FObjects := ALLOBjects;
  FTables := ALLTables;
  FViews := ALLViews;
  FProcedures := ALLProcedures;
  FUDFs := ALLUDFs;
  FSysInfos := False;
end;

end.
