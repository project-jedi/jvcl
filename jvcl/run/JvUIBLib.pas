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
{ The Original Code is JvUIBLib.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{                                                                              }
{ The Initial Developer of TMemoryPool is TurboPower FlashFiler.               }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}
{ UIB Library, class and functions helpers to use Interbase API. }

{$I jvcl.inc}
{$I JvUIB.inc}

unit JvUIBLib;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  {$IFDEF FPC}
  Variants,
  {$ENDIF FPC}
  JvUIBase, JvUIBError;

type
  TUIBFieldType = (uftUnKnown, uftNumeric, uftChar, uftVarchar, uftCstring, uftSmallint,
    uftInteger, uftQuad, uftFloat, uftDoublePrecision, uftTimestamp, uftBlob, uftBlobId,
    uftDate, uftTime, uftInt64 {$IFDEF IB7_UP}, uftBoolean {$ENDIF});

  TScale = 1..15;

//******************************************************************************
// Errors handling
//******************************************************************************

  EUIBConvertError = class(Exception);

  EUIBError = class(Exception)
  private
    FErrorCode: Integer;
    FSQLCode: Integer;
  public
    property ErrorCode: Integer read FErrorCode;
    property SQLCode: Integer read FSQLCode;
  end;

  EUIBException = class(EUIBError)
  private
    FNumber: Integer;
  public
    property Number: Integer read FNumber;
  end;

  EUIBGFixError = class(EUIBError);
  EUIBDSQLError = class(EUIBError);
  EUIBDynError = class(EUIBError);
  EUIBGBakError = class(EUIBError);
  EUIBGSecError = class(EUIBError);
  EUIBLicenseError = class(EUIBError);
  EUIBGStatError = class(EUIBError);

  EUIBExceptionClass = class of EUIBException;

  EUIBParser = class(Exception)
  private
    FLine: Integer;
    FCharacter: Integer;
  public
    // the extra parameter is a dummy parameter to force the generation of
    // a unique constructor declaration in the resulting hpp file
    // when used with BCB
    constructor Create(Line, Character: Integer; DummyForBCB: Integer = 0);
    property Line: Integer read FLine;
    property Character: Integer read FCharacter;
  end;

const
  QuadNull: TISCQuad = (gds_quad_high: 0; gds_quad_low: 0);

//******************************************************************************
// Database
//******************************************************************************

type
  TCharacterSet = (csNONE, csASCII, csBIG_5, csCYRL, csDOS437, csDOS850,
    csDOS852, csDOS857, csDOS860, csDOS861, csDOS863, csDOS865, csEUCJ_0208,
    csGB_2312, csISO8859_1, csISO8859_2, csKSC_5601, csNEXT, csOCTETS, csSJIS_0208,
    csUNICODE_FSS, csWIN1250, csWIN1251, csWIN1252, csWIN1253, csWIN1254
    {$IFDEF FB15_UP}
   , csDOS737, csDOS775, csDOS858, csDOS862, csDOS864, csDOS866, csDOS869, csWIN1255,
    csWIN1256, csWIN1257, csISO8859_3, csISO8859_4, csISO8859_5, csISO8859_6, csISO8859_7,
    csISO8859_8, csISO8859_9, csISO8859_13
    {$ENDIF FB15_UP}
    {$IFDEF IB71_UP}
   , csISO8859_15, csKOI8R
    {$ENDIF IB71_UP}
    );

const
  CharacterSetStr: array [TCharacterSet] of string = (
    'NONE', 'ASCII', 'BIG_5', 'CYRL', 'DOS437', 'DOS850', 'DOS852', 'DOS857',
    'DOS860', 'DOS861', 'DOS863', 'DOS865', 'EUCJ_0208', 'GB_2312', 'ISO8859_1',
    'ISO8859_2', 'KSC_5601', 'NEXT', 'OCTETS', 'SJIS_0208', 'UNICODE_FSS',
    'WIN1250', 'WIN1251', 'WIN1252', 'WIN1253', 'WIN1254'
    {$IFDEF FB15_UP}
   , 'DOS737', 'DOS775', 'DOS858', 'DOS862', 'DOS864', 'DOS866', 'DOS869',
    'WIN1255', 'WIN1256', 'WIN1257', 'ISO8859_3', 'ISO8859_4', 'ISO8859_5',
    'ISO8859_6', 'ISO8859_7', 'ISO8859_8', 'ISO8859_9', 'ISO8859_13'
    {$ENDIF FB15_UP}
    {$IFDEF IB71_UP}
   , 'ISO8859_15', 'KOI8R'
    {$ENDIF IB71_UP}
    );

  {$IFDEF DLLREGISTRY}
  FBINSTANCES = 'SOFTWARE\Firebird Project\Firebird Server\Instances';
  {$ENDIF DLLREGISTRY}

function StrToCharacterSet(const CharacterSet: string): TCharacterSet;
function CreateDBParams(Params: string; Delimiter: Char = ';'): string;
function GetClientLibrary: string;

//******************************************************************************
// Transaction
//******************************************************************************

const
  // Default Transaction Parameter
  TPBDefault = isc_tpb_version3 + isc_tpb_write + isc_tpb_concurrency + isc_tpb_wait;

//******************************************************************************
//  DSQL
//******************************************************************************

//****************************************
// TSQLDA
//****************************************

const
  {$IFDEF IB7_UP}
  MaxParamLength = 274;
  {$ELSE}
  MaxParamLength = 125;
  {$ENDIF IB7_UP}

type
  PUIBSQLVar = ^TUIBSQLVar;
  TUIBSQLVar = record
    SqlType: Smallint;
    SqlScale: Smallint;
    {$IFDEF IB7_UP}
    SqlPrecision: Smallint;
    {$ENDIF IB7_UP}
    SqlSubType: Smallint;
    SqlLen: Smallint;
    SqlData: PChar;
    SqlInd: PSmallint;
    case Byte of
      // TSQLResult
      0:
       (SqlNameLength: Smallint;
        SqlName: array [0..METADATALENGTH - 1] of Char;
        RelNameLength: Smallint;
        RelName: array [0..METADATALENGTH - 1] of Char;
        OwnNameLength: Smallint;
        OwnName: array [0..METADATALENGTH - 1] of Char;
        AliasNameLength: Smallint;
        AliasName: array [0..METADATALENGTH - 1] of Char;);
      // TSQLParam
      1:
       (Init: Boolean;
        ID: Word;
        ParamNameLength: Smallint;
        ParamName: array [0..MaxParamLength - 1] of Char;);
  end;

  PUIBSQLDa = ^TUIBSQLDa;
  TUIBSQLDa = record
    version: Smallint;                  // version of this XSQLDA
    sqldaid: array [0..7] of Char;      // XSQLDA name field         ->  RESERVED
    sqldabc: ISCLong;                   // length in bytes of SQLDA  ->  RESERVED
    sqln: Smallint;                     // number of fields allocated
    sqld: Smallint;                     // actual number of fields
    sqlvar: array [Word] of TUIBSQLVar; // first field address
  end;

  TUIBStatementType = (
    stSelect,          //  select                 SELECT
    stInsert,          //  insert                 INSERT INTO
    stUpdate,          //  update                 UPDATE
    stDelete,          //  delete                 DELETE FROM
    stDDL,             //
    stGetSegment,      //  blob                   READ BLOB
    stPutSegment,      //  blob                   INSERT BLOB
    stExecProcedure,   //  invoke_procedure       EXECUTE PROCEDURE
    stStartTrans,      //  declare                DECLARE
    stCommit,          //  commit                 COMMIT
    stRollback,        //  rollback               ROLLBACK [WORK]
    stSelectForUpdate, //                         SELECT ... FOR UPDATE
    stSetGenerator
    {$IFDEF FB15_UP}
   , stSavePoint       //  user_savepoint | undo_savepoint       SAVEPOINT | ROLLBACK [WORK] TO
    {$ENDIF FB15_UP}
    );

// TODO
//  alter                  ALTER              -> DDL
//  create                 CREATE
//  drop                   DROP
//  grant                  GRANT
//  recreate               RECREATE
//  replace                CREATE OR ALTER
//  revoke                 REVOKE
//  set                    SET

//******************************************************************************
//  Abstract Class
//******************************************************************************

const
  ScaleDivisor: array [-15..-1] of Int64 = (1000000000000000, 100000000000000,
    10000000000000, 1000000000000, 100000000000, 10000000000, 1000000000, 100000000,
    10000000, 1000000, 100000, 10000, 1000, 100, 10);
type
  TSQLDA = class(TObject)
  private
    FXSQLDa: PUIBSQLDa;
    function GetPointer: PUIBSQLDa;
    function GetAllocatedFields: Word;
    procedure SetAllocatedFields(Fields: Word);
    function GetActualFields: Word;
    function GetFieldCount: Integer;
    function GetSQLType(const Index: Word): Smallint;
    function GetSQLLen(const Index: Word): Smallint;
    function DecodeString(const Code: Smallint; Index: Word): string; overload;
    procedure DecodeString(const Code: Smallint; Index: Word; out Str: string); overload;
    procedure DecodeWideString(const Code: Smallint; Index: Word; out Str: WideString);
  protected
    function GetSqlName(const Index: Word): string;
    function GetRelName(const Index: Word): string;
    function GetOwnName(const Index: Word): string;
    function GetAliasName(const Index: Word): string;

    function GetIsNumeric(const Index: Word): Boolean;
    function GetIsBlob(const Index: Word): Boolean;
    function GetIsNull(const Index: Word): Boolean;
    function GetIsNullable(const Index: Word): Boolean;

    function GetAsDouble(const Index: Word): Double;
    function GetAsCurrency(const Index: Word): Currency;
    function GetAsInt64(const Index: Word): Int64;
    function GetAsInteger(const Index: Word): Integer;
    function GetAsSingle(const Index: Word): Single;
    function GetAsSmallint(const Index: Word): Smallint;
    function GetAsString(const Index: Word): string; virtual;
    function GetAsWideString(const Index: Word): WideString; virtual;
    function GetAsQuad(const Index: Word): TISCQuad;
    function GetAsVariant(const Index: Word): Variant; virtual;
    function GetAsDateTime(const Index: Word): TDateTime;
    function GetAsDate(const Index: Word): Integer;
    function GetAsTime(const Index: Word): Cardinal;
    function GetAsBoolean(const Index: Word): Boolean;

    function GetByNameIsNumeric(const Name: string): Boolean;
    function GetByNameIsBlob(const Name: string): Boolean;
    function GetByNameIsNull(const Name: string): Boolean;
    function GetByNameIsNullable(const Name: string): Boolean;

    function GetByNameAsDouble(const Name: string): Double;
    function GetByNameAsCurrency(const Name: string): Currency;
    function GetByNameAsInt64(const Name: string): Int64;
    function GetByNameAsInteger(const Name: string): Integer;
    function GetByNameAsSingle(const Name: string): Single;
    function GetByNameAsSmallint(const Name: string): Smallint;
    function GetByNameAsString(const Name: string): string;
    function GetByNameAsWideString(const Name: string): WideString;
    function GetByNameAsQuad(const Name: string): TISCQuad;
    function GetByNameAsVariant(const Name: string): Variant;
    function GetByNameAsDateTime(const Name: string): TDateTime;
    function GetByNameAsBoolean(const Name: string): Boolean;
    function GetByNameAsDate(const Name: string): Integer;
    function GetByNameAsTime(const Name: string): Cardinal;

    function GetFieldType(const Index: Word): TUIBFieldType; virtual;
  public
    procedure CheckRange(const Index: Word);
    function GetFieldIndex(const Name: string): Word; virtual;
    property Data: PUIBSQLDa read FXSQLDa;
    property IsBlob[const Index: Word]: Boolean read GetIsBlob;
    property IsNull[const Index: Word]: Boolean read GetIsNull;
    property IsNullable[const Index: Word]: Boolean read GetIsNullable;
    property IsNumeric[const Index: Word]: Boolean read GetIsNumeric;
    property AsQuad[const Index: Word]: TISCQuad read GetAsQuad;
    property XSQLDA: PUIBSQLDa read GetPointer;
    property FieldCount: Integer read GetFieldCount;
    property SQLType[const Index: Word]: Smallint read GetSQLType;
    property SQLLen[const Index: Word]: Smallint read GetSQLLen;
    property FieldType[const Index: Word]: TUIBFieldType read GetFieldType;
  end;

  PPageInfo = ^TPageInfo;
  TPageInfo = packed record
    NextPage: Pointer;
    UsageCounter: Integer;
  end;

  TMemoryPool = class(TObject)
  private
    function GetCount: Integer;
    function GetItems(const Index: Integer): Pointer;
  protected
    // (rom) This is not designed well. Better make that virtual access methods.
    FItemSize: Integer;
    FItemsInPage: Integer;
    FPageSize: Integer;
    FFirstPage: PPageInfo;
    FFreeList: Pointer;
    FList: TList;
  protected
    procedure AddPage;
    procedure CleanFreeList(const PageStart: Pointer);
  public
    constructor Create(ItemSize, ItemsInPage: Integer);
    destructor Destroy; override;
    function New: Pointer;
    function PageCount: Integer;
    function PageUsageCount(const PageIndex: Integer): Integer;
    procedure Dispose(var P: Pointer);
    function RemoveUnusedPages: Integer;

    property PageSize: Integer read FPageSize;
    property ItemsInPage: Integer read FItemsInPage;
    property ItemSize: Integer read FItemSize;
    property Items[const Index: Integer]: Pointer read GetItems;
    property Count: Integer read GetCount;
  end;

  TBlobData = packed record
    Size: Cardinal;
    Buffer: Pointer;
  end;
  TBlobDataArray = array [Word] of TBlobData;
  PBlobDataArray = ^TBlobDataArray;

  TSQLResult = class(TSQLDA)
  private
    FMemoryPool: TMemoryPool;
    FCachedFetch: Boolean;
    FFetchBlobs: Boolean;
    FDataBuffer: Pointer;
    FBlobArray: PBlobDataArray;
    FDataBufferLength: Word;
    FBlobsIndex: array of Word;
    FCurrentRecord: Integer;
    FBufferChunks: Cardinal;
    FScrollEOF: Boolean;
    procedure AddCurrentRecord;
    procedure FreeBlobs(Buffer: Pointer);
    function GetRecordCount: Integer;
    function GetCurrentRecord: Integer;
    procedure AllocateDataBuffer;
    function GetBlobIndex(const Index: Word): Word;
    function GetEof: Boolean;
    function GetUniqueRelationName: string;
  protected
    function GetAsString(const Index: Word): string; override;
    function GetAsWideString(const Index: Word): WideString; override;
    function GetAsVariant(const Index: Word): Variant; override;
  public
    constructor Create(Fields: Smallint = 0;
      CachedFetch: Boolean = False;
      FetchBlobs: Boolean = False;
      BufferChunks: Cardinal = 1000);
    destructor Destroy; override;
    procedure ClearRecords;
    procedure GetRecord(const Index: Integer);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    procedure ReadBlob(const Index: Word; Stream: TStream); overload;
    procedure ReadBlob(const Index: Word; var Str: string); overload;
    procedure ReadBlob(const Index: Word; var Str: WideString); overload;
    procedure ReadBlob(const Index: Word; var Value: Variant); overload;
    procedure ReadBlob(const Index: Word; Data: Pointer); overload;
    procedure ReadBlob(const Name: string; Stream: TStream); overload;
    procedure ReadBlob(const Name: string; var Str: string); overload;
    procedure ReadBlob(const Name: string; var Str: WideString); overload;
    procedure ReadBlob(const Name: string; var Value: Variant); overload;
    procedure ReadBlob(const Name: string; Data: Pointer); overload;

    function GetBlobSize(const Index: Word): Cardinal;

    property Eof: Boolean read GetEof;

    property CachedFetch: Boolean read FCachedFetch;
    property FetchBlobs: Boolean read FFetchBlobs;
    property RecordCount: Integer read GetRecordCount;
    property CurrentRecord: Integer read GetCurrentRecord write GetRecord;
    property BufferChunks: Cardinal read FBufferChunks;
    property UniqueRelationName: string read GetUniqueRelationName;

    // (rom) the type Word enforces a limitation of the number of fields
    // (rom) better make it Integer. CheckRange checks it anyway.
    property SqlName[const Index: Word]: string read GetSqlName;
    property RelName[const Index: Word]: string read GetRelName;
    property OwnName[const Index: Word]: string read GetOwnName;
    property AliasName[const Index: Word]: string read GetAliasName;

    property AsSmallint[const Index: Word]: Smallint read GetAsSmallint;
    property AsInteger[const Index: Word]: Integer read GetAsInteger;
    property AsSingle[const Index: Word]: Single read GetAsSingle;
    property AsDouble[const Index: Word]: Double read GetAsDouble;
    property AsCurrency[const Index: Word]: Currency read GetAsCurrency;
    property AsInt64[const Index: Word]: Int64 read GetAsInt64;
    property AsString[const Index: Word]: string read GetAsString;
    property AsWideString[const Index: Word]: WideString read GetAsWideString;
    property AsVariant[const Index: Word]: Variant read GetAsVariant;
    property AsDateTime[const Index: Word]: TDateTime read GetAsDateTime;
    property AsDate[const Index: Word]: Integer read GetAsDate;
    property AsTime[const Index: Word]: Cardinal read GetAsTime;
    property AsBoolean[const Index: Word]: Boolean read GetAsBoolean;

    property ByNameIsNull[const Name: string]: Boolean read GetByNameIsNull;
    property ByNameIsNullable[const Name: string]: Boolean read GetByNameIsNullable;

    property ByNameAsSmallint[const Name: string]: Smallint read GetByNameAsSmallint;
    property ByNameAsInteger[const Name: string]: Integer read GetByNameAsInteger;
    property ByNameAsSingle[const Name: string]: Single read GetByNameAsSingle;
    property ByNameAsDouble[const Name: string]: Double read GetByNameAsDouble;
    property ByNameAsCurrency[const Name: string]: Currency read GetByNameAsCurrency;
    property ByNameAsInt64[const Name: string]: Int64 read GetByNameAsInt64;
    property ByNameAsString[const Name: string]: string read GetByNameAsString;
    property ByNameAsWideString[const Name: string]: WideString read GetByNameAsWideString;
    property ByNameAsQuad[const Name: string]: TISCQuad read GetByNameAsQuad;
    property ByNameAsVariant[const Name: string]: Variant read GetByNameAsVariant;
    property ByNameAsDateTime[const Name: string]: TDateTime read GetByNameAsDateTime;
    property ByNameAsBoolean[const Name: string]: Boolean read GetByNameAsBoolean;
    property ByNameAsDate[const Name: string]: Integer read GetByNameAsDate;
    property ByNameAsTime[const Name: string]: Cardinal read GetByNameAsTime;

    property Values[const Name: string]: Variant read GetByNameAsVariant; default;
  end;

  TSQLResultClass = class of TSQLResult;

  TSQLParams = class(TSQLDA)
  private
    FParamCount: Word;
    procedure EncodeString(Code: Smallint; Index: Word; const Str: string);
    procedure EncodeWideString(Code: Smallint; Index: Word; const Str: WideString);
    function FindParam(const Name: string; out Index: Word): Boolean;
    function GetFieldName(const Index: Word): string;
  protected
    function AddField(const Name: string): Word;
    procedure SetFieldType(const Index: Word; Size: Integer; Code: Smallint;
      Scale: Smallint = 0);
    procedure SetIsNull(const Index: Word; const Value: Boolean);

    procedure SetAsDouble(const Index: Word; const Value: Double);
    procedure SetAsCurrency(const Index: Word; const Value: Currency);
    procedure SetAsInt64(const Index: Word; const Value: Int64);
    procedure SetAsInteger(const Index: Word; const Value: Integer);
    procedure SetAsSingle(const Index: Word; const Value: Single);
    procedure SetAsSmallint(const Index: Word; const Value: Smallint);
    procedure SetAsString(const Index: Word; const Value: string);
    procedure SetAsWideString(const Index: Word; const Value: WideString);
    procedure SetAsQuad(const Index: Word; const Value: TISCQuad);
    procedure SetAsDateTime(const Index: Word; const Value: TDateTime);
    procedure SetAsBoolean(const Index: Word; const Value: Boolean);
    procedure SetAsDate(const Index: Word; const Value: Integer);
    procedure SetAsTime(const Index: Word; const Value: Cardinal);

    procedure SetByNameIsNull(const Name: string; const Value: Boolean);
    procedure SetByNameAsDouble(const Name: string; const Value: Double);
    procedure SetByNameAsCurrency(const Name: string; const Value: Currency);
    procedure SetByNameAsInt64(const Name: string; const Value: Int64);
    procedure SetByNameAsInteger(const Name: string; const Value: Integer);
    procedure SetByNameAsSingle(const Name: string; const Value: Single);
    procedure SetByNameAsSmallint(const Name: string; const Value: Smallint);
    procedure SetByNameAsString(const Name: string; const Value: string);
    procedure SetByNameAsWideString(const Name: string; const Value: WideString);
    procedure SetByNameAsQuad(const Name: string; const Value: TISCQuad);
    procedure SetByNameAsDateTime(const Name: string; const Value: TDateTime);
    procedure SetByNameAsBoolean(const Name: string; const Value: Boolean);
    procedure SetByNameAsDate(const Name: string; const Value: Integer);

    function GetFieldType(const Index: Word): TUIBFieldType; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Parse(const SQL: string): string;
    function GetFieldIndex(const Name: string): Word; override;

    procedure AddFieldType(const Name: string; FieldType: TUIBFieldType;
      Scale: TScale = 1; Precision: Byte = 0);

    property IsNull[const Index: Word]: Boolean read GetIsNull write SetIsNull;

    property AsSmallint[const Index: Word]: Smallint read GetAsSmallint write SetAsSmallint;
    property AsInteger[const Index: Word]: Integer read GetAsInteger write SetAsInteger;
    property AsSingle[const Index: Word]: Single read GetAsSingle write SetAsSingle;
    property AsDouble[const Index: Word]: Double read GetAsDouble write SetAsDouble;
    property AsCurrency[const Index: Word]: Currency read GetAsCurrency write SetAsCurrency;
    property AsInt64[const Index: Word]: Int64 read GetAsInt64 write SetAsInt64;
    property AsString[const Index: Word]: string read GetAsString write SetAsString;
    property AsWideString[const Index: Word]: WideString read GetAsWideString write SetAsWideString;
    property AsQuad[const Index: Word]: TISCQuad read GetAsQuad write SetAsQuad;
    property AsDateTime[const Index: Word]: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBoolean[const Index: Word]: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDate[const Index: Word]: Integer read GetAsDate write SetAsDate;
    property AsTime[const Index: Word]: Cardinal read GetAsTime write SetAsTime;

    property ByNameIsNull[const Name: string]: Boolean read GetByNameIsNull write SetByNameIsNull;

    property ByNameAsSmallint[const Name: string]: Smallint read GetByNameAsSmallint write SetByNameAsSmallint;
    property ByNameAsInteger[const Name: string]: Integer read GetByNameAsInteger write SetByNameAsInteger;
    property ByNameAsSingle[const Name: string]: Single read GetByNameAsSingle write SetByNameAsSingle;
    property ByNameAsDouble[const Name: string]: Double read GetByNameAsDouble write SetByNameAsDouble;
    property ByNameAsCurrency[const Name: string]: Currency read GetByNameAsCurrency write SetByNameAsCurrency;
    property ByNameAsInt64[const Name: string]: Int64 read GetByNameAsInt64 write SetByNameAsInt64;
    property ByNameAsString[const Name: string]: string read GetByNameAsString write SetByNameAsString;
    property ByNameAsWideString[const Name: string]: WideString read GetByNameAsWideString write SetByNameAsWideString;
    property ByNameAsQuad[const Name: string]: TISCQuad read GetByNameAsQuad write SetByNameAsQuad;
    property ByNameAsVariant[const Name: string]: Variant read GetByNameAsVariant;
    property ByNameAsDateTime[const Name: string]: TDateTime read GetByNameAsDateTime write SetByNameAsDateTime;
    property ByNameAsBoolean[const Name: string]: Boolean read GetByNameAsBoolean write SetByNameAsBoolean;
    property ByNameAsDate[const Name: string]: Integer read GetByNameAsDate write SetByNameAsDate;

    property Values[const Name: string]: Variant read GetByNameAsVariant; default;
    property FieldName[const Index: Word]: string read GetFieldName;

  end;

  TSQLParamsClass = class of TSQLParams;

type
  TDSQLInfoData = packed record
    InfoCode: Byte;
    InfoLen: Word; // isc_portable_integer convert a Smallint to Word ??? so just say it is a word
    case Byte of
      isc_info_sql_stmt_type:
        (StatementType: TUIBStatementType);
      isc_info_sql_get_plan:
        (PlanDesc: array [0..255] of Char);
  end;

  {$IFDEF IB7_UP}
  TArrayDesc = TISCArrayDescV2;
  TBlobDesc = TISCBlobDescV2;
  {$ELSE}
  TArrayDesc = TISCArrayDesc;
  TBlobDesc = TISCBlobDesc;
  {$ENDIF IB7_UP}

  TUIBLibrary = class;

  TStatusVector = array [0..19] of ISCStatus;
  PStatusVector = ^TStatusVector;

  TOnConnectionLost = procedure(Lib: TUIBLibrary) of object;
  TOnGetDBExceptionClass = procedure(Number: Integer; out Excep: EUIBExceptionClass) of object;

  TUIBLibrary = class(TUIBaseLibrary)
  private
    FStatusVector: TStatusVector;
    FOnConnectionLost: TOnConnectionLost;
    FOnGetDBExceptionClass: TOnGetDBExceptionClass;
    FRaiseErrors: Boolean;
    FSegmentSize: Word;
    function GetSegmentSize: Word;
    procedure SetSegmentSize(Value: Word);
    procedure CheckUIBApiCall(const Status: ISCStatus);
  public
    constructor Create; override;

    property OnConnectionLost: TOnConnectionLost read FOnConnectionLost write FOnConnectionLost;
    property OnGetDBExceptionClass: TOnGetDBExceptionClass read FOnGetDBExceptionClass write FOnGetDBExceptionClass;
    property RaiseErrors: Boolean read FRaiseErrors write FRaiseErrors default True;

    {Attaches to an existing database.
     Ex: AttachDatabase('c:\DataBase.gdb', DBHandle, 'user_name=SYSDBA; password=masterkey'); }
    procedure AttachDatabase(FileName: string; var DbHandle: IscDbHandle; Params: string; Sep: Char = ';');
    {Detaches from a database previously connected with AttachDatabase.}
    procedure DetachDatabase(var DBHandle: IscDbHandle);

    procedure TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle; const TPB: string = '');
    procedure TransactionStartMultiple(var TraHandle: IscTrHandle; DBCount: Smallint; Vector: PISCTEB);
    procedure TransactionCommit(var TraHandle: IscTrHandle);
    procedure TransactionRollback(var TraHandle: IscTrHandle);
    procedure TransactionCommitRetaining(var TraHandle: IscTrHandle);
    procedure TransactionPrepare(var TraHandle: IscTrHandle);
    procedure TransactionRollbackRetaining(var TraHandle: IscTrHandle);
    procedure DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: string; Dialect: Word; Sqlda: TSQLDA = nil); overload;
    procedure DSQLExecuteImmediate(const Statement: string; Dialect: Word; Sqlda: TSQLDA = nil); overload;
    procedure DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
    function DSQLPrepare(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Statement: string; Dialect: Word; Sqlda: TSQLResult = nil): TUIBStatementType;
    procedure DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; Sqlda: TSQLParams = nil);
    procedure DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
      Dialect: Word; InSqlda: TSQLParams; OutSqlda: TSQLResult);
    procedure DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
    function DSQLFetch(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): Boolean;
    function DSQLFetchWithBlobs(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): Boolean;
    procedure DSQLDescribe(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult);
    procedure DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLDA);
    procedure DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: string);
    procedure DSQLExecImmed2(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
      const Statement: string; Dialect: Word; InSqlda, OutSqlda: TSQLDA);

    procedure DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of Byte; var Buffer: string);
    function DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
    function DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TUIBStatementType;
    function DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle;
      StatementType: TUIBStatementType): Cardinal;

    procedure DDLExecute(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; const ddl: string);

    function ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      const RelationName, FieldName: string): TArrayDesc;
    procedure ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      ArrayId: TISCQuad; var Desc: TArrayDesc; DestArray: PPointer; var SliceLength: Integer);
    procedure ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
      var ArrayId: TISCQuad; var Desc: TArrayDesc; DestArray: PPointer;
      var SliceLength: Integer);

    procedure ServiceAttach(const ServiceName: string; var SvcHandle: IscSvcHandle; const Spb: string);
    procedure ServiceDetach(var SvcHandle: IscSvcHandle);
    procedure ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: string; var Buffer: string);
    procedure ServiceStart(var SvcHandle: IscSvcHandle; const Spb: string);

    function ErrSqlcode: ISCLong;
    function ErrInterprete: string;
    function ErrSQLInterprete(SQLCODE: Smallint): string;

    procedure BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; var BlobHandle: IscBlobHandle; BlobId:
      TISCQuad; BPB: string = '');
    function BlobGetSegment(var BlobHandle: IscBlobHandle; out Length: Word; BufferLength: Cardinal; Buffer: PChar):
      Boolean;
    procedure BlobClose(var BlobHandle: IscBlobHandle);
    procedure BlobInfo(var BlobHandle: IscBlobHandle; out NumSegments, MaxSegment, TotalLength: Cardinal;
      out BType: Byte);
    procedure BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
    procedure BlobDefaultDesc(var Desc: TBlobDesc; const RelationName, FieldName: string);
    procedure BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
    function BlobReadString(var BlobHandle: IscBlobHandle): string; overload;
    procedure BlobReadString(var BlobHandle: IscBlobHandle; var Str: string); overload;
    procedure BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
    // you must free memory allocated by this method !!
    procedure BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Cardinal; var Buffer: Pointer);
    // the buffer size if known and Pointer allocated.
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle; Buffer: Pointer); overload;
    // DBexpress and SP: the component set the max blob size
    procedure BlobReadSizedBuffer(var BlobHandle: IscBlobHandle; Buffer: Pointer; MaxSize: Cardinal); overload;
    function BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle; var BlobHandle: IscBlobHandle;
      BPB: string = ''): TISCQuad;
    procedure BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: PChar);
    procedure BlobWriteString(var BlobHandle: IscBlobHandle; var Str: string);
    procedure BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);

    function StreamBlobOpen(var BlobId: TISCQuad; var Database: IscDbHandle;
      var Transaction: IscTrHandle; mode: Char): PBStream;
    function StreamBlobClose(Stream: PBStream): Integer;
    {$IFDEF IB71_UP}
    procedure SavepointRelease(var TrHandle: IscTrHandle; const Name: string);
    procedure SavepointRollback(var TrHandle: IscTrHandle; const Name: string; Option: Word);
    procedure SavepointStart(var TrHandle: IscTrHandle; const Name: string);
    {$ENDIF IB7_UP}

    property SegMentSize: Word read GetSegmentSize write SetSegmentSize;
  end;

//******************************************************************************
// Conversion
//******************************************************************************

procedure DecodeTimeStamp(V: PISCTimeStamp; out DateTime: Double); overload;
procedure DecodeTimeStamp(V: PISCTimeStamp; out TimeStamp: TTimeStamp); overload;
function DecodeTimeStamp(V: PISCTimeStamp): Double; overload;

procedure EncodeTimeStamp(const DateTime: TDateTime; V: PISCTimeStamp); overload;
procedure EncodeTimeStamp(const Date: Integer; V: PISCTimeStamp); overload;
procedure EncodeTimeStamp(const Time: Cardinal; V: PISCTimeStamp); overload;
procedure DecodeSQLDate(V: Integer; out Year: Smallint; out Month, Day: Word); overload;
procedure DecodeSQLDate(const V: Integer; out Date: Double); overload;
procedure DecodeSQLDate(const V: Integer; out Date: Integer); overload;
function DecodeSQLDate(const V: Integer): Integer; overload;
procedure EncodeSQLDate(Date: TDateTime; out V: Integer); overload;
procedure EncodeSQLDate(Date: Integer; out V: Integer); overload;
procedure EncodeSQLDate(Year: Smallint; Month, Day: Word; out V: Integer); overload;

procedure DecodeSQLTime(V: Cardinal; out Hour, Minute, Second: Word; out Fractions: Longword);
procedure EncodeSQLTime(const Hour, Minute, Second: Word; const Fractions: Longword; out V: Cardinal);

//******************************************************************************
// Event functions
//******************************************************************************
//    function  isc_cancel_events
//    function  isc_que_events
//    function  isc_wait_for_event
//    procedure isc_event_counts
//    function  isc_event_block

//******************************************************************************
// Security
//******************************************************************************
//    function isc_add_user
//    function isc_delete_user
//    function isc_modify_user

//******************************************************************************
// Other
//******************************************************************************
//    function isc_compile_request
//    function isc_compile_request2
//    function isc_ddl
//    function isc_prepare
//    function isc_receive
//    function isc_reconnect_transaction
//    function isc_release_request
//    function isc_request_info
//    function isc_seek_blob
//    function isc_send
//    function isc_start_and_send
//    function isc_start_request
//    function isc_transact_request
//    function isc_unwind_request

//******************************************************************************
//
//******************************************************************************
//    function isc_ftof
//    function isc_free
//    function isc_print_blr
//    procedure isc_qtoq
//    procedure isc_set_debug
//    procedure isc_vtof
//    procedure isc_vtov
//  {$IFDEF FB15}
//    function isc_reset_fpe
//  {$ENDIF FB15}
//  {$IFDEF IB7_UP}
//    procedure isc_get_client_version
//    function  isc_get_client_major_version
//    function isc_get_client_minor_version
//  {$ENDIF IB7_UP}

type
  TParamType = (
    prNone, // no param
    prByte, // Byte Param
    prCard, // Cardinal Param
    prStrg, // String Param
    prIgno // Ignore Command
    );

  TDPBInfo = record
    Name: string;
    ParamType: TParamType;
  end;

const

  DPBInfos: array [1..isc_dpb_Max_Value] of TDPBInfo =
   ((Name: 'cdd_pathname'; ParamType: prIgno), // not implemented
    (Name: 'allocation'; ParamType: prIgno), // not implemented
    (Name: 'journal'; ParamType: prIgno), // not implemented
    (Name: 'page_size'; ParamType: prCard), // ok
    (Name: 'num_buffers'; ParamType: prCard), // ok
    (Name: 'buffer_length'; ParamType: prIgno), // not implemented
    (Name: 'debug'; ParamType: prCard), // ok
    (Name: 'garbage_collect'; ParamType: prIgno), // not implemented
    (Name: 'verify'; ParamType: prCard), // ok
    (Name: 'sweep'; ParamType: prCard), // ok

    (Name: 'enable_journal'; ParamType: prStrg), // ok
    (Name: 'disable_journal'; ParamType: prNone), // ok
    (Name: 'dbkey_scope'; ParamType: prCard), // ok
    (Name: 'number_of_users'; ParamType: prIgno), // not implemented
    (Name: 'trace'; ParamType: prNone), // ok
    (Name: 'no_garbage_collect'; ParamType: prIgno), // not implemented
    (Name: 'damaged'; ParamType: prNone), // ok
    (Name: 'license'; ParamType: prStrg),
    (Name: 'sys_user_name'; ParamType: prStrg), // ok
    (Name: 'encrypt_key'; ParamType: prStrg), // ok

    (Name: 'activate_shadow'; ParamType: prNone), // ok deprecated
    (Name: 'sweep_interval'; ParamType: prCard), // ok
    (Name: 'delete_shadow'; ParamType: prNone), // ok
    (Name: 'force_write'; ParamType: prCard), // ok
    (Name: 'begin_log'; ParamType: prStrg), // ok
    (Name: 'quit_log'; ParamType: prNone), // ok
    (Name: 'no_reserve'; ParamType: prCard), // ok
    (Name: 'user_name'; ParamType: prStrg), // ok
    (Name: 'password'; ParamType: prStrg), // ok
    (Name: 'password_enc'; ParamType: prStrg), // ok

    (Name: 'sys_user_name_enc'; ParamType: prNone),
    (Name: 'interp'; ParamType: prCard), // ok
    (Name: 'online_dump'; ParamType: prCard), // ok
    (Name: 'old_file_size'; ParamType: prCard), // ok
    (Name: 'old_num_files'; ParamType: prCard), // ok
    (Name: 'old_file'; ParamType: prStrg), // ok
    (Name: 'old_start_page'; ParamType: prCard), // ok
    (Name: 'old_start_seqno'; ParamType: prCard), // ok
    (Name: 'old_start_file'; ParamType: prCard), // ok
    (Name: 'drop_walfile'; ParamType: prCard), // ok

    (Name: 'old_dump_id'; ParamType: prCard), // ok
    (Name: 'wal_backup_dir'; ParamType: prStrg), // ok
    (Name: 'wal_chkptlen'; ParamType: prCard), // ok
    (Name: 'wal_numbufs'; ParamType: prCard), // ok
    (Name: 'wal_bufsize'; ParamType: prCard), // ok
    (Name: 'wal_grp_cmt_wait'; ParamType: prCard), // ok
    (Name: 'lc_messages'; ParamType: prStrg), // ok
    (Name: 'lc_ctype'; ParamType: prStrg), // ok
    (Name: 'cache_manager'; ParamType: prIgno), // not used in fb1.5
    (Name: 'shutdown'; ParamType: prCard), // ok

    (Name: 'online'; ParamType: prNone), // ok
    (Name: 'shutdown_delay'; ParamType: prCard), // ok
    (Name: 'reserved'; ParamType: prStrg), // ok
    (Name: 'overwrite'; ParamType: prCard), // ok
    (Name: 'sec_attach'; ParamType: prCard), // ok
    (Name: 'disable_wal'; ParamType: prNone), // ok
    (Name: 'connect_timeout'; ParamType: prCard), // ok
    (Name: 'dummy_packet_interval'; ParamType: prCard), // ok
    (Name: 'gbak_attach'; ParamType: prStrg), // ok
    (Name: 'sql_role_name'; ParamType: prStrg), // ok rolename

    (Name: 'set_page_buffers'; ParamType: prCard), // ok Change age buffer 50 >= buf >= 65535 (default 2048)
    (Name: 'working_directory'; ParamType: prStrg), // ok
    (Name: 'sql_dialect'; ParamType: prCard), // ok Set SQL Dialect for this connection (1,2,3)
    (Name: 'set_db_readonly'; ParamType: prCard), // ok
    (Name: 'set_db_sql_dialect'; ParamType: prCard), // ok Change sqldialect (1,2,3))
    (Name: 'gfix_attach'; ParamType: prNone), // ok FB15: don't work
    (Name: 'gstat_attach'; ParamType: prNone) // ok FB15: don't work
    {$IFDEF IB65ORYF867}
    , (Name: 'gbak_ods_version'; ParamType: prCard) // ??
    , (Name: 'gbak_ods_minor_version'; ParamType: prCard) // ??
    {$ENDIF IB65ORYF867}

    {$IFDEF YF867_UP}
    , (Name: 'numeric_scale_reduction'; ParamType: prNone)
    {$ENDIF YF867_UP}

    {$IFDEF IB7_UP}
    , (Name: 'set_group_commit'; ParamType: prNone) // ??
    {$ENDIF IB7_UP}
    {$IFDEF IB71_UP}
    , (Name: 'gbak_validate'; ParamType: prNone) // ??
    {$ENDIF IB71_UP}
    {$IFDEF FB103_UP}
    , (Name: 'set_db_charset'; ParamType: prStrg) // ok
    {$ENDIF FB103_UP}
    );

{$IFNDEF COMPILER6_UP}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
{$ENDIF COMPILER6_UP}

implementation

uses
  JvUIBConst;

const
  BoolVals: array [Boolean] of Smallint =
    (0, -1);

{$IFNDEF COMPILER6_UP}
function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF COMPILER6_UP}

function BoolToUIBStr(const Value: Boolean): string;
begin
  if Value then
    Result := sUIBTrue
  else
    Result := sUIBFalse;
end;

function InternalGetSqlType(SqlType: Smallint): Smallint;
begin
  Result := SqlType and (not 1);
end;

function InternalGetIsNumeric(SqlScale: Smallint): Boolean;
begin
  Result := (SqlScale < 0);
end;

//******************************************************************************
// Errors handling
//******************************************************************************

const
  ISC_MASK   = $14000000; // Defines the code as a valid ISC code
  FAC_MASK   = $00FF0000; // Specifies the facility where the code is located
  CODE_MASK  = $0000FFFF; // Specifies the code in the message file
  CLASS_MASK = $F0000000; // Defines the code as warning, error, info, or other

  // Note: Perhaps a debug level could be interesting !!!
  CLASS_ERROR   = 0; // Code represents an error
  CLASS_WARNING = 1; // Code represents a warning
  CLASS_INFO    = 2; // Code represents an information msg

  //FACILITY   FAC_CODE    MAX_NUMBER    LAST_CHANGE
  FAC_JRD        = 0;  //         501   26/10/2002 17:02:13   <- In Use
  FAC_QLI        = 1;  //         509   07/11/1996 13:38:37
  FAC_GDEF       = 2;  //         345   07/11/1996 13:38:37
  FAC_GFIX       = 3;  //         114   25/12/2001 02:59:17   <- In Use
  FAC_GPRE       = 4;  //           1   07/11/1996 13:39:40
  FAC_GLTJ       = 5;  //           1   07/11/1996 13:39:40
  FAC_GRST       = 6;  //           1   07/11/1996 13:39:40
  FAC_DSQL       = 7;  //          15   22/07/2001 23:26:58   <- In Use
  FAC_DYN        = 8;  //         215   01/07/2001 17:43:07   <- In Use
  FAC_FRED       = 9;  //           1   07/11/1996 13:39:40
  FAC_INSTALL    = 10; //           1   07/11/1996 13:39:40
  FAC_TEST       = 11; //           4   07/11/1996 13:38:41
  FAC_GBAK       = 12; //         283   05/03/2002 02:38:49   <- In Use
  FAC_SQLERR     = 13; //         917   05/03/2002 02:55:22
  FAC_SQLWARN    = 14; //         102   07/11/1996 13:38:42
  FAC_JRD_BUGCHK = 15; //         305   05/03/2002 02:29:03
  FAC_GJRN       = 16; //         241   07/11/1996 13:38:43
  FAC_ISQL       = 17; //         109   10/10/2001 03:27:43
  FAC_GSEC       = 18; //          91   04/11/1998 11:06:15   <- In Use
  FAC_LICENSE    = 19; //          60   05/03/2002 02:30:12   <- In Use
  FAC_DOS        = 20; //          74   05/03/2002 02:31:54
  FAC_GSTAT      = 21; //          36   10/10/2001 18:05:16   <- In Use

function GetFacility(Code: ISCStatus): Word;
begin
  Result := (Code and FAC_MASK) shr 16;
end;

function GetClass(Code: ISCStatus): Word;
begin
  Result := (Code and CLASS_MASK) shr 30;
end;

function GETCode(Code: ISCStatus): Word;
begin
  Result := (Code and CODE_MASK) shr 0;
end;

//=== EUIBParser =============================================================

constructor EUIBParser.Create(Line, Character: Integer; DummyForBCB: Integer);
begin
  FLine := Line;
  FCharacter := Character;
  Message := Format('Parse error Line %d, Char %d', [FLine, FCharacter]);
end;

//******************************************************************************
// Database
//******************************************************************************

//=== TUIBLibrary ============================================================

constructor TUIBLibrary.Create;
begin
  inherited Create;
  FRaiseErrors := True;
  FSegmentSize := 16 * 1024;
end;

procedure TUIBLibrary.CheckUIBApiCall(const Status: ISCStatus);
var
  Exception: EUIBError;
  Number: Integer;
  Excep: EUIBExceptionClass;
begin
  if (Status <> 0) and FRaiseErrors then
    if GetClass(Status) = CLASS_ERROR then // only raise CLASS_ERROR
    begin
      case GetFacility(Status) of
        FAC_JRD:
          if Status = isc_except then
          begin
            Number := FStatusVector[3];
            if Assigned(FOnGetDBExceptionClass) then
            begin
              FOnGetDBExceptionClass(Number, Excep);
              Exception := Excep.Create(ErrInterprete)
            end
            else
              Exception := EUIBException.Create(ErrInterprete);
            EUIBException(Exception).FNumber := Number;
          end
          else
            Exception := EUIBError.Create(ErrInterprete);
        FAC_GFIX:
          Exception := EUIBGFIXError.Create(ErrInterprete);
        FAC_DSQL:
          Exception := EUIBDSQLError.Create(ErrInterprete);
        FAC_DYN:
          Exception := EUIBDYNError.Create(ErrInterprete);
        FAC_GBAK:
          Exception := EUIBGBAKError.Create(ErrInterprete);
        FAC_GSEC:
          Exception := EUIBGSECError.Create(ErrInterprete);
        FAC_LICENSE:
          Exception := EUIBLICENSEError.Create(ErrInterprete);
        FAC_GSTAT:
          Exception := EUIBGSTATError.Create(ErrInterprete);
      else
        Exception := EUIBError.Create(ErrInterprete);
      end;
      Exception.FSQLCode := ErrSqlcode;
      if Exception.FSQLCode <> 0 then
        Exception.Message := Exception.Message + ErrSQLInterprete(Exception.FSQLCode) + BreakLine;
      Exception.FErrorCode := GETCode(Status);
      Exception.Message := Exception.Message + Format(EUIB_ERRCODEFMT, [Exception.FErrorCode]);
      if (Exception.FErrorCode = 401) and Assigned(FOnConnectionLost) then
        FOnConnectionLost(Self);
      raise Exception;
    end;
end;

function GetClientLibrary: string;
{$IFDEF DLLREGISTRY}
const
  cDefaultInstance = 'DefaultInstance';
var
  Key: HKEY;
  Size: Cardinal;
  HR: Integer;
{$ENDIF DLLREGISTRY}
begin
  {$IFDEF DLLREGISTRY}
  HR := RegOpenKeyEx(HKEY_LOCAL_MACHINE, FBINSTANCES, 0, KEY_READ, Key);
  if HR = ERROR_SUCCESS then
  begin
    HR := RegQueryValueEx(Key, cDefaultInstance, nil, nil, nil, @Size);
    if HR = ERROR_SUCCESS then
    begin
      SetLength(Result, Size);
      HR := RegQueryValueEx(Key, cDefaultInstance, nil, nil, Pointer(Result), @Size);
      if HR = ERROR_SUCCESS then
        Result := Trim(Result) + 'bin\' + GDS32DLL;
    end;
    RegCloseKey(Key);
  end;
  if HR <> ERROR_SUCCESS then
  {$ENDIF DLLREGISTRY}
    Result := GDS32DLL;
end;

function CreateDBParams(Params: string; Delimiter: Char = ';'): string;
var
  BufferSize: Integer;
  CurPos, NextPos: PChar;
  CurStr, CurValue: string;
  EqualPos: Integer;
  Code: Byte;
  AValue: Integer;
  FinalSize: Integer;

  function Min(V1, V2: Integer): Integer;
  begin
    if V1 > V2 then
      Result := V2
    else
      Result := V1;
  end;

  // (rom) reimplemented
  // dont reallocate memory each time, step by step ...
  procedure CheckBufferSize(Add: Integer);
  begin
    Inc(FinalSize, Add);
    if FinalSize <= BufferSize then
    begin
      BufferSize := (FinalSize and (not 31)) + 32;
      SetLength(Result, BufferSize);
    end;
  end;

  procedure AddByte(AByte: Byte);
  begin
    CheckBufferSize(1);
    Result[FinalSize] := Chr(AByte);
  end;

  procedure AddWord(AWord: Word);
  begin
    CheckBufferSize(2);
    PWord(@Result[FinalSize - 1])^ := AWord;
  end;

  procedure AddCard(ACard: Cardinal);
  begin
    case ACard of
      0..255:
        begin
          AddByte(1);
          AddByte(Byte(ACard));
        end;
      256..65535:
        begin
          AddByte(2);
          AddWord(Word(ACard));
        end;
    else
      AddByte(4);
      CheckBufferSize(4);
      PCardinal(@Result[FinalSize - 3])^ := ACard;
    end;
  end;

  procedure AddString(var AString: string);
  var
    L: Integer;
  begin
    L := Min(Length(AString), 255);
    CheckBufferSize(L + 1);
    Result[FinalSize - l] := Chr(L);
    Move(PChar(AString)^, Result[FinalSize - L + 1], L);
  end;

begin
  FinalSize := 1;
  BufferSize := 0;
  CheckBufferSize(0);
  Result[1] := Chr(isc_dpb_version1);
  CurPos := PChar(Params);
  while CurPos <> nil do
  begin
    NextPos := StrScan(CurPos, Delimiter);
    if NextPos = nil then
      CurStr := CurPos
    else
    begin
      CurStr := Copy(CurPos, 0, NextPos - CurPos);
      Inc(NextPos);
    end;
    CurPos := NextPos;
    if CurStr = '' then
      Continue;
    begin
      CurValue := '';
      EqualPos := Pos('=', CurStr);
      if EqualPos <> 0 then
      begin
        CurValue := Copy(CurStr, EqualPos + 1, Length(CurStr) - EqualPos);
        CurStr := Copy(CurStr, 0, EqualPos - 1);
      end;
      CurStr := Trim(LowerCase(CurStr));
      CurValue := Trim(CurValue);
      for Code := 1 to isc_dpb_Max_Value do
        with DPBInfos[Code] do
          if Name = CurStr then
          begin
            case ParamType of
              prNone:
                AddByte(Code);
              prByte:
                if TryStrToInt(CurValue, AValue) and (AValue in [0..255]) then
                begin
                  AddByte(Code);
                  AddByte(Byte(AValue));
                end;
              prCard:
                if TryStrToInt(CurValue, AValue) and (AValue > 0) then
                begin
                  AddByte(Code);
                  AddCard(AValue);
                end;
              prStrg:
                if Length(CurValue) > 0 then
                begin
                  AddByte(Code);
                  AddString(CurValue);
                end;
            end;
            Break;
          end;
    end;
  end;
  SetLength(Result, FinalSize);
end;

procedure TUIBLibrary.AttachDatabase(FileName: string; var DbHandle: IscDbHandle;
  Params: string; Sep: Char = ';');
begin
  Params := CreateDBParams(Params, Sep);
  Lock;
  try
    CheckUIBApiCall(isc_attach_database(@FStatusVector, Length(FileName), Pointer(FileName),
      @DBHandle, Length(Params), PChar(Params)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DetachDatabase(var DBHandle: IscDbHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_detach_database(@FStatusVector, @DBHandle));
    // if connection lost DBHandle must be set manually to nil.
    DBHandle := nil;
  finally
    UnLock;
  end;
end;

function StrToCharacterSet(const CharacterSet: string): TCharacterSet;
var
  Len: Integer;
begin
  Len := Length(CharacterSet);
  for Result := Low(TCharacterSet) to High(TCharacterSet) do
    if (Len = Length(CharacterSetStr[Result])) and
      (CompareText(CharacterSetStr[Result], CharacterSet) = 0) then
      Exit;
  raise Exception.CreateFmt(EUIB_CHARSETNOTFOUND, [CharacterSet]);
end;

//******************************************************************************
// Transaction
//******************************************************************************

procedure TUIBLibrary.TransactionStart(var TraHandle: IscTrHandle; var DbHandle: IscDbHandle;
  const TPB: string = '');
var
  Vector: TISCTEB;
begin
  Vector.Handle := @DbHandle;
  Vector.Len := Length(TPB);
  Vector.Address := PChar(TPB);
  TransactionStartMultiple(TraHandle, 1, @Vector);
end;

procedure TUIBLibrary.TransactionStartMultiple(var TraHandle: IscTrHandle;
  DBCount: Smallint; Vector: PISCTEB);
begin
  Lock;
  try
    CheckUIBApiCall(isc_start_multiple(@FStatusVector, @TraHandle, DBCount, Vector));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.TransactionCommit(var TraHandle: IscTrHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_commit_transaction(@FStatusVector, @TraHandle));
    // if connection lost TraHandle must be set manually to nil.
    TraHandle := nil;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.TransactionRollback(var TraHandle: IscTrHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_rollback_transaction(@FStatusVector, @TraHandle));
    // if connection lost TraHandle must be set manually to nil.
    TraHandle := nil;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.TransactionCommitRetaining(var TraHandle: IscTrHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_commit_retaining(@FStatusVector, @TraHandle));
    // (rom) is the above reset of TraHandle not needed here?
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.TransactionPrepare(var TraHandle: IscTrHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_prepare_transaction(@FStatusVector, @TraHandle));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.TransactionRollbackRetaining(var TraHandle: IscTrHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_rollback_retaining(@FStatusVector, @TraHandle));
  finally
    UnLock;
  end;
end;

//******************************************************************************
// DSQL
//******************************************************************************

function GetSQLDAData(SQLDA: TSQLDA): Pointer;
begin
  if SQLDA <> nil then
    Result := SQLDA.FXSQLDa
  else
    Result := nil;
end;

//****************************************
// API CALLS
//****************************************

procedure TUIBLibrary.DSQLExecuteImmediate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
  const Statement: string; Dialect: Word; Sqlda: TSQLDA = nil);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_execute_immediate(@FStatusVector, @DBHandle, @TraHandle,
      Length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLExecuteImmediate(const Statement: string; Dialect: Word; Sqlda: TSQLDA = nil);
var
  P: Pointer;
begin
  Lock;
  try
    P := nil;
    CheckUIBApiCall(isc_dsql_execute_immediate(@FStatusVector, @P, @P,
      Length(Statement), Pointer(Statement), Dialect, GetSQLDAData(Sqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLAllocateStatement(var DBHandle: IscDbHandle; var StmtHandle: IscStmtHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_allocate_statement(@FStatusVector, @DBHandle, @StmtHandle));
  finally
    UnLock;
  end;
end;

function TUIBLibrary.DSQLPrepare(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
  Statement: string; Dialect: Word; Sqlda: TSQLResult = nil): TUIBStatementType;
var
  STInfo: packed record
    InfoCode: Byte;
    InfoLen: Word; // isc_portable_integer convert a Smallint to Word ??? so just say it is a word
    InfoType: TUIBStatementType;
    InfoIn: Byte;
  end;
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_prepare(@FStatusVector, @TraHandle, @StmtHandle, Length(Statement),
      PChar(Statement), Dialect, GetSQLDAData(Sqlda)));
    STInfo.InfoIn := isc_info_sql_stmt_type;
    isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @STInfo.InfoIn, SizeOf(STInfo), @STInfo);
    Dec(STInfo.InfoType);
    Result := STInfo.InfoType;
  finally
    UnLock;
  end;

  if Sqlda <> nil then
  begin
    Sqlda.ClearRecords;
    if Sqlda.GetActualFields <> Sqlda.GetAllocatedFields then
    begin
      Sqlda.SetAllocatedFields(Sqlda.FXSQLDa.sqld);
      DSQLDescribe(StmtHandle, Dialect, Sqlda);
    end
    else
      Sqlda.AllocateDataBuffer;
  end;

end;

procedure TUIBLibrary.DSQLExecute(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle;
  Dialect: Word; Sqlda: TSQLParams = nil);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_execute(@FStatusVector, @TraHandle, @StmtHandle,
      Dialect, GetSQLDAData(Sqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLExecute2(var TraHandle: IscTrHandle; var StmtHandle: IscStmtHandle; Dialect: Word;
  InSqlda: TSQLParams; OutSqlda: TSQLResult);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_execute2(@FStatusVector, @TraHandle, @StmtHandle, Dialect,
      GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLFreeStatement(var StmtHandle: IscStmtHandle; Option: Word);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_free_statement(@FStatusVector, @StmtHandle, Option));
      // if connection lost StmtHandle must be set manually to nil.
    if Option = DSQL_DROP then
      StmtHandle := nil;
  finally
    UnLock;
  end;
end;

function TUIBLibrary.DSQLFetch(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): Boolean;
var
  Status: ISCStatus;
begin
  Result := True;
  if Sqlda <> nil then
    // (rom) better make the Eof property writable and name the implementor FEof
    Sqlda.FScrollEOF := False;
  Lock;
  try
    Status := isc_dsql_fetch(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));
  finally
    UnLock;
  end;
  case Status of
    0:
      if Sqlda <> nil then
        if Sqlda.FCachedFetch then
          Sqlda.AddCurrentRecord;
    100:
      begin
        Result := False; // end of fetch
        if Sqlda <> nil then
          Sqlda.FScrollEOF := True;
      end;
  else
    CheckUIBApiCall(Status);
  end;
end;

function TUIBLibrary.DSQLFetchWithBlobs(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
  var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult): Boolean;
var
  Status: ISCStatus;
  BlobHandle: IscBlobHandle;
  I: Integer;
begin
  Result := True;
  if Sqlda <> nil then
    Sqlda.FScrollEOF := False;
  Lock;
  try
    Status := isc_dsql_fetch(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda));
  finally
    UnLock;
  end;

  case Status of
    0:
      if Sqlda <> nil then
      begin
        // read blobs
        for I := 0 to Length(Sqlda.FBlobsIndex) - 1 do
        begin
          // free previous blobs if not stored
          if (not Sqlda.FCachedFetch) and // not stored
            (Sqlda.FBlobArray[I].Size > 0) then // not null (null if the first one)
            FreeMem(Sqlda.FBlobArray[I].Buffer);

          if Sqlda.IsNull[Sqlda.FBlobsIndex[I]] then
          begin
            Sqlda.FBlobArray[I].Size := 0;
            Sqlda.FBlobArray[I].Buffer := nil;
          end
          else
          begin
            BlobHandle := nil;
            BlobOpen(DBHandle, TraHandle, BlobHandle, Sqlda.AsQuad[Sqlda.FBlobsIndex[I]]);
            try
              BlobReadBuffer(BlobHandle, Sqlda.FBlobArray[I].Size, Sqlda.FBlobArray[I].Buffer);
                // memory allocated here !!
            finally
              BlobClose(BlobHandle);
            end;
          end;
        end;
        // add to list after the blobs are fetched
        if Sqlda.FCachedFetch then
          Sqlda.AddCurrentRecord;
      end;
    100:
      begin
        Result := False; // end of fetch
        if Sqlda <> nil then
          Sqlda.FScrollEOF := True;
      end;
  else
    CheckUIBApiCall(Status);
  end;
end;

procedure TUIBLibrary.DSQLDescribe(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLResult);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_describe(@FStatusVector, @StmtHandle, Dialect, GetSQLDAData(Sqlda)));
  finally
    UnLock;
  end;
  if Sqlda <> nil then
    Sqlda.AllocateDataBuffer;
end;

procedure TUIBLibrary.DSQLDescribeBind(var StmtHandle: IscStmtHandle; Dialect: Word; Sqlda: TSQLDA);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_describe_bind(@FStatusVector, @StmtHandle, Dialect,
      GetSQLDAData(Sqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLSetCursorName(var StmtHandle: IscStmtHandle; const cursor: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_set_cursor_name(@FStatusVector, @StmtHandle, PChar(cursor), 0));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLExecImmed2(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
  const Statement: string; Dialect: Word; InSqlda, OutSqlda: TSQLDA);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_exec_immed2(@FStatusVector, @DBHandle, @TraHandle, Length(Statement),
      PChar(Statement), Dialect, GetSQLDAData(InSqlda), GetSQLDAData(OutSqlda)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.DSQLInfo(var StmtHandle: IscStmtHandle; const Items: array of Byte; var Buffer: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, Length(Items), @Items[0],
      Length(Buffer), PChar(Buffer)));
  finally
    UnLock;
  end;
end;

function TUIBLibrary.DSQLInfoPlan(var StmtHandle: IscStmtHandle): string;
var
  STInfo: packed record
    InfoCode: Byte;
    InfoLen: Word;
    PlanDesc: array [0..1024] of Char;
  end;
  InfoType: Byte;
begin
  InfoType := isc_info_sql_get_plan;
  Lock;
  try
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @InfoType,
      SizeOf(STInfo), @STInfo));
  finally
    UnLock;
  end;
  SetString(Result, PChar(@STInfo.PlanDesc[1]), STInfo.InfoLen - 1);
end;

function TUIBLibrary.DSQLInfoStatementType(var StmtHandle: IscStmtHandle): TUIBStatementType;
var
  STInfo: packed record
    InfoCode: Byte;
    InfoLen: Word;
    InfoType: TUIBStatementType;
    InfoIn: Byte;
  end;
begin
  STInfo.InfoIn := isc_info_sql_stmt_type;
  Lock;
  try
    CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1,
      @STInfo.InfoIn, SizeOf(STInfo), @STInfo));
  finally
    UnLock;
  end;
  Dec(STInfo.InfoType);
  Result := STInfo.InfoType;
end;

function TUIBLibrary.DSQLInfoRowsAffected(var StmtHandle: IscStmtHandle;
  StatementType: TUIBStatementType): Cardinal;
var
  InfoData: packed record
    InfoCode: Byte;
    InfoLen: Word;
    // (rom) record or packed record? Better redefine to get rid of the ALIGN ON.
    Infos: packed array [0..3] of record
      InfoCode: Byte;
      InfoLen: Word;
      Rows: Cardinal;
    end;
    Command: Word;
  end;
begin
  if not (StatementType in [stUpdate, stDelete, stInsert]) then
    Result := 0
  else
  begin
    Lock;
    try
      InfoData.Command := isc_info_sql_records;
      CheckUIBApiCall(isc_dsql_sql_info(@FStatusVector, @StmtHandle, 1, @InfoData.Command,
        SizeOf(InfoData), @InfoData));
      case StatementType of
        stUpdate:
          Result := InfoData.Infos[0].Rows;
        stDelete:
          Result := InfoData.Infos[1].Rows;
        stInsert:
          Result := InfoData.Infos[3].Rows;
      else
        Result := 0;
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TUIBLibrary.DDLExecute(var DBHandle: IscDbHandle;
  var TraHandle: IscTrHandle; const ddl: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_ddl(@FStatusVector, @DBHandle, @TraHandle,
      Length(ddl), Pointer(ddl)));
  finally
    UnLock;
  end;
end;

//******************************************************************************
//  Array
//******************************************************************************

function TUIBLibrary.ArrayLookupBounds(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
  const RelationName, FieldName: string): TArrayDesc;
begin
  Lock;
  try
    {$IFDEF IB7_UP}
    CheckUIBApiCall(isc_array_lookup_bounds2(@FStatusVector, @DBHandle, @TransHandle,
      PChar(RelationName), PChar(FieldName), @Result));
    {$ELSE}
    CheckUIBApiCall(isc_array_lookup_bounds(@FStatusVector, @DBHandle, @TransHandle,
      PChar(RelationName), PChar(FieldName), @Result));
    {$ENDIF IB7_UP}
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.ArrayGetSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle; ArrayId: TISCQuad;
  var Desc: TArrayDesc; DestArray: PPointer; var SliceLength: Integer);
begin
  Lock;
  try
    {$IFDEF IB7_UP}
    CheckUIBApiCall(isc_array_get_slice2(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
      @Desc, DestArray, @SliceLength));
    {$ELSE}
    CheckUIBApiCall(isc_array_get_slice(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
      @Desc, DestArray, @SliceLength));
    {$ENDIF IB7_UP}
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.ArrayPutSlice(var DBHandle: IscDbHandle; var TransHandle: IscTrHandle;
  var ArrayId: TISCQuad; var Desc: TArrayDesc; DestArray: PPointer; var SliceLength: Integer);
begin
  Lock;
  try
    {$IFDEF IB7_UP}
    CheckUIBApiCall(isc_array_put_slice2(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
      @Desc, DestArray, @SliceLength));
    {$ELSE}
    CheckUIBApiCall(isc_array_put_slice(@FStatusVector, @DBHandle, @TransHandle, @ArrayId,
      @Desc, DestArray, @SliceLength));
    {$ENDIF IB7_UP}
  finally
    UnLock;
  end;
end;

//******************************************************************************
//  Error-handling
//******************************************************************************

function TUIBLibrary.ErrSqlcode: ISCLong;
begin
  Lock;
  try
    Result := isc_sqlcode(@FStatusVector);
  finally
    UnLock;
  end;
end;

function TUIBLibrary.ErrInterprete: string;
var
  StatusVector: PStatusVector;
  Len: Integer;
  Buffer: array [0..512] of Char;
begin
  StatusVector := @FStatusVector;
  Lock;
  try
    repeat
      Len := isc_interprete(Buffer, @StatusVector);
      if Len > 0 then
        // (rom) fix BreakLine and use it here
        Result := Result + Copy(Buffer, 0, Len) + #13#10
      else
        Break;
    until False;
  finally
    UnLock;
  end;
end;

function TUIBLibrary.ErrSQLInterprete(SQLCODE: Smallint): string;
var
  I: Integer;
begin
  SetLength(Result, 255);
  Lock;
  try
    isc_sql_interprete(SQLCODE, PChar(Result), 255);
  finally
    UnLock;
  end;
  for I := 1 to 255 do
    if Result[I] = #0 then
      Break; // Quick trim
  SetLength(Result, I - 1);
end;

//******************************************************************************
// Services
//******************************************************************************

procedure TUIBLibrary.ServiceAttach(const ServiceName: string; var SvcHandle: IscSvcHandle; const Spb: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_service_attach(@FStatusVector, Length(ServiceName),
      PChar(ServiceName), @SvcHandle, Length(Spb), PChar(Spb)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.ServiceDetach(var SvcHandle: IscSvcHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_service_detach(@FStatusVector, @SvcHandle));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.ServiceQuery(var SvcHandle: IscSvcHandle; const SendSpb, RequestSpb: string; var Buffer: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_service_query(@FStatusVector, @SvcHandle, nil,
      Length(SendSpb), PChar(SendSpb), Length(RequestSpb), PChar(RequestSpb),
      Length(Buffer), PChar(Buffer)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.ServiceStart(var SvcHandle: IscSvcHandle; const Spb: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_service_start(@FStatusVector, @SvcHandle, nil, Length(Spb), PChar(Spb)));
  finally
    UnLock;
  end;
end;

//******************************************************************************
//  Blob
//******************************************************************************

procedure TUIBLibrary.BlobOpen(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
  var BlobHandle: IscBlobHandle; BlobId: TISCQuad; BPB: string = '');
begin
  Lock;
  try
    CheckUIBApiCall(isc_open_blob2(@FStatusVector, @DBHandle, @TraHandle, @BlobHandle,
      @BlobId, Length(BPB), PChar(BPB)));
  finally
    UnLock;
  end;
end;

function TUIBLibrary.BlobGetSegment(var BlobHandle: IscBlobHandle; out Length: Word;
  BufferLength: Cardinal; Buffer: PChar): Boolean;
var
  AStatus: ISCStatus;
begin
  if BufferLength > High(Word) then
    BufferLength := High(Word);
  Lock;
  try
    AStatus := isc_get_segment(@FStatusVector, @BlobHandle, @Length, BufferLength, Buffer);
  finally
    UnLock;
  end;
  Result := (AStatus = 0) or (FStatusVector[1] = isc_segment);
  if not Result then
    if FStatusVector[1] <> isc_segstr_eof then
      CheckUIBApiCall(AStatus);
end;

procedure TUIBLibrary.BlobClose(var BlobHandle: IscBlobHandle);
begin
  Lock;
  try
    CheckUIBApiCall(isc_close_blob(@FStatusVector, @BlobHandle));
  finally
    UnLock;
  end;
end;

type
  TBlobInfo = packed record
    Info: Char;
    Length: Word;
    case Byte of
      0:
        (CardType: Cardinal);
      1:
        (ByteType: Byte);
  end;

procedure TUIBLibrary.BlobSize(var BlobHandle: IscBlobHandle; out Size: Cardinal);
var
  BlobInfo: packed record
    Code: Char;
    Length: Word;
    Value: Cardinal;
    Reserved: Word; // alignment (8)
  end;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 1,
      isc_info_blob_total_length, SizeOf(BlobInfo), @BlobInfo));
    Size := BlobInfo.Value;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.BlobMaxSegment(var BlobHandle: IscBlobHandle; out Size: Cardinal);
var
  BlobInfo: array [0..1] of TBlobInfo;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 1,
      isc_info_blob_max_segment, SizeOf(BlobInfo), @BlobInfo));
    Size := BlobInfo[0].CardType;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.BlobInfo(var BlobHandle: IscBlobHandle; out NumSegments, MaxSegment,
  TotalLength: Cardinal; out BType: Byte);
var
  BlobInfos: array [0..3] of TBlobInfo;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 4,
      isc_info_blob_num_segments + isc_info_blob_max_segment +
      isc_info_blob_total_length + isc_info_blob_type, SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  NumSegments := BlobInfos[0].CardType;
  MaxSegment := BlobInfos[1].CardType;
  TotalLength := BlobInfos[2].CardType;
  BType := BlobInfos[3].ByteType;
end;

procedure TUIBLibrary.BlobDefaultDesc(var Desc: TBlobDesc; const RelationName, FieldName: string);
begin
  Lock;
  try
    {$IFDEF IB7_UP}
    isc_blob_default_desc2(@Desc, PChar(RelationName), PChar(FieldName));
    {$ELSE}
    isc_blob_default_desc(@Desc, PChar(RelationName), PChar(FieldName));
    {$ENDIF IB7_UP}
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.BlobSaveToStream(var BlobHandle: IscBlobHandle; Stream: TStream);
var
  BlobInfos: array [0..2] of TBlobInfo;
  Buffer: Pointer;
  CurrentLength: Word;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;

  Stream.Seek(0, soFromBeginning);
  GetMem(Buffer, BlobInfos[0].CardType);
  try
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[0].CardType, Buffer) do
      Stream.Write(Buffer^, CurrentLength);
  finally
    FreeMem(Buffer);
  end;
  Stream.Seek(0, soFromBeginning);
end;

function TUIBLibrary.BlobReadString(var BlobHandle: IscBlobHandle): string;
begin
  BlobReadString(BlobHandle, Result);
end;

procedure TUIBLibrary.BlobReadString(var BlobHandle: IscBlobHandle; var Str: string);
var
  BlobInfos: array [0..2] of TBlobInfo;
  CurrentLength: Word;
  Buffer: Pointer;
  Len: Cardinal;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  // (rom) the following deserves a helper method
  SetLength(Str, BlobInfos[1].CardType);
  Buffer := PChar(Str);
  Len := 0;
  while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - Len, Buffer) do
  begin
    Inc(Integer(Buffer), CurrentLength);
    Inc(Len, CurrentLength);
    if Len = BlobInfos[1].CardType then
      Break;
  end;
end;

procedure TUIBLibrary.BlobReadBuffer(var BlobHandle: IscBlobHandle; var Size: Cardinal; var Buffer: Pointer);
var
  BlobInfos: array [0..2] of TBlobInfo;
  CurrentLength: Word;
  TMP: Pointer;
  Len: Cardinal;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  Size := BlobInfos[1].CardType;
  GetMem(Buffer, Size);
  TMP := Buffer;
  Len := 0;
  while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - Len, TMP) do
  begin
    Inc(Integer(TMP), CurrentLength);
    Inc(Len, CurrentLength);
    if Len = Size then
      Break;
  end;
end;

procedure TUIBLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
  Buffer: Pointer);
var
  BlobInfos: array [0..2] of TBlobInfo;
  CurrentLength: Word;
  TMP: Pointer;
  Len: Cardinal;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  TMP := Buffer;
  Len := 0;
  while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - Len, TMP) do
  begin
    Inc(Integer(TMP), CurrentLength);
    Inc(Len, CurrentLength);
    if Len = BlobInfos[1].CardType then
      Break;
  end;
end;

procedure TUIBLibrary.BlobReadSizedBuffer(var BlobHandle: IscBlobHandle;
  Buffer: Pointer; MaxSize: Cardinal);
var
  BlobInfos: array [0..2] of TBlobInfo;
  CurrentLength: Word;
  TMP: Pointer;
  Len: Cardinal;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  if MaxSize > BlobInfos[1].CardType then
    MaxSize := BlobInfos[1].CardType;

  TMP := Buffer;
  Len := 0;
  while BlobGetSegment(BlobHandle, CurrentLength, MaxSize - Len, TMP) do
  begin
    Inc(Integer(TMP), CurrentLength);
    Inc(Len, CurrentLength);
    if Len = MaxSize then
      Break;
  end;
end;

procedure TUIBLibrary.BlobReadVariant(var BlobHandle: IscBlobHandle; var Value: Variant);
var
  BlobInfos: array [0..2] of TBlobInfo;
  CurrentLength: Word;
  Len: Cardinal;
  Buffer: Pointer;
begin
  Lock;
  try
    CheckUIBApiCall(isc_blob_info(@FStatusVector, @BlobHandle, 2,
      isc_info_blob_max_segment + isc_info_blob_total_length,
      SizeOf(BlobInfos), @BlobInfos));
  finally
    UnLock;
  end;
  Value := VarArrayCreate([0, BlobInfos[1].CardType - 1], varByte);
  Len := 0;
  Buffer := VarArrayLock(Value);
  try
    while BlobGetSegment(BlobHandle, CurrentLength, BlobInfos[1].CardType - Len, Buffer) do
    begin
      Inc(Integer(Buffer), CurrentLength);
      Inc(Len, CurrentLength);
      if Len = BlobInfos[1].CardType then
        Break;
    end;
  finally
    VarArrayUnlock(Value);
  end;
end;

function TUIBLibrary.BlobCreate(var DBHandle: IscDbHandle; var TraHandle: IscTrHandle;
  var BlobHandle: IscBlobHandle; BPB: string = ''): TISCQuad;
begin
  Lock;
  try
    CheckUIBApiCall(isc_create_blob2(@FStatusVector, @DBHandle, @TraHandle, @BlobHandle, @Result, Length(BPB),
      PChar(BPB)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.BlobWriteSegment(var BlobHandle: IscBlobHandle; BufferLength: Cardinal; Buffer: PChar);
var
  Size: Word;
begin
  Lock;
  try
    while BufferLength > 0 do
    begin
      if BufferLength > FSegmentSize then
        Size := FSegmentSize
      else
        Size := BufferLength;
      CheckUIBApiCall(isc_put_segment(@FStatusVector, @BlobHandle, Size, Buffer));
      Dec(BufferLength, Size);
      Inc(Buffer, Size);
    end;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.BlobWriteString(var BlobHandle: IscBlobHandle; var Str: string);
begin
  BlobWriteSegment(BlobHandle, Length(Str), PChar(Str));
end;

procedure TUIBLibrary.BlobWriteStream(var BlobHandle: IscBlobHandle; Stream: TStream);
var
  Buffer: PChar;
begin
  Stream.Seek(0, soFromBeginning);
  if Stream is TCustomMemoryStream then
    BlobWriteSegment(BlobHandle, TCustomMemoryStream(Stream).Size,
      TCustomMemoryStream(Stream).Memory)
  else
  begin
    GetMem(Buffer, Stream.Size);
    try
      Stream.Read(Buffer^, Stream.Size);
      BlobWriteSegment(BlobHandle, Stream.Size, Buffer);
      Stream.Seek(0, soFromBeginning);
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function TUIBLibrary.StreamBlobOpen(var BlobId: TISCQuad;
  var Database: IscDbHandle; var Transaction: IscTrHandle;
  Mode: Char): PBStream;
begin
  Lock;
  try
    Result := Bopen(@BlobId, @Database, @Transaction, @Mode);
  finally
    UnLock;
  end;
end;

function TUIBLibrary.StreamBlobClose(Stream: PBStream): Integer;
begin
  Lock;
  try
    Result := BLOB_close(Stream);
  finally
    UnLock;
  end;
end;

{$IFDEF IB71_UP}

procedure TUIBLibrary.SavepointRelease(var TrHandle: IscTrHandle;
  const Name: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_release_savepoint(@FStatusVector, @TrHandle, PChar(Name)));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.SavepointRollback(var TrHandle: IscTrHandle;
  const Name: string; Option: Word);
begin
  Lock;
  try
    CheckUIBApiCall(isc_rollback_savepoint(@FStatusVector, @TrHandle, PChar(Name), Option));
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.SavepointStart(var TrHandle: IscTrHandle;
  const Name: string);
begin
  Lock;
  try
    CheckUIBApiCall(isc_start_savepoint(@FStatusVector, @TrHandle, PChar(Name)));
  finally
    UnLock;
  end;
end;

{$ENDIF IB71_UP}

function TUIBLibrary.GetSegmentSize: Word;
begin
  Lock;
  try
    Result := FSegmentSize;
  finally
    UnLock;
  end;
end;

procedure TUIBLibrary.SetSegmentSize(Value: Word);
begin
  Lock;
  try
    Assert(Value > 0);
    FSegmentSize := Value;
  finally
    UnLock;
  end;
end;

//******************************************************************************
// Conversion
// Making a delphi conversion will help to transport data buffer and use it
// without GDS32 ;)
//******************************************************************************

// (rom) these functions should go to a helper unit

procedure DecodeSQLDate(const V: Integer; out Date: Double);
var
  Year: Smallint;
  Month, Day: Word;
begin
  DecodeSQLDate(V, Year, Month, Day);
  Date := EncodeDate(Year, Month, Day)
end;

procedure DecodeSQLDate(V: Integer; out Year: Smallint; out Month, Day: Word);
var
  C: Word;
begin
  Inc(V, 678882);
  C := (4 * V - 1) div 146097; // century
  V := 4 * V - 1 - 146097 * C;
  Day := V div 4;
  V := (4 * Day + 3) div 1461;
  Day := 4 * Day + 3 - 1461 * V;
  Day := (Day + 4) div 4;
  Month := (5 * Day - 3) div 153;
  Day := 5 * Day - 3 - 153 * Month;
  Day := (Day + 5) div 5;
  Year := 100 * C + V;
  if Month < 10 then
    Inc(Month, 3)
  else
  begin
    Dec(Month, 9);
    Inc(Year, 1);
  end;
end;

{$IFDEF FPC}
{$IFDEF LINUX}
type
  PDayTable = ^TDayTable;
  TDayTable = array [1..12] of Word;

const
  MonthDays: array [Boolean] of TDayTable =
   ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
{$ENDIF LINUX}
{$ENDIF FPC}

procedure EncodeDate(const Year: Smallint; const Month: Word; Day: Word; out Date: Integer);
var
  I: Integer;
  DayTable: PDayTable;
begin
  Date := 0;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do
      Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
  end;
end;

procedure DecodeSQLDate(const V: Integer; out Date: Integer);
var
  Year: Smallint;
  Month, Day: Word;
begin
  DecodeSQLDate(V, Year, Month, Day);
  EncodeDate(Year, Month, Day, Date)
end;

function DecodeSQLDate(const V: Integer): Integer;
begin
  DecodeSQLDate(V, Result);
end;

procedure EncodeSQLDate(Date: TDateTime; out V: Integer);
var
  Day, Month: Word;
  Year: Word;
  C, Ya: Integer;
begin
  DecodeDate(Date, Year, Month, Day);
  if Month > 2 then
    Dec(Month, 3)
  else
  begin
    Inc(Month, 9);
    Dec(Year);
  end;
  C := Year div 100;
  Ya := Year - (100 * C);
  V := ((146097 * C) div 4 + (1461 * Ya) div 4 + (153 * Month + 2) div 5 + Day - 678882);
end;

procedure EncodeSQLDate(Date: Integer; out V: Integer); overload;
var
  Day, Month: Word;
  Year: Word;
  C, Ya: Integer;
begin
  DecodeDate(Date, Year, Month, Day);
  if Month > 2 then
    Dec(Month, 3)
  else
  begin
    Inc(Month, 9);
    Dec(Year);
  end;
  C := Year div 100;
  Ya := Year - (100 * C);
  V := ((146097 * C) div 4 + (1461 * Ya) div 4 + (153 * Month + 2) div 5 + Day - 678882);
end;

procedure EncodeSQLDate(Year: Smallint; Month, Day: Word; out V: Integer); overload;
var
  C, Ya: Integer;
begin
  if Month > 2 then
    Dec(Month, 3)
  else
  begin
    Inc(Month, 9);
    Dec(Year);
  end;
  C := Year div 100;
  Ya := Year - (100 * C);
  V := ((146097 * C) div 4 + (1461 * Ya) div 4 + (153 * Month + 2) div 5 + Day - 678882);
end;

procedure DecodeTimeStamp(V: PISCTimeStamp; out DateTime: Double);
begin
  DecodeSQLDate(V.timestamp_date, DateTime);
  DateTime := DateTime + (V.timestamp_time / 864000000);
end;

procedure DecodeTimeStamp(V: PISCTimeStamp; out TimeStamp: TTimeStamp);
begin
  TimeStamp.Date := DecodeSQLDate(V.timestamp_date) + 693594;
  TimeStamp.Time := V.timestamp_time div 10;
end;

function DecodeTimeStamp(V: PISCTimeStamp): Double;
begin
  DecodeTimeStamp(V, Result);
end;

procedure EncodeTimeStamp(const DateTime: TDateTime; V: PISCTimeStamp);
begin
  EncodeSQLDate(DateTime, V.timestamp_date);
  V.timestamp_time := Round(Frac(DateTime) * 864000000);
end;

procedure EncodeTimeStamp(const Date: Integer; V: PISCTimeStamp);
begin
  EncodeSQLDate(Date, V.timestamp_date);
  V.timestamp_time := 0;
end;

procedure EncodeTimeStamp(const Time: Cardinal; V: PISCTimeStamp);
begin
  EncodeSQLDate(0, V.timestamp_date);
  V.timestamp_time := Time;
end;

procedure DecodeSQLTime(V: Cardinal; out Hour, Minute, Second: Word;
  out Fractions: Longword);
begin
  Hour := V div 36000000;
  V := V mod 36000000;
  if V > 0 then
  begin
    Minute := V div 600000;
    V := V mod 600000;
    if V > 0 then
    begin
      Second := V div 10000;
      V := V mod 10000;
      if V > 0 then
        Fractions := V div 10
      else
        Fractions := 0;
    end
    else
    begin
      Second := 0;
      Fractions := 0;
    end;
  end
  else
  begin
    Minute := 0;
    Second := 0;
    Fractions := 0;
  end;
end;

procedure EncodeSQLTime(const Hour, Minute, Second: Word; const Fractions: Longword; out V: Cardinal);
begin
  V := Hour * 36000000 + Minute * 600000 + Second * 10000 + Fractions * 10;
end;

//=== TSQLDA =================================================================

function TSQLDA.GetActualFields: Word;
begin
  Result := FXSQLDa.sqld;
end;

function TSQLDA.GetAllocatedFields: Word;
begin
  Result := FXSQLDa.sqln;
end;

function TSQLDA.GetPointer: PUIBSQLDa;
begin
  Result := FXSQLDa;
end;

procedure TSQLDA.SetAllocatedFields(Fields: Word);
begin
  if Fields <= 0 then
    Fields := 1;
  ReallocMem(FXSQLDa, XSQLDA_LENGTH(Fields));
  FXSQLDa.sqln := Fields;
  FXSQLDa.sqld := Fields;
  FXSQLDa.version := SQLDA_CURRENT_VERSION;
end;

function TSQLDA.GetSqlName(const Index: Word): string;
begin
  CheckRange(Index);
  SetString(Result, FXSQLDa.sqlvar[Index].SqlName,
    FXSQLDa.sqlvar[Index].SqlNameLength);
end;

function TSQLDA.GetAliasName(const Index: Word): string;
begin
  CheckRange(Index);
  SetString(Result, FXSQLDa.sqlvar[Index].AliasName,
    FXSQLDa.sqlvar[Index].AliasNameLength);
end;

function TSQLDA.GetOwnName(const Index: Word): string;
begin
  CheckRange(Index);
  SetString(Result, FXSQLDa.sqlvar[Index].OwnName,
    FXSQLDa.sqlvar[Index].OwnNameLength);
end;

function TSQLDA.GetRelName(const Index: Word): string;
begin
  CheckRange(Index);
  SetString(Result, FXSQLDa.sqlvar[Index].RelName,
    FXSQLDa.sqlvar[Index].RelNameLength);
end;

function TSQLDA.GetIsNull(const Index: Word): Boolean;
begin
  CheckRange(Index);
  Result := (FXSQLDa.sqlvar[Index].SqlInd <> nil) and
    // (rom) changed from "= -1" for robustness
    (FXSQLDa.sqlvar[Index].SqlInd^ <> BoolVals[False]);
end;

procedure TSQLDA.CheckRange(const Index: Word);
begin
  if Index >= Word(FXSQLDa.sqln) then
    raise Exception.CreateFmt(EUIB_FIELDNUMNOTFOUND, [Index]);
end;

function TSQLDA.DecodeString(const Code: Smallint; Index: Word): string;
begin
  with FXSQLDa.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        SetString(Result, SqlData, SqlLen);
      SQL_VARYING:
        SetString(Result, PVary(SqlData).vary_string, PVary(SqlData).vary_length);
    end;
end;

procedure TSQLDA.DecodeString(const Code: Smallint; Index: Word; out Str: string);
begin
  with FXSQLDa.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        SetString(Str, SqlData, SqlLen);
      SQL_VARYING:
        SetString(Str, PVary(SqlData).vary_string, PVary(SqlData).vary_length);
    end;
end;

procedure TSQLDA.DecodeWideString(const Code: Smallint; Index: Word; out Str: WideString);

  procedure SetWideString(var S: WideString; Buffer: PChar; Len: Integer);
  begin
    // (rom) a check for odd Len could be helpful
    SetLength(S, Len div 2);
    Move(Buffer^, PWideChar(S)^, Len);
  end;

begin
  with FXSQLDa.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        SetWideString(Str, SqlData, SqlLen);
      SQL_VARYING:
        SetWideString(Str, PVary(SqlData).vary_string, PVary(SqlData).vary_length);
    end;
end;

function TSQLDA.GetAsDouble(const Index: Word): Double;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  // (rom) changed from "(SqlInd <> nil) and (SqlInd^ = -1)" for better OO design
  // (rom) it is not inefficient because it replaces CheckRange
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          DecodeTimeStamp(PISCTimeStamp(SqlData), Result);
        SQL_TYPE_DATE:
          DecodeSQLDate(PInteger(SqlData)^, Result);
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToFloat(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToFloat(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsInt64(const Index: Word): Int64;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DecodeSQLDate(PISCTimeStamp(SqlData).timestamp_date); // Only Date
        SQL_TYPE_DATE:
          Result := DecodeSQLDate(PInteger(SqlData)^);
        SQL_TYPE_TIME:
          ; // Result := 0; What else ??
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToInt64(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToInt64(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsInteger(const Index: Word): Integer;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DecodeSQLDate(PISCTimeStamp(SqlData).timestamp_date); // Only Date
        SQL_TYPE_DATE:
          Result := DecodeSQLDate(PInteger(SqlData)^);
        SQL_TYPE_TIME:
          ; // Result := 0; What else ??
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToInt(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToInt(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsSingle(const Index: Word): Single;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          Result := DecodeTimeStamp(PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          Result := DecodeSQLDate(PInteger(SqlData)^);
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT: Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToFloat(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToFloat(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsSmallint(const Index: Word): Smallint;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DecodeSQLDate(PISCTimeStamp(SqlData).timestamp_date); // Only Date
        SQL_TYPE_DATE:
          Result := DecodeSQLDate(PInteger(SqlData)^);
        SQL_TYPE_TIME:
          ; // Result := 0; What else ??
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToInt(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToInt(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsString(const Index: Word): string;
var
  ASQLCode: Smallint;
begin
  Result := '';
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := FloatToStr(PSmallInt(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_LONG:
          Result := FloatToStr(PInteger(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          Result := FloatToStr(PInt64(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          Result := DateToStr(DecodeSQLDate(PInteger(SqlData)^));
        SQL_TYPE_TIME:
          Result := TimeToStr(PCardinal(SqlData)^ / 864000000);
        SQL_LONG:
          Result := IntToStr(PInteger(SqlData)^);
        SQL_D_FLOAT, SQL_FLOAT:
          Result := FloatToStr(PSingle(SqlData)^);
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := BoolToUIBStr(PSmallint(SqlData)^ = 1);
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := IntToStr(PSmallint(SqlData)^);
        SQL_INT64:
          Result := IntToStr(PInt64(SqlData)^);
        SQL_TEXT:
          DecodeString(SQL_TEXT, Index, Result);
        SQL_VARYING:
          DecodeString(SQL_VARYING, Index, Result);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetAsQuad(const Index: Word): TISCQuad;
begin
  with FXSQLDa.sqlvar[Index] do
    if not IsNull[Index] then
      case InternalGetSqlType(SqlType) of
        SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY:
          Result := PISCQuad(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end
    else
      Result := QuadNull;
end;

function TSQLDA.GetFieldCount: Integer;
begin
  Result := FXSQLDa.sqln;
end;

function TSQLDA.GetSQLType(const Index: Word): Smallint;
begin
  CheckRange(Index);
  Result := InternalGetSqlType(FXSQLDa.sqlvar[Index].SqlType);
end;

function TSQLDA.GetSQLLen(const Index: Word): Smallint;
begin
  CheckRange(Index);
  Result := FXSQLDa.sqlvar[Index].SqlLen;
end;

function TSQLDA.GetIsBlob(const Index: Word): Boolean;
begin
  CheckRange(Index);
  Result := (InternalGetSqlType(FXSQLDa.sqlvar[Index].SqlType) = SQL_BLOB);
end;

function TSQLDA.GetFieldIndex(const Name: string): Word;
begin
  for Result := 0 to GetAllocatedFields - 1 do
    if FXSQLDa.sqlvar[Result].AliasNameLength = Length(Name) then
      if StrLIComp(@FXSQLDa.sqlvar[Result].AliasName, PChar(Name),
        FXSQLDa.sqlvar[Result].AliasNameLength) = 0 then
        Exit;
  raise Exception.CreateFmt(EUIB_FIELDSTRNOTFOUND, [Name]);
end;

function TSQLDA.GetByNameAsDouble(const Name: string): Double;
begin
  Result := GetAsDouble(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsInt64(const Name: string): Int64;
begin
  Result := GetAsInt64(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsInteger(const Name: string): Integer;
begin
  Result := GetAsInteger(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsQuad(const Name: string): TISCQuad;
begin
  Result := GetAsQuad(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsSingle(const Name: string): Single;
begin
  Result := GetAsSingle(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsSmallint(const Name: string): Smallint;
begin
  Result := GetAsSmallint(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameAsString(const Name: string): string;
begin
  Result := GetAsString(GetFieldIndex(Name));
end;

function TSQLDA.GetAsVariant(const Index: Word): Variant;
var
  ASQLCode: Smallint;
  Dbl: Double;
begin
  Result := Null;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          Result := TDateTime(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          begin
            Dbl := DecodeSQLDate(PInteger(SqlData)^);
            Result := TDateTime(Dbl);
          end;
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT: Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := PSmallint(SqlData)^ = 1;
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := PSmallint(SqlData)^;
        {$IFDEF COMPILER6_UP}
        SQL_INT64:
          Result := PInt64(SqlData)^;
        {$ELSE}
        SQL_INT64:
          Result := Integer(PInt64(SqlData)^);
        {$ENDIF COMPILER6_UP}
        SQL_TEXT:
          Result := DecodeString(SQL_TEXT, Index);
        SQL_VARYING:
          Result := DecodeString(SQL_VARYING, Index);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsVariant(const Name: string): Variant;
begin
  Result := GetAsVariant(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameIsBlob(const Name: string): Boolean;
begin
  Result := GetIsBlob(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameIsNull(const Name: string): Boolean;
begin
  Result := GetIsNull(GetFieldIndex(Name));
end;

// (rom) why not implement through GetAsDouble?

function TSQLDA.GetAsDateTime(const Index: Word): TDateTime;
var
  ASQLCode: Smallint;
begin
  Result := 0.0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          DecodeTimeStamp(PISCTimeStamp(SqlData), Double(Result));
        SQL_TYPE_DATE:
          DecodeSQLDate(PInteger(SqlData)^, Double(Result));
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToDateTime(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToDateTime(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsDateTime(const Name: string): TDateTime;
begin
  // (rom) why not GetAsDateTime?
  Result := GetAsDouble(GetFieldIndex(Name));
end;

function TSQLDA.GetIsNullable(const Index: Word): Boolean;
begin
  CheckRange(Index);
  Result := (FXSQLDa.sqlvar[Index].SqlInd <> nil);
end;

function TSQLDA.GetByNameIsNullable(const Name: string): Boolean;
begin
  Result := GetIsNullable(GetFieldIndex(Name));
end;

function TSQLDA.GetAsCurrency(const Index: Word): Currency;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          Result := DecodeTimeStamp(PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          Result := DecodeSQLDate(PInteger(SqlData)^);
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToFloat(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToFloat(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsCurrency(const Name: string): Currency;
begin
  Result := GetAsCurrency(GetFieldIndex(Name));
end;

function TSQLDA.GetAsBoolean(const Index: Word): Boolean;
var
  ASQLCode: Smallint;
begin
  Result := False;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale] <> 0;
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale] <> 0;
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale] <> 0;
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^) > 0;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^) <> 0;
        SQL_LONG:
          Result := PInteger(SqlData)^ <> 0;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^) <> 0;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^ <> 0;
        SQL_INT64:
          Result := PInt64(SqlData)^ <> 0;
        SQL_TEXT:
          Result := StrToInt(DecodeString(SQL_TEXT, Index)) <> 0;
        SQL_VARYING:
          Result := StrToInt(DecodeString(SQL_VARYING, Index)) <> 0;
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsBoolean(const Name: string): Boolean;
begin
  Result := GetAsBoolean(GetFieldIndex(Name));
end;

function TSQLDA.GetByNameIsNumeric(const Name: string): Boolean;
begin
  Result := GetIsNumeric(GetFieldIndex(Name));
end;

function TSQLDA.GetIsNumeric(const Index: Word): Boolean;
begin
  CheckRange(Index);
  Result := InternalGetIsNumeric(FXSQLDa.sqlvar[Index].SqlScale);
end;

function TSQLDA.GetAsWideString(const Index: Word): WideString;
var
  ASQLCode: Smallint;
begin
  Result := '';
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := FloatToStr(PSmallInt(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_LONG:
          Result := FloatToStr(PInteger(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          Result := FloatToStr(PInt64(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          Result := DateToStr(DecodeSQLDate(PInteger(SqlData)^));
        SQL_TYPE_TIME:
          Result := TimeToStr(PCardinal(SqlData)^ / 864000000);
        SQL_LONG:
          Result := IntToStr(PInteger(SqlData)^);
        SQL_D_FLOAT, SQL_FLOAT:
          Result := FloatToStr(PSingle(SqlData)^);
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := BoolToUIBStr(PSmallint(SqlData)^ = 1);
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := IntToStr(PSmallint(SqlData)^);
        SQL_INT64:
          Result := IntToStr(PInt64(SqlData)^);
        SQL_TEXT:
          DecodeWideString(SQL_TEXT, Index, Result);
        SQL_VARYING:
          DecodeWideString(SQL_VARYING, Index, Result);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsWideString(const Name: string): WideString;
begin
  Result := GetAsWideString(GetFieldIndex(Name));
end;

function TSQLDA.GetFieldType(const Index: Word): TUIBFieldType;
begin
  CheckRange(Index);
  if InternalGetIsNumeric(FXSQLDa.sqlvar[Index].SqlScale) then
    Result := uftNumeric
  else
    case InternalGetSqlType(FXSQLDa.sqlvar[Index].SqlType) of
      SQL_TEXT:
       Result := uftChar;
      SQL_VARYING:
        Result := uftVarchar;
      SQL_SHORT:
        Result := uftSmallint;
      SQL_LONG:
        Result := uftInteger;
      SQL_FLOAT, SQL_D_FLOAT:
        Result := uftFloat;
      SQL_DOUBLE:
        Result := uftDoublePrecision;
      SQL_TIMESTAMP:
        Result := uftTimestamp;
      SQL_BLOB:
        Result := uftBlob;
      SQL_ARRAY, SQL_QUAD:
        Result := uftQuad;
      SQL_TYPE_TIME:
        Result := uftTime;
      SQL_TYPE_DATE:
        Result := uftDate;
      SQL_INT64:
        Result := uftInt64;
      {$IFDEF IB7_UP}
      SQL_BOOLEAN:
        Result := uftBoolean;
      {$ENDIF IB7_UP}
    else
      Result := uftUnKnown;
    end;
end;

function TSQLDA.GetAsDate(const Index: Word): Integer;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          DecodeSQLDate(PISCTimeStamp(SqlData).timestamp_date, Result);
        SQL_TYPE_DATE:
          DecodeSQLDate(PInteger(SqlData)^, Result);
        SQL_TYPE_TIME:
          Result := 0;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := Trunc(StrToDate(DecodeString(SQL_TEXT, Index)));
        SQL_VARYING:
          Result := Trunc(StrToDate(DecodeString(SQL_VARYING, Index)));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsDate(const Name: string): Integer;
begin
  Result := GetAsDate(GetFieldIndex(Name));
end;

function TSQLDA.GetAsTime(const Index: Word): Cardinal;
var
  ASQLCode: Smallint;
begin
  Result := 0;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ div ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := Trunc(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := PISCTimeStamp(SqlData).timestamp_time;
        SQL_TYPE_DATE:
          Result := 0;
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := Trunc(PSingle(SqlData)^);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          Result := PSmallint(SqlData)^;
        SQL_INT64:
          Result := PInt64(SqlData)^;
        SQL_TEXT:
          Result := StrToInt(DecodeString(SQL_TEXT, Index));
        SQL_VARYING:
          Result := StrToInt(DecodeString(SQL_VARYING, Index));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLDA.GetByNameAsTime(const Name: string): Cardinal;
begin
  Result := GetAsTime(GetFieldIndex(Name));
end;

//=== TSQLResult =============================================================

constructor TSQLResult.Create(Fields: Smallint = 0;
  CachedFetch: Boolean = False;
  FetchBlobs: Boolean = False;
  BufferChunks: Cardinal = 1000);
begin
  // (rom) added inherited Create
  inherited Create;
  FCachedFetch := CachedFetch;
  FFetchBlobs := FetchBlobs;
  FDataBufferLength := 0;
  FDataBuffer := nil;
  if Fields <= 0 then
    Fields := 0;
  GetMem(FXSQLDa, XSQLDA_LENGTH(Fields));
  FXSQLDa.sqln := Fields;
  FXSQLDa.sqld := Fields;
  FXSQLDa.version := SQLDA_CURRENT_VERSION;
  FBufferChunks := BufferChunks;
end;

destructor TSQLResult.Destroy;
begin
  ClearRecords;
  FreeMem(FXSQLDa);
  if FDataBuffer <> nil then
  begin
    if (not FCachedFetch) and FFetchBlobs then
      FreeBlobs(FDataBuffer);
    FreeMem(FDataBuffer)
  end;
  inherited Destroy;
end;

procedure TSQLResult.AddCurrentRecord;
begin
  if not Assigned(FMemoryPool) then
    FMemoryPool := TMemoryPool.Create(FDataBufferLength, FBufferChunks);
  Move(FDataBuffer^, FMemoryPool.New^, FDataBufferLength);
  FCurrentRecord := FMemoryPool.Count - 1;
end;

procedure TSQLResult.ClearRecords;
var
  I: Integer;
begin
  FScrollEOF := False;
  if Assigned(FMemoryPool) then
  begin
    if FFetchBlobs then
      for I := 0 to FMemoryPool.Count - 1 do
        FreeBlobs(FMemoryPool.Items[I]);
    FMemoryPool.Free;
    FMemoryPool := nil;
  end;
end;

procedure TSQLResult.GetRecord(const Index: Integer);
begin
  if Index <> FCurrentRecord then
  begin
    Move(FMemoryPool.Items[Index]^, FDataBuffer^, FDataBufferLength);
    FCurrentRecord := Index;
  end;
end;

function TSQLResult.GetRecordCount: Integer;
begin
  if Assigned(FMemoryPool) then
    Result := FMemoryPool.Count
  else
    Result := 0;
end;

procedure TSQLResult.AllocateDataBuffer;
var
  I, LastLen: Smallint;
  BlobCount: Word;
begin
  FDataBufferLength := 0;
  LastLen := 0;
  BlobCount := 0;
  SetLength(FBlobsIndex, BlobCount);
  // calculate offsets and store them instead of pointers ;)
  for I := 0 to FXSQLDa.sqln - 1 do
  begin
    Inc(FDataBufferLength, LastLen);
    FXSQLDa.sqlvar[I].SqlData := Pointer(FDataBufferLength);
    if InternalGetSqlType(FXSQLDa.sqlvar[I].SqlType) = SQL_VARYING then
      LastLen := FXSQLDa.sqlvar[I].SqlLen + 2
    else
      LastLen := FXSQLDa.sqlvar[I].SqlLen;
    if (FXSQLDa.sqlvar[I].SqlType and 1) = 1 then
    begin
      Inc(FDataBufferLength, LastLen);
      FXSQLDa.sqlvar[I].SqlInd := Pointer(FDataBufferLength);
      LastLen := 2; // SizeOf(Smallint)
    end
    else
      FXSQLDa.sqlvar[I].SqlInd := nil;
    // count blobs
    if FFetchBlobs and (InternalGetSqlType(FXSQLDa.sqlvar[I].SqlType) = SQL_BLOB) then
    begin
      Inc(BlobCount);
      SetLength(FBlobsIndex, BlobCount);
      FBlobsIndex[BlobCount - 1] := I;
    end;
  end;
  Inc(FDataBufferLength, LastLen);
  Inc(FDataBufferLength, BlobCount * SizeOf(TBlobData)); // Size + Pointer

  // Now we have the total length needed
  if FDataBuffer = nil then
    GetMem(FDataBuffer, FDataBufferLength {+ (FXSQLDa.sqln * 2)})
  else
    ReallocMem(FDataBuffer, FDataBufferLength {+ (FXSQLDa.sqln * 2)});
  FillChar(FDataBuffer^, FDataBufferLength, 0);
  FBlobArray := FDataBuffer;
  Inc(Integer(FBlobArray), FDataBufferLength - BlobCount * SizeOf(TBlobData));

  // increment Offsets with the buffer
  for I := 0 to FXSQLDa.sqln - 1 do
  begin
    // (rom) changed typecast to Cardinal
    Inc(Cardinal(FXSQLDa.sqlvar[I].SqlData), Cardinal(FDataBuffer));
    if FXSQLDa.sqlvar[I].SqlInd <> nil then
      Inc(Cardinal(FXSQLDa.sqlvar[I].SqlInd), Cardinal(FDataBuffer));
  end;
end;

procedure TSQLResult.SaveToStream(Stream: TStream);
var
  RecCount, I, J: Integer;
  BlobArray: PBlobDataArray;
begin
  Stream.Write(FCachedFetch, SizeOf(FCachedFetch));
  Stream.Write(FFetchBlobs, SizeOf(FFetchBlobs));

  Stream.Write(FXSQLDa.sqln, SizeOf(FXSQLDa.sqln));
  Stream.Write(FXSQLDa^, XSQLDA_LENGTH(FXSQLDa.sqln)); // MetaData
  RecCount := RecordCount;
  Stream.Write(RecCount, SizeOf(RecCount));
  for I := 0 to RecCount - 1 do
  begin
    Stream.Write(FMemoryPool.Items[I]^, FDataBufferLength);
    for J := 0 to Length(FBlobsIndex) - 1 do
    begin
      BlobArray := Pointer(Integer(FMemoryPool.Items[I]) + FDataBufferLength - (Length(FBlobsIndex) * 8));
      Stream.Write(BlobArray[J].Size, 4);
      Stream.Write(BlobArray[J].Buffer^, BlobArray[J].Size);
    end;
  end;
end;

procedure TSQLResult.LoadFromStream(Stream: TStream);
var
  Fields: Smallint;
  RecCount, I, J: Integer;
begin
  // CleanUp
  ClearRecords;
  if (not FCachedFetch) and FFetchBlobs then
    FreeBlobs(FDataBuffer);

  Stream.Read(FCachedFetch, SizeOf(FCachedFetch));
  Stream.Read(FFetchBlobs, SizeOf(FFetchBlobs));

  Stream.Read(Fields, SizeOf(Fields));
  SetAllocatedFields(Fields);
  Stream.Read(FXSQLDa^, XSQLDA_LENGTH(Fields));

  // realloc & index buffer
  AllocateDataBuffer;
  Stream.Read(RecCount, SizeOf(RecCount));
  FBufferChunks := RecCount; // Inprove memory allocation
  for I := 0 to RecCount - 1 do
  begin
    Stream.Read(FDataBuffer^, FDataBufferLength);
    for J := 0 to Length(FBlobsIndex) - 1 do
    begin
      Stream.Read(FBlobArray[J].Size, 4);
      if FBlobArray[J].Size > 0 then
      begin
        GetMem(FBlobArray[J].Buffer, FBlobArray[J].Size);
        Stream.Read(FBlobArray[J].Buffer^, FBlobArray[J].Size);
      end
      else
        FBlobArray[J].Buffer := nil;
    end;
    AddCurrentRecord;
  end;

  FScrollEOF := True;
end;

function TSQLResult.GetCurrentRecord: Integer;
begin
  if FMemoryPool = nil then
    Result := -1
  else
    Result := FCurrentRecord;
end;

procedure TSQLResult.FreeBlobs(Buffer: Pointer);
var
  BlobArray: PBlobDataArray;
  I: Integer;
begin
  BlobArray := Pointer(Integer(Buffer) + FDataBufferLength - (Length(FBlobsIndex) * 8));
  for I := 0 to Length(FBlobsIndex) - 1 do
    if BlobArray[I].Size > 0 then
      FreeMem(BlobArray[I].Buffer);
end;

function TSQLResult.GetBlobIndex(const Index: Word): Word;
begin
  if FFetchBlobs then
  begin
    for Result := 0 to Length(FBlobsIndex) - 1 do
      if FBlobsIndex[Result] = Index then
        Exit;
    raise Exception.CreateFmt(EUIB_BLOBFIELDNOTFOUND, [Index]);
  end
  else
    raise Exception.Create(EUIB_FETCHBLOBNOTSET);
end;

function TSQLResult.GetEof: Boolean;
begin
  Result := FScrollEOF;
end;

procedure TSQLResult.ReadBlob(const Index: Word; var Str: string);
begin
  CheckRange(Index);
  with FBlobArray[GetBlobIndex(Index)] do
  begin
    SetLength(Str, Size);
    Move(Buffer^, PChar(Str)^, Size);
  end;
end;

procedure TSQLResult.ReadBlob(const Index: Word; Stream: TStream);
begin
  CheckRange(Index);
  with FBlobArray[GetBlobIndex(Index)] do
  begin
    Stream.Seek(0, 0);
    Stream.Write(Buffer^, Size);
    Stream.Seek(0, 0);
  end;
end;

procedure TSQLResult.ReadBlob(const Index: Word; var Value: Variant);
var
  PData: Pointer;
begin
  CheckRange(Index);
  with FBlobArray[GetBlobIndex(Index)] do
  begin
    Value := VarArrayCreate([0, Size - 1], varByte);
    PData := VarArrayLock(Value);
    try
      Move(Buffer^, PData^, Size);
    finally
      VarArrayUnlock(Value);
    end;
  end;
end;

procedure TSQLResult.ReadBlob(const Index: Word; var Str: WideString);
begin
  CheckRange(Index);
  with FBlobArray[GetBlobIndex(Index)] do
  begin
    SetLength(Str, Size);
    Move(Buffer^, PWideChar(Str)^, Size);
  end;
end;

procedure TSQLResult.ReadBlob(const Index: Word; Data: Pointer);
begin
  CheckRange(Index);
  with FBlobArray[GetBlobIndex(Index)] do
    Move(Buffer^, Data^, Size);
end;

procedure TSQLResult.ReadBlob(const Name: string; Data: Pointer);
begin
  ReadBlob(GetFieldIndex(Name), Data);
end;

procedure TSQLResult.ReadBlob(const Name: string; var Str: WideString);
begin
  ReadBlob(GetFieldIndex(Name), Str);
end;

procedure TSQLResult.ReadBlob(const Name: string; var Str: string);
begin
  ReadBlob(GetFieldIndex(Name), Str);
end;

procedure TSQLResult.ReadBlob(const Name: string; Stream: TStream);
begin
  ReadBlob(GetFieldIndex(Name), Stream);
end;

procedure TSQLResult.ReadBlob(const Name: string; var Value: Variant);
begin
  ReadBlob(GetFieldIndex(Name), Value);
end;

function TSQLResult.GetBlobSize(const Index: Word): Cardinal;
begin
  CheckRange(Index);
  Result := FBlobArray[GetBlobIndex(Index)].Size;
end;

function TSQLResult.GetAsString(const Index: Word): string;
var
  ASQLCode: Smallint;
begin
  Result := '';
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := FloatToStr(PSmallInt(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_LONG:
          Result := FloatToStr(PInteger(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          Result := FloatToStr(PInt64(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          Result := DateToStr(DecodeSQLDate(PInteger(SqlData)^));
        SQL_TYPE_TIME:
          Result := TimeToStr(PCardinal(SqlData)^ / 864000000);
        SQL_LONG:
          Result := IntToStr(PInteger(SqlData)^);
        SQL_D_FLOAT, SQL_FLOAT:
          Result := FloatToStr(PSingle(SqlData)^);
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := BoolToUIBStr(PSmallint(SqlData)^ = 1);
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := IntToStr(PSmallint(SqlData)^);
        SQL_INT64:
          Result := IntToStr(PInt64(SqlData)^);
        SQL_TEXT:
          DecodeString(SQL_TEXT, Index, Result);
        SQL_VARYING:
          DecodeString(SQL_VARYING, Index, Result);
        SQL_BLOB:
          ReadBlob(Index, Result);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLResult.GetAsWideString(const Index: Word): WideString;
var
  ASQLCode: Smallint;
begin
  Result := '';
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := FloatToStr(PSmallInt(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_LONG:
          Result := FloatToStr(PInteger(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          Result := FloatToStr(PInt64(SqlData)^ / ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := FloatToStr(PDouble(SqlData)^);
        SQL_TIMESTAMP:
          Result := DateTimeToStr(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          Result := DateToStr(DecodeSQLDate(PInteger(SqlData)^));
        SQL_TYPE_TIME:
          Result := TimeToStr(PCardinal(SqlData)^ / 864000000);
        SQL_LONG:
          Result := IntToStr(PInteger(SqlData)^);
        SQL_D_FLOAT, SQL_FLOAT:
          Result := FloatToStr(PSingle(SqlData)^);
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := BoolToUIBStr(PSmallint(SqlData)^ = 1);
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := IntToStr(PSmallint(SqlData)^);
        SQL_INT64:
          Result := IntToStr(PInt64(SqlData)^);
        SQL_TEXT:
          DecodeWideString(SQL_TEXT, Index, Result);
        SQL_VARYING:
          DecodeWideString(SQL_VARYING, Index, Result);
        SQL_BLOB:
          ReadBlob(Index, Result);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLResult.GetAsVariant(const Index: Word): Variant;
var
  ASQLCode: Smallint;
  Dbl: Double;
begin
  Result := Null;
  if IsNull[Index] then
    Exit;
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          Result := PSmallInt(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_LONG:
          Result := PInteger(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          Result := PInt64(SqlData)^ / ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          Result := PDouble(SqlData)^;
        SQL_TIMESTAMP:
          Result := TDateTime(DecodeTimeStamp(PISCTimeStamp(SqlData)));
        SQL_TYPE_DATE:
          begin
            Dbl := DecodeSQLDate(PInteger(SqlData)^);
            Result := TDateTime(Dbl);
          end;
        SQL_TYPE_TIME:
          Result := PCardinal(SqlData)^ / 864000000;
        SQL_LONG:
          Result := PInteger(SqlData)^;
        SQL_D_FLOAT, SQL_FLOAT:
          Result := PSingle(SqlData)^;
        {$IFDEF IB7_UP}
        SQL_BOOLEAN:
          Result := WordBool(PSmallint(SqlData)^);
        {$ENDIF IB7_UP}
        SQL_SHORT:
          Result := PSmallint(SqlData)^;
        {$IFDEF COMPILER6_UP}
        SQL_INT64:
          Result := PInt64(SqlData)^;
        {$ELSE}
        SQL_INT64:
          Result := Integer(PInt64(SqlData)^);
        {$ENDIF COMPILER6_UP}
        SQL_TEXT:
          Result := DecodeString(SQL_TEXT, Index);
        SQL_VARYING:
          Result := DecodeString(SQL_VARYING, Index);
        SQL_BLOB:
          ReadBlob(Index, Result);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
  end;
end;

function TSQLResult.GetUniqueRelationName: string;
var
  I: Integer;
begin
  Result := '';
  if FXSQLDa.sqln > 1 then
    for I := 0 to FXSQLDa.sqln - 2 do
      if not ((FXSQLDa.sqlvar[I].RelNameLength = FXSQLDa.sqlvar[I + 1].RelNameLength) and
        (CompareText(FXSQLDa.sqlvar[I].RelName, FXSQLDa.sqlvar[I + 1].RelName) = 0)) then
        exit;
  if FXSQLDa.sqln > 0 then
    SetString(Result, FXSQLDa.sqlvar[0].RelName, FXSQLDa.sqlvar[0].RelNameLength);
end;

//=== TMemoryPool ============================================================

constructor TMemoryPool.Create(ItemSize, ItemsInPage: Integer);
const
  PageSizeAdjustment = SizeOf(TPageInfo);
  MaxPageSize = (64 * 1024) + (PageSizeAdjustment * 2);
var
  RealItemSize, TestSize: Integer;
const
  MinItemSize = SizeOf(Word) + SizeOf(Pointer);

  function Max(A, B: Integer): Integer;
  {$IFDEF FPC}
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;
  {$ELSE}
  asm
    cmp eax, edx
    jge @@Exit
    mov eax, edx
  @@Exit:
  end;
  {$ENDIF FPC}

begin
  // (rom) added inherited Create
  inherited Create;
  FList := TList.Create;
  FItemSize := Max(ItemSize, MinItemSize);
  FItemsInPage := ItemsInPage;
  RealItemSize := FItemSize + SizeOf(Word);
  TestSize := (RealItemSize * FItemsInPage) + PageSizeAdjustment;
  if TestSize > MaxPageSize then
  begin
    FItemsInPage := (MaxPageSize - PageSizeAdjustment) div RealItemSize;
    TestSize := (RealItemSize * FItemsInPage) + PageSizeAdjustment;
  end;
  FPageSize := TestSize;
end;

destructor TMemoryPool.Destroy;
var
  Temp, Next: PPageInfo;
begin
  Temp := FFirstPage;
  while Assigned(Temp) do
  begin
    Next := Temp^.NextPage;
    FreeMem(Temp, FPageSize);
    Temp := Next;
  end;
  FList.Free;
  // (rom) added inherited Destroy
  inherited Destroy;
end;

procedure TMemoryPool.AddPage;
var
  Page: PPageInfo;
  Temp: PAnsiChar;
  Prev: Pointer;
  I: Integer;
begin
  GetMem(Page, FPageSize);
  Page^.NextPage := FFirstPage;
  Page^.UsageCounter := 0;
  FFirstPage := Page;
  Temp := PAnsiChar(Page);
  Inc(Temp, SizeOf(Pointer) + SizeOf(Integer));
  Prev := nil;
  for I := 0 to FItemsInPage-1 do
  begin
    PWord(Temp)^ := Temp - PAnsiChar(Page);
    Inc(Temp, SizeOf(Word));
    PPointer(Temp)^ := Prev;
    Prev := Temp;
    Inc(Temp, FItemSize);
  end;
  FFreeList := Prev;
end;

function TMemoryPool.New: Pointer;
var
  Page: PPageInfo;
  Temp: PAnsiChar;
begin
  if not Assigned(FFreeList) then
    AddPage;
  Result := FFreeList;
  FFreeList := PPointer(Result)^;
  Temp := Result;
  Dec(Temp, SizeOf(Word));
  Dec(Temp, PWord(Temp)^);
  Page := PPageInfo(Temp);
  Inc(Page^.UsageCounter);
  FList.Add(Result);
end;

function TMemoryPool.PageCount: Integer;
var
  Temp: PPageInfo;
begin
  Result := 0;
  Temp := FFirstPage;
  while Assigned(Temp) do
  begin
    Inc(Result);
    Temp := Temp^.NextPage;
  end;
end;

function TMemoryPool.PageUsageCount(const PageIndex: Integer): Integer;
var
  Index: Integer;
  Temp: PPageInfo;
begin
  Result := -1;
  Index := 0;
  Temp := FFirstPage;
  while Assigned(Temp) and (Index <= PageIndex) do
  begin
    if Index = PageIndex then
    begin
      Result := Temp^.UsageCounter;
      Break;
    end
    else
    begin
      Inc(Index);
      Temp := Temp^.NextPage;
    end;
  end;
end;

procedure TMemoryPool.Dispose(var P: Pointer);
var
  Page: PPageInfo;
  Temp: PAnsiChar;
begin
  PPointer(P)^ := FFreeList;
  FFreeList := P;
  Temp := FFreeList;
  Dec(Temp, SizeOf(Word));
  Dec(Temp, PWord(Temp)^);
  Page := PPageInfo(Temp);
  Dec(Page^.UsageCounter);
  P := nil;
end;

procedure TMemoryPool.CleanFreeList(const PageStart: Pointer);
var
  PageEnd: Pointer;
  ItemsFound: Integer;
  Prev: Pointer;
  Temp: Pointer;
begin
  PageEnd := PAnsiChar(PageStart) + FPageSize;
  ItemsFound := 0;
  Prev := nil;
  Temp := FFreeList;
  while Assigned(Temp) and (ItemsFound < FItemsInPage) do
  begin
    if (PAnsiChar(Temp) > PageStart) and (PAnsiChar(Temp) <= PageEnd) then
    begin
      Inc(ItemsFound);
      if Temp = FFreeList then
        FFreeList := PPointer(Temp)^
      else
        PPointer(Prev)^ := PPointer(Temp)^;
      Temp := PPointer(Temp)^;
    end
    else
    begin
      Prev := Temp;
      Temp := PPointer(Temp)^;
    end;
  end;
end;

function TMemoryPool.RemoveUnusedPages: Integer;
var
  Next, Prev, Temp: PPageInfo;
begin
  Result := 0;
  Prev := nil;
  Temp := FFirstPage;
  while Assigned(Temp) do
  begin
    Next := Temp^.NextPage;
    if Temp^.UsageCounter = 0 then
    begin
      if Temp = FFirstPage then
        FFirstPage := Next
      else
      if Assigned(Prev) then
        Prev^.NextPage := Next;
      CleanFreeList(Temp);
      Freemem(Temp, FPageSize);
      Inc(Result);
    end
    else
      Prev := Temp;
    Temp := Next;
  end;
end;

function TMemoryPool.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMemoryPool.GetItems(const Index: Integer): Pointer;
begin
  Result := FList.Items[Index];
end;

//=== TSQLParams =============================================================

constructor TSQLParams.Create;
begin
  // (rom) added inherited Create
  inherited Create;
  GetMem(FXSQLDa, XSQLDA_LENGTH(0));
  FillChar(FXSQLDa^, XSQLDA_LENGTH(0), 0);
  FXSQLDa.sqln := 0;
  FXSQLDa.sqld := 0;
  FXSQLDa.version := SQLDA_CURRENT_VERSION;
  FParamCount := 0;
end;

destructor TSQLParams.Destroy;
begin
  Clear;
  FreeMem(FXSQLDa);
  inherited Destroy;
end;

procedure TSQLParams.Clear;
var
  I, J: Smallint;
begin
  for I := 0 to FXSQLDa.sqln - 1 do
  begin
    if FXSQLDa.sqlvar[I].SqlInd <> nil then
    begin
      FreeMem(FXSQLDa.sqlvar[I].SqlData);
      FreeMem(FXSQLDa.sqlvar[I].SqlInd);
      // don't free shared pointers
      for J := I + 1 to FXSQLDa.sqln - 1 do
        if FXSQLDa.sqlvar[I].ID = FXSQLDa.sqlvar[J].ID then
        begin
          FXSQLDa.sqlvar[J].SqlData := nil;
          FXSQLDa.sqlvar[J].SqlInd := nil;
        end;
    end;
  end;
  FXSQLDa.sqln := 0;
  FXSQLDa.sqld := 0;
  ReallocMem(FXSQLDa, XSQLDA_LENGTH(0));
  FParamCount := 0;
end;

function TSQLParams.GetFieldName(const Index: Word): string;
begin
  CheckRange(Index);
  SetString(Result, FXSQLDa.sqlvar[Index].ParamName,
    FXSQLDa.sqlvar[Index].ParamNameLength);
end;

procedure TSQLParams.AddFieldType(const Name: string; FieldType: TUIBFieldType;
  Scale: TScale = 1; Precision: Byte = 0);
begin
  case FieldType of
    uftNumeric:
      begin
        case Precision of
          0..4:
            SetFieldType(AddField(Name), SizeOf(Smallint), SQL_SHORT + 1, -scale);
          5..7:
            SetFieldType(AddField(Name), SizeOf(Integer), SQL_LONG + 1, -scale);
        else
          SetFieldType(AddField(Name), SizeOf(Int64), SQL_INT64 + 1, -scale);
        end;
      end;
    uftChar, uftVarchar, uftCstring:
      SetFieldType(AddField(Name), 0, SQL_TEXT + 1, 0);
    uftSmallint:
      SetFieldType(AddField(Name), SizeOf(Smallint), SQL_SHORT + 1, 0);
    uftInteger:
      SetFieldType(AddField(Name), SizeOf(Integer), SQL_LONG + 1, 0);
    uftQuad:
      SetFieldType(AddField(Name), SizeOf(TISCQuad), SQL_QUAD + 1, 0);
    uftFloat:
      SetFieldType(AddField(Name), SizeOf(Single), SQL_FLOAT + 1, 0);
    uftDoublePrecision:
      SetFieldType(AddField(Name), SizeOf(Double), SQL_DOUBLE + 1, 0);
    uftTimestamp:
      SetFieldType(AddField(Name), SizeOf(TISCTimeStamp), SQL_TIMESTAMP + 1, 0);
    uftBlob, uftBlobId:
      SetFieldType(AddField(Name), SizeOf(TISCQuad), SQL_BLOB + 1, 0);
    uftDate:
      SetFieldType(AddField(Name), SizeOf(Integer), SQL_TYPE_DATE + 1, 0);
    uftTime:
      SetFieldType(AddField(Name), SizeOf(Cardinal), SQL_TYPE_TIME + 1, 0);
    uftInt64:
      SetFieldType(AddField(Name), SizeOf(Int64), SQL_INT64 + 1, 0);
    {$IFDEF IB7_UP}
    uftBoolean:
      SetFieldType(AddField(Name), SizeOf(Smallint), SQL_BOOLEAN + 1, 0);
    {$ENDIF IB7_UP}
  end;
end;

procedure TSQLParams.SetFieldType(const Index: Word; Size: Integer; Code,
  Scale: Smallint);
var
  I: Word;
begin
  CheckRange(Index);
  with FXSQLDa.sqlvar[Index] do
    if Init then // need to be set, cf addfield
    begin
      Init := False; // don't need to be set
      SqlType := Code;
      SqlScale := Scale;
      SqlLen := Size;
      if Size > 0 then
        GetMem(SqlData, Size)
      else
        SqlData := nil;
      if ParamNameLength > 0 then
        for I := 0 to GetAllocatedFields - 1 do
          if (I <> Index) and (ID = FXSQLDa.sqlvar[I].ID) then
            Move(FXSQLDa.sqlvar[Index], FXSQLDa.sqlvar[I], SizeOf(TUIBSQLVar) - MaxParamLength - 2);
    end;
end;

function TSQLParams.Parse(const SQL: string): string;
const
  Identifiers: set of Char = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
var
  Src: PChar;
  Dest, IdLen: Word;

  procedure Next;
  begin
    Inc(Dest);
    Result[Dest] := Src^;
    Inc(Src);
  end;

  procedure Skip(C: Char);
  begin
    repeat
      Next;
      if Src^ = C then
      begin
        Next;
        Break;
      end;
    until Src^ = #0;
  end;

  {$IFDEF FPC}
  function PrevChar(C: PChar): Char;
  begin
    Dec(C);
    Result := C^;
  end;
  {$ENDIF FPC}

begin
  Clear;
  Src := PChar(SQL);
  Dest := 0;
  SetLength(Result, Length(SQL));
  while True do
    case Src^ of
      // eof
      #0:
        begin
          SetLength(Result, Dest);
          Exit;
        end;
      // normal comments
      '/':
        if Src[1] = '*' then
        begin
          Inc(Src, 2);
          while Src^ <> #0 do
            if (Src^ = '*') and (Src[1] = '/') then
            begin
              Inc(Src, 2);
              Break;
            end
            else
              Inc(Src);
        end
        else
          Next;
      // Firebird comments -- My comment + (eol or eof)
      {.$IFDEF FB15_UP}
      '-':
        if Src[1] = '-' then
        begin
          Inc(Src, 2);
          while not (Src^ in [#0, #13, #10]) do
            Inc(Src);
        end
        else
          Next;
      {.$ENDIF FB15_UP}
      // text ''
      '''':
        Skip('''');
      // text ""
      '"':
        Skip('"');
      // Unnamed Input
      '?':
        begin
          AddField('');
          Next;
        end;
      // Named Input
      ':':
        begin
          Inc(Dest);
          Result[Dest] := '?';
          Inc(Src);
          IdLen := 0;
          while Src[IdLen] in Identifiers do
            Inc(IdLen);
          AddField(Copy(Src, 0, IdLen));
          Inc(Src, IdLen);
        end;
      // skip everything when begin identifier found !
      // in procedures
      'b', 'B':
        begin
          if not ((Dest > 0) and
            ({$IFDEF FPC} PrevChar(src) {$ELSE} src[-1] {$ENDIF} in Identifiers)) and
            (CompareText(Copy(Src, 0, 5), 'begin') = 0) and
            not (Src[5] in Identifiers) then
            while (Src^ <> #0) do
              Next
          else
            Next;
        end;
    else
      Next;
    end;
end;

function TSQLParams.GetFieldType(const Index: Word): TUIBFieldType;
begin
  if IsNull[Index] and FXSQLDa.sqlvar[Index].Init then
    Result := uftUnKnown
  else
    Result := inherited GetFieldType(Index);
end;

function TSQLParams.GetFieldIndex(const Name: string): Word;
begin
  if not FindParam(Name, Result) then
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsBoolean(const Name: string;
  const Value: Boolean);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsBoolean(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsDate(const Name: string;
  const Value: Integer);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsDate(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsCurrency(const Name: string;
  const Value: Currency);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsCurrency(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsDateTime(const Name: string;
  const Value: TDateTime);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsDateTime(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsDouble(const Name: string;
  const Value: Double);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsDouble(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsInt64(const Name: string;
  const Value: Int64);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsInt64(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsInteger(const Name: string;
  const Value: Integer);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsInteger(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsQuad(const Name: string;
  const Value: TISCQuad);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsQuad(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsSingle(const Name: string;
  const Value: Single);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsSingle(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsSmallint(const Name: string;
  const Value: Smallint);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsSmallint(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsString(const Name, Value: string);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsString(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameAsWideString(const Name: string; const Value: WideString);
var
  Field: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Field) then
    SetAsWideString(Field, Value)
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.SetByNameIsNull(const Name: string;
  const Value: Boolean);
var
  Index: Word;
begin
  if (Length(Name) > 0) and FindParam(Name, Index) then
    IsNull[Index] := Value
  else
    raise Exception.CreateFmt(EUIB_PARAMSTRNOTFOUND, [Name]);
end;

procedure TSQLParams.EncodeString(Code: Smallint; Index: Word; const Str: string);
var
  I: smallint;
  OldLen: Smallint;
  NewLen: Integer;
begin
  OldLen := FXSQLDa.sqlvar[Index].SqlLen;
  with FXSQLDa.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        begin
          NewLen := Length(Str);
          if NewLen = 0 then
            Inc(NewLen); // empty string not null
          if SqlLen = 0 then
            GetMem(SqlData, NewLen)
          else
            ReallocMem(SqlData, NewLen);
          SqlLen := NewLen;
          Move(PChar(Str)^, SqlData^, SqlLen);
        end;
      SQL_VARYING:
        begin
          NewLen := Length(Str);
          if NewLen = 0 then
            Inc(NewLen); // empty string not null
          if SqlLen = 0 then
            GetMem(SqlData, NewLen + 2)
          else

            ReallocMem(SqlData, NewLen + 2);
          SqlLen := NewLen + 2;
          PVary(SqlData).vary_length := NewLen;
          Move(PChar(Str)^, PVary(SqlData).vary_string, PVary(SqlData).vary_length);
        end;
    end;

  // named parameters share the same memory !!
  with FXSQLDa.sqlvar[Index] do
    if (ParamNameLength > 0) and (OldLen <> SqlLen) then
      for I := 0 to FXSQLDa.sqln - 1 do
        if FXSQLDa.sqlvar[I].ID = ID then
        begin
          FXSQLDa.sqlvar[I].SqlData := SqlData;
          FXSQLDa.sqlvar[I].SqlLen := SqlLen;
        end;
end;

procedure TSQLParams.EncodeWideString(Code: Smallint; Index: Word; const Str: WideString);
var
  I: smallint;
  OldLen: Smallint;
  NewLen: Integer;
begin
  OldLen := FXSQLDa.sqlvar[Index].SqlLen;
  with FXSQLDa.sqlvar[Index] do
    case Code of
      SQL_TEXT:
        begin
          NewLen := Length(Str) * 2;
          if SqlLen = 0 then
            GetMem(SqlData, NewLen)
          else
            ReallocMem(SqlData, NewLen);
          SqlLen := NewLen;
          Move(PWideChar(Str)^, SqlData^, SqlLen);
        end;
      SQL_VARYING:
        begin
          NewLen := Length(Str) * 2;
          if SqlLen = 0 then
            GetMem(SqlData, NewLen + 2)
          else
            ReallocMem(SqlData, NewLen + 2);
          SqlLen := NewLen + 2;
          PVary(SqlData).vary_length := NewLen;
          Move(PWideChar(Str)^, PVary(SqlData).vary_string, PVary(SqlData).vary_length);
        end;
    end;

  // named parameters share the same memory !!
  with FXSQLDa.sqlvar[Index] do
    if (ParamNameLength > 0) and (OldLen <> SqlLen) then
      for I := 0 to FXSQLDa.sqln - 1 do
        if FXSQLDa.sqlvar[I].ID = ID then
        begin
          FXSQLDa.sqlvar[I].SqlData := SqlData;
          FXSQLDa.sqlvar[I].SqlLen := SqlLen;
        end;
end;

procedure TSQLParams.SetAsQuad(const Index: Word; const Value: TISCQuad);
begin
  SetFieldType(Index, SizeOf(TISCQuad), SQL_QUAD + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    case InternalGetSqlType(SqlType) of
      SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY:
        PISCQuad(SqlData)^ := Value;
    else
      raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
    end;
    if SqlInd <> nil then
      SqlInd^ := BoolVals[CompareMem(@Value, @QuadNull, SizeOf(TIscQuad))];
  end;
end;

procedure TSQLParams.SetAsDateTime(const Index: Word;
  const Value: TDateTime);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(TISCQuad), SQL_TIMESTAMP + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(Value) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value);
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(Value);
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(Value);
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, DateTimeToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, DateTimeToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsDate(const Index: Word; const Value: Integer);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Integer), SQL_TYPE_DATE + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := 0;
        SQL_LONG:
          PInteger(SqlData)^ := Value;
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Value;
        SQL_INT64:
          PInt64(SqlData)^ := Value;
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, DateToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, DateToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsTime(const Index: Word; const Value: Cardinal);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Cardinal), SQL_TYPE_TIME + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_DOUBLE: PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          PInteger(SqlData)^ := 0;
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Value;
        SQL_LONG:
          PInteger(SqlData)^ := Value;
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Value;
        SQL_INT64:
          PInt64(SqlData)^ := Value;
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, TimeToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, TimeToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsBoolean(const Index: Word; const Value: Boolean);
var
  ASQLCode: Smallint;
begin
  {$IFDEF IB7_UP}
  SetFieldType(Index, SizeOf(Smallint), SQL_BOOLEAN + 1);
  {$ELSE}
  SetFieldType(Index, SizeOf(Smallint), SQL_SHORT + 1);
  {$ENDIF IB7_UP}
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Ord(Value) * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Ord(Value) * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Ord(Value) * ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          PDouble(SqlData)^ := Ord(Value);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Ord(Value);
        SQL_LONG:
          PInteger(SqlData)^ := Ord(Value);
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Ord(Value);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Ord(Value);
        SQL_INT64:
          PInt64(SqlData)^ := Ord(Value);
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, IntToStr(Ord(Value)));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, IntToStr(Ord(Value)));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsInteger(const Index: Word; const Value: Integer);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Integer), SQL_LONG + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := 0;
        SQL_LONG:
          PInteger(SqlData)^ := Value;
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Value;
        SQL_INT64:
          PInt64(SqlData)^ := Value;
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsSingle(const Index: Word; const Value: Single);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Single), SQL_FLOAT + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(Value) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value);
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(Value);
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(Value);
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, FloatToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, FloatToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsSmallint(const Index: Word; const Value: Smallint);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Smallint), SQL_SHORT + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := 0;
        SQL_LONG:
          PInteger(SqlData)^ := Value;
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Value;
        SQL_INT64:
          PInt64(SqlData)^ := Value;
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsString(const Index: Word; const Value: string);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, Length(Value), SQL_TEXT + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := StrToFloat(Value);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := StrToFloat(Value);
        SQL_TIMESTAMP:
          EncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(StrToDate(Value), PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(StrToTime(Value)) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := StrToFloat(Value);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, Value);
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, Value);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsWideString(const Index: Word;
  const Value: WideString);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, Length(Value), SQL_TEXT + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(StrToFloat(Value) * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := StrToFloat(Value);
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := StrToFloat(Value);
        SQL_TIMESTAMP:
          EncodeTimeStamp(StrToDateTime(Value), PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(StrToDate(Value), PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(StrToTime(Value)) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := StrToFloat(Value);
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(StrToFloat(Value));
        SQL_TEXT:
          EncodeWideString(SQL_TEXT, Index, Value);
        SQL_VARYING:
          EncodeWideString(SQL_VARYING, Index, Value);
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsInt64(const Index: Word; const Value: Int64);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Int64), SQL_INT64 + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_LONG:
          PInteger(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Value * ScaleDivisor[SqlScale];
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := 0;
        SQL_LONG:
          PInteger(SqlData)^ := Value;
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Value;
        SQL_INT64:
          PInt64(SqlData)^ := Value;
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, IntToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, IntToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsDouble(const Index: Word; const Value: Double);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Double), SQL_DOUBLE + 1);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(Value) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value);
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(Value);
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(Value);
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, FloatToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, FloatToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetAsCurrency(const Index: Word;
  const Value: Currency);
var
  ASQLCode: Smallint;
begin
  SetFieldType(Index, SizeOf(Int64), SQL_INT64 + 1, -4);
  with FXSQLDa.sqlvar[Index] do
  begin
    ASQLCode := InternalGetSqlType(SqlType);
    if InternalGetIsNumeric(SqlScale) then
    begin
      case ASQLCode of
        SQL_SHORT:
          PSmallInt(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_INT64, SQL_QUAD:
          PInt64(SqlData)^ := Trunc(Value * ScaleDivisor[SqlScale]);
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
      else
        raise EUIBConvertError.Create(EUIB_UNEXPECTEDCASTERROR);
      end;
    end
    else
      case ASQLCode of
        SQL_DOUBLE:
          PDouble(SqlData)^ := Value;
        SQL_TIMESTAMP:
          EncodeTimeStamp(Value, PISCTimeStamp(SqlData));
        SQL_TYPE_DATE:
          EncodeSQLDate(Value, PInteger(SqlData)^);
        SQL_TYPE_TIME:
          PCardinal(SqlData)^ := Round(Frac(Value) * 864000000);
        SQL_LONG:
          PInteger(SqlData)^ := Trunc(Value);
        SQL_D_FLOAT, SQL_FLOAT:
          PSingle(SqlData)^ := Value;
        {$IFDEF IB7_UP} SQL_BOOLEAN, {$ENDIF} SQL_SHORT:
          PSmallint(SqlData)^ := Trunc(Value);
        SQL_INT64:
          PInt64(SqlData)^ := Trunc(Value);
        SQL_TEXT:
          EncodeString(SQL_TEXT, Index, FloatToStr(Value));
        SQL_VARYING:
          EncodeString(SQL_VARYING, Index, FloatToStr(Value));
      else
        raise EUIBConvertError.Create(EUIB_CASTERROR);
      end;
    IsNull[Index] := False;
  end;
end;

procedure TSQLParams.SetIsNull(const Index: Word; const Value: Boolean);
begin
  // (rom) changed implementation completely for better OO design
  if IsNullable[Index] then
    FXSQLDa.sqlvar[Index].SqlInd^ := BoolVals[Value];
end;

function TSQLParams.FindParam(const Name: string; out Index: Word): Boolean;
var
  Field: Smallint;
begin
  for Field := 0 to FXSQLDa.sqln - 1 do
    if FXSQLDa.sqlvar[Field].ParamNameLength = Length(Name) then
      if StrLIComp(@FXSQLDa.sqlvar[Field].ParamName, PChar(Name),
        FXSQLDa.sqlvar[Field].ParamNameLength) = 0 then
      begin
        Result := True;
        Index := Field;
        Exit;
      end;
  Result := False;
end;

function TSQLParams.AddField(const Name: string): Word;
var
  Num: Word;
  Len: Cardinal;
begin
  Len := Length(Name);
  if Len > MaxParamLength then
    raise Exception.CreateFmt(EUIB_SIZENAME, [Name]);

  Result := FXSQLDa.sqln;
  if (Len > 0) and FindParam(Name, Num) then
  begin
    Inc(FXSQLDa.sqln);
    Inc(FXSQLDa.sqld);
    ReallocMem(FXSQLDa, XSQLDA_LENGTH(FXSQLDa.sqln));
    Move(FXSQLDa.sqlvar[Num], FXSQLDa.sqlvar[Result], SizeOf(TUIBSQLVar));
  end
  else
  begin
    Inc(FXSQLDa.sqln);
    Inc(FXSQLDa.sqld);
    ReallocMem(FXSQLDa, XSQLDA_LENGTH(FXSQLDa.sqln));
    Inc(FParamCount);
    with FXSQLDa.sqlvar[Result] do
    begin
      Init := True;
      ID := FParamCount;
      ParamNameLength := Len;
      if ParamNameLength > 0 then
        Move(Pointer(Name)^, ParamName[0], ParamNameLength);
      SqlType := SQL_TEXT + 1; // tip: don't allocate memory if not defined
      SqlScale := 0;
      SqlSubType := 0;
      SqlLen := 0;
      SqlData := nil;
      GetMem(SqlInd, SizeOf(SqlInd)); // Can be NULL
      SqlInd^ := BoolVals[True]; // NULL
    end;
  end;
end;

end.

