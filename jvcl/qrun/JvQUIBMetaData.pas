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
{ Contributor:   Ritsaert Hornstra                                             }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}
{ Class needed to read MetaData. }

{$I jvcl.inc}
{$I JvUIB.inc}

unit JvQUIBMetaData;

interface

uses
  Classes, SysUtils,
  JvQUIBase, JvQUIBLib, JvQUIB, JvQUIBConst;

type
  // (rom) the names of the elements need prefixes
  TTriggerPrefix = (Before, After);
  TTriggerSuffix = (Insert, Update, Delete);
  TTriggerSuffixes = set of TTriggerSuffix;
  TIndexOrder = (IoDescending, IoAscending);
  TUpdateRule = (Restrict, Cascade, SetNull, SetDefault);
  TTableFieldInfo = (fPrimary, fForeign, fIndice, fUnique);
  TTableFieldInfos = set of TTableFieldInfo;

  // indentation = inherit
  TMetaNodeType =
   (
    MetaNode,
      MetaDatabase,
      MetaException,
      MetaGenerator,
      MetaCheck,
      MetaTrigger,
      MetaUDF,
      MetaView,
      MetaProcedure,
      MetaRole,
      MetaTable,
      MetaBaseField,
        MetaUDFField,
        MetaField,
          MetaProcInField,
          MetaProcOutField,
          MetaTableField,
            MetaDomain,
      MetaConstraint,
        MetaForeign,
        MetaIndex,
        MetaPrimary,
        MetaUnique
   );

  // forward declarations
  TMetaNode = class;
  TMetaDomain = class;
  TMetaTable = class;

  TMetaNodeClass = class of TMetaNode;

  TNodeItem = record
    Childs: TList;
    ClassID: TMetaNodeClass;
  end;

  TMetaNode = class(TObject)
  private
    FName: string;
    FOwner: TMetaNode;
    FNodeItems: array of TNodeItem;
    FNodeItemsCount: Integer;
    function GetItems(const ClassIndex, Index: Integer): TMetaNode;
    function GetAsDDL: string;
    procedure AddClass(ClassID: TMetaNodeClass);
    procedure CheckTransaction(Transaction: TJvUIBTransaction);
    procedure SaveNode(Stream: TStringStream; OID: Integer; Separator: string = BreakLine);
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    function GetAsDDLNode: string;
  public
    procedure SaveToDDLNode(Stream: TStringStream); virtual;
    function GetNodes(const Index: Integer): TNodeItem;
    class function NodeClass: string; virtual;
    class function NodeType: TMetaNodeType; virtual;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); virtual;
    constructor CreateFromStream(AOwner: TMetaNode; ClassIndex: Integer; Stream: TStream); virtual;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SaveToDDL(Stream: TStringStream); virtual;
    property Name: string read FName;
    property AsDDL: string read GetAsDDL;
    property AsDDLNode: string read GetAsDDLNode;
    property NodeCount: Integer read FNodeItemsCount;
    property Nodes[const Index: Integer]: TNodeItem read GetNodes;
    property Parent: TMetaNode read FOwner;
  end;

  TMetaGenerator = class(TMetaNode)
  private
    FValue: Integer;
    procedure LoadFromDataBase(Transaction: TJvUIBTransaction; const Name: string);
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Value: Integer read FValue;
  end;

  TMetaBaseField = class(TMetaNode)
  private
    FScale: Word;
    FLength: Smallint;
    FPrecision: Smallint;
    FFieldType: TUIBFieldType;
    FCharSet: string;
    FSegmentLength: Smallint;
    FSubType: Smallint;
    FBytesPerCharacter: Smallint;
    procedure LoadFromQuery(QField, QCharset: TJvUIBStatement); virtual;
    procedure LoadFromStream(Stream: TStream); override;
    property SegmentLength: Smallint read FSegmentLength;
    function GetShortFieldType: string;
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Scale: Word read FScale;
    property Length: Smallint read FLength;
    property Precision: Smallint read FPrecision;
    property FieldType: TUIBFieldType read FFieldType;
    property CharSet: string read FCharSet;
    property SubType: Smallint read FSubType;
    property BytesPerCharacter: Smallint read FBytesPerCharacter;
    property ShortFieldType: string read GetShortFieldType;
  end;

  TMetaField = class(TMetaBaseField)
  private
    procedure LoadFromQuery(Q, C: TJvUIBStatement); override;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDL(Stream: TStringStream); override;
    property SegmentLength;
  end;

  TMetaProcInField = class(TMetaField)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaProcOutField = class(TMetaField)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaTableField = class(TMetaField)
  private
    FDefaultValue: string;
    FNotNull: Boolean;
    FDomain: Integer;
    FInfos: TTableFieldInfos;
    FComputedSource: string;
    procedure LoadFromQuery(Q, C: TJvUIBStatement); override;
    procedure LoadFromStream(Stream: TStream); override;
    function GetDomain: TMetaDomain;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property DefaultValue: string read FDefaultValue;
    property NotNull: Boolean read FNotNull;
    property Domain: TMetaDomain read GetDomain;
    property FieldInfos: TTableFieldInfos read FInfos;
    property ComputedSource: string read FComputedSource;
  end;

  TMetaDomain = class(TMetaTableField)
  protected
    property Domain; // hidden
    property ComputedSource; // hidden
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToDDL(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
  end;

  TMetaConstraint = class(TMetaNode)
  private
    FFields: array of Integer;
    function GetFields(const Index: Word): TMetaTableField;
    function GetFieldsCount: Word;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Fields[const Index: Word]: TMetaTableField read GetFields;
    property FieldsCount: Word read GetFieldsCount;
  end;

  TMetaPrimary = class(TMetaConstraint)
  private
    procedure LoadFromQuery(Q: TJvUIBStatement);
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
  end;

  TMetaUnique = class(TMetaConstraint)
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDL(Stream: TStringStream); override;
  end;

  TMetaForeign = class(TMetaConstraint)
  private
    FForTable: Integer;
    FForFields: array of Integer;
    FOnDelete: TUpdateRule;
    FOnUpdate: TUpdateRule;
    function GetForFields(const Index: Word): TMetaTableField;
    function GetForFieldsCount: Word;
    function GetForTable: TMetaTable;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    property ForTable: TMetaTable read GetForTable;
    property ForFields[const Index: Word]: TMetaTableField read GetForFields;
    property ForFieldsCount: Word read GetForFieldsCount;
    property OnDelete: TUpdateRule read FOnDelete;
    property OnUpdate: TUpdateRule read FOnUpdate;
  end;

  TMetaCheck = class(TMetaNode)
  private
    FConstraint: string;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Constraint: string read FConstraint;
  end;

  TMetaIndex = class(TMetaConstraint)
  private
    FUnique: Boolean;
    FActive: Boolean;
    FOrder: TIndexOrder;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Unique: Boolean read FUnique;
    property Active: Boolean read FActive;
    property Order: TIndexOrder read FOrder;
  end;

  TMetaTrigger = class(TMetaNode)
  private
    FPrefix: TTriggerPrefix;
    FSuffix: TTriggerSuffixes;
    FPosition: Smallint;
    FActive: Boolean;
    FSource: string;
    class function DecodePrefix(Value: Integer): TTriggerPrefix;
    class function DecodeSuffixes(Value: Integer): TTriggerSuffixes;
    procedure LoadFromQuery(Q: TJvUIBStatement);
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    property Prefix: TTriggerPrefix read FPrefix;
    property Suffix: TTriggerSuffixes read FSuffix;
    property Position: Smallint read FPosition;
    property Active: Boolean read FActive;
    property Source: string read FSource;
  end;

  TMetaTable = class(TMetaNode)
  private
    function GetFields(const Index: Integer): TMetaTableField;
    function GetFieldsCount: Integer;
    procedure LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
      QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement; OIDs: TOIDTables);
    function FindFieldIndex(const Name: string): Integer;
    function GetUniques(const Index: Integer): TMetaUnique;
    function GetUniquesCount: Integer;
    function GetPrimary(const Index: Integer): TMetaPrimary;
    function GetPrimaryCount: Integer;
    function GetIndices(const Index: Integer): TMetaIndex;
    function GetIndicesCount: Integer;
    function GetForeign(const Index: Integer): TMetaForeign;
    function GetForeignCount: Integer;
    function GetChecks(const Index: Integer): TMetaCheck;
    function GetChecksCount: Integer;
    function GetTriggers(const Index: Integer): TMetaTrigger;
    function GetTriggersCount: Integer;
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    function FindFieldName(const Name: string): TMetaTableField;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDL(Stream: TStringStream); override;

    property Fields[const Index: Integer]: TMetaTableField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;

    property Primary[const Index: Integer]: TMetaPrimary read GetPrimary;
    property PrimaryCount: Integer read GetPrimaryCount; // 0 or 1

    property Uniques[const Index: Integer]: TMetaUnique read GetUniques;
    property UniquesCount: Integer read GetUniquesCount;

    property Indices[const Index: Integer]: TMetaIndex read GetIndices;
    property IndicesCount: Integer read GetIndicesCount;

    property Foreign[const Index: Integer]: TMetaForeign read GetForeign;
    property ForeignCount: Integer read GetForeignCount;

    property Checks[const Index: Integer]: TMetaCheck read GetChecks;
    property ChecksCount: Integer read GetChecksCount;

    property Triggers[const Index: Integer]: TMetaTrigger read GetTriggers;
    property TriggersCount: Integer read GetTriggersCount;
  end;

  TMetaView = class(TMetaNode)
  private
    FSource: string;
    function GetFields(const Index: Integer): TMetaField;
    function GetFieldsCount: Integer;
    function GetTriggers(const Index: Integer): TMetaTrigger;
    function GetTriggersCount: Integer;
    procedure LoadFromDataBase(QName, QFields, QTriggers,
      QCharset: TJvUIBStatement; OIDs: TOIDViews);
    procedure LoadFromStream(Stream: TStream); override;
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDL(Stream: TStringStream); override;
    property Source: string read FSource;
    property Fields[const Index: Integer]: TMetaField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;
    property Triggers[const Index: Integer]: TMetaTrigger read GetTriggers;
    property TriggersCount: Integer read GetTriggersCount;
  end;

  TMetaProcedure = class(TMetaNode)
  private
    FSource: string;
    procedure LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement; OIDs: TOIDProcedures);
    function GetInputFields(const Index: Integer): TMetaProcInField;
    function GetInputFieldsCount: Integer;
    function GetOutputFields(const Index: Integer): TMetaProcOutField;
    function GetOutputFieldsCount: Integer;
    procedure LoadFromStream(Stream: TStream); override;
    procedure InternalSaveToDDL(Stream: TStringStream; Operation: string);
    procedure SaveToPostDDL(Stream: TStringStream);
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToAlterDDL(Stream: TStringStream);

    property Source: string read FSource;

    property InputFields[const Index: Integer]: TMetaProcInField read GetInputFields;
    property InputFieldsCount: Integer read GetInputFieldsCount;

    property OutputFields[const Index: Integer]: TMetaProcOutField read GetOutputFields;
    property OutputFieldsCount: Integer read GetOutputFieldsCount;
  end;

  TMetaException = class(TMetaNode)
  private
    FMessage: string;
    FNumber: Integer;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QName: TJvUIBStatement);
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Message: string read FMessage;
    property Number: Integer read FNumber;
  end;

  TMetaUDFField = class(TMetaBaseField)
  private
    FPosition: Smallint;
    FMechanism: Smallint;
    procedure LoadFromQuery(QField, QCharset: TJvUIBStatement); override;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Position: Smallint read FPosition;
    property Mechanism: Smallint read FMechanism;
  end;

  TMetaUDF = class(TMetaNode)
  private
    FModule: string;
    FEntry: string;
    FReturn: Smallint;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement; OIDs: TOIDUDFs);
    function GetFields(const Index: Integer): TMetaUDFField;
    function GetFieldsCount: Integer;
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure SaveToStream(Stream: TStream); override;
    property Module: string read FModule;
    property Entry: string read FEntry;
    property Return: Smallint read FReturn;
    property Fields[const Index: Integer]: TMetaUDFField read GetFields;
    property FieldsCount: Integer read GetFieldsCount;
  end;

  TMetaRole = class(TMetaNode)
  private
    FOwner: string;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromQuery(QName: TJvUIBStatement);
  public
    procedure SaveToDDLNode(Stream: TStringStream); override;
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    property Owner: string read FOwner;
  end;

  TMetaDataBase = class(TMetaNode)
  private
    FOIDDatabases: TOIDDatabases;
    FOIDTables: TOIDTables;
    FOIDViews: TOIDViews;
    FOIDProcedures: TOIDProcedures;
    FOIDUDFs: TOIDUDFs;
    FSysInfos: Boolean;
    function GetGenerators(const Index: Integer): TMetaGenerator;
    function GetGeneratorsCount: Integer;

    function GetTables(const Index: Integer): TMetaTable;
    function GetTablesCount: Integer;
    function FindTableIndex(const TableName: string): Integer;
    function FindDomainIndex(const DomainName: string): Integer;
    function GetViews(const Index: Integer): TMetaView;
    function GetViewsCount: Integer;
    function GetDomains(const Index: Integer): TMetaDomain;
    function GetDomainsCount: Integer;

    procedure LoadFromStream(Stream: TStream); override;
    function GetProcedures(const Index: Integer): TMetaProcedure;
    function GetProceduresCount: Integer;
    function GetExceptions(const Index: Integer): TMetaException;
    function GetExceptionsCount: Integer;
    function GetUDFS(const Index: Integer): TMetaUDF;
    function GetUDFSCount: Integer;
    function GetRoles(const Index: Integer): TMetaRole;
    function GetRolesCount: Integer;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    function FindTableName(const TableName: string): TMetaTable;
    function FindProcName(const ProcName: string): TMetaProcedure;
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure LoadFromDatabase(Transaction: TJvUIBTransaction);
    procedure SaveToDDL(Stream: TStringStream); override;

    property OIDDatabases: TOIDDatabases read FOIDDatabases write FOIDDatabases;

    property Generators[const Index: Integer]: TMetaGenerator read GetGenerators;
    property GeneratorsCount: Integer read GetGeneratorsCount;

    property Tables[const Index: Integer]: TMetaTable read GetTables;
    property TablesCount: Integer read GetTablesCount;
    property OIDTables: TOIDTables read FOIDTables write FOIDTables;

    property Views[const Index: Integer]: TMetaView read GetViews;
    property ViewsCount: Integer read GetViewsCount;
    property OIDViews: TOIDViews read FOIDViews write FOIDViews;

    property Domains[const Index: Integer]: TMetaDomain read GetDomains;
    property DomainsCount: Integer read GetDomainsCount;

    property Procedures[const Index: Integer]: TMetaProcedure read GetProcedures;
    property ProceduresCount: Integer read GetProceduresCount;
    property OIDProcedures: TOIDProcedures read FOIDProcedures write FOIDProcedures;

    property Exceptions[const Index: Integer]: TMetaException read GetExceptions;
    property ExceptionsCount: Integer read GetExceptionsCount;

    property UDFS[const Index: Integer]: TMetaUDF read GetUDFS;
    property UDFSCount: Integer read GetUDFSCount;
    property OIDUDFs: TOIDUDFs read FOIDUDFs write FOIDUDFs;

    property Roles[const Index: Integer]: TMetaRole read GetRoles;
    property RolesCount: Integer read GetRolesCount;

    property SysInfos: Boolean read FSysInfos write FSysInfos;
  end;

implementation

//   Database Tree
//------------------------
//  OIDDomains   = 0;
//  OIDTable     = 1;
//    OIDTableFields   = 0;
//    OIDPrimary       = 1;
//    OIDForeign       = 2;
//    OIDTableTrigger  = 3;
//    OIDUnique        = 4;
//    OIDIndex         = 5;
//    OIDCheck         = 6;
//  OIDView      = 2;
//    OIDViewFields    = 0;
//    OIDViewTrigers   = 1;
//  OIDProcedure = 3;
//    OIDProcFieldIn   = 0;
//    OIDProcFieldOut  = 1;
//  OIDGenerator = 4;
//  OIDException = 5;
//  OIDUDF       = 6;
//    OIDUDFField      = 0;
//  OIDRole      = 7;

const
  TriggerPrefixTypes: array [TTriggerPrefix] of PChar =
    ('BEFORE', 'AFTER');

  TriggerSuffixTypes: array [TTriggerSuffix] of PChar =
    ('INSERT', 'UPDATE', 'DELETE');

  FieldTypes: array [TUIBFieldType] of PChar =
   ('', 'NUMERIC', 'CHAR', 'VARCHAR', 'CSTRING', 'SMALLINT', 'INTEGER', 'QUAD',
    'FLOAT', 'DOUBLE PRECISION', 'TIMESTAMP', 'BLOB', 'BLOBID', 'DATE', 'TIME',
    'INT64' {$IFDEF IB7_UP}, 'BOOLEAN' {$ENDIF});

  QRYGenerators =
    'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS GEN WHERE ' +
    '(NOT GEN.RDB$GENERATOR_NAME STARTING WITH ''RDB$'') AND ' +
    '(NOT GEN.RDB$GENERATOR_NAME STARTING WITH ''SQL$'') AND ' +
    '((GEN.RDB$SYSTEM_FLAG IS NULL) OR (GEN.RDB$SYSTEM_FLAG <> 1)) ' +
    'ORDER BY GEN.RDB$GENERATOR_NAME';

  QRYTables =
    'SELECT REL.RDB$RELATION_NAME FROM RDB$RELATIONS REL WHERE ' +
    '(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '(NOT REL.RDB$FLAGS IS NULL) AND ' +
    '(REL.RDB$VIEW_BLR IS NULL) AND ' +
    '(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'') ' +
    'ORDER BY REL.RDB$RELATION_NAME';

  QRYSysTables =
    'SELECT REL.RDB$RELATION_NAME FROM RDB$RELATIONS REL ' +
    'WHERE REL.RDB$VIEW_BLR IS NULL ORDER BY REL.RDB$RELATION_NAME';

  QRYTableFields =
    'SELECT FLD.RDB$FIELD_TYPE, FLD.RDB$FIELD_SCALE, ' +
    'FLD.RDB$FIELD_LENGTH, FLD.RDB$FIELD_PRECISION, ' +
    'FLD.RDB$CHARACTER_SET_ID, FLD.RDB$FIELD_SUB_TYPE, RFR.RDB$FIELD_NAME, ' +
    'FLD.RDB$SEGMENT_LENGTH, RFR.RDB$NULL_FLAG, RFR.RDB$DEFAULT_SOURCE, ' +
    'RFR.RDB$FIELD_SOURCE , FLD.RDB$COMPUTED_SOURCE ' +
    'FROM RDB$RELATIONS REL, RDB$RELATION_FIELDS RFR, RDB$FIELDS FLD ' +
    'WHERE (RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME) AND ' +
    '(RFR.RDB$RELATION_NAME = REL.RDB$RELATION_NAME) AND ' +
    '(REL.RDB$RELATION_NAME = ?) ' +
    'ORDER BY RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';

  QRYCharset =
    'SELECT RDB$CHARACTER_SET_ID, RDB$CHARACTER_SET_NAME, RDB$BYTES_PER_CHARACTER FROM RDB$CHARACTER_SETS';

  QRYUnique =
    'SELECT RC.RDB$CONSTRAINT_NAME, IDX.RDB$FIELD_NAME ' +
    'FROM RDB$RELATION_CONSTRAINTS RC, RDB$INDEX_SEGMENTS IDX ' +
    'WHERE (IDX.RDB$INDEX_NAME = RC.RDB$INDEX_NAME) AND ' +
    '(RC.RDB$CONSTRAINT_TYPE = ?) ' +
    'AND (RC.RDB$RELATION_NAME = ?) ' +
    'ORDER BY RC.RDB$RELATION_NAME, IDX.RDB$FIELD_POSITION';

  QRYIndex =
    'SELECT IDX.RDB$INDEX_NAME, ISG.RDB$FIELD_NAME, IDX.RDB$UNIQUE_FLAG, ' +
    'IDX.RDB$INDEX_INACTIVE, IDX.RDB$INDEX_TYPE FROM RDB$INDICES IDX ' +
    'LEFT JOIN RDB$INDEX_SEGMENTS ISG ON ISG.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME ' +
    'LEFT JOIN RDB$RELATION_CONSTRAINTS C ON IDX.RDB$INDEX_NAME = C.RDB$INDEX_NAME ' +
    'WHERE (C.RDB$CONSTRAINT_NAME IS NULL) AND (IDX.RDB$RELATION_NAME = ?) ' +
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, ISG.RDB$FIELD_POSITION';

  QRYForeign =
    'SELECT A.RDB$CONSTRAINT_NAME, B.RDB$UPDATE_RULE, B.RDB$DELETE_RULE, ' +
    'C.RDB$RELATION_NAME AS FK_TABLE, D.RDB$FIELD_NAME AS FK_FIELD, ' +
    'E.RDB$FIELD_NAME AS ONFIELD ' +
    'FROM RDB$REF_CONSTRAINTS B, RDB$RELATION_CONSTRAINTS A, RDB$RELATION_CONSTRAINTS C, ' +
    'RDB$INDEX_SEGMENTS D, RDB$INDEX_SEGMENTS E ' +
    'WHERE (A.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') AND ' +
    '(A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) AND ' +
    '(B.RDB$CONST_NAME_UQ=C.RDB$CONSTRAINT_NAME) AND (C.RDB$INDEX_NAME=D.RDB$INDEX_NAME) AND ' +
    '(A.RDB$INDEX_NAME=E.RDB$INDEX_NAME) AND ' +
    '(D.RDB$FIELD_POSITION = E.RDB$FIELD_POSITION) ' +
    'AND (A.RDB$RELATION_NAME = ?) ' +
    'ORDER BY A.RDB$CONSTRAINT_NAME, A.RDB$RELATION_NAME, D.RDB$FIELD_POSITION, E.RDB$FIELD_POSITION';

  QRYCheck =
    'SELECT A.RDB$CONSTRAINT_NAME, C.RDB$TRIGGER_SOURCE ' +
    'FROM RDB$RELATION_CONSTRAINTS A, RDB$CHECK_CONSTRAINTS B, RDB$TRIGGERS C ' +
    'WHERE (A.RDB$CONSTRAINT_TYPE = ''CHECK'') AND ' +
    '(A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) AND ' +
    '(B.RDB$TRIGGER_NAME = C.RDB$TRIGGER_NAME) AND ' +
    '(C.RDB$TRIGGER_TYPE = 1) ' +
    'AND (A.RDB$RELATION_NAME = ?)';

  QRYTrigger =
    'SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_SOURCE, T.RDB$TRIGGER_SEQUENCE, ' +
    'T.RDB$TRIGGER_TYPE, T.RDB$TRIGGER_INACTIVE, T.RDB$SYSTEM_FLAG ' +
    'from RDB$TRIGGERS T left join RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = ' +
    'T.RDB$TRIGGER_NAME where ((T.RDB$SYSTEM_FLAG = 0) or (T.RDB$SYSTEM_FLAG is null)) ' +
    'and (c.rdb$trigger_name is null) and (T.RDB$RELATION_NAME = ?) ' +
    'order by T.RDB$TRIGGER_NAME';

  QRYSysTrigger =
    'SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_SOURCE, T.RDB$TRIGGER_SEQUENCE, ' +
    'T.RDB$TRIGGER_TYPE, T.RDB$TRIGGER_INACTIVE, T.RDB$SYSTEM_FLAG ' +
    'FROM RDB$TRIGGERS T LEFT JOIN RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = ' +
    'T.RDB$TRIGGER_NAME WHERE (T.RDB$RELATION_NAME = ?) ORDER BY T.RDB$TRIGGER_NAME';

  QRYView =
    'SELECT REL.RDB$RELATION_NAME, REL.RDB$VIEW_SOURCE FROM RDB$RELATIONS REL WHERE ' +
    '(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND ' +
    '(NOT REL.RDB$FLAGS IS NULL) AND ' +
    '(NOT REL.RDB$VIEW_BLR IS NULL) AND ' +
    '(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'') ' +
    'ORDER BY REL.RDB$RELATION_NAME';

  QRYDomains =
    'select RDB$FIELD_TYPE, RDB$FIELD_SCALE, RDB$FIELD_LENGTH, ' +
    'RDB$FIELD_PRECISION, RDB$CHARACTER_SET_ID, RDB$FIELD_SUB_TYPE, ' +
    'RDB$FIELD_NAME, RDB$SEGMENT_LENGTH, RDB$NULL_FLAG, RDB$DEFAULT_SOURCE, RDB$COMPUTED_SOURCE ' +
    'FROM RDB$FIELDS WHERE NOT (RDB$FIELD_NAME STARTING WITH ''RDB$'')';

  QRYSysDomains =
    'select RDB$FIELD_TYPE, RDB$FIELD_SCALE, RDB$FIELD_LENGTH, ' +
    'RDB$FIELD_PRECISION, RDB$CHARACTER_SET_ID, RDB$FIELD_SUB_TYPE, ' +
    'RDB$FIELD_NAME, RDB$SEGMENT_LENGTH, RDB$NULL_FLAG, RDB$DEFAULT_SOURCE, RDB$COMPUTED_SOURCE ' +
    'from RDB$FIELDS';

  QRYProcedures =
    'SELECT RDB$PROCEDURE_NAME, RDB$PROCEDURE_SOURCE FROM  RDB$PROCEDURES ORDER BY RDB$PROCEDURE_NAME';

  QRYProcFields =
    'SELECT FS.RDB$FIELD_TYPE, FS.RDB$FIELD_SCALE, FS.RDB$FIELD_LENGTH, FS.RDB$FIELD_PRECISION, ' +
    'FS.RDB$CHARACTER_SET_ID, FS.RDB$FIELD_SUB_TYPE, PP.RDB$PARAMETER_NAME, FS.RDB$SEGMENT_LENGTH ' +
    'FROM RDB$PROCEDURES PR LEFT JOIN RDB$PROCEDURE_PARAMETERS PP ' +
    'ON PP.RDB$PROCEDURE_NAME = PR.RDB$PROCEDURE_NAME LEFT JOIN RDB$FIELDS FS ON ' +
    'FS.RDB$FIELD_NAME = PP.RDB$FIELD_SOURCE LEFT JOIN RDB$CHARACTER_SETS CR ON ' +
    'FS.RDB$CHARACTER_SET_ID = CR.RDB$CHARACTER_SET_ID LEFT JOIN RDB$COLLATIONS CO ' +
    'ON ((FS.RDB$COLLATION_ID = CO.RDB$COLLATION_ID) AND (FS.RDB$CHARACTER_SET_ID = ' +
    'CO.RDB$CHARACTER_SET_ID)) WHERE (PR.RDB$PROCEDURE_NAME = ?) AND ' +
    '(PP.RDB$PARAMETER_TYPE = ?) ORDER BY PP.RDB$PARAMETER_TYPE, PP.RDB$PARAMETER_NUMBER';

  QRYExceptions =
    'SELECT RDB$EXCEPTION_NAME, RDB$MESSAGE, RDB$EXCEPTION_NUMBER FROM RDB$EXCEPTIONS ORDER BY RDB$EXCEPTION_NAME';

  QRYUDF =
    'SELECT RDB$FUNCTION_NAME, RDB$MODULE_NAME, RDB$ENTRYPOINT, RDB$RETURN_ARGUMENT ' +
    'FROM RDB$FUNCTIONS WHERE (RDB$SYSTEM_FLAG IS NULL) ORDER BY RDB$FUNCTION_NAME';

  QRYUDFFields =
    'SELECT RDB$FIELD_TYPE, RDB$FIELD_SCALE, RDB$FIELD_LENGTH, RDB$FIELD_PRECISION, ' +
    'RDB$CHARACTER_SET_ID, RDB$FIELD_SUB_TYPE, RDB$ARGUMENT_POSITION, RDB$MECHANISM ' +
    'FROM RDB$FUNCTION_ARGUMENTS WHERE RDB$FUNCTION_NAME = ? ' +
    'ORDER BY RDB$ARGUMENT_POSITION';

  QRYRoles =
    'SELECT RDB$ROLE_NAME, RDB$OWNER_NAME FROM RDB$ROLES';

procedure WriteString(Stream: TStream; var Str: string);
var
  Len: Integer;
begin
  Len := Length(Str);
  Stream.Write(Len, SizeOf(Len));
  if Len > 0 then
    Stream.Write(PChar(Str)^, Len);
end;

procedure ReadString(Stream: TStream; var Str: string);
var
  Len: Integer;
begin
  Stream.Read(Len, SizeOf(Len));
  SetLength(Str, Len);
  if Len > 0 then
    Stream.Read(PChar(Str)^, Len);
end;

//=== { TMetaNode } ==========================================================

constructor TMetaNode.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  // (rom) added inherited Create
  inherited Create;
  FNodeItemsCount := 0;
  FOwner := AOwner;
  if (FOwner <> nil) and (ClassIndex >= 0) then
    FOwner.FNodeItems[ClassIndex].Childs.Add(Self)
end;

constructor TMetaNode.CreateFromStream(AOwner: TMetaNode; ClassIndex: Integer; Stream: TStream);
var
  I, J: Integer;
begin
  Create(AOwner, ClassIndex);
  LoadFromStream(Stream);
  for J := 0 to FNodeItemsCount - 1 do
  begin
    Stream.Read(I, SizeOf(I));
    for I := 0 to I - 1 do
      FNodeItems[J].ClassID.CreateFromStream(Self, J, Stream);
  end;
end;

destructor TMetaNode.Destroy;
var
  I, J: Integer;
begin
  for I := 0 to FNodeItemsCount - 1 do
  begin
    for J := 0 to FNodeItems[I].Childs.Count - 1 do
      TObJect(FNodeItems[I].Childs[J]).Free;
    FNodeItems[I].Childs.Free;
  end;
  inherited Destroy;
end;

function TMetaNode.GetAsDDL: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDL(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

function TMetaNode.GetItems(const ClassIndex, Index: Integer): TMetaNode;
var
  FChilds: TList;
begin
  FChilds := FNodeItems[ClassIndex].Childs;
  if (FChilds.Count > 0) and (Index >= 0) and
    (Index < FChilds.Count) then
    Result := TMetaNode(FChilds.Items[Index])
  else
    raise EUIBError.CreateFmt(EUIB_INDEXERROR, [Index]);
end;

procedure TMetaNode.SaveToStream(Stream: TStream);
var
  I, J: Integer;
begin
  for J := 0 to FNodeItemsCount - 1 do
  begin
    I := FNodeItems[J].Childs.Count;
    Stream.Write(I, SizeOf(I));
    for I := 0 to I - 1 do
      TMetaNode(FNodeItems[J].Childs.Items[I]).SaveToStream(Stream);
  end;
end;

procedure TMetaNode.AddClass(ClassID: TMetaNodeClass);
begin
  SetLength(FNodeItems, FNodeItemsCount + 1);
  FNodeItems[FNodeItemsCount].Childs := TList.Create;
  FNodeItems[FNodeItemsCount].ClassID := ClassID;
  Inc(FNodeItemsCount);
end;

procedure TMetaNode.CheckTransaction(Transaction: TJvUIBTransaction);
begin
  Assert(Transaction <> nil);
  Assert(Transaction.DataBase <> nil);
end;

procedure TMetaNode.SaveNode(Stream: TStringStream; OID: Integer;
  Separator: string);
var
  I: Integer;
begin
  for I := 0 to FNodeItems[OID].Childs.Count - 1 do
  begin
    if I = 0 then
      Stream.WriteString(NewLine)
    else
      Stream.WriteString(Separator);
    TMetaNode(FNodeItems[OID].Childs[I]).SaveToDDL(Stream);
  end;
end;

procedure TMetaNode.SaveToDDLNode(Stream: TStringStream);
begin
end;

function TMetaNode.GetNodes(const Index: Integer): TNodeItem;
begin
  Assert((Index >= 0) and (FNodeItemsCount > 0) and (Index < FNodeItemsCount));
  Result := FNodeItems[Index];
end;

class function TMetaNode.NodeClass: string;
begin
  Result := 'Node'
end;

procedure TMetaNode.SaveToDDL(Stream: TStringStream);
begin
  SaveToDDLNode(Stream);
end;

function TMetaNode.GetAsDDLNode: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToDDLNode(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

class function TMetaNode.NodeType: TMetaNodeType;
begin
  Result := MetaNode;
end;

//=== { TMetaGenerator } =====================================================

procedure TMetaGenerator.LoadFromDataBase(Transaction: TJvUIBTransaction;
  const Name: string);
var
  Query: TJvUIBStatement;
begin
  CheckTransaction(Transaction);
  Query := TJvUIBStatement.Create(nil);
  Query.Transaction := Transaction;
  Query.CachedFetch := False;
  try
    FName := Name;
    Query.SQL.Text := Format('select gen_id(%s, 0) from rdb$database', [FName]);
    Query.Open;
    if not Query.Eof then
      FValue := Query.Fields.AsInteger[0]
    else
      raise EUIBError.CreateFmt(EUIB_NOGENERATOR, [FName]);
  finally
    Query.Free;
  end;
end;

procedure TMetaGenerator.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FValue, SizeOf(FValue));
end;

procedure TMetaGenerator.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FValue, SizeOf(FValue));
end;

procedure TMetaGenerator.SaveToDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format(
    'CREATE GENERATOR %s;%sSET GENERATOR %0:s TO %2:d;',
    [FName, BreakLine, FValue]));
end;

class function TMetaGenerator.NodeClass: string;
begin
  Result := 'Generator';
end;

class function TMetaGenerator.NodeType: TMetaNodeType;
begin
  Result := MetaGenerator;
end;

//=== { TMetaTable } =========================================================

constructor TMetaTable.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaTableField);
  AddClass(TMetaPrimary);
  AddClass(TMetaForeign);
  AddClass(TMetaTrigger);
  AddClass(TMetaUnique);
  AddClass(TMetaIndex);
  AddClass(TMetaCheck);
end;

function TMetaTable.FindFieldName(const Name: string): TMetaTableField;
var
  I: Integer;
begin
  for I := 0 to FieldsCount - 1 do
    if Fields[I].FName = Name then
    begin
      Result := Fields[I];
      Exit;
    end;
  raise EUIBError.CreateFmt(EUIB_NOFIELD, [Name]);
end;

function TMetaTable.GetFields(const Index: Integer): TMetaTableField;
begin
  Result := TMetaTableField(GetItems(Ord(OIDTableField), Index))
end;

function TMetaTable.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTableField)].Childs.Count;
end;

function TMetaTable.GetPrimary(const Index: Integer): TMetaPrimary;
begin
  Result := TMetaPrimary(GetItems(Ord(OIDPrimary), Index))
end;

function TMetaTable.GetPrimaryCount: Integer;
begin
  Result := FNodeItems[Ord(OIDPrimary)].Childs.Count;
end;

function TMetaTable.GetUniques(const Index: Integer): TMetaUnique;
begin
  Result := TMetaUnique(GetItems(Ord(OIDUnique), Index))
end;

function TMetaTable.GetUniquesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUnique)].Childs.Count;
end;

procedure TMetaTable.LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
  QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement; OIDs: TOIDTables);
var
  Unk: string;
begin
  // Fields
  FName := Trim(QNames.Fields.AsString[0]);

  if OIDTableField in OIDs then
  begin
    QFields.Params.AsString[0] := FName;
    QFields.Open;
    while not QFields.Eof do
    begin
      with TMetaTableField.Create(Self, Ord(OIDTableField)) do
        LoadFromQuery(QFields, QCharset);
      QFields.Next;
    end;

    // PRIMARY
    if OIDPrimary in OIDs then
    begin
      QPrimary.Params.AsString[1] := FName;
      QPrimary.Params.AsString[0] := 'PRIMARY KEY';
      QPrimary.Open;
      if not QPrimary.Eof then
        TMetaPrimary.Create(Self, Ord(OIDPrimary)).LoadFromQuery(QPrimary);
    end;

    // INDICES
    if OIDIndex in OIDs then
    begin
      Unk := '';
      QIndex.Params.AsString[0] := FName;
      QIndex.Open;
      while not QIndex.Eof do
      begin
        if Unk <> Trim(QIndex.Fields.AsString[0]) then
          with TMetaIndex.Create(Self, Ord(OIDIndex)) do
          begin
            SetLength(FFields, 1);
            FName := Trim(QIndex.Fields.AsString[0]);
            FFields[0] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
            FUnique := QIndex.Fields.AsSingle[2] = 1;
            FActive := QIndex.Fields.AsSingle[3] = 0;
            if QIndex.Fields.AsSingle[4] = 0 then
              FOrder := IoAscending
            else
              FOrder := IoDescending;
            Unk := FName;
          end
        else
          with Indices[IndicesCount - 1] do
          begin
            SetLength(FFields, FieldsCount + 1);
            FFields[FieldsCount - 1] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
            Include(Fields[FieldsCount - 1].FInfos, fIndice);
          end;
        QIndex.Next;
      end;
    end;

    // UNIQUE
    if OIDUnique in OIDs then
    begin
      QPrimary.Params.AsString[0] := 'UNIQUE';
      if not (OIDPrimary in OIDs) then
        QPrimary.Params.AsString[1] := FName;
      QPrimary.Open;
      while not QPrimary.Eof do
      begin
        if Unk <> Trim(QPrimary.Fields.AsString[0]) then
          with TMetaUnique.Create(Self, Ord(OIDUnique)) do
          begin
            SetLength(FFields, 1);
            FName := Trim(QPrimary.Fields.AsString[0]);
            FFields[0] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));
            Unk := FName;
          end
        else
          with Uniques[UniquesCount - 1] do
          begin
            SetLength(FFields, FieldsCount + 1);
            FFields[FieldsCount - 1] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));
            Include(Fields[FieldsCount - 1].FInfos, fUnique);
          end;
        QPrimary.Next;
      end;
    end;

  end;

  // Check
  if OIDCheck in OIDs then
  begin
    QCheck.Params.AsString[0] := FName;
    QCheck.Open;
    while not QCheck.Eof do
      with TMetaCheck.Create(Self, Ord(OIDCheck)) do
      begin
        FName := Trim(QCheck.Fields.AsString[0]);
        QCheck.ReadBlob(1, FConstraint);
        QCheck.Next;
      end;
  end;

  // TRIGGER
  if OIDTableTrigger in OIDs then
  begin
    QTrigger.Params.AsString[0] := FName;
    QTrigger.Open;
    while not QTrigger.Eof do
    begin
      TMetaTrigger.Create(Self, Ord(OIDTableTrigger)).LoadFromQuery(QTrigger);
      QTrigger.Next;
    end;
  end;
end;

procedure TMetaTable.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
end;

procedure TMetaTable.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  inherited SaveToStream(Stream);
end;

procedure TMetaTable.SaveToDDL(Stream: TStringStream);
begin
  inherited SaveToDDL(Stream);
  SaveNode(Stream, Ord(OIDPrimary));
  SaveNode(Stream, Ord(OIDUnique));
  SaveNode(Stream, Ord(OIDIndex));
  SaveNode(Stream, Ord(OIDForeign));
  SaveNode(Stream, Ord(OIDCheck));
  SaveNode(Stream, Ord(OIDTableTrigger), NewLine);
end;

function TMetaTable.GetIndices(const Index: Integer): TMetaIndex;
begin
  Result := TMetaIndex(GetItems(Ord(OIDIndex), Index))
end;

function TMetaTable.GetIndicesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDIndex)].Childs.Count;
end;

function TMetaTable.GetForeign(const Index: Integer): TMetaForeign;
begin
  Result := TMetaForeign(GetItems(Ord(OIDForeign), Index))
end;

function TMetaTable.GetForeignCount: Integer;
begin
  Result := FNodeItems[Ord(OIDForeign)].Childs.Count;
end;

function TMetaTable.FindFieldIndex(const Name: string): Integer;
begin
  for Result := 0 to FieldsCount - 1 do
    if Fields[Result].FName = Name then
      Exit;
  raise EUIBError.CreateFmt(EUIB_FIELDSTRNOTFOUND, [Name]);
end;

function TMetaTable.GetChecks(const Index: Integer): TMetaCheck;
begin
  Result := TMetaCheck(GetItems(Ord(OIDCheck), Index));
end;

function TMetaTable.GetChecksCount: Integer;
begin
  Result := FNodeItems[Ord(OIDCheck)].Childs.Count;
end;

function TMetaTable.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(Ord(OIDTableTrigger), Index));
end;

function TMetaTable.GetTriggersCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTableTrigger)].Childs.Count;
end;

procedure TMetaTable.SaveToDDLNode(Stream: TStringStream);
var
  I: Integer;
begin
  Stream.WriteString(Format('CREATE TABLE %s (', [FName]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(BreakLine + '   ');
    Fields[I].SaveToDDL(Stream);
    if I <> FieldsCount - 1 then
      Stream.WriteString(',');
  end;
  Stream.WriteString(BreakLine + ');');
end;

class function TMetaTable.NodeClass: string;
begin
  Result := 'Table';
end;

class function TMetaTable.NodeType: TMetaNodeType;
begin
  Result := MetaTable;
end;

//=== { TMetaBaseField } =====================================================

procedure TMetaBaseField.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FFieldType, SizeOf(FFieldType));

  if FFieldType = uftNumeric then
  begin
    Stream.Read(FScale, SizeOf(FScale));
    Stream.Read(FPrecision, SizeOf(FPrecision));
  end
  else
  begin
    FScale := 0;
    FPrecision := 0;
  end;

  if FFieldType in [uftChar..uftCstring] then
  begin
    Stream.Read(FLength, SizeOf(FLength));
    ReadString(Stream, FCharSet);
    Stream.Read(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
  end
  else
  begin
    FLength := 0;
    FCharSet := '';
  end;

  if FFieldType = uftBlob then
  begin
    Stream.Read(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Read(FSubType, SizeOf(FSubType));
  end
  else
  begin
    FSegmentLength := 0;
    FSubType := 0;
  end;
end;

procedure TMetaBaseField.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FFieldType, SizeOf(FFieldType));

  if FFieldType = uftNumeric then
  begin
    Stream.Write(FScale, SizeOf(FScale));
    Stream.Write(FPrecision, SizeOf(FPrecision));
  end;

  if FFieldType in [uftChar..uftCstring] then
  begin
    Stream.Write(FLength, SizeOf(FLength));
    WriteString(Stream, FCharSet);
    Stream.Write(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
  end;

  if FFieldType = uftBlob then
  begin
    Stream.Write(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Write(FSubType, SizeOf(FSubType));
  end;
end;

procedure TMetaBaseField.LoadFromQuery(QField, QCharset: TJvUIBStatement);

  procedure FindCharset(const Id: Single; var Charset: string; var Count: Smallint);
  var
    I: Integer;
  begin
    for I := 0 to QCharset.Fields.RecordCount - 1 do
    begin
      QCharset.Fields.GetRecord(I);
      if QCharset.Fields.AsSmallint[0] = Id then
      begin
        Charset := Trim(QCharset.Fields.AsString[1]);
        Count := QCharset.Fields.AsSmallint[2];
        Exit;
      end;
      Charset := '';
      FBytesPerCharacter := 1;
    end;
  end;

begin
  FScale := Abs(QField.Fields.AsSmallInt[1]);
  FLength := QField.Fields.AsSmallInt[2];
  FPrecision := QField.Fields.AsSmallInt[3];
  if FScale > 0 then
  begin
    FFieldType := uftNumeric;
    if FPrecision = 0 then
      case QField.Fields.AsSmallint[0] of
        blr_short:
          FPrecision := 4;
        blr_long:
          FPrecision := 7;
        blr_int64, blr_quad, blr_double:
          FPrecision := 15;
      else
        raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
      end;
  end
  else
    case QField.Fields.AsSmallint[0] of
      blr_text, blr_text2:
        FFieldType := uftChar;
      blr_varying, blr_varying2:
        FFieldType := uftVarchar;
      blr_cstring, blr_cstring2:
        FFieldType := uftCstring;
      blr_short:
        FFieldType := uftSmallint;
      blr_long:
        FFieldType := uftInteger;
      blr_quad:
        FFieldType := uftQuad;
      blr_float, blr_d_float:
        FFieldType := uftFloat;
      blr_double:
        FFieldType := uftDoublePrecision;
      blr_timestamp:
        FFieldType := uftTimestamp;
      blr_blob:
        FFieldType := uftBlob;
      blr_blob_id:
        FFieldType := uftBlobId;
      blr_sql_date:
        FFieldType := uftDate;
      blr_sql_time:
        FFieldType := uftTime;
      blr_int64:
        FFieldType := uftInt64;
      {$IFDEF IB7_UP}
      blr_boolean_dtype:
        FFieldType := uftBoolean;
      {$ENDIF IB7_UP}
    end;
  if (FFieldType in [uftChar, uftVarchar, uftCstring]) and
    not QField.Fields.IsNull[4] then
    FindCharset(QField.Fields.AsSmallint[4], FCharSet, FBytesPerCharacter)
  else
    FBytesPerCharacter := 1;

  FSubType := QField.Fields.AsSmallint[5];
end;

procedure TMetaBaseField.SaveToDDLNode(Stream: TStringStream);
begin
  case FFieldType of
    uftNumeric:
      Stream.WriteString(Format('%s(%d,%d)',
        [FieldTypes[FFieldType], FPrecision, FScale]));
    uftChar..uftCstring:
      begin
        Stream.WriteString(Format('%s(%d)',
          [FieldTypes[FFieldType], FLength div FBytesPerCharacter]));
        if FCharSet <> '' then
          Stream.WriteString(' CHARACTER SET ' + FCharSet);
      end;
    uftBlob:
      Stream.WriteString(Format('%s SUB_TYPE %d SEGMENT SIZE %d',
        [FieldTypes[FFieldType], FSubType, FSegmentLength]));
  else
    Stream.WriteString(Format('%s', [FieldTypes[FFieldType]]));
  end;
end;

class function TMetaBaseField.NodeClass: string;
begin
  Result := 'Field';
end;

function TMetaBaseField.GetShortFieldType: string;
begin
  case FFieldType of
    uftChar..uftCstring:
      Result := Format('%s(%d)', [FieldTypes[FFieldType],
        FLength div FBytesPerCharacter]);
    uftNumeric:
      Result := Format('%s(%d,%d)',
        [FieldTypes[FFieldType], FPrecision, FScale]);
  else
    Result := Format('%s', [FieldTypes[FFieldType]]);
  end;
end;

class function TMetaBaseField.NodeType: TMetaNodeType;
begin
  Result := MetaBaseField;
end;

//=== { TMetaDataBase } ======================================================

constructor TMetaDataBase.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(nil, -1);
  AddClass(TMetaDomain);
  AddClass(TMetaTable);
  AddClass(TMetaView);
  AddClass(TMetaProcedure);
  AddClass(TMetaGenerator);
  AddClass(TMetaException);
  AddClass(TMetaUDF);
  AddClass(TMetaRole);

  FOIDDatabases := ALLOBjects;
  FOIDTables := ALLTables;
  FOIDViews := ALLViews;
  FOIDProcedures := ALLProcedures;
  FOIDUDFs := ALLUDFs;
  FSysInfos := False;
end;

procedure TMetaDataBase.LoadFromDatabase(Transaction: TJvUIBTransaction);
var
  I: Integer;
  ConStr, Str: string;
  QNames, QFields, QCharset, QPrimary: TJvUIBStatement;
  QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement;

  procedure Configure(var Q: TJvUIBStatement; const Qry: string;
    CachedFetch: Boolean = False);
  begin
    Q := TJvUIBStatement.Create(nil);
    Q.Transaction := Transaction;
    Q.CachedFetch := CachedFetch;
    Q.SQL.Text := Qry;
  end;

begin
  CheckTransaction(Transaction);

  FName := Transaction.DataBase.DatabaseName;

  Configure(QNames, '');
  if FSysInfos then
    Configure(QTrigger, QRYSysTrigger)
  else
    Configure(QTrigger, QRYTrigger);
  Configure(QCharset, QRYCharset, True);
  Configure(QFields, QRYTableFields);
  Configure(QPrimary, QRYUnique, True);
  Configure(QIndex, QRYIndex);
  Configure(QForeign, QRYForeign);
  Configure(QCheck, QRYCheck);
  try
    // preload Charsets
    QCharset.Open;
    QCharset.FetchAll;

    // DOMAINS
    if OIDDomain in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDDomain)].Childs.Clear;
      if FSysInfos then
        QNames.SQL.Text := QRYSysDomains
      else
        QNames.SQL.Text := QRYDomains;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaDomain.Create(Self, Ord(OIDDomain)) do
          LoadFromQuery(QNames, QCharset);
        QNames.Next;
      end;
    end;

    // GENERATORS
    if OIDGenerator in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDGenerator)].Childs.Clear;
      QNames.SQL.Text := QRYGenerators;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaGenerator.Create(Self, Ord(OIDGenerator)) do
          LoadFromDataBase(Transaction, Trim(QNames.Fields.AsString[0]));
        QNames.Next;
      end;
    end;

    // TABLES
    if OIDTable in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDTable)].Childs.Clear;
      if FSysInfos then
        QNames.SQL.Text := QRYSysTables
      else
        QNames.SQL.Text := QRYTables;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaTable.Create(Self, Ord(OIDTable)) do
          LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
            QIndex, QForeign, QCheck, QTrigger, FOIDTables);
        QNames.Next;
      end;

      // FOREIGN
      if [OIDForeign, OIDTableField] <= FOIDTables then
      begin
        for I := 0 to TablesCount - 1 do
        begin
          QForeign.Params.AsString[0] := Tables[I].Name;
          QForeign.Open;
          ConStr := '';
          while not QForeign.Eof do
          begin
            if ConStr <> Trim(QForeign.Fields.AsString[0]) then // new
            begin
              with TMetaForeign.Create(Tables[I], Ord(OIDForeign)) do
              begin
                FName := Trim(QForeign.Fields.AsString[0]);
                ConStr := FName;
                FForTable := FindTableIndex(Trim(QForeign.Fields.AsString[3]));
                if 'TABLE1' = Trim(QForeign.Fields.AsString[3]) then
                  beep;
                SetLength(FFields, 1);
                FFields[0] := Tables[I].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
                Include(Tables[I].Fields[FFields[0]].FInfos, fForeign);
                SetLength(FForFields, 1);
                FForFields[0] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));

                Str := Trim(QForeign.Fields.AsString[1]);
                if Str = 'RESTRICT' then
                  FOnUpdate := Restrict
                else
                if Str = 'CASCADE' then
                  FOnUpdate := Cascade
                else
                if Str = 'SET NULL' then
                  FOnUpdate := SetNull
                else
                  FOnUpdate := SetDefault;

                Str := Trim(QForeign.Fields.AsString[2]);
                if Str = 'RESTRICT' then
                  FOnDelete := Restrict
                else
                if Str = 'CASCADE' then
                  FOnDelete := Cascade
                else
                  if Str = 'SET NULL' then
                  FOnDelete := SetNull
                else
                  FOnDelete := SetDefault;
              end;
            end
            else
              with Tables[I].Foreign[Tables[I].ForeignCount - 1] do
              begin
                SetLength(FFields, Length(FFields) + 1);
                FFields[FieldsCount - 1] := Tables[I].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
                Include(Tables[I].Fields[FFields[FieldsCount - 1]].FInfos, fForeign);
                SetLength(FForFields, Length(FForFields) + 1);
                FForFields[ForFieldsCount - 1] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));
              end;
            QForeign.Next;
          end;
        end;
      end;
    end;

    // VIEWS
    if OIDView in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDView)].Childs.Clear;
      QNames.SQL.Text := QRYView;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaView.Create(Self, Ord(OIDView)) do
          LoadFromDataBase(QNames, QFields, QTrigger, QCharset, FOIDViews);
        QNames.Next;
      end;
    end;

    // PROCEDURE
    if OIDProcedure in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDProcedure)].Childs.Clear;
      QNames.SQL.Text := QRYProcedures;
      QFields.SQL.Text := QRYProcFields;
      QNames.Open;
      while not QNames.Eof do
      begin
        with TMetaProcedure.Create(Self, Ord(OIDProcedure)) do
          LoadFromQuery(QNames, QFields, QCharset, FOIDProcedures);
        QNames.Next;
      end;
    end;

    // EXCEPTION
    if OIDException in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDException)].Childs.Clear;
      QNames.SQL.Text := QRYExceptions;
      QNames.Open;
      while not QNames.Eof do
      begin
        TMetaException.Create(Self, Ord(OIDException)).LoadFromQuery(QNames);
        QNames.Next;
      end;
    end;

    // UDF
    if OIDUDF in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDUDF)].Childs.Clear;
      QNames.SQL.Text := QRYUDF;
      QFields.SQL.Text := QRYUDFFields;
      QNames.Open;
      while not QNames.Eof do
      begin
        TMetaUDF.Create(Self, Ord(OIDUDF)).LoadFromQuery(QNames, QFields, QCharset, FOIDUDFs);
        QNames.Next;
      end;
    end;

    // ROLES
    if OIDRole in FOIDDatabases then
    begin
      FNodeItems[Ord(OIDRole)].Childs.Clear;
      QNames.SQL.Text := QRYRoles;
      QNames.Open;
      while not QNames.Eof do
      begin
        TMetaRole.Create(Self, Ord(OIDRole)).LoadFromQuery(QNames);
        QNames.Next;
      end;
    end;
  finally
    QNames.Free;
    QCharset.Free;
    QFields.Free;
    QPrimary.Free;
    QIndex.Free;
    QForeign.Free;
    QCheck.Free;
    QTrigger.Free;
  end;
end;

procedure TMetaDataBase.SaveToDDL(Stream: TStringStream);
var
  I: Integer;

  procedure SaveChildNodes(comment: string; OIDParent, OIDChild: Integer;
    Separator: string = BreakLine);
  var
    I, J: Integer;
  begin
    if TablesCount > 0 then
    begin
      Stream.WriteString(NewLine);
      Stream.WriteString(Format('/* %s */', [comment]));
      Stream.WriteString(BreakLine);
      for I := 0 to FNodeItems[OIDParent].Childs.Count - 1 do
        for J := 0 to GetItems(OIDParent, I).FNodeItems[OIDChild].Childs.Count - 1 do
        begin
          Stream.WriteString(Separator);
          TMetaNode(GetItems(OIDParent, I).FNodeItems[OIDChild].Childs[J]).SaveToDDL(Stream);
        end;
    end;
  end;

  procedure SaveMainNodes(Comment: string; OID: Integer;
    Separator: string = NewLine);
  var
    I: Integer;
  begin
    if FNodeItems[OID].Childs.Count > 0 then
    begin
      Stream.WriteString(NewLine);
      Stream.WriteString(Format('/* %s */', [comment]));
      for I := 0 to FNodeItems[OID].Childs.Count - 1 do
      begin
        if I = 0 then
          Stream.WriteString(NewLine)
        else
          Stream.WriteString(Separator);
        if GetItems(OID, I) is TMetaProcedure then
          TMetaProcedure(GetItems(OID, I)).SaveToPostDDL(Stream)
        else
          GetItems(OID, I).SaveToDDLNode(Stream);
      end;
    end;
  end;

begin
  SaveMainNodes('ROLES', Ord(OIDRole), NewLine);
  SaveMainNodes('FUNCTIONS', Ord(OIDUDF), NewLine);
  SaveMainNodes('DOMAINS', Ord(OIDDomain), BreakLine);
  SaveMainNodes('GENERATORS', Ord(OIDGenerator));
  SaveMainNodes('EXEPTIONS', Ord(OIDException), BreakLine);
  SaveMainNodes('PROCEDURES', Ord(OIDProcedure));
  SaveMainNodes('TABLES', Ord(OIDTable));
  SaveMainNodes('VIEWS', Ord(OIDView));

  SaveChildNodes('UNIQUE', Ord(OIDTable), Ord(OIDUnique));
  SaveChildNodes('PRIMARY', Ord(OIDTable), Ord(OIDPrimary));
  SaveChildNodes('FOREIGN', Ord(OIDTable), Ord(OIDForeign));
  SaveChildNodes('INDICES', Ord(OIDTable), Ord(OIDIndex));
  SaveChildNodes('CHECKS', Ord(OIDTable), Ord(OIDCheck), NewLine);
  SaveChildNodes('TRIGGERS', Ord(OIDTable), Ord(OIDTableTrigger), NewLine);
  SaveChildNodes('TRIGGERS (Views)', Ord(OIDView), Ord(OIDViewTrigers), NewLine);

  if ProceduresCount > 0 then
  begin
    Stream.WriteString(NewLine);
    Stream.WriteString('/* PROCEDURES */');
    for I := 0 to ProceduresCount - 1 do
    begin
      Stream.WriteString(NewLine);
      Procedures[I].SaveToAlterDDL(Stream);
    end;
  end;

end;

function TMetaDataBase.GetGenerators(const Index: Integer): TMetaGenerator;
begin
  Result := TMetaGenerator(GetItems(Ord(OIDGenerator), Index));
end;

function TMetaDataBase.GetGeneratorsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDGenerator)].Childs.Count
end;

function TMetaDataBase.GetTables(const Index: Integer): TMetaTable;
begin
  Result := TMetaTable(GetItems(Ord(OIDTable), Index));
end;

function TMetaDataBase.GetTablesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDTable)].Childs.Count
end;

function TMetaDataBase.FindTableName(const TableName: string): TMetaTable;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to TablesCount - 1 do
    if Tables[I].Name = TableName then
    begin
      Result := Tables[I];
      Exit;
    end;
end;

function TMetaDataBase.FindTableIndex(const TableName: string): Integer;
begin
  for Result := 0 to TablesCount - 1 do
    if Tables[Result].Name = TableName then
      Exit;
  raise Exception.CreateFmt(EUIB_TABLESTRNOTFOUND, [TableName]);
end;

function TMetaDataBase.FindDomainIndex(const DomainName: string): Integer;
begin
  for Result := 0 to DomainsCount - 1 do
    if Domains[Result].Name = DomainName then
      Exit;
  raise Exception.CreateFmt(EUIB_DOMAINSTRNOTFOUND, [DomainName]);
end;

procedure TMetaDataBase.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
end;

function TMetaDataBase.GetViews(const Index: Integer): TMetaView;
begin
  Result := TMetaView(GetItems(Ord(OIDView), Index));
end;

function TMetaDataBase.GetViewsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDView)].Childs.Count
end;

function TMetaDataBase.GetDomains(const Index: Integer): TMetaDomain;
begin
  Result := TMetaDomain(GetItems(Ord(OIDDomain), Index));
end;

function TMetaDataBase.GetDomainsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDDomain)].Childs.Count
end;

function TMetaDataBase.GetProcedures(const Index: Integer): TMetaProcedure;
begin
  Result := TMetaProcedure(GetItems(Ord(OIDProcedure), Index));
end;

function TMetaDataBase.GetProceduresCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcedure)].Childs.Count
end;

function TMetaDataBase.GetExceptions(const Index: Integer): TMetaException;
begin
  Result := TMetaException(GetItems(Ord(OIDException), Index));
end;

function TMetaDataBase.GetExceptionsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDException)].Childs.Count
end;

function TMetaDataBase.GetUDFS(const Index: Integer): TMetaUDF;
begin
  Result := TMetaUDF(GetItems(Ord(OIDUDF), Index));
end;

function TMetaDataBase.GetUDFSCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUDF)].Childs.Count
end;

class function TMetaDataBase.NodeClass: string;
begin
  Result := 'Database';
end;

procedure TMetaDataBase.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  inherited SaveToStream(Stream);
end;

function TMetaDataBase.GetRoles(const Index: Integer): TMetaRole;
begin
  Result := TMetaRole(GetItems(Ord(OIDRole), Index));
end;

function TMetaDataBase.GetRolesCount: Integer;
begin
  Result := FNodeItems[Ord(OIDRole)].Childs.Count
end;

class function TMetaDataBase.NodeType: TMetaNodeType;
begin
  Result := MetaDatabase;
end;

function TMetaDataBase.FindProcName(const ProcName: string): TMetaProcedure;
var
  I: Integer;
begin
  for I := 0 to ProceduresCount - 1 do
    if Procedures[I].Name = ProcName then
    begin
      Result := Procedures[I];
      Exit;
    end;
  raise Exception.CreateFmt(EUIB_PROCSTRNOTFOUND, [ProcName]);
end;

//=== { TMetaConstraint } ====================================================

function TMetaConstraint.GetFields(const Index: Word): TMetaTableField;
begin
  Assert((FieldsCount > 0) and (Index < FieldsCount), IntToStr(Index) + ' ' + ClassName);
  Result := TMetaTable(FOwner).Fields[FFields[Index]];
end;

function TMetaConstraint.GetFieldsCount: Word;
begin
  Result := Length(FFields);
end;

class function TMetaConstraint.NodeClass: string;
begin
  Result := 'Constraint'
end;

procedure TMetaConstraint.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Read(I, SizeOf(I));
  SetLength(FFields, I);
  if I > 0 then
  begin
    ReadString(Stream, FName);
    for I := 0 to I - 1 do
    begin
      Stream.Read(FFields[I], SizeOf(FFields[I]));
      case NodeType of
        MetaForeign:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fForeign);
        MetaIndex:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fIndice);
        MetaPrimary:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fPrimary);
        MetaUnique:
          Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fPrimary);
      end;
    end;
  end;
end;

procedure TMetaConstraint.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  I := FieldsCount;
  Stream.Write(I, SizeOf(I));
  if I > 0 then
  begin
    WriteString(Stream, FName);
    for I := 0 to I - 1 do
      Stream.Write(FFields[I], SizeOf(FFields[I]));
  end;
end;

class function TMetaConstraint.NodeType: TMetaNodeType;
begin
  Result := MetaConstraint;
end;

//=== { TMetaUnique } ========================================================

class function TMetaUnique.NodeClass: string;
begin
  Result := 'Unique';
end;

class function TMetaUnique.NodeType: TMetaNodeType;
begin
  Result := MetaUnique;
end;

procedure TMetaUnique.SaveToDDL(Stream: TStringStream);
var
  I: Integer;
begin
  Stream.WriteString(Format('ALTER TABLE %s ADD UNIQUE (',
    [TMetaTable(FOwner).FName]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
end;

//=== { TMetaPrimary } =======================================================

class function TMetaPrimary.NodeClass: string;
begin
  Result := 'Primary key';
end;

procedure TMetaPrimary.LoadFromQuery(Q: TJvUIBStatement);
var
  I: Integer;
begin
  FName := Trim(Q.Fields.AsString[0]);
  Q.FetchAll;
  SetLength(FFields, Q.Fields.RecordCount);
  for I := 0 to Q.Fields.RecordCount - 1 do
  begin
    Q.Fields.GetRecord(I);
    FFields[I] := TMetaTable(FOwner).FindFieldIndex(Trim(Q.Fields.AsString[1]));
    Include(TMetaTable(FOwner).Fields[FFields[I]].FInfos, fPrimary);
  end;
end;

procedure TMetaPrimary.SaveToDDLNode(Stream: TStringStream);
var
  I: Integer;
begin
  if copy(FName, 0, 6) = 'INTEG_' then
    Stream.WriteString(Format('ALTER TABLE %s ADD PRIMARY KEY (',
      [TMetaTable(FOwner).FName]))
  else
    Stream.WriteString(Format('ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (',
      [TMetaTable(FOwner).FName, FName]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
end;

class function TMetaPrimary.NodeType: TMetaNodeType;
begin
  Result := MetaPrimary;
end;

//=== { TMetaIndex } =========================================================

class function TMetaIndex.NodeClass: string;
begin
  // (rom) better Indices or Index?
  Result := 'Indice';
end;

procedure TMetaIndex.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FUnique, SizeOf(FUnique));
  Stream.Read(FActive, SizeOf(FActive));
  Stream.Read(FOrder, SizeOf(FOrder));
end;

procedure TMetaIndex.SaveToDDLNode(Stream: TStringStream);
var
  I: Integer;
  UNIQUE, ORDER: string;
begin
  if FUnique then
    UNIQUE := ' UNIQUE';
  if FOrder = IoDescending then
    ORDER := ' DESCENDING';

  Stream.WriteString(Format('CREATE%s%s INDEX %s ON %s (',
    [ORDER, UNIQUE, FName, TMetaTable(FOwner).FName]));

  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
  if not FActive then
    Stream.WriteString(Format('%sALTER INDEX %s INACTIVE;', [BreakLine, FName]));
end;

procedure TMetaIndex.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FUnique, SizeOf(FUnique));
  Stream.Write(FActive, SizeOf(FActive));
  Stream.Write(FOrder, SizeOf(FOrder));
end;

class function TMetaIndex.NodeType: TMetaNodeType;
begin
  Result := MetaIndex;
end;

//=== { TMetaForeign } =======================================================

function TMetaForeign.GetForFields(const Index: Word): TMetaTableField;
begin
  Assert((ForFieldsCount > 0) and (Index < ForFieldsCount));

  Result := ForTable.Fields[FForFields[Index]];
end;

function TMetaForeign.GetForFieldsCount: Word;
begin
  Result := Length(FForFields);
end;

function TMetaForeign.GetForTable: TMetaTable;
begin
  Result := TMetaDataBase(FOwner.FOwner).Tables[FForTable];
end;

class function TMetaForeign.NodeClass: string;
begin
  Result := 'Foreign';
end;

procedure TMetaForeign.LoadFromStream(Stream: TStream);
var
  I: Integer;
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FForTable, SizeOf(FForTable));
  Stream.Read(FOnDelete, SizeOf(FOnDelete));
  Stream.Read(FOnUpdate, SizeOf(FOnUpdate));
  Stream.Read(I, SizeOf(I));
  SetLength(FForFields, I);
  for I := 0 to I - 1 do
    Stream.Read(FForFields[I], SizeOf(FForFields[I]));
end;

procedure TMetaForeign.SaveToDDLNode(Stream: TStringStream);
var
  I: Integer;
begin
  if copy(FName, 0, 6) = 'INTEG_' then
    Stream.WriteString(Format('ALTER TABLE %s ADD FOREIGN KEY (',
      [TMetaTable(FOwner).FName]))
  else
    Stream.WriteString(Format('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (',
      [TMetaTable(FOwner).FName, FName]));

  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(Format(') REFERENCES %s (', [ForTable.Name]));
  for I := 0 to ForFieldsCount - 1 do
  begin
    Stream.WriteString(ForFields[I].Name);
    if I <> ForFieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(')');

  case OnDelete of
    Cascade:
      Stream.WriteString(' ON DELETE CASCADE');
    SetNull:
      Stream.WriteString(' ON DELETE SET NULL');
    SetDefault:
      Stream.WriteString(' ON DELETE SET DEFAULT');
  end;

  case OnUpdate of
    Cascade:
      Stream.WriteString(' ON UPDATE CASCADE');
    SetNull:
      Stream.WriteString(' ON UPDATE SET NULL');
    SetDefault:
      Stream.WriteString(' ON UPDATE SET DEFAULT');
  end;

  Stream.WriteString(';');
end;

procedure TMetaForeign.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  inherited SaveToStream(Stream);
  Stream.Write(FForTable, SizeOf(FForTable));
  Stream.Write(FOnDelete, SizeOf(FOnDelete));
  Stream.Write(FOnUpdate, SizeOf(FOnUpdate));
  I := ForFieldsCount;
  Stream.Write(I, SizeOf(I));
  for I := 0 to I - 1 do
    Stream.Write(FForFields[I], SizeOf(FForFields[I]));
end;

class function TMetaForeign.NodeType: TMetaNodeType;
begin
  Result := MetaForeign;
end;

//=== { TMetaCheck } =========================================================

class function TMetaCheck.NodeClass: string;
begin
  Result := 'Check';
end;

procedure TMetaCheck.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FConstraint);
end;

procedure TMetaCheck.SaveToDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('ALTER TABLE %s ADD %s;',
    [TMetaTable(FOwner).Name, FConstraint]));
end;

procedure TMetaCheck.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FConstraint);
end;

class function TMetaCheck.NodeType: TMetaNodeType;
begin
  Result := MetaCheck;
end;

//=== { TMetaTrigger } =======================================================

class function TMetaTrigger.DecodePrefix(Value: Integer): TTriggerPrefix;
begin
  Result := TTriggerPrefix((Value + 1) and 1);
end;

class function TMetaTrigger.DecodeSuffixes(Value: Integer): TTriggerSuffixes;
var
  V, Slot: Integer;
begin
  Result := [];
  for Slot := 1 to 3 do
  begin
    V := ((Value + 1) shr (Slot * 2 - 1)) and 3;
    if V > 0 then
      Include(Result, TTriggerSuffix(V - 1));
  end;
end;

class function TMetaTrigger.NodeClass: string;
begin
  Result := 'Trigger';
end;

procedure TMetaTrigger.LoadFromQuery(Q: TJvUIBStatement);
begin
  FName := Trim(Q.Fields.AsString[0]);
  Q.ReadBlob(1, FSource);
  FPosition := Q.Fields.AsSmallint[2];
  FPrefix := DecodePrefix(Q.Fields.AsSmallint[3]);
  FSuffix := DecodeSuffixes(Q.Fields.AsSmallint[3]);
  FActive := Q.Fields.AsSmallint[4] = 0;
end;

procedure TMetaTrigger.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FPrefix, SizeOf(FPrefix));
  Stream.Read(FSuffix, SizeOf(FSuffix));
  Stream.Read(FPosition, SizeOf(FPosition));
  Stream.Read(FActive, SizeOf(FActive));
  ReadString(Stream, FSource);
end;

procedure TMetaTrigger.SaveToDDLNode(Stream: TStringStream);
var
  Count: Smallint;
  Suf: TTriggerSuffix;
begin
  Stream.WriteString(Format('CREATE TRIGGER %s FOR %s%s',
    [Name, TMetaNode(FOwner).Name, BreakLine]));
  if FActive then
    Stream.WriteString('ACTIVE ');

  Stream.WriteString(TriggerPrefixTypes[FPrefix] + ' ');
  Count := 0;
  for Suf := Insert to Delete do
    if Suf in FSuffix then
    begin
      Inc(Count);
      if Count > 1 then
        Stream.WriteString(' OR ');
      Stream.WriteString(TriggerSuffixTypes[Suf]);
    end;
  Stream.WriteString(Format(' POSITION %d%s%s;', [FPosition, BreakLine, FSource]));
end;

procedure TMetaTrigger.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FPrefix, SizeOf(FPrefix));
  Stream.Write(FSuffix, SizeOf(FSuffix));
  Stream.Write(FPosition, SizeOf(FPosition));
  Stream.Write(FActive, SizeOf(FActive));
  WriteString(Stream, FSource);
end;

class function TMetaTrigger.NodeType: TMetaNodeType;
begin
  Result := MetaTrigger;
end;

//=== { TMetaView } ==========================================================

constructor TMetaView.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaField);
  AddClass(TMetaTrigger);
end;

function TMetaView.GetFields(const Index: Integer): TMetaField;
begin
  Result := TMetaField(GetItems(Ord(OIDViewFields), Index))
end;

function TMetaView.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDViewFields)].Childs.Count;
end;

class function TMetaView.NodeClass: string;
begin
  Result := 'View';
end;

function TMetaView.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(Ord(OIDViewTrigers), Index))
end;

function TMetaView.GetTriggersCount: Integer;
begin
  Result := FNodeItems[Ord(OIDViewTrigers)].Childs.Count;
end;

procedure TMetaView.LoadFromDataBase(QName, QFields, QTriggers,
  QCharset: TJvUIBStatement; OIDs: TOIDViews);
begin
  FName := Trim(QName.Fields.AsString[0]);
  QName.ReadBlob(1, FSource);
  FSource := Trim(FSource);

  // FIELD
  if OIDViewFields in OIDs then
  begin
    QFields.Params.AsString[0] := FName;
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaField.Create(Self, Ord(OIDViewFields)).LoadFromQuery(QFields, QCharset);
      QFields.Next;
    end;
  end;

  // TRIGGER
  if OIDViewTrigers in OIDs then
  begin
    QTriggers.Params.AsString[0] := FName;
    QTriggers.Open;
    while not QTriggers.Eof do
    begin
      TMetaTrigger.Create(Self, Ord(OIDViewTrigers)).LoadFromQuery(QTriggers);
      QTriggers.Next;
    end;
  end;
end;

procedure TMetaView.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FSource);
end;

procedure TMetaView.SaveToDDL(Stream: TStringStream);
begin
  inherited SaveToDDL(Stream);
  SaveNode(Stream, Ord(OIDViewTrigers), NewLine);
end;

procedure TMetaView.SaveToDDLNode(Stream: TStringStream);
var
  I: Integer;
begin
  Stream.WriteString(Format('CREATE VIEW %s (', [Name]));
  for I := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(BreakLine + '   ' + Fields[I].Name);
    if I <> FieldsCount - 1 then
      Stream.WriteString(',');
  end;
  Stream.WriteString(BreakLine + ')' + BreakLine + 'AS' + BreakLine);
  Stream.WriteString(Source);
  Stream.WriteString(';');
end;

procedure TMetaView.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FSource);
  inherited SaveToStream(Stream);
end;

class function TMetaView.NodeType: TMetaNodeType;
begin
  Result := MetaView;
end;

//=== { TMetaDomain } ========================================================

class function TMetaDomain.NodeClass: string;
begin
  Result := 'Domain';
end;

class function TMetaDomain.NodeType: TMetaNodeType;
begin
  Result := MetaDomain;
end;

procedure TMetaDomain.SaveToDDL(Stream: TStringStream);
begin
  SaveToDDLNode(Stream);
end;

procedure TMetaDomain.SaveToDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE DOMAIN %s AS ', [FName]));
  inherited SaveToDDLNode(Stream);
  Stream.WriteString(';');
end;

//=== { TMetaProcedure } =====================================================

constructor TMetaProcedure.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaProcInField); // in
  AddClass(TMetaProcOutField); // out
end;

function TMetaProcedure.GetInputFields(const Index: Integer): TMetaProcInField;
begin
  Result := TMetaProcInField(GetItems(Ord(OIDProcFieldIn), Index))
end;

function TMetaProcedure.GetInputFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcFieldIn)].Childs.Count;
end;

class function TMetaProcedure.NodeClass: string;
begin
  Result := 'Procedure';
end;

function TMetaProcedure.GetOutputFields(const Index: Integer): TMetaProcOutField;
begin
  Result := TMetaProcOutField(GetItems(Ord(OIDProcFieldOut), Index))
end;

function TMetaProcedure.GetOutputFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDProcFieldOut)].Childs.Count;
end;

procedure TMetaProcedure.InternalSaveToDDL(Stream: TStringStream;
  Operation: string);
var
  I: Integer;
begin
  Stream.WriteString(Format('%s PROCEDURE %s', [Operation, FName]));
  if InputFieldsCount > 0 then
  begin
    Stream.WriteString(' (');
    for I := 0 to InPutFieldsCount - 1 do
    begin
      Stream.WriteString(BreakLine + '   ');
      InputFields[I].SaveToDDL(Stream);
      if I <> InputFieldsCount - 1 then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;

  if OutputFieldsCount > 0 then
  begin
    Stream.WriteString(Format('%sRETURNS (', [BreakLine]));
    for I := 0 to OutputFieldsCount - 1 do
    begin
      Stream.WriteString(BreakLine + '   ');
      OutputFields[I].SaveToDDL(Stream);
      if I <> OutputFieldsCount - 1 then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;
end;

procedure TMetaProcedure.LoadFromQuery(QNames, QFields,
  QCharset: TJvUIBStatement; OIDs: TOIDProcedures);
begin
  FName := Trim(QNames.Fields.AsString[0]);
  QNames.ReadBlob(1, FSource);
  QFields.Params.AsString[0] := FName;

  if OIDProcFieldIn in OIDs then
  begin
    QFields.Params.AsSmallint[1] := 0; // in
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaProcInField.Create(Self, Ord(OIDProcFieldIn)).LoadFromQuery(QFields, QCharset);
      QFields.Next;
    end;
  end;

  if OIDProcFieldOut in OIDs then
  begin
    QFields.Params.AsSmallint[1] := 1; // out
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaProcOutField.Create(Self, Ord(OIDProcFieldOut)).LoadFromQuery(QFields, QCharset);
      QFields.Next;
    end;
  end;
end;

procedure TMetaProcedure.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FSource);
end;

procedure TMetaProcedure.SaveToAlterDDL(Stream: TStringStream);
begin
  InternalSaveToDDL(Stream, 'ALTER');
  Stream.WriteString(BreakLine + 'AS');
  Stream.WriteString(FSource);
  Stream.WriteString(';');
end;

procedure TMetaProcedure.SaveToPostDDL(Stream: TStringStream);
begin
  InternalSaveToDDL(Stream, 'CREATE');
  Stream.WriteString(BreakLine + 'AS' + breakline + 'BEGIN' + breakline +
    '  EXIT;' + breakline + 'END;');
end;

procedure TMetaProcedure.SaveToDDLNode(Stream: TStringStream);
begin
  InternalSaveToDDL(Stream, 'CREATE');
  Stream.WriteString(BreakLine + 'AS');
  Stream.WriteString(FSource);
  Stream.WriteString(';');
end;

procedure TMetaProcedure.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FSource);
  inherited SaveToStream(Stream);
end;

class function TMetaProcedure.NodeType: TMetaNodeType;
begin
  Result := MetaProcedure;
end;

//=== { TMetaException } =====================================================

class function TMetaException.NodeClass: string;
begin
  Result := 'Exception';
end;

procedure TMetaException.LoadFromQuery(QName: TJvUIBStatement);
begin
  FName := Trim(QName.Fields.AsString[0]);
  FMessage := QName.Fields.AsString[1];
  FNumber := QName.Fields.AsInteger[2];
end;

procedure TMetaException.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FMessage);
  Stream.Read(FNumber, SizeOf(FNumber))
end;

procedure TMetaException.SaveToDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE EXCEPTION %s ''%s'';', [FName, FMessage]));
end;

procedure TMetaException.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FMessage);
  Stream.Write(FNumber, SizeOf(FNumber));
  inherited SaveToStream(Stream);
end;

class function TMetaException.NodeType: TMetaNodeType;
begin
  Result := MetaException;
end;

//=== { TMetaUDF } ===========================================================

constructor TMetaUDF.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited Create(AOwner, ClassIndex);
  AddClass(TMetaUDFField);
end;

class function TMetaUDF.NodeClass: string;
begin
  Result := 'UDF';
end;

procedure TMetaUDF.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FModule);
  ReadString(Stream, FEntry);
  Stream.Read(FReturn, SizeOf(FReturn));
end;

procedure TMetaUDF.SaveToDDLNode(Stream: TStringStream);
var
  I, C: Integer;
begin
  Stream.WriteString(Format('DECLARE EXTERNAL FUNCTION %s', [Fname]));
  C := 0;

  if FReturn = 0 then
  begin // return position
    for I := 0 to FieldsCount - 1 do
      if Fields[I].Position <> Return then
      begin
        if C > 0 then
          Stream.WriteString(',');
        Stream.WriteString(BreakLine + '  ');
        Fields[I].SaveToDDL(Stream);
        Inc(C);
      end;
    for I := 0 to FieldsCount - 1 do
      if Fields[I].Position = Return then
      begin
        Stream.WriteString(BreakLine + '  RETURNS ');
        Fields[I].SaveToDDL(Stream);
        Break;
      end;
  end
  else
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      if C > 0 then
        Stream.WriteString(',');
      Stream.WriteString(BreakLine + '  ');
      Fields[I].SaveToDDL(Stream);
      Inc(C);
    end;
    Stream.WriteString(Format('%s  RETURNS PARAMETER %d', [BreakLine, Freturn]));
  end;

  Stream.WriteString(Format('%s  ENTRY_POINT ''%s'' MODULE_NAME ''%s'';',
    [BreakLine, FEntry, FModule]));
end;

procedure TMetaUDF.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FModule);
  WriteString(Stream, FEntry);
  Stream.Write(FReturn, SizeOf(FReturn));
  inherited SaveToStream(Stream);
end;

procedure TMetaUDF.LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement; OIDs: TOIDUDFs);
begin
  FName := Trim(QNames.Fields.AsString[0]);
  FModule := QNames.Fields.AsString[1];
  FEntry := Trim(QNames.Fields.AsString[2]);
  FReturn := QNames.Fields.AsSmallint[3];

  if OIDUDFField in OIDs then
  begin
    QFields.Params.AsString[0] := FName;
    QFields.Open;
    while not QFields.Eof do
    begin
      TMetaUDFField.Create(Self, Ord(OIDUDFField)).LoadFromQuery(QFields, QCharset);
      QFields.Next;
    end;
  end;
end;

function TMetaUDF.GetFields(const Index: Integer): TMetaUDFField;
begin
  Result := TMetaUDFField(GetItems(Ord(OIDUDFField), Index))
end;

function TMetaUDF.GetFieldsCount: Integer;
begin
  Result := FNodeItems[Ord(OIDUDFField)].Childs.Count;
end;

class function TMetaUDF.NodeType: TMetaNodeType;
begin
  Result := MetaUDF;
end;

//=== { TMetaTableField } ====================================================

function TMetaTableField.GetDomain: TMetaDomain;
begin
  if FDomain >= 0 then
    Result := TMetaDatabase(FOwner.FOwner).Domains[FDomain]
  else
    Result := nil;
end;

procedure TMetaTableField.LoadFromQuery(Q, C: TJvUIBStatement);
begin
  inherited LoadFromQuery(Q, C);
  FNotNull := (Q.Fields.AsSmallint[8] = 1);
  if not Q.Fields.IsNull[9] then
  begin
    Q.ReadBlob(9, FDefaultValue);

    FDefaultValue := Trim(FDefaultValue);
    if FDefaultValue <> '' then
      FDefaultValue := Copy(FDefaultValue, 9,
        System.Length(FDefaultValue) - 8);
  end
  else
    FDefaultValue := '';

  FDomain := -1;
  if not (Self is TMetaDomain) then
  begin
    if OIDDomain in TMetaDataBase(FOwner.FOwner).FOIDDatabases then
      if not (Q.Fields.IsNull[10] or (Copy(Q.Fields.AsString[10], 1, 4) = 'RDB$')) then
        FDomain :=
          TMetaDataBase(FOwner.FOwner).FindDomainIndex(Trim(Q.Fields.AsString[10]));
    Q.ReadBlob(11, FComputedSource);
  end;
end;

procedure TMetaTableField.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  ReadString(Stream, FDefaultValue);
  Stream.Read(FNotNull, SizeOf(FNotNull));
  Stream.Read(FDomain, SizeOf(FDomain));
  ReadString(Stream, FComputedSource);
end;

class function TMetaTableField.NodeType: TMetaNodeType;
begin
  Result := MetaTableField;
end;

procedure TMetaTableField.SaveToDDLNode(Stream: TStringStream);
begin
  if FDomain >= 0 then
    Stream.WriteString(Domain.Name) else
    if FComputedSource <> '' then
      Stream.WriteString('COMPUTED BY ' + FComputedSource) else
      inherited SaveToDDLNode(Stream);
  if FDefaultValue <> '' then
    Stream.WriteString(' DEFAULT ' + FDefaultValue);
  if FNotNull then
    Stream.WriteString(' NOT NULL');
end;

procedure TMetaTableField.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  WriteString(Stream, FDefaultValue);
  Stream.Write(FNotNull, SizeOf(FNotNull));
  Stream.Write(FDomain, SizeOf(FDomain));
  WriteString(Stream, FComputedSource);
end;

//=== { TMetaProcInField } ===================================================

class function TMetaProcInField.NodeClass: string;
begin
  Result := 'Input parameter';
end;

class function TMetaProcInField.NodeType: TMetaNodeType;
begin
  Result := MetaProcInField;
end;

//=== { TMetaProcOutField } ==================================================

class function TMetaProcOutField.NodeClass: string;
begin
  Result := 'Output parameter';
end;

class function TMetaProcOutField.NodeType: TMetaNodeType;
begin
  Result := MetaProcOutField;
end;

//=== { TMetaField } =========================================================

procedure TMetaField.LoadFromQuery(Q, C: TJvUIBStatement);
begin
  inherited LoadFromQuery(Q, C);
  FName := Trim(Q.Fields.AsString[6]);
  FSegmentLength := Q.Fields.AsSmallint[7];
end;

class function TMetaField.NodeType: TMetaNodeType;
begin
  Result := MetaField;
end;

procedure TMetaField.SaveToDDL(Stream: TStringStream);
begin
  Stream.WriteString(FName + ' ');
  inherited SaveToDDL(Stream);
end;

//=== { TMetaUDFField } ======================================================

procedure TMetaUDFField.LoadFromQuery(QField, QCharset: TJvUIBStatement);
begin
  inherited LoadFromQuery(QField, QCharset);
  FPosition := QField.Fields.AsSmallint[6];
  FMechanism := QField.Fields.AsSmallint[7];
  FName := 'Field ' + IntToStr(FPosition);
end;

procedure TMetaUDFField.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FPosition, SizeOf(FPosition));
  Stream.Read(FMechanism, SizeOf(FMechanism));
end;

class function TMetaUDFField.NodeType: TMetaNodeType;
begin
  Result := MetaUDFField;
end;

procedure TMetaUDFField.SaveToDDLNode(Stream: TStringStream);
begin
  if FFieldType = uftBlob then
    Stream.WriteString('BLOB')
  else
    inherited SaveToDDLNode(Stream);
  case FMechanism of
    -1:
      Stream.WriteString(' FREE_IT');
    0:
      Stream.WriteString(' BY VALUE');
    1:
      ; // BY REFERENCE = default
    2:
      Stream.WriteString(' BY DESCRIPTOR');
  end;
end;

procedure TMetaUDFField.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FPosition, SizeOf(FPosition));
  Stream.Write(FMechanism, SizeOf(FMechanism));
end;

//=== { TMetaRole } ==========================================================

procedure TMetaRole.LoadFromQuery(QName: TJvUIBStatement);
begin
  FName := Trim(QName.Fields.AsString[0]);
  FOwner := QName.Fields.AsString[1];
end;

procedure TMetaRole.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FOwner);
end;

class function TMetaRole.NodeClass: string;
begin
  Result := 'Role';
end;

class function TMetaRole.NodeType: TMetaNodeType;
begin
  Result := MetaRole;
end;

procedure TMetaRole.SaveToDDLNode(Stream: TStringStream);
begin
  Stream.WriteString(Format('CREATE ROLE %s /* By user %s */', [FName, Trim(FOwner)]));
end;

procedure TMetaRole.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FOwner);
  inherited SaveToStream(Stream);
end;

end.

