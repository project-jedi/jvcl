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
{ The Original Code is JvUIBMetaData.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Contributor:   Ritsaert Hornstra                                             }
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

unit JvUIBMetaData;

{$I JVCL.INC}
{$I JvUIB.inc}

interface
uses Classes, SysUtils, JvUIBase, JvUIBLib, JvUIB, JvUIBConst;

type

  TTriggerPrefix = (Before, After);
  TTriggerSuffix = (Insert, Update, Delete);
  TTriggerSuffixes = set of TTriggerSuffix;
  TIndexOrder = (IoDescending, IoAscending);
  TUpdateRule = (Restrict, Cascade, SetNull, SetDefault);
  TTableFieldInfo = (fPrimary, fForeign, fIndice, fUnique);
  TTableFieldInfos = set of TTableFieldInfo;

  // indentation = inherit
  TMetaNodeType = (
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

const
  BreakLine = #13#10;
  NewLine = BreakLine + BreakLine;

  TriggerPrefixTypes: array[TTriggerPrefix] of string =
    ('BEFORE', 'AFTER');

  TriggerSuffixTypes: array[TTriggerSuffix] of string =
    ('INSERT', 'UPDATE', 'DELETE');

  FieldTypes: array[TUIBFieldType] of string = ('', 'NUMERIC', 'CHAR', 'VARCHAR',
    'CSTRING', 'SMALLINT', 'INTEGER', 'QUAD', 'FLOAT', 'DOUBLE PRECISION',
    'TIMESTAMP', 'BLOB', 'BLOBID', 'DATE', 'TIME', 'INT64' {$IFDEF IB7_UP}
    ,'BOOLEAN' {$ENDIF});

  QRYGenerators =
    'SELECT RDB$GENERATOR_NAME FROM RDB$GENERATORS GEN WHERE ' +
    '(NOT GEN.RDB$GENERATOR_NAME STARTING WITH ''RDB$'') AND ' +
    '(NOT GEN.RDB$GENERATOR_NAME STARTING WITH ''SQL$'') AND ' +
    '((GEN.RDB$SYSTEM_FLAG IS NULL) OR (GEN.RDB$SYSTEM_FLAG <> 1)) ' +
    'ORDER BY GEN.RDB$GENERATOR_NAME';

  QRYTables =
    'SELECT REL.RDB$RELATION_NAME FROM RDB$RELATIONS REL WHERE '+
    '(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND '+
    '(NOT REL.RDB$FLAGS IS NULL) AND '+
    '(REL.RDB$VIEW_BLR IS NULL) AND '+
    '(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'') '+
    'ORDER BY REL.RDB$RELATION_NAME';

  QRYTableFields =
    'SELECT FLD.RDB$FIELD_TYPE, FLD.RDB$FIELD_SCALE, '+
    'FLD.RDB$FIELD_LENGTH, FLD.RDB$FIELD_PRECISION, '+
    'FLD.RDB$CHARACTER_SET_ID, FLD.RDB$FIELD_SUB_TYPE, RFR.RDB$FIELD_NAME, '+
    'FLD.RDB$SEGMENT_LENGTH, RFR.RDB$NULL_FLAG, RFR.RDB$DEFAULT_SOURCE, ' +
    'RFR.RDB$FIELD_SOURCE '+
    'FROM RDB$RELATIONS REL, RDB$RELATION_FIELDS RFR, RDB$FIELDS FLD '+
    'WHERE (RFR.RDB$FIELD_SOURCE = FLD.RDB$FIELD_NAME) AND '+
    '(RFR.RDB$RELATION_NAME = REL.RDB$RELATION_NAME) AND '+
    '(REL.RDB$RELATION_NAME = ?) '+
    'ORDER BY RFR.RDB$FIELD_POSITION, RFR.RDB$FIELD_NAME';

  QRYCharset =
    'SELECT RDB$CHARACTER_SET_ID, RDB$CHARACTER_SET_NAME, RDB$BYTES_PER_CHARACTER FROM RDB$CHARACTER_SETS';

  QRYUnique =
    'SELECT RC.RDB$CONSTRAINT_NAME, IDX.RDB$FIELD_NAME '+
    'FROM RDB$RELATION_CONSTRAINTS RC, RDB$INDEX_SEGMENTS IDX '+
    'WHERE (IDX.RDB$INDEX_NAME = RC.RDB$INDEX_NAME) AND '+
    '(RC.RDB$CONSTRAINT_TYPE = ?) '+
    'AND (RC.RDB$RELATION_NAME = ?) '+
    'ORDER BY RC.RDB$RELATION_NAME, IDX.RDB$FIELD_POSITION';

  QRYIndex =
    'SELECT IDX.RDB$INDEX_NAME, ISG.RDB$FIELD_NAME, IDX.RDB$UNIQUE_FLAG, '+
    'IDX.RDB$INDEX_INACTIVE, IDX.RDB$INDEX_TYPE FROM RDB$INDICES IDX '+
    'LEFT JOIN RDB$INDEX_SEGMENTS ISG ON ISG.RDB$INDEX_NAME = IDX.RDB$INDEX_NAME '+
    'LEFT JOIN RDB$RELATION_CONSTRAINTS C ON IDX.RDB$INDEX_NAME = C.RDB$INDEX_NAME '+
    'WHERE (C.RDB$CONSTRAINT_NAME IS NULL) AND (IDX.RDB$RELATION_NAME = ?) '+
    'ORDER BY IDX.RDB$RELATION_NAME, IDX.RDB$INDEX_NAME, ISG.RDB$FIELD_POSITION';

  QRYForeign =
    'SELECT A.RDB$CONSTRAINT_NAME, B.RDB$UPDATE_RULE, B.RDB$DELETE_RULE, '+
    'C.RDB$RELATION_NAME AS FK_TABLE, D.RDB$FIELD_NAME AS FK_FIELD, '+
    'E.RDB$FIELD_NAME AS ONFIELD '+
    'FROM RDB$REF_CONSTRAINTS B, RDB$RELATION_CONSTRAINTS A, RDB$RELATION_CONSTRAINTS C, '+
    'RDB$INDEX_SEGMENTS D, RDB$INDEX_SEGMENTS E '+
    'WHERE (A.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'') AND '+
    '(A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) AND '+
    '(B.RDB$CONST_NAME_UQ=C.RDB$CONSTRAINT_NAME) AND (C.RDB$INDEX_NAME=D.RDB$INDEX_NAME) AND '+
    '(A.RDB$INDEX_NAME=E.RDB$INDEX_NAME) AND '+
    '(D.RDB$FIELD_POSITION = E.RDB$FIELD_POSITION) ' +
    'AND (A.RDB$RELATION_NAME = ?) '+
    'ORDER BY A.RDB$CONSTRAINT_NAME, A.RDB$RELATION_NAME, D.RDB$FIELD_POSITION, E.RDB$FIELD_POSITION';

  QRYCheck =
    'SELECT A.RDB$CONSTRAINT_NAME, C.RDB$TRIGGER_SOURCE '+
    'FROM RDB$RELATION_CONSTRAINTS A, RDB$CHECK_CONSTRAINTS B, RDB$TRIGGERS C '+
    'WHERE (A.RDB$CONSTRAINT_TYPE = ''CHECK'') AND '+
    '(A.RDB$CONSTRAINT_NAME = B.RDB$CONSTRAINT_NAME) AND '+
    '(B.RDB$TRIGGER_NAME = C.RDB$TRIGGER_NAME) AND '+
    '(C.RDB$TRIGGER_TYPE = 1) '+
    'AND (A.RDB$RELATION_NAME = ?)';

  QRYTrigger =
    'SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_SOURCE, T.RDB$TRIGGER_SEQUENCE, '+
    'T.RDB$TRIGGER_TYPE, T.RDB$TRIGGER_INACTIVE, T.RDB$SYSTEM_FLAG '+
    'from RDB$TRIGGERS T left join RDB$CHECK_CONSTRAINTS C ON C.RDB$TRIGGER_NAME = '+
    'T.RDB$TRIGGER_NAME where ((T.RDB$SYSTEM_FLAG = 0) or (T.RDB$SYSTEM_FLAG is null)) '+
    'and (c.rdb$trigger_name is null) and (T.RDB$RELATION_NAME = ?) '+
    'order by T.RDB$TRIGGER_NAME';

  QRYView =
    'SELECT REL.RDB$RELATION_NAME, REL.RDB$VIEW_SOURCE FROM RDB$RELATIONS REL WHERE '+
    '(REL.RDB$SYSTEM_FLAG <> 1 OR REL.RDB$SYSTEM_FLAG IS NULL) AND '+
    '(NOT REL.RDB$FLAGS IS NULL) AND '+
    '(NOT REL.RDB$VIEW_BLR IS NULL) AND '+
    '(REL.RDB$SECURITY_CLASS STARTING WITH ''SQL$'') '+
    'ORDER BY REL.RDB$RELATION_NAME';

  QRYDomains =
    'select RDB$FIELD_TYPE, RDB$FIELD_SCALE, RDB$FIELD_LENGTH, '+
    'RDB$FIELD_PRECISION, RDB$CHARACTER_SET_ID, RDB$FIELD_SUB_TYPE, '+
    'RDB$FIELD_NAME, RDB$SEGMENT_LENGTH, RDB$NULL_FLAG, RDB$DEFAULT_SOURCE ' +
    'from RDB$FIELDS where not (RDB$FIELD_NAME starting with ''RDB$'')';

  QRYProcedures =
    'SELECT RDB$PROCEDURE_NAME, RDB$PROCEDURE_SOURCE FROM  RDB$PROCEDURES ORDER BY RDB$PROCEDURE_NAME';

  QRYProcFields =
    'SELECT FS.RDB$FIELD_TYPE, FS.RDB$FIELD_SCALE, FS.RDB$FIELD_LENGTH, FS.RDB$FIELD_PRECISION, '+
    'FS.RDB$CHARACTER_SET_ID, FS.RDB$FIELD_SUB_TYPE, PP.RDB$PARAMETER_NAME, FS.RDB$SEGMENT_LENGTH '+
    'FROM RDB$PROCEDURES PR LEFT JOIN RDB$PROCEDURE_PARAMETERS PP '+
    'ON PP.RDB$PROCEDURE_NAME = PR.RDB$PROCEDURE_NAME LEFT JOIN RDB$FIELDS FS ON '+
    'FS.RDB$FIELD_NAME = PP.RDB$FIELD_SOURCE LEFT JOIN RDB$CHARACTER_SETS CR ON '+
    'FS.RDB$CHARACTER_SET_ID = CR.RDB$CHARACTER_SET_ID LEFT JOIN RDB$COLLATIONS CO '+
    'ON ((FS.RDB$COLLATION_ID = CO.RDB$COLLATION_ID) AND (FS.RDB$CHARACTER_SET_ID = '+
    'CO.RDB$CHARACTER_SET_ID)) WHERE (PR.RDB$PROCEDURE_NAME = ?) AND '+
    '(PP.RDB$PARAMETER_TYPE = ?) ORDER BY PP.RDB$PARAMETER_TYPE, PP.RDB$PARAMETER_NUMBER';

  QRYExceptions =
    'SELECT RDB$EXCEPTION_NAME, RDB$MESSAGE, RDB$EXCEPTION_NUMBER FROM RDB$EXCEPTIONS ORDER BY RDB$EXCEPTION_NAME';

  QRYUDF =
    'SELECT RDB$FUNCTION_NAME, RDB$MODULE_NAME, RDB$ENTRYPOINT, RDB$RETURN_ARGUMENT '+
    'FROM RDB$FUNCTIONS WHERE (RDB$SYSTEM_FLAG IS NULL) ORDER BY RDB$FUNCTION_NAME';

  QRYUDFFields =
    'SELECT RDB$FIELD_TYPE, RDB$FIELD_SCALE, RDB$FIELD_LENGTH, RDB$FIELD_PRECISION, '+
    'RDB$CHARACTER_SET_ID, RDB$FIELD_SUB_TYPE, RDB$ARGUMENT_POSITION, RDB$MECHANISM '+
    'FROM RDB$FUNCTION_ARGUMENTS WHERE RDB$FUNCTION_NAME = ? '+
    'ORDER BY RDB$ARGUMENT_POSITION';

  QRYRoles =
    'SELECT RDB$ROLE_NAME, RDB$OWNER_NAME FROM RDB$ROLES';

type
  // forward declarations
  TMetaNode = class;
  TMetaDomain = class;
  TMetaTable = class;

  TMetaNodeClass = class of TMetaNode;

  TNodeItem = record
    Childs: TList;
    ClassID: TMetaNodeClass;
  end;

  TMetaNode = class
  private
    FName: string;
    FOwner: TMetaNode;
    FNodeItems: array of TNodeItem;
    FNodeItemsCount: Integer;
    function GetItems(const ClassIndex, Index: Integer): TMetaNode;
    function GetAsDDL: string;
    procedure AddClass(ClassID: TMetaNodeClass);
    procedure CheckTransaction(Transaction: TJvUIBTransaction);
    procedure SaveNode(Stream: TStringStream; OID: Integer; separator: string = BreakLine);
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
    FNotNull: boolean;
    FDomain: Integer;
    FInfos: TTableFieldInfos;
    procedure LoadFromQuery(Q, C: TJvUIBStatement); override;
    procedure LoadFromStream(Stream: TStream); override;
    function GetDomain: TMetaDomain;
  public
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property DefaultValue: string read FDefaultValue;
    property NotNull: boolean read FNotNull;
    property Domain: TMetaDomain read GetDomain;
    property FieldInfos: TTableFieldInfos read FInfos;
  end;

  TMetaDomain = class(TMetaTableField)
  protected
    property Domain; // hidden
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
    property Fields[const index: Word]: TMetaTableField read GetFields;
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
    function GetForFields(const index: Word): TMetaTableField;
    function GetForFieldsCount: Word;
    function GetForTable: TMetaTable;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    property ForTable: TMetaTable read GetForTable;
    property ForFields[const index: Word]: TMetaTableField read GetForFields;
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
    FUnique: boolean;
    FActive: boolean;
    FOrder: TIndexOrder;
    procedure LoadFromStream(Stream: TStream); override;
  public
    class function NodeClass: string; override;
    class function NodeType: TMetaNodeType; override;
    procedure SaveToDDLNode(Stream: TStringStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property Unique: boolean read FUnique;
    property Active: boolean read FActive;
    property Order: TIndexOrder read FOrder;
  end;

  TMetaTrigger = class(TMetaNode)
  private
    FPrefix: TTriggerPrefix;
    FSuffix: TTriggerSuffixes;
    FPosition: Smallint;
    FActive: boolean;
    FSource: string;
    class function DecodePrefix(value: Integer): TTriggerPrefix;
    class function DecodeSuffixes(value: Integer): TTriggerSuffixes;
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
    property Active: boolean read FActive;
    property Source: string read FSource;
  end;

  TMetaTable = class(TMetaNode)
  private
    function GetFields(const Index: Integer): TMetaTableField;
    function GetFieldsCount: Integer;
    procedure LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
      QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement);
    function FindFieldIndex(const name: String): Integer;
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
    function FindFieldName(const name: String): TMetaTableField;
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
    procedure LoadFromDataBase(QName, QFields, QTriggers, QCharset: TJvUIBStatement);
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
    procedure LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement);
    function GetInputFields(const Index: Integer): TMetaProcInField;
    function GetInputFieldsCount: Integer;
    function GetOutputFields(const Index: Integer): TMetaProcOutField;
    function GetOutputFieldsCount: Integer;
    procedure LoadFromStream(Stream: TStream); override;
    procedure InternalSaveToDDL(Stream: TStringStream; operation: string);
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
    procedure LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement);
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
    constructor Create(AOwner: TMetaNode; ClassIndex: Integer); override;
    procedure LoadFromDatabase(Transaction: TJvUIBTransaction);
    procedure SaveToDDL(Stream: TStringStream); override;

    property Generators[const Index: Integer]: TMetaGenerator read GetGenerators;
    property GeneratorsCount: Integer read GetGeneratorsCount;

    property Tables[const Index: Integer]: TMetaTable read GetTables;
    property TablesCount: Integer read GetTablesCount;

    property Views[const Index: Integer]: TMetaView read GetViews;
    property ViewsCount: Integer read GetViewsCount;

    property Domains[const Index: Integer]: TMetaDomain read GetDomains;
    property DomainsCount: Integer read GetDomainsCount;

    property Procedures[const Index: Integer]: TMetaProcedure read GetProcedures;
    property ProceduresCount: Integer read GetProceduresCount;

    property Exceptions[const Index: Integer]: TMetaException read GetExceptions;
    property ExceptionsCount: Integer read GetExceptionsCount;

    property UDFS[const Index: Integer]: TMetaUDF read GetUDFS;
    property UDFSCount: Integer read GetUDFSCount;

    property Roles[const Index: Integer]: TMetaRole read GetRoles;
    property RolesCount: Integer read GetRolesCount;
  end;

implementation

const
  // Database Tree
  OIDDomains   = 0;
  OIDTable     = 1;
    OIDTableFields   = 0;
    OIDPrimary       = 1;
    OIDForeign       = 2;
    OIDTableTrigger  = 3;
    OIDUnique        = 4;
    OIDIndex         = 5;
    OIDCheck         = 6;
  OIDView      = 2;
    OIDViewFields    = 0;
    OIDViewTrigers   = 1;
  OIDProcedure = 3;
    OIDProcFieldIn   = 0;
    OIDProcFieldOut  = 1;
  OIDGenerator = 4;
  OIDException = 5;
  OIDUDF       = 6;
    OIDUDFField      = 0;
  OIDRole      = 7; 

procedure WriteString(Stream: TStream; var Str: String);
var len: Integer;
begin
  len := Length(Str);
  Stream.Write(len, SizeOf(len));
  if (len > 0) then
    Stream.Write(PChar(Str)^, len);
end;

procedure ReadString(Stream: TStream; var Str: String);
var len: Integer;
begin
  Stream.Read(len, SizeOf(Len));
  SetLength(Str, len);
  if (len > 0) then
    Stream.Read(PChar(Str)^, len);
end;

{ TMetaNode }

constructor TMetaNode.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  FNodeItemsCount := 0;
  FOwner := AOwner;
  if (FOwner <> nil) and (ClassIndex >= 0) then
    FOwner.FNodeItems[ClassIndex].Childs.Add(Self)
end;

destructor TMetaNode.Destroy;
var i, J: Integer;
begin
  for i := 0 to FNodeItemsCount - 1 do
  begin
    for j := 0 to FNodeItems[i].Childs.Count - 1 do
      TObJect(FNodeItems[i].Childs[j]).free;
    FNodeItems[i].Childs.Free;
  end;
  inherited;
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
var FChilds: TList;
begin
  FChilds := FNodeItems[ClassIndex].Childs;
  if (FChilds.Count > 0) and (Index >= 0)
    and (Index < FChilds.Count) then
  Result := TMetaNode(FChilds.Items[Index]) else
  raise EUIBError.CreateFmt(EUIB_INDEXERROR, [Index]);
end;

procedure TMetaNode.SaveToStream(Stream: TStream);
var i, j: Integer;
begin
  for j := 0 to FNodeItemsCount - 1 do
  begin
    i := FNodeItems[j].Childs.Count;
    Stream.Write(i, SizeOf(i));
    for i := 0 to i - 1 do
      TMetaNode(FNodeItems[j].Childs.Items[i]).SaveToStream(Stream);
  end;
end;

constructor TMetaNode.CreateFromStream(AOwner: TMetaNode; ClassIndex: Integer; Stream: TStream);
var i, j: Integer;
begin
  Create(AOwner, ClassIndex);
  LoadFromStream(Stream);
  for j := 0 to FNodeItemsCount - 1 do
  begin
    Stream.Read(i, SizeOf(i));
    for i := 0 to i - 1 do
      FNodeItems[j].ClassID.CreateFromStream(Self, j, Stream);
  end;
end;

procedure TMetaNode.AddClass(ClassID: TMetaNodeClass);
begin
  setLength(FNodeItems, FNodeItemsCount + 1);
  FNodeItems[FNodeItemsCount].Childs := TList.Create;
  FNodeItems[FNodeItemsCount].ClassID := ClassID;
  inc(FNodeItemsCount);
end;

procedure TMetaNode.CheckTransaction(Transaction: TJvUIBTransaction);
begin
  Assert(Transaction <> nil);
  Assert(Transaction.DataBase <> nil);
end;

procedure TMetaNode.SaveNode(Stream: TStringStream; OID: Integer;
separator: string);
var i: Integer;
begin
  for i := 0 to  FNodeItems[OID].Childs.Count - 1 do
  begin
    if i = 0 then
      Stream.WriteString(NewLine) else
      Stream.WriteString(separator);
    TMetaNode(FNodeItems[OID].Childs[i]).SaveToDDL(Stream);
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
  result := MetaNode;
end;

{ TMetaGenerator }

procedure TMetaGenerator.LoadFromDataBase(Transaction: TJvUIBTransaction;
  const Name: string);
var Query: TJvUIBStatement;
begin
  CheckTransaction(Transaction);
  Query := TJvUIBStatement.Create(nil);
  Query.Transaction := Transaction;
  Query.CachedFetch := False;
  try
    FName := Name;
    Query.SQL.Text := format('select gen_id(%s, 0) from rdb$database', [FName]);
    Query.Open;
    if not Query.Eof then
      FValue := Query.Fields.AsInteger[0] else
      raise EUIBError.CreateFmt('Generator %s not found', [FName]);
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

{ TMetaTable }

constructor TMetaTable.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited;
  AddClass(TMetaTableField);
  AddClass(TMetaPrimary);
  AddClass(TMetaForeign);
  AddClass(TMetaTrigger);
  AddClass(TMetaUnique);
  AddClass(TMetaIndex);
  AddClass(TMetaCheck);
end;

function TMetaTable.FindFieldName(const name: String): TMetaTableField;
var i: integer;
begin
  for i := 0 to FieldsCount - 1 do
    if Fields[i].FName = name then
    begin
      Result := Fields[i];
      Exit;
    end;
  raise EUIBError.CreateFmt('Field not found', [name]);
end;

function TMetaTable.GetFields(const Index: Integer): TMetaTableField;
begin
  Result := TMetaTableField(GetItems(OIDTableFields, Index))
end;

function TMetaTable.GetFieldsCount: Integer;
begin
  Result := FNodeItems[OIDTableFields].Childs.Count;
end;

function TMetaTable.GetPrimary(const Index: Integer): TMetaPrimary;
begin
  Result := TMetaPrimary(GetItems(OIDPrimary, Index))
end;

function TMetaTable.GetPrimaryCount: Integer;
begin
  Result := FNodeItems[OIDPrimary].Childs.Count;
end;

function TMetaTable.GetUniques(const Index: Integer): TMetaUnique;
begin
  Result := TMetaUnique(GetItems(OIDUnique, Index))
end;

function TMetaTable.GetUniquesCount: Integer;
begin
  Result := FNodeItems[OIDUnique].Childs.Count;
end;

procedure TMetaTable.LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
  QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement);
var
  unk: string;
begin
  // Fields
  FName := Trim(QNames.Fields.AsString[0]);
  QFields.Params.AsString[0] := FName;
  QFields.Open;
  while not QFields.Eof do
  begin
    with TMetaTableField.Create(Self, OIDTableFields) do
      LoadFromQuery(QFields, QCharset);
    QFields.Next;
  end;

  QPrimary.Params.AsString[1] := FName;

  // Primary
  QPrimary.Params.AsString[0] := 'PRIMARY KEY';
  QPrimary.Open;
  if not QPrimary.Eof then
    TMetaPrimary.Create(Self, OIDPrimary).LoadFromQuery(QPrimary);

  // UNIQUE
  QPrimary.Params.AsString[0] := 'UNIQUE';
  QPrimary.Open;
  while not QPrimary.Eof do
  begin
    if unk <> trim(QPrimary.Fields.AsString[0]) then
      with TMetaUnique.Create(Self, OIDUnique) do
      begin
        SetLength(FFields, 1);
        FName := trim(QPrimary.Fields.AsString[0]);
        FFields[0] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));
        unk := FName;
      end else
      with Uniques[UniquesCount-1] do
      begin
        SetLength(FFields, FieldsCount + 1);
        FFields[FieldsCount - 1] := FindFieldIndex(Trim(QPrimary.Fields.AsString[1]));
        include(Fields[FieldsCount - 1].FInfos, fUnique);
      end;
    QPrimary.Next;
  end;

  // INDICES
  unk := '';
  QIndex.Params.AsString[0] := FName;
  QIndex.Open;
  while not QIndex.Eof do
  begin
    if unk <> trim(QIndex.Fields.AsString[0]) then
      with TMetaIndex.Create(Self, OIDIndex) do
      begin
        SetLength(FFields, 1);
        FName := trim(QIndex.Fields.AsString[0]);
        FFields[0] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
        FUnique := QIndex.Fields.AsSingle[2] = 1;
        FActive := QIndex.Fields.AsSingle[3] = 0;
        if QIndex.Fields.AsSingle[4] = 0 then
          FOrder := IoAscending else
          FOrder := IoDescending;
        unk := FName;
      end else
      with Indices[IndicesCount-1] do
      begin
        SetLength(FFields, FieldsCount + 1);
        FFields[FieldsCount - 1] := FindFieldIndex(Trim(QIndex.Fields.AsString[1]));
        include(Fields[FieldsCount - 1].FInfos, fIndice);
      end;
    QIndex.Next;
  end;

  // Check
  QCheck.Params.AsString[0] := FName;
  QCheck.Open;
  while not QCheck.Eof do
    with TMetaCheck.Create(Self, OIDCheck) do
    begin
      FName := Trim(QCheck.Fields.AsString[0]);
      QCheck.ReadBlob(1, FConstraint);
      QCheck.Next;
    end;

  // TRIGGER
  QTrigger.Params.AsString[0] := FName;
  QTrigger.Open;
  while not QTrigger.Eof do
  begin
    TMetaTrigger.Create(Self, OIDTableTrigger).LoadFromQuery(QTrigger);
    QTrigger.Next;
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
  inherited;
  SaveNode(Stream, OIDPrimary);
  SaveNode(Stream, OIDUnique);
  SaveNode(Stream, OIDIndex);
  SaveNode(Stream, OIDForeign);
  SaveNode(Stream, OIDCheck);
  SaveNode(Stream, OIDTableTrigger, NewLine);
end;

function TMetaTable.GetIndices(const Index: Integer): TMetaIndex;
begin
  Result := TMetaIndex(GetItems(OIDIndex, Index))
end;

function TMetaTable.GetIndicesCount: Integer;
begin
  Result := FNodeItems[OIDIndex].Childs.Count;
end;

function TMetaTable.GetForeign(const Index: Integer): TMetaForeign;
begin
  Result := TMetaForeign(GetItems(OIDForeign, Index))
end;

function TMetaTable.GetForeignCount: Integer;
begin
  Result := FNodeItems[OIDForeign].Childs.Count;
end;

function TMetaTable.FindFieldIndex(const name: String): Integer;
begin
  for Result := 0 to FieldsCount - 1 do
    if Fields[Result].FName = name then
      Exit;
  raise EUIBError.CreateFmt('Field %s not found', [name]);
end;

function TMetaTable.GetChecks(const Index: Integer): TMetaCheck;
begin
  Result := TMetaCheck(GetItems(OIDCheck, Index));
end;

function TMetaTable.GetChecksCount: Integer;
begin
  Result := FNodeItems[OIDCheck].Childs.Count;
end;

function TMetaTable.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(OIDTableTrigger, Index));
end;

function TMetaTable.GetTriggersCount: Integer;
begin
  Result := FNodeItems[OIDTableTrigger].Childs.Count;
end;

procedure TMetaTable.SaveToDDLNode(Stream: TStringStream);
var i: Integer;
begin
  Stream.WriteString(Format('CREATE TABLE %s (', [FName]));
  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(BreakLine + '   ');
    Fields[i].SaveToDDL(Stream);
    if (i <> FieldsCount -1) then
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

{ TMetaBaseField }

procedure TMetaBaseField.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  Stream.Read(FFieldType, SizeOf(FFieldType));

  if (FFieldType = uftNumeric) then
  begin
    Stream.Read(FScale, SizeOf(FScale));
    Stream.Read(FPrecision, SizeOf(FPrecision));
  end else
  begin
    FScale := 0;
    FPrecision := 0;
  end;

  if (FFieldType in [uftChar..uftCstring]) then
  begin
    Stream.Read(FLength, SizeOf(FLength));
    ReadString(Stream, FCharSet);
    Stream.Read(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
  end else
  begin
    FLength := 0;
    FCharSet := '';
  end;

  if (FFieldType = uftBlob) then
  begin
    Stream.Read(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Read(FSubType, SizeOf(FSubType));
  end else
  begin
    FSegmentLength := 0;
    FSubType := 0;
  end;
end;

procedure TMetaBaseField.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  Stream.Write(FFieldType, SizeOf(FFieldType));

  if (FFieldType = uftNumeric) then
  begin
    Stream.Write(FScale, SizeOf(FScale));
    Stream.Write(FPrecision, SizeOf(FPrecision));
  end;

  if (FFieldType in [uftChar..uftCstring]) then
  begin
    Stream.Write(FLength, SizeOf(FLength));
    WriteString(Stream, FCharSet);
    Stream.Write(FBytesPerCharacter, SizeOf(FBytesPerCharacter));
  end;

  if (FFieldType = uftBlob) then
  begin
    Stream.Write(FSegmentLength, SizeOf(FSegmentLength));
    Stream.Write(FSubType, SizeOf(FSubType));
  end;
end;

procedure TMetaBaseField.LoadFromQuery(QField, QCharset: TJvUIBStatement);
  procedure FindCharset(const id: single; var Charset: string; var count: Smallint);
  var i: Integer;
  begin
    for i := 0 to QCharset.Fields.RecordCount - 1 do
    begin
      QCharset.Fields.GetRecord(i);
      if (QCharset.Fields.AsSmallint[0] = id) then
      begin
        Charset := Trim(QCharset.Fields.AsString[1]);
        count := QCharset.Fields.AsSmallint[2];
        Exit;
      end;
      Charset := '';
      FBytesPerCharacter := 1;
    end;
  end;
begin
  FScale     := abs(QField.Fields.AsSmallInt[1]);
  FLength    := QField.Fields.AsSmallInt[2];
  FPrecision := QField.Fields.AsSmallInt[3];
  if FScale > 0 then
  begin
    FFieldType := uftNumeric;
    if FPrecision = 0 then
    case QField.Fields.AsSmallint[0] of
      blr_short: FPrecision := 4;
      blr_long : FPrecision := 7;
      blr_int64, blr_quad, blr_double: FPrecision := 15;
    else
      Raise EUIBError.Create(EUIB_UNEXPECTEDERROR);
    end;
  end else
    case QField.Fields.AsSmallint[0] of
      blr_text,
      blr_text2     : FFieldType := uftChar;
      blr_varying,
      blr_varying2  : FFieldType := uftVarchar;
      blr_cstring,
      blr_cstring2  : FFieldType := uftCstring;
      blr_short     : FFieldType := uftSmallint;
      blr_long      : FFieldType := uftInteger;
      blr_quad      : FFieldType := uftQuad;
      blr_float,
      blr_d_float   : FFieldType := uftFloat;
      blr_double    : FFieldType := uftDoublePrecision;
      blr_timestamp : FFieldType := uftTimestamp;
      blr_blob      : FFieldType := uftBlob;
      blr_blob_id   : FFieldType := uftBlobId;
      blr_sql_date  : FFieldType := uftDate;
      blr_sql_time  : FFieldType := uftTime;
      blr_int64     : FFieldType := uftInt64;
  {$IFDEF IB7_UP}
      blr_boolean_dtype: FFieldType := uftBoolean;
  {$ENDIF}
    end;
  If (FFieldType in [uftChar, uftVarchar, uftCstring]) and
    not QField.Fields.IsNull[4] then
      FindCharset(QField.Fields.AsSmallint[4], FCharSet, FBytesPerCharacter) else
      FBytesPerCharacter := 1;

  FSubType       := QField.Fields.AsSmallint[5];
end;

procedure TMetaBaseField.SaveToDDLNode(Stream: TStringStream);
begin
  case FFieldType of
    uftNumeric : Stream.WriteString(format('%s(%d,%d)',
      [FieldTypes[FFieldType], FPrecision, FScale]));
    uftChar..uftCstring:
      begin
        Stream.WriteString(format('%s(%d)',
          [FieldTypes[FFieldType], FLength div FBytesPerCharacter]));
        if (FCharSet <> '') then
          Stream.WriteString(' CHARACTER SET ' + FCharSet);
      end;
    uftBlob: Stream.WriteString(format('%s SUB_TYPE %d SEGMENT SIZE %d',
      [FieldTypes[FFieldType], FSubType, FSegmentLength]));
  else
    Stream.WriteString(format('%s', [FieldTypes[FFieldType]]));
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
      Result := format('%s(%d)', [FieldTypes[FFieldType],
        FLength div FBytesPerCharacter]);
    uftNumeric : Result := format('%s(%d,%d)',
      [FieldTypes[FFieldType], FPrecision, FScale]);
  else
    Result := format('%s', [FieldTypes[FFieldType]]);
  end;
end;

class function TMetaBaseField.NodeType: TMetaNodeType;
begin
  Result := MetaBaseField;
end;

{ TMetaDataBase }

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
end;

procedure TMetaDataBase.LoadFromDatabase(Transaction: TJvUIBTransaction);
var
  i: Integer;
  constr, cForField, str: string;

  QNames, QFields, QCharset, QPrimary,
  QIndex, QForeign, QCheck, QTrigger: TJvUIBStatement;

  procedure Configure(var Q: TJvUIBStatement; const QRY: string;
    CachedFetch: boolean = False);
  begin
    Q := TJvUIBStatement.Create(nil);
    Q.Transaction := Transaction;
    Q.CachedFetch := CachedFetch;
    Q.SQL.Text := QRY;
  end;

begin
  CheckTransaction(Transaction);

  FName := Transaction.DataBase.DatabaseName;


  Configure(QNames, QRYTables);
  Configure(QCharset, QRYCharset, True);
  Configure(QFields, QRYTableFields);
  Configure(QPrimary, QRYUnique, True);
  Configure(QIndex, QRYIndex);
  Configure(QForeign, QRYForeign);
  Configure(QCheck, QRYCheck);
  Configure(QTrigger, QRYTrigger);
  try
    // preload Charsets
    QCharset.Open;
    QCharset.FetchAll;

    // DOMAINS
    FNodeItems[OIDDomains].Childs.Clear;
    QNames.SQL.Text := QRYDomains;
    QNames.Open;
    while not QNames.Eof do
    begin
      with TMetaDomain.Create(Self, OIDDomains) do
        LoadFromQuery(QNames, QCharset);
      QNames.Next;
    end;

    // GENERATORS
    FNodeItems[OIDGenerator].Childs.Clear;
    QNames.SQL.Text := QRYGenerators;
    QNames.Open;
    while not QNames.Eof do
    begin
      with TMetaGenerator.Create(Self, OIDGenerator) do
        LoadFromDataBase(Transaction, Trim(QNames.Fields.AsString[0]));
      QNames.Next;
    end;

    // TABLES
    FNodeItems[OIDTable].Childs.Clear;
    QNames.SQL.Text := QRYTables;
    QNames.Open;
    while not QNames.Eof do
    begin
      with TMetaTable.Create(Self, OIDTable) do
        LoadFromDataBase(QNames, QFields, QCharset, QPrimary,
           QIndex, QForeign, QCheck, QTrigger);
      QNames.Next;
    end;

    // FOREIGN
    for i := 0 to TablesCount - 1 do
    begin
      QForeign.Params.AsString[0] := Tables[i].Name;
      QForeign.Open;
      constr := '';
      cForField := '';
      while not QForeign.Eof do
      begin
        if (constr <> Trim(QForeign.Fields.AsString[0])) then // new
        begin
          with TMetaForeign.Create(Tables[i], OIDForeign) do
          begin
            FName := Trim(QForeign.Fields.AsString[0]);
            constr := FName;
            FForTable := FindTableIndex(Trim(QForeign.Fields.AsString[3]));
            if 'TABLE1' = Trim(QForeign.Fields.AsString[3]) then
              beep;
            SetLength(FFields, 1);
            FFields[0] := Tables[i].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
            include(Tables[i].Fields[FFields[0]].FInfos, fForeign);
            SetLength(FForFields, 1);
            FForFields[0] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));
            cForField := ForFields[0].Name;

            str := Trim(QForeign.Fields.AsString[1]);
            if str = 'RESTRICT' then FOnUpdate := Restrict else
            if str = 'CASCADE'  then FOnUpdate := Cascade  else
            if str = 'SET NULL' then FOnUpdate := SetNull  else
              FOnUpdate := SetDefault;

            str := Trim(QForeign.Fields.AsString[2]);
            if str = 'RESTRICT' then FOnDelete := Restrict else
            if str = 'CASCADE'  then FOnDelete := Cascade  else
            if str = 'SET NULL' then FOnDelete := SetNull  else
              FOnDelete := SetDefault;

          end;
        end else
        with Tables[i].Foreign[Tables[i].ForeignCount - 1] do
        begin
          SetLength(FFields, Length(FFields)+1);
          FFields[FieldsCount-1] := Tables[i].FindFieldIndex(Trim(QForeign.Fields.AsString[5]));
          include(Tables[i].Fields[FFields[FieldsCount-1]].FInfos, fForeign);
          SetLength(FForFields, Length(FForFields)+1);
          FForFields[ForFieldsCount-1] := ForTable.FindFieldIndex(Trim(QForeign.Fields.AsString[4]));
        end;
        QForeign.Next;
      end;
    end;

    // VIEWS
    FNodeItems[OIDView].Childs.Clear;
    QNames.SQL.Text := QRYView;
    QNames.Open;
    while not QNames.Eof do
    begin
      with TMetaView.Create(Self, OIDView) do
        LoadFromDataBase(QNames, QFields, QTrigger, QCharset);
      QNames.Next;
    end;

    // PROCEDURE
    FNodeItems[OIDProcedure].Childs.Clear;
    QNames.SQL.Text := QRYProcedures;
    QFields.SQL.Text := QRYProcFields;
    QNames.Open;
    while not QNames.Eof do
    begin
      with TMetaProcedure.Create(Self, OIDProcedure) do
        LoadFromQuery(QNames, QFields, QCharset);
      QNames.Next;
    end;

    // EXCEPTION
    FNodeItems[OIDException].Childs.Clear;
    QNames.SQL.Text := QRYExceptions;
    QNames.Open;
    while not QNames.Eof do
    begin
      TMetaException.Create(Self, OIDException).LoadFromQuery(QNames);
      QNames.Next;
    end;

    // UDF
    FNodeItems[OIDUDF].Childs.Clear;
    QNames.SQL.Text := QRYUDF;
    QFields.SQL.Text := QRYUDFFields;
    QNames.Open;
    while not QNames.Eof do
    begin
      TMetaUDF.Create(Self, OIDUDF).LoadFromQuery(QNames, QFields, QCharset);
      QNames.Next;
    end;

    // ROLES
    FNodeItems[OIDRole].Childs.Clear;
    QNames.SQL.Text := QRYRoles;
    QNames.Open;
    while not QNames.Eof do
    begin
      TMetaRole.Create(Self, OIDRole).LoadFromQuery(QNames);
      QNames.Next;
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
var i: Integer;
  procedure SaveChildNodes(comment: string; OIDParent, OIDChild: Integer;
    Separator: string = BreakLine);
  var i, j: Integer;
  begin
    if TablesCount > 0 then
    begin
      Stream.WriteString(NewLine);
      Stream.WriteString(format('/* %s */', [comment]));
      Stream.WriteString(BreakLine);
      for i := 0 to FNodeItems[OIDParent].Childs.Count - 1 do
        for j := 0 to GetItems(OIDParent, i).FNodeItems[OIDChild].Childs.Count - 1 do
        begin
          Stream.WriteString(Separator);
          TMetaNode(GetItems(OIDParent, i).FNodeItems[OIDChild].Childs[j]).SaveToDDL(Stream);
        end;
    end;
  end;

  procedure SaveMainNodes(Comment: string; OID: Integer;
    Separator: string = NewLine);
  var i: Integer;
  begin
    if FNodeItems[OID].Childs.Count > 0 then
    begin
      Stream.WriteString(NewLine);
      Stream.WriteString(format('/* %s */', [comment]));
      for i := 0 to FNodeItems[OID].Childs.Count - 1 do
      begin
        if i = 0 then
          Stream.WriteString(NewLine) else
          Stream.WriteString(Separator);
        if GetItems(OID, i) is TMetaProcedure then
        TMetaProcedure(GetItems(OID, i)).SaveToPostDDL(Stream) else
        GetItems(OID, i).SaveToDDLNode(Stream);
      end;
    end;
  end;
begin
  SaveMainNodes('ROLES', OIDRole, NewLine);
  SaveMainNodes('FUNCTIONS', OIDUDF, NewLine);
  SaveMainNodes('DOMAINS', OIDDomains, BreakLine);
  SaveMainNodes('GENERATORS', OIDGenerator);
  SaveMainNodes('EXEPTIONS', OIDException, BreakLine);
  SaveMainNodes('PROCEDURES', OIDProcedure);
  SaveMainNodes('TABLES', OIDTable);
  SaveMainNodes('VIEWS', OIDView);

  SaveChildNodes('UNIQUE',  OIDTable, OIDUnique);
  SaveChildNodes('PRIMARY', OIDTable, OIDPrimary);
  SaveChildNodes('FOREIGN', OIDTable, OIDForeign);
  SaveChildNodes('INDICES', OIDTable, OIDIndex);
  SaveChildNodes('CHECKS',  OIDTable, OIDCheck, NewLine);
  SaveChildNodes('TRIGGERS', OIDTable, OIDTableTrigger, NewLine);
  SaveChildNodes('TRIGGERS (Views)', OIDView, OIDViewTrigers, NewLine);

  if ProceduresCount > 0 then
  begin
    Stream.WriteString(NewLine);
    Stream.WriteString('/* PROCEDURES */');
    for i := 0 to ProceduresCount - 1 do
    begin
      Stream.WriteString(NewLine);
      Procedures[i].SaveToAlterDDL(Stream);
    end;
  end;

end;

function TMetaDataBase.GetGenerators(const Index: Integer): TMetaGenerator;
begin
  Result := TMetaGenerator(GetItems(OIDGenerator, Index));
end;

function TMetaDataBase.GetGeneratorsCount: Integer;
begin
  Result := FNodeItems[OIDGenerator].Childs.Count
end;

function TMetaDataBase.GetTables(const Index: Integer): TMetaTable;
begin
  Result := TMetaTable(GetItems(OIDTable, Index));
end;

function TMetaDataBase.GetTablesCount: Integer;
begin
  Result := FNodeItems[OIDTable].Childs.Count
end;

function TMetaDataBase.FindTableName(const TableName: string): TMetaTable;
var i: Integer;
begin
  for i := 0 to TablesCount - 1 do
    if Tables[i].Name = TableName then
    begin
      Result := Tables[i];
      Exit;
    end;
  raise Exception.CreateFmt('Table %s not found', [TableName]);
end;

function TMetaDataBase.FindTableIndex(const TableName: string): Integer;
begin
  for Result := 0 to TablesCount - 1 do
    if Tables[Result].Name = TableName then
      Exit;
  raise Exception.CreateFmt('Table %s not found', [TableName]);
end;

function TMetaDataBase.FindDomainIndex(const DomainName: string): Integer;
begin
  for Result := 0 to DomainsCount - 1 do
    if Domains[Result].Name = DomainName then
      Exit;
  raise Exception.CreateFmt('Domain %s not found', [DomainName]);
end;

procedure TMetaDataBase.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
end;

function TMetaDataBase.GetViews(const Index: Integer): TMetaView;
begin
  Result := TMetaView(GetItems(OIDView, Index));
end;

function TMetaDataBase.GetViewsCount: Integer;
begin
  Result := FNodeItems[OIDView].Childs.Count
end;

function TMetaDataBase.GetDomains(const Index: Integer): TMetaDomain;
begin
  Result := TMetaDomain(GetItems(OIDDomains, Index));
end;

function TMetaDataBase.GetDomainsCount: Integer;
begin
  Result := FNodeItems[OIDDomains].Childs.Count
end;

function TMetaDataBase.GetProcedures(const Index: Integer): TMetaProcedure;
begin
  Result := TMetaProcedure(GetItems(OIDProcedure, Index));
end;

function TMetaDataBase.GetProceduresCount: Integer;
begin
  Result := FNodeItems[OIDProcedure].Childs.Count
end;

function TMetaDataBase.GetExceptions(const Index: Integer): TMetaException;
begin
  Result := TMetaException(GetItems(OIDException, Index));
end;

function TMetaDataBase.GetExceptionsCount: Integer;
begin
  Result := FNodeItems[OIDException].Childs.Count
end;

function TMetaDataBase.GetUDFS(const Index: Integer): TMetaUDF;
begin
  Result := TMetaUDF(GetItems(OIDUDF, Index));
end;

function TMetaDataBase.GetUDFSCount: Integer;
begin
  Result := FNodeItems[OIDUDF].Childs.Count
end;

class function TMetaDataBase.NodeClass: string;
begin
  Result := 'Database';
end;

procedure TMetaDataBase.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  inherited;
end;

function TMetaDataBase.GetRoles(const Index: Integer): TMetaRole;
begin
  Result := TMetaRole(GetItems(OIDRole, Index));
end;

function TMetaDataBase.GetRolesCount: Integer;
begin
  Result := FNodeItems[OIDRole].Childs.Count
end;

class function TMetaDataBase.NodeType: TMetaNodeType;
begin
  Result := MetaDatabase;
end;

{ TMetaConstraint }

function TMetaConstraint.GetFields(const index: Word): TMetaTableField;
begin
  Assert((FieldsCount > 0) and (Index < FieldsCount), inttostr(index) + ' ' + ClassName);
  result := TMetaTable(FOwner).Fields[FFields[index]];
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
var i: Integer;
begin
  Stream.Read(i, SizeOf(i));
  SetLength(FFields, i);
  if i > 0 then
  begin
    ReadString(Stream, FName);
    for i := 0 to i - 1 do
    begin
      Stream.Read(FFields[i], SizeOf(FFields[i]));
      case NodeType of
        MetaForeign: include(TMetaTable(FOwner).Fields[FFields[i]].FInfos, fForeign);
        MetaIndex  : include(TMetaTable(FOwner).Fields[FFields[i]].FInfos, fIndice);
        MetaPrimary: include(TMetaTable(FOwner).Fields[FFields[i]].FInfos, fPrimary);
        MetaUnique : include(TMetaTable(FOwner).Fields[FFields[i]].FInfos, fPrimary);
      end;
    end;
  end;
end;

procedure TMetaConstraint.SaveToStream(Stream: TStream);
var
  i: Integer;
begin
  i := FieldsCount;
  Stream.Write(i, SizeOf(i));
  if i > 0 then
  begin
    WriteString(Stream, FName);
    for i := 0 to i - 1 do
      Stream.Write(FFields[i], SizeOf(FFields[i]));
  end;
end;

class function TMetaConstraint.NodeType: TMetaNodeType;
begin
  Result := MetaConstraint;
end;

{ TMetaUnique }

class function TMetaUnique.NodeClass: string;
begin
  Result := 'Unique';
end;

class function TMetaUnique.NodeType: TMetaNodeType;
begin
  Result := MetaUnique;
end;

procedure TMetaUnique.SaveToDDL(Stream: TStringStream);
var i: Integer;
begin
  Stream.WriteString(Format('ALTER TABLE %s ADD UNIQUE (',
    [TMetaTable(FOwner).FName]));
  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[i].Name);
    if i <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
end;

{ TMetaPrimary }

class function TMetaPrimary.NodeClass: string;
begin
  Result := 'Primary key';
end;

procedure TMetaPrimary.LoadFromQuery(Q: TJvUIBStatement);
var i: Integer;
begin
  FName := trim(Q.Fields.AsString[0]);
  Q.FetchAll;
  SetLength(FFields, Q.Fields.RecordCount);
  for i := 0 to Q.Fields.RecordCount - 1 do
  begin
    Q.Fields.GetRecord(i);
    FFields[i] := TMetaTable(FOwner).FindFieldIndex(Trim(Q.Fields.AsString[1]));
    include(TMetaTable(FOwner).Fields[FFields[i]].FInfos, fPrimary);
  end;
end;

procedure TMetaPrimary.SaveToDDLNode(Stream: TStringStream);
var i: Integer;
begin
  if copy(FName, 0, 6) = 'INTEG_' then
    Stream.WriteString(Format('ALTER TABLE %s ADD PRIMARY KEY (',
      [TMetaTable(FOwner).FName])) else
    Stream.WriteString(Format('ALTER TABLE %s ADD CONSTRAINT %s PRIMARY KEY (',
      [TMetaTable(FOwner).FName, FName]));
  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[i].Name);
    if i <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
end;


class function TMetaPrimary.NodeType: TMetaNodeType;
begin
  Result := MetaPrimary;
end;

{ TMetaIndex }

class function TMetaIndex.NodeClass: string;
begin
  Result := 'Indice';
end;

procedure TMetaIndex.LoadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(FUnique, SizeOf(FUnique));
  Stream.Read(FActive, SizeOf(FActive));
  Stream.Read(FOrder, SizeOf(FOrder));
end;

procedure TMetaIndex.SaveToDDLNode(Stream: TStringStream);
var
  i: Integer;
  UNIQUE, ORDER: string;
begin
  if FUnique then UNIQUE := ' UNIQUE';
  if (FOrder = IoDescending) then ORDER := ' DESCENDING';

  Stream.WriteString(Format('CREATE%s%s INDEX %s ON %s (',
    [ORDER, UNIQUE, FName, TMetaTable(FOwner).FName]));

  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[i].Name);
    if i <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(');');
  if not FActive then
    Stream.WriteString(Format('%sALTER INDEX %s INACTIVE;', [BreakLine, FName]));
end;

procedure TMetaIndex.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FUnique, SizeOf(FUnique));
  Stream.Write(FActive, SizeOf(FActive));
  Stream.Write(FOrder, SizeOf(FOrder));
end;

class function TMetaIndex.NodeType: TMetaNodeType;
begin
  Result := MetaIndex;
end;

{ TMetaForeign }

function TMetaForeign.GetForFields(const index: Word): TMetaTableField;
begin
  Assert((ForFieldsCount > 0) and (Index < ForFieldsCount));

  result := ForTable.Fields[FForFields[Index]];
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
  Result := 'Foreign'
end;

procedure TMetaForeign.LoadFromStream(Stream: TStream);
var
  i: Integer;
begin
  inherited;
  Stream.Read(FForTable, SizeOf(FForTable));
  Stream.Read(FOnDelete, SizeOf(FOnDelete));
  Stream.Read(FOnUpdate, SizeOf(FOnUpdate));
  Stream.Read(i, SizeOf(i));
  SetLength(FForFields, i);
  for i := 0 to i - 1 do
    Stream.Read(FForFields[i], SizeOf(FForFields[i]));
end;

procedure TMetaForeign.SaveToDDLNode(Stream: TStringStream);
var i: Integer;
begin
  if copy(FName, 0, 6) = 'INTEG_' then
  Stream.WriteString(Format('ALTER TABLE %s ADD FOREIGN KEY (',
    [TMetaTable(FOwner).FName])) else
  Stream.WriteString(Format('ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (',
    [TMetaTable(FOwner).FName, FName]));

  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(Fields[i].Name);
    if i <> FieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(format(') REFERENCES %s (', [ForTable.Name]));
  for i := 0 to ForFieldsCount - 1 do
  begin
    Stream.WriteString(ForFields[i].Name);
    if i <> ForFieldsCount - 1 then
      Stream.WriteString(', ');
  end;
  Stream.WriteString(')');

  case OnDelete of
    Cascade    : Stream.WriteString(' ON DELETE CASCADE');
    SetNull    : Stream.WriteString(' ON DELETE SET NULL');
    SetDefault : Stream.WriteString(' ON DELETE SET DEFAULT');
  end;

  case OnUpdate of
    Cascade    : Stream.WriteString(' ON UPDATE CASCADE');
    SetNull    : Stream.WriteString(' ON UPDATE SET NULL');
    SetDefault : Stream.WriteString(' ON UPDATE SET DEFAULT');
  end;

  Stream.WriteString(';');
end;

procedure TMetaForeign.SaveToStream(Stream: TStream);
var
  i: Integer;
begin
  inherited;
  Stream.Write(FForTable, SizeOf(FForTable));
  Stream.Write(FOnDelete, SizeOf(FOnDelete));
  Stream.Write(FOnUpdate, SizeOf(FOnUpdate));
  i := ForFieldsCount;
  Stream.Write(i, SizeOf(i));
  for i := 0 to i - 1 do
    Stream.Write(FForFields[i], SizeOf(FForFields[i]));
end;

class function TMetaForeign.NodeType: TMetaNodeType;
begin
  Result := MetaForeign;
end;

{ TMetaCheck }

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
  Stream.WriteString(format('ALTER TABLE %s ADD %s;',
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

{ TMetaTrigger }

class function TMetaTrigger.DecodePrefix(value: Integer): TTriggerPrefix;
begin
  Result := TTriggerPrefix((value + 1) and 1);
end;

class function TMetaTrigger.DecodeSuffixes(
  value: Integer): TTriggerSuffixes;
var v, slot: Integer;
begin
  Result := [];
  for slot := 1 to 3 do
  begin
    v := ((value + 1) shr (slot * 2 - 1)) and 3;
    if v > 0 then include(Result, TTriggerSuffix(v-1));
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
  count : Smallint;
  suf: TTriggerSuffix;
begin
  Stream.WriteString(format('CREATE TRIGGER %s FOR %s%s',
    [Name, TMetaNode(FOwner).Name, BreakLine]));
  if FActive then
    Stream.WriteString('ACTIVE ');

  Stream.WriteString(TriggerPrefixTypes[FPrefix] + ' ');
  count := 0;
  for suf := Insert to Delete do
    if suf in FSuffix then
    begin
      inc(count);
      if count > 1 then
        Stream.WriteString(' OR ');
      Stream.WriteString(TriggerSuffixTypes[suf]);
    end;
  Stream.WriteString(format(' POSITION %d%s%s;', [FPosition, BreakLine, FSource]));
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

{ TMetaView }

constructor TMetaView.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited;
  AddClass(TMetaField);
  AddClass(TMetaTrigger);
end;

function TMetaView.GetFields(const Index: Integer): TMetaField;
begin
  Result := TMetaField(GetItems(OIDViewFields, Index))
end;

function TMetaView.GetFieldsCount: Integer;
begin
  Result := FNodeItems[OIDViewFields].Childs.Count;
end;

class function TMetaView.NodeClass: string;
begin
  Result := 'View'
end;

function TMetaView.GetTriggers(const Index: Integer): TMetaTrigger;
begin
  Result := TMetaTrigger(GetItems(OIDViewTrigers, Index))
end;

function TMetaView.GetTriggersCount: Integer;
begin
  Result := FNodeItems[OIDViewTrigers].Childs.Count;
end;

procedure TMetaView.LoadFromDataBase(QName, QFields, QTriggers,
  QCharset: TJvUIBStatement);
begin
  FName := Trim(QName.Fields.AsString[0]);
  QName.ReadBlob(1, FSource);
  FSource := Trim(FSource);
  QFields.Params.AsString[0] := FName;
  QFields.Open;
  while not QFields.Eof do
  begin
    TMetaField.Create(Self, OIDViewFields).LoadFromQuery(QFields, QCharset);
    QFields.Next;
  end;

  // TRIGGER
  QTriggers.Params.AsString[0] := FName;
  QTriggers.Open;
  while not QTriggers.Eof do
  begin
    TMetaTrigger.Create(Self, OIDViewTrigers).LoadFromQuery(QTriggers);
    QTriggers.Next;
  end;

end;

procedure TMetaView.LoadFromStream(Stream: TStream);
begin
  ReadString(Stream, FName);
  ReadString(Stream, FSource);
end;

procedure TMetaView.SaveToDDL(Stream: TStringStream);
begin
  inherited;
  SaveNode(Stream, OIDViewTrigers, NewLine);
end;

procedure TMetaView.SaveToDDLNode(Stream: TStringStream);
var i: Integer;
begin
  Stream.WriteString(Format('CREATE VIEW %s (', [Name]));
  for i := 0 to FieldsCount - 1 do
  begin
    Stream.WriteString(BreakLine + '   ' + Fields[i].Name);
    if (i <> FieldsCount -1) then
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
  inherited;
end;

class function TMetaView.NodeType: TMetaNodeType;
begin
  Result := MetaView;
end;

{ TMetaDomain }

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
  Stream.WriteString(format('CREATE DOMAIN %s AS ', [FName]));
  inherited;
  Stream.WriteString(';');
end;

{ TMetaProcedure }

constructor TMetaProcedure.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited;
  AddClass(TMetaProcInField); // in
  AddClass(TMetaProcOutField); // out
end;

function TMetaProcedure.GetInputFields(const Index: Integer): TMetaProcInField;
begin
  Result := TMetaProcInField(GetItems(OIDProcFieldIn, Index))
end;

function TMetaProcedure.GetInputFieldsCount: Integer;
begin
  Result := FNodeItems[OIDProcFieldIn].Childs.Count;
end;

class function TMetaProcedure.NodeClass: string;
begin
  Result := 'Procedure';
end;

function TMetaProcedure.GetOutputFields(const Index: Integer): TMetaProcOutField;
begin
  Result := TMetaProcOutField(GetItems(OIDProcFieldOut, Index))
end;

function TMetaProcedure.GetOutputFieldsCount: Integer;
begin
  Result := FNodeItems[OIDProcFieldOut].Childs.Count;
end;

procedure TMetaProcedure.InternalSaveToDDL(Stream: TStringStream;
  operation: string);
var i: Integer;
begin
  Stream.WriteString(format('%s PROCEDURE %s', [operation, FName]));
  if InputFieldsCount > 0 then
  begin
    Stream.WriteString(' (');
    for i := 0 to InPutFieldsCount - 1 do
    begin
      Stream.WriteString(BreakLine + '   ');
      InputFields[i].SaveToDDL(Stream);
      if (i <> InputFieldsCount -1) then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;

  if OutputFieldsCount > 0 then
  begin
    Stream.WriteString(format('%sRETURNS (', [BreakLine]));
    for i := 0 to OutputFieldsCount - 1 do
    begin
      Stream.WriteString(BreakLine + '   ');
      OutputFields[i].SaveToDDL(Stream);
      if (i <> OutputFieldsCount -1) then
        Stream.WriteString(',');
    end;
    Stream.WriteString(')');
  end;
end;

procedure TMetaProcedure.LoadFromQuery(QNames, QFields,
  QCharset: TJvUIBStatement);
begin
  FName := Trim(QNames.Fields.AsString[0]);
  QNames.ReadBlob(1, FSource);
  QFields.Params.AsString[0] := FName;

  QFields.Params.AsSmallint[1] := 0; // in
  QFields.Open;
  while not QFields.Eof do
  begin
    TMetaProcInField.Create(Self, OIDProcFieldIn).LoadFromQuery(QFields, QCharset);
    QFields.Next;
  end;

  QFields.Params.AsSmallint[1] := 1; // out
  QFields.Open;
  while not QFields.Eof do
  begin
    TMetaProcOutField.Create(Self, OIDProcFieldOut).LoadFromQuery(QFields, QCharset);
    QFields.Next;
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
    '  EXIT;'+ breakline + 'END;');
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
  inherited;
end;

class function TMetaProcedure.NodeType: TMetaNodeType;
begin
  Result := MetaProcedure;
end;

{ TMetaException }

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
  inherited;
end;

class function TMetaException.NodeType: TMetaNodeType;
begin
  Result := MetaException;
end;

{ TMetaUDF }

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
var i, c: Integer;
begin
  stream.WriteString(format('DECLARE EXTERNAL FUNCTION %s', [Fname]));
  c := 0;

  if FReturn = 0 then
  begin // return position
    for i := 0 to FieldsCount - 1 do
    if Fields[i].Position <> Return then
    begin
      if c > 0 then Stream.WriteString(',');
      Stream.WriteString(BreakLine + '  ');
      Fields[i].SaveToDDL(Stream);
      inc(c);
    end;
    for i := 0 to FieldsCount - 1 do
      if Fields[i].Position = Return then
      begin
        Stream.WriteString(BreakLine + '  RETURNS ');
        Fields[i].SaveToDDL(Stream);
        Break;
      end;
  end else
  begin
    for i := 0 to FieldsCount - 1 do
    begin
      if c > 0 then Stream.WriteString(',');
      Stream.WriteString(BreakLine + '  ');
      Fields[i].SaveToDDL(Stream);
      inc(c);
    end;
    Stream.WriteString(format('%s  RETURNS PARAMETER %d', [BreakLine, Freturn]));
  end;

  stream.WriteString(format('%s  ENTRY_POINT ''%s'' MODULE_NAME ''%s'';', [BreakLine, FEntry, FModule]));
end;

procedure TMetaUDF.SaveToStream(Stream: TStream);
begin
  WriteString(Stream, FName);
  WriteString(Stream, FModule);
  WriteString(Stream, FEntry);
  Stream.Write(FReturn, SizeOf(FReturn));
  inherited;
end;

procedure TMetaUDF.LoadFromQuery(QNames, QFields, QCharset: TJvUIBStatement);
begin
  FName := Trim(QNames.Fields.AsString[0]);
  FModule := QNames.Fields.AsString[1];
  FEntry := Trim(QNames.Fields.AsString[2]);
  FReturn := QNames.Fields.AsSmallint[3];
  QFields.Params.AsString[0] := FName;
  QFields.Open;
  while not QFields.Eof do
  begin
    TMetaUDFField.Create(Self, OIDUDFField).LoadFromQuery(QFields, QCharset);
    QFields.Next;
  end;

end;

constructor TMetaUDF.Create(AOwner: TMetaNode; ClassIndex: Integer);
begin
  inherited;
  AddClass(TMetaUDFField);
end;

function TMetaUDF.GetFields(const Index: Integer): TMetaUDFField;
begin
  Result := TMetaUDFField(GetItems(OIDUDFField, Index))
end;

function TMetaUDF.GetFieldsCount: Integer;
begin
  Result := FNodeItems[OIDUDFField].Childs.Count;
end;

class function TMetaUDF.NodeType: TMetaNodeType;
begin
  Result := MetaUDF;
end;

{ TMetaTableField }

function TMetaTableField.GetDomain: TMetaDomain;
begin
  if (FDomain >= 0) then
    Result := TMetaDatabase(FOwner.FOwner).Domains[FDomain] else
    Result := nil;
end;

procedure TMetaTableField.LoadFromQuery(Q, C: TJvUIBStatement);
begin
  inherited;
  FNotNull := (Q.Fields.AsSmallint[8] = 1);
  if not Q.Fields.IsNull[9] then
  begin
    Q.ReadBlob(9, FDefaultValue);

    FDefaultValue := Trim(FDefaultValue);
    if (FDefaultValue <> '') then
      FDefaultValue := Copy(FDefaultValue, 9,
        System.Length(FDefaultValue) - 8);
  end else
    FDefaultValue := '';

  FDomain := -1;
  if not (self is TMetaDomain) then
    if not (Q.Fields.IsNull[10] or (Copy(Q.Fields.AsString[10],1,4) = 'RDB$')) then
      FDomain :=
        TMetaDataBase(FOwner.FOwner).FindDomainIndex(Trim(Q.Fields.AsString[10]));
end;

procedure TMetaTableField.LoadFromStream(Stream: TStream);
begin
  inherited;
  ReadString(Stream, FDefaultValue);
  Stream.Read(FNotNull, SizeOf(FNotNull));
  Stream.Read(FDomain, SizeOf(FDomain));
end;

class function TMetaTableField.NodeType: TMetaNodeType;
begin
  Result := MetaTableField;
end;

procedure TMetaTableField.SaveToDDLNode(Stream: TStringStream);
begin
  if (FDomain >= 0) then
    Stream.WriteString(Domain.Name) else
    inherited;
  if (FDefaultValue <> '') then
    Stream.WriteString(' DEFAULT ' + FDefaultValue);
  if FNotNull then
    Stream.WriteString(' NOT NULL');
end;

procedure TMetaTableField.SaveToStream(Stream: TStream);
begin
  inherited;
  WriteString(Stream, FDefaultValue);
  Stream.Write(FNotNull, SizeOf(FNotNull));
  Stream.Write(FDomain, SizeOf(FDomain));
end;

{ TMetaProcInField }

class function TMetaProcInField.NodeClass: string;
begin
  Result := 'Input parameter';
end;

class function TMetaProcInField.NodeType: TMetaNodeType;
begin
  Result := MetaProcInField;
end;

{ TMetaProcOutField }

class function TMetaProcOutField.NodeClass: string;
begin
  Result := 'Output parameter';
end;

class function TMetaProcOutField.NodeType: TMetaNodeType;
begin
  Result := MetaProcOutField;
end;

{ TMetaField }

procedure TMetaField.LoadFromQuery(Q, C: TJvUIBStatement);
begin
  inherited;
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
  inherited;
end;

{ TMetaUDFField }

procedure TMetaUDFField.LoadFromQuery(QField, QCharset: TJvUIBStatement);
begin
  inherited;
  FPosition := QField.Fields.AsSmallint[6];
  FMechanism := QField.Fields.AsSmallint[7];
  FName := 'Field ' + IntToStr(FPosition);
end;

procedure TMetaUDFField.LoadFromStream(Stream: TStream);
begin
  inherited;
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
    Stream.WriteString('BLOB') else
    inherited;
  case FMechanism of
   -1 : Stream.WriteString(' FREE_IT');
    0 : Stream.WriteString(' BY VALUE');
    1 : ; // BY REFERENCE = default
    2 : Stream.WriteString(' BY DESCRIPTOR');
  end;
end;

procedure TMetaUDFField.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(FPosition, SizeOf(FPosition));
  Stream.Write(FMechanism, SizeOf(FMechanism));
end;

{ TMetaRole }

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
  inherited;
end;

end.



