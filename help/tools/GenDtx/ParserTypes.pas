unit ParserTypes;

interface

uses
  Classes, Contnrs;

const
  CParamDescription = 'Description for this parameter';
  CItemDescription = '  Description for %s'#13#10;
  CDefaultEnters = 2;

type
  TEnterCount = 0..2;

  { The sections are ordered in the sorted output pas file as the order
    of TDelphiSection, thus first 'none' (should be empty), then 'uses',..,
    and finally the initalization/finalization section

    dsNone = properties; comments; class fields etc. Comments are removed from
             the list before the sort, and inserted again after the sort }
  TDelphiSection = (
    dsNone,
    dsUses, dsParamDirectives,
    dsResourceString, dsType, dsConst, dsVar, dsThreadVar, dsLocalProcs, dsGlobalProcs,
    dsClassMethods, dsInitializationFinalization);

const
  { Sections that are marked with a reserved word, ie 'resourcestring', 'type' etc. }
  CSectionsStartingWithAReservedWord = [dsResourceString, dsType, dsConst, dsVar, dsThreadVar];

type
  TClassVisibility = (inPrivate, inProtected, inPublic, inPublished);
  TClassVisibilities = set of TClassVisibility;

  TDelphiType = (dtClass, dtConst, dtDispInterface, dtFunction, dtFunctionType,
    dtInterface, dtMethodFunc, dtMethodProc, dtProcedure, dtProcedureType,
    dtProperty, dtRecord, dtResourceString, dtEnum, dtType, dtVar,
    dtClassField, dtMetaClass, dtOther);
  TDelphiTypes = set of TDelphiType;

  TDefineType = (dftIFDEF, dftIFNDEF);

  TMethodType = (mtNormal, mtConstructor, mtDestructor);
  TDirective = (diAbstract, diCdecl, diDynamic, diObject, diOf, diOverload,
    diOverride, diPascal, diRegister, diReintroduce, diSafecall, diStdcall,
    diVirtual, diAssembler, diDeprecated, diPlatform, diForward, diExport, diFar,
    diVarArgs, diMessage, diExternal, diNear);
  TDirectives = set of TDirective;

  TCompilerDirective = (cdIFDEF, cdIFNDEF, cdELSE, cdENDIF, cdELSEIF, cdIF, cdIFEND, cdIFOPT,
    cdAlignFields, cdApplicationType, cdAssert, cdBooleanEval,
    cdLibPrefix, cdLibSuffix, cdLibVersion, cdDebugInfo, cdDEFINE,
    cdDENYPACKAGEUNIT, cdDescription, cdDESIGNONLY, cdExecutableExtension, cdExportSymbols,
    cdExtendedSyntax, cdExternalSymbols, cdHints, cdHPPEmit, cdImageBase, cdImplicitBuild,
    cdImportedData, cdIncludeFile, cdInputOutputChecking, cdLinkObjectFile,
    cdLocalSymbolInformation, cdLongStrings, cdMINSTACKSIZE, cdMAXSTACKSIZE,
    cdMESSAGE, cdMinimumEnumSize, cdOpenStringParam,
    cdOptimization, cdOverflowChecking, cdSAFEDIVIDE, cdNODEFINE, cdNOINCLUDE,
    cdRangeChecking, cdReal48Compatibility, cdReservedAddressSpace,
    cdResource, cdRUNONLY, cdRTTI,
    cdREFERENCEINFO, cdDEFINITIONINFO, cdTYPEDADDRESS, cdUNDEF, cdVarStringChecking,
    cdWarningMessages, cdWarnings, cdWeakPackaging, cdStackFrames,
    cdWriteableTypedConstants);

  TSwitchCompilerDirectiveArgument = (scda1, scda2, scda4, scda8, scdaON, scdaOFF,
    scdaOther);

const
  CConditionalDirectives = [cdIFDEF, cdIFNDEF, cdELSE, cdENDIF, cdELSEIF, cdIF, cdIFEND, cdIFOPT];
  CParamDirectives = [cdApplicationType, cdLibPrefix, cdLibSuffix, cdLibVersion, cdDebugInfo,
    cdDescription, cdExecutableExtension, cdExternalSymbols, cdImageBase, cdIncludeFile,
    cdLinkObjectFile, cdMINSTACKSIZE, cdMAXSTACKSIZE, cdMinimumEnumSize, cdNOINCLUDE,
    cdReservedAddressSpace, cdResource, cdHPPEmit, cdNODEFINE, cdExternalSymbols];
  CSpecialParamDirectives = [cdHPPEmit, cdNODEFINE, cdExternalSymbols];

  CSwitchDirectives = [cdAlignFields, cdAssert, cdBooleanEval, cdDebugInfo, cdDENYPACKAGEUNIT,
    cdDESIGNONLY, cdExportSymbols, cdExtendedSyntax, cdHints, cdImplicitBuild, cdImportedData,
    cdInputOutputChecking, cdLocalSymbolInformation, cdLongStrings, cdOpenStringParam,
    cdOptimization, cdOverflowChecking, cdSAFEDIVIDE, cdRangeChecking, cdReal48Compatibility,
    cdRUNONLY, cdRTTI, cdREFERENCEINFO, cdDEFINITIONINFO, cdTYPEDADDRESS,
    cdVarStringChecking, cdWarningMessages, cdWarnings, cdWeakPackaging,
    cdStackFrames, cdWriteableTypedConstants];

type
  TPropertySpecifier = (psIndex, psRead, psWrite, psStored, psDefault, psNoDefault, psImplements);
  TPropertySpecifiers = set of TPropertySpecifier;
  { -- Filter }
  TDuplicatesType = (dtHide, dtHideCaseSensitive, dtOnlyDuplicates,
    dtOnlyCaseSensitiveDuplicates, dtOnlyCaseSensitiveSingles, dtAll);
  TClassPropertyType = (cptInherited, cptNonInherited);
  TClassPropertyTypes = set of TClassPropertyType;

  TFourState = (fsNo, fsDontCare, fsOneOf, fsYes);
  TTriState = (tsNo, tsDontCare, tsYes);

const
  CDirectives: array[TDirective] of string =
  ({.0}'abstract', 'cdecl', 'dynamic', 'object', 'of',
    {.5}'overload', 'override', 'pascal', 'register', 'reintroduce',
    {10}'safecall', 'stdcall', 'virtual', 'assembler', 'deprecated',
    {15}'platform', 'forward', 'export', 'far', 'varargs',
    {20}'message', 'external', 'near');

  CPropertySpecifiers: array[TPropertySpecifier] of string =
  ('index', 'read', 'write', 'stored', 'default', 'nodefault', 'implements');

  { dtHide, dtHideCaseSensitive, dtOnlyDuplicates,
    dtOnlyCaseSensitiveDuplicates, dtOnlyCaseSensitiveSingles, dtAll }
  CDuplicatesTypeStr: array[TDuplicatesType] of string = (
    'Hide duplicates',
    'Hide duplicates (case sensitive)',
    'Show only duplicates',
    'Show only case sensitive duplicates',
    'Show only case sensitive singles',
    'Show All'
    );

type
  TAbstractItem = class;

  TTypeList = class(TObjectList)
  private
    FAuthor: string;
    FFileName: string;
    FIndexDirty: Boolean;
    function GetItem(Index: Integer): TAbstractItem;
    procedure SetItem(Index: Integer; const Value: TAbstractItem);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure EnsureIndexOK;

    procedure CalculateAttachTo;
    procedure CalculateGroupedWith;
    procedure CombineComments;
    procedure FillClasses(Classes: TStrings);
    procedure InsertClassComments;
    procedure InsertComments(CommentList: TList);
    procedure InsertConditionalDefines;
    procedure InsertDirectives(CommentList: TList);
    procedure InsertSections;
    procedure InsertSwitchDefines;
    procedure PasSort;
    procedure RemoveComments(CommentList: TList);
    procedure RemoveDEFSwitches;
    procedure RemoveDuplicateDEFS;
    procedure RemoveDuplicateDEFSFrom(const Item1, Item2: TAbstractItem);
    procedure RemoveTrivialComments(Classes: TStrings);
  public
    function IndexOfName(const SimpleName: string): Integer;

    procedure SortImplementation;
    procedure OnlyCapitalization;
    procedure WriteImplementationToStream(Stream: TStream);

    procedure DtxSort;
    procedure CalculateCombines;

    property Items[Index: Integer]: TAbstractItem read GetItem write SetItem; default;
    property Author: string read FAuthor write FAuthor;
    property FileName: string read FFileName write FFileName;
  end;

  TAbstractItem = class(TPersistent)
  private
    FTypeList: TTypeList;
    FSimpleName: string;
    FCombineList: TObjectList;
    FCombineWithList: TObjectList;
    //    FHelpStr: string;
    FImplementationStr: string;
    FIndex: Integer;
    FBeginDEFList: TStrings;
    FEndDEFList: TStrings;
    FSwitchDEFList: TList;
    function GetAddDescriptionString: string; virtual;
    function GetAddSummaryString: string; virtual;
    function GetCanCombine: Boolean;
    function GetClassString: string; virtual;
    function GetCombineCount: Integer;
    function GetCombineString: string; virtual;
    function GetCombineWithCount: Integer;
    function GetDelphiType: TDelphiType; virtual; abstract;
    function GetDtxSortName: string; virtual;
    function GetIndentation: Integer; virtual;
    function GetItemsString: string; virtual;
    function GetOutputStr: string;
    function GetParamList: TStrings; virtual;
    function GetParamString: string;
    function GetPasSortName: string;
    function GetPasSortOnIndex: Boolean; virtual;
    function GetRealParamList: TStrings; virtual;
    function GetRealParamString: string;
    function GetReferenceName: string; virtual;
    function GetSection: TDelphiSection; virtual;
    function GetTitleName: string; virtual;
    function GetValueString: string; virtual;
    procedure SetBeginDEFList(const Value: TStrings);
    procedure SetEndDEFList(const Value: TStrings);
    procedure SetImplementationStr(const Value: string);
    procedure SetSwitchDEFList(const Value: TList);
    function GetIndex: Integer;
    function GetNext: TAbstractItem;
    function GetPrevious: TAbstractItem;
  protected
    procedure CheckTypeList;
    procedure AddCombine(AItem: TAbstractItem);
    procedure AddCombineWith(AItem: TAbstractItem);
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    function ConstructCopy: TAbstractItem;
    procedure Assign(Source: TPersistent); override;

    //    function IncludedInFilter(const AData: TFilterData): Boolean; virtual;

    property TypeList: TTypeList read FTypeList;
    { Simple name,   zonder . zonder @ }
    property SimpleName: string read FSimpleName write FSimpleName;
    { Reference name       met @ met . }
    property ReferenceName: string read GetReferenceName;
    { Title name     zonder . met 'function', 'type', 'procedure' }
    property TitleName: string read GetTitleName;
    property DtxSortName: string read GetDtxSortName;
    //    property CompilerPrefix: string read GetCompilerPrefix;
    //    property CompilerSuffix: string read GetCompilerSuffix;
    property PasSortName: string read GetPasSortName;
    property PasSortOnIndex: Boolean read GetPasSortOnIndex;
    property DelphiType: TDelphiType read GetDelphiType;
    property Section: TDelphiSection read GetSection;
    property ItemsString: string read GetItemsString;
    property ParamString: string read GetParamString;
    property RealParamString: string read GetRealParamString;
    property ParamList: TStrings read GetParamList;
    property RealParamList: TStrings read GetRealParamList;
    property ValueString: string read GetValueString;
    property ClassString: string read GetClassString;
    property CombineString: string read GetCombineString;
    property Index: Integer read GetIndex;
    property AddDescriptionString: string read GetAddDescriptionString;
    property AddSummaryString: string read GetAddSummaryString;
    { Voor function of object type > 0 als > 1 dan CanCombine = false }
    property CombineCount: Integer read GetCombineCount;
    { Voor event property = 1 }
    property CombineWithCount: Integer read GetCombineWithCount;
    property CanCombine: Boolean read GetCanCombine;
    { Returns CombineList }

    (* Wanted # of enters before the item, for example

                         |               | EntersAfter | EntersBefore
      const              | const         |     1  (a1) |     2  (b1)
        C = 10;          | C             |     2  (a2) |     2  (b2)
        {$EXTERNALSYM C} | $EXTERNALSYM  |     2  (a3) |     1  (b3)

      Enters between I and I+1 = Min(I.EntersAfter, (I+1).EntersBefore),
      thus here Min(a1, b2)=1 and Min(a2, b3) = 1
    *)
    function EntersAfter: TEnterCount; virtual;
    function EntersBefore: TEnterCount; virtual;

    property ImplementationStr: string read FImplementationStr write SetImplementationStr;
    property OutputStr: string read GetOutputStr;
    property BeginDEFList: TStrings read FBeginDEFList write SetBeginDEFList;
    property EndDEFList: TStrings read FEndDEFList write SetEndDEFList;
    property SwitchDEFList: TList read FSwitchDEFList write SetSwitchDEFList;
    property Indentation: Integer read GetIndentation;
    { Next item in the list, returns nil if this is the last item }
    property Next: TAbstractItem read GetNext;
    { Previous item in the list, returns nil if this is the first item }
    property Previous: TAbstractItem read GetPrevious;
  end;

  TAttacheType = (atBefore, atAfter, atParamDirective, atUses, atFinalization);

  TOtherItem = class(TAbstractItem)
  private
    FAttachedTo: TAbstractItem;
    function GetDelphiType: TDelphiType; override;
    function GetAttachType: TAttacheType; virtual;
  public
    constructor Create(const AValue: string); override;

    procedure Assign(Source: TPersistent); override;

    property AttachedTo: TAbstractItem read FAttachedTo write FAttachedTo;
    property AttachedType: TAttacheType read GetAttachType;
  end;

  TCompilerDirectiveItem = class(TOtherItem)
  private
    FArgumentStr: string;
    FCompilerDirective: TCompilerDirective;
    FCompilerDirectiveStr: string;
    FArgument: TSwitchCompilerDirectiveArgument;
    FGroupedWith: TCompilerDirectiveItem;
    function GetAttachType: TAttacheType; override;
    function GetIndentation: Integer; override;
    function GetIsConditionalDirective: Boolean;
    function GetIsParamDirective: Boolean;
    function GetIsSwitchDirective: Boolean;
    function GetIsBegin: Boolean;
    function GetIsEnd: Boolean;
    function GetIsSingle: Boolean;
    function GetSection: TDelphiSection; override;
  protected
    procedure Initialize;
  public
    constructor Create(const AValue: string); override;
    constructor CreateBeginDEF(ADefineType: TDefineType; const AArgumentStr: string);
    constructor CreateEndDEF(const AArgumentStr: string);

    procedure Assign(Source: TPersistent); override;

    function IsInverseOf(ASwitch: TCompilerDirectiveItem): Boolean;
    function SameAs(ASwitch: TCompilerDirectiveItem): Boolean;
    procedure Inverse;

    function EntersBefore: TEnterCount; override;
    function EntersAfter: TEnterCount; override;

    property IsParamDirective: Boolean read GetIsParamDirective;
    property IsSwitchDirective: Boolean read GetIsSwitchDirective;
    property IsConditionalDirective: Boolean read GetIsConditionalDirective;
    property Directive: TCompilerDirective read FCompilerDirective;
    property ArgumentStr: string read FArgumentStr;
    property Argument: TSwitchCompilerDirectiveArgument read FArgument;

    (* Conditinal defines can be grouped, ie

                          GroupedWith  IsBegin  IsEnd  IsSingle

      {$IFDEF A }         {$ENDIF A}   True     False  False
      CONST X = 0;        -            -        -      -
      {$EXTERNALSYM X }   nil          False    False  True
      {$ENDIF A }         {$IFDEF A}   False    False  False

      An {$IFDEF} can have IsSingle=True, for example if the {$ENDIF} appears
      in a procedure:

      {$IFDEF A}                ----- TCompilerDirectiveItem (IsSingle = True)
      procedure X(I: Integer);  -|
      {$ELSE}                    |
      procedure X(P: Pointer);   |
      {$ENDIF A}                 |--- TProcedureItem
      begin                      |
      end;                      -|
    *)
    property GroupedWith: TCompilerDirectiveItem read FGroupedWith write FGroupedWith;
    property IsBegin: Boolean read GetIsBegin;
    property IsEnd: Boolean read GetIsEnd;
    property IsSingle: Boolean read GetIsSingle;
  end;

  TInitializationFinaliziationItem = class(TOtherItem)
  private
    function GetAttachType: TAttacheType; override;
    function GetIndentation: Integer; override;
    function GetSection: TDelphiSection; override;
  public
    function EntersAfter: TEnterCount; override;
  end;

  TSectionItem = class(TOtherItem)
  private
    function GetIndentation: Integer; override;
  public
    constructor Create(const ASection: TDelphiSection); reintroduce; virtual;
    function EntersAfter: TEnterCount; override;
  end;

  TUsesItem = class(TOtherItem)
  private
    function GetAttachType: TAttacheType; override;
    function GetIndentation: Integer; override;
    function GetSection: TDelphiSection; override;
  end;

  TCommentItem = class(TOtherItem)
  private
    FIsSameLineComment: Boolean;
    function GetAttachType: TAttacheType; override;
    function GetIndentation: Integer; override;
  public
    function GetClassName(var AClassName: string): Boolean;

    function EntersBefore: TEnterCount; override;
    function IsSingleCharComment: Boolean;
    function CanThrowAwayComment: Boolean;
    property IsSameLineComment: Boolean read FIsSameLineComment write FIsSameLineComment;
  end;

  TValueItem = class(TAbstractItem)
  private
    FValue: string;
    function GetValueString: string; override;
    procedure SetValue(const AValue: string);
  protected
    function FormatValue(const S: string): string;
  public
    property Value: string read FValue write SetValue;
  end;

  TListItem = class(TAbstractItem)
  private
    FItems: TStringList;
    function GetItemsString: string; override;
    function GetSection: TDelphiSection; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    { For compare }
    procedure AddToList(AStrings: TStrings);
    property Items: TStringList read FItems;
  end;

  TBaseFuncItem = class(TAbstractItem)
  private
    FParams: TStringList;
    FParamTypes: TStringList;
    FDirectives: TDirectives;
    FIsLocal: Boolean;
    function GetAddDescriptionString: string; override;
    function GetIndentation: Integer; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetRealParamList: TStrings; override;
    function GetReferenceName: string; override;
    function GetSection: TDelphiSection; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    property Params: TStringList read FParams;
    property ParamTypes: TStringList read FParamTypes;
    property Directives: TDirectives read FDirectives write FDirectives;
    { Local: not defined in interface section }
    property IsLocal: Boolean read FIsLocal write FIsLocal;
  end;

  TClassItem = class;

  TClassMemberOrFieldItem = class(TAbstractItem)
  private
    FOwnerClass: TClassItem;
    FPosition: TClassVisibility;
    FOwnerClassAsString: string;
    function GetAddDescriptionString: string; override;
    function GetReferenceName: string; override;
    function GetClassString: string; override;
  public
    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property OwnerClassAsString: string read FOwnerClassAsString write FOwnerClassAsString;
    property OwnerClass: TClassItem read FOwnerClass write FOwnerClass;
    property Position: TClassVisibility read FPosition write FPosition;
  end;

  TParamClassMethodItem = class(TClassMemberOrFieldItem)
  private
    FParams: TStringList;
    FParamTypes: TStringList;
    FDirectives: TDirectives;
    FIsClassMethod: Boolean;
    function GetIndentation: Integer; override;
    function GetRealParamList: TStrings; override;
    function GetReferenceName: string; override;
    function GetAddDescriptionString: string; override;
    function GetSection: TDelphiSection; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    property Params: TStringList read FParams;
    property ParamTypes: TStringList read FParamTypes;
    property Directives: TDirectives read FDirectives write FDirectives;
    property IsClassMethod: Boolean read FIsClassMethod write FIsClassMethod;
  end;

  TClassItem = class(TAbstractItem)
  private
    FList: TList;
    FAncestor: string;
    FInterfaces: TStrings;
    function GetItem(Index: Integer): TAbstractItem;
    procedure SetItem(Index: Integer; const Value: TAbstractItem);
    function GetDelphiType: TDelphiType; override;
    function GetSection: TDelphiSection; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    procedure AddProcedure(AItem: TClassMemberOrFieldItem);
    procedure AddFunction(AItem: TClassMemberOrFieldItem);
    procedure AddProperty(AItem: TClassMemberOrFieldItem);
    property Items[Index: Integer]: TAbstractItem read GetItem write SetItem; default;
    property Ancestor: string read FAncestor write FAncestor;
    property Interfaces: TStrings read FInterfaces;
  end;

  TInterfaceItem = class(TClassItem)
  private
    function GetDelphiType: TDelphiType; override;
  end;

  TConstItem = class(TValueItem)
  private
    function GetDelphiType: TDelphiType; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetSection: TDelphiSection; override;
  public
    function EntersBefore: TEnterCount; override;
  end;

  TFunctionItem = class(TBaseFuncItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  public
    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TFunctionTypeItem = class(TBaseFuncItem)
  private
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetIndentation: Integer; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetSection: TDelphiSection; override;
    function GetTitleName: string; override;
  end;

  TMethodFuncItem = class(TParamClassMethodItem)
  private
    function GetDelphiType: TDelphiType; override;
  public
    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TMethodProcItem = class(TParamClassMethodItem)
  private
    FMethodType: TMethodType;
    function GetDelphiType: TDelphiType; override;
    function GetDtxSortName: string; override;
  public
    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property MethodType: TMethodType read FMethodType write FMethodType;
  end;

  TClassPropertyItem = class(TClassMemberOrFieldItem)
  private
    FIsArray: Boolean;
    FIsInherited: Boolean;
    FSpecifiers: TPropertySpecifiers;
    FTypeStr: string;
    FParamTypes: TStringList;
    FParams: TStringList;
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetParamList: TStrings; override;
    function GetParams: TStringList;
    function GetParamTypes: TStringList;
    function GetRealParamList: TStrings; override;
  public
    destructor Destroy; override;

    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;

    property IsArray: Boolean read FIsArray write FIsArray;
    property IsInherited: Boolean read FIsInherited write FIsInherited;
    property Params: TStringList read GetParams;
    property ParamTypes: TStringList read GetParamTypes;
    property Specifiers: TPropertySpecifiers read FSpecifiers write FSpecifiers;
    property TypeStr: string read FTypeStr write FTypeStr;
  end;

  TClassFieldItem = class(TClassMemberOrFieldItem)
  private
    FTypeStr: string;
    function GetDelphiType: TDelphiType; override;
  public
    property TypeStr: string read FTypeStr write FTypeStr;
  end;

  TProcedureItem = class(TBaseFuncItem)
  private
    function GetTitleName: string; override;
    function GetDelphiType: TDelphiType; override;
  public
    //    function IncludedInFilter(const AData: TFilterData): Boolean; override;
  end;

  TProcedureTypeItem = class(TBaseFuncItem)
  private
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetIndentation: Integer; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetTitleName: string; override;
    function GetSection: TDelphiSection; override;
  end;

  TRecordItem = class(TListItem)
  private
    function GetTitleName: string; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TResourceStringItem = class(TValueItem)
  private
    function GetDelphiType: TDelphiType; override;
    function GetSection: TDelphiSection; override;
  public
    function EntersAfter: TEnterCount; override;
  end;

  TEnumItem = class(TListItem)
  private
    function GetTitleName: string; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TTypeItem = class(TValueItem)
  private
    function GetAddDescriptionString: string; override;
    function GetDelphiType: TDelphiType; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetTitleName: string; override;
    function GetSection: TDelphiSection; override;
  public
    function EntersAfter: TEnterCount; override;
  end;

  TMetaClassItem = class(TTypeItem)
  private
    function GetAddDescriptionString: string; override;
    function GetAddSummaryString: string; override;
    function GetPasSortOnIndex: Boolean; override;
    function GetDelphiType: TDelphiType; override;
  end;

  TVarItem = class(TValueItem)
  private
    FIsThreadVar: Boolean;
    function GetDelphiType: TDelphiType; override;
    function GetTitleName: string; override;
    function GetSection: TDelphiSection; override;
  public
    property IsThreadVar: Boolean read FIsThreadVar write FIsThreadVar;
  end;

function StrToCompilerDirective(const S: string): TCompilerDirective;

implementation

uses
  SysUtils, Settings, Dialogs,
  Math;

type
  TCompilerDirectiveRec = record
    S: string;
    M: TCompilerDirective;
  end;

const
  CTitleFunction = '%s function';

  CDelphiSectionStr: array[TDelphiSection] of string = (
    '', '', '',
    'resourcestring'#13#10, 'type'#13#10, 'const'#13#10, 'var'#13#10, 'threadvar'#13#10,
    '', '', '', '');

  CTitleProcedure = '%s procedure';
  CTitleType = '%s type';
  CTitleVariable = '%s variable';

  CIFDEFFmt = '{$xIFDEF %s}'#13#10;
  CIFNDEFFmt = '{$xIFNDEF %s}'#13#10;
  CEndFmt = '{$xENDIF %s}'#13#10;

  CCompilerDirectives: array[0..84] of TCompilerDirectiveRec = (
    {0}(S: 'A'; M: cdAlignFields),
    (S: 'ALIGN'; M: cdAlignFields),
    (S: 'APPTYPE'; M: cdApplicationType),
    (S: 'ASSERTIONS'; M: cdAssert),
    (S: 'B'; M: cdBooleanEval),
    (S: 'BOOLEVAL'; M: cdBooleanEval),
    (S: 'C'; M: cdAssert),
    (S: 'D'; M: cdDebugInfo),
    (S: 'DEBUGINFO'; M: cdDebugInfo),
    (S: 'DEFINE'; M: cdDEFINE),
    (S: 'DEFINITIONINFO'; M: cdDEFINITIONINFO),
    (S: 'DENYPACKAGEUNIT'; M: cdDENYPACKAGEUNIT),
    (S: 'DESCRIPTION'; M: cdDescription),
    (S: 'DESIGNONLY'; M: cdDESIGNONLY),
    (S: 'E'; M: cdExecutableExtension),
    (S: 'ELSE'; M: cdELSE),
    (S: 'ELSEIF'; M: cdELSEIF),
    (S: 'ENDIF'; M: cdENDIF),
    (S: 'EXTENDEDSYNTAX'; M: cdExtendedSyntax),
    (S: 'EXTENSION'; M: cdExecutableExtension),
    (S: 'EXTERNALSYM'; M: cdExternalSymbols),
    (S: 'G'; M: cdImportedData),
    (S: 'H'; M: cdLongStrings),
    (S: 'HINTS'; M: cdHints),
    (S: 'HPPEMIT'; M: cdHPPEmit),
    (S: 'I'; M: cdInputOutputChecking { or cdIncludeFile }),
    (S: 'IF'; M: cdIF),
    (S: 'IFDEF'; M: cdIFDEF),
    (S: 'IFEND'; M: cdIFEND),
    (S: 'IFNDEF'; M: cdIFNDEF),
    (S: 'IFOPT'; M: cdIFOPT),
    (S: 'IMAGEBASE'; M: cdImageBase),
    (S: 'IMPLICITBUILD'; M: cdImplicitBuild),
    (S: 'IMPORTEDDATA'; M: cdImportedData),
    (S: 'INCLUDE'; M: cdIncludeFile),
    (S: 'IOCHECKS'; M: cdInputOutputChecking),
    (S: 'J'; M: cdWriteableTypedConstants),
    (S: 'L'; M: cdLocalSymbolInformation { or cdLinkObjectFile}),
    (S: 'LIBPREFIX'; M: cdLibPrefix),
    (S: 'LIBSUFFIX'; M: cdLibSuffix),
    (S: 'LIBVERSION'; M: cdLibVersion),
    (S: 'LINK'; M: cdLinkObjectFile),
    (S: 'LOCALSYMBOLS'; M: cdLocalSymbolInformation),
    (S: 'LONGSTRINGS'; M: cdLongStrings),
    (S: 'M'; M: cdRTTI { or cdMemoryAllocationSize}),
    (S: 'MAXSTACKSIZE'; M: cdMAXSTACKSIZE),
    (S: 'MESSAGE'; M: cdMESSAGE),
    (S: 'MINENUMSIZE'; M: cdMinimumEnumSize),
    (S: 'MINSTACKSIZE'; M: cdMINSTACKSIZE),
    (S: 'NODEFINE'; M: cdNODEFINE),
    (S: 'NOINCLUDE'; M: cdNOINCLUDE),
    (S: 'O'; M: cdOptimization),
    (S: 'ObjExportAll'; M: cdExportSymbols),
    (S: 'OPENSTRINGS'; M: cdOpenStringParam),
    (S: 'OPTIMIZATION'; M: cdOptimization),
    (S: 'OVERFLOWCHECKS'; M: cdOverflowChecking),
    (S: 'P'; M: cdOpenStringParam),
    (S: 'Q'; M: cdOverflowChecking),
    (S: 'R'; M: cdResource {or cdRangeChecking}),
    (S: 'RANGECHECKS'; M: cdRangeChecking),
    (S: 'REALCOMPATIBILITY'; M: cdReal48Compatibility),
    (S: 'REFERENCEINFO'; M: cdREFERENCEINFO),
    (S: 'RESOURCE'; M: cdResource),
    (S: 'RESOURCERESERVE'; M: cdReservedAddressSpace), //linux
    (S: 'RUNONLY'; M: cdRUNONLY),
    (S: 'SAFEDIVIDE'; M: cdSAFEDIVIDE),
    (S: 'STACKFRAMES'; M: cdStackFrames),
    (S: 'T'; M: cdTYPEDADDRESS),
    (S: 'TYPEDADDRESS'; M: cdTYPEDADDRESS),
    (S: 'TYPEINFO'; M: cdRTTI),
    (S: 'U'; M: cdSAFEDIVIDE),
    (S: 'UNDEF'; M: cdUNDEF),
    (S: 'V'; M: cdVarStringChecking),
    (S: 'VARSTRINGCHECKS'; M: cdVarStringChecking),
    (S: 'W'; M: cdStackFrames),
    (S: 'WARN'; M: cdWarningMessages),
    (S: 'WARNINGS'; M: cdWarnings),
    (S: 'WEAKPACKAGEUNIT'; M: cdWeakPackaging),
    (S: 'WRITEABLECONST'; M: cdWriteableTypedConstants),
    (S: 'X'; M: cdExtendedSyntax),
    (S: 'Y'; M: cdREFERENCEINFO),
    (S: 'YD'; M: cdDEFINITIONINFO),
    (S: 'Z1'; M: cdMinimumEnumSize),
    (S: 'Z2'; M: cdMinimumEnumSize),
    (S: 'Z4'; M: cdMinimumEnumSize)
    );

  //=== Local procedures =======================================================

function DtxSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TAbstractItem(Item1).DtxSortName,
    TAbstractItem(Item2).DtxSortName);
end;

function LeftFill(const S: string; Count: Integer; const Ch: Char = ' '): string;
var
  CopyCount: Integer;
begin
  { S = '12' Count = 4 -> Result = '  12' }
  SetLength(Result, Count);
  FillChar(PChar(Result)^, Count, Ch);
  CopyCount := Length(S);
  if CopyCount > Count then
    CopyCount := Count;
  Move(PChar(S)^, (PChar(Result) + Count - CopyCount)^, CopyCount);
end;

function RightFill(const S: string; Count: Integer; const Ch: Char = ' '): string;
var
  CopyCount: Integer;
begin
  { S = '12' Count = 4 -> Result = '12  ' }
  SetLength(Result, Count);
  FillChar(PChar(Result)^, Count, Ch);
  CopyCount := Length(S);
  if CopyCount > Count then
    CopyCount := Count;
  Move(PChar(S)^, PChar(Result)^, CopyCount);
end;

function ParamListToString(AStrings: TStrings): string;
var
  I: Integer;
  MaxLength: Integer;
begin
  Result := '';
  if (AStrings = nil) or (AStrings.Count = 0) then
    Exit;

  MaxLength := -1;
  for I := 0 to AStrings.Count - 1 do
    MaxLength := Max(MaxLength, Length(AStrings[I]));
  Inc(MaxLength);
  for I := 0 to AStrings.Count - 1 do
    Result := Result + '  ' + RightFill(AStrings[I], MaxLength) + '- ' + CParamDescription + #13#10;
  { Laatste return eraf halen }
  Delete(Result, Length(Result) - 1, 2);
end;

function PasSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TAbstractItem(Item1).PasSortName,
    TAbstractItem(Item2).PasSortName);
end;

function ArgumentStrToArgument(const S: string): TSwitchCompilerDirectiveArgument;
begin
  if (S = '+') or SameText(S, 'ON') then
    Result := scdaON
  else
    if (S = '-') or SameText(S, 'OFF') then
    Result := scdaOFF
  else
    if S = '1' then
    Result := scda1
  else
    if S = '2' then
    Result := scda2
  else
    if S = '4' then
    Result := scda4
  else
    if S = '8' then
    Result := scda4
  else
    Result := scdaOther;
end;

procedure CopyStrings(Strings1: TStrings; var Strings2: TStrings);
begin
  if Assigned(Strings1) and (Strings1.Count > 0) then
  begin
    if not Assigned(Strings2) then
      Strings2 := TStringList.Create;
    Strings2.Assign(Strings1);
  end
  else
    FreeAndNil(Strings2);
end;

procedure CopyAbstractItemList(List1: TList; var List2: TList);
var
  I: Integer;
begin
  if Assigned(List1) then
  begin
    if not Assigned(List2) then
      List2 := TObjectList.Create;

    List2.Clear;
    for I := 0 to List1.Count - 1 do
      if TObject(List1[i]) is TAbstractItem then
        List2.Add(TAbstractItem(List1[i]).ConstructCopy);
  end
  else
    FreeAndNil(List2);
end;

//=== Global procedures ======================================================

function StrToCompilerDirective(const S: string): TCompilerDirective;
var
  I: Integer;
begin
  for I := Low(CCompilerDirectives) to High(CCompilerDirectives) do
    if SameText(CCompilerDirectives[I].S, S) then
    begin
      Result := CCompilerDirectives[I].M;
      Exit;
    end;

  raise Exception.Create(Format('''%s'' is no compiler directive', [S]));
end;

//=== TAbstractItem ==========================================================

procedure TAbstractItem.AddCombine(AItem: TAbstractItem);
begin
  if not Assigned(FCombineList) then
    FCombineList := TObjectList.Create(False);

  FCombineList.Add(AItem);
end;

procedure TAbstractItem.AddCombineWith(AItem: TAbstractItem);
begin
  if not Assigned(FCombineWithList) then
    FCombineWithList := TObjectList.Create(False);

  FCombineWithList.Add(AItem);
end;

procedure TAbstractItem.Assign(Source: TPersistent);
var
  Src: TAbstractItem;
begin
  if Source is TAbstractItem then
  begin
    Src := Source as TAbstractItem;
    //FTypeList: TTypeList;
    FSimpleName := Src.FSimpleName;
    //FCombineList: TObjectList;
    //FCombineWithList: TObjectList;
//    FHelpStr := Src.FHelpStr;
    FImplementationStr := Src.FImplementationStr;
    FIndex := Src.FIndex;
    CopyStrings(Src.FBeginDEFList, FBeginDEFList);
    CopyStrings(Src.FEndDEFList, FEndDEFList);
    CopyAbstractItemList(Src.FSwitchDEFList, FSwitchDEFList);
  end
  else
    inherited Assign(Source);
end;

procedure TAbstractItem.CheckTypeList;
begin
  if not Assigned(FTypeList) then
    raise Exception.Create('No typelist');
end;

function TAbstractItem.ConstructCopy: TAbstractItem;
begin
  Result := Self.ClassType.Create as TAbstractItem;
  Result.Assign(Self);
end;

constructor TAbstractItem.Create(const AName: string);
begin
  FSimpleName := AName;
  FBeginDEFList := nil;
  FEndDEFList := nil;
  FSwitchDEFList := nil;
end;

destructor TAbstractItem.Destroy;
begin
  FBeginDEFList.Free;
  FEndDEFList.Free;
  FSwitchDEFList.Free;
  inherited Destroy;
end;

function TAbstractItem.EntersAfter: TEnterCount;
begin
  Result := CDefaultEnters;
end;

function TAbstractItem.EntersBefore: TEnterCount;
begin
  Result := CDefaultEnters;
end;

function TAbstractItem.GetAddDescriptionString: string;
begin
  Result := '';
end;

function TAbstractItem.GetAddSummaryString: string;
begin
  Result := '';
end;

function TAbstractItem.GetCanCombine: Boolean;
begin
  Result := CombineCount = 1;
end;

function TAbstractItem.GetClassString: string;
begin
  Result := '';
end;

function TAbstractItem.GetCombineCount: Integer;
begin
  if Assigned(FCombineList) then
    Result := FCombineList.Count
  else
    Result := 0;
end;

function TAbstractItem.GetCombineString: string;
begin
  if Assigned(FCombineList) and (FCombineList.Count = 1) then
    Result := TAbstractItem(FCombineList[0]).ReferenceName
  else
    Result := '';
end;

function TAbstractItem.GetCombineWithCount: Integer;
begin
  if Assigned(FCombineWithList) then
    Result := FCombineWithList.Count
  else
    Result := 0;
end;

//function TAbstractItem.GetCompilerPrefix: string;
//var
//  I: Integer;
//begin
//  Result := '';
//  if Assigned(FBeginDEFList) then
//  begin
//    for I := 0 to FBeginDefList.Count - 1 do
//      case TDefineType(FBeginDefList.Objects[i]) of
//        dftIFDEF: Result := Result + Format(CIFDEFFmt, [FBeginDefList[i]]);
//        dftIFNDEF: Result := Result + Format(CIFNDEFFmt, [FBeginDefList[i]]);
//      end;
//  end;
//end;
//function TAbstractItem.GetCompilerSuffix: string;
//var
//  I: Integer;
//begin
//  Result := '';
//  if Assigned(FEndDEFList) then
//  begin
//    for I := 0 to FEndDEFList.Count - 1 do
//      Result := Result + Format(CEndFmt, [FEndDEFList[i]]);
//  end;
//end;

function TAbstractItem.GetDtxSortName: string;
begin
  { Standaard de complete naam gebruiken, bij constructors, destructors
    doen we wat anders }
  Result := ReferenceName;
end;

function TAbstractItem.GetIndentation: Integer;
begin
  Result := 1;
end;

function TAbstractItem.GetIndex: Integer;
begin
  CheckTypeList;
  FTypeList.EnsureIndexOK;

  Result := FIndex;
end;

function TAbstractItem.GetItemsString: string;
begin
  Result := '';
end;

function TAbstractItem.GetNext: TAbstractItem;
begin
  CheckTypeList;
  if Index >= FTypeList.Count - 1 then
    Result := nil
  else
    Result := FTypeList[Index + 1];
end;

function TAbstractItem.GetOutputStr: string;
begin
  Result :=
    LeftFill(FImplementationStr, Length(FImplementationStr) + Indentation * 2);
end;

function TAbstractItem.GetParamList: TStrings;
begin
  if CanCombine then
    Result := nil
  else
    Result := RealParamList;
end;

function TAbstractItem.GetParamString: string;
begin
  Result := ParamListToString(ParamList);
  {if CanCombine then
    Result := ''
  else
    Result := RealParamString;}
end;

function TAbstractItem.GetPasSortName: string;
begin
  if PasSortOnIndex then
    Result := Char(Ord(Section) + 1) + LeftFill(IntToStr(Index), 10)
  else
    Result := Char(Ord(Section) + 1) + ReferenceName;
end;

function TAbstractItem.GetPasSortOnIndex: Boolean;
begin
  Result := False;
end;

function TAbstractItem.GetPrevious: TAbstractItem;
begin
  CheckTypeList;
  if Index <= 0 then
    Result := nil
  else
    Result := FTypeList[Index - 1];
end;

function TAbstractItem.GetRealParamList: TStrings;
begin
  Result := nil;
end;

function TAbstractItem.GetRealParamString: string;
begin
  Result := '';
end;

function TAbstractItem.GetReferenceName: string;
begin
  Result := SimpleName;
end;

function TAbstractItem.GetSection: TDelphiSection;
begin
  Result := dsNone;
end;

function TAbstractItem.GetTitleName: string;
begin
  Result := '';
end;

function TAbstractItem.GetValueString: string;
begin
  Result := '';
end;

//function TAbstractItem.IncludedInFilter(const AData: TFilterData): Boolean;
//begin
//  Result := DelphiType in AData.RShow;
//end;

procedure TAbstractItem.SetBeginDEFList(const Value: TStrings);
begin
  if Assigned(Value) and (Value.Count > 0) then
  begin
    if not Assigned(FBeginDEFList) then
      FBeginDEFList := TStringList.Create;
    FBeginDEFList.Assign(Value);
  end
  else
    FreeAndNil(FBeginDEFList);
end;

procedure TAbstractItem.SetEndDEFList(const Value: TStrings);
begin
  if Assigned(Value) and (Value.Count > 0) then
  begin
    if not Assigned(FEndDEFList) then
      FEndDEFList := TStringList.Create;
    FEndDEFList.Assign(Value);
  end
  else
    FreeAndNil(FEndDEFList);
end;

procedure TAbstractItem.SetImplementationStr(const Value: string);
var
  StartPtr, EndPtr: PChar;
begin
  StartPtr := PChar(Value);
  EndPtr := StartPtr + Length(Value) - 1;
  while (StartPtr <= EndPtr) and (StartPtr^ in [#13, #10]) do
    Inc(StartPtr);
  while (StartPtr < EndPtr) and (EndPtr^ in [' ', #13, #10]) do
    Dec(EndPtr);
  if StartPtr <= EndPtr then
    SetString(FImplementationStr, StartPtr, EndPtr - StartPtr + 1)
  else
    FImplementationStr := '';
end;

procedure TAbstractItem.SetSwitchDEFList(const Value: TList);
var
  I: Integer;
begin
  if (Value = nil) or (Value.Count = 0) then
    FreeAndNil(FSwitchDEFList)
  else
  begin
    if not Assigned(FSwitchDEFList) then
      FSwitchDEFList := TObjectList.Create;

    { FSwitchDEFList is a object list }
    FSwitchDEFList.Clear;
    for I := 0 to Value.Count - 1 do
      if TObject(Value[i]) is TCompilerDirectiveItem then
        FSwitchDEFList.Add(TCompilerDirectiveItem(Value[i]).ConstructCopy);
  end;
end;

//=== TBaseFuncItem ==========================================================

constructor TBaseFuncItem.Create(const AName: string);
begin
  inherited Create(AName);
  FParams := TStringList.Create;
  FParamTypes := TStringList.Create;
end;

destructor TBaseFuncItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TBaseFuncItem.GetAddDescriptionString: string;
begin
  if diOverload in Directives then
    Result :=
      '  This is an overloaded function/procedure, if possible you may combine the description'#13#10 +
      '  of all these functions into 1 general description. If you do so, combine all "Parameter" '#13#10 +
      '  lists into 1 list, and leave the "Summary", "Description" etc. fields empty for all'#13#10 +
      '  other overloaded functions with the same name.'#13#10
  else
    Result := '';
end;

function TBaseFuncItem.GetIndentation: Integer;
begin
  Result := 0;
end;

function TBaseFuncItem.GetPasSortOnIndex: Boolean;
begin
  Result := IsLocal;
end;

function TBaseFuncItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

function TBaseFuncItem.GetReferenceName: string;
var
  I: Integer;
begin
  Result := inherited GetReferenceName;

  if not (diOverload in Directives) then
    Exit;

  for I := 0 to FParamTypes.Count - 1 do
    Result := Result + '@' + FParamTypes[I];
end;

function TBaseFuncItem.GetSection: TDelphiSection;
begin
  if IsLocal then
    Result := dsLocalProcs
  else
    Result := dsGlobalProcs;
end;

//=== TClassFieldItem ========================================================

function TClassFieldItem.GetDelphiType: TDelphiType;
begin
  Result := dtClassField;
end;

//=== TClassItem =============================================================

procedure TClassItem.AddFunction(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

procedure TClassItem.AddProcedure(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

procedure TClassItem.AddProperty(AItem: TClassMemberOrFieldItem);
begin
  AItem.FOwnerClass := Self;
  FList.Add(AItem);
end;

constructor TClassItem.Create(const AName: string);
begin
  inherited Create(AName);
  FList := TList.Create;
end;

destructor TClassItem.Destroy;
begin
  FList.Free;
  inherited;
end;

function TClassItem.GetDelphiType: TDelphiType;
begin
  Result := dtClass;
end;

function TClassItem.GetItem(Index: Integer): TAbstractItem;
begin
  Result := FList[Index];
end;

function TClassItem.GetSection: TDelphiSection;
begin
  Result := dsType;
end;

//function TClassItem.IncludedInFilter(const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  if (AData.RClass_DescendantOf > '') then
//    Result := TSettings.Instance.IsDescendantOf(SimpleName, AData.RClass_DescendantOf);
//end;

procedure TClassItem.SetItem(Index: Integer; const Value: TAbstractItem);
begin
  FList[Index] := Value;
end;

//=== TClassMemberOrFieldItem ================================================

function TClassMemberOrFieldItem.GetAddDescriptionString: string;
const
  CPosition: array[TClassVisibility] of string = (
    'private', 'protected', 'public', 'published');
begin
  if Position in [inPrivate, inProtected] then
    Result := Format('  This is a %s member, you don''t have to describe these'#13#10,
      [CPosition[Position]])
  else
    Result := inherited GetAddDescriptionString;
end;

function TClassMemberOrFieldItem.GetClassString: string;
begin
  if Assigned(OwnerClass) then
    Result := OwnerClass.SimpleName
  else
    Result := OwnerClassAsString;
end;

function TClassMemberOrFieldItem.GetReferenceName: string;
begin
  if Assigned(OwnerClass) then
    Result := OwnerClass.ReferenceName + '.' + inherited GetReferenceName
  else
    Result := OwnerClassAsString + '.' + inherited GetReferenceName
end;

//function TClassMemberOrFieldItem.IncludedInFilter(
//  const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  if (AData.RClass_DescendantOf > '') then
//    Result := TSettings.Instance.IsDescendantOf(
//      ClassString, AData.RClass_DescendantOf);
//end;

//=== TClassPropertyItem =====================================================

destructor TClassPropertyItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TClassPropertyItem.GetAddDescriptionString: string;
begin
  if Position = inPrivate then
    Result := inherited GetAddDescriptionString
  else
    Result := '';
end;

function TClassPropertyItem.GetDelphiType: TDelphiType;
begin
  Result := dtProperty;
end;

function TClassPropertyItem.GetParamList: TStrings;
begin
  if (CombineWithCount = 1) and (TAbstractItem(FCombineWithList[0]).CombineCount = 1) then
    Result := TAbstractItem(FCombineWithList[0]).RealParamList
  else
    Result := RealParamList;
end;

function TClassPropertyItem.GetParams: TStringList;
begin
  if FParams = nil then
    FParams := TStringList.Create;
  Result := FParams;
end;

function TClassPropertyItem.GetParamTypes: TStringList;
begin
  if not Assigned(FParamTypes) then
    FParamTypes := TStringList.Create;
  Result := FParamTypes;
end;

function TClassPropertyItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

//function TClassPropertyItem.IncludedInFilter(
//  const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  with AData do
//    Result :=
//      TriStateOk(RProperty_ShowInherited, IsInherited) and
//      TriStateOk(RProperty_ShowArray, IsArray) and
//      (Position in RProperty_Scope) and
//      (RProperty_MustExcludeSpecifiers * Specifiers = []) and
//      (RProperty_MustIncludeSpecifiers * Specifiers = RProperty_MustIncludeSpecifiers) and
//
//    ((RProperty_MustIncludeOneOfSpecifiers = []) or
//      (RProperty_MustIncludeOneOfSpecifiers * Specifiers <> [])) and
//
//    (not Assigned(RProperty_In) or (RProperty_In.IndexOf(SimpleName) >= 0));
//end;

//=== TCommentItem ===========================================================

function TCommentItem.CanThrowAwayComment: Boolean;
const
  SkipChars = [#10, #13, ' ', '/', '{', '}'];
  DecoratorChars = ['*', '-', '+', '_', '=', '@', '.'];
  AllSkipChars = SkipChars + DecoratorChars;
var
  S: string;
  I, J: Integer;
begin
  SetLength(S, Length(FImplementationStr));
  J := 1;
  for I := 1 to Length(FImplementationStr) do
    if not (FImplementationStr[i] in AllSkipChars) then
    begin
      S[j] := FImplementationStr[i];
      Inc(J);
    end;
  SetLength(S, J - 1);

  Result :=
    SameText(S, 'GlobalProcedures') or
    SameText(S, 'LocalProcedures') or
    SameText(S, 'Methods');
end;

function TCommentItem.EntersBefore: TEnterCount;
begin
  if IsSameLineComment then
    Result := 0
  else
    Result := inherited EntersBefore;
end;

function TCommentItem.GetAttachType: TAttacheType;
begin
  if IsSameLineComment then
    Result := atAfter
  else
    Result := atBefore;
end;

function TCommentItem.GetClassName(var AClassName: string): Boolean;
const
  FirstChars = ['a'..'z', 'A'..'Z', '_'];
  OKChars = FirstChars + ['0'..'9'];
var
  I, J, K: Integer;
begin
  // Default = "{ TOtherItem }"
  // JVCL    = "//== TOtherItem ===================================="

  Result := False;
  I := 1;
  while I <= Length(FImplementationStr) do
  begin
    if FImplementationStr[I] in FirstChars then
      Break;
    if FImplementationStr[I] in OKChars then
      Exit;
    Inc(I);
  end;

  if I > Length(FImplementationStr) then
    Exit;

  J := I + 1;
  while J <= Length(FImplementationStr) do
    if FImplementationStr[J] in OKChars then
      Inc(J)
    else
      Break;

  { FImplementationStr[I..J) might be a class name }
  K := J + 1;
  while K <= Length(FImplementationStr) do
    if FImplementationStr[K] in FirstChars then
      Exit
    else
      Inc(K);

  Result := True;
  SetString(AClassName, PChar(FImplementationStr) + I - 1, J - I);
end;

function TCommentItem.GetIndentation: Integer;
var
  Item: TAbstractItem;
begin
  case AttachedType of
    atAfter:
      Item := Previous;
    atBefore:
      Item := Next;
  else
    raise Exception.Create('TCommentItem.GetIndentation');
  end;

  if Assigned(Item) then
    Result := Item.Indentation
  else
    Result := 0;
end;

function TCommentItem.IsSingleCharComment: Boolean;
const
  SkipChars = [#10, #13, ' ', '/', '{', '}'];
  DecoratorChars = ['*', '-', '+', '_', '=', '@', '.'];
  AllSkipChars = SkipChars + DecoratorChars;
var
  I: Integer;
  LastChar: Char;
begin
  LastChar := #0;
  Result := False;

  if FImplementationStr = '//' then
    Exit;

  for I := 1 to Length(FImplementationStr) do
    if not (FImplementationStr[I] in AllSkipChars) then
    begin
      if LastChar = #0 then
        LastChar := FImplementationStr[I]
      else
        if FImplementationStr[I] <> LastChar then
        Exit;
    end;

  Result := True;
end;

//=== TCompilerDirectiveItem =================================================

procedure TCompilerDirectiveItem.Assign(Source: TPersistent);
begin
  if Source is TCompilerDirectiveItem then
  begin
    FArgumentStr := TCompilerDirectiveItem(Source).FArgumentStr;
    FCompilerDirective := TCompilerDirectiveItem(Source).FCompilerDirective;
    FCompilerDirectiveStr := TCompilerDirectiveItem(Source).FCompilerDirectiveStr;
    FArgument := TCompilerDirectiveItem(Source).FArgument;
  end;

  inherited Assign(Source);
end;

constructor TCompilerDirectiveItem.Create(const AValue: string);
begin
  inherited Create(AValue);

  FGroupedWith := nil;
  Initialize;
end;

constructor TCompilerDirectiveItem.CreateBeginDEF(ADefineType: TDefineType;
  const AArgumentStr: string);
var
  S: string;
begin
  case ADefineType of
    dftIFDEF: S := Format('{$IFDEF %s}', [AArgumentStr]);
    dftIFNDEF: S := Format('{$IFNDEF %s}', [AArgumentStr]);
  else
    raise Exception.Create('');
  end;
  Create(S);
end;

constructor TCompilerDirectiveItem.CreateEndDEF(
  const AArgumentStr: string);
begin
  Create(Format('{$ENDIF %s}', [AArgumentStr]));
end;

function TCompilerDirectiveItem.EntersAfter: TEnterCount;
begin
  if IsSingle and not (Directive in CSpecialParamDirectives) then
    Result := 1
  else
    Result := inherited EntersAfter;
end;

function TCompilerDirectiveItem.EntersBefore: TEnterCount;
begin
  if Directive in CSpecialParamDirectives then
    Result := 1
  else
    Result := inherited EntersBefore;
end;

function TCompilerDirectiveItem.GetAttachType: TAttacheType;
begin
  if Directive in CSpecialParamDirectives then
    Result := atAfter
  else
    if IsParamDirective then
    Result := atParamDirective
  else
    Result := atBefore;
end;

function TCompilerDirectiveItem.GetIndentation: Integer;
var
  I: Integer;
begin
  CheckTypeList;

  if AttachedType = atAfter then
    Result := AttachedTo.Indentation
  else
    if IsBegin or IsSingle then
  begin
    I := Index + 1;
    while I < TypeList.Count do
    begin
      if not (TypeList[i] is TCompilerDirectiveItem) and
        not (TypeList[i] is TCommentItem) then
      begin
        Result := TypeList[i].Indentation;
        Exit;
      end;
      Inc(I);
    end;
    Result := 0;
  end
  else
    Result := GroupedWith.Indentation;
end;

function TCompilerDirectiveItem.GetIsBegin: Boolean;
begin
  Result := not IsSingle;

  if Result then
  begin
    if not Assigned(FGroupedWith) then
      raise Exception.Create('No GroupedWith');

    Result := GroupedWith.Index > Index;
  end;
end;

function TCompilerDirectiveItem.GetIsConditionalDirective: Boolean;
begin
  Result := Directive in CConditionalDirectives;
end;

function TCompilerDirectiveItem.GetIsEnd: Boolean;
begin
  Result := not IsSingle;

  if Result then
  begin
    if not Assigned(FGroupedWith) then
      raise Exception.Create('No GroupedWith');

    Result := GroupedWith.Index < Index;
  end;
end;

function TCompilerDirectiveItem.GetIsParamDirective: Boolean;
begin
  Result := (Directive in CParamDirectives) and (Argument = scdaOther);
end;

function TCompilerDirectiveItem.GetIsSingle: Boolean;
begin
  Result := IsParamDirective or (Directive = cdDEFINE);

  if not Result then
  begin
    if not Assigned(FGroupedWith) then
      raise Exception.Create('No GroupedWith');

    Result := GroupedWith = Self;
  end;
end;

function TCompilerDirectiveItem.GetIsSwitchDirective: Boolean;
begin
  Result := (Directive in CSwitchDirectives) and (Argument <> scdaOther);
end;

function TCompilerDirectiveItem.GetSection: TDelphiSection;
begin
  if IsParamDirective and not (Directive in CSpecialParamDirectives) then
    Result := dsParamDirectives
  else
    Result := dsNone;
end;

procedure TCompilerDirectiveItem.Initialize;
var
  P, Q: PChar;
begin
  if Length(FImplementationStr) < 4 then
    raise Exception.Create('No compiler directive');

  Q := PChar(FImplementationStr) + 2;
  P := Q;
  while P^ in ['A'..'Z', 'a'..'z'] do
    Inc(P);

  SetString(FCompilerDirectiveStr, Q, P - Q);
  FCompilerDirective := StrToCompilerDirective(FCompilerDirectiveStr);

  while P^ = ' ' do
    Inc(P);

  Q := P;
  while not (Q^ in ['}', #0]) do
    Inc(Q);

  Dec(Q);
  while (Q > P) and (Q^ = ' ') do
    Dec(Q);
  Inc(Q);

  SetString(FArgumentStr, P, Q - P);

  FArgument := ArgumentStrToArgument(FArgumentStr);
end;

procedure TCompilerDirectiveItem.Inverse;
begin
  if FArgumentStr = '+' then
    FArgumentStr := '-'
  else
    if FArgumentStr = '-' then
    FArgumentStr := '+'
  else
    if SameText(FArgumentStr, 'ON') then
    FArgumentStr := 'OFF'
  else
    if SameText(FArgumentStr, 'OFF') then
    FArgumentStr := 'ON'
  else
    raise Exception.CreateFmt('Can''t inverse ''%s''', [FImplementationStr]);

  FArgument := ArgumentStrToArgument(FArgumentStr);
  if Length(FArgumentStr) = 1 then
    FImplementationStr := Format('{$%s%s}', [FCompilerDirectiveStr, FArgumentStr])
  else
    FImplementationStr := Format('{$%s %s}', [FCompilerDirectiveStr, FArgumentStr]);
end;

function TCompilerDirectiveItem.IsInverseOf(
  ASwitch: TCompilerDirectiveItem): Boolean;
begin
  { simplified }
  Result :=
    (FCompilerDirective = ASwitch.FCompilerDirective) and
    (Argument <> ASwitch.Argument);
end;

function TCompilerDirectiveItem.SameAs(
  ASwitch: TCompilerDirectiveItem): Boolean;
begin
  Result :=
    (FCompilerDirective = ASwitch.FCompilerDirective) and
    (Argument = ASwitch.Argument);
end;

//=== TConstItem =============================================================

function TConstItem.EntersBefore: TEnterCount;
var
  S1, S2: string;
  LPrevious: TAbstractItem;
begin
  {
    // EntersBefore = 1 for const 'CMinWidth' (ignore token '// some comment')
    const
      CMinHeight : Integer = 20;  // some comment
      CMinWidth  : Integer = 20;

    // EntersBefore = 2 for const 'B'
    const
      A = 1;

      B = 2;
  }
  LPrevious := Previous;
  while Assigned(LPrevious) and (LPrevious.EntersBefore = 0) do
    LPrevious := LPrevious.Previous;
  if Assigned(LPrevious) and (LPrevious.Section = dsConst) then
  begin
    S1 := Self.SimpleName;
    S2 := LPrevious.SimpleName;
    if (Length(S1) > 3) and (Length(S2) > 3) and
      (StrLIComp(PChar(S1), PChar(S2), 3) = 0) then
    begin
      Result := 1;
      Exit;
    end;
  end;

  Result := inherited EntersAfter;
end;

function TConstItem.GetDelphiType: TDelphiType;
begin
  Result := dtConst;
end;

function TConstItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

function TConstItem.GetSection: TDelphiSection;
begin
  Result := dsConst;
end;

//=== TEnumItem ==============================================================

function TEnumItem.GetDelphiType: TDelphiType;
begin
  Result := dtEnum;
end;

function TEnumItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

function TEnumItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

//=== TFunctionItem ==========================================================

function TFunctionItem.GetDelphiType: TDelphiType;
begin
  Result := dtFunction;
end;

function TFunctionItem.GetTitleName: string;
begin
  Result := Format(CTitleFunction, [SimpleName]);
end;

//function TFunctionItem.IncludedInFilter(const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  with AData do
//    Result :=
//      (RFunction_MustExcludeDirectives * Directives = []) and
//      (RFunction_MustIncludeDirectives * Directives = RFunction_MustIncludeDirectives) and
//
//    ((RFunction_MustIncludeOneOfDirectives = []) or
//      (RFunction_MustIncludeOneOfDirectives * Directives <> []));
//
//  //RFunction_MinimalParamCount: Integer; { -1 -> ignore }
//  //RFunction_MaximalParamCount: Integer; { -1 -> ignore }
//end;

//=== TFunctionTypeItem ======================================================

function TFunctionTypeItem.GetAddDescriptionString: string;
var
  I: Integer;
begin
  if not Assigned(FCombineList) or (CombineCount = 0) then
  begin
    Result := '    Nothing in this unit.';
    Exit;
  end;

  Result := '';
  for I := 0 to FCombineList.Count - 1 do
    Result := Result + Format('    %s'#13#10, [TAbstractItem(FCombineList[I]).ReferenceName]);
end;

function TFunctionTypeItem.GetDelphiType: TDelphiType;
begin
  Result := dtFunctionType;
end;

function TFunctionTypeItem.GetIndentation: Integer;
begin
  Result := 1;
end;

function TFunctionTypeItem.GetPasSortOnIndex: Boolean;
begin
  REsult := True;
end;

function TFunctionTypeItem.GetSection: TDelphiSection;
begin
  Result := dsType;
end;

function TFunctionTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

//=== TInitializationFinaliziationItem =======================================

function TInitializationFinaliziationItem.EntersAfter: TEnterCount;
begin
  Result := 1;
end;

function TInitializationFinaliziationItem.GetAttachType: TAttacheType;
begin
  Result := atFinalization;
end;

function TInitializationFinaliziationItem.GetIndentation: Integer;
begin
  Result := 0;
end;

function TInitializationFinaliziationItem.GetSection: TDelphiSection;
begin
  Result := dsInitializationFinalization;
end;

//=== TInterfaceItem =========================================================

function TInterfaceItem.GetDelphiType: TDelphiType;
begin
  Result := dtInterface;
end;

//=== TListItem ==============================================================

procedure TListItem.AddToList(AStrings: TStrings);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    AStrings.Add('@@' + ReferenceName + '.' + FItems[I]);
end;

constructor TListItem.Create(const AName: string);
begin
  inherited Create(AName);
  FItems := TStringList.Create;
end;

destructor TListItem.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TListItem.GetItemsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FItems.Count - 1 do
    Result := Result + '@@' + ReferenceName + '.' + FItems[I] + #13#10 + Format(CItemDescription, [FItems[I]]);
  if Result > '' then
    { Laatste enter weghalen }
    Delete(Result, Length(Result) - 1, 2);
end;

function TListItem.GetSection: TDelphiSection;
begin
  Result := dsType;
end;

//=== TMetaClassItem =========================================================

function TMetaClassItem.GetAddDescriptionString: string;
begin
  Result := Format(
    '  %s is the metaclass for %s. Its value is the class reference for'#13#10 +
    '  %s or for one of its descendants.', [SimpleName, Value, Value]);
end;

function TMetaClassItem.GetAddSummaryString: string;
begin
  Result :=
    Format('  Defines the metaclass for %s.', [Value]);
end;

function TMetaClassItem.GetDelphiType: TDelphiType;
begin
  Result := dtMetaClass;
end;

function TMetaClassItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

//=== TMethodFuncItem ========================================================

function TMethodFuncItem.GetDelphiType: TDelphiType;
begin
  Result := dtMethodFunc;
end;

//function TMethodFuncItem.IncludedInFilter(
//  const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  with AData do
//    Result :=
//      TriStateOk(RMethodFunction_ShowClassMethod, IsClassMethod) and
//      (Position in RMethodFunction_Scope) and
//      (RMethodFunction_MustExcludeDirectives * Directives = []) and
//      (RMethodFunction_MustIncludeDirectives * Directives = RMethodFunction_MustIncludeDirectives) and
//
//    ((RMethodFunction_MustIncludeOneOfDirectives = []) or
//      (RMethodFunction_MustIncludeOneOfDirectives * Directives <> []));
//
//  //RMethodFunction_MinimalParamCount: Integer; { -1 -> ignore }
//  //RMethodFunction_MaximalParamCount: Integer; { -1 -> ignore }
//end;

//=== TMethodProcItem ========================================================

function TMethodProcItem.GetDelphiType: TDelphiType;
begin
  Result := dtMethodProc;
end;

function TMethodProcItem.GetDtxSortName: string;
var
  OwnerSortName: string;
begin
  if Assigned(OwnerClass) then
    OwnerSortName := OwnerClass.DtxSortName
  else
    OwnerSortName := OwnerClassAsString;

  case MethodType of
    mtNormal: Result := inherited GetDtxSortName;
    mtConstructor: Result := OwnerSortName + '.'#1 + SimpleName;
    mtDestructor: Result := OwnerSortName + '.'#2 + SimpleName;
  else
    raise Exception.Create('Unknown type');
  end;
end;

//function TMethodProcItem.IncludedInFilter(
//  const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  with AData do
//    Result :=
//      TriStateOk(RMethodProcedure_ShowConstructor, MethodType = mtConstructor) and
//      TriStateOk(RMethodProcedure_ShowDestructor, MethodType = mtDestructor) and
//      TriStateOk(RMethodProcedure_ShowClassMethod, IsClassMethod) and
//      (Position in RMethodProcedure_Scope) and
//      (RMethodProcedure_MustExcludeDirectives * Directives = []) and
//      (RMethodProcedure_MustIncludeDirectives * Directives = RMethodProcedure_MustIncludeDirectives) and
//
//    ((RMethodProcedure_MustIncludeOneOfDirectives = []) or
//      (RMethodProcedure_MustIncludeOneOfDirectives * Directives <> []));
//
//  //RMethodProcedure_MinimalParamCount: Integer; { -1 -> ignore }
//  //RMethodProcedure_MaximalParamCount: Integer; { -1 -> ignore }
//end;

//=== TOtherItem =============================================================

procedure TOtherItem.Assign(Source: TPersistent);
begin
  if Source is TOtherItem then
    FAttachedTo := TOtherItem(Source).FAttachedTo;

  inherited Assign(Source);
end;

constructor TOtherItem.Create(const AValue: string);
begin
  inherited Create('');
  ImplementationStr := AValue;
end;

function TOtherItem.GetAttachType: TAttacheType;
begin
  Result := atBefore;
end;

function TOtherItem.GetDelphiType: TDelphiType;
begin
  Result := dtOther;
end;

//=== TParamClassMethodItem ==================================================

constructor TParamClassMethodItem.Create(const AName: string);
begin
  inherited Create(AName);
  FParams := TStringList.Create;
  FParamTypes := TStringList.Create;
end;

destructor TParamClassMethodItem.Destroy;
begin
  FParams.Free;
  FParamTypes.Free;
  inherited;
end;

function TParamClassMethodItem.GetAddDescriptionString: string;
begin
  Result := '';

  if diOverride in Directives then
    Result := Result +
      '  This is an overridden method, you don''t have to describe these' +
      '  if it does the same as the inherited method'#13#10;

  if diOverload in Directives then
    Result := Result +
      '  This is an overloaded function/procedure, if possible you may combine the description'#13#10 +
      '  of all these functions into 1 general description. If you do so, combine all "Parameter" '#13#10 +
      '  lists into 1 list, and leave the "Summary", "Description" etc. fields empty for all'#13#10 +
      '  other overloaded functions with the same name.'#13#10;

  Result := Result + inherited GetAddDescriptionString;
end;

function TParamClassMethodItem.GetIndentation: Integer;
begin
  Result := 0;
end;

function TParamClassMethodItem.GetRealParamList: TStrings;
begin
  Result := FParams;
end;

function TParamClassMethodItem.GetReferenceName: string;
var
  I: Integer;
begin
  Result := inherited GetReferenceName;

  if not (diOverload in Directives) then
    Exit;

  for I := 0 to FParamTypes.Count - 1 do
    Result := Result + '@' + FParamTypes[I];
end;

function TParamClassMethodItem.GetSection: TDelphiSection;
begin
  Result := dsClassMethods;
end;

//=== TProcedureItem =========================================================

function TProcedureItem.GetDelphiType: TDelphiType;
begin
  Result := dtProcedure;
end;

function TProcedureItem.GetTitleName: string;
begin
  Result := Format(CTitleProcedure, [SimpleName]);
end;

//function TProcedureItem.IncludedInFilter(
//  const AData: TFilterData): Boolean;
//begin
//  Result := inherited IncludedInFilter(AData);
//  if not Result then
//    Exit;
//
//  with AData do
//    Result :=
//      (RProcedure_MustExcludeDirectives * Directives = []) and
//      (RProcedure_MustIncludeDirectives * Directives = RProcedure_MustIncludeDirectives) and
//
//    ((RProcedure_MustIncludeOneOfDirectives = []) or
//      (RProcedure_MustIncludeOneOfDirectives * Directives <> []));
//
//  //RProcedure_MinimalParamCount: Integer; { -1 -> ignore }
//  //RProcedure_MaximalParamCount: Integer; { -1 -> ignore }
//end;

//=== TProcedureTypeItem =====================================================

function TProcedureTypeItem.GetAddDescriptionString: string;
var
  I: Integer;
begin
  Result := '  This type is used by (for reference):'#13#10;
  if not Assigned(FCombineList) or (CombineCount = 0) then
  begin
    Result := Result + '    Nothing in this unit.'#13#10;
    Exit;
  end;

  for I := 0 to FCombineList.Count - 1 do
    Result := Result + Format('    %s'#13#10, [TAbstractItem(FCombineList[I]).ReferenceName]);
end;

function TProcedureTypeItem.GetDelphiType: TDelphiType;
begin
  Result := dtProcedureType;
end;

function TProcedureTypeItem.GetIndentation: Integer;
begin
  Result := 1;
end;

function TProcedureTypeItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

function TProcedureTypeItem.GetSection: TDelphiSection;
begin
  Result := dsType;
end;

function TProcedureTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

//=== TRecordItem ============================================================

function TRecordItem.GetDelphiType: TDelphiType;
begin
  Result := dtRecord;
end;

function TRecordItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

function TRecordItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

//=== TResourceStringItem ====================================================

function TResourceStringItem.EntersAfter: TEnterCount;
var
  LNext: TAbstractItem;
begin
  LNext := Next;
  if Assigned(LNext) and (LNext.Section = dsResourceString) then
    Result := 1
  else
    Result := inherited EntersAfter;
end;

function TResourceStringItem.GetDelphiType: TDelphiType;
begin
  Result := dtResourceString;
end;

function TResourceStringItem.GetSection: TDelphiSection;
begin
  Result := dsResourceString;
end;

//=== TSectionItem ===========================================================

constructor TSectionItem.Create(const ASection: TDelphiSection);
begin
  case ASection of
    dsResourceString: inherited Create('resourcestring');
    dsType: inherited Create('type');
    dsConst: inherited Create('const');
    dsVar: inherited Create('var');
    dsThreadVar: inherited Create('threadvar');
  else
    raise Exception.Create('Internal error: TSectionItem.Create');
  end;
end;

function TSectionItem.EntersAfter: TEnterCount;
begin
  Result := 1;
end;

function TSectionItem.GetIndentation: Integer;
begin
  Result := 0;
end;

//=== TTypeItem ==============================================================

function TTypeItem.EntersAfter: TEnterCount;
var
  S1, S2: string;
  LNext: TAbstractItem;
begin
  LNext := Next;
  if Assigned(LNext) and (LNext.Section = dsType) then
  begin
    S1 := LNext.SimpleName;
    S2 := LNext.SimpleName;
    if (Length(S1) = Length(S2)) and
      (Length(S1) > 2) and (Length(S2) > 2) and
      (S1[1] in ['P', 'p', 't', 'T']) and
      (S2[1] in ['P', 'p', 't', 'T']) and
      SameText(Copy(S1, 2, MaxInt), Copy(S2, 2, MaxInt)) then
    begin
      Result := 1;
      Exit;
    end;
  end;

  Result := inherited EntersAfter;
end;

function TTypeItem.GetAddDescriptionString: string;
begin
  Result :=
    '  You don''t have to document already described items such as sets,'#13#10 +
    '  pointers to records etc.'#13#10;
end;

function TTypeItem.GetDelphiType: TDelphiType;
begin
  Result := DtType;
end;

function TTypeItem.GetPasSortOnIndex: Boolean;
begin
  Result := True;
end;

function TTypeItem.GetSection: TDelphiSection;
begin
  Result := dsType;
end;

function TTypeItem.GetTitleName: string;
begin
  Result := Format(CTitleType, [SimpleName]);
end;

//=== TTypeList ==============================================================

procedure TTypeList.CalculateAttachTo;
var
  I: Integer;
  LastNonComment: TAbstractItem;
begin
  LastNonComment := nil;

  for I := 0 to Count - 1 do
    if (Items[i] is TCommentItem) or (Items[i] is TCompilerDirectiveItem) then
    begin
      if TOtherItem(Items[i]).AttachedType = atAfter then
        TOtherItem(Items[i]).AttachedTo := LastNonComment;
    end
    else
      LastNonComment := Items[i];

  for I := Count - 1 downto 0 do
    if (Items[i] is TCommentItem) or (Items[i] is TCompilerDirectiveItem) then
    begin
      if TOtherItem(Items[i]).AttachedType = atBefore then
        TOtherItem(Items[i]).AttachedTo := LastNonComment;
    end
    else
      LastNonComment := Items[i];
end;

procedure TTypeList.CalculateCombines;
var
  I: Integer;

  procedure Examine(const S: string);
  var
    Indx: Integer;
  begin
    Indx := IndexOfName(S);
    if Indx < 0 then
      Exit;

    Items[I].AddCombine(Items[Indx]);
    Items[Indx].AddCombineWith(Items[I]);
  end;

  procedure ExamineEvent(const S: string);
  var
    Indx: Integer;
  begin
    if S = '' then
      Exit;

    Indx := 0;
    while Indx < Count do
    begin
      if (Items[Indx] is TClassPropertyItem) and SameText(TClassPropertyItem(Items[Indx]).TypeStr, S) then
      begin
        Items[I].AddCombine(Items[Indx]);
        Items[Indx].AddCombineWith(Items[I]);
      end;

      Inc(Indx);
    end;
  end;

var
  S: string;
begin
  for I := 0 to Count - 1 do
    if Items[I] is TTypeItem then
    begin
      S := Items[I].ValueString;
      if S = '' then
        Continue;

      if StrLIComp(PChar(S), 'set of', 6) = 0 then
      begin
        S := Trim(Copy(S, 8, MaxInt));
        while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
          System.Delete(S, Length(S), 1);

        Examine(S);
        Continue;
      end;

      if S[1] = '^' then
      begin
        System.Delete(S, 1, 1);
        S := Trim(S);
        while (Length(S) > 0) and (S[Length(S)] in [' ', ';']) do
          System.Delete(S, Length(S), 1);

        Examine(S);
        Continue;
      end;
    end
    else
      if (Items[I] is TFunctionTypeItem) or (Items[I] is TProcedureTypeItem) then
    begin
      S := Items[I].SimpleName;
      ExamineEvent(S);
    end;
end;

procedure TTypeList.CalculateGroupedWith;
var
  List: TList;

  procedure ResolveGroupWith1(AItem: TCompilerDirectiveItem);
  var
    I: Integer;
  begin
    for I := List.Count - 1 downto 0 do
      if TCompilerDirectiveItem(List[i]).IsConditionalDirective and
        SameText(AItem.ArgumentStr, TCompilerDirectiveItem(List[i]).ArgumentStr) then
      begin
        AItem.GroupedWith := TObject(List[i]) as TCompilerDirectiveItem;
        AItem.GroupedWith.GroupedWith := AItem;
        List.Delete(I);
        Exit;
      end;
    raise Exception.Create('Invalid grouped conditional defines');
  end;

  procedure ResolveGroupWith2(AItem: TCompilerDirectiveItem);
  var
    I: Integer;
  begin
    for I := List.Count - 1 downto 0 do
      if TCompilerDirectiveItem(List[i]).IsSwitchDirective and
        TCompilerDirectiveItem(List[i]).IsInverseOf(AItem) then
      begin
        AItem.GroupedWith := TObject(List[i]) as TCompilerDirectiveItem;
        AItem.GroupedWith.GroupedWith := AItem;
        List.Delete(I);
        Exit;
      end;
    List.Add(AItem);
  end;
var
  I: Integer;
  Item: TCompilerDirectiveItem;
begin
  List := TList.Create;
  try
    for I := 0 to Count - 1 do
      if Items[i] is TCompilerDirectiveItem then
      begin
        Item := Items[i] as TCompilerDirectiveItem;
        if Item.IsConditionalDirective then
          case Item.Directive of
            cdIFDEF, cdIFNDEF: List.Add(Item);
            cdENDIF: ResolveGroupWith1(Item);
          else
            raise Exception.Create('Can''t work with that directive');
          end
        else
          if Item.IsSwitchDirective then
          ResolveGroupWith2(Item);
      end;

    for I := 0 to List.Count - 1 do
    begin
      Item := TObject(List[i]) as TCompilerDirectiveItem;
      Item.GroupedWith := Item;
    end;
  finally
    List.Free;
  end;
end;

procedure TTypeList.CombineComments;
var
  I, J: Integer;
  CombinedComment: string;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    while (I >= 0) and
      (not (Items[i] is TCommentItem) or TCommentItem(Items[i]).IsSameLineComment) do
      Dec(I);

    J := I - 1;
    while (J >= 0) and
      (Items[j] is TCommentItem) and not TCommentItem(Items[j]).IsSameLineComment do
      Dec(J);

    { (J..I] are comments }
    if I - J >= 2 then
    begin
      CombinedComment := '';
      while I > J do
      begin
        CombinedComment := Items[I].ImplementationStr + #13#10 + CombinedComment;
        Delete(I);
        Dec(I);
      end;
      Insert(I + 1, TCommentItem.Create(CombinedComment));
    end;

    I := J;
  end;
end;

procedure TTypeList.DtxSort;
begin
  Sort(DtxSortCompare);
  FIndexDirty := True;
end;

procedure TTypeList.EnsureIndexOK;
var
  I: Integer;
begin
  if not FIndexDirty then
    Exit;

  for I := 0 to Count - 1 do
    Items[I].FIndex := I;
  FIndexDirty := False;
end;

procedure TTypeList.FillClasses(Classes: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if (Items[I] is TClassItem) and (Items[I].DelphiType = dtClass) then
      Classes.Add(Items[I].SimpleName)
    else
      if (Items[I] is TClassMemberOrFieldItem) then
      Classes.Add(TClassMemberOrFieldItem(Items[I]).OwnerClassAsString);
end;

function TTypeList.GetItem(Index: Integer): TAbstractItem;
begin
  Result := TAbstractItem(inherited Items[Index]);
end;

function TTypeList.IndexOfName(const SimpleName: string): Integer;
begin
  Result := 0;
  while (Result < Count) and not SameText(Items[Result].SimpleName, SimpleName) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

procedure TTypeList.InsertClassComments;
const
  CComment = '//=== %s =';
  CLocalComment = '//=== Local procedures =';
  CGlobalComment = '//=== Global procedures =';
var
  I: Integer;
  LastClass: string;
  NewClass: string;
  IsLocal: Boolean;
  LocalProcsCommentAdded, GlobalProcsCommentAdded: Boolean;
  CommentItem: TCommentItem;
begin
  I := 0;
  LocalProcsCommentAdded := False;
  GlobalProcsCommentAdded := False;
  while I < Count do
  begin
    case Items[i].DelphiType of
      dtMethodFunc, dtMethodProc:
        begin
          if Items[i] is TClassMemberOrFieldItem then
            NewClass := TClassItem(Items[i]).ClassString
          else
            raise Exception.Create('Unknown type');

          if not SameText(NewClass, LastClass) then
          begin
            LastClass := NewClass;

            CommentItem := TCommentItem.Create(RightFill(Format(CComment, [LastClass]), 78, '='));
            CommentItem.AttachedTo := Items[i];
            Insert(I, CommentItem);
          end;
        end;
      dtProcedure, dtFunction:
        if not GlobalProcsCommentAdded or not LocalProcsCommentAdded then
        begin
          if Items[i] is TBaseFuncItem then
          begin
            IsLocal := TBaseFuncItem(Items[i]).IsLocal
          end
          else
            raise Exception.Create('Unknown type');

          if not GlobalProcsCommentAdded and not IsLocal then
          begin
            GlobalProcsCommentAdded := True;

            CommentItem := TCommentItem.Create(RightFill(CGlobalComment, 78, '='));
            CommentItem.AttachedTo := Items[i];
            Insert(I, CommentItem);
          end
          else
            if not LocalProcsCommentAdded and IsLocal then
          begin
            LocalProcsCommentAdded := True;

            CommentItem := TCommentItem.Create(RightFill(CLocalComment, 78, '='));
            CommentItem.AttachedTo := Items[i];
            Insert(I, CommentItem);
          end;
        end;
    end;
    Inc(I);
  end;
end;

procedure TTypeList.InsertComments(CommentList: TList);
var
  I: Integer;
  OtherItem: TOtherItem;
  Index: Integer;
begin
  for I := CommentList.Count - 1 downto 0 do
  begin
    OtherItem := TObject(CommentList[i]) as TOtherItem;
    Index := Self.IndexOf(OtherItem.AttachedTo);
    case OtherItem.AttachedType of
      atBefore: ;
      atAfter:
        if Index >= 0 then
          Inc(Index);
    else
      raise Exception.Create('Unexpected attach type');
    end;

    if Index >= 0 then
      Self.Insert(Index, OtherItem)
    else
      Self.Add(OtherItem);
  end;
end;

procedure TTypeList.InsertConditionalDefines;
var
  AddList, RemoveList: TList;

  procedure ConstructDiff(Strings1, Strings2: TStrings);
  var
    I, Index: Integer;
    CompilerDirective: TCompilerDirectiveItem;
  begin
    if (Strings1 <> nil) and (Strings2 <> nil) and
      (Strings1.Count > 0) and (Strings2.Count > 0) then
    begin
      { First remove duplicates }
      for I := Strings1.Count - 1 downto 0 do
      begin
        Index := Strings2.IndexOf(Strings1[i]);
        if (Index >= 0) and (Strings1.Objects[i] = Strings2.Objects[Index]) then
        begin
          Strings1.Delete(I);
          Strings2.Delete(Index);
        end;
      end;
    end;

    AddList.Clear;
    RemoveList.Clear;

    if Assigned(Strings1) and (Strings1.Count > 0) then
      for I := 0 to Strings1.Count - 1 do
      begin
        CompilerDirective := TCompilerDirectiveItem.CreateEndDEF(Strings1[i]);
        RemoveList.Add(CompilerDirective);
      end;
    if Assigned(Strings2) and (Strings2.Count > 0) then
      for I := 0 to Strings2.Count - 1 do
      begin
        CompilerDirective :=
          TCompilerDirectiveItem.CreateBeginDEF(TDefineType(Strings2.Objects[i]), Strings2[i]);
        AddList.Add(CompilerDirective);
      end;
  end;
var
  I, J: Integer;
  InsertItem: TCompilerDirectiveItem;
begin
  if Count = 0 then
    Exit;

  AddList := TList.Create;
  RemoveList := TList.Create;
  try
    I := 0;
    while I <= Count do
    begin
      if I = 0 then
        ConstructDiff(nil, Items[0].BeginDEFList)
      else
        if I < Count then
        ConstructDiff(Items[i - 1].EndDEFList, Items[i].BeginDEFList)
      else
        ConstructDiff(Items[i - 1].EndDEFList, nil);

      for J := RemoveList.Count - 1 downto 0 do
      begin
        InsertItem := TCompilerDirectiveItem(RemoveList[J]);
        Insert(I, InsertItem);
        Inc(I);
      end;
      for J := 0 to AddList.Count - 1 do
      begin
        InsertItem := TCompilerDirectiveItem(AddList[J]);
        Insert(I, InsertItem);
        Inc(I);
      end;
      Inc(I);
    end;
  finally
    AddList.Free;
    RemoveList.Free;
  end;
end;

procedure TTypeList.InsertDirectives(CommentList: TList);
var
  I: Integer;
  OtherItem: TOtherItem;
  Index: Integer;
begin
  for I := CommentList.Count - 1 downto 0 do
    if TObject(CommentList[i]) is TCompilerDirectiveItem then
    begin
      OtherItem := TObject(CommentList[i]) as TOtherItem;
      Index := Self.IndexOf(OtherItem.AttachedTo);
      case OtherItem.AttachedType of
        atBefore: ;
        atAfter:
          if Index >= 0 then
            Inc(Index);
      else
        raise Exception.Create('Unexpected attach type');
      end;

      if Index >= 0 then
        Self.Insert(Index, OtherItem)
      else
        Self.Add(OtherItem);

      CommentList.Delete(I);
    end;
end;

procedure TTypeList.InsertSections;
var
  I: Integer;
  LastSection, CurrentSection: TDelphiSection;

begin
  LastSection := dsNone;
  i := 0;
  while I < count do
  begin
    CurrentSection := Items[I].Section;
    if (CurrentSection <> LastSection) and (CurrentSection in CSectionsStartingWithAReservedWord) then
    begin
      Insert(I, TSectionItem.Create(CurrentSection));
      Inc(I);
    end;

    if not (Items[i] is TCommentItem) and
      (not (Items[i] is TCompilerDirectiveItem) or
      TCompilerDirectiveItem(Items[i]).IsConditionalDirective) then
      LastSection := CurrentSection;
    Inc(I);
  end;
end;

procedure TTypeList.InsertSwitchDefines;
var
  AddList, RemoveList: TList;

  procedure ConstructDiff(List1, List2: TList);
  var
    I, J: Integer;
    HasSame: Boolean;
  begin
    { full compare }
    AddList.Clear;
    RemoveList.Clear;
    if (List1 = nil) or (List2 = nil) then
    begin
      if List1 <> nil then
        RemoveList.Assign(List1);
      if List2 <> nil then
        AddList.Assign(List2);
      Exit;
    end;

    { Remove }
    for I := 0 to List1.Count - 1 do
    begin
      HasSame := False;
      J := 0;
      while not HasSame and (J < List2.Count) do
      begin
        HasSame := TCompilerDirectiveItem(List1[i]).SameAs(TCompilerDirectiveItem(List2[j]));
        Inc(J);
      end;
      if not HasSame then
        RemoveList.Add(List1[i]);
    end;

    { Add }
    for J := 0 to List2.Count - 1 do
    begin
      HasSame := False;
      I := 0;
      while not HasSame and (I < List1.Count) do
      begin
        HasSame := TCompilerDirectiveItem(List1[I]).SameAs(TCompilerDirectiveItem(List2[j]));
        Inc(I);
      end;
      if not HasSame then
        AddList.Add(List2[j]);
    end;
  end;
var
  I, J: Integer;
  InsertItem: TCompilerDirectiveItem;
begin
  if Count = 0 then
    Exit;

  AddList := TList.Create;
  RemoveList := TList.Create;
  try
    I := 0;
    while I <= Count do
    begin
      if I = 0 then
        ConstructDiff(nil, Items[0].SwitchDEFList)
      else
        if I < Count then
        ConstructDiff(Items[i - 1].SwitchDEFList, Items[i].SwitchDEFList)
      else
        ConstructDiff(Items[i - 1].SwitchDEFList, nil);

      for J := RemoveList.Count - 1 downto 0 do
      begin
        InsertItem := TCompilerDirectiveItem(TCompilerDirectiveItem(RemoveList[J]).ConstructCopy);
        InsertItem.Inverse;
        Insert(I, InsertItem);
        Inc(I);
      end;
      for J := 0 to AddList.Count - 1 do
      begin
        InsertItem := TCompilerDirectiveItem(TCompilerDirectiveItem(AddList[J]).ConstructCopy);
        //InsertItem.Inverse;
        Insert(I, InsertItem);
        Inc(I);
      end;
      Inc(I);
    end;
  finally
    AddList.Free;
    RemoveList.Free;
  end;
end;

procedure TTypeList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);

  FIndexDirty := FIndexDirty or (Action in [lnAdded, lnDeleted]);

  if (Action = lnAdded) and (TObject(Ptr) is TAbstractItem) then
    TAbstractItem(Ptr).FTypeList := Self;
end;

procedure TTypeList.OnlyCapitalization;
begin
  CalculateAttachTo;
  CalculateGroupedWith;
  InsertSections;
end;

procedure TTypeList.PasSort;
begin
  Sort(PasSortCompare);
  FIndexDirty := True;
end;

procedure TTypeList.RemoveComments(CommentList: TList);
var
  I: Integer;
begin
  Self.OwnsObjects := False;
  try
    for I := Count - 1 downto 0 do
      if (Items[i] is TOtherItem) and (TOtherItem(Items[i]).AttachedType in [atBefore, atAfter]) then
      begin
        CommentList.Add(Items[i]);
        Delete(I);
      end;
  finally
    Self.OwnsObjects := True;
  end;
end;

procedure TTypeList.RemoveDEFSwitches;
var
  SwitchDEFList: TList;

  procedure AddToList(ADirective: TCompilerDirectiveItem);
  var
    I: Integer;
  begin
    for I := SwitchDEFList.Count - 1 downto 0 do
      if TCompilerDirectiveItem(SwitchDEFList[i]).IsInverseOf(ADirective) then
      begin
        SwitchDEFList.Delete(I);
        Exit;
      end;
    SwitchDEFList.Add(ADirective)
  end;
var
  I: Integer;
begin
  SwitchDEFList := TList.Create;
  try
    for I := 0 to Count - 1 do
      if Items[i] is TCompilerDirectiveItem then
      begin
        if TCompilerDirectiveItem(Items[i]).IsSwitchDirective then
          AddToList(Items[i] as TCompilerDirectiveItem);
      end
      else
        Items[i].SwitchDEFList := SwitchDEFList;

    { and remove }
    for I := Count - 1 downto 0 do
      if (Items[i] is TCompilerDirectiveItem) and
        TCompilerDirectiveItem(Items[i]).IsSwitchDirective then
        Delete(I);
    { Note: values in SwitchDEFList are now no longer valid }
  finally
    SwitchDEFList.Free;
  end;
end;

procedure TTypeList.RemoveDuplicateDEFS;
var
  I: Integer;
begin
  for I := 0 to Count - 2 do
    RemoveDuplicateDEFSFrom(Items[I], Items[I + 1]);
end;

procedure TTypeList.RemoveDuplicateDEFSFrom(const Item1, Item2: TAbstractItem);
var
  List1, List2: TStrings;
  I, Index: Integer;
begin
  List1 := Item1.FEndDEFList;
  List2 := Item2.FBeginDEFList;

  if not Assigned(List1) or not Assigned(List2) then
    Exit;
  if (List1.Count = 0) or (List2.Count = 0) then
    Exit;

  for I := List1.Count - 1 downto 0 do
  begin
    Index := List2.IndexOf(List1[i]);
    if (Index >= 0) and (List1.Objects[i] = List2.Objects[Index]) then
    begin
      List1.Delete(I);
      List2.Delete(Index);
    end;
  end;
end;

procedure TTypeList.RemoveTrivialComments(Classes: TStrings);
var
  I: Integer;
  LClassName: string;
  CommentItem: TCommentItem;
begin
  for I := Count - 1 downto 0 do
    if Items[I].ImplementationStr = '' then
      Delete(I)
    else
      if Items[I] is TCommentItem then
    begin
      CommentItem := TCommentItem(Items[I]);
      if CommentItem.GetClassName(LClassName) and (Classes.IndexOf(LClassName) >= 0) then
        Delete(I)
      else
        if CommentItem.IsSingleCharComment or CommentItem.CanThrowAwayComment then
        Delete(I)
    end;
end;

procedure TTypeList.SetItem(Index: Integer; const Value: TAbstractItem);
begin
  inherited Items[Index] := Value;
end;

procedure TTypeList.SortImplementation;
var
  Classes: TStringList;
  CommentList: TList;
begin
  { Determine classes }
  Classes := TStringList.Create;
  try
    Classes.Sorted := True;
    Classes.Duplicates := dupIgnore;

    FillClasses(Classes);
    RemoveTrivialComments(Classes);
    RemoveDEFSwitches;
    CombineComments;
    CalculateAttachTo;

    CommentList := TList.Create;
    try
      RemoveComments(CommentList);
      PasSort;
      InsertDirectives(CommentList);
      InsertSwitchDefines;
      RemoveDuplicateDEFS;
      InsertConditionalDefines;
      { after InsertConditionalDefines }
      CalculateGroupedWith;
      InsertClassComments;
      InsertSections;
      InsertComments(CommentList);
    finally
      CommentList.Free;
    end;
  finally
    Classes.Free;
  end;
end;

procedure TTypeList.WriteImplementationToStream(Stream: TStream);
const
  CEnters: array[TEnterCount] of string = ('', #13#10, #13#10#13#10);

  procedure DoWrite(const S: string);
  begin
    Stream.Write(PChar(S)^, Length(S));
  end;
var
  I: Integer;
  EntersAfter, EntersBefore: TEnterCount;
  LNext: TAbstractItem;
begin
  DoWrite(CEnters[CDefaultEnters]);

  for I := 0 to Count - 1 do
  begin
    LNext := Items[i].Next;
    if Assigned(LNext) then
      EntersBefore := LNext.EntersBefore
    else
      EntersBefore := CDefaultEnters;

    EntersAfter := Items[i].EntersAfter;

    DoWrite(Items[i].OutputStr + CEnters[Min(EntersAfter, EntersBefore)]);
  end;

  DoWrite('end.'#13#10);
end;

//=== TUsesItem ==============================================================

function TUsesItem.GetAttachType: TAttacheType;
begin
  Result := atUses;
end;

function TUsesItem.GetIndentation: Integer;
begin
  Result := 0;
end;

function TUsesItem.GetSection: TDelphiSection;
begin
  Result := dsUses;
end;

//=== TValueItem =============================================================

function TValueItem.FormatValue(const S: string): string;

  procedure StrReplace(const OldPattern, NewPattern: string);
  var
    Tmp: string;
  begin
    Tmp := Result;
    Result := StringReplace(Tmp, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
  end;
begin
  Result := Trim(S);
  StrReplace('^ ', '^');
  StrReplace('^', '^');
  StrReplace('. ', '.');
  StrReplace(' .', '.');
  StrReplace('[ ', '[');
  StrReplace('] ', ']');
  StrReplace(' [', '[');
  StrReplace(' ]', ']');
  StrReplace('( ', '(');
  StrReplace(') ', ')');
  StrReplace(' (', '(');
  StrReplace(' )', ')');
  StrReplace('array[', 'array [');
  StrReplace(']of', '] of');
  StrReplace('of(', 'of (');
  //StrReplace('; ', ';');
  StrReplace('@ ', '@');
  StrReplace(' ;', ';');
  StrReplace(' :', ':');
  StrReplace(',', ', ');
  StrReplace(' ,', ',');
  StrReplace('=', ' = ');
  StrReplace('  ', ' ');
end;

function TValueItem.GetValueString: string;
begin
  Result := FValue;
end;

procedure TValueItem.SetValue(const AValue: string);
begin
  FValue := FormatValue(AValue);
end;

//=== TVarItem ===============================================================

function TVarItem.GetDelphiType: TDelphiType;
begin
  Result := dtVar;
end;

function TVarItem.GetSection: TDelphiSection;
begin
  if IsThreadVar then
    Result := dsThreadVar
  else
    Result := dsVar;
end;

function TVarItem.GetTitleName: string;
begin
  Result := Format(CTitleVariable, [SimpleName]);
end;

end.

