unit ParserTypes;

interface

uses
  Classes, Contnrs;

const
  CParamItemDescription = 'Description for this parameter';
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
    diVarArgs, diMessage, diExternal, diNear, diInline);
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
    cdWriteableTypedConstants, cdSomeWeirdUndocumentedSwitch);

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
    cdStackFrames, cdWriteableTypedConstants, cdSomeWeirdUndocumentedSwitch];

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
    {20}'message', 'external', 'near', 'inline');

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

  TPasItems = class(TObjectList)
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
    { TODO : Change name }
    function IndexOfReferenceName(ReferenceName: string): Integer;
    function IndexOfLinkName(Link: string): Integer;

    procedure SortImplementation;
    procedure OnlyCapitalization;
    procedure WriteImplementationToStream(Stream: TStream);

    procedure DtxSort;
    procedure CalculateCombines;

    procedure FillWithHeaders(const UnitName: string; Ignore, Optional, NotOptional: TStrings);

    property Items[Index: Integer]: TAbstractItem read GetItem write SetItem; default;
    property Author: string read FAuthor write FAuthor;
    property FileName: string read FFileName write FFileName;
  end;

  TAbstractItem = class(TPersistent)
  private
    FTypeList: TPasItems;
    FSimpleName: string;
    FCombineList: TObjectList;
    FCombineWithList: TObjectList;
    FImplementationStr: string;
    FIndex: Integer;
    FBeginDEFList: TStrings;
    FEndDEFList: TStrings;
    FSwitchDEFList: TList;
    FExpandName: Boolean;
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
    function GetLinkName: string; virtual;
  protected
    procedure CheckTypeList;
    procedure AddCombine(AItem: TAbstractItem);
    procedure AddCombineWith(AItem: TAbstractItem);
  public
    constructor Create(const AName: string); virtual;
    destructor Destroy; override;

    function ConstructCopy: TAbstractItem;
    procedure Assign(Source: TPersistent); override;

    procedure WriteDtxDataToStream(AStream: TStream);
    function DtxDataWithoutHeader: string;
    function DtxData: string;

    function IsOptionalInDtx: Boolean; virtual;
    function IncludeInGeneratedDtx: Boolean; virtual;

    { strings for dtx file }
    function DtxClassInfoStr: string;
    function DtxDescriptionStr: string;
    function DtxParamStr: string;
    function DtxReturnsStr: string;
    function DtxSummaryStr: string;
    function DtxTitleStr: string;
    function DtxCombineStr: string;

    property TypeList: TPasItems read FTypeList;
    property LinkName: string read GetLinkName;
    { Simple name,   zonder . zonder @ }
    property SimpleName: string read FSimpleName write FSimpleName;
    { Reference name       met @ met . }
    property ReferenceName: string read GetReferenceName;
    { Title name     zonder . met 'function', 'type', 'procedure' }
    property TitleName: string read GetTitleName;
    property DtxSortName: string read GetDtxSortName;
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
    property ExpandName: Boolean read FExpandName write FExpandName;
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
    function GetLinkName: string; override;
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
    function IsOptionalInDtx: Boolean; override;
//    function IncludeInGeneratedDtx: Boolean; override;

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
    function GetLinkName: string; override;
    function GetReferenceName: string; override;
    function GetAddDescriptionString: string; override;
    function GetSection: TDelphiSection; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

    function IsOptionalInDtx: Boolean; override;
    function IncludeInGeneratedDtx: Boolean; override;

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
    function GetClassString: string; override;
  public
    constructor Create(const AName: string); override;
    destructor Destroy; override;

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
  end;

  TMethodProcItem = class(TParamClassMethodItem)
  private
    FMethodType: TMethodType;
    function GetDelphiType: TDelphiType; override;
    function GetDtxSortName: string; override;
  public
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

    function IsOptionalInDtx: Boolean; override;
    function IncludeInGeneratedDtx: Boolean; override;

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
function StripLeading(const S: string): string;

implementation

uses
  SysUtils, Settings, Dialogs, DelphiParserUtils,
  Math;

type
  TCompilerDirectiveRec = record
    S: string;
    M: TCompilerDirective;
  end;

const
  CConvert: array[TDelphiType] of TOutputType =
  (otClass, otConst, otType, otFunction, otFunctionType,
    otInterface, otFunction, otProcedure, otProcedure, otProcedureType,
    otProperty, otRecord, otResourcestring, otSet, otType, otVar, otField, otMetaClass, otType);

  CTitleFunction = '%s function';

  CDelphiSectionStr: array[TDelphiSection] of string = (
    '', '', '',
    'resourcestring'#13#10, 'type'#13#10, 'const'#13#10, 'var'#13#10, 'threadvar'#13#10,
    '', '', '', '');

  CTitleProcedure = '%s procedure';
  CTitleType = '%s type';
  CTitleVariable = '%s variable';

  CIFDEFFmt = '{$IFDEF %s}'#13#10;
  CIFNDEFFmt = '{$IFNDEF %s}'#13#10;

  CEndFmt = '{$ENDIF %s}'#13#10;

  CCompilerDirectives: array[0..89] of TCompilerDirectiveRec = (
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
    (S: 'F'; M: cdSomeWeirdUndocumentedSwitch),
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
    (S: 'METHODINFO'; M: cdSomeWeirdUndocumentedSwitch),
    (S: 'MINENUMSIZE'; M: cdMinimumEnumSize),
    (S: 'MINSTACKSIZE'; M: cdMINSTACKSIZE),
    (S: 'N'; M: cdSomeWeirdUndocumentedSwitch),
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
    (S: 'S'; M: cdSomeWeirdUndocumentedSwitch),
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
    (S: 'Z'; M: cdSomeWeirdUndocumentedSwitch),
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
    Result := Result + '  ' + RightFill(AStrings[I], MaxLength) + '- ' + CParamItemDescription + #13#10;
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
      if TObject(List1[I]) is TAbstractItem then
        List2.Add(TAbstractItem(List1[I]).ConstructCopy);
  end
  else
    FreeAndNil(List2);
end;

function GetOutputStrFromSettings(const OutputType: TOutputType; const AName: string): string;
var
  Index: Integer;
begin
  with TSettings.Instance do
  begin
    Index := OutputTypeDesc[OutputType].IndexOf(UpperCase(AName));
    if Index < 0 then
      Result := OutputTypeDefaults[OutputType]
    else
      Result := OutputTypeStrings[OutputType][Index];
  end;
end;

function OnIgnoreList(IgnoreList: TStrings; const ReferenceName: string): Boolean;
var
  P: Integer;
begin
  if not Assigned(IgnoreList) then
  begin
    Result := False;
    Exit;
  end;

  Result := IgnoreList.IndexOf(ReferenceName) >= 0;
  if Result then
    Exit;

  P := Pos('.', ReferenceName);
  if P < 0 then
    Exit;

  Result := IgnoreList.IndexOf(Copy(ReferenceName, 1, P - 1)) >= 0;
end;

{ TODO: Change Name }
function StripLeading(const S: string): string;
begin
  if (Length(S) > 1) and (S[1] = '@') and (S[2] = '@') then
    Result := Copy(S, 3, MaxInt)
  else
    Result := S;
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
    //FTypeList: TPasItems;
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
  inherited Create;
  FSimpleName := AName;
  FBeginDEFList := nil;
  FEndDEFList := nil;
  FSwitchDEFList := nil;
  FExpandName := False;
end;

destructor TAbstractItem.Destroy;
begin
  FBeginDEFList.Free;
  FEndDEFList.Free;
  FSwitchDEFList.Free;
  inherited Destroy;
end;

function TAbstractItem.DtxClassInfoStr: string;
const
  CClassInfo = '<TITLEIMG %s>'#13#10'JVCLInfo'#13#10'  GROUP=JVCL.??'#13#10'  FLAG=Component'#13#10;
begin
  if (DelphiType = dtClass) and TSettings.Instance.IsRegisteredClass(SimpleName) then
    Result := Format(CClassInfo, [SimpleName])
  else
    Result := '';
end;

function TAbstractItem.DtxCombineStr: string;
begin
  if CombineString > '' then
    Result := Format('<COMBINE %s>', [CombineString])
  else
    Result := '';
end;

function TAbstractItem.DtxData: string;
begin
  Result := '@@' + ReferenceName + #13#10 + DtxDataWithoutHeader;
end;

function TAbstractItem.DtxDataWithoutHeader: string;
const
  CSeeAlsoDescription = 'See Also'#13#10'  List here other properties, methods (comma separated)'#13#10 +
    '  Remove the ''See Also'' section if there are no references';
  CValueReference = '(Value = %Value - for reference)';
begin
  Result := GetOutputStrFromSettings(CConvert[Self.DelphiType], Self.SimpleName);

  Result := StringReplace(Result, '%author', FTypeList.Author, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%name', SimpleName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%classinfo', DtxClassInfoStr, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%titlename', TitleName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%title', DtxTitleStr, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%referencename', ReferenceName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%sortname', DtxSortName, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%param', DtxParamStr, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%items', ItemsString, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%class', ClassString, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '%nicename', TSettings.Instance.NiceName[ClassString],
    [rfReplaceAll, rfIgnoreCase]);
  if not CanCombine then
  begin
    Result := StringReplace(Result, '%summary', DtxSummaryStr, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%description', DtxDescriptionStr, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%seealso', CSeeAlsoDescription, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%returns', DtxReturnsStr, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%combine', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%refvalue', CValueReference, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%value', ValueString, [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    Result := StringReplace(Result, '%summary', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%description', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%seealso', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%returns', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%combine', DtxCombineStr, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%refvalue', '', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%value', '', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, #13#10#13#10, #13#10, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, #13#10#13#10, #13#10, [rfReplaceAll, rfIgnoreCase]);
  Result := TrimRight(Result);

  EnsureEndingCRLF(Result);
  while Copy(Result, 1, 2) = #13#10 do
    Result := Copy(Result, 3, MaxInt);
end;

function TAbstractItem.DtxDescriptionStr: string;
const
  CDescriptionDescription = 'Description'#13#10'  Write here a description'#13#10;
begin
  Result := AddDescriptionString;
  if Result = '' then
    Result := CDescriptionDescription
  else
    Result := 'Description'#13#10 + Result;
end;

function TAbstractItem.DtxParamStr: string;
const
  CParamDescription = 'Parameters'#13#10;
begin
  Result := ParamString;
  if Result > '' then
    Result := CParamDescription + Result;
end;

function TAbstractItem.DtxReturnsStr: string;
const
  CReturnsDescription = 'Return value'#13#10'  Describe here what the function returns';
begin
  if DelphiType in [dtFunction, dtProcedure] then
    Result := ''
  else
    Result := CReturnsDescription;
end;

function TAbstractItem.DtxSummaryStr: string;
const
  CSummaryDescription = 'Summary'#13#10'  Write here a summary (1 line)';
begin
  Result := AddSummaryString;
  if Result = '' then
    Result := CSummaryDescription
  else
    Result := 'Summary'#13#10 + Result;
end;

function TAbstractItem.DtxTitleStr: string;
begin
  if TitleName > '' then
    Result := Format('<TITLE %s>', [TitleName])
  else
    Result := '';
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

function TAbstractItem.GetLinkName: string;
begin
  Result := ReferenceName;
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

function TAbstractItem.IncludeInGeneratedDtx: Boolean;
begin
  Result := TSettings.Instance.OutputTypeEnabled[CConvert[DelphiType]];
end;

function TAbstractItem.IsOptionalInDtx: Boolean;
begin
  Result := not TSettings.Instance.OutputTypeEnabled[CConvert[DelphiType]];
end;

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
      if TObject(Value[I]) is TCompilerDirectiveItem then
        FSwitchDEFList.Add(TCompilerDirectiveItem(Value[I]).ConstructCopy);
  end;
end;

procedure TAbstractItem.WriteDtxDataToStream(AStream: TStream);
const
  cSepLength = 100;
var
  cSep: array[0..cSepLength + 1] of Char;
var
  S: string;
begin
  S := DtxData;
  FillChar(cSep[0], cSepLength, '-');
  cSep[cSepLength] := #13;
  cSep[cSepLength + 1] := #10;
  AStream.write(cSep[0], cSepLength + 2);
  AStream.write(PChar(S)^, Length(S));
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

function TBaseFuncItem.GetLinkName: string;
begin
  Result := inherited GetReferenceName;
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

  if (diOverload in Directives) or ExpandName then
  begin
    for I := 0 to FParamTypes.Count - 1 do
      Result := Result + '@' + FParamTypes[I];
  end;
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

function TClassItem.GetClassString: string;
begin
  Result := SimpleName;
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

function TClassMemberOrFieldItem.IsOptionalInDtx: Boolean;
begin
  { private,protected members are optional; protected properties not }
  Result := inherited IsOptionalInDtx or
    (Position = inPrivate) or
    ((Position = inProtected) and (DelphiType <> dtProperty));
end;

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

function TClassPropertyItem.IncludeInGeneratedDtx: Boolean;
begin
  { inherited properties are optional }
  Result := inherited IncludeInGeneratedDtx and not IsInherited;
end;

function TClassPropertyItem.IsOptionalInDtx: Boolean;
begin
  { inherited properties are optional }
  Result := inherited IsOptionalInDtx or IsInherited;
end;

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
    if not (FImplementationStr[I] in AllSkipChars) then
    begin
      S[J] := FImplementationStr[I];
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
      if not (TypeList[I] is TCompilerDirectiveItem) and
        not (TypeList[I] is TCommentItem) then
      begin
        Result := TypeList[I].Indentation;
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
  Result := True;
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

function TParamClassMethodItem.GetLinkName: string;
begin
  Result := inherited GetReferenceName;
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

  if (diOverload in Directives) or ExpandName then
  begin
    for I := 0 to FParamTypes.Count - 1 do
      Result := Result + '@' + FParamTypes[I];
  end;
end;

function TParamClassMethodItem.GetSection: TDelphiSection;
begin
  Result := dsClassMethods;
end;

function TParamClassMethodItem.IncludeInGeneratedDtx: Boolean;
begin
  //{ overridden methods are optional }
  { create, destroy are optional}

  Result := inherited IncludeInGeneratedDtx and
    //(diOverride in Directives) or
    not SameText(SimpleName, 'create') and not SameText(SimpleName, 'destroy');
end;

function TParamClassMethodItem.IsOptionalInDtx: Boolean;
begin
  { overridden methods are optional }
  { create, destroy are optional}

  Result := inherited IsOptionalInDtx or
    (diOverride in Directives) or
    SameText(SimpleName, 'create') or SameText(SimpleName, 'destroy');
end;

//=== TPasItems ==============================================================

procedure TPasItems.CalculateAttachTo;
var
  I: Integer;
  LastNonComment: TAbstractItem;
begin
  LastNonComment := nil;

  for I := 0 to Count - 1 do
    if (Items[I] is TCommentItem) or (Items[I] is TCompilerDirectiveItem) then
    begin
      if TOtherItem(Items[I]).AttachedType = atAfter then
        TOtherItem(Items[I]).AttachedTo := LastNonComment;
    end
    else
      LastNonComment := Items[I];

  for I := Count - 1 downto 0 do
    if (Items[I] is TCommentItem) or (Items[I] is TCompilerDirectiveItem) then
    begin
      if TOtherItem(Items[I]).AttachedType = atBefore then
        TOtherItem(Items[I]).AttachedTo := LastNonComment;
    end
    else
      LastNonComment := Items[I];
end;

procedure TPasItems.CalculateCombines;
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

procedure TPasItems.CalculateGroupedWith;
var
  List: TList;

  procedure ResolveGroupWith1(AItem: TCompilerDirectiveItem);
  var
    I: Integer;
  begin
    for I := List.Count - 1 downto 0 do
      if TCompilerDirectiveItem(List[I]).IsConditionalDirective and
        SameText(AItem.ArgumentStr, TCompilerDirectiveItem(List[I]).ArgumentStr) then
      begin
        AItem.GroupedWith := TObject(List[I]) as TCompilerDirectiveItem;
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
      if TCompilerDirectiveItem(List[I]).IsSwitchDirective and
        TCompilerDirectiveItem(List[I]).IsInverseOf(AItem) then
      begin
        AItem.GroupedWith := TObject(List[I]) as TCompilerDirectiveItem;
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
      if Items[I] is TCompilerDirectiveItem then
      begin
        Item := Items[I] as TCompilerDirectiveItem;
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
      Item := TObject(List[I]) as TCompilerDirectiveItem;
      Item.GroupedWith := Item;
    end;
  finally
    List.Free;
  end;
end;

procedure TPasItems.CombineComments;
var
  I, J: Integer;
  CombinedComment: string;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    while (I >= 0) and
      (not (Items[I] is TCommentItem) or TCommentItem(Items[I]).IsSameLineComment) do
      Dec(I);

    J := I - 1;
    while (J >= 0) and
      (Items[J] is TCommentItem) and not TCommentItem(Items[J]).IsSameLineComment do
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

procedure TPasItems.DtxSort;
begin
  Sort(DtxSortCompare);
  FIndexDirty := True;
end;

procedure TPasItems.EnsureIndexOK;
var
  I: Integer;
begin
  if not FIndexDirty then
    Exit;

  for I := 0 to Count - 1 do
    Items[I].FIndex := I;
  FIndexDirty := False;
end;

procedure TPasItems.FillClasses(Classes: TStrings);
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

procedure TPasItems.FillWithHeaders(const UnitName: string; Ignore, Optional,
  NotOptional: TStrings);
var
  I, J: Integer;
  ATypeItem: TAbstractItem;
  ReferenceName, S: string;
  IsOptional: Boolean;
begin
  for I := 0 to Count - 1 do
  begin
    ATypeItem := Items[I];

    ReferenceName := '@@' + ATypeItem.ReferenceName;

    IsOptional := ATypeItem.IsOptionalInDtx or OnIgnoreList(Ignore, ReferenceName);

    if IsOptional then
    begin
      Optional.Add(ReferenceName);
      if ATypeItem is TListItem then
        TListItem(ATypeItem).AddToList(Optional);
    end
    else
    begin
      NotOptional.Add(ReferenceName);
      if ATypeItem is TListItem then
        with ATypeItem as TListItem do
          for J := 0 to Items.Count - 1 do
          begin
            S := '@@' + ReferenceName + '.' + Items[J];
            //              if TSettings.Instance.OnIgnoreTokenList(UnitName, S) then
            if OnignoreList(Ignore, S) then
              Optional.Add(S)
            else
              NotOptional.Add(S);
          end;
    end;
  end;
end;

function TPasItems.GetItem(Index: Integer): TAbstractItem;
begin
  Result := TAbstractItem(inherited Items[Index]);
end;

function TPasItems.IndexOfLinkName(Link: string): Integer;
begin
  Link := StripLeading(Link);

  Result := 0;
  while (Result < Count) and not SameText(Items[Result].LinkName, Link) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

function TPasItems.IndexOfName(const SimpleName: string): Integer;
begin
  Result := 0;
  while (Result < Count) and not SameText(Items[Result].SimpleName, SimpleName) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

function TPasItems.IndexOfReferenceName(
  ReferenceName: string): Integer;
begin
  ReferenceName := StripLeading(ReferenceName);

  Result := 0;
  while (Result < Count) and not SameText(Items[Result].ReferenceName, ReferenceName) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

procedure TPasItems.InsertClassComments;
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
    case Items[I].DelphiType of
      dtMethodFunc, dtMethodProc:
        begin
          if Items[I] is TClassMemberOrFieldItem then
            NewClass := TClassItem(Items[I]).ClassString
          else
            raise Exception.Create('Unknown type');

          if not SameText(NewClass, LastClass) then
          begin
            LastClass := NewClass;

            CommentItem := TCommentItem.Create(RightFill(Format(CComment, [LastClass]), 78, '='));
            CommentItem.AttachedTo := Items[I];
            Insert(I, CommentItem);
          end;
        end;
      dtProcedure, dtFunction:
        if not GlobalProcsCommentAdded or not LocalProcsCommentAdded then
        begin
          if Items[I] is TBaseFuncItem then
          begin
            IsLocal := TBaseFuncItem(Items[I]).IsLocal
          end
          else
            raise Exception.Create('Unknown type');

          if not GlobalProcsCommentAdded and not IsLocal then
          begin
            GlobalProcsCommentAdded := True;

            CommentItem := TCommentItem.Create(RightFill(CGlobalComment, 78, '='));
            CommentItem.AttachedTo := Items[I];
            Insert(I, CommentItem);
          end
          else
            if not LocalProcsCommentAdded and IsLocal then
          begin
            LocalProcsCommentAdded := True;

            CommentItem := TCommentItem.Create(RightFill(CLocalComment, 78, '='));
            CommentItem.AttachedTo := Items[I];
            Insert(I, CommentItem);
          end;
        end;
    end;
    Inc(I);
  end;
end;

procedure TPasItems.InsertComments(CommentList: TList);
var
  I: Integer;
  OtherItem: TOtherItem;
  Index: Integer;
begin
  for I := CommentList.Count - 1 downto 0 do
  begin
    OtherItem := TObject(CommentList[I]) as TOtherItem;
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

procedure TPasItems.InsertConditionalDefines;
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
        Index := Strings2.IndexOf(Strings1[I]);
        if (Index >= 0) and (Strings1.Objects[I] = Strings2.Objects[Index]) then
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
        CompilerDirective := TCompilerDirectiveItem.CreateEndDEF(Strings1[I]);
        RemoveList.Add(CompilerDirective);
      end;
    if Assigned(Strings2) and (Strings2.Count > 0) then
      for I := 0 to Strings2.Count - 1 do
      begin
        CompilerDirective :=
          TCompilerDirectiveItem.CreateBeginDEF(TDefineType(Strings2.Objects[I]), Strings2[I]);
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
        ConstructDiff(Items[I - 1].EndDEFList, Items[I].BeginDEFList)
      else
        ConstructDiff(Items[I - 1].EndDEFList, nil);

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

procedure TPasItems.InsertDirectives(CommentList: TList);
var
  I: Integer;
  OtherItem: TOtherItem;
  Index: Integer;
begin
  for I := CommentList.Count - 1 downto 0 do
    if TObject(CommentList[I]) is TCompilerDirectiveItem then
    begin
      OtherItem := TObject(CommentList[I]) as TOtherItem;
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

procedure TPasItems.InsertSections;
var
  I: Integer;
  LastSection, CurrentSection: TDelphiSection;

begin
  LastSection := dsNone;
  I := 0;
  while I < Count do
  begin
    CurrentSection := Items[I].Section;
    if (CurrentSection <> LastSection) and (CurrentSection in CSectionsStartingWithAReservedWord) then
    begin
      Insert(I, TSectionItem.Create(CurrentSection));
      Inc(I);
    end;

    if not (Items[I] is TCommentItem) and
      (not (Items[I] is TCompilerDirectiveItem) or
      TCompilerDirectiveItem(Items[I]).IsConditionalDirective) then
      LastSection := CurrentSection;
    Inc(I);
  end;
end;

procedure TPasItems.InsertSwitchDefines;
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
        HasSame := TCompilerDirectiveItem(List1[I]).SameAs(TCompilerDirectiveItem(List2[J]));
        Inc(J);
      end;
      if not HasSame then
        RemoveList.Add(List1[I]);
    end;

    { Add }
    for J := 0 to List2.Count - 1 do
    begin
      HasSame := False;
      I := 0;
      while not HasSame and (I < List1.Count) do
      begin
        HasSame := TCompilerDirectiveItem(List1[I]).SameAs(TCompilerDirectiveItem(List2[J]));
        Inc(I);
      end;
      if not HasSame then
        AddList.Add(List2[J]);
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
        ConstructDiff(Items[I - 1].SwitchDEFList, Items[I].SwitchDEFList)
      else
        ConstructDiff(Items[I - 1].SwitchDEFList, nil);

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

procedure TPasItems.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);

  FIndexDirty := FIndexDirty or (Action in [lnAdded, lnDeleted]);

  if (Action = lnAdded) and (TObject(Ptr) is TAbstractItem) then
    TAbstractItem(Ptr).FTypeList := Self;
end;

procedure TPasItems.OnlyCapitalization;
begin
  CalculateAttachTo;
  CalculateGroupedWith;
  InsertSections;
end;

procedure TPasItems.PasSort;
begin
  Sort(PasSortCompare);
  FIndexDirty := True;
end;

procedure TPasItems.RemoveComments(CommentList: TList);
var
  I: Integer;
begin
  Self.OwnsObjects := False;
  try
    for I := Count - 1 downto 0 do
      if (Items[I] is TOtherItem) and (TOtherItem(Items[I]).AttachedType in [atBefore, atAfter]) then
      begin
        CommentList.Add(Items[I]);
        Delete(I);
      end;
  finally
    Self.OwnsObjects := True;
  end;
end;

procedure TPasItems.RemoveDEFSwitches;
var
  SwitchDEFList: TList;

  procedure AddToList(ADirective: TCompilerDirectiveItem);
  var
    I: Integer;
  begin
    for I := SwitchDEFList.Count - 1 downto 0 do
      if TCompilerDirectiveItem(SwitchDEFList[I]).IsInverseOf(ADirective) then
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
      if Items[I] is TCompilerDirectiveItem then
      begin
        if TCompilerDirectiveItem(Items[I]).IsSwitchDirective then
          AddToList(Items[I] as TCompilerDirectiveItem);
      end
      else
        Items[I].SwitchDEFList := SwitchDEFList;

    { and remove }
    for I := Count - 1 downto 0 do
      if (Items[I] is TCompilerDirectiveItem) and
        TCompilerDirectiveItem(Items[I]).IsSwitchDirective then
        Delete(I);
    { Note: values in SwitchDEFList are now no longer valid }
  finally
    SwitchDEFList.Free;
  end;
end;

procedure TPasItems.RemoveDuplicateDEFS;
var
  I: Integer;
begin
  for I := 0 to Count - 2 do
    RemoveDuplicateDEFSFrom(Items[I], Items[I + 1]);
end;

procedure TPasItems.RemoveDuplicateDEFSFrom(const Item1, Item2: TAbstractItem);
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
    Index := List2.IndexOf(List1[I]);
    if (Index >= 0) and (List1.Objects[I] = List2.Objects[Index]) then
    begin
      List1.Delete(I);
      List2.Delete(Index);
    end;
  end;
end;

procedure TPasItems.RemoveTrivialComments(Classes: TStrings);
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

procedure TPasItems.SetItem(Index: Integer; const Value: TAbstractItem);
begin
  inherited Items[Index] := Value;
end;

procedure TPasItems.SortImplementation;
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

procedure TPasItems.WriteImplementationToStream(Stream: TStream);
const
  CEnters: array[TEnterCount] of string = ('', #13#10, #13#10#13#10);

  procedure DoWrite(const S: string);
  begin
    Stream.write(PChar(S)^, Length(S));
  end;
var
  I: Integer;
  EntersAfter, EntersBefore: TEnterCount;
  LNext: TAbstractItem;
begin
  DoWrite(CEnters[CDefaultEnters]);

  for I := 0 to Count - 1 do
  begin
    LNext := Items[I].Next;
    if Assigned(LNext) then
      EntersBefore := LNext.EntersBefore
    else
      EntersBefore := CDefaultEnters;

    EntersAfter := Items[I].EntersAfter;

    DoWrite(Items[I].OutputStr + CEnters[Min(EntersAfter, EntersBefore)]);
  end;

  DoWrite('end.'#13#10);
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
  Result := dtType;
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
