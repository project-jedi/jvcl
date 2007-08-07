unit DtxParser;

interface

uses
  Classes, ContNrs,
  JVCLHelpUtils, ParserTypes;

type
  TSymbolList = class;
  TDtxWriter = class;

  // Dtx items
  TParameterList = class(TDtxBaseItem)
  private
    FParamNames: TStringList;
    FParams: TObjectList;
    FParamStartsWithBackslash: TList;
    function GetParamName(const Index: Integer): string;
    function GetCount: Integer;
    function GetParams(const Index: Integer): TSymbolList;
    function GetIsEmpty: Boolean;
    function GetParamStartsWithBackslash(const Index: Integer): Boolean;
    function GetParamNameWithBackSlash(const Index: Integer): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CleanUp;
    procedure CheckDefaultText(const S: string);
    function Add(AParamName: string; AParams: TSymbolList): Integer;
    function IndexOf(const AParamName: string): Integer;
    procedure Delete(const Index: Integer);
    procedure MoveParam(const SourceIndex, DestIndex: Integer);
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter);
    function Parse(const S: string): Boolean;
    property ParamName[const Index: Integer]: string read GetParamName;
    // same as ParamName but with starting \ char if ParamStartsWithBackslash is true.
    property ParamNameWithBackSlash[const Index: Integer]: string read GetParamNameWithBackSlash;
    property ParamStartsWithBackslash[const Index: Integer]: Boolean read GetParamStartsWithBackslash;
    property Params[const Index: Integer]: TSymbolList read GetParams;
    property Count: Integer read GetCount;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TDtxList = class;

  TComponentInfoFlag = (cifComponent, cifClass, cifRoutine, cifNotInParentList, cifIgnore,
    cifAssignedToGroup);
  TComponentInfoFlags = set of TComponentInfoFlag;

  TDtxTopic = class(TDtxBaseItem)
  private
    FAlias: string;
    FFlags: TComponentInfoFlags;
    FTitleImg: string;
    FCombineWith: string;
    FTitle: string;
    FDonator: string; { JCL }
    FCombine: string;
    //    FInclude: string;
    FGroup: string;
    FAliasOf: string;
    FName: string;
    FDescription: TSymbolList;
    FSeeAlso: TSymbolList;
    FReturnValue: TSymbolList;
    FNote: TSymbolList;
    FSummary: TSymbolList;
    FParameters: TParameterList;
    FHasTocEntry: Boolean;
    FDelphiType: TDelphiType;
    FDirectives: TDirectives;
    FGroup2: string;

    FSubTopics: TObjectList;
    FCombineTopic: TDtxTopic;
    FCombineWiths: TList;

    FList: TDtxList;
    FMethodType: TMethodType;
    FHasPasFileEntry: Boolean;
    FTopicOrder: string;
    function GetHasJVCLInfo: Boolean;
    function GetSubTopics(const Index: Integer): TDtxTopic;
    function GetSubTopicScount: Integer;
    function GetIsDocumented: Boolean;

    procedure DestroyCombineWiths;
    procedure InsertCombineWith(ATopic: TDtxTopic);
    procedure RemoveCombineWith(ATopic: TDtxTopic);
    function GetCombineWith(AIndex: Integer): TDtxTopic;
    function GetCombineWithCount: Integer;
  protected
    function Compare(ADtxTopic: TDtxTopic): Integer;
  public
    constructor Create(AList: TDtxList); virtual;
    destructor Destroy; override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter);
    function ReadFrom(ATopic: TSimpleDtxTopic): Boolean;
    procedure RemoveGeneratedText;
    procedure CleanUp;

    procedure MoveSubTopic(const SourceIndex, DestIndex: Integer);
    function AddSubTopic(ASubTopic: TDtxTopic): Integer;
    procedure DeleteSubTopic(const Index: Integer);

    function ResolveCombines: Boolean;

    property IsDocumented: Boolean read GetIsDocumented;

    property Name: string read FName write FName;
    property Summary: TSymbolList read FSummary;
    property Note: TSymbolList read FNote;
    property ReturnValue: TSymbolList read FReturnValue;
    property Description: TSymbolList read FDescription;
    property Parameters: TParameterList read FParameters;
    property SeeAlso: TSymbolList read FSeeAlso;
    property Alias: string read FAlias write FAlias;
    property AliasOf: string read FAliasOf write FAliasOf;
    property Combine: string read FCombine write FCombine;
    property CombineWith: string read FCombineWith write FCombineWith;
    property Flags: TComponentInfoFlags read FFlags write FFlags;
    property Group: string read FGroup write FGroup;
    property HasTocEntry: Boolean read FHasTocEntry write FHasTocEntry;
    property HasPasFileEntry: Boolean read FHasPasFileEntry write FHasPasFileEntry;
    property HasJVCLInfo: Boolean read GetHasJVCLInfo;
    property Title: string read FTitle write FTitle;
    property Donator: string read FDonator write FDonator;
    property TitleImg: string read FTitleImg write FTitleImg;
    { JCL: <GROUP MathRoutines.Hardware> etc. }
    property Group2: string read FGroup2 write FGroup2;
    property TopicOrder: string read FTopicOrder write FTopicOrder;
    property DelphiType: TDelphiType read FDelphiType write FDelphiType;
    property MethodType: TMethodType read FMethodType write FMethodType;
    property Directives: TDirectives read FDirectives write FDirectives;

    property SubTopics[const Index: Integer]: TDtxTopic read GetSubTopics;
    property SubTopicsCount: Integer read GetSubTopicScount;

    property CombineTopic: TDtxTopic read FCombineTopic;
    property CombineWiths[Index: Integer]: TDtxTopic read GetCombineWith;
    property CombineWithCount: Integer read GetCombineWithCount;
  end;

  TBaseSymbol = class;
  TBaseSymbolClass = class of TBaseSymbol;
  // symbols
  TBaseSymbol = class(TDtxBaseItem)
  public
    constructor Create; virtual;
    procedure CleanUp; virtual;
    function ConstructCopy: TBaseSymbol;
    function CanRemoveFromEnd: Boolean; virtual;
    procedure SaveDtx(ADtxWriter: TDtxWriter); virtual;
    function CanFormatText: Boolean; virtual;
    function AllowedInTable: Boolean; virtual;
    function NoSpaceBefore: Boolean; virtual;
    function NoSpaceAfter: Boolean; virtual;
    function NeedParagraphAfter: Boolean; virtual;
    function DtxText: string; virtual; abstract;
    function IsEmpty: Boolean; virtual;
  end;

  TExtLinkSymbol = class(TBaseSymbol)
  private
    FLink: string;
  public
    constructor CreateNew(const ALink: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function NoSpaceAfter: Boolean; override;
    function IsEmpty: Boolean; override;
    function DtxText: string; override;
    property Link: string read FLink;
  end;

  TStringSymbol = class(TBaseSymbol)
  private
    FValue: string;
  public
    constructor CreateNew(const AString: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function IsEmpty: Boolean; override;
    function DtxText: string; override;
    property Value: string read FValue;
  end;

  TSpecialStringSymbol = class(TStringSymbol)
  public
    function CanFormatText: Boolean; override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
  end;

  TSimpleSymbol = class(TBaseSymbol)
  private
    FToken: TSymbolToken;
  public
    constructor CreateNew(const AToken: TSymbolToken);
    procedure Assign(Source: TPersistent); override;
    function CanFormatText: Boolean; override;
    function AllowedInTable: Boolean; override;
    function CanRemoveFromEnd: Boolean; override;
    function DtxText: string; override;
    function NoSpaceAfter: Boolean; override;
    function NoSpaceBefore: Boolean; override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
    property Token: TSymbolToken read FToken;
  end;

  TCodeSymbol = class(TBaseSymbol)
  private
    FCode: TStringList;
    FIsCode: Boolean; // either <PRE> or <CODE>
  public
    constructor Create; override;
    constructor CreateNew(const ACode: string; const AToken: TSymbolToken);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AllowedInTable: Boolean; override;
    function CanFormatText: Boolean; override;
    function DtxText: string; override;
    procedure CleanUp; override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
  end;

  { TODO : Combine TImageSymbol & TIncludeSymbol }
  TImageSymbol = class(TBaseSymbol)
  private
    FImage: string;
  public
    constructor CreateNew(const AImage: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function DtxText: string; override;
    function IsEmpty: Boolean; override;
    property Image: string read FImage;
  end;

  TIncludeSymbol = class(TBaseSymbol)
  private
    FFileName: string;
  public
    constructor CreateNew(const AFileName: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function DtxText: string; override;
    function IsEmpty: Boolean; override;
    property FileName: string read FFileName;
  end;

  TLinkSymbol = class(TBaseSymbol)
  private
    FDescription: string;
    FLink: string;
  public
    constructor CreateNew(const ALink, ADescription: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function IsEmpty: Boolean; override;
    function DtxText: string; override;
    property Link: string read FLink;
    property Description: string read FDescription;
  end;

  TDelphiLinkSymbol = class(TBaseSymbol)
  private
    FDescription: string;
    FLink: string;
  public
    constructor CreateNew(const ALink, ADescription: string);
    procedure Assign(Source: TPersistent); override;
    procedure Save(AXMLWriter: TXMLWriter); override;
    function IsEmpty: Boolean; override;
    function DtxText: string; override;
    property Link: string read FLink;
    property Description: string read FDescription;
  end;

  TItemsSymbol = class(TBaseSymbol)
  private
    FSymbols: TObjectList;
    function GetCount: Integer;
    function GetSymbols(const Index: Integer): TSymbolList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Add(const S: string): Boolean;
    function AllowedInTable: Boolean; override;
    function CanFormatText: Boolean; override;
    function DtxText: string; override;
    function IsEmpty: Boolean; override;
    function NeedParagraphAfter: Boolean; override;
    procedure CleanUp; override;
    property Symbols[const Index: Integer]: TSymbolList read GetSymbols;
    property Count: Integer read GetCount;
  end;

  TDotItemsSymbol = class(TItemsSymbol)
  public
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
  end;

  TNumberItemsSymbol = class(TItemsSymbol)
  public
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
  end;

  TDtxTable = class(TBaseSymbol)
  private
    FHeaders: TStringObjectList;
    FColWidths: TIntegerList; // lazy create
    FColIsComputeds: TBits; // lazy create
    FData: TObjectList;
    FHasHeader: Boolean;
    FRowCount: Integer;
    FColCount: Integer;
    FHeaderChars: string;
    FTableWidthPercentage: Integer;
    function GetCells(ACol, ARow: Integer): string;
    function GetCellsSymbols(ACol, ARow: Integer): TSymbolList;
    function GetHeader(ACol: Integer): string;
    function GetHeaderSymbols(ACol: Integer): TSymbolList;
    function GetRows(const ARow: Integer): TStrings;
    function GetText: string;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetCellsSymbols(ACol, ARow: Integer; ASymbolList: TSymbolList);
    procedure SetColCount(const Value: Integer);
    procedure SetHeader(ACol: Integer; const Value: string);
    procedure SetHeaderSymbols(ACol: Integer; ASymbolList: TSymbolList);
    procedure SetRowCount(const Value: Integer);
    function GetHeaderChar(const ACol: Integer): Char;
    procedure SetHeaderChar(const ACol: Integer; const Value: Char);
    function GetHasDefaultHeaderChars: Boolean;
    function GetColIsComputed(ACol: Integer): Boolean;
    function GetColWidth(ACol: Integer): Integer;
    procedure SetColIsComputed(ACol: Integer; const Value: Boolean);
    procedure SetColWidth(ACol: Integer; const Value: Integer);
    function GetWidthStr: string;
    function GetColWidthStr(const ACol: Integer): string;
    function GetHasWidthStr: Boolean;
    function GetIsColWidthSpecified: Boolean;
    function GetIsTableWidthSpecified: Boolean;
  protected
    procedure CheckColIndex(const ACol: integer);
    procedure CheckRowIndex(const ARow: integer);
    procedure ClearCellSymbols;

    function MaxTextLengthInCol(const ACol: Integer): Integer;
    function MaxWordLengthInCol(const ACol: Integer): Integer;
    function MaxWordLengthInCell(const ACol, ARow: Integer): Integer;
    procedure DetermineColWidths(Integers: TIntegerList);
    procedure WriteKop(Strings: TStrings; ColWidths: TIntegerList);
    procedure WriteKopSeparator(Strings: TStrings; ColWidths: TIntegerList);
    procedure WriteHeader(Strings: TStrings; ColWidths: TIntegerList);
    procedure WriteRow(Strings: TStrings; const ARow: Integer; ColWidths: TIntegerList);
    procedure CellStringAsText(const S: string; const MaxWidth: Integer; Dest: TStrings);
    procedure MergeStrings(SS1, SS2: TStrings; const ColWidth: Integer);

    property Rows[const Index: Integer]: TStrings read GetRows;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure CleanUp; override;
    function IsEmpty: Boolean; override;

    procedure AddToStrings(Strings: TStrings);
    procedure MergeRows(const ARowIndex, ADeleteRowIndex: Integer);
    function ParseCells: Boolean;
    function CheckCells: Boolean;
    function ReformatCells: Boolean;
    procedure Save(AXMLWriter: TXMLWriter); override;
    procedure SaveDtx(ADtxWriter: TDtxWriter); override;
    function CanFormatText: Boolean; override;
    function AllowedInTable: Boolean; override;
    function DtxText: string; override;
    function NeedParagraphAfter: Boolean; override;

    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property CellsSymbols[ACol, ARow: Integer]: TSymbolList read GetCellsSymbols write SetCellsSymbols;
    property Header[ACol: Integer]: string read GetHeader write SetHeader;
    property HeaderChar[const ACol: Integer]: Char read GetHeaderChar write SetHeaderChar;
    property HeaderSymbols[ACol: Integer]: TSymbolList read GetHeaderSymbols write SetHeaderSymbols;
    property HasHeader: Boolean read FHasHeader write FHasHeader;
    property HasDefaultHeaderChars: Boolean read GetHasDefaultHeaderChars;
    property ColWidth[ACol: Integer]: Integer read GetColWidth write SetColWidth;
    property ColIsComputed[ACol: Integer]: Boolean read GetColIsComputed write SetColIsComputed;

    property Text: string read GetText;
    property IsColWidthSpecified: Boolean read GetIsColWidthSpecified;
    property IsTableWidthSpecified: Boolean read GetIsTableWidthSpecified;
    property WidthStr: string read GetWidthStr;
    property ColWidthStr[const ACol: Integer]: string read GetColWidthStr;
    property HasWidthStr: Boolean read GetHasWidthStr;
    property TableWidthPercentage: Integer read FTableWidthPercentage write FTableWidthPercentage;
  end;

  TSymbolList = class(TObjectList)
  private
    function GetSymbols(const Index: Integer): TBaseSymbol;
    function GetIsEmpty: Boolean;
  public
    function Parse(const S: string): Boolean;
    procedure Assign(ASymbolList: TSymbolList);
    function ConstructCopy: TSymbolList;
    procedure CleanUp;
    procedure CheckDefaultText(const S: string);
    procedure MoveTo(ASymbolList: TSymbolList);
    procedure Save(AXMLWriter: TXMLWriter);
    procedure SaveDtx(ADtxWriter: TDtxWriter; const DoWrapped: Boolean = True);
    procedure SaveTag(const ATag: string; AXMLWriter: TXMLWriter);
    property Symbols[const Index: Integer]: TBaseSymbol read GetSymbols; default;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TDtxList = class
  private
    FPackage: string;
    FStatus: string;
    FAuthor: string;
    FPasFileName: string;
    FSummary: TSymbolList;
    FSkipList: TStrings;
    FTopics: TObjectList;
    FPasTopicRead: Boolean;
    FSeeAlso: string;
    procedure SetSkipList(Strings: TStrings);
    function GetCount: Integer;
    function GetTopics(const Index: Integer): TDtxTopic;
    procedure SetSummary(ASymbolList: TSymbolList);
  protected
    function AddComment(AComment: TSimpleDtxComment): Boolean;
    function AddTopic(ATopic: TSimpleDtxTopic): Boolean;
    function AddPasTopic(ATopic: TSimpleDtxTopic): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ReadFrom(AList: TSimpleDtxList): Boolean;
    procedure RemoveGeneratedText;
    procedure CleanUp;
    procedure MoveItemToSubItem(const SourceIndex: Integer; ADestTopic: TDtxTopic);
    procedure DeleteItem(const Index: Integer);
    function AddItem(ATopic: TDtxTopic): Integer;

    function Find(const S: string; out Index: Integer): Boolean;
    function IsOnSkipList(const AName: string): Boolean;

    function LoadFromFile(const AFileName: string): Boolean;

    procedure SaveToDtxFile(const AFileName: string; const ForBuild: Boolean);
    procedure SaveToDtxStream(AStream: TStream; const ForBuild: Boolean);

    procedure SaveToXMLFile(const AFileName: string);
    procedure SaveToXMLStream(AStream: TStream);

    function ResolveCombines: Boolean;
    procedure Sort;

    property PasFileName: string read FPasFileName write FPasFileName;
    property Summary: TSymbolList read FSummary write SetSummary;
    property Author: string read FAuthor write FAuthor;
    property SeeAlso: string read FSeeAlso write FSeeAlso;
    property Package: string read FPackage write FPackage;
    property SkipList: TStrings read FSkipList write SetSkipList;
    property Status: string read FStatus write FStatus;
    property Count: Integer read GetCount;
    property Topics[const Index: Integer]: TDtxTopic read GetTopics; default;
  end;

  // parsers
  TSymbolParser = class
  private
    FDataString: string;
    FTokenPtr: PChar; // begin of token
    FStringPtr: PChar; // end of string
    FSourcePtr: PChar; // end of token
    FIsOnBeginOfLine: Boolean;
    FEndsWithCloseBracket: Boolean;
    FEndsWithComma: Boolean;
    FTokenStream: TMemoryStream;
    FToken: TSymbolToken;
    FBeginLinePtr: PChar;
    FMultipleEnters: Boolean;
    FInternalList: TSymbolList;
    FList: TSymbolList;
    FNoWhiteSpace: Boolean;
    FExpNumber: Integer; // when reading a toNumber, the expected numberic value
    procedure ClearStream;
    procedure AddTokenToStream(const AddEndBracket: Boolean = False);
    //    procedure AddStringToStream(const S: string);
    function StringFromStream: string;

    procedure AddSymbol(ASymbol: TBaseSymbol);
    function GetList: TSymbolList;
  protected
    procedure SkipBlanks;
    function NextToken(const StopAtComma: Boolean = False): TSymbolToken;
    procedure AddBreak;
    procedure AddNoWhiteSpace;
    function ReadCode(const AToken: TSymbolToken): Boolean;
    function ReadAutoLink: Boolean;
    function ReadDotList: Boolean;
    function ReadLink: Boolean;
    function ReadDelphiLink: Boolean;
    function ReadImage: Boolean;
    function ReadInclude: Boolean;
    function ReadNumberList: Boolean;
    function ReadParagraph: Boolean;
    function ReadSimpleTag(const AToken: TSymbolToken): Boolean;
    function ReadExtLinkBegin: Boolean;
    function ReadString: Boolean;
    function ReadTable: Boolean;
  public
    constructor Create(const S: string);
    destructor Destroy; override;
    function Parse: Boolean;
    function TokenString: string;
    property Token: TSymbolToken read FToken;
    property List: TSymbolList read GetList write FList;
  end;

  TParamParser = class
  private
    FList: TParameterList;
    FLines: TStrings;
    FInternalList: TParameterList;
    function GetList: TParameterList;
  public
    constructor Create(const S: string);
    destructor Destroy; override;
    function Parse: Boolean;
    property List: TParameterList read GetList write FList;
  end;

  TTableParser = class
  private
    FTable: TDtxTable;
    FLines: TStrings;
    FTableWidths: TStrings;
    FInternalTable: TDtxTable;
    function GetTable: TDtxTable;
  protected
    function DetermineSepIndexen(ASepIndexen: TBits; out AHeaderSepIndex: Integer): Boolean;
    function DetermineColDimension(AColStarts, AColLengths: TIntegerList; out AHeaderSepIndex: Integer): Boolean;
    function DetermineMultiRowIndexen(AMultiRowIndexen: TIntegerList): Boolean;
    function FillTable(AColStarts, AColLengths: TIntegerList; out AHeaderSepIndex: Integer): Boolean;
    function MergeMultiRows: Boolean;

    function ParseTableWidths: Boolean;
    function ParseTable: Boolean;
  public
    constructor Create(const TableStr: string; ATableWidths: TStrings);
    destructor Destroy; override;
    function Parse: Boolean;
    property Table: TDtxTable read GetTable write FTable;

  end;

  TDtxWriter = class
  private
    FDestStream: TStream;
    FBeginOfLine: Boolean;
    FIndent: Integer;
    FForBuild: Boolean;
  protected
    procedure IncIndent(const Count: Integer);
    procedure DecIndent(const Count: Integer);
  public

    procedure Reset;

    procedure BeginSection(const ASection: string);
    procedure EndSection;

    procedure WriteSepLine;
    procedure WritePasHeader(const APasFileName: string; ASummary: TSymbolList;
      const AAuthor, ASeeAlso: string);
    procedure WriteComment(const AComment: string);
    procedure WriteLnFmt(const AFormat: string; const Args: array of const);
    procedure WriteLn(const Data: string = ''); overload;
    procedure WriteLn(Data: PChar; Length: Integer); overload;
    procedure Write(const Data: string); overload;
    procedure Write(Data: PChar; Length: Integer); overload;
    procedure WriteTitle(const ATitle: string);
    procedure WriteTopicName(const ATopicName: string);
    procedure WriteTopicItem(const AHeader: string; ASymbolList: TSymbolList);
    procedure WriteWrappedData(const S: string);
    procedure WriteMultiLineData(const Data: string; const DoIndent: Boolean); overload;
    procedure WriteMultiLineData(SS: TStrings; const DoIndent: Boolean); overload;
    property DestStream: TStream read FDestStream write FDestStream;
    property ForBuild: Boolean read FForBuild write FForBuild;
  end;

implementation

uses
  SysUtils, Math, RTLConsts, Dialogs;

type
  TStrToken = record
    Token: PChar;
    T: TSymbolToken;
  end;

const
  cDividerWidth = 2;

  cDefaultIndent = 2;

  cTokens: array[0..25] of TStrToken = (
    (Token: '<LINK'; T: xtoLink),
    (Token: '<AUTOLINK'; T: toAutoLink),
    (Token: '<TABLE'; T: toTableBegin),
    (Token: '</TABLE>'; T: toTableEnd),
    (Token: '<B>'; T: toBoldBegin),
    (Token: '</B>'; T: toBoldEnd),
    (Token: '<I>'; T: toItalicBegin),
    (Token: '</I>'; T: toItalicEnd),
    (Token: '<U>'; T: toUnderlineBegin),
    (Token: '</U>'; T: toUnderlineEnd),
    (Token: '<P>'; T: toParagraph),
    (Token: '<PRE>'; T: toPreBegin),
    (Token: '</PRE>'; T: toPreEnd),
    (Token: '<CODE>'; T: toCodeBegin),
    (Token: '</CODE>'; T: toCodeEnd),
    (Token: '<EXTLINK'; T: toExtLinkBegin),
    (Token: '</EXTLINK>'; T: toExtLinkEnd),
    (Token: '<IMAGE'; T: toImage),
    (Token: '<INCLUDE'; T: toInclude),
    (Token: '<COLOR'; T: toColorBegin),
    (Token: '</COLOR'; T: toColorEnd),
    (Token: '<FLAG'; T: toFlag),
    (Token: '<KEYWORDS'; T: toKeyWords),
    (Token: '<IMPLEMENTATION'; T: toImplementation),
    (Token: '<VERSIONSPECIFIC'; T: toVersionSpecific),
    (Token: '<DELPHILINK'; T: toDelphiLink)
    );

  cXMLDelphiType: array[TDelphiType] of string = (
    'Class', 'Const', 'DispInterface', 'Function', 'FunctionType',
    'Interface', 'MethodFunc', 'MethodProc', 'Procedure', 'ProcedureType',
    'Property', 'Record', 'ResourceString', 'Enum', 'Type', 'Var',
    'ClassField', 'MetaClass', ''
    );
  cXMLDirective: array[TDirective] of string = (
    'Abstract', 'Cdecl', 'Dynamic', 'Object', '', 'Overload',
    'Override', 'Pascal', 'Register', 'Reintroduce', 'Safecall', 'Stdcall',
    'Virtual', 'Assembler', 'Deprecated', 'Platform', 'Forward', 'Export', 'Far',
    'VarArgs', 'Message', 'External', 'Near', 'Inline'
    );

  cComponentInfoFlagStr: array[TComponentInfoFlag] of string = (
    'Component', 'Class', 'Routine', 'NotInParentList', '',
    '');

  //=== Local procedures =======================================================

function ExpString(const S: string; const ALength: Integer): string;
begin
  Result := S;
  SetLength(Result, ALength);
  if ALength > Length(S) then
    FillChar((PChar(Result) + Length(S))^, ALength - Length(S), Ord(' '));
end;

procedure RemoveEndCRLF(var S: string);
var
  L: Integer;
begin
  L := Length(S);
  while (L > 0) and (S[L] in [#10, #13]) do
    Dec(L);
  SetLength(S, L);
end;

function DtxSortCompore(Item1, Item2: Pointer): Integer;
begin
  if (TObject(Item1) is TDtxTopic) and (TObject(Item2) is TDtxTopic) then
  begin
    Result := TDtxTopic(Item1).Compare(TDtxTopic(Item2));
  end
  else
    Result := 0;
end;

function IndexOfText(const Str: string; const Strings: array of string): Integer;
begin
  Result := High(Strings);
  while (Result >= 0) and not AnsiSameText(Str, Strings[Result]) do
    Dec(Result);
end;

function ParseCompSettings(S: string): TComponentInfoFlags;
var
  I: Integer;
begin
  Result := [];
  while S <> '' do
  begin
    I := 1;
    while (I <= Length(S)) and not (S[I] in [',', ';']) do
      Inc(I);
    case IndexOfText(Trim(Copy(S, 1, I - 1)), ['Component', 'Class', 'Routine', 'NotInParentList']) of
      0:
        Result := Result + [cifComponent] - [cifClass, cifRoutine];
      1:
        Result := Result + [cifClass] - [cifComponent, cifRoutine];
      2:
        Result := Result + [cifRoutine] - [cifComponent, cifClass];
      3:
        Result := Result + [cifNotInParentList];
    else
      WarningMsgFmt('Unknown setting <%s>', [Trim(Copy(S, 1, I - 1))]);
    end;
    Delete(S, 1, I);
    S := Trim(S);
  end;
end;

function CompFlagsToStr(const AFlags: TComponentInfoFlags): string;
var
  InfoFlag: TComponentInfoFlag;
begin
  Result := '';
  for InfoFlag := Low(TComponentInfoFlag) to High(TComponentInfoFlag) do
    if InfoFlag in AFlags then
      Result := Result + cComponentInfoFlagStr[InfoFlag] + ',';
  if Result > '' then
    Delete(Result, Length(Result), 1);
end;

function IsNumberDot(StartPtr, EndPtr: PChar; const ExpNumber: Integer): Boolean;
const
  cNum = ['0'..'9'];
var
  L: Integer;
  Value: Integer;
begin
  // length must be >= 2 "1.", max = "999."
  L := EndPtr - StartPtr;
  Result := (L >= 2) and (L < 5);
  if not Result then
    Exit;

  Dec(EndPtr);
  Value := 0;

  while (StartPtr < EndPtr) and (StartPtr^ in cNum) do
  begin
    Value := Value * 10 + Ord(StartPtr^) - Ord('0');
    Inc(StartPtr);
  end;
  Result := (StartPtr = EndPtr) and (StartPtr^ = '.') and (Value = ExpNumber);
end;

//=== { TBaseSymbol } ========================================================

function TBaseSymbol.AllowedInTable: Boolean;
begin
  Result := True;
end;

function TBaseSymbol.CanFormatText: Boolean;
begin
  Result := True;
end;

function TBaseSymbol.CanRemoveFromEnd: Boolean;
begin
  Result := False;
end;

procedure TBaseSymbol.CleanUp;
begin
  { Nothing }
end;

function TBaseSymbol.ConstructCopy: TBaseSymbol;
begin
  Result := TBaseSymbolClass(ClassType).Create as TBaseSymbol;
  Result.Assign(Self);
end;

constructor TBaseSymbol.Create;
begin
  inherited Create;
end;

function TBaseSymbol.IsEmpty: Boolean;
begin
  Result := False;
end;

function TBaseSymbol.NeedParagraphAfter: Boolean;
begin
  Result := False;
end;

function TBaseSymbol.NoSpaceAfter: Boolean;
begin
  Result := False;
end;

function TBaseSymbol.NoSpaceBefore: Boolean;
begin
  Result := False;
end;

procedure TBaseSymbol.SaveDtx(ADtxWriter: TDtxWriter);
begin
  {}
end;

//=== { TCodeSymbol } ========================================================

constructor TCodeSymbol.CreateNew(const ACode: string; const AToken: TSymbolToken);
begin
  Create;
  FCode.Text := ACode;
  FIsCode := AToken <> toPreBegin;
end;

destructor TCodeSymbol.Destroy;
begin
  FCode.Free;
  inherited Destroy;
end;

function TCodeSymbol.AllowedInTable: Boolean;
begin
  Result := False;
end;

procedure TCodeSymbol.Assign(Source: TPersistent);
begin
  if Source is TCodeSymbol then
  begin
    FCode.Assign(TCodeSymbol(Source).FCode);
    FIsCode := TCodeSymbol(Source).FIsCode;
  end
  else
    inherited Assign(Source);
end;

function TCodeSymbol.CanFormatText: Boolean;
begin
  Result := False;
end;

procedure TCodeSymbol.CleanUp;
begin
  while (FCode.Count > 0) and IsNullStr(FCode[0]) do
    FCode.Delete(0);
  while (FCode.Count > 0) and IsNullStr(FCode[FCode.Count - 1]) do
    FCode.Delete(FCode.Count - 1);
end;

function TCodeSymbol.DtxText: string;
begin
  Result := FCode.Text;
end;

procedure TCodeSymbol.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.WriteContent(FCode.Text);
end;

procedure TCodeSymbol.SaveDtx(ADtxWriter: TDtxWriter);
begin
  if FIsCode then
  begin
//    ADtxWriter.WriteLn('<AUTOLINK OFF>');
    ADtxWriter.WriteLn('<CODE>');
  end
  else
    ADtxWriter.WriteLn('<PRE>');

  ADtxWriter.WriteMultiLineData(FCode, False);

  if FIsCode then
  begin
    ADtxWriter.WriteLn('</CODE>');
//    ADtxWriter.WriteLn('<AUTOLINK ON>');
  end
  else
    ADtxWriter.WriteLn('</PRE>');
end;

//=== { TDotItemsSymbol } ====================================================

procedure TDotItemsSymbol.Save(AXMLWriter: TXMLWriter);
var
  I: Integer;
begin
  AXMLWriter.BeginElement('DOTLIST');
  for I := 0 to Count - 1 do
  begin
    AXMLWriter.BeginElement('ITEM');
    Symbols[i].Save(AXMLWriter);
    AXMLWriter.EndElement;
  end;
  AXMLWriter.EndElement;
end;

procedure TDotItemsSymbol.SaveDtx(ADtxWriter: TDtxWriter);
var
  I: Integer;
begin
  for I := 0 to count - 1 do
  begin
    ADtxWriter.Write('* ');
    ADtxWriter.IncIndent(cDefaultIndent);
    Symbols[i].SaveDtx(ADtxWriter);
    ADtxWriter.DecIndent(cDefaultIndent);
  end;
end;

//=== { TDtxList } ===========================================================

constructor TDtxList.Create;
begin
  inherited Create;
  FSkipList := TStringList.Create;
  TStringList(FSkipList).Sorted := True;
  TStringList(FSkipList).Duplicates := dupIgnore;
  FTopics := TObjectLIst.Create;
  FSummary := TSymbolList.Create;
end;

destructor TDtxList.Destroy;
begin
  FSkipList.Free;
  FTopics.Free;
  FSummary.Free;
  inherited Destroy;
end;

function TDtxList.AddComment(AComment: TSimpleDtxComment): Boolean;
var
  Token: string;
  RestIndex: Integer;
  Rest: string;
begin
  Result := True;
  GetFirstToken(AComment.Value, Token, RestIndex);
  Rest := Copy(AComment.Value, RestIndex, MaxInt);
  EatChars(Rest, [SpaceChar, TabChar]);
  if SameText(Token, 'Package:') then
  begin
    if not IsNullStr(Package) then
      ErrorMsg(Format('Duplicate package ignored: %s', [AComment.Value]))
    else
      Package := Rest;
  end
  else
    if SameText(Token, 'Status:') then
  begin
    if not IsNullStr(Status) then
      ErrorMsg(Format('Duplicate status ignored: %s', [AComment.Value]))
    else
      Status := Rest;
  end
  else
    if SameText(Token, 'Skip:') then
    SkipList.Add(Rest)
      { TODO : Uncomment }
//  else
//    WarningMsg(Format('Ignored comment: %s', [AComment.Value]));
end;

function TDtxList.AddItem(ATopic: TDtxTopic): Integer;
begin
  Result := FTopics.Add(ATopic);
end;

function TDtxList.AddPasTopic(ATopic: TSimpleDtxTopic): Boolean;
begin
  FPasTopicRead := True;

  if not IsNullStr(ATopic.Description) then
  begin
    // copy description to summary

    ATopic.Summary := ATopic.Summary + #13#10 + ATopic.Description;
    ATopic.Description := '';
  end;

  PasFileName := ATopic.Name;
  Author := Trim(ATopic.Author);
  Result := Summary.Parse(ATopic.Summary);
  if not Result then
    Exit;

  SeeAlso := ATopic.SeeAlso;

  { TODO }
  //Include := ATopic.Include;

  with ATopic do
  begin
    if not IsNullStr(Note) then
      ErrorMsg(Format('Note is not null: %s', [Note]));
    if not IsNullStr(ReturnValue) then
      ErrorMsg(Format('ReturnValue is not null: %s', [ReturnValue]));
    if not IsNullStr(Description) then
      ErrorMsg(Format('Description is not null: %s', [Description]));
    if not IsNullStr(Parameters) then
      ErrorMsg(Format('Parameters is not null: %s', [Parameters]));
    if not IsNullStr(Group) then
      ErrorMsg(Format('Group is not null: %s', [Group]));
    if not IsNullStr(Flag) then
      ErrorMsg(Format('Flag is not null: %s', [Flag]));
    if not IsNullStr(Alias) then
      ErrorMsg(Format('Alias is not null: %s', [Alias]));
    if not IsNullStr(AliasOf) then
      ErrorMsg(Format('AliasOf is not null: %s', [AliasOf]));
    if not IsNullStr(Combine) then
      ErrorMsg(Format('Combine is not null: %s', [Combine]));
    if not IsNullStr(CombineWith) then
      ErrorMsg(Format('CombineWith is not null: %s', [CombineWith]));
    if not IsNullStr(Title) then
      ErrorMsg(Format('Title is not null: %s', [Title]));
    if not IsNullStr(TitleImg) then
      ErrorMsg(Format('TitleImg is not null: %s', [TitleImg]));
  end;
end;

function TDtxList.AddTopic(ATopic: TSimpleDtxTopic): Boolean;
var
  DtxTopic: TDtxTopic;
begin
  if not FPasTopicRead and ATopic.IsPasTopic then
  begin
    Result := AddPasTopic(ATopic);
    if not Result then
      Exit;
  end
  else
  begin
    DtxTopic := TDtxTopic.Create(Self);
    FTopics.Add(DtxTopic);
    Result := DtxTopic.ReadFrom(ATopic);
    if not Result then
      Exit;
  end;
end;

procedure TDtxList.CleanUp;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Topics[i].CleanUp;
end;

procedure TDtxList.DeleteItem(const Index: Integer);
begin
  FTopics.Delete(Index);
end;

function TDtxList.Find(const S: string; out Index: Integer): Boolean;
begin
  Index := 0;
  while (Index < Count) and not SameText(Topics[Index].Name, S) do
    Inc(Index);
  Result := Index < Count;
end;

function TDtxList.GetCount: Integer;
begin
  Result := FTopics.Count;
end;

function TDtxList.GetTopics(const Index: Integer): TDtxTopic;
begin
  Result := FTopics[Index] as TDtxTopic;
end;

function TDtxList.IsOnSkipList(const AName: string): Boolean;
var
  DotPos: PChar;
  S: string;
begin
  Result := SkipList.IndexOf(AName) >= 0;
  if not Result then
  begin
    DotPos := StrScan(PChar(AName), '.');
    if Assigned(DotPos) then
    begin
      SetString(S, PChar(AName), DotPos - PChar(AName));
      Result := SkipList.IndexOf(S) >= 0;
    end;
  end;
end;

function TDtxList.LoadFromFile(const AFileName: string): Boolean;
var
  SimpleList: TSimpleDtxList;
begin
  SimpleList := TSimpleDtxList.Create;
  try
    Result := SimpleList.LoadFromFile(AFileName);
    if not Result then
      Exit;

    Result := ReadFrom(SimpleList);
    if not Result then
      Exit;
  finally
    SimpleList.Free;
  end;
end;

procedure TDtxList.MoveItemToSubItem(const SourceIndex: Integer;
  ADestTopic: TDtxTopic);
var
  SubTopic: TDtxTopic;
begin
  SubTopic := Topics[SourceIndex];
  if SubTopic = ADestTopic then
    raise Exception.Create('Source = Dest');

  FTopics.OwnsObjects := False;
  try
    FTopics.Delete(SourceIndex);
    ADestTopic.AddSubTopic(SubTopic);
  finally
    FTopics.OwnsObjects := True;
  end;
end;

function TDtxList.ReadFrom(AList: TSimpleDtxList): Boolean;
var
  I: Integer;
begin
  Result := True;
  FPasTopicRead := False;
  for I := 0 to AList.Count - 1 do
    if AList[i] is TSimpleDtxComment then
    begin
      Result := AddComment(TSimpleDtxComment(AList[i]));
      if not Result then
        Exit;
    end
    else
      if AList[i] is TSimpleDtxTopic then
    begin
      Result := AddTopic(TSimpleDtxTopic(AList[i]));
      if not Result then
        Exit;
    end;

  CleanUp;
  RemoveGeneratedText;
end;

procedure TDtxList.RemoveGeneratedText;
const
  cSummary = 'Contains the T%s component.';
var
  I: Integer;
begin
  for I := FSummary.Count - 1 downto 0 do
    if (FSummary[i] is TIncludeSymbol) and
      SameText(TIncludeSymbol(FSummary[i]).FileName, 'JVCL.UnitText.dtx') then
      FSummary.Delete(i);

  FSummary.CheckDefaultText(Format(cSummary, [ChangeFileExt(PasFileName, '')]));

  for I := 0 to Count - 1 do
    Topics[i].RemoveGeneratedText;
end;

function TDtxList.ResolveCombines: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    Result := Topics[i].ResolveCombines;
    if not Result then
      Exit;
  end;
end;

procedure TDtxList.SaveToDtxFile(const AFileName: string; const ForBuild: Boolean);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToDtxStream(Stream, ForBuild);
  finally
    Stream.Free;
  end;
end;

procedure TDtxList.SaveToDtxStream(AStream: TStream; const ForBuild: Boolean);
var
  DtxWriter: TDtxWriter;
  I: Integer;
begin
  Sort;

  DtxWriter := TDtxWriter.Create;
  try
    DtxWriter.DestStream := AStream;
    DtxWriter.ForBuild := ForBuild;

    if not ForBuild then
    begin
      DtxWriter.WriteComment(Format('Package: %s', [Package]));
      DtxWriter.WriteComment(Format('Status: %s', [Status]));
      for I := 0 to SkipList.Count - 1 do
        DtxWriter.WriteComment(Format('Skip: %s', [SkipList[i]]));
    end;
    DtxWriter.WriteSepLine;
    DtxWriter.WritePasHeader(PasFileName, Summary, Author, SeeAlso);
    for I := 0 to Count - 1 do
      Topics[i].SaveDtx(DtxWriter);
  finally
    DtxWriter.Free;
  end;
end;

procedure TDtxList.SaveToXMLFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToXMLStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDtxList.SaveToXMLStream(AStream: TStream);
var
  I: Integer;
  XMLWriter: TXMLWriter;
begin
  XMLWriter := TXMLWriter.Create;
  try
    XMLWriter.DestStream := AStream;
    XMLWriter.WriteIndentData('<?xml version="1.0" encoding="iso-8859-1"?>');
    XMLWriter.BeginElement('DTX');
    XMLWriter.WriteElementContent('FILENAME', PasFileName);
    Summary.SaveTag('SUMMARY', XMLWriter);
    XMLWriter.WriteElementContent('AUTHOR', Author);
    XMLWriter.WriteElementContent('PACKAGE', Package);
    XMLWriter.WriteElementContent('STATUS', Status);
    if SkipList.Count > 0 then
    begin
      XMLWriter.BeginElement('SKIPLIST');
      for I := 0 to SkipList.Count - 1 do
        XMLWriter.WriteElementContent('SKIP', SkipList[i]);
      XMLWriter.EndElement;
    end;
    if Count > 0 then
    begin
      XMLWriter.BeginElement('TOPICS');
      for I := 0 to Count - 1 do
        Topics[i].Save(XMLWriter);
      XMLWriter.EndElement;
    end;
    XMLWriter.EndElement;
  finally
    XMLWriter.Free;
  end;
end;

procedure TDtxList.SetSkipList(Strings: TStrings);
begin
  FSkipList.Assign(Strings);
end;

procedure TDtxList.SetSummary(ASymbolList: TSymbolList);
begin
  if ASymbolList = nil then
    FSummary.Clear
  else
  begin
    FSummary.Free;
    FSummary := ASymbolList.ConstructCopy;
  end;
end;

procedure TDtxList.Sort;
begin
  FTopics.Sort(DtxSortCompore);
end;

//=== { TDtxTable } ==========================================================

constructor TDtxTable.Create;
begin
  inherited Create;

  FHeaders := TStringObjectList.Create;
  FData := TObjectList.Create;
  //  FColIsComputeds := TBits.Create;    // lazy create
  //  FColWidths := TIntegerList.Create;  // lazy create
end;

destructor TDtxTable.Destroy;
begin
  FHeaders.Free;
  FData.Free;
  FColIsComputeds.Free;
  FColWidths.Free;

  inherited Destroy;
end;

procedure TDtxTable.AddToStrings(Strings: TStrings);
var
  ColWidths: TIntegerList;
  ARow: Integer;
begin
  ColWidths := TIntegerList.Create;
  try
    DetermineColWidths(ColWidths);
    WriteHeader(Strings, ColWidths);
    for ARow := 0 to RowCount - 1 do
      WriteRow(Strings, ARow, ColWidths);
  finally
    ColWidths.Free;
  end;
end;

function TDtxTable.AllowedInTable: Boolean;
begin
  Result := False;
end;

procedure TDtxTable.Assign(Source: TPersistent);
var
  ARow, ACol: Integer;
  Src: TDtxTable;
begin
  if Source is TDtxTable then
  begin
    Src := TDtxTable(Source);

    ColCount := Src.ColCount;
    RowCount := Src.RowCount;
    FHeaderChars := Src.FHeaderChars;
    HasHeader := Src.HasHeader;
    for ACol := 0 to ColCount - 1 do
    begin
      if HasHeader then
      begin
        Header[ACol] := Src.Header[ACol];
        if Src.HeaderSymbols[ACol] = nil then
          HeaderSymbols[ACol] := nil
        else
          HeaderSymbols[ACol] := Src.HeaderSymbols[ACol].ConstructCopy;
      end;
      for ARow := 0 to RowCount - 1 do
      begin
        Cells[ACol, ARow] := Src.Cells[ACol, ARow];
        if Src.CellsSymbols[ACol, ARow] = nil then
          CellsSymbols[ACol, ARow] := nil
        else
          CellsSymbols[ACol, ARow] := Src.CellsSymbols[ACol, ARow].ConstructCopy;
      end;
    end;
  end
  else
    inherited Assign(Source);
end;

function TDtxTable.CanFormatText: Boolean;
begin
  Result := False;
end;

procedure TDtxTable.CellStringAsText(const S: string;
  const MaxWidth: Integer; Dest: TStrings);
var
  S1: string;
begin
  S1 := S;
  Wrap(S1, MaxWidth, 0, 2);
  Dest.Text := S1;
end;

function TDtxTable.CheckCells: Boolean;
var
  ACol, ARow: Integer;
  SymbolList: TSymbolList;
  I: Integer;
begin
  Result := True;
  for ACol := 0 to ColCount - 1 do
    for ARow := 0 to RowCount - 1 do
    begin
      SymbolList := CellsSymbols[ACol, ARow];
      if Assigned(SymbolList) then
        for I := 0 to SymbolList.Count - 1 do
          if not SymbolList.Symbols[i].AllowedInTable then
          begin
            Result := False;
            ErrorMsg('Invalid symbol in table');
            Exit;
          end;
    end;
end;

procedure TDtxTable.CheckColIndex(const ACol: integer);
begin
  if (ACol < 0) or (ACol >= ColCount) then
    raise Exception.Create('Invalid column index');
end;

procedure TDtxTable.CheckRowIndex(const ARow: integer);
begin
  if (ARow < 0) or (ARow >= RowCount) then
    raise Exception.Create('Invalid row index');
end;

procedure TDtxTable.CleanUp;
var
  ACol, ARow: Integer;
  SymbolList: TSymbolList;
begin
  for ACol := 0 to ColCount - 1 do
  begin
    SymbolList := HeaderSymbols[ACol];
    if Assigned(SymbolList) then
      SymbolList.CleanUp;

    for ARow := 0 to RowCount - 1 do
    begin
      SymbolList := CellsSymbols[ACol, ARow];
      if Assigned(SymbolList) then
        SymbolList.CleanUp;
    end;
  end;
end;

procedure TDtxTable.ClearCellSymbols;
var
  I: Integer;
begin
  FHeaders.ClearObjects;

  for I := 0 to FData.Count - 1 do
    (FData[i] as TStringObjectList).ClearObjects;
end;

procedure TDtxTable.DetermineColWidths(Integers: TIntegerList);
const
  cMinWidth = 40;
var
  ACol: Integer;
  MaxWordLength: Integer;
  MaxTextLength: Integer;
  TotalLength: Integer;
begin
  Integers.Clear;
  TotalLength := 0;
  for ACol := 0 to ColCount - 1 do
  begin
    MaxTextLength := MaxTextLengthInCol(ACol);
    if MaxTextLength < 1 then
      MaxTextLength := 1;
    if MaxTextLength < cMinWidth then
      Integers.Add(MaxTextLength)
    else
    begin
      MaxWordLength := MaxWordLengthInCol(ACol) + cDividerWidth;
      if ACol = ColCount - 1 then
        // last col
        MaxWordLength := Max(Min(90 - TotalLength, MaxTextLength), MaxWordLength);
      Integers.Add({cDividerWidth +}Max(cMinWidth, MaxWordLength));
    end;

    TotalLength := TotalLength + Integers[Integers.Count - 1] + 2;
  end;
end;

function TDtxTable.DtxText: string;
begin
  Result := Text;
end;

function TDtxTable.GetCells(ACol, ARow: Integer): string;
var
  LRow: TStrings;
begin
  CheckColIndex(ACol);

  LRow := Rows[ARow];
  EnsureMinCount(LRow, ACol + 1);
  Result := LRow[ACol];
end;

function TDtxTable.GetCellsSymbols(ACol, ARow: Integer): TSymbolList;
var
  LRow: TStrings;
begin
  CheckColIndex(ACol);

  LRow := Rows[ARow];
  EnsureMinCount(LRow, ACol + 1);
  Result := LRow.Objects[ACol] as TSymbolList;
end;

function TDtxTable.GetHasDefaultHeaderChars: Boolean;
var
  P: PChar;
begin
  Result := True;
  P := PChar(FHeaderChars);
  while Result and (P^ <> #0) do
  begin
    Result := P^ = '-';
    Inc(P);
  end;
end;

function TDtxTable.GetHeader(ACol: Integer): string;
begin
  CheckColIndex(ACol);
  EnsureMinCount(FHeaders, ACol + 1);
  Result := FHeaders[ACol];
end;

function TDtxTable.GetHeaderChar(const ACol: Integer): Char;
begin
  CheckColIndex(ACol);
  Result := FHeaderChars[ACol + 1];
end;

function TDtxTable.GetHeaderSymbols(ACol: Integer): TSymbolList;
begin
  CheckColIndex(ACol);
  EnsureMinCount(FHeaders, ACol + 1);
  Result := FHeaders.Objects[ACol] as TSymbolList;
end;

function TDtxTable.GetRows(const ARow: Integer): TStrings;
begin
  CheckRowIndex(ARow);
  EnsureMinCount(FData, ARow + 1);
  if FData[ARow] = nil then
    FData[ARow] := TStringObjectList.Create;
  Result := TStrings(FData[ARow]);
end;

function TDtxTable.GetText: string;
var
  Table: TStringList;
  I: Integer;
begin
  Table := TStringList.Create;
  try
    AddToStrings(Table);
    for I := 0 to Table.Count - 1 do
      Table[i] := TrimRight(Table[i]);
    Result := Table.Text;
  finally
    Table.Free;
  end;
end;

function TDtxTable.IsEmpty: Boolean;
begin
  Result := (ColCount = 0) or (RowCount = 0);
end;

function TDtxTable.MaxTextLengthInCol(const ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := 0;
  for ARow := 0 to RowCount - 1 do
    Result := Max(Result, Length(Cells[ACol, ARow]));
  if HasHeader then
    Result := Max(Result, Length(Header[ACol]));
end;

function TDtxTable.MaxWordLengthInCell(const ACol, ARow: Integer): Integer;
begin
  Result := MaxWordLengthInString(Cells[ACol, ARow]);
end;

function TDtxTable.MaxWordLengthInCol(const ACol: Integer): Integer;
var
  ARow: Integer;
begin
  Result := 0;
  for ARow := 0 to RowCount - 1 do
    Result := Max(Result, MaxWordLengthInCell(ACol, ARow));
  if HasHeader then
    Result := Max(Result, MaxWordLengthInString(Header[ACol]));
end;

procedure TDtxTable.MergeRows(const ARowIndex, ADeleteRowIndex: Integer);
var
  ARow, ACol: Integer;
begin
  if ARowIndex = ADeleteRowIndex then
    Exit;

  if ARowIndex > ADeleteRowIndex then
    raise Exception.Create('MergeRows');

  for ACol := 0 to ColCount - 1 do
    Cells[ACol, ARowIndex] := Cells[ACol, ARowIndex] + #13#10 + Cells[ACol, ADeleteRowIndex];

  FData.OwnsObjects := False;
  try
    FData[ADeleteRowIndex].Free;
    FData[ADeleteRowIndex] := nil;
    for ARow := ADeleteRowIndex + 1 to RowCount - 1 do
      FData[ARow - 1] := FData[ARow];
    if ADeleteRowIndex <> RowCount - 1 then
      FData[RowCount - 1] := nil;
    RowCount := RowCount - 1;
  finally
    FData.OwnsObjects := True;
  end;
end;

procedure TDtxTable.MergeStrings(SS1, SS2: TStrings; const ColWidth: Integer);
var
  I: Integer;
  CurrentLength: Integer;
begin
  if SS1.Count = 0 then
  begin
    for I := 0 to SS2.Count - 1 do
      SS1.Add(Pad(SS2[i], ColWidth));
    if SS1.Count = 0 then
      SS1.Add(StringOfChar(SpaceChar, ColWidth));
    Exit;
  end;

  { Assume SS1 strings have all same length }
  CurrentLength := Length(SS1[0]);

  while SS1.Count < SS2.Count do
    SS1.Add(StringOfChar(SpaceChar, CurrentLength));
  for I := 0 to SS2.Count - 1 do
    SS1[i] := SS1[i] + StringOfChar(SpaceChar, cDividerWidth) + Pad(SS2[i], ColWidth);
  for I := SS2.Count to SS1.Count - 1 do
    SS1[i] := SS1[i] + StringOfChar(SpaceChar, ColWidth + cDividerWidth);
end;

function TDtxTable.NeedParagraphAfter: Boolean;
begin
  Result := True;
end;

function TDtxTable.ParseCells: Boolean;
var
  ACol, ARow: Integer;
  S: string;
  SymbolList: TSymbolList;
begin
  Result := True;

  if HasHeader then
    for ACol := 0 to ColCount - 1 do
    begin
      S := Header[ACol];
      if IsNullStr(S) then
        HeaderSymbols[ACol] := nil
      else
      begin
        SymbolList := TSymbolList.Create;
        Result := SymbolList.Parse(S);
        if not Result then
          Exit;
        HeaderSymbols[ACol] := SymbolList;
      end;
    end;

  for ARow := 0 to RowCount - 1 do
    for ACol := 0 to ColCount - 1 do
    begin
      S := Cells[ACol, ARow];
      if IsNullStr(S) then
        CellsSymbols[ACol, ARow] := nil
      else
      begin
        SymbolList := TSymbolList.Create;
        Result := SymbolList.Parse(S);
        if not Result then
          Exit;
        CellsSymbols[ACol, ARow] := SymbolList;
      end;
    end;
end;

function TDtxTable.ReformatCells: Boolean;
var
  ACol, ARow: Integer;
  SymbolList: TSymbolList;
  DtxWriter: TDtxWriter;
  Stream: TStringStream;
  S: string;
begin
  Result := True;

  DtxWriter := TDtxWriter.Create;
  try
    Stream := TStringStream.Create('');
    try
      DtxWriter.DestStream := Stream;
      for ACol := 0 to ColCount - 1 do
        for ARow := 0 to RowCount - 1 do
        begin
          SymbolList := CellsSymbols[ACol, ARow];
          if Assigned(SymbolList) then
          begin
            DtxWriter.Reset;
            Stream.Position := 0;
            SymbolList.SaveDtx(DtxWriter, False);
            S := Stream.DataString;
            SetLength(S, Stream.Position);
            RemoveEndCRLF(S);
            Cells[ACol, ARow] := S;
          end
            //          else
            //            Cells[ACol, ARow] := '';
        end;
    finally
      Stream.Free;
    end;
  finally
    DtxWriter.Free;
  end;
end;

procedure TDtxTable.Save(AXMLWriter: TXMLWriter);
var
  ACol, ARow: Integer;
  SymbolList: TSymbolList;
begin
  AXMLWriter.BeginElement('TABLE', ['cols', 'rows'], [IntToStr(ColCount), IntToStr(RowCount)]);
  if HasHeader then
  begin
    AXMLWriter.BeginElement('HEADER');
    for ACol := 0 to ColCount - 1 do
    begin
      AXMLWriter.BeginElement('COL', ['char'], [HeaderChar[ACol]]);
      SymbolList := HeaderSymbols[ACol];
      if Assigned(SymbolList) then
        SymbolList.Save(AXMLWriter);
      AXMLWriter.EndElement;
    end;
    AXMLWriter.EndElement;
  end;

  for ARow := 0 to RowCount - 1 do
  begin
    AXMLWriter.BeginElement('ROW');
    for ACol := 0 to ColCount - 1 do
    begin
      AXMLWriter.BeginElement('COL');
      SymbolList := CellsSymbols[ACol, ARow];
      if Assigned(SymbolList) then
        SymbolList.Save(AXMLWriter);
      AXMLWriter.EndElement;
    end;
    AXMLWriter.EndElement;
  end;
  AXMLWriter.EndElement;
end;

procedure TDtxTable.SaveDtx(ADtxWriter: TDtxWriter);
begin
  ReformatCells;
  if HasWidthStr then
    ADtxWriter.WriteLnFmt('<TABLE %s>', [WidthStr])
  else
    ADtxWriter.WriteLn('<TABLE>');
  ADtxWriter.IncIndent(2);
  ADtxWriter.WriteMultiLineData(Text, True);
  ADtxWriter.DecIndent(2);
  ADtxWriter.WriteLn('</TABLE>');
end;

procedure TDtxTable.SetCells(ACol, ARow: Integer; const Value: string);
var
  LRow: TStrings;
begin
  CheckColIndex(ACol);

  LRow := Rows[ARow];
  EnsureMinCount(LRow, ACol + 1);
  LRow[ACol] := Value;
end;

procedure TDtxTable.SetCellsSymbols(ACol, ARow: Integer;
  ASymbolList: TSymbolList);
var
  LRow: TStrings;
begin
  CheckColIndex(ACol);

  LRow := Rows[ARow];
  EnsureMinCount(LRow, ACol + 1);
  LRow.Objects[ACol] := ASymbolList;
end;

procedure TDtxTable.SetColCount(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FColCount then
  begin
    SetLength(FHeaderChars, Value);
    for I := FColCount + 1 to Value do
      FHeaderChars[i] := '-';
    // if existing, too big?
    if Assigned(FColWidths) then
      while FColWidths.Count > Value do
        FColWidths.Delete(FColWidths.Count - 1);
    // if existing, too big?
    if Assigned(FColIsComputeds) and (FColIsComputeds.Size > Value) then
      FColIsComputeds.Size := Value;
    FColCount := Value;
    if FColCount <= 1 then
      WarningMsg('ColCount <= 1');
  end;
end;

procedure TDtxTable.SetHeader(ACol: Integer; const Value: string);
begin
  CheckColIndex(ACol);
  EnsureMinCount(FHeaders, ACol + 1);
  FHeaders[ACol] := Value;
end;

procedure TDtxTable.SetHeaderChar(const ACol: Integer; const Value: Char);
begin
  CheckColIndex(ACol);
  FHeaderChars[ACol + 1] := Value;
end;

procedure TDtxTable.SetHeaderSymbols(ACol: Integer;
  ASymbolList: TSymbolList);
begin
  CheckColIndex(ACol);
  EnsureMinCount(FHeaders, ACol + 1);
  FHeaders.Objects[ACol] := ASymbolList;
end;

procedure TDtxTable.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
end;

procedure TDtxTable.WriteHeader(Strings: TStrings;
  ColWidths: TIntegerList);
begin
  if HasHeader then
    WriteKop(Strings, ColWidths);
  if HasHeader or not HasDefaultHeaderChars then
    WriteKopSeparator(Strings, ColWidths);
end;

procedure TDtxTable.WriteKop(Strings: TStrings; ColWidths: TIntegerList);
var
  CellStrings, HeaderStrings: TStringList;
  ACol: Integer;
  S: string;
begin
  CellStrings := TStringList.Create;
  HeaderStrings := TStringList.Create;
  try
    for ACol := 0 to ColCount - 1 do
    begin
      CellStrings.Clear;
      S := Header[ACol];
      // A cell of a table header may not be empty:
      if S = '' then
        S := '\';
      CellStringAsText(S, ColWidths[ACol], CellStrings);
      MergeStrings(HeaderStrings, CellStrings, ColWidths[ACol]);
    end;
    Strings.AddStrings(HeaderStrings);
  finally
    CellStrings.Free;
    HeaderStrings.Free;
  end;
end;

procedure TDtxTable.WriteKopSeparator(Strings: TStrings;
  ColWidths: TIntegerList);
var
  Separator: string;
  ACol: Integer;
begin
  Separator := '';
  for ACol := 0 to ColCount - 1 do
  begin
    Separator := Separator + StringOfChar(HeaderChar[ACol], ColWidths[ACol]);
    if ACol < ColCount - 1 then
      Separator := Separator + '  ';
  end;
  Strings.Add(Separator);
end;

procedure TDtxTable.WriteRow(Strings: TStrings; const ARow: Integer; ColWidths: TIntegerList);
var
  CellStrings, RowStrings: TStringList;
  ACol: Integer;
begin
  CellStrings := TStringList.Create;
  RowStrings := TStringList.Create;
  try
    for ACol := 0 to ColCount - 1 do
    begin
      CellStrings.Clear;
      CellStringAsText(Cells[ACol, ARow], ColWidths[ACol], CellStrings);
      MergeStrings(RowStrings, CellStrings, ColWidths[ACol]);
    end;
    Strings.AddStrings(RowStrings);
  finally
    CellStrings.Free;
    RowStrings.Free;
  end;
end;

//=== { TDtxTopic } ==========================================================

constructor TDtxTopic.Create(AList: TDtxList);
begin
  inherited Create;
  FList := AList;
  FDescription := TSymbolList.Create;
  FSeeAlso := TSymbolList.Create;
  FReturnValue := TSymbolList.Create;
  FNote := TSymbolList.Create;
  FSummary := TSymbolList.Create;
  FParameters := TParameterList.Create;

  FDelphiType := dtOther;
  FMethodType := mtNormal;
  FDirectives := [];
  FHasTocEntry := True;
  FHasPasFileEntry := True;
end;

destructor TDtxTopic.Destroy;
begin
  FDescription.Free;
  FSeeAlso.Free;
  FReturnValue.Free;
  FNote.Free;
  FSummary.Free;
  FParameters.Free;
  FSubTopics.Free;
  DestroyCombineWiths;
  if FCombineTopic <> nil then
    FCombineTopic.RemoveCombineWith(Self);
  inherited Destroy;
end;

function TDtxTopic.AddSubTopic(ASubTopic: TDtxTopic): Integer;
begin
  if not Assigned(FSubTopics) then
    FSubTopics := TObjectList.Create;
  Result := FSubTopics.Add(ASubTopic);
end;

procedure TDtxTopic.CleanUp;
begin
  Summary.CleanUp;
  Note.CleanUp;
  ReturnValue.CleanUp;
  Description.CleanUp;
  Parameters.CleanUp;
  SeeAlso.CleanUp;
end;

function TDtxTopic.Compare(ADtxTopic: TDtxTopic): Integer;
begin
  Result := CompareText(Name, ADtxTopic.Name);
end;

procedure TDtxTopic.DeleteSubTopic(const Index: Integer);
begin
  FSubTopics.Delete(Index);
end;

procedure TDtxTopic.DestroyCombineWiths;
begin
  while FCombineWiths <> nil do
    RemoveCombineWith(TDtxTopic(FCombineWiths.Last));
end;

function TDtxTopic.GetCombineWith(AIndex: Integer): TDtxTopic;
begin
  if FCombineWiths = nil then
    TList.Error(@SListIndexError, AIndex);
  Result := FCombineWiths[AIndex];
end;

function TDtxTopic.GetCombineWithCount: Integer;
begin
  if FCombineWiths <> nil then
    Result := FCombineWiths.Count
  else
    Result := 0;
end;

function TDtxTopic.GetHasJVCLInfo: Boolean;
begin
  Result := not IsNullStr(Group) or (Flags <> []);
end;

function TDtxTopic.GetIsDocumented: Boolean;
var
  I: Integer;
begin
  Result :=
    not Summary.IsEmpty or
    not Note.IsEmpty or
    not ReturnValue.IsEmpty or
    not Description.IsEmpty or
    not Parameters.IsEmpty or
    not SeeAlso.IsEmpty or
    not IsNullStr(Alias) or
    not IsNullStr(AliasOf) or
    not IsNullStr(Combine) or
    not IsNullStr(CombineWith) or
    (Flags <> []) or
    not IsNullStr(Group) or
    not HasTocEntry or
    not IsNullStr(Title) or
    not IsNullStr(Donator) or
    not IsNullStr(Group2) or
    not IsNullStr(TitleImg);

  if not Result and (SubTopicsCount > 0) then
    for I := 0 to SubTopicsCount - 1 do
    begin
      Result := SubTopics[I].IsDocumented;
      if Result then
        Exit;
    end;
end;

function TDtxTopic.GetSubTopics(const Index: Integer): TDtxTopic;
begin
  if Assigned(FSubTopics) then
    Result := FSubTopics[Index] as TDtxTopic
  else
    raise Exception.Create('Invalid index');
end;

function TDtxTopic.GetSubTopicScount: Integer;
begin
  if Assigned(FSubTopics) then
    Result := FSubTopics.Count
  else
    Result := 0;
end;

procedure TDtxTopic.InsertCombineWith(ATopic: TDtxTopic);
begin
  if FCombineWiths = nil then
    FCombineWiths := TList.Create;
  FCombineWiths.Add(ATopic);
  ATopic.FCombineTopic := Self;
end;

procedure TDtxTopic.MoveSubTopic(const SourceIndex, DestIndex: Integer);
var
  Obj: TObject;
begin
  FSubTopics.OwnsObjects := False;
  try
    if SourceIndex > DestIndex then
    begin
      Obj := FSubTopics[SourceIndex];
      FSubTopics.Delete(SourceIndex);
      FSubTopics.Insert(DestIndex, Obj);
    end
    else
      if SourceIndex < DestIndex then
    begin
      Obj := FSubTopics[SourceIndex];
      FSubTopics.Delete(SourceIndex);
      FSubTopics.Insert(DestIndex - 1, Obj);
    end;
  finally
    FSubTopics.OwnsObjects := True;
  end;
end;

function TDtxTopic.ReadFrom(ATopic: TSimpleDtxTopic): Boolean;
begin
  Name := ATopic.Name;
  Result := Summary.Parse(ATopic.Summary);
  if not REsult then
    Exit;

  Result := Note.Parse(ATopic.Note);
  if not REsult then
    Exit;

  Result := ReturnValue.Parse(ATopic.ReturnValue);
  if not REsult then
    Exit;

  Result := Description.Parse(ATopic.Description);
  if not REsult then
    Exit;

  Result := FParameters.Parse(ATopic.Parameters);
  if not REsult then
    Exit;

  Result := SeeAlso.Parse(ATopic.SeeAlso);
  if not REsult then
    Exit;

  Group := Trim(ATopic.Group);
  Flags := ParseCompSettings(ATopic.Flag);
  //  Include := Trim(ATopic.Include);
  Alias := Trim(ATopic.Alias);
  AliasOf := Trim(ATopic.AliasOf);
  Combine := Trim(ATopic.Combine);
  CombineWith := Trim(ATopic.CombineWith);
  Title := Trim(ATopic.Title);
  Donator := Trim(ATopic.Donator);
  TitleImg := Trim(ATopic.TitleImg);
  Group2 := Trim(ATopic.Group2);
  TopicOrder := Trim(ATopic.TopicOrder);
  if not IsNullStr(ATopic.HasTocEntry) then
    HasTocEntry := SameText(Trim(ATopic.HasTocEntry), 'on')
  else
    HasTocEntry := True;
  if not IsNullStr(ATopic.HasPasFileEntry) then
    HasPasFileEntry := SameText(Trim(ATopic.HasPasFileEntry), 'on')
  else
    HasPasFileEntry := True;
end;

procedure TDtxTopic.RemoveCombineWith(ATopic: TDtxTopic);
begin
  ATopic.FCombineTopic := nil;
  FCombineWiths.Remove(ATopic);
  if FCombineWiths.Count = 0 then
  begin
    FCombineWiths.Free;
    FCombineWiths := nil;
  end;
end;

procedure TDtxTopic.RemoveGeneratedText;
var
  S: string;
  DotPos: PChar;
begin
  Summary.CheckDefaultText(cSummaryDefaultText);
  Description.CheckDefaultText(cDescriptionDefaultText);
  Description.CheckDefaultText(cDescriptionOverrideDefaultText);
  Description.CheckDefaultText(cDescriptionOverloadDefaultText);
  Parameters.CheckDefaultText(cParameterDefaultText);
  ReturnValue.CheckDefaultText(cReturnValueDefaultText);
  SeeAlso.CheckDefaultText(cSeeAlsoDefaultText);
  if not IsNullStr(Title) then
  begin
    if SameText(Title, Format('%s function', [Name])) then
      Title := ''
    else
      if SameText(Title, Format('%s procedure', [Name])) then
      Title := ''
    else
      if SameText(Title, Format('%s type', [Name])) then
      Title := ''
    else
      if SameText(Title, Format('%s variable', [Name])) then
      Title := '';
  end;

  DotPos := StrScan(PChar(Name), '.');
  if Assigned(DotPos) then
  begin
    Inc(DotPos);
    SetString(S, DotPos, PChar(Name) + Length(Name) - DotPos);
    Summary.CheckDefaultText(Format(cEnumerateDefaultText, [S]));
  end;
end;

function TDtxTopic.ResolveCombines: Boolean;
var
  Index: Integer;
begin
  if IsNullStr(CombineWith) and IsNullStr(Combine) then
    Result := True
  else
  begin
    if not IsNullStr(CombineWith) then
    begin
      Result := FList.Find(CombineWith, Index);
      if not Result then
      begin
        ErrorMsgFmt('COMBINEWITH: %s is not a topic', [CombineWith]);
        Exit;
      end;

      InsertCombineWith(FList.Topics[Index]);
    end
    else
    if not IsNullStr(Combine) then
    begin
      Result := FList.Find(Combine, Index);
      if not Result then
      begin
        ErrorMsgFmt('COMBINE: %s is not a topic (%s)', [Combine, Self.Name]);
        Exit;
      end;

      FList.Topics[Index].InsertCombineWith(Self);
    end
    else
    begin
      Result := False;
      ErrorMsg('COMBINEWITH and COMBINE filled');
      Exit;
    end;
  end;
end;

procedure TDtxTopic.Save(AXMLWriter: TXMLWriter);
var
  Directive: TDirective;
  I: Integer;
begin
  AXMLWriter.BeginElement('TOPIC');
  AXMLWriter.WriteElementContentNE('NAME', Name);
  AXMLWriter.WriteElementContentNE('TITLE', Title);
  AXMLWriter.WriteElementContentNE('TITLEIMG', TitleImg);
  AXMLWriter.WriteElementContentNE('COMBINE', Combine);
  AXMLWriter.WriteElementContentNE('COMBINEWITH', CombineWith);
  if not HasTocEntry then
    AXMLWriter.WriteEmptyElement('NOTOCENTRY');
  if not HasPasFileEntry then
    AXMLWriter.WriteEmptyElement('NOTINPASFILE');
  AXMLWriter.WriteElementContentNE('GROUP', Group);
  AXMLWriter.WriteElementContentNE('FLAG', CompFlagsToStr(Flags));
  //  AXMLWriter.WriteElementContentNE('INCLUDE', Include);
  AXMLWriter.WriteElementContentNE('ALIAS', Alias);
  AXMLWriter.WriteElementContentNE('ALIASOF', AliasOf);
  AXMLWriter.WriteElementContentNE('DONATOR', Donator);

  { delphi stuff }
  if DelphiType <> dtOther then
    AXMLWriter.WriteElementContentNE('TYPE', cXMLDelphiType[DelphiType]);
  if Directives <> [] then
  begin
    AXMLWriter.BeginElement('DIRECTIVES');
    for Directive := Low(TDirective) to High(TDirective) do
      if Directive in Directives then
        AXMLWriter.WriteElementContentNE('DIRECTIVE', cXMLDirective[Directive]);
    AXMLWriter.EndElement;
  end;

  Summary.SaveTag('SUMMARY', AXMLWriter);
  Description.SaveTag('DESCRIPTION', AXMLWriter);
  Parameters.Save(AXMLWriter);
  ReturnValue.SaveTag('RETURNVALUE', AXMLWriter);
  Note.SaveTag('NOTE', AXMLWriter);
  SeeAlso.SaveTag('SEEALSO', AXMLWriter);

  if SubTopicsCount > 0 then
  begin
    AXMLWriter.BeginElement('SUBTOPICS');
    for I := 0 to SubTopicsCount - 1 do
      SubTopics[i].Save(AXMLWriter);
    AXMLWriter.EndElement;
  end;

  AXMLWriter.EndElement;
end;

procedure TDtxTopic.SaveDtx(ADtxWriter: TDtxWriter);
var
  I: Integer;
begin
  ADtxWriter.WriteSepLine;
  ADtxWriter.WriteTopicName(Name);
  if SameText(AfterDot(Name), 'AboutJVCL') then
  begin
    ADtxWriter.WriteLn('<INCLUDE JVCL.Main.AboutJVCL.dtx>');
    ADtxWriter.WriteLn;
    Exit;
  end;

  if not IsNullStr(Title) then
    ADtxWriter.WriteTitle(Title);
  if not IsNullStr(TitleImg) then
  begin
    if ADtxWriter.ForBuild then
      ADtxWriter.WriteLnFmt('<TITLEIMG TITLEIMG_%s>', [TitleImg])
    else
      ADtxWriter.WriteLnFmt('<TITLEIMG %s>', [TitleImg]);
  end;
  if Assigned(CombineTopic) then
    ADtxWriter.WriteLnFmt('<COMBINE %s>', [CombineTopic.Name]);
  if not IsNullStr(Alias) then
    ADtxWriter.WriteLnFmt('<ALIAS %s>', [Alias]);
  if not HasTocEntry then
    ADtxWriter.WriteLn('<HASTOCENTRY OFF>');
  if not HasPasFileEntry then
    ADtxWriter.WriteLn('<HASPASFILEENTRY OFF>');
  if not IsNullStr(Group2) then
    ADtxWriter.WriteLnFmt('<GROUP %s>', [Group2]);
  if not IsNullStr(TopicOrder) then
    ADtxWriter.WriteLnFmt('<TOPICORDER %s>', [TopicOrder]);

  if ADtxWriter.ForBuild then
  begin
    if IsNullStr(Combine) and IsNullStr(Alias) then
    begin
      ADtxWriter.BeginSection('EditLink');
      ADtxWriter.WriteLnFmt(cEditLink, [StripOverloadAt(Name), 'Edit']);
      ADtxWriter.EndSection;
    end;
  end;

  if HasJVCLInfo then
  begin
    if ADtxWriter.ForBuild then
    begin
      ADtxWriter.BeginSection('Package');
      ADtxWriter.WriteLnFmt('<LINK %s,%s>', [PackageToTopicName(FList.Package), FList.Package]);
      ADtxWriter.EndSection;
    end
    else
    begin
      ADtxWriter.BeginSection('JVCLInfo');
      if not IsNullStr(Group) then
        ADtxWriter.WriteLnFmt('GROUP=%s', [Group]);
      if Flags <> [] then
        ADtxWriter.WriteLnFmt('FLAG=%s', [CompFlagsToStr(Flags)]);
      ADtxWriter.EndSection;
    end;
  end;

  ADtxWriter.WriteTopicItem('Summary', Summary);
  ADtxWriter.WriteTopicItem('Description', Description);
  ADtxWriter.WriteTopicItem('Note', Note);
  Parameters.SaveDtx(ADtxWriter);
  ADtxWriter.WriteTopicItem('Return value', ReturnValue);
  ADtxWriter.WriteTopicItem('See Also', SeeAlso);
  if not IsNullStr(Donator) then
  begin
  ADtxWriter.BeginSection('Donator:');
  ADtxWriter.WriteWrappedData(Donator);
  ADtxWriter.EndSection;
  end;

  //    property Alias: string read FAlias write FAlias;
  //    property AliasOf: string read FAliasOf write FAliasOf;
  //    property Combine: string read FCombine write FCombine;
  //    property CombineWith: string read FCombineWith write FCombineWith;
  //    property Flag: string read FFlag write FFlag;
  //    property Group: string read FGroup write FGroup;
  //    property HasTocEntry: Boolean read FHasTocEntry write FHasTocEntry;
  //    property Include: string read FInclude write FInclude;
  //    property DelphiType: TDelphiType read FDelphiType write FDelphiType;
  //    property Directives: TDirectives read FDirectives write FDirectives;
  //
  //    property SubTopics[const Index: Integer]: TDtxTopic read GetSubTopics;
  //    property SubTopicsCount: Integer read GetSubTopicScount;

  //    property CombineTopic: TDtxTopic read FCombineTopic;
  //    property CombineWiths[Index: Integer]: TDtxTopic read GetCombineWith;
  //    property CombineWithCount: Integer read GetCombineWithCount;
  ADtxWriter.WriteLn;

  for I := 0 to SubTopicsCount - 1 do
  begin
    ADtxWriter.WriteSepLine;
    ADtxWriter.WriteTopicName(Self.Name + '.' + SubTopics[i].Name);
    ADtxWriter.IncIndent(cDefaultIndent);
    SubTopics[i].Summary.SaveDtx(ADtxWriter);
    ADtxWriter.DecIndent(cDefaultIndent);
    ADtxWriter.WriteLn;
  end;
end;

//=== { TDtxWriter } =========================================================

procedure TDtxWriter.BeginSection(const ASection: string);
begin
  WriteLn(ASection);
  IncIndent(cDefaultIndent);
end;

procedure TDtxWriter.DecIndent(const Count: Integer);
begin
  Dec(FIndent, Count);
  if FIndent < 0 then
    FIndent := 0;
end;

procedure TDtxWriter.EndSection;
begin
  DecIndent(cDefaultIndent);
end;

procedure TDtxWriter.IncIndent(const Count: Integer);
begin
  Inc(FIndent, Count);
  if FIndent < 0 then
    FIndent := 0;
end;

procedure TDtxWriter.Reset;
begin
  FIndent := 0;
  FBeginOfLine := True;
end;

procedure TDtxWriter.Write(Data: PChar; Length: Integer);
begin
  if FBeginOfLine then
  begin
    FBeginOfLine := False;
    Write(StringOfChar(' ', FIndent));
  end;
  FDestStream.Write(Data^, Length);
end;

procedure TDtxWriter.Write(const Data: string);
begin
  Write(PChar(Data), Length(Data));
end;

procedure TDtxWriter.WriteComment(const AComment: string);
begin
  WriteLn('##' + AComment);
end;

procedure TDtxWriter.WriteLn(const Data: string);
begin
  WriteLn(PChar(Data), Length(Data));
end;

procedure TDtxWriter.WriteLn(Data: PChar; Length: Integer);
begin
  { TODO : Dirty }
  if (Data^ = #0) and FBeginOfLine then
    FBeginOfLine := False;
  // skip end spaces
  while (Length > 0) and ((Data + Length - 1)^ = ' ') do
    Dec(Length);
  Write(Data, Length);
  Write(#13#10, 2);
  FBeginOfLine := True;
end;

procedure TDtxWriter.WriteLnFmt(const AFormat: string;
  const Args: array of const);
begin
  WriteLn(Format(AFormat, Args));
end;

procedure TDtxWriter.WriteMultiLineData(const Data: string; const DoIndent: Boolean);
var
  SS: TStrings;
begin
  SS := TStringList.Create;
  try
    SS.Text := Data;
    WriteMultiLineData(SS, DoIndent);
  finally
    SS.Free;
  end;
end;

procedure TDtxWriter.WriteMultiLineData(SS: TStrings; const DoIndent: Boolean);
var
  I: Integer;
  SavedIndent: Integer;
begin
  SavedIndent := FIndent;
  if not DoIndent then
    FIndent := 0;
  try
    for I := 0 to SS.Count - 1 do
      WriteLn(SS[I]);
  finally
    if not DoIndent then
      FIndent := SavedIndent;
  end;
end;

procedure TDtxWriter.WritePasHeader(const APasFileName: string;
  ASummary: TSymbolList; const AAuthor, ASeeAlso: string);
begin
  WriteTopicName(APasFileName);
  WriteTopicItem('Summary', ASummary);
  WriteLn('<INCLUDE JVCL.UnitText.dtx>');

  BeginSection('Author');
  WriteWrappedData(AAuthor);
  EndSection;

  if not IsNullStr(ASeeALso) then
  begin
    BeginSection('See Also');
    WriteWrappedData(ASeeAlso);
    EndSection;
  end;
  WriteLn;
end;

procedure TDtxWriter.WriteSepLine;
begin
  { TODO : 100 -> Const }
  WriteLn(StringOfChar('-', 100));
end;

procedure TDtxWriter.WriteTitle(const ATitle: string);
begin
  // Need to escape , thus
  //    Libraries, Processes and Threads ->
  //    Libraries\, Processes and Threads
  WriteLnFmt('<TITLE %s>', [ATitle])
end;

procedure TDtxWriter.WriteTopicItem(const AHeader: string;
  ASymbolList: TSymbolList);
begin
  if not ASymbolList.IsEmpty then
  begin
    BeginSection(AHeader);
    ASymbolList.SaveDtx(Self);
    EndSection;
  end;
end;

procedure TDtxWriter.WriteTopicName(const ATopicName: string);
begin
  WriteLn('@@' + ATopicName);
end;

procedure TDtxWriter.WriteWrappedData(const S: string);
const
  cMaxLength = 103;
var
  P, Q: PChar;
  R: PChar;
  MaxLength: Integer;
begin
  Q := PChar(S);
  R := PChar(S) + Length(S);
  MaxLength := cMaxLength - FIndent;
  if MaxLength < 40 then
    MaxLength := 40;

  while Q < R do
  begin
    P := Q + MaxLength;
    if P > R then
      P := R
    else
    begin
      while (P > Q) and (P^ <> ' ') do
        Dec(P);
      if P = Q then
      begin
        P := Q + MaxLength;
        while (P < R) and (P^ <> ' ') do
          Inc(P);
      end;
    end;
    WriteLn(Q, P - Q);
    Q := P + 1;
  end;
end;

//=== { TExtLinkSymbol } =====================================================

constructor TExtLinkSymbol.CreateNew(const ALink: string);
begin
  Create;
  FLink := ALink;
end;

procedure TExtLinkSymbol.Assign(Source: TPersistent);
begin
  if Source is TExtLinkSymbol then
    FLink := TExtLinkSymbol(Source).Link
  else
    inherited Assign(Source);
end;

function TExtLinkSymbol.DtxText: string;
begin
  Result := Format('<EXTLINK %s>', [Link]);
end;

function TExtLinkSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(Link);
end;

function TExtLinkSymbol.NoSpaceAfter: Boolean;
begin
  Result := True;
end;

procedure TExtLinkSymbol.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.BeginElement('EXTLINK', ['dest'], [Link]);
end;

//=== { TImageSymbol } =======================================================

constructor TImageSymbol.CreateNew(const AImage: string);
begin
  Create;
  FImage := AImage;
end;

procedure TImageSymbol.Assign(Source: TPersistent);
begin
  if Source is TImageSymbol then
    FImage := TImageSymbol(Source).FImage
  else
    inherited Assign(Source);
end;

function TImageSymbol.DtxText: string;
begin
  Result := Format('<IMAGE %s>', [Image]);
end;

function TImageSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(Image);
end;

procedure TImageSymbol.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.WriteElementAttribute('IMAGE', 'dest', Image)
end;

//=== { TIncludeSymbol } =====================================================

constructor TIncludeSymbol.CreateNew(const AFileName: string);
begin
  Create;
  FFileName := AFileName;
end;

procedure TIncludeSymbol.Assign(Source: TPersistent);
begin
  if Source is TIncludeSymbol then
    FFileName := TIncludeSymbol(Source).FFileName
  else
    inherited Assign(Source);
end;

function TIncludeSymbol.DtxText: string;
begin
  Result := Format('<INCLUDE %s>', [FileName]);
end;

function TIncludeSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(FileName);
end;

procedure TIncludeSymbol.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.WriteElementAttribute('INCLUDE', 'dest', FileName)
end;

//=== { TItemsSymbol } =======================================================

constructor TItemsSymbol.Create;
begin
  inherited Create;
  FSymbols := TObjectList.Create;
end;

destructor TItemsSymbol.Destroy;
begin
  FSymbols.Free;
  inherited Destroy;
end;

function TItemsSymbol.Add(const S: string): Boolean;
var
  SymbolParser: TSymbolParser;
  SymbolList: TSymbolList;
begin
  SymbolList := TSymbolList.Create;
  FSymbols.Add(SymbolList);
  SymbolParser := TSymbolParser.Create(S);
  try
    SymbolParser.List := SymbolList;
    Result := SymbolParser.Parse;
  finally
    SymbolParser.Free;
  end;
end;

function TItemsSymbol.AllowedInTable: Boolean;
begin
  Result := False;
end;

procedure TItemsSymbol.Assign(Source: TPersistent);
var
  I: Integer;
  Src: TItemsSymbol;
begin
  if Source is TItemsSymbol then
  begin
    Src := TItemsSymbol(Source);

    FSymbols.Clear;
    for I := 0 to Src.Count - 1 do
      if Src.FSymbols[i] is TBaseSymbol then
        FSymbols.Add(TBaseSymbol(Src.FSymbols[i]).ConstructCopy);
  end
  else
    inherited Assign(Source);
end;

function TItemsSymbol.CanFormatText: Boolean;
begin
  Result := False;
end;

procedure TItemsSymbol.CleanUp;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Symbols[I].CleanUp;
end;

function TItemsSymbol.DtxText: string;
begin
  Result := '';
end;

function TItemsSymbol.GetCount: Integer;
begin
  Result := FSymbols.Count;
end;

function TItemsSymbol.GetSymbols(const Index: Integer): TSymbolList;
begin
  Result := FSymbols[Index] as TSymbolList;
end;

function TItemsSymbol.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TItemsSymbol.NeedParagraphAfter: Boolean;
begin
  Result := True;
end;

//=== { TLinkSymbol } ========================================================

constructor TLinkSymbol.CreateNew(const ALink, ADescription: string);
begin
  Create;
  FLink := ALink;
  FDescription := ADescription;
end;

procedure TLinkSymbol.Assign(Source: TPersistent);
begin
  if Source is TLinkSymbol then
  begin
    FLink := TLinkSymbol(Source).FLink;
    FDescription := TLinkSymbol(Source).FDescription;
  end
  else
    inherited Assign(Source);
end;

function TLinkSymbol.DtxText: string;
begin
  if not IsNullStr(Description) then
    Result := Format('<LINK %s, %s>', [Link, Description])
  else
    Result := Format('<LINK %s>', [Link]);
end;

function TLinkSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(Link) and IsNullStr(Description);
end;

procedure TLinkSymbol.Save(AXMLWriter: TXMLWriter);
begin
  if IsNullStr(Description) then
    AXMLWriter.WriteElementAttribute('LINK', 'dest', Link)
  else
    AXMLWriter.WriteElementAttributeContent('LINK', ['dest'], [Link], Description);
end;

//=== { TDelphiLinkSymbol } ========================================================

constructor TDelphiLinkSymbol.CreateNew(const ALink, ADescription: string);
begin
  Create;
  FLink := ALink;
  FDescription := ADescription;
end;

procedure TDelphiLinkSymbol.Assign(Source: TPersistent);
begin
  if Source is TDelphiLinkSymbol then
  begin
    FLink := TDelphiLinkSymbol(Source).FLink;
    FDescription := TDelphiLinkSymbol(Source).FDescription;
  end
  else
    inherited Assign(Source);
end;

function TDelphiLinkSymbol.DtxText: string;
begin
  if not IsNullStr(Description) then
    Result := Format('<DELPHILINK %s, %s>', [Link, Description])
  else
    Result := Format('<DELPHILINK %s>', [Link]);
end;

function TDelphiLinkSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(Link) and IsNullStr(Description);
end;

procedure TDelphiLinkSymbol.Save(AXMLWriter: TXMLWriter);
begin
  if IsNullStr(Description) then
    AXMLWriter.WriteElementAttribute('DELPHILINK', 'dest', Link)
  else
    AXMLWriter.WriteElementAttributeContent('DELPHILINK', ['dest'], [Link], Description);
end;

//=== { TNumberItemsSymbol } =================================================

procedure TNumberItemsSymbol.Save(AXMLWriter: TXMLWriter);
var
  I: Integer;
begin
  AXMLWriter.BeginElement('NUMBERLIST');
  for I := 0 to Count - 1 do
    Symbols[i].Save(AXMLWriter);
  AXMLWriter.EndElement;
end;

procedure TNumberItemsSymbol.SaveDtx(ADtxWriter: TDtxWriter);
var
  I: Integer;
  CharLength: Integer;
  S: string;
begin
  if Count = 0 then Exit;
  
  CharLength := 3 + Trunc(Log10(Count)); // dot + space
  for I := 0 to Count - 1 do
  begin
    S := IntToStr(I + 1) + '.';
    ADtxWriter.Write(S + StringOfChar(' ', CharLength - Length(S)));
    ADtxWriter.IncIndent(CharLength);
    Symbols[i].SaveDtx(ADtxWriter);
    ADtxWriter.DecIndent(CharLength);
  end;
end;

//=== { TParameterList } =====================================================

constructor TParameterList.Create;
begin
  inherited Create;
  FParamNames := TStringList.Create;
  FParams := TObjectList.Create;
  FParamStartsWithBackslash := TList.Create;
end;

destructor TParameterList.Destroy;
begin
  FParamNames.Free;
  FParams.Free;
  FParamStartsWithBackslash.Free;
  inherited Destroy;
end;

function TParameterList.Add(AParamName: string;
  AParams: TSymbolList): Integer;
var
  StartsWithBackslash: Boolean;
begin
  AParamName := Trim(AParamName);
  StartsWithBackslash := (AParamName > '') and (AParamName[1] = '\');
  if StartsWithBackslash then
    AParamName := Copy(AParamName, 2, MaxInt);
  if IsNullStr(AParamName) then
    ErrorMsg('Adding empty parameter');
  Result := FParamNames.Add(AParamName);
  FParamStartsWithBackslash.Add(Pointer(StartsWithBackslash));
  FParams.Add(AParams);
end;

procedure TParameterList.CheckDefaultText(const S: string);
var
  I: integer;
begin
  for I := Count - 1 downto 0 do
  begin
    Params[I].CheckDefaultText(S);
    //    if Params[i].Count = 0 then
    //    begin
    //      FParams.Delete(i);
    //      FParamNames.Delete(i);
    //      FParamStartsWithBackslash.Delete(I);
    //    end;
  end;
end;

procedure TParameterList.CleanUp;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Params[i].CleanUp;
end;

procedure TParameterList.Delete(const Index: Integer);
begin
  FParams.Delete(Index);
  FParamNames.Delete(Index);
  FParamStartsWithBackslash.Delete(Index);
end;

function TParameterList.GetCount: Integer;
begin
  Result := FParamNames.Count;
end;

function TParameterList.GetIsEmpty: Boolean;
var
  I: INteger;
begin
  Result := Count = 0;
  if not Result then
  begin
    for I := 0 to Count - 1 do
      if not Params[i].IsEmpty then
        Exit;
    Result := True;
  end;
end;

function TParameterList.GetParamName(const Index: Integer): string;
begin
  Result := FParamNames[Index];
end;

function TParameterList.GetParamNameWithBackSlash(
  const Index: Integer): string;
begin
  Result := ParamName[Index];
  if ParamStartsWithBackslash[Index] then
    Result := '\' + Result;
end;

function TParameterList.GetParams(const Index: Integer): TSymbolList;
begin
  Result := FParams[Index] as TSymbolList;
end;

function TParameterList.GetParamStartsWithBackslash(const Index: Integer): Boolean;
begin
  Result := Boolean(FParamStartsWithBackslash[Index]);
end;

function TParameterList.IndexOf(const AParamName: string): Integer;
begin
  Result := FParamNames.IndexOf(AParamName);
end;

procedure TParameterList.MoveParam(const SourceIndex, DestIndex: Integer);
var
  Obj: TObject;
  S: string;
  B: Boolean;
begin
  FParams.OwnsObjects := False;
  try
    if SourceIndex > DestIndex then
    begin
      Obj := FParams[SourceIndex];
      FParams.Delete(SourceIndex);
      FParams.Insert(DestIndex, Obj);

      S := FParamNames[SourceIndex];
      FParamNames.Delete(SourceIndex);
      FParamNames.Insert(DestIndex, S);

      B := ParamStartsWithBackslash[SourceIndex];
      FParamStartsWithBackslash.Delete(SourceIndex);
      FParamStartsWithBackslash.Insert(DestIndex, Pointer(B));
    end
    else
      if SourceIndex < DestIndex then
    begin
      Obj := FParams[SourceIndex];
      FParams.Delete(SourceIndex);
      FParams.Insert(DestIndex - 1, Obj);

      S := FParamNames[SourceIndex];
      FParamNames.Delete(SourceIndex);
      FParamNames.Insert(DestIndex - 1, S);

      B := ParamStartsWithBackslash[SourceIndex];
      FParamStartsWithBackslash.Delete(SourceIndex);
      FParamStartsWithBackslash.Insert(DestIndex, Pointer(B));
    end;
  finally
    FParams.OwnsObjects := True;
  end;
end;

function TParameterList.Parse(const S: string): Boolean;
var
  Parser: TParamParser;
begin
  if IsNullStr(S) then
  begin
    Result := True;
    Exit;
  end;

  Parser := TParamParser.Create(S);
  try
    Parser.List := Self;
    Result := Parser.Parse;
  finally
    Parser.Free;
  end;
end;

procedure TParameterList.Save(AXMLWriter: TXMLWriter);
var
  I: Integer;
begin
  if Count < 1 then
    Exit;

  AXMLWriter.BeginElement('PARAMETERS');
  for I := 0 to Count - 1 do
  begin
    AXMLWriter.BeginElement('PARAMETER', ['name'], [ParamName[i]]);
    Params[i].Save(AXMLWriter);
    AXMLWriter.EndElement;
  end;
  AXMLWriter.EndElement;
end;

procedure TParameterList.SaveDtx(ADtxWriter: TDtxWriter);
var
  MaxLength: Integer;
  I: Integer;
begin
  if Count = 0 then
    Exit;
  ADtxWriter.BeginSection('Parameters');
  MaxLength := 0;
  for I := 0 to Count - 1 do
    MaxLength := Max(MaxLength, Length(ParamNameWithBackSlash[i]));
  for I := 0 to Count - 1 do
  begin
    ADtxWriter.Write(ExpString(ParamNameWithBackSlash[i], MaxLength) + ' - ');
    ADtxWriter.IncIndent(MaxLength + 3 + 1); // ' - ' = 3, plus extra space
    Params[i].SaveDtx(ADtxWriter);
    ADtxWriter.DecIndent(MaxLength + 3 + 1); // ' - ' = 3, plus extra space
  end;
  ADtxWriter.EndSection;
end;

//=== { TParamParser } =======================================================

constructor TParamParser.Create(const S: string);
begin
  inherited Create;
  FLines := TStringList.Create;
  FLines.Text := S;
end;

destructor TParamParser.Destroy;
begin
  FLines.Free;
  FInternalList.Free;
  inherited Destroy;
end;

function TParamParser.GetList: TParameterList;
begin
  Result := FList;
  if Result = nil then
  begin
    if FInternalList = nil then
      FInternalList := TParameterList.Create;
    Result := FInternalList;
  end;
end;

function TParamParser.Parse: Boolean;
var
  I: Integer;
  RestIndex: Integer;
  AParamName, NewParamName: string;
  AParamDesc: string;
  SymbolList: TSymbolList;
begin
  i := 0;
  Result := (FLines.Count > 0) and IsParam(FLines[0]);
  if not Result then
  begin
    ErrorMsg('ParamParser: count = 0 or no starting param line');
    if FLines.Count > 0 then
      ErrorMsgFmt('<%s>', [FLines[0]]);
    Exit;
  end;
  AParamName := '';
  AParamDesc := '';
  while i < FLines.Count do
  begin
    if IsParam(FLines[i], NewParamName, RestIndex) then
    begin
      if AParamName > '' then
      begin
        SymbolList := TSymbolList.Create;
        Result := SymbolList.Parse(AParamDesc);
        if not REsult then
          Exit;

        List.Add(AParamName, SymbolList);
      end;
      AParamName := NewParamName;
      AParamDesc := Copy(FLines[i], RestIndex, MaxInt);
    end
    else
    begin
      AParamDesc := AParamDesc + FLines[i];
    end;
    Inc(I);
  end;

  if AParamName > '' then
  begin
    SymbolList := TSymbolList.Create;
    Result := SymbolList.Parse(AParamDesc);
    if not REsult then
      Exit;

    List.Add(AParamName, SymbolList);
  end;
end;

//=== { TSimpleSymbol } ======================================================

constructor TSimpleSymbol.CreateNew(const AToken: TSymbolToken);
begin
  Create;
  FToken := AToken;
end;

function TSimpleSymbol.AllowedInTable: Boolean;
begin
  Result := Token in [toString, toEmpty,
    xtoLink, toBoldBegin, toBoldEnd,
    toItalicBegin, toItalicEnd, toUnderlineBegin, toUnderlineEnd, toParagraph,
    toExtLinkBegin,
    toExtLinkEnd, toImage, toNoWhiteSpace, toDelphiLink];
end;

procedure TSimpleSymbol.Assign(Source: TPersistent);
begin
  if Source is TSimpleSymbol then
    FToken := TSimpleSymbol(Source).Token
  else
    inherited Assign(Source);
end;

function TSimpleSymbol.CanFormatText: Boolean;
begin
  Result := Token <> toParagraph;
end;

function TSimpleSymbol.CanRemoveFromEnd: Boolean;
begin
  Result := Token = toParagraph;
end;

function TSimpleSymbol.DtxText: string;
begin
  case FToken of
    toBoldBegin: Result := '<B>';
    toBoldEnd: Result := '</B>';
    toItalicBegin: Result := '<I>';
    toItalicEnd: Result := '</I>';
    toUnderlineBegin: Result := '<U>';
    toUnderlineEnd: Result := '</U>';
    toExtLinkEnd: Result := '</EXTLINK>';
    toNoWhiteSpace: Result := '';
  else
    ErrorMsg('Other token expected');
  end;
end;

function TSimpleSymbol.NoSpaceAfter: Boolean;
begin
  Result := Token in [toTableBegin, toBoldBegin, toItalicBegin, toUnderlineBegin, toPreBegin,
    toExtLinkBegin, toNoWhiteSpace, toParagraph];
end;

function TSimpleSymbol.NoSpaceBefore: Boolean;
begin
  Result := Token in [toTableEnd, toBoldEnd, toItalicEnd, toUnderlineEnd, toPreEnd,
    toExtLinkEnd, toNoWhiteSpace, toParagraph];
end;

procedure TSimpleSymbol.Save(AXMLWriter: TXMLWriter);
begin
  case FToken of
    toBoldBegin: AXMLWriter.BeginElement('B');
    toBoldEnd: AXMLWriter.EndElement;
    toItalicBegin: AXMLWriter.BeginElement('I');
    toItalicEnd: AXMLWriter.EndElement;
    toUnderlineBegin: AXMLWriter.BeginElement('U');
    toUnderlineEnd: AXMLWriter.EndElement;
    toParagraph: AXMLWriter.WriteEmptyElement('P');
    toExtLinkEnd: AXMLWriter.EndElement;
    toNoWhiteSpace: AXMLWriter.WriteEmptyElement('NWS');
  else
    ErrorMsg('Other token expected');
  end;
end;

procedure TSimpleSymbol.SaveDtx(ADtxWriter: TDtxWriter);
begin
  if Token = toParagraph then
    ADtxWriter.WriteLn;
end;

//=== { TStringSymbol } ======================================================

constructor TStringSymbol.CreateNew(const AString: string);
begin
  Create;
  FValue := AString;
end;

procedure TStringSymbol.Assign(Source: TPersistent);
begin
  if Source is TStringSymbol then
    FValue := TStringSymbol(Source).Value
  else
    inherited Assign(Source);
end;

function TStringSymbol.DtxText: string;
begin
  Result := Value;
end;

function TStringSymbol.IsEmpty: Boolean;
begin
  Result := IsNullStr(Value);
end;

procedure TStringSymbol.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.WriteContent(Value);
end;

//=== { TSymbolList } ========================================================

procedure TSymbolList.Assign(ASymbolList: TSymbolList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ASymbolList.Count - 1 do
    Add(ASymbolList[i].ConstructCopy);
end;

procedure TSymbolList.CheckDefaultText(const S: string);
begin
  if (Count = 1) and (Symbols[0] is TStringSymbol) and
    SameText(S, TStringSymbol(Symbols[0]).Value) then
    Delete(0);
end;

procedure TSymbolList.CleanUp;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Symbols[i].CleanUp;

  while (Count > 0) and Symbols[Count - 1].CanRemoveFromEnd do
    Delete(Count - 1);

  I := Count - 2;
  while I >= 0 do
  begin
    if Symbols[i].NeedParagraphAfter and Symbols[i + 1].CanFormatText then
      Insert(I + 1, TSimpleSymbol.CreateNew(toParagraph));
    Dec(I);
  end;
end;

function TSymbolList.ConstructCopy: TSymbolLIst;
var
  I: Integer;
begin
  Result := TSymbolList.Create;
  for I := 0 to Count - 1 do
    Result.Add(Symbols[i].ConstructCopy);
end;

function TSymbolList.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TSymbolList.GetSymbols(const Index: Integer): TBaseSymbol;
begin
  Result := Items[Index] as TBaseSymbol;
end;

procedure TSymbolList.MoveTo(ASymbolList: TSymbolList);
var
  I: Integer;
begin
  OwnsObjects := False;
  try
    for I := 0 to Count - 1 do
      ASymbolList.Add(Items[i]);
    Clear;
  finally
    OwnsObjects := True;
  end;
end;

function TSymbolList.Parse(const S: string): Boolean;
var
  Parser: TSymbolParser;
begin
  Parser := TSymbolParser.Create(S);
  try
    Parser.List := Self;
    Result := Parser.Parse;
  finally
    Parser.Free;
  end;
end;

procedure TSymbolList.Save(AXMLWriter: TXMLWriter);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Symbols[i].Save(AXMLWriter);
end;

procedure TSymbolList.SaveDtx(ADtxWriter: TDtxWriter; const DoWrapped: Boolean);
var
  S: string;
  I: Integer;
  NoSpaceAfter: Boolean;
begin
  NoSpaceAfter := False;
  S := '';
  for I := 0 to Count - 1 do
  begin
    if Symbols[i].CanFormatText then
    begin
      if not NoSpaceAfter and not Symbols[i].NoSpaceBefore and (S > '') then
        S := S + ' ' + Symbols[i].DtxText
      else
        S := S + Symbols[i].DtxText;
    end
    else
    begin
      if S > '' then
      begin
        if DoWrapped then
          ADtxWriter.WriteWrappedData(S)
        else
          ADtxWriter.WriteLn(S);
        S := '';
      end;
      Symbols[i].SaveDtx(ADtxWriter);
    end;
    NoSpaceAfter := Symbols[i].NoSpaceAfter;
    //    LastWasNoWhiteSpace := (Symbols[i] is TSimpleSymbol) and
    //      (TSimpleSymbol(Symbols[i]).Token in [toNoWhiteSpace, toParagraph]);

  end;

  if S > '' then
  begin
    if DoWrapped then
      ADtxWriter.WriteWrappedData(S)
    else
      ADtxWriter.WriteLn(S);
  end;
end;

procedure TSymbolList.SaveTag(const ATag: string; AXMLWriter: TXMLWriter);
begin
  if Count < 1 then
    Exit;

  AXMLWriter.BeginElement(ATag);
  Save(AXMLWriter);
  AXMLWRiter.EndElement;
end;

//=== { TSymbolParser } ======================================================

constructor TSymbolParser.Create(const S: string);
begin
  inherited Create;
  FDataString := S;
  FSourcePtr := PChar(FDataString);
  FTokenPtr := FSourcePtr;
  FEndsWithCloseBracket := False;
  FEndsWithComma := False;
  FTokenStream := TMemoryStream.Create;
  FBeginLinePtr := nil;
  FIsOnBeginOfLine := True;
  FExpNumber := 1;
  NextToken;
end;

destructor TSymbolParser.Destroy;
begin
  FTokenStream.Free;
  FInternalList.Free;
  inherited Destroy;
end;

procedure TSymbolParser.AddBreak;
begin
  AddSymbol(TSimpleSymbol.CreateNew(toParagraph));
end;

procedure TSymbolParser.AddNoWhiteSpace;
begin
  AddSymbol(TSimpleSymbol.CreateNew(toNoWhiteSpace));
end;

procedure TSymbolParser.AddSymbol(ASymbol: TBaseSymbol);
begin
  List.Add(ASymbol);
end;

procedure TSymbolParser.AddTokenToStream(const AddEndBracket: Boolean = False);
var
  ASpaceChar: Char;
  Head: PChar;
begin
  if (FTokenStream.Position > 0) {and (FTokenPtr^ <> '<')} then
  begin
    ASpaceChar := ' ';
    FTokenStream.Write(ASpaceChar, 1);
  end;
  Head := FStringPtr;
  if AddEndBracket and (Head^ = '>') then
    Inc(Head);
  FTokenStream.Write(FTokenPtr^, Head - FTokenPtr);
end;

procedure TSymbolParser.ClearStream;
begin
  FTokenStream.Seek(0, soFromBeginning);
end;

function TSymbolParser.GetList: TSymbolList;
begin
  Result := FList;
  if Result = nil then
  begin
    if FInternalList = nil then
      FInternalList := TSymbolList.Create;
    Result := FInternalList;
  end;
end;

function TSymbolParser.NextToken(const StopAtComma: Boolean): TSymbolToken;
var
  P: PChar;
  I: INteger;
  LastWasEscape: Boolean;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  //  LastWasEscape := False;

  if P^ <> #0 then
  begin
    if not ((P^ >= #33) and not (P^ in ['>']) and (not StopAtComma or (P^ <> ','))) then
      Result := toEmpty
    else
    begin
      LastWasEscape := P^ = '\';
      Inc(P);
      while (P^ >= #33) and not (not LastWasEscape and (P^ in ['<', '>'])) and (not StopAtComma or (P^ <> ',')) do
      begin
        LastWasEscape := not LastWasEscape and (P^ = '\');
        Inc(P);
      end;

      if FSourcePtr^ = '<' then
      begin
        Result := toString;
        for I := Low(cTokens) to High(cTokens) do
          if StrLIComp(cTokens[I].Token, FSourcePtr, Length(cTokens[I].Token)) = 0 then
          begin
            Result := cTokens[i].T;
            Break;
          end;
      end
      else
        if FIsOnBeginOfLine and (P - FTokenPtr = 1) and ((P - 1)^ = '*') then
        Result := toDot
      else
        if FIsOnBeginOfLine and IsNumberDot(FTokenPtr, P, FExpNumber) then
        Result := toNumber
      else
        Result := toString;
    end;
  end
  else
    Result := toEOF;
  FStringPtr := P;
  FSourcePtr := FStringPtr;
  FEndsWithCloseBracket := FSourcePtr^ = '>';
  FEndsWithComma := StopAtComma and (FSourcePtr^ = ',');
  if FEndsWithCloseBracket or FEndsWithComma then
    Inc(FSourcePtr);
  FToken := Result;
end;

function TSymbolParser.Parse: Boolean;
const
  // string = Before<B>test</b>After
  //                |  |   |  |
  //                1  2   3  4
  //
  // Check at position 1 & 4 for no whitespace, but not at 2 & 3
  //
  cAllTokens = [Low(TSymbolToken)..High(TSymbolToken)];
  cBeginTokens = [toTableBegin, toBoldBegin, toItalicBegin, toUnderlineBegin, toPreBegin,
    toExtLinkBegin];
  cEndTokens = [toTableEnd, toBoldEnd, toItalicEnd, toUnderlineEnd, toPreEnd,
    toExtLinkEnd];
  cCheckNWSAfterTokens = cAllTokens - [toBOF, toParagraph, toEmpty, toDot, toNumber] - cBeginTokens;
  cCheckNWSBeforeTokens = cAllTokens - [toEOF, toParagraph, toEmpty, toDot, toNumber] - cEndTokens;

  //  toEOF, toString, toEmpty, toDot, toNumber,
  //    toLink, toTableBegin, toTableEnd, toBoldBegin, toBoldEnd,
  //    toItalicBegin, toItalicEnd, toUnderlineBegin, toUnderlineEnd, toParagraph,
  //    toPreBegin, toPreEnd, toCodeBegin, toCodeEnd, toExtLinkBegin,
  //    toExtLinkEnd, toImage, toAutoLink, toNoWhiteSpace, toBOF
var
  PrevToken: TSymbolToken;
  BoldCount, UnderlineCount, ItalicCount, ExtLinkCount: Integer;
begin
  PrevToken := toBOF;
  Result := True;
  BoldCount := 0;
  UnderlineCount := 0;
  ItalicCount := 0;
  ExtLinkCount := 0;

  while Result and (Token <> toEOF) do
  begin
    if (Token <> toString) and (PrevToken <> toString) and FMultipleEnters then
      AddBreak;
    if (Token = toString) and (PrevToken in cCheckNWSAfterTokens) then
    begin
      if FNoWhiteSpace then
        AddNoWhiteSpace
      else
        if FMultipleEnters then
        AddBreak;
    end;
    if (Token in cCheckNWSBeforeTokens) and (PrevToken = toString) and FNoWhiteSpace then
      AddNoWhiteSpace;

    PrevToken := Token;
    case Token of
      toString, toEmpty: Result := ReadString;
      xtoLink: Result := ReadLink;
      toDelphiLink: Result := ReadDelphiLink;
      toImage: Result := ReadImage;
      toInclude: Result := ReadInclude;
      toExtLinkBegin:
        begin
          Inc(ExtLinkCount);
          Result := ReadExtLinkBegin;
        end;
      toExtLinkEnd:
        begin
          Dec(ExtLinkCount);
          Result := ExtLinkCount >= 0;
          if not Result then
            ErrorMsg('ExtLinkCount < 0');
          ReadSimpleTag(Token);
        end;
      toDot: Result := ReadDotList;
      toNumber: Result := ReadNumberList;
      toTableBegin: Result := ReadTable;
      toBoldBegin:
        begin
          Inc(BoldCount);
          ReadSimpleTag(Token);
        end;
      toBoldEnd:
        begin
          Dec(BoldCount);
          Result := BoldCount >= 0;
          if not Result then
            ErrorMsgFmt('BoldCount < 0 in <%s>', [FDataString]);
          ReadSimpleTag(Token);
        end;
      toItalicBegin:
        begin
          Inc(ItalicCount);
          ReadSimpleTag(Token);
        end;
      toItalicEnd:
        begin
          Dec(ItalicCount);
          Result := ItalicCount >= 0;
          if not Result then
            ErrorMsgFmt('ItalicCount < 0 in <%s>', [FDataString]);
          ReadSimpleTag(Token);
        end;
      toUnderlineBegin:
        begin
          Inc(UnderlineCount);
          ReadSimpleTag(Token);
        end;
      toUnderlineEnd:
        begin
          Dec(UnderlineCount);
          Result := UnderlineCount >= 0;
          if not Result then
            ErrorMsgFmt('UnderlineCount < 0 in <%s>', [FDataString]);
          ReadSimpleTag(Token);
        end;
      toParagraph: Result := ReadParagraph;
      toPreBegin, toCodeBegin: Result := ReadCode(Token);
      toTableEnd, toPreEnd, toCodeEnd:
        begin
          ErrorMsgFmt('Unexpected end tag in <%s>', [FDataString]);
          Result := False;
        end;
      toAutoLink: ReadAutoLink;
    else
      Result := False;
      ErrorMsgFmt('Unknown token in <%s>', [FDataString]);
    end;
  end;

  if ExtLinkCount > 0 then
    ErrorMsgFmt('ExtLinkCount > 0 in <%s>', [FDataString]);
  if BoldCount > 0 then
    ErrorMsgFmt('BoldCount > 0 in <%s>', [FDataString]);
  if ItalicCount > 0 then
    ErrorMsgFmt('ItalicCount > 0 in <%s>', [FDataString]);
  if UnderlineCount > 0 then
    ErrorMsgFmt('UnderlineCount > 0 in <%s>', [FDataString]);
end;

function TSymbolParser.ReadAutoLink: Boolean;
begin
  Result := NextToken = toString;
  if not Result then
  begin
    ErrorMsg('AUTOLINK: string expected');
    Exit;
  end;

  while (Token <> toEOF) and not FEndsWithCloseBracket do
    NextToken;

  Result := FEndsWithCloseBracket;
  if not Result then
  begin
    ErrorMsg('AUTOLINK: > expected');
    Exit;
  end;

  NextToken;
end;

function TSymbolParser.ReadCode(const AToken: TSymbolToken): Boolean;
var
  P: PChar;
  S: string;
  EndToken: TSymbolToken;
begin
  case AToken of
    toPreBegin: EndToken := toPreEnd;
    toCodeBegin: EndToken := toCodeEnd;
  else
    ErrorMsg('CODE: CODE or PRE expected');
    Result := False;
    Exit;
  end;

  P := FSourcePtr;
  NextToken;
  while (Token <> toEOF) and (Token <> EndToken) do
    NextToken;

  Result := Token = EndToken;
  if not Result then
  begin
    ErrorMsg('CODE: </CODE> or </PRE> expected');
    Exit;
  end;

  SetString(S, P, FTokenPtr - P);

  NextToken;

  AddSymbol(TCodeSymbol.CreateNew(S, AToken));
end;

function TSymbolParser.ReadDotList: Boolean;
var
  DotList: TDotItemsSymbol;
  IndentAmount: Integer;
  S: string;
  P: PChar;
begin
  Result := True;

  DotList := TDotItemsSymbol.Create;
  while FIsOnBeginOfLine and (Token = toDot) do
  begin
    P := FSourcePtr;
    NextToken;
    IndentAmount := FTokenPtr - FBeginLinePtr;
    while (Token <> toEOF) and
      (not FIsOnBeginOfLine or (FTokenPtr - FBeginLinePtr >= IndentAmount)) do
      NextToken;

    SetString(S, P, FTokenPtr - P);

    Result := DotList.Add(S);
    if not Result then
      Exit;
  end;
  AddSymboL(DotList);
end;

function TSymbolParser.ReadExtLinkBegin: Boolean;
begin
  Result := NextToken(True) = toString;
  if not Result then
  begin
    ErrorMsg('EXTLINK: String expected');
    Exit;
  end;

  ClearStream;
  AddTokenToStream;
  while not FEndsWithCloseBracket and (NextToken(True) = toString) do
    if Token <> toEmpty then
      AddTokenToStream;

  Result := FEndsWithCloseBracket;
  if not Result then
  begin
    ErrorMsg('EXTLINK: > expected');
    Exit;
  end;

  AddSymbol(TExtLinkSymbol.CreateNew(StringFromStream));

  NextToken;
end;

function TSymbolParser.ReadImage: Boolean;
begin
  Result := NextToken(True) = toString;
  if not Result then
  begin
    ErrorMsg('IMAGE: String expected');
    Exit;
  end;

  ClearStream;
  AddTokenToStream;
  while not FEndsWithCloseBracket and (NextToken(True) = toString) do
    if Token <> toEmpty then
      AddTokenToStream;

  Result := FEndsWithCloseBracket;
  if not Result then
  begin
    ErrorMsg('IMAGE: > expected');
    Exit;
  end;

  AddSymbol(TImageSymbol.CreateNew(StringFromStream));

  NextToken;
end;

function TSymbolParser.ReadInclude: Boolean;
begin
  Result := NextToken(True) = toString;
  if not Result then
  begin
    ErrorMsg('INCLUDE: String expected');
    Exit;
  end;

  ClearStream;
  AddTokenToStream;
  while not FEndsWithCloseBracket and (NextToken(True) = toString) do
    if Token <> toEmpty then
      AddTokenToStream;

  Result := FEndsWithCloseBracket;
  if not Result then
  begin
    ErrorMsg('INCLUDE: > expected');
    Exit;
  end;

  AddSymbol(TIncludeSymbol.CreateNew(StringFromStream));

  NextToken;
end;

function TSymbolParser.ReadLink: Boolean;
var
  Link: string;
  Description: string;
begin
  Result := NextToken(True) = toString;
  if not Result then
  begin
    ErrorMsg(Format('LINK <%s>: String expected', [FDataString]));
    Exit;
  end;

  ClearStream;
  AddTokenToStream;
  while not (FEndsWithCloseBracket or FEndsWithComma) and
    (NextToken(True) = toString) do
    if Token <> toEmpty then
      AddTokenToStream;

  Result := FEndsWithCloseBracket or FEndsWithComma;
  if not Result then
  begin
    ErrorMsg(Format('LINK <%s>: > or , expected', [FDataString]));
    Exit;
  end;

  Link := StringFromStream;

  if FEndsWithCloseBracket then
    Description := ''
  else
  begin
    ClearStream;
    while not FEndsWithCloseBracket and (NextToken = toString) do
      AddTokenToStream;

    Result := FEndsWithCloseBracket;
    if not Result then
    begin
      ErrorMsg(Format('LINK <%s>: > expected', [FDataString]));
      Exit;
    end;

    Description := StringFromStream;
  end;
  AddSymbol(TLinkSymbol.CreateNew(Link, Description));

  NextToken;
end;

function TSymbolParser.ReadNumberList: Boolean;
var
  NumberList: TNumberItemsSymbol;
  IndentAmount: Integer;
  S: string;
  P: PChar;
begin
  Result := True;

  try
    NumberList := TNumberItemsSymbol.Create;
    while FIsOnBeginOfLine and (Token = toNumber) do
    begin
      Inc(FExpNumber);
      P := FSourcePtr;
      NextToken;
      IndentAmount := FTokenPtr - FBeginLinePtr;
      while (Token <> toEOF) and
        (not FIsOnBeginOfLine or (FTokenPtr - FBeginLinePtr >= IndentAmount)) do
        NextToken;

      SetString(S, P, FTokenPtr - P);

      Result := NumberList.Add(S);
      if not Result then
        Exit;
    end;
    AddSymboL(NumberList);
  finally
    FExpNumber := 1;
  end;
end;

function TSymbolParser.ReadParagraph: Boolean;
begin
  AddSymbol(TSimpleSymbol.CreateNew(toParagraph));
  NextToken;
  Result := True;
end;

function TSymbolParser.ReadSimpleTag(const AToken: TSymbolToken): Boolean;
begin
  AddSymbol(TSimpleSymbol.CreateNew(AToken));
  NextToken;
  Result := True;
end;

function TSymbolParser.ReadString: Boolean;
begin
  Result := True;

  ClearStream;
  AddTokenToStream(True);
  while NextToken in [toString, toEmpty] do
  begin
    if FMultipleEnters then
    begin
      AddSymbol(TStringSymbol.CreateNew(StringFromStream));
      ClearStream;
      AddBreak;
    end;

    AddTokenToStream(True);
  end;

  AddSymbol(TStringSymbol.CreateNew(StringFromStream));
  if FMultipleEnters then
    AddBreak;
end;

function TSymbolParser.ReadTable: Boolean;
var
  P: PChar;
  TableParser: TTableParser;
  TableWidths: TStringList;
  Table: TDtxTable;
  S: string;
begin
  // ">" or "30c%, 40c%, 30c%>"

  TableWidths := nil;
  try
    if not FEndsWithCloseBracket then
    begin
      TableWidths := TStringList.Create;

      repeat
        NextToken(True);
        Result := Token = toString;
        if not Result then
        begin
          ErrorMsg('TABLE: string expected');
          Exit;
        end;

        TableWidths.Add(TokenString);
      until FEndsWithCloseBracket;
    end;

    P := FSourcePtr;
    NextToken;
    while not (Token in [toEOF, toTableEnd]) do
      NextToken;

    Result := Token = toTableEnd;
    if not Result then
    begin
      ErrorMsg('TABLE: </TABLE> expected');
      Exit;
    end;

    SetString(S, P, FTokenPtr - P);

    NextToken;

    Table := TDtxTable.Create;
    AddSymbol(Table);
    TableParser := TTableParser.Create(S, TableWidths);
    try
      TableParser.Table := Table;
      Result := TableParser.Parse;
    finally
      TableParser.Free;
    end;
  finally
    TableWidths.Free;
  end;
end;

procedure TSymbolParser.SkipBlanks;
var
  Char10Count: Integer;
begin
  FIsOnBeginOfLine := FSourcePtr = PChar(FDataString);
  FNoWhiteSpace := True;
  Char10Count := 0;

  while True do
  begin
    case FSourcePtr^ of
      #0: Break;
      #10, #13:
        begin
          if FSourcePtr^ = #10 then
            Inc(Char10Count);
          FIsOnBeginOfLine := True;
          FBeginLinePtr := nil;
          FNoWhiteSpace := False;
        end;
      #33..#255:
        begin
          if FBeginLinePtr = nil then
            FBeginLinePtr := FSourcePtr;
          Break;
        end;
    end;
    if FBeginLinePtr = nil then
      FBeginLinePtr := FSourcePtr;
    FNoWhiteSpace := False;
    Inc(FSourcePtr);
  end;

  FMultipleEnters := Char10Count > 1;
end;

function TSymbolParser.StringFromStream: string;
begin
  SetString(Result, PChar(FTokenStream.Memory), FTokenStream.Seek(0, soCurrent));
end;

function TSymbolParser.TokenString: string;
begin
  SetString(Result, FTokenPtr, FStringPtr - FTokenPtr);
end;

//=== { TTableParser } =======================================================

constructor TTableParser.Create(const TableStr: string; ATableWidths: TStrings);
begin
  inherited Create;
  FLines := TStringList.Create;
  FLines.Text := TableStr;
  while (FLines.Count > 0) and IsNullStr(FLines[0]) do
    FLines.Delete(0);
  FTableWidths := ATableWidths;
end;

destructor TTableParser.Destroy;
begin
  FLines.Free;
  FInternalTable.Free;
  inherited Destroy;
end;

function TTableParser.DetermineColDimension(
  AColStarts, AColLengths: TIntegerList; out AHeaderSepIndex: Integer): Boolean;
var
  SepIndexen: TBits;
  I: Integer;
begin
  // AColStarts is 0-based
  SepIndexen := TBits.Create;
  try
    Result := DetermineSepIndexen(SepIndexen, AHeaderSepIndex);
    if not Result then
      Exit;

    Table.HasHeader := AHeaderSepIndex >= 1;

    // SepIndexen[i] = False  -> always space or tab on this position
    I := 0;

    while I < SepIndexen.Size - 1 do
    begin
      while (I < SepIndexen.Size) and not SepIndexen[i] do
        Inc(I);
      AColStarts.Add(I);
      while (I < SepIndexen.Size - 1) and (SepIndexen[i] or SepIndexen[i + 1]) do
        Inc(I);
      if (I = SepIndexen.Size - 1) and SepIndexen[i] then
        Inc(I);

      AColLengths.Add(I - AColStarts[AColStarts.Count - 1]);
    end;
  finally
    SepIndexen.Free;
  end;
end;

function TTableParser.DetermineMultiRowIndexen(
  AMultiRowIndexen: TIntegerList): Boolean;
var
  ARow, ACol: Integer;
begin
  Result := True;

  for ARow := 0 to Table.RowCount - 1 do
  begin
    ACol := 0;
    while (ACol < Table.ColCount) and StartWith2Spaces(Table.Cells[ACol, ARow]) do
      Inc(ACol);

    if ACol = Table.ColCount then
      AMultiRowIndexen.Add(ARow);
  end;
end;

function TTableParser.DetermineSepIndexen(
  ASepIndexen: TBits; out AHeaderSepIndex: Integer): Boolean;
var
  MaxLength: Integer;
  I, J: Integer;
  S: string;
  LineChars: TSysCharSet;
begin
  AHeaderSepIndex := -1;

  MaxLength := 0;
  for I := 0 to FLines.Count - 1 do
    MaxLength := Max(MaxLength, Length(FLines[i]));

  Result := MaxLength > 0;
  if not Result then
  begin
    ErrorMsg('TableParser: MaxLength = 0');
    Exit;
  end;

  ASepIndexen.Size := MaxLength;

  for I := 0 to FLines.Count - 1 do
  begin
    S := FLines[i];
    LineChars := [];
    for J := 0 to Length(S) - 1 do
      if not (s[j + 1] in [SpaceChar, TabChar]) then
      begin
        ASepIndexen[j] := True;
        Include(LineChars, s[j + 1]);
      end;
    if (AHeaderSepIndex < 0) and
      (LineChars <> []) and (LineChars <= ['-', '=']) then
      AHeaderSepIndex := i;
  end;
end;

function TTableParser.FillTable(AColStarts, AColLengths: TIntegerList; out AHeaderSepIndex: Integer): Boolean;
var
  ACol, ALineIndex, ARow: Integer;
  S: string;
begin
  Result := True;

  for ALineIndex := 0 to AHeaderSepIndex - 1 do
  begin
    S := FLines[ALineIndex];

    for ACol := 0 to AColStarts.Count - 1 do
      Table.Header[ACol] := Table.Header[ACol] + Copy(S, 1 + AColStarts[ACol], AColLengths[ACol])
  end;

  if AHeaderSepIndex >= 0 then
  begin
    S := FLines[AHeaderSepIndex];
    for ACol := 0 to AColStarts.Count - 1 do
    begin
      if Length(S) < 1 + AColStarts[ACol] then
        Break;

      Table.HeaderChar[ACol] := S[1 + AColStarts[ACol]];
    end;
  end;

  ARow := 0;
  for ALineIndex := AHeaderSepIndex + 1 to FLines.Count - 1 do
  begin
    S := FLines[ALineIndex];

    for ACol := 0 to AColStarts.Count - 1 do
      Table.Cells[ACol, ARow] := Copy(S, 1 + AColStarts[ACol], AColLengths[ACol]);
    Inc(ARow);
  end;
end;

function TTableParser.GetTable: TDtxTable;
begin
  Result := FTable;
  if Result = nil then
  begin
    if FInternalTable = nil then
      FInternalTable := TDtxTable.Create;
    Result := FInternalTable;
  end;
end;

function TTableParser.MergeMultiRows: Boolean;
var
  MultiRowIndexen: TIntegerList;
  ARow: Integer;
begin
  MultiRowIndexen := TIntegerList.Create;
  try
    Result := DetermineMultiRowIndexen(MultiRowIndexen);
    if not Result then
      Exit;

    for ARow := Table.RowCount - 1 downto 1 do
      if MultiRowIndexen.Contains(ARow) then
        Table.MergeRows(ARow - 1, ARow);
  finally
    MultiRowIndexen.Free;
  end;
end;

function TTableParser.Parse: Boolean;
begin
  Result := ParseTable and ParseTableWidths;
end;

function TTableParser.ParseTable: Boolean;
var
  ColStarts, ColLengths: TIntegerList;
  HeaderSepIndex: Integer;
begin
  ColStarts := TIntegerList.Create;
  ColLengths := TIntegerList.Create;
  try
    Result := DetermineColDimension(ColStarts, ColLengths, HeaderSepIndex);
    if not Result then
      Exit;

    Result := (ColStarts.Count = ColLengths.Count) and (ColStarts.Count > 0);
    if not Result then
    begin
      ErrorMsg('TableParser: ColStarts.Count <> ColLengths.Count');
      Exit;
    end;

    Table.ColCount := ColStarts.Count;
    Table.RowCount := FLines.Count - HeaderSepIndex - 1;

    Result := FillTable(ColStarts, ColLengths, HeaderSepIndex);
    if not Result then
      Exit;

    Result := MergeMultiRows;
    if not Result then
      Exit;

    Result := Table.ParseCells;
    if not Result then
      Exit;

    Result := Table.CheckCells;
    if not Result then
      Exit;
  finally
    ColLengths.Free;
    ColStarts.Free;
  end;
end;

constructor TCodeSymbol.Create;
begin
  inherited Create;
  FCode := TStringList.create;
end;

function GetTableWidth(const TableWidthStr: string; out ATableWidth: Integer; out AComputated: Boolean): Boolean;
var
  P, Q: PChar;
  S: string;
begin
  // format is "33c%" or "c%" or "50%"

  // [Number] [c] [%]

  ATableWidth := 0;
  AComputated := False;

  P := PChar(TableWidthStr);
  while P^ = ' ' do
    Inc(P);

  if P^ in ['0'..'9'] then
  begin
    Q := P;
    while P^ in ['0'..'9'] do
      Inc(P);
    SetString(S, Q, P - Q);
    ATableWidth := StrToIntDef(S, -1);
    Result := ATableWidth >= 0;
    if not Result then
      Exit;

    while P^ = ' ' do
      Inc(P);
  end;

  if P^ in ['c', 'C'] then
  begin
    AComputated := True;
    Inc(P);
    while P^ = ' ' do
      Inc(P);
  end;

  Result := P^ = '%';
  if not Result then
    Exit;

  Inc(P);
  while P^ = ' ' do
    Inc(P);

  Result := P^ = #0;
  if not Result then
    Exit;
end;

function TTableParser.ParseTableWidths: Boolean;
var
  AColWidth: Integer;
  AColIsComputed: Boolean;
  HasTableWidth: Boolean;
  ColIndex: Integer;
  TableWidthIndex: Integer;
begin
  if FTableWidths = nil then
  begin
    Result := True;
    Exit;
  end;

  Result :=
    (FTableWidths.Count = FTable.ColCount) or
    (FTableWidths.Count = 1) or
    (FTableWidths.Count = FTable.ColCount + 1);
  if not Result then
  begin
    ErrorMsgFmt('TableParser: ColCount (%d) <> Width Count (%d)', [FTable.ColCount, FTableWidths.Count]);
    Exit;
  end;

  HasTableWidth :=
    (FTableWidths.Count = 1) or
    (FTableWidths.Count = FTable.ColCount + 1);

  TableWidthIndex := 0;
  if HasTableWidth then
  begin
    Result := GetTableWidth(FTableWidths[0], AColWidth, AColIsComputed);
    if not Result then
    begin
      ErrorMsgFmt('TableParser: Invalid table width: %s', [FTableWidths[0]]);
      Exit;
    end;

    FTable.TableWidthPercentage := AColWidth;
    TableWidthIndex := 1;
  end;

  ColIndex := 0;
  while TableWidthIndex < FTableWidths.Count do
  begin
    Result := GetTableWidth(FTableWidths[TableWidthIndex], AColWidth, AColIsComputed);
    if not Result then
    begin
      ErrorMsgFmt('TableParser: Invalid table width: %s', [FTableWidths[TableWidthIndex]]);
      Exit;
    end;

    FTable.ColWidth[ColIndex] := AColWidth;
    FTable.ColIsComputed[ColIndex] := AColIsComputed;

    Inc(ColIndex);
    Inc(TableWidthIndex);
  end;
end;

function TDtxTable.GetColIsComputed(ACol: Integer): Boolean;
begin
  CheckColIndex(ACol);
  if not Assigned(FColIsComputeds) or (ACol >= FColIsComputeds.Size) then
    Result := False
  else
    Result := FColIsComputeds[ACol];
end;

function TDtxTable.GetColWidth(ACol: Integer): Integer;
begin
  CheckColIndex(ACol);
  if not Assigned(FColWidths) or (ACol >= FColWidths.Count) then
    Result := 0
  else
    Result := FColWidths[ACol];
end;

procedure TDtxTable.SetColIsComputed(ACol: Integer; const Value: Boolean);
begin
  CheckColIndex(ACol);
  if FColIsComputeds = nil then
    FColIsComputeds := TBits.Create;
  if ACol >= FColIsComputeds.Size then
    FColIsComputeds.Size := ACol + 1;
  FColIsComputeds[ACol] := Value;
end;

procedure TDtxTable.SetColWidth(ACol: Integer; const Value: Integer);
begin
  CheckColIndex(ACol);
  if FColWidths = nil then
    FColWidths := TIntegerList.Create;
  while ACol >= FColWidths.Count do
    FColWidths.Add(0);
  FColWidths[ACol] := Value;
end;

function TDtxTable.GetWidthStr: string;
var
  I: Integer;
begin
  Result := '';

  if IsTableWidthSpecified then
    Result := Format('%d%%,', [TableWidthPercentage]);

  if IsColWidthSpecified then
    for I := 0 to ColCount - 1 do
      Result := Result + ColWidthStr[i] + ',';

  if Result > '' then
    Delete(Result, Length(Result), 1);
end;

function TDtxTable.GetColWidthStr(const ACol: Integer): string;
begin
  CheckColIndex(ACol);
  if ColIsComputed[ACol] then
  begin
    if ColWidth[ACol] > 0 then
      Result := Format('%dc%%', [ColWidth[ACol]])
    else
      Result := 'c%';
  end
  else
    Result := Format('%d%%', [ColWidth[ACol]]);
end;

function TDtxTable.GetHasWidthStr: Boolean;
begin
  Result := IsColWidthSpecified or IsTableWidthSpecified;
end;

function TDtxTable.GetIsColWidthSpecified: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ColCount - 1 do
  begin
    Result := ColIsComputed[i] or (ColWidth[i] > 0);
    if Result then
      Break;
  end;
end;

function TDtxTable.GetIsTableWidthSpecified: Boolean;
begin
  Result := (TableWidthPercentage > 0) and (TableWidthPercentage < 100);
end;

function TSymbolParser.ReadDelphiLink: Boolean;
var
  Link: string;
  Description: string;
begin
  Result := NextToken(True) = toString;
  if not Result then
  begin
    ErrorMsg(Format('DELPHILINK <%s>: String expected', [FDataString]));
    Exit;
  end;

  ClearStream;
  AddTokenToStream;
  while not (FEndsWithCloseBracket or FEndsWithComma) and
    (NextToken(True) = toString) do
    if Token <> toEmpty then
      AddTokenToStream;

  Result := FEndsWithCloseBracket or FEndsWithComma;
  if not Result then
  begin
    ErrorMsg(Format('DELPHILINK <%s>: > or , expected', [FDataString]));
    Exit;
  end;

  Link := StringFromStream;

  if FEndsWithCloseBracket then
    Description := ''
  else
  begin
    ClearStream;
    while not FEndsWithCloseBracket and (NextToken = toString) do
      AddTokenToStream;

    Result := FEndsWithCloseBracket;
    if not Result then
    begin
      ErrorMsg(Format('DELPHILINK <%s>: > expected', [FDataString]));
      Exit;
    end;

    Description := StringFromStream;
  end;
  AddSymbol(TDelphiLinkSymbol.CreateNew(Link, Description));

  NextToken;
end;

{ TSpecialStringSymbol }

function TSpecialStringSymbol.CanFormatText: Boolean;
begin
  Result := False;
end;

procedure TSpecialStringSymbol.SaveDtx(ADtxWriter: TDtxWriter);
var
  SS: TStringList;
  I: Integer;
begin
  SS := TStringList.Create;
  try
    SS.Text := Self.Value;
    for I := 0 to SS.Count-1 do
    ADtxWriter.WriteLn(SS[i]);
  finally
    SS.Free;
  end;
end;

end.







