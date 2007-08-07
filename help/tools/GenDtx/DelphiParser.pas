unit DelphiParser;

interface

uses
  Classes, ParserTypes, Contnrs;

const
  toComment = Char(6);
  toSameLineComment = Char(7);
  toCompilerDirective = Char(8);
  toDotDot = Char(15);

  toColon = Char(':'); // 58
  toSemiColon = Char(';'); // 59
  toLeftParens = Char('('); // 40
  toLeftBracket = Char('['); // 91
  toRightParens = Char(')'); // 41
  toRightBracket = Char(']'); // 93
  toEquals = Char('='); // 61
  toComma = Char(','); // 44
  toDot = Char('.'); // 46

type
  THaakType = (htParens, htBracket);
  TModuleType = (mtLibrary, mtUnit);
  TDtxParseOption = (dpoLinks, dpoParameters, dpoDefaultText);
  TDtxParseOptions = set of TDtxParseOption;

  TBasicParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar; // end of current token
    FSourceEnd: PChar;
    FTokenPtr: PChar; // start of current token
    FSourceLine: Integer;
    FSaveChar: Char;
    FToken: Char;
    FFloatType: Char;
    FWideStr: WideString;
    FRecordStr: string;
    FRecording: Boolean;
    FCompilerDirectives: TStringList;
    FLastWasNewLine: Boolean;
    FLowRecording: Boolean; { TODO : Use FLowRecordPtr }
    FLowRecordPtr: PChar;

    FLowRecordBuffer: PChar;
    FLowRecordBufPtr: PChar;
    FLowRecordBufEnd: PChar; { TODO : Use size ?? }

    FLowOutputStream: TStream;

    FSavedSourcePtr: PChar;
    FSavedTokenPtr: PChar;
    FSavedToken: Char;
    FSavedSourceLine: Integer;

    procedure ReadBuffer;
    function ReadPortion(CheckPosition: Integer): Boolean;
    procedure SkipBlanks;
    procedure AddToLowRecording(StartPtr, EndPtr: PChar);
    procedure AddTokenToRecordStr;
    procedure SetRecording(const Value: Boolean);
    function GetRecordStrWithCurrentToken: string;
    procedure SetLowRecording(const Value: Boolean);
    function GetLowRecordingStr: string;
  protected
    function ReadNextToken: Char; virtual; abstract;

    procedure SaveRollBackPosition; virtual;
    procedure RollBackToSavedPosition; virtual;
    procedure ForgetRollBackPosition; virtual;

    procedure Init; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckNotToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    function CheckTokenSymbolIn(const List: array of string): Integer;
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    function LowNextToken(SkipBlanks: Boolean = True): Char; virtual;
    function SourcePos: Longint;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt: Int64;
    function TokenString: string;
    function TokenWideString: WideString;
    function TokenSymbolIs(const S: string): Boolean;
    function TokenSymbolIsExact(const S: string): Boolean;
    function TokenSymbolIn(const List: array of string): Integer;

    procedure BeginRecording;
    procedure EndRecording;

    procedure BeginLowRecording;
    procedure EndLowRecording; // with current token
    procedure EndLowRecordingWithoutCurrent; // but with white space etc.

    procedure BeginOutputTo(AStream: TStream);
    procedure EndOutputTo;

    property FloatType: Char read FFloatType;
    property SourceLine: Integer read FSourceLine;
    property Token: Char read FToken;
    property RecordStr: string read GetRecordStrWithCurrentToken;
    property LowRecordingStr: string read GetLowRecordingStr;
    property RecordStrWithoutCurrentToken: string read FRecordStr;
    property Recording: Boolean read FRecording write SetRecording;
    property LowRecording: Boolean read FLowRecording write SetLowRecording;
  end;

  TDelphiParser = class(TBasicParser)
  private
    FPasItems: TPasItems;
    FErrorMsg: string;
    FAcceptCompilerDirectives: TStrings;
    FAcceptVisibilities: TClassVisibilities;
    FLastCompilerDirectiveAccepted: Boolean;
    FDEFList: TStrings;
    FBlockDEFList: TStrings;
    FSavedDEFList: TStrings;
    FCopiedDEFList: TStrings;
    FCapitalization: TStrings;
    FRecapitalize: Boolean;
    FHoldRecapitalize: Boolean;
    FWantCompilerDirectives: Boolean;
    FInterpretCompilerDirectives: Boolean;
    procedure SetAcceptCompilerDirectives(const Value: TStrings);
    function GetDoRecapitalize: Boolean;
  protected
    function ReadNextToken: Char; override;
    function TokenSymbolInC(const List: array of string): Integer;
    procedure CheckTokenSymbolC(const S: string);
    function CheckTokenSymbolInC(const List: array of string): Integer;
    function TokenSymbolIsC(const S: string): Boolean;

    procedure SaveRollBackPosition; override;
    procedure RollBackToSavedPosition; override;
    procedure ForgetRollBackPosition; override;

    procedure AddDefine(const S: string);
    procedure AddNotDefine(const S: string);
    procedure SwitchDefine;
    procedure EndDefine;
    procedure MakeCopyOfDEFList;
    procedure BeginBlock;
    procedure EndBlock(const AllowDoubleEnd: Boolean);

    procedure RecapitalizeToken;
    procedure RecapitalizeTokenWith(const S: string);
    procedure StartCapitalization;
    procedure StopCapitalization;

    function ReadClass: TAbstractItem;
    function ReadClass_ClassMethod(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
    function ReadClass_Field(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
    function ReadClass_Function(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
    function ReadClass_Procedure(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
    function ReadClass_Property(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
    function ReadConst: TAbstractItem;
    function ReadDispInterface: TAbstractItem;
    function ReadEnumerator: TAbstractItem;
    function ReadFunctionType: TAbstractItem;
    function ReadInterfaceType: TAbstractItem;
    function ReadProcedureType: TAbstractItem;
    function ReadRecord: TAbstractItem;
    function ReadResourceString: TAbstractItem;
    function ReadSimpleType: TAbstractItem;
    function ReadType(const DoReadDirectives: Boolean): TAbstractItem;
    function ReadTypeDef: TAbstractItem;
    function ReadVar: TAbstractItem;
    function ReadThreadVar: TAbstractItem;

    function SkipClass(const DoAdd: Boolean): TAbstractItem;
    function SkipConst(const DoAdd: Boolean): TAbstractItem;
    function SkipDispInterface(const DoAdd: Boolean): TAbstractItem;
    function SkipEnumerator(const DoAdd: Boolean): TAbstractItem;
    function SkipFunctionType(const DoAdd: Boolean): TAbstractItem;
    function SkipInterfaceType(const DoAdd: Boolean): TAbstractItem;
    function SkipProcedureType(const DoAdd: Boolean): TAbstractItem;
    function SkipRecord(const DoAdd: Boolean): TAbstractItem;
    function SkipResourceString(const DoAdd: Boolean): TAbstractItem;
    function SkipSimpleType(const DoAdd: Boolean): TAbstractItem;
    function SkipType(const DoAdd, DoReadDirectives: Boolean): TAbstractItem;
    function SkipTypeDef(const DoAdd: Boolean): TAbstractItem;
    function SkipVar(const DoAdd: Boolean): TAbstractItem;

    procedure ReadClassMethods(AClassItem: TClassItem);
    procedure ReadCommentBlock;
    procedure ReadDirectives(const MaySkipSemiColon: Boolean; var Directives: TDirectives);
    procedure ReadFunction;
    procedure SkipFunction;
    procedure ReadInterfaceBlock;
    procedure ReadInterfaceMethods(AInterfaceItem: TInterfaceItem);
    procedure ReadInterfaceStatement;
    procedure ReadParamList(AParams, ATypes: TStrings; const HaakType: THaakType = htParens);
    procedure ReadProcedure;
    procedure SkipProcedure;
    procedure ReadPropertySpecifiers(const MaySkipSemiColon: Boolean; var Specifiers: TPropertySpecifiers);
    procedure ReadUnitBlock(out AModuleType: TModuleType);
    function ReadUsesBlock(const DoAdd: Boolean): TAbstractItem;
    procedure SkipUsesBlock;

    procedure SkipClass_ClassMethod;
    procedure SkipClass_Field;
    procedure SkipClass_Function;
    procedure SkipClass_Procedure;
    procedure SkipClass_Property;
    procedure SkipClassMethods;
    procedure SkipConstantExpression(const ExpectDotDot: Boolean);
    procedure SkipConstValue;
    procedure SkipDirectives(const MaySkipSemiColon: Boolean);
    procedure SkipInterfaceBlock;
    procedure SkipInterfaceMethods;
    procedure SkipParamList(const HaakType: THaakType = htParens);
    procedure SkipPropertySpecifiers(const MaySkipSemiColon: Boolean);

    { implemenation }
    function ReadClassProcedureFunctionImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadCompilerDirectiveImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadCommentImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadConstImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadFunctionHeader(const DoAdd: Boolean; out IsOnlyHeader: Boolean): TAbstractItem;
    function ReadFunctionImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadProcedureHeader(const DoAdd: Boolean; out IsOnlyHeader: Boolean): TAbstractItem;
    function ReadProcedureImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadResourceStringImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadTypeDefImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadVarImpl(const DoAdd: Boolean): TAbstractItem;
    function ReadThreadVarImpl(const DoAdd: Boolean): TAbstractItem;
    procedure ReadConstVarTypeSection;
    procedure ReadFunctionProcedureBody;
    procedure ReadImplementationBlock;
    procedure ReadInitializationFinalization;

    procedure SkipUntilToken(T: Char);
    procedure SkipUntilSymbol(const Symbol: string);
    procedure SkipUntilTokenInHaak(T: Char; const InitHaak: Integer = 0); overload;
    procedure SkipUntilTokenInHaak(OpenSymbol, CloseSymbol: Char;
      const InitialOpenCount: Integer = 0); overload;

    procedure Init; override;
    function Parse: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function DoNextToken(SkipBlanks: Boolean = True): Char;

    function TokenCompilerDirective: TCompilerDirective;
    function TokenCompilerDirectiveArgument: string;
    function TokenIsConditionalCompilation: Boolean;

    function ExecuteFile(const AFileName: string): Boolean; virtual;
    function Execute(AStream: TStream): Boolean; virtual;
    property TypeList: TPasItems read FPasItems;
    property AcceptCompilerDirectives: TStrings read FAcceptCompilerDirectives
      write SetAcceptCompilerDirectives;
    property AcceptVisibilities: TClassVisibilities read FAcceptVisibilities
      write FAcceptVisibilities default [inPublic, inPublished];
    property ErrorMsg: string read FErrorMsg;
    property Recapitalize: Boolean read FRecapitalize write FRecapitalize;
    property InterpretCompilerDirectives: Boolean read FInterpretCompilerDirectives write FInterpretCompilerDirectives;
    property WantCompilerDirectives: Boolean read FWantCompilerDirectives write FWantCompilerDirectives;
    property DEFList: TStrings read FDEFList;
    property Capitalization: TStrings read FCapitalization write FCapitalization;
    property DoRecapitalize: Boolean read GetDoRecapitalize;
  end;

  TDtxCompareErrorFlag = (defNoPackageTag, defPackageTagNotFilled, 
    defNoStatusTag, defEmptySeeAlso, defNoAuthor, defHasTocEntryError, defUnknownTag);
  TDtxCompareErrorFlags = set of TDtxCompareErrorFlag;

  TPasCheckErrorFlag = (pefNoLicense, pefUnitCase);
  TPasCheckErrorFlags = set of TPasCheckErrorFlag;

  TDefaultText = (dtWriteSummary, dtWriteDescription, dtTypeUsedBy,
    dtListProperties, dtRemoveSeeAlso, dtDescribeReturns, dtOverridenMethod,
    dtInheritedMethod, dtDescriptionFor, dtDescriptionForThisParameter);

  TDefaultTexts = set of TDefaultText;

  TDtxCompareTokenType = (ctHelpTag, ctText, ctParseTag, ctSeperator);

  TJVCLInfoError = (jieGroupNotFilled, jieFlagNotFilled, jieOtherNotFilled, jieNoGroup, jieDoubles);
  TJVCLInfoErrors = set of TJVCLInfoError;

  TDtxBaseItem = class
  private
    FData: string;
    procedure SetData(const Value: string);
  public
    procedure WriteToStream(AStream: TStream); virtual; abstract;
    function CompareWith(AItem: TDtxBaseItem): Integer; virtual; abstract;
    property Data: string read FData write SetData;
  end;

  TDtxStartSymbol = (dssPackage, dssStatus, dssSkip, dssOther);

  TDtxItems = class(TObjectList)
  private
    FSkipList: TStrings;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DtxSort;

    procedure FillWithDtxHeaders(Dest: TStrings);
    procedure CombineWithPasList(APasItems: TPasItems);
    procedure ConstructSkipList;

    function IndexOfReferenceName(ReferenceName: string): Integer;
    procedure WriteToFile(const AFileName: string);

    property SkipList: TStrings read FSkipList;
  end;

  TDtxStartItem = class(TDtxBaseItem)
  private
    FSymbolStr: string;
    FSymbol: TDtxStartSymbol;
    procedure SetSymbolStr(const Value: string);
    procedure SetSymbol(const Value: TDtxStartSymbol);
  public
    function CompareWith(AItem: TDtxBaseItem): Integer; override;
    procedure WriteToStream(AStream: TStream); override;
    property SymbolStr: string read FSymbolStr write SetSymbolStr;
    property Symbol: TDtxStartSymbol read FSymbol write SetSymbol;
  end;

  TDtxHelpItem = class(TDtxBaseItem)
  private
    FTag: string;
    FParameters: TStrings;
    FTitle: string;
    FCombine: string;
    FCombineWith: string;
    FHasTocEntry: Boolean;
    FTitleImg: string;
    FHasJVCLInfo: Boolean;
    FJVCLInfoErrors: TJVCLInfoErrors;
    FIsRegisteredComponent: Boolean;
    FIsFileInfo: Boolean;
    FLinks: TStrings;
    FPasObj: TAbstractItem;
    FHasPasFileEntry: Boolean;
  protected
    procedure AddLink(const ALink: string);
    procedure AddParam(const AParam: string);
    procedure ClearLinks;
  public
    constructor Create(const ATag: string); virtual;
    destructor Destroy; override;
    function CompareWith(AItem: TDtxBaseItem): Integer; override;
    procedure WriteToStream(AStream: TStream); override;
    property IsFileInfo: Boolean read FIsFileInfo write FIsFileInfo;
    property Tag: string read FTag write FTag;
    property Parameters: TStrings read FParameters;
    property Links: TStrings read FLinks;
    property Title: string read FTitle write FTitle;
    property Combine: string read FCombine write FCombine;
    property CombineWith: string read FCombineWith write FCombineWith;
    property HasTocEntry: Boolean read FHasTocEntry write FHasTocEntry;
    property HasPasFileEntry: Boolean read FHasPasFileEntry write FHasPasFileEntry;
    property TitleImg: string read FTitleImg write FTitleImg;
    property HasJVCLInfo: Boolean read FHasJVCLInfo write FHasJVCLInfo;
    property IsRegisteredComponent: Boolean read FIsRegisteredComponent write FIsRegisteredComponent;
    property JVCLInfoErrors: TJVCLInfoErrors read FJVCLInfoErrors write FJVCLInfoErrors;
    property PasObj: TAbstractItem read FPasObj write FPasObj;
  end;

  TDtxCompareParser = class(TBasicParser)
  private
    FList: TDtxItems;
    FErrors: TDtxCompareErrorFlags;
    FDefaultTexts: TDefaultTexts;
    FPasFileNameWithoutPath: string;
    FTags: TStrings;
    FSkipList: TStrings;
    FCollectData: Boolean;
    FOptions: TDtxParseOptions;
    function GetDtxCompareTokenType: TDtxCompareTokenType;
    function GetPackage: string;
  protected
    function ReadNextToken: Char; override;

    function Parse: Boolean;

    procedure ReadAuthor;
    function ReadLink(out Link: string): Boolean;
    procedure ReadItem(Item: TDtxHelpItem);
    procedure ReadFileInfo;
    procedure ReadHasTocEntry(Item: TDtxHelpItem);
    procedure ReadHelpTopic(Item: TDtxHelpItem);
    procedure ReadJVCLINFO(Item: TDtxHelpItem);
    procedure ReadPackage;
    procedure ReadSkip;
    procedure ReadParameters(Item: TDtxHelpItem);
    procedure ReadRest;
    procedure ReadSeeAlso(Item: TDtxHelpItem);
    procedure ReadStartBlock;
    procedure ReadStatus;
    procedure ReadOther;
    procedure ReadTitleImg(Item: TDtxHelpItem);

    property CompareTokenType: TDtxCompareTokenType read GetDtxCompareTokenType;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Package: string read GetPackage;
    function Execute(const AFileName: string): Boolean;
    property CollectData: Boolean read FCollectData write FCollectData;
    property List: TDtxItems read FList;
    property Tags: TStrings read FTags;
    property Errors: TDtxCompareErrorFlags read FErrors;
    property DefaultTexts: TDefaultTexts read FDefaultTexts;
    property SkipList: TStrings read FSkipList write FSkipList;
    property Options: TDtxParseOptions read FOptions write FOptions;
  end;

  TDpkParser = class(TDelphiParser)
  private
    FList: TStrings;
    procedure ReadUntilContainsBlock;
    function ReadFileReference: Boolean;
  protected
    function Parse: Boolean; override;
    procedure Init; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property List: TStrings read FList;
  end;

  TRegisteredClassesParser = class(TDelphiParser)
  private
    FList: TStrings;
  protected
    function ReadUntilRegisterBlock: Boolean;
    function ReadUntilRegisterComponentsBlock: Boolean;
    function ReadRegisterComponentsBlock: Boolean;
    function Parse: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Init; override;

    property List: TStrings read FList;
  end;

  TPasCasingParser = class(TDelphiParser)
  private
    FList: TStrings;
    FID: Integer;
    FAllSymbols: Boolean;
    FDoCount: Boolean;
  protected
    function Parse: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property List: TStrings read FList;
    property ID: Integer read FID write FID;
    property AllSymbols: Boolean read FAllSymbols write FAllSymbols;
    property DoCount: Boolean read FDoCount write FDoCount;
  end;

  TPasCheckParser = class(TDelphiParser)
  private
    FErrors: TPasCheckErrorFlags;
    FFileName: string;
    FUnitName: string;
  protected
    procedure ReadCommentBlock;
    procedure ReadUnitName;
    function Parse: Boolean; override;
  public
    function ExecuteFile(const AFileName: string): Boolean; override;
    property Errors: TPasCheckErrorFlags read FErrors;

    property FileName: string read FFileName;
    property UnitName: string read FUnitName;
  end;

  TFunctionParser = class(TDelphiParser)
  private
    FFunctions: TStrings;
    FSkipped: TStrings;
    FFileName: string;
  protected
    procedure ReadUntilImplementationBlock;
    function ReadUntilFunction: Boolean;
    procedure ReadFunction;

    procedure Init; override;
    function Parse: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ExecuteFile(const AFileName: string): Boolean; override;
    property Functions: TStrings read FFunctions;
    property Skipped: TStrings read FSkipped;
  end;

  TImplementationParser = class(TDelphiParser)
  private
    FOutputStream: TStream;
    FSortImplementation: Boolean;
  protected
    function Parse: Boolean; override;
  public
    property OutputStream: TStream read FOutputStream write FOutputStream;
    property SortImplementation: Boolean read FSortImplementation write FSortImplementation;
  end;

  TRecapitalizeParser = class(TDelphiParser)
  private
    FOutputStream: TStream;
  protected
    function Parse: Boolean; override;
  public
    property OutputStream: TStream read FOutputStream write FOutputStream;
  end;

implementation

uses
  SysUtils, Dialogs, Windows, Math, DelphiParserUtils;

resourcestring
  SCharExpected = '''''%s'''' expected';
  SCompilerDirectiveExpected = 'Compiler directive expected';
  SIdentifierExpected = 'Identifier expected';
  SNumberExpected = 'Number expected';
  SParseError = '%s on line %d';
  SStringExpected = 'String expected';
  SSymbolExpected = '%s expected';

type
  TCheckTextResult = (ctPrefix, ctEqual, ctDifferent);

  TPosition = (inConst, inType, inNull, inVar, inThreadVar, inResourceString);

const
  CParseBufSize = 4096 * 16;

  CAllowableSymbolsInTypeDef: array[0..4] of string = ('Low', 'High', 'Ord', 'Succ', 'Pred');

  CDefaultText: array[TDefaultText] of string = (
    'write here a summary (1 line)',
    'write here a description',
    'this type is used by (for reference):',
    'list here other properties, methods',
    'remove the ''see also'' section if there are no references',
    'describe here what the function returns',
    'this is an overridden method, you don''t have to describe these',
    'if it does the same as the inherited method',
    'Description for', // case sensitive
    'description for this parameter'
    );

  SInvalidBinary = 'Invalid binary value';
  SInvalidString = 'Invalid string constant';

  SLineTooLong = 'Line too long';

  SNotCharExpected = 'Not ''''%s'''' expected';
  SNotEofExpected = 'Not EOF expected';
  SNotIdentifierExpected = 'Not identifier expected';
  SNotNumberExpected = 'Not number expected';
  SNotStringExpected = 'Not string expected';

  //=== Local procedures =======================================================

function CheckText(const Short, Long: string; const CaseSensitive: Boolean): TCheckTextResult;
var
  AreEqual: Boolean;
begin
  if CaseSensitive then
    AreEqual := StrLComp(PChar(Short), PChar(Long), Length(Short)) = 0
  else
    AreEqual := StrLIComp(PChar(Short), PChar(Long), Length(Short)) = 0;

  if not AreEqual then
    Result := ctDifferent
  else
    if Length(Short) = Length(Long) then
    Result := ctEqual
  else
    Result := ctPrefix;
end;

function RemoveEndChars(const S: string; const Chars: TSysCharSet): string;
begin
  Result := S;
  while (Length(Result) > 0) and (Result[Length(Result)] in Chars) do
    Delete(Result, Length(Result), 1);
end;

function FirstChar(const S: string): Char;
begin
  if S = '' then
    Result := #0
  else
    Result := S[1];
end;

function DtxSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := TDtxBaseItem(Item1).CompareWith(TDtxBaseItem(Item2));
end;

function AddLeading(const S: string): string;
begin
  if (Length(S) < 2) or (S[1] <> '@') or (S[2] <> '@') then
    Result := '@@' + S
  else
    Result := S;
end;

function RemoveSepLines(const S: string): string;

  procedure AddString(StartPtr, EndPtr: PChar);
  var
    CurrentLength: Integer;
  begin
    if EndPtr <= StartPtr then
      Exit;
    CurrentLength := Length(Result);
    SetLength(Result, CurrentLength + EndPtr - StartPtr);
    Move(StartPtr^, (PChar(Result) + CurrentLength)^, EndPtr - StartPtr);
  end;
var
  P, Q, R: PChar;
begin
  P := PChar(S);
  R := P;
  while P^ <> #0 do
  begin
    while not (P^ in ['-', #0]) do
      Inc(P);

    Q := P;
    while P^ = '-' do
      Inc(P);
    if Q + 3 < P then { 3 = arbitrary }
    begin
      if P^ = #0 then
      begin
        AddString(R, Q);
        R := P;
      end
      else
        if P^ = #13 then
      begin
        Inc(P);
        if P^ = #10 then
        begin
          AddString(R, Q);
          Inc(P);
          R := P;
          Continue;
        end;
      end;
    end;
    while not (P^ in [#13, #10, #0]) do
      Inc(P);
    while P^ in [#13, #10] do
      Inc(P);
  end;
  AddString(R, P);
end;

//=== TBasicParser ===========================================================

procedure TBasicParser.AddTokenToRecordStr;
begin
  if FRecordStr > '' then
    FRecordStr := FRecordStr + ' ' + TokenString
  else
    FRecordStr := TokenString;
end;

procedure TBasicParser.AddToLowRecording(StartPtr, EndPtr: PChar);
var
  NeededSize: Integer;
  PtrPos: Integer;
begin
  Assert(FLowRecording, 'not recording');

  if StartPtr >= EndPtr then
    Exit;

  { copy size = FSourcePtr - FLowRecordPtr }
  if FLowRecordBuffer = nil then
  begin
    GetMem(FLowRecordBuffer, $4000);
    FLowRecordBufPtr := FLowRecordBuffer;
    FLowRecordBufEnd := FLowRecordBuffer + $4000;
  end;

  NeededSize := (FLowRecordBufPtr - FLowRecordBuffer) + (EndPtr - StartPtr);
  if FLowRecordBuffer + NeededSize > FLowRecordBufEnd then
  begin
    NeededSize := $4000 * ((NeededSize + $4000 - 1) div $4000);
    PtrPos := FLowRecordBufPtr - FLowRecordBuffer;
    ReallocMem(FLowRecordBuffer, NeededSize);
    FLowRecordBufEnd := FLowRecordBuffer + NeededSize;
    FLowRecordBufPtr := PtrPos + FLowRecordBuffer;
  end;

  Move(FLowRecordPtr^, FLowRecordBufPtr^, EndPtr - StartPtr);
  Inc(FLowRecordBufPtr, EndPtr - StartPtr);
end;

procedure TBasicParser.BeginLowRecording;
begin
  if not FLowRecording then
  begin
    FLowRecordPtr := FTokenPtr;
    FLowRecordBufPtr := FLowRecordBuffer;
    FLowRecording := True;
  end;
end;

procedure TBasicParser.BeginOutputTo(AStream: TStream);
begin
  FLowOutputStream := AStream;
end;

procedure TBasicParser.BeginRecording;
begin
  FRecording := True;
  FRecordStr := '';
end;

procedure TBasicParser.CheckNotToken(T: Char);
begin
  if Token = T then
    case T of
      toSymbol:
        Error(SNotIdentifierExpected);
      toString, toWString:
        Error(SNotStringExpected);
      toInteger, toFloat:
        Error(SNotNumberExpected);
      toEof:
        Error(SNotEofExpected);
    else
      ErrorFmt(SNotCharExpected, [T]);
    end;
end;

procedure TBasicParser.CheckToken(T: Char);
begin
  if Token <> T then
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      toString, toWString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
      toCompilerDirective:
        Error(SCompilerDirectiveExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
end;

procedure TBasicParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then
    ErrorFmt(SSymbolExpected, [S]);
end;

function TBasicParser.CheckTokenSymbolIn(const List: array of string): Integer;
var
  I: Integer;
  Msg: string;
begin
  Result := TokenSymbolIn(List);
  if Result < 0 then
  begin
    Msg := '';
    for I := Low(List) to High(List) do
    begin
      Msg := Msg + List[I];
      if I = High(List) - 1 then
        Msg := Msg + ' or '
      else
        if I < High(List) - 1 then
        Msg := Msg + ', ';
    end;
    Error(Msg + ' expected');
  end;
end;

constructor TBasicParser.Create;
begin
  inherited Create;
  GetMem(FBuffer, CParseBufSize);
  FCompilerDirectives := TStringList.Create;
end;

destructor TBasicParser.Destroy;
begin
  FCompilerDirectives.Free;
  if FBuffer <> nil then
  begin
    if Assigned(FStream) then
      FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
    FreeMem(FBuffer, CParseBufSize);
  end;
  FreeMem(FLowRecordBuffer);
  inherited Destroy;
end;

procedure TBasicParser.EndLowRecording;
begin
  if FLowRecording then
  begin
    AddToLowRecording(FLowRecordPtr, FSourcePtr);
    FLowRecording := False;
  end;
end;

procedure TBasicParser.EndLowRecordingWithoutCurrent;
begin
  if FLowRecording then
  begin
    AddToLowRecording(FLowRecordPtr, FTokenPtr);
    FLowRecording := False;
  end;
end;

procedure TBasicParser.EndOutputTo;
begin
  FLowOutputStream.WriteBuffer(FBuffer^, FSourcePtr - FBuffer);
  FLowOutputStream := nil;
end;

procedure TBasicParser.EndRecording;
begin
  FRecording := False;
end;

procedure TBasicParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TBasicParser.ErrorFmt(const Ident: string;
  const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

procedure TBasicParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateFmt(SParseError, [Message, FSourceLine]);
end;

procedure TBasicParser.ForgetRollBackPosition;
begin
  FSavedSourcePtr := nil;
  FSavedTokenPtr := nil;
  FSavedSourceLine := -1;
  FSavedToken := #0;
end;

function TBasicParser.GetLowRecordingStr: string;
begin
  if Assigned(FLowRecordBuffer) then
    SetString(Result, FLowRecordBuffer, FLowRecordBufPtr - FLowRecordBuffer)
  else
    Result := '';
end;

function TBasicParser.GetRecordStrWithCurrentToken: string;
begin
  Result := FRecordStr + ' ' + TokenString;
end;

procedure TBasicParser.Init;
begin
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + CParseBufSize;

  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;

  FTokenPtr := FBuffer;

  FLowRecording := False;
  FLowRecordPtr := nil;
  FLowRecordBufPtr := FLowRecordBuffer;

  FSavedSourcePtr := nil;
  FSavedTokenPtr := nil;

  FSourceLine := 1;
  LowNextToken(False);
end;

function TBasicParser.LowNextToken(SkipBlanks: Boolean): Char;
const
  CComments = [toComment, toSameLineComment];
  CBlankTokens = CComments + [#10];
begin
  if Recording and not (Token in CComments) then
    AddTokenToRecordStr;

  Result := ReadNextToken;

  while SkipBlanks and (Token in CBlankTokens) do
    Result := ReadNextToken;
end;

procedure TBasicParser.ReadBuffer;
var
  Count: Integer;
  Start: PChar;
  SourcePtrDiff, SavedSourcePtrDiff: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);

  if Assigned(FLowOutputStream) then
    FLowOutputStream.Write(FBuffer^, FSourcePtr - FBuffer);

  if LowRecording and (FSourcePtr > FLowRecordPtr) then
    AddToLowRecording(FLowRecordPtr, FSourcePtr);

  SourcePtrDiff := 0;
  SavedSourcePtrDiff := 0;

  if Assigned(FSavedSourcePtr) then
  begin
    Start := FSavedTokenPtr;
    SourcePtrDiff := FSourcePtr - Start;
    SavedSourcePtrDiff := FSavedSourcePtr - Start;
  end
  else
    Start := FSourcePtr;

  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - Start;
  if Count <> 0 then
    Move(Start[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  if FBufEnd = FBufPtr then
    raise Exception.Create('ReadBuffer Error');
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));

  Start := FBuffer;
  if Assigned(FSavedSourcePtr) then
  begin
    FSavedTokenPtr := Start;
    FSavedSourcePtr := Start + SavedSourcePtrDiff;
  end;
  FSourcePtr := Start + SourcePtrDiff;

  FLowRecordPtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then
      Error(SLineTooLong);
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

function TBasicParser.ReadPortion(CheckPosition: Integer): Boolean;
begin
  ReadBuffer;
  Result := (FSourcePtr + CheckPosition)^ <> #0;
end;

procedure TBasicParser.RollBackToSavedPosition;
begin
  FSourcePtr := FSavedSourcePtr;
  FTokenPtr := FSavedTokenPtr;
  FToken := FSavedToken;
  FSourceLine := FSavedSourceLine;

  ForgetRollBackPosition;
end;

procedure TBasicParser.SaveRollBackPosition;
begin
  FSavedSourcePtr := FSourcePtr;
  FSavedTokenPtr := FTokenPtr;
  FSavedToken := FToken;
  FSavedSourceLine := FSourceLine;
end;

procedure TBasicParser.SetLowRecording(const Value: Boolean);
begin
  if Value then
    BeginLowRecording
  else
    EndLowRecording;
end;

procedure TBasicParser.SetRecording(const Value: Boolean);
begin
  if Value then
    BeginRecording
  else
    EndRecording;
end;

procedure TBasicParser.SkipBlanks;
begin
  FLastWasNewLine := False;

  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          ReadBuffer;
          if FSourcePtr^ = #0 then
            Exit;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Exit;
    end;
    FLastWasNewLine := (FSourcePtr^ = #10) or (FLastWasNewLine and (FSourcePtr^ in [#0..#32]));
    Inc(FSourcePtr);
  end;
end;

function TBasicParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TBasicParser.TokenComponentIdent: string;
var
  P: PChar;
begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

function TBasicParser.TokenFloat: Extended;
begin
  if FFloatType <> #0 then
    Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> #0 then
    Inc(FSourcePtr);
end;

function TBasicParser.TokenInt: Int64;
begin
  Result := StrToInt64(TokenString);
end;

function TBasicParser.TokenString: string;
var
  L: Integer;
begin
  L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

function TBasicParser.TokenSymbolIn(const List: array of string): Integer;
var
  S: string;
begin
  if not (Token in [toSymbol, toCompilerDirective]) or (High(List) <= 0) then
  begin
    Result := -1;
    Exit;
  end;

  S := TokenString;
  Result := 0;
  while (Result <= High(List)) and not SameText(S, List[Result]) do
    Inc(Result);
  if Result > High(List) then
    Result := -1;
end;

function TBasicParser.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and SameText(S, TokenString);
end;

function TBasicParser.TokenSymbolIsExact(const S: string): Boolean;
begin
  Result := (Token = toSymbol) and (CompareStr(S, TokenString) = 0);
end;

function TBasicParser.TokenWideString: WideString;
begin
  if FToken = toString then
    Result := TokenString
  else
    Result := FWideStr;
end;

//=== TDelphiParser ==========================================================

procedure TDelphiParser.AddDefine(const S: string);
begin
  FDEFList.AddObject(S, TObject(dftIFDEF));
end;

procedure TDelphiParser.AddNotDefine(const S: string);
begin
  FDEFList.AddObject(S, TObject(dftIFNDEF));
end;

procedure TDelphiParser.BeginBlock;
begin
  if not Assigned(FBlockDEFList) then
  begin
    FBlockDEFList := TStringList.Create;
    TStringList(FBlockDEFList).Sorted := True;
    TStringList(FBlockDEFList).Duplicates := dupIgnore;
  end;

  FBlockDEFList.Clear;
  FBlockDEFList.Assign(DEFList);
end;

procedure TDelphiParser.CheckTokenSymbolC(const S: string);
begin
  CheckTokenSymbol(S);
  RecapitalizeTokenWith(S);
end;

function TDelphiParser.CheckTokenSymbolInC(const List: array of string): Integer;
begin
  Result := CheckTokenSymbolIn(List);
  if Result >= 0 then
    RecapitalizeTokenWith(List[Result]);
end;

constructor TDelphiParser.Create;
begin
  inherited Create;
  FPasItems := TPasItems.Create;
  FAcceptCompilerDirectives := TStringList.Create;
  TStringList(FAcceptCompilerDirectives).Sorted := True;
  FAcceptVisibilities := [inPublic, inPublished];
  FInterpretCompilerDirectives := True;
  FWantCompilerDirectives := False;
  FDEFList := TStringList.Create;
  FSavedDEFList := TStringList.Create;
  FCopiedDEFList := nil;
end;

destructor TDelphiParser.Destroy;
begin
  FCopiedDEFList.Free;
  FPasItems.Free;
  FAcceptCompilerDirectives.Free;
  FDEFList.Free;
  FSavedDEFList.Free;
  inherited Destroy;
end;

function TDelphiParser.DoNextToken(SkipBlanks: Boolean = True): Char;
var
  IfDefCount: Integer;
  SkipUntilIfDefCount0: Boolean;

  procedure HandleIFDEF;
  begin
    if InterpretCompilerDirectives then
    begin
      if SkipUntilIfDefCount0 then
      begin
        Inc(IfDefCount);
        Exit;
      end;
    end;

    AddDefine(TokenCompilerDirectiveArgument);

    if InterpretCompilerDirectives then
    begin
      FLastCompilerDirectiveAccepted :=
        FAcceptCompilerDirectives.IndexOf(TokenCompilerDirectiveArgument) >= 0;

      if not FLastCompilerDirectiveAccepted then
      begin
        IfDefCount := 1;
        SkipUntilIfDefCount0 := True;
      end;
    end;
  end;

  procedure HandleIFNDEF;
  begin
    if InterpretCompilerDirectives then
    begin
      if SkipUntilIfDefCount0 then
      begin
        Inc(IfDefCount);
        Exit;
      end;
    end;

    AddNotDefine(TokenCompilerDirectiveArgument);

    if InterpretCompilerDirectives then
    begin
      FLastCompilerDirectiveAccepted :=
        FAcceptCompilerDirectives.IndexOf(TokenCompilerDirectiveArgument) < 0;
      if not FLastCompilerDirectiveAccepted then
      begin
        IfDefCount := 1;
        SkipUntilIfDefCount0 := True;
      end;
    end;
  end;

  procedure HandleENDIF;
  begin
    if InterpretCompilerDirectives then
    begin
      if SkipUntilIfDefCount0 then
      begin
        Dec(IfDefCount);
        if IfDefCount > 0 then
          { Not yet done }
          Exit;

        SkipUntilIfDefCount0 := False;
      end;
    end;

    EndDefine;
  end;

  procedure HandleELSE;
  begin
    if not SkipUntilIfDefCount0 or (IfDefCount = 1) then
      SwitchDefine;

    if InterpretCompilerDirectives then
    begin
      if SkipUntilIfDefCount0 then
      begin
        if IfDefCount = 1 then
          Dec(IfDefCount);
        if IfDefCount > 0 then
          Exit;

        SkipUntilIfDefCount0 := False;
        Exit;
      end;

      if FLastCompilerDirectiveAccepted then
      begin
        IfDefCount := 1;
        SkipUntilIfDefCount0 := True;
      end;
    end;
  end;

begin
  { SkipBlanks                   - Do not return compiler directives; comments
    InterpretCompilerDirectives  - Do not return conditional directives
                                   Jump over non-defined blocks
    SkipUntilIfDefCount0         - Loop until $ENDIF encountered
    WantCompilerDirectives       - Return compiler directives
  }
  IfDefCount := 0;
  SkipUntilIfDefCount0 := False;

  while True do
  begin
    Result := LowNextToken(SkipBlanks);

    if (Token = toSymbol) and DoRecapitalize then
      RecapitalizeToken;

    if not SkipUntilIfDefCount0 then
    begin
      if Token <> toCompilerDirective then
        Exit;

      { Token = toCompilerDirective }

      if not TokenIsConditionalCompilation then
      begin
        if SkipBlanks then
          Continue
        else
          Exit;
      end;
    end
    else
    begin
      while not (Token in [toCompilerDirective, toEof]) do
        LowNextToken(True);

      if Token = toEof then
      begin
        Result := toEof;
        Exit;
      end;
    end;

    { Token = toCompilerDirective }

    case TokenCompilerDirective of
      cdIFDEF: HandleIFDEF;
      cdIFNDEF: HandleIFNDEF;
      cdENDIF: HandleENDIF;
      cdELSE: HandleELSE;
    end;

    if WantCompilerDirectives and not InterpretCompilerDirectives then
      Exit;
  end;
end;

procedure TDelphiParser.EndBlock(const AllowDoubleEnd: Boolean);

  function DoDefinesIndicateBlock(out ABlockDEFStr: string; out ABlockDEFType: TDefineType): Boolean;
  var
    Tmp: TStringList;
    I: Integer;
    Index: Integer;
  begin
    Tmp := TStringList.Create;
    try
      Tmp.Assign(FBlockDEFList);
      FBlockDEFList.Assign(DEFList);
      for I := 0 to Tmp.Count - 1 do
      begin
        Index := FBlockDEFList.IndexOf(Tmp[I]);
        if (Index >= 0) and (FBlockDEFList.Objects[Index] = Tmp.Objects[I]) then
          FBlockDEFList.Delete(Index);
      end;

      { We can only skip a block if FBlockDEFList.Count = 1 }
      if FBlockDEFList.Count = 1 then
      begin
        //uses                  <- BeginBlock
        //  A, B,
        //  {$IFDEF STANDALONE}
        //  C;                  <- EndBlock; return 'STANDALONE', ifdef
        //  {$ELSE}             |
        //  D;                  |- Inverse of 'STANDALONE', ifdef
        //  {$ENDIF}            |

        ABlockDEFStr := FBlockDEFList[0];
        ABlockDEFType := TDefineType(FBlockDEFList.Objects[0]);
        FBlockDEFList.Assign(Tmp);
        Result := True;
      end
      else
        if (FBlockDEFList.Count = 0) and (Tmp.Count > 0) and AllowDoubleEnd then
      begin
        // {$IFNDEF A}
        // procedure X(A: TA);    <- BeginBlock, EndBlock, return 'A', 'ifndef'
        // {$ELSE}                |
        // procedure X(A': TA');  |= inverse of 'A', 'ifndef'
        // {$ENDIF}               |

        ABlockDEFStr := Tmp[Tmp.Count - 1];
        ABlockDEFType := TDefineType(Tmp.Objects[Tmp.Count - 1]);

        FBlockDEFList.Clear;
        Result := True;
      end
      else
        Result := False;
    finally
      Tmp.Free;
    end;
  end;

  function DEFIndexOf(const S: string): Integer;
  begin
    if SameText(S, 'VisualCLX') then
      Result := 0
    else
      if SameText(S, 'VCL') then
      Result := 1
    else
      if SameText(S, 'MSWINDOWS') then
      Result := 2
    else
      if SameText(S, 'LINUX') then
      Result := 3
    else
      Result := -1;
  end;

  function IsInverse(
    const S1: string; const Type1: TDefineType;
    const S2: string; const Type2: TDefineType): Boolean;
  var
    Index1, Index2, Tmp: Integer;
  begin
    if SameText(S1, S2) then
      Result := [Type1] + [Type2] = [dftIFDEF, dftIFNDEF]
    else
    begin
      Result := Type1 = Type2;
      if not Result then
        Exit;
      Index1 := DEFIndexOf(S1);
      Index2 := DEFIndexOf(S2);
      if Index1 > Index2 then
      begin
        Tmp := Index1;
        Index1 := Index2;
        Index2 := Tmp;
      end;

      Result :=
        ((Index1 = 0) and (Index2 = 1)) or
        ((Index1 = 2) and (Index2 = 3));
    end;
  end;

  function IsInverseOf(const ABlockDEFStr: string; const ABlockDEFType: TDefineType): Boolean;
  var
    I: Integer;
  begin
    // Check whether FDEFList is the inverse of ABlock_

    Result := FDEFList.Count > 0;
    if not Result then
      Exit;

    for I := FDEFList.Count - 1 downto 0 do
    begin
      Result := IsInverse(
        ABlockDEFStr, ABlockDEFType,
        FDEFList[I], TDefineType(FDEFList.Objects[I]));
      if Result then
        Exit;
    end;

    Result := False;
  end;

var
  NewBlockDEFStr: string;
  NewBlockDEFType: TDefineType;
begin
  if not DoDefinesIndicateBlock(NewBlockDEFStr, NewBlockDEFType) then
    Exit;

  SaveRollBackPosition;
  DoNextToken;
  if not IsInverseOf(NewBlockDEFStr, NewBlockDEFType) then
  begin
    RollBackToSavedPosition;
    Exit;
  end;

  ForgetRollBackPosition;

  WantCompilerDirectives := True;
  while IsInverseOf(NewBlockDEFStr, NewBlockDEFType) do
  begin
    CheckNotToken(toEof);
    DoNextToken(False);
  end;
  WantCompilerDirectives := False;
end;

procedure TDelphiParser.EndDefine;
begin
  if FDEFList.Count = 0 then
    Error('Unexpected {$ENDIF}');
  FDEFList.Delete(FDEFList.Count - 1);
end;

function TDelphiParser.Execute(AStream: TStream): Boolean;
begin
  FStream := AStream;
  Init;
  try
    Result := False;
    try
      Result := Parse;
    except
      on E: Exception do
        FErrorMsg := E.Message;
    end;
  finally
    FStream := nil;
  end;
end;

function TDelphiParser.ExecuteFile(const AFileName: string): Boolean;
var
  LStream: TFileStream;
begin
  Result := FileExists(AFileName);
  if not Result then
    Exit;

  FPasItems.FileName := AFileName;

  LStream := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := Execute(LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TDelphiParser.ForgetRollBackPosition;
begin
  FSavedDEFList.Clear;
  inherited ForgetRollBackPosition;
end;

function TDelphiParser.GetDoRecapitalize: Boolean;
begin
  Result := Recapitalize and not FHoldRecapitalize and Assigned(FCapitalization);
end;

procedure TDelphiParser.Init;
begin
  inherited Init;
  FPasItems.Clear;
  FLastCompilerDirectiveAccepted := False;
end;

procedure TDelphiParser.MakeCopyOfDEFList;
begin
  if (FDEFList.Count > 0) and not Assigned(FCopiedDEFList) then
    FCopiedDEFList := TStringList.Create;

  if Assigned(FCopiedDEFList) then
    FCopiedDEFList.Assign(FDEFList);
end;

function TDelphiParser.Parse: Boolean;
var
  LModuleType: TModuleType;
begin
  ReadCommentBlock;
  ReadUnitBlock(LModuleType);
  if LModuleType = mtLibrary then
  begin
    Result := True;
    Exit;
  end;
  ReadInterfaceStatement;
  SkipUsesBlock;
  ReadInterfaceBlock;
  FErrorMsg := '';
  FPasItems.DtxSort;
  FPasItems.CalculateCombines;

  Result := True;
end;

function TDelphiParser.ReadClass: TAbstractItem;
var
  LAncestor: string;
begin
  { Example:

    Tx = class(Tx1) .. end;
    Tx = class .. end;
    Tx = class;               -> Deze niet toevoegen, moet nog compleet
                                 gedefinieerd worden
    Tx = class(Tx1);          -> Hack class
    Tx = class of Tx1;        -> Meta class

    PRE : Token = 'class'
    POST: Token = ;
  }

  DoNextToken;
  if Token = toSemiColon then
  begin
    { Class is nog niet compleet gedefinieerd; niet toevoegen }
    Result := nil;
    Exit;
  end;

  if Token = toLeftParens then
  begin
    DoNextToken;
    CheckToken(toSymbol);
    LAncestor := TokenString;
    SkipUntilToken(toRightParens);
    DoNextToken;
  end;

  { Now add }
  if TokenSymbolIs('of') then
  begin
    Result := TMetaClassItem.Create('');
    FPasItems.Add(Result);
    DoNextToken;
    CheckToken(toSymbol);
    TMetaClassItem(Result).Value := TokenString;

    SkipUntilToken(toSemiColon);
  end
  else
  begin
    Result := TClassItem.Create('');
    TClassItem(Result).Ancestor := LAncestor;
    FPasItems.Add(Result);

    if Token <> toSemiColon then
      ReadClassMethods(TClassItem(Result)); { Token = ; or directive }
  end;
end;

procedure TDelphiParser.ReadClassMethods(AClassItem: TClassItem);
var
  Position: TClassVisibility;
begin
  { vb

    public property P1; end;

    PRE : Token = token after 'class'
    POST: Token = ; (= first token after 'end')
  }
  Position := inPublic;
  while True do
  begin
    CheckNotToken(toEof);

    if Token = toSymbol then
      case TokenSymbolInC(['end', 'private', 'protected', 'public', 'published',
        'property', 'procedure', 'constructor', 'destructor', 'function', 'class']) of
        0: { end }
          begin
            DoNextToken;
            { token = directive or ; }
            Exit;
          end;
        1: { private }
          begin
            Position := inPrivate;
            DoNextToken;
          end;
        2: { protected }
          begin
            Position := inProtected;
            DoNextToken;
          end;
        3: { public }
          begin
            Position := inPublic;
            DoNextToken;
          end;
        4: { published }
          begin
            Position := inPublished;
            DoNextToken;
          end;
        5: { property }
          if Position in AcceptVisibilities + [inProtected, inPublic, inPublished] then
            ReadClass_Property(AClassItem, Position)
          else
            SkipClass_Property;
        6, 7, 8: { procedure, constructor, destructor }
          if Position in AcceptVisibilities then
            ReadClass_Procedure(AClassItem, Position)
          else
            SkipClass_Procedure;
        9: {function }
          if Position in AcceptVisibilities then
            ReadClass_Function(AClassItem, Position)
          else
            SkipClass_Function;
        10: {class }
          if Position in AcceptVisibilities then
            ReadClass_ClassMethod(AClassItem, Position)
          else
            SkipClass_ClassMethod;
      else { field }
        if Position in AcceptVisibilities then
          ReadClass_Field(AClassItem, Position)
        else
          SkipClass_Field;
      end
    else { not a symbol, probably ; }
      DoNextToken;
  end;
end;

function TDelphiParser.ReadClassProcedureFunctionImpl(const DoAdd: Boolean): TAbstractItem;
begin
  Assert(FHoldRecapitalize, 'cap = true');

  BeginLowRecording;

  DoNextToken;
  CheckToken(toSymbol);
  case TokenSymbolInC(['function', 'procedure']) of
    0: Result := ReadFunctionImpl(DoAdd);
    1: Result := ReadProcedureImpl(DoAdd);
  else
    Result := nil;
    Error('function or procedure expected');
  end;
end;

function TDelphiParser.ReadClass_ClassMethod(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
begin
  DoNextToken;
  CheckToken(toSymbol);

  case TokenSymbolInC(['function', 'procedure']) of
    0: Result := ReadClass_Function(AClassItem, Position);
    1: Result := ReadClass_Procedure(AClassItem, Position);
  else
    Result := nil;
    Error('function or procedure expected');
  end;
  if Result is TParamClassMethodItem then
    TParamClassMethodItem(Result).IsClassMethod := True;
end;

function TDelphiParser.ReadClass_Field(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
var
  ClassField: TClassFieldItem;
begin
  { Example:

    FErrorMsg: string;

    Pre : Token = field name
    POST: Token = ;
  }

  Result := TClassFieldItem.Create(TokenString);
  FPasItems.Add(Result);

  ClassField := Result as TClassFieldItem;
  ClassField.OwnerClass := AClassItem;
  ClassField.Position := Position;

  { TODO : Can be multiple fields ie:
           FX, FY: Integer
  }
  SkipUntilToken(toColon);
  DoNextToken;

  BeginRecording;
  SkipType(False, True);
  //SkipUntilToken(toSemiColon);
  EndRecording;

  ClassField.TypeStr := RecordStrWithoutCurrentToken;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadClass_Function(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
var
  MethodFunc: TMethodFuncItem;
  LMethodName: string;
  Directives: TDirectives;
begin
  { Example:

    function F1(P1: T1; P2: T2);
    function IMalloc.Alloc = Allocate;

    PRE : Token = 'function'
    POST: Token = ;
  }

  DoNextToken;
  CheckToken(toSymbol);

  LMethodName := TokenString;
  DoNextToken;
  if Token = '.' then
  begin
    { Method resolution clause }
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toEquals);
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toSemiColon);

    Result := nil;
    Exit;
  end;

  Result := TMethodFuncItem.Create(LMethodName);
  FPasItems.Add(Result);

  MethodFunc := Result as TMethodFuncItem;
  MethodFunc.OwnerClass := AClassItem;
  MethodFunc.Position := Position;
  {MethodFunc.IsClassMethod := IsClassMethod;}

  if Token = toLeftParens then
    ReadParamList(MethodFunc.Params, MethodFunc.ParamTypes);

  CheckToken(toColon);
  DoNextToken;
  SkipType(False, False);

  Directives := [];
  ReadDirectives(True, Directives);
  MethodFunc.Directives := Directives;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadClass_Procedure(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
var
  MethodProc: TMethodProcItem;
  MethodType: TMethodType;
  Directives: TDirectives;
  LMethodName: string;
begin
  { Example:

    procedure P1(P1: T1; P2: T2);

    PRE : Token = 'procedure' or 'constructor' or 'destructor'
    POST: Token = ;
  }

  case TokenSymbolInC(['constructor', 'destructor']) of
    0: MethodType := mtConstructor;
    1: MethodType := mtDestructor;
  else
    MethodType := mtNormal;
  end;

  DoNextToken;
  CheckToken(toSymbol);
  LMethodName := TokenString;

  DoNextToken;
  if Token = toDot then
  begin
    { Method resolution clause }
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toEquals);
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toSemiColon);

    Result := nil;
    Exit;
  end;

  Result := TMethodProcItem.Create(LMethodName);
  FPasItems.Add(Result);

  MethodProc := Result as TMethodProcItem;
  MethodProc.OwnerClass := AClassItem;
  MethodProc.MethodType := MethodType;
  MethodProc.Position := Position;
  {MethodProc.IsClassMethod := IsClassMethod;}

  if Token = toLeftParens then
    ReadParamList(MethodProc.Params, MethodProc.ParamTypes);

  Directives := [];
  ReadDirectives(True, Directives);
  MethodProc.Directives := Directives;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadClass_Property(AClassItem: TClassItem; Position: TClassVisibility): TAbstractItem;
var
  I: Integer;
  Specifiers: TPropertySpecifiers;
  ClassProperty: TClassPropertyItem;
begin
  { Bv:

    propery P1: X1 read Get1 write Get2
    property P1;
    property P1 default X;
    property Bold[Year, Month, Day: Word]: Boolean read IsBold write SetBold;

    Pre : Token = 'property'
    POST: Token = ;
  }

  DoNextToken;
  CheckNotToken(toEof);

  Result := TClassPropertyItem.Create(TokenString);
  FPasItems.Add(Result);

  ClassProperty := Result as TClassPropertyItem;

  ClassProperty.OwnerClass := AClassItem;
  ClassProperty.Position := Position;

  DoNextToken;
  if (Token = toSemiColon) or
    ((Token = toSymbol) and (TokenSymbolInC(['default', 'stored']) >= 0)) then
  begin
    ClassProperty.IsInherited := True;
    SkipUntilToken(toSemiColon);
  end
  else
  begin
    ClassProperty.IsInherited := False;
    ClassProperty.IsArray := Token = toLeftBracket;
    if Token = toLeftBracket then
      with ClassProperty do
        ReadParamList(Params, ParamTypes, htBracket);
    SkipUntilToken(toSymbol);

    BeginRecording;
    repeat
      DoNextToken;
    until
      (Token in [toEof, toSemiColon]) or (TokenSymbolInC(CPropertySpecifiers) >= 0);
    EndRecording;

    ClassProperty.TypeStr := RecordStrWithoutCurrentToken;

    while not (Token in [toSemiColon, toEof]) do
    begin
      I := TokenSymbolInC(CPropertySpecifiers);
      if (I >= Integer(Low(TPropertySpecifier))) and (I <= Integer(High(TPropertySpecifier))) then
        ClassProperty.Specifiers := ClassProperty.Specifiers + [TPropertySpecifier(I)];
      DoNextToken;
    end;
  end;

  CheckToken(toSemiColon);

  Specifiers := [];
  ReadPropertySpecifiers(True, Specifiers);
  if Assigned(ClassProperty) then
    ClassProperty.Specifiers := ClassProperty.Specifiers + Specifiers;

  CheckToken(toSemiColon);
end;

procedure TDelphiParser.ReadCommentBlock;
const
  CCopyRight1 = 'Initial Developer of the Original Code is';
  CCopyRight2 = 'Initial Developers of the Original Code are:';
var
  AuthorFound: Boolean;
  S, T: string;
  P: Integer;
  Q, Q1, Q2, R: PChar;
  SpacesFound: Integer;
begin
  AuthorFound := False;
  while (Token = toComment) or
    ((Token = toCompilerDirective) and (TokenCompilerDirective in CParamDirectives)) do
  begin
    if not AuthorFound then
    begin
      S := TokenString;
      P := Pos(CCopyRight1, S);
      Q := nil;
      if P > 0 then
        Q := PChar(S) + P + Length(CCopyRight1)
      else
      begin
        P := Pos(CCopyRight2, S);
        if P > 0 then
          Q := PChar(S) + P + Length(CCopyRight2);
      end;
      AuthorFound := P > 0;
      if AuthorFound then
      begin
        R := Q;
        Q1 := AnsiStrPos(Q, 'Copyright');
        Q2 := AnsiStrPos(Q, 'Portions');

        if Q1 = nil then
          Q := Q2
        else
          if (Q2 = nil) or (Q1 < Q2) then
          Q := Q1
        else
          Q := Q2;

        if Q = nil then
        begin
          SpacesFound := 0;
          Q := R;
          while (Q^ <> #0) and (SpacesFound < 2) do
          begin
            if Q^ = ' ' then
              Inc(SpacesFound);
            Inc(Q);
          end;
        end;
        SetLength(T, Q - R);
        Move(R^, PChar(T)^, Q - R);

        { Remove e-mail address }
        if Pos(toLeftBracket, T) > 0 then
          T := Copy(T, 1, Pos(toLeftBracket, T) - 1);
        if Pos('<', T) > 0 then
          T := Copy(T, 1, Pos('<', T) - 1);

        FPasItems.Author := Trim(T);
      end;
    end;
    DoNextToken(False);
  end;

  while Token <> toSymbol do
    DoNextToken;
end;

function TDelphiParser.ReadCommentImpl(const DoAdd: Boolean): TAbstractItem;
begin
  Assert(FHoldRecapitalize, 'cap = true');

  if not (Token in [toComment, toSameLineComment]) then
    Error('Internal error: Comment expected');

  if DoAdd then
  begin
    Result := TCommentItem.Create(TokenString);
    TCommentItem(Result).IsSameLineComment := Token = toSameLineComment;
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken(False);
end;

function TDelphiParser.ReadCompilerDirectiveImpl(
  const DoAdd: Boolean): TAbstractItem;
begin
  Assert(FHoldRecapitalize, 'cap = true');

  CheckToken(toCompilerDirective);
  if DoAdd then
  begin
    MakeCopyOfDEFList;
    Result := TCompilerDirectiveItem.Create(TokenString);
    Result.BeginDEFList := FCopiedDEFList;
    Result.EndDEFList := FDEFList;
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken(False);
end;

function TDelphiParser.ReadConst: TAbstractItem;
var
  ConstItem: TConstItem;
begin
  { Example:

    C1 = V1;
    C1: type = V1;
    C1: array [xx] of TSpecialFolderInfo = ((), (), ());
    C1: function (AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction = nil;

    PRE : Token = identifier [C1]
    POST: Token = ;
  }

  ConstItem := TConstItem.Create(TokenString);
  FPasItems.Add(ConstItem);
  Result := ConstItem;

  BeginRecording;

  DoNextToken;
  case Token of
    toEquals:
      begin
        DoNextToken;
        SkipConstValue;

        EndRecording;
        if TokenSymbolInC(CDirectives) > 0 then
          ConstItem.Value := RecordStrWithoutCurrentToken
        else
          ConstItem.Value := RecordStr;
        SkipDirectives(False);
      end;
    toColon:
      begin
        DoNextToken;
        SkipType(False, True);
        CheckToken(toEquals);
        DoNextToken;
        SkipConstValue;
        ConstItem.Value := RecordStr;
      end;
  else
    Error('= or : expected');
  end;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadConstImpl(const DoAdd: Boolean): TAbstractItem;
begin
  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  Result := SkipConst(DoAdd);

  CheckToken(toSemiColon);

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

procedure TDelphiParser.ReadConstVarTypeSection;
var
  Position: TPosition;
begin
  Position := inNull;
  while Token <> toEof do
  begin
    StopCapitalization;

    case Token of
      toSymbol:
        case TokenSymbolInC([
          'procedure', 'function', 'resourcestring',
            'const', 'type', 'var', 'begin', 'end', 'asm', 'label']) of
          0: ReadProcedureImpl(False);
          1: ReadFunctionImpl(False);
          2: { resourecestring }
            begin
              Position := inResourceString;
              DoNextToken;
            end;
          3: { const }
            begin
              Position := inConst;
              DoNextToken;
            end;
          4: { type }
            begin
              Position := inType;
              DoNextToken;
            end;
          5: { var }
            begin
              Position := inVar;
              DoNextToken;
            end;
          6, 8: { begin }
            Exit;
          7: { end }
            CheckTokenSymbol('begin');
          9: { label }
            SkipUntilToken(toSemiColon);
        else
          case Position of
            inConst: ReadConstImpl(False);
            inType: ReadTypeDefImpl(False);
            inVar: ReadVarImpl(False);
            //inThreadVar: ReadThreadVarImpl(False); { can not be not here }
            inResourceString: ReadResourceStringImpl(False);
          else
            Error('Not in Type, Const, Var');
          end;
        end;
    else
      DoNextToken(False);
    end;
  end;

  while (Token <> toEof) and
    ((Token <> toSymbol) or not SameText(TokenString, 'implementation')) do
    DoNextToken;
end;

procedure TDelphiParser.ReadDirectives(const MaySkipSemiColon: Boolean; var Directives: TDirectives);
var
  Directive: Integer;
begin
  {
    Pre  : Token = ;  or some directive
    Post : Token = ; or =
  }

  StopCapitalization;

  if ((Token <> toSemiColon) or not MaySkipSemiColon) and
    ((Token <> toSymbol) or (TokenSymbolInC(CDirectives) < 0)) then
    Exit;

  while True do
  begin
    if Token = toSemiColon then
    begin
      SaveRollBackPosition;
      DoNextToken;
      if (Token <> toSymbol) or (TokenSymbolInC(CDirectives) < 0) then
      begin
        RollBackToSavedPosition;
        Exit;
      end;
      ForgetRollBackPosition;
    end;

    while Token = toSymbol do
    begin
      Directive := TokenSymbolInC(CDirectives);
      if Directive < 0 then
        Error('Directive expected')
      else
        Include(Directives, TDirective(Directive));

      case TDirective(Directive) of
        diMessage, diExternal:
          begin
            { message X ; }
            DoNextToken;
            StartCapitalization;
            SkipUntilToken(toSemiColon);
            StopCapitalization;
          end;
      else
        DoNextToken;
      end;
    end;

    if Token = toEquals then
      Exit;

    CheckToken(toSemiColon);
  end;
end;

function TDelphiParser.ReadDispInterface: TAbstractItem;
begin
  { TODO: Implement }
  SkipUntilSymbol('end');
  SkipUntilToken(toSemiColon);
  Result := nil;
end;

function TDelphiParser.ReadEnumerator: TAbstractItem;
var
  EnumItem: TEnumItem;
  NewValue: Boolean;
  Haakjes: Integer;
begin
  { Example:

    (E1, E2, E3);
    ();
    (Small = 5, Medium = 10, Large = Small + Medium);

    PRE : Token = (
    POST: Token = ;
  }

  EnumItem := TEnumItem.Create('');
  FPasItems.Add(EnumItem);
  Result := EnumItem;

  { NewValue hebben we nodig om bv in '(Small = 5, ..' 5 niet mee te nemen }
  NewValue := True;
  Haakjes := 0;
  while True do
  begin
    CheckNotToken(toEof);
    case Token of
      toSemiColon:
        if Haakjes = 0 then
          Exit;
      toLeftParens:
        Inc(Haakjes);
      toRightParens:
        begin
          Dec(Haakjes);
          if Haakjes = 0 then
          begin
            DoNextToken;
            Exit;
          end;
        end;
      toSymbol:
        if NewValue then
        begin
          EnumItem.Items.Add(TokenString);
          NewValue := False;
        end;
      toComma:
        NewValue := True;
    end;
    DoNextToken;
  end;
end;

procedure TDelphiParser.ReadFunction;
var
  FunctionItem: TFunctionItem;
  Directives: TDirectives;
begin
  { Example:

    function F1(P1: T1; P2: T2);

    PRE : Token = 'function'
    POST: Token = ;
  }
  CheckTokenSymbol('function');
  DoNextToken;
  CheckToken(toSymbol);

  FunctionItem := TFunctionItem.Create(TokenString);
  FPasItems.Add(FunctionItem);
  DoNextToken;

  { Token = toLeftParens or toColon }
  if Token = toLeftParens then
    ReadParamList(FunctionItem.Params, FunctionItem.ParamTypes);

  CheckToken(toColon);
  DoNextToken;

  SkipType(False, False);

  Directives := [];
  ReadDirectives(True, Directives);
  FunctionItem.Directives := Directives;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadFunctionHeader(const DoAdd: Boolean; out IsOnlyHeader: Boolean): TAbstractItem;
var
  FirstToken: string;
  Directives: TDirectives;
begin
  CheckTokenSymbolC('function');

  DoNextToken;

  CheckToken(toSymbol);
  StartCapitalization;

  FirstToken := TokenString;
  DoNextToken;
  if Token = '.' then
  begin
    DoNextToken;
    CheckToken(toSymbol);

    if DoAdd then
    begin
      Result := TMethodFuncItem.Create(TokenString);
      FPasItems.Add(Result);

      TMethodFuncItem(Result).OwnerClassAsString := FirstToken;
    end
    else
      Result := nil;

    DoNextToken;
  end
  else
  begin
    if DoAdd then
    begin
      Result := TFunctionItem.Create(FirstToken);
      FPasItems.Add(Result);
    end
    else
      Result := nil;
  end;

  { Token = toLeftParens or toColon }
  if Token = toLeftParens then
    SkipParamList;

  { Token may be ; (ie no result in the implementation) or : }
  if Token <> ';' then
  begin
    CheckToken(toColon);
    DoNextToken;
    SkipType(False, False);
  end;
  Directives := [];
  ReadDirectives(True, Directives);
  IsOnlyHeader := [diExternal, diForward] * Directives <> [];

  StopCapitalization;
  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadFunctionImpl(const DoAdd: Boolean): TAbstractItem;
var
  OnlyHeader: Boolean;
begin
  Assert(FHoldRecapitalize, 'cap = true');

  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  BeginBlock;
  Result := ReadFunctionHeader(DoAdd, OnlyHeader);
  EndBlock(True);

  if not OnlyHeader then
  begin
    BeginBlock;
    ReadFunctionProcedureBody;
    EndBlock(False);
  end;

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

procedure TDelphiParser.ReadFunctionProcedureBody;
var
  BeginCount: Integer;
  InASM: Boolean;
begin
  ReadConstVarTypeSection;

  InASM := CheckTokenSymbolInC(['asm', 'begin']) = 0;
  if InASM then
    StopCapitalization
  else
    StartCapitalization;
  BeginCount := 0;
  while True do
  begin
    CheckNotToken(toEof);
    if Token = toSymbol then
      case TokenSymbolInC(['begin', 'case', 'try', 'asm', 'end']) of

        0, 1, 2: { begin/case/try }
          Inc(BeginCount);
        3:
          begin
            { asm }
            Inc(BeginCount);
            InASM := True;
            StopCapitalization;
          end;
        4: { end }
          begin
            Dec(BeginCount);
            if BeginCount = 0 then
            begin
              DoNextToken;
              Exit;
            end;

            if InASM then
            begin
              InASM := False;
              StartCapitalization;
            end;
          end;
      end;
    DoNextToken;
  end;
end;

function TDelphiParser.ReadFunctionType: TAbstractItem;
var
  Directives: TDirectives;
begin
  { Example:

    F1 = Function();
    TODO : Deze gaat fout : dwz ReadType leest ook directives in
    function(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer cdecl = nil;
    T17 : function(arg0: PSSL_CTX; str: PChar):Integer cdecl = nil;
          __________________________________________________

    PRE : Token = 'function'
    POST: Token = ;
  }

  Result := TFunctionTypeItem.Create('');
  FPasItems.Add(Result);

  DoNextToken;

  { Token is toLeftParens or toSemiColon or 'of object' or ?? }
  if Token = toLeftParens then
    with TFunctionTypeItem(Result) do
      ReadParamList(Params, ParamTypes);

  CheckToken(toColon);
  DoNextToken;
  SkipType(False, False);

  Directives := [];
  ReadDirectives(True, Directives);
  TFunctionTypeItem(Result).Directives := Directives;

  if not (Token in [toSemiColon, toEquals]) then
    Error('; or = expected');
end;

procedure TDelphiParser.ReadImplementationBlock;
var
  Position: TPosition;
begin
  Position := inNull;

  while Token <> toEof do
  begin
    StopCapitalization;
    case Token of
      toCompilerDirective:
        ReadCompilerDirectiveImpl(True);
      toComment, toSameLineComment:
        ReadCommentImpl(True);
      toSymbol:
        case TokenSymbolInC([
          'class',
            'procedure', 'constructor', 'destructor', 'function', 'resourcestring',
            'const', 'type', 'var', 'threadvar', 'end', 'initialization', 'uses']) of
          0: ReadClassProcedureFunctionImpl(True);
          1, 2, 3: ReadProcedureImpl(True);
          4: ReadFunctionImpl(True);
          5: { resourcestring}
            begin
              Position := inResourceString;
              DoNextToken(False);
            end;
          6: { const }
            begin
              Position := inConst;
              DoNextToken(False);
            end;
          7: { type }
            begin
              Position := inType;
              DoNextToken(False);
            end;
          8: { var }
            begin
              Position := inVar;
              DoNextToken(False);
            end;
          9: { threadvar }
            begin
              Position := inThreadVar;
              DoNextToken(False);
            end;
          10: { end }
            begin
              DoNextToken;
              CheckToken('.');
              Exit;
            end;
          11: { initialization }
            ReadInitializationFinalization;
          12: { uses }
            ReadUsesBlock(True);
        else
          case Position of
            inConst: ReadConstImpl(True);
            inType: ReadTypeDefImpl(True);
            inVar: ReadVarImpl(True);
            inThreadVar: ReadThreadVarImpl(True);
            inResourceString: ReadResourceStringImpl(True);
          else
            Error('Not in Type, Const, Var');
          end;
        end;
    else
      DoNextToken(False);
    end;
  end;
end;

procedure TDelphiParser.ReadInitializationFinalization;
var
  BeginCount: Integer;
  S: string;
begin
  { PRE: Token = 'initialization'
    POST: Token = 'end'
  }

  BeginLowRecording;

  BeginCount := 1;
  while True do
  begin
    CheckNotToken(toEof);
    if Token = toSymbol then
      case TokenSymbolInC(['begin', 'case', 'try', 'asm', 'end']) of

        0, 1, 2, 3: { begin/case/try/asm }
          Inc(BeginCount);
        4: { end }
          begin
            Dec(BeginCount);
            if BeginCount = 0 then
            begin
              EndLowRecording;

              { Minus the 'end' }
              S := LowRecordingStr;
              FPasItems.Add(TInitializationFinaliziationItem.Create(
                Copy(S, 1, Length(S) - 3)));

              DoNextToken;
              Exit;
            end;
          end;
      end;

    DoNextToken;
  end;
end;

procedure TDelphiParser.ReadInterfaceBlock;
var
  Position: TPosition;
begin
  Position := inNull;
  StopCapitalization;

  while Token <> toEof do
    case Token of
      toSymbol:
        case TokenSymbolInC([
          {0}'implementation',
          {1}'procedure',
          {2}'function',
          {3}'resourcestring',
          {4}'const',
          {5}'type',
          {6}'var',
          {7}'threadvar']) of

          0: Exit;
          1: ReadProcedure;
          2: ReadFunction;
          3:
            begin
              Position := inResourceString;
              DoNextToken;
            end;
          4:
            begin
              Position := inConst;
              DoNextToken;
            end;
          5:
            begin
              Position := inType;
              DoNextToken;
            end;
          6: { var }
            begin
              Position := inVar;
              DoNextToken;
            end;
          7: { threadvar }
            begin
              Position := inThreadVar;
              DoNextToken;
            end
        else
          case Position of
            inConst: ReadConst;
            inType: ReadTypeDef;
            inVar: ReadVar;
            inThreadVar: ReadThreadVar;
            inResourceString: ReadResourceString;
          else
            Error('Not in Type, Const, Var');
          end;
        end;
    else
      DoNextToken;
    end;

  while (Token <> toEof) and
    ((Token <> toSymbol) or not SameText(TokenString, 'implementation')) do
    DoNextToken;
end;

procedure TDelphiParser.ReadInterfaceMethods(AInterfaceItem: TInterfaceItem);
begin
  while True do
  begin
    CheckNotToken(toEof);

    case TokenSymbolInC(['end', 'property', 'procedure', 'function']) of
      0: { end }
        begin
          DoNextToken;
          CheckToken(toSemiColon);
          Exit;
        end;
      1: { property }
        ReadClass_Property(AInterfaceItem, inPublic);
      2: { procedure }
        ReadClass_Procedure(AInterfaceItem, inPublic);
      3: { function }
        ReadClass_Function(AInterfaceItem, inPublic);
    else
      DoNextToken;
    end;
  end;
end;

procedure TDelphiParser.ReadInterfaceStatement;
begin
  CheckTokenSymbol('interface');
  DoNextToken(False);
end;

function TDelphiParser.ReadInterfaceType: TAbstractItem;
var
  InterfaceItem: TInterfaceItem;
begin
  (* vb

    IJvDataConsumer = interface;
    IJvDataConsumer = interface
    ['{B2F18D03-F615-4AA2-A51A-74D330C05C0E}']

    PRE : Token = 'interface'
    POST: Token = ;
  *)
  DoNextToken;
  if Token = toSemiColon then
  begin
    { Class is nog niet compleet gedefinieerd; niet toevoegen }
    Result := nil;
    Exit;
  end;

  if Token = toLeftBracket then
    SkipUntilToken(toRightBracket);

  { Nu pas toevoegen }
  InterfaceItem := TInterfaceItem.Create('');
  FPasItems.Add(InterfaceItem);
  Result := InterfaceItem;

  ReadInterfaceMethods(InterfaceItem);
  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadNextToken: Char;
var
  I: Integer;
  P: PChar;
  LStartSourceLine: Integer;

  function IsEndReached: Boolean;
  var
    Position: Integer;
  begin
    { P^ = #0 -> Einde buffer }
    Result := P^ = #0;
    if not Result then
      Exit;

    { Kan nog iets ingelezen worden? }
    Position := P - FSourcePtr;
    Result := ReadPortion(Position);
    P := FSourcePtr + Position;
  end;

  procedure NextChar;
  begin
    Inc(P);
    if P^ = #10 then
      Inc(FSourceLine);
    if P^ = #0 then
      IsEndReached;
  end;

begin
  { Let op: !!!!!!!!!!!!

    geen multi-exit; FToken moet nog gezet worden }

  LStartSourceLine := FSourceLine;
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    '{':
      begin
        NextChar;
        if P^ <> '$' then
        begin
          while P^ <> '}' do
            NextChar;
          if P^ = '}' then
            NextChar;
          if LStartSourceLine = FSourceLine then
            Result := toSameLineComment
          else
            Result := toComment;
        end
        else
        begin
          { A compiler directive starts with a $ as the first character after
            the opening comment delimiter, immediately followed by a name (one
            or more letters) }
          NextChar;
          while not (P^ in [#0, '}']) do
            NextChar;
          if P^ = '}' then
            NextChar;
          Result := toCompilerDirective;
        end;
      end;
    '(':
      begin
        NextChar;
        if P^ = '*' then
        begin
          NextChar;
          repeat
            while P^ <> '*' do
              NextChar;
            if P^ = '*' then
              NextChar;
          until P^ in [#0, ')'];
          if P^ = ')' then
            NextChar;

          if LStartSourceLine = FSourceLine then
            Result := toSameLineComment
          else
            Result := toComment;
        end
        else
          Result := toLeftParens;
      end;
    'A'..'Z', 'a'..'z', '_':
      begin
        NextChar;
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
          NextChar;
        Result := toSymbol;
      end;
    '#', '''':
      begin
        while True do
          case P^ of
            '#':
              begin
                NextChar;
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  NextChar;
                end;
              end;
            '''':
              begin
                NextChar;
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        NextChar;
                        if P^ <> '''' then
                          Break;
                      end;
                  end;
                  NextChar;
                end;
              end;
          else
            Break;
          end;
        Result := toString;
      end;
    '$':
      begin
        NextChar;
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do
          NextChar;
        Result := toInteger;
      end;
    '-', '0'..'9':
      begin
        NextChar;
        while P^ in ['0'..'9'] do
          NextChar;
        Result := toInteger;
        if P^ = '.' then
        begin
          NextChar;
          if P^ = '.' then
            Dec(P)
          else
            Result := toFloat;
        end;
        while P^ in ['0'..'9', 'e', 'E', '+', '-'] do
        begin
          NextChar;
          Result := toFloat;
        end;
        if P^ in ['c', 'C', 'd', 'D', 's', 'S'] then
        begin
          Result := toFloat;
          FFloatType := P^;
          NextChar;
        end
        else
          FFloatType := #0;
      end;
    '.':
      begin
        NextChar;
        if P^ = '.' then
        begin
          Result := toDotDot;
          NextChar;
        end
        else
          Result := toDot;
      end;
    '/':
      begin
        NextChar;
        if P^ = '/' then
        begin
          if LStartSourceLine = FSourceLine then
            Result := toSameLineComment
          else
            Result := toComment;

          while not (P^ in [#13, #10, #0]) do
            NextChar;
          //while P^ in [#13, #10] do
          //  NextChar;
        end
        else
          Result := '/';
      end;
  else
    Result := P^;
    if Result <> toEof then
      NextChar;
  end;

  FSourcePtr := P;
  FToken := Result;
end;

procedure TDelphiParser.ReadParamList(AParams, ATypes: TStrings; const HaakType: THaakType);
const
  CLeftHaak: array[THaakType] of Char = (toLeftParens, toLeftBracket);
  CRightHaak: array[THaakType] of Char = (toRightParens, toRightBracket);

  procedure EndParamTypeRecording;
  begin
    if not Recording then
    begin
      if Assigned(ATypes) and Assigned(AParams) then
        while ATypes.Count < AParams.Count do
          ATypes.Add('');
      Exit;
    end;

    if Assigned(ATypes) and Assigned(AParams) then
      while ATypes.Count < AParams.Count do
        ATypes.Add(Trim(RemoveEndChars(RecordStr, [' ', toSemiColon, toEquals, toRightParens])));

    Recording := False;
  end;

var
  Haakjes: Integer;
  NewParam: Boolean;
begin
  { Example:

    (P1: T1);
    (P1, P2: T1);
    [Year, Month, Day: Word]  // property
    ;

    PRE : Token = toLeftParens or Token = toSemiColon (no param list)
          AStrings might be nil
    POST: Token is first symbol after ')'
  }
  Haakjes := 0;
  NewParam := True;
  try
    while True do
    begin
      case Token of
        toSemiColon:
          begin
            EndParamTypeRecording;
            if Haakjes = 0 then
              Exit;

            NewParam := True;
          end;
        toEquals:
          { Do not add default values to the type }
          EndParamTypeRecording;
        toLeftParens, toLeftBracket:
          if CLeftHaak[HaakType] = Token then
            Inc(Haakjes);
        toRightParens, toRightBracket:
          if CRightHaak[HaakType] = Token then
          begin
            Dec(Haakjes);
            if Haakjes = 0 then
            begin
              EndParamTypeRecording;

              DoNextToken;
              Exit;
            end;
          end;
        toEof:
          Exit;
        toSymbol:
          if (Haakjes > 0) and NewParam then
          begin
            if TokenSymbolInC(['const', 'var', 'out']) >= 0 then
            begin
              DoNextToken;
              Continue;
            end;

            { (var X) is ook mogelijk }
            if Assigned(AParams) then
              AParams.Add(TokenString);
            DoNextToken;
            {SkipComments;}
            if Token <> toComma then
            begin
              NewParam := False;
              { Let op deze !! }
              Continue;
            end;
          end
          else
          begin
            if not Recording then
              Recording := True;
          end;
      end;
      DoNextToken;
    end;
  finally
    Recording := False;
  end;
end;

procedure TDelphiParser.ReadProcedure;
var
  ProcedureItem: TProcedureItem;
  Directives: TDirectives;
begin
  { Example:

    procedure P1(P1: T1; P2: T2);
    procedure P1;
    procedure T20(Unicode: PWideChar; var S: WideString; Len: Integer); cdecl; export;
    procedure PaintHook(p: QPainterH; R: PRect) cdecl;

    PRE : Token = 'procedure'
    POST: Token = ;
  }
  DoNextToken;
  CheckToken(toSymbol);

  ProcedureItem := TProcedureItem.Create(TokenString);
  FPasItems.Add(ProcedureItem);

  DoNextToken;

  { Token is ( or ; or some directive }
  if Token = toLeftParens then
    ReadParamList(ProcedureItem.Params, ProcedureItem.ParamTypes);

  { Token = ; or some directive }
  Directives := [];
  ReadDirectives(True, Directives);
  ProcedureItem.Directives := Directives;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadProcedureHeader(const DoAdd: Boolean; out IsOnlyHeader: Boolean): TAbstractItem;
var
  MethodType: TMethodType;
  FirstToken: string;
  Directives: TDirectives;
begin
  if TokenSymbolIs('constructor') then
    MethodType := mtConstructor
  else
    if TokenSymbolIs('destructor') then
    MethodType := mtDestructor
  else
    MethodType := mtNormal;

  DoNextToken;
  CheckToken(toSymbol);
  StartCapitalization;

  FirstToken := TokenString;
  DoNextToken;
  if Token = toDot then
  begin
    DoNextToken;
    CheckToken(toSymbol);

    if DoAdd then
    begin
      Result := TMethodProcItem.Create(TokenString);
      FPasItems.Add(Result);

      TMethodProcItem(Result).MethodType := MethodType;
      TMethodProcItem(Result).OwnerClassAsString := FirstToken;
    end
    else
      Result := nil;

    DoNextToken;
  end
  else
  begin
    if DoAdd then
    begin
      Result := TProcedureItem.Create(FirstToken);
      FPasItems.Add(Result);
    end
    else
      Result := nil;
  end;

  { Token is toLeftParens or  toSemiColon or some directive }
  if Token = toLeftParens then
    SkipParamList;

  Directives := [];
  ReadDirectives(True, Directives);
  IsOnlyHeader := [diExternal, diForward] * Directives <> [];

  StopCapitalization;
  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadProcedureImpl(const DoAdd: Boolean): TAbstractItem;
var
  OnlyHeader: Boolean;
begin
  Assert(FHoldRecapitalize, 'cap = true');

  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  BeginBlock;
  Result := ReadProcedureHeader(DoAdd, OnlyHeader);
  EndBlock(True);

  if not OnlyHeader then
  begin
    BeginBlock;
    ReadFunctionProcedureBody;
    EndBlock(False);
  end;

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

function TDelphiParser.ReadProcedureType: TAbstractItem;
var
  Directives: TDirectives;
begin
  { Example:

    TStrProc = procedure(const S: string);
    TNotifyEvent = procedure(Sender: TObject) of object;

    PRE : Token = 'procedure'
    POST: Token = ; or =
  }

  Result := TProcedureTypeItem.Create('');
  FPasItems.Add(Result);

  DoNextToken;
  { Token is toLeftParens or toSemiColon or 'of object' or ?? }
  if Token = toLeftParens then
    with TProcedureTypeItem(Result) do
      ReadParamList(Params, ParamTypes);

  Directives := [];
  ReadDirectives(True, Directives);
  TProcedureTypeItem(Result).Directives := Directives;

  if not (Token in [toSemiColon, toEquals]) then
    Error('; or = expected');
end;

procedure TDelphiParser.ReadPropertySpecifiers(
  const MaySkipSemiColon: Boolean; var Specifiers: TPropertySpecifiers);
var
  Specifier: Integer;
begin
  {
    Pre  : Token = ;  or some specifier
    Post : Token = ;
  }

  if ((Token <> toSemiColon) or not MaySkipSemiColon) and
    ((Token <> toSymbol) or (TokenSymbolInC(CPropertySpecifiers) < 0)) then
    Exit;

  while True do
  begin
    if Token = toSemiColon then
    begin
      SaveRollBackPosition;
      DoNextToken;
      if (Token <> toSymbol) or (TokenSymbolInC(CPropertySpecifiers) < 0) then
      begin
        RollBackToSavedPosition;
        Exit;
      end;
      ForgetRollBackPosition;
    end;

    while Token = toSymbol do
    begin
      Specifier := TokenSymbolInC(CPropertySpecifiers);
      if Specifier < 0 then
        Error('Specifier expected')
      else
        Include(Specifiers, TPropertySpecifier(Specifier));
      DoNextToken;
    end;

    CheckToken(toSemiColon);
  end;
end;

function TDelphiParser.ReadRecord: TAbstractItem;
begin
  { Example:

    R1 = record F1, F2: T1 end;          <-- Let op ","
    R1 = record F1: T1; F2: T2; end;
    R1 = record case tag: ordinalType of
           constantList1: (variant1);
           ...
           constantListn: (variantn);
         end;

         variant1 = fieldList1: type1;
                    ...
                    fieldListn: typen;

    R1 = record
      F1: T1; F2: T2;
      F3: record case T3 of
        constantList1: (F1': T1');
        ...
        constantListn: (Fn': Tn');
      end;
    end;

    PRE : Token is 'record'
    POST: Token is token after 'end'
  }
  Result := TRecordItem.Create('');
  FPasItems.Add(Result);

  DoNextToken;

  while True do
  begin
    case Token of
      toSymbol:
        if TokenSymbolIs('end') then
          Break
        else
          if TokenSymbolIs('case') then
        begin
          SkipUntilSymbol('of');
          DoNextToken;
          SkipUntilToken(toLeftParens);
        end
        else
        begin
          { Lees (identifier:type) paar in }
          TRecordItem(Result).Items.Add(TokenString);
          DoNextToken;
          if Token <> toComma then
          begin
            CheckToken(toColon);
            DoNextToken;

            SkipType(False, True);
            { Note : Token = ; or end }
            Continue;
          end;
        end;
      toRightParens: { empty list or closing }
        begin
          DoNextToken;
          while Token = toRightParens do
            DoNextToken;
          if Token = toSemiColon then
            DoNextToken;
          if TokenSymbolIs('end') then
            Break
          else
          begin
            SkipUntilToken(toColon);
            DoNextToken;
            CheckToken(toLeftParens);
          end;
        end;
      toEof:
        Exit;
    end;
    DoNextToken;
  end;

  CheckTokenSymbol('end');
  DoNextToken;
end;

function TDelphiParser.ReadResourceString: TAbstractItem;
var
  ResourceStringItem: TResourceStringItem;
begin
  { RS1 = V1;

    PRE : Token = Identifier (RS1)
    POST: Token = ;
  }

  StartCapitalization;

  ResourceStringItem := TResourceStringItem.Create(TokenString);
  FPasItems.Add(ResourceStringItem);

  DoNextToken;
  CheckToken(toEquals);

  BeginRecording;

  SkipUntilToken(toSemiColon);

  EndRecording;
  StopCapitalization;

  ResourceStringItem.Value := RecordStr;

  CheckToken(toSemiColon);

  Result := ResourceStringItem;
end;

function TDelphiParser.ReadResourceStringImpl(const DoAdd: Boolean): TAbstractItem;
begin
  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  Result := SkipResourceString(DoAdd);

  CheckToken(toSemiColon);

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

function TDelphiParser.ReadSimpleType: TAbstractItem;
begin
  StartCapitalization;
  BeginRecording;

  Result := SkipSimpleType(True);

  EndRecording;
  StopCapitalization;

  if Result is TTypeItem then
    TTypeItem(Result).Value := RecordStr;
end;

function TDelphiParser.ReadThreadVar: TAbstractItem;
begin
  Result := ReadVar;
  if Result is TVarItem then
    TVarItem(Result).IsThreadVar := True;
end;

function TDelphiParser.ReadThreadVarImpl(
  const DoAdd: Boolean): TAbstractItem;
begin
  Result := ReadVarImpl(DoAdd);
  if Result is TVarItem then
    TVarItem(Result).IsThreadVar := True;
end;

function TDelphiParser.ReadType(const DoReadDirectives: Boolean): TAbstractItem;
begin
  {
    PRE:
    POST: Type including directives read
  }

  if Token = toLeftParens then
    Result := ReadEnumerator
  else
    case TokenSymbolInC(['record', 'class', 'procedure', 'function', 'interface',
      'dispinterface', 'packed']) of

      0: { record }
        Result := ReadRecord;
      1: { class}
        Result := ReadClass;
      2: { procedure }
        Result := ReadProcedureType;
      3: { function }
        Result := ReadFunctionType;
      4: { interface }
        Result := ReadInterfaceType;
      5: { dispinterface }
        Result := ReadDispInterface;
      6: {packed }
        begin
          DoNextToken;
          CheckToken(toSymbol);
          if TokenSymbolIs('record') then
            Result := ReadRecord
          else
            if TokenSymbolIs('array') then
            Result := ReadSimpleType
          else
          begin
            Result := nil;
            Error('record or array expected');
          end;
        end
    else
      Result := ReadSimpleType;
    end;

  if DoReadDirectives then
    SkipDirectives(False);
end;

function TDelphiParser.ReadTypeDef: TAbstractItem;
var
  TypeName: string;
begin
  { Example:

    T1 = record F1: T1; F2: T2; end;
    T1 = class() [classdef] end;
    T1 = sometype;

    PRE : Token = identifier [T1]
    POST: Token = ;
  }
  TypeName := TokenString;
  DoNextToken;
  CheckToken(toEquals);
  DoNextToken;

  Result := ReadType(False);
  if Assigned(Result) then
    Result.SimpleName := TypeName;

  SkipDirectives(False); { 'deprecated', 'platform' }

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadTypeDefImpl(const DoAdd: Boolean): TAbstractItem;
begin
  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  Result := SkipTypeDef(DoAdd);

  CheckToken(toSemiColon);

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

procedure TDelphiParser.ReadUnitBlock(out AModuleType: TModuleType);
begin
  CheckToken(toSymbol);
  if TokenSymbolIs('library') then
    AModuleType := mtLibrary
  else
    if TokenSymbolIs('unit') then
    AModuleType := mtUnit
  else
    Error('''library'' or ''unit'' expected');
  DoNextToken;
  SkipUntilToken(toSemiColon);
  DoNextToken;
end;

function TDelphiParser.ReadUsesBlock(const DoAdd: Boolean): TAbstractItem;
begin
  Result := nil;

  if Token in [toComment, toSameLineComment, toCompilerDirective] then
  begin
    SaveRollBackPosition;
    DoNextToken;
    if not TokenSymbolIsC('uses') then
    begin
      RollBackToSavedPosition;
      Exit;
    end;

    ForgetRollBackPosition;
  end;

  if not TokenSymbolIsC('uses') then
    Exit;

  if DoAdd then
  begin
    MakeCopyOfDEFList;
    Result := TUsesItem.Create('');
    FPasItems.Add(Result);
    BeginLowRecording;
  end;

  BeginBlock;
  StartCapitalization;

  SkipUntilToken(toSemiColon);

  StopCapitalization;
  EndBlock(True);

  { Token = ; or ENDIF directive }

  if DoAdd then
  begin
    EndLowRecording;

    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

function TDelphiParser.ReadVar: TAbstractItem;
begin
  { Example:

    V1: T1;
    IdSslCtxFree : procedure(arg0: PSSL_CTX) cdecl = nil;
    QBDRed, QBDBlue, QBDGreen: byte;

    PRE:  Token = var. identifier (V1 in example)
    POST: Token = ;
  }

  Result := TVarItem.Create(TokenString);
  FPasItems.Add(Result);

  DoNextToken;
  while Token = toComma do
  begin
    DoNextToken;
    CheckToken(toSymbol);
    FPasItems.Add(TVarItem.Create(TokenString));
    DoNextToken;
  end;

  CheckToken(toColon);
  DoNextToken;

  SkipType(False, True);

  if Token = toEquals then
  begin
    DoNextToken;
    SkipConstValue;
  end;

  CheckToken(toSemiColon);
end;

function TDelphiParser.ReadVarImpl(const DoAdd: Boolean): TAbstractItem;
begin
  if DoAdd then
  begin
    BeginLowRecording;
    MakeCopyOfDEFList;
  end;

  Result := SkipVar(DoAdd);

  CheckToken(toSemiColon);

  if DoAdd then
  begin
    EndLowRecording;
    if Assigned(Result) then
    begin
      Result.ImplementationStr := LowRecordingStr;
      Result.BeginDEFList := FCopiedDEFList;
      Result.EndDEFList := FDEFList;
    end;
  end;

  DoNextToken(False);
end;

procedure TDelphiParser.RecapitalizeToken;
var
  Index: Integer;
begin
  if (Token <> toSymbol) or not Assigned(FCapitalization) then
    Exit;

  Index := FCapitalization.IndexOf(TokenString);
  if Index >= 0 then
    RecapitalizeTokenWith(FCapitalization[Index]);
end;

procedure TDelphiParser.RecapitalizeTokenWith(const S: string);
var
  L: Integer;
begin
  L := FSourcePtr - FTokenPtr;
  if Length(S) = L then
    Move(PChar(S)^, FTokenPtr^, L);
end;

procedure TDelphiParser.RollBackToSavedPosition;
begin
  FDEFList.Assign(FSavedDEFList);
  inherited RollBackToSavedPosition;
end;

procedure TDelphiParser.SaveRollBackPosition;
begin
  FSavedDEFList.Assign(FDEFList);
  inherited SaveRollBackPosition;
end;

procedure TDelphiParser.SetAcceptCompilerDirectives(const Value: TStrings);
begin
  FAcceptCompilerDirectives.Assign(Value);
end;

function TDelphiParser.SkipClass(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    Tx = class(Tx1) .. end;
    Tx = class .. end;
    Tx = class;               -> Deze niet toevoegen, moet nog compleet
                                 gedefinieerd worden
    Tx = class(Tx1);          -> Hack class
    Tx = class of Tx1;        -> Meta class

    PRE : Token = 'class'
    POST: Token = ;
  }

  DoNextToken;
  if Token = toSemiColon then
  begin
    { Class is nog niet compleet gedefinieerd; niet toevoegen }
    if DoAdd then
    begin
      Result := TTypeItem.Create('');
      FPasItems.Add(Result);
    end
    else
      Result := nil;
    Exit;
  end;

  if Token = toLeftParens then
  begin
    DoNextToken;
    CheckToken(toSymbol);
    SkipUntilToken(toRightParens);
    DoNextToken;
  end;

  { Now add }
  if DoAdd then
  begin
    if TokenSymbolIs('of') then
      Result := TMetaClassItem.Create('')
    else
      Result := TClassItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  if TokenSymbolIs('of') then
  begin
    DoNextToken;
    CheckToken(toSymbol);
    SkipUntilToken(toSemiColon);
  end
  else
    if Token <> toSemiColon then
    SkipClassMethods
end;

procedure TDelphiParser.SkipClassMethods;
begin
  { vb

    public property P1; end;

    PRE : Token = token after 'class'
    POST: Token = ; (= first token after 'end')
  }
  while True do
  begin
    CheckNotToken(toEof);

    if Token = toSymbol then
      case TokenSymbolInC(['end', 'private', 'protected', 'public', 'published',
        'property', 'procedure', 'constructor', 'destructor', 'function', 'class']) of
        0: { end }
          begin
            DoNextToken;
            { token = directive or ; }
            Exit;
          end;
        1, 2, 3, 4: { private, protected, public, published }
          DoNextToken;
        5: { property }
          SkipClass_Property;
        6, 7, 8: { procedure, constructor, destructor }
          SkipClass_Procedure;
        9: {function }
          SkipClass_Function;
        10: {class }
          SkipClass_ClassMethod;
      else { field }
        SkipClass_Field;
      end
    else { not a symbol, probably ; }
      DoNextToken;
  end;
end;

procedure TDelphiParser.SkipClass_ClassMethod;
begin
  DoNextToken;
  CheckToken(toSymbol);

  case CheckTokenSymbolInC(['function', 'procedure']) of
    0: SkipClass_Function;
    1: SkipClass_Procedure;
  end;
end;

procedure TDelphiParser.SkipClass_Field;
begin
  StartCapitalization;
  SkipUntilToken(toColon);
  DoNextToken;
  SkipType(False, True);
  //SkipUntilToken(toSemiColon);
  StopCapitalization;
end;

procedure TDelphiParser.SkipClass_Function;
begin
  { Example:

    function F1(P1: T1; P2: T2);
    function IMalloc.Alloc = Allocate;

    PRE : Token = 'function'
    POST: Token = ;
  }

  DoNextToken;

  StartCapitalization;
  CheckToken(toSymbol);
  DoNextToken;
  if Token = '.' then
  begin
    { Method resolution clause }
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toEquals);
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
  end
  else
  begin
    if Token = toLeftParens then
      SkipParamList;

    CheckToken(toColon);
    DoNextToken;
    SkipType(False, False);

    SkipDirectives(True);
  end;

  StopCapitalization;
  CheckToken(toSemiColon);
end;

procedure TDelphiParser.SkipClass_Procedure;
begin
  { Example:

    procedure P1(P1: T1; P2: T2);

    PRE : Token = 'procedure' or 'constructor' or 'destructor'
    POST: Token = ;
  }

  DoNextToken;

  StartCapitalization;
  CheckToken(toSymbol);
  DoNextToken;
  if Token = toDot then
  begin
    { Method resolution clause }
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
    CheckToken(toEquals);
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
  end
  else
  begin
    if Token = toLeftParens then
      SkipParamList;

    SkipDirectives(True);
  end;

  StopCapitalization;
  CheckToken(toSemiColon);
end;

procedure TDelphiParser.SkipClass_Property;
begin
  { Bv:

    propery P1: X1 read Get1 write Get2
    property P1;
    property P1 default X;
    property Bold[Year, Month, Day: Word]: Boolean read IsBold write SetBold;

    Pre : Token = 'property'
    POST: Token = ;
  }

  DoNextToken;

  StartCapitalization;
  DoNextToken;
  if (Token = toSemiColon) or
    ((Token = toSymbol) and (TokenSymbolInC(['default', 'stored']) >= 0)) then
  begin
    SkipUntilToken(toSemiColon);
  end
  else
  begin
    if Token = toLeftBracket then
      SkipParamList(htBracket);
    SkipUntilToken(toSymbol);

    while not (Token in [toEof, toSemiColon]) and (TokenSymbolInC(CPropertySpecifiers) < 0) do
      DoNextToken;

    while not (Token in [toSemiColon, toEof]) do
    begin
      { TODO : Anders }
      TokenSymbolInC(CPropertySpecifiers);
      DoNextToken;
    end;
  end;

  CheckToken(toSemiColon);

  SkipPropertySpecifiers(True);

  StopCapitalization;
  CheckToken(toSemiColon);
end;

function TDelphiParser.SkipConst(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    C1 = V1;
    C1: type = V1;
    C1: array [xx] of TSpecialFolderInfo = ((), (), ());
    C1: function (AOwner: TComponent; ActionClass: TBasicActionClass): TBasicAction = nil;

    PRE : Token = identifier [C1]
    POST: Token = ;
  }
  StartCapitalization;

  if DoAdd then
  begin
    Result := TConstItem.Create(TokenString);
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;
  case Token of
    toEquals:
      begin
        DoNextToken;
        SkipConstValue;
        StopCapitalization;
        SkipDirectives(False);
      end;
    toColon:
      begin
        DoNextToken;
        SkipType(False, True);
        CheckToken(toEquals);
        DoNextToken;
        SkipConstValue;
      end;
  else
    Error('= or : expected');
  end;

  CheckToken(toSemiColon);
end;

procedure TDelphiParser.SkipConstantExpression(const ExpectDotDot: Boolean);
begin
  { Examples

    const
      DOW_WEEK : TTFDaysOfWeek = [dowSunday..dowSaturday];
                                 _________________________

      StIdSymbols = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
                    ____________________________________

      StdWordDelims = [#0..' ', ','] + Brackets;
                      __________________________

  }

  { POST : Token = toSemiColon ';' or 'end' }

  while True do
  begin
    if (Token = toSymbol) and (TokenSymbolInC(CDirectives) > 0) then
      Exit;

    CheckNotToken(toEof);

    { TODO : Nodig? }
    if TokenSymbolInC(CAllowableSymbolsInTypeDef) >= 0 then
      SkipUntilTokenInHaak(toLeftParens, toRightParens)
    else
      if TokenSymbolIs('end') then
      Exit
    else
      case Token of
        toRightParens:
          Exit;
        toLeftParens:
          SkipUntilTokenInHaak(toLeftParens, toRightParens);
        toLeftBracket:
          SkipUntilTokenInHaak(toLeftBracket, toRightBracket);
        toDotDot:
          begin
            DoNextToken;
            CheckNotToken(toDot);
            SkipConstantExpression(False);
            Exit;
          end;
        toDot:
          begin
            DoNextToken;
            CheckNotToken(toDot);
            Continue;
          end;
        toSemiColon, '=':
          Exit;
      end;
    DoNextToken;
  end;
end;

procedure TDelphiParser.SkipConstValue;
begin
  SkipConstantExpression(False);
end;

procedure TDelphiParser.SkipDirectives(const MaySkipSemiColon: Boolean);
var
  Directive: Integer;
begin
  {
    Pre  : Token = ;  or some directive
    Post : Token = ; or =
  }

  StopCapitalization;
  if Token = toSemiColon then
  begin
    if not MaySkipSemiColon then
      Exit;
    Directive := 0;
  end
  else
  begin
    Directive := TokenSymbolInC(CDirectives);
    if Directive < 0 then
      Exit;

    { Directive is valid }
  end;

  { Token = ; or Directive is valid }
  while True do
  begin
    CheckNotToken(toEof);

    if Token = toSemiColon then
    begin
      SaveRollBackPosition;
      DoNextToken;

      Directive := TokenSymbolInC(CDirectives);
      if Directive < 0 then
      begin
        RollBackToSavedPosition;
        Exit;
      end;
      { Directive is valid }
      ForgetRollBackPosition;
    end;

    repeat
      if Directive < 0 then
        Error('Directive expected');

      case TDirective(Directive) of
        diMessage, diExternal:
          SkipUntilToken(toSemiColon);
      else
        DoNextToken;
      end;
      Directive := TokenSymbolInC(CDirectives);
    until Token <> toSymbol;

    if Token = toEquals then
      Exit;

    CheckToken(toSemiColon);
  end;
end;

function TDelphiParser.SkipDispInterface(
  const DoAdd: Boolean): TAbstractItem;
begin
  { TODO: Implement }
  SkipUntilSymbol('end');
  SkipUntilToken(toSemiColon);
  Result := nil;
end;

function TDelphiParser.SkipEnumerator(const DoAdd: Boolean): TAbstractItem;
var
  NewValue: Boolean;
  Haakjes: Integer;
begin
  { Example:

    (E1, E2, E3);
    ();
    (Small = 5, Medium = 10, Large = Small + Medium);

    PRE : Token = (
    POST: Token = ;
  }

  if DoAdd then
  begin
    Result := TEnumItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  { NewValue hebben we nodig om bv in '(Small = 5, ..' 5 niet mee te nemen }
  NewValue := True;
  Haakjes := 0;
  while True do
  begin
    CheckNotToken(toEof);
    case Token of
      toSemiColon:
        if Haakjes = 0 then
          Exit;
      toLeftParens:
        Inc(Haakjes);
      toRightParens:
        begin
          Dec(Haakjes);
          if Haakjes = 0 then
          begin
            DoNextToken;
            Exit;
          end;
        end;
      toSymbol:
        if NewValue then
        begin
          NewValue := False;
        end;
      toComma:
        NewValue := True;
    end;
    DoNextToken;
  end;
end;

procedure TDelphiParser.SkipFunction;
begin
  { Example:

    function F1(P1: T1; P2: T2);

    PRE : Token = 'function'
    POST: Token = ;
  }
  CheckTokenSymbol('function');

  StartCapitalization;

  DoNextToken;
  CheckToken(toSymbol);

  DoNextToken;

  { Token = toLeftParens or toColon }
  if Token = toLeftParens then
    SkipParamList;

  CheckToken(toColon);
  DoNextToken;

  SkipType(False, False);

  SkipDirectives(True);

  StopCapitalization;
  CheckToken(toSemiColon);
end;

function TDelphiParser.SkipFunctionType(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    F1 = Function();
    TODO : Deze gaat fout : dwz ReadType leest ook directives in
    function(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer cdecl = nil;
    T17 : function(arg0: PSSL_CTX; str: PChar):Integer cdecl = nil;
          __________________________________________________

    PRE : Token = 'function'
    POST: Token = ;
  }

  if DoAdd then
  begin
    Result := TFunctionTypeItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;

  { Token is toLeftParens or toSemiColon or 'of object' or ?? }
  if Token = toLeftParens then
    SkipParamList;

  CheckToken(toColon);
  DoNextToken;
  SkipType(False, False);
  SkipDirectives(True);

  if not (Token in [toSemiColon, toEquals]) then
    Error('; or = expected');
end;

procedure TDelphiParser.SkipInterfaceBlock;
var
  Position: TPosition;
begin
  { POST: Token = 'implementation' }

  Position := inNull;
  StopCapitalization;

  while Token <> toEof do
    case Token of
      toSymbol:
        case TokenSymbolInC([
          {0}'implementation',
          {1}'procedure',
          {2}'function',
          {3}'resourcestring',
          {4}'const',
          {5}'type',
          {6}'var',
          {7}'threadvar']) of

          0: Exit;
          1: ReadProcedure;
          2: ReadFunction;
          3:
            begin
              Position := inResourceString;
              DoNextToken;
            end;
          4:
            begin
              Position := inConst;
              DoNextToken;
            end;
          5:
            begin
              Position := inType;
              DoNextToken;
            end;
          6, 7:
            begin
              Position := inVar;
              DoNextToken;
            end
        else
          case Position of
            inConst: SkipConst(False);
            inType: SkipTypeDef(False);
            inVar: SkipVar(False);
            //inThreadVar: SkipThreadVar(False); { not possible here }
            inResourceString: SkipResourceString(False);
          else
            Error('Not in Type, Const, Var');
          end;
        end;
    else
      DoNextToken;
    end;

  { TODO : Huh ?? }
  while (Token <> toEof) and
    ((Token <> toSymbol) or not SameText(TokenString, 'implementation')) do
    DoNextToken;
end;

procedure TDelphiParser.SkipInterfaceMethods;
begin
  while True do
  begin
    CheckNotToken(toEof);

    case TokenSymbolInC(['end', 'property', 'procedure', 'function']) of
      0: { end }
        begin
          DoNextToken;
          CheckToken(toSemiColon);
          Exit;
        end;
      1: { property }
        SkipClass_Property;
      2: { procedure }
        SkipClass_Procedure;
      3: { function }
        SkipClass_Function;
    else
      DoNextToken;
    end;
  end;
end;

function TDelphiParser.SkipInterfaceType(const DoAdd: Boolean): TAbstractItem;
begin
  (* vb

    IJvDataConsumer = interface;
    IJvDataConsumer = interface
    ['{B2F18D03-F615-4AA2-A51A-74D330C05C0E}']

    PRE : Token = 'interface'
    POST: Token = ;
  *)
  DoNextToken;
  if Token = toSemiColon then
  begin
    { Class is nog niet compleet gedefinieerd; niet toevoegen }
    Result := nil;
    Exit;
  end;

  if Token = toLeftBracket then
    SkipUntilToken(toRightBracket);

  { Nu pas toevoegen }
  if DoAdd then
  begin
    Result := TInterfaceItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  SkipInterfaceMethods;
  CheckToken(toSemiColon);
end;

procedure TDelphiParser.SkipParamList(const HaakType: THaakType);
const
  CLeftHaak: array[THaakType] of Char = (toLeftParens, toLeftBracket);
  CRightHaak: array[THaakType] of Char = (toRightParens, toRightBracket);
var
  Haakjes: Integer;
  NewParam: Boolean;
begin
  { Example:

    (P1: T1);
    (P1, P2: T1);
    [Year, Month, Day: Word]  // property
    ;

    PRE : Token = toLeftParens or Token = toSemiColon (no param list)
          AStrings might be nil
    POST: Token is first symbol after ')'
  }
  Haakjes := 0;
  NewParam := True;
  while True do
  begin
    case Token of
      toSemiColon:
        begin
          if Haakjes = 0 then
            Exit;

          NewParam := True;
        end;
      toEquals:
        { Do not add default values to the type }
        ;
      toLeftParens, toLeftBracket:
        if CLeftHaak[HaakType] = Token then
          Inc(Haakjes);
      toRightParens, toRightBracket:
        if CRightHaak[HaakType] = Token then
        begin
          Dec(Haakjes);
          if Haakjes = 0 then
          begin
            DoNextToken;
            Exit;
          end;
        end;
      toEof:
        Exit;
      toSymbol:
        if (Haakjes > 0) and NewParam then
        begin
          if TokenSymbolInC(['const', 'var', 'out']) >= 0 then
          begin
            DoNextToken;
            Continue;
          end;

          DoNextToken;
          if Token <> toComma then
          begin
            NewParam := False;
            { Let op deze !! }
            Continue;
          end;
        end;
    end;
    DoNextToken;
  end;
end;

procedure TDelphiParser.SkipProcedure;
begin
  { Example:

    procedure P1(P1: T1; P2: T2);
    procedure P1;
    procedure T20(Unicode: PWideChar; var S: WideString; Len: Integer); cdecl; export;
    procedure PaintHook(p: QPainterH; R: PRect) cdecl;

    PRE : Token = 'procedure' or 'constructor' or 'destructor'
    POST: Token = ;
  }
  DoNextToken;

  StartCapitalization;
  CheckToken(toSymbol);

  DoNextToken;

  { Token is ( or ; or some directive }
  if Token = toLeftParens then
    SkipParamList;

  { Token = ; or some directive }
  SkipDirectives(True);

  StopCapitalization;
  CheckToken(toSemiColon);
end;

function TDelphiParser.SkipProcedureType(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    TStrProc = procedure(const S: string);
    TNotifyEvent = procedure(Sender: TObject) of object;

    PRE : Token = 'procedure'
    POST: Token = ; or =
  }

  if DoAdd then
  begin
    Result := TProcedureTypeItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;
  { Token is toLeftParens or toSemiColon or 'of object' or ?? }
  if Token = toLeftParens then
    SkipParamList;
  SkipDirectives(True);

  if not (Token in [toSemiColon, toEquals]) then
    Error('; or = expected');
end;

procedure TDelphiParser.SkipPropertySpecifiers(const MaySkipSemiColon: Boolean);
begin
  {
    Pre  : Token = ;  or some specifier
    Post : Token = ;
  }

  StopCapitalization;

  if ((Token <> toSemiColon) or not MaySkipSemiColon) and
    ((Token <> toSymbol) or (TokenSymbolInC(CPropertySpecifiers) < 0)) then
    Exit;

  while True do
  begin
    if Token = toSemiColon then
    begin
      SaveRollBackPosition;
      DoNextToken;
      if (Token <> toSymbol) or (TokenSymbolInC(CPropertySpecifiers) < 0) then
      begin
        RollBackToSavedPosition;
        Exit;
      end;
      ForgetRollBackPosition;
    end;

    while Token = toSymbol do
    begin
      if TokenSymbolInC(CPropertySpecifiers) < 0 then
        Error('Specifier expected');
      DoNextToken;
    end;

    CheckToken(toSemiColon);
  end;
end;

function TDelphiParser.SkipRecord(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    R1 = record F1, F2: T1 end;          <-- Let op ","
    R1 = record F1: T1; F2: T2; end;
    R1 = record case tag: ordinalType of
           constantList1: (variant1);
           ...
           constantListn: (variantn);
         end;

         variant1 = fieldList1: type1;
                    ...
                    fieldListn: typen;

    R1 = record
      F1: T1; F2: T2;
      F3: record case T3 of
        constantList1: (F1': T1');
        ...
        constantListn: (Fn': Tn');
      end;
    end;

    PRE : Token is 'record'
    POST: Token is token after 'end'
  }
  if DoAdd then
  begin
    Result := TRecordItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;

  while True do
  begin
    case Token of
      toSymbol:
        if TokenSymbolIs('end') then
          Break
        else
          if TokenSymbolIs('case') then
        begin
          SkipUntilSymbol('of');
          DoNextToken;
          SkipUntilToken(toLeftParens);
        end
        else
        begin
          { Lees (identifier:type) paar in }
          DoNextToken;
          if Token <> toComma then
          begin
            CheckToken(toColon);
            DoNextToken;

            SkipType(False, True);
            { Note : Token = ; or end }
            Continue;
          end;
        end;
      toRightParens: { empty list or closing }
        begin
          DoNextToken;
          while Token = toRightParens do
            DoNextToken;
          if Token = toSemiColon then
            DoNextToken;
          if TokenSymbolIs('end') then
            Break
          else
          begin
            SkipUntilToken(toColon);
            DoNextToken;
            CheckToken(toLeftParens);
          end;
        end;
      toEof:
        Exit;
    end;
    DoNextToken;
  end;

  CheckTokenSymbol('end');
  DoNextToken;
end;

function TDelphiParser.SkipResourceString(const DoAdd: Boolean): TAbstractItem;
begin
  { RS1 = V1;

    POST: Token = ;
  }

  if DoAdd then
  begin
    Result := TResourceStringItem.Create(TokenString);
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;
  CheckToken(toEquals);
  SkipUntilToken(toSemiColon);
end;

function TDelphiParser.SkipSimpleType(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    T1 = array [..] of record F1: T1; F2: T2; end;
    T1 = xx;

    PRE:  Token = xx/array
    POST: Token = ;
  }

  if DoAdd then
  begin
    Result := TTypeItem.Create('');
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  if TokenSymbolIs('array') then
  begin
    SkipUntilSymbol('of');
    DoNextToken;
    SkipType(False, False);
  end
  else
    if TokenSymbolIs('set') then
  begin
    { same as array }
    SkipUntilSymbol('of');
    DoNextToken;
    SkipType(False, False);
  end
  else
    if Token = '^' then
  begin
    DoNextToken;
    CheckToken(toSymbol);
    DoNextToken;
  end
  else
    if TokenSymbolIs('string') then
  begin
    DoNextToken;
    if Token = toLeftBracket then
    begin
      SkipUntilTokenInHaak(toLeftBracket, toRightBracket);
      CheckToken(toRightBracket);
      DoNextToken;
    end;
  end
  else
    if TokenSymbolIs('file') then
  begin
    { same as array }
    DoNextToken;
    if TokenSymbolIs('of') then
    begin
      DoNextToken;
      SkipType(False, False);
    end;
  end
  else
    if TokenSymbolInC(CAllowableSymbolsInTypeDef) >= 0 then
    SkipConstantExpression(True)
  else
  begin
    { We can't skip until ; }

    if TokenSymbolIs('type') then
      DoNextToken;

    DoNextToken;
    if Token = toDotDot then
    begin
      { TSomeType = a..b; }
      DoNextToken;

      SkipConstantExpression(False);
    end
    else
      if Token = '.' then
    begin
      DoNextToken;
      CheckToken(toSymbol);
      if TokenSymbolInC(CDirectives) > 0 then
        Exit;

      DoNextToken;
      while Token = '.' do
      begin
        DoNextToken;
        CheckToken(toSymbol);
        if TokenSymbolIn(CDirectives) > 0 then
          Exit;

        DoNextToken;
      end;
    end
    else
      if not (Token in [toRightParens, toSemiColon, toEquals]) and not TokenSymbolIs('end') then
      SkipConstantExpression(True);
  end;

  CheckNotToken(toEof);
end;

function TDelphiParser.SkipType(const DoAdd,
  DoReadDirectives: Boolean): TAbstractItem;
begin
  {
    PRE:
    POST: Type including directives read
  }

  if Token = toLeftParens then
    Result := SkipEnumerator(DoAdd)
  else
    case TokenSymbolIn(['record', 'class', 'procedure', 'function', 'interface',
      'dispinterface', 'packed']) of

      0: { record }
        Result := SkipRecord(DoAdd);
      1: { class}
        Result := SkipClass(DoAdd);
      2: { procedure }
        Result := SkipProcedureType(DoAdd);
      3: { function }
        Result := SkipFunctionType(DoAdd);
      4: { interface }
        Result := SkipInterfaceType(DoAdd);
      5: { dispinterface }
        Result := SkipDispInterface(DoAdd);
      6: {packed }
        begin
          Result := nil; //Satisfy compiler

          DoNextToken;
          CheckToken(toSymbol);
          case CheckTokenSymbolInC(['record', 'array']) of
            0: Result := SkipRecord(DoAdd);
            1: Result := SkipSimpleType(DoAdd);
          end;
        end
    else
      Result := SkipSimpleType(DoAdd);
    end;

  if DoReadDirectives then
    SkipDirectives(False);
end;

function TDelphiParser.SkipTypeDef(const DoAdd: Boolean): TAbstractItem;
var
  TypeName: string;
begin
  { Example:

    T1 = record F1: T1; F2: T2; end;
    T1 = class() [classdef] end;
    T1 = sometype;

    PRE : Token = identifier [T1]
    POST: Token = ;
  }
  TypeName := TokenString;
  DoNextToken;
  CheckToken(toEquals);
  DoNextToken;

  Result := SkipType(DoAdd, False);
  if Assigned(Result) then
    Result.SimpleName := TypeName;

  SkipDirectives(False); { 'deprecated', 'platform' }

  CheckToken(toSemiColon);
end;

procedure TDelphiParser.SkipUntilSymbol(const Symbol: string);
begin
  while (Token <> toEof) and not TokenSymbolIs(Symbol) do
    DoNextToken;
end;

procedure TDelphiParser.SkipUntilToken(T: Char);
begin
  { PRE : -
    POST: Token = T or Token = toEof
  }
  while not (Token in [T, toEof]) do
    DoNextToken;
  CheckToken(T);
end;

procedure TDelphiParser.SkipUntilTokenInHaak(OpenSymbol, CloseSymbol: Char;
  const InitialOpenCount: Integer);
var
  OpenCount: Integer;
begin
  { Post : Token on CloseHaak }

  if OpenSymbol = CloseSymbol then
    Error('Internal error: OpenHaak = CloseHaak');
  OpenCount := InitialOpenCount;
  while Token <> toEof do
  begin
    if Token = OpenSymbol then
      Inc(OpenCount)
    else
      if Token = CloseSymbol then
    begin
      Dec(OpenCount);
      if OpenCount = 0 then
        Exit;
    end;

    DoNextToken;
  end;
end;

procedure TDelphiParser.SkipUntilTokenInHaak(T: Char; const InitHaak: Integer);
var
  Haakjes: Integer;
begin
  { PRE  :
    POST : Token = ;
  }
  Haakjes := InitHaak;
  while True do
  begin
    case Token of
      toLeftParens:
        Inc(Haakjes);
      toRightParens:
        Dec(Haakjes);
      toEof:
        Exit;
    end;
    if Token = T then
      if Haakjes <= 0 then
        Exit;
    DoNextToken;
  end;
end;

procedure TDelphiParser.SkipUsesBlock;
begin
  if Token in [toComment, toSameLineComment, toCompilerDirective] then
  begin
    SaveRollBackPosition;
    DoNextToken;
    if not TokenSymbolIsC('uses') then
    begin
      RollBackToSavedPosition;
      Exit;
    end;

    ForgetRollBackPosition;
  end;

  if not TokenSymbolIsC('uses') then
    Exit;

  BeginBlock;
  StartCapitalization;
  SkipUntilToken(toSemiColon);
  StopCapitalization;
  EndBlock(True);
  DoNextToken(False);
end;

function TDelphiParser.SkipVar(const DoAdd: Boolean): TAbstractItem;
begin
  { Example:

    V1: T1;
    IdSslCtxFree : procedure(arg0: PSSL_CTX) cdecl = nil;
    QBDRed, QBDBlue, QBDGreen: byte;

    PRE:  Token = var. identifier (V1 in example)
    POST: Token = ;
  }

  StartCapitalization;

  if DoAdd then
  begin
    Result := TVarItem.Create(TokenString);
    FPasItems.Add(Result);
  end
  else
    Result := nil;

  DoNextToken;
  while Token = toComma do
  begin
    DoNextToken;
    CheckToken(toSymbol);
    if DoAdd then
      FPasItems.Add(TVarItem.Create(TokenString));
    DoNextToken;
  end;

  CheckToken(toColon);
  DoNextToken;

  { Never add }
  SkipType(False, True);

  if Token = toEquals then
  begin
    DoNextToken;
    SkipConstValue;
  end;

  CheckToken(toSemiColon);
  StopCapitalization;
end;

procedure TDelphiParser.StartCapitalization;
begin
  if not FHoldRecapitalize then
    Exit;
  FHoldRecapitalize := False;
  RecapitalizeToken;
end;

procedure TDelphiParser.StopCapitalization;
begin
  FHoldRecapitalize := True;
end;

procedure TDelphiParser.SwitchDefine;
var
  Current: TDefineType;
begin
  if FDEFList.Count = 0 then
    Error('Unexpected {$ELSE}');

  Current := TDefineType(FDEFList.Objects[FDEFList.Count - 1]);
  if Current = dftIFDEF then
    Current := dftIFNDEF
  else
    Current := dftIFDEF;
  FDEFList.Objects[FDEFList.Count - 1] := TObject(Current);
end;

function TDelphiParser.TokenCompilerDirective: TCompilerDirective;
var
  P: PChar;
  S: string;
begin
  CheckToken(toCompilerDirective);
  P := FTokenPtr + 2;
  while P^ in ['A'..'Z', 'a'..'z', '0'..'9'] do
    Inc(P);

  SetString(S, FTokenPtr + 2, P - FTokenPtr - 2);
  Result := StrToCompilerDirective(S);
  if (Length(S) = 1) and
    (Result in [cdInputOutputChecking, cdLocalSymbolInformation, cdRTTI, cdRangeChecking]) and
    not (FirstChar(TokenCompilerDirectiveArgument) in ['+', '-']) then
    case Result of
      cdInputOutputChecking: Result := cdIncludeFile;
      cdLocalSymbolInformation: Result := cdLinkObjectFile;
      cdRTTI: Result := cdMINSTACKSIZE;
      cdRangeChecking: Result := cdResource;
    end;
end;

function TDelphiParser.TokenCompilerDirectiveArgument: string;
var
  P, Q: PChar;
begin
  CheckToken(toCompilerDirective);
  P := FTokenPtr + 2;
  while P^ in ['A'..'Z', 'a'..'z', '0'..'9'] do
    Inc(P);
  while P^ = ' ' do
    Inc(P);
  Q := P;
  while Q^ <> '}' do
    Inc(Q);

  Dec(Q);
  while (Q > P) and (Q^ = ' ') do
    Dec(Q);
  Inc(Q);

  SetString(Result, P, Q - P);
end;

function TDelphiParser.TokenIsConditionalCompilation: Boolean;
begin
  Result := (Token = toCompilerDirective) and
    (TokenCompilerDirective in [cdIFDEF, cdIFNDEF, cdELSE, cdENDIF]);
end;

function TDelphiParser.TokenSymbolInC(
  const List: array of string): Integer;
begin
  Result := TokenSymbolIn(List);
  if (Result >= 0) and DoRecapitalize then
    RecapitalizeTokenWith(List[Result]);
end;

function TDelphiParser.TokenSymbolIsC(const S: string): Boolean;
begin
  Result := TokenSymbolIs(S);
  if Result then
    RecapitalizeTokenWith(S);
end;

//=== TDpkParser =============================================================

constructor TDpkParser.Create;
begin
  inherited;
  FList := TStringList.Create;
  TStringList(FList).Sorted := True;
end;

destructor TDpkParser.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TDpkParser.Init;
begin
  inherited Init;
  FList.Clear;
end;

function TDpkParser.Parse: Boolean;
begin
  ReadUntilContainsBlock;
  while ReadFileReference do
    ;
  {while ReadUntilFunction do
    ReadFunction;}
  Result := True;
end;

function TDpkParser.ReadFileReference: Boolean;
begin
  DoNextToken;
  Result := (Token = toSymbol) and not TokenSymbolIs('end');
  if not Result then
    Exit;

  List.Add(TokenString);
  ReadNextToken;
  while not (Token in [toComma, toSemiColon, toEof]) do
    ReadNextToken;
end;

procedure TDpkParser.ReadUntilContainsBlock;
begin
  SkipUntilSymbol('contains');
end;

//=== TDtxBaseItem ===========================================================

procedure TDtxBaseItem.SetData(const Value: string);
begin
  FData := RemoveSepLines(Value);
  EnsureEndingCRLF(FData);
end;

//=== TDtxCompareParser ======================================================

constructor TDtxCompareParser.Create;
begin
  inherited;
  FList := TDtxItems.Create;
  FTags := TStringList.Create;
  FOptions := [dpoParameters, dpoDefaultText];
end;

destructor TDtxCompareParser.Destroy;
begin
  FList.Free;
  FTags.Free;
  inherited;
end;

function TDtxCompareParser.Execute(const AFileName: string): Boolean;
begin
  FErrors := [];
  FDefaultTexts := [];
  FTags.Clear;

  Result := FileExists(AFileName);
  if not Result then
    Exit;

  FPasFileNameWithoutPath := ChangeFileExt(ExtractFileName(AFileName), '.pas');

  FStream := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
  try
    FList.Clear;
    Init;
    Result := Parse;
  finally
    FreeAndNil(FStream);
  end;
end;

function TDtxCompareParser.GetDtxCompareTokenType: TDtxCompareTokenType;
var
  S: string;
begin
  S := TokenString;
  if Length(S) > 2 then
  begin
    if (S[1] = '@') and (S[2] = '@') then
      Result := ctHelpTag
    else
      if (S[1] = '#') and (S[2] = '#') then
      Result := ctParseTag
    else
      if (S[1] = '-') and (S[2] = '-') then
      Result := ctSeperator
    else
      Result := ctText;
  end
  else
    Result := ctText;
end;

function TDtxCompareParser.GetPackage: string;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if (FList[I] is TDtxStartItem) and (TDtxStartItem(FList[I]).Symbol = dssPackage) then
    begin
      Result := RemoveStartEndCRLF(TDtxStartItem(FList[I]).Data);
      Exit;
    end;
  Result := '';
end;

function TDtxCompareParser.Parse: Boolean;
begin
  FErrors := [defNoPackageTag, defNoStatusTag, defNoAuthor];
  FLastWasNewLine := True; { BOF }

  ReadStartBlock;
  ReadRest;

  FList.ConstructSkipList;
  Result := True;
end;

procedure TDtxCompareParser.ReadAuthor;
begin
  LowNextToken;
  if CompareTokenType = ctText then
    Exclude(FErrors, defNoAuthor);
end;

procedure TDtxCompareParser.ReadFileInfo;
begin
  {
    PRE  : Token = first token after @@Somepasfilename.pas
    POST : Token = toEof or Token = @@SomeHelpTag }

  { We only determine whether an author is specified }

  while (Token <> toEof) and (CompareTokenType <> ctHelpTag) do
    if FLastWasNewLine and TokenSymbolIs('author') then
      ReadAuthor
    else
      LowNextToken;
end;

procedure TDtxCompareParser.ReadHasTocEntry(Item: TDtxHelpItem);
var
  S: string;
begin
  if Item = nil then
    ErrorStr('ReadCombineWith, Item = nil');
  LowNextToken;
  if CompareTokenType = ctText then
  begin
    S := TokenString;
    { Expect 'ON>' or 'OFF>' }
    if (S > '') and (S[Length(S)] = '>') then
      Delete(S, Length(S), 1);

    if SameText(S, 'on') then
      Item.HasTocEntry := True
    else
    if SameText(S, 'off') then
      Item.HasTocEntry := False
    else
      Include(FErrors, defHasTocEntryError);
    LowNextToken;
  end;
end;

procedure TDtxCompareParser.ReadHelpTopic(Item: TDtxHelpItem);

  procedure CheckDefaultText(var ACheck: string);
  var
    DefaultText: TDefaultText;
  begin
    for DefaultText := Low(TDefaultText) to High(TDefaultText) do
      case CheckText(ACheck, CDefaultText[DefaultText], DefaultText = dtDescriptionFor) of
        ctPrefix:
          Exit;
        ctEqual:
          begin
            ACheck := '';
            Include(FDefaultTexts, DefaultText);
            Exit;
          end;
      end;
    ACheck := '';
  end;

var
  Check: string;
  LTokenString: string;
  LastWasSee, CurrentIsSee: Boolean;
  Link: string;
begin
  { PRE  : Token = first token after @@SomeHelpTag
    POST : Token = toEof or Token = @@SomeHelpTag
  }

  Check := '';
  CurrentIsSee := False;

  while (Token <> toEof) and (CompareTokenType <> ctHelpTag) do
  begin
    LastWasSee := CurrentIsSee;
    CurrentIsSee := FLastWasNewLine and (CompareTokenType = ctText) and TokenSymbolIsExact('See');

    { We use continue, otherwise the code becomes unreadable }
    if CompareTokenType <> ctText then
    begin
      { Token is not a text token, thus ------etc or ##something }
      Check := '';
      LowNextToken;
      Continue;
    end;

    LTokenString := TokenString;

    if dpoDefaultText in Options then
    begin
      { check defaults }
      if Check = '' then
        Check := LTokenString
      else
        Check := Check + ' ' + LTokenString;
      CheckDefaultText(Check);
      if Check = '' then
        Check := LTokenString;
    end;

    if LastWasSee and (CompareStr(LTokenString, 'Also') = 0) then
    begin
      ReadSeeAlso(Item);
      Continue;
    end;

    if not FLastWasNewLine or (LTokenString = '') then
    begin
      { Some text in the middle of a line }
      LowNextToken;
      Continue;
    end;

    { Token is a text token that start on a new line }

    if LTokenString[1] = '<' then
    begin
      { Special tokens that start with a '<' }
      if SameText(LTokenString, '<LINK') or SameText(LTokenString, '<ALIAS') then
      begin
        LowNextToken;

        if dpoLinks in Options then
        begin
          if ReadLink(Link) then
            Item.AddLink(Link);
          LowNextToken;
        end;
      end
      else
        if SameText(LTokenString, '<COMBINE') then
      begin
        LowNextToken;
        if ReadLink(Link) then
          Item.Combine := Link;
        LowNextToken;
      end
      else
        if SameText(LTokenString, '<COMBINEWith') then
      begin
        LowNextToken;
        if ReadLink(Link) then
          Item.CombineWith := Link;
        LowNextToken;
      end
      else
        if SameText(LTokenString, '<HASTOCENTRY') then
        ReadHasTocEntry(Item)
      else
        if SameText(LTokenString, '<TITLEIMG') then
        ReadTitleImg(Item)
      else
        { Token starts with < but not a known token }
        LowNextToken;
    end
    else
    begin
      if CompareStr(LTokenString, 'Parameters') = 0 then
        ReadParameters(Item)
      else
        if SameText(LTokenString, 'JVCLInfo') then
        ReadJVCLINFO(Item)
      else
        { Token start on new line but not a known token }
        LowNextToken;
    end;
  end;
end;

procedure TDtxCompareParser.ReadItem(Item: TDtxHelpItem);
begin

end;

procedure TDtxCompareParser.ReadJVCLINFO(Item: TDtxHelpItem);
var
  IsFlag, IsGroup: Boolean;
  FlagFound, GroupFound: Boolean;
begin
  LowNextToken;

  Item.HasJVCLInfo := True;

  FlagFound := False;
  GroupFound := False;

  while (Token <> toEof) and (Pos(toEquals, TokenString) > 0) do
  begin
    IsFlag := StrLComp(PChar(TokenString), 'FLAG=', 5) = 0;
    IsGroup := not IsFlag and (StrLComp(PChar(TokenString), 'GROUP=', 6) = 0);

    if (IsFlag and FlagFound) or (IsGroup and GroupFound) then
      Include(Item.FJVCLInfoErrors, jieDoubles);

    FlagFound := FlagFound or IsFlag;
    GroupFound := GroupFound or IsGroup;

    if Pos('?', TokenString) > 0 then
    begin
      if IsFlag then
        Include(Item.FJVCLInfoErrors, jieFlagNotFilled)
      else
        if IsGroup then
        Include(Item.FJVCLInfoErrors, jieGroupNotFilled)
      else
        Include(Item.FJVCLInfoErrors, jieOtherNotFilled)
    end
    else
      if IsFlag then
      Item.IsRegisteredComponent :=
        SameText('component', Trim(Copy(TokenString, 6, MaxInt)));

    { Skip until new line }
    repeat
      LowNextToken
    until (Token = toEof) or FLastWasNewLine;
  end;

  if not GroupFound then
    Include(Item.FJVCLInfoErrors, jieNoGroup);
end;

function TDtxCompareParser.ReadLink(out Link: string): Boolean;
var
  NewPart: string;
  P: Integer;
begin
  Link := '';

  { identifier can have spaces, such as
    DSAMessageDlgEx@integer@string@string@TGraphic@array of string@array of integer@longint@TDlgCenterKind@integer@integer@integer@integer@TJvDynControlEngine

    we just look if we have a '>' at the end, if not keep reading }

  while CompareTokenType = ctText do
  begin
    { 'SomeLink>,' is possible }
    NewPart := TokenString;
    P := Pos('>', NewPart);
    if P > 0 then
    begin
      Link := Link + Copy(NewPart, 1, P - 1);
      Result := True;
      Exit;
    end;
    Link := Link + NewPart + ' ';

    LowNextToken;
  end;
  Result := False;
end;

function TDtxCompareParser.ReadNextToken: Char;
var
  P: PChar;

  procedure NextChar;
  begin
    Inc(P);
    if P^ = #10 then
      Inc(FSourceLine);
  end;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    '@':
      begin
        Result := toSymbol;

        NextChar;
        if P^ = '@' then
        begin
          // read till eol
          NextChar;
          while P^ in [#32..#255] do
            NextChar;
        end
        else
        begin
          // Same as #32..#255
          NextChar;
          while P^ in [#33..#255] do
            NextChar;
        end;
      end;
    { TODO : Ugly }
    #32..'?', 'A'..#255:
      begin
        NextChar;
        while P^ in [#33..#255] do
          NextChar;
        Result := toSymbol;
      end;
  else
    Result := P^;
    if Result <> toEof then
      NextChar;
  end;
  FSourcePtr := P;
  FToken := Result;
end;

procedure TDtxCompareParser.ReadOther;
begin
  Include(FErrors, defUnknownTag);
  FTags.Add(TokenString);
  LowNextToken;
end;

procedure TDtxCompareParser.ReadPackage;
var
  DtxItem: TDtxStartItem;
begin
  Exclude(FErrors, defNoPackageTag);

  LowNextToken;

  if CollectData then
    BeginLowRecording;

  while (Token <> toEof) and (CompareTokenType = ctText) do
  begin
    if Pos('?', TokenString) > 0 then
      Include(FErrors, defPackageTagNotFilled);

    LowNextToken;
  end;

  if CollectData then
  begin
    EndLowRecordingWithoutCurrent;

    DtxItem := TDtxStartItem.Create;
    FList.Add(DtxItem);

    DtxItem.Symbol := dssPackage;
    DtxItem.Data := LowRecordingStr;
  end;
end;

procedure TDtxCompareParser.ReadParameters(Item: TDtxHelpItem);
var
  Check: string;

  procedure CheckDefaultText;
  begin
    if dtDescriptionForThisParameter in FDefaultTexts then
      Exit;
    if Check = '' then
      Check := TokenString
    else
      Check := Check + ' ' + TokenString;
    case CheckText(Check, CDefaultText[dtDescriptionForThisParameter], False) of
      ctPrefix:
        Exit;
      ctEqual:
        begin
          Check := '';
          Include(FDefaultTexts, dtDescriptionForThisParameter);
          Exit;
        end;
    end;
    Check := '';
  end;
var
  S: string;
begin
  Check := '';

  LowNextToken;
  while True do
  begin
    while not FLastWasNewLine and (Token <> toEof) do
    begin
      if dpoDefaultText in Options then
        CheckDefaultText;
      LowNextToken;
    end;

    if Token = toEof then
      Exit;

    if CompareTokenType <> ctText then
      Break;

    S := TokenString;
    LowNextToken;

    if CompareTokenType <> ctText then
      Break;

    if SameText(S, 'See') and SameText(TokenString, 'Also') then
    begin
      ReadSeeAlso(Item);
      Break;
    end;

    if (Token <> toSymbol) or (TokenSymbolIn(['-', toColon]) < 0) then
      Continue;

    if dpoParameters in Options then
      Item.AddParam(S);
  end;
end;

procedure TDtxCompareParser.ReadRest;
var
  HelpToken: string;
  DtxItem: TDtxHelpItem;
begin
  while (Token <> toEof) and (CompareTokenType = ctHelpTag) do
  begin
    HelpToken := TokenString;
    DtxItem := TDtxHelpItem.Create(HelpToken);
    FList.Add(DtxItem);

    LowNextToken;

    if CollectData then
      BeginLowRecording;

    DtxItem.IsFileInfo := (Length(HelpToken) > 2) and
      SameText(Copy(HelpToken, 3, MaxInt), FPasFileNameWithoutPath);

    if DtxItem.IsFileInfo then
      ReadFileInfo
    else
      ReadHelpTopic(DtxItem);

    if CollectData then
    begin
      EndLowRecordingWithoutCurrent;
      DtxItem.Data := LowRecordingStr;
    end;
  end;
end;

procedure TDtxCompareParser.ReadSeeAlso(Item: TDtxHelpItem);
const
  CCheckWordCount = 3;
  CCheckWords: array[0..CCheckWordCount - 1] of string = ('list', 'here', 'other');
var
  Link: string;
  State: Integer;
  { 0 = first
    1 = first = 'List'
    2 = second = 'here'
    -1 = other
  }
begin
  LowNextToken;
  if CompareTokenType <> ctText then
  begin
    Include(FErrors, defEmptySeeAlso);
    Exit;
  end;

  if not (dpoLinks in Options) then
  begin
    { skip }
    while (Token <> toEof) and (CompareTokenType = ctText) do
      LowNextToken;
  end
  else
  begin
    State := 0;
    while (Token <> toEof) and (CompareTokenType = ctText) do
    begin
      if (State >= 0) and (State < CCheckWordCount) then
      begin
        if TokenSymbolIs(CCheckWords[State]) then
          Inc(State)
        else
          State := -1;
        if State = CCheckWordCount then
        begin
          Item.ClearLinks;
          if dpoDefaultText in Options then
            Include(FDefaultTexts, dtRemoveSeeAlso);

          { skip rest }
          while (Token <> toEof) and (CompareTokenType = ctText) do
            LowNextToken;
          Exit;
        end;
      end;

      if TokenSymbolIn(['<ALIAS', '<LINK']) >= 0 then
      begin
        State := -1;
        LowNextToken;
        if not ReadLink(Link) then
          Exit;
        Item.AddLink(Link);
      end
      else
        Item.AddLink(TokenString);

      LowNextToken;
    end;
  end;
end;

procedure TDtxCompareParser.ReadSkip;
var
  DtxItem: TDtxStartItem;
begin
  LowNextToken;

  if CollectData then
    BeginLowRecording;

  if CompareTokenType = ctText then
  begin
    if Assigned(FSkipList) then
      FSkipList.Add(TokenString);
    LowNextToken;
  end;

  while (Token <> toEof) and (CompareTokenType = ctText) do
  begin
    if Pos('?', TokenString) > 0 then
      Include(FErrors, defPackageTagNotFilled);

    LowNextToken;
  end;

  if CollectData then
  begin
    EndLowRecordingWithoutCurrent;

    DtxItem := TDtxStartItem.Create;
    FList.Add(DtxItem);

    DtxItem.Symbol := dssSkip;
    DtxItem.Data := LowRecordingStr;
  end;
end;

procedure TDtxCompareParser.ReadStartBlock;
begin
  { POST : Token = toEof or CompareTokenString = ctHelpTag }
  while (Token <> toEof) and (CompareTokenType <> ctHelpTag) do
  begin
    if FLastWasNewLine and (CompareTokenType = ctParseTag) then
    begin
      if TokenSymbolIs('##package:') then
        ReadPackage
      else
        if TokenSymbolIs('##status:') then
        ReadStatus
      else
        if TokenSymbolIs('##skip:') then
        ReadSkip
      else
        ReadOther;
    end
    else
      LowNextToken;
  end;
end;

procedure TDtxCompareParser.ReadStatus;
var
  DtxItem: TDtxStartItem;
begin
  Exclude(FErrors, defNoStatusTag);

  LowNextToken;

  if CollectData then
    BeginLowRecording;

  while (Token <> toEof) and (CompareTokenType = ctText) do
    LowNextToken;

  if CollectData then
  begin
    EndLowRecordingWithoutCurrent;

    DtxItem := TDtxStartItem.Create;
    FList.Add(DtxItem);

    DtxItem.Symbol := dssStatus;
    DtxItem.Data := LowRecordingStr;
  end;
end;

procedure TDtxCompareParser.ReadTitleImg(Item: TDtxHelpItem);
var
  S: string;
begin
  if Item = nil then
    ErrorStr('ReadCombineWith, Item = nil');
  LowNextToken;
  if CompareTokenType = ctText then
  begin
    S := TokenString;
    if (S > '') and (S[Length(S)] = '>') then
      Delete(S, Length(S), 1);
    Item.TitleImg := S;
    LowNextToken;
  end;
end;

//=== TDtxHelpItem ===========================================================

procedure TDtxHelpItem.AddLink(const ALink: string);
var
  P: Integer;
begin
  if not Assigned(FLinks) then
    FLinks := TStringList.Create;
  P := Pos(',', ALink);
  if P <= 0 then
    P := Pos('>', ALink);
  if P > 0 then
    FLinks.Add(Copy(ALink, 1, P - 1))
  else
    FLinks.Add(ALink);
end;

procedure TDtxHelpItem.AddParam(const AParam: string);
begin
  if not Assigned(FParameters) then
    FParameters := TStringList.Create;
  FParameters.Add(AParam);
end;

procedure TDtxHelpItem.ClearLinks;
begin
  if Assigned(FLinks) then
    FLinks.Clear;
end;

function TDtxHelpItem.CompareWith(AItem: TDtxBaseItem): Integer;
const
  CBoolInt: array[Boolean] of Integer = (0, -1);
var
  OtherItem: TDtxHelpItem;
begin
  if AItem is TDtxHelpItem then
  begin
    OtherItem := TDtxHelpItem(AItem);

    Result := CBoolInt[FIsFileInfo] - CBoolInt[OtherItem.FIsFileInfo];
    if (Result = 0) and not FIsFileInfo then
      Result := CompareText(FTag, OtherItem.FTag);
  end
  else
    Result := 1;
end;

constructor TDtxHelpItem.Create(const ATag: string);
begin
  FParameters := TStringList.Create;
  FLinks := TStringList.Create;
  FTag := ATag;
  FHasTocEntry := True;
end;

destructor TDtxHelpItem.Destroy;
begin
  FParameters.Free;
  FLinks.Free;
  inherited;
end;

procedure TDtxHelpItem.WriteToStream(AStream: TStream);
var
  S: string;
begin
  S := Tag + #13#10 + FData;
  AStream.Write(PChar(S)^, Length(S));
end;

//=== TDtxItems ==============================================================

procedure TDtxItems.CombineWithPasList(APasItems: TPasItems);
var
  I: Integer;
  HelpItem: TDtxHelpItem;
  Index: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] is TDtxHelpItem then
    begin
      HelpItem := TDtxHelpItem(Items[I]);
      Index := APasItems.IndexOfReferenceName(HelpItem.Tag);
      if Index >= 0 then
        HelpItem.PasObj := APasItems[Index];
    end;
end;

procedure TDtxItems.ConstructSkipList;
var
  I: Integer;
begin
  FSkipList.Clear;
  for I := 0 to Count - 1 do
    if (Items[I] is TDtxStartItem) and (TDtxStartItem(Items[I]).Symbol = dssSkip) then
      FSkipList.Add(
        AddLeading(RemoveStartEndCRLF(TDtxStartItem(Items[I]).Data)));
end;

constructor TDtxItems.Create;
begin
  inherited Create;
  FSkipList := TStringList.Create;
  with TStringList(FSkipList) do
  begin
    CaseSensitive := True;
    Sorted := True;
    Duplicates := dupIgnore;
  end;
end;

destructor TDtxItems.Destroy;
begin
  FSkipList.Free;
  inherited Destroy;
end;

procedure TDtxItems.DtxSort;
begin
  Sort(DtxSortCompare);
end;

procedure TDtxItems.FillWithDtxHeaders(Dest: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I] is TDtxHelpItem then
      with TDtxHelpItem(Items[I]) do
        if HasTocEntry then
          Dest.Add(Tag);
end;

function TDtxItems.IndexOfReferenceName(ReferenceName: string): Integer;
begin
  ReferenceName := AddLeading(ReferenceName);

  Result := 0;
  while (Result < Count) and
    (not (Items[Result] is TDtxHelpItem) or
    not SameText((Items[Result] as TDtxHelpItem).Tag, ReferenceName)) do
    Inc(Result);
  if Result >= Count then
    Result := -1;
end;

procedure TDtxItems.WriteToFile(const AFileName: string);
const
  cSepLength = 100;
var
  Stream: TFileStream;
  I: Integer;
  Sep: string;
begin
  SetLength(Sep, cSepLength + 2);
  FillChar(PChar(Sep)^, cSepLength, '-');
  Sep[cSepLength + 1] := #13;
  Sep[cSepLength + 2] := #10;

  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    for I := 0 to Count - 1 do
    begin
      if Items[I] is TDtxHelpItem then
        Stream.Write(PChar(Sep)^, cSepLength + 2);
      (Items[I] as TDtxBaseItem).WriteToStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

//=== TDtxStartItem ==========================================================

function TDtxStartItem.CompareWith(AItem: TDtxBaseItem): Integer;
begin
  if AItem is TDtxStartItem then
  begin
    Result := Ord(FSymbol) - Ord(TDtxStartItem(AItem).FSymbol);
    if (Result = 0) and (FSymbol in [dssOther, dssSkip]) then
      Result := CompareText(FData, TDtxStartItem(AItem).FData);
  end
  else
    Result := -1;
end;

procedure TDtxStartItem.SetSymbol(const Value: TDtxStartSymbol);
begin
  FSymbol := Value;
  case FSymbol of
    dssPackage: FSymbolStr := '##Package:';
    dssStatus: FSymbolStr := '##Status:';
    dssSkip: FSymbolStr := '##Skip:';
  end;
end;

procedure TDtxStartItem.SetSymbolStr(const Value: string);
begin
  FSymbolStr := Value;

  if SameText(FSymbolStr, '##package:') then
    FSymbol := dssPackage
  else
    if SameText(FSymbolStr, '##status:') then
    FSymbol := dssStatus
  else
    if SameText(FSymbolStr, '##skip:') then
    FSymbol := dssSkip
  else
    FSymbol := dssOther;
end;

procedure TDtxStartItem.WriteToStream(AStream: TStream);
var
  S: string;
begin
  S := SymbolStr + ' ' + Data;
  AStream.Write(PChar(S)^, Length(S));
end;

//=== TFunctionParser ========================================================

constructor TFunctionParser.Create;
begin
  inherited;
  FFunctions := TStringList.Create;
  FSkipped := TStringList.Create;
end;

destructor TFunctionParser.Destroy;
begin
  FFunctions.Free;
  FSkipped.Free;
  inherited;
end;

function TFunctionParser.ExecuteFile(const AFileName: string): Boolean;
begin
  FFileName := ExtractFileName(AFileName);

  Result := inherited ExecuteFile(AFileName);
end;

procedure TFunctionParser.Init;
begin
  inherited Init;
  FFunctions.Clear;
  FSkipped.Clear;
end;

function TFunctionParser.Parse: Boolean;
begin
  ReadUntilImplementationBlock;
  while ReadUntilFunction do
    ReadFunction;
  Result := True;
end;

procedure TFunctionParser.ReadFunction;

  function CheckAllChars(const S: string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    I := 0;
    while Result and (I < Length(S)) do
    begin
      Result := S[I + 1] in ['a'..'z', 'A'..'Z', '0'..'9', '_'];
      Inc(I);
    end;
  end;

var
  S: string;
begin
  DoNextToken;
  S := TokenString;
  if not CheckAllChars(S) then
  begin
    FSkipped.Add(S);
    Exit;
  end;

  DoNextToken;
  if TokenString <> '.' then
  begin
    FFunctions.Add(Format('%s (%s)', [S, FFileName]));
  end;
end;

function TFunctionParser.ReadUntilFunction: Boolean;
begin
  while (Token <> toEof) and not TokenSymbolIs('function')
    and not TokenSymbolIs('procedure') do

    if TokenSymbolIs('class') then
      SkipClass(False)
    else
      DoNextToken;

  Result := Token <> toEof;
end;

procedure TFunctionParser.ReadUntilImplementationBlock;
begin
  SkipUntilSymbol('implementation');
end;

//=== TImplementationParser ==================================================

function TImplementationParser.Parse: Boolean;
var
  LModuleType: TModuleType;
  GlobalProcedureNames: TStringList;
  I: Integer;
begin
  if Assigned(FOutputStream) then
    BeginOutputTo(FOutputStream);

  InterpretCompilerDirectives := False;

  while Token <> toSymbol do
    DoNextToken;
  ReadUnitBlock(LModuleType);
  if LModuleType = mtLibrary then
  begin
    Result := True;
    Exit;
  end;
  ReadInterfaceStatement;
  SkipUsesBlock;
  SkipInterfaceBlock;

  { collect names of global procedures }
  GlobalProcedureNames := TStringList.Create;
  try
    GlobalProcedureNames.Sorted := True;
    GlobalProcedureNames.Duplicates := dupIgnore;
    for I := 0 to FPasItems.Count - 1 do
      if FPasItems[I].DelphiType in [dtFunction, dtProcedure] then
        GlobalProcedureNames.Add(FPasItems[I].SimpleName);
    FPasItems.Clear;

    CheckTokenSymbolC('implementation');
    if Assigned(FOutputStream) then
      EndOutputTo;

    DoNextToken(False);

    ReadImplementationBlock;

    for I := 0 to FPasItems.Count - 1 do
      if FPasItems[I] is TBaseFuncItem then
        with TBaseFuncItem(FPasItems[I]) do
          IsLocal := GlobalProcedureNames.IndexOf(SimpleName) < 0;

    if SortImplementation then
      TypeList.SortImplementation
    else
      TypeList.OnlyCapitalization;

    if Assigned(FOutputStream) then
      TypeList.WriteImplementationToStream(FOutputStream);

    Result := True;
  finally
    GlobalProcedureNames.Free;
  end;
end;

//=== TPasCasingParser =======================================================

constructor TPasCasingParser.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  with FList as TStringList do
  begin
    Duplicates := dupIgnore;
    CaseSensitive := True;
    Sorted := True;
  end;
end;

destructor TPasCasingParser.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TPasCasingParser.Parse: Boolean;
var
  I: Integer;
  Item: TAbstractItem;
  Index: Integer;
begin
  if AllSymbols then
  begin
    while Token <> toEof do
    begin
      if Token = toSymbol then
      begin
        if DoCount then
        begin
          Index := FList.Add(TokenString);
          FList.Objects[Index] := TObject(Integer(FList.Objects[Index]) + 1);
        end
        else
          FList.AddObject(TokenString, TObject(ID));
      end;
      DoNextToken;
    end;

    Result := True;
  end
  else
  begin
    Result := inherited Parse;
    if not Result then
      Exit;

    for I := 0 to FPasItems.Count - 1 do
    begin
      Item := TAbstractItem(FPasItems[I]);
      if DoCount then
      begin
        Index := FList.Add(Item.SimpleName);
        FList.Objects[Index] := TObject(Integer(FList.Objects[Index]) + 1);
      end
      else
        FList.AddObject(Item.SimpleName, TObject(ID));

      if Item is TClassPropertyItem then
      begin
        if DoCount then
        begin
          Index := FList.Add(TClassPropertyItem(Item).TypeStr);
          FList.Objects[Index] := TObject(Integer(FList.Objects[Index]) + 1);
        end
        else
          FList.AddObject(TClassPropertyItem(Item).TypeStr, TObject(ID));
      end;
    end;
  end;
end;

//=== TPasCheckParser ========================================================

function TPasCheckParser.ExecuteFile(const AFileName: string): Boolean;
var
  FindData: TWin32FindData;
  Handle: THandle;
begin
  { TODO : Naar init }
  FErrors := [pefNoLicense];

  Result := FileExists(AFileName);
  if not Result then
    Exit;

  Handle := FindFirstFile(PChar(AFileName), FindData);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if not Result then
    Exit;

  Windows.FindClose(Handle);
  FFileName := ChangeFileExt(FindData.cFileName, '');

  Result := inherited ExecuteFile(AFileName);
end;

function TPasCheckParser.Parse: Boolean;
begin
  ReadCommentBlock;
  ReadUnitName;

  Result := True;
end;

procedure TPasCheckParser.ReadCommentBlock;
const
  CLicenseText = 'The contents of this file are subject to the';
var
  LicenseFound: Boolean;
begin
  LicenseFound := False;
  while Token = toComment do
  begin
    LicenseFound := LicenseFound or
      (Pos(CLicenseText, TokenString) > 0);
    DoNextToken(False);
  end;
  if LicenseFound then
    Exclude(FErrors, pefNoLicense);
end;

procedure TPasCheckParser.ReadUnitName;
begin
  while (Token <> toEof) and not TokenSymbolIs('unit') do
    DoNextToken;
  DoNextToken;
  CheckToken(toSymbol);
  FUnitName := TokenString;
  if CompareStr(FUnitName, FFileName) <> 0 then
    Include(FErrors, pefUnitCase);
end;

//=== TRecapitalizeParser ====================================================

function TRecapitalizeParser.Parse: Boolean;
var
  Ch: Char;
begin
  Result := Assigned(FOutputStream) and Assigned(FCapitalization);
  if not Result then
    Exit;

  BeginOutputTo(FOutputStream);

  while Token <> toEof do
    DoNextToken;

  EndOutputTo;

  Ch := '.';
  FOutputStream.WriteBuffer(Ch, 1);
end;

//=== TRegisteredClassesParser ===============================================

constructor TRegisteredClassesParser.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  TStringList(FList).Sorted := True;

  WantCompilerDirectives := False;
  InterpretCompilerDirectives := False;
end;

destructor TRegisteredClassesParser.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TRegisteredClassesParser.Init;
begin
  inherited Init;
  FList.Clear;
end;

function TRegisteredClassesParser.Parse: Boolean;
begin
  if not ReadUntilRegisterBlock then
  begin
    Result := True;
    Exit;
  end;
  while ReadUntilRegisterComponentsBlock and ReadRegisterComponentsBlock do
    ;
  Result := True;
end;

function TRegisteredClassesParser.ReadRegisterComponentsBlock: Boolean;
begin
  { PRE : Token = 'RegisterComponents'
    POST: Token = token after ; or )

   Example:

    RegisterComponents(x, [C1]);
    RegisterComponents(x, [C1, C2, C3]);
  }

  DoNextToken;
  CheckToken(toLeftParens);
  SkipUntilToken(toComma);
  DoNextToken;
  CheckToken(toLeftBracket);
  DoNextToken;
  CheckToken(toSymbol);
  List.Add(TokenString);
  DoNextToken;
  while Token = toComma do
  begin
    DoNextToken;
    CheckToken(toSymbol);
    List.Add(TokenString);
    DoNextToken;
  end;
  CheckToken(toRightBracket);
  DoNextToken;
  CheckToken(toRightParens);
  DoNextToken;
  if Token = toSemiColon then
    DoNextToken;
  Result := Token <> toEof;
end;

function TRegisteredClassesParser.ReadUntilRegisterBlock: Boolean;
begin
  while (Token <> toEof) and not TokenSymbolIsExact('Register') and not TokenSymbolIs('implementation') do
    DoNextToken;

  Result := TokenSymbolIsExact('Register');
end;

function TRegisteredClassesParser.ReadUntilRegisterComponentsBlock: Boolean;
begin
  SkipUntilSymbol('RegisterComponents');
  Result := TokenSymbolIs('RegisterComponents');
end;

end.

