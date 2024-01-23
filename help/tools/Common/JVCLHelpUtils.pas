unit JVCLHelpUtils;

interface

uses
  Windows, Classes, ContNrs,
  SysUtils;

const
  SpaceChar = #32;
  TabChar = #8;

  MaxGuidListSize = Maxint div 32;

const
  cUnknownGroupStr = 'JVCL.??';
const
  cSummaryDefaultText = 'Write here a summary (1 line)';
  cSummaryDefaultTextForBuild = 'Not documented';
  cDescriptionDefaultText = 'Write here a description';
  cEditLink = '<EXTLINK http://help.delphi-jedi.org/item.php?Name=%s>%s</EXTLINK>';
  //  cDescriptionDefaultTextForBuild = 'This topic is undocumented. Click <EXTLINK http://help.delphi-jedi.org/item.php?Name=%s>here</EXTLINK> to document it.';
  cDescriptionDefaultTextForBuild = 'This topic is undocumented. Click on "Edit topic" to document it.';
  cDescriptionOverrideDefaultText =
    'This is an overridden method, you don''t have to describe these if it does ' +
    'the same as the inherited method';
  cDescriptionOverloadDefaultText =
    'This is an overloaded function/procedure, if possible you may combine the ' +
    'description of all these functions into 1 general description. If you do so, ' +
    'combine all "Parameter" lists into 1 list, and leave the "Summary", "Description" ' +
    'etc. fields empty for all other overloaded functions with the same name.';
  cSeeAlsoDefaultText =
    'List here other properties, methods (comma separated) Remove the ''See Also'' ' +
    'section if there are no references';
  cParameterDefaultText = 'Description for this parameter';
  cParameterDefaultTextForBuild = cSummaryDefaultTextForBuild;
  cReturnValueDefaultText = 'Describe here what the function returns';
  cEnumerateDefaultText = 'Description for %s';
  cEnumerateDefaultTextForBuild = cSummaryDefaultTextForBuild;

type
  TTopicToken = (tkSummary, tkNote, tkReturnValue, tkDescription,
    tkParameters, tkSeeAlso, tkJVCLInfo, tkGroup, tkFlag, {tkInclude,}
    tkAuthor, tkAlias, tkAliasOf, tkCombine, tkCombineWith, tkTitle, tkTitleImg,
    xtkHasTocEntry, tkDonator, tkGroup2, tkHasPasFileEntry, tkPlatform, tkTopicOrder,
    tkOther);

  TSymbolToken = (toEOF, toString, toEmpty, toDot, toNumber,
    xtoLink, toTableBegin, toTableEnd, toBoldBegin, toBoldEnd,
    toItalicBegin, toItalicEnd, toUnderlineBegin, toUnderlineEnd, toParagraph,
    toPreBegin, toPreEnd, toCodeBegin, toCodeEnd, toExtLinkBegin,
    toExtLinkEnd, toImage, toAutoLink, toNoWhiteSpace, toBOF,
    toInclude, toColorBegin, toColorEnd, toFlag, toKeyWords, toImplementation, toVersionSpecific,
    toDelphiLink
    );

  TDtxSelectionKind = (dskMax, dskPercentage, dskSpecific);

  IProgress = interface
    procedure StatusMsg(const Msg: string);
    procedure ErrorMsg(const Msg: string);
    procedure WarningMsg(const Msg: string);
    procedure HintMsg(const Msg: string);
    procedure ErrorHeader(const Msg: string);
  end;

  TGuidList = class;

  PGuidItem = ^TGuidItem;
  TGuidItem = record
    FGuid: TGuid;
    FObject: TObject;
  end;

  PGuidItemList = ^TGuidItemList;
  TGuidItemList = array[0..MaxGuidListSize] of TGuidItem;
  TGUIDListSortCompare = function(List: TGUIDList; Index1, Index2: Integer): Integer;

  TGUIDList = class
  private
    FList: PGuidItemList;
    FSorted: Boolean;
    FCount: Integer;
    FDuplicates: TDuplicates;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    function GetCount: Integer;
    function GetObject(Index: Integer): TObject;
    procedure PutObject(Index: Integer; AObject: TObject);
    function Get(Index: Integer): TGUID;
    procedure Grow;
    procedure Put(Index: Integer; const AGUID: TGUID);
    procedure SetSorted(const Value: Boolean);
    procedure InsertItem(Index: Integer; const AGUID: TGUID; AObject: TObject);
    procedure QuickSort(L, R: Integer; SCompare: TGUIDListSortCompare);
    procedure SetCapacity(NewCapacity: Integer);
  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
  public
    constructor Create(AOwnsObjects: Boolean); virtual;
    destructor Destroy; override;
    function Add(const AGUID: TGUID): Integer;
    function AddObject(const AGUID: TGUID; AObject: TObject): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function Remove(const AGUID: TGUID): Integer;
    function Find(const AGUID: TGUID; var Index: Integer): Boolean;
    function IndexOf(const AGUID: TGUID): Integer;
    procedure Sort;
    procedure CustomSort(Compare: TGUIDListSortCompare); virtual;
    property Count: Integer read GetCount;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Guids[Index: Integer]: TGUID read Get write Put; default;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Sorted: Boolean read FSorted write SetSorted;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  TIntegerList = class(TList)
  private
    function GetItems(Index: Integer): Integer;
    procedure SetItems(Index: Integer; const Value: Integer);
  public
    function Add(AInteger: Integer): Integer;
    function Contains(AInteger: Integer): Boolean;
    procedure Sort;
    property Items[Index: Integer]: Integer read GetItems write SetItems; default;
  end;

  TStringStack = class(TObject)
  private
    FStrings: TStrings;
  protected
    procedure PushItem(const AItem: string);
    function PopItem: string;
    function PeekItem: string;
    property Strings: TStrings read FStrings;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function AtLeast(ACount: Integer): Boolean;
    function Push(const AItem: string): string;
    function Pop: string;
    function Peek: string;
  end;

type
  ITaskManager = interface
    procedure TaskStatus(const S: string);
    procedure TaskProgress(const Position, Max: Integer);
    procedure TaskDone;
  end;

  TProgress = class
  private
    FTaskCount: Integer;
    FTaskIndex: Integer;
    FCurrentTaskPosition: Integer;
    FCurrentTaskMax: Integer;
    function GetAllTaskPosition: Integer;
    procedure SetCurrentTaskMax(const Value: Integer);
    procedure SetCurrentTaskPosition(const Value: Integer);
    procedure SetTaskCount(const Value: Integer);
    function GetAllTaskMax: Integer;
  public
    procedure NextTask;
    procedure Start;

    { all task progress bar }
    property AllTaskMax: Integer read GetAllTaskMax;
    property AllTaskPosition: Integer read GetAllTaskPosition;

    { current task progress bar }
    property CurrentTaskMax: Integer read FCurrentTaskMax write SetCurrentTaskMax;
    property CurrentTaskPosition: Integer read FCurrentTaskPosition write SetCurrentTaskPosition;

    property TaskCount: Integer read FTaskCount write SetTaskCount;
    property TaskIndex: Integer read FTaskIndex;
  end;

  TTask = class
  private
    FTaskManager: ITaskManager;
  protected
    procedure Progress(const Position, Count: Integer);
    procedure Done;

    function CanStart: Boolean; virtual;
    function DoExecute: Boolean; virtual; abstract;
    function GetTaskDescription: string; virtual; abstract;
  public
    constructor Create(ATaskManager: ITaskManager); virtual;
    function Execute: Boolean;
    property TaskManager: ITaskManager read FTaskManager;
    property TaskDescription: string read GetTaskDescription;
  end;

  TXMLWriter = class
  private
    FDestStream: TStream;
    FStack: TStringStack;
    FIdent: Integer;
  protected
    procedure WriteStartElement(const AElement: string;
      const AAttributes: array of string; const AAttributeValues: array of string);
    procedure WriteCloseElement(const AElement: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure BeginElement(const AElement: string); overload;
    procedure BeginElement(const AElement: string;
      const AAttributes: array of string; const AAttributeValues: array of string); overload;
    procedure WriteIndentData(const Data: string);
    procedure WriteContent(const AContent: string);
    procedure WriteEmptyElement(const AElement: string);
    procedure WriteElementContent(const AElement, AContent: string);
    procedure WriteElementContentNE(const AElement, AContent: string);
    procedure WriteElementAttribute(const AElement, AAttribute, AAttributeValue: string);
    procedure WriteElementAttributeContent(const AElement: string;
      const AAttributes: array of string; const AAttributeValues: array of string;
      const AContent: string);
    procedure EndElement;

    property DestStream: TStream read FDestStream write FDestStream;
  end;

  TDtxBaseItem = class(TPersistent)
  public
    procedure Save(AXMLWriter: TXMLWriter); virtual; abstract;
  end;

  //  ##Package: Custom
  //  ##Status: Completed, Incomplete
  //  ##Skip: PHintData
  //  ##Skip: THintData
  //  ##Skip: TJvBalloonHint.Handle
  //  ##Skip: TJvStemSize

  TSimpleDtxComment = class(TDtxBaseItem)
  private
    FValue: string;
  public
    procedure Save(AXMLWriter: TXMLWriter); override;
    property Value: string read FValue write FValue;
  end;

  // (1)
  //@@TJvAnimationStyle
  //<TITLE TJvAnimationStyle type>
  //Summary
  //  Defines values that determine the animation style of a balloon hint.
  //Description
  //  TJvAnimationStyle values determine the animation style of a balloon hint.
  //
  // (2)
  // @@TJvAnimationStyle.atNone
  // No animation.
  //
  // (3)
  //JVCLInfo
  //  GROUP=JVCL.FormsAndApp.Forms.DecoratingAndEffects
  //  FLAG=Component
  //
  // (4)
  //@@TJvBalloonHint.ActivateHint@TControl@string@TImageIndex@string@Integer
  //<COMBINE TJvBalloonHint.ActivateHint@TControl@string@string@Integer>

  TSimpleDtxTopic = class(TDtxBaseItem)
  private
    FValues: array[TTopicToken] of string;
    FName: string;
    function GetString(const Index: TTopicToken): string;
    procedure SetString(const Index: TTopicToken; const Value: string);
    function GetIsPasTopic: Boolean;
  public
    procedure Save(AXMLWriter: TXMLWriter); override;

    procedure AddLineSkipEmpty(const ATopicToken: TTopicToken; const S: string);
    procedure AddLine(const ATopicToken: TTopicToken; const S: string);

    property IsPasTopic: Boolean read GetIsPasTopic;
    property Name: string read FName write FName;
    property Summary: string index tkSummary read GetString write SetString;
    property Note: string index tkNote read GetString write SetString;
    property ReturnValue: string index tkReturnValue read GetString write SetString;
    property Donator: string index tkDonator read GetString write SetString;
    property Description: string index tkDescription read GetString write SetString;
    property Parameters: string index tkParameters read GetString write SetString;
    property SeeAlso: string index tkSeeAlso read GetString write SetString;
    property Group: string index tkGroup read GetString write SetString;
    property HasTocEntry: string index xtkHasTocEntry read GetString write SetString;
    property HasPasFileEntry: string index tkHasPasFileEntry read GetString write SetString;
    property Flag: string index tkFlag read GetString write SetString;
    //    property Include: string index tkInclude read GetString write SetString;
    property Author: string index tkAuthor read GetString write SetString;
    property Alias: string index tkAlias read GetString write SetString;
    property AliasOf: string index tkAliasOf read GetString write SetString;
    property Combine: string index tkCombine read GetString write SetString;
    property CombineWith: string index tkCombineWith read GetString write SetString;
    property Title: string index tkTitle read GetString write SetString;
    property TitleImg: string index tkTitleImg read GetString write SetString;
    property Group2: string index tkGroup2 read GetString write SetString;
    property TopicOrder: string index tkTopicOrder read GetString write SetString;
  end;

  TSimpleDtxList = class
  private
    FProgress: IProgress;
    FLines: TStrings;
    FIndex: Integer;
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(const Index: Integer): TDtxBaseItem;
  protected
    procedure Add(AItem: TDtxBaseItem);

    function ParseLines: Boolean;

    function ReadTopicName(const ATopicName: string): Boolean;
    function ReadComment(const AComment: string): Boolean;
    procedure ReadUntilEndElement(ADtxTopic: TSimpleDtxTopic; const ATopicToken: TTopicToken);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadFromFile(const AFileName: string): Boolean;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: TDtxBaseItem read GetItems; default;
  end;

  TStringObjectList = class;

  TStringObjectList = class(TStringList)
  protected
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure ClearObjects;
    procedure Delete(Index: Integer); override;
  end;

  TRegisteredComponents = class
  private
    FRegisteredComponents: TStringList;
    FGroupList: TStringList;
    FRegisteredComponentsFileName: string;
    FGroupListFileName: string;
  public
    constructor Create(const ARegisterComponentsFileName, AGroupListFileName: string);
    destructor Destroy; override;
    function Parse: Boolean;
    function IsRegisteredComponent(const AComponentName: string): Boolean;
    function GroupStr(const AComponentName: string): string;
  end;

  TOwnedStringList = class(TStringList)
  public
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
  end;

function Pad(const S: string; const Width: Integer; const PadChar: Char = SpaceChar): string; forward;
procedure Wrap(var S: string; const Width, Indent, NextIndent: Integer); forward;
function MaxWordLengthInString(const S: string): Integer;
function LinkStr(const S: string): string;
procedure GetFirstToken(const Source: string; out Token: string; out RestIndex: Integer);
procedure EatChars(var S: string; const Chars: TSysCharSet);
procedure EatChar(var S: string; const Chars: TSysCharSet);
function IsNullStr(const S: string): Boolean;
procedure StatusMsg(const Msg: string);
procedure StatusMsgFmt(const Msg: string; const Args: array of const);
procedure ErrorMsg(const Msg: string);
procedure ErrorMsgFmt(const Msg: string; const Args: array of const);
procedure WarningMsg(const Msg: string);
procedure WarningMsgFmt(const Msg: string; const Args: array of const);
procedure ErrorHeaderFmt(const Msg: string; const Args: array of const);
procedure ErrorHeader(const Msg: string);
procedure HintMsg(const Msg: string);
procedure HintMsgFmt(const Msg: string; const Args: array of const);
procedure EnsureMinCount(SS: TStrings; const MinCount: Integer); overload;
procedure EnsureMinCount(SS: TList; const MinCount: Integer); overload;
function IsParam(const S: string): Boolean; overload;
function IsParam(const S: string; out Token: string; out RestIndex: Integer): Boolean; overload;
function StartWith2Spaces(const S: string): Boolean;
procedure DiffLists(Source1, Source2, InBoth, NotInSource1, NotInSource2: TStrings;
  const CaseSensitive: Boolean = False);
function HasDuplicates(Strings: TStrings): Boolean;
function IsReadonlyFile(const AFileName: string): Boolean;
function CheckDir(const ADir: string): Boolean;
function CheckFile(const AFileName: string): Boolean;
function AfterDot(const S: string): string;
function BeforeDot(const S: string): string;
procedure GetAllFilesFrom(const ADir, AFilter: string; AFiles: TStrings;
  const IncludeSubDirs: Boolean = False; const StripExtAndPath: Boolean = False);
function PackageToTopicName(const S: string): string;
function StripOverloadAt(const S: string): string;
function LocateString(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
function IsPrefix(const Prefix, S: string): Boolean;
function IsPostfix(const Postfix, S: string): Boolean;
function IsSubString(const SubString, S: string): Boolean;
function LocatePrefix(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
function LocatePostfix(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
function LocateSubString(Strings: TStrings; const StartIndex: Integer; SubStr: string): Integer;
function ChangeSubString(const S, OldStr, NewStr: string): string;
function RegisteredClassNameToImageFileName(const ARegisteredClassName: string): string;

function HasDot(const S: string): Boolean;
function HasAt(const S: string): Boolean;

var
  GProgress: IProgress = nil;

implementation

uses
  Math, JvSearchFiles, JclStrings, RtlConsts;

type
  TTokenTopic = record
    Token: string;
    Topic: TTopicToken;
  end;

const
  // only token on line
  cSingleTokenOnLineArray: array[0..12] of TTokenTopic = (
    (Token: 'Summary'; Topic: tkSummary),
    (Token: 'Notes'; Topic: tkNote),
    (Token: 'Note'; Topic: tkNote),
    (Token: 'Returns'; Topic: tkReturnValue),
    (Token: 'Return value'; Topic: tkReturnValue),
    (Token: 'Description'; Topic: tkDescription),
    (Token: 'Parameters'; Topic: tkParameters),
    (Token: 'See Also'; Topic: tkSeeAlso),
    (Token: 'JVCLInfo'; Topic: tkJVCLInfo),
    (Token: 'Author'; Topic: tkAuthor),
    (Token: 'Donator'; Topic: tkDonator),
    (Token: 'Platform'; Topic: tkPlatform),
    (Token: 'Platforms'; Topic: tkPlatform)
    );

  cTokenArray: array[0..23] of TTokenTopic = (
    (Token: 'Summary:'; Topic: tkSummary),
    (Token: 'Notes:'; Topic: tkNote),
    (Token: 'Note:'; Topic: tkNote),
    (Token: 'Returns:'; Topic: tkReturnValue),
    (Token: 'Result:'; Topic: tkReturnValue),
    (Token: 'Return value:'; Topic: tkReturnValue),
    (Token: 'Description:'; Topic: tkDescription),
    (Token: 'Parameters:'; Topic: tkParameters),
    (Token: 'See Also:'; Topic: tkSeeAlso),
    (Token: 'Platform:'; Topic: tkPlatform),
    (Token: 'Platforms:'; Topic: tkPlatform),
    //    (Token: '<INCLUDE'; Topic: tkInclude),
    (Token: '<ALIAS'; Topic: tkAlias),
    (Token: '<ALIASOF'; Topic: tkAliasOf),
    {10}(Token: '<COMBINE'; Topic: tkCombine),
    (Token: '<COMBINEWITH'; Topic: tkCombineWith),
    (Token: '<TITLE'; Topic: tkTitle),
    (Token: '<GROUP'; Topic: tkGroup2),
    (Token: '<TOPICORDER'; Topic: tkTopicOrder),
    (Token: '<TITLEIMG'; Topic: tkTitleImg),
    (Token: '<HASTOCENTRY'; Topic: xtkHasTocEntry),
    (Token: '<HASPASFILEENTRY'; Topic: tkHasPasFileEntry),
    (Token: 'GROUP'; Topic: tkGroup),
    (Token: 'FLAG'; Topic: tkFlag),
    (Token: 'Donator:'; Topic: tkDonator)
    );

  cDoubleQuote = '"';

  cSingleQuote = '''';

  cAmpersand = '&';

  cSmallerThan = '<';

  cGreaterThan = '>';

  cDoubleQuoteTag = '&quot;';

  cSingleQuoteTag = '&apos;';

  cAmpersandTag = '&amp;';

  cSmallerThanTag = '&lt;';

  cGreaterThanTag = '&gt;';

  cParamSeparators = ['-', ':'];

  //=== Local procedures =======================================================

function Max(const I1, I2: Integer): Integer;
begin
  if I1 > i2 then
    Result := I1
  else
    Result := I2;
end;

function IsAtAt(const S: string): Boolean; overload;
var
  Q: PChar;
begin
  Q := PChar(S);
  while Q^ = ' ' do
    Inc(Q);
  Result := (Q^ = '@') and ((Q + 1)^ = '@');
end;

function IsAtAt(const S: string; out Value: string): Boolean; overload;
var
  P, Q: PChar;
begin
  Q := PChar(S);
  while Q^ = ' ' do
    Inc(Q);
  Result := (Q^ = '@') and ((Q + 1)^ = '@');
  if Result then
  begin
    Inc(Q, 2);
    while Q^ = ' ' do
      Inc(Q);
    P := PChar(S) + Length(S);
    while (P > Q) and ((P - 1)^ = ' ') do
      Dec(P);
    SetString(Value, Q, P - Q);
  end;
end;

function IsComment(const S: string; out Value: string): Boolean;
var
  P, Q: PChar;
begin
  Q := PChar(S);
  while Q^ = ' ' do
    Inc(Q);
  Result := (Q^ = '#') and ((Q + 1)^ = '#');
  if Result then
  begin
    Inc(Q, 2);
    while Q^ = ' ' do
      Inc(Q);
    P := PChar(S) + Length(S);
    while (P > Q) and ((P - 1)^ = ' ') do
      Dec(P);
    SetString(Value, Q, P - Q);
  end;
end;

function IsSeparator(const S: string): Boolean;
var
  P: PChar;
begin
  P := PChar(S);
  Result := (P^ = '-') and ((P + 1)^ = '-');
end;

function TokenToTopicToken(const AToken: string; const IsOnlyTokenOnLine: Boolean): TTopicToken;
var
  I: Integer;
begin
  Result := tkOther;

  if (AToken = '') or (AToken[1] = '\') then
    Exit;

  if IsOnlyTokenOnLine then
  begin
    I := Low(cSingleTokenOnLineArray);
    while (I <= High(cSingleTokenOnLineArray)) and not SameText(AToken, cSingleTokenOnLineArray[I].Token) do
      Inc(I);
    if I <= High(cSingleTokenOnLineArray) then
    begin
      Result := cSingleTokenOnLineArray[I].Topic;
      Exit;
    end;
  end;
  I := Low(cTokenArray);
  while (I <= High(cTokenArray)) and not SameText(AToken, cTokenArray[I].Token) do
    Inc(I);
  if I <= High(cTokenArray) then
    Result := cTokenArray[I].Topic;
end;

function IsRestNull(const S: string; RestIndex: Integer): Boolean;
var
  P: PChar;
begin
  if RestIndex > Length(S) then
    Result := True
  else
  begin
    if RestIndex < 1 then
    begin
      WarningMsg('RestIndex < 1');
      RestIndex := 1;
    end;
    P := PChar(S) + RestIndex - 1;
    while P^ in [SpaceChar, TabChar] do
      Inc(P);
    Result := P^ = #0;
  end;
end;

function EntityEncode(const S: string): string;
var
  AddSize: Integer;
  P, Q: PChar;
begin
  AddSize := 0;
  P := PChar(S);
  while P^ <> #0 do
  begin
    case P^ of
      cDoubleQuote: Inc(AddSize, Length(cDoubleQuoteTag) - 1);
      cSingleQuote: Inc(AddSize, Length(cSingleQuoteTag) - 1);
      cAmpersand: Inc(AddSize, Length(cAmpersandTag) - 1);
      cSmallerThan: Inc(AddSize, Length(cSmallerThanTag) - 1);
      cGreaterThan: Inc(AddSize, Length(cGreaterThanTag) - 1);
    end;
    Inc(P);
  end;
  if AddSize = 0 then
    Result := S
  else
  begin
    SetLength(Result, Length(S) + AddSize);
    P := PChar(S);
    Q := PChar(Result);
    while P^ <> #0 do
    begin
      case P^ of
        cDoubleQuote:
          begin
            Move(cDoubleQuoteTag, Q^, Length(cDoubleQuoteTag));
            Inc(Q, Length(cDoubleQuoteTag));
          end;
        cSingleQuote:
          begin
            Move(cSingleQuoteTag, Q^, Length(cSingleQuoteTag));
            Inc(Q, Length(cSingleQuoteTag));
          end;
        cAmpersand:
          begin
            Move(cAmpersandTag, Q^, Length(cAmpersandTag));
            Inc(Q, Length(cAmpersandTag));
          end;
        cSmallerThan:
          begin
            Move(cSmallerThanTag, Q^, Length(cSmallerThanTag));
            Inc(Q, Length(cSmallerThanTag));
          end;
        cGreaterThan:
          begin
            Move(cGreaterThanTag, Q^, Length(cGreaterThanTag));
            Inc(Q, Length(cGreaterThanTag));
          end;
      else
        Q^ := P^;
        Inc(Q);
      end;
      Inc(P);
    end;
  end;
end;

function IntegerSortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := Integer(Item1) - Integer(Item2);
end;

//=== Global procedures ======================================================

procedure GetAllFilesFrom(const ADir, AFilter: string;
  AFiles: TStrings; const IncludeSubDirs, StripExtAndPath: Boolean);
const
  CIncludeSubDirs: array[Boolean] of TJvDirOption = (doExcludeSubDirs, doIncludeSubDirs);
var
  I: Integer;
begin
  AFiles.BeginUpdate;
  try
    AFiles.Clear;

    with TJvSearchFiles.Create(nil) do
    try
      DirOption := CIncludeSubDirs[IncludeSubDirs];
      RootDirectory := ADir;
      Options := [soSearchFiles, soSorted];
      if StripExtAndPath then
        Options := Options + [soStripDirs];
      ErrorResponse := erIgnore;
      DirParams.SearchTypes := [];
      FileParams.SearchTypes := [stFileMask];
      FileParams.FileMask := AFilter;

      Search;

      if StripExtAndPath then
        for I := 0 to Files.Count - 1 do
          AFiles.Add(ChangeFileExt(Files[I], ''))
      else
        AFiles.Assign(Files);
    finally
      Free;
    end;
  finally
    AFiles.EndUpdate;
  end;
end;

function CheckDir(const ADir: string): Boolean;
begin
  Result := DirectoryExists(ADir);
  if not Result then
    ErrorMsgFmt('Directory ''%s'' does not exist', [ADir]);
end;

function IsReadonlyFile(const AFileName: string): Boolean;
var
  f: THandle;
begin
  f := FileOpen(AFileName, fmOpenWrite);
  if f = THandle(-1) then
    Result := True
  else
  begin
    Result := False;
    FileClose(f);
  end;
end;

function CheckFile(const AFileName: string): Boolean;
begin
  Result := FileExists(AFileName);
  if not Result then
    ErrorMsgFmt('File ''%s'' does not exist', [AFileName]);
end;

function AfterDot(const S: string): string;
var
  DotPos: PChar;
begin
  DotPos := StrScan(PChar(S), '.');
  if Assigned(DotPos) then
  begin
    Inc(DotPos);
    SetString(Result, DotPos, PChar(S) + Length(S) - DotPos);
  end
  else
    Result := s;
end;

function BeforeDot(const S: string): string;
var
  DotPos: PChar;
begin
  DotPos := StrScan(PChar(S), '.');
  if Assigned(DotPos) then
    SetString(Result, PChar(S), DotPos - PChar(S))
  else
    Result := S;
end;

procedure DiffLists(Source1, Source2, InBoth, NotInSource1, NotInSource2: TStrings;
  const CaseSensitive: Boolean);
var
  Index1, Index2: Integer;
  C: Integer;
begin
  if not Assigned(Source1) or not Assigned(Source2) then
    Exit;

  Index1 := 0;
  Index2 := 0;
  while (Index1 < Source1.Count) and (Index2 < Source2.Count) do
  begin
    if CaseSensitive then
      C := CompareStr(Source1[Index1], Source2[Index2])
    else
      C := AnsiCompareText(Source1[Index1], Source2[Index2]);
    if C = 0 then
    begin
      if Assigned(InBoth) then
        InBoth.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
      Inc(Index2);
    end
    else
      if C < 0 then
    begin
      if Assigned(NotInSource2) then
        NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1)
    end
    else
      if C > 0 then
    begin
      if Assigned(NotInSource1) then
        NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  end;

  if Assigned(NotInSource1) then
    while Index2 < Source2.Count do
    begin
      NotInSource1.AddObject(Source2[Index2], Source2.Objects[Index2]);
      Inc(Index2);
    end;
  if Assigned(NotInSource2) then
    while Index1 < Source1.Count do
    begin
      NotInSource2.AddObject(Source1[Index1], Source1.Objects[Index1]);
      Inc(Index1);
    end;
end;

procedure EatChar(var S: string; const Chars: TSysCharSet);
begin
  if (Length(S) >= 1) and (S[1] in Chars) then
    Delete(S, 1, 1);
end;

procedure EatChars(var S: string; const Chars: TSysCharSet);
begin
  while (Length(S) >= 1) and (S[1] in Chars) do
    Delete(S, 1, 1);
end;

procedure EnsureMinCount(SS: TList; const MinCount: Integer); overload;
begin
  while SS.Count < MinCount do
    SS.Add(nil);
end;

procedure EnsureMinCount(SS: TStrings; const MinCount: Integer); overload;
begin
  while SS.Count < MinCount do
    SS.Add('');
end;

procedure ErrorMsg(const Msg: string);
begin
  if Assigned(GProgress) then
    GProgress.ErrorMsg(Msg);
end;

procedure ErrorMsgFmt(const Msg: string; const Args: array of const);
begin
  ErrorMsg(Format(Msg, Args));
end;

procedure GetFirstToken(const Source: string; out Token: string; out RestIndex: Integer);
const
  cSpecialToken1 = 'See';
  cSpecialToken2 = 'Return';
  cSpecialToken1Length = Length(cSpecialToken1);
  cSpecialToken2Length = Length(cSpecialToken2);
var
  P, Q: PChar;
begin
  Q := PChar(Source);
  while Q^ in [SpaceChar, TabChar] do
    Inc(Q);
  P := Q;
  if P^ <> #0 then
    Inc(P);
  while not (P^ in [SpaceChar, TabChar, '=', #0]) do
    Inc(P);
  // special case "See Also", "Return Value"
  if (P^ = ' ') and
    (
    ((P - Q = cSpecialToken1Length) and (StrLIComp(Q, cSpecialToken1, cSpecialToken1Length) = 0)) or
    ((P - Q = cSpecialToken2Length) and (StrLIComp(Q, cSpecialToken2, cSpecialToken2Length) = 0))
    ) then
  begin
    Inc(P);
    while not (P^ in [SpaceChar, TabChar, '=', #0]) do
      Inc(P);
  end;
  SetString(Token, Q, P - Q);
  RestIndex := P - PChar(Source) + 1;
end;

function HasDuplicates(Strings: TStrings): Boolean;
var
  SS: TStringList;
  I: Integer;
begin
  if not (Strings is TStringList) or not TStringList(Strings).Sorted then
  begin
    SS := TStringList.Create;
    try
      SS.Sorted := True;
      SS.AddStrings(Strings);

      Result := HasDuplicates(SS);
    finally
      SS.Free;
    end;

    Exit;
  end;

  for I := 0 to Strings.Count - 2 do
    if SameText(Strings[i], STrings[i + 1]) then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

procedure HintMsg(const Msg: string);
begin
  if Assigned(GProgress) then
    GProgress.HintMsg(Msg);
end;

procedure HintMsgFmt(const Msg: string; const Args: array of const);
begin
  HintMsg(Format(Msg, Args));
end;

procedure ErrorHeaderFmt(const Msg: string; const Args: array of const);
begin
  ErrorHeader(Format(Msg, Args));
end;

procedure ErrorHeader(const Msg: string);
begin
  if Assigned(GProgress) then
    GProgress.ErrorHeader(Msg);
end;

function IsNullStr(const S: string): Boolean;
const
  cWhiteSpace = [SpaceChar, TabChar];
var
  P: PChar;
begin
  P := PChar(S);
  while P^ in cWhiteSpace do
    Inc(P);
  Result := P^ = #0;
end;

function IsParam(const S: string): Boolean; overload;
var
  P: PChar;
begin
  P := PChar(S);
  while P^ in [SpaceChar, TabChar] do
    Inc(P);
  if P^ <> #0 then
    Inc(P);
  while not (P^ in (cParamSeparators + [SpaceChar, TabChar, #0])) do
    Inc(P);
  // special case "See Also", "Return Value"
  while P^ in [SpaceChar, TabChar] do
    Inc(P);
  Result := P^ in cParamSeparators;
end;

function IsParam(const S: string; out Token: string; out RestIndex: Integer): Boolean; overload;
const
  cMaxIndent = 4;
var
  P, Q: PChar;
begin
  RestIndex := -1;
  Q := PChar(S);
  P := Q;
  while P^ in [SpaceChar, TabChar] do
    Inc(P);
  Result := P - Q < cMaxIndent;
  if not Result then
    Exit;
  if P^ <> #0 then
    Inc(P);
  while not (P^ in (cParamSeparators + [SpaceChar, TabChar, #0])) do
    Inc(P);
  // special case "See Also", "Return Value"
  SetString(Token, Q, P - Q);
  while P^ in [SpaceChar, TabChar] do
    Inc(P);
  Result := P^ in cParamSeparators;
  if not Result then
    Exit;

  Inc(P);
  while P^ in [SpaceChar, TabCHar] do
    Inc(P);
  RestIndex := P - PChar(S) + 1;
end;

function LinkStr(const S: string): string;
var
  P, Q: PChar;
  ShortStr: string;
begin
  P := StrScan(PChar(S), '@');
  if P = nil then
    Result := Format('<LINK %s>', [S])
  else
  begin
    Q := PChar(S);
    SetString(ShortStr, Q, P - Q);
    Result := Format('<LINK %s, %s>', [S, ShortStr]);
  end;
end;

function MaxWordLengthInString(const S: string): Integer;
const
  cStartBracket = '<';
  cEndBracket = '>';
  cSplitters = [cStartBracket, cEndBracket];
  cWhiteSpace = [#13, #10, #8, SpaceChar] + cSplitters;
  // allowed in table
  //   <B> </B>   : bold
  //   <I> </I>   : italic
  //   <U> </U>   : underlined
  //   <P>        : paragraph
  //   <C> </C>   : code
  //   <IMAGE xx>
  //   <LINK xx>
  //   <LINK xx,yy>
  //   <EXTLINK xx>text</EXTLINK>
var
  P, Q: PChar;
begin
  P := PChar(S);
  Result := 0;
  while P^ <> #0 do
  begin
    while (P^ <> #0) and (P^ <> cStartBracket) and (P^ in cWhiteSpace) do
      Inc(P);
    Q := P;
    if P^ = cStartBracket then
    begin
      // goto , or >
      while (P^ <> #0) and (P^ <> cEndBracket) and (P^ <> ',') do
        Inc(P);
    end
    else
    begin
      while (P^ <> #0) and not (P^ in cWhiteSpace) and not (P^ in cSplitters) do
        Inc(P);
    end;
    Result := Max(Result, P - Q);
  end;
end;

function Pad(const S: string; const Width: Integer; const PadChar: Char = SpaceChar): string;
begin
  Result := S + StringOfChar(PadChar, Width - Length(S));
end;

function StartWith2Spaces(const S: string): Boolean;
var
  P: PChar;
begin
  P := PChar(S);
  Result := P^ in [SpaceChar, TabChar, #0];
end;

procedure StatusMsg(const Msg: string);
begin
  if Assigned(GProgress) then
    GProgress.StatusMsg(Msg);
end;

procedure StatusMsgFmt(const Msg: string; const Args: array of const);
begin
  StatusMsg(Format(Msg, Args));
end;

procedure WarningMsg(const Msg: string);
begin
  if Assigned(GProgress) then
    GProgress.WarningMsg(Msg);
end;

procedure WarningMsgFmt(const Msg: string; const Args: array of const);
begin
  WarningMsg(Format(Msg, Args));
end;

procedure Wrap(var S: string; const Width, Indent, NextIndent: Integer);
var
  SL: TStrings;
  ThisWidth: Integer;
  ThisIndent: Integer;
  I: Integer;
  J: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Text := S;
    ThisWidth := Width - Indent;
    ThisIndent := Indent;
    I := 0;
    while I < SL.Count do
    begin
      SL[I] := Trim(SL[I]);
      if Length(SL[I]) > ThisWidth then
      begin
        J := ThisWidth;
        while (J > 0) and (SL[I][J] > SpaceChar) do
          Dec(J);
        if J <= 0 then
        begin
          J := ThisWidth;
          if J < 0 then
            J := 1;
          while (J <= Length(SL[I])) and (SL[I][J] > SpaceChar) do
            Inc(J);
        end;
      end
      else
        J := Length(SL[I]) + 1;
      if J < Length(SL[I]) then // Split the line
      begin
        SL.Insert(I + 1, Copy(SL[I], J + 1, Length(SL[I]) - J));
        SL[I] := Trim(Copy(SL[I], 1, J - 1));
      end;
      SL[I] := StringOfChar(SpaceChar, ThisIndent) + SL[I];
      Inc(I);
      if I = 1 then
      begin
        Inc(ThisIndent, NextIndent);
        Dec(ThisWidth, NextIndent);
      end;
    end;
    S := SL.Text;
    Delete(S, Length(S) - 1, 2);
  finally
    SL.Free;
  end;
end;

//=== { TIntegerList } =======================================================

function TIntegerList.Add(AInteger: Integer): Integer;
begin
  Result := inherited Add(Pointer(AInteger));
end;

function TIntegerList.Contains(AInteger: Integer): Boolean;
begin
  Result := IndexOf(Pointer(AInteger)) >= 0;
end;

function TIntegerList.GetItems(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntegerList.SetItems(Index: Integer; const Value: Integer);
begin
  inherited Items[Index] := Pointer(Value);
end;

procedure TIntegerList.Sort;
begin
  inherited Sort(IntegerSortCompare);
end;

//=== { TOwnedStringList } ===================================================

procedure TOwnedStringList.Clear;
begin
  while Count > 0 do
    Delete(Count - 1);
  inherited Clear;
end;

procedure TOwnedStringList.Delete(Index: Integer);
begin
  if Objects[Index] <> nil then
    Objects[Index].Free;
  inherited Delete(Index);
end;

//=== { TRegisteredComponents } ==============================================

constructor TRegisteredComponents.Create(const ARegisterComponentsFileName,
  AGroupListFileName: string);
begin
  inherited Create;
  FRegisteredComponentsFileName := ARegisterComponentsFileName;
  FGroupListFileName := AGroupListFileName;
  FRegisteredComponents := TStringList.Create;
  FRegisteredComponents.CaseSensitive := False;
  FRegisteredComponents.Duplicates := dupIgnore;
  FRegisteredComponents.Sorted := True;
  FGroupList := TStringList.Create;
end;

destructor TRegisteredComponents.Destroy;
begin
  FRegisteredComponents.Free;
  FGroupList.Free;
  inherited Destroy;
end;

function TRegisteredComponents.GroupStr(
  const AComponentName: string): string;
var
  Index: Integer;
begin
  Result := '';
  Index := FRegisteredComponents.IndexOf(AComponentName);
  if Index >= 0 then
    Result := FGroupList[Index];
  if Result = '' then
    Result := cUnknownGroupStr;
end;

function TRegisteredComponents.IsRegisteredComponent(
  const AComponentName: string): Boolean;
begin
  Result := FRegisteredComponents.IndexOf(AComponentName) >= 0;
end;

function TRegisteredComponents.Parse: Boolean;
var
  SS: TStringLIst;
  I: Integer;
  Index: Integer;
begin
  Result := FileExists(FRegisteredComponentsFileName) and
    FileExists(FGroupListFileName);
  if not Result then
    Exit;

  StatusMsg('Loading registered components file');

  FRegisteredComponents.LoadFromFile(FRegisteredComponentsFileName);
  while FGroupList.Count < FRegisteredComponents.Count do
    FGroupList.Add('');

  SS := TStringList.Create;
  try
    SS.LoadFromFile(FGroupListFileName);
    for I := 0 to SS.Count - 1 do
    begin
      Index := FRegisteredComponents.IndexOf(Trim(SS.Names[i]));
      if Index >= 0 then
        FGroupList[Index] := Trim(SS.ValueFromIndex[i]);
    end;

    StatusMsg('Done');

  finally
    SS.Free;
  end;
end;

//=== { TSimpleDtxComment } ==================================================

procedure TSimpleDtxComment.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.WriteElementContent('COMMENT', Value);
end;

//=== { TSimpleDtxList } =====================================================

constructor TSimpleDtxList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TSimpleDtxList.Destroy;
begin
  FItems.Free;
  FProgress := nil;
  inherited Destroy;
end;

procedure TSimpleDtxList.Add(AItem: TDtxBaseItem);
begin
  FItems.Add(AItem);
end;

function TSimpleDtxList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSimpleDtxList.GetItems(const Index: Integer): TDtxBaseItem;
begin
  Result := FItems[Index] as TDtxBaseItem;
end;

function TSimpleDtxList.LoadFromFile(const AFileName: string): Boolean;
begin
  FLines := TStringList.Create;
  try
    FLines.LoadFromFile(AFileName);

    Result := ParseLines;
  finally
    FLines.Free;
  end;
end;

function TSimpleDtxList.ParseLines: Boolean;
var
  Value: string;
begin
  FIndex := 0;
  Result := True;
  while Result and (FIndex < FLines.Count) do
  begin
    //    ProgressMsg(FIndex, FLines.Count);
    if IsAtAt(FLines[FIndex], Value) then
      Result := ReadTopicName(Value)
    else
      if IsComment(FLines[FIndex], Value) then
      Result := ReadComment(Value)
    else
      Inc(FIndex);
  end;
end;

function TSimpleDtxList.ReadComment(const AComment: string): Boolean;
var
  DtxComment: TSimpleDtxComment;
begin
  DtxComment := TSimpleDtxComment.Create;
  DtxComment.Value := AComment;
  Add(DtxComment);

  Inc(FIndex);
  Result := True;
end;

function TSimpleDtxList.ReadTopicName(const ATopicName: string): Boolean;
var
  Value: string;
  DtxTopic: TSimpleDtxTopic;
  CurrentTopicToken: TTopicToken;
  TopicToken: TTopicToken;
  Token: string;
  RestIndex: Integer;
  CurrentLine: string;
begin
  Result := True;
  Inc(FIndex);

  DtxTopic := TSimpleDtxTopic.Create;
  DtxTopic.Name := ATopicName;
  Add(DtxTopic);

  CurrentTopicToken := tkSummary;

  while (FIndex < FLines.Count) and not IsAtAt(FLines[FIndex]) do
  begin
    CurrentLine := FLines[FIndex];

    if IsComment(CurrentLine, Value) then
      Result := ReadComment(Value)
    else
    if IsSeparator(CurrentLine) then
      Inc(FIndex)
    else
    begin
      GetFirstToken(CurrentLine, Token, RestIndex);
      TopicToken := TokenToTopicToken(Token, IsRestNull(CurrentLine, RestIndex));
      case TopicToken of
        tkSummary, tkNote, tkDescription, tkParameters, tkSeeAlso, tkReturnValue, tkDonator, tkPlatform:
          begin
            CurrentTopicToken := TopicToken;
            Delete(CurrentLine, 1, RestIndex - 1);
            DtxTopic.AddLineSkipEmpty(CurrentTopicToken, CurrentLine);
          end;
        tkJVCLInfo, tkAuthor:
          CurrentTopicToken := TopicToken;
        tkGroup, tkFlag:
          if CurrentTopicToken = tkJVCLInfo then
          begin
            Delete(CurrentLine, 1, RestIndex - 1);
            EatChars(CurrentLine, [SpaceChar, TabChar]);
            EatChar(CurrentLine, ['=']);
            EatChars(CurrentLine, [SpaceChar, TabChar]);
            DtxTopic.AddLine(TopicToken, CurrentLine)
          end
          else
            // add complete line
            DtxTopic.AddLine(CurrentTopicToken, CurrentLine);
        {tkInclude,}tkAlias, tkAliasOf, tkCombine, tkCombineWith,
        tkTitleImg, tkTitle, xtkHasTocEntry, tkHasPasFileEntry, tkGroup2, tkTopicOrder:
          begin
            Delete(CurrentLine, 1, RestIndex - 1);
            FLines[FIndex] := CurrentLine;

            ReadUntilEndElement(DtxTopic, TopicToken);
            Continue;
          end;
      else
        DtxTopic.AddLine(CurrentTopicToken, CurrentLine);
      end;
      Inc(FIndex);
    end;
  end;
end;

procedure TSimpleDtxList.ReadUntilEndElement(ADtxTopic: TSimpleDtxTopic; const ATopicToken: TTopicToken);
var
  CurrentLine: string;
  I: Integer;
begin
  while FIndex < FLines.Count do
  begin
    CurrentLine := FLines[FIndex];
    I := 1;
    while I <= Length(CurrentLine) do
      case CurrentLine[i] of
        '\': Inc(I, 2);
        '>': Break;
      else
        Inc(I);
      end;

    if (I <= Length(CurrentLine)) and (CurrentLine[i] = '>') then
    begin
      ADtxTopic.AddLine(ATopicToken, Copy(CurrentLine, 1, I - 1));
      Delete(CurrentLine, 1, I);
      FLines[FIndex] := CurrentLine;
      Break;
    end
    else
    begin
      ADtxTopic.AddLine(ATopicToken, CurrentLine);
      Inc(FIndex);
    end;
  end;
end;

procedure TSimpleDtxList.SaveToFile(const AFileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TSimpleDtxList.SaveToStream(AStream: TStream);
var
  I: Integer;
  XMLWriter: TXMLWriter;
begin
  XMLWriter := TXMLWriter.Create;
  try
    XMLWriter.DestStream := AStream;
    XMLWriter.WriteIndentData('<?xml version="1.0" encoding="iso-8859-1"?>');
    XMLWriter.BeginElement('DTX');
    for I := 0 to FItems.Count - 1 do
      TDtxBaseItem(FItems[i]).Save(XMLWriter);
    XMLWriter.EndElement;
  finally
    XMLWriter.Free;
  end;
end;

//=== { TSimpleDtxTopic } ====================================================

procedure TSimpleDtxTopic.AddLine(const ATopicToken: TTopicToken;
  const S: string);
begin
  if FValues[ATopicToken] = '' then
    FValues[ATopicToken] := S
  else
    FValues[ATopicToken] := FValues[ATopicToken] + #13#10 + S;
end;

procedure TSimpleDtxTopic.AddLineSkipEmpty(const ATopicToken: TTopicToken;
  const S: string);
begin
  if not IsNullStr(S) then
    AddLine(ATopicToken, S);
end;

function TSimpleDtxTopic.GetIsPasTopic: Boolean;
begin
  // Name = xxx.pas
  Result := (Length(Name) > 4) and
    SameText(Copy(Name, Length(Name) - 3, 4), '.pas');
end;

function TSimpleDtxTopic.GetString(const Index: TTopicToken): string;
begin
  Result := FValues[Index];
end;

procedure TSimpleDtxTopic.Save(AXMLWriter: TXMLWriter);
begin
  AXMLWriter.BeginElement('TOPIC');
  if not IsNullStr(Name) then
    AXMLWriter.WriteElementContent('NAME', Name);
  if not IsNullStr(Title) then
    AXMLWriter.WriteElementContent('TITLE', Title);
  if not IsNullStr(TitleImg) then
    AXMLWriter.WriteElementContent('TITLEIMG', TitleImg);
  if not IsNullStr(Combine) then
    AXMLWriter.WriteElementContent('COMBINE', Combine);
  if not IsNullStr(CombineWith) then
    AXMLWriter.WriteElementContent('COMBINEWITH', CombineWith);
  if not IsNullStr(Summary) then
    AXMLWriter.WriteElementContent('SUMMARY', Summary);
  if not IsNullStr(Description) then
    AXMLWriter.WriteElementContent('DESCRIPTION', Description);
  if not IsNullStr(Parameters) then
    AXMLWriter.WriteElementContent('PARAMETERS', Parameters);
  if not IsNullStr(ReturnValue) then
    AXMLWriter.WriteElementContent('RETURNVALUE', ReturnValue);
  if not IsNullStr(Note) then
    AXMLWriter.WriteElementContent('NOTE', Note);
  if not IsNullStr(SeeAlso) then
    AXMLWriter.WriteElementContent('SEEALSO', SeeAlso);
  if not IsNullStr(Donator) then
    AXMLWriter.WriteElementContent('Donator', Donator);
  if not IsNullStr(Group) then
    AXMLWriter.WriteElementContent('GROUP', Group);
  if not IsNullStr(Flag) then
    AXMLWriter.WriteElementContent('FLAG', Flag);
  //  if not IsNullStr(Include) then
  //    AXMLWriter.WriteElementContent('INCLUDE', Include);
  if not IsNullStr(Author) then
    AXMLWriter.WriteElementContent('AUTHOR', Author);
  if not IsNullStr(Alias) then
    AXMLWriter.WriteElementContent('ALIAS', Alias);
  if not IsNullStr(AliasOf) then
    AXMLWriter.WriteElementContent('ALIASOF', AliasOf);

  AXMLWriter.EndElement;
end;

procedure TSimpleDtxTopic.SetString(const Index: TTopicToken; const Value: string);
begin
  FValues[Index] := Value;
end;

//=== { TStringObjectList } ==================================================

destructor TStringObjectList.Destroy;
begin
  ClearObjects;
  inherited Destroy;
end;

procedure TStringObjectList.Clear;
begin
  ClearObjects;
  inherited Clear;
end;

procedure TStringObjectList.ClearObjects;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[i] := nil;
end;

procedure TStringObjectList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  Obj := Objects[Index];
  inherited Delete(Index);
  Obj.Free;
end;

procedure TStringObjectList.PutObject(Index: Integer; AObject: TObject);
var
  Obj: TObject;
begin
  Obj := Objects[Index];
  inherited PutObject(Index, AObject);
  if Obj <> AObject then
    Obj.Free;
end;

//=== { TStringStack } =======================================================

constructor TStringStack.Create;
begin
  inherited Create;
  FStrings := TStringList.Create;
end;

destructor TStringStack.Destroy;
begin
  Strings.Free;
  inherited Destroy;
end;

function TStringStack.AtLeast(ACount: integer): boolean;
begin
  Result := Strings.Count >= ACount;
end;

function TStringStack.Count: Integer;
begin
  Result := Strings.Count;
end;

function TStringStack.Peek: string;
begin
  Result := PeekItem;
end;

function TStringStack.PeekItem: string;
begin
  Result := Strings[Strings.Count - 1];
end;

function TStringStack.Pop: string;
begin
  Result := PopItem;
end;

function TStringStack.PopItem: string;
begin
  Result := PeekItem;
  Strings.Delete(Strings.Count - 1);
end;

function TStringStack.Push(const AItem: string): string;
begin
  PushItem(AItem);
  Result := AItem;
end;

procedure TStringStack.PushItem(const AItem: string);
begin
  Strings.Add(AItem);
end;

//=== { TXMLWriter } =========================================================

constructor TXMLWriter.Create;
begin
  inherited Create;
  FStack := TStringStack.Create;
end;

destructor TXMLWriter.Destroy;
begin
  FStack.Free;
  inherited Destroy;
end;

procedure TXMLWriter.BeginElement(const AElement: string);
begin
  BeginElement(AElement, [], []);
end;

procedure TXMLWriter.BeginElement(const AElement: string;
  const AAttributes, AAttributeValues: array of string);
begin
  WriteStartElement(FStack.Push(AElement), AAttributes, AAttributeValues);
  Inc(FIdent);
end;

procedure TXMLWriter.EndElement;
begin
  Dec(FIdent);
  WriteCloseElement(FStack.Pop);
end;

procedure TXMLWriter.WriteCloseElement(const AElement: string);
begin
  WriteIndentData(Format('</%s>', [AElement]));
end;

procedure TXMLWriter.WriteContent(const AContent: string);
begin
  WriteIndentData(EntityEncode(AContent));
end;

procedure TXMLWriter.WriteElementAttribute(const AElement, AAttribute,
  AAttributeValue: string);
begin
  WriteIndentData(Format('<%s %s="%s" />', [AElement, AAttribute, AAttributeValue]))
end;

procedure TXMLWriter.WriteElementAttributeContent(const AElement: string;
  const AAttributes, AAttributeValues: array of string;
  const AContent: string);
begin
  BeginElement(AElement, AAttributes, AAttributeValues);
  WriteContent(AContent);
  EndElement;
end;

procedure TXMLWriter.WriteElementContent(const AElement, AContent: string);
begin
  BeginElement(AElement);
  WriteContent(AContent);
  EndElement;
end;

procedure TXMLWriter.WriteElementContentNE(const AElement, AContent: string);
begin
  if not IsNullStr(AContent) then
    WriteElementContent(AElement, AContent);
end;

procedure TXMLWriter.WriteEmptyElement(const AElement: string);
begin
  WriteIndentData(Format('<%s />', [AElement]));
end;

procedure TXMLWriter.WriteIndentData(const Data: string);
var
  S: string;
begin
  S := StringOfChar(SpaceChar, FIdent * 2) + Data + #13#10;
  DestStream.Write(PChar(S)^, Length(S));
end;

procedure TXMLWriter.WriteStartElement(const AElement: string;
  const AAttributes, AAttributeValues: array of string);
var
  I: Integer;
  AttrStr: string;
begin
  if High(AAttributes) <> High(AAttributeValues) then
    raise Exception.Create('WriteStartElement');
  AttrStr := '';
  for I := Low(AAttributes) to High(AAttributes) do
    AttrStr := AttrStr + Format(' %s="%s"', [AAttributes[i], AAttributeValues[i]]);

  WriteIndentData(Format('<%s%s>', [AElement, AttrStr]));
end;

function PackageToTopicName(const S: string): string;
begin
  Result := Format('$JVCL.Packages.%s', [S]);
end;

{ TTask }

function TTask.CanStart: Boolean;
begin
  Result := True;
end;

constructor TTask.Create(ATaskManager: ITaskManager);
begin
  inherited Create;
  FTaskManager := ATaskManager;
end;

procedure TTask.Done;
begin
  TaskManager.TaskDone;
end;

function TTask.Execute: Boolean;
begin
  Result := CanStart;
  if not Result then
  begin
    ErrorMsgFmt('Could not initialize task %s', [QuotedStr(TaskDescription)]);
    Exit;
  end;

  TaskManager.TaskStatus(TaskDescription);

  Result := DoExecute;
  if Result then
    Done;
end;

procedure TTask.Progress(const Position, Count: Integer);
begin
  TaskManager.TaskProgress(Position, Count);
end;

function StripOverloadAt(const S: string): string;
var
  P, Q: PChar;
begin
  Q := PChar(S);
  P := StrScan(Q, '@');
  if P = nil then
    Result := S
  else
    SetString(Result, Q, P - Q);
end;

function HasAt(const S: string): Boolean;
begin
  Result := StrScan(PChar(S), '@') <> nil;
end;

function LocateString(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
begin
  Result := StartIndex;
  while (Result < Strings.Count) and not SameText(S, Strings[Result]) do
    Inc(Result);
  if Result >= Strings.Count then
    Result := -1;
end;

function IsPrefix(const Prefix, S: string): Boolean;
begin
  Result := (Length(S) >= Length(Prefix)) and
    (StrLIComp(PChar(Prefix), PChar(S), Length(Prefix)) = 0);
end;

function IsPostfix(const Postfix, S: string): Boolean;
begin
  //  p='ab', S := 'cccab'
  //       2         5
  Result := (Length(S) >= Length(Postfix)) and
    (StrLIComp(PChar(Postfix), PChar(S) + Length(S) - Length(Postfix), Length(Postfix)) = 0);
end;

function IsSubString(const SubString, S: string): Boolean;
begin
  Result := (Length(S) >= Length(SubString)) and
    (Pos(SubString, LowerCase(S)) >= 1);
end;

function LocateSubString(Strings: TStrings; const StartIndex: Integer; SubStr: string): Integer;
begin
  SubStr := LowerCase(SubStr);

  Result := StartIndex;
  while (Result < Strings.Count) and not IsSubString(SubStr, Strings[Result]) do
    Inc(Result);
  if Result >= Strings.Count then
    Result := -1;
end;

function LocatePrefix(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
begin
  Result := StartIndex;
  while (Result < Strings.Count) and not IsPrefix(S, Strings[Result]) do
    Inc(Result);
  if Result >= Strings.Count then
    Result := -1;
end;

function LocatePostfix(Strings: TStrings; const StartIndex: Integer; const S: string): Integer;
begin
  Result := StartIndex;
  while (Result < Strings.Count) and not IsPostfix(S, Strings[Result]) do
    Inc(Result);
  if Result >= Strings.Count then
    Result := -1;
end;

function ChangeSubString(const S, OldStr, NewStr: string): string;
var
  Index: Integer;
begin
  Result := S;

  Index := StrIPos(OldStr, Result);
  if Index >= 1 then
  begin
    Delete(Result, Index, Length(OldStr));
    Insert(NewStr, Result, Index);
  end;
end;

function RegisteredClassNameToImageFileName(const ARegisteredClassName: string): string;
begin
  Result := 'TITLEIMG_' + ARegisteredClassName;
end;

function HasDot(const S: string): Boolean;
begin
  Result := StrScan(PChar(S), '.') <> nil;
end;

{ TProgress }

function TProgress.GetAllTaskMax: Integer;
begin
  Result := CurrentTaskMax * TaskCount;
end;

function TProgress.GetAllTaskPosition: Integer;
begin
  Result := TaskIndex * CurrentTaskMax + CurrentTaskPosition;
end;

procedure TProgress.NextTask;
begin
  if TaskIndex < TaskCount then
    Inc(FTaskIndex);
  CurrentTaskMax := 1;
  CurrentTaskPosition := 0;
end;

procedure TProgress.SetCurrentTaskMax(const Value: Integer);
begin
  if Value <> FCurrentTaskMax then
  begin
    FCurrentTaskMax := Max(1, Value);
    CurrentTaskPosition := Min(CurrentTaskMax, CurrentTaskPosition);
  end;
end;

procedure TProgress.SetCurrentTaskPosition(const Value: Integer);
begin
  if Value <> FCurrentTaskPosition then
  begin
    CurrentTaskMax := Max(CurrentTaskMax, Value);
    FCurrentTaskPosition := Max(0, Value);
  end;
end;

procedure TProgress.SetTaskCount(const Value: Integer);
begin
  FTaskCount := Max(Value, 0);
  FTaskIndex := Min(TaskIndex, TaskCount);
end;

procedure TProgress.Start;
begin
  FTaskIndex := 0;
end;

{ TGUIDList }

function TGUIDList.Add(const AGUID: TGUID): Integer;
begin
  Result := AddObject(AGUID, nil);
end;

function TGUIDList.AddObject(const AGUID: TGUID;
  AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(AGUID, Result) then
    case Duplicates of
      dupIgnore: Exit;
      dupError: Error(@SDuplicateString, 0);
    end;
  InsertItem(Result, AGUID, AObject);
end;

procedure TGUIDList.Clear;
begin
  if FCount <> 0 then
  begin
    while Count > 0 do
      Delete(Count - 1);
    //    FCount := 0;
    SetCapacity(0);
  end;
end;

function CompareInt(const A1, A2: Integer): Integer;
begin
  if A1 < A2 then Result := -1 else
  if A1 > A2 then Result := +1 else
  Result := 0;
end;

constructor TGUIDList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

function CompareGUIDS(const AGUID1, AGUID2: TGUID): Integer;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@Aguid1);
  b := PIntegerArray(@Aguid2);
  Result := CompareInt(a^[0] , b^[0]);
  if Result = 0 then
    Exit;

  Result := CompareInt(a^[1] , b^[1]);
  if Result = 0 then
    Exit;

  Result := CompareInt(a^[2] , b^[2]);
  if Result = 0 then
    Exit;

  Result := CompareInt(a^[3] , b^[3]);
end;

procedure TGUIDList.CustomSort(Compare: TGUIDListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    QuickSort(0, FCount - 1, Compare);
  end;
end;

procedure TGUIDList.Delete(Index: Integer);
var
  Temp: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := Objects[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TGuidItem));
  if OwnsObjects then
    Temp.Free;
end;

destructor TGUIDList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TGUIDList.Error(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg), Data);
end;

procedure TGUIDList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EStringListError.CreateFmt(Msg, [Data])at ReturnAddr;
end;

procedure TGUIDList.ExchangeItems(Index1, Index2: Integer);
var
  TempGuid: TGUID;
  TempObj: TObject;
  Item1, Item2: PGuidItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  TempGuid := Item1^.FGuid;
  Item1^.FGuid := Item2^.FGuid;
  Item2^.FGuid := TempGuid;
  TempObj := Item1^.FObject;
  Item1^.FObject := Item2^.FObject;
  Item2^.FObject := TempObj;
end;

function TGUIDList.Find(const AGUID: TGUID; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareGUIDS(FList^[I].FGUID, AGUID);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end;

function TGUIDList.Get(Index: Integer): TGUID;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index].FGUID;
end;

function TGUIDList.GetCount: Integer;
begin
  Result := FCount;

end;

function TGUIDList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index].FObject;
end;

procedure TGUIDList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TGUIDList.IndexOf(const AGUID: TGUID): Integer;
begin
  if not Sorted then
  begin
    for Result := 0 to GetCount - 1 do
      if CompareGUIDS(Get(Result), AGUID) = 0 then
        Exit;
    Result := -1;
  end
  else
    if not Find(AGUID, Result) then
    Result := -1;
end;

procedure TGUIDList.InsertItem(Index: Integer; const AGUID: TGUID;
  AObject: TObject);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TGuidItem));
  with FList^[Index] do
  begin
    FObject := AObject;
    FGUID := AGUID;
  end;
  Inc(FCount);
end;

procedure TGUIDList.Put(Index: Integer; const AGUID: TGUID);
begin
  if Sorted then
    Error(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  FList^[Index].FGUID := AGUID;
end;

procedure TGUIDList.PutObject(Index: Integer; AObject: TObject);
var
  Temp: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if AObject <> FList^[Index].FObject then
  begin
    Temp := FList^[Index].FObject;
    FList^[Index].FObject := AObject;
    if OwnsObjects then
      Temp.Free;
  end;
end;

procedure TGUIDList.QuickSort(L, R: Integer;
  SCompare: TGUIDListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do
        Inc(I);
      while SCompare(Self, J, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else
          if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

function TGUIDList.Remove(const AGUID: TGUID): Integer;
begin
  Result := IndexOf(AGUID);
  if Result >= 0 then
    Delete(Result);
end;

procedure TGUIDList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TGuidItem));
  FCapacity := NewCapacity;
end;

procedure TGUIDList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;

function GuidListCompareGuids(List: TGuidList; Index1, Index2: Integer): Integer;
begin
  Result := CompareGuids(List.FList^[Index1].FGuid,
    List.FList^[Index2].FGuid);
end;

procedure TGUIDList.Sort;
begin
  CustomSort(GuidListCompareGuids);
end;

end.



