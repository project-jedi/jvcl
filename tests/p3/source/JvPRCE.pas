unit JvPRCE;

interface
uses
  Windows, Classes, SysUtils;

type
  EPCREError = class(Exception)
  private
    FErrorCode: integer;
  public
    constructor Create(const Msg: string; ErrorCode: integer);
    property ErrorCode: integer read FErrorCode;
  end;

  TPCREIntArray = array[0..2999] of integer; // 1000 subpatterns should be enough...
  PPCREIntArray = ^TPCREIntArray;

  TRegExOption = (roIgnoreCase, roMultiLine, roDotAll, roExtended,
    roAnchored, roDollarEndOnly, roExtra, roNotBOL, roNotEOL, roUnGreedy, roNotEmpty, roUTF8);
  TRegExOptions = set of TRegExOption;
  TCaptureOffset = record
    FirstPos, LastPos: integer;
  end;

  TRegEx = class
  private
    FCode: Pointer;
    FExtra: Pointer;
    FOptions: TRegExOptions;
    FSubject, FErrorMessage: string;
    FErrorOffset: integer;
    FVector: TPCREIntArray;
    FStringCount,FVectorSize: integer;
    FTables:PChar;
    function GetCaptureCount: integer;
    function GetCaptures(Index: integer): string;
    function GetAPIOptions(RunTime: boolean): integer;
    function GetCapturesOffset(Index: integer): TCaptureOffset;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const Pattern: string; Study, UserLocale: boolean): boolean;
    function Match(const Subject: string; StartOffset: Cardinal = 1): boolean;
    property Options: TRegExOptions read FOptions write FOptions;
    property CaptureCount: integer read GetCaptureCount;
    property Captures[Index: integer]: string read GetCaptures;
    property CaptureOffset[Index: integer]: TCaptureOffset read GetCapturesOffset;
    property ErrorMessage: string read FErrorMessage;
    property ErrorOffset: integer read FErrorOffset;
  end;

implementation
uses
  pcre;


function PCRECheck(Value: integer): boolean;
var S: string;
begin
  Result := false;
  case Value of
    PCRE_ERROR_NOMATCH: S := 'No match';
    PCRE_ERROR_NULL: S := 'Required value is null';
    PCRE_ERROR_BADOPTION: S := 'Bad option';
    PCRE_ERROR_BADMAGIC: S := 'Bad magic';
    PCRE_ERROR_UNKNOWN_NODE: S := 'Unknown node';
    PCRE_ERROR_NOMEMORY: S := 'Out of memory';
    PCRE_ERROR_NOSUBSTRING: S := 'No substring';
  else
    Result := true;
  end;
  if not Result then
    raise EPCREError.Create(S, Value);
end;

{ TRegEx }

function TRegEx.Compile(const Pattern: string; Study,UserLocale: boolean): boolean;
var errptr: PChar; erroffset: Integer;
begin
  if UserLocale then
    FTables := pcre_maketables
  else
    FTables := nil;
  FCode := pcre_compile(PChar(Pattern), GetAPIOptions(false), @errptr, @erroffset, FTables);
  FErrorMessage := errptr;
  FErrorOffset := erroffset;
  Result := (FCode <> nil);
  if Result and Study then
    FExtra := pcre_study(FCode, 0, @errptr);
end;

constructor TRegEx.Create;
begin
  inherited Create;
  FVectorSize := sizeof(FVector) div sizeof(integer);
end;

destructor TRegEx.Destroy;
begin
(*
  if FCode <> nil then
    pcre_free(FCode);
  if FExtra <> nil then
    pcre_free(FExtra);
*)
  inherited;
end;

function TRegEx.GetAPIOptions(RunTime: boolean): integer;
const
  cDesignOptions: array[TRegExOption] of integer =
  (PCRE_CASELESS, PCRE_MULTILINE, PCRE_DOTALL, PCRE_EXTENDED, PCRE_ANCHORED, PCRE_DOLLAR_ENDONLY,
    PCRE_EXTRA, 0, 0, PCRE_UNGREEDY, 0, PCRE_UTF8);
  cRunOptions: array[TRegExOption] of integer =
  (0, 0, 0, 0, 0, 0,
    0, PCRE_NOTBOL, PCRE_NOTEOL, 0, PCRE_NOTEMPTY, 0);
var
  i: TRegExOption;
begin
  Result := 0;
  if RunTime then
  begin
    for i := Low(TRegExOption) to High(TRegExOption) do
      if (i in Options) then
        Result := Result or cRunOptions[i];
  end
  else
  begin
    for i := Low(TRegExOption) to High(TRegExOption) do
      if (i in Options) then
        Result := Result or cDesignOptions[i];
  end;
end;

function TRegEx.GetCaptureCount: integer;
begin
  Result := FStringCount;
  //  PCRECheck(pcre_fullinfo(FCode, FExtra, PCRE_INFO_CAPTURECOUNT, @Result));
end;

function TRegEx.GetCaptures(Index: integer): string;
var buffer: array[0..1024] of char;
begin
  PCRECheck(pcre_copy_substring(PChar(FSubject), @FVector, FStringCount, Index, buffer, sizeof(buffer)));
  Result := string(buffer);
end;

function TRegEx.GetCapturesOffset(Index: integer): TCaptureOffset;
begin
  if (Index < 0) or (Index >= FStringCount) then
  begin
    Result.FirstPos := -1;
    Result.LastPos := -1;
  end;
  Result.FirstPos := FVector[Index * 2];
  Result.LastPos := FVector[Index * 2 + 1];
end;

function TRegEx.Match(const Subject: string; StartOffset: Cardinal = 1): boolean;
begin
  if StartOffset < 1 then
    StartOffset := 1;
  FSubject := Subject;
  FStringCount := pcre_exec(FCode, FExtra, PChar(FSubject), Length(FSubject),
    StartOffset - 1, GetAPIOptions(true), @FVector, FVectorSize);
  Result := FStringCount > 0;
end;

{ EPCREError }

constructor EPCREError.Create(const Msg: string; ErrorCode: integer);
begin
  FErrorCode := ErrorCode;
  inherited Create(Msg);
end;

initialization
  LoadPCRE;
finalization
  UnloadPCRE;

end.

