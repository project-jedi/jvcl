unit JvGnugettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and others               *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*                                                            *)
(*  Contributors: Peter Thornqvist, Troy Wolbrink,            *)
(*                Frank Andreas de Groot, Igor Siticov,       *)
(*                Jacques Garcia Vazquez                      *)
(*                Andreas Hausladen                           *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// The names of any contributor may not be used to endorse or promote
// products derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

// If the conditional define DXGETTEXTDEBUG is defined, debugging log is activated.
// Use DefaultInstance.DebugLogToFile() to write the log to a file.
{ $define DXGETTEXTDEBUG}

{$ifdef VER100}
  // Delphi 3
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
{$endif}
{$ifdef VER110}
  // C++ Builder 3
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
{$endif}
{$ifdef VER120}
  // Delphi 4
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
{$endif}
{$ifdef VER125}
  // C++ Builder 4
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
{$endif}
{$ifdef VER130}
  // Delphi 5
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
  {$ifdef WIN32}
  {$DEFINE MSWINDOWS}
  {$endif}
{$endif}
{$ifdef VER135}
  // C++ Builder 5
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
  {$ifdef WIN32}
  {$DEFINE MSWINDOWS}
  {$endif}
{$endif}
{$ifdef VER140}
  // Delphi 6
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI7OROLDER}
{$endif}
{$ifdef VER150}
  {$DEFINE DELPHI7OROLDER}
  // Delphi 7
{$endif}
{$ifdef VER160}
  // Delphi 8
{$endif}

uses
{$ifdef DELPHI5OROLDER}
  JvGnugettextD5,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef LINUX}
  Libc,
{$endif}
  Classes, SysUtils, Contnrs, TypInfo;

(*****************************************************************************)
(*                                                                           *)
(*  MAIN API                                                                 *)
(*                                                                           *)
(*****************************************************************************)

// Main GNU gettext functions. See documentation for instructions on how to use them.
{$ifdef DELPHI5OROLDER}
function _(const szMsgId: WideString): WideString;
function gettext(const szMsgId: WideString): WideString;
function dgettext(const szDomain: string; const szMsgId: WideString): WideString;
function dngettext(const szDomain: string; const singular, plural: WideString; Number: Longint): WideString;
function ngettext(const singular, plural: WideString; Number: Longint): WideString;
{$endif}
{$ifndef DELPHI5OROLDER}
function _(const szMsgId: AnsiString): WideString; overload;
function _(const szMsgId: WideString): WideString; overload;
function gettext(const szMsgId: AnsiString): WideString; overload;
function gettext(const szMsgId: WideString): WideString; overload;
function dgettext(const szDomain: string; const szMsgId: AnsiString): WideString; overload;
function dgettext(const szDomain: string; const szMsgId: WideString): WideString; overload;
function dngettext(const szDomain: string; const singular, plural: AnsiString; Number: Longint): WideString; overload;
function dngettext(const szDomain: string; const singular, plural: WideString; Number: Longint): WideString; overload;
function ngettext(const singular, plural: AnsiString; Number: Longint): WideString; overload;
function ngettext(const singular, plural: WideString; Number: Longint): WideString; overload;
{$endif}
procedure textdomain(const szDomain: string);
function getcurrenttextdomain: string;
procedure bindtextdomain(const szDomain, szDirectory: string);

  // Set language to use
procedure UseLanguage(const LanguageCode: string);
function GetCurrentLanguage: string;

  // Translates a component (form, frame etc.) to the currently selected language.
  // Put TranslateComponent(self) in the OnCreate event of all your forms.
  // See the manual for documentation on these functions
type
  TTranslator = procedure(obj: TObject) of object;

procedure TP_Ignore(AnObject: TObject; const Name: AnsiString);
procedure TP_IgnoreClass(IgnClass: TClass);
procedure TP_IgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString);
procedure TP_GlobalIgnoreClass(IgnClass: TClass);
procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString);
procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator);
procedure TranslateComponent(AnObject: TComponent; const TextDomain: string = '');
procedure RetranslateComponent(AnObject: TComponent; const TextDomain: string = '');

  // Add more domains that resourcestrings can be extracted from. If a translation
  // is not found in the default domain, this domain will be searched, too.
  // This is useful for adding mo files for certain runtime libraries and 3rd
  // party component libraries
procedure AddDomainForResourceString(const domain: string);
procedure RemoveDomainForResourceString(const domain: string);

{$ifndef CLR}
  // Unicode-enabled way to get resourcestrings, automatically translated
  // Use like this: ws := LoadResStringW(@NameOfResourceString);
function LoadResString(ResStringRec: PResStringRec): WideString;
function LoadResStringA(ResStringRec: PResStringRec): AnsiString;
function LoadResStringW(ResStringRec: PResStringRec): WideString;
{$endif}

  // This returns an empty string if not translated or translator name is not specified.
function GetTranslatorNameAndEmail: WideString; 


(*****************************************************************************)
(*                                                                           *)
(*  ADVANCED FUNCTIONALITY                                                   *)
(*                                                                           *)
(*****************************************************************************)

const
  DefaultTextDomain = 'default'; 

var
  ExecutableFilename: string;
  // This is set to paramstr(0) or the name of the DLL you are creating.

type
  EGnuGettext = class(Exception); 
  EGGProgrammingError = class(EGnuGettext);
  EGGComponentError = class(EGnuGettext);
  EGGIOError = class(EGnuGettext); 
  EGGAnsi2WideConvError = class(EGnuGettext); 

{$ifndef CLR}
  // This function will turn resourcestring hooks on or off, eventually with BPL file support.
  // Please do not activate BPL file support when the package is in design mode.
const
  AutoCreateHooks = True;

procedure HookIntoResourceStrings(Enabled: Boolean = True;
  SupportPackages: Boolean = False);
{$endif}



(*****************************************************************************)
(*                                                                           *)
(*  CLASS based implementation.                                              *)
(*  Use TGnuGettextInstance to have more than one language                   *)
(*  in your application at the same time                                     *)
(*                                                                           *)
(*****************************************************************************)

{$ifdef MSWINDOWS}
{$ifndef DELPHI6OROLDER}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$endif}
{$endif}
{$ifndef DELPHI7OROLDER}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$endif}

type
  TOnDebugLine = procedure(Sender: TObject; const Line: AnsiString;
    var Discard: Boolean) of object;  // Set Discard to False if output should still go to ordinary debug log
  TGetPluralForm = function(Number: Longint): Integer;
  TDebugLogger = procedure(Line: AnsiString) of object;

  TMoFile = class // Don't use this class. It's for internal use.
      // Threadsafe. Only constructor and destructor are writing to memory
  private
    doswap: Boolean;
  public
    Users: Integer;
    // Reference count. If it reaches zero, this object should be destroyed.
    constructor Create(const Filename: string; Offset, Size: Int64);
    function gettext(const msgid: AnsiString; var Found: Boolean): AnsiString;
    // uses mo file
    property isSwappedArchitecture: Boolean read doswap;
  private
    N, O, T: Cardinal; // Values defined at http://www.linuxselfhelp.com/gnu/gettext/html_chapter/gettext_6.html
    StartIndex, StartStep: Integer;
    moMemory: array of byte;
    function CardinalInMem(Offset: Cardinal): Cardinal;
  end;

  TDomain = class // Don't use this class. It's for internal use.
  private
    Enabled: Boolean;
    vDirectory: string;
    procedure SetDirectory(const Value: string);
  public
    DebugLogger: TDebugLogger;
    Domain: string;
    property Directory: string read vDirectory write SetDirectory;
    constructor Create;
    destructor Destroy; override;
    // Set parameters
    procedure SetLanguageCode(const LangCode: string);
    procedure SetFilename(const Filename: string); // Bind this domain to a specific file
    // Get information
    procedure GetListOfLanguages(List: TStrings);
    function GetTranslationProperty(PropertyName: string): WideString;
    function gettext(const msgid: AnsiString): AnsiString; // uses mo file
  private
    moFile: TMoFile;
    SpecificFilename: string;
    curlang: string;
    OpenHasFailedBefore: Boolean;
    procedure OpenMoFile;
    procedure CloseMoFile;
  end;

  TExecutable = class
    procedure Execute; virtual; abstract;
  end;

  TGnuGettextInstance = class
  private
    fOnDebugLine: TOnDebugLine;
    {$ifndef CLR}
    CreatorThread: Cardinal;  // Only this thread can use LoadResString
    {$endif}
  public
    Enabled: Boolean;      // Set this to False to disable translations
    DesignTimeCodePage: Integer;
    // See MultiByteToWideChar() in Win32 API for documentation
    constructor Create;
    destructor Destroy; override;
    procedure UseLanguage(LanguageCode: string);
    procedure GetListOfLanguages(const domain: string; List: TStrings); // Puts List of language codes, for which there are translations in the specified domain, into List
    {$ifdef DELPHI5OROLDER}
    function gettext(const szMsgId: WideString): WideString;
    function ngettext(const singular, plural: WideString; Number: Longint): WideString;
    {$endif}
    {$ifndef DELPHI5OROLDER}
    function gettext(const szMsgId: AnsiString): WideString; overload;
    function gettext(const szMsgId: WideString): WideString; overload;
    function ngettext(const singular, plural: AnsiString; Number: Longint): WideString; overload;
    function ngettext(const singular, plural: WideString; Number: Longint): WideString; overload;
    {$endif}
    function GetCurrentLanguage: string;
    function GetTranslationProperty(const PropertyName: AnsiString): WideString;
    function GetTranslatorNameAndEmail: WideString;

    // Form translation tools, these are not threadsafe. All TP_ procs must be called just before TranslateProperites()
    procedure TP_Ignore(AnObject: TObject; const Name: AnsiString);
    procedure TP_IgnoreClass(IgnClass: TClass);
    procedure TP_IgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString);
    procedure TP_GlobalIgnoreClass(IgnClass: TClass);
    procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString);
    procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator);
    procedure TranslateProperties(AnObject: TObject; TextDomain: string = '');
    procedure TranslateComponent(AnObject: TComponent; const TextDomain: string = '');
    procedure RetranslateComponent(AnObject: TComponent; const TextDomain: string = '');

    // Multi-domain functions
    {$ifdef DELPHI5OROLDER}
    function dgettext(const szDomain: string; const szMsgId: WideString): WideString;
    function dngettext(const szDomain: string; singular, plural: WideString; Number: Longint): WideString;
    {$endif}
    {$ifndef DELPHI5OROLDER}
    function dgettext(const szDomain: string; const szMsgId: AnsiString): WideString; overload;
    function dgettext(const szDomain: string; const szMsgId: WideString): WideString; overload;
    function dngettext(const szDomain: string; singular, plural: AnsiString; Number: Longint): WideString; overload;
    function dngettext(const szDomain: string; singular, plural: WideString; Number: Longint): WideString; overload;
    {$endif}
    procedure textdomain(const szDomain: string);
    function getcurrenttextdomain: string;
    procedure bindtextdomain(const szDomain, szDirectory: string);
    procedure bindtextdomainToFile(const szDomain, Filename: string);
    // Also works with files embedded in exe file

    {$ifndef CLR}
    // Windows API functions
    function LoadResString(ResStringRec: PResStringRec): WideString;
    {$endif}

    // Output all log info to this file. This may only be called once.
    procedure DebugLogToFile(const Filename: string; append: Boolean = False);
    procedure DebugLogPause(PauseEnabled: Boolean);
    property OnDebugLine: TOnDebugLine read fOnDebugLine write fOnDebugLine;
    // If set, all debug output goes here

    // Conversion according to design-time character set
    function ansi2wide(const s: AnsiString): WideString; 
  protected
    procedure TranslateStrings(sl: TStrings; const TextDomain: string);

    // Override these three, if you want to inherited from this class
    // to create a new class that handles other domain and language dependent
    // issues
    procedure WhenNewLanguage(const LanguageID: AnsiString); virtual;
      // Override to know when language changes
    procedure WhenNewDomain(const TextDomain: string); virtual;
      // Override to know when text domain changes. Directory is purely informational
    procedure WhenNewDomainDirectory(const TextDomain, Directory: string); virtual;
      // Override to know when any text domain's directory changes. It won't be called if a domain is fixed to a specific file.
  private
    curlang: string;
    curGetPluralForm: TGetPluralForm;
    curmsgdomain: string;
    savefileCS: TMultiReadExclusiveWriteSynchronizer;
    savefile: TextFile;
    SaveMemory: TStringList;
    DefaultDomainDirectory: string;
    DomainList: TStringList;     // List of domain names. Objects are TDomain.
    TP_IgnoreList: TStringList;
    // Temporary List, reset each time TranslateProperties is called
    TP_ClassHandling: TObjectList;
    // Items are TClassMode. If a is derived from b, a comes first
    TP_GlobalClassHandling: TObjectList;
    // Items are TClassMode. If a is derived from b, a comes first
    TP_Retranslator: TExecutable; // Cast this to TTP_Retranslator
    DebugLogCS: TMultiReadExclusiveWriteSynchronizer;
    DebugLog: TStream;
    DebugLogOutputPaused: Boolean;
    function TP_CreateRetranslator: TExecutable;  // Must be freed by caller!
    procedure DebugWriteln(Line: AnsiString);
    function Getdomain(const domain, DefaultDomainDirectory, CurLang: string): TDomain;
      // Translates a single property of an object
    {$ifndef CLR}
    procedure TranslateProperty(AnObject: TObject; PropInfo: PPropInfo;
      TodoList: TStrings; const TextDomain: string);
    {$endif}
  end;

var
  DefaultInstance: TGnuGettextInstance;

implementation

(**************************************************************************)
// Some comments on the implementation:
// This unit should be independent of other units where possible.
// It should have a small footprint in any way.
(**************************************************************************)
// TMultiReadExclusiveWriteSynchronizer is used instead of TCriticalSection
// because it makes this unit independent of the SyncObjs unit
(**************************************************************************)

{$ifdef CLR}
uses
  System.Globalization, System.Diagnostics, System.Windows.Forms;
{$endif}

type
  TTP_RetranslatorItem = class
    obj: TObject;
    Propname: AnsiString;
    OldValue: WideString;
  end;

  TTP_Retranslator = class(TExecutable)
    TextDomain: string;
    Instance: TGnuGettextInstance;
    constructor Create;
    destructor Destroy; override;
    procedure Remember(obj: TObject; const PropName: AnsiString; OldValue: WideString);
    procedure Execute; override;
  private
    List: TList;
  end;

  TEmbeddedFileInfo = class
    Offset, Size: Int64;
  end;

  TFileLocator = class // This class finds files even when embedded inside executable
    constructor Create;
    destructor Destroy; override;
    procedure Analyze;  // List files embedded inside executable
    function FileExists(Filename: string): Boolean;
    function GetMoFile(Filename: string; DebugLogger: TDebugLogger): TMoFile;
    procedure ReleaseMoFile(var moFile: TMoFile);
  private
    BaseDirectory: string;
    FileList: TStringList;
    //Objects are TEmbeddedFileInfo. Filenames are relative to .exe file
    MoFilesCS: TMultiReadExclusiveWriteSynchronizer;
    MoFiles: TStringList; // Objects are filenames+Offset, objects are TMoFile
    function ReadInt64(str: TStream): Int64;
  end;

  TGnuGettextComponentMarker = class(TComponent)
  public
    LastLanguage: AnsiString;
    Retranslator: TExecutable;
    destructor Destroy; override;
  end;

  TClassMode = class
    HClass: TClass;
    SpecialHandler: TTranslator;
    PropertiesToIgnore: TStringList; // This is ignored if Handler is set
    constructor Create;
    destructor Destroy; override;
  end;

  TRStrinfo = record
    strlength, stroffset: Cardinal;
  end;

  TStrInfoArr = array[0..10000000] of TRStrinfo;
  PStrInfoArr = ^TStrInfoArr;

  TCharArray5 = array[0..4] of ansichar;

  {$ifndef CLR}
  THook = class // Replaces a runtime library procedure with a custom procedure
  public
    constructor Create(OldProcedure, NewProcedure: Pointer; FollowJump: Boolean = False);
    destructor Destroy; override;  // Restores unhooked state
    procedure Reset(FollowJump: Boolean = False);
    // Disables and picks up patch points again
    procedure Disable;
    procedure Enable;
  private
    OldProc, NewProc: Pointer;
    Patch: TCharArray5;
    Original: TCharArray5;
    PatchPosition: PChar;
    procedure Shutdown; // Same as destroy, except that object is not destroyed
  end;
  {$endif}

var
  // System information
  Win32PlatformIsUnicode: Boolean = False;

  // Information about files embedded inside .exe file
  FileLocator: TFileLocator;

  ResourceStringDomainListCS: TMultiReadExclusiveWriteSynchronizer;
  ResourceStringDomainList: TStringList;
  {$ifndef CLR}
  // Hooks into runtime library functions
  HookLoadResString: THook;
  HookLoadStr: THook;
  HookFmtLoadStr: THook;
  {$endif}

function Utf8EncodeChar(wc: WideChar): AnsiString;
var
  w: Word;
begin
  w := Ord(wc);
  case w of
    0..$7F:
      Result := AnsiChar(w);
    $80..$3FF:
      Result := AnsiChar($C0 + (w shr 6)) +
                AnsiChar($80 + (w and $3F));
    $400..$FFFF:
      Result := AnsiChar($E0 +(w shr 12))+
                AnsiChar($80 +((w shr 6) and $3F)) +
                AnsiChar($80 +(w and $3F));
  else
    raise Exception.Create('Huh, what happened here?');
  end;
end;

function Utf8Encode(ws: WideString): AnsiString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(ws) do
    Result := Result + Utf8EncodeChar(ws[i]);
end;

// If dummychar is #0, it will raise Exception when an error occurs
function Utf8Decode(s: AnsiString; dummychar: WideChar = #0): WideString;
var
  i: Integer;
  b: Byte;
  c: Cardinal;
  mode: 0..5;
begin
  Result := '';
  mode := 0;
  c := 0;
  for i := 1 to Length(s) do
  begin
    b := Ord(s[i]);
    if mode = 0 then
    begin
      case b of
        0..$7F:
          Result := Result + WideChar(b);
        $80..$BF, $FF:
          begin
            if dummychar = #0 then
              raise Exception.Create ('Invalid byte sequence encountered in utf-8 string')
            else
              Result := Result + dummychar;
            mode := 0;
          end;
        $C0..$DF:
          begin
            c := (b and $1F);
            mode := 1;
          end;
        $E0..$EF:
          begin
            c := (b and $F);
            mode := 2;
          end;
        $F0..$F7:
          begin
            c := (b and $7);
            mode := 3;
          end;
        $F8..$FB:
          begin
            c := (b and $3);
            mode := 4;
          end;
        $FC..$FE:
          begin
            c := (b and $1);
            mode := 5;
          end;
      end;
    end
    else
    begin
      case b of
        $00..$7F, $C0..$FF:
          if dummychar = #0 then
            raise Exception.Create('Invalid byte sequence encountered in utf-8 string')
          else
            Result:=Result+dummychar;
        $80..$BF:
          begin
            c := c * $40 + (b and $3F);
            Dec(mode);
            if mode = 0 then
            begin
              if c <= $FFFF then
                Result := Result + Chr(c)
              else
              begin
                if dummychar = #0 then
                  raise Exception.Create('Utf-8 string contained unicode character larger than $FFFF. This is not supported.')
                else
                  Result := Result + dummychar;
              end;
            end;
          end;
      else
        raise Exception.Create ('Huh? More than 256 different values in a byte?');
      end;
    end;
  end;
  if mode <> 0 then begin
    if dummychar = #0 then
      raise Exception.Create ('Utf-8 string terminated unexpectedly in the middle of a multibyte sequence')
    else
      Result := Result + dummychar;
  end;
end;

function StripCR(s: AnsiString): AnsiString;
var
  i: Integer;
begin
  i := 1;
  while i <= Length(s) do
  begin
    if s[i] = #13 then
      Delete(s, i, 1)
    else
      Inc(i);
  end;
  Result := s;
end;

function GGGetEnvironmentVariable(const Name: string): string;
{$ifdef DELPHI5OROLDER}
var
  Len: DWORD;
{$endif}
begin
  {$ifdef DELPHI5OROLDER}
  SetLength(Result, 1024);
  Len := Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), 1024);
  SetLength(Result, Len);
  if Len > 1024 then
    if Windows.GetEnvironmentVariable(PChar(Name),PChar(Result), Len) <> Len then
      Result := 'ERROR: Windows environment changes concurrently with this application';
  {$endif}
  {$ifndef DELPHI5OROLDER}
  Result := SysUtils.GetEnvironmentVariable(Name);
  {$endif}
end;

function StartsWith(const Text, StartText: string; CaseInsensitive: Boolean = False): Boolean;
var
  Len, i: Integer;
begin
  Result := False;
  Len := Length(StartText);
  if Len > Length(Text) then
    Exit;
  if CaseInsensitive then
  begin
    for i := 1 to Len do
      if UpCase(Text[i]) <> UpCase(StartText[i]) then
        Exit;
  end
  else
  begin
    for i := 1 to Len do
      if Text[i] <> StartText[i] then
        Exit;
  end;
  Result := True;
end;

function EndsWith(const Text, EndText: string; CaseInsensitive: Boolean): Boolean;
var
  Len, i, x: Integer;
begin
  Result := False;
  Len := Length(EndText);
  x := Length(Text);
  if Len > x then
    Exit;
  if CaseInsensitive then
  begin
    for i := Len downto 1 do
      if UpCase(Text[x]) <> UpCase(EndText[i]) then
        Exit
      else
        Dec(x);
  end
  else
  begin
    for i := Len downto 1 do
      if Text[x] <> EndText[i] then
        Exit
      else
        Dec(x);
  end;
  Result := True;
end;

function IsInDirStrOf(const Filename, Dir: string): Boolean;
begin
  Result := StartsWith(Filename, Dir, {$ifdef MSWINDOWS}True{$else}False{$endif});
end;

function EndsWithFilename(const Path, Filename: string): Boolean;
begin
  Result := EndsWith(Path, Filename, {$ifdef MSWINDOWS}True{$else}False{$endif});
end;

{$ifdef CLR}
function TrimCopy(const S: string; Index, Count: Integer): string; overload;
var
  Len, StartIndex, EndIndex: Integer;
begin
  Result := '';

  Len := Length(S);
  if Index <= 0 then
    Index := 1;
  if Count > Len then
    Count := Len;

  if (Count > 0) and (Len > 0) then
  begin
    StartIndex := Index;
    while (StartIndex <= Len) and (S[StartIndex] <= #32) do
      Inc(StartIndex);
    Dec(Count, StartIndex - Index);

    EndIndex := StartIndex + Count - 1;
    if EndIndex > Len then
    begin
      Dec(Count, EndIndex - Len);
      EndIndex := Len;
    end;

    while (EndIndex > 0) and (S[EndIndex] <= #32) do
    begin
      Dec(EndIndex);
      Dec(Count);
    end;

    if EndIndex >= StartIndex then
      Result := Copy(S, StartIndex, Count);
  end;
end;
{$endif}

function TrimCopy(const S: AnsiString; Index, Count: Integer): AnsiString; overload;
var
  Len, StartIndex, EndIndex: Integer;
begin
  Result := '';

  Len := Length(S);
  if Index <= 0 then
    Index := 1;
  if Count > Len then
    Count := Len;

  if (Count > 0) and (Len > 0) then
  begin
    StartIndex := Index;
    while (StartIndex <= Len) and (S[StartIndex] <= #32) do
      Inc(StartIndex);
    Dec(Count, StartIndex - Index);

    EndIndex := StartIndex + Count - 1;
    if EndIndex > Len then
    begin
      Dec(Count, EndIndex - Len);
      EndIndex := Len;
    end;

    while (EndIndex > 0) and (S[EndIndex] <= #32) do
    begin
      Dec(EndIndex);
      Dec(Count);
    end;

    if EndIndex >= StartIndex then
      {$ifdef CLR}
      Result := Copy(S, StartIndex, Count);
      {$else}
      SetString(Result, PChar(Pointer(S)) + StartIndex - 1, Count);
      {$endif CLR}
  end;
end;

function LF2LineBreakA(s: AnsiString): AnsiString;
{$ifdef MSWINDOWS}
var
  i: Integer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  Assert(sLinebreak = #13#10);
  i := 1;
  while i <= Length(s) do
  begin
    if (s[i] = #10) and (i > 1) and (s[i - 1] <> #13) then
    begin
      Insert(#13, s, i);
      Inc(i, 2);
    end
    else
      Inc(i);
  end;
  {$endif}
  Result := s;
end;

function IsWriteProp(Info: PPropInfo): Boolean;
begin
  {$ifndef CLR}
  Result := Assigned(Info) and (Info^.SetProc <> nil);
  {$else}
  Result := Assigned(Info) and CanWrite(Info);
  {$endif}
end;

{ not used }
{
function string2csyntax(const s: AnsiString): AnsiString;
  // Converts a string to the syntax that is used in .po files
var
  i: Integer;
  c: AnsiChar;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    c := s[i];
    case c of
      #32..#33, #35..#255: Result := Result + c;
      #13: Result := Result + '\r';
      #10: Result := Result + '\n"'#13#10'"';
      #34: Result := Result + '\"';
      else
        Result := Result + '\0x' + IntToHex(Ord(c), 2);
    end;
  end;
  Result := '"' + Result + '"';
end;
}

{$ifdef DELPHI5OROLDER}
function GetPropList(AObject: TObject; out PropList: PPropList): Integer;
var
  Data: PTypeData;
begin
  Result := 0;
  PropList := nil;
  if (AObject.ClassInfo <> nil) then
  begin
    Data := GetTypeData(AObject.ClassInfo);
    Result := Data^.PropCount;
    if Result > 0 then
    begin
      GetMem(PropList, Result * SizeOf(PPropInfo));
      GetPropInfos(AObject.ClassInfo, PropList);
    end;
  end;
end;
{$endif}

function ResourceStringGettext(const MsgId: WideString): WideString;
var
  i: Integer;
begin
  if (MsgId = '') or (ResourceStringDomainListCS = nil) then
  begin
    // This only happens during very complicated program startups that fail
    // or when MsgId=''
    Result := MsgId;
    Exit;
  end;
  ResourceStringDomainListCS.BeginRead;
  try
    for i := 0 to ResourceStringDomainList.Count - 1 do
    begin
      Result := dgettext(ResourceStringDomainList.Strings[i], MsgId); 
      if Result <> MsgId then
        Break; 
    end; 
  finally
    ResourceStringDomainListCS.EndRead; 
  end; 
end; 

{$ifndef DELPHI5OROLDER}
function gettext(const szMsgId: AnsiString): WideString;
begin
  Result := DefaultInstance.gettext(szMsgId);
end;
{$endif}

function gettext(const szMsgId: WideString): WideString; 
begin
  Result := DefaultInstance.gettext(szMsgId); 
end; 

{$ifndef DELPHI5OROLDER}
function _(const szMsgId: AnsiString): WideString;
begin
  Result := DefaultInstance.gettext(szMsgId);
end;
{$endif}

function _(const szMsgId: WideString): WideString; 
begin
  Result := DefaultInstance.gettext(szMsgId); 
end;

{$ifndef DELPHI5OROLDER}
function dgettext(const szDomain: string; const szMsgId: AnsiString): WideString;
begin
  Result := DefaultInstance.dgettext(szDomain, szMsgId);
end;
{$endif}

function dgettext(const szDomain: string; const szMsgId: WideString): WideString;
begin
  Result := DefaultInstance.dgettext(szDomain, szMsgId);
end;

{$ifndef DELPHI5OROLDER}
function dngettext(const szDomain: string; const singular, plural: AnsiString;
  Number: Longint): WideString;
begin
  Result := DefaultInstance.dngettext(szDomain, singular, plural, Number);
end;
{$endif}

function dngettext(const szDomain: string; const singular, plural: WideString;
  Number: Longint): WideString;
begin
  Result := DefaultInstance.dngettext(szDomain, singular, plural, Number);
end;

{$ifndef DELPHI5OROLDER}
function ngettext(const singular, plural: AnsiString; Number: Longint): WideString;
begin
  Result := DefaultInstance.ngettext(singular, plural, Number);
end;
{$endif}

function ngettext(const singular, plural: WideString; Number: Longint): WideString;
begin
  Result := DefaultInstance.ngettext(singular, plural, Number);
end; 

procedure textdomain(const szDomain: string);
begin
  DefaultInstance.textdomain(szDomain);
end; 

{not used}
{
procedure SetGettextEnabled(Enabled: Boolean);
begin
  DefaultInstance.Enabled := Enabled;
end;
}

function getcurrenttextdomain: string;
begin
  Result := DefaultInstance.getcurrenttextdomain;
end; 

procedure bindtextdomain(const szDomain, szDirectory: string);
begin
  DefaultInstance.bindtextdomain(szDomain, szDirectory);
end;

procedure TP_Ignore(AnObject: TObject; const Name: AnsiString);
begin
  DefaultInstance.TP_Ignore(AnObject, Name); 
end; 

procedure TP_GlobalIgnoreClass(IgnClass: TClass); 
begin
  DefaultInstance.TP_GlobalIgnoreClass(IgnClass); 
end; 

procedure TP_IgnoreClass(IgnClass: TClass);
begin
  DefaultInstance.TP_IgnoreClass(IgnClass); 
end; 

procedure TP_IgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString); 
begin
  DefaultInstance.TP_IgnoreClassProperty(IgnClass, PropertyName);
end; 

procedure TP_GlobalIgnoreClassProperty(IgnClass: TClass; const PropertyName: AnsiString);
begin
  DefaultInstance.TP_GlobalIgnoreClassProperty(IgnClass, PropertyName);
end; 

procedure TP_GlobalHandleClass(HClass: TClass; Handler: TTranslator); 
begin
  DefaultInstance.TP_GlobalHandleClass(HClass, Handler); 
end; 

{not used}
{
procedure TranslateProperties(AnObject: TObject; TextDomain: AnsiString = '');
begin
  DefaultInstance.TranslateProperties(AnObject, TextDomain);
end;
} 

procedure TranslateComponent(AnObject: TComponent; const TextDomain: string = '');
begin
  DefaultInstance.TranslateComponent(AnObject, TextDomain);
end;

procedure RetranslateComponent(AnObject: TComponent; const TextDomain: string = '');
begin
  DefaultInstance.RetranslateComponent(AnObject, TextDomain); 
end; 

{$ifdef MSWINDOWS}

// These constants are only used in Windows 95
// Thanks to Frank Andreas de Groot for this table
const
  IDAfrikaans = $0436;
  IDAlbanian = $041C;
  IDArabicAlgeria = $1401;
  IDArabicBahrain = $3C01;
  IDArabicEgypt = $0C01;
  IDArabicIraq = $0801;
  IDArabicJordan = $2C01;
  IDArabicKuwait = $3401;
  IDArabicLebanon = $3001;
  IDArabicLibya = $1001;
  IDArabicMorocco = $1801;
  IDArabicOman = $2001;
  IDArabicQatar = $4001;
  IDArabic = $0401;
  IDArabicSyria = $2801;
  IDArabicTunisia = $1C01;
  IDArabicUAE = $3801;
  IDArabicYemen = $2401;
  IDArmenian = $042B;
  IDAssamese = $044D;
  IDAzeriCyrillic = $082C;
  IDAzeriLatin = $042C;
  IDBasque = $042D;
  IDByelorussian = $0423;
  IDBengali = $0445;
  IDBulgarian = $0402; 
  IDBurmese = $0455;  
  IDCatalan = $0403; 
  IDChineseHongKong = $0C04;  
  IDChineseMacao = $1404; 
  IDSimplifiedChinese = $0804;  
  IDChineseSingapore = $1004; 
  IDTraditionalChinese = $0404;  
  IDCroatian = $041A;
  IDCzech = $0405;
  IDDanish = $0406; 
  IDBelgianDutch = $0813;  
  IDDutch = $0413; 
  IDEnglishAUS = $0C09;  
  IDEnglishBelize = $2809; 
  IDEnglishCanadian = $1009;  
  IDEnglishCaribbean = $2409; 
  IDEnglishIreland = $1809;  
  IDEnglishJamaica = $2009; 
  IDEnglishNewZealand = $1409;  
  IDEnglishPhilippines = $3409; 
  IDEnglishSouthAfrica = $1C09;  
  IDEnglishTrinidad = $2C09; 
  IDEnglishUK = $0809;  
  IDEnglishUS = $0409; 
  IDEnglishZimbabwe = $3009;
  IDEstonian = $0425; 
  IDFaeroese = $0438;  
  IDFarsi = $0429; 
  IDFinnish = $040B;  
  IDBelgianFrench = $080C; 
  IDFrenchCameroon = $2C0C;
  IDFrenchCanadian = $0C0C; 
  IDFrenchCotedIvoire = $300C;  
  IDFrench = $040C; 
  IDFrenchLuxembourg = $140C;  
  IDFrenchMali = $340C; 
  IDFrenchMonaco = $180C;  
  IDFrenchReunion = $200C; 
  IDFrenchSenegal = $280C;  
  IDSwissFrench = $100C; 
  IDFrenchWestIndies = $1C0C;  
  IDFrenchZaire = $240C; 
  IDFrisianNetherlands = $0462;  
  IDGaelicIreland = $083C; 
  IDGaelicScotland = $043C;
  IDGalician = $0456;
  IDGeorgian = $0437;  
  IDGermanAustria = $0C07; 
  IDGerman = $0407;  
  IDGermanLiechtenstein = $1407; 
  IDGermanLuxembourg = $1007;  
  IDSwissGerman = $0807; 
  IDGreek = $0408;  
  IDGujarati = $0447; 
  IDHebrew = $040D;  
  IDHindi = $0439; 
  IDHungarian = $040E;
  IDIcelandic = $040F; 
  IDIndonesian = $0421;  
  IDItalian = $0410; 
  IDSwissItalian = $0810;  
  IDJapanese = $0411; 
  IDKannada = $044B;
  IDKashmiri = $0460; 
  IDKazakh = $043F;  
  IDKhmer = $0453; 
  IDKirghiz = $0440;  
  IDKonkani = $0457; 
  IDKorean = $0412;  
  IDLao = $0454; 
  IDLatvian = $0426;  
  IDLithuanian = $0427; 
  IDMacedonian = $042F;  
  IDMalaysian = $043E; 
  IDMalayBruneiDarussalam = $083E;  
  IDMalayalam = $044C; 
  IDMaltese = $043A;  
  IDManipuri = $0458; 
  IDMarathi = $044E;  
  IDMongolian = $0450; 
  IDNepali = $0461;  
  IDNorwegianBokmol = $0414;
  IDNorwegianNynorsk = $0814;
  IDOriya = $0448; 
  IDPolish = $0415;  
  IDBrazilianPortuguese = $0416; 
  IDPortuguese = $0816;  
  IDPunjabi = $0446; 
  IDRhaetoRomanic = $0417;
  IDRomanianMoldova = $0818; 
  IDRomanian = $0418;  
  IDRussianMoldova = $0819; 
  IDRussian = $0419;  
  IDSamiLappish = $043B; 
  IDSanskrit = $044F;
  IDSerbianCyrillic = $0C1A; 
  IDSerbianLatin = $081A;  
  IDSesotho = $0430; 
  IDSindhi = $0459;  
  IDSlovak = $041B; 
  IDSlovenian = $0424;  
  IDSorbian = $042E; 
  IDSpanishArgentina = $2C0A;  
  IDSpanishBolivia = $400A; 
  IDSpanishChile = $340A;  
  IDSpanishColombia = $240A; 
  IDSpanishCostaRica = $140A;  
  IDSpanishDominicanRepublic = $1C0A; 
  IDSpanishEcuador = $300A;  
  IDSpanishElSalvador = $440A; 
  IDSpanishGuatemala = $100A;  
  IDSpanishHonduras = $480A; 
  IDMexicanSpanish = $080A;  
  IDSpanishNicaragua = $4C0A; 
  IDSpanishPanama = $180A;  
  IDSpanishParaguay = $3C0A; 
  IDSpanishPeru = $280A;  
  IDSpanishPuertoRico = $500A; 
  IDSpanishModernSort = $0C0A;
  IDSpanish = $040A;
  IDSpanishUruguay = $380A;
  IDSpanishVenezuela = $200A; 
  IDSutu = $0430;  
  IDSwahili = $0441; 
  IDSwedishFinland = $081D;  
  IDSwedish = $041D; 
  IDTajik = $0428;
  IDTamil = $0449; 
  IDTatar = $0444;  
  IDTelugu = $044A; 
  IDThai = $041E;  
  IDTibetan = $0451; 
  IDTsonga = $0431;  
  IDTswana = $0432; 
  IDTurkish = $041F;  
  IDTurkmen = $0442; 
  IDUkrainian = $0422;  
  IDUrdu = $0420; 
  IDUzbekCyrillic = $0843;  
  IDUzbekLatin = $0443; 
  IDVenda = $0433;  
  IDVietnamese = $042A; 
  IDWelsh = $0452;  
  IDXhosa = $0434; 
  IDZulu = $0435; 

function GetOSLanguage: AnsiString; 
var
  langid: Cardinal; 
  LangCode: AnsiString;
  CountryName: array[0..4] of AnsiChar;
  LanguageName: array[0..4] of AnsiChar; 
  works: Boolean;
begin
  // The return value of GetLocaleInfo is compared with 3 = 2 characters and a zero
  works := 3 = GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME,
    LanguageName, SizeOf(LanguageName));
  works := works and (3 = GetLocaleInfo(LOCALE_USER_DEFAULT,
    LOCALE_SISO3166CTRYNAME, CountryName,
    SizeOf(CountryName)));
  if works then
  begin
    // Windows 98, Me, NT4, 2000, XP and newer
    LangCode := PChar(@LanguageName[0]) + '_' + PChar(@CountryName[0]);
  end
  else
  begin
    // This part should only happen on Windows 95.
    langid := GetThreadLocale;
    case langid of
      IDBelgianDutch: LangCode := 'nl_BE';
      IDBelgianFrench: LangCode := 'fr_BE';
      IDBrazilianPortuguese: LangCode := 'pt_BR';
      IDDanish: LangCode := 'da_DK';
      IDDutch: LangCode := 'nl_NL';
      IDEnglishUK: LangCode := 'en_GB';
      IDEnglishUS: LangCode := 'en_US';
      IDFinnish: LangCode := 'fi_FI';
      IDFrench: LangCode := 'fr_FR';
      IDFrenchCanadian: LangCode := 'fr_CA';
      IDGerman: LangCode := 'de_DE';
      IDGermanLuxembourg: LangCode := 'de_LU';
      IDGreek: LangCode := 'gr_GR';
      IDIcelandic: LangCode := 'is_IS';
      IDItalian: LangCode := 'it_IT';
      IDKorean: LangCode := 'ko_KO';
      IDNorwegianBokmol: LangCode := 'nb_NO';
      IDNorwegianNynorsk: LangCode := 'nn_NO';
      IDPolish: LangCode := 'pl_PL'; 
      IDPortuguese: LangCode := 'pt_PT'; 
      IDRussian: LangCode := 'ru_RU'; 
      IDSpanish, IDSpanishModernSort: LangCode := 'es_ES'; 
      IDSwedish: LangCode := 'sv_SE';
      IDSwedishFinland: LangCode := 'sv_FI';
    else
      LangCode := 'C';
    end; 
  end; 
  Result := LangCode; 
end; 
{$endif}

{$ifdef CLR}
function GetOSLanguage: string;
var
  p: Integer;
begin
  Result := CultureInfo.get_CurrentCulture.ToString;
  p := Pos('-', Result);
  if p <> 0 then
    Result[p] := '_';
end;
{$endif}

{$ifdef LINUX}
function GetOSLanguage: AnsiString;
begin
  Result := '';
end;
{$endif}

{$ifndef CLR}
function LoadResStringA(ResStringRec: PResStringRec): string;
begin
  if DefaultInstance <> nil then
    Result := DefaultInstance.LoadResString(ResStringRec)
  else
    Result := PChar(ResStringRec.Identifier);
end;
{$endif}

function GetTranslatorNameAndEmail: WideString;
begin
  Result := DefaultInstance.GetTranslatorNameAndEmail;
end;

procedure UseLanguage(const LanguageCode: string);
begin
  DefaultInstance.UseLanguage(LanguageCode);
end;

{$ifndef CLR}
type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: AnsiString;
  end;

function SysUtilsEnumStringModules(Instance: Longint; Data: Pointer): Boolean;
{$ifdef MSWINDOWS}
var
  Buffer: array[0..1023] of AnsiChar;
begin
  with PStrData(Data)^ do
  begin
    SetString(Str, Buffer,
      LoadString(Instance, Ident, Buffer, SizeOf(Buffer)));
    Result := Str = '';
  end;
end;
{$endif}
{$ifdef LINUX}
var
  rs: TResStringRec;
  Module: HModule;
begin
  Module := Instance;
  rs.Module := @Module;
  with PStrData(Data)^ do
  begin
    rs.Identifier := Ident;
    Str := System.LoadResString(@rs);
    Result := Str = '';
  end;
end;
{$endif}

function SysUtilsFindStringResource(Ident: Integer): AnsiString; 
var
  StrData: TStrData;
begin
  StrData.Ident := Ident; 
  StrData.Str := ''; 
  EnumResourceModules(SysUtilsEnumStringModules, @StrData); 
  Result := StrData.Str; 
end; 

function SysUtilsLoadStr(Ident: Integer): AnsiString; 
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln('SysUtils.LoadRes(' + IntToStr(ident) + ') called');
  {$endif}
  Result := ResourceStringGettext(SysUtilsFindStringResource(Ident)); 
end; 

function SysUtilsFmtLoadStr(Ident: Integer; const Args: array of const): AnsiString; 
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln('SysUtils.FmtLoadRes(' + IntToStr(ident) + ', Args) called'); 
  {$endif}
  FmtStr(Result, SysUtilsFindStringResource(Ident), Args); 
  Result := ResourceStringGettext(Result); 
end; 

function LoadResString(ResStringRec: PResStringRec): WideString;
begin
  Result := DefaultInstance.LoadResString(ResStringRec);
end;

function LoadResStringW(ResStringRec: PResStringRec): WideString;
begin
  Result := DefaultInstance.LoadResString(ResStringRec);
end;
{$endif}


function GetCurrentLanguage: string; 
begin
  Result := DefaultInstance.GetCurrentLanguage;
end; 

{ TDomain }

procedure TDomain.CloseMoFile;
begin
  if moFile <> nil then 
    FileLocator.ReleaseMoFile(moFile);
  OpenHasFailedBefore := False;
end; 

destructor TDomain.Destroy; 
begin
  CloseMoFile; 
  inherited Destroy; 
end;

{$ifdef MSWINDOWS}
{not used}
{
function GetLastWinError: AnsiString;
var
  ErrCode: Cardinal;
begin
  SetLength(Result, 2000);
  ErrCode := GetLastError();
  Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrCode,
    0, PChar(Result), 2000, nil);
  Result := StrPas(PChar(Result));
end;
} 
{$endif}

procedure TDomain.OpenMoFile;
const
  ErrorMsg = 'The translation for the language code %s (in %s) does not have ' +
    'charset=utf-8 in its Content-Type. Translations are turned off.';
var
  Filename: string;
begin
  // Check if it is already open
  if moFile <> nil then
    Exit;

  // Check if it has been attempted to open the file before
  if OpenHasFailedBefore then
    Exit;

  if SpecificFilename <> '' then
    Filename := SpecificFilename
  else
  begin
    Filename := Directory + curlang + PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
    if (not FileLocator.FileExists(Filename)) and (not FileExists(Filename)) then
      Filename := Directory + Copy(curlang, 1, 2) + PathDelim +
        'LC_MESSAGES' + PathDelim + domain + '.mo';
  end;
  if (not FileLocator.FileExists(Filename)) and (not FileExists(Filename)) then
  begin
    OpenHasFailedBefore := True;
    Exit;
  end;
  moFile := FileLocator.GetMoFile(Filename, DebugLogger);

  {$ifdef DXGETTEXTDEBUG}
  if moFile.isSwappedArchitecture then
    DebugLogger('.mo file is swapped (comes from another CPU architecture)');
  {$endif}

  // Check, that the contents of the file is utf-8
  if Pos('CHARSET=UTF-8', UpperCase(GetTranslationProperty('Content-Type'))) = 0 then
  begin
    CloseMoFile;
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger(Format(ErrorMsg, [curlang, Filename]));
    {$endif}
    {$ifdef MSWINDOWS}
    MessageBox(0, PChar(Format(ErrorMsg, [curlang, Filename])),
      'Localization problem', MB_OK);
    {$endif}
    {$ifdef CLR}
    MessageBox.show(Format(ErrorMsg, [curlang, Filename]));
    {$endif}
    {$ifdef LINUX}
    WriteLn(stderr, Format(ErrorMsg, [curlang, Filename]));
    {$endif}
    Enabled := False;
  end;
end;

function TDomain.GetTranslationProperty(PropertyName: string): WideString;
var
  sl: TStringList;
  i, PropLen: Integer;
  s: string;
begin
  PropertyName := PropertyName + ': ';
  PropLen := Length(PropertyName) + 1;
  sl := TStringList.Create;
  try
    {$ifdef CLR}
    s := gettext('');
    if Pos(sLineBreak, s) = 0 then
      sl.LineBreak := #10
    else
      sl.LineBreak := sLineBreak;
    sl.Text := s;
    {$else}
    sl.Text := Utf8Encode(gettext(''));
    {$endif}
    for i := 0 to sl.Count - 1 do
    begin
      s := sl.Strings[i];
      if StartsWith(s, PropertyName, True) then
      begin
        {$ifdef CLR}
        Result := TrimCopy(s, PropLen, MaxInt);
        {$else}
        Result := Utf8Decode(TrimCopy(s, PropLen, MaxInt));
        {$endif}
        {$ifdef DXGETTEXTDEBUG}
        DebugLogger('GetTranslationProperty(' + PropertyName + ') returns ''' + Result + '''.');
        {$endif}
        Exit;
      end;
    end;
  finally
    sl.Free;
  end; 
  Result := ''; 
  {$ifdef DXGETTEXTDEBUG}
  DebugLogger('GetTranslationProperty(' + PropertyName +
    ') did not find any value. An empty string is returned.'); 
  {$endif}
end; 

procedure TDomain.SetDirectory(const Value: string);
begin
  vDirectory := IncludeTrailingPathDelimiter(Value);
  SpecificFilename := ''; 
  CloseMoFile; 
end; 

procedure AddDomainForResourceString(const domain: string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln('Extra domain for resourcestring: ' + domain); 
  {$endif}
  ResourceStringDomainListCS.BeginWrite; 
  try
    if ResourceStringDomainList.IndexOf(domain) = -1 then
      ResourceStringDomainList.Add(domain);
  finally
    ResourceStringDomainListCS.EndWrite;
  end;
end;

procedure RemoveDomainForResourceString(const domain: string);
var
  i: Integer;
begin
  {$ifdef DXGETTEXTDEBUG}
  DefaultInstance.DebugWriteln('Remove domain for resourcestring: ' + domain);
  {$endif}
  ResourceStringDomainListCS.BeginWrite;
  try
    i := ResourceStringDomainList.IndexOf(domain);
    if i <> -1 then
      ResourceStringDomainList.Delete(i);
  finally
    ResourceStringDomainListCS.EndWrite;
  end;
end;

procedure TDomain.SetLanguageCode(const LangCode: string);
begin
  CloseMoFile;
  curlang := LangCode;
end; 

function GetPluralForm2EN(Number: Integer): Integer; 
begin
  Number := abs(Number); 
  if Number = 1 then Result := 0 
  else 
    Result := 1; 
end; 

function GetPluralForm1(Number: Integer): Integer; 
begin
  Result := 0; 
end; 

function GetPluralForm2FR(Number: Integer): Integer; 
begin
  Number := abs(Number); 
  if (Number = 1) or (Number = 0) then Result := 0 
  else 
    Result := 1; 
end; 

function GetPluralForm3LV(Number: Integer): Integer; 
begin
  Number := abs(Number); 
  if (Number mod 10 = 1) and (Number mod 100 <> 11) then
    Result := 0
  else if Number <> 0 then Result := 1
  else 
    Result := 2; 
end; 

function GetPluralForm3GA(Number: Integer): Integer; 
begin
  Number := abs(Number); 
  if Number = 1 then Result := 0
  else if Number = 2 then Result := 1
  else 
    Result := 2; 
end; 

function GetPluralForm3LT(Number: Integer): Integer; 
var
  n1, n2: Byte; 
begin
  Number := abs(Number); 
  n1 := Number mod 10; 
  n2 := Number mod 100; 
  if (n1 = 1) and (n2 <> 11) then
    Result := 0
  else if (n1 >= 2) and ((n2 < 10) or (n2 >= 20)) then Result := 1
  else 
    Result := 2; 
end; 

function GetPluralForm3PL(Number: Integer): Integer; 
var
  n1, n2: Byte; 
begin
  Number := abs(Number); 
  n1 := Number mod 10; 
  n2 := Number mod 100; 
  if n1 = 1 then Result := 0
  else if (n1 >= 2) and (n1 <= 4) and ((n2 < 10) or (n2 >= 20)) then Result := 1
  else 
    Result := 2; 
end; 

function GetPluralForm3RU(Number: Integer): Integer; 
var
  n1, n2: Byte; 
begin
  Number := abs(Number);
  n1 := Number mod 10; 
  n2 := Number mod 100; 
  if (n1 = 1) and (n2 <> 11) then
    Result := 0
  else if (n1 >= 2) and (n1 <= 4) and ((n2 < 10) or (n2 >= 20)) then Result := 1
  else 
    Result := 2; 
end; 

function GetPluralForm4SL(Number: Integer): Integer; 
var
  n2: Byte; 
begin
  Number := abs(Number);
  n2 := Number mod 100;
  if n2 = 1 then Result := 0
  else if n2 = 2 then Result := 1
  else if (n2 = 3) or (n2 = 4) then Result := 2
  else
    Result := 3;
end;

procedure TDomain.GetListOfLanguages(List: TStrings);
var
  sr: TSearchRec;
  more: Boolean;
  Filename, Path, LangCode: AnsiString;
  i, j: Integer;
begin
  List.Clear;

  // Iterate through filesystem
  more := FindFirst(Directory + '*', faAnyFile, sr) = 0;
  while more do
  begin
    if (sr.Attr and faDirectory <> 0) and (sr.Name <> '.') and (sr.Name <> '..') then
    begin
      Filename := Directory + sr.Name + PathDelim + 'LC_MESSAGES' +
        PathDelim + domain + '.mo';
      if FileExists(Filename) then
      begin
        LangCode := LowerCase(sr.Name);
        if List.IndexOf(LangCode) = -1 then
          List.Add(LangCode);
      end;
    end;
    more := FindNext(sr) = 0;
  end;

  // Iterate through embedded files
  for i := 0 to FileLocator.FileList.Count - 1 do
  begin
    Filename := FileLocator.BaseDirectory + FileLocator.FileList.Strings[i];
    if IsInDirStrOf(Filename, Directory) then
    begin
      j := Length(Directory);
      Path := PathDelim + 'LC_MESSAGES' + PathDelim + domain + '.mo';
      if EndsWithFilename(Filename, Path) then
      begin
        LangCode := LowerCase(Copy(Filename, j + 1, Length(Filename) - Length(Path) - j));
        if List.IndexOf(LangCode) = -1 then
          List.Add(LangCode);
      end;
    end;
  end;
end;

procedure TDomain.SetFilename(const Filename: string);
begin
  CloseMoFile;
  vDirectory := '';
  SpecificFilename := Filename;
end;

function TDomain.gettext(const msgid: AnsiString): AnsiString;
var
  found: Boolean;
begin
  if not Enabled then
  begin
    Result := msgid; 
    Exit; 
  end; 
  if (moFile = nil) and (not OpenHasFailedBefore) then
    OpenMoFile; 
  if moFile = nil then 
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugLogger('.mo file is not open. Not translating "' + msgid + '"'); 
    {$endif}
    Result := msgid; 
  end 
  else 
  begin
    Result := moFile.gettext(msgid, found); 
    {$ifdef DXGETTEXTDEBUG}
    if found then
      DebugLogger('Found in .mo (' + Domain + '): "' + Utf8Encode(msgid) +
        '"->"' + Utf8Encode(Result) + '"')
    else
      DebugLogger('Translation not found in .mo file (' + Domain +
        ') : "' + Utf8Encode(msgid) + '"'); 
    {$endif}
  end; 
end; 

constructor TDomain.Create;
begin
  inherited Create;
  Enabled := True;
end;

{ TGnuGettextInstance }

procedure TGnuGettextInstance.bindtextdomain(const szDomain, szDirectory: string);
var
  dir: string;
begin
  dir := IncludeTrailingPathDelimiter(szDirectory);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Text domain "' + szDomain + '" is now located at "' + dir + '"'); 
  {$endif}
  getdomain(szDomain, DefaultDomainDirectory, CurLang).Directory := dir; 
  WhenNewDomainDirectory(szDomain, szDirectory); 
end;

constructor TGnuGettextInstance.Create;
begin
  inherited Create;
  {$ifndef CLR}
  CreatorThread := GetCurrentThreadId;
  { TODO : Do something about Thread handling if resourcestrings are enabled }
  {$endif}
  {$ifdef MSWINDOWS}
  DesignTimeCodePage := CP_ACP;
  {$endif}
  {$ifdef DXGETTEXTDEBUG}
  DebugLogCS := TMultiReadExclusiveWriteSynchronizer.Create;
  DebugLog := TMemoryStream.Create;
  DebugWriteln('Debug log started ' + DateTimeToStr(Now));
  DebugWriteln('');
  {$endif}
  curGetPluralForm := GetPluralForm2EN;
  Enabled := True;
  curmsgdomain := DefaultTextDomain;
  savefileCS := TMultiReadExclusiveWriteSynchronizer.Create;
  DomainList := TStringList.Create;
  TP_IgnoreList := TStringList.Create;
  TP_IgnoreList.Sorted := True;
  TP_GlobalClassHandling := TObjectList.Create;
  TP_ClassHandling := TObjectList.Create;

  // Set some settings
  DefaultDomainDirectory := IncludeTrailingPathDelimiter(extractfilepath(ExecutableFilename))
    + 'locale';

  UseLanguage('');

  bindtextdomain(DefaultTextDomain, DefaultDomainDirectory); 
  TextDomain(DefaultTextDomain); 

  // Add default properties to ignore
  TP_GlobalIgnoreClassProperty(TComponent, 'Name'); 
  TP_GlobalIgnoreClassProperty(TCollection, 'PropName'); 
end; 

destructor TGnuGettextInstance.Destroy;
var
  I: Integer;
begin
  if SaveMemory <> nil then
  begin
    savefileCS.BeginWrite;
    try
      CloseFile(savefile);
    finally
      savefileCS.EndWrite;
    end;
    FreeAndNil(SaveMemory);
  end;
  FreeAndNil(savefileCS);
  FreeAndNil(TP_IgnoreList);
  FreeAndNil(TP_GlobalClassHandling);
  FreeAndNil(TP_ClassHandling);
  for I := 0 to DomainList.Count - 1 do
    DomainList.Objects[I].Free;
  FreeAndNil(DomainList);
  {$ifdef DXGETTEXTDEBUG}
  FreeAndNil(DebugLog);
  FreeAndNil(DebugLogCS);
  {$endif}
  inherited Destroy;
end; 

{$ifndef DELPHI5OROLDER}
function TGnuGettextInstance.dgettext(const szDomain: string;
  const szMsgId: AnsiString): WideString;
begin
  Result := dgettext(szDomain, ansi2wide(szMsgId));
end;
{$endif}

function TGnuGettextInstance.dgettext(const szDomain: string;
  const szMsgId: WideString): WideString;
begin
  if not Enabled then 
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('Translation has been disabled. Text is not being translated: ' + szMsgid); 
    {$endif}
    Result := szMsgId; 
  end 
  else 
  begin
    Result := Utf8Decode(LF2LineBreakA(getdomain(szDomain, DefaultDomainDirectory,
      CurLang).gettext(StripCR(Utf8Encode(szMsgId))))); 
    {$ifdef DXGETTEXTDEBUG}
    if (szMsgId <> '') and (Result = '') then
      DebugWriteln(Format('Error: Translation of %s was an empty string. This may never occur.',
        [szMsgId])); 
    {$endif}
  end; 
end; 

function TGnuGettextInstance.GetCurrentLanguage: string;
begin
  Result := curlang;
end;

function TGnuGettextInstance.getcurrenttextdomain: string;
begin
  Result := curmsgdomain; 
end; 

{$ifndef DELPHI5OROLDER}
function TGnuGettextInstance.gettext(const szMsgId: AnsiString): WideString;
begin
  Result := dgettext(curmsgdomain, szMsgId);
end;
{$endif} 

function TGnuGettextInstance.gettext(const szMsgId: WideString): WideString; 
begin
  Result := dgettext(curmsgdomain, szMsgId); 
end; 

procedure TGnuGettextInstance.textdomain(const szDomain: string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Changed text domain to "' + szDomain + '"'); 
  {$endif}
  curmsgdomain := szDomain; 
  WhenNewDomain(szDomain); 
end; 

function TGnuGettextInstance.TP_CreateRetranslator: TExecutable;
var
  ttpr: TTP_Retranslator; 
begin
  ttpr := TTP_Retranslator.Create; 
  ttpr.Instance := self; 
  TP_Retranslator := ttpr; 
  Result := ttpr; 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('A retranslator was created.'); 
  {$endif}
end; 

procedure TGnuGettextInstance.TP_GlobalHandleClass(HClass: TClass; 
  Handler: TTranslator); 
var
  cm: TClassMode; 
  i: Integer; 
begin
  for i := 0 to TP_GlobalClassHandling.Count - 1 do
  begin
    cm := TClassMode(TP_GlobalClassHandling.Items[i]);
    if cm.HClass = HClass then
      raise EGGProgrammingError.Create(
        'You cannot set a handler for a class that has already been assigned otherwise.'); 
    if HClass.InheritsFrom(cm.HClass) then 
    begin
      // This is the place to insert this class
      cm := TClassMode.Create; 
      cm.HClass := HClass; 
      cm.SpecialHandler := Handler; 
      TP_GlobalClassHandling.Insert(i, cm); 
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('A handler was set for class ' + HClass.ClassName + '.'); 
      {$endif}
      Exit; 
    end; 
  end; 
  cm := TClassMode.Create; 
  cm.HClass := HClass; 
  cm.SpecialHandler := Handler; 
  TP_GlobalClassHandling.Add(cm); 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('A handler was set for class ' + HClass.ClassName + '.'); 
  {$endif}
end; 

procedure TGnuGettextInstance.TP_GlobalIgnoreClass(IgnClass: TClass); 
var
  cm: TClassMode; 
  i: Integer; 
begin
  for i := 0 to TP_GlobalClassHandling.Count - 1 do
  begin
    cm := TClassMode(TP_GlobalClassHandling.Items[i]); 
    if cm.HClass = IgnClass then
      raise EGGProgrammingError.Create('You cannot add a class to the ignore List that is already on that List: '
        + IgnClass.ClassName + '. You should keep all TP_Global functions in one place in your source code.'); 
    if IgnClass.InheritsFrom(cm.HClass) then 
    begin
      // This is the place to insert this class
      cm := TClassMode.Create; 
      cm.HClass := IgnClass; 
      TP_GlobalClassHandling.Insert(i, cm); 
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Globally, class ' + IgnClass.ClassName + ' is being ignored.'); 
      {$endif}
      Exit; 
    end; 
  end; 
  cm := TClassMode.Create; 
  cm.HClass := IgnClass; 
  TP_GlobalClassHandling.Add(cm); 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Globally, class ' + IgnClass.ClassName + ' is being ignored.'); 
  {$endif}
end; 

procedure TGnuGettextInstance.TP_GlobalIgnoreClassProperty(IgnClass: TClass;
  const PropertyName: AnsiString);
var
  cm: TClassMode;
  i, idx: Integer;
begin
  for i := 0 to TP_GlobalClassHandling.Count - 1 do
  begin
    cm := TClassMode(TP_GlobalClassHandling.Items[i]);
    if cm.HClass = IgnClass then
    begin
      if Assigned(cm.SpecialHandler) then
        raise EGGProgrammingError.Create(
          'You cannot ignore a class property for a class that has a handler set.');
      if not cm.PropertiesToIgnore.Find(PropertyName, idx) then
        cm.PropertiesToIgnore.Add(PropertyName);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Globally, the ' + PropertyName + ' property of class ' +
        IgnClass.ClassName + ' is being ignored.');
      {$endif}
      Exit;
    end;
    if IgnClass.InheritsFrom(cm.HClass) then
    begin
      // This is the place to insert this class
      cm := TClassMode.Create;
      cm.HClass := IgnClass;
      cm.PropertiesToIgnore.Add(PropertyName);
      TP_GlobalClassHandling.Insert(i, cm);
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Globally, the ' + PropertyName + ' property of class ' +
        IgnClass.ClassName + ' is being ignored.');
      {$endif}
      Exit;
    end;
  end;
  cm := TClassMode.Create;
  cm.HClass := IgnClass;
  cm.PropertiesToIgnore.Add(PropertyName);
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Globally, the ' + PropertyName + ' property of class ' +
    IgnClass.ClassName + ' is being ignored.'); 
  {$endif}
end;

procedure TGnuGettextInstance.TP_Ignore(AnObject: TObject; 
  const Name: AnsiString); 
begin
  TP_IgnoreList.Add(Name); 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('On object with class name ' + AnObject.ClassName +
    ', ignore is set on ' + Name); 
  {$endif}
end; 

procedure TGnuGettextInstance.TranslateComponent(AnObject: TComponent; 
  const TextDomain: string);
var
  Comp: TGnuGettextComponentMarker; 
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('======================================================================'); 
  DebugWriteln('TranslateComponent() was called for a component with name ' +
    AnObject.Name + '.'); 
  {$endif}
  Comp := AnObject.FindComponent('GNUgettextMarker') as TGnuGettextComponentMarker; 
  if Comp = nil then 
  begin
    Comp := TGnuGettextComponentMarker.Create(nil); 
    Comp.Name := 'GNUgettextMarker';
    Comp.Retranslator := TP_CreateRetranslator;
    TranslateProperties(AnObject, TextDomain); 
    AnObject.InsertComponent(Comp); 
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln(
      'This is the first time, that this component has been translated. A retranslator component has been created for this component.'); 
    {$endif}
  end 
  else 
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('This is not the first time, that this component has been translated.'); 
    {$endif}
    if Comp.LastLanguage <> curlang then 
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln(
        'ERROR: TranslateComponent() was called twice with different languages. This indicates an attempt to switch language at runtime, but by using TranslateComponent every time. This API has changed - please use RetranslateComponent() instead.'
        ); 
      {$endif}
      {$ifdef MSWINDOWS}
      MessageBox(0,
        'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.', 'Error', MB_OK);
      {$endif}
      {$ifdef CLR}
      MessageBox.show('This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.');
      {$endif}
      {$ifdef LINUX}
      WriteLn(stderr,
        'This application tried to switch the language, but in an incorrect way. The programmer needs to replace a call to TranslateComponent with a call to RetranslateComponent(). The programmer should see the changelog of gnugettext.pas for more information.'); 
      {$endif}
    end 
    else
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln(
        'ERROR: TranslateComponent has been called twice, but with the same language chosen. This is a mistake, but in order to prevent that the application breaks, no exception is raised.'
        ); 
      {$endif}
    end; 
  end; 
  Comp.LastLanguage := curlang; 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('======================================================================'); 
  {$endif}
end; 

{$ifndef CLR}
procedure TGnuGettextInstance.TranslateProperty(AnObject: TObject;
  PropInfo: PPropInfo; TodoList: TStrings; const TextDomain: AnsiString); 
var
  {$ifdef DELPHI5OROLDER}
  ws: AnsiString;
  old: AnsiString;
  {$endif}
  {$ifndef DELPHI5OROLDER}
  ppi: PPropInfo;
  ws: WideString; 
  old: WideString;
  {$endif} 
  obj: TObject; 
  Propname: AnsiString; 
begin
  PropName := PropInfo^.Name; 
  try
    // Translate certain types of properties
    case PropInfo^.PropType^.Kind of
      tkString, tkLString, tkWString:
        begin
          {$ifdef DXGETTEXTDEBUG}
          DebugWriteln('Translating ' + AnObject.ClassName + '.' + PropName); 
          {$endif}
          {$ifdef DELPHI5OROLDER}
          old := GetStrProp(AnObject, PropName);
          {$endif}
          {$ifndef DELPHI5OROLDER}
          if PropInfo^.PropType^.Kind <> tkWString then
            old := ansi2wide(GetStrProp(AnObject, PropName))
          else
            old := GetWideStrProp(AnObject, PropName);
          {$endif}
          {$ifdef DXGETTEXTDEBUG}
          if old = '' then
            DebugWriteln('(Empty, not translated)')
          else
            DebugWriteln('Old value: "' + old + '"');
          {$endif}
          if (old <> '') and (IsWriteProp(PropInfo)) then
          begin
            if TP_Retranslator <> nil then
              TTP_Retranslator(TP_Retranslator).Remember(AnObject, PropName, old);
            ws := dgettext(TextDomain, old);
            if ws <> old then
            begin
              {$ifdef DELPHI5OROLDER}
              SetStrProp(AnObject, PropName, ws);
              {$endif}
              {$ifndef DELPHI5OROLDER}
              ppi := GetPropInfo(AnObject, Propname);
              if ppi <> nil then
              begin
                SetWideStrProp(AnObject, ppi, ws);
              end
              else
              begin
                {$ifdef DXGETTEXTDEBUG}
                DebugWriteln('ERROR: Property disappeared: ' + Propname +
                  ' for object of type ' + AnObject.ClassName);
                {$endif}
              end;
              {$endif}
            end;
          end;
        end { case item };
      tkClass:
        begin
          obj := GetObjectProp(AnObject, PropName); 
          if obj <> nil then
            TodoList.AddObject('', obj); 
        end { case item }; 
    end { case }; 
  except
    on E: Exception do
      raise EGGComponentError.Create('Property cannot be translated.' + sLineBreak +
        'Add TP_GlobalIgnoreClassProperty(' + AnObject.ClassName +
        ', ''' + PropName + ''') to your source code or use' + sLineBreak +
        'TP_Ignore (self, ''.' + PropName + ''') to prevent this message.' + sLineBreak +
        'Reason: ' + e.Message);
  end;
end;
{$endif}

procedure TGnuGettextInstance.TranslateProperties(AnObject: TObject;
  TextDomain: string = '');
{$ifndef CLR}
var
  TodoList: TStringList; // List of Name/TObject's that is to be processed
  DoneList: TStringList;
  // List of hex codes representing pointers to objects that have been done
  i, j, Count: Integer;
  PropList: PPropList;
  UPropName: AnsiString;
  PropInfo: PPropInfo;
  Comp: TComponent;
  cm, currentcm: TClassMode;
  ObjectPropertyIgnoreList: TStringList;
  objid, Name: AnsiString;
{$endif}
begin
  {$ifndef CLR}
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('----------------------------------------------------------------------');
  DebugWriteln('TranslateProperties() was called for an object of class ' +
    AnObject.ClassName + ' with domain "' + TextDomain + '".');
  {$endif}
  if TextDomain = '' then
    TextDomain := curmsgdomain;
  if TP_Retranslator <> nil then
    TTP_Retranslator(TP_Retranslator).TextDomain := TextDomain;
  DoneList := TStringList.Create;
  TodoList := TStringList.Create;
  ObjectPropertyIgnoreList := TStringList.Create;
  try
    TodoList.AddObject('', AnObject);
    DoneList.Sorted := True;
    ObjectPropertyIgnoreList.Sorted := True;
    ObjectPropertyIgnoreList.Duplicates := dupIgnore;
    {$ifndef DELPHI5OROLDER}
    ObjectPropertyIgnoreList.CaseSensitive := False;
    DoneList.Duplicates := dupError;
    DoneList.CaseSensitive := True;
    {$endif}

    while TodoList.Count <> 0 do
    begin
      AnObject := TodoList.Objects[0];
      Name := TodoList.Strings[0];
      TodoList.Delete(0); 
      if (AnObject <> nil) and (AnObject is TPersistent) then
      begin
        // Make sure each object is only translated once
        Assert(SizeOf(Integer) = SizeOf(TObject));
        objid := IntToHex(Integer(AnObject), 8);
        if DoneList.Find(objid, i) then
        begin
          Continue;
        end
        else
        begin
          DoneList.Add(objid);
        end;

        ObjectPropertyIgnoreList.Clear;

        // Find out if there is special handling of this object
        currentcm := nil;
        // First check the local handling instructions
        for j := 0 to TP_ClassHandling.Count - 1 do
        begin
          cm := TClassMode(TP_ClassHandling.Items[j]);
          if AnObject.InheritsFrom(cm.HClass) then
          begin
            if cm.PropertiesToIgnore.Count <> 0 then
            begin
              ObjectPropertyIgnoreList.AddStrings(cm.PropertiesToIgnore);
            end
            else
            begin
              // Ignore the entire class
              currentcm := cm;
              Break;
            end;
          end;
        end;
        // Then check the global handling instructions
        if currentcm = nil then
          for j := 0 to TP_GlobalClassHandling.Count - 1 do
          begin
            cm := TClassMode(TP_GlobalClassHandling.Items[j]);
            if AnObject.InheritsFrom(cm.HClass) then
            begin
              if cm.PropertiesToIgnore.Count <> 0 then
              begin
                ObjectPropertyIgnoreList.AddStrings(cm.PropertiesToIgnore);
              end
              else
              begin
                // Ignore the entire class
                currentcm := cm;
                Break;
              end;
            end;
          end;
        if currentcm <> nil then
        begin
          ObjectPropertyIgnoreList.Clear;
          // Ignore or use special handler
          if Assigned(currentcm.SpecialHandler) then
          begin
            currentcm.SpecialHandler(AnObject);
            {$ifdef DXGETTEXTDEBUG}
            DebugWriteln('Special handler activated for ' + AnObject.ClassName);
            {$endif}
          end
          else
          begin
            {$ifdef DXGETTEXTDEBUG}
            DebugWriteln('Ignoring object ' + AnObject.ClassName);
            {$endif}
          end;
          Continue;
        end;
        Count := GetPropList(AnObject, PropList);
        try
          for j := 0 to Count - 1 do
          begin
            PropInfo := PropList[j];
            UPropName := PropInfo^.Name;
            // Ignore properties that are meant to be ignored
            if ((currentcm = nil) or (not currentcm.PropertiesToIgnore.Find(UPropName, i)))
              and
              (not TP_IgnoreList.Find(Name + '.' + UPropName, i)) and
              (not ObjectPropertyIgnoreList.Find(UPropName, i)) then
            begin
              TranslateProperty(AnObject, PropInfo, TodoList, TextDomain);
            end;  // if
          end;  // for
        finally
          if Count <> 0 then
            FreeMem(PropList);
        end;
        if AnObject is TStrings then
        begin
          if (TStrings(AnObject).Count > 0) and (TP_Retranslator <> nil) then
            TTP_Retranslator(TP_Retranslator).Remember(AnObject,
              'Text', TStrings(AnObject).Text);
          TranslateStrings(TStrings(AnObject), TextDomain);
        end;
        // Check for TCollection
        if AnObject is TCollection then
        begin
          for i := 0 to TCollection(AnObject).Count - 1 do
            TodoList.AddObject('', TCollection(AnObject).Items[i]);
        end;
        if AnObject is TComponent then
        begin
          for i := 0 to TComponent(AnObject).ComponentCount - 1 do
          begin
            Comp := TComponent(AnObject).Components[i];
            if (not TP_IgnoreList.Find(Comp.Name, j)) then
            begin
              TodoList.AddObject(Comp.Name, Comp);
            end;
          end;
        end;
      end { if AnObject <> nil };
    end { while TodoList.count <> 0 };
  finally
    TodoList.Free;
    ObjectPropertyIgnoreList.Free;
    DoneList.Free;
  end;
  TP_ClassHandling.Clear; // deletes the objects
  TP_IgnoreList.Clear;
  TP_Retranslator := nil;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('----------------------------------------------------------------------');
  {$endif}
  {$endif}
end;

procedure TGnuGettextInstance.UseLanguage(LanguageCode: string);
var
  i, p: Integer;
  dom: TDomain;
  l2: string[2];
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('UseLanguage(''' + LanguageCode + '''); called');
  {$endif}

  if LanguageCode = '' then
  begin
    LanguageCode := GGGetEnvironmentVariable('LANG');
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('LANG env variable is ''' + LanguageCode + '''.');
    {$endif}
    if LanguageCode = '' then
    begin
      LanguageCode := GetOSLanguage;
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Found OS language code to be ''' + LanguageCode + '''.');
      {$endif}
    end;
    p := Pos('.', LanguageCode);
    if p <> 0 then
      Delete(LanguageCode, p, MaxInt);
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('Language code that will be set is ''' + LanguageCode + '''.');
    {$endif}
  end;

  curlang := LanguageCode;
  for i := 0 to DomainList.Count - 1 do
  begin
    dom := TDomain(DomainList.Objects[i]);
    dom.SetLanguageCode(curlang); 
  end; 

  l2 := LowerCase(Copy(curlang, 1, 2));
  if (l2 = 'en') or (l2 = 'de') then curGetPluralForm := GetPluralForm2EN
  else if (l2 = 'hu') or (l2 = 'ko') or (l2 = 'zh') or (l2 = 'ja') or (l2 = 'tr') then
    curGetPluralForm := GetPluralForm1
  else if (l2 = 'fr') or (l2 = 'fa') or (LowerCase(curlang) = 'pt_br') then
    curGetPluralForm := GetPluralForm2FR
  else if (l2 = 'lv') then curGetPluralForm := GetPluralForm3LV
  else if (l2 = 'ga') then curGetPluralForm := GetPluralForm3GA
  else if (l2 = 'lt') then curGetPluralForm := GetPluralForm3LT
  else if (l2 = 'ru') or (l2 = 'cs') or (l2 = 'sk') or (l2 = 'uk') or (l2 = 'hr') then
    curGetPluralForm := GetPluralForm3RU
  else if (l2 = 'pl') then curGetPluralForm := GetPluralForm3PL
  else if (l2 = 'sl') then curGetPluralForm := GetPluralForm4SL
  else
  begin
    curGetPluralForm := GetPluralForm2EN;
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('Plural form for the language was not found. English plurality System assumed.');
    {$endif}
  end;

  WhenNewLanguage(curlang);

  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('');
  {$endif}
end;

procedure TGnuGettextInstance.TranslateStrings(sl: TStrings; const TextDomain: string);
var
  Line: AnsiString;
  i: Integer;
  TempList: TStringList;
begin
  if sl.Count > 0 then
  begin
    sl.BeginUpdate;
    try
      TempList := TStringList.Create;
      try
        TempList.Assign(sl);
        for i := 0 to TempList.Count - 1 do
        begin
          Line := TempList.Strings[i];
          if Line <> '' then
            TempList.Strings[i] := dgettext(TextDomain, Line);
        end;
        sl.Assign(TempList);
      finally
        TempList.Free;
      end;
{
      for i := 0 to sl.Count - 1 do
      begin
        Line := sl.Strings[i];
        if Line <> '' then
          sl.Strings[i] := dgettext(TextDomain, Line);
      end;
      }
    finally
      sl.EndUpdate;
    end;
  end;
end;

function TGnuGettextInstance.GetTranslatorNameAndEmail: WideString;
begin
  Result := GetTranslationProperty('LAST-TRANSLATOR'); 
end; 

function TGnuGettextInstance.GetTranslationProperty(const PropertyName: AnsiString): WideString;
begin
  Result := getdomain(curmsgdomain, DefaultDomainDirectory,
    CurLang).GetTranslationProperty(PropertyName);
end;

function TGnuGettextInstance.dngettext(const szDomain: string;
  singular, plural: WideString;
  Number: Integer): WideString;
var
  org, trans: WideString; 
  idx: Integer; 
  p: Integer; 
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('dngettext translation (domain ' + szDomain + ', number is ' +
    IntToStr(Number) + ') of ' + singular + '/' + plural);
  {$endif}
  org := singular + #0 + plural; 
  trans := dgettext(szDomain, org); 
  if org = trans then 
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('Translation was equal to english version. English plural forms assumed.'); 
    {$endif}
    idx := GetPluralForm2EN(Number)
  end 
  else
    idx := curGetPluralForm(Number); 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Index ' + IntToStr(idx) + ' will be used'); 
  {$endif}
  while True do 
  begin
    p := Pos(#0, string(trans)); 
    if p = 0 then 
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Last translation used: ' + Utf8Encode(trans)); 
      {$endif}
      Result := trans; 
      Exit; 
    end; 
    if idx = 0 then 
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Translation found: ' + Utf8Encode(trans)); 
      {$endif}
      Result := Copy(trans, 1, p - 1);
      Exit; 
    end; 
    Delete(trans, 1, p); 
    Dec(idx); 
  end; 
end; 

{$ifndef DELPHI5OROLDER}
function TGnuGettextInstance.ngettext(const singular, plural: AnsiString;
  Number: Integer): WideString;
begin
  Result := dngettext(curmsgdomain, singular, plural, Number);
end;
{$endif}

function TGnuGettextInstance.ngettext(const singular, plural: WideString; 
  Number: Integer): WideString; 
begin
  Result := dngettext(curmsgdomain, singular, plural, Number); 
end; 

procedure TGnuGettextInstance.WhenNewDomain(const TextDomain: string);
begin
  // This is meant to be empty.
end;

procedure TGnuGettextInstance.WhenNewLanguage(const LanguageID: AnsiString);
begin
  // This is meant to be empty.
end; 

procedure TGnuGettextInstance.WhenNewDomainDirectory(const TextDomain,
  Directory: string);
begin
  // This is meant to be empty.
end;

procedure TGnuGettextInstance.GetListOfLanguages(const domain: string;
  List: TStrings);
begin
  getdomain(Domain, DefaultDomainDirectory, CurLang).GetListOfLanguages(List);
end; 

procedure TGnuGettextInstance.bindtextdomainToFile(const szDomain, Filename: string);
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Text domain "' + szDomain + '" is now bound to file named "' + Filename + '"'); 
  {$endif}
  getdomain(szDomain, DefaultDomainDirectory, CurLang).SetFilename(Filename); 
end; 

procedure TGnuGettextInstance.DebugLogPause(PauseEnabled: Boolean); 
begin
  DebugLogOutputPaused := PauseEnabled; 
end; 

procedure TGnuGettextInstance.DebugLogToFile(const Filename: string; append: Boolean = False); 
var
  fs: TFileStream;
  marker: AnsiString;
begin
  // Create the file if needed
  if (not FileExists(Filename)) or (not append) then
    FileClose(FileCreate(Filename)); 

  // Open file
  fs := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite); 
  if append then
    fs.Seek(0, soFromEnd); 

  // Write header if appending
  if fs.Position <> 0 then 
  begin
    marker := sLineBreak +
      '===========================================================================' + sLineBreak; 
    fs.WriteBuffer(marker[1], Length(marker)); 
  end; 

  // Copy the memorystream contents to the file
  DebugLog.Seek(0, soFromBeginning); 
  fs.CopyFrom(DebugLog, 0); 

  // Make DebugLog point to the filestream
  FreeAndNil(DebugLog); 
  DebugLog := fs; 
end; 

procedure TGnuGettextInstance.DebugWriteln(Line: AnsiString);
var
  Discard: Boolean;
begin
  Assert(DebugLogCS <> nil);
  Assert(DebugLog <> nil); 

  DebugLogCS.BeginWrite; 
  try
    if DebugLogOutputPaused then
      Exit; 

    if Assigned(fOnDebugLine) then 
    begin
      Discard := True; 
      fOnDebugLine(Self, Line, Discard); 
      if Discard then Exit; 
    end; 

    Line := Line + sLineBreak; 

    // Ensure that memory usage doesn't get too big.
    if (DebugLog is TMemoryStream) and (DebugLog.Position > 1000000) then 
    begin
      Line := sLineBreak + sLineBreak + sLineBreak + sLineBreak + sLineBreak +
        'Debug log halted because memory usage grew too much.' + sLineBreak +
        'Specify a Filename to store the debug log in or disable debug loggin in gnugettext.pas.' +
        sLineBreak + sLineBreak + sLineBreak + sLineBreak + sLineBreak; 
      DebugLogOutputPaused := True; 
    end; 
    DebugLog.WriteBuffer(Line[1], Length(Line)); 
  finally
    DebugLogCS.EndWrite; 
  end; 
end; 

function TGnuGettextInstance.Getdomain(const domain, DefaultDomainDirectory,
  CurLang: string): TDomain;
  // Retrieves the TDomain object for the specified domain.
  // Creates one, if none there, yet.
var
  idx: Integer;
begin
  idx := DomainList.IndexOf(Domain);
  if idx = -1 then
  begin
    Result := TDomain.Create;
    Result.DebugLogger := DebugWriteln;
    Result.Domain := Domain;
    Result.Directory := DefaultDomainDirectory;
    Result.SetLanguageCode(curlang);
    DomainList.AddObject(Domain, Result);
  end
  else
  begin
    Result := TDomain(DomainList.Objects[idx]);
  end;
end;

{$ifndef CLR}
function TGnuGettextInstance.LoadResString(ResStringRec: PResStringRec): WideString;
{$ifdef MSWINDOWS}
var
  Len: Integer;
  Buffer: array[0..1023] of AnsiChar;
{$endif}
{$ifdef LINUX}
const
  ResStringTableLen = 16;
type
  ResStringTable = array[0..ResStringTableLen - 1] of Longword;
var
  Handle: TResourceHandle;
  Tab: ^ResStringTable;
  ResMod: HMODULE;
{$endif}
begin
  if (ResStringRec = nil) or (Self = nil) then
    Exit;
  if ResStringRec.Identifier >= 64 * 1024 then
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('LoadResString was given an invalid ResStringRec.Identifier');
    {$endif}
    Result := PChar(ResStringRec.Identifier);
    Exit;
  end
  else
  begin
    {$ifdef LINUX}
    // This works with Unicode if the Linux has utf-8 character set
    // Result := System.LoadResString(ResStringRec);
    ResMod := FindResourceHInstance(ResStringRec^.Module^);
    Handle := FindResource(ResMod,
      PChar(ResStringRec^.Identifier div ResStringTableLen), PChar(6));   // RT_STRING
    Tab := Pointer(LoadResource(ResMod, Handle));
    if Tab = nil then
      Result := ''
    else
      Result := PWideChar(PChar(Tab) + Tab[ResStringRec^.Identifier mod ResStringTableLen]);
    {$endif}
    {$ifdef MSWINDOWS}
    if not Win32PlatformIsUnicode then
    begin
      SetString(Result, Buffer,
        LoadString(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, SizeOf(Buffer)))
    end
    else
    begin
      Result := '';
      Len := 0;
      while Len = Length(Result) do
      begin
        if Length(Result) = 0 then
          SetLength(Result, 1024)
        else
          SetLength(Result, Length(Result) * 2);
        Len := LoadStringW(FindResourceHInstance(ResStringRec.Module^),
          ResStringRec.Identifier, PWideChar(Result), Length(Result));
      end;
      SetLength(Result, Len);
    end;
    {$endif}
  end;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Loaded resourcestring: ' + Utf8Encode(Result));
  {$endif}
  if CreatorThread <> GetCurrentThreadId then
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln('LoadResString was called from an invalid thread. Resourcestring was not translated.');
    {$endif}
  end
  else
    Result := ResourceStringGettext(Result);
end;
{$endif}

procedure TGnuGettextInstance.RetranslateComponent(AnObject: TComponent;
  const TextDomain: string);
var
  Comp: TGnuGettextComponentMarker;
begin
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('======================================================================'); 
  DebugWriteln('RetranslateComponent() was called for a component with name ' +
    AnObject.Name + '.'); 
  {$endif}
  Comp := AnObject.FindComponent('GNUgettextMarker') as TGnuGettextComponentMarker; 
  if Comp = nil then 
  begin
    {$ifdef DXGETTEXTDEBUG}
    DebugWriteln(
      'Retranslate was called on an object that has not been translated before. An Exception is being raised.'); 
    {$endif}
    raise EGGProgrammingError.Create(
      'Retranslate was called on an object that has not been translated before. Please use TranslateComponent() before RetranslateComponent().'); 
  end 
  else 
  begin
    if Comp.LastLanguage <> curlang then 
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('The retranslator is being executed.'); 
      {$endif}
      Comp.Retranslator.Execute; 
    end 
    else 
    begin
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('The language has not changed. The retranslator is not executed.'); 
      {$endif}
    end; 
  end; 
  Comp.LastLanguage := curlang;
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('======================================================================'); 
  {$endif}
end; 

procedure TGnuGettextInstance.TP_IgnoreClass(IgnClass: TClass);
var
  cm: TClassMode;
  i: Integer;
begin
  for i := 0 to TP_ClassHandling.Count - 1 do
  begin
    cm := TClassMode(TP_ClassHandling.Items[i]); 
    if cm.HClass = IgnClass then
      raise EGGProgrammingError.Create('You cannot add a class to the ignore List that is already on that List: '
        + IgnClass.ClassName + '.'); 
    if IgnClass.InheritsFrom(cm.HClass) then 
    begin
      // This is the place to insert this class
      cm := TClassMode.Create; 
      cm.HClass := IgnClass; 
      TP_ClassHandling.Insert(i, cm); 
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Locally, class ' + IgnClass.ClassName + ' is being ignored.'); 
      {$endif}
      Exit; 
    end; 
  end; 
  cm := TClassMode.Create; 
  cm.HClass := IgnClass; 
  TP_ClassHandling.Add(cm); 
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Locally, class ' + IgnClass.ClassName + ' is being ignored.'); 
  {$endif}
end; 

procedure TGnuGettextInstance.TP_IgnoreClassProperty(IgnClass: TClass;
  const PropertyName: AnsiString);
var
  cm: TClassMode; 
  i: Integer; 
begin
  for i := 0 to TP_ClassHandling.Count - 1 do
  begin
    cm := TClassMode(TP_ClassHandling.Items[i]);
    if cm.HClass = IgnClass then
    begin
      if Assigned(cm.SpecialHandler) then
        raise EGGProgrammingError.Create(
          'You cannot ignore a class property for a class that has a handler set.'); 
      cm.PropertiesToIgnore.Add(PropertyName); 
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Globally, the ' + PropertyName + ' property of class ' +
        IgnClass.ClassName + ' is being ignored.'); 
      {$endif}
      Exit; 
    end; 
    if IgnClass.InheritsFrom(cm.HClass) then 
    begin
      // This is the place to insert this class
      cm := TClassMode.Create; 
      cm.HClass := IgnClass; 
      cm.PropertiesToIgnore.Add(PropertyName); 
      TP_ClassHandling.Insert(i, cm); 
      {$ifdef DXGETTEXTDEBUG}
      DebugWriteln('Locally, the ' + PropertyName + ' property of class ' +
        IgnClass.ClassName + ' is being ignored.'); 
      {$endif}
      Exit; 
    end; 
  end; 
  cm := TClassMode.Create; 
  cm.HClass := IgnClass; 
  cm.PropertiesToIgnore.Add(PropertyName);
  TP_GlobalClassHandling.Add(cm);
  {$ifdef DXGETTEXTDEBUG}
  DebugWriteln('Locally, the ' + PropertyName + ' property of class ' +
    IgnClass.ClassName + ' is being ignored.'); 
  {$endif}
end; 

function TGnuGettextInstance.ansi2wide(const s: AnsiString): WideString;
  {$ifdef MSWINDOWS}
var
  len: Integer;
  {$endif}
begin
  {$ifdef MSWINDOWS}
  if DesignTimeCodePage = CP_ACP then
  begin
    // No design-time codepage specified. Using runtime codepage instead.
    {$endif}
    Result := s;
    {$ifdef MSWINDOWS}
  end 
  else 
  begin
    len := Length(s); 
    if len = 0 then
      Result := ''
    else 
    begin
      SetLength(Result, len); 
      len := MultiByteToWideChar(DesignTimeCodePage, 0, PChar(s), len,
        PWideChar(Result), len); 
      if len = 0 then
        raise EGGAnsi2WideConvError.Create('Cannot convert AnsiString to WideString:' +
          sLineBreak + s); 
      SetLength(Result, len); 
    end; 
  end; 
  {$endif}
end; 

{$ifndef DELPHI5OROLDER}
function TGnuGettextInstance.dngettext(const szDomain: string; singular, 
  plural: AnsiString; Number: Integer): WideString;
begin
  Result := dngettext(szDomain, ansi2wide(singular), ansi2wide(plural), Number); 
end; 
{$endif}

{ TClassMode }

constructor TClassMode.Create;
begin
  inherited Create;
  PropertiesToIgnore := TStringList.Create;
  PropertiesToIgnore.Sorted := True;
  PropertiesToIgnore.Duplicates := dupError;
  {$ifndef DELPHI5OROLDER}
  PropertiesToIgnore.CaseSensitive := False;
  {$endif}
end;

destructor TClassMode.Destroy;
begin
  PropertiesToIgnore.Free;
  inherited Destroy;
end;

{ TFileLocator }

procedure TFileLocator.Analyze;
var
  s: AnsiString;
  i: Integer;
  Offset: Int64;
  fs: TFileStream;
  fi: TEmbeddedFileInfo;
  AnsiFilename: AnsiString;
begin
  s := '6637DB2E-62E1-4A60-AC19-C23867046A89'#0#0#0#0#0#0#0#0; // this string is patched in the executable
  s := Copy(s, Length(s) - 7, 8);

  Offset := 0;
  for i := 8 downto 1 do
    Offset := Offset shl 8 + Ord(s[i]);
  if Offset = 0 then
    Exit;
  BaseDirectory := ExtractFilePath(ExecutableFilename);
  try
    fs := TFileStream.Create(ExecutableFilename, fmOpenRead or fmShareDenyNone);
    try
      while True do
      begin
        fs.Seek(Offset, soFromBeginning);
        Offset := ReadInt64(fs);
        if Offset = 0 then
          Exit;
        fi := TEmbeddedFileInfo.Create;
        try
          fi.Offset := ReadInt64(fs);
          fi.Size := ReadInt64(fs);
          SetLength(AnsiFilename, Offset - fs.Position);
          fs.ReadBuffer(AnsiFilename[1], Offset - fs.Position);
          FileList.AddObject(Trim(AnsiFilename), fi);
        except
          fi.Free;
          raise;
        end;
      end;
    finally
      fs.Free;
    end;
  except
    {$ifdef DXGETTEXTDEBUG}
    raise;
    {$endif}
  end;
end;

constructor TFileLocator.Create;
begin
  inherited Create;
  MoFilesCS := TMultiReadExclusiveWriteSynchronizer.Create; 
  MoFiles := TStringList.Create; 
  FileList := TStringList.Create; 
  {$ifdef LINUX}
  FileList.Duplicates := dupError; 
  FileList.CaseSensitive := True; 
  {$endif}
  MoFiles.Sorted := True; 
  {$ifndef DELPHI5OROLDER}
  MoFiles.Duplicates := dupError; 
  MoFiles.CaseSensitive := False; 
  {$ifdef MSWINDOWS}
  FileList.Duplicates := dupError; 
  FileList.CaseSensitive := False;
  {$endif}
  {$endif}
  FileList.Sorted := True;
end;

destructor TFileLocator.Destroy;
var
  I: Integer;
begin
  for I := 0 to FileList.Count - 1 do
    FileList.Objects[I].Free;
{  while FileList.Count <> 0 do
  begin
    FileList.Objects[0].Free;
    FileList.Delete(0);
  end;}
  FileList.Free;
  MoFiles.Free;
  MoFilesCS.Free;
  inherited Destroy;
end;

function TFileLocator.FileExists(Filename: string): Boolean;
var
  idx: Integer;
begin
  if IsInDirStrOf(Filename, BaseDirectory) then
    Delete(Filename, 1, Length(BaseDirectory));
  Result := FileList.Find(Filename, idx);
end;

function TFileLocator.GetMoFile(Filename: string; DebugLogger: TDebugLogger): TMoFile;
var
  fi: TEmbeddedFileInfo;
  idx: Integer;
  idxName: string;
  Offset, Size: Int64;
  RealFilename: string;
begin
  // Find real Filename
  Offset := 0;
  Size := 0;
  RealFilename := Filename;
  if IsInDirStrOf(Filename, BaseDirectory) then
  begin
    Delete(Filename, 1, Length(BaseDirectory));
    idx := FileList.IndexOf(Filename);
    if idx <> -1 then
    begin
      fi := TEmbeddedFileInfo(FileList.Objects[idx]);
      RealFilename := ExecutableFilename;
      Offset := fi.Offset;
      Size := fi.Size;
      {$ifdef DXGETTEXTDEBUG}
      DebugLogger('Instead of ' + Filename + ', using ' + RealFilename +
        ' from Offset ' + IntToStr(Offset) + ', Size ' + IntToStr(Size));
      {$endif}
    end;
  end;


  {$ifdef DXGETTEXTDEBUG}
  DebugLogger('Reading .mo data from file ''' + Filename + '''');
  {$endif}

  // Find TMoFile object
  MoFilesCS.BeginWrite;
  try
    idxName := RealFilename + #0 + IntToStr(Offset);
    if MoFiles.Find(idxName, idx) then
    begin
      Result := TMoFile(MoFiles.Objects[idx]);
    end
    else
    begin
      Result := TMoFile.Create(RealFilename, Offset, Size);
      MoFiles.AddObject(idxName, Result);
    end;
    Inc(Result.Users);
  finally
    MoFilesCS.EndWrite;
  end;
end;

function TFileLocator.ReadInt64(str: TStream): Int64;
begin
  Assert(SizeOf(Result) = 8);
  str.ReadBuffer(Result, 8);
end;

procedure TFileLocator.ReleaseMoFile(var moFile: TMoFile);
var
  i: Integer;
begin
  Assert(moFile <> nil);

  MoFilesCS.BeginWrite;
  try
    Dec(moFile.Users);
    if moFile.Users <= 0 then
    begin
      i := MoFiles.Count - 1;
      while i >= 0 do
      begin
        if MoFiles.Objects[i] = moFile then
        begin
          MoFiles.Delete(i);
          FreeAndNil(moFile);
          Break;
        end;
        Dec(i);
      end;
    end;
  finally
    MoFilesCS.EndWrite;
  end;
end;

{ TTP_Retranslator }

constructor TTP_Retranslator.Create; 
begin
  inherited Create;
  List := TList.Create; 
end; 

destructor TTP_Retranslator.Destroy; 
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    TObject(List.Items[i]).Free;
  List.Free;
  inherited Destroy;
end;

procedure TTP_Retranslator.Execute;
var
  i: Integer;
  sl: TStrings;
  Item: TTP_RetranslatorItem;
  newvalue: WideString;
  {$ifndef DELPHI5OROLDER}
  ppi: PPropInfo;
  {$endif}
begin
  for i := 0 to List.Count - 1 do
  begin
    Item := TTP_RetranslatorItem(List.items[i]);
    if Item.obj is TStrings then
    begin
      // Since we don't know the order of items in sl, and don't have
      // the original .Objects[] anywhere, we cannot anticipate anything
      // about the current sl.Strings[] and sl.Objects[] values. We therefore
      // have to discard both values. We can, however, set the original .Strings[]
      // value into the List and retranslate that.
      sl := TStringList.Create; 
      try
        sl.Text := Item.OldValue; 
        Instance.TranslateStrings(sl, TextDomain);
        TStrings(Item.obj).BeginUpdate;
        try
          TStrings(Item.obj).Assign(sl);
        finally
          TStrings(Item.obj).EndUpdate; 
        end; 
      finally
        sl.Free; 
      end; 
    end 
    else 
    begin
      newValue := Instance.dgettext(TextDomain, Item.OldValue);
      {$ifdef DELPHI5OROLDER}
      SetStrProp(Item.obj, Item.PropName, newValue); 
      {$endif}
      {$ifndef DELPHI5OROLDER}
      ppi := GetPropInfo(Item.obj, Item.Propname); 
      if ppi <> nil then 
      begin
        SetWideStrProp(Item.obj, ppi, newValue); 
      end 
      else 
      begin
        {$ifdef DXGETTEXTDEBUG}
        Instance.DebugWriteln('ERROR: On retranslation, property disappeared: ' +
          Item.Propname + ' for object of type ' + Item.obj.ClassName); 
        {$endif}
      end; 
      {$endif}
    end; 
  end; 
end; 

procedure TTP_Retranslator.Remember(obj: TObject; const PropName: AnsiString;
  OldValue: WideString);
var
  Item: TTP_RetranslatorItem;
begin
  Item := TTP_RetranslatorItem.Create;
  Item.obj := obj; 
  Item.Propname := Propname; 
  Item.OldValue := OldValue; 
  List.Add(Item); 
end; 

{ TGnuGettextComponentMarker }

destructor TGnuGettextComponentMarker.Destroy; 
begin
  Retranslator.Free; 
  inherited Destroy;
end; 

{$ifndef CLR}
{ THook }

constructor THook.Create(OldProcedure, NewProcedure: Pointer; FollowJump: Boolean = False);
  { Idea and original code from Igor Siticov }
  { Modified by Jacques Garcia Vazquez and Lars Dybdahl }
begin
  inherited Create;
  {$ifndef CPU386}
    'This procedure only works on Intel i386 compatible processors.'
  {$endif}

  OldProc := OldProcedure;
  NewProc := NewProcedure;

  Reset(FollowJump); 
end; 

destructor THook.Destroy; 
begin
  Shutdown; 
  inherited Destroy; 
end; 

procedure THook.Disable; 
begin
  Assert(PatchPosition <> nil, 'Patch position in THook was nil when Disable was called'); 
  PatchPosition[0] := Original[0];
  PatchPosition[1] := Original[1];
  PatchPosition[2] := Original[2];
  PatchPosition[3] := Original[3];
  PatchPosition[4] := Original[4];
end;

procedure THook.Enable;
begin
  Assert(PatchPosition <> nil, 'Patch position in THook was nil when Enable was called');
  PatchPosition[0] := Patch[0];
  PatchPosition[1] := Patch[1];
  PatchPosition[2] := Patch[2];
  PatchPosition[3] := Patch[3];
  PatchPosition[4] := Patch[4]; 
end; 

procedure THook.Reset(FollowJump: Boolean);
var
  Offset: Integer; 
  {$ifdef LINUX}
  p: Pointer; 
  pagesize: Integer;
  {$endif}
  {$ifdef MSWINDOWS}
  ov: Cardinal; 
  {$endif}
begin
  if PatchPosition <> nil then
    Shutdown;

  PatchPosition := OldProc;
  if FollowJump and (Word(OldProc^) = $25FF) then
  begin
    // This finds the correct procedure if a virtual jump has been inserted
    // at the procedure address
    Inc(Integer(PatchPosition), 2); // skip the jump
    PatchPosition := PChar(Pointer(Pointer(PatchPosition)^)^);
  end;
  Offset := Integer(NewProc) - Integer(Pointer(PatchPosition)) - 5;

  Patch[0] := AnsiChar($E9);
  Patch[1] := AnsiChar(Offset and 255);
  Patch[2] := AnsiChar((Offset shr 8) and 255);
  Patch[3] := AnsiChar((Offset shr 16) and 255);
  Patch[4] := AnsiChar((Offset shr 24) and 255); 

  Original[0] := PatchPosition[0]; 
  Original[1] := PatchPosition[1];
  Original[2] := PatchPosition[2]; 
  Original[3] := PatchPosition[3];
  Original[4] := PatchPosition[4]; 

  {$ifdef MSWINDOWS}
  if not VirtualProtect(Pointer(PatchPosition), 5, PAGE_EXECUTE_READWRITE, @ov) then
    RaiseLastOSError;
  {$endif}
  {$ifdef LINUX}
  pageSize := sysconf(_SC_PAGE_SIZE);
  p := Pointer(PatchPosition);
  p := Pointer((Integer(p) + PAGESIZE - 1) and not (PAGESIZE - 1) - pageSize);
  if mprotect(p, pageSize, PROT_READ + PROT_WRITE + PROT_EXEC) <> 0 then
    RaiseLastOSError;
  {$endif}
end;

procedure THook.Shutdown;
begin
  Disable;
  PatchPosition := nil;
end;

procedure HookIntoResourceStrings(Enabled: Boolean = True;
  SupportPackages: Boolean = False);
begin
  HookLoadResString.Reset(SupportPackages);
  HookLoadStr.Reset(SupportPackages);
  HookFmtLoadStr.Reset(SupportPackages);
  if Enabled then
  begin
    HookLoadResString.Enable;
    HookLoadStr.Enable;
    HookFmtLoadStr.Enable;
  end;
end;
{$endif}

{ TMoFile }

constructor TMoFile.Create(const Filename: string; Offset, Size: Int64);
var
  i: Cardinal;
  nn: Integer;
  moFile: TFileStream;
begin
  inherited Create;

  if SizeOf(i) <> 4 then
    raise EGGProgrammingError.Create(
      'TDomain in gnugettext is written for an architecture that has 32 bit integers.');

  // Read the whole file into memory
  moFile := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  try
    if Size = 0 then
      Size := moFile.Size;
    SetLength(moMemory, Size);
    moFile.Seek(Offset, soFromBeginning);
    {$ifdef CLR}
    mofile.ReadBuffer(moMemory, Size);
    {$else}
    mofile.ReadBuffer(moMemory[0], Size);
    {$endif}
  finally
    moFile.Free;
  end;

  // Check the magic number
  doswap := False; 
  i := CardinalInMem(0);
  if (i <> $950412DE) and (i <> $DE120495) then
    EGGIOError.Create('This file is not a valid GNU gettext mo file: ' + Filename);
  doswap := (i = $DE120495);


  // Find the positions in the file according to the file format spec
  CardinalInMem(4);
  // Read the version number, but don't use it for anything.
  N := CardinalInMem(8);    // Get string count
  O := CardinalInMem(12);   // Get Offset of original strings
  T := CardinalInMem(16);   // Get Offset of translated strings

  // Calculate start conditions for a binary search
  nn := N;
  StartIndex := 1;
  while nn <> 0 do
  begin
    nn := nn shr 1;
    StartIndex := StartIndex shl 1;
  end;
  StartIndex := StartIndex shr 1;
  StartStep := StartIndex shr 1;
end;

function TMoFile.CardinalInMem (Offset: Cardinal): Cardinal;
begin
  if doswap then
  begin
    Result:=
      moMemory[Offset]+
      (moMemory[Offset + 1] shl 8)+
      (moMemory[Offset + 2] shl 16)+
      (moMemory[Offset + 3] shl 24);
  end
  else
  begin
    Result:=
      (momemory[Offset] shl 24)+
      (momemory[Offset + 1] shl 16)+
      (momemory[Offset + 2] shl 8)+
      momemory[Offset + 3];
  end;
end;


function TMoFile.gettext(const msgid: AnsiString; var Found: Boolean): AnsiString;
var
  {$ifdef CLR}
  j: Cardinal;
  {$endif}
  i, Step: Cardinal;
  Offset, Pos: Cardinal;
  CompareResult: Integer;
  msgidptr, a, b: Integer;
  abidx: Integer;
  Size, msgidsize: Integer;
begin
  Found := False;
  msgidptr := 1;
  msgidsize := Length(msgid);

  // Do binary search
  i := StartIndex;
  Step := StartStep;
  while True do
  begin
    // Get string for index i
    Pos := O + 8 * (i - 1);
    Offset := CardinalInMem(Pos + 4);
    Size := CardinalInMem(Pos);
    a := msgidptr;
    b := Offset;
    abidx := Size;
    if msgidsize < abidx then
      abidx := msgidsize;
    CompareResult := 0;
    while abidx <> 0 do
    begin
      CompareResult := Integer(Byte(msgid[a])) - Integer(moMemory[b]);
      if CompareResult <> 0 then
        Break;
      Dec(abidx);
      Inc(a);
      Inc(b);
    end;
    if CompareResult = 0 then
      CompareResult := msgidsize - Size;
    if CompareResult = 0 then
    begin  // msgid=s
      // Found the msgid
      Pos := T + 8 * (i - 1);
      Offset := CardinalInMem(Pos + 4);
      Size := CardinalInMem(Pos);
      {$ifdef CLR}
      SetLength(Result, Size);
      for j := 0 to Size - 1 do
        Result[j + 1] := AnsiChar(moMemory[Offset + j]);
      {$else}
      SetString(Result, PChar(@moMemory[0]) + Offset, Size);
      {$endif}
      Found := True;
      Break;
    end;
    if Step = 0 then
    begin
      // Not found
      Result := msgid;
      Break;
    end;
    if CompareResult < 0 then
    begin  // msgid<s
      if i < 1 + Step then
        i := 1
      else
        i := i - Step;
      Step := Step shr 1;
    end
    else
    begin  // msgid>s
      i := i + Step;
      if i > N then
        i := N;
      Step := Step shr 1;
    end;
  end;
end;

{$ifdef DXGETTEXTDEBUG}
const
  DebuggingText = 'gnugettext.pas debugging is enabled. Turn it off before ' +
    'releasing this piece of software.';
{$endif}

initialization
  {$ifdef DXGETTEXTDEBUG}
  {$ifdef CLR}
  MessageBox.show(DebuggingText);
  {$else}
   {$ifdef MSWINDOWS}
  MessageBox(0, PChar(DebuggingText), 'Information', MB_OK);
   {$endif}
   {$ifdef LINUX}
  WriteLn(stderr, DebuggingText);
   {$endif}
  {$endif}
  {$endif}
  if IsLibrary then
  begin
    // Get DLL/shared object Filename
    SetLength(ExecutableFilename, 300);
    {$ifdef MSWINDOWS}
    SetLength(ExecutableFilename, GetModuleFileName(HInstance,
      PChar(ExecutableFilename), Length(ExecutableFilename)));
    {$endif}
    {$ifdef LINUX}
    // This line has not been tested on Linux, yet, but should work.
    SetLength(ExecutableFilename, GetModuleFileName(0, PChar(ExecutableFilename),
      Length(ExecutableFilename)));
    {$endif}
    {$ifdef CLR}
    ExecutableFilename := System.Diagnostics.Process.GetCurrentProcess.MainModule.FileName;
    {$endif}
  end
  else
    ExecutableFilename := ParamStr(0);
  FileLocator := TFileLocator.Create;
  FileLocator.Analyze;
  ResourceStringDomainList := TStringList.Create;
  ResourceStringDomainList.Add(DefaultTextDomain);
  ResourceStringDomainListCS := TMultiReadExclusiveWriteSynchronizer.Create;
  DefaultInstance := TGnuGettextInstance.Create;
  {$ifdef MSWINDOWS}
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);
  {$endif}

  {$ifndef CLR}
  // replace Borlands LoadResString with gettext enabled version:
  HookLoadResString := THook.Create(@System.LoadResString, @LoadResStringA);
  HookLoadStr := THook.Create(@SysUtils.LoadStr, @SysUtilsLoadStr);
  HookFmtLoadStr := THook.Create(@SysUtils.FmtLoadStr, @SysUtilsFmtLoadStr);
  HookIntoResourceStrings(AutoCreateHooks, False);
  {$endif}

finalization
  FreeAndNil(DefaultInstance);
  FreeAndNil(ResourceStringDomainListCS);
  FreeAndNil(ResourceStringDomainList);
  {$ifndef CLR}
  FreeAndNil(HookFmtLoadStr);
  FreeAndNil(HookLoadStr);
  FreeAndNil(HookLoadResString);
  {$endif} 
  FreeAndNil(FileLocator);

end.
