{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: WStrUtils.PAS, released on 2004-01-25

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
All Rights Reserved.

Contributers:
  André Snepvangers: several functions from  JclUnicode added.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

This is a light weight Unicode unit. For more features use JclUnicode.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQWStrUtils;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SyncObjs;

const
  // asn: taken from JclUnicode
  // definitions of often used characters:
  // Note: Use them only for tests of a certain character not to determine character
  //       classes (like white spaces) as in Unicode are often many code points defined
  //       being in a certain class. Hence your best option is to use the various
  //       UnicodeIs* functions.
  WideNull = WideChar(#0);
  WideTabulator = WideChar(#9);
  WideSpace = WideChar(#32);

  // logical line breaks
  WideLF = WideChar(#10);
  WideLineFeed = WideChar(#10);
  WideVerticalTab = WideChar(#11);
  WideFormFeed = WideChar(#12);
  WideCR = WideChar(#13);
  WideCarriageReturn = WideChar(#13);
  WideCRLF: WideString = #13#10;
  WideLineSeparator = WideChar($2028);
  WideParagraphSeparator = WideChar($2029);

  BOM_LSB_FIRST = WideChar($FEFF);
  BOM_MSB_FIRST = WideChar($FFFE);

  

type
  TWideFileOptionsType =
   (
    foAnsiFile,  // loads/writes an ANSI file
    foUnicodeLB  // reads/writes BOM_LSB_FIRST/BOM_MSB_FIRST
   );
  TWideFileOptions = set of TWideFileOptionsType;
 
  TSearchFlag = (
    sfCaseSensitive,    // match letter case
    sfIgnoreNonSpacing, // ignore non-spacing characters in search
    sfSpaceCompress,    // handle several consecutive white spaces as one white space
                        // (this applies to the pattern as well as the search text)
    sfWholeWordOnly     // match only text at end/start and/or surrounded by white spaces
  );
  TSearchFlags = set of TSearchFlag; 
    // UTF conversion schemes (UCS) data types
  PUCS4 = ^UCS4;
  UCS4 = Cardinal;
  PUCS2 = PWideChar;
  UCS2 = WideChar;

  TUCS2Array = array of UCS2;
  TUCS4Array = array of UCS4;

const
  // taken from JclUniCode
  ReplacementCharacter: UCS4 = $0000FFFD;
  MaximumUCS2: UCS4 = $0000FFFF;
  MaximumUTF16: UCS4 = $0010FFFF;
  MaximumUCS4: UCS4 = $7FFFFFFF;

  SurrogateHighStart: UCS4 = $D800;
  SurrogateHighEnd: UCS4 = $DBFF;
  SurrogateLowStart: UCS4 = $DC00;
  SurrogateLowEnd: UCS4 = $DFFF;


type
  TWStrings = class;
  TWStringList = class;

  TWStringListSortCompare = function(List: TWStringList; Index1, Index2: Integer): Integer;

  TWStrings = class(TPersistent)
  private
    FDelimiter: WideChar;
    FQuoteChar: WideChar;
    FNameValueSeparator: WideChar;
    FLineSeparator: WideString;
    FUpdateCount: Integer;
    function GetCommaText: WideString;
    function GetDelimitedText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetDelimitedText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function ExtractName(const S: WideString): WideString;
    function GetP(Index: Integer): PWideString; virtual; abstract;
    function Get(Index: Integer): WideString; 
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual; abstract;
    procedure PutObject(Index: Integer; AObject: TObject); virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: WideString): Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    function Add(const S: WideString): Integer; virtual;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TWStrings); overload; virtual;
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    function CreateAnsiStringList: TStrings;
    procedure AddStringsTo(Dest: TStrings); virtual;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TWStrings): Boolean; overload;
    function Equals(Strings: TStrings): Boolean; overload;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetText: PWideChar; virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: WideString); virtual;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: AnsiString;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure LoadFromStream(Stream: TStream;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: AnsiString;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure SaveToStream(Stream: TStream;
      WideFileOptions: TWideFileOptions = []); virtual;
    procedure SetText(Text: PWideChar); virtual;
    function GetDelimtedTextEx(ADelimiter, AQuoteChar: WideChar): WideString;
    procedure SetDelimtedTextEx(ADelimiter, AQuoteChar: WideChar; const Value: WideString);
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: WideString read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property Delimiter: WideChar read FDelimiter write FDelimiter;
    property DelimitedText: WideString read GetDelimitedText write SetDelimitedText;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: WideChar read FQuoteChar write FQuoteChar;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: WideChar read FNameValueSeparator write FNameValueSeparator;
    property LineSeparator: WideString read FLineSeparator write FLineSeparator;
    property PStrings[Index: Integer]: PWideString read GetP;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetTextStr write SetTextStr;
  end;

  // do not replace by JclUnicode.TWideStringList (speed and size issue)
  PWStringItem = ^TWStringItem;
  TWStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TWStringList = class(TWStrings)
  private
    FList: TList;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    function GetItem(Index: Integer): PWStringItem;
    procedure Changed; virtual;
    procedure Changing; virtual;
    function GetP(Index: Integer): PWideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Value: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: WideString): Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    function AddObject(const S: WideString; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    // Find() also works with unsorted lists
    function IndexOf(const S: WideString): Integer; override;
    procedure InsertObject(Index: Integer; const S: WideString;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TWStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TWideStringList = TWStringList;
  TWideStrings = TWStrings;


// WideChar functions
function CharToWideChar(Ch: AnsiChar): WideChar;
function WideCharToChar(Ch: WideChar): AnsiChar;

// PWideChar functions
procedure MoveWideChar(const Source; var Dest; Count: Integer);

function StrAllocW(WideSize: Cardinal): PWideChar;
function StrNewW(const S: WideString): PWideChar;
procedure StrDisposeW(var P: PWideChar);
function StrLICompW(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrLICompW2(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrCompW(S1, S2: PWideChar): Integer;
function StrLCompW(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrIComp(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrPosW(S, SubStr: PWideChar): PWideChar;
function StrLenW(P: PWideChar): Integer;
function StrScanW(P: PWideChar; Ch: WideChar): PWideChar;
function StrEndW(P: PWideChar): PWideChar;
function StrCopyW(Dest, Source: PWideChar): PWideChar;
function StrLCopyW(Dest, Source: PWideChar; MaxLen: Integer): PWideChar;
function StrCatW(Dest, Source: PWideChar): PWideChar;
function StrLCatW(Dest, Source: PWideChar; MaxLen: Integer): PWideChar;

// WideString functions
function WidePos(const SubStr, S: WideString): Integer;
function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;

function TrimW(const S: WideString): WideString;
function TrimLeftW(const S: WideString): WideString;
function TrimRightW(const S: WideString): WideString;

function TrimLeftLengthW(const S: WideString): Integer;
function TrimRightLengthW(const S: WideString): Integer;

// asn: functions taken from JclUnicode
function WideStringToStringEx(const WS: WideString; CodePage: Word): string;
function StringToWideStringEx(const S: string; CodePage: Word): WideString;
function UTF8ToWideString(S: AnsiString): WideString;
function WideStringToUTF8(S: WideString): AnsiString;
function WideCaseFolding(C: WideChar): WideString; overload;
function WideCaseFolding(const S: WideString): WideString; overload;

var
  // As the global data can be accessed by several threads it should be guarded
  // while the data is loaded.
  LoadInProgress: TCriticalSection;

implementation

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvWStrUtils.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvWStrUtils.res}
{$ENDIF LINUX}


uses 
  RTLConsts, 
  Math;



procedure SwapWordByteOrder(P: PChar; Len: Cardinal);
var
  B: Char;
begin
  while Len > 0 do
  begin
    B := P[0];
    P[0] := P[1];
    P[1] := B;
    Inc(P, 2);
    Dec(Len);
  end;
end;

const
  HalfShift: Integer = 10;

  HalfBase: UCS4 = $0010000;
  HalfMask: UCS4 = $3FF;

  OffsetsFromUTF8: array[0..5] of UCS4 = ($00000000, $00003080, $000E2080,
                                          $03C82080, $FA082080, $82082080);

  BytesFromUTF8: array[0..255] of Byte = (
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5);

  FirstByteMark: array[0..6] of Byte = ($00, $00, $C0, $E0, $F0, $F8, $FC);

function WideStringToUTF8(S: WideString): AnsiString;

var
  Ch: UCS4;
  L, J, T,
  BytesToWrite: Cardinal;
  ByteMask: UCS4;
  ByteMark: UCS4;

begin
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, Length(S) * 6); // assume worst case
    T := 1;
    ByteMask := $BF;
    ByteMark := $80;

    for J := 1 to Length(S) do
    begin
      Ch := UCS4(S[J]);

      if Ch < $80 then
        BytesToWrite := 1
      else
        if Ch < $800 then
          BytesToWrite := 2
        else
          if Ch < $10000 then
            BytesToWrite := 3
          else
            if Ch < $200000 then
              BytesToWrite := 4
            else
              if Ch < $4000000 then
                BytesToWrite := 5
              else
                if Ch <= MaximumUCS4 then
                  BytesToWrite := 6
                else
                begin
                  BytesToWrite := 2;
                  Ch := ReplacementCharacter;
                end;

      for L := BytesToWrite downto 2 do
      begin
        Result[T + L - 1] := Char((Ch or ByteMark) and ByteMask);
        Ch := Ch shr 6;
      end;
      Result[T] := Char(Ch or FirstByteMark[BytesToWrite]);
      Inc(T, BytesToWrite);
    end;
    SetLength(Result, T - 1); // set to actual length
  end;
end;


// WideChar functions

function CharToWideChar(Ch: Char): WideChar;
var
  WS: WideString;
begin
  WS := Ch;
  Result := WS[1];
end;

function WideCharToChar(Ch: WideChar): AnsiChar;
var
  S: AnsiString;
begin
  S := Ch;
  Result := S[1];
end;

// PWideChar functions

procedure MoveWideChar(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count * SizeOf(WideChar));
end;

function StrAllocW(WideSize: Cardinal): PWideChar;
begin
  if WideSize > 0 then
    Result := AllocMem(WideSize * SizeOf(WideChar))
  else
    Result := nil;
end;

function StrNewW(const S: WideString): PWideChar;
begin
  Result := nil;
  if S <> '' then
  begin
    Result := StrAllocW(Length(S) + 1);
    MoveWideChar(S[1], Result^, Length(S));
  end;
end;

procedure StrDisposeW(var P: PWideChar);
begin
  if P <> nil then
    FreeMem(P);
  P := nil;
end;

function StrLICompW(S1, S2: PWideChar; MaxLen: Integer): Integer;
var
  P1, P2: WideString;
begin
  SetString(P1, S1, Min(MaxLen, StrLenW(S1)));
  SetString(P2, S2, Min(MaxLen, StrLenW(S2)));
  Result := WideCompareText(P1, P2);
end;

function StrLICompW2(S1, S2: PWideChar; MaxLen: Integer): Integer;
var
  P1, P2: WideString;
begin
  // faster than the JclUnicode.StrLICompW function
  SetString(P1, S1, Min(MaxLen, StrLenW(S1)));
  SetString(P2, S2, Min(MaxLen, StrLenW(S2)));
  Result := WideCompareText(P1, P2);
end;

function StrCompW(S1, S2: PWideChar): Integer;
var
  NullWide: WideChar;
begin
  Result := 0;
  if S1 = S2 then // "equal" and "nil" case
    Exit;
  NullWide := #0;

  if S1 = nil then
    S1 := @NullWide
  else
  if S2 = nil then
    S2 := @NullWide;
  while (S1^ = S2^) and (S1^ <> #0) and (S2^ <> #0) do
  begin
    Inc(S1);
    Inc(S2);
  end;
  Result := Sign(Integer(S1^) - Integer(S2^));
end;

function StrLCompW(S1, S2: PWideChar; MaxLen: Integer): Integer;
var
  NullWide: WideChar;
begin
  Result := 0;
  if S1 = S2 then // "equal" and "nil" case
    Exit;
  NullWide := #0;

  if S1 = nil then
    S1 := @NullWide
  else
  if S2 = nil then
    S2 := @NullWide;
  while (MaxLen > 0) and (S1^ = S2[0]) and (S1^ <> #0) and (S2^ <> #0) do
  begin
    Inc(S1);
    Inc(S2);
    Dec(MaxLen);
  end;
  Result := Sign(Integer(S1^) - Integer(S2^));
end;

function StrIComp(S1, S2: PWideChar; MaxLen: Integer): Integer;
var
  NullWide: WideChar;
begin
  Result := 0;
  if S1 = S2 then // "equal" and "nil" case
    Exit;
  NullWide := #0;

  if S1 = nil then
    S1 := @NullWide
  else
  if S2 = nil then
    S2 := @NullWide;
  while (MaxLen > 0) and (S1^ = S2^) and (S1^ <> #0) and (S2^ <> #0) do
  begin
    Inc(S1);
    Inc(S2);
    Dec(MaxLen);
  end;
  Result := Sign(Integer(S1^) - Integer(S2^));
end;

function StrPosW(S, SubStr: PWideChar): PWideChar;
var
  P: PWideChar;
  I: Integer;
begin
  Result := nil;
  if (S = nil) or (SubStr = nil) or (S^ = #0) or (SubStr^ = #0) then
    Exit;
  Result := S;
  while Result^ <> #0 do
  begin
    if Result^ <> SubStr^ then
      Inc(Result)
    else
    begin
      P := Result + 1;
      I := 1;
      while (P^ <> #0) and (P^ = SubStr[I]) do
      begin
        Inc(I);
        Inc(P);
      end;
      if SubStr[I] = #0 then
        Exit
      else
        Inc(Result);
    end;
  end;
  Result := nil;
end;

function StrLenW(P: PWideChar): Integer;
begin
  Result := 0;
  if P <> nil then
    while P[Result] <> #0 do
      Inc(Result);
end;

function StrScanW(P: PWideChar; Ch: WideChar): PWideChar;
begin
  Result := P;
  if Result <> nil then
    while (Result^ <> #0) and (Result^ <> Ch) do
      Inc(Result);
end;

function StrEndW(P: PWideChar): PWideChar;
begin
  Result := P;
  if Result <> nil then
    while Result^ <> #0 do
      Inc(Result);
end;

function StrCopyW(Dest, Source: PWideChar): PWideChar;
begin
  Result := Dest;
  if Dest <> nil then
  begin
    if Source <> nil then
      while Source^ <> #0 do
      begin
        Dest^ := Source^;
        Inc(Source);
        Inc(Dest);
      end;
    Dest^ := #0;
  end;
end;

function StrLCopyW(Dest, Source: PWideChar; MaxLen: Integer): PWideChar;
begin
  Result := Dest;
  if (Dest <> nil) and (MaxLen > 0) then
  begin
    if Source <> nil then
      while (MaxLen > 0) and (Source^ <> #0) do
      begin
        Dest^ := Source^;
        Inc(Source);
        Inc(Dest);
        Dec(MaxLen);
      end;
    Dest^ := #0;
  end;
end;

function StrCatW(Dest, Source: PWideChar): PWideChar;
begin
  Result := Dest;
  while Dest^ <> #0 do
    Inc(Dest);
  StrCopyW(Dest, Source);
end;

function StrLCatW(Dest, Source: PWideChar; MaxLen: Integer): PWideChar;
begin
  Result := Dest;
  while Dest^ <> #0 do
  begin
    Inc(Dest);
    Dec(MaxLen);
  end;
  StrLCopyW(Dest, Source, MaxLen);
end;


// WideString functions

function WidePos(const SubStr, S: WideString): Integer;
var
  P: PWideChar;
begin
  P := StrPosW(PWideChar(S), PWideChar(SubStr));
  if P <> nil then
    Result := P - PWideChar(S) + 1
  else
    Result := 0;
end;

// original code by Mike Lischke (extracted from JclUnicode.pas)

function WideQuotedStr(const S: WideString; Quote: WideChar): WideString;
var
  P, Src,
  Dest: PWideChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := StrScanW(PWideChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := StrScanW(P, Quote);
  end;

  if AddCount = 0 then
    Result := Quote + S + Quote
  else
  begin
    SetLength(Result, Length(S) + AddCount + 2);
    Dest := PWideChar(Result);
    Dest^ := Quote;
    Inc(Dest);
    Src := PWideChar(S);
    P := StrScanW(Src, Quote);
    repeat
      Inc(P);
      MoveWideChar(Src^, Dest^, P - Src);
      Inc(Dest, P - Src);
      Dest^ := Quote;
      Inc(Dest);
      Src := P;
      P := StrScanW(Src, Quote);
    until P = nil;
    P := StrEndW(Src);
    MoveWideChar(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
  end;
end;

// original code by Mike Lischke (extracted from JclUnicode.pas)

function WideExtractQuotedStr(var Src: PWideChar; Quote: WideChar): WideString;
var
  P, Dest: PWideChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then
    Exit;

  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := StrScanW(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then
      Break;
    Inc(Src);
    Inc(DropCount);
    Src := StrScanW(Src, Quote);
  end;

  if Src = nil then
    Src := StrEndW(P);
  if (Src - P) <= 1 then
    Exit;

  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PWideChar(Result);
    Src := StrScanW(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then
        Break;
      MoveWideChar(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := StrScanW(Src, Quote);
    end;
    if Src = nil then
      Src := StrEndW(P);
    MoveWideChar(P^, Dest^, Src - P - 1);
  end;
end;
//----------------- support for case mapping -------------------------------------------------------

type
  TCase = array[0..3] of TUCS4Array; // mapping for case fold, lower, title and upper in this order
  TCaseArray = array of TCase;

var
  // An array for all case mappings (including 1 to many casing if saved by the extraction program).
  // The organization is a sparse, two stage matrix.
  // SingletonMapping is to quickly return a single default mapping.
  CaseDataLoaded: Boolean;
  CaseMapping: array[Byte] of TCaseArray;
  SingletonMapping: TUCS4Array;

//--------------------------------------------------------------------------------------------------

procedure LoadCaseMappingData;

var
  Stream: TResourceStream;
  I, Code,
  Size: Cardinal;
  First,
  Second: Byte;

begin
  if not CaseDataLoaded then
  begin
    // make sure no other code is currently modifying the global data area
    LoadInProgress.Enter;

    try
      SetLength(SingletonMapping, 1);
      CaseDataLoaded := True;
      Stream := TResourceStream.Create(HInstance, 'CASE', 'UNICODEDATA');
      try
        // the first entry in the stream is the number of entries in the case mapping table
        Stream.ReadBuffer(Size, 4);
        for I := 0 to Size - 1 do
        begin
          // a) read actual code point
          Stream.ReadBuffer(Code, 4);

          Assert(Code < $10000, 'cased Unicode character > $FFFF found');
          // if there is no high byte entry in the first stage table then create one
          First := (Code shr 8) and $FF;
          Second := Code and $FF;
          if CaseMapping[First] = nil then
            SetLength(CaseMapping[First], 256);

          // b) read fold case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, 0], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, 0, 0], Size * SizeOf(UCS4));
          end;
          // c) read lower case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, 1], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, 1, 0], Size * SizeOf(UCS4));
          end;
          // d) read title case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, 2], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, 2, 0], Size * SizeOf(UCS4));
          end;
          // e) read upper case array
          Stream.ReadBuffer(Size, 4);
          if Size > 0 then
          begin
            SetLength(CaseMapping[First, Second, 3], Size);
            Stream.ReadBuffer(CaseMapping[First, Second, 3, 0], Size * SizeOf(UCS4));
          end;
        end;

      finally
        Stream.Free;
      end;
    finally
      LoadInProgress.Leave;
    end;
  end;
end;


//----------------- general purpose case mapping ---------------------------------------------------

function CaseLookup(Code: Cardinal; CaseType: Cardinal): TUCS4Array;

// Performs a lookup of the given code and returns its case mapping if found.
// CaseType must be 0 for case folding, 1 for lower case, 2 for title case and 3 for upper case, respectively.
// If Code could not be found (or there is no case mapping) then the result is a mapping of length 1 with the
// code itself. Otherwise an array of code points is returned which represent the mapping.

var
  First,
  Second: Byte;

begin
  // load case mapping data if not already done
  if not CaseDataLoaded then
    LoadCaseMappingData;

  First := (Code shr 8) and $FF;
  Second := Code and $FF;
  // Check first stage table whether there is a mapping for a particular block and
  // (if so) then whether there is a mapping or not.
  if (CaseMapping[First] = nil) or (CaseMapping[First, Second, CaseType] = nil) then
  begin
    SingletonMapping[0] := Code;
    Result := SingletonMapping;
  end
  else
    Result := CaseMapping[First, Second, CaseType];
end;

//--------------------------------------------------------------------------------------------------

function UnicodeCaseFold(Code: UCS4): TUCS4Array;

// This function returnes an array of special case fold mappings if there is one defined for the given
// code, otherwise the lower case will be returned. This all applies only to cased code points.
// Uncased code points are returned unchanged.

begin
  Result := CaseLookup(Code, 0);
end;

function WideCaseFolding(C: WideChar): WideString;

// Special case folding function to map a string to either its lower case or
// to special cases. This can be used for case-insensitive comparation.

var
  I: Integer;
  Mapping: TUCS4Array;

begin
  Mapping := UnicodeCaseFold(UCS4(C));
  SetLength(Result, Length(Mapping));
  for I := 0 to High(Mapping) do
    Result[I + 1] := WideChar(Mapping[I]);
end;

//--------------------------------------------------------------------------------------------------

function WideCaseFolding(const S: WideString): WideString;

var
  I: Integer;

begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + WideCaseFolding(S[I]);
end;


function StringToWideStringEx(const S: string; CodePage: Word): WideString;
{$IFDEF MSWINDOWS}
var
  InputLength,
  OutputLength: Integer;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  InputLength := Length(S);
  OutputLength := MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, nil, 0);
  SetLength(Result, OutputLength);
  MultiByteToWideChar(CodePage, 0, PChar(S), InputLength, PWideChar(Result), OutputLength);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := S;  // TODO
  {$ENDIF LINUX}
end;

function WideStringToStringEx(const WS: WideString; CodePage: Word): string;
{$IFDEF MSWINDOWS}
var
  InputLength,
  OutputLength: Integer;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  InputLength := Length(WS);
  OutputLength := WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, nil, 0, nil, nil);
  SetLength(Result, OutputLength);
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), InputLength, PChar(Result), OutputLength, nil, nil);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := WS;     // TODO
  {$ENDIF LINUX}
end;

function UTF8ToWideString(S: AnsiString): WideString;
var
  L, J, T: Cardinal;
  Ch: UCS4;
  ExtraBytesToWrite: Word;
begin
  if Length(S) = 0 then
    Result := ''
  else
  begin
    SetLength(Result, Length(S)); // create enough room

    L := 1;
    T := 1;
    while L <= Cardinal(Length(S)) do
    begin
      Ch := 0;
      ExtraBytesToWrite := BytesFromUTF8[Ord(S[L])];

      for J := ExtraBytesToWrite downto 1 do
      begin
        Ch := Ch + Ord(S[L]);
        Inc(L);
        Ch := Ch shl 6;
      end;
      Ch := Ch + Ord(S[L]);
      Inc(L);
      Ch := Ch - OffsetsFromUTF8[ExtraBytesToWrite];

      if Ch <= MaximumUCS2 then
      begin
        Result[T] := WideChar(Ch);
        Inc(T);
      end
      else
        if Ch > MaximumUCS4 then
        begin
          Result[T] := WideChar(ReplacementCharacter);
          Inc(T);
        end
        else
        begin
          Ch := Ch - HalfBase;
          Result[T] := WideChar((Ch shr HalfShift) + SurrogateHighStart);
          Inc(T);
          Result[T] := WideChar((Ch and HalfMask) + SurrogateLowStart);
          Inc(T);
        end;
    end;
    SetLength(Result, T - 1); // now fix up length
  end;
end;

// END of code by Mike Lischke (extracted from JclUnicode.pas)





function TrimW(const S: WideString): WideString;
begin
  Result := Trim(S);
end;

function TrimLeftW(const S: WideString): WideString;
begin
  Result := TrimLeft(S);
end;

function TrimRightW(const S: WideString): WideString;
begin
  Result := TrimRight(S);
end;



function TrimLeftLengthW(const S: WideString): Integer;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := 1;
  while (Result <= Len) and (S[Result] <= #32) do
    Inc(Result);
  Result := Len - Result + 1;
end;

function TrimRightLengthW(const S: WideString): Integer;
begin
  Result := Length(S);
  while (Result > 0) and (S[Result] <= #32) do
    Dec(Result);
end;

//=== { TWStrings } ==========================================================

constructor TWStrings.Create;
begin
  inherited Create;
  // FLineSeparator := WideChar($2028);
  {$IFDEF MSWINDOWS}
  FLineSeparator := WideChar(13) + '' + WideChar(10); // compiler wants it this way
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FLineSeparator := WideChar(10);
  {$ENDIF LINUX}
  FNameValueSeparator := '=';
  FDelimiter := ',';
  FQuoteChar := '"';
end;

function TWStrings.Add(const S: WideString): Integer;
begin
  Result := AddObject(S, nil);
end;

function TWStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Count;
  InsertObject(Result, S, AObject);
end;

procedure TWStrings.AddStrings(Strings: TWStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings.GetP(I)^, Strings.Objects[I]);
end;

procedure TWStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    AddObject(Strings.Strings[I], Strings.Objects[I]);
end;

procedure TWStrings.AddStringsTo(Dest: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dest.AddObject(GetP(I)^, Objects[I]);
end;

procedure TWStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TWStrings.Assign(Source: TPersistent);
begin
  if Source is TWStrings then
  begin
    BeginUpdate;
    try
      Clear;
      FDelimiter := TWStrings(Source).FDelimiter;
      FNameValueSeparator := TWStrings(Source).FNameValueSeparator;
      FQuoteChar := TWStrings(Source).FQuoteChar;
      AddStrings(TWStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      Clear; 
      FNameValueSeparator := CharToWideChar(TStrings(Source).NameValueSeparator);  
      FQuoteChar := CharToWideChar(TStrings(Source).QuoteChar);
      FDelimiter := CharToWideChar(TStrings(Source).Delimiter); 
      AddStrings(TStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TWStrings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStrings then
  begin
    TStrings(Dest).BeginUpdate;
    try
      TStrings(Dest).Clear; 
      TStrings(Dest).NameValueSeparator := WideCharToChar(NameValueSeparator);  
      TStrings(Dest).QuoteChar := WideCharToChar(QuoteChar);
      TStrings(Dest).Delimiter := WideCharToChar(Delimiter); 
      for I := 0 to Count - 1 do
        TStrings(Dest).AddObject(GetP(I)^, Objects[I]);
    finally
      TStrings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TWStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

function TWStrings.CompareStrings(const S1, S2: WideString): Integer;
begin
  Result := WideCompareText(S1, S2);
end;

function TWStrings.CreateAnsiStringList: TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  try
    Result.BeginUpdate;
    for I := 0 to Count - 1 do
      Result.AddObject(GetP(I)^, Objects[I]);
    Result.EndUpdate;
  except
    Result.Free;
    raise;
  end;
end;

procedure TWStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TWStrings then
        Result := not Equals(TWStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty('Strings', ReadData, WriteData, DoWrite);
end;

procedure TWStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

function TWStrings.Equals(Strings: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Strings.Count = Count then
  begin
    for I := 0 to Count - 1 do
      if Strings[I] <> PStrings[I]^ then
        Exit;
    Result := True;
  end;
end;

function TWStrings.Equals(Strings: TWStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Strings.Count = Count then
  begin
    for I := 0 to Count - 1 do
      if Strings[I] <> PStrings[I]^ then
        Exit;
    Result := True;
  end;
end;

procedure TWStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := PStrings[Index1]^;
    TempObject := Objects[Index1];
    PStrings[Index1]^ := PStrings[Index2]^;
    Objects[Index1] := Objects[Index2];
    PStrings[Index2]^ := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TWStrings.ExtractName(const S: WideString): WideString;
var
  Index: Integer;
begin
  Result := S;
  Index := WidePos(NameValueSeparator, Result);
  if Index <> 0 then
    SetLength(Result, Index - 1)
  else
    SetLength(Result, 0);
end;

function TWStrings.Get(Index: Integer): WideString;
begin
  Result := GetP(Index)^;
end;

function TWStrings.GetCapacity: Integer;
begin
  Result := Count;
end;

function TWStrings.GetCommaText: WideString;
begin
  Result := GetDelimtedTextEx(',', '"');
end;

function TWStrings.GetDelimitedText: WideString;
begin
  Result := GetDelimtedTextEx(FDelimiter, FQuoteChar);
end;

function TWStrings.GetDelimtedTextEx(ADelimiter, AQuoteChar: WideChar): WideString;
var
  S: WideString;
  P: PWideChar;
  I, Num: Integer;
begin
  Num := GetCount;
  if (Num = 1) and (GetP(0)^ = '') then
    Result := AQuoteChar + '' + AQuoteChar // Compiler wants it this way
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := GetP(I)^;
      P := PWideChar(S);
      while True do
      begin
        case P[0] of
          WideChar(0)..WideChar(32):
            Inc(P);
        else
          if (P[0] = AQuoteChar) or (P[0] = ADelimiter) then
            Inc(P)
          else
            Break;
        end;
      end;
      if P[0] <> WideChar(0) then
        S := WideQuotedStr(S, AQuoteChar);
      Result := Result + S + ADelimiter;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

function TWStrings.GetName(Index: Integer): WideString;
var
  I: Integer;
begin
  Result := GetP(Index)^;
  I := WidePos(FNameValueSeparator, Result);
  if I > 0 then
    SetLength(Result, I - 1);
end;

function TWStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TWStrings.GetText: PWideChar;
begin
  Result := StrNewW(GetTextStr);
end;

function TWStrings.GetTextStr: WideString;
var
  I: Integer;
  Len, LL: Integer;
  P: PWideChar;
  W: PWideString;
begin
  Len := 0;
  LL := Length(LineSeparator);
  for I := 0 to Count - 1 do
    Inc(Len, Length(GetP(I)^) + LL);
  SetLength(Result, Len);
  P := PWideChar(Result);
  for I := 0 to Count - 1 do
  begin
    W := GetP(I);
    Len := Length(W^);
    if Len > 0 then
    begin
      MoveWideChar(W^[1], P[0], Len);
      Inc(P, Len);
    end;
    if LL > 0 then
    begin
      MoveWideChar(FLineSeparator[1], P[0], LL);
      Inc(P, LL);
    end;
  end;
end;

function TWStrings.GetValue(const Name: WideString): WideString;
var
  Idx: Integer;
begin
  Idx := IndexOfName(Name);
  if Idx >= 0 then
    Result := GetValueFromIndex(Idx)
  else
    Result := '';
end;

function TWStrings.GetValueFromIndex(Index: Integer): WideString;
var
  I: Integer;
begin
  Result := GetP(Index)^;
  I := WidePos(FNameValueSeparator, Result);
  if I > 0 then
    System.Delete(Result, 1, I)
  else
    Result := '';
end;

function TWStrings.IndexOf(const S: WideString): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(GetP(Result)^, S) = 0 then
      Exit;
  Result := -1;
end;

function TWStrings.IndexOfName(const Name: WideString): Integer;
begin
  for Result := 0 to Count - 1 do
    if CompareStrings(Names[Result], Name) = 0 then
      Exit;
  Result := -1;
end;

function TWStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if Objects[Result] = AObject then
      Exit;
  Result := -1;
end;

procedure TWStrings.Insert(Index: Integer; const S: WideString);
begin
  InsertObject(Index, S, nil);
end;

procedure TWStrings.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
begin
end;

procedure TWStrings.LoadFromFile(const FileName: AnsiString;
  WideFileOptions: TWideFileOptions = []);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, WideFileOptions);
  finally
    Stream.Free;
  end;
end;

procedure TWStrings.LoadFromStream(Stream: TStream;
  WideFileOptions: TWideFileOptions = []);
var
  AnsiS: AnsiString;
  WideS: WideString;
  WC: WideChar;
begin
  BeginUpdate;
  try
    Clear;
    if foAnsiFile in WideFileOptions then
    begin
      Stream.Read(WC, SizeOf(WC));
      Stream.Seek(-SizeOf(WC), soFromCurrent);
      if (Hi(Word(WC)) <> 0) and (WC <> BOM_LSB_FIRST) and (WC <> BOM_MSB_FIRST) then
      begin
        SetLength(AnsiS, Stream.Size - Stream.Position);
        Stream.Read(AnsiS[1], Length(AnsiS));
        SetTextStr(AnsiS);
        Exit;
      end;
    end;

    Stream.Read(WC, SizeOf(WC));
    if (WC <> BOM_LSB_FIRST) and (WC <> BOM_MSB_FIRST) then
      Stream.Seek(-SizeOf(WC), soFromCurrent);
    SetLength(WideS, Stream.Size - Stream.Position);
    Stream.Read(WideS[1], Length(WideS) * SizeOf(WideChar));

    if WC = BOM_MSB_FIRST then
      SwapWordByteOrder(Pointer(WideS), Length(WideS));

    SetTextStr(WideS);
  finally
    EndUpdate;
  end;
end;

procedure TWStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := GetP(CurIndex)^;
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TWStrings.ReadData(Reader: TReader);
begin
  BeginUpdate;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      if Reader.NextValue in [vaLString, vaString] then
        Add(Reader.ReadString)
      else
        Add(Reader.ReadWideString);
    Reader.ReadListEnd;
  finally
    EndUpdate;
  end;
end;

procedure TWStrings.SaveToFile(const FileName: AnsiString;
  WideFileOptions: TWideFileOptions = []);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, WideFileOptions);
  finally
    Stream.Free;
  end;
end;

procedure TWStrings.SaveToStream(Stream: TStream;
  WideFileOptions: TWideFileOptions = []);
var
  AnsiS: AnsiString;
  WideS: WideString;
  WC: WideChar;
begin
  if foAnsiFile in WideFileOptions then
  begin
    AnsiS := GetTextStr;
    Stream.Write(AnsiS[1], Length(AnsiS));
  end
  else
  begin
    if foUnicodeLB in WideFileOptions then
    begin
      WC := BOM_LSB_FIRST;
      Stream.Write(WC, SizeOf(WC));
    end;
    WideS := GetTextStr;
    Stream.Write(WideS[1], Length(WideS) * SizeOf(WideChar));
  end;
end;

procedure TWStrings.SetCapacity(NewCapacity: Integer);
begin
end;

procedure TWStrings.SetCommaText(const Value: WideString);
begin
  SetDelimtedTextEx(',', '"', Value);
end;

procedure TWStrings.SetDelimitedText(const Value: WideString);
begin
  SetDelimtedTextEx(Delimiter, QuoteChar, Value);
end;

procedure TWStrings.SetDelimtedTextEx(ADelimiter, AQuoteChar: WideChar;
  const Value: WideString);
var
  P, P1: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while True do
    begin
      case P[0] of
        WideChar(1)..WideChar(32):
          Inc(P);
      else
        Break;
      end;
    end;
    while P[0] <> WideChar(0) do
    begin
      if P[0] = AQuoteChar then
        S := WideExtractQuotedStr(P, AQuoteChar)
      else
      begin
        P1 := P;
        while (P[0] > WideChar(32)) and (P[0] <> ADelimiter) do
          Inc(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);

      while True do
        case P[0] of
          WideChar(1)..WideChar(32):
            Inc(P);
        else
          Break;
        end;
      if P[0] = ADelimiter then
        repeat
          Inc(P);
          case P[0] of
            WideChar(1)..WideChar(32):
              ;
          else
            Break;
          end;
        until False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

procedure TWStrings.SetTextStr(const Value: WideString);
var
  P, Start: PWideChar;
  S: WideString;
  Len: Integer;
begin
  BeginUpdate;
  try
    Clear;
    if Value <> '' then
    begin
      P := PWideChar(Value);
      if P <> nil then
      begin
        while P[0] <> WideChar(0) do
        begin
          Start := P;
          while True do
          begin
            case P[0] of
              WideChar(0), WideChar(10), WideChar(13):
                Break;
            end;
            Inc(P);
          end;
          Len := P - Start;
          if Len > 0 then
          begin
            SetString(S, Start, Len);
            AddObject(S, nil); // consumes most time
          end
          else
            AddObject('', nil);
          if P[0] = WideChar(13) then
            Inc(P);
          if P[0] = WideChar(10) then
            Inc(P);
        end;
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TWStrings.SetValue(const Name, Value: WideString);
var
  Idx: Integer;
begin
  Idx := IndexOfName(Name);
  if Idx >= 0 then
    SetValueFromIndex(Idx, Value)
  else
  if Value <> '' then
    Add(Name + NameValueSeparator + Value);
end;

procedure TWStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
var
  S: WideString;
  I: Integer;
begin
  if Value = '' then
    Delete(Index)
  else
  begin
    if Index < 0 then
      Index := Add('');
    S := GetP(Index)^;
    I := WidePos(NameValueSeparator, S);
    if I > 0 then
      System.Delete(S, I, MaxInt);
    S := S + NameValueSeparator + Value;
    Put(Index, S);
  end;
end;

procedure TWStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
     Writer.WriteWideString(GetP(I)^);
  Writer.WriteListEnd;
end;

//=== { TWStringList } =======================================================

constructor TWStringList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TWStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  Inc(FUpdateCount); // do not call unnecessary functions
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TWStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := Count
  else
  if Find(S, Result) then
    case Duplicates of
      dupIgnore:
        Exit;
      dupError:
        raise EListError.Create(SDuplicateString);
    end;
  InsertObject(Result, S, AObject);
end;

procedure TWStringList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TWStringList.Changing;
begin
  if Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TWStringList.Clear;
var
  I: Integer;
  Item: PWStringItem;
begin
  if FUpdateCount = 0 then
    Changing;
  for I := 0 to Count - 1 do
  begin
    Item := PWStringItem(FList[I]);
    Item.FString := '';
    FreeMem(Item);
  end;
  FList.Clear;
  if FUpdateCount = 0 then
    Changed;
end;

function TWStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  if CaseSensitive then
    Result := WideCompareStr(S1, S2)
  else
    Result := WideCompareText(S1, S2);
end;

threadvar
  CustomSortList: TWStringList;
  CustomSortCompare: TWStringListSortCompare;

function WStringListCustomSort(Item1, Item2: Pointer): Integer;
begin
  Result := CustomSortCompare(CustomSortList,
    CustomSortList.FList.IndexOf(Item1),
    CustomSortList.FList.IndexOf(Item2));
end;

procedure TWStringList.CustomSort(Compare: TWStringListSortCompare);
var
  TempList: TWStringList;
  TempCompare: TWStringListSortCompare;
begin
  TempList := CustomSortList;
  TempCompare := CustomSortCompare;
  CustomSortList := Self;
  CustomSortCompare := Compare;
  try
    Changing;
    FList.Sort(WStringListCustomSort);
    Changed;
  finally
    CustomSortList := TempList;
    CustomSortCompare := TempCompare;
  end;
end;

procedure TWStringList.Delete(Index: Integer);
var
  Item: PWStringItem;
begin
  if FUpdateCount = 0 then
    Changing;
  Item := PWStringItem(FList[Index]);
  FList.Delete(Index);
  Item.FString := '';
  FreeMem(Item);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TWStringList.Exchange(Index1, Index2: Integer);
begin
  if FUpdateCount = 0 then
    Changing;
  FList.Exchange(Index1, Index2);
  if FUpdateCount = 0 then
    Changed;
end;

function TWStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  if Sorted then
  begin
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompareStrings(GetItem(I).FString, S);
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
  end
  else
  begin
    Index := IndexOf(S);
    Result := Index <> -1;
  end;
end;

function TWStringList.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

function TWStringList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TWStringList.GetItem(Index: Integer): PWStringItem;
begin
  Result := FList[Index];
end;

function TWStringList.GetObject(Index: Integer): TObject;
begin
  Result := GetItem(Index).FObject;
end;

function TWStringList.GetP(Index: Integer): PWideString;
begin
  Result := @GetItem(Index).FString;
end;

function TWStringList.IndexOf(const S: WideString): Integer;
begin
  if Sorted then
  begin
    if not Find(S, Result) then
      Result := -1;
  end
  else
  begin
    for Result := 0 to Count - 1 do
      if CompareStrings(GetItem(Result).FString, S) = 0 then
        Exit;
    Result := -1;
  end;
end;

procedure TWStringList.InsertObject(Index: Integer; const S: WideString;
  AObject: TObject);
var
  P: PWStringItem;
begin
  if FUpdateCount = 0 then
    Changing;
  FList.Insert(Index, nil); // error check
  P := AllocMem(SizeOf(TWStringItem));
  FList[Index] := P;

  Put(Index, S);
  if AObject <> nil then
    PutObject(Index, AObject);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TWStringList.Put(Index: Integer; const Value: WideString);
begin
  if FUpdateCount = 0 then
    Changing;
  GetItem(Index).FString := Value;
  if FUpdateCount = 0 then
    Changed;
end;

procedure TWStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if FUpdateCount = 0 then
    Changing;
  GetItem(Index).FObject := AObject;
  if FUpdateCount = 0 then
    Changed;
end;

procedure TWStringList.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

procedure TWStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      Sorted := False;
      Sorted := True; // re-sort
    end;
  end;
end;

procedure TWStringList.SetSorted(Value: Boolean);
begin
  if Value <> FSorted then
  begin
    FSorted := Value;
    if FSorted then
    begin
      FSorted := False;
      Sort;
      FSorted := True;
    end;
  end;
end;

procedure TWStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

function DefaultSort(List: TWStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.GetItem(Index1).FString, List.GetItem(Index2).FString);
end;

procedure TWStringList.Sort;
begin
  if not Sorted then
    CustomSort(DefaultSort);
end;

initialization
  LoadInProgress := TCriticalSection.Create;

finalization
  LoadInProgress.Free;


end.
