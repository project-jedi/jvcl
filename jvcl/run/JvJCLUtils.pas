{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJCLUtils.pas, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Andreas Hausladen
  Ralf Kaiser
  Vladimir Gaitanoff
  Dejoy den

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

unit JvJCLUtils;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

// (p3) note: this unit should only contain JCL compatible routines (no Forms etc)
// and no JVCL units!

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Messages, ShlObj, ActiveX,
  {$ENDIF MSWINDOWS}
  Types,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  Variants, SysUtils, Classes, Contnrs, Graphics, Clipbrd, Controls,
  StrUtils, TypInfo,
  JclBase,
  JvTypes;

const
  {$IFDEF MSWINDOWS}
  PathDelim = '\';
  DriveDelim = ':';
  PathSep = ';';
  AllFilesMask = '*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  PathDelim = '/';
  AllFilesMask = '*';
  {$ENDIF UNIX}
    // Note: the else is on purpose, VCL is not defined for a console application
  NullHandle = 0;
  USDecimalSeparator = '.';

{$IFNDEF COMPILER12_UP} // Delphi 2009 introduced it and fixed the NativeInt of Delphi 2007
type
  // Compatibility for older Delphi versions, so the JVCL doesn't need to IFDEFs every call to
  // SetWindowLongPtr/GetWindowLongPtr.
  NativeInt = Integer;

  {$EXTERNALSYM INT_PTR}
  INT_PTR = Integer;
  {$EXTERNALSYM LONG_PTR}
  LONG_PTR = Longint;
  {$EXTERNALSYM UINT_PTR}
  UINT_PTR = Cardinal;
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR = LongWord;
  {$EXTERNALSYM DWORD_PTR}
  DWORD_PTR = ULONG_PTR;

const
  GWLP_WNDPROC    = -4;
  {$EXTERNALSYM GWLP_WNDPROC}
  GWLP_HINSTANCE  = -6;
  {$EXTERNALSYM GWLP_HINSTANCE}
  GWLP_HWNDPARENT = -8;
  {$EXTERNALSYM GWLP_HWNDPARENT}
  GWLP_USERDATA   = -21;
  {$EXTERNALSYM GWLP_USERDATA}
  GWLP_ID         = -12;
  {$EXTERNALSYM GWLP_ID}

{$EXTERNALSYM GetWindowLongPtr}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;
{$EXTERNALSYM SetWindowLongPtr}
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;
{$ENDIF ~COMPILER12_UP}

type
  EJvConvertError = Class(EConvertError);  { subclass EConvertError raised by some non-Def versions of floating point conversion routine }
  {$IFDEF UNIX}
  TFileTime = Integer;
  {$ENDIF UNIX}

  {$IFNDEF RTL150_UP}
  TFormatSettings = record
    DecimalSeparator: Char;
  end;
  {$ENDIF RTL150_UP}

function SendRectMessage(Handle: THandle; Msg: Integer; wParam: WPARAM; var R: TRect): Integer;
function SendStructMessage(Handle: THandle; Msg: Integer; wParam: WPARAM; var Data): Integer;
function ReadCharsFromStream(Stream: TStream; var Buf: array of AnsiChar; BufSize: Integer): Integer; // ANSI-Stream
function WriteStringToStream(Stream: TStream; const Buf: AnsiString; BufSize: Integer): Integer; // ANSI-Stream

{$IFNDEF COMPILER12_UP}
function UTF8ToString(const S: UTF8String): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF ~COMPILER12_UP}

const
  DefaultDateOrder = doDMY;
  CenturyOffset: Byte = 60;
  NullDate: TDateTime = {-693594} 0;


//------------------------------------------------------------------------------------
// This 'USA' hack functionality is made useless by the JvSafeStrToFloatDef routine:
//------------------------------------------------------------------------------------
   //function USToLocalFloatStr(const Text: string): string; // deprecated.
   //function StrToFloatUS(const Text: string): Extended;
   // StrToFloatUS uses US '.' as decimal seperator and ',' as thousand separator
   //function StrToFloatUSDef(const Text: string; Default: Extended): Extended;


function VarIsInt(Value: Variant): Boolean;
 // VarIsInt returns VarIsOrdinal-[varBoolean]

{ PosIdx returns the index of the first appearance of SubStr in Str. The search
  starts at index "Index". }
function PosIdx(const SubStr, S: string; Index: Integer = 0): Integer;
function PosIdxW(const SubStr, S: WideString; Index: Integer = 0): Integer;
function PosLastCharIdx(Ch: Char; const S: string; Index: Integer = 0): Integer;

{ GetWordOnPos returns Word from string, S, on the cursor position, P}
function GetWordOnPos(const S: string; const P: Integer): string;
function GetWordOnPosW(const S: WideString; const P: Integer): WideString;
function GetWordOnPos2(const S: string; P: Integer; var iBeg, iEnd: Integer): string;
function GetWordOnPos2W(const S: WideString; P: Integer; var iBeg, iEnd: Integer): WideString;
{ GetWordOnPosEx working like GetWordOnPos function, but
  also returns Word position in iBeg, iEnd variables }
function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string;
function GetWordOnPosExW(const S: WideString; const P: Integer; var iBeg, iEnd: Integer): WideString;
function GetNextWordPosEx(const Text: string; StartIndex: Integer;
  var iBeg, iEnd: Integer): string;
function GetNextWordPosExW(const Text: WideString; StartIndex: Integer;
  var iBeg, iEnd: Integer): WideString;
procedure GetEndPosCaret(const Text: string; CaretX, CaretY: Integer;
  var X, Y: Integer);
{ GetEndPosCaret returns the caret position of the last char. For the position
  after the last char of Text you must add 1 to the returned X value. }
procedure GetEndPosCaretW(const Text: WideString; CaretX, CaretY: Integer;
  var X, Y: Integer);
{ GetEndPosCaret returns the caret position of the last char. For the position
  after the last char of Text you must add 1 to the returned X value. }

{ SubStrBySeparator returns substring from string, S, separated with Separator string}
function SubStrBySeparator(const S: string; const Index: Integer; const Separator: string; StartIndex: Integer = 1): string;
function SubStrBySeparatorW(const S: WideString; const Index: Integer; const Separator: WideString; StartIndex: Integer = 1): WideString;
{ SubStrEnd same to previous function but Index numerated from the end of string }
//function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
{ SubWord returns next Word from string, P, and offsets Pointer to the end of Word, P2 }
function SubWord(P: PChar; var P2: PChar): string;
//  function CurrencyByWord(Value: Currency): string;
{ GetLineByPos returns the Line number, there
  the symbol Pos is pointed. Lines separated with #13 symbol }
function GetLineByPos(const S: string; const Pos: Integer): Integer;
{ GetXYByPos is same as GetLineByPos, but returns X position in line as well}
procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
procedure GetXYByPosW(const S: WideString; const Pos: Integer; var X, Y: Integer);
{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
function ReplaceString(S: string; const OldPattern, NewPattern: string; StartIndex: Integer = 1): string;
function ReplaceStringW(S: WideString; const OldPattern, NewPattern: WideString; StartIndex: Integer = 1): WideString;
{ ConcatSep concatenate S1 and S2 strings with Separator.
  if S = '' then separator not included }
function ConcatSep(const S1, S2, Separator: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ ConcatLeftSep is same to previous function, but
  strings concatenate right to left }
function ConcatLeftSep(const S1, S2, Separator: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

{ Next 4 function for russian chars transliterating.
  This functions are needed because Oem2Ansi and Ansi2Oem functions
  sometimes suck }
procedure Dos2Win(var S: AnsiString);
procedure Win2Dos(var S: AnsiString);
function Dos2WinRes(const S: AnsiString): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function Win2DosRes(const S: AnsiString): AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function Win2Koi(const S: AnsiString): AnsiString;

{ FillString fills the string Buffer with Count Chars }
procedure FillString(var Buffer: string; Count: Integer; const Value: Char); overload;
procedure FillString(var Buffer: string; StartIndex, Count: Integer; const Value: Char); overload;
{ MoveString copies Count Chars from Source to Dest }
procedure MoveString(const Source: string; var Dest: string; Count: Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE} overload;
procedure MoveString(const Source: string; SrcStartIdx: Integer; var Dest: string;
  DstStartIdx: Integer; Count: Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE} overload;
{ FillWideChar fills Buffer with Count WideChars (2 Bytes) }
procedure FillWideChar(var Buffer; Count: Integer; const Value: WideChar);
{ MoveWideChar copies Count WideChars from Source to Dest }
procedure MoveWideChar(const Source; var Dest; Count: Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ FillNativeChar fills Buffer with Count NativeChars }
procedure FillNativeChar(var Buffer; Count: Integer; const Value: Char); // D2009 internal error {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ MoveWideChar copies Count WideChars from Source to Dest }
procedure MoveNativeChar(const Source; var Dest; Count: Integer); // D2009 internal error {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ IsSubString() compares the sub string to the string. Indices are 1th based. }
function IsSubString(const S: string; StartIndex: Integer; const SubStr: string): Boolean;

{ Spaces returns string consists on N space chars }
function Spaces(const N: Integer): string;
{ AddSpaces adds spaces to string S, if its Length is smaller than N }
function AddSpaces(const S: string; const N: Integer): string;
function SpacesW(const N: Integer): WideString;
function AddSpacesW(const S: WideString; const N: Integer): WideString;
{ function LastDateRUS for russian users only }
{ returns date relative to current date: 'два дня назад' }
function LastDateRUS(const Dat: TDateTime): string;
{ CurrencyToStr format Currency, Cur, using ffCurrency float format}
function CurrencyToStr(const Cur: Currency): string;
{ HasChar returns True, if Char, Ch, contains in string, S }
function HasChar(const Ch: Char; const S: string): Boolean;
function HasCharW(const Ch: WideChar; const S: WideString): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function HasAnyChar(const Chars: string; const S: string): Boolean;
{$IFNDEF COMPILER12_UP}
function ToUpper(C: Char): Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{$ENDIF ~COMPILER12_UP}
{$IFNDEF COMPILER12_UP}
function CharInSet(const Ch: AnsiChar; const SetOfChar: TSysCharSet): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{$ENDIF ~COMPILER12_UP}
function CharInSetW(const Ch: WideChar; const SetOfChar: TSysCharSet): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function CountOfChar(const Ch: Char; const S: string): Integer;
function DefStr(const S: string; Default: string): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}

{ StrLICompW2 is a faster replacement for JclUnicode.StrLICompW }
function StrLICompW2(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrPosW(S, SubStr: PWideChar): PWideChar;
function StrLenW(S: PWideChar): Integer;
function TrimW(const S: WideString): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TrimLeftW(const S: WideString): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function TrimRightW(const S: WideString): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{**** files routines}
procedure SetDelimitedText(List: TStrings; const Text: string; Delimiter: Char);

const
  {$IFDEF MSWINDOWS}
  DefaultCaseSensitivity = False;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  DefaultCaseSensitivity = True;
  {$ENDIF UNIX}

{ GenTempFileName returns temporary file name on
  drive, there FileName is placed }
function GenTempFileName(FileName: string): string;
{ GenTempFileNameExt same to previous function, but
  returning filename has given extension, FileExt }
function GenTempFileNameExt(FileName: string; const FileExt: string): string;
{ ClearDir clears folder Dir }
function ClearDir(const Dir: string): Boolean;
{ DeleteDir clears and than delete folder Dir }
function DeleteDir(const Dir: string): Boolean;
{ FileEquMask returns True if file, FileName,
  is compatible with given dos file mask, Mask }
function FileEquMask(FileName, Mask: TFileName;
  CaseSensitive: Boolean = DefaultCaseSensitivity): Boolean;
{ FileEquMasks returns True if file, FileName,
  is compatible with given Masks.
  Masks must be separated with SepPath (MSW: ';' / UNIX: ':') }
function FileEquMasks(FileName, Masks: TFileName;
  CaseSensitive: Boolean = DefaultCaseSensitivity): Boolean;
function DeleteFiles(const Folder: TFileName; const Masks: string): Boolean;

{$IFDEF MSWINDOWS}
{ LZFileExpand expand file, FileSource,
  into FileDest. Given file must be compressed, using MS Compress program }
function LZFileExpand(const FileSource, FileDest: string): Boolean;
{$ENDIF MSWINDOWS}

{ FileGetInfo fills SearchRec record for specified file attributes}
function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): Boolean;
{ HasSubFolder returns True, if folder APath contains other folders }
function HasSubFolder(APath: TFileName): Boolean;
{ IsEmptyFolder returns True, if there are no files or
  folders in given folder, APath}
function IsEmptyFolder(APath: TFileName): Boolean;
{ AddSlash returns string with added slash Char to Dir parameter, if needed }
function AddSlash(const Dir: TFileName): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ AddPath returns FileName with Path, if FileName not contain any path }
function AddPath(const FileName, Path: TFileName): TFileName;
function AddPaths(const PathList, Path: string): string;
function ParentPath(const Path: TFileName): TFileName;
function FindInPath(const FileName, PathList: string): TFileName;
{ DeleteReadOnlyFile clears R/O file attribute and delete file }
function DeleteReadOnlyFile(const FileName: TFileName): Boolean;
{ HasParam returns True, if program running with specified parameter, Param }
function HasParam(const Param: string): Boolean;
function HasSwitch(const Param: string): Boolean;
function Switch(const Param: string): string;
{ ExePath returns ExtractFilePath(ParamStr(0)) }
function ExePath: TFileName; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
//function FileTimeToDateTime(const FT: TFileTime): TDateTime;
procedure FileTimeToDosDateTimeDWord(const FT: TFileTime; out Dft: DWORD);
function MakeValidFileName(const FileName: TFileName; ReplaceBadChar: Char): TFileName;

{**** Graphic routines }


{ IsTTFontSelected returns True, if True Type font
  is selected in specified device context }
function IsTTFontSelected(const DC: HDC): Boolean;
function KeyPressed(VK: Integer): Boolean;




{ TrueInflateRect inflates rect in other method, than InflateRect API function }
function TrueInflateRect(const R: TRect; const I: Integer): TRect;
{**** Color routines }
procedure RGBToHSV(R, G, B: Integer; var H, S, V: Integer);
function RGBToBGR(Value: Cardinal): Cardinal;
function ColorToPrettyName(Value: TColor): string;
function PrettyNameToColor(const Value: string): TColor;

{**** other routines }
procedure SwapInt(var Int1, Int2: Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function IntPower(Base, Exponent: Integer): Integer;
function StrToBool(const S: string): Boolean;

function Var2Type(V: Variant; const DestVarType: Integer): Variant;
function VarToInt(V: Variant): Integer;
function VarToFloat(V: Variant): Double;

{ following functions are not documented
  because they do not work properly sometimes, so do not use them }
// (rom) ReplaceStrings1, GetSubStr removed

function GetLongFileName(const FileName: string): string;
function FileNewExt(const FileName, NewExt: TFileName): TFileName;
function GetParameter: string;
function GetComputerID: string;
function GetComputerName: string;

{**** string routines }

{ ReplaceAllStrings searches for all substrings, Words,
  in a string, S, and replaces them with Frases with the same Index. }
function ReplaceAllStrings(const S: string; Words, Frases: TStrings): string;
{ ReplaceStrings searches the Word in a string, S, on PosBeg position,
  in the list, Words, and if founds, replaces this Word
  with string from another list, Frases, with the same Index,
  and then update NewSelStart variable }
function ReplaceStrings(const S: string; PosBeg, Len: Integer; Words, Frases: TStrings; var NewSelStart: Integer): string;
{ CountOfLines calculates the lines count in a string, S,
  each line must be separated from another with CrLf sequence }
function CountOfLines(const S: string): Integer;
{ DeleteLines deletes all lines from strings which in the words,  words.
  The word of will be deleted from strings. }
procedure DeleteOfLines(Ss: TStrings; const Words: array of string);
{ DeleteEmptyLines deletes all empty lines from strings, Ss.
  Lines contained only spaces also deletes. }
procedure DeleteEmptyLines(Ss: TStrings);
{ SQLAddWhere addes or modifies existing where-statement, where,
  to the strings, SQL.
  Note: If strings SQL allready contains where-statement,
  it must be started on the begining of any line }
procedure SQLAddWhere(SQL: TStrings; const Where: string);

{**** files routines - }

{$IFDEF MSWINDOWS}
{ ResSaveToFile save resource named as Name with Typ type into file FileName.
  Resource can be compressed using MS Compress program}
function ResSaveToFile(const Typ, Name: string; const Compressed: Boolean; const FileName: string): Boolean;
function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar;
  const Compressed: Boolean; const FileName: string): Boolean;
function ResSaveToString(Instance: HINST; const Typ, Name: string;
  var S: string): Boolean;
{$ENDIF MSWINDOWS}
{ IniReadSection read section, Section, from ini-file,
  IniFileName, into strings, Ss.
  This function reads ALL strings from specified section.
  Note: TIninFile.ReadSection function reads only strings with '=' symbol.}
function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): Boolean;
{ LoadTextFile load text file, FileName, into string }
function LoadTextFile(const FileName: TFileName): string;
procedure SaveTextFile(const FileName: TFileName; const Source: string);
{ ReadFolder reads files list from disk folder, Folder,
  that are equal to mask, Mask, into strings, FileList}
function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): Integer;
function ReadFolders(const Folder: TFileName; FolderList: TStrings): Integer;

{ RATextOut same with TCanvas.TextOut procedure, but
  can clipping drawing with rectangle, RClip. }
procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string);
{ RATextOutEx same with RATextOut function, but
  can calculate needed height for correct output }
function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string; const CalcHeight: Boolean): Integer;
{ RATextCalcHeight calculate needed height for
  correct output, using RATextOut or RATextOutEx functions }
function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): Integer;
{ Cinema draws some visual effect }
procedure Cinema(Canvas: TCanvas; rS {Source}, rD {Dest}: TRect);
{ Roughed fills rect with special 3D pattern }
procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: Boolean);
{ BitmapFromBitmap creates new small bitmap from part
  of source bitmap, SrcBitmap, with specified width and height,
  AWidth, AHeight and placed on a specified Index, Index in the
  source bitmap }
function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, Index: Integer): TBitmap;
{ TextWidth calculate text with for writing using standard desktop font }
function TextWidth(const AStr: string): Integer;
{ TextHeight calculate text height for writing using standard desktop font }
function TextHeight(const AStr: string): Integer;

procedure SetChildPropOrd(Owner: TComponent; const PropName: string; Value: Longint);
procedure Error(const Msg: string);
procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
{ example for Text parameter :
  'Item 1 <b>bold</b> <i>italic ITALIC <c:Red>red <c:Green>green <c:blue>blue </i>' }
function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;
function ItemHtPlain(const Text: string): string;
{ ClearList - clears list of TObject }
procedure ClearList(List: TList);

procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: Word);
procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: Word);

{ RTTI support }
function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
function GetPropStr(Obj: TObject; const PropName: string): string;
function GetPropOrd(Obj: TObject; const PropName: string): Integer;
function GetPropMethod(Obj: TObject; const PropName: string): TMethod;

procedure PrepareIniSection(Ss: TStrings);
{ following functions are not documented because
  they are don't work properly, so don't use them }

// (rom) from JvBandWindows to make it obsolete
function PointL(const X, Y: Longint): TPointL; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
// (rom) from JvBandUtils to make it obsolete
function iif(const Test: Boolean; const ATrue, AFalse: Variant): Variant; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}


procedure CopyIconToClipboard(Icon: TIcon; BackColor: TColor);
function CreateIconFromClipboard: TIcon;
{ begin JvIconClipboardUtils }
{ Icon clipboard routines }
function CF_ICON: Word;

procedure AssignClipboardIcon(Icon: TIcon);

{ Real-size icons support routines (32-bit only) }
procedure GetIconSize(Icon: HICON; var W, H: Integer);
function CreateRealSizeIcon(Icon: TIcon): HICON;
procedure DrawRealSizeIcon(Canvas: TCanvas; Icon: TIcon; X, Y: Integer);
{end JvIconClipboardUtils }

function CreateScreenCompatibleDC: HDC;

{$IFNDEF COMPILER12_UP} // Delphi 2009 introduced the "TRect" overload
function InvalidateRect(hWnd: HWND; const lpRect: TRect; bErase: BOOL): BOOL; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function InvalidateRect(hWnd: HWND; lpRect: PRect; bErase: BOOL): BOOL; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF ~COMPILER12_UP}

{ begin JvRLE }

// (rom) changed API for inclusion in JCL

procedure RleCompressTo(InStream, OutStream: TStream);
procedure RleDecompressTo(InStream, OutStream: TStream);
procedure RleCompress(Stream: TStream);
procedure RleDecompress(Stream: TStream);
{ end JvRLE }

{ begin JvDateUtil }
function CurrentYear: Word; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function IsLeapYear(AYear: Integer): Boolean;
function DaysInAMonth(const AYear, AMonth: Word): Word;
function DaysPerMonth(AYear, AMonth: Integer): Integer;
function FirstDayOfPrevMonth: TDateTime;
function LastDayOfPrevMonth: TDateTime;
function FirstDayOfNextMonth: TDateTime;
function ExtractDay(ADate: TDateTime): Word;
function ExtractMonth(ADate: TDateTime): Word;
function ExtractYear(ADate: TDateTime): Word;
function IncDate(ADate: TDateTime; Days, Months, Years: Integer): TDateTime;
function IncDay(ADate: TDateTime; Delta: Integer = 1): TDateTime; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
function IncMonth(ADate: TDateTime; Delta: Integer = 1): TDateTime;
function IncYear(ADate: TDateTime; Delta: Integer = 1): TDateTime;
function ValidDate(ADate: TDateTime): Boolean;
procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Word);
function MonthsBetween(Date1, Date2: TDateTime): Double;
function DaysInPeriod(Date1, Date2: TDateTime): Longint;
{ Count days between Date1 and Date2 + 1, so if Date1 = Date2 result = 1 }
function DaysBetween(Date1, Date2: TDateTime): Longint;
{ The same as previous but if Date2 < Date1 result = 0 }
function IncTime(ATime: TDateTime; Hours, Minutes, Seconds, MSecs: Integer): TDateTime;
function IncHour(ATime: TDateTime; Delta: Integer): TDateTime;
function IncMinute(ATime: TDateTime; Delta: Integer): TDateTime;
function IncSecond(ATime: TDateTime; Delta: Integer): TDateTime;
function IncMSec(ATime: TDateTime; Delta: Integer): TDateTime;
function CutTime(ADate: TDateTime): TDateTime; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE} { Set time to 00:00:00:00 }

{ String to date conversions }
function GetDateOrder(const DateFormat: string): TDateOrder;
function MonthFromName(const S: string; MaxLen: Byte): Byte;
function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
function StrToDateFmt(const DateFormat, S: string): TDateTime;
function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
function DefDateFormat(AFourDigitYear: Boolean): string;
function DefDateMask(BlanksChar: Char; AFourDigitYear: Boolean): string;

function FormatLongDate(Value: TDateTime): string;
function FormatLongDateTime(Value: TDateTime): string;
{ end JvDateUtil }
function BufToBinStr(Buf: Pointer; BufSize: Integer): string;
function BinStrToBuf(Value: string; Buf: Pointer; BufSize: Integer): Integer;


{ begin JvStrUtils }

  { ** Common string handling routines ** }

{$IFDEF UNIX}
function iconversion(InP: PAnsiChar; OutP: Pointer; InBytes, OutBytes: Cardinal;
  const ToCode, FromCode: AnsiString): Boolean;
function iconvString(const S, ToCode, FromCode: AnsiString): string;
function iconvWideString(const S: WideString; const ToCode, FromCode: AnsiString): WideString;
function OemStrToAnsi(const S: AnsiString): AnsiString;
function AnsiStrToOem(const S: AnsiString): AnsiString;
{$ENDIF UNIX}

function StrToOem(const AnsiStr: AnsiString): AnsiString;
{ StrToOem translates a string from the Windows character set into the
  OEM character set. }
function OemToAnsiStr(const OemStr: AnsiString): AnsiString;
{ OemToAnsiStr translates a string from the OEM character set into the
  Windows character set. }
function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
{ EmptyStr returns True if the given string contains only character
  from the EmptyChars. }
function ReplaceStr(const S, Srch, Replace: string): string;
{ Returns string with every occurrence of Srch string replaced with
  Replace string. }
function DelSpace(const S: string): string;
{ DelSpace return a string with all white spaces removed. }
function DelChars(const S: string; Chr: Char): string;
{ DelChars return a string with all Chr characters removed. }
function DelBSpace(const S: string): string;
{ DelBSpace trims leading spaces from the given string. }
function DelESpace(const S: string): string;
{ DelESpace trims trailing spaces from the given string. }
function DelRSpace(const S: string): string;
{ DelRSpace trims leading and trailing spaces from the given string. }
function DelSpace1(const S: string): string;
{ DelSpace1 return a string with all non-single white spaces removed. }
function Tab2Space(const S: string; Numb: Byte): string;
{ Tab2Space converts any tabulation character in the given string to the
  Numb spaces characters. }
function NPos(const C: string; S: string; N: Integer): Integer;
{ NPos searches for a N-th position of substring C in a given string. }
function MakeStr(C: Char; N: Integer): string; overload;
{$IFNDEF COMPILER12_UP}
function MakeStr(C: WideChar; N: Integer): WideString; overload;
{$ENDIF !COMPILER12_UP}
function MS(C: Char; N: Integer): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ MakeStr return a string of length N filled with character C. }
function AddChar(C: Char; const S: string; N: Integer): string;
{ AddChar return a string left-padded to length N with characters C. }
function AddCharR(C: Char; const S: string; N: Integer): string;
{ AddCharR return a string right-padded to length N with characters C. }
function LeftStr(const S: string; N: Integer): string;
{ LeftStr return a string right-padded to length N with blanks. }
function RightStr(const S: string; N: Integer): string;
{ RightStr return a string left-padded to length N with blanks. }
function CenterStr(const S: string; Len: Integer): string;
{ CenterStr centers the characters in the string based upon the
  Len specified. }
function CompStr(const S1, S2: string): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ CompStr compares S1 to S2, with case-sensitivity. The return value is
  -1 if S1 < S2, 0 if S1 = S2, or 1 if S1 > S2. }
function CompText(const S1, S2: string): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ CompText compares S1 to S2, without case-sensitivity. The return value
  is the same as for CompStr. }
function Copy2Symb(const S: string; Symb: Char): string;
{ Copy2Symb returns a substring of a string S from begining to first
  character Symb. }
function Copy2SymbDel(var S: string; Symb: Char): string;
{ Copy2SymbDel returns a substring of a string S from begining to first
  character Symb and removes this substring from S. }
function Copy2Space(const S: string): string;
{ Copy2Symb returns a substring of a string S from begining to first
  white space. }
function Copy2SpaceDel(var S: string): string;
{ Copy2SpaceDel returns a substring of a string S from begining to first
  white space and removes this substring from S. }
function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
{ Returns string, with the first letter of each word in uppercase,
  all other letters in lowercase. Words are delimited by WordDelims. }
function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
{ WordCount given a set of word delimiters, returns number of words in S. }
function WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
{ Given a set of word delimiters, returns start position of N'th word in S. }
function ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;
function ExtractWordPos(N: Integer; const S: string;
  const WordDelims: TSysCharSet; var Pos: Integer): string;
function ExtractDelimited(N: Integer; const S: string;
  const Delims: TSysCharSet): string;
{ ExtractWord, ExtractWordPos and ExtractDelimited given a set of word
  delimiters, return the N'th word in S. }
function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
{ ExtractSubstr given a set of word delimiters, returns the substring from S,
  that started from position Pos. }
function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
{ IsWordPresent given a set of word delimiters, returns True if word W is
  present in string S. }
function QuotedString(const S: string; Quote: Char): string;
{ QuotedString returns the given string as a quoted string, using the
  provided Quote character. }
function ExtractQuotedString(const S: string; Quote: Char): string;
{ ExtractQuotedString removes the Quote characters from the beginning and
  end of a quoted string, and reduces pairs of Quote characters within
  the quoted string to a single character. }
function FindPart(const HelpWilds, InputStr: string): Integer;
{ FindPart compares a string with '?' and another, returns the position of
  HelpWilds in InputStr. }
function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;
{ IsWild compares InputString with WildCard string and returns True
  if corresponds. }
function XorString(const Key, Src: ShortString): ShortString;
function XorEncode(const Key, Source: string): string;
  {$IFDEF SUPPORTS_DEPRECATED}deprecated{$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'use XorEncodeString that has support for non-ASCII chars'{$ENDIF};{$ENDIF}
function XorDecode(const Key, Source: string): string;
  {$IFDEF SUPPORTS_DEPRECATED}deprecated{$IFDEF SUPPORTS_DEPRECATED_DETAILS} 'use XorEncodeString that has support for non-ASCII chars'{$ENDIF};{$ENDIF}
function XorEncodeString(const Key, Source: string): string;
function XorDecodeString(const Key, Source: string): string;

{ ** Command line routines ** }

function GetCmdLineArg(const Switch: string; ASwitchChars: TSysCharSet): string;

{ ** Numeric string handling routines ** }

function Numb2USA(const S: string): string;
{ Numb2USA converts numeric string S to USA-format. }
function Dec2Hex(N: Longint; A: Byte): string; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF SUPPORTS_INLINE}
{ Dec2Hex converts the given value to a hexadecimal string representation
  with the minimum number of digits (A) specified. }
function Hex2Dec(const S: string): Longint;
{ Hex2Dec converts the given hexadecimal string to the corresponding integer
  value. }
function Dec2Numb(N: Int64; A, B: Byte): string;
{ Dec2Numb converts the given value to a string representation with the
  base equal to B and with the minimum number of digits (A) specified. }
function Numb2Dec(S: string; B: Byte): Int64;
{ Numb2Dec converts the given B-based numeric string to the corresponding
  integer value. }
function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
{ IntToBin converts the given value to a binary string representation
  with the minimum number of digits specified. }
function IntToRoman(Value: Longint): string;
{ IntToRoman converts the given value to a roman numeric string
  representation. }
function RomanToInt(const S: string): Longint;
{ RomanToInt converts the given string to an integer value. If the string
  doesn't contain a valid roman numeric value, the 0 value is returned. }

function FindNotBlankCharPos(const S: string): Integer;
function FindNotBlankCharPosW(const S: WideString): Integer;
function AnsiChangeCase(const S: string): string;
function WideChangeCase(const S: string): string;

function StartsText(const SubStr, S: string): Boolean;
function EndsText(const SubStr, S: string): Boolean;

function DequotedStr(const S: string; QuoteChar: Char = ''''): string;
function AnsiDequotedStr(const S: string; AQuote: Char): string; // follow Delphi 2009's "Ansi" prefix

{end JvStrUtils}

{$IFDEF UNIX}
function GetTempFileName(const Prefix: AnsiString): AnsiString;
{$ENDIF UNIX}

function HasAttr(const FileName: string; Attr: Integer): Boolean;
function DeleteFilesEx(const FileMasks: array of string): Boolean;
function NormalDir(const DirName: string): string;
function RemoveBackSlash(const DirName: string): string; // only for Windows/DOS Paths
function ValidFileName(const FileName: string): Boolean;

{$IFDEF MSWINDOWS}
function FileLock(Handle: Integer; Offset, LockSize: Longint): Integer; overload;
function FileLock(Handle: Integer; Offset, LockSize: Int64): Integer; overload;
function FileUnlock(Handle: Integer; Offset, LockSize: Longint): Integer; overload;
function FileUnlock(Handle: Integer; Offset, LockSize: Int64): Integer; overload;
{$ENDIF MSWINDOWS}
function GetWindowsDir: string;
function GetSystemDir: string;

function ShortToLongFileName(const ShortName: string): string;
function LongToShortFileName(const LongName: string): string;
function ShortToLongPath(const ShortName: string): string;
function LongToShortPath(const LongName: string): string;
{$IFDEF MSWINDOWS}
procedure CreateFileLink(const FileName, DisplayName: string; Folder: Integer);
procedure DeleteFileLink(const DisplayName: string; Folder: Integer);
{$ENDIF MSWINDOWS}

{ end JvFileUtil }

// Works like PtInRect but includes all edges in comparision
function PtInRectInclusive(R: TRect; Pt: TPoint): Boolean;
// Works like PtInRect but excludes all edges from comparision
function PtInRectExclusive(R: TRect; Pt: TPoint): Boolean;

function FourDigitYear: Boolean; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function IsFourDigitYear: Boolean;

{ moved from JvJVCLUTils }

//Open an object with the shell (url or something like that)
function OpenObject(const Value: string): Boolean; overload;
function OpenObject(Value: PChar): Boolean; overload;

{$IFDEF MSWINDOWS}
//Raise the last Exception
procedure RaiseLastWin32; overload;
procedure RaiseLastWin32(const Text: string); overload;
//Raise the last Exception with a small comment from your part

{ GetFileVersion returns the most significant 32 bits of a file's binary
  version number. Typically, this includes the major and minor version placed
  together in one 32-bit Integer. It generally does not include the release
  or build numbers. It returns 0 if it failed. }
function GetFileVersion(const AFileName: string): Cardinal;
{$EXTERNALSYM GetFileVersion}

//Get version of Shell.dll
function GetShellVersion: Cardinal;
{$EXTERNALSYM GetShellVersion}

// CD functions
procedure OpenCdDrive;
procedure CloseCdDrive;

// returns True if Drive is accessible
function DiskInDrive(Drive: Char): Boolean;
{$ENDIF MSWINDOWS}

//Same as linux function ;)
procedure PError(const Text: string);

// execute a program without waiting
procedure Exec(const FileName, Parameters, Directory: string);
// execute a program and wait for it to finish
function ExecuteAndWait(CommandLine: string; const WorkingDirectory: string; Visibility: Integer = SW_SHOW): Integer;


// returns True if this is the first instance of the program that is running
function FirstInstance(const ATitle: string): Boolean;
// restores a window based on it's classname and Caption. Either can be left empty
// to widen the search
procedure RestoreOtherInstance(const MainFormClassName, MainFormCaption: string);

// manipulate the traybar and start button
procedure HideTraybar;
procedure ShowTraybar;
procedure ShowStartButton(Visible: Boolean = True);

// (rom) SC_MONITORPOWER is documented as Windows 95 only
// (rom) better do some testing
// set monitor functions
procedure MonitorOn;
procedure MonitorOff;
procedure LowPower;

// send a key to the window named AppName
function SendKey(const AppName: string; Key: Char): Boolean;

{$IFDEF MSWINDOWS}

// returns a list of all windows currently visible, the Objects property is filled with their window handle
procedure GetVisibleWindows(List: TStrings);
// associates an extension to a specific program
procedure AssociateExtension(const IconPath, ProgramName, Path, Extension: string);

procedure AddToRecentDocs(const FileName: string);
function GetRecentDocs: TStringList;
{$ENDIF MSWINDOWS}

// JvComponentFunctions
{-----------------------------------------------------------------------------
Comments:
  Functions pulled out of MemoEx, used in MemoEx.pas and TypedEdit.pas

  This unit has low internal cohesion (ie it contains routines that do all kinds of stuff)
  Some are very good candidates for wider reuse
  some are quite specific to the controls
  and in a larger library this unit would be broken up

  I have tried to group related functions together
}

function CharIsMoney(const Ch: Char): Boolean;

{ there is a STrToIntDef provided by Delphi, but no "safe" versions of
  StrToFloat or StrToCurr }
// Note: before using JvSafeStrToFloatDef, please be aware that it will ignore
// any character that is not a valid character for a float, which is different
// from what StrToFloatDef in Delphi 6 up is doing. This has been documented in Mantis
// issue# 2935: http://issuetracker.delphi-jedi.org/view.php?id=2935
// and in Mantis 4466: http://issuetracker.delphi-jedi.org/view.php?id=4466

function JvSafeStrToFloatDef(const Str: string; Def: Extended; aDecimalSeparator: Char = ' '): Extended; {NOTE: default value of Space is a magic wildcard}

function JvSafeStrToFloat(const Str: string; aDecimalSeparator: Char = ' '): Extended; {NOTE: default value of Space is a magic wildcard}


function StrToCurrDef(const Str: string; Def: Currency): Currency;
function IntToExtended(I: Integer): Extended;

{ GetChangedText works out the new text given the current cursor pos & the key pressed
  It is not very useful in other contexts,
  but it is in this unit as it is needed in both MemoEx and TypedEdit }
function GetChangedText(const Text: string; SelStart, SelLength: Integer; Key: Char): string;

function MakeYear4Digit(Year, Pivot: Integer): Integer;

function StrIsInteger(const S: string): Boolean;
function StrIsFloatMoney(const Ps: string): Boolean;
function StrIsDateTime(const Ps: string): Boolean;

function PreformatDateString(Ps: string): string;

function BooleanToInteger(const B: Boolean): Integer;
function StringToBoolean(const Ps: string): Boolean;

function SafeStrToDateTime(const Ps: string): TDateTime;
function SafeStrToDate(const Ps: string): TDateTime;
function SafeStrToTime(const Ps: string): TDateTime;

function StrDelete(const psSub, psMain: string): string;

  { returns the fractional value of pcValue}
function TimeOnly(pcValue: TDateTime): TTime;
{ returns the integral value of pcValue }
function DateOnly(pcValue: TDateTime): TDate;

type
  TdtKind = (dtkDateOnly, dtkTimeOnly, dtkDateTime);

const
  { TDateTime value used to signify Null value}
  NullEquivalentDate: TDateTime = 0.0;

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
// Replacement for Win32Check to avoid platform specific warnings in D6
function OSCheck(RetVal: Boolean): Boolean;

{ Shortens a fully qualified Path name so that it can be drawn with a specified length limit.
  Same as FileCtrl.MinimizeName in functionality (but not implementation). Included here to
  not be forced to use FileCtrl unnecessarily }
function MinimizeFileName(const FileName: string; Canvas: TCanvas; MaxLen: Integer): string;
function MinimizeText(const Text: string; Canvas: TCanvas; MaxWidth: Integer): string;
{ MinimizeString trunactes long string, S, and appends
  '...' symbols, if Length of S is more than MaxLen }
function MinimizeString(const S: string; const MaxLen: Integer): string;

{$IFDEF MSWINDOWS}
{ RunDLL32 runs a function in a DLL using the utility rundll32.exe (on NT) or rundll.exe (on Win95/98)
 ModuleName is the name of the DLL to load, FuncName is the function to call and CmdLine is
 the command-line parameters (if any) to send to the function. Set WaitForCompletion to False to
 return immediately after the call.
 CmdShow should be one of the SW_SHOWXXXX constants and defaults SW_SHOWDEFAULT
 Return value:
 if WaitForCompletion is True, returns True if the wait didn't return WAIT_FAILED
 if WaitForCompletion is False, returns True if the process could be created
 To get information on why RunDLL32 might have failed, call GetLastError
 To get more info on what can actually be called using rundll32.exe, take a look at
 http://www.dx21.com/SCRIPTING/RUNDLL32/REFGUIDE.ASP?NTI=4&SI=6
}
type
  // the signature of procedures in DLL's that can be called using rundll32.exe
  TRunDLL32Proc = procedure(Handle: THandle; HInstance: HMODULE; CmdLine: PChar; CmdShow: Integer); stdcall;

function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: Boolean; CmdShow: Integer =
  SW_SHOWDEFAULT): Boolean;
{ RunDll32Internal does the same as RunDLL32 but does not use the RunDLL32.exe application to do it.
 Rather it loads the DLL, gets a pointer to the function in FuncName and calls it with the given parameters.
 Because of this behaviour, RunDll32Internal works slightly different from RunDLL32:
 * It doesn't return any value indicating success/failure
 * There is no WaitForCompletion parameter (but see comment below on how to circumvent this)
 * You must pass in a valid windows handle in Wnd. Note that if you pass 0, the call might fail, with no indication of why.
 * To simulate WaitForCompletion = False, pass the return value of GetDesktopWindow as the Wnd parameter,
 * To simulate WaitForCompletion = True, pass the handle of the calling window (f ex the form you are calling the procedure from)
 * If you try to call a function in a DLL that doesn't use the TRunDLL32Proc signature, your program
   might crash. Using the RunDLL32 function protects you from any problems with calling the wrong functions
   (a dialog is displayed if do something wrong)
 * RunDll32Internal is slightly faster but RunDLL32 is safer
}
procedure RunDll32Internal(Wnd: THandle; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
{ GetDLLVersion loads DLLName, gets a pointer to the DLLVersion function and calls it, returning the major and minor version values
from the function. Returns False if the DLL couldn't be loaded or if GetDLLVersion couldn't be found. }
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;
{$ENDIF MSWINDOWS}

procedure ResourceNotFound(ResID: PChar);
function EmptyRect: TRect;
function RectWidth(R: TRect): Integer;
function RectHeight(R: TRect): Integer;
function CompareRect(const R1, R2: TRect): Boolean;
procedure RectNormalize(var R: TRect);
function RectIsSquare(const R: TRect): Boolean;
function RectSquare(var ARect: TRect; AMaxSize: Integer = -1): Boolean;
//If AMaxSize = -1 ,then auto calc Square's max size

{$IFDEF MSWINDOWS}
procedure FreeUnusedOle;
function GetWindowsVersionString: string;
function LoadDLL(const LibName: string): THandle;
function RegisterServer(const ModuleName: string): Boolean;
function UnregisterServer(const ModuleName: string): Boolean;
{$ENDIF MSWINDOWS}

{ String routines }
function GetEnvVar(const VarName: string): string;
function AnsiUpperFirstChar(const S: string): string; // follow Delphi 2009's example with the "Ansi" prefix
function StringToPChar(var S: string): PChar;
function StrPAlloc(const S: string): PChar;
procedure SplitCommandLine(const CmdLine: string; var ExeName, Params: string);
function DropT(const S: string): string;

function WindowClassName(Wnd: THandle): string;

procedure SwitchToWindow(Wnd: THandle; Restore: Boolean);
procedure ActivateWindow(Wnd: THandle);
procedure ShowWinNoAnimate(Handle: THandle; CmdShow: Integer);
procedure KillMessage(Wnd: THandle; Msg: Cardinal);

{ SetWindowTop put window to top without recreating window }
procedure SetWindowTop(const Handle: THandle; const Top: Boolean);
procedure CenterWindow(Wnd: THandle);
function MakeVariant(const Values: array of Variant): Variant;

{ Convert dialog units to pixels and backwards }

{$IFDEF MSWINDOWS}
function DialogUnitsToPixelsX(DlgUnits: Word): Word;
function DialogUnitsToPixelsY(DlgUnits: Word): Word;
function PixelsToDialogUnitsX(PixUnits: Word): Word;
function PixelsToDialogUnitsY(PixUnits: Word): Word;
{$ENDIF MSWINDOWS}

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

{$IFDEF BCB}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): THandle;
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): THandle;
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF BCB}


{$IFDEF MSWINDOWS}
{ BrowseForFolderNative displays Browse For Folder dialog }
function BrowseForFolderNative(const Handle: THandle; const Title: string; var Folder: string): Boolean;
{$ENDIF MSWINDOWS}


procedure AntiAlias(Clip: TBitmap);
procedure AntiAliasRect(Clip: TBitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);

procedure CopyRectDIBits(ACanvas: TCanvas; const DestRect: TRect;
  ABitmap: TBitmap; const SourceRect: TRect);
function IsTrueType(const FontName: string): Boolean;


// Removes all non-numeric characters from AValue and returns
// the resulting string
function TextToValText(const AValue: string): string;


// VisualCLX compatibility functions
function DrawText(DC: HDC; const Text: TCaption; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawText(Canvas: TCanvas; const Text: string; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawText(Canvas: TCanvas; Text: PAnsiChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextEx(Canvas: TCanvas; lpchText: PChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
function DrawText(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;

function DrawTextW(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextW(Canvas: TCanvas; Text: PWideChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextExW(Canvas: TCanvas; lpchText: PWideChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
function DrawTextExW(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;

type
  RasterOp = (
    RasterOp_CopyROP,
    RasterOp_OrROP,
    RasterOp_XorROP,
    RasterOp_NotAndROP,
    RasterOp_EraseROP = 3,
    RasterOp_NotCopyROP,
    RasterOp_NotOrROP,
    RasterOp_NotXorROP,
    RasterOp_AndROP,
    RasterOp_NotEraseROP = 7,
    RasterOp_NotROP,
    RasterOp_ClearROP,
    RasterOp_SetROP,
    RasterOp_NopROP,
    RasterOp_AndNotROP,
    RasterOp_OrNotROP,
    RasterOp_NandROP,
    RasterOp_NorROP,
    RasterOp_LastROP = 15);

function BitBlt(DestCanvas: TCanvas; X, Y, Width, Height: Integer; SrcCanvas: TCanvas;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean = True): LongBool;overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; Rop: RasterOp; IgnoreMask: Boolean): LongBool; overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean): LongBool; overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; WinRop: Cardinal): LongBool; overload;



function IsEqualGUID(const IID1, IID2: TGUID): Boolean;
{$EXTERNALSYM IsEqualGUID}


// Containers
type
  TIntegerListChange = procedure(Sender: TObject; Item: Integer; Action: TListNotification) of object;

  TIntegerList = class(TList)
  private
    FOnChange: TIntegerListChange;
    FLoading: Boolean;

    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index: Integer; const Value: Integer);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    procedure DoChange(Item: Integer; Action: TListNotification);
  public
    // To be used with DefineProperties in client classes.
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
    property Loading: Boolean read FLoading;

    // Overloaded to accept/return Integer instead of Pointer.
    function Add(Value: Integer): Integer;
    function Extract(Item: Integer): Integer;
    function First: Integer;
    function IndexOf(Item: Integer): Integer;
    procedure Insert(Index: Integer; Item: Integer);
    function Last: Integer;
    function Remove(Item: Integer): Integer;
    property Items[Index: Integer]: Integer read GetItem write SetItem; default;

    property OnChange: TIntegerListChange read FOnChange write FOnChange;
  end;

type
  TCollectionSortProc = function(Item1, Item2: TCollectionItem): Integer;

procedure CollectionSort(Collection: Classes.TCollection; SortProc: TCollectionSortProc);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  RTLConsts, SysConst,
  {$IFDEF MSWINDOWS}
  ComObj, ShellAPI, MMSystem, Registry,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_CHARACTER}
  Character, // needed for JclStrings inlined functions
  {$ENDIF HAS_UNIT_CHARACTER}
  Consts,
  JclStrings, JclSysInfo, JclFileUtils,
  Math, JclSysUtils;

const
  Separators: TSysCharSet = [#00, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
  ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];
  {$IFDEF MSWINDOWS}
  RC_OpenCDDrive = 'set cdaudio door open wait';
  RC_CloseCDDrive = 'set cdaudio door closed wait';
  RC_ShellName = 'Shell_TrayWnd';
  RC_DefaultIcon = 'DefaultIcon';
  {$ENDIF MSWINDOWS}
  tkStrings: set of TTypeKind = [tkString, tkLString, {$IFDEF UNICODE} tkUString, {$ENDIF} tkWString];

resourcestring
  // (p3) duplicated from JvConsts since this unit should not rely on JVCL at all
  RsEPropertyNotExists = 'Property "%s" does not exist';
  RsEInvalidPropertyType = 'Property "%s" has invalid type';
  RsEPivotLessThanZero = 'JvJCLUtils.MakeYear4Digit: Pivot < 0';

{$IFNDEF COMPILER12_UP} // Delphi 2009 introduced it and fixed the NativeInt of Delphi 2007
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR; stdcall;
asm
  pop ebp
  jmp GetWindowLong
end;

function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR; stdcall;
asm
  pop ebp
  jmp SetWindowLong
end;
{$ENDIF ~COMPILER12_UP}

function SendRectMessage(Handle: THandle; Msg: Integer; wParam: WPARAM; var R: TRect): Integer;
begin
  Result := SendMessage(Handle, Msg, wParam, LPARAM(@R));
end;

function SendStructMessage(Handle: THandle; Msg: Integer; wParam: WPARAM; var Data): Integer;
begin
  Result := SendMessage(Handle, Msg, wParam, LPARAM(@Data));
end;


function ReadCharsFromStream(Stream: TStream; var Buf: array of AnsiChar; BufSize: Integer): Integer;
begin
  Result := Stream.Read(Buf[0], BufSize);
end;

function WriteStringToStream(Stream: TStream; const Buf: AnsiString; BufSize: Integer): Integer;
begin
  Result := Stream.Write(Buf[1], BufSize);
end;

{$IFNDEF COMPILER12_UP}
function UTF8ToString(const S: UTF8String): string;
begin
  Result := UTF8Decode(S);
end;
{$ENDIF ~COMPILER12_UP}

// DEPRECATED:
// StrToFloatUS uses US '.' as decimal separator and ',' as thousand separator
//   [the right way to do this would have been to set up a TFormatSettings record]
(*
function USToLocalFloatStr(const Text: string): string;
var
  I: Integer;
begin
  Result := Text;
  if (DecimalSeparator <> '.') or (ThousandSeparator <> ',') then
  begin
    for I := 0 to Length(Result) do
      case Result[I] of
        '.':
          Result[I] := DecimalSeparator;
        ',':
          Result[I] := ThousandSeparator;
      end;
  end;
end;*)


// DEPRECATED:
(*
function StrToFloatUS(const Text: string): Extended;
begin
  try
    Result := StrToFloat(USToLocalFloatStr(Text));
  except
    Result := StrToFloat(Text); // try it with local settings
  end;
end;
*)

// DEPRECATED:
(*
function StrToFloatUSDef(const Text: string; Default: Extended): Extended;
begin
  Result := JvSafeStrToFloatDef(USToLocalFloatStr(Text), Default);
end;
*)

function VarIsInt(Value: Variant): Boolean;
begin
  Result := VarType(Value) in [varByte,
    varShortInt, varWord, varLongWord, {varInt64,}
    varSmallint, varInteger];
end;

function PosIdx(const SubStr, S: string; Index: Integer = 0): Integer;
  // use best register allocation
  function Find(Index, EndPos: Integer; StartChar: Char; const S: string): Integer;
  begin
    for Result := Index to EndPos do
      if S[Result] = StartChar then
        Exit;
    Result := 0;
  end;

  // use best register allocation
  function FindNext(Index, EndPos: Integer; const S, SubStr: string): Integer;
  begin
    for Result := Index + 1 to EndPos do
      if S[Result] <> SubStr[Result - Index + 1] then
        Exit;
    Result := 0;
  end;

var
  StartChar: Char;
  LenSubStr, LenStr: Integer;
  EndPos: Cardinal;
begin
  if Index <= 0 then
    Index := 1;
  Result := 0;
  LenSubStr := Length(SubStr);
  LenStr := Length(S);
  if (LenSubStr = 0) or (S = '') or (LenSubStr > LenStr - (Index - 1)) then
    Exit;

  StartChar := SubStr[1];
  EndPos := LenStr - LenSubStr + 1;
  if LenSubStr = 1 then
    Result := Find(Index, EndPos, StartChar, S)
  else
  begin
    repeat
      Result := Find(Index, EndPos, StartChar, S);
      if Result = 0 then
        Break;
      Index := Result;
      Result := FindNext(Result, Index + LenSubStr - 1, S, SubStr);
      if Result = 0 then
      begin
        Result := Index;
        Exit;
      end
      else
        Inc(Index);
    until False;
  end;
end;

function PosIdxW(const SubStr, S: WideString; Index: Integer = 0): Integer;

  // use best register allocation
  function Find(Index, EndPos: Integer; StartChar: WideChar; const S: WideString): Integer;
  begin
    for Result := Index to EndPos do
      if S[Result] = StartChar then
        Exit;
    Result := 0;
  end;

  // use best register allocation
  function FindNext(Index, EndPos: Integer; const S, SubStr: WideString): Integer;
  begin
    for Result := Index + 1 to EndPos do
      if S[Result] <> SubStr[Result - Index + 1] then
        Exit;
    Result := 0;
  end;

var
  StartChar: WideChar;
  LenSubStr, LenStr: Integer;
  EndPos: Cardinal;
begin
  if Index <= 0 then
    Index := 1;
  Result := 0;
  LenSubStr := Length(SubStr);
  LenStr := Length(S);
  if (LenSubStr = 0) or (S = '') or (LenSubStr > LenStr - (Index - 1)) then
    Exit;

  StartChar := SubStr[1];
  EndPos := LenStr - LenSubStr + 1;
  if LenSubStr = 1 then
    Result := Find(Index, EndPos, StartChar, S)
  else
  begin
    repeat
      Result := Find(Index, EndPos, StartChar, S);
      if Result = 0 then
        Break;
      Index := Result;
      Result := FindNext(Result, Index + LenSubStr - 1, S, SubStr);
      if Result = 0 then
      begin
        Result := Index;
        Exit;
      end
      else
        Inc(Index);
    until False;
  end;
end;

function PosLastCharIdx(Ch: Char; const S: string; Index: Integer = 0): Integer;
begin
  if (Index = 0) or (Index > Length(S)) then
    Index := Length(S);
  for Result := Index downto 1 do
    if S[Result] = Ch then
      Exit;
  Result := 0;
end;

function GetLineByPos(const S: string; const Pos: Integer): Integer;
var
  I: Integer;
begin
  if Length(S) < Pos then
    Result := -1
  else
  begin
    I := 1;
    Result := 0;
    while I <= Pos do
    begin
      if S[I] = #13 then
        Inc(Result);
      Inc(I);
    end;
  end;
end;

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
var
  I, IB: Integer;
begin
  X := -1;
  Y := -1;
  IB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #10 then
      begin
        Inc(Y);
        IB := I + 1;
      end;
      Inc(I);
    end;
    X := Pos - IB;
  end;
end;

procedure GetXYByPosW(const S: WideString; const Pos: Integer; var X, Y: Integer);
var
  I, IB: Integer;
begin
  X := -1;
  Y := -1;
  IB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #10 then
      begin
        Inc(Y);
        IB := I + 1;
      end;
      Inc(I);
    end;
    X := Pos - IB;
  end;
end;

function GetWordOnPos(const S: string; const P: Integer): string;
var
  I, Beg: Integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  for I := P downto 1 do
    if CharInSet(S[I], Separators) then
      Break;
  Beg := I + 1;
  for I := P to Length(S) do
    if CharInSet(S[I], Separators) then
      Break;
  if I > Beg then
    Result := Copy(S, Beg, I - Beg)
  else
    Result := S[P];
end;

function GetWordOnPosW(const S: WideString; const P: Integer): WideString;
var
  I, Beg: Integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  for I := P downto 1 do
    if CharInSetW(S[I], Separators) then
      Break;
  Beg := I + 1;
  for I := P to Length(S) do
    if CharInSetW(S[I], Separators) then
      Break;
  if I > Beg then
    Result := Copy(S, Beg, I - Beg)
  else
    Result := S[P];
end;

function GetWordOnPos2(const S: string; P: Integer; var iBeg, iEnd: Integer): string;
begin
  Result := '';
  if P < 1 then
    Exit;
  if CharInSet(S[P], Separators) and ((P < 1) or CharInSet(S[P - 1], Separators)) then
    Inc(P);
  iBeg := P;
  while iBeg >= 1 do
    if CharInSet(S[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if CharInSet(S[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg)
  else
    Result := S[P];
end;

function GetWordOnPos2W(const S: WideString; P: Integer; var iBeg, iEnd: Integer): WideString;
begin
  Result := '';
  if P < 1 then
    Exit;
  if CharInSetW(S[P], Separators) and
    ((P < 1) or (CharInSetW(S[P - 1], Separators))) then
    Inc(P);
  iBeg := P;
  while iBeg >= 1 do
    if CharInSetW(S[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if CharInSetW(S[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg)
  else
    Result := S[P];
end;

function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  iBeg := P;
  if P > 1 then
    if CharInSet(S[P], Separators) then
      if (P < 1) or ((P - 1 > 0) and CharInSet(S[P - 1], Separators)) then
        Inc(iBeg)
      else
      if not ((P - 1 > 0) and CharInSet(S[P - 1], Separators)) then
        Dec(iBeg);
  while iBeg >= 1 do
    if CharInSet(S[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if CharInSet(S[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg)
  else
    Result := S[P];
end;

function GetWordOnPosExW(const S: WideString; const P: Integer; var iBeg, iEnd: Integer): WideString;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  iBeg := P;
  if P > 1 then
    if CharInSetW(S[P], Separators) then
      if (P < 1) or ((P - 1 > 0) and CharInSetW(S[P - 1], Separators)) then
        Inc(iBeg)
      else
      if not ((P - 1 > 0) and CharInSetW(S[P - 1], Separators)) then
        Dec(iBeg);
  while iBeg >= 1 do
    if CharInSetW(S[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if CharInSetW(S[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg)
  else
    Result := S[P];
end;

function GetNextWordPosEx(const Text: string; StartIndex: Integer;
  var iBeg, iEnd: Integer): string;
var
  Len: Integer;
begin
  Len := Length(Text);
  Result := '';
  if (StartIndex < 1) or (StartIndex > Len) then
    Exit;
  if CharInSet(Text[StartIndex], Separators) and
     ((StartIndex < 1) or CharInSet(Text[StartIndex - 1], Separators)) then
    Inc(StartIndex);
  iBeg := StartIndex;
  while iBeg >= 1 do
    if CharInSet(Text[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := StartIndex;
  while iEnd <= Len do
    if CharInSet(Text[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  Dec(iEnd);
  if iEnd >= iBeg then
    Result := Copy(Text, iBeg, iEnd - iBeg)
  else
    Result := Text[StartIndex];

  // go right
  iEnd := iBeg;
  while (iEnd <= Len) and not CharInSet(Text[iEnd], Separators) do
    Inc(iEnd);
  if iEnd > Len then
    iEnd := Len
  else
    Dec(iEnd);
  Result := Copy(Text, iBeg, iEnd - iBeg + 1);
end;

function GetNextWordPosExW(const Text: WideString; StartIndex: Integer;
  var iBeg, iEnd: Integer): WideString;
var
  Len: Integer;
begin
  Len := Length(Text);
  Result := '';
  if (StartIndex < 1) or (StartIndex > Len) then
    Exit;
  if CharInSetW(Text[StartIndex], Separators) and
    ((StartIndex < 1) or CharInSetW(Text[StartIndex - 1], Separators)) then
    Inc(StartIndex);
  iBeg := StartIndex;
  while iBeg >= 1 do
    if CharInSetW(Text[iBeg], Separators) then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := StartIndex;
  while iEnd <= Len do
    if CharInSetW(Text[iEnd], Separators) then
      Break
    else
      Inc(iEnd);
  Dec(iEnd);
  if iEnd >= iBeg then
    Result := Copy(Text, iBeg, iEnd - iBeg)
  else
    Result := Text[StartIndex];

  // go right
  iEnd := iBeg;
  while (iEnd <= Len) and not CharInSetW(Text[iEnd], Separators) do
    Inc(iEnd);
  if iEnd > Len then
    iEnd := Len
  else
    Dec(iEnd);
  Result := Copy(Text, iBeg, iEnd - iBeg + 1);
end;

procedure GetEndPosCaret(const Text: string; CaretX, CaretY: Integer;
  var X, Y: Integer);
begin
  GetXYByPos(Text, Length(Text), X, Y);
  if Y = 0 then
    Inc(X, CaretX)
  else
    Inc(X);
  Dec(X);
  Inc(Y, CaretY);
end;

procedure GetEndPosCaretW(const Text: WideString; CaretX, CaretY: Integer;
  var X, Y: Integer);
begin
  GetXYByPosW(Text, Length(Text), X, Y);
  if Y = 0 then
    Inc(X, CaretX)
  else
    Inc(X);
  Dec(X);
  Inc(Y, CaretY);
end;

function SubStrBySeparator(const S: string; const Index: Integer; const Separator: string; StartIndex: Integer): string;
{ Returns a substring. Substrings are divided by a separator character }
var
  I, LenS, LenSeparator: Integer;
begin
  Result := '';
  LenSeparator := Length(Separator);
  LenS := Length(S);

  if StartIndex <= 0 then
    StartIndex := 1;
  if (LenS = 0) or (LenSeparator = 0) or (StartIndex > LenS) or
     ((Index < 0) or ((Index = 0) and (LenS > 0) and (S[StartIndex] = Separator[1]))) then
    Exit;

  for I := 1 to Index do
  begin
    StartIndex := PosIdx(Separator, S, StartIndex);
    if StartIndex = 0 then
      Exit;
    Inc(StartIndex, LenSeparator);
    if StartIndex > LenS then
      Exit;
  end;
  I := PosIdx(Separator, S, StartIndex);
  if I = 0 then
    I := LenS + 1;
  Result := Copy(S, StartIndex, I - StartIndex);
  //if CompareText(Result, Separator) = 0 then
  //  Result := '';
end;

function SubStrBySeparatorW(const S: WideString; const Index: Integer; const Separator: WideString; StartIndex: Integer): WideString;
{ Returns a substring. Substrings are divided by a separator character }
var
  I, LenS, LenSeparator: Integer;
begin
  Result := '';
  LenSeparator := Length(Separator);
  LenS := Length(S);

  if StartIndex <= 0 then
    StartIndex := 1;
  if (LenS = 0) or (LenSeparator = 0) or (StartIndex > LenS) or
     ((Index < 0) or ((Index = 0) and (LenS > 0) and (S[StartIndex] = Separator[1]))) then
    Exit;

  for I := 1 to Index do
  begin
    StartIndex := PosIdx(Separator, S, StartIndex);
    if StartIndex = 0 then
      Exit;
    Inc(StartIndex, LenSeparator);
    if StartIndex > LenS then
      Exit;
  end;
  I := PosIdx(Separator, S, StartIndex);
  if I = 0 then
    I := LenS + 1;
  Result := Copy(S, StartIndex, I - StartIndex);
  //if WideCompareText(Result, Separator) = 0 then
  //  Result := '';
end;

function SubWord(P: PChar; var P2: PChar): string;
var
  I: Integer;
begin
  I := 0;
  while not CharInSet(P[I], Separators) do
    Inc(I);
  SetString(Result, P, I);
  P2 := P + I;
end;

function ReplaceString(S: string; const OldPattern, NewPattern: string; StartIndex: Integer): string;
var
  I, LenOldPattern: Integer;
begin
  if OldPattern <> '' then
  begin
    if StartIndex <= 0 then
      StartIndex := 1;
    LenOldPattern := Length(OldPattern);
    I := PosIdx(OldPattern, S, StartIndex);
    while I > 0 do
    begin
      StartIndex := I + LenOldPattern;
      S := Copy(S, 1, I - 1) + NewPattern + Copy(S, StartIndex, MaxInt);
      I := PosIdx(OldPattern, S, StartIndex);
    end;
  end;
  Result := S;
end;

function ReplaceStringW(S: WideString; const OldPattern, NewPattern: WideString; StartIndex: Integer): WideString;
var
  I, LenOldPattern: Integer;
begin
  if OldPattern <> '' then
  begin
    if StartIndex <= 0 then
      StartIndex := 1;
    LenOldPattern := Length(OldPattern);
    I := PosIdxW(OldPattern, S, StartIndex);
    while I > 0 do
    begin
      StartIndex := I + LenOldPattern;
      S := Copy(S, 1, I - 1) + NewPattern + Copy(S, StartIndex, MaxInt);
      I := PosIdxW(OldPattern, S, StartIndex);
    end;
  end;
  Result := S;
end;

function ConcatSep(const S1, S2, Separator: string): string;
begin
  Result := S1;
  if Result <> '' then
    Result := Result + Separator;
  Result := Result + S2;
end;

function ConcatLeftSep(const S1, S2, Separator: string): string;
begin
  Result := S1;
  if Result <> '' then
    Result := Separator + Result;
  Result := S2 + Result;
end;

function MinimizeString(const S: string; const MaxLen: Integer): string;
begin
  if Length(S) > MaxLen then
    if MaxLen < 3 then
      Result := Copy(S, 1, MaxLen)
    else
      Result := Copy(S, 1, MaxLen - 3) + '...'
  else
    Result := S;
end;

function TrueInflateRect(const R: TRect; const I: Integer): TRect;
begin
  SetRect(Result, R.Left - I, R.Top - I, R.Right + I, R.Bottom + I);
end;

function FileGetInfo(FileName: TFileName; var SearchRec: TSearchRec): Boolean;
var
  DosError: Integer;
  Path: TFileName;
begin
  Result := False;
  Path := ExtractFilePath(ExpandFileName(FileName)) + AllFilesMask;
  {$IFDEF MSWINDOWS}
  FileName := AnsiUpperCase(ExtractFileName(FileName));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileName := ExtractFileName(FileName);
  {$ENDIF UNIX}
  DosError := FindFirst(Path, faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    {$IFDEF MSWINDOWS}
    if SameFileName(SearchRec.FindData.cFileName, FileName) or
      SameFileName(SearchRec.FindData.cAlternateFileName, FileName) then
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    if AnsiSameStr(SearchRec.Name, FileName) then
    {$ENDIF UNIX}
    begin
      Result := True;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function HasSubFolder(APath: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  Result := False;
  APath := Concat(AddSlash(APath), AllFilesMask);
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Attr and faDirectory = faDirectory) and
      (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      Result := True;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function IsEmptyFolder(APath: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  Result := True;
  APath := Concat(AddSlash(APath), AllFilesMask);
  DosError := FindFirst(APath, faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      Result := False;
      Break;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

{$IFDEF MSWINDOWS}
function LZFileExpand(const FileSource, FileDest: string): Boolean;
type
  TLZCopy = function(Source, Dest: Integer): Longint; stdcall;
  TLZOpenFile = function(FileName: PChar; var ReOpenBuff: TOFStruct; Style: Word): Integer; stdcall;
  TLZClose = procedure(hFile: Integer); stdcall;
var
  Source, Dest: Integer;
  OSSource, OSDest: TOFStruct;
  Res: Integer;
  Inst: THandle;
  LZCopy: TLZCopy;
  LZOpenFile: TLZOpenFile;
  LZClose: TLZClose;
begin
  Result := False;
  Inst := SafeLoadLibrary('LZ32.dll');
  try
    if Inst = 0 then
      RaiseLastOSError;
    LZCopy := GetProcAddress(Inst, 'LZCopy');
    {$IFDEF SUPPORTS_UNICODE}
    LZOpenFile := GetProcAddress(Inst, 'LZOpenFileW');
    {$ELSE}
    LZOpenFile := GetProcAddress(Inst, 'LZOpenFileA');
    {$ENDIF SUPPORTS_UNICODE}
    LZClose := GetProcAddress(Inst, 'LZClose');
    if not Assigned(LZCopy) or not Assigned(LZOpenFile) or not Assigned(LZClose) then
    begin
      SetLastError(ERROR_NOT_SUPPORTED);
      RaiseLastOSError;
    end;
    OSSource.cBytes := SizeOf(TOFStruct);
    OSDest.cBytes := SizeOf(TOFStruct);
    Source := LZOpenFile(
      PChar(FileSource), // address of name of file to be opened
      OSSource, // address of open file structure
      OF_READ or OF_SHARE_DENY_NONE); // action to take
    if Source < 0 then
    begin
      DeleteFile(FileDest);
      Dest := LZOpenFile(
        PChar(FileDest), // address of name of file to be opened
        OSDest, // address of open file structure
        OF_CREATE or OF_WRITE or OF_SHARE_EXCLUSIVE); // action to take
      if Dest >= 0 then
      begin
        Res := LZCopy(Source, Dest);
        if Res >= 0 then
          Result := True;
      end;
      LZClose(Source);
      LZClose(Dest);
    end;
  finally
    FreeLibrary(Inst);
  end;
end;
{$ENDIF MSWINDOWS}

procedure Dos2Win(var S: AnsiString);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$80..#$AF:
        S[I] := AnsiChar(Byte(S[I]) + (192 - $80));
      #$E0..#$EF:
        S[I] := AnsiChar(Byte(S[I]) + (240 - $E0));
    end;
end;

procedure Win2Dos(var S: AnsiString);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$C0..#$EF:
        S[I] := AnsiChar(Byte(S[I]) - (192 - $80));
      #$F0..#$FF:
        S[I] := AnsiChar(Byte(S[I]) - (240 - $E0));
    end;
end;

function Dos2WinRes(const S: AnsiString): AnsiString;
begin
  Result := S;
  Dos2Win(Result);
end;

function Win2DosRes(const S: AnsiString): AnsiString;
begin
  Result := S;
  Win2Dos(Result);
end;

function Win2Koi(const S: AnsiString): AnsiString;
const
  W: AnsiString = 'абвгдеёжзийклмнопрс=уфхчцшщьыъэЭя+--+-+ЁЖЗ++--Э-+ПРСТУФiЧЦШ+_Э+ЭЮо';
  K: AnsiString = '--ЧЗ-+ЈЦ++--Э-+ПРТУФiЖ+Ю+ЭЭШ+о_+СбвчздеЭцъйклмнопр=уфхжиЭгыэшщяьас';
var
  I, J: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
  begin
    J := Pos(Result[I], W);
    if J > 0 then
      Result[I] := K[J];
  end;
end;

procedure FillString(var Buffer: string; Count: Integer; const Value: Char);
begin
  {$IFDEF COMPILER12_UP}
  Buffer := StringOfChar(Value, Count);
  {$ELSE}
  FillChar(Buffer[1], Count * SizeOf(Char), Value);
  {$ENDIF COMPILER12_UP}
end;

procedure FillString(var Buffer: string; StartIndex, Count: Integer; const Value: Char);
begin
  if StartIndex <= 0 then
    StartIndex := 1;
  {$IFDEF COMPILER12_UP}
  Buffer := Copy(Buffer, 1, StartIndex - 1) + StringOfChar(Value, Count);
  {$ELSE}
  FillChar(Buffer[StartIndex], Count * SizeOf(Char), Value);
  {$ENDIF COMPILER12_UP}
end;

procedure MoveString(const Source: string; var Dest: string; Count: Integer);
begin
  Move(Source[1], Dest[1], Count * SizeOf(Char));
end;

procedure MoveString(const Source: string; SrcStartIdx: Integer; var Dest: string;
  DstStartIdx: Integer; Count: Integer);
begin
  if DstStartIdx <= 0 then
    DstStartIdx := 1;
  if SrcStartIdx <= 0 then
    SrcStartIdx := 1;

  Move(Source[SrcStartIdx], Dest[DstStartIdx], Count * SizeOf(Char));
end;

procedure FillWideChar(var Buffer; Count: Integer; const Value: WideChar);
var
  P: PLongint;
  Value2: Cardinal;
  CopyWord: Boolean;
begin
  Value2 := (Cardinal(Value) shl 16) or Cardinal(Value);
  CopyWord := Count and $1 <> 0;
  Count := Count div 2;
  P := @Buffer;
  while Count > 0 do
  begin
    P^ := Value2;
    Inc(P);
    Dec(Count);
  end;
  if CopyWord then
    PWideChar(P)^ := Value;
end;

procedure MoveWideChar(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count * SizeOf(WideChar));
end;

procedure FillNativeChar(var Buffer; Count: Integer; const Value: Char);
begin
  {$IFDEF COMPILER12_UP}
  FillWideChar(Buffer, Count, Value);
  {$ELSE}
  FillChar(Buffer, Count, Value);
  {$ENDIF COMPILER12_UP}
end;

procedure MoveNativeChar(const Source; var Dest; Count: Integer);
begin
  {$IFDEF COMPILER12_UP}
  MoveWideChar(Source, Dest, Count);
  {$ELSE}
  Move(Source, Dest, Count);
  {$ENDIF COMPILER12_UP}
end;

function IsSubString(const S: string; StartIndex: Integer; const SubStr: string): Boolean;
begin
  if StartIndex < 1 then
    StartIndex := 1;
  if StartIndex > Length(S) then
    StartIndex := Length(S);
  Result := StrLComp(PChar(S) + StartIndex - 1, PChar(SubStr), Length(SubStr)) = 0;
end;

function Spaces(const N: Integer): string;
begin
  if N > 0 then
  begin
    SetLength(Result, N);
    FillString(Result, N, ' ');
  end
  else
    Result := '';
end;

function AddSpaces(const S: string; const N: Integer): string;
var
  Len: Integer;
begin
  Len := Length(S);
  if (Len < N) and (N > 0) then
  begin
    SetLength(Result, N);
    MoveString(S, Result, Len);
    FillString(Result, Len + 1, N - Len, ' ');
  end
  else
    Result := S;
end;

function SpacesW(const N: Integer): WideString;
begin
  if N > 0 then
  begin
    SetLength(Result, N);
    FillWideChar(Result[1], N, ' ');
  end
  else
    Result := '';
end;

function AddSpacesW(const S: WideString; const N: Integer): WideString;
var
  Len: Integer;
begin
  Len := Length(S);
  if (Len < N) and (N > 0) then
  begin
    SetLength(Result, N);
    MoveWideChar(S[1], Result[1], Len);
    FillWideChar(Result[Len + 1], N - Len, ' ');
  end
  else
    Result := S;
end;

{ (rb) maybe construct an english variant? }

function LastDateRUS(const Dat: TDateTime): string;
const
  D2D: array [0..9] of Byte =
    (3, 1, 2, 2, 2, 3, 3, 3, 3, 3);
  Day: array [1..3] of string =
    ('день', 'дня', 'дней'); // Day, Days, Days
  Month: array [1..3] of string =
    ('месяц', 'месяца', 'месяцев'); // Month, Months, Months
  Year: array [1..3] of string =
    ('год', 'года', 'ле='); // Year, Years, Years
  Week: array [1..4] of string =
    ('неделЭ', '2 недели', '3 недели', 'месяц'); // Week, 2 Weeks, 3 Weeks, Month
var
  Y, M, D: Integer;
begin
  if Date = Dat then
    Result := 'сегодня' // Today
  else
  if Dat = Date - 1 then
    Result := 'вчера' // Yesterday
  else
  if Dat = Date - 2 then
    Result := 'позавчера' // Day before yesterday
  else
  if Dat > Date then
    Result := 'в будущем' // In the future
  else
  begin
    D := Trunc(Date - Dat);
    Y := Round(D / 365);
    M := Round(D / 30);
    if Y > 0 then
      Result := IntToStr(Y) + ' ' + Year[D2D[StrToInt(IntToStr(Y)[Length(IntToStr(Y))])]] + ' назад' // ago
    else
    if M > 0 then
      Result := IntToStr(M) + ' ' + Month[D2D[StrToInt(IntToStr(M)[Length(IntToStr(M))])]] + ' назад' // ago
    else
    if D > 6 then
      Result := Week[D div 7] + ' назад' // ago
    else
    if D > 0 then
      Result := IntToStr(D) + ' ' + Day[D2D[StrToInt(IntToStr(D)[Length(IntToStr(D))])]] + ' назад' // ago
  end;
end;

function AddSlash(const Dir: TFileName): string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> PathDelim) then
    Result := Dir + PathDelim;
end;

function AddPath(const FileName, Path: TFileName): TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash(Path) + FileName
  else
    Result := FileName;
end;

function AddPaths(const PathList, Path: string): string;
var
  I: Integer;
  S: string;
begin
  Result := '';
  I := 0;
  S := SubStrBySeparator(PathList, I, PathSep);
  while S <> '' do
  begin
    Result := ConcatSep(Result, AddPath(S, Path), PathSep);
    Inc(I);
    S := SubStrBySeparator(PathList, I, PathSep);
  end;
end;

function ParentPath(const Path: TFileName): TFileName;
begin
  Result := Path;
  if (Length(Result) > 0) and (Result[Length(Result)] = PathDelim) then
    Delete(Result, Length(Result), 1);
  Result := ExtractFilePath(Result);
end;

function FindInPath(const FileName, PathList: string): TFileName;
var
  I: Integer;
  S: string;
begin
  I := 0;
  S := SubStrBySeparator(PathList, I, PathSep);
  while S <> '' do
  begin
    Result := AddSlash(S) + FileName;
    if FileExists(Result) then
      Exit;
    Inc(I);
    S := SubStrBySeparator(PathList, I, PathSep);
  end;
  Result := '';
end;

{$IFDEF MSWINDOWS}
function GetComputerID: string;
var
  SN: DWORD;
  Nul: DWORD;
  WinDir: array [0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(WinDir, MAX_PATH);
  WinDir[3] := #0;
  if GetVolumeInformation(
    WinDir, // address of root directory of the file system
    nil, // address of name of the volume
    0, // Length of lpVolumeNameBuffer
    @SN, // address of volume serial number
    Nul, // address of system's maximum filename Length
    Nul, // address of file system flags
    nil, // address of name of file system
    0) {// Length of lpFileSystemNameBuffer} then
    Result := IntToHex(SN, 8)
  else
    Result := 'None';
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
function GetComputerID: string;
begin
  Result := 'None';
end;
{$ENDIF UNIX}

function GetComputerName: string;
var
  nSize: Cardinal;
begin
  nSize := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, nSize);
  if Windows.GetComputerName(PChar(Result), nSize) then
    SetLength(Result, nSize)
  else
    Result := '';
end;

function CurrencyToStr(const Cur: Currency): string;
begin
  Result := CurrToStrF(Cur, ffCurrency, JclFormatSettings.CurrencyDecimals)
end;

function HasChar(const Ch: Char; const S: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(S) do
    if S[I] = Ch then
      Exit;
  Result := False;
end;

function HasCharW(const Ch: WideChar; const S: WideString): Boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function HasAnyChar(const Chars: string; const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(Chars) do
    if HasChar(Chars[I], S) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function CountOfChar(const Ch: Char; const S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = Ch then
      Inc(Result);
end;

procedure SwapInt(var Int1, Int2: Integer);
var
  Tmp: Integer;
begin
  Tmp := Int1;
  Int1 := Int2;
  Int2 := Tmp;
end;

function DeleteReadOnlyFile(const FileName: TFileName): Boolean;
begin
  {$IFDEF MSWINDOWS}
  FileSetAttr(FileName, 0); {clear Read Only Flag}
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileSetReadOnly(FileName, False);
  {$ENDIF UNIX}
  Result := DeleteFile(FileName);
end;

function HasParam(const Param: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
  begin
    Result := SameText(ParamStr(I), Param);
    if Result then
      Exit;
  end;
end;

function HasSwitch(const Param: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamCount do
    if HasChar(ParamStr(I)[1], '-/') then
    begin
      Result := SameText(Copy(ParamStr(I), 2, Length(Param)), Param);
      if Result then
        Exit;
    end;
end;

function Switch(const Param: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount do
    if HasChar(ParamStr(I)[1], '-/\') and
      SameText(Copy(ParamStr(I), 2, Length(Param)), Param) then
    begin
      Result := Copy(ParamStr(I), 2 + Length(Param), 260);
      Exit;
    end;
end;

function ExePath: TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function FileNewExt(const FileName, NewExt: TFileName): TFileName;
begin
  Result := Copy(FileName, 1, Length(FileName) - Length(ExtractFileExt(FileName))) + NewExt;
end;

{$IFNDEF COMPILER12_UP}
function ToUpper(C: Char): Char;
var s : string;
begin
  s := UpperCase(c);
  Result := s[1];
end;
{$ENDIF ~COMPILER12_UP}

{$IFNDEF COMPILER12_UP}
function CharInSet(const Ch: AnsiChar; const SetOfChar: TSysCharSet): Boolean;
begin
  Result := Ch in SetOfChar;
end;
{$ENDIF ~COMPILER12_UP}

function CharInSetW(const Ch: WideChar; const SetOfChar: TSysCharSet): Boolean;
begin
  if Word(Ch) > 255 then
    Result := False
  else
    Result := AnsiChar(Ch) in SetOfChar;
end;

function IntPower(Base, Exponent: Integer): Integer;
begin
  if Exponent > 0 then
  begin
    Result := Base;
    Dec(Exponent);
    while Exponent > 0 do
    begin
      Result := Result * Base;
      Dec(Exponent);
    end;
  end
  else
  if Exponent < 0 then
    Result := 0
  else
    Result := 1;
end;

function KeyPressed(VK: Integer): Boolean;
begin
  Result := Windows.GetKeyState(VK) and $8000 = $8000;
end;


function Var2Type(V: Variant; const DestVarType: Integer): Variant;
var
  VType: TVarType;
begin
  VType := TVarData(V).VType;
  if VType in [varEmpty, varNull] then
  begin
    case DestVarType of
      varOleStr,
      varString:
        Result := '';
      varInteger, varSmallint, varByte:
        Result := 0;
      varBoolean:
        Result := False;
      varSingle, varDouble, varCurrency, varDate:
        Result := 0.0;
      varVariant:
        Result := Null;
    else
      Result := VarAsType(V, DestVarType);
    end;
  end
  else
    Result := VarAsType(V, DestVarType);
  if (DestVarType = varInteger) and (VType = varBoolean) then
    Result := Integer(V = True);
end;

function VarToInt(V: Variant): Integer;
begin
  Result := Var2Type(V, varInteger);
end;

function VarToFloat(V: Variant): Double;
begin
  Result := Var2Type(V, varDouble);
end;

function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path, DestPath: TFileName;
begin
  Result := False;
  if not CreateDir(DestDir) then
    Exit;
  Path := SourceDir;
  DestPath := AddSlash(DestDir);
  Path := AddSlash(Path);
  DosError := FindFirst(Path + AllFilesMask, faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Result := CopyDir(Path + SearchRec.Name, AddSlash(DestDir) + SearchRec.Name)
      else
        Result := CopyFile(PChar(Path + SearchRec.Name), PChar(DestPath + SearchRec.Name), True);
      if not Result then
        Exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := True;
end;

//////////////////////////////////////////////////////////////////////////////
{ Note: FileTimeToDateTime has been commented out, it is not used anywhere
        in the JVCL code. Further, the old version is not to be returned
        as it does not behave like the JCL version it is supposed to mimick.
        See Mantis 2452 for details.
}
{const
  FileTimeBase      = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day
function FileTimeToDateTime(const FT: TFileTime): TDateTime;
begin
  Result := Int64(FileTime) / FileTimeStep;
  Result := Result + FileTimeBase;
end;}
// ---------------------------- old version ---------------------------
//{$IFDEF MSWINDOWS}
{var
  LocalFileTime: TFileTime;
  FileDate: Integer;
begin
  FileTimeToLocalFileTime(FT, LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime, LongRec(FileDate).Hi, LongRec(FileDate).Lo);
  Result := FileDateToDateTime(FileDate);
end;}
//{$ENDIF MSWINDOWS}
//{$IFDEF UNIX}
{begin
  Result := FileDateToDateTime(FT);
end;}
//{$ENDIF UNIX}
// ------------------------- old version --------------------------------

procedure FileTimeToDosDateTimeDWord(const FT: TFileTime; out Dft: DWORD);
begin
  FileTimeToDosDateTime(FT, LongRec(Dft).Hi, LongRec(Dft).Lo);
end;

function MakeValidFileName(const FileName: TFileName;
  ReplaceBadChar: Char): TFileName;
var
  I: Integer;
begin
  Result := FileName;
  for I := 1 to Length(Result) do
    if HasChar(Result[I], '''":?*\/') then
      Result[I] := ReplaceBadChar;
end;

function DefStr(const S: string; Default: string): string;
begin
  if S <> '' then
    Result := S
  else
    Result := Default;
end;

function StrLICompW2(S1, S2: PWideChar; MaxLen: Integer): Integer;
// faster than the JclUnicode.StrLICompW function
var
  P1, P2: WideString;
begin
  SetString(P1, S1, Min(MaxLen, StrLenW(S1)));
  SetString(P2, S2, Min(MaxLen, StrLenW(S2)));
  Result := SysUtils.WideCompareText(P1, P2);
end;

function StrPosW(S, SubStr: PWideChar): PWideChar;
var
  P: PWideChar;
  I: Integer;
begin
  Result := nil;
  if (S = nil) or (SubStr = nil) or
    (S[0] = #0) or (SubStr[0] = #0) then
    Exit;
  Result := S;
  while Result[0] <> #0 do
  begin
    if Result[0] <> SubStr[0] then
      Inc(Result)
    else
    begin
      P := Result + 1;
      I := 0;
      while (P[0] <> #0) and (P[0] = SubStr[I]) do
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

function StrLenW(S: PWideChar): Integer;
begin
  Result := 0;
  if S <> nil then
    while S[Result] <> #0 do
      Inc(Result);
end;

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

procedure SetDelimitedText(List: TStrings; const Text: string; Delimiter: Char);
var
  Ch: Char;
begin
  Ch := List.Delimiter;
  try
    List.Delimiter := Delimiter;
    List.DelimitedText := Text;
  finally
    List.Delimiter := Ch;
  end;
end;

function StrToBool(const S: string): Boolean;
begin
  Result := (S = '1') or SameText(S, 'True') or SameText(S, 'yes');
end;

function RATextOutEx(Canvas: TCanvas; const R, RClip: TRect; const S: string;
  const CalcHeight: Boolean): Integer;
var
  Ss: TStrings;
  I: Integer;
  H: Integer;
begin
  Ss := TStringList.Create;
  try
    Ss.Text := S;
    H := Canvas.TextHeight('A');
    Result := H * Ss.Count;
    if not CalcHeight then
      for I := 0 to Ss.Count - 1 do
        ExtTextOut(
          Canvas.Handle, // handle of device context
          R.Left, // X-coordinate of reference point
          R.Top + H * I, // Y-coordinate of reference point
          ETO_CLIPPED, // text-output options
          @RClip, // optional clipping and/or opaquing rectangle
          PChar(Ss[I]),
          Length(Ss[I]), // number of characters in string
          nil); // address of array of intercharacter spacing values
  finally
    Ss.Free;
  end;
end;

procedure RATextOut(Canvas: TCanvas; const R, RClip: TRect; const S: string);
begin
  RATextOutEx(Canvas, R, RClip, S, False);
end;

function RATextCalcHeight(Canvas: TCanvas; const R: TRect; const S: string): Integer;
begin
  Result := RATextOutEx(Canvas, R, R, S, True);
end;

procedure Cinema(Canvas: TCanvas; rS, rD: TRect);
const
  Pause = 30; {milliseconds}
  Steps = 7;
  Width = 1;
var
  R: TRect;
  I: Integer;
  PenOld: TPen;

  procedure FrameR(R: TRect);
  begin
    with Canvas do
    begin
      MoveTo(R.Left, R.Top);
      LineTo(R.Left, R.Bottom);
      LineTo(R.Right, R.Bottom);
      LineTo(R.Right, R.Top);
      LineTo(R.Left, R.Top);
    end;
  end;

  procedure Frame;
  begin
    FrameR(R);
    with Canvas do
    begin
      MoveTo(rS.Left, rS.Top);
      LineTo(R.Left, R.Top);
      if R.Top <> rS.Top then
      begin
        MoveTo(rS.Right, rS.Top);
        LineTo(R.Right, R.Top);
      end;
      if R.Left <> rS.Left then
      begin
        MoveTo(rS.Left, rS.Bottom);
        LineTo(R.Left, R.Bottom);
      end;
      if (R.Bottom <> rS.Bottom) and (R.Right <> rS.Right) then
      begin
        MoveTo(rS.Right, rS.Bottom);
        LineTo(R.Right, R.Bottom);
      end;
    end;
  end;

begin
  PenOld := TPen.Create;
  PenOld.Assign(Canvas.Pen);
  Canvas.Pen.Mode := pmNot;
  Canvas.Pen.Width := Width;
  Canvas.Pen.Style := psDot;
  FrameR(rS);
  R := rS;
  for I := 1 to Steps do
  begin
    R.Left := rS.Left + (rD.Left - rS.Left) div Steps * I;
    R.Top := rS.Top + (rD.Top - rS.Top) div Steps * I;
    R.Bottom := rS.Bottom + (rD.Bottom - rS.Bottom) div Steps * I;
    R.Right := rS.Right + (rD.Right - rS.Right) div Steps * I;
    Frame;
    Sleep(Pause);
    Frame;
  end;
  FrameR(rS);
  Canvas.Pen.Assign(PenOld);
end;

function IniReadSection(const IniFileName: TFileName; const Section: string; Ss: TStrings): Boolean;
var
  F: Integer;
  S: string;
begin
  with TStringList.Create do
  try
    LoadFromFile(IniFileName);
    F := IndexOf('[' + Section + ']');
    Result := F > -1;
    if Result then
    begin
      Ss.BeginUpdate;
      try
        Ss.Clear;
        Inc(F);
        while F < Count do
        begin
          S := Strings[F];
          if (Length(S) > 0) and (Trim(S[1]) = '[') then
            Break;
          Ss.Add(S);
          Inc(F);
        end;
      finally
        Ss.EndUpdate;
      end;
    end;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName: TFileName; const Source: string);
begin
  with TStringList.Create do
  try
    Text := Source;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

function LoadTextFile(const FileName: TFileName): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    Result := Text;
  finally
    Free;
  end;
end;

function ReadFolder(const Folder, Mask: TFileName; FileList: TStrings): Integer;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  FileList.BeginUpdate;
  try
    FileList.Clear;
    Result := FindFirst(AddSlash(Folder) + Mask, faAnyFile, SearchRec);
    DosError := Result;
    while DosError = 0 do
    begin
      if not ((SearchRec.Attr and faDirectory) = faDirectory) then
        FileList.Add(SearchRec.Name);
      DosError := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  finally
    FileList.EndUpdate;
  end;
end;

function ReadFolders(const Folder: TFileName; FolderList: TStrings): Integer;
var
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  FolderList.BeginUpdate;
  try
    FolderList.Clear;
    Result := FindFirst(AddSlash(Folder) + AllFilesMask, faAnyFile, SearchRec);
    DosError := Result;
    while DosError = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory) and
        (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        FolderList.Add(SearchRec.Name);
      DosError := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  finally
    FolderList.EndUpdate;
  end;
end;

{ example for ReplaceStrings:
    with memEdit do
    begin
      Text := ReplaceStrings(Text, SelStart+1, SelLength, memWords.Lines, memFrases.Lines, NewSelStart);
      SelStart := NewSelStart-1;
    end; }

function ReplaceStrings(const S: string; PosBeg, Len: Integer; Words, Frases: TStrings;
  var NewSelStart: Integer): string;
var
  I, Beg, Ent, LS, F: Integer;
  Word: string;
begin
  NewSelStart := PosBeg;
  Result := S;
  LS := Length(S);
  if Len = 0 then
  begin
    if PosBeg < 1 then
      Exit;
    if PosBeg = 1 then
      PosBeg := 2;
    for I := PosBeg - 1 downto 1 do
      if CharInSet(S[I], Separators) then
        Break;
    Beg := I + 1;
    for Ent := PosBeg to LS do
      if CharInSet(S[Ent], Separators) then
        Break;
    if Ent > Beg then
      Word := Copy(S, Beg, Ent - Beg)
    else
      Word := S[PosBeg];
  end
  else
  begin
    Word := Copy(S, PosBeg, Len);
    Beg := PosBeg;
    Ent := PosBeg + Len;
  end;
  if Word = '' then
    Exit;
  F := Words.IndexOf(Word);
  if (F > -1) and (F < Frases.Count) then
  begin
    Result := Copy(S, 1, Beg - 1) + Frases[F] + Copy(S, Ent, LS);
    NewSelStart := Beg + Length(Frases[F]);
  end;
end;

{  example for ReplaceAllStrings:

    with memEdit do
      Text := ReplaceAllStrings(Text, memWords.Lines, memFrases.Lines);
}

function ReplaceAllStrings(const S: string; Words, Frases: TStrings): string;
var
  I: Integer;
begin
  Result := S;
  for I := 0 to Words.Count - 1 do
    Result := ReplaceString(Result, Words[I], Frases[I]);
end;

function CountOfLines(const S: string): Integer;
begin
  with TStringList.Create do
  try
    Text := S;
    Result := Count;
  finally
    Free;
  end;
end;

procedure DeleteOfLines(Ss: TStrings; const Words: array of string);
var
  I, J: Integer;
begin
  Ss.BeginUpdate;
  try
    for J:= Low(Words) to High(Words) do
      for I := Ss.Count - 1 downto 0 do
        if Trim(Ss[I]) = Trim(Words[J]) then
          Ss.Delete(I);
  finally
    Ss.EndUpdate;
  end;
end;

procedure DeleteEmptyLines(Ss: TStrings);
begin
  DeleteOfLines(Ss,['']);
end;

procedure SQLAddWhere(SQL: TStrings; const Where: string);
var
  I, J: Integer;
begin
  J := SQL.Count - 1;
  for I := 0 to SQL.Count - 1 do
    // (rom) does this always work? Think of a fieldname "grouporder"
    if StrLIComp(PChar(SQL[I]), 'where ', 6) = 0 then
    begin
      J := I + 1;
      while J < SQL.Count do
      begin
        if (StrLIComp(PChar(SQL[J]), 'order ', 6) = 0) or
          (StrLIComp(PChar(SQL[J]), 'group ', 6) = 0) then
          Break;
        Inc(J);
      end;
    end;
  SQL.Insert(J, 'and ' + Where);
end;

procedure InternalFrame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    TopRight.X := Rect.Right;
    TopRight.Y := Rect.Top;
    BottomLeft.X := Rect.Left;
    BottomLeft.Y := Rect.Bottom;
    Canvas.Pen.Color := TopColor;
    Canvas.PolyLine([BottomLeft, Rect.TopLeft, TopRight]);
    Canvas.Pen.Color := BottomColor;
    Dec(BottomLeft.X);
    Canvas.PolyLine([TopRight, Rect.BottomRight, BottomLeft]);
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom);
  Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom);
  Inc(Rect.Right);
end;

procedure Roughed(ACanvas: TCanvas; const ARect: TRect; const AVert: Boolean);
var
  I: Integer;
  J: Integer;
  R: TRect;
  V: Boolean;
  H: Boolean;
begin
  H := True;
  V := True;
  for I := 0 to (ARect.Right - ARect.Left) div 4 do
  begin
    for J := 0 to (ARect.Bottom - ARect.Top) div 4 do
    begin
      if AVert then
      begin
        if V then
          R := Bounds(ARect.Left + I * 4 + 2, ARect.Top + J * 4, 2, 2)
        else
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4, 2, 2);
      end
      else
      begin
        if H then
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4 + 2, 2, 2)
        else
          R := Bounds(ARect.Left + I * 4, ARect.Top + J * 4, 2, 2);
      end;

      InternalFrame3D(ACanvas, R, clBtnHighlight, clBtnShadow, 1);
      V := not V;
    end;
    H := not H;
  end;
end;

function BitmapFromBitmap(SrcBitmap: TBitmap; const AWidth, AHeight, Index: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.Canvas.CopyRect(Rect(0, 0, AWidth, AHeight), SrcBitmap.Canvas, Bounds(AWidth * Index, 0, AWidth, AHeight));
end;

{$IFDEF MSWINDOWS}

function ResSaveToFileEx(Instance: HINST; Typ, Name: PChar;
  const Compressed: Boolean; const FileName: string): Boolean;
var
  RhRsrc: HRSRC;
  RhGlobal: HGLOBAL;
  RAddr: Pointer;
  RLen: DWORD;
  Stream: TFileStream;
  FileDest: string;
begin
  Result := False;
  RhRsrc := FindResource(
    Instance, // resource-module handle
    Name, // address of resource name
    Typ); // address of resource type
  if RhRsrc = 0 then
    Exit;
  RhGlobal := LoadResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RhGlobal = 0 then
    Exit;
  RAddr := LockResource(
    RhGlobal); // handle to resource to lock
  FreeResource(RhGlobal);
  if RAddr = nil then
    Exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RLen = 0 then
    Exit;
  { And now it is possible to duplicate [translated] }
  Stream := nil; { for Free [translated] }
  if Compressed then
    FileDest := GenTempFileName(FileName)
  else
    FileDest := FileName;
  try
    try
      Stream := TFileStream.Create(FileDest, fmCreate or fmOpenWrite or fmShareExclusive);
      Stream.WriteBuffer(RAddr^, RLen);
    finally
      Stream.Free;
    end;
    if Compressed then
    begin
      Result := LZFileExpand(FileDest, FileName);
      DeleteFile(FileDest);
    end
    else
      Result := True;
  except
  end;
end;

function ResSaveToFile(const Typ, Name: string; const Compressed: Boolean;
  const FileName: string): Boolean;
begin
  Result := ResSaveToFileEx(HInstance, PChar(Typ), PChar(Name), Compressed, FileName);
end;

function ResSaveToString(Instance: HINST; const Typ, Name: string;
  var S: string): Boolean;
var
  RhRsrc: HRSRC;
  RhGlobal: HGLOBAL;
  RAddr: Pointer;
  RLen: DWORD;
begin
  Result := False;
  RhRsrc := FindResource(
    Instance, // resource-module handle
    PChar(Name), // address of resource name
    PChar(Typ)); // address of resource type
  if RhRsrc = 0 then
    Exit;
  RhGlobal := LoadResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RhGlobal = 0 then
    Exit;
  RAddr := LockResource(RhGlobal); // handle to resource to lock
  FreeResource(RhGlobal);
  if RAddr = nil then
    Exit;
  RLen := SizeofResource(
    Instance, // resource-module handle
    RhRsrc); // resource handle
  if RLen = 0 then
    Exit;
  { And now it is possible to duplicate [translated] }
  SetString(S, PChar(RAddr), RLen);
end;

{$ENDIF MSWINDOWS}

function TextHeight(const AStr: string): Integer;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  DC := GetDC(HWND_DESKTOP);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    Result := Canvas.TextHeight(AStr);
    Canvas.Handle := NullHandle;
  finally
    ReleaseDC(HWND_DESKTOP, DC);
    Canvas.Free;
  end;
end;

function TextWidth(const AStr: string): Integer;
var
  Canvas: TCanvas;
  DC: HDC;
begin
  DC := GetDC(HWND_DESKTOP);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    Result := Canvas.TextWidth(AStr);
    Canvas.Handle := NullHandle;
  finally
    ReleaseDC(HWND_DESKTOP, DC);
    Canvas.Free;
  end;
end;

procedure SetChildPropOrd(Owner: TComponent; const PropName: string; Value: Longint);
var
  I: Integer;
  PropInfo: PPropInfo;
begin
  for I := 0 to Owner.ComponentCount - 1 do
  begin
    PropInfo := GetPropInfo(Owner.Components[I].ClassInfo, PropName);
    if PropInfo <> nil then
      SetOrdProp(Owner.Components[I], PropInfo, Value);
  end;
end;

procedure Error(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean; var PlainItem: string;
  var Width: Integer; CalcWidth: Boolean);
var
  CL: string;
  I: Integer;
  M1: string;
  OriRect: TRect; // it's added
  LastFontStyle: TFontStyles;
  LastFontColor: TColor;

  function Cmp(const M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(const M1: string): Boolean;
  begin
    Result := Cmp(M1);
    if Result then
      Inc(I, Length(M1));
  end;

  function CmpL(const M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(const M1: string): Boolean;
  begin
    Result := Cmp1(M1 + '>');
  end;

  procedure Draw(const M: string);
  begin
    if not Assigned(Canvas) then
      Exit;
    if not CalcWidth then
      Canvas.TextOut(Rect.Left, Rect.Top, M);
    Rect.Left := Rect.Left + Canvas.TextWidth(M);
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if not Assigned(Canvas) then
      Exit;
    if Include then
      Canvas.Font.Style := Canvas.Font.Style + [Style]
    else
      Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

begin
  PlainItem := '';
  LastFontColor := 0; { satisfy compiler }
  if Canvas <> nil then
  begin
    LastFontStyle := Canvas.Font.Style;
    LastFontColor := Canvas.Font.Color;
  end;
  try
    if HideSelColor and Assigned(Canvas) then
    begin
      Canvas.Brush.Color := clWindow;
      Canvas.Font.Color := clWindowText;
    end;
    if Assigned(Canvas) then
      Canvas.FillRect(Rect);

    Width := Rect.Left;
    Rect.Left := Rect.Left + 2;

    OriRect := Rect; //save origin rectangle

    M1 := '';
    I := 1;
    while I <= Length(Text) do
    begin
      if (Text[I] = '<') and
        (CmpL('b') or CmpL('/b') or
        CmpL('i') or CmpL('/i') or
        CmpL('u') or CmpL('/u') or
        Cmp('c:')) then
      begin
        Draw(M1);
        PlainItem := PlainItem + M1;

        if CmpL1('b') then
          Style(fsBold, True)
        else
        if CmpL1('/b') then
          Style(fsBold, False)
        else
        if CmpL1('i') then
          Style(fsItalic, True)
        else
        if CmpL1('/i') then
          Style(fsItalic, False)
        else
        if CmpL1('u') then
          Style(fsUnderline, True)
        else
        if CmpL1('/u') then
          Style(fsUnderline, False)
        else
        if Cmp1('c:') then
        begin
          CL := SubStrBySeparator(Text, 0, '>', I);
          if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
          try
            if (Length(CL) > 0) and (CL[1] <> '$') then
              Canvas.Font.Color := StringToColor('cl' + CL)
            else
              Canvas.Font.Color := StringToColor(CL);
          except
          end;
          Inc(I, Length(CL) + 1 {'>'});
        end;
        Inc(I);
        if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
        begin
          Rect.Left := OriRect.Left;
          Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
          Inc(I);
        end;
        Dec(I);
        M1 := '';
      end
      else
      if (Text[I] = Chr(13)) and Cmp1(string(Chr(10))) then
      begin
        // new line
        Draw(M1);
        PlainItem := PlainItem + M1;
        Rect.Left := OriRect.Left;
        Rect.Top := Rect.Top + Canvas.TextHeight(M1 + 'W');
        M1 := '';
      end
      else
        M1 := M1 + Text[I]; // add text
      Inc(I);
    end; { for }
    Draw(M1);
    PlainItem := PlainItem + M1;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := LastFontStyle;
      Canvas.Font.Color := LastFontColor;
    end;
  end;
  Width := Rect.Left - Width + 2;
end;

function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False);
end;

function ItemHtPlain(const Text: string): string;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(nil, Rect(0, 0, -1, -1), [], Text, False, S, W, False);
  Result := S;
end;

function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string;
  const HideSelColor: Boolean): Integer;
var
  S: string;
  W: Integer;
begin
  ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
  Result := W;
end;

procedure ClearList(List: TList);
var
  I: Integer;
begin
  if Assigned(List) then
  begin
    if not (List is TObjectList) then
      for I := 0 to List.Count - 1 do
        TObject(List[I]).Free;
    List.Clear;
  end;
end;

procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: Word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GlobalAlloc(GMEM_MOVEABLE, MemStream.Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(MemStream.Memory^, DataPtr^, MemStream.Size);
        Clipboard.Clear;
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: Word);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Clipboard.Open;
  try
    Data := GetClipboardData(Format);
    if Data = 0 then
      Exit;
    DataPtr := GlobalLock(Data);
    if DataPtr = nil then
      Exit;
    try
      MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
      MemStream.Position := 0;
    finally
      GlobalUnlock(Data);
    end;
  finally
    Clipboard.Close;
  end;
end;

function GetPropTypeKind(PropInf: PPropInfo): TTypeKind;
begin
  Result := PropInf.PropType^.Kind;
end;

function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    Result := tkUnknown
  else
    Result := GetPropTypeKind(PropInf);
end;

function GetPropStr(Obj: TObject; const PropName: string): string;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateResFmt(@RsEPropertyNotExists, [PropName]);
  if not (GetPropTypeKind(PropInf) in tkStrings) then
    raise Exception.CreateResFmt(@RsEInvalidPropertyType, [PropName]);
  Result := GetStrProp(Obj, PropInf);
end;

function GetPropOrd(Obj: TObject; const PropName: string): Integer;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateResFmt(@RsEPropertyNotExists, [PropName]);
  if not (GetPropTypeKind(PropInf) in [tkInteger, tkChar, tkWChar, tkEnumeration, tkClass]) then
    raise Exception.CreateResFmt(@RsEInvalidPropertyType, [PropName]);
  Result := GetOrdProp(Obj, PropInf);
end;

function GetPropMethod(Obj: TObject; const PropName: string): TMethod;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateResFmt(@RsEPropertyNotExists, [PropName]);
  if not (GetPropTypeKind(PropInf) = tkMethod) then
    raise Exception.CreateResFmt(@RsEInvalidPropertyType, [PropName]);
  Result := GetMethodProp(Obj, PropInf);
end;

procedure PrepareIniSection(Ss: TStrings);
var
  I: Integer;
  S: string;
begin
  Ss.BeginUpdate;
  try
    for I := Ss.Count - 1 downto 0 do
    begin
      S := Trim(Ss[I]);
      if (S = '') or (S[1] = ';') or (S[1] = '#') then
        Ss.Delete(I);
    end;
  finally
    Ss.EndUpdate;
  end;
end;

{:Creates a TPointL structure from a pair of coordinates.
Call PointL to create a TPointL structure that represents the specified
coordinates. Use PointL to construct parameters for functions
that require a TPointL, rather than setting up local variables
for each parameter.
@param  X    The X coordinate.
@param  Y    The Y coordinate.
@return      A TPointL structure for coordinates X and Y.
@example        <Code>
var
  p: TPointL;
begin
  p := PointL(100, 100);
end;
</Code>
}

function PointL(const X, Y: Longint): TPointL;
begin
  Result.X := X;
  Result.Y := Y;
end;

{:Conditional assignment.
Returns the value in True or False depending on the condition Test.
@param  Test    The test condition.
@param  True    Returns this value if Test is True.
@param  False   Returns this value if Test is False.
@return         Value in True or False depending on Test.
@example        <Code>
bar := iif(foo, 1, 0);
</Code>
<br>has the same effects as:<br>
<Code>
if foo then
  bar := 1
else
  bar := 0;
</Code>
}

function iif(const Test: Boolean; const ATrue, AFalse: Variant): Variant;
begin
  if Test then
    Result := ATrue
  else
    Result := AFalse;
end;


{ begin JvIconClipboardUtils}
{ Icon clipboard routines }

var
  Private_CF_ICON: Word;

function CF_ICON: Word;
begin
  if Private_CF_ICON = 0 then
  begin
    { The following string should not be localized }
    Private_CF_ICON := RegisterClipboardFormat('Delphi Icon');
    TPicture.RegisterClipboardFormat(Private_CF_ICON, TIcon);
  end;
  Result := Private_CF_ICON;
end;

function CreateBitmapFromIcon(Icon: TIcon; BackColor: TColor): TBitmap;
var
  Ico: HICON;
  W, H: Integer;
begin
  Ico := CreateRealSizeIcon(Icon);
  try
    GetIconSize(Ico, W, H);
    Result := TBitmap.Create;
    try
      Result.Width := W;
      Result.Height := H;
      Result.Canvas.Brush.Color := BackColor;
      Result.Canvas.FillRect(Rect(0, 0, W, H));
      DrawIconEx(Result.Canvas.Handle, 0, 0, Ico, W, H, 0, 0, DI_NORMAL);
    except
      Result.Free;
      raise;
    end;
  finally
    DestroyIcon(Ico);
  end;
end;

procedure CopyIconToClipboard(Icon: TIcon; BackColor: TColor);
var
  Bmp: TBitmap;
  Stream: TStream;
  Data: THandle;
  Format: Word;
  Palette: HPalette;
  Buffer: Pointer;
begin
  Bmp := CreateBitmapFromIcon(Icon, BackColor);
  try
    Stream := TMemoryStream.Create;
    try
      Icon.SaveToStream(Stream);
      Palette := 0;
      with Clipboard do
      begin
        Open;
        try
          Clear;
          Bmp.SaveToClipboardFormat(Format, Data, Palette);
          SetClipboardData(Format, Data);
          if Palette <> 0 then
            SetClipboardData(CF_PALETTE, Palette);
          Data := GlobalAlloc(HeapAllocFlags, Stream.Size);
          try
            if Data <> 0 then
            begin
              Buffer := GlobalLock(Data);
              try
                Stream.Seek(0, 0);
                Stream.Read(Buffer^, Stream.Size);
                SetClipboardData(CF_ICON, Data);
              finally
                GlobalUnlock(Data);
              end;
            end;
          except
            GlobalFree(Data);
            raise;
          end;
        finally
          Close;
        end;
      end;
    finally
      Stream.Free;
    end;
  finally
    Bmp.Free;
  end;
end;

procedure AssignClipboardIcon(Icon: TIcon);
var
  Stream: TStream;
  Data: THandle;
  Buffer: Pointer;
begin
  if not Clipboard.HasFormat(CF_ICON) then
    Exit;
  with Clipboard do
  begin
    Open;
    try
      Data := GetClipboardData(CF_ICON);
      Buffer := GlobalLock(Data);
      try
        Stream := TMemoryStream.Create;
        try
          Stream.Write(Buffer^, GlobalSize(Data));
          Stream.Seek(0, 0);
          Icon.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
      finally
        GlobalUnlock(Data);
      end;
    finally
      Close;
    end;
  end;
end;

function CreateIconFromClipboard: TIcon;
begin
  Result := nil;
  if not Clipboard.HasFormat(CF_ICON) then
    Exit;
  Result := TIcon.Create;
  try
    AssignClipboardIcon(Result);
  except
    Result.Free;
    raise;
  end;
end;

{ Real-size icons support routines }
const
  RC3_STOCKICON = 0;
  RC3_ICON = 1;
  RC3_CURSOR = 2;

type
  PCursorOrIcon = ^TCursorOrIcon;
  TCursorOrIcon = packed record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

  PIconRec = ^TIconRec;
  TIconRec = packed record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

function WidthBytes(I: Longint): Longint;
begin
  Result := ((I + 31) div 32) * 4;
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8:
      Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

function DupBits(Src: HBITMAP; Size: TPoint; Mono: Boolean): HBITMAP;
var
  DC, Mem1, Mem2: HDC;
  Old1, Old2: HBITMAP;
  Bitmap: tagBITMAP;
begin
  Mem1 := CreateCompatibleDC(NullHandle);
  Mem2 := CreateCompatibleDC(NullHandle);
  GetObject(Src, SizeOf(Bitmap), @Bitmap);
  if Mono then
    Result := CreateBitmap(Size.X, Size.Y, 1, 1, nil)
  else
  begin
    DC := GetDC(HWND_DESKTOP);
    if DC = NullHandle then
      OutOfResources;
    try
      Result := CreateCompatibleBitmap(DC, Size.X, Size.Y);
      if Result = NullHandle then
        OutOfResources;
    finally
      ReleaseDC(HWND_DESKTOP, DC);
    end;
  end;
  if Result <> NullHandle then
  begin
    Old1 := SelectObject(Mem1, Src);
    Old2 := SelectObject(Mem2, Result);
    StretchBlt(Mem2, 0, 0, Size.X, Size.Y, Mem1, 0, 0, Bitmap.bmWidth,
      Bitmap.bmHeight, SRCCOPY);
    if Old1 <> NullHandle then
      SelectObject(Mem1, Old1);
    if Old2 <> NullHandle then
      SelectObject(Mem2, Old2);
  end;
  DeleteDC(Mem1);
  DeleteDC(Mem2);
end;

procedure TwoBitsFromDIB(var BI: TBitmapInfoHeader; var XorBits, AndBits: HBITMAP);
type
  PLongArray = ^TLongArray;
  TLongArray = array [0..1] of Longint;
var
  Temp: HBITMAP;
  NumColors: Integer;
  DC: HDC;
  Bits: Pointer;
  Colors: PLongArray;
  IconSize: TPoint;
  BM: tagBITMAP;
begin
  IconSize.X := GetSystemMetrics(SM_CXICON);
  IconSize.Y := GetSystemMetrics(SM_CYICON);
  with BI do
  begin
    biHeight := biHeight shr 1; { Size in record is doubled }
    biSizeImage := WidthBytes(Longint(biWidth) * biBitCount) * biHeight;
    NumColors := GetDInColors(biBitCount);
  end;
  DC := GetDC(HWND_DESKTOP);
  if DC = NullHandle then
    OutOfResources;
  try
    Bits := Pointer(PAnsiChar(@BI) + SizeOf(BI) + NumColors * SizeOf(TRGBQuad));
    Temp := CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS);
    if Temp = NullHandle then
      OutOfResources;
    try
      GetObject(Temp, SizeOf(BM), @BM);
      IconSize.X := BM.bmWidth;
      IconSize.Y := BM.bmHeight;
      XorBits := DupBits(Temp, IconSize, False);
    finally
      DeleteObject(Temp);
    end;
    with BI do
    begin
      Inc(INT_PTR(Bits), biSizeImage);
      biBitCount := 1;
      biSizeImage := WidthBytes(Longint(biWidth) * biBitCount) * biHeight;
      biClrUsed := 2;
      biClrImportant := 2;
    end;
    Colors := Pointer(PAnsiChar(@BI) + SizeOf(BI));
    Colors^[0] := 0;
    Colors^[1] := $FFFFFF;
    Temp := CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS);
    if Temp = NullHandle then
      OutOfResources;
    try
      AndBits := DupBits(Temp, IconSize, True);
    finally
      DeleteObject(Temp);
    end;
  finally
    ReleaseDC(HWND_DESKTOP, DC);
  end;
end;

procedure ReadIcon(Stream: TStream; var Icon: HICON; ImageCount: Integer;
  StartOffset: Integer);
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array [0..300] of TIconRec;
var
  List: PIconRecArray;
  HeaderLen, Length: Integer;
  Colors, BitsPerPixel: Word;
  C1, C2, N, Index: Integer;
  IconSize: TPoint;
  DC: HDC;
  BI: PBitmapInfoHeader;
  ResData: Pointer;
  XorBits, AndBits: HBITMAP;
  XorInfo, AndInfo: Windows.TBitmap;
  XorMem, AndMem: Pointer;
  XorLen, AndLen: Integer;
begin
  HeaderLen := SizeOf(TIconRec) * ImageCount;
  List := AllocMem(HeaderLen);
  try
    Stream.Read(List^, HeaderLen);
    IconSize.X := GetSystemMetrics(SM_CXICON);
    IconSize.Y := GetSystemMetrics(SM_CYICON);
    DC := GetDC(HWND_DESKTOP);
    if DC = NullHandle then
      OutOfResources;
    try
      BitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if BitsPerPixel = 24 then
        Colors := 0
      else
        Colors := 1 shl BitsPerPixel;
    finally
      ReleaseDC(HWND_DESKTOP, DC);
    end;
    Index := -1;
    { the following code determines which image most closely matches the
      current device. It is not meant to absolutely match Windows
      (known broken) algorithm }
    C2 := 0;
    for N := 0 to ImageCount - 1 do
    begin
      C1 := List^[N].Colors;
      if C1 = Colors then
      begin
        Index := N;
        Break;
      end
      else
      if Index = -1 then
      begin
        if C1 <= Colors then
        begin
          Index := N;
          C2 := List^[N].Colors;
        end;
      end
      else
      if C1 > C2 then
        Index := N;
    end;
    if Index = -1 then
      Index := 0;
    with List^[Index] do
    begin
      BI := AllocMem(DIBSize);
      try
        Stream.Seek(DIBOffset - (HeaderLen + StartOffset), 1);
        Stream.Read(BI^, DIBSize);
        TwoBitsFromDIB(BI^, XorBits, AndBits);
        GetObject(AndBits, SizeOf(Windows.TBitmap), @AndInfo);
        GetObject(XorBits, SizeOf(Windows.TBitmap), @XorInfo);
        IconSize.X := AndInfo.bmWidth;
        IconSize.Y := AndInfo.bmHeight;
        with AndInfo do
          AndLen := bmWidthBytes * bmHeight * bmPlanes;
        with XorInfo do
          XorLen := bmWidthBytes * bmHeight * bmPlanes;
        Length := AndLen + XorLen;
        ResData := AllocMem(Length);
        try
          AndMem := ResData;
          with AndInfo do
            XorMem := Pointer(PAnsiChar(ResData) + AndLen);
          GetBitmapBits(AndBits, AndLen, AndMem);
          GetBitmapBits(XorBits, XorLen, XorMem);
          DeleteObject(XorBits);
          DeleteObject(AndBits);
          Icon := CreateIcon(HInstance, IconSize.X, IconSize.Y,
            XorInfo.bmPlanes, XorInfo.bmBitsPixel, AndMem, XorMem);
          if Icon = 0 then
            OutOfResources;
        finally
          FreeMem(ResData, Length);
        end;
      finally
        FreeMem(BI, DIBSize);
      end;
    end;
  finally
    FreeMem(List, HeaderLen);
  end;
end;

procedure GetIconSize(Icon: HICON; var W, H: Integer);
var
  IconInfo: TIconInfo;
  BM: Windows.TBitmap;
begin
  if GetIconInfo(Icon, IconInfo) then
  begin
    try
      if IconInfo.hbmColor <> 0 then
      begin
        GetObject(IconInfo.hbmColor, SizeOf(BM), @BM);
        W := BM.bmWidth;
        H := BM.bmHeight;
      end
      else
      if IconInfo.hbmMask <> 0 then
      begin { Monochrome icon }
        GetObject(IconInfo.hbmMask, SizeOf(BM), @BM);
        W := BM.bmWidth;
        H := BM.bmHeight shr 1; { Size in record is doubled }
      end
      else
      begin
        W := GetSystemMetrics(SM_CXICON);
        H := GetSystemMetrics(SM_CYICON);
      end;
    finally
      if IconInfo.hbmColor <> 0 then
        DeleteObject(IconInfo.hbmColor);
      if IconInfo.hbmMask <> 0 then
        DeleteObject(IconInfo.hbmMask);
    end;
  end
  else
  begin
    W := GetSystemMetrics(SM_CXICON);
    H := GetSystemMetrics(SM_CYICON);
  end;
end;

function CreateRealSizeIcon(Icon: TIcon): HICON;
var
  Mem: TMemoryStream;
  CI: TCursorOrIcon;
begin
  Result := 0;
  Mem := TMemoryStream.Create;
  try
    Icon.SaveToStream(Mem);
    Mem.Position := 0;
    Mem.ReadBuffer(CI, SizeOf(CI));
    case CI.wType of
      RC3_STOCKICON:
        Result := LoadIcon(0, IDI_APPLICATION);
      RC3_ICON:
        ReadIcon(Mem, Result, CI.Count, SizeOf(CI));
    else
      Result := CopyIcon(Icon.Handle);
    end;
  finally
    Mem.Free;
  end;
end;

procedure DrawRealSizeIcon(Canvas: TCanvas; Icon: TIcon; X, Y: Integer);
var
  Ico: HICON;
  W, H: Integer;
begin
  Ico := CreateRealSizeIcon(Icon);
  try
    GetIconSize(Ico, W, H);
    DrawIconEx(Canvas.Handle, X, Y, Ico, W, H, 0, 0, DI_NORMAL);
  finally
    DestroyIcon(Ico);
  end;
end;

{ end JvIconClipboardUtils }

function CreateScreenCompatibleDC: HDC;
const
  HDC_DESKTOP = HDC(0);
begin
  Result := CreateCompatibleDC(HDC_DESKTOP);
end;

{$IFNDEF COMPILER12_UP}
function InvalidateRect(hWnd: HWND; const lpRect: TRect; bErase: BOOL): BOOL;
begin
  Result := Windows.InvalidateRect(hWnd, @lpRect, bErase);
end;

function InvalidateRect(hWnd: HWND; lpRect: PRect; bErase: BOOL): BOOL;
begin
  Result := Windows.InvalidateRect(hWnd, lpRect, bErase);
end;
{$ENDIF ~COMPILER12_UP}

{ begin JvRLE }

procedure RleCompressTo(InStream, OutStream: TStream);
var
  Count, Count2, Count3, I: Integer;
  Buf1: array [0..1024] of Byte;
  Buf2: array [0..60000] of Byte;
  B: Byte;
begin
  InStream.Position := 0;
  repeat
    Count := InStream.Read(Buf1, 1024);
    Count2 := 0;
    I := 0;
    while I < Count do
    begin
      B := Buf1[I];
      Count3 := 0;
      while (Buf1[I] = B) and (I < Count) and (Count3 < $30) do
      begin
        Inc(I);
        Inc(Count3);
      end;
      if (I = Count) and (Count3 in [2..$2F]) and (Count = 1024) then
        InStream.Position := InStream.Position - Count3
      else
      begin
        if Count3 = 1 then
        begin
          if (B and $C0) = $C0 then
          begin
            Buf2[Count2] := $C1;
            Buf2[Count2 + 1] := B;
            Inc(Count2, 2);
          end
          else
          begin
            Buf2[Count2] := B;
            Inc(Count2);
          end;
        end
        else
        begin
          Buf2[Count2] := Count3 or $C0;
          Buf2[Count2 + 1] := B;
          Inc(Count2, 2);
        end;
      end;
    end;
    OutStream.Write(Buf2, Count2);
  until Count <> 1024;
end;

procedure RleDecompressTo(InStream, OutStream: TStream);
var
  Count, Count2, Count3, I: Integer;
  Buf1: array [0..1024] of Byte;
  Buf2: array [0..60000] of Byte;
  B: Byte;
begin
  InStream.Position := 0;
  repeat
    Count := InStream.Read(Buf1, 1024);
    Count2 := 0;
    I := 0;
    while I < Count do
    begin
      if (Buf1[I] and $C0) = $C0 then
      begin
        if I = Count - 1 then
          InStream.Position := InStream.Position - 1
        else
        begin
          B := Buf1[I] and $3F;
          Inc(I);
          for Count3 := Count2 to Count2 + B - 1 do
            Buf2[Count3] := Buf1[I];
          Count2 := Count2 + B;
        end;
      end
      else
      begin
        Buf2[Count2] := Buf1[I];
        Inc(Count2);
      end;
      Inc(I);
    end;
    OutStream.Write(Buf2, Count2);
  until Count <> 1024;
end;

procedure RleCompress(Stream: TStream);
var
  Tmp: TMemoryStream;
begin
  Tmp := TMemoryStream.Create;
  try
    RleCompressTo(Stream, Tmp);
    Tmp.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Tmp, 0);
  finally
    Tmp.Free;
  end;
end;

procedure RleDecompress(Stream: TStream);
var
  Tmp: TMemoryStream;
begin
  Tmp := TMemoryStream.Create;
  try
    RleDecompressTo(Stream, Tmp);
    Tmp.Position := 0;
    Stream.Size := 0;
    Stream.CopyFrom(Tmp, 0);
  finally
    Tmp.Free;
  end;
end;
{ end JvRLE }

{ begin JvDateUtil }

function IsLeapYear(AYear: Integer): Boolean;
begin
  Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0));
end;

function DaysInAMonth(const AYear, AMonth: Word): Word;
begin
  Result := MonthDays[(AMonth = 2) and IsLeapYear(AYear), AMonth];
end;

function DaysPerMonth(AYear, AMonth: Integer): Integer;
begin
  Result := DaysInAMonth(AYear, AMonth);
end;

function FirstDayOfNextMonth: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Day := 1;
  if Month < 12 then
    Inc(Month)
  else
  begin
    Inc(Year);
    Month := 1;
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function FirstDayOfPrevMonth: TDateTime;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Day := 1;
  if Month > 1 then
    Dec(Month)
  else
  begin
    Dec(Year);
    Month := 12;
  end;
  Result := EncodeDate(Year, Month, Day);
end;

function LastDayOfPrevMonth: TDateTime;
var
  D: TDateTime;
  Year, Month, Day: Word;
begin
  D := FirstDayOfPrevMonth;
  DecodeDate(D, Year, Month, Day);
  Day := DaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day);
end;

function ExtractDay(ADate: TDateTime): Word;
var
  M, Y: Word;
begin
  DecodeDate(ADate, Y, M, Result);
end;

function ExtractMonth(ADate: TDateTime): Word;
var
  D, Y: Word;
begin
  DecodeDate(ADate, Y, Result, D);
end;

function ExtractYear(ADate: TDateTime): Word;
var
  D, M: Word;
begin
  DecodeDate(ADate, Result, M, D);
end;

function IncDate(ADate: TDateTime; Days, Months, Years: Integer): TDateTime;
var
  D, M, Y: Word;
  Day, Month, Year: Longint;
begin
  DecodeDate(ADate, Y, M, D);
  Year := Y;
  Month := M;
  Day := D;
  Inc(Year, Years);
  Inc(Year, Months div 12);
  Inc(Month, Months mod 12);
  if Month < 1 then
  begin
    Inc(Month, 12);
    Dec(Year);
  end
  else
  if Month > 12 then
  begin
    Dec(Month, 12);
    Inc(Year);
  end;
  if Day > DaysPerMonth(Year, Month) then
    Day := DaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day) + Days + Frac(ADate);
end;

procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Word);
{ Corrected by Anatoly A. Sanko (2:450/73) }
var
  DtSwap: TDateTime;
  Day1, Day2, Month1, Month2, Year1, Year2: Word;
begin
  if Date1 > Date2 then
  begin
    DtSwap := Date1;
    Date1 := Date2;
    Date2 := DtSwap;
  end;
  DecodeDate(Date1, Year1, Month1, Day1);
  DecodeDate(Date2, Year2, Month2, Day2);
  Years := Year2 - Year1;
  Months := 0;
  Days := 0;
  if Month2 < Month1 then
  begin
    Inc(Months, 12);
    Dec(Years);
  end;
  Inc(Months, Month2 - Month1);
  if Day2 < Day1 then
  begin
    Inc(Days, DaysPerMonth(Year1, Month1));
    if Months = 0 then
    begin
      Dec(Years);
      Months := 11;
    end
    else
      Dec(Months);
  end;
  Inc(Days, Day2 - Day1);
end;

function IncDay(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := ADate + Delta;
end;

function IncMonth(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncDate(ADate, 0, Delta, 0);
end;

function IncYear(ADate: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncDate(ADate, 0, 0, Delta);
end;

function MonthsBetween(Date1, Date2: TDateTime): Double;
var
  D, M, Y: Word;
begin
  DateDiff(Date1, Date2, D, M, Y);
  Result := 12 * Y + M;
  if (D > 1) and (D < 7) then
    Result := Result + 0.25
  else
  if (D >= 7) and (D < 15) then
    Result := Result + 0.5
  else
  if (D >= 15) and (D < 21) then
    Result := Result + 0.75
  else
  if D >= 21 then
    Result := Result + 1;
end;

function IsValidDate(Y, M, D: Word): Boolean;
begin
  Result := (Y >= 1) and (Y <= 9999) and (M >= 1) and (M <= 12) and
    (D >= 1) and (D <= DaysPerMonth(Y, M));
end;

function ValidDate(ADate: TDateTime): Boolean;
var
  Year, Month, Day: Word;
begin
  try
    DecodeDate(ADate, Year, Month, Day);
    Result := IsValidDate(Year, Month, Day);
  except
    Result := False;
  end;
end;

function DaysInPeriod(Date1, Date2: TDateTime): Longint;
begin
  if ValidDate(Date1) and ValidDate(Date2) then
    Result := Abs(Trunc(Date2) - Trunc(Date1)) + 1
  else
    Result := 0;
end;

{ // (ahuser) wrong implementation
function DaysBetween(Date1, Date2: TDateTime): Longint;
begin
  Result := Trunc(Date2) - Trunc(Date1) + 1;
  if Result < 0 then
    Result := 0;
end;}

function DaysBetween(Date1, Date2: TDateTime): Longint;
begin
  if Date1 < Date2 then
    Result := Trunc(Date2 - Date1)
  else
    Result := Trunc(Date1 - Date2);
end;

function IncTime(ATime: TDateTime; Hours, Minutes, Seconds,
  MSecs: Integer): TDateTime;
begin
  Result := ATime + (Hours div 24) + (((Hours mod 24) * 3600000 +
    Minutes * 60000 + Seconds * 1000 + MSecs) / MSecsPerDay);
  if Result < 0 then
    Result := Result + 1;
end;

function IncHour(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, Delta, 0, 0, 0);
end;

function IncMinute(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, Delta, 0, 0);
end;

function IncSecond(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, 0, Delta, 0);
end;

function IncMSec(ATime: TDateTime; Delta: Integer): TDateTime;
begin
  Result := IncTime(ATime, 0, 0, 0, Delta);
end;

function CutTime(ADate: TDateTime): TDateTime;
begin
  Result := Trunc(ADate);
end;

function CurrentYear: Word;
begin
  Result := ExtractYear(Date);
end;

{ String to date conversions. Copied from SYSUTILS.PAS unit. }

procedure ScanBlanks(const S: string; var Pos: Integer);
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do
    Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; MaxLength: Integer; var Pos: Integer;
  var Number: Longint): Boolean;
var
  I: Integer;
  N: Word;
begin
  Result := False;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and (Longint(I - Pos) < MaxLength) and
    CharInSet(S[I], ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean;
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

procedure ScanToNumber(const S: string; var Pos: Integer);
begin
  while (Pos <= Length(S)) and not CharInSet(S[Pos], ['0'..'9']) do
  begin
    {$IFNDEF UNICODE} // Utf16: '0'..'9' are in the BMP => no lead byte handling necessary
    if S[Pos] in LeadBytes then
      Inc(Pos);
    {$ENDIF ~UNICODE}
    Inc(Pos);
  end;
end;

function GetDateOrder(const DateFormat: string): TDateOrder;
var
  I: Integer;
begin
  Result := DefaultDateOrder;
  I := 1;
  while I <= Length(DateFormat) do
  begin
    case Chr(Ord(DateFormat[I]) and $DF) of
      'E':
        Result := doYMD;
      'Y':
        Result := doYMD;
      'M':
        Result := doMDY;
      'D':
        Result := doDMY;
    else
      Inc(I);
      Continue;
    end;
    Exit;
  end;
end;

function CurrentMonth: Word;
begin
  Result := ExtractMonth(Date);
end;

{Modified}

function ExpandYear(Year: Integer): Integer;
var
  N: Longint;
begin
  if Year = -1 then
    Result := CurrentYear
  else
  begin
    Result := Year;
    if Result < 100 then
    begin
      N := CurrentYear - CenturyOffset;
      Inc(Result, N div 100 * 100);
      if (CenturyOffset > 0) and (Result < N) then
        Inc(Result, 100);
    end;
  end;
end;

function ScanDate(const S, DateFormat: string; var Position: Integer;
  var Y, M, D: Integer): Boolean;
var
  DateOrder: TDateOrder;
  N1, N2, N3: Longint;
begin
  Result := False;
  Y := 0;
  M := 0;
  D := 0;
  DateOrder := GetDateOrder(DateFormat);
  if JclFormatSettings.ShortDateFormat[1] = 'g' then { skip over prefix text }
    ScanToNumber(S, Position);
  if not (ScanNumber(S, MaxInt, Position, N1) and ScanChar(S, Position, JclFormatSettings.DateSeparator) and
    ScanNumber(S, MaxInt, Position, N2)) then
    Exit;
  if ScanChar(S, Position, JclFormatSettings.DateSeparator) then
  begin
    if not ScanNumber(S, MaxInt, Position, N3) then
      Exit;
    case DateOrder of
      doMDY:
        begin
          Y := N3;
          M := N1;
          D := N2;
        end;
      doDMY:
        begin
          Y := N3;
          M := N2;
          D := N1;
        end;
      doYMD:
        begin
          Y := N1;
          M := N2;
          D := N3;
        end;
    end;
    Y := ExpandYear(Y);
  end
  else
  begin
    Y := CurrentYear;
    if DateOrder = doDMY then
    begin
      D := N1;
      M := N2;
    end
    else
    begin
      M := N1;
      D := N2;
    end;
  end;
  ScanChar(S, Position, JclFormatSettings.DateSeparator);
  ScanBlanks(S, Position);
  if SysLocale.FarEast and (Pos('ddd', JclFormatSettings.ShortDateFormat) <> 0) then
  begin { ignore trailing text }
    if CharInSet(JclFormatSettings.ShortTimeFormat[1], ['0'..'9']) then { stop at time digit }
      ScanToNumber(S, Position)
    else { stop at time prefix }
      repeat
        while (Position <= Length(S)) and (S[Position] <> ' ') do
          Inc(Position);
        ScanBlanks(S, Position);
      until (Position > Length(S)) or
        AnsiSameText(JclFormatSettings.TimeAMString, Copy(S, Position, Length(JclFormatSettings.TimeAMString))) or
        AnsiSameText(JclFormatSettings.TimePMString, Copy(S, Position, Length(JclFormatSettings.TimePMString)));
  end;
  Result := IsValidDate(Y, M, D) and (Position > Length(S));
end;

function MonthFromName(const S: string; MaxLen: Byte): Byte;
begin
  if Length(S) > 0 then
    for Result := 1 to 12 do
    begin
      if (Length(JclFormatSettings.LongMonthNames[Result]) > 0) and
         AnsiSameText(Copy(S, 1, MaxLen), Copy(JclFormatSettings.LongMonthNames[Result], 1, MaxLen)) then
        Exit;
    end;
  Result := 0;
end;

procedure ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
  var I: Integer; Blank, Default: Integer);
var
  Tmp: string;
  J, L: Integer;
begin
  I := Default;
  Ch := UpCase(Ch);
  L := Length(Format);
  if Length(S) < L then
    L := Length(S)
  else
  if Length(S) > L then
    Exit;
  J := Pos(MakeStr(Ch, Cnt), AnsiUpperCase(Format));
  if J <= 0 then
    Exit;
  Tmp := '';
  while (UpCase(Format[J]) = Ch) and (J <= L) do
  begin
    if S[J] <> ' ' then
      Tmp := Tmp + S[J];
    Inc(J);
  end;
  if Tmp = '' then
    I := Blank
  else
  if Cnt > 1 then
  begin
    I := MonthFromName(Tmp, Length(Tmp));
    if I = 0 then
      I := -1;
  end
  else
    I := StrToIntDef(Tmp, -1);
end;

function ScanDateStr(const Format, S: string; var D, M, Y: Integer): Boolean;
var
  Pos: Integer;
begin
  ExtractMask(Format, S, 'm', 3, M, -1, 0); { short month name? }
  if M = 0 then
    ExtractMask(Format, S, 'm', 1, M, -1, 0);
  ExtractMask(Format, S, 'd', 1, D, -1, 1);
  ExtractMask(Format, S, 'y', 1, Y, -1, CurrentYear);
  if M = -1 then
    M := CurrentMonth;
  Y := ExpandYear(Y);
  Result := IsValidDate(Y, M, D);
  if not Result then
  begin
    Pos := 1;
    Result := ScanDate(S, Format, Pos, Y, M, D);
  end;
end;

function InternalStrToDate(const DateFormat, S: string;
  var Date: TDateTime): Boolean;
var
  D, M, Y: Integer;
begin
  if S = '' then
  begin
    Date := NullDate;
    Result := True;
  end
  else
  begin
    Result := ScanDateStr(DateFormat, S, D, M, Y);
    if Result then
    try
      Date := EncodeDate(Y, M, D);
    except
      Result := False;
    end;
  end;
end;

function StrToDateFmt(const DateFormat, S: string): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDate, [S]);
end;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(JclFormatSettings.ShortDateFormat, S, Result) then
    Result := Trunc(Default);
end;

function StrToDateFmtDef(const DateFormat, S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    Result := Trunc(Default);
end;

function DefDateFormat(AFourDigitYear: Boolean): string;
begin
  if AFourDigitYear then
  begin
    case GetDateOrder(JclFormatSettings.ShortDateFormat) of
      doMDY:
        Result := 'MM/DD/YYYY';
      doDMY:
        Result := 'DD/MM/YYYY';
      doYMD:
        Result := 'YYYY/MM/DD';
    end;
  end
  else
  begin
    case GetDateOrder(JclFormatSettings.ShortDateFormat) of
      doMDY:
        Result := 'MM/DD/YY';
      doDMY:
        Result := 'DD/MM/YY';
      doYMD:
        Result := 'YY/MM/DD';
    end;
  end;
end;

function DefDateMask(BlanksChar: Char; AFourDigitYear: Boolean): string;
begin
  if AFourDigitYear then
  begin
    case GetDateOrder(JclFormatSettings.ShortDateFormat) of
      doMDY, doDMY:
        Result := '!99/99/9999;1;';
      doYMD:
        Result := '!9999/99/99;1;';
    end;
  end
  else
  begin
    case GetDateOrder(JclFormatSettings.ShortDateFormat) of
      doMDY, doDMY:
        Result := '!99/99/99;1;';
      doYMD:
        Result := '!99/99/99;1;';
    end;
  end;
  if Result <> '' then
    Result := Result + BlanksChar;
end;

function FormatLongDate(Value: TDateTime): string;
{$IFDEF MSWINDOWS}
var
  Buffer: array [0..1023] of Char;
  SystemTime: TSystemTime;
begin
  DateTimeToSystemTime(Value, SystemTime);
  SetString(Result, Buffer, GetDateFormat(GetThreadLocale, DATE_LONGDATE,
    @SystemTime, nil, Buffer, SizeOf(Buffer) - 1));
  Result := TrimRight(Result);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := TrimRight(FormatDateTime(LongDateFormat, Value));
end;
{$ENDIF UNIX}

function FormatLongDateTime(Value: TDateTime): string;
begin
  if Value <> NullDate then
    Result := FormatLongDate(Value) + FormatDateTime(' tt', Value)
  else
    Result := '';
end;

function FourDigitYear: Boolean; // deprecated
begin
  Result := IsFourDigitYear;
end;

function IsFourDigitYear: Boolean;
begin
  Result := Pos('YYYY', AnsiUpperCase(JclFormatSettings.ShortDateFormat)) > 0;
end;
{ end JvDateUtil }

function BufToBinStr(Buf: Pointer; BufSize: Integer): string;
var
  I: Integer;
  P: PByteArray;
begin
  P := Buf;
  for I := 0 to Pred(BufSize) do
    Result := Result + IntToHex(P[I] , 2);
end;

function BinStrToBuf(Value: string; Buf: Pointer; BufSize: Integer): Integer;
var
  I: Integer;
  P: PByteArray;
begin
  if Odd(Length(Value)) then
    Value := '0' + Value;      // should not occur, might indicate corrupted Value
  if (Length(Value) div 2) < BufSize then
    BufSize := Length(Value) div 2;
  P := Buf;
  for I := 0 to Pred(BufSize) do
    P[I] := StrToInt('$' + Value[2 * I + 1] + Value[2 * I + 2]);
  Result := BufSize;
end;

{ begin JvStrUtils }
{$IFDEF UNIX}

function iconversion(InP: PAnsiChar; OutP: Pointer; InBytes, OutBytes: Cardinal;
  const ToCode, FromCode: AnsiString): Boolean;
var
  conv: iconv_t;
begin
  Result := False;
  if (InBytes > 0) and (OutBytes > 0) and (InP <> nil) and (OutP <> nil) then
  begin
    conv := iconv_open(PAnsiChar(ToCode), PAnsiChar(FromCode));
    if Integer(conv) <> -1 then
    begin
      if Integer(iconv(conv, InP, InBytes, OutP, OutBytes)) <> -1 then
        Result := True;
      iconv_close(conv);
    end;
  end;
end;

function iconvString(const S, ToCode, FromCode: AnsiString): AnsiString;
begin
  SetLength(Result, Length(S));
  if not iconversion(PAnsiChar(S), Pointer(Result),
    Length(S), Length(Result),
    ToCode, FromCode) then
    Result := S;
end;

function iconvWideString(const S: WideString; const ToCode, FromCode: AnsiString): WideString;
begin
  SetLength(Result, Length(S));
  if not iconversion(Pointer(S), Pointer(Result),
    Length(S) * SizeOf(WideChar), Length(Result) * SizeOf(WideChar),
    ToCode, FromCode) then
    Result := S;
end;

function OemStrToAnsi(const S: AnsiString): AnsiString;
begin
  Result := iconvString(S, 'WINDOWS-1252', 'CP850');
end;

function AnsiStrToOem(const S: AnsiString): AnsiString;
begin
  Result := iconvString(S, 'CP850', 'WINDOWS-1250');
end;

{$ENDIF UNIX}

function StrToOem(const AnsiStr: AnsiString): AnsiString;
begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, Length(AnsiStr));
  if Result <> '' then
    CharToOemBuffA(PAnsiChar(AnsiStr), PAnsiChar(Result), Length(Result));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := AnsiStrToOem(AnsiStr);
  {$ENDIF UNIX}
end;

function OemToAnsiStr(const OemStr: AnsiString): AnsiString;
begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, Length(OemStr));
  if Length(Result) > 0 then
    OemToCharBuffA(PAnsiChar(OemStr), PAnsiChar(Result), Length(Result));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := OemStrToAnsi(OemStr);
  {$ENDIF UNIX}
end;

function IsEmptyStr(const S: string; const EmptyChars: TSysCharSet): Boolean;
var
  I, SLen: Integer;
begin
  SLen := Length(S);
  I := 1;
  while I <= SLen do
  begin
    if not CharInSet(S[I], EmptyChars) then
    begin
      Result := False;
      Exit;
    end
    else
      Inc(I);
  end;
  Result := True;
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then
    begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else
      Result := Result + Source;
  until I <= 0;
end;

function DelSpace(const S: string): string;
begin
  Result := DelChars(S, ' ');
end;

function DelChars(const S: string; Chr: Char): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
  begin
    if Result[I] = Chr then
      Delete(Result, I, 1);
  end;
end;

function DelBSpace(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] = ' ') do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function DelESpace(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function DelRSpace(const S: string): string;
begin
  Result := DelBSpace(DelESpace(S));
end;

function DelSpace1(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 2 do
  begin
    if (Result[I] = ' ') and (Result[I - 1] = ' ') then
      Delete(Result, I, 1);
  end;
end;

function Tab2Space(const S: string; Numb: Byte): string;
var
  I: Integer;
begin
  I := 1;
  Result := S;
  while I <= Length(Result) do
  begin
    if Result[I] = Chr(9) then
    begin
      Delete(Result, I, 1);
      Insert(MakeStr(' ', Numb), Result, I);
      Inc(I, Numb);
    end
    else
      Inc(I);
  end;
end;

function MakeStr(C: Char; N: Integer): string; overload;
begin
  if N < 1 then
    Result := ''
  else
  begin
    SetLength(Result, N);
    FillString(Result, Length(Result), C);
  end;
end;

{$IFNDEF COMPILER12_UP}
function MakeStr(C: WideChar; N: Integer): WideString; overload;
begin
  if N < 1 then
    Result := ''
  else
  begin
    SetLength(Result, N);
    FillWideChar(Result[1], Length(Result), C);
  end;
end;
{$ENDIF !COMPILER12_UP}

function MS(C: Char; N: Integer): string;
begin
  Result := MakeStr(C, N);
end;

function NPos(const C: string; S: string; N: Integer): Integer;
var
  I, P, K: Integer;
begin
  Result := 0;
  K := 0;
  for I := 1 to N do
  begin
    P := Pos(C, S);
    Inc(K, P);
    if (I = N) and (P > 0) then
    begin
      Result := K;
      Exit;
    end;
    if P > 0 then
      Delete(S, 1, P)
    else
      Exit;
  end;
end;

function AddChar(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else
    Result := S;
end;

function AddCharR(C: Char; const S: string; N: Integer): string;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else
    Result := S;
end;

function LeftStr(const S: string; N: Integer): string;
begin
  Result := AddCharR(' ', S, N);
end;

function RightStr(const S: string; N: Integer): string;
begin
  Result := AddChar(' ', S, N);
end;

{$IFDEF MSWINDOWS}

function CompStr(const S1, S2: string): Integer;
begin
  Result := CompareString(GetThreadLocale, SORT_STRINGSORT, PChar(S1),
    Length(S1), PChar(S2), Length(S2)) - 2;
end;

function CompText(const S1, S2: string): Integer;
begin
  Result := CompareString(GetThreadLocale, SORT_STRINGSORT or NORM_IGNORECASE,
    PChar(S1), Length(S1), PChar(S2), Length(S2)) - 2;
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}

function CompStr(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(S1, S2);
end;

function CompText(const S1, S2: string): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;

{$ENDIF UNIX}

function Copy2Symb(const S: string; Symb: Char): string;
var
  P: Integer;
begin
  P := Pos(Symb, S);
  if P = 0 then
    P := Length(S) + 1;
  Result := Copy(S, 1, P - 1);
end;

function Copy2SymbDel(var S: string; Symb: Char): string;
begin
  Result := Copy2Symb(S, Symb);
  S := DelBSpace(Copy(S, Length(Result) + 1, Length(S)));
end;

function Copy2Space(const S: string): string;
begin
  Result := Copy2Symb(S, ' ');
end;

function Copy2SpaceDel(var S: string): string;
begin
  Result := Copy2SymbDel(S, ' ');
end;

function AnsiProperCase(const S: string; const WordDelims: TSysCharSet): string;
var
  SLen, I: Cardinal;
begin
  Result := AnsiLowerCase(S);
  I := 1;
  SLen := Length(Result);
  while I <= SLen do
  begin
    while (I <= SLen) and CharInSet(Result[I], WordDelims) do
      Inc(I);
    if I <= SLen then
      Result[I] := AnsiUpperCase(Result[I])[1];
    while (I <= SLen) and not CharInSet(Result[I], WordDelims) do
      Inc(I);
  end;
end;

function WordCount(const S: string; const WordDelims: TSysCharSet): Integer;
var
  SLen, I: Cardinal;
begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  while I <= SLen do
  begin
    while (I <= SLen) and CharInSet(S[I], WordDelims) do
      Inc(I);
    if I <= SLen then
      Inc(Result);
    while (I <= SLen) and not CharInSet(S[I], WordDelims) do
      Inc(I);
  end;
end;

function WordPosition(const N: Integer; const S: string;
  const WordDelims: TSysCharSet): Integer;
var
  Count, I: Integer;
begin
  Count := 0;
  I := 1;
  Result := 0;
  while (I <= Length(S)) and (Count <> N) do
  begin
    { skip over delimiters }
    while (I <= Length(S)) and CharInSet(S[I], WordDelims) do
      Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then
      Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
        Inc(I)
    else
      Result := I;
  end;
end;

function ExtractWord(N: Integer; const S: string;
  const WordDelims: TSysCharSet): string;
var
  I: Integer;
  Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

function ExtractWordPos(N: Integer; const S: string;
  const WordDelims: TSysCharSet; var Pos: Integer): string;
var
  I, Len: Integer;
begin
  Len := 0;
  I := WordPosition(N, S, WordDelims);
  Pos := I;
  if I <> 0 then
    { find the end of the current word }
    while (I <= Length(S)) and not CharInSet(S[I], WordDelims) do
    begin
      { add the I'th character to result }
      Inc(Len);
      SetLength(Result, Len);
      Result[Len] := S[I];
      Inc(I);
    end;
  SetLength(Result, Len);
end;

function ExtractDelimited(N: Integer; const S: string;
  const Delims: TSysCharSet): string;
var
  CurWord: Integer;
  I, Len, SLen: Integer;
begin
  CurWord := 0;
  I := 1;
  Len := 0;
  SLen := Length(S);
  SetLength(Result, 0);
  while (I <= SLen) and (CurWord <> N) do
  begin
    if CharInSet(S[I], Delims) then
      Inc(CurWord)
    else
    begin
      if CurWord = N - 1 then
      begin
        Inc(Len);
        SetLength(Result, Len);
        Result[Len] := S[I];
      end;
    end;
    Inc(I);
  end;
end;

function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not CharInSet(S[I], Delims) do
    Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and CharInSet(S[I], Delims) then
    Inc(I);
  Pos := I;
end;

function IsWordPresent(const W, S: string; const WordDelims: TSysCharSet): Boolean;
var
  Count, I: Integer;
begin
  Result := False;
  Count := WordCount(S, WordDelims);
  for I := 1 to Count do
    if ExtractWord(I, S, WordDelims) = W then
    begin
      Result := True;
      Exit;
    end;
end;

function QuotedString(const S: string; Quote: Char): string;
begin
  Result := AnsiQuotedStr(S, Quote);
end;

function ExtractQuotedString(const S: string; Quote: Char): string;
begin
  Result := DequotedStr(S, Quote);
end;

function Numb2USA(const S: string): string;
var
  I, NA: Integer;
begin
  I := Length(S);
  Result := S;
  NA := 0;
  while (I > 0) do
  begin
    if ((Length(Result) - I + 1 - NA) mod 3 = 0) and (I <> 1) then
    begin
      Insert(',', Result, I);
      Inc(NA);
    end;
    Dec(I);
  end;
end;

function CenterStr(const S: string; Len: Integer): string;
begin
  if Length(S) < Len then
  begin
    Result := MakeStr(' ', (Len div 2) - (Length(S) div 2)) + S;
    Result := Result + MakeStr(' ', Len - Length(Result));
  end
  else
    Result := S;
end;

function Dec2Hex(N: Longint; A: Byte): string;
begin
  Result := IntToHex(N, A);
end;

function Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$', S) = 0 then
    HexStr := '$' + S
  else
    HexStr := S;
  Result := StrToIntDef(HexStr, 0);
end;

function Dec2Numb(N: Int64; A, B: Byte): string;
var
  C: Integer;
  Number: Cardinal;
begin
  if N = 0 then
    Result := '0'
  else
  begin
    Number := Cardinal(N);
    Result := '';
    while Number > 0 do
    begin
      C := Number mod B;
      if C > 9 then
        C := C + 55
      else
        C := C + 48;
      Result := Chr(C) + Result;
      Number := Number div B;
    end;
  end;
  if Result <> '' then
    Result := AddChar('0', Result, A);
end;

function Numb2Dec(S: string; B: Byte): Int64;
var
  I, P: Int64;
begin
  I := Length(S);
  Result := 0;
  S := UpperCase(S);
  P := 1;
  while (I >= 1) do
  begin
    if S[I] > '@' then
      Result := Result + (Ord(S[I]) - 55) * P
    else
      Result := Result + (Ord(S[I]) - 48) * P;
    Dec(I);
    P := P * B;
  end;
end;

function RomanToInt(const S: string): Longint;
const
  RomanChars = ['C', 'D', 'I', 'L', 'M', 'V', 'X'];
  RomanValues: array ['C'..'X'] of Word =
    (100, 500, 0, 0, 0, 0, 1, 0, 0, 50, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 10);
var
  Index, Next: Char;
  I: Integer;
  Negative: Boolean;
begin
  Result := 0;
  I := 0;
  Negative := (Length(S) > 0) and (S[1] = '-');
  if Negative then
    Inc(I);
  while (I < Length(S)) do
  begin
    Inc(I);
    Index := UpCase(S[I]);
    if CharInSet(Index, RomanChars) then
    begin
      if Succ(I) <= Length(S) then
        Next := UpCase(S[I + 1])
      else
        Next := #0;
      if CharInSet(Next, RomanChars) and (RomanValues[Index] < RomanValues[Next]) then
      begin
        Inc(Result, RomanValues[Next]);
        Dec(Result, RomanValues[Index]);
        Inc(I);
      end
      else
        Inc(Result, RomanValues[Index]);
    end
    else
    begin
      Result := 0;
      Exit;
    end;
  end;
  if Negative then
    Result := -Result;
end;

function IntToRoman(Value: Longint): string;
label
  A500, A400, A100, A90, A50, A40, A10, A9, A5, A4, A1;
begin
  Result := '';
  while Value >= 1000 do
  begin
    Dec(Value, 1000);
    Result := Result + 'M';
  end;
  if Value < 900 then
    goto A500
  else
  begin
    Dec(Value, 900);
    Result := Result + 'CM';
  end;
  goto A90;
  A400:
  if Value < 400 then
    goto A100
  else
  begin
    Dec(Value, 400);
    Result := Result + 'CD';
  end;
  goto A90;
  A500:
  if Value < 500 then
    goto A400
  else
  begin
    Dec(Value, 500);
    Result := Result + 'D';
  end;
  A100:
  while Value >= 100 do
  begin
    Dec(Value, 100);
    Result := Result + 'C';
  end;
  A90:
  if Value < 90 then
    goto A50
  else
  begin
    Dec(Value, 90);
    Result := Result + 'XC';
  end;
  goto A9;
  A40:
  if Value < 40 then
    goto A10
  else
  begin
    Dec(Value, 40);
    Result := Result + 'XL';
  end;
  goto A9;
  A50:
  if Value < 50 then
    goto A40
  else
  begin
    Dec(Value, 50);
    Result := Result + 'L';
  end;
  A10:
  while Value >= 10 do
  begin
    Dec(Value, 10);
    Result := Result + 'X';
  end;
  A9:
  if Value < 9 then
    goto A5
  else
    Result := Result + 'IX';
  Exit;
  A4:
  if Value < 4 then
    goto A1
  else
    Result := Result + 'IV';
  Exit;
  A5:
  if Value < 5 then
    goto A4
  else
  begin
    Dec(Value, 5);
    Result := Result + 'V';
  end;
  goto A1;
  A1:
  while Value >= 1 do
  begin
    Dec(Value);
    Result := Result + 'I';
  end;
end;

function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
begin
  Result := '';
  if Digits > 32 then
    Digits := 32;
  while Digits > 0 do
  begin
    if (Digits mod Spaces) = 0 then
      Result := Result + ' ';
    Dec(Digits);
    Result := Result + IntToStr((Value shr Digits) and 1);
  end;
end;

function FindPart(const HelpWilds, InputStr: string): Integer;
var
  I, J: Integer;
  Diff: Integer;
begin
  I := Pos('?', HelpWilds);
  if I = 0 then
  begin
    { if no '?' in HelpWilds }
    Result := Pos(HelpWilds, InputStr);
    Exit;
  end;
  { '?' in HelpWilds }
  Diff := Length(InputStr) - Length(HelpWilds);
  if Diff < 0 then
  begin
    Result := 0;
    Exit;
  end;
  { now move HelpWilds over InputStr }
  for I := 0 to Diff do
  begin
    for J := 1 to Length(HelpWilds) do
    begin
      if (InputStr[I + J] = HelpWilds[J]) or (HelpWilds[J] = '?') then
      begin
        if J = Length(HelpWilds) then
        begin
          Result := I + 1;
          Exit;
        end;
      end
      else
        Break;
    end;
  end;
  Result := 0;
end;

function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;

  function SearchNext(var Wilds: string): Integer;
    { looking for next *, returns position and string until position }
  begin
    Result := Pos('*', Wilds);
    if Result > 0 then
      Wilds := Copy(Wilds, 1, Result - 1);
  end;

var
  CWild, CInputWord: Integer; { counter for positions }
  I, LenHelpWilds: Integer;
  MaxInputWord, MaxWilds: Integer; { Length of InputStr and Wilds }
  HelpWilds: string;
begin
  if Wilds = InputStr then
  begin
    Result := True;
    Exit;
  end;
  repeat { delete '**', because '**' = '*' }
    I := Pos('**', Wilds);
    if I > 0 then
      Wilds := Copy(Wilds, 1, I - 1) + '*' + Copy(Wilds, I + 2, MaxInt);
  until I = 0;
  if Wilds = '*' then
  begin { for fast end, if Wilds only '*' }
    Result := True;
    Exit;
  end;
  MaxInputWord := Length(InputStr);
  MaxWilds := Length(Wilds);
  if IgnoreCase then
  begin { upcase all letters }
    InputStr := AnsiUpperCase(InputStr);
    Wilds := AnsiUpperCase(Wilds);
  end;
  if (MaxWilds = 0) or (MaxInputWord = 0) then
  begin
    Result := False;
    Exit;
  end;
  CInputWord := 1;
  CWild := 1;
  Result := True;
  repeat
    if InputStr[CInputWord] = Wilds[CWild] then
    begin { equal letters }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '?' then
    begin { equal to '?' }
      { goto next letter }
      Inc(CWild);
      Inc(CInputWord);
      Continue;
    end;
    if Wilds[CWild] = '*' then
    begin { handling of '*' }
      HelpWilds := Copy(Wilds, CWild + 1, MaxWilds);
      I := SearchNext(HelpWilds);
      LenHelpWilds := Length(HelpWilds);
      if I = 0 then
      begin
        { no '*' in the rest, compare the ends }
        if HelpWilds = '' then
          Exit; { '*' is the last letter }
        { check the rest for equal Length and no '?' }
        for I := 0 to LenHelpWilds - 1 do
        begin
          if (HelpWilds[LenHelpWilds - I] <> InputStr[MaxInputWord - I]) and
            (HelpWilds[LenHelpWilds - I] <> '?') then
          begin
            Result := False;
            Exit;
          end;
        end;
        Exit;
      end;
      { handle all to the next '*' }
      Inc(CWild, 1 + LenHelpWilds);
      I := FindPart(HelpWilds, Copy(InputStr, CInputWord, MaxInt));
      if I = 0 then
      begin
        Result := False;
        Exit;
      end;
      CInputWord := I + LenHelpWilds;
      Continue;
    end;
    Result := False;
    Exit;
  until (CInputWord > MaxInputWord) or (CWild > MaxWilds);
  { no completed evaluation }
  if CInputWord <= MaxInputWord then
    Result := False;
  if (CWild <= MaxWilds) and (Wilds[MaxWilds] <> '*') then
    Result := False;
end;

function XorString(const Key, Src: ShortString): ShortString;
var
  I: Integer;
begin
  Result := Src;
  if Length(Key) > 0 then
    for I := 1 to Length(Src) do
      Result[I] := AnsiChar(Chr(Byte(Key[1 + ((I - 1) mod Length(Key))]) xor Ord(Src[I])));
end;

function XorEncode(const Key, Source: string): string;
var
  I, KeyLen: Integer;
  C: Byte;
begin
  Result := '';
  KeyLen := Length(Key);
  for I := 1 to Length(Source) do
  begin
    if KeyLen > 0 then
      C := Byte(Key[1 + ((I - 1) mod KeyLen)]) xor Byte(Source[I])
    else
      C := Byte(Source[I]);
    Result := Result + AnsiLowerCase(IntToHex(C, 2));
  end;
end;

function XorDecode(const Key, Source: string): string;
var
  I, KeyLen: Integer;
  C: Char;
begin
  Result := '';
  KeyLen := Length(Key);
  for I := 0 to Length(Source) div 2 - 1 do
  begin
    C := Char(StrToIntDef('$' + string(Source[I * 2 + 1] + Source[I * 2 + 2]), Ord(' ')));
    if KeyLen > 0 then
      C := Char(Byte(Key[1 + (I mod KeyLen)]) xor Byte(C));
    Result := Result + C;
  end;
end;

function XorEncodeString(const Key, Source: string): string;
const
  HexChars: array[0..15] of Char =
    ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  I, KeyLen: Integer;
  C: Byte;
  Utf8Src, Utf8Key: UTF8String;
begin
  Result := '';
  Utf8Src := UTF8Encode(Source);
  Utf8Key := UTF8Encode(Key);
  KeyLen := Length(Utf8Key);
  SetLength(Result, Length(Utf8Src) * 2);
  for I := 1 to Length(Utf8Src) do
  begin
    if KeyLen > 0 then
      C := Byte(Utf8Src[I]) xor Byte(Utf8Key[1 + ((I - 1) mod KeyLen)])
    else
      C := Byte(Utf8Src[I]);
    Result[1 + (I - 1) * 2] := HexChars[C shr 4];
    Result[1 + (I - 1) * 2 + 1] := HexChars[C and $0F];
  end;
end;

function XorDecodeString(const Key, Source: string): string;
var
  I, KeyLen: Integer;
  C: Char;
  B: Byte;
  Utf8Result, Utf8Key: UTF8String;
begin
  Result := '';
  Utf8Key := UTF8Encode(Key);
  KeyLen := Length(Utf8Key);
  SetLength(Utf8Result, Length(Source) div 2);
  for I := 0 to Length(Source) div 2 - 1 do
  begin
    // HexToInt
    C := Source[1 + I * 2];
    case C of
      '0'..'9': B := Ord(C) - Ord('0');
      'A'..'F': B := Ord(C) - 55;
      'a'..'f': B := Ord(C) - 87;
    else
      B := Ord(' ');
    end;
    B := B shl 4;
    C := Source[1 + I * 2 + 1];
    case C of
      '0'..'9': B := B or (Ord(C) - Ord('0'));
      'A'..'F': B := B or (Ord(C) - 55);
      'a'..'f': B := B or (Ord(C) - 87);
    else
      B := Ord(' ');
    end;
    if KeyLen > 0 then
      B := B xor Byte(Utf8Key[1 + (I mod KeyLen)]);
    Utf8Result[1 + I] := AnsiChar(B);
  end;
  Result := UTF8ToString(Utf8Result);
end;

function GetCmdLineArg(const Switch: string; ASwitchChars: TSysCharSet): string;
var
  I: Integer;
  S: string;
begin
  I := 1;
  while I <= ParamCount do
  begin
    S := ParamStr(I);
    if (ASwitchChars = []) or ((Length(S) > 1) and CharInSet(S[1], ASwitchChars)) then
    begin
      if AnsiSameText(Copy(S, 2, MaxInt), Switch) then
      begin
        Inc(I);
        if I <= ParamCount then
        begin
          Result := ParamStr(I);
          Exit;
        end;
      end;
    end;
    Inc(I);
  end;
  Result := '';
end;

{ begin JvStrUtil }

function FindNotBlankCharPos(const S: string): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] <> ' ' then
      Exit;
  Result := Length(S) + 1;
end;

function FindNotBlankCharPosW(const S: WideString): Integer;
begin
  for Result := 1 to Length(S) do
    if S[Result] <> ' ' then
      Exit;
  Result := Length(S) + 1;
end;

// (rom) reimplemented

function AnsiChangeCase(const S: string): string;
var
  I: Integer;
  Up: string;
  Down: string;
begin
  Result := S;
  Up := AnsiUpperCase(S);
  Down := AnsiLowerCase(S);
  for I := 1 to Length(Result) do
    if Result[I] = Up[I] then
      Result[I] := Down[I]
    else
      Result[I] := Up[I];
end;

function WideChangeCase(const S: string): string;
var
  I: Integer;
  Up: string;
  Down: string;
begin
  Result := S;
  Up := WideUpperCase(S);
  Down := WideLowerCase(S);
  for I := 1 to Length(Result) do
    if Result[I] = Up[I] then
      Result[I] := Down[I]
    else
      Result[I] := Up[I];
end;

{ end JvStrUtil }
{ end JvStrUtils }

{ begin JvFileUtil }

function NormalDir(const DirName: string): string;
begin
  Result := DirName;
  {$IFDEF MSWINDOWS}
  if (Result <> '') and
    not CharInSet(AnsiLastChar(Result)^, [':', '\'])
  then
    if (Length(Result) = 1) and CharInSet(Result[1], ['A'..'Z', 'a'..'z']) then
      Result := Result + ':\'
    else
      Result := Result + '\';
  {$ENDIF MSWINDOWS}
end;

function RemoveBackSlash(const DirName: string): string;
begin
  Result := DirName;
  if (Length(Result) > 1) and
    (AnsiLastChar(Result)^ = '\')
  then
    if not ((Length(Result) = 3) and CharInSet(Result[1], ['A'..'Z', 'a'..'z']) and
      (Result[2] = ':')) then
      Delete(Result, Length(Result), 1);
end;

function HasAttr(const FileName: string; Attr: Integer): Boolean;
var
  FileAttr: Integer;
begin
  FileAttr := FileGetAttr(FileName);
  Result := (FileAttr >= 0) and (FileAttr and Attr = Attr);
end;

function DeleteFilesEx(const FileMasks: array of string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(FileMasks) to High(FileMasks) do
    Result := Result and DeleteFiles(ExtractFilePath(FileMasks[I]), ExtractFileName(FileMasks[I]));
end;

{$IFDEF MSWINDOWS}

function GetWindowsDir: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  SetString(Result, Buffer, GetWindowsDirectory(Buffer, SizeOf(Buffer)));
end;

function GetSystemDir: string;
var
  Buffer: array [0..MAX_PATH] of Char;
begin
  SetString(Result, Buffer, GetSystemDirectory(Buffer, SizeOf(Buffer)));
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function GetTempFileName(const Prefix: AnsiString): AnsiString;
var
  P: PAnsiChar;
begin
  P := tempnam(nil, Pointer(Prefix));
  Result := P;
  if P <> nil then
    Libc.free(P);
end;
{$ENDIF UNIX}

function GenTempFileName(FileName: string): string;
var
  TempDir: string;
  {$IFDEF MSWINDOWS}
  TempFile: array [0..MAX_PATH] of Char;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  TempFile: string;
  {$ENDIF UNIX}
  STempDir: TFileName;
  Res: Integer;
begin
  TempDir := PathGetTempPath;
  if FileName <> '' then
  begin
    if Length(FileName) < 4 then
      FileName := ExpandFileName(FileName);
    if (Length(FileName) > 4) and (FileName[2] = ':') and
      (Length(TempDir) > 4) and
      (AnsiCompareFileName(TempDir, FileName) <> 0) then
    begin
      STempDir := ExtractFilePath(FileName);
      MoveString(STempDir, TempDir, Length(STempDir) + 1);
    end;
  end;
  {$IFDEF MSWINDOWS}
  Res := GetTempFileName(
    PChar(TempDir), { address of directory name for temporary file}
    '~JV', { address of filename prefix}
    0, { number used to create temporary filename}
    TempFile); { address of buffer that receives the new filename}
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  TempFile := GetTempFileName('~JV');
  Res := 1;
  {$ENDIF UNIX}
  if Res <> 0 then
    Result := TempFile
  else
    Result := '~JVCLTemp.tmp';
  DeleteFile(Result);
end;

function GenTempFileNameExt(FileName: string; const FileExt: string): string;
begin
  Result := ChangeFileExt(GenTempFileName(FileName), FileExt);
end;

function ClearDir(const Dir: string): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path: TFileName;
begin
  Result := True;
  Path := AddSlash(Dir);
  DosError := FindFirst(Path + AllFilesMask, faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Result := Result and DeleteDir(Path + SearchRec.Name)
      else
        Result := Result and DeleteFile(Path + SearchRec.Name);
      // if not Result then Exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function DeleteDir(const Dir: string): Boolean;
begin
  ClearDir(Dir);
  Result := RemoveDir(Dir);
end;

function DeleteFiles(const Folder: TFileName; const Masks: string): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path: TFileName;
begin
  Result := False;
  Path := AddSlash(Folder);
  DosError := FindFirst(Path + AllFilesMask, faAnyFile and not faDirectory, SearchRec);
  while DosError = 0 do
  begin
    if FileEquMasks(Path + SearchRec.Name, Masks) then
      Result := DeleteFile(Path + SearchRec.Name);
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

function GetParameter: string;
var
  FN, FN1: PChar;
begin
  if ParamCount = 0 then
  begin
    Result := '';
    Exit
  end;
  FN := CmdLine;
  if FN[0] = '"' then
  begin
    FN := StrScan(FN + 1, '"');
    if (FN[0] = #0) or (FN[1] = #0) then
      Result := ''
    else
    begin
      Inc(FN, 2);
      if FN[0] = '"' then
      begin
        Inc(FN, 1);
        FN1 := StrScan(FN + 1, '"');
        if FN1[0] <> #0 then
          FN1[0] := #0;
      end;
      Result := FN;
    end;
  end
  else
    Result := Copy(CmdLine, Length(ParamStr(0)) + 1, 260);
  while (Length(Result) > 0) and (Result[1] = ' ') do
    Delete(Result, 1, 1);
  Result := ReplaceString(Result, '"', '');
  if FileExists(Result) then
    Result := GetLongFileName(Result);
end;

function GetLongFileName(const FileName: string): string;
{$IFDEF MSWINDOWS}
var
  SearchRec: TSearchRec;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  if FileGetInfo(FileName, SearchRec) then
    Result := ExtractFilePath(ExpandFileName(FileName)) + SearchRec.FindData.cFileName
  else
    Result := FileName;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := ExpandFileName(FileName);
  {$ENDIF UNIX}
end;

function FileEquMask(FileName, Mask: TFileName; CaseSensitive: Boolean): Boolean;
var
  I: Integer;
  C: Char;
  Index: Integer;
  LenFileName: Integer;
begin
  if not CaseSensitive then
  begin
    FileName := AnsiUpperCase(ExtractFileName(FileName));
    Mask := AnsiUpperCase(Mask);
  end;
  Result := False;
  {$IFDEF MSWINDOWS}
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';
  {$ENDIF MSWINDOWS}
  LenFileName := Length(FileName);
  I := 1;
  Index := 1;
  while I <= Length(Mask) do
  begin
    C := Mask[I];
    if (Index > LenFileName) and (C <> '*') then
      Exit;
    case C of
      '*':
        if I = Length(Mask) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          Index := PosIdx(Mask[I + 1], FileName, Index);
          if Index = 0 then
            Exit;
        end;
      '?':
        Inc(Index);
    else
      if C = FileName[Index] then
        Inc(Index)
      else
        Exit;
    end;
    Inc(I);
  end;
  if Index > LenFileName then
    Result := True;
end;

function FileEquMasks(FileName, Masks: TFileName; CaseSensitive: Boolean): Boolean;
var
  I: Integer;
  Mask: string;
begin
  Result := False;
  I := 0;
  Mask := Trim(SubStrBySeparator(Masks, I, PathSep));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask, CaseSensitive) then
    begin
      Result := True;
      Break;
    end
    else
    begin
      Inc(I);
      Mask := Trim(SubStrBySeparator(Masks, I, PathSep));
    end;
end;

function ValidFileName(const FileName: string): Boolean;

  function HasAny(const Str, SubStr: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(SubStr) do
    begin
      if Pos(SubStr[I], Str) > 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  Result := (FileName <> '') and
  {$IFDEF MSWINDOWS}
  (not HasAny(FileName, '/<>"?*|'));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  (not HasAny(FileName, '<>"?*|'));
  {$ENDIF UNIX}
  if Result then
    Result := Pos(PathDelim, ExtractFileName(FileName)) = 0;
end;

{$IFDEF MSWINDOWS}

function FileLock(Handle: Integer; Offset, LockSize: Longint): Integer; overload;
begin
  if LockFile(Handle, Offset, 0, LockSize, 0) then
    Result := 0
  else
    Result := GetLastError;
end;

function FileUnlock(Handle: Integer; Offset, LockSize: Longint): Integer; overload;
begin
  if UnlockFile(Handle, Offset, 0, LockSize, 0) then
    Result := 0
  else
    Result := GetLastError;
end;

function FileLock(Handle: Integer; Offset, LockSize: Int64): Integer; overload;
begin
  if LockFile(Handle, Int64Rec(Offset).Lo, Int64Rec(Offset).Hi,
    Int64Rec(LockSize).Lo, Int64Rec(LockSize).Hi) then
    Result := 0
  else
    Result := GetLastError;
end;

function FileUnlock(Handle: Integer; Offset, LockSize: Int64): Integer; overload;
begin
  if UnlockFile(Handle, Int64Rec(Offset).Lo, Int64Rec(Offset).Hi,
    Int64Rec(LockSize).Lo, Int64Rec(LockSize).Hi) then
    Result := 0
  else
    Result := GetLastError;
end;

{$ENDIF MSWINDOWS}

function ShortToLongFileName(const ShortName: string): string;
{$IFDEF MSWINDOWS}
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
begin
  SearchHandle := FindFirstFile(PChar(ShortName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := Temp.cFileName;
    if Result = '' then
      Result := Temp.cAlternateFileName;
  end
  else
    Result := '';
  Windows.FindClose(SearchHandle);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if FileExists(ShortName) then
    Result := ShortName
  else
    Result := '';
end;
{$ENDIF UNIX}

function LongToShortFileName(const LongName: string): string;
{$IFDEF MSWINDOWS}
var
  Temp: TWin32FindData;
  SearchHandle: THandle;
begin
  SearchHandle := FindFirstFile(PChar(LongName), Temp);
  if SearchHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := Temp.cAlternateFileName;
    if Result = '' then
      Result := Temp.cFileName;
  end
  else
    Result := '';
  Windows.FindClose(SearchHandle);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  if FileExists(LongName) then
    Result := LongName
  else
    Result := '';
end;
{$ENDIF UNIX}

function ShortToLongPath(const ShortName: string): string;
var
  LastSlash: PChar;
  TempPathPtr: PChar;
begin
  Result := '';
  TempPathPtr := StrNew(PChar(ShortName));
  try
    LastSlash := StrRScan(TempPathPtr, PathDelim);
    while LastSlash <> nil do
    begin
      Result := PathDelim + ShortToLongFileName(TempPathPtr) + Result;
      if LastSlash <> nil then
      begin
        LastSlash^ := #0;
        LastSlash := StrRScan(TempPathPtr, PathDelim);
      end;
    end;
    Result := TempPathPtr + Result;
  finally
    StrDispose(TempPathPtr);
  end;
end;

function LongToShortPath(const LongName: string): string;
var
  LastSlash: PChar;
  TempPathPtr: PChar;
begin
  Result := '';
  TempPathPtr := StrNew(PChar(LongName));
  try
    LastSlash := StrRScan(TempPathPtr, PathDelim);
    while LastSlash <> nil do
    begin
      Result := PathDelim + LongToShortFileName(TempPathPtr) + Result;
      if LastSlash <> nil then
      begin
        LastSlash^ := #0;
        LastSlash := StrRScan(TempPathPtr, PathDelim);
      end;
    end;
    Result := TempPathPtr + Result;
  finally
    StrDispose(TempPathPtr);
  end;
end;

{$IFDEF MSWINDOWS}

const
  IID_IPersistFile: TGUID =
  (D1: $0000010B; D2: $0000; D3: $0000; D4: ($C0, $00, $00, $00, $00, $00, $00, $46));

const
  LinkExt = '.lnk';

procedure CreateFileLink(const FileName, DisplayName: string; Folder: Integer);
var
  ShellLink: IShellLink;
  PersistFile: IPersistFile;
  ItemIDList: PItemIDList;
  FileDestPath: array [0..MAX_PATH] of Char;
  {$IFNDEF UNICODE}
  FileNameW: array [0..MAX_PATH] of WideChar;
  {$ENDIF ~UNICODE}
begin
  CoInitialize(nil);
  try
    OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_SERVER,
      IID_IShellLinkA, ShellLink));
    try
      OleCheck(ShellLink.QueryInterface(IID_IPersistFile, PersistFile));
      try
        OleCheck(SHGetSpecialFolderLocation(0, Folder, ItemIDList));
        SHGetPathFromIDList(ItemIDList, FileDestPath);
        StrCat(FileDestPath, PChar('\' + DisplayName + LinkExt));
        ShellLink.SetPath(PChar(FileName));
        ShellLink.SetIconLocation(PChar(FileName), 0);
        {$IFDEF UNICODE}
        OleCheck(PersistFile.Save(FileDestPath, True));
        {$ELSE}
        MultiByteToWideChar(CP_ACP, 0, FileDestPath, -1, FileNameW, MAX_PATH);
        OleCheck(PersistFile.Save(FileNameW, True));
        {$ENDIF UNICODE}
      finally
        PersistFile := nil;
      end;
    finally
      ShellLink := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure DeleteFileLink(const DisplayName: string; Folder: Integer);
var
  ShellLink: IShellLink;
  ItemIDList: PItemIDList;
  FileDestPath: array [0..MAX_PATH] of Char;
begin
  CoInitialize(nil);
  try
    OleCheck(CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_SERVER,
      IID_IShellLinkA, ShellLink));
    try
      OleCheck(SHGetSpecialFolderLocation(0, Folder, ItemIDList));
      SHGetPathFromIDList(ItemIDList, FileDestPath);
      StrCat(FileDestPath, PChar('\' + DisplayName + LinkExt));
      DeleteFile(FileDestPath);
    finally
      ShellLink := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

{$ENDIF MSWINDOWS}

{ end JvFileUtil }

function PtInRectInclusive(R: TRect; Pt: TPoint): Boolean;
begin
  R.Right := R.Right + 1;
  R.Bottom := R.Bottom + 1;
  Result := PtInRect(R, Pt);
end;

function PtInRectExclusive(R: TRect; Pt: TPoint): Boolean;
begin
  R.Left := R.Left + 1;
  R.Top := R.Top + 1;
  Result := PtInRect(R, Pt);
end;

function OpenObject(const Value: string): Boolean; overload;
begin
  Result := OpenObject(PChar(Value));
end;

{ (rb) Duplicate of JvFunctions.Exec }
function OpenObject(Value: PChar): Boolean; overload;
begin
  Result := ShellExecute(0, 'open', Value, nil, nil, SW_SHOWNORMAL) > HINSTANCE_ERROR;
end;

{$IFDEF MSWINDOWS}

procedure RaiseLastWin32; overload;
begin
  PError('');
end;

procedure RaiseLastWin32(const Text: string); overload;
begin
  PError(Text);
end;

function GetFileVersion(const AFileName: string): Cardinal;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf: Pointer;
  FI: PVSFixedFileInfo;
  VerSize: DWORD;
begin
  Result := 0;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  FileName := AFileName;
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\', Pointer(FI), VerSize) then
          Result := FI.dwFileVersionMS;
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

var
  ShellVersion: Integer;

function GetShellVersion: Cardinal;
begin
  if ShellVersion = 0 then
    ShellVersion := GetFileVersion('shell32.dll');
  Result := ShellVersion;
end;

procedure OpenCdDrive;
begin
  mciSendString(PChar(RC_OpenCDDrive), nil, 0, Windows.GetForegroundWindow);
end;

procedure CloseCdDrive;
begin
  mciSendString(PChar(RC_CloseCDDrive), nil, 0, Windows.GetForegroundWindow);
end;

{ (rb) Duplicate of JclFileUtils.DiskInDrive }

function DiskInDrive(Drive: Char): Boolean;
var
  DrvNum: Byte;
  EMode: Word;
begin
  DrvNum := Ord(Drive);
  if DrvNum >= Ord('a') then
    Dec(DrvNum, $20);
  EMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := DiskSize(DrvNum - $40) <> -1;
  finally
    SetErrorMode(EMode);
  end;
end;

{$ENDIF MSWINDOWS}

procedure PError(const Text: string);
var
  LastError: Integer;
  St: string;
begin
  LastError := GetLastError;
  if LastError <> 0 then
  begin
    St := SysUtils.Format(SOSError, [LastError, SysErrorMessage(LastError)]);
    if Text <> '' then
      St := Text + ':' + St;
    raise EOSError.Create(St);
  end;
end;

procedure Exec(const FileName, Parameters, Directory: string);
begin
  {$IFDEF MSWINDOWS}
  ShellExecute(Windows.GetForegroundWindow, 'open', PChar(FileName), PChar(Parameters), PChar(Directory),
    SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ShellExecute(GetForegroundWindow, 'open', PChar(FileName), PChar(Parameters), PChar(Directory),
    SW_SHOWNORMAL);
  {$ENDIF UNIX}
end;
{$IFDEF UNIX}
// begin
//  if Directory = '' then Directory := GetCurrentDir;
//  Libc.system(PChar(Format('cd "%s" ; "%s" %s &', [Directory, FileName, Parameters])));
// end;
{$ENDIF UNIX}

{ (rb) Duplicate of JclMiscel.WinExec32AndWait }

function ExecuteAndWait(CommandLine: string; const WorkingDirectory: string; Visibility: Integer): Integer;
{$IFDEF MSWINDOWS}
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Result := 0;
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  UniqueString(CommandLine);//in the Unicode version the parameter lpCommandLine needs to be writable
  if not CreateProcess(nil, PChar(CommandLine), nil, nil, False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
    nil, Pointer(WorkingDirectory), StartupInfo, ProcessInfo) then
  begin
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);

    // required to avoid running resource leak.
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end
  else
  begin
    Result := GetLastError;
  end;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  // ignores Visibility
  { TODO : Untested }
  if Libc.system(PChar(Format('kfmclient exec "%s"', [CommandLine]))) = -1 then
  begin
    if WorkingDirectory = '' then
      Result := Libc.system(PChar(Format('cd "%s" ; %s',
        [GetCurrentDir, CommandLine])))
    else
      Result := Libc.system(PChar(Format('cd "%s" ; %s',
        [WorkingDirectory, CommandLine])));
  end;
end;
{$ENDIF UNIX}


function FirstInstance(const ATitle: string): Boolean;
var
  Mutex: THandle;
begin
  Mutex := CreateMutex(nil, False, PChar(ATitle));
  try
    Result := (Mutex <> 0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  finally
    ReleaseMutex(Mutex);
  end;
end;

procedure RestoreOtherInstance(const MainFormClassName, MainFormCaption: string);
var
  OtherWnd, OwnerWnd: HWND;
begin
  OtherWnd := FindWindow(PChar(MainFormClassName), PChar(MainFormCaption));
  ShowWindow(OtherWnd, SW_SHOW); //in case the window was not visible before

  OwnerWnd := 0;
  if OtherWnd <> 0 then
    OwnerWnd := GetWindow(OtherWnd, GW_OWNER);

  if OwnerWnd <> 0 then
    OtherWnd := OwnerWnd;

  if OtherWnd <> 0 then
  begin
    { (rb) Use JvVCLUtils.SwitchToWindow }
    if IsIconic(OtherWnd) then
      ShowWindow(OtherWnd, SW_RESTORE);

    SetForegroundWindow(OtherWnd);
  end;
end;

procedure HideTraybar;
begin
  ShowWindow(FindWindow(PChar(RC_ShellName), nil), SW_HIDE);
end;

procedure ShowTraybar;
begin
  ShowWindow(FindWindow(PChar(RC_ShellName), nil), SW_SHOW);
end;

procedure ShowStartButton(Visible: Boolean);
var
  Tray, Child: HWND;
  C: array [0..127] of Char;
  S: string;
begin
  Tray := FindWindow(PChar(RC_ShellName), nil);
  Child := GetWindow(Tray, GW_CHILD);
  while Child <> 0 do
  begin
    if GetClassName(Child, C, SizeOf(C)) > 0 then
    begin
      S := StrPas(C);
      if UpperCase(S) = 'BUTTON' then
        if Visible then
          ShowWindow(Child, SW_SHOWNORMAL)
        else
          ShowWindow(Child, SW_HIDE);
    end;
    Child := GetWindow(Child, GW_HWNDNEXT);
  end;
end;

procedure MonitorOn;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
end;

procedure MonitorOff;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 2);
end;

procedure LowPower;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MONITORPOWER, 1);
end;

procedure SendShift(H: THandle; Down: Boolean);
var
  VKey: Word;
  ScanCode: DWORD;
begin
  VKey := VK_SHIFT;
  ScanCode := Longint(MapVirtualKey(VKey, 0)) shl 16 or 1;
  if not Down then
    ScanCode := ScanCode or $C0000000;
  SendMessage(H, WM_KEYDOWN, VKey, LPARAM(ScanCode));
end;

procedure SendCtrl(H: THandle; Down: Boolean);
var
  VKey: Word;
  ScanCode: DWORD;
begin
  VKey := VK_CONTROL;
  ScanCode := Longint(MapVirtualKey(VKey, 0)) shl 16 or 1;
  if not Down then
    ScanCode := ScanCode or $C0000000;
  SendMessage(H, WM_KEYDOWN, VKey, LPARAM(ScanCode));
end;

function SendKey(const AppName: string; Key: Char): Boolean;
var
  VKey: Word;
  ConvKey: Longint;
  ScanCode: DWORD;
  Shift, Ctrl: Boolean;
  H: Windows.HWND;
begin
  H := FindWindow(PChar(AppName), nil);
  if H <> 0 then
  begin
    ConvKey := OemKeyScan(Ord(Key));
    Shift := (ConvKey and $00020000) <> 0;
    Ctrl := (ConvKey and $00040000) <> 0;
    ScanCode := ConvKey and $000000FF or $FF00;
    VKey := Ord(Key);
    ScanCode := (ScanCode shl 16) or 1;
    if Shift then
      SendShift(H, True);
    if Ctrl then
      SendCtrl(H, True);
    SendMessage(H, WM_KEYDOWN, VKey, LPARAM(ScanCode));
    SendMessage(H, WM_CHAR, VKey, LPARAM(ScanCode));
    ScanCode := ScanCode or $C0000000;
    SendMessage(H, WM_KEYUP, VKey, LPARAM(ScanCode));
    if Shift then
      SendShift(H, False);
    if Ctrl then
      SendCtrl(H, False);
    Result := True;
  end
  else
    Result := False;
end;



{$IFDEF MSWINDOWS}

procedure RebuildIconCache;
var
  Dummy: DWORD;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    LPARAM(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, {$IFDEF RTL230_UP}@{$ENDIF}Dummy);
end;

procedure AssociateFileExtension(const IconPath, ProgramName, Path, Extension: string);
begin
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey(ProgramName, True);
    WriteString('', ProgramName);
    if IconPath <> '' then
    begin
      OpenKey(RC_DefaultIcon, True);
      WriteString('', IconPath);
    end;
    CloseKey;
    OpenKey(ProgramName, True);
    OpenKey('shell', True);
    OpenKey('open', True);
    OpenKey('command', True);
    WriteString('', '"' + Path + '" "%1"');
    Free;
  end;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('.' + Extension, True);
    WriteString('', ProgramName);
    Free;
  end;
  RebuildIconCache;
end;

procedure AssociateExtension(const IconPath, ProgramName, Path, Extension: string);
begin
  AssociateFileExtension(IconPath, ProgramName, Path, Extension);
end;

function GetRecentDocs: TStringList;

var
  Path: string;
  T: TSearchRec;
  Res: Integer;

begin
  Result := TStringList.Create;
  Path := IncludeTrailingPathDelimiter(GetRecentFolder);
  //search for all files
  Res := FindFirst(Path + '*.*', faAnyFile, T);
  try
    while Res = 0 do
    begin
      if (T.Name <> '.') and (T.Name <> '..') then
        Result.Add(Path + T.Name);
      Res := FindNext(T);
    end;
  finally
    FindClose(T);
  end;
end;

{ (rb) Duplicate of JvWinDialogs.AddToRecentDocs }

procedure AddToRecentDocs(const FileName: string);
begin
  SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
end;

function EnumWindowsProc(Handle: THandle; LParam: TStrings): Boolean; stdcall;
var
  St: array [0..256] of Char;
  St2: string;
begin
  if Windows.IsWindowVisible(Handle) then
  begin
    GetWindowText(Handle, St, SizeOf(St));
    St2 := St;
    if St2 <> '' then
      with TStrings(LParam) do
        AddObject(St2, TObject(Handle));
  end;
  Result := True;
end;

procedure GetVisibleWindows(List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    EnumWindows(@EnumWindowsProc, LPARAM(List));
  finally
    List.EndUpdate;
  end;
end;

{$ENDIF MSWINDOWS}
// from JvComponentFunctions

function StrPosNoCase(const psSub, psMain: string): Integer;
begin
  Result := Pos(AnsiUpperCase(psSub), AnsiUpperCase(psMain));
end;

function StrRestOf(const Ps: string; const N: Integer): string;
begin
  Result := Copy(Ps, N, {(Length(Ps) - N + 1)} MaxInt);
end;

{!!!!!!!! use these because the JCL one is badly broken }

{ I am using this one purely as an internal for StrReplace

 Replaces parts of a string with new text. iUpdatePos is the last update position
 i.e. the position where substr was found + the length of the replacement string + 1.
 Use 0 first time in }

function StrReplaceInstance(const psSource, psSearch, psReplace: string;
  var piUpdatePos: Integer; const pbCaseSens: Boolean): string;
var
  liIndex: Integer;
  lsCopy: string;
begin
  Result := psSource;
  if piUpdatePos >= Length(psSource) then
    Exit;
  if psSearch = '' then
    Exit;

  Result := Copy(psSource, 1, piUpdatePos - 1);
  lsCopy := StrRestOf(psSource, piUpdatePos);

  if pbCaseSens then
    liIndex := Pos(psSearch, lsCopy)
  else
    liIndex := StrPosNoCase(psSearch, lsCopy);
  if liIndex = 0 then
  begin
    Result := psSource;
    piUpdatePos := Length(psSource) + 1;
    Exit;
  end;

  Result := Result + Copy(lsCopy, 1, liIndex - 1);
  Result := Result + psReplace;
  piUpdatePos := Length(Result) + 1;
  Result := Result + StrRestOf(lsCopy, liIndex + Length(psSearch));
end;

function LStrReplace(const psSource, psSearch, psReplace: string;
  const pbCaseSens: Boolean): string;
var
  liUpdatePos: Integer;
begin
  liUpdatePos := 0;
  Result := psSource;
  while liUpdatePos < Length(Result) do
    Result := StrReplaceInstance(Result, psSearch, psReplace, liUpdatePos, pbCaseSens);
end;

{ if it's not a decimal point then it must be a digit, space or Currency symbol
  also always use $ for money }

function CharIsMoney(const Ch: Char): Boolean;
begin
  Result := CharIsDigit(Ch) or (Ch = NativeSpace) or (Ch = '$') or (Ch = '-') or
    (Pos(Ch, JclFormatSettings.CurrencyString) > 0);
end;

function StrToCurrDef(const Str: string; Def: Currency): Currency;
var
  LStr: TJclStringBuilder;
  I: Integer;
  CharSet: TSysCharSet;
begin
  if Str = '' then
    Result := Def
  else
  begin
    LStr := TJclStringBuilder.Create(Length(Str));
    try
      CharSet := ['0'..'9', '-', '+', AnsiChar(JclFormatSettings.DecimalSeparator)];
      for I := 1 to Length(Str) do
        if CharInSet(Str[I], CharSet) then
          LStr.Append(Str[I]);
      try
        if not TextToFloat(PChar(LStr.ToString), Result, fvCurrency) then
          Result := Def;
      except
        Result := Def;
      end;
    finally
      LStr.Free;
    end;
  end;
end;

{ JvStrConvertErrorFmt used from JvSafeStrToFloat }
procedure JvStrConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EJvConvertError.CreateResFmt(ResString, Args); { will be also caught if you catch E:EConvertERror }
end;

{$IFNDEF RTL150_UP}
function TextToFloatD5D6(Buffer: PAnsiChar; var Value; ValueType: TFloatValue;
  const FormatSettings: TFormatSettings): Boolean;
var
  DecimalSep: Char;
begin
  { not threadsafe }
  DecimalSep := DecimalSeparator;
  try
    DecimalSeparator := FormatSettings.DecimalSeparator;
    Result := TextToFloat(Buffer, Value, ValueType);
  finally
    DecimalSeparator := DecimalSep;
  end;
end;
{$ENDIF ~RTL150_UP}


{ _JvSafeStrToFloat:  [PRIVATE INTERNAL FUNCTION]

     [ not to be called outside this unit, see below for public api ]

    This is a refactored version of the internal guts of the former routine
    StrToFloatDefIgnoreInvalidCharacters with some improvements made to decimal
    separator handling.
}
function _JvSafeStrToFloat(const Str: string; aDecimalSeparator: Char; var OutValue: Extended): Boolean;
var
  LStr: TJclStringBuilder;
  I: Integer;
  CharSet: TSysCharSet;
  LocalFormatSettings: TFormatSettings;
begin
  Result := false;
  if Str = '' then
    Exit; { hows this for a nice optimization?  WPostma. }

  { Locale Handling logic October 2008 supercedes former StrToFloatUS functionality. }
  {$IFDEF RTL150_UP}
  LocalFormatSettings.ThousandSeparator := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_STHOUSAND, '.');
  LocalFormatSettings.DecimalSeparator := GetLocaleChar(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL, '.');
  {$ELSE}
  LocalFormatSettings.DecimalSeparator := DecimalSeparator;
  {$ENDIF RTL150_UP}
  if aDecimalSeparator = ' ' then {magic mode}
    aDecimalSeparator := LocalFormatSettings.DecimalSeparator { default case! use system defaults! }
  else
    LocalFormatSettings.DecimalSeparator := aDecimalSeparator; { custom format specified! }

  { Cross-codepage safety feature:  Handed '1.2', a string without a comma,
    but which is obviously a floating point number, convert it properly also.
    This functionality is important for JvCsvDataSet and may be important in other
    places. }
  if (Pos(USDecimalSeparator, Str) > 0) and (Pos(aDecimalSeparator, Str) = 0) then
  begin
    aDecimalSeparator := USDecimalSeparator; { automatically works when US decimal values are encountered }
    LocalFormatSettings.DecimalSeparator := aDecimalSeparator; { custom format specified! }
  end;

  LStr := TJclStringBuilder.Create(Length(Str));
  try
    CharSet := ['0'..'9', '-', '+', 'e', 'E', AnsiChar(aDecimalSeparator)];
    //if (aDecimalSeparator<>USDecimalSeparator) then
    //    CharSet := CharSet + [USDecimalSeparator]; { we allow US Decimal separators, even when it's not the regional setting, we just grandfather it in as valid }

    for I := 1 to Length(Str) do
      if CharInSet(Str[I], CharSet) then
        LStr.Append(Str[I]);


    if LStr.Length > 0 then
    try
      { the string '-' fails StrToFloat, but it can be interpreted as 0  }
      if LStr[LStr.Length - 1] = '-' then
        LStr.Append('0');

      { a string that ends in a '.' such as '12.' fails StrToFloat,
       but as far as I am concerned, it may as well be interpreted as 12.0 }
      if LStr[LStr.Length - 1] = aDecimalSeparator then
        LStr.Append('0');

      {$IFDEF RTL150_UP}
      if not TextToFloat(PChar(LStr.ToString), OutValue, fvExtended, LocalFormatSettings) then
      {$ELSE}
      if not TextToFloatD5D6(PChar(LStr.ToString), OutValue, fvExtended, LocalFormatSettings) then
      {$ENDIF RTL150_UP}
        Result := False
      else
        Result := True; { success! }

    except
      Result := False;
    end;
  finally
    LStr.Free;
  end;
end;

// JvSafeStrToFloatDef:
//
// Note: before using StrToFloatDef, please be aware that it will ignore
// any character that is not a valid character for a float, which is different
// from what the one in Delphi 6 up is doing. This has been documented in Mantis
// issue# 2935: http://homepages.borland.com/jedi/issuetracker/view.php?id=2935
//
// This function was extended by WPostma, to allow specification of custom decimal
// separators. This was required by JvCsvDataSet and may be required elsewhere in the
// VCL wherever custom (fixed) non-current-region-settings floating point value
// encoding must be supported.  We renamed this from StrToFloatDefIgnoreInvalidCharacters
// to JvSafeStrToFloatDef because it has multiple "floating point runtime exception safety"
// enhancements.
function JvSafeStrToFloatDef(const Str: string; Def: Extended; aDecimalSeparator: Char): Extended;
begin
  { one handy dandy api expects a Default value returned instead }
  if not _JvSafeStrToFloat(Str, aDecimalSeparator, Result) then
    Result := Def; { failed, use default }
end;

// New routine, same as JvSafeStrToFloatDef but it will raise a conversion exception,
// for cases when you actually want to handle an EConvertError yourself and where
// there is no convenient or possible float value for your case.
function JvSafeStrToFloat(const Str: string; aDecimalSeparator: Char): Extended;
begin
  { the other handy dandy api style expects us to raise an EConvertError. }
  if not _JvSafeStrToFloat(Str, aDecimalSeparator, Result) then
    JvStrConvertErrorFmt(@SInvalidFloat, [Str]); {failed, raise exception }
end;

function IntToExtended(I: Integer): Extended;
begin
  Result := I;
end;

function GetChangedText(const Text: string; SelStart, SelLength: Integer; Key: Char): string;
begin
  { take the original text, replace what will be overwritten with new value }
  Result := Text;

  if SelLength > 0 then
    Delete(Result, SelStart + 1, SelLength);
  if Key <> #0 then
    Insert(Key, Result, SelStart + 1);
end;

{ "window" technique for years to translate 2 digits to 4 digits.
   The window is 100 years wide
   The pivot year is the lower edge of the window
  A pivot year of 1900 is equivalent to putting 1900 before every 2-digit year
 if pivot is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
 The system default is 1950

 Why the reimplementation?
 JclDatetime.Make4DigitYear will fail after 2100, this won't
 note that in this implementation pivot is a 4-digit year
 I have made it accept JclDatetime.Make4DigitYear's 2 digit pivot years.
 They are expanded by adding 1900.

 It is also better in that a valid 4-digit year will pass through unchanged,
 not fail an assertion.
}

function MakeYear4Digit(Year, Pivot: Integer): Integer;
var
  Century: Integer;
begin
  if Pivot < 0 then
    raise Exception.CreateRes(@RsEPivotLessThanZero);

  { map 100 to zero }
  if Year = 100 then
    Year := 0;
  if Pivot = 100 then
    Pivot := 0;

  // turn 2 digit pivot to 4 digit
  if Pivot < 100 then
    Pivot := Pivot + 1900;

  { turn 2 digit years to 4 digits }
  if (Year >= 0) and (Year < 100) then
  begin
    Century := (Pivot div 100) * 100;

    Result := Year + Century; // give the result the same century as the pivot
    if Result < Pivot then
      //  cannot be lower than the Pivot
      Result := Result + 100;
  end
  else
    Result := Year;
end;

function StrIsInteger(const S: string): Boolean;
var
  I: Integer;
  Ch: Char;
begin
  Result := S <> '';
  for I := 1 to Length(S) do
  begin
    Ch := Char(S[I]);
    if not CharIsNumberChar(Ch) or (Ch = JclFormatSettings.DecimalSeparator) then //Az
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function StrIsFloatMoney(const Ps: string): Boolean;
var
  I, liDots: Integer;
  Ch: Char;
begin
  Result := True;
  liDots := 0;

  for I := 1 to Length(Ps) do
  begin
    { allow digits, space, Currency symbol and one decimal dot }
    Ch := Ps[I];

    if Ch = JclFormatSettings.DecimalSeparator then
    begin
      Inc(liDots);
      if liDots > 1 then
      begin
        Result := False;
        Break;
      end;
    end
    else
    if not CharIsMoney(Ch) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function StrIsDateTime(const Ps: string): Boolean;
const
  MIN_DATE_TIME_LEN = 6; {2Jan02 }
  MAX_DATE_TIME_LEN = 30; { 30 chars or so in '12 December 1999 12:23:23:00' }
var
  I: Integer;
  Ch: Char;
  liColons, liSlashes, liSpaces, liDigits, liAlpha: Integer;
  lbDisqualify: Boolean;
begin
  if Length(Ps) < MIN_DATE_TIME_LEN then
  begin
    Result := False;
    Exit;
  end;

  if Length(Ps) > MAX_DATE_TIME_LEN then
  begin
    Result := False;
    Exit;
  end;

  lbDisqualify := False;
  liColons := 0;
  liSlashes := 0;
  liSpaces := 0;
  liDigits := 0;
  liAlpha := 0;

  for I := 1 to Length(Ps) do
  begin
    Ch := Ps[I];

    if Ch = ':' then
      Inc(liColons)
    else
    if Ch = NativeForwardSlash then
      Inc(liSlashes)
    else
    if CharIsSpace(Ch) then
      Inc(liSpaces)
    else
    if CharIsDigit(Ch) then
      Inc(liDigits)
    else
    if CharIsAlpha(Ch) then
      Inc(liAlpha)
    else
    begin
      // no wierd punctuation in dates!
      lbDisqualify := True;
      Break;
    end;
  end;

  Result := False;
  if not lbDisqualify then
    { a date must have colons and slashes and spaces, but not to many of each }
    if (liColons > 0) or (liSlashes > 0) or (liSpaces > 0) then
      { only 2 slashes in "dd/mm/yy" or 3 colons in "hh:mm:ss:ms" or 6 spaces "yy mm dd hh mm ss ms" }
      if (liSlashes <= 2) and (liColons <= 3) and (liSpaces <= 6) then
        { must have some digits (min 3 digits, eg in "2 jan 02", max 16 dgits in "01/10/2000 10:10:10:10"
        longest month name is 8 chars }
        if (liDigits >= 3) and (liDigits <= 16) and (liAlpha <= 10) then
          Result := True;

  { define in terms of results - if I can interpret it as a date, then I can }
  if Result then
    Result := (SafeStrToDateTime(PreformatDateString(Ps)) <> 0);
end;

function PreformatDateString(Ps: string): string;
var
  I: Integer;
begin
  { turn any month names to numbers }

  { use the StrReplace in stringfunctions -
  the one in JclStrings is badly broken and brings down the app }

  for I := JclFormatSettings.MonthNamesLowIndex to JclFormatSettings.MonthNamesHighIndex do
    Ps := LStrReplace(Ps, JclFormatSettings.LongMonthNames[I], IntToStr(I), False);

  { now that 'January' is gone, catch 'Jan' }
  for I := JclFormatSettings.MonthNamesLowIndex to JclFormatSettings.MonthNamesHighIndex do
    Ps := LStrReplace(Ps, JclFormatSettings.ShortMonthNames[I], IntToStr(I), False);

  { remove redundant spaces }
  Ps := LStrReplace(Ps, NativeSpace + NativeSpace, NativeSpace, False);

  Result := Ps;
end;

function BooleanToInteger(const B: Boolean): Integer;
begin
  Result := Ord(B);
end;

{ from my ConvertFunctions unit }

function StringToBoolean(const Ps: string): Boolean;
const
  TRUE_STRINGS: array [1..5] of string = ('True', 't', 'y', 'yes', '1');
var
  I: Integer;
begin
  Result := False;

  for I := Low(TRUE_STRINGS) to High(TRUE_STRINGS) do
    if AnsiSameText(Ps, TRUE_STRINGS[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function SafeStrToDateTime(const Ps: string): TDateTime;
begin
  try
    Result := StrToDateTime(PreformatDateString(Ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToDate(const Ps: string): TDateTime;
begin
  try
    Result := StrToDate(PreformatDateString(Ps));
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

function SafeStrToTime(const Ps: string): TDateTime;
begin
  try
    Result := StrToTime(Ps)
  except
    on E: EConvertError do
      Result := 0.0
  else
    raise;
  end;
end;

{!! from strFunctions }

function StrDeleteChars(const Ps: string; const piPos: Integer; const piCount: Integer): string;
begin
  Result := Copy(Ps, 1, piPos - 1) + StrRestOf(Ps, piPos + piCount);
end;

function StrDelete(const psSub, psMain: string): string;
var
  liPos: Integer;
begin
  Result := psMain;
  if psSub = '' then
    Exit;

  liPos := StrIPos(psSub, psMain);

  while liPos > 0 do
  begin
    Result := StrDeleteChars(Result, liPos, Length(psSub));
    liPos := StrIPos(psSub, Result);
  end;
end;

function TimeOnly(pcValue: TDateTime): TTime;
begin
  Result := Frac(pcValue);
end;

function DateOnly(pcValue: TDateTime): TDate;
begin
  Result := Trunc(pcValue);
end;

{ have to do this as it depends what the datekind of the control is}

function DateIsNull(const pdtValue: TDateTime; const pdtKind: TdtKind): Boolean;
begin
  Result := False;
  case pdtKind of
    dtkDateOnly:
      Result := pdtValue < 1; //if date only then anything less than 1 is considered null
    dtkTimeOnly:
      Result := Frac(pdtValue) = NullEquivalentDate; //if time only then anything without a remainder is null
    dtkDateTime:
      Result := pdtValue = NullEquivalentDate;
  end;
end;

function OSCheck(RetVal: Boolean): Boolean;
begin
  if not RetVal then
    RaiseLastOSError;
  Result := RetVal;
end;

function MinimizeFileName(const FileName: string; Canvas: TCanvas; MaxLen: Integer): string;
var
  R: TRect;
begin
  Result := FileName;
  R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
  UniqueString(Result);
  if DrawText(Canvas.Handle, PChar(Result), Length(Result), R,
       DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or
       DT_NOPREFIX) <= 0 then
    Result := FileName;
end;

function MinimizeText(const Text: string; Canvas: TCanvas;
  MaxWidth: Integer): string;
var
  I: Integer;
begin
  Result := Text;
  I := 1;
  while (I <= Length(Text)) and (Canvas.TextWidth(Result) > MaxWidth) do
  begin
    Inc(I);
    Result := Copy(Text, 1, Max(0, Length(Text) - I)) + '...';
  end;
end;

{$IFDEF MSWINDOWS}

function RunDLL32(const ModuleName, FuncName, CmdLine: string; WaitForCompletion: Boolean; CmdShow: Integer =
  SW_SHOWDEFAULT): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  S: string;
begin
  SI.cb := SizeOf(SI);
  GetStartupInfo(SI);
  SI.wShowWindow := CmdShow;
  S := SysUtils.Format('rundll32.exe %s,%s %s', [ModuleName, FuncName, CmdLine]);
  Result := CreateProcess(nil, PChar(S), nil, nil, False, 0, nil, nil, SI, PI);
  try
    if WaitForCompletion then
      Result := WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_FAILED;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

procedure RunDll32Internal(Wnd: THandle; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
var
  H: THandle;
  P: TRunDLL32Proc;
begin
  H := SafeLoadLibrary(DLLName, SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  if H <> 0 then
  begin
    try
      P := GetProcAddress(H, PChar(FuncName));
      if Assigned(P) then
        P(Wnd, H, PChar(CmdLine), CmdShow);
    finally
      FreeLibrary(H);
    end;
  end;
end;

type
  // (p3) from ShLwAPI
  TDLLVersionInfo = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
  end;

function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;
var
  hDLL, hr: THandle;
  pDllGetVersion: function(var Dvi: TDLLVersionInfo): Integer; stdcall;
  Dvi: TDLLVersionInfo;
begin
  hDLL := SafeLoadLibrary(DLLName);
  if hDLL <> 0 then
  begin
    Result := True;
    (*  You must get this function explicitly
        because earlier versions of the DLL's
        don't implement this function.
        That makes the lack of implementation
        of the function a version marker in itself.   *)
    @pDllGetVersion := GetProcAddress(hDLL, PChar('DllGetVersion'));
    if Assigned(pDllGetVersion) then
    begin
      FillChar(Dvi, SizeOf(Dvi), 0);
      Dvi.cbSize := SizeOf(Dvi);
      hr := pDllGetVersion(Dvi);
      if hr = 0 then
      begin
        pdwMajor := Dvi.dwMajorVersion;
        pdwMinor := Dvi.dwMinorVersion;
      end;
    end
    else (*   If GetProcAddress failed, the DLL is a version previous to the one  shipped with IE 3.x. *)
    begin
      pdwMajor := 4;
      pdwMinor := 0;
    end;
    FreeLibrary(hDLL);
    Exit;
  end;
  Result := False;
end;

{$ENDIF MSWINDOWS}
{from JvVCLUtils }

{ Exceptions }

procedure ResourceNotFound(ResID: PChar);
var
  S: string;
begin
  if DWORD_PTR(ResID) <= $FFFF then
    S := IntToStr(INT_PTR(ResID))
  else
    S := ResID;
  raise EResNotFound.CreateResFmt(@SResNotFound, [S]);
end;

function EmptyRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
end;

function RectWidth(R: TRect): Integer;
begin
  Result := Abs(R.Right - R.Left);
end;

function RectHeight(R: TRect): Integer;
begin
  Result := Abs(R.Bottom - R.Top);
end;

procedure RectNormalize(var R: TRect);
var
  Temp: Integer;
begin
  if R.Left > R.Right then
  begin
    Temp := R.Left;
    R.Left := R.Right;
    R.Right := Temp;
  end;
  if R.Top > R.Bottom then
  begin
    Temp := R.Top;
    R.Top := R.Bottom;
    R.Bottom := Temp;
  end;
end;

function CompareRect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top) and
            (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

function RectIsSquare(const R: TRect): Boolean;
begin
  Result := RectHeight(R) = RectWidth(R);
end;

function RectSquare(var ARect: TRect; AMaxSize: Integer): Boolean;
const
  cSquareMinSize = 4; // Min size is 4 pixel
var
  iMinSize, iW, iH :Integer;
  pTopLeft, pRightBottom: TPoint;
begin
  Result := False;
  if IsRectEmpty(ARect) or ((AMaxSize <> -1) and (AMaxSize < cSquareMinSize)) then
    Exit
  else if  RectIsSquare(ARect) then
  begin
    Result := True;
    Exit;
  end;

  iW := RectWidth(ARect);
  iH := RectHeight(ARect);
  iMinSize := Min(iW, iH);
  if AMaxSize = -1 then
    AMaxSize := iMinSize
  else
    AMaxSize := Min(iMinSize, AMaxSize);

  pTopLeft.Y :=ARect.Top + (iH - AMaxSize) div 2;
  pTopLeft.X :=ARect.Left + (iW - AMaxSize) div 2;

  pRightBottom.Y := pTopLeft.Y + AMaxSize;
  pRightBottom.X := pTopLeft.X + AMaxSize;

  ARect := Rect(pTopLeft, pRightBottom);
  Result := True;
end;

{$IFDEF MSWINDOWS}
{ Service routines }

function LoadDLL(const LibName: string): THandle;
begin
  Result := SafeLoadLibrary(LibName);
  if Result = 0 then
    OSCheck(False);
end;

function GetWindowsVersionString: string;
const
  sWindowsVersion = 'Windows %s %d.%.2d.%.3d %s';
var
  Ver: TOSVersionInfo;
  Platfrm: string[4];
begin
  Ver.dwOSVersionInfoSize := SizeOf(Ver);
  GetVersionEx(Ver);
  with Ver do
  begin
    case dwPlatformId of
      VER_PLATFORM_WIN32s:
        Platfrm := '32s';
      VER_PLATFORM_WIN32_WINDOWS:
        begin
          dwBuildNumber := dwBuildNumber and $0000FFFF;
          if (dwMajorVersion > 4) or ((dwMajorVersion = 4) and
            (dwMinorVersion >= 10)) then
            Platfrm := '98'
          else
            Platfrm := '95';
        end;
      VER_PLATFORM_WIN32_NT: Platfrm := 'NT';
    end;
    Result := Trim(SysUtils.Format(sWindowsVersion, [Platfrm, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;

{ RegisterServer procedure written by Vladimir Gaitanoff, 2:50/430.2 }

function RegisterServer(const ModuleName: string): Boolean;
type
  TCOMFunc = function: HRESULT;
const
  S_OK = $00000000;
var
  Handle: THandle;
  DllRegServ: TCOMFunc;
begin
  Handle := LoadDLL(ModuleName);
  try
    DllRegServ := GetProcAddress(Handle, 'DllRegisterServer');
    Result := Assigned(DllRegServ) and (DllRegServ() = S_OK);
  finally
    FreeLibrary(Handle);
  end;
end;

// UnregisterServer by Ralf Kaiser patterned on RegisterServer
function UnregisterServer(const ModuleName: string): Boolean;
type
  TCOMFunc = function: HRESULT;
const
  S_OK = $00000000;
var
  Handle: THandle;
  DllUnRegServ: TCOMFunc;
  DllCanUnloadNow: TCOMFunc;
begin
  Handle := LoadDLL(ModuleName);
  try
    DllUnRegServ := GetProcAddress(Handle, 'DllUnregisterServer');
    DllCanUnloadNow := GetProcAddress(Handle, 'DllCanUnloadNow');
    Result := Assigned(DllCanUnloadNow) and (DllCanUnloadNow() = S_OK) and
      Assigned(DllUnRegServ) and (DllUnRegServ() = S_OK);
  finally
    FreeLibrary(Handle);
  end;
end;

procedure FreeUnusedOle;
begin
  FreeLibrary(GetModuleHandle('OleAut32'));
end;

function GetEnvVar(const VarName: string): string;
begin
  Result := GetEnvironmentVariable(VarName);
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function GetEnvVar(const VarName: string): string;
begin
  Result := getenv(PChar(VarName));
end;
{$ENDIF UNIX}

{ String routines }

procedure SplitCommandLine(const CmdLine: string; var ExeName, Params: string);

  function SkipString(P: PChar): PChar;
  begin
    if P^ = '"' then
    begin
      Inc(P);
      while (P^ <> #0) and (P^ <> '"') do
        Inc(P);
      if P^ <> #0 then
        Inc(P);
    end
    else
      while P^ > ' ' do
      begin
        if P^ = '"' then
        begin
          Inc(P);
          while (P^ <> #0) and (P^ <> '"') do
            Inc(P);
          if P^ = #0 then
            Break;
        end;
        Inc(P);
      end;
    Result := P;
  end;

  function SkipWhiteChars(P: PChar): PChar;
  begin
    Result := P;
    while (Result^ <> #0) and (Result^ <= ' ') do
      Inc(Result);
  end;

var
  F, P: PChar;
begin
  ExeName := '';
  Params := '';
  if CmdLine <> '' then
  begin
    F := PChar(CmdLine);
    P := SkipString(F);
    if F^ = '"' then
      SetString(ExeName, F + 1, P - F - 2)
    else
      SetString(ExeName, F, P - F);
    P := SkipWhiteChars(P);
    SetString(Params, P, StrLen(P));
  end;
end;

function AnsiUpperFirstChar(const S: string): string;
var
  Temp: string;
begin
  Result := AnsiLowerCase(S);
  if S <> '' then
  begin
    Temp := Result[1];
    Temp := AnsiUpperCase(Temp);
    Result[1] := Temp[1];
  end;
end;

function StrPAlloc(const S: string): PChar;
begin
  Result := StrPCopy(StrAlloc(Length(S) + 1), S);
end;

function StringToPChar(var S: string): PChar;
begin
  Result := PChar(S);
end;

function DropT(const S: string): string;
begin
  if (UpCase(S[1]) = 'T') and (Length(S) > 1) then
    Result := Copy(S, 2, MaxInt)
  else
    Result := S;
end;

function WindowClassName(Wnd: THandle): string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, GetClassName(Wnd, Buffer, SizeOf(Buffer) - 1));
end;



function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(Info);
  if SystemParametersInfo(SPI_GETANIMATION, Info.cbSize, @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(Info);
  Info.iMinAnimate := Ord(Value);
  SystemParametersInfo(SPI_SETANIMATION, Info.cbSize, @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: THandle; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := GetAnimation;
  if Animation then
    SetAnimation(False);
  ShowWindow(Handle, CmdShow);
  if Animation then
    SetAnimation(True);
end;

procedure SwitchToWindow(Wnd: THandle; Restore: Boolean);
begin
  if Windows.IsWindowEnabled(Wnd) then
  begin
    SetForegroundWindow(Wnd);
    if Restore and Windows.IsWindowVisible(Wnd) then
    begin
      if not IsZoomed(Wnd) then
        SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
      Windows.SetFocus(Wnd);
    end;
  end;
end;

function GetWindowParent(Wnd: THandle): THandle;
begin
  Result := THandle(GetWindowLongPtr(Wnd, GWLP_HWNDPARENT));
end;

procedure ActivateWindow(Wnd: THandle);
begin
  if Wnd <> 0 then
  begin
    ShowWinNoAnimate(Wnd, SW_SHOW);
    SetForegroundWindow(Wnd);
  end;
end;

{$IFDEF BCB}
function FindPrevInstance(const MainFormClass: ShortString; const ATitle: string): THandle;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): THandle;
{$ENDIF BCB}
var
  BufClass, BufTitle: PChar;
begin
  Result := 0;
  if (MainFormClass = '') and (ATitle = '') then
    Exit;
  BufClass := nil;
  BufTitle := nil;
  if MainFormClass <> '' then
    BufClass := StrPAlloc(MainFormClass);
  if ATitle <> '' then
    BufTitle := StrPAlloc(ATitle);
  try
    Result := FindWindow(BufClass, BufTitle);
  finally
    StrDispose(BufTitle);
    StrDispose(BufClass);
  end;
end;

function WindowsEnum(Handle: HWND; var IsDelphi: Boolean): BOOL; stdcall;
begin
  if WindowClassName(Handle) = 'TAppBuilder' then
  begin
    IsDelphi := True;
    Result := False;
  end
  else
    Result := True;
end;

{$IFDEF BCB}
function ActivatePrevInstance(const MainFormClass: ShortString; const ATitle: string): Boolean;
{$ELSE}
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF BCB}
var
  PrevWnd, PopupWnd, ParentWnd: HWND;
  IsDelphi: Boolean;
begin
  Result := False;
  PrevWnd := FindPrevInstance(MainFormClass, ATitle);
  if PrevWnd <> 0 then
  begin
    ParentWnd := GetWindowParent(PrevWnd);
    while (ParentWnd <> GetDesktopWindow) and (ParentWnd <> 0) do
    begin
      PrevWnd := ParentWnd;
      ParentWnd := GetWindowParent(PrevWnd);
    end;
    if WindowClassName(PrevWnd) = 'TApplication' then
    begin
      IsDelphi := False;
      EnumThreadWindows(GetWindowTask(PrevWnd), @WindowsEnum, LPARAM(@IsDelphi));
      if IsDelphi then
        Exit;
      if IsIconic(PrevWnd) then
      begin { application is minimized }
        SendMessage(PrevWnd, WM_SYSCOMMAND, SC_RESTORE, 0);
        Result := True;
        Exit;
      end
      else
        ShowWinNoAnimate(PrevWnd, SW_SHOWNOACTIVATE);
    end
    else
      ActivateWindow(PrevWnd);
    PopupWnd := GetLastActivePopup(PrevWnd);
    if (PrevWnd <> PopupWnd) and Windows.IsWindowVisible(PopupWnd) and
      Windows.IsWindowEnabled(PopupWnd) then
    begin
      SetForegroundWindow(PopupWnd);
    end
    else
      ActivateWindow(PopupWnd);
    Result := True;
  end;
end;

{$IFDEF MSWINDOWS}
function BrowseForFolderNative(const Handle: THandle; const Title: string; var Folder: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Id: PItemIDList;
  FN: array [0..MAX_PATH] of Char;
begin
  with BrowseInfo do
  begin
    hwndOwner := Handle;
    pidlRoot := nil;
    pszDisplayName := FN;
    lpszTitle := PChar(Title);
    ulFlags := 0;
    lpfn := nil;
  end;
  Id := SHBrowseForFolder(BrowseInfo);
  Result := Id <> nil;
  if Result then
  begin
    SHGetPathFromIDList(Id, FN);
    Folder := FN;
  end;
end;
{$ENDIF MSWINDOWS}

procedure FitRectToScreen(var Rect: TRect);
var
  X, Y, Delta: Integer;
begin
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  if Rect.Right > X then
  begin
    Delta := Rect.Right - Rect.Left;
    Rect.Right := X;
    Rect.Left := Rect.Right - Delta;
  end;
  if Rect.Left < 0 then
  begin
    Delta := Rect.Right - Rect.Left;
    Rect.Left := 0;
    Rect.Right := Rect.Left + Delta;
  end;
  if Rect.Bottom > Y then
  begin
    Delta := Rect.Bottom - Rect.Top;
    Rect.Bottom := Y;
    Rect.Top := Rect.Bottom - Delta;
  end;
  if Rect.Top < 0 then
  begin
    Delta := Rect.Bottom - Rect.Top;
    Rect.Top := 0;
    Rect.Bottom := Rect.Top + Delta;
  end;
end;

procedure CenterWindow(Wnd: THandle);
var
  R: TRect;
begin
  GetWindowRect(Wnd, R);
  R := Rect((GetSystemMetrics(SM_CXSCREEN) - R.Right + R.Left) div 2,
    (GetSystemMetrics(SM_CYSCREEN) - R.Bottom + R.Top) div 2,
    R.Right - R.Left, R.Bottom - R.Top);
  FitRectToScreen(R);
  SetWindowPos(Wnd, 0, R.Left, R.Top, 0, 0, SWP_NOACTIVATE or
    SWP_NOSIZE or SWP_NOZORDER);
end;

{ Delete the requested message from the queue, but throw back }
{ any WM_QUIT msgs that PeekMessage may also return.          }
{ Copied from DbGrid.pas                                      }
procedure KillMessage(Wnd: THandle; Msg: Cardinal);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.WParam);
end;

procedure SetWindowTop(const Handle: THandle; const Top: Boolean);
const
  TopFlag: array [Boolean] of THandle = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, TopFlag[Top], 0, 0, 0, 0, SWP_NOMOVE or
    SWP_NOSIZE or SWP_NOACTIVATE);
end;

function MakeVariant(const Values: array of Variant): Variant;
begin
  if High(Values) - Low(Values) > 1 then
    Result := VarArrayOf(Values)
  else
  if High(Values) - Low(Values) = 1 then
    Result := Values[Low(Values)]
  else
    Result := Null;
end;

{$IFDEF MSWINDOWS}
{ Dialog units }

function DialogUnitsToPixelsX(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

function DialogUnitsToPixelsY(DlgUnits: Word): Word;
begin
  Result := (DlgUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

function PixelsToDialogUnitsX(PixUnits: Word): Word;
begin
  Result := PixUnits * 4 div LoWord(GetDialogBaseUnits);
end;

function PixelsToDialogUnitsY(PixUnits: Word): Word;
begin
  Result := PixUnits * 8 div HiWord(GetDialogBaseUnits);
end;

{$ENDIF MSWINDOWS}

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := '';
  for I := 0 to MaxInt do
  begin
    CurrentName := SysUtils.Format(FileNameMask, [I]);
    if not FileExists(NormalDir(Path) + CurrentName) then
    begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

procedure AntiAlias(Clip: TBitmap);
begin
  AntiAliasRect(Clip, 0, 0, Clip.Width, Clip.Height);
end;


  // (p3) duplicated from JvTypes to avoid JVCL dependencies
type
  TJvRGBTriple = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;

type
  PJvRGBArray = ^TJvRGBArray;
  TJvRGBArray = array [0..32766] of TJvRGBTriple;


procedure AntiAliasRect(Clip: TBitmap;
  XOrigin, YOrigin, XFinal, YFinal: Integer);
var
  Tmp, X, Y: Integer;
  Line0, Line1, Line2: PJvRGBArray;
  OPF: TPixelFormat;
begin
  // swap values
  if XFinal < XOrigin then
  begin
    Tmp := XOrigin;
    XOrigin := XFinal;
    XFinal := Tmp;
  end;
  if YFinal < YOrigin then
  begin
    Tmp := YOrigin;
    YOrigin := YFinal;
    YFinal := Tmp;
  end;
  XOrigin := Max(1, XOrigin);
  YOrigin := Max(1, YOrigin);
  XFinal := Min(Clip.Width - 2, XFinal);
  YFinal := Min(Clip.Height - 2, YFinal);
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := YOrigin to YFinal do
  begin
    Line0 := Clip.ScanLine[Y - 1];
    Line1 := Clip.ScanLine[Y];
    Line2 := Clip.ScanLine[Y + 1];
    for X := XOrigin to XFinal do
    begin
      Line1[X].rgbRed := (Line0[X].rgbRed + Line2[X].rgbRed + Line1[X - 1].rgbRed + Line1[X + 1].rgbRed) div 4;
      Line1[X].rgbGreen := (Line0[X].rgbGreen + Line2[X].rgbGreen + Line1[X - 1].rgbGreen + Line1[X + 1].rgbGreen) div
        4;
      Line1[X].rgbBlue := (Line0[X].rgbBlue + Line2[X].rgbBlue + Line1[X - 1].rgbBlue + Line1[X + 1].rgbBlue) div 4;
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure CopyRectDIBits(ACanvas: TCanvas; const DestRect: TRect; ABitmap: TBitmap;
  const SourceRect: TRect);
var
  Header, Bits: Pointer;
  HeaderSize, BitsSize: Cardinal;
  Bmp: TBitmap;
begin
  if ABitmap.PixelFormat < pf15bit then
  begin
    Bmp := ABitmap;
    // this function does not support palettes
    ABitmap := TBitmap.Create;
    ABitmap.Assign(Bmp);
    ABitmap.PixelFormat := pf24bit;
  end
  else
    Bmp := nil;
  try
    GetDIBSizes(ABitmap.Handle, HeaderSize, BitsSize);
    { Do not use Delphi's memory manager. }
    Header := VirtualAlloc(nil, HeaderSize, MEM_COMMIT, PAGE_READWRITE);
    Bits := VirtualAlloc(nil, BitsSize, MEM_COMMIT, PAGE_READWRITE);
    try
      GetDIB(ABitmap.Handle, ABitmap.Palette, Header^, Bits^);
      StretchDIBits(ACanvas.Handle,
        DestRect.Left, DestRect.Top,
        DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
        SourceRect.Left, SourceRect.Top,
        SourceRect.Right - SourceRect.Left, SourceRect.Bottom - SourceRect.Top,
        Bits, TBitmapInfo(Header^),
        DIB_RGB_COLORS, ACanvas.CopyMode);
    finally
      VirtualFree(Bits, 0, MEM_RELEASE);
      VirtualFree(Header, 0, MEM_RELEASE);
    end;
  finally
    if Bmp <> nil then
      ABitmap.Free;
  end;
end;

function IsTTFontSelected(const DC: HDC): Boolean;
var
  Metrics: TTextMetric;
begin
  GetTextMetrics(DC, Metrics);
  Result := (Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
end;

// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/gdi/fontext_6rlf.asp

function IsTrueType(const FontName: string): Boolean;
var
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(HWND_DESKTOP);
    Canvas.Font.Name := FontName;
    Result := IsTTFontSelected(Canvas.Handle);
    ReleaseDC(HWND_DESKTOP, Canvas.Handle);
    Canvas.Handle := NullHandle;
  finally
    Canvas.Free;
  end;
end;

{$IFDEF DELPHI2007}
{$WARNINGS OFF}  // D2007 gives a bogus W1035 on the first line that assigns Result.
{$ENDIF DELPHI2007}
function TextToValText(const AValue: string): string;
var
  I, J: Integer;
  CharSet: TSysCharSet;
begin
  Result := DelRSpace(AValue);
  if JclFormatSettings.DecimalSeparator <> JclFormatSettings.ThousandSeparator then
    Result := DelChars(Result, JclFormatSettings.ThousandSeparator);

  if (JclFormatSettings.DecimalSeparator <> '.') and (JclFormatSettings.ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', JclFormatSettings.DecimalSeparator);
  if (JclFormatSettings.DecimalSeparator <> ',') and (JclFormatSettings.ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', JclFormatSettings.DecimalSeparator);

  J := 1;
  CharSet := ['0'..'9', '-', '+',
        AnsiChar(JclFormatSettings.DecimalSeparator),
        AnsiChar(JclFormatSettings.ThousandSeparator)];
  for I := 1 to Length(Result) do
    if CharInSet(Result[I], CharSet) then
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
  SetLength(Result, J - 1);

  if Result = '' then
    Result := '0'
  else
  if Result = '-' then
    Result := '-0';
end;
{$WARNINGS ON}

function DrawText(Canvas: TCanvas; const Text: string; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  {$IFDEF UNICODE}
  Result := Windows.DrawText(Canvas.Handle, PChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
  {$ELSE}
  Result := DrawText(Canvas, PAnsiChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
  {$ENDIF UNICODE}
end;

function DrawText(DC: HDC; const Text: TCaption; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := Windows.DrawText(DC, PChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
end;

function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextEx(Canvas.Handle, PChar(Text), cchText, p4, dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;

function DrawText(Canvas: TCanvas; Text: PAnsiChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer;
begin
  Result := Windows.DrawTextA(Canvas.Handle, Text, Len, R, WinFlags);
end;

function DrawTextEx(Canvas: TCanvas; lpchText: PChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextEx(Canvas.Handle, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

function DrawText(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := DrawTextW(Canvas, Text, Len, R, WinFlags and not DT_MODIFYSTRING);
end;

function DrawTextEx(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
begin
  Result := DrawTextExW(Canvas, Text, cchText, p4, dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;

function DrawTextW(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := DrawTextW(Canvas, PWideChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING);
end;

function DrawTextW(Canvas: TCanvas; Text: PWideChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer;
begin
  Result := Windows.DrawTextW(Canvas.Handle, Text, Len, R, WinFlags);
end;

function DrawTextExW(Canvas: TCanvas; lpchText: PWideChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextExW(Canvas.Handle, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

function DrawTextExW(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextExW(Canvas.Handle, PWideChar(Text), cchText, p4, dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;

const
  // (p3) move to interface?
  ROP_DSna    = $00220326;   // RasterOp_NotAndROP
  {$EXTERNALSYM ROP_DSna}
  ROP_DSno    = MERGEPAINT;
  {$EXTERNALSYM ROP_DSno}
  ROP_DPSnoo  = PATPAINT;
  {$EXTERNALSYM ROP_DPSnoo}
  ROP_D       = $00AA0029;   // RasterOp_NopROP
  {$EXTERNALSYM ROP_D}
  ROP_Dn      = DSTINVERT;   // DSTINVERT
  {$EXTERNALSYM ROP_Dn}
  ROP_SDna    = SRCERASE;    // SRCERASE
  {$EXTERNALSYM ROP_SDna}
  ROP_SDno    = $00DD0228;   // RasterOp_OrNotROP
  {$EXTERNALSYM ROP_SDno}
  ROP_DSan    = $007700E6;   // RasterOp_NandROP
  {$EXTERNALSYM ROP_DSan}
  ROP_DSon    = $001100A6;   // NOTSRCERASE
  {$EXTERNALSYM ROP_DSon}

function RasterOpToWinRop(Rop: RasterOp): Cardinal;
begin
  case Rop of
    RasterOp_ClearROP:
      Result := BLACKNESS;
    RasterOp_NotROP:
      Result := DSTINVERT;
    RasterOp_NotOrROP:
      Result := MERGEPAINT;
    RasterOp_NotCopyROP:
      Result := NOTSRCCOPY;
    RasterOp_NorROP:
      Result := NOTSRCERASE;
    RasterOp_AndROP:
      Result := SRCAND;
    RasterOp_CopyROP:
      Result := SRCCOPY;
    RasterOp_AndNotROP:
      Result := SRCERASE;
    RasterOp_XorROP:
      Result := SRCINVERT;
    RasterOp_OrROP:
      Result := SRCPAINT;
    RasterOp_SetROP:
      Result := WHITENESS;
    RasterOp_NotAndROP:
      Result := ROP_DSna;
    RasterOp_NopROP:
      Result := ROP_D;
    RasterOp_OrNotROP:
      Result := ROP_SDno;
    RasterOp_NandROP:
      Result := ROP_DSan;
  else
    Result := 0;
  end;
end;

function BitBlt(DestCanvas: TCanvas; X, Y, Width, Height: Integer; SrcCanvas: TCanvas;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean = True): LongBool;
begin
  // NB! IgnoreMask is not supported in VCL!
  Result := Windows.BitBlt(DestCanvas.Handle, X, Y, Width, Height, SrcCanvas.Handle,
    XSrc, YSrc, WinRop);
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; Rop: RasterOp; IgnoreMask: Boolean): LongBool;
begin
  Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, RasterOpToWinRop(Rop));
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean): LongBool;
begin
  Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, WinRop);
end;

function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; WinRop: Cardinal): LongBool;
begin
  Result := Windows.BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, WinRop);
end;





function IsEqualGUID(const IID1, IID2: TGUID): Boolean;
begin
  Result := SysUtils.IsEqualGUID(IID1, IID2);
end;

{Color functions}
procedure RGBToHSV(R, G, B: Integer; var H, S, V: Integer);

var
  Delta: Integer;
  Min, Max: Integer;

  function GetMax(I, J, K: Integer): Integer;
  begin
    if J > I then
      I := J;
    if K > I then
      I := K;
    Result := I;
  end;

  function GetMin(I, J, K: Integer): Integer;
  begin
    if J < I then
      I := J;
    if K < I then
      I := K;
    Result := I;
  end;


begin
  Min := GetMin(R, G, B);
  Max := GetMax(R, G, B);
  V := Max;
  Delta := Max - Min;
  if Max = 0 then
    S := 0
  else
    S := (255 * Delta) div Max;
  if S = 0 then
    H := 0
  else
  begin
    if R = Max then
      H := (60 * (G - B)) div Delta
    else
    if G = Max then
      H := 120 + (60 * (B - R)) div Delta
    else
      H := 240 + (60 * (R - G)) div Delta;
    if H < 0 then
      H := H + 360;
  end;
end;

function RGBToBGR(Value: Cardinal): Cardinal;
begin
  Result :=
   ((Value and $00FF0000) shr 16) or
    (Value and $0000FF00) or
   ((Value and $000000FF) shl 16);
end;

function ColorToPrettyName(Value: TColor): string;
var
  Index: Integer;
begin
  for Index := Low(ColorValues) to High(ColorValues) do
    if Value = ColorValues[Index].Value then
    begin
      Result := ColorValues[Index].Description;
      Exit;
    end;
  for Index := Low(StandardColorValues) to High(StandardColorValues) do
    if Value = StandardColorValues[Index].Value then
    begin
      Result := StandardColorValues[Index].Description;
      Exit;
    end;
  for Index := Low(SysColorValues) to High(SysColorValues) do
    if Value = SysColorValues[Index].Value then
    begin
      Result := SysColorValues[Index].Description;
      Exit;
    end;
  Result := ColorToString(Value);
end;

function PrettyNameToColor(const Value: string): TColor;
var
  Index: Integer;
  ColorResult: Integer;
begin
  for Index := Low(ColorValues) to High(ColorValues) do
  begin
    if CompareText(Value, ColorValues[Index].Description) = 0 then
    begin
      Result := ColorValues[Index].Value;
      Exit;
    end;
  end;
  for Index := Low(StandardColorValues) to High(StandardColorValues) do
  begin
    if CompareText(Value, StandardColorValues[Index].Description) = 0 then
    begin
      Result := StandardColorValues[Index].Value;
      Exit;
    end;
  end;
  for Index := Low(SysColorValues) to High(SysColorValues) do
  begin
    if CompareText(Value, SysColorValues[Index].Description) = 0 then
    begin
      Result := SysColorValues[Index].Value;
      Exit;
    end;
  end;
  if IdentToColor(Value, ColorResult) then
    Result := ColorResult
  else
    Result := clNone;
end;

function StartsText(const SubStr, S: string): Boolean;
begin
  Result := AnsiStartsText(SubStr, S);
end;

function EndsText(const SubStr, S: string): Boolean;
begin
  Result := AnsiEndsText(SubStr, S);
end;

function DequotedStr(const S: string; QuoteChar: Char = ''''): string;
begin
  Result := AnsiDequotedStr(S, Char(QuoteChar));
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  P: PChar;
begin
  P := PChar(S);
  Result := AnsiExtractQuotedStr(P, AQuote);
end;

procedure CollectionQuickSort(List: Classes.TCollection; L, R: Integer; SortProc: TCollectionSortProc);
var
 I, J, pix: Integer;
 P, t1, t2: TCollectionItem;
begin
  List.BeginUpdate;
  repeat
    I := L;
    J := R;
    pix := (L+R) shr 1;
    if pix > List.Count - 1 then
      pix := List.Count - 1;
    P := List.Items[pix];

    repeat
      while SortProc(List.Items[I], P) < 0 do
        Inc(I);
      while SortProc(List.Items[J], P) > 0 do
        Dec(J);

      if I <= J then
      begin
        t1 := List.Items[I];
        t2 := List.Items[J];
        t1.Index := J;
        t2.Index := I;

        if pix = I then
          pix := J
        else
        if pix = J then
          pix := I;

        P := List.Items[pix];
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      CollectionQuickSort(List, L, J, SortProc);
    L := I;
  until I >= R;
  List.EndUpdate;
end;

procedure CollectionSort(Collection: Classes.TCollection; SortProc: TCollectionSortProc);
begin
  if Assigned(Collection) and Assigned(SortProc) and (Collection.Count >= 2) then
    CollectionQuickSort(Collection, 0, Collection.Count - 1, SortProc);
end;

{ TIntegerList }

function TIntegerList.Add(Value: Integer): Integer;
begin
  Result := inherited Add(TObject(Value));
end;

procedure TIntegerList.DoChange(Item: Integer; Action: TListNotification);
begin
  if Assigned(OnChange) then
    OnChange(Self, Item, Action);
end;

function TIntegerList.Extract(Item: Integer): Integer;
begin
  Result := Integer(inherited Extract(TObject(Item)));
end;

function TIntegerList.First: Integer;
begin
  Result := Integer(inherited First);
end;

function TIntegerList.GetItem(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

function TIntegerList.IndexOf(Item: Integer): Integer;
begin
  Result := inherited IndexOf(TObject(Item));
end;

procedure TIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, TObject(Item));
end;

function TIntegerList.Last: Integer;
begin
  Result := Integer(inherited Last);
end;

procedure TIntegerList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  DoChange(Integer(Ptr), Action);
end;

procedure TIntegerList.ReadData(Reader: TReader);
begin
  FLoading := True;
  try
    Clear;
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      Add(Reader.ReadInteger);
    Reader.ReadListEnd;
  finally
    FLoading := False;
  end;
end;

function TIntegerList.Remove(Item: Integer): Integer;
begin
  Result := Integer(inherited Remove(TObject(Item)));
end;

procedure TIntegerList.SetItem(Index: Integer; const Value: Integer);
begin
  inherited Items[Index] := TObject(Value);
end;

procedure TIntegerList.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    Writer.WriteInteger(Items[I]);
  Writer.WriteListEnd;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

