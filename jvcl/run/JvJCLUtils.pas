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
  
You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

// (ahuser) No dependency on JCL units. Required functions are emulated.
//          With NO_JCL defined the executable file size shrinks because
//          the JCL has no JvFinalize support and executes all in the
//          initialization sections.
{$DEFINE NO_JCL}

unit JvJCLUtils;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

// (p3) note: this unit should only contain JCL compatible routines (no Forms etc)
// and no JVCL units!
// (ahuser) Unfortunately the QGraphics unit imports the QForms unit. Because
//          the JCL has the same problem with CLX it should not make any difference.

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages, ShlObj, ActiveX,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFDEF UNIX}
  Xlib,
  {$ENDIF UNIX}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils, Classes, Graphics, Clipbrd, Controls,
  {$IFDEF VisualCLX}
  Qt, QWindows, QStdCtrls, 
  {$ENDIF VisualCLX}
  {$IFDEF HAS_UNIT_STRUTILS}
  StrUtils,
  {$ENDIF HAS_UNIT_STRUTILS}
  TypInfo;

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

  {$IFDEF VCL}
  NullHandle = 0;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  NullHandle = nil;
  {$ENDIF VisualCLX}

{$IFDEF UNIX}
type
  TFileTime = Integer;
{$ENDIF UNIX}

// (p3) duplicated from JvTypes since this unit should not rely on JVCL at all
type
  TDateOrder = (doMDY, doDMY, doYMD);
const
  DefaultDateOrder = doDMY;
  CenturyOffset: Byte = 60;
  NullDate: TDateTime = {-693594} 0;
                 
function USToLocalFloatStr(const Text: string): string;
function StrToFloatUS(const Text: string): Extended;
// StrToFloatUS uses US '.' as decimal seperator and ',' as thousand separator
function StrToFloatUSDef(const Text: string; Default: Extended): Extended;

function VarIsInt(Value: Variant): Boolean;
 // VarIsInt returns VarIsOrdinal-[varBoolean]

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

{ SubStr returns substring from string, S, separated with Separator string}
function SubStr(const S: string; const Index: Integer; const Separator: string): string;
function SubStrW(const S: WideString; const Index: Integer; const Separator: WideString): WideString;
{ SubStrEnd same to previous function but Index numerated from the end of string }
function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
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
function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
function ReplaceStringW(S: WideString; const OldPattern, NewPattern: WideString): WideString;
{ ConcatSep concatenate S1 and S2 strings with Separator.
  if S = '' then separator not included }
function ConcatSep(const S1, S2, Separator: string): string;
{ ConcatLeftSep is same to previous function, but
  strings concatenate right to left }
function ConcatLeftSep(const S1, S2, Separator: string): string;

{ Next 4 function for russian chars transliterating.
  This functions are needed because Oem2Ansi and Ansi2Oem functions
  sometimes suck }
procedure Dos2Win(var S: string);
procedure Win2Dos(var S: string);
function Dos2WinRes(const S: string): string;
function Win2DosRes(const S: string): string;
function Win2Koi(const S: string): string;

{ FillWideChar fills Buffer with Count WideChars (2 Bytes) }
procedure FillWideChar(var Buffer; Count: Integer; const Value: WideChar);
{ MoveWideChar copies Count WideChars from Source to Dest }
procedure MoveWideChar(const Source; var Dest; Count: Integer);

{ Spaces returns string consists on N space chars }
function Spaces(const N: Integer): string;
function SpacesW(const N: Integer): WideString;
{ AddSpaces adds spaces to string S, if its Length is smaller than N }
function AddSpaces(const S: string; const N: Integer): string;
function AddSpacesW(const S: WideString; const N: Integer): WideString;
{ function LastDateRUS for russian users only }
{ returns date relative to current date: '‰‚‡ ‰Ìˇ Ì‡Á‡‰' }
function LastDateRUS(const Dat: TDateTime): string;
{ CurrencyToStr format Currency, Cur, using ffCurrency float format}
function CurrencyToStr(const Cur: Currency): string;
{ Cmp compares two strings and returns True if they are equal. Case-insensitive.}
function Cmp(const S1, S2: string): Boolean;
{ StringCat add S2 string to S1 and returns this string }
function StringCat(var S1: string; S2: string): string;
{ HasChar returns True, if Char, Ch, contains in string, S }
function HasChar(const Ch: Char; const S: string): Boolean;
function HasCharW(const Ch: WideChar; const S: WideString): Boolean;
function HasAnyChar(const Chars: string; const S: string): Boolean;
function CharInSet(const Ch: Char; const SetOfChar: TSysCharSet): Boolean;
function CharInSetW(const Ch: WideChar; const SetOfChar: TSysCharSet): Boolean;
function CountOfChar(const Ch: Char; const S: string): Integer;
function DefStr(const S: string; Default: string): string;

{ StrLICompW2 is a faster replacement for JclUnicode.StrLICompW }
function StrLICompW2(S1, S2: PWideChar; MaxLen: Integer): Integer;
function StrPosW(S, SubStr: PWideChar): PWideChar;
function StrLenW(S: PWideChar): Integer;
{$IFDEF COMPILER5}
function WideCompareText(const S1, S2: WideString): Integer;
function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;
{$ENDIF COMPILER5}
function TrimW(const S: WideString): WideString;
function TrimLeftW(const S: WideString): WideString;
function TrimRightW(const S: WideString): WideString;
{**** files routines}
procedure SetDelimitedText(List: TStrings; const Text: string; Delimiter: Char);

{$IFDEF COMPILER5}

type
  TValueSign = -1..1;

const
  NegativeValue = Low(TValueSign);
  ZeroValue = 0;
  PositiveValue = High(TValueSign);

function Sign(const AValue: Integer): TValueSign; overload;
function Sign(const AValue: Int64): TValueSign; overload;
function Sign(const AValue: Double): TValueSign; overload;

{$ENDIF COMPILER5}

const
  {$IFDEF MSWINDOWS}
  DefaultCaseSensitivity = False;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  DefaultCaseSensitivity = True;
  {$ENDIF UNIX}

{ GetTempDir returns Windows temporary folder name }
function GetTempDir: string;
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
{ AddSlash add slash Char to Dir parameter, if needed }
procedure AddSlash(var Dir: TFileName);
{ AddSlash returns string with added slash Char to Dir parameter, if needed }
function AddSlash2(const Dir: TFileName): string;
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
function ExePath: TFileName;
function CopyDir(const SourceDir, DestDir: TFileName): Boolean;
function FileTimeToDateTime(const FT: TFileTime): TDateTime;
function MakeValidFileName(const FileName: TFileName; ReplaceBadChar: Char): TFileName;

{**** Graphic routines }

{$IFDEF VCL}
{ IsTTFontSelected returns True, if True Type font
  is selected in specified device context }
function IsTTFontSelected(const DC: HDC): Boolean;
function KeyPressed(VK: Integer): Boolean;
{$ENDIF VCL}

{$IFDEF VisualCLX}
{ VisualCLX/crossplatform versions of the same functions in JclQGraphics }
type
  TGradientDirection = (gdVertical, gdHorizontal);
  TRegionBitmapMode = (rmInclude, rmExclude);

procedure ScreenShot(Bmp: TBitmap; Left, Top, Width, Height: Integer; Window: QWidgetH); {overload;}
function FillGradient(DC: HDC; ARect: TRect; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean;
function CreateRegionFromBitmap(Bitmap: TBitmap; RegionColor: TColor;
  RegionBitmapMode: TRegionBitmapMode): QRegionH;
{$ENDIF VisualCLX}

{ TrueInflateRect inflates rect in other method, than InflateRect API function }
function TrueInflateRect(const R: TRect; const I: Integer): TRect;

{**** other routines }
procedure SwapInt(var Int1, Int2: Integer);
function IntPower(Base, Exponent: Integer): Integer;
function ChangeTopException(E: TObject): TObject; // Linux version writes error message to ErrOutput
function StrToBool(const S: string): Boolean;

function Var2Type(V: Variant; const VarType: Integer): Variant;
function VarToInt(V: Variant): Integer;
function VarToFloat(V: Variant): Double;
{$IFDEF COMPILER5}
function VarIsStr(const V: Variant): Boolean;
{$ENDIF COMPILER5}

{ following functions are not documented
  because they do not work properly sometimes, so do not use them }
// (rom) ReplaceStrings1, GetSubStr removed
function GetParameter: string;
function GetLongFileName(const FileName: string): string;
{$IFDEF COMPILER5}
{* from unit FileCtrl}
function DirectoryExists(const Name: string): Boolean;
procedure ForceDirectories(Dir: string);
{# from unit FileCtrl}
function SameFileName(const FN1, FN2: string): Boolean;
{$ENDIF COMPILER5}
function FileNewExt(const FileName, NewExt: TFileName): TFileName;
function GetComputerID: string;
function GetComputerName: string;

{**** string routines }

{ ReplaceAllStrings searches for all substrings, Words,
  in a string, S, and replaces them with Frases with the same Index. }
function ReplaceAllStrings(S: string; Words, Frases: TStrings): string;
{ ReplaceStrings searches the Word in a string, S, on PosBeg position,
  in the list, Words, and if founds, replaces this Word
  with string from another list, Frases, with the same Index,
  and then update NewSelStart variable }
function ReplaceStrings(const S: string; PosBeg, Len: Integer; Words, Frases: TStrings; var NewSelStart: Integer): string;
{ CountOfLines calculates the lines count in a string, S,
  each line must be separated from another with CrLf sequence }
function CountOfLines(const S: string): Integer;
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
function PointL(const X, Y: Longint): TPointL;
// (rom) from JvBandUtils to make it obsolete
function iif(const Test: Boolean; const ATrue, AFalse: Variant): Variant;

procedure CopyIconToClipboard(Icon: TIcon; BackColor: TColor);
function CreateIconFromClipboard: TIcon;
{$IFDEF VCL}
{ begin JvIconClipboardUtils }
{ Icon clipboard routines }
function CF_ICON: Word;
procedure AssignClipboardIcon(Icon: TIcon);

{ Real-size icons support routines (32-bit only) }
procedure GetIconSize(Icon: HICON; var W, H: Integer);
function CreateRealSizeIcon(Icon: TIcon): HICON;
procedure DrawRealSizeIcon(Canvas: TCanvas; Icon: TIcon; X, Y: Integer);
{end JvIconClipboardUtils }
{$ENDIF VCL}

{ begin JvRLE }

// (rom) changed API for inclusion in JCL

procedure RleCompressTo(InStream, OutStream: TStream);
procedure RleDecompressTo(InStream, OutStream: TStream);
procedure RleCompress(Stream: TStream);
procedure RleDecompress(Stream: TStream);
{ end JvRLE }

{ begin JvDateUtil }
function CurrentYear: Word;
function IsLeapYear(AYear: Integer): Boolean;
function DaysPerMonth(AYear, AMonth: Integer): Integer;
function FirstDayOfPrevMonth: TDateTime;
function LastDayOfPrevMonth: TDateTime;
function FirstDayOfNextMonth: TDateTime;
function ExtractDay(ADate: TDateTime): Word;
function ExtractMonth(ADate: TDateTime): Word;
function ExtractYear(ADate: TDateTime): Word;
function IncDate(ADate: TDateTime; Days, Months, Years: Integer): TDateTime;
function IncDay(ADate: TDateTime; Delta: Integer): TDateTime;
function IncMonth(ADate: TDateTime; Delta: Integer): TDateTime;
function IncYear(ADate: TDateTime; Delta: Integer): TDateTime;
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
function CutTime(ADate: TDateTime): TDateTime; { Set time to 00:00:00:00 }

{ String to date conversions }
function GetDateOrder(const DateFormat: string): TDateOrder;
function MonthFromName(const S: string; MaxLen: Byte): Byte;
{$IFDEF COMPILER5}
function TryStrToDateTime(const S: string; out Date: TDateTime): Boolean;
{$ENDIF COMPILER5}
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
function iconversion(InP: PChar; OutP: Pointer; InBytes, OutBytes: Cardinal;
  const ToCode, FromCode: string): Boolean;
function iconvString(const S, ToCode, FromCode: string): string;
function iconvWideString(const S: WideString; const ToCode, FromCode: string): WideString;
function OemStrToAnsi(const S: string): string;
function AnsiStrToOem(const S: string): string;
{$ENDIF UNIX}

function StrToOem(const AnsiStr: string): string;
{ StrToOem translates a string from the Windows character set into the
  OEM character set. }
function OemToAnsiStr(const OemStr: string): string;
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
function MakeStr(C: WideChar; N: Integer): WideString; overload;
function MS(C: Char; N: Integer): string;
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
function CompStr(const S1, S2: string): Integer;
{ CompStr compares S1 to S2, with case-sensitivity. The return value is
  -1 if S1 < S2, 0 if S1 = S2, or 1 if S1 > S2. }
function CompText(const S1, S2: string): Integer;
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
function XorDecode(const Key, Source: string): string;

{ ** Command line routines ** }

function GetCmdLineArg(const Switch: string; ASwitchChars: TSysCharSet): string;

{ ** Numeric string handling routines ** }

function Numb2USA(const S: string): string;
{ Numb2USA converts numeric string S to USA-format. }
function Dec2Hex(N: Longint; A: Byte): string;
function D2H(N: Longint; A: Byte): string;
{ Dec2Hex converts the given value to a hexadecimal string representation
  with the minimum number of digits (A) specified. }
function Hex2Dec(const S: string): Longint;
function H2D(const S: string): Longint;
{ Hex2Dec converts the given hexadecimal string to the corresponding integer
  value. }
function Dec2Numb(N: Longint; A, B: Byte): string;
{ Dec2Numb converts the given value to a string representation with the
  base equal to B and with the minimum number of digits (A) specified. }
function Numb2Dec(S: string; B: Byte): Longint;
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
function StringStartsWith(const Str, SubStr: string): Boolean; // case insensitive
function StringEndsWith(const Str, SubStr: string): Boolean; // case insensitive
function ExtractFilePath2(const FileName: string): string;

{$IFDEF COMPILER5}
// missing in Delphi 5
function AnsiStartsText(const SubText, Text: string): Boolean;
function AnsiEndsText(const SubText, Text: string): Boolean;
function AnsiStartsStr(const SubStr, Str: string): Boolean;
function AnsiEndsStr(const SubStr, Str: string): Boolean;
{$ENDIF COMPILER5}

{end JvStrUtils}

{$IFDEF UNIX}
function GetTempFileName(const Prefix: string): string;
{$ENDIF UNIX}

{ begin JvFileUtil }
function FileDateTime(const FileName: string): TDateTime;
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
function GetWindowsDir: string;
function GetSystemDir: string;
{$ENDIF MSWINDOWS}

function ShortToLongFileName(const ShortName: string): string;
function ShortToLongPath(const ShortName: string): string;
function LongToShortFileName(const LongName: string): string;
function LongToShortPath(const LongName: string): string;
{$IFDEF MSWINDOWS}
procedure CreateFileLink(const FileName, DisplayName: string; Folder: Integer);
procedure DeleteFileLink(const DisplayName: string; Folder: Integer);
{$ENDIF MSWINDOWS}

{ end JvFileUtil }

{$IFDEF COMPILER5}
{ begin JvXMLDatabase D5 compatiblility functions }
function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
function CompareDateTime(const A, B: TDateTime): Integer;
// function StrToFloatDef(const Str: string; Default: Extended): Extended;
{ end JvXMLDatabase D5 compatiblility functions }

{ D5 compatibility functions }
procedure RaiseLastOSError;
function IncludeTrailingPathDelimiter(const APath: string): string;
function ExcludeTrailingPathDelimiter(const APath: string): string;
{$ENDIF COMPILER5}

// Works like PtInRect but includes all edges in comparision
function PtInRectInclusive(R: TRect; Pt: TPoint): Boolean;
// Works like PtInRect but excludes all edges from comparision
function PtInRectExclusive(R: TRect; Pt: TPoint): Boolean;

function FourDigitYear: Boolean; {$IFDEF SUPPORTS_DEPRECATED} deprecated; {$ENDIF}
function IsFourDigitYear: Boolean;

{ moved from JvJVCLUTils }

//Open an object with the shell (url or something like that)
function OpenObject(Value: PChar): Boolean; overload;
function OpenObject(const Value: string): Boolean; overload;

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
procedure ExecuteAndWait(const FileName: string; Visibility: Integer);

{$IFDEF VCL}
// returns True if this is the first instance of the program that is running
function FirstInstance(const ATitle: string): Boolean;
// restores a window based on it's classname and Caption. Either can be left empty
// to widen the search
procedure RestoreOtherInstance(const MainFormClassName, MainFormCaption: string);

// manipulate the traybar and start button
procedure HideTraybar;
procedure ShowTraybar;
procedure ShowStartButton;
procedure HideStartButton;

// (rom) SC_MONITORPOWER is documented as Windows 95 only
// (rom) better do some testing
// set monitor functions
procedure MonitorOn;
procedure MonitorOff;
procedure LowPower;

// send a key to the window named AppName
function SendKey(const AppName: string; Key: Char): Boolean;
{$ENDIF VCL}
{$IFDEF MSWINDOWS}
// associates an extension to a specific program
procedure AssociateExtension(const IconPath, ProgramName, Path, Extension: string);

function GetRecentDocs: TStringList;
procedure AddToRecentDocs(const FileName: string);

// returns a list of all windows currently visible, the Objects property is filled with their window handle
procedure GetVisibleWindows(List: TStrings);
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
function StrToFloatDef(const Str: string; Def: Extended): Extended;
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

type
  TTime = type TDateTime;
  {$EXTERNALSYM TTime}
  TDate = type TDateTime;
  {$EXTERNALSYM TDate}

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
  TRunDLL32Proc = procedure(Handle: HWND; HInstance: HMODULE; CmdLine: PChar; CmdShow: Integer); stdcall;

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
procedure RunDll32Internal(Wnd: HWND; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
{ GetDLLVersion loads DLLName, gets a pointer to the DLLVersion function and calls it, returning the major and minor version values
from the function. Returns False if the DLL couldn't be loaded or if GetDLLVersion couldn't be found. }
function GetDLLVersion(const DLLName: string; var pdwMajor, pdwMinor: Integer): Boolean;
{$ENDIF MSWINDOWS}

procedure ResourceNotFound(ResID: PChar);
function RectWidth(R: TRect): Integer;
function RectHeight(R: TRect): Integer;

{$IFDEF MSWINDOWS}
procedure FreeUnusedOle;
function GetWindowsVersion: string;
function LoadDLL(const LibName: string): THandle;
function RegisterServer(const ModuleName: string): Boolean;
function UnregisterServer(const ModuleName: string): Boolean;
{$ENDIF MSWINDOWS}

{ String routines }
function GetEnvVar(const VarName: string): string;
function AnsiUpperFirstChar(const S: string): string;
function StringToPChar(var S: string): PChar;
function StrPAlloc(const S: string): PChar;
procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
function DropT(const S: string): string;

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
procedure FreeMemo(var fpBlock: Pointer);
function GetMemoSize(fpBlock: Pointer): Longint;
function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean;

{ Manipulate huge pointers routines }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);
function WindowClassName(Wnd: HWND): string;
{$IFDEF VCL}
procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
procedure ActivateWindow(Wnd: HWND);
procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
procedure KillMessage(Wnd: HWND; Msg: Cardinal);
{$ENDIF VCL}
{ SetWindowTop put window to top without recreating window }
procedure SetWindowTop(const Handle: HWND; const Top: Boolean);
procedure CenterWindow(Wnd: HWND);
function MakeVariant(const Values: array of Variant): Variant;

{ Convert dialog units to pixels and backwards }

{$IFDEF MSWINDOWS}
function DialogUnitsToPixelsX(DlgUnits: Word): Word;
function DialogUnitsToPixelsY(DlgUnits: Word): Word;
function PixelsToDialogUnitsX(PixUnits: Word): Word;
function PixelsToDialogUnitsY(PixUnits: Word): Word;
{$ENDIF MSWINDOWS}

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

{$IFDEF VCL}
{$IFDEF BCB}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWND;
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF BCB}
{$ENDIF VCL}

{$IFDEF MSWINDOWS}
{ BrowseForFolderNative displays Browse For Folder dialog }
function BrowseForFolderNative(const Handle: HWND; const Title: string; var Folder: string): Boolean;
{$ENDIF MSWINDOWS}

procedure AntiAlias(Clip: TBitmap);
procedure AntiAliasRect(Clip: TBitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);

{$IFDEF VCL}
procedure CopyRectDIBits(ACanvas: TCanvas; const DestRect: TRect;
  ABitmap: TBitmap; const SourceRect: TRect);
function IsTrueType(const FontName: string): Boolean;
{$ENDIF VCL}

// Removes all non-numeric characters from AValue and returns
// the resulting string
function TextToValText(const AValue: string): string;

{$IFDEF VCL}
// VisualCLX compatibility functions
function DrawText(DC: HDC; const Text: TCaption; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawText(Canvas: TCanvas; Text: PAnsiChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawText(Canvas: TCanvas; const Text: string; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextEx(Canvas: TCanvas; lpchText: PChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
{$IFDEF COMPILER6_UP}
function DrawText(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextEx(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
{$ENDIF COMPILER6_UP}

function DrawTextW(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextW(Canvas: TCanvas; Text: PWideChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
function DrawTextExW(Canvas: TCanvas; lpchText: PWideChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
function DrawTextExW(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;

type
  {$IFDEF COMPILER6_UP}
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
  {$ELSE}
  // Delphi 5 and below doesn't support values in enums
  RasterOp = Integer;
const  
  RasterOp_CopyROP = 0;
  RasterOp_OrROP = 1;
  RasterOp_XorROP = 2;
  RasterOp_NotAndROP = 3;
  RasterOp_EraseROP = 3;
  RasterOp_NotCopyROP = 4;
  RasterOp_NotOrROP = 5;
  RasterOp_NotXorROP = 6;
  RasterOp_AndROP = 7;
  RasterOp_NotEraseROP = 7;
  RasterOp_NotROP = 8;
  RasterOp_ClearROP = 9;
  RasterOp_SetROP = 10;
  RasterOp_NopROP = 11;
  RasterOp_AndNotROP = 12;
  RasterOp_OrNotROP = 13;
  RasterOp_NandROP = 14;
  RasterOp_NorROP = 15;
  RasterOp_LastROP = 15;
  {$ENDIF COMPILER6_UP}

function BitBlt(DestCanvas: TCanvas; X, Y, Width, Height: Integer; SrcCanvas: TCanvas;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean = True): LongBool;overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; Rop: RasterOp; IgnoreMask: Boolean): LongBool; overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean): LongBool; overload;
function BitBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC;
  XSrc, YSrc: Integer; WinRop: Cardinal): LongBool; overload;

{$ENDIF VCL}

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  SysConst,
  {$IFDEF MSWINDOWS}
  ComObj, ShellAPI, MMSystem, Registry,
  {$ENDIF MSWINDOWS}
  Consts,
  {$IFNDEF NO_JCL}
  JclStrings, JclSysInfo,
  {$ENDIF !NO_JCL}
  Math;

const
  Separators: TSysCharSet = [#00, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
  ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];
  {$IFDEF MSWINDOWS}
  RC_OpenCDDrive = 'set cdaudio door open wait';
  RC_CloseCDDrive = 'set cdaudio door closed wait';
  RC_ShellName = 'Shell_TrayWnd';
  RC_DefaultIcon = 'DefaultIcon';
  {$ENDIF MSWINDOWS}

resourcestring
  // (p3) duplicated from JvConsts since this unit should not rely on JVCL at all
  RsEPropertyNotExists = 'Property "%s" does not exist';
  RsEInvalidPropertyType = 'Property "%s" has invalid type';
  RsEPivotLessThanZero = 'JvJCLUtils.MakeYear4Digit: Pivot < 0';

{$IFDEF NO_JCL}

  // These are the replacement functions for the JCL.

const
  AnsiSpace = AnsiChar(#32);
  AnsiForwardSlash = AnsiChar('/');

function StrIPos(const SubStr, S: string): Integer;
begin
  Result := Pos(AnsiLowerCase(SubStr), AnsiLowerCase(S));
end;

function CharIsDigit(Ch: AnsiChar): Boolean;
begin
  Result := Ch in ['0'..'9'];
end;

function CharIsNumber(Ch: AnsiChar): Boolean;
begin
  Result := Ch in ['0'..'9'];
end;

function CharIsAlpha(Ch: AnsiChar): Boolean;
begin
  Result := Windows.IsCharAlpha(Ch);
end;

{ (ahuser) make Delphi 5 compiler happy
function StrStripNonNumberChars(const S: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    Ch := S[I];
    if CharIsNumber(Ch) or (Ch = DecimalSeparator) then
      Result := Result + Ch;
  end;
end;}

{$IFDEF MSWINDOWS}
function GetRecentFolder: string;
var
  ItemIDList: PItemIDList;
begin
  OleCheck(SHGetSpecialFolderLocation(0, CSIDL_RECENT, ItemIDList));
  SetLength(Result, MAX_PATH);
  SHGetPathFromIDList(ItemIDList, PChar(Result));
  SetLength(Result, Length(PChar(Result)));
end;
{$ENDIF MSWINDOWS}

{$ENDIF NO_JCL}

// StrToFloatUS uses US '.' as decimal separator and ',' as thousand separator

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
end;

function StrToFloatUS(const Text: string): Extended;
begin
  try
    Result := StrToFloat(USToLocalFloatStr(Text));
  except
    Result := StrToFloat(Text); // try it with local settings
  end;
end;

function StrToFloatUSDef(const Text: string; Default: Extended): Extended;
begin
  Result := StrToFloatDef(USToLocalFloatStr(Text), Default);
end;

function VarIsInt(Value: Variant): Boolean;
begin
  Result := VarType(Value) in [varByte,
    {$IFDEF COMPILER6_UP}
    varShortInt, varWord, varLongWord, {varInt64,}
    {$ENDIF COMPILER6_UP}
    varSmallint, varInteger];
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
  I, iB: Integer;
begin
  X := -1;
  Y := -1;
  iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #10 then
      begin
        Inc(Y);
        iB := I + 1;
      end;
      Inc(I);
    end;
    X := Pos - iB;
  end;
end;

procedure GetXYByPosW(const S: WideString; const Pos: Integer; var X, Y: Integer);
var
  I, iB: Integer;
begin
  X := -1;
  Y := -1;
  iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #10 then
      begin
        Inc(Y);
        iB := I + 1;
      end;
      Inc(I);
    end;
    X := Pos - iB;
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
    if S[I] in Separators then
      Break;
  Beg := I + 1;
  for I := P to Length(S) do
    if S[I] in Separators then
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
  if (S[P] in Separators) and ((P < 1) or (S[P - 1] in Separators)) then
    Inc(P);
  iBeg := P;
  while iBeg >= 1 do
    if S[iBeg] in Separators then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then
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
  if (CharInSetW(S[P], Separators)) and
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
    if S[P] in Separators then
      if (P < 1) or ((P - 1 > 0) and (S[P - 1] in Separators)) then
        Inc(iBeg)
      else
      if not ((P - 1 > 0) and (S[P - 1] in Separators)) then
        Dec(iBeg);
  while iBeg >= 1 do
    if S[iBeg] in Separators then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if S[iEnd] in Separators then
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
  if (Text[StartIndex] in Separators) and
    ((StartIndex < 1) or (Text[StartIndex - 1] in Separators)) then
    Inc(StartIndex);
  iBeg := StartIndex;
  while iBeg >= 1 do
    if Text[iBeg] in Separators then
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := StartIndex;
  while iEnd <= Len do
    if Text[iEnd] in Separators then
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
  while (iEnd <= Len) and (not (Text[iEnd] in Separators)) do
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
  while (iEnd <= Len) and (not CharInSetW(Text[iEnd], Separators)) do
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

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
{ Returns a substring. Substrings are divided by Sep character [translated] }
var
  I: Integer;
  PB, PE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  PB := PChar(S);
  for I := 1 to Index do
  begin
    PB := StrPos(PB, PChar(Separator));
    if PB = nil then
      Exit;
    PB := PB + Length(Separator);
    if PB[0] = #0 then
      Exit;
  end;
  PE := StrPos(PB + 1, PChar(Separator));
  if PE = nil then
    PE := PChar(S) + Length(S);
  if not (AnsiStrLIComp(PB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, PB, PE - PB);
end;

function SubStrW(const S: WideString; const Index: Integer; const Separator: WideString): WideString;
{ Returns a substring. Substrings are divided by Sep character [translated] }
var
  I: Integer;
  PB, PE: PWideChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  PB := PWideChar(S);
  for I := 1 to Index do
  begin
    PB := StrPosW(PB, PWideChar(Separator));
    if PB = nil then
      Exit;
    PB := PB + Length(Separator);
    if PB[0] = #0 then
      Exit;
  end;
  PE := StrPosW(PB + 1, PWideChar(Separator));
  if PE = nil then
    PE := PWideChar(S) + Length(S);
  if not (StrLICompW2(PB, PWideChar(Separator), Length(Separator)) = 0) then
    SetString(Result, PB, PE - PB);
end;

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
{ The same as SubStr, but substrings are numbered from the end [translated]}
var
  MaxIndex: Integer;
  PB: PChar;
begin
  { Not optimal implementation [translated] }
  MaxIndex := 0;
  PB := StrPos(PChar(S), PChar(Separator));
  while PB <> nil do
  begin
    Inc(MaxIndex);
    PB := StrPos(PB + Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - Index, Separator);
end;

function SubWord(P: PChar; var P2: PChar): string;
var
  I: Integer;
begin
  I := 0;
  while not (P[I] in Separators) do
    Inc(I);
  SetString(Result, P, I);
  P2 := P + I;
end;

function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
var
  LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do
  begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + NewPattern + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function ReplaceStringW(S: WideString; const OldPattern, NewPattern: WideString): WideString;
var
  LW: Integer;
  P: PWideChar;
  Sm: Integer;
begin
  LW := Length(OldPattern);
  P := StrPosW(PWideChar(S), PWideChar(OldPattern));
  while P <> nil do
  begin
    Sm := P - PWideChar(S);
    S := Copy(S, 1, Sm) + NewPattern + Copy(S, Sm + LW + 1, Length(S));
    P := StrPosW(PWideChar(S) + Sm + Length(NewPattern), PWideChar(OldPattern));
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
  with R do
    SetRect(Result, Left - I, Top - I, Right + I, Bottom + I);
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
  AddSlash(APath);
  APath := Concat(APath, AllFilesMask);
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
  AddSlash(APath);
  APath := Concat(APath, AllFilesMask);
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

{$IFDEF COMPILER5}
function DirectoryExists(const Name: string): Boolean;
var
  Code: Cardinal;
begin
  Code := Integer(GetFileAttributes(PChar(Name)));
  Result := (Code <> $FFFFFFFF) and (Code and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

procedure ForceDirectories(Dir: string);
begin
  if Dir[Length(Dir)] = PathDelim then
    Delete(Dir, Length(Dir), 1);
  if (Length(Dir) < 3) or DirectoryExists(Dir) or (ExtractFilePath(Dir) = Dir) then
    Exit; { avoid 'xyz:\' problem.}
  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

function SameFileName(const FN1, FN2: string): Boolean;
begin
  Result := CompareText(FN1, FN2) = 0;
end;
{$ENDIF COMPILER5}

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
  Ins: Integer;
  LZCopy: TLZCopy;
  LZOpenFile: TLZOpenFile;
  LZClose: TLZClose;
begin
  Result := False;
  Ins := LoadLibrary('LZ32.dll');
  try
    LZCopy := GetProcAddress(Ins, 'LZCopy');
    LZOpenFile := GetProcAddress(Ins, 'LZOpenFileA');
    LZClose := GetProcAddress(Ins, 'LZClose');
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
    FreeLibrary(Ins);
  end;
end;
{$ENDIF MSWINDOWS}

procedure Dos2Win(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$80..#$AF:
        S[I] := Char(Byte(S[I]) + (192 - $80));
      #$E0..#$EF:
        S[I] := Char(Byte(S[I]) + (240 - $E0));
    end;
end;

procedure Win2Dos(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$C0..#$EF:
        S[I] := Char(Byte(S[I]) - (192 - $80));
      #$F0..#$FF:
        S[I] := Char(Byte(S[I]) - (240 - $E0));
    end;
end;

function Dos2WinRes(const S: string): string;
begin
  Result := S;
  Dos2Win(Result);
end;

function Win2DosRes(const S: string): string;
begin
  Result := S;
  Win2Dos(Result);
end;

function Win2Koi(const S: string): string;
const
  W = '‡·‚„‰Â∏ÊÁËÈÍÎÏÌÓÔÒÚÛÙı˜ˆ¯˘¸˚˙˝˛ˇ¿¡¬√ƒ≈®∆«»… ÀÃÕŒœ–—“”‘’◊÷ÿŸ‹€⁄›ﬁﬂ';
  K = '¡¬◊«ƒ≈£÷⁄… ÀÃÕŒœ–“”‘’∆»ﬁ√€›ÿŸﬂ‹¿—·‚˜Á‰Â≥ˆ˙ÈÍÎÏÌÓÔÚÛÙıÊË˛„˚˝¯˘ˇ¸‡Ò';
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

function Spaces(const N: Integer): string;
begin
  if N > 0 then
  begin
    SetLength(Result, N);
    FillChar(Result[1], N, ' ');
  end
  else
    Result := '';
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

function AddSpaces(const S: string; const N: Integer): string;
var
  Len: Integer;
begin
  Len := Length(S);
  if (Len < N) and (N > 0) then
  begin
    SetLength(Result, N);
    Move(S[1], Result[1], Len * SizeOf(Char));
    FillChar(Result[Len + 1], N - Len, ' ');
  end
  else
    Result := S;
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
  Day: array [1..3] of PChar =
    ('‰ÂÌ¸', '‰Ìˇ', '‰ÌÂÈ'); // Day, Days, Days
  Month: array [1..3] of PChar =
    ('ÏÂÒˇˆ', 'ÏÂÒˇˆ‡', 'ÏÂÒˇˆÂ‚'); // Month, Months, Months
  Year: array [1..3] of PChar =
    ('„Ó‰', '„Ó‰‡', 'ÎÂÚ'); // Year, Years, Years
  Week: array [1..4] of PChar =
    ('ÌÂ‰ÂÎ˛', '2 ÌÂ‰ÂÎË', '3 ÌÂ‰ÂÎË', 'ÏÂÒˇˆ'); // Week, 2 Weeks, 3 Weeks, Month
var
  Y, M, D: Integer;
begin                      
  if Date = Dat then
    Result := 'ÒÂ„Ó‰Ìˇ' // Today
  else
  if Dat = Date - 1 then
    Result := '‚˜Â‡' // Yesterday
  else
  if Dat = Date - 2 then
    Result := 'ÔÓÁ‡‚˜Â‡' // Day before yesterday
  else
  if Dat > Date then
    Result := '‚ ·Û‰Û˘ÂÏ' // In the future
  else
  begin
    D := Trunc(Date - Dat);
    Y := Round(D / 365);
    M := Round(D / 30);
    if Y > 0 then
      Result := IntToStr(Y) + ' ' + Year[D2D[StrToInt(IntToStr(Y)[Length(IntToStr(Y))])]] + ' Ì‡Á‡‰' // ago
    else
    if M > 0 then
      Result := IntToStr(M) + ' ' + Month[D2D[StrToInt(IntToStr(M)[Length(IntToStr(M))])]] + ' Ì‡Á‡‰' // ago
    else
    if D > 6 then
      Result := Week[D div 7] + ' Ì‡Á‡‰' // ago
    else
    if D > 0 then
      Result := IntToStr(D) + ' ' + Day[D2D[StrToInt(IntToStr(D)[Length(IntToStr(D))])]] + ' Ì‡Á‡‰' // ago
  end;
end;

procedure AddSlash(var Dir: TFileName);
begin
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> PathDelim) then
    Dir := Dir + PathDelim;
end;

function AddSlash2(const Dir: TFileName): string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> PathDelim) then
    Result := Dir + PathDelim;
end;

function AddPath(const FileName, Path: TFileName): TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
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
  S := SubStr(PathList, I, PathSep);
  while S <> '' do
  begin
    Result := ConcatSep(Result, AddPath(S, Path), PathSep);
    Inc(I);
    S := SubStr(PathList, I, PathSep);
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
  S := SubStr(PathList, I, PathSep);
  while S <> '' do
  begin
    Result := AddSlash2(S) + FileName;
    if FileExists(Result) then
      Exit;
    Inc(I);
    S := SubStr(PathList, I, PathSep);
  end;
  Result := '';
end;

function GetComputerID: string;
{$IFDEF MSWINDOWS}
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
begin
  Result := 'None';
end;
{$ENDIF UNIX}

function CurrencyToStr(const Cur: Currency): string;
begin
  Result := CurrToStrF(Cur, ffCurrency, CurrencyDecimals)
end;

function Cmp(const S1, S2: string): Boolean;
begin
  Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0;
end;

function StringCat(var S1: string; S2: string): string;
begin
  S1 := S1 + S2;
  Result := S1;
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
    Result := Cmp(ParamStr(I), Param);
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
      Result := Cmp(Copy(ParamStr(I), 2, Length(Param)), Param);
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
      Cmp(Copy(ParamStr(I), 2, Length(Param)), Param) then
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

function CharInSet(const Ch: Char; const SetOfChar: TSysCharSet): Boolean;
begin
  Result := Ch in SetOfChar;
end;

function CharInSetW(const Ch: WideChar; const SetOfChar: TSysCharSet): Boolean;
begin
  if Word(Ch) > 255 then
    Result := False
  else
    Result := Char(Ch) in SetOfChar;
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

function ChangeTopException(E: TObject): TObject;
type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    //ExceptionRecord: PExceptionRecord;
  end;
begin
  { C++ Builder 3 Warning !}
  { if linker error occured with message "unresolved external 'System::RaiseList'" try
    comment this function implementation, compile,
    then uncomment and compile again. }
  {$IFDEF MSWINDOWS}
  {$IFDEF SUPPORTS_DEPRECATED}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF SUPPORTS_DEPRECATED}
  if RaiseList <> nil then
  begin
    Result := PRaiseFrame(RaiseList)^.ExceptObject;
    PRaiseFrame(RaiseList)^.ExceptObject := E
  end
  else
    Result := nil;
  {$IFDEF SUPPORTS_DEPRECATED}
  {$WARN SYMBOL_DEPRECATED ON}
  {$ENDIF SUPPORTS_DEPRECATED}
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  // XXX: changing exception in stack frame is not supported on Kylix
  Writeln(ErrOutput, 'ChangeTopException');
  Result := E;
  {$ENDIF UNIX}
end;

{$IFDEF VCL}
function KeyPressed(VK: Integer): Boolean;
begin
  Result := Windows.GetKeyState(VK) and $8000 = $8000;
end;
{$ENDIF VCL}

function Var2Type(V: Variant; const VarType: Integer): Variant;
begin
  if TVarData(V).VType in [varEmpty, varNull] then
  begin
    case VarType of
      varString, varOleStr:
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
      Result := VarAsType(V, VarType);
    end;
  end
  else
    Result := VarAsType(V, VarType);
  if (VarType = varInteger) and (TVarData(V).VType = varBoolean) then
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

{$IFDEF COMPILER5}
function VarIsStr(const V: Variant): Boolean;
var
  VarType: TVarType;
  VarData: PVarData;
begin
  VarData := @TVarData(V);
  while VarData.VType = varByRef or varVariant do
    VarData := PVarData(VarData.VPointer);

  VarType := VarData^.VType;
  Result := (VarType = varOleStr) or (VarType = varString);
end;
{$ENDIF COMPILER5}

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
  DestPath := AddSlash2(DestDir);
  AddSlash(Path);
  DosError := FindFirst(Path + AllFilesMask, faAnyFile, SearchRec);
  while DosError = 0 do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
        Result := CopyDir(Path + SearchRec.Name, AddSlash2(DestDir) + SearchRec.Name)
      else
        Result := CopyFile(PChar(Path + SearchRec.Name),
          PChar(DestPath + SearchRec.Name), True);
      if not Result then
        Exit;
    end;
    DosError := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  Result := True;
end;

function FileTimeToDateTime(const FT: TFileTime): TDateTime;
{$IFDEF MSWINDOWS}
var
  LocalFileTime: TFileTime;
  FileDate: Integer;
begin
  FileTimeToLocalFileTime(FT, LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime, LongRec(FileDate).Hi, LongRec(FileDate).Lo);
  Result := FileDateToDateTime(FileDate);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := FileDateToDateTime(FT);
end;
{$ENDIF UNIX}

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
  {$IFDEF COMPILER6_UP}
  Result := SysUtils.WideCompareText(P1, P2);
  {$ELSE}
  Result := WideCompareText(P1, P2);
  {$ENDIF COMPILER6_UP}
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

{$IFDEF COMPILER5}

function WideCompareText(const S1, S2: WideString): Integer;
begin
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    Result := CompareText(string(S1), string(S2))
  else
    Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) - 2;
end;

function WideUpperCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    CharUpperBuffW(Pointer(Result), Length(Result));
end;

function WideLowerCase(const S: WideString): WideString;
begin
  Result := S;
  if Result <> '' then
    CharLowerBuffW(Pointer(Result), Length(Result));
end;

{$ENDIF COMPILER}

function TrimW(const S: WideString): WideString;
{$IFDEF COMPILER6_UP}
begin
  Result := Trim(S);
end;
{$ELSE}
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;
{$ENDIF COMPILER6_UP}

function TrimLeftW(const S: WideString): WideString;
{$IFDEF COMPILER6_UP}
begin
  Result := TrimLeft(S);
end;
{$ELSE}
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;
{$ENDIF COMPILER6_UP}

function TrimRightW(const S: WideString): WideString;
{$IFDEF COMPILER6_UP}
begin
  Result := TrimRight(S);
end;
{$ELSE}
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;
{$ENDIF COMPILER6_UP}

procedure SetDelimitedText(List: TStrings; const Text: string; Delimiter: Char);
var
  {$IFDEF COMPILER6_UP}
  Ch: Char;
  {$ELSE}
  S: string;
  F, P: PChar;
  {$ENDIF COMPILER6_UP}
begin
  {$IFDEF COMPILER6_UP}
  Ch := List.Delimiter;
  try
    List.Delimiter := Delimiter;
    List.DelimitedText := Text;
  finally
    List.Delimiter := Ch;
  end;
  {$ELSE}
  List.BeginUpdate;
  try
    List.Clear;
    P := PChar(Text);
    while P^ in [#1..#32] do
      Inc(P);
    while P^ <> #0 do
    begin
      if P^ = '"' then
      begin
        F := P;
        while (P[0] <> #0) and (P[0] <> '"') do
          Inc(P);
        SetString(S, F, P - F);
      end
      else
      begin
        F := P;
        while not (P[0] < #32) and (P[0] <> Delimiter) do
          Inc(P);
        SetString(S, F, P - F);
      end;
      List.Add(S);
      while P[0] in [#1..#32] do
        Inc(P);
      if P[0] = Delimiter then
      begin
        F := P;
        Inc(F);
        if F[0] = #0 then
          List.Add('');
        repeat
          Inc(P);
        until not (P[0] in [#1..#32]);
      end;
    end;
  finally
    List.EndUpdate;
  end;
  {$ENDIF COMPILER6_UP}
end;

{$IFDEF COMPILER5}

function Sign(const AValue: Integer): TValueSign;
begin
  if AValue < 0 then
    Result := NegativeValue
  else
  if AValue > 0 then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function Sign(const AValue: Int64): TValueSign;
begin
  if AValue < 0 then
    Result := NegativeValue
  else
  if AValue > 0 then
    Result := PositiveValue
  else
    Result := ZeroValue;
end;

function Sign(const AValue: Double): TValueSign;
begin
  if (PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000 then
    Result := ZeroValue
  else
  if (PInt64(@AValue)^ and $8000000000000000) = $8000000000000000 then
    Result := NegativeValue
  else
    Result := PositiveValue;
end;

{$ENDIF COMPILER5}

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

function StrToBool(const S: string): Boolean;
begin
  Result := (S = '1') or Cmp(S, 'True') or Cmp(S, 'yes');
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
    Result := FindFirst(AddSlash2(Folder) + Mask, faAnyFile, SearchRec);
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
    Result := FindFirst(AddSlash2(Folder) + AllFilesMask, faAnyFile, SearchRec);
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
      if S[I] in Separators then
        Break;
    Beg := I + 1;
    for Ent := PosBeg to LS do
      if S[Ent] in Separators then
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

function ReplaceAllStrings(S: string; Words, Frases: TStrings): string;
var
  I, LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  for I := 0 to Words.Count - 1 do
  begin
    LW := Length(Words[I]);
    P := StrPos(PChar(S), PChar(Words[I]));
    while P <> nil do
    begin
      Sm := P - PChar(S);
      S := Copy(S, 1, Sm) + Frases[I] + Copy(S, Sm + LW + 1, Length(S));
      P := StrPos(PChar(S) + Sm + Length(Frases[I]), PChar(Words[I]));
    end;
  end;
  Result := S;
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

procedure DeleteEmptyLines(Ss: TStrings);
var
  I: Integer;
begin
  Ss.BeginUpdate;
  try
    for I := Ss.Count - 1 downto 0 do
      if Trim(Ss[I]) = '' then
        Ss.Delete(I);
  finally
    Ss.EndUpdate;
  end;
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
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
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

  function Cmp(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
  end;

  function Cmp1(M1: string): Boolean;
  begin
    Result := AnsiStrLIComp(PChar(Text) + I, PChar(M1), Length(M1)) = 0;
    if Result then
      Inc(I, Length(M1));
  end;

  function CmpL(M1: string): Boolean;
  begin
    Result := Cmp(M1 + '>');
  end;

  function CmpL1(M1: string): Boolean;
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
          CL := SubStr(PChar(Text) + I, 0, '>');
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
    for I := 0 to List.Count - 1 do
      TObject(List[I]).Free;
    List.Clear;
  end;
end;

procedure MemStreamToClipBoard(MemStream: TMemoryStream; const Format: Word);
{$IFDEF VCL}
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
{$ENDIF VCL}
{$IFDEF VisualCLX}
var
  Position: Integer;
begin
  Position := MemStream.Position;
  try
    MemStream.Position := 0;
    Clipboard.SetFormat(SysUtils.Format('Stream#%d', [Format]), MemStream);
  finally
    MemStream.Position := Position;
  end;
end;
{$ENDIF VisualCLX}

procedure ClipBoardToMemStream(MemStream: TMemoryStream; const Format: Word);
{$IFDEF VCL}
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
{$ENDIF VCL}
{$IFDEF VisualCLX}
begin
  if Clipboard.Provides(SysUtils.Format('Stream#%d', [Format])) then
  begin
    Clipboard.GetFormat(SysUtils.Format('Stream#%d', [Format]), MemStream);
    MemStream.Position := 0;
  end;
end;
{$ENDIF VisualCLX}

function GetPropType(Obj: TObject; const PropName: string): TTypeKind;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    Result := tkUnknown
  else
    Result := PropInf^.PropType^.Kind;
end;

function GetPropStr(Obj: TObject; const PropName: string): string;
var
  PropInf: PPropInfo;
begin
  PropInf := GetPropInfo(Obj.ClassInfo, PropName);
  if PropInf = nil then
    raise Exception.CreateResFmt(@RsEPropertyNotExists, [PropName]);
  if not (PropInf^.PropType^.Kind in
    [tkString, tkLString, tkWString]) then
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
  if not (PropInf^.PropType^.Kind in
    [tkInteger, tkChar, tkWChar, tkEnumeration, tkClass]) then
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
  if not (PropInf^.PropType^.Kind = tkMethod) then
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
      if (S = '') or (S[1] in [';', '#']) then
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

{$IFDEF VCL}
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
      with Result.Canvas do
      begin
        Brush.Color := BackColor;
        FillRect(Rect(0, 0, W, H));
        DrawIconEx(Handle, 0, 0, Ico, W, H, 0, 0, DI_NORMAL);
      end;
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

{$ENDIF VCL}

{$IFDEF VisualCLX}

type
  TIconAccessProtected = class(TIcon);

function Bmp2Icon(Bmp: TBitmap): TIcon;
begin
  Result := TIcon.Create;
  Result.Assign(Bmp);
end;

function Icon2Bmp(Ico: TIcon): TBitmap;
begin
  Result := TBitmap.Create;
  TIconAccessProtected(Ico).AssignTo(Result);
end;

procedure CopyIconToClipboard(Icon: TIcon; BackColor: TColor);
var
  Bmp: TBitmap;
begin
  Bmp := Icon2Bmp(Icon);
  Clipboard.Assign(Bmp);
end;

function CreateIconFromClipboard: TIcon;
var
  Bmp: TBitmap;
begin
  Result := nil;
  if not Clipboard.Provides('image/delphi.bitmap') then
    Exit;
  Bmp := TBitmap.Create;
  try
    Bmp.Assign(Clipboard);
    Result := Bmp2Icon(Bmp);
  except
    Bmp.Free;
  end;
end;

{$ENDIF VisualCLX}

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
  raise EOutOfResources.CreateRes(@SOutOfResources);
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
    DC := GetDC(0);
    if DC = NullHandle then
      OutOfResources;
    try
      Result := CreateCompatibleBitmap(DC, Size.X, Size.Y);
      if Result = NullHandle then
        OutOfResources;
    finally
      ReleaseDC(0, DC);
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
  DC := GetDC(0);
  if DC = NullHandle then
    OutOfResources;
  try
    Bits := Pointer(Longint(@BI) + SizeOf(BI) + NumColors * SizeOf(TRGBQuad));
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
      Inc(Longint(Bits), biSizeImage);
      biBitCount := 1;
      biSizeImage := WidthBytes(Longint(biWidth) * biBitCount) * biHeight;
      biClrUsed := 2;
      biClrImportant := 2;
    end;
    Colors := Pointer(Longint(@BI) + SizeOf(BI));
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
    ReleaseDC(0, DC);
  end;
end;

{$IFDEF MSWINDOWS}
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
    DC := GetDC(0);
    if DC = NullHandle then
      OutOfResources;
    try
      BitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if BitsPerPixel = 24 then
        Colors := 0
      else
        Colors := 1 shl BitsPerPixel;
    finally
      ReleaseDC(0, DC);
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
            XorMem := Pointer(Longint(ResData) + AndLen);
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
{$ENDIF MSWINDOWS}

{$IFDEF VCL}

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

{$ENDIF VCL}

{ end JvIconClipboardUtils }

{ begin JvRLE }

procedure RleCompressTo(InStream, OutStream: TStream);
var
  Count, Count2, Count3, I: Integer;
  Buf1: array [0..1024] of Byte;
  Buf2: array [0..60000] of Byte;
  B: Byte;
begin
  InStream.Position := 0;
  Count := 1024;
  while Count = 1024 do
  begin
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
  end;
end;

procedure RleDecompressTo(InStream, OutStream: TStream);
var
  Count, Count2, Count3, I: Integer;
  Buf1: array [0..1024] of Byte;
  Buf2: array [0..60000] of Byte;
  B: Byte;
begin
  InStream.Position := 0;
  Count := 1024;
  while Count = 1024 do
  begin
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
  end;
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

function DaysPerMonth(AYear, AMonth: Integer): Integer;
const
  DaysInMonth: array [1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then
    Inc(Result); { leap-year Feb is special }
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
    (S[I] in ['0'..'9']) and (N < 1000) do
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
  while (Pos <= Length(S)) and not (S[Pos] in ['0'..'9']) do
  begin
    if S[Pos] in LeadBytes then
      Inc(Pos);
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
  Result := DefaultDateOrder; { default }
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

function ScanDate(const S, DateFormat: string; var Pos: Integer;
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
  if ShortDateFormat[1] = 'g' then { skip over prefix text }
    ScanToNumber(S, Pos);
  if not (ScanNumber(S, MaxInt, Pos, N1) and ScanChar(S, Pos, DateSeparator) and
    ScanNumber(S, MaxInt, Pos, N2)) then
    Exit;
  if ScanChar(S, Pos, DateSeparator) then
  begin
    if not ScanNumber(S, MaxInt, Pos, N3) then
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
  ScanChar(S, Pos, DateSeparator);
  ScanBlanks(S, Pos);
  if SysLocale.FarEast and (System.Pos('ddd', ShortDateFormat) <> 0) then
  begin { ignore trailing text }
    if ShortTimeFormat[1] in ['0'..'9'] then { stop at time digit }
      ScanToNumber(S, Pos)
    else { stop at time prefix }
      repeat
        while (Pos <= Length(S)) and (S[Pos] <> ' ') do
          Inc(Pos);
        ScanBlanks(S, Pos);
      until (Pos > Length(S)) or
        AnsiSameText(TimeAMString, Copy(S, Pos, Length(TimeAMString))) or
        AnsiSameText(TimePMString, Copy(S, Pos, Length(TimePMString)));
  end;
  Result := IsValidDate(Y, M, D) and (Pos > Length(S));
end;

function MonthFromName(const S: string; MaxLen: Byte): Byte;
begin
  if Length(S) > 0 then
    for Result := 1 to 12 do
    begin
      if (Length(LongMonthNames[Result]) > 0) and
        AnsiSameText(Copy(S, 1, MaxLen),
        Copy(LongMonthNames[Result], 1, MaxLen)) then
        Exit;
    end;
  Result := 0;
end;

procedure ExtractMask(const Format, S: string; Ch: Char; Cnt: Integer;
  var I: Integer; Blank, Default: Integer);
var
  Tmp: string[20];
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

{$IFDEF COMPILER5}
function TryStrToDateTime(const S: string; out Date: TDateTime): Boolean;
begin
  Result := True;
  try
    Date := StrToDateTime(S);
  except
    Result := False;
  end;
end;
{$ENDIF COMPILER5}

function StrToDateFmt(const DateFormat, S: string): TDateTime;
begin
  if not InternalStrToDate(DateFormat, S, Result) then
    raise EConvertError.CreateResFmt(@SInvalidDate, [S]);
end;

function StrToDateDef(const S: string; Default: TDateTime): TDateTime;
begin
  if not InternalStrToDate(ShortDateFormat, S, Result) then
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
    case GetDateOrder(ShortDateFormat) of
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
    case GetDateOrder(ShortDateFormat) of
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
    case GetDateOrder(ShortDateFormat) of
      doMDY, doDMY:
        Result := '!99/99/9999;1;';
      doYMD:
        Result := '!9999/99/99;1;';
    end;
  end
  else
  begin
    case GetDateOrder(ShortDateFormat) of
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

function FourDigitYear: Boolean;
begin
  Result := IsFourDigitYear;
end;

function IsFourDigitYear: Boolean;
begin
  Result := Pos('YYYY', AnsiUpperCase(ShortDateFormat)) > 0;
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

function iconversion(InP: PChar; OutP: Pointer; InBytes, OutBytes: Cardinal;
  const ToCode, FromCode: string): Boolean;
var
  conv: iconv_t;
begin
  Result := False;
  if (InBytes > 0) and (OutBytes > 0) and (InP <> nil) and (OutP <> nil) then
  begin
    conv := iconv_open(PChar(ToCode), PChar(FromCode));
    if Integer(conv) <> -1 then
    begin
      if Integer(iconv(conv, InP, InBytes, OutP, OutBytes)) <> -1 then
        Result := True;
      iconv_close(conv);
    end;
  end;
end;

function iconvString(const S, ToCode, FromCode: string): string;
begin
  SetLength(Result, Length(S));
  if not iconversion(PChar(S), Pointer(Result),
    Length(S), Length(Result),
    ToCode, FromCode) then
    Result := S;
end;

function iconvWideString(const S: WideString; const ToCode, FromCode: string): WideString;
begin
  SetLength(Result, Length(S));
  if not iconversion(Pointer(S), Pointer(Result),
    Length(S) * SizeOf(WideChar), Length(Result) * SizeOf(WideChar),
    ToCode, FromCode) then
    Result := S;
end;

function OemStrToAnsi(const S: string): string;
begin
  Result := iconvString(S, 'WINDOWS-1252', 'CP850');
end;

function AnsiStrToOem(const S: string): string;
begin
  Result := iconvString(S, 'CP850', 'WINDOWS-1250');
end;

{$ENDIF UNIX}

function StrToOem(const AnsiStr: string): string;
begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, Length(AnsiStr));
  if Length(Result) > 0 then
    CharToOemBuff(PChar(AnsiStr), PChar(Result), Length(Result));
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Result := AnsiStrToOem(AnsiStr);
  {$ENDIF UNIX}
end;

function OemToAnsiStr(const OemStr: string): string;
begin
  {$IFDEF MSWINDOWS}
  SetLength(Result, Length(OemStr));
  if Length(Result) > 0 then
    OemToCharBuff(PChar(OemStr), PChar(Result), Length(Result));
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
    if not (S[I] in EmptyChars) then
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
    FillChar(Result[1], Length(Result), C);
  end;
end;

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
    while (I <= SLen) and (Result[I] in WordDelims) do
      Inc(I);
    if I <= SLen then
      Result[I] := AnsiUpperCase(Result[I])[1];
    while (I <= SLen) and not (Result[I] in WordDelims) do
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
    while (I <= SLen) and (S[I] in WordDelims) do
      Inc(I);
    if I <= SLen then
      Inc(Result);
    while (I <= SLen) and not (S[I] in WordDelims) do
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
    while (I <= Length(S)) and (S[I] in WordDelims) do
      Inc(I);
    { if we're not beyond end of S, we're at the start of a word }
    if I <= Length(S) then
      Inc(Count);
    { if not finished, find the end of the current word }
    if Count <> N then
      while (I <= Length(S)) and not (S[I] in WordDelims) do
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
    while (I <= Length(S)) and not (S[I] in WordDelims) do
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
    while (I <= Length(S)) and not (S[I] in WordDelims) do
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
    if S[I] in Delims then
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
  while (I <= Length(S)) and not (S[I] in Delims) do
    Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and (S[I] in Delims) then
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
var
  P: PChar;
begin
  P := PChar(S);
  if P^ = Quote then
    Result := AnsiExtractQuotedStr(P, Quote)
  else
    Result := S;
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

function D2H(N: Longint; A: Byte): string;
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

function H2D(const S: string): Longint;
begin
  Result := Hex2Dec(S);
end;

function Dec2Numb(N: Longint; A, B: Byte): string;
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

function Numb2Dec(S: string; B: Byte): Longint;
var
  I, P: Longint;
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
    if Index in RomanChars then
    begin
      if Succ(I) <= Length(S) then
        Next := UpCase(S[I + 1])
      else
        Next := #0;
      if (Next in RomanChars) and (RomanValues[Index] < RomanValues[Next]) then
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
      Result[I] := Chr(Byte(Key[1 + ((I - 1) mod Length(Key))]) xor Ord(Src[I]));
end;

function XorEncode(const Key, Source: string): string;
var
  I: Integer;
  C: Byte;
begin
  Result := '';
  for I := 1 to Length(Source) do
  begin
    if Length(Key) > 0 then
      C := Byte(Key[1 + ((I - 1) mod Length(Key))]) xor Byte(Source[I])
    else
      C := Byte(Source[I]);
    Result := Result + AnsiLowerCase(IntToHex(C, 2));
  end;
end;

function XorDecode(const Key, Source: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := '';
  for I := 0 to Length(Source) div 2 - 1 do
  begin
    C := Chr(StrToIntDef('$' + Copy(Source, (I * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C := Chr(Byte(Key[1 + (I mod Length(Key))]) xor Byte(C));
    Result := Result + C;
  end;
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
    if (ASwitchChars = []) or ((S[1] in ASwitchChars) and (Length(S) > 1)) then
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

function StringStartsWith(const Str, SubStr: string): Boolean;
begin
  Result := AnsiStartsText(SubStr, Str);
end;

function StringEndsWith(const Str, SubStr: string): Boolean;
begin
  Result := AnsiEndsText(SubStr, Str);
end;

function ExtractFilePath2(const FileName: string): string;
var
  P, P1, P2, PP: PChar;
begin
  P := PChar(FileName);
  P1 := StrRScan(P, '\');
  P2 := StrRScan(P, '/');
  if P1 <> nil then
    if P2 <> nil then
      if P2 > P1 then
        PP := P2
      else
        PP := P1
    else
      PP := P1
  else
    PP := P2;

  if PP = nil then
    Result := ''
  else
    SetString(Result, P, PP - P + 1);
end;
{ end JvStrUtil }
{ end JvStrUtils }

{ begin JvFileUtil }

function NormalDir(const DirName: string): string;
begin
  Result := DirName;
  {$IFDEF MSWINDOWS}
  if (Result <> '') and
    not (AnsiLastChar(Result)^ in [':', '\']) then
    if (Length(Result) = 1) and (UpCase(Result[1]) in ['A'..'Z']) then
      Result := Result + ':\'
    else
      Result := Result + '\';
  {$ENDIF MSWINDOWS}
end;

{$IFDEF COMPILER5}
function AnsiStartsText(const SubText, Text: string): Boolean;
var
  SubTextLen: Integer;
begin
  SubTextLen := Length(SubText);
  if SubTextLen > Length(Text) then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PChar(Text), SubTextLen, PChar(SubText), SubTextLen) = 2;
end;

function AnsiEndsText(const SubText, Text: string): Boolean;
var
  SubTextStart: Integer;
begin
  SubTextStart := Length(Text) - Length(SubText) + 1;
  if (SubTextStart > 0) and (SubText <> '') and (ByteType(Text, SubTextStart) <> mbTrailByte) then
    Result := AnsiStrIComp(Pointer(SubText), PChar(Pointer(Text)) + SubTextStart - 1) = 0
  else
    Result := False;
end;

function AnsiStartsStr(const SubStr, Str: string): Boolean;
var
  SubStrLen: Integer;
begin
  SubStrLen := Length(SubStr);
  if SubStrLen > Length(Str) then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, 0,
      PChar(Str), SubStrLen, PChar(SubStr), SubStrLen) = 2;
end;

function AnsiEndsStr(const SubStr, Str: string): Boolean;
var
  SubStrStart: Integer;
begin
  SubStrStart := Length(Str) - Length(SubStr) + 1;
  if (SubStrStart > 0) and (SubStr <> '') and (ByteType(Str, SubStrStart) <> mbTrailByte) then
    Result := AnsiStrComp(Pointer(SubStr), PChar(Pointer(Str)) + SubStrStart - 1) = 0
  else
    Result := False;
end;
{$ENDIF COMPILER5}

function RemoveBackSlash(const DirName: string): string;
begin
  Result := DirName;
  if (Length(Result) > 1) and
    (AnsiLastChar(Result)^ = '\') then
    if not ((Length(Result) = 3) and (UpCase(Result[1]) in ['A'..'Z']) and
      (Result[2] = ':')) then
      Delete(Result, Length(Result), 1);
end;

function FileDateTime(const FileName: string): TDateTime;
var
  Age: Longint;
begin
  Age := FileAge(FileName);
  {$IFDEF MSWINDOWS}
  // [roko] -1 is valid FileAge value on Linux
  if Age = -1 then
    Result := NullDate
  else
  {$ENDIF MSWINDOWS}
    Result := FileDateToDateTime(Age);
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
function GetTempFileName(const Prefix: string): string;
var
  P: PChar;
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
  TempDir := GetTempDir;
  if FileName <> '' then
  begin
    if Length(FileName) < 4 then
      FileName := ExpandFileName(FileName);
    if (Length(FileName) > 4) and (FileName[2] = ':') and
      (Length(TempDir) > 4) and
      (AnsiCompareFileName(TempDir, FileName) <> 0) then
    begin
      STempDir := ExtractFilePath(FileName);
      Move(STempDir[1], TempDir, Length(STempDir) + 1);
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

function GetTempDir: string;
{$IFDEF MSWINDOWS}
var
  TempDir: array [0..MAX_PATH] of Char;
begin
  TempDir[GetTempPath(260, TempDir)] := #0;
  Result := TempDir;
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  Result := ExtractFileDir(GetTempFileName(''));
  if Result = '' then
    Result := '/tmp'; // hard coded
end;
{$ENDIF UNIX}

function ClearDir(const Dir: string): Boolean;
var
  SearchRec: TSearchRec;
  DosError: Integer;
  Path: TFileName;
begin
  Result := True;
  Path := Dir;
  AddSlash(Path);
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
  Path := AddSlash2(Folder);
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
  P: PChar;
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
  I := 1;
  P := PChar(FileName);
  while I <= Length(Mask) do
  begin
    C := Mask[I];
    if (P[0] = #0) and (C <> '*') then
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
          P := StrScan(P, Mask[I + 1]);
          if P = nil then
            Exit;
        end;
      '?':
        Inc(P);
    else
      if C = P[0] then
        Inc(P)
      else
        Exit;
    end;
    Inc(I);
  end;
  if P[0] = #0 then
    Result := True;
end;

function FileEquMasks(FileName, Masks: TFileName; CaseSensitive: Boolean): Boolean;
var
  I: Integer;
  Mask: string;
begin
  Result := False;
  I := 0;
  Mask := Trim(SubStr(Masks, I, PathSep));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask, CaseSensitive) then
    begin
      Result := True;
      Break;
    end
    else
    begin
      Inc(I);
      Mask := Trim(SubStr(Masks, I, PathSep));
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
  TempPathPtr := PChar(ShortName);
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
end;

function LongToShortPath(const LongName: string): string;
var
  LastSlash: PChar;
  TempPathPtr: PChar;
begin
  Result := '';
  TempPathPtr := PChar(LongName);
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
  FileNameW: array [0..MAX_PATH] of WideChar;
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
        MultiByteToWideChar(CP_ACP, 0, FileDestPath, -1, FileNameW, MAX_PATH);
        OleCheck(PersistFile.Save(FileNameW, True));
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

{$IFDEF COMPILER5}
{ begin JvXMLDatabase D5 compatiblility functions }

{ TODO -oJVCL -cTODO : Implement these better for D5! }

function StrToDateTimeDef(const S: string; Default: TDateTime): TDateTime;
begin
  // stupid and slow but at least simple
  try
    Result := StrToDateTime(S);
  except
    Result := Default;
  end;
end;

const
  OneMillisecond = 1 / 24 / 60 / 60 / 1000; // as TDateTime

function CompareDateTime(const A, B: TDateTime): Integer;
begin
  if Abs(A - B) < OneMillisecond then
    Result := 0
  else
  if A < B then
    Result := -1
  else
    Result := 1;
end;

{ end JvXMLDatabase D5 compatiblility functions }

procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;

function IncludeTrailingPathDelimiter(const APath: string): string;
begin
  if (Length(APath) > 0) and (APath[Length(APath)] <> PathDelim) then
    Result := APath + PathDelim
  else
    Result := APath;
end;

function ExcludeTrailingPathDelimiter(const APath: string): string;
var
  I: Integer;
begin
  Result := APath;
  I := Length(Result);
  while (I > 0) and (Result[I] = PathDelim) do
    Dec(I);
  SetLength(Result, I);
end;

{$ENDIF COMPILER5}

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
    St := Format({$IFDEF COMPILER6_UP} SOSError {$ELSE} SWin32Error {$ENDIF},
      [LastError, SysErrorMessage(LastError)]);
    if Text <> '' then
      St := Text + ':' + St;
    raise {$IFDEF COMPILER6_UP} EOSError{$ELSE} EWin32Error{$ENDIF}.Create(St);
  end;
end;

procedure Exec(const FileName, Parameters, Directory: string);
var
  Operation: string;
begin
  Operation := 'open';
  {$IFDEF MSWINDOWS}
  ShellExecute(Windows.GetForegroundWindow, PChar(Operation), PChar(FileName), PChar(Parameters), PChar(Directory),
    SW_SHOWNORMAL);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ShellExecute(GetForegroundWindow, PChar(Operation), PChar(FileName), PChar(Parameters), PChar(Directory),
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

procedure ExecuteAndWait(const FileName: string; Visibility: Integer);
{$IFDEF MSWINDOWS}
var
  zAppName: array [0..512] of Char;
  zCurDir: array [0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil, zAppName, nil, nil, False, Create_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  // ignores Visibility
  Libc.system(PChar(Format('kfmclient exec "%s"', [FileName])));
end;
{$ENDIF UNIX}

{$IFDEF VCL}

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
var
  Wnd: HWND;
begin
  Wnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(Wnd, SW_HIDE);
end;

procedure ShowTraybar;
var
  Wnd: HWND;
begin
  Wnd := FindWindow(PChar(RC_ShellName), nil);
  ShowWindow(Wnd, SW_SHOW);
end;

procedure HideStartBtn(Visible: Boolean);
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

procedure ShowStartButton;
begin
  HideStartBtn(True);
end;

procedure HideStartButton;
begin
  HideStartBtn(False);
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

procedure SendShift(H: HWND; Down: Boolean);
var
  VKey, ScanCode: Word;
  LParam: Cardinal;
begin
  VKey := VK_SHIFT;
  ScanCode := MapVirtualKey(VKey, 0);
  LParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    LParam := LParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, VKey, LParam);
end;

procedure SendCtrl(H: HWND; Down: Boolean);
var
  VKey, ScanCode: Word;
  LParam: Cardinal;
begin
  VKey := VK_CONTROL;
  ScanCode := MapVirtualKey(VKey, 0);
  LParam := Longint(ScanCode) shl 16 or 1;
  if not Down then
    LParam := LParam or $C0000000;
  SendMessage(H, WM_KEYDOWN, VKey, LParam);
end;

function SendKey(const AppName: string; Key: Char): Boolean;
var
  VKey, ScanCode: Word;
  ConvKey: Longint;
  LParam: Cardinal;
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
    LParam := Longint(ScanCode) shl 16 or 1;
    if Shift then
      SendShift(H, True);
    if Ctrl then
      SendCtrl(H, True);
    SendMessage(H, WM_KEYDOWN, VKey, LParam);
    SendMessage(H, WM_CHAR, VKey, LParam);
    LParam := LParam or $C0000000;
    SendMessage(H, WM_KEYUP, VKey, LParam);
    if Shift then
      SendShift(H, False);
    if Ctrl then
      SendCtrl(H, False);
    Result := True;
  end
  else
    Result := False;
end;

{$ENDIF VCL}

{$IFDEF MSWINDOWS}

procedure RebuildIconCache;
var
  Dummy: DWORD;
begin
  SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS,
    Longint(PChar('WindowMetrics')), SMTO_NORMAL or SMTO_ABORTIFHUNG, 10000, Dummy);
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
{$IFDEF VCL}
var
  Path: string;
  T: TSearchRec;
  Res: Integer;
{$ENDIF VCL}
begin
  Result := TStringList.Create;
  {$IFDEF VCL}
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
  {$ENDIF VCL}
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
    EnumWindows(@EnumWindowsProc, Integer(List));
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
  Result := CharIsDigit(Ch) or (Ch = AnsiSpace) or (Ch = '$') or (Ch = '-') or
    (Pos(Ch, CurrencyString) > 0);
end;

function StrToCurrDef(const Str: string; Def: Currency): Currency;
var
  LStr: string;
  I: Integer;
begin
  LStr := '';
  for I := 1 to Length(Str) do
    if Str[I] in ['0'..'9','-','+', DecimalSeparator] then
      LStr := LStr + Str[I];
  try
    if not TextToFloat(PChar(LStr), Result, fvCurrency) then
      Result := Def;
  except
    Result := Def;
  end;
end;

function StrToFloatDef(const Str: string; Def: Extended): Extended;
var
  LStr: string;
  I: Integer;
begin
  for I := 1 to Length(Str) do
    if Str[I] in ['0'..'9','-','+', DecimalSeparator] then
      LStr := LStr + Str[I];
  Result := Def;
  if LStr <> '' then
  try
    { the string '-' fails StrToFloat, but it can be interpreted as 0  }
    if LStr[Length(LStr)] = '-' then
      LStr := LStr + '0';

    { a string that ends in a '.' such as '12.' fails StrToFloat,
     but as far as I am concerned, it may as well be interpreted as 12.0 }
    if LStr[Length(LStr)] = DecimalSeparator then
      LStr := LStr + '0';
    if not TextToFloat(PChar(LStr), Result, fvExtended) then
      Result := Def;
  except
    Result := Def;
  end;
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
    Ch := S[I];
    if (not CharIsNumber(Ch)) or (Ch = DecimalSeparator) then //Az
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

    if Ch = DecimalSeparator then
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
    if Ch = AnsiForwardSlash then
      Inc(liSlashes)
    else
    if Ch = AnsiSpace then
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

  for I := Low(LongMonthNames) to High(LongMonthNames) do
    Ps := LStrReplace(Ps, LongMonthNames[I], IntToStr(I), False);

  { now that 'January' is gone, catch 'Jan' }
  for I := Low(ShortMonthNames) to High(ShortMonthNames) do
    Ps := LStrReplace(Ps, ShortMonthNames[I], IntToStr(I), False);

  { remove redundant spaces }
  Ps := LStrReplace(Ps, AnsiSpace + AnsiSpace, AnsiSpace, False);

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
  B: string;
  R: TRect;
begin
  B := FileName;
  UniqueString(B);
  R := Rect(0, 0, MaxLen, Canvas.TextHeight('Wq'));
  if DrawText(Canvas, PChar(B), Length(B), R,
       DT_SINGLELINE or DT_MODIFYSTRING or DT_PATH_ELLIPSIS or DT_CALCRECT or
       DT_NOPREFIX) > 0 then
    Result := string(PChar(B))
  else
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
  S := Format('rundll32.exe %s,%s %s', [ModuleName, FuncName, CmdLine]);
  Result := CreateProcess(nil, PChar(S), nil, nil, False, 0, nil, nil, SI, PI);
  try
    if WaitForCompletion then
      Result := WaitForSingleObject(PI.hProcess, INFINITE) <> WAIT_FAILED;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;
end;

procedure RunDll32Internal(Wnd: HWND; const DLLName, FuncName, CmdLine: string; CmdShow: Integer = SW_SHOWDEFAULT);
var
  H: THandle;
  ErrMode: Cardinal;
  P: TRunDLL32Proc;
begin
  ErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOOPENFILEERRORBOX);
  H := LoadLibrary(PChar(DLLName));
  try
    if H <> INVALID_HANDLE_VALUE then
    begin
      P := GetProcAddress(H, PChar(FuncName));
      if Assigned(P) then
        P(Wnd, H, PChar(CmdLine), CmdShow);
    end;
  finally
    SetErrorMode(ErrMode);
    if H <> INVALID_HANDLE_VALUE then
      FreeLibrary(H);
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
  hDLL := LoadLibrary(PChar(DLLName));
  if hDLL < 32 then
    hDLL := 0;
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
      FillChar(Dvi, SizeOf(Dvi), #0);
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
  if LongRec(ResID).Hi = 0 then
    S := IntToStr(LongRec(ResID).Lo)
  else
    S := StrPas(ResID);
  raise EResNotFound.CreateResFmt(@SResNotFound, [S]);
end;

function RectWidth(R: TRect): Integer;
begin
  Result := R.Right - R.Left;
end;

function RectHeight(R: TRect): Integer;
begin
  Result := R.Bottom - R.Top;
end;

{$IFDEF MSWINDOWS}
{ Service routines }

function LoadDLL(const LibName: string): THandle;
var
  ErrMode: Cardinal;
begin
  ErrMode := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  Result := LoadLibrary(PChar(LibName));
  SetErrorMode(ErrMode);
  if Result < HINSTANCE_ERROR then
    OSCheck(False);
end;

function GetWindowsVersion: string;
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
    Result := Trim(Format(sWindowsVersion, [Platfrm, dwMajorVersion,
      dwMinorVersion, dwBuildNumber, szCSDVersion]));
  end;
end;

{ RegisterServer procedure written by Vladimir Gaitanoff, 2:50/430.2 }

function RegisterServer(const ModuleName: string): Boolean;
type
  TCOMFunc = function:HResult;
const
  S_OK    = $00000000;
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
var
  S: array [0..16383] of Char;
begin
  if GetEnvironmentVariable(PChar(VarName), S, SizeOf(S) - 1) > 0 then
    Result := StrPas(S)
  else
    Result := '';
end;

{$ENDIF MSWINDOWS}

{$IFDEF UNIX}
function GetEnvVar(const VarName: string): string;
begin
  Result := getenv(PChar(VarName));
end;
{$ENDIF UNIX}

{ Memory routines }

function AllocMemo(Size: Longint): Pointer;
begin
  if Size > 0 then
    Result := GlobalAllocPtr(HeapAllocFlags or GMEM_ZEROINIT, Size)
  else
    Result := nil;
end;

function ReallocMemo(fpBlock: Pointer; Size: Longint): Pointer;
begin
  Result := GlobalReallocPtr(fpBlock, Size, HeapAllocFlags or GMEM_ZEROINIT);
end;

procedure FreeMemo(var fpBlock: Pointer);
begin
  if fpBlock <> nil then
  begin
    GlobalFreePtr(fpBlock);
    fpBlock := nil;
  end;
end;

function GetMemoSize(fpBlock: Pointer): Longint;
var
  hMem: THandle;
begin
  Result := 0;
  if fpBlock <> nil then
  begin
    hMem := GlobalHandle(fpBlock);
    if hMem <> 0 then
      Result := GlobalSize(hMem);
  end;
end;

function CompareMem(fpBlock1, fpBlock2: Pointer; Size: Cardinal): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,fpBlock1
        MOV     EDI,fpBlock2
        MOV     ECX,Size
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

{ Manipulate huge pointers routines by Ray Lischner, The Waite Group, Inc. }

procedure HugeInc(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) + Amount;
end;

procedure HugeDec(var HugePtr: Pointer; Amount: Longint);
begin
  HugePtr := PChar(HugePtr) - Amount;
end;

function HugeOffset(HugePtr: Pointer; Amount: Longint): Pointer;
begin
  Result := PChar(HugePtr) + Amount;
end;

procedure HMemCpy(DstPtr, SrcPtr: Pointer; Amount: Longint);
begin
  Move(SrcPtr^, DstPtr^, Amount);
end;

procedure HugeMove(Base: Pointer; Dst, Src, Size: Longint);
var
  SrcPtr, DstPtr: PChar;
begin
  SrcPtr := PChar(Base) + Src * SizeOf(Pointer);
  DstPtr := PChar(Base) + Dst * SizeOf(Pointer);
  Move(SrcPtr^, DstPtr^, Size * SizeOf(Pointer));
end;
{ String routines }

{ function GetParamStr copied from SYSTEM.PAS unit of Delphi 2.0 }

function GetParamStr(P: PChar; var Param: string): PChar;
var
  Len: Integer;
  Buffer: array [Byte] of Char;
begin
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then
      Inc(P, 2)
    else
      Break;
  end;
  Len := 0;
  while P[0] > ' ' do
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Buffer[Len] := P[0];
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Buffer[Len] := P[0];
      Inc(Len);
      Inc(P);
    end;
  SetString(Param, Buffer, Len);
  Result := P;
end;

function ParamCountFromCommandLine(CmdLine: PChar): Integer;
var
  S: string;
  P: PChar;
begin
  P := CmdLine;
  Result := 0;
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then
      Break;
    Inc(Result);
  end;
end;

function ParamStrFromCommandLine(CmdLine: PChar; Index: Integer): string;
var
  P: PChar;
begin
  P := CmdLine;
  while True do
  begin
    P := GetParamStr(P, Result);
    if (Index = 0) or (Result = '') then
      Break;
    Dec(Index);
  end;
end;

procedure SplitCommandLine(const CmdLine: string; var ExeName,
  Params: string);
var
  Buffer: PChar;
  Cnt, I: Integer;
  S: string;
begin
  ExeName := '';
  Params := '';
  Buffer := StrPAlloc(CmdLine);
  try
    Cnt := ParamCountFromCommandLine(Buffer);
    if Cnt > 0 then
    begin
      ExeName := ParamStrFromCommandLine(Buffer, 0);
      for I := 1 to Cnt - 1 do
      begin
        S := ParamStrFromCommandLine(Buffer, I);
        if Pos(' ', S) > 0 then
          S := '"' + S + '"';
        Params := Params + S;
        if I < Cnt - 1 then
          Params := Params + ' ';
      end;
    end;
  finally
    StrDispose(Buffer);
  end;
end;

function AnsiUpperFirstChar(const S: string): string;
var
  Temp: string[1];
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

function WindowClassName(Wnd: HWND): string;
var
  Buffer: array [0..255] of Char;
begin
  SetString(Result, Buffer, GetClassName(Wnd, Buffer, SizeOf(Buffer) - 1));
end;

{$IFDEF VCL}

function GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  if SystemParametersInfo(SPI_GETANIMATION, SizeOf(Info), @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

procedure SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(TAnimationInfo);
  BOOL(Info.iMinAnimate) := Value;
  SystemParametersInfo(SPI_SETANIMATION, SizeOf(Info), @Info, 0);
end;

procedure ShowWinNoAnimate(Handle: HWND; CmdShow: Integer);
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

procedure SwitchToWindow(Wnd: HWND; Restore: Boolean);
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

function GetWindowParent(Wnd: HWND): HWND;
begin
  Result := GetWindowLong(Wnd, GWL_HWNDPARENT);
end;

procedure ActivateWindow(Wnd: HWND);
begin
  if Wnd <> 0 then
  begin
    ShowWinNoAnimate(Wnd, SW_SHOW);
    SetForegroundWindow(Wnd);
  end;
end;

{$IFDEF BCB}
function FindPrevInstance(const MainFormClass: ShortString;
  const ATitle: string): HWND;
{$ELSE}
function FindPrevInstance(const MainFormClass, ATitle: string): HWND;
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

function WindowsEnum(Handle: HWND; Param: Longint): BOOL; export; stdcall;
begin
  if WindowClassName(Handle) = 'TAppBuilder' then
  begin
    Result := False;
    PLongint(Param)^ := 1;
  end
  else
    Result := True;
end;

{$IFDEF BCB}
function ActivatePrevInstance(const MainFormClass: ShortString;
  const ATitle: string): Boolean;
{$ELSE}
function ActivatePrevInstance(const MainFormClass, ATitle: string): Boolean;
{$ENDIF BCB}
var
  PrevWnd, PopupWnd, ParentWnd: HWND;
  IsDelphi: Longint;
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
      IsDelphi := 0;
      EnumThreadWindows(GetWindowTask(PrevWnd), @WindowsEnum,
        LPARAM(@IsDelphi));
      if Boolean(IsDelphi) then
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

{$ENDIF VCL}

{$IFDEF MSWINDOWS}
function BrowseForFolderNative(const Handle: HWND; const Title: string; var Folder: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Id: PItemIDList;
  FN: array [0..MAX_PATH] of Char;
begin
  with BrowseInfo do
  begin
    {$IFDEF VCL}
    hwndOwner := Handle;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    hwndOwner := QWidget_WinID(Handle);
    {$ENDIF VisualCLX}
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
  with Rect do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

procedure CenterWindow(Wnd: HWND);
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

{$IFDEF VCL}
{ Delete the requested message from the queue, but throw back }
{ any WM_QUIT msgs that PeekMessage may also return.          }
{ Copied from DbGrid.pas                                      }
procedure KillMessage(Wnd: HWND; Msg: Cardinal);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.WParam);
end;
{$ENDIF VCL}

procedure SetWindowTop(const Handle: HWND; const Top: Boolean);
const
  TopFlag: array [Boolean] of Longword = (HWND_NOTOPMOST, HWND_TOPMOST);
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
    CurrentName := Format(FileNameMask, [I]);
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

type
  // (p3) duplicated from JvTypes to avoid JVCL dependencies
  {$IFDEF VCL}
  TJvRGBTriple = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  TJvRGBTriple = TRGBQuad; // VisualCLX does not support pf24bit
  {$ENDIF VisualCLX}

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

{$IFDEF VCL}

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
      VirtualFree(Bits, 0, MEM_FREE);
      VirtualFree(Header, 0, MEM_FREE);
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

{$ENDIF VCL}

function TextToValText(const AValue: string): string;
var
  I, J: Integer;
begin
  Result := DelRSpace(AValue);
  if DecimalSeparator <> ThousandSeparator then
    Result := DelChars(Result, ThousandSeparator);

  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := ReplaceStr(Result, '.', DecimalSeparator);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := ReplaceStr(Result, ',', DecimalSeparator);

  J := 1;
  for I := 1 to Length(Result) do
    if Result[I] in ['-','+','0'..'9', DecimalSeparator, ThousandSeparator] then
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

{$IFDEF VCL}
function DrawText(Canvas: TCanvas; Text: PAnsiChar; Len: Integer; var R: TRect; WinFlags: Integer): Integer;
begin
  Result := Windows.DrawText(Canvas.Handle, Text, Len, R, WinFlags);
end;

function DrawText(Canvas: TCanvas; const Text: string; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := DrawText(Canvas, PChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
end;

function DrawText(DC: HDC; const Text: TCaption; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := Windows.DrawText(DC, PChar(Text), Len, R, WinFlags and not DT_MODIFYSTRING); // make sure the string cannot be modified
end;

function DrawTextEx(Canvas: TCanvas; lpchText: PChar; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextEx(Canvas.Handle, lpchText, cchText, p4, dwDTFormat, DTParams);
end;

function DrawTextEx(Canvas: TCanvas; const Text: string; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer;
begin
  Result := Windows.DrawTextEx(Canvas.Handle, PChar(Text), cchText, p4, dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;

{$IFDEF COMPILER6_UP}
function DrawText(Canvas: TCanvas; const Text: WideString; Len: Integer; var R: TRect; WinFlags: Integer): Integer; overload;
begin
  Result := DrawTextW(Canvas, Text, Len, R, WinFlags and not DT_MODIFYSTRING);
end;

function DrawTextEx(Canvas: TCanvas; const Text: WideString; cchText: Integer; var p4: TRect; dwDTFormat: UINT; DTParams: PDrawTextParams): Integer; overload;
begin
  Result := DrawTextExW(Canvas, Text, cchText, p4, dwDTFormat and not DT_MODIFYSTRING, DTParams);
end;
{$ENDIF COMPILER6_UP}

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
{$ENDIF VCL}
{$IFDEF VisualCLX}

{ JclQGraphics: Crossplatform versions  }

procedure ScreenShot(Bmp: TBitmap; Left, Top, Width, Height: Integer; Window: QWidgetH); {overload;}
begin
  if not Assigned(Bmp.Handle) then
    Bmp.Handle := QPixmap_create;
  QPixmap_grabWindow(Bmp.Handle, QWidget_winID(Window), Left, Top, Width, Height);
end;

function CreateRegionFromBitmap(Bitmap: TBitmap; RegionColor: TColor;
  RegionBitmapMode: TRegionBitmapMode): QRegionH;
var
  FBitmap: TBitmap;
  X, Y: Integer;
  StartX: Integer;
  Region: QRegionH;
begin
  Result := NullHandle;
  (*
  if Bitmap = nil then
    EJclGraphicsError.CreateResRec(@RsNoBitmapForRegion);
  *)
  if (Bitmap.Width = 0) or (Bitmap.Height = 0) then
    Exit;

  FBitmap := TBitmap.Create;
  try
    FBitmap.Assign(Bitmap);

    for Y := 0 to FBitmap.Height - 1 do
    begin
      X := 0;
      while X < FBitmap.Width do
      begin

        if RegionBitmapMode = rmExclude then
        begin
          while FBitmap.Canvas.Pixels[X,Y] = RegionColor do
          begin
            Inc(X);
            if X = FBitmap.Width then
              Break;
          end;
        end
        else
        begin
          while FBitmap.Canvas.Pixels[X,Y] <> RegionColor do
          begin
            Inc(X);
            if X = FBitmap.Width then
              Break;
          end;
        end;

        if X = FBitmap.Width then
          Break;

        StartX := X;
        if RegionBitmapMode = rmExclude then
        begin
          while FBitmap.Canvas.Pixels[X,Y] <> RegionColor do
          begin
            if X = FBitmap.Width then
              Break;
            Inc(X);
          end;
        end
        else
        begin
          while FBitmap.Canvas.Pixels[X,Y] = RegionColor do
          begin
            if X = FBitmap.Width then
              Break;
            Inc(X);
          end;
        end;
        if Result = NullHandle then
          Result := CreateRectRgn(StartX, Y, X, Y + 1)
        else
        begin
          Region := CreateRectRgn(StartX, Y, X, Y + 1);
          if Region <> NullHandle then
          begin
            CombineRgn(Result, Result, Region, RGN_OR);
            DeleteObject(Region);
          end;
        end;
      end;
    end;
  finally
    FBitmap.Free;
  end;
end;

function FillGradient(DC: QPainterH; ARect: TRect; ColorCount: Integer;
  StartColor, EndColor: TColor; ADirection: TGradientDirection): Boolean;
var
  StartRGB: array [0..2] of Byte;
  RGBKoef: array [0..2] of Double;
  Brush: HBRUSH;
  AreaWidth, AreaHeight, I: Integer;
  ColorRect: TRect;
  RectOffset: Double;
begin
  RectOffset := 0;
  Result := False;
  if ColorCount < 1 then
    Exit;
  StartColor := ColorToRGB(StartColor);
  EndColor := ColorToRGB(EndColor);
  StartRGB[0] := GetRValue(StartColor);
  StartRGB[1] := GetGValue(StartColor);
  StartRGB[2] := GetBValue(StartColor);
  RGBKoef[0] := (GetRValue(EndColor) - StartRGB[0]) / ColorCount;
  RGBKoef[1] := (GetGValue(EndColor) - StartRGB[1]) / ColorCount;
  RGBKoef[2] := (GetBValue(EndColor) - StartRGB[2]) / ColorCount;
  AreaWidth := ARect.Right - ARect.Left;
  AreaHeight :=  ARect.Bottom - ARect.Top;
  case ADirection of
    gdHorizontal:
      RectOffset := AreaWidth / ColorCount;
    gdVertical:
      RectOffset := AreaHeight / ColorCount;
  end;
  for I := 0 to ColorCount - 1 do
  begin
    Brush := CreateSolidBrush(RGB(
      StartRGB[0] + Round((I + 1) * RGBKoef[0]),
      StartRGB[1] + Round((I + 1) * RGBKoef[1]),
      StartRGB[2] + Round((I + 1) * RGBKoef[2])));
    case ADirection of
      gdHorizontal:
        SetRect(ColorRect, Round(RectOffset * I), 0, Round(RectOffset * (I + 1)), AreaHeight);
      gdVertical:
        SetRect(ColorRect, 0, Round(RectOffset * I), AreaWidth, Round(RectOffset * (I + 1)));
    end;
    OffsetRect(ColorRect, ARect.Left, ARect.Top);
    FillRect(DC, ColorRect, Brush);
    DeleteObject(Brush);
  end;
  Result := True;
end;

{$ENDIF VisualCLX}


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

