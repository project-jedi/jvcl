{-------------------------------------------------------------------------------
 QWindows.pas

 Copyright (c) 2003, Andre Snepvangers (asn@xs4all.nl)
 All rights reserved.

 Version 0.4
  Description: Qt based wrappers for common MS Windows API's
  Purpose: Reduce coding effort for porting VCL based components to VisualCLX
           compatible components

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files(the "Software"), to deal in
 the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is furnished
 to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
--------------------------------------------------------------------------------

Last Modified: 2003-12-26

Known Issues:
  - Covers only a small part of the Windows APIs
  - Not all functionality is supported
{-----------------------------------------------------------------------------}

unit QWindows;

interface

uses
  QForms, QControls, QGraphics, QTypes, Qt, QConsts,
  Types, StrUtils, Math, SysUtils, Classes;

const
  { Windows VK_ keycodes to Qt key }
  VK_BACK     = Key_Backspace;
  VK_TAB      = Key_Tab;
  VK_RETURN   = 4100;  // Enter key from keypad
  VK_SHIFT    = Key_Shift;
  VK_CONTROL  = Key_Control;
  VK_MENU     = Key_Alt;
  VK_PAUSE    = Key_Pause;
  VK_CAPITAL  =  Key_CapsLock;
  VK_ESCAPE   = 4096;
  VK_SPACE    = Key_Space;
  VK_PRIOR    = Key_Prior;
  VK_NEXT     = Key_Next;
  VK_END      = Key_End;
  VK_HOME     = Key_Home;
  VK_LEFT     = Key_Left;
  VK_UP       = Key_Up;
  VK_RIGHT    = Key_Right;
  VK_DOWN     = Key_Down;
  { VK_SELECT   = 41; }
  VK_PRINT    = Key_Print;
  { VK_EXECUTE  = 43; }
  VK_SNAPSHOT = Key_Print;
  VK_INSERT   = Key_Insert;
  VK_DELETE   = Key_Delete;
  VK_HELP     = Key_Help;
  { VK_LWIN     = 91; }
  { VK_RWIN     = 92; }
  VK_APPS     = Key_Menu;
  VK_NUMPAD0  = Key_0;
  VK_NUMPAD1  = Key_1;
  VK_NUMPAD2  = Key_2;
  VK_NUMPAD3  = Key_3;
  VK_NUMPAD4  = Key_4;
  VK_NUMPAD5  = Key_5;
  VK_NUMPAD6  = Key_6;
  VK_NUMPAD7  = Key_7;
  VK_NUMPAD8  = Key_8;
  VK_NUMPAD9  = Key_9;
  VK_MULTIPLY = Key_Asterisk;
  VK_ADD      = Key_Plus;
  VK_SUBTRACT = Key_Minus;
  VK_DECIMAL  = Key_Period;
  VK_DIVIDE   = Key_Slash;
  VK_F1       = Key_F1;
  VK_F2       = Key_F2;
  VK_F3       = Key_F3;
  VK_F4       = Key_F4;
  VK_F5       = Key_F5;
  VK_F6       = Key_F6;
  VK_F7       = Key_F7;
  VK_F8       = Key_F8;
  VK_F9       = Key_F1;
  VK_F10      = Key_F10;
  VK_F11      = Key_F11;
  VK_F12      = Key_F12;
  VK_F13      = Key_F13;
  VK_F14      = Key_F14;
  VK_F15      = Key_F15;
  VK_F16      = Key_F16;
  VK_F17      = Key_F17;
  VK_F18      = Key_F18;
  VK_F19      = Key_F19;
  VK_F20      = Key_F20;
  VK_F21      = Key_F21;
  VK_F22      = Key_F22;
  VK_F23      = Key_F23;
  VK_F24      = Key_F24;
  VK_NUMLOCK  = Key_NumLock;
  VK_SCROLL   = Key_ScrollLock;
  { VK_L.. & VK_R.. mapping:                      }
  { Alt, Ctrl and Shift keys produce same keycode }
  VK_LSHIFT   = Key_Shift;
  VK_RSHIFT   = Key_Shift;
  VK_LCONTROL = Key_Control;
  VK_RCONTROL = Key_Control;
  VK_LMENU    = Key_Alt;
  VK_RMENU    = Key_Alt;

  { Qt alignment flags, (as used by Canvas.TextRect) }
  AlignLeft     = $1;
  AlignRight    = $2;
  AlignHCenter  = $4;
  AlignTop      = $8;
  AlignBottom   = $10;
  AlignVCenter  = $20;
  AlignCenter   = $24;
  SingleLine    = $40;
  DontClip      = $80;
  ExpandTabs    = $100;
  ShowPrefix    = $200;
  WordBreak     = $400;
  BreakAnywhere = $800;
  {
  DontPrint = $1000; not used

  Additional constanst for Qt text alignments
  used by DrawText
  }
  QtAlignMask   = $FFF;
  CalcRect      = $1000;
  ClipPath      = $2000;
  ClipName      = $4000;
  ClipToWord    = $10000;
  ModifyString  = $20000;

  pf24bit = pf32bit;
  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue = TColor($F0CAA6);
  clCream = TColor($F0FBFF);
  clMedGray = TColor($A4A0A0);

type
  HWND = QWidgetH;
  HCURSOR = QCursorH;
  HRGN = QRegionH;
  HBRUSH = QBrushH;
  HBITMAP = QPainterH;
  HDC = QPainterH;
  HFONT = QFontH;
  UINT = Cardinal;
  BOOL = LongBool;

  TWinControl = TWidgetControl;
  TControlClass = class of TControl;
  TColorRef = Integer;
  TPointL = record
    X: Longint;
    Y: Longint;
  end;
  TTime = TDateTime;
  TDate = TDateTime;

{
  2 dummies for ... VCL
 asn: AFAIK use of RightToLeft or LeftToRight is automatic
}
function DrawTextBiDiModeFlagsReadingOnly: Longint;
function DrawTextBiDiModeFlags(Flags: Longint): Longint;
{ colors }
function GetSysColor(Color: Integer): TColorRef;
function RGB(Red, Green, Blue: Integer): TColorRef;
function GetBValue(Col: TColorRef): Integer;
function GetGValue(Col: TColorRef): Integer;
function GetRValue(Col: TColorRef): Integer;
function SetRect(var R: TRect; Left, Top, Right, Bottom: Integer): LongBool;
function IsRectEmpty(R: TRect): LongBool;
function EqualRect(R1, R2: TRect): LongBool;
function UnionRect(var Dst: TRect; R1, R2: TRect): LongBool;
function CopyRect(var Dst: TRect; const Src: TRect): LongBool;

type
  TRGBQuad = packed record
    rgbReserved: Byte;
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
  end;

  TRGBTriple = packed record
    rgbtReserved: Byte;
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  tagTEXTMETRICA = record
    { supported by QFontMetrics }
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    (* unsupported:
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
    tmWeight: Longint;
    tmInternalLeading: Longint;
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmOverhang: Longint;
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: AnsiChar;
    tmLastChar: AnsiChar;
    tmDefaultChar: AnsiChar;
    tmBreakChar: AnsiChar;
    *)
  end;
  TTextMetric = tagTEXTMETRICA;

{ brushes }
function CreateSolidBrush(crColor: TColorRef): QBrushH;
function CreateHatchBrush(bStyle: BrushStyle; crColor: TColorRef): QBrushH;
function DeleteObject(Handle: QBrushH): LongBool; overload;

const
  { BrushStyle mappings}
  HS_BDIAGONAL  = BrushStyle_BDiagPattern;	// 45-degree downward left-to-right hatch
  HS_CROSS      = BrushStyle_CrossPattern;	// Hor. and vertical crosshatch
  HS_DIAGCROSS  = BrushStyle_DiagCrossPattern;  // 45-degree crosshatch
  HS_FDIAGONAL  = BrushStyle_FDiagPattern;	// 45-degree upward left-to-right hatch
  HS_HORIZONTAL = BrushStyle_HorPattern;        // Horizontal hatch
  HS_VERTICAL   = BrushStyle_VerPattern;        // Vertical hatch

function CreatePen(Style, Width: Integer; Color: TColorRef): QPenH;
function DeleteObject(Handle: QPenH): LongBool; overload;

const
  { Pen Styles }
  PS_NULL          = 0;   // PenStyle_NoPen
  PS_SOLID         = 1;   // PenStyle_SolidLine
  PS_DASH          = 2;   // PenStyle_DashLine
  PS_DOT           = 3;   // PenStyle_DotLine
  PS_DASHDOT       = 4;   // PenStyle_DashDotLine
  PS_DASHDOTDOT    = 5;   // PenStyle_DashDotDotLine
  PS_STYLE_MASK    = 15;  // PenStyle_MPenStyle
  {caps}
  PS_ENDCAP_FLAT   = 0;   // PenCapStyle_FlatCap
  PS_ENDCAP_SQUARE = 16;  // PenCapStyle_SquareCap
  PS_ENDCAP_ROUND  = 32;  // PenCapStyle_RoundCap
  PS_ENDCAP_MASK   = 48;  // PenCapStyle_MPenCapStyle
  {join}
  PS_JOIN_MITER    = 0;   // PenJoinStyle_MiterJoin
  PS_JOIN_BEVEL    = 64;  // PenJoinStyle_BevelJoin
  PS_JOIN_ROUND    = 128; // PenJoinStyle_RoundJoin
  PS_JOIN_MASK     = $C0; // PenCapStyle_MPenCapStyle

function SetWindowOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
function GetWindowOrgEx(Handle: QPainterH; Org: PPoint): LongBool;
{ limited implementations of }
function BitBlt(DestDC: QPainterH; X, Y, W, H: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; Rop: RasterOp): LongBool; overload;
function BitBlt(DestDC: QPainterH; X, Y, W, H: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; WinRop: Cardinal): LongBool; overload;
function PatBlt(Handle: QPainterH; X, Y, W, H: Integer;
  WinRop: Cardinal): LongBool; //overload;

const
  { bitblt:  windows dwRop values
    asn: limited implementation, possible to extend }
  BLACKNESS   = $00000042;   // RasterOp_ClearROP
  DSTINVERT   = $00550009;   // RasterOp_NotROP
  MERGECOPY   = $00C000CA;   // RasterOp_OrROP
  MERGEPAINT  = $00BB0226;   // RasterOp_NotOrRop
  NOTSRCCOPY  = $00330008;   // RasterOp_NotCopyROP
  NOTSRCERASE = $001100A6;   // RasterOp_NorROP
  SRCAND      = $008800C6;   // RasterOp_AndROP
  SRCCOPY     = $00CC0020;   // RasterOp_CopyROP
  SRCERASE    = $00440328;   // RasterOp_AndNotROP
  SRCINVERT   = $00660046;   // RasterOp_XorROP
  SRCPAINT    = $00EE0086;   // RasterOp_OrROP;
  WHITENESS   = $00FF0062;   // RasterOp_SetROP
  PATCOPY     = $00F00021;   // dest = pattern
  PATPAINT    = $00FB0A09;   // dest = DPSnoo = PDSnoo
  PATINVERT   = $005A0049;   // dest = pattern XOR dest
  ROP_DSPDxax = $00E20746;   // dest = ((pattern XOR dest) AND source) XOR Dest
  ROP_DSna    = $00220326;   // RasterOp_NotAndROP
  ROP_DSno    = MERGEPAINT;
  ROP_DPSnoo  = PATPAINT;
  ROP_D       = $00AA0029;   // RasterOp_NopROP
  ROP_Dn      = DSTINVERT;   // DSTINVERT
  ROP_SDna    = SRCERASE;    // SRCERASE
  ROP_SDno    = $00DD0228;   // RasterOp_OrNotROP
  ROP_DSan    = $007700E6;   // RasterOp_NandROP
  ROP_DSon    = $001100A6;   // NOTSRCERASE
  //ROP_Pn      = $000F0001;   //

function SetROP2(Handle: QPainterH; Rop: Integer): Integer; overload;
function SetROP2(Handle: QPainterH; Rop: RasterOp): RasterOp; overload;
function GetROP2(Handle: QPainterH): Integer; overload;
//function GetROP2(Handle: QPainterH): RasterOp; overload;

const
  R2_BLACK       = 9;  // RasterOp_ClearROP:	Pixel is always 0.
  R2_WHITE       = 10; // RasterOp_SetROP:Pixel is always 1.
  R2_NOP         = 11; // RasterOp_NopROP: Pixel remains unchanged.
  R2_NOT         = 8;  // RasterOp_NotROP:      inverse of the screen color.
  R2_COPYPEN     = 0;  // RasterOp_CopyROP: Pixel is the pen color.
  R2_NOTCOPYPEN	 = 4;  // RasterOp_NotCopyROP; inverse of the pen color.
  R2_MERGEPENNOT = 13; // RasterOp_OrNotROP: combination of the pen color and the inverse of the screen color.
  R2_MASKPENNOT  = 12; // RasterOp_AndNotROP: combination of the colors common to both the pen and the inverse of the screen.
  R2_MERGEPEN    = 1;  // RasterOp_OrROP: combination of the pen color and the screen color.
  R2_NOTMERGEPEN = 15; // RasterOp_NorROP:	inverse of the R2_MERGEPEN color.
  R2_MASKPEN     = 7;  // RasterOp_AndROP: combination of the colors common to both the pen and the screen.
  R2_NOTMASKPEN	 = 14; // RasterOp_NandROP: inverse of the R2_MASKPEN color.
  R2_XORPEN	 = 2;  // RasterOp_XorROP: combination of the colors in the pen and in the screen, but not in both.
  R2_NOTXORPEN   = 6;  // RasterOp_NotXorROP: inverse of the R2_XORPEN color.
  R2_MASKNOTPEN	 = 3;  // RasterOp_NotAndROP: combination of the colors common to both the screen and the inverse of the pen.
  R2_MERGENOTPEN = 5;  // RasterOp_NotOrROP: combination of the screen color and the inverse of the pen color.

function GetPixel(Handle: QPainterH; X, Y: Integer): TColorRef;
function SetPixel(Handle: QPainterH; X, Y: Integer; Color: TColorRef): TColorRef;
function SetTextColor(Handle: QPainterH; color: TColor): TColor;
function SetBkColor(Handle: QPainterH; color: TColor): TColor;
function SetBkMode(Handle: QPainterH; BkMode: Integer): Integer;
function SetDCBrushColor(Handle: QPainterH; Color: TColorRef): TColorRef;
function SetDCPenColor(Handle: QPainterH; Color: TColorRef): TColorRef;
function SetPenColor(Handle: QPainterH; Color: TColor): TColor;

const
  { SetBkMode: background mode }
  TRANSPARENT = 1; // BGMode_TransparentMode
  OPAQUE      = 2; // BGMode_OpaqueMode

function CreateCompatibleBitmap(Handle: QPainterH; Width, Height: Integer): QPainterH;
// DeleteObject is intended to destroy the Handle returned by CreateCompatibleBitmap
// (it destroys the Painter AND PaintDevice) 
function DeleteObject(Handle: QPainterH): LongBool; overload;
function GetDC(Handle: QWidgetH): QPainterH; overload;
function GetDC(Handle: Integer): QPainterH; overload;

procedure ReleaseDC(wdgtH: QWidgetH; Handle: QPainterH); overload;
procedure ReleaseDC(wdgtH: Integer; Handle: QPainterH); overload;

function RestoreDC(Handle: QPainterH; nSavedDC: Integer): LongBool;
function SaveDC(Handle: QPainterH): Integer;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; Text: WideString; len: Integer;lpDx: Pointer): LongBool; overload;
function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PChar; len: Integer;lpDx: Pointer): LongBool; overload;
function ExtTextOutW(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PWideChar; len: Integer;lpDx: Pointer): LongBool;

const
  { ExtTextOut format flags }
  ETO_OPAQUE     = 2;
  ETO_CLIPPED    = 4;
  ETO_RTLREADING = $80; // ignored

function FillRect(Handle: QPainterH; R: TRect; Brush: QBrushH): LongBool;
function GetCurrentPositionEx(Handle: QPainterH; pos: PPoint): LongBool;
function GetTextExtentPoint32(Handle: QPainterH; Text: WideString; len: Integer;
  var size: TSize): LongBool; overload;
function GetTextExtentPoint32(Handle: QPainterH; pText: PChar; len: Integer;
  var size: TSize): LongBool; overload;
function GetTextExtentPoint32W(Handle: QPainterH; pText: PWideChar; len: Integer;
  var size: TSize): LongBool;

function DrawFocusRect(Handle: QPainterH; const R: TRect): LongBool;
function InvertRect(Handle: QPainterH; const R: TRect): LongBool;
function RoundRect(Handle: QPainterH; Left, Top, Right, Bottom, X3, Y3: Integer): LongBool;
function Ellipse(Handle: QPainterH; Left, Top, Right, Bottom: Integer): LongBool;
function LineTo(Handle: QPainterH; X, Y:Integer): LongBool;
function MoveToEx(Handle: QPainterH; X, Y:Integer; oldpos: PPoint): LongBool;
function DrawIcon(Handle: QPainterH; X, Y: Integer; hIcon: QPixMapH): LongBool;
function DrawIconEx(Handle: QPainterH; X, Y: Integer; hIcon: QPixMapH;
  W, H: Integer; istepIfAniCur: Integer; hbrFlickerFreeDraw: QBrushH;
  diFlags: Cardinal): LongBool;

const
{ DrawIconEx diFlags }
  DI_MASK        = 1;
  DI_IMAGE       = 2;
  DI_NORMAL      = 3;
//  DI_COMPAT    = 4;   not supported
  DI_DEFAULTSIZE = 8;


function DrawText(Handle: QPainterH; var text: WideString; len: Integer;
  var R: TRect; WinFlags: Integer): Integer; overload;
{ limited implementation of }
function DrawText(Handle: QPainterH; text: PChar; len: Integer;
  var R: TRect; WinFlags: Integer): Integer; overload;

const
  { DrawText format (windows) flags }
  DT_TOP           = 0;
  DT_LEFT          = 0;
  DT_CENTER        = 1;
  DT_RIGHT         = 2;
  DT_VCENTER       = 4;
  DT_BOTTOM        = 8;
  DT_WORDBREAK     = $10;
  DT_SINGLELINE    = $20;
  DT_EXPANDTABS    = $40;
  DT_TABSTOP       = $80;
  DT_NOCLIP        = $100;
(* DT_EXTERNALLEADING = $200;  // not supported *)
  DT_CALCRECT      = $400;
  DT_NOPREFIX      = $800;
  DT_INTERNAL      = $1000;  // Uses the system font to calculate text metrics.
  DT_EDITCONTROL   = $2000; // ignored
  DT_PATH_ELLIPSIS = $4000;
  DT_ELLIPSIS      = $8000;
  DT_END_ELLIPSIS  = DT_ELLIPSIS;
  DT_MODIFYSTRING  = $10000;
  DT_RTLREADING    = $20000; // ignored
  DT_WORD_ELLIPSIS = $40000;
  DT_HIDEPREFIX    = $100000; //unsupported yet
  DT_PREFIXONLY    = $200000; //unsupported yet

(*
  function DrawFrameControl(Handle: QPainterH; const Rect: TRect;
  uType, uState: Cardinal): LongBool;
 *)

type
  TSysMetrics = (
    SM_CXSCREEN,  SM_CYSCREEN,
    SM_CXVSCROLL, SM_CYVSCROLL,
    SM_CXSMICON,  SM_CYSMICON,
    SM_CXICON,    SM_CYICON,
    SM_CXBORDER,  SM_CYBORDER,
    SM_CXFRAME,   SM_CYFRAME
  );

{ limited implementation of }
function GetSystemMetrics(PropItem: TSysMetrics): Integer;

type
  TDeviceCap = (
  HORZSIZE,        {Horizontal size in millimeters}
  VERTSIZE,        {Vertical size in millimeters}
  HORZRES,         {Horizontal width in pixels}
  VERTRES,         {Vertical height in pixels}
  BITSPIXEL,       {Number of bits per pixel}
  NUMCOLORS,
  LOGPIXELSX,      {Logical pixelsinch in X}
  LOGPIXELSY,      {Logical pixelsinch in Y}
  PHYSICALWIDTH,   {Physical Width in device units}
  PHYSICALHEIGHT,  {Physical Height in device units}
  PHYSICALOFFSETX, {Physical Printable Area X margin}
  PHYSICALOFFSETY  {Physical Printable Area Y margin}
  );

{ (very) limited implementations of }
function GetDeviceCaps(Handle: QPainterH; devcap: TDeviceCap): Integer;
function GetTextMetrics(Handle: QPainterH; tt: TTextMetric): Integer;

type
  TMinMaxInfo = packed record
    ptReserved: TPoint;
    ptMaxSize: TPoint;
    ptMaxPosition: TPoint;
    ptMinTrackSize: TPoint;
    ptMaxTrackSize: TPoint;
  end;

(*)
  PPaintStruct = ^TPaintStruct;
  tagPAINTSTRUCT = packed record
    hdc: HDC;
    fErase: BOOL;
    rcPaint: TRect;
    fRestore: BOOL;
    fIncUpdate: BOOL;
    rgbReserved: array[0..31] of Byte;
  end;
  TPaintStruct = tagPAINTSTRUCT;
  PAINTSTRUCT = tagPAINTSTRUCT;
(*)
  TWindowPlacement = packed record
    length: Cardinal;
    flags: Integer;
    showCmd: UInt;
    ptMinPosition: TPoint;
    ptMaxPosition: TPoint;
    rcNormalPosition: TRect;
  end;
  PWindowPlacement = ^TWindowPlacement;


// widget related  function
function BringWindowToTop(Handle: QWidgetH): LongBool;
function CloseWindow(Handle: QWidgetH): LongBool;
function DestroyWindow(Handle: QWidgetH): LongBool;
function EnableWindow(Handle: QWidgetH; Value: Boolean): LongBool;
function GetClientRect(Handle: QWidgetH; var R: TRect): LongBool;
function GetFocus: QWidgetH;
function GetParent(Handle: QWidgetH): QWidgetH;
function GetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
function GetWindowRect(Handle: QWidgetH; var  R: TRect): LongBool;

function HWND_DESKTOP: QWidgetH;
function InvalidateRect(Handle: QWidgetH; R: PRect; erasebackground: Boolean): LongBool;
function IsChild(ParentHandle, Childhandle: QWidgetH): LongBool;
function IsWindowEnabled(Handle: QWidgetH): LongBool;
function IsWindowVisible(Handle: QWidgetH): LongBool;
function MapWindowPoints(WidgetTo, WidgetFrom: QWidgetH; var Points; nr: Cardinal): Integer;
function SetFocus(Handle: QWidgetH): QWidgetH;
function SetForegroundWindow(Handle: QWidgetH): LongBool;
//function SetParent(hWndChild, hWndNewParent: QWidgetH): QWidgetH;
function SetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
function ShowWindow(Handle: QWidgetH; showCmd: UInt): LongBool;
function SwitchToThisWindow(Handle: QWidgetH; Restore: Boolean): LongBool;

function ClientToScreen(Handle: QWidgetH; var Point: TPoint): LongBool;
function ScreenToClient(Handle: QWidgetH; var Point: TPoint): LongBool;


//function WindowFromPoint(Point: TPoint): QWidgetH; 	// structure with point

const
  { ShowWindow() Commands }
  SW_HIDE = 0;
  SW_SHOWNORMAL = 1;
  SW_NORMAL = 1;
  SW_SHOWMINIMIZED = 2;
  SW_SHOWMAXIMIZED = 3;
  SW_MAXIMIZE = 3;
  SW_SHOWNOACTIVATE = 4;
  SW_SHOW = 5;
  SW_MINIMIZE = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA = 8;
  SW_RESTORE = 9;
  SW_SHOWDEFAULT = 10;
  SW_MAX = 10;

function MessageBox(parent: QWidgetH; Text, Caption: string; WinFlags: Cardinal): Integer; overload;
function MessageBox(parent: QWidgetH; Text, Caption: WideString; WinFlags: Cardinal): Integer; overload;
function MessageBox(parent: QWidgetH; pText, pCaption: PChar; WinFlags: Cardinal): Integer; overload;
//function MessageBoxW(parent: QWidgetH; pText, pCaption: PWideChar; WinFlags: Cardinal): Integer;

const
  { MessageBox() WinFlags }
  MB_OK              = $0000;
  MB_OKCANCEL        = $0001;
  MB_ABORTRETRYIGNORE= $0002;
  MB_YESNOCANCEL     = $0003;
  MB_YESNO           = $0004;
  MB_RETRYCANCEL     = $0005;
  MB_HELP            = $4000; { Help Button not supported}
  MB_ICONHAND        = $0010;
  MB_ICONQUESTION    = $0020;
  MB_ICONEXCLAMATION = $0030;
  MB_ICONASTERISK    = $0040;
  MB_USERICON        = $0080;
  MB_DEFBUTTON1      = $0000;
  MB_DEFBUTTON2      = $0100;
  MB_DEFBUTTON3      = $0200;
  MB_DEFBUTTON4      = $0300;
  MB_ICONWARNING     = MB_ICONEXCLAMATION;
  MB_ICONERROR       = MB_ICONHAND;
  MB_ICONINFORMATION = MB_ICONASTERISK;
  MB_ICONSTOP        = MB_ICONHAND;

  { MessageBox() return values }
  IDCLOSE  = 0;
  IDOK     = 1;
  IDCANCEL = 2;
  IDYES    = 3;
  IDNO     = 4;
  IDABORT  = 5;
  IDRETRY  = 6;
  IDIGNORE = 7;
  { aliases }
  ID_OK      = IDOK;
  ID_CANCEL  = IDCANCEL;
  ID_ABORT   = IDABORT;
  ID_RETRY   = IDRETRY;
  ID_IGNORE  = IDIGNORE;
  ID_YES     = IDYES;
  ID_NO      = IDNO;
  ID_CLOSE   = IDCLOSE;
  IDHELP     = 9;        //  not supported
  ID_HELP    = IDHELP;  //  not supported
  IDTRYAGAIN = IDRETRY;
  IDCONTINUE = IDIGNORE;

function SelectObject(Handle: QPainterH; Font: QFontH): QFontH; overload;
function SelectObject(Handle: QPainterH; Brush: QBrushH): QBrushH; overload;
function SelectObject(Handle: QPainterH; Pen: QPenH): QPenH; overload;

// region related API's 
type
  TCombineMode = (
    RGN_AND,	// Creates the intersection of the two combined regions.
    RGN_COPY,	// Creates a copy of the region identified by hrgnSrc1.
    RGN_DIFF,	// Combines the parts of hrgnSrc1 that are not part of hrgnSrc2.
    RGN_OR,	  // Creates the union of two combined regions.
    RGN_XOR	  // Creates the union of two combined regions except for any overlapping areas.
    );

function CombineRgn(Destination, Source1, Source2: QRegionH; Operation: TCombineMode): Integer;
function CreateEllipticRgn(Left, Top, Right, Bottom: Integer): QRegionH;
function CreateEllipticRgnIndirect(Rect: TRect): QRegionH;
//function CreatePolygonRgn(p1: TPointArray; Count, FillMode: Integer): QRegionH;
function CreatePolygonRgn(const Points; Count, FillMode: Integer): QRegionH;
function CreateRectRgn(left, top, right, bottom: Integer): QRegionH;
function CreateRectRgnIndirect(Rect: TRect): QRegionH;
//function CreateRoundRectRgn(Left, Top, Right, Bottom, Width, Height: Integer): QRegionH;
function DeleteObject(Region: QRegionH): LongBool; overload;
function EqualRgn(Rgn1, Rgn2: QRegionH): LongBool;
function FillRgn(Handle: QPainterH; Region: QRegionH; Brush: QBrushH): LongBool;
function GetClipRgn(Handle: QPainterH; rgn: QRegionH): Integer;
function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer;
function InvertRgn(Handle: QPainterH; Region: QRegionH): LongBool;
function OffsetClipRgn(Handle: QPainterH; X, Y: Integer): Integer;
function OffsetRgn(Region: QRegionH; X, Y: Integer): Integer;
function PtInRegion(Rgn: QRegionH; X, Y: Integer): Boolean;
function RectInRegion(RGN: QRegionH; Rect: TRect): LongBool;
function SelectClipRgn(Handle: QPainterH; Region: QRegionH): Integer;
function SetRectRgn(Rgn: QRegionH; X1, Y1, X2, Y2: Integer): LongBool;

const
// constants for CreatePolygon
  ALTERNATE     = 1;
  WINDING       = 2;
  // CombineRgn return values
  NULLREGION    = 1;     // Region is empty
  SIMPLEREGION  = 2;     // Region is a rectangle
  COMPLEXREGION = 3;	   // Region is not a rectangle
  ERROR	        = 0;     // Region error
  RGN_ERROR     = ERROR;

// viewports
function SetViewportExtEx(Handle: QPainterH; XExt, YExt: Integer; Size: PSize): LongBool;
function SetViewPortOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
function GetViewportExtEx(Handle: QPainterH; Size: PSize): LongBool;

// Text clipping
function TruncatePath(const FilePath: string; Canvas: TCanvas; MaxLen: Integer): string;
function TruncateName(const Name: WideString; Canvas: TCanvas; MaxLen: Integer): WideString;


procedure TextOutAngle(ACanvas: TCanvas;Angle, Left, Top: Integer; Text: WideString);
//

procedure CopyMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
procedure FillMemory(Dest: Pointer; Len: Cardinal; Fill: Byte);
procedure MoveMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
procedure ZeroMemory(Dest: Pointer; Len: Cardinal);

function GetCaretBlinkTime: Cardinal;
function SetCaretBlinkTime(uMSeconds: Cardinal): LongBool;

function GetDoubleClickTime: Cardinal;
function SetDoubleClickTime(Interval: Cardinal): LongBool;

function Win2QtAlign(Flags: Integer): Integer;
function QtStdAlign(Flags: Integer): Word;

{$IFDEF LINUX}

resourcestring
  SFCreateError = 'Unable to create file %s';
  SFOpenError = 'Unable to open file %s';
  SReadError = 'Error reading file';
  SWriteError = 'Error writing file';

function CopyFile(lpExistingFileName, lpNewFileName: PChar;
  bFailIfExists: LongBool): LongBool; overload;
function CopyFileA(lpExistingFileName, lpNewFileName: PAnsiChar;
  bFailIfExists: LongBool): LongBool;
function CopyFileW(lpExistingFileName, lpNewFileName: PWideChar;
  bFailIfExists: LongBool): LongBool;
function CopyFile(const source: string; const destination: string;
  FailIfExists: Boolean): LongBool; overload;

function FileGetSize(const FileName: string): Cardinal;
function FileGetAttr(const FileName: string): Integer;
function GetUserName(lpBuffer: PChar; var nSize: DWORD): LongBool;
function GetComputerName(lpBuffer: PChar; var nSize: DWORD): LongBool;
function MakeIntResource(Value: Integer): PChar;
function GetTickCount: Cardinal;
procedure MessageBeep(Value: Integer);   // value ignored

{$ENDIF LINUX}

function DPtoLP(Handle: QPainterH; var Points; Count: Integer): LongBool;
function LPtoDP(Handle: QPainterH; var Points; Count: Integer): LongBool;
function ExcludeClipRect(Handle: QPainterH; x0, y0, x1, y1: Integer): Integer;
function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer; // improved
function SelectClipRgn(Handle: QPainterH; Region: QRegionH): integer; // improved
function CreateRoundRectRgn(x1, y1, x2, y2, WidthEllipse, HeightEllipse: Integer): QRegionH;
function StretchBlt(DestHandle: QPainterH; X, Y, Width, Height: Integer; SrcHandle: QPainterH;
  XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: RasterOp): LongBool;
function GetClientRect(Handle: QWidgetH; var R: TRect): LongBool; // improved
function WindowFromDC(Handle: QPainterH): QWidgetH;

{ ------------ Caret -------------- }
function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean; overload;
function CreateCaret(Widget: QWidgetH; ColorCaret: Cardinal; Width, Height: Integer): Boolean; overload;
function GetCaretBlinkTime: Cardinal;
function SetCaretBlinkTime(MSeconds: Cardinal): Boolean;
function HideCaret(Widget: QWidgetH): Boolean;
function ShowCaret(Widget: QWidgetH): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var Pt: TPoint): Boolean;
function DestroyCaret: Boolean;

implementation

{$IFDEF LINUX}
uses
  Libc;
{$ENDIF LINUX}

function DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  Result := 0;
end;

function DrawTextBiDiModeFlags(Flags: Longint): Longint;
begin
  Result := Flags;
end;

function GetSysColor(Color: Integer): TColorRef;
begin
  Result := TColorRef(Application.Palette.GetColor(Color));
end;

function CreatePen(Style, Width: Integer; Color: TColorRef): QPenH;
begin
  Result := QPen_create(QColor(Color), Width, PenStyle(Style));
end;

function DeleteObject(Handle: QPenH): LongBool;
begin
  try
    QPen_destroy(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function CreateSolidBrush(crColor: TColorRef): QBrushH;
begin
  Result := QBrush_create(QColor(crColor), BrushStyle_SolidPattern);
end;

function CreateHatchBrush(bStyle: BrushStyle; crColor: TColorRef): QBrushH;
begin
  Result := QBrush_create(QColor(crColor), bStyle);
end;

function DeleteObject(Handle: QBrushH): LongBool;
begin
  try
    QBrush_destroy(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function EnableWindow(Handle: QWidgetH; Value: Boolean): LongBool;
begin
  try
    QWidget_setEnabled(Handle, Value);
    Result := True;
  except
    Result := False;
  end;
end;

function SetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
begin
  try
    with W.rcNormalPosition do
      QWidget_setGeometry(Handle, Left, Top, Right - Left, Bottom - Top);
    Result := ShowWindow(Handle, W.ShowCmd);
  except
    Result := False;
  end;
end;

function GetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
var
  R: TRect;
begin
  try
    QWidget_geometry(Handle, @R);
    W.rcNormalPosition.Left := R.Left;
    W.rcNormalPosition.Top := R.Top;
    W.rcNormalPosition.Right := R.Right;
    W.rcNormalPosition.Bottom := R.Left;
    if QWidget_isMinimized(Handle) then
      W.showCmd := SW_SHOWMINIMIZED
    else if QWidget_isMaximized(Handle) then
      W.showCmd := SW_SHOWMAXIMIZED
    else if not QWidget_isVisible(Handle) then
      W.showCmd := SW_HIDE
    else
      W.showCmd := SW_SHOWNORMAL;
    Result := True;
  except
    Result := False;
  end;
end;

function GetWindowRect(Handle: QWidgetH; var R: TRect): LongBool;
begin
  try
    QWidget_frameGeometry(Handle, @R);
    Result := True;
  except
    Result := False;
  end;
end;

function GetClientRect(Handle: QWidgetH; var R: TRect): LongBool;
begin
  try
    QWidget_rect(Handle, @R);
    Result := True;
  except
    Result := False;
  end;
end;


function ShowWindow(Handle: QWidgetH; showCmd: UInt): LongBool;
begin
  try
    case ShowCmd of
      SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
        QWidget_showMinimized(Handle);
      SW_MAXIMIZE:
        QWidget_showMaximized(Handle);
      SW_HIDE:
        QWidget_hide(Handle);
    else
      QWidget_showNormal(Handle);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function IsWindowVisible(Handle: QWidgetH): LongBool;
begin
  Result := QWidget_isVisible(Handle);
end;

function IsWindowEnabled(Handle: QWidgetH): LongBool;
begin
  Result := QWidget_isEnabled(Handle);
end;

function SetFocus(Handle: QWidgetH): QWidgetH;
begin
  try
    Result := GetFocus;
    QWidget_setFocus(Handle);
  except
    Result := nil;
  end;
end;

function GetFocus: QWidgetH;
begin
  Result := QApplication_focusWidget(Application.Handle);
end;

function SetBkMode(Handle: QPainterH; BkMode: Integer): Integer;
begin
  case QPainter_BackgroundMode(Handle) of
  BGMode_TransparentMode: Result := TRANSPARENT; // BGMode_TransparentMode
  BGMode_OpaqueMode: Result := OPAQUE; // BGMode_OpaqueMode
  end;
  case BkMode of
  TRANSPARENT: QPainter_setBackgroundMode(Handle, BGMode_TransparentMode);
  OPAQUE: QPainter_setBackgroundMode(Handle, BGMode_OpaqueMode);
  end;
end;

function SetPenColor(Handle: QPainterH; Color: TColor): TColor;
begin
  Result :=  QColorColor(QPen_color(QPainter_pen(Handle)));
  QPainter_setPen(Handle, QColor(Color));
end;

function SetTextColor(Handle: QPainterH; Color: TColor): TColor;
begin
  Result := SetPenColor(Handle, Color);
end;

function SetBkColor(Handle: QPainterH; Color: TColor): TColor;
begin
  Result := QColorColor(QPainter_BackGroundColor(Handle));
  QPainter_setBackGroundColor(Handle, QColor(Color));
end;

function SetDCBrushColor(Handle: QPainterH; Color: TColorRef): TColorRef;
begin
  Result := QColorColor(QBrush_color(QPainter_brush(Handle)));
  QPainter_setBrush(Handle, QColor(Color));
end;

function SetDCPenColor(Handle: QPainterH; Color: TColorRef): TColorRef;
begin
  Result := SetPenColor(Handle, Color);
end;

function GetParent(Handle: QWidgetH): QWidgetH;
begin
  Result := QWidget_parentWidget(Handle);
end;

function SetParent(hWndChild, hWndNewParent: QWidgetH): QWidgetH;
begin
  Result := GetParent(hWndChild);
// QWidget_reparent(Handle: QWidgetH; parent: QWidgetH; p2: PPoint; showIt: Boolean)
end;

function PatternPaint(DestDC: QPainterH; X, Y, W, H: Integer; rop: RasterOp): LongBool;
var
  trop: RasterOp;
  bkmode: Integer;
begin
  try
    trop := QPainter_rasterOp(DestDC);
    QPainter_setRasterOp(DestDC, rop);    // asn: or use current ?
    bkmode := SetBkMode(DestDC, OPAQUE);
    QPainter_fillRect(DestDC, X, Y, W, H, QPainter_brush(DestDC)); // current brush
    SetBkMode(DestDC, bkmode);

    QPainter_setRasterOp(DestDC, trop);
    Result := True;
  except
    Result := False;
  end;
end;

function BitBlt(DestDC: QPainterH; X, Y, W, H: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; WinRop: Cardinal): LongBool;
var
  TempDC: QPainterH;
begin
  case WinRop of
  BLACKNESS:   Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_ClearROP);
  DSTINVERT:   Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NotROP);
  MERGECOPY:   Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_OrROP);
  MERGEPAINT:  Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NotOrRop);
  NOTSRCCOPY:  Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NotCopyROP);
  NOTSRCERASE: Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NorROP);
  SRCAND:      Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_AndROP);
  SRCCOPY:     Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_CopyROP);
  SRCERASE:    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_AndNotROP);
  SRCINVERT:   Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_XorROP);
  SRCPAINT:    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_OrROP);
  WHITENESS:   Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_SetROP);
  PATCOPY:     Result := PatternPaint(DestDc, X, Y, W, H, RasterOp_CopyROP);
  PATINVERT:   Result := PatternPaint(DestDc, X, Y, W, H, RasterOp_XorROP);
  ROP_DSna:    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NotAndROP);
  ROP_D:       Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NopROP);
  ROP_SDno:    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_OrNotROP);
  ROP_DSan:    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NandROP);
  PATPAINT:
  begin  // DPSnoo   = PDSnoo
    Result := BitBlt(DestDc, X, Y, W, H, SrcDC, XSrc, YSrc, RasterOp_NotOrRop); // DSno
    if Result then
      Result := PatternPaint(DestDc, X, Y, W, H, RasterOp_XOrROP);
  end;
  ROP_DSPDxax:
  begin
    TempDC := CreateCompatibleBitmap(DestDC, W, H);
    try
        // copy destDC to pixmap
      BitBlt(TempDC, 0, 0, W, H, DestDC, X, Y,  RasterOp_CopyROP);
      BitBlt(TempDC, 0, 0, W, H, nil, 0, 0, PATINVERT);  // PDx
      BitBlt(TempDC, 0, 0, W, H, SrcDC, XSrc, YSrc, RasterOp_AndROP); // SPDxa
      Result := BitBlt(DestDC, X, Y, W, H, TempDC, 0, 0, RasterOp_XorROP); // DSPDxax
    except
      Result := False;
    end;
    DeleteObject(TempDC);
  end;
  else
    Result := False;
  end;
end;

function BitBlt(DestDC: QPainterH; X, Y, W, H: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; Rop: RasterOp): LongBool;
var
  dp: TPoint;
  sr: TRect;
begin
  Result := True;
  dp.X := X;
  dp.Y := Y;
  sr := Bounds(XSrc, YSrc, W, H);
  try
    BitBlt(QPainter_device(DestDC), @dp, QPainter_device(SrcDC), @sr, Rop);
  except
    Result := False;
  end;
end;

function PatBlt(Handle: QPainterH; X, Y, W, H: Integer; WinRop: Cardinal): LongBool;
begin
  Result := BitBlt(Handle, X, Y, W, H, Handle, X, Y, WinRop);
end;

function SetROP2(Handle: QPainterH; Rop: Integer): Integer;
var
  rop2: RasterOp;
begin
//  Result := GetROP2(Handle);
//  QPainter_setRasterOp(Handle, RasterOp(Rop));
  rop2 := RasterOp(Rop);
  rop2 := SetROP2(Handle, rop2);
  Result := Integer(rop2);
end;

function GetROP2(Handle: QPainterH): Integer;
begin
  Result := Integer(QPainter_rasterOp(Handle));
end;

function SetROP2(Handle: QPainterH; Rop: RasterOp): RasterOp;
begin
  Result := QPainter_rasterOp(Handle);
  QPainter_setRasterOp(Handle, Rop);
end;

(*
function GetROP2(Handle: QPainterH): RasterOp;
begin
  Result := QPainter_rasterOp(Handle);
end;
*)
function SetForegroundWindow(Handle: QWidgetH): LongBool;
begin
  try
    Result := QWidget_isTopLevel(Handle);
    if Result then
      QWidget_raise(Handle);
  except
    Result := False;
  end;
end;

function BringWindowToTop(Handle: QWidgetH): LongBool;
begin
  try
    while not QWidget_isTopLevel(Handle)
    do
      Handle := QWidget_parentWidget(Handle);
    QWidget_Show(Handle);
    QWidget_raise(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function SwitchToThisWindow(Handle: QWidgetH; Restore: Boolean): LongBool;
begin
  try
    Result := QWidget_isTopLevel(Handle);
    if Result then
    begin
      if Restore then
        QWidget_Show(Handle);
      QWidget_setActiveWindow(Handle);
    end;
  except
    Result:= True;
  end;
end;

function CloseWindow(Handle: QWidgetH): LongBool;
begin
  try
    Result := QWidget_isTopLevel(Handle);
    if Result
    then
      QWidget_close(Handle)
  except
    Result := False;
  end;
end;

function DestroyWindow(Handle: QWidgetH): LongBool;
begin
  Result := True;
  try
    if QWidget_isTopLevel(Handle)
    then
      QWidget_destroy(Handle)
    else
      Result := False;
  except
    Result := False;
  end;
end;

function ClientToScreen(Handle: QWidgetH; var Point: TPoint): LongBool;
begin
  try
    QWidget_mapToGlobal(Handle, @Point, @Point);
    Result := True;
  except
    Result := False;
  end;
end;

function ScreenToClient(Handle: QWidgetH; var Point: TPoint): LongBool;
begin
  try
    QWidget_mapFromGlobal(Handle, @Point, @Point);
    Result := True;
  except
    Result := False;
  end;
end;

function MapWindowPoints(WidgetTo, WidgetFrom: QWidgetH; var Points; nr: Cardinal): Integer;
var
  i: Integer;
  p1: PPoint;
  p2: TPoint;
begin
  p1 := @Points;
  try
    if not IsChild(WidgetTo, WidgetFrom) then
    begin
      Result := 0;
      exit;
    end
    else if nr > 0 then
    begin
      QWidget_mapTo(WidgetFrom, p1, WidgetTo, @p2);
      Result := (((p2.Y - p1.Y) shl 16) and $FFFF0000) +
                ((p2.X - p1.X) and $FFFF);
      for i:= 0 to nr-1 do
      begin
        QWidget_mapTo(WidgetFrom, p1, WidgetTo, p1);
        inc(p1);
      end;
    end
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function IsChild(ParentHandle, Childhandle: QWidgetH): LongBool;
var
  ParentH: QWidgetH;
begin
  Result := False;
  parentH := QWidget_parentWidget(ChildHandle);
  while (parentH <> nil) and (Not Result) do
  begin
    Result := parentH = ParentHandle;
    parentH := QWidget_parentWidget(parentH);
  end;
end;

function MessageBox(parent: QWidgetH; Text, Caption: WideString; WinFlags: Cardinal): Integer; overload;
var
  Button0, Button1, Button2: Integer;
const
  ButtonOk = 1;
  ButtonCancel = 2;
  ButtonYes = 3;
  ButtonNo = 4;
  ButtonAbort = 5;
  ButtonRetry = 6;
  ButtonIgnore = 7;
  ButtonDefault = $100;
  ButtonEscape = $200;
begin
  case (WinFlags and $7) of
  MB_OKCANCEL:
    begin
      Button0 := ButtonOk;
      Button1 := ButtonCancel or ButtonEscape;
      Button2 := 0;
    end;
  MB_ABORTRETRYIGNORE:
    begin
      Button0 := ButtonAbort;
      Button1 := ButtonRetry or ButtonDefault;
      Button2 := ButtonIgnore;
    end;
  MB_YESNOCANCEL:
    begin
      Button0 := ButtonYes;
      Button1 := ButtonNo;
      Button2 := ButtonCancel or ButtonEscape;
    end;
  MB_YESNO:
    begin
      Button0 := ButtonYes;
      Button1 := ButtonNo or ButtonEscape;
      Button2 := 0;
    end;
  MB_RETRYCANCEL:
    begin
      Button0 := ButtonRetry;
      Button1 := ButtonCancel or ButtonEscape;
      Button2 := 0;
    end;
  else     // MB_OK and non supported
    begin
      Button0 := ButtonOk or ButtonEscape;
      Button1 := 0;
      Button2 := 0;
    end;
  end;
  case (WinFlags and $300) of
  MB_DEFBUTTON2: Button1 := Button1 or ButtonDefault;
  MB_DEFBUTTON3: Button2 := Button2 or ButtonDefault;
  else
  // MB_DEFBUTTON1:
    Button0 := Button0 or ButtonDefault;
  end;
  case (WinFlags and $F0) of
  MB_ICONINFORMATION:
    Result := QMessageBox_information(parent, @caption, @text,
                                      button0, button1, button2);
  MB_ICONWARNING:
    Result := QMessageBox_warning(parent, @caption, @text,
                                      button0, button1, button2);
  MB_ICONQUESTION:
    Result := QMessageBox_information(parent, @caption, @text,
                                      button0, button1, button2);
  else
//  MB_ICONSTOP:
    Result := QMessageBox_critical(parent, @caption, @text,
                                      button0, button1, button2);
  end;
end;

function MessageBox(parent: QWidgetH; pText, pCaption: PChar; WinFlags: Cardinal): Integer;
var
  wsText, wsCaption: WideString;
begin
  wsText := pText;
  wsCaption := pCaption;
  Result := MessageBox(parent, wsText, wsCaption, WinFlags);
end;

function MessageBox(parent: QWidgetH; Text, Caption: String; WinFlags: Cardinal): Integer; overload;
begin
  Result := MessageBox(parent, Text, Caption, WinFlags);
end;

function MessageBoxW(parent: QWidgetH; pText, pCaption: PWideChar; WinFlags: Cardinal): Integer;
var
  wsText, wsCaption: WideString;
begin
  wsText := pText;
  wsCaption := pCaption;
  Result := MessageBox(parent, wsText, wsCaption, WinFlags);
end;

function SelectObject(Handle: QPainterH; Font: QFontH): QFontH;
begin
  Result := QPainter_font(Handle);
  QPainter_setFont(Handle, Font);
end;

function SelectObject(Handle: QPainterH; Brush: QBrushH): QBrushH;
begin
  Result := QPainter_Brush(Handle);
  QPainter_setBrush(Handle, Brush);
end;

function SelectObject(Handle: QPainterH; Pen: QPenH): QPenH;
begin
  Result := QPainter_Pen(Handle);
  QPainter_setPen(Handle, Pen);
end;

function GetRegionType(rgn: QRegionH): Integer;
var
  R: TRect;
begin
  try
    if QRegion_isEmpty(rgn) then
      Result := NULLREGION
    else
    begin
      QRegion_boundingRect(rgn, @R);
      if QRegion_contains(rgn, PRect(@R)) then
        Result := SIMPLEREGION
      else
        Result := COMPLEXREGION;
    end;
  except
    Result := RGN_ERROR;
  end;
end;

function CreateEllipticRgn(left, top, right, bottom: Integer): QRegionH;
begin
  Result := QRegion_create(left, top, right-left, bottom-top, QRegionRegionType_Ellipse);
end;

function CreateEllipticRgnIndirect(Rect: TRect): QRegionH;
begin
  Result := QRegion_create(@Rect, QRegionRegionType_Ellipse);
end;

function CreateRectRgn(left, top, right, bottom: Integer): QRegionH;
var
  R: TRect;
begin
  SetRect(R, left, top, right, bottom);
  Result := QRegion_create(@R, QRegionRegionType_Rectangle);
end;

function CreateRectRgnIndirect(Rect: TRect): QRegionH;
begin
  Result := QRegion_create(@Rect, QRegionRegionType_Rectangle);
end;

function CreatePolygonRgn(const Points; Count, FillMode: Integer): QRegionH;
var
  pts: TPointArray;
  i: Integer;
  p: PPoint;
begin
  SetLength(pts, Count);
  p := PPoint(Points);
  for i:=0 to Count-1 do
  begin
    pts[i].X := p.X;
    pts[i].Y := p.Y;
    inc(p);
  end;
  Result := QRegion_create(@pts, Fillmode = WINDING);
end;

function SelectClipRgn(Handle: QPainterH; Region: QRegionH): Integer;
begin
  try
    QPainter_setClipRegion(Handle, Region);
    QPainter_setClipping(Handle, True);
    Result := GetRegionType(Region);
  except
    Result := RGN_ERROR;
  end;
end;

function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer;
var
  rgn, rrgn: QRegionH;
  R: TRect;
begin
  SetRect(R, X1, Y1, X2, Y2);
  rrgn := QRegion_create(@R, QRegionRegionType_Rectangle);
  try
    rgn := QPainter_clipRegion(Handle);
    QRegion_intersect(rgn, rgn, rrgn);
    Result := GetRegionType(rgn);
  except
    Result := RGN_ERROR;
  end;
  QRegion_destroy(rrgn);
end;

function SetRectRgn(Rgn: QRegionH; X1, Y1, X2, Y2: Integer): LongBool;
var
  rgn2: QRegionH;
  R: TRect;
begin
  SetRect(R, X1, Y1, X2, Y2);
  rgn2 := QRegion_create(@R, QRegionRegionType_Rectangle);
  try
    QRegion_unite(rgn2, rgn, rgn2);
    Result := True;
  except
    Result := False;
  end;
  QRegion_destroy(rgn2);
end;

function EqualRgn(Rgn1, Rgn2: QRegionH): LongBool;
var
  tmpRgn: QRegionH;
begin
  tmpRgn := QRegion_create;
  try
    Result := CombineRgn(tmpRgn, Rgn1, Rgn2, RGN_XOR) =  NULLREGION
  except
    Result := False;
  end;
  QRegion_destroy(tmpRgn);
end;

function GetClipRgn(Handle: QPainterH; rgn: QRegionH): Integer;
begin
  if QPainter_hasClipping(Handle)
  then
  begin
    QRegion_unite(QPainter_clipRegion(Handle), rgn, QPainter_clipRegion(Handle));
    Result := GetRegionType(rgn);
  end
  else
    Result := NULLREGION;
end;

function CombineRgn(Destination, Source1, Source2: QRegionH;
  Operation: TCombineMode): Integer;
begin
  try
    case Operation of
      RGN_OR: QRegion_unite(Source1, Destination, Source2);
      RGN_AND: QRegion_intersect(Source1, Destination, Source2);
      // RGN_DIFF Subtracts Source2 from Source1
      RGN_DIFF: QRegion_subtract(Source1, Destination, Source2);
      // RGN_XOR creates the union of two combined regions except for any
      // overlapping areas.
      RGN_XOR: QRegion_eor(Source1, Destination, Source2);
      // RGN_COPY: Creates a copy of the region identified by Source1.
      RGN_COPY:	QRegion_unite(Source1, Destination, Source1)
      else
        Raise Exception.Create('CombineRgn: operation not implemented');
    end;
    Result := GetRegionType(destination);
  except
    Result := RGN_ERROR;
  end;
end;

function OffsetRgn(Region: QRegionH; X, Y:Integer): Integer;
begin
  QRegion_translate(Region, X, Y);
  Result := GetRegionType(Region);
end;

function OffsetClipRgn(Handle: QPainterH; X, Y: Integer): Integer;
begin
  try
    if QPainter_hasClipping(Handle) then
    begin
      OffsetRgn(QPainter_clipRegion(Handle), X, Y);
      Result := GetRegionType(QPainter_clipRegion(Handle));
    end
    else
      Result := RGN_ERROR;
  except
    Result := RGN_ERROR;
  end;
end;

function InvertRgn(Handle: QPainterH; Region: QRegionH): LongBool;
var
  Rgn: QRegionH;
  R: TRect;
begin
  try
    QPainter_window(Handle, @R);
    OffsetRect(R, -R.Left, -R.Top);
    rgn := QRegion_create(@R, QRegionRegionType_Rectangle);
    try
      QRegion_subtract(rgn, Region, Region);
      Result := True;
    except
      Result := False;
    end;
    QRegion_destroy(rgn);
  except
    Result := False;
  end;
end;

function FillRgn(Handle: QPainterH; Region: QRegionH; Brush: QBrushH): LongBool;
var
  oRgn: QRegionH;
  R: TRect;
  hasClipping: Boolean;
begin
  Result := False;
  QPainter_save(Handle);
  hasClipping := QPainter_hasClipping(Handle);
  if hasClipping then
    oRgn := QPainter_clipRegion(Handle);
  if SelectClipRgn(Handle, Region) <> RGN_ERROR then
  begin
    QRegion_boundingRect(Region, @R);
    QPainter_fillRect(Handle, @R, Brush);
    if hasClipping then
      SelectClipRgn(Handle, oRgn);
    Result := True;  
  end;
  QPainter_restore(Handle);
end;

function DeleteObject(Region: QRegionH): LongBool;
begin
  try
    QRegion_destroy(Region);
    Result := True;
  except
    Result := False;
  end;
end;

function PtInRegion(Rgn: QRegionH; X, Y: Integer): Boolean;
var
  P :TPoint;
begin
  P.X := X;
  P.Y := Y;
  Result := QRegion_contains(Rgn, PPoint(@P));
end;

function RectInRegion(Rgn: QRegionH; Rect: TRect): LongBool;
var
  tmpRgn: QRegionH;
  retval: Integer;
begin
  tmpRgn := CreateRectRgnIndirect(Rect);
  try
    Retval := CombineRgn(tmpRgn, Rgn, TmpRgn, RGN_And);
    Result := (RetVal = SIMPLEREGION) or (RetVal = COMPLEXREGION);
  except
    Result := False;
  end;
  QRegion_destroy(tmpRgn);
end;

function SetViewPortOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
var
  R :TRect;
begin
  try
    QPainter_viewport(Handle, @R);
    if OldOrg <> nil then
    begin
      OldOrg.X := R.Left;
      OldOrg.Y := R.Top;
    end;
    QPainter_setViewport(Handle, X, Y, R.Right-R.Left, R.bottom-R.top);
    Result := True;
  except
    Result := False;
  end;
end;

function SetViewportExtEx(Handle: QPainterH; XExt, YExt: Integer; Size: PSize): LongBool;
var
  R :TRect;
begin
  Result := True;
  try
    QPainter_viewport(Handle, @R);
    if size <> nil then
    begin
      Size.cx := R.Right-R.Left;
      Size.cy := R.bottom-R.top;
    end;
    QPainter_setViewport(Handle, R.Left, R.Top, XExt, YExt);
  except
    Result := False;
  end;
end;

function GetViewportExtEx(Handle: QPainterH; Size: PSize): LongBool;
var
  R :TRect;
begin
  Result := True;
  try
    QPainter_viewport(Handle, @R);
    Size.cx := R.Right-R.Left;
    Size.cy := R.bottom-R.top;
  except
    Result := False;
  end;
end;

function GetWindowOrgEx(Handle: QPainterH; Org: PPoint): LongBool;
var
  R :TRect;
begin
  try
    QPainter_Window(Handle, @R);
    Org.X := R.Left;
    Org.Y := R.Top;
    Result := True;
  except
    Result := False;
  end;
end;

function SetWindowOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
var
  R :TRect;
begin
  try
    QPainter_Window(Handle, @R);
    with R do
    begin
      if OldOrg <> nil then
      begin
        OldOrg.X := Left;
        OldOrg.Y := Top;
      end;
      QPainter_setWindow(Handle, X, Y, Right - Left, Bottom - Top);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure CopyMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
begin
  Move(Src^, Dest^, Len);
end;

procedure FillMemory(Dest: Pointer; Len: Cardinal; Fill: Byte);
begin
  FillChar(Dest^, Len, Fill);
end;

procedure MoveMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
begin
  Move(Src^, Dest^, Len);
end;

procedure ZeroMemory(Dest: Pointer; Len: Cardinal);
begin
  FillChar(Dest^, Len, 0);
end;

function GetCaretBlinkTime: Cardinal;
begin
  Result := QApplication_cursorFlashTime;
end;

function SetCaretBlinkTime(uMSeconds: Cardinal): LongBool;
begin
  Result := True;
  try
    QApplication_setCursorFlashTime(uMSeconds);
  except
    Result := False;
  end;
end;

function GetDoubleClickTime: Cardinal;
begin
  Result := QApplication_doubleClickInterval;
end;

function SetDoubleClickTime(Interval: Cardinal): LongBool;
begin
  try
    QApplication_setDoubleClickInterval(Interval);
    Result := True;
  except
    Result := False;
  end;
end;

// limited implementation of
function GetSystemMetrics(PropItem: TSysMetrics): Integer;
var
  size: TSize;
begin
  case PropItem of
    SM_CXVSCROLL:
      begin
        QStyle_scrollBarExtent(QApplication_Style, @size);
        Result := size.cx;
      end;
    SM_CYVSCROLL:
      begin
        QStyle_scrollBarExtent(QApplication_Style, @size);
        Result := size.cy;
      end;
    SM_CXSMICON, SM_CYSMICON:
      Result := 16;
    SM_CXICON, SM_CYICON:
      Result := 32;
    SM_CXSCREEN:
      Result := QWidget_width(QApplication_desktop);
    SM_CYSCREEN:
      Result := QWidget_height(QApplication_desktop);
    SM_CXBORDER, SM_CYBORDER:
      Result := QStyle_DefaultFrameWidth(QApplication_style); // (probably) wrong ?
    SM_CXFRAME, SM_CYFRAME:
      Result := QStyle_DefaultFrameWidth(QApplication_style); // or this one
  else
    raise Exception.Create('GetSystemMetrics: unsupported property')
  end;
end;

{ limited implementation of}
function GetDeviceCaps(Handle: QPainterH; devcap: TDeviceCap): Integer;
var
  pdm:  QPaintDeviceMetricsH;
begin
  Result := 0;
  pdm := QPaintDeviceMetrics_create(QPainter_device(Handle));
  try
    case devcap of
    HORZSIZE:
      Result := QPaintDeviceMetrics_widthMM(pdm);
    VERTSIZE:
      Result := QPaintDeviceMetrics_heightMM(pdm);
    PHYSICALWIDTH, HORZRES:
      Result := QPaintDeviceMetrics_width(pdm); // Horizontal width in pixels
    BITSPIXEL:
      Result := QPaintDeviceMetrics_Depth(pdm); // Number of bits per pixel
    NUMCOLORS:
      Result := QPaintDeviceMetrics_numColors(pdm);
    LOGPIXELSX:
      Result := QPaintDeviceMetrics_logicalDpiX(pdm); // Logical pixelsinch in X
    LOGPIXELSY:
      Result := QPaintDeviceMetrics_logicalDpiY(pdm); // Logical pixelsinch in Y
    PHYSICALOFFSETX:
      Result := 0;
    PHYSICALOFFSETY:
      Result := 0;
    PHYSICALHEIGHT, VERTRES:
      Result := QPaintDeviceMetrics_height(pdm); // Vertical height in pixels
    else
      raise Exception.Create('GetDeviceCaps: unsupported capability');
    end;
  finally
    QPaintDeviceMetrics_destroy(pdm);
  end;
end;

{ a very limited implementation of }
function GetTextMetrics(Handle: QPainterH; tt: TTextMetric): Integer;
var
  tm: QFontMetricsH;
begin
  QPainter_FontMetrics(Handle, @tm);
  with tt do
  begin
    tmHeight := QFontMetrics_height(tm);
    tmAscent := QFontMetrics_ascent(tm);
    tmDescent := QFontMetrics_descent(tm);
  end;
  Result := 0;
end;

function RGB(Red, Green, Blue: Integer): TColorRef;
begin
  Result := (Blue shl 16) or (Green shl 8) or Red;
end;

function GetBValue(Col: TColorRef): Integer;
begin
  Result := (Col shr 16) and $FF;
end;

function GetGValue(Col: TColorRef): Integer;
begin
  Result := (Col shr 8) and $FF;
end;

function GetRValue(Col: TColorRef): Integer;
begin
  Result := Col and $FF;
end;

function SetRect(var R: TRect; left, top, right, bottom: Integer): LongBool;
begin
  R := Rect(left, top, right, bottom);
  Result := True;
end;

function CopyRect(var Dst: TRect; const Src: TRect): LongBool;
begin
  with src do
    Dst := Rect(left, top, right, bottom);
  Result := True;
end;

function UnionRect(var Dst: TRect; R1, R2: TRect): LongBool;
begin
  if IsRectEmpty(R1) then
  begin
    if IsRectEmpty(R2)
    then
      Result := False  // both empty
    else
    begin
      with R2 do dst:= rect(left, top, right, bottom);
      Result := True;
    end;
  end
  else if IsRectEmpty(R2)
  then
    with R1 do dst := rect(left, top, right, bottom)
  else
    with Dst do
    begin
      Left := Min(R1.Left, R2.Left);
      Top := Min(R1.Top, R2.Top);
      Right := Max(R1.Right, R2.Right);
      Bottom := Max(R1.Bottom, R2.Bottom);
    end;
end;

function IsRectEmpty(R: TRect): LongBool;
begin
  with R do
    Result := (Right <= Left) or (Bottom <= Top);
end;

function EqualRect(R1, R2: TRect): LongBool;
begin
  Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and
            (R1.Top = R2.Top) and (R1.Bottom = R2.Bottom)
end;

procedure TextOutAngle(ACanvas: TCanvas; Angle, Left, Top: Integer; Text: WideString);
begin
 {this code is courtesy of Jon Shemitz <jon@midnightbeach.com>}
 {Outside of a Paint handler, bracket QPainter_ calls with a Start/Stop}
  ACanvas.Start;
  try
    Qt.QPainter_save(ACanvas.Handle);
 {Move 0,0 to the center of the form}
    Qt.QPainter_translate(ACanvas.Handle, Left, Top);
 {Rotate; note negative angle:}
    QPainter_rotate(ACanvas.Handle, -Angle);
    ACanvas.TextOut(0, 0, Text);
  finally
    Qt.QPainter_restore(ACanvas.Handle);
    ACanvas.Stop;
  end;
end;

function TextWidth(Handle: QPainterH; caption: WideString;
  flags: Integer): Integer;
var
  R :TRect;
begin
  QPainter_boundingRect(Handle, @R, @R, Flags, PWideString(@Caption), -1, nil);
  Result := R.Right - R.Left;
end;

function TextHeight(Handle: QPainterH; caption: WideString; R: TRect;
  flags: Integer): Integer;
var
  R1, R2: TRect;
begin
  R1 := R;
  R1.Bottom := MaxInt;
  QPainter_boundingRect(Handle, @R1, @R2, Flags, PWideString(@Caption), -1, nil);
  Result := R2.Bottom - R2.Top;
end;

function GetTextExtentPoint32(Handle: QPainterH; Text: WideString; len: Integer;
  var size :TSize): LongBool;
var
  R: TRect;
begin
  try
    QPainter_boundingRect(Handle, @R, @R, 0, @Text, len, nil);
    with R do
    begin
      size.cx := Right - Left;
      size.cy := Bottom - Top;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function GetTextExtentPoint32(Handle: QPainterH; pText: PChar; len: Integer;
  var size :TSize): LongBool;
var
  Text: WideString;
begin
  Text := pText;
  Result := GetTextExtentPoint32(Handle, Text, len, size);
end;

function GetTextExtentPoint32W(Handle: QPainterH; pText: PWideChar; len: Integer;
  var size :TSize): LongBool;
var
  Text: WideString;
begin
  Text := pText;
  Result := GetTextExtentPoint32(Handle, text, len, size);
end;

function TextExtent(Handle: QPainterH; caption: WideString; R: TRect;
  flags: Integer): TRect;
var
  R2: TRect;
begin
  R2 := R;
  R2.Bottom := MaxInt;
  QPainter_boundingRect(Handle, @R2, @Result, Flags, PWideString(@Caption), -1, nil);
end;

const
  Ellipses = '...';

function NameEllipsis(const Name: WideString; Handle: QPainterH;
  MaxLen: Integer): WideString;
var
  I: Integer;
begin
  if TextWidth(Handle, Name, 0) > MaxLen then
  begin
    Result := Ellipses;
    I := 0;
    while TextWidth(Handle, Result, 0) <= MaxLen do
    begin
      Inc(I);
      Result := LeftStr(Name, I) + Ellipses;
    end;
    if I <> 0 then
      Result := LeftStr(Name, I-1) + Ellipses;
  end
  else
    Result := name;
end;

function WordEllipsis(Words: WideString; Handle: QPainterH; R: TRect;
  flags:Integer): WideString;
var
  R2, R1: TRect;
  ShortedText: WideString;
  I: Integer;
  function RectInsideRect(const R1, R2: TRect): Boolean;
  begin
    with R1 do
      Result := (Left >= R2.Left) and (Right  <= R2.Right) and
                (Top  >= R2.Top)  and (Bottom <= R2.Bottom);
  end;
begin
  Result := ShortedText;
  R1 := R;
  R1.Bottom := MaxInt;
  QPainter_boundingRect(Handle, @R1, @R2, Flags, PWideString(@Result), -1, nil);
  if not RectInsideRect(R2, R) then
  begin
    I := 1;
    ShortedText := '';
    while RectInsideRect(R2, R) and (I <= Length(Words)) do
    begin
      repeat                 // one more word
        ShortedText := LeftStr(Words, I);
        if Words[I] = ' '
        then
          break;
        inc(I);
      until I > Length(Words);
      Result := ShortedText + '...';
      QPainter_boundingRect(Handle, @R1, @R2, Flags, PWideString(@Result), -1, nil);
    end;
    While not RectInsideRect(R2, R) and (I > 0) do
    begin
      repeat // one word less
        dec(I);
        ShortedText := LeftStr(Words, I);
        if ShortedText[I] = ' '
        then
          break;
      until I <= 1;
      Result := ShortedText + '...';
      QPainter_boundingRect(Handle, @R1, @R2, Flags, PWideString(@Result), -1, nil);
    end;
  end;
end;

function FileEllipsis(const FilePath: AnsiString; Handle: QPainterH; MaxLen: Integer): string;
var
  Paths: TStrings;
  k, i, start: Integer;
  CurPath, F: AnsiString;
begin
  if TextWidth(Handle, FilePath, SingleLine) <= MaxLen then
    Result := FilePath
  else
  begin    // FilePath too long
     F := FilePath;
    {$IFDEF LINUX}
    CurPath := IncludeTrailingPathDelimiter(GetEnvironMentVariable('HOME'));
    if AnsiStartsStr(CurPath, Filepath)
    then
    begin
      F := '~/' + AnsiRightStr(Filepath, Length(Filepath) - Length(CurPath));
      if TextWidth(Handle, paths[0], 0) <= MaxLen
      then
      begin
        Result := F;
        exit;
      end
    end;
    {$ENDIF LINUX}
    Paths := TStringList.Create;
    try
      Paths.Delimiter := PathDelim;
      Paths.DelimitedText := F; // splits the filepath
      if Length(paths[0]) = 0 then
        start := 1     // absolute path
      else
        start := 0;    // relative path
      for k := start to Paths.Count - 2 do
      begin
        CurPath := Paths[k];
        if Length(CurPath) > 2 then   // this excludes '~' '..'
        begin
          Paths[k] := CurPath; // replace with ellipses
          I:= Length(CurPath);
          while (I > 0)  and (TextWidth(Handle, Paths.DelimitedText, 0) > MaxLen) do
          begin
            dec(I);
            Paths[k] := AnsiLeftStr(Curpath, I) + Ellipses;// remove a character
          end;
          if TextWidth(Handle, Paths.DelimitedText, 0)<= MaxLen then
          begin
            Result := Paths.DelimitedText;
            exit;
          end;
        end
      end;
      // not succeeded.
      // replace /.../.../.../<filename> with .../<filename>
      // before starting to minimize filename
      for k := Paths.count - 2 downto 1 do
        Paths.Delete(k);
      Paths[0] := Ellipses;
      if TextWidth(Handle, Paths.DelimitedText, 0) > MaxLen then
      begin
        CurPath := Paths[1];
        Paths[1] := Ellipses; // replace with ellipses
        //I := 1;
        //Paths[1] := CurPath; // replace with ellipses
        I:= Length(CurPath);
        while (I > 0)  and (TextWidth(Handle, Paths.DelimitedText,0) > MaxLen) do
        begin
          dec(I);
          Paths[I] := AnsiLeftStr(Curpath, I) + Ellipses;// remove a character
        end;
      end;
      Result := Paths.DelimitedText;    // will be something .../Progr...
    finally
      Paths.Free;
    end;
  end;
end;

function TruncatePath(const FilePath: string; Canvas: TCanvas; MaxLen: Integer): string;
begin
  Canvas.Start;
  Result := FileEllipsis(FilePath, Canvas.Handle, MaxLen);
  Canvas.Stop;
end;

function TruncateName(const Name: WideString; Canvas: TCanvas; MaxLen: Integer): WideString;
begin
  Canvas.start;
  Result := NameEllipsis(Name, Canvas.Handle, MaxLen);
  Canvas.stop;
end;

function DrawText(Handle :QPainterH; var text: WideString; len: Integer;
  var R: TRect; WinFlags: Integer): Integer;
var
  flags: Integer;
  R2: TRect;  // bliep bliep bl...
  caption: WideString;
  FontSaved, FontSet: QFontH;
  function HidePrefix(txt: WideString): WideString;
  begin
   // StrPos
    Result := txt;
  end;

  function CheckTabStop(winflags: Integer): Integer;
  var
    size: Integer;
  begin
    if WinFlags and DT_TABSTOP <> 0 then
    begin
      size := WinFlags and $FF00;
      WinFlags := WinFlags - size;
      size := (size shr 8) and $FF;
      QPainter_setTabStops(Handle, size);
    end;
    Result := WinFlags;
  end;

begin
  if WinFlags and DT_INTERNAL <> 0 then
  begin
    FontSaved := QPainter_Font(Handle);
    QApplication_font(FontSet, nil);
    QPainter_setFont(Handle, FontSet);
  end;
  flags := Win2QtAlign(CheckTabStop(WinFlags));
  (*  TODO
  if WinFlags and DT_PREFIXONLY <> 0 then
  begin

  end
  else if WinFlags and DT_HIDEPREFIX <> 0 then
  begin
    // Flags := Flags or NoPrefix;
//    Caption := HidePrefix(text);
  end;
  *)
  if Flags and Calcrect = 0 then
  begin
    if ClipName and flags <> 0
    then
      Caption := NameEllipsis(text, Handle, R.Right-R.Left)
    else if ClipPath and flags <> 0
    then
      Caption := FileEllipsis(text, Handle, R.Right-R.Left)
    else if ClipToWord and flags <> 0
    then
      Caption := WordEllipsis(text, Handle, R, flags)
    else
    begin
      Caption := text;
    end;
    QPainter_DrawText(Handle, @R, flags, PWideString(@Caption), len, @R2, nil);
    if ModifyString and flags <> 0
    then
      text := Caption;
    Result := R2.Bottom - R2.Top;
  end
  else
  begin
    QPainter_boundingRect(Handle, @R, @R, Flags, PWideString(@Text), -1, nil);
    Result := R.bottom - R.Top;
  end;
  if WinFlags and DT_INTERNAL <> 0 then
  begin
    QPainter_setFont(Handle, FontSaved);
  end;
end;

function DrawText(Handle :QPainterH; text: PChar; len: Integer;
  var R: TRect; WinFlags: Integer): Integer;
var
  wtext: WideString;
  atext: string;
begin
  wtext := text;
  Result := DrawText(Handle, wtext, Len, R, WinFlags);
  if DT_MODIFYSTRING and WinFlags <> 0 then
  begin
    atext := wtext;
    strcopy(text, PChar(atext));
  end;
end;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; Text: WideString; len: Integer;lpDx: Pointer): LongBool;
begin
  try
    if WinFlags and ETO_CLIPPED = 0 then
    begin
      DrawText(Handle, Text, len, R^, DT_CALCRECT);
//      OffsetRect(R, X - R.left, Y - R.Top);
    end;
    if WinFlags and ETO_OPAQUE <> 0
    then
      FillRect(Handle, R^, QPainter_brush(Handle));
    Result:= DrawText(Handle, Text, len, R^, 0) <> 0;
  except
    Result := False;
  end;
end;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PChar; len: Integer; lpDx: Pointer): LongBool;
var
  ws: WideString;
begin
  ws := pText;
  Result := ExtTextOut(Handle, X, Y, WinFlags, R, ws, len, lpDx);
end;

function ExtTextOutW(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PWideChar; len: Integer; lpDx: Pointer): LongBool;
var
  ws: WideString;
begin
  ws := pText;
  Result := ExtTextOut(Handle, X, Y, WinFlags, R, ws, len, lpDx);
end;

function FillRect(Handle: QPainterH; R: TRect; Brush: QBrushH): LongBool;
begin
  try
    QPainter_fillRect(Handle, @R, Brush);
    Result := True;
  except
    Result := False;
  end;
end;

function DrawIcon(Handle: QPainterH; X, Y: Integer; hIcon: QPixMapH): LongBool;
var
  point: TPoint;
begin
  point.X := X;
  point.Y := Y;
  try
    QPainter_drawPixmap(Handle, @point, hIcon);
    Result := True;
  except
    Result := False;
  end;
end;

function DrawIconEx(Handle: QPainterH; X, Y: Integer; hIcon: QPixMapH; W, H: Integer;
  istepIfAniCur: Integer; hbrFlickerFreeDraw: QBrushH; diFlags: Cardinal): LongBool;
var
  TempDC: QPainterH;
  R: TRect;
begin
  if diFlags = DI_DEFAULTSIZE then
  begin
    if W = 0 then
    begin
      W := GetSystemMetrics(SM_CYICON);
      istepIfAniCur := 0;
    end;
    if H = 0
    then
      H := GetSystemMetrics(SM_CXICON);
  end
  else
  begin         // DI_NORMAL / DI_IMAGE / DI_MASK
    if W = 0 then
    begin
      W := QPixmap_width(hIcon);
      istepIfAniCur := 0;
    end;
    if H = 0
    then
      H := QPixmap_height(hIcon);
    if (DiFlags and DI_MASK) = 0     // DI_NORMAL / DT_IMAGE
    then
      QPixmap_setMask(hIcon, nil);
  end;

  if QPixmap_width(hIcon) < ((istepIfAniCur + 1) * W)
  then
    istepIfAniCur := 0;

  if hbrFlickerFreeDraw <> nil then
  begin
    R := Bounds(0, 0, W, H);
    try
      TempDC := CreateCompatibleBitmap(Handle, W, H);
      try
        FillRect(TempDC, R, hbrFlickerFreeDraw);
        QPainter_drawPixmap(TempDC, 0, 0, hIcon, istepIfAniCur * W, 0, W, H);
        BitBlt(Handle, X, Y, W, H, TempDC, 0, 0, RasterOp_CopyRop);
        Result := True;
      except
        Result := False;
      end;
      DeleteObject(TempDC);
    except
      Result := False;
    end;
  end
  else
  begin
    try
      QPainter_drawPixmap(Handle, X, Y, hIcon, istepIfAniCur * W, 0, W, H);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function InvertRect(Handle: QPainterH; const R: TRect): LongBool;
begin
  with R do
    Result := BitBlt(Handle, Left, Top, Right - Left, Bottom-Top,
                     Handle, Left, Top, DSTINVERT);
end;

function Rectangle(Handle: QPainterH; Left, Top, Right, Bottom: Integer): LongBool;
begin
  try
    QPainter_drawRect(Handle, Left, Top, Right, Bottom);
    Result := True;
  except
    Result := False;
  end;
end;

function RoundRect(Handle: QPainterH; Left, Top, Right, Bottom, X3, Y3: Integer): LongBool;
begin
  try
    QPainter_drawRoundRect(Handle, Left, Top, Right, Bottom, X3, Y3);
    Result := True;
  except
    Result := False;
  end;
end;

function Ellipse(Handle: QPainterH; Left, Top, Right, Bottom: Integer): LongBool;
begin
  try
    QPainter_drawEllipse(Handle, Left, Top, Right, Bottom);
    Result := True;
  except
    Result := False;
  end;
end;

function InvalidateRect(Handle: QWidgetH; R: PRect;
  erasebackground: Boolean): LongBool;
begin
  try
    QWidget_repaint(Handle, R, erasebackground);
    Result := True;
  except
    Result := False;
  end;
end;

function DrawFocusRect(Handle: QPainterH; const R: TRect): LongBool;
begin
  try
    QPainter_drawWinFocusRect(Handle, @R);
    Result := True;
  except
    Result := False;
  end;
end;

function GetCurrentPositionEx(Handle: QPainterH; pos: PPoint): LongBool;
begin
  try
    QPainter_pos(Handle, pos);
    Result := True;
  except
    Result := False;
  end;
end;

function LineTo(Handle: QPainterH; X, Y:Integer): LongBool;
begin
  try
    QPainter_lineTo(Handle, X, Y);
    Result := True;
  except
    Result := False;
  end;
end;

function MoveToEx(Handle: QPainterH; X, Y:Integer; oldpos: PPoint): LongBool;
begin
  try
    if oldpos <> nil
    then
      QPainter_pos(Handle, oldpos);
    QPainter_moveTo(Handle, X, Y);
    Result := True;
  except
    Result := False;
  end;
end;

function GetDC(Handle: QWidgetH): QPainterH;
var
  PaintDevice: QPaintDeviceH;
begin
  try
    if Handle = nil then
      Handle := QApplication_desktop;
    PaintDevice := QWidget_to_QPaintDevice(Handle);
    Result := QPainter_create(PaintDevice, Handle);
    if not QPainter_isActive(Result) then
      QPainter_begin(Result, PaintDevice, Handle);
  except
    Result := nil;
  end;
end;

function GetDC(Handle: Integer): QPainterH;
begin
  Result := GetDC(QWidgetH(Handle));
end;

procedure ReleaseDC(wdgtH: QWidgetH; Handle: QPainterH);
begin
  // asn: wdgtH ignored
  QPainter_end(Handle);
  QPainter_destroy(Handle);
end;

procedure ReleaseDC(wdgtH: Integer; Handle: QPainterH);
begin
  ReleaseDC(QWidgetH(wdgtH), Handle);
end;

function CreateCompatibleBitmap(Handle: QPainterH; Width, Height: Integer): QPainterH;
var
  depth: Integer;
  pixmap: QPixmapH;
  pdm: QPaintDeviceMetricsH;
begin
  pdm := QPaintDeviceMetrics_create(QPainter_device(Handle));
  try
    depth := QPaintDeviceMetrics_depth(pdm);
    pixmap := QPixmap_create(Width, Height, depth,
                           QPixmapOptimization_DefaultOptim);
    try
      Result := QPainter_create(pixmap);
      QPainter_setPen(Result, QPainter_pen(Handle));
      QPainter_setBackgroundColor(Result, QPainter_BackgroundColor(Handle));
      QPainter_setFont(Result, QPainter_Font(Handle));
      QPainter_begin(Result, QPainter_device(Result));
    except
      Result := nil;
    end;
  finally
    QPaintDeviceMetrics_destroy(pdm);
  end;
end;

function SetPixel(Handle: QPainterH; X, Y: Integer; Color: TColorRef): TColorRef;
var
  Brsh: QBrushH;
  oRop: RasterOp;
  R: TRect;
begin
  Brsh := CreateSolidBrush(Color);
  R := Bounds(X, Y, 1, 1);
  oRop := QPainter_rasterOp(Handle);
  QPainter_setRasterOp(Handle, RasterOp_CopyROP);
  FillRect(Handle, R, Brsh);
  QPainter_setRasterOp(Handle, oRop);
  QBrush_destroy(Brsh);
  Result := GetPixel(Handle, X, Y);
end;

function GetPixel(Handle: QPainterH; X, Y: Integer): TColorRef;
var
  depth: Integer;
  pixmap: QPixmapH;
  pdm: QPaintDeviceMetricsH;
  tempDC: QPainterH;
  img: QImageH;
begin
  pdm := QPaintDeviceMetrics_create(QPainter_device(Handle));
  try
    depth := QPaintDeviceMetrics_depth(pdm);
    QPaintDeviceMetrics_destroy(pdm);
    pixmap := QPixmap_create(2, 2, depth, QPixmapOptimization_NoOptim);
    tempDC := QPainter_create(pixmap);
    bitblt(tempDC, 0, 0, 1, 1, Handle, X, Y, RasterOp_CopyROP);
    img := QImage_create;
    QPixmap_convertToImage(pixmap, img);
    Result := QImage_pixelIndex(img, 0, 0);
    QImage_destroy(img);
    QPainter_destroy(tempdc);
    QPixmap_destroy(pixmap);
  except
    Result := 0;
  end;
end;

function DeleteObject(Handle: QPainterH): LongBool;
var
  pixmap: QPaintDeviceH;
begin
  try
    pixmap := QPainter_device(Handle); // get paintdevice
    if QPainter_isActive(Handle)
    then
      QPainter_end(Handle);
    QPainter_destroy(Handle);  // destroy painter
    QPixmap_destroy(QPixmapH(pixmap)); // destroy pixmap paintdevice
    Result := True;
  except
    Result := False;
  end;
end;

function SaveDC(Handle: QPainterH): Integer;
begin
  try
    QPainter_save(Handle);
    Result := -1;
  except
    Result := 0;
  end;
end;

{ only negative values of nSaveDC are supported }
function RestoreDC(Handle: QPainterH; nSavedDC: Integer): LongBool;
var
  i: Integer;
begin
  try
    for i:= nSavedDC-1 to 0
    do
      QPainter_restore(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function HWND_DESKTOP: QWidgetH;
begin
  Result := QApplication_desktop;
end;

// maps DT_ alignment flags to Qt (extended) alignment flags
function Win2QtAlign(Flags: Integer): Integer;
begin
  Result := 0;
  // Singleline & multiline
  if Flags and DT_SINGLELINE <> 0 then
     Result := SingleLine
  // multiline:
  else if Flags and DT_WORDBREAK <> 0 then
    Result := Result or WordBreak;
//  else
//    Result := Result or BreakAnywhere;
  // <tab> and '&' prefix
  if Flags and DT_EXPANDTABS <> 0 then
    Result := Result or ExpandTabs;
  if Flags and DT_NOPREFIX = 0 then
    Result := Result or ShowPrefix;
  // Horizontal alignment
  if Flags and DT_RIGHT <> 0 then
    Result := Result or AlignRight
  else if Flags and DT_CENTER <> 0 then
    Result := Result or AlignHCenter
  else
    Result := Result or AlignLeft; // default
  // vertical alignment
  if Flags and DT_BOTTOM <> 0
  then
    Result := Result or AlignTop
  else if Flags and DT_VCENTER <> 0
  then
    Result := Result or AlignVCenter
  else
    Result := Result or AlignTop;  // default
  // extended Qt alignments
  if Flags and DT_CALCRECT <> 0
  then
    Result := Result or CalcRect
  else
  begin                            //
    if Flags and DT_ELLIPSIS <> 0 then
      Result := Result or ClipName or SingleLine
    else if Flags and DT_PATH_ELLIPSIS <> 0 then
      Result := Result or ClipPath or SingleLine
    else if Flags and DT_WORD_ELLIPSIS <> 0 then
      Result := Result or ClipToWord;
    if Flags and DT_MODIFYSTRING <> 0 then
      Result := Result or ModifyString;
  end;
end;

// strips Qt extended alignment from flags
function QtStdAlign(Flags: Integer): Word;
begin
  Result := Word(Flags and QtAlignMask);
end;

{$IFDEF LINUX}

uses
  Libc;

function FileGetAttr(const FileName: string): Integer;
var
  sr: TSearchRec;
  valid: Boolean;
begin
  Result := 0;
  valid := FindFirst(FileName, faAnyFile, sr) = 0;
  if valid then
  begin
    Result := sr.attr;
    FindClose(sr);
  end;
end;

function FileGetSize(const FileName: string): Cardinal;
var
  sr: TSearchRec;
  valid: Boolean;
begin
  Result := 0;
  valid := FindFirst(FileName, faAnyFile, sr) = 0;
  if valid then
  begin
    Result := sr.size;
    FindClose(sr);
  end;
end;

function CopyFile(const source: string; const destination: string; FailIfExists: Boolean): LongBool;
const
  ChunkSize = 8192;
var
  CopyBuffer: Pointer;
  Src, Dest: Integer;
  {FSize,} BytesCopied {, TotalCopied}: Longint;
  DestName: string;
begin
  Result := False;
  if DirectoryExists(Destination)
  then
    DestName := IncludeTrailingPathDelimiter(Destination) + ExtractFileName(Source)
  else
    DestName := Destination;
  if FailIfExists  and FileExists(DestName) then
  begin
    exit;
  end;
  GetMem(CopyBuffer, ChunkSize);
  try
    ForceDirectories(ExtractFilePath(Destination));
    Dest := FileCreate(DestName);
    if Dest < 0 then
      raise EFCreateError.CreateFmt(SFCreateError, [DestName]);
    try
      // TotalCopied := 0;
      Src := FileOpen(source, fmShareDenyWrite);
      if Src < 0 then
        raise EFOpenError.CreateFmt(SFOpenError, [source]);
      try
        //FSize := GetFileSize(source);
        repeat
        begin
          BytesCopied := FileRead(Src, CopyBuffer^, ChunkSize);
          if BytesCopied = -1 then
            raise EReadError.Create(SReadError);
          // TotalCopied := TotalCopied + BytesCopied;
          if BytesCopied > 0 then
          begin
            if FileWrite(Dest, CopyBuffer^, BytesCopied) = -1 then
            raise EWriteError.Create(SWriteError);
          end;
        end
        until BytesCopied < ChunkSize;
        FileSetDate(DestName, FileGetDate(Src));
        Result := True;
      finally
        FileClose(Dest);
      end;
    finally
      FileClose(Src);
    end;
  finally
    FreeMem(CopyBuffer, ChunkSize);
  end;
end;

function CopyFile(lpExistingFileName, lpNewFileName: PChar;
  bFailIfExists: LongBool): LongBool;
var
  EF, NF: string;
begin
  EF := lpExistingFileName;
  NF := lpNewFilename;
  Result :=  CopyFile(EF, NF, Boolean(bFailIfExists));
end;

function CopyFileA(lpExistingFileName, lpNewFileName: PAnsiChar;
  bFailIfExists: LongBool): LongBool;
var
  EF, NF: string;
begin
  EF := lpExistingFileName;
  NF := lpNewFilename;
  Result :=  CopyFile(EF, NF, Boolean(bFailIfExists));
end;

function CopyFileW(lpExistingFileName, lpNewFileName: PWideChar;
  bFailIfExists: LongBool): LongBool;
var
  EF, NF: string;
begin
  EF := lpExistingFileName;
  NF := lpNewFilename;
  Result := CopyFile(EF, NF, bFailIfExists);
end;

function MakeIntResource(Value: Integer): PChar;
begin
  Result := PChar(Value and $0000ffff);
end;

procedure MessageBeep(Value: Integer);
begin
  QApplication_beep;
end;

var
  StartTimeVal: TTimeVal;

// linux systems tend to run for months,
// rolls over after 49.7 days since application start
// predictable !
// return value: number of milliseconds since application start

function GetTickCount: Cardinal;
var
  TimeVal: TTimeVal;
begin
  gettimeofday(TimeVal, nil);
  Result := 1000 * (TimeVal.tv_sec - StartTimeVal.tv_sec) +
   (TimeVal.tv_usec- StartTimeVal.tv_usec) div 1000;
end;

procedure InitGetTickCount;
begin
  gettimeofday(StartTimeVal, nil);
end;

{$ENDIF LINUX}

// used internally
procedure MapPainterLP(Handle: QPainterH; var x, y: Integer); overload;
var
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(Handle) then
  begin
    Matrix := QPainter_worldMatrix(Handle);
    QWMatrix_map(Matrix, x, y, @x, @y);
  end;
end;

procedure MapPainterLP(Handle: QPainterH; var x0, y0, x1, y1: Integer); overload;
var
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(Handle) then
  begin
    Matrix := QPainter_worldMatrix(Handle);
    QWMatrix_map(Matrix, x0, y0, @x0, @y0);
    QWMatrix_map(Matrix, x1, y1, @x1, @y1);
  end;
end;

procedure MapPainterLP(Handle: QPainterH; var R: TRect); overload;
var
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(Handle) then
  begin
    Matrix := QPainter_worldMatrix(Handle);
    QWMatrix_map(Matrix, PRect(@R), PRect(@R));
  end;
end;

procedure MapPainterLP(Handle: QPainterH; var Pt: TPoint); overload;
var
  Matrix: QWMatrixH;
begin
  if QPainter_hasWorldXForm(Handle) then
  begin
    Matrix := QPainter_worldMatrix(Handle);
    QWMatrix_map(Matrix, PPoint(@Pt), PPoint(@Pt));
  end;
end;

function LPtoDP(Handle: QPainterH; var Points; Count: Integer): LongBool;
var
  Matrix: QWMatrixH;
  P: PPoint;
begin
  Result := True;
  try
    if QPainter_hasWorldXForm(Handle) then
    begin
      Matrix := QPainter_worldMatrix(Handle);
      P := @Points;
      while Count > 0 do
      begin
        Dec(Count);
        QWMatrix_map(Matrix, P, P);
        Inc(P);
      end;
    end;
  except
    Result := False;
  end;
end;

function DPtoLP(Handle: QPainterH; var Points; Count: Integer): LongBool;
var
  Matrix: QWMatrixH;
  P: PPoint;
  dx, dy, m11, m12, m21, m22: Double;
begin
  Result := True;
  try
    if QPainter_hasWorldXForm(Handle) then
    begin
      Matrix := QPainter_worldMatrix(Handle);
      P := @Points;
      while Count > 0 do
      begin
        Dec(Count);
        dx := QWMatrix_dx(Matrix);
        dy := QWMatrix_dy(Matrix);
        m11 := QWMatrix_m11(Matrix);
        m12 := QWMatrix_m12(Matrix);
        m21 := QWMatrix_m21(Matrix);
        m22 := QWMatrix_m22(Matrix);

        with P^ do
        begin
          X := Round(X / m11 - Y / m21 - dx);
          Y := Round(X / m12 - Y / m22 - dy);
        end;
        Inc(P);
      end;
    end;
  except
    Result := False;
  end;
end;

function ExcludeClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer;
var
  ExcludeRgn, Rgn: QRegionH;
begin
  MapPainterLP(Handle, X1, Y1, X2, Y2);
  ExcludeRgn := QRegion_create(X1, Y1, X2 - X1, Y2 - Y1, QRegionRegionType_Rectangle);
  try
    Rgn := QPainter_clipRegion(Handle);
    QRegion_subtract(Rgn, Rgn, ExcludeRgn);
    QPainter_setClipping(Handle, True);
    Result := GetRegionType(Rgn);
  except
    Result := RGN_ERROR;
  end;
  QRegion_destroy(ExcludeRgn);
end;

function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer;
var
  IntersectRgn, Rgn: QRegionH;
  ClipRect: TRect;
begin
  SetRect(ClipRect, X1, Y2, X2, Y2);
  MapPainterLP(Handle, ClipRect);
  IntersectRgn := QRegion_create(@ClipRect, QRegionRegionType_Rectangle);
  try
    Rgn := QPainter_clipRegion(Handle);
    if QRegion_isNull(Rgn) then
      QRegion_unite(Rgn, Rgn, IntersectRgn)
    else
      QRegion_intersect(Rgn, Rgn, IntersectRgn);
    QPainter_setClipping(Handle, True);
    Result := GetRegionType(Rgn);
  except
    Result := RGN_ERROR;
  end;
  QRegion_destroy(IntersectRgn);
end;

function SelectClipRgn(Handle: QPainterH; Region: QRegionH): integer;
var
  rgn: QRegionH;
  x, y: Integer;
begin
  try
    x := 0;
    y := 0;
    MapPainterLP(Handle, x, y);
    rgn := QRegion_create(Region);
    try
      QRegion_translate(rgn, x, y);
      QPainter_setClipRegion(Handle , rgn);
    finally
      QRegion_destroy(rgn);
    end;
    QPainter_setClipping(Handle, True);
    Result := GetRegionType(Region);
  except
    Result := RGN_ERROR;
  end;
end;

function CreateRoundRectRgn(x1, y1, x2, y2, WidthEllipse, HeightEllipse: Integer): QRegionH;
var
  Ellipse, R: QRegionH;
begin
  Result := CreateRectRgn(x1 + WidthEllipse div 2, y1 + HeightEllipse div 2,
                          x2 - WidthEllipse div 2, y2 - HeightEllipse div 2);

  // left/top corner
  Ellipse := CreateEllipticRgn(x1, y1, x1 + WidthEllipse, y1 + HeightEllipse);
  CombineRgn(Result, Result, Ellipse, RGN_OR);
  DeleteObject(Ellipse);

  // right/top corner
  Ellipse := CreateEllipticRgn(x2 - WidthEllipse, y1, x2, y1 + HeightEllipse);
  CombineRgn(Result, Result, Ellipse, RGN_OR);
  DeleteObject(Ellipse);

  // left/bottom corner
  Ellipse := CreateEllipticRgn(x1, y2 - HeightEllipse, x1 + WidthEllipse, y2);
  CombineRgn(Result, Result, Ellipse, RGN_OR);
  DeleteObject(Ellipse);

  // right/bottom corner
  Ellipse := CreateEllipticRgn(x2 - WidthEllipse, y2 - HeightEllipse, x2, y2);
  CombineRgn(Result, Result, Ellipse, RGN_OR);
  DeleteObject(Ellipse);

  // top rect
  R := CreateRectRgn(x1 - WidthEllipse, y1, x2 - WidthEllipse, y1 + HeightEllipse);
  CombineRgn(Result, Result, R, RGN_OR);
  DeleteObject(Ellipse);

  // bottom rect
  R := CreateRectRgn(x1 - WidthEllipse, y2 - HeightEllipse, x2 - WidthEllipse, y2);
  CombineRgn(Result, Result, R, RGN_OR);
  DeleteObject(Ellipse);

  // left rect
  R := CreateRectRgn(x1, y1 + HeightEllipse, x1 + WidthEllipse, y2 - HeightEllipse);
  CombineRgn(Result, Result, R, RGN_OR);
  DeleteObject(Ellipse);

  // right rect
  R := CreateRectRgn(x2 - WidthEllipse, y1 + HeightEllipse, x2, y2 - HeightEllipse);
  CombineRgn(Result, Result, R, RGN_OR);
  DeleteObject(Ellipse);
end;

function StretchBlt(DestHandle: QPainterH; X, Y, Width, Height: Integer; SrcHandle: QPainterH;
  XSrc, YSrc, SrcWidth, SrcHeight: Integer; Rop: RasterOp): LongBool;
var
  Pixmap: QPixmapH;
  NewMatrix: QWMatrixH;
  SavedRop: RasterOp;
begin
  Result := False;
  if (DestHandle = nil) or (SrcHandle = nil) then
    Exit;
  Result := True;
  try
    Pixmap := QPixmap_create(SrcWidth, SrcHeight, 32, QPixmapOptimization_BestOptim);
    try
      // copy to pixmap
      bitBlt(Pixmap, 0, 0, QPainter_device(SrcHandle),
        XSrc, YSrc, SrcWidth, SrcHeight,
        RasterOp_CopyROP, True);

      // draw stretched pixmap
      SavedRop := QPainter_rasterOp(DestHandle);
      try
        QPainter_setRasterOp(DestHandle, Rop);
        QPainter_saveWorldMatrix(DestHandle);
        try
          NewMatrix := QWMatrix_create(
            Width / SrcWidth, 0,  // x factor
            0, Height / SrcHeight, // y factor
            X, Y); // translate
          try
            QPainter_setWorldMatrix(DestHandle, NewMatrix, True);
            QPainter_drawPixmap(DestHandle, 0, 0, Pixmap, 0, 0, SrcWidth, SrcHeight);
          finally
            QWMatrix_destroy(NewMatrix);
          end;
        finally
          QPainter_restoreWorldMatrix(DestHandle);
        end;
      finally
        QPainter_setRasterOp(DestHandle, SavedRop);
      end;
    finally
      QPixmap_destroy(Pixmap);
    end;
  except
    Result := False;
  end;
end;

function GetClientRect(Handle: QWidgetH; var R: TRect): LongBool;
var
  Control: TWidgetControl;
begin
  try
    // some CLX controls have a user modified ClientRect
    Control := FindControl(Handle);
    if Control <> nil then
      R := Control.ClientRect
    else
      QWidget_rect(Handle, @R);
    Result := True;
  except
    Result := False;
  end;
end;

function WindowFromDC(Handle: QPainterH): QWidgetH;
var
  WinId: Cardinal;
  {$IFDEF MSWINDOWS}
  WinDC: Windows.HDC;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  dev: QPaintDeviceH;
  {$ENDIF LINUX}
begin
  Result := nil;
  WinId := 0;
  {$IFDEF MSWINDOWS}
  WinDC := QPainter_handle(Handle);
  if WinDC <> 0 then
    WinId := Cardinal(Windows.WindowFromDC(WinDC));
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  dev := QPainter_device(Handle);
  if dev <> nil then
    WinId := Cardinal(QPaintDevice_handle(dev));
  {$ENDIF LINUX}
  if WinId <> 0 then
    Result := QWidget_find(WinId);
end;

{ ------------ Caret -------------- }
type
  TEmulatedCaret = class(TObject)
  private
    FTimer: TTimer;
    FWndId: Cardinal;
    FWidget: QWidgetH;
    FPixmap: QPixmapH;
    FWidth, FHeight: Integer;
    FPos: TPoint;
    FVisible: Boolean;
    FShown: Boolean;
    FCritSect: TRTLCriticalSection;
    procedure SetPos(const Value: TPoint);
  protected
    procedure DoTimer(Sender: TObject);
    procedure DrawCaret; virtual;
    function CreateColorPixmap(Color: Cardinal): QPixmapH;
    procedure SetWidget(AWidget: QWidgetH);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock;
    procedure Unlock;

    function CreateCaret(AWidget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
    function DestroyCaret: Boolean;

    function IsValid: Boolean;

    function Show(AWidget: QWidgetH): Boolean;
    function Hide: Boolean;

    property Timer: TTimer read FTimer;
    property Pos: TPoint read FPos write SetPos;
  end;

var
  GlobalCaret: TEmulatedCaret;

function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.CreateCaret(Widget, Pixmap, Width, Height);
  finally
    GlobalCaret.Unlock;
  end;
end;

function CreateCaret(Widget: QWidgetH; ColorCaret: Cardinal; Width, Height: Integer): Boolean;
begin
  Result := CreateCaret(Widget, QPixmapH(ColorCaret), Width, Height);
end;

function GetCaretBlinkTime: Cardinal;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Timer.Interval;
  finally
    GlobalCaret.Unlock;
  end;
end;

function SetCaretBlinkTime(MSeconds: Cardinal): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    GlobalCaret.Timer.Interval := MSeconds;
  finally
    GlobalCaret.Unlock;
  end;
end;

function HideCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Hide;
  finally
    GlobalCaret.Unlock;
  end;
end;

function ShowCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.Show(Widget);
  finally
    GlobalCaret.Unlock;
  end;
end;

function SetCaretPos(X, Y: Integer): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    GlobalCaret.Pos := Point(X, Y);
  finally
    GlobalCaret.Unlock;
  end;
end;

function GetCaretPos(var Pt: TPoint): Boolean;
begin
  Result := True;
  GlobalCaret.Lock;
  try
    Pt := GlobalCaret.Pos;
  finally
    GlobalCaret.Unlock;
  end;
end;

function DestroyCaret: Boolean;
begin
  GlobalCaret.Lock;
  try
    Result := GlobalCaret.DestroyCaret;
  finally
    GlobalCaret.Unlock;
  end;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCritSect);

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  {$IFDEF MSWINDOWS}
  FTimer.Interval := Windows.GetCaretBlinkTime;
  {$ELSE}
  FTimer.Interval := 800;
  {$ENDIF MSWINDOWS}
  FTimer.OnTimer := DoTimer;
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;
  FTimer.Free;
  DeleteCriticalSection(FCritSect);
  inherited Destroy;
end;

function TEmulatedCaret.CreateCaret(AWidget: QWidgetH; Pixmap: QPixmapH;
  Width, Height: Integer): Boolean;
begin
  DestroyCaret;
  SetWidget(AWidget);
  FWidth := Width;
  FHeight := Height;
  if Cardinal(Pixmap) > $FFFF then
    FPixmap := QPixmap_create(Pixmap)
  else
    FPixmap := CreateColorPixmap(Integer(Pixmap));

  Result := IsValid;
end;

function TEmulatedCaret.DestroyCaret: Boolean;
begin
  Hide;
  if Assigned(FPixmap) then
    QPixmap_destroy(FPixmap);
  FWidget := nil;
  FPixmap := nil;
  FWidth := 0;
  FHeight := 0;
  Result := not IsValid;
end;

procedure TEmulatedCaret.DrawCaret;
var
  DestDev: QPaintDeviceH;
  R: TRect;
begin
  if IsValid then
  begin
    DestDev := QWidget_to_QPaintDevice(FWidget);
    R := Rect(0, 0, QPixmap_width(FPixmap), QPixmap_height(FPixmap));

    BitBlt(DestDev, @FPos, FPixmap, @R, RasterOp_XorROP);
    FShown := not FShown;
  end;
end;

function TEmulatedCaret.Show(AWidget: QWidgetH): Boolean;
begin
  if FWidget <> AWidget then
    Hide;
  SetWidget(AWidget);
  Result := IsValid;
  if Result then
  begin
    FVisible := True;
    FTimer.Enabled := True;
    DrawCaret;
  end;
end;

function TEmulatedCaret.Hide: Boolean;
begin
  Result := IsValid;
  if Result then
  begin
    FVisible := False;
    FTimer.Enabled := False;
    if FShown then
      DrawCaret;
    FShown := False;
  end;
end;

procedure TEmulatedCaret.SetPos(const Value: TPoint);
begin
  if FVisible then
  begin
    Hide;
    try
      FPos := Value;
    finally
      Show(FWidget);
    end;
  end
  else
    FPos := Value;
end;

procedure TEmulatedCaret.DoTimer(Sender: TObject);
begin
  DrawCaret;
end;

procedure TEmulatedCaret.Lock;
begin
  EnterCriticalSection(FCritSect);
end;

procedure TEmulatedCaret.Unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

function TEmulatedCaret.CreateColorPixmap(Color: Cardinal): QPixmapH;
var
  QC: QColorH;
begin
  if (FWidth <= 0) or (FHeight <= 0) then
    Result := nil
  else
  begin
    case Color of
      0:
        QC := QColor(clWhite);
      1:
        QC := QColor(clGray);
    else
      Result := nil;
      Exit;
    end;
    try
      Result := QPixmap_create(FWidth, FHeight, 32, QPixmapOptimization_MemoryOptim);
      try
        QPixmap_fill(Result, QC);
      except
        QPixmap_destroy(Result);
        Result := nil;
      end;
    finally
      QColor_destroy(QC);
    end;
  end;
end;

function TEmulatedCaret.IsValid: Boolean;
begin
  Result := (FWidget <> nil) and (FPixmap <> nil) and
    (QWidget_find(FWndId) <> nil);
end;

procedure TEmulatedCaret.SetWidget(AWidget: QWidgetH);
begin
  FWidget := AWidget;
  if FWidget <> nil then
    FWndId := QWidget_winId(FWidget)
  else
    FWndId := 0;
end;

initialization
  {$IFDEF LINUX}
  InitGetTickCount;
  {$ENDIF LINUX}
  GlobalCaret := TEmulatedCaret.Create;

finalization
  GlobalCaret.Free;

end.
