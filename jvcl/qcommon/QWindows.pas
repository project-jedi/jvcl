{-------------------------------------------------------------------------------
 QWindows.pas

 Copyright (c) 2003,2004, Andre Snepvangers (asn att xs4all dott nl),

 All rights reserved.

 Version 1.0
 Description: Qt based wrappers for common MS Windows API's
 Purpose: Reduce coding effort for porting VCL based components to VisualCLX
          compatible components. Simplify VCL code sharing.

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files(the "Software"), to deal in
 the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is furnished
 to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 The origin of this software must not be misrepresented, you must
 not claim that you wrote the original software. If you use this
 software in a product, an acknowledgment in the product documentation
 would be appreciated but is not required.

 Altered source versions must be plainly marked as such, and must not
 be misrepresented as being the original software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
--------------------------------------------------------------------------------

Known Issues:
  - Covers only a small part of the Windows APIs
  - Not all functionality is supported
{-----------------------------------------------------------------------------}
// $Id$

unit QWindows;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, DateUtils,
  {$ENDIF LINUX}
  Types, StrUtils, SysUtils, Classes, Math, Contnrs, SyncObjs, QDialogs,
  QTypes, Qt, QConsts, QGraphics, QControls, QForms, QExtCtrls, QStdCtrls,
  QButtons, QImgList, QStyle;

type
  IPerformControl = interface
    ['{B11AA73D-D7C2-43E5-BED8-8F82DE6152AB}']
    function Perform(Msg: Cardinal; WPar, LPar: Longint): Longint;
  end;

{$IFDEF LINUX}
resourcestring
  SFCreateError = 'Unable to create file %s';
  SFOpenError = 'Unable to open file %s';
  SReadError = 'Error reading file';
  SWriteError = 'Error writing file';
  SQThreadError = 'Thread Error in QWindows: %s (%d)';
{$ENDIF LINUX}

var
  NewStyleControls: Boolean = True;

const
  { SetBkMode: background modes }
  TRANSPARENT = 1; // BGMode_TransparentMode
  OPAQUE      = 2; // BGMode_OpaqueMode

{ constants for CreateDIBitmap }
  CBM_INIT = 4;     { initialize bitmap  }
  DIB_RGB_COLORS = 0;
//  DIB_PAL_COLORS = 1; // not supported by CreateDIBitmap

  { windows symbolic colors }         { mapping VisualCLX Symbolic colors}
  COLOR_SCROLLBAR = 0;                // clNormalButton
  COLOR_BACKGROUND = 1;               // clNormalBackground
  COLOR_ACTIVECAPTION = 2;            // clActiveHighlightedText
  COLOR_INACTIVECAPTION = 3;          // clDisabledHighlightedText
  COLOR_MENU = 4;                     // clNormalMid
  COLOR_WINDOW = 5;                   // clNormalBase
  COLOR_WINDOWFRAME = 6;              // clNormalHighlight
  COLOR_MENUTEXT = 7;                 // clNormalButtonText
  COLOR_WINDOWTEXT = 8;               // clNormalText
  COLOR_CAPTIONTEXT = 9;              // clNormalHighlightedText
  COLOR_ACTIVEBORDER = 10;            // clActiveHighlight
  COLOR_INACTIVEBORDER = 11;          // clDisabledHighlight
  COLOR_APPWORKSPACE = 12;            // clNormalMid
  COLOR_HIGHLIGHT = 13;               // clNormalHighlight
  COLOR_HIGHLIGHTTEXT = 14;           // clNormalHighlightedText
  COLOR_BTNFACE = 15;                 // clNormalButton
  COLOR_BTNSHADOW = $10;              // clNormalDark
  COLOR_GRAYTEXT = 17;                // clNormalDisabledText
  COLOR_BTNTEXT = 18;                 // clNormalButtonText
  COLOR_INACTIVECAPTIONTEXT = 19;     // clDisabledHighlightedText
  COLOR_BTNHIGHLIGHT = 20;            // clActiveLight
  COLOR_3DDKSHADOW = 21;              // clNormalMid
  COLOR_3DLIGHT = 22;                 // clNormalMidLight
  COLOR_INFOTEXT = 23;                // clNormalText
  COLOR_INFOBK = 24;                  // TColor($E1FFFF)
//             = 25;                  // ?? (asn: defined as clBlack for now)
  COLOR_HOTLIGHT = 26;                // clActiveHighlight (asn: ??)
  COLOR_GRADIENTACTIVECAPTION = 27;   // clActiveHighLight (asn: ??)
  COLOR_GRADIENTINACTIVECAPTION = 28; // clDisabledHighlight (asn: ??)

  COLOR_ENDCOLORS = COLOR_GRADIENTINACTIVECAPTION;
(*
  COLOR_MENUHILIGHT = 29;
  COLOR_MENUBAR = 30;
  COLOR_ENDCOLORS = COLOR_MENUBAR;
*)

  COLOR_DESKTOP     = COLOR_BACKGROUND;
  COLOR_3DFACE      = COLOR_BTNFACE;
  COLOR_3DSHADOW    = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT   = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT  = COLOR_BTNHIGHLIGHT;

  { CombineRgn return values }
  NULLREGION    = 1;     // Region is empty
  SIMPLEREGION  = 2;     // Region is a rectangle
  COMPLEXREGION = 3;     // Region is not a rectangle
  ERROR         = 0;     // Region error
  RGN_ERROR     = ERROR;

  { constants for CreatePolygon  }
  ALTERNATE     = 1;
  WINDING       = 2;

  { flags for DrawFrameControl }
  DFC_CAPTION   = 1;
  DFC_MENU      = 2;
  DFC_SCROLL    = 3;
  DFC_BUTTON    = 4;
  DFC_POPUPMENU = 5;

  DFCS_CAPTIONCLOSE   = 0;
  DFCS_CAPTIONMIN     = 1;
  DFCS_CAPTIONMAX     = 2;
  DFCS_CAPTIONRESTORE = 3;
  DFCS_CAPTIONHELP    = 4;

  DFCS_MENUARROW      = 0;
  DFCS_MENUCHECK      = 1;
  DFCS_MENUBULLET     = 2;
  DFCS_MENUARROWRIGHT = 4;

  DFCS_SCROLLUP            = 0;
  DFCS_SCROLLDOWN          = 1;
  DFCS_SCROLLLEFT          = 2;
  DFCS_SCROLLRIGHT         = 3;
  DFCS_SCROLLCOMBOBOX      = 5;
  DFCS_SCROLLSIZEGRIP      = 8;
  DFCS_SCROLLSIZEGRIPRIGHT = $10;

  DFCS_BUTTONCHECK      = 0;
  DFCS_BUTTONRADIOIMAGE = 1;
  DFCS_BUTTONRADIOMASK  = 2;
  DFCS_BUTTONRADIO      = 4;
  DFCS_BUTTON3STATE     = 8;
  DFCS_BUTTONPUSH       = $10;

  DFCS_INACTIVE    = $100;
  DFCS_PUSHED      = $200;
  DFCS_CHECKED     = $400;
  DFCS_TRANSPARENT = $800;
  DFCS_HOT         = $1000;
  DFCS_ADJUSTRECT  = $2000;
  DFCS_FLAT        = $4000;
  DFCS_MONO        = $8000;

  { 3D border styles }
  BDR_RAISEDOUTER = 1;
  BDR_SUNKENOUTER = 2;
  BDR_RAISEDINNER = 4;
  BDR_SUNKENINNER = 8;

  BDR_OUTER   = BDR_SUNKENOUTER or BDR_RAISEDOUTER;
  BDR_INNER   = BDR_SUNKENINNER or BDR_SUNKENOUTER;
  BDR_RAISED  = BDR_RAISEDINNER or BDR_RAISEDOUTER;
  BDR_SUNKEN  = BDR_SUNKENINNER or BDR_SUNKENOUTER;

  EDGE_RAISED = BDR_RAISEDOUTER or BDR_RAISEDINNER;
  EDGE_SUNKEN = BDR_SUNKENOUTER or BDR_SUNKENINNER;
  EDGE_ETCHED = BDR_SUNKENOUTER or BDR_RAISEDINNER;
  EDGE_BUMP   = BDR_RAISEDOUTER or BDR_SUNKENINNER;

  { Border flags }
  BF_LEFT     = 1;
  BF_TOP      = 2;
  BF_RIGHT    = 4;
  BF_BOTTOM   = 8;
  BF_DIAGONAL = $10;

  BF_TOPLEFT     = BF_TOP or BF_LEFT;
  BF_TOPRIGHT    = BF_TOP or BF_RIGHT;
  BF_BOTTOMLEFT  = BF_BOTTOM or BF_LEFT;
  BF_BOTTOMRIGHT = BF_BOTTOM or BF_RIGHT;
  BF_RECT        = BF_TOPLEFT or BF_BOTTOMRIGHT;

  { For diagonal lines, the BF_RECT flags specify the end point of the}
  { vector bounded by the rectangle parameter.}
  BF_DIAGONAL_ENDTOPRIGHT    = BF_DIAGONAL or BF_TOP or BF_RIGHT;
  BF_DIAGONAL_ENDTOPLEFT     = BF_DIAGONAL or BF_TOP or BF_LEFT;
  BF_DIAGONAL_ENDBOTTOMLEFT  = BF_DIAGONAL or BF_BOTTOM or BF_LEFT;
  BF_DIAGONAL_ENDBOTTOMRIGHT = BF_DIAGONAL or BF_BOTTOM or BF_RIGHT;

  BF_MIDDLE = $800;   { Fill in the middle }
  BF_SOFT   = $1000;  { For softer buttons }
  BF_ADJUST = $2000;  { Calculate the space left over }
  BF_FLAT   = $4000;  { For flat rather than 3D borders }
  BF_MONO   = $8000;  { For monochrome borders }

  { DrawIconEx diFlags }
  DI_MASK        = 1;
  DI_IMAGE       = 2;
  DI_NORMAL      = 3;
//  DI_COMPAT    = 4;   not supported
  DI_DEFAULTSIZE = 8;

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
  DT_EDITCONTROL   = $2000;  // ignored
  DT_PATH_ELLIPSIS = $4000;
  DT_ELLIPSIS      = $8000;
  DT_END_ELLIPSIS  = DT_ELLIPSIS;
  DT_MODIFYSTRING  = $10000;
  DT_RTLREADING    = $20000; // ignored
  DT_WORD_ELLIPSIS = $40000;
  DT_HIDEPREFIX    = $100000;
  DT_PREFIXONLY    = $200000;

  { ExtTextOut format flags }
  ETO_OPAQUE     = 2;
  ETO_CLIPPED    = 4;
  ETO_RTLREADING = $80; // ignored

  { font metrics }
  DEFAULT_PITCH   = 0;
  FIXED_PITCH     = 1;
  VARIABLE_PITCH  = 2;
  DEFAULT_CHARSET = 1;

  {$IFDEF LINUX}
  HINSTANCE_ERROR = 0;
  HINSTANCE_OK    = HINSTANCE_ERROR + 1;
  {$ENDIF LINUX}

  { BrushStyle mappings}
  HS_BDIAGONAL  = BrushStyle_BDiagPattern;    // 45-degree downward left-to-right hatch
  HS_CROSS      = BrushStyle_CrossPattern;    // Hor. and vertical crosshatch
  HS_DIAGCROSS  = BrushStyle_DiagCrossPattern;// 45-degree crosshatch
  HS_FDIAGONAL  = BrushStyle_FDiagPattern;    // 45-degree upward left-to-right hatch
  HS_HORIZONTAL = BrushStyle_HorPattern;      // Horizontal hatch
  HS_VERTICAL   = BrushStyle_VerPattern;      // Vertical hatch

  HWND_TOP       = Cardinal(0);
  HWND_BOTTOM    = Cardinal(1);
  HWND_TOPMOST   = Cardinal(-1);
  HWND_NOTOPMOST = Cardinal(-2);

{$IFDEF LINUX}
  { GlobalMemory }
  GMEM_FIXED          = 0;
  GMEM_MOVEABLE       = 2;
  GMEM_NOCOMPACT      = $10;
  GMEM_NODISCARD      = $20;
  GMEM_ZEROINIT       = $40; // only supported flag
  GMEM_MODIFY         = $80;
  GMEM_DISCARDABLE    = $100;
  GMEM_NOT_BANKED     = $1000;
  GMEM_SHARE          = $2000;
  GMEM_DDESHARE       = $2000;
  GMEM_NOTIFY         = $4000;
  GMEM_LOWER          = GMEM_NOT_BANKED;
  GMEM_VALID_FLAGS    = 32626;
  GMEM_INVALID_HANDLE = $8000;

  GHND = GMEM_MOVEABLE or GMEM_ZEROINIT;
  GPTR = GMEM_FIXED or GMEM_ZEROINIT;

{$ENDIF LINUX}


  INFINITE = Longword($FFFFFFFF); // Infinite timeout
  INVALID_HANDLE_VALUE = DWORD(-1);
  MaxWord = High(Cardinal);

  { MessageBox() return values }
  IDCLOSE    = 0;
  IDOK       = 1;
  IDCANCEL   = 2;
  IDYES      = 3;
  IDNO       = 4;
  IDABORT    = 5;
  IDRETRY    = 6;
  IDIGNORE   = 7;
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

  {$EXTERNALSYM LB_OKAY}
  LB_OKAY     = 0;
  {$EXTERNALSYM LB_ERR}
  LB_ERR      = -1;
  {$EXTERNALSYM LB_ERRSPACE}
  LB_ERRSPACE = -2;
  {$EXTERNALSYM CB_OKAY}
  CB_OKAY = 0;
  {$EXTERNALSYM CB_ERR}
  CB_ERR = -1;
  {$EXTERNALSYM CB_ERRSPACE}
  CB_ERRSPACE = -2;


  MAX_COMPUTERNAME_LENGTH = 15;

  { MessageBox() WinFlags }
  MB_OK               = $0000;
  MB_OKCANCEL         = $0001;
  MB_ABORTRETRYIGNORE = $0002;
  MB_YESNOCANCEL      = $0003;
  MB_YESNO            = $0004;
  MB_RETRYCANCEL      = $0005;
  MB_HELP             = $4000; { Help Button not supported}
  MB_ICONHAND         = $0010;
  MB_ICONQUESTION     = $0020;
  MB_ICONEXCLAMATION  = $0030;
  MB_ICONASTERISK     = $0040;
  MB_USERICON         = $0080;
  MB_DEFBUTTON1       = $0000;
  MB_DEFBUTTON2       = $0100;
  MB_DEFBUTTON3       = $0200;
  MB_DEFBUTTON4       = $0300;
  MB_ICONWARNING      = MB_ICONEXCLAMATION;
  MB_ICONERROR        = MB_ICONHAND;
  MB_ICONINFORMATION  = MB_ICONASTERISK;
  MB_ICONSTOP         = MB_ICONHAND;

  { MouseKeys }
  MK_LBUTTON   = 1;
  MK_RBUTTON   = 2;
  MK_SHIFT     = 4;
  MK_CONTROL   = 8;
  MK_MBUTTON   = $10;

  { TDrawItemStruct itemstate }
  ODS_DISABLED = 1;
  ODS_SELECTED = 2;
  ODS_FOCUS    = 4;

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

  { BitBlt/StretchBlt: supported  windows dwRop Raster OPerations }
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

 { SetROP2:  windows ROP2 values }
  R2_BLACK       = 9;  // RasterOp_ClearROP:  Pixel is always 0.
  R2_WHITE       = 10; // RasterOp_SetROP:Pixel is always 1.
  R2_NOP         = 11; // RasterOp_NopROP: Pixel remains unchanged.
  R2_NOT         = 8;  // RasterOp_NotROP:      inverse of the screen color.
  R2_COPYPEN     = 0;  // RasterOp_CopyROP: Pixel is the pen color.
  R2_NOTCOPYPEN  = 4;  // RasterOp_NotCopyROP; inverse of the pen color.
  R2_MERGEPENNOT = 13; // RasterOp_OrNotROP: combination of the pen color and the inverse of the screen color.
  R2_MASKPENNOT  = 12; // RasterOp_AndNotROP: combination of the colors common to both the pen and the inverse of the screen.
  R2_MERGEPEN    = 1;  // RasterOp_OrROP: combination of the pen color and the screen color.
  R2_NOTMERGEPEN = 15; // RasterOp_NorROP:  inverse of the R2_MERGEPEN color.
  R2_MASKPEN     = 7;  // RasterOp_AndROP: combination of the colors common to both the pen and the screen.
  R2_NOTMASKPEN  = 14; // RasterOp_NandROP: inverse of the R2_MASKPEN color.
  R2_XORPEN      = 2;  // RasterOp_XorROP: combination of the colors in the pen and in the screen, but not in both.
  R2_NOTXORPEN   = 6;  // RasterOp_NotXorROP: inverse of the R2_XORPEN color.
  R2_MASKNOTPEN  = 3;  // RasterOp_NotAndROP: combination of the colors common to both the screen and the inverse of the pen.
  R2_MERGENOTPEN = 5;  // RasterOp_NotOrROP: combination of the screen color and the inverse of the pen color.

  RT_RCDATA = Types.RT_RCDATA;
  RT_BITMAP = PChar(2);

  { WM_xSCROLL ScrollCodes }
  SB_BOTTOM        = 1;
  SB_ENDSCROLL     = 2;
  SB_LINEDOWN      = 3;
  SB_LINEUP        = 4;
  SB_PAGEDOWN      = 5;
  SB_PAGEUP        = 6;
  SB_THUMBPOSITION = 7;
  SB_THUMBTRACK    = 8;
  SB_TOP           = 9;

  SB_HORZ          = 1;
  SB_VERT          = 2;
  SB_BOTH          = SB_HORZ or SB_VERT;


  { semaphores }
  STATUS_WAIT_0           = $00000000;
  STATUS_ABANDONED_WAIT_0 = $00000080;
  STATUS_TIMEOUT          = $00000102;
  WAIT_FAILED             = Longword($FFFFFFFF);
  WAIT_OBJECT_0           = STATUS_WAIT_0;
  WAIT_ABANDONED          = STATUS_ABANDONED_WAIT_0;
  WAIT_ABANDONED_0        = STATUS_ABANDONED_WAIT_0;
  WAIT_TIMEOUT            = STATUS_TIMEOUT;
  MAXIMUM_WAIT_OBJECTS    = 64;

  { ShowWindow() Commands }
  SW_HIDE            = 0;
  SW_SHOWNORMAL      = 1;
  SW_NORMAL          = 1;
  SW_SHOWMINIMIZED   = 2;
  SW_SHOWMAXIMIZED   = 3;
  SW_MAXIMIZE        = 3;
  SW_SHOWNOACTIVATE  = 4;
  SW_SHOW            = 5;
  SW_MINIMIZE        = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA          = 8;
  SW_RESTORE         = 9;
  SW_SHOWDEFAULT     = 10;
  SW_MAX             = 10;

  { SetWindowPos Flags }
  SWP_NOSIZE         = 1;
  SWP_NOMOVE         = 2;
  SWP_NOZORDER       = 4;
  SWP_NOREDRAW       = 8;
  SWP_NOACTIVATE     = $10;
  SWP_FRAMECHANGED   = $20;  { The frame changed: send WM_NCCALCSIZE }
  SWP_SHOWWINDOW     = $40;
  SWP_HIDEWINDOW     = $80;
  SWP_NOCOPYBITS     = $100; // ignored
  SWP_NOOWNERZORDER  = $200; { Don't do owner Z ordering }
  SWP_NOSENDCHANGING = $400; // ignores
  SWP_DRAWFRAME      = SWP_FRAMECHANGED;
  SWP_NOREPOSITION   = SWP_NOOWNERZORDER;
  SWP_DEFERERASE     = $2000;
  SWP_ASYNCWINDOWPOS = $4000; // ignored

  TA_LEFT       = Integer(AlignmentFlags_AlignLeft);
  TA_RIGHT      = Integer(AlignmentFlags_AlignRight);
  TA_CENTER     = Integer(AlignmentFlags_AlignHCenter);
  TA_TOP        = Integer(AlignmentFlags_AlignTop);
  TA_BOTTOM     = Integer(AlignmentFlags_AlignBottom);
  VTA_CENTER    = Integer(AlignmentFlags_AlignVCenter);
  TA_NOUPDATECP = 0;
  TA_UPDATECP   = $8000;
  TA_BASELINE   = $4000;
  VTA_BASELINE  = TA_BASELINE;

  {$IFDEF LINUX}
  {virtual memory handling}
  PAGE_NOACCESS          = 0;
  PAGE_READONLY          = PROT_READ;
  PAGE_READWRITE         = PROT_READ or PROT_WRITE;
//PAGE_WRITECOPY         = PROT_ ; // not implemented
  PAGE_EXECUTE           = PROT_EXEC;
  PAGE_EXECUTE_READ      = PAGE_EXECUTE or PAGE_READONLY;
  PAGE_EXECUTE_READWRITE = PAGE_EXECUTE or PAGE_READWRITE;
//PAGE_EXECUTE_WRITECOPY = PAGE_EXECUTE or PAGE_WRITECOPY;
  {$ENDIF LINUX}


  { Windows VK_ keycodes to Qt key }
  VK_BACK     = Key_Backspace;
  VK_TAB      = Key_Tab;
  VK_RETURN   = Key_Enter; //Key_Return = Enter key from keypad
  VK_SHIFT    = Key_Shift;
  VK_CONTROL  = Key_Control;
  VK_MENU     = Key_Alt;
  VK_PAUSE    = Key_Pause;
  VK_CAPITAL  = Key_CapsLock;
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
//  DontPrint = $1000; not used
  { Additional constanst for Qt text alignments used by DrawText }
  QtAlignMask   = $FFF;
  CalcRect      = $10000;
  ClipPath      = $20000;
  ClipName      = $40000;
  ClipToWord    = $100000;
  ModifyString  = $200000;

  pf24bit       = pf32bit;
  clMoneyGreen  = TColor($C0DCC0);
  clSkyBlue     = TColor($F0CAA6);
  clCream       = TColor($F0FBFF);
  clMedGray     = TColor($A4A0A0);
  clWindowFrame = cl3DDkShadow;

  crColorTo         = crNoRole;
  clColorTo         = TColor(-15);
  clNormalColorTo   = TColor(clColorTo - cloNormal);
  clActiveColorTo   = TColor(clColorTo - cloActive);
  clDisabledColorTo = TColor(clColorTo - cloDisabled);

  clNoRole         = TColor(-15);
  clNormalNoRole   = TColor(clNoRole - cloNormal);
  clDisabledNoRole = TColor(clNoRole - cloDisabled);
  clActiveNoRole   = TColor(clNoRole - cloActive);
  clDesktop        = clDisabledNoRole;
  clColor0         = clMask;
  clColor1         = clDontMask;

  // Windows symbolic colors to mapping VisualCLX symbolic colors
  Win2TColor: array [0..COLOR_ENDCOLORS] of TColor = (
    clNormalButton, clNormalBackground, clActiveHighlightedText,     // 0
    clDisabledHighlightedText, clNormalMid, clNormalBase,            // 3
    clNormalHighlight, clNormalButtonText, clNormalText,             // 6
    clNormalHighlightedText, clActiveHighlight, clDisabledHighlight, // 9
    clNormalMid, clNormalHighlight,  clNormalHighlightedText,        // 12
    clNormalButton, clNormalDark, clDisabledText,                    // 15
    clNormalButtonText, clDisabledHighlightedText, clActiveLight,    // 18
    clNormalMid, clNormalMidLight, clNormalText,                     // 21
    clInfoBk, clBlack, clActiveHighlight,                            // 24
    clActiveHighLight, clDisabledHighlight                           // 27
  );

  { SendMessage / PostMessage }
  QEventType_Message = QEventType(2105);
  { Timer message id}
  WM_TIMER = $0113;  { 275 }

type
  TAppEventHook = class(TComponent)
  private
    FHook: QApplication_hookH;
  protected
    function EventFilter(Receiver: QObjectH; Event: QEventH): Boolean; cdecl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TMsg = packed record
    hwnd: QWidgetH;
    message: Integer;
    wParam: Integer;
    lParam: Integer;
    case integer of
      0:
      (
        time: Cardinal;
        pt: TPoint;
      );
      1: ( Result: Integer);
      2: ( Handled: LongBool);
  end;
  PMsg = ^TMsg;

  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0:
      (
        WParam: Longint;
        LParam: Longint;
        Result: Longint;
      );
      1:
      (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word
      );
  end;
  PMessage = ^TMessage;

  { Provided to simplify VCL source sharing }
  HWND        = QWidgetH;
  HCURSOR     = QCursorH;
  HRGN        = QRegionH;
  HBRUSH      = QBrushH;
  HBITMAP     = QPixmapH;
  HDC         = QPainterH;
  HFONT       = QFontH;
  UINT        = Cardinal;
  ULONG       = Cardinal;
  DWORD       = Cardinal;
  BOOL        = LongBool;
  WPARAM      = Integer;
  LPARAM      = Integer;
  LRESULT     = Integer;
  TPointL     = TPoint;
  COLORREF    = Integer;
  TColorRef   = COLORREF;

  TWinControlActionLink = TWidgetControlActionLink;
  TControlClass = class of TControl;

  {$EXTERNALSYM TCaption}
  TCaption    = QTypes.TCaption;

  {$EXTERNALSYM TOwnerDrawState}
  TOwnerDrawState = QStdCtrls.TOwnerDrawState;

  {$EXTERNALSYM PPoint}
  {$EXTERNALSYM TPoint}
  PPoint      = Types.PPoint;
  TPoint      = Types.TPoint;

  {$EXTERNALSYM PRect}
  {$EXTERNALSYM TRect}
  PRect       = Types.PRect;
  TRect       = Types.TRect;

  {$EXTERNALSYM PSize}
  {$EXTERNALSYM TSize}
  TSize       = Types.TSize;
  PSize       = Types.PSize;

  PSmallPoint = Types.PSmallPoint;
  TSmallPoint = Types.TSmallPoint;
  {$EXTERNALSYM PSmallPoint}
  {$EXTERNALSYM TSmallPoint}

  TTime       = type TDateTime;
  TDate       = type TDateTime;
  {$EXTERNALSYM TDate}
  {$EXTERNALSYM TTime}

  { colors }
  TRGBQuad = packed record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
  TRGBTriple  = TRGBQuad; // Qt does not support 24 bit pixmaps

  { fonts }
  tagTEXTMETRICA = record
    tmHeight: Longint;
    tmAscent: Longint;
    tmDescent: Longint;
    //tmInternalLeading: Longint; // not supported
    tmExternalLeading: Longint;
    tmAveCharWidth: Longint;
    tmMaxCharWidth: Longint;
    tmWeight: Longint;
    //tmOverhang: Longint; // not supported
    tmDigitizedAspectX: Longint;
    tmDigitizedAspectY: Longint;
    tmFirstChar: AnsiChar;
    tmLastChar: AnsiChar;
    tmDefaultChar: AnsiChar;
    tmBreakChar: AnsiChar;
    tmItalic: Byte;
    tmUnderlined: Byte;
    tmStruckOut: Byte;
    tmPitchAndFamily: Byte;
    tmCharSet: Byte;
  end;
  TTextMetric = tagTEXTMETRICA;
  TEXTMETRIC  = TTextMetric;

  { Logical Pen }
  PLogPen = ^TLogPen;
  tagLOGPEN = packed record
    lopnStyle: UINT;
    lopnWidth: TPoint;
    lopnColor: COLORREF;
  end;
  TLogPen     = tagLOGPEN;
  LOGPEN      = tagLOGPEN;

  { Logical Palette }
  PPaletteEntry = ^TPaletteEntry;
  tagPALETTEENTRY = packed record
    peRed: Byte;
    peGreen: Byte;
    peBlue: Byte;
    peFlags: Byte;
  end;
  TPaletteEntry = tagPALETTEENTRY;
  PALETTEENTRY = tagPALETTEENTRY;

  PLogPalette = ^TLogPalette;
  tagLOGPALETTE = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..0] of TPaletteEntry;
  end;
  TLogPalette = tagLOGPALETTE;
  LOGPALETTE  = tagLOGPALETTE;

  PMaxLogPalette = ^TMaxLogPalette;
  TMaxLogPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[Byte] of TPaletteEntry;
  end;

  PtagBITMAP  = ^tagBITMAP;
  tagBITMAP = packed record
    //bmType: Longint;
    bmWidth: Longint;
    bmHeight: Longint;
    //bmWidthBytes: Longint;
    //bmPlanes: Word;
    bmBitsPixel: Word;
    //bmBits: Pointer;
  end;

  PBitmapInfoHeader = ^TBitmapInfoHeader;
  tagBITMAPINFOHEADER = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;
  TBitmapInfoHeader = tagBITMAPINFOHEADER;
  BITMAPINFOHEADER  = tagBITMAPINFOHEADER;

  PBitmapInfo = ^TBitmapInfo;
  tagBITMAPINFO = packed record
    bmiHeader: TBitmapInfoHeader;
    bmiColors: array[0..0] of TRGBQuad;
  end;
  TBitmapInfo = tagBITMAPINFO;
  BITMAPINFO = tagBITMAPINFO;

  PBitmapFileHeader = ^TBitmapFileHeader;
  tagBITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: DWORD;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: DWORD;
  end;
  TBitmapFileHeader = tagBITMAPFILEHEADER;
  BITMAPFILEHEADER  = tagBITMAPFILEHEADER;

  tagDRAWITEMSTRUCT = packed record
    CtlType: Cardinal;
    CtlID: Cardinal;
    itemID: Cardinal;
    itemAction: Cardinal;
    itemState: Cardinal;
    hwndItem: QWidgetH;
    hDC: QPainterH;
    rcItem: TRect;
    itemData: Cardinal;
  end;
  TDrawItemStruct   = tagDRAWITEMSTRUCT;
  DRAWITEMSTRUCT    = tagDRAWITEMSTRUCT;

type
  TDeviceCap = (
    HORZSIZE,        {Horizontal size in millimeters}
    VERTSIZE,        {Vertical size in millimeters}
    HORZRES,         {Horizontal width in pixels}
    VERTRES,         {Vertical height in pixels}
    BITSPIXEL,       {Number of bits per pixel}
    PLANES,          {Number of planes}
    NUMCOLORS,
    LOGPIXELSX,      {Logical pixelsinch in X}
    LOGPIXELSY,      {Logical pixelsinch in Y}
    PHYSICALWIDTH,   {Physical Width in device units}
    PHYSICALHEIGHT,  {Physical Height in device units}
    PHYSICALOFFSETX, {Physical Printable Area X margin}
    PHYSICALOFFSETY  {Physical Printable Area Y margin}
  );

  { Mapping Modes }
  TMapMode = (
    MM_TEXT        = 1,
    MM_LOMETRIC    = 2,
    MM_HIMETRIC    = 3,
    MM_LOENGLISH   = 4,
    MM_HIENGLISH   = 5,
    MM_TWIPS       = 6,
    MM_ISOTROPIC   = 7,
    MM_ANISOTROPIC = 8
  );

const
  { Min and Max Mapping Mode values }
  MM_MIN = MM_TEXT;
  MM_MAX = MM_ANISOTROPIC;
  MM_MAX_FIXEDSCALE = MM_TWIPS;

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

{ regions }
type
  TCombineMode = (
    RGN_AND,   // Creates the intersection of the two combined regions.
    RGN_COPY,  // Creates a copy of the region identified by hrgnSrc1.
    RGN_DIFF,  // Combines the parts of hrgnSrc1 that are not part of hrgnSrc2.
    RGN_OR,    // Creates the union of two combined regions.
    RGN_XOR    // Creates the union of two combined regions except for any overlapping areas.
  );

  PSecurityAttributes = Pointer;

{ StretchBlt() Modes }
  TStretchMode = (
    BLACKONWHITE = 1,
    WHITEONBLACK = 2,
    COLORONCOLOR = 3,
    HALFTONE     = 4
  );

const
  MAXSTRETCHBLTMODE   = 4;
  STRETCH_ANDSCANS    = BLACKONWHITE;
  STRETCH_ORSCANS     = WHITEONBLACK;
  STRETCH_DELETESCANS = COLORONCOLOR;
  STRETCH_HALFTONE    = HALFTONE;

{ Stock Logical Objects }
type
  TStockObjectBrush = (
    WHITE_BRUSH  = 0,
    LTGRAY_BRUSH = 1,
    GRAY_BRUSH   = 2,
    DKGRAY_BRUSH = 3,
    BLACK_BRUSH  = 4,
    NULL_BRUSH   = 5,
    DC_BRUSH     = 18
  );

  TStockObjectPen = (
    WHITE_PEN = 6,
    BLACK_PEN = 7,
    NULL_PEN  = 8,
    DC_PEN    = 19
  );

  TStockObjectFont = (
    OEM_FIXED_FONT      = 10,
    ANSI_FIXED_FONT     = 11,
    ANSI_VAR_FONT       = 12,
    SYSTEM_FONT         = 13,
    DEVICE_DEFAULT_FONT = 14,
    DEFAULT_PALETTE     = 15,
    SYSTEM_FIXED_FONT   = $10,
    DEFAULT_GUI_FONT    = 17
  );

const
  HOLLOW_BRUSH = NULL_BRUSH;
  STOCK_LAST   = DC_PEN;

type
  TSysMetrics = (
    SM_CXSCREEN,  SM_CYSCREEN,
    SM_CXVSCROLL, SM_CYVSCROLL,
    SM_CXHSCROLL, SM_CYHSCROLL,
    SM_CXSMICON,  SM_CYSMICON,
    SM_CXICON,    SM_CYICON,
    SM_CXBORDER,  SM_CYBORDER,
    SM_CXFRAME,   SM_CYFRAME,
    SM_CYCAPTION, SM_CXDLGFRAME,
    SM_CYDLGFRAME
  );

{$IFDEF LINUX}
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
{$ENDIF LINUX}

  { threads }
type
  TThreadPriority = Integer;
const
  tpIdle: TThreadPriority = 0;
  THREAD_PRIORITY_ERROR_RETURN = 255;

type { wait for object}
  TWOHandleArray = array[0..MAXIMUM_WAIT_OBJECTS - 1] of THandle;
  PWOHandleArray = ^TWOHandleArray;

  TWindowPlacement = packed record
    length: Cardinal;
    flags: Integer;
    showCmd: UInt;
    ptMinPosition: TPoint;
    ptMaxPosition: TPoint;
    rcNormalPosition: TRect;
  end;
  PWindowPlacement = ^TWindowPlacement;

type
  TTimerProc = procedure(Widget: QWidgetH; Msg: Cardinal; WMTimerId: Cardinal;  TickCount: Cardinal);
  TAppEventFilterMethod = function (Sender: QObjectH; Event: QEventH): Boolean; cdecl;
  PAppEventFilterMethod = ^TAppEventFilterMethod;

function InstallApplicationEventHook(EventFilter: TEventFilterMethod): QApplication_hookH;

procedure WakeUpGuiThread;
{
  Dummies for ... VCL
 asn: AFAIK use of RightToLeft or LeftToRight is automatic
}
const
  BiDiMode: TBiDiMode = bdLeftToRight; // asn: var?

function DrawTextBiDiModeFlagsReadingOnly: Longint;
function DrawTextBiDiModeFlags(Flags: Longint): Longint;
procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
function UseRightToLeftAlignment: Boolean;

{ Palette colors }
function GetSysColor(SysColor: Integer): TColorRef;  // windows SysColor !!
function SetSysColor(RefColor: TColor; TrueColor: TColorRef): Boolean;
function SetSysColors(Elements: Integer; const lpaElements;
  const lpaRgbValues): LongBool;

{
 QGraphics.ColorToRGB supports only
 TMappedColor = clActiveHighlightedText..clNormalForeground
 Returns clBlack for any other color !

  This implementation uses Instance.Palette and supports all colors
  if Instance = nil then it will use Application.MainForm, in that
  case clHighlightedText..clForeground is mapped to clNormalHighlightedText..clNormalForeground
  If the Color is an RGB value, clDefault, clNone,  it returns the Color itself.
}
function ColorToRGB(Color: TColor; Instance: TWidgetControl = nil): TColor;

function RGB(Red, Green, Blue: Integer): TColorRef;
function GetBValue(Col: TColorRef): Byte;
function GetGValue(Col: TColorRef): Byte;
function GetRValue(Col: TColorRef): Byte;
function pfDevice: TPixelFormat;

function SetRect(var R: TRect; Left, Top, Right, Bottom: Integer): LongBool;
function IsRectEmpty(R: TRect): LongBool;
function EqualRect(R1, R2: TRect): LongBool;
function UnionRect(var Dst: TRect; R1, R2: TRect): LongBool;
function CopyRect(var Dst: TRect; const Src: TRect): LongBool; overload;
function SubtractRect(var dR: TRect; const R1, R2: TRect): LongBool;
function CenterRect(InnerRect, OuterRect: TRect): TRect;
function PtInRect(const R: TRect; pt: TPoint): LongBool; overload;
function PtInRect(const R: TRect; X, Y: integer): LongBool; overload;
function IntersectRect(var R: TRect; const R1, R2: TRect): LongBool;

{ brushes }
function CreateSolidBrush(Color: TColor): QBrushH;
function CreateHatchBrush(bStyle: BrushStyle; Color: TColor): QBrushH;
function DeleteObject(Handle: QBrushH): LongBool; overload;

function CreatePen(Style, Width: Integer; Color: TColor): QPenH;
function DeleteObject(Handle: QPenH): LongBool; overload;

function DPtoLP(Handle: QPainterH; var Points; Count: Integer): LongBool;
function LPtoDP(Handle: QPainterH; var Points; Count: Integer): LongBool;
function SetWindowOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
function GetWindowOrgEx(Handle: QPainterH; Org: PPoint): LongBool; overload;
function GetWindowOrgEx(Handle: QPainterH; var Org: TPoint): LongBool; overload;
{ limited implementations of }
function BitBlt(DestDC: QPainterH; X, Y, Width, Height: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; Rop: RasterOp; IgnoreMask: Boolean = true): LongBool; overload;
function BitBlt(DestDC: QPainterH; X, Y, Width, Height: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean = true): LongBool; overload;
{
 does the required start/stop painting if needed
 adjust x,y & XSrc,YSrc ico TControlCanvas (as used by TGraphicControl)
}
function BitBlt(DestCanvas: TCanvas; X, Y, Width, Height: Integer; SrcCanvas: TCanvas;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean = true): LongBool; overload;

{
// Calculates coord of TopLeft in Paintdevice coordinates
// ((0,0) for bitmaps and TWidgetControl derived classes)
}
function PainterOffset(Canvas: TCanvas): TPoint;

function PatBlt(Handle: QPainterH; X, Y, Width, Height: Integer;
  WinRop: Cardinal): LongBool; overload;
function PatBlt(Canvas: TCanvas; X, Y, Width, Height: Integer;
  WinRop: Cardinal): LongBool; overload;

procedure CopyRect(DstCanvas: TCanvas; const Dest: TRect; Canvas: TCanvas;
  const Source: TRect); overload;
procedure BrushCopy(DstCanvas: TCanvas; const Dest: TRect; Bitmap: TBitmap;
  const Source: TRect; Color: TColor);

function StretchBlt(DestDC: QPainterH; dx, dy, dw, dh: Integer;
  SrcDC: QPainterH; sx, sy, sw, sh: Integer; WinRop: Cardinal;
  IgnoreMask: Boolean = True): LongBool; overload;
function StretchBlt(DestDC: QPainterH; dx, dy, dw, dh: Integer;
  SrcDC: QPainterH; sx, sy, sw, sh: Integer; Rop: RasterOp;
  IgnoreMask: Boolean = True): LongBool; overload;
function StretchBlt(DestCanvas: TCanvas; dx, dy, dw, dh: Integer;
  SrcCanvas: TCanvas; sx, sy, sw, sh: Integer; WinRop: Cardinal;
  IgnoreMask: Boolean = True): LongBool; overload;
function ScrollDC(Handle: QPainterH; dx, dy: Integer; var Scroll, Clip: TRect;
  Rgn: QRegionH; Update: PRect): LongBool;

{ TODO -oahuser : StretchBlt function should use the flag }
function GetStretchBltMode(DC: QPainterH): TStretchMode;
function SetStretchBltMode(DC: QPainterH; StretchMode: TStretchMode): TStretchMode;

function SetROP2(Handle: QPainterH; Rop: Integer): Integer; overload;
function GetROP2(Handle: QPainterH): Integer; overload;
function GetPixel(Handle: QPainterH; X, Y: Integer): TColorRef;
function SetPixel(Handle: QPainterH; X, Y: Integer; Color: TColor): TColorRef;
function SetTextColor(Handle: QPainterH; color: TColor): TColorRef;
function SetBkColor(Handle: QPainterH; color: TColor): TColorRef;
function GetBkMode(Handle: QPainterH): Integer;
function SetBkMode(Handle: QPainterH; BkMode: Integer): Integer;
function SetDCBrushColor(Handle: QPainterH; Color: TColor): TColorRef;
function SetDCPenColor(Handle: QPainterH; Color: TColor): TColorRef;
function SetPenColor(Handle: QPainterH; Color: TColor): TColorRef;
procedure SetPainterFont(Handle: QPainterH; Font: TFont);

function CreateCompatibleDC(Handle: QPainterH; Width: Integer = 1; Height: Integer = 1): QPainterH;
function CreateCompatibleBitmap(Handle: QPainterH; Width, Height: Integer): QPixmapH;
function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; Bits: Pointer): QPixmapH;
function CreateDIBitmap(Handle: QPainterH; var InfoHeader: TBitmapInfoHeader;
  dwUsage: Longword; InitBits: PChar; var InitInfo: TBitmapInfo; wUsage: Cardinal): QPixmapH;

function GetBitmapBits(Bitmap: QPixmapH; Count: Longint; Bits: Pointer): Longint;
function SetBitmapBits(Bitmap: QPixmapH; Count: Longint; Bits: Pointer): Longint;

function GetObject(Handle: QPixmapH; Size: Cardinal; Data: PtagBITMAP): Boolean; overload;
function GetObject(Handle: QPenH; Size: Cardinal; Data: PLogPen): Boolean; overload;

function GetStockObject(fnObject: TStockObjectBrush): QBrushH; overload;
function GetStockObject(fnObject: TStockObjectPen): QPenH; overload;
function GetStockObject(fnObject: TStockObjectFont): QFontH; overload;

function GetMapMode(Handle: QPainterH): TMapMode;
function SetMapMode(Handle: QPainterH; MapMode: TMapMode): TMapMode;

// DeleteObject is intended to destroy the Handle returned by CreateCompatibleBitmap
// (it destroys the Painter AND PaintDevice)
function DeleteObject(Handle: QPainterH): LongBool; overload;
function DeleteObject(Handle: QPixmapH): LongBool; overload;
function GetDC(Handle: QWidgetH): QPainterH; overload;
function GetDC(Handle: Integer): QPainterH; overload;
function GetWindowDC(Handle: QWidgetH): QPainterH;

function ReleaseDC(wdgtH: QWidgetH; Handle: QPainterH): Integer; overload;
function ReleaseDC(wdgtH: Integer; Handle: QPainterH): Integer; overload;
function DeleteDC(Handle: QPainterH): LongBool;

function SaveDC(Handle: QPainterH): Integer;
{ only negative and zero values of nSaveDC are supported }
function RestoreDC(Handle: QPainterH; nSavedDC: Integer): LongBool;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; const Text: WideString; Len: Integer; lpDx: Pointer): LongBool; overload;
function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PChar; Len: Integer; lpDx: Pointer): LongBool; overload;
function ExtTextOutW(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PWideChar; Len: Integer; lpDx: Pointer): LongBool;

{ TODO -oahuser : text functions (ExtTextOut, TextOut) should use these flags }
function SetTextAlign(Handle: QPainterH; Mode: Cardinal): Cardinal;
function GetTextAlign(Handle: QPainterH): Cardinal;

function FillRect(Handle: QPainterH; const R: TRect; Brush: QBrushH): LongBool;

function GetCurrentPositionEx(Handle: QPainterH; pos: PPoint): LongBool;
function GetTextExtentPoint32(Handle: QPainterH; const Text: WideString; Len: Integer;
  var Size: TSize): LongBool; overload;
function GetTextExtentPoint32(Handle: QPainterH; pText: PChar; Len: Integer;
  var Size: TSize): LongBool; overload;
function GetTextExtentPoint32W(Handle: QPainterH; pText: PWideChar; Len: Integer;
  var Size: TSize): LongBool;
function GetTextExtentPoint32(Canvas: TCanvas; const Text: WideString; Len: Integer;
  var Size: TSize): LongBool; overload;

function FrameRect(Handle: QPainterH; const R: TRect; Brush: QBrushH): LongBool; overload;
procedure FrameRect(Canvas: TCanvas; const R: TRect); overload;
function FrameRgn(Handle: QPainterH; Region: QRegionH; Brush: QBrushH; Width, Height: integer): LongBool;

function DrawFocusRect(Handle: QPainterH; const R: TRect): LongBool;
function InvertRect(Handle: QPainterH; const R: TRect): LongBool;
function Rectangle(Handle: QPainterH; Left, Top, Right, Bottom: Integer): LongBool;
function RoundRect(Handle: QPainterH; Left, Top, Right, Bottom, X3, Y3: Integer): LongBool;
function Ellipse(Handle: QPainterH; Left, Top, Right, Bottom: Integer): LongBool;
function LineTo(Handle: QPainterH; X, Y: Integer): LongBool;
function MoveToEx(Handle: QPainterH; X, Y: Integer; Point: PPoint): LongBool;
function DrawIcon(Handle: QPainterH; X, Y: Integer; hIcon: QPixmapH): LongBool;
function DrawIconEx(Handle: QPainterH; X, Y: Integer; hIcon: QPixmapH;
  W, H: Integer; istepIfAniCur: Integer; hbrFlickerFreeDraw: QBrushH;
  diFlags: Cardinal): LongBool;

function DrawFrameControl(Handle: QPainterH; const Rect: TRect; uType,
  uState: Longword): LongBool; overload;
  { missing DrawFrameControl flags:
      DFC_SCROLL: DFCS_SCROLLSIZEGRIP, DFCS_SCROLLSIZEGRIPRIGHT
      DFC_BUTTON: DFCS_BUTTONRADIOIMAGE, DFCS_BUTTONRADIOMASK, DFCS_BUTTON3STATE
      DFC_POPUPMENU: all }
function DrawFrameControl(Canvas: TCanvas; const Rect: TRect; uType,
  uState: Longword): LongBool; overload;
function DrawEdge(Handle: QPainterH; var Rect: TRect; Edge: Cardinal;
  Flags: Cardinal): LongBool;

procedure RequiredState(ACanvas: TCanvas; State: TCanvasState);

{ limited implementation of }
function DrawText2(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer): Integer; overload;
function DrawText(Handle: QPainterH; Text: PAnsiChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
function DrawText(Handle: QPainterH; Text: TCaption; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
function DrawTextW(Handle: QPainterH; Text: PWideChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
function DrawText(Handle: QPainterH; var Text: WideString; Len: Integer;
  x,y, w, h: Integer; WinFlags: Integer; Angle: Integer = 0): Integer;  overload;
function DrawText(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
{
 additional functionality DrawText(Canvas, .....
 - canvas start/stop
 - sets painterfont
}
function DrawText(Canvas :TCanvas; Text: TCaption; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: integer = 0): Integer; overload;
function DrawText(Canvas: TCanvas; Text: PAnsiChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
function DrawTextW(Canvas :TCanvas; Text: PWideChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
function DrawTextEx(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer; DTParams: Pointer): Integer; overload;
function DrawTextEx(Handle: QPainterH; Text: PChar; Len: Integer;
  var R: TRect; WinFlags: Integer; DTParams: Pointer): Integer; overload;

{ limited implementation of }
function GetSystemMetrics(PropItem: TSysMetrics): Integer;

{ (very) limited implementations of }
function GetDeviceCaps(Handle: QPainterH; devcap: TDeviceCap): Integer; overload;
function GetDeviceCaps(Handle: QPaintDeviceH; devcap: TDeviceCap): Integer; overload;

function GetTextMetrics(Handle: QPainterH; var tt: TTextMetric): Integer;
// widget related  function
function BringWindowToTop(Handle: QWidgetH): LongBool;
function CloseWindow(Handle: QWidgetH): LongBool;
function DestroyWindow(Handle: QWidgetH): LongBool;
function EnableWindow(Handle: QWidgetH; Value: Boolean): LongBool;
function GetClientRect(Handle: QWidgetH; var R: TRect): LongBool;
function GetFocus: QWidgetH;
function GetParent(Handle: QWidgetH): QWidgetH;
function SetParent(hWndChild, hWndNewParent: QWidgetH): QWidgetH;
function GetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
function GetWindowRect(Handle: QWidgetH; var  R: TRect): LongBool;
function WindowFromDC(Handle: QPainterH): QWidgetH; overload;
function WindowFromDC(Handle: QPaintDeviceH): QWidgetH; overload;

{ hWndParent is ignored under Linux }
function ChildWindowFromPoint(hWndParent: QWidgetH; Point: TPoint): QWidgetH;

function WindowFromPoint(Point: TPoint): QWidgetH;
function FindCLXWindow(const Point: TPoint): TWidgetControl;
function FindVCLWindow(const Point: TPoint): TWidgetControl;
function GetClassName(Handle: QWidgetH; Buffer: PChar; MaxCount: Integer): Integer;
function IsIconic(Handle: QWidgetH): LongBool;

function HWND_DESKTOP: QWidgetH;
function GetDesktopWindow: QWidgetH;
function GetActiveWindow: QWidgetH;
function GetForegroundWindow: QWidgetH;
procedure SetActiveWindow(Handle: QWidgetH);
function InvalidateRect(Handle: QWidgetH; R: PRect; EraseBackground: Boolean): LongBool;
function ValidateRect(hWnd: QWidgetH; R: PRect): LongBool;
function UpdateWindow(Handle: QWidgetH): LongBool;
function IsChild(ParentHandle, ChildHandle: QWidgetH): LongBool;
function IsWindowEnabled(Handle: QWidgetH): LongBool;
function IsWindowVisible(Handle: QWidgetH): LongBool;
function MapWindowPoints(WidgetTo, WidgetFrom: QWidgetH; var Points; nr: Cardinal): Integer;
function SetFocus(Handle: QWidgetH): QWidgetH;
function SetForegroundWindow(Handle: QWidgetH): LongBool;
function SetWindowPlacement(Handle: QWidgetH; W: PWindowPlacement): LongBool;
function SwitchToThisWindow(Handle: QWidgetH; Restore: Boolean): LongBool;

function ClientToScreen(Handle: QWidgetH; var Point: TPoint): LongBool;
function ScreenToClient(Handle: QWidgetH; var Point: TPoint): LongBool;
function SmallPointToPoint(const P: TSmallPoint): TPoint;
function PointToSmallPoint(const P: TPoint): TSmallPoint;

function SetWindowPos(Wnd, WndInsertAfter: QWidgetH; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool; overload;
function SetWindowPos(Wnd, WndInsertAfter: Cardinal; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool; overload;
function SetWindowPos(Wnd: QWidgetH; WndInsertAfter: Cardinal; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool; overload;

{ Controls.pas implements, so we need it, too }
procedure MoveWindowOrg(DC: QPainterH; DX, DY: Integer);

function ShowWindow(Handle: QWidgetH; showCmd: UInt): LongBool;

function MessageBox(parent: QWidgetH; Text, Caption: string; WinFlags: Cardinal): Integer; overload;
function MessageBox(parent: QWidgetH; Text, Caption: WideString; WinFlags: Cardinal): Integer; overload;
function MessageBox(parent: QWidgetH; pText, pCaption: PChar; WinFlags: Cardinal): Integer; overload;
//function MessageBoxW(parent: QWidgetH; pText, pCaption: PWideChar; WinFlags: Cardinal): Integer;

function SelectObject(Handle: QPainterH; Font: QFontH): QFontH; overload;
function SelectObject(Handle: QPainterH; Brush: QBrushH): QBrushH; overload;
function SelectObject(Handle: QPainterH; Pen: QPenH): QPenH; overload;
// limited to CreateCompatibleDC Handles.
function SelectObject(Handle: QPainterH; Bitmap: QPixmapH): QPixmapH; overload;

function CombineRgn(DestRgn, Source1, Source2: QRegionH; Operation: TCombineMode): Integer;
function CreateEllipticRgn(Left, Top, Right, Bottom: Integer): QRegionH;
function CreateEllipticRgnIndirect(Rect: TRect): QRegionH;
//function CreatePolygonRgn(p1: TPointArray; FillMode: Integer): QRegionH;
function CreatePolygonRgn(const Points; Count, FillMode: Integer): QRegionH;
function CreateRectRgn(Left, Top, Right, Bottom: Integer): QRegionH;
function CreateRectRgnIndirect(Rect: TRect): QRegionH;
function CreateRoundRectRgn(x1, y1, x2, y2, WidthEllipse, HeightEllipse: Integer): QRegionH;
function DeleteObject(Region: QRegionH): LongBool; overload;
function EqualRgn(Rgn1, Rgn2: QRegionH): LongBool;
function FillRgn(Handle: QPainterH; Region: QRegionH; Brush: QBrushH): LongBool;
function GetClipRgn(Handle: QPainterH; rgn: QRegionH): Integer;
function ExcludeClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer; overload;
function ExcludeClipRect(Handle: QPainterH; const R: TRect): Integer; overload;
function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer; overload;
function IntersectClipRect(Handle: QPainterH; const R: TRect): Integer; overload;
function InvertRgn(Handle: QPainterH; Region: QRegionH): LongBool;
function OffsetClipRgn(Handle: QPainterH; X, Y: Integer): Integer;
function OffsetRgn(Region: QRegionH; X, Y: Integer): Integer;
function PtInRegion(Rgn: QRegionH; X, Y: Integer): Boolean;
function RectInRegion(RGN: QRegionH; const Rect: TRect): LongBool;
function SelectClipRgn(Handle: QPainterH; Region: QRegionH): Integer; overload;
function SelectClipRgn(Handle: QPainterH; Region: Integer): Integer; overload;
function SetRectRgn(Rgn: QRegionH; X1, Y1, X2, Y2: Integer): LongBool;
function SetWindowRgn(Handle: QWidgetH; Region: QRegionH; Redraw: LongBool): Integer;
  { SetWindowRgn limitation: The region must have negative top coordinate in
    order to contain the window's caption bar. }
  { asn: Qt operates on the client rectangle of the form: windows/x11 titlebar
         and windows/x11 borders are not included, hence the negative values }
function GetWindowRgn(Handle: QWidgetH; Region: QRegionH): Integer;

{ viewports }
function SetViewportExtEx(Handle: QPainterH; XExt, YExt: Integer; Size: PSize): LongBool;
function SetViewPortOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
function GetViewportExtEx(Handle: QPainterH; Size: PSize): LongBool;

{ Text clipping }
function TruncatePath(const FilePath: string; Canvas: TCanvas; MaxLen: Integer): string;
function TruncateName(const Name: WideString; Canvas: TCanvas; MaxLen: Integer; QtFlags: integer = 0): WideString;

procedure TextOutAngle(Handle: QPainterH; Angle, Left, Top: Integer; Text: WideString); overload;
procedure TextOutAngle(ACanvas: TCanvas; Angle, Left, Top: Integer; Text: WideString); overload;

procedure CopyMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
procedure FillMemory(Dest: Pointer; Len: Cardinal; Fill: Byte);
procedure MoveMemory(Dest: Pointer; Src: Pointer; Len: Cardinal);
procedure ZeroMemory(Dest: Pointer; Len: Cardinal);

{ ------------ Caret -------------- }
function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean; overload;
function CreateCaret(Widget: QWidgetH; ColorCaret: Cardinal; Width, Height: Integer): Boolean; overload;
function GetCaretBlinkTime: Cardinal;
function SetCaretBlinkTime(uMSeconds: Cardinal): LongBool;
function HideCaret(Widget: QWidgetH): Boolean;
function ShowCaret(Widget: QWidgetH): Boolean;
function SetCaretPos(X, Y: Integer): Boolean;
function GetCaretPos(var Pt: TPoint): Boolean;
function DestroyCaret: Boolean;

procedure SetCursorPos(X, Y: integer);

function GetDoubleClickTime: Cardinal;
function SetDoubleClickTime(Interval: Cardinal): LongBool;
function ReleaseCapture: LongBool;
function SetCapture(Widget: QWidgetH): QWidgetH;
function GetCapture: QWidgetH;
function SetCursor(Handle: QCursorH; Save: Boolean = False): QCursorH;
function Win2QtAlign(Flags: Integer): Integer;
function QtStdAlign(Flags: Integer): Word;

function IsCharAlpha(Ch: Char): LongBool;
function IsCharAlphaNumeric(Ch: Char): LongBool;

{ Messaging }
function Perform(Control: TControl; Msg: Cardinal; WPar, LPar: Longint): Longint;
function PostMessage(Receiver: QWidgetH; MsgId: Integer; WPar, LPar: Longint): LongBool; overload;
function PostMessage(AControl: TWidgetControl; MsgId: Integer; WPar, LPar: Longint): LongBool; overload;
{ SendMessage synchronizes with the main (event handling) thread. }
function SendMessage(Receiver: QWidgetH; MsgId: Integer; WPar, LPar: Longint): Integer; overload;
function SendMessage(AControl: TWidgetControl; MsgId: Integer; WPar, LPar: Longint): Integer; overload;

// procedure IgnoreNextEvents(Handle: QObjectH; const Events: array of QEventType);
{ equivalent to "while PeekMessage(h, evstart, evend, PM_REMOVE" }
//procedure IgnoreMouseEvents(Handle: QObjectH);
function IgnoreMouseEvents(Handle: QObjectH; Event: QEventH): boolean;

function SetTimer(Wnd: QWidgetH; WMTimerID, Elapse: Cardinal;
  TimerFunc: TTimerProc): Cardinal; overload;
function SetTimer(Instance: TWidgetControl; WMTimerID, Elapse: Cardinal;
  TimerFunc: TTimerProc): Cardinal; overload;
function KillTimer(Wnd: QWidgetH; WMTimerId: Cardinal): LongBool; overload;
function KillTimer(Instance: TWidgetControl; WMTimerId: Cardinal): LongBool; overload;

function MAKEIPRANGE(low, high: Byte): integer;
function MAKEIPADDRESS(b1, b2, b3, b4: cardinal): integer;
function FIRST_IPADDRESS(x: cardinal): cardinal;
function SECOND_IPADDRESS(x: cardinal): cardinal;
function THIRD_IPADDRESS(x: cardinal): cardinal;
function FOURTH_IPADDRESS(x: cardinal): cardinal;

{ wrappers for Windows, implementations for Linux}
function ShellExecute(Handle: QWidgetH; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): THandle; overload;
function ShellExecute(Handle: QWidgetH; const Operation, FileName, Parameters,
  Directory: string; ShowCmd: Integer): THandle; overload;
function ShellExecute(Handle: Integer; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): THandle; overload;

{ Platform dependendant wrappers}
function GetTickCount: Cardinal;
function GetUserName(Buffer: PChar; var Size: Cardinal): LongBool;
function GetComputerName(Buffer: PChar; var Size: Cardinal): LongBool;
procedure OutputDebugString(lpOutputString: PAnsiChar); overload;
procedure OutputDebugString(OutputString: AnsiString); overload;
function InterlockedIncrement(var I: Integer): Integer;
function InterlockedDecrement(var I: Integer): Integer;
function InterlockedExchange(var A: Integer; B: Integer): Integer;
function InterlockedExchangeAdd(var A: Integer; B: Integer): Integer;
function QueryPerformanceCounter(var PerformanceCount: int64): LongBool;
function QueryPerformanceFrequency(var Frequency: int64): LongBool;

{$IFDEF MSWINDOWS}
function GetKeyState(nVirtKey: Integer): SmallInt;
//
// Taken from QDialogs
//
procedure EnableTaskWindows(WindowList: Pointer);
function DisableTaskWindows(ActiveWindow: Windows.HWnd): Pointer;
{$ENDIF MSWINDOWS}

function CopyFile(lpExistingFileName, lpNewFileName: PChar;
  bFailIfExists: LongBool): LongBool; overload;

function CopyFileA(lpExistingFileName, lpNewFileName: PAnsiChar;
  bFailIfExists: LongBool): LongBool;
function CopyFileW(lpExistingFileName, lpNewFileName: PWideChar;
  bFailIfExists: LongBool): LongBool;
{$IFDEF LINUX}
function CopyFile(const Source, Destination: string;
  FailIfExists: Boolean): LongBool; overload;

function FileGetSize(const FileName: string): Cardinal;
function FileGetAttr(const FileName: string): Integer;
function MakeIntResource(Value: Integer): PChar;
function MakeWord(A, B: Byte): Word;
function MakeLong(A, B: Word): Longint;
function HiWord(L: DWORD): Word;
function HiByte(W: Word): Byte;

procedure GetLocalTime(var st: TSystemTime);

procedure MessageBeep(Value: Integer);   // value ignored

function CoCreateGUID(out Guid: TGUID): HResult;

function Succeeded(Res: HResult): Boolean;
function Failed(Res: HResult): Boolean;
function ResultCode(Res: HResult): Integer;

function GetCurrentProcess: THandle;

function TerminateThread(ThreadID: TThreadID; RetVal: Integer): LongBool;
{
 NOTE:
 The Windows API's  SuspendThread & ResumeThread are functions.
 With QWindows / Linux these are procedures
}
procedure SuspendThread(ThreadID: TThreadID);
procedure ResumeThread(ThreadID: TThreadID);
function GetThreadPolicy(ThreadID: TThreadID): Integer;
procedure SetThreadPolicy(ThreadID: TThreadID; value: Integer);
function GetThreadPriority(ThreadID: TThreadID): Integer;
function SetThreadPriority(ThreadID: TThreadID; priority: Integer): LongBool;

function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: Cardinal;
  lpflOldProtect: Pointer): LongBool; overload;
function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: Cardinal;
  var OldProtect: Cardinal): LongBool; overload;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: LongWord; var lpNumberOfBytesRead: Longword): LongBool;
function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: LongWord; var lpNumberOfBytesWritten: Longword): LongBool;
procedure FlushInstructionCache(PID: cardinal; OrgCalProc: Pointer; size: Integer);

{ Limitations:
    - GetKeyState calls GetAsyncKeyState
    - GetAsyncKeyState only supports VK_SHIFT, VK_CONTROL and VK_MENU }
function GetKeyState(nVirtKey: Integer): SmallInt;
function GetAsyncKeyState(vKey: Integer): SmallInt;

// events are limited to the process
function CreateEvent(EventAttributes: PSecurityAttributes;
  ManualReset, InitialState: LongBool; Name: PChar): THandle;
function OpenEvent(DesiredAccess: Longword; InheritHandle: LongBool;
  Name: PChar): THandle;
{$ENDIF LINUX}
function SetEvent(Event: THandle): LongBool;
function ResetEvent(Event: THandle): LongBool;
function PulseEvent(Event: THandle): LongBool; // calls SetEvent()

{$IFDEF LINUX}
function CreateMutex(MutexAttributes: PSecurityAttributes; InitialOwner: LongBool;
  Name: PChar): THandle;
function OpenMutex(DesiredAccess: Longword; InheritHandle: Boolean;
  Name: PChar): THandle;
function ReleaseMutex(Mutex: THandle): LongBool;

function CreateSemaphore(SemaphoreAttributes: PSecurityAttributes;
  InitialCount, MaximumCount: Longint; Name: PChar): THandle;
function OpenSemaphore(DesiredAccess: Longword; InheritHandle: LongBool;
  Name: PChar): THandle;
function ReleaseSemaphore(Semaphore: THandle; ReleaseCount: Longint;
  PreviousCount: PInteger): LongBool;

function semtimedop(semid: Integer; sops: PSemaphoreBuffer;
  nsops: size_t; timeout: PTimeSpec): Integer; {$IFDEF DEBUG}cdecl;{$ENDIF}

function WaitForSingleObject(Handle: THandle; Milliseconds: Cardinal): Cardinal;

{ Operate on semaphore.  }

function WaitForMultipleObjects(Count: Cardinal; Handles: PWOHandleArray;
  WaitAll: LongBool; Milliseconds: Cardinal): Cardinal;

{ all Handles are TObject derived classes }

function CloseHandle(hObject: THandle): LongBool;

{ memory management }
function GlobalAllocPtr(Flags: Integer; Bytes: Longint): Pointer;
function GlobalReAllocPtr(P: Pointer; Bytes: Longint; Flags: Integer): Pointer;
function GlobalFreePtr(P: Pointer): THandle;

function GlobalAlloc(uFlags: Cardinal; dwBytes: Longword): Cardinal;
function GlobalReAlloc(hMem: Cardinal; dwBytes: Longword; uFlags: Cardinal): Cardinal;
function GlobalSize(hMem: Cardinal): Longword;
function GlobalLock(hMem: Cardinal): Pointer;
function GlobalHandle(Mem: Pointer): Cardinal;
function GlobalUnlock(hMem: Cardinal): LongBool;
function GlobalFree(hMem: Cardinal): Cardinal;
{$ENDIF LINUX}


{$IFDEF LINUX}
var
  Shell: string = 'kfmclient exec'; // KDE. Gnome equivalent ?
  IpcDirectory: string = '/tmp/kylix/ipc'; // for named semaphores/mutex
{$ENDIF LINUX}

implementation

{$IFDEF MSWINDOWS}
uses
  ShellAPI, DateUtils;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
uses
  Xlib;
{$ENDIF LINUX}

const
  VersionInfo = '$RCSfile$' + #13 + '$Revision$' + #13 + '$Date$' + #13;

type
  THackCanvas = class(TCanvas);
  TOpenWidgetControl = class(TWidgetControl);

var
  AppEventHook: TAppEventHook = nil;

procedure AppEventHookNeeded;
begin
  if not Assigned(AppEventHook) then
    AppEventHook := TAppEventHook.Create(nil);
end;
{ used internally }

procedure MapPainterLP(Handle: QPainterH; var x, y: Integer); overload;
begin
  QWMatrix_map(QPainter_worldMatrix(Handle), x, y, @x, @y);
end;

procedure MapPainterLP(Handle: QPainterH; var x0, y0, x1, y1: Integer); overload;
var
  Matrix: QWMatrixH;
begin
  Matrix := QPainter_worldMatrix(Handle);
  QWMatrix_map(Matrix, x0, y0, @x0, @y0);
  QWMatrix_map(Matrix, x1, y1, @x1, @y1);
end;

procedure MapPainterLP(Handle: QPainterH; var Pt: TPoint); overload;
begin
  QWMatrix_map(QPainter_worldMatrix(Handle), PPoint(@Pt), PPoint(@Pt));
end;

procedure MapPainterLP(Handle: QPainterH; var R: TRect); overload;
begin
  QWMatrix_map(QPainter_worldMatrix(Handle), PRect(@R), PRect(@R));
end;

procedure MapPainterLPwh(Handle: QPainterH; var Width, Height: Integer); overload;
var
  Matrix: QWMatrixH;
begin
  Matrix := QPainter_worldMatrix(Handle);

  Matrix := QWMatrix_create(QWMatrix_m11(Matrix), QWMatrix_m12(Matrix),
    QWMatrix_m21(Matrix), QWMatrix_m22(Matrix), 0, 0);  // no translation
  try
    QWMatrix_map(Matrix, Width, Height, @Width, @Height);
  finally
    QWMatrix_destroy(Matrix);
  end;
end;

function CreateMappedRegion(Handle: QPainterH; Region: QRegionH): QRegionH;
var
  Matrix, RelativeMatrix: QWMatrixH;
  Bmp1, Bmp2: QBitmapH;
  Painter: QPainterH;
  R, FillR: TRect;
  Brush: QBrushH;
begin
  Result := QRegion_create(Region);
  Matrix := QPainter_worldMatrix(Handle);

  if (QWMatrix_m11(Matrix) = 1) and (QWMatrix_m12(Matrix) = 0) and
     (QWMatrix_m21(Matrix) = 0) and (QWMatrix_m22(Matrix) = 1) then
  begin
    if (QWMatrix_dx(Matrix) <> 0) or (QWMatrix_dy(Matrix) <> 0) then
      QRegion_translate(Result, Round(QWMatrix_dx(Matrix)), Round(QWMatrix_dy(Matrix)));
  end
  else
  begin
    RelativeMatrix := QWMatrix_create(
      QWMatrix_m11(Matrix), QWMatrix_m12(Matrix),
      QWMatrix_m21(Matrix), QWMatrix_m22(Matrix),
      0, 0
    );
    QRegion_boundingRect(Result, @R);
    QRegion_translate(Result, -R.Left, -R.Top);

    Bmp1 := QBitmap_create(Abs(R.Right - R.Left), Abs(R.Bottom - R.Top), True,
      QPixmapOptimization_DefaultOptim);
    try
      FillR := R;
      OffsetRect(FillR, -R.Left, -R.Top);

      Painter := QPainter_create(Bmp1);
      try
        QPainter_setClipRegion(Painter, Result);
        QPainter_setClipping(Painter, True);

        Brush := GetStockObject(BLACK_BRUSH);
        QPainter_fillRect(Painter, @FillR, Brush);
        DeleteObject(Brush);
      finally
        QPainter_destroy(Painter);
      end;
      QRegion_destroy(Result);

      QWMatrix_map(RelativeMatrix, PRect(@R), PRect(@R));

      Bmp2 := QBitmap_create(Abs(R.Right - R.Left), Abs(R.Bottom - R.Top), False,
        QPixmapOptimization_DefaultOptim);
      try
        QPixmap_xForm(Bmp1, Bmp2, RelativeMatrix);
        Result := QRegion_create(Bmp2);
      finally
        QBitmap_destroy(Bmp2);
      end;
    finally
      QBitmap_destroy(Bmp1);
    end;
  end;
end;

{---------------------------------------}
type
  PPainterInfo = ^TPainterInfo;
  TPainterInfo = record
    Painter: QPainterH;
    IsCompatibleDC: Boolean;
    TextAlignment: Cardinal;
    StetchBltMode: TStretchMode;
    MapMode: TMapMode;
  end;

var
  PainterInfos: TList = nil;

function GetPainterInfo(Handle: QPainterH; var Info: PPainterInfo): Boolean;
var
  i: Integer;
begin
  Result := False;
  Info := nil;
  if PainterInfos <> nil then
  begin
    for i := 0 to PainterInfos.Count - 1 do
      if PPainterInfo(PainterInfos[i])^.Painter = Handle then
      begin
        Result := True;
        Info := PPainterInfo(PainterInfos[i]);
        Exit;
      end;
  end;
end;

function NewPainterInfo(Handle: QPainterH): PPainterInfo;
begin
  New(Result);
  Result^.Painter := Handle;
  Result^.IsCompatibleDC := False;
  Result^.TextAlignment := TA_LEFT or TA_TOP;
  Result^.StetchBltMode := STRETCH_DELETESCANS;
  Result^.MapMode := MM_TEXT;

  if PainterInfos = nil then
    PainterInfos := TList.Create;
  PainterInfos.Add(Result);
end;

function SetPainterInfo(Handle: QPainterH): PPainterInfo;
var
  i: Integer;
begin
  Result := nil;
  if PainterInfos <> nil then
  begin
    for i := 0 to PainterInfos.Count - 1 do
      if PPainterInfo(PainterInfos[i])^.Painter = Handle then
      begin
        Result := PPainterInfo(PainterInfos[i]);
        Exit;
      end;
  end;
  if Result = nil then
    Result := NewPainterInfo(Handle);
end;

procedure DeletePainterInfo(Handle: QPainterH);
var
  P: PPainterInfo;
begin
  if PainterInfos <> nil then
  begin
    if GetPainterInfo(Handle, P) then
    begin
      PainterInfos.Delete(PainterInfos.IndexOf(P));
      Dispose(P);
    end;
  end;
end;

procedure FreePainterInfos;
var
  i: Integer;
begin
  if PainterInfos <> nil then
  begin
    for i := 0 to PainterInfos.Count - 1 do
      Dispose(PPainterInfo(PainterInfos[i]));
    PainterInfos.Free;
  end;
end;
{----------------------------------------}

procedure WakeUpGuiThread;
begin
  QApplication_wakeUpGuiThread(Application.Handle);
end;

function DrawTextBiDiModeFlagsReadingOnly: Longint;
begin
  Result := 0;
end;

function DrawTextBiDiModeFlags(Flags: Longint): Longint;
begin
  Result := Flags;
end;

procedure ChangeBiDiModeAlignment(var Alignment: TAlignment);
begin
end;

function UseRightToLeftAlignment: Boolean;
begin
  Result := False;
end;

function GetSysColor(SysColor: Integer): TColorRef;
// VCL / windows colors
begin
  if (SysColor >= 0) and (SysColor <= COLOR_ENDCOLORS) then
    SysColor := GetSysColor( Win2TColor[SysColor] );
  case SysColor of
    clInfoBk:
      Result := TColorRef(Application.HintColor);
    clDeskTop:
      Result := TColorRef(QColorColor(QWidget_BackgroundColor(QApplication_desktop)));
  else
    Result := TColorRef(Application.Palette.GetColor(SysColor));
  end;
end;

const
  ColorRoles: array[1..15] of TColorRole =(
    crForeground, crButton, crLight, crMidlight, crDark, crMid,
    crText, crBrightText, crButtonText, crBase, crBackground, crShadow,
    crHighlight, crHighlightText, crNoRole);

function SetSysColor(RefColor: TColor; TrueColor: TColorRef): Boolean;
begin
  with Application.Palette do // asn: positive values or only rgb values?
    if TrueColor >= TColor(0)  then
    begin
      Result := True;
      case RefColor of
        clNormalNoRole..clNormalForeground:
          SetColor(cgInactive, ColorRoles[-(RefColor+cloNormal)], TrueColor);
        clDisabledHighlightedText..clDisabledForeground:
          SetColor(cgDisabled, ColorRoles[-(RefColor+cloDisabled)], TrueColor);
        clActiveNoRole..clActiveForeground:
          SetColor(cgActive, ColorRoles[-(RefColor+cloActive)], TrueColor);
      else
        Result := False
      end;
    end
    else  // if
      Result := False;   // only rgb values are accepted (asn: see remark above)

end;

function SetSysColors(Elements: Integer; const lpaElements;
  const lpaRgbValues): LongBool;
var
  i: Integer;
  refcolor : PColor;
  realcolor : PColor;
begin
  Result := True;
  refcolor := PColor(lpaElements);
  realcolor := PColor(lpaRGBvalues);
  Application.Palette.BeginUpdate;
  try
    for i := 0 to Elements-1 do
    begin
      if not SetSysColor( refcolor^, realcolor^)
      then
        Result := False;
      inc(refcolor);
      inc(realcolor);
    end;
  finally
    Application.Palette.EndUpdate;
  end;
end;

type
  TStockObjectResource = class(TObject)
  private
    FStockObject: Integer;
    FHandle: Pointer;
    FRefCount: Integer;
  public
    constructor Create(AHandle: Pointer; AStockObject: Integer);
    destructor Destroy; override;
    function AddRef: Pointer;
    procedure Release;

    property Handle: Pointer read FHandle;
    property StockObject: Integer read FStockObject;
  end;

  TStockObjectList = class(TObjectList)
  public
    function FindStockObject(AStockObject: Integer): TStockObjectResource;
    class function ReleaseStockObject(AHandle: Pointer): Boolean;
  end;

var
  StockObjectList: TStockObjectList = nil;
  StockObjectListCritSect: TRTLCriticalSection;

constructor TStockObjectResource.Create(AHandle: Pointer; AStockObject: Integer);
begin
  inherited Create;
  AddRef;
  FHandle := AHandle;
  FStockObject := AStockObject;
end;

destructor TStockObjectResource.Destroy;
type
  Int = Integer;
begin
  case StockObject of
    Int(WHITE_BRUSH)..Int(NULL_BRUSH), Int(DC_BRUSH):
      QBrush_destroy(QBrushH(Handle));
    Int(WHITE_PEN)..Int(NULL_PEN), Int(DC_PEN):
      QPen_destroy(QPenH(Handle));
    Int(OEM_FIXED_FONT)..Int(DEVICE_DEFAULT_FONT), Int(SYSTEM_FIXED_FONT), Int(DEFAULT_GUI_FONT):
      QFont_destroy(QFontH(Handle));
  end;
  StockObjectList.Extract(Self);
  inherited Destroy;
end;

function TStockObjectResource.AddRef: Pointer;
begin
  Inc(FRefCount);
  Result := Handle;
end;

procedure TStockObjectResource.Release;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
    Free;
end;

function TStockObjectList.FindStockObject(AStockObject: Integer): TStockObjectResource;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := TStockObjectResource(Items[i]);
    if Result.StockObject = AStockObject then
      Exit;
  end;
  Result := nil;
end;

class function TStockObjectList.ReleaseStockObject(AHandle: Pointer): Boolean;
var
  i: Integer;
begin
  EnterCriticalSection(StockObjectListCritSect);
  try
    if Assigned(StockObjectList) then
    begin
      for i := 0 to StockObjectList.Count - 1 do
        if TStockObjectResource(StockObjectList.Items[i]).Handle = AHandle then
        begin
          TStockObjectResource(StockObjectList.Items[i]).Release;
          Result := True;
          Exit;
        end;
    end;
    Result := False;
  finally
    LeaveCriticalSection(StockObjectListCritSect);
  end;
end;

function GetStockObject(fnObject: Integer): Pointer; overload;
const
  BrushColors: array[WHITE_BRUSH..BLACK_BRUSH] of TColor =
    (clWhite, clLtGray, clGray, clDkGray, clBlack);
  {$IFDEF MSWINDOWS}
  SystemFont: WideString = 'System';
  GuiFont: array[Boolean] of WideString = ('MS Sans Serife', 'Tahoma');
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  SystemFont: WideString = 'Fixed';   // asn: is not always True
//  GuiFont: array[Boolean] of WideString = ('Verdana', 'Verdana');
//asn:  in JVCL units Helvetica is used. Why introduce another?
  GuiFont: array[Boolean] of WideString = ('Helvetica', 'Helvetica');
  {$ENDIF LINUX}
type
  Int = Integer;
var
  Resource: TStockObjectResource;
  GuiFontSelector: Boolean;
begin
  {$IFDEF MSWINDOWS}
  GuiFontSelector := Win32MajorVersion >= 5;
  {$ELSE}
  GuiFontSelector := True;
  {$ENDIF MSWINDOWS}
  EnterCriticalSection(StockObjectListCritSect);
  try
    if not Assigned(StockObjectList) then
      StockObjectList := TStockObjectList.Create;
    Resource := StockObjectList.FindStockObject(fnObject);
    if Resource <> nil then
      Result := Resource.AddRef
    else
    begin
      case fnObject of
        Int(WHITE_BRUSH)..Int(BLACK_BRUSH):
          Result := CreateSolidBrush(BrushColors[TStockObjectBrush(fnObject)]);
        Int(NULL_BRUSH):
          Result := QBrush_create(BrushStyle_NoBrush);
        Int(DC_BRUSH):
          Result := CreateSolidBrush(clWhite);
        Int(WHITE_PEN):
          Result := CreatePen(PS_SOLID, 1, clWhite);
        Int(BLACK_PEN):
          Result := CreatePen(PS_SOLID, 1, clBlack);
        Int(NULL_PEN):
          Result := CreatePen(PS_NULL, 1, clWhite);
        Int(DC_PEN):
          Result := CreatePen(PS_SOLID, 1, clWhite);
        Int(OEM_FIXED_FONT), Int(ANSI_FIXED_FONT), Int(SYSTEM_FONT), Int(SYSTEM_FIXED_FONT):
          Result := QFont_create(@SystemFont, 8, 1, False);
        Int(ANSI_VAR_FONT), Int(DEVICE_DEFAULT_FONT), Int(DEFAULT_GUI_FONT):
          Result := QFont_create(@GuiFont[GuiFontSelector], 8, 1, False);
      else
        Result := nil;
      end;
      if Result <> nil then
        StockObjectList.Add(TStockObjectResource.Create(Result, fnObject));
    end;
  finally
    LeaveCriticalSection(StockObjectListCritSect);
  end;
end;

function GetStockObject(fnObject: TStockObjectBrush): QBrushH;
begin
  Result := QBrushH(GetStockObject(Integer(fnObject)));
end;

function GetStockObject(fnObject: TStockObjectPen): QPenH;
begin
  Result := QPenH(GetStockObject(Integer(fnObject)));
end;

function GetStockObject(fnObject: TStockObjectFont): QFontH;
begin
  Result := QFontH(GetStockObject(Integer(fnObject)));
end;

function GetMapMode(Handle: QPainterH): TMapMode;
var
  P: PPainterInfo;
begin
  if GetPainterInfo(Handle, P) then
    Result := P.MapMode
  else
    Result := MM_TEXT;
end;

function SetMapMode(Handle: QPainterH; MapMode: TMapMode): TMapMode;
var
  Matrix: QWMatrixH;
  dpi: TSize;
  m11, m22: Double;
  dx, dy: Double;

  procedure SetM(const Am11, Am22: Double);
  begin
    m11 := Am11;
    m22 := Am22;
  end;

begin
  dpi.cx := GetDeviceCaps(Handle, LOGPIXELSX);
  dpi.cy := GetDeviceCaps(Handle, LOGPIXELSY);

  Result := GetMapMode(Handle);
  case MapMode of
    MM_TEXT:
      SetM(1, 1);

    MM_LOMETRIC:
      SetM((dpi.cx / 2.54) / 100, -(dpi.cy / 2.54) / 100);
    MM_HIMETRIC:
      SetM((dpi.cx / 2.54) / 1000, -(dpi.cy / 2.54) / 1000);

    MM_LOENGLISH:
      SetM(dpi.cx / 10, -dpi.cy / 10);
    MM_HIENGLISH:
      SetM(dpi.cx / 100, -dpi.cy / 100);

    MM_TWIPS:
      SetM(dpi.cx / 1440, -dpi.cy / 1440);

    MM_ISOTROPIC:

  end;
 // translate matrix
  if QPainter_hasWorldXForm(Handle) then
  begin
    dx := QWMatrix_dx(QPainter_worldMatrix(Handle));
    dy := QWMatrix_dy(QPainter_worldMatrix(Handle));
  end
  else
  begin
    dx := 0;
    dy := 0;
  end;
  Matrix := QWMatrix_create(m11, 0, 0, m22, dx, dy);
  QPainter_setWorldMatrix(Handle, Matrix, False);
  QWMatrix_destroy(Matrix);
  SetPainterInfo(Handle).MapMode := MapMode;
end;

function CreatePen(Style, Width: Integer; Color: TColor): QPenH;
var
  QC: QColorH;
begin
  QC := QColor(Color);
  Result := QPen_create(QC, Width, PenStyle(Style));
  QColor_destroy(QC);
end;

function DeleteObject(Handle: QPenH): LongBool;
begin
  try
//    if not TStockObjectList.ReleaseStockObject(Handle) then
      QPen_destroy(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function CreateSolidBrush(Color: TColor): QBrushH;
var
  QC: QColorH;
begin
  QC := QColor(Color);
  Result := QBrush_create(QC, BrushStyle_SolidPattern);
  QColor_destroy(QC);
end;

function CreateHatchBrush(bStyle: BrushStyle; Color: TColor): QBrushH;
var
  QC: QColorH;
begin
  QC := QColor(Color);
  Result := QBrush_create(QC, bStyle);
  QColor_destroy(QC);
end;

function DeleteObject(Handle: QBrushH): LongBool;
begin
  Result := False;
  if Handle <> nil then
  begin
    try
//      if not TStockObjectList.ReleaseStockObject(Handle) then
        QBrush_destroy(Handle);
      Result := True;
    except
    end;
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
    Result := ShowWindow(Handle, W.ShowCmd);
    with W.rcNormalPosition do
       QWidget_setGeometry(Handle, Left, Top, Right - Left, Bottom - Top);
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
var
  ActWidget: QWidgetH;
begin
  try
    case ShowCmd of
      SW_MINIMIZE, SW_SHOWMINIMIZED:
        QWidget_showMinimized(Handle);
      SW_MAXIMIZE:
        QWidget_showMaximized(Handle);
      SW_HIDE:
        QWidget_hide(Handle);
      SW_SHOWNOACTIVATE, SW_SHOWMINNOACTIVE:
        begin
          ActWidget := QApplication_activeWindow(Application.Handle);
          if ShowCmd = SW_SHOWNOACTIVATE then
            QWidget_showNormal(Handle)
          else
            QWidget_showMinimized(Handle);
          if Assigned(ActWidget) then
            QWidget_setActiveWindow(ActWidget);
        end;
    else
      QWidget_showNormal(Handle);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

type
  THackedWidgetControl = class(TWidgetControl);

function SetWindowPos(Wnd, WndInsertAfter: QWidgetH; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool;
var
  R, Geometry: TRect;
  WidgetFlags: Cardinal;
  Control: THackedWidgetControl;
  LastActiveWidget: QWidgetH;
  LastActiveWinId: Cardinal;
begin
  Result := False;
  if Wnd = nil then
    Exit;
  try
    LastActiveWidget := QApplication_activeWindow(Application.Handle);
    if LastActiveWidget <> nil then
      LastActiveWinId := QWidget_winId(LastActiveWidget)
    else
      LastActiveWinId := 0;

    // we must use CLX methods or CLX will be a pain.
    Control := THackedWidgetControl(FindControl(Wnd));

    if Control <> nil then
      Geometry := Control.BoundsRect
    else
      QWidget_geometry(Wnd, @Geometry);

    if uFlags and SWP_HIDEWINDOW <> 0 then
    begin
      if Control <> nil then
        Control.Hide
      else
        QWidget_hide(Wnd);
    end;

    if uFlags and SWP_NOSIZE <> 0 then
    begin
      cx := Geometry.Right - Geometry.Left;
      cy := Geometry.Bottom - Geometry.Top;
    end;
    if uFlags and SWP_NOMOVE <> 0 then
    begin
      X := Geometry.Left;
      Y := Geometry.Top;
    end;
    R := Rect(X, Y, X + cx, Y + cy);
    if not EqualRect(R, Geometry) then
    begin
      if Control <> nil then
        Control.BoundsRect := R
      else
        QWidget_setGeometry(Wnd, X, Y, cx, cy);
    end;

    if uFlags and SWP_FRAMECHANGED <> 0 then
    begin
      QWidget_adjustSize(Wnd);
      if Control <> nil then
        Control.AdjustSize;
    end;

    if (uFlags and SWP_NOOWNERZORDER = 0) and (uFlags and SWP_NOZORDER = 0) then
      if (not QWidget_isTopLevel(Wnd)) and (QWidget_parentWidget(Wnd) <> nil) then
        SetWindowPos(QWidget_parentWidget(Wnd), WndInsertAfter, 0, 0, 0, 0,
          SWP_NOSIZE or SWP_NOMOVE or SWP_NOREDRAW or SWP_NOACTIVATE or
          SWP_NOCOPYBITS or SWP_NOSENDCHANGING);

    if (uFlags and SWP_NOZORDER = 0) then
    begin
      WidgetFlags := QOpenWidget_getWFlags(QOpenWidgetH(Wnd));

      case Cardinal(WndInsertAfter) of
        HWND_TOP:
          QWidget_raise(Wnd);
        HWND_BOTTOM:
          QWidget_lower(Wnd);
        HWND_TOPMOST:
          if (Control <> nil) and (TWidgetControl(Control) is TForm) then
            TForm(Control).FormStyle := fsStayOnTop
          else
            QOpenWidget_setWFlags(QOpenWidgetH(Wnd),
              WidgetFlags or Cardinal(WidgetFlags_WStyle_StaysOnTop));
        HWND_NOTOPMOST:
          if (Control <> nil) and (TWidgetControl(Control) is TForm) then
            TForm(Control).FormStyle := fsNormal
          else
            QOpenWidget_setWFlags(QOpenWidgetH(Wnd),
              WidgetFlags and not Cardinal(WidgetFlags_WStyle_StaysOnTop));
      else
       // remove top most state
        if WidgetFlags and Cardinal(WidgetFlags_WStyle_StaysOnTop) <> 0 then
        begin
          if (Control <> nil) and (TWidgetControl(Control) is TForm) then
            TForm(Control).FormStyle := fsNormal
          else
            QOpenWidget_setWFlags(QOpenWidgetH(Wnd),
              WidgetFlags and not Cardinal(WidgetFlags_WStyle_StaysOnTop));
        end;
        // after widget
        QWidget_stackUnder(Wnd, WndInsertAfter);
      end;
    end;

    if uFlags and SWP_SHOWWINDOW <> 0 then
    begin
      if Control <> nil then
        Control.Show
      else
        QWidget_show(Wnd);
    end;

    if (uFlags and SWP_NOACTIVATE = 0) and (QWidget_isVisible(Wnd)) then
      QWidget_setActiveWindow(Wnd);

    if (uFlags and SWP_NOREDRAW = 0) and (QWidget_isVisible(Wnd)) then
      QWidget_update(Wnd);

    if (uFlags and SWP_NOACTIVATE <> 0) and Assigned(LastActiveWidget) then
      if QWidget_find(LastActiveWinId) = LastActiveWidget then // valid LastActiveWidget
        QWidget_setActiveWindow(LastActiveWidget);
  except
    Result := False;
  end;
end;

function SetWindowPos(Wnd, WndInsertAfter: Cardinal; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool;
begin
  Result := SetWindowPos(QWidgetH(Wnd), QWidgetH(WndInsertAfter), X, Y, cx, cy, uFlags);
end;

function SetWindowPos(Wnd: QWidgetH; WndInsertAfter: Cardinal; X, Y, cx, cy: Integer;
  uFlags: Longword): LongBool;
begin
  Result := SetWindowPos(Wnd, QWidgetH(WndInsertAfter), X, Y, cx, cy, uFlags);
end;

procedure MoveWindowOrg(DC: QPainterH; DX, DY: Integer);
var
  P: TPoint;
begin
  GetWindowOrgEx(DC, P);
  SetWindowOrgEx(DC, P.X - DX, P.Y - DY, nil);
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

function GetBkMode(Handle: QPainterH): Integer;
begin
  case QPainter_BackgroundMode(Handle) of
    BGMode_TransparentMode:
      Result := TRANSPARENT; // BGMode_TransparentMode
    BGMode_OpaqueMode:
      Result := OPAQUE; // BGMode_OpaqueMode
  else
    Result := OPAQUE;
  end;
end;

function SetBkMode(Handle: QPainterH; BkMode: Integer): Integer;
begin
  Result := GetBkMode(Handle);
  case BkMode of
    TRANSPARENT:
      QPainter_setBackgroundMode(Handle, BGMode_TransparentMode);
    OPAQUE:
      QPainter_setBackgroundMode(Handle, BGMode_OpaqueMode);
  end;
end;

function SetPenColor(Handle: QPainterH; Color: TColor): TColorRef;
var
  QC: QColorH;
begin
  Result :=  QColorColor(QPen_color(QPainter_pen(Handle)));
  QC := QColor(Color);
  QPainter_setPen(Handle, QC);
  QColor_destroy(QC);
end;

function SetTextColor(Handle: QPainterH; Color: TColor): TColorRef;
begin
  Result := SetPenColor(Handle, Color);
end;

function SetBkColor(Handle: QPainterH; Color: TColor): TColorRef;
var
  QC: QColorH;
begin
  Result := QColorColor(QPainter_backgroundColor(Handle));
  QC := QColor(Color);
  QPainter_setBackgroundColor(Handle, QC);
  QColor_destroy(QC);
end;

function SetDCBrushColor(Handle: QPainterH; Color: TColor): TColorRef;
var
  QC: QColorH;
begin
  Result := QColorColor(QBrush_color(QPainter_brush(Handle)));
  QC := QColor(Color);
  QPainter_setBrush(Handle, QC);
  QColor_destroy(QC);
end;

function SetDCPenColor(Handle: QPainterH; Color: TColor): TColorRef;
begin
  Result := SetPenColor(Handle, Color);
end;

procedure SetPainterFont(Handle: QPainterH; Font: QGraphics.TFont);
begin
  QPainter_setFont(Handle, Font.Handle);
  QPainter_setPen(Handle, Font.FontPen);
end;

function GetParent(Handle: QWidgetH): QWidgetH;
begin
  Result := QWidget_parentWidget(Handle);
end;

function SetParent(hWndChild, hWndNewParent: QWidgetH): QWidgetH;
var
  Pt: TPoint;
begin
  try
    Result := GetParent(hWndChild);
    QWidget_pos(hWndChild, @Pt);
    QWidget_reparent(hWndChild, hWndNewParent, @Pt, QWidget_isVisible(hWndChild));
  except
    Result := nil;
  end;
end;

function RasterOpToWinRop(Rop: RasterOp): cardinal;
begin
  case Rop of
    RasterOp_ClearROP   : Result := BLACKNESS;
    RasterOp_NotROP     : Result := DSTINVERT;
    RasterOp_NotOrRop   : Result := MERGEPAINT;
    RasterOp_NotCopyROP : Result := NOTSRCCOPY;
    RasterOp_NorROP     : Result := NOTSRCERASE;
    RasterOp_AndROP     : Result := SRCAND;
    RasterOp_CopyROP    : Result := SRCCOPY;
    RasterOp_AndNotROP  : Result := SRCERASE;
    RasterOp_XorROP     : Result := SRCINVERT;
    RasterOp_OrROP      : Result := SRCPAINT;
    RasterOp_SetROP     : Result := WHITENESS;
    RasterOp_NotAndROP  : Result := ROP_DSna;
    RasterOp_NopROP     : Result := ROP_D;
    RasterOp_OrNotROP   : Result := ROP_SDno;
    RasterOp_NandROP    : Result := ROP_DSan;
  else
    Result := 0;   // to satisfy compiler
  end;
end;

function WinRopToRasterOp(WinRop: Cardinal; var Rop: RasterOp): Boolean;
begin
  Result := True;
  case WinRop of
    BLACKNESS  : Rop := RasterOp_ClearROP;
    DSTINVERT  : Rop := RasterOp_NotROP;
//    MERGECOPY  : Rop := RasterOp_OrROP;     {DSa}
    MERGEPAINT : Rop := RasterOp_NotOrRop;
    NOTSRCCOPY : Rop := RasterOp_NotCopyROP;
    NOTSRCERASE: Rop := RasterOp_NorROP;
    SRCAND     : Rop := RasterOp_AndROP;
    SRCCOPY    : Rop := RasterOp_CopyROP;
    SRCERASE   : Rop := RasterOp_AndNotROP;
    SRCINVERT  : Rop := RasterOp_XorROP;
    SRCPAINT   : Rop := RasterOp_OrROP;
    WHITENESS  : Rop := RasterOp_SetROP;
    ROP_DSna   : Rop := RasterOp_NotAndROP;
    ROP_D      : Rop := RasterOp_NopROP;
    ROP_SDno   : Rop := RasterOp_OrNotROP;
    ROP_DSan   : Rop := RasterOp_NandROP;
  else
    Rop := RasterOp(-1);
    Result := False;
  end;
end;

function PatternPaint(DestDC: QPainterH; X, Y, W, H: Integer; rop: RasterOp): LongBool;
var
  trop: RasterOp;
  bkmode: Integer;
begin
  try
    trop := QPainter_rasterOp(DestDC);
    bkmode := SetBkMode(DestDC, OPAQUE);
    try
      QPainter_setRasterOp(DestDC, rop);    // asn: or use current ?
      QPainter_fillRect(DestDC, X, Y, W, H, QPainter_brush(DestDC)); // current brush
    finally
      QPainter_setRasterOp(DestDC, trop);
      SetBkMode(DestDC, bkmode);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function BitBlt(DestDC: QPainterH; X, Y, Width, Height: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: Boolean): LongBool;
var
  TempDC: QPainterH;
  Rop: RasterOp;
begin
  if WinRopToRasterOp(WinRop, Rop) then  // directly maps ?
    Result := BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Rop, IgnoreMask)
  else // no
  begin
    case WinRop of
      MERGECOPY: { PSa: Dest := Pattern AND Source }
        begin
          try
            TempDC := CreateCompatibleDC(DestDC, Width, Height);
            try
              PatternPaint(TempDc, 0, 0, Width, Height, RasterOp_CopyROP); // Create Pattern
              BitBlt(TempDc, 0, 0, Width, Height, SrcDC, XSrc, YSrc, RasterOp_AndRop); {PSa}
              Result := BitBlt(DestDc, X, Y, Width, Height, tempDC, XSrc, YSrc, RasterOp_CopyROP);
            except
              Result := False;
            end;
            QPainter_destroy(tempDC);
          except
            Result := False;
          end;
        end;

      PATCOPY:
        Result := PatternPaint(DestDC, X, Y, Width, Height, RasterOp_CopyROP);

      PATINVERT:
        Result := PatternPaint(DestDC, X, Y, Width, Height, RasterOp_XorROP);

      PATPAINT:
        begin  // DPSnoo   = PDSnoo
          Result := BitBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc,
                           RasterOp_NotOrRop); // DSno
          if Result then
            Result := PatternPaint(DestDC, X, Y, Width, Height, RasterOp_XOrROP);
        end;

      ROP_DSPDxax:
        begin
          TempDC := CreateCompatibleDC(DestDC, Width, Height);
          try
            // copy DestDC to pixmap
            BitBlt(TempDC, 0, 0, Width, Height, DestDC, X, Y,  RasterOp_CopyROP);
            BitBlt(TempDC, 0, 0, Width, Height, TempDC, 0, 0, PATINVERT);  // PDx
            BitBlt(TempDC, 0, 0, Width, Height, SrcDC, XSrc, YSrc, RasterOp_AndROP); // SPDxa
            Result := BitBlt(DestDC, X, Y, Width, Height, TempDC, 0, 0, RasterOp_XorROP); // DSPDxax
          except
            Result := False;
          end;
          DeleteObject(TempDC);
        end;
    else
      Result := False;
    end;
  end
end;

function BitBlt(DestDC: QPainterH; X, Y, Width, Height: Integer; SrcDC: QPainterH;
  XSrc, YSrc: Integer; Rop: RasterOp; IgnoreMask: Boolean): LongBool;
var
  d_dx, d_dy, d_sx, d_sy, d_sw, d_sh, d_dw, d_dh: Integer;
begin
  if (DestDC = nil) or (SrcDC = nil) then
    Result := False
  else
  begin
    Result := True;
    try
     // Windows's BitBlt uses logical units
      d_dx := X;d_dy := Y;d_dw := Width;d_dh := Height;
      d_sx := XSrc; d_sy := YSrc;d_sw := Width; d_sh := Height;

      MapPainterLP(DestDC, d_dx, d_dy);
      MapPainterLPwh(DestDC, d_dw, d_dh);
      MapPainterLP(SrcDC, d_sx, d_sy);
      MapPainterLPwh(SrcDC, d_sw, d_sh);
      
      if (d_dw = d_sw) and (d_dh = d_sh) then // device bitBlt possible
        Qt.bitBlt(QPainter_device(DestDC), d_dx, d_dy, QPainter_device(SrcDC),
          d_sx, d_sy, d_sw, d_sh, Rop,
          IgnoreMask) // ignore the Mask because Windows's BitBlt does not use Masks
      else
        StretchBlt(DestDC, X, Y, Width, Height, SrcDC, XSrc, YSrc, Width, Height,
          Rop, IgnoreMask);
    except
      Result := False;
    end;
  end;
end;

function PainterOffset(Canvas: TCanvas): TPoint;
var
  aControl: TControl;
begin
  Result.X := 0;
  Result.Y := 0;
  if Canvas is TControlCanvas then
  begin
    AControl := TControlCanvas(Canvas).Control;
    if AControl = nil then
      Exit;
    if not (AControl is TWidgetControl) then
    begin
      Result.X := aControl.Left;
      Result.Y := aControl.Top;
    end;
  end;
end;

function BitBlt(DestCanvas: TCanvas; X, Y, Width, Height: Integer; SrcCanvas: TCanvas;
  XSrc, YSrc: Integer; WinRop: Cardinal; IgnoreMask: boolean): LongBool;
begin
  DestCanvas.Start;
  SrcCanvas.Start;
  Result := BitBlt(DestCanvas.Handle, X, Y , Width, Height, SrcCanvas.Handle,
    XSrc, YSrc, WinRop, IgnoreMask);
  SrcCanvas.Stop;
  DestCanvas.Stop;
end;

const
  CopyModeToRasterOp: array[TCopyMode] of RasterOp = (
    {cmBlackness}   RasterOp_ClearROP,    {cmDstInvert}  RasterOp_NotROP,
    {cmMergeCopy}   RasterOp_AndROP,      {cmMergePaint} RasterOP_NotOrROP,
    {cmNotSrcCopy}  RasterOp_NotCopyROP,  {cmNotSrcErase}RasterOp_NorROP,
    {cmPatCopy}     RasterOp_NopROP,      {cmPatInvert}  RasterOp_NopROP,
    {cmPatPaint}    RasterOp_NotOrROP,    {cmSrcAnd}     RasterOp_AndROP,
    {cmSrcCopy}     RasterOp_CopyROP,     {cmSrcErase}   RasterOp_AndNotROP,
    {cmSrcInvert}   RasterOp_XorROP,      {cmSrcPaint}   RasterOP_OrROP,
    {cmWhiteness}   RasterOp_SetROP,      {cmCreateMask} RasterOp_NopROP);


procedure CopyRect(DstCanvas: TCanvas; const Dest: TRect; Canvas: TCanvas;
  const Source: TRect);
begin
  StretchBlt(DstCanvas, Dest.Left, Dest.Top,
    Dest.Right - Dest.Left, Dest.Bottom - Dest.Top,
    Canvas, Source.Left, Source.Top,
    Source.Right - Source.Left, Source.Bottom - Source.Top,
    RasterOpToWinRop(CopyModeToRasterOp[DstCanvas.CopyMode]));
end;

procedure BrushCopy(DstCanvas: TCanvas; const Dest: TRect; Bitmap: TBitmap;
  const Source: TRect; Color: TColor);
var
  Bmp: TBitmap;
  X, Y: integer;
begin
  Bmp := TBitmap.Create;
  with Bmp do
  begin
    Width := Source.Right - Source.Left;
    Height := Source.Bottom - Source.Top;
    Canvas.Start;
    Canvas.Draw( -Source.Left, -Source.Top, Bitmap);
    TransparentColor := Color;
    Transparent := True;
    Canvas.Stop;
  end;
  with DstCanvas do
  begin
    Start;
    FillRect(Dest);
    X := Dest.Left;
    while X < Dest.Right do
    begin
      Y := Dest.Top;
      while Y < Dest.Bottom do
      begin
        Draw(X, Y, Bmp);
        Y := Y + Bmp.Height;
      end;
      X := X + Bmp.Width;
    end;
    Stop;
  end;
  Bmp.Free;
end;

function PatBlt(Handle: QPainterH; X, Y, Width, Height: Integer; WinRop: Cardinal): LongBool;
begin
  Result := BitBlt(Handle, X, Y, Width, Height, Handle, X, Y, WinRop);
end;

function PatBlt(Canvas: TCanvas; X, Y, Width, Height: Integer; WinRop: Cardinal): LongBool;
begin
  Canvas.Start;
  Result := BitBlt(Canvas.Handle, X, Y, Width, Height, Canvas.Handle, X, Y, WinRop);
  Canvas.Stop;
end;

function StretchBlt(DestDC: QPainterH; dx, dy, dw, dh: Integer;
  SrcDC: QPainterH; sx, sy, sw, sh: Integer; WinRop: Cardinal;
  IgnoreMask: Boolean): LongBool;
var
  Bmp1, Bmp2: QPixmapH;
  Painter : QPainterH;
  d_sx, d_sy, d_sw, d_sh: Integer;
  d_dx, d_dy, d_dw, d_dh: Integer;
begin
  Result := False;
  if (DestDC = nil) and (QPainter_isActive(DestDC)) then
    Exit;

 // Windows's StretchBlt uses logical units
  d_sx := sx;d_sy := sy;d_sw := sw;d_sh := sh;
  d_dx := dx;d_dy := dy;d_dw := dw;d_dh := dh;

  MapPainterLP(DestDC, d_dx, d_dy);
  MapPainterLPwh(DestDC, d_dw, d_dh);
  MapPainterLP(SrcDC, d_sx, d_sy);
  MapPainterLPwh(SrcDC, d_sw, d_sh);

  if (d_dw = d_sw) and (d_dh = d_sh) then // device bitBlt possible
    Result := BitBlt(DestDC, dx, dy, dw, dh, SrcDC, sx, sy, WinRop, IgnoreMask)
  else
  begin
    if not QPainter_isActive(SrcDC) then
      Exit;
    try
      Bmp1 := nil;
      Bmp2 := nil;
      try
       // temporary bitmaps are in device units
        Bmp1 := CreateCompatibleBitmap(SrcDC, d_sw, d_sh);
        Bmp2 := CreateCompatibleBitmap(DestDC, d_dw, d_dh);
        Qt.bitBlt(Bmp1, 0, 0, QPainter_device(SrcDC), d_sx, d_sy, d_sw, d_sh,
          RasterOp_CopyROP, IgnoreMask); // use device units
        Painter := QPainter_create(Bmp2);
        QPainter_save(Painter);
        QPainter_scale(Painter, d_dw/d_sw, d_dh/d_sh);
        QPainter_drawPixmap(Painter, 0, 0, Bmp1, 0, 0, d_sw, d_sh);
        QPainter_restore(Painter);
        Result := BitBlt(DestDC, dx, dy, dw, dh, Painter, 0, 0, WinRop, IgnoreMask); // maps logical units
        QPainter_destroy(Painter);
      finally
        if Assigned(Bmp1) then
          QPixmap_destroy(Bmp1);
        if Assigned(Bmp2) then
          QPixmap_destroy(Bmp2);
      end;
    except
      Result := False;
    end;
  end;
end;

function StretchBlt(DestDC: QPainterH; dx, dy, dw, dh: Integer;
  SrcDC: QPainterH; sx, sy, sw, sh: Integer; Rop: RasterOp; IgnoreMask: Boolean): LongBool;
begin
  Result := StretchBlt(DestDC, dx, dy, dw, dh, SrcDC, sx, sy, sw, sh,
    RasterOpToWinRop(Rop), IgnoreMask);
end;

function StretchBlt(DestCanvas: TCanvas; dx, dy, dw, dh: Integer;
  SrcCanvas: TCanvas; sx, sy, sw, sh: Integer; WinRop: Cardinal; IgnoreMask: Boolean): LongBool;
var
  d,s :TPoint;
begin
  DestCanvas.Start;
  SrcCanvas.Start;
  d := PainterOffset(DestCanvas);
  s := PainterOffset(SrcCanvas);
  Result := StretchBlt(DestCanvas.Handle, dx + d.x, dy + d.y, dw, dh, SrcCanvas.Handle,
    sx + s.x, sy + s.y, sw, sh, WinRop,  IgnoreMask);
  SrcCanvas.Stop;
  DestCanvas.Stop;
end;

function GetStretchBltMode(DC: QPainterH): TStretchMode;
var
  P: PPainterInfo;
begin
  if DC <> nil then
  begin
    if GetPainterInfo(DC, P) then
      Result := P.StetchBltMode
    else
      Result := STRETCH_DELETESCANS;
  end
  else
    Result := STRETCH_DELETESCANS;
end;

function SetStretchBltMode(DC: QPainterH; StretchMode: TStretchMode): TStretchMode;
begin
  try
    Result := GetStretchBltMode(DC);
    SetPainterInfo(DC).StetchBltMode := StretchMode;
  except
    Result := STRETCH_DELETESCANS;
  end;
end;

function ScrollDC(Handle: QPainterH; dx, dy: Integer; var Scroll, Clip: TRect;
  Rgn: QRegionH; Update: PRect): LongBool;
var
  R1, R2: TRect;
  rg1, rg2: QRegionH;
begin
  // assume device units = pixels
  IntersectRect(R2, Scroll, Clip); // clipped source rectangle
  OffsetRect(R2, dx, dy);
  IntersectRect(R2, R2, Clip);  // R2: clipped destination rectangle
  if not isRectEmpty(R2) then
  begin
    R1 := R2;
    OffsetRect(R1, -dx, -dy); // R1: adjusted source rectangle
    Result := BitBlt(Handle, R2.Left, R2.Top, R2.Right-R2.Left, R2.Bottom-R2.Top,
           Handle, R1.Left, R1.Top, SRCCOPY);
  end
  else
    Result := False;    // asn: or True ?
  if (Rgn <> nil) or  (Update <> nil) then
  begin
    rg1 := CreateRectRgnIndirect(Scroll);
    rg2 := CreateRectRgnIndirect(R2);
    CombineRgn(rg2, rg1, rg2, RGN_DIFF);
    if Rgn <> nil then
      CombineRgn(Rgn, Rg2, Rg2, RGN_OR);
    if Update <> nil then
      QRegion_boundingRect(rg2, Update);
    QRegion_destroy(rg2);
    QRegion_destroy(rg1);
  end;
end;

function SetROP2(Handle: QPainterH; Rop: Integer): Integer;
var
  rop2: RasterOp;
begin
  case Rop of
    R2_BLACK      : rop2 := RasterOp_ClearROP;
    R2_WHITE      : rop2 := RasterOp_SetROP;
    R2_NOP        : rop2 := RasterOp_NopROP;
    R2_NOT        : rop2 := RasterOp_NotROP;
    R2_COPYPEN    : rop2 := RasterOp_CopyROP;
    R2_NOTCOPYPEN : rop2 := RasterOp_NotCopyROP;
    R2_MERGEPENNOT: rop2 := RasterOp_OrNotROP;
    R2_MASKPENNOT : rop2 := RasterOp_AndNotROP;
    R2_MERGEPEN   : rop2 := RasterOp_OrROP;
    R2_NOTMERGEPEN: rop2 := RasterOp_NorROP;
    R2_MASKPEN    : rop2 := RasterOp_AndROP;
    R2_NOTMASKPEN : rop2 := RasterOp_NandROP;
    R2_XORPEN     : rop2 := RasterOp_XorROP;
    R2_NOTXORPEN  : rop2 := RasterOp_NotXorROP;
    R2_MASKNOTPEN : rop2 := RasterOp_NotAndROP;
    R2_MERGENOTPEN: rop2 := RasterOp_NotOrROP;
  else
    Result := -1;
    Exit;
  end;
  Result := GetROP2(Handle);
  QPainter_setRasterOp(Handle, rop2);
end;

function GetROP2(Handle: QPainterH): Integer;
var
  rop2: RasterOp;
begin
  rop2 := QPainter_rasterOp(Handle);
  case rop2 of
    RasterOp_ClearROP  : Result := R2_BLACK;
    RasterOp_SetROP    : Result := R2_WHITE;
    RasterOp_NopROP    : Result := R2_NOP;
    RasterOp_NotROP    : Result := R2_NOT;
    RasterOp_CopyROP   : Result := R2_COPYPEN;
    RasterOp_NotCopyROP: Result := R2_NOTCOPYPEN;
    RasterOp_OrNotROP  : Result := R2_MERGEPENNOT;
    RasterOp_AndNotROP : Result := R2_MASKPENNOT;
    RasterOp_OrROP     : Result := R2_MERGEPEN;
    RasterOp_NorROP    : Result := R2_NOTMERGEPEN;
    RasterOp_AndROP    : Result := R2_MASKPEN;
    RasterOp_NandROP   : Result := R2_NOTMASKPEN;
    RasterOp_XorROP    : Result := R2_XORPEN;
    RasterOp_NotXorROP : Result := R2_NOTXORPEN;
    RasterOp_NotAndROP : Result := R2_MASKNOTPEN;
    RasterOp_NotOrROP  : Result := R2_MERGENOTPEN;
  else
    Result := -1;
  end;
end;

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
    while not QWidget_isTopLevel(Handle) do
      Handle := QWidget_parentWidget(Handle);
    Result := Handle <> nil;
    if Result then
    begin
      QWidget_show(Handle);
      QWidget_raise(Handle);
    end;
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
        QWidget_show(Handle);
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
    if Result then
      QWidget_close(Handle)
  except
    Result := False;
  end;
end;

function DestroyWindow(Handle: QWidgetH): LongBool;
begin
  try
    Result := QWidget_isTopLevel(Handle);
    if Result then
      QWidget_destroy(Handle);
  except
    Result := False;
  end;
end;

function WindowFromDC(Handle: QPaintDeviceH): QWidgetH;
var
  WidgetList: QObjectListH;
  Widget: QWidgetH;
  I: integer;
begin
  Result := nil;
  WidgetList := QObject_queryList(Application.AppWidget, 'QWidget','*', true, true);
  for I := 0 to QObjectList_count(WidgetList) - 1  do
  begin
    Widget := QWidgetH(QObjectList_at(WidgetList, I));
    if QWidget_isVisible(Widget) and (QWidget_to_QPaintDevice(Widget) = Handle) then
    begin
      Result := Widget;
      break;
    end;
  end;
  QObjectList_destroy(WidgetList);
end;

function WindowFromDC(Handle: QPainterH): QWidgetH;
begin
  Result := nil;
  if QPainter_isActive(Handle) then
    Result := WindowFromDC(QPainter_device(Handle));
end;


function ChildWindowFromPoint(hWndParent: QWidgetH; Point: TPoint): QWidgetH;
begin
  try
    Result := QApplication_widgetAt(@Point, True);
  except
    Result := nil;
  end;
end;

function WindowFromPoint(Point: TPoint): QWidgetH;
begin
  try
    Result := QApplication_widgetAt(@Point, False);
  except
    Result := nil;
  end;
end;

function FindCLXWindow(const Point: TPoint): TWidgetControl;
var
  Handle: QWidgetH;
begin
  Handle := WindowFromPoint(Point);
  Result := nil;
  while Handle <> nil do
  begin
    Result := FindControl(Handle);
    if Result <> nil
    then
      Exit;
    Handle := GetParent(Handle);
  end;
end;

function FindVCLWindow(const Point: TPoint): TWidgetControl;
begin
  Result := FindCLXWindow(Point);
end;

function GetClassName(Handle: QWidgetH; Buffer: PChar; MaxCount: Integer): Integer;
begin
  Result := 0;
  if Handle <> nil then
  begin
    Result := Length(QObject_className(Handle));
    if Buffer <> nil then
      StrLCopy(Buffer, QObject_className(Handle), MaxCount);
  end;
end;

function IsIconic(Handle: QWidgetH): LongBool;
begin
  Result := False;
  if Handle = nil then
    Exit;
  try
    Result := QWidget_isMinimized(Handle);
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

function SmallPointToPoint(const P: TSmallPoint): TPoint;
begin
  Result := Point(P.x, P.y);
end;

function PointToSmallPoint(const P: TPoint): TSmallPoint;
begin
  Result.x := SmallInt(P.X);
  Result.y := SmallInt(P.Y);
end;

function MapWindowPoints(WidgetTo, WidgetFrom: QWidgetH; var Points; nr: Cardinal): Integer;
var
  i: Integer;
  p1: PPoint;
  p2: TPoint;
begin
  p1 := @Points;
  try
    if (IsChild(WidgetTo, WidgetFrom)) and (nr > 0) then
    begin
      QWidget_mapTo(WidgetFrom, p1, WidgetTo, @p2);
      TSmallPoint(Result).x := p2.X - p1.X;
      TSmallPoint(Result).y := p2.Y - p1.Y;
      for i:= 0 to nr - 1 do
      begin
        QWidget_mapTo(WidgetFrom, p1, WidgetTo, p1);
        Inc(p1);
      end;
    end
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

function InvalidateRect(Handle: QWidgetH; R: PRect; EraseBackground: Boolean): LongBool;
var
  Control: TWidgetControl;
begin
  Result := False;
  if Handle = nil then
    Exit;
  try
    Control := FindControl(Handle);
    if Control <> nil then
      Control.InvalidateRect(R^, EraseBackGround);
    Result := True;
  except
    Result := False;
  end;
end;

function ValidateRect(hWnd: QWidgetH; R: PRect): LongBool;
var
  Event: QPaintEventH;
begin
  Event := QPaintEvent_create(R, false);
  try
    Result := QApplication_sendEvent(hWnd, QEventH(Event));
  finally
    QPaintEvent_destroy(Event);
  end;
end;

function UpdateWindow(Handle: QWidgetH): LongBool;
begin
  Result := False;
  if Handle <> nil then
    Exit;
  try
    QWidget_update(Handle);
    Result := True;
  except
    Result := False;
  end;
end;

function IsChild(ParentHandle, ChildHandle: QWidgetH): LongBool;
var
  ParentH: QWidgetH;
begin
  Result := False;
  ParentH := QWidget_parentWidget(ChildHandle);
  while (ParentH <> nil) and (not Result) do
  begin
    Result := ParentH = ParentHandle;
    ParentH := QWidget_parentWidget(ParentH);
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
  Result := QPainter_brush(Handle);
  QPainter_setBrush(Handle, Brush);
end;

function SelectObject(Handle: QPainterH; Pen: QPenH): QPenH;
begin
  Result := QPainter_pen(Handle);
  QPainter_setPen(Handle, Pen);
end;

function SelectObject(Handle: QPainterH; Bitmap: QPixmapH): QPixmapH;
var
  P: PPainterInfo;
begin
  if GetPainterInfo(Handle, P) and (P.IsCompatibleDC) then
  begin
    Result := QPixmapH(QPainter_device(Handle)); // IsCompatibleDC -> device is QPixmapH
    if QPainter_isActive(Handle) then
      QPainter_end(Handle);

    QPainter_begin(Handle, Bitmap);
  end
  else
    //Result := nil;
    raise Exception.Create('SelectObject(HBITMAP) is limited to CreateCompatibleDC handles');
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

function CreateEllipticRgn(Left, Top, Right, Bottom: Integer): QRegionH;
begin
  Result := QRegion_create(Left, Top, Right - Left, Bottom - Top, QRegionRegionType_Ellipse);
end;

function CreateEllipticRgnIndirect(Rect: TRect): QRegionH;
begin
  Result := QRegion_create(@Rect, QRegionRegionType_Ellipse);
end;

function CreateRectRgn(Left, Top, Right, Bottom: Integer): QRegionH;
var
  R: TRect;
begin
  SetRect(R, Left, Top, Right, Bottom);
  Result := QRegion_create(@R, QRegionRegionType_Rectangle);
end;

function CreateRectRgnIndirect(Rect: TRect): QRegionH;
begin
  Result := QRegion_create(@Rect, QRegionRegionType_Rectangle);
end;

function CreateRoundRectRgn(x1, y1, x2, y2, WidthEllipse, HeightEllipse: Integer): QRegionH;
var
  Bmp: QBitmapH;
  Painter: QPainterH;
begin
  Bmp := QBitmap_create(x2-x1+1, y2-y1+1, True, QPixmapOptimization_DefaultOptim);
  Painter := QPainter_create(Bmp);
  QPainter_setBrush(Painter, QPen_color(QPainter_pen(painter)));
  QPainter_drawRoundRect(Painter, 0, 0, x2-x1, y2-y1, WidthEllipse, HeightEllipse);
  QPainter_destroy(Painter);
  Result := QRegion_create(Bmp);
  QBitmap_destroy(Bmp);
  QRegion_translate(Result, x1, y1);
end;

function CreatePolygonRgn(const Points; Count, FillMode: Integer): QRegionH;
var
  pts: TPointArray;
  i: Integer;
  p: PPoint;
begin
  SetLength(pts, Count);
  p := PPoint(Points);
  for i := 0 to Count - 1 do
  begin
    pts[i].X := p.X;
    pts[i].Y := p.Y;
    Inc(p);
  end;
  Result := QRegion_create(@pts[0], Fillmode = WINDING);
end;

function SelectClipRgn(Handle: QPainterH; Region: QRegionH): Integer;
var
  Clipping: Boolean;
begin
  Result := RGN_ERROR;
  if Handle = nil then
    Exit;
  try
    Clipping := Region <> nil;
    if Clipping then
    begin
      Region := CreateMappedRegion(Handle, Region);
      try
        QPainter_setClipRegion(Handle, Region);
        Result := GetRegionType(Region);
      finally
        QRegion_destroy(Region);
      end;
    end
    else
      Result := NULLREGION;
    QPainter_setClipping(Handle, Clipping);
  except
    Result := RGN_ERROR;
  end;
end;

function SelectClipRgn(Handle: QPainterH; Region: Integer): Integer;
begin
  Result := SelectClipRgn(Handle, QRegionH(Region));
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
    QPainter_setClipRegion(Handle, Rgn); // otherwide the new clip region is not accepter
    QPainter_setClipping(Handle, True);
    Result := GetRegionType(Rgn);
  except
    Result := RGN_ERROR;
  end;
  QRegion_destroy(ExcludeRgn);
end;

function ExcludeClipRect(Handle: QPainterH; const R: TRect): Integer;
begin
  with R do
    Result := ExcludeClipRect(Handle, Left, Top, Right, Bottom);
end;

function IntersectClipRect(Handle: QPainterH; X1, Y1, X2, Y2: Integer): Integer;
var
  IntersectRgn, Rgn: QRegionH;
begin
  MapPainterLP(Handle, X1, Y1, X2, Y2);
  IntersectRgn := QRegion_create(X1, Y1, X2 - X1, Y2 - Y1, QRegionRegionType_Rectangle);
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

function IntersectClipRect(Handle: QPainterH; const R: TRect): Integer;
begin
  with R do
    Result := IntersectClipRect(Handle, Left, Top, Right, Bottom);
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
    Result := CombineRgn(tmpRgn, Rgn1, Rgn2, RGN_XOR) = NULLREGION
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

function CombineRgn(DestRgn, Source1, Source2: QRegionH;
  Operation: TCombineMode): Integer;
begin
  try
    case Operation of
      RGN_OR:
        QRegion_unite(Source1, DestRgn, Source2);
      RGN_AND:
        QRegion_intersect(Source1, DestRgn, Source2);
      // RGN_DIFF Subtracts Source2 from Source1
      RGN_DIFF:
        QRegion_subtract(Source1, DestRgn, Source2);
      // RGN_XOR creates the union of two combined regions except for any
      // overlapping areas.
      RGN_XOR:
        QRegion_eor(Source1, DestRgn, Source2);
      // RGN_COPY: Creates a copy of the region identified by Source1.
      RGN_COPY:
        QRegion_unite(Source1, DestRgn, Source1)
    else
      Result := RGN_ERROR;
      Exit;
    end;
    Result := GetRegionType(DestRgn);
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
  OldRgn: QRegionH;
  R: TRect;
  hasClipping: Boolean;
begin
  OldRgn := nil;
  Result := False;
  QPainter_save(Handle);
  try
    hasClipping := QPainter_hasClipping(Handle);
    if hasClipping then
      OldRgn := QPainter_clipRegion(Handle);
    if SelectClipRgn(Handle, Region) <> RGN_ERROR then
    begin
      QRegion_boundingRect(Region, @R);
      QPainter_fillRect(Handle, @R, Brush);
      if hasClipping then
        SelectClipRgn(Handle, OldRgn);
      Result := True;
    end;
  finally
    QPainter_restore(Handle);
  end;
end;

function FrameRgn(Handle: QPainterH; Region: QRegionH; Brush: QBrushH; Width, Height: integer): LongBool;
var
  R: TRect;
  X, Y: integer;
  Pen: QPenH;

  function IsBorderPoint(X, Y: integer): Boolean;
  var
    I, J, K: integer;
  begin
    Result := False;
    if PtInRegion(Region, X, Y) then
    begin
      K := 0;
      For I := -Width to Width do
        For J := -Height to Height do
        begin
          If not PtInRegion(Region, X + I , Y + J) then
          begin
            Inc(K);
            if K > 1 then  // 2 points required windows uses 1 point
            begin
              Result := True;
              exit;
            end;
          end;
        end
    end;
  end;
begin
//  Result := false;
  QPainter_save(Handle);
  try
    QRegion_boundingRect(Region, @R);
    Pen := QPen_create(QBrush_color(Brush), 1, PenStyle_SolidLine);
    try
      QPainter_setPen(Handle, Pen);
    finally
      QPen_destroy(Pen);
    end;
    for X := R.Left to R.Right do
      for Y := R.Top to R.Bottom do
        if IsBorderPoint(X, Y) then
          QPainter_drawPoint(Handle, X, Y);
    Result := True;
  finally
    QPainter_restore(Handle);
  end;
end;

function DeleteObject(Region: QRegionH): LongBool;
begin
  try
//    if not TStockObjectList.ReleaseStockObject(Region) then
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

function RectInRegion(Rgn: QRegionH; const Rect: TRect): LongBool;
begin
  try
    Result := QRegion_contains(Rgn, PRect(@Rect));
  except
    Result := False;
  end;
end;

function SetWindowRgn(Handle: QWidgetH; Region: QRegionH; Redraw: LongBool): Integer;
begin
  Result := 0;
  if (Region <> nil) and (Handle <> nil) then
  begin
    try
      QWidget_setMask(Handle, Region);
      DeleteObject(Region); // Windows owns the window region
      if Redraw then
        UpdateWindow(Handle);
      Result := 1;
    except
      Result := 0;
    end;
  end;
end;

function GetWindowRgn(Handle: QWidgetH; Region: QRegionH): Integer;
begin
  if (Region <> nil) and (Handle <> nil) then
  begin
    try
      // there is no QWidget_mask() function
      // asn: note region without windows/X11 decoration
      QWidget_childrenRegion(Handle, Region);
      Result := GetRegionType(Region);
    except
      Result := ERROR;
    end;
  end
  else
    Result := ERROR;
end;

function LPtoDP(Handle: QPainterH; var Points; Count: Integer): LongBool;
var
  P: PPoint;
begin
  Result := True;
  try
    P := @Points;
    while Count > 0 do
    begin
      Dec(Count);
      QPainter_xForm(Handle, P, P);
      Inc(P);
    end;
  except
    Result := False;
  end;
end;

function DPtoLP(Handle: QPainterH; var Points; Count: Integer): LongBool;
var
  P: PPoint;
begin
  Result := True;
  try
    P := @Points;
    while Count > 0 do
    begin
      Dec(Count);
      QPainter_xFormDev(Handle, P, P);
      Inc(P);
    end;
  except
    Result := False;
  end;
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
    QPainter_setViewport(Handle, X, Y, R.Right - R.Left, R.Bottom - R.Top);
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
      Size.cx := R.Right - R.Left;
      Size.cy := R.Bottom - R.Top;
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
    Size.cx := R.Right - R.Left;
    Size.cy := R.Bottom - R.Top;
  except
    Result := False;
  end;
end;

function GetWindowOrgEx(Handle: QPainterH; Org: PPoint): LongBool;
var
  R :TRect;
begin
  try
    QPainter_window(Handle, @R);
    Org.X := R.Left;
    Org.Y := R.Top;
    Result := True;
  except
    Result := False;
  end;
end;

function GetWindowOrgEx(Handle: QPainterH; var Org: TPoint): LongBool;
begin
  Result := GetWindowOrgEx(Handle, @Org);
end;

function SetWindowOrgEx(Handle: QPainterH; X, Y: Integer; OldOrg: PPoint): LongBool;
var
  R :TRect;
begin
  try
    QPainter_window(Handle, @R);
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

function ReleaseCapture: LongBool;
var
  Handle: QWidgetH;
begin
  Handle := QWidget_mouseGrabber;
  if Handle <> nil then
  begin
    QWidget_releaseMouse(Handle);
    Result := True;
  end
  else
    Result := False;
end;

function SetCapture(Widget: QWidgetH): QWidgetH;
begin
  Result := QWidget_mouseGrabber;
  ReleaseCapture;
  if Widget <> nil then
    QWidget_grabMouse(Widget);
end;

function GetCapture: QWidgetH;
begin
  Result := QWidget_mouseGrabber;
end;

function SetCursor(Handle: QCursorH; Save: Boolean): QCursorH;
begin
  Result := QApplication_overrideCursor;
  if Handle <> nil then
    QApplication_setOverrideCursor(Handle, Save)
  else
    QApplication_restoreOverrideCursor;
end;

// limited implementation of
function GetSystemMetrics(PropItem: TSysMetrics): Integer;
var
  size: TSize;
begin
  case PropItem of
    SM_CXVSCROLL, SM_CXHSCROLL:
      begin
        QStyle_scrollBarExtent(QApplication_Style, @size);
        Result := size.cx;
      end;
    SM_CYVSCROLL, SM_CYHSCROLL:
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
      Result := 1;
        // (ahuser) Windows returns "1"
        //QStyle_DefaultFrameWidth(QApplication_style); // (probably) wrong ?
    SM_CXFRAME, SM_CYFRAME, SM_CXDLGFRAME, SM_CYDLGFRAME:
      Result := QStyle_DefaultFrameWidth(QApplication_style); // or this one
    SM_CYCAPTION:
      Result := 19;
  else
    raise Exception.Create('GetSystemMetrics: unsupported property')
  end;
end;

{ limited implementation of}

function GetDeviceCaps(Handle: QPainterH; devcap: TDeviceCap): Integer;
begin
  Result := GetDeviceCaps(QPainter_device(Handle), devcap);
end;

function GetDeviceCaps(Handle: QPaintDeviceH; devcap: TDeviceCap): Integer;
var
  pdm:  QPaintDeviceMetricsH;
begin
  Result := 0;
  pdm := QPaintDeviceMetrics_create(Handle);
  if pdm <> nil then
  begin
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
        PLANES:
          Result := 1;
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
        raise Exception.Create('QWindows.GetDeviceCaps: unsupported capability');
      end;
    finally
      QPaintDeviceMetrics_destroy(pdm);
    end;
  end;
end;

function pfDevice: TPixelFormat;
var
  DC: QPaintDeviceH;
begin
  DC := QWidget_to_QPaintDevice(QApplication_desktop);
  case GetDeviceCaps(DC, BITSPIXEL) of
     1: Result := pf1bit;
     8: Result := pf8bit;
    16: Result := pf16bit;
    24: Result := pf24bit;
    32: Result := pf32bit;
  else
    Result := pfCustom;
  end;
end;

{ a very limited implementation of }
function GetTextMetrics(Handle: QPainterH; var tt: TTextMetric): Integer;
var
  fm: QFontMetricsH;
  fi: QFontInfoH;
begin
  FillChar(tt, SizeOf(tt), 0);
  with tt do
  begin
    fm := QFontMetrics_create(QPainter_font(Handle));
    try
      tmHeight := QFontMetrics_height(fm);
      tmAscent := QFontMetrics_ascent(fm);
      tmDescent := QFontMetrics_descent(fm);
      tmAveCharWidth := QFontMetrics_width(fm, 'x');
      tmMaxCharWidth := QFontMetrics_maxWidth(fm);
      //tmInternalLeading := 0;
      tmExternalLeading := QFontMetrics_leading(fm);
      //tmOverhang := 0;
    finally
      QFontMetrics_destroy(fm);
    end;

    fi := QFontInfo_create(QPainter_font(Handle));
    try
      QPainter_fontInfo(Handle, fi);
      case QFontInfo_weight(fi) of
        25: // Light
          tmWeight := 300;
        50: // Normal:
          tmWeight := 400;
        63: // DemiBold
          tmWeight := 600;
        75: // Bold
          tmWeight := 700;
        87: // Black
          tmWeight := 900;
      else
        tmWeight := Round(QFontInfo_weight(fi) * 9.5);
      end;

      tmItalic := Ord(QFontInfo_italic(fi));
      tmUnderlined := Ord(QFontInfo_underline(fi));
      tmStruckOut := Ord(QFontInfo_strikeOut(fi));
      if QFontInfo_fixedPitch(fi) then
        tmPitchAndFamily := FIXED_PITCH
      else
        tmPitchAndFamily := VARIABLE_PITCH;
      tmCharSet := DEFAULT_CHARSET;
    finally
      QFontInfo_destroy(fi);
    end;
  end;
  Result := 0;
end;

function ColorToRGB(Color: TColor; Instance: TWidgetControl = nil): TColor;
var
  FColor: QColorH;
  FColorGroup: QPaletteColorGroup;

  FColorRole: QColorGroupColorRole;
  FPalette: QPaletteH;

  function GetPaletteHandle(Widget: TWidgetControl): QPaletteH;
  begin
    if Widget = nil then
      Result := Application.Palette.Handle
    else
    begin
      Result := THackedWidgetControl(Widget).Palette.Handle;
      while (Result = nil) and (Widget.Parent <> nil) do
      begin
        Widget := Widget.Parent;
        Result := THackedWidgetControl(Widget).Palette.Handle;
      end;
      if Result = nil then
        Result := Application.Palette.Handle;
    end;
  end;

begin
  case Color of
  clColorTo..clForeground:
    begin
      if assigned(Instance) then
        if not Instance.Enabled then
          FColorGroup := QPaletteColorGroup_Disabled
        else
          if Instance.Focused then
            FColorGroup := QPaletteColorGroup_Active
          else
            FColorGroup := QPaletteColorGroup_InActive
      else
        FColorGroup := QPaletteColorGroup_InActive ;
    end;
  clActiveColorTo..clActiveForeground:
    begin
      FColorGroup := QPaletteColorGroup_Active;
    end;
  clNormalColorTo..clNormalForeground:
    begin
      FColorGroup := QPaletteColorGroup_InActive;
    end;
  clDisabledColorTo..clDisabledForeground:
    begin
      FColorGroup := QPaletteColorGroup_Disabled;
    end;
  else
    Result := Color;
    Exit;
  end;
  FColorRole := QColorGroupColorRole( $000000f and (-Integer(Color) )); {1..15}
  FPalette := GetPaletteHandle(Instance);
  FColor := QPalette_color(FPalette, FColorGroup, FColorRole);
  Result := QColorColor(FColor);
//  QColor_destroy(FColor);  {not owned}
end;


function RGB(Red, Green, Blue: Integer): TColorRef;
begin
  Result := (Blue shl 16) or (Green shl 8) or Red;
end;

function GetBValue(Col: TColorRef): Byte;
begin
  Result := Byte((Col shr 16) and $FF);
end;

function GetGValue(Col: TColorRef): Byte;
begin
  Result := Byte((Col shr 8) and $FF);
end;

function GetRValue(Col: TColorRef): Byte;
begin
  Result := Byte(Col and $FF);
end;

function SetRect(var R: TRect; Left, Top, Right, Bottom: Integer): LongBool;
begin
  R := Rect(Left, Top, Right, Bottom);
  Result := True;
end;

function CopyRect(var Dst: TRect; const Src: TRect): LongBool;
begin
  Dst := Src;
  Result := True;
end;

function UnionRect(var Dst: TRect; R1, R2: TRect): LongBool;
begin
  Result := True;
  if IsRectEmpty(R1) then
  begin
    if IsRectEmpty(R2) then
      Result := False  // both empty
    else
    begin
      Dst := R2;
      Result := True;
    end;
  end
  else if IsRectEmpty(R2) then
    Dst := R1
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

function EqualPoints(const P1: TPoint; const P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

function CenterRect(InnerRect, OuterRect: TRect): TRect;
var
  w,h : Integer;
begin
  w := InnerRect.Right - InnerRect.Left;
  h := InnerRect.Bottom - InnerRect.Top;
  Result.Left := (OuterRect.Right + OuterRect.Left - w)div 2;
  Result.Top := (OuterRect.Bottom + OuterRect.Top - h)div 2;
  Result := Bounds( Result.Left, Result.Top, w, h );
end;

function SubtractRect(var dR: TRect; const R1, R2: TRect): LongBool;
var
  R3: TRect;
begin
  try
    dR := R1;
    if IntersectRect(R3, R1, R2) then
    begin
      if EqualPoints(R3.BottomRight, R1.BottomRight) then
      begin
        if R3.Top = R1.Top
        then
          dR.Right := R3.Left
        else if R3.Left = R1.Left
        then
          dR.Bottom := R3.Top
      end
      else
      if EqualPoints(R3.TopLeft, R1.TopLeft) then
      begin
        if R3.Bottom = R1.Bottom
        then
          dR.Left := R3.Right
        else if R3.Right = R1.Right
        then
          dR.Bottom := R3.Top;
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function IntersectRect(var R: TRect; const R1, R2: TRect): LongBool;
begin
  Result := Types.IntersectRect(R, R1, R2);
end;

function PtInRect(const R: TRect; X, Y: integer): LongBool;
begin
  with R do
    Result := (X >= Left) and (X <= Right) and
              (Y >= Top) and (Y <= Bottom);
end;

function PtInRect(const R: TRect; pt: TPoint): LongBool;
begin
  Result := PtInRect(R, pt.X, Pt.Y);
end;

procedure TextOutAngle(Handle: QPainterH; Angle, Left, Top: Integer; Text: WideString);
{ deprecated use DrawText instead }
begin
  try
    QPainter_save(Handle);
    QPainter_translate(Handle, Left, Top);
    QPainter_rotate(Handle, -Angle);
    QPainter_drawText(Handle, 0, 0, @Text, -1);
  finally
    QPainter_restore(Handle);
  end;
end;

procedure RequiredState(ACanvas: TCanvas; State: TCanvasState);
begin
  THackCanvas(Acanvas).RequiredState(State);
end;

procedure TextOutAngle(ACanvas: TCanvas; Angle, Left, Top: Integer; Text: WideString);
{ deprecated use DrawText instead }
begin
  ACanvas.Start;
  RequiredState(ACanvas, [csHandleValid, csFontValid, csBrushValid]);
  TextOutAngle(ACanvas.Handle, Angle, Left, Top, Text);
  ACanvas.Stop;
end;

function TextWidth(Handle: QPainterH; Caption: WideString;
  QtFlags: Integer = 0): Integer;
var
  R :TRect;
begin
  QPainter_boundingRect(Handle, @R, @R, QtFlags, PWideString(@Caption), -1, nil);
  Result := R.Right - R.Left;
end;

function TextHeight(Handle: QPainterH; Caption: WideString; R: TRect;
  QtFlags: Integer = 0): Integer;
var
  R1, R2: TRect;
begin
  R1 := R;
  R1.Bottom := MaxInt;
  QPainter_boundingRect(Handle, @R1, @R2, QtFlags, PWideString(@Caption), -1, nil);
  Result := R2.Bottom - R2.Top;
end;

function GetTextExtentPoint32(Handle: QPainterH; const Text: WideString; Len: Integer;
  var Size: TSize): LongBool;
var
  R: TRect;
begin
  try
    QPainter_boundingRect(Handle, @R, @R, 0, @Text, Len, nil);
    with R do
    begin
      Size.cx := Right - Left;
      Size.cy := Bottom - Top;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function GetTextExtentPoint32(Handle: QPainterH; pText: PChar; Len: Integer;
  var Size: TSize): LongBool;
var
  Text: WideString;
begin
  Text := pText;
  Result := GetTextExtentPoint32(Handle, Text, Len, Size);
end;

function GetTextExtentPoint32(Canvas: TCanvas; const Text: WideString; Len: Integer;
  var Size: TSize): LongBool;
begin
  Canvas.Start;
  RequiredState(Canvas, [csHandleValid, csFontValid, csBrushValid]);
  Result := GetTextExtentPoint32W(Canvas.Handle, PWideChar(Text), Len, Size);
  Canvas.Stop;
end;

function GetTextExtentPoint32W(Handle: QPainterH; pText: PWideChar; Len: Integer;
  var Size :TSize): LongBool;
var
  Text: WideString;
begin
  Text := pText;
  Result := GetTextExtentPoint32(Handle, Text, Len, Size);
end;

function TextExtent(Handle: QPainterH; const Caption: WideString; R: TRect;
  QtFlags: Integer): TRect;
var
  R2: TRect;
begin
  R2 := R;
  R2.Bottom := MaxInt;
  QPainter_boundingRect(Handle, @R2, @Result, QtFlags, PWideString(@Caption), -1, nil);

//  QPainter_boundingRect(Handle, @Result, @Result, Flags, PWideString(@Caption), -1, nil);
end;

const
  Ellipses = '...';

function NameEllipsis(const Name: WideString; Handle: QPainterH;
  MaxLen: Integer; QtFlags: integer = 0): WideString;
var
  I: Integer;
begin
  if TextWidth(Handle, Name, QtFlags) > MaxLen then
  begin
    Result := Ellipses;
    I := 0;
    while TextWidth(Handle, Result, QtFlags) <= MaxLen do
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

function WordEllipsis(Words: WideString; Handle: QPainterH; const R: TRect;
  QtFlags: Integer = 0): WideString;
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
  QPainter_boundingRect(Handle, @R1, @R2, QtFlags, PWideString(@Result), -1, nil);
  if not RectInsideRect(R2, R) then
  begin
    I := 1;
    ShortedText := '';
    while RectInsideRect(R2, R) and (I <= Length(Words)) do
    begin
      repeat                 // one more word
        ShortedText := LeftStr(Words, I);
        if Words[I] = ' ' then
          Break;
        Inc(I);
      until I > Length(Words);
      Result := ShortedText + '...';
      QPainter_boundingRect(Handle, @R1, @R2, QtFlags, PWideString(@Result), -1, nil);
    end;
    While not RectInsideRect(R2, R) and (I > 0) do
    begin
      repeat // one word less
        Dec(I);
        ShortedText := LeftStr(Words, I);
        if ShortedText[I] = ' ' then
          Break;
      until I <= 1;
      Result := ShortedText + Ellipses;
      QPainter_boundingRect(Handle, @R1, @R2, QtFlags, PWideString(@Result), -1, nil);
    end;
  end;
end;

function FileEllipsis(const FilePath: AnsiString; Handle: QPainterH; MaxLen: Integer): string;
var
  Paths: TStrings;
  k, i, Start: Integer;
  CurPath, F: AnsiString;
begin
  if TextWidth(Handle, FilePath, SingleLine) <= MaxLen then
    Result := FilePath
  else
  begin    // FilePath too long
    F := FilePath;
    {$IFDEF LINUX}
    CurPath := IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME'));
    if AnsiStartsStr(CurPath, FilePath) then
    begin
      F := '~/' + AnsiRightStr(FilePath, Length(FilePath) - Length(CurPath));
      if TextWidth(Handle, F, SingleLine) <= MaxLen then
      begin
        Result := F;
        Exit;
      end
    end;
    {$ENDIF LINUX}
    Paths := TStringList.Create;
    try
      Paths.Delimiter := PathDelim;
      Paths.DelimitedText := F; // splits the filepath
      if Paths[0] = '' then
        Start := 1     // absolute path
      else
        Start := 0;    // relative path
      for k := Start to Paths.Count - 2 do
      begin
        CurPath := Paths[k];
        if Length(CurPath) > 2 then   // this excludes '~' '..'
        begin
          Paths[k] := CurPath; // replace with ellipses
          I := Length(CurPath);
          while (I > 0) and (TextWidth(Handle, Paths.DelimitedText, SingleLine) > MaxLen) do
          begin
            Dec(I);
            Paths[k] := LeftStr(CurPath, I) + Ellipses;// remove a character
          end;
          if TextWidth(Handle, Paths.DelimitedText, SingleLine) <= MaxLen then
          begin
            Result := Paths.DelimitedText;
            Exit;
          end;
        end
      end;
      // not succeeded.
      // replace /.../.../.../<filename> with .../<filename>
      // before starting to minimize filename
      for k := Paths.Count - 2 downto 1 do
        Paths.Delete(k);
      Paths[0] := Ellipses;
      if TextWidth(Handle, Paths.DelimitedText, SingleLine) > MaxLen then
      begin
        CurPath := Paths[1];
        Paths[1] := Ellipses; // replace with ellipses
        //I := 1;
        //Paths[1] := CurPath; // replace with ellipses
        I:= Length(CurPath);
        while (I > 0)  and (TextWidth(Handle, Paths.DelimitedText, SingleLine) > MaxLen) do
        begin
          Dec(I);
          Paths[I] := LeftStr(CurPath, I) + Ellipses;// remove a character
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
  try
    Result := FileEllipsis(FilePath, Canvas.Handle, MaxLen);
  finally
    Canvas.Stop;
  end;
end;

function TruncateName(const Name: WideString; Canvas: TCanvas; MaxLen: Integer; QtFlags: integer = 0): WideString;
begin
  Canvas.Start;
  try
    Result := NameEllipsis(Name, Canvas.Handle, MaxLen, QtFlags);
  finally
    Canvas.Stop;
  end;
end;

function DrawText2(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer): Integer;
var
  Flags: Integer;
  R2: TRect;  // bliep bliep bl...
  Caption: WideString;
  FontSaved, FontSet: QFontH;

  function HidePrefix(const Text: WideString): WideString;
  var
    i, Len: Integer;
  begin
    Result := Text;
    Len := Length(Result);
    i := 1;
    while i <= Len do
    begin
      if (Result[i] = '&') then
      begin
        Delete(Result, i, 1);
        Dec(Len);
        if (Result[i] = '&') and (Result[i + 1] = '&') then
        begin
          Delete(Result, i, 1);
          Dec(Len);
        end;
      end;
      Inc(i);
    end;
  end;

  function OnlyPrefix(const Text: WideString): WideString;
  var
    i, Len: Integer;
  begin
    Result := Text;
    Len := Length(Result);
    i := 1;
    while i <= Len do
    begin
      if (Result[i] = '&') then
      begin
        Delete(Result, i, 1);
        Dec(Len);
        if (Result[i] = '&') and (Result[i + 1] = '&') then
        begin
          Delete(Result, i, 1);
          Dec(Len);
        end;
        Result[i] := '&';
      end
      else
        Result[i] := ' ';
      Inc(i);
    end;
  end;

  function CheckTabStop(WinFlags: Integer): Integer;
  var
    Size: Integer;
  begin
    if WinFlags and DT_TABSTOP <> 0 then
    begin
      Size := WinFlags and $FF00;
      WinFlags := WinFlags - Size;
      Size := (Size shr 8) and $FF;
      QPainter_setTabStops(Handle, Size);
    end;
    Result := WinFlags;
  end;

begin
  FontSaved := nil;
  FontSet := nil;
  if len > 0 then
    Caption := LeftStr(Text, len)
  else
    Caption := Text;
  if WinFlags and DT_INTERNAL <> 0 then
  begin
    FontSaved := QPainter_Font(Handle);
    QApplication_font(FontSet, nil);
    QPainter_setFont(Handle, FontSet);
  end;
  Flags := Win2QtAlign(CheckTabStop(WinFlags));
  if WinFlags and DT_PREFIXONLY <> 0 then
  begin
    Flags := Flags or ShowPrefix;
    Caption := OnlyPrefix(Caption);
  end
  else if WinFlags and DT_HIDEPREFIX <> 0 then
  begin
    Flags := Flags and not ShowPrefix;
    Caption := HidePrefix(Caption);
  end;
  if WinFlags and DT_CALCRECT = 0 then
  begin
    if Flags and ClipName <> 0 then
      Caption := NameEllipsis(Caption, Handle, R.Right - R.Left, Flags)
    else if Flags and ClipPath <> 0 then
      Caption := FileEllipsis(Caption, Handle, R.Right - R.Left)
    else if Flags and ClipToWord <> 0 then
      Caption := WordEllipsis(Caption, Handle, R, Flags);
    QPainter_save(Handle);
    if Flags and DontClip = 0 then // clipping
      IntersectClipRect(Handle, R); // QPainter::drawText() does not clip left/top border
    QPainter_DrawText(Handle, @R, Flags, PWideString(@Caption), -1, @R2, nil);
    QPainter_restore(Handle);
    if ModifyString and Flags <> 0 then
    begin
      if len > 0 then
        Caption := Caption + RightStr(Text, Length(Text)-len);
      Text := Caption;
    end;
    Result := R2.Bottom - R2.Top;
  end
  else
  begin
    R2.Left := R.Left;
    R2.Top := R.Top;
    QPainter_boundingRect(Handle, @R, @R, Flags and not $3F{Alignment},
                          PWideString(@Caption), -1, nil);
    if R.Left <> R2.Left then
      OffsetRect(R, R2.Left, 0);
    if R.Top <> R2.Top then
      OffsetRect(R, 0, R2.Top);
//    QPainter_boundingRect(Handle, @R, @R, Flags and not $3F{Alignment},
//                          @Caption, -1, nil);
    Result := R.Bottom - R.Top;
  end;
  if WinFlags and DT_INTERNAL <> 0 then
    QPainter_setFont(Handle, FontSaved);
end;

function DrawText(Handle :QPainterH; Text: PAnsiChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer;
var
  WText: WideString;
  AText: string;
begin
  WText := Text;
  Result := DrawTextW(Handle, PWideChar(WText), Len, R, WinFlags, 0);
  if (DT_MODIFYSTRING and WinFlags <> 0) and (Text <> nil) then
  begin
    AText := WText;
    StrCopy(Text, PChar(AText));
  end;
end;

function DrawText(Handle: QPainterH; var Text: WideString; Len: Integer;
  x,y, w, h: Integer; WinFlags: Integer; Angle: Integer): Integer;
var
  R2: TRect;
begin
  R2 := Bounds(x,y,w,h);
  Result := DrawTextW(Handle,  PWideChar(Text), Len, R2, WinFlags, Angle);
end;

function DrawTextW(Handle :QPainterH; Text: PWideChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: integer = 0): Integer;
var
  WText: WideString;
begin
  WText := Text;
  Result := DrawText2(Handle, WText, Len, R, WinFlags);
  if (DT_MODIFYSTRING and WinFlags <> 0) and (Text <> nil) then
  begin
    Move(WText[1], Text^, Length(WText) * SizeOf(WideChar));
    //WStrCopy(Text, PChar(AText));
  end;
end;

function DrawTextW(Canvas :TCanvas; Text: PWideChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: integer = 0): Integer;
begin
  with Canvas do
  begin
    Start;
    RequiredState(Canvas, [csHandleValid, csBrushValid, csFontValid]);
    Result := DrawTextW(Handle, Text, Len, R, WinFlags, Angle);
    Stop;
  end;
end;

function DrawText(Canvas :TCanvas; Text: PAnsiChar; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer;
begin
  with Canvas do
  begin
    Start;
    RequiredState(Canvas, [csHandleValid, csBrushValid, csFontValid]);
    Result := DrawText(Handle, Text, Len, R, WinFlags, Angle);
    Stop;
  end;
end;

function DrawText(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer;
var
  R2: TRect;
begin
  R2:= R;
  OffsetRect(R2, -R.Left, -R.Top);
  try
    QPainter_save(Handle);
    QPainter_translate(Handle, R.Left, R.Top);
    QPainter_rotate(Handle, -Angle);
    Result := DrawText2(Handle, Text, Len, R2, WinFlags);
  finally
    QPainter_restore(Handle);
  end;
  OffsetRect(R2, R.Left, R.Top);
  R := R2;
end;

function DrawText(Canvas: TCanvas; Text: TCaption; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: integer = 0): Integer;
begin
  with Canvas do
  begin
    Start;
    RequiredState(Canvas, [csHandleValid, csBrushValid, csFontValid]);
    Result := DrawTextW(Handle, PWideChar(Text), Len, R, WinFlags, Angle);
    Stop;
  end;
end;

function DrawText(Handle: QPainterH; Text: TCaption; Len: Integer;
  var R: TRect; WinFlags: Integer; Angle: Integer = 0): Integer; overload;
begin
  Result := DrawTextW(Handle, PWideChar(Text), Len, R, WinFlags, Angle);
end;

function DrawTextEx(Handle: QPainterH; var Text: WideString; Len: Integer;
  var R: TRect; WinFlags: Integer; DTParams: Pointer): Integer;
begin
  Result := DrawTextW(Handle, PWideChar(Text), Len, R, WinFlags);
end;

function DrawTextEx(Handle: QPainterH; Text: PChar; Len: Integer;
  var R: TRect; WinFlags: Integer; DTParams: Pointer): Integer;
begin
  Result := DrawText(Handle, Text, Len, R, WinFlags);
end;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; const Text: WideString; Len: Integer; lpDx: Pointer): LongBool;
var
  WS: WideString;
  Index, Width: Integer;
  Dx: PInteger;
  RR{, CellRect}: TRect;
  TextLen: Integer;
  Canvas: TCanvas;
  TextFlags: Integer;
  CursorPos: TPoint;
begin
  Result := False;
  if (Text = '') then
    Exit;
  if (WinFlags and ETO_CLIPPED <> 0) and (R = nil) then
    WinFlags := WinFlags and not ETO_CLIPPED;

  TextFlags := GetTextAlign(Handle);

  QPainter_save(Handle);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := Handle;
    Canvas.Start(False);
    with Canvas do
    begin
      Result := True;
      if WinFlags and ETO_OPAQUE <> 0 then
      begin
        if Brush.Style <> bsSolid then
          Brush.Style := bsSolid;
        if R <> nil then
          FillRect(R^);
      end
      else
        if Brush.Style = bsSolid then
          Brush.Style := bsClear;

      if lpDx = nil then
      begin
        if (WinFlags and ETO_CLIPPED <> 0) then
          TextRect(R^, X, Y, Text, TextFlags  and $0FFF)
        else
          TextOut(X, Y, Text);
      end
      else
      begin
       // put each char into its cell
        TextLen := Length(Text);
        if (WinFlags and ETO_OPAQUE <> 0) and (R = nil) then
        begin
          Dx := lpDx;
          Width := 0;
          for Index := 1 to TextLen do
          begin
            Inc(Width, Dx^);
            Inc(Dx);
          end;
          RR.Left := X;
          RR.Right := X + Width;
          RR.Top := Y;
          RR.Bottom := Y + TextHeight(Text);
          FillRect(RR);
        end;

        Dx := lpDx;
        SetLength(WS, 1);
        for Index := 1 to TextLen do
        begin
          if (R <> nil) and (X >= R^.Right) then
            Break;

          WS[1] := Text[Index];
          if WinFlags and ETO_CLIPPED <> 0 then
          begin
            {CellRect.Left := X;
            CellRect.Right := X + Dx^;
            CellRect.Top := R^.Top;
            CellRect.Bottom := R^.Bottom;
            if CellRect.Right > R^.Right then
              CellRect.Right := R^.Right;}
            TextRect(RR, X, Y, WS, TextFlags and $0FFF);
          end
          else
            TextOut(X, Y, WS);

          if Index = TextLen then
            Break;

          Inc(X, Dx^);
          Inc(Dx);
        end;
      end;
    end;
  finally
    Canvas.Stop;
    Canvas.Free;
    QPainter_pos(Handle, @CursorPos);
    QPainter_restore(Handle);
    if WinFlags and TA_UPDATECP <> 0 then
      QPainter_moveTo(Handle, @CursorPos);
  end;
end;

function ExtTextOut(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PChar; Len: Integer; lpDx: Pointer): LongBool;
var
  ws: WideString;
begin
  ws := pText;
  Result := ExtTextOut(Handle, X, Y, WinFlags, R, ws, Len, lpDx);
end;

function ExtTextOutW(Handle: QPainterH; X, Y: Integer; WinFlags: Cardinal;
  R: PRect; pText: PWideChar; Len: Integer; lpDx: Pointer): LongBool;
var
  ws: WideString;
begin
  ws := pText;
  Result := ExtTextOut(Handle, X, Y, WinFlags, R, ws, Len, lpDx);
end;

function GetTextAlign(Handle: QPainterH): Cardinal;
var
  P: PPainterInfo;
begin
  if GetPainterInfo(Handle, P) then
    Result := P.TextAlignment
  else
    Result := TA_LEFT or TA_TOP;
end;

function SetTextAlign(Handle: QPainterH; Mode: Cardinal): Cardinal;
begin
  Result := GetTextAlign(Handle);
  if Result <> Mode then
    SetPainterInfo(Handle).TextAlignment := Mode;
end;

function FillRect(Handle: QPainterH; const R: TRect; Brush: QBrushH): LongBool;
begin
  try
    QPainter_fillRect(Handle, @R, Brush);
    Result := True;
  except
    Result := False;
  end;
end;

function DrawIcon(Handle: QPainterH; X, Y: Integer; hIcon: QPixmapH): LongBool;
var
  Pt: TPoint;
begin
  Pt.X := X;
  Pt.Y := Y;
  try
    QPainter_drawPixmap(Handle, @Pt, hIcon);
    Result := True;
  except
    Result := False;
  end;
end;

function DrawIconEx(Handle: QPainterH; X, Y: Integer; hIcon: QPixmapH; W, H: Integer;
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
    if H = 0 then
      H := GetSystemMetrics(SM_CXICON);
  end
  else
  begin         // DI_NORMAL / DI_IMAGE / DI_MASK
    if W = 0 then
    begin
      W := QPixmap_width(hIcon);
      istepIfAniCur := 0;
    end;
    if H = 0 then
      H := QPixmap_height(hIcon);
    if (DiFlags and DI_MASK) = 0 then    // DI_NORMAL / DT_IMAGE
      QPixmap_setMask(hIcon, nil);
  end;

  if QPixmap_width(hIcon) < ((istepIfAniCur + 1) * W) then
    istepIfAniCur := 0;

  if hbrFlickerFreeDraw <> nil then
  begin
    R := Bounds(0, 0, W, H);
    try
      TempDC := CreateCompatibleDC(Handle, W, H);
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

function FrameRect(Handle: QPainterH; const R: TRect; Brush: QBrushH): LongBool;
var
  Pen: QPenH;
begin
  Result := False;
  if (Handle = nil) or (R.Right - R.Left <= 0) or (R.Bottom - R.Top <= 0) or
     (Brush = nil) then
    Exit;
  try
    Pen := nil;
    QPainter_save(Handle);
    try
      Pen := QPen_create(QBrush_color(Brush), 1, PenStyle_SolidLine);
      QPainter_setPen(Handle, Pen);
      QPainter_setBrush(Handle, BrushStyle_NoBrush);
      QPainter_drawRect(Handle, @R);
    finally
      if Assigned(Pen) then
        QPen_destroy(Pen);
      QPainter_restore(Handle);
    end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure FrameRect(Canvas: TCanvas; const R: TRect);
begin
  Canvas.Start;
  try
    FrameRect(Canvas.Handle, R, Canvas.Brush.Handle);
  finally
    Canvas.Stop;
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

function DrawFrameControl(Handle: QPainterH; const Rect: TRect; uType, uState: Longword): LongBool;

  function GetColorGroup(uState: LongWord): QColorGroupH;
  begin
    if uState and DFCS_INACTIVE <> 0 then
      Result := Application.Palette.ColorGroup(cgDisabled)
    else if uState and DFCS_HOT <> 0 then
      Result := Application.Palette.ColorGroup(cgActive)
    else
      Result := Application.Palette.ColorGroup(cgInActive);
  end;

const
  Mask = $00FF;
var
  InnerRect, R: TRect;
  Brush: QBrushH;
  Pen: QPenH;
  Font: QFontH;
  Size: TSize;
  QC: QColorH;
  oBkMode: Integer;
  MaskPainter, Painter: QPainterH;
  MaskBitmap: QBitmapH;
  Pixmap: QPixmapH;
//  FInstance: TControl;
//  FObject: TObject;
begin
  Result := False;
  if (Handle = nil) or (not QPainter_isActive(Handle)) then
    Exit;

  QPainter_save(Handle);
  try
    if uState and DFCS_TRANSPARENT <> 0 then
    begin
      Brush := nil;
      oBkMode := SetBkMode(Handle, TRANSPARENT);
    end
    else
    begin
      oBkMode := SetBkMode(Handle, OPAQUE);
      Brush := QPainter_brush(Handle);

      if uState and DFCS_INACTIVE <> 0 then
        QC := QColor(clDisabledButton)
      else if uState and DFCS_HOT <> 0 then
        QC := QColor(clActiveButton)
      else
        QC := QColor(clNormalButton);
      QPainter_setBackgroundColor(Handle, QC);
      QBrush_setColor(Brush, QC);
      QColor_destroy(QC);
      QPainter_eraseRect(Handle, @Rect);
    end;
    try
      R := Rect;
      case uType of
        DFC_CAPTION:
          begin
            // draw button
            Result := DrawFrameControl(Handle, Rect, DFC_BUTTON,
              DFCS_BUTTONPUSH or (uState and not Mask));
            if Result then
            begin
              // draw image
              Pen := CreatePen(PS_SOLID, 1, clBlack);
              QPainter_setPen(Handle, Pen);
              QPen_destroy(Pen);
              SetBkMode(Handle, TRANSPARENT);
              case uState and Mask of
                DFCS_CAPTIONCLOSE:
                  begin
                    SetRect(R, 0, 0, 6, 6);
                    R := CenterRect(R, Rect);
                    if (uState and DFCS_PUSHED) = 0 then
                      OffsetRect(R, -1, -1);
                    QPainter_moveTo(Handle, R.Left , R.Top);
                    QPainter_lineTo(Handle, R.Right, R.Bottom);
                    QPainter_moveTo(Handle, R.Left , R.Bottom);
                    QPainter_lineTo(Handle, R.Right, R.Top);
                    OffsetRect(R, 1, 0);
                    QPainter_moveTo(Handle, R.Left , R.Top);
                    QPainter_lineTo(Handle, R.Right, R.Bottom);
                    QPainter_moveTo(Handle, R.Left , R.Bottom);
                    QPainter_lineTo(Handle, R.Right, R.Top);
                  end;
                DFCS_CAPTIONMIN:
                  begin
                    SetRect(R, 0, 0, 9, 9);
                    R := CenterRect(R, Rect);
                    if (uState and DFCS_PUSHED) <> 0 then
                      OffsetRect(R, 1, 1);
//                    Inc(R.Left, 4);
//                    Dec(R.Right, 6);
//                    Dec(R.Bottom, 4);
                    QPainter_moveTo(Handle, R.Left , R.Bottom - 1);
                    QPainter_lineTo(Handle, R.Right, R.Bottom - 1);
                    QPainter_moveTo(Handle, R.Left , R.Bottom );
                    QPainter_lineTo(Handle, R.Right, R.Bottom );
                  end;
                DFCS_CAPTIONMAX:
                  begin
                    {$IFDEF MSWINDOWS}
                 //   InflateRect(R, -4, -4);
                    {$ENDIF MSWINDOWS}
                    {$IFDEF LINUX}
                 //   InflateRect(R, -3, -2);
                    {$ENDIF LINUX}
                    SetRect(R, 0, 0, 9, 9);
                    R := CenterRect(R, Rect);
                    if (uState and DFCS_PUSHED) <> 0 then
                      OffsetRect(R, 1, 1);
                    QPainter_drawRect(Handle, @R);
                    QPainter_moveTo(Handle, R.Left, R.Top + 1);
                    QPainter_lineTo(Handle, R.Right - 1, R.Top + 1);
                  end;
                DFCS_CAPTIONRESTORE:
                  begin
                    QPainter_save(Handle);
                    SetRect(R, 0, 0, 6, 6);
                    R := CenterRect(R, Rect);
                    if (uState and DFCS_PUSHED) <> 0 then
                      OffsetRect(R, 1, 1);
                    OffsetRect(R, -2, 1);
                    ExcludeClipRect(Handle, R.Left, R.Top, R.Right, R.Bottom);
                    OffsetRect(R, 2, -3);
                    QPainter_drawRect(Handle, @R);
                    QPainter_moveTo(Handle, R.Left + 1, R.Top + 1);
                    QPainter_lineTo(Handle, R.Right - 1, R.Top + 1);
                    QPainter_restore(Handle);

                    OffsetRect(R, -2, 3);
                    QPainter_drawRect(Handle, @R);
                    QPainter_moveTo(Handle, R.Left + 1, R.Top + 1);
                    QPainter_lineTo(Handle, R.Right - 1, R.Top + 1);
                    QPainter_restore(Handle);
                  end;
                DFCS_CAPTIONHELP:
                  begin
                    Font := QFont_create(Application.Font.Handle);
                    QFont_setBold(Font, True);
                    QPainter_setFont(Handle, Font);
                    QFont_destroy(Font);
                    if (uState and DFCS_PUSHED) = 0 then
                      OffsetRect(R, -1, -1);
                    OffsetRect(R, -1, 0);
                    DrawText(Handle, '?', 1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
                  end;
              end; // case of
            end;
          end; // DFC_CAPTION

        DFC_MENU:
          begin
            // white background color. Windows paints it so we must paint it, too.
            QC := QColor(clWhite);
            QPainter_setBrush(Handle, QC);
            QColor_destroy(QC);

            case uState and Mask of
              DFCS_MENUARROW: // (ahuser) this is the submenu arrow that points left-to-right !
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_RightArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                  Result := True;
                end;
              DFCS_MENUARROWRIGHT: // (ahuser) this is the right submenu arrow that points right-to-left !
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_LeftArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                  Result := True;
                end;
              DFCS_MENUCHECK:
                begin
                  QPainter_fillRect(Handle, @R, QPainter_brush(Handle));
                  QStyle_drawCheckMark(Application.Style.Handle, Handle,
                                       R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                                       GetColorGroup(uState),
                                       uState and DFCS_CHECKED <> 0,
                                       uState and DFCS_INACTIVE <> 0
                                       );
                  Result := True;
                end;
              DFCS_MENUBULLET:
                begin
                  QPainter_fillRect(Handle, @R, QPainter_brush(Handle));
                  R := Types.Rect(0, 0, 7, 7);
                  OffsetRect(R,
                       ((Rect.Right - Rect.Left) - R.Right) div 2 + Rect.Left,
                       ((Rect.Bottom - Rect.Top) - R.Bottom) div 2 + Rect.Top);
                  SetDCBrushColor(Handle, clBlack);
                  SetDCPenColor(Handle, clBlack);
                  QPainter_drawEllipse(Handle, @R);
                  Result := True;
                end;
            end;
          end; // DFC_MENU

        DFC_SCROLL:
          begin
            DrawFrameControl(Handle, Rect, DFC_BUTTON, DFCS_BUTTONPUSH or (uState and not Mask));
            InflateRect(R, -2,-2);
            case uState and Mask of
              DFCS_SCROLLUP:
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_UpArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                  Result := True;
                end;
              DFCS_SCROLLCOMBOBOX, // looks equal to DFCS_SCROLLDOWN
              DFCS_SCROLLDOWN:
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_DownArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                  Result := True;
                end;
              DFCS_SCROLLLEFT:
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_LeftArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                  Result := True;
                end;
              DFCS_SCROLLRIGHT:
                begin
                  QStyle_drawArrow(Application.Style.Handle, Handle,
                                   ArrowType_RightArrow,
                                   uState and DFCS_PUSHED <> 0,
                                   R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top,
                                   GetColorGroup(uState),
                                   uState and DFCS_INACTIVE = 0,
                                   Brush);
                   Result := True;
                end;
            else
              raise Exception.Create('not implemented');
            end;
          end; // DFC_SCROLL

        DFC_BUTTON:
          case uState and Mask of
            DFCS_BUTTONRADIO:
              begin
                QStyle_exclusiveIndicatorSize(Application.Style.Handle, @Size);
                OffsetRect(R, (R.Right - R.Left - Size.cx) div 2,
                              (R.Bottom - R.Top - Size.cy) div 2);
                Pixmap := CreateCompatibleBitmap(Handle, R.Right - R.Left, R.Bottom - R.Top);
                try
                  MaskBitmap := QBitmap_create(R.Right - R.Left, R.Bottom - R.Top,
                    False, QPixmapOptimization_DefaultOptim);
                  try
                    QC := QColor(clBlack);
                    QPixmap_fill(MaskBitmap, QC);
                    QColor_destroy(QC);
                    MaskPainter := QPainter_create(MaskBitmap);
                    QStyle_drawExclusiveIndicatorMask(Application.Style.Handle, MaskPainter,
                      0, 0, QPixmap_width(MaskBitmap), QPixmap_height(MaskBitmap),
                      uState and DFCS_CHECKED <> 0);
                    QPainter_destroy(MaskPainter);
                    QPixmap_setMask(Pixmap, MaskBitmap);
                  finally
                    QBitmap_destroy(MaskBitmap);
                  end;

                  Painter := QPainter_create(Pixmap);
                  QStyle_drawExclusiveIndicator(
                       Application.Style.Handle, Painter,
                       0, 0, R.Right - R.Left, R.Bottom - R.Top,
                       GetColorGroup(uState),
                       uState and DFCS_CHECKED <> 0,
                       uState and DFCS_INACTIVE <> 0,
                       True);
                  QPainter_destroy(Painter);
                  QPainter_drawPixmap(Handle, R.Left, R.Top, Pixmap, 0, 0,
                    QPixmap_width(Pixmap), QPixmap_height(Pixmap));
                finally
                  DeleteObject(Pixmap);
                end;
                Result := True;
              end;
            DFCS_BUTTONCHECK:
              begin
                QStyle_drawIndicatorMask(Application.Style.Handle, Handle,
                   R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, 0);

                if uState and DFCS_INACTIVE = 0  then
                  QStyle_drawIndicator(Application.Style.Handle, Handle,
                     R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                     Application.Palette.ColorGroup(cgActive), //GetColorGroup(cgInActive),
                     0, False, True)
                else
                  QStyle_drawIndicator(Application.Style.Handle, Handle,
                     R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                     Application.Palette.ColorGroup(cgActive), //GetColorGroup(cgInActive),
                     0, True, True);
                InflateRect(R, -2, -2);
                if uState and DFCS_CHECKED	<> 0 then
                  DrawFrameControl(Handle, R, DFC_MENU, DFCS_MENUCHECK or (uState and not Mask));
                Result := True;
              end;
            DFCS_BUTTONPUSH:
              begin
                R := Rect;
                InnerRect := Rect;
                InflateRect(InnerRect, -2, -2);
                if uState and DFCS_ADJUSTRECT <> 0 then
                  PRect(@Rect)^ := InnerRect;
                if uState and DFCS_FLAT <> 0 then
                begin
                  if uState and (DFCS_PUSHED or DFCS_HOT) <> 0 then
                  begin
                    QStyle_drawButton(Application.Style.Handle, Handle,
                        R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                        GetColorGroup(uState),
                        uState and DFCS_PUSHED <> 0,
                        Brush
                        );
                   {QStyle_drawPanel(Application.Style.Handle, Handle,
                        R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                        GetColorGroup(uState),
                        uState and DFCS_PUSHED <> 0,
                        1,
                        Brush);}
                    InflateRect(R, -1, -1);
                  end;
                  Result := True;
                end
                else if uState and DFCS_MONO = 0 then
                begin
                  QStyle_drawButton(Application.Style.Handle, Handle,
                      R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                      GetColorGroup(uState),
                      uState and DFCS_PUSHED <> 0,
                      Brush);
                  InflateRect(R, -2, -2);
                  Result := True;
                end
                else
                begin
                  SetDCPenColor(Handle, clBlack);
                  QPainter_drawRect(Handle, @R);
                  InflateRect(R, -1, -1);
                  Result := True;
                end;
              end;
          else
              // not implemented
            raise Exception.Create('QWindows.DrawFrameControl: not implemented');
          end;

        DFC_POPUPMENU:
          begin
          //
            QStyle_drawPopupPanel(Application.Style.Handle, Handle,
                R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top,
                GetColorGroup(uState), 2, Brush);
          end;
      end;
    finally
      SetBkMode(Handle, oBkMode);
    end;
  finally
    QPainter_restore(Handle);
  end;
end;

function DrawFrameControl(Canvas: TCanvas; const Rect: TRect; uType, uState: Longword): LongBool;
begin
  with Canvas do
  begin
    Start;
    RequiredState(Canvas, [csHandleValid, csBrushValid, csPenValid]);
    Result := DrawFrameControl(Canvas.Handle, Rect, uType, uState);
    Stop;
  end;
end;

function DrawEdge(Handle: QPainterH; var Rect: TRect; Edge: Cardinal;
  Flags: Cardinal): LongBool;
var
  Brush: QBrushH;
  ColorDark, ColorLight: TColor;
  ClientRect: TRect;

  procedure DrawLine(X1, Y1, X2, Y2: Integer);
  begin
    QPainter_moveTo(Handle, X1, Y1);
    QPainter_lineTo(Handle, X2, Y2);
  end;

  procedure DoDrawEdge(Outer: Boolean; const R: TRect);
  var
    X1, Y1, X2, Y2: Integer;
    ColorLeftTop, ColorRightBottom: TColor;
  begin
    X1 := R.Left;
    Y1 := R.Top;
    X2 := R.Right;
    Y2 := R.Bottom;
    ColorLeftTop := clNone;
    ColorRightBottom := clNone;

    if Outer then
    begin
      if Edge and BDR_RAISEDOUTER <> 0 then
      begin
        ColorLeftTop := ColorLight;
        ColorRightBottom := ColorDark;
      end
      else if Edge and BDR_SUNKENOUTER <> 0 then
      begin
        ColorLeftTop := ColorDark;
        ColorRightBottom := ColorLight;
      end;
    end
    else
    begin
      if Edge and BDR_RAISEDINNER <> 0 then
      begin
        ColorLeftTop := ColorLight;
        ColorRightBottom := ColorDark;
      end
      else if Edge and BDR_SUNKENINNER <> 0 then
      begin
        ColorLeftTop := ColorDark;
        ColorRightBottom := ColorLight;
      end;
    end;

    if Flags and BF_DIAGONAL = 0 then
    begin
      SetDCPenColor(Handle, ColorLeftTop);
      if Flags and BF_LEFT <> 0 then
        DrawLine(X1, Y1, X1, Y2);
      if Flags and BF_TOP <> 0 then
        DrawLine(X1, Y1, X2, Y1);

      SetDCPenColor(Handle, ColorRightBottom);
      if Flags and BF_RIGHT <> 0 then
        DrawLine(X2, Y1, X2, Y2);
      if Flags and BF_BOTTOM <> 0 then
        DrawLine(X1, Y2, X2, Y2);
    end
    else
    begin
      // diagonal (does not really work properly with Qt's line algorithm)
      SetDCPenColor(Handle, ColorLeftTop);
      if (Flags and BF_DIAGONAL_ENDTOPLEFT = BF_DIAGONAL_ENDTOPLEFT) or
         (Flags and BF_DIAGONAL_ENDBOTTOMRIGHT = BF_DIAGONAL_ENDBOTTOMRIGHT) then
        DrawLine(X1, Y1, X2, Y2)
      else
      {if (Flags and BF_DIAGONAL_ENDBOTTOMLEFT = BF_DIAGONAL_ENDBOTTOMLEFT) or
         (Flags and BF_DIAGONAL_ENDTOPRIGHT = BF_DIAGONAL_ENDTOPRIGHT) then} // default
        DrawLine(X1, Y2, X2, Y1);
    end;
  end;

begin
  Result := False;
  if Handle = nil then
    Exit;
  try
    ClientRect := Rect;
    QPainter_save(Handle);
    try
      ColorDark := ColorToRGB(clDark);
      ColorLight := ColorToRGB(clLight);
      if Flags and BF_FLAT <> 0 then
        ColorLight := clSilver;
      if Flags and BF_MONO <> 0 then
      begin
        ColorDark := clBlack;
        ColorLight := clWhite;
      end;
      try
        InflateRect(ClientRect, -1, -1); // remove outer rect
        DoDrawEdge(True, Rect); // outer
        DoDrawEdge(False, ClientRect); // inner
        InflateRect(ClientRect, -1, -1); // remove inner rect
      finally

      end;

      if Flags and BF_MIDDLE <> 0 then
      begin
       // fill interior rect
        Brush := CreateSolidBrush(clButton);
        try
          FillRect(Handle, ClientRect, Brush);
        finally
          DeleteObject(Brush);
        end;
      end;

      if Flags and BF_ADJUST <> 0 then
        Rect := ClientRect;

      Result := True;
    finally
      QPainter_restore(Handle);
    end;
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

function MoveToEx(Handle: QPainterH; X, Y:Integer; Point: PPoint): LongBool;
begin
  try
    if Point <> nil then
      QPainter_pos(Handle, Point);
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

function GetWindowDC(Handle: QWidgetH): QPainterH;
begin
  Result := GetDC(Handle);
end;

function ReleaseDC(wdgtH: QWidgetH; Handle: QPainterH): Integer;
begin
  try
    // asn: wdgtH ignored
    QPainter_end(Handle);
    QPainter_destroy(Handle);
    Result := 1;
  except
    Result := 0;
  end;
end;

function ReleaseDC(wdgtH: Integer; Handle: QPainterH): Integer;
begin
  Result := ReleaseDC(QWidgetH(wdgtH), Handle);
end;

function DeleteDC(Handle: QPainterH): LongBool;
var
  P: PPainterInfo;
begin
  if GetPainterInfo(Handle, P) and P.IsCompatibleDC then
    Result := DeleteObject(Handle)
  else
    Result := ReleaseDC(0, Handle) = 1;
end;

function CreateCompatibleDC(Handle: QPainterH; Width: Integer = 1; Height: Integer = 1): QPainterH;
var
  Pixmap: QPixmapH;
  DesktopPainter: Boolean;
begin
  Result := nil;
  try
    if Handle = nil then
    begin
      Handle := GetDC(0);
      DesktopPainter := True;
    end
    else
      DesktopPainter := False;
    try
      Pixmap := CreateCompatibleBitmap(Handle, Width, Height);
      if Pixmap = nil then
        Exit;
      Result := QPainter_create(Pixmap);
      try
        SetPainterInfo(Result).IsCompatibleDC := True;
        QPainter_setPen(Result, QPainter_pen(Handle));
        QPainter_setBackgroundColor(Result, QPainter_BackgroundColor(Handle));
        QPainter_setFont(Result, QPainter_Font(Handle));

        if not QPainter_isActive(Result) then
          QPainter_begin(Result, QPainter_device(Result));
      except
        DeleteObject(Result);
        Result := nil;
      end;
    finally
      if DesktopPainter then
        ReleaseDC(0, Handle);
    end;
  except
    Result := nil;
  end;
end;

function CreateCompatibleBitmap(Handle: QPainterH; Width, Height: Integer): QPixmapH;
var
  pdm: QPaintDeviceMetricsH;
  DesktopPainter: Boolean;
begin
  Result := nil;
  if (Width <= 0) or (Height <= 0) then
    Exit;
  if Handle = nil then
  begin
    Handle := GetDC(0);
    DesktopPainter := True;
  end
  else
    DesktopPainter := False;
  try
    if QPainter_device(Handle) <> nil then
    begin
      try
        pdm := QPaintDeviceMetrics_create(QPainter_device(Handle));
        Result := QPixmap_create(Width, Height, QPaintDeviceMetrics_depth(pdm),
          QPixmapOptimization_DefaultOptim);
        QPaintDeviceMetrics_destroy(pdm);
      except
        Result := nil;
      end;
    end
    else
    begin
      try
        Result := QPixmap_create(Width, Height, -1, QPixmapOptimization_DefaultOptim);
      except
        Result := nil;
      end;
    end;
  finally
    if DesktopPainter then
      ReleaseDC(0, Handle);
  end;
end;

function Convert24To32(Bits: PByte; PixelCount: Cardinal): PByte;
var
  i: Integer;
  P: PByte;
begin
  GetMem(Result, PixelCount * 4);
  P := Result;
  for i := 0 to PixelCount - 1 do
  begin
    P^ := PByte(Bits)^;
    Inc(Bits);
    Inc(P);
    P^ := PByte(Bits)^;
    Inc(Bits);
    Inc(P);
    P^ := PByte(Bits)^;
    Inc(Bits);
    Inc(P);
    P^ := 0;
    Inc(P);
  end;
end;

function CreateBitmap(Width, Height: Integer; Planes, BitCount: Longint; Bits: Pointer): QPixmapH;
var
  Image: QImageH;
  Data: PByte;
begin
  if (Width <= 0) or (Height <= 0) or (Planes <= 0) or (BitCount <= 0) then
    Result := nil
  else
  begin
    try
      Data := PByte(Bits);
      if BitCount = 24 then
      begin
        BitCount := 32;
        if Bits <> nil then
          // convert InitBits
          Data := Convert24To32(Bits, Width * Height);
      end;
      try
        if Data = nil then
          Result := QPixmap_create(Width, Height, BitCount, QPixmapOptimization_DefaultOptim)
        else
        begin
          Image := QImage_create(Width, Height, BitCount, 0, QImageEndian_IgnoreEndian);
          try
            Move(Data^, QImage_bits(Image)^, (Width * Height * BitCount + 7) div 8);
            Result := QPixmap_create;
            try
              QPixmap_convertFromImage(Result, Image, QPixmapColorMode_Auto);
            except
              QPixmap_destroy(Result);
              Result := nil;
            end;
          finally
            QImage_destroy(Image);
          end;
        end;
      finally
        if Data <> Bits then
          FreeMem(Data);
      end;
    except
      Result := nil;
    end;
  end;
end;

function CreateDIBitmap(Handle: QPainterH; var InfoHeader: TBitmapInfoHeader;
  dwUsage: Longword; InitBits: PChar; var InitInfo: TBitmapInfo; wUsage: Cardinal): QPixmapH;
var
  Image: QImageH;
  Data: PByte;
  NumColors: Integer;
begin
  with InfoHeader do
  begin
    if (biWidth <= 0) or (biHeight <= 0) or (biPlanes <> 1) or (biBitCount <= 0) then
      Result := nil
    else
    begin
      Data := PByte(InitBits);
      if biBitCount = 24 then
      begin
        biBitCount := 32;
        if (InitBits <> nil) and (dwUsage = CBM_INIT) then
          // convert InitBits
          Data := Convert24To32(PByte(InitBits), biWidth * biHeight);
      end;
      try
        try
          case biBitCount of
            1: NumColors := 1;
            4: NumColors := 16; // (ahuser) is this supported by Qt ?
            8: NumColors := 256;
          else
            NumColors := 0;
          end;
          Image := QImage_create(biWidth, biHeight, biBitCount, NumColors, QImageEndian_IgnoreEndian);
          try
            if (dwUsage = CBM_INIT) then
            begin
              if (InitBits <> nil) then
                Move(Data^, QImage_bits(Image)^, (biWidth * biHeight * biBitCount + 7) div 8);
              case biBitCount of
                1: Move(InitInfo.bmiColors[0], QImage_colorTable(Image)^, 1 * SizeOf(QRgb));
                4: Move(InitInfo.bmiColors[0], QImage_colorTable(Image)^, 16 * SizeOf(QRgb)); // (ahuser) is this supported by Qt ?
                8: Move(InitInfo.bmiColors[0], QImage_colorTable(Image)^, 256 * SizeOf(QRgb));
              end;
            end;

            Result := QPixmap_create;
            try
              QPixmap_convertFromImage(Result, Image, QPixmapColorMode_Auto);
            except
              QPixmap_destroy(Result);
              Result := nil;
            end;
          finally
            QImage_destroy(Image);
          end;
        except
          Result := nil;
        end;
      finally
        if Data <> PByte(InitBits) then
          FreeMem(Data);
      end;
    end;
  end;
end;

function GetBitmapBits(Bitmap: QPixmapH; Count: Longint; Bits: Pointer): Longint;
var
  Image: QImageH;
begin
  Result := 0;
  if (Bitmap = nil) or (Count <= 0) then
    Exit;
  try
    Image := QImage_create;
    try
      QPixmap_convertToImage(Bitmap, Image);
      Result := (QImage_width(Image) * QImage_height(Image) * QImage_depth(Image) + 7) div 8;
      if Count < Result then
        Result := Count;
      if Result > 0 then
        Move(QImage_bits(Image)^, Bits^, Result);
    finally
      QImage_destroy(Image);
    end;
  except
    Result := 0;
  end;
end;

function SetBitmapBits(Bitmap: QPixmapH; Count: Longint; Bits: Pointer): Longint;
var
  Image: QImageH;
begin
  Result := 0;
  if (Bitmap = nil) or (Count <= 0) then
    Exit;
  try
    Image := QImage_create;
    try
      QPixmap_convertToImage(Bitmap, Image);
      Result := (QImage_width(Image) * QImage_height(Image) * QImage_depth(Image) + 7) div 8;
      if Count < Result then
        Result := Count;
      if Result > 0 then
        Move(Bits^, QImage_bits(Image)^, Result);
      QPixmap_convertFromImage(Bitmap, Image, QPixmapColorMode_Auto);
    finally
      QImage_destroy(Image);
    end;
  except
    Result := 0;
  end;
end;

function GetObject(Handle: QPixmapH; Size: Cardinal; Data: PtagBITMAP): Boolean;
begin
  Result := False;
  if (Handle <> nil) and (Size > 0) and (Data <> nil) then
  begin
    try
      Data.bmWidth := QPixmap_width(Handle);
      Data.bmHeight := QPixmap_height(Handle);
      Data.bmBitsPixel := QPixmap_depth(Handle);
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function GetObject(Handle: QPenH; Size: Cardinal; Data: PLogPen): Boolean;
begin
  Result := False;
  if (Handle <> nil) and (Size > 0) and (Data <> nil) then
  begin
    try
      Data.lopnStyle := Cardinal(QPen_style(Handle));
      Data.lopnWidth := Point(QPen_width(Handle), 0);
      Data.lopnColor := QColorColor(QPen_color(Handle));
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function SetPixel(Handle: QPainterH; X, Y: Integer; Color: TColor): TColorRef;
var
  Brush: QBrushH;
  OldRop: RasterOp;
  R: TRect;
begin
  R := Bounds(X, Y, 1, 1);
  Brush := CreateSolidBrush(Color);
  OldRop := QPainter_rasterOp(Handle);
  if OldRop <> RasterOp_CopyROP then
    QPainter_setRasterOp(Handle, RasterOp_CopyROP);
  FillRect(Handle, R, Brush);
  if OldRop <> RasterOp_CopyROP then
    QPainter_setRasterOp(Handle, OldRop);
  QBrush_destroy(Brush);
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
  try
    pdm := QPaintDeviceMetrics_create(QPainter_device(Handle));
    depth := QPaintDeviceMetrics_depth(pdm);
    QPaintDeviceMetrics_destroy(pdm);
    img := nil;
    tempdc := nil;
    pixmap := nil;
    try
      pixmap := QPixmap_create(2, 2, depth, QPixmapOptimization_NoOptim);
      tempDC := QPainter_create(pixmap);
      BitBlt(tempDC, 0, 0, 2, 2, Handle, X, Y, SRCCOPY);
      img := QImage_create;
      QPixmap_convertToImage(pixmap, img);
      Result := QImage_pixelIndex(img, 0, 0);
    finally
      if Assigned(img) then
        QImage_destroy(img);
      if Assigned(tempdc) then
        QPainter_destroy(tempdc);
      if Assigned(pixmap) then
        QPixmap_destroy(pixmap);
    end;
  except
    Result := 0;
  end;
end;

function DeleteObject(Handle: QPainterH): LongBool;
var
  Pixmap: QPaintDeviceH;
  P: PPainterInfo;
  IsCompatible: Boolean;
begin
  if Handle = nil then
    Result := False
  else
  try
    Pixmap := QPainter_device(Handle); // get paintdevice
    if QPainter_isActive(Handle) then
      QPainter_end(Handle);

    IsCompatible := GetPainterInfo(Handle, P) and (P.IsCompatibleDC);
    if P <> nil then
      DeletePainterInfo(Handle);
    QPainter_destroy(Handle);  // destroy painter
    if IsCompatible then
      QPixmap_destroy(QPixmapH(Pixmap)); // destroy pixmap paintdevice
    Result := True;
  except
    Result := False;
  end;
end;

function DeleteObject(Handle: QPixmapH): LongBool;
begin
  try
//    if not TStockObjectList.ReleaseStockObject(Handle) then
      QPixmap_destroy(Handle);
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

{ only negative and zero values of nSaveDC are supported }
function RestoreDC(Handle: QPainterH; nSavedDC: Integer): LongBool;
var
  i: Integer;
begin
  if nSavedDC < 0 then
  begin
    try
      for i:= nSavedDC - 1 to 0 do // nSavedDC
        QPainter_restore(Handle);
      Result := True;
    except
      Result := False;
    end;
  end
  else // limited implementation
    Result := True;
end;

function HWND_DESKTOP: QWidgetH;
begin
  Result := QApplication_desktop;
end;

function GetDesktopWindow: QWidgetH;
begin
  Result := HWND_DESKTOP;
end;

function GetActiveWindow: QWidgetH;
begin
  Result := QApplication_activeWindow(Application.Handle);
end;

function GetForegroundWindow: QWidgetH;
begin
  // is this correct ?
  Result := QApplication_focusWidget(Application.Handle);
end;

procedure SetActiveWindow(Handle: QWidgetH);
begin
  QWidget_setActiveWindow(Handle);
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
  if Flags and DT_BOTTOM <> 0 then
    Result := Result or AlignTop
  else if Flags and DT_VCENTER <> 0 then
    Result := Result or AlignVCenter
  else
    Result := Result or AlignTop;  // default
  // extended Qt alignments
  if Flags and DT_CALCRECT <> 0 then
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
  if Flags and DT_NOCLIP <> 0 then
    Result := Result or DontClip;
end;

{ strips Qt extended alignment from flags }
function QtStdAlign(Flags: Integer): Word;
begin
  Result := Word(Flags and QtAlignMask);
end;


function IsCharAlpha(Ch: Char): LongBool;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.IsCharAlpha(Ch);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := IsAlpha(cardinal(ch)) <> 0 ;
  {$ENDIF LINUX}
end;

function IsCharAlphaNumeric(Ch: Char): LongBool;
begin
  Result := (Ch in ['0'..'9']) or IsCharAlpha(Ch);
end;

{ IP Address edit control }

function MAKEIPRANGE(low, high: Byte): integer;
begin
  Result := high;
  Result := (Result shl 8) + low;
end;

function MAKEIPADDRESS(b1, b2, b3, b4: cardinal): integer;
begin
  Result := (b1 shl 24) + (b2 shl 16) + (b3 shl 8) + b4;
end;

function FIRST_IPADDRESS(x: cardinal): cardinal;
begin
  Result := (x shr 24) and $FF;
end;

function SECOND_IPADDRESS(x: cardinal): cardinal;
begin
  Result := (x shr 16) and $FF;
end;

function THIRD_IPADDRESS(x: cardinal): cardinal;
begin
  Result := (x shr 8) and $FF;
end;

function FOURTH_IPADDRESS(x: cardinal): cardinal;
begin
  Result := x and $FF;
end;

{$IFDEF LINUX}
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

function CopyFile(const Source, Destination: string; FailIfExists: Boolean): LongBool;
const
  ChunkSize = 8192;
var
  CopyBuffer: Pointer;
  Src, Dest: Integer;
  {FSize,} BytesCopied {, TotalCopied}: Longint;
  DestName: string;
begin
  Result := False;
  if DirectoryExists(Destination) then
    DestName := IncludeTrailingPathDelimiter(Destination) + ExtractFileName(Source)
  else
    DestName := Destination;
  if FailIfExists and FileExists(DestName) then
    Exit;
  Result := ForceDirectories(ExtractFilePath(Destination));
  if Result then
  begin
    GetMem(CopyBuffer, ChunkSize);
    try
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
            BytesCopied := FileRead(Src, CopyBuffer^, ChunkSize);
            if BytesCopied = -1 then
              raise EReadError.Create(SReadError);
            // TotalCopied := TotalCopied + BytesCopied;
            if BytesCopied > 0 then
            begin
              if FileWrite(Dest, CopyBuffer^, BytesCopied) = -1 then
                raise EWriteError.Create(SWriteError);
            end;
          until BytesCopied < ChunkSize;
          FileSetDate(DestName, FileGetDate(Src));
          Result := True;
        finally
          FileClose(Src);
        end;
      finally
        FileClose(Dest);
      end;
    finally
      FreeMem(CopyBuffer, ChunkSize);
    end;
  end;
end;

function CopyFile(lpExistingFileName, lpNewFileName: PChar;
  bFailIfExists: LongBool): LongBool;
var
  EF, NF: string;
begin
  EF := lpExistingFileName;
  NF := lpNewFilename;
  Result := CopyFile(EF, NF, Boolean(bFailIfExists));
end;

function CopyFileA(lpExistingFileName, lpNewFileName: PAnsiChar;
  bFailIfExists: LongBool): LongBool;
var
  EF, NF: string;
begin
  EF := lpExistingFileName;
  NF := lpNewFilename;
  Result := CopyFile(EF, NF, Boolean(bFailIfExists));
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

function GetComputerName(Buffer: PChar; var Size: Cardinal): LongBool;
var
  S: string;
begin
  Result := True;
  try
    SetLength(S, 255);
    if gethostname(PChar(S), Length(S)) <> -1 then
    begin
      SetLength(S, StrLen(PChar(S)));
      Size := Length(S) + 1;
      Result := S <> '';
      if Result and (Buffer <> nil) then
        StrLCopy(Buffer, PChar(S), Size - 1);
    end;
  except
    Result := False;
  end;
end;

function GetUserName(Buffer: PChar; var Size: Cardinal): LongBool;
var
  S: string;
  psswrd: PPasswordRecord;
begin
  Result := False;
  try
    psswrd :=  getpwuid(getuid); // static no need to free
    if psswrd <> nil then
    begin
      S := psswrd.pw_gecos; //  user's real name? or pwd.pw_name
      Size := Length(S) + 1;
      Result := S <> '';
      if Result and (Buffer <> nil) then
        StrLCopy(Buffer, PChar(S), Size - 1);
    end;
  except
    Result := False;
  end;
end;

function MakeIntResource(Value: Integer): PChar;
begin
  Result := PChar(Value and $0000ffff);
end;

function MakeWord(A, B: Byte): Word;
begin
  Result := A or B shl 8;
end;

function MakeLong(A, B: Word): Longint;
begin
  Result := A or B shl 16;
end;

function HiWord(L: DWORD): Word;
begin
  Result := L shr 16;
end;

function HiByte(W: Word): Byte;
begin
  Result := W shr 8;
end;

procedure MessageBeep(Value: Integer);
begin
  QApplication_beep;
end;

procedure OutputDebugString(OutputString: AnsiString);
begin
  WriteLn(ErrOutput, OutputString);
end;

procedure OutputDebugString(lpOutputString: PAnsiChar);
begin
  OutputDebugString(string(lpOutputString));
end;

function GetCurrentProcess: THandle;
begin
  Result := THandle(0);
end;

function CheckThreadError(ErrCode: Integer): Integer;
begin
  if ErrCode <> 0 then
    raise EThread.CreateResFmt(@SQThreadError, [SysErrorMessage(ErrCode), ErrCode]);
  Result := ErrCode;
end;

function TerminateThread(ThreadID: TThreadID; RetVal: Integer): LongBool;
begin
  case RetVal of
    0:
      Result := CheckThreadError(pthread_kill(ThreadID, SIGQUIT)) = 0;
    130:
      Result := CheckThreadError(pthread_kill(ThreadID, SIGABRT)) = 0; /// CTRL_C
  else
    Result := CheckThreadError(pthread_kill(ThreadID, SIGKILL))= 0; // unmaskable
  end;
end;

procedure SuspendThread(ThreadID: TThreadID);
begin
  CheckThreadError(pthread_kill(ThreadID, SIGSTOP));
end;

procedure ResumeThread(ThreadID: TThreadID);
begin
  CheckThreadError(pthread_kill(ThreadID, SIGCONT));
end;

function GetThreadPolicy(ThreadID: TThreadID): Integer;
var
  SP: TSchedParam;
begin
  CheckThreadError(pthread_getschedparam(ThreadID, Result, SP));
end;

procedure SetThreadPolicy(ThreadID: TThreadID; value: Integer);
var
  SP: TSchedParam;
begin
  if Value <> GetThreadPolicy(ThreadID) then
  begin
    SP.sched_priority := GetThreadPriority(ThreadID);
    CheckThreadError(pthread_setschedparam(ThreadID, Value, @SP));
  end;
end;

function GetThreadPriority(ThreadID: TThreadID): TThreadPriority;
var
  P: Integer;
  SP: TSchedParam;
begin
  if CheckThreadError(pthread_getschedparam(ThreadID, P, SP)) <> 0
  then
    Result := THREAD_PRIORITY_ERROR_RETURN
  else
    Result := SP.sched_priority;
end;

function SetThreadPriority(ThreadID: TThreadID; priority: Integer): LongBool; 	// handle to the thread
var
  SP: TSchedParam;
  P: Integer;
begin
  if priority <> GetThreadPriority(ThreadID) then
  begin
    SP.sched_priority := priority;
    P:= GetThreadPolicy(ThreadID);
    CheckThreadError(pthread_setschedparam(ThreadID, P, @SP));
    Result := errno = 0;
  end
  else
    Result := True;
end;

function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: Cardinal;
  lpflOldProtect: Pointer): LongBool; overload;
var
  AlignedAddress: Cardinal;
  PageSize, ProtectSize: Cardinal;
begin
  if lpflOldProtect <> nil then
  begin
    // (ahuser) I have not found a Libc function for that
    PCardinal(lpflOldProtect)^ := PAGE_EXECUTE_READWRITE;
  end;

  PageSize := Cardinal(Libc.getpagesize);
  AlignedAddress := Cardinal(lpAddress) and not (PageSize - 1); // start memory page
  // get the number of needed memory pages
  ProtectSize := PageSize;
  while Cardinal(lpAddress) + dwSize > AlignedAddress + ProtectSize do
    Inc(ProtectSize, PageSize);
  Result := mprotect(Pointer(AlignedAddress), ProtectSize, flNewProtect) = 0;
end;

function VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: Cardinal;
  var OldProtect: Cardinal): LongBool; overload;
begin
  Result := VirtualProtect(lpAddress, dwSize, flNewProtect, @OldProtect);
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: LongWord; var lpNumberOfBytesRead: Cardinal): LongBool;
var
  Prot: Cardinal;
begin
  Result := False;
  lpNumberOfBytesRead := 0;
  if (hProcess = GetCurrentProcess) and (lpBuffer <> nil) then
  begin
    if nSize = 0 then
      Result := True
    else
    if VirtualProtect(lpBaseAddress, nSize, PAGE_READWRITE, Prot) then
    begin
      try
        Move(lpBaseAddress^, lpBuffer^, nSize);
        lpNumberOfBytesRead := nSize;
        Result := True;
      except
        Result := False;
      end;
      VirtualProtect(lpBaseAddress, nSize, Prot, nil);
    end;
  end;
end;

function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer;
  lpBuffer: Pointer; nSize: LongWord; var lpNumberOfBytesWritten: Longword): LongBool;
var
  Prot: Cardinal;
begin
  Result := False;
  lpNumberOfBytesWritten := 0;
  if (hProcess = GetCurrentProcess) and (lpBuffer <> nil) then
  begin
    if nSize = 0 then
      Result := True
    else
    if VirtualProtect(lpBaseAddress, nSize, PAGE_READWRITE, Prot) then
    begin
      try
        Move(lpBuffer^, lpBaseAddress^, nSize);
        lpNumberOfBytesWritten := nSize;
        Result := True;
      except
        Result := False;
      end;
      VirtualProtect(lpBaseAddress, nSize, Prot, nil);
    end;
  end;
end;

procedure FlushInstructionCache(PID: cardinal; OrgCalProc: Pointer; size: Integer);
asm
        JMP     @@Exit
// 64 Bytes:
        DD      0, 0, 0, 0, 0, 0, 0, 0
        DD      0, 0, 0, 0, 0, 0, 0, 0
@@Exit:
end;

function GetKeyState(nVirtKey: Integer): SmallInt;
begin
  Result := 0;
  case nVirtKey of
    Key_Shift:
      if ssShift in Application.KeyState  then
        Result := -32768;  // = $8000
    Key_Control:
      if ssCtrl in Application.KeyState then
        Result := -32768;
    Key_Menu:
      if ssAlt in Application.KeyState then
        Result := -32768;
  end;
end;

function GetAsyncKeyState(vKey: Integer): SmallInt;
var
  Root: Window;
  Child: Window;
  RootX, RootY, WinX, WinY: Longint;
  Mask: Cardinal;
begin
  XQueryPointer(Application.Display,
    XRootWindow(Application.Display, XDefaultScreen(Application.Display)),
    @Root, @Child, @RootX, @RootY, @WinX, @WinY, @Mask);
  Result := 0;
  case vKey of
    VK_SHIFT:
      if Mask and ShiftMask <> 0 then
        Result := -32768;  // = $8000
    VK_CONTROL:
      if Mask and ControlMask <> 0 then
        Result := -32768;
    VK_MENU:
      if Mask and Mod1Mask <> 0 then
        Result := -32768;
  end;
end;


// Handle-Values and IPC
type
  THandleObjectList = class;

  THandleObject = class(TObject)
  private
    FRefCount: Integer;
    FName: string;
    FList: THandleObjectList;
  public
    constructor Create(AList: THandleObjectList; const AName: string);
    destructor Destroy; override;
    procedure AddRef;
    procedure Release;

    property Name: string read FName;
    property List: THandleObjectList read FList;
  end;

  TWaitObject = class(THandleObject)
    constructor Create(const AName: string);
    function WaitFor(Timeout: Longword): Cardinal; virtual; abstract;
  end;

  THandleObjectList = class(TObjectList)
  private
    FLockHandle: TSemaphore;
    function GetItems(Index: Integer): THandleObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
    function Find(const AName: string): THandleObject;
    property Items[Index: Integer]: THandleObject read GetItems;
  end;

  TSemaphoreWaitObject = class(TWaitObject)
  private
    FOwnSem: Boolean;
    FSemId: Integer;
  public
    constructor Create(InitialCount, Max: Integer; const AName: string; Open: Boolean);
    destructor Destroy; override;
    function WaitFor(Timeout: Longword): Cardinal; override;
    function ReleaseSemaphore(ReleaseCount: Integer; PreviousCount: PInteger): Boolean;
  end;

  TEventWaitObject = class(TWaitObject)
  private
    FManualReset: Boolean;
    FEvent: TEvent;
    FSignaled: Boolean;
  public
    constructor Create(EventAttributes: PSecurityAttributes;
      ManualReset, InitialState: LongBool; const AName: string);
    destructor Destroy; override;
    function WaitFor(Timeout: Longword): Cardinal; override;
    function SetEvent: Boolean;
    function ResetEvent: Boolean;
  end;

  TEventTimeoutThread = class(TThread)
  private
    FStopped: Boolean;
    FEvent: TEventWaitObject;
    FTimeout: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AEvent: TEventWaitObject; ATimeout: Integer);
    property Stopped: Boolean read FStopped write FStopped;
  end;

  TMutexWaitObject = class(TSemaphoreWaitObject)
  private
    FOwnerThreadId: Cardinal;
    FThreadLocks: Integer;
    FCritSect: TRTLCriticalSection;
  public
    constructor Create(const AName: string; Open: Boolean);
    destructor Destroy; override;
    function WaitFor(Timeout: Longword): Cardinal; override;
    function ReleaseMutex: Boolean;
  end;

var
  WaitObjectList: THandleObjectList;

{ THandleObject }

constructor THandleObject.Create(AList: THandleObjectList; const AName: string);
begin
  inherited Create;
  FList := AList;
  FName := AName;
  FRefCount := 0;
  if Assigned(FList) then
    FList.Add(Self);
  AddRef;
end;

destructor THandleObject.Destroy;
begin
  if Assigned(FList) then
    FList.Extract(Self);
  inherited Destroy;
end;

procedure THandleObject.AddRef;
begin
  Inc(FRefCount);
end;

procedure THandleObject.Release;
begin
  Dec(FRefCount);
  if FRefCount <= 0 then
    Free;
end;

{ TWaitObject }

constructor TWaitObject.Create(const AName: string);
begin
  inherited Create(WaitObjectList, AName);
end;

{ THandleObjectList }

constructor THandleObjectList.Create;
begin
  inherited Create;
  sem_init(FLockHandle, False, 1);
end;

destructor THandleObjectList.Destroy;
begin
  sem_destroy(FLockHandle);
  inherited Destroy;
end;

procedure THandleObjectList.Enter;
begin
  sem_wait(FLockHandle);
end;

procedure THandleObjectList.Leave;
begin
  sem_post(FLockHandle);
end;

function THandleObjectList.GetItems(Index: Integer): THandleObject;
begin
  Result := THandleObject(inherited Items[Index]);
end;

function THandleObjectList.Find(const AName: string): THandleObject;
var
  I: Integer;
begin
  if AName <> '' then
  begin
    for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if Result.Name = AName then
        Exit;
    end;
  end;
  Result := nil;
end;

{ TSemaphoreWaitObject }

// asn: documented, but (afaics) not in libc.so.6
//function semtimedop; external libcmodulename name 'semtimedop';

function semtimedop(semid: Integer; sops: PSemaphoreBuffer;
  nsops: size_t;timeout: PTimeSpec): Integer;
var
  sem: TSemaphoreBuffer;
  psem: PSemaphoreBuffer;
  i: Integer;
  WaitTicks, StartTicks: cardinal;
begin
  if timeout = nil then
    Result := semop(semid, sops, nsops)
  else
  begin
    Result := 0; // incase nsops = 0
    WaitTicks := 1000 * timeout.tv_sec  + timeout.tv_nsec div 1000000;
    psem := sops;
    StartTicks := GetTickCount ;
    try
      for i:= 0 to nsops-1 do
      begin
        sem := psem^ ;
        sem.sem_flg := sem.sem_flg OR IPC_NOWAIT;

        // process one sem
        while (GetTickCount - StartTicks) <= WaitTicks do
        begin
          Result := semop(semid, @sem, 1);
          if Result <> -1 then
          begin
            inc(psem);  // succes, next semaphore
            break;
          end
          else
          begin
            if (errno = EAGAIN) and ((psem^.sem_flg and IPC_NOWAIT) = 0)
            then
              Sleep(10) // try again
            else
              Exit;    // no wait allowed or other error
          end;
        end; // while

        if (Result = -1)  and (errno <> EAGAIN) then
          break;
      end;  // for i:= 0 to ..
    except
      Result := -1;
      // errno := EFAULT;
    end;
  end;
end;

function GetIPCKey(const AName: string; What: Integer): Integer;
var
  Filename: string;
begin
  if AName = '' then
    Result := IPC_PRIVATE
  else
  begin
    Filename := IpcDirectory + PathDelim + AName;
    ForceDirectories(IpcDirectory);
    if not FileExists(Filename) then
      FileClose(FileCreate(Filename));
    Result := ftok(PChar(Filename), What);
  end;
end;

type
  TSemUnion = record
    case Integer of
      0: (val: Integer);
      1: (buf: PSemaphoreIdDescriptor);
      2: (ary: PWord);
      3: (__buf: PSemaphoreInfo);
  end;

constructor TSemaphoreWaitObject.Create(InitialCount, Max: Integer; const AName: string;
  Open: Boolean);
const
  AccessMode = S_IREAD or S_IWRITE or S_IRGRP or S_IWGRP;
var
  Arg: TSemUnion;
  IPCKey: Integer;
begin
  inherited Create(AName);
  IPCKey := GetIPCKey(Name, 1);
  if not Open then
    FSemId := semget(IPCKey, 1, IPC_CREAT or IPC_EXCL or AccessMode)
  else
    FSemId := -1; // open

  if FSemId = -1 then
  begin
    // open sempahore
    FOwnSem := False;
    FSemId := semget(IPCKey, 0, SEM_UNDO);
    if FSemId = -1 then
      RaiseLastOSError;
  end
  else
  begin
    FOwnSem := True;
    Arg.val := Max - InitialCount;
    if semctl(FSemId, 0, SETVAL, Arg) = -1 then
      RaiseLastOSError;
  end;
end;

destructor TSemaphoreWaitObject.Destroy;
begin
  if FOwnSem then
  begin
    semctl(FSemId, 0, IPC_RMID);
    if Name <> '' then
      DeleteFile(IpcDirectory + PathDelim + Name); // only if allowed
  end;
  inherited Destroy;
end;

function TSemaphoreWaitObject.WaitFor(Timeout: Longword): Cardinal;
var
  Buf: TSemaphoreBuffer;
  RetValue: Integer;
  timespec: TTimeSpec;
begin
  Buf.sem_num := 0;
  Buf.sem_op := -1;
  Buf.sem_flg := SEM_UNDO;
  if Timeout = INFINITE then
    RetValue := semop(FSemId, @Buf, 1)
  else if Timeout = 0 then
  begin
    Buf.sem_flg := Buf.sem_flg or IPC_NOWAIT;
    RetValue := semop(FSemId, @Buf, 1);
  end
  else
  begin
    timespec.tv_sec := Timeout div 1000;
    timespec.tv_nsec := (Timeout mod 1000) * 1000000;
    RetValue := semtimedop(FSemId, @Buf, 1, @timespec); // Timeout=0 -> INFINTE
  end;

  if RetValue = -1 then
  begin
    if errno = EAGAIN then
      Result := WAIT_TIMEOUT
    else
      Result := WAIT_FAILED;
  end
  else
    Result := WAIT_OBJECT_0;
end;

function TSemaphoreWaitObject.ReleaseSemaphore(ReleaseCount: Integer; PreviousCount: PInteger): Boolean;
var
  Buf: TSemaphoreBuffer;
  Arg: TSemUnion;
begin
  Result := False;
  if ReleaseCount >= 0 then
  begin
    if PreviousCount <> nil then
    begin
      Result := semctl(FSemId, 0, GETVAL, @Arg) = 0;
      PreviousCount^ := Arg.val;
    end;
    if ReleaseCount > 0 then
    begin
      Buf.sem_num := 0;
      Buf.sem_op := ReleaseCount;
      Buf.sem_flg := SEM_UNDO;
      Result := semop(FSemId, @Buf, 1) = 0;
    end;
  end
end;

{ TEventTimeoutThread }

constructor TEventTimeoutThread.Create(AEvent: TEventWaitObject; ATimeout: Integer);
begin
  FEvent := AEvent;
  FTimeout := ATimeout;
  FStopped := False;
  inherited Create(False);
end;

procedure TEventTimeoutThread.Execute;
var
  StartTime, CurrentTime: Int64;
begin
  StartTime := GetTickCount;
  while not FStopped do
  begin
    CurrentTime := GetTickCount;
    if CurrentTime < StartTime then
      Inc(CurrentTime, $100000000);
    if CurrentTime - StartTime > FTimeout then
    begin
      FStopped := True;
      FEvent.SetEvent;
      Break;
    end;
    Sleep(10);
  end;
end;

{ TEventWaitObject }

type
  TPrivateEvent = class(TEvent)
  protected
    FEvent: TSemaphore;
    {...}
  end;

constructor TEventWaitObject.Create(EventAttributes: PSecurityAttributes;
  ManualReset, InitialState: LongBool; const AName: string);
begin
  inherited Create(AName);
  FManualReset := ManualReset;
  FEvent := TEvent.Create(EventAttributes,
    False, // ManualReset: handled by this class
    InitialState, AName);
  FSignaled := False;
end;

destructor TEventWaitObject.Destroy;
begin
  sem_destroy(TPrivateEvent(FEvent).FEvent);
  FEvent.Free;
  inherited Destroy;
end;

function TEventWaitObject.WaitFor(Timeout: Longword): Cardinal;
var
  TimeoutThread: TEventTimeoutThread;
begin
  Result := WAIT_FAILED;
  TimeoutThread := nil;
  if Timeout <> INFINITE then
    { POSIX semaphores do not support a timeout value. Here we use a
      second thread that produces a timeout by releasing the semaphore. }
    TimeoutThread := TEventTimeoutThread.Create(Self, Integer(Timeout));
  try
    case FEvent.WaitFor(Timeout) of
      wrSignaled:
        Result := WAIT_OBJECT_0;
      wrTimeout:
        Result := WAIT_TIMEOUT; // POSIX semaphores do not have a timeout
      wrAbandoned:
        Result := WAIT_ABANDONED; // for events ?
    end;
  finally
    if Assigned(TimeoutThread) then
    begin
      if TimeoutThread.Stopped then
        Result := WAIT_TIMEOUT;
      TimeoutThread.Stopped := True;
      TimeoutThread.WaitFor;
      TimeoutThread.Free;
    end;
    if FManualReset then
    begin
      FSignaled := True;
      SetEvent; // do not auto-reset
    end
    else
      FSignaled := False;
  end;
end;

function TEventWaitObject.SetEvent: Boolean;
begin
  FEvent.SetEvent;
  Result := True;
end;

function TEventWaitObject.ResetEvent: Boolean;
begin
  if FSignaled then
  begin
    FSignaled := False;
    WaitFor(INFINITE); // auto-reset
  end;
  FEvent.ResetEvent;
  Result := True;
end;

{ TMutexWaitObject }

constructor TMutexWaitObject.Create(const AName: string; Open: Boolean);
begin
  inherited Create(0, 1, AName, Open);
  FOwnerThreadId := 0;
  FThreadLocks := 0;
  InitializeCriticalSection(FCritSect);
end;

destructor TMutexWaitObject.Destroy;
begin
  if FOwnerThreadId <> 0 then
    ReleaseSemaphore(1, nil);
  DeleteCriticalSection(FCritSect);
  inherited Destroy;
end;

function TMutexWaitObject.WaitFor(Timeout: Longword): Cardinal;
var
  CurThreadId: Cardinal;
begin
  CurThreadId := GetCurrentThreadID;
  if CurThreadId = FOwnerThreadId then
  begin
    InterlockedIncrement(FThreadLocks);
    Result := WAIT_OBJECT_0;
  end
  else
  begin
    Result := inherited WaitFor(Timeout);
    if Result = WAIT_OBJECT_0 then
    begin
      EnterCriticalSection(FCritSect);
      try
        FOwnerThreadId := CurThreadId;
        InterlockedIncrement(FThreadLocks);
      finally
        LeaveCriticalSection(FCritSect);
      end;
    end;
  end;
end;

function TMutexWaitObject.ReleaseMutex: Boolean;
begin
  if GetCurrentThreadId = FOwnerThreadId then
  begin
    EnterCriticalSection(FCritSect);
    try
      InterlockedDecrement(FThreadLocks);
      if FThreadLocks <= 0 then
      begin
        Result := ReleaseSemaphore(1, nil);
        FOwnerThreadId := 0;
        FThreadLocks := 0;
      end
      else
        Result := True;
    finally
      LeaveCriticalSection(FCritSect);
    end;
  end
  else
    Result := False;
end;

// ======= IPC API functions =======

function CreateEvent(EventAttributes: PSecurityAttributes;
  ManualReset, InitialState: LongBool; Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if Result <> 0 then
    begin
      if THandleObject(Result) is TEventWaitObject then
        THandleObject(Result).AddRef
      else
        Result := 0;
    end
    else
      Result := THandle(TEventWaitObject.Create(EventAttributes, ManualReset,
        InitialState, Name));
  finally
    WaitObjectList.Leave;
  end;
end;

function OpenEvent(DesiredAccess: Longword; InheritHandle: LongBool;
  Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if (Result <> 0) and (THandleObject(Result) is TEventWaitObject) then
      THandleObject(Result).AddRef
    else
      Result := 0;
  finally
    WaitObjectList.Leave;
  end;
end;

function SetEvent(Event: THandle): LongBool;
begin
  WaitObjectList.Enter;
  try
    try
      Result := (Event <> 0) and (THandleObject(Event) is TEventWaitObject);
      if Result then
        TEventWaitObject(Event).SetEvent;
    except
      Result := False;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function ResetEvent(Event: THandle): LongBool;
begin
  WaitObjectList.Enter;
  try
    try
      Result := (Event <> 0) and (THandleObject(Event) is TEventWaitObject);
      if Result then
        TEventWaitObject(Event).ResetEvent;
    except
      Result := False;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function PulseEvent(Event: THandle): LongBool;
begin
 // not implemented
  Result := SetEvent(Event);
end;

function CreateMutex(MutexAttributes: PSecurityAttributes; InitialOwner: LongBool;
  Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if Result <> 0 then
    begin
      if THandleObject(Result) is TMutexWaitObject then
        THandleObject(Result).AddRef
      else
        Result := 0; // no mutex
    end
    else
    begin
      try
        Result := THandle(TMutexWaitObject.Create(Name, False));
      except
        Result := 0;
      end;
    end;
  finally
    WaitObjectList.Leave;
  end;

  if (Result <> 0) and InitialOwner then
    WaitForSingleObject(Result, INFINITE);
end;

function OpenMutex(DesiredAccess: Longword; InheritHandle: Boolean;
  Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if (Result <> 0) and (THandleObject(Result).ClassType = TMutexWaitObject) then
      THandleObject(Result).AddRef
    else
    begin
      try
        Result := THandle(TMutexWaitObject.Create(Name, True));
      except
        Result := 0;
      end;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function ReleaseMutex(Mutex: THandle): LongBool;
begin
  WaitObjectList.Enter;
  try
    try
      Result := (Mutex <> 0) and (THandleObject(Mutex).ClassType = TMutexWaitObject);
      if Result then
        TMutexWaitObject(Mutex).ReleaseMutex;
    except
      Result := False;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function CreateSemaphore(SemaphoreAttributes: PSecurityAttributes;
  InitialCount, MaximumCount: Longint; Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if Result <> 0 then
    begin
      if THandleObject(Result).ClassType = TSemaphoreWaitObject then
        THandleObject(Result).AddRef
      else
        Result := 0; // no semaphore
    end
    else
    begin
      if (InitialCount < 0) or (MaximumCount <= 0) or (InitialCount > MaximumCount) then
        Result := 0 // invalid
      else
      begin
        try
          Result := THandle(TSemaphoreWaitObject.Create(InitialCount, MaximumCount,
                                                        Name, False));
        except
          Result := 0;
        end;
      end;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function OpenSemaphore(DesiredAccess: Longword; InheritHandle: LongBool;
  Name: PChar): THandle;
begin
  WaitObjectList.Enter;
  try
    Result := THandle(WaitObjectList.Find(Name));
    if (Result <> 0) and (THandleObject(Result).ClassType = TSemaphoreWaitObject) then
      THandleObject(Result).AddRef
    else
    begin
      try
        Result := THandle(TSemaphoreWaitObject.Create(0, 0, Name, True));
      except
        Result := 0;
      end;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function ReleaseSemaphore(Semaphore: THandle; ReleaseCount: Longint;
  PreviousCount: PInteger): LongBool;
begin
  WaitObjectList.Enter;
  try
    try
      Result := (Semaphore <> 0) and (THandleObject(Semaphore).ClassType = TSemaphoreWaitObject);
      if Result then
        TSemaphoreWaitObject(Semaphore).ReleaseSemaphore(ReleaseCount, PreviousCount);
    except
      Result := False;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

// common handle functions

function WaitForSingleObject(Handle: THandle; Milliseconds: Cardinal): Cardinal;
begin
  Result := WAIT_FAILED;
  try
    if (Handle <> 0) and (THandleObject(Handle) is TWaitObject) then
    begin
      TWaitObject(Handle).AddRef;
      try
        Result := TWaitObject(Handle).WaitFor(Milliseconds);
      finally
        TWaitObject(Handle).Release;
      end;
    end;
  except
    Result := WAIT_FAILED;
  end;
end;

// The WaitForMultipleObjects function returns when one of the following occurs:
//
// ·	Either any one or all of the specified objects are in the signaled state.
// ·	The time-out interval elapses.
function WaitForMultipleObjects(Count: Cardinal; Handles: PWOHandleArray;
  WaitAll: LongBool; Milliseconds: Cardinal): Cardinal;
var
  i: Integer;
  startticks: int64;
  ticks: Integer;
begin
  startticks := GetTickCount;
  Result := WAIT_OBJECT_0;
  if WaitAll then
  begin
    for i := 0 to Count - 1  do
    begin
      ticks :=  Milliseconds - (GetTickCount - StartTicks);
      if ticks < 0 then
      begin
        Result := WAIT_TIMEOUT;
        Exit;
      end
      else
      begin
        Result := WaitForSingleObject(Handles[i], Ticks);
        if Result <> WAIT_OBJECT_0 then
        begin
          Inc(Result, i);
          Exit;
        end;
      end;
    end;
  end
  else
  begin  //
    while True do
    begin
      for i := 0 to Count-1 do
      begin
        Result := WaitForSingleObject(Handles[i], 0);
        case Result of
          WAIT_FAILED, WAIT_ABANDONED, WAIT_OBJECT_0:
            begin
              Inc(Result, i);
              Exit;
            end;
        else
          if (startticks - GetTickCount) > MilliSeconds then
          begin
            Result := WAIT_TIMEOUT;
            Exit;
          end;
        end;
      end;
      Sleep(5);
    end;
  end;
end;

// all Handles are THandleObject derived classes
function CloseHandle(hObject: THandle): LongBool;
begin
  WaitObjectList.Enter;
  try
    try
      if (hObject <> 0) then
        THandleObject(hObject).Release;
      Result := True;
    except
      Result := False;
    end;
  finally
    WaitObjectList.Leave;
  end;
end;

function GlobalAllocPtr(Flags: Integer; Bytes: Longint): Pointer;
begin
  Result := GlobalLock(GlobalAlloc(Flags, Bytes));
end;

function GlobalReAllocPtr(P: Pointer; Bytes: Longint; Flags: Integer): Pointer;
var
  hMem: Cardinal;
begin
  hMem := GlobalHandle(P);
  GlobalUnlock(hMem);
  Result := GlobalLock(GlobalReAlloc(hMem, Bytes, Flags));
end;

function GlobalFreePtr(P: Pointer): THandle;
var
  hMem: Cardinal;
begin
  hMem := GlobalHandle(P);
  GlobalUnlock(hMem);
  Result := GlobalFree(hMem);
end;

type
  PGlobalBlock = ^TGlobalBlock;
  TGlobalBlock = packed record
    Start: Pointer;
    Size: Longword;
  end;

function GlobalAlloc(uFlags: Cardinal; dwBytes: Longword): Cardinal;
var
  Info: PGlobalBlock;
  Start, P: PByte;
begin
  Result := 0;
  if dwBytes > 0 then
  begin
    GetMem(P, SizeOf(TGlobalBlock) + dwBytes + 16);
    Start := P;
    Inc(P, SizeOf(TGlobalBlock) + $0F);
    P := Pointer(Cardinal(P) and not $0F);
    Info := Pointer(Cardinal(P) - SizeOf(TGlobalBlock));
    Info^.Start := Start;
    Info^.Size := dwBytes;

    if uFlags and GMEM_ZEROINIT <> 0 then
      FillChar(P^, dwBytes, 0);

    Result := Cardinal(P);
  end;
end;

function GlobalReAlloc(hMem: Cardinal; dwBytes: Longword; uFlags: Cardinal): Cardinal;
var
  CurSize: Longword;
  Offset: Cardinal;
  Start: Pointer;
  Info: PGlobalBlock;
  P: PChar;
begin
  if dwBytes = 0 then
  begin
    GlobalFree(hMem);
    Result := 0;
    Exit;
  end;
  if hMem = 0 then
    Result := GlobalAlloc(uFlags, dwBytes)
  else
  begin
    CurSize := GlobalSize(hMem);
    Start := PGlobalBlock(hMem - SizeOf(TGlobalBlock))^.Start;
    Offset := hMem - Cardinal(Start);
    ReallocMem(Start, SizeOf(TGlobalBlock) + dwBytes + 16);
    hMem := Cardinal(Start) + Offset;
    P := Pointer(hMem);

    if hMem and $0F <> 0 then
    begin
      P := Start;
      Inc(P, SizeOf(TGlobalBlock) + $0F);
      P := Pointer(Cardinal(P) and not $0F);
      Info := Pointer(Cardinal(P) - SizeOf(TGlobalBlock));
      Move(Pointer(hMem)^, P^, CurSize); // move data
      hMem := Cardinal(P);
      Info^.Start := Start;
      Info^.Size := dwBytes;
    end;

    PGlobalBlock(hMem - SizeOf(TGlobalBlock))^.Size := dwBytes; 
    if uFlags and GMEM_ZEROINIT <> 0 then
      if CurSize < dwBytes then
        FillChar(P[CurSize], dwBytes - CurSize, 0);
    Result := hMem;
  end;
end;

function GlobalSize(hMem: Cardinal): Longword;
begin
  if hMem > 0 then
    Result := PGlobalBlock(hMem - SizeOf(TGlobalBlock))^.Size
  else
    Result := 0;
end;

function GlobalLock(hMem: Cardinal): Pointer;
begin
  Result := Pointer(hMem);
end;

function GlobalHandle(Mem: Pointer): Cardinal;
begin
  Result := Cardinal(Mem);
end;

function GlobalUnlock(hMem: Cardinal): LongBool;
begin
  Result := hMem <> 0;
end;

function GlobalFree(hMem: Cardinal): Cardinal;
begin
  if hMem <> 0 then
  begin
    FreeMem(PGlobalBlock(hMem - SizeOf(TGlobalBlock))^.Start);
    Result := 0;
  end
  else
    Result := GMEM_INVALID_HANDLE;
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
    (TimeVal.tv_usec - StartTimeVal.tv_usec) div 1000;
end;

procedure InitGetTickCount;
begin
  gettimeofday(StartTimeVal, nil);
end;

function QueryPerformanceCounter(var PerformanceCount: int64): LongBool;
var
  TimeVal: TTimeVal;
begin
  gettimeofday(TimeVal, nil);
  PerformanceCount := CLOCKS_PER_SEC * TimeVal.tv_sec + Timeval.tv_usec;
  Result := true;
end;

function QueryPerformanceFrequency(var Frequency: int64): LongBool;
begin
  Frequency := CLOCKS_PER_SEC; // 1 Mhz resolution gettimeofday
  Result := True;
end;

procedure GetLocalTime(var st: TSystemTime);
var
  dt: TDateTime;
begin
  dt := Now;
  DecodeDateTime(dt, st.wYear, st.wMonth, st.wDay, st.wHour, st.wMinute,
    st.wSecond, st.wMilliseconds);
  st.wDayOfWeek := DayOfTheWeek(dt);
end;

// Provider helpers

function Succeeded(Res: HResult): Boolean;
begin
  Result := Res and $80000000 = 0;
end;

function Failed(Res: HResult): Boolean;
begin
  Result := Res and $80000000 <> 0;
end;

function ResultCode(Res: HResult): Integer;
begin
  Result := Res and $0000FFFF;
end;

function CoCreateGUID(out Guid: TGUID): HResult;
begin
  Result := CreateGuid(Guid);
end;

{$ENDIF LINUX}

// for ShellExecute(0, ..
function ShellExecute(Handle: Integer; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): THandle;
begin
  Result := ShellExecute(QWidgetH(Handle), Operation, FileName,
                         Parameters, Directory, ShowCmd);
end;

function ShellExecute(Handle: QWidgetH; Operation, FileName, Parameters,
  Directory: PChar; ShowCmd: Integer): THandle;
var
  Name: string;
  Dir: string;
  Par: string;
begin
  if Directory <> nil then
    Dir := Directory;
  if Parameters <> nil then
    Par := Parameters;
  if Filename <> nil then
    Name := FileName;
  Result := ShellExecute(Handle, Operation, Name, Par, Dir, ShowCmd);
end;

function ShellExecute(Handle: QWidgetH; const Operation, FileName, Parameters,
  Directory: string; ShowCmd: Integer): THandle;
var
  {$IFDEF MSWINDOWS}
  WinId: Integer;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Line: string;
  {$ENDIF LINUX}
begin
  {$IFDEF MSWINDOWS}
  if Handle = nil then
    WinId := 0
  else
    WinId := QWidget_winID(Handle);
  Result := ShellAPI.ShellExecute(WinId, PChar(Operation),
                         PChar(FileName), PChar(Parameters),
                         PChar(Directory), ShowCmd);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if (Operation = 'open') or (Operation = '') then
    Line := Format('%s "%s" %s',[Shell, Filename, Parameters])
  else
  if Operation = 'browse' then
    Line := Format('%s "%s" %s',
      [GetEnvironmentVariable('BROWSER'), Filename, Parameters])
  else
  begin
    Result := THandle(HINSTANCE_ERROR);
    Exit;
  end;
  Line := Trim(Line)+ '&';
  if Directory <> '' then
    Line := Format('cd "%s";', [Directory]) + Line;
  if Libc.system(PChar(Line)) <> -1 then
    Result := THandle(HINSTANCE_OK)
  else
    Result := THandle(HINSTANCE_ERROR)
  {$ENDIF LINUX}
end;

function InterlockedIncrement(var I: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.InterlockedIncrement(I);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := SysUtils.InterlockedIncrement(I);
  {$ENDIF LINUX}
end;

function InterlockedDecrement(var I: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.InterlockedDecrement(I);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := SysUtils.InterlockedDecrement(I);
  {$ENDIF LINUX}
end;

function InterlockedExchange(var A: Integer; B: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.InterlockedExchange(A, B);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := SysUtils.InterlockedExchange(A, B);
  {$ENDIF LINUX}
end;

function InterlockedExchangeAdd(var A: Integer; B: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := Windows.InterlockedExchangeAdd(A, B);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := SysUtils.InterlockedExchangeAdd(A, B);
  {$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
{ wrappers to windows}

function CopyFile(lpExistingFileName, lpNewFileName: PChar;
  bFailIfExists: LongBool): LongBool;
begin
  Result := Windows.CopyFile(lpExistingFileName, lpNewFileName, bFailIfExists);
end;

function CopyFileA(lpExistingFileName, lpNewFileName: PAnsiChar;
  bFailIfExists: LongBool): LongBool;
begin
  Result := Windows.CopyFileA(lpExistingFileName, lpNewFileName, bFailIfExists);
end;

function CopyFileW(lpExistingFileName, lpNewFileName: PWideChar;
  bFailIfExists: LongBool): LongBool;
begin
  Result := Windows.CopyFileW(lpExistingFileName, lpNewFileName, bFailIfExists);
end;

function GetUserName(Buffer: PChar; var Size: Cardinal): LongBool;
begin
  Result := Windows.GetUserName(Buffer,Size);
end;

function GetComputerName(Buffer: PChar; var Size: Cardinal): LongBool;
begin
  Result := Windows.GetComputerName(Buffer,Size);
end;

function GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;

function QueryPerformanceCounter(var PerformanceCount: int64): LongBool;
begin
  Result := Windows.QueryPerformanceCounter(PerformanceCount);
end;

function QueryPerformanceFrequency(var Frequency: int64): LongBool;
begin
  Result := Windows.QueryPerformanceFrequency(Frequency);
end;

function SetEvent(Event: THandle): LongBool;
begin
  Result := Windows.SetEvent(Event);
end;

function ResetEvent(Event: THandle): LongBool;
begin
  Result := Windows.ResetEvent(Event);
end;

function PulseEvent(Event: THandle): LongBool;
begin
  Result := Windows.PulseEvent(Event);
end;

procedure OutputDebugString(OutputString: AnsiString);
begin
  Windows.OutputDebugString(PAnsiChar(OutputString));
end;

procedure OutputDebugString(lpOutputString: PAnsiChar);
begin
  Windows.OutputDebugString(lpOutputString);
end;

function GetKeyState(nVirtKey: Integer): SmallInt;
begin
  Result := Windows.GetKeyState(nVirtKey);
end;

//
// Taken from QDialogs.
//
type
  PTaskWindow = ^TTaskWindow;
  TTaskWindow = record
    Next: PTaskWindow;
    Window: Windows.HWnd;
  end;

var
  TaskActiveWindow: Windows.HWnd = 0;
  TaskFirstWindow: Windows.HWnd = 0;
  TaskFirstTopMost: Windows.HWnd = 0;
  TaskWindowList: PTaskWindow = nil;

function DoDisableWindow(Window: Windows.HWnd; Data: Longint): Bool; stdcall;
var
  P: PTaskWindow;
begin
  if (Window <> TaskActiveWindow) and Windows.IsWindowVisible(Window) and
    Windows.IsWindowEnabled(Window) then
  begin
    New(P);
    P^.Next := TaskWindowList;
    P^.Window := Window;
    TaskWindowList := P;
    Windows.EnableWindow(Window, False);
  end;
  Result := True;
end;

procedure EnableTaskWindows(WindowList: Pointer);
var
  P: PTaskWindow;
begin
  while WindowList <> nil do
  begin
    P := WindowList;
    if Windows.IsWindow(P^.Window) then Windows.EnableWindow(P^.Window, True);
    WindowList := P^.Next;
    Dispose(P);
  end;
end;

function DisableTaskWindows(ActiveWindow: Windows.HWnd): Pointer;
var
  SaveActiveWindow: Windows.HWND;
  SaveWindowList: Pointer;
begin
  Result := nil;
  SaveActiveWindow := TaskActiveWindow;
  SaveWindowList := TaskWindowList;
  TaskActiveWindow := ActiveWindow;
  TaskWindowList := nil;
  try
    try
      EnumThreadWindows(GetCurrentThreadID, @DoDisableWindow, 0);
      Result := TaskWindowList;
    except
      EnableTaskWindows(TaskWindowList);
      raise;
    end;
  finally
    TaskWindowList := SaveWindowList;
    TaskActiveWindow := SaveActiveWindow;
  end;
end;
{$ENDIF MSWINDOWS}

function IgnoreMouseEvents(Handle: QObjectH; Event: QEventH): boolean;
begin
  case QEvent_type(Event) of
    QEventType_MouseButtonPress,
    QEventType_MouseButtonRelease,
    QEventType_MouseButtonDblClick,
    QEventType_MouseMove,
    QEventType_Enter,
    QEventType_Leave,
    QEventType_Wheel:
      Result := true;
  else
    Result := false;
  end;
end;

procedure SetCursorPos(X, Y: integer);
var
  Value: TPoint;
begin
  Value.X := X;
  Value.Y := Y;
  QCursor_setPos(@Value);
end;

{ ------------ Caret -------------- }
type
  TEmulatedCaret = class(TComponent)
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
    constructor Create(AOwner: TComponent); override;
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
  GlobalCaret: TEmulatedCaret = nil ;

procedure GlobalCaretNeeded;
begin
  if GlobalCaret = nil then
    GlobalCaret := TEmulatedCaret.Create(nil);
end;

function CreateCaret(Widget: QWidgetH; Pixmap: QPixmapH; Width, Height: Integer): Boolean;
begin
  GlobalCaretNeeded;
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
  Result := QApplication_cursorFlashTime;
end;

function SetCaretBlinkTime(uMSeconds: Cardinal): LongBool;
begin
  Result := True;
  try
    QApplication_setCursorFlashTime(uMSeconds);
    if assigned(GlobalCaret) then
    begin
      GlobalCaret.Lock;
      try
        GlobalCaret.Timer.Interval := GetCaretBlinkTime;
      finally
        GlobalCaret.Unlock;
      end;
    end;
  except
    Result := False;
  end;
end;

function HideCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaretNeeded;
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.Hide;
    finally
      GlobalCaret.Unlock;
    end;
  end
  else
    Result := false;
end;

function ShowCaret(Widget: QWidgetH): Boolean;
begin
  GlobalCaretNeeded;
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
  GlobalCaretNeeded;
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
  GlobalCaretNeeded;
  GlobalCaret.Lock;
  try
    Pt := GlobalCaret.Pos;
  finally
    GlobalCaret.Unlock;
  end;
end;

function DestroyCaret: Boolean;
begin
  if Assigned(GlobalCaret) then
  begin
    GlobalCaret.Lock;
    try
      Result := GlobalCaret.DestroyCaret;
    finally
      GlobalCaret.Unlock;
    end;
  end
  else
    Result := False;
end;

{ TEmulatedCaret }

constructor TEmulatedCaret.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(FCritSect);

  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := GetCaretBlinkTime;
  FTimer.OnTimer := DoTimer;
end;

destructor TEmulatedCaret.Destroy;
begin
  DestroyCaret;
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

    Qt.bitBlt(DestDev, @FPos, FPixmap, @R, RasterOp_CopyROP);
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
      0: QC := QColor(clBlack);
      1: QC := QColor(clGray);
    else
      Result := nil;
      Exit;
    end;
    try
      Result := QPixmap_create(FWidth, FHeight, -1, QPixmapOptimization_MemoryOptim);
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


type
  TCriticalSections = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TCriticalSections.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(StockObjectListCritSect);
end;

destructor TCriticalSections.Destroy;
begin
  DeleteCriticalSection(StockObjectListCritSect);
  inherited Destroy;
end;

function Perform(Control: TControl; Msg: Cardinal; WPar, LPar: Longint): Longint;
var
  Mesg: TMessage;
begin
  Mesg.Msg := Msg;
  Mesg.WParam := WPar;
  Mesg.LParam := LPar;
  Mesg.Result := 0;
  Control.Dispatch(Mesg);
  Result := Mesg.Result;
end;

function SendMessage(Receiver: QWidgetH; MsgId: Integer; WPar, LPar: Longint): Integer;
var
  Event: QCustomEventH;
  Mesg: TMessage;
begin
  AppEventHookNeeded;
  with Mesg do
  begin
    Msg := MsgId;
    wParam := WPar;
    lParam := LPar;
    Result := 0;
  end;
  Event := QCustomEvent_create(QEventType_Message, @Mesg);
  QApplication_sendEvent(Receiver, Event);
  Result := Mesg.Result;
  QEvent_destroy(Event);
end;

function SendMessage(AControl: TWidgetControl; MsgId: Integer; WPar, LPar: Longint): Integer;
begin
//  OutputDebugString(Pchar(Format('%s: %s Sended message %x', [AControl.Name, AControl.ClassName, MsgId ])));
  Result := SendMessage(AControl.Handle, MsgId, WPar, LPar);
end;

type
  TCustomMsg = packed record
    Msg: integer;
    WParam: integer;
    LParam: integer;
    Result: integer;
    Pt: TPoint;
  end;

function PostMessage(Receiver: QWidgetH; MsgId: Integer; WPar, LPar: Longint): LongBool;
var
  Mesg: PMessage;
  Event: QCustomEventH;
begin
  AppEventHookNeeded;
  New(Mesg);
  with Mesg^ do
  begin
    Msg := MsgId;
    WParam := WPar;
    LParam := LPar;
    Result := 0; //GetTickCount;
  end;
  Event := QCustomEvent_Create(QEventType_Message, Mesg);
  try
    QApplication_postEvent(Receiver, QEventH(Event));
    Result := true;
  except
    QCustomEvent_Destroy(Event);
    Result := false;
  end;
end;

function PostMessage(AControl: TWidgetControl; MsgId: Integer; WPar, LPar: Longint): LongBool;
begin
//  OutputDebugString(Pchar(Format('%s: %s Posted message %x', [AControl.Name, AControl.ClassName, MsgId ])));
  Result := PostMessage(AControl.Handle, MsgId, WPar, LPar);
end;

{
  implements
  - WMTimer dispatch
  - Handles messages posted through QCustomEvent_message
}

type
  TWinTimer = class(TComponent)
  private
    FWMTimer: Cardinal;
    FQtTimer: Cardinal;
    FTimerProc: TTimerProc;
  public
    constructor Create(AOwner: TComponent); override;
    class function CreateTimer(AOwner: TWinControl; WMTimerId: Cardinal;
      Elapse: Cardinal; Proc: TTimerProc): TWinTimer;
    destructor Destroy; override;
    property WMTimer: Cardinal read FWMTimer;
    property QtTimer: Cardinal read FQtTimer;
    property TimerProc: TTimerProc read FTimerProc write FTimerProc;
  end;

constructor TWinTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWMTimer := 0;
  FQtTimer := 0;
  FTimerProc := nil;
end;

class function TWinTimer.CreateTimer(AOwner: TWinControl; WMTimerId: Cardinal;
      Elapse: Cardinal; Proc: TTimerProc): TWinTimer;
begin
  AppEventHookNeeded;
  Result := Create(AOwner);
  with Result do
  begin
    FWMTimer := WMTimerId;
    FTimerProc := Proc;
    FQtTimer := QObject_startTimer(AOwner.Handle, Elapse);
  end;
end;

destructor TWinTimer.Destroy;
begin
  QObject_killTimer(TWinControl(Owner).Handle, FQtTimer);
  inherited Destroy;
end;


function FindTimer(Receiver: QWidgetH; QtTimerID: Cardinal): TWinTimer;
var
  I: Integer;
  Instance: TWinControl;
begin
  Result := nil;
  Instance := FindControl(Receiver);
  if Assigned(Instance) then
    with Instance do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinTimer) and
           (TWinTimer(Components[I]).QtTimer = QtTimerID) then
        begin
          Result := TWinTimer(Components[I]);
          Exit;
        end;
end;

function FindWMTimer(Receiver: QWidgetH; WinTimerID: Longword): TWinTimer;
var
  I: Integer;
  Instance: TComponent;
begin
  Result := nil;
  Instance := FindControl(Receiver);
  if Assigned(Instance) then
    with Instance do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinTimer) and
           (TWinTimer(Components[I]).WMTimer = WinTimerID) then
        begin
          Result := TWinTimer(Components[I]);
          Exit;
        end;
end;

function SetTimer(Instance: TWidgetControl; WMTimerID, Elapse: Cardinal;
  TimerFunc: TTimerProc): Cardinal;
var
  FWinTimer: TWinTimer;
begin
  if Assigned(AppEventHook) then
  begin
    FWinTimer := FindWMTimer(Instance.Handle, WMTimerId);
    if Assigned(FWinTimer) then
      FWinTimer.Destroy;
  end
  else
    AppEventHookNeeded;

  FWinTimer := TWinTimer.CreateTimer(Instance, WMTimerID, Elapse, TimerFunc);
  Result := FWinTimer.WMTimer;
end;

function SetTimer(Wnd: QWidgetH; WMTimerID, Elapse: Cardinal;
  TimerFunc: TTimerProc): Cardinal;
var
  Instance: TWidgetControl;
begin
  Instance := FindControl(Wnd);
  if assigned(Instance) then
    Result := SetTimer(Instance, WMTimerID, Elapse, TimerFunc)
  else
    Result := 0;
end;

function KillTimer(Instance: TWidgetControl; WMTimerId: Cardinal): LongBool;
begin
  Result := KillTimer(Instance.Handle, WMTimerId);
end;

function KillTimer(Wnd: QWidgetH; WMTimerId: Cardinal): LongBool;
var
  WinTimer: TWinTimer;
begin
  WinTimer := FindWMTimer(Wnd, WMTimerId);
  if Assigned(WinTimer) then
  begin
    WinTimer.Destroy;
    Result := true;
  end
  else
    Result := false;
end;

function TAppEventHook.EventFilter(Receiver: QObjectH; Event: QEventH): Boolean;
var
  WinTimer: TWinTimer;
  Mesg: TMessage;
  Id: Integer;
  Instance: TWidgetControl;
begin
  Result := false;
  case QEvent_Type(Event) of

  QEventType_Message:
    begin
      Instance := FindControl(QWidgetH(Receiver));
      if Assigned(Instance)  and not (csDestroying in Instance.ComponentState) then
      begin
        Mesg := TMessage(QCustomEvent_data(QCustomEventH(Event))^);
        Mesg.Result := 0;
        Instance.Dispatch(Mesg);
      end;
      Result := True;
    end;

  QEventType_Timer:
    begin
      if not QObject_isWidgetType(Receiver) then
        Exit;
      Id := QTimerEvent_timerId(QTimerEventH(Event));
      WinTimer := FindTimer(QWidgetH(Receiver), Id );
      if Assigned(WinTimer) then
        with WinTimer do
        begin
          if Assigned(TimerProc) then
            TimerProc(QWidgetH(Receiver), WM_TIMER, WMTimer, GetTickCount);
          with Mesg do
          begin
            Mesg.Msg := WM_TIMER;
            Mesg.WParam := WMTimer;
            Mesg.LParam := Integer(@TimerProc);
            Mesg.Result := 0;
            Owner.Dispatch(Mesg);
          end;
          Result := True;
        end;
    end;

  end;       // case EventType of
end;

function InstallApplicationEventHook(EventFilter: TEventFilterMethod): QApplication_hookH;
var
  Method: TMethod;
begin
  Result := QApplication_hook_create(Application.Handle);
  TEventFilterMethod(Method) := EventFilter;
  Qt_hook_hook_events(Result, Method);
end;

constructor TAppEventHook.Create(AOwner: TComponent);
var
  Method: TMethod;
begin
  inherited Create(AOwner);
  FHook := QApplication_hook_create(Application.Handle);
  TEventFilterMethod(Method) := EventFilter;
  Qt_hook_hook_events(FHook, Method);
end;

destructor TAppEventHook.Destroy;
begin
  if Assigned(FHook) then
  begin
    QApplication_hook_destroy(FHook);
  end;
  inherited Destroy;
end;

initialization
  OutputDebugString('Loading QWindows.pas');
  {$IFDEF LINUX}
  InitGetTickCount;
  WaitObjectList := THandleObjectList.Create;
  {$ENDIF LINUX}
//  TCriticalSections.Create(Application);

finalization
  {$IFDEF LINUX}
  WaitObjectList.Free;
  {$ENDIF LINUX}
  AppEventHook.Free;
  GlobalCaret.Free;
  OutputDebugString('Unloaded QWindows.pas');
end.














