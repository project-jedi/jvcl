{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http:{www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QMessages.pas, released on 2004-11-05

The Initial Developer of the Original Code is André Snepvangers [ASnepvangers att xs4all dot nl]
Portions created by André Snepvangers are Copyright (C) 2004 André Snepvangers.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http:{jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
{-------------------------------------------------------------------------------
 QMessages.pas

 Copyright (c) 2004  André Snepvangers (asn att xs4all dott nl)
 All rights reserved.

 Description: Windows Messages ID's for
 Purpose: Reduce coding effort for porting VCL based components to VisualCLX
          compatible components

 Copyright (c) 2004 Andre Snepvangers (asn att xs4all dott nl),

 All rights reserved.

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

{-----------------------------------------------------------------------------}
// $Id$

unit QMessages;

interface

uses
  Classes, Types, Qt, QControls, QWindows;

const
  WM_SETFOCUS         = $0007;
  WM_KILLFOCUS        = $0008;
  WM_GETTEXTLENGTH    = $000E;
  WM_ERASEBKGND       = $0014;  { 20 }
  WM_NCPAINT          = $0085;  { 133 }
  WM_GETDLGCODE       = $0087;

  EM_GETRECT          = $00B2;  { 178 }  // TODO
  EM_SETRECT          = $00B3;  { 179 }  // TODO
  EM_UNDO             = $00C7;  { 199 }  // routed through WM_UNDO

  WM_TIMER            = $0113;  { 275 }   // implemented in QWindows
  WM_HSCROLL          = $0114;  { 276 }   // TODO
  WM_VSCROLL          = $0115;  { 277 }   // TODO
  WM_CUT              = $0300;  { 768 }
  WM_COPY             = $0301;  { 769 }
  WM_PASTE            = $0302;  { 770 }
  WM_CLEAR            = $0303;  { 771 }
  WM_UNDO             = $0304;  { 772 }
  WM_USER             = $0400;  { 1024 }

  { WM_GETDLGCODE return codes}
  DLGC_WANTARROWS   = 1;     { Control wants arrow keys         }
  DLGC_WANTTAB      = 2;     { Control wants tab keys           }
  DLGC_WANTALLKEYS  = 4;     { Control wants all keys           }
  DLGC_HASSETSEL    = 8;     { Understands EM_SETSEL message    }
  DLGC_WANTCHARS    = $80;   { Want WM_CHAR messages            }
  DLGC_BUTTON       = $2000; { Button item: can be checked      }

  { WM_SIZE message wParam values }
  SIZE_RESTORED  = 0; // The window has been resized, but neither the SIZE_MINIMIZED nor SIZE_MAXIMIZED value applies
  SIZE_MINIMIZED = 1; // The window has been minimized.
  SIZE_MAXIMIZED = 2; // The window has been maximized.
  SIZE_MAXSHOW   = 3; // Message is sent to all pop-up windows when some other window has been restored to its former size.
  SIZE_MAXHIDE   = 4; // Message is sent to all pop-up windows when some other window is maximized.

//
//  VCL control message IDs
//
  CM_VCLBASE                = CM_BASE + 10;
  CM_ACTIVATE               = CM_VCLBASE + 0; // not connected
  CM_DEACTIVATE             = CM_VCLBASE + 1; // not connected
  CM_DIALOGKEY              = CM_VCLBASE + 5;
  CM_DIALOGCHAR             = CM_VCLBASE + 6;
  CM_FOCUSCHANGED           = CM_VCLBASE + 7;
//  CM_PARENTFONTCHANGED      = CM_VCLBASE + 8; native VisualCLX message
//  CM_PARENTCOLORCHANGED     = CM_VCLBASE + 9; native VisualCLX message
  CM_HITTEST               = CM_VCLBASE + 10;
  CM_VISIBLECHANGED        = CM_VCLBASE + 11;
  CM_ENABLEDCHANGED        = CM_VCLBASE + 12;
  CM_COLORCHANGED          = CM_VCLBASE + 13;
  CM_FONTCHANGED           = CM_VCLBASE + 14;
  CM_CURSORCHANGED         = CM_VCLBASE + 15;
  CM_TEXTCHANGED           = CM_VCLBASE + 18;
  CM_MOUSEENTER            = CM_VCLBASE + 19;
  CM_MOUSELEAVE            = CM_VCLBASE + 20;
//  CM_BUTTONPRESSED         = CM_VCLBASE + 24;  native VisualCLX message
  CM_SHOWINGCHANGED        = CM_VCLBASE + 25;
  CM_ENTER                 = CM_VCLBASE + 26;
  CM_EXIT                  = CM_VCLBASE + 27;
  CM_DESIGNHITTEST         = CM_VCLBASE + 28;
  CM_SHOWHINTCHANGED       = CM_VCLBASE + 34;
  CM_SYSCOLORCHANGE        = CM_VCLBASE + 36;  // -> application palette changed
  CM_CONTROLLISTCHANGE     = CM_VCLBASE + 44;
  CM_GETDATALINK           = CM_VCLBASE + 45;
  CM_HINTSHOW              = CM_VCLBASE + 48;
  CM_RECREATEWND           = CM_RECREATEWINDOW; // native clx message
  CM_SYSFONTCHANGED        = CM_VCLBASE + 53;   // application font changed
  CM_BORDERCHANGED         = CM_VCLBASE + 59;
  CM_MOUSEWHEEL            = CM_VCLBASE + 67;

  { CM_HITTEST }
  HTNOWHERE = 0;
  HTCLIENT = 1;

type
  TMessage = QWindows.TMessage;
//  TJvMessage = JvQTypes.TJvMessage;

  TEMGetRect = packed record
    Msg: Cardinal;
    Reserved: Integer;
    Rect: PRect;
    Result: Integer;
  end;

  TEMSetRect       = TEMGetRect;

  TWMNoParams = packed record
    Msg: Cardinal;
    Unused: array[0..3] of Word;
    Handled: LongBool;
  end;

  TWMActivate = packed record
    Msg: Integer;
    Active: Word; { WA_INACTIVE, WA_ACTIVE, WA_CLICKACTIVE }
    Minimized: WordBool;
    ActiveWindow: QWidgetH;
    Result: Integer;
  end;

  TWMCancelMode     = TWMNoParams;

  TWMChar = packed record
    Msg: Cardinal;
    CharCode: Word;
    Unused: Word;
    KeyData: Longint;
    Result: Longint;
  end;

  TWMChildActivate  = TWMNoParams;
  TWMClear          = TWMNoParams;
  TWMClose          = TWMNoParams;

  TWMContextMenu = packed record
    Msg: Cardinal;
    hWnd: QWidgetH;      // WParam
    case Integer of  // LParam
    0:
    (
      Position: TSmallPoint;
      Result: Integer;
    );
    1:
    (
      XPos: Smallint;
      YPos: Smallint
    );
  end;

  TWMCopy           = TWMNoParams;
  TWMCut            = TWMNoParams;
  TWMDestroy        = TWMNoParams;

  TWMEraseBkgnd = packed record
    Msg: Cardinal;
    DC: QPainterH;
    Unused: Integer;
    Result: Integer;
  end;

  TWMGetDlgCode     = TWMNoParams;
  TWMGetFont        = TWMNoParams;
  TWMGetHotKey      = TWMNoParams;

  TWMGetText = packed record
    Msg: Cardinal;
    TextMax: Integer;
    Text: PWideChar;
    Result: Integer;
  end;

  TWMHotKey = packed record
    Msg: Cardinal;
    HotKey: Integer;
    Unused: Integer;
    Result: Integer;
  end;

  TWMKey            = TWMChar;
  TWMKeyDown        = TWMKey;
  TWMKeyUp          = TWMKey;

  TWMPaint          = TWMEraseBkgnd;

  TWMKillFocus = packed record
    Msg: Cardinal;
    FocusedWnd: QWidgetH;
    Unused: Integer;
    Result: Integer;
  end;

  TWMSetFocus = TWMKillFocus;

  TWMSetCursor = packed record
    Msg: Cardinal;
    CursorWnd: QWidgetH;
    HitTest: Word;
    MouseMsg: Word;
    Result: Integer;
  end;

  TWMSetText = packed record
    Msg: Cardinal;
    Unused: Integer;
    Text: PWideChar;
    Result: Integer;
  end;

  TWMMove = packed record
    Msg: Cardinal;
    Unused: integer;

    case Integer of  // LParam
    0:
    (
      Position: TSmallPoint;
      Result: Integer;
    );
    1:
    (
      XPos: Smallint;
      YPos: Smallint;
    );
  end;

  TWMMouse = packed record
    Msg: Cardinal;
    Keys: Integer;   // WParam

    case Integer of  // LParam
    0:
    (
      Position: TSmallPoint;
      Result: Integer;
    );
    1:
    (
      XPos: Smallint;
      YPos: Smallint
    );
  end;

  TWMLButtonDblClk = TWMMouse;   // left mouse button
  TWMLButtonDown   = TWMMouse;
  TWMLButtonUp     = TWMMouse;
  TWMMButtonDblClk = TWMMouse;   // middle mouse button
  TWMMButtonDown   = TWMMouse;
  TWMMButtonUp     = TWMMouse;

  TWMMouseWheel = packed record
    Msg: Cardinal;
    Keys: SmallInt;          // WParamLo
    WheelDelta: SmallInt;    // WParamHi

    case Integer of
    0:
    (
      Pos: TSmallPoint;  // LParam
      Result: Integer;
    );
    1:
    (
      XPos: Smallint;        // LParamLo
      YPos: Smallint         // LParamHi
    );
  end;

  TWMNCHitTest      = TWMMouse;

  TWMScroll = packed record
    Msg: Cardinal;
    Pos: Integer;
    ScrollCode: Integer;
    Result: Integer;
    pt: TPoint
  end;

  TWMHScroll        = TWMScroll;
  TWMVScroll        = TWMScroll;

  TWMTimer = packed record
    Msg: Cardinal;
    TimerID: Integer;
    TimerProc: TTimerProc;

    case Integer of
    0:
    (
      Result: Integer
    );
    1:
    (
      Time: Cardinal;
      pt: TPoint
    );
  end;
  
procedure BroadCastMsg(AControl: TWidgetControl; var Mesg);

implementation

procedure BroadCastMsg(AControl: TWidgetControl; var Mesg);
var
  I: integer;
begin
  with AControl do
    for I := 0 to ControlCount - 1 do
    begin
      Controls[I].Dispatch(Mesg);
      if TMessage(Mesg).Result <> 0 then
        Exit;
    end;
end;


end.




