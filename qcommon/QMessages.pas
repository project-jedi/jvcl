{-------------------------------------------------------------------------------
 QMessages.pas

 Copyright (c) 2004 Andreas Hausladen (Andreas dott Hausladen att gmx dott de),
                    André Snepvangers (asn att xs4all dott nl)
 All rights reserved.

 Version 0.1
 Description: Wrappers for common VCL control Messages
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
The Original Code is: QMessages.pas, released on 2004-05-21.


Known Issues:
  - Under development. It contains all message related stuff, taken from QWindows.
    No new functionality has been added yet.
{-----------------------------------------------------------------------------}
// $Id$

unit QMessages;

interface

uses
  SysUtils, Classes,
  Qt, QControls, QWindows;

const

  EM_GETRECT          = $00B2;
  EM_SETRECT          = $00B3;

// VCL Messages
// taken from Controls (VCL)
  CM_BASE                   = 100; //$B000;
  CM_ACTIVATE               = CM_BASE + 0;
  CM_DEACTIVATE             = CM_BASE + 1;
  CM_GOTFOCUS               = CM_BASE + 2;
  CM_LOSTFOCUS              = CM_BASE + 3;
  CM_CANCELMODE             = CM_BASE + 4;
  CM_DIALOGKEY              = CM_BASE + 5;
  CM_DIALOGCHAR             = CM_BASE + 6;
  CM_FOCUSCHANGED           = CM_BASE + 7;
  CM_PARENTFONTCHANGED      = CM_BASE + 8;
  CM_PARENTCOLORCHANGED     = CM_BASE + 9;
  CM_HITTEST                = CM_BASE + 10;
  CM_VISIBLECHANGED         = CM_BASE + 11;
  CM_ENABLEDCHANGED         = CM_BASE + 12;
  CM_COLORCHANGED           = CM_BASE + 13;
  CM_FONTCHANGED            = CM_BASE + 14;
  CM_CURSORCHANGED          = CM_BASE + 15;
//  CM_CTL3DCHANGED           = CM_BASE + 16;
//  CM_PARENTCTL3DCHANGED     = CM_BASE + 17;
  CM_TEXTCHANGED            = CM_BASE + 18;
  CM_MOUSEENTER             = CM_BASE + 19;
  CM_MOUSELEAVE             = CM_BASE + 20;
  CM_MENUCHANGED            = CM_BASE + 21;
  CM_APPKEYDOWN             = CM_BASE + 22;
  CM_APPSYSCOMMAND          = CM_BASE + 23;
  CM_BUTTONPRESSED          = CM_BASE + 24;
  CM_SHOWINGCHANGED         = CM_BASE + 25;
  CM_ENTER                  = CM_BASE + 26;
  CM_EXIT                   = CM_BASE + 27;
  CM_DESIGNHITTEST          = CM_BASE + 28;
  CM_ICONCHANGED            = CM_BASE + 29;
  CM_WANTSPECIALKEY         = CM_BASE + 30;
  CM_INVOKEHELP             = CM_BASE + 31;
  CM_WINDOWHOOK             = CM_BASE + 32;
  CM_RELEASE                = CM_BASE + 33;
  CM_SHOWHINTCHANGED        = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED  = CM_BASE + 35;
  CM_SYSCOLORCHANGE         = CM_BASE + 36;
//  CM_WININICHANGE           = CM_BASE + 37;
  CM_FONTCHANGE             = CM_BASE + 38;
  CM_TIMECHANGE             = CM_BASE + 39;
  CM_TABSTOPCHANGED         = CM_BASE + 40;
  CM_UIACTIVATE             = CM_BASE + 41;
  CM_UIDEACTIVATE           = CM_BASE + 42;
  CM_DOCWINDOWACTIVATE      = CM_BASE + 43;
  CM_CONTROLLISTCHANGE      = CM_BASE + 44;
  CM_GETDATALINK            = CM_BASE + 45;
  CM_CHILDKEY               = CM_BASE + 46;
  CM_DRAG                   = CM_BASE + 47;
  CM_HINTSHOW               = CM_BASE + 48;
  CM_DIALOGHANDLE           = CM_BASE + 49;
  CM_ISTOOLCONTROL          = CM_BASE + 50;
  CM_RECREATEWND            = CM_BASE + 51;
  CM_INVALIDATE             = CM_BASE + 52;
  CM_SYSFONTCHANGED         = CM_BASE + 53;
  CM_CONTROLCHANGE          = CM_BASE + 54;
  CM_CHANGED                = CM_BASE + 55;
//  CM_DOCKCLIENT             = CM_BASE + 56;
//  CM_UNDOCKCLIENT           = CM_BASE + 57;
  CM_FLOAT                  = CM_BASE + 58;
  CM_BORDERCHANGED          = CM_BASE + 59;
//  CM_BIDIMODECHANGED        = CM_BASE + 60;
//  CM_PARENTBIDIMODECHANGED  = CM_BASE + 61;
//  CM_ALLCHILDRENFLIPPED     = CM_BASE + 62;
  CM_ACTIONUPDATE           = CM_BASE + 63;
  CM_ACTIONEXECUTE          = CM_BASE + 64;
  CM_HINTSHOWPAUSE          = CM_BASE + 65;
  CM_DOCKNOTIFICATION       = CM_BASE + 66;
  CM_MOUSEWHEEL             = CM_BASE + 67;
  CM_ISSHORTCUT             = CM_BASE + 68;
{$IFDEF LINUX}
  CM_RAWX11EVENT            = CM_BASE + 69;
{$ENDIF}

// windows messages
  WM_HSCROLL                = CM_BASE + 100;
  WM_VSCROLL                = CM_BASE + 101;

  WM_USER             = $0400;
  WM_TIMER            = $0113;
  WM_NCPAINT          = $0085;

type
  IPerformControl = interface
    ['{B11AA73D-D7C2-43E5-BED8-8F82DE6152AB}']
    function Perform(Msg: Cardinal; WParam, LParam: Longint): Longint;
  end;
//

  PMessage = ^TMessage;
  TMessage = packed record
    Msg: Integer;
    WParam: Longint;
    LParam: Longint;
    Result: Integer;
  end;

  TMsg = packed record
    hwnd: QWidgetH;
    message: cardinal;
    wParam: Longint;
    lParam: Longint;
    time: cardinal;
    pt: TPoint;
  end;

  TWMScroll = packed record
    Msg: Integer;
    Pos: Integer;
    ScrollCode: Integer;
  end;

  TCMActivate = packed record
    Msg: Integer;
    WParam: Integer;
    LParam: Longint;
    Result: Integer;
  end;

  TCMFocusChanged = record
    Msg: Cardinal;
    Unused: Integer;
    Sender: TWinControl;
    Result: Longint;
  end;

  TCMControlListChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: Longint;
  end;

  TCMControlChange = record
    Msg: Cardinal;
    Control: TControl;
    Inserting: LongBool;
    Result: Longint;
  end;

  TCMChanged = record
    Msg: Cardinal;
    Unused: Longint;
    Child: TControl;
    Result: Longint;
  end;


  { Message }
function Perform(Control: TControl; Msg: Cardinal; WParam, LParam: Longint): Longint;
 { Limitation: Handle must be a TWidgetControl derived class handle }
function PostMessage(Handle: QWidgetH; Msg: Integer; WParam, LParam: Longint): LongBool;
 { SendMsg synchronizes with the main thread }
function SendMessage(Handle: QWidgetH; Msg: Integer; WParam, LParam: Longint): Integer; overload;
function SendMessage(AControl: TWidgetControl; Msg: Integer; WParam, LParam: Longint): Integer; overload;


implementation

type
  TQtObject = class(TObject)
  private
    FHandle: QObjectH;
    FHooks: QObject_hookH;
  protected
    function MainEventFilter(Sender: QObjectH; Event: QEventH): Boolean; cdecl;
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; virtual;
  public
    constructor Create(const AName: string = '');
    destructor Destroy; override;
    property Handle: QObjectH read FHandle;
    property Hooks: QObject_hookH read FHooks;
  end;

constructor TQtObject.Create(const AName: string = '');
var
  Method: TMethod;
begin
  inherited Create;
  FHandle := QObject_create(nil, Pointer(AName));
  FHooks := QObject_hook_create(FHandle);
  TEventFilterMethod(Method) := MainEventFilter;
  Qt_hook_hook_events(FHooks, Method);
end;

destructor TQtObject.Destroy;
begin
  QObject_hook_destroy(FHooks);
  QObject_destroy(FHandle);
  inherited Destroy;
end;

function TQtObject.MainEventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  try
    Result := EventFilter(Sender, Event);
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      Result := False;
    end;
  end;
end;

function TQtObject.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  Result := False; // default handling
end;

const
  QEventType_CMDispatchMessagePost = QEventType(Integer(QEventType_ClxUser) - 1);
  QEventType_CMDispatchMessageSend = QEventType(Integer(QEventType_ClxUser) - 2);

type
  PMessageData = ^TMessageData;
  TMessageData = record
    Control: TWidgetControl;
    Event: TEvent;
    Msg: TMessage;
  end;

function AppEventFilter(App: TApplication; Sender: QObjectH; Event: QEventH): Boolean; cdecl;
var
  Msg: PMessageData;
begin
  try
    Result := False;
    if QEvent_isQCustomEvent(Event) then
    begin
      Msg := QCustomEvent_data(QCustomEventH(Event));
      case QEvent_type(Event) of
        QEventType_CMDispatchMessagePost:
          begin
            Result := True;
            if Msg <> nil then
              try
                Msg^.Control.Dispatch(Msg^.Msg);
              finally
                Dispose(Msg);
              end;
            Exit;
          end;
        QEventType_CMDispatchMessageSend:
          begin
            Result := True;
            try
              if Msg <> nil then
                Msg^.Control.Dispatch(Msg^.Msg);
            finally
              if Msg^.Event <> nil then
                Msg^.Event.SetEvent;
            end;
            Exit;
          end;
      end;
    end;
  except
    on E: Exception do
    begin
      Application.ShowException(E);
      Result := False;
    end;
  end;
end;

var
  AppEventFilterHook: QObject_hookH = nil;

procedure InstallAppEventFilter;
var
  Method: TMethod;
begin
  if AppEventFilterHook <> nil then
    Exit;

  Method.Code := @AppEventFilter;
  Method.Data := Application;

  AppEventFilterHook := QObject_hook_create(Application.Handle);
  Qt_hook_hook_events(AppEventFilterHook, Method);
end;

function Perform(Control: TControl; Msg: Cardinal; WParam, LParam: Longint): Longint;
var
  M: TMessage;
  P: IPerformControl;
begin
  if Supports(Control, IPerformControl, P) then
    Result := P.Perform(Msg, WParam, LParam)
  else
  begin
    M.Msg := Msg;
    M.WParam := WParam;
    M.LParam := LParam;
    M.Result := 0;
    Control.Dispatch(M);
    Result := M.Result;
  end;
end;

function PostMessage(Handle: QWidgetH; Msg: Integer; WParam, LParam: Longint): LongBool;
var
  M: PMessageData;
  Control: TWidgetControl;
begin
  Result := False;
  if Handle = nil then
    Exit;
  Control := FindControl(Handle);
  if Control = nil then
    Exit;
  try
    New(M);
    M^.Control := Control;
    M.Event := nil;
    M^.Msg.Msg := Msg;
    M^.Msg.WParam := WParam;
    M^.Msg.LParam := LParam;
    M^.Msg.Result := 0;

    InstallAppEventFilter;
    QApplication_postEvent(Handle, QCustomEvent_create(QEventType_CMDispatchMessagePost, M));
    Result := True;
  except
    Result := False;
  end;
end;

function SendMessage(AControl: TWidgetControl; Msg: Integer; WParam, LParam: Longint): Integer;
begin
  Result := SendMessage(AControl.Handle, Msg, WParam, LParam);
end;

function SendMessage(Handle: QWidgetH; Msg: Integer; WParam, LParam: Longint): Integer;
var
  Event: QCustomEventH;
  M: TMessageData;
  Control: TWidgetControl;
begin
  Result := 0;
  if Handle = nil then
    Exit;
  Control := FindControl(Handle);
  if Control = nil then
    Exit;
  try
    M.Control := Control;
    M.Event := nil;
    M.Msg.Msg := Msg;
    M.Msg.WParam := WParam;
    M.Msg.LParam := LParam;
    M.Msg.Result := 0;

    InstallAppEventFilter;
    Event := QCustomEvent_create(QEventType_CMDispatchMessageSend, @M);
    try
      if GetCurrentThreadId = MainThreadID then
        QApplication_sendEvent(Handle, Event)
      else
      begin
       // synchronize with main thread
        M.Event := TSimpleEvent.Create;
        try
          QApplication_postEvent(Handle, Event);
          M.Event.WaitFor(INFINITE);
        finally
          M.Event.Free;
          M.Event := nil;
        end;
      end;
      Result := M.Msg.Result;
    finally
      if GetCurrentThreadId = MainThreadID then
        QCustomEvent_destroy(Event);
    end;
  except
  end;
end;

finalization
  if Assigned(AppEventFilterHook) then
    QObject_hook_destroy(AppEventFilterHook);
end.


end.
