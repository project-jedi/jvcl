{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTrayIcon.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Feng Mingyu(Winston Feng), [winstonf@tom.com]
Vlad S

Last Modified: 2003-09-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{
History:
  2/29/2004 12:02PM
     VladS separate click and dblclick
  2003-09-28 by Winston Feng
    Add WM_SESSIONEND message handler, TaskbarRestart message handler to:
      Clean the trayicon when session ends.
      Restore the trayicon when session restart.
    Remove the old unsuccessful DoCheckCrash method.
}

{$I jvcl.inc}

unit JvTrayIcon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  Menus, ShellApi,
{$IFDEF COMPILER6_UP}
  DateUtils,
{$ENDIF COMPILER6_UP}
  JvConsts, JvTypes, JvComponent;

type
  TBalloonType = (btNone, btError, btInfo, btWarning);

  TNotifyIconDataXP = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array[0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array[0..255] of AnsiChar;
    uTimeOut: DWORD;
    szInfoTitle: array[0..63] of AnsiChar;
    dwInfoFlags: DWORD;
  end;

  TAnimateEvent = procedure(Sender: TObject; const ImageIndex: Integer) of object;
  TRegisterServiceProcess = function(dwProcessID, dwType: Integer): Integer; stdcall;

  TTrayVisibility = (tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide, tvAutoHideIcon, tvVisibleDesign,
    tvRestoreClick, tvRestoreDbClick, tvMinimizeClick, tvMinimizeDbClick);
  TTrayVisibilities = set of TTrayVisibility;

  TJvTrayIcon = class(TJvComponent)
  private
    FTaskbarRestartMsg: Cardinal;
    FCurrentIcon: TIcon;
  protected
    FActive: Boolean;
    FIcon: TIcon;
    FIconData: TNotifyIconDataXP;
    FHandle: THandle;
    FHint: string;
    FBalloonCloser: TTimer;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TMouseEvent;

    { Vlad S}
    {
    distinguish single-click and a double-click
    Create a timer which is started on the first click, set the timeout value to
    something a bit longer than the double-click, then connect the timeout() signal
    to a slot of your own. When a double click event is received you simply stop
    the timer. If the custom slot is visited you know that a single click was
    done.
    }
    FDblClicked: boolean;
    FDblClickTimer: TTimer;
    FClickedButton: TMouseButton;
    FClickedShift: TShiftState;
    FClickedX: Integer;
    FClickedY: Integer;
    { Vlad S end.}

    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FApplicationVisible: Boolean; { Used??}
    FAnimated: Boolean;
    FDelay: Cardinal;
    FIcons: TImageList;
    FTimer: TTimer;
    FIconIndex: Integer;
    FDropDownMenu: TPopupMenu;
    FTask: Boolean;
    FRegisterServiceProcess: TRegisterServiceProcess;
    FDllHandle: THandle;
    FOnBalloonHide: TNotifyEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonClick: TNotifyEvent;
    FTime: TDateTime;
    FTimeDelay: Integer;
    FOldTray: HWND;
    FOnAnimate: TAnimateEvent;
    FVisibility: TTrayVisibilities;
    FOldHint: string;
    FSnap: Boolean;
    FHooked: Boolean;
    function GetSystemMinimumBalloonDelay: integer;
    procedure OnAnimateTimer(Sender: TObject);
    procedure OnBalloonCloserTimer(Sender: TObject);
    procedure IconChanged(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIcon(Icon: TIcon);
    procedure SetApplicationVisible(Value: Boolean);
    procedure SetAnimated(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetIcons(const Value: TImageList);
    procedure SetTask(const Value: Boolean);
    procedure SetIconIndex(const Value: Integer);
    procedure SetVisibility(const Value: TTrayVisibilities);
    procedure Hook;
    procedure UnHook;
    procedure WndProc(var Mesg: TMessage);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDoubleClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnDblClickTimer(Sender: TObject); { Vlad S}
    function ApplicationHook(var Msg: TMessage): Boolean;
    function NotifyIcon(dwMessage: DWORD): boolean;
    procedure SetCurrentIcon(Value: TIcon); //HEG: New
    procedure IconPropertyChanged; //HEG: New
    procedure ActivePropertyChanged; //HEG: New
    procedure Loaded; override; //HEG: New

    property ApplicationVisible: Boolean read FApplicationVisible write SetApplicationVisible default True;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentIcon: TIcon read FCurrentIcon write SetCurrentIcon;
    procedure DoCheckCrash;
    procedure HideApplication;
    procedure ShowApplication;
    procedure BalloonHint(Title, Value: string; BalloonType:
      TBalloonType = btNone; Delay: Integer = 5000; CancelPrevious: boolean = false);
    function AcceptBalloons: Boolean;
  published
    { (rb) Active should be set in Loaded; Icon isn't set when Active is now
      set to True etc. }
    property Active: Boolean read FActive write SetActive default False;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Icon: TIcon read FIcon write SetIcon;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property Icons: TImageList read FIcons write SetIcons;
    property Hint: string read FHint write SetHint;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property Snap: Boolean read FSnap write FSnap default False;
    property Visibility: TTrayVisibilities read FVisibility write SetVisibility
      default [tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide];
    property OnAnimate: TAnimateEvent read FOnAnimate write FOnAnimate;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TMouseEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnBalloonShow: TNotifyEvent read FOnBalloonShow write FOnBalloonShow;
    property OnBalloonHide: TNotifyEvent read FOnBalloonHide write FOnBalloonHide;
    property OnBalloonClick: TNotifyEvent read FOnBalloonClick write FOnBalloonClick;
  end;

implementation

uses
  JvJCLUtils, JvJVCLUtils;

const
  WM_CALLBACKMESSAGE = WM_USER + 1;
  NOTIFYICON_VERSION = 3;
  NIM_ADD = $00000000;
  NIM_MODIFY = $00000001;
  NIM_DELETE = $00000002;
  NIM_SETFOCUS = $00000003;
  NIM_SETVERSION = $00000004;

  NIF_MESSAGE = $00000001;
  NIF_ICON = $00000002;
  NIF_TIP = $00000004;
  NIF_STATE = $00000008;
  NIF_INFO = $00000010;

  NIS_HIDDEN = $00000001;
  NIS_SHAREDICON = $00000002;

  NIIF_NONE = $00000000;
  NIIF_INFO = $00000001;
  NIIF_WARNING = $00000002;
  NIIF_ERROR = $00000003;

  NIN_SELECT = (WM_USER + 0);
  NINF_KEY = 1;
  NIN_KEYSELECT = (NIN_SELECT or NINF_KEY);

  NIN_BALLOONSHOW = WM_USER + 2;
  NIN_BALLOONHIDE = WM_USER + 3;
  NIN_BALLOONTIMEOUT = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;

{$IFNDEF COMPILER6_UP}

function SecondsBetween(const Now: TDateTime; const FTime: TDateTime): Integer;
begin
  Result := Trunc(86400 * (FTime - Now));
end;
{$ENDIF COMPILER6_UP}

constructor TJvTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FCurrentIcon := TIcon.Create;
  FApplicationVisible := True;
  FSnap := False;
  FHandle := AllocateHWndEx(WndProc);

  FVisibility := [tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide];
  FAnimated := False;
  FDelay := 100;
  FIconIndex := 0;
  FActive := False;
  FTask := True;

  FBalloonCloser := TTimer.Create(self);
  FBalloonCloser.Enabled := false;
  FBalloonCloser.OnTimer := OnBalloonCloserTimer;

  FDblClickTimer := TTimer.Create(self);  { Vlad S}
  FDblClickTimer.Enabled := false;
  FDblClickTimer.OnTimer := OnDblClickTimer; { Vlad S}

  if not (csDesigning in ComponentState) then
  begin
    FDllHandle := LoadLibrary('KERNEL32.DLL');
    if FDllHandle <> 0 then
      @FRegisterServiceProcess := GetProcAddress(FDllHandle, 'RegisterServiceProcess')
    else
      @FRegisterServiceProcess := nil;
  end;

  FTaskbarRestartMsg := RegisterWindowMessage('TaskbarCreated');
end;

destructor TJvTrayIcon.Destroy;
begin
  UnHook;
  if not (csDestroying in Application.ComponentState) then
    SetTask(False);

  FDblClickTimer.Free; { Vlad S}
  FTimer.Free;
  SetActive(False);
  FBalloonCloser.Enabled := false;
  FBalloonCloser.Free;
  FIcon.Free;
  FCurrentIcon.Free;
  DeallocateHWndEx(FHandle);

  if not (csDesigning in ComponentState) then
    if FDllHandle <> 0 then
      FreeLibrary(FDllHandle);
  inherited Destroy;
end;

procedure TJvTrayIcon.Loaded;
begin
  inherited Loaded;
  IconPropertyChanged;
  ActivePropertyChanged;
end;

function TJvTrayIcon.AcceptBalloons: Boolean;
begin
  // Balloons are only accepted with shell32.dll 5.0+
  Result := GetShellVersion >= $00050000;
end;

procedure TJvTrayIcon.SetTask(const Value: Boolean);
begin
  if FTask <> Value then
  begin
    FTask := Value;
    if not (csDesigning in ComponentState) then
      if Assigned(FRegisterServiceProcess) then
        if FTask then
          FRegisterServiceProcess(GetCurrentProcessID, 0)
        else
          FRegisterServiceProcess(GetCurrentProcessID, 1);
  end;
end;

procedure TJvTrayIcon.WndProc(var Mesg: TMessage);
var
  I: Integer;
  Pt: TPoint;
  ShState: TShiftState;
begin
  try
    with Mesg do
      case Msg of
        WM_CALLBACKMESSAGE:
          begin
            GetCursorPos(Pt);
            ShState := [];
            if GetKeyState(VK_SHIFT) < 0 then
              Include(ShState, ssShift);
            if GetKeyState(VK_CONTROL) < 0 then
              Include(ShState, ssCtrl);
            if GetKeyState(VK_LBUTTON) < 0 then
              Include(ShState, ssLeft);
            if GetKeyState(VK_RBUTTON) < 0 then
              Include(ShState, ssRight);
            if GetKeyState(VK_MBUTTON) < 0 then
              Include(ShState, ssMiddle);
            if GetKeyState(VK_MENU) < 0 then
              Include(ShState, ssAlt);
            case LParam of
              WM_MOUSEMOVE:
                DoMouseMove(shState, Pt.X, Pt.Y);
              WM_LBUTTONDOWN:
                DoMouseDown(mbLeft, ShState, Pt.X, Pt.Y);
              WM_RBUTTONDOWN:
                DoMouseDown(mbRight, ShState, Pt.X, Pt.Y);
              WM_MBUTTONDOWN:
                DoMouseDown(mbMiddle, ShState, Pt.X, Pt.Y);
              WM_LBUTTONUP:
                DoMouseUp(mbLeft, ShState, Pt.X, Pt.Y);
              WM_MBUTTONUP:
                DoMouseUp(mbMiddle, ShState, Pt.X, Pt.Y);
              WM_RBUTTONUP, NIN_KEYSELECT: //Mimics previous versions of shell32.dll
                DoMouseUp(mbRight, ShState, Pt.X, Pt.Y);
              WM_LBUTTONDBLCLK:
                DoDoubleClick(mbLeft, ShState, Pt.X, Pt.Y);
              WM_RBUTTONDBLCLK:
                DoDoubleClick(mbRight, ShState, Pt.X, Pt.Y);
              WM_MBUTTONDBLCLK:
                DoDoubleClick(mbMiddle, ShState, Pt.X, Pt.Y);
              NIN_BALLOONHIDE: //sb
                begin
                  try
                    if Assigned(FOnBalloonHide) then
                      FOnBalloonHide(self);
                  except
                  end;
                  Result := Ord(True);
                end;
              NIN_BALLOONTIMEOUT: //sb
                begin
                  I := SecondsBetween(Now, FTime);
                  if I > FTimeDelay then
                    BalloonHint('', '');
                  Result := Ord(True);
                end;
              NIN_BALLOONUSERCLICK: //sb
                begin
                  try
                    if Assigned(FOnBalloonClick) then
                      FOnBalloonClick(self);
                  except
                  end;
                  Result := Ord(True);
                  //Result := DefWindowProc(FHandle, Msg, wParam, lParam);
                  BalloonHint('', '');
                end;
            end;
          end;
        // Add by Winston Feng 2003-9-28
        // Handle the QueryEndSession and TaskbarCreated message, so trayicon
        // will be deleted and restored correctly.
        WM_QUERYENDSESSION:
          Result := 1;
        WM_ENDSESSION:
          begin
            if FActive then
            begin
              SetActive(False);
              FActive := True;
            end
            else
            begin
              SetActive(False);
              FActive := False;
            end;
          end;
      else
          if (Msg = FTaskbarRestartMsg) and (FActive) then
            SetActive(True);
          Result := DefWindowProc(FHandle, Msg, wParam, lParam);
    end; // with
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvTrayIcon.IconChanged(Sender: TObject);
begin
  IconPropertyChanged;
end;

procedure TJvTrayIcon.SetHint(Value: string);
begin
  //DoCheckCrash;
  //Remove sLineBreak on w98/me as they are not supported
  if not AcceptBalloons then
    FHint := StringReplace(Value, sLineBreak, ' - ', [rfReplaceAll])
  else
    FHint := Value;
end;

//HEG: New
procedure TJvTrayIcon.SetCurrentIcon(Value: TIcon);
begin
  FCurrentIcon.Assign(Value);
  FIconData.hIcon := FCurrentIcon.Handle;
  if FActive and not (csLoading in ComponentState) then
    NotifyIcon(NIM_MODIFY);
end;

//HEG: New
procedure TJvTrayIcon.IconPropertyChanged;
var
  Ico: TIcon;
begin
  if not (csLoading in ComponentState) then
  begin
    if (FIcons <> nil) and (FIconIndex >= 0) and (FIconIndex < FIcons.Count) then
    begin
      Ico := TIcon.Create;
      try
        FIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
        FIcons.GetIcon(FIconIndex, Ico);
        SetCurrentIcon(Ico);
      finally
        Ico.Free;
      end;
    end
    else
    if Assigned(Icon) and (not Icon.Empty) then
      SetCurrentIcon(Icon)
    else
      SetCurrentIcon(Application.Icon);
  end;
end;
procedure TJvTrayIcon.SetIcon(Icon: TIcon);
begin
  FIcon.Assign(Icon);
  IconPropertyChanged;
end;

procedure TJvTrayIcon.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    ActivePropertyChanged;
  end;
end;

procedure TJvTrayIcon.SetApplicationVisible(Value: Boolean);
begin
  FApplicationVisible := Value;
  if (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    Application.ShowMainForm := FApplicationVisible;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    if Value then
      ShowApplication
    else
      HideApplication;
end;

procedure TJvTrayIcon.HideApplication;
begin
  if (Application.MainForm <> nil) and (Application.MainForm.WindowState <> wsMinimized) then
  begin
    if Snap then
    begin
      Application.MainForm.Visible := False;
      Application.ShowMainForm := False;
    end;
    Application.Minimize;
  end;
  ShowWindow(Application.Handle, SW_HIDE);
  FApplicationVisible := False;
  if tvAutoHideIcon in Visibility then
    NotifyIcon(NIM_ADD);
end;

procedure TJvTrayIcon.ShowApplication;
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Application.Restore;
  if Application.MainForm <> nil then
    Application.MainForm.Visible := True;
  if tvAutoHideIcon in Visibility then
    NotifyIcon(NIM_DELETE);
end;

procedure TJvTrayIcon.OnAnimateTimer(Sender: TObject);
begin
  if FActive and (FIcons <> nil) then
  begin
    if IconIndex < 0 then
      IconIndex := 0
    else
      IconIndex := (IconIndex + 1) mod FIcons.Count;
    if Assigned(FOnAnimate) then
      FOnAnimate(Self, IconIndex);
  end;
end;

procedure TJvTrayIcon.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  if (not (csDesigning in ComponentState)) or (tvVisibleDesign in Visibility) then
  begin
    if Value then
    begin
      if FTimer = nil then
      begin
        FTimer := TTimer.Create(Self);
        FTimer.Interval := FDelay;
        FTimer.OnTimer := OnAnimateTimer;
      end;
      FTimer.Enabled := True;
    end
    else
      FreeAndNil(FTimer);
  end;
end;

procedure TJvTrayIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  if FTimer <> nil then
    FTimer.Interval := FDelay;
end;

procedure TJvTrayIcon.SetIcons(const Value: TImageList);
begin
  if FIcons <> Value then
  begin
    FIcons := Value;
    IconPropertyChanged; //HEG: New
  end;
end;

function TJvTrayIcon.GetSystemMinimumBalloonDelay: integer;
begin
  // from Microsoft's documentation, a balloon is shown for at
  // least 10 seconds, but it is a system settings which must
  // be somewhere in the registry. The only question is : Where ?
  Result := 10000;
end;

procedure TjvTrayIcon.OnBalloonCloserTimer(Sender: TObject);
begin
  // we stop the timer
  FBalloonCloser.Enabled := false;
  // then we call BalloonHint with title and info set to
  // empty strings which surprisingly will cancel any existing
  // balloon for the icon. This is clearly not documented by
  // microsoft and may not work in later releases of Windows
  // it has been tested on XP Home French
  BalloonHint('', '');
end;

procedure TJvTrayIcon.BalloonHint(Title, Value: string;
  BalloonType: TBalloonType; Delay: Integer; CancelPrevious: boolean);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/reference/functions/shell_notifyicon.asp
begin
  if AcceptBalloons then
  begin
    FTime := Now;
    FTimeDelay := Delay div 1000;
    FIconData.uFlags := NIF_INFO;

    // if we must cancel an existing balloon
    if CancelPrevious then
      // then we call BalloonHint with title and info set to
      // empty strings which surprisingly will cancel any existing
      // balloon for the icon. This is clearly not documented by
      // microsoft and may not work in later releases of Windows
      // it has been tested on XP Home French
      BalloonHint('', '');

    with FIconData do
      StrPLCopy(szInfoTitle, Title, SizeOf(szInfoTitle) - 1);
    with FIconData do
      StrPLCopy(szInfo, Value, SizeOf(szInfo) - 1);
    FIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
    FIconData.uTimeOut := Delay;
    case BalloonType of
      btError:
        FIconData.dwInfoFlags := NIIF_ERROR;
      btInfo:
        FIconData.dwInfoFlags := NIIF_INFO;
      btNone:
        FIconData.dwInfoFlags := NIIF_NONE;
      btWarning:
        FIconData.dwInfoFlags := NIIF_WARNING;
    end;
    NotifyIcon(NIM_MODIFY);

    // if the delay is less than the system's minimum and the balloon
    // was really shown (title and value are not empty)
    if (Delay < GetSystemMinimumBalloonDelay) and (Title <> '') and (Value <> '') then
    begin
      // then we enable the ballon closer timer which will cancel
      // the balloon when the delay is elapsed
      FBalloonCloser.Interval := Delay;
      FBalloonCloser.Enabled := true;
    end;

    if Assigned(FOnBalloonShow) then
      FOnBalloonShow(self);
  end;
end;

procedure TJvTrayIcon.DoCheckCrash;
var
  HWndTray: HWND;
begin
  if Active then
  begin
    HWndTray := FindWindow('Shell_TrayWnd', nil);
    if FOldTray <> HWndTray then
    begin
      FOldTray := HWndTray;
      Active := False;
      Active := True;
    end;
  end;
end;

procedure TJvTrayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FOldHint <> FHint then
  begin
    FOldHint := FHint;
    with FIconData do
      StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    FIconData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    if FActive then
      NotifyIcon(NIM_MODIFY);
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TJvTrayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDblClicked := false;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if (Button = mbLeft) and (FDropDownMenu <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FDropDownMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
  end;
end;

procedure TJvTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
    hasPopup: boolean;
begin
  hasPopup := false;
  if (Button = mbRight) and (FPopupMenu <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FPopupMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
    hasPopup := true;
  end
  else
  if Button = mbLeft then
    if not ApplicationVisible then
    begin
      if tvRestoreClick in Visibility then
        Visibility := Visibility + [tvVisibleTaskBar]
    end
    else
    begin
      if tvMinimizeClick in Visibility then
        Visibility := Visibility - [tvVisibleTaskBar]
    end;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if (not hasPopup) and Assigned(FOnClick) and not FDblClicked then
  begin
    { Vlad S}
    //FOnClick(Self, Button, Shift, X, Y);
    FClickedButton := Button;
    FClickedShift := Shift;
    FClickedX  := X;
    FClickedY := Y;
    FDblClickTimer.Interval := GetDoubleClickTime() + 10;
    FDblClickTimer.Enabled := true;
  end;
end;

procedure TJvTrayIcon.OnDblClickTimer(Sender: TObject); { Vlad S}
begin
    FDblClickTimer.Enabled := false;
    // Double-clicking the right mouse button actually generates four messages:
    // WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK, and WM_RBUTTONUP again
    if (not FDblClicked) then
        FOnClick(Self, FClickedButton, FClickedShift, FClickedX, FClickedY);
end;

procedure TJvTrayIcon.DoDoubleClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  FDblClicked := true; { Vlad S}
  FDblClickTimer.Enabled := false; { Vlad S}

  if Assigned(FOnDblClick) then
    FOnDblClick(Self, Button, Shift, X, Y)
  else
  if Button = mbLeft then
  begin
    if FPopupMenu <> nil then
      for I := 0 to FPopupMenu.Items.Count - 1 do
        if FPopupMenu.Items[I].Default then
        begin
          FPopupMenu.Items[I].Click;
          Break;
        end;
    if ApplicationVisible then
    begin
      if tvMinimizeDbClick in Visibility then
        Visibility := Visibility - [tvVisibleTaskBar];
    end
    else
    begin
      if tvRestoreDbClick in Visibility then
        Visibility := Visibility + [tvVisibleTaskBar];
    end;
  end;
end;

procedure TJvTrayIcon.SetIconIndex(const Value: Integer);
begin
  if FIconIndex <> Value then
  begin
    FIconIndex := Value;
    IconPropertyChanged;
  end;
end;

procedure TJvTrayIcon.SetVisibility(const Value: TTrayVisibilities);
begin
  FVisibility := Value;
  VisibleInTaskList := tvVisibleTaskList in Value;
  ApplicationVisible := tvVisibleTaskBar in Value;
  SetActive(FActive);
  SetAnimated(FAnimated);
end;

function TJvTrayIcon.ApplicationHook(var Msg: TMessage): Boolean;
begin
  if (Msg.Msg = WM_SYSCOMMAND) and (Msg.WParam = SC_MINIMIZE) and
    (tvAutoHide in Visibility) and Active then
    ApplicationVisible := False;
  Result := False;
end;

procedure TJvTrayIcon.Hook;
begin
  if FHooked or (csDesigning in ComponentState) then
    Exit;

  FHooked := True;

  Application.HookMainWindow(ApplicationHook);
end;

procedure TJvTrayIcon.UnHook;
begin
  if not FHooked then
    Exit;

  FHooked := False;

  Application.UnHookMainWindow(ApplicationHook);
end;

function TJvTrayIcon.NotifyIcon(dwMessage: DWORD): boolean;
begin
  Result := Shell_NotifyIcon(dwMessage, @FIconData);

{  if FActive and (tvAutoHideIcon in Visibility) then
  begin
    if Application.MainForm.Visible then
      Result := Shell_NotifyIcon(NIM_DELETE,@FIconData)
    else
      Result := Shell_NotifyIcon(NIM_ADD,@FIconData);
  end;
  }
end;

procedure TJvTrayIcon.ActivePropertyChanged;
begin
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/Structures/NOTIFYICONDATA.asp
  if not (csLoading in ComponentState) then
  begin
    if Active then
      Hook
    else
      UnHook;

    if Active and ((not (csDesigning in ComponentState)) or (tvVisibleDesign in Visibility)) then
    begin
      FOldTray := FindWindow('Shell_TrayWnd', nil);

      {HEG: Removed obsolete code
      if (FIcon = nil) or FIcon.Empty then
      // (rb) This triggers the IconChanged event, note that Active is loaded
      //before Icon from the stream, thus FIcon is always empty at this
      //point when loaded from the dfm stream }
      //  FIcon.Assign(Application.Icon);}
      with FIconData do
      begin
        if AcceptBalloons then
        begin
          cbSize := SizeOf(FIconData);
          FIconData.uTimeOut := NOTIFYICON_VERSION;
        end
        else
          cbSize := SizeOf(TNotifyIconData);
        Wnd := FHandle;
        uId := 1;
        uCallBackMessage := WM_CALLBACKMESSAGE;
        if CurrentIcon <> nil then
          hIcon := CurrentIcon.Handle
        else
          CurrentIcon := Application.Icon;
        StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
        uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
      end;
      if not ((tvAutoHideIcon in Visibility) and (Application.MainForm <> nil) and Application.MainForm.Visible) then
        NotifyIcon(NIM_ADD);
      if AcceptBalloons then
        NotifyIcon(NIM_SETVERSION);
    end
    else
      NotifyIcon(NIM_DELETE);
  end;
end;

procedure TJvTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = DropDownMenu then
      DropDownMenu := nil;
    if AComponent = PopupMenu then
      PopupMenu := nil;
    if AComponent = Icons then
      Icons := nil;
  end;
end;
end.

