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

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2002-06-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvTrayIcon;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  Menus, ShellApi,
  {$IFDEF COMPILER6_UP}
  DateUtils,
  {$ENDIF}
  JvTypes, JvComponent;

type
  TBalloonType = (btNone, btError, btInfo, btWarning);
  TNotifyIconDataXP = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of AnsiChar;
    uTimeOut: DWORD;
    szInfoTitle: array [0..63] of AnsiChar;
    dwInfoFlags: DWORD;
  end;

  TAnimateEvent = procedure(Sender: TObject; const ImageIndex: Integer) of object;
  TRegisterServiceProcess = function(dwProcessID, dwType: Integer): Integer; stdcall;

  TTrayVisibility = (tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide, tvVisibleDesign,
    tvRestoreClick, tvRestoreDbClick, tvMinimizeClick, tvMinimizeDbClick);
  TTrayVisibilities = set of TTrayVisibility;

  TJvTrayIcon = class(TJvComponent)
  private
    FActive: Boolean;
    FIcon: TIcon;
    FIc: TNotifyIconDataXP;
    FHandle: THandle;
    FHint: string;
    FBalloonCloser : TTimer;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FApplicationVisible: Boolean; { Used??}
    FAnimated: Boolean;
    FDelay: Cardinal;
    FImgList: TImageList;
    FTimer: TTimer;
    FNumber: Integer;
    FDropDown: TPopupMenu;
    FTask: Boolean;
    FRegisterServiceProcess: TRegisterServiceProcess;
    FDllHandle: THandle;
    FOnBalloonHide: TNotifyEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonClick: TNotifyEvent;
    FOnHint : TNotifyEvent;
    FTime: TDateTime;
    FTimeDelay: Integer;
    FOldTray: HWND;
    FOnAnimate: TAnimateEvent;
    FVisibility: TTrayVisibilities;
    FOldHint: string;
    FSnap: Boolean;
    FHooked: Boolean;

    function  GetSystemMinimumBalloonDelay : integer;
    procedure OnAnimateTimer(Sender: TObject);
    procedure OnBalloonCloserTimer(Sender: TObject);
    procedure IconChanged(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIcon(Icon: TIcon);
    procedure SetApplicationVisible(Value: Boolean);
    procedure SetAnimated(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetImgList(const Value: TImageList);
    procedure SetTask(const Value: Boolean);
    procedure SetNumber(const Value: Integer);
    procedure SetVisibility(const Value: TTrayVisibilities);
  protected
    procedure Hook;
    procedure UnHook;
    procedure WndProc(var Mesg: TMessage);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDoubleClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function ApplicationHook(var Msg: TMessage): Boolean;
    property ApplicationVisible: Boolean read FApplicationVisible write SetApplicationVisible default True;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCheckCrash;
    procedure HideApplication;
    procedure ShowApplication;
    procedure BalloonHint(Title, Value: string; BalloonType:
      TBalloonType = btNone; Delay: Integer = 5000; CancelPrevious :boolean=false);
    function AcceptBalloons: Boolean;
  published
    { (rb) Active should be set in Loaded; Icon isn't set when Active is now
      set to True etc. }
    property Active: Boolean read FActive write SetActive default False;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Icon: TIcon read FIcon write SetIcon;
    property IconIndex: Integer read FNumber write SetNumber;
    property Icons: TImageList read FImgList write SetImgList;
    property Hint: string read FHint write SetHint;
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
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
    property OnHint : TNotifyEvent read FOnHint write FOnHint;
  end;

implementation

uses
  JvFunctions;

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
  FApplicationVisible := True;
  FSnap := False;
  {$IFDEF COMPILER6_UP}
  FHandle := Classes.AllocateHWnd(WndProc);
  {$ELSE}
  FHandle := AllocateHWnd(WndProc);
  {$ENDIF}

  FVisibility := [tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide];
  FAnimated := False;
  FDelay := 100;
  FNumber := 0;
  FActive := False;
  FTask := True;

  FBalloonCloser := TTimer.Create(self);
  FBalloonCloser.Enabled := false;
  FBalloonCloser.OnTimer := OnBalloonCloserTimer;

  if not (csDesigning in ComponentState) then
  begin
    FDllHandle := LoadLibrary('KERNEL32.DLL');
    if FDllHandle <> 0 then
      @FRegisterServiceProcess := GetProcAddress(FDllHandle, 'RegisterServiceProcess')
    else
      @FRegisterServiceProcess := nil;
  end;
end;

destructor TJvTrayIcon.Destroy;
begin
  UnHook;
  if not (csDestroying in Application.ComponentState) then
    SetTask(False);

  FTimer.Free;
  SetActive(False);
  FBalloonCloser.Enabled := false;
  FBalloonCloser.Free;
  FIcon.Free;
  {$IFDEF COMPILER6_UP}
  Classes.DeallocateHWnd(FHandle);
  {$ELSE}
  DeallocateHWnd(FHandle);
  {$ENDIF}

  if not (csDesigning in ComponentState) then
    if FDllHandle <> 0 then
      FreeLibrary(FDllHandle);
  inherited Destroy;
end;

function TJvTrayIcon.AcceptBalloons: Boolean;
begin
  //Balloons are only accepted with shell32.dll 5.0+
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
      if Msg = WM_CALLBACKMESSAGE then
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
              Result := Integer(True);
            end;
          NIN_BALLOONTIMEOUT: //sb
            begin
              I := SecondsBetween(Now, FTime);
              if I > FTimeDelay then
                BalloonHint('', '');
              Result := Integer(True);
            end;
          NIN_BALLOONUSERCLICK: //sb
            begin
              try
                if Assigned(FOnBalloonClick) then
                  FOnBalloonClick(self);
              except
              end;
              Result := Integer(True);
              //Result := DefWindowProc(FHandle, Msg, wParam, lParam);
              BalloonHint('', '');
            end;
        end;
      end
      else
        Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

procedure TJvTrayIcon.IconChanged(Sender: TObject);
begin
  DoCheckCrash;
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, PNotifyIconData(@FIc));
end;

procedure TJvTrayIcon.SetHint(Value: string);
begin
  DoCheckCrash;
  //Remove sLineBreak on w98/me as they are not supported
  if not AcceptBalloons then
    FHint := StringReplace(Value, CrLf, ' - ', [rfReplaceAll])
  else
    FHint := Value;
end;

procedure TJvTrayIcon.SetIcon(Icon: TIcon);
begin
  DoCheckCrash;
  FIcon.Assign(ICon);
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, PNotifyIconData(@FIc));
end;

procedure TJvTrayIcon.SetActive(Value: Boolean);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/Structures/NOTIFYICONDATA.asp
begin
  if Value then
    Hook
  else
    UnHook;

  if Value and ((not (csDesigning in ComponentState)) or (tvVisibleDesign in Visibility)) then
  begin
    FOldTray := FindWindow('Shell_TrayWnd', nil);

    if (FIcon = nil) or FIcon.Empty then
      { (rb) This triggers the IconChanged event, note that Active is loaded
        before Icon from the stream, thus FIcon is always empty at this
        point when loaded from the dfm stream }
      FIcon.Assign(Application.Icon);
    with FIc do
    begin
      if AcceptBalloons then
      begin
        cbSize := SizeOf(FIc);
        FIc.uTimeOut := NOTIFYICON_VERSION;
      end
      else
        cbSize := SizeOf(TNotifyIconData);
      Wnd := FHandle;
      uId := 1;
      uCallBackMessage := WM_CALLBACKMESSAGE;
      if FIcon <> nil then
        hIcon := FIcon.Handle
      else
        FIcon := Application.Icon;
      StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
      uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
    end;
    Shell_NotifyIcon(NIM_ADD, PNotifyIconData(@FIc));
    if AcceptBalloons then
      Shell_NotifyIcon(NIM_SETVERSION, PNotifyIconData(@FIc));
  end
  else
    Shell_NotifyIcon(NIM_DELETE, PNotifyIconData(@FIc));

  FActive := Value;
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
end;

procedure TJvTrayIcon.ShowApplication;
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Application.Restore;
  if Application.MainForm <> nil then
    Application.MainForm.Visible := True;
end;

procedure TJvTrayIcon.OnAnimateTimer(Sender: TObject);
begin
  if (FActive) and (FImgList <> nil) then
  begin
    if IconIndex < 0 then
      IconIndex := 0
    else
      IconIndex := (IconIndex + 1) mod FimgList.Count;
    if Assigned(FOnAnimate) then
      FOnAnimate(self, IconIndex);
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
      if FTimer <> nil then
    begin
      FTimer.Free;
      FTimer := nil;
    end;
  end;
end;

procedure TJvTrayIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  if FTimer <> nil then
    FTimer.Interval := FDelay;
end;

procedure TJvTrayIcon.SetImgList(const Value: TImageList);
begin
  FImgList := Value;
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
  BalloonHint('','');
end;

procedure TJvTrayIcon.BalloonHint(Title, Value: string;
  BalloonType: TBalloonType; Delay: Integer; CancelPrevious : boolean);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/reference/functions/shell_notifyicon.asp
begin
  if AcceptBalloons then
  begin
    FTime := Now;
    FTimeDelay := Delay div 1000;
    FIc.uFlags := NIF_INFO;

    // if we must cancel an existing balloon
    if CancelPrevious then
    begin
      // then we call BalloonHint with title and info set to
      // empty strings which surprisingly will cancel any existing
      // balloon for the icon. This is clearly not documented by
      // microsoft and may not work in later releases of Windows
      // it has been tested on XP Home French
      BalloonHint('','');
    end;

    with FIc do
      StrPLCopy(szInfoTitle, Title, SizeOf(szInfoTitle) - 1);
    with FIc do
      StrPLCopy(szInfo, Value, SizeOf(szInfo) - 1);
    FIc.uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
    FIc.uTimeOut := Delay;
    case BalloonType of
      btError:
        FIc.dwInfoFlags := NIIF_ERROR;
      btInfo:
        FIc.dwInfoFlags := NIIF_INFO;
      btNone:
        FIc.dwInfoFlags := NIIF_NONE;
      btWarning:
        FIc.dwInfoFlags := NIIF_WARNING;
    end;
    Shell_NotifyIcon(NIM_MODIFY, PNotifyIconData(@FIc));

    // if the delay is less than the system's minimum and the balloon
    // was really shown (title and value are not empty)
    if (Delay < GetSystemMinimumBalloonDelay) and
        (Title <> '') and
        (Value <> '') then
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
    with FIc do
      StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    FIc.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    if FActive then
      Shell_NotifyIcon(NIM_MODIFY, PNotifyIconData(@FIc));
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
  if Assigned(FOnHint) then
    FOnHint(Self);
end;

procedure TJvTrayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if (Button = mbLeft) and (FDropDown <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FDropDown.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
  end;
end;

procedure TJvTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and (FPopupMenu <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FPopupMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
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
  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, X, Y);
end;

procedure TJvTrayIcon.DoDoubleClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
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

procedure TJvTrayIcon.SetNumber(const Value: Integer);
var
  Ico: TIcon;
begin
  FNumber := Value;
  if (FImglist <> nil) and (FNumber >= 0) and (FNumber < FImgList.Count) then
  begin
    Ico := TIcon.Create;
    try
      FIc.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      FImgList.GetIcon(FNumber, Ico);
      SetIcon(Ico);
    finally
      Ico.Free;
    end;
  end
  else
    FNumber := -1;
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

end.

