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
  Hans-Eric Grönlund
  Vlad S

Last Modified: 2004-03-23

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
  Menus, ShellAPI, ImgList,
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

  { (rb) Change tvVisibleTaskBar to tvStartHidden or something; tvVisibleTaskBar
         is mainly used to indicate whether the application should start hidden
         with a trayicon; Functionality of tvVisibleTaskBar is available at
         run-time by using ShowApplication/HideApplication, at design-time it has
         no use, except to indicate whether the application should start hidden.
         Did not do the change because it changes the functionality of
         the trayicon, and could not come up with a backwards compatible way
         right-away }
  TTrayVisibility = (tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide, tvAutoHideIcon, tvVisibleDesign,
    tvRestoreClick, tvRestoreDbClick, tvMinimizeClick, tvMinimizeDbClick);
  TTrayVisibilities = set of TTrayVisibility;

  TJvTrayIconState = (tisTrayIconVisible, tisAnimating, tisHooked, tisHintChanged,
    tisWaitingForDoubleClick, tisAppHiddenButNotMinimized);
  TJvTrayIconStates = set of TJvTrayIconState;

  TJvTrayIcon = class(TJvComponent)
  private
    FTaskbarRestartMsg: Cardinal;
    FCurrentIcon: TIcon;
    FState: TJvTrayIconStates;
    FStreamedActive: Boolean;

    function GetApplicationVisible: Boolean;
    procedure SetApplicationVisible(const Value: Boolean);
  protected
    FActive: Boolean;
    FIcon: TIcon;
    FIconData: TNotifyIconDataXP;
    FHandle: THandle;
    FHint: string;
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
    FClickedButton: TMouseButton;
    FClickedShift: TShiftState;
    FClickedX: Integer;
    FClickedY: Integer;
    { Vlad S end.}

    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FAnimated: Boolean;
    FDelay: Cardinal;
    FIcons: TCustomImageList;
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
    FOnAnimate: TAnimateEvent;
    FVisibility: TTrayVisibilities;
    FSnap: Boolean;
    function GetSystemMinimumBalloonDelay: Cardinal;
    procedure DoAnimation;
    procedure DoCloseBalloon;
    procedure DoTimerDblClick; { Vlad S}
    procedure IconChanged(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetAnimated(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetHint(Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetIconIndex(const Value: Integer);
    procedure SetIcons(const Value: TCustomImageList);
    procedure SetTask(const Value: Boolean);
    procedure SetVisibility(const Value: TTrayVisibilities);
    procedure StopTimer(ID: Integer);
    procedure Hook;
    procedure Unhook;
    procedure WndProc(var Mesg: TMessage);
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoDoubleClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function ApplicationHook(var Msg: TMessage): Boolean;
    function NotifyIcon(uFlags: UINT; dwMessage: DWORD): Boolean;
    procedure SetCurrentIcon(Value: TIcon); //HEG: New
    procedure IconPropertyChanged; //HEG: New
    procedure Loaded; override; //HEG: New

    procedure InitIconData;

    procedure ShowTrayIcon;
    procedure HideTrayIcon;

    procedure StartAnimation;
    procedure EndAnimation;

    property ApplicationVisible: Boolean read GetApplicationVisible write SetApplicationVisible;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentIcon: TIcon read FCurrentIcon write SetCurrentIcon;
    procedure HideApplication;
    procedure ShowApplication;
    procedure BalloonHint(Title, Value: string; BalloonType:
      TBalloonType = btNone; ADelay: Cardinal = 5000; CancelPrevious: Boolean = False);
    function AcceptBalloons: Boolean;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Icon: TIcon read FIcon write SetIcon;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property Icons: TCustomImageList read FIcons write SetIcons;
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
  AnimationTimer = 1;
  CloseBalloonTimer = 2;
  DblClickTimer = 3;

  WM_CALLBACKMESSAGE = WM_USER + 1;

  // WIN32_IE >= = $0500
  NIN_SELECT          = (WM_USER + 0);
  NINF_KEY            = $1;
  NIN_KEYSELECT       = (NIN_SELECT or NINF_KEY);

  // WIN32_IE >= = $0501
  NIN_BALLOONSHOW     = (WM_USER + 2);
  NIN_BALLOONHIDE     = (WM_USER + 3);
  NIN_BALLOONTIMEOUT  = (WM_USER + 4);
  NIN_BALLOONUSERCLICK = (WM_USER + 5);

  NIM_ADD         = $00000000;
  NIM_MODIFY      = $00000001;
  NIM_DELETE      = $00000002;
  // WIN32_IE >= = $0500
  NIM_SETFOCUS    = $00000003;
  NIM_SETVERSION  = $00000004;
  NOTIFYICON_VERSION = 3;

  NIF_MESSAGE     = $00000001;
  NIF_ICON        = $00000002;
  NIF_TIP         = $00000004;
  // WIN32_IE >= = $0500
  NIF_STATE       = $00000008;
  NIF_INFO        = $00000010;
  // WIN32_IE >= = $600
  NIF_GUID        = $00000020;

  // WIN32_IE >= = $0500
  NIS_HIDDEN              = $00000001;
  NIS_SHAREDICON          = $00000002;

  // says this is the source of a shared icon

  // Notify Icon Infotip flags
  NIIF_NONE       = $00000000;
  // icon flags are mutually exclusive
  // and take only the lowest 2 bits
  NIIF_INFO       = $00000001;
  NIIF_WARNING    = $00000002;
  NIIF_ERROR      = $00000003;
  NIIF_ICON_MASK  = $0000000F;
  // WIN32_IE >= = $0501
  NIIF_NOSOUND    = $00000010;

{$IFNDEF COMPILER6_UP}
function SecondsBetween(const Now: TDateTime; const FTime: TDateTime): Integer;
begin
  Result := Trunc(86400 * (FTime - Now));
end;
{$ENDIF COMPILER6_UP}

function IsApplicationMinimized: Boolean;
begin
  Result := IsIconic(Application.Handle);
end;

//=== TJvTrayIcon ============================================================

function TJvTrayIcon.AcceptBalloons: Boolean;
begin
  // Balloons are only accepted with shell32.dll 5.0+
  Result := GetShellVersion >= $00050000;
end;

function TJvTrayIcon.ApplicationHook(var Msg: TMessage): Boolean;
begin
  if (Msg.Msg = WM_SYSCOMMAND) and (Msg.WParam = SC_MINIMIZE) and
    (tvAutoHide in Visibility) and Active then

    HideApplication;

  Result := False;
end;

procedure TJvTrayIcon.BalloonHint(Title, Value: string;
  BalloonType: TBalloonType; ADelay: Cardinal; CancelPrevious: Boolean);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/reference/functions/shell_notifyicon.asp
const
  cInfoFlagValues: array [TBalloonType] of DWORD =
    (NIIF_NONE, NIIF_ERROR, NIIF_INFO, NIIF_WARNING);
begin
  if AcceptBalloons then
  begin
    FTime := Now;
    FTimeDelay := ADelay div 1000;

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

    FIconData.uTimeOut := ADelay;
    FIconData.dwInfoFlags := cInfoFlagValues[BalloonType];

    NotifyIcon(NIF_INFO, NIM_MODIFY);

    // if the delay is less than the system's minimum and the balloon
    // was really shown (title and value are not empty)
    if (ADelay < GetSystemMinimumBalloonDelay) and (Title <> '') and (Value <> '') then
    begin
      // then we enable the ballon closer timer which will cancel
      // the balloon when the delay is elapsed
      SetTimer(FHandle, CloseBalloonTimer, ADelay, nil);
    end;

    if Assigned(FOnBalloonShow) then
      FOnBalloonShow(Self);
  end;
end;

constructor TJvTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FCurrentIcon := TIcon.Create;
  FSnap := False;
  FHandle := AllocateHWndEx(WndProc);

  FState := [];
  FVisibility := [tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide];
  FAnimated := False;
  FDelay := 100;
  FIconIndex := 0;
  FActive := False;
  FTask := True;

  { (rb) todo: make global }
  if not (csDesigning in ComponentState) then
  begin
    FDllHandle := LoadLibrary('KERNEL32.DLL');
    if FDllHandle <> 0 then
      @FRegisterServiceProcess := GetProcAddress(FDllHandle, 'RegisterServiceProcess')
    else
      @FRegisterServiceProcess := nil;
  end;

  { (rb) todo: make global }
  FTaskbarRestartMsg := RegisterWindowMessage('TaskbarCreated');
end;

destructor TJvTrayIcon.Destroy;
begin
  StopTimer(DblClickTimer); { Vlad S}
  StopTimer(CloseBalloonTimer);

  SetActive(False);

  if not (csDestroying in Application.ComponentState) then
    SetTask(False);

  FIcon.Free;
  FCurrentIcon.Free;
  DeallocateHWndEx(FHandle);

  { (rb) todo: make global }
  if not (csDesigning in ComponentState) then
    if FDllHandle <> 0 then
      FreeLibrary(FDllHandle);
  inherited Destroy;
end;

procedure TJvTrayIcon.DoAnimation;
begin
  if (tisTrayIconVisible in FState) and (FIcons <> nil) and (FIcons.Count > 0) then
  begin
    if IconIndex < 0 then
      IconIndex := 0
    else
      IconIndex := (IconIndex + 1) mod FIcons.Count;
    if Assigned(FOnAnimate) then
      FOnAnimate(Self, IconIndex);
  end;
end;

procedure TJvTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    if FDropDownMenu <> nil then
    begin
      SetForegroundWindow(FHandle);
      FDropDownMenu.Popup(X, Y);
      PostMessage(FHandle, WM_NULL, 0, 0);
    end;
    if IsApplicationMinimized or (tisAppHiddenButNotMinimized in FState) then
    begin
      if tvRestoreClick in Visibility then
        ShowApplication;
    end
    else
    if tvMinimizeClick in Visibility then
      { (rb) Call Application.Minimize instead of HideApplication
             if tvAutoHide not in Visibility ? }
      HideApplication;
  end
  else if (Button = mbRight) and (FPopupMenu <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FPopupMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
  end
end;

procedure TJvTrayIcon.DoCloseBalloon;
begin
  StopTimer(CloseBalloonTimer);

  // We call BalloonHint with title and info set to
  // empty strings which surprisingly will cancel any existing
  // balloon for the icon. This is clearly not documented by
  // microsoft and may not work in later releases of Windows
  // it has been tested on XP Home French
  BalloonHint('', '');
end;

procedure TJvTrayIcon.DoDoubleClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if tisWaitingForDoubleClick in FState then
  begin
    Exclude(FState, tisWaitingForDoubleClick); { Vlad S}
    StopTimer(DblClickTimer); { Vlad S}
  end;

  if Assigned(FOnDblClick) then
    FOnDblClick(Self, Button, Shift, X, Y)
  else if Button = mbLeft then
  begin
    if FPopupMenu <> nil then
      for I := 0 to FPopupMenu.Items.Count - 1 do
        if FPopupMenu.Items[I].Default then
        begin
          FPopupMenu.Items[I].Click;
          Break;
        end;
    if IsApplicationMinimized or (tisAppHiddenButNotMinimized in FState) then
    begin
      if tvRestoreDbClick in Visibility then
        ShowApplication;
    end
    else
    if tvMinimizeDbClick in Visibility then
      { (rb) Call Application.Minimize instead of HideApplication
             if tvAutoHide in Visibility ? }
      HideApplication;
  end;
end;

procedure TJvTrayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FClickedButton := Button;
  FClickedShift := Shift;
  FClickedX := X;
  FClickedY := Y;

  if not (tisWaitingForDoubleClick in FState) then
  begin
    Include(FState, tisWaitingForDoubleClick);

    SetTimer(FHandle, DblClickTimer, GetDoubleClickTime, nil);
  end;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvTrayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if tisHintChanged in FState then
  begin
    Exclude(FState, tisHintChanged);

    with FIconData do
      StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);

    if tisTrayIconVisible in FState then
      NotifyIcon(NIF_TIP, NIM_MODIFY);
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TJvTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TJvTrayIcon.DoTimerDblClick;
begin
  StopTimer(DblClickTimer);

  if tisWaitingForDoubleClick in FState then
  begin
    Exclude(FState, tisWaitingForDoubleClick);
    // Double-clicking a mouse button actually generates four messages:
    // WM_XBUTTONDOWN, WM_XBUTTONUP, WM_XBUTTONDBLCLK, and WM_XBUTTONUP again
    DoClick(FClickedButton, FClickedShift, FClickedX, FClickedY);
  end;
end;

procedure TJvTrayIcon.EndAnimation;
begin
  // reentrance check
  if tisAnimating in FState then
  begin
    Exclude(FState, tisAnimating);

    StopTimer(AnimationTimer);
  end;
end;

function TJvTrayIcon.GetApplicationVisible: Boolean;
begin
  Result := not IsApplicationMinimized;
end;

function TJvTrayIcon.GetSystemMinimumBalloonDelay: Cardinal;
begin
  // from Microsoft's documentation, a balloon is shown for at
  // least 10 seconds, but it is a system settings which must
  // be somewhere in the registry. The only question is : Where ?
  Result := 10000;
end;

procedure TJvTrayIcon.HideApplication;
begin
  if not IsApplicationMinimized and not (tisAppHiddenButNotMinimized in FState) then
  begin
    // Minimize the application..
    if Snap then
    begin
      if Assigned(Application.MainForm) then
        Application.MainForm.Visible := False;
      Application.ShowMainForm := False;
    end;
    Application.Minimize;
  end;

  // ..and hide the taskbar button of the application
  ShowWindow(Application.Handle, SW_HIDE);
  Exclude(FVisibility, tvVisibleTaskBar);

  if tvAutoHideIcon in Visibility then
    ShowTrayIcon;
end;

procedure TJvTrayIcon.HideTrayIcon;
begin
  // reentrance check
  if tisTrayIconVisible in FState then
  begin
    Exclude(FState, tisTrayIconVisible);

    EndAnimation;

    NotifyIcon(0, NIM_DELETE);
  end;
end;

procedure TJvTrayIcon.Hook;
begin
  // reentrance check; also no hooking while designing
  if (tisHooked in FState) or (csDesigning in ComponentState) then
    Exit;

  Include(FState, tisHooked);

  Application.HookMainWindow(ApplicationHook);
end;

procedure TJvTrayIcon.IconChanged(Sender: TObject);
begin
  IconPropertyChanged;
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
        FIcons.GetIcon(FIconIndex, Ico);
        SetCurrentIcon(Ico);
      finally
        Ico.Free;
      end;
    end
    else if Assigned(Icon) and (not Icon.Empty) then
      SetCurrentIcon(Icon)
    else
      SetCurrentIcon(Application.Icon);
  end;
end;

procedure TJvTrayIcon.InitIconData;
begin
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/Structures/NOTIFYICONDATA.asp
  with FIconData do
  begin
    if AcceptBalloons then
    begin
      cbSize := SizeOf(FIconData);
      { (rb) todo: if AcceptBalloons then win2000 or up ?? }
      FIconData.uTimeOut := NOTIFYICON_VERSION;
    end
    else
      cbSize := SizeOf(TNotifyIconData);
    Wnd := FHandle;
    uId := 1;
    uCallBackMessage := WM_CALLBACKMESSAGE;
    if not CurrentIcon.Empty then
      hIcon := CurrentIcon.Handle
    else
      CurrentIcon := Application.Icon;
    StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    uFlags := 0;
  end;
end;

procedure TJvTrayIcon.Loaded;
begin
  inherited Loaded;

  IconPropertyChanged;

  if FStreamedActive then
  begin
    SetActive(True);

    if not (tvVisibleTaskBar in Visibility) then
    begin
      // Start hidden 
      Application.ShowMainForm := False;
      // Note that the application is not really minimized
      // (ie IsIconic(Application.Handle) = False), just hidden.
      // Calling Application.Minimize or something would show the
      // application's button on the taskbar for a short time.
      // So we use the tisHiddenNotMinized flag as work-around, to indicate that
      // the application is minimized.

      ShowWindow(Application.Handle, SW_HIDE);

      Include(FState, tisAppHiddenButNotMinimized);
    end;
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

function TJvTrayIcon.NotifyIcon(uFlags: UINT; dwMessage: DWORD): Boolean;
begin
  FIconData.uFlags := uFlags;

  Result := Shell_NotifyIcon(dwMessage, @FIconData);
end;

procedure TJvTrayIcon.SetActive(Value: Boolean);
begin
  if csLoading in ComponentState then
    FStreamedActive := Value
  else
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
    begin
      InitIconData;

      if (csDesigning in ComponentState) and not (tvVisibleDesign in Visibility) then
        Exit;

      Hook;

      ShowTrayIcon;

      if AcceptBalloons then
        NotifyIcon(0, NIM_SETVERSION);
    end
    else
    begin
      EndAnimation;

      Unhook;

      HideTrayIcon;
    end;
  end;
end;

procedure TJvTrayIcon.SetAnimated(const Value: Boolean);
begin
  if Value <> FAnimated then
  begin
    FAnimated := Value;
    if FAnimated then
      StartAnimation
    else
      EndAnimation;
  end;
end;

procedure TJvTrayIcon.SetApplicationVisible(const Value: Boolean);
begin
  if Value then
    ShowApplication
  else
    HideApplication;
end;

//HEG: New

procedure TJvTrayIcon.SetCurrentIcon(Value: TIcon);
begin
  FCurrentIcon.Assign(Value);
  FIconData.hIcon := FCurrentIcon.Handle;
  if tisTrayIconVisible in FState then
    //    if FIconData.hIcon = 0 then
    //      HideTrayIcon
    //    else
    NotifyIcon(NIF_ICON, NIM_MODIFY);
end;

procedure TJvTrayIcon.SetDelay(const Value: Cardinal);
var
  WasAnimated: Boolean;
begin
  if FDelay <> Value then
  begin
    WasAnimated := Animated;
    try
      Animated := False;
      FDelay := Value;
    finally
      Animated := WasAnimated;
    end;
  end;
end;

procedure TJvTrayIcon.SetHint(Value: string);
begin
  //Remove sLineBreak on w98/me as they are not supported
  if not AcceptBalloons then
    Value := StringReplace(Value, sLineBreak, ' - ', [rfReplaceAll]);

  if FHint <> Value then
  begin
    { (rb) No idea why this isn't applied immediately }
    Include(FState, tisHintChanged);
    FHint := Value;
  end;
end;

procedure TJvTrayIcon.SetIcon(Value: TIcon);
begin
  // triggers IconPropertyChanged
  FIcon.Assign(Value);
end;

procedure TJvTrayIcon.SetIconIndex(const Value: Integer);
begin
  if FIconIndex <> Value then
  begin
    FIconIndex := Value;
    IconPropertyChanged;
  end;
end;

procedure TJvTrayIcon.SetIcons(const Value: TCustomImageList);
begin
  if FIcons <> Value then
  begin
    FIcons := Value;
    IconPropertyChanged; //HEG: New
  end;
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

procedure TJvTrayIcon.SetVisibility(const Value: TTrayVisibilities);
var
  ToBeSet, ToBeCleared: TTrayVisibilities;
begin
  if Value <> FVisibility then
  begin
    ToBeSet := Value - FVisibility;
    ToBeCleared := FVisibility - Value;
    FVisibility := Value;

    if not Active then
      Exit;

    if csDesigning in ComponentState then
    begin
      if tvVisibleDesign in ToBeSet then
        ShowTrayIcon
      else
      if tvVisibleDesign in ToBeCleared then
        HideTrayIcon;
    end
    else
    begin
      if tvAutoHide in ToBeSet then
      begin
        if IsApplicationMinimized then
          HideApplication;
      end;

      if tvVisibleTaskBar in ToBeSet then
        ShowApplication
      else
      if tvVisibleTaskBar in ToBeCleared then
        HideApplication;

      if (tvAutoHideIcon in ToBeSet) and not IsApplicationMinimized then
        HideTrayIcon;
      if (tvAutoHideIcon in ToBeCleared) and not IsApplicationMinimized then
        ShowTrayIcon;
    end;
  end;
end;

procedure TJvTrayIcon.ShowApplication;
begin
  if tisAppHiddenButNotMinimized in FState then
  begin
    Exclude(FState, tisAppHiddenButNotMinimized);

    Application.Minimize;
    Application.ShowMainForm := True;
  end;

  // Show the taskbar button of the application..
  Include(FVisibility, tvVisibleTaskBar);
  ShowWindow(Application.Handle, SW_SHOW);

  // ..and restore the application
  Application.Restore;

  if Application.MainForm <> nil then
    Application.MainForm.Visible := True;
  if tvAutoHideIcon in Visibility then
    HideTrayIcon;
end;

procedure TJvTrayIcon.ShowTrayIcon;
begin
  // reentrance check
  if tisTrayIconVisible in FState then
    Exit;

  if not Active then
    Exit;

  if csDesigning in ComponentState then
  begin
    if not (tvVisibleDesign in Visibility) then
      Exit;
  end
  else
  if (tvAutoHideIcon in Visibility) and not IsApplicationMinimized then
    Exit;

  // All checks passed, make the trayicon visible:

  Include(FState, tisTrayIconVisible);

  NotifyIcon(NIF_MESSAGE or NIF_ICON or NIF_TIP, NIM_ADD);

  if Animated then
    StartAnimation;
end;

procedure TJvTrayIcon.StartAnimation;
begin
  // reentrance check, and trayicon needs to be visible
  if [tisAnimating, tisTrayIconVisible] * FState = [tisTrayIconVisible] then
  begin
    Include(FState, tisAnimating);

    SetTimer(FHandle, AnimationTimer, FDelay, nil)
  end;
end;

procedure TJvTrayIcon.StopTimer(ID: Integer);
begin
  if FHandle <> 0 then
    KillTimer(FHandle, ID);
end;

procedure TJvTrayIcon.Unhook;
begin
  // reentrance check
  if tisHooked in FState then
  begin
    Exclude(FState, tisHooked);

    Application.UnhookMainWindow(ApplicationHook);
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
          if not (csDesigning in ComponentState) then
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
                  { (rb) Double try..except }
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
                  { (rb) Double try..except }
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
            HideTrayIcon;
            //if FActive then
            //begin
            //  SetActive(False);
            //  FActive := True;
            //end
            //else
            //begin
            //  { (rb) Does not work anymore }
            //  SetActive(False);
            //  FActive := False;
            //end;
          end;
        WM_TIMER:
          case TWMTimer(Mesg).TimerID of
            AnimationTimer:
              DoAnimation;
            CloseBalloonTimer:
              DoCloseBalloon;
            DblClickTimer:
              DoTimerDblClick;
          end;
      else
        if Msg = FTaskbarRestartMsg then
        begin
          if tisTrayIconVisible in FState then
          begin
            { You can test this on XP:
                - Click Start, then click Turn Off Computer
                - Press CTRL + Shift + Alt + Click Cancel          (all at once)
                    this will dump explorer.exe
                - Press CTRL + Alt + Delete
                - Click New Task...
                - Enter 'explorer.exe' and click OK.
                    this will restart explorer.exe
            }
            // Ensure tisTrayIconVisible is not in FState:
            HideTrayIcon;
            ShowTrayIcon;
          end;
        end
        else
          Result := DefWindowProc(FHandle, Msg, wParam, lParam);
      end; // case
  except
    Application.HandleException(Self);
  end;
end;

end.

