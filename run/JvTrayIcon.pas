{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTrayIcon.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is S�bastien Buysse [sbuysse att buypin dott com]
Portions created by S�bastien Buysse are Copyright (C) 2001 S�bastien Buysse.
All Rights Reserved.

Contributor(s):
  Michael Beck [mbeck att bigfoot dott com].
  Feng Mingyu(Winston Feng), [winstonf att tom dott com]
  Hans-Eric Grnlund
  Vlad S

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

History:
  2004-03-23
     Added code to hide balloon correctly under W2k, as suggested by VladS
  2004-02-29
     VladS separate click and dblclick
  2003-09-28 by Winston Feng
    Add WM_SESSIONEND message handler, TaskbarRestart message handler to:
      Clean the trayicon when session ends.
      Restore the trayicon when session restart.
    Remove the old unsuccessful DoCheckCrash method.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvTrayIcon;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Menus, ShellAPI, ImgList, DateUtils,
  JvComponentBase;

type
  TBalloonType = (btNone, btError, btInfo, btWarning);

  TNotifyIconDataXP = record
    cbSize: DWORD;
    Wnd: THandle;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of Char; // 0..64 for pre 5.0 shell versions
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of Char;
    uTimeOut: DWORD;
    szInfoTitle: array [0..63] of Char;
    dwInfoFlags: DWORD;
  end;

  TAnimateEvent = procedure(Sender: TObject; const ImageIndex: Integer) of object;

  { (rb) Change tvVisibleTaskBar to tvStartHidden or something; tvVisibleTaskBar
         is mainly used to indicate whether the application should start hidden
         with a trayicon; Functionality of tvVisibleTaskBar is available at
         run-time by using ShowApplication/HideApplication, at design-time it has
         no use, except to indicate whether the application should start hidden.
         Did not do the change because it changes the functionality of
         the trayicon, and could not come up with a backwards compatible way
         right-away }
  TTrayVisibility = (tvVisibleTaskBar, tvVisibleTaskList, tvAutoHide, tvAutoHideIcon, tvVisibleDesign,
    tvRestoreClick, tvRestoreDbClick, tvMinimizeClick, tvMinimizeDbClick, tvAnimateToTray,
    tvNoRetryOnFailure);
  TTrayVisibilities = set of TTrayVisibility;

  TJvTrayIconState = (tisTrayIconVisible, tisAnimating, tisHooked, tisHintChanged,
    tisWaitingForDoubleClick, tisAppHiddenButNotMinimized, tisClicked);
  TJvTrayIconStates = set of TJvTrayIconState;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvTrayIcon = class(TJvComponent)
  private
    FCurrentIcon: TIcon;
    FState: TJvTrayIconStates;
    FStreamedActive: Boolean;

    function GetApplicationVisible: Boolean;
    procedure SetApplicationVisible(const Value: Boolean);
    function GetIconVisible: Boolean;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetIconVisible(const Value: Boolean);
    procedure SetPopupMenu(const Value: TPopupMenu);
  protected
    FActive: Boolean;
    FIcon: TIcon;
    FIconData: TNotifyIconDataXP;
    FHandle: THandle;
    FHint: string;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TMouseEvent;

    // Under Windows 2000, in order to hide a balloon hint, BalloonHint must be
    // called with empty strings as many times it was called with real messages.
    // So we keep a counter of the number of times ballon hint was called to
    // track this and be sure to call a the right number of times when trying
    // to hide the balloon
    FBalloonCount: Integer;

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
    FOnContextPopup: TContextPopupEvent;
    FAnimated: Boolean;
    FDelay: Cardinal;
    FIcons: TCustomImageList;
    FIconIndex: Integer;
    FDropDownMenu: TPopupMenu;
    FTask: Boolean;
    FOnBalloonHide: TNotifyEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonClick: TNotifyEvent;
    FTime: TDateTime;
    FTimeDelay: Integer;
    FOnAnimate: TAnimateEvent;
    FVisibility: TTrayVisibilities;
    FSnap: Boolean;
    FSwingDirectionState: Integer;
    FSwingForthAndBack: Boolean;
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
    procedure DoContextPopup(X, Y: Integer);
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

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CurrentIcon: TIcon read FCurrentIcon write SetCurrentIcon;
    procedure HideApplication; virtual;
    procedure ShowApplication; virtual;
    procedure BalloonHint(Title, Value: string; BalloonType:
      TBalloonType = btNone; ADelay: Cardinal = 5000; CancelPrevious: Boolean = False);
    function AcceptBalloons: Boolean;
    procedure HideBalloon;
    function GetIconRect(var IconRect: TRect): Boolean;

    property ApplicationVisible: Boolean read GetApplicationVisible write SetApplicationVisible;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;
    property IconVisible: Boolean read GetIconVisible write SetIconVisible;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Icon: TIcon read FIcon write SetIcon;
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property Icons: TCustomImageList read FIcons write SetIcons;
    property Hint: string read FHint write SetHint;
    property DropDownMenu: TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property Snap: Boolean read FSnap write FSnap default False;
    property SwingForthAndBack: Boolean read FSwingForthAndBack write FSwingForthAndBack default False;
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
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
  end;

procedure RefreshTray;

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
  JvJCLUtils, JvJVCLUtils, CommCtrl;

var
  WM_TaskbarRestartMsg: Cardinal;

type
  TRegisterServiceProcess = function(dwProcessID, dwType: Integer): Integer; stdcall;

  TExtraData = packed record
    Wnd: THandle;
    uID: UINT;
  end;

  TTrayIconEnumerator = class
  private
    FToolbarHandle: THandle;
    FProcess: THandle;
    FCount: Integer;
    FData: Pointer;
    FIndex: Integer;
    FButton: TTBButton;
    FExtraData: TExtraData;
    procedure Init(const DataSize: Integer);
  public
    constructor Create; overload;
    constructor Create(DataSize: Integer); overload;
    destructor Destroy; override;
    function MoveNext: Boolean;

    function ReadProcessMemory(const Address: Pointer; Count: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF}; var Buffer): Boolean;

    property CurrentButton: TTBButton read FButton;
    property CurrentWnd: THandle read FExtraData.Wnd;
    property CurrentID: UINT read FExtraData.uID;

    property ToolbarHandle: THandle read FToolbarHandle;
    property Index: Integer read FIndex;
    property Data: Pointer read FData;
  end;

const
  AnimationTimer = 1;
  CloseBalloonTimer = 2;
  DblClickTimer = 3;

  cTaskbarIconIdentifier = 1;

  // The hint size is 64 for pre IE 5.0 Shell32 versions, 128 for newer versions.
  cHintSize: array [Boolean] of Cardinal = (64 - 1, 128 - 1);  // -1 for trailing #0

  Shell32VersionIE5 = $00050000;

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

  RegisterServiceProcessName = 'RegisterServiceProcess';

var
  RegisterServiceProcess: TRegisterServiceProcess = nil;

{ We get the following messages while clicking:

  Shell version < 5.0                |  Shell version >= 5.0
                                     |
  Single click     Double click      |  Single click          Double click
                                     |
  WM_BUTTONDOWN    WM_BUTTONDOWN     |  WM_BUTTONDOWN         WM_BUTTONDOWN
  WM_BUTTONUP      WM_BUTTONUP       |  WM_BUTTONUP           WM_BUTTONUP
                   WM_BUTTONDBLCLK   |  WM_CONTEXTMENU (*)    WM_CONTEXTMENU (*)
                   WM_BUTTONUP       |                        WM_BUTTONDBLCLK
                                     |                        WM_BUTTONUP
                                     |                        WM_CONTEXTMENU (*)
  (*) if clicked with the right mouse button.

  o  We use the tisClicked flag to indicate that we received a WM_BUTTONDOWN;
     if we receive a WM_BUTTONUP we can then make a difference between button ups
     from double click and from single clicks. DoClick is thus not called twice
     for double clicks.
     (similar to csClicked flag in TControl.ControlState)
  o  Normal behaviour for window controls is to call both DoClick and DoDoubleClick
     when the user double clicks the control. For the tray icon we don't want that.
     We use the tisWaitingForDoubleClick flag to indicate that we received a
     WM_BUTTONDOWN and WM_BUTTONUP and thus want to call DoClick. But instead of
     calling DoClick we start a timer; if we receive a WM_BUTTONDBLCLK before the
     timer ends, the user double clicked the icon otherwise it was a single click.
  o  For Shell32.dll versions before 5.0 we call DoContextPopup in WM_BUTTONUP
     to simulate WM_CONTEXTMENU messages.

  Thus the result is:

  Shell version < 5.0                     |  Shell version >= 5.0
                                          |
  Single click         Double click       |  Single click         Double click
                                          |
  WM_BUTTONDOWN        WM_BUTTONDOWN      |  WM_BUTTONDOWN        WM_BUTTONDOWN
    OnMouseDown          OnMouseDown      |    OnMouseDown          OnMouseDown
  WM_BUTTONUP          WM_BUTTONUP        |  WM_BUTTONUP          WM_BUTTONUP
    Start Timer          Start Timer      |    Start Timer          Start Timer
    OnMouseUp            OnMouseUp        |    OnMouseUp            OnMouseUp
    OnContextPopup (*)   OnContextPopup (*)| WM_CONTEXTMENU (*)   WM_CONTEXTMENU (*)
  WM_TIMER             WM_BUTTONDBLCLK    |    OnContextPopup        OnContextPopup
    OnClick      (**)    Stop Timer       |  WM_TIMER             WM_BUTTONDBLCLK
                         OnDoubleClick    |    OnClick     (**)     Stop Timer
                       WM_BUTTONUP        |                         OnDoubleClick
                         OnMouseUp        |                       WM_BUTTONUP
                         OnContextPopup   |                         OnMouseUp
                                          |                       WM_CONTEXTMENU (*)
                                          |                         OnContextPopup

   (*) if clicked with the right mouse button.
  (**) OnClick comes after the OnMouseUp. Another design decision could
       be to also delay OnMouseUp.
}

function GetHandleOnTaskBar: THandle;
begin
  {$IFDEF COMPILER11_UP}
  if Application.MainFormOnTaskBar and Assigned(Application.MainForm) then
    Result := Application.MainForm.Handle
  else
  {$ENDIF COMPILER11_UP}
    Result := Application.Handle;
end;

function IsApplicationMinimized: Boolean;
begin
  Result := IsIconic(GetHandleOnTaskBar);
end;

function GetTrayHandle: THandle;
var
  ShellTrayHandle: THandle;
begin
  ShellTrayHandle := FindWindow('Shell_TrayWnd', nil);
  if ShellTrayHandle <> 0 then
    Result := FindWindowEx(ShellTrayHandle, 0, 'TrayNotifyWnd', nil)
  else
    Result := 0;
end;

procedure AnimateToTray(AHandle: THandle);
var
  SourceRect, DestRect: TRect;
  TrayHandle: THandle;
begin
  TrayHandle := GetTrayHandle;
  if TrayHandle <> 0 then
  begin
    GetWindowRect(AHandle, SourceRect);
    GetWindowRect(TrayHandle, DestRect);

    DrawAnimatedRects(AHandle, IDANI_CAPTION, SourceRect, DestRect);
  end;
end;

procedure AnimateFromTray(AHandle: THandle);
var
  SourceRect, DestRect: TRect;
  TrayHandle: THandle;
begin
  TrayHandle := GetTrayHandle;
  if TrayHandle <> 0 then
  begin
    GetWindowRect(TrayHandle, SourceRect);
    GetWindowRect(AHandle, DestRect);

    DrawAnimatedRects(AHandle, IDANI_CAPTION, SourceRect, DestRect);
  end;
end;

function FindToolbar(Window: THandle; var ToolbarHandle: THandle): BOOL; stdcall;
var
  Buf: array[Byte] of Char;
begin
  GetClassName(Window, Buf, Length(Buf) - 1);
  // Set result to false when we have found a toolbar
  Result := StrIComp(Buf, TOOLBARCLASSNAME) <> 0;
  if not Result then
    ToolbarHandle := Window;
end;

function GetToolbarHandle: THandle;
var
  TrayHandle: THandle;
begin
  Result := 0;
  TrayHandle := GetTrayHandle;
  if TrayHandle <> 0 then
    EnumChildWindows(TrayHandle, @FindToolbar, LPARAM(@Result));
end;

function GetIconRect(const AWnd: THandle; const AID: UINT; var IconRect: TRect): Boolean;
begin
  Result := False;

  with TTrayIconEnumerator.Create(SizeOf(IconRect)) do
  try
    while MoveNext do
      if (CurrentWnd = AWnd) and (CurrentID = AID) then
      begin
        // Button can be hidden in XP
        if (CurrentButton.fsState and TBSTATE_HIDDEN) <> 0 then
          Exit;

        // Retrieve the button rectangle..
        SendMessage(ToolbarHandle, TB_GETITEMRECT, Index, LPARAM(Data));
        // ..and copy it to the current process. If it fails no need to continue
        if not ReadProcessMemory(FData, SizeOf(IconRect), IconRect) then
          Exit;

        // Convert it to the desktop coordinate space
        MapWindowPoints(ToolbarHandle, HWND_DESKTOP, IconRect.TopLeft, 2);

        Result := True;
        Exit;
      end;
  finally
    Free;
  end;
end;

procedure RefreshTray;
var
  IconData: TNotifyIconData;
begin
  FillChar(IconData, SizeOf(IconData), #0);
  IconData.cbSize := SizeOf(IconData);

  with TTrayIconEnumerator.Create do
  try
    while MoveNext do
      if not IsWindow(CurrentWnd) then
      begin
        IconData.Wnd := CurrentWnd;
        IconData.uID := CurrentID;
        Shell_NotifyIcon(NIM_DELETE, @IconData);
      end;
  finally
    Free;
  end;
end;

//=== { TJvTrayIcon } ========================================================

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
  FBalloonCount := 0;
  FActive := False;
  FTask := True;
  FSwingDirectionState := 1;
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

  inherited Destroy;
end;

function TJvTrayIcon.AcceptBalloons: Boolean;
begin
  // Balloons are only accepted with shell32.dll 5.0+
  Result := GetShellVersion >= Shell32VersionIE5;
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
  if (tisTrayIconVisible in FState) and AcceptBalloons then
  begin
    FTime := Now;
    FTimeDelay := ADelay div 1000;

    // if we must cancel an existing balloon
    if CancelPrevious then
      HideBalloon;

    with FIconData do
      StrPLCopy(szInfoTitle, Title, Length(szInfoTitle) - 1);
    with FIconData do
      StrPLCopy(szInfo, Value, Length(szInfo) - 1);

    FIconData.uTimeOut := ADelay;
    FIconData.dwInfoFlags := cInfoFlagValues[BalloonType];

    if NotifyIcon(NIF_INFO, NIM_MODIFY) then
    begin
      if (Title = '') and (Value = '') then
      begin
        Dec(FBalloonCount);
        if FBalloonCount < 0 then
          FBalloonCount := 0;
      end
      else
        Inc(FBalloonCount);

      // if the delay is less than the system's minimum and the balloon
      // was really shown (title and value are not both empty)
      // (rb) XP: if Value = '' then balloon is not shown
      if (ADelay < GetSystemMinimumBalloonDelay) and ((Title <> '') or (Value <> '')) then
        // then we enable the ballon closer timer which will cancel
        // the balloon when the delay is elapsed
        SetTimer(FHandle, CloseBalloonTimer, ADelay, nil);

      if Assigned(FOnBalloonShow) then
        FOnBalloonShow(Self);
    end;
  end;
end;

procedure TJvTrayIcon.DoAnimation;
begin
  if (tisTrayIconVisible in FState) and (FIcons <> nil) and (FIcons.Count > 1) then
  begin
    if IconIndex < 0 then
      IconIndex := 0
    else
    if SwingForthAndBack then
    begin
      if IconIndex = FIcons.Count - 1 then
        FSwingDirectionState := -1
      else
      if IconIndex = 0 then
        FSwingDirectionState := 1;
      IconIndex := (IconIndex + FSwingDirectionState) mod FIcons.Count;
    end
    else
      IconIndex := (IconIndex + 1) mod FIcons.Count;

    if Assigned(FOnAnimate) then
      FOnAnimate(Self, IconIndex);
  end
  else
    IconIndex := 0;
end;

procedure TJvTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
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
    if ApplicationVisible then
    begin
      if tvMinimizeClick in Visibility then
        { (rb) Call Application.Minimize instead of HideApplication
               if tvAutoHide not in Visibility ? }
        HideApplication;
    end
    else
    if tvRestoreClick in Visibility then
      ShowApplication;
  end;
end;

procedure TJvTrayIcon.DoCloseBalloon;
begin
  // we stop the timer and hide the balloon
  StopTimer(CloseBalloonTimer);
  HideBalloon;
end;

procedure TJvTrayIcon.DoContextPopup(X, Y: Integer);
var
  Handled: Boolean;
begin
  Handled := False;
  if Assigned(FOnContextPopup) then
    FOnContextPopup(Self, Point(X, Y), Handled);
  if not Handled and Assigned(FPopupMenu) then
  begin
    SetForegroundWindow(FHandle);
    FPopupMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
  end;
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
        { (rb) Call Application.Minimize instead of HideApplication
               if tvAutoHide not in Visibility ? }
        HideApplication;
    end
    else
    if tvRestoreDbClick in Visibility then
      ShowApplication;
  end;
end;

procedure TJvTrayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Include(FState, tisClicked);
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvTrayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if tisHintChanged in FState then
  begin
    Exclude(FState, tisHintChanged);

    with FIconData do
      StrPLCopy(szTip, GetShortHint(FHint), cHintSize[GetShellVersion >= Shell32VersionIE5]);

    if tisTrayIconVisible in FState then
      NotifyIcon(NIF_TIP, NIM_MODIFY);
  end;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TJvTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  function HasSingleClickFunctionality: Boolean;
  begin
    Result :=
        Assigned(FOnClick) or
        ((Button = mbLeft) and (Assigned(FDropDownMenu) or
          ([tvRestoreClick, tvMinimizeClick] * Visibility <> [])));
  end;
  function HasDoubleClickFunctionality: Boolean;
  begin
    Result :=
        Assigned(FOnDblClick) or
        ([tvRestoreDbClick, tvMinimizeDbClick] * Visibility <> []);
  end;
begin
  if tisClicked in FState then
  begin
    Exclude(FState, tisClicked);
    if HasSingleClickFunctionality then
    begin
      if HasDoubleClickFunctionality then
      begin
        // Delay DoClick
        FClickedButton := Button;
        FClickedShift := Shift;
        FClickedX := X;
        FClickedY := Y;

        if not (tisWaitingForDoubleClick in FState) then
        begin
          Include(FState, tisWaitingForDoubleClick);
          SetTimer(FHandle, DblClickTimer, GetDoubleClickTime, nil);
        end;
      end
      else
        DoClick(Button, Shift, X, Y);
    end;
    //else
    //  DoClick(Button, Shift, X, Y);
  end;

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if (Button = mbRight) and (GetShellVersion < Shell32VersionIE5) then
    DoContextPopup(X, Y);
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
  Result := not (tisAppHiddenButNotMinimized in FState) and not IsApplicationMinimized;
  {$IFDEF COMPILER11_UP}
  if Result and (Snap or (tvAnimateToTray in Visibility)) and
     Application.MainFormOnTaskBar and not IsWindowVisible(Application.MainFormHandle) then
    Result := False;
  {$ENDIF COMPILER11_UP}
end;

function TJvTrayIcon.GetIconRect(var IconRect: TRect): Boolean;
begin
  Result := JvTrayIcon.GetIconRect(Self.FHandle, cTaskbarIconIdentifier, IconRect);
end;

function TJvTrayIcon.GetIconVisible: Boolean;
begin
  Result := tisTrayIconVisible in FState;
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
  // Note: some actions will show/hide the taskbar button of the application,
  // so we have to do them in a certain order.

  if ApplicationVisible then
  begin
    // Custom animation?
    if Snap or (tvAnimateToTray in Visibility) then
    begin
      if Assigned(Application.MainForm) then
      begin
        if tvAnimateToTray in Visibility then
          AnimateToTray(Application.MainForm.Handle);
        // To prevent another animation we have to set both
        // ShowMainForm and MainForm.Visible to false
        Application.MainForm.Visible := False;
      end;
      Application.ShowMainForm := False;
    end;
    // This will show the taskbar button
    Application.Minimize;
  end;

  // ..and hide the taskbar button of the application
  ShowWindow(GetHandleOnTaskBar, SW_HIDE);
  Exclude(FVisibility, tvVisibleTaskBar);

  if tvAutoHideIcon in Visibility then
    ShowTrayIcon;
end;

procedure TJvTrayIcon.HideBalloon;
var
  I: Integer;
begin
  // We call BalloonHint with title and info set to
  // empty strings which surprisingly will cancel any existing
  // balloon for the icon. This is clearly not documented by
  // Microsoft and may not work in later releases of Windows
  // Under Windows XP, you only need to do this once. But under
  // Windows 2000, it seems one must do this one time more than
  // there were calls to BalloonHint with real messages

  // (rb) A bit confusing because calling BalloonHint changes FBalloonCount
  for I := 0 to FBalloonCount do
    BalloonHint('', '');
end;

procedure TJvTrayIcon.HideTrayIcon;
begin
  // reentrance check
  if tisTrayIconVisible in FState then
  begin
    EndAnimation;

    if NotifyIcon(0, NIM_DELETE) then
      Exclude(FState, tisTrayIconVisible);
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
    else
    if Assigned(Icon) and (not Icon.Empty) then
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
    if GetShellVersion >= Shell32VersionIE5 then
    begin
      cbSize := SizeOf(FIconData);
      FIconData.uTimeOut := NOTIFYICON_VERSION;
    end
    else
      cbSize := SizeOf(TNotifyIconData);
    Wnd := FHandle;
    // We have only 1 icon per FHandle, so no need to uniquely identify
    uID := cTaskbarIconIdentifier;
    uCallbackMessage := WM_CALLBACKMESSAGE;
    if not CurrentIcon.Empty then
      hIcon := CurrentIcon.Handle
    else
      CurrentIcon := Application.Icon;
    StrPLCopy(szTip, GetShortHint(FHint), cHintSize[GetShellVersion >= Shell32VersionIE5]);
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

    if not (csDesigning in ComponentState) then
    begin
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

        Application.NormalizeTopMosts;
        SetActiveWindow(GetHandleOnTaskBar);
        ShowWinNoAnimate(GetHandleOnTaskBar, SW_HIDE);
        Include(FState, tisAppHiddenButNotMinimized);
      end;

      if not (tvVisibleTaskList in Visibility) then
        SetTask(False);
    end;
  end;
end;

procedure TJvTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
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
const
  cMaxRetryCount = 30; // arbitrary
  cDelay = 1000; // arbitrary
var
  ErrorCode: Integer;
  RetryCount: Integer;
begin
  FIconData.uFlags := uFlags;
  Result := Shell_NotifyIcon(dwMessage, @FIconData);
  if not Result and not (tvNoRetryOnFailure in Visibility) then
  begin
    { Calling Shell_NotifyIcon can fail on XP when the shell is busy
      See http://support.microsoft.com/default.aspx?scid=kb;ja;418138

      Shell_NotifyIcon has a timeout of 4 sec. to complete. If that fails
      because the shell is busy, then False is returned and GetLastError
      returns ERROR_TIMEOUT (but testing shows that it can also return 0)
      Solution is to wait a bit and retry.

      However, even when GetLastError() returns ERROR_TIMEOUT,
      the icon can often be actually added(or deleted).

      If NIM_ADD times out and Shell_NotifyIcon(NIM_MODIFY) returns true,
      the addition of the icon was actually successful.
      Similarly, if NIM_DELETE times out and Shell_NotifyIcon(NIM_MODIFY) returns
      false, the deletion of the icon was actually successful. (See Mantis #3747)

      http://qc.borland.com/wc/qcmain.aspx?d=29306 provides steps to
      reproduce this problem.
    }
    ErrorCode := GetLastError;
    if (ErrorCode = 0) or (ErrorCode = ERROR_TIMEOUT) then
    begin
      RetryCount := 0;
      repeat
        Sleep(cDelay);

        case dwMessage of
          NIM_ADD: Result := Shell_NotifyIcon(NIM_MODIFY, @FIconData);
          NIM_DELETE: Result := not Shell_NotifyIcon(NIM_MODIFY, @FIconData);
        end;

        if Result then
          Exit;

        Inc(RetryCount);
        Result := Shell_NotifyIcon(dwMessage, @FIconData);
      until Result or (RetryCount > cMaxRetryCount);
    end;
  end;
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
      Hook;
      ShowTrayIcon;
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

procedure TJvTrayIcon.SetDropDownMenu(const Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FDropDownMenu));
end;

procedure TJvTrayIcon.SetHint(Value: string);
begin
  //Remove sLineBreak on w98/me as they are not supported
  if GetShellVersion < Shell32VersionIE5 then
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
  if ReplaceComponentReference(Self, Value, TComponent(FIcons)) then
    IconPropertyChanged; //HEG: New
end;

procedure TJvTrayIcon.SetIconVisible(const Value: Boolean);
begin
  if Value then
    ShowTrayIcon
  else
    HideTrayIcon;
end;

procedure TJvTrayIcon.SetPopupMenu(const Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FPopupMenu));
end;

procedure TJvTrayIcon.SetTask(const Value: Boolean);
begin
  if FTask <> Value then
  begin
    FTask := Value;
    if not (csDesigning in ComponentState) then
    begin
      if not Assigned(RegisterServiceProcess) then
        RegisterServiceProcess := GetProcAddress(GetModuleHandle(kernel32), RegisterServiceProcessName);

      if Assigned(RegisterServiceProcess) then
        if FTask then
          RegisterServiceProcess(GetCurrentProcessID, 0)
        else
          RegisterServiceProcess(GetCurrentProcessID, 1);
    end;
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
      VisibleInTaskList := tvVisibleTaskList in FVisibility;

      if tvAutoHide in ToBeSet then
      begin
        if not ApplicationVisible then
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
  // Note: some actions will show/hide the taskbar button of the application,
  // so we have to do them in a certain order.

  if tisAppHiddenButNotMinimized in FState then
  begin
    // Make the application not iconic; this will show the taskbar button
    ShowWinNoAnimate(GetHandleOnTaskBar, SW_MINIMIZE);
    // If we set ShowMainForm to true we get an animation when we call
    // Application.Restore
    if not Snap and not (tvAnimateToTray in Visibility) then
      Application.ShowMainForm := True;
  end;

  // Show the taskbar button of the application..
  Include(FVisibility, tvVisibleTaskBar);
  ShowWindow(GetHandleOnTaskBar, SW_SHOW);

  if not ApplicationVisible then
  begin
    if (tvAnimateToTray in Visibility) and Assigned(Application.MainForm) then
      AnimateFromTray(Application.MainForm.Handle);
    {$IFDEF COMPILER11_UP}
    // Application.Restore checks the IsIconic state of the app window
    if Application.MainFormOnTaskBar and not IsIconic(Application.Handle) then
      ShowWinNoAnimate(Application.Handle, SW_SHOWMINNOACTIVE);
    {$ENDIF COMPILER11_UP}
    // ..and restore the application
    Application.Restore;
    {$IFDEF COMPILER11_UP}
    // Without this Application.Restore would only work the first time
    if Application.MainFormOnTaskBar and IsIconic(Application.Handle) then
      ShowWinNoAnimate(Application.Handle, SW_SHOWNOACTIVATE);
    {$ENDIF COMPILER11_UP}
    if Application.MainForm <> nil then
      Application.MainForm.Visible := True;
  end;

  Exclude(FState, tisAppHiddenButNotMinimized);
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
  if (tvAutoHideIcon in Visibility) and ApplicationVisible then
    Exit;

  // All checks passed, make the trayicon visible:

  if NotifyIcon(NIF_MESSAGE or NIF_ICON or NIF_TIP, NIM_ADD) then
  begin
    Include(FState, tisTrayIconVisible);

    // If we call NIM_SETVERSION, we must call it *after* NIM_ADD.
    if GetShellVersion >= Shell32VersionIE5 then
      NotifyIcon(0, NIM_SETVERSION);

    if Animated then
      StartAnimation;
  end;
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
                DoMouseMove(ShState, Pt.X, Pt.Y);
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
              WM_RBUTTONUP:
                DoMouseUp(mbRight, ShState, Pt.X, Pt.Y);
              WM_CONTEXTMENU, NIN_KEYSELECT:
                // WM_CONTEXTMENU: press Shift+F10 while trayicon has focus.
                // NIN_KEYSELECT:  press Enter or Space while trayicon has focus.
                // Windows moves the mouse pointer to the trayicon before these messages,
                // so Pt is valid.
                DoContextPopup(Pt.X, Pt.Y);
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
                      FOnBalloonHide(Self);
                  except
                  end;
                  Result := Ord(True);
                end;
              NIN_BALLOONTIMEOUT: //sb
                begin
                  I := SecondsBetween(Now, FTime);
                  if I > FTimeDelay then
                    HideBalloon;
                  Result := Ord(True);
                end;
              NIN_BALLOONUSERCLICK: //sb
                begin
                  { (rb) Double try..except }
                  try
                    if Assigned(FOnBalloonClick) then
                      FOnBalloonClick(Self);
                  except
                  end;
                  Result := Ord(True);
                  //Result := DefWindowProc(FHandle, Msg, WParam, LParam);
                  HideBalloon;
                end;
            end;
          end;
        {$IFNDEF DELPHI2009_UP}
        // Add by Winston Feng 2003-9-28
        // Handle the QueryEndSession and TaskbarCreated message, so trayicon
        // will be deleted and restored correctly.
        // For D2009 and upper, we must let the default window proc handle it.
        WM_QUERYENDSESSION:
          Result := 1;
        {$ENDIF ~DELPHI2009_UP}
        WM_ENDSESSION:
          // (rb) Is it really necessairy to respond to WM_ENDSESSION?
          if TWMEndSession(Mesg).EndSession then
            HideTrayIcon
          else
          if Active then
            ShowTrayIcon;
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
        if Msg = WM_TaskbarRestartMsg then
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
          if Active then
            ShowTrayIcon;
        end
        else
          Result := DefWindowProc(FHandle, Msg, WParam, LParam);
      end; // case
  except
    Application.HandleException(Self);
  end;
end;

//=== { TTrayIconEnumerator } ================================================

constructor TTrayIconEnumerator.Create(DataSize: Integer);
begin
  inherited Create;
  if DataSize < SizeOf(TTBButton) then
    DataSize := SizeOf(TTBButton);
  Init(DataSize);
  FIndex := FCount;
end;

constructor TTrayIconEnumerator.Create;
begin
  inherited Create;
  Init(SizeOf(TTBButton));
  FIndex := FCount;
end;

destructor TTrayIconEnumerator.Destroy;
begin
  if FData <> nil then
    VirtualFreeEx(FProcess, FData, 0, MEM_RELEASE);
  if FProcess <> 0 then
    CloseHandle(FProcess);
  inherited Destroy;
end;

procedure TTrayIconEnumerator.Init(const DataSize: Integer);
{ Taken from http://www.thecodeproject.com/shell/ctrayiconposition.asp }
var
  ProcessID: DWORD;
begin
  // The trayicons are actually buttons on a toolbar
  FToolbarHandle := GetToolbarHandle;
  if FToolbarHandle = 0 then
    Exit;

  FCount := SendMessage(FToolbarHandle, TB_BUTTONCOUNT, 0, 0);
  if FCount < 1 then
    Exit;

  // We want to get data from another process - it's not possible
  // to just send messages like TB_GETBUTTON with a locally
  // allocated buffer for return data. Pointer to locally allocated
  // data has no usefull meaning in a context of another
  // process (since Win95) - so we need
  // to allocate some memory inside Tray process.
  
  if GetWindowThreadProcessId(FToolbarHandle, ProcessID) = 0 then
    Exit;

  FProcess := OpenProcess(PROCESS_ALL_ACCESS, False, ProcessID);
  if FProcess = 0 then
    Exit;

  // Allocate needed memory in the context of the tray process. We reuse
  // Data to read multiple parts so we set it to the biggest chunk we need
  // (TTBButton)
  FData := VirtualAllocEx(FProcess, nil, DataSize, MEM_COMMIT, PAGE_READWRITE);
end;

function TTrayIconEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if (FProcess = 0) or not Assigned(FData) then
    Exit;

  Dec(FIndex);

  while FIndex >= 0 do
  begin
    SendMessage(FToolbarHandle, TB_GETBUTTON, FIndex, LPARAM(FData));

    // Read the data from the tray process into the current process.
    if ReadProcessMemory(FData, SizeOf(FButton), FButton) then
    begin
      // Read the extra data, Button.dwData points to its location
      if ReadProcessMemory(Pointer(FButton.dwData), SizeOf(FExtraData), FExtraData) then
      begin
        Result := True;
        Exit;
      end;
    end;

    Dec(FIndex);
  end;
end;

function TTrayIconEnumerator.ReadProcessMemory(const Address: Pointer;
  Count: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF}; var Buffer): Boolean;
var
  BytesRead: {$IFDEF RTL230_UP}NativeUInt{$ELSE}DWORD{$ENDIF};
begin
  Result := Windows.ReadProcessMemory(FProcess, Address, @Buffer, Count, BytesRead) and
    (BytesRead = Count);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  WM_TaskbarRestartMsg := RegisterWindowMessage('TaskbarCreated');

finalization
  RegisterServiceProcess := nil;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
