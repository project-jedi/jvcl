{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShell.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


{.$DEFINE USE_TIMER}
{ - Use Windows timer instead thread to the animated TrayIcon }

{$IFNDEF WIN32}
  {$DEFINE USE_TIMER}  { - Always use timer in 16-bit version }
{$ENDIF}

unit JvShell;

interface

uses {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Messages,
  Classes, Graphics, SysUtils, Forms, Controls, Menus, ShellAPI, JvComponent,
  {$IFDEF USE_TIMER} ExtCtrls, {$ENDIF} JvIcoList{, JvComponent};

type
{$IFNDEF WIN32}
  PNotifyIconData = ^TNotifyIconData;
  TNotifyIconData = record
    cbSize: Longint;
    Wnd: Longint;
    uID: Longint;
    uFlags: Longint;
    uCallbackMessage: Longint;
    hIcon: Longint;
    szTip: array [0..63] of Char;
  end;
{$ENDIF}

  TMouseButtons = set of TMouseButton;

{ TJvxTrayIcon }

  TJvxTrayIcon = class(TJvComponent)
  private
    FHandle: HWnd;
    FActive: Boolean;
    FAdded: Boolean;
    FAnimated: Boolean;
    FEnabled: Boolean;
    FClicked: TMouseButtons;
    FIconIndex: Integer;
    FInterval: Word;
    FIconData: TNotifyIconData;
    FIcon: TIcon;
    FIconList: TJvIconList;
{$IFDEF USE_TIMER}
    FTimer: TTimer;
{$ELSE}
    FTimer: TThread;
{$ENDIF}
    FHint: string;
    FShowDesign: Boolean;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    procedure ChangeIcon;
{$IFDEF USE_TIMER}
    procedure Timer(Sender: TObject);
{$ELSE}
    procedure Timer;
{$ENDIF}
    procedure SendCancelMode;
    function CheckMenuPopup(X, Y: Integer): Boolean;
    function CheckDefaultMenuItem: Boolean;
    procedure SetHint(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetIconList(Value: TJvIconList);
    procedure SetPopupMenu(Value: TPopupMenu);
    procedure Activate;
    procedure Deactivate;
    procedure SetActive(Value: Boolean);
    function GetAnimated: Boolean;
    procedure SetAnimated(Value: Boolean);
    procedure SetShowDesign(Value: Boolean);
    procedure SetInterval(Value: Word);
    procedure IconChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage);
    function GetActiveIcon: TIcon;
  protected
    procedure DblClick; dynamic;
    procedure DoClick(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateNotifyData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Hide;
    procedure Show;
    property Handle: HWnd read FHandle;
  published
    property Active: Boolean read FActive write SetActive default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Hint: string read FHint write SetHint;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TJvIconList read FIconList write SetIconList;
    { Ensure Icons is declared before Animated }
    property Animated: Boolean read GetAnimated write SetAnimated default False;
    property Interval: Word read FInterval write SetInterval default 150;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property ShowDesign: Boolean read FShowDesign write SetShowDesign stored False;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
procedure WinAbout(const AppName, Stuff: string);

type
  TExecState = (esNormal, esMinimized, esMaximized, esHidden);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;

implementation

uses JvConst, JvCConst, JvVCLUtils, JvMaxMin;

{$IFNDEF WIN32}
const
  Shell = 'shell';

function ExtractAssociatedIcon(hInst: THandle; lpIconPath: PChar;
  var lpiIcon: Word): HIcon; far; external Shell;
function ShellAbout(Wnd: HWnd; App, Stuff: PChar; Icon: HIcon): Integer;
  far; external Shell;
{$ENDIF WIN32}

procedure WinAbout(const AppName, Stuff: string);
var
{$IFNDEF WIN32}
  szApp, szStuff: array[0..255] of Char;
{$ENDIF}
  Wnd: HWnd;
  Icon: HIcon;
begin
  if Application.MainForm <> nil then Wnd := Application.MainForm.Handle
  else Wnd := 0;
  Icon := Application.Icon.Handle;
  if Icon = 0 then Icon := LoadIcon(0, IDI_APPLICATION);
{$IFDEF WIN32}
  ShellAbout(Wnd, PChar(AppName), PChar(Stuff), Icon);
{$ELSE}
  StrPLCopy(szApp, AppName, SizeOf(szApp) - 1);
  StrPLCopy(szStuff, Stuff, SizeOf(szStuff) - 1);
  ShellAbout(Wnd, szApp, szStuff, Icon);
{$ENDIF}
end;

function IconExtract(const FileName: string; Id: Integer): TIcon;
var
  S: array[0..255] of char;
  IconHandle: HIcon;
  Index: Word;
begin
  Result := TIcon.Create;
  try
    StrPLCopy(S, FileName, SizeOf(S) - 1);
    IconHandle := ExtractIcon(hInstance, S, Id);
    if IconHandle < 2 then begin
      Index := Id;
      IconHandle := ExtractAssociatedIcon(hInstance, S, Index);
    end;
    if IconHandle < 2 then begin
      if IconHandle = 1 then
        raise EResNotFound.Create(LoadStr(SFileNotExec))
      else begin
        Result.Free;
        Result := nil;
      end;
    end else Result.Handle := IconHandle;
  except
    Result.Free;
    raise;
  end;
end;

const
  ShowCommands: array[TExecState] of Integer =
    (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED, SW_HIDE);

function FileExecute(const FileName, Params, StartDir: string;
  InitialState: TExecState): THandle;
{$IFDEF WIN32}
begin
  Result := ShellExecute(Application.Handle, nil, PChar(FileName),
    PChar(Params), PChar(StartDir), ShowCommands[InitialState]);
end;
{$ELSE}
var
  cFileName, cParams, cPath: array [0..80] of Char;
begin
  Result := ShellExecute(Application.Handle, nil, StrPCopy(cFileName,
    FileName), StrPCopy(cParams, Params), StrPCopy(cPath, StartDir),
    ShowCommands[InitialState]);
end;
{$ENDIF}

function FileExecuteWait(const FileName, Params, StartDir: string;
  InitialState: TExecState): Integer;
{$IFDEF WIN32}
var
  Info: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);
  with Info do begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := Application.Handle;
    lpFile := PChar(FileName);
    lpParameters := PChar(Params);
    lpDirectory := PChar(StartDir);
    nShow := ShowCommands[InitialState];
  end;
  if ShellExecuteEx(@Info) then begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(Info.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE) or Application.Terminated;
    Result := ExitCode;
  end
  else Result := -1;
end;
{$ELSE}
var
  Task: THandle;
begin
  Result := 0;
  Task := FileExecute(FileName, Params, StartDir, InitialState);
  if Task >= HINSTANCE_ERROR then begin
    repeat
      Application.ProcessMessages;
    until (GetModuleUsage(Task) = 0) or Application.Terminated;
  end
  else Result := -1;
end;
{$ENDIF}

{$IFNDEF USE_TIMER}

{ TJvTimerThread }

type
  TJvTimerThread = class(TThread)
  private
    FOwnerTray: TJvxTrayIcon;
  protected
    procedure Execute; override;
  public
    constructor Create(TrayIcon: TJvxTrayIcon; CreateSuspended: Boolean);
  end;

constructor TJvTimerThread.Create(TrayIcon: TJvxTrayIcon; CreateSuspended: Boolean);
begin
  FOwnerTray := TrayIcon;
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TJvTimerThread.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FOwnerTray = nil);
  end;

begin
  while not Terminated do begin
    if not ThreadClosed then
      if SleepEx(FOwnerTray.FInterval, False) = 0 then begin
        if not ThreadClosed and FOwnerTray.Animated then
          FOwnerTray.Timer;
      end;
  end;
end;

{$ENDIF USE_TIMER}

{$IFNDEF WIN32}

type
  TLoadLibrary32 = function (FileName: PChar; Handle, Special: Longint): Longint;
  TFreeLibrary32 = function (Handle: Longint): Bool;
  TGetAddress32 = function (Handle: Longint; ProcName: PChar): Pointer;
  TCallProc32 = function (Msg: Longint; Data: PNotifyIconData; ProcHandle: Pointer;
    AddressConvert, Params: Longint): Longint;

const
  NIM_ADD     = $00000000;
  NIM_MODIFY  = $00000001;
  NIM_DELETE  = $00000002;

  NIF_MESSAGE = $00000001;
  NIF_ICON    = $00000002;
  NIF_TIP     = $00000004;

const
  Shell32: Longint = 0;
  ProcAddr: Pointer = nil;
  FreeLib32: TFreeLibrary32 = nil;
  CallPrc32: TCallProc32 = nil;

procedure FreeHandles; far;
begin
  if (ProcAddr <> nil) and Assigned(FreeLib32) then FreeLib32(Shell32);
end;

procedure InitHandles;
var
  Kernel16: THandle;
  LoadLib32: TLoadLibrary32;
  GetAddr32: TGetAddress32;
begin
  Kernel16 := GetModuleHandle('kernel');
  @LoadLib32 := GetProcAddress(Kernel16, 'LoadLibraryEx32W');
  @FreeLib32 := GetProcAddress(Kernel16, 'FreeLibrary32W');
  @GetAddr32 := GetProcAddress(Kernel16, 'GetProcAddress32W');
  @CallPrc32 := GetProcAddress(Kernel16, 'CallProc32W');
  if (@LoadLib32 <> nil) and (@FreeLib32 <> nil) and (@GetAddr32 <> nil)
    and (@CallPrc32 <> nil) then
  begin
    Shell32 := LoadLib32('shell32', 0, 0);
    if Shell32 >= HINSTANCE_ERROR then begin
      ProcAddr := GetAddr32(Shell32, 'Shell_NotifyIcon');
      if ProcAddr = nil then begin
        FreeLib32(Shell32);
        Shell32 := 1;
      end
      else AddExitProc(FreeHandles);
    end
    else Shell32 := 1;
  end;
end;

function Shell_NotifyIcon(dwMessage: Longint; lpData: PNotifyIconData): Bool;
begin
  if (ProcAddr = nil) and (Shell32 <> 1) then InitHandles;
  if ProcAddr <> nil then
    Result := Bool(CallPrc32(dwMessage, lpData, ProcAddr, $01, 2));
end;

{$ENDIF WIN32}

{ TJvxTrayIcon }

constructor TJvxTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHandle := {$IFDEF COMPILER6_UP}Classes.{$ENDIF}AllocateHWnd(WndProc);
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChanged;
  FIconList := TJvIconList.Create;
  FIconList.OnChange := IconChanged;
  FIconIndex := -1;
  FEnabled := True;
  FInterval := 150;
  FActive := True;
end;

destructor TJvxTrayIcon.Destroy;
begin
  Destroying;
  FEnabled := False;
  FIconList.OnChange := nil;
  FIcon.OnChange := nil;
  SetAnimated(False);
  Deactivate;
  {$IFDEF COMPILER6_UP}Classes.{$ENDIF}DeallocateHWnd(FHandle);
  FIcon.Free;
  FIcon := nil;
  FIconList.Free;
  FIconList := nil;
  inherited Destroy;
end;

procedure TJvxTrayIcon.Loaded;
begin
  inherited Loaded;
  if FActive and not (csDesigning in ComponentState) then Activate;
end;

procedure TJvxTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = PopupMenu) and (Operation = opRemove) then
    PopupMenu := nil;
end;

procedure TJvxTrayIcon.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
{$IFDEF WIN32}
  if Value <> nil then Value.FreeNotification(Self);
{$ENDIF}
end;

procedure TJvxTrayIcon.SendCancelMode;
var
  F: TForm;
begin
  if not (csDestroying in ComponentState) then begin
    F := Screen.ActiveForm;
    if F = nil then F := Application.MainForm;
    if F <> nil then F.SendCancelMode(nil);
  end;
end;

function TJvxTrayIcon.CheckMenuPopup(X, Y: Integer): Boolean;
begin
  Result := False;
  if not (csDesigning in ComponentState) and Active and
    (PopupMenu <> nil) and PopupMenu.AutoPopup then
  begin
    PopupMenu.PopupComponent := Self;
    SendCancelMode;
    SwitchToWindow(FHandle, False);
    Application.ProcessMessages;
    try
      PopupMenu.Popup(X, Y);
    finally
{$IFDEF WIN32}
      SwitchToWindow(FHandle, False);
{$ENDIF}
    end;
    Result := True;
  end;
end;

function TJvxTrayIcon.CheckDefaultMenuItem: Boolean;
{$IFDEF WIN32}
var
  Item: TMenuItem;
  I: Integer;
{$ENDIF}
begin
  Result := False;
{$IFDEF WIN32}
  if not (csDesigning in ComponentState) and Active and
    (PopupMenu <> nil) and (PopupMenu.Items <> nil) then
  begin
    I := 0;
    while (I < PopupMenu.Items.Count) do begin
      Item := PopupMenu.Items[I];
      if Item.Default and Item.Enabled then begin
        Item.Click;
        Result := True;
        Break;
      end;
      Inc(I);
    end;
  end;
{$ENDIF}
end;

procedure TJvxTrayIcon.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

procedure TJvxTrayIcon.SetIconList(Value: TJvIconList);
begin
  FIconList.Assign(Value);
end;

function TJvxTrayIcon.GetActiveIcon: TIcon;
begin
  Result := FIcon;
  if (FIconList <> nil) and (FIconList.Count > 0) and Animated then
    Result := FIconList[Max(Min(FIconIndex, FIconList.Count - 1), 0)];
end;

function TJvxTrayIcon.GetAnimated: Boolean;
begin
  Result := FAnimated;
end;

procedure TJvxTrayIcon.SetAnimated(Value: Boolean);
begin
  Value := Value and Assigned(FIconList) and (FIconList.Count > 0);
  if Value <> Animated then begin
    if Value then begin
{$IFDEF USE_TIMER}
      FTimer := TTimer.Create(Self);
      FTimer.Enabled := FAdded;
      FTimer.Interval := FInterval;
      FTimer.OnTimer := Timer;
{$ELSE}
      FTimer := TJvTimerThread.Create(Self, not FAdded);
{$ENDIF}
      FAnimated := True;
    end
    else begin
      FAnimated := False;
{$IFDEF USE_TIMER}
      FTimer.Free;
      FTimer := nil;
{$ELSE}
      TJvTimerThread(FTimer).FOwnerTray := nil;
      while FTimer.Suspended do FTimer.Resume;
      FTimer.Terminate;
{$ENDIF}
    end;
    FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TJvxTrayIcon.SetActive(Value: Boolean);
begin
  if (Value <> FActive) then begin
    FActive := Value;
    if not (csDesigning in ComponentState) then
      if Value then Activate else Deactivate;
  end;
end;

procedure TJvxTrayIcon.Show;
begin
  Active := True;
end;

procedure TJvxTrayIcon.Hide;
begin
  Active := False;
end;

procedure TJvxTrayIcon.SetShowDesign(Value: Boolean);
begin
  if (csDesigning in ComponentState) then begin
    if Value then Activate else Deactivate;
    FShowDesign := FAdded;
  end;
end;

procedure TJvxTrayIcon.SetInterval(Value: Word);
begin
  if FInterval <> Value then begin
    FInterval := Value;
{$IFDEF USE_TIMER}
    if Animated then FTimer.Interval := FInterval;
{$ENDIF}
  end;
end;

{$IFDEF USE_TIMER}
procedure TJvxTrayIcon.Timer(Sender: TObject);
{$ELSE}
procedure TJvxTrayIcon.Timer;
{$ENDIF}
begin
  if not (csDestroying in ComponentState) and Animated then begin
    Inc(FIconIndex);
    if (FIconList = nil) or (FIconIndex >= FIconList.Count) then
      FIconIndex := 0;
    ChangeIcon;
  end;
end;

procedure TJvxTrayIcon.IconChanged(Sender: TObject);
begin
  ChangeIcon;
end;

procedure TJvxTrayIcon.SetHint(const Value: string);
begin
  if FHint <> Value then begin
    FHint := Value;
    ChangeIcon;
  end;
end;

procedure TJvxTrayIcon.UpdateNotifyData;
var
  Ico: TIcon;
begin
  with FIconData do begin
    cbSize := SizeOf(TNotifyIconData);
    Wnd := FHandle;
    uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    Ico := GetActiveIcon;
    if Ico <> nil then hIcon := Ico.Handle
{$IFDEF WIN32}
    else hIcon := INVALID_HANDLE_VALUE;
{$ELSE}
    else hIcon := 0;
{$ENDIF}
    StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    uCallbackMessage := CM_TRAYICON;
    uID := 0;
  end;
end;

procedure TJvxTrayIcon.Activate;
var
  Ico: TIcon;
begin
  Deactivate;
  Ico := GetActiveIcon;
  if (Ico <> nil) and not Ico.Empty then begin
    FClicked := [];
    UpdateNotifyData;
    FAdded := Shell_NotifyIcon(NIM_ADD, @FIconData);
    if (GetShortHint(FHint) = '') and FAdded then
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
{$IFDEF USE_TIMER}
    if Animated then FTimer.Enabled := True;
{$ELSE}
    if Animated then
      while FTimer.Suspended do FTimer.Resume;
{$ENDIF}
  end;
end;

procedure TJvxTrayIcon.Deactivate;
begin
  Shell_NotifyIcon(NIM_DELETE, @FIconData);
  FAdded := False;
  FClicked := [];
{$IFDEF USE_TIMER}
  if Animated then FTimer.Enabled := False;
{$ELSE}
  if Animated and not FTimer.Suspended then FTimer.Suspend;
{$ENDIF}
end;

procedure TJvxTrayIcon.ChangeIcon;
var
  Ico: TIcon;
begin
  if (FIconList = nil) or (FIconList.Count = 0) then SetAnimated(False);
  if FAdded then begin
    Ico := GetActiveIcon;
    if (Ico <> nil) and not Ico.Empty then begin
      UpdateNotifyData;
      Shell_NotifyIcon(NIM_MODIFY, @FIconData);
    end
    else Deactivate;
  end
  else begin
    if ((csDesigning in ComponentState) and FShowDesign) or
      (not (csDesigning in ComponentState) and FActive) then Activate;
  end;
end;

procedure TJvxTrayIcon.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TJvxTrayIcon.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TJvxTrayIcon.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TJvxTrayIcon.DblClick;
begin
  if not CheckDefaultMenuItem and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TJvxTrayIcon.DoClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and CheckMenuPopup(X, Y) then Exit;
  if Assigned(FOnClick) then FOnClick(Self, Button, Shift, X, Y);
end;

procedure TJvxTrayIcon.WndProc(var Message: TMessage);

  function GetShiftState: TShiftState;
  begin
    Result := [];
    if GetKeyState(VK_SHIFT) < 0 then Include(Result, ssShift);
    if GetKeyState(VK_CONTROL) < 0 then Include(Result, ssCtrl);
    if GetKeyState(VK_MENU) < 0 then Include(Result, ssAlt);
  end;

var
  P: TPoint;
  Shift: TShiftState;
begin
  try
    with Message do
      if (Msg = CM_TRAYICON) and Self.FEnabled then begin
        case lParam of
          WM_LBUTTONDBLCLK:
            begin
              DblClick;
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_RBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssDouble], P.X, P.Y);
            end;
          WM_MOUSEMOVE:
            begin
              GetCursorPos(P);
              MouseMove(GetShiftState, P.X, P.Y);
            end;
          WM_LBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbLeft, GetShiftState + [ssLeft], P.X, P.Y);
              Include(FClicked, mbLeft);
            end;
          WM_LBUTTONUP:
            begin
              Shift := GetShiftState + [ssLeft];
              GetCursorPos(P);
              if mbLeft in FClicked then begin
                Exclude(FClicked, mbLeft);
                DoClick(mbLeft, Shift, P.X, P.Y);
              end;
              MouseUp(mbLeft, Shift, P.X, P.Y);
            end;
          WM_RBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbRight, GetShiftState + [ssRight], P.X, P.Y);
              Include(FClicked, mbRight);
            end;
          WM_RBUTTONUP:
            begin
              Shift := GetShiftState + [ssRight];
              GetCursorPos(P);
              if mbRight in FClicked then begin
                Exclude(FClicked, mbRight);
                DoClick(mbRight, Shift, P.X, P.Y);
              end;
              MouseUp(mbRight, Shift, P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              MouseDown(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              MouseUp(mbMiddle, GetShiftState + [ssMiddle], P.X, P.Y);
            end;
        end;
      end
      else Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

end.
