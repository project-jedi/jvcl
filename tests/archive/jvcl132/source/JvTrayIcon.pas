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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTrayIcon;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls,
  Menus, ShellApi, JvTypes, JvComponent
 {$IFDEF Delphi6_UP}
  ,DateUtils
 {$ENDIF}
  ;

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
    szInfo: array[0..255] of AnsiChar;
    uTimeOut: DWORD;
    szInfoTitle: array[0..63] of AnsiChar;
    dwInfoFlags: DWORD;
  end;

  TRegisterServiceProcess = function(dwProcessID, dwType: Integer): Integer; stdcall;

  TAnimateEvent = procedure (Sender: TObject; const ImageIndex: Integer) of object; 
  TJvTrayIcon = class(TJvComponent)
  private
    FActive: Boolean;
    FIcon: TIcon;
    FIc: TNotifyIconDataXP;
    FHandle: THandle;
    FHint: string;
    FPopupMenu: TPopupMenu;
    FOnClick: TMouseEvent;
    FOnDblClick: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FVisible: Boolean;
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
    FTime: TDateTime;
    FTimeDelay: Integer;
    FOldTray: HWND;
    FOnAnimate: TAnimateEvent;
    procedure OnAnimateTimer(Sender: TObject);
    procedure IconChanged(Sender: TObject);
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIcon(Icon: TIcon);
    procedure SetVisible(Value: Boolean);
    procedure SetAnimated(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetImgList(const Value: TImageList);
    procedure SetTask(const Value: Boolean);
    procedure SetNumber(const Value: Integer);
  protected
    procedure DoMouseMove(Shift: TShiftState;X, Y: Integer);
    procedure DoMouseDown(Button: TMouseButton;Shift: TShiftState;X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton;Shift: TShiftState;X, Y: Integer);
    procedure DoDoubleClick(Button: TMouseButton;Shift: TShiftState;X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCheckCrash;
    procedure WndProc(var Mesg: TMessage);
  published
    procedure HideApplication;
    procedure ShowApplication;
    procedure BalloonHint(Title,Value: string;BalloonType:
      TBalloonType = btNone;Delay: Integer = 5000);
    function AcceptBalloons: Boolean;

    property Active: Boolean read FActive write SetActive default False;
    property Icon: TIcon read FIcon write SetIcon;
    property Hint: string read FHint write SetHint;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property ApplicationVisible: Boolean read FVisible write SetVisible default True;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property IconIndex: Integer read FNumber write SetNumber;
    property Icons: TImageList read FImgList write SetImgList;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;

    property OnAnimate: TAnimateEvent read FOnAnimate write FOnAnimate;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TMouseEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnBalloonShow:TNotifyEvent read FOnBalloonShow write FOnBalloonShow;
    property OnBalloonHide:TNotifyEvent read FOnBalloonHide write FOnBalloonHide;
    property OnBalloonClick:TNotifyEvent read FOnBalloonClick write FOnBalloonClick;
  end;

implementation



const
  WM_CALLBACKMESSAGE            = WM_USER + 1;
  NOTIFYICON_VERSION            = 3;
  NIM_ADD                       = $00000000;
  NIM_MODIFY                    = $00000001;
  NIM_DELETE                    = $00000002;
  NIM_SETFOCUS                  = $00000003;
  NIM_SETVERSION                = $00000004;

  NIF_MESSAGE                   = $00000001;
  NIF_ICON                      = $00000002;
  NIF_TIP                       = $00000004;
  NIF_STATE                     = $00000008;
  NIF_INFO                      = $00000010;

  NIS_HIDDEN                    = $00000001;
  NIS_SHAREDICON                = $00000002;

  NIIF_NONE                     = $00000000;
  NIIF_INFO                     = $00000001;
  NIIF_WARNING                  = $00000002;
  NIIF_ERROR                    = $00000003;

  NIN_SELECT                    = (WM_USER + 0);
  NINF_KEY                      = 1;
  NIN_KEYSELECT                 = (NIN_SELECT or NINF_KEY);

  NIN_BALLOONSHOW               = WM_USER + 2;
  NIN_BALLOONHIDE               = WM_USER + 3;
  NIN_BALLOONTIMEOUT            = WM_USER + 4;
  NIN_BALLOONUSERCLICK          = WM_USER + 5;

{**************************************************}

{$IFNDEF COMPILER6_UP}
function SecondsBetween(const Now: TDateTime; const FTime: TDateTime): Integer;
begin
  Result := Trunc(86400 * (FTime-Now));
end;
{$ENDIF COMPILER6_UP}




function TJvTrayIcon.AcceptBalloons: Boolean;
var
 Info: Pointer;
 InfoSize: DWORD;
 FileInfo: PVSFixedFileInfo;
 FileInfoSize: DWORD;
 Tmp: DWORD;
 Major: Integer;
begin
  //Balloons are only accepted with shell32.dll 5.0+
  result := false;
  try
    InfoSize := GetFileVersionInfoSize('shell32.dll', Tmp);
    if InfoSize = 0 then
      Exit;
    GetMem(Info, InfoSize);
    try
      GetFileVersionInfo('shell32.dll', 0, InfoSize, Info);
      VerQueryValue(Info, '\', Pointer(FileInfo), FileInfoSize);
      Major := FileInfo.dwFileVersionMS shr 16;
      result := Major>=5;
    finally
      FreeMem(Info, FileInfoSize);
    end;
  except
  end;
end;

{**************************************************}

constructor TJvTrayIcon.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TIcon.Create;
  if not (csDesigning in ComponentState) then
    FIcon.Assign(Application.Icon);
  FIcon.OnChange := IconChanged;
  FVisible := True;
  {$IFDEF Delphi6_UP}
  FHandle := Classes.AllocateHWnd(WndProc);
  {$ELSE}
  FHandle := AllocateHWnd(WndProc);
  {$ENDIF}

  FAnimated := False;
  FDelay := 100;
  FNumber := 0;
  FTimer := TTimer.Create(Self);
  FTImer.Interval := FDelay;
  FTimer.Enabled := FAnimated;
  FTimer.OnTimer := OnAnimateTimer;
  FActive := False;
  FTask := True;

  if not (csDesigning in ComponentState) then
  begin
    FDllHandle := LoadLibrary('KERNEL32.DLL');
    if FDllHandle <> 0 then
      @FRegisterServiceProcess := GetProcAddress(FDllHandle, 'RegisterServiceProcess')
    else
      @FRegisterServiceProcess := nil;
  end;
end;

{**************************************************}

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

{**************************************************}

destructor TJvTrayIcon.Destroy;
begin
  if not (csDestroying in Application.ComponentState) then
    SetTask(False);
  FTimer.Free;
  SetActive(False);
  FIcon.Free;
 {$IFDEF Delphi6_UP}
  Classes.DeallocateHWnd(FHandle);
  {$ELSE}
  DeallocateHWnd(FHandle);
  {$ENDIF}

  if not (csDesigning in ComponentState) then
    if FDllHandle <> 0 then
      FreeLibrary(FDllHandle);
  inherited;
end;

{**************************************************}

procedure TJvTrayIcon.WndProc(var Mesg: TMessage);
var
  i: Integer;
  po: TPoint;
  ShState: TShiftState;
begin
  try
    with Mesg do
      if Msg = WM_CALLBACKMESSAGE then
      begin
        GetCursorPos(po);
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
            DoMouseMove(shState,po.x,po.y);

          WM_LBUTTONDOWN:
            DoMouseDown(mbLeft, ShState, po.X, po.Y);
          WM_RBUTTONDOWN:
            DoMouseDown(mbRight, ShState, po.X, po.Y);
          WM_MBUTTONDOWN:
            DoMouseDown(mbMiddle, ShState, po.X, po.Y);

          WM_LBUTTONUP:
            DoMouseUp(mbLeft, ShState, po.x, po.y);
          WM_MBUTTONUP:
            DoMouseUp(mbMiddle, ShState, po.x, po.y);
          WM_RBUTTONUP,NIN_KEYSELECT: //Mimics previous versions of shell32.dll
            DoMouseUp(mbRight, ShState, po.x, po.y);

          WM_LBUTTONDBLCLK:
            DoDoubleClick(mbLeft, ShState, po.x, po.y);
          WM_RBUTTONDBLCLK:
            DoDoubleClick(mbRight, ShState, po.x, po.y);
          WM_MBUTTONDBLCLK:
            DoDoubleClick(mbMiddle, ShState, po.x, po.y);

          NIN_BALLOONHIDE: //sb
            begin
              if Assigned(FOnBalloonHide) then
                FOnBalloonHide(self);
              result := Integer(true);
            end;
          NIN_BALLOONTIMEOUT: //sb
            begin
              i := SecondsBetween(Now,FTime);
              if i>FTimeDelay then
                BalloonHint('','');
              result := Integer(true);
            end;
          NIN_BALLOONUSERCLICK: //sb
            begin
              if Assigned(FOnBalloonClick) then
                FOnBalloonClick(self);
              result := Integer(true);
              //Result := DefWindowProc(FHandle, Msg, wParam, lParam);
              BalloonHint('','');
            end;
        end;
      end
      else
        Result := DefWindowProc(FHandle, Msg, wParam, lParam);
  except
    Application.HandleException(Self);
  end;
end;

{**************************************************}

procedure TJvTrayIcon.IconChanged(Sender: TObject);
begin
  DoCheckCrash;
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, @fic);
end;

{**************************************************}

procedure TJvTrayIcon.SetHint(Value: string);
begin
  DoCheckCrash;
  if FHint<>Value then
  begin
    FHint := Value;
    with FIc do
      StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
    Fic.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
    if FActive then
      Shell_NotifyIcon(NIM_MODIFY, @fic);
  end;
end;

{**************************************************}

procedure TJvTrayIcon.SetIcon(Icon: TIcon);
begin
  DoCheckCrash;
  FIcon.Assign(ICon);
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, @fic);
end;

{**************************************************}

procedure TJvTrayIcon.SetActive(Value: Boolean);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/Structures/NOTIFYICONDATA.asp
begin
  FActive := Value;
  if not (csDesigning in ComponentState) then
  begin
    if Value then
    begin
      if FIcon = nil then
        FIcon.Assign(Application.Icon);
      with FIc do
      begin
        if AcceptBalloons then
        begin
          cbsize := SizeOf(Fic);
          fic.uTimeOut := NOTIFYICON_VERSION;
        end
        else
          cbsize := SizeOf(TNotifyIconData);
        wnd := Fhandle;
        uId := 1;
        uCallBackMessage := WM_CALLBACKMESSAGE;
        if FIcon<>nil then
          hIcon := FIcon.Handle
        else
          FIcon := Application.Icon;
        StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
        uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
      end;
      Shell_NotifyIcon(NIM_ADD,@Fic);
      if AcceptBalloons then
        Shell_NotifyIcon(NIM_SETVERSION,@Fic);

      FOldTray := FindWindow('Shell_TrayWnd',nil);
    end
    else
      Shell_NotifyIcon(NIM_DELETE, @FIc);
  end;
end;

{**************************************************}

procedure TJvTrayIcon.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  if not (csDesigning in ComponentState) then
    if Value then
      ShowApplication
    else
      HideApplication;
end;

{**************************************************}

procedure TJvTrayIcon.HideApplication;
begin
  Application.Minimize;
  ShowWindow(Application.Handle, SW_HIDE);
end;

{**************************************************}

procedure TJvTrayIcon.ShowApplication;
begin
  ShowWindow(Application.Handle, SW_SHOW);
  Application.Restore;
end;

{**************************************************}

procedure TJvTrayIcon.OnAnimateTimer(Sender: TObject);
begin
  if (FActive) then
  begin
    if IconIndex<0 then
      IconIndex := 0
    else
      IconIndex := (IconIndex + 1) mod FimgList.Count;
    if Assigned(FOnAnimate) then
      FOnAnimate(self, IconIndex);
  end;
end;

{**************************************************}

procedure TJvTrayIcon.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  if not (csDesigning in ComponentState) then
    FTimer.Enabled := FAnimated;
end;

{**************************************************}

procedure TJvTrayIcon.SetDelay(const Value: Cardinal);
begin
  FDelay := Value;
  FTimer.Interval := FDelay;
end;

{**************************************************}

procedure TJvTrayIcon.SetImgList(const Value: TImageList);
begin
  FImgList := Value;
end;


{**************************************************}

procedure TJvTrayIcon.BalloonHint(Title, Value: string;
  BalloonType: TBalloonType; Delay: Integer);
//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/Shell/reference/functions/shell_notifyicon.asp
begin
  if AcceptBalloons then
  begin
    FTime := Now;
    FTimeDelay := Delay div 1000;
    Fic.uFlags := NIF_INFO;
    with Fic do
      StrPLCopy(szInfoTitle, Title, SizeOf(szInfoTitle) - 1);
    with Fic do
      StrPLCopy(szInfo, Value, SizeOf(szInfo) - 1);
    Fic.uFlags := NIF_MESSAGE or NIF_ICON or NIF_INFO or NIF_TIP;
    Fic.uTimeOut := Delay;
    case BalloonType of
      btError: Fic.dwInfoFlags := NIIF_ERROR;
      btInfo: Fic.dwInfoFlags := NIIF_INFO;
      btNone: Fic.dwInfoFlags := NIIF_NONE;
      btWarning: Fic.dwInfoFlags := NIIF_WARNING;
    end;
    Shell_NotifyIcon(NIM_MODIFY,@Fic);

    if Assigned(FOnBalloonShow) then
      FOnBalloonShow(self);
  end;
end;

{**************************************************}

procedure TJvTrayIcon.DoCheckCrash;
var
 hwndTray:HWND;
begin
  if Active then
  begin
    hwndTray := FindWindow('Shell_TrayWnd',nil);
    if FOldTray<>hWndTray then
    begin
      FOldTray := hWndTray;
      Active := false;
      Active := true;
    end;
  end;
end;

{**************************************************}

procedure TJvTrayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

{**************************************************}

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

{**************************************************}

procedure TJvTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbRight) and (FPopupMenu <> nil) then
  begin
    SetForegroundWindow(FHandle);
    FPopupMenu.Popup(X, Y);
    PostMessage(FHandle, WM_NULL, 0, 0);
  end;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if Assigned(FOnClick) then
    FOnClick(Self, Button, Shift, X, Y);
end;

{**************************************************}

procedure TJvTrayIcon.DoDoubleClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self, Button, Shift, X, Y)
  else if (Button = mbLeft) and (FPopupMenu <> nil) then
  begin
    for i := 0 to FPopupMenu.Items.Count - 1 do
      if FPopupMenu.Items[i].Default then
      begin
        FPopupMenu.Items[i].Click;
        Break;
      end;
  end;
end;

{**************************************************}

procedure TJvTrayIcon.SetNumber(const Value: Integer);
var
  ico: TIcon;
begin
  FNumber := Value;
  if (FImglist <> nil) and (FNumber>=0) and (FNumber<FImgList.Count) then
  begin
    ico := TIcon.Create;
    try
      Fic.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      FImgList.GetIcon(FNumber, ico);
      SetIcon(ico);
    finally
      ico.Free;
    end;
  end
  else
    FNumber := -1;
end;

end.
