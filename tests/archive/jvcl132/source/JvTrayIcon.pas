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

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvTrayIcon;

{$ObjExportAll On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,  ExtCtrls, Menus, ShellApi,  JvTypes,JvComponent;

// (rom) Heavily modified this one. Mouse and Click events completely redesigned.

type
  TRegisterServiceProcess = function(dwProcessID, dwType: Integer): Integer; stdcall;

  TJvTrayIcon = class(TJvComponent)
  private
    FActive: Boolean;
    FIcon: TIcon;
    FIc: TNotifyIconData;
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
    procedure SetActive(Value: Boolean);
    procedure SetHint(Value: string);
    procedure SetIcon(Icon: TIcon);
    procedure SetVisible(Value: Boolean);
    procedure OnAnimate(Sender: TObject);
    procedure SetAnimated(const Value: Boolean);
    procedure SetDelay(const Value: Cardinal);
    procedure SetImgList(const Value: TImageList);
    procedure IconChanged(Sender: TObject);
    procedure SetTask(const Value: Boolean);
  public
    procedure WndProc(var Mesg: TMessage);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Icon: TIcon read FIcon write SetIcon;
    property Hint: string read FHint write SetHint;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property ApplicationVisible: Boolean read FVisible write SetVisible default True;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Icons: TImageList read FImgList write SetImgList;
    property Delay: Cardinal read FDelay write SetDelay default 100;
    property OnClick: TMouseEvent read FOnClick write FOnClick;
    property OnDblClick: TMouseEvent read FOnDblClick write FOnDblClick;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    procedure HideApplication;
    procedure ShowApplication;
    property DropDownMenu: TPopupMenu read FDropDown write FDropDown;
    property VisibleInTaskList: Boolean read FTask write SetTask default True;
  end;

implementation

const
  WM_CALLBACKMESSAGE = WM_USER + 1;

{**************************************************}

constructor TJvTrayIcon.Create(AOwner: TComponent);
begin
  inherited;
  FIcon := TIcon.Create;
  if not (csDesigning in ComponentState) then
    FIcon.Assign(Application.Icon);
  FIcon.OnChange := IconChanged;
  FVisible := True;
  FHandle := AllocateHWnd(WndProc);
  FAnimated := False;
  FDelay := 100;
  FNumber := 0;
  FTimer := TTimer.Create(Self);
  FTImer.Interval := FDelay;
  FTimer.Enabled := FAnimated;
  FTimer.OnTimer := OnAnimate;
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
      if @FRegisterServiceProcess <> nil then
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
  DeallocateHWnd(FHandle);
  if not (csDesigning in ComponentState) then
    if FDllHandle <> 0 then
      FreeLibrary(FDllHandle);
  inherited;
end;

{**************************************************}

procedure TJvTrayIcon.WndProc(var Mesg: TMessage);
var
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
            begin
              if Assigned(FOnMouseMove) then
                FOnMouseMove(Self, ShState, po.x, po.y);
            end;
          WM_LBUTTONDOWN:
            begin
              if Assigned(FOnMouseDown) then
                FOnMouseDown(Self, mbLeft, ShState, po.x, po.y);
              if FDropDown <> nil then
                FDropDown.Popup(po.x, po.y);
            end;
          WM_LBUTTONUP:
            begin
              if Assigned(FOnMouseUp) then
                FOnMouseUp(Self, mbLeft, ShState, po.x, po.y);
              if Assigned(FOnClick) then
                FOnClick(Self, mbLeft, ShState, po.x, po.y);
            end;
          WM_LBUTTONDBLCLK:
            if Assigned(FOnDblClick) then
              FOnDblClick(Self, mbLeft, ShState, po.x, po.y);
          WM_RBUTTONDOWN:
            if Assigned(FOnMouseDown) then
              FOnMouseDown(Self, mbRight, ShState, po.x, po.y);
          WM_RBUTTONUP:
            begin
              if FPopupMenu <> nil then
                FPopupMenu.Popup(po.x, po.y);
              if Assigned(FOnMouseUp) then
                FOnMouseUp(Self, mbRight, ShState, po.x, po.y);
              if Assigned(FOnClick) then
                FOnClick(Self, mbRight, ShState, po.x, po.y);
            end;
          WM_RBUTTONDBLCLK:
            if Assigned(FOnDblClick) then
              FOnDblClick(Self, mbRight, ShState, po.x, po.y);
          WM_MBUTTONDOWN:
            if Assigned(FOnMouseDown) then
              FOnMouseDown(Self, mbMiddle, ShState, po.x, po.y);
          WM_MBUTTONUP:
            begin
              if Assigned(FOnMouseUp) then
                FOnMouseUp(Self, mbMiddle, ShState, po.x, po.y);
              if Assigned(FOnClick) then
                FOnClick(Self, mbMiddle, ShState, po.x, po.y);
            end;
          WM_MBUTTONDBLCLK:
            if Assigned(FOnDblClick) then
              FOnDblClick(Self, mbMiddle, ShState, po.x, po.y);
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
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, @fic);
end;

{**************************************************}

procedure TJvTrayIcon.SetHint(Value: string);
begin
  FHint := Value;
  with FIc do
    StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, @fic);
end;

{**************************************************}

procedure TJvTrayIcon.SetIcon(Icon: TIcon);
begin
  FIcon.Assign(ICon);
  with FIc do
    hIcon := FIcon.Handle;
  if FActive then
    Shell_NotifyIcon(NIM_MODIFY, @fic);
end;

{**************************************************}

procedure TJvTrayIcon.SetActive(Value: Boolean);
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
        cbSize := SizeOf(FIc);
        Wnd := FHandle;
        Uid := 1;
        uCallBackMessage := WM_CALLBACKMESSAGE;
        if FIcon <> nil then
          hicon := Ficon.Handle
        else
          FIcon := Application.Icon;
        StrPLCopy(szTip, GetShortHint(FHint), SizeOf(szTip) - 1);
        uflags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
      end;
      Shell_NotifyIcon(NIM_ADD, @FIc);
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
  ShowWindow(Application.Handle, SW_HIDE);
end;

{**************************************************}

procedure TJvTrayIcon.ShowApplication;
begin
  ShowWindow(Application.Handle, SW_SHOW);
end;

{**************************************************}

procedure TJvTrayIcon.OnAnimate(Sender: TObject);
var
  ico: TIcon;
begin
  if (FImglist <> nil) and (FImgList.Count > 0) and FActive then
  begin
    ico := TIcon.Create;
    FNumber := (FNumber + 1) mod FimgList.Count;
    FImgList.GetIcon(FNumber, ico);
    SetIcon(ico);
    ico.Free;
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

end.
