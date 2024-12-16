{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvShellHook.pas, released on 2002-10-27.

The Initial Developer of the Original Code is Peter Thornqvist <peter3 at sourceforge dot net>.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

Description:
  A wrapper for the Register/DeregisterShellHookWindow functions recently documented by Microsoft.
  See MSDN (http://msdn.microsoft.com, search for "RegisterShellHookWindow") for more details
  NOTE: this might not work on all OS'es and versions!

-----------------------------------------------------------------------------}
// $Id$

unit JvShellHook;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes,
  JvComponentBase;

type
  PShellHookInfo = ^TShellHookInfo;
  TShellHookInfo = record
    hwnd: THandle;
    rc: TRect;
  end;
  SHELLHOOKINFO = TShellHookInfo;
  {$EXTERNALSYM SHELLHOOKINFO}
  LPSHELLHOOKINFO = PShellHookInfo;
  {$EXTERNALSYM LPSHELLHOOKINFO}

type
  TJvShellHookEvent = procedure(Sender: TObject; var Msg: TMessage) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvShellHook = class(TJvComponent)
  private
    FWndHandle: THandle;
    FHookMsg: Cardinal;
    FOnShellMessage: TJvShellHookEvent;
    FActive: Boolean;
    procedure SetActive(Value: Boolean);
  protected
    procedure DoShellMessage(var Msg: TMessage); dynamic;
    procedure ShellHookMethod(var Msg: TMessage);
  public
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property OnShellMessage: TJvShellHookEvent read FOnShellMessage write FOnShellMessage;
  end;

// load DLL and init function pointers
function InitJvShellHooks: Boolean;
// unload DLL and clear function pointers
procedure UnInitJvShellHooks;

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
  JvJVCLUtils;

type
  TRegisterShellHookWindowFunc = function(THandle: HWND): BOOL; stdcall;

var
  RegisterShellHookWindow: TRegisterShellHookWindowFunc = nil;
  DeregisterShellHookWindow: TRegisterShellHookWindowFunc = nil;
  GlobalLibHandle: THandle = 0;

procedure UnInitJvShellHooks;
begin
  RegisterShellHookWindow := nil;
  DeregisterShellHookWindow := nil;
  GlobalLibHandle := 0;
end;

function InitJvShellHooks: Boolean;
begin
  if GlobalLibHandle = 0 then
  begin
    GlobalLibHandle := GetModuleHandle(user32);
    if GlobalLibHandle > 0 then
    begin
      RegisterShellHookWindow := GetProcAddress(GlobalLibHandle, 'RegisterShellHookWindow');
      DeregisterShellHookWindow := GetProcAddress(GlobalLibHandle, 'DeregisterShellHookWindow');
    end;
  end;
  Result := (GlobalLibHandle <> 0) and Assigned(RegisterShellHookWindow) and Assigned(DeregisterShellHookWindow);
end;

destructor TJvShellHook.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TJvShellHook.DoShellMessage(var Msg: TMessage);
begin
  if Assigned(FOnShellMessage) then
    FOnShellMessage(Self, Msg);
end;

procedure TJvShellHook.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if csDesigning in ComponentState then
    begin
      FActive := Value;
      Exit;
    end;
    if FActive and (FWndHandle <> 0) then
    begin
      DeregisterShellHookWindow(FWndHandle);
      DeallocateHWndEx(FWndHandle);
    end;
    FWndHandle := 0;
    if Value then
    begin
      if not InitJvShellHooks then
        Exit; // raise ?
      FWndHandle := AllocateHWndEx(ShellHookMethod);
      if FWndHandle <> 0 then
        FHookMsg := RegisterWindowMessage('SHELLHOOK'); // do not localize
      if not RegisterShellHookWindow(FWndHandle) then
        Value := False;
    end;
    FActive := Value;
  end;
end;

procedure TJvShellHook.ShellHookMethod(var Msg: TMessage);
begin
  if Msg.Msg = FHookMsg then
    DoShellMessage(Msg)
  else
    with Msg do
      Result := DefWindowProc(FWndHandle, Msg, WParam, LParam);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  UnInitJvShellHooks;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
