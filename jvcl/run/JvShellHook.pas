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
located at http://jvcl.sourceforge.net

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
  Windows, Messages, SysUtils, Classes,
  JvComponent;

const
  HSHELL_WINDOWCREATED = 1;
  {$EXTERNALSYM HSHELL_WINDOWCREATED}
  HSHELL_WINDOWDESTROYED = 2;
  {$EXTERNALSYM HSHELL_WINDOWDESTROYED}
  HSHELL_ACTIVATESHELLWINDOW = 3;
  {$EXTERNALSYM HSHELL_ACTIVATESHELLWINDOW}

  HSHELL_WINDOWACTIVATED = 4;
  {$EXTERNALSYM HSHELL_WINDOWACTIVATED}
  HSHELL_GETMINRECT = 5;
  {$EXTERNALSYM HSHELL_GETMINRECT}
  HSHELL_REDRAW = 6;
  {$EXTERNALSYM HSHELL_REDRAW}
  HSHELL_TASKMAN = 7;
  {$EXTERNALSYM HSHELL_TASKMAN}
  HSHELL_LANGUAGE = 8;
  {$EXTERNALSYM HSHELL_LANGUAGE}
  HSHELL_SYSMENU = 9;
  {$EXTERNALSYM HSHELL_SYSMENU}
  HSHELL_ENDTASK = 10;
  {$EXTERNALSYM HSHELL_ENDTASK}
  HSHELL_ACCESSIBILITYSTATE = 11;
  {$EXTERNALSYM HSHELL_ACCESSIBILITYSTATE}
  HSHELL_APPCOMMAND = 12;
  {$EXTERNALSYM HSHELL_APPCOMMAND}
  HSHELL_WINDOWREPLACED = 13;
  {$EXTERNALSYM HSHELL_WINDOWREPLACED}
  HSHELL_WINDOWREPLACING = 14;
  {$EXTERNALSYM HSHELL_WINDOWREPLACING}

  HSHELL_HIGHBIT = $8000;
  {$EXTERNALSYM HSHELL_HIGHBIT}
  HSHELL_FLASH = (HSHELL_REDRAW or HSHELL_HIGHBIT);
  {$EXTERNALSYM HSHELL_FLASH}
  HSHELL_RUDEAPPACTIVATED = (HSHELL_WINDOWACTIVATED or HSHELL_HIGHBIT);
  {$EXTERNALSYM HSHELL_RUDEAPPACTIVATED}

  (* wparam for HSHELL_ACCESSIBILITYSTATE *)
  ACCESS_STICKYKEYS = $0001;
  {$EXTERNALSYM ACCESS_STICKYKEYS}
  ACCESS_FILTERKEYS = $0002;
  {$EXTERNALSYM ACCESS_FILTERKEYS}
  ACCESS_MOUSEKEYS = $0003;
  {$EXTERNALSYM ACCESS_MOUSEKEYS}

  (* cmd for HSHELL_APPCOMMAND and WM_APPCOMMAND *)
  APPCOMMAND_BROWSER_BACKWARD = 1;
  {$EXTERNALSYM APPCOMMAND_BROWSER_BACKWARD}
  APPCOMMAND_BROWSER_FORWARD = 2;
  {$EXTERNALSYM APPCOMMAND_BROWSER_FORWARD}
  APPCOMMAND_BROWSER_REFRESH = 3;
  {$EXTERNALSYM APPCOMMAND_BROWSER_REFRESH}
  APPCOMMAND_BROWSER_STOP = 4;
  {$EXTERNALSYM APPCOMMAND_BROWSER_STOP}
  APPCOMMAND_BROWSER_SEARCH = 5;
  {$EXTERNALSYM APPCOMMAND_BROWSER_SEARCH}
  APPCOMMAND_BROWSER_FAVORITES = 6;
  {$EXTERNALSYM APPCOMMAND_BROWSER_FAVORITES}
  APPCOMMAND_BROWSER_HOME = 7;
  {$EXTERNALSYM APPCOMMAND_BROWSER_HOME}
  APPCOMMAND_VOLUME_MUTE = 8;
  {$EXTERNALSYM APPCOMMAND_VOLUME_MUTE}
  APPCOMMAND_VOLUME_DOWN = 9;
  {$EXTERNALSYM APPCOMMAND_VOLUME_DOWN}
  APPCOMMAND_VOLUME_UP = 10;
  {$EXTERNALSYM APPCOMMAND_VOLUME_UP}
  APPCOMMAND_MEDIA_NEXTTRACK = 11;
  {$EXTERNALSYM APPCOMMAND_MEDIA_NEXTTRACK}
  APPCOMMAND_MEDIA_PREVIOUSTRACK = 12;
  {$EXTERNALSYM APPCOMMAND_MEDIA_PREVIOUSTRACK}
  APPCOMMAND_MEDIA_STOP = 13;
  {$EXTERNALSYM APPCOMMAND_MEDIA_STOP}
  APPCOMMAND_MEDIA_PLAY_PAUSE = 14;
  {$EXTERNALSYM APPCOMMAND_MEDIA_PLAY_PAUSE}
  APPCOMMAND_LAUNCH_MAIL = 15;
  {$EXTERNALSYM APPCOMMAND_LAUNCH_MAIL}
  APPCOMMAND_LAUNCH_MEDIA_SELECT = 16;
  {$EXTERNALSYM APPCOMMAND_LAUNCH_MEDIA_SELECT}
  APPCOMMAND_LAUNCH_APP1 = 17;
  {$EXTERNALSYM APPCOMMAND_LAUNCH_APP1}
  APPCOMMAND_LAUNCH_APP2 = 18;
  {$EXTERNALSYM APPCOMMAND_LAUNCH_APP2}
  APPCOMMAND_BASS_DOWN = 19;
  {$EXTERNALSYM APPCOMMAND_BASS_DOWN}
  APPCOMMAND_BASS_BOOST = 20;
  {$EXTERNALSYM APPCOMMAND_BASS_BOOST}
  APPCOMMAND_BASS_UP = 21;
  {$EXTERNALSYM APPCOMMAND_BASS_UP}
  APPCOMMAND_TREBLE_DOWN = 22;
  {$EXTERNALSYM APPCOMMAND_TREBLE_DOWN}
  APPCOMMAND_TREBLE_UP = 23;
  {$EXTERNALSYM APPCOMMAND_TREBLE_UP}
  APPCOMMAND_MICROPHONE_VOLUME_MUTE = 24;
  {$EXTERNALSYM APPCOMMAND_MICROPHONE_VOLUME_MUTE}
  APPCOMMAND_MICROPHONE_VOLUME_DOWN = 25;
  {$EXTERNALSYM APPCOMMAND_MICROPHONE_VOLUME_DOWN}
  APPCOMMAND_MICROPHONE_VOLUME_UP = 26;
  {$EXTERNALSYM APPCOMMAND_MICROPHONE_VOLUME_UP}
  APPCOMMAND_HELP = 27;
  {$EXTERNALSYM APPCOMMAND_HELP}
  APPCOMMAND_FIND = 28;
  {$EXTERNALSYM APPCOMMAND_FIND}
  APPCOMMAND_NEW = 29;
  {$EXTERNALSYM APPCOMMAND_NEW}
  APPCOMMAND_OPEN = 30;
  {$EXTERNALSYM APPCOMMAND_OPEN}
  APPCOMMAND_CLOSE = 31;
  {$EXTERNALSYM APPCOMMAND_CLOSE}
  APPCOMMAND_SAVE = 32;
  {$EXTERNALSYM APPCOMMAND_SAVE}
  APPCOMMAND_PRINT = 33;
  {$EXTERNALSYM APPCOMMAND_PRINT}
  APPCOMMAND_UNDO = 34;
  {$EXTERNALSYM APPCOMMAND_UNDO}
  APPCOMMAND_REDO = 35;
  {$EXTERNALSYM APPCOMMAND_REDO}
  APPCOMMAND_COPY = 36;
  {$EXTERNALSYM APPCOMMAND_COPY}
  APPCOMMAND_CUT = 37;
  {$EXTERNALSYM APPCOMMAND_CUT}
  APPCOMMAND_PASTE = 38;
  {$EXTERNALSYM APPCOMMAND_PASTE}
  APPCOMMAND_REPLY_TO_MAIL = 39;
  {$EXTERNALSYM APPCOMMAND_REPLY_TO_MAIL}
  APPCOMMAND_FORWARD_MAIL = 40;
  {$EXTERNALSYM APPCOMMAND_FORWARD_MAIL}
  APPCOMMAND_SEND_MAIL = 41;
  {$EXTERNALSYM APPCOMMAND_SEND_MAIL}
  APPCOMMAND_SPELL_CHECK = 42;
  {$EXTERNALSYM APPCOMMAND_SPELL_CHECK}
  APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE = 43;
  {$EXTERNALSYM APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE}
  APPCOMMAND_MIC_ON_OFF_TOGGLE = 44;
  {$EXTERNALSYM APPCOMMAND_MIC_ON_OFF_TOGGLE}
  APPCOMMAND_CORRECTION_LIST = 45;
  {$EXTERNALSYM APPCOMMAND_CORRECTION_LIST}
  APPCOMMAND_MEDIA_PLAY = 46;
  {$EXTERNALSYM APPCOMMAND_MEDIA_PLAY}
  APPCOMMAND_MEDIA_PAUSE = 47;
  {$EXTERNALSYM APPCOMMAND_MEDIA_PAUSE}
  APPCOMMAND_MEDIA_RECORD = 48;
  {$EXTERNALSYM APPCOMMAND_MEDIA_RECORD}
  APPCOMMAND_MEDIA_FAST_FORWARD = 49;
  {$EXTERNALSYM APPCOMMAND_MEDIA_FAST_FORWARD}
  APPCOMMAND_MEDIA_REWIND = 50;
  {$EXTERNALSYM APPCOMMAND_MEDIA_REWIND}
  APPCOMMAND_MEDIA_CHANNEL_UP = 51;
  {$EXTERNALSYM APPCOMMAND_MEDIA_CHANNEL_UP}
  APPCOMMAND_MEDIA_CHANNEL_DOWN = 52;
  {$EXTERNALSYM APPCOMMAND_MEDIA_CHANNEL_DOWN}

  FAPPCOMMAND_MOUSE = $8000;
  {$EXTERNALSYM FAPPCOMMAND_MOUSE}
  FAPPCOMMAND_KEY = 0;
  {$EXTERNALSYM FAPPCOMMAND_KEY}
  FAPPCOMMAND_OEM = $1000;
  {$EXTERNALSYM FAPPCOMMAND_OEM}
  FAPPCOMMAND_MASK = $F000;
  {$EXTERNALSYM FAPPCOMMAND_MASK}

type
  PShellHookInfo = ^TShellHookInfo;
  TShellHookInfo = record
    hwnd: HWND;
    rc: TRect;
  end;
  SHELLHOOKINFO = TShellHookInfo;
  {$EXTERNALSYM SHELLHOOKINFO}
  LPSHELLHOOKINFO = PShellHookInfo;
  {$EXTERNALSYM LPSHELLHOOKINFO}

type
  TJvShellHookEvent = procedure(Sender: TObject; var Msg: TMessage) of object;

  TJvShellHook = class(TJvComponent)
  private
    FWndHandle: HWND;
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

function GET_APPCOMMAND_LPARAM(lParam: Integer): Word;
{$EXTERNALSYM GET_APPCOMMAND_LPARAM}
function GET_DEVICE_LPARAM(lParam: Integer): Word;
{$EXTERNALSYM GET_DEVICE_LPARAM}
function GET_MOUSEORKEY_LPARAM(lParam: Integer): Integer;
{$EXTERNALSYM GET_MOUSEORKEY_LPARAM}
function GET_FLAGS_LPARAM(lParam: Integer): Word;
{$EXTERNALSYM GET_FLAGS_LPARAM}
function GET_KEYSTATE_LPARAM(lParam: Integer): Word;
{$EXTERNALSYM GET_KEYSTATE_LPARAM}

// load DLL and init function pointers
function InitJvShellHooks: Boolean;
// unload DLL and clear function pointers
procedure UnInitJvShellHooks;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFNDEF COMPILER6_UP}
  Forms,
  {$ENDIF COMPILER6_UP}
  JvJVCLUtils;

const
  cUser32 = 'user32.dll';

// converted macros

function GET_APPCOMMAND_LPARAM(lParam: Integer): Word;
begin
  Result := HiWord(lParam) and not FAPPCOMMAND_MASK;
end;

function GET_DEVICE_LPARAM(lParam: Integer): Word;
begin
  Result := HiWord(lParam) and FAPPCOMMAND_MASK;
end;

function GET_MOUSEORKEY_LPARAM(lParam: Integer): Integer;
begin
  Result := GET_DEVICE_LPARAM(lParam);
end;

function GET_FLAGS_LPARAM(lParam: Integer): Word;
begin
  Result := LoWord(lParam);
end;

function GET_KEYSTATE_LPARAM(lParam: Integer): Word;
begin
  Result := GET_FLAGS_LPARAM(lParam);
end;

type
  TRegisterShellHookWindowFunc = function(hWnd: HWND): BOOL; stdcall;

var
  RegisterShellHookWindow: TRegisterShellHookWindowFunc = nil;
  DeregisterShellHookWindow: TRegisterShellHookWindowFunc = nil;
  GlobalLibHandle: HMODULE = 0;

procedure UnInitJvShellHooks;
begin
  RegisterShellHookWindow := nil;
  DeregisterShellHookWindow := nil;
  if GlobalLibHandle > 0 then
    FreeLibrary(GlobalLibHandle);
  GlobalLibHandle := 0;
end;

function InitJvShellHooks: Boolean;
begin
  Result := True;
  if GlobalLibHandle > 0 then
    Exit; // already done this
  GlobalLibHandle := LoadLibrary(cUser32);
  if GlobalLibHandle > 0 then
  begin
    RegisterShellHookWindow := GetProcAddress(GlobalLibHandle, 'RegisterShellHookWindow');
    DeregisterShellHookWindow := GetProcAddress(GlobalLibHandle, 'DeregisterShellHookWindow');
  end;
  Result := (GlobalLibHandle > 0) and Assigned(RegisterShellHookWindow) and Assigned(DeregisterShellHookWindow);
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
    // (rom) why inherited? The method is introduced here.
    inherited;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

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

