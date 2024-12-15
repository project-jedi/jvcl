{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppCommand.PAS, released on 2005-09-02.

The Initial Developer of the Original Code is Robert Marquardt [robert_marquardt att dmx dott de]
Portions created by Robert Marquardt are Copyright (C) 2001 Robert Marquardt.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppCommand;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  JvComponentBase;

const
  // from JwaWinUser.pas
  WM_APPCOMMAND = $0319;

  // Cmd values
  APPCOMMAND_BROWSER_BACKWARD    = 1;
  APPCOMMAND_BROWSER_FORWARD     = 2;
  APPCOMMAND_BROWSER_REFRESH     = 3;
  APPCOMMAND_BROWSER_STOP        = 4;
  APPCOMMAND_BROWSER_SEARCH      = 5;
  APPCOMMAND_BROWSER_FAVORITES   = 6;
  APPCOMMAND_BROWSER_HOME        = 7;
  APPCOMMAND_VOLUME_MUTE         = 8;
  APPCOMMAND_VOLUME_DOWN         = 9;
  APPCOMMAND_VOLUME_UP           = 10;
  APPCOMMAND_MEDIA_NEXTTRACK     = 11;
  APPCOMMAND_MEDIA_PREVIOUSTRACK = 12;
  APPCOMMAND_MEDIA_STOP          = 13;
  APPCOMMAND_MEDIA_PLAY_PAUSE    = 14;
  APPCOMMAND_LAUNCH_MAIL         = 15;
  APPCOMMAND_LAUNCH_MEDIA_SELECT = 16;
  APPCOMMAND_LAUNCH_APP1         = 17;
  APPCOMMAND_LAUNCH_APP2         = 18;
  APPCOMMAND_BASS_DOWN           = 19;
  APPCOMMAND_BASS_BOOST          = 20;
  APPCOMMAND_BASS_UP             = 21;
  APPCOMMAND_TREBLE_DOWN         = 22;
  APPCOMMAND_TREBLE_UP           = 23;

  APPCOMMAND_MICROPHONE_VOLUME_MUTE = 24;
  APPCOMMAND_MICROPHONE_VOLUME_DOWN = 25;
  APPCOMMAND_MICROPHONE_VOLUME_UP   = 26;
  APPCOMMAND_HELP                   = 27;
  APPCOMMAND_FIND                   = 28;
  APPCOMMAND_NEW                    = 29;
  APPCOMMAND_OPEN                   = 30;
  APPCOMMAND_CLOSE                  = 31;
  APPCOMMAND_SAVE                   = 32;
  APPCOMMAND_PRINT                  = 33;
  APPCOMMAND_UNDO                   = 34;
  APPCOMMAND_REDO                   = 35;
  APPCOMMAND_COPY                   = 36;
  APPCOMMAND_CUT                    = 37;
  APPCOMMAND_PASTE                  = 38;
  APPCOMMAND_REPLY_TO_MAIL          = 39;
  APPCOMMAND_FORWARD_MAIL           = 40;
  APPCOMMAND_SEND_MAIL              = 41;
  APPCOMMAND_SPELL_CHECK            = 42;
  APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE = 43;
  APPCOMMAND_MIC_ON_OFF_TOGGLE      = 44;
  APPCOMMAND_CORRECTION_LIST        = 45;

  APPCOMMAND_MEDIA_PLAY             = 46;
  APPCOMMAND_MEDIA_PAUSE            = 47;
  APPCOMMAND_MEDIA_RECORD           = 48;
  APPCOMMAND_MEDIA_FAST_FORWARD     = 49;
  APPCOMMAND_MEDIA_REWIND           = 50;
  APPCOMMAND_MEDIA_CHANNEL_UP       = 51;
  APPCOMMAND_MEDIA_CHANNEL_DOWN     = 52;

  // KeyState bit values
  MK_LBUTTON  = $0001;
  MK_RBUTTON  = $0002;
  MK_SHIFT    = $0004;
  MK_CONTROL  = $0008;
  MK_MBUTTON  = $0010;
  MK_XBUTTON1 = $0020;
  MK_XBUTTON2 = $0040;

type
  // source of app command
  TJvAppCommandDevice = (acdKey, acdMouse, acdOEM);

  TJvAppCommandEvent = procedure(Handle: THandle;
    Cmd: WORD; Device: TJvAppCommandDevice; KeyState: WORD;
    var Handled: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAppCommand = class(TJvComponent)
  private
    FActive: Boolean;
    FOnAppCommand: TJvAppCommandEvent;
    FForm: TCustomForm;
    procedure SetActive(Value: Boolean);
    function NewWndProc(var Msg: TMessage): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default True;
    property OnAppCommand: TJvAppCommandEvent read FOnAppCommand write FOnAppCommand;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

// Required for outside use (see the MegaDemo for instance)
function GET_APPCOMMAND_LPARAM(lParam: LPARAM): WORD;
function GET_DEVICE_LPARAM(lParam: LPARAM): WORD;
function GET_KEYSTATE_LPARAM(lParam: LPARAM): WORD;

implementation

uses
  JvWndProcHook, JvResources;

const
  // from JwaWinUser.pas
  FAPPCOMMAND_MOUSE = $8000;
  FAPPCOMMAND_KEY   = 0;
  FAPPCOMMAND_OEM   = $1000;
  FAPPCOMMAND_MASK  = $F000;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): WORD;
begin
  Result := WORD(HIWORD(lParam) and not FAPPCOMMAND_MASK);
end;

function GET_DEVICE_LPARAM(lParam: LPARAM): WORD;
begin
  Result := WORD(HIWORD(lParam) and FAPPCOMMAND_MASK);
end;

function GET_KEYSTATE_LPARAM(lParam: LPARAM): WORD;
begin
  Result := LOWORD(lParam);
end;

//=== { TJvAppCommand } ======================================================

constructor TJvAppCommand.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := nil;
  if AOwner is TControl then
    FForm := GetParentForm(AOwner as TControl);
  if FForm = nil then
    raise Exception.CreateResFmt(@RsEOwnerMustBeForm, [ClassName]);
  Active := True;
end;

destructor TJvAppCommand.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TJvAppCommand.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if (FForm <> nil) and not (csDesigning in ComponentState) then
      if Value then
        RegisterWndProcHook(FForm, NewWndProc, hoBeforeMsg)
      else
        UnregisterWndProcHook(FForm, NewWndProc, hoBeforeMsg);
  end;
end;

function TJvAppCommand.NewWndProc(var Msg: TMessage): Boolean;
var
  Dev: TJvAppCommandDevice;
begin
  Result := False;
  if (Msg.Msg = WM_APPCOMMAND) and Active then
  begin
    Msg.Result := 1;
    Result := True;
    if Assigned(FOnAppCommand) then
    begin
      case GET_DEVICE_LPARAM(Msg.LParam) of
        FAPPCOMMAND_MOUSE:
          Dev := acdMouse;
        FAPPCOMMAND_OEM:
          Dev := acdOEM;
      else
        Dev := acdKey;
      end;

      FOnAppCommand(THandle(Msg.WParam), GET_APPCOMMAND_LPARAM(Msg.LParam),
        Dev, GET_KEYSTATE_LPARAM(Msg.LParam), Result);
      Msg.Result := Ord(Result);
    end;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
