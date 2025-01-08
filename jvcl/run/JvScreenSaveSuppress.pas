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

unit JvScreenSaveSuppress;

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
  SC_SCREENSAVE = $F140;
  {$EXTERNALSYM SC_SCREENSAVE}

type
  TJvScreenSaveEvent = procedure(var Handled: Boolean) of object;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvScreenSaveSuppressor = class(TJvComponent)
  private
    FActive: Boolean;
    FOnScreenSave: TJvScreenSaveEvent;
    FForm: TCustomForm;
    function NewWndProc(var Msg: TMessage): Boolean;
    procedure SetActive(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive default True;
    property OnScreenSave: TJvScreenSaveEvent read FOnScreenSave write FOnScreenSave;
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

implementation

uses
  JvWndProcHook;

constructor TJvScreenSaveSuppressor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := GetParentForm(TControl(AOwner));
  Active := True;
end;

destructor TJvScreenSaveSuppressor.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TJvScreenSaveSuppressor.SetActive(Value: Boolean);
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

function TJvScreenSaveSuppressor.NewWndProc(var Msg: TMessage): Boolean;
begin
  Result := False;
  if (Msg.Msg = WM_SYSCOMMAND) and (Msg.WParam = SC_SCREENSAVE) and Active then
  begin
    Result := True;
    if Assigned(FOnScreenSave) then
      FOnScreenSave(Result);
    Msg.Result := Ord(Result);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
