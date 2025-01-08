{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBPasswordDialogDoa.pas, released on 2006-07-21.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBPasswordDialogDoa;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Menus,
  Oracle,
  JvBaseDBPasswordDialog;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBDoaPasswordDialog = class(TJvBaseDBPasswordDialog)
  private
    function GetSession: TOracleSession;
    procedure SetSession(const Value: TOracleSession); reintroduce;
  protected
    function ChangePasswordInSession(NewPassword: string): Boolean; override;
    function GetPasswordFromSession: string; override;
  public
  published
    property Session: TOracleSession read GetSession write SetSession;
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
  SysUtils, ExtCtrls, ComCtrls, StdCtrls, Types;

function TJvDBDoaPasswordDialog.ChangePasswordInSession(NewPassword: string): Boolean;
begin
  Result := False;
  if Assigned(Session) then
    begin
      TOracleSession(Session).SetPassword(NewPassword);
      Session.LogonPassword := NewPassword;
      Result := True;
    end;
end;

function TJvDBDoaPasswordDialog.GetPasswordFromSession: string;
begin
   Result := Session.LogonPassword;
end;

function TJvDBDoaPasswordDialog.GetSession: TOracleSession;
begin
  if (inherited Session) is TOracleSession then
    Result := TOracleSession(inherited Session)
  else
    Result := nil;
end;

procedure TJvDBDoaPasswordDialog.SetSession(const Value: TOracleSession);
begin
  inherited SetSession(Value);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.