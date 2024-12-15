{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBPasswordDialogOdac.pas, released on 2006-07-21.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBPasswordDialogUniDac;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
  Classes,
  uni, dbaccess,
  {$ENDIF USE_3RDPARTY_DEVART_UNIDAC}
  JvBaseDBPasswordDialog;

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvDBOdacPasswordDialog = class(TJvBaseDBPasswordDialog)
  private
    function GetConnection: TUniConnection;
    procedure SetConnection(const Value: TUniConnection); reintroduce;
  protected
    function ChangePasswordInSession(NewPassword: string): Boolean; override;
    function GetPasswordFromSession: string; override;
  public
  published
    property Connection: TUniConnection read GetConnection write SetConnection;
  end;
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

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

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
uses
  SysUtils, Types;

function TJvDBOdacPasswordDialog.ChangePasswordInSession(NewPassword: string): Boolean;
begin
  Result := False;
  if Assigned(Connection) then
    begin
      if Connection is TOraSession then
        TOraSession(Connection).ChangePassword(NewPassword)
      else
        Connection.ExecSQL('ALTER USER ' + Connection.Username + ' IDENTIFIED BY ' + NewPassword, []);
      Connection.Password := NewPassword;
      Result := True;
    end;
end;

function TJvDBOdacPasswordDialog.GetPasswordFromSession: string;
begin
   Result := Connection.Password;
end;

function TJvDBOdacPasswordDialog.GetConnection: TUniConnection;
begin
  if (inherited Connection) is TCustomDAConnection then
    Result := TCustomDAConnection(inherited Connection)
  else
    Result := nil;
end;

procedure TJvDBOdacPasswordDialog.SetConnection(const Value: TUniConnection);
begin
  inherited SetConnection(Value);
end;
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.