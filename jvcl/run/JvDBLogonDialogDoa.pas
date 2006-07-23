{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLogonDialogDoa.pas, released on 2006-07-21

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):
Jens Fudickar

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBLogonDialogDoa;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  Classes, JvBaseDlg, JvAppStorage, JvBaseDBLogonDialog, Forms, Oracle,
  JvDynControlEngine, JvBaseDBPasswordDialog, JvDynControlEngineIntf, Controls;

type
  TJvDBDoaLogonDialogOptions = class(TJvBaseDBOracleLogonDialogOptions)
  private
  public
    constructor Create; override;
  published
    property AllowPasswordChange default true;
    property PasswordDialogOptions;
  end;

  TJvDBDoaLogonDialog = class(TJvBaseDBOracleLogonDialog)
  private
    function GetOptions: TJvDBDoaLogonDialogOptions;
    function GetSession: TOracleSession;
    procedure SetOptions(const Value: TJvDBDoaLogonDialogOptions);
    procedure SetSession(const Value: TOracleSession); reintroduce;
  protected
    procedure CreateAdditionalConnectDialogControls(aOwner: TComponent;
      aParentControl: TWinControl); override;
    procedure CreateFormControls(aForm: TForm); override;
    function CreatePasswordChangeDialog: TJvBaseDBPasswordDialog; override;
    procedure FillDatabaseComboBoxDefaultValues(Items: tStrings); override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass;
      override;
    procedure HandleExpiredPassword(const ErrorMessage: string);
    procedure ResizeFormControls; override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo:
      TJvBaseConnectionInfo); override;
    procedure TransferConnectionInfoToDialog(ConnectionInfo:
      TJvBaseConnectionInfo); override;
    procedure TransferSessionDataFromConnectionInfo(ConnectionInfo:
      TJvBaseConnectionInfo); override;
    procedure TransferSessionDataToConnectionInfo(ConnectionInfo:
      TJvBaseConnectionInfo); override;
  public
    procedure ClearControlInterfaceObjects; override;
    procedure ConnectSession; override;
    function SessionIsConnected: Boolean; override;
  published
    property Options: TJvDBDoaLogonDialogOptions read GetOptions write SetOptions;
    property Session: TOracleSession read GetSession write SetSession;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL:$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses Sysutils, OraClasses, OraError, JvdsADialogs, Dialogs,
  JvDBPasswordDialogDoa, stdCtrls,JvResources;


constructor TJvDBDoaLogonDialogOptions.Create;
begin
  inherited Create;
  AllowPasswordChange := True;
end;

procedure TJvDBDoaLogonDialog.ClearControlInterfaceObjects;
begin
  inherited ClearControlInterfaceObjects;
end;

procedure TJvDBDoaLogonDialog.ConnectSession;
begin
  if Assigned(Session) then
  try
    Session.LogOn;
  except
    on e: eOraError do
    begin
      case e.ErrorCode of
        1005, 1017: ActivatePasswordControl;
        12203, 12154: ActivateDatabaseControl;
      end;
      if (e.ErrorCode = 28001) or
        (e.ErrorCode = 28002) or
        (e.ErrorCode = 28011) then
        HandleExpiredPassword(e.Message)
      else
        JVDsaDialogs.MessageDlg(e.Message, mtError, [mbok], 0, dckScreen,
          0, mbDefault, mbDefault, mbDefault, DynControlEngine);
    end;
  end;
end;

procedure TJvDBDoaLogonDialog.CreateAdditionalConnectDialogControls(aOwner:
  TComponent; aParentControl: TWinControl);
begin
  inherited CreateAdditionalConnectDialogControls(aOwner, aParentControl);
end;

procedure TJvDBDoaLogonDialog.CreateFormControls(aForm: TForm);
begin
  inherited CreateFormControls(aForm);
end;

function TJvDBDoaLogonDialog.CreatePasswordChangeDialog:
  TJvBaseDBPasswordDialog;
begin
  Result := TJvDBDoaPasswordDialog.Create(self);
end;

procedure TJvDBDoaLogonDialog.FillDatabaseComboBoxDefaultValues(Items:
  tStrings);
begin

end;

class function TJvDBDoaLogonDialog.GetDBLogonDialogOptionsClass:
  TJvBaseDBLogonDialogOptionsClass;
begin
  Result := TJvDBDoaLogonDialogOptions;
end;

function TJvDBDoaLogonDialog.GetOptions: TJvDBDoaLogonDialogOptions;
begin
  Result := TJvDBDoaLogonDialogOptions(inherited Options);
end;

function TJvDBDoaLogonDialog.GetSession: TOracleSession;
begin
  Result := TOracleSession(inherited Session);
end;

procedure TJvDBDoaLogonDialog.HandleExpiredPassword(const ErrorMessage:
  string);
begin
  if JVDsaDialogs.MessageDlg(ErrorMessage + #13#10 + RsDoYouWantToChangePassword, mtInformation, [mbYes, mbNo], 0,
    dckScreen,
    0, mbDefault, mbDefault, mbDefault, DynControlEngine) = mrYes then
    if ChangePassword then
      if not SessionIsConnected then
        Session.LogOn;
end;

procedure TJvDBDoaLogonDialog.ResizeFormControls;
begin
  inherited ResizeFormControls;
end;

function TJvDBDoaLogonDialog.SessionIsConnected: Boolean;
begin
  Result := Session.Connected;
end;

procedure TJvDBDoaLogonDialog.SetOptions(const Value:
  TJvDBDoaLogonDialogOptions);
begin
  (inherited Options).Assign(Value);
end;

procedure TJvDBDoaLogonDialog.SetSession(const Value: TOracleSession);
begin
  inherited SetSession(Value);
end;

procedure TJvDBDoaLogonDialog.TransferConnectionInfoFromDialog(ConnectionInfo:
  TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoFromDialog(ConnectionInfo);
end;

procedure TJvDBDoaLogonDialog.TransferConnectionInfoToDialog(ConnectionInfo:
  TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoToDialog(ConnectionInfo);
end;

procedure TJvDBDoaLogonDialog.TransferSessionDataFromConnectionInfo(
  ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(Session) then
  begin
    Session.LogonDatabase := ConnectionInfo.Database;
    Session.LogonPassword := ConnectionInfo.Password;
    Session.LogonUsername := ConnectionInfo.Username;
    if tJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSDBA' then
      Session.ConnectAs := caSYSDBA
    else
      if tJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSOPER' then
        Session.ConnectAs := caSYSOper
      else
        Session.ConnectAs := caNormal;
  end;
end;

procedure TJvDBDoaLogonDialog.TransferSessionDataToConnectionInfo(
  ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(Session) then
  begin
    ConnectionInfo.Database := Session.LogonDatabase;
    ConnectionInfo.Password := Session.LogonPassword;
    ConnectionInfo.Username := Session.LogonUsername;
    case Session.Connectas of
      caSYSDBA: tJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSDBA';
      caSYSOPER: tJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSOPER';
    else
      tJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'NORMAL';
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

