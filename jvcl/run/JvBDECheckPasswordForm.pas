{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvChPswDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDECheckPasswordForm;

{$I jvcl.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Buttons,
  DBTables, DB,
  JvComponent;

type
  TChangePasswordEvent = function(UsersTable: TTable;
    const OldPassword, NewPassword: string): Boolean of object;

function ChangePasswordDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField, LoginName: string;
  MaxPwdLen: Integer; EnableEmptyPassword: Boolean;
  ChangePasswordEvent: TChangePasswordEvent): Boolean;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Consts,
  JvResources, JvJVCLUtils;

{$R *.dfm}

type
  // (rom) moved to implementation for security reasons
  TJvChPswdForm = class(TJvForm)
    OldPswdLabel: TLabel;
    OldPswd: TEdit;
    NewPswdLabel: TLabel;
    NewPswd: TEdit;
    ConfirmLabel: TLabel;
    ConfirmNewPswd: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PswdChange(Sender: TObject);
  private
    FAttempt: Integer;
    FEnableEmpty: Boolean;
    procedure ClearEdits;
    procedure OkEnabled;
  public
    Database: TDatabase;
    AttemptNumber: Integer;
    UsersTableName: string;
    UserNameField: string;
    LoginName: string;
    OnChangePassword: TChangePasswordEvent;
  end;

function ChangePasswordDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField, LoginName: string;
  MaxPwdLen: Integer; EnableEmptyPassword: Boolean;
  ChangePasswordEvent: TChangePasswordEvent): Boolean;
var
  Form: TJvChPswdForm;
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  try
    Form := TJvChPswdForm.Create(Application);
    try
      Form.Database := Database;
      Form.AttemptNumber := AttemptNumber;
      Form.UsersTableName := UsersTableName;
      Form.UserNameField := UserNameField;
      Form.LoginName := LoginName;
      Form.OldPswd.MaxLength := MaxPwdLen;
      Form.NewPswd.MaxLength := MaxPwdLen;
      Form.ConfirmNewPswd.MaxLength := MaxPwdLen;
      Form.FEnableEmpty := EnableEmptyPassword;
      Form.OnChangePassword := ChangePasswordEvent;
      Result := (Form.ShowModal = mrOk);
    finally
      Form.Free;
    end;
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

procedure TJvChPswdForm.FormCreate(Sender: TObject);
begin
  Caption := RsChangePassword;
  OldPswdLabel.Caption := RsOldPasswordLabel;
  NewPswdLabel.Caption := RsNewPasswordLabel;
  ConfirmLabel.Caption := RsConfirmPasswordLabel;
  OkBtn.Caption := SOKButton;
  CancelBtn.Caption := SCancelButton;
end;

procedure TJvChPswdForm.ClearEdits;
begin
  OldPswd.Text := '';
  NewPswd.Text := '';
  ConfirmNewPswd.Text := '';
  OkBtn.Enabled := FEnableEmpty;
end;

procedure TJvChPswdForm.OkEnabled;
begin
  OkBtn.Enabled := FEnableEmpty or
    ((OldPswd.Text <> '') and (NewPswd.Text <> '') and (ConfirmNewPswd.Text <> ''));
end;

procedure TJvChPswdForm.OkBtnClick(Sender: TObject);
type
  TChangePasswordError = (peMismatch, peOther);
var
  Table: TTable;
  Ok: Boolean;
  Error: TChangePasswordError;
begin
  Ok := False;
  Inc(FAttempt);
  try
    if FAttempt <= AttemptNumber then
    begin
      if UsersTableName <> '' then
        Table := TTable.Create(Self)
      else
        Table := nil;
      try
        Error := peOther;
        if Table <> nil then
        begin
          Table.DatabaseName := Database.DatabaseName;
          Table.SessionName := Database.SessionName;
          Table.TableName := UsersTableName;
          Table.IndexFieldNames := UserNameField;
          Table.Open;
          if Table.FindKey([LoginName]) then
            if NewPswd.Text <> ConfirmNewPswd.Text then
              Error := peMismatch
            else
            if Assigned(OnChangePassword) then
              Ok := OnChangePassword(Table, OldPswd.Text, NewPswd.Text);
        end
        else
        begin
          if NewPswd.Text <> ConfirmNewPswd.Text then
            Error := peMismatch
          else
          if Assigned(OnChangePassword) then
            Ok := OnChangePassword(Table, OldPswd.Text, NewPswd.Text);
        end;
        if Ok then
          MessageBox(RsPasswordChanged, '', MB_OK or MB_ICONINFORMATION)
        else
        if Error = peMismatch then
          MessageBox(RsPasswordsMismatch, '', MB_OK or MB_ICONERROR)
        else
          MessageBox(RsPasswordNotChanged, '', MB_OK or MB_ICONERROR);
      finally
        if Table <> nil then
          Table.Free;
      end;
    end;
  finally
    if Ok then
      ModalResult := mrOk
    else
    if FAttempt > AttemptNumber then
      ModalResult := mrCancel
    else
      ModalResult := mrNone;
  end;
end;

procedure TJvChPswdForm.FormShow(Sender: TObject);
begin
  ClearEdits;
end;

procedure TJvChPswdForm.PswdChange(Sender: TObject);
begin
  OkEnabled;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

