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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}



unit JvChPswDlg;


interface

uses
  SysUtils, {$IFDEF WIN32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons,
  DBTables, DB;

type
  TChangePasswordEvent = function(UsersTable: TTable;
    const OldPassword, NewPassword: string): Boolean of object;

  TJvChPswdForm = class(TForm)
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
    { Private declarations }
    FAttempt: Integer;
    FEnableEmpty: Boolean;
    procedure ClearEdits;
    procedure OkEnabled;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
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

implementation

uses Consts, JvDConst, JvVCLUtils;

{$R *.DFM}

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

{ TJvChPswdForm }

procedure TJvChPswdForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
end;

procedure TJvChPswdForm.FormCreate(Sender: TObject);
begin
  Caption := LoadStr(SChangePassword);
  OldPswdLabel.Caption := LoadStr(SOldPasswordLabel);
  NewPswdLabel.Caption := LoadStr(SNewPasswordLabel);
  ConfirmLabel.Caption := LoadStr(SConfirmPasswordLabel);
  OkBtn.Caption := ResStr(SOKButton);
  CancelBtn.Caption := ResStr(SCancelButton);
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
  OkBtn.Enabled := FEnableEmpty or ((OldPswd.Text <> '') and (NewPswd.Text <> '')
    and (ConfirmNewPswd.Text <> ''));
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
    if not (FAttempt > AttemptNumber) then begin
      if UsersTableName <> '' then Table := TTable.Create(Self)
      else Table := nil;
      try
        Error := peOther;
        if Table <> nil then begin
          Table.DatabaseName := Database.DatabaseName;
{$IFDEF WIN32}
          Table.SessionName := Database.SessionName;
{$ENDIF}
          Table.TableName := UsersTableName;
          Table.IndexFieldNames := UserNameField;
          Table.Open;
          if Table.FindKey([LoginName]) then begin
            if NewPswd.Text <> ConfirmNewPswd.Text then
              Error := peMismatch
            else begin
              if Assigned(OnChangePassword) then
                Ok := OnChangePassword(Table, OldPswd.Text, NewPswd.Text);
            end;
          end;
        end
        else begin
          if NewPswd.Text <> ConfirmNewPswd.Text then
            Error := peMismatch
          else begin
            if Assigned(OnChangePassword) then
              Ok := OnChangePassword(Table, OldPswd.Text, NewPswd.Text);
          end;
        end;
        if Ok then
          MessageDlg(LoadStr(SPasswordChanged), mtInformation, [mbOk], 0)
        else
          if Error = peMismatch then
            MessageDlg(LoadStr(SPasswordsMismatch), mtError, [mbOk], 0)
          else MessageDlg(LoadStr(SPasswordNotChanged), mtError, [mbOk], 0);
      finally
        if Table <> nil then Table.Free;
      end;
    end;
  finally
    if Ok then ModalResult := mrOk
    else begin
      if FAttempt > AttemptNumber then ModalResult := mrCancel
      else ModalResult := mrNone;
    end;
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

end.
