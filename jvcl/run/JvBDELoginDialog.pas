{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxLoginDlg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Polaris Software

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDELoginDialog;

interface

uses
  Windows, Classes, DBTables,
  JvLoginForm, JvAppStorage;

type
  TCheckUserNameEvent = function(UsersTable: TTable;
    const UserName, Password: string): Boolean of object;

  TDialogMode = (dmAppLogin, dmDBLogin, dmUnlock);

  TJvDBLoginEvent = procedure(Sender: TObject; const UserName, Password: string) of object;

  TJvDBLoginDialog = class(TObject)
  private
    FDialog: TJvLoginForm;
    FMode: TDialogMode;
    FSelectDatabase: Boolean;
    FIniAliasName: string;
    FCheckUserEvent: TCheckUserNameEvent;
    FCheckUnlock: TCheckUnlockEvent;
    FIconDblClick: TNotifyEvent;
    FDatabase: TDatabase;
    FAttemptNumber: Integer;
    FShowDBName: Boolean;
    FUsersTableName: string;
    FUserNameField: string;
    FMaxPwdLen: Integer;
    FLoginName: string;
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FOnLoginFailure: TJvDBLoginEvent;
    procedure Login(Database: TDatabase; LoginParams: TStrings);
    function GetUserInfo: Boolean;
    function CheckUser(Table: TTable): Boolean;
    function CheckUnlock: Boolean;
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function ExecuteAppLogin: Boolean;
    function ExecuteDbLogin(LoginParams: TStrings): Boolean;
    function ExecuteUnlock: Boolean;
  public
    constructor Create(DialogMode: TDialogMode; DatabaseSelect: Boolean);
    destructor Destroy; override;
    function Execute(LoginParams: TStrings): Boolean;
    function GetUserName: string;
    function CheckDatabaseChange: Boolean;
    procedure FillParams(LoginParams: TStrings);
    property Mode: TDialogMode read FMode;
    property SelectDatabase: Boolean read FSelectDatabase;
    property OnCheckUnlock: TCheckUnlockEvent read FCheckUnlock write FCheckUnlock;
    property OnCheckUserEvent: TCheckUserNameEvent read FCheckUserEvent write FCheckUserEvent;
    property OnIconDblClick: TNotifyEvent read FIconDblClick write FIconDblClick;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
    property Database: TDatabase read FDatabase write FDatabase;
    property AttemptNumber: Integer read FAttemptNumber write FAttemptNumber;
    property ShowDBName: Boolean read FShowDBName write FShowDBName;
    property UsersTableName: string read FUsersTableName write FUsersTableName;
    property UserNameField: string read FUserNameField write FUserNameField;
    property MaxPwdLen: Integer read FMaxPwdLen write FMaxPwdLen;
    property LoginName: string read FLoginName write FLoginName;
  published
    property OnLoginFailure: TJvDBLoginEvent read FOnLoginFailure write FOnLoginFailure;
  end;

procedure OnLoginDialog(Database: TDatabase; LoginParams: TStrings;
  AttemptNumber: Integer; ShowDBName: Boolean);

function LoginDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField: string; MaxPwdLen: Integer;
  CheckUserEvent: TCheckUserNameEvent; IconDblClick: TNotifyEvent;
  var LoginName: string; AppStorage: TJvCustomAppStorage;
  AppStoragePath: string; SelectDatabase: Boolean;
  LoginFailure: TJvDBLoginEvent): Boolean;

function UnlockDialog(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent): Boolean;
function UnlockDialogEx(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent; MaxPwdLen, AttemptNumber: Integer): Boolean;

implementation

uses
  SysUtils, Graphics, Controls, Forms, DB, BDE,
  JvBDELists, 
  JvConsts, JvResources;

constructor TJvDBLoginDialog.Create(DialogMode: TDialogMode; DatabaseSelect: Boolean);
begin
  inherited Create;
  FMode := DialogMode;
  FSelectDatabase := DatabaseSelect;
  FDialog := CreateLoginDialog((FMode = dmUnlock), FSelectDatabase,
    FormShow, OkBtnClick);
  AttemptNumber := 3;
  ShowDBName := True;
end;

destructor TJvDBLoginDialog.Destroy;
begin
  FDialog.Free;
  inherited Destroy;
end;

procedure TJvDBLoginDialog.OkBtnClick(Sender: TObject);
var
  Ok: Boolean;
  SaveLogin: TDatabaseLoginEvent;
  SetCursor: Boolean;
begin
  if FMode = dmUnlock then
  begin
    Ok := False;
    try
      Ok := CheckUnlock;
    except
      Application.HandleException(Self);
    end;
    if Ok then
      FDialog.ModalResult := mrOk
    else
      FDialog.ModalResult := mrCancel;
  end
  else
  if Mode = dmAppLogin then
  begin
    SetCursor := GetCurrentThreadID = MainThreadID;
    SaveLogin := Database.OnLogin;
    try
      try
        if Database.Connected then
          Database.Close; //Polaris
        if FSelectDatabase then
          Database.AliasName := FDialog.CustomCombo.Text;
        Database.OnLogin := Login;
        if SetCursor then
          Screen.Cursor := crHourGlass;
        try
          Database.Open;
        finally
          if SetCursor then
            Screen.Cursor := crDefault;
        end;
      except
        Application.HandleException(Self);
      end;
    finally
      Database.OnLogin := SaveLogin;
    end;
    if Database.Connected then
    try
      if SetCursor then
        Screen.Cursor := crHourGlass;
      Ok := False;
      try
        Ok := GetUserInfo;
      except
        Application.HandleException(Self);
      end;
      if Ok then
        FDialog.ModalResult := mrOk
      else
      begin
        FDialog.ModalResult := mrNone;
        Database.Close;
      end;
    finally
      if SetCursor then
        Screen.Cursor := crDefault;
    end;
  end
  else { dmDBLogin }
    FDialog.ModalResult := mrOk
end;

procedure TJvDBLoginDialog.FormShow(Sender: TObject);
var
  S: string;
begin
  if (FMode in [dmAppLogin, dmDBLogin]) and FSelectDatabase then
  begin
    with TJvBDEItems.Create(FDialog) do
    try
      SessionName := Database.SessionName;
      ItemType := bdDatabases;
      FDialog.CustomCombo.Items.Clear;
      Open;
      while not Eof do
      begin
        FDialog.CustomCombo.Items.Add(FieldByName('NAME').AsString);
        Next;
      end;
      if FIniAliasName = '' then
        S := Database.AliasName
      else
        S := FIniAliasName;
      with FDialog.CustomCombo do
        ItemIndex := Items.IndexOf(S);
    finally
      Free;
    end;
  end;
end;

function TJvDBLoginDialog.ExecuteAppLogin: Boolean;
begin
  if Assigned(AppStorage) then
  begin
    FDialog.UserNameEdit.Text := AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]),
      LoginName);
    FSelectDatabase := AppStorage.ReadBoolean(AppStorage.ConcatPaths([AppStoragePath, RsSelectDatabase]),
      FSelectDatabase);
    FIniAliasName := AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath, RsLastAliasName]), '');
  end;
  FDialog.SelectDatabase := SelectDatabase;
  Result := (FDialog.ShowModal = mrOk);
  Database.OnLogin := nil;
  if Result then
  begin
    LoginName := GetUserName;
    if Assigned(AppStorage) then
    begin
      AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]), GetUserName);
      AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, RsLastAliasName]), Database.AliasName);
    end;
  end;
end;

function TJvDBLoginDialog.ExecuteDbLogin(LoginParams: TStrings): Boolean;
var
  CurrSession: TSession;
begin
  Result := False;
  if (Database = nil) or not Assigned(LoginParams) then
    Exit;
  if ShowDBName then
    FDialog.AppTitleLabel.Caption := Format(RsDatabaseName, [Database.DatabaseName]);
  FDialog.UserNameEdit.Text := LoginParams.Values[szUSERNAME];
  CurrSession := Sessions.CurrentSession;
  try
    Result := FDialog.ShowModal = mrOk;
    if Result then
      FillParams(LoginParams)
    else
      SysUtils.Abort;
  finally
    Sessions.CurrentSession := CurrSession;
  end;
end;

function TJvDBLoginDialog.ExecuteUnlock: Boolean;
begin
  with FDialog.UserNameEdit do
  begin
    Text := LoginName;
    ReadOnly := True;
    Font.Color := clGrayText;
  end;
  Result := (FDialog.ShowModal = mrOk);
end;

function TJvDBLoginDialog.Execute(LoginParams: TStrings): Boolean;
var
  SaveCursor: TCursor;
begin
  SaveCursor := Screen.Cursor;
  Screen.Cursor := crDefault;
  try
    if Assigned(FIconDblClick) then
    begin
      with FDialog.AppIcon do
      begin
        OnDblClick := OnIconDblClick;
        Cursor := crHand;
      end;
      with FDialog.KeyImage do
      begin
        OnDblClick := OnIconDblClick;
        Cursor := crHand;
      end;
    end;
    FDialog.PasswordEdit.MaxLength := MaxPwdLen;
    FDialog.AttemptNumber := AttemptNumber;
    case FMode of
      dmAppLogin:
        Result := ExecuteAppLogin;
      dmDBLogin:
        Result := ExecuteDbLogin(LoginParams);
      dmUnlock:
        Result := ExecuteUnlock;
    else
      Result := False;
    end;
    if Result then
      LoginName := GetUserName;
  finally
    Screen.Cursor := SaveCursor;
  end;
end;

function TJvDBLoginDialog.GetUserName: string;
begin
  if CheckDatabaseChange then
    Result := Copy(FDialog.UserNameEdit.Text, 1,
      Pos('@', FDialog.UserNameEdit.Text) - 1)
  else
    Result := FDialog.UserNameEdit.Text;
end;

function TJvDBLoginDialog.CheckDatabaseChange: Boolean;
begin
  Result := (FMode in [dmAppLogin, dmDBLogin]) and
    (Pos('@', FDialog.UserNameEdit.Text) > 0) and
    ((Database <> nil) and (Database.DriverName <> '') and
    (CompareText(Database.DriverName, szCFGDBSTANDARD) <> 0));
end;

procedure TJvDBLoginDialog.FillParams(LoginParams: TStrings);
begin
  LoginParams.BeginUpdate;
  try
    LoginParams.Values[szUSERNAME] := GetUserName;
    LoginParams.Values[szPASSWORD] := FDialog.PasswordEdit.Text;
    if CheckDatabaseChange then
    begin
      LoginParams.Values[szSERVERNAME] := Copy(FDialog.UserNameEdit.Text,
        Pos('@', FDialog.UserNameEdit.Text) + 1, MaxInt)
    end;
  finally
    LoginParams.EndUpdate;
  end;
end;

procedure TJvDBLoginDialog.Login(Database: TDatabase; LoginParams: TStrings);
begin
  FillParams(LoginParams);
end;

function TJvDBLoginDialog.GetUserInfo: Boolean;
var
  Table: TTable;
begin
  if UsersTableName = '' then
    Result := CheckUser(nil)
  else
  begin
    Result := False;
    // Table := TTable.Create(Database);
    Table := TTable.Create(Application); // Polaris (?)
    try
      try
        Table.DatabaseName := Database.DatabaseName;
        Table.SessionName := Database.SessionName;
        Table.TableName := UsersTableName;
        Table.IndexFieldNames := UserNameField;
        Table.Open;
        if Table.FindKey([GetUserName]) then
        begin
          Result := CheckUser(Table);

     if not Result then
          begin
            if Assigned(FOnLoginFailure) then
              FOnLoginFailure(Self, GetUserName, FDialog.PasswordEdit.Text)
            else
              raise EDatabaseError.CreateRes(@RsEInvalidUserName);
          end;
        end
        else
        begin
          if Assigned(FOnLoginFailure) then
            FOnLoginFailure(Self, GetUserName, FDialog.PasswordEdit.Text)
          else
            raise EDatabaseError.CreateRes(@RsEInvalidUserName);
        end;
      except
        Application.HandleException(Self);
      end;
    finally
      Table.Free;
    end;
  end;
end;


function TJvDBLoginDialog.CheckUser(Table: TTable): Boolean;
begin
  if Assigned(FCheckUserEvent) then
    Result := FCheckUserEvent(Table, GetUserName, FDialog.PasswordEdit.Text)
  else
    Result := True;
end;

function TJvDBLoginDialog.CheckUnlock: Boolean;
begin
  if Assigned(FCheckUnlock) then
    Result := FCheckUnlock(FDialog.PasswordEdit.Text)
  else
    Result := True;
end;

//=== Utility routines =======================================================

procedure OnLoginDialog(Database: TDatabase; LoginParams: TStrings;
  AttemptNumber: Integer; ShowDBName: Boolean);
var
  Dlg: TJvDBLoginDialog;
begin
  Dlg := TJvDBLoginDialog.Create(dmDBLogin, False);
  try
    Dlg.Database := Database;
    Dlg.ShowDBName := ShowDBName;
    Dlg.AttemptNumber := AttemptNumber;
    Dlg.Execute(LoginParams);
  finally
    Dlg.Free;
  end;
end;

function UnlockDialogEx(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent; MaxPwdLen, AttemptNumber: Integer): Boolean;
var
  Dlg: TJvDBLoginDialog;
begin
  Dlg := TJvDBLoginDialog.Create(dmUnlock, False);
  try
    Dlg.LoginName := UserName;
    Dlg.OnIconDblClick := IconDblClick;
    Dlg.OnCheckUnlock := OnUnlock;
    Dlg.MaxPwdLen := MaxPwdLen;
    Dlg.AttemptNumber := AttemptNumber;
    Result := Dlg.Execute(nil);
  finally
    Dlg.Free;
  end;
end;

function UnlockDialog(const UserName: string; OnUnlock: TCheckUnlockEvent;
  IconDblClick: TNotifyEvent): Boolean;
begin
  Result := UnlockDialogEx(UserName, OnUnlock, IconDblClick, 0, 1);
end;

function LoginDialog(Database: TDatabase; AttemptNumber: Integer;
  const UsersTableName, UserNameField: string; MaxPwdLen: Integer;
  CheckUserEvent: TCheckUserNameEvent; IconDblClick: TNotifyEvent;
  var LoginName: string; AppStorage: TJvCustomAppStorage;
  AppStoragePath: string; SelectDatabase: Boolean;
  LoginFailure: TJvDBLoginEvent): Boolean;
var
  Dlg: TJvDBLoginDialog;
begin
  Dlg := TJvDBLoginDialog.Create(dmAppLogin, SelectDatabase);
  try
    Dlg.LoginName := LoginName;
    Dlg.OnIconDblClick := IconDblClick;
    Dlg.OnCheckUserEvent := CheckUserEvent;
    Dlg.OnLoginFailure := LoginFailure;
    Dlg.MaxPwdLen := MaxPwdLen;
    Dlg.Database := Database;
    Dlg.AttemptNumber := AttemptNumber;
    Dlg.UsersTableName := UsersTableName;
    Dlg.UserNameField := UserNameField;
    Dlg.AppStorage := AppStorage;
    Dlg.AppStoragePath := AppStoragePath;
    Result := Dlg.Execute(nil);
    if Result then
      LoginName := Dlg.LoginName;
  finally
    Dlg.Free;
  end;
end;

end.

