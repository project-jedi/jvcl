{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBSecur.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDESecurity;

interface

uses
  SysUtils, Classes, DBTables,
  JvLoginForm, JvBDELoginDialog, JvBDECheckPasswordForm;

type
  TCheckUserEvent = function(UsersTable: TTable;
    const Password: string): Boolean of object;


  TJvDBSecurity = class(TJvCustomLogin)
  private
    FDatabase: TDatabase;
    FUsersTableName: TFileName;
    FLoginNameField: string;
    FSelectAlias: Boolean;
    FOnCheckUser: TCheckUserEvent;
    FOnChangePassword: TChangePasswordEvent;
    FOnLoginFailure: TJvDBLoginEvent;
    procedure SetDatabase(Value: TDatabase);
    procedure SetUsersTableName(const Value: TFileName);
    function GetLoginNameField: string;
    procedure SetLoginNameField(const Value: string);
  protected
    function DoCheckUser(UsersTable: TTable; const UserName,
      Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function ChangePassword: Boolean;
  published
    property Database: TDatabase read FDatabase write SetDatabase;
    property LoginNameField: string read GetLoginNameField write SetLoginNameField;
    property SelectAlias: Boolean read FSelectAlias write FSelectAlias default False;
    property UsersTableName: TFileName read FUsersTableName write SetUsersTableName;
    property Active;
    property AllowEmptyPassword;
    property AppStorage;
    property AppStoragePath;
    property AttemptNumber;
    property MaxPasswordLen;
    property UpdateCaption;
    property OnCheckUser: TCheckUserEvent read FOnCheckUser write FOnCheckUser;
    property OnChangePassword: TChangePasswordEvent read FOnChangePassword
      write FOnChangePassword;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlock;
    property OnUnlockApp;
    property OnIconDblClick;
    property OnLoginFailure:TJvDBLoginEvent read FOnLoginFailure write FOnLoginFailure;
  end;

implementation

constructor TJvDBSecurity.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectAlias := False;
  FLoginNameField := '';
end;

procedure TJvDBSecurity.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Database) then
    Database := nil;
end;

procedure TJvDBSecurity.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) and Active and (Database <> nil) then
  begin
    Database.LoginPrompt := True;
    if not Login then
      TerminateApplication;
  end;
end;

procedure TJvDBSecurity.SetDatabase(Value: TDatabase);
begin
  if FDatabase <> Value then
  begin
    FDatabase := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TJvDBSecurity.SetUsersTableName(const Value: TFileName);
begin
  if FUsersTableName <> Value then
    FUsersTableName := Value;
end;

function TJvDBSecurity.GetLoginNameField: string;
begin
  Result := FLoginNameField;
end;

procedure TJvDBSecurity.SetLoginNameField(const Value: string);
begin
  FLoginNameField := Value;
end;

function TJvDBSecurity.DoCheckUser(UsersTable: TTable;
  const UserName, Password: string): Boolean;
var
  SaveLoggedUser: string;
begin
  if Assigned(FOnCheckUser) then
  begin
    SaveLoggedUser := LoggedUser;
    try
      SetLoggedUser(UserName);
      Result := FOnCheckUser(UsersTable, Password);
    finally
      SetLoggedUser(SaveLoggedUser);
    end;
  end
  else
    Result := True;
end;

function TJvDBSecurity.DoLogin(var UserName: string): Boolean;
var
  IconClick: TNotifyEvent;
begin
  IconClick := OnIconDblClick;
  if Assigned(IconClick) then
    IconClick := DoIconDblClick;
  Result := LoginDialog(Database, AttemptNumber, UsersTableName,
    LoginNameField, MaxPasswordLen, DoCheckUser, IconClick, UserName,
    AppStorage, AppStoragePath, SelectAlias,FOnLoginFailure );
end;

function TJvDBSecurity.ChangePassword: Boolean;
begin
  Result := ChangePasswordDialog(Database, AttemptNumber, UsersTableName,
    LoginNameField, LoggedUser, MaxPasswordLen, AllowEmptyPassword,
    FOnChangePassword);
end;

end.

