{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRemLog.PAS, released on 2002-07-04.

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

unit JvDBRemoteLogin;

interface

{$IFDEF JV_MIDAS}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, DBClient,
  JvLoginForm;

type
  TJvDBRemoteLogin = class(TJvCustomLogin)
  private
    FRemoteServer: TCustomRemoteServer;
    FPrepared: Boolean;
    FUserName: string;
    FInLogin: Boolean;
    FOnCheckUser: TJvLoginEvent;
    FSaveAfterConnect: TNotifyEvent;
    procedure AbortConnection;
    procedure OkButtonClick(Sender: TObject);
    procedure PrepareRemoteServer;
    procedure UnprepareRemoteServer;
    procedure ServerAfterConnect(Sender: TObject);
    procedure SetRemoteServer(Value: TCustomRemoteServer);
    procedure WriteUserName(const UserName: string);
    function ReadUserName(const UserName: string): string;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  published
    property RemoteServer: TCustomRemoteServer read FRemoteServer write SetRemoteServer;
    property Active;
    property AllowEmptyPassword;
    property AppStorage;
    property AppStoragePath;
    property AttemptNumber;
    property MaxPasswordLen;
    property UpdateCaption;
    property OnCheckUser: TJvLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlock;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

{$ENDIF JV_MIDAS}

implementation

{$IFDEF JV_MIDAS}

uses
  IniFiles,
  MConnect,
  JvJVCLUtils, JvResources;

type
  TJvServer = class(TCustomRemoteServer);

destructor TJvDBRemoteLogin.Destroy;
begin
  UnprepareRemoteServer;
  inherited Destroy;
end;

procedure TJvDBRemoteLogin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RemoteServer) then
    RemoteServer := nil;
end;

procedure TJvDBRemoteLogin.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading and
    Assigned(FRemoteServer) then
  begin
    if not Active then
      PrepareRemoteServer
    else
    if not Login then
      TerminateApplication;
  end;
end;

procedure TJvDBRemoteLogin.SetRemoteServer(Value: TCustomRemoteServer);
begin
  if FRemoteServer <> Value then
  begin
    UnprepareRemoteServer;
    FRemoteServer := Value;
    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      if not (csLoading in ComponentState) then
        PrepareRemoteServer;
    end;
  end;
end;

procedure TJvDBRemoteLogin.PrepareRemoteServer;
begin
  if Assigned(FRemoteServer) and not FPrepared then
    with TJvServer(RemoteServer) do
    begin
      if RemoteServer is TDispatchConnection then
        TDispatchConnection(RemoteServer).LoginPrompt := False;
      FSaveAfterConnect := AfterConnect;
      AfterConnect := ServerAfterConnect;
      FPrepared := True;
    end;
end;

procedure TJvDBRemoteLogin.UnprepareRemoteServer;
begin
  if Assigned(FRemoteServer) and FPrepared then
    with TJvServer(RemoteServer) do
    begin
      AfterConnect := FSaveAfterConnect;
      FPrepared := False;
    end;
end;

procedure TJvDBRemoteLogin.OkButtonClick(Sender: TObject);
var
  SetCursor: Boolean;
begin
  with TJvLoginForm(Sender) do
  begin
    SetCursor := GetCurrentThreadID = MainThreadID;
    try
      if SetCursor then
        Screen.Cursor := crHourGlass;
      try
        if DoCheckUser(UserNameEdit.Text, PasswordEdit.Text) then
          ModalResult := mrOk
        else
          ModalResult := mrNone;
      finally
        if SetCursor then
          Screen.Cursor := crDefault;
      end;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TJvDBRemoteLogin.ServerAfterConnect(Sender: TObject);
var
  OnGetUser: TGetUsernameEvent;
begin
  if Sender = FRemoteServer then
  begin
    if not FInLogin then
      DoBeforeLogin;
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      FUserName := ReadUserName(FUserName);
      if FRemoteServer is TDispatchConnection then
      begin
        OnGetUser := TDispatchConnection(FRemoteServer).OnGetUsername;
        if Assigned(OnGetUser) then
          OnGetUser(FRemoteServer, FUserName);
      end;
      UserNameEdit.Text := FUserName;
      if ShowModal = mrOk then
      begin
        FUserName := UserNameEdit.Text;
        WriteUserName(FUserName);
        if not FInLogin then
        begin
          SetLoggedUser(FUserName);
          DoUpdateCaption;
          DoAfterLogin;
        end;
      end
      else
      begin
        AbortConnection;
        SysUtils.Abort;
      end;
    finally
      Free;
    end;
  end;
end;

function TJvDBRemoteLogin.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TJvDBRemoteLogin.WriteUserName(const UserName: string);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteString (AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]), UserName);
end;

function TJvDBRemoteLogin.ReadUserName(const UserName: string): string;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadString (AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]), UserName)
  else
    Result := UserName;
end;

procedure TJvDBRemoteLogin.AbortConnection;
var
  OnAfterDisconnect, OnBeforeDisconnect: TNotifyEvent;
begin
  if Assigned(FRemoteServer) and TJvServer(FRemoteServer).Connected then
  try
    OnAfterDisconnect := TJvServer(FRemoteServer).AfterDisconnect;
    OnBeforeDisconnect := TJvServer(FRemoteServer).BeforeDisconnect;
    try
      TJvServer(FRemoteServer).AfterDisconnect := nil;
      TJvServer(FRemoteServer).BeforeDisconnect := nil;
      TJvServer(FRemoteServer).Connected := False;
    finally
      TJvServer(FRemoteServer).AfterDisconnect := OnAfterDisconnect;
      TJvServer(FRemoteServer).BeforeDisconnect := OnBeforeDisconnect;
    end;
  except
  end;
end;

function TJvDBRemoteLogin.DoLogin(var UserName: string): Boolean;
begin
  Result := False;
  if not Assigned(FRemoteServer) then
    Exit;
  PrepareRemoteServer;
  FUserName := UserName;
  try
    FInLogin := True;
    try
      TJvServer(FRemoteServer).Connected := True;
      Result := TJvServer(FRemoteServer).Connected;
      UserName := FUserName;
      FUserName := '';
    finally
      FInLogin := False;
    end;
  except
    Application.HandleException(Self);
    Result := False;
    FUserName := '';
    AbortConnection;
  end;
  if Result and Assigned(FSaveAfterConnect) then
    FSaveAfterConnect(FRemoteServer);
end;

{$ENDIF JV_MIDAS}

end.

