{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxRemLog.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvxRemLog;

interface

{$IFDEF Delphi3_Up}
{$IFDEF RX_MIDAS}

uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, JvxLogin, DBClient;

{ TJvxRemoteLogin }

type
  TJvxRemoteLogin = class(TJvxCustomLogin)
  private
    FRemoteServer: TCustomRemoteServer;
    FPrepared: Boolean;
    FUsername: string;
    FInLogin: Boolean;
    FOnCheckUser: TJvxLoginEvent;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RemoteServer: TCustomRemoteServer read FRemoteServer write SetRemoteServer;
    property Active;
    property AllowEmptyPassword;
    property AttemptNumber;
    property IniFileName;
    property MaxPasswordLen;
    property UpdateCaption;
    property UseRegistry;
    property OnCheckUser: TJvxLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlock;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

{$ENDIF RX_MIDAS}
{$ENDIF Delphi3_Up}

implementation

{$IFDEF Delphi3_Up}
{$IFDEF RX_MIDAS}

uses IniFiles, Registry, JvxAppUtils, JvxVCLUtils {$IFDEF Delphi4_Up}, MConnect {$ENDIF};

const
  keyLoginSection  = 'Remote Login';
  keyLastLoginUserName = 'Last User';

type
  TJvxServer = class(TCustomRemoteServer);

{ TJvxRemoteLogin }

constructor TJvxRemoteLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvxRemoteLogin.Destroy;
begin
  UnprepareRemoteServer;
  inherited Destroy;
end;

procedure TJvxRemoteLogin.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = RemoteServer) then
    RemoteServer := nil;
end;

procedure TJvxRemoteLogin.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading and
    Assigned(FRemoteServer) then
  begin
    if not Active then PrepareRemoteServer
    else if not Login then TerminateApplication;
  end;
end;

procedure TJvxRemoteLogin.SetRemoteServer(Value: TCustomRemoteServer);
begin
  if FRemoteServer <> Value then begin
    UnprepareRemoteServer;
    FRemoteServer := Value;
    if Value <> nil then begin
      Value.FreeNotification(Self);
      if not (csLoading in ComponentState) then PrepareRemoteServer;
    end;
  end;
end;

procedure TJvxRemoteLogin.PrepareRemoteServer;
begin
  if Assigned(FRemoteServer) and not FPrepared then
    with TJvxServer(RemoteServer) do begin
{$IFDEF Delphi4_Up}
      if RemoteServer is TDispatchConnection then
        TDispatchConnection(RemoteServer).LoginPrompt := False;
{$ENDIF}
      FSaveAfterConnect := AfterConnect;
      AfterConnect := ServerAfterConnect;
      FPrepared := True;
    end;
end;

procedure TJvxRemoteLogin.UnprepareRemoteServer;
begin
  if Assigned(FRemoteServer) and FPrepared then
    with TJvxServer(RemoteServer) do begin
      AfterConnect := FSaveAfterConnect;
      FPrepared := False;
    end;
end;

procedure TJvxRemoteLogin.OkButtonClick(Sender: TObject);
var
  SetCursor: Boolean;
begin
  with TJvxLoginForm(Sender) do begin
    SetCursor := GetCurrentThreadID = MainThreadID;
    try
      if SetCursor then Screen.Cursor := crHourGlass;
      try
        if DoCheckUser(UserNameEdit.Text, PasswordEdit.Text) then
          ModalResult := mrOk
        else ModalResult := mrNone;
      finally
        if SetCursor then Screen.Cursor := crDefault;
      end;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TJvxRemoteLogin.ServerAfterConnect(Sender: TObject);
{$IFDEF Delphi4_Up}
var
  OnGetUser: TGetUsernameEvent;
{$ENDIF}
begin
  if Sender = FRemoteServer then begin
    if not FInLogin then DoBeforeLogin;
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      FUsername := ReadUserName(FUsername);
{$IFDEF Delphi4_Up}
      if FRemoteServer is TDispatchConnection then begin
        OnGetUser := TDispatchConnection(FRemoteServer).OnGetUsername;
        if Assigned(OnGetUser) then OnGetUser(FRemoteServer, FUsername);
      end;
{$ENDIF}
      UserNameEdit.Text := FUsername;
      if ShowModal = mrOk then begin
        FUsername := UserNameEdit.Text;
        WriteUserName(FUsername);
        if not FInLogin then begin
          SetLoggedUser(FUsername);
          DoUpdateCaption;
          DoAfterLogin;
        end;
      end
      else begin
        AbortConnection;
        SysUtils.Abort;
      end;
    finally
      Free;
    end;
  end;
end;

function TJvxRemoteLogin.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TJvxRemoteLogin.WriteUserName(const UserName: string);
var
  Ini: TObject;
begin
  try
    if UseRegistry then Ini := TRegIniFile.Create(IniFileName)
    else Ini := TIniFile.Create(IniFileName);
    try
      IniWriteString(Ini, keyLoginSection, keyLastLoginUserName, UserName);
    finally
      Ini.Free;
    end;
  except
  end;
end;

function TJvxRemoteLogin.ReadUserName(const UserName: string): string;
var
  Ini: TObject;
begin
  try
    if UseRegistry then begin
      Ini := TRegIniFile.Create(IniFileName);
{$IFDEF Delphi5_Up}
      TRegIniFile(Ini).Access := KEY_READ;
{$ENDIF}
    end
    else Ini := TIniFile.Create(IniFileName);
    try
      Result := IniReadString(Ini, keyLoginSection, keyLastLoginUserName,
        UserName);
    finally
      Ini.Free;
    end;
  except
    Result := UserName;
  end;
end;

procedure TJvxRemoteLogin.AbortConnection;
var
  OnAfterDisconnect, OnBeforeDisconnect: TNotifyEvent;
begin
  if Assigned(FRemoteServer) and TJvxServer(FRemoteServer).Connected then
  try
    OnAfterDisconnect := TJvxServer(FRemoteServer).AfterDisconnect;
    OnBeforeDisconnect := TJvxServer(FRemoteServer).BeforeDisconnect;
    try
      TJvxServer(FRemoteServer).AfterDisconnect := nil;
      TJvxServer(FRemoteServer).BeforeDisconnect := nil;
      TJvxServer(FRemoteServer).Connected := False;
    finally
      TJvxServer(FRemoteServer).AfterDisconnect := OnAfterDisconnect;
      TJvxServer(FRemoteServer).BeforeDisconnect := OnBeforeDisconnect;
    end;
  except
  end;
end;

function TJvxRemoteLogin.DoLogin(var UserName: string): Boolean;
begin
  Result := False;
  if not Assigned(FRemoteServer) then Exit;
  PrepareRemoteServer;
  FUsername := UserName;
  try
    FInLogin := True;
    try
      TJvxServer(FRemoteServer).Connected := True;
      Result := TJvxServer(FRemoteServer).Connected;
      UserName := FUsername;
      FUsername := '';
    finally
      FInLogin := False;
    end;
  except
    Application.HandleException(Self);
    Result := False;
    FUsername := '';
    AbortConnection;
  end;
  if Result and Assigned(FSaveAfterConnect) then
    FSaveAfterConnect(FRemoteServer);
end;

{$ENDIF RX_MIDAS}
{$ENDIF Delphi3_Up}

end.
