{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxLogin.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Contributor(s):
  Hofi

Last Modified: 2004-10-07

Changes:
2004-10-07:
  * Added
     TJvCustomLogin
       property Caption to support a custom dialog Caption.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvLoginForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF MSWINDOWS}
  Windows, // GetCurrentThreadID => Linux: System.pas
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc, // GetCurrentThreadID Linux
  {$ENDIF HAS_UNIT_LIBC}
  Messages, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
  JvComponent, JvBaseDlg, JvAppStorage;

type
  TUpdateCaption = (ucNoChange, ucAppTitle, ucFormCaption);
  TJvLoginEvent = procedure(Sender: TObject; const UserName, Password: string;
    var AllowLogin: Boolean) of object;
  TCheckUnlockEvent = function(const Password: string): Boolean of object;
  TUnlockAppEvent = procedure(Sender: TObject; const UserName,
    Password: string; var AllowUnlock: Boolean) of object;

  TJvLoginForm = class;

  TJvCustomLogin = class(TJvCommonDialogF)
  private
    FActive: Boolean;
    FAttemptNumber: Integer;
    FCaption: string;
    FLoggedUser: string;
    FMaxPasswordLen: Integer;
    FAllowEmptyPassword: Boolean;
    FUpdateCaption: TUpdateCaption;
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    FLocked: Boolean;
    FSaveOnRestore: TNotifyEvent;
    FAfterLogin: TNotifyEvent;
    FBeforeLogin: TNotifyEvent;
    FOnUnlock: TCheckUnlockEvent;
    FOnUnlockApp: TUnlockAppEvent;
    FOnIconDblClick: TNotifyEvent;
    {$IFDEF VCL}
    FUnlockDlgShowing: Boolean;
    FPasswordChar: Char;
    function UnlockHook(var Msg: TMessage): Boolean;
    {$ENDIF VCL}
    function GetLoggedUser: string;
    procedure SetAppStorage(Value: TJvCustomAppStorage);
  protected
    function CheckUnlock(const UserName, Password: string): Boolean; dynamic;
    function CreateLoginForm(UnlockMode: Boolean): TJvLoginForm; virtual;
    procedure DoAfterLogin; dynamic;
    procedure DoBeforeLogin; dynamic;
    procedure DoIconDblClick(Sender: TObject); dynamic;
    function DoLogin(var UserName: string): Boolean; virtual; abstract;
    function DoUnlockDialog: Boolean; virtual;
    procedure SetLoggedUser(const Value: string);
    procedure DoUpdateCaption;
    procedure UnlockOkClick(Sender: TObject);
    property Active: Boolean read FActive write FActive default True;
    property AllowEmptyPassword: Boolean read FAllowEmptyPassword write FAllowEmptyPassword default True;
    property AttemptNumber: Integer read FAttemptNumber write FAttemptNumber default 3;
    property MaxPasswordLen: Integer read FMaxPasswordLen write FMaxPasswordLen default 0;
    property UpdateCaption: TUpdateCaption read FUpdateCaption write FUpdateCaption default ucNoChange;
    {$IFDEF VCL}
    property PasswordChar: Char read FPasswordChar write FPasswordChar default '*';
    {$ENDIF VCL}
    property AfterLogin: TNotifyEvent read FAfterLogin write FAfterLogin;
    property BeforeLogin: TNotifyEvent read FBeforeLogin write FBeforeLogin;
    property OnUnlock: TCheckUnlockEvent read FOnUnlock write FOnUnlock; { obsolete }
    property OnUnlockApp: TUnlockAppEvent read FOnUnlockApp write FOnUnlockApp;
    property OnIconDblClick: TNotifyEvent read FOnIconDblClick write FOnIconDblClick;
    property AppStorage: TJvCustomAppStorage read FAppStorage write SetAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Login: Boolean; virtual;
    function Execute: Boolean; override;
    procedure TerminateApplication;
    procedure Lock;
    property LoggedUser: string read GetLoggedUser;
  published
    property Caption: string read FCaption write FCaption;
  end;

  TJvLoginDialog = class(TJvCustomLogin)
  private
    FOnCheckUser: TJvLoginEvent;
    procedure OkButtonClick(Sender: TObject);
    procedure WriteUserName(const UserName: string);
    function ReadUserName(const UserName: string): string;
  protected
    function DoCheckUser(const UserName, Password: string): Boolean; dynamic;
    function DoLogin(var UserName: string): Boolean; override;
    procedure Loaded; override;
  published
    property Active;
    property AppStorage;
    property AppStoragePath;
    property AttemptNumber;
    property Caption;
    property MaxPasswordLen;
    property UpdateCaption;
    {$IFDEF VCL}
    property PasswordChar;
    {$ENDIF VCL}
    property OnCheckUser: TJvLoginEvent read FOnCheckUser write FOnCheckUser;
    property AfterLogin;
    property BeforeLogin;
    property OnUnlockApp;
    property OnIconDblClick;
  end;

  TJvLoginForm = class(TJvForm)
    AppIcon: TImage;
    KeyImage: TImage;
    HintLabel: TLabel;
    UserNameLabel: TLabel;
    PasswordLabel: TLabel;
    UserNameEdit: TEdit;
    PasswordEdit: TEdit;
    AppTitleLabel: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    CustomLabel: TLabel;
    CustomCombo: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FSelectDatabase: Boolean;
    FUnlockMode: Boolean;
    FAttempt: Integer;
    FOnFormShow: TNotifyEvent;
    FOnOkClick: TNotifyEvent;
  public
    AttemptNumber: Integer;
    property Attempt: Integer read FAttempt;
    property SelectDatabase: Boolean read FSelectDatabase write FSelectDatabase;
    property OnFormShow: TNotifyEvent read FOnFormShow write FOnFormShow;
    property OnOkClick: TNotifyEvent read FOnOkClick write FOnOkClick;
  end;

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
 FormShowEvent, OkClickEvent: TNotifyEvent;
 ACaption: string = ''): TJvLoginForm;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Consts,
  IniFiles,
  JvJCLUtils, JvJVCLUtils, JvResources, JvConsts;

{$R *.dfm}

function CreateLoginDialog(UnlockMode, ASelectDatabase: Boolean;
 FormShowEvent, OkClickEvent: TNotifyEvent; ACaption: string = ''): TJvLoginForm;
begin
  Result := TJvLoginForm.Create(Application);
  with Result do
  begin
    Caption := ACaption;
    FSelectDatabase := ASelectDatabase;
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
      FormStyle := fsStayOnTop;
    OnFormShow := FormShowEvent;
    OnOkClick := OkClickEvent;
  end;
end;

//=== { TJvCustomLogin } =====================================================

constructor TJvCustomLogin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoggedUser := '';
  FActive := True;
  FAttemptNumber := 3;
  {$IFDEF VCL}
  FPasswordChar := '*';
  {$ENDIF VCL}
  FAllowEmptyPassword := True;
end;

destructor TJvCustomLogin.Destroy;
begin
  if FLocked then
  begin
    {$IFDEF VCL}
    Application.UnhookMainWindow(UnlockHook);
    {$ENDIF VCL}
    FLocked := False;
  end;
  inherited Destroy;
end;

procedure TJvCustomLogin.SetAppStorage(Value: TJvCustomAppStorage);
begin
  FAppStorage := Value;
end;

function TJvCustomLogin.GetLoggedUser: string;
begin
  Result := FLoggedUser;
end;

procedure TJvCustomLogin.SetLoggedUser(const Value: string);
begin
  FLoggedUser := Value;
end;

procedure TJvCustomLogin.DoAfterLogin;
begin
  if Assigned(FAfterLogin) then
    FAfterLogin(Self);
end;

procedure TJvCustomLogin.DoBeforeLogin;
begin
  if Assigned(FBeforeLogin) then
    FBeforeLogin(Self);
end;

procedure TJvCustomLogin.DoIconDblClick(Sender: TObject);
begin
  if Assigned(FOnIconDblClick) then
    FOnIconDblClick(Self);
end;

procedure TJvCustomLogin.DoUpdateCaption;
var
  F: TCustomForm;
begin
  F := Application.MainForm;
  if (F = nil) and (Owner is TForm) then
    F := Owner as TForm;
  if (F <> nil) and (LoggedUser <> '') then
    case UpdateCaption of
      ucAppTitle:
        F.Caption := Format('%s (%s)', [Application.Title, LoggedUser]);
      ucFormCaption:
        begin
          F.Caption := Format('%s (%s)', [F.Caption, LoggedUser]);
          UpdateCaption := ucNoChange;
        end;
    end;
end;

function TJvCustomLogin.Login: Boolean;
var
  LoginName: string;
begin
  LoginName := '';
  DoBeforeLogin;
  Result := DoLogin(LoginName);
  if Result then
  begin
    SetLoggedUser(LoginName);
    DoUpdateCaption;
    DoAfterLogin;
  end;
end;

procedure TJvCustomLogin.Lock;
begin
  FSaveOnRestore := Application.OnRestore;
  Application.Minimize;
  {$IFDEF VCL}
  Application.HookMainWindow(UnlockHook);
  {$ENDIF VCL}
  FLocked := True;
end;

procedure TJvCustomLogin.TerminateApplication;
begin
  with Application do
  begin
    ShowMainForm := False;
    {$IFDEF VCL}
    if Handle <> 0 then
      ShowOwnedPopups(Handle, False);
    {$ENDIF VCL}
    Terminate;
  end;
  CallTerminateProcs;
end;

procedure TJvCustomLogin.UnlockOkClick(Sender: TObject);
var
  Ok: Boolean;
begin
  with TJvLoginForm(Sender) do
  begin
    Ok := False;
    try
      Ok := CheckUnlock(UserNameEdit.Text, PasswordEdit.Text);
    except
      Application.HandleException(Self);
    end;
    if Ok then
      ModalResult := mrOk
    else
      ModalResult := mrCancel;
  end;
end;

function TJvCustomLogin.CheckUnlock(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnUnlockApp) then
    FOnUnlockApp(Self, UserName, Password, Result)
  else
  if Assigned(FOnUnlock) then
    Result := FOnUnlock(Password);
end;

function TJvCustomLogin.CreateLoginForm(UnlockMode: Boolean): TJvLoginForm;
begin
  Result := TJvLoginForm.Create(Application);
  Result.Caption := FCaption;
  with Result do
  begin
    FUnlockMode := UnlockMode;
    if FUnlockMode then
    begin
      FormStyle := fsNormal;
      FSelectDatabase := False;
    end
    else
      FormStyle := fsStayOnTop;
    if Assigned(Self.FOnIconDblClick) then
    begin
      with AppIcon do
      begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
      with KeyImage do
      begin
        OnDblClick := DoIconDblClick;
        Cursor := crHand;
      end;
    end;
    PasswordEdit.MaxLength := FMaxPasswordLen;
    {$IFDEF VCL}
    PasswordEdit.PasswordChar := PasswordChar;
    {$ENDIF VCL}
    AttemptNumber := Self.AttemptNumber;
  end;
end;

function TJvCustomLogin.DoUnlockDialog: Boolean;
begin
  with CreateLoginForm(True) do
  try
    OnFormShow := nil;
    OnOkClick := UnlockOkClick;
    with UserNameEdit do
    begin
      Text := LoggedUser;
      ReadOnly := True;
      Font.Color := clGrayText;
    end;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

{$IFDEF VCL}
function TJvCustomLogin.UnlockHook(var Msg: TMessage): Boolean;

  function DoUnlock: Boolean;
  var
    Popup: HWND;
  begin
    with Application do
      if IsWindowVisible(Handle) and IsWindowEnabled(Handle) then
        SetForegroundWindow(Handle);
    if FUnlockDlgShowing then
    begin
      Popup := GetLastActivePopup(Application.Handle);
      if (Popup <> 0) and IsWindowVisible(Popup) and
        (WindowClassName(Popup) = TJvLoginForm.ClassName) then
        SetForegroundWindow(Popup);
      Result := False;
      Exit;
    end;
    FUnlockDlgShowing := True;
    try
      Result := DoUnlockDialog;
    finally
      FUnlockDlgShowing := False;
    end;
    if Result then
    begin
      Application.UnhookMainWindow(UnlockHook);
      FLocked := False;
    end;
  end;

begin
  Result := False;
  if not FLocked then
    Exit;
  with Msg do
  begin
    case Msg of
      WM_QUERYOPEN:
        UnlockHook := not DoUnlock;
      WM_SHOWWINDOW:
        if WParam <> 0 then
          UnlockHook := not DoUnlock;
      WM_SYSCOMMAND:
        if ((WParam and $FFF0) = SC_RESTORE) or ((WParam and $FFF0) = SC_ZOOM) then
          UnlockHook := not DoUnlock;
    end;
  end;
end;
{$ENDIF VCL}

//=== { TJvLoginDialog } =====================================================

procedure TJvLoginDialog.Loaded;
var
  Loading: Boolean;
begin
  Loading := csLoading in ComponentState;
  inherited Loaded;
  if not (csDesigning in ComponentState) and Loading then
    if Active and not Login then
      TerminateApplication;
end;

procedure TJvLoginDialog.OkButtonClick(Sender: TObject);
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

function TJvLoginDialog.DoCheckUser(const UserName, Password: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCheckUser) then
    FOnCheckUser(Self, UserName, Password, Result);
end;

procedure TJvLoginDialog.WriteUserName(const UserName: string);
begin
  if Assigned(AppStorage) then
    AppStorage.WriteString(AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]), UserName);
end;

function TJvLoginDialog.ReadUserName(const UserName: string): string;
begin
  if Assigned(AppStorage) then
    Result := AppStorage.ReadString(AppStorage.ConcatPaths([AppStoragePath, RsLastLoginUserName]), UserName)
  else
    Result := UserName;
end;

function TJvLoginDialog.DoLogin(var UserName: string): Boolean;
begin
  try
    with CreateLoginForm(False) do
    try
      OnOkClick := Self.OkButtonClick;
      UserName := ReadUserName(UserName);
      UserNameEdit.Text := UserName;
      Result := (ShowModal = mrOk);
      if Result then
      begin
        UserName := UserNameEdit.Text;
        WriteUserName(UserName);
      end;
    finally
      Free;
    end;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

//=== { TJvLoginForm } =======================================================

procedure TJvLoginForm.FormCreate(Sender: TObject);
begin
  Icon := Application.Icon;
  {$IFDEF VCL}
  if Icon.Empty then
    Icon.Handle := LoadIcon(0, IDI_APPLICATION);
  {$ENDIF VCL}
  AppIcon.Picture.Assign(Icon);
  AppTitleLabel.Caption := Format(RsAppTitleLabel, [Application.Title]);
  PasswordLabel.Caption := RsPasswordLabel;
  UserNameLabel.Caption := RsUserNameLabel;
  OkBtn.Caption := SOKButton;
  CancelBtn.Caption := SCancelButton;
end;

procedure TJvLoginForm.OkBtnClick(Sender: TObject);
begin
  Inc(FAttempt);
  if Assigned(FOnOkClick) then
    FOnOkClick(Self)
  else
    ModalResult := mrOk;
  if (ModalResult <> mrOk) and (FAttempt >= AttemptNumber) then
    ModalResult := mrCancel;
end;

procedure TJvLoginForm.FormShow(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if FSelectDatabase then
  begin
    ClientHeight := CustomCombo.Top + PasswordEdit.Top - UserNameEdit.Top;
    S := RsDatabaseName;
    I := Pos(':', S);
    if I = 0 then
      I := Length(S);
    CustomLabel.Caption := '&' + Copy(S, 1, I);
  end
  else
  begin
    ClientHeight := PasswordEdit.Top + PasswordEdit.Top - UserNameEdit.Top;
    CustomLabel.Visible := False;
    CustomCombo.Visible := False;
  end;
  if not FUnlockMode then
  begin
    HintLabel.Caption := RsHintLabel;
    if Caption = '' then
      Caption := RsRegistrationCaption;
  end
  else
  begin
    HintLabel.Caption := RsUnlockHint;
    if Caption = '' then
      Caption := RsUnlockCaption;
  end;
  if (UserNameEdit.Text = '') and not FUnlockMode then
    ActiveControl := UserNameEdit
  else
    ActiveControl := PasswordEdit;
  if Assigned(FOnFormShow) then
    FOnFormShow(Self);
  FAttempt := 0;
end;

function TJvCustomLogin.Execute: Boolean;
begin
  Result := Login;
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

