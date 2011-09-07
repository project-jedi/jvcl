{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLogonDialogOdac.pas, released on 2006-07-21.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):
Jens Fudickar

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBLogonDialogOdac;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_CORELAB_ODAC}
  Classes, Forms, Controls, DBAccess, Ora,
  JvBaseDlg, JvAppStorage, JvBaseDBLogonDialog,
  JvDynControlEngine, JvBaseDBPasswordDialog, 
  {$ENDIF USE_3RDPARTY_CORELAB_ODAC}
  JvDynControlEngineIntf;

{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
type

  TJvOdacOracleConnectionInfo = class(TJvBaseOracleConnectionInfo)
  private
    FNet: Boolean;
    FOracleHome: string;
  public
    constructor Create(AOwner: TComponent); override;
    function ConnectString(ShowShortCut, ShowConnectGroup: Boolean): string;
        override;
  published
    property Net: Boolean read FNet write FNet default false;
    property OracleHome: string read FOracleHome write FOracleHome;
  end;

  TJvOdacOracleConnectionList = class(TJvBaseOracleConnectionList)
  protected
    function CreateObject: TPersistent; override;
  end;

  TJvDBOdacLogonDialogOptions = class(TJvBaseDBOracleLogonDialogOptions)
  private
    FShowNetOption: Boolean;
    FShowOracleHome: Boolean;
  public
    constructor Create; override;
  published
    property AllowPasswordChange default True;
    property PasswordDialogOptions;
    property ShowNetOption: Boolean read FShowNetOption write FShowNetOption default True;
    property ShowOracleHome: Boolean read FShowOracleHome write FShowOracleHome default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBOdacLogonDialog = class(TJvBaseDBOracleLogonDialog)
  private
    FOraSession: TOraSession;
    INetOptionCheckBox: IJvDynControlCheckBox;
    IOracleHomeEditData: IJvDynControlData;
    NetOptionCheckBox: TWinControl;
    OracleHomeEdit: TWinControl;
    OracleHomePanel: TWinControl;
    function GetOptions: TJvDBOdacLogonDialogOptions;
    function GetOraSession: TOraSession;
    function GetSession: TCustomDAConnection;
    procedure SetOptions(const Value: TJvDBOdacLogonDialogOptions);
    procedure SetSession(const Value: TCustomDAConnection); reintroduce;
  protected
    procedure CreateAdditionalConnectDialogControls(AOwner: TComponent;
      AParentControl: TWinControl); override;
    procedure CreateFormControls(AForm: TForm); override;
    function CreatePasswordChangeDialog: TJvBaseDBPasswordDialog; override;
    procedure FillDatabaseComboBoxDefaultValues(Items: TStrings); override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonConnectionListClass: TJvBaseConnectionListClass;
        override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass; override;
    procedure HandleExpiredPassword(const ErrorMessage: string);
    procedure ResizeFormControls; override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    property OraSession: TOraSession read GetOraSession;
  public
    procedure ClearControlInterfaceObjects; override;
    procedure ConnectSession; override;
    function SessionIsConnected: Boolean; override;
  published
    property Options: TJvDBOdacLogonDialogOptions read GetOptions write SetOptions;
    property Session: TCustomDAConnection read GetSession write SetSession;
  end;

  TJvDBOdacConnectDialog = class(TCustomConnectDialog)
  private
    FLogonDialogInternal: TJvDBOdacLogonDialog;
    FOnFillDatabaseList: TJvLogonDialogFillListEvent;
    function GetAfterTransferSessionDataToConnectionInfo:
      TJvLogonDialogConnectionInfoEvent;
    function GetAppStorage: TJvCustomAppStorage;
    function GetAppStoragePath: string;
    function GetBeforeTransferConnectionInfoToSessionData:
      TJvLogonDialogConnectionInfoEvent;
    function GetConnectedDialogConnectionInfo: TJvBaseConnectionInfo;
    function GetDynControlEngine: TJvDynControlEngine;
    function GetOnDecryptPassword: TJvLogonDialogEncryptDecryptEvent;
    function GetOnEncryptPassword: TJvLogonDialogEncryptDecryptEvent;
    function GetOnFillShortcutList: TJvLogonDialogFillListEvent;
    function GetOnSessionConnect: TJvLogonDialogBaseSessionEvent;
    function GetOptions: TJvDBOdacLogonDialogOptions;
    procedure SetAfterTransferSessionDataToConnectionInfo(const Value: TJvLogonDialogConnectionInfoEvent);
    procedure SetBeforeTransferConnectionInfoToSessionData(const Value: TJvLogonDialogConnectionInfoEvent);
    procedure SetDynControlEngine(const Value: TJvDynControlEngine);
    procedure SetOnDecryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
    procedure SetOnEncryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
    procedure SetOnFillShortcutList(const Value: TJvLogonDialogFillListEvent);
    procedure SetOnSessionConnect(const Value: TJvLogonDialogBaseSessionEvent);
    procedure SetOptions(const Value: TJvDBOdacLogonDialogOptions);
  protected
    procedure SetAppStorage(Value: TJvCustomAppStorage);
    procedure SetAppStoragePath(Value: string); virtual;
    property LogonDialogInternal: TJvDBOdacLogonDialog read FLogonDialogInternal write FLogonDialogInternal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConnectSession;
    function Execute: Boolean; override;
    function ExecuteOnSession(Session: TCustomDAConnection): Boolean;
    property ConnectedDialogConnectionInfo: TJvBaseConnectionInfo read
        GetConnectedDialogConnectionInfo;
  published
    procedure InternalFillDatabaseList(List: TStringList);
    //1 This events gives you the possibility to modify the connection data after receiving the data from the current session
    property AfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent read
      GetAfterTransferSessionDataToConnectionInfo write SetAfterTransferSessionDataToConnectionInfo;
    property AppStorage: TJvCustomAppStorage read GetAppStorage write SetAppStorage;
    property AppStoragePath: string read GetAppStoragePath write SetAppStoragePath;
    //1 This Event gives you the possibility to modify the connection data before it is transfered to the current session
    property BeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent read
      GetBeforeTransferConnectionInfoToSessionData write SetBeforeTransferConnectionInfoToSessionData;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write
      SetDynControlEngine;
    property Options: TJvDBOdacLogonDialogOptions read GetOptions write SetOptions;
    property OnDecryptPassword: TJvLogonDialogEncryptDecryptEvent read
      GetOnDecryptPassword write SetOnDecryptPassword;
    property OnEncryptPassword: TJvLogonDialogEncryptDecryptEvent read
      GetOnEncryptPassword write SetOnEncryptPassword;
    //1 Event for filling the database list
    property OnFillDatabaseList: TJvLogonDialogFillListEvent read
        FOnFillDatabaseList write FOnFillDatabaseList;
    //1 Event for customizing the shortcut list
    property OnFillShortcutList: TJvLogonDialogFillListEvent read GetOnFillShortcutList write SetOnFillShortcutList;
    property OnSessionConnect: TJvLogonDialogBaseSessionEvent read
        GetOnSessionConnect write SetOnSessionConnect;
  end;
{$ENDIF USE_3RDPARTY_CORELAB_ODAC}

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

{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
uses
  SysUtils, StdCtrls, Dialogs,
  OraClasses, OraError, OraCall, OraServices,
  JvDSADialogs, JvDBPasswordDialogOdac, JvResources;

//=== { TJvDBOdacLogonDialogOptions } ========================================

constructor TJvDBOdacLogonDialogOptions.Create;
begin
  inherited Create;
  FShowNetOption := True;
  AllowPasswordChange := True;
  FShowOracleHome := False;
end;

//=== { TJvDBOdacConnectDialog } =============================================

constructor TJvDBOdacConnectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogonDialogInternal := TJvDBOdacLogonDialog.Create(Self);
  FLogonDialogInternal.OnFillDatabaseList := InternalFillDatabaseList;
end;

destructor TJvDBOdacConnectDialog.Destroy;
begin
  FreeAndNil(FLogonDialogInternal);
  inherited Destroy;
end;

procedure TJvDBOdacConnectDialog.ConnectSession;
begin
  if Assigned(FLogonDialogInternal) then
    FLogonDialogInternal.ConnectSession;
end;

function TJvDBOdacConnectDialog.Execute: Boolean;
begin
  Result := ExecuteOnSession(Connection);
end;

function TJvDBOdacConnectDialog.ExecuteOnSession(Session: TCustomDAConnection):
    Boolean;
begin
  if Assigned(FLogonDialogInternal) then
  begin
    LogonDialogInternal.Session := Session;
    Result := LogonDialogInternal.Execute;
  end
  else
    Result := False;
end;

function TJvDBOdacConnectDialog.GetAfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent;
begin
  Result := LogonDialogInternal.AfterTransferSessionDataToConnectionInfo;
end;

function TJvDBOdacConnectDialog.GetAppStorage: TJvCustomAppStorage;
begin
  Result := LogonDialogInternal.AppStorage;
end;

function TJvDBOdacConnectDialog.GetAppStoragePath: string;
begin
  Result := LogonDialogInternal.AppStoragePath;
end;

function TJvDBOdacConnectDialog.GetBeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent;
begin
  Result := LogonDialogInternal.BeforeTransferConnectionInfoToSessionData;
end;

function TJvDBOdacConnectDialog.GetConnectedDialogConnectionInfo:
    TJvBaseConnectionInfo;
begin
  Result := LogonDialogInternal.ConnectedDialogConnectionInfo;
end;

function TJvDBOdacConnectDialog.GetDynControlEngine: TJvDynControlEngine;
begin
  Result := LogonDialogInternal.DynControlEngine
end;

function TJvDBOdacConnectDialog.GetOnDecryptPassword: TJvLogonDialogEncryptDecryptEvent;
begin
  Result := LogonDialogInternal.OnDecryptPassword;
end;

function TJvDBOdacConnectDialog.GetOnEncryptPassword: TJvLogonDialogEncryptDecryptEvent;
begin
  Result := LogonDialogInternal.OnEncryptPassword;
end;

function TJvDBOdacConnectDialog.GetOnFillShortcutList: TJvLogonDialogFillListEvent;
begin
  Result := LogonDialogInternal.OnFillShortcutList;
end;

function TJvDBOdacConnectDialog.GetOnSessionConnect:
    TJvLogonDialogBaseSessionEvent;
begin
  Result := LogonDialogInternal.OnSessionConnect;
end;

function TJvDBOdacConnectDialog.GetOptions: TJvDBOdacLogonDialogOptions;
begin
  Result := LogonDialogInternal.Options
end;

procedure TJvDBOdacConnectDialog.InternalFillDatabaseList(List: TStringList);
begin
  GetServerList(List);
  if Assigned(OnFillDatabaseList) then
    OnFillDatabaseList(List);
end;

procedure TJvDBOdacConnectDialog.SetAfterTransferSessionDataToConnectionInfo(
  const Value: TJvLogonDialogConnectionInfoEvent);
begin
  LogonDialogInternal.AfterTransferSessionDataToConnectionInfo := Value;
end;

procedure TJvDBOdacConnectDialog.SetAppStorage(Value: TJvCustomAppStorage);
begin
  LogonDialogInternal.AppStorage := Value;
end;

procedure TJvDBOdacConnectDialog.SetAppStoragePath(Value: string);
begin
  LogonDialogInternal.AppStoragePath := Value;
end;

procedure TJvDBOdacConnectDialog.SetBeforeTransferConnectionInfoToSessionData(
  const Value: TJvLogonDialogConnectionInfoEvent);
begin
  LogonDialogInternal.BeforeTransferConnectionInfoToSessionData := Value;
end;

procedure TJvDBOdacConnectDialog.SetDynControlEngine(const Value: TJvDynControlEngine);
begin
  LogonDialogInternal.DynControlEngine := Value;
end;

procedure TJvDBOdacConnectDialog.SetOnDecryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
begin
  LogonDialogInternal.OnDecryptPassword := Value;
end;

procedure TJvDBOdacConnectDialog.SetOnEncryptPassword(const Value: TJvLogonDialogEncryptDecryptEvent);
begin
  LogonDialogInternal.OnEncryptPassword := Value;
end;

procedure TJvDBOdacConnectDialog.SetOnFillShortcutList(const Value: TJvLogonDialogFillListEvent);
begin
  LogonDialogInternal.OnFillShortcutList := Value;
end;

procedure TJvDBOdacConnectDialog.SetOnSessionConnect(const Value:
    TJvLogonDialogBaseSessionEvent);
begin
  LogonDialogInternal.OnSessionConnect:= Value;
end;

procedure TJvDBOdacConnectDialog.SetOptions(const Value: TJvDBOdacLogonDialogOptions);
begin
  LogonDialogInternal.Options.Assign(Value);
end;

procedure TJvDBOdacLogonDialog.ClearControlInterfaceObjects;
begin
  inherited ClearControlInterfaceObjects;
  INetOptionCheckBox:= nil;
  IOracleHomeEditData:= nil;
end;

procedure TJvDBOdacLogonDialog.ConnectSession;
begin
  if Assigned(Session) then
  try
    Session.DisConnect;
    Session.PerformConnect;
  except
    on E: EOraError do
    begin
      case E.ErrorCode of
        1005, 1017:
          ActivatePasswordControl;
        12203, 12154:
          ActivateDatabaseControl;
      end;
      if (E.ErrorCode = 28001) or (E.ErrorCode = 28002) or (E.ErrorCode = 28011) then
        HandleExpiredPassword(E.Message)
      else
        JvDSADialogs.MessageDlg(E.Message, mtError, [mbok], 0, dckScreen,
          0, mbDefault, mbDefault, mbDefault, DynControlEngine);
    end;
  end;
end;

procedure TJvDBOdacLogonDialog.CreateAdditionalConnectDialogControls(AOwner: TComponent;
  AParentControl: TWinControl);
var
  LabelControl: TControl;
  IDynControlLabel: IJvDynControlLabel;
  Items : TStringList;
  i: Integer;
  IDynControlComboBox: IJvDynControlComboBox;
  IDynControlAutoSize: IJvDynControlAutoSize;
begin
  inherited CreateAdditionalConnectDialogControls (AOwner, AParentControl);
  OracleHomePanel := DynControlEngine.CreatePanelControl(AOwner, AParentControl, 'OracleHomePanel', '', alTop);
  AlignControlTop(OracleHomePanel, nil);
  LabelControl := DynControlEngine.CreateLabelControl(AOwner, OracleHomePanel, 'OracleHomeLabel', RsOracleHome, nil);
  AlignControlTop(LabelControl, nil);
  Items := TStringList.Create;
  try
    for i := 0 to OracleHomeCount - 1 do
      Items.Add(OracleHomeNames[i]);
    OracleHomeEdit := DynControlEngine.CreateComboBoxControl(AOwner, OracleHomePanel, 'OracleHomeEdit', Items);
    if Supports(OracleHomeEdit, IJvDynControlComboBox, IDynControlComboBox) then
      IDynControlComboBox.ControlSetNewEntriesAllowed(False);
  finally
    Items.Free;
  end;
  Supports(OracleHomeEdit, IJvDynControlData, IOracleHomeEditData);
  IOracleHomeEditData.ControlValue := '';
  AlignControlTop(OracleHomeEdit, LabelControl);
  if Supports(LabelControl, IJvDynControlLabel, IDynControlLabel) then
    IDynControlLabel.ControlSetFocusControl(OracleHomeEdit);
  if Supports(LabelControl, IJvDynControlAutoSize,IDynControlAutoSize) then
    IDynControlAutoSize.ControlSetAutoSize(True);
  SetPanelHeight(OracleHomePanel);
  OracleHomePanel.Visible := Options.ShowOracleHome;
  NetOptionCheckBox := DynControlEngine.CreateCheckboxControl(AOwner,AParentControl, 'NetOptionCheckBox',
    RsUseNetOptionForDirectConnect);
  AlignControlTop(NetOptionCheckBox, OracleHomePanel);
  NetOptionCheckBox.Visible := Options.ShowNetOption;
  Supports(NetOptionCheckBox, IJvDynControlCheckBox, INetOptionCheckBox);
  NetOptionCheckBox.Hint := RsNetOptionCheckBoxHint;
end;

procedure TJvDBOdacLogonDialog.CreateFormControls(AForm: TForm);
begin
  inherited CreateFormControls(AForm);
end;

function TJvDBOdacLogonDialog.CreatePasswordChangeDialog:
  TJvBaseDBPasswordDialog;
begin
  Result := TJvDBOdacPasswordDialog.Create(Self);
end;

procedure TJvDBOdacLogonDialog.FillDatabaseComboBoxDefaultValues(Items: TStrings);
var
  Enum: TOraServerEnumerator;
  List: TStringList;
begin
  List := TStringList.Create;
  Enum := TOraServerEnumerator.Create;
  try
    Enum.GetServerList(List);
    Items.AddStrings(List);
  finally
    Enum.Free;
    List.Free;
  end;
end;

class function TJvDBOdacLogonDialog.GetDBLogonConnectionListClass:
    TJvBaseConnectionListClass;
begin
  Result := TJvOdacOracleConnectionList;
end;

class function TJvDBOdacLogonDialog.GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass;
begin
  Result := TJvDBOdacLogonDialogOptions;
end;

function TJvDBOdacLogonDialog.GetOptions: TJvDBOdacLogonDialogOptions;
begin
  Result := TJvDBOdacLogonDialogOptions(inherited Options);
end;

function TJvDBOdacLogonDialog.GetOraSession: TOraSession;
begin
  Result := FOraSession;
end;

function TJvDBOdacLogonDialog.GetSession: TCustomDAConnection;
begin
  Result := TCustomDAConnection(inherited Session);
end;

procedure TJvDBOdacLogonDialog.HandleExpiredPassword(const ErrorMessage: string);
begin
  if JvDSADialogs.MessageDlg(ErrorMessage + #13#10 + RsDoYouWantToChangePassword,
    mtInformation, [mbYes, mbNo], 0, dckScreen,
    0, mbDefault, mbDefault, mbDefault, DynControlEngine) = mrYes then
    if ChangePassword then
      if not SessionIsConnected then
        Session.PerformConnect;
end;

procedure TJvDBOdacLogonDialog.ResizeFormControls;
begin
  inherited ResizeFormControls;
  SetPanelHeight(OracleHomePanel);
end;

function TJvDBOdacLogonDialog.SessionIsConnected: Boolean;
begin
  Result := Session.Connected;
end;

procedure TJvDBOdacLogonDialog.SetOptions(const Value: TJvDBOdacLogonDialogOptions);
begin
  (inherited Options).Assign(Value);
end;

procedure TJvDBOdacLogonDialog.SetSession(const Value: TCustomDAConnection);
begin
  inherited SetSession(Value);
  if Value is TOraSession then
    FOraSession := TOraSession(Value)
  else
    FORaSession := nil;
end;

procedure TJvDBOdacLogonDialog.TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoFromDialog(ConnectionInfo);
  if (ConnectionInfo is TJvOdacOracleConnectionInfo) then
  begin
    if Assigned (INetOptionCheckBox)  then
      TJvOdacOracleConnectionInfo(ConnectionInfo).Net := INetOptionCheckBox.ControlState = cbChecked
    else
      TJvOdacOracleConnectionInfo(ConnectionInfo).Net := False;
    if Assigned (IOracleHomeEditData) then
      TJvOdacOracleConnectionInfo(ConnectionInfo).OracleHome := IOracleHomeEditData.ControlValue
    else
      TJvOdacOracleConnectionInfo(ConnectionInfo).OracleHome := '';
  end;
end;

procedure TJvDBOdacLogonDialog.TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoToDialog(ConnectionInfo);
  if (ConnectionInfo is TJvOdacOracleConnectionInfo) then
  begin
    if Assigned (INetOptionCheckBox)  then
      if TJvOdacOracleConnectionInfo(ConnectionInfo).Net then
        INetOptionCheckBox.ControlState := cbChecked
      else
        INetOptionCheckBox.ControlState := cbunChecked;
    if Assigned (IOracleHomeEditData) then
      IOracleHomeEditData.ControlValue := TJvOdacOracleConnectionInfo(ConnectionInfo).OracleHome;
  end;
end;

procedure TJvDBOdacLogonDialog.TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(Session) then
  begin
    Session.Server := ConnectionInfo.Database;
    Session.Password := ConnectionInfo.Password;
    Session.Username := ConnectionInfo.Username;
    if Assigned(OraSession) and (ConnectionInfo is TJvBaseOracleConnectionInfo) then
    begin
      if TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSDBA' then
        OraSession.ConnectMode := cmSYSDBA
      else
        if TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSOPER' then
          OraSession.ConnectMode := cmSYSOper
        else
          OraSession.ConnectMode := cmNormal;
      if (ConnectionInfo is TJvOdacOracleConnectionInfo) then
      begin
        if Options.ShowNetOption then
          OraSession.Options.Net := TJvOdacOracleConnectionInfo(ConnectionInfo).Net;
        if Options.ShowOracleHome then
          OraSession.HomeName := TJvOdacOracleConnectionInfo(ConnectionInfo).OracleHome;
      end;
    end;
  end;
end;

procedure TJvDBOdacLogonDialog.TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(Session) then
  begin
    ConnectionInfo.Database := Session.Server;
    ConnectionInfo.Password := Session.Password;
    ConnectionInfo.Username := Session.Username;
    if Assigned(OraSession) and (ConnectionInfo is TJvBaseOracleConnectionInfo) then
    begin
      case OraSession.ConnectMode of
        cmSYSDBA:
          TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSDBA';
        cmSYSOPER:
          TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSOPER';
      else
        TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'NORMAL';
      end;
      if (ConnectionInfo is TJvOdacOracleConnectionInfo) then
      begin
        if Options.ShowNetOption then
          TJvOdacOracleConnectionInfo(ConnectionInfo).net := OraSession.Options.net;
        if Options.ShowOracleHome and Assigned (IOracleHomeEditData) then
          TJvOdacOracleConnectionInfo(ConnectionInfo).OracleHome := OraSession.HomeName;
      end;
    end;
  end;
end;

function TJvOdacOracleConnectionList.CreateObject: TPersistent;
begin
  Result := TJvOdacOracleConnectionInfo.Create(Self);
end;

constructor TJvOdacOracleConnectionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNet := false;
end;

function TJvOdacOracleConnectionInfo.ConnectString(ShowShortCut,
    ShowConnectGroup: Boolean): string;
begin
  Result := inherited ConnectString(ShowShortCut, ShowConnectGroup);
  if OracleHome <> '' then
    Result:= Result + ' - '+OracleHome;
  if Net then
    Result := Result + ' - '+RsNetOptionConnectionList;
end;
{$ENDIF USE_3RDPARTY_CORELAB_ODAC}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
