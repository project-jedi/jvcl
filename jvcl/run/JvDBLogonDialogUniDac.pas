{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBLogonDialogUniDac.pas, released on 2006-07-21.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):
Jens Fudickar

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id: jvcl/run/JvDBLogonDialogUniDac.pas Jens Fudickar date $

unit JvDBLogonDialogUniDac;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
  Classes, Forms, Controls, DBAccess, Uni, UniProvider, UniDacVcl,
  JvAppStorage, JvBaseDBLogonDialog,
  JvDynControlEngine, JvBaseDBPasswordDialog,
  {$ENDIF USE_3RDPARTY_DEVART_UNIDAC}
  JvDynControlEngineIntf, JvDBLogonDialogBaseDevart;

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
type
  TJvUniDacLogonDialogFillListEvent = procedure(const Provider : string;List: TStringList) of object;

  TJvUniDacConnectionInfo = class(TJvBaseOracleConnectionInfo)
  private
    FDatabaseEnabled: Boolean;
    FUserNameEnabled: Boolean;
    FPasswordEnabled: Boolean;
    FServerEnabled: Boolean;
    FPortEnabled: Boolean;
    FDirect: Boolean;
    FOracleHome: string;
    FPort: Integer;
    FProvider: String;
    FServer: String;
    FUniConnection: TUniConnection;
    procedure RecalculateEnabledProperties;
    procedure SetOracleHome(const Value: string);
    procedure SetProvider(const Value: String);
    procedure SetServer(const Value: String);
    procedure SetUniConnection(const Value: TUniConnection);
  protected
    function GetAliasEnabled: Boolean; override;
    function GetConnectAsEnabled: Boolean; override;
    function GetDatabaseEnabled: Boolean; override;
    function GetServerEnabled: Boolean; virtual;
    function GetPortEnabled: Boolean; virtual;
    function GetOracleHomeEnabled: Boolean; virtual;
    function GetDirectEnabled: Boolean; virtual;
    function GetPasswordEnabled: Boolean; override;
    function GetUsernameEnabled: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    //1 This function is to identify the connection info in the connection list
    function SearchName: String; override;
    property ServerEnabled: Boolean read GetServerEnabled;
    property PortEnabled: Boolean read GetPortEnabled;
    property OracleHomeEnabled: Boolean read GetOracleHomeEnabled;
    property DirectEnabled: Boolean read GetDirectEnabled;
    property UniConnection: TUniConnection read FUniConnection write SetUniConnection;
  public
    constructor Create(AOwner: TComponent); override;
    function ConnectString: string; override;
    function DatabaseGroupIdentifier: string; override;
    function IsConnectAllowed(AllowNullPasswords: Boolean): Boolean; override;
  published
    property Direct: Boolean read FDirect write FDirect;
    property OracleHome: string read FOracleHome write SetOracleHome;
    property Port: Integer read FPort write FPort;
    property Provider: String read FProvider write SetProvider;
    property Server: String read FServer write SetServer;
  end;

  TJvUniDacConnectionList = class(TJvBaseConnectionList)
  private
    FUniConnection: TUniConnection;
    procedure SetUniConnection(const Value: TUniConnection);
  protected
    function CreateObject: TPersistent; override;
    function GetConnection(I: Longint): TJvUniDacConnectionInfo;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property UniConnection: TUniConnection read FUniConnection write SetUniConnection;
  public
    property Connection[I: Longint]: TJvUniDacConnectionInfo read GetConnection;
  end;

  TJvDBUniDacLogonDialogOptions = class(TJvBaseDBOracleLogonDialogOptions)
  private
    FShowDirectConnect: Boolean;
    FShowOracleHome: Boolean;
  public
    constructor Create; override;
  published
//    property AllowPasswordChange default True;
//    property PasswordDialogOptions;
    property ShowDirectConnect: Boolean read FShowDirectConnect write FShowDirectConnect default True;
    property ShowOracleHome: Boolean read FShowOracleHome write FShowOracleHome default False;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBUniDacLogonDialog = class(TJvBaseDBOracleLogonDialog)
  private
    DirectCheckBox: TWinControl;
    FInternalConnectDialog: TUniConnectDialog;
    FInternalConnection: TUniConnection;
    FOnFillServerList: TJvUniDacLogonDialogFillListEvent;
    FUniConnection: TUniConnection;
    IDirectCheckBox: IJvDynControlCheckBox;
    IOracleHomeEditData: IJvDynControlData;
    IOracleHomeEditItems: IJvDynControlItems;
    IPortEditData: IJvDynControlData;
    IProviderEditData: IJvDynControlData;
    IProviderEditItems: IJvDynControlItems;
    IServerEditData: IJvDynControlData;
    IServerEditItems: IJvDynControlItems;
    OracleHomeEdit: TWinControl;
    OracleHomePanel: TWinControl;
    PortEdit: TWinControl;
    PortPanel: TWinControl;
    ProviderEdit: TWinControl;
    ProviderPanel: TWinControl;
    ServerEdit: TWinControl;
    ServerPanel: TWinControl;
    function GetConnectionList: TJvUniDacConnectionList;
    function GetCurrentConnectionInfo: TJvUniDacConnectionInfo;
    function GetOptions: TJvDBUniDacLogonDialogOptions;
    function GetUniConnection: TUniConnection;
    procedure SetOptions(const Value: TJvDBUniDacLogonDialogOptions);
    property InternalConnectDialog: TUniConnectDialog read FInternalConnectDialog;
    property InternalConnection: TUniConnection read FInternalConnection;
  protected
    procedure CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl: TWinControl); override;
    procedure FillAllComoboBoxes; override;
    procedure FillDatabaseComboBoxValues(Items: TStrings); override;
    procedure FillServerComboBox;
    procedure FillServerComboBoxValues(Items: TStrings); virtual;
    procedure FreeFormControls; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonConnectionListClass: TJvBaseConnectionListClass; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass; override;
    procedure HandleExpiredPassword(const ErrorMessage: string);
    procedure ProviderOnEditChange(Sender: TObject);
    procedure SetEditPanelsTabOrder; override;
    procedure SetEditPanelsVisibility; override;
    procedure SetSession(const Value: TComponent); override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    property ConnectionList: TJvUniDacConnectionList read GetConnectionList;
    property UniConnection: TUniConnection read GetUniConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearControlInterfaceObjects; override;
    procedure ConnectSession; override;
    procedure DisconnectSession; override;
    function SessionIsConnected: Boolean; override;
    property CurrentConnectionInfo: TJvUniDacConnectionInfo read GetCurrentConnectionInfo;
  published
    property Options: TJvDBUniDacLogonDialogOptions read GetOptions write SetOptions;
    property OnFillServerList: TJvUniDacLogonDialogFillListEvent read FOnFillServerList write FOnFillServerList;
  end;

  TJvDBUniDacConnectDialog = class(TJvDBBaseDevartConnectDialog)
  private
    FOnFillDatabaseList: TJvLogonDialogFillListEvent;
    FOnFillServerList: TJvLogonDialogFillListEvent;
  protected
    function CreateLogonDialogInternal: TJvBaseDBLogonDialog; override;
  published
    procedure InternalFillDatabaseList(List: TStringList);
    //1 Event for filling the database list
    property OnFillDatabaseList: TJvLogonDialogFillListEvent read FOnFillDatabaseList write FOnFillDatabaseList;
    //1 Event for filling the server list
    property OnFillServerList: TJvLogonDialogFillListEvent read FOnFillServerList write FOnFillServerList;
  end;
{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: jvcl/run/JvDBLogonDialogUniDac.pas $';
    Revision: '$Revision: 452e37996b3821b0e7ca9082f2fe381bf5c65e15 $';
    Date: '$Date: 2014-09-21 21:26:51 +0200 $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF USE_3RDPARTY_DEVART_UNIDAC}
uses
  SysUtils, StdCtrls, Dialogs,
  JvDSADialogs, JvResources, JvJVCLUtils;

//=== { TJvDBOdacLogonDialogOptions } ========================================


constructor TJvDBUniDacLogonDialogOptions.Create;
begin
  inherited Create;
  FShowDirectConnect := True;
//  AllowPasswordChange := True;
  FShowOracleHome := False;
end;

function TJvDBUniDacConnectDialog.CreateLogonDialogInternal: TJvBaseDBLogonDialog;
begin
  Result := TJvDBUniDacLogonDialog.Create(Self);
  TJvDBUniDacLogonDialog(Result).OnFillDatabaseList := InternalFillDatabaseList;
end;

procedure TJvDBUniDacConnectDialog.InternalFillDatabaseList(List: TStringList);
begin
  GetServerList(List);
  if Assigned(OnFillDatabaseList) then
    OnFillDatabaseList(List);
end;

constructor TJvDBUniDacLogonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalConnectDialog := TUniConnectDialog.Create(Self);
  FInternalConnection := TUniConnection.Create(self);
  FInternalConnection.ConnectDialog := FInternalConnectDialog;
  TJvUniDacConnectionList(ConnectionList).UniConnection := InternalConnection;
  CurrentConnectionInfo.UniConnection := InternalConnection;
end;

destructor TJvDBUniDacLogonDialog.Destroy;
begin
  FreeAndNil(FInternalConnection);
  FreeAndNil(FInternalConnectDialog);
  inherited Destroy;
end;

procedure TJvDBUniDacLogonDialog.ClearControlInterfaceObjects;
begin
  inherited ClearControlInterfaceObjects;
  IDirectCheckBox := nil;
  IOracleHomeEditData := nil;
  IOracleHomeEditItems := nil;
  IPortEditData := nil;
  IProviderEditData := nil;
  IProviderEditItems := nil;
  IServerEditData := nil;
  IServerEditItems := nil;
end;

procedure TJvDBUniDacLogonDialog.ConnectSession;
begin
  if Assigned(UniConnection) then
  try
    UniConnection.DisConnect;
    UniConnection.PerformConnect;
  except
    on E: EDAError do
    begin
//      case E.ErrorCode of
//        1005, 1017:
//          ActivatePasswordControl;
//        12203, 12154:
//          ActivateDatabaseControl;
//      end;
//      if (E.ErrorCode = 28001) or (E.ErrorCode = 28002) or (E.ErrorCode = 28011) then
//        HandleExpiredPassword(E.Message)
//      else
        JvDSADialogs.MessageDlg(E.Message, mtError, [mbok], 0, dckScreen,
          0, mbDefault, mbDefault, mbDefault, DynControlEngine);
    end;
  end;
end;

procedure TJvDBUniDacLogonDialog.DisconnectSession;
begin
  if Assigned(UniConnection) then
    UniConnection.DisConnect;
end;

procedure TJvDBUniDacLogonDialog.CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl: TWinControl);
var
  DynControlComboBox:IJvDynControlComboBox;
  UniProviderNames: TStringList;
  IDynControl: IJvDynControl;
begin
  inherited CreateAdditionalConnectDialogControls (AOwner, AParentControl);
  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'Provider', RsProvider, jctComboBox, ProviderPanel, ProviderEdit, IProviderEditData, ProviderOnEditChange);
  if Supports(ProviderEdit, IJvDynControlComboBox, DynControlComboBox) then
    DynControlComboBox.ControlSetNewEntriesAllowed(False);
  if Supports(ProviderEdit, IJvDynControlItems, IProviderEditItems) then
  begin
    UniProviderNames := TStringList.Create;
    try
      UniProviders.GetProviderNames(UniProviderNames);
      IProviderEditItems.ControlItems.Assign(UniProviderNames);
    finally
      UniProviderNames.Free;
    end;
    IProviderEditItems.ControlSetSorted(True);
  end;

  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'Server', RsServer, jctComboBox, ServerPanel, ServerEdit, IServerEditData, DefaultOnEditChange);
  Supports(ServerEdit, IJvDynControlItems, IServerEditItems);
  if Supports(ServerEdit, IJvDynControl, IDynControl) then
  begin
    IDynControl.ControlSetOnClick(DefaultOnEditChange);
    IDynControl.ControlSetOnExit(DefaultOnEditChange); // Fix for the VCL/JVCL Controls which did not react on OnChange and OnClick
  end;

  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'OracleHome', RsOracleHome, jctComboBox, OracleHomePanel, OracleHomeEdit, IOracleHomeEditData, DefaultOnEditChange);
  Supports(OracleHomeEdit, IJvDynControlItems, IOracleHomeEditItems);
  if Supports(OracleHomeEdit, IJvDynControl, IDynControl) then
  begin
    IDynControl.ControlSetOnClick(DefaultOnEditChange);
    IDynControl.ControlSetOnExit(DefaultOnEditChange); // Fix for the VCL/JVCL Controls which did not react on OnChange and OnClick
  end;

  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'Port', RsPort, jctEdit, PortPanel, PortEdit, IPortEditData, DefaultOnEditChange);

  DirectCheckBox := DynControlEngine.CreateCheckboxControl(AOwner, AParentControl, 'DirectCheckBox', RsDirectConnect);
  AlignControlTop(DirectCheckBox, PortPanel);
  Supports(DirectCheckBox, IJvDynControlCheckBox, IDirectCheckBox);
  //DirectCheckBox.Hint := RsNetOptionCheckBoxHint;
end;

procedure TJvDBUniDacLogonDialog.FillAllComoboBoxes;
begin
  inherited FillAllComoboBoxes;
  FillServerComboBox;
end;

procedure TJvDBUniDacLogonDialog.FillDatabaseComboBoxValues(Items: TStrings);
var i : Integer;
  Connection: TJvUniDacConnectionInfo;
begin
  if Options.AddConnectionValuesToComboBox then
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if Connection.Provider = CurrentConnectionInfo.Provider then
        if Connection.Database <> '' then
          if Items.IndexOf(Connection.Database) < 0 then
            Items.Add(Connection.Database);
    end;
end;

procedure TJvDBUniDacLogonDialog.FillServerComboBox;
var
  List: TStringList;
  Items: TStringList;
begin
  Items := TStringList.Create;
  try
    Items.Sorted := True;
    InternalConnection.ProviderName := CurrentConnectionInfo.Provider;
    if TUniUtils.CanGetProvider(InternalConnection) then begin
      List := TStringList.Create;
      try
        InternalConnectDialog.GetServerList(List);
        Items.AddStrings(List);
      finally
        List.Free;
      end;
    end;
    FillServerComboBoxValues(Items);
    if Assigned(FOnFillServerList) then
      FOnFillServerList(CurrentConnectionInfo.Provider, Items);
    IServerEditItems.ControlItems.Assign(Items);
  finally
    Items.Free;
  end;
end;

procedure TJvDBUniDacLogonDialog.FillServerComboBoxValues(Items: TStrings);
var i : Integer;
  Connection: TJvUniDacConnectionInfo;
begin
  if Options.AddConnectionValuesToComboBox then
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if Connection.Provider = CurrentConnectionInfo.Provider then
        if Connection.Server <> '' then
          if Items.IndexOf(Connection.Server) < 0 then
            Items.Add(Connection.Server);
    end;
end;

procedure TJvDBUniDacLogonDialog.FreeFormControls;
begin
  IDirectCheckBox := nil;
  IOracleHomeEditData := nil;
  IOracleHomeEditItems := nil;
  IPortEditData := nil;
  IProviderEditData := nil;
  IProviderEditItems := nil;
  IServerEditData := nil;
  IServerEditItems := nil;
  inherited;
end;

function TJvDBUniDacLogonDialog.GetConnectionList: TJvUniDacConnectionList;
begin
  Result := TJvUniDacConnectionList(Inherited ConnectionList);
end;

function TJvDBUniDacLogonDialog.GetCurrentConnectionInfo: TJvUniDacConnectionInfo;
begin
  Result := TJvUniDacConnectionInfo(inherited CurrentConnectionInfo);
end;

class function TJvDBUniDacLogonDialog.GetDBLogonConnectionListClass: TJvBaseConnectionListClass;
begin
  Result := TJvUniDacConnectionList;
end;

class function TJvDBUniDacLogonDialog.GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass;
begin
  Result := TJvDBUniDacLogonDialogOptions;
end;

function TJvDBUniDacLogonDialog.GetOptions: TJvDBUniDacLogonDialogOptions;
begin
  Result := TJvDBUniDacLogonDialogOptions(inherited Options);
end;

function TJvDBUniDacLogonDialog.GetUniConnection: TUniConnection;
begin
  Result := FUniConnection;
end;

procedure TJvDBUniDacLogonDialog.HandleExpiredPassword(const ErrorMessage: string);
begin
  if JvDSADialogs.MessageDlg(ErrorMessage + #13#10 + RsDoYouWantToChangePassword, mtInformation, [mbYes, mbNo], 0, dckScreen,
    0, mbDefault, mbDefault, mbDefault, DynControlEngine) = mrYes then
    if ChangePassword then
      if not SessionIsConnected then
        UniConnection.PerformConnect;
end;

procedure TJvDBUniDacLogonDialog.ProviderOnEditChange(Sender: TObject);
begin
  TransferConnectionInfoFromDialog(CurrentConnectionInfo);
  RearrangeEditPanel;
  FillAllComoboBoxes;
  ValidateConnectBtnEnabled;
end;

function TJvDBUniDacLogonDialog.SessionIsConnected: Boolean;
begin
  Result := UniConnection.Connected;
end;

procedure TJvDBUniDacLogonDialog.SetEditPanelsTabOrder;
begin
  inherited SetEditPanelsTabOrder;
  ProviderPanel.TabOrder := 0;
  ServerPanel.TabOrder := 3;
  PortPanel.TabOrder := 4;
  OracleHomePanel.TabOrder := 5;
end;

procedure TJvDBUniDacLogonDialog.SetEditPanelsVisibility;
begin
  Inherited SetEditPanelsVisibility;
  SetPanelVisible(ServerPanel, CurrentConnectionInfo.ServerEnabled);
  SetPanelVisible(PortPanel, CurrentConnectionInfo.PortEnabled);
  SetPanelVisible(OracleHomePanel, CurrentConnectionInfo.OracleHomeEnabled and Options.ShowOracleHome);
  DirectCheckBox.Visible := CurrentConnectionInfo.DirectEnabled and Options.ShowDirectConnect ;
end;

procedure TJvDBUniDacLogonDialog.SetOptions(const Value: TJvDBUniDacLogonDialogOptions);
begin
  (inherited Options).Assign(Value);
end;

procedure TJvDBUniDacLogonDialog.SetSession(const Value: TComponent);
begin
  inherited SetSession(Value);
  if Value is TUniConnection then
    FUniConnection := tUniConnection(Value)
  else
    FUniConnection := nil;
end;

procedure TJvDBUniDacLogonDialog.TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo);
var UniConnectionInfo : TJvUniDacConnectionInfo;
begin
  if (ConnectionInfo is TJvUniDacConnectionInfo) then
  begin
    UniConnectionInfo := TJvUniDacConnectionInfo(ConnectionInfo);
    if Assigned (IProviderEditData) then
      UniConnectionInfo.Provider := IProviderEditData.ControlValue
    else
      UniConnectionInfo.Provider := '';
  end;
  inherited TransferConnectionInfoFromDialog(ConnectionInfo);
  if (ConnectionInfo is TJvUniDacConnectionInfo) then
  begin
    UniConnectionInfo := TJvUniDacConnectionInfo(ConnectionInfo);
    if Assigned (IOracleHomeEditData) and UniConnectionInfo.OracleHomeEnabled then
      UniConnectionInfo.OracleHome := IOracleHomeEditData.ControlValue
    else
      UniConnectionInfo.OracleHome := '';
    if Assigned (IServerEditData) and UniConnectionInfo.ServerEnabled then
      UniConnectionInfo.Server := IServerEditData.ControlValue
    else
      UniConnectionInfo.Server := '';
    if Assigned (IPortEditData) and (IPortEditData.ControlValue <> '') and UniConnectionInfo.PortEnabled then
      UniConnectionInfo.Port := IPortEditData.ControlValue
    else
      UniConnectionInfo.Port := 0;
    if Assigned (IDirectCheckBox) and UniConnectionInfo.DirectEnabled then
      UniConnectionInfo.Direct := IDirectCheckBox.ControlState = cbChecked
    else
      UniConnectionInfo.Direct := False;
  end;
end;

procedure TJvDBUniDacLogonDialog.TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo);
var UniConnectionInfo : TJvUniDacConnectionInfo;
begin
  inherited TransferConnectionInfoToDialog(ConnectionInfo);
  if (ConnectionInfo is TJvUniDacConnectionInfo) then
  begin
    UniConnectionInfo := TJvUniDacConnectionInfo(ConnectionInfo);
    if Assigned (IOracleHomeEditData) then
      IOracleHomeEditData.ControlValue := UniConnectionInfo.OracleHome;
    if Assigned (IProviderEditData) then
      IProviderEditData.ControlValue := UniConnectionInfo.Provider;
    if Assigned (IServerEditData) then
      IServerEditData.ControlValue := UniConnectionInfo.Server;
    if Assigned (IPortEditData) then
      IPortEditData.ControlValue := UniConnectionInfo.Port;
    if Assigned(IDirectCheckBox) then
      if UniConnectionInfo.Direct then
        IDirectCheckBox.ControlState := cbChecked
      else
        IDirectCheckBox.ControlState := cbunChecked;
    ProviderOnEditChange(nil);
  end;
end;

procedure TJvDBUniDacLogonDialog.TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
var UniConnectionInfo : TJvUniDacConnectionInfo;
begin
  if Assigned(Session) and Assigned(UniConnection)  then
  begin
    UniConnection.Password := ConnectionInfo.Password;
    UniConnection.Username := ConnectionInfo.Username;
    UniConnection.Database := ConnectionInfo.Database;
    if (ConnectionInfo is TJvUniDacConnectionInfo) then
    begin
      UniConnectionInfo := TJvUniDacConnectionInfo(ConnectionInfo);
      UniConnection.Server:= UniConnectionInfo.Server;
      UniConnection.ProviderName:= UniConnectionInfo.Provider;
      UniConnection.Port := UniConnectionInfo.Port;
      if Options.ShowOracleHome then
        if UniConnectionInfo.OracleHomeEnabled then
          UniConnection.SpecificOptions.Values['HomeName'] := UniConnectionInfo.OracleHome;
      if UniConnectionInfo.DirectEnabled then
        if UniConnectionInfo.Direct then
          UniConnection.SpecificOptions.Values['Direct'] := 'true'
        else
          UniConnection.SpecificOptions.Values['Direct'] := 'false';
      if Options.ShowConnectAs then
        if UniConnectionInfo.ConnectAsEnabled then
          if UniConnectionInfo.ConnectAs = 'SYSDBA' then
            UniConnection.SpecificOptions.Values['ConnectMode'] := 'cmSysDBA'
          else if UniConnectionInfo.ConnectAs = 'SYSOPER' then
            UniConnection.SpecificOptions.Values['ConnectMode'] := 'cmSysOPER'
          else if UniConnectionInfo.ConnectAs = 'SYSASM' then
            UniConnection.SpecificOptions.Values['ConnectMode'] := 'cmSysASM'
          else
            UniConnection.SpecificOptions.Values['ConnectMode'] := 'cmNormal';
    end;
  end;
end;

procedure TJvDBUniDacLogonDialog.TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
var UniConnectionInfo : TJvUniDacConnectionInfo;
    ConnectAs: String;
begin
  if Assigned(Session) and Assigned(UniConnection) then
  begin
    ConnectionInfo.Password := UniConnection.Password;
    ConnectionInfo.Username := UniConnection.Username;
    ConnectionInfo.Database := UniConnection.Database;
    if (ConnectionInfo is TJvUniDacConnectionInfo) then
    begin
      UniConnectionInfo := TJvUniDacConnectionInfo(ConnectionInfo);
      UniConnectionInfo.Server :=UniConnection.Server;
      UniConnectionInfo.Provider := UniConnection.ProviderName;
      UniConnectionInfo.Port := UniConnection.Port;
      if UniConnectionInfo.OracleHomeEnabled then
        UniConnectionInfo.OracleHome := UniConnection.SpecificOptions.Values['HomeName'];
      if UniConnectionInfo.DirectEnabled then
        UniConnectionInfo.Direct := Uppercase(UniConnection.SpecificOptions.Values['Direct']) = 'TRUE';
      if UniConnectionInfo.ConnectAsEnabled then
      begin
        ConnectAs := UniConnection.SpecificOptions.Values['ConnectMode'];
        Delete(ConnectAs,1,2);
        if ConnectAs = '' then
          UniConnectionInfo.ConnectAs := UpperCase(ConnectAs)
        else
          UniConnectionInfo.ConnectAs := 'NORMAL';
      end;

    end;
  end;
end;

function TJvUniDacConnectionList.CreateObject: TPersistent;
begin
  Result := TJvUniDacConnectionInfo.Create(Self);
  TJvUniDacConnectionInfo(Result).UniConnection := UniConnection;
end;

function TJvUniDacConnectionList.GetConnection(I: Longint): TJvUniDacConnectionInfo;
begin
  Result := TJvUniDacConnectionInfo(inherited Connection[i])
end;

procedure TJvUniDacConnectionList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FUniConnection) then
    UniConnection := nil;
end;

procedure TJvUniDacConnectionList.SetUniConnection(const Value: TUniConnection);
var
  i: Integer;
begin
  ReplaceComponentReference(Self, Value, tComponent(FUniConnection));
  for i := 0 to Count-1 do
    TJvUniDacConnectionInfo(Connection[i]).UniConnection := Value;
end;

constructor TJvUniDacConnectionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RecalculateEnabledProperties;
end;

function TJvUniDacConnectionInfo.ConnectString: string;
begin
  if UsernameEnabled then
    Result := TranslateUserName(Username)
  else
    Result := '';
  if PasswordEnabled and (Password <> '') then
    Result := Result + '/*****';
  if AliasEnabled and (Alias <> '') then
    Result := Result + '@' + Alias
  else
    if ServerEnabled and (Server <> '') then
      Result := Result +'@'+TranslateDatabaseName(Server);
  if ConnectAsEnabled and (ConnectAs <> 'NORMAL') then
    Result := Result +' ['+ConnectAs+']';
  if not(AliasEnabled and (Alias <> '')) and
    DatabaseEnabled and (Database <> '') then
    Result := Result + ' - ' + Database;
  Result := Result + ' {'+Provider+'}';
  if ShortCutText <> '' then
    Result := Result + ' ('+ShortCutText+')';
end;

function TJvUniDacConnectionInfo.DatabaseGroupIdentifier: string;
begin
  if AliasEnabled and (Alias <> '') then
    Result := Alias
  else
    if ServerEnabled then
      Result := TranslateDatabaseName(Server)
    else
      Result := Database;
end;

function TJvUniDacConnectionInfo.GetAliasEnabled: Boolean;
begin
  Result := inherited GetAliasEnabled or ServerEnabled;
end;

function TJvUniDacConnectionInfo.GetConnectAsEnabled: Boolean;
begin
  Result := GetOracleHomeEnabled;
end;

function TJvUniDacConnectionInfo.GetDatabaseEnabled: Boolean;
begin
  Result := fDatabaseEnabled;
end;

function TJvUniDacConnectionInfo.GetServerEnabled: Boolean;
begin
  Result := FServerEnabled;
end;

function TJvUniDacConnectionInfo.GetPortEnabled: Boolean;
begin
  Result := FPortEnabled;
end;

function TJvUniDacConnectionInfo.GetOracleHomeEnabled: Boolean;
begin
  Result := Provider = 'Oracle';
end;

function TJvUniDacConnectionInfo.GetDirectEnabled: Boolean;
begin
  Result := Provider = 'Oracle';
end;

function TJvUniDacConnectionInfo.GetPasswordEnabled: Boolean;
begin
  Result := fPasswordEnabled;
end;

function TJvUniDacConnectionInfo.GetUsernameEnabled: Boolean;
begin
  Result := fUsernameEnabled;
end;

function TJvUniDacConnectionInfo.IsConnectAllowed(AllowNullPasswords: Boolean): Boolean;
begin
  Result:= inherited IsConnectAllowed(AllowNullPasswords);
  Result := Result and (Provider <> '');
  if PortEnabled then
    Result := Result and (Port > 0);
  if ServerEnabled then
    Result := Result and (Server <> '');
end;

procedure TJvUniDacConnectionInfo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FUniConnection) then
    UniConnection := nil;
end;

procedure TJvUniDacConnectionInfo.RecalculateEnabledProperties;
var
  UniProvider: TUniProvider;
begin
  FDatabaseEnabled := True;
  FUserNameEnabled := True;
  FPasswordEnabled := True;
  FServerEnabled := True;
  FPortEnabled := True;
  if Assigned(UniConnection) and Assigned(UniConnection.ConnectDialog) and(UniConnection.ConnectDialog is TUniConnectDialog) then
  begin
    UniConnection.ProviderName := Provider;
    if TUniUtils.CanGetProvider(UniConnection) then
    begin
      UniProvider := TUniUtils.GetProvider(UniConnection);
      FUserNameEnabled := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(UniConnection.ConnectDialog)).UsernameEnabled;
      FPasswordEnabled := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(UniConnection.ConnectDialog)).PasswordEnabled;
      FServerEnabled := TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(UniConnection.ConnectDialog)).ServerEnabled;
      FDatabaseEnabled := UniProvider.IsDatabaseSupported and
        TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(UniConnection.ConnectDialog)).DatabaseEnabled;
      FPortEnabled := UniProvider.IsPortSupported and
        TUniConnectDialogUtils.GetConnectDialogService(TUniConnectDialog(UniConnection.ConnectDialog)).PortEnabled;
    end;
  end;
end;

function TJvUniDacConnectionInfo.SearchName: String;
begin
  Result := TranslateUserName(UserName) +'@'+TranslateDatabaseName(Server);
  Result := Result +'['+inttostr(Port)+']';
  Result := Result + '-'+Database;
  Result := Result + '('+Provider+')';
end;

procedure TJvUniDacConnectionInfo.SetOracleHome(const Value: string);
begin
  FOracleHome := Trim(Value);
end;

procedure TJvUniDacConnectionInfo.SetProvider(const Value: String);
begin
  FProvider := Trim(Value);
  RecalculateEnabledProperties;
end;

procedure TJvUniDacConnectionInfo.SetServer(const Value: String);
begin
  FServer := Trim(Value);
end;

procedure TJvUniDacConnectionInfo.SetUniConnection(const Value: TUniConnection);
begin
  ReplaceComponentReference(Self, Value, tComponent(FUniConnection));
  RecalculateEnabledProperties;
end;

{$ENDIF USE_3RDPARTY_DEVART_UNIDAC}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
