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
  {$IFDEF USE_3RDPARTY_DEVART_ODAC}
  Classes, Forms, Controls, DBAccess, Ora,
  JvAppStorage, JvBaseDBLogonDialog,
  JvDynControlEngine, JvBaseDBPasswordDialog,
  {$ENDIF USE_3RDPARTY_DEVART_ODAC}
  JvDynControlEngineIntf, JvDBLogonDialogBaseDevart;

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
type

  TJvOdacOracleConnectionInfo = class(TJvBaseOracleConnectionInfo)
  private
    FNet: Boolean;
    FOracleHome: string;
  public
    constructor Create(AOwner: TComponent); override;
    function ConnectString: string; override;
  published
    property Net: Boolean read FNet write FNet default false;
    property OracleHome: string read FOracleHome write FOracleHome;
  end;

  TJvOdacOracleConnectionList = class(TJvBaseOracleConnectionList)
  protected
    function CreateObject: TPersistent; override;
    function GetConnection(I: Longint): TJvOdacOracleConnectionInfo;
  public
    property Connection[I: Longint]: TJvOdacOracleConnectionInfo read GetConnection;
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
    function GetConnectionList: TJvOdacOracleConnectionList;
    function GetOptions: TJvDBOdacLogonDialogOptions;
    function GetOraSession: TOraSession;
    procedure SetOptions(const Value: TJvDBOdacLogonDialogOptions);
  protected
    procedure CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl: TWinControl); override;
    procedure CreateFormControls(AForm: TForm); override;
    function CreatePasswordChangeDialog: TJvBaseDBPasswordDialog; override;
    procedure FillAllComoboBoxes; override;
    procedure FillDatabaseComboBoxValues(Items: TStrings); override;
    procedure FillOracleHomeComboBox;
    procedure FreeFormControls; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonConnectionListClass: TJvBaseConnectionListClass; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass; override;
    procedure HandleExpiredPassword(const ErrorMessage: string);
    procedure SetEditPanelsVisibility; override;
    procedure SetSession(const Value: TComponent); override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); override;
    property ConnectionList: TJvOdacOracleConnectionList read GetConnectionList;
    property OraSession: TOraSession read GetOraSession;
  public
    procedure ClearControlInterfaceObjects; override;
    procedure ConnectSession; override;
    procedure DisconnectSession; override;
    function SessionIsConnected: Boolean; override;
  published
    property Options: TJvDBOdacLogonDialogOptions read GetOptions write SetOptions;
  end;

  TJvDBOdacConnectDialog = class(TJvDBBaseDevartConnectDialog)
  private
    FOnFillDatabaseList: TJvLogonDialogFillListEvent;
    function GetLogonDialogInternal: TJvDBOdacLogonDialog; reintroduce; virtual;
  protected
    function CreateLogonDialogInternal: TJvBaseDBLogonDialog; override;
    function GetOptions: TJvDBOdacLogonDialogOptions; reintroduce; virtual;
    procedure SetOptions(const Value: TJvDBOdacLogonDialogOptions); reintroduce; virtual;
    property LogonDialogInternal: TJvDBOdacLogonDialog read GetLogonDialogInternal;
  published
    procedure InternalFillDatabaseList(List: TStringList);
    property Options: TJvDBOdacLogonDialogOptions read GetOptions write SetOptions;
    //1 Event for filling the database list
    property OnFillDatabaseList: TJvLogonDialogFillListEvent read FOnFillDatabaseList write FOnFillDatabaseList;
  end;
{$ENDIF USE_3RDPARTY_DEVART_ODAC}

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

{$DEFINE ODAC_V10}

{$IFDEF USE_3RDPARTY_DEVART_ODAC}
uses
  SysUtils, StdCtrls, Dialogs,
  OraClasses, OraError, OraCall, OraServices,
  {$IFDEF ODAC_V10}OraServerEnumerator, {$ENDIF ODAC_V10}JvDSADialogs, JvDBPasswordDialogOdac, JvResources;

//=== { TJvDBOdacLogonDialogOptions } ========================================

constructor TJvDBOdacLogonDialogOptions.Create;
begin
  inherited Create;
  FShowNetOption := True;
  AllowPasswordChange := True;
  FShowOracleHome := False;
end;

function TJvDBOdacConnectDialog.CreateLogonDialogInternal: TJvBaseDBLogonDialog;
begin
  Result := TJvDBOdacLogonDialog.Create(Self);
  TJvDBOdacLogonDialog(Result).OnFillDatabaseList := InternalFillDatabaseList;
end;

function TJvDBOdacConnectDialog.GetLogonDialogInternal: TJvDBOdacLogonDialog;
begin
  Result := TJvDBOdacLogonDialog(inherited GetLogonDialogInternal);
end;

function TJvDBOdacConnectDialog.GetOptions: TJvDBOdacLogonDialogOptions;
begin
  Result := LogonDialogInternal.Options;
end;

procedure TJvDBOdacConnectDialog.InternalFillDatabaseList(List: TStringList);
begin
  GetServerList(List);
  if Assigned(OnFillDatabaseList) then
    OnFillDatabaseList(List);
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
    OraSession.DisConnect;
    OraSession.PerformConnect;
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

procedure TJvDBOdacLogonDialog.DisconnectSession;
begin
  if Assigned(Session) then
    OraSession.DisConnect;
end;

procedure TJvDBOdacLogonDialog.CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl: TWinControl);
var
  IDynControlComboBox: IJvDynControlComboBox;
begin
  inherited CreateAdditionalConnectDialogControls (AOwner, AParentControl);
  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'OracleHome', RsOracleHome, jctComboBox, OracleHomePanel, OracleHomeEdit, IOracleHomeEditData, DefaultOnEditChange);
  if Supports(OracleHomeEdit, IJvDynControlComboBox, IDynControlComboBox) then
    IDynControlComboBox.ControlSetNewEntriesAllowed(False);

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

procedure TJvDBOdacLogonDialog.FillAllComoboBoxes;
begin
  inherited FillAllComoboBoxes;
  FillOracleHomeComboBox;
end;

procedure TJvDBOdacLogonDialog.FillDatabaseComboBoxValues(Items: TStrings);
var
  Enum: TOraServerEnumerator;
  List: TStringList;
begin
  Inherited FillDatabaseComboBoxValues(Items);
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

procedure TJvDBOdacLogonDialog.FillOracleHomeComboBox;
var
  Items: TStringList;
  IDynControlItems: IJvDynControlItems;
  i : Integer;
begin
  Items := TStringList.Create;
  try
    for i := 0 to OracleHomes.Count - 1 do
      Items.Add(OracleHomes[i].Name);
    if Supports(OracleHomeEdit, IJvDynControlItems, IDynControlItems) then
      IDynControlItems.ControlItems.Assign(Items);
  finally
    Items.Free;
  end;
end;

procedure TJvDBOdacLogonDialog.FreeFormControls;
begin
  INetOptionCheckBox:= Nil;
  IOracleHomeEditData:= Nil;
  inherited;
end;

function TJvDBOdacLogonDialog.GetConnectionList: TJvOdacOracleConnectionList;
begin
  Result := TJvOdacOracleConnectionList(Inherited ConnectionList);
end;

class function TJvDBOdacLogonDialog.GetDBLogonConnectionListClass: TJvBaseConnectionListClass;
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

procedure TJvDBOdacLogonDialog.HandleExpiredPassword(const ErrorMessage: string);
begin
  if JvDSADialogs.MessageDlg(ErrorMessage + #13#10 + RsDoYouWantToChangePassword,
        mtInformation, [mbYes, mbNo], 0, dckScreen, 0, mbDefault, mbDefault, mbDefault, DynControlEngine) = mrYes then
    if ChangePassword then
      if not SessionIsConnected then
        OraSession.PerformConnect;
end;

function TJvDBOdacLogonDialog.SessionIsConnected: Boolean;
begin
  Result := OraSession.Connected;
end;

procedure TJvDBOdacLogonDialog.SetEditPanelsVisibility;
begin
  Inherited SetEditPanelsVisibility;
  SetPanelVisible(OracleHomePanel, Options.ShowOracleHome);
  NetOptionCheckBox.Visible := Options.ShowNetOption;
end;

procedure TJvDBOdacLogonDialog.SetOptions(const Value: TJvDBOdacLogonDialogOptions);
begin
  (inherited Options).Assign(Value);
end;

procedure TJvDBOdacLogonDialog.SetSession(const Value: TComponent);
begin
  inherited SetSession(Value);
  if Value is TOraSession then
    FOraSession := TOraSession(Value)
  else
    FOraSession := nil;
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
    OraSession.Server := ConnectionInfo.Database;
    OraSession.Password := ConnectionInfo.Password;
    OraSession.Username := ConnectionInfo.Username;
    if Assigned(OraSession) and (ConnectionInfo is TJvBaseOracleConnectionInfo) then
    begin
      if TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSDBA' then
        OraSession.ConnectMode := cmSYSDBA
      else
        if TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs = 'SYSASM' then
          OraSession.ConnectMode := cmSYSASM
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
    ConnectionInfo.Database := OraSession.Server;
    ConnectionInfo.Password := OraSession.Password;
    ConnectionInfo.Username := OraSession.Username;
    if Assigned(OraSession) and (ConnectionInfo is TJvBaseOracleConnectionInfo) then
    begin
      case OraSession.ConnectMode of
        cmSYSDBA:
          TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSDBA';
        cmSYSASM:
          TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := 'SYSASM';
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

function TJvOdacOracleConnectionList.GetConnection(I: Longint): TJvOdacOracleConnectionInfo;
begin
  Result := TJvOdacOracleConnectionInfo(inherited Connection[i])
end;

constructor TJvOdacOracleConnectionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNet := false;
end;

function TJvOdacOracleConnectionInfo.ConnectString: string;
begin
  Result := inherited ConnectString;
  if OracleHome <> '' then
    Result:= Result + ' - '+OracleHome;
  if Net then
    Result := Result + ' - '+RsNetOptionConnectionList;
end;
{$ENDIF USE_3RDPARTY_DEVART_ODAC}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
