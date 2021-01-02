{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBaseDBLogonDialog.pas, released on 2006-07-21

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id: jvcl/run/JvBaseDBLogonDialog.pas Jens Fudickar date $

unit JvBaseDBLogonDialog;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Forms, Controls, Menus,
  JvAppStorage, JvDynControlEngine, JvDynControlEngineIntf,
  JvPropertyStore, JvBaseDBDialog, JvBaseDBPasswordDialog, Graphics,
  ExtCtrls;

type
  TJvLogonDialogFillListEvent = procedure(List: TStringList) of object;
  TJvLogonDialogEncryptDecryptEvent = procedure(var Password: string) of object;
  TJvLogonDialogBaseSessionEvent = function(Session: TComponent): Boolean of object;

  TJvDBLogonDialogActivePage = (ldapConnectList, ldapUserTree, ldapDatabaseTree, ldapGroupTree);

  TJvBaseDBLogonDialogOptions = class(TPersistent)
  private
    FAddConnectionValuesToComboBox: Boolean;
    FAllowNullPasswords: Boolean;
    FAllowPasswordChange: Boolean;
    FDatabasenameCaseSensitive: Boolean;
    FPasswordChar: char;
    FPasswordDialogOptions: TJvBaseDBPasswordDialogOptions;
    FUsernameCaseSensitive: Boolean;
    FSaveLastConnect: Boolean;
    FSavePasswords: Boolean;
    FSetLastConnectToTop: Boolean;
    FShowAlias: Boolean;
    FShowColors: Boolean;
    FShowConnectGroup: Boolean;
    FShowConnectionsExport: Boolean;
    FShowConnectionFilter: Boolean;
    FShowSavePasswords: Boolean;
    FShowShortcuts: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property AllowPasswordChange: Boolean read FAllowPasswordChange write FAllowPasswordChange;
    property PasswordDialogOptions: TJvBaseDBPasswordDialogOptions read FPasswordDialogOptions;
  published
    //1 Add each of the values from the connection list to the different comboboxes
    property AddConnectionValuesToComboBox: Boolean read FAddConnectionValuesToComboBox write
        FAddConnectionValuesToComboBox default true;
    property AllowNullPasswords: Boolean read FAllowNullPasswords write FAllowNullPasswords default False;
    //1 Group the Databasename casesensitive in the Databasename tree list
    property DatabasenameCaseSensitive: Boolean read FDatabasenameCaseSensitive write FDatabasenameCaseSensitive default
        False;
    property PasswordChar: char read FPasswordChar write FPasswordChar default '*';
    //1 Group the username casesensitive in the username tree list
    property UsernameCaseSensitive: Boolean read FUsernameCaseSensitive write FUsernameCaseSensitive default False;
    property SaveLastConnect: Boolean read FSaveLastConnect write FSaveLastConnect default True;
    property SavePasswords: Boolean read FSavePasswords write FSavePasswords default True;
    property SetLastConnectToTop: Boolean read FSetLastConnectToTop write FSetLastConnectToTop default True;
    property ShowAlias: Boolean read FShowAlias write FShowAlias default False;
    property ShowColors: Boolean read FShowColors write FShowColors default False;
    property ShowConnectGroup: Boolean read FShowConnectGroup write FShowConnectGroup default True;
    property ShowConnectionsExport: Boolean read FShowConnectionsExport write FShowConnectionsExport default True;
    property ShowConnectionFilter: Boolean read FShowConnectionFilter write FShowConnectionFilter default true;
    property ShowSavePasswords: Boolean read FShowSavePasswords write FShowSavePasswords default False;
    property ShowShortcuts: Boolean read FShowShortcuts write FShowShortcuts default True;
  end;

  TJvBaseDBOracleLogonDialogOptions = class(TJvBaseDBLogonDialogOptions)
  private
    FShowConnectAs: Boolean;
  public
    constructor Create; override;
  published
    property ShowConnectAs: Boolean read FShowConnectAs write FShowConnectAs default True;
  end;

  TJvBaseDBLogonDialogOptionsClass = class of TJvBaseDBLogonDialogOptions;

  TJvBaseConnectionInfo = class(TJvCustomPropertyStore)
  private
    FAlias: String;
    FColor: TColor;
    FDatabase: string;
    FGroup: string;
    FPassword: string;
    FSavePassword: Boolean;
    FShortCut: Integer;
    FUsername: string;
    function GetShortCutText: string;
    procedure SetGroup(const Value: string);
    procedure SetSavePassword(const Value: Boolean);
    procedure SetShortCutText(const Value: string);
  protected
    function GetDatabaseEnabled: Boolean; virtual;
    function GetAliasEnabled: Boolean; virtual;
    function GetUsernameEnabled: Boolean; virtual;
    function GetPasswordEnabled: Boolean; virtual;
    //1 This function is to identify the connection info in the connection list
    function SearchName: String; virtual;
    procedure SetDatabase(Value: string);
    procedure SetUsername(Value: string);
    function TranslateUserName(iName: string): string; virtual;
    function TranslateDatabaseName(iName: string): string; virtual;
    function UseTranslateUserName: Boolean; virtual;
    function UseTranslateDatabaseName: Boolean; virtual;
    property DatabaseEnabled: Boolean read GetDatabaseEnabled;
    property AliasEnabled: Boolean read GetAliasEnabled;
    property UsernameEnabled: Boolean read GetUsernameEnabled;
    property PasswordEnabled: Boolean read GetPasswordEnabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsConnectAllowed(AllowNullPasswords: Boolean): Boolean; virtual;
    function ConnectString: string; virtual;
    function DatabaseGroupIdentifier: string; virtual;
    function MatchesFilter(iFilterValue: String): Boolean; virtual;
    property SavePassword: Boolean read FSavePassword write SetSavePassword;
    property ShortCut: Integer read FShortCut write FShortCut;
  published
    property Alias: String read FAlias write FAlias;
    property Color: TColor read FColor write FColor;
    property Database: string read FDatabase write SetDatabase;
    property Group: string read FGroup write SetGroup;
    property Password: string read FPassword write FPassword;
    property ShortCutText: string read GetShortCutText write SetShortCutText;
    property Username: string read FUsername write SetUsername;
  end;

  TJvBaseOracleConnectionInfo = class(TJvBaseConnectionInfo)
  private
    FConnectAs: string;
    procedure SetConnectAs(const Value: string);
  protected
    function GetConnectAsEnabled: Boolean; virtual;
    property ConnectAsEnabled: Boolean read GetConnectAsEnabled;
  public
    constructor Create(AOwner: TComponent); override;
    function ConnectString: string; override;
  published
    property ConnectAs: string read FConnectAs write SetConnectAs;
  end;

  TJvLogonDialogConnectionInfoEvent = procedure(ConnectionInfo: TJvBaseConnectionInfo) of object;

  TJvBaseConnectionListClass = class of TJvBaseConnectionList;

  TJvBaseConnectionList = class(TJvCustomPropertyListStore)
  private
    FActivePage: TJvDBLogonDialogActivePage;
    FGroupByDatabase: Boolean;
    FGroupByUser: Boolean;
    FLastConnect: TJvBaseConnectionInfo;
    FSavePasswords: Boolean;
    procedure SetLastConnect(const Value: TJvBaseConnectionInfo);
    procedure SetSavePasswords(const Value: Boolean);
  protected
    procedure CopyContents(iConnectionList: TJvBaseConnectionList; iClearBefore: Boolean);
    function CreateObject: TPersistent; override;
    function GetConnection(I: Longint): TJvBaseConnectionInfo;
    procedure LoadData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddConnection(ConnectionInfo: TJvBaseConnectionInfo);
    function CreateConnection: TJvBaseConnectionInfo;
    property Connection[I: Longint]: TJvBaseConnectionInfo read GetConnection;
  published
    //1 Stores the data of the last connection
    property LastConnect: TJvBaseConnectionInfo read FLastConnect write SetLastConnect;
    property ActivePage: TJvDBLogonDialogActivePage read FActivePage write FActivePage;
    property GroupByDatabase: Boolean read FGroupByDatabase write FGroupByDatabase;
    property GroupByUser: Boolean read FGroupByUser write FGroupByUser;
    property SavePasswords: Boolean read FSavePasswords write SetSavePasswords;
  end;

  TJvBaseOracleConnectionList = class(TJvBaseConnectionList)
  protected
    function CreateObject: TPersistent; override;
  end;

  TJvBaseDBLogonDialog = class(TJvBaseDBDialog)
  private
    AdditionalBtn: TWinControl;
    AdditionalPopupMenu: TPopupMenu;
    AddToListBtn: TWinControl;
    AliasPanel: TWinControl;
    ButtonPanel: TWinControl;
    CancelBtn: TWinControl;
    ColorBoxPanel: TWinControl;
    ConnectBtn: TWinControl;
    ConnectGroupPanel: TWinControl;
    ConnectionFilterTimer: TTimer;
    ConnectListListBox: TWinControl;
    DatabaseComboBox: TWinControl;
    DatabasePanel: TWinControl;
    DatabaseTreeView: TWinControl;
    EditConnectionPanel: TWinControl;
    FAfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent;
    FBeforeTransferConnectionInfoToSessionData: TJvLogonDialogConnectionInfoEvent;
    FConnectionFilterEdit: TWincontrol;
    IConnectionFilterEditData: IJvDynControlData;
    FConnectionList: TJvBaseConnectionList;
    FCurrentConnectionInfo: TJvBaseConnectionInfo;
    FGroupByDatabase: Boolean;
    fGroupByUser: Boolean;
    FOnDecryptPassword: TJvLogonDialogEncryptDecryptEvent;
    FOnEncryptPassword: TJvLogonDialogEncryptDecryptEvent;
    FOnFillDatabaseList: TJvLogonDialogFillListEvent;
    FOnFillShortcutList: TJvLogonDialogFillListEvent;
    FOnSessionConnect: TJvLogonDialogBaseSessionEvent;
    FOnSessionDisconnect: TJvLogonDialogBaseSessionEvent;
    FOptions: TJvBaseDBLogonDialogOptions;
    GetFromListBtn: TWinControl;
    GroupByDatabaseCheckBox: TWinControl;
    GroupByUserCheckBox: TWinControl;
    GroupTreeView: TWinControl;
    IAliasEditData: IJvDynControlData;
    IColorComboBox: IJvDynControlColorComboBoxControl;
    IConnectGroupComboBoxData: IJvDynControlData;
    IConnectGroupComboBoxItems: IJvDynControlItems;
    IConnectionListPageControlTab: IJvDynControlTabControl;
    IConnectListListBoxData: IJvDynControlData;
    IConnectListListBoxItems: IJvDynControlItems;
    IDatabaseComboBoxData: IJvDynControlData;
    IDatabaseTreeView: IJvDynControlTreeView;
    IGroupByDatabaseCheckBox: IJvDynControlCheckBox;
    IGroupByUserCheckBox: IJvDynControlCheckBox;
    IGroupTreeView: IJvDynControlTreeView;
    IPasswordEditData: IJvDynControlData;
    ISavePasswordsCheckBox: IJvDynControlCheckBox;
    IShortCutComboBoxData: IJvDynControlData;
    IUserNameEditData: IJvDynControlData;
    IUserTreeView: IJvDynControlTreeView;
    LeftBottomPanel: TWinControl;
    LeftPanel: TWinControl;
    PasswordEdit: TWinControl;
    PasswordPanel: TWinControl;
    RemoveFromListBtn: TWinControl;
    SavePasswordsCheckBox: TWinControl;
    ShortCutPanel: TWinControl;
    UserNameEdit: TWinControl;
    UserNamePanel: TWinControl;
    UserTreeView: TWinControl;
    ConnectionListPageControl: TWinControl;
    procedure AdditionalBtnClick(Sender: TObject);
    procedure AddToListBtnClick(Sender: TObject);
    function CalculatePanelHeight(iPanel: TWinControl): Integer;
    procedure CancelBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure ConnectionListPageControlChange(Sender: TObject);
    procedure ConnectListListBoxClick(Sender: TObject);
    procedure ConnectListListBoxDblClick(Sender: TObject);
    procedure ConnectionFilterEditChange(Sender: TObject);
    procedure ConnectionFilterTimerTimer(Sender: TObject);
    function DecryptPassword(const Value: string): string;
    function EncryptPassword(const Value: string): string;
    procedure FillAllConnectionLists;
    procedure FillConnectGroupComboBox;
    procedure FillConnectionList;
    procedure FillDatabaseTreeView;
    procedure FillGroupTreeView;
    procedure FillUserTreeView;
    procedure FillShortCutList(Items: TStringList);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function GetActivePage: TJvDBLogonDialogActivePage;
    function GetConnectionFilterValue: string;
    function GetCurrentDialogListConnectionInfo: TJvBaseConnectionInfo;
    function GetDialogDatabase: string;
    function GetDialogPassword: string;
    function GetDialogUserName: string;
    procedure GetFromListBtnClick(Sender: TObject);
    procedure GroupByDatabaseCheckBoxClick(Sender: TObject);
    procedure GroupByUserCheckBoxClick(Sender: TObject);
    function ListConnectString(Connection: TJvBaseConnectionInfo; ShowShortCut, ShowConnectGroup: Boolean): string;
    procedure LoadSettings;
    procedure PasswordDialog_AfterTransferPasswordFromSession(var Password: string);
    procedure PasswordDialog_BeforeTransferPasswordToSession(var Password: string);
    procedure RearrangePanelControlsByTaborder(iPanel: TWinControl);
    procedure RemoveFromListBtnClick(Sender: TObject);
    procedure ResizeAllControls;
    procedure SetActivePage(const Value: TJvDBLogonDialogActivePage);
    procedure SetButtonState;
    procedure SetConnectionToTop(const SearchName: string);
    procedure SetDialogDatabase(const Value: string);
    procedure SetDialogPassword(const Value: string);
    procedure SetDialogUserName(const Value: string);
    procedure SetOptions(const Value: TJvBaseDBLogonDialogOptions);
    procedure StoreSettings;
    property ConnectionFilterValue: string read GetConnectionFilterValue;
  protected
    procedure ActivateDatabaseControl;
    procedure ActivatePasswordControl;
    procedure AlignControlTop(aControl, aPreviousControl: TControl);
    function ChangePassword: Boolean;
    procedure ClearControlInterfaceObjects; virtual;
    procedure ClearFormControls; virtual;
    procedure ConnectToSession;
    procedure CreateAdditionalConnectDialogControls(AOwner: TComponent;
      AParentControl: TWinControl); virtual;
    procedure CreateAdditionalConnectDialogEditPanel(AOwner: TComponent; AParentControl: TWinControl; const
        ControlBaseName, Caption: string; AControlType: TJvDynControlType; var oPanel, oEditControl: TWinControl; var
        oEditData: IJvDynControlData; onEditChange: TNotifyEvent = nil);
    procedure CreateFormControls(AForm: TForm); override;
    function CreatePasswordChangeDialog: TJvBaseDBPasswordDialog; virtual;
    procedure DefaultOnEditChange(Sender: TObject);
    procedure DoSessionConnect;
    procedure DoSessionDisconnect;
    procedure FillAdditionalPopupMenuEntries(APopupMenu: TPopupMenu); virtual;
    procedure FillAllComoboBoxes; virtual;
    procedure FillDatabaseComboBox;
    procedure FillDatabaseComboBoxValues(Items: TStrings); virtual;
    procedure FreeFormControls; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonConnectionListClass: TJvBaseConnectionListClass; virtual;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass; virtual;
    function GetGroupByDatabase: Boolean;
    function GetGroupByUser: Boolean;
    procedure OnExportConnectionList(Sender: TObject);
    procedure OnImportConnectionList(Sender: TObject);
    procedure RearrangeEditPanel;
    procedure RearrangeEditPanelControlsByTaborder;
    procedure ResizeDialogClientHeight;
    procedure ResizeFormControls; virtual;
    function SavePasswords: Boolean;
    procedure SetAppStorage(Value: TJvCustomAppStorage); override;
    procedure SetAppStoragePath(Value: string); override;
    procedure SetEditPanelsTabOrder; virtual;
    procedure SetEditPanelsVisibility; virtual;
    procedure SetGroupByDatabase(Value: Boolean);
    procedure SetGroupByUser(Value: Boolean);
    procedure SetPanelHeight(iPanel: TWinControl);
    procedure SetPanelVisible(iPanel: TWinControl; iVisible: Boolean);
    procedure SetSession(const Value: TComponent); override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo); virtual;
    procedure TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo); virtual;
    procedure TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); virtual;
    procedure TransferSessionDataFromDialog;
    procedure TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo); virtual;
    procedure TransferSessionDataToDialog;
    procedure ValidateConnectBtnEnabled;
    property ActivePage: TJvDBLogonDialogActivePage read GetActivePage write SetActivePage;
    property ConnectionList: TJvBaseConnectionList read FConnectionList;
    property CurrentDialogListConnectionInfo: TJvBaseConnectionInfo read GetCurrentDialogListConnectionInfo;
    property DialogDatabase: string read GetDialogDatabase write SetDialogDatabase;
    property DialogPassword: string read GetDialogPassword write SetDialogPassword;
    property DialogUserName: string read GetDialogUserName write SetDialogUserName;
    property GroupByDatabase: Boolean read GetGroupByDatabase write SetGroupByDatabase;
    property GroupByUser: Boolean read GetGroupByUser write SetGroupByUser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ConnectSession; virtual;
    procedure DisconnectSession; virtual;
    function IsConnectAllowed: Boolean; virtual;
    property CurrentConnectionInfo: TJvBaseConnectionInfo read FCurrentConnectionInfo;
  published
    property AppStorage;
    property AppStoragePath;
    property Options: TJvBaseDBLogonDialogOptions read FOptions write SetOptions;
    //1 This events gives you the possibility to modify the connection data after receiving the data from the current session
    property AfterTransferSessionDataToConnectionInfo: TJvLogonDialogConnectionInfoEvent read
      FAfterTransferSessionDataToConnectionInfo write FAfterTransferSessionDataToConnectionInfo;
    //1 This Event gives you the possibility to modify the connection data before it is transfered to the current session
    property BeforeTransferConnectionInfoToSessionData:
      TJvLogonDialogConnectionInfoEvent read
      FBeforeTransferConnectionInfoToSessionData write
      FBeforeTransferConnectionInfoToSessionData;
    property OnDecryptPassword: TJvLogonDialogEncryptDecryptEvent read FOnDecryptPassword write FOnDecryptPassword;
    property OnEncryptPassword: TJvLogonDialogEncryptDecryptEvent read FOnEncryptPassword write FOnEncryptPassword;
    //1 Event for filling the database list
    property OnFillDatabaseList: TJvLogonDialogFillListEvent read FOnFillDatabaseList write FOnFillDatabaseList;
    //1 Event for customizing the shortcut list
    property OnFillShortcutList: TJvLogonDialogFillListEvent read FOnFillShortcutList write FOnFillShortcutList;
    property OnSessionConnect: TJvLogonDialogBaseSessionEvent read FOnSessionConnect write FOnSessionConnect;
    property OnSessionDisconnect: TJvLogonDialogBaseSessionEvent read FOnSessionDisconnect write FOnSessionDisconnect;
  end;

  TJvBaseDBOracleLogonDialog = class(TJvBaseDBLogonDialog)
  private
    ConnectAsComboBox: TWinControl;
    ConnectAsPanel: TWinControl;
    IConnectAsComboBoxData: IJvDynControlData;
    function GetCurrentConnectionInfo: TJvBaseOracleConnectionInfo;
    function GetDialogConnectAs: string;
    function GetOptions: TJvBaseDBOracleLogonDialogOptions;
    procedure SetDialogConnectAs(const Value: string);
    procedure SetOptions(const Value: TJvBaseDBOracleLogonDialogOptions);
  protected
    procedure ClearFormControls; override;
    procedure CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl: TWinControl); override;
    procedure CreateFormControls(AForm: TForm); override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonConnectionListClass: TJvBaseConnectionListClass; override;
    { Retrieve the class that holds the storage options and format settings. }
    class function GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass; override;
    procedure SetEditPanelsTabOrder; override;
    procedure SetEditPanelsVisibility; override;
    procedure TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    procedure TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo); override;
    property DialogConnectAs: string read GetDialogConnectAs write SetDialogConnectAs;
  public
    procedure ClearControlInterfaceObjects; override;
    property CurrentConnectionInfo: TJvBaseOracleConnectionInfo read GetCurrentConnectionInfo;
  published
    property Options: TJvBaseDBOracleLogonDialogOptions read GetOptions write SetOptions;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: jvcl/run/JvBaseDBLogonDialog.pas $';
    Revision: '$Revision: 452e37996b3821b0e7ca9082f2fe381bf5c65e15 $';
    Date: '$Date: 2014-09-21 21:26:51 +0200 $';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

const
  cDefaultColorComboBoxColor = clWindow;

implementation

uses
  Windows, SysUtils, Types, ComCtrls, StdCtrls, Dialogs,
  {$IFDEF HAS_UNIT_CHARACTER}
  Character,
  {$ENDIF HAS_UNIT_CHARACTER}
  JvJCLUtils,
  JvAppIniStorage, JvAppXMLStorage, JvDSADialogs, JvResources,
  Variants, StrUtils;


//=== { TJvBaseDBLogonDialog } ===============================================

constructor TJvBaseDBLogonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := GetDBLogonDialogOptionsClass.Create;
  FConnectionList := GetDBLogonConnectionListClass.Create(Self);
  FCurrentConnectionInfo := FConnectionList.CreateConnection;
end;

destructor TJvBaseDBLogonDialog.Destroy;
begin
  FreeAndNil(FCurrentConnectionInfo);
  FreeAndNil(FConnectionList);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJvBaseDBLogonDialog.ActivateDatabaseControl;
begin
  if Assigned(DatabaseComboBox) and Assigned(DBDialog) then
    DBDialog.ActiveControl := DatabaseComboBox;
end;

procedure TJvBaseDBLogonDialog.ActivatePasswordControl;
begin
  if Assigned(DatabaseComboBox) and Assigned(DBDialog) then
    DBDialog.ActiveControl := PasswordEdit;
end;

procedure TJvBaseDBLogonDialog.AdditionalBtnClick(Sender: TObject);
var
  P: TPoint;
begin
  P := AdditionalBtn.Parent.ClientToScreen(Point(AdditionalBtn.Left + AdditionalBtn.Width, AdditionalBtn.Top));
  AdditionalPopupMenu.Popup(P.X, P.Y);
end;

procedure TJvBaseDBLogonDialog.AddToListBtnClick(Sender: TObject);
var
  ConnectionInfo: TJvBaseConnectionInfo;
begin
  ConnectionInfo := ConnectionList.CreateConnection;
  TransferConnectionInfoFromDialog(ConnectionInfo);
  ConnectionList.AddConnection(ConnectionInfo);
  IConnectionFilterEditData.ControlValue := '';
  FillAllConnectionLists;
end;

procedure TJvBaseDBLogonDialog.AlignControlTop(aControl, aPreviousControl: TControl);
begin
  aControl.Align := alTop;
  if Assigned(aPreviousControl) then
    aControl.Top := aPreviousControl.Top + aPreviousControl.Height
  else
    aControl.Top := 0;
end;

function TJvBaseDBLogonDialog.CalculatePanelHeight(iPanel: TWinControl):
    Integer;
var
  i: Integer;
  t: Integer;
  h: Integer;
  Found : Boolean;
begin
  t := 99999;
  h := 0;
  Found := False;
  for i := 0 to iPanel.ControlCount - 1 do
  begin
    if not iPanel.Controls[i].Visible then
      Continue;
    Found := True;
    if iPanel.Controls[i].Top < t then
      t := iPanel.Controls[i].Top;
    if iPanel.Controls[i].Top+iPanel.Controls[i].Height > h then
      h := iPanel.Controls[i].Top+iPanel.Controls[i].Height;
  end;
  if found then
    Result := t+h+1
  else
    Result := 0;
end;

procedure TJvBaseDBLogonDialog.CancelBtnClick(Sender: TObject);
begin
  DBDialog.ModalResult := mrCancel;
end;

function TJvBaseDBLogonDialog.ChangePassword: Boolean;
var
  PasswordDialog: TJvBaseDBPasswordDialog;
begin
  Result := False;
  if not Options.AllowPasswordChange then
    Exit;
  PasswordDialog := TJvBaseDBPasswordDialog(CreatePasswordChangeDialog);
  if Assigned(PasswordDialog) then
  try
    PasswordDialog.Session := Session;
    PasswordDialog.AfterTransferPasswordFromSession := PasswordDialog_AfterTransferPasswordFromSession;
    PasswordDialog.BeforeTransferPasswordToSession := PasswordDialog_BeforeTransferPasswordToSession;
    PasswordDialog.Options := Options.PasswordDialogOptions;
    Result := PasswordDialog.Execute;
  finally
    PasswordDialog.Free;
  end;
end;

procedure TJvBaseDBLogonDialog.ClearControlInterfaceObjects;
begin
  IAliasEditData:= nil;
  IColorComboBox:= nil;
  IConnectGroupComboBoxData:= nil;
  IConnectGroupComboBoxItems:= nil;
  IConnectionListPageControlTab:= nil;
  IConnectListListBoxData:= nil;
  IConnectListListBoxItems:= nil;
  IDatabaseComboBoxData:= nil;
  IDatabaseTreeView:= nil;
  IGroupByDatabaseCheckBox:= nil;
  IGroupByUserCheckBox:= nil;
  IGroupTreeView:= nil;
  IPasswordEditData:= nil;
  ISavePasswordsCheckBox:= nil;
  IShortCutComboBoxData:= nil;
  IUserNameEditData:= nil;
  IUserTreeView:= nil;
  IConnectionFilterEditData := nil;
end;

procedure TJvBaseDBLogonDialog.ClearFormControls;
begin
  if Assigned(IUserNameEditData)  then
    IUserNameEditData.ControlValue := '';
  if Assigned(IPasswordEditData)  then
    IPasswordEditData.ControlValue := '';
  if Assigned(IDatabaseComboBoxData)  then
    IDatabaseComboBoxData.ControlValue := '';
  if Assigned(IConnectGroupComboBoxData)  then
    IConnectGroupComboBoxData.ControlValue := '';
  if Assigned(IShortCutComboBoxData)  then
    IShortCutComboBoxData.ControlValue := '';
  if Assigned(IColorComboBox)  then
    IColorComboBox.ControlSelectedColor  := cDefaultColorComboBoxColor;
  if Assigned(IConnectionFilterEditData)  then
    IConnectionFilterEditData.ControlValue  := '';

end;

procedure TJvBaseDBLogonDialog.ConnectBtnClick(Sender: TObject);
begin
  if not ConnectBtn.Enabled then
    Exit;
  ConnectToSession;
end;

procedure TJvBaseDBLogonDialog.ConnectionFilterEditChange(Sender: TObject);
begin
  ConnectionFilterTimer.Enabled := False;
  ConnectionFilterTimer.Enabled := True;
end;

procedure TJvBaseDBLogonDialog.ConnectionFilterTimerTimer(Sender: TObject);
begin
  ConnectionFilterTimer.Enabled := False;
  FillAllConnectionLists;
end;

procedure TJvBaseDBLogonDialog.ConnectionListPageControlChange(Sender: TObject);
begin
  SetButtonState;
end;

procedure TJvBaseDBLogonDialog.ConnectListListBoxClick(Sender: TObject);
begin
  SetButtonState;
end;

procedure TJvBaseDBLogonDialog.ConnectListListBoxDblClick(Sender: TObject);
begin
  if Assigned(CurrentDialogListConnectionInfo) then
  begin
    TransferConnectionInfoToDialog(CurrentDialogListConnectionInfo);
    ConnectToSession;
  end;
end;

procedure TJvBaseDBLogonDialog.ConnectSession;
begin
end;

procedure TJvBaseDBLogonDialog.DisconnectSession;
begin
end;

procedure TJvBaseDBLogonDialog.ConnectToSession;
begin
  ValidateConnectBtnEnabled;
  if ConnectBtn.Enabled then
  begin
    DoSessionDisconnect;
    DoSessionConnect;
  end
  else
    if DialogPassword = '' then
      PasswordEdit.SetFocus;
end;

procedure TJvBaseDBLogonDialog.CreateAdditionalConnectDialogControls(AOwner: TComponent;
  AParentControl: TWinControl);
begin
end;

procedure TJvBaseDBLogonDialog.CreateAdditionalConnectDialogEditPanel(AOwner: TComponent; AParentControl: TWinControl;
    const ControlBaseName, Caption: string; AControlType: TJvDynControlType; var oPanel, oEditControl: TWinControl; var
    oEditData: IJvDynControlData; onEditChange: TNotifyEvent = nil);
var
  LabelControl: TControl;
  IDynControlLabel: IJvDynControlLabel;
  IDynControlAutoSize: IJvDynControlAutoSize;
  IDynControl: IJvDynControl;
begin
  oPanel := DynControlEngine.CreatePanelControl(AOwner, AParentControl, ControlBaseName+'Panel', '', alTop);
  AlignControlTop(oPanel, nil);
  LabelControl := DynControlEngine.CreateLabelControl(AOwner, oPanel, ControlBaseName+'Label', Caption, nil);
  AlignControlTop(LabelControl, nil);
  oEditControl := TWinControl(DynControlEngine.CreateControl(AControlType, AOwner, oPanel, ControlBaseName+'Edit'));
  Supports(oEditControl, IJvDynControlData, oEditData);
  oEditData.ControlValue := '';
  oEditData.ControlSetOnChange(onEditChange);
  AlignControlTop(oEditControl, LabelControl);
  if Supports(oEditControl, IJvDynControl, IDynControl) then
    IDynControl.ControlSetOnClick(onEditChange);
  if Supports(LabelControl, IJvDynControlLabel, IDynControlLabel) then
    IDynControlLabel.ControlSetFocusControl(oEditControl);
  if Supports(LabelControl, IJvDynControlAutoSize,IDynControlAutoSize) then
    IDynControlAutoSize.ControlSetAutoSize(True);
  SetPanelHeight(oPanel);
end;

procedure TJvBaseDBLogonDialog.CreateFormControls(AForm: TForm);
var
  MainPanel, ListPanel, ListBtnPanel, GroupListPanel: TWinControl;
  AliasEdit: TWinControl;
  ShortCutComboBox: TWinControl;
  ConnectGroupComboBox: TWinControl;
  ColorComboBox: TWinControl;
  Items: TStringList;
  ITabControl: IJvDynControlTabControl;
  IDynControl: IJvDynControl;
  IDynControlAutoSize: IJvDynControlAutoSize;
  IDynControlDblClick: IJvDynControlDblClick;
  IDynControlReadOnly: IJvDynControlReadOnly;
  IDynControlPageControl: IJvDynControlPageControl;
  IDynControlBevelBorder: IJvDynControlBevelBorder;
  IDynControlComboBox: IJvDynControlComboBox;
  IDynControlEdit: IJvDynControlEdit;
  LabelControl: TControl;
  IDynControlLabel: IJvDynControlLabel;
  ConnectListLabel: TWinControl;
  IDynControlItems: IJvDynControlItems;
begin
  AForm.Name := 'DBDialog';
//  AForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize, biHelp];
  AForm.BorderStyle := bsDialog;
  AForm.Caption := RsLogonToDatabase;
  AForm.ClientHeight := 440;
  AForm.ClientWidth := 680;
  {$IFDEF COMPILER7_UP}
  aForm.Position := poOwnerFormCenter;
  {$ELSE}
  aForm.Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  AForm.KeyPreview := True;
  AForm.OnClose := FormClose;
  AForm.OnKeyDown := FormKeyDown;
  AForm.OnShow := FormShow;

  ButtonPanel := DynControlEngine.CreatePanelControl(AForm, AForm, 'ButtonPanel', '', alBottom);
  ConnectBtn := DynControlEngine.CreateButton(AForm, ButtonPanel, 'ConnectBtn', RsBtnConnect, '', ConnectBtnClick, True, False);
  ConnectBtn.Left := 60;
  ConnectBtn.Top := 3;
  ConnectBtn.Width := 90;
  ConnectBtn.Height := 25;
  CancelBtn := DynControlEngine.CreateButton(AForm, ButtonPanel, 'CancelBtn', RsButtonCancelCaption, '', CancelBtnClick, False, True);
  CancelBtn.Left := 460;
  CancelBtn.Top := ConnectBtn.Top;
  CancelBtn.Width := 90;
  CancelBtn.Height := 25;

  AdditionalBtn := DynControlEngine.CreateButton(AForm, ButtonPanel, 'AdditionalBtn', RsBtnAdditional, '', AdditionalBtnClick, False, False);
  AdditionalBtn.Left := 460;
  AdditionalBtn.Top := ConnectBtn.Top;
  AdditionalBtn.Width := 100;
  AdditionalBtn.Height := 25;

  SetPanelHeight(ButtonPanel);

  AdditionalPopupMenu := TPopupMenu.Create(AForm);
  FillAdditionalPopupMenuEntries(AdditionalPopupMenu);

  AdditionalBtn.Visible := AdditionalPopupMenu.Items.Count > 0;

  MainPanel := DynControlEngine.CreatePanelControl(AForm, AForm, 'MainPanel', '', alClient);
  MainPanel.TabOrder := 0;
  if Supports(MainPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBorderWidth(5);
  if Supports(MainPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBevelOuter(bvNone);
  ListPanel := DynControlEngine.CreatePanelControl(AForm, MainPanel, 'ListPanel', '', alClient);
  ListPanel.Anchors := [akTop, akRight, akBottom];
  if Supports(ListPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBevelOuter(bvNone);
  ListPanel.TabOrder := 1;

  ConnectListLabel := DynControlEngine.CreateStaticTextControl(AForm, ListPanel, 'ConnectListLabel', 'Connection List');
  AlignControlTop(ConnectListLabel, nil);
  ConnectListLabel.Height := 18;

  FConnectionFilterEdit := TWinControl(DynControlEngine.CreateControl(jctEdit, AForm, ListPanel, 'ConnectionFilterEdit'));
  FConnectionFilterEdit.Visible := Options.ShowConnectionFilter;
  Supports(FConnectionFilterEdit, IJvDynControlData, IConnectionFilterEditData);
  IConnectionFilterEditData.ControlValue := '';
  IConnectionFilterEditData.ControlSetOnChange(ConnectionFilterEditChange);
  AlignControlTop(FConnectionFilterEdit, ConnectListLabel);

  ConnectionFilterTimer:= TTimer.Create(self);
  ConnectionFilterTimer.Interval := 250;
  ConnectionFilterTimer.Enabled := False;
  ConnectionFilterTimer.OnTimer := ConnectionFilterTimerTimer;

  ListBtnPanel := DynControlEngine.CreatePanelControl(AForm, MainPanel, 'ListBtnPanel', '', alLeft);
  ListBtnPanel.Width := 32;

  AddToListBtn := DynControlEngine.CreateButton(AForm, ListBtnPanel, 'AddToListBtn',
    '>', RsBtnHintAddDefinitionToList, AddToListBtnClick, False, False);
  AddToListBtn.Left := 4;
  AddToListBtn.Top := 45;
  AddToListBtn.Width := 23;
  AddToListBtn.Height := 22;
  GetFromListBtn := DynControlEngine.CreateButton(AForm, ListBtnPanel, 'GetFromListBtn',
    '<', RsBtnHintSelectDefinitionFromList, GetFromListBtnClick, False, False);
  GetFromListBtn.Left := 4;
  GetFromListBtn.Top := 85;
  GetFromListBtn.Width := 23;
  GetFromListBtn.Height := 22;
  RemoveFromListBtn := DynControlEngine.CreateButton(AForm, ListBtnPanel, 'RemoveFromListBtn',
    'X', RsBtnHintDeleteDefinitionFromList, RemoveFromListBtnClick, False, False);
  RemoveFromListBtn.Left := 4;
  RemoveFromListBtn.Top := 125;
  RemoveFromListBtn.Width := 23;
  RemoveFromListBtn.Height := 22;

  Items := tStringList.Create;
  try
    Items.Add(RsPageDefaultList);
    Items.Add(RsPageByUser);
    Items.Add(RsPageByDatabase);
    Items.Add(RsPageByGroup);
    ConnectionListPageControl := DynControlEngine.CreatePageControlControl(AForm, ListPanel,
      'ConnectionListPageControl', Items);
  finally
    Items.Free;
  end;

  ConnectionListPageControl.Align := alClient;
  Supports(ConnectionListPageControl, IJvDynControlTabControl, IConnectionListPageControlTab);
  if Supports(ConnectionListPageControl, IJvDynControlTabControl, ITabControl) then
  begin
    ITabControl.ControlSetOnChangeTab(ConnectionListPageControlChange);
    ITabControl.ControlSetMultiLine(True);
    ITabControl.ControlTabIndex := 2;
  end;

  ConnectListListBox := DynControlEngine.CreateListBoxControl(AForm, AForm, 'ConnectListListBox', nil);
  ConnectListListBox.Align := alClient;
  if Supports(ConnectionListPageControl, IJvDynControlPageControl, IDynControlPageControl) then
    ConnectListListBox.Parent := IDynControlPageControl.ControlGetPage(RsPageDefaultList);

  Supports(ConnectListListBox, IJvDynControlItems, IConnectListListBoxItems);
  Supports(ConnectListListBox, IJvDynControlData, IConnectListListBoxData);
  if Supports(ConnectListListBox, IJvDynControl, IDynControl) then
    IDynControl.ControlSetOnClick(ConnectListListBoxClick);
  if Supports(ConnectListListBox, IJvDynControlDblClick, IDynControlDblClick) then
    IDynControlDblClick.ControlSetOnDblClick(ConnectListListBoxDblClick);

  UserTreeView := DynControlEngine.CreateTreeViewControl(AForm, AForm, 'UserTreeView');
  UserTreeView.Align := alClient;
  if Supports(ConnectionListPageControl, IJvDynControlPageControl, IDynControlPageControl) then
    UserTreeView.Parent := IDynControlPageControl.ControlGetPage(RsPageByUser);
  if Supports(UserTreeView, IJvDynControl, IDynControl) then
    IDynControl.ControlSetOnClick(ConnectListListBoxClick);
  if Supports(UserTreeView, IJvDynControlDblClick, IDynControlDblClick) then
    IDynControlDblClick.ControlSetOnDblClick(ConnectListListBoxDblClick);
  if Supports(UserTreeView, IJvDynControlTreeView, IUserTreeView) then
  begin
    IUserTreeView.ControlSetSortType(stText);
    IUserTreeView.ControlSetAutoExpand(true);
  end;
  if Supports(UserTreeView, IJvDynControlReadOnly, IDynControlReadOnly) then
    IDynControlReadOnly.ControlSetReadOnly(True);
  DatabaseTreeView := DynControlEngine.CreateTreeViewControl(AForm, AForm, 'DatabaseTreeView');
  DatabaseTreeView.Align := alClient;
  if Supports(ConnectionListPageControl, IJvDynControlPageControl, IDynControlPageControl) then
    DatabaseTreeView.Parent := IDynControlPageControl.ControlGetPage(RsPageByDatabase);
  if Supports(DatabaseTreeView, IJvDynControl, IDynControl) then
    IDynControl.ControlSetOnClick(ConnectListListBoxClick);
  if Supports(DatabaseTreeView, IJvDynControlDblClick, IDynControlDblClick) then
    IDynControlDblClick.ControlSetOnDblClick(ConnectListListBoxDblClick);
  if Supports(DatabaseTreeView, IJvDynControlTreeView, IDatabaseTreeView) then
    IDatabaseTreeView.ControlSetSortType(stText);
  if Supports(DatabaseTreeView, IJvDynControlReadOnly, IDynControlReadOnly) then
    IDynControlReadOnly.ControlSetReadOnly(True);

  if Options.ShowConnectGroup then
  begin
    GroupTreeView := DynControlEngine.CreateTreeViewControl(AForm, AForm, 'GroupTreeView');
    GroupTreeView.Align := alClient;
    if Supports(ConnectionListPageControl, IJvDynControlPageControl, IDynControlPageControl) then
      GroupTreeView.Parent := IDynControlPageControl.ControlGetPage(RsPageByGroup);
    if Supports(GroupTreeView, IJvDynControl, IDynControl) then
      IDynControl.ControlSetOnClick(ConnectListListBoxClick);
    if Supports(GroupTreeView, IJvDynControlDblClick, IDynControlDblClick) then
      IDynControlDblClick.ControlSetOnDblClick(ConnectListListBoxDblClick);
    if Supports(GroupTreeView, IJvDynControlTreeView, IGroupTreeView) then
      IGroupTreeView.ControlSetSortType(stText);
    if Supports(GroupTreeView, IJvDynControlReadOnly, IDynControlReadOnly) then
      IDynControlReadOnly.ControlSetReadOnly(True);

    GroupListPanel := DynControlEngine.CreatePanelControl(AForm, AForm, 'GroupListPanel', '', alBottom);
    if Supports(ConnectionListPageControl, IJvDynControlPageControl, IDynControlPageControl) then
      GroupListPanel.Parent := IDynControlPageControl.ControlGetPage(RsPageByGroup);
    GroupListPanel.Height := 25;
    GroupListPanel.Align := alBottom;
    if Supports(GroupListPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
      IDynControlBevelBorder.ControlSetBevelOuter(bvNone);

    GroupByDatabaseCheckBox := DynControlEngine.CreateCheckboxControl(AForm, GroupListPanel, 'GroupByDatabaseCheckBox',
      RsCheckBoxGroupByDatabase);
    GroupByDatabaseCheckBox.Left := 0;
    GroupByDatabaseCheckBox.Top := 5;
    GroupByDatabaseCheckBox.Width := 116;
    GroupByDatabaseCheckBox.Height := 17;
    GroupByDatabaseCheckBox.TabOrder := 0;
    if Supports(GroupByDatabaseCheckBox, IJvDynControl, IDynControl) then
      IDynControl.ControlSetOnClick(GroupByDatabaseCheckBoxClick);
    Supports(GroupByDatabaseCheckBox, IJvDynControlCheckBox, IGroupByDatabaseCheckBox);

    GroupByUserCheckBox := DynControlEngine.CreateCheckboxControl(AForm, GroupListPanel, 'GroupByUserCheckBox',
      RsCheckBoxGroupByUser);
    GroupByUserCheckBox.Left := 125;
    GroupByUserCheckBox.Top := 5;
    GroupByUserCheckBox.Width := 97;
    GroupByUserCheckBox.Height := 17;
    if Supports(GroupByUserCheckBox, IJvDynControl, IDynControl) then
      IDynControl.ControlSetOnClick(GroupByUserCheckBoxClick);
    Supports(GroupByUserCheckBox, IJvDynControlCheckBox, IGroupByUserCheckBox);
  end;

  SavePasswordsCheckBox := DynControlEngine.CreateCheckboxControl(AForm, ListPanel, 'SavePasswordsCheckBox', RsCheckboxSavePasswords);
  SavePasswordsCheckBox.Align := alBottom;
  Supports(SavePasswordsCheckBox, IJvDynControlCheckBox, ISavePasswordsCheckBox);
  SavePasswordsCheckBox.Visible := Options.ShowSavePasswords;

  LeftPanel := DynControlEngine.CreatePanelControl(AForm, MainPanel, 'LeftPanel', '', alLeft);
  LeftPanel.Width := 280;
  if Supports(LeftPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBevelOuter(bvNone);
  LeftPanel.TabOrder := 0;

  EditConnectionPanel := DynControlEngine.CreatePanelControl(AForm, LeftPanel, 'EditConnectionPanel', '', alTop);
  AlignControlTop(EditConnectionPanel,nil);
  if Supports(EditConnectionPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBevelOuter(bvNone);

  LeftBottomPanel := DynControlEngine.CreatePanelControl(AForm, LeftPanel, 'LeftBottomPanel', '', alTop);
  AlignControlTop(LeftBottomPanel,EditConnectionPanel);
  if Supports(LeftBottomPanel, IJvDynControlBevelBorder, IDynControlBevelBorder) then
    IDynControlBevelBorder.ControlSetBevelOuter(bvNone);

  CreateAdditionalConnectDialogEditPanel(AForm, EditConnectionPanel, 'UserName', RsUsername, jctEdit, UserNamePanel, UserNameEdit, IUsernameEditData, DefaultOnEditChange);

  CreateAdditionalConnectDialogEditPanel(AForm, EditConnectionPanel, 'Password', RsPassword, jctEdit, PasswordPanel, PasswordEdit, IPasswordEditData, DefaultOnEditChange);
  if Supports(PasswordEdit, IJvDynControlEdit, IDynControlEdit) then
    IDynControlEdit.ControlSetPasswordChar('*');

  CreateAdditionalConnectDialogEditPanel(AForm, EditConnectionPanel, 'Database', RsDatabase, jctComboBox, DatabasePanel, DatabaseComboBox, IDatabaseComboBoxData, DefaultOnEditChange);
  if Supports(DatabaseComboBox, IJvDynControl, IDynControl) then
  begin
    IDynControl.ControlSetOnClick(DefaultOnEditChange);
    IDynControl.ControlSetOnExit(DefaultOnEditChange); // Fix for the VCL/JVCL Controls which did not react on OnChange and OnClick
  end;

  CreateAdditionalConnectDialogEditPanel(AForm, EditConnectionPanel, 'Alias', RsAlias, jctEdit, AliasPanel, AliasEdit, IAliasEditData, DefaultOnEditChange);

  CreateAdditionalConnectDialogControls(AForm, EditConnectionPanel);

  CreateAdditionalConnectDialogEditPanel(AForm, LeftBottomPanel, 'ShortCut', RsShortCut, jctComboBox, ShortCutPanel, ShortCutComboBox, IShortCutComboBoxData, DefaultOnEditChange);
  if Supports(ShortCutComboBox, IJvDynControlComboBox, IDynControlComboBox) then
    IDynControlComboBox.ControlSetNewEntriesAllowed(False);
  Items := tStringList.Create;
  try
    FillShortCutList(Items);
    if Supports(ShortCutComboBox, IJvDynControlItems, IDynControlItems) then
      IDynControlItems.ControlItems.Assign(Items);
  finally
    Items.Free;
  end;
  ShortCutPanel.Visible := Options.ShowShortcuts;

  CreateAdditionalConnectDialogEditPanel(AForm, LeftBottomPanel, 'ConnectGroup', RsConnectGroup, jctComboBox, ConnectGroupPanel, ConnectGroupComboBox, IConnectGroupComboBoxData, DefaultOnEditChange);
  ConnectGroupPanel.Visible := Options.ShowConnectGroup;
  if Supports(ConnectGroupComboBox, IJvDynControl, IDynControl) then
  begin
    IDynControl.ControlSetOnClick(DefaultOnEditChange);
    IDynControl.ControlSetOnExit(DefaultOnEditChange); // Fix for the VCL/JVCL Controls which did not react on OnChange and OnClick
  end;
  Supports(ConnectGroupComboBox, IJvDynControlItems, IConnectGroupComboBoxItems);

  ColorBoxPanel := DynControlEngine.CreatePanelControl(AForm, LeftBottomPanel, 'ColorBoxPanel', '', alTop);
  AlignControlTop(ColorBoxPanel, ConnectGroupPanel);

  LabelControl := DynControlEngine.CreateLabelControl(AForm, ColorBoxPanel, 'ColorBoxLabel', 'Co&lor');
  AlignControlTop(LabelControl, nil);
  Items := tStringList.Create;
  try
    ColorComboBox := DynControlEngine.CreateColorComboBoxControl(AForm, ColorBoxPanel, 'ColorComboBox',
      cDefaultColorComboBoxColor);
    Supports(ColorComboBox, IJvDynControlColorComboBoxControl, IColorComboBox);
    AlignControlTop(ColorComboBox, LabelControl);
  finally
    Items.Free;
  end;
  if Supports(LabelControl, IJvDynControlLabel, IDynControlLabel) then
    IDynControlLabel.ControlSetFocusControl(ColorComboBox);
  if Supports(LabelControl, IJvDynControlAutoSize,IDynControlAutoSize) then
    IDynControlAutoSize.ControlSetAutoSize(True);
  ColorBoxPanel.Visible := Options.ShowColors;
  SetPanelHeight(ColorBoxPanel);

end;

function TJvBaseDBLogonDialog.CreatePasswordChangeDialog: TJvBaseDBPasswordDialog;
begin
  Result := nil;
end;

procedure TJvBaseDBLogonDialog.FillUserTreeView;
var
  i, j: Integer;
  Node: TTreeNode;
  Found: Boolean;
  s: string;
  Connection: TJvBaseConnectionInfo;
  UserName : String;
  Items: TTreeNodes;
begin
  Items := IUserTreeView.ControlItems;
  try
    Items.BeginUpdate;
    Items.Clear;
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if not Connection.MatchesFilter(ConnectionFilterValue) then
        Continue;
      s := ListConnectString(Connection, Options.ShowShortcuts, Options.ShowConnectGroup);
      UserName := UpperCase(Connection.Username);

      Found := False;
      for j := 0 to Items.Count - 1 do
        if Items[j].Level = 0 then
        begin
          Node := Items[j];
          if Node.Text = Username then
          begin
            Node := Items.AddChild(Node, s);
            Node.Data := Connection;
            Found := True;
            break;
          end;
        end;
      if not Found then
      begin
        Node := Items.AddChild(nil, Username);
        Node := Items.AddChild(Node, s);
        Node.Data := Connection;
      end;
    end;
    if (ConnectionFilterValue <> '') then
      for i := 0 to Items.Count-1 do
        Items.Item[i].Expand(True);
    if Items.Count > 0 then
      Items.Item[0].Selected := True;
  finally
    Items.EndUpdate;
  end;
  IUserTreeView.ControlSortItems;
end;

function TJvBaseDBLogonDialog.DecryptPassword(const Value: string): string;
begin
  try
    Result := Value;
    if Assigned(FOnDecryptPassword) then
      FOnDecryptPassword(Result);
  except
    Result := '';
  end;
end;

procedure TJvBaseDBLogonDialog.DefaultOnEditChange(Sender: TObject);
begin
  if csDestroying in ComponentState then
    Exit;
  TransferConnectionInfoFromDialog(CurrentConnectionInfo);
  ValidateConnectBtnEnabled;
end;

procedure TJvBaseDBLogonDialog.DoSessionConnect;
begin
  TransferSessionDataFromDialog;
  if Options.SetLastConnectToTop then
    SetConnectionToTop(CurrentConnectionInfo.SearchName);
  if Assigned(OnSessionConnect) then
    OnSessionConnect(Session)
  else
    ConnectSession;
  if SessionIsConnected then
    DBDialog.ModalResult := mrok;
end;

procedure TJvBaseDBLogonDialog.DoSessionDisconnect;
begin
  if SessionIsConnected then
    if Assigned(OnSessionDisconnect) then
      OnSessionDisconnect(Session)
    else
      DisconnectSession;
end;

function TJvBaseDBLogonDialog.EncryptPassword(const Value: string): string;
begin
  try
    Result := Value;
    if Assigned(FOnEncryptPassword) then
      FOnEncryptPassword(Result);
  except
    Result := '';
  end;
end;

procedure TJvBaseDBLogonDialog.FillAdditionalPopupMenuEntries(APopupMenu: TPopupMenu);
var
  MenuItem: TMenuItem;
begin
  if Options.ShowConnectionsExport then
  begin
    MenuItem := TMenuItem.Create(APopupMenu.Owner);
    MenuItem.Caption := RSExportConnectionList;
    MenuItem.OnClick := OnExportConnectionList;
    APopupMenu.Items.Add(MenuItem);
    MenuItem := TMenuItem.Create(APopupMenu.Owner);
    MenuItem.Caption := RSImportConnectionList;
    MenuItem.OnClick := OnImportConnectionList;
    APopupMenu.Items.Add(MenuItem);
  end;
end;

procedure TJvBaseDBLogonDialog.FillAllComoboBoxes;
begin
  FillDatabaseComboBox;
end;

procedure TJvBaseDBLogonDialog.FillAllConnectionLists;
begin
  FillAllComoboBoxes;
  FillConnectionList;
  FillDatabaseTreeView;
  FillGroupTreeView;
  FillConnectGroupComboBox;
  FillUserTreeView;
  SetButtonState;
end;

procedure TJvBaseDBLogonDialog.FillConnectGroupComboBox;
var
  i: Integer;
  Connection: TJvBaseConnectionInfo;
  Items: TStringList;
begin
  if Assigned(IConnectGroupComboBoxItems) then
  begin
    Items := TStringList.Create;
    try
      Items.Sorted := True;
      for i := 0 to ConnectionList.Count - 1 do
      begin
        Connection := ConnectionList.Connection[i];
        if not Connection.MatchesFilter(ConnectionFilterValue) then
          Continue;
        if Connection.Group <> '' then
          if Items.IndexOf(Connection.Group) < 0 then
            Items.Add(Connection.Group);
      end;
      IConnectGroupComboBoxItems.ControlItems.Assign(Items);
    finally
      Items.Free;
    end;
  end;
end;

procedure TJvBaseDBLogonDialog.FillConnectionList;
var
  i: Integer;
  Connection: TJvBaseConnectionInfo;
  Items: TStrings;
begin
  if Assigned(IConnectListListBoxItems) then
  begin
    Items := IConnectListListBoxItems.ControlItems;
    try
      Items.BeginUpdate;
      Items.Clear;
      for i := 0 to ConnectionList.Count - 1 do
      begin
        Connection := ConnectionList.Connection[i];
        if not Connection.MatchesFilter(ConnectionFilterValue) then
          Continue;
        Items.AddObject(ListConnectString(Connection, Options.ShowShortCuts, Options.ShowConnectGroup), Connection);
      end;
      if Assigned(IConnectListListBoxData) and (Items.Count > 0) then
      begin
        IConnectListListBoxData.ControlValue := Items[0];
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TJvBaseDBLogonDialog.FillDatabaseComboBox;
var
  Items: TStringList;
  IDynControlItems: IJvDynControlItems;
begin
  if Supports(DatabaseComboBox, IJvDynControlItems, IDynControlItems) then
  begin
    Items := TStringList.Create;
    try
      Items.Sorted := True;
      FillDatabaseComboBoxValues (Items);
      if Assigned(FOnFillDatabaseList) then
        FOnFillDatabaseList(Items);
      IDynControlItems.ControlItems.Assign(Items);
    finally
      Items.Free;
    end;
  end;
end;

procedure TJvBaseDBLogonDialog.FillDatabaseComboBoxValues(Items: TStrings);
var i : Integer;
  Connection: TJvBaseConnectionInfo;
begin
  if Options.AddConnectionValuesToComboBox then
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if Connection.Database <> '' then
        if Items.IndexOf(Connection.Database) < 0 then
          Items.Add(Connection.Database);
    end;
end;

procedure TJvBaseDBLogonDialog.FillDatabaseTreeView;
var
  i, j: Integer;
  Node: TTreeNode;
  Found: Boolean;
  s: string;
  Connection: TJvBaseConnectionInfo;
  DatabaseName : String;
  Items: TTreeNodes;
begin
  Items := IDatabaseTreeView.ControlItems;
  try
    Items.BeginUpdate;
    Items.Clear;
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if not Connection.MatchesFilter(ConnectionFilterValue) then
        Continue;
      s := ListConnectString(Connection, Options.ShowShortCuts, Options.ShowConnectGroup);
      DatabaseName := UpperCase(Connection.DatabaseGroupIdentifier);
      Found := False;
      for j := 0 to Items.Count - 1 do
        if Items[j].Level = 0 then
        begin
          Node := Items[j];
          if Node.Text = DatabaseName then
          begin
            Node := Items.AddChild(Node, s);
            Node.Data := Connection;
            Found := True;
            break;
          end;
        end;
      if not Found then
      begin
        Node := Items.AddChild(nil, DatabaseName);
        Node := Items.AddChild(Node, s);
        Node.Data := Connection;
      end;
    end;
    if (ConnectionFilterValue <> '') then
      for i := 0 to Items.Count-1 do
        Items.Item[i].Expand(True);
    if Items.Count > 0 then
      Items.Item[0].Selected := True;
  finally
    Items.EndUpdate;
  end;
  IDatabaseTreeView.ControlSortItems;
end;

procedure TJvBaseDBLogonDialog.FillGroupTreeView;
var
  i, j, k, g: Integer;
  Items: TTreeNodes;
  Node: TTreeNode;
  Node2: TTreeNode;
  Found: Boolean;
  s: string;
  gr: string;
  GroupList: TStringList;
  Connection: TJvBaseConnectionInfo;
  UserName : String;
  DatabaseName : String;
begin
  if not Assigned(IGroupTreeView) then
    Exit;
  Items := IGroupTreeView.ControlItems;
  try
    Items.BeginUpdate;
    Items.Clear;
    for i := 0 to ConnectionList.Count - 1 do
    begin
      Connection := ConnectionList.Connection[i];
      if not Connection.MatchesFilter(ConnectionFilterValue) then
        Continue;
      UserName := UpperCase(Connection.Username);
      DatabaseName := UpperCase(Connection.DatabaseGroupIdentifier);
      GroupList := TStringList.Create;
      try
        {$IFDEF DELPHI2009_UP}
        GroupList.StrictDelimiter:=true;
        {$ENDIF DELPHI2009_UP}
        GroupList.Duplicates := dupIgnore;
        GroupList.Sorted := True;
        if (Pos(';',Connection.Group) >= 1) and (Pos(',',Connection.Group) < 1)then
          GroupList.Delimiter := ';'
        else
          GroupList.Delimiter := ',';
        GroupList.DelimitedText := Connection.Group;
        GroupList.Delimiter := ',';
        Connection.Group := GroupList.CommaText;
        if GroupList.CommaText = '' then
          GroupList.CommaText := RsGroupNameUndefined;

        for g := 0 to GroupList.Count - 1 do
        begin
          Gr := GroupList[g];
          if gr = '' then
            continue;
          s := ListConnectString(Connection, Options.ShowShortcuts, False);

          Found := False;
          for j := 0 to Items.Count - 1 do
            if Items[j].Level = 0 then
            begin
              Node := Items[j];
              if Node.Text = Gr then
                if GroupByDatabase then
                begin
                  for k := 0 to Node.Count - 1 do
                  begin
                    Node2 := Node.Item[k];
                    if Node2.Text = DatabaseName then
                    begin
                      Node := Items.AddChild(Node2, s);
                      Node.Data := Connection;
                      Found := True;
                      break;
                    end;
                  end;
                  if not Found then
                  begin
                    Node := Items.AddChild(Node, DatabaseName);
                    Node := Items.AddChild(Node, s);
                    Node.Data := Connection;
                    Found := True;
                  end;
                  Break;
                end
                else
                  if GroupByUser then
                  begin
                    for k := 0 to Node.Count - 1 do
                    begin
                      Node2 := Node.Item[k];
                      if Node2.Text = Username then
                      begin
                        Node := Items.AddChild(Node2, s);
                        Node.Data := Connection;
                        Found := True;
                        break;
                      end;
                    end;
                    if not Found then
                    begin
                      Node := Items.AddChild(Node, Username);
                      Node := Items.AddChild(Node, s);
                      //Node.SelectedIndex := i;
                      Node.Data := Connection;
                      Found := True;
                    end;
                    Break;
                  end
                  else
                  begin
                    Node := Items.AddChild(Node, s);
                    Node.Data := Connection;
                    Found := True;
                    break;
                  end; {*** IF Node.Text = UpperCase(Databases[i]) THEN ***}
            end; {*** IF Items[i].Level = 0 THEN ***}
          if not Found then
          begin
            Node := Items.AddChild(nil, Gr);
            if GroupByDataBase then
              Node := Items.AddChild(Node, DatabaseName)
            else
              if GroupByUser then
                Node := Items.AddChild(Node, Username);
            Node := Items.AddChild(Node, s);
            Node.Data := Connection;
          end;
        end;
      finally
        GroupList.Free;
      end;
    end;
    if (ConnectionFilterValue <> '') then
      for i := 0 to Items.Count-1 do
        Items.Item[i].Expand(True);
    if Items.Count > 0 then
      Items.Item[0].Selected := True;
  finally
    Items.EndUpdate;
  end;
  IGroupTreeView.ControlSortItems;
end;

procedure TJvBaseDBLogonDialog.FillShortCutList(Items: TStringList);
var
  I: Integer;
begin
  Items.Add('');
  for I := 1 to 10 do
    Items.Add(ShortCutToText(ShortCut($30 + Ord('0') + (I mod 10), [ssCtrl])));
  for I := 1 to 10 do
    Items.Add(ShortCutToText(ShortCut($30 + Ord('0') + (I mod 10), [ssCtrl, ssShift])));
  for I := 1 to 10 do
    Items.Add(ShortCutToText(ShortCut($30 + Ord('0') + (I mod 10), [ssAlt, ssShift])));
  for I := 1 to 10 do
    Items.Add(ShortCutToText(ShortCut($30 + Ord('0') + (I mod 10), [ssAlt, ssCtrl])));
  for I := 1 to 10 do
    Items.Add(ShortCutToText(ShortCut($30 + Ord('0') + (I mod 10), [ssAlt, ssCtrl, ssShift])));
end;

procedure TJvBaseDBLogonDialog.FormClose(Sender: TObject; var Action:TCloseAction);
begin
  if DBDialog.ModalResult = mrOk then
  begin
    if Options.SaveLastConnect then
      TransferConnectionInfoFromDialog(ConnectionList.LastConnect)
    else
      ConnectionList.LastConnect.Clear;
    StoreSettings;
  end;
  ClearControlInterfaceObjects;
  Action := caFree;
end;

procedure TJvBaseDBLogonDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  Connection: TJvBaseConnectionInfo;
  sKey: Word;
  sShift: TShiftState;
begin
  for i := 0 to ConnectionList.Count - 1 do
  begin
    ShortCutToKey(ConnectionList.Connection[i].Shortcut, sKey, sShift);
    if (sKey = Key) and (sShift = Shift) then
    begin
      Connection := ConnectionList.Connection[i];
      TransferConnectionInfoToDialog(Connection);
      ConnectToSession;
      Exit;
    end;
  end;
  if (Shift = [ssCtrl]) then
    Case Key of
       VK_UP,
       Ord ('F') : if FConnectionFilterEdit.Visible then FConnectionFilterEdit.SetFocus;
       VK_LEFT : if GetFromListBtn.Enabled then GetFromListBtnClick(nil);
       VK_RIGHT : AddToListBtnClick(nil);
       VK_Down :
          if FConnectionFilterEdit.Focused then
          case ActivePage of
            ldapUserTree: if Assigned(UserTreeView) then UserTreeView.SetFocus;
            ldapDatabaseTree: if Assigned(DatabaseTreeView) then DatabaseTreeView.SetFocus;
            ldapGroupTree: if Assigned(GroupTreeView) then GroupTreeView.SetFocus;
            ldapConnectList: if Assigned(ConnectListListBox) then ConnectListListBox.SetFocus;
          end;

    End;
end;

procedure TJvBaseDBLogonDialog.FormShow(Sender: TObject);
begin
  LoadSettings;
  ResizeAllControls;
  ClearFormControls;
  FillAllConnectionLists;
  TransferSessionDataToDialog;
  if Options.SaveLastConnect then
  begin
    if (ConnectionList.LastConnect.SearchName = CurrentConnectionInfo.SearchName) then
      ConnectionList.LastConnect.Password := CurrentConnectionInfo.Password;
    if ((DialogUserName = '') or
        (ConnectionList.LastConnect.SearchName = CurrentConnectionInfo.SearchName)) then
      TransferConnectionInfoToDialog(ConnectionList.LastConnect);
  end;
  ValidateConnectBtnEnabled;
end;

procedure TJvBaseDBLogonDialog.FreeFormControls;
begin
  IAliasEditData:= nil;
  IConnectionFilterEditData:= nil;
  IColorComboBox:= nil;
  IConnectGroupComboBoxData:= nil;
  IConnectGroupComboBoxItems:= nil;
  IConnectionListPageControlTab:= nil;
  IConnectListListBoxData:= nil;
  IConnectListListBoxItems:= nil;
  IDatabaseComboBoxData:= nil;
  IDatabaseTreeView:= nil;
  IGroupByDatabaseCheckBox:= nil;
  IGroupByUserCheckBox:= nil;
  IGroupTreeView:= nil;
  IPasswordEditData:= nil;
  ISavePasswordsCheckBox:= nil;
  IShortCutComboBoxData:= nil;
  IUserNameEditData:= nil;
  IUserTreeView:= nil;
  inherited;
end;

function TJvBaseDBLogonDialog.GetActivePage: TJvDBLogonDialogActivePage;
begin
  if IConnectionListPageControlTab.ControlTabIndex = 1 then
    Result := ldapUserTree
  else
    if IConnectionListPageControlTab.ControlTabIndex = 2 then
      Result := ldapDatabaseTree
    else
      if IConnectionListPageControlTab.ControlTabIndex = 3 then
        Result := ldapGroupTree
      else
        Result := ldapConnectList;
end;

function TJvBaseDBLogonDialog.GetConnectionFilterValue: string;
begin
  Result := VarToStr(IConnectionFilterEditData.ControlValue);
end;

function TJvBaseDBLogonDialog.GetCurrentDialogListConnectionInfo: TJvBaseConnectionInfo;
begin
  Result := nil;
  case ActivePage of
    ldapUserTree:
      if Assigned(IUserTreeView.ControlGetSelected) and
        (IUserTreeView.ControlGetSelected.Level > 0) then
        Result := IUserTreeView.ControlGetSelected.Data;
    ldapDatabaseTree:
      if Assigned(IDatabaseTreeView.ControlGetSelected) and
        (IDatabaseTreeView.ControlGetSelected.Level > 0) then
        Result := IDatabaseTreeView.ControlGetSelected.Data;
    ldapGroupTree:
      if Assigned(IGroupTreeView) and
        Assigned(IGroupTreeView.ControlGetSelected) and
        (IGroupTreeView.ControlGetSelected.Level > 0) then
        Result := IGroupTreeView.ControlGetSelected.Data;
    ldapConnectList:
      if (IConnectListListBoxItems.ControlItems.Count > 0) and
        (IConnectListListBoxData.ControlValue >= 0) and
        (IConnectListListBoxData.ControlValue <= IConnectListListBoxItems.ControlItems.Count - 1) then
        Result :=
          TJvBaseConnectionInfo(IConnectListListBoxItems.ControlItems.Objects[IConnectListListBoxData.ControlValue]);
  end;
end;

class function TJvBaseDBLogonDialog.GetDBLogonConnectionListClass: TJvBaseConnectionListClass;
begin
  Result := TJvBaseConnectionList;
end;

class function TJvBaseDBLogonDialog.GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass;
begin
  Result := TJvBaseDBLogonDialogOptions;
end;

function TJvBaseDBLogonDialog.GetDialogDatabase: string;
begin
  if Assigned(IDatabaseComboBoxData) then
    Result := IDatabaseComboBoxData.ControlValue
  else
    Result := '';
end;

function TJvBaseDBLogonDialog.GetDialogPassword: string;
begin
  if Assigned(IPasswordEditData) then
    Result := IPasswordEditData.ControlValue
  else
    Result := '';
end;

function TJvBaseDBLogonDialog.GetDialogUserName: string;
begin
  if Assigned(IUserNameEditData) then
    Result := IUserNameEditData.ControlValue
  else
    Result := '';
end;

procedure TJvBaseDBLogonDialog.GetFromListBtnClick(Sender: TObject);
begin
  TransferConnectionInfoToDialog(CurrentDialogListConnectionInfo);
  ValidateConnectBtnEnabled;
end;

function TJvBaseDBLogonDialog.GetGroupByDatabase: Boolean;
begin
  Result := FGroupByDatabase;
end;

function TJvBaseDBLogonDialog.GetGroupByUser: Boolean;
begin
  Result := fGroupByUser;
end;

procedure TJvBaseDBLogonDialog.GroupByDatabaseCheckBoxClick(Sender: TObject);
begin
  if Assigned(IGroupByDatabaseCheckBox) then
    GroupByDatabase := IGroupByDatabaseCheckBox.ControlState = cbChecked;
end;

procedure TJvBaseDBLogonDialog.GroupByUserCheckBoxClick(Sender: TObject);
begin
  if Assigned(IGroupByUserCheckBox) then
    GroupByUser := IGroupByUserCheckBox.ControlState = cbChecked;
end;

function TJvBaseDBLogonDialog.IsConnectAllowed: Boolean;
begin
  Result:= CurrentConnectionInfo.IsConnectAllowed(Options.AllowNullPasswords);
end;

function TJvBaseDBLogonDialog.ListConnectString(Connection: TJvBaseConnectionInfo; ShowShortCut, ShowConnectGroup:
    Boolean): string;
var
  s: string;
begin
  if Not Assigned(Connection) then
    Exit;
  s := Connection.ConnectString;
  if ShowShortCut then
    if Connection.ShortCutText <> '' then
      Result := Result + ' (' + Connection.ShortCutText + ')';
  if ShowConnectGroup then
    if Connection.Group <> '' then
      Result := Result + ' - ' + Connection.Group;
  Result := s;
end;

procedure TJvBaseDBLogonDialog.LoadSettings;
begin
  ConnectionList.SavePasswords := SavePasswords;
  ConnectionList.LoadProperties;
  if Options.ShowSavePasswords then
    if ConnectionList.SavePasswords then
      ISavePasswordsCheckBox.ControlState := cbChecked
    else
      ISavePasswordsCheckBox.ControlState := cbUnChecked;
  if Assigned(IGroupByDatabaseCheckBox) then
    if ConnectionList.GroupByDatabase then
      IGroupByDatabaseCheckBox.ControlState := cbChecked
    else
      IGroupByDatabaseCheckBox.ControlState := cbUnChecked;
  if Assigned(IGroupByUserCheckBox) then
    if ConnectionList.GroupByUser then
      IGroupByUserCheckBox.ControlState := cbChecked
    else
      IGroupByUserCheckBox.ControlState := cbUnChecked;
  ActivePage := ConnectionList.ActivePage;
end;

procedure TJvBaseDBLogonDialog.OnExportConnectionList(Sender: TObject);
var
  Savedialog: TSaveDialog;
  TmpAppStorage: TJvCustomAppMemoryFileStorage;
  FileName: string;
  Extention: string;
  TmpConnectionList: TJvBaseConnectionList;
begin
  TmpAppStorage := nil;
  Savedialog := TSaveDialog.Create(Self);
  TmpConnectionList := GetDBLogonConnectionListClass.Create(Self);
  try
    TmpConnectionList.CopyContents(ConnectionList, True);
    SaveDialog.Filter := RsConnectionListExportImportFilter;
    SaveDialog.Name := 'SaveDialog';
    SaveDialog.DefaultExt := 'xml';
    SaveDialog.Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing];
    if Savedialog.Execute then
    begin
      FileName := SaveDialog.FileName;
      Extention := ExtractFileExt(Filename);
      if UpperCase(extention) = '.INI' then
      begin
        TmpAppStorage := TJvAppIniFileStorage.Create(Self);
        //        TJvAppIniFileStorage(TmpAppStorage).Section := 'Export/Import';
      end
      else
      begin
        TmpAppStorage := TJvAppXMLFileStorage.Create(Self);
        TJvAppXMLFileStorage(TMPAppStorage).StorageOptions.WhiteSpaceReplacement := '_';
      end;
      TmpAppStorage.FileName := Filename;
      TmpAppStorage.Location := flCustom;
      TmpConnectionList.AppStorage := TmpAppStorage;
      TmpConnectionList.AppStoragePath := ConnectionList.AppStoragePath;
      TmpConnectionList.SavePasswords := False;
      TmpConnectionList.StoreProperties;
      TmpAppStorage.Flush;
    end;
  finally
    if Assigned(TmpAppStorage) then
      FreeAndNil(TmpAppStorage);
    FreeAndNil(SaveDialog);
    FreeAndNIl(TmpConnectionList);
  end;
end;

procedure TJvBaseDBLogonDialog.OnImportConnectionList(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  TmpAppStorage: TJvCustomAppMemoryFileStorage;
  FileName: string;
  Extention: string;
  TmpConnectionList: TJvBaseConnectionList;
  ClearBefore: Boolean;
  Buttons: array[0..2] of string;
  Results: array[0..2] of Integer;
begin
  TmpAppStorage := nil;
  OpenDialog := TOpenDialog.Create(Self);
  TmpConnectionList := GetDBLogonConnectionListClass.Create(Self);
  try
    OpenDialog.Filter := RsConnectionListExportImportFilter;
    OpenDialog.Name := 'OpenDialog';
    OpenDialog.DefaultExt := 'xml';
    OpenDialog.Options := [ofHideReadOnly, ofExtensionDifferent, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    if OpenDialog.Execute then
    begin
      FileName := OpenDialog.FileName;
      Extention := ExtractFileExt(Filename);
      if UpperCase(extention) = '.INI' then
      begin
        TmpAppStorage := TJvAppIniFileStorage.Create(Self);
      end
      else
      begin
        TmpAppStorage := TJvAppXMLFileStorage.Create(Self);
        TJvAppXMLFileStorage(TMPAppStorage).StorageOptions.WhiteSpaceReplacement := '_';
      end;
      TmpAppStorage.FileName := Filename;
      TmpAppStorage.Location := flCustom;
      TmpConnectionList.AppStorage := TmpAppStorage;
      TmpConnectionList.AppStoragePath := ConnectionList.AppStoragePath;
      TmpConnectionList.LoadProperties;
      if TmpConnectionList.Count <= 0 then
      begin
        JvDSADialogs.MessageDlg(RsNoConnectionEntriesFound, mtError, [mbok], 0, dckScreen,
          0, mbDefault, mbDefault, mbDefault, DynControlEngine);
        exit;
      end;
      ClearBefore := True;
      if ConnectionList.Count > 0 then
      begin
        Buttons[0] := RsConnectionListImportAppend;
        Buttons[1] := RsConnectionListImportOverwrite;
        Buttons[2] := RsButtonCancelCaption;
        Results[0] := Integer(mrYes);
        Results[1] := Integer(mrNo);
        Results[2] := Integer(mrCancel);
        case JvDSADialogs.MessageDlgEx(RsConnectionListImportAppendOverwriteExistingEntries,
          mtConfirmation, Buttons, Results, 0, dckScreen, 0,
          0, 2, -1, DynControlEngine) of
          mrYes:
            ClearBefore := False;
          mrNo:
            ClearBefore := True;
        else
          Exit;
        end;
      end;
      ConnectionList.CopyContents(TmpConnectionList, ClearBefore);
      FillAllConnectionLists
    end;
  finally
    if Assigned(TmpAppStorage) then
      FreeAndNil(TmpAppStorage);
    FreeAndNil(OpenDialog);
    FreeAndNIl(TmpConnectionList);
  end;
end;

procedure TJvBaseDBLogonDialog.PasswordDialog_AfterTransferPasswordFromSession(var Password: string);
var
  Connection: TJvBaseConnectionInfo;
begin
  Connection := TJvBaseConnectionInfo.Create(nil);
  try
    Connection.Password := Password;
    if Assigned(AfterTransferSessionDataToConnectionInfo) then
      AfterTransferSessionDataToConnectionInfo(Connection);
    Password := Connection.Password;
  finally
    Connection.Free;
  end;
end;

procedure TJvBaseDBLogonDialog.PasswordDialog_BeforeTransferPasswordToSession(var Password: string);
var
  Connection: TJvBaseConnectionInfo;
begin
  Connection := TJvBaseConnectionInfo.Create(nil);
  try
    Connection.Password := Password;
    if Assigned(BeforeTransferConnectionInfoToSessionData) then
      BeforeTransferConnectionInfoToSessionData(Connection);
    Password := Connection.Password;
  finally
    Connection.Free;
  end;
end;

procedure TJvBaseDBLogonDialog.RearrangeEditPanel;
begin
  SetEditPanelsVisibility;
  RearrangeEditPanelControlsByTaborder;
  SetPanelHeight(EditConnectionPanel);
end;

procedure TJvBaseDBLogonDialog.RearrangeEditPanelControlsByTaborder;
begin
  SetEditPanelsTabOrder;
  RearrangePanelControlsByTaborder(LeftBottomPanel);
  RearrangePanelControlsByTaborder(EditConnectionPanel);
end;

procedure TJvBaseDBLogonDialog.RearrangePanelControlsByTaborder(iPanel: TWinControl);
var
  i: Integer;
  p: Integer;
  t: Integer;
  Ctrl: TWinControl;
begin
  t := 0;
  p := 0;
  while p < iPanel.ControlCount do
  begin
    for I := 0 to iPanel.ControlCount-1 do
    begin
      if not (iPanel.Controls[i] is TWinControl) then
        Continue;
      Ctrl := TWinControl(iPanel.Controls[i]);
      if not Ctrl.Visible then
        Continue;
      if Ctrl.TabOrder = p then
      begin
        Ctrl.Top := t;
        t := Ctrl.Top+Ctrl.Height+1;
        break;
      end;
    end;
    Inc(p);
  end;
end;

procedure TJvBaseDBLogonDialog.RemoveFromListBtnClick(Sender: TObject);
var
  Index: Integer;
  Connection: TJvBaseConnectionInfo;
begin
  Connection := CurrentDialogListConnectionInfo;
  if Assigned(Connection) then
  begin
    Index := ConnectionList.Items.IndexOfObject(Connection);
    if (Index >= 0) and
      (Index < ConnectionList.Count) then
    begin
      ConnectionList.Items.delete(Index);
      FillAllConnectionLists;
    end;
  end;
end;

procedure TJvBaseDBLogonDialog.ResizeAllControls;
begin
  RearrangeEditPanel;
  ResizeFormControls;
  ResizeDialogClientHeight;
end;

procedure TJvBaseDBLogonDialog.ResizeDialogClientHeight;
var
  m : Integer;
begin
  m := CalculatePanelHeight(LeftPanel)+10;
  if m > LeftPanel.Height then
    if m + ButtonPanel.Height > DBDialog.ClientHeight then
    begin
      DBDialog.ClientHeight := m + ButtonPanel.Height;
      DBDialog.Refresh;
    end;
end;

procedure TJvBaseDBLogonDialog.ResizeFormControls;
begin
  SetPanelHeight(LeftBottomPanel);
  CancelBtn.Left := DBDialog.ClientWidth - CancelBtn.Width - 10;
  ConnectBtn.Left := CancelBtn.Left - ConnectBtn.Width - 5;
  AdditionalBtn.Left := 10;
  SetPanelHeight(ButtonPanel);
end;

function TJvBaseDBLogonDialog.SavePasswords: Boolean;
begin
  if not Options.SavePasswords then
    Result := False
  else
    if Options.ShowSavePasswords then
      Result := ISavePasswordsCheckBox.ControlState = cbChecked
    else
      Result := True;
end;

procedure TJvBaseDBLogonDialog.SetActivePage(const Value: TJvDBLogonDialogActivePage);
begin
  case Value of
    ldapUserTree:
      IConnectionListPageControlTab.ControlTabIndex := 1;
    ldapDatabaseTree:
      IConnectionListPageControlTab.ControlTabIndex := 2;
    ldapGroupTree:
      IConnectionListPageControlTab.ControlTabIndex := 3;
  else
    IConnectionListPageControlTab.ControlTabIndex := 0;
  end;
end;

procedure TJvBaseDBLogonDialog.SetAppStorage(Value: TJvCustomAppStorage);
begin
  if Value <> AppStorage then
    ConnectionList.AppStorage := Value;
  inherited SetAppStorage(Value);
end;

procedure TJvBaseDBLogonDialog.SetAppStoragePath(Value: string);
begin
  if Value <> AppStoragePath then
    ConnectionList.AppStoragePath := Value;
  inherited SetAppStoragePath(Value);
end;

procedure TJvBaseDBLogonDialog.SetButtonState;
begin
  if not Assigned(DBDialog) then
    Exit;
  GetFromListBtn.Enabled := Assigned(CurrentDialogListConnectionInfo);
  RemoveFromListBtn.Enabled := GetFromListBtn.Enabled;
end;

procedure TJvBaseDBLogonDialog.SetConnectionToTop(const SearchName: string);
var
  p: Integer;
begin
  p := Connectionlist.IndexOf(SearchName);
  if p >= 0 then
    ConnectionList.Items.Move(p, 0);
end;

procedure TJvBaseDBLogonDialog.SetDialogDatabase(const Value: string);
begin
  if Assigned(IDatabaseComboBoxData) then
    IDatabaseComboBoxData.ControlValue := Value;
end;

procedure TJvBaseDBLogonDialog.SetDialogPassword(const Value: string);
begin
  if Assigned(IPasswordEditData) then
    IPasswordEditData.ControlValue := Value;
end;

procedure TJvBaseDBLogonDialog.SetDialogUserName(const Value: string);
begin
  if Assigned(IUserNameEditData) then
    IUserNameEditData.ControlValue := Value;
end;

procedure TJvBaseDBLogonDialog.SetEditPanelsTabOrder;
begin
  if Assigned(UsernamePanel) then
    UsernamePanel.TabOrder := 0;
  if Assigned(PasswordPanel) then
    PasswordPanel.TabOrder := 1;
  if Assigned(DataBasePanel) then
    DataBasePanel.TabOrder := 2;
end;

procedure TJvBaseDBLogonDialog.SetEditPanelsVisibility;
begin
  SetPanelVisible(UsernamePanel, CurrentConnectionInfo.UsernameEnabled);
  SetPanelVisible(PasswordPanel, CurrentConnectionInfo.PasswordEnabled);
  SetPanelVisible(DataBasePanel, CurrentConnectionInfo.DatabaseEnabled);
  SetPanelVisible(AliasPanel, CurrentConnectionInfo.AliasEnabled and Options.ShowAlias);
end;

procedure TJvBaseDBLogonDialog.SetGroupByDatabase(Value: Boolean);
var
  Change: Boolean;
begin
  Change := Value <> FGroupByDatabase;
  if Change then
  begin
    if Assigned(IGroupByDatabaseCheckBox) then
      if Value then
        IGroupByDatabaseCheckBox.ControlSetState(cbChecked)
      else
        IGroupByDatabaseCheckBox.ControlSetState(cbUnChecked);
    if Assigned(IGroupByUserCheckBox) then
      if Value and GroupByUser then
        IGroupByUserCheckBox.ControlSetState(cbUnChecked);
  end;
  FGroupByDatabase := Value;
  if Change then
    FillGroupTreeView;
end;

procedure TJvBaseDBLogonDialog.SetGroupByUser(Value: Boolean);
var
  Change: Boolean;
begin
  Change := Value <> fGroupByUser;
  if Change then
  begin
    if Assigned(IGroupByUserCheckBox) then
      if Value then
        IGroupByUserCheckBox.ControlSetState(cbChecked)
      else
        IGroupByUserCheckBox.ControlSetState(cbUnChecked);
    if Assigned(IGroupByDatabaseCheckBox) then
      if Value and GroupByDatabase then
        IGroupByDatabaseCheckBox.ControlSetState(cbUnChecked);
  end;
  fGroupByUser := Value;
  if Change then
    FillGroupTreeView;
end;

procedure TJvBaseDBLogonDialog.SetOptions(const Value: TJvBaseDBLogonDialogOptions);
begin
  FOptions.Assign(Value);
end;

procedure TJvBaseDBLogonDialog.SetPanelHeight(iPanel: TWinControl);
begin
  if not Assigned(iPanel) then
    Exit;
  iPanel.Height := CalculatePanelHeight(iPanel);
end;

procedure TJvBaseDBLogonDialog.SetPanelVisible(iPanel: TWinControl; iVisible: Boolean);
begin
  if not Assigned(iPanel) or (iPanel.Visible = iVisible) then
    Exit;
  iPanel.Visible := iVisible;
end;

procedure TJvBaseDBLogonDialog.SetSession(const Value: TComponent);
begin
  inherited SetSession(Value);
  TransferSessionDataToDialog;
end;

procedure TJvBaseDBLogonDialog.StoreSettings;
begin
  ConnectionList.GroupByDatabase := Assigned(IGroupByDatabaseCheckBox) and
    (IGroupByDatabaseCheckBox.ControlState = cbChecked);
  ConnectionList.GroupByUser := Assigned(IGroupByUserCheckBox) and
    (IGroupByUserCheckBox.ControlState = cbChecked);
  ConnectionList.ActivePage := ActivePage;
  ConnectionList.SavePasswords := SavePasswords;
  ConnectionList.StoreProperties;
end;

procedure TJvBaseDBLogonDialog.TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(ConnectionInfo) and Assigned(DBDialog) then
  begin
    if ConnectionInfo.UsernameEnabled then
      if Options.UsernameCaseSensitive then
        ConnectionInfo.Username := DialogUserName
      else
        ConnectionInfo.Username := UpperCase(DialogUserName)
    else
      ConnectionInfo.Username := '';
    if ConnectionInfo.PasswordEnabled then
      ConnectionInfo.Password := EncryptPassword(DialogPassword)
    else
      ConnectionInfo.Password := '';
    if  ConnectionInfo.DatabaseEnabled  then
      if Options.DatabasenameCaseSensitive then
        ConnectionInfo.Database := DialogDatabase
      else
        ConnectionInfo.Database := UpperCase(DialogDatabase)
    else
      ConnectionInfo.Database := '';
    if Options.ShowAlias and Assigned(IAliasEditData) and ConnectionInfo.AliasEnabled then
      ConnectionInfo.Alias := IAliasEditData.ControlValue;
    if Options.ShowConnectGroup and Assigned(IConnectGroupComboBoxData) then
      ConnectionInfo.Group := IConnectGroupComboBoxData.ControlValue;
    if Options.ShowColors and Assigned(IColorComboBox) then
      ConnectionInfo.Color := IColorComboBox.ControlSelectedColor;
    if Options.ShowShortcuts and Assigned(IShortCutComboBoxData) then
      ConnectionInfo.ShortCutText := IShortCutComboBoxData.ControlValue;
  end;
end;

procedure TJvBaseDBLogonDialog.TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  if Assigned(ConnectionInfo) and Assigned(DBDialog) then
  begin
    DialogUserName := ConnectionInfo.Username;
    if SavePasswords then
      DialogPassword := DecryptPassword(ConnectionInfo.Password);
    DialogDatabase := ConnectionInfo.Database;
    if Options.ShowAlias and Assigned(IAliasEditData) then
      IAliasEditData.ControlValue := ConnectionInfo.Alias;
    if Options.ShowConnectGroup and Assigned(IConnectGroupComboBoxData) then
      IConnectGroupComboBoxData.ControlValue := ConnectionInfo.Group;
    if Options.ShowShortcuts and Assigned(IShortCutComboBoxData) then
      IShortCutComboBoxData.ControlValue := ConnectionInfo.ShortCutText;
    if Options.ShowColors and Assigned(IColorComboBox) then
      IColorComboBox.ControlSelectedColor := ConnectionInfo.Color;
    if (ConnectionInfo.Username = '') and Assigned(UserNameEdit) and UserNameEdit.CanFocus then
      UserNameEdit.SetFocus
    else
      if (ConnectionInfo.Password = '') and Assigned(PasswordEdit) and PasswordEdit.CanFocus then
        PasswordEdit.SetFocus
      else
        if (ConnectionInfo.Database = '') and Assigned(DatabaseComboBox) and DatabaseComboBox.CanFocus  then
          DatabaseComboBox.SetFocus;
  end;
end;

procedure TJvBaseDBLogonDialog.TransferSessionDataFromConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
begin
end;

procedure TJvBaseDBLogonDialog.TransferSessionDataFromDialog;
var
  tmpConnectionInfo: TJvBaseConnectionInfo;
begin
  if not Assigned(DBDialog) then
    Exit;
  tmpConnectionInfo := ConnectionList.CreateConnection;
  try
    TransferConnectionInfoFromDialog(tmpConnectionInfo);
    tmpConnectionInfo.Password := DecryptPassword(tmpConnectionInfo.Password);
    CurrentConnectionInfo.Assign(tmpConnectionInfo);
    if Assigned(BeforeTransferConnectionInfoToSessionData) then
      BeforeTransferConnectionInfoToSessionData(tmpConnectionInfo);
    TransferSessionDataFromConnectionInfo(tmpConnectionInfo);
  finally
    tmpConnectionInfo.Free;
  end;
end;

procedure TJvBaseDBLogonDialog.TransferSessionDataToConnectionInfo(ConnectionInfo: TJvBaseConnectionInfo);
begin
end;

procedure TJvBaseDBLogonDialog.TransferSessionDataToDialog;
var
  tmpConnectionInfo: TJvBaseConnectionInfo;
begin
  if not Assigned(DBDialog) then
    Exit;
  tmpConnectionInfo := ConnectionList.CreateConnection;
  try
    TransferSessionDataToConnectionInfo(tmpConnectionInfo);
    tmpConnectionInfo.Password := EncryptPassword(tmpConnectionInfo.Password);
    if Assigned(AfterTransferSessionDataToConnectionInfo) then
      AfterTransferSessionDataToConnectionInfo(tmpConnectionInfo);
    TransferConnectionInfoToDialog(tmpConnectionInfo);
    CurrentConnectionInfo.Assign(tmpConnectionInfo);
    ValidateConnectBtnEnabled;
  finally
    tmpConnectionInfo.Free;
  end;
end;

procedure TJvBaseDBLogonDialog.ValidateConnectBtnEnabled;
begin
  ConnectBtn.Enabled := IsConnectAllowed;
//  AddToListBtn.Enabled := ConnectBtn.Enabled;
end;

//=== { TJvBaseDBLogonDialogOptions } ========================================

constructor TJvBaseDBLogonDialogOptions.Create;
begin
  inherited Create;
  FShowShortcuts := True;
  FShowConnectionsExport := True;
  FSavePasswords := True;
  FShowColors := False;
  FAddConnectionValuesToComboBox := True;
  FAllowNullPasswords := False;
  FSaveLastConnect := True;
  FSetLastConnectToTop := True;
  FShowConnectGroup := True;
  FShowSavePasswords := False;
  FUsernameCaseSensitive := False;
  FDatabasenameCaseSensitive := False;
  FPasswordChar := '*';
  FAllowPasswordChange := False;
  FPasswordDialogOptions := TJvBaseDBPasswordDialogOptions.Create;
  FShowAlias := false;
  FShowConnectionFilter := true;
end;

destructor TJvBaseDBLogonDialogOptions.Destroy;
begin
  FreeAndNil(FPasswordDialogOptions);
  inherited Destroy;
end;

//=== { TJvBaseDBOracleLogonDialogOptions } ==================================

constructor TJvBaseDBOracleLogonDialogOptions.Create;
begin
  inherited Create;
  FShowConnectAs := True;
end;

procedure TJvBaseDBOracleLogonDialog.ClearControlInterfaceObjects;
begin
  inherited ClearControlInterfaceObjects;
  IConnectAsComboBoxData := nil;
end;

procedure TJvBaseDBOracleLogonDialog.ClearFormControls;
begin
  inherited ClearFormControls;
  if Assigned(IConnectAsComboBoxData) then
    IConnectAsComboBoxData.ControlValue := 'NORMAL';
end;

procedure TJvBaseDBOracleLogonDialog.CreateAdditionalConnectDialogControls(AOwner: TComponent; AParentControl:
    TWinControl);
var
  Items: TStringList;
  IDynControlComboBox: IJvDynControlComboBox;
  IDynControlItems: IJvDynControlItems;
begin
  CreateAdditionalConnectDialogEditPanel(AOwner, AParentControl, 'ConnectAs', RsConnectAs, jctComboBox, ConnectAsPanel, ConnectAsComboBox, IConnectAsComboBoxData, DefaultOnEditChange);
  Items := tStringList.Create;
  try
    Items.Add('NORMAL');
    Items.Add('SYSDBA');
    Items.Add('SYSOPER');
    Items.Add('SYSASM');
    if Supports(ConnectAsComboBox, IJvDynControlItems, IDynControlItems) then
      IDynControlItems.ControlItems.Assign(Items);
    if Supports(ConnectAsComboBox, IJvDynControlComboBox, IDynControlComboBox) then
      IDynControlComboBox.ControlSetNewEntriesAllowed(False);
  finally
    Items.Free;
  end;
  SetPanelHeight(ConnectAsPanel);
  ConnectAsPanel.Visible := Options.ShowConnectAs;
end;

procedure TJvBaseDBOracleLogonDialog.CreateFormControls(AForm: TForm);
begin
  inherited CreateFormControls(AForm);
end;

function TJvBaseDBOracleLogonDialog.GetCurrentConnectionInfo: TJvBaseOracleConnectionInfo;
begin
  Result := TJvBaseOracleConnectionInfo(inherited CurrentConnectionInfo);
end;

class function TJvBaseDBOracleLogonDialog.GetDBLogonConnectionListClass: TJvBaseConnectionListClass;
begin
  Result := TJvBaseOracleConnectionList;
end;

class function TJvBaseDBOracleLogonDialog.GetDBLogonDialogOptionsClass: TJvBaseDBLogonDialogOptionsClass;
begin
  Result := TJvBaseDBOracleLogonDialogOptions;
end;

function TJvBaseDBOracleLogonDialog.GetDialogConnectAs: string;
begin
  if Assigned(IConnectAsComboBoxData) then
    Result := UpperCase(IConnectAsComboBoxData.ControlValue)
  else
    Result := '';
end;

function TJvBaseDBOracleLogonDialog.GetOptions: TJvBaseDBOracleLogonDialogOptions;
begin
  Result := TJvBaseDBOracleLogonDialogOptions(inherited Options);
end;

procedure TJvBaseDBOracleLogonDialog.SetDialogConnectAs(const Value: string);
begin
  if Assigned(IConnectAsComboBoxData) then
    IConnectAsComboBoxData.ControlValue := Value;
end;

procedure TJvBaseDBOracleLogonDialog.SetEditPanelsTabOrder;
begin
  inherited SetEditPanelsTabOrder;
  if Assigned(ConnectAsPanel) then
    ConnectAsPanel.TabOrder := 3;
end;

procedure TJvBaseDBOracleLogonDialog.SetEditPanelsVisibility;
begin
  inherited SetEditPanelsVisibility;
  SetPanelVisible(ConnectAsPanel, CurrentConnectionInfo.ConnectAsEnabled and Options.ShowConnectAs);
end;

procedure TJvBaseDBOracleLogonDialog.SetOptions(const Value: TJvBaseDBOracleLogonDialogOptions);
begin
  (inherited Options).Assign(Value);
end;

procedure TJvBaseDBOracleLogonDialog.TransferConnectionInfoFromDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoFromDialog(ConnectionInfo);
  if Assigned(ConnectionInfo) and (ConnectionInfo is TJvBaseOracleConnectionInfo) and Assigned(IConnectAsComboBoxData) then
  begin
    if Options.ShowConnectAs then
      TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs := IConnectAsComboBoxData.ControlValue;
  end;
end;

procedure TJvBaseDBOracleLogonDialog.TransferConnectionInfoToDialog(ConnectionInfo: TJvBaseConnectionInfo);
begin
  inherited TransferConnectionInfoToDialog(ConnectionInfo);
  if Assigned(ConnectionInfo) and (ConnectionInfo is TJvBaseOracleConnectionInfo) and Assigned(IConnectAsComboBoxData) then
  begin
    if Options.ShowConnectAs then
      IConnectAsComboBoxData.ControlValue := TJvBaseOracleConnectionInfo(ConnectionInfo).ConnectAs;
  end;
end;

constructor TJvBaseConnectionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := cDefaultColorComboBoxColor;
end;

//=== { TJvBaseConnectionInfo } ==============================================

destructor TJvBaseConnectionInfo.Destroy;
begin
  inherited Destroy;
end;

function TJvBaseConnectionInfo.IsConnectAllowed(AllowNullPasswords: Boolean): Boolean;
begin
  if AllowNullPasswords then
    Result := (not UsernameEnabled or (UserName <> '')) and (not DatabaseEnabled or (Database <> ''))
  else
    Result := (not UsernameEnabled or (UserName <> '')) and (not PasswordEnabled or (Password <> '')) and (not DatabaseEnabled or (Database <> ''));
end;

function TJvBaseConnectionInfo.ConnectString: string;
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
    if DatabaseEnabled and (Database <> '') then
      Result := Result + '@' + TranslateDatabaseName(Database);
  if ShortCutText <> '' then
    Result := Result + ' ('+ShortCutText+')';
end;

function TJvBaseConnectionInfo.DatabaseGroupIdentifier: string;
begin
  if AliasEnabled and (Alias <> '') then
    Result := Alias
  else
    Result := TranslateDatabaseName(Database);
end;

function TJvBaseConnectionInfo.GetDatabaseEnabled: Boolean;
begin
  Result := True;
end;

function TJvBaseConnectionInfo.GetAliasEnabled: Boolean;
begin
  Result := DatabaseEnabled;
end;

function TJvBaseConnectionInfo.GetUsernameEnabled: Boolean;
begin
  Result := True;
end;

function TJvBaseConnectionInfo.GetPasswordEnabled: Boolean;
begin
  Result := True;
end;

function TJvBaseConnectionInfo.GetShortCutText: string;
begin
  Result := ShortCutToText(ShortCut);
end;

function TJvBaseConnectionInfo.MatchesFilter(iFilterValue: String): Boolean;
begin
  if iFilterValue = '' then
    Result := True
  else
    Result := AnsiContainsText(Alias, iFilterValue) or
              AnsiContainsText(Database, iFilterValue) or
              AnsiContainsText(Group, iFilterValue)or
              AnsiContainsText(Username, iFilterValue);
end;

function TJvBaseConnectionInfo.SearchName: String;
begin
  Result := TranslateUserName(UserName)+'@'+TranslateDatabaseName(Database);
end;

procedure TJvBaseConnectionInfo.SetDatabase(Value: string);
begin
  FDatabase := Trim(Value);
end;

procedure TJvBaseConnectionInfo.SetGroup(const Value: string);
begin
  FGroup := Trim(Value);
end;

procedure TJvBaseConnectionInfo.SetSavePassword(const Value: Boolean);
const
  cPassword = 'Password';
begin
  FSavePassword := Value;
  if Value then
  begin
    if IgnoreProperties.IndexOf(cPassword) >= 0 then
      IgnoreProperties.Delete(IgnoreProperties.IndexOf(cPassword))
  end
  else
    if IgnoreProperties.IndexOf(cPassword) < 0 then
      IgnoreProperties.Add(cPassword);
end;

procedure TJvBaseConnectionInfo.SetShortCutText(const Value: string);
begin
  try
    ShortCut := TextToShortCut(Value);
  except
    ShortCut := 0;
  end;
end;

procedure TJvBaseConnectionInfo.SetUsername(Value: string);
begin
  fUserName := Trim(Value);
end;

function TJvBaseConnectionInfo.TranslateUserName(iName: string): string;
// CharIsUpper and CharIsLower are not allowed to use. Only basic ASCII characters are supported for database name generation without "
const UpperChars : set of ansichar = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                                      '0','1','2','3','4','5','6','7','8','9','_','$','#','@'];
const LowerChars : set of ansichar = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];
var i : Integer;
    s : String;
begin
  s := trim(iName);
  if UseTranslateUserName and (s <> '') then
    if (s[1] <> '"') or (s[length(s)] <> '"') then
      for i := 1 to length(s) do
      begin
        if CharInSet(s[i], LowerChars) then
          {$IFDEF RTL250_UP}
          s[i] := s[i].ToUpper
          {$ELSE}
          s[i] := ToUpper(s[i])
          {$ENDIF RTL250_UP}
        else if not CharInSet(s[i], UpperChars) then
        begin
          Result := Trim(iName);
          Exit;
        end;
      end;
  Result := s;
end;

function TJvBaseConnectionInfo.TranslateDatabaseName(iName: string): string;
// CharIsUpper and CharIsLower are not allowed to use. Only basic ASCII characters are supported for database name generation without "
const UpperChars : set of ansichar = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                                      '0','1','2','3','4','5','6','7','8','9','_','$','#','@','.'];
const LowerChars : set of ansichar = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];
var i : Integer;
    s : String;
begin
  s := trim(iName);
  if UseTranslateDatabaseName and (s <> '') then
    if (s[1] <> '"') or (s[length(s)] <> '"') then
      for i := 1 to length(s) do
      begin
        if CharInSet(s[i], LowerChars) then
          {$IFDEF RTL250_UP}
          s[i] := s[i].ToUpper
          {$ELSE}
          s[i] := ToUpper(s[i])
          {$ENDIF RTL250_UP}
        else if not CharInSet(s[i], UpperChars) then
        begin
          Result := Trim(iName);
          Exit;
        end;
      end;
  Result := s;
end;

function TJvBaseConnectionInfo.UseTranslateUserName: Boolean;
begin
  Result := True;
end;

function TJvBaseConnectionInfo.UseTranslateDatabaseName: Boolean;
begin
  Result := True;
end;

//=== { TJvBaseOracleConnectionInfo } ========================================

constructor TJvBaseOracleConnectionInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectAs := 'NORMAL';
end;

function TJvBaseOracleConnectionInfo.ConnectString: string;
begin
  Result := inherited ConnectString;
  if ConnectAsEnabled and (ConnectAs <> 'NORMAL') then
    Result := Result + ' [' + ConnectAs + ']';
end;

function TJvBaseOracleConnectionInfo.GetConnectAsEnabled: Boolean;
begin
  Result := True;
end;

procedure TJvBaseOracleConnectionInfo.SetConnectAs(const Value: string);
begin
  FConnectAs := Trim(UpperCase(Value));
end;

//=== { TJvBaseConnectionList } ==============================================

constructor TJvBaseConnectionList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLastConnect := TJvBaseConnectionInfo(CreateObject);
  FLastConnect.SavePassword := False;
  ItemName := RsConnectionListItemName;
end;

destructor TJvBaseConnectionList.Destroy;
begin
  FreeAndNil(FLastConnect);
  inherited Destroy;
end;

procedure TJvBaseConnectionList.AddConnection(ConnectionInfo: TJvBaseConnectionInfo);
var
  p, p2, i: Integer;
begin
  p := Items.IndexOf(ConnectionInfo.SearchName);
  while p <> -1 do
  begin
    Items.Delete(p);
    p := Items.IndexOf(ConnectionInfo.SearchName);
  end;
  p2 := Items.AddObject(ConnectionInfo.SearchName, ConnectionInfo);
  if ConnectionInfo.ShortCut > 0 then
    for i := 0 to Count - 1 do
      if i <> p2 then
        if Connection[i].ShortCut = ConnectionInfo.ShortCut then
          Connection[i].ShortCut := 0;
  Items.Move(p2, 0);
end;

procedure TJvBaseConnectionList.CopyContents(iConnectionList: TJvBaseConnectionList; iClearBefore: Boolean);
var
  i: Integer;
  Connection: TJvBaseConnectionInfo;
begin
  if iClearBefore then
    Clear;
  if not Assigned(iConnectionList) then
    Exit;
  for i := 0 to iConnectionList.Items.Count - 1 do
  begin
    Connection := CreateConnection;
    Connection.Assign(iConnectionList.Connection[i]);
    AddConnection(Connection);
  end;
end;

function TJvBaseConnectionList.CreateConnection: TJvBaseConnectionInfo;
begin
  Result := TJvBaseConnectionInfo(CreateObject);
end;

function TJvBaseConnectionList.CreateObject: TPersistent;
begin
  Result := TJvBaseConnectionInfo.Create(Self);
end;

function TJvBaseConnectionList.GetConnection(I: Longint): TJvBaseConnectionInfo;
begin
  if (i >= 0) and (i < Count) then
    Result := TJvBaseConnectionInfo(Objects[i])
  else
    Result := nil;
end;

procedure TJvBaseConnectionList.LoadData;
var
  i: Integer;
begin
  inherited LoadData;
  for i := 0 to Items.Count - 1 do
    Items[i] := Connection[i].SearchName;
end;

procedure TJvBaseConnectionList.SetLastConnect(const Value: TJvBaseConnectionInfo);
begin
  FLastConnect := Value;
end;

procedure TJvBaseConnectionList.SetSavePasswords(const Value: Boolean);
var
  i: Integer;
begin
  FSavePasswords := Value;
  for i := 0 to Count - 1 do
    Connection[i].SavePassword := Value;
end;

function TJvBaseOracleConnectionList.CreateObject: TPersistent;
begin
  Result := TJvBaseOracleConnectionInfo.Create(Self);
  TJvBaseOracleConnectionInfo(Result).SavePassword := SavePasswords;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
