{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developers of the Original Code is: Jens Fudickar
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppStorageSelectList;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, Controls, Forms,
  JvComponentBase, JvTypes, JvAppStorage, JvDynControlEngine,
  JvDynControlEngineIntf;

type
  TJvAppStorageSelectListOperation = (sloLoad, sloStore, sloManage);

  TJvBaseAppStorageSelectList = class;
  TJvBaseAppStorageSelectListDialogInstance = class(TJvComponent)
  private
    FAppStorageSelectList: TJvBaseAppStorageSelectList;
    FCaption: String;
    FDynControlEngine: TJvDynControlEngine;
    FOperation: TJvAppStorageSelectListOperation;
    FSelectDialog: TForm;
    function GetModalResult: TModalResult;
  protected
    function GetDynControlEngine: TJvDynControlEngine; virtual;
    property SelectDialog: TForm read FSelectDialog write FSelectDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateControls(AOperation: TJvAppStorageSelectListOperation; ACaption: string = ''): TForm; virtual; abstract;
    function DialogResultValue: String; virtual; abstract;
    procedure ShowModal;
    property ModalResult: TModalResult read GetModalResult;
    property AppStorageSelectList: TJvBaseAppStorageSelectList read FAppStorageSelectList write FAppStorageSelectList;
    property Caption: String read FCaption write FCaption;
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write FDynControlEngine;
    property Operation: TJvAppStorageSelectListOperation read FOperation write FOperation;
  end;

  TJvBaseAppStorageSelectListDialogInstanceClass = class of TJvBaseAppStorageSelectListDialogInstance;

  TJvBaseAppStorageSelectListDialog = class(TJvComponent)
  private
    FDynControlEngine: TJvDynControlEngine;
  protected
    function DialogInstanceClass: TJvBaseAppStorageSelectListDialogInstanceClass; virtual; abstract;
    function GetDynControlEngine: TJvDynControlEngine; virtual;
  public
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write FDynControlEngine;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAppStorageSelectListEasyDialog = class(TJvBaseAppStorageSelectListDialog)
  protected
    function DialogInstanceClass: TJvBaseAppStorageSelectListDialogInstanceClass; override;
  end;

  TJvAppStorageSelectListEasyDialogInstance = class(TJvBaseAppStorageSelectListDialogInstance)
  private
    FIComboBoxData: IJvDynControlData;
    FIComboBoxItems: IJvDynControlItems;
    FIListBoxData: IJvDynControlData;
    FIListBoxItems: IJvDynControlItems;
  protected
    procedure DialogOnCancelButtonClick(Sender: TObject);
    procedure DialogOnDestroy(Sender: TObject);
    procedure DialogOnListBoxChange(Sender: TObject);
    procedure DialogOnOkButtonClick(Sender: TObject);
    property IComboBoxData: IJvDynControlData read FIComboBoxData write FIComboBoxData;
    property IComboBoxItems: IJvDynControlItems read FIComboBoxItems write FIComboBoxItems;
    property IListBoxData: IJvDynControlData read FIListBoxData write FIListBoxData;
    property IListBoxItems: IJvDynControlItems read FIListBoxItems write FIListBoxItems;
  public
    function CreateControls(AOperation: TJvAppStorageSelectListOperation; ACaption: string = ''): TForm; override;
    function DialogResultValue: String; override;
  end;

  TJvBaseAppStorageSelectList = class(TJvComponent)
  private
    FAppStorage: TJvCustomAppStorage;
    FCheckEntries: Boolean;
    FSelectList: TStringList;
    FSelectListDialog: TJvBaseAppStorageSelectListDialog;
    FSelectPath: string;
    procedure SetSelectListDialog(const Value: TJvBaseAppStorageSelectListDialog);
  protected
    function CreateSelectListDialogInstance(AOwner: TComponent;AOperation: TJvAppStorageSelectListOperation; ACaption:
        string = ''): TJvBaseAppStorageSelectListDialogInstance; virtual;
    function GetAppStorage: TJvCustomAppStorage; virtual;
    function GetSelectList: TStrings; virtual;
    function GetStoragePath: string; virtual;
    procedure LoadSelectList;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetAppStorage(Value: TJvCustomAppStorage); virtual;
    procedure SetSelectList(const Value: TStrings); virtual;
    procedure SetSelectPath(Value: string);
    procedure StoreSelectList;
    property SelectList: TStrings read GetSelectList write SetSelectList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddEntry(iName: string);
    procedure DeleteEntry(iName: string; iDeletePath: Boolean = true);
    function GetSelectListPath(AOperation: TJvAppStorageSelectListOperation; ACaption: string = ''): string;
    procedure ManageSelectList(ACaption: string = '');
    function StorageNamePath(iName : String): string;
    property AppStorage: TJvCustomAppStorage read GetAppStorage write SetAppStorage;
    property CheckEntries: Boolean read FCheckEntries write FCheckEntries default True;
    property SelectListDialog: TJvBaseAppStorageSelectListDialog read FSelectListDialog write SetSelectListDialog;
    property SelectPath: string read FSelectPath write SetSelectPath;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvAppStorageSelectList = class(TJvBaseAppStorageSelectList)
  published
    property AppStorage;
    property CheckEntries;
    property SelectListDialog;
    property SelectPath;
  end;



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

uses
  SysUtils,
  JvResources, JvJVCLUtils;

function TJvAppStorageSelectListEasyDialogInstance.CreateControls(AOperation: TJvAppStorageSelectListOperation;
    ACaption: string = ''): TForm;
var
  MainPanel, ButtonPanel, ListBoxPanel, ComboBoxPanel: TWinControl;
  OkButton, CancelButton: TWinControl;
  ComboBox: TWinControl;
  ListBox: TWinControl;
  ITmpPanel: IJvDynControlPanel;
  ITmpControl: IJvDynControl;
  ITmpControlCaption: IJvDynControlCaption;
  ITmpComboBox: IJvDynControlComboBox;
  ITmpDblClick: IJvDynControlDblClick;
  FDialog: TForm;
begin
  if not Assigned(DynControlEngine) then
    raise EJVCLException.CreateRes(@RsEDynControlEngineNotDefined);

  Operation := AOperation;

  FDialog := TForm(DynControlEngine.CreateForm('', ''));

  FDialog.BorderIcons := [];
  FDialog.DefaultMonitor := dmActiveForm;
  FDialog.BorderStyle := bsDialog;
  FDialog.FormStyle := fsNormal;
  {$IFDEF COMPILER7_UP}
  FDialog.Position := poOwnerFormCenter;
  {$ELSE}
  FDialog.Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
  FDialog.OnDestroy := DialogOnDestroy;

  if ACaption <> '' then
    FDialog.Caption := ACaption
  else
    case Operation of
      sloLoad:
        FDialog.Caption := RsLoadSettings;
      sloStore:
        FDialog.Caption := RsSaveSettings;
      sloManage:
        FDialog.Caption := RsDeleteSettings;
    end;

  MainPanel := DynControlEngine.CreatePanelControl(Self, FDialog, 'MainPanel', '', alClient);
  ButtonPanel := DynControlEngine.CreatePanelControl(Self, FDialog, 'ButtonPanel', '', alBottom);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton',
    RsButtonOKCaption, '', DialogOnOkButtonClick, True, False);
  if Operation <> sloStore then
    OkButton.Enabled := AppStorageSelectList.SelectList.Count > 0;
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton',
    RsButtonCancelCaption, '', DialogOnCancelButtonClick, False, True);
  ButtonPanel.Height := OkButton.Height + 10;
  CancelButton.Top := 5;
  CancelButton.Left := ButtonPanel.Width - 5 - CancelButton.Width;
  CancelButton.Anchors := [akTop, akRight];
  OkButton.Top := 5;
  OkButton.Left := CancelButton.Left - 10 - OkButton.Width;
  OkButton.Anchors := [akTop, akRight];

  ComboBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ComboBoxPanel', '', alBottom);
  IntfCast(ComboBoxPanel, IJvDynControlPanel, ITmpPanel);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);
  ListBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ListPanel', '', alClient);
  IntfCast(ListBoxPanel, IJvDynControlPanel, ITmpPanel);
  ITmpPanel.ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  ComboBox := DynControlEngine.CreateComboBoxControl(Self, ComboBoxPanel, 'ComboBox', AppStorageSelectList.SelectList);
  IntfCast(ComboBox, IJvDynControlItems, FIComboBoxItems);
  IntfCast(ComboBox, IJvDynControlData, FIComboBoxData);

  IComboBoxItems.ControlSetSorted(True);
  if Supports(ComboBox, IJvDynControlComboBox, ITmpComboBox) then
    case AOperation of
      sloLoad:
        ITmpComboBox.ControlSetNewEntriesAllowed(False);
      sloStore:
        ITmpComboBox.ControlSetNewEntriesAllowed(True);
      sloManage:
        ITmpComboBox.ControlSetNewEntriesAllowed(False);
    end;

  IComboBoxData.ControlValue := '';

  ListBox := DynControlEngine.CreateListBoxControl(Self, ListBoxPanel, 'ListBox', AppStorageSelectList.SelectList);
  Supports(ListBox, IJvDynControlItems, FIListBoxItems);
  Supports(ListBox, IJvDynControl, ITmpControl);
  Supports(ListBox, IJvDynControlData, FIListBoxData);
  ITmpControl.ControlSetOnClick(DialogOnListBoxChange);
  FIListBoxData.ControlSetOnChange(DialogOnListBoxChange);
  if Supports(ListBox, IJvDynControlDblClick, ITmpDblClick) then  // ListBox instead of ListBox.ClassType and ITmpControl are needed here for D5/C5 support (obones)
    ITmpDblClick.ControlSetOnDblClick(DialogOnOkButtonClick);

  ComboBoxPanel.Height := ComboBox.Height + 12;
  ListBox.Align := alClient;
  ComboBox.Align := alClient;

  IntfCast(OkButton, IJvDynControlCaption, ITmpControlCaption);
  case AOperation of
    sloLoad:
      ITmpControlCaption.ControlSetCaption(RsLoadCaption);
    sloStore:
      ITmpControlCaption.ControlSetCaption(RsSaveCaption);
    sloManage:
      ITmpControlCaption.ControlSetCaption(RsDeleteCaption);
  end;
  Result := FDialog;
end;

procedure TJvAppStorageSelectListEasyDialogInstance.DialogOnCancelButtonClick(Sender: TObject);
begin
  SelectDialog.ModalResult := mrCancel;
end;

procedure TJvAppStorageSelectListEasyDialogInstance.DialogOnDestroy(Sender: TObject);
begin
  FIComboBoxItems := nil;
  FIComboBoxData := nil;
  FIListBoxItems := nil;
  FIListBoxData := nil;
end;

procedure TJvAppStorageSelectListEasyDialogInstance.DialogOnListBoxChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := IListBoxData.ControlValue;
  if (Index >= 0) and (Index < IListBoxItems.ControlItems.Count) then
    IComboBoxData.ControlValue := IListBoxItems.ControlItems[Index];
end;

procedure TJvAppStorageSelectListEasyDialogInstance.DialogOnOkButtonClick(Sender: TObject);
var
  Value: string;
begin
  Value := IComboBoxData.ControlValue;
  if Operation = sloStore then
    SelectDialog.ModalResult := mrOk
  else
    if AppStorageSelectList.SelectList.IndexOf(Value) >= 0 then
      SelectDialog.ModalResult := mrOk;
end;

function TJvAppStorageSelectListEasyDialogInstance.DialogResultValue: String;
begin
  Result := IComboBoxData.ControlValue;
end;

constructor TJvBaseAppStorageSelectList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCheckEntries := True;
  FSelectList := TStringList.Create;
end;

destructor TJvBaseAppStorageSelectList.Destroy;
begin
  FreeAndNil(FSelectList);
  inherited Destroy;
end;

procedure TJvBaseAppStorageSelectList.AddEntry(iName: string);
begin
  if SelectList.IndexOf(iName) < 0 then
  begin
    SelectList.Add(iName);
    StoreSelectList;
  end;
end;

function TJvBaseAppStorageSelectList.CreateSelectListDialogInstance(AOwner: TComponent;AOperation:
    TJvAppStorageSelectListOperation; ACaption: string = ''): TJvBaseAppStorageSelectListDialogInstance;
begin
  if Assigned(FSelectListDialog) then
  begin
    Result := FSelectListDialog.DialogInstanceClass.Create(AOwner);
    Result.DynControlEngine := FSelectListDialog.DynControlEngine;
  end
  else
    Result := TJvAppStorageSelectListEasyDialogInstance.Create(AOwner);
  Result.AppStorageSelectList := Self;
  Result.Operation := AOperation;
  Result.Caption := ACaption;
end;

procedure TJvBaseAppStorageSelectList.DeleteEntry(iName: string; iDeletePath: Boolean = true);
begin
  if SelectList.IndexOf(iName) >= 0 then
  begin
    SelectList.Delete(SelectList.IndexOf(iName));
    AppStorage.BeginUpdate;
    try
      if iDeletePath then
        AppStorage.DeleteSubTree(StorageNamePath(iName));
      StoreSelectList;
    finally
      AppStorage.EndUpdate;
    end;
  end;
end;

function TJvBaseAppStorageSelectList.GetAppStorage: TJvCustomAppStorage;
begin
  Result := FAppStorage;
end;

function TJvBaseAppStorageSelectList.GetSelectList: TStrings;
begin
  Result := FSelectList;
end;

function TJvBaseAppStorageSelectList.GetSelectListPath(AOperation: TJvAppStorageSelectListOperation; ACaption: string =
    ''): string;
var
  SelectDialog: TJvBaseAppStorageSelectListDialogInstance;
begin
  if not Assigned(AppStorage) then
    raise EJVCLException.CreateRes(@RsEDynAppStorageNotDefined);
  try
    LoadSelectList;
    SelectDialog := CreateSelectListDialogInstance(self, AOperation, ACaption);
    SelectDialog.ShowModal;
    if (SelectDialog.ModalResult = mrOk) and (SelectDialog.DialogResultValue <> '') then
    begin
      case AOperation of
        sloLoad:
          Result := StorageNamePath(SelectDialog.DialogResultValue);
        sloStore:
          begin
            AddEntry(SelectDialog.DialogResultValue);
            Result := StorageNamePath(SelectDialog.DialogResultValue);
          end;
        sloManage:
          begin
            DeleteEntry(SelectDialog.DialogResultValue);
            Result := StorageNamePath(SelectDialog.DialogResultValue);
          end;
      end;
    end;
  finally
    FreeAndNil(SelectDialog);
  end;
end;

function TJvBaseAppStorageSelectList.GetStoragePath: string;
begin
  Result := SelectPath;
end;

procedure TJvBaseAppStorageSelectList.LoadSelectList;
var
  I: Integer;
begin
  if Assigned(AppStorage) then
  begin
    AppStorage.BeginUpdate;
    try
      AppStorage.ReadStringList(GetStoragePath, FSelectList, True);
      if CheckEntries then
        for I := FSelectList.Count - 1 downto 0 do
          if not AppStorage.PathExists(AppStorage.ConcatPaths ([GetStoragePath,FSelectList[I]])) then
            FSelectList.Delete(I);
    finally
      AppStorage.EndUpdate;
    end;
  end;
end;

procedure TJvBaseAppStorageSelectList.ManageSelectList(ACaption: string = '');
begin
  GetSelectListPath(sloManage, ACaption);
end;

procedure TJvBaseAppStorageSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
    if (AComponent = FSelectListDialog) then
      FSelectListDialog := nil
    else if (AComponent = FAppStorage) then
      FAppStorage := nil;
end;

procedure TJvBaseAppStorageSelectList.SetAppStorage(Value: TJvCustomAppStorage);
begin
  ReplaceComponentReference(Self, Value, TComponent(FAppStorage));
end;

procedure TJvBaseAppStorageSelectList.SetSelectList(const Value: TStrings);
begin
  FSelectList.Assign(Value);
end;

procedure TJvBaseAppStorageSelectList.SetSelectListDialog(const Value: TJvBaseAppStorageSelectListDialog);
begin
  ReplaceComponentReference(Self, Value, TComponent(FSelectListDialog));
end;

procedure TJvBaseAppStorageSelectList.SetSelectPath(Value: string);
begin
  FSelectPath := Value;
end;

function TJvBaseAppStorageSelectList.StorageNamePath(iName : String): string;
begin
  Result := AppStorage.ConcatPaths ([GetStoragePath,iName]);
end;

procedure TJvBaseAppStorageSelectList.StoreSelectList;
begin
  if Assigned(AppStorage) then
    AppStorage.WriteStringList(GetStoragePath, FSelectList);
end;

constructor TJvBaseAppStorageSelectListDialogInstance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectDialog := nil;
end;

destructor TJvBaseAppStorageSelectListDialogInstance.Destroy;
begin
  FreeAndNil(FSelectDialog);
  inherited Destroy;
end;

function TJvBaseAppStorageSelectListDialogInstance.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

function TJvBaseAppStorageSelectListDialogInstance.GetModalResult: TModalResult;
begin
  Result := FSelectDialog.ModalResult;
end;

procedure TJvBaseAppStorageSelectListDialogInstance.ShowModal;
begin
  FreeAndNil(FSelectDialog);
  FSelectDialog := CreateControls (Operation, Caption);
  FSelectDialog.ShowModal;
end;

function TJvAppStorageSelectListEasyDialog.DialogInstanceClass: TJvBaseAppStorageSelectListDialogInstanceClass;
begin
  Result := TJvAppStorageSelectListEasyDialogInstance;
end;

function TJvBaseAppStorageSelectListDialog.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
