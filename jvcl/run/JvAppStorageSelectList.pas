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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppStorageSelectList;

{$I jvcl.inc}

interface

uses
  Classes, Controls, Forms,
  JvComponent, JvTypes, JvAppStorage, JvDynControlEngine,
  JvDynControlEngineIntf;

type
  TJvAppStorageSelectListOperation = (sloLoad, sloStore, sloManage);

  TJvAppStorageSelectList = class(TJvComponent)
  private
    FSelectDialog: TForm;
    FListBox: TWinControl;
    FIListBoxItems: IJvDynControlItems;
    FIListBoxData: IJvDynControlData;
    FComboBox: TWinControl;
    FIComboBoxItems: IJvDynControlItems;
    FIComboBoxData: IJvDynControlData;
    FAppStorage: TJvCustomAppStorage;
    FSelectPath: string;
    FDynControlEngine: TJvDynControlEngine;
    FSelectList: TStringList;
    FOperation: TJvAppStorageSelectListOperation;
    FCheckEntries: Boolean;
  protected
    function GetSelectList: TStrings; virtual;
    procedure SetSelectList(const Value: TStrings); virtual;
    function GetAppStorage: TJvCustomAppStorage; virtual;
    procedure SetAppStorage(Value: TJvCustomAppStorage); virtual;
    procedure SetSelectPath(Value: string);
    function GetStoragePath: string; virtual;
    function GetDynControlEngine: TJvDynControlEngine; virtual;
    procedure SetDynControlEngine(Value: TJvDynControlEngine); virtual;
    {$IFDEF BCB}
    procedure CreateDlg(AOperation: TJvAppStorageSelectListOperation; ACaption: string = '');
    {$ELSE}
    procedure CreateDialog(AOperation: TJvAppStorageSelectListOperation; ACaption: string = '');
    {$ENDIF BCB}

    procedure DialogOnOkButtonClick(Sender: TObject);
    procedure DialogOnCancelButtonClick(Sender: TObject);
    procedure DialogOnListBoxChange(Sender: TObject);
    procedure SelectFormDestroying(Sender: TObject);

    procedure LoadSelectList;
    procedure StoreSelectList;
    property SelectDialog: TForm read FSelectDialog write FSelectDialog;
    property ListBox: TWinControl read FListBox write FListBox;
    property IListBoxItems: IJvDynControlItems read FIListBoxItems write FIListBoxItems;
    property IListBoxData: IJvDynControlData read FIListBoxData write FIListBoxData;
    property ComboBox: TWinControl read FComboBox write FComboBox;
    property IComboBoxItems: IJvDynControlItems read FIComboBoxItems write FIComboBoxItems;
    property IComboBoxData: IJvDynControlData read FIComboBoxData write FIComboBoxData;
    property SelectList: TStrings read GetSelectList write SetSelectList;
    property Operation: TJvAppStorageSelectListOperation read FOperation write FOperation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetSelectListPath(AOperation: TJvAppStorageSelectListOperation; ACaption: string = ''): string;
    procedure ManageSelectList(ACaption: string = '');
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
  published
    property AppStorage: TJvCustomAppStorage read GetAppStorage write SetAppStorage;
    property SelectPath: string read FSelectPath write SetSelectPath;
    property CheckEntries: Boolean read FCheckEntries write FCheckEntries default True;
  end;

implementation

uses
  SysUtils,
  JvConsts, JvResources;

constructor TJvAppStorageSelectList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectList := TStringList.Create;
  FCheckEntries := True;
  FSelectDialog := nil;
end;

destructor TJvAppStorageSelectList.Destroy;
begin
  FreeAndNil(FSelectList);
  FreeAndNil(FSelectDialog);
  inherited Destroy;
end;

procedure TJvAppStorageSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAppStorage) then
    FAppStorage := nil;
end;

function TJvAppStorageSelectList.GetSelectList: TStrings;
begin
  Result := FSelectList;
end;

procedure TJvAppStorageSelectList.SetSelectList(const Value: TStrings);
begin
  FSelectList.Assign(Value);
end;

function TJvAppStorageSelectList.GetAppStorage: TJvCustomAppStorage;
begin
  Result := FAppStorage;
end;

procedure TJvAppStorageSelectList.SetAppStorage(Value: TJvCustomAppStorage);
begin
  FAppStorage := Value;
end;

procedure TJvAppStorageSelectList.SetSelectPath(Value: string);
begin
  FSelectPath := Value;
end;

function  TJvAppStorageSelectList.GetStoragePath : string;
begin
  Result := SelectPath;
end;

function TJvAppStorageSelectList.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

procedure TJvAppStorageSelectList.SetDynControlEngine(Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvAppStorageSelectList.DialogOnOkButtonClick(Sender: TObject);
var
  Value: string;
begin
  Value := IComboBoxData.ControlValue;
  if Operation = sloStore then
    SelectDialog.ModalResult := mrOk
  else
    if SelectList.IndexOf(Value) >= 0 then
      SelectDialog.ModalResult := mrOk;
end;

procedure TJvAppStorageSelectList.DialogOnCancelButtonClick(Sender: TObject);
begin
  SelectDialog.ModalResult := mrCancel;
end;

procedure TJvAppStorageSelectList.DialogOnListBoxChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := IListBoxData.ControlValue;
  if (Index >= 0) and (Index < IListBoxItems.ControlItems.Count) then
    IComboBoxData.ControlValue := IListBoxItems.ControlItems[Index];
end;

procedure TJvAppStorageSelectList.SelectFormDestroying(Sender: TObject);
begin
  FIComboBoxItems := nil;
  FIComboBoxData := nil;
  FIListBoxItems := nil;
  FIListBoxData := nil;
end;

{$IFDEF BCB}
procedure TJvAppStorageSelectList.CreateDlg(AOperation: TJvAppStorageSelectListOperation; ACaption: string = '');
{$ELSE}
procedure TJvAppStorageSelectList.CreateDialog(AOperation: TJvAppStorageSelectListOperation; ACaption: string = '');
{$ENDIF BCB}
var
  MainPanel, ButtonPanel, ListBoxPanel, ComboBoxPanel: TWinControl;
  OkButton, CancelButton: TWinControl;
  ITmpPanel: IJvDynControlPanel;
  ITmpControl: IJvDynControl;
  ITmpComboBox: IJvDynControlComboBox;
begin
  if not Assigned(DynControlEngine) then
    raise EJVCLException.CreateRes(@RsEDynControlEngineNotDefined);

  Operation := AOperation;
  FreeAndNil(FSelectDialog);

  FSelectDialog := TForm(DynControlEngine.CreateForm('', ''));

  with SelectDialog do
  begin
    BorderIcons := [];
    {$IFDEF VCL}
    DefaultMonitor := dmActiveForm;
    BorderStyle := bsDialog;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    BorderStyle := fbsDialog;
    {$ENDIF VisualCLX}
    FormStyle := fsNormal;
    Position := poScreenCenter;
    OnDestroy := SelectFormDestroying;
  end;

  if ACaption <> '' then
    SelectDialog.Caption := ACaption
  else
    case Operation of
      sloLoad:
        SelectDialog.Caption := RsLoadSettings;
      sloStore:
        SelectDialog.Caption := RsSaveSettings;
      sloManage:
        SelectDialog.Caption := RsDeleteSettings;
    end;

  MainPanel := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'MainPanel', '', alClient);
  ButtonPanel := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'ButtonPanel', '', alBottom);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton',
    RsButtonOKCaption, '', DialogOnOkButtonClick, True, False);
  if Operation <> sloStore then
    OkButton.Enabled := SelectList.Count > 0;
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
  if not Supports(ComboBoxPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);
  ListBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ListPanel', '', alClient);
  if not Supports(ListBoxPanel, IJvDynControlPanel, ITmpPanel) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  ComboBox := DynControlEngine.CreateComboBoxControl(Self, ComboBoxPanel, 'ComboBox', SelectList);
  if not Supports(ComboBox, IJvDynControlItems, FIComboBoxItems) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  if not Supports(ComboBox, IJvDynControlData, FIComboBoxData) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);

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

  ListBox := DynControlEngine.CreateListBoxControl(Self, ListBoxPanel, 'ListBox', SelectList);
  Supports(ListBox, IJvDynControlItems, FIListBoxItems);
  Supports(ListBox, IJvDynControlData, FIListBoxData);
  with IListBoxItems as IJvDynControl do
    ControlSetOnClick(DialogOnListBoxChange);
  with IListBoxItems as IJvDynControlData do
    ControlSetOnChange(DialogOnListBoxChange);
  if Supports(ListBox.ClassType, IJvDynControlDblClick) then
    with IListBoxItems as IJvDynControlDblClick do
      ControlSetOnDblClick(DialogOnOkButtonClick);

  ComboBoxPanel.Height := ComboBox.Height + 10;
  ListBox.Align := alClient;
  ComboBox.Align := alClient;

  if not Supports(OkButton, IJvDynControl, ITmpControl) then
    raise EIntfCastError.CreateRes(@RsEIntfCastError);
  with ITmpControl do
    case AOperation of
      sloLoad:
        ControlSetCaption(RsLoadCaption);
      sloStore:
        ControlSetCaption(RsSaveCaption);
      sloManage:
        ControlSetCaption(RsDeleteCaption);
    end;
end;

function TJvAppStorageSelectList.GetSelectListPath(AOperation: TJvAppStorageSelectListOperation;
  ACaption: string = ''): string;
begin
  if not Assigned(AppStorage) then
    raise EJVCLException.CreateRes(@RsEDynAppStorageNotDefined);
  try
    LoadSelectList;
    {$IFDEF BCB}
    CreateDlg(AOperation, ACaption);
    {$ELSE}
    CreateDialog(AOperation, ACaption);
    {$ENDIF BCB}
    SelectDialog.ShowModal;
    if SelectDialog.ModalResult = mrOk then
    begin
      case AOperation of
        sloLoad:
          Result := AppStorage.ConcatPaths ([GetStoragePath,IComboBoxData.ControlValue]);
        sloStore:
          begin
            if SelectList.IndexOf(IComboBoxData.ControlValue) < 0 then
            begin
              SelectList.Add(IComboBoxData.ControlValue);
              StoreSelectList;
            end;
            Result := AppStorage.ConcatPaths ([GetStoragePath,IComboBoxData.ControlValue]);
          end;
        sloManage:
          begin
            if SelectList.IndexOf(IComboBoxData.ControlValue) >= 0 then
            begin
              SelectList.Delete(SelectList.IndexOf(IComboBoxData.ControlValue));
              StoreSelectList;
            end;
            Result := AppStorage.ConcatPaths ([GetStoragePath,IComboBoxData.ControlValue]);
          end;
      end;
    end;
  finally
    FreeAndNil(FSelectDialog);
  end;
end;

procedure TJvAppStorageSelectList.ManageSelectList(ACaption: string = '');
begin
  GetSelectListPath(sloManage, ACaption);
end;

procedure TJvAppStorageSelectList.LoadSelectList;
var
  I: Integer;
begin
  if Assigned(AppStorage) then
  begin
    AppStorage.ReadStringList(GetStoragePath, FSelectList, True);
    if CheckEntries then
      for I := FSelectList.Count - 1 downto 0 do
        if not AppStorage.PathExists(AppStorage.ConcatPaths ([GetStoragePath,FSelectList[I]])) then
          FSelectList.Delete(I);
  end;
end;

procedure TJvAppStorageSelectList.StoreSelectList;
begin
  if Assigned(AppStorage) then
    AppStorage.WriteStringList(GetStoragePath, FSelectList);
end;

end.

