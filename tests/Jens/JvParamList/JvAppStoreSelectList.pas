{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPlacemnt.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvAppStoreSelectList;

interface

uses  Classes, Controls, Forms,
  JvComponent, JvAppStore, JvDynControlEngine, JvDynControlEngine_Interface;

type
  tJvAppStoreSelectListOperation = (sloLoad, sloStore, sloManage);

  tJvAppStoreSelectList = class (tJvComponent)
  private
    fSelectDialog : TForm;
    fListBox : TWinControl;
    fIListBoxItems : IJvDynControlItems;
    fIListBoxData : IJvDynControlData;
    fComboBox : TWinControl;
    fIComboBoxItems : IJvDynControlItems;
    fIComboBoxData : IJvDynControlData;
    fAppStore : TJvCustomAppStore;
    fSelectPath : string;
    fDynControlEngine : tJvDynControlEngine;
    fSelectList : TStringList;
    fOperation : tJvAppStoreSelectListOperation;
    fCheckEntries : boolean;
  protected
    function GetAppStore : TJvCustomAppStore; virtual;
    procedure SetAppStore(Value : TJvCustomAppStore); virtual;
    procedure SetSelectPath(Value : string);
    function GetDynControlEngine : tJvDynControlEngine; virtual;
    procedure SetDynControlEngine(Value : tJvDynControlEngine); virtual;

    procedure CreateDialog(aOperation : tJvAppStoreSelectListOperation; aCaption : string = '');

    property SelectDialog : TForm Read fSelectDialog Write fSelectDialog;
    property ListBox : TWinControl Read fListBox Write fListBox;
    property IListBoxItems : IJvDynControlItems Read fIListBoxItems Write fIListBoxItems;
    property IListBoxData : IJvDynControlData Read fIListBoxData Write fIListBoxData;
    property ComboBox : TWinControl Read fComboBox Write fComboBox;
    property IComboBoxItems : IJvDynControlItems Read fIComboBoxItems Write fIComboBoxItems;
    property IComboBoxData : IJvDynControlData Read fIComboBoxData Write fIComboBoxData;
    property SelectList : TStringList Read fSelectList Write fSelectList;
    property Operation : tJvAppStoreSelectListOperation Read fOperation Write fOperation;

    procedure OnOkButtonClick(Sender : TObject);
    procedure OnCancelButtonClick(Sender : TObject);
    procedure OnListBoxChange(Sender : TObject);
    procedure SelectFormDestroying(Sender: TObject);

    procedure LoadSelectList;
    procedure StoreSelectList;

  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    property DynControlEngine : tJvDynControlEngine Read GetDynControlEngine Write SetDynControlEngine;
    function GetSelectPath(aOperation : tJvAppStoreSelectListOperation; aCaption : string = '') : string;
    procedure ManageSelectList(aCaption : string = '');
  published

    property AppStore : TJvCustomAppStore Read GetAppStore Write SetAppStore;
    property SelectPath : string Read fSelectPath Write SetSelectPath;
    property CheckEntries : boolean Read fCheckEntries Write fCheckEntries Default true;
  end;

implementation

uses SysUtils;

constructor tJvAppStoreSelectList.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  fSelectList   := TStringList.Create;
  fCheckEntries := true;
end;

destructor tJvAppStoreSelectList.Destroy;
begin
  FreeAndNil(fSelectList);
  if Assigned(fSelectDialog) then
    FreeAndNil(fSelectDialog);
  inherited Destroy;
end;

procedure tJvAppStoreSelectList.Notification(AComponent : TComponent; Operation : TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = fAppStore) then
    fAppStore := nil;
end;

function tJvAppStoreSelectList.GetAppStore : TJvCustomAppStore;
begin
  Result := fAppStore;
end;

procedure tJvAppStoreSelectList.SetAppStore(Value : TJvCustomAppStore);
begin
  fAppStore := Value;
end;

procedure tJvAppStoreSelectList.SetSelectPath(Value : string);
begin
  fSelectPath := Value;
end;

function tJvAppStoreSelectList.GetDynControlEngine : tJvDynControlEngine;
begin
  if Assigned(fDynControlEngine) then
    Result := fDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

procedure tJvAppStoreSelectList.SetDynControlEngine(Value : tJvDynControlEngine);
begin
  fDynControlEngine := Value;
end;


procedure tJvAppStoreSelectList.OnOkButtonClick(Sender : TObject);
var
  Value : string;
begin
  Value := IComboBoxData.Value;
  if Operation <> sloStore then
    if SelectList.IndexOf(Value) < 0 then
      Exit;
  SelectDialog.ModalResult := mrOk;
end;

procedure tJvAppStoreSelectList.OnCancelButtonClick(Sender : TObject);
begin
  SelectDialog.ModalResult := mrCancel;
end;

procedure tJvAppStoreSelectList.OnListBoxChange(Sender : TObject);
var
  Index : integer;
begin
  Index := IListBoxData.Value;
  if (Index >= 0) and (Index < IListBoxItems.Items.Count) then
    IComboBoxData.Value := IListBoxItems.Items[Index];
end;

procedure tJvAppStoreSelectList.SelectFormDestroying(Sender: TObject);
begin
  fIComboBoxItems := nil;
  fIComboBoxData := nil;
  fIListBoxItems := nil;
  fIListBoxData := nil;
end;

procedure tJvAppStoreSelectList.CreateDialog(aOperation : tJvAppStoreSelectListOperation; aCaption : string = '');
var
  MainPanel, ButtonPanel, ListBoxPanel, ComboBoxPanel : TWinControl;
  OkButton, CancelButton : tWinControl;
  TmpPanel: IJVDynControlPanel;
  TmpControl: IJvDynControl;
begin
  Operation     := aOperation;
  IF Assigned(SelectDialog) THEN
    FreeAndNil(fSelectDialog);

  fSelectDialog := TForm(DynControlEngine.CreateForm('', ''));

  with SelectDialog do
  begin
    BorderIcons := [];
    DefaultMonitor := dmActiveForm;
    FormStyle := fsNormal;
    BorderStyle := bsDialog;
    Position := poScreenCenter;
    OnDestroy := SelectFormDestroying;
  end;

  if aCaption <> '' then
    SelectDialog.Caption := aCaption
  else
    case Operation of
      sloLoad : SelectDialog.Caption   := 'Load Settings';
      sloStore : SelectDialog.Caption  := 'Save Settings';
      sloManage : SelectDialog.Caption := 'Delete Settings';
    end;

  MainPanel   := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'MainPanel', '', alClient);
  ButtonPanel := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'ButtonPanel', '', alBottom);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton', '&Ok', '', OnOkButtonClick, true, false);
  if Operation <> sloStore then
    OkButton.Enabled := SelectList.Count > 0;
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton', '&Cancel', '', OnCancelButtonClick, false, true);
  ButtonPanel.Height := OkButton.Height + 10;
  CancelButton.Top := 5;
  CancelButton.Left := ButtonPanel.Width - 5 - CancelButton.Width;
  CancelButton.Anchors := [akTop, akRight];
  OkButton.Top     := 5;
  OkButton.Left    := CancelButton.Left - 10 - OkButton.Width;
  OkButton.Anchors := [akTop, akRight];

  ComboBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ComboBoxPanel', '', alBottom);
  if not Supports(ComboBoxPanel, IJVDynControlPanel, TmpPanel) then
    raise EIntfCastError.Create('SIntfCastError');
  with TmpPanel do
    ControlSetBorderWidth(5);
  ListBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ListPanel', '', alClient);
  if not Supports(ListBoxPanel, IJVDynControlPanel, TmpPanel) then
    raise EIntfCastError.Create('SIntfCastError');
  with TmpPanel do
    ControlSetBorderWidth(5);

  ComboBox      := DynControlEngine.CreateComboBoxControl(Self, ComboBoxPanel, 'ComboBox', SelectList);
  Supports(Combobox, IJvDynControlItems, fIComboBoxItems);
  Supports(ComboBox, IJvDynControlData, fIComboBoxData);

  IComboBoxData.Value := '';

  ListBox      := DynControlEngine.CreateListBoxControl(Self, ListBoxPanel, 'ListBox', SelectList);
  Supports(ListBox, IJvDynControlItems, fIListBoxItems);
  Supports(ListBox, IJvDynControlData, fIListBoxData);
  with IListBoxItems as IJvDynControl do
    ControlSetOnClick(OnListBoxChange);
  with IListBoxItems as IJvDynControlData do
    ControlSetOnChange(OnListBoxChange);
  if Supports(ListBox.ClassType, IJvDynControlDblClick) then
    with IListBoxItems as IJvDynControlDblClick do
      ControlSetOnDblClick(OnOkButtonClick);

  ComboBoxPanel.Height := ComboBox.Height + 10;
  ListBox.Align  := alClient;
  ComboBox.Align := alClient;

  if not Supports(OkButton, IJvDynControl, TmpControl) then
    raise EIntfCastError.Create('SIntfCastError');
  with TmpControl do
    case aOperation of
      sloLoad : ControlSetCaption('&Load');
      sloStore : ControlSetCaption('&Save');
      sloManage : ControlSetCaption('&Delete');
    end;
end;

function tJvAppStoreSelectList.GetSelectPath(aOperation : tJvAppStoreSelectListOperation; aCaption : string = '') : string;
begin
  try
    LoadSelectList;
    CreateDialog(aOperation, aCaption);
    SelectDialog.ShowModal;
    if SelectDialog.ModalResult = mrOk then
    begin
      case aOperation of
        sloLoad : Result := SelectPath + '\' + IComboBoxData.Value;
        sloStore :
        begin
          if SelectList.IndexOf(IComboBoxData.Value) < 0 then
          begin
            SelectList.Add(IComboBoxData.Value);
            StoreSelectList;
          end;
          Result := SelectPath + '\' + IComboBoxData.Value;
        end;
        sloManage :
        begin
          if SelectList.IndexOf(IComboBoxData.Value) >= 0 then
          begin
            SelectList.Delete(SelectList.IndexOf(IComboBoxData.Value));
            StoreSelectList;
          end;
          Result := SelectPath + '\' + IComboBoxData.Value;
        end;
      end;
    end;
  finally
    FreeAndNil(fSelectDialog);
  end;
end;

procedure tJvAppStoreSelectList.ManageSelectList(aCaption : string = '');
begin
  GetSelectPath(sloManage, aCaption);
end;

procedure tJvAppStoreSelectList.LoadSelectList;
var
  i : integer;
begin
  if not Assigned(AppStore) then
    Exit;
  AppStore.ReadStringList(SelectPath, fSelectList, true);
  if CheckEntries then
    for i := fSelectList.Count - 1 downto 0 do
      if not AppStore.PathExists(SelectPath + '\' + fSelectList[i]) then
        fSelectList.Delete(i);
end;

procedure tJvAppStoreSelectList.StoreSelectList;
var
  i : integer;
begin
  if not Assigned(AppStore) then
    Exit;
//  if CheckEntries then
//    for i := fSelectList.Count - 1 downto 0 do
//      if not AppStore.PathExists(SelectPath + '\' + fSelectList[i]) then
//        fSelectList.Delete(i);
  AppStore.WriteStringList(SelectPath, fSelectList);
end;

end.
