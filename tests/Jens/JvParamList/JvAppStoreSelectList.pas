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

uses
  Classes, Controls, Forms,
  JvComponent, JvTypes, JvAppStore, JvDynControlEngine,
  JvDynControlEngine_Interface;

type
  TJvAppStoreSelectListOperation = (sloLoad, sloStore, sloManage);

  TJvAppStoreSelectList = class(TJvComponent)
  private
    FSelectDialog: TForm;
    FListBox: TWinControl;
    FIListBoxItems: IJvDynControlItems;
    FIListBoxData: IJvDynControlData;
    FComboBox: TWinControl;
    FIComboBoxItems: IJvDynControlItems;
    FIComboBoxData: IJvDynControlData;
    FAppStore: TJvCustomAppStore;
    FSelectPath: string;
    FDynControlEngine: TJvDynControlEngine;
    FSelectList: TStringList;
    FOperation: TJvAppStoreSelectListOperation;
    FCheckEntries: Boolean;
  protected
    function GetSelectList: TStrings; virtual;
    procedure SetSelectList(const Value: TStrings); virtual;
    function GetAppStore: TJvCustomAppStore; virtual;
    procedure SetAppStore(Value: TJvCustomAppStore); virtual;
    procedure SetSelectPath(Value: string);
    function GetDynControlEngine: TJvDynControlEngine; virtual;
    procedure SetDynControlEngine(Value: TJvDynControlEngine); virtual;
    procedure CreateDialog(AOperation: TJvAppStoreSelectListOperation; ACaption: string = '');
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
    property Operation: TJvAppStoreSelectListOperation read FOperation write FOperation;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetSelectPath(AOperation: TJvAppStoreSelectListOperation; ACaption: string = ''): string;
    procedure ManageSelectList(ACaption: string = '');
    property DynControlEngine: TJvDynControlEngine read GetDynControlEngine write SetDynControlEngine;
  published
    property AppStore: TJvCustomAppStore read GetAppStore write SetAppStore;
    property SelectPath: string read FSelectPath write SetSelectPath;
    property CheckEntries: Boolean read FCheckEntries write FCheckEntries default True;
  end;

implementation

uses
  SysUtils;

resourcestring
  SLoadSettings = 'Load Settings';
  SSaveSettings = 'Save Settings';
  SDeleteSettings = 'Delete Settings';
  SOk = '&Ok';
  SCancel = '&Cancel';
  SLoad = '&Load';
  SSave = '&Save';
  SDelete = '&Delete';

  SDynControlEngineNotDefined = 'TJvAppStoreSelectList.CreateDialog: DynControlEngine not defined!';

constructor TJvAppStoreSelectList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectList := TStringList.Create;
  FCheckEntries := True;
  FSelectDialog := nil;
end;

destructor TJvAppStoreSelectList.Destroy;
begin
  FreeAndNil(FSelectList);
  FreeAndNil(FSelectDialog);
  inherited Destroy;
end;

procedure TJvAppStoreSelectList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FAppStore) then
    FAppStore := nil;
end;

function TJvAppStoreSelectList.GetSelectList: TStrings;
begin
  Result := FSelectList;
end;

procedure TJvAppStoreSelectList.SetSelectList(const Value: TStrings);
begin
  FSelectList.Assign(Value);
end;

function TJvAppStoreSelectList.GetAppStore: TJvCustomAppStore;
begin
  Result := FAppStore;
end;

procedure TJvAppStoreSelectList.SetAppStore(Value: TJvCustomAppStore);
begin
  FAppStore := Value;
end;

procedure TJvAppStoreSelectList.SetSelectPath(Value: string);
begin
  FSelectPath := Value;
end;

function TJvAppStoreSelectList.GetDynControlEngine: TJvDynControlEngine;
begin
  if Assigned(FDynControlEngine) then
    Result := FDynControlEngine
  else
    Result := DefaultDynControlEngine;
end;

procedure TJvAppStoreSelectList.SetDynControlEngine(Value: TJvDynControlEngine);
begin
  FDynControlEngine := Value;
end;

procedure TJvAppStoreSelectList.DialogOnOkButtonClick(Sender: TObject);
var
  Value: string;
begin
  Value := IComboBoxData.Value;
  if Operation <> sloStore then
    if SelectList.IndexOf(Value) >= 0 then
      SelectDialog.ModalResult := mrOk;
end;

procedure TJvAppStoreSelectList.DialogOnCancelButtonClick(Sender: TObject);
begin
  SelectDialog.ModalResult := mrCancel;
end;

procedure TJvAppStoreSelectList.DialogOnListBoxChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := IListBoxData.Value;
  if (Index >= 0) and (Index < IListBoxItems.Items.Count) then
    IComboBoxData.Value := IListBoxItems.Items[Index];
end;

procedure TJvAppStoreSelectList.SelectFormDestroying(Sender: TObject);
begin
  FIComboBoxItems := nil;
  FIComboBoxData := nil;
  FIListBoxItems := nil;
  FIListBoxData := nil;
end;

procedure TJvAppStoreSelectList.CreateDialog(AOperation: TJvAppStoreSelectListOperation; ACaption: string = '');
var
  MainPanel, ButtonPanel, ListBoxPanel, ComboBoxPanel: TWinControl;
  OkButton, CancelButton: tWinControl;
  ITmpPanel: IJVDynControlPanel;
  ITmpControl: IJvDynControl;
  ITmpComboBox: IJvDynControlComboBox;
begin
  if not Assigned(DynControlEngine) then
    raise EJVCLException.Create(SDynControlEngineNotDefined);

  Operation := AOperation;
  FreeAndNil(FSelectDialog);

  FSelectDialog := TForm(DynControlEngine.CreateForm('', ''));

  with SelectDialog do
  begin
    BorderIcons := [];
    DefaultMonitor := dmActiveForm;
    FormStyle := fsNormal;
    BorderStyle := bsDialog;
    Position := poScreenCenter;
    OnDestroy := SelectFormDestroying;
  end;

  if ACaption <> '' then
    SelectDialog.Caption := ACaption
  else
    case Operation of
      sloLoad:
        SelectDialog.Caption := SLoadSettings;
      sloStore:
        SelectDialog.Caption := SSaveSettings;
      sloManage:
        SelectDialog.Caption := SDeleteSettings;
    end;

  MainPanel := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'MainPanel', '', alClient);
  ButtonPanel := DynControlEngine.CreatePanelControl(Self, SelectDialog, 'ButtonPanel', '', alBottom);

  OkButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'OkButton', SOk, '', DialogOnOkButtonClick, True, False);
  if Operation <> sloStore then
    OkButton.Enabled := SelectList.Count > 0;
  CancelButton := DynControlEngine.CreateButton(Self, ButtonPanel, 'CancelButton', SCancel, '',
    DialogOnCancelButtonClick, False, True);
  ButtonPanel.Height := OkButton.Height + 10;
  CancelButton.Top := 5;
  CancelButton.Left := ButtonPanel.Width - 5 - CancelButton.Width;
  CancelButton.Anchors := [akTop, akRight];
  OkButton.Top := 5;
  OkButton.Left := CancelButton.Left - 10 - OkButton.Width;
  OkButton.Anchors := [akTop, akRight];

  ComboBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ComboBoxPanel', '', alBottom);
  if not Supports(ComboBoxPanel, IJVDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);
  ListBoxPanel := DynControlEngine.CreatePanelControl(Self, MainPanel, 'ListPanel', '', alClient);
  if not Supports(ListBoxPanel, IJVDynControlPanel, ITmpPanel) then
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpPanel do
    ControlSetBorder(bvNone, bvNone, 0, bsNone, 5);

  ComboBox := DynControlEngine.CreateComboBoxControl(Self, ComboBoxPanel, 'ComboBox', SelectList);
  if not Supports(ComboBox, IJvDynControlItems, FIComboBoxItems) then
    raise EIntfCastError.Create(SIntfCastError);
  if not Supports(ComboBox, IJvDynControlData, FIComboBoxData) then
    raise EIntfCastError.Create(SIntfCastError);

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

  IComboBoxData.Value := '';

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
    raise EIntfCastError.Create(SIntfCastError);
  with ITmpControl do
    case AOperation of
      sloLoad:
        ControlSetCaption(SLoad);
      sloStore:
        ControlSetCaption(SSave);
      sloManage:
        ControlSetCaption(SDelete);
    end;
end;

function TJvAppStoreSelectList.GetSelectPath(AOperation: TJvAppStoreSelectListOperation; ACaption: string = ''): string;
begin
  try
    LoadSelectList;
    CreateDialog(AOperation, ACaption);
    SelectDialog.ShowModal;
    if SelectDialog.ModalResult = mrOk then
    begin
      case AOperation of
        sloLoad:
          Result := SelectPath + PathDelim + IComboBoxData.Value;
        sloStore:
          begin
            if SelectList.IndexOf(IComboBoxData.Value) < 0 then
            begin
              SelectList.Add(IComboBoxData.Value);
              StoreSelectList;
            end;
            Result := SelectPath + PathDelim + IComboBoxData.Value;
          end;
        sloManage:
          begin
            if SelectList.IndexOf(IComboBoxData.Value) >= 0 then
            begin
              SelectList.Delete(SelectList.IndexOf(IComboBoxData.Value));
              StoreSelectList;
            end;
            Result := SelectPath + PathDelim + IComboBoxData.Value;
          end;
      end;
    end;
  finally
    FreeAndNil(FSelectDialog);
  end;
end;

procedure TJvAppStoreSelectList.ManageSelectList(ACaption: string = '');
begin
  GetSelectPath(sloManage, ACaption);
end;

procedure TJvAppStoreSelectList.LoadSelectList;
var
  I: Integer;
begin
  if Assigned(AppStore) then
  begin
    AppStore.ReadStringList(SelectPath, FSelectList, True);
    if CheckEntries then
      for I := FSelectList.Count - 1 downto 0 do
        if not AppStore.PathExists(SelectPath + PathDelim + FSelectList[I]) then
          FSelectList.Delete(I);
  end;
end;

procedure TJvAppStoreSelectList.StoreSelectList;
var
  I: Integer;
begin
  if Assigned(AppStore) then
  //  if CheckEntries then
  //    for I := FSelectList.Count - 1 downto 0 do
  //      if not AppStore.PathExists(SelectPath + PathDelim + FSelectList[I]) then
  //        FSelectList.Delete(I);
    AppStore.WriteStringList(SelectPath, FSelectList);
end;

end.

