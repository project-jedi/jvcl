{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPropertyStoreEditor.pas, released on 2008-01-01.

The Initial Developer of the Original Code is Jens Fudickar
All Rights Reserved.

Contributor(s):
  Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
unit JvPropertyStoreEditor;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvExComCtrls, JvComCtrls, ExtCtrls, JvExControls,
  JvInspector, StdCtrls, JvExStdCtrls, JvListBox, JvPropertyStore,
  JvPropertyStoreEditorIntf, JvDynControlEngineIntf;


type
  TJvPropertyStoreEditorControl = class(TCustomControl)
    procedure JvInspectorAfterItemCreate(Sender: TObject; Item:
        TJvCustomInspectorItem);
    procedure JvInspectorBeforeItemCreate(Sender: TObject; Data:
      TJvCustomInspectorData; var ItemClass: TJvInspectorItemClass);
    procedure ListCopyButtonClick(Sender: TObject);
    procedure ListDeleteButtonClick(Sender: TObject);
    procedure ListDownButtonClick(Sender: TObject);
    procedure ListEditButtonClick(Sender: TObject);
    procedure ListInsertButtonClick(Sender: TObject);
    procedure ListUpButtonClick(Sender: TObject);
    procedure ListSortUpButtonClick(Sender: TObject);
    procedure ListSortDownButtonClick(Sender: TObject);
    procedure PropertyStoreTreeViewEnter(Sender: TObject);
    procedure PropertyStoreTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure PropertyStoreTreeViewChanging(Sender: TObject; Node: TTreeNode; var
        AllowChange: Boolean);
    procedure RTTIInspectorEnter(Sender: TObject);
  private
    FInspectedObject: TPersistent;
    FInspectedObjectEditorHandlerIntf: IJvPropertyEditorHandler;
    FInspectedObjectListEditorHandlerIntf: IJvPropertyListEditorHandler;
    FPropertyStore: TComponent;
    InfoGroupBoxDynControlCaptionIntf: IJvDynControlCaption;
    InfoMemoDynControlDataIntf: IJvDynControlData;
    InfoMemo: TWinControl;
    InfoPanel: TWinControl;
    Inspector: TWinControl;
    InspectorPanel: TWinControl;
    ListBoxControlItemsIntf: IJvDynControlItems;
    ListBoxControlItemIndexIntf: IJvDynControlItemIndex;
    ListInspectorPanel: TWinControl;
    ListPanel: TWinControl;
    PropertyStoreTreeViewIntf: IJvDynControlTreeView;
    RTTIInspectorControlIntf: IJvDynControlRTTIInspectorControl;
    TreePanel: TWinControl;
    TreeSplitter: TSplitter;
    ListSplitter: TSplitter;
    ListButtonPanel: TWinControl;
    function GetPropCount(Instance: TPersistent): Integer;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    procedure SetInspectedObject(const Value: TPersistent);
    procedure SetPropertyStore(const Value: TComponent);
    function ShowPropertyInTreeView(PropObject: TObject; const PropertyName:
        string): Boolean;
    property InspectedObject: TPersistent read FInspectedObject write
        SetInspectedObject;
    property InspectedObjectEditorHandlerIntf: IJvPropertyEditorHandler read
        FInspectedObjectEditorHandlerIntf;
    procedure FillListBox;
    procedure RTTIInspectorOnCanResize(Sender: TObject; var NewWidth, NewHeight:
        Integer; var Resize: Boolean);
  protected
    procedure CreateControls;
    procedure DestroyControls;
    procedure FillTreeView(GotoNodeObject: TPersistent = nil);
    procedure FillTreeViewByComponent(TreeNodes: TTreeNodes; Parent: TTreeNode;
        aPropertyStore: TPersistent);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function OnDisplayProperty(const aPropertyName : String): Boolean;
    function OnInspectorTranslatePropertyName(const aPropertyName : String): string;
    procedure OnPropertyChange(var OldPropertyName, NewPropertyName : string);
    function OnTranslatePropertyName(const aPropertyName : String): string;
    procedure SetInformation(const iCaption, iInfo: string);
  public
    destructor Destroy; override;
    procedure GotoEditObject(EditObject: TPersistent);
    property InspectedObjectListEditorHandler: IJvPropertyListEditorHandler read
        FInspectedObjectListEditorHandlerIntf;
    property PropertyStore: TComponent read FPropertyStore write SetPropertyStore;
  end;

type
  TJvPropertyStoreEditorForm = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPropertyStore: TComponent;
    FPropertyStoreEditorControl: TJvPropertyStoreEditorControl;
    procedure CancelButtonClick(Sender: TObject);
    procedure IntOnShow(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure SetPropertyStore(const Value: TComponent);
  protected
    procedure CreateFormControls;
    procedure DestroyFormControls;
  public
    property PropertyStore: TComponent read FPropertyStore write SetPropertyStore;
  end;

function EditPropertyStore(PropertyStore: TJvCustomPropertyStore): Boolean;

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
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RTLConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  JvResources,
  TypInfo, JvDynControlEngine;

{$R *.dfm}

type tAccessControl = class(TControl);


function EditPropertyStore(PropertyStore: TJvCustomPropertyStore): Boolean;
var
  JvPropertyStoreEditorForm: TJvPropertyStoreEditorForm;
  SavePropertyStore : TJvCustomPropertyStore;
begin
  Result := false;
  if not Assigned(PropertyStore) then
    Exit;
  SavePropertyStore := PropertyStore.Clone(nil);
  JvPropertyStoreEditorForm := TJvPropertyStoreEditorForm.Create(Application);
  try
    JvPropertyStoreEditorForm.PropertyStore := SavePropertyStore;
    Result := JvPropertyStoreEditorForm.ShowModal = mrOk;
    if Result then
      PropertyStore.Assign(SavePropertyStore);
  finally
    SavePropertyStore.Free;
    JvPropertyStoreEditorForm.Free;
  end;
end;

procedure TJvPropertyStoreEditorForm.FormCreate(Sender: TObject);
begin
  CreateFormControls;
  OnShow := IntOnShow;
end;

type tAccessCustomPanel = class(tCustomPanel);

procedure TJvPropertyStoreEditorForm.FormDestroy(Sender: TObject);
begin
  DestroyFormControls;
end;

procedure TJvPropertyStoreEditorForm.CreateFormControls;
var BottomPanel: TWinControl;
  Button: TButton;
  ITmpBevelBorder: IJvDynControlBevelBorder;
begin
  BottomPanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'BottomPanel', '', alBottom);
  if Supports(BottomPanel, IJvDynControlBevelBorder, ITmpBevelBorder) then
    ITmpBevelBorder.ControlSetBevelOuter(bvNone);
  BottomPanel.TabOrder := 0;
  Button := DefaultDynControlEngine.CreateButton(Self, BottomPanel, 'OKButton', RSPropertyStoreEditorDialogButtonOk, '', OkButtonClick);
  Button.Top := 3;
  Button.Width := 75;
  Button.Height := 25;
  Button.Left := BottomPanel.Width-2*Button.Width-10;
  Button.Anchors := [akTop, akRight];
  Button.ModalResult := mrOk;
  Button := DefaultDynControlEngine.CreateButton(Self, BottomPanel, 'CancelButton', RSPropertyStoreEditorDialogButtonCancel, '', CancelButtonClick);
  Button.Top := 3;
  Button.Width := 75;
  Button.Height := 25;
  Button.Left := BottomPanel.Width-Button.Width-5;
  Button.Anchors := [akTop, akRight];
  Button.ModalResult := mrCancel;
  BottomPanel.Height := 2*Button.Top+Button.Height+1;

  FPropertyStoreEditorControl:= TJvPropertyStoreEditorControl.Create(self);
  FPropertyStoreEditorControl.Parent := Self;
  FPropertyStoreEditorControl.Align := alClient;

  Caption := RSPropertyStoreEditorDialogCaptionEditProperties;

end;

procedure TJvPropertyStoreEditorForm.SetPropertyStore(const Value: TComponent);
begin
  FPropertyStore := Value;
  if Assigned(FPropertyStoreEditorControl) then
    FPropertyStoreEditorControl.PropertyStore := PropertyStore;
end;

procedure TJvPropertyStoreEditorForm.OkButtonClick(Sender: TObject);
begin
  // Do Not Remove
end;

procedure TJvPropertyStoreEditorForm.CancelButtonClick(Sender: TObject);
begin
  // Do Not Remove
end;

procedure TJvPropertyStoreEditorForm.DestroyFormControls;
begin
  FreeAndNil(FPropertyStoreEditorControl);
end;

procedure TJvPropertyStoreEditorForm.IntOnShow(Sender: TObject);
begin
  if Assigned(FPropertyStoreEditorControl) then
  begin
    FPropertyStoreEditorControl.PropertyStore := PropertyStore;
  end;
end;


destructor TJvPropertyStoreEditorControl.Destroy;
begin
  DestroyControls;
  inherited Destroy;
end;

procedure TJvPropertyStoreEditorControl.CreateControls;
var
  TreeView: TWinControl;
  EditPanel: TWinControl;
  DynControlDblClick : IJvDynControlDblClick;
  ListBox: TWinControl;
  InfoGroupBox: TWinControl;
  InfoMemoPanel: TWinControl;
  DynControlMemo: IJvDynControlMemo;
  DynControlReadOnly: IJvDynControlReadOnly;
  DynControl: IJvDynControl;
  Button: TButton;
begin
  TreePanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'TreePanel', '', alLeft);
  TreePanel.Width := 250;
  if TreePanel is TCustomPanel then
  begin
    tAccessCustomPanel(TreePanel).BevelOuter := bvNone;
    tAccessCustomPanel(TreePanel).BorderWidth := 3;
  end;

  TreeView := DefaultDynControlEngine.CreateTreeViewControl(Self, TreePanel, 'PropertyStoreTreeViewIntf');
  Supports(TreeView, IJvDynControlTreeView, PropertyStoreTreeViewIntf);
  TreeView.Align := alClient;
  PropertyStoreTreeViewIntf.ControlSetReadOnly (True);
  PropertyStoreTreeViewIntf.ControlSetHotTrack (True);
  PropertyStoreTreeViewIntf.ControlSetOnChange (PropertyStoreTreeViewChange);
  PropertyStoreTreeViewIntf.ControlSetOnChanging (PropertyStoreTreeViewChanging);
  PropertyStoreTreeViewIntf.ControlSetSortType(stNone);
  Supports(TreeView, IJvDynControl, DynControl);
  DynControl.ControlSetOnEnter(PropertyStoreTreeViewEnter);

  TreeSplitter := TSplitter.Create(Self);
  TreeSplitter.Align := alLeft;
  TreeSplitter.Parent := Self;
  TreeSplitter.Left := TreePanel.Left+TreePanel.Width+1;
  EditPanel  := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'EditPanel', '', alClient);
  if EditPanel is TPanel then
  begin
    TPanel(EditPanel).BevelOuter := bvNone;
    TPanel(EditPanel).BorderWidth := 3;
  end;
  InfoPanel  := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'InfoPanel', '', alBottom);
  if InfoPanel is TCustomPanel then
  begin
    tAccessCustomPanel(InfoPanel).BevelOuter := bvNone;
  end;
  InfoPanel.Height := 100;
  InfoGroupBox := DefaultDynControlEngine.CreateGroupBoxControl(Self, InfoPanel, 'InfoGroupBox', 'Info');
  InfoGroupBox.Align := alClient;
  Supports(InfoGroupBox, IJvDynControlCaption, InfoGroupBoxDynControlCaptionIntf);
  InfoMemoPanel  := DefaultDynControlEngine.CreatePanelControl(Self, InfoGroupBox, 'InfoMemoPanel', '', alClient);
  if InfoMemoPanel is TCustomPanel then
  begin
    tAccessCustomPanel(InfoMemoPanel).BevelOuter := bvNone;
    tAccessCustomPanel(InfoMemoPanel).BorderWidth := 3;
  end;
  InfoMemo := DefaultDynControlEngine.CreateMemoControl(Self, InfoGroupbox, 'InfoMemo');
  InfoMemo.Align := alClient;
  if Supports(InfoMemo, IJvDynControlMemo, DynControlMemo) then
  begin
    DynControlMemo.ControlSetWordWrap(True);
    DynControlMemo.ControlSetScrollbars(ssVertical);
  end;
  if Supports(InfoMemo, IJvDynControlReadOnly, DynControlReadOnly) then
    DynControlReadOnly.ControlSetReadOnly(True);
  Supports(InfoMemo, IJvDynControlData, InfoMemoDynControlDataIntf);
  ListPanel  := DefaultDynControlEngine.CreatePanelControl(Self, EditPanel, 'ListPanel', '', alClient);
  if ListPanel is TPanel then
    TPanel(ListPanel).BevelOuter := bvNone;
  ListInspectorPanel  := DefaultDynControlEngine.CreatePanelControl(Self, ListPanel, 'ListInspectorPanel', '', alTop);
  if ListInspectorPanel is TPanel then
    TPanel(ListInspectorPanel).BevelOuter := bvNone;
  ListInspectorPanel.Height := 141;
  ListSplitter := TSplitter.Create (Self);
  ListSplitter.Parent := ListPanel;
  ListSplitter.Align := alTop;
  ListSplitter.Cursor := crVSplit;
  ListButtonPanel  := DefaultDynControlEngine.CreatePanelControl(Self, ListPanel, 'ListButtonPanel', '', alTop);
  ListButtonPanel.Height := 25;
  if ListButtonPanel is TPanel then
    TPanel(ListButtonPanel).BevelOuter := bvNone;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListInsertButton', RSPropertyStoreEditorListButtonInsert, '', ListInsertButtonClick);
  Button.Left := 0;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListCopyButton', RSPropertyStoreEditorListButtonCopy, '', ListCopyButtonClick);
  Button.Left := 40;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListEditButton', RSPropertyStoreEditorListButtonEdit, '', ListEditButtonClick);
  Button.Left := 80;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListDeleteButton', RSPropertyStoreEditorListButtonDelete, '', ListDeleteButtonClick);
  Button.Left := 120;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListUpButton', RSPropertyStoreEditorListButtonUp, '', ListUpButtonClick);
  Button.Left := 165;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListDownButton', RSPropertyStoreEditorListButtonDown, '', ListDownButtonClick);
  Button.Left := 205;
  Button.Width := 40;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListSortUpButton', RSPropertyStoreEditorListButtonSortUp, '', ListSortUpButtonClick);
  Button.Left := 245;
  Button.Width := 50;
  Button := DefaultDynControlEngine.CreateButton(Self, ListButtonPanel, 'ListSortDownButton', RSPropertyStoreEditorListButtonSortDown, '', ListSortDownButtonClick);
  Button.Left := 295;
  Button.Width := 60;
  ListBox := DefaultDynControlEngine.CreateListBoxControl(Self, ListPanel, 'ListBox', Nil);
  ListBox.Align := alClient;
  Supports (ListBox, IJvDynControlItems, ListBoxControlItemsIntf);
  Supports (ListBox, IJvDynControlItemIndex, ListBoxControlItemIndexIntf);
  if Supports(ListBox, IJvDynControlDblClick, DynControlDblClick) then
    DynControlDblClick.ControlSetOnDblClick(ListEditButtonClick);
  InspectorPanel  := DefaultDynControlEngine.CreatePanelControl(Self, EditPanel, 'InspectorPanel', '', alClient);
  if InspectorPanel is TCustomPanel then
    tAccessCustomPanel(InspectorPanel).BevelOuter := bvNone;

  Inspector := DefaultDynControlEngine.CreateRTTIInspectorControl(self, InspectorPanel,
      'Inspector', OnDisplayProperty, OnTranslatePropertyName);
  Supports (Inspector, IJvDynControlRTTIInspectorControl, RTTIInspectorControlIntf);
  RTTIInspectorControlIntf.ControlOnPropertyChange := OnPropertyChange;
  RTTIInspectorControlIntf.ControlOnTranslatePropertyName := OnInspectorTranslatePropertyName;
  Inspector.Align := alClient;
  tAccessControl(Inspector).OnCanResize := RTTIInspectorOnCanResize;
  Supports(RTTIInspectorControlIntf, IJvDynControl, DynControl);
  RTTIInspectorControlIntf.ControlDividerWidth := 200;

  DynControl.ControlSetOnEnter(RTTIInspectorEnter);

  Caption := RSPropertyStoreEditorDialogCaptionEditProperties;

  SetInformation('', '');
end;

procedure TJvPropertyStoreEditorControl.DestroyControls;
begin
  PropertyStore := nil;
  InspectedObject := nil;
  FInspectedObjectEditorHandlerIntf := nil;
  FInspectedObjectListEditorHandlerIntf := nil;
  InfoGroupBoxDynControlCaptionIntf := nil;
  InfoMemoDynControlDataIntf := nil;
  ListBoxControlItemsIntf := nil;
  ListBoxControlItemIndexIntf := nil;
  PropertyStoreTreeViewIntf := nil;
  RTTIInspectorControlIntf := nil;
  FreeAndNil(TreePanel);
  FreeAndNil(InfoMemo);
  FreeAndNil(InfoPanel);
  FreeAndNil(Inspector);
  FreeAndNil(ListPanel);
  FreeAndNil(InspectorPanel);
end;

procedure TJvPropertyStoreEditorControl.FillTreeView(GotoNodeObject: TPersistent =
    nil);
begin
  if (csDestroying in Componentstate) then
    Exit;
  if not Assigned(PropertyStoreTreeViewIntf) then
    CreateControls;
  PropertyStoreTreeViewIntf.ControlItems.BeginUpdate;
  try
    PropertyStoreTreeViewIntf.ControlItems.Clear;
    FillTreeViewByComponent(PropertyStoreTreeViewIntf.ControlItems, nil, PropertyStore);
  finally
    PropertyStoreTreeViewIntf.ControlItems.EndUpdate;
  end;
  if not Assigned(GotoNodeObject ) then
    if PropertyStoreTreeViewIntf.ControlItems.Count > 0 then
      GotoEditObject(PropertyStoreTreeViewIntf.ControlItems[0].Data)
    else
      GotoEditObject(nil)
  else
    GotoEditObject(GotoNodeObject);
end;

procedure TJvPropertyStoreEditorControl.FillTreeViewByComponent(TreeNodes:
    TTreeNodes; Parent: TTreeNode; aPropertyStore: TPersistent);
var
  I: Integer;
  SubObj: TObject;
  Node: TTreeNode;
  PropName: string;
  PropertyEditorHandler: IJvPropertyEditorHandler;
  DetailPropertyEditorHandler: IJvPropertyEditorHandler;
  PropertyListEditorHandler: IJvPropertyListEditorHandler;

begin
  if not assigned(aPropertyStore) or
    not Supports(aPropertyStore, IJvPropertyEditorHandler, PropertyEditorHandler) then
    Exit;
  if not Assigned(Parent) then
  begin
    Node := TreeNodes.AddChildObject(Parent, PropertyEditorHandler.EditIntf_GetVisibleObjectName,
      aPropertyStore);
    FillTreeViewByComponent(TreeNodes, Node,
      aPropertyStore);
  end
  else
  begin
    RTTIInspectorControlIntf.ControlInspectedObject := aPropertyStore;
    for I := 0 to GetPropCount(aPropertyStore) - 1 do
    begin
      PropName := GetPropName(aPropertyStore,I);
      if PropIsType(aPropertyStore, PropName, tkClass) then
      begin
        SubObj := GetObjectProp(aPropertyStore, PropName);
        if PropertyEditorHandler.EditIntf_DisplayProperty(PropName) then
        if ShowPropertyInTreeView (aPropertyStore, PropName) then
        if Supports(SubObj, IJvPropertyEditorHandler, DetailPropertyEditorHandler) then
        if (SubObj is TPersistent)then
         begin
           Node := TreeNodes.AddChildObject(Parent,
               DetailPropertyEditorHandler.EditIntf_TranslatePropertyName(PropName),
               SubObj);
           FillTreeViewByComponent(TreeNodes, Node, TPersistent(SubObj));
         end;
      end;
    end;
    if Supports (aPropertyStore, IJvPropertyListEditorHandler, PropertyListEditorHandler) then
      for i := 0 to PropertyListEditorHandler.ListEditIntf_ObjectCount  - 1 do
      begin
        SubObj := PropertyListEditorHandler.ListEditIntf_GetObject(i);
        if Supports(SubObj, IJvPropertyEditorHandler, DetailPropertyEditorHandler) and
          (SubObj is TPersistent) then
        begin
          Node := TreeNodes.AddChildObject(Parent,
            DetailPropertyEditorHandler.EditIntf_GetVisibleObjectName, SubObj);
          FillTreeViewByComponent(TreeNodes, Node, TPersistent(SubObj));
        end;
      end;
  end;
end;

function TJvPropertyStoreEditorControl.GetPropCount(Instance: TPersistent):
    Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data.PropCount;
end;

function TJvPropertyStoreEditorControl.GetPropName(Instance: TPersistent; Index:
    Integer): string;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.ClassInfo);
  {$IFDEF CLR}
  PropList := GetPropInfos(Instance.ClassInfo);
  PropInfo := PropList[Index];
  Result := PropInfo.Name;
  {$ELSE}
  GetMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := {$IFDEF SUPPORTS_UNICODE}UTF8ToString{$ENDIF SUPPORTS_UNICODE}(PropInfo^.Name);
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
  {$ENDIF CLR}
end;

procedure TJvPropertyStoreEditorControl.GotoEditObject(EditObject: TPersistent);
var
  TreeNode: TTreeNode;
  i: Integer;
begin
  if csDestroying in Componentstate then
    Exit;
  if not Assigned(EditObject) then
  begin
    PropertyStoreTreeViewChange(nil, PropertyStoreTreeViewIntf.ControlSelected);
    Exit;
  end;
  for i  := 0 to PropertyStoreTreeViewIntf.ControlItems.Count - 1 do
  begin
    TreeNode := PropertyStoreTreeViewIntf.ControlItems[i];
    if Assigned(TreeNode.Data) and (TreeNode.Data = EditObject) then
    begin
      TreeNode.Expand(false);
      PropertyStoreTreeViewIntf.ControlSelected := TreeNode;
      Exit;
    end;
  end;
end;

procedure TJvPropertyStoreEditorControl.FillListBox;
var
  DetailObjectEditorHandler: IJvPropertyEditorHandler;
  i: Integer;
  SubObj: TObject;
begin
  if csDestroying in Componentstate then
    Exit;
  ListBoxControlItemsIntf.ControlItems.Clear;
  for i := 0 to InspectedObjectListEditorHandler.ListEditIntf_ObjectCount - 1 do
  begin
    SubObj := InspectedObjectListEditorHandler.ListEditIntf_GetObject(i);
    if Supports(SubObj, IJvPropertyEditorHandler, DetailObjectEditorHandler) then
    begin
      ListBoxControlItemsIntf.ControlItems.AddObject(DetailObjectEditorHandler.EditIntf_GetVisibleObjectName + ' - ' + ' [' + inttostr(i + 1) + '] ', SubObj);
    end;
  end;
end;

procedure TJvPropertyStoreEditorControl.JvInspectorAfterItemCreate(Sender:
    TObject; Item: TJvCustomInspectorItem);
begin
  if Assigned(Item) and Assigned(InspectedObjectEditorHandlerIntf) then
    Item.DisplayName := InspectedObjectEditorHandlerIntf.EditIntf_TranslatePropertyName(Item.Name);
end;

procedure TJvPropertyStoreEditorControl.JvInspectorBeforeItemCreate(Sender:
  TObject; Data: TJvCustomInspectorData; var ItemClass:
  TJvInspectorItemClass);
var
  PropertyEditorHandler: IJvPropertyEditorHandler;
begin
  if Assigned(Data) and
     (Data is TJvInspectorPropData) and
     Assigned(TJvInspectorPropData(Data).Instance)  then
  begin
    if Supports(TJvInspectorPropData(Data).Instance, IJvPropertyEditorHandler, PropertyEditorHandler) then
      if not PropertyEditorHandler.EditIntf_DisplayProperty(Data.Name) then
        ItemClass := nil
      else if ShowPropertyInTreeView(TJvInspectorPropData(Data).Instance, Data.Name)  then
        ItemClass := nil;
  end;
end;

procedure TJvPropertyStoreEditorControl.ListCopyButtonClick(Sender: TObject);
var
  NewObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    NewObject := InspectedObjectListEditorHandler.ListEditIntf_CloneNewObject(ListBoxControlItemIndexIntf.ControlItemIndex);
    if Assigned(NewObject) then
    begin
      FillTreeView (NewObject);
    end;
  end;
end;

procedure TJvPropertyStoreEditorControl.ListDeleteButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
    if (MessageDlg(RSPropertyStoreEditorDeleteEntry, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      EditObject := TPersistent(PropertyStoreTreeViewIntf.ControlSelected.Data);
      InspectedObjectListEditorHandler.ListEditIntf_DeleteObject(ListBoxControlItemIndexIntf.ControlItemIndex);
      FillTreeView (EditObject);
    end;
end;

procedure TJvPropertyStoreEditorControl.ListDownButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
  Ind : Integer;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemIndexIntf.ControlItemIndex < ListBoxControlItemsIntf.ControlItems.Count) then
  begin
    EditObject := TPersistent(PropertyStoreTreeViewIntf.ControlSelected.Data);
    Ind := ListBoxControlItemIndexIntf.ControlItemIndex;
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBoxControlItemIndexIntf.ControlItemIndex, ListBoxControlItemIndexIntf.ControlItemIndex+1);
    FillTreeView (EditObject);
    ListBoxControlItemIndexIntf.ControlItemIndex := Ind +1;
  end;
end;

procedure TJvPropertyStoreEditorControl.ListEditButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := InspectedObjectListEditorHandler.ListEditIntf_GetObject(ListBoxControlItemIndexIntf.ControlItemIndex);
    if Assigned(EditObject) then
      GotoEditObject (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorControl.ListInsertButtonClick(Sender: TObject);
var
  newObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    NewObject := InspectedObjectListEditorHandler.ListEditIntf_CreateNewObject;
    FillTreeView (NewObject);
  end;
end;

procedure TJvPropertyStoreEditorControl.ListUpButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
  Ind : Integer;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemIndexIntf.ControlItemIndex > 0) then
  begin
    EditObject := TPersistent(PropertyStoreTreeViewIntf.ControlSelected.Data);
    Ind := ListBoxControlItemIndexIntf.ControlItemIndex;
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBoxControlItemIndexIntf.ControlItemIndex, ListBoxControlItemIndexIntf.ControlItemIndex-1);
    FillTreeView (EditObject);
    ListBoxControlItemIndexIntf.ControlItemIndex := Ind -1;
  end;
end;

procedure TJvPropertyStoreEditorControl.ListSortUpButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemsIntf.ControlItems.Count > 0) then
  begin
    EditObject := TPersistent(PropertyStoreTreeViewIntf.ControlSelected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_SortObjects(True);
    FillTreeView (EditObject);
    ListBoxControlItemIndexIntf.ControlItemIndex := 0;
  end;
end;

procedure TJvPropertyStoreEditorControl.ListSortDownButtonClick(Sender:
    TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemsIntf.ControlItems.Count > 0) then
  begin
    EditObject := TPersistent(PropertyStoreTreeViewIntf.ControlSelected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_SortObjects(False);
    FillTreeView (EditObject);
    ListBoxControlItemIndexIntf.ControlItemIndex := 0;
  end;
end;

procedure TJvPropertyStoreEditorControl.Notification(AComponent: TComponent;
    Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FPropertyStore) then
  begin
    PropertyStore := nil;
    InspectedObject := nil;
  end;
end;

procedure TJvPropertyStoreEditorControl.PropertyStoreTreeViewEnter(Sender: TObject);
begin
  if Assigned(InspectedObjectEditorHandlerIntf) then
    SetInformation (InspectedObjectEditorHandlerIntf.EditIntf_GetVisibleObjectName, InspectedObjectEditorHandlerIntf.EditIntf_GetObjectHint);
end;

function TJvPropertyStoreEditorControl.OnDisplayProperty(const aPropertyName :
    String): Boolean;
begin
  if Assigned(InspectedObjectEditorHandlerIntf) then
    Result := InspectedObjectEditorHandlerIntf.EditIntf_DisplayProperty(aPropertyName)
       and InspectedObjectEditorHandlerIntf.EditIntf_IsPropertySimple(aPropertyName)
  else
    Result := False;
end;

function TJvPropertyStoreEditorControl.OnInspectorTranslatePropertyName(const
    aPropertyName : String): string;
begin
  if Assigned(InspectedObjectEditorHandlerIntf) then
    Result := InspectedObjectEditorHandlerIntf.EditIntf_TranslatePropertyName(aPropertyName)
  else
    Result := aPropertyName;
end;

procedure TJvPropertyStoreEditorControl.OnPropertyChange(var OldPropertyName,
    NewPropertyName : string);
begin
  if Assigned(InspectedObjectEditorHandlerIntf) then
    SetInformation (InspectedObjectEditorHandlerIntf.EditIntf_TranslatePropertyName(NewPropertyName),
                    InspectedObjectEditorHandlerIntf.EditIntf_GetPropertyHint(NewPropertyName));
end;

procedure TJvPropertyStoreEditorControl.PropertyStoreTreeViewChange(Sender:
    TObject; Node: TTreeNode);
begin
  if csDestroying in Componentstate then
    Exit;
  if not Assigned(Node) or
    not Assigned(Node.Data) or
    not (TObject(Node.Data) is TPersistent) or
    not (Supports(TObject(Node.Data), IJvPropertyEditorHandler)) then
    InspectedObject := nil
  else
    InspectedObject :=
      TPersistent(Node.Data);
end;

procedure TJvPropertyStoreEditorControl.PropertyStoreTreeViewChanging(Sender:
    TObject; Node: TTreeNode; var AllowChange: Boolean);
var JvPropertyEditorHandler : IJvPropertyEditorHandler;
begin
  if csDestroying in Componentstate then
    Exit;
  RTTIInspectorControlIntf.ControlSaveEditorValues;
  if Assigned(PropertyStoreTreeViewIntf.ControlSelected) and
    Assigned(PropertyStoreTreeViewIntf.ControlSelected.Data) and
    (TObject(PropertyStoreTreeViewIntf.ControlSelected.Data) is TPersistent) then
    if Supports(TObject(PropertyStoreTreeViewIntf.ControlSelected.Data),
      IJvPropertyEditorHandler, JvPropertyEditorHandler) then
      if (JvPropertyEditorHandler.EditIntf_GetVisibleObjectName  <> '') then
        PropertyStoreTreeViewIntf.ControlSelected.Text := JvPropertyEditorHandler.EditIntf_GetVisibleObjectName;
end;

procedure TJvPropertyStoreEditorControl.SetInspectedObject(const Value:
    TPersistent);
begin
  if csDestroying in Componentstate then
  begin
    FInspectedObject := nil;
    FInspectedObjectEditorHandlerIntf := nil;
    FInspectedObjectListEditorHandlerIntf := nil;
    if Assigned(RTTIInspectorControlIntf) then
      RTTIInspectorControlIntf.ControlInspectedObject := Value;
    Exit;
  end;
  FInspectedObject := Value;
  Supports(InspectedObject, IJvPropertyEditorHandler, FInspectedObjectEditorHandlerIntf);
  Supports(InspectedObject, IJvPropertyListEditorHandler, FInspectedObjectListEditorHandlerIntf);
  RTTIInspectorControlIntf.ControlSaveEditorValues;
  RTTIInspectorControlIntf.ControlInspectedObject := Value;
  if Assigned(FInspectedObjectListEditorHandlerIntf) then
  begin
    ListPanel.visible := True;
    Inspector.Parent := ListInspectorPanel;
    InspectorPanel.visible := False;
    ListInspectorPanel.visible := RTTIInspectorControlIntf.ControlGetVisibleItemsCount > 0;
    ListSplitter.visible := ListInspectorPanel.visible;
    ListButtonPanel.Top := ListInspectorPanel.Top+ListInspectorPanel.Height+1;
    ListSplitter.Top := ListButtonPanel.Top-1;
    FillListBox;
  end
  else
  begin
    InspectorPanel.visible := True;
    ListPanel.visible := False;
    Inspector.Parent := InspectorPanel;
  end;
  if Assigned(InspectedObjectEditorHandlerIntf) then
    SetInformation (InspectedObjectEditorHandlerIntf.EditIntf_GetVisibleObjectName, InspectedObjectEditorHandlerIntf.EditIntf_GetObjectHint);
end;

procedure TJvPropertyStoreEditorControl.SetPropertyStore(const Value: TComponent);
begin
  FPropertyStore := Value;
  if csDestroying in Componentstate then
    Exit;
  if Assigned(Value) and not Supports(Value, IJvPropertyEditorHandler) then
    Raise Exception.Create ('TJvPropertyStoreEditorControl.SetPropertyStore : PropertyStore must support IJvPropertyEditorHandler');
  if Assigned(FPropertyStore) then
    FPropertyStore.FreeNotification(Self);
  FillTreeView(Value);
end;

function TJvPropertyStoreEditorControl.ShowPropertyInTreeView(PropObject: TObject;
    const PropertyName: string): Boolean;
var
  PropertyEditorHandler: IJvPropertyEditorHandler;
begin
  Result := True;
  if csDestroying in Componentstate then
    Exit;
  if not Assigned(PropObject) then
    Result := False
  else
    if Supports(PropObject, IJvPropertyEditorHandler, PropertyEditorHandler) then
    begin
      Result := (not PropertyEditorHandler.EditIntf_IsPropertySimple(PropertyName));
      Result := Result or (not RTTIInspectorControlIntf.ControlIsPropertySupported(PropertyName))
    end
    else
      Result := not RTTIInspectorControlIntf.ControlIsPropertySupported(PropertyName);
end;

function TJvPropertyStoreEditorControl.OnTranslatePropertyName(const aPropertyName :
    String): string;
begin
  Result := aPropertyName;
end;

procedure TJvPropertyStoreEditorControl.RTTIInspectorEnter(Sender:
    TObject);
begin
  if csDestroying in Componentstate then
    Exit;
  if Assigned(InspectedObjectEditorHandlerIntf) and Assigned(RTTIInspectorControlIntf) then
    SetInformation (InspectedObjectEditorHandlerIntf.EditIntf_TranslatePropertyName(RTTIInspectorControlIntf.ControlGetCurrentPropertyName),
                    InspectedObjectEditorHandlerIntf.EditIntf_GetPropertyHint(RTTIInspectorControlIntf.ControlGetCurrentPropertyName));
end;

procedure TJvPropertyStoreEditorControl.RTTIInspectorOnCanResize(Sender:
    TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
  Ratio: Double;
begin
  if not Assigned(RTTIInspectorControlIntf) then
    Exit;
  Ratio := Inspector.Width / RTTIInspectorControlIntf.ControlDividerWidth;
  RTTIInspectorControlIntf.ControlDividerWidth := Round(NewWidth/Ratio);
end;

procedure TJvPropertyStoreEditorControl.SetInformation(const iCaption, iInfo:
    string);
begin
  if csDestroying in Componentstate then
    Exit;
  InfoMemoDynControlDataIntf.ControlValue := iInfo;
  InfoGroupBoxDynControlCaptionIntf.ControlSetCaption(iCaption);
  InfoPanel.Visible := iInfo <> '';
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
