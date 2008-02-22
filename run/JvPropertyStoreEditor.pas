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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvExComCtrls, JvComCtrls, ExtCtrls, JvExControls,
  JvInspector, StdCtrls, JvExStdCtrls, JvListBox, JvPropertyStore,
  JvPropertyStoreEditorIntf, JvDynControlEngineIntf;

type
  TJvPropertyStoreEditorForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
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
    procedure PropertyStoreTreeViewEnter(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PropertyStoreTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure PropertyStoreTreeViewChanging(Sender: TObject; Node: TTreeNode; var
        AllowChange: Boolean);
    procedure RTTIInspectorEnter(Sender: TObject);
  private
    FInspectedObject: TPersistent;
    FInspectedObjectEditorHandler: IJvPropertyEditorHandler;
    FInspectedObjectListEditorHandler: IJvPropertyListEditorHandler;
    FPropertyStore: TComponent;
    InfoGroupBoxDynControl: IJvDynControl;
    InfoMemoDynControlData: IJvDynControlData;
    InfoPanel: TWinControl;
    Inspector: TWinControl;
    InspectorPanel: TWinControl;
    ListBoxControlItems: IJvDynControlItems;
    ListBoxControlItemIndex : IJvDynControlItemIndex;
    ListPanel: TWinControl;
    PropertyStoreTreeView: IJvDynControlTreeView;
    RTTIInspectorControl: IJvDynControlRTTIInspectorControl;
    TreePanel: TWinControl;
    function GetPropCount(Instance: TPersistent): Integer;
    function GetPropName(Instance: TPersistent; Index: Integer): string;
    procedure SetInspectedObject(const Value: TPersistent);
    procedure SetPropertyStore(const Value: TComponent);
    function ShowPropertyInTreeView(PropObject: TObject; const PropertyName:
        string): Boolean;
    property InspectedObject: TPersistent read FInspectedObject write
        SetInspectedObject;
    property InspectedObjectEditorHandler: IJvPropertyEditorHandler read
        FInspectedObjectEditorHandler;
    procedure FillListBox;
  protected
    ListInspectorPanel: TWinControl;
    procedure CreateFormControls;
    procedure FillTreeView(GotoNodeObject: TPersistent = nil);
    procedure FillTreeViewByComponent(TreeNodes: TTreeNodes; Parent: TTreeNode;
        aPropertyStore: TPersistent);
    function OnDisplayProperty(const aPropertyName : String): Boolean;
    procedure OnPropertyChange(var OldPropertyName, NewPropertyName : string);
    function OnTranslatePropertyName(const aPropertyName : String): string;
    procedure SetInformation(const iCaption, iInfo: string);
  public
    procedure GotoEditObject(EditObject: TPersistent);
    property InspectedObjectListEditorHandler: IJvPropertyListEditorHandler read
        FInspectedObjectListEditorHandler;
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


function EditPropertyStore(PropertyStore: TJvCustomPropertyStore): Boolean;
var
  JvPropertyStoreEditorForm: TJvPropertyStoreEditorForm;
  SavePropertyStore : TJvCustomPropertyStore;
begin
  Result := false;
  if not Assigned(PropertyStore) then
    Exit;
  JvPropertyStoreEditorForm := TJvPropertyStoreEditorForm.Create(Application);
  SavePropertyStore := TJvCustomPropertyStoreClass(PropertyStore.ClassType).Create(nil);
  try
    SavePropertyStore.Assign(PropertyStore);
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
end;

procedure TJvPropertyStoreEditorForm.CancelButtonClick(Sender: TObject);
begin
  //
end;

type tAccessCustomPanel = class(tCustomPanel);

procedure TJvPropertyStoreEditorForm.CreateFormControls;
var BottomPanel, BottomButtonPanel : TWinControl;
  Button: TButton;
  TreeView: TWinControl;
  TreeSplitter: TSplitter;
  EditPanel: TWinControl;
  ListSplitter: TSplitter;
  DynControlDblClick : IJvDynControlDblClick;
  ListButtonPanel: TWinControl;
  ListBox: TWinControl;
  InfoGroupBox: TWinControl;
  InfoMemoPanel: TWinControl;
  InfoMemo: TWinControl;
  DynControlMemo: IJvDynControlMemo;
  DynControlReadOnly: IJvDynControlReadOnly;
  DynControl: IJvDynControl;
begin
  BottomPanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'BottomPanel', '', alBottom);
  BottomPanel.Height := 34;
  if BottomPanel is TPanel then
    TPanel(BottomPanel).BevelOuter := bvNone;
  BottomPanel.TabOrder := 0;
  BottomButtonPanel := DefaultDynControlEngine.CreatePanelControl(Self, BottomPanel, 'BottomButtonPanel', '', alRight);
  BottomButtonPanel.Width := 166;
  if BottomButtonPanel is TPanel then
    TPanel(BottomButtonPanel).BevelOuter := bvNone;
  Button := DefaultDynControlEngine.CreateButton(Self, BottomButtonPanel, 'OKButton', RSPropertyStoreEditorDialogButtonOk, '', OkButtonClick);
  Button.Left := 4;
  Button.Top := 6;
  Button.Width := 75;
  Button.Height := 25;
  Button.ModalResult := mrOk;
  Button := DefaultDynControlEngine.CreateButton(Self, BottomButtonPanel, 'CancelButton', RSPropertyStoreEditorDialogButtonCancel, '', CancelButtonClick);
  Button.Left := 85;
  Button.Top := 6;
  Button.Width := 75;
  Button.Height := 25;
  Button.ModalResult := mrCancel;
  TreePanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'TreePanel', '', alLeft);
  TreePanel.Width := 200;
  if TreePanel is TCustomPanel then
  begin
    tAccessCustomPanel(TreePanel).BevelOuter := bvNone;
    tAccessCustomPanel(TreePanel).BorderWidth := 3;
  end;
  TreeView := DefaultDynControlEngine.CreateTreeViewControl(Self, TreePanel, 'PropertyStoreTreeView');
  Supports(TreeView, IJvDynControlTreeView, PropertyStoreTreeView);
  TreeView.Align := alClient;
  PropertyStoreTreeView.ControlSetHotTrack (True);
  PropertyStoreTreeView.ControlSetOnChange (PropertyStoreTreeViewChange);
  Supports(TreeView, IJvDynControl, DynControl);
  DynControl.ControlSetOnEnter(PropertyStoreTreeViewEnter);
  PropertyStoreTreeView.ControlSetOnChanging (PropertyStoreTreeViewChanging);
  PropertyStoreTreeView.ControlSetSortType(stNone);
  TreeSplitter := TSplitter.Create(Self);
  TreeSplitter.Align := alLeft;
  TreeSplitter.Parent := Self;
  EditPanel  := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'EditPanel', '', alClient);
  if EditPanel is TPanel then
  begin
    TPanel(EditPanel).BevelOuter := bvNone;
    TPanel(EditPanel).BorderWidth := 3;
  end;
  InfoPanel  := DefaultDynControlEngine.CreatePanelControl(Self, EditPanel, 'InfoPanel', '', alBottom);
  if InfoPanel is TCustomPanel then
  begin
    tAccessCustomPanel(InfoPanel).BevelOuter := bvNone;
//    tAccessCustomPanel(InfoPanel).BorderWidth := 3;
  end;
  InfoPanel.Height := 100;
  InfoGroupBox := DefaultDynControlEngine.CreateGroupBoxControl(Self, InfoPanel, 'InfoGroupBox', 'Info');
  InfoGroupBox.Align := alClient;
  Supports(InfoGroupBox, IJvDynControl, InfoGroupBoxDynControl);
  InfoMemoPanel  := DefaultDynControlEngine.CreatePanelControl(Self, InfoGroupBox, 'InfoMemoPanel', '', alClient);
  if InfoMemoPanel is TCustomPanel then
  begin
    tAccessCustomPanel(InfoMemoPanel).BevelOuter := bvNone;
    tAccessCustomPanel(InfoMemoPanel).BorderWidth := 3;
  end;
  InfoMemo := DefaultDynControlEngine.CreateMemoControl(Self, InfoMemoPanel, 'InfoMemo');
  InfoMemo.Align := alClient;
  if Supports(InfoMemo, IJvDynControlMemo, DynControlMemo) then
  begin
    DynControlMemo.ControlSetWordWrap(True);
    DynControlMemo.ControlSetScrollbars(ssVertical);
  end;
  if Supports(InfoMemo, IJvDynControlReadOnly, DynControlReadOnly) then
    DynControlReadOnly.ControlSetReadOnly(True);
  Supports(InfoMemo, IJvDynControlData, InfomemoDynControlData);
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
  ListBox := DefaultDynControlEngine.CreateListBoxControl(Self, ListPanel, 'ListBox', Nil);
  ListBox.Align := alClient;
  Supports (ListBox, IJvDynControlItems, ListBoxControlItems);
  Supports (ListBox, IJvDynControlItemIndex, ListBoxControlItemIndex);
  if Supports(ListBox, IJvDynControlDblClick, DynControlDblClick) then
    DynControlDblClick.ControlSetOnDblClick(ListEditButtonClick);
  InspectorPanel  := DefaultDynControlEngine.CreatePanelControl(Self, EditPanel, 'InspectorPanel', '', alClient);
  if InspectorPanel is TCustomPanel then
    tAccessCustomPanel(InspectorPanel).BevelOuter := bvNone;

  Inspector := DefaultDynControlEngine.CreateRTTIInspectorControl(self, InspectorPanel,
      'Inspector', OnDisplayProperty, OnTranslatePropertyName);
  Supports (Inspector, IJvDynControlRTTIInspectorControl, RTTIInspectorControl);
  RTTIInspectorControl.ControlOnPropertyChange := OnPropertyChange;
  Inspector.Align := alClient;
  Supports(RTTIInspectorControl, IJvDynControl, DynControl);
  DynControl.ControlSetOnEnter(RTTIInspectorEnter);

  Caption := RSPropertyStoreEditorDialogCaptionEditProperties;

  SetInformation('', '');
end;

procedure TJvPropertyStoreEditorForm.FillTreeView(GotoNodeObject: TPersistent =
    nil);
begin
  PropertyStoreTreeView.ControlItems.BeginUpdate;
  try
    PropertyStoreTreeView.ControlItems.Clear;
    FillTreeViewByComponent(PropertyStoreTreeView.ControlItems, nil, PropertyStore);
  finally
    PropertyStoreTreeView.ControlItems.EndUpdate;
  end;
  if not Assigned(GotoNodeObject ) then
    if PropertyStoreTreeView.ControlItems.Count > 0 then
      GotoEditObject(PropertyStoreTreeView.ControlItems[0].Data)
    else
      GotoEditObject(nil)
  else
    GotoEditObject(GotoNodeObject);
end;

procedure TJvPropertyStoreEditorForm.FillTreeViewByComponent(TreeNodes:
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
    RTTIInspectorControl.ControlInspectedObject := aPropertyStore;
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

function TJvPropertyStoreEditorForm.GetPropCount(Instance: TPersistent):
    Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.ClassInfo);
  Result := Data.PropCount;
end;

function TJvPropertyStoreEditorForm.GetPropName(Instance: TPersistent; Index:
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
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * SizeOf(PPropInfo));
  end;
  {$ENDIF CLR}
end;

procedure TJvPropertyStoreEditorForm.GotoEditObject(EditObject: TPersistent);
var
  TreeNode: TTreeNode;
  i: Integer;
begin
  if not Assigned(EditObject) then
  begin
    PropertyStoreTreeViewChange(nil, PropertyStoreTreeView.ControlSelected);
    Exit;
  end;
  for i  := 0 to PropertyStoreTreeView.ControlItems.Count - 1 do
  begin
    TreeNode := PropertyStoreTreeView.ControlItems[i];
    if Assigned(TreeNode.Data) and (TreeNode.Data = EditObject) then
    begin
      TreeNode.Expand(false);
      PropertyStoreTreeView.ControlSelected := TreeNode;
      Exit;
    end;
  end;
end;

procedure TJvPropertyStoreEditorForm.FillListBox;
var
  DetailObjectEditorHandler: IJvPropertyEditorHandler;
  i: Integer;
  SubObj: TObject;
begin
  ListBoxControlItems.ControlItems.Clear;
  for i := 0 to InspectedObjectListEditorHandler.ListEditIntf_ObjectCount - 1 do
  begin
    SubObj := InspectedObjectListEditorHandler.ListEditIntf_GetObject(i);
    if Supports(SubObj, IJvPropertyEditorHandler, DetailObjectEditorHandler) then
    begin
      ListBoxControlItems.ControlItems.AddObject(DetailObjectEditorHandler.EditIntf_GetVisibleObjectName + ' - ' + ' [' + inttostr(i + 1) + '] ', SubObj);
    end;
  end;
end;

procedure TJvPropertyStoreEditorForm.JvInspectorAfterItemCreate(Sender:
    TObject; Item: TJvCustomInspectorItem);
begin
  if Assigned(Item) and Assigned(InspectedObjectEditorHandler) then
    Item.DisplayName := InspectedObjectEditorHandler.EditIntf_TranslatePropertyName(Item.Name);
end;

procedure TJvPropertyStoreEditorForm.JvInspectorBeforeItemCreate(Sender:
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

procedure TJvPropertyStoreEditorForm.ListCopyButtonClick(Sender: TObject);
var
  NewObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    NewObject := InspectedObjectListEditorHandler.ListEditIntf_CloneNewObject(ListBoxControlItemIndex.ControlItemIndex);
    if Assigned(NewObject) then
    begin
      FillTreeView (NewObject);
    end;
  end;
end;

procedure TJvPropertyStoreEditorForm.ListDeleteButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := TPersistent(PropertyStoreTreeView.ControlSelected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_DeleteObject(ListBoxControlItemIndex.ControlItemIndex);
    FillTreeView (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.ListDownButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
  Ind : Integer;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemIndex.ControlItemIndex < ListBoxControlItems.ControlItems.Count) then
  begin
    EditObject := TPersistent(PropertyStoreTreeView.ControlSelected.Data);
    Ind := ListBoxControlItemIndex.ControlItemIndex;
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBoxControlItemIndex.ControlItemIndex, ListBoxControlItemIndex.ControlItemIndex+1);
    FillTreeView (EditObject);
    ListBoxControlItemIndex.ControlItemIndex := Ind +1;
  end;
end;

procedure TJvPropertyStoreEditorForm.ListEditButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := InspectedObjectListEditorHandler.ListEditIntf_GetObject(ListBoxControlItemIndex.ControlItemIndex);
    if Assigned(EditObject) then
      GotoEditObject (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.ListInsertButtonClick(Sender: TObject);
var
  newObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    NewObject := InspectedObjectListEditorHandler.ListEditIntf_CreateNewObject;
    FillTreeView (NewObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.ListUpButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
  Ind : Integer;
begin
  if Assigned(InspectedObjectListEditorHandler) and (ListBoxControlItemIndex.ControlItemIndex > 0) then
  begin
    EditObject := TPersistent(PropertyStoreTreeView.ControlSelected.Data);
    Ind := ListBoxControlItemIndex.ControlItemIndex;
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBoxControlItemIndex.ControlItemIndex, ListBoxControlItemIndex.ControlItemIndex-1);
    FillTreeView (EditObject);
    ListBoxControlItemIndex.ControlItemIndex := Ind -1;
  end;
end;

procedure TJvPropertyStoreEditorForm.PropertyStoreTreeViewEnter(Sender: TObject);
begin
  if Assigned(InspectedObjectEditorHandler) then
    SetInformation (InspectedObjectEditorHandler.EditIntf_GetVisibleObjectName, InspectedObjectEditorHandler.EditIntf_GetObjectHint);
end;

procedure TJvPropertyStoreEditorForm.OkButtonClick(Sender: TObject);
begin
  //
end;

function TJvPropertyStoreEditorForm.OnDisplayProperty(const aPropertyName :
    String): Boolean;
begin
  if Assigned(InspectedObjectEditorHandler) then
    Result := InspectedObjectEditorHandler.EditIntf_DisplayProperty(aPropertyName)
       and InspectedObjectEditorHandler.EditIntf_IsPropertySimple(aPropertyName) ;
end;

procedure TJvPropertyStoreEditorForm.OnPropertyChange(var OldPropertyName,
    NewPropertyName : string);
begin
  if Assigned(InspectedObjectEditorHandler) then
    SetInformation (InspectedObjectEditorHandler.EditIntf_TranslatePropertyName(NewPropertyName),
                    InspectedObjectEditorHandler.EditIntf_GetPropertyHint(NewPropertyName));
end;

procedure TJvPropertyStoreEditorForm.PropertyStoreTreeViewChange(Sender:
    TObject; Node: TTreeNode);
begin
  if not Assigned(Node) or
    not Assigned(Node.Data) or
    not (TObject(Node.Data) is TPersistent) or
    not (Supports(TObject(Node.Data), IJvPropertyEditorHandler)) then
    InspectedObject := nil
  else
    InspectedObject :=
      TPersistent(Node.Data);
end;

procedure TJvPropertyStoreEditorForm.PropertyStoreTreeViewChanging(Sender:
    TObject; Node: TTreeNode; var AllowChange: Boolean);
var JvPropertyEditorHandler : IJvPropertyEditorHandler;
begin
  RTTIInspectorControl.ControlSaveEditorValues;
  if Assigned(PropertyStoreTreeView.ControlSelected) and
    Assigned(PropertyStoreTreeView.ControlSelected.Data) and
    (TObject(PropertyStoreTreeView.ControlSelected.Data) is TPersistent) then
    if Supports(TObject(PropertyStoreTreeView.ControlSelected.Data), IJvPropertyEditorHandler, JvPropertyEditorHandler) then
    if (JvPropertyEditorHandler.EditIntf_GetVisibleObjectName  <> '') then
      PropertyStoreTreeView.ControlSelected.Text := JvPropertyEditorHandler.EditIntf_GetVisibleObjectName;
end;

procedure TJvPropertyStoreEditorForm.SetInspectedObject(const Value:
    TPersistent);
begin
  FInspectedObject := Value;
  Supports(InspectedObject, IJvPropertyEditorHandler, FInspectedObjectEditorHandler);
  Supports(InspectedObject, IJvPropertyListEditorHandler, FInspectedObjectListEditorHandler);
  RTTIInspectorControl.ControlSaveEditorValues;
  RTTIInspectorControl.ControlInspectedObject := Value;
  if Assigned(FInspectedObjectListEditorHandler) then
  begin
    ListPanel.visible := True;
    Inspector.Parent := ListInspectorPanel;
    InspectorPanel.visible := False;
    ListInspectorPanel.visible := RTTIInspectorControl.ControlGetVisibleItemsCount > 0;
    FillListBox;
  end
  else
  begin
    InspectorPanel.visible := True;
    ListPanel.visible := False;
    Inspector.Parent := InspectorPanel;
  end;
  if Assigned(InspectedObjectEditorHandler) then
    SetInformation (InspectedObjectEditorHandler.EditIntf_GetVisibleObjectName, InspectedObjectEditorHandler.EditIntf_GetObjectHint);
end;

procedure TJvPropertyStoreEditorForm.SetPropertyStore(const Value: TComponent);
begin
  if not Supports(Value, IJvPropertyEditorHandler) then
    Raise Exception.Create ('TJvPropertyStoreEditorForm.SetPropertyStore : PropertyStore must support IJvPropertyEditorHandler');
  FPropertyStore := Value;
  FillTreeView(Value);
end;

function TJvPropertyStoreEditorForm.ShowPropertyInTreeView(PropObject: TObject;
    const PropertyName: string): Boolean;
var
  PropertyEditorHandler: IJvPropertyEditorHandler;
begin
  Result := True;
  if not Assigned(PropObject) then
    Result := False
  else
  if Supports(PropObject, IJvPropertyEditorHandler, PropertyEditorHandler) then
  begin
    Result := (not PropertyEditorHandler.EditIntf_IsPropertySimple(PropertyName));
    Result := Result or (not RTTIInspectorControl.ControlIsPropertySupported(PropertyName))
  end
  else
    Result := not RTTIInspectorControl.ControlIsPropertySupported(PropertyName);
end;

function TJvPropertyStoreEditorForm.OnTranslatePropertyName(const aPropertyName :
    String): string;
begin
  Result := aPropertyName;
end;

procedure TJvPropertyStoreEditorForm.RTTIInspectorEnter(Sender:
    TObject);
begin
  if Assigned(InspectedObjectEditorHandler) and Assigned(RTTIInspectorControl) then
    SetInformation (InspectedObjectEditorHandler.EditIntf_TranslatePropertyName(RTTIInspectorControl.ControlGetCurrentPropertyName),
                    InspectedObjectEditorHandler.EditIntf_GetPropertyHint(RTTIInspectorControl.ControlGetCurrentPropertyName));
end;

procedure TJvPropertyStoreEditorForm.SetInformation(const iCaption, iInfo:
    string);
begin
  InfoMemoDynControlData.ControlValue := iInfo;
  InfoGroupBoxDynControl.ControlSetCaption(iCaption);
  InfoPanel.Visible := iInfo <> '';
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
