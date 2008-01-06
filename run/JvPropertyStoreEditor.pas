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
  JvInspector, StdCtrls, JvExStdCtrls, JvListBox, JvPropertyStore, JvPropertyStoreEditorIntf;

type
  TJvPropertyStoreEditorForm = class(TForm)
    BottomButtonPanel: TPanel;
    BottomPanel: TPanel;
    CancelButton: TButton;
    EditPanel: TPanel;
    InspectorPanel: TPanel;
    JvInspector: TJvInspector;
    ListBox: TJvListBox;
    ListButtonPanel: TPanel;
    ListCopyButton: TButton;
    ListDeleteButton: TButton;
    ListDownButton: TButton;
    ListEditButton: TButton;
    ListInsertButton: TButton;
    ListInspectorPanel: TPanel;
    ListPanel: TPanel;
    ListUpButton: TButton;
    OkButton: TButton;
    PropertyStoreTreeView: TJvTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreePanel: TPanel;
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
    procedure OkButtonClick(Sender: TObject);
    procedure PropertyStoreTreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FInspectedObject: TPersistent;
    FInspectedObjectEditorHandler: IJvPropertyEditorHandler;
    FInspectedObjectListEditorHandler: IJvPropertyListEditorHandler;
    FPropertyStore: TComponent;
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
    procedure FillTreeView(GotoNodeObject: TPersistent = nil);
    procedure FillTreeViewByComponent(TreeNodes: TTreeNodes; Parent: TTreeNode;
        aPropertyStore: TPersistent);
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
  TypInfo;

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

procedure TJvPropertyStoreEditorForm.CancelButtonClick(Sender: TObject);
begin
  //
end;

procedure TJvPropertyStoreEditorForm.FillTreeView(GotoNodeObject: TPersistent =
    nil);
begin
  PropertyStoreTreeView.Items.BeginUpdate;
  try
    PropertyStoreTreeView.Items.Clear;
    FillTreeViewByComponent(PropertyStoreTreeView.Items, nil, PropertyStore);
  finally
    PropertyStoreTreeView.Items.EndUpdate;
  end;
  if not Assigned(GotoNodeObject ) then
    if PropertyStoreTreeView.Items.Count > 0 then
      GotoEditObject(PropertyStoreTreeView.Items[0].Data)
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
    for I := 0 to GetPropCount(aPropertyStore) - 1 do
    begin
      PropName := GetPropName(aPropertyStore,I);
      if PropertyEditorHandler.EditIntf_DisplayProperty(PropName) and
         ShowPropertyInTreeView (aPropertyStore, PropName) then
        case PropType(aPropertyStore, PropName) of
          tkClass:
            begin
              SubObj := GetObjectProp(aPropertyStore, PropName);
              if Supports(SubObj, IJvPropertyEditorHandler, DetailPropertyEditorHandler) and
                 (SubObj is TPersistent)then
              begin
                Node := TreeNodes.AddChildObject(Parent,
                   DetailPropertyEditorHandler.EditIntf_TranslatePropertyName(PropName),
                   SubObj);
                FillTreeViewByComponent(TreeNodes, Node, TPersistent(SubObj));
              end;
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
    PropertyStoreTreeViewChange(nil, PropertyStoreTreeView.Selected);
    Exit;
  end;
  for i  := 0 to PropertyStoreTreeView.Items.Count - 1 do
  begin
    TreeNode := PropertyStoreTreeView.Items[i];
    if Assigned(TreeNode.Data) and (TreeNode.Data = EditObject) then
    begin
      TreeNode.Expand(false);
      PropertyStoreTreeView.Selected := TreeNode;
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
  ListBox.Items.Clear;
  for i := 0 to InspectedObjectListEditorHandler.ListEditIntf_ObjectCount - 1 do
  begin
    SubObj := InspectedObjectListEditorHandler.ListEditIntf_GetObject(i);
    if Supports(SubObj, IJvPropertyEditorHandler, DetailObjectEditorHandler) then
    begin
      ListBox.Items.AddObject(DetailObjectEditorHandler.EditIntf_GetVisibleObjectName + ' - ' + ' [' + inttostr(i + 1) + '] ', SubObj);
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
    NewObject := InspectedObjectListEditorHandler.ListEditIntf_CloneNewObject(ListBox.ItemIndex);
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
    EditObject := TPersistent(PropertyStoreTreeView.Selected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_DeleteObject(ListBox.ItemIndex);
    FillTreeView (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.ListDownButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := TPersistent(PropertyStoreTreeView.Selected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBox.ItemIndex, ListBox.ItemIndex+1);
    FillTreeView (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.ListEditButtonClick(Sender: TObject);
var
  EditObject: TPersistent;
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := InspectedObjectListEditorHandler.ListEditIntf_GetObject(ListBox.ItemIndex);
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
begin
  if Assigned(InspectedObjectListEditorHandler) then
  begin
    EditObject := TPersistent(PropertyStoreTreeView.Selected.Data);
    InspectedObjectListEditorHandler.ListEditIntf_MoveObjectPosition(ListBox.ItemIndex, ListBox.ItemIndex-1);
    FillTreeView (EditObject);
  end;
end;

procedure TJvPropertyStoreEditorForm.OkButtonClick(Sender: TObject);
begin
  //
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

procedure TJvPropertyStoreEditorForm.SetInspectedObject(const Value:
    TPersistent);
begin
  FInspectedObject := Value;
  Supports(InspectedObject, IJvPropertyEditorHandler, FInspectedObjectEditorHandler);
  Supports(InspectedObject, IJvPropertyListEditorHandler, FInspectedObjectListEditorHandler);
  JvInspector.InspectObject := Value;
  if not Assigned(InspectedObject) then
  begin
    ListPanel.visible := False;
    InspectorPanel.visible := False;
  end
  else if Assigned(FInspectedObjectListEditorHandler) then
  begin
    ListPanel.visible := True;
    ListPanel.Align := alClient;
    InspectorPanel.Parent := ListInspectorPanel;
    InspectorPanel.Align := alClient;
    ListInspectorPanel.visible := JvInspector.VisibleCount > 0;
    FillListBox;
  end
  else
  begin
    InspectorPanel.visible := True;
    ListPanel.visible := False;
    ListPanel.Align := alClient;
    InspectorPanel.Parent := EditPanel;
    InspectorPanel.Align := alClient;
  end;
end;

procedure TJvPropertyStoreEditorForm.SetPropertyStore(const Value: TComponent);
begin
  if not Supports(Value, IJvPropertyEditorHandler) then
    Raise Exception.Create ('TJvPropertyStoreEditorForm.SetPropertyStore : PropertyStore must support IJvPropertyEditorHandler');
  FPropertyStore := Value;
  FillTreeView;
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
    Result := not PropertyEditorHandler.EditIntf_IsPropertySimple(PropertyName);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
