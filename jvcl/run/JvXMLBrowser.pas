{ -----------------------------------------------------------------------------
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
  located at http://jvcl.delphi-jedi.org

  Known Issues:
  ----------------------------------------------------------------------------- }
// $Id$
unit JvXMLBrowser;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
{$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.Types,
  System.UITypes,
{$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Variants,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  JvComponent, ExtCtrls, JvExControls,
  StdCtrls, JvPropertyStore,
  JvPropertyStoreEditorIntf, JvDynControlEngineIntf, ActnList, Menus, JclSimpleXml, JvSimpleXml;

type
{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
{$ENDIF RTL230_UP}

  TJvXMLBrowserControl = class(TJvCustomControl)
    procedure XMLTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure XMLTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
  private
    FInspectedObject: TJclSimpleXMLElem;
    FItemNameProperty: string;
    FXML: TJvSimpleXML;
    Inspector: TWinControl;
    InspectorPanel: TWinControl;
    IntXML: TJvSimpleXML;
    TreePanel: TWinControl;
    TreeSplitter: TSplitter;
    XMLTreeViewIntf: IJvDynControlTreeView;
    function GetXMLData: string;
    procedure SetInspectedObject(const Value: TJclSimpleXMLElem);
    procedure SetXML(const Value: TJvSimpleXML);
    procedure SetXMLData(const Value: string);
    property InspectedObject: TJclSimpleXMLElem read FInspectedObject write SetInspectedObject;
  protected
    procedure CreateControls;
    function CreateInspector(AOwner: TComponent; AParentControl: TWinControl): TWinControl; virtual;
    function CurrentXML: TJvSimpleXML;
    procedure DestroyControls;
    procedure FillTreeView(iGotoXMLElement: TJclSimpleXMLElem = nil);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TransferXMLItemToInspector(iXMLItem: TJclSimpleXMLElem; iInspector: TWinControl); virtual;
    procedure TransferXMLItemToTreeNode(iXMLItem: TJclSimpleXMLElem; iParent: TTreeNode; iTreeNodes: TTreenodes);
    procedure TransferXMLToTreeNodes(iXML: TJvSimpleXML; iTreeNodes: TTreenodes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GotoXMLElement(iXMLElement: TJclSimpleXMLElem);
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property Enabled;
    property Font;
    property ItemNameProperty: string read FItemNameProperty write FItemNameProperty;
    property OnCanResize;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
{$IFDEF COMPILER7_UP}
    property ParentBackground default True;
{$ENDIF COMPILER7_UP}
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property XML: TJvSimpleXML read FXML write SetXML;
    property XMLData: string read GetXMLData write SetXMLData;
  end;

type
  TJvXMLBrowserForm = class(TJvForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FItemNameProperty: string;
    FXML: TJvSimpleXML;
    FXMLBrowserControl: TJvXMLBrowserControl;
    FXMLData: string;
    procedure IntOnShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure SetItemNameProperty(const Value: string);
    procedure SetXML(const Value: TJvSimpleXML);
    procedure SetXMLData(const Value: string);
  protected
    procedure CreateFormControls;
    procedure DestroyFormControls;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property XML: TJvSimpleXML read FXML write SetXML;
    property XMLData: string read FXMLData write SetXMLData;
  published
    property ItemNameProperty: string read FItemNameProperty write SetItemNameProperty;
  end;

procedure ShowXMLBrowser(iXML: TJvSimpleXML; const iDialogCaption: string = ''; const iItemNameProperty: String =
    'Name'); overload;
procedure ShowXMLBrowser(const iXMLData: String; const iDialogCaption: string = ''; const iItemNameProperty: String =
    'Name'); overload;
procedure ShowXMLBrowserFromFile(const iXMLFile: String; const iDialogCaption: string = ''; const iItemNameProperty:
    String = 'Name');

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
  JvResources,
  TypInfo, JvDynControlEngine, JvJVCLUtils, JclLogic, Grids, JclStrings;

{$R *.dfm}


type
  tAccessControl = class(TControl);

procedure ShowXMLBrowser(iXML: TJvSimpleXML; const iDialogCaption: string = ''; const iItemNameProperty: String =
    'Name');
var
  JvXMLBrowserForm: TJvXMLBrowserForm;
begin
  JvXMLBrowserForm := TJvXMLBrowserForm.Create(Application);
  try
    JvXMLBrowserForm.XML := iXML;
    JvXMLBrowserForm.Caption := iDialogCaption;
    JvXMLBrowserForm.ItemNameProperty := iItemNameProperty;
    JvXMLBrowserForm.ShowModal;
  finally
    JvXMLBrowserForm.Free;
  end;
end;

procedure ShowXMLBrowser(const iXMLData: String; const iDialogCaption: string = ''; const iItemNameProperty: String =
    'Name');
var
  JvXMLBrowserForm: TJvXMLBrowserForm;
begin
  JvXMLBrowserForm := TJvXMLBrowserForm.Create(Application);
  try
    JvXMLBrowserForm.XMLData := iXMLData;
    JvXMLBrowserForm.Caption := iDialogCaption;
    JvXMLBrowserForm.ItemNameProperty := iItemNameProperty;
    JvXMLBrowserForm.ShowModal;
  finally
    JvXMLBrowserForm.Free;
  end;
end;

procedure ShowXMLBrowserFromFile(const iXMLFile: String; const iDialogCaption: string = ''; const iItemNameProperty:
    String = 'Name');
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    if FileExists(iXMLFile) then
      sl.LoadFromFile(iXMLFile);
    ShowXMLBrowser(sl.Text, iDialogCaption, iItemNameProperty);
  finally
    sl.Free;
  end;
end;

procedure TJvXMLBrowserForm.CreateFormControls;
var
  BottomPanel: TWinControl;
  Button: TButton;
  ITmpBevelBorder: IJvDynControlBevelBorder;
begin
  BottomPanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'BottomPanel', '', alBottom);
  if Supports(BottomPanel, IJvDynControlBevelBorder, ITmpBevelBorder) then
    ITmpBevelBorder.ControlSetBevelOuter(bvNone);
  BottomPanel.TabOrder := 0;
  Button := DefaultDynControlEngine.CreateButton(Self, BottomPanel, 'CloseButton', RSXMLBrowserDialogButtonClose, '',
    CloseButtonClick);
  Button.Top := 3;
  Button.Width := 75;
  Button.Height := 25;
  Button.Left := BottomPanel.Width - Button.Width - 5;
  Button.Anchors := [akTop, akRight];
  Button.ModalResult := mrOk;
  BottomPanel.Height := 2 * Button.Top + Button.Height + 1;

  FXMLBrowserControl := TJvXMLBrowserControl.Create(Self);
  FXMLBrowserControl.Parent := Self;
  FXMLBrowserControl.Align := alClient;

  {$IFDEF COMPILER7_UP}
  Position := poOwnerFormCenter;
  {$ELSE}
  Position := poScreenCenter;
  {$ENDIF COMPILER7_UP};  
end;

procedure TJvXMLBrowserForm.DestroyFormControls;
begin
  FreeAndNil(FXMLBrowserControl);
end;

procedure TJvXMLBrowserForm.FormCreate(Sender: TObject);
begin
  CreateFormControls;
  OnShow := IntOnShow;
end;

procedure TJvXMLBrowserForm.FormDestroy(Sender: TObject);
begin
  DestroyFormControls;
end;

procedure TJvXMLBrowserForm.IntOnShow(Sender: TObject);
begin
  if Assigned(FXMLBrowserControl) then
    FXMLBrowserControl.XML := XML;
end;

procedure TJvXMLBrowserForm.CloseButtonClick(Sender: TObject);
begin
  // Do Not Remove
end;

procedure TJvXMLBrowserForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)  then
    if (AComponent = FXML) then
      XML := nil;
end;

procedure TJvXMLBrowserForm.SetItemNameProperty(const Value: string);
begin
  FItemNameProperty := Value;
  if Assigned(FXMLBrowserControl) then
    FXMLBrowserControl.ItemNameProperty := ItemNameProperty;
end;

procedure TJvXMLBrowserForm.SetXML(const Value: TJvSimpleXML);
begin
  ReplaceComponentReference(Self, Value, TComponent(FXML));
  if Assigned(FXMLBrowserControl) then
    FXMLBrowserControl.XML := XML;
end;

procedure TJvXMLBrowserForm.SetXMLData(const Value: string);
begin
  FXMLData := Value;
  if Assigned(FXMLBrowserControl) and not Assigned(XML) then
    FXMLBrowserControl.XMLData := XMLData;
end;

type
  tAccessCustomPanel = class(tCustomPanel);

constructor TJvXMLBrowserControl.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  IntXML := TJvSimpleXML.Create(self);
  CreateControls;
  FItemNameProperty := 'Name';
end;

destructor TJvXMLBrowserControl.Destroy;
begin
  FreeAndNil(IntXML);
  DestroyControls;
  inherited Destroy;
end;

procedure TJvXMLBrowserControl.CreateControls;
var
  TreeView: TWinControl;
  EditPanel: TWinControl;
  DynControl: IJvDynControl;

  function CreateBtn(AOwner: TComponent; AParentControl: TWinControl; const AButtonName, ACaption, AHint: string; AOnClick:
    TNotifyEvent; AAction: TAction; var ALeft: Integer; AWidth: Integer): TButton;
  begin
    Result := DefaultDynControlEngine.CreateButton(AOwner, AParentControl, AButtonName, ACaption, AHint, AOnClick);
    Result.Action := AAction;
    Result.Left := ALeft;
    Result.Width := AWidth;
    ALeft := ALeft + AWidth;
  end;

begin

  TreePanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'TreePanel', '', alLeft);
  TreePanel.Width := 250;
  if TreePanel is tCustomPanel then
  begin
    tAccessCustomPanel(TreePanel).BevelOuter := bvNone;
    tAccessCustomPanel(TreePanel).BorderWidth := 3;
  end;

  TreeView := DefaultDynControlEngine.CreateTreeViewControl(Self, TreePanel, 'XMLTreeViewIntf');
  Supports(TreeView, IJvDynControlTreeView, XMLTreeViewIntf);
  TreeView.Align := alClient;
  XMLTreeViewIntf.ControlSetReadOnly(True);
  XMLTreeViewIntf.ControlSetHotTrack(True);
  XMLTreeViewIntf.ControlSetOnChange(XMLTreeViewChange);
  XMLTreeViewIntf.ControlSetOnChanging(XMLTreeViewChanging);
  XMLTreeViewIntf.ControlSetSortType(stNone);
  Supports(TreeView, IJvDynControl, DynControl);

  TreeSplitter := TSplitter.Create(Self);
  TreeSplitter.Align := alLeft;
  TreeSplitter.Parent := Self;
  TreeSplitter.Left := TreePanel.Left + TreePanel.Width + 1;
  EditPanel := DefaultDynControlEngine.CreatePanelControl(Self, Self, 'EditPanel', '', alClient);
  if EditPanel is tCustomPanel then
  begin
    tAccessCustomPanel(EditPanel).BevelOuter := bvNone;
    tAccessCustomPanel(EditPanel).BorderWidth := 3;
  end;

  InspectorPanel := DefaultDynControlEngine.CreatePanelControl(Self, EditPanel, 'InspectorPanel', '', alClient);
  if InspectorPanel is tCustomPanel then
  begin
    tAccessCustomPanel(InspectorPanel).BevelOuter := bvNone;
    tAccessCustomPanel(EditPanel).BorderWidth := 3;
  end;

  Inspector := CreateInspector(self, InspectorPanel);
  Inspector.Align := alClient;

  Caption := 'XML Browser';

end;

function TJvXMLBrowserControl.CreateInspector(AOwner: TComponent; AParentControl: TWinControl): TWinControl;
var InspectorControlIntf: IJvDynControlStringGrid;
begin
  Result := DefaultDynControlEngine.CreateStringGridControl(AOwner, AParentControl, 'Inspector', 2, 0, 1, 0);
  Supports(Result , IJvDynControlStringGrid, InspectorControlIntf);
  InspectorControlIntf.ControlOptions := InspectorControlIntf.ControlOptions + [goAlwaysShowEditor];
end;

function TJvXMLBrowserControl.CurrentXML: TJvSimpleXML;
begin
  if Assigned(XML) then
    Result := XML
  else
    Result := IntXML;
end;

procedure TJvXMLBrowserControl.DestroyControls;
begin
  XML := nil;
  InspectedObject := nil;
  XMLTreeViewIntf := nil;
  FreeAndNil(TreePanel);
  FreeAndNil(Inspector);
  FreeAndNil(InspectorPanel);
end;

procedure TJvXMLBrowserControl.FillTreeView(iGotoXMLElement: TJclSimpleXMLElem = nil);
begin
  if (csDestroying in Componentstate) or
    not Assigned(XMLTreeViewIntf) then
    Exit;
  XMLTreeViewIntf.ControlItems.BeginUpdate;
  try
    XMLTreeViewIntf.ControlItems.Clear;
    TransferXMLToTreeNodes(CurrentXML, XMLTreeViewIntf.ControlItems);
  finally
    XMLTreeViewIntf.ControlItems.EndUpdate;
  end;
  if not Assigned(iGotoXMLElement) then
    if XMLTreeViewIntf.ControlItems.Count > 0 then
      GotoXMLElement(XMLTreeViewIntf.ControlItems[0].Data)
    else
      GotoXMLElement(nil)
  else
    GotoXMLElement(iGotoXMLElement);
end;

function TJvXMLBrowserControl.GetXMLData: string;
begin
  Result := CurrentXML.XMLData;
end;

procedure TJvXMLBrowserControl.GotoXMLElement(iXMLElement: TJclSimpleXMLElem);
var
  TreeNode: TTreeNode;
  i: Integer;
begin
  if (csDestroying in Componentstate) or
    not Assigned(XMLTreeViewIntf) then
    Exit;
  if Assigned(iXMLElement) then
    for i := 0 to XMLTreeViewIntf.ControlItems.Count - 1 do
    begin
      TreeNode := XMLTreeViewIntf.ControlItems[i];
      if Assigned(TreeNode.Data) and (TreeNode.Data = iXMLElement) then
      begin
        TreeNode.Expand(false);
        XMLTreeViewIntf.ControlSelected := TreeNode;
        break;
      end;
    end;
  XMLTreeViewChange(nil, XMLTreeViewIntf.ControlSelected);
end;

procedure TJvXMLBrowserControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)  then
    if (AComponent = FXML) then
      FXML := nil;
end;

procedure TJvXMLBrowserControl.SetInspectedObject(const Value: TJclSimpleXMLElem);
begin
  if csDestroying in Componentstate then
  begin
    FInspectedObject := nil;
    Exit;
  end;
  FInspectedObject := Value;
  TransferXMLItemToInspector(Value, Inspector);
end;

procedure TJvXMLBrowserControl.SetXML(const Value: TJvSimpleXML);
begin
  ReplaceComponentReference(Self, Value, TComponent(FXML));
  FillTreeView(nil);
end;

procedure TJvXMLBrowserControl.SetXMLData(const Value: string);
begin
  CurrentXML.XMLData := Value;
  FillTreeView(nil);
end;

procedure TJvXMLBrowserControl.TransferXMLItemToInspector(iXMLItem: TJclSimpleXMLElem; iInspector: TWinControl);
var
  Item: TJclSimpleXMLElem;
  RowCount: Integer;
  ColWidth: Integer;
  iStringGridIntf: IJvDynControlStringGrid;
  i : Integer;
  Prop: TJclSimpleXMLProp;

  procedure AddRow (iName, iValue : String);
  begin
    Inc(RowCount);
    iStringGridIntf.ControlRowCount := RowCount;
    ColWidth := Max(ColWidth, Canvas.TextWidth(iName + 'XXX'));
    iStringGridIntf.ControlCells[0, RowCount - 1] := iName;
    iStringGridIntf.ControlCells[1, RowCount - 1] := iValue;
    iStringGridIntf.ControlRowHeights[RowCount - 1] := Max(22, Canvas.TextHeight(iValue));
  end;

begin
  if not Supports(iInspector, IJvDynControlStringGrid, iStringGridIntf) then
    Exit;
  iStringGridIntf.ControlRowCount := 0;
  RowCount := 0;
  ColWidth := 0;
  if Assigned(iXMLItem) then
  begin
    if iXMLItem.Value <> '' then
      AddRow('Value',iXMLItem.Value);
    if iXMLItem.PropertyCount > 0 then
      AddRow('Properties','');
    for i := 0 to iXMLItem.PropertyCount-1 do
    begin
      Prop := iXMLItem.Properties[i];
      AddRow('    '+Prop.Name, Prop.Value);
    end;
    for i := 0 to iXMLItem.ItemCount-1 do
    begin
      Item := iXMLItem.Items[i];
      if Item.ItemCount <= 0 then
      begin
        AddRow('Elements','');
        Break;
      end;
    end;
    for i := 0 to iXMLItem.ItemCount-1 do
    begin
      Item := iXMLItem.Items[i];
      if Item.ItemCount > 0 then
        Continue;
      AddRow('    '+Item.Name, Item.Value);
    end;
    iStringGridIntf.ControlColWidths[0] := ColWidth;
    iStringGridIntf.ControlColWidths[1] := iInspector.Width - ColWidth - 2;
  end;
  if iStringGridIntf.ControlRowCount > 0 then
    iStringGridIntf.ControlColCount := 2
  else
    iStringGridIntf.ControlColCount := 0;
end;

procedure TJvXMLBrowserControl.TransferXMLItemToTreeNode(iXMLItem: TJclSimpleXMLElem; iParent: TTreeNode; iTreeNodes:
  TTreenodes);
var
  XMLItem: TJclSimpleXMLElem;
  Node: TTreeNode;
  NameProp : TJclSimpleXMLProp;
  i : Integer;
begin
  if not (Assigned(iXMLItem) and Assigned(iTreeNodes)) then
    Exit;
  NameProp := iXMLItem.Properties.ItemNamed[ItemNameProperty];
  if Assigned(NameProp) and (NameProp.Value <> '') then
    Node := iTreeNodes.AddChildObject(iParent, iXMLItem.Name+' : '+NameProp.Value, iXMLItem)
  else
    Node := iTreeNodes.AddChildObject(iParent, iXMLItem.Name, iXMLItem);
  for i := 0 to iXMLItem.ItemCount-1 do
  begin
    XMLItem := iXMLItem.Items[i];
    if (XMLItem.ItemCount+XMLItem.PropertyCount <= 0) then
      Continue;
    TransferXMLItemToTreeNode(XMLItem, Node, iTreeNodes);
  end;
end;

procedure TJvXMLBrowserControl.TransferXMLToTreeNodes(iXML: TJvSimpleXML; iTreeNodes: TTreenodes);
begin
  iTreeNodes.Clear;
  if Assigned(iXML) then
    TransferXMLItemToTreeNode(iXML.Root, nil, iTreeNodes);
end;

procedure TJvXMLBrowserControl.XMLTreeViewChange(Sender:
  TObject; Node: TTreeNode);
begin
  if csDestroying in Componentstate then
    Exit;
  if not Assigned(Node) or
    not Assigned(Node.Data) or
    not (TObject(Node.Data) is TJclSimpleXMLElem) then
    InspectedObject := nil
  else
    InspectedObject := TJclSimpleXMLElem(Node.Data);
end;

procedure TJvXMLBrowserControl.XMLTreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
var
  JvPropertyEditorHandler: IJvPropertyEditorHandler;
begin
  if csDestroying in Componentstate then
    Exit;
  if Assigned(XMLTreeViewIntf.ControlSelected) and
    Assigned(XMLTreeViewIntf.ControlSelected.Data) and
    (TObject(XMLTreeViewIntf.ControlSelected.Data) is TPersistent) then
    if Supports(TObject(XMLTreeViewIntf.ControlSelected.Data),
      IJvPropertyEditorHandler, JvPropertyEditorHandler) then
      if (JvPropertyEditorHandler.EditIntf_GetVisibleObjectName <> '') then
        XMLTreeViewIntf.ControlSelected.Text := JvPropertyEditorHandler.EditIntf_GetVisibleObjectName;
end;

{$IFDEF UNITVERSIONING}

initialization

RegisterUnitVersion(HInstance, UnitVersioning);

finalization

UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
