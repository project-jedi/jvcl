{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListTreeView.PAS, released on 2003-01-22.

The Initial Developer of the Original Code is Peter Th�rnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Th�rnqvist are Copyright (C) 2003 Peter Th�rnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Changes:
2002-10-22:
  Changed TJvPageIndexNode.SetPageIndex to only set the parent PageIndex if the Treeview is a
  TJvCustomSettingsTreeView since this is the first class implementing this behaviour

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvPageListTreeView;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, ImgList, ComCtrls,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvPageList, JvExComCtrls;

type
  TJvCustomPageListTreeView = class;

  TJvPageIndexNode = class(TTreeNode)
  private
    FPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
  published
    procedure Assign(Source: TPersistent); override;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  end;

  TJvPageIndexNodes = class(TTreeNodes)
  private
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

  // this is  a "fake" class so we have something to anchor the design-time editor with
  TJvPageLinks = class(TPersistent)
  private
    FTreeView: TJvCustomPageListTreeView;
  public
    property TreeView: TJvCustomPageListTreeView read FTreeView;
  end;

  { TJvCustomPageListTreeView is a base treeview class that can be hooked up with an IPageList
    implementor. When the selected tree node is changed, the associated page in the IPageList is changed too as
    determined by the TJvPageIndexNode.PageIndex property
    Properties:
    * PageDefault is the default PageIndex to assign to new nodes
    * PageLinks is the property used att design time to set up a Nodes PageIndex. At run-time, use
      TJvPageIndexNode(Node).PageIndex := Value;
    * PageList is the IPageList implementor that is attached to this control
    * CanChange calls IPageList.CanChange method and Change calls IPageList.SetActivePageIndex
    * IPageList.getPageCaption is only used by the design-time editor for the PageLinks property
    }

  TJvCustomPageListTreeView = class(TJvExCustomTreeView)
  private
    FItems: TJvPageIndexNodes;
    FPageList: IPageList;
    FPageDefault: Integer;
    FLinks: TJvPageLinks;
    FMemStream: TMemoryStream;
    procedure SetPageDefault(const Value: Integer);
    procedure SetLinks(const Value: TJvPageLinks);
    procedure SetPageList(const Value: IPageList);
    function GetItems: TJvPageIndexNodes;
    procedure SetItems(const Value: TJvPageIndexNodes);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateNode: TTreeNode;  override;
    function CreateNodes: TTreeNodes; override; 
    function CanChange(Node: TTreeNode): Boolean;  override;
    procedure Change(Node: TTreeNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageDefault: Integer read FPageDefault write SetPageDefault;
    property PageLinks: TJvPageLinks read FLinks write SetLinks;
    property PageList: IPageList read FPageList write SetPageList;
  protected
    property AutoExpand default True;
    property ShowButtons default False;
    property ShowLines default False;
    property ReadOnly default True;
    property Items: TJvPageIndexNodes read GetItems write SetItems;
  end;

  { TJvSettingsTreeImages is a property class that describes the images used in a
    TJvCustomSettingsTreeView as the usage of images in this control differs from the normal
    TreeView
  }
  TJvSettingsTreeImages = class(TPersistent)
  private
    FSelectedIndex: TImageIndex;
    FCollapsedIndex: TImageIndex;
    FImageIndex: TImageIndex;
    FExpandedIndex: TImageIndex;
    FTreeView: TJvCustomPageListTreeView;
  public
    constructor Create;
    property TreeView: TJvCustomPageListTreeView read FTreeView write FTreeView;
  published
    property CollapsedIndex: TImageIndex read FCollapsedIndex write FCollapsedIndex default 0;
    property ExpandedIndex: TImageIndex read FExpandedIndex write FExpandedIndex default 1;
    property SelectedIndex: TImageIndex read FSelectedIndex write FSelectedIndex default 2;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex default -1;
  end;

  { TJvCustomSettingsTreeView is a base class for treeviews that behave like the
    treeview in the Settings Dialog in Visual Studio: When a node in the treeview
    is selected, a new page of settings is shown on a panel to the right.

    Specifically, the following is True:

    * The normal ImageIndex/SelectedIndex is ignored for nodes - use PageNodeImages instead. You still
      need to assign a TImageList to the Images property
    * When a node is expanded, it is assigned the expanded image until it is collapsed, regardless
      whether it's selected or not
    * When a parent folder is selected, the first non-folder child has it's
      normal image set as the selected image
    * By default, AutoExpand and ReadOnly is True, ShowButtons and ShowLines are False

    Other than that, it should work like a normal TreeView. Note that the treeview was designed with AutoExpand = True
    in mind but should work with AutoExpand = False

    To get the VS look , Images should contain:
      Image 0: Closed Folder
      Image 1: Open Folder
      Image 2: Right-pointing teal-colored arrow

      PageNodeImages should then be set to (the defaults):
        ClosedFolder = 0;
        ImageIndex = -1; (no image)
        OpenFolder = 1;
        SelectedIndex = 2;
    }

  TJvCustomSettingsTreeView = class(TJvCustomPageListTreeView)
  private
    FNodeImages: TJvSettingsTreeImages;
    FOnGetImageIndex: TTVExpandedEvent;
    FOnGetSelectedIndex: TTVExpandedEvent;
    procedure SetImageSelection(const Value: TJvSettingsTreeImages);
  protected
    FLastSelected: TTreeNode;
    procedure Delete(Node: TTreeNode); override;
    procedure DoGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure DoGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure GetSelectedIndex(Node: TTreeNode); override;
    procedure GetImageIndex(Node: TTreeNode);  override;
    function CanChange(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure ResetPreviousNode(NewNode: TTreeNode); virtual;
    procedure SetPreviousNode(NewNode: TTreeNode); virtual;
    procedure Loaded; override;
    procedure Expand(Node: TTreeNode); override;
    procedure Collapse(Node: TTreeNode); override;
    property PageNodeImages: TJvSettingsTreeImages read FNodeImages write SetImageSelection;
    property OnGetImageIndex: TTVExpandedEvent read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent read FOnGetSelectedIndex write FOnGetSelectedIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvPageListTreeView = class(TJvCustomPageListTreeView)
  published
    property AutoExpand;
    property ShowButtons;
    property ShowLines;
    property ReadOnly;
    property PageDefault;
    property PageLinks;
    property PageList;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property DragKind;
    property DragCursor;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderWidth;
    property ChangeDelay;
    property HideSelection;
    property HotTrack;
    property MultiSelect;
    property MultiSelectStyle;
    property StateImages;
    property ToolTips;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property ShowRoot;
    property RightClickSelect;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property Indent;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowSelect;
    property ShowHint;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnCompare;
    property OnAddition;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Items;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvSettingsTreeView = class(TJvCustomSettingsTreeView)
  published
    property AutoExpand;
    property ShowButtons;
    property ShowLines;
    property ReadOnly;
    property PageDefault;
    property PageNodeImages;
    property PageLinks;
    property PageList;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property ChangeDelay;
    property DragKind;
    property DragCursor;
    property HideSelection;
    property HotTrack;
    property ParentBiDiMode;
    property OnAddition;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnEndDock;
    property OnStartDock;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCompare;
    property RightClickSelect;
    property ShowRoot;
    property StateImages;
    property ToolTips;
    property BorderStyle;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property Indent;
    // don't use!
//    property MultiSelect;
//    property MultiSelectStyle;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowSelect;
    property ShowHint;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanding;
    property OnExpanded;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Items;
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
  Forms;

procedure ResetSiblingFolders(Node: TTreeNode; ImageIndex, SelectedIndex: Integer; Recurse: Boolean = False);
var
  N: TTreeNode;
begin
  N := Node.getPrevSibling;
  while Assigned(N) do
  begin
    if N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
      if Recurse then
        ResetSiblingFolders(N.getFirstChild, ImageIndex, SelectedIndex, Recurse);
    end;
    N := N.getPrevSibling;
  end;
  N := Node.getNextSibling;
  while Assigned(N) do
  begin
    if N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
      if Recurse then
        ResetSiblingFolders(N.getFirstChild, ImageIndex, SelectedIndex, Recurse);
    end;
    N := N.getNextSibling;
  end;
end;

//=== { TJvCustomPageListTreeView } ==========================================

constructor TJvCustomPageListTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinks := TJvPageLinks.Create;
  FLinks.FTreeView := Self;
  ReadOnly := True;
  ShowLines := False;
  AutoExpand := True;
  ShowButtons := False;
end;

destructor TJvCustomPageListTreeView.Destroy;
begin
  FLinks.Free;
  FMemStream.Free;
  inherited Destroy;
end;

function TJvCustomPageListTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := inherited CanChange(Node);
  if Result and Assigned(Node) and Assigned(FPageList) then
    Result := FPageList.CanChange(TJvPageIndexNode(Node).PageIndex);
end;

procedure TJvCustomPageListTreeView.Change(Node: TTreeNode);
var
  I: Integer;
begin
  inherited Change(Node);
  if Assigned(FPageList) and Assigned(Node) then
  begin
    I := TJvPageIndexNode(Node).PageIndex;
    if (I >= 0) and (I < FPageList.GetPageCount) then
      FPageList.SetActivePageIndex(I)
    else
    if (PageDefault >= 0) and (PageDefault < FPageList.GetPageCount) then
      FPageList.SetActivePageIndex(PageDefault)
    else
      FPageList.SetActivePageIndex(-1);
  end;
end;

function TJvCustomPageListTreeView.CreateNode: TTreeNode;
begin
  Result := TJvPageIndexNode.Create(Items);
  TJvPageIndexNode(Result).PageIndex := PageDefault;
end;

function TJvCustomPageListTreeView.CreateNodes: TTreeNodes;
begin
  if (FItems = nil) and not (csDestroying in ComponentState) then
    FItems := TJvPageIndexNodes.Create(Self);
  Result := FItems;
end;

procedure TJvCustomPageListTreeView.CreateWnd;
begin
  inherited CreateWnd;

  if FMemStream <> nil then
  begin
    Items.BeginUpdate;
    try
      Items.ReadData(FMemStream);
      FreeAndNil(FMemStream);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TJvCustomPageListTreeView.DestroyWnd;
begin
  if CreateWndRestores and
     {$IFDEF COMPILER10_UP}
     (csRecreating in ControlState) and
     {$ENDIF COMPILER10_UP}
     (Items.Count > 0) then
  begin
    FMemStream := TMemoryStream.Create;
    Items.WriteData(FMemStream);
    FMemStream.Position := 0;
  end;

  inherited DestroyWnd;
end;

procedure TJvCustomPageListTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if AComponent.IsImplementorOf(PageList) then
      PageList := nil;
  end;
end;

procedure TJvCustomPageListTreeView.SetPageDefault(const Value: Integer);
var
  N: TTreeNode;
begin
  if FPageDefault <> Value then
  begin
    N := Items.GetFirstNode;
    while Assigned(N) do
    begin
      if TJvPageIndexNode(N).PageIndex = FPageDefault then
        TJvPageIndexNode(N).PageIndex := Value;
      N := N.GetNext;
    end;
    FPageDefault := Value;
  end;
end;

procedure TJvCustomPageListTreeView.SetLinks(const Value: TJvPageLinks);
begin
  FLinks.Assign(Value);
end;

procedure TJvCustomPageListTreeView.SetPageList(const Value: IPageList);
begin
  if FPageList <> Value then
  begin
    ReferenceInterface(FPageList, opRemove);
    FPageList := Value;
    ReferenceInterface(FPageList, opInsert);
  end;
end;

function TJvCustomPageListTreeView.GetItems: TJvPageIndexNodes;
begin
  Result := TJvPageIndexNodes(CreateNodes);
end;

procedure TJvCustomPageListTreeView.SetItems(const Value: TJvPageIndexNodes);
begin
  inherited Items := Value;
end;

//=== { TJvPageIndexNode } ===================================================

procedure TJvPageIndexNode.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TJvPageIndexNode then
    PageIndex := TJvPageIndexNode(Source).PageIndex;
end;

procedure TJvPageIndexNode.SetPageIndex(const Value: Integer);
begin
  if FPageIndex <> Value then
  begin
    FPageIndex := Value;
    if (TreeView is TJvCustomSettingsTreeView) and (Parent <> nil) and
      (Parent.getFirstChild = Self) and not HasChildren then
      TJvPageIndexNode(Parent).PageIndex := Value;
  end;
end;

//=== { TJvPageIndexNodes } ==================================================

procedure TJvPageIndexNodes.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Links', ReadData, WriteData, True);
end;

procedure TJvPageIndexNodes.ReadData(Stream: TStream);
var
  APageIndex, ACount: Integer;
  LNode: TTreeNode;
  LHandleAllocated: Boolean;
begin
  LHandleAllocated := Owner.HandleAllocated;
  if LHandleAllocated then
    BeginUpdate;
  try
    Stream.Read(ACount, SizeOf(ACount));
    if ACount > 0 then
    begin
      LNode := GetFirstNode;
      while Assigned(LNode) and (ACount > 0) do
      begin
        Stream.Read(APageIndex, SizeOf(APageIndex));
        TJvPageIndexNode(LNode).PageIndex := APageIndex;
        LNode := LNode.GetNext;
        Dec(ACount);
      end;
      // read any "left-overs" (should never happen)
      while ACount > 0 do
      begin
        Stream.Read(APageIndex, SizeOf(APageIndex));
        Dec(ACount);
      end;
    end;
  finally
    if LHandleAllocated then
      EndUpdate;
  end;
end;

procedure TJvPageIndexNodes.WriteData(Stream: TStream);
var
  Node: TTreeNode;
  APageIndex: Integer;
  ACount: Integer;
begin
  ACount := Count;
  Stream.Write(ACount, SizeOf(Count));
  if ACount > 0 then
  begin
    Node := GetFirstNode;
    while (Node <> nil) do
    begin
      APageIndex := TJvPageIndexNode(Node).PageIndex;
      Stream.Write(APageIndex, SizeOf(APageIndex));
      Node := Node.GetNext;
    end;
  end;
end;

//=== { TJvSettingsTreeImages } ==============================================

constructor TJvSettingsTreeImages.Create;
begin
  inherited Create;
  FCollapsedIndex := 0;
  FExpandedIndex := 1;
  FSelectedIndex := 2;
  FImageIndex := -1;
end;

//=== { TJvCustomSettingsTreeView } ==========================================

constructor TJvCustomSettingsTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNodeImages := TJvSettingsTreeImages.Create;
  FNodeImages.TreeView := Self;
  AutoExpand := True;
  ShowButtons := False;
  ReadOnly := True;
  ShowLines := False;
  // we need to assign to these since the TTreeView checks if they are assigned
  // and won't call GetImageIndex without them
  inherited OnGetImageIndex := DoGetImageIndex;
  inherited OnGetSelectedIndex := DoGetSelectedIndex;
end;

destructor TJvCustomSettingsTreeView.Destroy;
begin
  FNodeImages.TreeView := nil;
  FNodeImages.Free;
  inherited Destroy;
end;

function TJvCustomSettingsTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := inherited CanChange(Node);
  if Result and (Selected <> nil) and not Selected.HasChildren then // Selected is the previous selected node
  begin
    Selected.ImageIndex := FNodeImages.ImageIndex;
    Selected.SelectedIndex := FNodeImages.ImageIndex;
  end;
end;

procedure TJvCustomSettingsTreeView.Change(Node: TTreeNode);
begin
  inherited Change(Node);
  if not AutoExpand and Node.Expanded then
    Node.Expand(False); // refresh node and children
end;

procedure TJvCustomSettingsTreeView.Collapse(Node: TTreeNode);
begin
  inherited Collapse(Node);
  if Node.HasChildren then
  begin
    Node.ImageIndex := FNodeImages.CollapsedIndex;
    Node.SelectedIndex := FNodeImages.CollapsedIndex;
  end;
end;

procedure TJvCustomSettingsTreeView.Delete(Node: TTreeNode);
begin
  inherited Delete(Node);
  if Node = FLastSelected then
    FLastSelected := nil;
end;

procedure TJvCustomSettingsTreeView.DoGetImageIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(FOnGetImageIndex) then
    FOnGetImageIndex(Sender, Node)
  else
    GetImageIndex(Node);
end;

procedure TJvCustomSettingsTreeView.DoGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(FOnGetSelectedIndex) then
    FOnGetSelectedIndex(Sender, Node)
  else
    GetSelectedIndex(Node);
end;

procedure TJvCustomSettingsTreeView.Expand(Node: TTreeNode);
var
  N: TTreeNode;
  R: TRect;
begin
  if Node.HasChildren then
  begin
    if AutoExpand then
      ResetSiblingFolders(Node, FNodeImages.CollapsedIndex, FNodeImages.CollapsedIndex, True);
    Node.ImageIndex := FNodeImages.ExpandedIndex;
    Node.SelectedIndex := FNodeImages.ExpandedIndex;
    N := Node.getFirstChild;
    if (N <> nil) and not N.HasChildren then
    begin
      ResetPreviousNode(N);
      N.ImageIndex := FNodeImages.SelectedIndex;
      N.SelectedIndex := FNodeImages.SelectedIndex;
      R := N.DisplayRect(False);
      Windows.InvalidateRect(Handle, @R, True);
      SetPreviousNode(N);
    end;
  end;
  inherited Expand(Node);
end;


procedure TJvCustomSettingsTreeView.GetImageIndex(Node: TTreeNode);
begin
  if Node.HasChildren then
  begin
    if Node.Expanded then
      Node.ImageIndex := FNodeImages.ExpandedIndex
    else
      Node.ImageIndex := FNodeImages.CollapsedIndex;
  end
  else
  if Node.Selected or
    ((Node.Parent <> nil) and Node.Parent.Selected and
    (Node.Parent.getFirstChild = Node)) then
  begin
    ResetPreviousNode(Node);
    Node.ImageIndex := FNodeImages.SelectedIndex;
    SetPreviousNode(Node);
  end
  else
    Node.ImageIndex := FNodeImages.ImageIndex;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TJvCustomSettingsTreeView.GetSelectedIndex(Node: TTreeNode);
begin
  GetImageIndex(Node);
end;

procedure TJvCustomSettingsTreeView.Loaded;
begin
  inherited Loaded;
  if Items.Count > 0 then
  begin
    ResetSiblingFolders(Items[0], FNodeImages.CollapsedIndex, FNodeImages.CollapsedIndex, True);
    Items[0].MakeVisible;
  end;
end;

procedure TJvCustomSettingsTreeView.ResetPreviousNode(NewNode: TTreeNode);
var
  R: TRect;
begin
  if (FLastSelected <> nil) and (FLastSelected <> NewNode) and
    (NewNode <> nil) and not NewNode.HasChildren then
  begin
    FLastSelected.ImageIndex := FNodeImages.ImageIndex;
    FLastSelected.SelectedIndex := FNodeImages.ImageIndex;
    R := FLastSelected.DisplayRect(False);
    Windows.InvalidateRect(Handle, @R, True);
  end;
end;

procedure TJvCustomSettingsTreeView.SetImageSelection(const Value: TJvSettingsTreeImages);
begin
  //  FNodeImages := Value;
end;

procedure TJvCustomSettingsTreeView.SetPreviousNode(NewNode: TTreeNode);
begin
  FLastSelected := NewNode;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
