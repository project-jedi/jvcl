{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListTreeView.PAS, released on 2003-01-22.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-04-08

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}
{$I WINDOWSONLY.INC}
unit JvPageListTreeView;
{
Changes:
2002-10-22:
  Changed TJvPageIndexNode.SetPageIndex to only set the parent PageIndex if the Treeview is a
  TJvCustomSettingsTreeView since this is the first class implementing this behaviour
}
interface
uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls,
  ImgList, ComCtrls, JVCLVer;

type
  EPageListError = class(Exception);

  IPageList = interface
    ['{6BB90183-CFB1-4431-9CFD-E9A032E0C94C}']
    function CanChange(AIndex: integer): boolean;
    procedure SetActivePageIndex(AIndex: integer);
    function getPageCount: integer;
    function getPageCaption(AIndex: integer): string;
  end;

  TJvCustomPageList = class;
  TJvCustomPageListTreeView = class;

  TJvPageIndexNode = class(TTreeNode)
  private
    FPageIndex: integer;
    procedure SetPageIndex(const Value: integer);
  published
    procedure Assign(Source: TPersistent); override;
    property PageIndex: integer read FPageIndex write SetPageIndex;
  end;

  TJvPageIndexNodes = class(TTreeNodes)
  private
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  end;

  TJvPagePaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect) of object;
  TJvPageCanPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas; ARect: TRect; var DefaultDraw: boolean) of object;
  { TJvCustomPage is the base class for pages in a TJvPageList and implements the basic behaviour of such
    a control. It has support for accepting components, propagating it's Enabled state, changing it's order in the
    page list and custom painting }
  TJvCustomPage = class(TCustomControl)
  private
    FPageList: TJvCustomPageList;
    FOnBeforePaint: TJvPageCanPaintEvent;
    FOnPaint: TJvPagePaintEvent;
    FOnAfterPaint: TJvPagePaintEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMTextchanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;

    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetPageList(const Value: TJvCustomPageList);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    function DoBeforePaint(ACanvas: TCanvas; ARect: TRect): boolean; dynamic;
    procedure DoAfterPaint(ACanvas: TCanvas; ARect: TRect); dynamic;
    procedure DoPaint(ACanvas: TCanvas; ARect: TRect); virtual;
    procedure DoShow; virtual;
    procedure DoHide; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageList: TJvCustomPageList read FPageList write SetPageList;
  protected
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;

    property PageIndex: integer read GetPageIndex write SetPageIndex stored False;

    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnBeforePaint: TJvPageCanPaintEvent read FOnBeforePaint write FOnBeforePaint;
    property OnPaint: TJvPagePaintEvent read FOnPaint write FOnPaint;
    property OnAfterPaint: TJvPagePaintEvent read FOnAfterPaint write FOnAfterPaint;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvCustomPageClass = class of TJvCustomPage;
  TJvPageChangingEvent = procedure(Sender: TObject; PageIndex: integer; var AllowChange: boolean) of object;

  {
   TJvCustomPageList is a base class for components that implements the IPageList interface.
    It works like TPageControl but does not have any tabs
   }
  TJvShowDesignCaption = (sdcNone, sdcTopLeft, sdcTopCenter, sdcTopRight, sdcLeftCenter, sdcCenter, sdcRightCenter, sdcBottomLeft, sdcBottomCenter, sdcBottomRight);
  TJvCustomPageList = class(TCustomControl, IUnknown, IPageList)
  private
    FPages: TList;
    FActivePage: TJvCustomPage;
    FPropagateEnable: boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TJvPageChangingEvent;
    FShowDesignCaption: TJvShowDesignCaption;
    FAboutJVCL: TJVCLAboutInfo;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    procedure CMDesignHitTest(var Message: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure UpdateEnabled;
    procedure SetActivePage(Page: TJvCustomPage);
    procedure SetPropagateEnable(const Value: boolean);
    procedure SetShowDesignCaption(const Value: TJvShowDesignCaption);
    function GetPage(Index: integer): TJvCustomPage;
  protected
    { IPageList }
    function CanChange(AIndex: integer): boolean;
    function GetActivePageIndex: integer;
    procedure SetActivePageIndex(AIndex: integer);
    function GetPageFromIndex(AIndex: integer): TJvCustomPage;
    function getPageCount: integer;
    function getPageCaption(AIndex: integer): string;
    procedure Paint; override;

    procedure Change; dynamic;
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ShowControl(AControl: TControl); override;
    function InternalGetPageClass: TJvCustomPageClass; virtual;

    procedure InsertPage(APage: TJvCustomPage);
    procedure RemovePage(APage: TJvCustomPage);
    property PageList: TList read FPages;
    property PropagateEnable: boolean read FPropagateEnable write SetPropagateEnable;
    property ShowDesignCaption: TJvShowDesignCaption read FShowDesignCaption write SetShowDesignCaption default sdcCenter;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TJvPageChangingEvent read FOnChanging write FOnChanging;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindNextPage(CurPage: TJvCustomPage; GoForward: boolean; IncludeDisabled: boolean): TJvCustomPage;
    procedure PrevPage;
    procedure NextPage;
    function GetPageClass: TJvCustomPageClass;
    property Height default 200;
    property Width default 300;

    property ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    property ActivePage: TJvCustomPage read FActivePage write SetActivePage;
    property Pages[Index:integer]:TJvCustomPage read GetPage;
    property PageCount: integer read getPageCount;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvStandardPage = class(TJvCustomPage)
  published
    property BorderWidth;
    property Caption;
    property Color;
    property DragMode;
    property Enabled;
    property Font;
    property Constraints;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property PageIndex;

    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;

    property OnBeforePaint;
    property OnPaint;
    property OnAfterPaint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
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
    * PageList is the IPageList implementor that is attached to this control. NOTE: for D5, PageList is
      instead a TComponent: to get at the IPageList interface, use PageListIntf instead
    * CanChange calls IPageList.CanChange method and Change calls IPageList.SetActivePageIndex
    * IPageList.getPageCaption is only used by the design-time editor for the PageLinks property
    }

  TJvCustomPageListTreeView = class(TCustomTreeView)
  private
    FItems:TJvPageIndexNodes;
    FPageList: IPageList;
    FPageDefault: integer;
    FLinks: TJvPageLinks;
    FAboutJVCL: TJVCLAboutInfo;
    FOnCtl3DChanged: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    {$IFNDEF COMPILER6_UP}
    FPageListComponent: TComponent;
    procedure SetPageListComponent(const Value: TComponent);
    {$ENDIF}
    procedure SetPageDefault(const Value: integer);
    procedure SetLinks(const Value: TJvPageLinks);
    procedure SetPageList(const Value: IPageList);

    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    function GetItems: TJvPageIndexNodes;
    procedure SetItems(const Value: TJvPageIndexNodes);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CreateNode: TTreeNode; override;
    function CreateNodes: TTreeNodes; {$IFDEF COMPILER6_UP} override; {$ENDIF}

    function CanChange(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageDefault: integer read FPageDefault write SetPageDefault;
    property PageLinks: TJvPageLinks read FLinks write SetLinks;
    {$IFDEF COMPILER6_UP}
    property PageList: IPageList read FPageList write SetPageList;
    {$ELSE}
    property PageListIntf: IPageList read FPageList write SetPageList;
    property PageList: TComponent read FPageListComponent write SetPageListComponent;
    {$ENDIF}
  protected
    property AutoExpand default true;
    property ShowButtons default false;
    property ShowLines default false;
    property ReadOnly default true;
    property Items:TJvPageIndexNodes read GetItems write SetItems;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;

  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
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

    Specifically, the following is true:

    * The normal ImageIndex/SelectedIndex is ignored for nodes - use PageNodeImages instead. You still
      need to assign a TImageList to the Images property
    * When a node is expanded, it is assigned the expanded image until it is collapsed, regardless
      whether it's selected or not
    * When a parent folder is selected, the first non-folder child has it's
      normal image set as the selected image
    * By default, AutoExpand and ReadOnly is true, ShowButtons and ShowLines are false

    Other than that, it should work like a normal TreeView. Note that the treeview was designed with AutoExpand = true
    in mind but should work with AutoExpand = false

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
    procedure GetImageIndex(Node: TTreeNode); override;
    procedure GetSelectedIndex(Node: TTreeNode); override;
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
    property OnCtl3DChanged;
    property OnParentColorChange;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    {$IFDEF COMPILER6_UP}
    property MultiSelect;
    property MultiSelectStyle;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property RowSelect;
    property ShowHint;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    {$IFDEF COMPILER6_UP}
    property OnAddition;
    {$ENDIF}
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    {$IFDEF COMPILER6_UP}
    property OnCreateNodeClass;
    {$ENDIF}
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
    property Items;
  end;

  TJvSettingsTreeView = class(TJvCustomSettingsTreeView)
  published
    property AutoExpand default true;
    property ShowButtons default false;
    property ShowLines default false;
    property ReadOnly default true;
    property PageDefault;
    property PageNodeImages;
    property PageLinks;
    property PageList;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    // don't use!
//    property MultiSelect;
//    property MultiSelectStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property RowSelect;
    property ShowHint;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    {$IFDEF COMPILER6_UP}
    property OnAddition;
    {$ENDIF}
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    {$IFDEF COMPILER6_UP}
    property OnCreateNodeClass;
    {$ENDIF}
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
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
    property OnStartDock;
    property OnStartDrag;
    property Items;
  end;

  TJvPageList = class(TJvCustomPageList)
  protected
    function InternalGetPageClass: TJvCustomPageClass; override;
  public
    property PageCount;
  published
    property ActivePage;
    property PropagateEnable;
    property ShowDesignCaption;

    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;

    property OnCanResize;
    property OnChange;
    property OnChanging;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
uses
  Forms;

type
  THackTab = class(TCustomTabControl);

procedure ResetAllNonParentNodes(Items: TTreeNodes; ImageIndex, SelectedIndex: integer);
var N: TTreeNode;
begin
  N := Items.GetFirstNode;
  while Assigned(N) do
  begin
    if not N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
    end;
    N := N.GetNext;
  end;
end;

procedure ResetSiblings(Node: TTreeNode; ImageIndex, SelectedIndex: integer; Recurse: boolean = false);
var N: TTreeNode;
begin
  N := Node.getPrevSibling;
  while Assigned(N) do
  begin
    if not N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
    end
    else if Recurse then
      ResetSiblings(N.getFirstChild, ImageIndex, SelectedIndex, Recurse);
    N := N.getPrevSibling;
  end;
  N := Node.getNextSibling;
  while Assigned(N) do
  begin
    if not N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
    end
    else if Recurse then
      ResetSiblings(N.getFirstChild, ImageIndex, SelectedIndex, Recurse);
    N := N.getNextSibling;
  end;
end;

procedure ResetSiblingFolders(Node: TTreeNode; ImageIndex, SelectedIndex: integer; Recurse: boolean = false);
var N: TTreeNode;
begin
  N := Node.getPrevSibling;
  while Assigned(N) do
  begin
    if N.HasChildren then
    begin
      N.ImageIndex := ImageIndex;
      N.SelectedIndex := SelectedIndex;
      if Recurse then
        ResetSiblingFolders(N.getFirstCHild, ImageINdex, SelectedIndex, Recurse);
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
        ResetSiblingFolders(N.getFirstChild, ImageINdex, SelectedIndex, Recurse);
    end;
    N := N.getNextSibling;
  end;
end;

{ TJvCustomPageListTreeView }

function TJvCustomPageListTreeView.CanChange(Node: TTreeNode): Boolean;
begin
  Result := inherited CanChange(Node);
  if Result and Assigned(Node) and Assigned(FPageList) then
    Result := FPageList.CanChange(TJvPageIndexNode(Node).PageIndex);
end;

procedure TJvCustomPageListTreeView.Change(Node: TTreeNode);
var i: integer;
begin
  inherited;
  if Assigned(FPageList) and Assigned(Node) then
  begin
    i := TJvPageIndexNode(Node).PageIndex;
    if (i >= 0) and (i < FPageList.getPageCount) then
      FPageList.SetActivePageIndex(i)
    else if (PageDefault >= 0) and (PageDefault < FPageList.getPageCount) then
      FPageList.SetActivePageIndex(PageDefault)
    else
      FPageList.SetActivePageIndex(-1);
  end;
end;

constructor TJvCustomPageListTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FLinks := TJvPageLinks.Create;
  FLinks.FTreeView := self;
end;

function TJvCustomPageListTreeView.CreateNode: TTreeNode;
begin
  Result := TJvPageIndexNode.Create(Items);
  TJvPageIndexNode(Result).PageIndex := PageDefault;
end;

destructor TJvCustomPageListTreeView.Destroy;
begin
  FLinks.Free;
  {$IFNDEF COMPILER_UP}
  // TreeNodes are destroyed by TCustomTreeview in D6 and above!!!
  FreeAndNil(FItems);
  {$ENDIF}
  inherited;
end;

function TJvCustomPageListTreeView.CreateNodes: TTreeNodes;
begin
  if (FItems = nil) and not (csDestroying in ComponentState) then
    FItems := TJvPageIndexNodes.Create(self);
  Result := FItems;
end;

procedure TJvCustomPageListTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
  {$IFDEF COMPILER6_UP}
    if AComponent.IsImplementorOf(PageList) then
      PageList := nil;
  {$ELSE}
    if (AComponent = FPageListComponent) then
      PageList := nil;
  {$ENDIF}
  end;
end;

procedure TJvCustomPageListTreeView.SetPageDefault(const Value: integer);
var N: TTreeNode;
begin
  if FPageDefault <> Value then
  begin
    N := Items.GetFirstNode;
    while Assigned(N) do
    begin
      if TJvPageIndexNode(N).PageIndex = FPageDefault then
        TJvPageIndexNode(N).PageIndex := Value;
      N := N.getNext;
    end;
    FPageDefault := Value;
  end;
end;

procedure TJvCustomPageListTreeView.SetLinks(const Value: TJvPageLinks);
begin
  //  FLinks.Assign(Value);
end;

procedure TJvCustomPageListTreeView.SetPageList(
  const Value: IPageList);
begin
  if FPageList <> Value then
  begin
    {$IFDEF COMPILER6_UP}
    ReferenceInterface(FPageList, opRemove);
    {$ENDIF}
    FPageList := Value;
    {$IFDEF COMPILER6_UP}
    ReferenceInterface(FPageList, opInsert);
    {$ENDIF}
  end;
end;

procedure TJvCustomPageListTreeView.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(self);
end;

procedure TJvCustomPageListTreeView.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TJvCustomPageListTreeView.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomPageListTreeView.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{$IFNDEF COMPILER6_UP}
resourcestring
  SInterfaceNotSupported = '%s does not support the %s interface';

procedure TJvCustomPageListTreeView.SetPageListComponent(const Value: TComponent);
var obj: IPageList;
begin
  if Value <> FPageListComponent then
  begin
    if FPageListComponent <> nil then
      FPageListComponent.RemoveFreeNotification(self);
    if Value = nil then
    begin
      FPageListComponent := nil;
      SetPageList(nil);
      Exit;
    end;
    if not Supports(Value, IPageList, obj) then
      raise EPageListError.CreateFmt(SInterfaceNotSupported, [Value.Name, 'IPageList']);
    SetPageList(obj);
    FPageListComponent := Value;
    FPageListComponent.FreeNotification(self);
  end;
end;
{$ENDIF}

function TJvCustomPageListTreeView.GetItems: TJvPageIndexNodes;
begin
  Result := TJvPageIndexNodes(CreateNodes);
end;

procedure TJvCustomPageListTreeView.SetItems(
  const Value: TJvPageIndexNodes);
begin
  inherited Items := Value;
end;

{ TJvPageIndexNode }

procedure TJvPageIndexNode.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TJvPageIndexNode) then
    PageIndex := TJvPageIndexNode(Source).PageIndex;
end;

procedure TJvPageIndexNode.SetPageIndex(const Value: integer);
begin
  if FPageIndex <> Value then
  begin
    FPageIndex := Value;
    if (TreeView is TJvCustomSettingsTreeView) and (Parent <> nil) and (Parent.getFirstChild = self) and not HasChildren then
      TJvPageIndexNode(Parent).PageIndex := Value;
  end;
end;

{ TJvPageIndexNodes }

procedure TJvPageIndexNodes.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Links', ReadData, WriteData, true);
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
    Stream.Read(ACount, sizeof(ACount));
    if ACount > 0 then
    begin
      LNode := GetFirstNode;
      while Assigned(LNode) and (ACount > 0) do
      begin
        Stream.Read(APageIndex, sizeof(APageIndex));
        TJvPageIndexNode(LNode).PageIndex := APageIndex;
        LNode := LNode.GetNext;
        Dec(ACount);
      end;
      // read any "left-overs" (should never happen)
      while ACount > 0 do
      begin
        Stream.Read(APageIndex, sizeof(APageIndex));
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
  APageIndex: integer;
  ACount: integer;
begin
  ACount := Count;
  Stream.Write(ACount, sizeof(Count));
  if ACount > 0 then
  begin
    Node := GetFirstNode;
    while (Node <> nil) do
    begin
      APageIndex := TJvPageIndexNode(Node).PageIndex;
      Stream.Write(APageIndex, sizeof(APageIndex));
      Node := Node.GetNext;
    end;
  end;
end;

{ TJvCustomPage }

constructor TJvCustomPage.Create(AOwner: TComponent);
begin
  inherited;
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  Color := clBtnFace;
  DoubleBuffered := True;
end;

procedure TJvCustomPage.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params.WindowClass do
    Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TJvCustomPage.Destroy;
begin
  if Assigned(PageList) then
    PageList := nil;
  inherited;
end;

procedure TJvCustomPage.DoAfterPaint(ACanvas: TCanvas; ARect: TRect);
begin
  if Assigned(FOnAfterPaint) then
    FOnAfterPaint(self, ACanvas, ARect);
end;

function TJvCustomPage.DoBeforePaint(ACanvas: TCanvas; ARect: TRect): boolean;
begin
  Result := true;
  if Assigned(FonBeforePaint) then
    FOnBeforePaint(self, ACanvas, ARect, Result);
end;

function GetDesignCaptionFlags(Value: TJvShowDesignCaption): Cardinal;
begin
  case Value of
    sdcTopLeft: Result := DT_TOP or DT_LEFT;
    sdcTopCenter: Result := DT_TOP or DT_CENTER;
    sdcTopRight: Result := DT_TOP or DT_RIGHT;
    sdcLeftCenter: Result := DT_VCENTER or DT_LEFT;
    sdcCenter: Result := DT_VCENTER or DT_CENTER;
    sdcRightCenter: Result := DT_VCENTER or DT_RIGHT;
    sdcBottomLeft: Result := DT_BOTTOM or DT_LEFT;
    sdcBottomCenter: Result := DT_BOTTOM or DT_CENTER;
    sdcBottomRight: Result := DT_BOTTOM or DT_RIGHT;
  else
    Result := 0;
  end;
end;

procedure TJvCustomPage.DoPaint(ACanvas: TCanvas; ARect: TRect);
var S: string;
begin
  with ACanvas do
  begin
    Font := self.Font;
    Brush.Style := bsSolid;
    Brush.Color := self.Color;
    FillRect(ARect);
    if (csDesigning in ComponentState) then
    begin
      Pen.Style := psDot;
      Pen.Color := clBlack;
      Brush.Style := bsClear;
      Rectangle(ARect);
      Brush.Style := bsSolid;
      Brush.Color := Color;
      if (PageList <> nil) and (PageList.ShowDesignCaption <> sdcNone) then
      begin
        S := Caption;
        if S = '' then
          S := Name;
        // make some space around the edges
        InflateRect(ARect, -4, -4);
        if not Enabled then
        begin
          SetBkMode(Handle, Windows.TRANSPARENT);
          Canvas.Font.Color := clHighlightText;
          DrawText(Handle, PChar(S), Length(S), ARect, GetDesignCaptionFlags(PageList.ShowDesignCaption) or DT_SINGLELINE);
          OffsetRect(ARect, -1, -1);
          Canvas.Font.Color := clGrayText;
        end;
        DrawText(Handle, PChar(S), Length(S), ARect, GetDesignCaptionFlags(PageList.ShowDesignCaption) or DT_SINGLELINE);
        InflateRect(ARect, 4, 4);
      end;
    end;
  end;
  if Assigned(FOnPaint) then
    FOnPaint(self, ACanvas, ARect);
end;

function TJvCustomPage.GetPageIndex: Integer;
begin
  if Assigned(FPageList) then
    Result := FPageList.PageList.IndexOf(self)
  else
    Result := -1;
end;

procedure TJvCustomPage.Paint;
var R: TRect;
begin
  R := ClientRect;
  if DoBeforePaint(Canvas, R) then
    DoPaint(Canvas, R);
  DoAfterPaint(Canvas, R);
end;

procedure TJvCustomPage.ReadState(Reader: TReader);
begin
  if Reader.Parent is TJvCustomPageList then
    PageList := TJvCustomPageList(Reader.Parent);
  inherited;
end;

procedure TJvCustomPage.SetPageList(
  const Value: TJvCustomPageList);
begin
  if FPageList <> Value then
  begin
    if Assigned(FPageList) then
      FPageList.RemovePage(self);
    FPageList := Value;
    Parent := FPageList;
    if FPageList <> nil then
      FPageList.InsertPage(self);
  end;
end;

procedure TJvCustomPage.SetPageIndex(const Value: Integer);
var
  OldIndex: Integer;
begin
  OldIndex := PageIndex;
  if Assigned(FPageList) and (Value <> OldIndex) and (Value >= 0) and (Value < FPageList.PageCount) then
    FPageList.PageList.Move(OldIndex, Value);
end;

procedure TJvCustomPage.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
  //  inherited;
end;

procedure TJvCustomPage.CMTextchanged(var Message: TMessage);
begin
  inherited;
  if csDesigning in ComponentState then
    Invalidate;
end;

procedure TJvCustomPage.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TJvCustomPage.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TJvCustomPage.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    try
      DoShow
    except
      Application.HandleException(Self);
    end;
  end
  else if not Showing then
  begin
    try
      DoHide;
    except
      Application.HandleException(Self);
    end;
  end;
end;

procedure TJvCustomPage.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(self);
end;

procedure TJvCustomPage.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TJvCustomPage.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomPage.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{ TJvCustomPageList }

function TJvCustomPageList.CanChange(AIndex: integer): boolean;
begin
  Result := (AIndex >= 0) and (AIndex < getPageCount);
  if Result and Assigned(FOnChanging) then
    FOnChanging(self, AIndex, Result);
end;

procedure TJvCustomPageList.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TJvCustomPageList.CMDesignHitTest(
  var Message: TCMDesignHitTest);
var
  Pt: TPoint;
begin
  Pt := SmallPointToPoint(Message.Pos);
  if Assigned(ActivePage) and PtInRect(ActivePage.BoundsRect, Pt) then
  begin
    Message.Result := 1;
  end;
end;

constructor TJvCustomPageList.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FPages := TList.Create;
  Height := 200;
  Width := 300;
  FShowDesignCaption := sdcCenter;
  ActivePageIndex := -1;
end;

destructor TJvCustomPageList.Destroy;
var i: integer;
begin
  for i := FPages.Count - 1 downto 0 do
    TJvCustomPage(FPages[i]).FPageList := nil;
  FPages.Free;
  inherited;
end;

procedure TJvCustomPageList.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  i: Integer;
  Control: TControl;
begin
  for i := 0 to FPages.Count - 1 do
  begin
    Proc(TComponent(FPages[i]));
  end;
  for i := 0 to ControlCount - 1 do
  begin
    Control := Controls[i];
    if not (Control is TJvCustomPage) and (Control.Owner = Root) then
    begin
      Proc(Control);
    end;
  end;
end;

function TJvCustomPageList.getPageCaption(AIndex: integer): string;
begin
  if (AIndex >= 0) and (AIndex < getPageCOunt) then
    Result := TJvCustomPage(FPages[AIndex]).Caption
  else
    Result := '';
end;

function TJvCustomPageList.InternalGetPageClass: TJvCustomPageClass;
begin
  Result := TJvCustomPage;
end;

function TJvCustomPageList.getPageCount: integer;
begin
  if FPages = nil then
    Result := 0
  else
    Result := FPages.Count;
end;

procedure TJvCustomPageList.InsertPage(APage: TJvCustomPage);
begin
  if (APage <> nil) and (FPages.IndexOf(APage) = -1) then
    FPages.Add(APage);
end;

procedure TJvCustomPageList.Loaded;
begin
  inherited;
  if getPageCount > 0 then
    ActivePage := Pages[0];
end;

procedure TJvCustomPageList.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and (getPageCount = 0) then
    with Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDot;
      Brush.Style := bsClear;
      Rectangle(ClientRect);
    end;
end;

procedure TJvCustomPageList.RemovePage(APage: TJvCustomPage);
var
  NextPage: TJvCustomPage;
begin
  NextPage := FindNextPage(APage, True, not (csDesigning in ComponentState));
  if NextPage = APage then
    NextPage := nil;
  APage.Visible := false;
  APage.FPageList := nil;
  FPages.Remove(APage);
  SetActivePage(NextPage);
end;

function TJvCustomPageList.GetPageFromIndex(AIndex: integer): TJvCustomPage;
begin
  if (AIndex >= 0) and (AIndex < getPageCount) then
    Result := TJvCustomPage(Pages[AIndex])
  else
    Result := nil;
end;

procedure TJvCustomPageList.SetActivePageIndex(AIndex: integer);
begin
  if (AIndex > -1) and (AIndex < PageCount) then
    ActivePage := Pages[AIndex]
  else
    ActivePage := nil;
end;

procedure TJvCustomPageList.ShowControl(AControl: TControl);
begin
  if (AControl is TJvCustomPage) then
    ActivePage := TJvCustomPage(AControl);
  inherited ShowControl(AControl);
end;

function TJvCustomPageList.GetPageClass: TJvCustomPageClass;
begin
  Result := InternalGetPageClass;
end;

procedure TJvCustomPageList.SetActivePage(Page: TJvCustomPage);
var
  ParentForm: TCustomForm;
begin
  if (csLoading in ComponentState) or ((Page <> nil) and (Page.PageList <> Self)) then
    Exit;
  if FActivePage <> Page then
  begin
    ParentForm := GetParentForm(Self);
    if (ParentForm <> nil) and (FActivePage <> nil) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) then
    begin
      ParentForm.ActiveControl := FActivePage;
      if ParentForm.ActiveControl <> FActivePage then
      begin
        ActivePage := GetPageFromIndex(FActivePage.PageIndex);
        Exit;
      end;
    end;
    if Page <> nil then
    begin
      Page.BringToFront;
      Page.Visible := True;
      if (ParentForm <> nil) and (FActivePage <> nil) and (ParentForm.ActiveControl = FActivePage) then
      begin
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
          ParentForm.ActiveControl := Self;
      end;
      Page.Refresh;
    end;
    if FActivePage <> nil then
      FActivePage.Visible := False;
    FActivePage := Page;
    if (ParentForm <> nil) and (FActivePage <> nil) and
      (ParentForm.ActiveControl = FActivePage) then
    begin
      FActivePage.SelectFirst;
    end;
  end;
end;

function TJvCustomPageList.GetActivePageIndex: integer;
begin
  if ACtivePage <> nil then
    Result := ActivePage.PageIndex
  else
    Result := -1;
end;

procedure TJvCustomPageList.NextPage;
begin
  if (ActivePageIndex < PageCount - 1) and (PageCount > 1) then
    ActivePageIndex := ActivePageIndex + 1
  else if PageCOunt > 0 then
    ActivePageIndex := 0
  else
    ActivePageIndex := -1;
end;

procedure TJvCustomPageList.PrevPage;
begin
  if (ActivePageIndex > 0) then
    ActivePageIndex := ActivePageIndex - 1
  else
    ActivePageIndex := PageCount - 1;
end;

procedure TJvCustomPageList.SetPropagateEnable(const Value: boolean);
begin
  if FPropagateEnable <> Value then
  begin
    FPropagateEnable := Value;
    UpdateEnabled;
  end;
end;

procedure TJvCustomPageList.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateEnabled;
end;

function TJvCustomPageList.FindNextPage(CurPage: TJvCustomPage;
  GoForward, IncludeDisabled: boolean): TJvCustomPage;
var i, StartIndex: integer;
begin
  if PageCount <> 0 then
  begin
    StartIndex := FPages.IndexOf(CurPage);
    if StartIndex < 0 then
      if GoForward then
        StartIndex := FPages.Count - 1
      else
        StartIndex := 0;
    i := StartIndex;
    repeat
      if GoForward then
      begin
        Inc(i);
        if i >= FPages.Count - 1 then
          i := 0;
      end
      else
      begin
        if i <= 0 then
          i := FPages.Count - 1;
        Dec(i);
      end;
      Result := Pages[i];
      if IncludeDisabled or Result.Enabled then
        Exit;
    until i = StartIndex;
  end;
  Result := nil;
end;

procedure TJvCustomPageList.SetShowDesignCaption(const Value: TJvShowDesignCaption);
begin
  if FShowDesignCaption <> Value then
  begin
    FShowDesignCaption := Value;
    if HandleAllocated and (csDesigning in ComponentState) then
      RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TJvCustomPageList.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(self);
end;

procedure TJvCustomPageList.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TJvCustomPageList.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TJvCustomPageList.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomPageList.UpdateEnabled;
  procedure InternalSetEnabled(AControl: TWinControl);
  var i: integer;
  begin
    for i := 0 to AControl.ControlCount - 1 do
    begin
      AControl.Controls[i].Enabled := self.Enabled;
      if AControl.Controls[i] is TWinControl then
        InternalSetEnabled(TWinControl(AControl.Controls[i]));
    end;
  end;
begin
  if PropagateEnable then
    InternalSetEnabled(self);
end;

function TJvCustomPageList.GetPage(Index: integer): TJvCustomPage;
begin
  if (Index >= 0) and (Index < FPages.Count) then
    Result := FPages[Index]
  else
    Result := nil;
end;

{ TJvSettingsTreeImages }

constructor TJvSettingsTreeImages.Create;
begin
  inherited Create;
  FCollapsedIndex := 0;
  FExpandedIndex := 1;
  FSelectedIndex := 2;
  FImageIndex := -1;
end;

{ TJvPageList }

function TJvPageList.InternalGetPageClass: TJvCustomPageClass;
begin
  Result := TJvStandardPage;
end;

{ TJvCustomSettingsTreeView }

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
  inherited;
  if not AutoExpand and Node.Expanded then
    Node.Expand(false); // refresh node and children
end;

procedure TJvCustomSettingsTreeView.Collapse(Node: TTreeNode);
begin
  inherited;
  if Node.HasChildren then
  begin
    Node.ImageIndex := FNodeImages.CollapsedIndex;
    Node.SelectedIndex := FNodeImages.CollapsedIndex;
  end;
end;

constructor TJvCustomSettingsTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FNodeImages := TJvSettingsTreeImages.Create;
  FNodeImages.TreeView := self;
  AutoExpand := true;
  ShowButtons := false;
  ShowLines := false;
  ReadOnly := true;
  // we need to assign to these since the TTreeView checks if they are assigned
  // and won't call GetImageIndex without them
  inherited OnGetImageIndex := DoGetImageIndex;
  inherited OnGetSelectedIndex := DoGetSelectedIndex;
end;

procedure TJvCustomSettingsTreeView.Delete(Node: TTreeNode);
begin
  inherited;
  if Node = FLastSelected then
    FLastSelected := nil;
end;

destructor TJvCustomSettingsTreeView.Destroy;
begin
  FNodeImages.TreeView := nil;
  FNodeImages.Free;
  inherited;
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
      ResetSiblingFolders(Node, FNodeImages.CollapsedIndex, FNodeImages.CollapsedIndex, true);
    Node.ImageIndex := FNodeImages.ExpandedIndex;
    Node.SelectedIndex := FNodeImages.ExpandedIndex;
    N := Node.getFirstChild;
    if (N <> nil) and not N.HasChildren then
    begin
      ResetPreviousNode(N);
      N.ImageIndex := FNodeImages.SelectedIndex;
      N.SelectedIndex := FNodeImages.SelectedIndex;
      R := N.DisplayRect(false);
      InvalidateRect(Handle, @R, true);
      SetPreviousNode(N);
    end;
  end;
  inherited;
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
  else if Node.Selected
    or ((Node.Parent <> nil) and Node.Parent.Selected and (Node.Parent.getFirstChild = Node)) then
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
  inherited;
  if Items.Count > 0 then
  begin
    ResetSiblingFolders(Items[0], FNodeImages.CollapsedIndex, FNodeImages.CollapsedIndex, true);
    Items[0].MakeVisible;
  end;
end;

procedure TJvCustomSettingsTreeView.ResetPreviousNode(NewNode: TTreeNode);
var R: TRect;
begin
  if (FLastSelected <> nil) and (FLastSelected <> NewNode) and (NewNode <> nil) and not NewNode.HasChildren then
  begin
    FLastSelected.ImageIndex := FNodeImages.ImageIndex;
    FLastSelected.SelectedIndex := FNodeImages.ImageIndex;
    R := FLastSelected.DisplayRect(false);
    InvalidateRect(Handle, @R, true);
  end;
end;

procedure TJvCustomSettingsTreeView.SetImageSelection(
  const Value: TJvSettingsTreeImages);
begin
  //  FNodeImages := Value;
end;

procedure TJvCustomSettingsTreeView.SetPreviousNode(NewNode: TTreeNode);
begin
  FLastSelected := NewNode;
end;

end.

