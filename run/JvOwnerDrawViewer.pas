
unit JvOwnerDrawViewer;
{$I JVCL.INC}
interface
uses
  Classes, Graphics, ComCtrls, JvCustomItemViewer;

type
  TJvOwnerDrawViewerOptions = class(TJvCustomItemViewerOptions)
  published
    property Alignment;
    property AutoCenter;
    property BrushPattern;
    property DragAutoScroll;
    property Height;
    property HorzSpacing;
    property HotTrack;
    property Layout;
    property LazyRead;
    property MultiSelect;
    property RightClickSelect;
    property ScrollBar;
    property ShowCaptions;
    property Smooth;
    property Tracking;
    property VertSpacing;
    property Width;
  end;

  TJvOwnerDrawViewer = class(TJvCustomItemViewer)
  private
    function GetOptions: TJvOwnerDrawViewerOptions;
    procedure SetOptions(const Value: TJvOwnerDrawViewerOptions);
  protected
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Count;
    property Items;
  published
    property Options: TJvOwnerDrawViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
    property OnDrawItem;

    property Align;
    property Anchors;
    //    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;




implementation

{ TJvOwnerDrawViewer }

constructor TJvOwnerDrawViewer.Create(AOwner: TComponent);
begin
  inherited;
  Color := clWindow;
end;

function TJvOwnerDrawViewer.GetOptions: TJvOwnerDrawViewerOptions;
begin
  Result := TJvOwnerDrawViewerOptions(inherited Options);
end;

function TJvOwnerDrawViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvOwnerDrawViewerOptions;
end;

procedure TJvOwnerDrawViewer.SetOptions(const Value: TJvOwnerDrawViewerOptions);
begin
  inherited Options := Value;
end;

end.

