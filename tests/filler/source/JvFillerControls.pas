unit JvFillerControls;
// peter3
interface
uses
  Windows, Classes, StdCtrls, Controls, Graphics, JvFillIntf5;

type
  TJvFillListBox = class(TCustomListBox, IFillerNotify)
  private
    FFiller: IFiller;
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure setFiller(const Value: iFiller);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer);override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer);override;
  published
    property Items: IFiller read FFiller write setFiller;
    property AutoComplete;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TFillLabel = class(TCustomLabel, IFillerNotify)
  private
    FFiller: IFiller;
    FIndex: Integer;
    procedure SetFiller(const Value: IFiller);
    procedure SetIndex(const Value: Integer);
  protected
    procedure FillerChanging(const AFiller: IFiller; AReason:
      TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason:
      TJvFillerChangeReason);
    procedure UpdateCaption;
    function GetLabelText: string; override;
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filler: IFiller read FFiller write SetFiller;
    property Index: Integer read FIndex write SetIndex;

    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses Math;
const
  cStyle = lbVirtualOwnerDraw;

  { TJvFillListBox }

constructor TJvFillListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvFillListBox.Destroy;
begin
  //  Style := lbStandard;
  Items := nil;
  inherited;
end;

procedure TJvFillListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Items <> nil then
  begin
    Canvas.Font := Font;
    if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color  := clHighlightText;
    end
    else
      Canvas.Brush.Color := Color;
    Items.DrawItem(Canvas, Rect, Index, State);
  end
  else
    inherited;
end;

procedure TJvFillListBox.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if Style = cStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cStyle then
      Count := Items.Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if Style = cStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cStyle then
      Count := Items.Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.MeasureItem(Index: Integer; var Height: Integer);
var aSize: TSize;
begin
  aSize.cy := 0;
  if Items <> nil then
    aSize := Items.MeasureItem(Canvas, Index);
  if aSize.cy <> 0 then
    Height := aSize.cy;
end;

procedure TJvFillListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TJvFillListBox.setFiller(const Value: iFiller);
var aSize:TSize;
begin
  if FFiller <> Value then
  begin
    if FFiller <> nil then
    begin
      FFiller.UnRegisterChangeNotify(self);
      if (Style = cStyle) and HasParent then
        Count := 0;
    end;
    FFiller := Value;
    if FFiller <> nil then
    begin
      FFiller.RegisterChangeNotify(self);
      if HasParent then
      begin
        Style := cStyle;
        Count := FFiller.Count;
      end
      else
        Style := lbStandard;
      aSize := FFiller.MeasureItem(Canvas,-1);
      if aSize.cx <> 0 then
        ClientWidth := aSize.cx;
      if aSize.cy <> 0 then
        ItemHeight := aSize.cy;
    end;
  end;
end;

{ TFillLabel }

constructor TFillLabel.Create(AOwner: TComponent);
begin
  inherited;
  FIndex := -1;
end;

destructor TFillLabel.Destroy;
begin
  Filler := nil;
  inherited;
end;

procedure TFillLabel.DoDrawText(var Rect: TRect; Flags: Integer);
begin
  inherited;
  if Assigned(Filler) and (fsCanRender in Filler.getSupports) and
    (FIndex >= 0) and (FIndex < Filler.Count) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    Filler.DrawItem(Canvas, Rect, FIndex,[]);
  end;
end;

procedure TFillLabel.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  UpdateCaption;
end;

procedure TFillLabel.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      Filler := nil;
  end;
end;

function TFillLabel.GetLabelText: string;
begin
  if Assigned(Filler) and (fsText in FFiller.getSupports) and
    (FIndex >= 0) and (FIndex < Filler.Count) then
    Result := (FFiller.Items[FIndex] as IFillerItemText).Caption
  else
    Result := inherited GetLabelText;
end;

procedure TFillLabel.SetFiller(const Value: IFiller);
begin
  if FFiller = Value then
    Exit;

  if Assigned(FFiller) then
    FFiller.UnRegisterChangeNotify(Self);

  FFiller := Value;

  if Assigned(FFiller) then
    FFiller.RegisterChangeNotify(Self);
  UpdateCaption;
end;

procedure TFillLabel.SetIndex(const Value: Integer);
begin
  if Value <> FIndex then
  begin
    FIndex := Value;
    UpdateCaption;
  end;
end;

procedure TFillLabel.UpdateCaption;
var tmp:TSize;
begin
  if (FFiller <> nil) and not AutoSize then
  begin
    tmp := FFiller.MeasureItem(Canvas,Index);
    if (tmp.cy <> 0)  then
      Height := tmp.cy;
    if tmp.cx <> 0 then
      Width := tmp.cx;
  end;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

end.

