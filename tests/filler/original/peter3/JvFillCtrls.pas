unit JvFillCtrls;
// peter3
interface
uses
  Windows, Classes, StdCtrls, Controls, Graphics, JvFillIntf_mbe2;

type
  TJvFillListBox = class(TCustomListBox, IFillerNotify)
  private
    FFiller: IFiller;
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure setFiller(const Value: IFiller);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
  public
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
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

  TJvFillComboBox = class(TCustomComboBox, IFillerNotify)
  private
    FFiller: IFiller;
    FCount: integer;
    procedure setFiller(const Value: IFiller);
    procedure SetCount(const Value: integer);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    property Count: integer read FCount write SetCount;
  public
    destructor Destroy; override;
    procedure FillerChanged(const AFiller: IFiller;
      AReason: TJvFillerChangeReason);
    procedure FillerChanging(const AFiller: IFiller;
      AReason: TJvFillerChangeReason);
  published
    property Items: IFiller read FFiller write setFiller;
    property ItemIndex;
  end;

  TJvFillLabel = class(TLabel, IFillerNotify)
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
  end;

implementation

uses SysUtils, Math;
const
  cListBoxStyle = lbVirtualOwnerDraw;
  cComboBoxStyle = csOwnerDrawVariable;

  { TJvFillListBox }

destructor TJvFillListBox.Destroy;
begin
  //  Style := lbStandard;
  Items := nil;
  inherited;
end;

procedure TJvFillListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  FillerItems: IFillerItems;
begin
  if Items <> nil then
  begin
    if Supports(Items, IFillerItems, FillerItems) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      if fsCanRender in Items.getSupports then
        FillerItems.DrawItem(Canvas, Rect, Index, State)
      else
        Canvas.TextRect(Rect, Rect.left, Rect.Top, (FillerItems.Items[Index] as IFillerItemText).Caption);
    end
    else
      inherited;
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
        if Style = cListBoxStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cListBoxStyle then
      Count := (Items as IFillerItems).Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if Style = cListBoxStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cListBoxStyle then
      Count := (Items as IFillerItems).Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  FillerItems: IFillerItems;
begin
  if fsCanMeasure in Items.getSupports then
  begin
    if Supports(Items, IFillerItems, FillerItems) then
    begin
      aSize.cy := 0;
      if Items <> nil then
        aSize := FillerItems.MeasureItem(Canvas, Index);
      if aSize.cy <> 0 then
        Height := aSize.cy;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TJvFillListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TJvFillListBox.setFiller(const Value: iFiller);
var aSize: TSize;
begin
  if FFiller <> Value then
  begin
    if FFiller <> nil then
    begin
      FFiller.UnRegisterChangeNotify(self);
      if (Style = cListBoxStyle) and HasParent then
        Count := 0;
    end;
    FFiller := Value;
    if FFiller <> nil then
    begin
      FFiller.RegisterChangeNotify(self);
      if HasParent then
      begin
        Style := cListBoxStyle;
        Count := (FFiller as IFillerItems).Count;
      end
      else
        Style := lbStandard;
      if fsCanMeasure in FFiller.getSupports then
      begin
        aSize := (FFiller as IFillerItems).MeasureItem(Canvas, -1);
        if aSize.cx <> 0 then
          ClientWidth := aSize.cx;
        if aSize.cy <> 0 then
          ItemHeight := aSize.cy;
      end;
    end;
  end;
end;

{ TJvFillLabel }

constructor TJvFillLabel.Create(AOwner: TComponent);
begin
  inherited;
  FIndex := -1;
end;

destructor TJvFillLabel.Destroy;
begin
  Filler := nil;
  inherited;
end;

procedure TJvFillLabel.DoDrawText(var Rect: TRect; Flags: Integer);
var F: IFillerItems;
begin
  if (Flags and DT_CALCRECT = 0) and Assigned(Filler) and (fsCanRender in Filler.getSupports) and
    Supports(Filler, IFillerItems, F) and
    (FIndex >= 0) and (FIndex < F.Count) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    F.DrawItem(Canvas, Rect, FIndex, []);
  end
  else
    inherited DoDrawText(Rect, Flags);
end;

procedure TJvFillLabel.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  UpdateCaption;
end;

procedure TJvFillLabel.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      Filler := nil;
  end;
end;

function TJvFillLabel.GetLabelText: string;
var F: IFillerItems;
begin
  if Assigned(Filler) and (fsText in FFiller.getSupports) and
    Supports(Filler, IFillerItems, F) and
    (FIndex >= 0) and (FIndex < F.Count) then
    Result := (F.Items[FIndex] as IFillerItemText).Caption
  else
    Result := inherited GetLabelText;
end;

procedure TJvFillLabel.SetFiller(const Value: IFiller);
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

procedure TJvFillLabel.SetIndex(const Value: Integer);
begin
  if Value <> FIndex then
  begin
    FIndex := Value;
    UpdateCaption;
  end;
end;

procedure TJvFillLabel.UpdateCaption;
begin
  Perform(CM_TEXTCHANGED, 0, 0);
end;

{ TJvFillComboBox }

destructor TJvFillComboBox.Destroy;
begin
  Items := nil;
  inherited;
end;

procedure TJvFillComboBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  FillerItems: IFillerItems;
begin
  if Items <> nil then
  begin
    if Supports(Items, IFillerItems, FillerItems) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      if fsCanRender in Items.getSupports then
        FillerItems.DrawItem(Canvas, Rect, Index, State)
      else
        Canvas.TextRect(Rect, Rect.left, Rect.Top, (FillerItems.Items[Index] as IFillerItemText).Caption);
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TJvFillComboBox.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  if AReason = frDestroy then
    Items := nil;
  Invalidate;
end;

procedure TJvFillComboBox.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  if AReason = frDestroy then
    Items := nil;
  Invalidate;
end;

procedure TJvFillComboBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  FillerItems: IFillerItems;
begin
  if fsCanMeasure in Items.getSupports then
  begin
    if Supports(Items, IFillerItems, FillerItems) then
    begin
      aSize.cy := 0;
      if Items <> nil then
        aSize := FillerItems.MeasureItem(Canvas, Index);
      if aSize.cy <> 0 then
        Height := aSize.cy + 2;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TJvFillComboBox.SetCount(const Value: integer);
var I: TStrings;
begin
  // combo boxes can't be virtual, so I hack it like this:
  if (FCount <> Value) then
  begin
    FCount := Value;
    I := inherited Items;
    if FCount = 0 then
    begin
      I.Clear;
      Exit;
    end;
    I.BeginUpdate;
    try
      while I.Count > Value do
        I.Delete(0);
      while I.Count < Value do
        I.Add('');
    finally
      I.EndUpdate;
    end;
  end;
end;

procedure TJvFillComboBox.setFiller(const Value: IFiller);
var aSize: TSize;
begin
  if FFiller <> Value then
  begin
    if FFiller <> nil then
    begin
      FFiller.UnRegisterChangeNotify(self);
      if (Style = cComboBoxStyle) and HasParent then
        Count := 0;
    end;
    FFiller := Value;
    if FFiller <> nil then
    begin
      FFiller.RegisterChangeNotify(self);
      if HasParent then
      begin
        Style := cComboBoxStyle;
        Count := (FFiller as IFillerItems).Count;
      end
      else
        Style := csDropDown;
      if fsCanMeasure in FFiller.getSupports then
      begin
        aSize := (FFiller as IFillerItems).MeasureItem(Canvas, -1);
        if aSize.cx <> 0 then
          ClientWidth := aSize.cx;
        if aSize.cy <> 0 then
          ItemHeight := aSize.cy + 2;
      end;
    end
    else
      Style := csDropDown;
  end;
end;

end.

