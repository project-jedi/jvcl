// Temporary file containing test controls for JvDataProvider.

{$I JVCL.INC}

unit JvDataProviderControls;

interface

uses
  Windows, Classes, StdCtrls, Controls, Graphics,
  JvDataProvider, JvDataProviderImpl, JvListBox, JvLabel;

type
  TJvProvidedListBox = class(TJvCustomListBox)
  private
    FItemList: TStrings;
    FConsumerService: TJvDataConsumer;
    procedure SetConsumerService(Value: TJvDataConsumer);
  protected
    { Used to emulate owner draw, creates a list of empty items (low overhead for the items) and
      thus maintain the ability to use lbOwnerDrawVariable as drawing style. }
    function GetCount: Integer; {$IFNDEF COMPILER6_UP}virtual;{$ELSE}override;{$ENDIF COMPILER6_UP}
    procedure SetCount(Value: Integer); virtual;
    { Direct link to actual provider interface. Shortens the implementation of the various methods }
    function ProviderIntf: IJvDataprovider;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure ConsumerServiceChanged(Sender: TObject);
    procedure CalcViewList;
    property Count: Integer read GetCount write SetCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
  published
    property Provider: TJvDataConsumer read FConsumerService write SetConsumerService;
    {$IFDEF COMPILER6_UP}
    property AutoComplete;
    {$ENDIF COMPILER6_UP}
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
    {$IFDEF COMPILER6_UP}
    property ScrollWidth;
    {$ENDIF COMPILER6_UP}
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
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

  TJvProvidedLabel = class(TJvLabel)
  private
    FConsumerService: TJvDataConsumer;
    procedure SetConsumerService(Value: TJvDataConsumer);
  protected
    { Direct link to actual provider interface. Shortens the implementation of the various methods }
    function ProviderIntf: IJvDataProvider;
    procedure ConsumerServiceChanged(Sender: TObject);
    function GetItem: IJvDataItem;
    procedure UpdateCaption;
    function GetLabelCaption: string; override;
    procedure DoDrawText(var Rect: TRect; Flags: Word); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Provider: TJvDataConsumer read FConsumerService write SetConsumerService;
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
    {$IFDEF COMPILER6_UP}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF COMPILER6_UP}
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Math, SysUtils,
  JclStrings,
  JvConsts, JvTypes;

{ TJvFillListBox }

constructor TJvProvidedListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemList := TStringList.Create;
  FConsumerService := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed,
    DPA_ConsumerDisplaysList]);
  FConsumerService.OnChanged := ConsumerServiceChanged;
end;

destructor TJvProvidedListBox.Destroy;
begin
  FreeAndNil(FConsumerService);
  FreeAndNil(FItemList);
  inherited Destroy;
end;

procedure TJvProvidedListBox.SetConsumerService(Value: TJvDataConsumer);
begin
end;

function TJvProvidedListBox.GetCount: Integer;
begin
  Result := Items.Count;
end;

procedure TJvProvidedListBox.SetCount(Value: Integer);
begin
  Items.Text := StrRepeat(#13#10, Value);
end;

function TJvProvidedListBox.ProviderIntf: IJvDataProvider;
begin
  Result := FConsumerService.ProviderIntf;
end;

procedure TJvProvidedListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  Item: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  ItemText: IJvDataItemText;
  DrawState: TProviderDrawStates;
begin
  DrawState := DP_OwnerDrawStateToProviderDrawState(State);
  if not Enabled then
    DrawState := DrawState + [pdsDisabled, pdsGrayed];
  if (ProviderIntf <> nil) then
  begin
    Provider.Enter;
    try
      Item := (ProviderIntf as IJvDataIDSearch).Find(FItemList[Index], True);
      if Item <> nil then
      begin
        Inc(Rect.Left, Integer(FItemList.Objects[Index]) * 16);
        Canvas.Font := Font;
        if odSelected in State then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color  := clHighlightText;
        end
        else
          Canvas.Brush.Color := Color;
        Canvas.FillRect(Rect);
        if Supports(Item, IJvDataItemRenderer, ItemRenderer) then
          ItemRenderer.Draw(Canvas, Rect, DrawState)
        else if DP_FindItemsRenderer(Item, ItemsRenderer) then
          ItemsRenderer.DrawItem(Canvas, Rect, Item, DrawState)
        else if Supports(Item, IJvDataItemText, ItemText) then
          Canvas.TextRect(Rect, Rect.Left, Rect.Top, ItemText.Caption)
        else
          Canvas.TextRect(Rect, Rect.Left, Rect.Top, SDataItemRenderHasNoText);
      end
      else
        inherited DrawItem(Index, Rect, State);
    finally
      Provider.Leave;
    end;
  end
  else
    inherited DrawItem(Index, Rect, State);
end;

procedure TJvProvidedListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  AItem: IJvDataItem;
  ItemRenderer: IJvDataItemRenderer;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemText: IJvDataItemText;
begin
  if (ProviderIntf <> nil) then
  begin
    Provider.Enter;
    try
      if Height <> 0 then
        aSize.cy := Height
      else
        aSize.cy := ItemHeight;
      AItem := (ProviderIntf as IJvDataIDSearch).Find(FItemList[Index], True);
      if (AItem <> nil) then
      begin
        if DP_FindItemsRenderer(AItem, ItemsRenderer) then
          aSize := ItemsRenderer.MeasureItem(Canvas, AItem)
        else if Supports(AItem, IJvDataItemRenderer, ItemRenderer) then
          aSize := ItemRenderer.Measure(Canvas)
        else if Supports(AItem, IJvdataItemText, ItemText) then
          aSize.cy := Canvas.TextHeight(ItemText.Caption)
        else
          aSize.cy := Canvas.TextHeight(SDataItemRenderHasNoText);
        if aSize.cy <> 0 then
          Height := aSize.cy;
      end
      else
        inherited MeasureItem(Index, Height);
    finally
      Provider.Leave;
    end;
  end
  else
    inherited MeasureItem(Index, Height);
end;

procedure TJvProvidedListBox.ConsumerServiceChanged(Sender: TObject);
begin
  CalcViewList;
end;

procedure TJvProvidedListBox.CalcViewList;
begin
  { Iterate item tree and add item IDs to a string list. Objects value for each item holds the
    indent level }
  Provider.Enter;
  try
    DP_GenItemsList(ProviderIntf as IJvDataItems, FItemList);
  finally
    Provider.Leave;
    Count := FItemList.Count;
  end;
end;

procedure TJvProvidedListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Style = lbStandard then
    Params.Style := Params.Style or LBS_OWNERDRAWFIXED;
end;

procedure TJvProvidedListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  Invalidate;
end;

{ TJvProvidedLabel }

constructor TJvProvidedLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerService := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed,
    DPA_RendersSingleItem]);
  FConsumerService.OnChanged := ConsumerServiceChanged;
end;

destructor TJvProvidedLabel.Destroy;
begin
  FreeAndNil(FConsumerService);
  inherited Destroy;
end;

procedure TJvProvidedLabel.SetConsumerService(Value: TJvDataConsumer);
begin
end;

function TJvProvidedLabel.ProviderIntf: IJvDataProvider;
begin
  Result := FConsumerService.ProviderIntf;
end;

procedure TJvProvidedLabel.ConsumerServiceChanged(Sender: TObject);
begin
  UpdateCaption;
end;

function TJvProvidedLabel.GetItem: IJvDataItem;
begin
  if ProviderIntf <> nil then
  begin
    Provider.Enter;
    try
      Result := (Provider as IJvDataConsumerItemSelect).GetItem;
    finally
      Provider.Leave;
    end;
  end
  else
    Result := nil;
end;

procedure TJvProvidedLabel.UpdateCaption;
var
  tmp: TSize;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
begin
  if ProviderIntf <> nil then
  begin
    Provider.Enter;
    try
      if AutoSize and (GetItem <> nil) and (
        Supports(GetItem.Items, IJvDataItemsRenderer, ItemsRenderer) or
        Supports(GetItem, IJvDataItemRenderer, ItemRenderer)) then
      begin
        if ItemsRenderer <> nil then
          tmp := ItemsRenderer.MeasureItem(Canvas, GetItem)
        else
          tmp := ItemRenderer.Measure(Canvas);
        if (tmp.cy <> 0)  then
          ClientHeight := tmp.cy;
        if tmp.cx <> 0 then
          ClientWidth := tmp.cx + LeftMargin + RightMargin + 4;
        Invalidate;
      end
      else
        Perform(CM_TEXTCHANGED, 0, 0);
    finally
      Provider.Leave;
    end;
  end
end;

function TJvProvidedLabel.GetLabelCaption: string;
var
  ItemText: IJvDataItemText;
begin
  if ProviderIntf <> nil then
  begin
    Provider.Enter;
    try
      if (GetItem <> nil) and Supports(GetItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := inherited GetLabelCaption;
    finally
      Provider.Leave;
    end;
  end;
end;

procedure TJvProvidedLabel.DoDrawText(var Rect: TRect; Flags: Word);
var
  Tmp: TSize;
  TmpItem: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
  DrawState: TProviderDrawStates;
begin
  if ProviderIntf <> nil then
  begin
    Provider.Enter;
    try
      if not Enabled then
        DrawState := [pdsDisabled]
      else
        DrawState := [];
      TmpItem := GetItem;
      if (TmpItem <> nil) and (Supports(TmpItem.Items, IJvDataItemsRenderer, ItemsRenderer) or
        Supports(TmpItem, IJvDataItemRenderer, ItemRenderer)) then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font := Font;
        if (Flags and DT_CALCRECT <> 0) then
        begin
          if ItemsRenderer <> nil then
            Tmp := ItemsRenderer.MeasureItem(Canvas, TmpItem)
          else
            Tmp := ItemRenderer.Measure(Canvas);
          Rect.Right := Tmp.cx;
          Rect.Bottom := Tmp.cy;
        end
        else
        begin
          if ItemsRenderer <> nil then
            ItemsRenderer.DrawItem(Canvas, Rect, TmpItem, DrawState)
          else
            ItemRenderer.Draw(Canvas, Rect, DrawState);
        end;
      end
      else
        inherited DoDrawText(Rect, Flags);
    finally
      Provider.Leave;
    end;
  end
  else
    inherited DoDrawText(Rect, Flags);
end;

end.

