// Temporary file containing test controls for JvDataProvider.

{$I JVCL.INC}

unit JvDataProviderControls;

interface

uses
  Windows, Classes, StdCtrls, Controls, Graphics,
  JvDataProvider, JvDataProviderImpl, JvListBox, JvLabel;

type
  TJvProvidedListBox = class(TJvCustomListBox, IJvDataProviderNotify)
  private
    FProvider: IJvDataProvider;
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure SetProvider(const Value: IJvDataProvider);
    {$IFNDEF COMPILER6_UP}
    function GetProviderComp: TComponent;
    procedure SetProviderComp(Value: TComponent);
    {$ENDIF COMPILER6_UP}
  protected
    { Used to emulate owner draw, creates a list of empty items (low overhead for the items) and
      thus maintain the ability to use lbOwnerDrawVariable as drawing style. }
    function GetCount: Integer; {$IFNDEF COMPILER6_UP}virtual;{$ELSE}override;{$ENDIF COMPILER6_UP}
    procedure SetCount(Value: Integer); virtual;
    { Direct link to actual provider interface. This is done to aid in the implementation (less
      IFDEF's in the code; always refer to ProviderIntf and it's working in all Delphi versions). }
    function ProviderIntf: IJvDataprovider;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer);override;
    property Count: Integer read GetCount write SetCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);override;
  published
    {$IFDEF COMPILER6_UP}
    property Provider: IJvDataProvider read FProvider write setProvider;
    {$ELSE}
    property Provider: TComponent read GetProviderComp write SetProviderComp;
    {$ENDIF COMPILER6_UP}
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

  TJvProvidedLabel = class(TJvLabel, IJvDataProviderNotify)
  private
    FProvider: IJvDataprovider;
    FItemID: string;
    procedure SetProvider(const Value: IJvDataProvider);
    {$IFNDEF COMPILER6_UP}
    function GetProviderComp: TComponent;
    procedure SetProviderComp(Value: TComponent);
    {$ENDIF COMPILER6_UP}
    function GetItem: IJvDataItem;
    procedure SetItem(Value: IJvDataItem);
    function GetItemID: TJvDataProviderItemID;
    procedure SetItemID(Value: TJvDataProviderItemID);
  protected
    { Direct link to actual provider interface. This is done to aid in the implementation (less
      IFDEF's in the code; always refer to ProviderIntf and it's working in all Delphi versions). }
    function ProviderIntf: IJvDataProvider;
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure UpdateCaption;
    function GetLabelCaption: string; override;
    procedure DoDrawText(var Rect: TRect; Flags: Word); override;

    property ItemIntf: IJvDataItem read GetItem write SetItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$IFDEF COMPILER6_UP}
    property Provider: IJvDataProvider read FProvider write SetProvider;
    property Item: IJvDataItem read GetItem write SetItem;
    {$ELSE}
    property Provider: TComponent read GetProviderComp write SetProviderComp;
    property Item: TJvDataProviderItemID read GetItemID write SetItemID;
    {$ENDIF COMPILER6_UP}
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
  JvTypes;

{ TJvFillListBox }

constructor TJvProvidedListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  OwnerData := False;
end;

destructor TJvProvidedListBox.Destroy;
begin
  Provider := nil;
  inherited Destroy;
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
  Result := FProvider;
end;

procedure TJvProvidedListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ItemsRenderer: IJvDataItemsRenderer;
  Item: IJvDataItem;
  ItemRenderer: IJvDataItemRenderer;
  ItemText: IJvDataItemText;
begin
  if (ProviderIntf <> nil) and Supports(ProviderIntf, IJvDataItemsRenderer, ItemsRenderer) then
  begin
    Canvas.Font := Font;
    if odSelected in State then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color  := clHighlightText;
    end
    else
      Canvas.Brush.Color := Color;
    ItemsRenderer.DrawItemByIndex(Canvas, Rect, Index, State);
  end
  else if ProviderIntf <> nil then
  begin
    Item := (ProviderIntf as IJvDataItems).GetItem(Index);
    if Supports(Item, IJvDataItemRenderer, ItemRenderer) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      ItemRenderer.Draw(Canvas, Rect, State);
    end
    else if Supports(Item, IJvDataItemText, ItemText) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, ItemText.Caption);
    end
    else
      inherited DrawItem(Index, Rect, State);
  end
  else
    inherited DrawItem(Index, Rect, State);
end;

procedure TJvProvidedListBox.DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  case AReason of
    pcrDestroy: // Should never occur in this method.
      begin
        if HasParent then
          Count := 0;
        FProvider := nil;
      end;
    else
      begin
        if HasParent then
          Count := (ProviderIntf as IJvDataItems).Count;
      end;
  end;
  Invalidate;
end;

procedure TJvProvidedListBox.DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  case AReason of
    pcrDestroy:
      begin
        if HasParent then
          Count := 0;
        FProvider := nil;
        Invalidate;
      end;
  end;
end;

procedure TJvProvidedListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  ItemsRenderer: IJvDataItemsRenderer;
begin
  if (ProviderIntf <> nil) and Supports(ProviderIntf, IJvDataItemsRenderer, ItemsRenderer) then
  begin
    aSize.cy := ItemHeight;
    aSize := ItemsRenderer.MeasureItemByIndex(Canvas, Index);
    if aSize.cy <> 0 then
      Height := aSize.cy;
  end
  else
    inherited MeasureItem(Index, Height);
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

procedure TJvProvidedListBox.SetProvider(const Value: IJvDataProvider);
begin
  if FProvider <> Value then
  begin
    if FProvider <> nil then
    begin
      FProvider.UnregisterChangeNotify(Self);
      if HasParent then
        Count := 0;
    end;
    FProvider := Value;
    if FProvider <> nil then
    begin
      FProvider.RegisterChangeNotify(Self);
      if HasParent then
        Count := (ProviderIntf as IJvDataItems).Count;
    end;
    Invalidate;
  end;
end;

{$IFNDEF COMPILER6_UP}
function TJvProvidedListBox.GetProviderComp: TComponent;
var
  CompRef: IInterfaceComponentReference;
begin
  if FProvider = nil then
    Result := nil
  else
  begin
    if Succeeded(FProvider.QueryInterface(IInterfaceComponentReference, CompRef)) then
      Result := CompRef.GetComponent as TComponent
    else
      Result := nil;
  end;
end;

procedure TJvProvidedListBox.SetProviderComp(Value: TComponent);
var
  CompRef: IInterfaceComponentReference;
  ProviderRef: IJvDataProvider;
begin
  if Value = nil then
    SetProvider(nil)
  else
  begin
    if Value.GetInterface(IInterfaceComponentReference, CompRef) then
    begin
      if Value.GetInterface(IJvDataProvider, ProviderRef) then
        SetProvider(ProviderRef)
      else
        raise EJVCLException.Create('Component does not support the IJvDataProvider interface.');
    end
    else
      raise EJVCLException.Create('Component does not support the IInterfaceComponentReference interface.');
  end;
end;
{$ENDIF COMPILER6_UP}

{ TJvProvidedLabel }

constructor TJvProvidedLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItemID := '';
end;

destructor TJvProvidedLabel.Destroy;
begin
  Provider := nil;
  inherited Destroy;
end;

{$IFNDEF COMPILER6_UP}
function TJvProvidedLabel.GetProviderComp: TComponent;
var
  CompRef: IInterfaceComponentReference;
begin
  if FProvider = nil then
    Result := nil
  else
  begin
    if Succeeded(FProvider.QueryInterface(IInterfaceComponentReference, CompRef)) then
      Result := CompRef.GetComponent as TComponent
    else
      Result := nil;
  end;
end;

procedure TJvProvidedLabel.SetProviderComp(Value: TComponent);
var
  CompRef: IInterfaceComponentReference;
  ProviderRef: IJvDataProvider;
begin
  if Value = nil then
    SetProvider(nil)
  else
  begin
    if Value.GetInterface(IInterfaceComponentReference, CompRef) then
    begin
      if Value.GetInterface(IJvDataProvider, ProviderRef) then
        SetProvider(ProviderRef)
      else
        raise EJVCLException.Create('Component does not support the IJvDataProvider interface.');
    end
    else
      raise EJVCLException.Create('Component does not support the IInterfaceComponentReference interface.');
  end;
end;
{$ENDIF COMPILER6_UP}

procedure TJvProvidedLabel.DoDrawText(var Rect: TRect; Flags: Word);
var
  Tmp: TSize;
  TmpItem: IJvDataItem;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
begin
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
        ItemsRenderer.DrawItem(Canvas, Rect, TmpItem, [])
      else
        ItemRenderer.Draw(Canvas, Rect, []);
    end;
  end
  else
    inherited DoDrawText(Rect, Flags);
end;

function TJvProvidedLabel.ProviderIntf: IJvDataProvider;
begin
  Result := FProvider;
end;

procedure TJvProvidedLabel.DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  UpdateCaption;
end;

procedure TJvProvidedLabel.DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  case AReason of
    pcrDestroy:
      Provider := nil;
  end;
end;

function TJvProvidedLabel.GetLabelCaption: string;
var
  ItemText: IJvDataItemText;
begin
  if (GetItem <> nil) and Supports(GetItem, IJvDataItemText, ItemText) then
    Result := ItemText.Caption
  else
    Result := inherited GetLabelCaption;
end;

procedure TJvProvidedLabel.SetProvider(const Value: IJvDataProvider);
begin
  if FProvider = Value then
    Exit;

  if Assigned(FProvider) then
  begin
    FProvider.UnregisterChangeNotify(Self);
    FItemID := '';
  end;

  FProvider := Value;

  if Assigned(FProvider) then
    FProvider.RegisterChangeNotify(Self);
  UpdateCaption;
end;

function TJvProvidedLabel.GetItem: IJvDataItem;
begin
  if (FItemID = '') or (ProviderIntf = nil) then
    Result := nil
  else
    Result := (ProviderIntf as IJvDataIDSearch).FindByID(FItemID, True);
end;

procedure TJvProvidedLabel.SetItem(Value: IJvDataItem);
begin
  if Value = nil then
    SetItemID('')
  else
    SetItemID(Value.GetID);
end;

function TJvProvidedLabel.GetItemID: TJvDataProviderItemID;
begin
  Result := FItemID;
end;

procedure TJvProvidedLabel.SetItemID(Value: TJvDataProviderItemID);
begin
  if Value <> FItemID then
  begin
    FItemID := Value;
    UpdateCaption;
  end;
end;

procedure TJvProvidedLabel.UpdateCaption;
var
  tmp: TSize;
  ItemsRenderer: IJvDataItemsRenderer;
  ItemRenderer: IJvDataItemRenderer;
begin
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
end;

end.

