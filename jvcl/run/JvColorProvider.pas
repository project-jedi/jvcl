{.$DEFINE TestContexts}

unit JvColorProvider;

interface

uses
  Classes, Windows,
  JvDataProvider, JvDataProviderImpl;

type
  TJvColorProvider = class(TJvCustomDataProvider)
  protected
    {$IFDEF TestContexts}
    class function ContextsClass: TJvDataContextsClass; override;
    class function ContextsManagerClass: TJvDataContextsManagerClass; override;
    {$ENDIF TestContexts}
    class function ItemsClass: TJvDataItemsClass; override;
    function ConsumerClasses: TClassArray; override;
  public
    procedure AfterConstruction; override;
  end;

  TJvColorProviderColorBoxSettings = class(TPersistent)
  private
    FActive: Boolean;
    FHeight: Integer;
    FMargin: Integer;
    FShadowed: Boolean;
    FShadowSize: Integer;
    FSpacing: Integer;
    FWidth: Integer;
    FConsumerServiceExt: TJvDataConsumerAggregatedObject;
  protected
    procedure Changed; virtual;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetHeight(Value: Integer); virtual;
    procedure SetMargin(Value: Integer); virtual;
    procedure SetShadowed(Value: Boolean); virtual;
    procedure SetShadowSize(Value: Integer); virtual;
    procedure SetSpacing(Value: Integer); virtual;
    procedure SetWidth(Value: Integer); virtual;
    property ConsumerServiceExt: TJvDataConsumerAggregatedObject read FConsumerServiceExt;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Active: Boolean read FActive write SetActive default True;
    property Height: Integer read FHeight write SetHeight default 13;
    property Margin: Integer read FMargin write SetMargin default 2;
    property Shadowed: Boolean read FShadowed write SetShadowed default True;
    property ShadowSize: Integer read FShadowSize write SetShadowSize default 2;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width: Integer read FWidth write SetWidth default 21;
  end;

  TJvColorProviderTextSettings = class(TPersistent)
  private
    FActive: Boolean;
    FShowHex: Boolean;
    FShowName: Boolean;
    FShowRGB: Boolean;
    FConsumerServiceExt: TJvDataConsumerAggregatedObject;
  protected
    procedure Changed; virtual;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetShowHex(Value: Boolean); virtual;
    procedure SetShowName(Value: Boolean); virtual;
    procedure SetShowRGB(Value: Boolean); virtual; 
    property ConsumerServiceExt: TJvDataConsumerAggregatedObject read FConsumerServiceExt;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Active: Boolean read FActive write SetActive default True;
    property ShowHex: Boolean read FShowHex write SetShowHex;
    property ShowName: Boolean read FShowName write SetShowName default True;
    property ShowRGB: Boolean read FShowRGB write SetShowRGB;
  end;

  IJvColorProviderSettings = interface
    ['{5381D2E0-D8EA-46E7-A3C6-42B5353B896B}']
    function Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;

    property ColorBoxSettings: TJvColorProviderColorBoxSettings read Get_ColorBoxSettings;
    property TextSettings: TJvColorProviderTextSettings read Get_TextSettings;
  end;

procedure Register;

implementation

uses
  Graphics, SysUtils,
  JclRTTI,
  JvConsts;

function GetItemColorValue(Item: IJvDataItem; out Color: TColor): Boolean;
var
  S: string;
begin
  S := Item.GetID;
  Result := Copy(S, 1, 7) = 'TCOLOR=';
  if Result then
    Color := StrToInt('$0' + Copy(S, 8, 8));
end;

type
  TJvColorItems = class(TJvBaseDataItems)
  private
    FColors: TStrings;
  protected
    procedure AddStdColor(const S: string); virtual;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
    procedure InitImplementers; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJvColorItemText = class(TJvBaseDataItemTextImpl)
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  TOpenBaseDataItem = class(TJvBaseDataItem);

  TJvColorItemsRenderer = class(TJvCustomDataItemsRenderer)
  protected
    CurrentCanvas: TCanvas;
    CurrentRect: TRect;
    CurrentItem: IJvDataItem;
    CurrentState: TProviderDrawStates;
    CurrentSettings: IJvColorProviderSettings;
    CurrentItemIsColorItem: Boolean;
    CurrentColorValue: TColor;
    procedure RenderColorBox;
    procedure RenderColorText;
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TProviderDrawStates); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; override;
    function GetConsumerSettings: IJvColorProviderSettings;
  end;

  {$IFDEF TestContexts}
  TJvColorDataContextsManager = class(TJvBaseDataContextsManager)
    function New: IJvDataContext; override;
  end;
  {$ENDIF TestContexts}

  TOpenConsumerServiceExt = class(TJvDataConsumerAggregatedObject);

  TJvColorProviderSettings = class(TJvDataConsumerAggregatedObject, IJvColorProviderSettings)
  private
    FColorBoxSettings: TJvColorProviderColorBoxSettings;
    FTextSettings: TJvColorProviderTextSettings;
  protected
    function Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;
    procedure Set_ColorBoxSettings(Value: TJvColorProviderColorBoxSettings);
    procedure Set_TextSettings(Value: TJvColorProviderTextSettings);
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
  published
    property ColorBoxSettings: TJvColorProviderColorBoxSettings read Get_ColorBoxSettings
      write Set_ColorBoxSettings;
    property TextSettings: TJvColorProviderTextSettings read Get_TextSettings
      write Set_TextSettings;
  end;

//===TJvColorProviderColorBoxSettings===============================================================

procedure TJvColorProviderColorBoxSettings.Changed;
begin
  TOpenConsumerServiceExt(ConsumerServiceExt).Changed;
end;

procedure TJvColorProviderColorBoxSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetHeight(Value: Integer);
begin
  if Value <> Height then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetMargin(Value: Integer);
begin
  if Value <> Margin then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetShadowed(Value: Boolean);
begin
  if Value <> Shadowed then
  begin
    FShadowed := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetShadowSize(Value: Integer);
begin
  if Value <> ShadowSize then
  begin
    FShadowSize := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetSpacing(Value: Integer);
begin
  if Value <> Spacing then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetWidth(Value: Integer);
begin
  if Value <> Width then
  begin
    FWidth := Value;
    Changed;
  end;
end;

constructor TJvColorProviderColorBoxSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create;
  FConsumerServiceExt := AConsumerService;
  FActive := True;
  FHeight := 13;
  FMargin := 2;
  FShadowed := True;
  FShadowSize := 2;
  FSpacing := 4;
  FWidth := 21;
end;

//===TJvColorProviderTextSettings===================================================================

procedure TJvColorProviderTextSettings.Changed;
begin
  TOpenConsumerServiceExt(ConsumerServiceExt).Changed;
end;

procedure TJvColorProviderTextSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TJvColorProviderTextSettings.SetShowHex(Value: Boolean);
begin
  if Value <> ShowHex then
  begin
    FShowHex := Value;
    Changed;
  end;
end;

procedure TJvColorProviderTextSettings.SetShowName(Value: Boolean);
begin
  if Value <> ShowName then
  begin
    FShowName := Value;
    Changed;
  end;
end;

procedure TJvColorProviderTextSettings.SetShowRGB(Value: Boolean);
begin
  if Value <> ShowRGB then
  begin
    FShowRGB := Value;
    Changed;
  end;
end;

constructor TJvColorProviderTextSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create;
  FConsumerServiceExt := AConsumerService;
  FActive := True;
  FShowName := True;
end;

//===TJvColorItems==================================================================================

procedure TJvColorItems.AddStdColor(const S: string);
begin
  FColors.AddObject(S, TObject(JclStrToTypedInt(S, TypeInfo(TColor))));
end;

function TJvColorItems.GetCount: Integer;
begin
  Result := FColors.Count;
end;

function TJvColorItems.GetItem(I: Integer): IJvDataItem;
begin
  Result := TJvBaseDataItem.Create(Self);
  TOpenBaseDataItem(Result.GetImplementer).SetID('TCOLOR=' +
    IntToHex(Integer(FColors.Objects[I]), 8));
  TJvColorItemText.Create(TJvBaseDataItem(Result.GetImplementer));
end;

procedure TJvColorItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvColorItemsRenderer.Create(Self);
end;

constructor TJvColorItems.Create;
begin
  inherited Create;
  FColors := TStringList.Create;
  GetColorValues(AddStdColor);
end;

destructor TJvColorItems.Destroy;
begin
  FreeAndNil(FColors);
  inherited Destroy;
end;

//===TJvColorItemText===============================================================================

function TJvColorItemText.GetCaption: string;
var
  ColorValue: TColor;
  ItemsImpl: TJvColorItems;
  ColorIdx: Integer;
begin
  if GetItemColorValue(Item, ColorValue) then
  begin
    ItemsImpl := TJvColorItems(Item.GetItems.GetImplementer);
    ColorIdx := ItemsImpl.FColors.IndexOfObject(TObject(ColorValue));
    if ColorIdx >= 0 then
      Result := ItemsImpl.FColors[ColorIdx]
    else
      Result := '$' + IntToHex(ColorValue, 8);
  end
  else
    Result := 'Invalid ID:' + Item.GetID;
end;

procedure TJvColorItemText.SetCaption(const Value: string);
begin
end;

//===TJvColorProvider===============================================================================

{$IFDEF TestContexts}
class function TJvColorProvider.ContextsClass: TJvDataContextsClass;
begin
  Result := TJvDataContexts;
end;

class function TJvColorProvider.ContextsManagerClass: TJvDataContextsManagerClass;
begin
  Result := nil; //TJvColorDataContextsManager;
end;

{$ENDIF TestContexts}

class function TJvColorProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvColorItems;
end;

function TJvColorProvider.ConsumerClasses: TClassArray;
begin
  Result := inherited ConsumerClasses;
  AddToArray(Result, TJvColorProviderSettings);
end;

procedure TJvColorProvider.AfterConstruction;
var
  CtxMan: IJvDataContextsManager;
begin
  inherited AfterConstruction;
  if GetInterface(IJvDataContextsManager, CtxMan) then
  begin
    with CtxMan.New do
    begin

    end;
  end;
end;

//===TJvColorItemsRenderer==========================================================================

procedure TJvColorItemsRenderer.RenderColorBox;
var
  Margin: Integer;
  BoxW: Integer;
  BoxH: Integer;
  ShadowSize: Integer;
  R: TRect;
  SaveColor: TColor;
begin
  if CurrentSettings.ColorBoxSettings.Active then
  begin
    // We want a color box
    Margin := CurrentSettings.ColorBoxSettings.Margin;
    if CurrentSettings.TextSettings.Active then
      BoxW := CurrentSettings.ColorBoxSettings.Width
    else
      BoxW := CurrentRect.Right - CurrentRect.Left - (2 * Margin);
    BoxH := CurrentSettings.ColorBoxSettings.Height;
    if CurrentSettings.ColorBoxSettings.Shadowed then
      ShadowSize := CurrentSettings.ColorBoxSettings.ShadowSize
    else
      ShadowSize := 0;
    R := CurrentRect;
    OffsetRect(R, Margin, Margin);
    R.Right := R.Left + BoxW - ShadowSize;
    R.Bottom := R.Top + BoxH - ShadowSize;
    if (CurrentItemIsColorItem) and (CurrentColorValue < clNone) then
      with CurrentCanvas do
      begin
        SaveColor := Brush.Color;
        try
          Brush.Color := CurrentColorValue;
          FillRect(R);
          if CurrentSettings.ColorBoxSettings.Shadowed then
          begin
            Brush.Color := clGray;
            OffsetRect(R, ShadowSize, ShadowSize);
            FillRect(R);
            OffsetRect(R, -ShadowSize, -ShadowSize);
          end;
          Brush.Color := CurrentColorValue;
          Rectangle(R);
        finally
          Brush.Color := SaveColor;
        end;
      end;
    if CurrentSettings.TextSettings.Active then
      CurrentRect.Left := R.Right + CurrentSettings.ColorBoxSettings.Spacing;
  end;
end;

procedure TJvColorItemsRenderer.RenderColorText;
var
  ItemText: IJvDataItemText;
  S: string;
  R: TRect;
  OldBkMode: Integer;
begin
  with CurrentSettings.TextSettings do
  begin
    if Active then
    begin
      if ShowName and Supports(CurrentItem, IJvDataItemText, ItemText) then
        S := ItemText.Caption
      else
        S := '';
      if CurrentItemIsColorItem then
      begin
        if ShowHex then
        begin
          if S <> '' then
            S := S + Format(' (%s%.8x)', [HexDisplayPrefix, CurrentColorValue])
          else
            S := Format('%s%.8x', [HexDisplayPrefix, CurrentColorValue]);
        end;
        if ShowRGB then
        begin
          if S <> '' then
            S := S + Format(' (%d, %d, %d)', [
              GetRValue(ColorToRGB(CurrentColorValue)),
              GetGValue(ColorToRGB(CurrentColorValue)),
              GetBValue(ColorToRGB(CurrentColorValue))])
          else
            S := Format('(%d, %d, %d)', [
              GetRValue(ColorToRGB(CurrentColorValue)),
              GetGValue(ColorToRGB(CurrentColorValue)),
              GetBValue(ColorToRGB(CurrentColorValue))]);
        end;
      end;
    end
    else
    if not CurrentItemIsColorItem then
    begin
      if Supports(CurrentItem, IJvDataItemText, ItemText) then
        S := ItemText.Caption
      else
        S := SDataItemRenderHasNoText;
    end
    else
      S := '';
    R := CurrentRect;
    OldBkMode := SetBkMode(CurrentCanvas.Handle, TRANSPARENT);
    try
      DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_END_ELLIPSIS or
        DT_VCENTER or DT_NOPREFIX);
    finally
      SetBkMode(CurrentCanvas.Handle, OldBkMode);
    end;
  end;
end;

procedure TJvColorItemsRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
  State: TProviderDrawStates);
begin
  // setup protected fields
  CurrentCanvas := ACanvas;
  CurrentRect := ARect;
  CurrentItem := Item;
  CurrentState := State;
  CurrentSettings := GetConsumerSettings;
  CurrentItemIsColorItem := GetItemColorValue(Item, CurrentColorValue);
  // render the color box and/or text
  RenderColorBox;
  RenderColorText;
end;

(*
procedure TJvColorItemsRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
  State: TProviderDrawStates);
var
  Color: TColor;
  OldBrush: TBrush;
  OldPen: TPen;
begin
  if GetItemColorValue(Item, Color) then
  begin
    if Color < clNone then
    begin
      // Actual color; clNone and clDefault aren't real colors
      OldBrush := TBrush.Create;
      try
        OldBrush.Assign(ACanvas.Brush);
        OldPen := TPen.Create;
        try
          OldPen.Assign(ACanvas.Pen);
          ACanvas.Brush.Color := Color;
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Pen.Color := Color;
          ACanvas.Pen.Style := psSolid;
          ACanvas.Rectangle(ARect.Left + 4, ARect.Top + 4,
            ARect.Left + (ARect.Bottom - ARect.Top) - 4, ARect.Bottom - 4);
        finally
          ACanvas.Pen := OldPen;
          OldPen.Free;
        end;
      finally
        ACanvas.Brush := OldBrush;
        OldBrush.Free;
      end;
    end;
    Inc(ARect.Left, (ARect.Bottom - ARect.Top));
  end;
  inherited DoDrawItem(ACanvas, ARect, Item, State);
end;
*)

function TJvColorItemsRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
begin
end;

(*function TJvColorItemsRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
var
  Temp: TColor;
begin
  Result := inherited DoMeasureItem(ACanvas, Item);
  if GetItemColorValue(Item, Temp) then
    Inc(Result.cx, Result.cy);
end;*)

function TJvColorItemsRenderer.GetConsumerSettings: IJvColorProviderSettings;
begin
  Supports(Items.GetProvider.SelectedConsumer, IJvColorProviderSettings, Result);
end;

{$IFDEF TestContexts}
//===TJvColorDataContextsManager====================================================================

function TJvColorDataContextsManager.New: IJvDataContext;
begin
  Result := Add(TJvManagedDataContext.Create(ContextsImpl, 'New context'));
end;
{$ENDIF TestContexts}

//===TJvColorProviderSettings=======================================================================

function TJvColorProviderSettings.Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
begin
  Result := FColorBoxSettings;
end;

function TJvColorProviderSettings.Get_TextSettings: TJvColorProviderTextSettings;
begin
  Result := FTextSettings;
end;

procedure TJvColorProviderSettings.Set_ColorBoxSettings(Value: TJvColorProviderColorBoxSettings);
begin
end;

procedure TJvColorProviderSettings.Set_TextSettings(Value: TJvColorProviderTextSettings);
begin
end;

constructor TJvColorProviderSettings.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FColorBoxSettings := TJvColorProviderColorBoxSettings.Create(Self);
  FTextSettings := TJvColorProviderTextSettings.Create(Self);
end;

procedure Register;
begin
  RegisterComponents('Jv DataProviders', [TJvColorProvider]);
end;

initialization
  RegisterClasses([TJvColorProviderSettings]);
{$IFDEF TestContexts}
  RegisterClasses([TJvColorDataContextsManager]);
{$ENDIF TestContexts}
end.
