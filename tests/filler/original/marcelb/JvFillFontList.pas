unit JvFillFontList;

interface

uses
  Windows, SysUtils, Classes,
  JvFillBasicImpl, JvFillIntf_mbe2;

type
  TJvFontFiller = class(TComponent, IBaseFiller, IFiller, IFillerItems)
  private
    FFillerItemsImpl: TJvFillerItems;
    FNotifiers: TInterfaceList;
  protected
    procedure NotifyConsumers(ChangeReason: TJvFillerChangeReason);
    { IFiller }
    function getSupports: TJvFillerSupports;
    function getOptionClass: TJvFillerOptionsClass;
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify);
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify);

    { IFillerItems }
    property FillerItemsImpl: TJvFillerItems read FFillerItemsImpl implements IFillerItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
  end;

  TFontFillerOptions = class(TJvFillerOptions)
  private
    FUseFontNames: Boolean;
  protected
    procedure SetUseFontNames(Value: Boolean);
  public
  published
    property UseFontNames: Boolean read FUseFontNames write SetUseFontNames;
  end;

implementation

uses
  Forms, Graphics;

type
  TJvFontItems = class(TJvFillerItems)
  protected
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent = nil): TSize; override;
  end;

  TJvFontItem = class(TJvFillerTextItem)
  public
    constructor Create(AItems: IFillerItems; const Index: Integer);
  end;

  TJvFontItemText = class(TJvBaseFillerTextItemImpl)
  private
    FIndex: Integer;
  protected
    function getCaption: string; override;
    procedure setCaption(const Value: string); override;
  end;

{ TJvFontItems }

function TJvFontItems.getCount: Integer;
begin
  Result := Screen.Fonts.Count;
end;

function TJvFontItems.getItem(I: Integer): IFillerItem;
begin
  Result := TJvFontItem.Create(Self, I);
end;

procedure TJvFontItems.DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent);
var
  TmpFont: TFont;
begin
  if (AOptions <> nil) and (AOptions is TFontFillerOptions) and TFontFillerOptions(AOptions).UseFontNames then
  begin
    TmpFont := TFont.Create;
    try
      TmpFont.Assign(ACanvas.Font);
      try
        ACanvas.Font.Name := Screen.Fonts[Index];
        ACanvas.TextRect(ARect, ARect.Left, ARect.Top, ACanvas.Font.Name);
      finally
        ACanvas.Font.Assign(TmpFont);
      end;
    finally
      TmpFont.Free;
    end;
  end
  else
    ACanvas.TextRect(ARect, ARect.Left, ARect.Top, (getItem(Index) as IFillerItemText).Caption);
end;

function TJvFontItems.MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent): TSize;
var
  TmpFont: TFont;
begin
  if (Index > -1) and (AOptions <> nil) and (AOptions is TFontFillerOptions) and TFontFillerOptions(AOptions).UseFontNames then
  begin
    TmpFont := TFont.Create;
    try
      TmpFont.Assign(ACanvas.Font);
      try
        ACanvas.Font.Name := Screen.Fonts[Index];
        Result := ACanvas.TextExtent(ACanvas.Font.Name);
      finally
        ACanvas.Font.Assign(TmpFont);
      end;
    finally
      TmpFont.Free;
    end;
  end
  else if Index > -1 then
    Result := ACanvas.TextExtent((getItem(Index) as IFillerItemText).Caption)
  else
    Result := ACanvas.TextExtent('WyWyWyWyWyWyWy');
end;

{ TJvFontItem }

constructor TJvFontItem.Create(AItems: IFillerItems; const Index: Integer);
begin
  inherited Create(AItems, TJvFontItemText);
  TJvFontItemText(TextImpl).FIndex := Index; 
end;

{ TJvFontItemText }

function TJvFontItemText.getCaption: string;
begin
  Result := Screen.Fonts[FIndex];
end;

procedure TJvFontItemText.setCaption(const Value: string);
begin
  raise Exception.Create('Font filler is a read-only list.');
end;

{ TJvFontFiller }

procedure TJvFontFiller.NotifyConsumers(ChangeReason: TJvFillerChangeReason);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IFillerNotify).FillerChanging(Self, ChangeReason);
end;

function TJvFontFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsReadOnly, fsCanrender, fsCanMeasure];
end;

function TJvFontFiller.getOptionClass: TJvFillerOptionsClass;
begin
  Result := TFontFillerOptions;
end;

procedure TJvFontFiller.RegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  if FNotifiers.IndexOf(AFillerNotify) < 0 then
    FNotifiers.Add(AFillerNotify);
end;

procedure TJvFontFiller.UnRegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  FNotifiers.Remove(AFillerNotify);
end;

constructor TJvFontFiller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifiers := TInterfaceList.Create;
  FFillerItemsImpl := TJvFontItems.CreateFiller(Self as IBaseFiller);
end;

destructor TJvFontFiller.Destroy;
begin
  FNotifiers.Clear;
  FFillerItemsImpl.Free;
  inherited Destroy;
end;

procedure TJvFontFiller.BeforeDestruction;
begin
  NotifyConsumers(frDestroy);
end;

{ TFontFillerOptions }

procedure TFontFillerOptions.SetUseFontNames(Value: Boolean);
begin
  if Value <> UseFontNames then
  begin
    FUseFontNames := Value;
    Changed;
  end;
end;

initialization
  RegisterClass(TFontFillerOptions);
end.
