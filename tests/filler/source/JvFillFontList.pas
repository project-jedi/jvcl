unit JvFillFontList;

interface

{$I JVCL.INC}

uses
  Windows, SysUtils, Classes,
  JvFillBasicImpl, JvFillIntf;

type
  TJvFontFiller = class(TJvCustomFiller)
  protected
    class function ItemsClass: TJvFillerItemsClass; override;
    function getSupports: TJvFillerSupports; override;
    function getOptionClass: TJvFillerOptionsClass; override;
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
  TJvFontItems = class(TJvBaseFillerItems)
  protected
    procedure InitImplementers; override;
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
  end;

  TJvFontItem = class(TJvFillerTextItem)
  protected
    procedure InitID; override;
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

procedure TJvFontItems.InitImplementers;
begin
  inherited InitImplementers;
  {AddIntfImpl(}TJvCustomFillerItemsTextRenderer.Create(Self){)};
end;

function TJvFontItems.getCount: Integer;
begin
  Result := Screen.Fonts.Count;
end;

function TJvFontItems.getItem(I: Integer): IFillerItem;
begin
  Result := TJvFontItem.Create(Self, I);
end;

{
procedure TJvFontItems.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent);
var
  TmpFont: TFont;
begin
  if (AOptions <> nil) and (AOptions is TFontFillerOptions) and TFontFillerOptions(AOptions).UseFontNames then
  begin
    TmpFont := TFont.Create;
    try
      TmpFont.Assign(ACanvas.Font);
      try
        ACanvas.Font.Name := (Item as IFillerItemText).Caption;
        ACanvas.TextRect(ARect, ARect.Left, ARect.Top, ACanvas.Font.Name);
      finally
        ACanvas.Font.Assign(TmpFont);
      end;
    finally
      TmpFont.Free;
    end;
  end
  else
    ACanvas.TextRect(ARect, ARect.Left, ARect.Top, (Item as IFillerItemText).Caption);
end;

function TJvFontItems.MeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent): TSize;
var
  TmpFont: TFont;
begin
  if (Item <> nil) and (AOptions <> nil) and (AOptions is TFontFillerOptions) and TFontFillerOptions(AOptions).UseFontNames then
  begin
    TmpFont := TFont.Create;
    try
      TmpFont.Assign(ACanvas.Font);
      try
        ACanvas.Font.Name := (Item as IFillerItemText).Caption;
        Result := ACanvas.TextExtent(ACanvas.Font.Name);
      finally
        ACanvas.Font.Assign(TmpFont);
      end;
    finally
      TmpFont.Free;
    end;
  end
  else if Item <> nil then
    Result := ACanvas.TextExtent((Item as IFillerItemText).Caption)
  else
    Result := ACanvas.TextExtent('WyWyWyWyWyWyWy');
end;
}

{ TJvFontItem }

procedure TJvFontItem.InitID;
begin
  SetID(IntToHex(TJvFontItemText(TextImpl).FIndex, 4));
end;

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

class function TJvFontFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvFontItems;
end;

function TJvFontFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsReadOnly, fsCanrender, fsCanMeasure];
end;

function TJvFontFiller.getOptionClass: TJvFillerOptionsClass;
begin
  Result := TFontFillerOptions;
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
