unit JvFillStringList;

{$I JVCL.INC}

interface

uses
  Windows, SysUtils, Classes,
  JvFillBasicImpl, JvFillIntf;

type
  TJvStringsFiller = class(TJvCustomFiller)
  protected
    class function ItemsClass: TJvFillerItemsClass; override;
    function getSupports: TJvFillerSupports; override;
    function getOptionClass: TJvFillerOptionsClass; override;

    function GetStrings: TStrings;
    procedure SetStrings(Value: TStrings);
  published
    property Strings: TStrings read GetStrings write SetStrings; 
  end;

implementation

uses
  Graphics,
  JvTypes;

type
  TJvStringsFillerItems = class(TJvFillerItems)
  private
    FItems: TStrings;
  protected
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent = nil): TSize; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TJvStringsFillerItem = class(TJvFillerTextItem)
  public
    constructor Create(AItems: IFillerItems; AStringList: TStrings; AIndex: Integer);
  end;

  TJvStringsFillerItemText = class(TJvBaseFillerTextItemImpl)
  private
    FIndex: Integer;
    FStrings: TStrings;
  protected
    function getCaption: string; override;
    procedure setCaption(const Value: string); override;
  end;

{ TJvStringsFillerItems }

function TJvStringsFillerItems.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvStringsFillerItems.getItem(I: Integer): IFillerItem;
begin
  Result := TJvStringsFillerItem.Create(Self, FItems, I);
end;

procedure TJvStringsFillerItems.DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent = nil);
begin
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, (getItem(Index) as IFillerItemText).Caption);
end;

function TJvStringsFillerItems.MeasureItem(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent = nil): TSize;
begin
  if Index > -1 then
    Result := ACanvas.TextExtent((getItem(Index) as IFillerItemText).Caption)
  else
    Result := ACanvas.TextExtent('WyWyWyWyWyWyWy');
end;

procedure TJvStringsFillerItems.AfterConstruction;
begin
  inherited AfterConstruction;
  FItems := TStringList.Create;
end;

procedure TJvStringsFillerItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FItems.Free;
end;

{ TJvStringsFillerItem }

constructor TJvStringsFillerItem.Create(AItems: IFillerItems; AStringList: TStrings; AIndex: Integer);
begin
  inherited Create(AItems, TJvStringsFillerItemText);
  TJvStringsFillerItemText(TextImpl).FIndex := AIndex;
  TJvStringsFillerItemText(TextImpl).FStrings := AStringList;
end;

{ TJvStringsFillerItemText }

function TJvStringsFillerItemText.getCaption: string;
begin
  Result := FStrings[FIndex];
end;

procedure TJvStringsFillerItemText.setCaption(const Value: string);
begin
  if not (fsReadonly in (Item.Items.Filler as IFiller).getSupports) then
    FStrings[FIndex] := Value
  else
    raise EJVCLException.Create('Filler is read only; you can''t change the item.');
end;

{ TJvStringsFiller }

class function TJvStringsFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvStringsFillerItems;
end;

function TJvStringsFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsCanRender, fsCanMeasure];
end;

function TJvStringsFiller.getOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

function TJvStringsFiller.GetStrings: TStrings;
begin
  Result := (FillerItemsImpl as TJvStringsFillerItems).FItems;
end;

procedure TJvStringsFiller.SetStrings(Value: TStrings);
begin
  if fsReadonly in getSupports then
    raise EJVCLException.Create('Filler is marked read only; you can''t change the list.');
  (FillerItemsImpl as TJvStringsFillerItems).FItems.Assign(Value);
  NotifyConsumers(frUpdate);
end;

end.
