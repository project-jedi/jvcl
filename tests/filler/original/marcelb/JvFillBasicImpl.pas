unit JvFillBasicImpl;

interface

uses
  Windows, Classes, SysUtils, Graphics, JvFillIntf_mbe2, ComObj;

type
  // Basic implementers
  TJvBaseFillerTextItemImpl = class(TAggregatedObject)
  protected
    function getCaption: string; virtual; abstract;
    procedure setCaption(const Value: string); virtual; abstract;
  public
    property Caption: string read getCaption write setCaption;
  end;

  TJvFillerTextItemImpl = class(TJvBaseFillerTextItemImpl)
  private
    FCaption: string;
  protected
    function getCaption: string; override;
    procedure setCaption(const Value: string); override;
  public
    property Caption: string read getCaption write setCaption;
  end;

  TJvFillerImageItemImpl = class(TAggregatedObject)
  private
    FAlignment: TAlignment;
    FImageIndex: Integer;
    FSelectedIndex: Integer;
  protected
    function getAlignment: TAlignment;
    procedure setAlignment(Value: TAlignment);
    function getImageIndex: integer;
    procedure setImageIndex(Index: integer);
    function getSelectedIndex: integer;
    procedure setSelectedIndex(Value: integer);
  end;

  TJvFillerTextItemImplClass = class of TJvBaseFillerTextItemImpl;

  TJvFillerItems = class(TAggregatedObject, IFillerItems)
  private
    FParent: Pointer; // Don't want a hard reference
  protected
    { IFillerItems }
    function getCount: Integer; virtual; abstract;
    function getItem(I: Integer): IFillerItem; virtual; abstract;
    function GetParent: IFillerItem;
    function GetFiller: IBaseFiller;
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent = nil); virtual; abstract;
    function MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent = nil): TSize; virtual; abstract;

    property Filler: IBaseFiller read GetFiller;
  public
    constructor CreateFiller(const Filler: IBaseFiller);
    constructor CreateParent(const Parent: IFillerItem);
  end;

  TJvFillerItemsList = class(TJvFillerItems)
  private
    FList: TInterfaceList;
  protected
    { IFillerItems }
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent = nil): TSize; override;
  public
    procedure AfterConstruction; override;

    property List: TInterfaceList read FList;
  end;

  TJvFillerItem = class(TInterfacedObject, IFillerItem)
  private
    FItems: Pointer; // No hard reference
  protected
    function GetItems: IFillerItems;
    property Items: IFillerItems read GetItems;
  public
    constructor Create(AItems: IFillerItems);
  end;

  TJvFillerTextItem = class(TJvFillerItem, IFillerItem, IFillerItemText)
  private
    FTextImpl: TJvBaseFillerTextItemImpl;
  protected
  public
    constructor Create(AItems: IFillerItems; TextImplClass: TJvFillerTextItemImplClass);

    property TextImpl: TJvBaseFillerTextItemImpl read FTextImpl implements IFillerItemText;
  end;

implementation

{
====================================================================================================
  Basic implementers. Could be used for any other filler that needs these types.
====================================================================================================
}

{ TJvFillerTextItem }

function TJvFillerTextItemImpl.getCaption: string;
begin
  Result := FCaption;
end;

procedure TJvFillerTextItemImpl.setCaption(const Value: string);
begin
  FCaption := Value;
end;

{ TJvFillerImageItem }

function TJvFillerImageItemImpl.getAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TJvFillerImageItemImpl.setAlignment(Value: TAlignment);
begin
  FAlignment := Value;
end;

function TJvFillerImageItemImpl.getImageIndex: integer;
begin
  Result := FImageIndex;
end;

procedure TJvFillerImageItemImpl.setImageIndex(Index: integer);
begin
  FImageIndex := Index;
end;

function TJvFillerImageItemImpl.getSelectedIndex: integer;
begin
  Result := FSelectedIndex;
end;

procedure TJvFillerImageItemImpl.setSelectedIndex(Value: integer);
begin
  FSelectedIndex := Value;
end;

{ TJvFillerItems }

function TJvFillerItems.GetParent: IFillerItem;
begin
  Result := IFillerItem(FParent);
end;

function TJvFillerItems.GetFiller: IBaseFiller;
begin
  Result := Controller as IBaseFiller;
end;

constructor TJvFillerItems.CreateFiller(const Filler: IBaseFiller);
begin
  inherited Create(Filler);
  FParent := nil;
end;

constructor TJvFillerItems.CreateParent(const Parent: IFillerItem);
begin
  inherited Create(Parent.Items.Filler);
  FParent := Pointer(Parent);
end;

{ TJvFillerItemsList }

function TJvFillerItemsList.getCount: Integer;
begin
  Result := List.Count;
end;

function TJvFillerItemsList.getItem(I: Integer): IFillerItem;
begin
  Result := List[I] as IFillerItem;
end;

procedure TJvFillerItemsList.DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: integer;State: TOwnerDrawState; AOptions: TPersistent);
begin
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, (List[Index] as IFillerItemText).Caption);
end;

function TJvFillerItemsList.MeasureItem(ACanvas:TCanvas; Index: integer; AOptions: TPersistent): TSize;
begin
  Result := ACanvas.TextExtent((List[Index] as IFillerItemText).Caption);
end;

procedure TJvFillerItemsList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TInterfaceList.Create;
end;

{ TJvFillerItem }

function TJvFillerItem.GetItems: IFillerItems;
begin
  Result := IFillerItems(FItems);
end;

constructor TJvFillerItem.Create(AItems: IFillerItems);
begin
  inherited Create;
  FItems := Pointer(AItems);
end;

{ JvFillerTextItem }

constructor TJvFillerTextItem.Create(AItems: IFillerItems; TextImplClass: TJvFillerTextItemImplClass);
begin
  inherited Create(AItems);
  FTextImpl := TextImplClass.Create(Self);
end;

end.
