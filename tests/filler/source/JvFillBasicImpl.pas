unit JvFillBasicImpl;
{
====================================================================================================
  Basic implementers. Could be used for any other filler that needs these types.

  Original author:
    Marcel Bestebroer
====================================================================================================
}

interface

uses
  Windows, Classes, SysUtils, Graphics{$IFNDEF COMPILER6UP}, ComObj{$ENDIF}, ImgList, Contnrs,
  JvComponent, JvFillIntf;

type
  TAggregatedObjectEx = class;
  TJvBaseFillerItem = class;        

  TExtensibleInterfacedObject = class(TObject, IUnknown)
  private
    FAdditionalIntfImpl: TList;
  protected
    FRefCount: Integer;
    { IUnknown }
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure AddIntfImpl(const Obj: TAggregatedObjectEx);
    procedure RemoveIntfImpl(const Obj: TAggregatedObjectEx);
    procedure ClearIntfImpl;
    procedure InitImplementers; virtual;
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  TAggregatedObjectEx = class(TAggregatedObject)
  private
    FOwner: TExtensibleInterfacedObject;
  protected
    property Owner: TExtensibleInterfacedObject read FOwner;
  public
    constructor Create(AOwner: TExtensibleInterfacedObject);
    procedure BeforeDestruction; override;
  end;

  // Basic implementers
  TJvFillerItemAggregatedObject = class(TAggregatedObjectEx)
  protected
    function Item: IFillerItem;
    function ItemImpl: TJvBaseFillerItem;
  end;

  TJvBaseFillerTextItemImpl = class(TJvFillerItemAggregatedObject, IFillerItemText)
  protected
    function getCaption: string; virtual; abstract;
    procedure setCaption(const Value: string); virtual; abstract;
  public
    property Caption: string read getCaption write setCaption;
  end;

  TJvBaseFillerImageItemImpl = class(TJvFillerItemAggregatedObject, IFillerItemImage)
  protected
    function getAlignment: TAlignment; virtual; abstract;
    procedure setAlignment(Value: TAlignment); virtual; abstract;
    function getImageIndex: integer; virtual; abstract;
    procedure setImageIndex(Index: integer); virtual; abstract;
    function getSelectedIndex: integer; virtual; abstract;
    procedure setSelectedIndex(Value: integer); virtual; abstract;
  end;

  TJvBaseFillerItems = class(TExtensibleInterfacedObject, IFillerItems, IFillerIDSearch)
  private
    FParent: Pointer;
    FParentIntf: IFillerItem;
    FFiller: IFiller;
  protected
    { IFillerItems }
    function getCount: Integer; virtual; abstract;
    function getItem(I: Integer): IFillerItem; virtual; abstract;
    function GetParent: IFillerItem;
    function GetFiller: IFiller;
    function GetImplementer: TObject;
    function Attributes: TJvFillerItemsAttributes; virtual;
    { IFillerItemsRenderer }
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent = nil); virtual; abstract;
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent = nil): TSize; virtual; abstract;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil); virtual; abstract;
    function MeasureItem(ACanvas:TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize; virtual; abstract;
    { IFillerIDSearch }
    function FindByID(ID: string; const Recursive: Boolean = False): IFillerItem;
  public
    constructor CreateFiller(const Filler: IFiller);
    constructor CreateParent(const Parent: IFillerItem);
    procedure AfterConstruction; override;
  end;

  TJvFillerItemsAggregatedObject = class(TAggregatedObjectEx)
  protected
    function Items: IFillerItems;
    function ItemsImpl: TJvBaseFillerItems;
  end;

  TJvBaseFillerItemsRenderer = class(TJvFillerItemsAggregatedObject, IFillerItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil); virtual; abstract;
    function DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize; virtual; abstract; 
    { IFillerItemsRenderer}
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent = nil); virtual;
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent = nil): TSize; virtual;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil); virtual;
    function MeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize; virtual; 
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize; virtual; abstract;
  end;

  TJvBaseFillerItemManagment = class(TJvFillerItemsAggregatedObject, IFillerItemManagment)
  protected
    { IFillerItemManagment }
    function Add(Item: IFillerItem): IFillerItem; virtual; abstract;
    function New: IFillerItem; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Remove(Item: IFillerItem); virtual; abstract;
  end;

  TJvBaseFillerItemImagesImpl = class(TJvFillerItemsAggregatedObject, IFillerItemImages)
  protected
    { IFillerItemImages }
    function getImageList: TCustomImageList; virtual; abstract;
    procedure setImageList(const Value: TCustomImageList); virtual; abstract;
  end;

  TJvBaseFillerItem = class(TExtensibleInterfacedObject, IFillerItem)
  private
    FItems: Pointer;
    FItemsIntf: IFillerItems;
    FID: string;
  protected
    procedure InitID; virtual;
    procedure SetID(Value: string);
    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;
    { IFillerItem }
    function GetItems: IFillerItems;
    function GetImplementer: TObject;
    function GetID: string;
    property Items: IFillerItems read GetItems;
    property Implementer: TObject read GetImplementer;
  public
    constructor Create(AItems: IFillerItems);
    procedure AfterConstruction; override;
  end;

  // Standard implementers
  TJvCustomFillerItemsTextRenderer = class(TJvBaseFillerItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize; override;
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize; override;
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

  TJvFillerImageItemImpl = class(TJvBaseFillerImageItemImpl)
  private
    FAlignment: TAlignment;
    FImageIndex: Integer;
    FSelectedIndex: Integer;
  protected
    function getAlignment: TAlignment; override;
    procedure setAlignment(Value: TAlignment); override;
    function getImageIndex: integer; override;
    procedure setImageIndex(Index: integer); override;
    function getSelectedIndex: integer; override;
    procedure setSelectedIndex(Value: integer); override;
  end;

  TJvFillerTextItemImplClass = class of TJvBaseFillerTextItemImpl;
  TJvFillerItemsClass = class of TJvBaseFillerItems;

  TJvFillerItemsList = class(TJvBaseFillerItems)
  private
    FList: TObjectList;
  protected
    function Attributes: TJvFillerItemsAttributes; override;
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent = nil): TSize; override;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function MeasureItem(ACanvas:TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property List: TObjectList read FList;
  end;

  TJvBaseFillerItemsListManagment = class(TJvBaseFillerItemManagment)
  protected
    function Add(Item: IFillerItem): IFillerItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(Item: IFillerItem); override;
  end;

  TJvFillerTextItem = class(TJvBaseFillerItem)
  private
    FTextImpl: TJvBaseFillerTextItemImpl;
  protected
  public
    constructor Create(AItems: IFillerItems; TextImplClass: TJvFillerTextItemImplClass);

    property TextImpl: TJvBaseFillerTextItemImpl read FTextImpl;
  end;

  TJvCustomFiller = class(TJvComponent, {$IFNDEF COMPILER6_UP}IInterfaceComponentReference, {$ENDIF}
    IFiller, IFillerItems)
  private
    FFillerItemsImpl: TJvBaseFillerItems;
    FNotifiers: TInterfaceList;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure NotifyConsumers(ChangeReason: TJvFillerChangeReason);
    class function ItemsClass: TJvFillerItemsClass; virtual;
    {$IFNDEF COMPILER6_UP}
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    {$ENDIF COMPILER6_UP}
    { IFiller }
    function getSupports: TJvFillerSupports; virtual;
    function getOptionClass: TJvFillerOptionsClass; virtual;
    function getItems: IFillerItems; virtual;
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify); virtual;
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify); virtual;

    property FillerItemsImpl: TJvBaseFillerItems read FFillerItemsImpl implements IFillerItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
  end;

function HexBytes(const Buf; Length: Integer): string;

implementation

uses
  ActiveX,
  JclDateTime,
  JvTypes;

function HexBytes(const Buf; Length: Integer): string;
var
  P: PChar;
begin
  Result := '';
  P := @Buf;
  while Length > 1 do
  begin
    Result := Result + IntToHex(Ord(P^), 2);
    Inc(P);
    Dec(Length);
  end;
end;

{ TJvFillerItemAggregatedObject }

function TJvFillerItemAggregatedObject.Item: IFillerItem;
begin
  Result := Owner as IFillerItem;
end;

function TJvFillerItemAggregatedObject.ItemImpl: TJvBaseFillerItem;
begin
  Result := Owner as TJvBaseFillerItem;
end;

{ TJvCustomFillerItemsTextRenderer }

procedure TJvCustomFillerItemsTextRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent);
var
  TextIntf: IFillerItemText;
  S: string;
begin
  if Supports(Item, IFillerItemText, TextIntf) then
    S := TextIntf.Caption
  else
    S := '(item doesn''t support IFillerItemText interface)';
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, S);
end;

function TJvCustomFillerItemsTextRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent): TSize;
var
  TextIntf: IFillerItemText;
  S: string;
begin
  if Supports(Item, IFillerItemText, TextIntf) then
    S := TextIntf.Caption
  else
    S := '(item doesn''t support IFillerItemText interface)';
  Result := ACanvas.TextExtent(S);
end;

function TJvCustomFillerItemsTextRenderer.AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent): TSize;
begin
  Result := ACanvas.TextExtent('WyWyWyWyWyWyWyWyWyWy');
end;

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

{ TExtensibleInterfacedObject }

function TExtensibleInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TExtensibleInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TExtensibleInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TExtensibleInterfacedObject.AddIntfImpl(const Obj: TAggregatedObjectEx);
var
  I: Integer;
begin
  I := FAdditionalIntfImpl.IndexOf(Obj);
  if I < 0 then
    FAdditionalIntfImpl.Add(Obj);
end;

procedure TExtensibleInterfacedObject.RemoveIntfImpl(const Obj: TAggregatedObjectEx);
var
  I: Integer;
begin
  I := FAdditionalIntfImpl.IndexOf(Obj);
  if I > -1 then
  begin
    FAdditionalIntfImpl[I] := nil;
    Obj.Free;
    FAdditionalIntfImpl.Delete(I);
  end;
end;

procedure TExtensibleInterfacedObject.ClearIntfImpl;
var
  I: Integer;
begin
  for I := FAdditionalIntfImpl.Count - 1 downto 0 do
    TObject(FAdditionalIntfImpl[I]).Free;
  FAdditionalIntfImpl.Clear;
end;

procedure TExtensibleInterfacedObject.InitImplementers;
begin
end;

constructor TExtensibleInterfacedObject.Create;
begin
  inherited Create;
  FAdditionalIntfImpl := TList.Create;
end;

procedure TExtensibleInterfacedObject.AfterConstruction;
begin
  inherited AfterConstruction;
  InitImplementers;
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TExtensibleInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then RunError(2);
  inherited BeforeDestruction;
  ClearIntfImpl;
  FAdditionalIntfImpl.Free;
end;

function TExtensibleInterfacedObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  I: Integer;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
  begin
    I := FAdditionalIntfImpl.Count - 1;
    while (I >= 0) and ((FAdditionalIntfImpl[I] = nil) or not TObject(FAdditionalIntfImpl[I]).GetInterface(IID, Obj)) do
      Dec(I);
    Result := I >= 0;
  end;
end;

class function TExtensibleInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  // set a refcount to avoid destruction due to refcounting during construction
  TExtensibleInterfacedObject(Result).FRefCount := 1;
end;

{ TAggregatedObjectEx }

constructor TAggregatedObjectEx.Create(AOwner: TExtensibleInterfacedObject);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
end;

procedure TAggregatedObjectEx.BeforeDestruction;
var
  I: Integer;
begin
  inherited BeforeDestruction;
  I := FOwner.FAdditionalIntfImpl.IndexOf(Self);
  if I >= 0 then
    FOwner.FAdditionalIntfImpl.Delete(I);
end;

{ TJvBaseFillerItems }

function TJvBaseFillerItems.GetParent: IFillerItem;
begin
  Result := IFillerItem(FParent);
end;

function TJvBaseFillerItems.GetFiller: IFiller;
begin
  Result := FFiller;
end;

function TJvBaseFillerItems.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseFillerItems.Attributes: TJvFillerItemsAttributes;
begin
  Result := [fiaDynamicItems];
end;

function TJvBaseFillerItems.FindByID(ID: string; const Recursive: Boolean): IFillerItem;
var
  I: Integer;
  SubItems: IFillerItems;
  Search: IFillerIDSearch;
begin
  I := getCount - 1;
  while (I >= 0) and (getItem(I).GetID <> ID) do
    Dec(I);
  if I >= 0 then
    Result := getItem(I)
  else
  begin
    Result := nil;
    if Recursive then
    begin
      I := getCount - 1;
      while (I >= 0) and (Result = nil) do
      begin
        if Supports(getItem(I), IFillerItems, SubItems) then
        begin
          if Supports(SubItems, IFillerIDSearch, Search) then
            Result := Search.FindByID(ID, Recursive);
        end;
        Dec(I);
      end;
    end;
  end;
end;

constructor TJvBaseFillerItems.CreateFiller(const Filler: IFiller);
begin
  Create;
  FFiller := Filler;
end;

constructor TJvBaseFillerItems.CreateParent(const Parent: IFillerItem);
begin
  CreateFiller(Parent.Items.Filler);
  FParent := Pointer(Parent);
  if (Parent <> nil) and (fiaDynamicItems in Parent.Items.Attributes) then
    FParentIntf := Parent;
end;

procedure TJvBaseFillerItems.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ TJvFillerItemsAggregatedObject }

function TJvFillerItemsAggregatedObject.Items: IFillerItems;
begin
  Result := Owner as IFillerItems;
end;

function TJvFillerItemsAggregatedObject.ItemsImpl: TJvBaseFillerItems;
begin
  Result := Owner as TJvBaseFillerItems;
end;

{ TJvBaseFillerItemsRenderer }

procedure TJvBaseFillerItemsRenderer.DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent);
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise EJVCLException.CreateFmt('Index out of range (%d)', [Index]);
  DrawItem(ACanvas, ARect, Items.Items[Index], State, AOptions);
end;

function TJvBaseFillerItemsRenderer.MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent): TSize;
begin
  if Index = -1 then
    Result := AvgItemSize(ACanvas, AOptions)
  else
  begin
    if (Index < 0) or (Index >= Items.Count) then
      raise EJVCLException.CreateFmt('Index out of range (%d)', [Index]);
    Result := MeasureItem(ACanvas, Items.Items[Index], AOptions);
  end;
end;

procedure TJvBaseFillerItemsRenderer.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent);
var
  ImgRender: IFillerItemRenderer;
begin
  if Supports(Item, IFillerItemRenderer, ImgRender) then
    ImgRender.Draw(ACanvas, ARect, State, AOptions)
  else
    DoDrawItem(ACanvas, ARect, Item, State, AOptions);
end;

function TJvBaseFillerItemsRenderer.MeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent): TSize;
var
  ImgRender: IFillerItemRenderer;
begin
  if Supports(Item, IFillerItemRenderer, ImgRender) then
    Result := ImgRender.Measure(ACanvas, AOptions)
  else
    Result := DoMeasureItem(ACanvas, Item, AOptions);
end;

{ TJvFillerItemsList }

function TJvFillerItemsList.Attributes: TJvFillerItemsAttributes;
begin
  Result := [];
end;

function TJvFillerItemsList.getCount: Integer;
begin
  Result := List.Count;
end;

function TJvFillerItemsList.getItem(I: Integer): IFillerItem;
begin
  Result := (List[I] as TJvBaseFillerItem) as IFillerItem;
end;

procedure TJvFillerItemsList.DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState; AOptions: TPersistent);
begin
  DrawItem(ACanvas, ARect, getItem(Index), State, AOptions);
end;

function TJvFillerItemsList.MeasureItemByIndex(ACanvas:TCanvas; Index: Integer; AOptions: TPersistent): TSize;
begin
  Result := MeasureItem(ACanvas, getItem(Index), AOptions);
end;

procedure TJvFillerItemsList.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent);
begin
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, (Item as IFillerItemText).Caption);
end;

function TJvFillerItemsList.MeasureItem(ACanvas:TCanvas; Item: IFillerItem; AOptions: TPersistent): TSize;
begin
  Result := ACanvas.TextExtent((Item as IFillerItemText).Caption);
end;

procedure TJvFillerItemsList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TObjectList.Create;
end;

procedure TJvFillerItemsList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FList.Free;
end;

{ TJvBaseFillerItemsListManagment }

function TJvBaseFillerItemsListManagment.Add(Item: IFillerItem): IFillerItem;
begin
  TJvFillerItemsList(ItemsImpl).List.Add(Item.GetImplementer);
  Result := Item;
end;

procedure TJvBaseFillerItemsListManagment.Clear;
begin
  TJvFillerItemsList(ItemsImpl).List.Clear;
end;

procedure TJvBaseFillerItemsListManagment.Delete(Index: Integer);
begin
  TJvFillerItemsList(ItemsImpl).List.Delete(Index);
end;

procedure TJvBaseFillerItemsListManagment.Remove(Item: IFillerItem);
begin
  TJvFillerItemsList(ItemsImpl).List.Remove(Item.GetImplementer);
end;

{ TJvBaseFillerItem }

procedure TJvBaseFillerItem.InitID;
var
  G: TGUID;
begin
  CoCreateGuid(G);
  FID := HexBytes(G, SizeOf(G));
end;

procedure TJvBaseFillerItem.SetID(Value: string);
begin
  FID := Value;
end;

function TJvBaseFillerItem._AddRef: Integer;
begin
  if fiaDynamicItems in Items.Attributes then
    Result := inherited _AddRef
  else
    Result := -1;
end;

function TJvBaseFillerItem._Release: Integer;
begin
  if fiaDynamicItems in Items.Attributes then
    Result := inherited _Release
  else
    Result := -1;
end;

function TJvBaseFillerItem.GetItems: IFillerItems;
begin
  Result := IFillerItems(FItems);
end;

function TJvBaseFillerItem.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseFillerItem.GetID: string;
begin
  Result := FID;
end;

constructor TJvBaseFillerItem.Create(AItems: IFillerItems);
begin
  inherited Create;
  FItems := Pointer(AItems);
  // Dynamically generated items will need a hard reference to the IFillerItems owner.
  if fiaDynamicItems in AItems.Attributes then
    FItemsIntf := AItems;
end;

procedure TJvBaseFillerItem.AfterConstruction;
begin
  InitID;
  inherited AfterConstruction;
end;

{ JvFillerTextItem }

constructor TJvFillerTextItem.Create(AItems: IFillerItems; TextImplClass: TJvFillerTextItemImplClass);
begin
  inherited Create(AItems);
  FTextImpl := TextImplClass.Create(Self);
  AddIntfImpl(FTextImpl);
end;

{ TJvCustomFiller }

function TJvCustomFiller.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TJvCustomFiller.NotifyConsumers(ChangeReason: TJvFillerChangeReason);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IFillerNotify).FillerChanging(Self, ChangeReason);
end;

class function TJvCustomFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvFillerItemsList;
end;

{$IFNDEF COMPILER6_UP}
function TJvCustomFiller.GetComponent: TComponent;
begin
  Result := Self;
end;
{$ENDIF COMPILER6_UP}

function TJvCustomFiller.getSupports: TJvFillerSupports;
begin
  Result := [];
end;

function TJvCustomFiller.getOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

function TJvCustomFiller.getItems: IFillerItems;
begin
  Result := FillerItemsImpl;
end;

procedure TJvCustomFiller.RegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  if FNotifiers.IndexOf(AFillerNotify) < 0 then
    FNotifiers.Add(AFillerNotify);
end;

procedure TJvCustomFiller.UnRegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  FNotifiers.Remove(AFillerNotify);
end;

constructor TJvCustomFiller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifiers := TInterfaceList.Create;
  if ItemsClass <> nil then
    FFillerItemsImpl := ItemsClass.CreateFiller(Self)
  else
    raise EJVCLException.Create('Can''t create a filler without an IFillerItems implementation.');
  FFillerItemsImpl._AddRef;
end;

destructor TJvCustomFiller.Destroy;
begin
  FNotifiers.Clear;
  inherited Destroy;
end;

procedure TJvCustomFiller.BeforeDestruction;
begin
  if (FFillerItemsImpl <> nil) and (FFillerItemsImpl.RefCount > 1) then
    RunError(2);
  FFillerItemsImpl._Release;
  inherited BeforeDestruction;
  NotifyConsumers(frDestroy);
end;

function TJvCustomFiller.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or (FFillerItemsImpl.GetInterface(IID, Obj));
end;

end.
