unit Filler;

interface

uses
  Classes, Types, StdCtrls, JvFillIntf4, Controls, Graphics, ImgList;

type
  TFillClass1 = class;

  TItemText = class(TInterfacedObject, IFillerItemText, IFillerItem)
  private
    FOwner: TFillClass1;
    FIndex: Integer;
  protected
    function getCaption: string;
    procedure setCaption(const Value: string);
  public
    constructor Create(AOwner: TFillClass1; AIndex: Integer); virtual;
  end;

  TFillerItemTextImpl = class(TAggregatedObject, IFillerItemText)
  private
    FCaption: string;
  protected
    function getCaption: string;
    procedure setCaption(const Value: string);
  end;

  TFillerItemImageImpl = class(TAggregatedObject, IFillerItemImage)
  private
    FAlignment: TAlignment;
    FImageIndex: Integer;
    FSelectedIndex: Integer;
  protected
    function getAlignment: TAlignment;
    procedure setAlignment(Value: TAlignment);
    function getImageIndex: Integer;
    procedure setImageIndex(Index: Integer);
    function getSelectedIndex: Integer;
    procedure setSelectedIndex(Value: Integer);
  end;

  TXCollection = class;
  TFillClass2 = class;

  TXContainerItem = class(TCollectionItem, IInterface,
      IFillerItem,
      IFillerItemImages,
      IFillerItemText,
      IFillerItemImage)
  private
    FImageImpl: TFillerItemImageImpl;
    FTextImpl: TFillerItemTextImpl;
    FImagesImpl: IFillerItemImages;
    function GetIntf: IFillerItem;
    function GetCaption: string;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetSelectedIndex(const Value: Integer);
    function GetAlignment: TAlignment;
    function GetImageIndex: Integer;
    function GetSelectedIndex: Integer;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  public
    property ImageImpl: TFillerItemImageImpl read FImageImpl
      implements IFillerItemImage;
    property ImagesImpl: IFillerItemImages read FImagesImpl
      implements IFillerItemImages;
    property TextImpl: TFillerItemTextImpl read FTextImpl implements
      IFillerItemText;
    property Intf: IFillerItem read GetIntf;
  published
    property Caption: string read GetCaption write SetCaption;
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property SelectedIndex: Integer read GetSelectedIndex write
      SetSelectedIndex;
  end;

  TXCollection = class(TCollection)
  private
    FParent: TFillClass2;
    function getItems(Index: Integer): TXContainerItem;
    procedure setItems(Index: Integer; const Value: TXContainerItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AParent: TFillClass2);
    function Add: TXContainerItem;
    property Items[Index: Integer]: TXContainerItem read getItems write
    setItems; default;
  end;

  TBaseFiller = class(TComponent, IBaseFiller)
  private
    FNotifyList: TInterfaceList;
  protected
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify);
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify);

    procedure DoChanged(ChangeReason: TJvFillerChangeReason);
    procedure DoChanging(ChangeReason: TJvFillerChangeReason);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFillClass1 = class(TBaseFiller, IFiller)
  private
    FCaptions: TStrings;
    procedure SetCaptions(const Value: TStrings);
  protected
    procedure DrawItem(hDC: LongWord; var ARect: TRect; Index: Integer);
    function MeasureItem(Index: Integer): TSize;
    function GetCount: Integer;
    function getItem(Index: Integer): IFillerItem;
    function getSupports: TJvFillerSupports;
    procedure CaptionsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Captions: TStrings read FCaptions write SetCaptions;
    property Count: Integer read GetCount;
  end;

  TFillClass2 = class(TBaseFiller, IFiller, IFillerItemImages)
  private
    FCanvas: TCanvas;
    FItems: TXCollection;
    FImageChangeLink: TChangeLink;
    // Should probably be TCustomImageList
    FImages: TImageList;
    procedure ImageListChange(Sender: TObject);
    function GetItems: TXCollection;
    procedure SetItems(const Value: TXCollection);
  protected
    { IFiller }
    procedure DrawItem(hDC: LongWord; var ARect: TRect; Index: Integer);
    function MeasureItem(Index: Integer): TSize;
    function GetCount: Integer;
    function getItem(Index: Integer): IFillerItem;
    function getSupports: TJvFillerSupports;
    { IFillerItemImages }
    function getImageList: TImageList;
    procedure setImageList(const Value: TImageList);

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure UpdateItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Images: TImageList read getImageList write setImageList;
    property Items: TXCollection read GetItems write SetItems;
  end;

  TFillLabel = class(TLabel, IFillerNotify)
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

procedure Register;

implementation

{ TFillClass1 }

procedure TFillClass1.CaptionsChanged(Sender: TObject);
begin
  DoChanged(frUpdate);
end;

constructor TFillClass1.Create(AOwner: TComponent);
begin
  inherited;
  FCaptions := TStringList.Create;
  TStringList(FCaptions).OnChange := CaptionsChanged;
end;

destructor TFillClass1.Destroy;
begin
  inherited;
  FCaptions.Free;
end;

procedure TFillClass1.DrawItem(hDC: LongWord; var ARect: TRect;
  Index: Integer);
begin
end;

function TFillClass1.GetCount: Integer;
begin
  Result := FCaptions.Count;
end;

function TFillClass1.getItem(Index: Integer): IFillerItem;
begin
  Result := TItemText.Create(Self, Index);
end;

function TFillClass1.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsReadOnly];
end;

function TFillClass1.MeasureItem(Index: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

procedure TFillClass1.SetCaptions(const Value: TStrings);
begin
  FCaptions.Assign(Value);
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
  if Assigned(Filler) and (fsCanRender in Filler.getSupports) and
    (FIndex >= 0) and (FIndex < Filler.Count) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Font := Font;
    Filler.DrawItem(Canvas.Handle, Rect, FIndex);
  end
  else
    inherited DoDrawText(Rect, Flags);
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
begin
  Perform(CM_TEXTCHANGED, 0, 0);
end;

{ TItemText }

function TItemText.getCaption: string;
begin
  Result := FOwner.Captions[FIndex];
end;

procedure TItemText.setCaption(const Value: string);
begin
  FOwner.Captions[FIndex] := Value;
end;

constructor TItemText.Create(AOwner: TFillClass1; AIndex: Integer);
begin
  FOwner := AOwner;
  FIndex := AIndex;
end;

procedure Register;
begin
  RegisterComponents('Filler', [TFillClass1, TFillLabel, TFillClass2]);
end;

{ TBaseClass }

constructor TBaseFiller.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyList := TInterfaceList.Create;
end;

destructor TBaseFiller.Destroy;
begin
  DoChanging(frDestroy);
  FNotifyList.Free;
  inherited;
end;

procedure TBaseFiller.DoChanged(ChangeReason: TJvFillerChangeReason);
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
    (FNotifyList[i] as IFillerNotify).FillerChanged(Self as IFiller,
      ChangeReason);
end;

procedure TBaseFiller.DoChanging(ChangeReason: TJvFillerChangeReason);
var
  i: Integer;
begin
  for i := 0 to FNotifyList.Count - 1 do
    (FNotifyList[i] as IFillerNotify).FillerChanging(Self as IFiller,
      ChangeReason);
end;

procedure TBaseFiller.RegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  FNotifyList.Add(AFillerNotify);
end;

procedure TBaseFiller.UnRegisterChangeNotify(AFillerNotify: IFillerNotify);
var
  Index: Integer;
begin
  Index := FNotifyList.IndexOf(AFillerNotify);
  if Index >= 0 then
    FNotifyList.Delete(Index);
end;

{ TFillerItemTextImpl }

function TFillerItemTextImpl.getCaption: string;
begin
  Result := FCaption;
end;

procedure TFillerItemTextImpl.setCaption(const Value: string);
begin
  FCaption := Value;
end;

{ TFillerItemImageImpl }

function TFillerItemImageImpl.getAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TFillerItemImageImpl.getImageIndex: Integer;
begin
  Result := FImageIndex;
end;

function TFillerItemImageImpl.getSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

procedure TFillerItemImageImpl.setAlignment(Value: TAlignment);
begin
  FAlignment := Value;
end;

procedure TFillerItemImageImpl.setImageIndex(Index: Integer);
begin
  FImageIndex := Index;
end;

procedure TFillerItemImageImpl.setSelectedIndex(Value: Integer);
begin
  FSelectedIndex := Value;
end;

{ TXContainerItem }

constructor TXContainerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FImageImpl := TFillerItemImageImpl.Create(Self);
  FImagesImpl := (Collection as TXCollection).FParent;
  FTextImpl := TFillerItemTextImpl.Create(Self);
end;

destructor TXContainerItem.Destroy;
begin
  FImageImpl.Free;
  FTextImpl.Free;
  inherited;
end;

function TXContainerItem._AddRef: Integer;
begin
  Result := -1;
end;

function TXContainerItem._Release: Integer;
begin
  Result := -1;
end;

function TXContainerItem.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

function TXContainerItem.GetIntf: IFillerItem;
begin
  Result := Self;
end;

function TXContainerItem.GetCaption: string;
begin
  Result := FTextImpl.getCaption;
end;

procedure TXContainerItem.SetAlignment(const Value: TAlignment);
begin
  FImageImpl.setAlignment(Value);
end;

procedure TXContainerItem.SetCaption(const Value: string);
begin
  FTextImpl.setCaption(Value);
end;

procedure TXContainerItem.SetImageIndex(const Value: Integer);
begin
  FImageImpl.setImageIndex(Value);
end;

procedure TXContainerItem.SetSelectedIndex(const Value: Integer);
begin
  FImageImpl.setSelectedIndex(Value);
end;

function TXContainerItem.GetAlignment: TAlignment;
begin
  Result := FImageImpl.getAlignment;
end;

function TXContainerItem.GetImageIndex: Integer;
begin
  Result := FImageImpl.getImageIndex;
end;

function TXContainerItem.GetSelectedIndex: Integer;
begin
  Result := FImageImpl.getSelectedIndex;
end;

{ TXCollection }

function TXCollection.Add: TXContainerItem;
begin
  Result := TXContainerItem(inherited Add);
end;

constructor TXCollection.Create(AParent: TFillClass2);
begin
  inherited Create(TXContainerItem);
  FParent := AParent;
end;

function TXCollection.getItems(Index: Integer): TXContainerItem;
begin
  Result := TXContainerItem(inherited Items[Index])
end;

procedure TXCollection.setItems(Index: Integer;
  const Value: TXContainerItem);
begin
  TXContainerItem(Items[Index]).Assign(Value);
end;

procedure TXCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FParent) then
    FParent.DoChanged(frUpdate);
end;

{ TFillClass2 }

constructor TFillClass2.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TCanvas.Create;
  FItems := TXCollection.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
end;

destructor TFillClass2.Destroy;
begin
  FCanvas.Handle := 0;
  FCanvas.Free;
  FItems.Free;
  FImageChangeLink.Free;
  inherited;
end;

procedure TFillClass2.DrawItem(hDC: LongWord; var ARect: TRect;
  Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;
  FCanvas.Handle := hDC;
  FCanvas.FillRect(ARect);
  FCanvas.TextRect(ARect, ARect.Left + 18, ARect.Top + 2,
    (getItem(Index) as IFillerItemText).Caption);
  with (getItem(Index) as IFillerItemImages),
    (getItem(Index) as IFillerItemImage) do
  begin
    if Assigned(ImageList) and (ImageIndex >= 0) and (ImageIndex <
      ImageList.Count) then
    begin
      ImageList.Draw(FCanvas, ARect.Left, ARect.Top, ImageIndex);
    end;
  end;
end;

function TFillClass2.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFillClass2.getImageList: TImageList;
begin
  Result := FImages;
end;

function TFillClass2.getItem(Index: Integer): IFillerItem;
begin
  if (Index >= 0) and (Index < FItems.Count) then
    Result := FItems[Index].Intf
  else
    Result := nil;
end;

function TFillClass2.GetItems: TXCollection;
begin
  Result := FItems;
end;

function TFillClass2.getSupports: TJvFillerSupports;
begin
  Result :=
    [fsText, // supports IFillerItemText
  fsImages, // supports IFillerItemImages
  fsImageIndex, // supports IFillerItemImage
  //fsReadOnly, // does *not* support IFillerItemManagment
  fsCanRender // can render it's content to a DC
  //fsCanMeasure // can measure the size of it's content
  //fsSubItems // supports IFillerSubItems
  ];
end;

procedure TFillClass2.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    UpdateItems;
end;

function TFillClass2.MeasureItem(Index: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

procedure TFillClass2.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Images) and (Operation = opRemove) then
    Images := nil;
end;

procedure TFillClass2.setImageList(const Value: TImageList);
begin
  if FImages <> nil then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if FImages <> nil then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  UpdateItems;
end;

procedure TFillClass2.SetItems(const Value: TXCollection);
begin
  FItems.Assign(Value);
end;

procedure TFillClass2.UpdateItems;
begin
  DoChanged(frUpdate);
end;

end.

