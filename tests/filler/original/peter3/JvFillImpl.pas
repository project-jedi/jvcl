{
  (peter@peter3.com):
 * test implementation of IFillerItemText and IFiller
 * uses collections and disables refcounting by returning -1 from _AddRef and _Release
 * no aggregation or delegation implemented
}
unit JvFillImpl;

interface
uses
  Windows, Controls, Types, Classes, SysUtils, Graphics, ImgList, JvFillIntf5;

type
  // internal update notification interface for implementors of IFiller that want to be notified
  // about changes and updates in the TCollectionItems associated with it
  IItemUpdateNotify = interface
    ['{D85279CD-21D2-4237-AD59-716356727F7B}']
    procedure Notify(Sender: TObject; ABeforeChange: boolean; AReason: TJvFillerChangeReason);
  end;

  TJvFillerItem = class(TCollectionItem, IUnknown, IFillerItem)
  private
    FUpdateNotify: IItemUpdateNotify;
  protected
    procedure UpdateNotify(ABeforeChange: boolean; AReason: TJvFillerChangeReason);

    { IUnknown}
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    // makes it easier to get the interface
    function Intf: IFillerItem; virtual;
    property Notify: IItemUpdateNotify read FUpdateNotify write FUpdateNotify;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TJvFillerItems = class(TOwnedCollection)
  private
    FNotify: IItemUpdateNotify;
  protected
    procedure Update(Item: TCollectionItem); override;
    property UpdateNotify: IItemUpdateNotify read FNotify;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
    destructor Destroy; override;
  end;

  TJvTextFillerItem = class(TJvFillerItem, IFillerItemText)
  private
    FCaption: string;
    {IFillerItemText }
    function getCaption: string;
    procedure setCaption(const Value: string);
  public
  published
    property Caption: string read getCaption write setCaption;
  end;

  TJvTextFillerItems = class(TJvFillerItems)
  private
    function getItems(Index: integer): TJvTextFillerItem;
    procedure setItems(Index: integer; const Value: TJvTextFillerItem);
  public
    constructor Create(AOwner: TComponent);
    property Items[Index: integer]: TJvTextFillerItem read getItems write setItems; default;
    function Add: TJvTextFillerItem;
  end;

  TJvImageFillerItem = class(TJvTextFillerItem, IFillerItemImage)
  private
    FAlignment: TAlignment;
    FImageIndex, FSelectedIndex: integer;
    function getAlignment: TAlignment;
    function getImageIndex: Integer;
    function getSelectedIndex: Integer;
    procedure setAlignment(Value: TAlignment);
    procedure setImageIndex(Value: Integer);
    procedure setSelectedIndex(Value: Integer);
  published
    property Alignment: TAlignment read getAlignment write setAlignment;
    property ImageIndex: Integer read getImageIndex write setImageIndex;
    property SelectedIndex: Integer read getSelectedIndex write setSelectedIndex;
  end;

  TJvImageFillerItems = class(TJvFillerItems)
  private
    function getItems(Index: integer): TJvImageFillerItem;
    procedure setItems(Index: integer; const Value: TJvImageFillerItem);
  public
    constructor Create(AOwner:TComponent);
    property Items[Index: integer]: TJvImageFillerItem read getItems write setItems;default;
    function Add: TJvImageFillerItem;
  end;

  TJvBaseFiller = class(TComponent, IBaseFiller,IItemUpdateNotify)
  private
    FNotifyList: TInterfaceList;
  protected
    { IBaseFiller }
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify); virtual;
    procedure UnRegisterChangeNotify(AFillerNotify: IFillerNotify); virtual;
    { IItemUpdateNotify }
    procedure Notify(Sender: TObject; ABeforeChange: Boolean;
      AReason: TJvFillerChangeReason);virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
  end;

  TJvCustomFiller = class(TJvBaseFiller, IFiller)
  protected
    { IFiller }
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState); virtual;
    function MeasureItem(ACanvas:TCanvas; Index: Integer): TSize; virtual;
    function getCount: Integer; virtual;
    function getItem(Index: Integer): IFillerItem; virtual;
    function getSupports: TJvFillerSupports; virtual;
  end;

  TJvTextFiller = class(TJvCustomFiller, IFillerItemManagment)
  private
    FItems: TJvTextFillerItems;
    function getItems: TJvTextFillerItems;
    procedure setItems(const Value: TJvTextFillerItems);

    { IFillerItemManagment }
    function FillerAdd: IFillerItem;
    procedure FillerClear;
    procedure FillerDelete(Index: integer);
    procedure FillerRemove(Item: IFillerItem);
    function IFillerItemManagment.Add = FillerAdd;
    procedure IFillerItemManagment.Clear = FillerClear;
    procedure IFillerItemManagment.Delete = FillerDelete;
    procedure IFillerItemManagment.Remove = FillerRemove;
  protected
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: Integer;
      State: TOwnerDrawState);override;
    function getCount: Integer;override;
    function getItem(Index: Integer): IFillerItem;override;
    function getSupports: TJvFillerSupports;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TJvTextFillerItems read getItems write setItems;
  end;

  TJvImageFiller = class(TJvCustomFiller, IFillerItemManagment,IFillerItemImages)
  private
    FChangeLink:TChangeLink;
    FItems: TJvImageFillerItems;
    function getItems: TJvImageFillerItems;
    procedure setItems(const Value: TJvImageFillerItems);

    { IFillerItemManagment }
    function FillerAdd: IFillerItem;
    procedure FillerClear;
    procedure FillerDelete(Index: integer);
    procedure FillerRemove(Item: IFillerItem);
    function IFillerItemManagment.Add = FillerAdd;
    procedure IFillerItemManagment.Clear = FillerClear;
    procedure IFillerItemManagment.Delete = FillerDelete;
    procedure IFillerItemManagment.Remove = FillerRemove;
  private
    FImageList:TImageList;
    function getImageList: TImageList;
    procedure setImageList(const Value: TImageList);
    procedure DoChange(Sender:TObject);
  protected
    procedure DrawItem(ACanvas:TCanvas; var ARect: TRect; Index: Integer;
      State: TOwnerDrawState);override;
    function getCount: Integer;override;
    function getItem(Index: Integer): IFillerItem;override;
    function getSupports: TJvFillerSupports;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;
    function MeasureItem(ACanvas:TCanvas; Index: Integer): tagSIZE;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TJvImageFillerItems read getItems write setItems;
    property ImageList:TImageList read getImageList write setImageList;
  end;


implementation

{ TJvFillerItem }

function TJvFillerItem.QueryInterface(const IID: TGUID;
  out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TJvFillerItem._AddRef: Integer;
begin
  Result := -1; // no ref counting
end;

function TJvFillerItem._Release: Integer;
begin
  Result := -1; // no ref counting
end;

function TJvFillerItem.Intf: IFillerItem;
begin
  Result := self;
end;

procedure TJvFillerItem.UpdateNotify(ABeforeChange: boolean;
  AReason: TJvFillerChangeReason);
begin
  if FUpdateNotify <> nil then
    FUpdateNotify.Notify(Self, ABeforeChange, AReason);
end;

constructor TJvFillerItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TJvFillerItems then
    FUpdateNotify := TJvFillerItems(Collection).UpdateNotify;
  UpdateNotify(false, frAdd);
end;

destructor TJvFillerItem.Destroy;
begin
  UpdateNotify(true, frDelete);
  inherited;
end;

{ TJvFillerItems }

constructor TJvFillerItems.Create(AOwner: TPersistent; ItemClass: TCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
  // this assumes Supports sets FNotify to nil if it doesn't support the requested interface:
  Supports(AOwner, IItemUpdateNotify, FNotify);
end;

destructor TJvFillerItems.Destroy;
begin
  FNotify := nil;
  inherited;
end;

procedure TJvFillerItems.Update(Item: TCollectionItem);
begin
  inherited;
  if FNotify <> nil then
    FNotify.Notify(self, true, frUpdate);
end;

{ TJvTextFillerItem }

function TJvTextFillerItem.getCaption: string;
begin
  Result := FCaption;
end;

procedure TJvTextFillerItem.setCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    UpdateNotify(false, frUpdate);
  end;
end;

{ TJvTextFillerItems }

function TJvTextFillerItems.Add: TJvTextFillerItem;
begin
  Result := TJvTextFillerItem(inherited Add);
end;

constructor TJvTextFillerItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TJvTextFillerItem);
end;

function TJvTextFillerItems.getItems(Index: integer): TJvTextFillerItem;
begin
  Result := TJvTextFillerItem(inherited Items[Index])
end;

procedure TJvTextFillerItems.setItems(Index: integer;
  const Value: TJvTextFillerItem);
begin
  TJvTextFillerItem(Items[Index]).Assign(Value);
end;

{ TJvImageFillerItem }

function TJvImageFillerItem.getAlignment: TAlignment;
begin
  Result := FAlignment;
end;

function TJvImageFillerItem.getImageIndex: Integer;
begin
  Result := FImageIndex;
end;

function TJvImageFillerItem.getSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

procedure TJvImageFillerItem.setAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    UpdateNotify(false, frUpdate);
  end;
end;

procedure TJvImageFillerItem.setImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    UpdateNotify(false, frUpdate);
  end;
end;

procedure TJvImageFillerItem.setSelectedIndex(Value: Integer);
begin
  if FSelectedIndex <> Value then
  begin
    FSelectedIndex := Value;
    UpdateNotify(false, frUpdate);
  end;
end;

{ TJvImageFillerItems }

function TJvImageFillerItems.Add: TJvImageFillerItem;
begin
  Result := TJvImageFillerItem(inherited Add);
end;

constructor TJvImageFillerItems.Create(AOwner: TComponent);
begin
  inherited Create(AOwner,TJvImageFillerItem);
end;

function TJvImageFillerItems.getItems(Index: integer): TJvImageFillerItem;
begin
  Result := TJvImageFillerItem(inherited Items[Index]);
end;

procedure TJvImageFillerItems.setItems(Index: integer;
  const Value: TJvImageFillerItem);
begin
  inherited Items[Index] := Value;
end;

{ TJvTextFiller }

constructor TJvTextFiller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvTextFillerItems.Create(self);
end;

destructor TJvTextFiller.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TJvTextFiller.DrawItem(ACanvas:TCanvas; var ARect: TRect;
  Index: Integer; State: TOwnerDrawState);
begin
  // notice that the component doesn't alter the properties of the hDC:
  // this should be done by the consumer before calling DrawItem
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;
  ACanvas.FillRect(ARect);
  ACanvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, FItems[Index].Caption);
end;

function TJvTextFiller.FillerAdd: IFillerItem;
begin
  Result := FItems.Add.Intf;
end;

procedure TJvTextFiller.FillerClear;
begin
  Fitems.Clear;
end;

procedure TJvTextFiller.FillerDelete(Index: integer);
begin
  FItems.Delete(Index);
end;

procedure TJvTextFiller.FillerRemove(Item: IFillerItem);
var i:integer;
begin
  for i := 0 to FItems.Count - 1 do
    if Pointer(FItems[i].Intf) = Pointer(Item) then
    begin
      FItems.Delete(i);
      Break;
    end;
end;

function TJvTextFiller.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvTextFiller.getItem(Index: Integer): IFillerItem;
begin
  if (Index >= 0) and (Index < FItems.Count) then
    Result := FItems[Index].Intf
  else
    Result := nil;
end;

function TJvTextFiller.getItems: TJvTextFillerItems;
begin
  Result := FItems;
end;

function TJvTextFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsCanRender];
end;

procedure TJvTextFiller.setItems(const Value: TJvTextFillerItems);
begin
  FItems.Assign(Value);
end;

{ TJvCustomFiller }

procedure TJvCustomFiller.DrawItem(ACanvas:TCanvas; var ARect: TRect;
  Index: Integer; State: TOwnerDrawState);
begin
  // do nothing
end;

function TJvCustomFiller.getCount: Integer;
begin
  Result := 0;
end;

function TJvCustomFiller.getItem(Index: Integer): IFillerItem;
begin
  Result := nil;
end;

function TJvCustomFiller.getSupports: TJvFillerSupports;
begin
  Result := [];
end;

function TJvCustomFiller.MeasureItem(ACanvas:TCanvas; Index: Integer): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;


{ TJvImageFiller }

constructor TJvImageFiller.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChange;
  FItems := TJvImageFillerItems.Create(self);
end;

destructor TJvImageFiller.Destroy;
begin
  FChangeLink.Free;
  FItems.Free;
  inherited;
end;

procedure TJvImageFiller.DoChange(Sender: TObject);
begin
  Notify(self,false,frUpdate);  
end;

procedure TJvImageFiller.DrawItem(ACanvas:TCanvas; var ARect: TRect;
  Index: Integer; State: TOwnerDrawState);
var R:TRect;F:IFillerItemImage;F2:IFillerItemText;FFlags:Cardinal;
begin
  if (Index < 0) or (Index >= FItems.Count) then Exit;
  R := ARect;
  FFlags := 0;
  ACanvas.FillRect(ARect);
  if Supports(FItems[Index],IFillerItemImage,F) and Assigned(FImageList) then
  begin
    case F.Alignment of
      taLeftJustify:
      begin
        FFlags := DT_LEFT;
        FImageList.Draw(ACanvas,R.Left + 2,R.Top + 2,F.ImageIndex,dsTransparent,itImage);
        R.Left := R.Left + FImageList.Width + 4;
      end;
      taRightJustify:
      begin
        FFlags := DT_RIGHT;
        FImageList.Draw(ACanvas,R.Right - FImageList.Width - 2,R.Top + 2,F.ImageIndex,dsTransparent,itImage);
        R.Right := R.Right - FImageList.Width - 4;
      end;
      taCenter:
      begin
        FFlags := DT_CENTER;
        // how to handle this?
      end;
    end;
  end;
  SetBkMode(ACanvas.Handle,Windows.TRANSPARENT);
  if Supports(FItems[Index],IFillerItemText,F2) then
  begin
    FFlags := FFlags or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX;
    DrawText(ACanvas.Handle,PChar(F2.Caption),-1,R,FFlags);
  end;
end;

function TJvImageFiller.FillerAdd: IFillerItem;
begin
  Result := FItems.Add.Intf;
end;

procedure TJvImageFiller.FillerClear;
begin
  FItems.Clear;
end;

procedure TJvImageFiller.FillerDelete(Index: integer);
begin
  FItems.Delete(Index);
end;

procedure TJvImageFiller.FillerRemove(Item: IFillerItem);
var i:integer;
begin
  for i := 0 to FItems.Count - 1 do
    if Pointer(FItems[i].Intf) = Pointer(Item) then
    begin
      FItems.Delete(i);
      Break;
    end;
end;

function TJvImageFiller.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvImageFiller.getImageList: TImageList;
begin
  Result := FImageList;
end;

function TJvImageFiller.getItem(Index: Integer): IFillerItem;
begin
  Result := Items[Index].Intf;
end;

function TJvImageFiller.getItems: TJvImageFillerItems;
begin
  Result := FItems;
end;

function TJvImageFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText,fsCanRender,fsImages,fsImageIndex];
end;

function TJvImageFiller.MeasureItem(ACanvas:TCanvas;
  Index: Integer): tagSIZE;
begin
  Result.cx := 0;
  if Assigned(FImageList) then
    Result.cy := FImageList.Height + 4
  else
    Result.cy := ACanvas.TextHeight('Wq');
end;

procedure TJvImageFiller.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FImageList) and (Operation = opRemove) then
    ImageList := nil;
end;

procedure TJvImageFiller.setImageList(const Value: TImageList);
begin
  if FImageList <> Value then
  begin
    if FImageList <> nil then
      FImageList.UnRegisterChanges(FChangeLink);
    FImageList := Value;
    if FImageList <> nil then
      FImageList.RegisterChanges(FChangeLink);
    Notify(self,false,frUpdate);
  end;
end;

procedure TJvImageFiller.setItems(const Value: TJvImageFillerItems);
begin
  FItems.Assign(Value);
end;

{ TJvBaseFiller }

constructor TJvBaseFiller.Create(AOwner: TComponent);
begin
  inherited;
  FNotifyList := TInterfaceList.Create;
end;

destructor TJvBaseFiller.Destroy;
begin
  Notify(self, true, frDestroy);
  FNotifyList.Clear;
  FNotifyList.Free;
  inherited;
end;

procedure TJvBaseFiller.Notify(Sender: TObject; ABeforeChange: Boolean;
  AReason: TJvFillerChangeReason);
var i: integer; F: IFillerNotify;
begin
  for i := 0 to FNotifyList.Count - 1 do
    if Supports(FNotifyList[i], IFillerNotify, F) then
    begin
      if ABeforeChange then
        F.FillerChanging(self as IFiller, AReason)
      else
        F.FillerChanged(self as IFiller, AReason);
    end;
end;

procedure TJvBaseFiller.RegisterChangeNotify(
  AFillerNotify: IFillerNotify);
begin
  FNotifyList.Add(AFillerNotify);
end;

procedure TJvBaseFiller.UnRegisterChangeNotify(
  AFillerNotify: IFillerNotify);
begin
  FNotifyList.Remove(AFillerNotify);
end;

end.

