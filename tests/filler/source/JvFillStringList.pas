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

  TJvTreeFillerTree = (TreeFillerTree);

  { Supports text, imagelists, imageindex and subitems }
  TJvTreeFiller = class(TJvCustomFiller)
  private
    FItems: TJvTreeFillerTree;
  protected
    class function ItemsClass: TJvFillerItemsClass; override;
    function getSupports: TJvFillerSupports; override;
    function getOptionClass: TJvFillerOptionsClass; override;
  public
    procedure BeforeDestruction; override;
  published
    property Items: TJvTreeFillerTree read FItems write FItems stored False;
  end;

implementation

uses
  ImgList, Graphics,
  JvTypes;

type
  TJvStringsFillerItems = class(TJvBaseFillerItems)
  private
    FItems: TStrings;
  protected
    procedure InitImplementers; override;
    function getCount: Integer; override;
    function getItem(I: Integer): IFillerItem; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TJvStringsFillerItem = class(TJvFillerTextItem)
  protected
    procedure InitID; override;
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

procedure TJvStringsFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  AddIntfImpl(TJvCustomFillerItemsTextRenderer.Create(Self));
end;

function TJvStringsFillerItems.getCount: Integer;
begin
  Result := FItems.Count;
end;

function TJvStringsFillerItems.getItem(I: Integer): IFillerItem;
begin
  Result := TJvStringsFillerItem.Create(Self, FItems, I);
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

procedure TJvStringsFillerItem.InitID; 
begin
  SetID(IntToHex(TJvStringsFillerItemText(TextImpl).FIndex, 4));
end;

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
  if not (fsReadonly in Item.Items.Filler.getSupports) then
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

type
  TJvTreeFillerItems = class(TJvFillerItemsList)
  protected
    procedure InitImplementers; override;
  public
    procedure BeforeDestruction; override;
  end;

  TJvTreeFillerItemsManagment = class(TJvBaseFillerItemsListManagment)
  protected
    function New: IFillerItem; override;
  end;

  TJvTreeFillerImages = class(TJvBaseFillerItemImagesImpl)
  private
    FImageList: TCustomImageList;
  protected
    { IFillerItemImages }
    function getImageList: TCustomImageList; override;
    procedure setImageList(const Value: TCustomImageList); override;
  end;

  TJvTreeFillerItem = class(TJvBaseFillerItem, IFillerItems)
  private
    FSubItems: IFillerItems;
  protected
    procedure InitImplementers; override;
    property SubItems: IFillerItems read FSubItems implements IFillerItems;
  public
    procedure BeforeDestruction; override;
  end;

{ TJvTreeFillerItems }

procedure TJvTreeFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  AddIntfImpl(TJvTreeFillerItemsManagment.Create(Self));
  AddIntfImpl(TJvTreeFillerImages.Create(Self));
end;

procedure TJvTreeFillerItems.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeFillerItemsManagment }

function TJvTreeFillerItemsManagment.New: IFillerItem;
begin
  Result := TJvTreeFillerItem.Create(Items);
  Add(Result);
end;

{ TJvTreeFillerImages }

function TJvTreeFillerImages.getImageList: TCustomImageList;
var
  CurItems: IFillerItems;
  Img: IFillerItemImages;
begin
  Result := FImageList;
  if FImageList = nil then
  begin
    CurItems := Items;
    while (CurItems.Parent <> nil) do
    begin
      CurItems := CurItems.Parent.Items;
      if Supports(CurItems, IFillerItemImages, Img) then
      begin
        if (Img.ImageList <> nil) then
          Result := Img.ImageList;
        Break; 
      end;
    end;
  end;
end;

procedure TJvTreeFillerImages.setImageList(const Value: TCustomImageList);
begin
  if Value <> getImageList then
  begin
    FImageList := Value;
    (Owner as IFillerItems).Filler.NotifyConsumers(frUpdate);
  end;
end;

{ TJvTreeFillerItem }

procedure TJvTreeFillerItem.InitImplementers;
begin
  inherited InitImplementers;
  FSubItems := TJvTreeFillerItems.CreateParent(Self);
  AddIntfImpl(TJvFillerTextItemImpl.Create(Self));
  AddIntfImpl(TJvFillerImageItemImpl.Create(Self));
end;

procedure TJvTreeFillerItem.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeFiller }

class function TJvTreeFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvTreeFillerItems;
end;

function TJvTreeFiller.getSupports: TJvFillerSupports;
begin
  Result := [fsText, fsImages, fsImageIndex, fsCanRender, fsCanMeasure, fsSubItems];
end;

function TJvTreeFiller.getOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

procedure TJvTreeFiller.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

end.
