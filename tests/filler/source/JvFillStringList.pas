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
    class function PersistentFillerItems: Boolean; override;
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

  TJvStringsFillerItemText = class(TJvBaseFillerTextItemImpl)
  private
    FIndex: Integer;
    FStrings: TStrings;
  protected
    function getCaption: string; override;
    procedure setCaption(const Value: string); override;
  end;

  TJvStringsFillerItem = class(TJvBaseFillerItem)
  protected
    Impl: TJvStringsFillerItemText;
    procedure InitID; override;
  public
    constructor Create(AItems: IFillerItems; AStringList: TStrings; AIndex: Integer);
  end;

{ TJvStringsFillerItems }

procedure TJvStringsFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvCustomFillerItemsTextRenderer.Create(Self);
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
  SetID(IntToHex(Impl.FIndex, 4));
end;

constructor TJvStringsFillerItem.Create(AItems: IFillerItems; AStringList: TStrings; AIndex: Integer);
begin
  inherited Create(AItems);
  Impl := TJvStringsFillerItemText.Create(Self);
  Impl.FIndex := AIndex;
  Impl.FStrings := AStringList;
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
  Changing(frUpdate);
  (FillerItemsImpl as TJvStringsFillerItems).FItems.Assign(Value);
  Changed(frUpdate);
end;

type
  TJvTreeFillerItems = class(TJvFillerItemsList)
  protected
    procedure InitImplementers; override;
  public
    procedure BeforeDestruction; override;
  end;

  TJvTreeFillerItemsDesigner = class(TJvFillerItemsAggregatedObject, IFillerItemsDesigner)
  protected
    function getCount: Integer;
    function getKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IFillerItem;
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
  published
    property ImageList: TCustomImageList read getImageList write setImageList;
  end;

  TJvTreeFillerItem = class(TJvBaseFillerItem)
  protected
    procedure InitImplementers; override;
  public
    procedure BeforeDestruction; override;
  end;

{ TJvTreeFillerItems }

procedure TJvTreeFillerItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvTreeFillerItemsManagment.Create(Self);
  TJvTreeFillerItemsDesigner.Create(Self);
  TJvTreeFillerImages.Create(Self);
end;

procedure TJvTreeFillerItems.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeFillerItemsDesigner }

function TJvTreeFillerItemsDesigner.getCount: Integer;
begin
  Result := 4;
end;

function TJvTreeFillerItemsDesigner.getKind(Index: Integer; out Caption: string): Boolean;
begin
  Result := True;
  case Index of
    0: Caption := 'Text only';
    1: Caption := 'Text and sub items';
    2: Caption := 'Text and image';
    3: Caption := 'Text, image and sub items';
    else Result := False;
  end;
end;

function TJvTreeFillerItemsDesigner.NewByKind(Kind: Integer): IFillerItem;
var
  Man: IFillerItemManagment;
begin
  if not Supports(Items, IFillerItemManagment, Man) then
    raise EJVCLException.Create('IFillerItemManagment interface is not supported.');
  case Kind of
    0:
      begin
        Result := Man.Add(TJvTreeFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
      end;
    1:
      begin
        Result := Man.Add(TJvTreeFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvTreeFillerItems.CreateParent(Result);
      end;
    2:
      begin
        Result := Man.Add(TJvTreeFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvFillerImageItemImpl.Create((Result.Implementer as TJvBaseFillerItem));
      end;
    3:
      begin
        Result := Man.Add(TJvTreeFillerItem.Create(Items));
        TJvFillerTextItemImpl.Create(Result.Implementer as TJvBaseFillerItem);
        TJvFillerImageItemImpl.Create((Result.Implementer as TJvBaseFillerItem));
        TJvTreeFillerItems.CreateParent(Result);
      end;
    else raise EJVCLException.Create('Invalid item type requested.');
  end;
end;

{ TJvTreeFillerItemsManagment }

function TJvTreeFillerItemsManagment.New: IFillerItem;
begin
  Result := TJvTreeFillerItem.Create(Items);
  Add(Result);
end;

{ TJvTreeFillerImages }

function TJvTreeFillerImages.getImageList: TCustomImageList;
begin
  Result := FImageList;
end;

procedure TJvTreeFillerImages.setImageList(const Value: TCustomImageList);
begin
  if Value <> getImageList then
  begin
    (Owner as IFillerItems).Filler.Changing(frUpdate);
    FImageList := Value;
    (Owner as IFillerItems).Filler.Changed(frUpdate);
  end;
end;

{ TJvTreeFillerItem }

procedure TJvTreeFillerItem.InitImplementers;
begin
  inherited InitImplementers;
//  TJvTreeFillerItems.CreateParent(Self);
//  TJvFillerTextItemImpl.Create(Self);
end;

procedure TJvTreeFillerItem.BeforeDestruction;
begin
  inherited;
end;

{ TJvTreeFiller }

class function TJvTreeFiller.PersistentFillerItems: Boolean;
begin
  Result := True;
end;

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

initialization
  RegisterClasses([TJvTreeFillerItems, TJvTreeFillerItemsDesigner, TJvTreeFillerItemsManagment,
    TJvTreeFillerImages, TJvTreeFillerItem]);
end.
