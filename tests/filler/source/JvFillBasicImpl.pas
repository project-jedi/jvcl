{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFillBasicImpl.Pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Remko Bonte
  Peter Thörnqvist

Last Modified: 2003-04-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFillBasicImpl;

interface

uses
  Windows, Classes, SysUtils, Graphics, ImgList, Contnrs,
  JvComponent, JvFillIntf;

type
  // Forwards
  TExtensibleInterfacedPersistent = class;
  TAggregatedPersistentEx = class;
  TJvBaseFillerItem = class;
  TJvBaseFillerItems = class;

  // Class references
  TAggregatedPersistentExClass = class of TAggregatedPersistentEx;
  TJvFillerTextItemImplClass = class of TJvBaseFillerTextItemImpl;
  TJvBaseFillerItemClass = class of TJvBaseFillerItem;
  TJvFillerItemsClass = class of TJvBaseFillerItems;

  // Generic classes (move to some other unit?)
  TExtensibleInterfacedPersistent = class(TPersistent, IUnknown)
  private
    FAdditionalIntfImpl: TList;
  protected
    FRefCount: Integer;
    { IUnknown }
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    procedure AddIntfImpl(const Obj: TAggregatedPersistentEx);
    procedure RemoveIntfImpl(const Obj: TAggregatedPersistentEx);
    function IndexOfImplClass(const AClass: TAggregatedPersistentExClass): Integer;
    procedure ClearIntfImpl;
    procedure InitImplementers; virtual;
    procedure SuspendRefCount;
    procedure ResumeRefCount;

    function IsStreamableExtension(AnExtension: TAggregatedPersistentEx): Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadImplementers(Reader: TReader);
    procedure WriteImplementers(Writer: TWriter);
    procedure ReadImplementer(Reader: TReader);
    procedure WriteImplementer(Writer: TWriter; Instance: TAggregatedPersistentEx);
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  TAggregatedPersistent = class(TPersistent)
  private
    FController: Pointer;
    function GetController: IUnknown;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(Controller: IUnknown);
    property Controller: IUnknown read GetController;
  end;

  TAggregatedPersistentEx = class(TAggregatedPersistent)
  private
    FOwner: TExtensibleInterfacedPersistent;
  protected
    property Owner: TExtensibleInterfacedPersistent read FOwner;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
  end;

  // Item implementation classes
  TJvFillerItemAggregatedObject = class(TAggregatedPersistentEx)
  protected
    function Item: IFillerItem;
    function ItemImpl: TJvBaseFillerItem;
  end;

  TJvBaseFillerItem = class(TExtensibleInterfacedPersistent, IFillerItem)
  private
    FItems: Pointer;
    FItemsIntf: IFillerItems;
    FID: string;
  protected
    procedure InitID; virtual;
    procedure SetID(Value: string);
    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadSubItems(Reader: TReader);
    procedure WriteSubItems(Writer: TWriter);
    { IFillerItem }
    function GetItems: IFillerItems;
    function GetImplementer: TObject;
    function GetID: string;
    property Items: IFillerItems read GetItems;
    property Implementer: TObject read GetImplementer;
  public
    constructor Create(AItems: IFillerItems);
    procedure AfterConstruction; override;
  published
    property ID: string read GetID write SetID;
  end;

  TJvBaseFillerTextItemImpl = class(TJvFillerItemAggregatedObject, IFillerItemText)
  protected
    function GetCaption: string; virtual; abstract;
    procedure SetCaption(const Value: string); virtual; abstract;
  public
    property Caption: string read getCaption write setCaption;
  end;

  TJvBaseFillerImageItemImpl = class(TJvFillerItemAggregatedObject, IFillerItemImage)
  protected
    function GetAlignment: TAlignment; virtual; abstract;
    procedure SetAlignment(Value: TAlignment); virtual; abstract;
    function GetImageIndex: Integer; virtual; abstract;
    procedure SetImageIndex(Index: Integer); virtual; abstract;
    function GetSelectedIndex: Integer; virtual; abstract;
    procedure SetSelectedIndex(Value: Integer); virtual; abstract;
  end;

  // Items implementation classes
  TJvFillerItemsAggregatedObject = class(TAggregatedPersistentEx)
  protected
    function Items: IFillerItems;
    function ItemsImpl: TJvBaseFillerItems;
  end;

  TJvBaseFillerItems = class(TExtensibleInterfacedPersistent, IFillerItems, IFillerIDSearch)
  private
    FParent: Pointer;
    FParentIntf: IFillerItem;
    FFiller: IFiller;
    FSubAggregate: TAggregatedPersistentEx;
  protected
    procedure InternalAdd(Item: IFillerItem); virtual; abstract;
    function IsStreamableItem(Item: IFillerItem): Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadItems(Reader: TReader);
    procedure WriteItems(Writer: TWriter);
    procedure ReadItem(Reader: TReader);
    procedure WriteItem(Writer: TWriter; Item: IFillerItem);
    { IFillerItems }
    function GetCount: Integer; virtual; abstract;
    function GetItem(I: Integer): IFillerItem; virtual; abstract;
    function GetParent: IFillerItem;
    function GetFiller: IFiller;
    function GetImplementer: TObject;
    function Attributes: TJvFillerItemsAttributes; virtual;
    { IFillerIDSearch }
    function FindByID(ID: string; const Recursive: Boolean = False): IFillerItem;
  public
    constructor CreateFiller(const Filler: IFiller);
    constructor CreateParent(const Parent: IFillerItem);
    procedure BeforeDestruction; override;
  end;

  TJvBaseFillerItemsRenderer = class(TJvFillerItemsAggregatedObject, IFillerItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem;
      State: TOwnerDrawState; AOptions: TPersistent = nil); virtual; abstract;
    function DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem;
      AOptions: TPersistent = nil): TSize; virtual; abstract;
    { IFillerItemsRenderer}
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer;
      State: TOwnerDrawState; AOptions: TPersistent = nil); virtual;
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer;
      AOptions: TPersistent = nil): TSize; virtual;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem;
      State: TOwnerDrawState; AOptions: TPersistent = nil); virtual;
    function MeasureItem(ACanvas: TCanvas; Item: IFillerItem;
      AOptions: TPersistent = nil): TSize; virtual;
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize; virtual; abstract;
  end;

  TJvBaseFillerItemsManagment = class(TJvFillerItemsAggregatedObject, IFillerItemsManagment)
  protected
    { IFillerItemManagment }
    function Add(Item: IFillerItem): IFillerItem; virtual; abstract;
    function New: IFillerItem; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Remove(Item: IFillerItem); virtual; abstract;
  end;

  TJvBaseFillerItemsImagesImpl = class(TJvFillerItemsAggregatedObject, IFillerItemsImages)
  protected
    { IFillerItemImages }
    function GetImageList: TCustomImageList; virtual; abstract;
    procedure SetImageList(const Value: TCustomImageList); virtual; abstract;
  end;

  // Standard item implementers
  TJvFillerTextItemImpl = class(TJvBaseFillerTextItemImpl)
  private
    FCaption: string;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  published
    property Caption: string read getCaption write setCaption;
  end;

  TJvFillerImageItemImpl = class(TJvBaseFillerImageItemImpl)
  private
    FAlignment: TAlignment;
    FImageIndex: Integer;
    FSelectedIndex: Integer;
  protected
    function GetAlignment: TAlignment; override;
    procedure SetAlignment(Value: TAlignment); override;
    function GetImageIndex: Integer; override;
    procedure SetImageIndex(Index: Integer); override;
    function GetSelectedIndex: Integer; override;
    procedure SetSelectedIndex(Value: Integer); override;
  published
    property Alignment: TAlignment read getAlignment write SetAlignment default taLeftJustify;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default 0;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex default 0;
  end;

  TJvBaseFillerSubItems = class(TJvFillerItemAggregatedObject, IFillerItems)
  private
    FItems: IFillerItems;
  protected
    property Items: IFillerItems read FItems implements IFillerItems;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent; AItems: TJvBaseFillerItems); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

  // Standard items implementers
  TJvCustomFillerItemsTextRenderer = class(TJvBaseFillerItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem;
      State: TOwnerDrawState; AOptions: TPersistent = nil); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem;
      AOptions: TPersistent = nil): TSize; override;
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize; override;
  end;

  TJvFillerItemsList = class(TJvBaseFillerItems)
  private
    FList: TObjectList;
  protected
    procedure InternalAdd(Item: IFillerItem); override;
    function Attributes: TJvFillerItemsAttributes; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IFillerItem; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property List: TObjectList read FList;
  end;

  TJvBaseFillerItemsListManagment = class(TJvBaseFillerItemsManagment)
  protected
    function Add(Item: IFillerItem): IFillerItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(Item: IFillerItem); override;
  end;

  TJvCustomFillerItemsImages = class(TJvBaseFillerItemsImagesImpl)
  private
    FImageList: TCustomImageList;
  protected
    { IFillerItemImages }
    function GetImageList: TCustomImageList; override;
    procedure SetImageList(const Value: TCustomImageList); override;
  published
    property ImageList: TCustomImageList read getImageList write setImageList;
  end;

  // Generic filler implementation
  TJvCustomFiller = class(TJvComponent, {$IFNDEF COMPILER6_UP}IInterfaceComponentReference, {$ENDIF}
    IFiller, IFillerItems)
  private
    FFillerItemsImpl: TJvBaseFillerItems;
    FNotifiers: TInterfaceList;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure Changing(ChangeReason: TJvFillerChangeReason);
    procedure Changed(ChangeReason: TJvFillerChangeReason);
    class function PersistentFillerItems: Boolean; virtual;
    class function ItemsClass: TJvFillerItemsClass; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadRoot(Reader: TReader);
    procedure WriteRoot(Writer: TWriter);
    {$IFNDEF COMPILER6_UP}
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    {$ENDIF COMPILER6_UP}
    { IFiller }
    function GetSupports: TJvFillerSupports; virtual;
    function GetOptionClass: TJvFillerOptionsClass; virtual;
    function GetItems: IFillerItems; virtual;
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify); virtual;
    procedure UnregisterChangeNotify(AFillerNotify: IFillerNotify); virtual;

    property FillerItemsImpl: TJvBaseFillerItems read FFillerItemsImpl implements IFillerItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
  end;

// Rename and move to JvFunctions? Converts a buffer into a string of hex digits.
function HexBytes(const Buf; Length: Integer): string;

implementation

uses
  ActiveX,
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

type
  TOpenReader = class(TReader);
  TOpenWriter = class(TWriter);
  
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

procedure TJvCustomFillerItemsTextRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Item: IFillerItem; State: TOwnerDrawState; AOptions: TPersistent);
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

function TJvCustomFillerItemsTextRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IFillerItem;
  AOptions: TPersistent): TSize;
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

function TJvCustomFillerItemsTextRenderer.AvgItemSize(ACanvas: TCanvas;
  AOptions: TPersistent): TSize;
begin
  Result := ACanvas.TextExtent('WyWyWyWyWyWyWyWyWyWy');
end;

{ TJvFillerTextItem }

function TJvFillerTextItemImpl.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TJvFillerTextItemImpl.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    Item.Items.Filler.Changing(frUpdate);
    FCaption := Value;
    Item.Items.Filler.Changed(frUpdate);
  end;
end;

{ TJvFillerImageItem }

function TJvFillerImageItemImpl.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TJvFillerImageItemImpl.SetAlignment(Value: TAlignment);
begin
  if GetAlignment <> Value then
  begin
    Item.Items.Filler.Changing(frUpdate);
    FAlignment := Value;
    Item.Items.Filler.Changed(frUpdate);
  end;
end;

function TJvFillerImageItemImpl.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TJvFillerImageItemImpl.SetImageIndex(Index: Integer);
begin
  if GetImageIndex <> Index then
  begin
    Item.Items.Filler.Changing(frUpdate);
    FImageIndex := Index;
    Item.Items.Filler.Changed(frUpdate);
  end;
end;

function TJvFillerImageItemImpl.GetSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

procedure TJvFillerImageItemImpl.SetSelectedIndex(Value: Integer);
begin
  if GetSelectedIndex <> Value then
  begin
    Item.Items.Filler.Changing(frUpdate);
    FSelectedIndex := Value;
    Item.Items.Filler.Changed(frUpdate);
  end;
end;

{ TExtensibleInterfacedPersistent }

function TExtensibleInterfacedPersistent._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TExtensibleInterfacedPersistent._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TExtensibleInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TExtensibleInterfacedPersistent.AddIntfImpl(const Obj: TAggregatedPersistentEx);
begin
  if IndexOfImplClass(TAggregatedPersistentExClass(Obj.ClassType)) >= 0 then
    raise EJVCLException.Create('Implementation of that class already exists.');
  FAdditionalIntfImpl.Add(Obj);
end;

procedure TExtensibleInterfacedPersistent.RemoveIntfImpl(const Obj: TAggregatedPersistentEx);
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

function TExtensibleInterfacedPersistent.IndexOfImplClass(const AClass: TAggregatedPersistentExClass): Integer;
begin
  Result := FAdditionalIntfImpl.Count - 1;
  while (Result >= 0) and not (TObject(FAdditionalIntfImpl[Result]) is AClass) do
    Dec(Result);
end;

procedure TExtensibleInterfacedPersistent.ClearIntfImpl;
var
  I: Integer;
begin
  for I := FAdditionalIntfImpl.Count - 1 downto 0 do
    TObject(FAdditionalIntfImpl[I]).Free;
  FAdditionalIntfImpl.Clear;
end;

procedure TExtensibleInterfacedPersistent.InitImplementers;
begin
end;

procedure TExtensibleInterfacedPersistent.SuspendRefCount;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TExtensibleInterfacedPersistent.ResumeRefCount;
begin
  InterlockedDecrement(FRefCount);
end;

function TExtensibleInterfacedPersistent.IsStreamableExtension(AnExtension: TAggregatedPersistentEx): Boolean;
begin
  Result := GetClass(AnExtension.ClassName) <> nil;
end;

procedure TExtensibleInterfacedPersistent.DefineProperties(Filer: TFiler);
var
  I: Integer;
begin
  inherited DefineProperties(Filer);
  I := FAdditionalIntfImpl.Count - 1;
  while (I >= 0) and not IsStreamableExtension(TAggregatedPersistentEx(FAdditionalIntfImpl[I])) do
    Dec(I);
  Filer.DefineProperty('Implementers', ReadImplementers, WriteImplementers, I >= 0);
end;

procedure TExtensibleInterfacedPersistent.ReadImplementers(Reader: TReader);
begin
  { When loading implementers the interface of this object maybe referenced. We don't want the
    instance destroyed yet, so reference counting will be suspended (by incrementing it) and resumed
    when we're done (by decrementing it without checking if it became zero) }
  SuspendRefCount;
  try
    if Reader.ReadValue <> vaCollection then
      raise EReadError.Create('Expected collection.');
    while not Reader.EndOfList do
      ReadImplementer(Reader);
    Reader.ReadListEnd;
  finally
    ResumeRefCount;
  end;
end;

procedure TExtensibleInterfacedPersistent.WriteImplementers(Writer: TWriter);
var
  I: Integer;
begin
  TOpenWriter(Writer).WriteValue(vaCollection);
  for I := 0 to FAdditionalIntfImpl.Count - 1 do
    if IsStreamableExtension(TAggregatedPersistentEx(FAdditionalIntfImpl[I])) then
      WriteImplementer(Writer, TAggregatedPersistentEx(FAdditionalIntfImpl[I]));
  Writer.WriteListEnd;
end;

procedure TExtensibleInterfacedPersistent.ReadImplementer(Reader: TReader);
var
  ClassName: string;
  ClassType: TPersistentClass;
  I: Integer;
  Impl: TAggregatedPersistentEx;
begin
  Reader.ReadListBegin;
  ClassName := Reader.ReadStr;
  if not AnsiSameText(ClassName, 'ClassName') then
    raise EReadError.Create('Missing ClassName property');
  ClassName := Reader.ReadString;
  ClassType := FindClass(ClassName);
  if not ClassType.InheritsFrom(TAggregatedPersistentEx) then
    raise EReadError.Create('Invalid class type');
  I := IndexOfImplClass(TAggregatedPersistentExClass(ClassType));
  if I >= 0 then
    Impl := TAggregatedPersistentEx(FAdditionalIntfImpl[I])
  else
    Impl := TAggregatedPersistentExClass(ClassType).Create(Self);
  while not Reader.EndOfList do
    TOpenReader(Reader).ReadProperty(Impl);
  Reader.ReadListEnd;
end;

procedure TExtensibleInterfacedPersistent.WriteImplementer(Writer: TWriter;
  Instance: TAggregatedPersistentEx);
begin
  Writer.WriteListBegin;
  TOpenWriter(Writer).WritePropName('ClassName');
  Writer.WriteString(Instance.ClassName);
  TOpenWriter(Writer).WriteProperties(Instance);
  Writer.WriteListEnd;
end;

constructor TExtensibleInterfacedPersistent.Create;
begin
  inherited Create;
  FAdditionalIntfImpl := TList.Create;
end;

procedure TExtensibleInterfacedPersistent.AfterConstruction;
begin
  inherited AfterConstruction;
  InitImplementers;
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TExtensibleInterfacedPersistent.BeforeDestruction;
begin
  if RefCount <> 0 then RunError(2);
  inherited BeforeDestruction;
  ClearIntfImpl;
  FAdditionalIntfImpl.Free;
end;

function TExtensibleInterfacedPersistent.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  I: Integer;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
  begin
    I := FAdditionalIntfImpl.Count - 1;
    while (I >= 0) and ((FAdditionalIntfImpl[I] = nil) or
        not TAggregatedPersistentEx(FAdditionalIntfImpl[I]).GetInterface(IID, Obj)) do
      Dec(I);
    Result := I >= 0;
  end;
end;

class function TExtensibleInterfacedPersistent.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  // set a refcount to avoid destruction due to refcounting during construction
  TExtensibleInterfacedPersistent(Result).FRefCount := 1;
end;

{ TAggregatedPersistent }

function TAggregatedPersistent.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TAggregatedPersistent.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := Controller.QueryInterface(IID, Obj);
end;

function TAggregatedPersistent._AddRef: Integer;
begin
  Result := Controller._AddRef;
end;

function TAggregatedPersistent._Release: Integer;
begin
  Result := Controller._Release;
end;

constructor TAggregatedPersistent.Create(Controller: IUnknown);
begin
  inherited Create;
  FController := Pointer(Controller);
end;

{ TAggregatedPersistentEx }

constructor TAggregatedPersistentEx.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
end;

procedure TAggregatedPersistentEx.AfterConstruction;
begin
  inherited AfterConstruction;
  FOwner.AddIntfImpl(Self);
end;

procedure TAggregatedPersistentEx.BeforeDestruction;
var
  I: Integer;
begin
  inherited BeforeDestruction;
  I := FOwner.FAdditionalIntfImpl.IndexOf(Self);
  if I >= 0 then
    FOwner.FAdditionalIntfImpl.Delete(I);
end;

function TAggregatedPersistentEx.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
end;

{ TJvBaseFillerItems }

function TJvBaseFillerItems.IsStreamableItem(Item: IFillerItem): Boolean;
var
  AClass: TPersistentClass;
begin
  AClass := GetClass(Item.GetImplementer.ClassName);
  Result := (AClass <> nil) and AClass.InheritsFrom(TJvBaseFillerItem);
end;

procedure TJvBaseFillerItems.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, WriteItems, True);
end;

procedure TJvBaseFillerItems.ReadItems(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.Create('Expected collection.');
  while not Reader.EndOfList do
    ReadItem(Reader);
  Reader.ReadListEnd;
end;

procedure TJvBaseFillerItems.WriteItems(Writer: TWriter);
var
  I: Integer;
begin
  TOpenWriter(Writer).WriteValue(vaCollection);
  for I := 0 to getCount - 1 do
  begin
    if IsStreamableItem(getItem(I)) then
      WriteItem(Writer, getItem(I));
  end;
  Writer.WriteListEnd;
end;

procedure TJvBaseFillerItems.ReadItem(Reader: TReader);
var
  PropName: string;
  ClassName: string;
  PerstClass: TPersistentClass;
  ItemClass: TJvBaseFillerItemClass;
  ItemInstance: TJvBaseFillerItem;
begin
  Reader.ReadListBegin;
  PropName := Reader.ReadStr;
  if not AnsiSameText(PropName, 'ClassName') then
    raise EReadError.Create('Missing property ClassName.');
  ClassName := Reader.ReadString;
  PerstClass := FindClass(ClassName);
  if not PerstClass.InheritsFrom(TJvBaseFillerItem) then
    raise EReadError.Create('Invalid item class.');
  ItemClass := TJvBaseFillerItemClass(PerstClass);
  ItemInstance := ItemClass.Create(Self);
  try
    InternalAdd(ItemInstance);
  except
    ItemInstance.Free;
    raise;
  end;
  while not Reader.EndOfList do
    TOpenReader(Reader).ReadProperty(ItemInstance);
  Reader.ReadListEnd;
end;

procedure TJvBaseFillerItems.WriteItem(Writer: TWriter; Item: IFillerItem);
var
  Inst: TPersistent;
begin
  Writer.WriteListBegin;
  Inst := TPersistent(Item.GetImplementer);
  Writer.WriteStr('ClassName');
  Writer.WriteString(Inst.ClassName);
  TOpenWriter(Writer).WriteProperties(Inst);
  Writer.WriteListEnd;
end;

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
  if (Parent <> nil) and (Parent.GetImplementer is TExtensibleInterfacedPersistent) then
    FSubAggregate := TJvBaseFillerSubItems.Create(
      TExtensibleInterfacedPersistent(Parent.GetImplementer), Self);
end;

procedure TJvBaseFillerItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FSubAggregate <> nil then
    FreeAndNil(FSubAggregate);
end;

{ TJvBaseFillerSubItems }

constructor TJvBaseFillerSubItems.Create(AOwner: TExtensibleInterfacedPersistent;
  AItems: TJvBaseFillerItems);
begin
  inherited Create(AOwner);
  FItems := AItems;
end;

destructor TJvBaseFillerSubItems.Destroy;
begin
  inherited Destroy;
end;

procedure TJvBaseFillerSubItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FItems.GetImplementer is TJvBaseFillerItems then
    TJvBaseFillerItems(FItems.GetImplementer).FSubAggregate := nil;
end;

function TJvBaseFillerSubItems.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or Succeeded(FItems.QueryInterface(IID, Obj));
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

procedure TJvBaseFillerItemsRenderer.DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; State: TOwnerDrawState; AOptions: TPersistent);
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise EJVCLException.CreateFmt('Index out of range (%d)', [Index]);
  DrawItem(ACanvas, ARect, Items.Items[Index], State, AOptions);
end;

function TJvBaseFillerItemsRenderer.MeasureItemByIndex(ACanvas:TCanvas; Index: Integer;
  AOptions: TPersistent): TSize;
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

procedure TJvBaseFillerItemsRenderer.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem;
  State: TOwnerDrawState; AOptions: TPersistent);
var
  ImgRender: IFillerItemRenderer;
begin
  if Supports(Item, IFillerItemRenderer, ImgRender) then
    ImgRender.Draw(ACanvas, ARect, State, AOptions)
  else
    DoDrawItem(ACanvas, ARect, Item, State, AOptions);
end;

function TJvBaseFillerItemsRenderer.MeasureItem(ACanvas: TCanvas; Item: IFillerItem;
  AOptions: TPersistent): TSize;
var
  ImgRender: IFillerItemRenderer;
begin
  if Supports(Item, IFillerItemRenderer, ImgRender) then
    Result := ImgRender.Measure(ACanvas, AOptions)
  else
    Result := DoMeasureItem(ACanvas, Item, AOptions);
end;

{ TJvFillerItemsList }

procedure TJvFillerItemsList.InternalAdd(Item: IFillerItem);
begin
  List.Add(Item.GetImplementer);
end;

function TJvFillerItemsList.Attributes: TJvFillerItemsAttributes;
begin
  Result := [];
end;

function TJvFillerItemsList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TJvFillerItemsList.GetItem(I: Integer): IFillerItem;
begin
  Result := (List[I] as TJvBaseFillerItem) as IFillerItem;
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
  Items.Filler.Changing(frAdd);
  TJvFillerItemsList(ItemsImpl).List.Add(Item.GetImplementer);
  Result := Item;
  Items.Filler.Changed(frAdd);
end;

procedure TJvBaseFillerItemsListManagment.Clear;
begin
  Items.Filler.Changing(frUpdate);
  TJvFillerItemsList(ItemsImpl).List.Clear;
  Items.Filler.Changed(frUpdate);
end;

procedure TJvBaseFillerItemsListManagment.Delete(Index: Integer);
begin
  Items.Filler.Changing(frDelete);
  TJvFillerItemsList(ItemsImpl).List.Delete(Index);
  Items.Filler.Changed(frDelete);
end;

procedure TJvBaseFillerItemsListManagment.Remove(Item: IFillerItem);
var
  Impl: TObject;
begin
  Items.Filler.Changing(frDelete);
  Impl := Item.GetImplementer;
  if (Impl is TExtensibleInterfacedPersistent) and
      (TExtensibleInterfacedPersistent(Impl).RefCount = 0) then
    Pointer(Item) := nil;
  TJvFillerItemsList(ItemsImpl).List.Remove(Impl);
  Items.Filler.Changed(frDelete);
end;

{ TJvCustomFillerItemsImages }

function TJvCustomFillerItemsImages.GetImageList: TCustomImageList;
begin
  Result := FImageList;
end;

procedure TJvCustomFillerItemsImages.SetImageList(const Value: TCustomImageList);
begin
  if Value <> GetImageList then
  begin
    (Owner as IFillerItems).Filler.Changing(frUpdate);
    FImageList := Value;
    (Owner as IFillerItems).Filler.Changed(frUpdate);
  end;
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

procedure TJvBaseFillerItem.DefineProperties(Filer: TFiler);
var
  Tmp: IFillerItems;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SubItems', ReadSubItems, WriteSubItems,
    Supports(Self as IFillerItem, IFillerItems, Tmp));
end;

procedure TJvBaseFillerItem.ReadSubItems(Reader: TReader);
var
  PropName: string;
  ClassName: string;
  AClass: TPersistentClass;
  I: Integer;
begin
  { When loading sub items the interface of this object may be referenced. We don't want the
    instance destroyed yet, so reference counting will be suspended (by incrementing it) and resumed
    when we're done (by decrementing it without checking if it became zero) }
  SuspendRefCount;
  try
    if Reader.ReadValue <> vaCollection then
      raise EReadError.Create('Expected collection.');
    Reader.ReadListBegin;
    PropName := Reader.ReadStr;
    if not AnsiSameText(PropName, 'ClassName') then
      raise EReadError.Create('Expected ClassName property.');
    ClassName := Reader.ReadString;
    AClass := FindClass(ClassName);
    if not AClass.InheritsFrom(TJvBaseFillerItems) then
      raise EReadError.Create('Invalid sub items implementer class.');
    I := IndexOfImplClass(TJvBaseFillerSubItems);
    if I > -1 then
    begin
      if TJvBaseFillerSubItems(FAdditionalIntfImpl[I]).Items.GetImplementer.ClassType <> AClass then
      begin
        FAdditionalIntfImpl.Delete(I);
        I := -1;
      end;
    end;
    if I = -1 then
    begin
      TJvFillerItemsClass(AClass).CreateParent(Self);
      I := IndexOfImplClass(TJvBaseFillerSubItems);
    end;
    while not Reader.EndOfList do
      TOpenReader(Reader).ReadProperty(
        TJvBaseFillerItems(TJvBaseFillerSubItems(FAdditionalIntfImpl[I]).Items.GetImplementer));
    Reader.ReadListEnd;
  finally
    ResumeRefCount;
  end;
end;

procedure TJvBaseFillerItem.WriteSubItems(Writer: TWriter);
var
  Items: IFillerItems;
begin
  QueryInterface(IFillerItems, Items);
  TOpenWriter(Writer).WriteValue(vaCollection);
  Writer.WriteListBegin;
  Writer.WriteStr('ClassName');
  Writer.WriteString(Items.GetImplementer.ClassName);
  TOpenWriter(Writer).WriteProperties(Items.GetImplementer as TPersistent);
  Writer.WriteListEnd;
  Writer.WriteListEnd;
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

procedure TJvCustomFiller.Changing(ChangeReason: TJvFillerChangeReason);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IFillerNotify).FillerChanging(Self, ChangeReason);
end;

procedure TJvCustomFiller.Changed(ChangeReason: TJvFillerChangeReason);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IFillerNotify).FillerChanged(Self, ChangeReason);
end;

class function TJvCustomFiller.PersistentFillerItems: Boolean;
begin
  Result := False;
end;

class function TJvCustomFiller.ItemsClass: TJvFillerItemsClass;
begin
  Result := TJvFillerItemsList;
end;

procedure TJvCustomFiller.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  if PersistentFillerItems then
    Filer.DefineProperty('Root', ReadRoot, WriteRoot, True);
end;

procedure TJvCustomFiller.ReadRoot(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.Create('Expected collection.');
  Reader.ReadListBegin;
  // We don''t really have a root item; just stream in the FillerItemsImpl instance.
  while not Reader.EndOfList do
    TOpenReader(Reader).ReadProperty(FillerItemsImpl);
  Reader.ReadListEnd;
  Reader.ReadListEnd;
end;

procedure TJvCustomFiller.WriteRoot(Writer: TWriter);
begin
  TOpenWriter(Writer).WriteValue(vaCollection);
  Writer.WriteListBegin;
  // We don''t really have a root item; just stream out the FillerItemsImpl instance.
  TOpenWriter(Writer).WriteProperties(FillerItemsImpl);
  Writer.WriteListEnd;
  Writer.WriteListEnd;
end;

{$IFNDEF COMPILER6_UP}
function TJvCustomFiller.GetComponent: TComponent;
begin
  Result := Self;
end;
{$ENDIF COMPILER6_UP}

function TJvCustomFiller.GetSupports: TJvFillerSupports;
begin
  Result := [];
end;

function TJvCustomFiller.GetOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;

function TJvCustomFiller.GetItems: IFillerItems;
begin
  Result := FillerItemsImpl;
end;

procedure TJvCustomFiller.RegisterChangeNotify(AFillerNotify: IFillerNotify);
begin
  if FNotifiers.IndexOf(AFillerNotify) < 0 then
    FNotifiers.Add(AFillerNotify);
end;

procedure TJvCustomFiller.UnregisterChangeNotify(AFillerNotify: IFillerNotify);
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
  FFillerItemsImpl._Release;
  inherited Destroy;
end;

procedure TJvCustomFiller.BeforeDestruction;
begin
  if (FFillerItemsImpl <> nil) and (FFillerItemsImpl.RefCount > 1) then
    RunError(2);
  inherited BeforeDestruction;
  Changing(frDestroy);
end;

function TJvCustomFiller.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or (FFillerItemsImpl.GetInterface(IID, Obj));
end;

initialization
  RegisterClasses([TJvBaseFillerItem, TJvCustomFillerItemsTextRenderer, TJvFillerTextItemImpl,
    TJvFillerImageItemImpl, TJvFillerItemsList, TJvBaseFillerItemsListManagment,
    TJvCustomFillerItemsImages]);
end.
