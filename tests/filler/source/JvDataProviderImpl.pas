{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderImpl.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Remko Bonte
  Peter Thörnqvist

Last Modified: 2003-06-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderImpl;

interface

uses
  Windows, Classes, SysUtils, Graphics, ImgList, Contnrs,
  JvComponent, JvDataProvider;

type
  // Forwards
  TExtensibleInterfacedPersistent = class;
  TAggregatedPersistentEx = class;
  TJvBaseDataItem = class;
  TJvBaseDataItems = class;

  // Class references
  TAggregatedPersistentExClass = class of TAggregatedPersistentEx;
  TJvDataItemTextImplClass = class of TJvBaseDataItemTextImpl;
  TJvBaseDataItemClass = class of TJvBaseDataItem;
  TJvDataItemsClass = class of TJvBaseDataItems;

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
  TJvDataItemAggregatedObject = class(TAggregatedPersistentEx)
  protected
    function Item: IJvDataItem;
    function ItemImpl: TJvBaseDataItem;
  end;

  TJvBaseDataItem = class(TExtensibleInterfacedPersistent, IJvDataItem)
  private
    FItems: Pointer;
    FItemsIntf: IJvDataItems;
    FID: string;
  protected
    { Initialize ID. Each item must have an unique identification. Implementers may choose how this
      ID is generated. No checks are made when items are added to a provider to ensure it's
      unique. If multiple items with the same ID are added only the first item in the tree will be
      selectable at design time. }
    procedure InitID; virtual;
    { Set the ID string. Used by InitID to set the actual ID string. }
    procedure SetID(Value: string);
    { Reference counting: add 1 if this item is part of a dynamic list (Items.IsDynamic returns
      True). Otherwise reference counting is not used. }
    function _AddRef: Integer; override; stdcall;
    { Reference counting: substract 1 if this item is part of a dynamic list (Items.IsDynamic returns
      True). Otherwise reference counting is not used. }
    function _Release: Integer; override; stdcall;

    { Streaming of an item. }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadSubItems(Reader: TReader);
    procedure WriteSubItems(Writer: TWriter);
    { IJvDataItem methods and properties. }
    function GetItems: IJvDataItems;
    function GetImplementer: TObject;
    function GetID: string;
    property Items: IJvDataItems read GetItems;
    property Implementer: TObject read GetImplementer;
  public
    constructor Create(AOwner: IJvDataItems);
    procedure AfterConstruction; override;
  published
    property ID: string read GetID write SetID;
  end;

  TJvBaseDataItemTextImpl = class(TJvDataItemAggregatedObject, IJvDataItemText)
  protected
    function GetCaption: string; virtual; abstract;
    procedure SetCaption(const Value: string); virtual; abstract;
  public
    property Caption: string read GetCaption write SetCaption;
  end;

  TJvBaseDataItemImageImpl = class(TJvDataItemAggregatedObject, IJvDataItemImage)
  protected
    function GetAlignment: TAlignment; virtual; abstract;
    procedure SetAlignment(Value: TAlignment); virtual; abstract;
    function GetImageIndex: Integer; virtual; abstract;
    procedure SetImageIndex(Index: Integer); virtual; abstract;
    function GetSelectedIndex: Integer; virtual; abstract;
    procedure SetSelectedIndex(Value: Integer); virtual; abstract;
  end;

  // Items implementation classes
  TJvDataItemsAggregatedObject = class(TAggregatedPersistentEx)
  protected
    function Items: IJvDataItems;
    function ItemsImpl: TJvBaseDataItems;
  end;

  TJvBaseDataItems = class(TExtensibleInterfacedPersistent, IJvDataItems, IJvDataIDSearch)
  private
    FParent: Pointer;
    FParentIntf: IJvDataItem;
    FProvider: IJvDataProvider;
    FSubAggregate: TAggregatedPersistentEx;
  protected
    { Adds an item to the list. Called by the various add methods of the IJvDataItemsManagement and
      IJvDataItemsDesigner. }
    procedure InternalAdd(Item: IJvDataItem); virtual; abstract;
    { Determines if the item is streamable. }
    function IsStreamableItem(Item: IJvdataItem): Boolean; virtual;
    { Streaming methods }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadItems(Reader: TReader);
    procedure WriteItems(Writer: TWriter);
    procedure ReadItem(Reader: TReader);
    procedure WriteItem(Writer: TWriter; Item: IJvDataItem);
    { IJvDataItems methods }
    function GetCount: Integer; virtual; abstract;
    function GetItem(I: Integer): IJvDataItem; virtual; abstract;
    function GetParent: IJvDataItem;
    function GetProvider: IJvDataProvider;
    function GetImplementer: TObject;
    function IsDynamic: Boolean; virtual;
    { IJvDataIDSearch methods }
    function FindByID(ID: string; const Recursive: Boolean = False): IJvDataItem;
  public
    constructor CreateProvider(const Provider: IJvDataProvider);
    constructor CreateParent(const Parent: IJvDataItem);
    procedure BeforeDestruction; override;
  end;

  TJvBaseDataItemsRenderer = class(TJvDataItemsAggregatedObject, IJvDataItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TOwnerDrawState); virtual; abstract;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; virtual; abstract;
    { IJvDataItemsRenderer methods }
    procedure DrawItemByIndex(ACanvas:TCanvas; var ARect: TRect; Index: Integer;
      State: TOwnerDrawState); virtual;
    function MeasureItemByIndex(ACanvas:TCanvas; Index: Integer): TSize; virtual;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TOwnerDrawState); virtual;
    function MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; virtual;
    function AvgItemSize(ACanvas: TCanvas): TSize; virtual; abstract;
  end;

  TJvBaseDataItemsManagement = class(TJvDataItemsAggregatedObject, IJvDataItemsManagement)
  protected
    { IJvDataItemManagement methods }
    function Add(Item: IJvDataItem): IJvDataItem; virtual; abstract;
    function New: IJvDataItem; virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure Remove(Item: IJvDataItem); virtual; abstract;
  end;

  TJvBaseDataItemsImagesImpl = class(TJvDataItemsAggregatedObject, IJvDataItemsImages)
  protected
    { IJvDataItemImages methods }
    function GetImageList: TCustomImageList; virtual; abstract;
    procedure SetImageList(const Value: TCustomImageList); virtual; abstract;
  end;

  // Standard item implementers
  TJvDataItemTextImpl = class(TJvBaseDataItemTextImpl)
  private
    FCaption: string;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  published
    property Caption: string read GetCaption write SetCaption;
  end;

  TJvDataItemImageImpl = class(TJvBaseDataItemImageImpl)
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

  TJvBaseDataItemSubItems = class(TJvDataItemAggregatedObject, IJvDataItems)
  private
    FItems: IJvDataItems;
  protected
    property Items: IJvDataItems read FItems implements IJvDataItems;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent; AItems: TJvBaseDataItems); virtual;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

  // Standard items implementers
  TJvCustomDataItemsTextRenderer = class(TJvBaseDataItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TOwnerDrawState); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; override;
    function AvgItemSize(ACanvas: TCanvas): TSize; override;
  end;

  TJvDataItemsList = class(TJvBaseDataItems)
  private
    FList: TObjectList;
  protected
    procedure InternalAdd(Item: IJvDataItem); override;
    function IsDynamic: Boolean; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
  public
    { TODO: Rewrite as constructor/destructor }
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property List: TObjectList read FList;
  end;

  TJvBaseDataItemsListManagement = class(TJvBaseDataItemsManagement)
  protected
    function Add(Item: IJvDataItem): IJvDataItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(Item: IJvDataItem); override;
  end;

  TJvCustomDataItemsImages = class(TJvBaseDataItemsImagesImpl)
  private
    FImageList: TCustomImageList;
  protected
    function GetImageList: TCustomImageList; override;
    procedure SetImageList(const Value: TCustomImageList); override;
  published
    property ImageList: TCustomImageList read GetImageList write SetImageList;
  end;

  // Generic data provider implementation
  TJvCustomDataProvider = class(TJvComponent, {$IFNDEF COMPILER6_UP}IInterfaceComponentReference, {$ENDIF}
    IJvDataProvider, IJvDataItems)
  private
    FDataItemsImpl: TJvBaseDataItems;
    FNotifiers: TInterfaceList;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    procedure Changing(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    procedure Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    class function PersistentDataItems: Boolean; virtual;
    class function ItemsClass: TJvDataItemsClass; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadRoot(Reader: TReader);
    procedure WriteRoot(Writer: TWriter);
    {$IFNDEF COMPILER6_UP}
    { IInterfaceComponentReference }
    function GetComponent: TComponent;
    {$ENDIF COMPILER6_UP}
    { IDataProvider }
    function GetItems: IJvDataItems; virtual;
    procedure RegisterChangeNotify(ANotify: IJvDataProviderNotify); virtual;
    procedure UnregisterChangeNotify(ANotify: IJvDataProviderNotify); virtual;

    property DataItemsImpl: TJvBaseDataItems read FDataItemsImpl implements IJvDataItems;
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
  ActiveX, Consts,
  JvDataProviderConsts, JvTypes;

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
  
{ TJvDataItemAggregatedObject }

function TJvDataItemAggregatedObject.Item: IJvDataItem;
begin
  Result := Owner as IJvDataItem;
end;

function TJvDataItemAggregatedObject.ItemImpl: TJvBaseDataItem;
begin
  Result := Owner as TJvBaseDataItem;
end;

{ TJvCustomDataItemsTextRenderer }

procedure TJvCustomDataItemsTextRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Item: IJvDataItem; State: TOwnerDrawState);
var
  TextIntf: IJvDataItemText;
  S: string;
begin
  if Supports(Item, IJvDataItemText, TextIntf) then
    S := TextIntf.Caption
  else
    S := SDataItemRenderHasNoText;
  ACanvas.TextRect(ARect, ARect.Left, ARect.Top, S);
end;

function TJvCustomDataItemsTextRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
var
  TextIntf: IJvDataItemText;
  S: string;
begin
  if Supports(Item, IJvDataItemText, TextIntf) then
    S := TextIntf.Caption
  else
    S := SDataItemRenderHasNoText;
  Result := ACanvas.TextExtent(S);
end;

function TJvCustomDataItemsTextRenderer.AvgItemSize(ACanvas: TCanvas): TSize;
begin
  Result := ACanvas.TextExtent('WyWyWyWyWyWyWyWyWyWy');
end;

{ TJvDataItemTextImpl }

function TJvDataItemTextImpl.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TJvDataItemTextImpl.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    Item.Items.Provider.Changing(pcrUpdate, Item);
    FCaption := Value;
    Item.Items.Provider.Changed(pcrUpdate, Item);
  end;
end;

{ TJvDataItemImageImpl }

function TJvDataItemImageImpl.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TJvDataItemImageImpl.SetAlignment(Value: TAlignment);
begin
  if GetAlignment <> Value then
  begin
    Item.Items.Provider.Changing(pcrUpdate, Item);
    FAlignment := Value;
    Item.Items.Provider.Changed(pcrUpdate, Item);
  end;
end;

function TJvDataItemImageImpl.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TJvDataItemImageImpl.SetImageIndex(Index: Integer);
begin
  if GetImageIndex <> Index then
  begin
    Item.Items.Provider.Changing(pcrUpdate, Item);
    FImageIndex := Index;
    Item.Items.Provider.Changed(pcrUpdate, Item);
  end;
end;

function TJvDataItemImageImpl.GetSelectedIndex: Integer;
begin
  Result := FSelectedIndex;
end;

procedure TJvDataItemImageImpl.SetSelectedIndex(Value: Integer);
begin
  if GetSelectedIndex <> Value then
  begin
    Item.Items.Provider.Changing(pcrUpdate, Item);
    FSelectedIndex := Value;
    Item.Items.Provider.Changed(pcrUpdate, Item);
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
    raise EJVCLException.Create(SExtensibleIntObjDuplicateClass);
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
      raise EReadError.Create(SExtensibleIntObjCollectionExpected);
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
    raise EReadError.Create(SExtensibleIntObjClassNameExpected);
  ClassName := Reader.ReadString;
  ClassType := FindClass(ClassName);
  if not ClassType.InheritsFrom(TAggregatedPersistentEx) then
    raise EReadError.Create(SExtensibleIntObjInvalidClass);
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

{ TJvBaseDataItems }

function TJvBaseDataItems.IsStreamableItem(Item: IJvDataItem): Boolean;
var
  AClass: TPersistentClass;
begin
  AClass := GetClass(Item.GetImplementer.ClassName);
  Result := (AClass <> nil) and AClass.InheritsFrom(TJvBaseDataItem);
end;

procedure TJvBaseDataItems.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, WriteItems, True);
end;

procedure TJvBaseDataItems.ReadItems(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.Create(SExtensibleIntObjCollectionExpected);
  while not Reader.EndOfList do
    ReadItem(Reader);
  Reader.ReadListEnd;
end;

procedure TJvBaseDataItems.WriteItems(Writer: TWriter);
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

procedure TJvBaseDataItems.ReadItem(Reader: TReader);
var
  PropName: string;
  ClassName: string;
  PerstClass: TPersistentClass;
  ItemClass: TJvBaseDataItemClass;
  ItemInstance: TJvBaseDataItem;
begin
  Reader.ReadListBegin;
  PropName := Reader.ReadStr;
  if not AnsiSameText(PropName, 'ClassName') then
    raise EReadError.Create(SExtensibleIntObjClassNameExpected);
  ClassName := Reader.ReadString;
  PerstClass := FindClass(ClassName);
  if not PerstClass.InheritsFrom(TJvBaseDataItem) then
    raise EReadError.Create(SExtensibleIntObjInvalidClass);
  ItemClass := TJvBaseDataItemClass(PerstClass);
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

procedure TJvBaseDataItems.WriteItem(Writer: TWriter; Item: IJvDataItem);
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

function TJvBaseDataItems.GetParent: IJvDataItem;
begin
  Result := IJvDataItem(FParent);
end;

function TJvBaseDataItems.GetProvider: IJvDataProvider;
begin
  Result := FProvider;
end;

function TJvBaseDataItems.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseDataItems.IsDynamic: Boolean;
begin
  Result := True;
end;

function TJvBaseDataItems.FindByID(ID: string; const Recursive: Boolean): IJvDataItem;
var
  I: Integer;
  SubItems: IJvDataItems;
  Search: IJvDataIDSearch;
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
        if Supports(GetItem(I), IJvDataItems, SubItems) then
        begin
          if Supports(SubItems, IJvDataIDSearch, Search) then
            Result := Search.FindByID(ID, Recursive);
        end;
        Dec(I);
      end;
    end;
  end;
end;

constructor TJvBaseDataItems.CreateProvider(const Provider: IJvDataProvider);
begin
  Create;
  FProvider := Provider;
end;

constructor TJvBaseDataItems.CreateParent(const Parent: IJvDataItem);
begin
  CreateProvider(Parent.Items.Provider);
  FParent := Pointer(Parent);
  if (Parent <> nil) and Parent.Items.IsDynamic then
    FParentIntf := Parent;
  if (Parent <> nil) and (Parent.GetImplementer is TExtensibleInterfacedPersistent) then
    FSubAggregate := TJvBaseDataItemSubItems.Create(
      TExtensibleInterfacedPersistent(Parent.GetImplementer), Self);
end;

procedure TJvBaseDataItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FSubAggregate <> nil then
    FreeAndNil(FSubAggregate);
end;

{ TJvBaseDataItemSubItems }

constructor TJvBaseDataItemSubItems.Create(AOwner: TExtensibleInterfacedPersistent;
  AItems: TJvBaseDataItems);
begin
  inherited Create(AOwner);
  FItems := AItems;
end;

destructor TJvBaseDataItemSubItems.Destroy;
begin
  inherited Destroy;
end;

procedure TJvBaseDataItemSubItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FItems.GetImplementer is TJvBaseDataItems then
    TJvBaseDataItems(FItems.GetImplementer).FSubAggregate := nil;
end;

function TJvBaseDataItemSubItems.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or Succeeded(FItems.QueryInterface(IID, Obj));
end;

{ TJvDataItemsAggregatedObject }

function TJvDataItemsAggregatedObject.Items: IJvDataItems;
begin
  Result := Owner as IJvDataItems;
end;

function TJvDataItemsAggregatedObject.ItemsImpl: TJvBaseDataItems;
begin
  Result := Owner as TJvBaseDataItems;
end;

{ TJvBaseDataItemsRenderer }

procedure TJvBaseDataItemsRenderer.DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; State: TOwnerDrawState);
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise EJVCLException.CreateFmt(SListIndexError, [Index]);
  DrawItem(ACanvas, ARect, Items.Items[Index], State);
end;

function TJvBaseDataItemsRenderer.MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
begin
  if Index = -1 then
    Result := AvgItemSize(ACanvas)
  else
  begin
    if (Index < 0) or (Index >= Items.Count) then
      raise EJVCLException.CreateFmt(SListIndexError, [Index]);
    Result := MeasureItem(ACanvas, Items.Items[Index]);
  end;
end;

procedure TJvBaseDataItemsRenderer.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
  State: TOwnerDrawState);
var
  ImgRender: IJvDataItemRenderer;
begin
  if Supports(Item, IJvDataItemRenderer, ImgRender) then
    ImgRender.Draw(ACanvas, ARect, State)
  else
    DoDrawItem(ACanvas, ARect, Item, State);
end;

function TJvBaseDataItemsRenderer.MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
var
  ImgRender: IJvDataItemRenderer;
begin
  if Supports(Item, IJvDataItemRenderer, ImgRender) then
    Result := ImgRender.Measure(ACanvas)
  else
    Result := DoMeasureItem(ACanvas, Item);
end;

{ TJvDataItemsList }

procedure TJvDataItemsList.InternalAdd(Item: IJvDataItem);
begin
  List.Add(Item.GetImplementer);
end;

function TJvDataItemsList.IsDynamic: Boolean;
begin
  Result := False;
end;

function TJvDataItemsList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TJvDataItemsList.GetItem(I: Integer): IJvDataItem;
begin
  Result := (List[I] as TJvBaseDataItem) as IJvDataItem;
end;

procedure TJvDataItemsList.AfterConstruction;
begin
  inherited AfterConstruction;
  FList := TObjectList.Create;
end;

procedure TJvDataItemsList.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FList.Free;
end;

{ TJvBaseDataItemsListManagement }

function TJvBaseDataItemsListManagement.Add(Item: IJvDataItem): IJvDataItem;
begin
  Items.Provider.Changing(pcrAdd, Items);
  TJvDataItemsList(ItemsImpl).List.Add(Item.GetImplementer);
  Result := Item;
  Items.Provider.Changed(pcrAdd, Result);
end;

procedure TJvBaseDataItemsListManagement.Clear;
begin
  Items.Provider.Changing(pcrUpdate, Items);
  TJvDataItemsList(ItemsImpl).List.Clear;
  Items.Provider.Changed(pcrUpdate, Items);
end;

procedure TJvBaseDataItemsListManagement.Delete(Index: Integer);
begin
  Items.Provider.Changing(pcrDelete, Items.GetItem(Index));
  TJvDataItemsList(ItemsImpl).List.Delete(Index);
  Items.Provider.Changed(pcrDelete, nil);
end;

procedure TJvBaseDataItemsListManagement.Remove(Item: IJvDataItem);
var
  Impl: TObject;
begin
  Items.Provider.Changing(pcrDelete, Item);
  Impl := Item.GetImplementer;
  if (Impl is TExtensibleInterfacedPersistent) and
      (TExtensibleInterfacedPersistent(Impl).RefCount = 0) then
    Pointer(Item) := nil;
  TJvDataItemsList(ItemsImpl).List.Remove(Impl);
  Items.Provider.Changed(pcrDelete, nil);
end;

{ TJvCustomDataItemsImages }

function TJvCustomDataItemsImages.GetImageList: TCustomImageList;
begin
  Result := FImageList;
end;

procedure TJvCustomDataItemsImages.SetImageList(const Value: TCustomImageList);
begin
  if Value <> GetImageList then
  begin
    (Owner as IJvDataItems).Provider.Changing(pcrUpdate, Items);
    FImageList := Value;
    (Owner as IJvDataItems).Provider.Changed(pcrUpdate, Items);
  end;
end;

{ TJvBaseDataItem }

procedure TJvBaseDataItem.InitID;
var
  G: TGUID;
begin
  CoCreateGuid(G);
  FID := HexBytes(G, SizeOf(G));
end;

procedure TJvBaseDataItem.SetID(Value: string);
begin
  FID := Value;
end;

function TJvBaseDataItem._AddRef: Integer;
begin
  if Items.IsDynamic then
    Result := inherited _AddRef
  else
    Result := -1;
end;

function TJvBaseDataItem._Release: Integer;
begin
  if Items.IsDynamic then
    Result := inherited _Release
  else
    Result := -1;
end;

procedure TJvBaseDataItem.DefineProperties(Filer: TFiler);
var
  Tmp: IJvDataItems;
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SubItems', ReadSubItems, WriteSubItems,
    Supports(Self as IJvDataItem, IJvDataItems, Tmp));
end;

procedure TJvBaseDataItem.ReadSubItems(Reader: TReader);
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
      raise EReadError.Create(SExtensibleIntObjCollectionExpected);
    Reader.ReadListBegin;
    PropName := Reader.ReadStr;
    if not AnsiSameText(PropName, 'ClassName') then
      raise EReadError.Create(SExtensibleIntObjClassNameExpected);
    ClassName := Reader.ReadString;
    AClass := FindClass(ClassName);
    if not AClass.InheritsFrom(TJvBaseDataItems) then
      raise EReadError.Create(SExtensibleIntObjInvalidClass);
    I := IndexOfImplClass(TJvBaseDataItemSubItems);
    if I > -1 then
    begin
      if TJvBaseDataItemSubItems(FAdditionalIntfImpl[I]).Items.GetImplementer.ClassType <> AClass then
      begin
        FAdditionalIntfImpl.Delete(I);
        I := -1;
      end;
    end;
    if I = -1 then
    begin
      TJvDataItemsClass(AClass).CreateParent(Self);
      I := IndexOfImplClass(TJvBaseDataItemSubItems);
    end;
    while not Reader.EndOfList do
      TOpenReader(Reader).ReadProperty(
        TJvBaseDataItems(TJvBaseDataItemSubItems(FAdditionalIntfImpl[I]).Items.GetImplementer));
    Reader.ReadListEnd;
  finally
    ResumeRefCount;
  end;
end;

procedure TJvBaseDataItem.WriteSubItems(Writer: TWriter);
var
  Items: IJvDataItems;
begin
  QueryInterface(IJvDataItems, Items);
  TOpenWriter(Writer).WriteValue(vaCollection);
  Writer.WriteListBegin;
  Writer.WriteStr('ClassName');
  Writer.WriteString(Items.GetImplementer.ClassName);
  TOpenWriter(Writer).WriteProperties(Items.GetImplementer as TPersistent);
  Writer.WriteListEnd;
  Writer.WriteListEnd;
end;

function TJvBaseDataItem.GetItems: IJvDataItems;
begin
  Result := IJvDataItems(FItems);
end;

function TJvBaseDataItem.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseDataItem.GetID: string;
begin
  Result := FID;
end;

constructor TJvBaseDataItem.Create(AOwner: IJvDataItems);
begin
  inherited Create;
  FItems := Pointer(AOwner);
  // Dynamically generated items will need a hard reference to the IJvDataItems owner.
  if AOwner.IsDynamic then
    FItemsIntf := AOwner;
end;

procedure TJvBaseDataItem.AfterConstruction;
begin
  InitID;
  inherited AfterConstruction;
end;

{ TJvCustomDataProvider }

function TJvCustomDataProvider.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TJvCustomDataProvider.Changing(ChangeReason: TDataProviderChangeReason; Source: IUnknown);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IJvDataProviderNotify).DataProviderChanging(Self, ChangeReason, Source);
end;

procedure TJvCustomDataProvider.Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown);
var
  I: Integer;
begin
  for I := 0 to FNotifiers.Count - 1 do
    (FNotifiers[I] as IJvDataProviderNotify).DataProviderChanged(Self, ChangeReason, Source);
end;

class function TJvCustomDataProvider.PersistentDataItems: Boolean;
begin
  Result := False;
end;

class function TJvCustomDataProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvDataItemsList;
end;

procedure TJvCustomDataProvider.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  if PersistentDataItems then
    Filer.DefineProperty('Root', ReadRoot, WriteRoot, True);
end;

procedure TJvCustomDataProvider.ReadRoot(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.Create(SExtensibleIntObjCollectionExpected);
  Reader.ReadListBegin;
  // We don''t really have a root item; just stream in the DataItemsImpl instance.
  while not Reader.EndOfList do
    TOpenReader(Reader).ReadProperty(DataItemsImpl);
  Reader.ReadListEnd;
  Reader.ReadListEnd;
end;

procedure TJvCustomDataProvider.WriteRoot(Writer: TWriter);
begin
  TOpenWriter(Writer).WriteValue(vaCollection);
  Writer.WriteListBegin;
  // We don''t really have a root item; just stream out the DataItemsImpl instance.
  TOpenWriter(Writer).WriteProperties(DataItemsImpl);
  Writer.WriteListEnd;
  Writer.WriteListEnd;
end;

{$IFNDEF COMPILER6_UP}
function TJvCustomDataProvider.GetComponent: TComponent;
begin
  Result := Self;
end;
{$ENDIF COMPILER6_UP}

(*function TJvCustomFiller.GetSupports: TJvFillerSupports;
begin
  Result := [];
end;

function TJvCustomFiller.GetOptionClass: TJvFillerOptionsClass;
begin
  Result := nil;
end;
*)
function TJvCustomDataProvider.GetItems: IJvDataItems;
begin
  Result := DataItemsImpl;
end;

procedure TJvCustomDataProvider.RegisterChangeNotify(ANotify: IJvDataProviderNotify);
begin
  if FNotifiers.IndexOf(ANotify) < 0 then
    FNotifiers.Add(ANotify);
end;

procedure TJvCustomDataProvider.UnregisterChangeNotify(ANotify: IJvDataProviderNotify);
begin
  FNotifiers.Remove(ANotify);
end;

constructor TJvCustomDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifiers := TInterfaceList.Create;
  if ItemsClass <> nil then
    FDataItemsImpl := ItemsClass.CreateProvider(Self)
  else
    raise EJVCLException.Create(SDataProviderNeedsItemsImpl);
  FDataItemsImpl._AddRef;
end;

destructor TJvCustomDataProvider.Destroy;
begin
  FNotifiers.Clear;
  FDataItemsImpl._Release;
  inherited Destroy;
end;

procedure TJvCustomDataProvider.BeforeDestruction;
begin
  if (FDataItemsImpl <> nil) and (FDataItemsImpl.RefCount > 1) then
    RunError(2);
  inherited BeforeDestruction;
  Changing(pcrDestroy);
end;

function TJvCustomDataProvider.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or (FDataItemsImpl.GetInterface(IID, Obj));
end;

initialization
  RegisterClasses([TJvBaseDataItem, TJvCustomDataItemsTextRenderer, TJvDataItemTextImpl,
    TJvDataItemImageImpl, TJvDataItemsList, TJvBaseDataItemsListManagement,
    TJvCustomDataItemsImages]);
end.
