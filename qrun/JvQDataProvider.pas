{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQDataProvider;

interface


{$HPPEMIT '#define IInterface System::IInterface'}


uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX} 
  Types, QWindows,
  QStdCtrls, // type TOwnerDrawState 
  Classes, Contnrs, QGraphics, QControls, QImgList,
  JclBase,
  JvQComponent, JvQDataProviderIntf;

type
  // Forwards
  TExtensibleInterfacedPersistent = class;
  TAggregatedPersistentEx = class;
  TJvBaseDataItem = class;
  TJvBaseDataItems = class;
  TJvBaseDataContexts = class;
  TJvBaseDataContextsManager = class;
  TJvBaseDataContext = class;
  TJvDataConsumer = class;
  TJvDataConsumerAggregatedObject = class;
  TJvDataConsumerServerNotify = class;
  TJvDataConsumerClientNotifyList = class;
  TJvDataConsumerClientNotifyItem = class;

  // Class references
  TAggregatedPersistentExClass = class of TAggregatedPersistentEx;
  TJvDataItemTextImplClass = class of TJvBaseDataItemTextImpl;
  TJvBaseDataItemClass = class of TJvBaseDataItem;
  TJvDataItemsClass = class of TJvBaseDataItems;
  TJvDataContextsClass = class of TJvBaseDataContexts;
  TJvDataContextsManagerClass = class of TJvBaseDataContextsManager;
  TJvDataContextClass = class of TJvBaseDataContext;
  TJvDataConsumerAggregatedObjectClass = class of TJvDataConsumerAggregatedObject;

  // Various types
  TProviderNotifyEvent = procedure(ADataProvider: IJvDataProvider;
    AReason: TDataProviderChangeReason; Source: IUnknown) of object;
  TJvDataProviderTree = type Integer;
  TJvDataProviderItemID = type string;
  TJvDataProviderContexts = type Integer;
  TBeforeCreateSubSvcEvent = procedure(Sender: TJvDataConsumer;
    var SubSvcClass: TJvDataConsumerAggregatedObjectClass) of object;
  TAfterCreateSubSvcEvent = procedure(Sender: TJvDataConsumer;
    SubSvc: TJvDataConsumerAggregatedObject) of object;
  TJvDataConsumerChangeReason = (ccrProviderSelect, ccrProviderChange, ccrViewChange,
    ccrContextChange, ccrOther);
  TJvDataConsumerChangeEvent = procedure(Sender: TJvDataConsumer;
    Reason: TJvDataConsumerChangeReason) of object;

  // Generic classes (move to some other unit?)
  TExtensibleInterfacedPersistent = class(TPersistent, IUnknown)
  private
    FAdditionalIntfImpl: TList;
  protected
    FRefCount: Integer;
    { IUnknown }
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
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
    destructor Destroy; override;
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
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  public
    constructor Create(Controller: IUnknown);
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
    property Controller: IUnknown read GetController;
  end;

  TAggregatedPersistentEx = class(TAggregatedPersistent)
  private
    FOwner: TExtensibleInterfacedPersistent;
  protected
    property Owner: TExtensibleInterfacedPersistent read FOwner;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); virtual;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  // Generic event based provider notification
  TJvProviderNotification = class(TObject, IUnknown, IJvDataProviderNotify)
  private
    FProvider: IJvDataProvider;
    FOnChanging: TProviderNotifyEvent;
    FOnChanged: TProviderNotifyEvent;
  protected
    procedure SetProvider(Value: IJvDataProvider);
    { IUnknown }
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    { IJvDataProviderNotify }
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    function Consumer: IJvDataConsumer;
  public
    destructor Destroy; override;
    property OnChanging: TProviderNotifyEvent read FOnChanging write FOnChanging;
    property OnChanged: TProviderNotifyEvent read FOnChanged write FOnChanged;
    property Provider: IJvDataProvider read FProvider write SetProvider;
  end;

  // Item implementation classes
  TJvDataItemAggregatedObject = class(TAggregatedPersistentEx)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;
    procedure ContextDestroying(Context: IJvDataContext); dynamic;
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
    function GetIndex: Integer;
    function GetImplementer: TObject;
    function GetID: string;
    procedure ContextDestroying(Context: IJvDataContext); dynamic;
    function IsParentOf(AnItem: IJvDataItem; DirectParent: Boolean = False): Boolean; virtual;
    function IsDeletable: Boolean; dynamic;
    property Items: IJvDataItems read GetItems;
    property Implementer: TObject read GetImplementer;
    { Optional IJvDataContextSensitive interface implementation }
    procedure RevertToAncestor; dynamic;
    function IsEqualToAncestor: Boolean; dynamic;
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
    function Editable: Boolean; virtual; abstract;
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

  TJvBaseDataItemRenderer = class(TJvDataItemAggregatedObject, IJvDataItemRenderer)
  protected
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates); virtual; abstract;
    function Measure(ACanvas: TCanvas): TSize; virtual; abstract;
  end;

  TJvBaseDataItemStates = class(TJvDataItemAggregatedObject, IJvDataItemStates)
  protected
    function Get_Enabled: TDataItemState; virtual; abstract;
    procedure Set_Enabled(Value: TDataItemState); virtual; abstract;
    function Get_Checked: TDataItemState; virtual; abstract;
    procedure Set_Checked(Value: TDataItemState); virtual; abstract;
    function Get_Visible: TDataItemState; virtual; abstract;
    procedure Set_Visible(Value: TDataItemState); virtual; abstract;
  end;

  // Items implementation classes
  TJvDataItemsAggregatedObject = class(TAggregatedPersistentEx)
  protected
    procedure ContextDestroying(Context: IJvDataContext); dynamic;
    function Items: IJvDataItems;
    function ItemsImpl: TJvBaseDataItems;
  end;

  TJvBaseDataItems = class(TExtensibleInterfacedPersistent, IJvDataItems, IJvDataIDSearch)
    function IJvDataIDSearch.Find = FindByID;
  private
    FParent: Pointer;
    FParentIntf: IJvDataItem;
    FProvider: IJvDataProvider;
    FSubAggregate: TAggregatedPersistentEx;
  protected
    { Adds an item to the list. }
    procedure InternalAdd(Item: IJvDataItem); virtual; abstract;
    { Removes an item from the list. }
    procedure InternalDelete(Index: Integer); virtual; abstract;
    { Moves an item in the list to a new index. }
    procedure InternalMove(OldIndex, NewIndex: Integer); virtual; abstract;
    { Called by the IJvDataItemsManagement and IJvDataItemsDesigner implementations to add a new
      item. It will redirect it to InternalAdd. InternalAdd will perform the add, but may also
      perform addition steps if needed (in case of context specific list it might need to copy the
      list first). }
    procedure ItemAdd(Item: IJvDataItem);
    { Called by the IJvDataItemsManagement implementation to remove an item. It will redirect it to
      InternalDelete. InternalDelete will perform the removal, but may also perform addition steps
      if needed (i.e. notify the other contexts if the delete is performed on the context-less list
      or copy the list for a context specific list that inherits from an ancestor). }
    procedure ItemDelete(Index: Integer);
    { Called by the IJvDataItem implementation to move an item. It will redirect it to
      InternalMove. InternalMove will perform the moving if it's called from within a context.
      The context-less list does not allow moving of items. }
    procedure ItemMove(OldIndex, NewIndex: Integer);
    { Determines if the item is streamable. }
    function IsStreamableItem(Item: IJvDataItem): Boolean; virtual;
    function ScanForID(Items: IJvDataItems; ID: string; Recursive: Boolean): IJvDataItem;
    { Streaming methods }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadItems(Reader: TReader);
    procedure WriteItems(Writer: TWriter);
    procedure ReadItem(Reader: TReader);
    procedure WriteItem(Writer: TWriter; Item: IJvDataItem);
    { IJvDataItems methods }
    function GetCount: Integer; virtual; abstract;
    function GetItem(I: Integer): IJvDataItem; virtual; abstract;
    function GetItemByID(ID: string): IJvDataItem;
    function GetItemByIndexPath(IndexPath: array of Integer): IJvDataItem;
    function GetParent: IJvDataItem; virtual;
    function GetProvider: IJvDataProvider;
    function GetImplementer: TObject;
    function IsDynamic: Boolean; virtual;
    procedure ContextDestroying(Context: IJvDataContext); dynamic;
    { IJvDataIDSearch methods }
    function FindByID(ID: string; const Recursive: Boolean = False): IJvDataItem;
  public
    constructor Create; overload; virtual;
    constructor Create(const Provider: IJvDataProvider); overload; virtual;
    constructor Create(const Parent: IJvDataItem); overload; virtual;
    procedure BeforeDestruction; override;
  end;

  TJvBaseDataItemsRenderer = class(TJvDataItemsAggregatedObject, IJvDataItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TProviderDrawStates); virtual; abstract;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; virtual; abstract;
    { IJvDataItemsRenderer methods }
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer;
      State: TProviderDrawStates); virtual;
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize; virtual;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TProviderDrawStates); virtual;
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
    procedure Remove(var Item: IJvDataItem); virtual; abstract;
  end;

  TJvBaseDataItemsImagesImpl = class(TJvDataItemsAggregatedObject, IJvDataItemsImages)
  protected
    { IJvDataItemImages methods }
    function GetDisabledImages: TCustomImageList; virtual; abstract;
    procedure SetDisabledImages(const Value: TCustomImageList); virtual; abstract;
    function GetHotImages: TCustomImageList; virtual; abstract;
    procedure SetHotImages(const Value: TCustomImageList); virtual; abstract;
    function GetImages: TCustomImageList; virtual; abstract;
    procedure SetImages(const Value: TCustomImageList); virtual; abstract;
  end;

  // Standard item implementers
  TJvDataItemTextImpl = class(TJvBaseDataItemTextImpl)
  private
    FCaption: string;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
    function Editable: Boolean; override;
  published
    property Caption: string read GetCaption write SetCaption;
  end;

  { Context sensitive text implementation: Retrieves/Sets the caption linked to the currently
    selected context. The implementation provides in a default caption that is not linked to any
    context. If there's no active context set at the provider; this caption will be retrieved/set.
    If the active context set at the provider has no caption linked to it, the standard caption
    is retrieved, but a new link is added when the caption is changed. }
  TJvDataItemContextTextImpl = class(TJvDataItemTextImpl, IJvDataContextSensitive)
  private
    FContextStrings: TStringList;
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
    function Editable: Boolean; override;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
    destructor Destroy; override;
    procedure RevertToAncestor; dynamic;
    function IsEqualToAncestor: Boolean; dynamic;
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
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex default 0;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex default 0;
  end;

  TJvBaseDataItemSubItems = class(TJvDataItemAggregatedObject, IJvDataItems)
  private
    FItems: IJvDataItems;
  protected
    property Items: IJvDataItems read FItems implements IJvDataItems;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent; AItems: TJvBaseDataItems); reintroduce; virtual;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

  TJvCustomDataItemTextRenderer = class(TJvBaseDataItemRenderer)
  protected
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates); override;
    function Measure(ACanvas: TCanvas): TSize; override;
  end;

  TJvCustomDataItemRenderer = class(TJvBaseDataItemRenderer)
  protected
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates); override;
    function Measure(ACanvas: TCanvas): TSize; override;
  end;

  TJvCustomDataItemStates = class(TJvBaseDataItemStates)
  private
    FEnabled: TDataItemState;
    FChecked: TDataItemState;
    FVisible: TDataItemState;
  protected
    procedure InitStatesUsage(UseEnabled, UseChecked, UseVisible: Boolean);
    function Get_Enabled: TDataItemState; override;
    procedure Set_Enabled(Value: TDataItemState); override;
    function Get_Checked: TDataItemState; override;
    procedure Set_Checked(Value: TDataItemState); override;
    function Get_Visible: TDataItemState; override;
    procedure Set_Visible(Value: TDataItemState); override;
  published
    property Enabled: TDataItemState read Get_Enabled write Set_Enabled;
    property Checked: TDataItemState read Get_Checked write Set_Checked;
    property Visible: TDataItemState read Get_Visible write Set_Visible;
  end;

  // Standard items implementers
  TJvCustomDataItemsTextRenderer = class(TJvBaseDataItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TProviderDrawStates); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; override;
    function AvgItemSize(ACanvas: TCanvas): TSize; override;
  end;

  TJvCustomDataItemsRenderer = class(TJvBaseDataItemsRenderer)
  protected
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TProviderDrawStates); override;
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
    constructor Create; override;
    destructor Destroy; override;

    property List: TObjectList read FList;
  end;

  TJvBaseDataItemsListManagement = class(TJvBaseDataItemsManagement)
  protected
    function Add(Item: IJvDataItem): IJvDataItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(var Item: IJvDataItem); override;
  end;

  TJvCustomDataItemsImages = class(TJvBaseDataItemsImagesImpl)
  private
    FDisabledImages: TCustomImageList;
    FHotImages: TCustomImageList;
    FImages: TCustomImageList;
  protected
    function GetDisabledImages: TCustomImageList; override;
    procedure SetDisabledImages(const Value: TCustomImageList); override;
    function GetHotImages: TCustomImageList; override;
    procedure SetHotImages(const Value: TCustomImageList); override;
    function GetImages: TCustomImageList; override;
    procedure SetImages(const Value: TCustomImageList); override;
  published
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    property HotImages: TCustomImageList read GetHotImages write SetHotImages;
    property Images: TCustomImageList read GetImages write SetImages;
  end;

  // Generic data provider implementation
  TJvCustomDataProvider = class(TJvComponent, IUnknown,  IJvDataProvider)
  private
    FDataItems: IJvDataItems;
    FDataContextsImpl: TJvBaseDataContexts;
    FDataContextsIntf: IJvDataContexts;
    FNotifiers: TInterfaceList;
    FTreeItems: TJvDataProviderTree;
    FConsumerStack: TInterfaceList;
    FContextStack: TInterfaceList;
    FContexts: TJvDataProviderContexts;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override;
    procedure Changing(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    procedure Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    class function PersistentDataItems: Boolean; virtual;
    class function ItemsClass: TJvDataItemsClass; virtual;
    class function ContextsClass: TJvDataContextsClass; virtual;
    class function ContextsManagerClass: TJvDataContextsManagerClass; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadRoot(Reader: TReader);
    procedure WriteRoot(Writer: TWriter);
    procedure ReadContexts(Reader: TReader);
    procedure WriteContexts(Writer: TWriter);
    procedure ReadContext(Reader: TReader; Index: Integer); 
    procedure WriteContext(Writer: TWriter; AContext: IJvDataContext);
    procedure AddToArray(var ClassArray: TClassArray; AClass: TClass);
    procedure DeleteFromArray(var ClassArray: TClassArray; Index: Integer);
    function IndexOfClass(AClassArray: TClassArray; AClass: TClass): Integer;
    procedure RemoveFromArray(var ClassArray: TClassArray; AClass: TClass);
    function IsTreeProvider: Boolean; dynamic;
    function GetDataItemsImpl: TJvBaseDataItems; 
    { IDataProvider }
    function GetItems: IJvDataItems; virtual;
    procedure RegisterChangeNotify(ANotify: IJvDataProviderNotify); dynamic;
    procedure UnregisterChangeNotify(ANotify: IJvDataProviderNotify); dynamic;
    function ConsumerClasses: TClassArray; dynamic;
    procedure SelectConsumer(Consumer: IJvDataConsumer);
    function SelectedConsumer: IJvDataConsumer;
    procedure ReleaseConsumer;
    procedure SelectContext(Context: IJvDataContext);
    function SelectedContext: IJvDataContext;
    procedure ReleaseContext;
    procedure ContextAdded(Context: IJvDataContext); dynamic;
    procedure ContextDestroying(Context: IJvDataContext); dynamic;
    procedure ConsumerDestroying(Consumer: IJvDataConsumer); dynamic;
    function AllowProviderDesigner: Boolean; dynamic;
    function AllowContextManager: Boolean; dynamic;
    function GetNotifierCount: Integer;
    function GetNotifier(Index: Integer): IJvDataProviderNotify;
    function GetImplementer: TObject;

    property DataItemsImpl: TJvBaseDataItems read GetDataItemsImpl;
    property DataContextsImpl: TJvBaseDataContexts read FDataContextsImpl;
    property Items: TJvDataProviderTree read FTreeItems write FTreeItems stored False;
    property Contexts: TJvDataProviderContexts read FContexts write FContexts stored False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function GetInterface(const IID: TGUID; out Obj): Boolean; virtual;
  end;

  // Basic context list
  TJvBaseDataContexts = class(TExtensibleInterfacedPersistent, IJvDataContexts)
  private
    FProvider: IJvDataProvider;
    FDsgnContext: IJvDataContext;
    FAncestor: IJvDataContext;
  protected
    procedure DoAddContext(Context: IJvDataContext); virtual; abstract;
    procedure DoDeleteContext(Index: Integer); virtual; abstract;
    procedure DoRemoveContext(Context: IJvDataContext); virtual; abstract;
    procedure DoClearContexts; virtual; abstract;
    function Provider: IJvDataProvider;
    function Ancestor: IJvDataContext;
    function GetCount: Integer; virtual; abstract;
    function GetContext(Index: Integer): IJvDataContext; virtual; abstract;
    function GetContextByName(Name: string): IJvDataContext; virtual;
    function IndexOf(Ctx: IJvDataContext): Integer; virtual;
    property DsgnContext: IJvDataContext read FDsgnContext write FDsgnContext;
  public
    constructor Create(AProvider: IJvDataProvider; AAncestor: IJvDataContext;
      ManagerClass: TJvDataContextsManagerClass = nil); virtual;
  end;

  // Basic context list manager
  TJvBaseDataContextsManager = class(TAggregatedPersistentEx, IJvDataContextsManager)
  protected
    function Contexts: IJvDataContexts;
    function ContextsImpl: TJvBaseDataContexts;
    function Add(Context: IJvDataContext): IJvDataContext;
    function New: IJvDataContext; virtual; abstract;
    procedure Delete(Context: IJvDataContext);
    procedure Clear;
  end;

  // Basic context
  TJvBaseDataContext = class(TExtensibleInterfacedPersistent, IJvDataContext)
  private
    FContexts: TJvBaseDataContexts;
  protected
    { Will actually set the name without any checks or notification. You should use SetName to
      change the context's name which in turn will call this method after it has checked the
      name is unique. }
    procedure DoSetName(Value: string); virtual; abstract;
    { Changes this context's name to the given name. It will first check if the new name is
      unique and then calls DoSetName to change it. }
    procedure SetName(Value: string); virtual;
    function GetImplementer: TObject;
    function ContextsImpl: TJvBaseDataContexts;
    function Contexts: IJvDataContexts;
    function Name: string; virtual; abstract;
    function IsDeletable: Boolean; dynamic;
    function IsStreamable: Boolean; dynamic;
  public
    constructor Create(AContexts: TJvBaseDataContexts; AName: string); virtual;
  end;

  // Basic managed context
  TJvBaseManagedDataContext = class(TJvBaseDataContext, IJvDataContextManager);

  // Basic fixed context
  TJvBaseFixedDataContext = class(TJvBaseDataContext)
  protected
    function IsDeletable: Boolean; override;
  end;

  // Standard context list
  TJvDataContexts = class(TJvBaseDataContexts)
  private
    FContexts: TInterfaceList;
  protected
    procedure DoAddContext(Context: IJvDataContext); override;
    procedure DoDeleteContext(Index: Integer); override;
    procedure DoRemoveContext(Context: IJvDataContext); override;
    procedure DoClearContexts; override;
    function GetCount: Integer; override;
    function GetContext(Index: Integer): IJvDataContext; override;
  public
    constructor Create(AProvider: IJvDataProvider; AAncestor: IJvDataContext;
      ManagerClass: TJvDataContextsManagerClass = nil); override;
    destructor Destroy; override;
  end;

  // Standard context
  TJvDataContext = class(TJvBaseDataContext)
  private
    FName: string;
  protected
    procedure DoSetName(Value: string); override;
    function Name: string; override;
  end;

  // Standard managed context
  TJvManagedDataContext = class(TJvDataContext, IJvDataContextManager);

  // Standard fixed context
  TJvFixedDataContext = class(TJvDataContext)
  protected
    function IsDeletable: Boolean; override;
  end;

// Helper classes: rendering helpers
  { Render class to be used by both the IJvDataItemsRenderer as well as IJvDataItemRenderer
    implementers. Reduces code duplication if both type of implementers can use the same rendering
    mechanism. }
  TJvDP_ProviderBaseRender = class(TObject)
  private
    FItem: IJvDataItem;
    FCanvas: TCanvas;
    FState: TProviderDrawStates;
  protected
    Rect: TRect;
    procedure Prepare(ForMeasure: Boolean); virtual; abstract; 
    procedure DoDraw; virtual; abstract;
    function DoMeasure: TSize; virtual; abstract;

    property Item: IJvDataItem read FItem;
    property Canvas: TCanvas read FCanvas;
    property State: TProviderDrawStates read FState;
  public
    constructor Create(AItem: IJvDataItem; ACanvas: TCanvas; AState: TProviderDrawStates);
    class procedure Draw(AItem: IJvDataItem; ACanvas: TCanvas; var ARect: TRect; AState: TProviderDrawStates);
    class function Measure(AItem: IJvDataItem; ACanvas: TCanvas; AState: TProviderDrawStates): TSize;
  end;

  TJvDP_ProviderTextOnlyRender = class(TJvDP_ProviderBaseRender)
  private
    FHasNoText: Boolean;
    FText: string;
    FTextRect: TRect;
  protected
    procedure Prepare(ForMeasure: Boolean); override;
    procedure DoDraw; override;
    function DoMeasure: TSize; override;

    property HasNoText: Boolean read FHasNoText write FHasNoText;
    property Text: string read FText write FText;
    property TextRect: TRect read FTextRect write FTextRect;
  end;

  TJvDP_ProviderImgAndTextRender = class(TJvDP_ProviderTextOnlyRender)
  private
    FHasImage: Boolean;
    FHasDisabledImage: Boolean;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FAlignment: TAlignment;
  protected
    procedure Prepare(ForMeasure: Boolean); override;
    procedure DoDraw; override;
    function DoMeasure: TSize; override;

    property HasImage: Boolean read FHasImage write FHasImage;
    property HasDisabledImage: Boolean read FHasDisabledImage write FHasDisabledImage;
    property Images: TCustomImageList read FImages write FImages;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Alignment: TAlignment read FAlignment write FAlignment;
  end;

  TJvDataConsumer = class(TExtensibleInterfacedPersistent, IJvDataConsumer, IJvDataProviderNotify,
    IJvDataConsumerProvider, IJvDataConsumerClientNotify)
  private
    FOwner: TComponent;
    FAttrList: array of Integer;
    FProvider: IJvDataProvider;
    FContext: IJvDataContext;
    FAfterCreateSubSvc: TAfterCreateSubSvcEvent;
    FBeforeCreateSubSvc: TBeforeCreateSubSvcEvent;
    FOnChanging: TJvDataConsumerChangeEvent;
    FOnChanged: TJvDataConsumerChangeEvent;
    FNeedFixups: Boolean;
    FFixupContext: TJvDataContextID;
    FOnProviderChanging: TProviderNotifyEvent;
    FOnProviderChanged: TProviderNotifyEvent;
    FServerList: TInterfaceList;
    procedure SetProvider(Value: IJvDataProvider); 
  protected
    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;
    { Event triggering }
    procedure DoProviderChanging(ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DoProviderChanged(ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DoAfterCreateSubSvc(ASvc: TJvDataConsumerAggregatedObject);
    procedure DoBeforeCreateSubSvc(var AClass: TJvDataConsumerAggregatedObjectClass);
    procedure DoChanging(Reason: TJvDataConsumerChangeReason);
    procedure DoChanged(Reason: TJvDataConsumerChangeReason);
    { Misc. }
    procedure DoAddAttribute(Attr: Integer);
    procedure Changing(Reason: TJvDataConsumerChangeReason); virtual;
    procedure Changed(Reason: TJvDataConsumerChangeReason); virtual;
    procedure ProviderChanging;
    procedure ProviderChanged;
    procedure ContextChanging;
    procedure ContextChanged;
    procedure AfterSubSvcAdded(ASvc: TJvDataConsumerAggregatedObject); virtual;
    procedure UpdateExtensions; virtual;
    procedure FixupExtensions;
    procedure FixupContext;
    procedure ViewChanged(AExtension: TJvDataConsumerAggregatedObject);
    procedure NotifyItemSelected(Value: IJvDataItem);
    procedure NotifyServerItemChanged(Server: IJvDataConsumerServerNotify; Value: IJvDataItem);
    procedure NotifyServerProviderChanged;
    function ExtensionCount: Integer;
    function Extension(Index: Integer): TJvDataConsumerAggregatedObject;
    function IsContextStored: Boolean;
    function GetNeedExtensionFixups: Boolean;
    function GetNeedContextFixup: Boolean;
    { Property access }
    function GetContext: TJvDataContextID;
    procedure SetContext(Value: TJvDataContextID);
    function GetServerCount: Integer;
    function GetServers(I: Integer): IJvDataConsumerServerNotify;
    { IJvDataProviderNotify methods }
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    function Consumer: IJvDataConsumer;
    { IJvDataConsumer methods }
    function VCLComponent: TComponent;
    function AttributeApplies(Attr: Integer): Boolean;
    { IJvDataConsumerProvider methods }
    function IJvDataConsumerProvider.GetProvider = ProviderIntf;
    { IJvDataConsumerClientNotify methods }
    procedure IJvDataConsumerClientNotify.ItemSelected = ServerItemChanged;
    procedure ServerItemChanged(Server: IJvDataConsumerServerNotify; Value: IJvDataItem); virtual;
    procedure LinkAdded(Server: IJvDataConsumerServerNotify);
    procedure LinkRemoved(Server: IJvDataConsumerServerNotify);
    { States }
    property NeedExtensionFixups: Boolean read GetNeedExtensionFixups;
    property NeedContextFixup: Boolean read GetNeedContextFixup;
    { Other }
    property ServerCount: Integer read GetServerCount;
    property Servers[I: Integer]: IJvDataConsumerServerNotify read GetServers;
  public
    constructor Create(AOwner: TComponent; Attributes: array of Integer);
    destructor Destroy; override;
    { Direct link to actual provider interface. This is done to aid in the implementation (less
      IFDEF's in the code; always refer to ProviderIntf and it's working in all Delphi versions). }
    function ProviderIntf: IJvDataProvider; virtual;
    procedure SetProviderIntf(Value: IJvDataProvider); virtual;
    function ContextIntf: IJvDataContext; virtual;
    procedure SetContextIntf(Value: IJvDataContext); virtual;
    procedure Loaded; virtual;
    procedure Enter;
    procedure Leave;
    { Notifies the consumer the specified item is selected in the control. Will execute the item's
      IJvDataItemExecute interface if one is assigned to the item and notifies all service
      extensions of the selection change. }
    procedure ItemSelected(Value: IJvDataItem);
    function IsLoading: Boolean;

    property OnChanging: TJvDataConsumerChangeEvent read FOnChanging write FOnChanging;
    property OnChanged: TJvDataConsumerChangeEvent read FOnChanged write FOnChanged;
    property OnProviderChanging: TProviderNotifyEvent read FOnProviderChanging
      write FOnProviderChanging;
    property OnProviderChanged: TProviderNotifyEvent read FOnProviderChanged
      write FOnProviderChanged;
    property AfterCreateSubSvc: TAfterCreateSubSvcEvent read FAfterCreateSubSvc
      write FAfterCreateSubSvc;
    property BeforeCreateSubSvc: TBeforeCreateSubSvcEvent read FBeforeCreateSubSvc
      write FBeforeCreateSubSvc;
  published 
    property Provider: IJvDataProvider read ProviderIntf write SetProvider; 
    property Context: TJvDataContextID read GetContext write SetContext stored IsContextStored;
  end;

  TJvDataConsumerAggregatedObject = class(TAggregatedPersistentEx)
  protected
    StreamedInWithoutProvider: Boolean;
    procedure DataProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); virtual;
    procedure DataProviderChanged(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); virtual;
    { Called when the Provider/Context are set and NotifyFixups has been called earlier. It doesn't
      matter which sub service called NotifyFixups, all services are notified if the
      provider/context are set. }
    procedure Fixup; virtual;
    { Called after a new provider is selected to determine if the sub service can stay around.
      Return False to have the sub service removed (the default implementation) or set to True to
      keep it around. Note that on entry to this method the new provider is already selected. }
    function KeepOnProviderChange: Boolean; virtual;
    { Called after a new context is selected to determine if the sub service can stay around.
      Return False to have the sub service removed or set to True to keep it around (default
      implementation). Note that on entry to this method the new context is already selected. }
    function KeepOnContextChange: Boolean; virtual;
    { Notifies the consumer service a change is about to take place. Sub services should call this
      method when something is changing. }
    procedure Changing(Reason: TJvDataConsumerChangeReason);
    { Notifies the consumer service a change has taken place. Sub services should call this method
      when something has changed. }
    procedure Changed(Reason: TJvDataConsumerChangeReason);
    { Notifies the consumer service (and other extensions) a change has taken place that might have
      influenced the view list. }
    procedure NotifyViewChanged;
    { Called after the view has changed by another extension. }
    procedure ViewChanged(AExtension: TJvDataConsumerAggregatedObject); virtual;
    { Called when an item has been selected by the consumer. }
    procedure ItemSelected(Value: IJvDataItem); virtual;
    { Called when a linked server consumer has selected a new item. }
    procedure ServerItemChanged(Server: IJvDataConsumerServerNotify; Value: IJvDataItem); virtual;
    { Signal to the consumer service that settings need to be applies but the provider/context was
      not yet available. This may occur during streaming in from the DFM. As soon as the provider is
      known, the context is also set and Fixup is called for all sub services. }
    procedure NotifyFixups;
    { Called when the provider is about to be changed. }
    procedure ProviderChanging; virtual;
    { Called when the provider has changed but only after KeepOnProviderChange returned True. }
    procedure ProviderChanged; virtual;
    { Called when the context is about to be changed. }
    procedure ContextChanging; virtual;
    { Called when the context has changed but only after KeepOnContextChange returned True. }
    procedure ContextChanged; virtual;
    { Reference to the consumer service interface. }
    function Consumer: IJvDataConsumer;
    { Reference to the consumer service implementation. }
    function ConsumerImpl: TJvDataConsumer;
    { Retrieve the root IJvDataItems reference. }
    function RootItems: IJvDataItems;
  end;

  { Consumer sub service to select the context to use for the consumer. Only needed for design time
    purposes; use TJvDataConsumer.Context to change it directly. }
  TJvDataConsumerContext = class(TJvDataConsumerAggregatedObject, IJvDataConsumerContext)
  protected
    function GetContextID: TJvDataContextID;
    procedure SetContextID(Value: TJvDataContextID);
    function GetContext: IJvDataContext;
    procedure SetContext(Value: IJvDataContext);
  public
    property ContextIntf: IJvDataContext read GetContext write SetContext;
  published
    property Context: TJvDataContextID read GetContextID write SetContextID;
  end;

  { Consumer sub service to select the item to display or item that serves as the root. }
  TJvDataConsumerItemSelect = class(TJvDataConsumerAggregatedObject, IJvDataConsumerItemSelect)
    { Method resolutions }
    function IJvDataConsumerItemSelect.GetItem = GetItemIntf;
    procedure IJvDataConsumerItemSelect.SetItem = SetItemIntf;
  private
    FItemID: TJvDataItemID;
    FItem: IJvDataItem;
  protected
    procedure Fixup; override;
    function GetItem: TJvDataItemID;
    procedure SetItem(Value: TJvDataItemID);
    procedure DataProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); override;
    procedure DataProviderChanged(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); override;
  public
    function GetItemIntf: IJvDataItem;
    procedure SetItemIntf(Value: IJvDataItem);
  published
    property Item: TJvDataItemID read GetItem write SetItem;
  end;

  { Consumer sub service to maintain a flat list of the data tree. }
  TJvCustomDataConsumerViewList = class(TJvDataConsumerAggregatedObject, IJvDataConsumerViewList)
  private
    FAutoExpandLevel: Integer;
    FExpandOnNewItem: Boolean;
    FLevelIndent: Integer;
  protected
    function KeepOnProviderChange: Boolean; override;
    procedure ProviderChanging; override;
    procedure ProviderChanged; override;
    procedure ContextChanged; override;
    procedure ViewChanged(AExtension: TJvDataConsumerAggregatedObject); override;
    procedure DataProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); override;
    procedure DataProviderChanged(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown); override;
    function InternalItemSibling(ParentIndex: Integer; var ScanIndex: Integer): Integer;
    function Get_AutoExpandLevel: Integer;
    procedure Set_AutoExpandLevel(Value: Integer);
    function Get_ExpandOnNewItem: Boolean;
    procedure Set_ExpandOnNewItem(Value: Boolean);
    function Get_LevelIndent: Integer;
    procedure Set_LevelIndent(Value: Integer);
    { Add an item as the sub item of the item specified. The parent item will be marked as being
      expanded. }
    procedure AddItem(Index: Integer; Item: IJvDataItem; ExpandToLevel: Integer = 0); virtual; abstract;
    { Add a list of items at the specified Index. The item preceding that index will be handled as
      if it was the parent of all items to be inserted. This will also mark that item as being
      expanded. }
    procedure AddItems(var Index: Integer; Items: IJvDataItems; ExpandToLevel: Integer = 0); virtual; abstract;
    procedure AddChildItem(ParentIndex: Integer; Item: IJvDataItem); virtual; abstract;
    procedure InsertItem(InsertIndex, ParentIndex: Integer; Item: IJvDataItem); virtual; abstract;
    { Delete the specified item and the items sub tree. }
    procedure DeleteItem(Index: Integer); virtual; abstract;
    { Deletes the specified items sub tree and mark the item as not-expanded }
    procedure DeleteItems(Index: Integer); virtual; abstract;
    procedure UpdateItemFlags(Index: Integer; Value, Mask: Integer); virtual; abstract;
    procedure ClearView; virtual;
    procedure RebuildView; virtual;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
    procedure ExpandTreeTo(Item: IJvDataItem); virtual;
    { Toggles an item's expanded state. If an item becomes expanded, the item's sub item as present
      in the IJvDataItems instance will be added; if an item becomes collapsed the sub items are
      removed from the view. }
    procedure ToggleItem(Index: Integer); virtual; abstract;
    { Locate an item in the view list, returning it's absolute index. }
    function IndexOfItem(Item: IJvDataItem): Integer; virtual; abstract;
    { Locate an item ID in the view list, returning it's absolute index. }
    function IndexOfID(ID: TJvDataItemID): Integer; virtual; abstract;
    { Locate an item in the view list, returning it's index in the parent item. }
    function ChildIndexOfItem(Item: IJvDataItem): Integer; virtual; abstract;
    { Locate an item ID in the view list, returning it's index in the parent item. }
    function ChildIndexOfID(ID: TJvDataItemID): Integer; virtual; abstract;
    { Retrieve the IJvDataItem reference given the absolute index into the view list. }
    function Item(Index: Integer): IJvDataItem; virtual; abstract;
    { Retrieve an items level given the absolute index into the view list. }
    function ItemLevel(Index: Integer): Integer; virtual; abstract;
    { Retrieve an items expanded state given the absolute index into the view list. }
    function ItemIsExpanded(Index: Integer): Boolean; virtual; abstract;
    { Determine if an item has children given the absolute index into the view list. }
    function ItemHasChildren(Index: Integer): Boolean; virtual; abstract;
    { Retrieve an items parent given the absolute index into the view list. }
    function ItemParent(Index: Integer): IJvDataItem; virtual; abstract;
    { Retrieve an items parent absolute index given the absolute index into the view list. }
    function ItemParentIndex(Index: Integer): Integer; virtual; abstract;
    { Retrieve an items sibling given an absolute index. }
    function ItemSibling(Index: Integer): IJvDataItem; virtual; abstract;
    { Retrieve the index of an items sibling given an absolute index. }
    function ItemSiblingIndex(Index: Integer): Integer; virtual; abstract;
    { Retrieve the IJvDataItem reference given the child index and a parent item. }
    function SubItem(Parent: IJvDataItem; Index: Integer): IJvDataItem; overload; virtual; abstract;
    { Retrieve the IJvDataItem reference given the child index and a parent absolute index. }
    function SubItem(Parent, Index: Integer): IJvDataItem; overload; virtual; abstract;
    { Retrieve the absolute index given a child index and a parent item. }
    function SubItemIndex(Parent: IJvDataItem; Index: Integer): Integer; overload; virtual; abstract;
    { Retrieve the absolute index given a child index and a parent absolute index. }
    function SubItemIndex(Parent, Index: Integer): Integer; overload; virtual; abstract;
    { Retrieve info on grouping; each bit represents a level, if the bit is set the item at that
      level has another sibling. Can be used to render tree lines. Note that this is very generic
      implementation that is not the fastest. To make this info readily available will require
      a descendant that stores and updates this info on a per item basis. This method can then be
      adpated to use that info directly. }
    function ItemGroupInfo(Index: Integer): TDynIntegerArray; virtual;
    { Retrieve the number of viewable items. }
    function Count: Integer; virtual; abstract;

    property AutoExpandLevel: Integer read FAutoExpandLevel write FAutoExpandLevel;
    property ExpandOnNewItem: Boolean read FExpandOnNewItem write FExpandOnNewItem;
    property LevelIndent: Integer read Get_LevelIndent write Set_LevelIndent default 16;
  end;

  { View list; uses the least possible amount of memory but may be slow to find sibling/child
    items. }
  TViewListItem = record
    ItemID: string;
    Flags: Integer; // lower 24 bits contain item level
  end;
  TViewListItems = array of TViewListItem;

  TJvDataConsumerViewList = class(TJvCustomDataConsumerViewList)
  private
    FViewItems: TViewListItems;
  protected
    procedure AddItem(Index: Integer; Item: IJvDataItem; ExpandToLevel: Integer = 0); override;
    procedure AddChildItem(ParentIndex: Integer; Item: IJvDataItem); override;
    procedure AddItems(var Index: Integer; Items: IJvDataItems; ExpandToLevel: Integer = 0); override;
    procedure InsertItem(InsertIndex, ParentIndex: Integer; Item: IJvDataItem); override;
    procedure DeleteItem(Index: Integer); override;
    procedure DeleteItems(Index: Integer); override;
    procedure UpdateItemFlags(Index: Integer; Value, Mask: Integer); override;
  public
    procedure ToggleItem(Index: Integer); override;
    function IndexOfItem(Item: IJvDataItem): Integer; override;
    function IndexOfID(ID: TJvDataItemID): Integer; override;
    function ChildIndexOfItem(Item: IJvDataItem): Integer; override;
    function ChildIndexOfID(ID: TJvDataItemID): Integer; override;
    function Item(Index: Integer): IJvDataItem; override;
    function ItemLevel(Index: Integer): Integer; override;
    function ItemIsExpanded(Index: Integer): Boolean; override;
    function ItemHasChildren(Index: Integer): Boolean; override;
    function ItemParent(Index: Integer): IJvDataItem; override;
    function ItemParentIndex(Index: Integer): Integer; override;
    function ItemSibling(Index: Integer): IJvDataItem; override;
    function ItemSiblingIndex(Index: Integer): Integer; override;
    function SubItem(Parent: IJvDataItem; Index: Integer): IJvDataItem; override;
    function SubItem(Parent, Index: Integer): IJvDataItem; override;
    function SubItemIndex(Parent: IJvDataItem; Index: Integer): Integer; override;
    function SubItemIndex(Parent, Index: Integer): Integer; override;
    function Count: Integer; override;
  published
    property LevelIndent;
  end;

  TJvDataConsumerServerNotify = class(TJvDataConsumerAggregatedObject, IJvDataConsumerServerNotify)
  private
    FClients: TJvDataConsumerClientNotifyList;
  protected
    procedure SetClients(Value: TJvDataConsumerClientNotifyList);
    procedure ItemSelected(Value: IJvDataItem); override;
    function GetOwner: TPersistent; override;
    procedure NotifyItemSelected(Value: IJvDataItem);
    { IJvDataConsumerServerNotify }
    procedure AddClient(Client: IJvDataConsumerClientNotify);
    procedure RemoveClient(Client: IJvDataConsumerClientNotify);
    procedure NotifyProviderChanged(Client: IJvDataConsumerClientNotify); virtual;
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean; virtual;
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
    destructor Destroy; override;
  published
    property Clients: TJvDataConsumerClientNotifyList read FClients write SetClients;
  end;

  TJvDataConsumerClientNotifyList = class(TOwnedCollection)
  private
    FServer: TJvDataConsumerServerNotify;
  protected
    function GetServer: TJvDataConsumerServerNotify;
    function GetNotifyItems(I: Integer): TJvDataConsumerClientNotifyItem;
    function GetConsumer(I: Integer): IJvDataConsumer;
    procedure SetItemName(Item: TCollectionItem); override;
  public
    constructor Create(AServer: TJvDataConsumerServerNotify);

    procedure Add(AComponent: TComponent); overload;
    procedure Add(AConsumer: IJvDataConsumer); overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(AComponent: TComponent); overload;
    procedure Delete(AConsumer: IJvDataConsumer); overload;
    function IndexOf(AComponent: TComponent): Integer; overload;
    function IndexOf(AConsumer: IJvDataConsumer): Integer; overload;

    property Server: TJvDataConsumerServerNotify read GetServer;
    property NotifyItems[I: Integer]: TJvDataConsumerClientNotifyItem read GetNotifyItems;
    property Clients[I: Integer]: IJvDataConsumer read GetConsumer; default;
  end;

  TJvDataConsumerClientNotifyItem = class(TCollectionItem)
  private
    FNotifier: IJvDataConsumerClientNotify;
  protected
    function GetList: TJvDataConsumerClientNotifyList;
    function GetConsumer: IJvDataConsumer;
    function GetComponent: TComponent;
    procedure SetComponent(Value: TComponent);
    procedure SetNotifier(Value: IJvDataConsumerClientNotify);
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;

    property List: TJvDataConsumerClientNotifyList read GetList;
    property Notifier: IJvDataConsumerClientNotify read FNotifier write SetNotifier;
  published
    property Component: TComponent read GetComponent write SetComponent;
  end;

  { TStrings descendant that will use the specified consumer's view list to retrieve the individual
    items.  }
  TJvConsumerStrings = class(TStrings)
  private
    FConsumer: TJvDataConsumer;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    property Consumer: TJvDataConsumer read FConsumer;
  public
    constructor Create(AConsumer: TJvDataConsumer);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
  end;

// Helper routines
{ Locate nearest IJvDataItems* implementation for a specific item. }
function DP_FindItemsIntf(AItem: IJvDataItem; IID: TGUID; out Obj): Boolean;
{ Locate nearest IJvDataItemsRenderer implementation for a specific item. }
function DP_FindItemsRenderer(AItem: IJvDataItem; out Renderer: IJvDataItemsRenderer): Boolean;
{ Locate nearest IJvDataItemsImages implementation for a specific item. }
function DP_FindItemsImages(AItem: IJvDataItem; out Images: IJvDataItemsImages): Boolean;
{ Generate items list to emulate trees in a flat list control }
procedure DP_GenItemsList(RootList: IJvDataItems; ItemList: TStrings);
{ Convert TOwnerDrawState to TProviderDrawStates }
function DP_OwnerDrawStateToProviderDrawState(State: TOwnerDrawState): TProviderDrawStates;
{ Atomically select a consumer/context pair, pushing the current consumer/context onto their
  internal stacks. }
procedure DP_SelectConsumerContext(Provider: IJvDataProvider; Consumer: IJvDataConsumer; Context: IJvDataContext);
{ Atomically release a consumer/context pair, reinstating the prior pair on the respective stacks. }
procedure DP_ReleaseConsumerContext(Provider: IJvDataProvider);
{ Retrieve the specified context's name path. }
function GetContextPath(Context: IJvDataContext): string;
{ Retrieve the specified item's ID path. The path is based on the currently active context. }
function GetItemIDPath(Item: IJvDataItem): string;
{ Retrieve the specified item's index path. The path is based on the currently active context. }
function GetItemIndexPath(Item: IJvDataItem): TDynIntegerArray;
{ Determine a unique context name for the given prefix in the given context list. }
function GetUniqueCtxName(Contexts: IJvDataContexts; Prefix: string): string;
{ Determine checked state for an item, combining both consumer and provider info. }
function GetItemCheckedState(Item: IJvDataItem): TDataItemState;
{ Determine enabled state for an item, combining both consumer and provider info. }
function GetItemEnabledState(Item: IJvDataItem): TDataItemState;
{ Determine visible state for an item, combining both consumer and provider info. }
function GetItemVisibleState(Item: IJvDataItem): TDataItemState;

// Rename and move to JvFunctions? Converts a buffer into a string of hex digits.
function HexBytes(const Buf; Length: Integer): string;
// Move to other unit? Render text in a disabled way (much like TLabel does)
procedure DisabledTextRect(ACanvas: TCanvas; var ARect: TRect; Left, Top: Integer; Text: string);

implementation

uses
  {$IFDEF MSWINDOWS}
  ActiveX,
  {$ENDIF MSWINDOWS}
  SysUtils, QConsts, TypInfo, 
  RTLConsts, 
  JclStrings,
  JvQTypes, JvQConsts, JvQResources;

const
  vifHasChildren = Integer($80000000);
  vifCanHaveChildren = Integer($40000000);
  vifExpanded = Integer($20000000);

  cClassName = 'ClassName';
  cName = 'Name';
  cProvider = 'Provider';

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

//TODO: Copied from JvLabel.pas to avoid dependency. Must move to another unit.

type
  TShadowPosition = (spLeftTop, spLeftBottom, spRightBottom, spRightTop);

function DrawShadowText(DC: HDC; Str: PChar; Count: Integer; var Rect: TRect;
  Format: Word; ShadowSize: Byte; ShadowColor: TColorRef;
  ShadowPos: TShadowPosition): Integer;
var
  RText, RShadow: TRect;
  Color: TColorRef;
  OldBkMode: Integer;
begin
  RText := Rect;
  RShadow := Rect;
  Color := SetTextColor(DC, ShadowColor);
  case ShadowPos of
    spLeftTop:
      OffsetRect(RShadow, -ShadowSize, -ShadowSize);
    spRightBottom:
      OffsetRect(RShadow, ShadowSize, ShadowSize);
    spLeftBottom:
      begin
        {OffsetRect(RText, ShadowSize, 0);}
        OffsetRect(RShadow, -ShadowSize, ShadowSize);
      end;
    spRightTop:
      begin
        {OffsetRect(RText, 0, ShadowSize);}
        OffsetRect(RShadow, ShadowSize, -ShadowSize);
      end;
  end;
  Result := DrawText(DC, Str, Count, RShadow, Format);
  if Result > 0 then
    Inc(Result, ShadowSize);
  SetTextColor(DC, Color);
  OldBkMode := SetBkMode(DC, TRANSPARENT);
  try
    DrawText(DC, Str, Count, RText, Format);
  finally
    SetBkMode(DC, OldBkMode);
  end;
  UnionRect(Rect, RText, RShadow);
end;

procedure DisabledTextRect(ACanvas: TCanvas; var ARect: TRect; Left, Top: Integer; Text: string);
begin
  ACanvas.Font.Color := clGrayText; 
  ACanvas.Start; 
  DrawShadowText(ACanvas.Handle, PChar(Text), Length(Text), ARect, 0, 1, ColorToRGB(clBtnHighlight),
    spRightBottom); 
  ACanvas.Stop; 
end;

procedure AddItemsToList(AItems: IJvDataItems; ItemList: TStrings; Level: Integer);
var
  I: Integer;
  ThisItem: IJvDataItem;
  SubItems: IJvDataItems;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    ThisItem := AItems.Items[I];
    ItemList.AddObject(ThisItem.GetID, TObject(Level));
    if Supports(ThisItem, IJvDataItems, SubItems) then
      AddItemsToList(SubItems, ItemList, Level + 1);
  end;
end;

function DP_FindItemsIntf(AItem: IJvDataItem; IID: TGUID; out Obj): Boolean;
begin
  while (AItem <> nil) and not Supports(AItem.GetItems, IID, Obj) do
    AItem := AItem.GetItems.Parent;
  Result := AItem <> nil;
end;

function DP_FindItemsRenderer(AItem: IJvDataItem; out Renderer: IJvDataItemsRenderer): Boolean;
begin
  Result := DP_FindItemsIntf(AItem, IJvDataItemsRenderer, Renderer);
end;

function DP_FindItemsImages(AItem: IJvDataItem; out Images: IJvDataItemsImages): Boolean;
begin
  Result := DP_FindItemsIntf(AItem, IJvDataItemsImages, Images);
end;

procedure DP_GenItemsList(RootList: IJvDataItems; ItemList: TStrings);
begin
  ItemList.Clear;
  AddItemsToList(RootList, ItemList, 0);
end;

function DP_OwnerDrawStateToProviderDrawState(State: TOwnerDrawState): TProviderDrawStates;
begin
  Move(State, Result, SizeOf(State));
end;

procedure DP_SelectConsumerContext(Provider: IJvDataProvider; Consumer: IJvDataConsumer; Context: IJvDataContext);
begin
  Provider.SelectConsumer(Consumer);
  try
    Provider.SelectContext(Context);
  except
    Provider.ReleaseConsumer;
    raise;
  end;
end;

procedure DP_ReleaseConsumerContext(Provider: IJvDataProvider);
var
  CurConsumer: IJvDataConsumer;
begin
  CurConsumer := Provider.SelectedConsumer;
  Provider.ReleaseConsumer;
  try
    Provider.ReleaseContext;
  except
    Provider.SelectConsumer(CurConsumer);
    raise;
  end;
end;

function IsExtensionSpecificIntf(IID: TGUID): Boolean;
begin
  Result := IsEqualGuid(IID, IJvDataContextSensitive);
end;

function GetContextPath(Context: IJvDataContext): string;
begin
  if Context <> nil then
  begin
    Result := Context.Name;
    while Context <> nil do
    begin
      Context := Context.Contexts.Ancestor;
      if Context <> nil then
        Result := Context.Name + '\' + Result;
    end;
  end;
end;

function GetItemIDPath(Item: IJvDataItem): string;
begin
  if Item <> nil then
  begin
    Result := Item.GetID;
    while Item <> nil do
    begin
      Item := Item.Items.Parent;
      if Item <> nil then
        Result := Item.GetID + '\' + Result;
    end;
  end;
end;

procedure InsertIntArray(var Arr: TDynIntegerArray; Index: Integer; Item: Integer);
begin
  SetLength(Arr, Length(Arr) + 1);
  if Index < High(Arr) then
    Move(Arr[Index], Arr[Index + 1], (High(Arr) - Index) * SizeOf(Integer));
  Arr[Index] := Item;
end;

function GetItemIndexPath(Item: IJvDataItem): TDynIntegerArray;
begin
  if Item <> nil then
  begin
    SetLength(Result, 1);
    Result[0] := Item.GetIndex;
    while Item <> nil do
    begin
      Item := Item.Items.Parent;
      if Item <> nil then
        InsertIntArray(Result, 0, Item.GetIndex);
    end;
  end
  else
    SetLength(Result, 0);
end;

function GetUniqueCtxName(Contexts: IJvDataContexts; Prefix: string): string;
var
  PrefixLen: Integer;
  SuffixNum: Int64;
  CtxIdx: Integer;
  TmpNum: Int64;
begin
  PrefixLen := Length(Prefix);
  SuffixNum := 1;
  for CtxIdx := 0 to Contexts.GetCount - 1 do
    if AnsiSameStr(Prefix, Copy(Contexts.GetContext(CtxIdx).Name, 1, PrefixLen)) then
      with Contexts.GetContext(CtxIdx) do
      begin
        if StrIsSubset(Copy(Name, PrefixLen + 1, Length(Name) - PrefixLen), DigitSymbols) then
        begin
          TmpNum := StrToInt64(Copy(Name, PrefixLen + 1, Length(Name) - PrefixLen));
          if TmpNum >= SuffixNum then
            SuffixNum := TmpNum + 1;
        end;
      end;
  Result := Prefix + IntToStr(SuffixNum);
end;

function GetItemCheckedState(Item: IJvDataItem): TDataItemState;
var
  Provider: IJvDataProvider;
  ConsState: IJvDataConsumerItemState;
  ItemState: IJvDataItemStates;
begin
  Result := disNotUsed;
  if Item <> nil then
  begin
    Provider := Item.Items.Provider;
    if Supports(Provider.SelectedConsumer, IJvDataConsumerItemState, ConsState) then
      Result := ConsState.Checked(Item);
    if (Result = disNotUsed) and Supports(Item, IJvDataItemStates, ItemState) then
      Result := ItemState.Checked;
  end;
end;

function GetItemEnabledState(Item: IJvDataItem): TDataItemState;
var
  Provider: IJvDataProvider;
  ConsState: IJvDataConsumerItemState;
  ItemState: IJvDataItemStates;
begin
  Result := disNotUsed;
  if Item <> nil then
  begin
    Provider := Item.Items.Provider;
    if Supports(Provider.SelectedConsumer, IJvDataConsumerItemState, ConsState) then
      Result := ConsState.Enabled(Item);
    if (Result <> disFalse) and Supports(Item, IJvDataItemStates, ItemState) then
      Result := ItemState.Enabled;
  end;
end;

function GetItemVisibleState(Item: IJvDataItem): TDataItemState;
var
  Provider: IJvDataProvider;
  ConsState: IJvDataConsumerItemState;
  ItemState: IJvDataItemStates;
begin
  Result := disNotUsed;
  if Item <> nil then
  begin
    Provider := Item.Items.Provider;
    if Supports(Provider.SelectedConsumer, IJvDataConsumerItemState, ConsState) then
      Result := ConsState.Visible(Item);
    if (Result in [disIndetermined, disNotUsed]) and Supports(Item, IJvDataItemStates,
        ItemState) then
      Result := ItemState.Visible;
  end;
end;

//=== { TJvDP_ProviderBaseRender } ===========================================

constructor TJvDP_ProviderBaseRender.Create(AItem: IJvDataItem; ACanvas: TCanvas; AState: TProviderDrawStates);
begin
  inherited Create;
  FItem := AItem;
  FCanvas := ACanvas;
  FState := AState;
end;

class procedure TJvDP_ProviderBaseRender.Draw(AItem: IJvDataItem; ACanvas: TCanvas; var ARect: TRect; AState: TProviderDrawStates);
begin
  with Self.Create(AItem, ACanvas, AState) do
  try
    Rect := ARect;
    Prepare(False);
    DoDraw;
  finally
    Free;
  end;
end;

class function TJvDP_ProviderBaseRender.Measure(AItem: IJvDataItem; ACanvas: TCanvas; AState: TProviderDrawStates): TSize;
begin
  with Self.Create(AItem, ACanvas, AState) do
  try
    Prepare(True);
    Result := DoMeasure;
  finally
    Free;
  end;
end;

//=== { TJvDP_ProviderTextOnlyRender } =======================================

procedure TJvDP_ProviderTextOnlyRender.Prepare(ForMeasure: Boolean);
var
  TextIntf: IJvDataItemText;
begin
  HasNoText := not Supports(Item, IJvDataItemText, TextIntf);
  if HasNoText then
    FText := RsDataItemRenderHasNoText
  else
    FText := TextIntf.Caption;
end;

procedure TJvDP_ProviderTextOnlyRender.DoDraw;
begin
  Canvas.TextRect(Rect, Rect.Left, Rect.Top, FText);
end;

function TJvDP_ProviderTextOnlyRender.DoMeasure: TSize;
begin
  Result := Canvas.TextExtent(FText);
end;

//=== { TJvDP_ProviderImgAndTextRender } =====================================

procedure TJvDP_ProviderImgAndTextRender.Prepare(ForMeasure: Boolean);
var
  ImgIntf: IJvDataItemImage;
  ImgsIntf: IJvDataItemsImages;
begin
  inherited Prepare(ForMeasure);
  FImageIndex := -1;
  FImages := nil;
  if Supports(Item, IJvDataItemImage, ImgIntf) then
  begin
    FAlignment := ImgIntf.Alignment;
    if DP_FindItemsImages(Item, ImgsIntf) then
    begin
      { We have an item that supports an image and one of it's parents has an imagelist assigned. }
      if (pdsDisabled in State) and (ImgsIntf.DisabledImages <> nil) then
      begin
        FImages := ImgsIntf.DisabledImages;
        FHasDisabledImage := True;
      end
      else
      begin
        FHasDisabledImage := False;
        if (pdsHot in State) and (ImgsIntf.HotImages <> nil) then
          FImages := ImgsIntf.HotImages
        else
          FImages := ImgsIntf.Images;
      end;
      if (pdsSelected in State) and (ImgIntf.SelectedIndex <> -1) then
        FImageIndex := ImgIntf.SelectedIndex
      else
      begin
        FImageIndex := ImgIntf.ImageIndex;
        if FImageIndex < 0 then
          FImageIndex := ImgIntf.SelectedIndex;
      end;
    end;
  end;
  FHasImage := (FImages <> nil) and (FImageIndex > -1);
  if HasImage and HasNoText then
    Text := '';
end;

procedure TJvDP_ProviderImgAndTextRender.DoDraw;
var
  rgn: HRGN;
  iSaveDC: Integer;
  TxtW: Integer;
begin 
  Canvas.Start; 
  rgn := CreateRectRgn(0,0,0,0);
  GetClipRgn(Canvas.Handle, rgn);
  try
    IntersectClipRect(Canvas.Handle, Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
    if HasImage then
    begin
      iSaveDC := SaveDC(Canvas.Handle);
      try
        // Apply alignment rules and render the image
        case Alignment of
          taLeftJustify:
            begin  
              Images.Draw(Canvas, Rect.Left, Rect.Top, ImageIndex, itImage, HasDisabledImage or not (pdsDisabled in State)); 
              Rect.Left := Rect.Left + Images.Width + 2;
            end;
          taRightJustify:
            begin  
              Images.Draw(Canvas, Rect.Right - Images.Width, Rect.Top, ImageIndex, itImage, HasDisabledImage or not (pdsDisabled in State)); 
              Rect.Right := Rect.Right - Images.Width - 2;
            end;
          taCenter:
            begin  
              Images.Draw(Canvas, Rect.Left + ((Rect.Right - Rect.Left - Images.Width) div 2),
                Rect.Top, ImageIndex, itImage, HasDisabledImage or not (pdsDisabled in State)); 
              Rect.Top := Rect.Top + Images.Height + 2;
              TxtW := Canvas.TextWidth(Text);
              Rect.Left := Rect.Left + ((Rect.Right - Rect.Left - TxtW) div 2);
            end;
        end;
      finally
        if iSaveDC <> 0 then
          RestoreDC(Canvas.Handle, iSaveDC);
      end;
    end;
    if pdsGrayed in State then
      Canvas.Font.Color := clGrayText;
    if (pdsDisabled in State) and not (pdsGrayed in State) then
      DisabledTextRect(Canvas, Rect, Rect.Left, Rect.Top, Text)
    else
      Canvas.TextRect(Rect, Rect.Left, Rect.Top, Text);
  finally
    SelectClipRgn(Canvas.Handle, rgn);
    DeleteObject(rgn); 
    Canvas.Stop; 
  end;
end;

function TJvDP_ProviderImgAndTextRender.DoMeasure: TSize;
begin
  if HasImage then
  begin
    // Apply alignment rules and render the image
    case Alignment of
      taLeftJustify,
      taRightJustify:
        begin
          Result := Canvas.TextExtent(Text);
          Inc(Result.cx, Images.Width + 2);
          if Images.Height > Result.cy then
            Result.cy := Images.Height;
        end;
      taCenter:
        begin
          Result := Canvas.TextExtent(Text);
          Inc(Result.cy, Images.Height + 2);
          if Images.Width > Result.cx then
            Result.cx := Images.Width;
        end;
    end;
  end
  else
    Result := inherited DoMeasure;
end;

type
  TReaderAccessProtected = class(TReader);

  {$TYPEINFO ON}
  THackWriter = class(TWriter)
    // (rom) public or protected missing
    function GetPropPath: string;
    function PropPathField: PString;
    procedure SetPropPath(const NewPath: string);
    property PropPath: string read GetPropPath write SetPropPath;
  published
    property RootAncestor;
  end;
  {$IFNDEF TYPEINFO_ON}
  {$TYPEINFO OFF}
  {$ENDIF TYPEINFO_ON}

function THackWriter.GetPropPath: string;
begin
  Result := PropPathField^;
end;

function THackWriter.PropPathField: PString;
var
  RAPI: PPropInfo;
begin
  RAPI := GetPropInfo(THackWriter, 'RootAncestor');
  if RAPI = nil then // Should never happen
    raise EJVCLException.CreateRes(@RsEInternalError);
  Result := Pointer(Cardinal(RAPI.GetProc) and $00FFFFFF + Cardinal(Self) + 4);
end;

procedure THackWriter.SetPropPath(const NewPath: string);
begin
  if NewPath <> PropPath then
    PropPathField^ := NewPath;
end;

//=== { TJvDataItemAggregatedObject } ========================================

function TJvDataItemAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if not GetInterface(IID, Obj) then
  begin
    if IsExtensionSpecificIntf(IID) then
      Result := E_NOINTERFACE
    else
      Result := inherited QueryInterface(IID, Obj);
  end
  else
    Result := S_OK;
end;

procedure TJvDataItemAggregatedObject.ContextDestroying(Context: IJvDataContext);
begin
end;

function TJvDataItemAggregatedObject.Item: IJvDataItem;
begin
  Result := Owner as IJvDataItem;
end;

function TJvDataItemAggregatedObject.ItemImpl: TJvBaseDataItem;
begin
  Result := Owner as TJvBaseDataItem;
end;

//=== { TJvCustomDataItemsTextRenderer } =====================================

procedure TJvCustomDataItemsTextRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Item: IJvDataItem; State: TProviderDrawStates);
begin
  TJvDP_ProviderTextOnlyRender.Draw(Item, ACanvas, ARect, State);
end;

function TJvCustomDataItemsTextRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
begin
  Result := TJvDP_ProviderTextOnlyRender.Measure(Item, ACanvas, []);
end;

function TJvCustomDataItemsTextRenderer.AvgItemSize(ACanvas: TCanvas): TSize;
begin
  Result := ACanvas.TextExtent('WyWyWyWyWyWyWyWyWyWy');
end;

//=== { TJvCustomDataItemsRenderer } =========================================

procedure TJvCustomDataItemsRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect;
  Item: IJvDataItem; State: TProviderDrawStates);
begin
  TJvDP_ProviderImgAndTextRender.Draw(Item, ACanvas, ARect, State);
end;

function TJvCustomDataItemsRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
begin
  Result := TJvDP_ProviderImgAndTextRender.Measure(Item, ACanvas, []);
end;

function TJvCustomDataItemsRenderer.AvgItemSize(ACanvas: TCanvas): TSize;
begin
  Result := ACanvas.TextExtent('WyWyWyWyWyWyWyWyWyWy');
end;

//=== { TJvDataItemTextImpl } ================================================

function TJvDataItemTextImpl.GetCaption: string;
begin
  Result := FCaption;
end;

function TJvDataItemTextImpl.Editable: Boolean;
begin
  Result := True;
end;

procedure TJvDataItemTextImpl.SetCaption(const Value: string);
begin
  if Caption <> Value then
  begin
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FCaption := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
  end;
end;

//=== { TJvDataItemContextTextImpl } =========================================

constructor TJvDataItemContextTextImpl.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FContextStrings := TStringList.Create;
end;

destructor TJvDataItemContextTextImpl.Destroy;
begin
  FreeAndNil(FContextStrings);
  inherited Destroy;
end;

function TJvDataItemContextTextImpl.GetCaption: string;
var
  CurCtx: IJvDataContext;
begin
  CurCtx := Item.GetItems.Provider.SelectedContext;
  while (CurCtx <> nil) and (FContextStrings.IndexOfObject(TObject(CurCtx)) = -1) do
    CurCtx := CurCtx.Contexts.Ancestor;
  if (CurCtx <> nil) and (FContextStrings.IndexOfObject(TObject(CurCtx)) > -1) then
    Result := FContextStrings[FContextStrings.IndexOfObject(TObject(CurCtx))]
  else
    Result := inherited GetCaption;
end;

function TJvDataItemContextTextImpl.Editable: Boolean;
begin
  Result := True;
end;

procedure TJvDataItemContextTextImpl.SetCaption(const Value: string);
var
  CurCtx: IJvDataContext;
  I: Integer;
begin
  CurCtx := Item.GetItems.Provider.SelectedContext;
  if CurCtx <> nil then
  begin
    if Caption <> Value then
    begin
      Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
      I := FContextStrings.IndexOfObject(TObject(CurCtx));
      if I > -1 then
        FContextStrings[I] := Value
      else
        FContextStrings.AddObject(Value, TObject(CurCtx));
      Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
    end;
  end
  else
    inherited SetCaption(Value);
end;

procedure TJvDataItemContextTextImpl.RevertToAncestor;
var
  CurCtx: IJvDataContext;
  I: Integer;
begin
  CurCtx := Item.GetItems.Provider.SelectedContext;
  if CurCtx <> nil then
  begin
    I := FContextStrings.IndexOfObject(TObject(CurCtx));
    if I > -1 then
    begin
      Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
      FContextStrings.Delete(I);
      Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
    end;
  end;
end;

function TJvDataItemContextTextImpl.IsEqualToAncestor: Boolean;
var
  CurCtx: IJvDataContext;
begin
  CurCtx := Item.GetItems.Provider.SelectedContext;
  Result := FContextStrings.IndexOfObject(TObject(CurCtx)) = -1;
end;

//=== { TJvDataItemImageImpl } ===============================================

function TJvDataItemImageImpl.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TJvDataItemImageImpl.SetAlignment(Value: TAlignment);
begin
  if GetAlignment <> Value then
  begin
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FAlignment := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
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
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FImageIndex := Index;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
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
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FSelectedIndex := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
  end;
end;

//=== { TExtensibleInterfacedPersistent } ====================================

constructor TExtensibleInterfacedPersistent.Create;
begin
  inherited Create;
  FAdditionalIntfImpl := TList.Create;
end;

destructor TExtensibleInterfacedPersistent.Destroy;
begin
  ClearIntfImpl;
  FreeAndNil(FAdditionalIntfImpl);
  inherited Destroy;
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
  if RefCount <> 0 then
    RunError(2);
  inherited BeforeDestruction;
end;

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

function TExtensibleInterfacedPersistent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TExtensibleInterfacedPersistent.AddIntfImpl(const Obj: TAggregatedPersistentEx);
begin
  if IndexOfImplClass(TAggregatedPersistentExClass(Obj.ClassType)) >= 0 then
    raise EJVCLException.CreateRes(@RsEExtensibleIntObjDuplicateClass);
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
  Obj: TObject;
begin
  for I := FAdditionalIntfImpl.Count - 1 downto 0 do
  begin
    Obj := TObject(FAdditionalIntfImpl[I]);
    FAdditionalIntfImpl[I] := nil;
    Obj.Free;
    FAdditionalIntfImpl.Delete(I);
  end;
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
  Filer.DefineProperty('Extensions', ReadImplementers, WriteImplementers, I >= 0);
end;

procedure TExtensibleInterfacedPersistent.ReadImplementers(Reader: TReader);
begin
  { When loading implementers the interface of this object may be referenced. We don't want the
    instance destroyed yet, so reference counting will be suspended (by incrementing it) and resumed
    when we're done (by decrementing it without checking if it became zero) }
  SuspendRefCount;
  try
    if Reader.ReadValue <> vaCollection then
      raise EReadError.CreateRes(@RsEExtensibleIntObjCollectionExpected);
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
  SavePropPath: string;
begin
  THackWriter(Writer).WriteValue(vaCollection);
  SavePropPath := THackWriter(Writer).PropPath;
  THackWriter(Writer).PropPath := '';
  try
    for I := 0 to FAdditionalIntfImpl.Count - 1 do
      if IsStreamableExtension(TAggregatedPersistentEx(FAdditionalIntfImpl[I])) then
        WriteImplementer(Writer, TAggregatedPersistentEx(FAdditionalIntfImpl[I]));
    Writer.WriteListEnd;
  finally
    THackWriter(Writer).PropPath := SavePropPath;
  end;
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
  if not AnsiSameText(ClassName, cClassName) then
    raise EReadError.CreateRes(@RsEExtensibleIntObjClassNameExpected);
  ClassName := Reader.ReadString;
  ClassType := FindClass(ClassName);
  if not ClassType.InheritsFrom(TAggregatedPersistentEx) then
    raise EReadError.CreateRes(@RsEExtensibleIntObjInvalidClass);
  I := IndexOfImplClass(TAggregatedPersistentExClass(ClassType));
  if I >= 0 then
    Impl := TAggregatedPersistentEx(FAdditionalIntfImpl[I])
  else
    Impl := TAggregatedPersistentExClass(ClassType).Create(Self);
  while not Reader.EndOfList do
    TReaderAccessProtected(Reader).ReadProperty(Impl);
  Reader.ReadListEnd;
end;

procedure TExtensibleInterfacedPersistent.WriteImplementer(Writer: TWriter;
  Instance: TAggregatedPersistentEx);
begin
  Writer.WriteListBegin;
  THackWriter(Writer).WritePropName(cClassName);
  Writer.WriteString(Instance.ClassName);
  THackWriter(Writer).WriteProperties(Instance);
  Writer.WriteListEnd;
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

//=== { TAggregatedPersistent } ==============================================

constructor TAggregatedPersistent.Create(Controller: IUnknown);
begin
  inherited Create;
  FController := Pointer(Controller);
end;

function TAggregatedPersistent.GetController: IUnknown;
begin
  Result := IUnknown(FController);
end;

function TAggregatedPersistent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
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

function TAggregatedPersistent.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
end;

//=== { TAggregatedPersistentEx } ============================================

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

//=== { TJvProviderNotification } ============================================

destructor TJvProviderNotification.Destroy;
begin
  Provider := nil;
  inherited Destroy;
end;

procedure TJvProviderNotification.SetProvider(Value: IJvDataProvider);
begin
  if Value <> Provider then
  begin
    if Provider <> nil then
      Provider.UnregisterChangeNotify(Self);
    FProvider := Value;
    if Provider <> nil then
      Provider.RegisterChangeNotify(Self);
  end;
end;

function TJvProviderNotification._AddRef: Integer;
begin
  Result := -1;
end;

function TJvProviderNotification._Release: Integer;
begin
  Result := -1;
end;

function TJvProviderNotification.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

procedure TJvProviderNotification.DataProviderChanging(const ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if (AReason = pcrDestroy) and (Provider <> nil) then
  begin
    Provider.UnregisterChangeNotify(Self);
    FProvider := nil;
  end;
  if Assigned(FOnChanging) then
    FOnChanging(ADataProvider, AReason, Source);
end;

procedure TJvProviderNotification.DataProviderChanged(const ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if Assigned(FOnChanged) then
    FOnChanged(ADataProvider, AReason, Source);
end;

function TJvProviderNotification.Consumer: IJvDataConsumer;
begin
  Result := nil;
end;

//=== { TJvBaseDataItems } ===================================================

constructor TJvBaseDataItems.Create;
begin
  inherited Create;
end;

constructor TJvBaseDataItems.Create(const Provider: IJvDataProvider);
begin
  Create;
  FProvider := Provider;
end;

constructor TJvBaseDataItems.Create(const Parent: IJvDataItem);
begin
  Create(Parent.GetItems.Provider);
  FParent := Pointer(Parent);
  if (Parent <> nil) and Parent.GetItems.IsDynamic then
    FParentIntf := Parent;
  if (Parent <> nil) and (Parent.GetImplementer is TExtensibleInterfacedPersistent) then
    FSubAggregate := TJvBaseDataItemSubItems.Create(
      TExtensibleInterfacedPersistent(Parent.GetImplementer), Self);
end;

procedure TJvBaseDataItems.ItemAdd(Item: IJvDataItem);
begin
  GetProvider.Changing(pcrAdd, Self);
  InternalAdd(Item);
  GetProvider.Changed(pcrAdd, Item);
end;

procedure TJvBaseDataItems.ItemDelete(Index: Integer);
var
  Item: IJvDataItem;
begin
  Item := GetItem(Index);
  if (Item <> nil) and (Item.IsDeletable) then
  begin
    GetProvider.Changing(pcrDelete, Item);
    Item := nil;
    InternalDelete(Index);
    GetProvider.Changed(pcrDelete, Self);
  end;
end;

procedure TJvBaseDataItems.ItemMove(OldIndex, NewIndex: Integer);
begin
  if OldIndex <> NewIndex then
  begin
    if (NewIndex <= GetCount) and (NewIndex >= 0) then
    begin
      if GetProvider.SelectedContext <> nil then
      begin
        GetProvider.Changing(pcrUpdateItems, Self);
        InternalMove(OldIndex, NewIndex);
        GetProvider.Changed(pcrUpdateItems, Self);
      end
      else
        raise EJVCLDataItems.CreateRes(@RsEItemsMayNotBeMovedInTheMainTree);
    end
    else
      raise EJVCLDataItems.CreateRes(@RsEInvalidIndex);
  end;
end;

function TJvBaseDataItems.IsStreamableItem(Item: IJvDataItem): Boolean;
var
  AClass: TPersistentClass;
begin
  AClass := GetClass(Item.GetImplementer.ClassName);
  Result := (AClass <> nil) and AClass.InheritsFrom(TJvBaseDataItem);
end;

function TJvBaseDataItems.ScanForID(Items: IJvDataItems; ID: string; Recursive: Boolean): IJvDataItem;
var
  I: Integer;
  SubItems: IJvDataItems;
begin
  if (Items <> nil) then
  begin
    Result := Items.GetItemByID(ID);
    if (Result = nil) and Recursive then
    begin
      I := Items.GetCount - 1;
      while (I >= 0) and (Result = nil) do
      begin
        if Supports(Items.GetItem(I), IJvDataItems, SubItems) then
          Result := ScanForID(SubItems, ID, True);
        Dec(I);
      end;
    end;
  end;
end;

procedure TJvBaseDataItems.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Items', ReadItems, WriteItems, True);
end;

procedure TJvBaseDataItems.ReadItems(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.CreateRes(@RsEExtensibleIntObjCollectionExpected);
  while not Reader.EndOfList do
    ReadItem(Reader);
  Reader.ReadListEnd;
end;

procedure TJvBaseDataItems.WriteItems(Writer: TWriter);
var
  I: Integer;
  SavePropPath: string;
begin
  THackWriter(Writer).WriteValue(vaCollection);
  SavePropPath := THackWriter(Writer).PropPath;
  THackWriter(Writer).PropPath := '';
  try
    for I := 0 to GetCount - 1 do
    begin
      if IsStreamableItem(GetItem(I)) then
        WriteItem(Writer, GetItem(I));
    end;
    Writer.WriteListEnd;
  finally
    THackWriter(Writer).PropPath := SavePropPath;
  end;
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
  if not AnsiSameText(PropName, cClassName) then
    raise EReadError.CreateRes(@RsEExtensibleIntObjClassNameExpected);
  ClassName := Reader.ReadString;
  PerstClass := FindClass(ClassName);
  if not PerstClass.InheritsFrom(TJvBaseDataItem) then
    raise EReadError.CreateRes(@RsEExtensibleIntObjInvalidClass);
  ItemClass := TJvBaseDataItemClass(PerstClass);
  ItemInstance := ItemClass.Create(Self);
  try
    InternalAdd(ItemInstance);
  except
    ItemInstance.Free;
    raise;
  end;
  while not Reader.EndOfList do
    TReaderAccessProtected(Reader).ReadProperty(ItemInstance);
  Reader.ReadListEnd;
end;

procedure TJvBaseDataItems.WriteItem(Writer: TWriter; Item: IJvDataItem);
var
  Inst: TPersistent;
begin
  Writer.WriteListBegin;
  Inst := TPersistent(Item.GetImplementer);
  Writer.WriteStr(cClassName);
  Writer.WriteString(Inst.ClassName);
  THackWriter(Writer).WriteProperties(Inst);
  Writer.WriteListEnd;
end;

function TJvBaseDataItems.GetItemByID(ID: string): IJvDataItem;
var
  CurItems: IJvDataItems;
  PathSep: Integer;
  PathSep2: Integer;
  ThisPath: string;
  Idx: Integer;
begin
  CurItems := Self;
  while (CurItems <> nil) and (Result = nil) and (ID <> '') do
  begin
    PathSep := Pos('\', ID);
    PathSep2 := Pos('/', ID);
    if (PathSep > PathSep2) or (PathSep = 0) then
      PathSep := PathSep2;
    if PathSep = 0 then
      PathSep := Length(ID) + 1;
    ThisPath := Copy(ID, 1, PathSep - 1);
    if ThisPath = '..' then
    begin
      if GetParent <> nil then
        CurItems := GetParent.GetItems
      else
        CurItems := nil;
    end
    else
    if (ThisPath = '') and (GetParent <> nil) and (PathSep <> 0) then
      CurItems := GetProvider.GetItems
    else
    begin
      Idx := CurItems.GetCount - 1;
      while (Idx >= 0) and not AnsiSameText(CurItems.GetItem(Idx).GetID, ThisPath) do
        Dec(Idx);
      Delete(ID, 1, PathSep);
      if Idx >= 0 then
      begin
        if ID = '' then
          Result := CurItems.GetItem(Idx)
        else
          Supports(CurItems.GetItem(Idx), IJvDataItems, CurItems);
      end;
    end;
  end;
end;

function TJvBaseDataItems.GetItemByIndexPath(IndexPath: array of Integer): IJvDataItem;
var
  Idx: Integer;
  ItemList: IJvDataItems;
begin
  if Length(IndexPath) > 0 then
  begin
    ItemList := Self;
    Idx := 0;
    while (Idx < Length(IndexPath)) do
    begin
      Supports(ItemList.GetItem(IndexPath[Idx]), IJvDataItems, ItemList);
      Inc(Idx);
    end;
    Result := ItemList.GetParent;
  end;
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

procedure TJvBaseDataItems.ContextDestroying(Context: IJvDataContext);
var
  I: Integer;
begin
  for I := 0 to FAdditionalIntfImpl.Count - 1 do
    TJvDataItemsAggregatedObject(FAdditionalIntfImpl[I]).ContextDestroying(Context);
  for I := 0 to GetCount - 1 do
    GetItem(I).ContextDestroying(Context);
end;

function TJvBaseDataItems.FindByID(ID: string; const Recursive: Boolean): IJvDataItem;
begin
  Result := ScanForID(Self, ID, Recursive);
end;

procedure TJvBaseDataItems.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FSubAggregate <> nil then
    FreeAndNil(FSubAggregate);
end;

//=== { TJvBaseDataItemSubItems } ============================================

constructor TJvBaseDataItemSubItems.Create(AOwner: TExtensibleInterfacedPersistent;
  AItems: TJvBaseDataItems);
begin
  inherited Create(AOwner);
  FItems := AItems;
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

//=== { TJvCustomDataItemTextRenderer } ======================================

procedure TJvCustomDataItemTextRenderer.Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates);
begin
  TJvDP_ProviderTextOnlyRender.Draw(Item, ACanvas, ARect, State);
end;

function TJvCustomDataItemTextRenderer.Measure(ACanvas: TCanvas): TSize;
begin
  Result := TJvDP_ProviderTextOnlyRender.Measure(Item, ACanvas, []);
end;

//=== { TJvCustomDataItemRenderer } ==========================================

procedure TJvCustomDataItemRenderer.Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates);
begin
  TJvDP_ProviderImgAndTextRender.Draw(Item, ACanvas, ARect, State);
end;

function TJvCustomDataItemRenderer.Measure(ACanvas: TCanvas): TSize;
begin
  Result := TJvDP_ProviderImgAndTextRender.Measure(Item, ACanvas, []);
end;

//=== { TJvCustomDataItemStates } ============================================

procedure TJvCustomDataItemStates.InitStatesUsage(UseEnabled, UseChecked, UseVisible: Boolean);
begin
  if UseEnabled then
    FEnabled := disTrue
  else
    FEnabled := disNotUsed;
  if UseChecked then
    FChecked := disFalse
  else
    FChecked := disNotUsed;
  if UseVisible then
    FVisible := disTrue
  else
    FVisible := disNotUsed;
end;

function TJvCustomDataItemStates.Get_Enabled: TDataItemState;
begin
  Result := FEnabled;
end;

procedure TJvCustomDataItemStates.Set_Enabled(Value: TDataItemState);
begin
  if Value = disNotUsed then
    Exit;
  if Value <> Get_Enabled then
  begin
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FEnabled := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
  end;
end;

function TJvCustomDataItemStates.Get_Checked: TDataItemState;
begin
  Result := FChecked;
end;

procedure TJvCustomDataItemStates.Set_Checked(Value: TDataItemState);
begin
  if Value = disNotUsed then
    Exit;
  if Value <> Get_Checked then
  begin
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FChecked := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
  end;
end;

function TJvCustomDataItemStates.Get_Visible: TDataItemState;
begin
  Result := FVisible;
end;

procedure TJvCustomDataItemStates.Set_Visible(Value: TDataItemState);
begin
  if Value = disNotUsed then
    Exit;
  if Value <> Get_Visible then
  begin
    Item.GetItems.Provider.Changing(pcrUpdateItem, Item);
    FVisible := Value;
    Item.GetItems.Provider.Changed(pcrUpdateItem, Item);
  end;
end;

//=== { TJvDataItemsAggregatedObject } =======================================

procedure TJvDataItemsAggregatedObject.ContextDestroying(Context: IJvDataContext);
begin
end;

function TJvDataItemsAggregatedObject.Items: IJvDataItems;
begin
  Result := Owner as IJvDataItems;
end;

function TJvDataItemsAggregatedObject.ItemsImpl: TJvBaseDataItems;
begin
  Result := Owner as TJvBaseDataItems;
end;

//=== { TJvBaseDataItemsRenderer } ===========================================

procedure TJvBaseDataItemsRenderer.DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect;
  Index: Integer; State: TProviderDrawStates);
begin
  if (Index < 0) or (Index >= Items.Count) then
    raise EJVCLDataItems.CreateResFmt(@SListIndexError, [Index]);
  DrawItem(ACanvas, ARect, Items.Items[Index], State);
end;

function TJvBaseDataItemsRenderer.MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
begin
  if Index = -1 then
    Result := AvgItemSize(ACanvas)
  else
  begin
    if (Index < 0) or (Index >= Items.Count) then
      raise EJVCLDataItems.CreateResFmt(@SListIndexError, [Index]);
    Result := MeasureItem(ACanvas, Items.Items[Index]);
  end;
end;

procedure TJvBaseDataItemsRenderer.DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
  State: TProviderDrawStates);
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

//=== { TJvDataItemsList } ===================================================

constructor TJvDataItemsList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TJvDataItemsList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

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

//=== { TJvBaseDataItemsListManagement } =====================================

function TJvBaseDataItemsListManagement.Add(Item: IJvDataItem): IJvDataItem;
begin
  Items.Provider.Changing(pcrAdd, Items);
  TJvDataItemsList(ItemsImpl).List.Add(Item.GetImplementer);
  Result := Item;
  Items.Provider.Changed(pcrAdd, Result);
end;

procedure TJvBaseDataItemsListManagement.Clear;
begin
  Items.Provider.Changing(pcrUpdateItems, Items);
  TJvDataItemsList(ItemsImpl).List.Clear;
  Items.Provider.Changed(pcrUpdateItems, Items);
end;

procedure TJvBaseDataItemsListManagement.Delete(Index: Integer);
begin
  if (Items.GetItem(Index) <> nil) and Items.GetItem(Index).IsDeletable then
  begin
    Items.Provider.Changing(pcrDelete, Items.GetItem(Index));
    TJvDataItemsList(ItemsImpl).List.Delete(Index);
    Items.Provider.Changed(pcrDelete, nil);
  end
  else
  if Items.GetItem(Index) <> nil then
    raise EJVCLDataItems.CreateRes(@RsEItemCanNotBeDeleted);
end;

procedure TJvBaseDataItemsListManagement.Remove(var Item: IJvDataItem);
var
  Impl: TObject;
begin
  if (Item <> nil) and Item.IsDeletable then
  begin
    Impl := Item.GetImplementer;
    Pointer(Item) := nil;
    if (Impl is TExtensibleInterfacedPersistent) and
        (TExtensibleInterfacedPersistent(Impl).RefCount = 0) then
    begin
      TExtensibleInterfacedPersistent(Impl).SuspendRefCount;
      try
        Item := TExtensibleInterfacedPersistent(Impl) as IJvDataItem;
        try
          Items.Provider.Changing(pcrDelete, Item);
        finally
          Pointer(Item) := nil;
        end;
      finally
        TExtensibleInterfacedPersistent(Impl).ResumeRefCount;
      end;
      TJvDataItemsList(ItemsImpl).List.Remove(Impl);
      Items.Provider.Changed(pcrDelete, nil);
    end;
  end
  else
  if Item <> nil then
    raise EJVCLDataItems.CreateRes(@RsEItemCanNotBeDeleted);
end;

//=== { TJvCustomDataItemsImages } ===========================================

function TJvCustomDataItemsImages.GetDisabledImages: TCustomImageList;
begin
  Result := FDisabledImages;
end;

procedure TJvCustomDataItemsImages.SetDisabledImages(const Value: TCustomImageList);
begin
  if Value <> GetDisabledImages then
  begin
    (Owner as IJvDataItems).Provider.Changing(pcrUpdateItems, Items);
    FDisabledImages := Value;
    (Owner as IJvDataItems).Provider.Changed(pcrUpdateItems, Items);
  end;
end;

function TJvCustomDataItemsImages.GetHotImages: TCustomImageList;
begin
  Result := FHotImages;
end;

procedure TJvCustomDataItemsImages.SetHotImages(const Value: TCustomImageList);
begin
  if Value <> GetHotImages then
  begin
    (Owner as IJvDataItems).Provider.Changing(pcrUpdateItems, Items);
    FHotImages := Value;
    (Owner as IJvDataItems).Provider.Changed(pcrUpdateItems, Items);
  end;
end;

function TJvCustomDataItemsImages.GetImages: TCustomImageList;
begin
  Result := FImages;
end;

procedure TJvCustomDataItemsImages.SetImages(const Value: TCustomImageList);
begin
  if Value <> GetImages then
  begin
    (Owner as IJvDataItems).Provider.Changing(pcrUpdateItems, Items);
    FImages := Value;
    (Owner as IJvDataItems).Provider.Changed(pcrUpdateItems, Items);
  end;
end;

//=== { TJvBaseDataItem } ====================================================

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
  GetItems.GetProvider.SelectContext(nil);
  try
    if GetItems.IsDynamic then
      Result := inherited _AddRef
    else
      Result := -1;
  finally
    GetItems.GetProvider.ReleaseContext;
  end;
end;

function TJvBaseDataItem._Release: Integer;
var
  NeedsRelease: Boolean;
begin
  GetItems.GetProvider.SelectContext(nil);
  try
    NeedsRelease := GetItems.IsDynamic;
  finally
    GetItems.GetProvider.ReleaseContext;
  end;
  if NeedsRelease then
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
      raise EReadError.CreateRes(@RsEExtensibleIntObjCollectionExpected);
    Reader.ReadListBegin;
    PropName := Reader.ReadStr;
    if not AnsiSameText(PropName, cClassName) then
      raise EReadError.CreateRes(@RsEExtensibleIntObjClassNameExpected);
    ClassName := Reader.ReadString;
    AClass := FindClass(ClassName);
    if not AClass.InheritsFrom(TJvBaseDataItems) then
      raise EReadError.CreateRes(@RsEExtensibleIntObjInvalidClass);
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
      TJvDataItemsClass(AClass).Create(Self);
      I := IndexOfImplClass(TJvBaseDataItemSubItems);
    end;
    while not Reader.EndOfList do
      TReaderAccessProtected(Reader).ReadProperty(
        TJvBaseDataItems(TJvBaseDataItemSubItems(FAdditionalIntfImpl[I]).Items.GetImplementer));
    Reader.ReadListEnd;
    Reader.ReadListEnd;
  finally
    ResumeRefCount;
  end;
end;

procedure TJvBaseDataItem.WriteSubItems(Writer: TWriter);
var
  Items: IJvDataItems;
  SavePropPath: string;
begin
  QueryInterface(IJvDataItems, Items);
  THackWriter(Writer).WriteValue(vaCollection);
  SavePropPath := THackWriter(Writer).PropPath;
  THackWriter(Writer).PropPath := '';
  try
    Writer.WriteListBegin;
    Writer.WriteStr(cClassName);
    Writer.WriteString(Items.GetImplementer.ClassName);
    THackWriter(Writer).WriteProperties(Items.GetImplementer as TPersistent);
    Writer.WriteListEnd;
    Writer.WriteListEnd;
  finally
    THackWriter(Writer).PropPath := SavePropPath;
  end;
end;

function TJvBaseDataItem.GetItems: IJvDataItems;
begin
  Result := IJvDataItems(FItems);
end;

function TJvBaseDataItem.GetIndex: Integer;
var
  Owner: IJvDataItems;
begin
  Owner := GetItems;
  Result := Owner.GetCount - 1;
  while (Result >= 0) and (Owner.GetItem(Result) <> Self as IJvDataItem) do
    Dec(Result);
end;

function TJvBaseDataItem.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseDataItem.GetID: string;
begin
  Result := FID;
end;

procedure TJvBaseDataItem.ContextDestroying(Context: IJvDataContext);
var
  I: Integer;
  SubItems: IJvDataItems;
begin
  for I := 0 to FAdditionalIntfImpl.Count - 1 do
    TJvDataItemAggregatedObject(FAdditionalIntfImpl[I]).ContextDestroying(Context);
  if Supports(Self as IJvDataItem, IJvDataItems, SubItems) then
    SubItems.ContextDestroying(Context);
end;

function TJvBaseDataItem.IsParentOf(AnItem: IJvDataItem; DirectParent: Boolean): Boolean;
begin
  Result := AnItem.GetItems.Parent = (Self as IJvDataItem);
  if not Result and not DirectParent then
  begin
    AnItem := AnItem.GetItems.Parent;
    while (AnItem <> nil) and (AnItem <> (Self as IJvDataItem)) do
      AnItem := AnItem.GetItems.Parent;
    Result := AnItem = (Self as IJvDataItem);
  end;
end;

function TJvBaseDataItem.IsDeletable: Boolean;
begin
  Result := True;
end;

procedure TJvBaseDataItem.RevertToAncestor;
var
  I: Integer;
  Inst: TJvDataItemAggregatedObject;
  CtxSens: IJvDataContextSensitive;
begin
  for I := 0 to FAdditionalIntfImpl.Count - 1 do
  begin
    Inst := TJvDataItemAggregatedObject(FAdditionalIntfImpl[I]);
    if Inst.GetInterface(IJvDataContextSensitive, CtxSens) then
      CtxSens.RevertToAncestor;
  end;
end;

function TJvBaseDataItem.IsEqualToAncestor: Boolean;
var
  I: Integer;
  Inst: TJvDataItemAggregatedObject;
  CtxSens: IJvDataContextSensitive;
begin
  Result := True;
  I := 0;
  while Result and (I < FAdditionalIntfImpl.Count) do
  begin
    Inst := TJvDataItemAggregatedObject(FAdditionalIntfImpl[I]);
    if Inst.GetInterface(IJvDataContextSensitive, CtxSens) then
      Result := CtxSens.IsEqualToAncestor;
    Inc(I);
  end;
end;

//=== { TJvCustomDataProvider } ==============================================

constructor TJvCustomDataProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotifiers := TInterfaceList.Create;
  FConsumerStack := TInterfaceList.Create;
  FContextStack := TInterfaceList.Create;
  if ContextsClass <> nil then
  begin
    FDataContextsImpl := ContextsClass.Create(Self, nil, ContextsManagerClass);
    FDataContextsIntf := FDataContextsImpl;
  end;
  if ItemsClass <> nil then
    FDataItems := ItemsClass.Create(Self)
  else
    raise EJVCLDataProvider.CreateRes(@RsEDataProviderNeedsItemsImpl);
end;

destructor TJvCustomDataProvider.Destroy;
begin
  FreeAndNil(FNotifiers);
  FreeAndNil(FConsumerStack);
  FreeAndNil(FContextStack);
  inherited Destroy;
end;

procedure TJvCustomDataProvider.BeforeDestruction;
begin
  inherited BeforeDestruction;
  Changing(pcrDestroy);
end;

function TJvCustomDataProvider.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
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
  for I := FNotifiers.Count - 1 downto 0 do
    (FNotifiers[I] as IJvDataProviderNotify).DataProviderChanging(Self, ChangeReason, Source);
  if ChangeReason = pcrContextDelete then
    ContextDestroying(IJvDataContext(Source));
end;

procedure TJvCustomDataProvider.Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown);
var
  I: Integer;
begin
  for I := FNotifiers.Count - 1 downto 0 do
    (FNotifiers[I] as IJvDataProviderNotify).DataProviderChanged(Self, ChangeReason, Source);
  if ChangeReason = pcrContextAdd then
    ContextAdded(IJvDataContext(Source));
end;

class function TJvCustomDataProvider.PersistentDataItems: Boolean;
begin
  Result := False;
end;

class function TJvCustomDataProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvDataItemsList;
end;

class function TJvCustomDataProvider.ContextsClass: TJvDataContextsClass;
begin
  Result := nil;
end;

class function TJvCustomDataProvider.ContextsManagerClass: TJvDataContextsManagerClass;
begin
  Result := nil;
end;

procedure TJvCustomDataProvider.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  if (ContextsClass <> nil) and (ContextsManagerClass <> nil) then
    Filer.DefineProperty('ContextList', ReadContexts, WriteContexts, True);
  if PersistentDataItems then
    Filer.DefineProperty('Root', ReadRoot, WriteRoot, True);
end;

procedure TJvCustomDataProvider.ReadRoot(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.CreateRes(@RsEExtensibleIntObjCollectionExpected);
  Reader.ReadListBegin;
  // We don't really have a root item; just stream in the DataItemsImpl instance.
  while not Reader.EndOfList do
    TReaderAccessProtected(Reader).ReadProperty(DataItemsImpl);
  // (rom) why twice? Please comment.
  Reader.ReadListEnd;
  Reader.ReadListEnd;
end;

procedure TJvCustomDataProvider.WriteRoot(Writer: TWriter);
begin
  THackWriter(Writer).WriteValue(vaCollection);
  Writer.WriteListBegin;
  // We don't really have a root item; just stream out the DataItemsImpl instance.
  THackWriter(Writer).WriteProperties(DataItemsImpl);
  // (rom) why twice? Please comment.
  Writer.WriteListEnd;
  Writer.WriteListEnd;
end;

procedure TJvCustomDataProvider.ReadContexts(Reader: TReader);
var
  I: Integer;
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.CreateRes(@RsEExtensibleIntObjCollectionExpected);
  for I := DataContextsImpl.GetCount - 1 downto 0 do
    if DataContextsImpl.GetContext(I).IsDeletable then
      DataContextsImpl.DoDeleteContext(I);
  I := 0;
  while not Reader.EndOfList do
  begin
    ReadContext(Reader, I);
    Inc(I);
  end;
  Reader.ReadListEnd;
end;

procedure TJvCustomDataProvider.WriteContexts(Writer: TWriter);
var
  I: Integer;
begin
  THackWriter(Writer).WriteValue(vaCollection);
  for I := 0 to FDataContextsImpl.GetCount - 1 do
    if not FDataContextsImpl.GetContext(I).IsDeletable and
        TJvBaseDataContext(FDataContextsImpl.GetContext(I).GetImplementer).IsStreamable then
      WriteContext(Writer, FDataContextsImpl.GetContext(I));
  for I := 0 to FDataContextsImpl.GetCount - 1 do
    if FDataContextsImpl.GetContext(I).IsDeletable then
      WriteContext(Writer, FDataContextsImpl.GetContext(I));
  Writer.WriteListEnd;
end;

procedure TJvCustomDataProvider.ReadContext(Reader: TReader; Index: Integer);
var
  ClassName: string;
  ClassType: TClass;
  CtxName: string;
  CtxInst: TJvBaseDataContext;
begin
  Reader.ReadListBegin;
  ClassType := nil;
  if Index >= DataContextsImpl.GetCount then
  begin
    ClassName := Reader.ReadStr;
    if not AnsiSameText(ClassName, cClassName) then
      raise EReadError.CreateRes(@RsEExtensibleIntObjClassNameExpected);
    ClassName := Reader.ReadString;
    ClassType := FindClass(ClassName);
    if not ClassType.InheritsFrom(TJvBaseDataContext) then
      raise EReadError.CreateRes(@RsEExtensibleIntObjInvalidClass);
  end;
  CtxName := Reader.ReadStr;
  if not AnsiSameText(CtxName, cName) then
    raise EReadError.CreateRes(@RsEContextNameExpected);
  CtxName := Reader.ReadString;
  if Index >= DataContextsImpl.GetCount then
  begin
    CtxInst := TJvDataContextClass(ClassType).Create(FDataContextsImpl, CtxName);
    try
      FDataContextsImpl.DoAddContext(CtxInst);
    except
      CtxInst.Free;
      raise;
    end;
  end
  else
    CtxInst := TJvBaseDataContext(DataContextsImpl.GetContextByName(CtxName).GetImplementer);
  while not Reader.EndOfList do
    TReaderAccessProtected(Reader).ReadProperty(CtxInst);
  Reader.ReadListEnd;
end;

procedure TJvCustomDataProvider.WriteContext(Writer: TWriter; AContext: IJvDataContext);
begin
  Writer.WriteListBegin;
  if AContext.IsDeletable then
  begin
    Writer.WriteStr(cClassName);
    Writer.WriteString(AContext.GetImplementer.ClassName);
  end;
  Writer.WriteStr(cName);
  Writer.WriteString(AContext.Name);
  THackWriter(Writer).WriteProperties(TPersistent(AContext.GetImplementer));
  Writer.WriteListEnd;
end;

procedure TJvCustomDataProvider.AddToArray(var ClassArray: TClassArray; AClass: TClass);
begin
  SetLength(ClassArray, Length(ClassArray) + 1);
  ClassArray[High(ClassArray)] := AClass;
end;

procedure TJvCustomDataProvider.DeleteFromArray(var ClassArray: TClassArray; Index: Integer);
begin
  if (Index >= 0) and (Index <= High(ClassArray)) then
  begin
    if Index < High(ClassArray) then
      Move(ClassArray[Index + 1], ClassArray[Index], SizeOf(TClass) * (High(ClassArray) - Index));
    SetLength(ClassArray, High(ClassArray));
  end;
end;

function TJvCustomDataProvider.IndexOfClass(AClassArray: TClassArray; AClass: TClass): Integer;
begin
  Result := High(AClassArray);
  while (Result >= 0) and (AClassArray[Result] <> AClass) do
    Dec(Result);
end;

procedure TJvCustomDataProvider.RemoveFromArray(var ClassArray: TClassArray; AClass: TClass);
var
  I: Integer;
begin
  I := IndexOfClass(ClassArray, AClass);
  if I > -1 then
    DeleteFromArray(ClassArray, I);
end;

function TJvCustomDataProvider.IsTreeProvider: Boolean;
var
  I: Integer;
  Obj: IJvDataItems;
begin
  I := GetItems.Count - 1;
  while (I >= 0) and not Supports(GetItems.GetItem(I), IJvDataItems, Obj) do
    Dec(I);
  Result := I >= 0;
end;

function TJvCustomDataProvider.GetDataItemsImpl: TJvBaseDataItems;
begin
  if FDataItems <> nil then
    Result := TJvBaseDataItems(FDataItems.GetImplementer)
  else
    Result := nil;
end;



function TJvCustomDataProvider.GetItems: IJvDataItems;
begin
  Result := FDataItems;
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

function TJvCustomDataProvider.ConsumerClasses: TClassArray;
var
  Obj: IUnknown;
begin
  SetLength(Result, 0);

  // Generic provider based extensions
  if Supports(Self as IJvDataProvider, IJvDataContexts, Obj) then
    AddToArray(Result, TJvDataConsumerContext);

  // Consumer based extensions
  if SelectedConsumer <> nil then
  begin
    // Generic consumer based extensions
    if SelectedConsumer.AttributeApplies(DPA_RendersSingleItem) or IsTreeProvider then
      AddToArray(Result, TJvDataConsumerItemSelect);
    if SelectedConsumer.AttributeApplies(DPA_ConsumerDisplaysList) then
      AddToArray(Result, TJvDataConsumerViewList);
  end;
end;

procedure TJvCustomDataProvider.SelectConsumer(Consumer: IJvDataConsumer);
begin
  if FConsumerStack <> nil then
    FConsumerStack.Insert(0, Consumer);
end;

function TJvCustomDataProvider.SelectedConsumer: IJvDataConsumer;
begin
  if (FConsumerStack <> nil) and (FConsumerStack.Count > 0) then
    Result := IJvDataConsumer(FConsumerStack[0])
  else
    Result := nil;
end;

procedure TJvCustomDataProvider.ReleaseConsumer;
begin
  if (FConsumerStack <> nil) and (FConsumerStack.Count > 0) then
    FConsumerStack.Delete(0)
  else
  if FConsumerStack <> nil then
    raise EJVCLDataProvider.CreateRes(@RsEConsumerStackIsEmpty);
end;

procedure TJvCustomDataProvider.SelectContext(Context: IJvDataContext);
begin
  if FContextStack <> nil then
  FContextStack.Insert(0, Context);
end;

function TJvCustomDataProvider.SelectedContext: IJvDataContext;
begin
  if (FContextStack <> nil) and (FContextStack.Count > 0) then
    Result := IJvDataContext(FContextStack[0])
  else
    Result := nil;
end;

procedure TJvCustomDataProvider.ReleaseContext;
begin
  if (FContextStack <> nil) and (FContextStack.Count > 0) then
    FContextStack.Delete(0)
  else
  if FContextStack <> nil then
    raise EJVCLDataProvider.CreateRes(@RsEContextStackIsEmpty);
end;

procedure TJvCustomDataProvider.ContextAdded(Context: IJvDataContext);
begin
end;

procedure TJvCustomDataProvider.ContextDestroying(Context: IJvDataContext);
begin
  DataItemsImpl.ContextDestroying(Context);
end;

procedure TJvCustomDataProvider.ConsumerDestroying(Consumer: IJvDataConsumer);
begin
end;

function TJvCustomDataProvider.AllowProviderDesigner: Boolean;
begin
  Result := PersistentDataItems;
end;

function TJvCustomDataProvider.AllowContextManager: Boolean;
var
  CtxMan: IJvDataContextsManager;
begin
  Result := (FDataContextsImpl <> nil) and
    Supports(FDataContextsImpl as IJvDataContexts, IJvDataContextsManager, CtxMan);
end;

function TJvCustomDataProvider.GetNotifierCount: Integer;
begin
  Result := FNotifiers.Count;
end;

function TJvCustomDataProvider.GetNotifier(Index: Integer): IJvDataProviderNotify;
begin
  Result := IJvDataProviderNotify(FNotifiers[Index]);
end;

function TJvCustomDataProvider.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvCustomDataProvider.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj) or Supports(GetItems, IID, Obj) or (
    // If we have contexts, check the interface table of that implementation as well.
    (FDataContextsImpl <> nil) and Supports(TObject(FDataContextsImpl), IID, Obj)
  );
end;

//=== { TJvBaseDataContexts } ================================================

constructor TJvBaseDataContexts.Create(AProvider: IJvDataProvider; AAncestor: IJvDataContext;
  ManagerClass: TJvDataContextsManagerClass);
begin
  inherited Create;
  FProvider := AProvider;
  FAncestor := AAncestor;
  if ManagerClass <> nil then
    ManagerClass.Create(Self);
end;

function TJvBaseDataContexts.Provider: IJvDataProvider;
begin
  Result := FProvider;
end;

function TJvBaseDataContexts.Ancestor: IJvDataContext;
begin
  Result := FAncestor;
end;

function TJvBaseDataContexts.GetContextByName(Name: string): IJvDataContext;
var
  PathSep: Integer;
  PathSep2: Integer;
  ThisPath: string;
  Idx: Integer;
begin
  PathSep := Pos('\', Name);
  PathSep2 := Pos('/', Name);
  if (PathSep > PathSep2) or (PathSep = 0) then
    PathSep := PathSep2;
  if PathSep = 0 then
    PathSep := Length(Name) + 1;
  ThisPath := Copy(Name, 1, PathSep - 1);
  if ThisPath = '..' then
  begin
    if Ancestor <> nil then
      Result := Ancestor.Contexts.GetContextByName(Copy(Name, PathSep + 1, Length(Name) - PathSep));
  end
  else
  if (ThisPath = '') and (Ancestor <> nil) and (PathSep <> 0) then
    Result := (Provider as IJvDataContexts).GetContextByName(Copy(Name, PathSep + 1, Length(Name) - PathSep))
  else
  begin
    Idx := GetCount - 1;
    while (Idx >= 0) and not AnsiSameText(GetContext(Idx).Name, ThisPath) do
      Dec(Idx);
    if Idx >= 0 then
    begin
      Result := GetContext(Idx);
      if PathSep < Length(Name) then
        Result := Result.Contexts.GetContextByName(Copy(Name, PathSep + 1, Length(Name) - PathSep));
    end;
  end;
end;

function TJvBaseDataContexts.IndexOf(Ctx: IJvDataContext): Integer;
begin
  Result := GetCount - 1;
  while (Result >= 0) and (Ctx <> GetContext(Result)) do
    Dec(Result);
end;

//=== { TJvBaseDataContextsManager } =========================================

function TJvBaseDataContextsManager.Contexts: IJvDataContexts;
begin
  Result := Owner as IJvDataContexts;
end;

function TJvBaseDataContextsManager.ContextsImpl: TJvBaseDataContexts;
begin
  Result := Owner as TJvBaseDataContexts;
end;

function TJvBaseDataContextsManager.Add(Context: IJvDataContext): IJvDataContext;
begin
  Result := Context;
  ContextsImpl.DoAddContext(Result);
end;

procedure TJvBaseDataContextsManager.Delete(Context: IJvDataContext);
begin
  ContextsImpl.DoRemoveContext(Context);
end;

procedure TJvBaseDataContextsManager.Clear;
begin
  ContextsImpl.DoClearContexts;
end;

//=== { TJvBaseDataContext } =================================================

constructor TJvBaseDataContext.Create(AContexts: TJvBaseDataContexts; AName: string);
begin
  if AContexts <> nil then
  begin
    inherited Create;
    FContexts := AContexts;
    SetName(AName);
  end
  else
    raise EJVCLDataContexts.CreateRes(@RsECannotCreateAContextWithoutAContext);
end;

procedure TJvBaseDataContext.SetName(Value: string);
var
  ExistingContext: IJvDataContext;
begin
  if Value <> Name then
  begin
    ExistingContext := Contexts.GetContextByName(Value);
    if (ExistingContext = nil) or (ExistingContext = (Self as IJvDataContext)) then
      DoSetName(Value)
    else
      raise EJVCLDataContexts.CreateRes(@RsEAContextWithThatNameAlreadyExists);
  end;
end;

function TJvBaseDataContext.GetImplementer: TObject;
begin
  Result := Self;
end;

function TJvBaseDataContext.ContextsImpl: TJvBaseDataContexts;
begin
  Result := FContexts;
end;

function TJvBaseDataContext.Contexts: IJvDataContexts;
begin
  Result := FContexts;
end;

function TJvBaseDataContext.IsDeletable: Boolean;
begin
  Result := True;
end;

function TJvBaseDataContext.IsStreamable: Boolean;
begin
  Result := not IsDeletable;
end;

//=== { TJvBaseFixedDataContext } ============================================

function TJvBaseFixedDataContext.IsDeletable: Boolean;
begin
  Result := False;
end;

//=== { TJvDataContexts } ====================================================

constructor TJvDataContexts.Create(AProvider: IJvDataProvider; AAncestor: IJvDataContext;
  ManagerClass: TJvDataContextsManagerClass);
begin
  inherited Create(AProvider, AAncestor, ManagerClass);
  FContexts := TInterfaceList.Create;
end;

destructor TJvDataContexts.Destroy;
begin
  FreeAndNil(FContexts);
  inherited Destroy;
end;

procedure TJvDataContexts.DoAddContext(Context: IJvDataContext);
var
  Tmp: IJvDataContext;
begin
  Tmp := GetContextByName(Context.Name);
  if Tmp = nil then
  begin
    Provider.Changing(pcrContextAdd, Ancestor);
    FContexts.Add(Context);
    Provider.Changed(pcrContextAdd, Context);
  end
  else
  begin
    if Tmp <> Context then
      raise EJVCLDataContexts.CreateRes(@RsEAContextWithThatNameAlreadyExists);
  end;
end;

procedure TJvDataContexts.DoDeleteContext(Index: Integer);
var
  Ctx: IJvDataContext;
  Anc: IJvDataContext;
begin
  Ctx := GetContext(Index);
  if (Ctx <> nil) and Ctx.IsDeletable then
  begin
    Anc := Ctx.Contexts.Ancestor;
    Provider.Changing(pcrContextDelete, Ctx);
    Ctx := nil;
    FContexts.Delete(Index);
    Provider.Changed(pcrContextDelete, Anc);
  end;
end;

procedure TJvDataContexts.DoRemoveContext(Context: IJvDataContext);
var
  Idx: Integer;
begin
  Idx := GetCount - 1;
  while (Idx >= 0) and (GetContext(Idx) <> Context) do
    Dec(Idx);
  if Idx >= 0 then
    DoDeleteContext(Idx);
end;

procedure TJvDataContexts.DoClearContexts;
begin
  FContexts.Clear;
end;

function TJvDataContexts.GetCount: Integer;
begin
  Result := FContexts.Count;
end;

function TJvDataContexts.GetContext(Index: Integer): IJvDataContext;
begin
  Result := IJvDataContext(FContexts[Index]);
end;

//=== { TJvDataContext } =====================================================

procedure TJvDataContext.DoSetName(Value: string);
begin
  FName := Value;
end;

function TJvDataContext.Name: string;
begin
  Result := FName;
end;

//=== { TJvFixedDataContext } ================================================

function TJvFixedDataContext.IsDeletable: Boolean;
begin
  Result := False;
end;

//=== { TJvDataConsumer } ====================================================

constructor TJvDataConsumer.Create(AOwner: TComponent; Attributes: array of Integer);
var
  I: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  FServerList := TInterfaceList.Create;
  for I := Low(Attributes) to High(Attributes) do
    DoAddAttribute(Attributes[I]);
end;

destructor TJvDataConsumer.Destroy;
begin
  // detach event handlers to avoid AVs when destroying
  FOnChanged := nil;
  FOnProviderChanging := nil;
  FOnProviderChanged := nil;
  FAfterCreateSubSvc := nil;
  FBeforeCreateSubSvc := nil;
  Provider := nil;
  FreeAndNil(FServerList);
  inherited Destroy;
end;

procedure TJvDataConsumer.SetProvider(Value: IJvDataProvider);
var
  CtxList: IJvDataContexts;
begin
  if FProvider <> Value then
  begin
    Changing(ccrProviderSelect);
    if FProvider <> nil then
      FProvider.UnregisterChangeNotify(Self);
    ProviderChanging;
    FProvider := Value;
    if FProvider <> nil then
      FProvider.RegisterChangeNotify(Self);
    if NeedContextFixup then
      FixupContext
    else
    begin
      if Supports(ProviderIntf, IJvDataContexts, CtxList) and (CtxList.GetCount >0 ) then
        SetContextIntf(CtxList.GetContext(0))
      else
        SetContextIntf(nil);
    end;
    ProviderChanged;
    NotifyServerProviderChanged;
    if NeedExtensionFixups then
      FixupExtensions;
    ViewChanged(nil);
    Changed(ccrProviderSelect);
  end;
end;



function TJvDataConsumer._AddRef: Integer;
begin
  Result := -1;
end;

function TJvDataConsumer._Release: Integer;
begin
  Result := -1;
end;

procedure TJvDataConsumer.DoProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if Assigned(FOnProviderChanging) then
    OnProviderChanging(ADataProvider, AReason, Source);
end;

procedure TJvDataConsumer.DoProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if Assigned(FOnProviderChanged) then
    OnProviderChanged(ADataProvider, AReason, Source);
end;

procedure TJvDataConsumer.DoAfterCreateSubSvc(ASvc: TJvDataConsumerAggregatedObject);
begin
  if Assigned(FAfterCreateSubSvc) then
    AfterCreateSubSvc(Self, ASvc);
end;

procedure TJvDataConsumer.DoBeforeCreateSubSvc(var AClass: TJvDataConsumerAggregatedObjectClass);
begin
  if Assigned(FBeforeCreateSubSvc) then
    BeforeCreateSubSvc(Self, AClass);
end;

procedure TJvDataConsumer.DoChanging(Reason: TJvDataConsumerChangeReason);
begin
  if Assigned(FOnChanging) then
    OnChanging(Self, Reason);
end;

procedure TJvDataConsumer.DoChanged(Reason: TJvDataConsumerChangeReason);
begin
  if Assigned(FOnChanged) then
    OnChanged(Self, Reason);
end;

procedure TJvDataConsumer.DoAddAttribute(Attr: Integer);
begin
  if not AttributeApplies(Attr) then
  begin
    SetLength(FAttrList, Length(FAttrList) + 1);
    FAttrList[High(FAttrList)] := Attr;
  end;
end;

procedure TJvDataConsumer.Changing(Reason: TJvDataConsumerChangeReason);
begin
  DoChanging(Reason);
end;

procedure TJvDataConsumer.Changed(Reason: TJvDataConsumerChangeReason);
begin
  if VCLComponent is TControl then
    TControl(VCLComponent).Invalidate;
  DoChanged(Reason);
end;

procedure TJvDataConsumer.ProviderChanging;
var
  I: Integer;
begin
  if FAdditionalIntfImpl <> nil then
  begin
    if not FNeedFixups and (FFixupContext = '') then
    begin
      I := 0;
      while I < ExtensionCount do
      begin
        Extension(I).ProviderChanging;
        Inc(I);
      end
    end;
  end;
end;

procedure TJvDataConsumer.ProviderChanged;
var
  I: Integer;
begin
  if FAdditionalIntfImpl <> nil then
  begin
    if not FNeedFixups and (FFixupContext = '') then
    begin
      I := 0;
      while I < ExtensionCount do
      begin
        if Extension(I).StreamedInWithoutProvider or Extension(I).KeepOnProviderChange then
        begin
          Extension(I).ProviderChanged;
          Inc(I);
        end
        else
          RemoveIntfImpl(Extension(I));
      end;
    end;
    UpdateExtensions;
  end;
end;

procedure TJvDataConsumer.ContextChanging;
var
  I: Integer;
begin
  if FAdditionalIntfImpl <> nil then
  begin
    if not FNeedFixups and (FFixupContext = '') then
    begin
      I := 0;
      while I < ExtensionCount do
      begin
        Extension(I).ContextChanging;
        Inc(I);
      end
    end;
  end;
end;

procedure TJvDataConsumer.ContextChanged;
var
  I: Integer;
begin
  if FAdditionalIntfImpl <> nil then
  begin
    if not FNeedFixups and (FFixupContext = '') then
    begin
      I := 0;
      while I < ExtensionCount do
      begin
        if Extension(I).StreamedInWithoutProvider or Extension(I).KeepOnContextChange then
        begin
          Extension(I).ContextChanged;
          Inc(I);
        end
        else
          RemoveIntfImpl(Extension(I));
      end;
    end;
    UpdateExtensions;
  end;
end;

procedure TJvDataConsumer.AfterSubSvcAdded(ASvc: TJvDataConsumerAggregatedObject);
begin
  DoAfterCreateSubSvc(ASvc);
  if ASvc is TJvCustomDataConsumerViewList then
    TJvCustomDataConsumerViewList(ASvc).RebuildView
end;

procedure TJvDataConsumer.UpdateExtensions;
var
  ImplArray: TClassArray;
  I: Integer;
  TmpClass: TJvDataConsumerAggregatedObjectClass;
begin
  SetLength(ImplArray, 0);
  if ProviderIntf <> nil then
  begin
    DP_SelectConsumerContext(ProviderIntf, Self, ContextIntf);
    try
      ImplArray := ProviderIntf.ConsumerClasses;
    finally
      DP_ReleaseConsumerContext(ProviderIntf);
    end;
    for I := Low(ImplArray) to High(ImplArray) do
    begin
      TmpClass := TJvDataConsumerAggregatedObjectClass(ImplArray[I]);
      if IndexOfImplClass(TmpClass) < 0 then
      begin
        DoBeforeCreateSubSvc(TmpClass);
        if TmpClass <> nil then
          DoAfterCreateSubSvc(TmpClass.Create(Self));
      end;
    end;
    if AttributeApplies(DPA_ConsumerDisplaysList) then
    begin
      TmpClass := TJvDataConsumerViewList;
      if IndexOfImplClass(TJvDataConsumerViewList) < 0 then
      begin
        DoBeforeCreateSubSvc(TmpClass);
        if TmpClass <> nil then
          AfterSubSvcAdded(TmpClass.Create(Self));
      end;
    end;
  end
  else
    ClearIntfImpl;
end;

procedure TJvDataConsumer.FixupExtensions;
var
  I: Integer;
begin
  for I := 0 to ExtensionCount - 1 do
    Extension(I).Fixup;
  FNeedFixups := False;
end;

procedure TJvDataConsumer.FixupContext;
begin
  Context := FFixupContext;
  FFixupContext := '';
end;

procedure TJvDataConsumer.ViewChanged(AExtension: TJvDataConsumerAggregatedObject);
var
  I: Integer;
begin
  try
    for I := 0 to ExtensionCount - 1 do
      if Extension(I) <> AExtension then
        Extension(I).ViewChanged(AExtension);
  finally
    Changed(ccrViewChange);
  end;
end;

procedure TJvDataConsumer.NotifyItemSelected(Value: IJvDataItem);
var
  I: Integer;
begin
  for I := 0 to ExtensionCount - 1 do
    Extension(I).ItemSelected(Value);
end;

procedure TJvDataConsumer.NotifyServerItemChanged(Server: IJvDataConsumerServerNotify;
  Value: IJvDataItem);
var
  I: Integer;
begin
  for I := 0 to ExtensionCount - 1 do
    Extension(I).ServerItemChanged(Server, Value);
end;

procedure TJvDataConsumer.NotifyServerProviderChanged;
var
  I: Integer;
begin
  if not IsLoading then
    for I := 0 to ServerCount - 1 do
      Servers[I].NotifyProviderChanged(Self);
end;

function TJvDataConsumer.ExtensionCount: Integer;
begin
  Result := FAdditionalIntfImpl.Count;
end;

function TJvDataConsumer.Extension(Index: Integer): TJvDataConsumerAggregatedObject;
begin
  Result := TJvDataConsumerAggregatedObject(FAdditionalIntfImpl[Index]);
end;

function TJvDataConsumer.IsContextStored: Boolean;
var
  CtxList: IJvDataContexts;
begin
  Result := (ProviderIntf <> nil) and Supports(ProviderIntf, IJvDataContexts, CtxList) and
    (CtxList.GetCount > 0) and (ContextIntf <> CtxList.GetContext(0));
end;

function TJvDataConsumer.GetNeedExtensionFixups: Boolean;
var
  I: Integer;
begin
  Result := FNeedFixups and ((VCLComponent = nil) or
    not (csLoading in VCLComponent.ComponentState));
  if not Result then
  begin
    I := ExtensionCount - 1;
    while not Result and (I >= 0) do
    begin
      Result := Extension(I).StreamedInWithoutProvider;
      Dec(I);
    end;
  end;
end;

function TJvDataConsumer.GetNeedContextFixup: Boolean;
begin
  Result := (FFixupContext <> '') and ((VCLComponent = nil) or
    not (csLoading in VCLComponent.ComponentState));
end;

function TJvDataConsumer.GetContext: TJvDataContextID;
begin
  if FContext = nil then
    Result := ''
  else
    Result := FContext.Name;
end;

procedure TJvDataConsumer.SetContext(Value: TJvDataContextID);
var
  ContextsIntf: IJvDataContexts;
  ContextIntf: IJvDataContext;
begin
  if not AnsiSameStr(Value, GetContext) then
  begin
    if ProviderIntf = nil then
    begin
      if (VCLComponent <> nil) and (csLoading in VCLComponent.ComponentState) then
        FFixupContext := Value
      else
        raise EJVCLDataConsumer.CreateRes(@RsEYouMustSpecifyAProviderBeforeSettin);
    end
    else
    begin
      if Value <> '' then
      begin
        if Supports(ProviderIntf, IJvDataContexts, ContextsIntf) then
        begin
          ContextIntf := ContextsIntf.GetContextByName(Value);
          if ContextIntf <> nil then
            SetContextIntf(ContextIntf)
          else
            raise EJVCLDataConsumer.CreateResFmt(@RsEProviderHasNoContextNameds, [Value]);
        end
        else
          raise EJVCLDataConsumer.CreateRes(@RsEProviderDoesNotSupportContexts);
      end
      else
        SetContextIntf(nil);
    end;
  end;
end;

function TJvDataConsumer.GetServerCount: Integer;
begin
  Result := FServerList.Count;
end;

function TJvDataConsumer.GetServers(I: Integer): IJvDataConsumerServerNotify;
begin
  Result := IJvDataConsumerServerNotify(FServerList[I]);
end;

procedure TJvDataConsumer.DataProviderChanging(const ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  I: Integer;
begin
  DoProviderChanging(ADataProvider, AReason, Source);
  for I := 0 to ExtensionCount - 1 do
    Extension(I).DataProviderChanging(ADataProvider, AReason, Source);
  if AReason = pcrDestroy then
    Provider := nil;
end;

procedure TJvDataConsumer.DataProviderChanged(const ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  I: Integer;
begin
  DoProviderChanged(ADataProvider, AReason, Source);
  for I := 0 to ExtensionCount - 1 do
    Extension(I).DataProviderChanged(ADataProvider, AReason, Source);
  if AReason = pcrFullRefresh then
    ViewChanged(nil)
  else
    Changed(ccrProviderChange);
end;

function TJvDataConsumer.Consumer: IJvDataConsumer;
begin
  Result := Self;
end;

function TJvDataConsumer.VCLComponent: TComponent;
begin
  Result := FOwner;
end;

function TJvDataConsumer.AttributeApplies(Attr: Integer): Boolean;
var
  I: Integer;
begin
  I := High(FAttrList);
  while (I >= 0) and (FAttrList[I] <> Attr) do
    Dec(I);
  Result := I >= 0;
end;

procedure TJvDataConsumer.ServerItemChanged(Server: IJvDataConsumerServerNotify; Value: IJvDataItem);
begin
  NotifyServerItemChanged(Server, Value);
end;

procedure TJvDataConsumer.LinkAdded(Server: IJvDataConsumerServerNotify);
begin
  FServerList.Add(Server);
end;

procedure TJvDataConsumer.LinkRemoved(Server: IJvDataConsumerServerNotify);
begin
  FServerList.Remove(Server);
end;

function TJvDataConsumer.ProviderIntf: IJvDataProvider;
begin
  Result := FProvider;
end;

procedure TJvDataConsumer.SetProviderIntf(Value: IJvDataProvider);
begin
  SetProvider(Value);
end;

function TJvDataConsumer.ContextIntf: IJvDataContext;
begin
  Result := FContext;
end;

procedure TJvDataConsumer.SetContextIntf(Value: IJvDataContext);
begin
  if Value <> ContextIntf then
  begin
    if (Value <> nil) and (Value.Contexts.Provider <> ProviderIntf) then
      raise EJVCLDataConsumer.CreateRes(@RsETheSpecifiedContextIsNotPartOfTheSa);
    Changing(ccrContextChange);
    ContextChanging;
    FContext := Value;
    ContextChanged;
    Changed(ccrContextChange);
  end;
end;

procedure TJvDataConsumer.Loaded;
begin
  if FFixupContext <> '' then
  begin
    Context := FFixupContext;
    FFixupContext := '';
  end;
  if FNeedFixups then
  begin
    FixupExtensions;
    FNeedFixups := False;
  end;
end;

procedure TJvDataConsumer.Enter;
begin
  DP_SelectConsumerContext(ProviderIntf, Self, ContextIntf);
end;

procedure TJvDataConsumer.Leave;
begin
  DP_ReleaseConsumerContext(ProviderIntf);
end;

procedure TJvDataConsumer.ItemSelected(Value: IJvDataItem);
var
  ItemAct: IJvDataItemBasicAction;
begin
  NotifyItemSelected(Value);
  if Supports(Value, IJvDataItemBasicAction, ItemAct) then
    ItemAct.Execute(VCLComponent);
end;

function TJvDataConsumer.IsLoading: Boolean;
begin
  Result := NeedExtensionFixups or NeedContextFixup;
end;

//=== { TJvDataConsumerAggregatedObject } ====================================

procedure TJvDataConsumerAggregatedObject.DataProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
end;

procedure TJvDataConsumerAggregatedObject.DataProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
end;

procedure TJvDataConsumerAggregatedObject.Fixup;
begin
end;

function TJvDataConsumerAggregatedObject.KeepOnProviderChange: Boolean;
begin
  Result := False;
end;

function TJvDataConsumerAggregatedObject.KeepOnContextChange: Boolean;
begin
  Result := True;
end;

procedure TJvDataConsumerAggregatedObject.Changing(Reason: TJvDataConsumerChangeReason);
begin
  ConsumerImpl.Changing(Reason);
end;

procedure TJvDataConsumerAggregatedObject.Changed(Reason: TJvDataConsumerChangeReason);
begin
  StreamedInWithoutProvider := ConsumerImpl.ProviderIntf = nil;
  ConsumerImpl.Changed(Reason);
end;

procedure TJvDataConsumerAggregatedObject.NotifyViewChanged;
begin
  ConsumerImpl.ViewChanged(Self);
end;

procedure TJvDataConsumerAggregatedObject.ViewChanged(AExtension: TJvDataConsumerAggregatedObject);
begin
end;

procedure TJvDataConsumerAggregatedObject.ItemSelected(Value: IJvDataItem);
begin
end;

procedure TJvDataConsumerAggregatedObject.ServerItemChanged(Server: IJvDataConsumerServerNotify;
  Value: IJvDataItem);
begin
end;

procedure TJvDataConsumerAggregatedObject.NotifyFixups;
begin
  ConsumerImpl.FNeedFixups := True;
  StreamedInWithoutProvider := True;
end;

procedure TJvDataConsumerAggregatedObject.ProviderChanging;
begin
end;

procedure TJvDataConsumerAggregatedObject.ProviderChanged;
begin
end;

procedure TJvDataConsumerAggregatedObject.ContextChanging;
begin
end;

procedure TJvDataConsumerAggregatedObject.ContextChanged;
begin
end;

function TJvDataConsumerAggregatedObject.Consumer: IJvDataConsumer;
begin
  Result := Owner as IJvDataConsumer;
end;

function TJvDataConsumerAggregatedObject.ConsumerImpl: TJvDataConsumer;
begin
  Result := Owner as TJvDataConsumer;
end;

function TJvDataConsumerAggregatedObject.RootItems: IJvDataItems;
var
  RootSelect: IJvDataConsumerItemSelect;
begin
  if Supports(Consumer, IJvDataConsumerItemSelect, RootSelect) and (RootSelect.GetItem <> nil) then
    RootSelect.GetItem.QueryInterface(IJvDataItems, Result)
  else
    ConsumerImpl.ProviderIntf.QueryInterface(IJvDataItems, Result);
end;

//=== { TJvDataConsumerContext } =============================================

function TJvDataConsumerContext.GetContextID: TJvDataContextID;
begin
  Result := ConsumerImpl.Context;
end;

procedure TJvDataConsumerContext.SetContextID(Value: TJvDataContextID);
begin
  ConsumerImpl.Context := Value;
end;

function TJvDataConsumerContext.GetContext: IJvDataContext;
begin
  Result := ConsumerImpl.ContextIntf;
end;

procedure TJvDataConsumerContext.SetContext(Value: IJvDataContext);
begin
  ConsumerImpl.SetContextIntf(Value);
end;

//=== { TJvDataConsumerItemSelect } ==========================================

procedure TJvDataConsumerItemSelect.Fixup;
begin
  SetItem(FItemID);
  FItemID := '';
end;

function TJvDataConsumerItemSelect.GetItem: TJvDataItemID;
begin
  if GetItemIntf = nil then
    Result := ''
  else
    Result := GetItemIntf.GetID;
end;

procedure TJvDataConsumerItemSelect.SetItem(Value: TJvDataItemID);
var
  TmpItem: IJvDataItem;
begin
  if not AnsiSameStr(Value, GetItem) then
  begin
    if Value = '' then
      SetItemIntf(nil)
    else
    begin
      if ConsumerImpl.ProviderIntf = nil then
      begin
        if (Consumer.VCLComponent <> nil) and (csLoading in Consumer.VCLComponent.ComponentState) then
        begin
          FItemID := Value;
          NotifyFixups;
          Exit;
        end
        else
          raise EJVCLDataConsumer.CreateRes(@RsEYouMustSpecifyAProviderBeforeSettin_);
      end
      else
      begin
        ConsumerImpl.Enter;
        try
          TmpItem := (ConsumerImpl.ProviderIntf as IJvDataIDSearch).Find(Value, True);
          if TmpItem <> nil then
            SetItemIntf(TmpItem)
          else
            raise EJVCLDataConsumer.CreateRes(@RsEItemNotFoundInTheSelectedContext);
        finally
          ConsumerImpl.Leave;
        end;
      end;
    end;
  end;
end;

procedure TJvDataConsumerItemSelect.DataProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  SourceItem: IJvDataItem;
begin
  if AReason = pcrDelete then
  begin
    SourceItem := IJvDataItem(Source);
    if (SourceItem <> nil) and (GetItemIntf <> nil) then
    begin
      ConsumerImpl.Enter;
      try
        if (SourceItem = GetItemIntf) or (SourceItem.IsParentOf(GetItemIntf)) then
          FItem := nil;
      finally
        ConsumerImpl.Leave;
      end;
    end;
  end;
end;

procedure TJvDataConsumerItemSelect.DataProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
end;

function TJvDataConsumerItemSelect.GetItemIntf: IJvDataItem;
begin
  Result := FItem;
end;

procedure TJvDataConsumerItemSelect.SetItemIntf(Value: IJvDataItem);
begin
  if Value <> GetItemIntf then
  begin
    FItem := Value;
    NotifyViewChanged;
  end;
end;

//=== { TJvCustomDataConsumerViewList } ======================================

constructor TJvCustomDataConsumerViewList.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FLevelIndent := 16;
  if ConsumerImpl.ProviderIntf <> nil then
    RebuildView;
end;

function TJvCustomDataConsumerViewList.KeepOnProviderChange: Boolean;
begin
  Result := True;
end;

procedure TJvCustomDataConsumerViewList.ProviderChanging;
begin
  ClearView;
end;

procedure TJvCustomDataConsumerViewList.ProviderChanged;
begin
  RebuildView;
end;

procedure TJvCustomDataConsumerViewList.ContextChanged;
begin
  RebuildView;
end;

procedure TJvCustomDataConsumerViewList.ViewChanged(AExtension: TJvDataConsumerAggregatedObject);
begin
  RebuildView;
end;

procedure TJvCustomDataConsumerViewList.DataProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  ItemIdx: Integer;
begin
  case AReason of
    pcrDelete:
      begin
        // Source is a reference to the item being deleted
        if Source <> nil then
        begin
          ConsumerImpl.Enter;
          try
            if IJvDataItem(Source) <> nil then
            begin
              ItemIdx := IndexOfItem(IJvDataItem(Source));
              if ItemIdx >= 0 then
              begin
                DeleteItem(ItemIdx);
                NotifyViewChanged;
              end;
            end;
          finally
            ConsumerImpl.Leave;
          end;
        end;
      end;
  end;
end;

procedure TJvCustomDataConsumerViewList.DataProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  ParItem: IJvDataItem;
  ParIdx: Integer;
begin
  case AReason of
    pcrAdd:
      begin
        // Source is a reference to the new item
        if Source <> nil then
        begin
          ConsumerImpl.Enter;
          try
            if IJvDataItem(Source) <> nil then
            begin
              ParItem := IJvDataItem(Source).GetItems.GetParent;
              if ParItem <> nil then
              begin
                ParIdx := IndexOfItem(ParItem);
                if (ParIdx < 0) and ExpandOnNewItem then
                begin
                  // Make sure the tree is expanded up to the parent item
                  ExpandTreeTo(ParItem);
                  ParIdx := IndexOfItem(ParItem);
                end;
                if ParIdx >= 0 then
                begin
                  if not ItemIsExpanded(ParIdx) and ExpandOnNewItem then
                  begin
                    // Expand parent item; will retrieve all sub items, including the newly added item
                    if not ItemHasChildren(ParIdx) then
                      UpdateItemFlags(ParIdx, vifHasChildren + vifCanHaveChildren,
                        vifHasChildren + vifCanHaveChildren);
                    ToggleItem(ParIdx);
                  end
                  else
                  if ItemIsExpanded(ParIdx) then
                  begin
                    // parent is expanded, add the new item to the view.
                    AddChildItem(ParIdx, IJvDataItem(Source));
                    NotifyViewChanged;
                  end;
                end;
              end
              else
              begin
                // Item at the root; always add it
                AddChildItem(-1, IJvDataItem(Source));
                NotifyViewChanged;
              end;
            end;
          finally
            ConsumerImpl.Leave;
          end;
        end;
      end;
  end;
end;

function TJvCustomDataConsumerViewList.InternalItemSibling(ParentIndex: Integer;
  var ScanIndex: Integer): Integer;
var
  Lvl: Integer;
begin
  Lvl := ItemLevel(ParentIndex);
  if ScanIndex <= ParentIndex then
    ScanIndex := ParentIndex + 1;
  while (ScanIndex < Count) and (ItemLevel(ScanIndex) > Lvl) do
    Inc(ScanIndex);
  if (ScanIndex >= Count) or (ItemLevel(ScanIndex) < Lvl) then
    Result := -1
  else
    Result := ScanIndex;
  if ScanIndex > Count then
    ScanIndex := Count;
end;

function TJvCustomDataConsumerViewList.Get_AutoExpandLevel: Integer;
begin
  Result := FAutoExpandLevel;
end;

procedure TJvCustomDataConsumerViewList.Set_AutoExpandLevel(Value: Integer);
begin
  FAutoExpandLevel := Value;
end;

function TJvCustomDataConsumerViewList.Get_ExpandOnNewItem: Boolean;
begin
  Result := FExpandOnNewItem;
end;

procedure TJvCustomDataConsumerViewList.Set_ExpandOnNewItem(Value: Boolean);
begin
  FExpandOnNewItem := Value;
end;

function TJvCustomDataConsumerViewList.Get_LevelIndent: Integer;
begin
  Result := FLevelIndent;
end;

procedure TJvCustomDataConsumerViewList.Set_LevelIndent(Value: Integer);
begin
  if Value <> LevelIndent then
  begin
    FLevelIndent := Value;
    Changed(ccrOther);
  end;
end;

procedure TJvCustomDataConsumerViewList.ClearView;
begin
  // override if the implementation can be optimized
  while Count > 0 do
    DeleteItem(0);
end;

procedure TJvCustomDataConsumerViewList.RebuildView;
var
  Idx: Integer;
begin
  ClearView;
  if (ConsumerImpl <> nil) and (ConsumerImpl.ProviderIntf <> nil) then
  begin
    ConsumerImpl.Enter;
    try
      Idx := 0;
      AddItems(Idx, RootItems, AutoExpandLevel);
    finally
      ConsumerImpl.Leave;
    end;
  end;
  NotifyViewChanged;
end;

procedure TJvCustomDataConsumerViewList.ExpandTreeTo(Item: IJvDataItem);
var
  ParIdx: Integer;
begin
  if (Item <> nil) and (GetItemVisibleState(Item) <> disFalse) then
  begin
    if (IndexOfID(Item.GetID) >= 0) and (Item.Items.GetParent <> nil) then
    begin
      ExpandTreeTo(Item.GetItems.GetParent);
      ParIdx := IndexOfID(Item.GetItems.GetParent.GetID);
      if ParIdx >= 0 then
      begin
        if ItemIsExpanded(ParIdx) then // we have a big problem <g>
          raise EJVCLDataConsumer.CreateRes(@RsEViewListOutOfSync);
        ToggleItem(ParIdx);
      end;
    end;
  end;
end;

procedure SetBit(var IntArray: array of Integer; BitNo: Integer);
var
  ArrayOffset: Integer;
  BitOffset: Integer;
begin
  ArrayOffset := BitNo div 32;
  BitOffset := BitNo mod 32;
  IntArray[ArrayOffset] := IntArray[ArrayOffset] or (1 shl BitOffset);
end;

function TJvCustomDataConsumerViewList.ItemGroupInfo(Index: Integer): TDynIntegerArray;
var
  LvlIdx: Integer;
  LastScanIndex: Integer;
begin
  LvlIdx := ItemLevel(Index) - 1;
  SetLength(Result, LvlIdx div 32 + Ord((LvlIdx mod 32) > 0));
  LastScanIndex := Index;
  { Keep using the last scanned item as a start point to find a sibling for the next parent. Reduces
    the number of compares to make. }
  while LvlIdx >= 0 do
  begin
    Index := ItemParentIndex(Index);
    if InternalItemSibling(Index, LastScanIndex) <> -1 then
      SetBit(Result, LvlIdx); // There's another sibling at this level; set the corresponding bit
    Dec(LvlIdx);
  end;
end;

//=== { TJvDataConsumerViewList } ============================================

procedure TJvDataConsumerViewList.AddItem(Index: Integer; Item: IJvDataItem; ExpandToLevel: Integer);
var
  Lvl: Integer;
  Idx: Integer;
  SubItems: IJvDataItems;
begin
  if GetItemVisibleState(Item) <> disFalse then
  begin
    if Index < 0 then
    begin
      Lvl := 0;
      Idx := Count;
    end
    else
    begin
      Lvl := Succ(ItemLevel(Index));
      Idx := Index + 1;
      if FViewItems[Index].Flags and (vifHasChildren + vifExpanded) = vifHasChildren then
      begin
        ToggleItem(Index);
        Exit;
      end;
    end;
    while (Idx < Count) and (ItemLevel(Idx) >= Lvl) do
      Inc(Idx);
    SetLength(FViewItems, Length(FViewItems) + 1);
    if Idx < High(FViewItems) then
    begin
      Move(FViewItems[Idx], FViewItems[Idx + 1], (High(FViewItems) - Idx) * SizeOf(FViewItems[0]));
      FillChar(FViewItems[Idx], SizeOf(FViewItems[0]), 0);
    end;
    with FViewItems[Idx] do
    begin
      ItemID := Item.GetID;
      if Supports(Item, IJvDataItems, SubItems) then
      begin
        if SubItems.Count > 0 then
          Flags := Lvl + vifHasChildren + vifCanHaveChildren
        else
          Flags := Lvl + vifCanHaveChildren
      end
      else
        Flags := Lvl;
    end;
    if Index > -1 then
      with FViewItems[Index] do
        Flags := Flags or vifHasChildren or vifCanHaveChildren or vifExpanded;
    if (ExpandToLevel <> 0) and (SubItems <> nil) and (SubItems.Count > 0) then
    begin
      Inc(Index);
      AddItems(Index, SubItems, ExpandToLevel - 1);
    end;
  end;
end;

procedure TJvDataConsumerViewList.AddChildItem(ParentIndex: Integer; Item: IJvDataItem);
var
  InsertIndex: Integer;
begin
  if GetItemVisibleState(Item) <> disFalse then
  begin
    InsertIndex := -1;
    if ParentIndex > -1 then
    begin
      if not ItemIsExpanded(ParentIndex) then
      begin
        if not ItemHasChildren(ParentIndex) then
          UpdateItemFlags(ParentIndex, vifHasChildren + vifCanHaveChildren, vifHasChildren +
            vifCanHaveChildren);
        ToggleItem(ParentIndex);
      end;
      if IndexOfItem(Item) < 0 then
        InternalItemSibling(ParentIndex, InsertIndex);
    end
    else
      InsertIndex := Count;
    if InsertIndex > -1 then
      InsertItem(InsertIndex, ParentIndex, Item);
  end;
end;

procedure TJvDataConsumerViewList.AddItems(var Index: Integer; Items: IJvDataItems; ExpandToLevel: Integer);
var
  SkipCount: Integer;
  I: Integer;
  J: Integer;
  SubItems: IJvDataItems;
begin
  J := Count;
  SetLength(FViewItems, Count + Items.Count);
  if Index < J then
  begin
    Move(FViewItems[Index], FViewItems[Index + Items.Count], (J - Index) * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Index], Items.Count * SizeOf(FViewItems[0]), 0);
  end;
  J := 0;
  if Index > 0 then
  begin
    J := 1 + FViewItems[Index - 1].Flags and $00FFFFFF;
    FViewItems[Index - 1].Flags := FViewItems[Index - 1].Flags or vifExpanded;
  end;
  SkipCount := 0;
  for I  := 0 to Items.Count - 1 do
  begin
    if GetItemVisibleState(Items.Items[I]) <> disFalse then
    begin
      with FViewItems[Index] do
      begin
        ItemID := Items.Items[I].GetID;
        Flags := J;
        if Supports(Items.Items[I], IJvDataItems, SubItems) then
        begin
          Flags := Flags + vifCanHaveChildren;
          if SubItems.Count > 0 then
          begin
            Flags := Flags + vifHasChildren;
            if ExpandToLevel <> 0 then
            begin
              Inc(Index);
              AddItems(Index, SubItems, ExpandToLevel - 1);
              Dec(Index);
            end;
          end;
        end;
      end;
      Inc(Index);
    end
    else
      Inc(SkipCount);
  end;
  if SkipCount > 0 then
  begin
    if Index < High(FViewItems) then
      Move(FViewItems[Index + 1], FViewItems[Index], SkipCount * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Length(FViewItems) - SkipCount], SkipCount * SizeOf(FViewItems[0]), 0);
    SetLength(FViewItems, Length(FViewItems) - SkipCount);
  end;
end;

procedure TJvDataConsumerViewList.InsertItem(InsertIndex, ParentIndex: Integer; Item: IJvDataItem);
var
  Level: Integer;
  SubItems: IJvDataItems;
begin
  if GetItemVisibleState(Item) <> disFalse then
  begin
    if ParentIndex < 0 then
      Level := 0
    else
      Level := Succ(ItemLevel(ParentIndex));
    SetLength(FViewItems, Count + 1);
    if InsertIndex < High(FViewItems) then
    begin
      Move(FViewItems[InsertIndex], FViewItems[InsertIndex + 1], (High(FViewItems) - InsertIndex) * SizeOf(FViewItems[0]));
      FillChar(FViewItems[InsertIndex], SizeOf(FViewItems[0]), 0);
    end;
    with FViewItems[InsertIndex] do
    begin
      ItemID := Item.GetID;
      if Supports(Item, IJvDataItems, SubItems) then
      begin
        Level := Level + vifCanHaveChildren;
        if SubItems.Count > 0 then
          Level := Level + vifHasChildren;
      end;
      Flags := Level;
    end;
    if ParentIndex >= 0 then
      FViewItems[ParentIndex].Flags := FViewItems[ParentIndex].Flags or (vifCanHaveChildren +
        vifHasChildren + vifExpanded);
  end;
end;

procedure TJvDataConsumerViewList.DeleteItem(Index: Integer);
var
  PrevIsParent: Boolean;
begin
  DeleteItems(Index);
  PrevIsParent := (Index > 0) and (ItemLevel(Index - 1) = (ItemLevel(Index) - 1));
  FViewItems[Index].ItemID := '';
  if Index < High(FViewItems) then
    Move(FViewItems[Index + 1], FViewItems[Index], (Length(FViewItems) - Index) * SizeOf(FViewItems[0]));
  FillChar(FViewItems[High(FViewItems)], SizeOf(FViewItems[0]), 0);
  SetLength(FViewItems, High(FViewItems));
  if PrevIsParent and ((Index = Length(FViewItems)) or (ItemLevel(Index - 1) <> (ItemLevel(Index) - 1))) then
    FViewItems[Index - 1].Flags := FViewItems[Index - 1].Flags and not (vifHasChildren or vifExpanded);
end;

procedure TJvDataConsumerViewList.DeleteItems(Index: Integer);
var
  Idx: Integer;
  Lvl: Integer;
begin
  if FViewItems[Index].Flags and (vifExpanded + vifHasChildren) = (vifExpanded + vifHasChildren) then
  begin
    Lvl := ItemLevel(Index) + 1;
    Idx := Index + 1;
    while (Idx < Length(FViewItems)) and (ItemLevel(Idx) >= Lvl) do
    begin
      FViewItems[Idx].ItemID := '';
      Inc(Idx);
    end;
    // Idx points to next item that is not a child
    if Idx < Count then
      Move(FViewItems[Idx], FViewItems[Index + 1], (Length(FViewItems) - Idx) * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Length(FViewItems) - Pred(Idx - Index)], Pred(Idx - Index) * SizeOf(FViewItems[0]), 0);
    SetLength(FViewItems, Length(FViewItems) - (Idx - Index - 1));
    FViewItems[Index].Flags := FViewItems[Index].Flags and not vifExpanded;
  end;
end;

procedure TJvDataConsumerViewList.UpdateItemFlags(Index: Integer; Value, Mask: Integer);
begin
  FViewItems[Index].Flags := FViewItems[Index].Flags and not Mask or (Value and Mask);
end;

procedure TJvDataConsumerViewList.ToggleItem(Index: Integer);
var
  TmpItem: IJvDataItem;
  Items: IJvDataItems;
begin
  if ItemHasChildren(Index) then
  begin
    if ItemIsExpanded(Index) then
      DeleteItems(Index)
    else
    begin
      TmpItem := Item(Index);
      if (TmpItem <> nil) and Supports(TmpItem, IJvDataItems, Items) then
      begin
        Inc(Index);
        AddItems(Index, Items);
      end;
    end;
    NotifyViewChanged;
  end;
end;

function TJvDataConsumerViewList.IndexOfItem(Item: IJvDataItem): Integer;
begin
  Result := IndexOfID(Item.GetID);
end;

function TJvDataConsumerViewList.IndexOfID(ID: TJvDataItemID): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and not AnsiSameText(FViewItems[Result].ItemID, ID) do
    Dec(Result);
end;

function TJvDataConsumerViewList.ChildIndexOfItem(Item: IJvDataItem): Integer;
begin
  Result := ChildIndexOfID(Item.GetID);
end;

function TJvDataConsumerViewList.ChildIndexOfID(ID: TJvDataItemID): Integer;
var
  Index: Integer;
  ChildLevel: Integer;
begin
  Result := -1;
  Index := IndexOfID(ID);
  if Index >= 0 then
  begin
    Inc(Result);
    if Index > 0 then
    begin
      ChildLevel := ItemLevel(Index);
      Dec(Index);
      while (Index >= 0) and (ItemLevel(Index) >= ChildLevel) do
      begin
        if ItemLevel(Index) = ChildLevel then
          Inc(Result);
        Dec(Index);
      end;
    end;
  end;
end;

function TJvDataConsumerViewList.Item(Index: Integer): IJvDataItem;
var
  Items: IJvDataItems;
  Finder: IJvDataIDSearch;
  {$IFNDEF ViewList_UseFinder}
  ParIdx: Integer;
  {$ENDIF ViewList_UseFinder}
begin
  {$IFDEF ViewList_UseFinder}
  { The easiest way: use IJvDataIDSearch to locate the item given it's ID value. Scans all items
    recursively until it finds a match or nothing at all. Could be rather slow on larger trees. }
  Items := RootItems;
  if Supports(RootItems, IJvDataIDSearch, Finder) then
    Result := Finder.Find(FViewItems[Index].ItemID, True);
  {$ELSE}
  { This should be faster, especially with larger trees. This will only scan the parent item's
    IJvDataItems list (the parent item is retrieved using this same method). This still saves a lot
    of ID comparisons in large trees and for dynamic items also an enormous amount of
    creation/destruction of items. The entire implementation of this class should be adapted to not
    store the ID but the item's index in the provider list, so we can streamline this method to use
    and index path only. }
  ParIdx := ItemParentIndex(Index);
  if ParIdx >= 0 then
    // Parent found, retrieve the IJVDataItems reference
    Item(ParIdx).QueryInterface(IJvDataItems, Items)
  else
    // Apparantly this item is at the root of the view; retrieve the proper IJvDataItems reference
    Items := RootItems;
  if Supports(Items, IJvDataIDSearch, Finder) then
    Result := Finder.Find(FViewItems[Index].ItemID, False);
  {$ENDIF ViewList_UseFinder}
end;

function TJvDataConsumerViewList.ItemLevel(Index: Integer): Integer;
begin
  Result := FViewItems[Index].Flags and $00FFFFFF;
end;

function TJvDataConsumerViewList.ItemIsExpanded(Index: Integer): Boolean;
begin
  Result := FViewItems[Index].Flags and vifExpanded <> 0;
end;

function TJvDataConsumerViewList.ItemHasChildren(Index: Integer): Boolean;
begin
  Result := FViewItems[Index].Flags and vifHasChildren <> 0;
end;

function TJvDataConsumerViewList.ItemParent(Index: Integer): IJvDataItem;
var
  ParIdx: Integer;
begin
  ParIdx := ItemParentIndex(Index);
  if ParIdx >= 0 then
    Result := Item(ParIdx);
end;

function TJvDataConsumerViewList.ItemParentIndex(Index: Integer): Integer;
var
  ParLevel: Integer;
begin
  ParLevel := ItemLevel(Index) - 1;
  Result := Index - 1;
  while (Result >= 0) and (ItemLevel(Result) > ParLevel) do
    Dec(Result);
end;

function TJvDataConsumerViewList.ItemSibling(Index: Integer): IJvDataItem;
var
  Idx: Integer;
begin
  Idx := ItemSiblingIndex(Index);
  if Idx > -1 then
    Result := Item(Idx)
  else
    Result := nil;
end;

function TJvDataConsumerViewList.ItemSiblingIndex(Index: Integer): Integer;
begin
  Result := InternalItemSibling(Index, Index);
end;

function TJvDataConsumerViewList.SubItem(Parent: IJvDataItem; Index: Integer): IJvDataItem;
begin
  Result := SubItem(IndexOfItem(Parent), Index);
end;

function TJvDataConsumerViewList.SubItem(Parent, Index: Integer): IJvDataItem;
var
  Idx: Integer;
begin
  Idx := SubItemIndex(Parent, Index);
  if Idx > -1 then
    Result := Item(Idx)
  else
    Result := nil;
end;

function TJvDataConsumerViewList.SubItemIndex(Parent: IJvDataItem; Index: Integer): Integer;
begin
  Result := SubItemIndex(IndexOfItem(Parent), Index);
end;

function TJvDataConsumerViewList.SubItemIndex(Parent, Index: Integer): Integer;
begin
  Result := Parent + 1;
  while (Result >= 0) and (Index >= 0) do
  begin
    Dec(Index);
    if Index >= 0 then
      Result := ItemSiblingIndex(Result);
  end;
end;

function TJvDataConsumerViewList.Count: Integer;
begin
  Result := Length(FViewItems);
end;

//=== { TJvDataConsumerServerNotify } ========================================

constructor TJvDataConsumerServerNotify.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FClients := TJvDataConsumerClientNotifyList.Create(Self);
end;

destructor TJvDataConsumerServerNotify.Destroy;
begin
  FreeAndNil(FClients);
  inherited Destroy;
end;

procedure TJvDataConsumerServerNotify.SetClients(Value: TJvDataConsumerClientNotifyList);
begin
  if Value <> nil then
    FClients.Assign(Value)
  else
    FClients.Clear;
end;

procedure TJvDataConsumerServerNotify.ItemSelected(Value: IJvDataItem);
begin
  { Default behavior: notify clients about the newly selected item. Override the method to take
    other action (either in addition to or instead of the default behavior). }
  NotifyItemSelected(Value);
end;

function TJvDataConsumerServerNotify.GetOwner: TPersistent;
begin
  // To make the collection editor actually show up.
  Result := ConsumerImpl.VCLComponent;
end;

procedure TJvDataConsumerServerNotify.NotifyItemSelected(Value: IJvDataItem);
var
  I: Integer;
begin
  for I := 0 to Clients.Count - 1 do
    if Clients.NotifyItems[I].Notifier <> nil then
      Clients.NotifyItems[I].Notifier.ItemSelected(Self, Value);
end;

function TJvDataConsumerServerNotify.IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
begin
  // Override this method to determine if the specified client can be linked to this server.
  Result := False;
end;

procedure TJvDataConsumerServerNotify.AddClient(Client: IJvDataConsumerClientNotify);
begin
  if IsValidClient(Client) then
    FClients.Add(Client as IJvDataConsumer);
end;

procedure TJvDataConsumerServerNotify.RemoveClient(Client: IJvDataConsumerClientNotify);
begin
  FClients.Delete(Client as IJvDataConsumer);
end;

procedure TJvDataConsumerServerNotify.NotifyProviderChanged(Client: IJvDataConsumerClientNotify);
begin
  if not IsValidClient(Client) then
    RemoveClient(Client);
end;

//=== { TJvDataConsumerClientNotifyList } ====================================

constructor TJvDataConsumerClientNotifyList.Create(AServer: TJvDataConsumerServerNotify);
begin
  inherited Create(AServer.ConsumerImpl.VCLComponent, TJvDataConsumerClientNotifyItem);
  FServer := AServer;
end;

function TJvDataConsumerClientNotifyList.GetServer: TJvDataConsumerServerNotify;
begin
  Result := FServer;
end;

function TJvDataConsumerClientNotifyList.GetNotifyItems(
  I: Integer): TJvDataConsumerClientNotifyItem;
begin
  Result := TJvDataConsumerClientNotifyItem(Items[I]);
end;

function TJvDataConsumerClientNotifyList.GetConsumer(I: Integer): IJvDataConsumer;
var
  Item: TJvDataConsumerClientNotifyItem;
begin
  Item := GetNotifyItems(I);
  Supports(Item.Notifier, IJvDataConsumer, Result);
end;

procedure TJvDataConsumerClientNotifyList.SetItemName(Item: TCollectionItem);
begin
  Server.StreamedInWithoutProvider := Server.ConsumerImpl.ProviderIntf = nil;
end;

procedure TJvDataConsumerClientNotifyList.Add(AComponent: TComponent);
var
  PI: PPropInfo;
  Obj: TObject;
  Consumer: IJvDataConsumer;
begin
  if AComponent <> nil then
  begin
    PI := GetPropInfo(AComponent, cProvider);
    if PI <> nil then
    begin
      Obj := GetObjectProp(AComponent, cProvider);
      if (Obj <> nil) and Supports(Obj, IJvDataConsumer, Consumer) then
        Add(Consumer)
      else
        raise EJVCLDataConsumer.CreateResFmt(@RsEProviderIsNoIJvDataConsumer, [AComponent.Name]);
    end
    else
      raise EJVCLDataConsumer.CreateResFmt(@RsEComponentIsNotDataConsumer, [AComponent.Name]);
  end
  else
    raise EJVCLDataConsumer.CreateRes(@RsECannotAddNil);
end;

procedure TJvDataConsumerClientNotifyList.Add(AConsumer: IJvDataConsumer);
var
  Notifier: IJvDataConsumerClientNotify;
begin
  if AConsumer <> nil then
  begin
    if IndexOf(AConsumer) = -1 then
    begin
      if Supports(AConsumer, IJvDataConsumerClientNotify, Notifier) then
        TJvDataConsumerClientNotifyItem.Create(Self).Notifier := Notifier
      else
        raise EJVCLDataConsumer.CreateRes(@RsEConsumerNoSupportIJvDataConsumerClientNotify);
    end;
  end
  else
    raise EJVCLDataConsumer.CreateRes(@RsECannotAddNil);
end;

procedure TJvDataConsumerClientNotifyList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TJvDataConsumerClientNotifyList.Delete(AComponent: TComponent);
var
  Idx: Integer;
begin
  Idx := IndexOf(AComponent);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvDataConsumerClientNotifyList.Delete(AConsumer: IJvDataConsumer);
var
  Idx: Integer;
begin
  Idx := IndexOf(AConsumer);
  if Idx > -1 then
    Delete(Idx);
end;

function TJvDataConsumerClientNotifyList.IndexOf(AComponent: TComponent): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (NotifyItems[Result].Component <> AComponent) do
    Dec(Result);
end;

function TJvDataConsumerClientNotifyList.IndexOf(AConsumer: IJvDataConsumer): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Clients[Result] <> AConsumer) do
    Dec(Result);
end;

//=== { TJvDataConsumerClientNotifyItem } ====================================

function TJvDataConsumerClientNotifyItem.GetList: TJvDataConsumerClientNotifyList;
begin
  Result := TJvDataConsumerClientNotifyList(Collection);
end;

function TJvDataConsumerClientNotifyItem.GetConsumer: IJvDataConsumer;
begin
  Supports(FNotifier, IJvDataConsumer, Result);
end;

function TJvDataConsumerClientNotifyItem.GetComponent: TComponent;
var
  Con: IJvDataConsumer;
begin
  Con := GetConsumer;
  if Con <> nil then
    Result := Con.VCLComponent
  else
    Result := nil;
end;

procedure TJvDataConsumerClientNotifyItem.SetComponent(Value: TComponent);
var
  PI: PPropInfo;
  Obj: TObject;
  Consumer: IJvDataConsumer;
  TmpNotifier: IJvDataConsumerClientNotify;
begin
  if Value <> Component then
  begin
    if Value <> nil then
    begin
      PI := GetPropInfo(Value, cProvider);
      if PI <> nil then
      begin
        Obj := GetObjectProp(Value, cProvider);
        if (Obj <> nil) and Supports(Obj, IJvDataConsumer, Consumer) then
        begin
          if Supports(Consumer, IJvDataConsumerClientNotify, TmpNotifier) then
          begin
            if Notifier <> nil then
              Notifier.LinkRemoved(List.Server);
            FNotifier := TmpNotifier;
            Notifier.LinkAdded(List.Server);
          end
          else
            raise EJVCLDataConsumer.CreateRes(@RsEConsumerNoSupportIJvDataConsumerClientNotify);
        end
        else
          raise EJVCLDataConsumer.CreateResFmt(@RsEProviderIsNoIJvDataConsumer, [Value.Name]);
      end
      else
        raise EJVCLDataConsumer.CreateResFmt(@RsEComponentIsNotDataConsumer, [Value.Name]);
    end
    else
      raise EJVCLDataConsumer.CreateRes(@RsECannotAddNil);
  end;
end;

procedure TJvDataConsumerClientNotifyItem.SetNotifier(Value: IJvDataConsumerClientNotify);
var
  Consumer: IJvDataConsumer;
begin
  if Value <> Notifier then
  begin
    if Value <> nil then
    begin
      if Supports(Value, IJvDataConsumer, Consumer) then
      begin
        if Notifier <> nil then
          Notifier.LinkRemoved(List.Server);
        FNotifier := Value;
        Notifier.LinkAdded(List.Server);
      end
      else
        raise EJVCLDataConsumer.CreateRes(@RsENotifierNoSupprtIJvDataConsumer);
    end
    else
      raise EJVCLDataConsumer.CreateRes(@RsECannotAddNil);
  end;
end;

function TJvDataConsumerClientNotifyItem.GetDisplayName: string;
begin
  if (Component = nil) or (Component.Name = '') then
    Result := inherited GetDisplayName
  else
    Result := Component.Name;
end;

destructor TJvDataConsumerClientNotifyItem.Destroy;
begin
  if Notifier <> nil then
    Notifier.LinkRemoved(List.Server);
  inherited Destroy;
end;

//=== { TJvConsumerStrings } =================================================

constructor TJvConsumerStrings.Create(AConsumer: TJvDataConsumer);
begin
  inherited Create;
  FConsumer := AConsumer;
end;

function TJvConsumerStrings.Get(Index: Integer): string;
var
  VL: IJvDataConsumerViewList;
  ItemText: IJvDataItemText;
begin
  if Index < 0 then
    Error(SListIndexError, Index);
  Result := '';
  Consumer.Enter;
  try
    if Supports(Consumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
    begin
      if Index >= VL.Count then
        Error(SListIndexError, Index);
      if Supports(VL.Item(Index), IJvDataItemText, ItemText) then
        Result := ItemText.Caption;
    end;
  finally
    Consumer.Leave;
  end;
end;

function TJvConsumerStrings.GetCount: Integer;
var
  VL: IJvDataConsumerViewList;
begin
  Result := 0;
  Consumer.Enter;
  try
    if Supports(Consumer as IJvDataConsumer, IJvDataConsumerViewList, VL) then
      Result := VL.Count;
  finally
    Consumer.Leave;
  end;
end;

procedure TJvConsumerStrings.Clear;
begin
  // Do not allow the consumer view list to be modified this way.
end;

procedure TJvConsumerStrings.Delete(Index: Integer);
begin
  // Do not allow the consumer view list to be modified this way.
end;

procedure TJvConsumerStrings.Insert(Index: Integer; const S: string);
begin
  // Do not allow the consumer view list to be modified this way.
end;

initialization 
  GroupDescendentsWith(TExtensibleInterfacedPersistent, TControl);
  GroupDescendentsWith(TAggregatedPersistent, TControl); 
  RegisterClasses([
    // Items related
    TJvDataItemsList, TJvCustomDataItemsImages, TJvCustomDataItemsTextRenderer,
    TJvBaseDataItemsListManagement,
    // Item related
    TJvBaseDataItem, TJvDataItemTextImpl, TJvDataItemImageImpl,
    // Consumer related
    TJvDataConsumer, TJvDataConsumerItemSelect,
    // Context list related
    TJvDataContexts,
    // Context related
    TJvDataContext, TJvManagedDataContext, TJvFixedDataContext]);

end.

