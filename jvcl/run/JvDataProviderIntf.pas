{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProvider.pas, released on --.

The Initial Developers of the Original Code are Marcel Bestebroer, Peter
Thörnqvist and Remko Bonte
Portions created by these individuals are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-11-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderIntf;

interface

uses
  Windows, ImgList, Classes, Graphics,
  JclBase,
  JvTypes;

type
  TDataProviderChangeReason =
   (pcrAdd, pcrDelete, pcrUpdateItem, pcrUpdateItems, pcrDestroy,
    pcrContextAdd, pcrContextDelete, pcrContextUpdate, pcrFullRefresh);
  TDataItemState = (disFalse, disTrue, disIndetermined, disNotUsed);
  TProviderDrawState =
   (pdsSelected, pdsGrayed, pdsDisabled, pdsChecked,
    pdsFocused, pdsDefault, pdsHot);
  TProviderDrawStates = set of TProviderDrawState;
  TClassArray = array of TClass;
  TJvDataContextID = type string;
  TJvDataItemID = type string;

  // forwards
  IJvDataProvider = interface;
  IJvDataItems = interface;
  IJvDataItem = interface;
  IJvDataConsumer = interface;
  IJvDataProviderNotify = interface;
  IJvDataContexts = interface;
  IJvDataContext = interface;
  IJvDataConsumerServerNotify = interface;
  IJvDataConsumerClientNotify = interface;

  IJvDataProvider = interface
  ['{62A7A17D-1E21-427E-861D-C92FBB9B09A6}']
    procedure RegisterChangeNotify(ANotify: IJvDataProviderNotify);
    procedure UnregisterChangeNotify(ANotify: IJvDataProviderNotify);
    function GetItems: IJvDataItems;
    procedure Changing(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    procedure Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    function ConsumerClasses: TClassArray;
    procedure SelectConsumer(Consumer: IJvDataConsumer);
    function SelectedConsumer: IJvDataConsumer;
    procedure ReleaseConsumer;
    procedure SelectContext(Context: IJvDataContext);
    function SelectedContext: IJvDataContext;
    procedure ReleaseContext;
    procedure ContextDestroying(Context: IJvDataContext);
    procedure ConsumerDestroying(Consumer: IJvDataConsumer);
    function AllowProviderDesigner: Boolean;
    function AllowContextManager: Boolean;
    function GetNotifierCount: Integer;
    function GetNotifier(Index: Integer): IJvDataProviderNotify;
    function GetImplementer: TObject;
  end;

  IJvDataProviderNotify = interface
  ['{5B9D1847-6D35-4D9C-8BC2-2054997AB120}']
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    function Consumer: IJvDataConsumer;
  end;

  IJvDataItems = interface
  ['{93747660-24FB-4294-BF4E-C7F88EA23983}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IJvDataItem;
    function GetItemByID(ID: string): IJvDataItem;
    function GetItemByIndexPath(IndexPath: array of Integer): IJvDataItem;
    function GetParent: IJvDataItem;
    function GetProvider: IJvDataProvider;
    function GetImplementer: TObject;
    function IsDynamic: Boolean;
    procedure ContextDestroying(Context: IJvDataContext);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IJvDataItem read GetItem;
    property Parent: IJvDataItem read GetParent;
    property Provider: IJvDataProvider read GetProvider;
  end;

  IJvDataItemsImages = interface
  ['{735755A6-AD11-460C-B985-46464D73EDBC}']
    function GetDisabledImages: TCustomImageList;
    procedure SetDisabledImages(const Value: TCustomImageList);
    function GetHotImages: TCustomImageList;
    procedure SetHotImages(const Value: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    property HotImages: TCustomImageList read GetHotImages write SetHotImages;
    property Images: TCustomImageList read GetImages write SetImages;
  end;

  IJvDataItemsRenderer = interface
    ['{4EA490F4-7CCF-44A1-AA26-5320CDE9FAFC}']
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer; State: TProviderDrawStates);
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TProviderDrawStates);
    function MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
    function AvgItemSize(ACanvas: TCanvas): TSize;
  end;

  IJvDataItemsManagement = interface
  ['{76611CC0-9DCD-4394-8B6E-1ADEF1942BC3}']
    function Add(Item: IJvDataItem): IJvDataItem;
    function New: IJvDataItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Remove(var Item: IJvDataItem);
  end;

  IJvDataItemsDesigner = interface
    ['{31B2544C-8E4F-40FE-94B8-04243EF40821}']
    function GetCount: Integer;
    function GetKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IJvDataItem;
  end;

  { base search interface. Can be supported by IJvDataItems implementers if the implementation needs
    it.

    The basic idea is to declare additional interfaces to implement searching on other properties
    as well. eg. for the color filler:

      IDataColorSearch = interface
        function IndexOfTColor(Color: TColor; const Recursive: Boolean = False): Integer;
      end;

    IJvDataItems implement those search interface that apply for the implementation.
    The recursive parameter has a default parameter value so it could be left out. }

  IJvDataIDSearch = interface
    ['{0F5BDC79-893B-45C9-94E9-C2B2FD4ABFE7}']
    function Find(ID: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataTextSearch = interface
  ['{E3BC388D-50F6-402D-9E30-36D5F7F40616}']
    function Find(Text: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataItem = interface
  ['{C965CF64-A1F2-44A4-B856-3A4EC6B693E1}']
    function GetItems: IJvDataItems;
    function GetIndex: Integer;
    function GetImplementer: TObject;
    function GetID: string;
    procedure ContextDestroying(Context: IJvDataContext);
    function IsParentOf(AnItem: IJvDataItem; DirectParent: Boolean = False): Boolean;
    function IsDeletable: Boolean;
    property Items: IJvDataItems read GetItems;
    property Implementer: TObject read GetImplementer;
  end;

  { Rendering interface for an item. Provides support for both rendering and measuring of the item.
    Implemented by IJvDataItem. }
  IJvDataItemRenderer = interface
    ['{9E877A0D-01C2-4204-AA74-84D6516BBEB9}']
    { Render the item to the specified canvas in the specified rectangle. }
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates);
    { Measure the item. }
    function Measure(ACanvas: TCanvas): TSize;
  end;

  { Supported by the IJvDataItem implementer if it supports text. }
  IJvDataItemText = interface
  ['{94FA56D9-281B-4252-B46D-15E7BADA70DA}']
    { Retrieve the text of the item. }
    function GetCaption: string;
    { Set the text of the item. }
    procedure SetCaption(const Value: string);
    { Determines if the item's caption can be modified by a UI control (such as an edit control). }
    function Editable: Boolean;
    { Get or set the text of the item. }
    property Caption: string read GetCaption write SetCaption;
  end;

  IJvDataItemImage = interface
  ['{6425D73A-90CF-42ED-9AB2-63125A4C0774}']
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetImageIndex: Integer;
    procedure SetImageIndex(Index: Integer);
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(Value: Integer);
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

  IJvDataItemBasicAction = interface
  ['{86859A20-560D-4E9A-AC8B-2457789451B0}']
    function Execute(Sender: TObject): Boolean;
  end;

  IJvDataItemStates = interface
  ['{5BD81E0B-DAD2-4560-943A-205E0FF2A97F}']
    function Get_Enabled: TDataItemState;
    procedure Set_Enabled(Value: TDataItemState);
    function Get_Checked: TDataItemState;
    procedure Set_Checked(Value: TDataItemState);
    function Get_Visible: TDataItemState;
    procedure Set_Visible(Value: TDataItemState);
    property Enabled: TDataItemState read Get_Enabled write Set_Enabled;
    property Checked: TDataItemState read Get_Checked write Set_Checked;
    property Visible: TDataItemState read Get_Visible write Set_Visible;
  end;

  { Support interface for provider editor. Must be implemented by IJvDataItem implementers who
    allow their item to be edited in the provider editor. }
  IJvDataItemDesigner = interface
    ['{8F1A1283-2D13-4A28-9616-08B3EF73F29A}']
    { Number of actions/menu items. }
    function GetVerbCount: Integer;
    function GetVerb(Index: Integer; out Caption: string; out Enabled, Checked, Visible,
      RadioItem: Boolean): Boolean;
    function ExecVerb(Index: Integer): Boolean;
  end;

  IJvDataContextSensitive = interface
    ['{7067F5C1-05DC-4DAC-A595-AF9151695FBB}']
    procedure RevertToAncestor;
    function IsEqualToAncestor: Boolean;
  end;

  {$IFNDEF COMPILER6_UP}
  { Needed in D5 to use components with interface. Declaration copied from D6 Classes.pas. See
    various filler implementations for details. This declaration should probably be moved to
    JvTypes or JvComponents. }
  IInterfaceComponentReference = interface
    ['{E28B1858-EC86-4559-8FCD-6B4F824151ED}']
    function GetComponent: TComponent;
  end;
  {$ENDIF COMPILER6_UP}

  IJvDataConsumer = interface
    ['{B2F18D03-F615-4AA2-A51A-74D330C05C0E}']
    function VCLComponent: TComponent;
    function AttributeApplies(Attr: Integer): Boolean;
  end;

  { Consumer support interface to retrieve the selected provider. Used by the various property
    editors. }
  IJvDataConsumerProvider = interface
    ['{1F01D2E5-2ACB-4B84-AFE6-67E563FB470B}']
    function GetProvider: IJvDataProvider;
  end;

  { Consumer support interface to retrieve or set the selected context. Used by the various
    property editors. }
  IJvDataConsumerContext = interface
    ['{7AA9F53D-BBD4-4B64-916A-AAF4AB25A496}']
    function GetContext: IJvDataContext;
    procedure SetContext(Value: IJvDataContext);
  end;

  { Consumer support interface to retrieve the state of an item as specified by the consumer.
    disNotUsed means the state is not supported by the consumer and should be taken from the
    item instead. }
  IJvDataConsumerItemState = interface
    ['{09EBDED8-502E-4C2E-9842-312850FF3358}']
    function Enabled(Item: IJvDataItem): TDataItemState;
    function Checked(Item: IJvDataItem): TDataItemState;
    function Visible(Item: IJvDataItem): TDataItemState;
  end;

  { Consumer support interface to get or set the (root) item to render. Consumers that only support
    showing a single item use this interface to specify which item is to be displayed, while
    consumers that can render trees use it to specify the root item to show. }
  IJvDataConsumerItemSelect = interface
    ['{F11554AE-263D-4C04-BCDB-79F04DE89609}']
    function GetItem: IJvDataItem;
    procedure SetItem(Value: IJvDataItem);
  end;

  IJvDataConsumerViewList = interface
    ['{F3A78F68-D998-4877-8C73-1E0D2987808D}']
    function Get_AutoExpandLevel: Integer;
    procedure Set_AutoExpandLevel(Value: Integer);
    function Get_ExpandOnNewItem: Boolean;
    procedure Set_ExpandOnNewItem(Value: Boolean);
    function Get_LevelIndent: Integer;
    procedure Set_LevelIndent(Value: Integer);
    procedure RebuildView;
    procedure ExpandTreeTo(Item: IJvDataItem);
    { Toggles an item's expanded state. If an item becomes expanded, the item's sub item as present
      in the IJvDataItems instance will be added; if an item becomes collapsed the sub items are
      removed from the view. }
    procedure ToggleItem(Index: Integer);
    { Locate an item in the view list, returning it's absolute index. }
    function IndexOfItem(Item: IJvDataItem): Integer;
    { Locate an item ID in the view list, returning it's absolute index. }
    function IndexOfID(ID: TJvDataItemID): Integer;
    { Locate an item in the view list, returning it's index in the parent item. }
    function ChildIndexOfItem(Item: IJvDataItem): Integer;
    { Locate an item ID in the view list, returning it's index in the parent item. }
    function ChildIndexOfID(ID: TJvDataItemID): Integer;
    { Retrieve the IJvDataItem reference given the absolute index into the view list. }
    function Item(Index: Integer): IJvDataItem;
    { Retrieve an items level given the absolute index into the view list. }
    function ItemLevel(Index: Integer): Integer;
    { Retrieve an items expanded state given the absolute index into the view list. }
    function ItemIsExpanded(Index: Integer): Boolean;
    { Determine if an item has children given the absolute index into the view list. }
    function ItemHasChildren(Index: Integer): Boolean;
    { Retrieve an items parent given the absolute index into the view list. }
    function ItemParent(Index: Integer): IJvDataItem;
    { Retrieve an items parent absolute index given the absolute index into the view list. }
    function ItemParentIndex(Index: Integer): Integer;
    { Retrieve an items sibling given an absolute index. }
    function ItemSibling(Index: Integer): IJvDataItem; 
    { Retrieve the index of an items sibling given an absolute index. }
    function ItemSiblingIndex(Index: Integer): Integer;
    { Retrieve the IJvDataItem reference given the child index and a parent item. }
    function SubItem(Parent: IJvDataItem; Index: Integer): IJvDataItem; overload;
    { Retrieve the IJvDataItem reference given the child index and a parent absolute index. }
    function SubItem(Parent, Index: Integer): IJvDataItem; overload;
    { Retrieve the absolute index given a child index and a parent item. }
    function SubItemIndex(Parent: IJvDataItem; Index: Integer): Integer; overload;
    { Retrieve the absolute index given a child index and a parent absolute index. }
    function SubItemIndex(Parent, Index: Integer): Integer; overload;
    { Retrieve info on grouping; each bit represents a level, if the bit is set the item at that
      level has another sibling. Can be used to render tree lines. }
    function ItemGroupInfo(Index: Integer): TDynIntegerArray;
    { Retrieve the number of viewable items. }
    function Count: Integer;
    { Level of auto expanding. -1 is infinite, 0 is none. }
    property AutoExpandLevel: Integer read Get_AutoExpandLevel write Set_AutoExpandLevel;
    { Expand automatically if an item is added to the provider and it would not be visible by
      default (i.e. one of the parents are still in collapsed state). }
    property ExpandOnNewItem: Boolean read Get_ExpandOnNewItem write Set_ExpandOnNewItem;
    { Indentation in pixels for each level. Used by the rendering engine. }
    property LevelIndent: Integer read Get_LevelIndent write Set_LevelIndent;
  end;

  { Consumer support interface to be used as a callback for IJvDataConsumerClientNotify
    implementations. Used to add or remove the link, as well as be notified if a change occured
    at the client that may result in the link being removed (at the discretion of the server
    implementation) }
  IJvDataConsumerServerNotify = interface
    ['{636CF1CD-6A5A-414F-9506-EAC461202119}']
    { Add a client notifier, to be called when this consumer changes. Will call Client.LinkAdded
      when the link was succesful. }
    procedure AddClient(Client: IJvDataConsumerClientNotify);
    { Remove a client notifier. Will call Client.LinkRemoved when done. }
    procedure RemoveClient(Client: IJvDataConsumerClientNotify);
    { Called when the consumer belonging to the client has selected another provider. The server
      may at this point decide to remove the link or keep it. }
    procedure NotifyProviderChanged(Client: IJvDataConsumerClientNotify);
    { Determine if the specified client is valid for this server. }
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
  end;

  { Consumer support interface to be notified if another consumer has changed it's selected
    (current) item. Used to react to changes of another consumer. For example, if another consumer
    lists contexts related to this consumer and the context list consumer selects a different
    context, this interface's implementer can switch the context of it's own consumer. }
  IJvDataConsumerClientNotify = interface
    ['{D1AAAFDF-BEB1-44DB-B8D8-A60080CEF3C7}']
    { Called when the server consumer has selected another item. }
    procedure ItemSelected(Server: IJvDataConsumerServerNotify; Value: IJvDataItem);
    { Called when the server has added this client to it's list. The provided Server interface
      can be used to notify the server if another provider is selected for this consumer or remove
      the link if the consumer is ending. }
    procedure LinkAdded(Server: IJvDataConsumerServerNotify);
    { Called when the server has disconnected this client. }
    procedure LinkRemoved(Server: IJvDataConsumerServerNotify);
  end;

  { Provider context list interface. Note that there is always an implicit (nameless) context
    at the provider, even if there is no IJvDataContexts interface available. }
  IJvDataContexts = interface
    ['{BA5DC787-29C6-40FA-9542-F0A1E92A2B30}']
    { Reference to the provider. }
    function Provider: IJvDataProvider;
    { Reference to the parent context (ancestor context). }
    function Ancestor: IJvDataContext;
    { Number of contexts. }
    function GetCount: Integer;
    { Array of available contexts. }
    function GetContext(Index: Integer): IJvDataContext;
    { Retrieve a context by name. Returns nil if the context does not exist. Name may be a path to
      the context (e.g.: 'Context1\Context1.1\Context1.1.3' or '..\Context1.2') }
    function GetContextByName(Name: string): IJvDataContext;
    { Retrieve the index of the specified context. If the context does not belong to this list the
      function will return -1. }
    function IndexOf(Ctx: IJvDataContext): Integer;
  end;

  { Support interface for IJvDataContexts to allow adding/deleting contexts. }
  IJvDataContextsManager = interface
    ['{A94D62CA-F9B4-4DAA-9091-86D01A962BB1}']
    { Add a context. }
    function Add(Context: IJvDataContext): IJvDataContext;
    { Create a new context and add it to the list. }
    function New: IJvDataContext;
    { Delete the specified context. }
    procedure Delete(Context: IJvDataContext);
    { Clear the list of contexts. }
    procedure Clear;
  end;

  IJvDataContext = interface
    ['{F226D92A-3493-4EF8-9CE6-037357EB0CEA}']
    { Retrieve the implementing instance. }
    function GetImplementer: TObject;
    { Reference to the context manager. }
    function Contexts: IJvDataContexts;
    { Unique name of the context (used at design time and by the streaming system). }
    function Name: string;
    { Determines if the context can be deleted. Some providers may generate some fixed contexts that
      should never be deleted if the Context Manager is used. }
    function IsDeletable: Boolean;
  end;

  IJvDataContextManager = interface
    ['{530367D8-601C-4E36-B5F0-357160497C50}']
    { Allows the name to be changed. }
    procedure SetName(Value: string);
  end;

  EJVCLDataProvider = class(EJVCLException);
  EJVCLDataConsumer = class(EJVCLDataProvider);
  EJVCLDataItems = class(EJVCLDataProvider);
  EJVCLDataContexts = class(EJVCLDataProvider);

implementation

end.


