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
Portions created by the individuals are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-06-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProvider;

interface

uses
  Windows, ImgList, Classes, Graphics;

type
  TDataProviderChangeReason = (pcrAdd, pcrDelete, pcrUpdateItem, pcrUpdateItems, pcrDestroy);
  TDataItemState = (disFalse, disTrue, disIndeterminate, disNotUsed);
  TProviderDrawState = (pdsSelected, pdsGrayed, pdsDisabled, pdsChecked, pdsFocused, pdsDefault,
    pdsHot);
  TProviderDrawStates = set of TProviderDrawState;
  TClassArray = array of TClass;

  // forwards
  IJvDataProvider = interface;
  IJvDataItems = interface;
  IJvDataItem = interface;
  IJvDataConsumer = interface;
  IJvDataProviderNotify = interface;
  IJvDataContexts = interface;
  IJvDataContext = interface;

  { base interface for components that supports storing lists of data (0..M items) }
  IJvDataProvider = interface
  ['{62A7A17D-1E21-427E-861D-C92FBB9B09A6}']
    { Register a notification listener (a client control) }
    procedure RegisterChangeNotify(ANotify: IJvDataProviderNotify);
    { Unregister a notification listener (a client control) }
    procedure UnregisterChangeNotify(ANotify: IJvDataProviderNotify);
    { Return a reference to the list of items at the root (you also use <ProviderIntf> as IJvDataItems) }
    function GetItems: IJvDataItems;
    { Notify clients a change is about to occur. }
    procedure Changing(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    { Notify clients a changes has just occurred. }
    procedure Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
    { Return an array of consumer setting classes to add to the consumer. Returned array may depend
      on the currently selected consumer/context pair. }
    function ConsumerClasses: TClassArray;
    { Selects a new consumer. The current consumer is pushed onto a stack. Use ReleaseConsumer to
      return to it. When the provider is created no consumer is selected. }
    procedure SelectConsumer(Consumer: IJvDataConsumer);
    { Currently selected consumer. }
    function SelectedConsumer: IJvDataConsumer;
    { Deselect the current consumer and revert back to the consumer that was selected before it. }
    procedure ReleaseConsumer;
    { Selects a new context. The current context is pushed onto a stack. Use ReleaseContext to
      return to it. When the provider is create it's implicit context is preselected. If you select
      a nil context the data provider implicit context is selected. }
    procedure SelectContext(Context: IJvDataContext);
    { Currently selected context. }
    function SelectedContext: IJvDataContext;
    { Deselect the current context and revert back to the context that was selected before it. }
    procedure ReleaseContext;
  end;

  { Implemented by clients (i.e list/comboboxes, labels, buttons, edits, listviews, treeviews, menus etc)
   to get notifications from IFiller }
  IJvDataProviderNotify = interface
  ['{5B9D1847-6D35-4D9C-8BC2-2054997AB120}']
    { Called when a change is about to occur at the provider. }
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    { Called when a change has occurred at the provider. }
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
  end;

  { Item list. (0..N items)
    Must be supported/implemented by IJvDataProvider implementers.
    May be Supported by IJvDataItem implementers. }
  IJvDataItems = interface
  ['{93747660-24FB-4294-BF4E-C7F88EA23983}']
    { Retrieves the number of items in the list. }
    function GetCount: Integer;
    { Retrieve an item in the list }
    function GetItem(Index: Integer): IJvDataItem;
    { Reference to the parent item or nil if this list is the root list (i.e. implemented at the
      IJvDataProvider level). }
    function GetParent: IJvDataItem;
    { Reference to the data provider instance. }
    function GetProvider: IJvDataProvider;
    { Reference to the implementing object. }
    function GetImplementer: TObject;
    { Determines if the list is a dynamic list of items (i.e. items are generated as needed and
      disposed of when the last reference to it goes out of scope). }
    function IsDynamic: Boolean;

    { Number of items in the list }
    property Count: Integer read GetCount;
    { Array of list items. }
    property Items[Index: Integer]: IJvDataItem read GetItem;
    { Reference to the parent item or nil if this list is the root list (i.e. implemented at the
      IJvDataProvider level). }
    property Parent: IJvDataItem read GetParent;
    { Reference to the data provider instance. }
    property Provider: IJvDataProvider read GetProvider;
  end;

  { May be supported by IJvDataItems implementers. }
  IJvDataItemsImages = interface
  ['{735755A6-AD11-460C-B985-46464D73EDBC}']
    function GetDisabledImages: TCustomImageList;
    procedure SetDisabledImages(const Value: TCustomImageList);
    function GetHotImages: TCustomImageList;
    procedure SetHotImages(const Value: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);

    { Get or set the image list to use when an item is disabled. If left unassigned, the standard
      image list is used (set by Images). }
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    { Get or set the image list to use when an item is 'hot'. If left unassigned, the standard image
      list is used (set by Images). }
    property HotImages: TCustomImageList read GetHotImages write SetHotImages;
    { Get or set the image list to use. }
    property Images: TCustomImageList read GetImages write SetImages;
  end;

  { Rendering interface. Provides support for both rendering and measuring of items.
    Implemented by IFillerItems. }
  IJvDataItemsRenderer = interface
    ['{4EA490F4-7CCF-44A1-AA26-5320CDE9FAFC}']
    { Draw an item in the IJvDataItems list, using an index to specify which item. }
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer; State: TProviderDrawStates);
    { Measure an item in the IJvDataItems list, using an index to specify which item. }
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
    { Draw the specified item. If the item is not part of the IJvDataItems list or nil is specified, an exception will be raised. }
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TProviderDrawStates);
    { Measure the specified item. If the item is not part of the IJvDataItems list or nil is specified, an exception will be raised. }
    function MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
    { Retrieve the average size of the items in the list. This depends on the implementation on how
      this value is determined (either by iterating over the items and calculate the real average
      or by assuming data depending on current font, etc). }
    function AvgItemSize(ACanvas: TCanvas): TSize;
  end;

  { Implemented by servers that allows editing the list. Supported by IJvDataItems implementers. }
  IJvDataItemsManagement = interface
  ['{76611CC0-9DCD-4394-8B6E-1ADEF1942BC3}']
    { Add the specified item to the list. }
    function Add(Item: IJvDataItem): IJvDataItem;
    { Create a new item and add it to the list. }
    function New: IJvDataItem;
    { Clear the list of items. }
    procedure Clear;
    { Delete the item at the given index. }
    procedure Delete(Index: Integer);
    { Remove the specified item from the list. }
    procedure Remove(Item: IJvDataItem);
  end;

  { Support interface for provider editor. May be implemented by IJvDataItemsManagement implementers
    who allow their list/tree to be edited. }
  IJvDataItemsDesigner = interface
    ['{31B2544C-8E4F-40FE-94B8-04243EF40821}']
    { Number of item types the designer can support. }
    function GetCount: Integer;
    { Retrieve the name of item type specified by the index. If the Index is out of range, False
      is returned, otherwise True is returned and Caption contains the type name. }
    function GetKind(Index: Integer; out Caption: string): Boolean;
    { Create and add a new item based on the specified type. }
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

  { base item interface: holds reference to the IJvDataItems owner as well as provide a reference to
    the implementer. }
  IJvDataItem = interface
  ['{C965CF64-A1F2-44A4-B856-3A4EC6B693E1}']
    { Retrieve the reference to the IJvDataItems owner. }
    function GetItems: IJvDataItems;
    { Retrieve a reference to the implementing object. }
    function GetImplementer: TObject;
    { Retrieve the items ID string. }
    function GetID: string;

    { Reference to the IJvDataItems owner. }
    property Items: IJvDataItems read GetItems;
    { Reference to the implementing object. }
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
    { Get or set the text of the item. }
    property Caption: string read GetCaption write SetCaption;
  end;

  { supported by the IJvDataItem implementer if it supports images.
    Note that the IJvDataItems owner needs to implement the IJvDataItemImages interface as well. }
  IJvDataItemImage = interface
  ['{6425D73A-90CF-42ED-9AB2-63125A4C0774}']
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetImageIndex: Integer;
    procedure SetImageIndex(Index: Integer);
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(Value: Integer);

    { Alignment to use for the image (left of the item, center of the item, right of the item). Note
      that center alignment means the optional text should be rendered below the image }
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    { Index into the image list of the image to render. }
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    { Index into the image list of the image to render when the item is selected. }
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

  { implemented by servers that supports a default action for an item. }
  IJvDataItemBasicAction = interface
  ['{86859A20-560D-4E9A-AC8B-2457789451B0}']
    { Called in response to a click/double click of an item. Sender is the control that initiated
      the action. Result is set to False when nothing happened and set to True when the action was
      executed. }
    function Execute(Sender: TObject): Boolean;
  end;

  { Implemented by servers that support one or more states for an item. }
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

  {$IFNDEF COMPILER6_UP}
  { Needed in D5 to use components with interface. Declaration copied from D6 Classes.pas. See
    various filler implementations for details. This declaration should probably be moved to
    JvTypes or JvComponents. }
  IInterfaceComponentReference = interface
    ['{E28B1858-EC86-4559-8FCD-6B4F824151ED}']
    function GetComponent: TComponent;
  end;
  {$ENDIF}

  { Interface used to link back to the data consumer (the component/control). It's primary use is
    to retrieve a reference to VCLComponent (if any) and check if certain attributes apply for a
    consumer. Other interfaces maybe supported, depending on the control. Not all providers may be
    interested in all the interfaces or even who the consumer is. }
  IJvDataConsumer = interface
    ['{B2F18D03-F615-4AA2-A51A-74D330C05C0E}']
    function VCLComponent: TComponent;
    { Checks if a certain attribute applies (attributes are declared in JvDataProviderConsts). If
      the specified attribute applies for the consumer True should be returned False otherwise. }
    function AttributeApplies(Attr: Integer): Boolean;
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

  { Provider context management interface. Note that there is always an implicit (nameless) context
    at the provider, even if there is no IJvDataContexts interface available. }
  IJvDataContexts = interface
    ['{BA5DC787-29C6-40FA-9542-F0A1E92A2B30}']
    { Reference to the provider. }
    function Provider: IJvDataProvider;
    { Number of contexts. }
    function GetCount: Integer;
    { Array of available contexts. }
    function GetContext(Index: Integer): IJvDataContext;
    { Retrieve a context by name. Returns nil if the context does not exist. }
    function GetContextByName(Name: string): IJvDataContext;
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
    { Reference to the context manager (this will be nil for the implicit context of the provider). }
    function Contexts: IJvDataContexts;
    { Unique name of the context (used at design time and by the streaming system). }
    function Name: string;
  end;

implementation

end.


