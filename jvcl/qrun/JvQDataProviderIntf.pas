{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProvider.pas, released on 2003-12-24.

The Initial Developers of the Original Code are Marcel Bestebroer, Peter
Thörnqvist and Remko Bonte
Portions created by these individuals are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQDataProviderIntf;

interface

uses
  Classes,
  
  
  Types, QGraphics, QImgList,
  
  JclBase, JvQTypes;

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

  IJvDataItemRenderer = interface
    ['{9E877A0D-01C2-4204-AA74-84D6516BBEB9}']
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates);
    function Measure(ACanvas: TCanvas): TSize;
  end;

  IJvDataItemText = interface
    ['{94FA56D9-281B-4252-B46D-15E7BADA70DA}']
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
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

  IJvDataItemDesigner = interface
    ['{8F1A1283-2D13-4A28-9616-08B3EF73F29A}']
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

  

  IJvDataConsumer = interface
    ['{B2F18D03-F615-4AA2-A51A-74D330C05C0E}']
    function VCLComponent: TComponent;
    function AttributeApplies(Attr: Integer): Boolean;
  end;

  IJvDataConsumerProvider = interface
    ['{1F01D2E5-2ACB-4B84-AFE6-67E563FB470B}']
    function GetProvider: IJvDataProvider;
  end;

  IJvDataConsumerContext = interface
    ['{7AA9F53D-BBD4-4B64-916A-AAF4AB25A496}']
    function GetContext: IJvDataContext;
    procedure SetContext(Value: IJvDataContext);
  end;

  IJvDataConsumerItemState = interface
    ['{09EBDED8-502E-4C2E-9842-312850FF3358}']
    function Enabled(Item: IJvDataItem): TDataItemState;
    function Checked(Item: IJvDataItem): TDataItemState;
    function Visible(Item: IJvDataItem): TDataItemState;
  end;

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
    procedure ToggleItem(Index: Integer);
    function IndexOfItem(Item: IJvDataItem): Integer;
    function IndexOfID(ID: TJvDataItemID): Integer;
    function ChildIndexOfItem(Item: IJvDataItem): Integer;
    function ChildIndexOfID(ID: TJvDataItemID): Integer;
    function Item(Index: Integer): IJvDataItem;
    function ItemLevel(Index: Integer): Integer;
    function ItemIsExpanded(Index: Integer): Boolean;
    function ItemHasChildren(Index: Integer): Boolean;
    function ItemParent(Index: Integer): IJvDataItem;
    function ItemParentIndex(Index: Integer): Integer;
    function ItemSibling(Index: Integer): IJvDataItem;
    function ItemSiblingIndex(Index: Integer): Integer;
    function SubItem(Parent: IJvDataItem; Index: Integer): IJvDataItem; overload;
    function SubItem(Parent, Index: Integer): IJvDataItem; overload;
    function SubItemIndex(Parent: IJvDataItem; Index: Integer): Integer; overload;
    function SubItemIndex(Parent, Index: Integer): Integer; overload;
    function ItemGroupInfo(Index: Integer): TDynIntegerArray;
    function Count: Integer;
    property AutoExpandLevel: Integer read Get_AutoExpandLevel write Set_AutoExpandLevel;
    property ExpandOnNewItem: Boolean read Get_ExpandOnNewItem write Set_ExpandOnNewItem;
    property LevelIndent: Integer read Get_LevelIndent write Set_LevelIndent;
  end;

  IJvDataConsumerServerNotify = interface
    ['{636CF1CD-6A5A-414F-9506-EAC461202119}']
    procedure AddClient(Client: IJvDataConsumerClientNotify);
    procedure RemoveClient(Client: IJvDataConsumerClientNotify);
    procedure NotifyProviderChanged(Client: IJvDataConsumerClientNotify);
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
  end;

  IJvDataConsumerClientNotify = interface
    ['{D1AAAFDF-BEB1-44DB-B8D8-A60080CEF3C7}']
    procedure ItemSelected(Server: IJvDataConsumerServerNotify; Value: IJvDataItem);
    procedure LinkAdded(Server: IJvDataConsumerServerNotify);
    procedure LinkRemoved(Server: IJvDataConsumerServerNotify);
  end;

  IJvDataContexts = interface
    ['{BA5DC787-29C6-40FA-9542-F0A1E92A2B30}']
    function Provider: IJvDataProvider;
    function Ancestor: IJvDataContext;
    function GetCount: Integer;
    function GetContext(Index: Integer): IJvDataContext;
    function GetContextByName(Name: string): IJvDataContext;
    function IndexOf(Ctx: IJvDataContext): Integer;
  end;

  IJvDataContextsManager = interface
    ['{A94D62CA-F9B4-4DAA-9091-86D01A962BB1}']
    function Add(Context: IJvDataContext): IJvDataContext;
    function New: IJvDataContext;
    procedure Delete(Context: IJvDataContext);
    procedure Clear;
  end;

  IJvDataContext = interface
    ['{F226D92A-3493-4EF8-9CE6-037357EB0CEA}']
    function GetImplementer: TObject;
    function Contexts: IJvDataContexts;
    function Name: string;
    function IsDeletable: Boolean;
  end;

  IJvDataContextManager = interface
    ['{530367D8-601C-4E36-B5F0-357160497C50}']
    procedure SetName(Value: string);
  end;

  EJVCLDataProvider = class(EJVCLException);
  EJVCLDataConsumer = class(EJVCLDataProvider);
  EJVCLDataItems = class(EJVCLDataProvider);
  EJVCLDataContexts = class(EJVCLDataProvider);

implementation

end.

