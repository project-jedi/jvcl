{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
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

Last Modified: 2003-12-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQDataProviderIntf;

interface

uses
  
  
  Types, QGraphics, QImgList,
  
  Classes,
  JclBase,
  JvQTypes;

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
    
    
    ['{FF97DB5E-73D0-4D47-AFDA-0564C568BA93}']
    
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
    
    
    ['{82DB52CA-C680-47C8-A747-5E8E10C879CA}']
    
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    function Consumer: IJvDataConsumer;
  end;

  IJvDataItems = interface
    
    
    ['{B5503745-873F-4B33-B2BC-8C7B9378B816}']
    
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
    
    
    ['{DC8A0887-9A79-4557-99EC-6BB2BB923A40}']
    

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
    
    
    ['{3C0F0964-E0CF-4EBD-892E-F83D70BC44BA}']
    
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer; State: TProviderDrawStates);
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TProviderDrawStates);
    function MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
    function AvgItemSize(ACanvas: TCanvas): TSize;
  end;

  IJvDataItemsManagement = interface
    
    
    ['{9E857FAB-67B3-459A-AC19-759069122597}']
    

    function Add(Item: IJvDataItem): IJvDataItem;
    function New: IJvDataItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Remove(var Item: IJvDataItem);
  end;

  IJvDataItemsDesigner = interface
    
    
    ['{F0AD7891-8EA4-4553-8F31-F2B88B88B5F2}']
    
    function GetCount: Integer;
    function GetKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IJvDataItem;
  end;

  IJvDataIDSearch = interface
    
    
    ['{4EFC2D23-B76E-4AAA-9803-BAC20F754A6E}']
    
    function Find(ID: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataTextSearch = interface
    
    
    ['{548887D7-E10F-44C6-9FB7-72C83E77AE30}']
    

    function Find(Text: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataItem = interface
    
    
    ['{7052A0F7-73EB-4980-AE85-399D970BE291}']
    
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
    
    
    ['{E55FCE97-339F-4724-98BF-C128E5FF5729}']
    
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TProviderDrawStates);
    function Measure(ACanvas: TCanvas): TSize;
  end;

  IJvDataItemText = interface
    
    
    ['{170E40A0-6BE2-4578-BAF1-4EDE2FA8E549}']
    
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
    property Caption: string read GetCaption write SetCaption;
  end;

  IJvDataItemImage = interface
    
    
    ['{25E45D22-B910-4CA5-85D6-0F66FEE239A3}']
    
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
    
    
    ['{7B6239D1-F94C-4E9F-B0D8-22FA0663A656}']
    
    function Execute(Sender: TObject): Boolean;
  end;

  IJvDataItemStates = interface
    
    
    ['{8D665C15-C519-4F5A-AB51-715ECC6C88E1}']
    
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
    
    
    ['{11E15F61-F2E5-4958-B3ED-DE1C14FA5FA0}']
    
    function GetVerbCount: Integer;
    function GetVerb(Index: Integer; out Caption: string; out Enabled, Checked, Visible,
      RadioItem: Boolean): Boolean;
    function ExecVerb(Index: Integer): Boolean;
  end;

  IJvDataContextSensitive = interface
    
    
    ['{DADF606F-BA0F-410A-87AF-DED9F9D09A2D}']
    
    procedure RevertToAncestor;
    function IsEqualToAncestor: Boolean;
  end;

  

  IJvDataConsumer = interface
    
    
    ['{AD1CC0CE-81A6-49D6-81E1-28EB83102522}']
    
    function VCLComponent: TComponent;
    function AttributeApplies(Attr: Integer): Boolean;
  end;

  IJvDataConsumerProvider = interface
    
    
    ['{43A5B0F7-DDA0-447D-A178-8F8E77440EB3}']
    
    function GetProvider: IJvDataProvider;
  end;

  IJvDataConsumerContext = interface
    
    
    ['{AFDA7988-415E-4B81-BE93-8D01E2E271F3}']
    
    function GetContext: IJvDataContext;
    procedure SetContext(Value: IJvDataContext);
  end;

  IJvDataConsumerItemState = interface
    
    
    ['{9777CD34-19B2-44FA-9E6B-7B754195F985}']
    
    function Enabled(Item: IJvDataItem): TDataItemState;
    function Checked(Item: IJvDataItem): TDataItemState;
    function Visible(Item: IJvDataItem): TDataItemState;
  end;

  IJvDataConsumerItemSelect = interface
    
    
    ['{5CD1C5E9-5F54-4590-9057-5C8700F3B30D}']
    
    function GetItem: IJvDataItem;
    procedure SetItem(Value: IJvDataItem);
  end;

  IJvDataConsumerViewList = interface
    
    
    ['{812AB5BD-0EE5-4A6A-9304-3507102F208A}']
    
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
    
    
    ['{5D5A20F7-C541-4F6F-A068-C04FF0295B98}']
    
    procedure AddClient(Client: IJvDataConsumerClientNotify);
    procedure RemoveClient(Client: IJvDataConsumerClientNotify);
    procedure NotifyProviderChanged(Client: IJvDataConsumerClientNotify);
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
  end;

  IJvDataConsumerClientNotify = interface
    
    
    ['{F75820BB-E565-4771-AFEB-3F2E94DDE36A}']
    
    procedure ItemSelected(Server: IJvDataConsumerServerNotify; Value: IJvDataItem);
    procedure LinkAdded(Server: IJvDataConsumerServerNotify);
    procedure LinkRemoved(Server: IJvDataConsumerServerNotify);
  end;

  IJvDataContexts = interface
    
    
    ['{9AC039D7-2AE0-4848-86F5-EBBB662DA3BE}']
    
    function Provider: IJvDataProvider;
    function Ancestor: IJvDataContext;
    function GetCount: Integer;
    function GetContext(Index: Integer): IJvDataContext;
    function GetContextByName(Name: string): IJvDataContext;
    function IndexOf(Ctx: IJvDataContext): Integer;
  end;

  IJvDataContextsManager = interface
    
    
    ['{0C3877D1-596F-411A-824C-E47886174B48}']
    
    function Add(Context: IJvDataContext): IJvDataContext;
    function New: IJvDataContext;
    procedure Delete(Context: IJvDataContext);
    procedure Clear;
  end;

  IJvDataContext = interface
    
    
    ['{B9D0DF44-ECEB-41AF-9503-FC5B95A48955}']
    
    function GetImplementer: TObject;
    function Contexts: IJvDataContexts;
    function Name: string;
    function IsDeletable: Boolean;
  end;

  IJvDataContextManager = interface
    
    
    ['{83ACF887-A6D6-437D-B8A5-2A24EB5777FC}']
    
    procedure SetName(Value: string);
  end;

  EJVCLDataProvider = class(EJVCLException);
  EJVCLDataConsumer = class(EJVCLDataProvider);
  EJVCLDataItems = class(EJVCLDataProvider);
  EJVCLDataContexts = class(EJVCLDataProvider);

implementation

end.

