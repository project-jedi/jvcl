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

The Original Code is: JvColorProvider.pas, released on 2003-07-18.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQColorProvider;

interface

uses
  Types, QWindows, Classes, Contnrs, QGraphics, QDialogs,
  JclBase,
  JvQDataProvider, JvQDataProviderIntf, JvQTypes;

type
  TJvColorProvider = class;
  TJvColorProviderNameMappings = class;
  TJvColorProviderNameMapping = class;
  IJvColorProvider = interface;

  TColorItem = record
    Value: TColor;
    Names: TDynStringArray;
  end;

  TColorItems = array of TColorItem;
  TJvColorProviderMapping = type Integer;
  TJvColorProviderAddItemLocation = (ailUseHeader, ailTop, ailBottom);
  TJvColorProviderAddColorStyle = type Integer;
  TColorGroupHeaderAlign = (ghaLeft, ghaCenter, ghaColorText);
  TColorGroupHeaderStyle = (ghsBoldFont, ghsSingleCenterLine, ghsDoubleCenterLine);
  TColorGroupHeaderStyles = set of TColorGroupHeaderStyle;

  TJvColorProviderColorAdder = procedure(Provider: IJvColorProvider; ColorType: TColorType;
    var Color: TColor; var DoAdd: Boolean);
  TJvColorProviderAddColorEvent = procedure(Provider: TJvColorProvider; ColorType: TColorType;
    var Color: TColor; var DoAdd: Boolean) of object;

  IJvColorProvider = interface
    ['{3DF32721-553B-4759-A628-35F5CA62F3D5}']
    procedure DoAddColor(ColorType: TColorType; var Color: TColor; var DoAdd: Boolean);
    function AddColor(ColorType: TColorType; Color: TColor): Boolean;
    function IndexOfMapping(Mapping: TJvColorProviderNameMapping): Integer;
    function IndexOfMappingName(Name: string): Integer;
    function Get_MappingCount: Integer;
    function Get_Mapping(Index: Integer): TJvColorProviderNameMapping;
    function AddMapping(AName: string): Integer;
    function NewMapping: Integer;
    procedure DeleteMapping(Index: Integer);
    function GetStandardCount: Integer;
    function GetSystemCount: Integer;
    function GetCustomCount: Integer;
    function GetStandardColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function GetSystemColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function GetCustomColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function FindColor(Value: TColor; out ColorType: TColorType; out Index: Integer): Boolean;
    procedure SetStandardColorName(Index: Integer; NewName: string);
    procedure SetSystemColorName(Index: Integer; NewName: string);
    procedure SetCustomColorName(Index: Integer; NewName: string);
    function AddStdColor(Value: TColor): Boolean;
    procedure DeleteStdColor(Value: TColor);
    procedure DeleteStdColorAt(Index: Integer);
    procedure ClearStdColorList;
    function AddSysColor(Value: TColor): Boolean;
    procedure DeleteSysColor(Value: TColor);
    procedure DeleteSysColorAt(Index: Integer);
    procedure ClearSysColorList;
    function AddCstColor(Value: TColor): Boolean;
    procedure DeleteCstColor(Value: TColor);
    procedure DeleteCstColorAt(Index: Integer);
    procedure ClearCstColorList;

    property MappingCount: Integer read Get_MappingCount;
    property Mappings[Index: Integer]: TJvColorProviderNameMapping read Get_Mapping;
  end;

  IJvColorMappingProvider = interface
    ['{B6BA8036-8ECF-463B-BAD3-6855D4845F3F}']
    function Get_ClientProvider: IJvColorProvider;
    procedure Set_ClientProvider(Value: IJvColorProvider);

    property ClientProvider: IJvColorProvider read Get_ClientProvider write Set_ClientProvider;
  end;

  IJvColorItem = interface
    ['{ED95EC41-EEE2-4E14-ABF6-5B7B5EA47FFF}']
    function Get_Color: TColor;

    property Color: TColor read Get_Color;
  end;

  IJvColorMapItem = interface
    ['{8906A180-71C0-4DF6-9029-5513B341541E}']
    function Get_NameMapping: TJvColorProviderNameMapping;

    property NameMapping: TJvColorProviderNameMapping read Get_NameMapping;
  end;

  TJvColorProvider = class(TJvCustomDataProvider, IJvColorProvider)
  private
    FColorList: TColorItems;                // all colors the provider knows about
    FStdColors: array of TDynIntegerArray;  // list of standard colors for each context
    FSysColors: array of TDynIntegerArray;  // list of system colors for each context
    FCstColors: array of TDynIntegerArray;  // list of custom colors for each context
    FMappings: TJvColorProviderNameMappings;
    FColorListChanged: Boolean;             // Flag to keep track of changes in FColorList w/resp. to the default
    FOnAddColor: TJvColorProviderAddColorEvent;
  protected
    { Notify any registered notifiers something is changing. For each notifier that is also a
      consumer, that consumer will only be notified if it has the proper context. The settings
      determine if either the GroupedItem or RootItem is specified as the Source. Any non-consumer
      or consumers without a context or settings interface will receive the GroupedItem as the
      Source parameter. }
    procedure NotifyChanging(Reason: TDataProviderChangeReason; ContextIndex: Integer; GroupedItem,
      RootItem: IUnknown);
    { Notify any registered notifiers something has changed. For each notifier that is also a
      consumer, that consumer will only be notified if it has the proper context. The settings
      determine if either the GroupedItem or RootItem is specified as the Source. Any non-consumer
      or consumers without a context or settings interface will receive the GroupedItem as the
      Source parameter. }
    procedure NotifyChanged(Reason: TDataProviderChangeReason; ContextIndex: Integer; GroupedItem,
      RootItem: IUnknown);
    { Notify all registered notifiers a color is being added to the specified group. }
    procedure NotifyColorAdding(ColorType: TColorType; ContextIndex: Integer = -1);
    { Notify all registered notifiers a color has been added to the specified group. }
    procedure NotifyColorAdded(ColorType: TColorType; ListIndex: Integer; ContextIndex: Integer = -1);
    { Notify all registered notifiers a color is being removed from the specified group. }
    procedure NotifyColorDeleting(ColorType: TColorType; ListIndex: Integer; ContextIndex: Integer = -1);
    { Notify all registered notifiers a color has been removed to the specified group. }
    procedure NotifyColorDeleted(ColorType: TColorType; ContextIndex: Integer = -1);
    { Notify all registered notifiers a color is being removed from the specified group. }
    procedure NotifyColorsUpdating(ColorType: TColorType; ContextIndex: Integer = -1);
    { Notify all registered notifiers a color has been removed to the specified group. }
    procedure NotifyColorsUpdated(ColorType: TColorType; ContextIndex: Integer = -1);

    procedure DoAddColor(ColorType: TColorType; var Color: TColor; var DoAdd: Boolean);
    function AddInternalColor(Color: TColor; AddToCustomDefaultList: Boolean): Integer;
    function AddColor(var List: TDynIntegerArray; Color: TColor;
      ColorType: TColorType; Context: Integer): Integer; overload;
    function AddColor(var List: TDynIntegerArray; Color: TColor; ColorType: TColorType;
      Context: Integer; out Added: Boolean): Integer; overload;
    procedure DeleteColor(var List: TDynIntegerArray; Index: Integer; ColorType: TColorType;
      Context: Integer);
    procedure RemoveContextList(Index: Integer); virtual;
    function IndexOfColor(Color: TColor): Integer;
    function IndexOfColIdx(const List: TDynIntegerArray; ColorIndex: Integer): Integer;
    procedure CopyFromDefCtx(const TargetContextIndex: Integer);
    function SelectedContextIndex: Integer;
    class function ItemsClass: TJvDataItemsClass; override;
    class function ContextsClass: TJvDataContextsClass; override;
    class function ContextsManagerClass: TJvDataContextsManagerClass; override;
    function ConsumerClasses: TClassArray; override;
    procedure ContextAdded(Context: IJvDataContext); override;
    procedure ContextDestroying(Context: IJvDataContext); override;
    procedure InsertMapping(var Strings: TDynStringArray; Index: Integer);
    procedure DeleteMapping(var Strings: TDynStringArray; Index: Integer); overload;
    procedure MappingAdding;
    procedure MappingAdded(Index: Integer);
    procedure MappingDestroying(Index: Integer);
    procedure MappingDestroyed;
    function GetColor(List, Index: Integer; out Value: TColor; out Name: string): Boolean;
    procedure SetColorName(List, Index: Integer; const NewName: string);
    function GetColorCount(List: Integer): Integer;
    procedure GenDelphiConstantMapping;
    procedure GenEnglishMapping;
    procedure InitColorList(var List: TDynIntegerArray; const Definitions: array of TDefColorItem;
      ColorType: TColorType);
    procedure InitColors;
    function GetMappings: TJvColorProviderNameMappings;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadColors(Reader: TReader);
    procedure WriteColors(Writer: TWriter);
    procedure ReadMappings(Reader: TReader);
    procedure WriteMappings(Writer: TWriter);
    procedure ReadMapping(Reader: TReader);
    procedure WriteMapping(Writer: TWriter; Index: Integer);
    function GetColorItem(ColorType: TColorType; Index: Integer): IJvDataItem;
    function GetColorItemByValue(ColorType: TColorType; Color: TColor): IJvDataItem;

    property ColorListChanged: Boolean read FColorListChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddColor(ColorType: TColorType; Color: TColor): Boolean; overload;
    function IndexOfMapping(Mapping: TJvColorProviderNameMapping): Integer;
    function IndexOfMappingName(Name: string): Integer;
    function Get_MappingCount: Integer;
    function Get_Mapping(Index: Integer): TJvColorProviderNameMapping;
    function AddMapping(AName: string): Integer;
    function NewMapping: Integer;
    procedure DeleteMapping(Index: Integer); overload;
    function GetStandardCount: Integer;
    function GetSystemCount: Integer;
    function GetCustomCount: Integer;
    function GetStandardColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function GetSystemColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function GetCustomColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
    function FindColor(Value: TColor; out ColorType: TColorType; out Index: Integer): Boolean;
    procedure SetStandardColorName(Index: Integer; NewName: string);
    procedure SetSystemColorName(Index: Integer; NewName: string);
    procedure SetCustomColorName(Index: Integer; NewName: string);
    function AddStdColor(Value: TColor): Boolean;
    procedure DeleteStdColor(Value: TColor);
    procedure DeleteStdColorAt(Index: Integer);
    procedure ClearStdColorList;
    function AddSysColor(Value: TColor): Boolean;
    procedure DeleteSysColor(Value: TColor);
    procedure DeleteSysColorAt(Index: Integer);
    procedure ClearSysColorList;
    function AddCstColor(Value: TColor): Boolean;
    procedure DeleteCstColor(Value: TColor);
    procedure DeleteCstColorAt(Index: Integer);
    procedure ClearCstColorList;

    property Mappings: TJvColorProviderNameMappings read GetMappings;
  published
    property OnAddColor: TJvColorProviderAddColorEvent read FOnAddColor write FOnAddColor;
  end;

  TJvColorProviderNameMappings = class(TObjectList)
  private
    FProvider: TJvColorProvider;
  protected
    function GetItem(Index: Integer): TJvColorProviderNameMapping;
    procedure SetItem(Index: Integer; AObject: TJvColorProviderNameMapping);

    property Provider: TJvColorProvider read FProvider;
  public
    constructor Create(AProvider: TJvColorProvider);
    function Add(Item: TJvColorProviderNameMapping): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TJvColorProviderNameMapping read GetItem write SetItem; default;
  end;

  TJvColorProviderNameMapping = class(TObject)
  private
    FName: string;
    FOwner: TJvColorProviderNameMappings;
  protected
    property Owner: TJvColorProviderNameMappings read FOwner;
  public
    constructor Create(AOwner: TJvColorProviderNameMappings; AName: string);

    property Name: string read FName write FName;
  end;

  TJvColorProviderSubSettings = class(TPersistent)
  private
    FActive: Boolean;
    FConsumerServiceExt: TJvDataConsumerAggregatedObject;
  protected
    procedure Changed; virtual;
    procedure ViewChanged; virtual;
    procedure SetActive(Value: Boolean); virtual;
    property Active: Boolean read FActive write SetActive;
    property ConsumerServiceExt: TJvDataConsumerAggregatedObject read FConsumerServiceExt;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  end;

  TJvColorProviderColorBoxSettings = class(TJvColorProviderSubSettings)
  private
    FHeight: Integer;
    FMargin: Integer;
    FShadowed: Boolean;
    FShadowSize: Integer;
    FSpacing: Integer;
    FWidth: Integer;
  protected
    procedure SetHeight(Value: Integer); virtual;
    procedure SetMargin(Value: Integer); virtual;
    procedure SetShadowed(Value: Boolean); virtual;
    procedure SetShadowSize(Value: Integer); virtual;
    procedure SetSpacing(Value: Integer); virtual;
    procedure SetWidth(Value: Integer); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Active default True;
    property Height: Integer read FHeight write SetHeight default 13;
    property Margin: Integer read FMargin write SetMargin default 2;
    property Shadowed: Boolean read FShadowed write SetShadowed default True;
    property ShadowSize: Integer read FShadowSize write SetShadowSize default 2;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width: Integer read FWidth write SetWidth default 21;
  end;

  TJvColorProviderTextSettings = class(TJvColorProviderSubSettings)
  private
    FShowHex: Boolean;
    FShowName: Boolean;
    FShowRGB: Boolean;
  protected
    procedure SetShowHex(Value: Boolean); virtual;
    procedure SetShowName(Value: Boolean); virtual;
    procedure SetShowRGB(Value: Boolean); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Active default True;
    property ShowHex: Boolean read FShowHex write SetShowHex;
    property ShowName: Boolean read FShowName write SetShowName default True;
    property ShowRGB: Boolean read FShowRGB write SetShowRGB;
  end;

  TJvColorProviderGroupingSettings = class(TJvColorProviderSubSettings)
  private
    FFlatList: Boolean;
    FHeaderAlign: TColorGroupHeaderAlign;
    FHeaderStyle: TColorGroupHeaderStyles;
  protected
    FActiveChanging: Boolean;
    procedure SetActive(Value: Boolean); override;
    procedure Changed; override;
    procedure SetFlatList(Value: Boolean); virtual;
    procedure SetHeaderAlign(Value: TColorGroupHeaderAlign); virtual;
    procedure SetHeaderStyle(Value: TColorGroupHeaderStyles); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Active default True;
    property FlatList: Boolean read FFlatList write SetFlatList default True;
    property HeaderAlign: TColorGroupHeaderAlign read FHeaderAlign write SetHeaderAlign
      default ghaLeft;
    property HeaderStyle: TColorGroupHeaderStyles read FHeaderStyle write SetHeaderStyle
      default [ghsBoldFont, ghsSingleCenterLine];
  end;

  TJvColorProviderColorGroupSettings = class(TJvColorProviderSubSettings)
  private
    FCaption: string;
    FShowHeader: Boolean;
  protected
    FActiveChanging: Boolean;
    procedure SetActive(Value: Boolean); override;
    procedure Changed; override;
    procedure SetCaption(Value: string); virtual;
    procedure SetShowHeader(Value: Boolean); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject; ACaption: string);
  published
    property Active default True;
    property Caption: string read FCaption write SetCaption;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default False;
  end;

  TJvColorProviderAddColorSettings = class(TJvColorProviderSubSettings)
  private
    FLocation: TJvColorProviderAddItemLocation;
    FCaption: string;
    FStyle: Integer;
  protected
    procedure SetLocation(Value: TJvColorProviderAddItemLocation); virtual;
    procedure SetCaption(Value: string); virtual;
    function GetStyle: TJvColorProviderAddColorStyle; virtual;
    procedure SetStyle(Value: TJvColorProviderAddColorStyle); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
    destructor Destroy; override;
  published
    property Active;
    property Location: TJvColorProviderAddItemLocation read FLocation write SetLocation
      default ailBottom;
    property Caption: string read FCaption write SetCaption;
    property Style: TJvColorProviderAddColorStyle read GetStyle write SetStyle;
  end;

  TJvColorProviderCustomColorGroupSettings = class(TJvColorProviderColorGroupSettings)
  private
    FAddColorSettings: TJvColorProviderAddColorSettings;
  protected
    procedure SetAddColorSettings(Value: TJvColorProviderAddColorSettings); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject; ACaption: string);
    destructor Destroy; override;
  published
    property AddColorSettings: TJvColorProviderAddColorSettings read FAddColorSettings
      write SetAddColorSettings;
  end;

  IJvColorProviderSettings = interface
    ['{5381D2E0-D8EA-46E7-A3C6-42B5353B896B}']
    function Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
    function Get_CustomColorSettings: TJvColorProviderCustomColorGroupSettings;
    function Get_GroupingSettings: TJvColorProviderGroupingSettings;
    function Get_NameMapping: TJvColorProviderNameMapping;
    function Get_NameMappingIndex: Integer;
    function Get_StandardColorSettings: TJvColorProviderColorGroupSettings;
    function Get_SystemColorSettings: TJvColorProviderColorGroupSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;
    procedure Set_NameMapping(Value: TJvColorProviderNameMapping);
    procedure Set_NameMappingIndex(Value: Integer);
    procedure MappingAdding;
    procedure MappingAdded(Index: Integer; Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroying(Index: Integer; Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroyed;

    property ColorBoxSettings: TJvColorProviderColorBoxSettings read Get_ColorBoxSettings;
    property CustomColorSettings: TJvColorProviderCustomColorGroupSettings
      read Get_CustomColorSettings;
    property GroupingSettings: TJvColorProviderGroupingSettings read Get_GroupingSettings;
    property NameMapping: TJvColorProviderNameMapping read Get_NameMapping write Set_NameMapping;
    property NameMappingIndex: Integer read Get_NameMappingIndex write Set_NameMappingIndex;
    property StandardColorSettings: TJvColorProviderColorGroupSettings
      read Get_StandardColorSettings;
    property SystemColorSettings: TJvColorProviderColorGroupSettings read Get_SystemColorSettings;
    property TextSettings: TJvColorProviderTextSettings read Get_TextSettings;
  end;

  { Provider containing the available name mappings of a color provider. }
  TJvColorMappingProvider = class(TJvCustomDataProvider, IJvColorMappingProvider)
    function IJvColorMappingProvider.Get_ClientProvider = GetColorProviderIntf;
    procedure IJvColorMappingProvider.Set_ClientProvider = SetColorProviderIntf;
  private
    function GetColorProviderIntf: IJvColorProvider;
    procedure SetColorProviderIntf(Value: IJvColorProvider); 
  protected
    class function ItemsClass: TJvDataItemsClass; override;
    function ConsumerClasses: TClassArray; override;
  public
    property ProviderIntf: IJvColorProvider read GetColorProviderIntf write SetColorProviderIntf;
  published 
    property Provider: IJvColorProvider read GetColorProviderIntf write SetColorProviderIntf; 
  end;

  TJvColorProviderServerNotify = class(TJvDataConsumerServerNotify)
  protected
    procedure ItemSelected(Value: IJvDataItem); override;
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean; override;
  end;

  TJvColorProviderColorAdderRegister = class(TObject)
  private
    FDefaultAdder: Integer;
    FList: TStringList;
    FMinimumKeep: Integer;
    FDefaultAfterClear: Integer;
    FNotifiers: TList;
  protected
    procedure RegisterNotify(Value: TJvColorProviderAddColorSettings);
    procedure UnregisterNotify(Value: TJvColorProviderAddColorSettings);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(Name: string; Callback: TJvColorProviderColorAdder): Integer;
    procedure Delete(Callback: TJvColorProviderColorAdder); overload;
    procedure Delete(Index: Integer); overload;
    procedure Delete(Name: string); overload;
    procedure Clear;
    function IndexOf(Name: string): Integer; overload;
    function IndexOf(Callback: TJvColorProviderColorAdder): Integer; overload;

    function Count: Integer;
    function Names(Index: Integer): string;
    function Callbacks(Index: Integer): TJvColorProviderColorAdder;

    property DefaultAdder: Integer read FDefaultAdder write FDefaultAdder;
  end;

const
  cColorItemIDPrefix = 'TCOLOR=';
  cColorProviderGroupHeaderID = 'ColorGroupHeader_';
  cColorProviderAddItemID = 'CP_ADDITEM';
  cColorProviderColorMapItemID = 'COLMAP:';
  ColorProvider_NotAColor = TColor($EFFFFFFF);

function ColorProviderColorAdderRegister: TJvColorProviderColorAdderRegister;

implementation

uses
  SysUtils, 
  RTLConsts, 
  QControls,
  JclStrings,
  JvQJVCLUtils, JvQConsts, JvQResources;

const
  aisPrvEvt = 'aisPrvEvt';
  aisStdDlg = 'aisStdDlg';

type
  TWriterAccessProtected = class(TWriter);

function GetItemColorValue(Item: IJvDataItem; out Color: TColor): Boolean;
var
  S: string;
begin
  S := Item.GetID;
  Result := Copy(S, 1, 7) = cColorItemIDPrefix;
  if Result then
    Color := StrToInt('$0' + Copy(S, 8, 8));
end;

function GetUniqueMappingName(Mappings: TJvColorProviderNameMappings; Prefix: string): string;
var
  PrefixLen: Integer;
  SuffixNum: Int64;
  MapIdx: Integer;
  TmpNum: Int64;
begin
  PrefixLen := Length(Prefix);
  SuffixNum := 1;
  for MapIdx := 0 to Mappings.Count - 1 do
    if AnsiSameStr(Prefix, Copy(Mappings[MapIdx].Name, 1, PrefixLen)) then
      with Mappings[MapIdx] do
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

//==================================================================================================
// Color provider color adding methods
//==================================================================================================

procedure AddColorProviderEvent(Provider: IJvColorProvider; ColorType: TColorType;
  var Color: TColor; var DoAdd: Boolean);
begin
  Provider.DoAddColor(ColorType, Color, DoAdd);
end;

procedure AddColorColorDialog(Provider: IJvColorProvider; ColorType: TColorType; var AColor: TColor;
  var DoAdd: Boolean);
begin
  with TColorDialog.Create(nil) do
  try
    DoAdd := Execute;
    AColor := Color;
  finally
    Free;
  end;
end;

//==================================================================================================
// Color provider color adding methods registration
//==================================================================================================

var
  AdderReg: TJvColorProviderColorAdderRegister;

function ColorProviderColorAdderRegister: TJvColorProviderColorAdderRegister;
begin
  if AdderReg = nil then
  begin
    AdderReg := TJvColorProviderColorAdderRegister.Create;
    AdderReg.Add(aisPrvEvt, AddColorProviderEvent);
    AdderReg.DefaultAdder := AdderReg.Add(aisStdDlg, AddColorColorDialog);
    AdderReg.FMinimumKeep := AdderReg.Count;
    AdderReg.FDefaultAfterClear := AdderReg.DefaultAdder;
  end;
  Result := AdderReg;
end;

var
  MasterColorConsumer: IJvDataConsumer;

type
  TJvColorItems = class(TJvBaseDataItems)
  private
    FColorProvider: IJvColorProvider;
  protected
    function GetColorSettings: IJvColorProviderSettings;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
    procedure InitImplementers; override;

    property ColorProvider: IJvColorProvider read FColorProvider write FColorProvider;
  public
    procedure AfterConstruction; override;
  end;

  TJvColorItemsList = class(TJvColorItems)
  private
    FListNum: Integer;
  protected
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;

    property ListNum: Integer read FListNum write FListNum;
  public
    procedure AfterConstruction; override;
  end;

  TJvColorItem = class(TJvBaseDataItem, IJvDataItemText, IJvColorItem)
  private
    FListNumber: Integer; // 0 = StdColors, 1 = SysColors, 2 = CstColors
    FListIndex: Integer;  // Index in color list
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
    function Get_Color: TColor;
    procedure InitID; override;
    property ListNumber: Integer read FListNumber;
    property ListIndex: Integer read FListIndex;
  public
    constructor Create(AOwner: IJvDataItems; AListNumber, AListIndex: Integer);
  end;

  TJvColorHeaderItem = class(TJvBaseDataItem, IJvDataItemText)
  private
    FListNumber: Integer; // 0 = StdColors, 1 = SysColors, 2 = CstColors
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
    procedure InitID; override;
    procedure InitImplementers; override;
    function IsDeletable: Boolean; override;
    property ListNumber: Integer read FListNumber;
  public
    constructor Create(AOwner: IJvDataItems; AListNumber: Integer);
  end;

  TJvColorAddItem = class(TJvBaseDataItem, IJvDataItemText)
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
    procedure InitID; override;
    procedure InitImplementers; override;
  end;

  TJvColorItemAddExecute = class(TJvDataItemAggregatedObject, IJvDataItemBasicAction)
  protected
    function Execute(Sender: TObject): Boolean;
  end;

  TJvColorItemsRenderer = class(TJvCustomDataItemsRenderer)
  protected
    CurrentCanvas: TCanvas;
    CurrentRect: TRect;
    CurrentItem: IJvDataItem;
    CurrentState: TProviderDrawStates;
    CurrentSettings: IJvColorProviderSettings;
    CurrentItemIsColorItem: Boolean;
    CurrentColorValue: TColor;
    function GetRenderText: string;
    procedure RenderColorBox;
    procedure RenderColorText;
    procedure RenderGroupHeader;
    procedure MeasureColorBox(var Size: TSize);
    procedure MeasureColorText(var Size: TSize);
    procedure DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
      State: TProviderDrawStates); override;
    function DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize; override;
    function AvgItemSize(ACanvas: TCanvas): TSize; override;
    function GetConsumerSettings: IJvColorProviderSettings;
  end;

  TJvDataConsumerAggregatedObjectAccessProtected = class(TJvDataConsumerAggregatedObject);
  TJvColorProviderSettings = class;

  TJvColorMappingChangeEvent = procedure(Sender: TJvColorProviderSettings; Index: Integer;
    Mapping: TJvColorProviderNameMapping) of object;

  TJvColorProviderSettings = class(TJvDataConsumerAggregatedObject, IJvColorProviderSettings)
  private
    FColorBoxSettings: TJvColorProviderColorBoxSettings;
    FCustomColorSettings: TJvColorProviderCustomColorGroupSettings;
    FGroupingSettings: TJvColorProviderGroupingSettings;
    FStandardColorSettings: TJvColorProviderColorGroupSettings;
    FSystemColorSettings: TJvColorProviderColorGroupSettings;
    FTextSettings: TJvColorProviderTextSettings;
    FMapping: Integer;
    FOnMappingAdding: TNotifyEvent;
    FOnMappingAdded: TJvColorMappingChangeEvent;
    FOnMappingDestroying: TJvColorMappingChangeEvent;
    FOnMappingDestroyed: TNotifyEvent;
  protected
    function Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
    function Get_CustomColorSettings: TJvColorProviderCustomColorGroupSettings;
    function Get_GroupingSettings: TJvColorProviderGroupingSettings;
    function Get_NameMapping: TJvColorProviderNameMapping;
    function Get_NameMappingIndex: Integer;
    function Get_StandardColorSettings: TJvColorProviderColorGroupSettings;
    function Get_SystemColorSettings: TJvColorProviderColorGroupSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;
    procedure Set_ColorBoxSettings(Value: TJvColorProviderColorBoxSettings);
    procedure Set_CustomColorSettings(Value: TJvColorProviderCustomColorGroupSettings);
    procedure Set_GroupingSettings(Value: TJvColorProviderGroupingSettings);
    procedure Set_NameMapping(Value: TJvColorProviderNameMapping);
    procedure Set_NameMappingIndex(Value: Integer);
    procedure Set_StandardColorSettings(Value: TJvColorProviderColorGroupSettings);
    procedure Set_SystemColorSettings(Value: TJvColorProviderColorGroupSettings);
    procedure Set_TextSettings(Value: TJvColorProviderTextSettings);
    procedure MappingAdding;
    procedure MappingAdded(Index: Integer; Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroying(Index: Integer; Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroyed;
    function GetNameMappingIndex: TJvColorProviderMapping;
    procedure SetNameMappingIndex(Value: TJvColorProviderMapping);
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
    destructor Destroy; override;
    property OnMappingAdding: TNotifyEvent read FOnMappingAdding write FOnMappingAdding;
    property OnMappingAdded: TJvColorMappingChangeEvent read FOnMappingAdded write FOnMappingAdded;
    property OnMappingDestroying: TJvColorMappingChangeEvent read FOnMappingDestroying
      write FOnMappingDestroying;
    property OnMappingDestroyed: TNotifyEvent read FOnMappingDestroyed write FOnMappingDestroyed;
  published
    property ColorBoxSettings: TJvColorProviderColorBoxSettings read Get_ColorBoxSettings
      write Set_ColorBoxSettings;
    property CustomColorSettings: TJvColorProviderCustomColorGroupSettings
      read Get_CustomColorSettings write Set_CustomColorSettings;
    property TextSettings: TJvColorProviderTextSettings read Get_TextSettings
      write Set_TextSettings;
    property StandardColorSettings: TJvColorProviderColorGroupSettings
      read Get_StandardColorSettings write Set_StandardColorSettings;
    property SystemColorSettings: TJvColorProviderColorGroupSettings read Get_SystemColorSettings
      write Set_SystemColorSettings;
    property GroupingSettings: TJvColorProviderGroupingSettings read Get_GroupingSettings
      write Set_GroupingSettings;
    property Mapping: TJvColorProviderMapping read GetNameMappingIndex write SetNameMappingIndex
      default -1;
  end;

  TJvColorContextsManager = class(TJvBaseDataContextsManager)
  protected
    function New: IJvDataContext; override;
  end;

  TJvColorContext = class(TJvDataContext, IJvDataContextManager)
  protected
    function IsDeletable: Boolean; override;
    function IsStreamable: Boolean; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadStdColors(Reader: TReader);
    procedure WriteStdColors(Writer: TWriter);
    procedure ReadSysColors(Reader: TReader);
    procedure WriteSysColors(Writer: TWriter);
    procedure ReadCstColors(Reader: TReader);
    procedure WriteCstColors(Writer: TWriter);
    procedure ReadCtxList(Reader: TReader; var List: TDynIntegerArray);
    procedure WriteCtxList(Writer: TWriter; const List: TDynIntegerArray);
  end;

  TJvColorMapItems = class(TJvBaseDataItems)
  private
    FConsumer: TJvDataConsumer;
    FItemInstances: TList;
    function GetClientProvider: IJvDataProvider;
    procedure SetClientProvider(Value: IJvDataProvider);
    procedure DataProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure SubServiceCreated(Sender: TJvDataConsumer; SubSvc: TJvDataConsumerAggregatedObject);
    procedure ConsumerChanged(Sender: TJvDataConsumer; Reason: TJvDataConsumerChangeReason);
    procedure MappingAdding(Sender: TObject);
    procedure MappingAdded(Sender: TJvColorProviderSettings; Index: Integer;
      Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroying(Sender: TJvColorProviderSettings; Index: Integer;
      Mapping: TJvColorProviderNameMapping);
    procedure MappingDestroyed(Sender: TObject);
  protected
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
    procedure InitImplementers; override;

    property Consumer: TJvDataConsumer read FConsumer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    property ClientProvider: IJvDataProvider read GetClientProvider write SetClientProvider;
  end;

  TJvColorMapItem = class(TJvBaseDataItem, IJvDataItemText, IJvColorMapItem)
  private
    FIndex: Integer;
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
    function Get_NameMapping: TJvColorProviderNameMapping;
    procedure InitID; override;
    property Index: Integer read FIndex;
  public
    constructor Create(AOwner: IJvDataItems; AIndex: Integer);
    destructor Destroy; override;
  end;

  TJvColorMapItemsManager = class(TJvBaseDataItemsManagement)
  protected
    { IJvDataItemManagement methods }
    function Add(Item: IJvDataItem): IJvDataItem; override;
    function New: IJvDataItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(var Item: IJvDataItem); override;
  end;

  TJvColorConsumer = class(TInterfacedObject, IJvDataConsumer)
  protected
    { IJvDataConsumer methods }
    function VCLComponent: TComponent;
    function AttributeApplies(Attr: Integer): Boolean;
  end;

//=== { TJvColorProviderNameMapping } ========================================

constructor TJvColorProviderNameMapping.Create(AOwner: TJvColorProviderNameMappings; AName: string);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
end;

//=== { TJvColorProviderSubSettings } ========================================

constructor TJvColorProviderSubSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create;
  FConsumerServiceExt := AConsumerService;
end;

procedure TJvColorProviderSubSettings.Changed;
begin
  TJvDataConsumerAggregatedObjectAccessProtected(ConsumerServiceExt).Changed(ccrOther);
end;

procedure TJvColorProviderSubSettings.ViewChanged;
begin
  TJvDataConsumerAggregatedObjectAccessProtected(ConsumerServiceExt).NotifyViewChanged;
end;

procedure TJvColorProviderSubSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    Changed;
  end;
end;

//=== { TJvColorProviderColorBoxSettings } ===================================

constructor TJvColorProviderColorBoxSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FHeight := 13;
  FMargin := 2;
  FShadowed := True;
  FShadowSize := 2;
  FSpacing := 4;
  FWidth := 21;
end;

procedure TJvColorProviderColorBoxSettings.SetHeight(Value: Integer);
begin
  if Value <> Height then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetMargin(Value: Integer);
begin
  if Value <> Margin then
  begin
    FMargin := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetShadowed(Value: Boolean);
begin
  if Value <> Shadowed then
  begin
    FShadowed := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetShadowSize(Value: Integer);
begin
  if Value <> ShadowSize then
  begin
    FShadowSize := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetSpacing(Value: Integer);
begin
  if Value <> Spacing then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorBoxSettings.SetWidth(Value: Integer);
begin
  if Value <> Width then
  begin
    FWidth := Value;
    Changed;
  end;
end;

//=== { TJvColorProviderTextSettings } =======================================

constructor TJvColorProviderTextSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FShowName := True;
end;

procedure TJvColorProviderTextSettings.SetShowHex(Value: Boolean);
begin
  if Value <> ShowHex then
  begin
    FShowHex := Value;
    Changed;
  end;
end;

procedure TJvColorProviderTextSettings.SetShowName(Value: Boolean);
begin
  if Value <> ShowName then
  begin
    FShowName := Value;
    Changed;
  end;
end;

procedure TJvColorProviderTextSettings.SetShowRGB(Value: Boolean);
begin
  if Value <> ShowRGB then
  begin
    FShowRGB := Value;
    Changed;
  end;
end;

//=== { TJvColorProviderGroupingSettings } ===================================

constructor TJvColorProviderGroupingSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FFlatList := True;
  FHeaderAlign := ghaLeft;
  FHeaderStyle := [ghsBoldFont, ghsSingleCenterLine];
end;

procedure TJvColorProviderGroupingSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActiveChanging := True;
    inherited SetActive(Value);
    FActiveChanging := False;
  end;
end;

procedure TJvColorProviderGroupingSettings.Changed;
begin
  if FActiveChanging then
    ViewChanged;
  inherited Changed;
end;

procedure TJvColorProviderGroupingSettings.SetFlatList(Value: Boolean);
begin
  if Value <> FlatList then
  begin
    FFlatList := Value;
    ViewChanged;
    Changed;
  end;
end;

procedure TJvColorProviderGroupingSettings.SetHeaderAlign(Value: TColorGroupHeaderAlign);
begin
  if Value <> HeaderAlign then
  begin
    FHeaderAlign := Value;
    Changed;
  end;
end;

procedure TJvColorProviderGroupingSettings.SetHeaderStyle(Value: TColorGroupHeaderStyles);
begin
  if (ghsSingleCenterLine in Value) and not (ghsSingleCenterLine in HeaderStyle) then
    Exclude(Value, ghsDoubleCenterLine)
  else
  if (ghsDoubleCenterLine in Value) and not (ghsDoubleCenterLine in HeaderStyle) then
    Exclude(Value, ghsSingleCenterLine);
  if Value <> HeaderStyle then
  begin
    FHeaderStyle := Value;
    Changed;
  end;
end;

//=== { TJvColorProviderColorGroupSettings } =================================

constructor TJvColorProviderColorGroupSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject;
  ACaption: string);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FCaption := ACaption;
end;

procedure TJvColorProviderColorGroupSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActiveChanging := True;
    inherited SetActive(Value);
    FActiveChanging := False;
  end;
end;

procedure TJvColorProviderColorGroupSettings.Changed;
begin
  if FActiveChanging then
    ViewChanged;
  inherited Changed;
end;

procedure TJvColorProviderColorGroupSettings.SetCaption(Value: string);
begin
  if Value <> Caption then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvColorProviderColorGroupSettings.SetShowHeader(Value: Boolean);
begin
  if Value <> ShowHeader then
  begin
    FShowHeader := Value;
    Changed;
    ViewChanged;
  end;
end;

//=== { TJvColorProviderAddColorSettings } ===================================

constructor TJvColorProviderAddColorSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FLocation := ailBottom;
  FStyle := ColorProviderColorAdderRegister.DefaultAdder;
  ColorProviderColorAdderRegister.RegisterNotify(Self);
end;

destructor TJvColorProviderAddColorSettings.Destroy;
begin
  ColorProviderColorAdderRegister.UnregisterNotify(Self);
  inherited Destroy;
end;

procedure TJvColorProviderAddColorSettings.SetLocation(Value: TJvColorProviderAddItemLocation);
begin
  if Value <> Location then
  begin
    FLocation := Value;
    Changed;
    ViewChanged;
  end;
end;

procedure TJvColorProviderAddColorSettings.SetCaption(Value: string);
begin
  if Value <> Caption then
  begin
    FCaption := Value;
    Changed;
  end;
end;

function TJvColorProviderAddColorSettings.GetStyle: TJvColorProviderAddColorStyle;
begin
  Result := FStyle;
end;

procedure TJvColorProviderAddColorSettings.SetStyle(Value: TJvColorProviderAddColorStyle);
begin
  if Value <> Style then
  begin
    FStyle := Value;
    Changed;
  end;
end;

//=== { TJvColorProviderCustomColorGroupSettings } ===========================

constructor TJvColorProviderCustomColorGroupSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject;
  ACaption: string);
begin
  inherited Create(AConsumerService, ACaption);
  FAddColorSettings := TJvColorProviderAddColorSettings.Create(AConsumerService);
end;

destructor TJvColorProviderCustomColorGroupSettings.Destroy;
begin
  FreeAndNil(FAddColorSettings);
  inherited Destroy;
end;

procedure TJvColorProviderCustomColorGroupSettings.SetAddColorSettings(
  Value: TJvColorProviderAddColorSettings);
begin
end;

//=== { TJvColorMappingProvider } ============================================

function TJvColorMappingProvider.GetColorProviderIntf: IJvColorProvider;
begin
  Result := TJvColorMapItems(DataItemsImpl).ClientProvider as IJvColorProvider;
end;

procedure TJvColorMappingProvider.SetColorProviderIntf(Value: IJvColorProvider);
begin
  TJvColorMapItems(DataItemsImpl).ClientProvider := (Value as IJvDataProvider);
end;



class function TJvColorMappingProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvColorMapItems;
end;

function TJvColorMappingProvider.ConsumerClasses: TClassArray;
begin
  Result := inherited ConsumerClasses;
  AddToArray(Result, TJvColorProviderServerNotify);
end;

//=== { TJvColorProviderServerNotify } =======================================

procedure TJvColorProviderServerNotify.ItemSelected(Value: IJvDataItem);
var
  MapItem: IJvColorMapItem;
  Mapping: TJvColorProviderNameMapping;
  I: Integer;
  ConSet: IJvColorProviderSettings;
begin
  inherited ItemSelected(Value);
  if Supports(Value, IJvColorMapItem, MapItem) then
  begin
    Mapping := MapItem.NameMapping;
    for I := 0 to Clients.Count - 1 do
      if Supports(Clients[I], IJvColorProviderSettings, ConSet) then
        ConSet.NameMapping := Mapping;
  end;
end;

function TJvColorProviderServerNotify.IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
var
  ClientProv: IJvDataProvider;
  ConsumerProv: IJvDataConsumerProvider;
begin
  { Only allow client consumers who's Provider points to the ColorProvider of the mapping
    provider this consumer is linked to. }
  ClientProv := (ConsumerImpl.ProviderIntf as IJvColorMappingProvider).ClientProvider as
    IJvDataProvider;
  Result := Supports(Client, IJvDataConsumerProvider, ConsumerProv) and
    (ConsumerProv.GetProvider = ClientProv);
end;

//=== { TJvColorProviderColorAdderRegister } =================================

constructor TJvColorProviderColorAdderRegister.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FNotifiers := TList.Create;
end;

destructor TJvColorProviderColorAdderRegister.Destroy;
begin
  FreeAndNil(FNotifiers);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TJvColorProviderColorAdderRegister.RegisterNotify(Value: TJvColorProviderAddColorSettings);
begin
  if FNotifiers.IndexOf(Value) = -1 then
    FNotifiers.Add(Value);
end;

procedure TJvColorProviderColorAdderRegister.UnregisterNotify(Value: TJvColorProviderAddColorSettings);
begin
  FNotifiers.Remove(Value);
end;

function TJvColorProviderColorAdderRegister.Add(Name: string;
  Callback: TJvColorProviderColorAdder): Integer;
begin
  Result := IndexOf(Name);
  if Result = -1 then
    Result := FList.AddObject(Name, TObject(@Callback))
  else
  if @Callbacks(Result) <> @Callback then
    raise EJVCLException.CreateResFmt(@RsEAlreadyRegistered, [Name]);
end;

procedure TJvColorProviderColorAdderRegister.Delete(Callback: TJvColorProviderColorAdder);
var
  Idx: Integer;
begin
  Idx := IndexOf(Callback);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvColorProviderColorAdderRegister.Delete(Index: Integer);
var
  I: Integer;
begin
  FList.Delete(Index);
  if Index < DefaultAdder then
    Dec(FDefaultAdder)
  else
  if Index = DefaultAdder then
  begin
    if Count > 0 then
      FDefaultAdder := 0
    else
      FDefaultAdder := -1;
  end;
  for I := 0 to FNotifiers.Count -1 do
    with TJvColorProviderAddColorSettings(FNotifiers[I]) do
      if Style > Index then
        Dec(FStyle)
      else
      if Style = Index then
        FStyle := DefaultAdder;
end;

procedure TJvColorProviderColorAdderRegister.Delete(Name: string);
var
  Idx: Integer;
begin
  Idx := IndexOf(Name);
  if Idx > -1 then
    Delete(Idx);
end;

procedure TJvColorProviderColorAdderRegister.Clear;
var
  I: Integer;
begin
  while FList.Count > FMinimumKeep do
    FList.Delete(FList.Count);
  DefaultAdder := FDefaultAfterClear;
  for I := 0 to FNotifiers.Count - 1 do
    with TJvColorProviderAddColorSettings(FNotifiers[I]) do
      if Style >= FMinimumKeep then
        FStyle := DefaultAdder;
end;

function TJvColorProviderColorAdderRegister.IndexOf(Name: string): Integer;
begin
  Result := FList.IndexOf(Name);
end;

function TJvColorProviderColorAdderRegister.IndexOf(Callback: TJvColorProviderColorAdder): Integer;
begin
  Result := FList.IndexOfObject(TObject(@Callback));
end;

function TJvColorProviderColorAdderRegister.Count: Integer;
begin
  Result := FList.Count;
end;

function TJvColorProviderColorAdderRegister.Names(Index: Integer): string;
begin
  Result := FList[Index];
end;

function TJvColorProviderColorAdderRegister.Callbacks(Index: Integer): TJvColorProviderColorAdder;
begin
  Result := TJvColorProviderColorAdder(FList.Objects[Index]);
end;

//=== { TJvColorItems } ======================================================

function TJvColorItems.GetColorSettings: IJvColorProviderSettings;
begin
  if GetProvider = nil then
    Result := nil
  else
    Supports(GetProvider.SelectedConsumer, IJvColorProviderSettings, Result);
end;

function TJvColorItems.GetCount: Integer;
var
  Settings: IJvColorProviderSettings;
begin
  Settings := GetColorSettings;
  Result := 0;
  if Settings = nil then
    Exit;
  if Settings.GroupingSettings.Active and not Settings.GroupingSettings.FlatList then
  begin
    Inc(Result, Ord(Settings.StandardColorSettings.Active) +
      Ord(Settings.SystemColorSettings.Active) + Ord(Settings.CustomColorSettings.Active));
  end
  else
  begin
    if Settings.StandardColorSettings.Active then
      Inc(Result, ColorProvider.GetStandardCount +
        Ord(Settings.StandardColorSettings.ShowHeader and Settings.GroupingSettings.Active));
    if Settings.SystemColorSettings.Active then
      Inc(Result, ColorProvider.GetSystemCount +
        Ord(Settings.SystemColorSettings.ShowHeader and Settings.GroupingSettings.Active));
    if Settings.CustomColorSettings.Active then
      Inc(Result, ColorProvider.GetCustomCount +
        Ord(Settings.CustomColorSettings.ShowHeader and Settings.GroupingSettings.Active) +
        Ord(Settings.CustomColorSettings.AddColorSettings.Active and
          (Settings.CustomColorSettings.ShowHeader and Settings.GroupingSettings.Active or
          (Settings.CustomColorSettings.AddColorSettings.Location <> ailUseHeader))));
  end;
end;

function TJvColorItems.GetItem(I: Integer): IJvDataItem;
var
  OrgIdx: Integer;
  Settings: IJvColorProviderSettings;
  ListNum: Integer;
begin
  if I < 0 then
    TList.Error(SListIndexError, I);
  OrgIdx := I;
  Settings := GetColorSettings;
  if Settings = nil then
    Exit;
  ListNum := -1;
  if Settings.GroupingSettings.Active and not Settings.GroupingSettings.FlatList then
  begin
    if Settings.StandardColorSettings.Active then
      Dec(I);
    if I < 0 then
      ListNum := 0;
    if (ListNum < 0) and Settings.SystemColorSettings.Active then
    begin
      Dec(I);
      if I < 0 then
        ListNum := 1;
    end;
    if (ListNum < 0) and Settings.CustomColorSettings.Active then
    begin
      Dec(I);
      if I < 0 then
        ListNum := 2;
    end;
  end
  else
  begin
    if Settings.StandardColorSettings.Active then
    begin
      if Settings.StandardColorSettings.ShowHeader and Settings.GroupingSettings.Active then
        Dec(I);
      if I < ColorProvider.GetStandardCount then
        ListNum := 0
      else
        Dec(I, ColorProvider.GetStandardCount);
    end;
    if (ListNum < 0) and Settings.SystemColorSettings.Active then
    begin
      if Settings.SystemColorSettings.ShowHeader and Settings.GroupingSettings.Active then
        Dec(I);
      if I < ColorProvider.GetSystemCount then
        ListNum := 1
      else
        Dec(I, ColorProvider.GetSystemCount);
    end;
    if (ListNum < 0) and Settings.CustomColorSettings.Active then
    begin
      if Settings.CustomColorSettings.ShowHeader and Settings.GroupingSettings.Active then
        Dec(I);
      with Settings.CustomColorSettings.AddColorSettings do
        if Active and (Location = ailTop) and (I = 0) then
          I := -2;
      if I < ColorProvider.GetCustomCount then
        ListNum := 2
      else
      begin
        Dec(I, ColorProvider.GetCustomCount);
        with Settings.CustomColorSettings.AddColorSettings do
          if (I = 0) and Active and ((Location = ailBottom) or ((Location = ailUseHeader) and
            not (Settings.CustomColorSettings.ShowHeader and Settings.GroupingSettings.Active))) then
          begin
            ListNum := 2;
            I := -2;
          end;
      end;
    end;
  end;
  if ListNum < 0 then
    TList.Error(SListIndexError, OrgIdx);
  if I = -1 then
    Result := TJvColorHeaderItem.Create(Self, ListNum)
  else
  if I = -2 then
    Result := TJvColorAddItem.Create(Self)
  else
  if I >= 0 then
    Result := TJvColorItem.Create(Self, ListNum, I);
end;

procedure TJvColorItems.InitImplementers;
begin
  inherited InitImplementers;
  if GetParent = nil then
    TJvColorItemsRenderer.Create(Self);
end;

procedure TJvColorItems.AfterConstruction;
begin
  inherited AfterConstruction;
  Supports(GetProvider, IJvColorProvider, FColorProvider);
end;

//=== { TJvColorItemsList } ==================================================

function TJvColorItemsList.GetCount: Integer;
begin
  case ListNum of
    0:
      Result := ColorProvider.GetStandardCount;
    1:
      Result := ColorProvider.GetSystemCount;
    2:
      Result := ColorProvider.GetCustomCount;
    else
      Result := 0;
  end;
end;

function TJvColorItemsList.GetItem(I: Integer): IJvDataItem;
begin
  if I < 0 then
    TList.Error(SListIndexError, I);
  case ListNum of
    0:
      begin
        if I >= ColorProvider.GetStandardCount then
          TList.Error(SListIndexError, I)
        else
          Result := TJvColorItem.Create(Self, 0, I);
      end;
    1:
      begin
        if I >= ColorProvider.GetSystemCount then
          TList.Error(SListIndexError, I)
        else
          Result := TJvColorItem.Create(Self, 1, I);
      end;
    2:
      begin
        if I >= ColorProvider.GetCustomCount then
          TList.Error(SListIndexError, I)
        else
          Result := TJvColorItem.Create(Self, 2, I);
      end;
    else
      TList.Error(SListIndexError, I);
  end;
end;

procedure TJvColorItemsList.AfterConstruction;
begin
  inherited AfterConstruction;
  ListNum := TJvColorHeaderItem(GetParent.GetImplementer).ListNumber;
end;

//=== { TJvColorItem } =======================================================

constructor TJvColorItem.Create(AOwner: IJvDataItems; AListNumber, AListIndex: Integer);
begin
  inherited Create(AOwner);
  FListNumber := AListNumber;
  FListIndex := AListIndex;
end;

function TJvColorItem.GetCaption: string;
var
  ColorFound: Boolean;
  ColorValue: TColor;
begin
  case ListNumber of
    0:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetStandardColor(ListIndex, ColorValue, Result);
    1:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetSystemColor(ListIndex, ColorValue, Result);
    2:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetCustomColor(ListIndex, ColorValue, Result);
    else
    begin
      ColorFound := False;
      Result := '';
    end;
  end;
  if (Result = '') and ColorFound then
    ColorToIdent(ColorValue, Result);
end;

procedure TJvColorItem.SetCaption(const Value: string);
begin
  case ListNumber of
    0:
      (GetItems.GetProvider as IJvColorProvider).SetStandardColorName(ListIndex, Value);
    1:
      (GetItems.GetProvider as IJvColorProvider).SetSystemColorName(ListIndex, Value);
    2:
      (GetItems.GetProvider as IJvColorProvider).SetCustomColorName(ListIndex, Value);
  end;
end;

function TJvColorItem.Editable: Boolean;
begin
  Result := True;
end;

function TJvColorItem.Get_Color: TColor;
begin
  if not GetItemColorValue(Self, Result) then
    Result := ColorProvider_NotAColor;
end;

procedure TJvColorItem.InitID;
var
  ColorFound: Boolean;
  ColorValue: TColor;
  ColorName: string;
begin
  case ListNumber of
    0:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetStandardColor(ListIndex, ColorValue, ColorName);
    1:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetSystemColor(ListIndex, ColorValue, ColorName);
    2:
      ColorFound := (GetItems.GetProvider as IJvColorProvider).GetCustomColor(ListIndex, ColorValue, ColorName);
    else
    begin
      ColorFound := False;
      ColorValue := -1;
    end;
  end;
  if ColorFound then
    SetID(cColorItemIDPrefix + IntToHex(ColorValue, 8))
  else
    SetID('Item' + IntToStr(ListNumber) + '.' + IntToStr(ListIndex));
end;

//=== { TJvColorHeaderItem } =================================================

constructor TJvColorHeaderItem.Create(AOwner: IJvDataItems; AListNumber: Integer);
begin
  inherited Create(AOwner);
  FListNumber := AListNumber;
end;

function TJvColorHeaderItem.GetCaption: string;
var
  Settings: IJvColorProviderSettings;
begin
  Supports(GetItems.GetProvider.SelectedConsumer, IJvColorProviderSettings, Settings);
  if Settings = nil then
    Result := RsNoSettings
  else
    case ListNumber of
      0:
        Result := Settings.StandardColorSettings.Caption;
      1:
        Result := Settings.SystemColorSettings.Caption;
      2:
        Result := Settings.CustomColorSettings.Caption;
    end;
end;

function TJvColorHeaderItem.Editable: Boolean;
begin
  Result := False;
end;

procedure TJvColorHeaderItem.SetCaption(const Value: string);
begin
end;

procedure TJvColorHeaderItem.InitID;
begin
  SetID(cColorProviderGroupHeaderID + IntToStr(ListNumber));
end;

procedure TJvColorHeaderItem.InitImplementers;
var
  Settings: IJvColorProviderSettings;
begin
  inherited InitImplementers;
  // (rom) suppress warnings about abstract methods
  // (rom) TJvBaseDataItems.InternalAdd, InternalDelete and InternalMove
  // (rom) can this be fixed by empty methods?
  {$WARNINGS OFF}
  if GetItems.GetProvider.SelectedConsumer = MasterColorConsumer then
    TJvColorItemsList.Create(Self)
  else
  if Supports(GetItems.GetProvider.SelectedConsumer, IJvColorProviderSettings, Settings) then
  begin
    if not Settings.GroupingSettings.FlatList then
      TJvColorItemsList.Create(Self);
    if (FListNumber = 2) and Settings.CustomColorSettings.AddColorSettings.Active and
        (Settings.CustomColorSettings.AddColorSettings.Location = ailUseHeader) then
      TJvColorItemAddExecute.Create(Self);
  end;
  {$WARNINGS ON}
end;

function TJvColorHeaderItem.IsDeletable: Boolean;
begin
  Result := False;
end;

//=== { TJvColorAddItem } ====================================================

function TJvColorAddItem.GetCaption: string;
var
  Settings: IJvColorProviderSettings;
begin
  Supports(GetItems.GetProvider.SelectedConsumer, IJvColorProviderSettings, Settings);
  if Settings = nil then
    Result := RsNoSettings
  else
    Result := Settings.CustomColorSettings.AddColorSettings.Caption;
end;

procedure TJvColorAddItem.SetCaption(const Value: string);
begin
end;

function TJvColorAddItem.Editable: Boolean;
begin
  Result := False;
end;

procedure TJvColorAddItem.InitID;
begin
  SetID(cColorProviderAddItemID);
end;

procedure TJvColorAddItem.InitImplementers;
begin
  inherited InitImplementers;
  TJvColorItemAddExecute.Create(Self);
end;

//=== { TJvColorItemAddExecute } =============================================

function TJvColorItemAddExecute.Execute(Sender: TObject): Boolean;
var
  ColorSettings: IJvColorProviderSettings;
  StyleIdx: Integer;
  Color: TColor;
  DoAdd: Boolean;
  Callback: TJvColorProviderColorAdder;
begin
  Result := Supports(Item.Items.Provider.SelectedConsumer, IJvColorProviderSettings, ColorSettings);
  if Result then
  begin
    StyleIdx := ColorSettings.CustomColorSettings.AddColorSettings.Style;
    Color := ColorProvider_NotAColor;
    DoAdd := False;
    Callback := ColorProviderColorAdderRegister.Callbacks(StyleIdx);
    if Assigned(Callback) then
      Callback(Item.Items.Provider as IJvColorProvider, ctCustom, Color, DoAdd);
    if DoAdd then
      (Item.Items.Provider as IJvColorProvider).AddColor(ctCustom, Color);
  end;
end;

//=== { TJvColorProvider } ===================================================

constructor TJvColorProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMappings := TJvColorProviderNameMappings.Create(Self);
  GenDelphiConstantMapping;
  GenEnglishMapping;
  (DataContextsImpl as IJvDataContextsManager).Add(TJvColorContext.Create(DataContextsImpl, 'Default'));
  InitColors;
  FColorListChanged := False;
end;

destructor TJvColorProvider.Destroy;
begin
  FreeAndNil(FMappings);
  inherited Destroy;
end;

procedure TJvColorProvider.NotifyChanging(Reason: TDataProviderChangeReason; ContextIndex: Integer;
  GroupedItem, RootItem: IUnknown);
var
  I: Integer;
  Consumer: IJvDataConsumer;
  ConCtx: IJvDataConsumerContext;
  Settings: IJvColorProviderSettings;
  CtxIdx: Integer;
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
  try
    for I := 0 to GetNotifierCount - 1 do
    begin
      if Supports(GetNotifier(I), IJvDataConsumer, Consumer) and
        Supports(Consumer, IJvDataConsumerContext, ConCtx) and
        Supports(Consumer, IJvColorProviderSettings, Settings) then
      begin
        CtxIdx := ConCtx.GetContext.Contexts.IndexOf(ConCtx.GetContext);
        if (CtxIdx = ContextIndex) or ((CtxIdx = -1) and (ContextIndex = 0)) then
        begin
          if Settings.GroupingSettings.Active and not Settings.GroupingSettings.FlatList then
            GetNotifier(I).DataProviderChanging(Self, Reason, GroupedItem)
          else
            GetNotifier(I).DataProviderChanging(Self, Reason, RootItem);
        end;
      end
      else
        { No consumer, contextless consumer or no color settings. Hand over the grouped item as it
          is more specific. }
        GetNotifier(I).DataProviderChanging(Self, pcrAdd, GroupedItem);
    end;
  finally
    ReleaseContext;
  end;
end;

procedure TJvColorProvider.NotifyChanged(Reason: TDataProviderChangeReason; ContextIndex: Integer;
  GroupedItem, RootItem: IUnknown);
var
  I: Integer;
  Consumer: IJvDataConsumer;
  ConCtx: IJvDataConsumerContext;
  Settings: IJvColorProviderSettings;
  CtxIdx: Integer;
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      for I := 0 to GetNotifierCount - 1 do
      begin
        if Supports(GetNotifier(I), IJvDataConsumer, Consumer) and
          Supports(Consumer, IJvDataConsumerContext, ConCtx) and
          Supports(Consumer, IJvColorProviderSettings, Settings) then
        begin
          CtxIdx := ConCtx.GetContext.Contexts.IndexOf(ConCtx.GetContext);
          if (CtxIdx = ContextIndex) or ((CtxIdx = -1) and (ContextIndex = 0)) then
          begin
            if Settings.GroupingSettings.Active and not Settings.GroupingSettings.FlatList then
              GetNotifier(I).DataProviderChanged(Self, Reason, GroupedItem)
            else
              GetNotifier(I).DataProviderChanged(Self, Reason, RootItem);
          end;
        end
        else
          { No consumer, contextless consumer or no color settings. Hand over the grouped item as it
            is more specific. }
          GetNotifier(I).DataProviderChanged(Self, pcrAdd, GroupedItem);
      end;
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorAdding(ColorType: TColorType; ContextIndex: Integer);
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      NotifyChanging(pcrAdd, ContextIndex, TJvColorHeaderItem.Create(GetItems, Ord(ColorType)).Items,
        GetItems);
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorAdded(ColorType: TColorType; ListIndex: Integer;
  ContextIndex: Integer);
var
  Grouped: IJvDataItem;
  Rooted: IJvDataItem;
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      Grouped := TJvColorItem.Create(
        TJvColorHeaderItem.Create(GetItems, Ord(ColorType)) as IJvDataItems, Ord(ColorType),
        ListIndex);
      Rooted := TJvColorItem.Create(GetItems, Ord(ColorType), ListIndex);
      NotifyChanged(pcrAdd, ContextIndex,
        Grouped, Rooted);
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorDeleting(ColorType: TColorType; ListIndex: Integer; ContextIndex: Integer);
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      NotifyChanging(pcrDelete, ContextIndex, TJvColorItem.Create(
          TJvColorHeaderItem.Create(GetItems, Ord(ColorType)) as IJvDataItems, Ord(ColorType),
            ListIndex),
          TJvColorItem.Create(GetItems, Ord(ColorType), ListIndex));
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorDeleted(ColorType: TColorType; ContextIndex: Integer);
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      NotifyChanged(pcrDelete, ContextIndex,
        TJvColorHeaderItem.Create(GetItems, Ord(ColorType)).Items, GetItems);
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorsUpdating(ColorType: TColorType; ContextIndex: Integer);
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      NotifyChanging(pcrUpdateItems, ContextIndex,
        TJvColorHeaderItem.Create(GetItems, Ord(ColorType)) as IJvDataItems, GetItems);
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.NotifyColorsUpdated(ColorType: TColorType; ContextIndex: Integer);
begin
  if ContextIndex = -1 then
    ContextIndex := SelectedContextIndex;
  SelectConsumer(MasterColorConsumer);
  try
    SelectContext((DataContextsImpl as IJvDataContexts).GetContext(ContextIndex));
    try
      NotifyChanged(pcrUpdateItems, ContextIndex,
        TJvColorHeaderItem.Create(GetItems, Ord(ColorType)) as IJvDataItems, GetItems);
    finally
      ReleaseContext;
    end;
  finally
    ReleaseConsumer;
  end;
end;

procedure TJvColorProvider.DoAddColor(ColorType: TColorType; var Color: TColor; var DoAdd: Boolean);
begin
  if Assigned(FOnAddColor) then
    OnAddColor(Self, ColorType, Color, DoAdd);
end;

function TJvColorProvider.AddInternalColor(Color: TColor; AddToCustomDefaultList: Boolean): Integer;
begin
  Result := IndexOfColor(Color);
  if Result = -1 then
  begin
    Result := Length(FColorList);
    SetLength(FColorList, Result + 1);
    FColorList[Result].Value := Color;
    SetLength(FColorList[Result].Names, Mappings.Count);
    if Mappings.Count > 0 then
      FColorList[Result].Names[0] := Format('%s%.8x', [HexDisplayPrefix, Color]);
    FColorListChanged := True;
    if AddToCustomDefaultList then
    begin
      NotifyColorAdding(ctCustom, 0);
      SetLength(FCstColors[0], Length(FCstColors[0]) + 1);
      FCstColors[0][High(FCstColors[0])] := Result;
      NotifyColorAdded(ctCustom, High(FCstColors[0]), 0);
    end;
  end;
end;

function TJvColorProvider.AddColor(var List: TDynIntegerArray; Color: TColor;
  ColorType: TColorType; Context: Integer): Integer;
var
  Temp: Boolean;
begin
  Result := AddColor(List, Color, ColorType, Context, Temp);
end;

function TJvColorProvider.AddColor(var List: TDynIntegerArray; Color: TColor; ColorType: TColorType;
  Context: Integer; out Added: Boolean): Integer;
var
  ColorIdx: Integer;
begin
  ColorIdx := AddInternalColor(Color, (List <> FStdColors[0]) and (List <> FSysColors[0]) and
    (List <> FCstColors[0]));
  Result := IndexOfColIdx(List, ColorIdx);
  if Result = -1 then
  begin
    NotifyColorAdding(ColorType, Context);
    if (List <> FStdColors[0]) and (List <> FSysColors[0]) then
      FColorListChanged := True;
    Result := Length(List);
    SetLength(List, Result + 1);
    List[Result] := ColorIdx;
    NotifyColorAdded(ColorType, Result, Context);
  end;
end;

procedure TJvColorProvider.DeleteColor(var List: TDynIntegerArray; Index: Integer;
  ColorType: TColorType; Context: Integer);
begin
  NotifyColorDeleting(ColorType, Index, Context);
  if (List <> FStdColors[0]) and (List <> FSysColors[0]) then
    FColorListChanged := True;
  if (Index < High(List)) then
    Move(List[Index + 1], List[Index], SizeOf(List[0]) * (High(List) - Index));
  SetLength(List, High(List));
  NotifyColorDeleted(ColorType, Context);
end;

procedure TJvColorProvider.RemoveContextList(Index: Integer);
begin
  if (Index > -1) and (Index < Length(FStdColors)) then
  begin
    FColorListChanged := True;
    SetLength(FStdColors[Index], 0);
    SetLength(FSysColors[Index], 0);
    SetLength(FCstColors[Index], 0);

    if Index < High(FStdColors) then
    begin
      Move(FStdColors[Index + 1], FStdColors[Index], SizeOf(FStdColors[0]) * (High(FStdColors) - Index));
      Move(FSysColors[Index + 1], FSysColors[Index], SizeOf(FSysColors[0]) * (High(FSysColors) - Index));
      Move(FCstColors[Index + 1], FCstColors[Index], SizeOf(FCstColors[0]) * (High(FCstColors) - Index));
      FillChar(FStdColors[High(FStdColors)], SizeOf(FStdColors[High(FStdColors)]), 0);
      FillChar(FSysColors[High(FSysColors)], SizeOf(FSysColors[High(FSysColors)]), 0);
      FillChar(FCstColors[High(FCstColors)], SizeOf(FCstColors[High(FCstColors)]), 0);
    end;
    SetLength(FStdColors, High(FStdColors));
  end;
end;

function TJvColorProvider.IndexOfColor(Color: TColor): Integer;
begin
  Result := High(FColorList);
  while (Result >= 0) and (FColorList[Result].Value <> Color) do
    Dec(Result);
end;

function TJvColorProvider.IndexOfColIdx(const List: TDynIntegerArray; ColorIndex: Integer): Integer;
begin
  Result := High(List);
  while (Result >= 0) and (List[Result] <> ColorIndex) do
    Dec(Result);
end;

procedure TJvColorProvider.CopyFromDefCtx(const TargetContextIndex: Integer);
begin
  if Length(FStdColors) > TargetContextIndex then
  begin
    SetLength(FStdColors[TargetContextIndex], Length(FStdColors[0]));
    Move(FStdColors[0][0], FStdColors[TargetContextIndex][0],
      SizeOf(FStdColors[0]) * Length(FStdColors[0]));
  end;
  if Length(FSysColors) > TargetContextIndex then
  begin
    SetLength(FSysColors[TargetContextIndex], Length(FSysColors[0]));
    Move(FSysColors[0][0], FSysColors[TargetContextIndex][0],
      SizeOf(FSysColors[0]) * Length(FSysColors[0]));
  end;
end;

function TJvColorProvider.SelectedContextIndex: Integer;
begin
  if SelectedContext = nil then
    Result := 0
  else
    Result := (DataContextsImpl as IJvDataContexts).IndexOf(SelectedContext);
  if Result < 0 then
    Result := 0;
end;

class function TJvColorProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvColorItems;
end;

class function TJvColorProvider.ContextsClass: TJvDataContextsClass;
begin
  Result := TJvDataContexts;
end;

class function TJvColorProvider.ContextsManagerClass: TJvDataContextsManagerClass;
begin
  Result := TJvColorContextsManager;
end;

function TJvColorProvider.ConsumerClasses: TClassArray;
begin
  Result := inherited ConsumerClasses;
  AddToArray(Result, TJvColorProviderSettings);
end;

procedure TJvColorProvider.ContextAdded(Context: IJvDataContext);
var
  Idx: Integer;
begin
  inherited ContextAdded(Context);
  Idx := (DataContextsImpl as IJvDataContexts).IndexOf(Context);
  if Idx > -1 then
  begin
    FColorListChanged := True;
    SetLength(FStdColors, Length(FStdColors) + 1);
    SetLength(FSysColors, Length(FSysColors) + 1);
    SetLength(FCstColors, Length(FCstColors) + 1);
    if Idx < High(FStdColors) then
    begin
      Move(FStdColors[Idx], FStdColors[Idx + 1], SizeOf(FStdColors[0]) * (High(FStdColors) - Idx));
      Move(FSysColors[Idx], FSysColors[Idx + 1], SizeOf(FSysColors[0]) * (High(FSysColors) - Idx));
      Move(FCstColors[Idx], FCstColors[Idx + 1], SizeOf(FCstColors[0]) * (High(FCstColors) - Idx));
    end;
    FillChar(FStdColors[Idx], SizeOf(FStdColors[Idx]), 0);
    FillChar(FSysColors[Idx], SizeOf(FSysColors[Idx]), 0);
    FillChar(FCstColors[Idx], SizeOf(FCstColors[Idx]), 0);
  end;
end;

procedure TJvColorProvider.ContextDestroying(Context: IJvDataContext);
var
  Idx: Integer;
begin
  inherited ContextDestroying(Context);
  Idx := (DataContextsImpl as IJvDataContexts).IndexOf(Context);
  if Idx > -1 then
    RemoveContextList(Idx);
end;

procedure TJvColorProvider.InsertMapping(var Strings: TDynStringArray; Index: Integer);
begin
  SetLength(Strings, Length(Strings) + 1);
  if Index < High(Strings) then
    Move(Strings[Index], Strings[Index + 1], (High(Strings) - Index) * SizeOf(string));
  FillChar(Strings[Index], 0, SizeOf(string));
  FColorListChanged := True;
end;

procedure TJvColorProvider.DeleteMapping(var Strings: TDynStringArray; Index: Integer);
begin
  Strings[Index] := '';
  if Index < High(Strings) then
  begin
    Move(Strings[Index + 1], Strings[Index], (High(Strings) - Index) * SizeOf(string));
    FillChar(Strings[High(Strings)], 0, SizeOf(string));
  end;
  SetLength(Strings, High(Strings));
  FColorListChanged := True;
end;

procedure TJvColorProvider.MappingAdding;
var
  I: Integer;
  ColorSettings: IJvColorProviderSettings;
begin
  { Iterate all consumers and notify them of the addition. }
  for I := GetNotifierCount - 1 downto 0 do
    if Supports(GetNotifier(I).Consumer, IJvColorProviderSettings, ColorSettings) then
      ColorSettings.MappingAdding;
end;

procedure TJvColorProvider.MappingAdded(Index: Integer);
var
  Instance: TJvColorProviderNameMapping;
  I: Integer;
  ColorSettings: IJvColorProviderSettings;
begin
  Instance := Mappings[Index];
  { Iterate all consumers and notify them of the addition. }
  for I := GetNotifierCount - 1 downto 0 do
    if Supports(GetNotifier(I).Consumer, IJvColorProviderSettings, ColorSettings) then
      ColorSettings.MappingAdded(Index, Instance);
  { Iterate the colors list and insert the new mapping. }
  for I := 0 to High(FColorList) do
    InsertMapping(FColorList[I].Names, Index);
end;

procedure TJvColorProvider.MappingDestroying(Index: Integer);
var
  Instance: TJvColorProviderNameMapping;
  I: Integer;
  ColorSettings: IJvColorProviderSettings;
begin
  if not (csDestroying in ComponentState) then
  begin
    Instance := Mappings[Index];
    { Iterate all consumers and notify them of the removal of that mapping. }
    for I := GetNotifierCount - 1 downto 0 do
      if Supports(GetNotifier(I).Consumer, IJvColorProviderSettings, ColorSettings) then
        ColorSettings.MappingDestroying(Index, Instance);
    { Iterate the colors list and delete the mapping. }
    for I := 0 to High(FColorList) do
      DeleteMapping(FColorList[I].Names, Index);
  end;
end;

procedure TJvColorProvider.MappingDestroyed;
var
  I: Integer;
  ColorSettings: IJvColorProviderSettings;
begin
  { Iterate all consumers and notify them of the removal. }
  for I := GetNotifierCount - 1 downto 0 do
    if Supports(GetNotifier(I).Consumer, IJvColorProviderSettings, ColorSettings) then
      ColorSettings.MappingDestroyed;
end;

function TJvColorProvider.GetColor(List, Index: Integer; out Value: TColor;
  out Name: string): Boolean;
var
  CtxIdx: Integer;
  ColorIdx: Integer;
  ColorSettings: IJvColorProviderSettings;
  MapIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  ColorIdx := -1;
  case List of
    0:
      begin
        Result := (Index >= 0) and (Index < Length(FStdColors[CtxIdx]));
        if Result then
          ColorIdx := FStdColors[CtxIdx][Index];
      end;
    1:
      begin
        Result := (Index >= 0) and (Index < Length(FSysColors[CtxIdx]));
        if Result then
          ColorIdx := FSysColors[CtxIdx][Index];
      end;
    2:
      begin
        Result := (Index >= 0) and (Index < Length(FCstColors[CtxIdx]));
        if Result then
          ColorIdx := FCstColors[CtxIdx][Index];
      end;
    else
      Result := False;
  end;
  if Result then
  begin
    Value := FColorList[ColorIdx].Value;
    Name := '';
    if Mappings.Count > 0 then
    begin
      if Supports(SelectedConsumer, IJvColorProviderSettings, ColorSettings) then
        MapIdx := ColorSettings.NameMappingIndex
      else
        MapIdx := 0;
      if MapIdx = -1 then
        MapIdx := 0;
      Name := FColorList[ColorIdx].Names[MapIdx];
      if (Name = '') and (MapIdx <> 0) then
        Name := FColorList[ColorIdx].Names[0];
    end;
  end;
end;

procedure TJvColorProvider.SetColorName(List, Index: Integer; const NewName: string);
var
  ValidIndex: Boolean;
  CtxIdx: Integer;
  ColorIdx: Integer;
  ColorSettings: IJvColorProviderSettings;
  MapIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  ColorIdx := -1;
  case List of
    0:
      begin
        ValidIndex := (Index >= 0) and (Index < Length(FStdColors[CtxIdx]));
        if ValidIndex then
          ColorIdx := FStdColors[CtxIdx][Index];
      end;
    1:
      begin
        ValidIndex := (Index >= 0) and (Index < Length(FSysColors[CtxIdx]));
        if ValidIndex then
          ColorIdx := FSysColors[CtxIdx][Index];
      end;
    2:
      begin
        ValidIndex := (Index >= 0) and (Index < Length(FCstColors[CtxIdx]));
        if ValidIndex then
          ColorIdx := FCstColors[CtxIdx][Index];
      end;
    else
      ValidIndex := False;
  end;
  if ValidIndex and (Mappings.Count > 0) then
  begin
    if Supports(SelectedConsumer, IJvColorProviderSettings, ColorSettings) then
      MapIdx := ColorSettings.NameMappingIndex
    else
      MapIdx := 0;
    if MapIdx = -1 then
      MapIdx := 0;
    FColorList[ColorIdx].Names[MapIdx] := NewName;
  end;
end;

function TJvColorProvider.GetColorCount(List: Integer): Integer;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  case List of
    0:
      Result := Length(FStdColors[CtxIdx]);
    1:
      Result := Length(FSysColors[CtxIdx]);
    2:
      Result := Length(FCstColors[CtxIdx]);
    else
      Result := 0;
  end;
end;

procedure TJvColorProvider.GenDelphiConstantMapping;
begin
  Mappings.Add(TJvColorProviderNameMapping.Create(Mappings, RsDelphiConstantNames));
end;

procedure TJvColorProvider.GenEnglishMapping;
begin
  Mappings.Add(TJvColorProviderNameMapping.Create(Mappings, RsEnglishNames));
end;

procedure TJvColorProvider.InitColorList(var List: TDynIntegerArray;
  const Definitions: array of TDefColorItem; ColorType: TColorType);
var
  I: Integer;
  LstIdx: Integer;
  ColIdx: Integer;
begin
  for I := 0 to High(Definitions) do
  begin
    LstIdx := AddColor(List, Definitions[I].Value, ColorType, 0);
    ColIdx := List[LstIdx];
    FColorList[ColIdx].Names[0] := Definitions[I].Constant;
    FColorList[ColIdx].Names[1] := Definitions[I].Description;
  end;
end;

procedure TJvColorProvider.InitColors;
begin
  InitColorList(FStdColors[0], ColorValues, ctStandard);
  InitColorList(FSysColors[0], SysColorValues, ctSystem);
end;

function TJvColorProvider.GetMappings: TJvColorProviderNameMappings;
begin
  Result := FMappings;
end;

procedure TJvColorProvider.DefineProperties(Filer: TFiler);
begin
  { The color list and name mappings must be written first, before the contexts, as the context
    will read in the context specific lists, based on the complete color list. }
  Filer.DefineProperty('Colors', ReadColors, WriteColors, FColorListChanged);
  Filer.DefineProperty('Mappings', ReadMappings, WriteMappings, FColorListChanged);
  inherited DefineProperties(Filer);
end;

procedure TJvColorProvider.ReadColors(Reader: TReader);
begin
  Reader.ReadListBegin;
  SetLength(FColorList, 0);
  FColorListChanged := True; // Make sure it will write the list next time.
  { Since mappings will be read next, clear the list now to save some time when colors are added as
    well as when the mappings are read later. }
  Mappings.Clear;
  while not Reader.EndOfList do
    AddInternalColor(TColor(Reader.ReadInteger), False);
  Reader.ReadListEnd;
end;

procedure TJvColorProvider.WriteColors(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to High(FColorList) do
    Writer.WriteInteger(FColorList[I].Value);
  Writer.WriteListEnd;
end;

procedure TJvColorProvider.ReadMappings(Reader: TReader);
begin
  if Reader.ReadValue <> vaCollection then
    raise EReadError.CreateRes(@RsEMappingCollectionExpected);
  Mappings.Clear;
  while not Reader.EndOfList do
    ReadMapping(Reader);
  Reader.ReadListEnd;
end;

procedure TJvColorProvider.WriteMappings(Writer: TWriter);
var
  I: Integer;
begin
  TWriterAccessProtected(Writer).WriteValue(vaCollection);
  for I := 0 to Mappings.Count - 1 do
    WriteMapping(Writer, I);
  Writer.WriteListEnd;
end;

procedure TJvColorProvider.ReadMapping(Reader: TReader);
var
  Index: Integer;
  I: Integer;
  S: string;
  IEqualPos: Integer;
begin
  Reader.ReadListBegin;
  if not AnsiSameStr(Reader.ReadStr, 'Name') then
    raise EReadError.CreateRes(@RsEExpectedMappingName);
  Index := AddMapping(Reader.ReadString);
  if not AnsiSameStr(Reader.ReadStr, 'Names') then
    raise EReadError.CreateRes(@RsEExpectedNameMappings);
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    S := Reader.ReadString;
    IEqualPos := Pos('=', S);
    if IEqualPos < 1 then
      raise EReadError.CreateRes(@RsEInvalidNameMappingSpecification);
    I := IndexOfColor(StrToInt(Trim(Copy(S, 1, IEqualPos - 1))));
    if I < 0 then
      raise EReadError.CreateResFmt(@RsEUnknownColor, [Trim(Copy(S, 1, IEqualPos - 1))]);
    FColorList[I].Names[Index] := Trim(Copy(S, IEqualPos + 1, Length(S) - IEqualPos));
  end;
  Reader.ReadListEnd;
  Reader.ReadListEnd;
end;

procedure TJvColorProvider.WriteMapping(Writer: TWriter; Index: Integer);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  Writer.WriteStr('Name');
  Writer.WriteString(Mappings[Index].Name);
  Writer.WriteStr('Names');
  Writer.WriteListBegin;
  for I := 0 to High(FColorList) do
  begin
    if FColorList[I].Names[Index] <> '' then
      Writer.WriteString(HexDisplayPrefix + IntToHex(FColorList[I].Value, 8) + ' = ' +
        FColorList[I].Names[Index]);
  end;
  Writer.WriteListEnd;
  Writer.WriteListEnd;
end;

function TJvColorProvider.GetColorItem(ColorType: TColorType; Index: Integer): IJvDataItem;
var
  Settings: IJvColorProviderSettings;
  ItemList: IJvDataItems;
begin
  if Supports(SelectedConsumer, IJvColorProviderSettings, Settings) then
  begin
    if Settings.GroupingSettings.Active and not Settings.GroupingSettings.FlatList then
      ItemList := TJvColorHeaderItem.Create(GetItems, Ord(ColorType)).Items
    else
      ItemList := GetItems;
  end
  else
    ItemList := GetItems;
  Result := TJvColorItem.Create(ItemList, Ord(ColorType), Index);
end;

function TJvColorProvider.GetColorItemByValue(ColorType: TColorType; Color: TColor): IJvDataItem;
var
  Idx: Integer;
begin
  Idx := IndexOfColor(Color);
  if Idx > -1 then
  begin
    case ColorType of
      ctStandard:
        Idx := IndexOfColIdx(FStdColors[SelectedContextIndex], Idx);
      ctSystem:
        Idx := IndexOfColIdx(FSysColors[SelectedContextIndex], Idx);
      ctCustom:
        Idx := IndexOfColIdx(FCstColors[SelectedContextIndex], Idx);
      else
        Idx := -1;
    end;
  end;
  if Idx > -1 then
    Result := GetColorItem(ColorType, Idx)
  else
    Result := nil;
end;

function TJvColorProvider.AddColor(ColorType: TColorType; Color: TColor): Boolean;
begin
  case ColorType of
    ctStandard:
      Result := AddStdColor(Color);
    ctSystem:
      Result := AddSysColor(Color);
    ctCustom:
      Result := AddCstColor(Color);
    else
      Result := False;
  end;
end;

function TJvColorProvider.IndexOfMapping(Mapping: TJvColorProviderNameMapping): Integer;
begin
  Result := Mappings.IndexOf(Mapping);
end;

function TJvColorProvider.IndexOfMappingName(Name: string): Integer;
begin
  Result := 0;
  while (Result < Get_MappingCount) and not AnsiSameText(Mappings[Result].Name, Name) do
    Inc(Result);
  if Result > Get_MappingCount then
    Result := -1;
end;

function TJvColorProvider.Get_MappingCount: Integer;
begin
  Result := Mappings.Count;
end;

function TJvColorProvider.Get_Mapping(Index: Integer): TJvColorProviderNameMapping;
begin
  Result := TJvColorProviderNameMapping(Mappings[Index]);
end;

function TJvColorProvider.AddMapping(AName: string): Integer;
begin
  Result := FMappings.Add(TJvColorProviderNameMapping.Create(FMappings, AName));
end;

function TJvColorProvider.NewMapping: Integer;
begin
  Result := AddMapping(GetUniqueMappingName(FMappings, 'Mapping '));
end;

procedure TJvColorProvider.DeleteMapping(Index: Integer);
begin
  FMappings.Delete(Index);
end;

function TJvColorProvider.GetStandardCount: Integer;
begin
  Result := GetColorCount(0);
end;

function TJvColorProvider.GetSystemCount: Integer;
begin
  Result := GetColorCount(1);
end;

function TJvColorProvider.GetCustomCount: Integer;
begin
  Result := GetColorCount(2);
end;

function TJvColorProvider.GetStandardColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
begin
  Result := GetColor(0, Index, Value, Name);
end;

function TJvColorProvider.GetSystemColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
begin
  Result := GetColor(1, Index, Value, Name);
end;

function TJvColorProvider.GetCustomColor(Index: Integer; out Value: TColor; out Name: string): Boolean;
begin
  Result := GetColor(2, Index, Value, Name);
end;

function TJvColorProvider.FindColor(Value: TColor; out ColorType: TColorType; out Index: Integer): Boolean;
var
  CtxIdx: Integer;
  ColIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  ColIdx := IndexOfColor(Value);

  Index := IndexOfColIdx(FStdColors[CtxIdx], ColIdx);
  if Index > -1 then
    ColorType := ctStandard
  else
  begin
    Index := IndexOfColIdx(FSysColors[CtxIdx], ColIdx);
    if Index > -1 then
      ColorType := ctSystem
    else
    begin
      Index := IndexOfColIdx(FCstColors[CtxIdx], ColIdx);
      if Index > -1 then
        ColorType := ctCustom;
    end;
  end;
  Result := Index >= 0;
end;

procedure TJvColorProvider.SetStandardColorName(Index: Integer; NewName: string);
begin
  SetColorName(0, Index, NewName);
end;

procedure TJvColorProvider.SetSystemColorName(Index: Integer; NewName: string);
begin
  SetColorName(1, Index, NewName);
end;

procedure TJvColorProvider.SetCustomColorName(Index: Integer; NewName: string);
begin
  SetColorName(2, Index, NewName);
end;

function TJvColorProvider.AddStdColor(Value: TColor): Boolean;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    AddColor(FStdColors[CtxIdx], Value, ctStandard, CtxIdx, Result);
end;

procedure TJvColorProvider.DeleteStdColor(Value: TColor);
var
  ColIdx: Integer;
  CtxIdx: Integer;
  ItemIdx: Integer;
begin
  ColIdx := IndexOfColor(Value);
  if ColIdx > -1 then
  begin
    CtxIdx := SelectedContextIndex;
    if CtxIdx > -1 then
    begin
      ItemIdx := IndexOfColIdx(FStdColors[CtxIdx], ColIdx);
      if ItemIdx > -1 then
        DeleteColor(FStdColors[CtxIdx], ItemIdx, ctStandard, CtxIdx);
    end;
  end;
end;

procedure TJvColorProvider.DeleteStdColorAt(Index: Integer);
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    DeleteColor(FStdColors[CtxIdx], Index, ctStandard, CtxIdx);
end;

procedure TJvColorProvider.ClearStdColorList;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
  begin
    NotifyColorsUpdating(ctStandard, CtxIdx);
    SetLength(FStdColors[CtxIdx], 0);
    NotifyColorsUpdated(ctStandard, CtxIdx);
  end;
end;

function TJvColorProvider.AddSysColor(Value: TColor): Boolean;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    AddColor(FSysColors[CtxIdx], Value, ctSystem, CtxIdx, Result);
end;

procedure TJvColorProvider.DeleteSysColor(Value: TColor);
var
  ColIdx: Integer;
  CtxIdx: Integer;
  ItemIdx: Integer;
begin
  ColIdx := IndexOfColor(Value);
  if ColIdx > -1 then
  begin
    CtxIdx := SelectedContextIndex;
    if CtxIdx > -1 then
    begin
      ItemIdx := IndexOfColIdx(FSysColors[CtxIdx], ColIdx);
      if ItemIdx > -1 then
        DeleteColor(FSysColors[CtxIdx], ItemIdx, ctSystem, CtxIdx);
    end;
  end;
end;

procedure TJvColorProvider.DeleteSysColorAt(Index: Integer);
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    DeleteColor(FSysColors[CtxIdx], Index, ctSystem, CtxIdx);
end;

procedure TJvColorProvider.ClearSysColorList;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
  begin
    NotifyColorsUpdating(ctSystem, CtxIdx);
    SetLength(FSysColors[CtxIdx], 0);
    NotifyColorsUpdated(ctSystem, CtxIdx);
  end;
end;

function TJvColorProvider.AddCstColor(Value: TColor): Boolean;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    AddColor(FCstColors[CtxIdx], Value, ctCustom, CtxIdx, Result);
end;

procedure TJvColorProvider.DeleteCstColor(Value: TColor);
var
  ColIdx: Integer;
  CtxIdx: Integer;
  ItemIdx: Integer;
begin
  ColIdx := IndexOfColor(Value);
  if ColIdx > -1 then
  begin
    CtxIdx := SelectedContextIndex;
    if CtxIdx > -1 then
    begin
      ItemIdx := IndexOfColIdx(FCstColors[CtxIdx], ColIdx);
      if ItemIdx > -1 then
        DeleteColor(FCstColors[CtxIdx], ItemIdx, ctCustom, CtxIdx);
    end;
  end;
end;

procedure TJvColorProvider.DeleteCstColorAt(Index: Integer);
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
    DeleteColor(FCstColors[CtxIdx], Index, ctCustom, CtxIdx);
end;

procedure TJvColorProvider.ClearCstColorList;
var
  CtxIdx: Integer;
begin
  CtxIdx := SelectedContextIndex;
  if CtxIdx > -1 then
  begin
    NotifyColorsUpdating(ctCustom, CtxIdx);
    SetLength(FCstColors[CtxIdx], 0);
    NotifyColorsUpdated(ctCustom, CtxIdx);
  end;
end;

//=== { TJvColorProviderNameMappings } =======================================

constructor TJvColorProviderNameMappings.Create(AProvider: TJvColorProvider);
begin
  inherited Create(True);
  FProvider := AProvider;
end;

function TJvColorProviderNameMappings.GetItem(Index: Integer): TJvColorProviderNameMapping;
begin
  Result := TJvColorProviderNameMapping(inherited GetItem(Index));
end;

procedure TJvColorProviderNameMappings.SetItem(Index: Integer;
  AObject: TJvColorProviderNameMapping);
begin
  inherited SetItem(Index, AObject);
end;

function TJvColorProviderNameMappings.Add(Item: TJvColorProviderNameMapping): Integer;
begin
  Provider.MappingAdding;
  Result := inherited Add(Item);
  Provider.MappingAdded(Result);
end;

procedure TJvColorProviderNameMappings.Clear;
var
  I: Integer;
begin
  I := Count - 1;
  while I >= 0 do
  begin
    Delete(I);
    Dec(I);
  end;
end;

procedure TJvColorProviderNameMappings.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < Count) then
  begin
    Provider.MappingDestroying(Index);
    inherited Delete(Index);
    Provider.MappingDestroyed;
  end;
end;

//=== { TJvColorItemsRenderer } ==============================================

function TJvColorItemsRenderer.GetRenderText: string;
var
  ItemText: IJvDataItemText;
begin
  with CurrentSettings.TextSettings do
  begin
    if Active or (CurrentItemIsColorItem and (CurrentColorValue >= clNone)) then
    begin
      if ShowName and Supports(CurrentItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := '';
      if CurrentItemIsColorItem then
      begin
        if ShowHex or (Result = '') then
        begin
          if Result <> '' then
            Result := Result + Format(' (%s%.8x)', [HexDisplayPrefix, CurrentColorValue])
          else
            Result := Format('%s%.8x', [HexDisplayPrefix, CurrentColorValue]);
        end;
        if ShowRGB then
        begin
          if Result <> '' then
            Result := Result + Format(' (%d, %d, %d)', [
              GetRValue(ColorToRGB(CurrentColorValue)),
              GetGValue(ColorToRGB(CurrentColorValue)),
              GetBValue(ColorToRGB(CurrentColorValue))])
          else
            Result := Format('(%d, %d, %d)', [
              GetRValue(ColorToRGB(CurrentColorValue)),
              GetGValue(ColorToRGB(CurrentColorValue)),
              GetBValue(ColorToRGB(CurrentColorValue))]);
        end;
      end;
    end
    else
    if not CurrentItemIsColorItem then
    begin
      if Supports(CurrentItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := RsDataItemRenderHasNoText;
    end
    else
      Result := '';
  end;
end;

procedure TJvColorItemsRenderer.RenderColorBox;
var
  Margin: Integer;
  BoxW: Integer;
  BoxH: Integer;
  ShadowSize: Integer;
  R: TRect;
  SaveColor: TColor;
begin
  if CurrentSettings.ColorBoxSettings.Active then
  begin
    Margin := CurrentSettings.ColorBoxSettings.Margin;
    if CurrentSettings.TextSettings.Active then
      BoxW := CurrentSettings.ColorBoxSettings.Width
    else
      BoxW := CurrentRect.Right - CurrentRect.Left - (2 * Margin);
    BoxH := CurrentSettings.ColorBoxSettings.Height;
    if CurrentSettings.ColorBoxSettings.Shadowed then
      ShadowSize := CurrentSettings.ColorBoxSettings.ShadowSize
    else
      ShadowSize := 0;
    R := CurrentRect;
    OffsetRect(R, Margin, Margin);
    R.Right := R.Left + BoxW - ShadowSize;
    R.Bottom := R.Top + BoxH - ShadowSize;
    if (CurrentItemIsColorItem) and (CurrentColorValue < clNone) then
      with CurrentCanvas do
      begin
        SaveColor := Brush.Color;
        try
          Brush.Color := CurrentColorValue;
          FillRect(R);
          if CurrentSettings.ColorBoxSettings.Shadowed then
          begin
            Brush.Color := clGray;
            OffsetRect(R, ShadowSize, ShadowSize);
            FillRect(R);
            OffsetRect(R, -ShadowSize, -ShadowSize);
          end;
          Brush.Color := CurrentColorValue;
          Rectangle(R);
        finally
          Brush.Color := SaveColor;
        end;
      end;
    if CurrentSettings.TextSettings.Active then
      CurrentRect.Left := R.Right + CurrentSettings.ColorBoxSettings.Spacing;
  end;
end;

procedure TJvColorItemsRenderer.RenderColorText;
var
  S: string;
  R: TRect;
  OldBkMode: Integer;
begin
  if CurrentSettings.TextSettings.Active or (CurrentColorValue >= clNone) then
  begin
    S := GetRenderText;
    R := CurrentRect; 
    CurrentCanvas.Start; 
    OldBkMode := SetBkMode(CurrentCanvas.Handle, TRANSPARENT);
    try  
      DrawText(CurrentCanvas, S, Length(S), R, DT_SINGLELINE or DT_END_ELLIPSIS or
        DT_VCENTER or DT_NOPREFIX); 
    finally
      SetBkMode(CurrentCanvas.Handle, OldBkMode); 
      CurrentCanvas.Stop; 
    end;
  end;
end;

procedure TJvColorItemsRenderer.RenderGroupHeader;
var
  S: string;
  OldFont: TFont;
  R: TRect;
  ha: TColorGroupHeaderAlign;
  RWidth: Integer;
  RVCenter: Integer;
  OldBkMode: Integer;
begin
  S := GetRenderText;
  OldFont := TFont.Create; 
  CurrentCanvas.Start; 
  try
    OldFont.Assign(CurrentCanvas.Font);
    try
      if ghsBoldFont in CurrentSettings.GroupingSettings.HeaderStyle then
        CurrentCanvas.Font.Style := CurrentCanvas.Font.Style + [fsBold];
      R := CurrentRect;
      Dec(R.Right, 2);
      if not CurrentSettings.GroupingSettings.FlatList then
        ha := ghaLeft
      else
        ha := CurrentSettings.GroupingSettings.HeaderAlign;
      case ha of
        ghaLeft:
          Inc(R.Left, 2);
        ghaColorText:
          begin
            if not CurrentSettings.TextSettings.Active or not CurrentSettings.ColorBoxSettings.Active then
              Inc(R.Left, 2)
            else
              with CurrentSettings.ColorBoxSettings do
                Inc(R.Left, Margin + Width + Spacing);
          end;
        ghaCenter:
          begin
            R.Left := 0;
            DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_NOPREFIX or
              DT_CALCRECT);
            RWidth := R.Right;
            R := CurrentRect;
            Inc(R.Left, 2);
            Dec(R.Right, 2);
            if RWidth < (R.Right - R.Left) then
            begin
              R.Left := R.Left + ((R.Right - R.Left - RWidth) div 2);
              R.Right := R.Left + RWidth;
            end;
          end;
      end;
      OldBkMode := SetBkMode(CurrentCanvas.Handle, TRANSPARENT);
      try
        DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_END_ELLIPSIS or
          DT_VCENTER or DT_NOPREFIX);
      finally
        SetBkMode(CurrentCanvas.Handle, OldBkMode);
      end;
      with CurrentSettings.GroupingSettings do
        if ([ghsSingleCenterLine, ghsDoubleCenterLine] * HeaderStyle) <> [] then
        begin
          RVCenter := CurrentRect.Top + (CurrentRect.Bottom - CurrentRect.Top) div 2;
          if R.Left > (CurrentRect.Left + 6) then
          begin
            if ghsSingleCenterLine in HeaderStyle then
            begin
              CurrentCanvas.MoveTo(CurrentRect.Left + 2, RVCenter);
              CurrentCanvas.LineTo(R.Left - 1, RVCenter);
            end
            else
            begin
              CurrentCanvas.MoveTo(CurrentRect.Left + 2, RVCenter - 1);
              CurrentCanvas.LineTo(R.Left - 1, RVCenter - 1);
              CurrentCanvas.MoveTo(CurrentRect.Left + 2, RVCenter + 1);
              CurrentCanvas.LineTo(R.Left - 1, RVCenter + 1);
            end
          end;
          DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_CALCRECT or
            DT_NOPREFIX);
          if R.Right < (CurrentRect.Right - 6) then
          begin
            if ghsSingleCenterLine in HeaderStyle then
            begin
              CurrentCanvas.MoveTo(R.Right + 2, RVCenter);
              CurrentCanvas.LineTo(CurrentRect.Right - 1, RVCenter);
            end
            else
            begin
              CurrentCanvas.MoveTo(R.Right + 2, RVCenter - 1);
              CurrentCanvas.LineTo(CurrentRect.Right - 1, RVCenter - 1);
              CurrentCanvas.MoveTo(R.Right + 2, RVCenter + 1);
              CurrentCanvas.LineTo(CurrentRect.Right - 1, RVCenter + 1);
            end;
          end;
        end;
    finally
      CurrentCanvas.Font.Assign(OldFont);
    end;
  finally 
    CurrentCanvas.Stop; 
    OldFont.Free;
  end;
end;

procedure TJvColorItemsRenderer.MeasureColorBox(var Size: TSize);
var
  Margin: Integer;
  BoxW: Integer;
  BoxH: Integer;
  XSize: Integer;
  YSize: Integer;
begin
  if CurrentSettings.ColorBoxSettings.Active then
  begin
    Margin := CurrentSettings.ColorBoxSettings.Margin;
    if CurrentSettings.TextSettings.Active then
      BoxW := CurrentSettings.ColorBoxSettings.Width
    else
      BoxW := CurrentSettings.ColorBoxSettings.Width + Margin;
    BoxH := CurrentSettings.ColorBoxSettings.Height;

    XSize := Margin + BoxW;
    YSize := 2 * Margin + BoxH;
    if Size.cx < XSize then
      Size.cx := XSize;
    if Size.cy < YSize then
      Size.cy := YSize;
  end;
end;

procedure TJvColorItemsRenderer.MeasureColorText(var Size: TSize);
var
  XAdd: Integer;
  S: string;
  R: TRect;
begin
  if CurrentSettings.TextSettings.Active then
  begin
    XAdd := Size.cx;
    if XAdd > 0 then
      Inc(XAdd, CurrentSettings.ColorBoxSettings.Spacing);
    S := GetRenderText;
    R := Rect(0, 0, 0, 0); 
    CurrentCanvas.Start; 
    DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_NOPREFIX or
      DT_CALCRECT); 
    CurrentCanvas.Stop; 
    Inc(R.Right, XAdd);
    if R.Right > Size.cx then
      Size.cx := R.Right;
    if R.Bottom > Size.cy then
      Size.cy := R.Bottom;
  end;
end;

procedure TJvColorItemsRenderer.DoDrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem;
  State: TProviderDrawStates);
begin
  // setup protected fields
  CurrentCanvas := ACanvas;
  CurrentRect := ARect;
  CurrentItem := Item;
  try
    CurrentState := State;
    CurrentSettings := GetConsumerSettings;
    try
      CurrentItemIsColorItem := GetItemColorValue(Item, CurrentColorValue);
      if CurrentItemIsColorItem then
      begin
        // render the color box and/or text
        RenderColorBox;
        RenderColorText;
      end
      else
      if AnsiSameText(Item.GetID, cColorProviderAddItemID) then
      begin
        CurrentColorValue := clNone;
        RenderColorBox;
        RenderColorText;
      end
      else
        RenderGroupHeader;
    finally
      CurrentSettings := nil;
    end;
  finally
    CurrentItem := nil;
  end;
end;

function TJvColorItemsRenderer.DoMeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
begin
  // setup protected fields
  CurrentCanvas := ACanvas;
  CurrentItem := Item;
  try
    CurrentSettings := GetConsumerSettings;
    try
      CurrentItemIsColorItem := GetItemColorValue(Item, CurrentColorValue);
      Result.cx := 0;
      Result.cy := 0;
      MeasureColorBox(Result);
      MeasureColorText(Result);
    finally
      CurrentSettings := nil;
    end;
  finally
    CurrentItem := nil;
  end;
end;

function TJvColorItemsRenderer.AvgItemSize(ACanvas: TCanvas): TSize;
var
  ChHeight: Integer;
  Metrics: TTextMetric;
  ChWdth: Integer;
begin
  CurrentSettings := GetConsumerSettings;
  try
    Result.cx := 0;
    Result.cy := 0;
    MeasureColorBox(Result);
    if CurrentSettings.TextSettings.Active then
    begin
      ChHeight := CanvasMaxTextHeight(ACanvas);
      if ChHeight > Result.cy then
        Result.cy := ChHeight; 
      ACanvas.Start; 
      GetTextMetrics(ACanvas.Handle, Metrics); 
      ACanvas.Stop; 
      ChWdth := Metrics.tmAveCharWidth;
      if CurrentSettings.ColorBoxSettings.Active then
        Result.cx := Result.cx + CurrentSettings.ColorBoxSettings.Spacing + (10 * ChWdth)
      else
        Result.cx := 10 * ChWdth;
    end;
  finally
    CurrentSettings := nil;
  end;
end;

function TJvColorItemsRenderer.GetConsumerSettings: IJvColorProviderSettings;
begin
  Supports(Items.GetProvider.SelectedConsumer, IJvColorProviderSettings, Result);
end;

//=== { TJvColorProviderSettings } ===========================================

constructor TJvColorProviderSettings.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  Changing(ccrViewChange);
  FColorBoxSettings := TJvColorProviderColorBoxSettings.Create(Self);
  FCustomColorSettings := TJvColorProviderCustomColorGroupSettings.Create(Self, RsCustomColors);
  FGroupingSettings := TJvColorProviderGroupingSettings.Create(Self);
  FTextSettings := TJvColorProviderTextSettings.Create(Self);
  FStandardColorSettings := TJvColorProviderColorGroupSettings.Create(Self, RsStandardColors);
  FSystemColorSettings := TJvColorProviderColorGroupSettings.Create(Self, RsSystemColors);
  FMapping := -1;
  Changed(ccrViewChange);
end;

destructor TJvColorProviderSettings.Destroy;
begin
  FreeAndNil(FColorBoxSettings);
  FreeAndNil(FCustomColorSettings);
  FreeAndNil(FGroupingSettings);
  FreeAndNil(FTextSettings);
  FreeAndNil(FStandardColorSettings);
  FreeAndNil(FSystemColorSettings);
  inherited Destroy;
end;

function TJvColorProviderSettings.Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
begin
  Result := FColorBoxSettings;
end;

function TJvColorProviderSettings.Get_CustomColorSettings: TJvColorProviderCustomColorGroupSettings;
begin
  Result := FCustomColorSettings;
end;

function TJvColorProviderSettings.Get_GroupingSettings: TJvColorProviderGroupingSettings;
begin
  Result := FGroupingSettings;
end;

function TJvColorProviderSettings.Get_NameMapping: TJvColorProviderNameMapping;
var
  ColorProvider: IJvColorProvider;
begin
  if (FMapping > -1) and Supports(ConsumerImpl.ProviderIntf, IJvColorProvider, ColorProvider) then
    Result := ColorProvider.Mappings[FMapping]
  else
    Result := nil;
end;

function TJvColorProviderSettings.Get_NameMappingIndex: Integer;
begin
  Result := FMapping;
end;

function TJvColorProviderSettings.Get_StandardColorSettings: TJvColorProviderColorGroupSettings;
begin
  Result := FStandardColorSettings;
end;

function TJvColorProviderSettings.Get_SystemColorSettings: TJvColorProviderColorGroupSettings;
begin
  Result := FSystemColorSettings;
end;

function TJvColorProviderSettings.Get_TextSettings: TJvColorProviderTextSettings;
begin
  Result := FTextSettings;
end;

procedure TJvColorProviderSettings.Set_ColorBoxSettings(Value: TJvColorProviderColorBoxSettings);
begin
end;

procedure TJvColorProviderSettings.Set_CustomColorSettings(
  Value: TJvColorProviderCustomColorGroupSettings);
begin
end;

procedure TJvColorProviderSettings.Set_GroupingSettings(Value: TJvColorProviderGroupingSettings);
begin
end;

procedure TJvColorProviderSettings.Set_NameMapping(Value: TJvColorProviderNameMapping);
var
  Idx: Integer;
  ColorProvider: IJvColorProvider;
begin
  if Value = nil then
    Idx := -2
  else
  begin
    if Supports(ConsumerImpl.ProviderIntf, IJvColorProvider, ColorProvider) then
      Idx := ColorProvider.IndexOfMapping(Value)
    else
      Idx := -1;
  end;
  if Idx <> -1 then
  begin
    if Idx < 0 then
      Idx := -1;
    Set_NameMappingIndex(Idx);
  end
  else
    raise EJVCLDataConsumer.CreateRes(@RsESpecifiedMappingError);
end;

procedure TJvColorProviderSettings.Set_NameMappingIndex(Value: Integer);
begin
  if FMapping <> Value then
  begin
    FMapping := Value;
    Changed(ccrOther);
  end;
end;

procedure TJvColorProviderSettings.Set_StandardColorSettings(
  Value: TJvColorProviderColorGroupSettings);
begin
end;

procedure TJvColorProviderSettings.Set_SystemColorSettings(
  Value: TJvColorProviderColorGroupSettings);
begin
end;

procedure TJvColorProviderSettings.Set_TextSettings(Value: TJvColorProviderTextSettings);
begin
end;

procedure TJvColorProviderSettings.MappingAdding;
begin
  if Assigned(FOnMappingAdding) then
    FOnMappingAdding(Self);
end;

procedure TJvColorProviderSettings.MappingAdded(Index: Integer; Mapping: TJvColorProviderNameMapping);
begin
  if FMapping >= Index then
  begin
    Inc(FMapping);
    Changed(ccrOther);
  end;
  if Assigned(FOnMappingAdded) then
    FOnMappingAdded(Self, Index, Mapping);
end;

procedure TJvColorProviderSettings.MappingDestroying(Index: Integer;
  Mapping: TJvColorProviderNameMapping);
begin
  if Assigned(FOnMappingDestroying) then
    FOnMappingDestroying(Self, Index, Mapping);
  if FMapping = Index then
  begin
    FMapping := -1;
    Changed(ccrOther);
  end
  else
  if FMapping > Index then
  begin
    Dec(FMapping);
    Changed(ccrOther);
  end;
end;

procedure TJvColorProviderSettings.MappingDestroyed;
begin
  if Assigned(FOnMappingDestroyed) then
    FOnMappingDestroyed(Self);
end;

function TJvColorProviderSettings.GetNameMappingIndex: TJvColorProviderMapping;
begin
  Result := Get_NameMappingIndex;
end;

procedure TJvColorProviderSettings.SetNameMappingIndex(Value: TJvColorProviderMapping);
begin
  Set_NameMappingIndex(Value);
end;

//=== { TJvColorContextsManager } ============================================

function TJvColorContextsManager.New: IJvDataContext;
begin
  Result := Add(TJvColorContext.Create(ContextsImpl, GetUniqueCtxName(ContextsImpl, 'Context')));
end;

//=== { TJvColorContext } ====================================================

function TJvColorContext.IsDeletable: Boolean;
begin
  Result := Contexts.IndexOf(Self) > 0;
end;

function TJvColorContext.IsStreamable: Boolean;
begin
  Result := True;
end;

procedure TJvColorContext.DefineProperties(Filer: TFiler);
var
  CPImpl: TJvColorProvider;
begin
  inherited DefineProperties(Filer);
  CPImpl := Contexts.Provider.GetImplementer as TJvColorProvider;
  Filer.DefineProperty('StdColors', ReadStdColors, WriteStdColors, IsDeletable or
    CPImpl.FColorListChanged);
  Filer.DefineProperty('SysColors', ReadSysColors, WriteSysColors, IsDeletable or
    CPImpl.FColorListChanged);
  Filer.DefineProperty('CstColors', ReadCstColors, WriteCstColors, True);
end;

procedure TJvColorContext.ReadStdColors(Reader: TReader);
begin
  ReadCtxList(Reader,
    TJvColorProvider(Contexts.Provider.GetImplementer).FStdColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.WriteStdColors(Writer: TWriter);
begin
  WriteCtxList(Writer,
    TJvColorProvider(Contexts.Provider.GetImplementer).FStdColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.ReadSysColors(Reader: TReader);
begin
  ReadCtxList(Reader,
    TJvColorProvider(Contexts.Provider.GetImplementer).FSysColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.WriteSysColors(Writer: TWriter);
begin
  WriteCtxList(Writer,
    TJvColorProvider(Contexts.Provider.GetImplementer).FSysColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.ReadCstColors(Reader: TReader);
begin
  ReadCtxList(Reader,
    TJvColorProvider(Contexts.Provider.GetImplementer).FCstColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.WriteCstColors(Writer: TWriter);
begin
  WriteCtxList(Writer,
    TJvColorProvider(Contexts.Provider.GetImplementer).FCstColors[Contexts.IndexOf(Self)]);
end;

procedure TJvColorContext.ReadCtxList(Reader: TReader; var List: TDynIntegerArray);
var
  CPImpl: TJvColorProvider;
  Color: Integer;
  ColIdx: Integer;
begin
  Reader.ReadListBegin;
  CPImpl := Contexts.Provider.GetImplementer as TJvColorProvider;
  SetLength(List, 0);
  while not Reader.EndOfList do
  begin
    Color := Reader.ReadInteger;
    ColIdx := CPImpl.IndexOfColor(Color);
    if ColIdx < 0 then
      raise EReadError.CreateResFmt(@RsEInvalidColor, [Color]);
    if CPImpl.IndexOfColIdx(List, ColIdx) < 0 then
    begin
      SetLength(List, Length(List) + 1);
      List[High(List)] := ColIdx;
    end;
  end;
  Reader.ReadListEnd;
end;

procedure TJvColorContext.WriteCtxList(Writer: TWriter; const List: TDynIntegerArray);
var
  CPImpl: TJvColorProvider;
  I: Integer;
begin
  CPImpl := Contexts.Provider.GetImplementer as TJvColorProvider;
  Writer.WriteListBegin;
  for I := 0 to High(List) do
    Writer.WriteInteger(CPImpl.FColorList[List[I]].Value);
  Writer.WriteListEnd;
end;

//=== { TJvColorMapItems } ===================================================

constructor TJvColorMapItems.Create;
begin
  inherited Create;
  FItemInstances := TList.Create;
end;

destructor TJvColorMapItems.Destroy;
begin
  FreeAndNil(FConsumer);
  FreeAndNil(FItemInstances);
  inherited Destroy;
end;

function TJvColorMapItems.GetClientProvider: IJvDataProvider;
begin
  Result := FConsumer.ProviderIntf;
end;

procedure TJvColorMapItems.SetClientProvider(Value: IJvDataProvider);
begin
  if Value <> ClientProvider then
  begin
    GetProvider.Changing(pcrFullRefresh, nil);
    FConsumer.SetProviderIntf(Value);
    ClearIntfImpl;
    if Value <> nil then
      InitImplementers;
    GetProvider.Changed(pcrFullRefresh, nil);
  end;
end;

procedure TJvColorMapItems.DataProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  if AReason = pcrDestroy then
    ClientProvider := nil;
end;

procedure TJvColorMapItems.DataProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
begin
end;

procedure TJvColorMapItems.SubServiceCreated(Sender: TJvDataConsumer;
  SubSvc: TJvDataConsumerAggregatedObject);
begin
  if SubSvc is TJvColorProviderSettings then
    with TJvColorProviderSettings(SubSvc) do
    begin
      OnMappingAdding := Self.MappingAdding;
      OnMappingAdded := Self.MappingAdded;
      OnMappingDestroying := Self.MappingDestroying;
      OnMappingDestroyed := Self.MappingDestroyed;
    end;
end;

procedure TJvColorMapItems.ConsumerChanged(Sender: TJvDataConsumer;
  Reason: TJvDataConsumerChangeReason);
begin
end;

procedure TJvColorMapItems.MappingAdding(Sender: TObject);
begin
  GetProvider.Changing(pcrAdd, Self);
end;

procedure TJvColorMapItems.MappingAdded(Sender: TJvColorProviderSettings; Index: Integer;
  Mapping: TJvColorProviderNameMapping);
var
  I: Integer;
begin
  // Iterate instance list and update item indices...
  for I := 0 to FItemInstances.Count - 1 do
    if TJvColorMapItem(FItemInstances[I]).FIndex >= Index then
      Inc(TJvColorMapItem(FItemInstances[I]).FIndex);
  GetProvider.Changed(pcrAdd, GetItem(Index));
end;

procedure TJvColorMapItems.MappingDestroying(Sender: TJvColorProviderSettings; Index: Integer;
  Mapping: TJvColorProviderNameMapping);
var
  I: Integer;
begin
  GetProvider.Changing(pcrDelete, GetItem(Index));
  // Iterate instance list and update item indices...
  for I := 0 to FItemInstances.Count - 1 do
    if TJvColorMapItem(FItemInstances[I]).FIndex = Index then
      TJvColorMapItem(FItemInstances[I]).FIndex := -1
    else
    if TJvColorMapItem(FItemInstances[I]).FIndex > Index then
      Dec(TJvColorMapItem(FItemInstances[I]).FIndex);
end;

procedure TJvColorMapItems.MappingDestroyed(Sender: TObject);
begin
  GetProvider.Changed(pcrDelete, Self);
end;

function TJvColorMapItems.GetCount: Integer;
var
  ColProv: IJvColorProvider;
begin
  if Supports(ClientProvider, IJvColorProvider, ColProv) then
    Result := ColProv.MappingCount
  else
    Result := 0;
end;

function TJvColorMapItems.GetItem(I: Integer): IJvDataItem;
var
  InstIdx: Integer;
begin
  InstIdx := FItemInstances.Count - 1;
  while (InstIdx >= 0) and (TJvColorMapItem(FItemInstances[InstIdx]).Index <> I) do
    Dec(InstIdx);
  if InstIdx > -1 then
    Result := TJvColorMapItem(FItemInstances[InstIdx])
  else
  begin
    Result := TJvColorMapItem.Create(Self, I);
    FItemInstances.Add(Result.GetImplementer);
  end;
end;

procedure TJvColorMapItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvColorMapItemsManager.Create(Self);
end;

procedure TJvColorMapItems.AfterConstruction;
var
  ICR: IInterfaceComponentReference;
begin
  inherited AfterConstruction;
  if Supports(GetProvider, IInterfaceComponentReference, ICR) then
    FConsumer := TJvDataConsumer.Create(ICR.GetComponent, [])
  else
    FConsumer := TJvDataConsumer.Create(nil, []);
  FConsumer.OnProviderChanging := DataProviderChanging;
  FConsumer.OnProviderChanged := DataProviderChanged;
  FConsumer.AfterCreateSubSvc := SubServiceCreated;
  FConsumer.OnChanged := ConsumerChanged;
end;

//=== { TJvColorMapItem } ====================================================

constructor TJvColorMapItem.Create(AOwner: IJvDataItems; AIndex: Integer);
begin
  inherited Create(AOwner);
  FIndex := AIndex;
end;

destructor TJvColorMapItem.Destroy;
begin
  TJvColorMapItems(GetItems.GetImplementer).FItemInstances.Remove(Self);
  inherited Destroy;
end;

function TJvColorMapItem.GetCaption: string;
begin
  Result := Get_NameMapping.Name;
end;

procedure TJvColorMapItem.SetCaption(const Value: string);
begin
  Get_NameMapping.Name := Value;
end;

function TJvColorMapItem.Editable: Boolean;
begin
  Result := True;
end;

function TJvColorMapItem.Get_NameMapping: TJvColorProviderNameMapping;
begin
  Result := (TJvColorMapItems(GetItems.GetImplementer).ClientProvider as
    IJvColorProvider).Mappings[Index];
end;

procedure TJvColorMapItem.InitID;
begin
  SetID(cColorProviderColorMapItemID + IntToStr(Index));
end;

//=== { TJvColorMapItemsManager } ============================================

function TJvColorMapItemsManager.Add(Item: IJvDataItem): IJvDataItem;
begin
  { Any item that comes through this method is always referencing an existing mapping. Since
    duplicate mappings are not allowed, we can just ignore the add and return the same item. }
  Result := Item;
end;

function TJvColorMapItemsManager.New: IJvDataItem;
var
  ColProv: IJvColorProvider;
  MapIdx: Integer;
begin
  if Supports(TJvColorMapItems(ItemsImpl).ClientProvider, IJvColorProvider, ColProv) then
  begin
    MapIdx := ColProv.NewMapping;
    Result := Items.GetItem(MapIdx);
  end;
end;

procedure TJvColorMapItemsManager.Clear;
var
  ColProv: IJvColorProvider;
begin
  if Supports(TJvColorMapItems(ItemsImpl).ClientProvider, IJvColorProvider, ColProv) then
    while ColProv.MappingCount > 0 do
      ColProv.DeleteMapping(0);
end;

procedure TJvColorMapItemsManager.Delete(Index: Integer);
var
  ColProv: IJvColorProvider;
begin
  if Supports(TJvColorMapItems(ItemsImpl).ClientProvider, IJvColorProvider, ColProv) then
    ColProv.DeleteMapping(Index);
end;

procedure TJvColorMapItemsManager.Remove(var Item: IJvDataItem);
var
  MapItem: IJvColorMapItem;
  ColProv: IJvColorProvider;
  MapIdx: Integer;
begin
  if Supports(Item, IJvColorMapItem, MapItem) then
  begin
    if Supports(TJvColorMapItems(ItemsImpl).ClientProvider, IJvColorProvider, ColProv) then
    begin
      MapIdx := ColProv.IndexOfMapping(MapItem.NameMapping);
      if MapIdx > -1 then
      begin
        Item := nil;
        MapItem := nil;
        Delete(MapIdx);
      end
      else
        raise EJVCLDataItems.CreateRes(@RsEItemNotForList);
    end
    else
      raise EJVCLDataItems.CreateRes(@RsEItemNotForList);
  end
  else
    raise EJVCLDataItems.CreateRes(@RsEItemNotForList);
end;

//=== { TJvColorConsumer } ===================================================

function TJvColorConsumer.VCLComponent: TComponent;
begin
  Result := nil;
end;

function TJvColorConsumer.AttributeApplies(Attr: Integer): Boolean;
begin
  Result := False;
end;

initialization
  RegisterClasses([TJvColorProviderSettings, TJvColorProviderServerNotify, TJvColorContext]);
  MasterColorConsumer := TJvColorConsumer.Create;

finalization
  MasterColorConsumer := nil;

end.

