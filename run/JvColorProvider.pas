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

Last Modified: 2003-09-17

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvColorProvider;

interface

uses
  Classes, Graphics, Windows,
  JclBase,
  JvDataProvider, JvDataProviderImpl;

type
  TColorItem = record
    Value: TColor;
//    Names: TDynStringArray;
  end;
  TColorItems = array of TColorItem;

  TJvColorProvider = class(TJvCustomDataProvider)
  private
    FStdColors: TColorItems;
    FSysColors: TColorItems;
    FCstColors: TColorItems;
  private
    procedure AddColorStr(const S: string);
  protected
    procedure AddColor(var List: TColorItems; Color: TColor);
    procedure DeleteColor(var List: TColorItems; Index: Integer);
    class function ItemsClass: TJvDataItemsClass; override;
    function ConsumerClasses: TClassArray; override;

    property StdColors: TColorItems read FStdColors write FStdColors;
    property SysColors: TColorItems read FSysColors write FSysColors;
    property CstColors: TColorItems read FCstColors write FCstColors;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TJvColorProviderAddItemLocation = (ailUseHeader, ailTop, ailBottom);
  TJvColorProviderAddColorStyle = (aisBorland, aisEvent);
  TColorGroupHeaderAlign = (ghaLeft, ghaCenter, ghaColorText);
  TColorGroupHeaderStyle = (ghsBoldFont, ghsSingleCenterLine, ghsDoubleCenterLine);
  TColorGroupHeaderStyles = set of TColorGroupHeaderStyle;

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
    procedure SetActive(Value: Boolean); override;
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
    FStyle: TJvColorProviderAddColorStyle;
  protected
    procedure SetLocation(Value: TJvColorProviderAddItemLocation); virtual;
    procedure SetCaption(Value: string); virtual;
    procedure SetStyle(Value: TJvColorProviderAddColorStyle); virtual;
  public
    constructor Create(AConsumerService: TJvDataConsumerAggregatedObject);
  published
    property Location: TJvColorProviderAddItemLocation read FLocation write SetLocation
      default ailBottom;
    property Caption: string read FCaption write SetCaption;
    property Style: TJvColorProviderAddColorStyle read FStyle write SetStyle default aisBorland;
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
    function Get_StandardColorSettings: TJvColorProviderColorGroupSettings;
    function Get_SystemColorSettings: TJvColorProviderColorGroupSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;

    property ColorBoxSettings: TJvColorProviderColorBoxSettings read Get_ColorBoxSettings;
    property CustomColorSettings: TJvColorProviderCustomColorGroupSettings
      read Get_CustomColorSettings;
    property GroupingSettings: TJvColorProviderGroupingSettings read Get_GroupingSettings;
    property StandardColorSettings: TJvColorProviderColorGroupSettings
      read Get_StandardColorSettings;
    property SystemColorSettings: TJvColorProviderColorGroupSettings read Get_SystemColorSettings;
    property TextSettings: TJvColorProviderTextSettings read Get_TextSettings;
  end;

procedure Register;

implementation

uses
  Controls, SysUtils,
  JclRTTI,
  JvConsts;

function GetItemColorValue(Item: IJvDataItem; out Color: TColor): Boolean;
var
  S: string;
begin
  S := Item.GetID;
  Result := Copy(S, 1, 7) = 'TCOLOR=';
  if Result then
    Color := StrToInt('$0' + Copy(S, 8, 8));
end;

type
  TJvColorItems = class(TJvBaseDataItems)
  private
    FProviderComp: TJvColorProvider;
  protected
    function GetColorSettings: IJvColorProviderSettings;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
    procedure InitImplementers; override;

    property ProviderComp: TJvColorProvider read FProviderComp write FProviderComp;
  public
    procedure AfterConstruction; override;
  end;

  TJvColorItem = class(TJvBaseDataItem, IJvDataItemText)
  private
    FListNumber: Integer; // 0 = StdColors, 1 = SysColors, 2 = CstColors
    FListIndex: Integer;  // Index in color list
  protected
    function GetCaption: string;
    procedure SetCaption(const Value: string); 
    procedure InitID; override;
    function List: TColorItems;
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
    procedure InitID; override;
    property ListNumber: Integer read FListNumber;
  public
    constructor Create(AOwner: IJvDataItems; AListNumber: Integer);
  end;

  TJvColorItemText = class(TJvBaseDataItemTextImpl)
  protected
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
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

  TOpenConsumerServiceExt = class(TJvDataConsumerAggregatedObject);

  TJvColorProviderSettings = class(TJvDataConsumerAggregatedObject, IJvColorProviderSettings)
  private
    FColorBoxSettings: TJvColorProviderColorBoxSettings;
    FCustomColorSettings: TJvColorProviderCustomColorGroupSettings;
    FGroupingSettings: TJvColorProviderGroupingSettings;
    FStandardColorSettings: TJvColorProviderColorGroupSettings;
    FSystemColorSettings: TJvColorProviderColorGroupSettings;
    FTextSettings: TJvColorProviderTextSettings;
  protected
    function Get_ColorBoxSettings: TJvColorProviderColorBoxSettings;
    function Get_CustomColorSettings: TJvColorProviderCustomColorGroupSettings;
    function Get_GroupingSettings: TJvColorProviderGroupingSettings;
    function Get_StandardColorSettings: TJvColorProviderColorGroupSettings;
    function Get_SystemColorSettings: TJvColorProviderColorGroupSettings;
    function Get_TextSettings: TJvColorProviderTextSettings;
    procedure Set_ColorBoxSettings(Value: TJvColorProviderColorBoxSettings);
    procedure Set_CustomColorSettings(Value: TJvColorProviderCustomColorGroupSettings);
    procedure Set_GroupingSettings(Value: TJvColorProviderGroupingSettings);
    procedure Set_StandardColorSettings(Value: TJvColorProviderColorGroupSettings);
    procedure Set_SystemColorSettings(Value: TJvColorProviderColorGroupSettings);
    procedure Set_TextSettings(Value: TJvColorProviderTextSettings);
  public
    constructor Create(AOwner: TExtensibleInterfacedPersistent); override;
    destructor Destroy; override;
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
  end;

//===TJvColorProviderSubSettings====================================================================

procedure TJvColorProviderSubSettings.Changed;
begin
  TOpenConsumerServiceExt(ConsumerServiceExt).Changed(ccrOther);
end;

procedure TJvColorProviderSubSettings.ViewChanged;
begin
  TOpenConsumerServiceExt(ConsumerServiceExt).ViewChanged(ConsumerServiceExt);
end;

procedure TJvColorProviderSubSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    FActive := Value;
    Changed;
  end;
end;

constructor TJvColorProviderSubSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create;
  FConsumerServiceExt := AConsumerService;
end;

//===TJvColorProviderColorBoxSettings===============================================================

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

constructor TJvColorProviderColorBoxSettings.Create(
  AConsumerService: TJvDataConsumerAggregatedObject);
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

//===TJvColorProviderTextSettings===================================================================

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

constructor TJvColorProviderTextSettings.Create(AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FShowName := True;
end;

//===TJvColorProviderGroupingSettings===============================================================

procedure TJvColorProviderGroupingSettings.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    inherited SetActive(Value);
    ViewChanged;
  end;
end;

procedure TJvColorProviderGroupingSettings.SetFlatList(Value: Boolean);
begin
  if Value <> FlatList then
  begin
    FFlatList := Value;
    Changed;
    ViewChanged;
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

constructor TJvColorProviderGroupingSettings.Create(
  AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FFlatList := True;
  FHeaderAlign := ghaLeft;
  FHeaderStyle := [ghsBoldFont, ghsSingleCenterLine];
end;

//===TJvColorProviderColorGroupSettings=============================================================

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

constructor TJvColorProviderColorGroupSettings.Create(
  AConsumerService: TJvDataConsumerAggregatedObject; ACaption: string);
begin
  inherited Create(AConsumerService);
  FActive := True;
  FCaption := ACaption;
end;

//===TJvColorProviderAddColorSettings===============================================================

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

procedure TJvColorProviderAddColorSettings.SetStyle(Value: TJvColorProviderAddColorStyle);
begin
  if Value <> Style then
  begin
    FStyle := Value;
    Changed;
  end;
end;

constructor TJvColorProviderAddColorSettings.Create(
  AConsumerService: TJvDataConsumerAggregatedObject);
begin
  inherited Create(AConsumerService);
  FLocation := ailBottom;
  FStyle := aisBorland;
end;

//===TJvColorProviderCustomColorGroupSettings=======================================================

procedure TJvColorProviderCustomColorGroupSettings.SetAddColorSettings(
  Value: TJvColorProviderAddColorSettings);
begin
end;

constructor TJvColorProviderCustomColorGroupSettings.Create(
  AConsumerService: TJvDataConsumerAggregatedObject; ACaption: string);
begin
  inherited Create(AConsumerService, ACaption);
  FAddColorSettings := TJvColorProviderAddColorSettings.Create(AConsumerService);
end;

destructor TJvColorProviderCustomColorGroupSettings.Destroy;
begin
  FreeAndNil(FAddColorSettings);
  inherited Destroy;
end;

//===TJvColorItems==================================================================================

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
  if Settings.StandardColorSettings.Active then
    Inc(Result, Length(ProviderComp.StdColors) +
      Ord(Settings.StandardColorSettings.ShowHeader and Settings.GroupingSettings.Active));
  if Settings.SystemColorSettings.Active then
    Inc(Result, Length(ProviderComp.SysColors) +
      Ord(Settings.SystemColorSettings.ShowHeader and Settings.GroupingSettings.Active));
end;

function TJvColorItems.GetItem(I: Integer): IJvDataItem;
var
  OrgIdx: Integer;
  Settings: IJvColorProviderSettings;
  ListNum: Integer;
begin
  if I < 0 then
    TList.Error('Index', I);
  OrgIdx := I;
  Settings := GetColorSettings;
  if Settings = nil then
    Exit;
  ListNum := -1;
  if Settings.StandardColorSettings.Active then
  begin
    if Settings.StandardColorSettings.ShowHeader and Settings.GroupingSettings.Active then
      Dec(I);
    if I < Length(ProviderComp.StdColors) then
      ListNum := 0
    else
      Dec(I, Length(ProviderComp.StdColors));
  end;
  if (ListNum < 0) and Settings.SystemColorSettings.Active then
  begin
    if Settings.SystemColorSettings.ShowHeader and Settings.GroupingSettings.Active then
      Dec(I);
    if I < Length(ProviderComp.SysColors) then
      ListNum := 1
    else
      Dec(I, Length(ProviderComp.SysColors));
  end;
  if ListNum < 0 then
    TList.Error('Index', OrgIdx);
  if I < 0 then
    Result := TJvColorHeaderItem.Create(Self, ListNum)
  else
    Result := TJvColorItem.Create(Self, ListNum, I);
end;

procedure TJvColorItems.InitImplementers;
begin
  inherited InitImplementers;
  TJvColorItemsRenderer.Create(Self);
end;

procedure TJvColorItems.AfterConstruction;
var
  ICR: IInterfaceComponentReference;
begin
  inherited AfterConstruction;
  if Supports(GetProvider, IInterfaceComponentReference, ICR) then
    FProviderComp := ICR.GetComponent as TJvColorProvider;
end;

//===TJvColorItem===================================================================================

function TJvColorItem.GetCaption: string;
begin
  Result := ID;
end;

procedure TJvColorItem.SetCaption(const Value: string);
begin
end;

procedure TJvColorItem.InitID;
begin
  SetID('TCOLOR=' + IntToHex(List[ListIndex].Value, 8));
end;

function TJvColorItem.List: TColorItems;
begin
  with (GetItems.GetImplementer as TJvColorItems).ProviderComp do
  begin
    case ListNumber of
      0:
        Result := StdColors;
      1:
        Result := SysColors;
      2:
        Result := CstColors;
      else
        Result := nil;
    end;
  end;
end;

constructor TJvColorItem.Create(AOwner: IJvDataItems; AListNumber, AListIndex: Integer);
begin
  inherited Create(AOwner);
  FListNumber := AListNumber;
  FListIndex := AListIndex;
end;

//===TJvColorHeaderItem=============================================================================

function TJvColorHeaderItem.GetCaption: string;
var
  Settings: IJvColorProviderSettings;
begin
  Supports(GetItems.GetProvider.SelectedConsumer, IJvColorProviderSettings, Settings);
  if Settings = nil then
    Result := '(no settings)'
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

procedure TJvColorHeaderItem.SetCaption(const Value: string);
begin
end;

procedure TJvColorHeaderItem.InitID;
begin
  SetID('ColorGroupHeader_' + IntToStr(ListNumber));
end;

constructor TJvColorHeaderItem.Create(AOwner: IJvDataItems; AListNumber: Integer);
begin
  inherited Create(AOwner);
  FListNumber := AListNumber;
end;

//===TJvColorItemText===============================================================================

function TJvColorItemText.GetCaption: string;
(*var
  ColorValue: TColor;
  ItemsImpl: TJvColorItems;
  ColorIdx: Integer;*)
begin
(*  if GetItemColorValue(Item, ColorValue) then
  begin
    ItemsImpl := TJvColorItems(Item.GetItems.GetImplementer);
    ColorIdx := ItemsImpl.FColors.IndexOfObject(TObject(ColorValue));
    if ColorIdx >= 0 then
      Result := ItemsImpl.FColors[ColorIdx]
    else
      Result := '$' + IntToHex(ColorValue, 8);
  end
  else
    Result := 'Invalid ID:' + Item.GetID;*)
end;

procedure TJvColorItemText.SetCaption(const Value: string);
begin
end;

//===TJvColorProvider===============================================================================

procedure TJvColorProvider.AddColorStr(const S: string);
var
  Col: TColor;
begin
  if IdentToColor(S, Integer(Col)) then
  begin
    if (Col >= 0) and (Col < clNone) then
      AddColor(FStdColors, Col)
    else
      AddColor(FSysColors, Col);
  end;
end;

procedure TJvColorProvider.AddColor(var List: TColorItems; Color: TColor);
begin
  SetLength(List, Length(List) + 1);
  List[High(List)].Value := Color;
end;

procedure TJvColorProvider.DeleteColor(var List: TColorItems; Index: Integer);
begin
  if (Index < High(List)) then
  begin
    Move(List[Index + 1], List[Index], SizeOf(List[0]) * (High(List) - Index));
    FillChar(List[High(List)], 0, SizeOf(List[0]));
  end;
  SetLength(List, High(List));
end;

class function TJvColorProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TJvColorItems;
end;

function TJvColorProvider.ConsumerClasses: TClassArray;
begin
  Result := inherited ConsumerClasses;
  AddToArray(Result, TJvColorProviderSettings);
end;

constructor TJvColorProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetColorValues(AddColorStr);
end;

destructor TJvColorProvider.Destroy;
begin
//  TJvColorItems(DataItemsImpl).ProviderComp := nil;
  inherited Destroy;
end;

//===TJvColorItemsRenderer==========================================================================

function TJvColorItemsRenderer.GetRenderText: string;
var
  ItemText: IJvDataItemText;
begin
  with CurrentSettings.TextSettings do
  begin
    if Active then
    begin
      if ShowName and Supports(CurrentItem, IJvDataItemText, ItemText) then
        Result := ItemText.Caption
      else
        Result := '';
      if CurrentItemIsColorItem then
      begin
        if ShowHex then
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
        Result := SDataItemRenderHasNoText;
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
  if CurrentSettings.TextSettings.Active then
  begin
    S := GetRenderText;
    R := CurrentRect;
    OldBkMode := SetBkMode(CurrentCanvas.Handle, TRANSPARENT);
    try
      DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_END_ELLIPSIS or
        DT_VCENTER or DT_NOPREFIX);
    finally
      SetBkMode(CurrentCanvas.Handle, OldBkMode);
    end;
  end;
end;

procedure TJvColorItemsRenderer.RenderGroupHeader;
var
  S: string;
  OldFont: TFont;
  R: TRect;
  RWidth: Integer;
  RVCenter: Integer;
  OldBkMode: Integer;
begin
  S := GetRenderText;
  OldFont := TFont.Create;
  try
    OldFont.Assign(CurrentCanvas.Font);
    try
      if ghsBoldFont in CurrentSettings.GroupingSettings.HeaderStyle then
        CurrentCanvas.Font.Style := CurrentCanvas.Font.Style + [fsBold];
      R := CurrentRect;
      Dec(R.Right, 2);
      case CurrentSettings.GroupingSettings.HeaderAlign of
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
    DrawText(CurrentCanvas.Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_NOPREFIX or
      DT_CALCRECT);
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

type
  TOpenControl = class(TControl);

function TJvColorItemsRenderer.AvgItemSize(ACanvas: TCanvas): TSize;
var
  Comp: TComponent;
  ChWdth: Integer;
begin
  CurrentSettings := GetConsumerSettings;
  try
    Result.cx := 0;
    Result.cy := 0;
    MeasureColorBox(Result);
    if CurrentSettings.TextSettings.Active then
    begin
      Comp := Items.GetProvider.SelectedConsumer.VCLComponent;
      if (Comp <> nil) and (Comp is TControl) then
      begin
        with TOpenControl(Comp) do
        begin
          if (Abs(Font.Height) + 2) > Result.cy then
            Result.cy := Abs(Font.Height) + 2;
          ChWdth := Abs(Font.Height) div 3;
        end;
      end
      else
      begin
        if Result.cy < 15 then
          Result.cy := 15;
        ChWdth := 4;
      end;
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

//===TJvColorProviderSettings=======================================================================

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

constructor TJvColorProviderSettings.Create(AOwner: TExtensibleInterfacedPersistent);
begin
  inherited Create(AOwner);
  FColorBoxSettings := TJvColorProviderColorBoxSettings.Create(Self);
  FCustomColorSettings := TJvColorProviderCustomColorGroupSettings.Create(Self, 'Custom colors');
  FGroupingSettings := TJvColorProviderGroupingSettings.Create(Self);
  FTextSettings := TJvColorProviderTextSettings.Create(Self);
  FStandardColorSettings := TJvColorProviderColorGroupSettings.Create(Self, 'Standard colors');
  FSystemColorSettings := TJvColorProviderColorGroupSettings.Create(Self, 'System colors');
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

procedure Register;
begin
  RegisterComponents('Jv DataProviders', [TJvColorProvider]);
end;

initialization
  RegisterClasses([TJvColorProviderSettings]);
end.
