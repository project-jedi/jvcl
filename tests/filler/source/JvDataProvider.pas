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
  TDataProviderChangeReason = (pcrAdd, pcrDelete, pcrUpdate, pcrDestroy);

(*
  TJvFillerSupport = (fsText,       // supports IFillerItemText
                      fsImages,     // supports IFillerItemImages
                      fsImageIndex, // supports IFillerItemImage
                      fsReadOnly,   // does *not* support IFillerItemManagment
                      fsCanRender,  // can render it's content to a DC
                      fsCanMeasure, // can measure the size of it's content
                      fsSubItems    // supports IFillerSubItems
                      );
  TJvFillerSupports = set of TJvFillerSupport;
  Removed; use Supports(xxx, <interface>, <ref>) to check if an item, or item list supports certain
  features:
    fsText        -> IJvDataItemText
    fsImages      -> IJvDataItemsImages
    fsImageIndex  -> IJvDataItemImage
    fsReadOnly    -> IJvDataItemsManagement
    fsCanRender   -> IJvDataItemsRenderer or IJvDataItemRenderer
    fsCanMeasure  -> IJvDataItemsRenderer or IJvDataItemRenderer
    fsSubItems    -> IJvDataItems (if IJvDataItem supports IJvDataItems it has sub items)
*)

(*  TJvFillerItemsAttribute = (fiaDynamicItems);
  TJvFillerItemsAttributes = set of TJvFillerItemsAttribute;
  Use IJvDataItems.IsDynamic method instead.
*)

  // forward
  IJvDataProvider = interface;
  IJvDataItems = interface;
  IJvDataItem = interface;
  IJvDataProviderNotify = interface;

(*  TJvFillerOptions = class;
  TJvFillerOptionsClass = class of TJvFillerOptions;
  Will be done in another way
*)

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
    { Notify clients a changes has just occured. }
    procedure Changed(ChangeReason: TDataProviderChangeReason; Source: IUnknown = nil);
  end;

  { Implemented by clients (i.e list/comboboxes, labels, buttons, edits, listviews, treeviews, menus etc)
   to get notifications from IFiller }
  IJvDataProviderNotify = interface
  ['{5B9D1847-6D35-4D9C-8BC2-2054997AB120}']
    { Called when a change is about to occur at the provider. }
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    { Called when a change has occured at the provider. }
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
    { Retrieve the image list to use. }
    function GetImageList: TCustomImageList;
    { Specify the image list to use. }
    procedure SetImageList(const Value: TCustomImageList);
    
    { Get or set the image list to use. }
    property ImageList: TCustomImageList read GetImageList write SetImageList;
  end;

  { Rendering interface. Provides support for both rendering and measuring of items.
    Implemented by IFillerItems. }
  IJvDataItemsRenderer = interface
    ['{4EA490F4-7CCF-44A1-AA26-5320CDE9FAFC}']
    { Draw an item in the IJvDataItems list, using an index to specify which item. }
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer; State: TOwnerDrawState);
    { Measure an item in the IJvDataItems list, using an index to specify which item. }
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer): TSize;
    { Draw the specified item. If the item is not part of the IJvDataItems list or nil is specified, an exception will be raised. }
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IJvDataItem; State: TOwnerDrawState);
    { Measure the specified item. If the item is not part of the IJvDataItems list or nil is specified, an exception will be raised. }
    function MeasureItem(ACanvas: TCanvas; Item: IJvDataItem): TSize;
    { Retrieve the average size of the items in the list. }
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
    function FindByID(ID: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataTextSearch = interface
  ['{E3BC388D-50F6-402D-9E30-36D5F7F40616}']
    function FindByText(Text: string; const Recursive: Boolean = False): IJvDataItem;
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
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TOwnerDrawState);
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

(*  { base class for options that are dynamically added to the client by the server implementation,
    see JvFillBasicImpl for details }
  TJvFillerOptions = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
  protected
    procedure Changed;
  public
    constructor Create(AOnChanged: TNotifyEvent); virtual;
  end;
*)
  { An instance of this class is created when an item is selected in the ProviderEditor. The class
    provides a reference to the item selected. Based on the interfaces supported by the item,
    published properties are "injected" into this class. }
  TJvDataProviderItem = class(TPersistent)
  private
    FItem: IJvDataItem;
  protected
    function Item: IJvDataItem;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AnItem: IJvDataItem);
    function GetNamePath: string; override;
  end;

  TJvDataProviderItemClass = class of TJvDataProviderItem;

procedure RegisterDataItemIntfProp(const IID: TGUID; const PropClass: TJvDataProviderItemClass);

implementation

uses
  SysUtils, TypInfo;

type
  PPropData = ^TPropData;

  TIntfItem = record
    GUID: TGUID;
    PropClass: TJvDataProviderItemClass;
  end;
  TIntfItems = array of TIntfItem;

var
  GIntfPropReg: TIntfItems;

function LocateReg(IID: TGUID): Integer;
begin
  Result := High(GIntfPropReg);
  while (Result >= 0) and not CompareMem(@GIntfPropReg[Result].GUID, @IID, SizeOf(TGUID)) do
    Dec(Result);
end;

procedure RegisterDataItemIntfProp(const IID: TGUID; const PropClass: TJvDataProviderItemClass);
var
  IIDIdx: Integer;
begin
  IIDIdx := LocateReg(IID);
  if IIDIdx < 0 then
  begin
    IIDIdx := Length(GIntfPropReg);
    SetLength(GIntfPropReg, IIDIdx + 1);
    GIntfPropReg[IIDIdx].GUID := IID;
  end;
  GIntfPropReg[IIDIdx].PropClass := PropClass;
end;

function StringBaseLen(NumItems: Integer; StartString: PChar): Integer;
begin
  Result := 0;
  while (NumItems > 0) do
  begin
    Inc(Result, 1 + PByte(StartString)^);
    Inc(StartString, 1 + PByte(StartString)^);
    Dec(NumItems);
  end;
end;

function PropListSize(ListPos: PChar): Integer;
var
  Cnt: Integer;
  BaseInfoSize: Integer;
begin
  Result := SizeOf(Word);
  Cnt := PWord(ListPos)^;
  Inc(ListPos, Result);
  BaseInfoSize := SizeOf(TPropInfo) - SizeOf(ShortString) + 1;
  while Cnt > 0 do
  begin
    Inc(Result, BaseInfoSize + Length(PPropInfo(ListPos)^.Name));
    Inc(ListPos, BaseInfoSize + Length(PPropInfo(ListPos)^.Name));
    Dec(Cnt);
  end;
end;

function TypeInfoSize(TypeInfo: PTypeInfo): Integer;
var
  TypeData: PTypeData;
begin
  Result := 2 + Length(TypeInfo.Name);
  TypeData := GetTypeData(TypeInfo);
  case TypeInfo.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Inc(Result, SizeOf(TOrdType));
        case TypeInfo.Kind of
          tkInteger, tkChar, tkEnumeration, tkWChar:
            begin
              Inc(Result, 8);
              if TypeInfo.Kind = tkEnumeration then
                Inc(Result, 4 + StringBaseLen(TypeData.MaxValue - TypeData.MinValue + 1, @TypeData.NameList));
            end;
          tkSet:
            Inc(Result, 4);
        end;
      end;
    tkFloat:
      Inc(Result, SizeOf(TFloatType));
    tkString:
      Inc(Result);
    tkClass:
      begin
        Inc(Result, SizeOf(TClass) + SizeOf(PPTypeInfo) + SizeOf(SmallInt) + StringBaseLen(1, @TypeData.UnitName));
        Inc(Result, PropListSize(Pointer(Integer(@TypeData.UnitName) + StringBaseLen(1, @TypeData.UnitName))));
      end;
  end;
end;

function CloneTypeInfo(OrgTypeInfo: PTypeInfo; AdditionalSpace: Longint = 0): PTypeInfo;
var
  P: PChar;
begin
  P := AllocMem(SizeOf(Pointer) + TypeInfoSize(OrgTypeInfo) + AdditionalSpace);
  PInteger(P)^ := Integer(OrgTypeInfo);
  Inc(P, 4);
  Result := PTypeInfo(P);
  Move(OrgTypeInfo^ , Result^, TypeInfoSize(OrgTypeInfo));
end;

procedure CreateTypeInfo(const AClass: TClass);
var
  P: PChar;
  PNewInfo: Pointer;
  OldProtect: Cardinal;
begin
  P := Pointer(AClass);
  Dec(P, 60);                         // Now pointing to TypeInfo of the VMT table.
  { Below the typeinfo is cloned, while an additional 2048 bytes are reserved at the end. This 2048
    bytes will be used to "inject" additional properties. Since each property takes 27 + the length
    of the property name bytes, assuming an average of 40 bytes/property will allow approximately 50
    properties to be appended to the existing property list. }
  PNewInfo := CloneTypeInfo(Pointer(PInteger(P)^), 2048);
  if VirtualProtect(P, 4, PAGE_WRITECOPY, OldProtect) then
  try
    PInteger(P)^ := Integer(PNewInfo);
  finally
    VirtualProtect(P, 4, OldProtect, OldProtect);
  end;
end;

procedure ClearTypeInfo(const AClass: TClass);
var
  P: PChar;
  PNewType: PChar;
  OldProtect: Cardinal;
begin
  P := Pointer(AClass);
  Dec(P, 60);                         // Now pointing to TypeInfo of the VMT table.
  PNewType := Pointer(PInteger(P)^);  // The new type currently in use.
  Dec(PNewType, 4);                   // Points to the original PTypeInfo value.
  if VirtualProtect(P, 4, PAGE_WRITECOPY, OldProtect) then
  try
    PInteger(P)^ := Integer(PInteger(PNewType)^);
  finally
    VirtualProtect(P, 4, OldProtect, OldProtect);
  end;
end;

function GetPropData(TypeData: PTypeData): PPropData;
begin
  Result := PPropData(Integer(@TypeData.UnitName) + StringBaseLen(1, @TypeData.UnitName));
end;

procedure ClearPropList(const AClass: TClass);
var
  RTTI: PTypeInfo;
  TypeData: PTypeData;
  PropList: PPropData;
begin
  RTTI := PTypeInfo(AClass.ClassInfo);
  TypeData := GetTypeData(RTTI);
  TypeData.PropCount := 0;
  PropList := GetPropData(TypeData);
  PropList.PropCount := 0;
end;

procedure CopyPropInfo(var Source, Dest: PPropInfo; var PropNum: Smallint);
var
  BaseInfoSize: Integer;
  NameLen: Integer;
begin
  BaseInfoSize := SizeOf(TPropInfo) - SizeOf(ShortString) + 1;
  NameLen := Length(Source.Name);
  Move(Source^, Dest^, BaseInfoSize + NameLen);
  Dest.NameIndex := PropNum;
  Inc(PChar(Source), BaseInfoSize + NameLen);
  Inc(PChar(Dest), BaseInfoSize + NameLen);
  Inc(PropNum);
end;

procedure AppendPropList(const AClass: TClass; PropList: PPropInfo; Count: Integer);
var
  RTTI: PTypeInfo;
  TypeData: PTypeData;
  ClassPropList: PPropInfo;
  ExistingCount: Integer;
  BaseInfoSize: Integer;
  PropNum: Smallint;
begin
  RTTI := PTypeInfo(AClass.ClassInfo);
  TypeData := GetTypeData(RTTI);
  TypeData.PropCount := TypeData.PropCount + Count;
  ClassPropList := PPropInfo(GetPropData(TypeData));
  ExistingCount := PPropData(ClassPropList).PropCount;
  PropNum := ExistingCount;
  PPropData(ClassPropList).PropCount := ExistingCount + Count;
  Inc(PChar(ClassPropList), 2);
  BaseInfoSize := SizeOf(TPropInfo) - SizeOf(ShortString) + 1;
  while ExistingCount > 0 do
  begin
    Inc(PChar(ClassPropList), BaseInfoSize + Length(ClassPropList.Name));
    Dec(ExistingCount);
  end;
  while Count > 0 do
  begin
    CopyPropInfo(PropList, ClassPropList, PropNum);
    Dec(Count);
  end;
end;

(*
{ TJvFillerOptions }

procedure TJvFillerOptions.Changed;
begin
  if @FOnChanged <> nil then
    FOnChanged(Self);
end;

constructor TJvFillerOptions.Create(AOnChanged: TNotifyEvent);
begin
  inherited Create;
  FOnChanged := AOnChanged;
end;
*)

{ TJvDataProviderItem }

function TJvDataProviderItem.Item: IJvDataItem;
begin
  Result := FItem;
end;

function TJvDataProviderItem.GetOwner: TPersistent;
begin
  if Item <> nil then
    Result := (Item.Items.Provider as IInterfaceComponentReference).GetComponent
  else
    Result := inherited GetOwner;
end;

constructor TJvDataProviderItem.Create(AnItem: IJvDataItem);
var
  I: Integer;
  IUnk: IUnknown;
  PrpData: PPropData;
begin
  inherited Create;
  FItem := AnItem;
  ClearPropList(ClassType);
  for I := High(GIntfPropReg) downto 0 do
  begin
    if Supports(AnItem, GIntfPropReg[I].GUID, IUnk) then
    begin
      PrpData := GetPropData(GetTypeData(GIntfPropReg[I].PropClass.ClassInfo));
      AppendPropList(ClassType, PPropInfo(Cardinal(PrpData) + 2), PrpData.PropCount);
    end;
  end;
end;

function TJvDataProviderItem.GetNamePath: string;
var
  Comp: TPersistent;
begin
  Comp := GetOwner;
  if (Comp <> nil) and (Comp is TComponent) then
    Result := (Comp as TComponent).Name
  else
    Result := '<unknown>';
  if Item <> nil then
    Result := Result + ': Item[' + Item.GetID + ']'
  else
    Result := Result + ': <no item>';
end;

(* Move to implementers? *)

type
  TJvDataItemTextPropView = class(TJvDataProviderItem)
  protected
    function GetCaption: string;
    procedure SetCaption(Value: string);
  published
    property Caption: string read GetCaption write SetCaption;
  end;

function TJvDataItemTextPropView.GetCaption: string;
begin
  Result := (Item as IJvDataItemText).Caption;
end;

procedure TJvDataItemTextPropView.SetCaption(Value: string);
begin
  (Item as IJvDataItemText).Caption := Value;
end;

type
  TJvDataItemImagePropView = class(TJvDataProviderItem)
  protected
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetImageIndex: Integer;
    procedure SetImageIndex(Value: Integer);
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(Value: Integer);
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

function TJvDataItemImagePropView.GetAlignment: TAlignment;
begin
  Result := (Item as IJvDataItemImage).Alignment;
end;

procedure TJvDataItemImagePropView.SetAlignment(Value: TAlignment);
begin
  (Item as IJvDataItemImage).Alignment := Value;
end;

function TJvDataItemImagePropView.GetImageIndex: Integer;
begin
  Result := (Item as IJvDataItemImage).ImageIndex
end;

procedure TJvDataItemImagePropView.SetImageIndex(Value: Integer);
begin
  (Item as IJvDataItemImage).ImageIndex := Value;
end;

function TJvDataItemImagePropView.GetSelectedIndex: Integer;
begin
  Result := (Item as IJvDataItemImage).SelectedIndex;
end;

procedure TJvDataItemImagePropView.SetSelectedIndex(Value: Integer);
begin
  (Item as IJvDataItemImage).SelectedIndex := Value;
end;

procedure RegProviderItemInterfaces;
begin
  RegisterDataItemIntfProp(IJvDataItemText, TJvDataItemTextPropView);
  RegisterDataItemIntfProp(IJvDataItemImage, TJvDataItemImagePropView);
end;

initialization
  CreateTypeInfo(TJvDataProviderItem);
  RegProviderItemInterfaces;

finalization
  ClearTypeInfo(TJvDataProviderItem);
end.


