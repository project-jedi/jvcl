{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvFillIntf.Pas, released on --.

The Initial Developers of the Original Code are Marcel Bestebroer, Peter
Thörnqvist and Remko Bonte
Portions created by the individuals are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-04-23

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvFillIntf;

interface

uses
  Windows, ImgList, Classes, Graphics;

type
  TJvFillerChangeReason = (frAdd,     // an item is added
                           frDelete,  // an item is removed
                           frUpdate,  // the entire list is updated
                           frDestroy //  the IFiller implementor is being destroyed, clients should call UnRegisterChangeNotify and remove the reference to Filler
                           );

  TJvFillerSupport = (fsText,       // supports IFillerItemText
                      fsImages,     // supports IFillerItemImages
                      fsImageIndex, // supports IFillerItemImage
                      fsReadOnly,   // does *not* support IFillerItemManagment
                      fsCanRender,  // can render it's content to a DC
                      fsCanMeasure, // can measure the size of it's content
                      fsSubItems    // supports IFillerSubItems
                      );
  TJvFillerSupports = set of TJvFillerSupport;

  TJvFillerItemsAttribute = (fiaDynamicItems);
  TJvFillerItemsAttributes = set of TJvFillerItemsAttribute;

  // forward
  IFiller = interface;
  IFillerItems = interface;
  IFillerItem = interface;
  IFillerNotify = interface;

  TJvFillerOptions = class;
  TJvFillerOptionsClass = class of TJvFillerOptions;

  { base interface for components that supports storing lists of data (0..M items) }
  IFiller = interface
  ['{62A7A17D-1E21-427E-861D-C92FBB9B09A6}']
    procedure RegisterChangeNotify(AFillerNotify: IFillerNotify);
    procedure UnregisterChangeNotify(AFillerNotify: IFillerNotify);
    function GetSupports: TJvFillerSupports;
    function GetOptionClass: TJvFillerOptionsClass;
    function GetItems: IFillerItems;
    procedure Changing(ChangeReason: TJvFillerChangeReason);
    procedure Changed(ChangeReason: TJvFillerChangeReason);
  end;

  { Implemented by clients (i.e list/comboboxes, labels, buttons, edits, listviews, treeviews, menus etc)
   to get notifications from IFiller }
  IFillerNotify = interface
  ['{5B9D1847-6D35-4D9C-8BC2-2054997AB120}']
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
  end;

  { Item list. (0..N items)
    Implemented by IFiller implementors
    Supported by IFillerItem implementers only when fsSubItems is in IFiller.FillerSupports. }
  IFillerItems = interface
  ['{93747660-24FB-4294-BF4E-C7F88EA23983}']
    function GetCount: Integer;
    function GetItem(Index: Integer): IFillerItem;
    function GetParent: IFillerItem; // returns nil for the IFiller implementation
    function GetFiller: IFiller;
    function Attributes: TJvFillerItemsAttributes;
    function GetImplementer: TObject;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IFillerItem read GetItem;
    property Parent: IFillerItem read GetParent;
    property Filler: IFiller read GetFiller;
  end;

  { only supported when fsImages is in  IFiller.FillerSupports; supported by IFillerItems implementers. }
  IFillerItemsImages = interface
  ['{735755A6-AD11-460C-B985-46464D73EDBC}']
    function GetImageList: TCustomImageList;
    procedure SetImageList(const Value: TCustomImageList);
    property ImageList: TCustomImageList read GetImageList write SetImageList;
  end;

  { Rendering interface. Provides support for both rendering and measuring of items.
    Implemented by IFillerItems. }
  IFillerItemsRenderer = interface
    ['{4EA490F4-7CCF-44A1-AA26-5320CDE9FAFC}']
    procedure DrawItemByIndex(ACanvas: TCanvas; var ARect: TRect; Index: Integer;
      State: TOwnerDrawState; AOptions: TPersistent = nil);
    function MeasureItemByIndex(ACanvas: TCanvas; Index: Integer;
      AOptions: TPersistent = nil): TSize;
    procedure DrawItem(ACanvas: TCanvas; var ARect: TRect; Item: IFillerItem;
      State: TOwnerDrawState; AOptions: TPersistent = nil);
    function MeasureItem(ACanvas: TCanvas; Item: IFillerItem; AOptions: TPersistent = nil): TSize;
    function AvgItemSize(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize;
  end;

  { Implemented by servers that allows editing the list. Supported by IFillerItems implementers. }
  IFillerItemsManagment = interface
  ['{76611CC0-9DCD-4394-8B6E-1ADEF1942BC3}']
    function Add(Item: IFillerItem): IFillerItem;
    function New: IFillerItem;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Remove(Item: IFillerItem);
  end;

  { Support interface for filler editor. May be implemented by IFillerItemManagment implementers who
    allow their list/tree to be edited. }
  IFillerItemsDesigner = interface
    ['{31B2544C-8E4F-40FE-94B8-04243EF40821}']
    function GetCount: Integer;
    function GetKind(Index: Integer; out Caption: string): Boolean;
    function NewByKind(Kind: Integer): IFillerItem;
  end;

  { base search interface. Can be supported by IFillerItems implementers if the
    implementation needs it.

    The basic idea is to declare additional interfaces to implement searching on other properties
    as well. eg. for the color filler:

      IFillerColorSearch = interface
        function IndexOfTColor(Color: TColor; const Recursive: Boolean = False): Integer;
      end;

    IFillerItems implement those search interface that apply for the implementation.
    The recursive parameter has a default parameter value so it could be left out. }

  IFillerIDSearch = interface
    ['{0F5BDC79-893B-45C9-94E9-C2B2FD4ABFE7}']
    function FindByID(ID: string; const Recursive: Boolean = False): IFillerItem;
  end;

  IFillerTextSearch = interface
  ['{E3BC388D-50F6-402D-9E30-36D5F7F40616}']
    function FindByText(Text: string; const Recursive: Boolean = False): IFillerItem;
  end;

  { base item interface: holds reference to the IFillerItems owner as well as provide a reference to the implementer }
  IFillerItem = interface
  ['{C965CF64-A1F2-44A4-B856-3A4EC6B693E1}']
    function GetItems: IFillerItems;
    function GetImplementer: TObject;
    function GetID: string;

    property Items: IFillerItems read GetItems;
    property Implementer: TObject read GetImplementer;
  end;

  { Rendering interface for an item. Provides support for both rendering and measuring of the item.
    Implemneted by IFillerItem. }
  IFillerItemRenderer = interface
    ['{9E877A0D-01C2-4204-AA74-84D6516BBEB9}']
    procedure Draw(ACanvas: TCanvas; var ARect: TRect; State: TOwnerDrawState;
      AOptions: TPersistent = nil);
    function Measure(ACanvas: TCanvas; AOptions: TPersistent = nil): TSize;
  end;

  { only supported when fsText is in IFiller.FillerSupports; supported by the IFillerItem implementer }
  IFillerItemText = interface
  ['{94FA56D9-281B-4252-B46D-15E7BADA70DA}']
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    property Caption: string read GetCaption write SetCaption;
  end;

  { only supported when fsImageIndex is in IFiller.FillerSupports;
    supported by the IFillerItem implementer.
    Note that the IFiller probably need to implement the
    IFillerItemImages interface as well }
  IFillerItemImage = interface
  ['{6425D73A-90CF-42ED-9AB2-63125A4C0774}']
    function GetAlignment: TAlignment;
    procedure SetAlignment(Value: TAlignment);
    function GetImageIndex: integer;
    procedure SetImageIndex(Index: integer);
    function GetSelectedIndex: integer;
    procedure SetSelectedIndex(Value: integer);

    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

  { implemented by servers that supports a default action for each item }
  IFillerItemBasicAction = interface
  ['{86859A20-560D-4E9A-AC8B-2457789451B0}']
    function Execute(Sender: TObject): Boolean;
  end;

  { Support interface for filler editor. Must be implemented by IFillerItem implementers who
    allow their item to be edited in the filler editor. }
  IFillerItemDesigner = interface
    ['{8F1A1283-2D13-4A28-9616-08B3EF73F29A}']
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

  { base class for options that are dynamically added to the client by the server implementation,
    see JvFillBasicImpl for details }
  TJvFillerOptions = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
  protected
    procedure Changed;
  public
    constructor Create(AOnChanged: TNotifyEvent); virtual;
  end;

  { An instance of this class is created when an item is selected in the FillerEditor. The class
    provides a reference to the item selected. Based on the interfaces supported by the item,
    published properties are "injected" into this class. }
  TJvFillerItem = class(TPersistent)
  private
    FItem: IFillerItem;
  protected
    function Item: IFillerItem;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AnItem: IFillerItem);
    function GetNamePath: string; override;
  end;

  TJvFillerItemClass = class of TJvFillerItem;

procedure RegisterFillerIntfProp(const IID: TGUID; const PropClass: TJvFillerItemClass);

implementation

uses
  SysUtils, TypInfo;

type
  PPropData = ^TPropData;

  TIntfItem = record
    GUID: TGUID;
    PropClass: TJvFillerItemClass;
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

procedure RegisterFillerIntfProp(const IID: TGUID; const PropClass: TJvFillerItemClass);
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

{ TJvFillerItem }

function TJvFillerItem.Item: IFillerItem;
begin
  Result := FItem;
end;

function TJvFillerItem.GetOwner: TPersistent;
begin
  if Item <> nil then
    Result := (Item.Items.Filler as IInterfaceComponentReference).GetComponent
  else
    Result := inherited GetOwner;
end;

constructor TJvFillerItem.Create(AnItem: IFillerItem);
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

function TJvFillerItem.GetNamePath: string;
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

type
  TJvFillerItemTextPropView = class(TJvFillerItem)
  protected
    function GetCaption: string;
    procedure SetCaption(Value: string);
  published
    property Caption: string read GetCaption write SetCaption;
  end;

function TJvFillerItemTextPropView.GetCaption: string;
begin
  Result := (Item as IFillerItemText).Caption;
end;

procedure TJvFillerItemTextPropView.SetCaption(Value: string);
begin
  (Item as IFillerItemText).Caption := Value;
end;

type
  TJvFillerItemImagePropView = class(TJvFillerItem)
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

function TJvFillerItemImagePropView.GetAlignment: TAlignment;
begin
  Result := (Item as IFillerItemImage).Alignment;
end;

procedure TJvFillerItemImagePropView.SetAlignment(Value: TAlignment);
begin
  (Item as IFillerItemImage).Alignment := Value;
end;

function TJvFillerItemImagePropView.GetImageIndex: Integer;
begin
  Result := (Item as IFillerItemImage).ImageIndex
end;

procedure TJvFillerItemImagePropView.SetImageIndex(Value: Integer);
begin
  (Item as IFillerItemImage).ImageIndex := Value;
end;

function TJvFillerItemImagePropView.GetSelectedIndex: Integer;
begin
  Result := (Item as IFillerItemImage).SelectedIndex;
end;

procedure TJvFillerItemImagePropView.SetSelectedIndex(Value: Integer);
begin
  (Item as IFillerItemImage).SelectedIndex := Value;
end;

procedure RegFillerItemInterfaces;
begin
  RegisterFillerIntfProp(IFillerItemText, TJvFillerItemTextPropView);
  RegisterFillerIntfProp(IFillerItemImage, TJvFillerItemImagePropView);
end;

initialization
  CreateTypeInfo(TJvFillerItem);
  RegFillerItemInterfaces;

finalization
  ClearTypeInfo(TJvFillerItem);
end.


