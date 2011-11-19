{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderItemDesign.pas, released on 2003-06-27.

The Initial Developers of the Original Code is Marcel Bestebroer.
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDataProviderItemDesign;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, TypInfo, 
  JvDataProvider, JvDataProviderIntf;

type
  { Default item designer for IJvDataItem and its supporting interfaces. This designer is able to handle any IJvDataItem
    implementation and injects properties based on the interfaces it supports. This is accomplished by descendants of
    this class (called Views in this context), which can not contain instance fields and who's published properties are
    read and injected into the main designer class. Each descendant should be linked to its accompanying interface using
    the RegisterDataItemIntfProp routine. Note that for the standard interfaces views have already been registered.

    The downside is that you can only edit properties for registered interfaces. In some situations an interface could
    be setup in multiple ways, ie. using a reference to a TAction or using a simple event, and the system is not able
    to distinguish these two options (unless you provide a separate interface for each possibility and register a view
    for it. See TBaseItemDsgn for the class that is used when the implementer of IJvDataItem is based on the
    TExtensibleInterfacedPersistent class, providing a more flexible approach. }
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

  { Item designer based on implementations with TExtenisbleInterfacedPersistent as their base. Since these
    implementations have access to the various implementers more directly, the property injection is handled slightly
    different.

    Instead of creating a single instance and linking it to the IJvDataItem implementation, we simply inject the
    published properties from the implementers, and revert it when the item is deselected. The required amount of memory
    is pre-calculated, too, to avoid possible access violations in case there are a lot of implementers and/or
    properties to inject.

    The downside is a decrease in performance, though the difference isn't much of an issue when in designing mode (who
    really notices or cares about the difference between waiting 10ms or 100ms after selecting an item and seeing its
    properties appear in the Object Inspector?).
  }
  TBaseItemDsgn = class (TExtensibleInterfacedPersistent)
  private
    function GetIsStoredProp(Index: Integer): Boolean;
    {$IFDEF COMPILER10_UP}
    function GetDynArrayProp(Index: Integer): Pointer;
    procedure SetDynArrayProp(Index: Integer; Value: Pointer);
    {$ENDIF COMPILER10_UP}
    function GetFloatProp(Index: Integer): Extended;
    procedure SetFloatProp(Index: Integer; Value: Extended);
    function GetInt64Prop(Index: Integer): Int64;
    procedure SetInt64Prop(Index: Integer; Value: Int64);
    function GetInterfaceProp(Index: Integer): IInterface;
    procedure SetInterfaceProp(Index: Integer; Value: IInterface);
    function GetMethodProp(Index: Integer): TMethod;
    procedure SetMethodProp(Index: Integer; Value: TMethod);
    function GetOrdProp(Index: Integer): Longint;
    procedure SetOrdProp(Index: Integer; Value: Longint);
    function GetStrProp(Index: Integer): string;
    procedure SetStrProp(Index: Integer; Value: string);
    function GetVariantProp(Index: Integer): Variant;
    procedure SetVariantProp(Index: Integer; Value: Variant);
    function GetWideStrProp(Index: Integer): WideString;
    procedure SetWideStrProp(Index: Integer; Value: WideString);
  protected
    procedure GetPropDataFromIndex(Index: Integer; out Instance: TObject; out Info: PPropInfo);
    procedure InjectImplementers;
    procedure RevertRTTI;
  end;

// Free the item designer
procedure FreeItemDesigner(var designer: TPersistent);
// Retrieve an instance of the designer for the given item
function GetItemDesigner(AnItem: IJvDataItem): TPersistent;
// Register a property view for an IJvDataItem support interface
procedure RegisterDataItemIntfProp(const IID: TGUID; const PropClass: TJvDataProviderItemClass);

implementation

uses
  Windows, ImgList,
  JclSysUtils,
  JvDsgnConsts, JvJCLUtils;

type
  PPropData = ^TPropData;

  TIntfItem = record
    GUID: TGUID;
    PropClass: TJvDataProviderItemClass;
  end;
  TIntfItems = array of TIntfItem;

var
  GIntfPropReg: TIntfItems;

procedure FreeItemDesigner(var designer: TPersistent);
var
  obj: TPersistent;
begin
  obj := designer;
  designer := nil;
  if (obj <> nil) then
  begin
    if obj is TJvDataProviderItem then
      // free the instance of TJvDataProviderItem we created in GetItemDesigner
      obj.Free
    else
      // revert our changes to the RTTI
      TBaseItemDsgn(obj).RevertRTTI;
  end;
end;

function GetItemDesigner(AnItem: IJvDataItem): TPersistent;
var
  impl: TObject;
begin
  impl := AnItem.Implementer;
  if (impl is TExtensibleInterfacedPersistent) then
  begin
    // the implementing instance will be edited directly
    Result := TPersistent(impl);
    // inject published properties from each of the extension implementers
    TBaseItemDsgn(Result).InjectImplementers;
  end
  else
    // create an instance of the designer; will automatically inject properties based on the interfaces it supports
    Result := TJvDataProviderItem.Create(AnItem);
end;

function LocateReg(IID: TGUID): Integer;
begin
  Result := High(GIntfPropReg);
  while (Result >= 0) and not IsEqualGUID(GIntfPropReg[Result].GUID, IID) do
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
  while NumItems > 0 do
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
        Inc(Result, SizeOf(TClass) + SizeOf(PPTypeInfo) + SizeOf(Smallint) + StringBaseLen(1, @TypeData.UnitName));
        Inc(Result, PropListSize(Pointer(PAnsiChar(@TypeData.UnitName) + StringBaseLen(1, @TypeData.UnitName))));
      end;
  end;
end;

function AllocTypeInfo(Size: Integer): PTypeInfo;
var
  P: PPointer;
begin
  P := AllocMem(SizeOf(P) + Size);
  Inc(P);
  Result := PTypeInfo(P);
end;

procedure FreeTypeInfo(ATypeInfo: PTypeInfo);
var
  P: PPointer;
begin
  P := PPointer(ATypeInfo);
  Dec(P);
  FreeMem(P);
end;

function GetOrgTypeInfo(ATypeInfo: PTypeInfo): PTypeInfo;
var
  P: PPointer;
begin
  P := PPointer(ATypeInfo);
  Dec(P);
  Result := P^;
end;

procedure SetOrgTypeInfo(ATypeInfo, Value: PTypeInfo);
var
  P: PPointer;
begin
  P := PPointer(ATypeInfo);
  Dec(P);
  P^ := Value;
end;

function CloneTypeInfo(OrgTypeInfo: PTypeInfo; AdditionalSpace: Longint = 0): PTypeInfo;
var
  OrgSize: Integer;
begin
  OrgSize := TypeInfoSize(OrgTypeInfo);
  Result := AllocTypeInfo(OrgSize + AdditionalSpace);
  SetOrgTypeInfo(Result, OrgTypeInfo);
  Move(OrgTypeInfo^, Result^, OrgSize);
end;

function VMTTypeInfoFromClass(const AClass: TClass): PPTypeInfo;
var
  P: {$IFDEF COMPILER12_UP}PByte{$ELSE}PChar{$ENDIF COMPILER12_UP};
begin
  P := Pointer(AClass);
  Inc(P, vmtTypeInfo); // Now pointing to TypeInfo of the VMT table.
  Result := PPTypeInfo(P);
end;

procedure CreateTypeInfo(const AClass: TClass);
var
  VMTTypeInfo: PPTypeInfo;
  NewTypeInfo: PTypeInfo;
  WrittenBytes: Cardinal;
begin
  VMTTypeInfo := VMTTypeInfoFromClass(AClass);
  { Below the typeinfo is cloned, while an additional 2048 bytes are reserved at the end. This 2048
    bytes will be used to "inject" additional properties. Since each property takes 27 + the length
    of the property name bytes, assuming an average of 40 bytes/property will allow approximately 50
    properties to be appended to the existing property list. }
  // (rom) is there some security so we do not blow up everything by exceeding the 2048 bytes?
  NewTypeInfo := CloneTypeInfo(VMTTypeInfo^, 2048);
  if not WriteProtectedMemory(VMTTypeInfo, @NewTypeInfo, SizeOf(NewTypeInfo), WrittenBytes) then
    FreeTypeInfo(NewTypeInfo);
end;

procedure ClearTypeInfo(const AClass: TClass);
var
  VMTTypeInfo: PPTypeInfo;
  OldTypeInfo, NewTypeInfo: PTypeInfo;
  WrittenBytes: Cardinal;
begin
  VMTTypeInfo := VMTTypeInfoFromClass(AClass);
  OldTypeInfo := VMTTypeInfo^;
  NewTypeInfo := GetOrgTypeInfo(OldTypeInfo);

  WriteProtectedMemory(VMTTypeInfo, @NewTypeInfo, SizeOf(NewTypeInfo), WrittenBytes);

  FreeTypeInfo(OldTypeInfo);
end;

function GetPropData(TypeData: PTypeData): PPropData;
begin
  Result := PPropData(PAnsiChar(@TypeData.UnitName) + StringBaseLen(1, @TypeData.UnitName));
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

//=== { TJvDataProviderItem } ================================================

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
    if Supports(AnItem, GIntfPropReg[I].GUID, IUnk) then
    begin
      PrpData := GetPropData(GetTypeData(GIntfPropReg[I].PropClass.ClassInfo));
      AppendPropList(ClassType, PPropInfo(Cardinal(PrpData) + 2), PrpData.PropCount);
    end;
end;

function TJvDataProviderItem.Item: IJvDataItem;
begin
  Result := FItem;
end;

function TJvDataProviderItem.GetOwner: TPersistent;
begin
  if Item <> nil then
    Result := (Item.GetItems.Provider as IInterfaceComponentReference).GetComponent
  else
    Result := inherited GetOwner;
end;

function TJvDataProviderItem.GetNamePath: string;
var
  Comp: TPersistent;
begin
  Comp := GetOwner;
  if (Comp <> nil) and (Comp is TComponent) then
    Result := (Comp as TComponent).Name
  else
    Result := RsUnknown;
  if Item <> nil then
    Result := Result + ': Item[' + Item.GetID + ']'
  else
    Result := Result + ': ' + RsNoItem;
end;

//=== { TJvDataItemTextPropView } ============================================

type
  TJvDataItemTextPropView = class(TJvDataProviderItem)
  protected
    function GetText: string;
    procedure SetText(Value: string);
  published
    property Text: string read GetText write SetText;
  end;

function TJvDataItemTextPropView.GetText: string;
begin
  Result := (Item as IJvDataItemText).Text;
end;

procedure TJvDataItemTextPropView.SetText(Value: string);
begin
  (Item as IJvDataItemText).Text := Value;
end;

//=== { TJvDataItemImagePropView } ===========================================

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

//=== { TJvDataItemsImagesPropView } =========================================

type
  TJvDataItemsImagesPropView = class(TJvDataProviderItem)
  protected
    function GetDisabledImages: TCustomImageList;
    procedure SetDisabledImages(Value: TCustomImageList);
    function GetHotImages: TCustomImageList;
    procedure SetHotImages(Value: TCustomImageList);
    function GetImages: TCustomImageList;
    procedure SetImages(Value: TCustomImageList);
  published
    property DisabledImages: TCustomImageList read GetDisabledImages write SetDisabledImages;
    property HotImages: TCustomImageList read GetHotImages write SetHotImages;
    property Images: TCustomImageList read GetImages write SetImages;
  end;

function TJvDataItemsImagesPropView.GetDisabledImages: TCustomImageList;
begin
  Result := (Item as IJvDataItemsImages).DisabledImages;
end;

procedure TJvDataItemsImagesPropView.SetDisabledImages(Value: TCustomImageList);
begin
  (Item as IJvDataItemsImages).DisabledImages := Value;
end;

function TJvDataItemsImagesPropView.GetHotImages: TCustomImageList;
begin
  Result := (Item as IJvDataItemsImages).HotImages;
end;

procedure TJvDataItemsImagesPropView.SetHotImages(Value: TCustomImageList);
begin
  (Item as IJvDataItemsImages).HotImages := Value;
end;

function TJvDataItemsImagesPropView.GetImages: TCustomImageList;
begin
  Result := (Item as IJvDataItemsImages).Images;
end;

procedure TJvDataItemsImagesPropView.SetImages(Value: TCustomImageList);
begin
  (Item as IJvDataItemsImages).Images := Value;
end;

//=== { TBaseItemDsgn } ======================================================

{$IFDEF COMPILER10_UP}
function TBaseItemDsgn.GetDynArrayProp(Index: Integer): Pointer;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetDynArrayProp(instance, info);
end;

{$ENDIF COMPILER10_UP}
function TBaseItemDsgn.GetFloatProp(Index: Integer): Extended;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetFloatProp(instance, info);
end;

function TBaseItemDsgn.GetInt64Prop(Index: Integer): Int64;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetInt64Prop(instance, info);
end;

function TBaseItemDsgn.GetIsStoredProp(Index: Integer): Boolean;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.IsStoredProp(instance, info);
end;

function TBaseItemDsgn.GetInterfaceProp(Index: Integer): IInterface;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetInterfaceProp(instance, info);
end;

function TBaseItemDsgn.GetMethodProp(Index: Integer): TMethod;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetMethodProp(instance, info);
end;

function TBaseItemDsgn.GetOrdProp(Index: Integer): Longint;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetOrdProp(instance, info);
end;

procedure TBaseItemDsgn.GetPropDataFromIndex(Index: Integer; out Instance: TObject; out Info: PPropInfo);
var
  props: PPropList;
begin
  // instance is the implementer at index specified in the high-word of the Index parameter
  Instance := GetImplementer(HiWord(Index));
  // Retrieve the property list
  GetPropList(PTypeInfo(Instance.ClassInfo), props);
  try
    // Get the property at the index specified in the low-word of the Index parameter
    Info := props[LoWord(Index)];
  finally
    FreeMem(props);
  end;
end;

function TBaseItemDsgn.GetStrProp(Index: Integer): string;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetStrProp(instance, info);
end;

function TBaseItemDsgn.GetVariantProp(Index: Integer): Variant;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetVariantProp(instance, info);
end;

function TBaseItemDsgn.GetWideStrProp(Index: Integer): WideString;
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  Result := TypInfo.GetWideStrProp(instance, info);
end;

procedure TBaseItemDsgn.InjectImplementers;
var
  lstSize: Integer;
  numNewProps: Integer;
  implInst: TAggregatedPersistentEx;
  implProps: PPropList;
  destProp: PPropInfo;
  destIdx: Integer;

  procedure CalcAdditionalSize;
  var
    implIndex: Integer;
    thisCount: Integer;
    propIndex: Integer;
  begin
    lstSize := 0;
    numNewProps := 0;
    // iterate over the extension list
    for implIndex := 0 to ImplCount - 1 do
    begin
      // get the implementation
      implInst := GetImplementer(implIndex);
      // get the property info list, including inherited properties
      thisCount := GetPropList(PTypeInfo(implInst.ClassInfo), implProps);
      try
        // update the count
        Inc(numNewProps, thisCount);
        // iterate to determine size
        for propIndex := 0 to thisCount - 1 do
          Inc(lstSize, SizeOf(TPropInfo) - SizeOf(ShortString) + 1 + Length(implProps[propIndex].Name));
      finally
        FreeMem(implProps);
      end;
    end;
  end;

  function CreateClonedTypeInfo: Boolean;
  var
    VMTTypeInfo: PPTypeInfo;
    NewTypeInfo: PTypeInfo;
    WrittenBytes: Cardinal;
  begin
    VMTTypeInfo := VMTTypeInfoFromClass(ClassType);
    // make a copy and reserve additional space to include the extension properties
    NewTypeInfo := CloneTypeInfo(VMTTypeInfo^, lstSize);
    // update the RTTI
    Result := WriteProtectedMemory(VMTTypeInfo, @NewTypeInfo, SizeOf(NewTypeInfo), WrittenBytes);
    // if this failed, free the cloned type info
    if not Result then
      FreeTypeInfo(NewTypeInfo);
  end;

  procedure InitDestinationPointer;
  var
    TypeData: PTypeData;
    ExistingCount: Integer;
    BaseInfoSize: Integer;
  begin
    TypeData := GetTypeData(PTypeInfo(ClassInfo));
    destProp := PPropInfo(GetPropData(TypeData));
    destIdx := TypeData.PropCount;
    ExistingCount := PPropData(destProp).PropCount;
    Inc(PAnsiChar(destProp), SizeOf(Word));
    BaseInfoSize := SizeOf(TPropInfo) - SizeOf(ShortString) + 1;
    while ExistingCount > 0 do
    begin
      Inc(PAnsiChar(destProp), BaseInfoSize + Length(destProp.Name));
      Dec(ExistingCount);
    end;
  end;

  procedure MakeNewProperty(implementationIndex, propertyIndex: Integer);
  var
    size: Integer;
  begin;
    // get the size...
    size := SizeOf(TPropInfo) - SizeOf(ShortString) + 1 + Length(implProps[propertyIndex].Name);
    // copy all info...
    Move(implProps[propertyIndex]^, destProp^, size);
    // setup name index
    destProp.NameIndex := destIdx;
    // advance name index
    Inc(destIdx);
    // setup the property index info
    destProp.Index := Word(implementationIndex) shl 16 + Word(propertyIndex);
    // update stored proc
    destProp.StoredProc := @TBaseItemDsgn.GetIsStoredProp;
    // update GetProc/SetProc
    case implProps[propertyIndex].PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet, tkClass, tkWChar:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetOrdProp;
          destProp.SetProc := @TBaseItemDsgn.SetOrdProp;
        end;
      tkFloat:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetFloatProp;
          destProp.SetProc := @TBaseItemDsgn.SetFloatProp;
        end;
      {$IFDEF UNICODE} tkUString, {$ENDIF}
      tkString, tkLString:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetStrProp;
          destProp.SetProc := @TBaseItemDsgn.SetStrProp;
        end;
      tkMethod:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetMethodProp;
          destProp.SetProc := @TBaseItemDsgn.SetMethodProp;
        end;
      tkWString:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetWideStrProp;
          destProp.SetProc := @TBaseItemDsgn.SetWideStrProp;
        end;
      tkVariant:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetVariantProp;
          destProp.SetProc := @TBaseItemDsgn.SetVariantProp;
        end;
      tkInterface:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetInterfaceProp;
          destProp.SetProc := @TBaseItemDsgn.SetInterfaceProp;
        end;
      tkInt64:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetInt64Prop;
          destProp.SetProc := @TBaseItemDsgn.SetInt64Prop;
        end;
      {$IFDEF COMPILER10_UP}
      tkDynArray:
        begin
          destProp.GetProc := @TBaseItemDsgn.GetDynArrayProp;
          destProp.SetProc := @TBaseItemDsgn.SetDynArrayProp;
        end;
      {$ENDIF COMPILER10_UP}
    end;
    // advance destination pointer
    Inc(PAnsiChar(destProp), size);
  end;

  procedure CopyExtensionProperties;
  var
    implIndex: Integer;
    numProps: Integer;
    propIndex: Integer;
    TypeData: PTypeData;

  begin
    // iterate over the extension list
    for implIndex := 0 to ImplCount - 1 do
    begin
      // get the implementation
      implInst := GetImplementer(implIndex);
      // get the properties to add
      numProps := GetPropList(PTypeInfo(implInst.ClassInfo), implProps);
      try
        // iterate to copy the property
        for propIndex := 0 to numProps - 1 do
        begin
          // create a new property to reference the original one
          MakeNewProperty(implIndex, propIndex);
        end;
      finally
        FreeMem(implProps);
      end;
    end;
    // Update the count
    TypeData := GetTypeData(PTypeInfo(ClassInfo));
    TypeData.PropCount := TypeData.PropCount + numNewProps;
    GetPropData(TypeData).PropCount := GetPropData(TypeData).PropCount + numNewProps;
  end;

begin
  // Determine how much more memory we need...
  CalcAdditionalSize;
  // Create a clone, reserving enough additional memory for the properties of all extensions
  if CreateClonedTypeInfo then
  begin
    // init destination pointer
    InitDestinationPointer;
    // Copy extension properties
    CopyExtensionProperties;
  end;
end;

procedure TBaseItemDsgn.RevertRTTI;
begin
  ClearTypeInfo(ClassType);
end;

{$IFDEF COMPILER10_UP}
procedure TBaseItemDsgn.SetDynArrayProp(Index: Integer; Value: Pointer);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetDynArrayProp(instance, info, Value);
end;

{$ENDIF COMPILER10_UP}
procedure TBaseItemDsgn.SetFloatProp(Index: Integer; Value: Extended);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetFloatProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetInt64Prop(Index: Integer; Value: Int64);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetInt64Prop(instance, info, Value);
end;

procedure TBaseItemDsgn.SetInterfaceProp(Index: Integer; Value: IInterface);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetInterfaceProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetMethodProp(Index: Integer; Value: TMethod);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetMethodProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetOrdProp(Index, Value: Integer);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetOrdProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetStrProp(Index: Integer; Value: string);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetStrProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetVariantProp(Index: Integer; Value: Variant);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetVariantProp(instance, info, Value);
end;

procedure TBaseItemDsgn.SetWideStrProp(Index: Integer; Value: WideString);
var
  instance: TObject;
  info: PPropInfo;
begin
  GetPropDataFromIndex(Index, instance, info);
  TypInfo.SetWideStrProp(instance, info, Value);
end;

//=== Registration of default interface property views =======================

procedure RegProviderItemInterfaces;
begin
  RegisterDataItemIntfProp(IJvDataItemText, TJvDataItemTextPropView);
  RegisterDataItemIntfProp(IJvDataItemImage, TJvDataItemImagePropView);
  RegisterDataItemIntfProp(IJvDataItemsImages, TJvDataItemsImagesPropView);
end;

initialization
  CreateTypeInfo(TJvDataProviderItem);  // Duplicate class type info to allow properties to be injected.
  RegProviderItemInterfaces;            // register default interface property views.

finalization
  ClearTypeInfo(TJvDataProviderItem);   // undo the hacking of TJvDataProviderItem

end.