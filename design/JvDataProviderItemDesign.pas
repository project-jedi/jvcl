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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDataProviderItemDesign;

interface

uses
  Classes,
  JvDataProviderIntf;

type
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
  SysUtils, TypInfo,
  Windows, ImgList,
  {$IFDEF VisualCLX}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$ENDIF VisualCLX}
  JvDsgnConsts;

type
  PPropData = ^TPropData;

  TIntfItem = record
    GUID: TGUID;
    PropClass: TJvDataProviderItemClass;
  end;
  TIntfItems = array of TIntfItem;

var
  GIntfPropReg: TIntfItems;

{$IFNDEF COMPILER6_UP} 
function IsEqualGUID(const IID1, IID2: TGUID): Boolean;
begin
  Result := CompareMem(@IID1, @IID2, SizeOf(IID1));
end;
{$ENDIF COMPILER6_UP}
  
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

function TypeInfoFromClass(const AClass: TClass): PChar;
begin
  Result := Pointer(AClass);
  Dec(Result, 60); // Now pointing to TypeInfo of the VMT table.
end;

procedure CreateTypeInfo(const AClass: TClass);
var
  P: PChar;
  PNewInfo: Pointer;
  OldProtect: Cardinal;
begin
  P := TypeInfoFromClass(AClass);
  { Below the typeinfo is cloned, while an additional 2048 bytes are reserved at the end. This 2048
    bytes will be used to "inject" additional properties. Since each property takes 27 + the length
    of the property name bytes, assuming an average of 40 bytes/property will allow approximately 50
    properties to be appended to the existing property list. }
  // (rom) is there some security so we do not blow up everything by exceeding the 2048 bytes?
  PNewInfo := CloneTypeInfo(Pointer(PInteger(P)^), 2048);
  {$IFDEF MSWINDOWS}
  if VirtualProtect(P, 4, PAGE_WRITECOPY, OldProtect) then
  try
    PInteger(P)^ := Integer(PNewInfo);
  finally
    VirtualProtect(P, 4, OldProtect, OldProtect);
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  WriteProcessMemory(GetCurrentProcess, P, PNewInfo, 4, OldProtect);  // asn ???
  {$ENDIF LINUX}
end;

procedure ClearTypeInfo(const AClass: TClass);
var
  P: PChar;
  PNewType: PChar;
  OldProtect: Cardinal;
begin
  P := TypeInfoFromClass(AClass);
  PNewType := Pointer(PInteger(P)^);  // The new type currently in use.
  Dec(PNewType, 4);                   // Points to the original PTypeInfo value.
  {$IFDEF MSWINDOWS}
  if VirtualProtect(P, 4, PAGE_WRITECOPY, OldProtect) then
  try
    PInteger(P)^ := Integer(PInteger(PNewType)^);
  finally
    VirtualProtect(P, 4, OldProtect, OldProtect);
  end;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  WriteProcessMemory(GetCurrentProcess, P, PNewType, 4, OldProtect);  // asn ???
  {$ENDIF LINUX}
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

//=== TJvDataProviderItem ====================================================

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

//=== TJvDataItemTextPropView ================================================

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

//=== TJvDataItemImagePropView ===============================================

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

//=== TJvDataItemsImagesPropView =============================================

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
