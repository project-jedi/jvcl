{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderPropEdits.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Peter Thörnqvist

Last Modified: 2003-07-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderEditors;

interface

uses
  {$IFNDEF COMPILER6_UP}DsgnIntf,{$ELSE}DesignIntf, DesignEditors,{$ENDIF}
  JvDataProvider, JvDataProviderImpl;
  
type
  TJvDataConsumerExtPropertyEditor = class(TPropertyEditor)
  protected
    function GetConsumerExt: TJvDataConsumerAggregatedObject;
    function GetConsumer: IJvDataConsumer;
    function GetConsumerImpl: TJvDataConsumer;
  end;

procedure Register;

implementation

uses
  Classes, {$IFNDEF COMPILER6_UP}Consts,{$ELSE}RTLConsts,{$ENDIF} SysUtils, TypInfo,
  JvDataConsumerItemSelectForm, JvDataProviderDesignerForm;

type
{$IFDEF COMPILER6_UP}
  TGetPropEditProc = TGetPropProc;
{$ENDIF COMPILER6_UP}

  TJvDataConsumerProperty = class(TEnumProperty)
  private
    OrgStrProc: TGetStrProc;
    procedure CheckAndAddComp(const S: string);
  protected
    function GetConsumerServiceAt(Index: Integer): TJvDataConsumer;
    function GetProviderIntfAt(Index: Integer): IJvDataProvider; dynamic;
    procedure SetProviderIntfAt(Index: Integer; Value: IJvDataProvider); dynamic;
    function GetProviderIntf: IJvDataProvider;
    procedure SetProviderIntf(Value: IJvDataProvider);
    procedure GetExtensionsProperties(ConsumerSvc: TJvDataConsumer; Proc: TGetPropEditProc);
    procedure GetExtensionProperties(ConsumerSvcExt: TJvDataConsumerAggregatedObject; Proc: TGetPropEditProc);
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TJvDataProviderTreeProperty = class(TEnumProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvDataProviderItemIDProperty = class(TJvDataConsumerExtPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
//    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TOpenSvc = class(TJvDataConsumer);
  TOpenConsumerAggregate = class(TJvDataConsumerAggregatedObject);

//===TJvDataConsumerExtPropertyEditor===============================================================

function TJvDataConsumerExtPropertyEditor.GetConsumerExt: TJvDataConsumerAggregatedObject;
begin
  Result := TJvDataConsumerAggregatedObject(GetComponent(0));
end;

function TJvDataConsumerExtPropertyEditor.GetConsumer: IJvDataConsumer;
begin
  Result := GetConsumerImpl;
end;

function TJvDataConsumerExtPropertyEditor.GetConsumerImpl: TJvDataConsumer;
begin
  Result := TOpenConsumerAggregate(GetConsumerExt).ConsumerImpl;
end;

//===TJvDataConsumerProperty========================================================================

procedure TJvDataConsumerProperty.CheckAndAddComp(const S: string);
var
  Comp: TComponent;
  Prov: IJvDataProvider;
  Ref: IUnknown;
begin
  if @OrgStrProc <> nil then
  begin
    Comp := Designer.GetComponent(S);
    if (Comp <> nil) then
      with Comp do
        if GetInterface(IInterfaceComponentReference, Ref) and GetInterface(IJvDataProvider, Prov) then
      OrgStrProc(S);
  end;
end;

function TJvDataConsumerProperty.GetConsumerServiceAt(Index: Integer): TJvDataConsumer;
begin
  Result := TJvDataConsumer(GetOrdValueAt(Index));
end;

function TJvDataConsumerProperty.GetProviderIntfAt(Index: Integer): IJvDataProvider;
var
  Svc: TJvDataConsumer;
begin
  Svc := GetConsumerServiceAt(Index);
  if Svc = nil then
    Result := nil
  else
    Result := Svc.ProviderIntf;
end;

procedure TJvDataConsumerProperty.SetProviderIntfAt(Index: Integer; Value: IJvDataProvider);
var
  Svc: TJvDataConsumer;
  {$IFNDEF COMPILER6_UP}
  CompRef: IInterfaceComponentReference;
  ProvComp: TComponent;
  {$ENDIF}
begin
  Svc := GetConsumerServiceAt(Index);
  if Svc <> nil then
  begin
    {$IFNDEF COMPILER6_UP}
    if Value <> nil then
    begin
      if not Supports(Value, IInterfaceComponentReference, CompRef) then
        raise EPropertyError.Create('Specified provider is not a TComponent descendant.');
      ProvComp := CompRef.GetComponent;
    end
    else
      ProvComp := nil;
    Svc.Provider := ProvComp;
    {$ELSE}
    Svc.Provider := Value;
    {$ENDIF}
    Modified;
  end;
end;

function TJvDataConsumerProperty.GetProviderIntf: IJvDataProvider;
begin
  Result := GetProviderIntfAt(0);
end;

procedure TJvDataConsumerProperty.SetProviderIntf(Value: IJvDataProvider);
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    SetProviderIntfAt(I, Value);
end;

procedure TJvDataConsumerProperty.GetExtensionsProperties(ConsumerSvc: TJvDataConsumer;
  Proc: TGetPropEditProc);
var
  I: Integer;
begin
  for I := 0 to TOpenSvc(ConsumerSvc).ExtensionCount - 1 do
    GetExtensionProperties(TOpenSvc(ConsumerSvc).Extension(I), Proc);
end;

procedure TJvDataConsumerProperty.GetExtensionProperties(
  ConsumerSvcExt: TJvDataConsumerAggregatedObject; Proc: TGetPropEditProc);
var
  Components: IDesignerSelections;
begin
  Components := CreateSelectionList;
  Components.Add({$IFNDEF COMPILER6_UP}MakeIPersistent({$ENDIF}ConsumerSvcExt{$IFNDEF COMPILER6_UP}){$ENDIF});
  {$IFDEF COMPILER6_UP}
  GetComponentProperties(Components, tkAny, Designer, Proc);
  {$ELSE}
  GetComponentProperties((Components as IComponentList).GetComponentList, tkAny, Designer, Proc);
  {$ENDIF}
end;

function TJvDataConsumerProperty.AllEqual: Boolean;
begin
  Result := True;
end;

function TJvDataConsumerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSubProperties, paSortList
    {$IFDEF COMPILER6_UP}, paVolatileSubproperties{$ENDIF}];
end;

procedure TJvDataConsumerProperty.GetProperties(Proc: TGetPropEditProc);
begin
  GetExtensionsProperties(GetConsumerServiceAt(0), Proc);
end;

function TJvDataConsumerProperty.GetValue: string;
var
  ICR: IInterfaceComponentReference;
begin
  if GetProviderIntf = nil then
    Result := ''
  else if Supports(GetProviderIntf, IInterfaceComponentReference, ICR) then
    Result := ICR.GetComponent.Name
  else
    raise EPropertyError.Create('Specified provider is not a TComponent descendant.');
end;

procedure TJvDataConsumerProperty.SetValue(const Value: string);
var
  Comp: TComponent;
  ProvIntf: IJvDataProvider;
  Ref: IUnknown;
  {$IFNDEF COMPILER6_UP}
  List: IDesignerSelections;
  Dsgnr: IFormDesigner;
  {$ENDIF COMPILER6_UP}
begin
  {$IFNDEF COMPILER6_UP}
  Dsgnr := Self.Designer;
  {$ENDIF COMPILER6_UP}
  if Value = '' then
    Comp := nil
  else
    Comp := Designer.GetComponent(Value);

  if Comp <> nil then
  begin
    with Comp do
      if not GetInterface(IInterfaceComponentReference, Ref) or not GetInterface(IJvDataProvider, ProvIntf) then
        raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  end
  else
    ProvIntf := nil;
  SetProviderIntf(ProvIntf);
  {$IFNDEF COMPILER6_UP}
  { Force the object inspector to discard its property editors. Code copied from post from
    Constantine Yannakopoulos in borland.public.delphi.vcl.components.writing }
  List := CreateSelectionList;
  Dsgnr.GetSelections(List);
  Dsgnr.NoSelection; // Self will be gone here
  Dsgnr.SetSelections(List);
  Dsgnr.Modified;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDataConsumerProperty.GetValues(Proc: TGetStrProc);
begin
  OrgStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TComponent.ClassInfo), CheckAndAddComp);
  finally
    OrgStrProc := nil;
  end;
end;

//===TJvDataProviderTreeProperty====================================================================

procedure TJvDataProviderTreeProperty.Edit;
begin
  DesignProvider(TJvCustomDataProvider(GetComponent(0)), Designer, GetName);
end;

function TJvDataProviderTreeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvDataProviderTreeProperty.GetValue: string;
begin
  Result := 'Data tree';
end;

procedure TJvDataProviderTreeProperty.SetValue(const Value: string);
begin
end;

//===TJvDataProviderItemIDProperty==================================================================

procedure TJvDataProviderItemIDProperty.Edit;
begin
  DataConsumerSelectItem(GetConsumerImpl, Designer);
end;

function TJvDataProviderItemIDProperty.GetAttributes: TPropertyAttributes;
begin
//  Result := [paValueList, paRevertable];
  Result := [paDialog, paReadOnly];
end;

(*procedure TJvDataProviderItemIDProperty.GetValues(Proc: TGetStrProc);
type
  TStackItem = record
    Items: IJvDataItems;
    Idx: Integer;
  end;
var
  ItemsStack: array of TStackItem;
  Provider: IJvDataProvider;
  CurItems: IJvDataItems;
  CurIdx: Integer;
  TempItems: IJvDataItems;
begin
  SetLength(ItemsStack, 0);
  Provider := GetConsumerImpl.ProviderIntf;
  GetConsumerImpl.Enter;
  try
    if Supports(Provider, IJvDataItems, CurItems) then
    begin
      CurIdx := 0;
      while CurItems <> nil do
      begin
        while CurIdx < CurItems.Count do
        begin
          Proc(CurItems.Items[CurIdx].GetID);
          if Supports(CurItems.Items[CurIdx], IJvDataItems, TempItems) then
          begin
            SetLength(ItemsStack, Length(ItemsStack) + 1);
            with ItemsStack[High(ItemsStack)] do
            begin
              Items := CurItems;
              Idx := CurIdx + 1;
              CurItems := TempItems;
              Tempitems := nil;
              CurIdx := 0;
            end;
          end
          else
            Inc(CurIdx);
        end;
        if Length(ItemsStack) > 0 then
        begin
          with ItemsStack[High(ItemsStack)] do
          begin
            CurItems := Items;
            CurIdx := Idx;
          end;
          SetLength(ItemsStack, High(ItemsStack));
        end
        else
          CurItems := nil;
      end;
    end;
  finally
    GetConsumerImpl.Leave;
  end;
end;
*)

function TJvDataProviderItemIDProperty.GetValue: string;
var
  Item: IJvDataItem;
  Text: IJvDataItemText;
begin
//  Result := GetStrValue;
  GetConsumerImpl.Enter;
  try
    Item := TJvDataConsumerItemSelect(GetConsumerExt).GetItemIntf;
    if Item <> nil then
    begin
      if Supports(Item, IJvDataItemText, Text) then
        Result := Text.Caption
      else
        Result := '[ID:' + Item.GetID + ']';
    end
    else
      Result := '(none)';
  finally
    GetConsumerImpl.Leave;
  end;
end;

procedure TJvDataProviderItemIDProperty.SetValue(const Value: string);
(*var
  Provider: IJvDataProvider;
  Item: IJvDataItem;*)
begin
(*  GetConsumerImpl.Enter;
  try
    Provider := GetConsumerImpl.ProviderIntf;
    if Value = '' then
      SetStrValue('')
    else if (Provider <> nil) then
    begin
      Item := (Provider.GetItems as IJvDataIDSearch).Find(Value, True);
      if Item <> nil then
        SetStrValue(Item.GetID)
      else
        raise EPropertyError.CreateRes(@SInvalidPropertyValue);
    end
    else
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  finally
    GetConsumerImpl.Leave;
  end;*)
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TJvDataConsumer), TComponent, 'Provider', TJvDataConsumerProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataItemID), TPersistent, '', TJvDataProviderItemIDProperty);
  RegisterPropertyEditor(TypeInfo(TJvDataProviderTree), TComponent, '', TJvDataProviderTreeProperty);
end;

end.
