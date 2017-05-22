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
  Peter Th�rnqvist

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDataProviderEditors;

{$I jvcl.inc}

interface

uses
  Classes,
  DesignIntf, DesignEditors, DesignMenus,
  JvDataProvider, JvDataProviderIntf;

type
  TGetPropEditProc = TGetPropProc;

  TJvDataConsumerExtPropertyEditor = class(TPropertyEditor)
  protected
    function GetConsumerExt: TJvDataConsumerAggregatedObject;
    function GetConsumer: IJvDataConsumer;
    function GetConsumerImpl: TJvDataConsumer;
  end;

  TJvProviderEditor = class(TDefaultEditor)
  protected
    function Provider: IJvDataProvider;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  TJvDataConsumerProperty = class(TEnumProperty)
  private
    FOrgStrProc: TGetStrProc;
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
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvDataConsumerContextProperty = class(TJvDataConsumerExtPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TJvConsumerNotifyComponentProperty = class(TComponentProperty)
  protected
    EnumProc: TGetStrProc;
    function IsValidComp(AComponent: TComponent): Boolean;
    procedure CheckAndAddComp(const S: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  SysUtils, TypInfo, RTLConsts,
  JvDataConsumerContextSelectForm, JvDataConsumerItemSelectForm,
  JvDataProviderDesignerForm, JvDataContextManagerForm, JvDsgnConsts;

type
  TOpenSvc = class(TJvDataConsumer);
  TOpenConsumerAggregate = class(TJvDataConsumerAggregatedObject);

//=== { TJvDataConsumerExtPropertyEditor } ===================================

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

//=== { TJvDataConsumerProperty } ============================================

procedure TJvDataConsumerProperty.CheckAndAddComp(const S: string);
var
  Comp: TComponent;
  Prov: IJvDataProvider;
  Ref: IUnknown;
begin
  if Assigned(FOrgStrProc) then
  begin
    Comp := Designer.GetComponent(S);
    if Comp <> nil then
      with Comp do
        if GetInterface(IInterfaceComponentReference, Ref) and GetInterface(IJvDataProvider, Prov) then
          FOrgStrProc(S);
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
begin
  Svc := GetConsumerServiceAt(Index);
  if Svc <> nil then
  begin
    Svc.Provider := Value;
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
  Components.Add(ConsumerSvcExt);
  GetComponentProperties(Components, tkAny, Designer, Proc);
end;

function TJvDataConsumerProperty.AllEqual: Boolean;
begin
  Result := True;
end;

function TJvDataConsumerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSubProperties, paSortList, paVolatileSubproperties];
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
  else
  if Supports(GetProviderIntf, IInterfaceComponentReference, ICR) then
    Result := ICR.GetComponent.Name
  else
    raise EPropertyError.CreateRes(@RsESpecifiedProviderIsNotATComponentDe);
end;

procedure TJvDataConsumerProperty.SetValue(const Value: string);
var
  Comp: TComponent;
  ProvIntf: IJvDataProvider;
  Ref: IUnknown;
begin
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
end;

procedure TJvDataConsumerProperty.GetValues(Proc: TGetStrProc);
begin
  FOrgStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TComponent.ClassInfo), CheckAndAddComp);
  finally
    FOrgStrProc := nil;
  end;
end;

//=== { TJvDataProviderTreeProperty } ========================================

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
  Result := 'Data tree'; // do not localize
end;

procedure TJvDataProviderTreeProperty.SetValue(const Value: string);
begin
end;

//=== { TJvDataProviderItemIDProperty } ======================================

procedure TJvDataProviderItemIDProperty.Edit;
begin
  DataConsumerSelectItem(GetConsumerImpl, Designer);
end;

function TJvDataProviderItemIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TJvDataProviderItemIDProperty.GetValue: string;
var
  Item: IJvDataItem;
  Text: IJvDataItemText;
begin
  GetConsumerImpl.Enter;
  try
    Item := TJvDataConsumerItemSelect(GetConsumerExt).GetItemIntf;
    if Item <> nil then
    begin
      if Supports(Item, IJvDataItemText, Text) then
        Result := Text.Text
      else
        Result := '[ID:' + Item.GetID + ']';
    end
    else
      Result := RsNone;
  finally
    GetConsumerImpl.Leave;
  end;
end;

procedure TJvDataProviderItemIDProperty.SetValue(const Value: string);
begin
end;

//=== { TJvDataConsumerContextEditor } =======================================

procedure TJvDataConsumerContextProperty.Edit;
begin
  if ConsumerSelectContext(GetConsumer) then
    Designer.Modified;
end;

function TJvDataConsumerContextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TJvDataConsumerContextProperty.GetValue: string;
var
  Ctx: IJvDataContext;
begin
  Ctx := TJvDataConsumerContext(GetConsumerExt).ContextIntf;
  if Ctx <> nil then
  begin
    Result := Ctx.Name;
    repeat
      Ctx := Ctx.Contexts.Ancestor;
      if Ctx <> nil then
        Result := Ctx.Name + '\' + Result;
    until Ctx = nil;
  end
  else
    Result := '';
end;

procedure TJvDataConsumerContextProperty.SetValue(const Value: string);
var
  CtxList: IJvDataContexts;
  Ctx: IJvDataContext;
begin
  if Supports(GetConsumerImpl.ProviderIntf, IJvDataContexts, CtxList) then
  begin
    Ctx := CtxList.GetContextByName(Value);
    if (Value = '') or (Value = '\') or (Ctx <> nil) then
    begin
      TJvDataConsumerContext(GetConsumerExt).ContextIntf := Ctx;
      Designer.Modified;
    end;
  end;
end;

//=== { TJvConsumerNotifyComponentProperty } =================================

function TJvConsumerNotifyComponentProperty.IsValidComp(AComponent: TComponent): Boolean;
const
 cProvider = 'Provider';
var
  PI: PPropInfo;
  Obj: TObject;
  Consumer: IJvDataConsumer;
  TmpNotifier: IJvDataConsumerClientNotify;
  Srv: IJvDataConsumerServerNotify;
begin
  Result := (AComponent is GetTypeData(GetPropType)^.ClassType);
  if Result then
  begin
    PI := TypInfo.GetPropInfo(AComponent, cProvider);
    if PI <> nil then
    begin
      Obj := TypInfo.GetObjectProp(AComponent, cProvider);
      Result := Supports(Obj, IJvDataConsumer, Consumer) and
        Supports(Consumer, IJvDataConsumerClientNotify, TmpNotifier) and
        Supports(TJvDataConsumerClientNotifyItem(GetComponent(0)).List.Server.Controller,
          IJvDataConsumerServerNotify, Srv) and Srv.IsValidClient(TmpNotifier);
    end
    else
      Result := False;
  end;
end;

procedure TJvConsumerNotifyComponentProperty.CheckAndAddComp(const S: string);
begin
  if Assigned(EnumProc) and (S <> '') and IsValidComp(Designer.GetComponent(S)) then
    EnumProc(S);
end;

procedure TJvConsumerNotifyComponentProperty.GetValues(Proc: TGetStrProc);
begin
  EnumProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TComponent.ClassInfo), CheckAndAddComp);
  finally
    EnumProc := nil;
  end;
end;

//=== { TJvProviderEditor } ==================================================

function TJvProviderEditor.Provider: IJvDataProvider;
begin
  Supports(Component, IJvDataProvider, Result);
end;

procedure TJvProviderEditor.Edit;
begin
  if Provider.AllowProviderDesigner then
    ExecuteVerb(0)
  else
  if Provider.AllowContextManager then
    ExecuteVerb(1)
  else
    inherited Edit;
end;

procedure TJvProviderEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      DesignProvider(Provider, Designer, '');
    1:
      ManageProviderContexts(Provider, Designer, '');
  end;
end;

function TJvProviderEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsTreeDesignerEllipsis;
    1:
      Result := RsContextManagerEllipsis;
    else
      Result := Format(RsInvalidVerbd, [Index]);
  end;
end;

function TJvProviderEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TJvProviderEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    0:
      AItem.Enabled := Provider.AllowProviderDesigner;
    1:
      AItem.Enabled := Provider.AllowContextManager;
  end;
end;

end.