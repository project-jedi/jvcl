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

The Original Code is: JvContextProvider.pas, released on 2003-07-18.

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

unit JvQContextProvider;

{$I jvcl.inc}

interface

uses
  Classes,
  JvQDataProvider, JvQDataProviderIntf;

type
  { Context provider related interfaces. }
  IJvDataContextProvider = interface
    ['{78EB1037-11A5-4871-8115-4AE1AC60B59C}']
    function Get_ClientProvider: IJvDataProvider;
    procedure Set_ClientProvider(Value: IJvDataProvider);
    property ClientProvider: IJvDataProvider read Get_ClientProvider write Set_ClientProvider;
  end;

  IJvDataContextSearch = interface
    ['{C8513B84-FAA0-4794-A4A9-B2899797F52B}']
    function Find(Context: IJvDataContext; const Recursive: Boolean = False): IJvDataItem;
    function FindByName(Name: string; const Recursive: Boolean = False): IJvDataItem;
  end;

  IJvDataContextItems = interface
    ['{3303276D-2596-4FDB-BA1C-CE6E043BEB7A}']
    function GetContexts: IJvDataContexts;
  end;

  IJvDataContextItem = interface
    ['{7156CAC8-0DB9-43B7-96C5-5A56723C5158}']
    function GetContext: IJvDataContext;
  end;

  TJvContextProvider = class(TJvCustomDataProvider, IJvDataContextProvider)
    function IJvDataContextProvider.Get_ClientProvider = GetProviderIntf;
    procedure IJvDataContextProvider.Set_ClientProvider = SetProviderIntf;
  private
    function GetProviderIntf: IJvDataProvider;
    procedure SetProviderIntf(Value: IJvDataProvider);
    function GetProviderComp: TComponent;
    procedure SetProviderComp(Value: TComponent);
  protected
    class function ItemsClass: TJvDataItemsClass; override;
    function ConsumerClasses: TClassArray; override;
  public
    property ProviderComp: TComponent read GetProviderComp write SetProviderComp;
    property ProviderIntf: IJvDataProvider read GetProviderIntf write SetProviderIntf;
  published 
    property Provider: IJvDataProvider read GetProviderIntf write SetProviderIntf; 
  end;

  TJvContextProviderServerNotify = class(TJvDataConsumerServerNotify)
  protected
    procedure ItemSelected(Value: IJvDataItem); override;
    function IsValidClient(Client: IJvDataConsumerClientNotify): Boolean; override;
  end;

implementation

uses
  SysUtils,
  JvQTypes, JvQResources;

type
  TContextItems = class;
  TContextRootItems = class;
  TContextItem = class;
  TContextItemsManager = class;

  TContextItems = class(TJvBaseDataItems, IJvDataContextItems, IJvDataContextSearch)
  protected
    function GetContexts: IJvDataContexts; virtual;
    function Find(Context: IJvDataContext; const Recursive: Boolean = False): IJvDataItem;
    function FindByName(Name: string; const Recursive: Boolean = False): IJvDataItem;
    procedure InitImplementers; override;
    function GetCount: Integer; override;
    function GetItem(I: Integer): IJvDataItem; override;
  end;

  TContextRootItems = class(TContextItems)
  private
    FClientProvider: IJvDataProvider;
    FNotifier: TJvProviderNotification;
    procedure SetClientProvider(Value: IJvDataProvider);
    procedure DataProviderChanging(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(ADataProvider: IJvDataProvider;
      AReason: TDataProviderChangeReason; Source: IUnknown);
  protected
    function GetContexts: IJvDataContexts; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ClientProvider: IJvDataProvider read FClientProvider write SetClientProvider;
  end;

  TContextItem = class(TJvBaseDataItem, IJvDataItemText, IJvDataContextItem)
  private
    FContext: IJvDataContext;
    { IContextItem methods }
    function GetContext: IJvDataContext;
    { IJvDataItemText methods }
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    function Editable: Boolean;
  protected
    procedure InitID; override;
    function IsDeletable: Boolean; override;
    constructor CreateCtx(AOwner: IJvDataItems; AContext: IJvDataContext);
  public
    property Context: IJvDataContext read GetContext;
  end;

  TContextItemsManager = class(TJvBaseDataItemsManagement)
  protected
    function GetContexts: IJvDataContexts;
    { IJvDataItemManagement methods }
    function Add(Item: IJvDataItem): IJvDataItem; override;
    function New: IJvDataItem; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Remove(var Item: IJvDataItem); override;
  end;

//=== { TContextItems } ======================================================

function TContextItems.GetContexts: IJvDataContexts;
var
  ParentCtx: IJvDataContext;
begin
  if GetParent <> nil then
  begin
    if Supports(GetParent, IJvDataContext, ParentCtx) then
      Supports(ParentCtx, IJvDataContexts, Result);
  end
  else
    Result := nil;
end;

function TContextItems.Find(Context: IJvDataContext; const Recursive: Boolean = False): IJvDataItem;
var
  CtxStack: array of IJvDataContext;
  CtxIdx: Integer;
begin
  if Context <> nil then
  begin
    if Context.Contexts = GetContexts then
      Result := TContextItem.CreateCtx(Self, Context)
    else
    if Recursive then
    begin
      SetLength(CtxStack, 128); // reserve some space; should be enough for most situations
      CtxIdx := 0;
      while (Context <> nil) and (Context.Contexts <> GetContexts) do
      begin
        if CtxIdx = Length(CtxStack) then
          SetLength(CtxStack, CtxIdx + 128);
        CtxStack[CtxIdx] := Context;
        Inc(CtxIdx);
        Context := Context.Contexts.Ancestor;
      end;
      if Context <> nil then
      begin
        // unwind the stack to create the actual data item
        Result := TContextItem.CreateCtx(Self, Context);
        Dec(CtxIdx);
        while (CtxIdx >= 0) do
        begin
          Result := TContextItem.CreateCtx(Result.GetItems, CtxStack[CtxIdx]);
          Dec(CtxIdx);
        end;
      end;
    end;
  end;
end;

function TContextItems.FindByName(Name: string; const Recursive: Boolean = False): IJvDataItem;
var
  CtxList: IJvDataContexts;
  Ctx: IJvDataContext;
  I: Integer;
  CtxSubList: IJvDataContexts;
begin
  //TODO: Recursive only checks one level deep!!
  CtxList := GetContexts;
  if CtxList <> nil then
  begin
    Ctx := CtxList.GetContextByName(Name);
    if (Ctx = nil) and (Recursive) then
    begin
      I := 0;
      while I <= CtxList.GetCount do
      begin
        Ctx := CtxList.GetContext(I);
        if Supports(Ctx, IJvDataContexts, CtxSubList) then
        begin
          Ctx := CtxSubList.GetContextByName(Name);
          if Ctx <> nil then
            Break;
        end
        else
          Ctx := nil;
        Inc(I);
      end;
    end;
    if Ctx <> nil then
      Result := TContextItem.CreateCtx(Self, Ctx);
  end;
end;

procedure TContextItems.InitImplementers;
var
  CtxList: IJvDataContexts;
  CtxMan: IJvDataContextsManager;
begin
  CtxList := GetContexts;
  if (CtxList <> nil) and Supports(CtxList, IJvDataContextsManager, CtxMan) then
    TContextItemsManager.Create(Self);
end;

function TContextItems.GetCount: Integer;
var
  ParentCtxList: IJvDataContexts;
begin
  ParentCtxList := GetContexts;
  if ParentCtxList <> nil then
    Result := ParentCtxList.GetCount
  else
    Result := 0;
end;

function TContextItems.GetItem(I: Integer): IJvDataItem;
var
  CtxList: IJvDataContexts;
begin
  CtxList := GetContexts;
  if CtxList <> nil then
    Result := TContextItem.CreateCtx(Self, CtxList.GetContext(I));
end;

//=== { TContextRootItems } ==================================================

constructor TContextRootItems.Create;
begin
  inherited Create;
  FNotifier := TJvProviderNotification.Create;
  FNotifier.OnChanging := DataProviderChanging;
  FNotifier.OnChanged := DataProviderChanged;
end;

destructor TContextRootItems.Destroy;
begin
  FreeAndNil(FNotifier);
  inherited Destroy;
end;

procedure TContextRootItems.SetClientProvider(Value: IJvDataProvider);
begin
  if Value <> FClientProvider then
  begin
    GetProvider.Changing(pcrFullRefresh, nil);
    FClientProvider := Value;
    FNotifier.Provider := Value;
    ClearIntfImpl;
    if Value <> nil then
      InitImplementers;
    GetProvider.Changed(pcrFullRefresh, nil);
  end;
end;

procedure TContextRootItems.DataProviderChanging(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  CtxItem: IJvDataItem;
  ParentList: IJvDataItems;
begin
  case AReason of
    pcrDestroy:
      ClientProvider := nil;
    pcrContextAdd:
      begin
        { Source contains the IJvDataContext where the context is added to or nil if the new
          context is added to the root. }
        if Source <> nil then
        begin
          CtxItem := Find(IJvDataContext(Source), True);
          if CtxItem <> nil then
          begin
            if not Supports(CtxItem, IJvDataItems, ParentList) then
              ParentList := Self;
          end
          else
            ParentList := Self;
        end
        else
          ParentList := Self;
        GetProvider.Changing(pcrAdd, ParentList);
      end;
    pcrContextDelete:
      begin
        { Source is the IJvDataContext that is about to be destroyed. }
        CtxItem := Find(IJvDataContext(Source), True);
        GetProvider.Changing(pcrDelete, CtxItem);
      end;
    pcrContextUpdate:
      begin
        { Source is the IJvDataContext that is about to be changed. }
        CtxItem := Find(IJvDataContext(Source), True);
        GetProvider.Changing(pcrUpdateItem, CtxItem);
      end;
  end;
end;

procedure TContextRootItems.DataProviderChanged(ADataProvider: IJvDataProvider;
  AReason: TDataProviderChangeReason; Source: IUnknown);
var
  CtxItem: IJvDataItem;
  ParentList: IJvDataItems;
begin
  case AReason of
    pcrContextAdd:
      begin
        { Source contains the IJvDataContext that was just added. }
        CtxItem := Find(IJvDataContext(Source), True);
        GetProvider.Changed(pcrAdd, CtxItem);
      end;
    pcrContextDelete:
      begin
        { Source is the IJvDataContext from which the item was just removed or nil if the removed
          context was at the root. }
        if Source <> nil then
        begin
          CtxItem := Find(IJvDataContext(Source), True);
          if CtxItem <> nil then
          begin
            if not Supports(CtxItem, IJvDataItems, ParentList) then
              ParentList := Self;
          end
          else
            ParentList := Self;
        end
        else
          ParentList := Self;
        GetProvider.Changed(pcrDelete, ParentList);
      end;
    pcrContextUpdate:
      begin
        { Source is the IJvDataContext that has changed. }
        CtxItem := Find(IJvDataContext(Source), True);
        GetProvider.Changed(pcrUpdateItem, CtxItem);
      end;
  end;
end;

function TContextRootItems.GetContexts: IJvDataContexts;
var
  ParentCtx: IJvDataContext;
begin
  if GetParent <> nil then
  begin
    if Supports(GetParent, IJvDataContext, ParentCtx) then
      Supports(ParentCtx, IJvDataContexts, Result);
  end
  else
    Supports(ClientProvider, IJvDataContexts, Result);
end;

//=== { TContextItem } =======================================================

constructor TContextItem.CreateCtx(AOwner: IJvDataItems; AContext: IJvDataContext);
begin
  Create(AOwner);
  FContext := AContext;
end;

function TContextItem.GetContext: IJvDataContext;
begin
  Result := FContext;
end;

function TContextItem.GetCaption: string;
begin
  if Context <> nil then
    Result := Context.Name
  else
    Result := RsContextItemEmptyCaption;
end;

procedure TContextItem.SetCaption(const Value: string);
var
  CtxMan: IJvDataContextManager;
begin
  if Context <> nil then
  begin
    if Supports(Context, IJvDataContextManager, CtxMan) then
    begin
      if Context.Name <> Value then
      begin
        GetItems.GetProvider.Changing(pcrUpdateItem, Self as IJvDataItem);
        CtxMan.SetName(Value);
        GetItems.GetProvider.Changed(pcrUpdateItem, Self as IJvDataItem);
      end;
    end;
  end
  else
    raise EJVCLException.CreateRes(@RsENoContextAssigned);
end;

function TContextItem.Editable: Boolean;
begin
  Result := True;
end;

procedure TContextItem.InitID;
var
  S: string;
  Ctx: IJvDataContext;
begin
  S := GetContext.Name;
  Ctx := GetContext.Contexts.Ancestor;
  while Ctx <> nil do
  begin
    S := Ctx.Name + '\' + S;
    Ctx := Ctx.Contexts.Ancestor;
  end;
  SetID('CTX:' + S);
end;

function TContextItem.IsDeletable: Boolean;
begin
  if GetContext <> nil then
    Result := GetContext.IsDeletable
  else
    Result := True;
end;

//=== { TContextItemsManager } ===============================================

function TContextItemsManager.GetContexts: IJvDataContexts;
var
  ICI: IJvDataContextItems;
begin
  if Supports(Items, IJvDataContextItems, ICI) then
    Result := ICI.GetContexts
  else
    Result := nil;
end;

function TContextItemsManager.Add(Item: IJvDataItem): IJvDataItem;
var
  Contexts: IJvDataContexts;
  Mngr: IJvDataContextsManager;
  CtxItem: IJvDataContextItem;
begin
  Contexts := GetContexts;
  if (Contexts <> nil) and Supports(Contexts, IJvDataContextsManager, Mngr) then
  begin
    if Supports(Item, IJvDataContextItem, CtxItem) then
      Result := Item
    else
      raise EJVCLException.CreateRes(@RsENoContextItem);
  end;
end;

function TContextItemsManager.New: IJvDataItem;
var
  Contexts: IJvDataContexts;
  Mngr: IJvDataContextsManager;
begin
  Contexts := GetContexts;
  if (Contexts <> nil) and Supports(Contexts, IJvDataContextsManager, Mngr) then
    Result := Add(TContextItem.CreateCtx(Items, Mngr.New));
end;

procedure TContextItemsManager.Clear;
var
  Contexts: IJvDataContexts;
  Mngr: IJvDataContextsManager;
begin
  Contexts := GetContexts;
  if (Contexts <> nil) and Supports(Contexts, IJvDataContextsManager, Mngr) then
    Mngr.Clear;
end;

procedure TContextItemsManager.Delete(Index: Integer);
var
  Item: IJvDataItem;
begin
  Item := Items.GetItem(Index);
  if Item <> nil then
    Remove(Item);
end;

procedure TContextItemsManager.Remove(var Item: IJvDataItem);
var
  Contexts: IJvDataContexts;
  Mngr: IJvDataContextsManager;
  CtxItem: IJvDataContextItem;
  Ctx: IJvDataContext;
begin
  Contexts := GetContexts;
  if (Contexts <> nil) and Supports(Contexts, IJvDataContextsManager, Mngr) then
  begin
    if Supports(Item, IJvDataContextItem, CtxItem) then
    begin
      Ctx := CtxItem.GetContext;
      Item := nil;
      CtxItem := nil;
      Mngr.Delete(Ctx);
    end;
  end;
end;

//=== { TJvContextProvider } =================================================

function TJvContextProvider.GetProviderIntf: IJvDataProvider;
begin
  Result := TContextRootItems(DataItemsImpl).ClientProvider;
end;

procedure TJvContextProvider.SetProviderIntf(Value: IJvDataProvider);
begin
  if Value <> ProviderIntf then
    TContextRootItems(DataItemsImpl).ClientProvider := Value;
end;

function TJvContextProvider.GetProviderComp: TComponent;
var
  ICR: IInterfaceComponentReference;
begin
  if Supports(ProviderIntf, IInterfaceComponentReference, ICR) then
    Result := ICR.GetComponent
  else
    Result := nil;
end;

procedure TJvContextProvider.SetProviderComp(Value: TComponent);
var
  PI: IJvDataProvider;
  ICR: IInterfaceComponentReference;
begin
  if (Value = nil) or Supports(Value, IJvDataProvider, PI) then
  begin
    if (Value = nil) or Supports(Value, IInterfaceComponentReference, ICR) then
      ProviderIntf := PI
    else
      raise EJVCLException.CreateRes(@RsENotSupportedIInterfaceComponentReference);
  end
  else
    raise EJVCLException.CreateRes(@RsENotSupportedIJvDataProvider);
end;

class function TJvContextProvider.ItemsClass: TJvDataItemsClass;
begin
  Result := TContextRootItems;
end;

function TJvContextProvider.ConsumerClasses: TClassArray;
begin
  Result := inherited ConsumerClasses;
  AddToArray(Result, TJvContextProviderServerNotify);
end;

//=== { TJvContextProviderServerNotify } =====================================

procedure TJvContextProviderServerNotify.ItemSelected(Value: IJvDataItem);
var
  CtxItem: IJvDataContextItem;
  Ctx: IJvDataContext;
  I: Integer;
  ConCtx: IJvDataConsumerContext;
begin
  // First we allow the default behavior to take place
  inherited ItemSelected(Value);
  // Now we find out which context is selected and update the linked client consumers accordingly.
  if Supports(Value, IJvDataContextItem, CtxItem) then
    Ctx := CtxItem.GetContext
  else
    Ctx := nil;
  for I := 0 to Clients.Count - 1 do
    if Supports(Clients[I], IJvDataConsumerContext, ConCtx) then
      ConCtx.SetContext(Ctx);
end;

function TJvContextProviderServerNotify.IsValidClient(Client: IJvDataConsumerClientNotify): Boolean;
var
  ClientProv: IJvDataProvider;
  ConsumerProv: IJvDataConsumerProvider;
begin
  { Only allow client consumers who's Provider points to the ClientProvider of the context
    provider this consumer is linked to. }
  ClientProv := (ConsumerImpl.ProviderIntf as IJvDataContextProvider).ClientProvider;
  Result := Supports(Client, IJvDataConsumerProvider, ConsumerProv) and
    (ConsumerProv.GetProvider = ClientProv);
end;

initialization
  RegisterClasses([TJvContextProviderServerNotify]);
  
end.
