{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderDesignerForm.pas, released on --.

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

unit JvDataProviderDesignerForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, ImgList, ToolWin, ComCtrls, StdCtrls, ExtCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvBaseDsgnForm, JvDataProvider, JvDataProviderItemDesign, JvDataProviderIntf,
  JvProviderTreeListFrame, JvBaseDsgnFrame, JvBaseDsgnToolbarFrame,
  JvStdToolbarDsgnFrame, JvProviderToolbarFrame, JvDsgnTypes;

type
  TfrmDataProviderDesigner = class(TJvBaseDesign)
    pmProviderEditor: TPopupMenu;
    miAddItem: TMenuItem;
    miDivider1: TMenuItem;
    miDeleteItem: TMenuItem;
    miClearSub: TMenuItem;
    pmAddMenu: TPopupMenu;
    miDivider2: TMenuItem;
    miShowToolbar: TMenuItem;
    fmeToolbar: TfmeJvProviderToolbar;
    fmeTreeList: TfmeJvProviderTreeList;
    procedure aiAddItemExecute(Sender: TObject);
    procedure aiDeleteItemExecute(Sender: TObject);
    procedure aiClearSubExecute(Sender: TObject);
    procedure cbContextsChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDesigner: IJvFormDesigner;
  protected
    FOrgSelect: IDesignerSelections;
    FPropView: TJvDataProviderItem;
    FRootItem: IJvDataItem;
    procedure ResetSelection;
    procedure SetNewSelection(AnItem: IJvDataItem);
    procedure NeedRoot(Sender: TObject; var AVirtualRoot: IJvDataItem); 
    procedure UpdateSelectedItem(Sender: TObject);
    procedure InitContexts;
    procedure InitViewList(Sender: TJvDataConsumer; SubSvc: TJvDataConsumerAggregatedObject);
    function InternalProvider: IJvDataProvider;
    function GetProvider: IJvDataProvider; virtual;
    procedure SetProvider(Value: IJvDataProvider); virtual;
    procedure SetDesigner(Value: IJvFormDesigner);
    procedure Loaded; override;
    function DesignerFormName: string; override;
    function AutoStoreSettings: Boolean; override;
  public
    PropName: string;
    destructor Destroy; override;
    property Provider: IJvDataProvider read GetProvider write SetProvider;
    property Designer: IJvFormDesigner read FDesigner write SetDesigner;
  end;

procedure DesignProvider(AProvider: IJvDataProvider;
  ADesigner: IJvFormDesigner; PropName: string);

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF VCL}
{$IFDEF VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  CommCtrl,
  JvConsts, JvDsgnConsts, JvTypes;

function IsProviderDesignForm(Form: TJvBaseDesign; const Args: array of const): Boolean;
begin
  Result := Form is TfrmDataProviderDesigner;
  if Result then
    with Form as TfrmDataProviderDesigner do
      Result := (Pointer(Provider) = Args[0].VInterface) and
        (Pointer(Designer) = Args[1].VInterface);
end;

procedure DesignProvider(AProvider: IJvDataProvider;
  ADesigner: IJvFormDesigner; PropName: string);
var
  Form: TfrmDataProviderDesigner;
begin
  Form := TfrmDataProviderDesigner(GetDesignerForm(IsProviderDesignForm, [AProvider, ADesigner]));
  if Form = nil then
  begin
    Form := TfrmDataProviderDesigner.Create(nil);
    try
      if PropName <> '' then
        Form.PropName := '.' + PropName;
      Form.Provider := AProvider;
      Form.Designer := ADesigner;
    except
      FreeAndNil(Form);
      raise;
    end;
  end;
  Form.Show;
  Form.BringToFront;
end;

//=== { TJvProviderRootItem } ================================================

type
  TJvProviderRootItem = class(TJvBaseDataItem)
  protected
    procedure InitID; override;
  public
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

procedure TJvProviderRootItem.InitID;
begin
  SetID(RsDataItemRootID);
end;

function TJvProviderRootItem.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
    Result := TExtensibleInterfacedPersistent(GetItems.GetImplementer).GetInterface(IID, Obj);
end;

//=== { TfrmDataProviderDesigner } ===========================================

procedure TfrmDataProviderDesigner.ResetSelection;
begin
  if (Designer <> nil) and (FOrgSelect <> nil) then
    Designer.SetSelections(FOrgSelect);
  if FPropView <> nil then
    FreeAndNil(FPropView);
end;

procedure TfrmDataProviderDesigner.SetNewSelection(AnItem: IJvDataItem);
begin
  FreeAndNil(FPropView);
  FPropView := TJvDataProviderItem.Create(AnItem);
  if Designer <> nil then
    Designer.SelectComponent(FPropView);
end;

procedure TfrmDataProviderDesigner.NeedRoot(Sender: TObject; var AVirtualRoot: IJvDataItem);
begin
  AVirtualRoot := FRootItem;
end;

procedure TfrmDataProviderDesigner.UpdateSelectedItem(Sender: TObject);
var
  Item: IJvDataItem;
  Items: IJvDataItems;
  Man: IJvDataItemsManagement;
  Dsgn: IJvDataItemsDesigner;
  ParentMan: IJvDataItemsManagement;
  I: Integer;

  function MakeMenuItem(const Idx: Integer; const AOwner: TComponent): TMenuItem;
  var
    S: string;
  begin
    Dsgn.GetKind(Idx, S);
    Result := TMenuItem.Create(AOwner);
    Result.Caption := S;
    Result.OnClick := fmeToolbar.aiAddItem.OnExecute;
    Result.Tag := Idx;
  end;

begin
  if fmeTreeList.lvProvider.SelCount <> 0 then
  begin
    Item := fmeTreeList.GetDataItem(fmeTreeList.lvProvider.Selected.Index);
    if (Item <> nil) and Supports(Item, IJvDataItems, Items) then
      if Supports(Items, IJvDataItemsManagement, Man) then
        Supports(Items, IJvDataItemsDesigner, Dsgn);
    if Item <> nil then
      Item.GetItems.QueryInterface(IJvDataItemsManagement, ParentMan);
  end
  else
  if Supports(InternalProvider, IJvDataItems, Items) then
    if Supports(Items, IJvDataItemsManagement, Man) then
      Supports(Items, IJvDataItemsDesigner, Dsgn);

  // Update OI
  if Item = nil then
    ResetSelection
  else
    SetNewSelection(Item);

  // Update action states
  miAddItem.Clear;
  pmAddMenu.Items.Clear;
  if (Dsgn = nil) or (Dsgn.GetCount = 0) then
  begin
    miAddItem.Action := fmeToolbar.aiAddItem;
    fmeToolbar.tbAddItem.Action := fmeToolbar.aiAddItem;
    fmeToolbar.tbAddItem.Style := tbsButton;
  end
  else
  begin
    miAddItem.Action := nil;
    miAddItem.OnClick := nil;
    fmeToolbar.tbAddItem.Action := nil;
    fmeToolbar.tbAddItem.OnClick := nil;
    fmeToolbar.tbAddItem.Style := tbsDropDown;
    for I := 0 to Dsgn.GetCount - 1 do
    begin
      miAddItem.Add(MakeMenuItem(I, miAddItem));
      pmAddMenu.Items.Add(MakeMenuItem(I, pmAddMenu));
    end;
    miAddItem.Visible := Man <> nil;
    miAddItem.Enabled := (Man <> nil) and (Items <> nil);
    fmeToolbar.tbAddItem.Visible := miAddItem.Visible;
    fmeToolbar.tbAddItem.Enabled := miAddItem.Enabled;
  end;
  fmeToolbar.aiAddItem.Enabled := (Man <> nil) and (Items <> nil);
  fmeToolbar.aiDeleteItem.Enabled := (ParentMan <> nil) and (Item <> nil) and
    (Item <> FRootItem) and Item.IsDeletable;
  fmeToolbar.aiDeleteSubItems.Enabled := (Man <> nil) and (Items <> nil) and
    (Items.Count > 0);
end;

procedure TfrmDataProviderDesigner.InitContexts;
var
  Ctx: IJvDataContexts;
  I: Integer;
begin
  fmeToolbar.cbContexts.Items.BeginUpdate;
  try
    fmeToolbar.cbContexts.ItemIndex := -1;
    fmeToolbar.cbContexts.Items.Clear;
    fmeToolbar.cbContexts.Sorted := False;
    if (InternalProvider <> nil) and Supports(InternalProvider, IJvDataContexts, Ctx) then
    begin
      for I := 0 to Ctx.GetCount - 1 do
        fmeToolbar.cbContexts.Items.AddObject(Ctx.GetContext(I).Name, TObject(I));
      fmeToolbar.cbContexts.Sorted := True;
      fmeToolbar.cbContexts.Sorted := False;
    end;
    // (rom) TObject(-99) needs explanation.
    fmeToolbar.cbContexts.Items.InsertObject(0, RsDefault, TObject(-99));
  finally
    fmeToolbar.cbContexts.Items.EndUpdate;
    if fmeToolbar.cbContexts.Items.Count > 0 then
      fmeToolbar.cbContexts.ItemIndex := 0;
    fmeToolbar.pnlContexts.Visible := fmeToolbar.cbContexts.Items.Count > 1;
    fmeToolbar.UpdateToolbarSeparators;
  end;
end;

procedure TfrmDataProviderDesigner.InitViewList(Sender: TJvDataConsumer;
  SubSvc: TJvDataConsumerAggregatedObject);
var
  VL: IJvDataConsumerViewList;
begin
  if SubSvc is TJvCustomDataConsumerViewList then
    if SubSvc.GetInterface(IJvDataConsumerViewList, VL) then
      VL.ExpandOnNewItem := True;
end;

function TfrmDataProviderDesigner.InternalProvider: IJvDataProvider;
begin
  Result := fmeTreeList.Provider.ProviderIntf;
end;

function TfrmDataProviderDesigner.GetProvider: IJvDataProvider;
begin
  Result := fmeTreeList.Provider.ProviderIntf;
end;

procedure TfrmDataProviderDesigner.SetProvider(Value: IJvDataProvider);
var
  ViewList: IJvDataConsumerViewList;
  ProviderImpl: TComponent;
begin
  fmeTreeList.Provider.SetProviderIntf(Value);
  if csDestroying in ComponentState then
    Exit;
  if Provider <> nil then
  begin
    FRootItem := TJvProviderRootItem.Create(Provider as IJvDataItems);
    fmeTreeList.Provider.SetContextIntf(nil);
    // Odd behaviour: ViewList is not filled when the provider is set. Look into this issue later!
    if Supports(fmeTreeList.Provider as IJvDataConsumer, IJvDataConsumerViewList, ViewList) then
      ViewList.RebuildView;
    ProviderImpl := (Provider as IInterfaceComponentReference).GetComponent;
    Caption := Format(RsDataProviderDesignerCaption, [ProviderImpl.Name, PropName]);
  end;
  InitContexts;
  UpdateSelectedItem(Self);
end;

procedure TfrmDataProviderDesigner.SetDesigner(Value: IJvFormDesigner);
begin
  if Value <> FDesigner then
  begin
    if FDesigner <> nil then
      ResetSelection;
    {$IFDEF COMPILER6_UP}
    FOrgSelect := TDesignerSelections.Create;
    {$ELSE}
    FOrgSelect := TDesignerSelectionList.Create;
    {$ENDIF COMPILER6_UP}
    FDesigner := Value;
    if Designer <> nil then
      Designer.GetSelections(FOrgSelect);
  end;
end;

procedure TfrmDataProviderDesigner.Loaded;
begin
  inherited Loaded;
  if fmeTreeList <> nil then
    with fmeTreeList do
    begin
      OnGetVirtualRoot := NeedRoot;
      OnItemSelect := UpdateSelectedItem;
      Provider.AfterCreateSubSvc := InitViewList;
      UseVirtualRoot := True;
    end;
end;

function TfrmDataProviderDesigner.DesignerFormName: string;
begin
  Result := RsDataProviderDesigner;
end;

function TfrmDataProviderDesigner.AutoStoreSettings: Boolean;
begin
  Result := True;
end;

destructor TfrmDataProviderDesigner.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmDataProviderDesigner.aiAddItemExecute(Sender: TObject);
var
  Item: IJvDataItem;
  Items: IJvDataItems;
  Dsgn: IJvDataItemsDesigner;
  Mangr: IJvDataItemsManagement;
begin
  if fmeTreeList.lvProvider.Selected <> nil then
  begin
    Item := fmeTreeList.GetDataItem(fmeTreeList.lvProvider.Selected.Index);
    if Item <> nil then
      Item.QueryInterface(IJvDataItems, Items)
    else // should never occur
      raise EJVCLException.CreateRes(@RsEDataItemNotFound);
  end
  else
    Items := InternalProvider as IJvDataItems;
  Item := nil;
  if Items <> nil then
  begin
    if Supports(Items, IJvDataItemsDesigner, Dsgn) then
      Item := Dsgn.NewByKind(TMenuItem(Sender).Tag)
    else
    if Supports(Items, IJvDataItemsManagement, Mangr) then
      Item := Mangr.New
    else // should never occur
      raise EJVCLException.CreateResFmt(@RsEDataProviderAddErrorReason, [RsEDataProviderNoManOrDsgn]);
    if Item <> nil then
    begin
      fmeTreeList.SelectItemID(Item.GetID);
      if Designer <> nil then
        Designer.Modified;
    end
    else
      raise EJVCLException.CreateRes(@RsEDataProviderAddFailed);
  end
  else // should never occur
    raise EJVCLException.CreateResFmt(@RsEDataProviderAddErrorReason, [RsEDataProviderNoSubItems]);
end;

procedure TfrmDataProviderDesigner.aiDeleteItemExecute(Sender: TObject);
var
  I: Integer;
  Item: IJvDataItem;
  Items: IJvDataItems;
  Mangr: IJvDataItemsManagement;
begin
  if fmeTreeList.lvProvider.Selected <> nil then
  begin
    I := fmeTreeList.lvProvider.Selected.Index;
    Item := fmeTreeList.GetDataItem(I);
    if Item <> nil then
      Items := Item.GetItems
    else
      raise EJVCLException.CreateRes(@RsEDataItemNotFound);
    if Supports(Items, IJvDataItemsManagement, Mangr) then
    begin
      ResetSelection;
      Mangr.Remove(Item);
      if Designer <> nil then
        Designer.Modified;
    end
    else
      raise EJVCLException.CreateResFmt(@RsEDataProviderDeleteErrorReason, [RsEDataProviderNoMan]);
  end;
end;

procedure TfrmDataProviderDesigner.aiClearSubExecute(Sender: TObject);
var
  Item: IJvDataItem;
  Items: IJvDataItems;
  Mangr: IJvDataItemsManagement;
begin
  if fmeTreeList.lvProvider.Selected <> nil then
  begin
    Item := fmeTreeList.GetDataItem(fmeTreeList.lvProvider.Selected.Index);
    if Item <> nil then
    begin
      if not Supports(Item, IJvDataItems, Items) then
        raise EJVCLException.CreateResFmt(@RsEDataProviderDeleteErrorReason, [RsEDataProviderNoSubItems]);
    end
    else
      raise EJVCLException.CreateRes(@RsEDataItemNotFound);
    if Supports(Items, IJvDataItemsManagement, Mangr) then
    begin
      Mangr.Clear;
      if Designer <> nil then
        Designer.Modified;
    end
    else
      raise EJVCLException.CreateResFmt(@RsEDataProviderDeleteErrorReason, [RsEDataProviderNoMan]);
  end;
end;

procedure TfrmDataProviderDesigner.cbContextsChange(Sender: TObject);
var
  CtxIdx: Integer;
  CtxList: IJvDataContexts;
begin
  if fmeToolbar.cbContexts.ItemIndex > 0 then
  begin
    CtxIdx := Integer(fmeToolbar.cbContexts.Items.Objects[fmeToolbar.cbContexts.ItemIndex]);
    if CtxIdx >= 0 then
    begin
      { Retrieve context and activate it in the consumer service. }
      if Supports(InternalProvider, IJvDataContexts, CtxList) then
        fmeTreeList.Provider.SetContextIntf(CtxList.GetContext(CtxIdx))
      else
        raise EJVCLException.CreateRes(@RsEInternalErrorUnableToRetrieveContext);
    end
    else
      fmeTreeList.Provider.SetContextIntf(nil);
  end
  else
    fmeTreeList.Provider.SetContextIntf(nil);
end;

procedure TfrmDataProviderDesigner.FormDestroy(Sender: TObject);
begin
  ResetSelection;
  FRootItem := nil;
  Provider := nil;
  Designer := nil;
end;

end.
