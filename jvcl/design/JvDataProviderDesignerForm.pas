{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderDesignerForm.pas, released on 2003-06-27.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Peter Thörnqvist

Last Modified: 2003-06-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderDesignerForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, ImgList, ActnList, Menus, ExtCtrls, ToolWin,
  {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JvDataProvider, JvDataProviderItemDesign, JvDataProviderImpl;

type
  TfrmDataProviderDesigner = class(TForm)
    lvProvider: TListView;
    alProviderEditor: TActionList;
    aiAddItem: TAction;
    aiDeleteItem: TAction;
    aiClearSub: TAction;
    pmProviderEditor: TPopupMenu;
    miAddItem: TMenuItem;
    miDeleteItem: TMenuItem;
    miClearSub: TMenuItem;
    ilActions: TImageList;
    miDivider1: TMenuItem;
    pmToolbar: TPopupMenu;
    miTextLabels: TMenuItem;
    tbrActions: TToolBar;
    tbAddItem: TToolButton;
    tbDivider1: TToolButton;
    tbDeleteItem: TToolButton;
    tbClearSub: TToolButton;
    pnlSpacer: TPanel;
    pmAddMenu: TPopupMenu;
    procedure lvProviderData(Sender: TObject; Item: TListItem);
    procedure lvProviderCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvProviderSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvProviderDblClick(Sender: TObject);
    procedure lvProviderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aiAddItemExecute(Sender: TObject);
    procedure aiDeleteItemExecute(Sender: TObject);
    procedure aiClearSubExecute(Sender: TObject);
    procedure lvProviderResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miTextLabelsClick(Sender: TObject);
  private
    { Private declarations }
    FProvider: IJvDataProvider;
    FDesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF};
    FOrgSelect: IDesignerSelections;
    FPropView: TJvDataProviderItem;
    FRootItem: TJvBaseDataItem;
    FConsumerSvc: TJvDataConsumer;
    function GetViewList: IJvDataConsumerViewList;
    function GetDataItem(Index: Integer): IJvDataItem;
    function LocateID(ID: string): Integer;
    procedure ResetSelection;
    procedure SetNewSelection(AnItem: IJvDataItem);
    procedure ConsumerChanged(Sender: TObject);
    procedure UpdateLV;
    procedure UpdateColumnSize;
    procedure UpdateSelectedItem;
    procedure SelectItemID(ID: string);
    procedure SetProvider(Value: IJvDataProvider);
    procedure SetDesigner(Value: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
    property ConsumerSvc: TJvDataConsumer read FConsumerSvc;
    property ViewList: IJvDataConsumerViewList read GetViewList;
  public
    { Public declarations }
    PropName: string;
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    property Provider: IJvDataProvider read FProvider write SetProvider;
    property Designer: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF} read FDesigner write SetDesigner;
  end;

procedure DesignProvider(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);

implementation

{$R *.DFM}

uses
  Commctrl,
  JvDsgnConsts, JvConsts, JvTypes;

const
  vifHasChildren = Integer($80000000);
  vifCanHaveChildren = Integer($40000000);
  vifExpanded = Integer($20000000);
  vifHasMan = Integer($10000000);
  vifHasDsgn = Integer($08000000);

var
  DesignerList: TList;

type
  TJvProviderRootItem = class(TJvBaseDataItem)
  protected
    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;
    procedure InitID; override;
  public
    function GetInterface(const IID: TGUID; out Obj): Boolean; override;
  end;

function TJvProviderRootItem._AddRef: Integer;
begin
  Result := -1;
end;

function TJvProviderRootItem._Release: Integer;
begin
  Result := -1;
end;

procedure TJvProviderRootItem.InitID;
begin
  SetID(SDataItemRootID);
end;

function TJvProviderRootItem.GetInterface(const IID: TGUID; out Obj): Boolean;
begin
  Result := inherited GetInterface(IID, Obj);
  if not Result then
    Result := TExtensibleInterfacedPersistent(Items.GetImplementer).GetInterface(IID, Obj);
end;

procedure DesignProvider(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);
var
  DesignerForm: TfrmDataProviderDesigner;
  I: Integer;
begin
  if DesignerList = nil then
    DesignerList := TList.Create;

  I := DesignerList.Count - 1;
  while (I >= 0) do
  begin
    DesignerForm := TfrmDataProviderDesigner(DesignerList[I]);
    if (DesignerForm.Provider = AProvider) and (DesignerForm.Designer = ADesigner) then
    begin
      DesignerForm.Show;
      DesignerForm.BringToFront;
      Exit;
    end;
    Dec(I);
  end;
  DesignerForm := TfrmDataProviderDesigner.Create(nil);
  try
    DesignerForm.PropName := PropName;
    DesignerForm.Provider := AProvider;
    DesignerForm.Designer := ADesigner;
    DesignerForm.Show;
  except
    DesignerForm.Free;
  end;
end;

function GetItemIndexAt(LV: TListView; X, Y: Integer): Integer;
var
  Info: TLVHitTestInfo;
begin
  if LV.HandleAllocated then
  begin
    Info.pt := Point(X, Y);
    Result := ListView_HitTest(LV.Handle, Info);
  end
  else
    Result := -1;
end;

function TfrmDataProviderDesigner.GetViewList: IJvDataConsumerViewList;
begin
  Supports(ConsumerSvc as IJvDataConsumer, IJvDataConsumerViewList, Result);
end;

function TfrmDataProviderDesigner.GetDataItem(Index: Integer): IJvDataItem;
begin
  if Index = 0 then
    Result := FRootItem
  else
    Result := ViewList.Item(Index - 1);
end;

function TfrmDataProviderDesigner.LocateID(ID: string): Integer;
begin
  if AnsiSameText(ID, (FRootItem as IJvDataItem).GetID) then
    Result := 0
  else
  begin
    Result := ViewList.IndexOfID(ID);
    if Result >= 0 then
      Inc(Result);
  end;
end;

procedure TfrmDataProviderDesigner.ResetSelection;
begin
  if (Designer <> nil) and (FOrgSelect <> nil) then
    Designer.SetSelections(FOrgSelect);
  if FPropView <> nil then
    FreeAndNil(FPropView);
end;

procedure TfrmDataProviderDesigner.SetNewSelection(AnItem: IJvDataItem);
begin
  if FPropView <> nil then
    FreeAndNil(FPropView);
  FPropView := TJvDataProviderItem.Create(AnItem);
  if Designer <> nil then
    Designer.SelectComponent(FPropView);
end;

procedure TfrmDataProviderDesigner.ConsumerChanged(Sender: TObject);
begin
  UpdateLV;
end;

procedure TfrmDataProviderDesigner.UpdateLV;
begin
  if ViewList = nil then
    lvProvider.Items.Count := 0
  else
    lvProvider.Items.Count := ViewList.Count + 1;
  lvProvider.Invalidate;
end;

procedure TfrmDataProviderDesigner.UpdateColumnSize;
begin
  lvProvider.Columns[0].Width := lvProvider.ClientWidth;
end;

procedure TfrmDataProviderDesigner.UpdateSelectedItem;
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
    Result.OnClick := aiAddItem.OnExecute;
    Result.Tag := Idx;
  end;

begin
  if lvProvider.SelCount <> 0 then
  begin
    Item := GetDataItem(lvProvider.Selected.Index);
    if (Item <> nil) and Supports(Item, IJvDataItems, Items) then
    begin
      if Supports(Items, IJvDataItemsManagement, Man) then
        Supports(Items, IJvDataItemsDesigner, Dsgn);
    end;
    if (Item <> nil) then
      Item.Items.QueryInterface(IJvDataItemsManagement, ParentMan);
  end
  else
  begin
    if Supports(Provider, IJvDataItems, Items) then
    begin
      if Supports(Items, IJvDataItemsManagement, Man) then
        Supports(Items, IJvDataItemsDesigner, Dsgn);
    end;
  end;

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
    miAddItem.Action := aiAddItem;
    tbAddItem.Action := aiAddItem;
    tbAddItem.Style := tbsButton;
  end
  else
  begin
    miAddItem.Action := nil;
    miAddItem.OnClick := nil;
    tbAddItem.Action := nil;
    tbAddItem.OnClick := nil;
    tbAddItem.Style := tbsDropDown;
    for I := 0 to Dsgn.GetCount - 1 do
    begin
      miAddItem.Add(MakeMenuItem(I, miAddItem));
      pmAddMenu.Items.Add(MakeMenuItem(I, pmAddMenu));
    end;
    miAddItem.Visible := Man <> nil;
    miAddItem.Enabled := (Man <> nil) and (Items <> nil);
    tbAddItem.Visible := miAddItem.Visible;
    tbAddItem.Enabled := miAddItem.Enabled;
  end;
  aiAddItem.Enabled := (Man <> nil) and (Items <> nil);
  aiDeleteItem.Enabled := (ParentMan <> nil) and (Item <> nil) and
    (Item.GetImplementer <> FRootItem);
  aiClearSub.Enabled := (Man <> nil) and (Items <> nil) and (Items.Count > 0);
end;

procedure TfrmDataProviderDesigner.SelectItemID(ID: string);
var
  Idx: Integer;
begin
  Idx := LocateID(ID);
  if Idx > -1 then
    ListView_SetItemState(lvProvider.Handle, Idx, LVIS_SELECTED or LVIS_FOCUSED,
      LVIS_SELECTED or LVIS_FOCUSED);
end;

procedure TfrmDataProviderDesigner.SetProvider(Value: IJvDataProvider);
var
  ProviderImpl: TComponent;
begin
  ConsumerSvc.SetProviderIntf(Value);
  FProvider := Value;
  if Provider <> nil then
  begin
    FRootItem := TJvProviderRootItem.Create(Provider as IJvDataItems);
    if ViewList <> nil then
      ViewList.RebuildView;
    ProviderImpl := (Provider as IInterfaceComponentReference).GetComponent;
    Caption := Format(SDataProviderDesignerCaption, [ProviderImpl.Name, '.' + PropName]);
  end;
  UpdateLV;
  UpdateSelectedItem;
end;

procedure TfrmDataProviderDesigner.SetDesigner(
  Value: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
begin
  if Value <> FDesigner then
  begin
    if FDesigner <> nil then
      ResetSelection;
    {$IFDEF COMPILER6_UP}
    FOrgSelect := TDesignerSelections.Create;
    {$ELSE}
    FOrgSelect := TDesignerSelectionList.Create;
    {$ENDIF}
    FDesigner := Value;
    if Designer <> nil then
      Designer.GetSelections(FOrgSelect);
  end;
end;

constructor TfrmDataProviderDesigner.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FConsumerSvc := TJvDataConsumer.Create(Self, [DPA_RenderDisabledAsGrayed, DPA_ConsumerDisplaysList]);
  FConsumerSvc.OnChanged := ConsumerChanged;
end;

destructor TfrmDataProviderDesigner.Destroy;
begin
  FreeAndNil(FConsumerSvc);
  inherited Destroy;
end;

procedure TfrmDataProviderDesigner.lvProviderData(Sender: TObject; Item: TListItem);
var
  DataItem: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if (ConsumerSvc.ProviderIntf = nil) or (Item.Index > ViewList.Count) then
    Exit;
  if Item.Index = 0 then
  begin
    DataItem := FRootItem;
    Item.Indent := 0;
  end
  else
  begin
    DataItem := ViewList.Item(Item.Index - 1);
    Item.Indent := ViewList.ItemLevel(Item.Index - 1) + 1;
  end;
  if DataItem <> nil then
  begin
    if Supports(DataItem, IJvDataItemText, ItemText) then
      Item.Caption := ItemText.Caption
    else
    begin
      if DataItem.GetImplementer = FRootItem then
        Item.Caption := SDataItemRootCaption
      else
        Item.Caption := SDataItemNoTextIntf;
    end;
  end
end;

procedure TfrmDataProviderDesigner.lvProviderCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ACanvas: TCanvas;
  ARect: TRect;
  BtnWdth: Integer;
  MidX, MidY: Integer;
begin
  ACanvas := Sender.Canvas;
  DefaultDraw := False;
  ARect := Item.DisplayRect(drBounds);
  ARect.Right := Sender.ClientRect.Right;
  if Item.Selected then
  begin
    ACanvas.Brush.Color := clHighlight;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Font.Color := clHighlightText;
    ACanvas.FillRect(ARect);
  end
  else
  begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.Brush.Style := bsSolid;
    ACanvas.Font.Color := clWindowText;
    ACanvas.FillRect(ARect);
  end;
  BtnWdth := Succ(ARect.Bottom - ARect.Top) + 2;
  ARect.Left := ARect.Left + (BtnWdth * Item.Indent);
  if (Item.Index = 0) or ViewList.ItemHasChildren(Item.Index - 1) then
  begin
    with ACanvas do
    begin
      MidX := ARect.Left + (BtnWdth - 3) div 2;
      MidY := ARect.Top + (BtnWdth - 3) div 2;
      Pen.Color := ACanvas.Font.Color;
      Pen.Style := psSolid;
      Pen.Width := 1;
      MoveTo(ARect.Left + 3, ARect.Top + 3);
      LineTo(ARect.Left + BtnWdth - 6, ARect.Top + 3);
      LineTo(ARect.Left + BtnWdth - 6, ARect.Top + BtnWdth - 6);
      LineTo(ARect.Left + 3, ARect.Top + BtnWdth - 6);
      LineTo(ARect.Left + 3, ARect.Top + 3);

      MoveTo(ARect.Left + 5, MidY);
      LineTo(ARect.Left + BtnWdth - 7, MidY);

      if (Item.Index <> 0) and not ViewList.ItemIsExpanded(Item.Index - 1) then
      begin
        MoveTo(MidX, ARect.Top + 5);
        LineTo(MidX, ARect.Top + BtnWdth - 7);
      end;
    end;
  end;
  ARect.Left := ARect.Left + BtnWdth;
  DrawText(ACanvas.Handle, PChar(Item.Caption), Length(Item.Caption), ARect, DT_SINGLELINE + DT_LEFT + DT_END_ELLIPSIS);
end;

procedure TfrmDataProviderDesigner.lvProviderSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelectedItem;
end;

procedure TfrmDataProviderDesigner.lvProviderDblClick(Sender: TObject);
begin
  if lvProvider.Selected <> nil then
    if lvProvider.Selected.Index > 0 then
      ViewList.ToggleItem(lvProvider.Selected.Index - 1);
end;

procedure TfrmDataProviderDesigner.lvProviderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: Integer;
  ItemLevel: Integer;
  TmpRect: TRect;
begin
  Item := GetItemIndexAt(lvProvider, X, Y);
  if Item <> -1 then
  begin
    if Item = 0 then
      ItemLevel := 0
    else
      ItemLevel := ViewList.ItemLevel(Item - 1) + 1;
    ListView_GetItemRect(lvProvider.Handle, Item, TmpRect, LVIR_BOUNDS);
    TmpRect.Right := TmpRect.Left + (Succ((TmpRect.Bottom - TmpRect.Top) + 2) * Succ(ItemLevel));
    if (X < TmpRect.Right) and (X > TmpRect.Right - ((TmpRect.Bottom - TmpRect.Top) + 2)) then
      if Item > 0 then
        ViewList.ToggleItem(Item - 1);
  end;
end;

procedure TfrmDataProviderDesigner.aiAddItemExecute(Sender: TObject);
var
  Item: IJvDataItem;
  Items: IJvDataItems;
  Dsgn: IJvDataItemsDesigner;
  Mangr: IJvDataItemsManagement;
begin
  if lvProvider.Selected <> nil then
  begin
    Item := GetDataItem(lvProvider.Selected.Index);
    if Item <> nil then
      Item.QueryInterface(IJvDataItems, Items)
    else // should never occur
      raise EJVCLException.Create(SDataItemNotFound);
  end
  else
    Items := Provider as IJvDataItems;
  Item := nil;
  if Items <> nil then
  begin
    if Supports(Items, IJvDataItemsDesigner, Dsgn) then
      Item := Dsgn.NewByKind(TMenuItem(Sender).Tag)
    else if Supports(Items, IJvDataItemsManagement, Mangr) then
      Item := Mangr.New
    else // should never occur
      raise EJVCLException.CreateFmt(SDataProviderAddErrorReason, [SDataProviderNoManOrDsgn]);
    if Item <> nil then
      SelectItemID(Item.GetID)
    else
      raise EJVCLException.Create(SDataProviderAddFailed);
  end
  else // should never occur
    raise EJVCLException.CreateFmt(SDataProviderAddErrorReason, [SDataProviderNoSubItems]);
end;

procedure TfrmDataProviderDesigner.aiDeleteItemExecute(Sender: TObject);
var
  I: Integer;
  Item: IJvDataItem;
  Items: IJvDataItems;
  Mangr: IJvDataItemsManagement;
begin
  if lvProvider.Selected <> nil then
  begin
    I := lvProvider.Selected.Index;
    Item := GetDataItem(I);
    if Item <> nil then
      Items := Item.Items
    else
      raise EJVCLException.Create(SDataItemNotFound);
    if Supports(Items, IJvDataItemsManagement, Mangr) then
    begin
      ResetSelection;
      Mangr.Remove(Item);
      Pointer(Item) := nil;
    end
    else
      raise EJVCLException.CreateFmt(SDataProviderDeleteErrorReason, [SDataProviderNoMan]);
    UpdateSelectedItem;
  end;
end;

procedure TfrmDataProviderDesigner.aiClearSubExecute(Sender: TObject);
var
  Item: IJvDataItem;
  Items: IJvDataItems;
  Mangr: IJvDataItemsManagement;
begin
  if lvProvider.Selected <> nil then
  begin
    Item := GetDataItem(lvProvider.Selected.Index);
    if Item <> nil then
    begin
      if not Supports(Item, IJvDataItems, Items) then
        raise EJVCLException.CreateFmt(SDataProviderDeleteErrorReason, [SDataProviderNoSubItems]);
    end
    else
      raise EJVCLException.Create(SDataItemNotFound);
    if Supports(Items, IJvDataItemsManagement, Mangr) then
      Mangr.Clear
    else
      raise EJVCLException.CreateFmt(SDataProviderDeleteErrorReason, [SDataProviderNoMan]);
  end;
end;

procedure TfrmDataProviderDesigner.lvProviderResize(Sender: TObject);
begin
  UpdateColumnSize;
end;

procedure TfrmDataProviderDesigner.FormCreate(Sender: TObject);
begin
  if DesignerList.IndexOf(Self) = -1 then
    DesignerList.Add(Self);
end;

procedure TfrmDataProviderDesigner.FormDestroy(Sender: TObject);
begin
  ResetSelection;
  Provider := nil;
  Designer := nil;
  DesignerList.Remove(Self);
end;

procedure TfrmDataProviderDesigner.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmDataProviderDesigner.miTextLabelsClick(Sender: TObject);
begin
  if TMenuItem(Sender).Checked then
  begin
    tbrActions.ShowCaptions := False;
    tbrActions.ButtonWidth := 22;
    tbrActions.ButtonHeight := 22;
  end
  else
    tbrActions.ShowCaptions := True;
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

end.
