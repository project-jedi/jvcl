{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDataProviderEditor.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Peter Thörnqvist

Last Modified: 2003-06-19

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDataProviderEditorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, ImgList, ActnList, Menus, ExtCtrls, ToolWin,
  {$IFNDEF COMPILER6_UP} DsgnIntf, {$ELSE} DesignIntf, DesignEditors, {$ENDIF}
  JvDataProvider, JvDataProviderDsgn, JvDataProviderImpl;

type

  TProviderEditItem = packed record
    ID: string;
    Flags: Integer;
  end;
  TProviderEditItems = array of TProviderEditItem;

  TfrmDataProviderEditor = class(TForm, IJvDataProviderNotify)
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
    FViewItems: TProviderEditItems;
    FOrgSelect: IDesignerSelections;
    FPropView: TJvDataProviderItem;
    FRootItem: TJvBaseDataItem;
    function GetDataItem(Index: Integer): IJvDataItem;
    function LocateID(ID: string): Integer;
    procedure ResetSelection;
    procedure SetNewSelection(AnItem: IJvDataItem);
    procedure UpdateLV;
    procedure UpdateColumnSize;
    procedure UpdateSelectedItem;
    procedure ToggleItem(Index: Integer);
    procedure SelectItemID(ID: string);
    procedure DeleteItem(Index: Integer);
    procedure DeleteSubItems(Index: Integer);
    procedure AddSubItem(Index: Integer; Item: IJvDataItem);
    procedure InsertItems(var Index: Integer; Items: IJvDataItems);
    procedure SetProvider(Value: IJvDataProvider);
    procedure SetDesigner(Value: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF});
    { IJvDataProviderNotify }
    procedure DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
    procedure DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
  public
    { Public declarations }
    PropName: string;
    property Provider: IJvDataProvider read FProvider write SetProvider;
    property Designer: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF} read FDesigner write SetDesigner;
  end;

procedure EditProvider(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);

implementation

{$R *.DFM}

uses
  Commctrl, Dialogs,
  JvDataProviderDsgnConsts, JvTypes;

const
  vifHasChildren = Integer($80000000);
  vifCanHaveChildren = Integer($40000000);
  vifExpanded = Integer($20000000);
  vifHasMan = Integer($10000000);
  vifHasDsgn = Integer($08000000);

var
  EditorList: TList;

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

procedure EditProvider(AProvider: IJvDataProvider;
  ADesigner: {$IFDEF COMPILER6_UP}IDesigner{$ELSE}IFormDesigner{$ENDIF}; PropName: string);
var
  EditorForm: TfrmDataProviderEditor;
  I: Integer;
begin
  if EditorList = nil then
    EditorList := TList.Create;

  I := EditorList.Count - 1;
  while (I >= 0) do
  begin
    EditorForm := TfrmDataProviderEditor(EditorList[I]);
    if (EditorForm.Provider = AProvider) and (EditorForm.Designer = ADesigner) then
    begin
      EditorForm.Show;
      EditorForm.BringToFront;
      Exit;
    end;
    Dec(I);
  end;
  EditorForm := TfrmDataProviderEditor.Create(nil);
  try
    EditorForm.PropName := PropName;
    EditorForm.Provider := AProvider;
    EditorForm.Designer := ADesigner;
    EditorForm.Show;
  except
    EditorForm.Free;
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

function TfrmDataProviderEditor.GetDataItem(Index: Integer): IJvDataItem;
begin
  if Index = 0 then
    Result := FRootItem
  else
    Result := (Provider as IJvDataIDSearch).Find(FViewItems[Index].ID, True);
end;

function TfrmDataProviderEditor.LocateID(ID: string): Integer;
var
  Item: IJvDataItem;
begin
  Result := High(FViewItems);
  while (Result >= 0) and (FViewItems[Result].ID <> ID) do
    Dec(Result);
  if Result < 0 then
  begin
    if ID = (FRootItem as IJvDataItem).GetID then
      Item := FRootItem
    else
      Item := (Provider as IJvDataIDSearch).Find(ID, True);
    if Item <> nil then
    begin
      Item := Item.Items.Parent;
      if Item = nil then
        Item := FRootItem;
      if Item <> nil then
      begin
        Result := LocateID(Item.GetID);
        if Result > -1 then
        begin
          ToggleItem(Result);
          while (Result < High(FViewItems)) and (FViewItems[Result].ID <> ID) do
            Inc(Result);
          if Result > High(FViewItems) then
            Result := -1;
        end;
      end;
    end;
  end;
end;

procedure TfrmDataProviderEditor.ResetSelection;
begin
  if (Designer <> nil) and (FOrgSelect <> nil) then
    Designer.SetSelections(FOrgSelect);
  if FPropView <> nil then
    FreeAndNil(FPropView);
end;

procedure TfrmDataProviderEditor.SetNewSelection(AnItem: IJvDataItem);
begin
  if FPropView <> nil then
    FreeAndNil(FPropView);
  FPropView := TJvDataProviderItem.Create(AnItem);
  if Designer <> nil then
    Designer.SelectComponent(FPropView);
end;

procedure TfrmDataProviderEditor.UpdateLV;
begin
  lvProvider.Items.Count := Length(FViewItems);
  lvProvider.Invalidate;
end;

procedure TfrmDataProviderEditor.UpdateColumnSize;
begin
  lvProvider.Columns[0].Width := lvProvider.ClientWidth;
end;

procedure TfrmDataProviderEditor.UpdateSelectedItem;
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

procedure TfrmDataProviderEditor.ToggleItem(Index: Integer);
var
  Info: TProviderEditItem;
  Item: IJvDataItem;
  Items: IJvDataItems;
begin
  Info := FViewItems[Index];
  if (Info.Flags and vifHasChildren <> 0) and ((Index > 0) or (Info.Flags and vifExpanded = 0)) then
  begin
    if Info.Flags and vifExpanded <> 0 then
      DeleteSubItems(Index)
    else
    begin
      if Index = 0 then
        Item := FRootItem
      else
        Item := (Provider as IJvDataIDSearch).Find(Info.ID, True);
      if (Item <> nil) and Supports(Item, IJvDataItems, Items) then
      begin
        Inc(Index);
        InsertItems(Index, Items);
      end;
    end;
  end;
  UpdateLV;
end;

procedure TfrmDataProviderEditor.SelectItemID(ID: string);
var
  Idx: Integer;
begin
  Idx := LocateID(ID);
  if Idx > -1 then
    ListView_SetItemState(lvProvider.Handle, Idx, LVIS_SELECTED or LVIS_FOCUSED,
      LVIS_SELECTED or LVIS_FOCUSED);
end;

procedure TfrmDataProviderEditor.DeleteItem(Index: Integer);
var
  PrevIsParent: Boolean;
begin
  DeleteSubItems(Index);
  PrevIsParent := (Index > 0) and ((FViewItems[Index - 1].Flags and $00FFFFFF) = ((FViewItems[Index].Flags and $00FFFFFF) - 1));
  FViewItems[Index].ID := '';
  if Index < High(FViewItems) then
    Move(FViewItems[Index + 1], FViewItems[Index], (Length(FViewItems) - Index) * SizeOf(FViewItems[0]));
  FillChar(FViewItems[High(FViewItems)], SizeOf(FViewItems[0]), 0);
  SetLength(FViewItems, High(FViewItems));
  if PrevIsParent and ((Index = High(FViewItems)) or ((FViewItems[Index - 1].Flags and $00FFFFFF) <> ((FViewItems[Index].Flags and $00FFFFFF) - 1))) then
    FViewItems[Index - 1].Flags := FViewItems[Index - 1].Flags and not (vifHasChildren or vifExpanded);
end;

procedure TfrmDataProviderEditor.DeleteSubItems(Index: Integer);
var
  Idx: Integer;
  Lvl: Integer;
begin
  if FViewItems[Index].Flags and (vifExpanded + vifHasChildren) = (vifExpanded + vifHasChildren) then
  begin
    Lvl := (FViewItems[Index].Flags and $00FFFFFF) + 1;
    Idx := Index + 1;
    while (Idx < Length(FViewItems)) and ((FViewItems[Idx].Flags and $00FFFFFF) >= Lvl) do
    begin
      FViewItems[Idx].ID := '';
      Inc(Idx);
    end;
    // Idx points to next item that is not a child
    if Idx < Length(FViewItems) then
      Move(FViewItems[Idx], FViewItems[Index + 1], (Length(FViewItems) - Idx) * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Length(FViewItems) - Pred(Idx - Index)], Pred(Idx - Index) * SizeOf(FViewItems[0]), 0);
    SetLength(FViewItems, Length(FViewItems) - (Idx - Index - 1));
    FViewItems[Index].Flags := FViewItems[Index].Flags and not vifExpanded;
  end;
end;

procedure TfrmDataProviderEditor.AddSubItem(Index: Integer; Item: IJvDataItem);
var
  Lvl: Integer;
  Idx: Integer;
  SubItems: IJvDataItems;
begin
  if Index < 0 then
  begin
    Lvl := 0;
    Idx := Length(FViewItems);
  end
  else
  begin
    Lvl := Succ(FViewItems[Index].Flags and $00FFFFFF);
    Idx := Index + 1;
    if FViewItems[Index].Flags and (vifHasChildren + vifExpanded) = vifHasChildren then
    begin
      ToggleItem(Index);
      Exit;
    end;
  end;
  while (Idx < Length(FViewItems)) and ((FViewItems[Idx].Flags and $00FFFFFF) >= Lvl) do
    Inc(Idx);
  SetLength(FViewItems, Length(FViewItems) + 1);
  if Idx < High(FViewItems) then
  begin
    Move(FViewItems[Idx], FViewItems[Idx + 1], (High(FViewItems) - Idx) * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Idx], SizeOf(FViewItems[0]), 0);
  end;
  with FViewItems[Idx] do
  begin
    ID := Item.GetID;
    if Supports(Item, IJvDataItems, SubItems) then
    begin
      if SubItems.Count > 0 then
        Flags := Lvl + vifHasChildren + vifCanHaveChildren
      else
        Flags := Lvl + vifCanHaveChildren
    end
    else
      Flags := Lvl;
  end;
  if Index > -1 then
    with FViewItems[Index] do
      Flags := Flags or vifHasChildren or vifCanHaveChildren or vifExpanded;
end;

procedure TfrmDataProviderEditor.InsertItems(var Index: Integer; Items: IJvDataItems);
var
  I: Integer;
  J: Integer;
  SubItems: IJvDataItems;
  Man: IJvDataItemsManagement;
  Dsgn: IJvDataItemsDesigner;
begin
  J := Length(FViewItems);
  SetLength(FViewItems, Length(FViewItems) + Items.Count);
  if Index < J then
  begin
    Move(FViewItems[Index], FViewItems[Index + Items.Count], (J - Index) * SizeOf(FViewItems[0]));
    FillChar(FViewItems[Index], Items.Count * SizeOf(FViewItems[0]), 0);
  end;
  J := 0;
  if Index > 0 then
  begin
    J := 1 + FViewItems[Index - 1].Flags and $00FFFFFF;
    FViewItems[Index - 1].Flags := FViewItems[Index - 1].Flags or vifExpanded;
  end;
  for I  := 0 to Items.Count - 1 do
  begin
    with FViewItems[Index] do
    begin
      ID := Items.Items[I].GetID;
      Flags := J;
      if Supports(Items.Items[I], IJvDataItems, SubItems) then
      begin
        Flags := Flags + vifCanHaveChildren;
        if Supports(SubItems, IJvDataItemsManagement, Man) then
          Flags := Flags + vifHasMan;
        if Supports(SubItems, IJvDataItemsDesigner, Dsgn) then
          Flags := Flags + vifHasDsgn;
        if SubItems.Count > 0 then
          Flags := Flags + vifHasChildren;
      end;
    end;
    Inc(Index);
  end;
end;

procedure TfrmDataProviderEditor.SetProvider(Value: IJvDataProvider);
var
  LstIdx: Integer;
  ProviderImpl: TComponent;
begin
  if Provider <> nil then
    Provider.UnregisterChangeNotify(Self);
  if FRootItem <> nil then
    FreeAndNil(FRootItem);
  FProvider := Value;
  SetLength(FViewItems, 0); // Clears the list
  if Provider <> nil then
  begin
    FRootItem := TJvProviderRootItem.Create(Provider as IJvDataItems);
    AddSubItem(-1, FRootItem);
    LstIdx := 1;
    InsertItems(LstIdx, Provider as IJvDataItems);
    Provider.RegisterChangeNotify(Self);
    ProviderImpl := (Provider as IInterfaceComponentReference).GetComponent;
    Caption := Format(SDataProviderEditorCaption, [ProviderImpl.Name, '.' + PropName]);
  end;
  UpdateLV;
  UpdateSelectedItem;
end;

procedure TfrmDataProviderEditor.SetDesigner(
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

procedure TfrmDataProviderEditor.DataProviderChanging(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  case AReason of
    pcrDestroy:
      SetProvider(nil);
  end;
end;

procedure TfrmDataProviderEditor.DataProviderChanged(const ADataProvider: IJvDataProvider; AReason: TDataProviderChangeReason; Source: IUnknown);
begin
  case AReason of
    pcrDestroy: // Won't happen
      SetProvider(nil);
    pcrUpdateItem,
    pcrUpdateItems:
      lvProvider.Invalidate;
  end;
end;

procedure TfrmDataProviderEditor.lvProviderData(Sender: TObject; Item: TListItem);
var
  ItemData: TProviderEditItem;
  DataItem: IJvDataItem;
  ItemText: IJvDataItemText;
begin
  if (Provider = nil) or (Item.Index >= Length(FViewItems)) then
    Exit;
  ItemData := FViewItems[Item.Index];
  Item.Indent := ItemData.Flags and $00FFFFFF;
  if ItemData.ID = (FRootItem as IJvDataItem).GetID then
    DataItem := FRootItem
  else
    DataItem := (FProvider as IJvDataIDSearch).Find(ItemData.ID, True);
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
  else
    Item.Caption := Format(SDataItemIDNotFound, [ItemData.ID]);
end;

procedure TfrmDataProviderEditor.lvProviderCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ACanvas: TCanvas;
  ARect: TRect;
  ViewInfo: TProviderEditItem;
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
  ViewInfo := FViewItems[Item.Index];
  if (ViewInfo.Flags and vifHasChildren) <> 0 then
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

      if (ViewInfo.Flags and vifExpanded) = 0 then
      begin
        MoveTo(MidX, ARect.Top + 5);
        LineTo(MidX, ARect.Top + BtnWdth - 7);
      end;
    end;
  end;
  ARect.Left := ARect.Left + BtnWdth;
  DrawText(ACanvas.Handle, PChar(Item.Caption), Length(Item.Caption), ARect, DT_SINGLELINE + DT_LEFT + DT_END_ELLIPSIS);
end;

procedure TfrmDataProviderEditor.lvProviderSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelectedItem;
end;

procedure TfrmDataProviderEditor.lvProviderDblClick(Sender: TObject);
begin
  if lvProvider.Selected <> nil then
    ToggleItem(lvProvider.Selected.Index);
end;

procedure TfrmDataProviderEditor.lvProviderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: Integer;
  ViewInfo: TProviderEditItem;
  TmpRect: TRect;
begin
  Item := GetItemIndexAt(lvProvider, X, Y);
  if Item <> -1 then
  begin
    ViewInfo := FViewItems[Item];
    ListView_GetItemRect(lvProvider.Handle, Item, TmpRect, LVIR_BOUNDS);
    TmpRect.Right := TmpRect.Left + (Succ((TmpRect.Bottom - TmpRect.Top) + 2) * Succ(ViewInfo.Flags and $00FFFFFF));
    if (X < TmpRect.Right) and (X > TmpRect.Right - ((TmpRect.Bottom - TmpRect.Top) + 2)) then
      ToggleItem(Item);
  end;
end;

procedure TfrmDataProviderEditor.aiAddItemExecute(Sender: TObject);
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
    begin
      if lvProvider.Selected <> nil then
        AddSubItem(lvProvider.Selected.Index, Item)
      else
        AddSubItem(-1, Item);
      UpdateLV;
      SelectItemID(Item.getID);
    end
    else
      raise EJVCLException.Create(SDataProviderAddFailed);
  end
  else // should never occur
    raise EJVCLException.CreateFmt(SDataProviderAddErrorReason, [SDataProviderNoSubItems]);
end;

procedure TfrmDataProviderEditor.aiDeleteItemExecute(Sender: TObject);
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
    DeleteItem(I);
    UpdateLV;
    UpdateSelectedItem;
  end;
end;

procedure TfrmDataProviderEditor.aiClearSubExecute(Sender: TObject);
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
    DeleteSubItems(lvProvider.Selected.Index);
    with FViewItems[lvProvider.Selected.Index] do
      Flags := Flags and not vifHasChildren;
    UpdateLV;
  end;
end;

procedure TfrmDataProviderEditor.lvProviderResize(Sender: TObject);
begin
  UpdateColumnSize;
end;

procedure TfrmDataProviderEditor.FormCreate(Sender: TObject);
begin
  if EditorList.IndexOf(Self) = -1 then
    EditorList.Add(Self);
end;

procedure TfrmDataProviderEditor.FormDestroy(Sender: TObject);
begin
  ResetSelection;
  Provider := nil;
  Designer := nil;
  EditorList.Remove(Self);
end;

procedure TfrmDataProviderEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmDataProviderEditor.miTextLabelsClick(Sender: TObject);
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
