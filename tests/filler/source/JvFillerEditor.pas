unit JvFillerEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  JvFillIntf, ComCtrls, ImgList, ActnList, Menus;

type
  TFillerEditItem = packed record
    ID: string;
    Flags: Integer;
  end;
  TFillerEditItems = array of TFillerEditItem;

  TfrmFillerEditor = class(TForm)
    lvFiller: TListView;
    alFillerEditor: TActionList;
    aiAddItem: TAction;
    aiDeleteItem: TAction;
    aiClearSub: TAction;
    aiClear: TAction;
    pmFillerEditor: TPopupMenu;
    miAddItem: TMenuItem;
    miDeleteItem: TMenuItem;
    miClearSub: TMenuItem;
    miClear: TMenuItem;
    procedure lvFillerData(Sender: TObject; Item: TListItem);
    procedure lvFillerCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvFillerSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvFillerDblClick(Sender: TObject);
    procedure lvFillerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aiAddItemExecute(Sender: TObject);
    procedure aiDeleteItemExecute(Sender: TObject);
    procedure aiClearSubExecute(Sender: TObject);
    procedure aiClearExecute(Sender: TObject);
    procedure lvFillerResize(Sender: TObject);
  private
    { Private declarations }
    FFiller: IFiller;
    FViewItems: TFillerEditItems;
    function GetFillerItem(Index: Integer): IFillerItem;
    function LocateID(ID: string): Integer;
    procedure UpdateLV;
    procedure UpdateColumnSize;
    procedure UpdateSelectedItem;
    procedure ToggleItem(Index: Integer);
    procedure SelectItemID(ID: string);
    procedure DeleteItem(Index: Integer);
    procedure DeleteSubItems(Index: Integer);
    procedure AddSubItem(Index: Integer; Item: IFillerItem);
    procedure InsertItems(Index: Integer; Items: IFillerItems);
    procedure SetFiller(Value: IFiller);
  public
    { Public declarations }
    property Filler: IFiller read FFiller write SetFiller;
  end;

function EditFiller(AFiller: IFiller): Boolean;

implementation

{$R *.DFM}

uses
  Commctrl, Dialogs,
  JvTypes;
  
const
  vifHasChildren = Integer($80000000);
  vifCanHaveChildren = Integer($40000000);
  vifExpanded = Integer($20000000);
  
function EditFiller(AFiller: IFiller): Boolean;
begin
  with TfrmFillerEditor.Create(Screen.ActiveCustomForm) do
  try
    Filler := AFiller;
    Result := ShowModal = mrOk;
//    Filler := nil;
  finally
    Free;
  end;
end;

function AddByDesigner(Designer: IFillerItemsDesigner): IFillerItem;
begin
  // TODO: Use designer specified item kind to add items.
  Result := nil;
end;

function TfrmFillerEditor.GetFillerItem(Index: Integer): IFillerItem;
begin
  Result := (Filler as IFillerIDSearch).FindByID(FViewItems[Index].ID, True);
end;

function TfrmFillerEditor.LocateID(ID: string): Integer;
var
  Item: IFillerItem;
begin
  Result := High(FViewItems);
  while (Result >= 0) and (FViewItems[Result].ID <> ID) do
    Dec(Result);
  if Result < 0 then
  begin
    Item := (Filler as IFillerIDSearch).FindByID(ID, True);
    if Item <> nil then
    begin
      Item := Item.Items.Parent;
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

procedure TfrmFillerEditor.UpdateLV;
begin
  lvFiller.Items.Count := Length(FViewItems);
  lvFiller.Invalidate;
end;

procedure TfrmFillerEditor.UpdateColumnSize;
begin
  lvFiller.Columns[0].Width := lvFiller.ClientWidth;
end;

procedure TfrmFillerEditor.UpdateSelectedItem;
begin
  // Update action states
end;

procedure TfrmFillerEditor.ToggleItem(Index: Integer);
var
  Info: TFillerEditItem;
  Item: IFillerItem;
  Items: IFillerItems;
begin
  Info := FViewItems[Index];
  if Info.Flags and vifHasChildren <> 0 then
  begin
    if Info.Flags and vifExpanded <> 0 then
      DeleteSubItems(Index)
    else
    begin
      Item := (Filler as IFillerIDSearch).FindByID(Info.ID, True);
      if (Item <> nil) and Supports(Item, IFillerItems, Items) then
        InsertItems(Index + 1, Items);
    end;
  end;
  UpdateLV;
end;

procedure TfrmFillerEditor.SelectItemID(ID: string);
var
  Idx: Integer;
  lvi: TLVItem;
begin
  Idx := LocateID(ID);
  if Idx > -1 then
  begin
    lvi.iItem := Idx;
    lvi.mask := LVIF_STATE;
    lvi.state := LVIS_SELECTED or LVIS_FOCUSED;
    lvi.stateMask := LVIS_SELECTED or LVIS_FOCUSED;
    ListView_SetItem(lvFiller.Handle, lvi);
    lvFiller.Invalidate;
  end;
end;

procedure TfrmFillerEditor.DeleteItem(Index: Integer);
begin
  DeleteSubItems(Index);
  if Index < High(FViewItems) then
    Move(FViewItems[Index + 1], FViewItems[Index], (Length(FViewItems) - Index) * SizeOf(FViewItems[0]));
  SetLength(FViewItems, High(FViewItems));
end;

procedure TfrmFillerEditor.DeleteSubItems(Index: Integer);
var
  Idx: Integer;
  Lvl: Integer;
begin
  if FViewItems[Index].Flags and (vifExpanded + vifHasChildren) = (vifExpanded + vifHasChildren) then
  begin
    Lvl := (FViewItems[Index].Flags and $00FFFFFF) + 1;
    Idx := Index + 1;
    while (Idx < Length(FViewItems)) and ((FViewItems[Idx].Flags and $00FFFFFF) >= Lvl) do
      Inc(Idx);
    // Idx points to next item that is not a child
    if Idx < Length(FViewItems) then
      Move(FViewItems[Idx], FViewItems[Index + 1], (Length(FViewItems) - Idx) * SizeOf(FViewItems[0]));
    SetLength(FViewItems, Length(FViewItems) - (Idx - Index - 1));
    FViewItems[Index].Flags := FViewItems[Index].Flags and not vifExpanded;
  end;
end;

procedure TfrmFillerEditor.AddSubItem(Index: Integer; Item: IFillerItem);
var
  Lvl: Integer;
  Idx: Integer;
  SubItems: IFillerItems;
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
  end;
  while (Idx < Length(FViewItems)) and ((FViewItems[Idx].Flags and $00FFFFFF) >= Lvl) do
    Inc(Idx);
  SetLength(FViewItems, Length(FViewItems) + 1);
  if Idx < High(FViewItems) then
    Move(FViewItems[Idx], FViewItems[Idx + 1], (High(FViewItems) - Idx) * SizeOf(FViewItems[0]));
  with FViewItems[Idx] do
  begin
    ID := Item.GetID;
    if Supports(Item, IFillerItems, SubItems) then
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

procedure TfrmFillerEditor.InsertItems(Index: Integer; Items: IFillerItems);
var
  I: Integer;
  J: Integer;
  SubItems: IFillerItems;
begin
  J := Length(FViewItems);
  SetLength(FViewItems, Length(FViewItems) + Items.Count);
  if Index < J then
    Move(FViewItems[Index], FViewItems[Index + Items.Count], (J - Index - 1) * SizeOf(FViewItems[0]));
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
      if Supports(Items.Items[I], IFillerItems, SubItems) then
      begin
        if SubItems.Count > 0 then
          Flags := J + vifHasChildren + vifCanHaveChildren
        else
          Flags := J + vifCanHaveChildren
      end
      else
        Flags := J;
    end;
    Inc(Index);
  end;
end;

procedure TfrmFillerEditor.SetFiller(Value: IFiller);
var
  LstIdx: Integer;
begin
  FFiller := Value;
  SetLength(FViewItems, 0); // Clears the list
  LstIdx := 0;
  if Filler <> nil then
    InsertItems(LstIdx, Filler as IFillerItems);
  UpdateLV;
end;

procedure TfrmFillerEditor.lvFillerData(Sender: TObject; Item: TListItem);
var
  ItemData: TFillerEditItem;
  FillerItem: IFillerItem;
  ItemText: IFillerItemText;
begin
  if (Filler = nil) or (Item.Index >= Length(FViewItems)) then
    Exit;
  ItemData := FViewItems[Item.Index];
  Item.Indent := ItemData.Flags and $00FFFFFF;
  FillerItem := (FFiller as IFillerIDSearch).FindByID(ItemData.ID, True);
  if FillerItem <> nil then
  begin
    if Supports(FillerItem, IFillerItemText, ItemText) then
      Item.Caption := ItemText.Caption
    else
      Item.Caption := 'Item has no text support.';
  end
  else
    Item.Caption := 'Item not found!';
end;

procedure TfrmFillerEditor.lvFillerCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ACanvas: TCanvas;
  ARect: TRect;
  ViewInfo: TFillerEditItem;
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

procedure TfrmFillerEditor.lvFillerSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  UpdateSelectedItem;
end;

procedure TfrmFillerEditor.lvFillerDblClick(Sender: TObject);
begin
  if lvFiller.Selected <> nil then
    ToggleItem(lvFiller.Selected.Index);
end;

procedure TfrmFillerEditor.lvFillerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
  ViewInfo: TFillerEditItem;
  TmpRect: TRect;
begin
{  Item := lvFiller.GetItemAt(X, Y);
  if Item <> nil then
  begin
    ViewInfo := FViewItems[Item.Index];
    TmpRect := Item.DisplayRect(drBounds);
    TmpRect.Right := TmpRect.Left + Succ((TmpRect.Bottom - TmpRect.Top) + 2 * Succ(Item.Indent));
    if (X < TmpRect.Right) and (X > TmpRect.Right - ((TmpRect.Bottom - TmpRect.Top) + 2)) then
      ToggleItem(Item.Index);
  end;}
end;

procedure TfrmFillerEditor.aiAddItemExecute(Sender: TObject);
var
  Item: IFillerItem;
  Items: IFillerItems;
  Dsgn: IFillerItemsDesigner;
  Mangr: IFillerItemManagment;
  Txt: IFillerItemText;
begin
  if lvFiller.Selected <> nil then
  begin
    Item := GetFillerItem(lvFiller.Selected.Index);
    if Item <> nil then
      Item.QueryInterface(IFillerItems, Items)
    else
      raise EJVCLException.Create('Item not found.');
  end
  else
    Items := Filler as IFillerItems;
  if Items <> nil then
  begin
    if Supports(Items, IFillerItemsDesigner, Dsgn) then
      Item := AddByDesigner(Dsgn)
    else if Supports(Items, IFillerItemManagment, Mangr) then
    begin
      Item := Mangr.New;
      if Item = nil then
        raise EJVCLException.Create('Failed to add a new item.');
    end
    else
      raise EJVCLException.Create('Unable to add new item; neither IFillerItemManagment nor IFillerItemsDesigner are supported.');
    if Item <> nil then
    begin
      if Supports(Item, IFillerItemText, Txt) then
        Txt.Caption := 'Test';
      // Item added, expand the original item if it wasn't or add the new item otherwise
      {if (lvFiller.Selected <> nil) and (FViewItems[lvFiller.Selected.Index].Flags and vifExpanded = 0) then
        ToggleItem(lvFiller.Selected.Index)
      else }if lvFiller.Selected <> nil then
        AddSubItem(lvFiller.Selected.Index, Item)
      else
        AddSubItem(-1, Item);
      UpdateLV;
      SelectItemID(Item.getID);
    end;
  end
  else
    raise EJVCLException.Create('Unable to add new item; item doesn''t support IFillerItems.');
end;

procedure TfrmFillerEditor.aiDeleteItemExecute(Sender: TObject);
var
  Item: IFillerItem;
  Items: IFillerItems;
  Mangr: IFillerItemManagment;
begin
  if lvFiller.Selected <> nil then
  begin
    Item := GetFillerItem(lvFiller.Selected.Index);
    if Item <> nil then
      Items := Item.Items
    else
      raise EJVCLException.Create('Item not found.');
    if Supports(Items, IFillerItemManagment, Mangr) then
      Mangr.Remove(Item)
    else
      raise EJVCLException.Create('Unable to delete item; IFillerItemManagment is not supported.');
    DeleteItem(lvFiller.Selected.Index);
    UpdateLV;
  end;
end;

procedure TfrmFillerEditor.aiClearSubExecute(Sender: TObject);
var
  Item: IFillerItem;
  Items: IFillerItems;
  Mangr: IFillerItemManagment;
begin
  if lvFiller.Selected <> nil then
  begin
    Item := GetFillerItem(lvFiller.Selected.Index);
    if Item <> nil then
      Items := Item.Items
    else
      raise EJVCLException.Create('Item not found.');
    if Supports(Items, IFillerItemManagment, Mangr) then
      Mangr.Clear
    else
      raise EJVCLException.Create('Unable to delete items; IFillerItemManagment is not supported.');
    DeleteSubItems(lvFiller.Selected.Index);
    UpdateLV;
  end;
end;

procedure TfrmFillerEditor.aiClearExecute(Sender: TObject);
var
  Mangr: IFillerItemManagment;
begin
  if Supports(Filler, IFillerItemManagment, Mangr) then
    Mangr.Clear
  else
    raise EJVCLException.Create('Unable to delete items; IFillerItemManagment is not supported.');
  SetLength(FViewItems, 0);
  UpdateLV;
end;

procedure TfrmFillerEditor.lvFillerResize(Sender: TObject);
begin
  UpdateColumnSize;
end;

end.
