{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgInspectorGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgInspectorGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, JvgStringGrid, JvgTypes, ExtCtrls, JvgCommClasses, StdCtrls;

type

  TJvgGridItem = class(TCollectionItem)
  private
    FCaption: string;
    FEditMask: string;
    FOriginalValues: TStringList;
    FValues: TStringList;
    FExpanded: boolean;
    FSelected: boolean;
    procedure SetCaption(Value: string);
    procedure SetExpanded(Value: boolean);
    procedure SetSelected(Value: boolean);
    procedure SetChanged(Value: boolean);
    function GetChanged: boolean;
    procedure OnValuesChange(Sender: TObject);

    procedure Undo(Index: integer);
    procedure UndoAll;
  public
    Sequence: integer;
    Row: integer;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetValue: string;
    procedure SetValue(ValueIndex: integer; const Value: string);
    function IsChanged(ValueIndex: integer): boolean;

    property OriginalValues: TStringList read FOriginalValues write FOriginalValues;
    property EditMask: string read FEditMask write FEditMask;
  published
    property Caption: string read FCaption write SetCaption;
    property Values: TStringList read FValues write FValues;
    property Expanded: boolean read FExpanded write SetExpanded;
    property Selected: boolean read FSelected write SetSelected;
    property Changed: boolean read GetChanged write SetChanged;
  end;

  TJvgGridItems = class(TCollection)
  private
    FOnUpdate: TNotifyEvent;
    FShowMultiValues: boolean;
    function GetItem(Index: Integer): TJvgGridItem;
    procedure SetItem(Index: Integer; Value: TJvgGridItem);
    procedure SetShowMultiValues(Value: boolean);
  protected
    procedure Update(Item: TCollectionItem); override;
    property ShowMultiValues: boolean read FShowMultiValues write SetShowMultiValues;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy; override;
    function Add: TJvgGridItem;
    function Insert(Index: Integer): TJvgGridItem;

    property Items[Index: Integer]: TJvgGridItem read GetItem write SetItem; default;
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
  published
  end;

  TJvgInspectorGrid = class(TJvgStringGrid)
  private
    FItems: TJvgGridItems;
    procedure ItemsUpdate(Sender: TObject);
  protected
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    function CanEditModify: Boolean; override;
    //    function GetEditText(ACol, ARow: Longint): String; override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditMask(ACol, ARow: Longint): string; override;

    procedure DrawButton(ARow: longint; fExpanded: boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;

    procedure GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var Style: TglGridCellStyle); override;
    procedure GetCellGradientParams(Sender: TObject; ACol, ARow: longint; var CellRect: TRect; var Gradient: TJvgGradient); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UndoCurent;
    procedure UndoAll;

    property Items: TJvgGridItems read FItems write FItems;
    function RowToItem(ARow: integer): TJvgGridItem;
    function ItemToRow(Item: TJvgGridItem): integer;
  published
    { Published declarations }
  end;

procedure Register;

implementation
uses JvgUtils;

procedure Register;
begin
  //  RegisterComponents('Gl Controls', [TJvgInspectorGrid]);
end;

{ TJvgGridItems }

function TJvgGridItems.Add: TJvgGridItem;
begin
  Result := TJvgGridItem(inherited Add);
end;

constructor TJvgGridItems.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FShowMultiValues := true;
end;

destructor TJvgGridItems.Destroy;
begin
  inherited;
end;

function TJvgGridItems.GetItem(Index: Integer): TJvgGridItem;
begin
  Result := TJvgGridItem(inherited Items[Index]);
end;

function TJvgGridItems.Insert(Index: Integer): TJvgGridItem;
begin
  Result := TJvgGridItem(inherited Insert(Index));
end;

procedure TJvgGridItems.SetItem(Index: Integer; Value: TJvgGridItem);
begin
  Items[Index].Assign(Value);
end;

procedure TJvgGridItems.SetShowMultiValues(Value: boolean);
begin
  if FShowMultiValues = Value then exit;
  FShowMultiValues := Value;
  if Assigned(FOnUpdate) then FOnUpdate(self);
end;

procedure TJvgGridItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnUpdate) then FOnUpdate(self);
end;

{ TJvgGridItem }

constructor TJvgGridItem.Create(Collection: TCollection);
begin
  FValues := TStringList.Create;
  FOriginalValues := TStringList.Create;
  FValues.OnChange := OnValuesChange;
  inherited;
end;

destructor TJvgGridItem.Destroy;
begin
  FValues.Free;
  FOriginalValues.Free;
  inherited;
end;

//==================================================================================================

function TJvgGridItem.GetChanged: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to FValues.Count - 1 do
    Result := Result or bool(FValues.Objects[i]);
end;

function TJvgGridItem.GetValue: string;
var
  i: integer;
begin
  Result := '';
  if Values.Count = 0 then
    Result := ''
  else if (Values.Count = 1) or (not (Collection as TJvgGridItems).ShowMultiValues) then
    Result := Values[0]
  else
    for i := 0 to Values.Count - 1 do
    begin
      Result := Result + IIF(i = 0, '[', '') + IIF(i > 0, ',', '') + Values[i] + IIF(i = Values.Count - 1, ']', '');
    end;

end;

function TJvgGridItem.IsChanged(ValueIndex: integer): boolean;
begin
  if ValueIndex > 0 then dec(ValueIndex);
  Result := bool(Values.Objects[ValueIndex]);
end;

procedure TJvgGridItem.OnValuesChange(Sender: TObject);
var
  i: integer;
begin
  while FOriginalValues.Count > FValues.Count do
    FOriginalValues.Delete(FOriginalValues.Count - 1);

  for i := 0 to FValues.Count - 1 do
  begin
    if i = FOriginalValues.Count then
      FOriginalValues.Add(FValues[i])
    else if FOriginalValues.Objects[i] = nil then
      FOriginalValues.Objects[i] := Pointer(1);
  end;

  (Collection as TJvgGridItems).Update(self);
end;

procedure TJvgGridItem.SetCaption(Value: string);
begin
  if FCaption = Value then exit;
  FCaption := Value;
  (Collection as TJvgGridItems).Update(self);
end;

procedure TJvgGridItem.SetChanged(Value: boolean);
begin
  Values.OnChange := nil;
  if Sequence > 0 then
    Values.Objects[Sequence - 1] := Pointer(1)
  else
    Values.Objects[Sequence] := Pointer(1);
  Values.OnChange := OnValuesChange;
end;

procedure TJvgGridItem.SetExpanded(Value: boolean);
begin
  if FExpanded = Value then exit;
  FExpanded := Value and (Values.Count > 1);
  (Collection as TJvgGridItems).Update(self);
end;

procedure TJvgGridItem.SetSelected(Value: boolean);
begin
  if FSelected = Value then exit;
  FSelected := Value;
  //  (Collection as TJvgGridItems).Update(self);
end;

procedure TJvgGridItem.SetValue(ValueIndex: integer; const Value: string);
var
  i, Seq: integer;
begin
  if (Sequence = 0) and (FValues.Count > 1) then exit;

  if Sequence > 0 then
    Seq := Sequence - 1
  else
    Seq := Sequence;
  if Values[Seq] = Value then exit;

  FValues.OnChange := nil;
  FValues[Seq] := Value;
  FValues.Objects[Seq] := Pointer(integer(Value <> FOriginalValues[Seq]));
  FValues.OnChange := OnValuesChange;

  //  FChanged := false;
end;

procedure TJvgGridItem.Undo(Index: integer);
begin
  FValues[Index] := FOriginalValues[Index];
  FValues.Objects[Index] := Pointer(integer(FValues[Index] <> FOriginalValues[Index]));
end;

procedure TJvgGridItem.UndoAll;
var
  i: integer;
begin
  FValues.OnChange := nil;
  for i := 0 to FValues.Count - 1 do
    Undo(i);
  FValues.OnChange := OnValuesChange;
  OnValuesChange(self);
end;

{ TJvgInspectorGrid }

constructor TJvgInspectorGrid.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TJvgGridItems.Create(TJvgGridItem);
  DefaultDrawing := false;
  DefaultRowHeight := 16; //Canvas.TextHeight('Th');
  ColCount := 2;
  FItems.OnUpdate := ItemsUpdate;
  Options := Options + [goEditing, goAlwaysShowEditor];
  CaptionTextAlignment := taLeftJustify;
end;

destructor TJvgInspectorGrid.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TJvgInspectorGrid.DrawButton(ARow: Integer; fExpanded: boolean);
var
  R: TRect;
begin
  with Canvas do
  begin
    R := Bounds(2, (DefaultRowHeight + 1) * ARow + (DefaultRowHeight shr 1) - 3, 7, 7);
    FillRect(R);
    Pen.Color := clBlack;
    MoveTo(R.Left + 1, R.Top + 3);
    LineTo(R.Right - 1, R.Top + 3);
    if not fExpanded then
    begin
      MoveTo(R.Left + 3, R.Top + 1);
      LineTo(R.Left + 3, R.Bottom - 1);
    end;
  end;
end;

procedure TJvgInspectorGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  Item: TJvgGridItem;
begin
  inherited;
  //  if Items.Count <= ARow then exit;
  if ACol = 1 then InvalidateCol(0);

  Item := RowToItem(ARow);
  if Item = nil then exit;

  if (Item.Values.Count > 1) and (Item.Sequence = 0) then
    DrawButton(ARow, Item.Expanded);

end;

procedure TJvgInspectorGrid.GetCellGradientParams(Sender: TObject; ACol,
  ARow: Integer; var CellRect: TRect; var Gradient: TJvgGradient);
begin
  inherited;

end;

procedure TJvgInspectorGrid.GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var Style: TglGridCellStyle);
var
  i, ItemNo: integer;
  Item: TJvgGridItem;
begin
  with Style do
  begin
    //  inherited;
    ItemNo := 0;
    BevelInner := bvNone;
    BevelOuter := bvSpace;
    BevelBold := false;

    Item := RowToItem(Row);
    if Item = nil then exit;

    if ACol = 0 then
      BackgrColor := IIF(ARow = Row, clBtnShadow, DecColor(ColorToRGB(clBtnShadow), 20))
    else
      BackgrColor := clBtnFace;

    if not Hottracking then
    begin
      FontColor := IIF(ACol = 0, clWhite, clBlack);
      if (ACol = 0) and (ARow = Item.Row) then FontColor := clYellow;
    end;
    Interspace := IIF(ACol = 0, 13, 3);

    Item := RowToItem(ARow);
    if Item = nil then exit;

    if Item.Changed and (Item.IsChanged(Item.Sequence) or (Item.Sequence = 0)) then
      FontStyle := [fsBold]
    else
      FontStyle := [];
  end;

end;

{function TJvgInspectorGrid.GetEditText(ACol, ARow: Longint): string;
var i: integer;
begin
  inherited GetEditText(ACol, ARow);
  for i := 0 to Items.Count-1 do
    Items[i].Selected := false;
  Items[ARow].Selected := true;
end;}

procedure TJvgInspectorGrid.ItemsUpdate(Sender: TObject);
var
  i, j: integer;
begin
  SendMessage(handle, WM_SETREDRAW, 0, 0);

  RowCount := 1;
  for i := 0 to Items.Count - 1 do
  begin
    Items[i].Row := RowCount - 1;
    Cells[0, RowCount - 1] := Items[i].Caption;
    if Items[i].Values.Count = 1 then
    begin
      Cells[1, RowCount - 1] := Items[i].Values[0];
      if ColCount > 2 then
        Cells[2, RowCount - 1] := Items[i].OriginalValues[0];
    end
    else if Items[i].Values.Count > 1 then
    begin
      if ColCount > 2 then
        Cells[2, RowCount - 1] := '';
      Cells[1, RowCount - 1] := Items[i].GetValue;
      if Items[i].Expanded then
        for j := 0 to Items[i].Values.Count - 1 do
        begin

          RowCount := RowCount + 1;
          Cells[0, RowCount - 1] := '';
          Cells[1, RowCount - 1] := Items[i].Values[j];
          if ColCount > 2 then
            Cells[2, RowCount - 1] := Items[i].OriginalValues[j];
        end;
    end
    else
      Cells[1, RowCount - 1] := '';
    RowCount := RowCount + 1;
  end;
  RowCount := RowCount - 1;

  SendMessage(handle, WM_SETREDRAW, 1, 0);
  Invalidate; //Rect(Handle, nil, false);
end;

procedure TJvgInspectorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
type
  PClass = ^TClass;
var
  ClassOld: TClass;
  GridCoord: TGridCoord;
  Item: TJvgGridItem;
begin
  //...inherit grandfather
{  ClassOld := PClass(self)^;
  PClass(self)^ := self.ClassParent.ClassParent;
  self.MouseDown(Button, Shift, X, Y);
  PClass(self)^ := ClassOld;}
  inherited;

  GridCoord := MouseCoord(X, Y);
  if GridCoord.X = 0 then
  begin
    Item := RowToItem(GridCoord.Y);
    if Item = nil then exit;
    if Item.Sequence = 0 then
      Item.Expanded := not Item.Expanded;
    Row := GridCoord.Y;
  end;
end;

function TJvgInspectorGrid.RowToItem(ARow: integer): TJvgGridItem;
var
  i, Index: integer;
begin
  Index := 0;
  i := 0;
  Result := nil;
  if Items.Count = 0 then exit;
  for i := 0 to ARow - 1 do
  begin
    if Items[i].Expanded then inc(Index, Items[i].Values.Count);
    inc(Index);
    if Index > ARow then break;
  end;

  Result := Items[i];
  Result.Sequence := ARow - ItemToRow(Result);

end;

function TJvgInspectorGrid.ItemToRow(Item: TJvgGridItem): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Item.Index - 1 do
  begin
    if Items[i].Expanded then inc(Result, Items[i].Values.Count);
    inc(Result);
  end;
end;

function TJvgInspectorGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
  if Assigned(OnSelectCell) then OnSelectCell(Self, ACol, ARow, Result);
  // if Items.FShowMultiValues and (Items[ARow].Values.Count > 1) then
 //   Result := false;
end;

function TJvgInspectorGrid.CanEditModify: Boolean;
var
  Item: TJvgGridItem;
begin
  if (Col <> 1) or not (goEditing in Options) then
  begin
    Result := false;
    exit;
  end;
  Item := RowToItem(Row);
  if Item = nil then exit;
  Result := not (Items.FShowMultiValues and (Item.Values.Count > 1) and (Item.Row = Row));
end;

procedure TJvgInspectorGrid.SetEditText(ACol, ARow: Integer; const Value: string);
var
  Item: TJvgGridItem;
begin
  inherited SetEditText(ACol, ARow, Value);
  if Assigned(OnSetEditText) then OnSetEditText(Self, ACol, ARow, Value);
  Item := RowToItem(ARow);
  if Item = nil then exit;
  //  if Value <> Item.Values[Item.Sequence] then Item.Changed := true;// Values.Data :=
  Item.SetValue(Item.Sequence, Value);

  if Item.Sequence > 0 then
    Cells[1, Item.Row] := Item.GetValue;
  //  Item.Changed := Item.Changed or ;
    //Cells[1, ARow] := Value;
end;

procedure TJvgInspectorGrid.KeyPress(var Key: Char);
var
  Item: TJvgGridItem;
  i, OldRow: integer;
begin
  inherited;
  Item := RowToItem(Row);
  if Item = nil then exit;
  OldRow := Row;
  case Key of
    '+': if (Item.Values.Count > 0) and not Item.Expanded then Item.Expanded := true;
    '-': if (Item.Values.Count > 0) and Item.Expanded and (Row = ItemToRow(Item)) then Item.Expanded := false;
    '*': for i := 0 to Items.Count - 1 do
        Items[i].Expanded := true;
  end;
  Row := OldRow;
end;

procedure TJvgInspectorGrid.UndoAll;
var
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
    Items[i].UndoAll;
end;

procedure TJvgInspectorGrid.UndoCurent;
begin
  RowToItem(Row).UndoAll;
end;

procedure TJvgInspectorGrid.WMSize(var Msg: TWMSize);
var
  i, FreeClientWidth: integer;
begin
  inherited;
  FreeClientWidth := Width;
  i := GetScrollPos(handle, SB_VERT);
  if i <> 0 then dec(FreeClientWidth, GetSystemMetrics(SM_CXHSCROLL) + 2);
  ColWidths[1] := FreeClientWidth - ColWidths[0];
end;

function TJvgInspectorGrid.GetEditMask(ACol, ARow: Integer): string;
var
  Item: TJvgGridItem;
begin
  Item := RowToItem(ARow);
  if Item = nil then exit;
  if CanEditModify then
    Result := Item.EditMask
  else
    Result := '';
  if Assigned(OnGetEditMask) then OnGetEditMask(Self, ACol, ARow, Result);
end;

end.
