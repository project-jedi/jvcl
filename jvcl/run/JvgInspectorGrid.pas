{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgInspectorGrid.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgInspectorGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls,
  JvgStringGrid, JvgTypes, JvgCommClasses;

type

  TJvgGridItem = class(TCollectionItem)
  private
    FCaption: string;
    FEditMask: string;
    FOriginalValues: TStringList;
    FValues: TStringList;
    FExpanded: Boolean;
    FSelected: Boolean;
    FSequence: Integer;
    FRow: Integer;
    procedure SetCaption(Value: string);
    procedure SetExpanded(Value: Boolean);
    procedure SetSelected(Value: Boolean);
    procedure SetChanged(Value: Boolean);
    function GetChanged: Boolean;
    procedure OnValuesChange(Sender: TObject);
    procedure Undo(Index: Integer);
    procedure UndoAll;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetValue: string;
    procedure SetValue(ValueIndex: Integer; const Value: string);
    function IsChanged(ValueIndex: Integer): Boolean;
    property OriginalValues: TStringList read FOriginalValues write FOriginalValues;
    property EditMask: string read FEditMask write FEditMask;
    property Sequence: Integer read FSequence write FSequence;
    property Row: Integer read FRow write FRow;
  published
    property Caption: string read FCaption write SetCaption;
    property Values: TStringList read FValues write FValues;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property Selected: Boolean read FSelected write SetSelected;
    property HasChanged: Boolean read GetChanged write SetChanged;
  end;

  TJvgGridItems = class(TCollection)
  private
    FOnUpdate: TNotifyEvent;
    FShowMultiValues: Boolean;
    function GetItem(Index: Integer): TJvgGridItem;
    procedure SetItem(Index: Integer; Value: TJvgGridItem);
    procedure SetShowMultiValues(Value: Boolean);
  protected
    procedure Update(Item: TCollectionItem); override;
    property ShowMultiValues: Boolean read FShowMultiValues write SetShowMultiValues;
  public
    constructor Create(ItemClass: TCollectionItemClass);
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

    procedure DrawButton(ARow: Longint; Expanded: Boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;

    procedure GetCellStyle(Sender: TObject; var ACol, ARow: Integer; var Style: TglGridCellStyle); override;
    procedure GetCellGradientParams(Sender: TObject; ACol, ARow: Longint; var CellRect: TRect; var Gradient:
      TJvgGradient); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UndoCurent;
    procedure UndoAll;

    property Items: TJvgGridItems read FItems write FItems;
    function RowToItem(ARow: Integer): TJvgGridItem;
    function ItemToRow(Item: TJvgGridItem): Integer;
  end;

implementation

uses
  JvgUtils;

//=== { TJvgGridItems } ======================================================

constructor TJvgGridItems.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FShowMultiValues := True;
end;

function TJvgGridItems.Add: TJvgGridItem;
begin
  Result := TJvgGridItem(inherited Add);
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

procedure TJvgGridItems.SetShowMultiValues(Value: Boolean);
begin
  if FShowMultiValues <> Value then
  begin
    FShowMultiValues := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TJvgGridItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
end;

//=== { TJvgGridItem } =======================================================

constructor TJvgGridItem.Create(Collection: TCollection);
begin
  // (rom) moved inherited up
  inherited Create(Collection);
  FValues := TStringList.Create;
  FOriginalValues := TStringList.Create;
  FValues.OnChange := OnValuesChange;
end;

destructor TJvgGridItem.Destroy;
begin
  FValues.Free;
  FOriginalValues.Free;
  inherited Destroy;
end;

function TJvgGridItem.GetChanged: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FValues.Count - 1 do
    Result := Result or bool(FValues.Objects[I]);
end;

function TJvgGridItem.GetValue: string;
var
  I: Integer;
begin
  Result := '';
  if Values.Count = 0 then
    Result := ''
  else
  if (Values.Count = 1) or (not (Collection as TJvgGridItems).ShowMultiValues) then
    Result := Values[0]
  else
    for I := 0 to Values.Count - 1 do
      Result := Result + IIF(I = 0, '[', '') + IIF(I > 0, ',', '') + Values[I] + IIF(I = Values.Count - 1, ']', '');
end;

function TJvgGridItem.IsChanged(ValueIndex: Integer): Boolean;
begin
  if ValueIndex > 0 then
    Dec(ValueIndex);
  Result := Values.Objects[ValueIndex] <> nil;
end;

procedure TJvgGridItem.OnValuesChange(Sender: TObject);
var
  I: Integer;
begin
  while FOriginalValues.Count > FValues.Count do
    FOriginalValues.Delete(FOriginalValues.Count - 1);

  for I := 0 to FValues.Count - 1 do
    if I = FOriginalValues.Count then
      FOriginalValues.Add(FValues[I])
    else
    if FOriginalValues.Objects[I] = nil then
      FOriginalValues.Objects[I] := Pointer(1);

  (Collection as TJvgGridItems).Update(Self);
end;

procedure TJvgGridItem.SetCaption(Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    (Collection as TJvgGridItems).Update(Self);
  end;
end;

procedure TJvgGridItem.SetChanged(Value: Boolean);
begin
  Values.OnChange := nil;
  if Sequence > 0 then
    Values.Objects[Sequence - 1] := Pointer(1)
  else
    Values.Objects[Sequence] := Pointer(1);
  Values.OnChange := OnValuesChange;
end;

procedure TJvgGridItem.SetExpanded(Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    FExpanded := Value and (Values.Count > 1);
    (Collection as TJvgGridItems).Update(Self);
  end;
end;

procedure TJvgGridItem.SetSelected(Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    // (Collection as TJvgGridItems).Update(Self);
  end;
end;

procedure TJvgGridItem.SetValue(ValueIndex: Integer; const Value: string);
var
  Seq: Integer;
begin
  if (Sequence = 0) and (FValues.Count > 1) then
    Exit;

  if Sequence > 0 then
    Seq := Sequence - 1
  else
    Seq := Sequence;

  if Values[Seq] <> Value then
  begin
    FValues.OnChange := nil;
    FValues[Seq] := Value;
    FValues.Objects[Seq] := Pointer(Integer(Value <> FOriginalValues[Seq]));
    FValues.OnChange := OnValuesChange;
    //  FChanged := False;
  end;
end;

procedure TJvgGridItem.Undo(Index: Integer);
begin
  FValues[Index] := FOriginalValues[Index];
  FValues.Objects[Index] := Pointer(Integer(FValues[Index] <> FOriginalValues[Index]));
end;

procedure TJvgGridItem.UndoAll;
var
  I: Integer;
begin
  FValues.OnChange := nil;
  for I := 0 to FValues.Count - 1 do
    Undo(I);
  FValues.OnChange := OnValuesChange;
  OnValuesChange(Self);
end;

//=== { TJvgInspectorGrid } ==================================================

constructor TJvgInspectorGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJvgGridItems.Create(TJvgGridItem);
  DefaultDrawing := False;
  DefaultRowHeight := 16; //Canvas.TextHeight('Th');
  ColCount := 2;
  FItems.OnUpdate := ItemsUpdate;
  Options := Options + [goEditing, goAlwaysShowEditor];
  CaptionTextAlignment := taLeftJustify;
end;

destructor TJvgInspectorGrid.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJvgInspectorGrid.DrawButton(ARow: Integer; Expanded: Boolean);
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
    if not Expanded then
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
  inherited DrawCell(ACol, ARow, ARect, AState);
  //  if Items.Count <= ARow then Exit;
  if ACol = 1 then
    InvalidateCol(0);

  Item := RowToItem(ARow);
  if Assigned(Item) and (Item.Values.Count > 1) and (Item.Sequence = 0) then
    DrawButton(ARow, Item.Expanded);
end;

procedure TJvgInspectorGrid.GetCellGradientParams(Sender: TObject; ACol,
  ARow: Integer; var CellRect: TRect; var Gradient: TJvgGradient);
begin
  inherited GetCellGradientParams(Sender, ACol, ARow, CellRect, Gradient);
end;

procedure TJvgInspectorGrid.GetCellStyle(Sender: TObject; var ACol, ARow: Integer;
  var Style: TglGridCellStyle);
var
  //  ItemNo: Integer;
  Item: TJvgGridItem;
begin
  with Style do
  begin
    // inherited;
    // ItemNo := 0;
    BevelInner := bvNone;
    BevelOuter := bvSpace;
    BevelBold := False;

    Item := RowToItem(Row);
    if Item = nil then
      Exit;

    if ACol = 0 then
      BackgrColor := IIF(ARow = Row, clBtnShadow, DecColor(ColorToRGB(clBtnShadow), 20))
    else
      BackgrColor := clBtnFace;

    if not Hottracking then
    begin
      FontColor := IIF(ACol = 0, clWhite, clBlack);
      if (ACol = 0) and (ARow = Item.Row) then
        FontColor := clYellow;
    end;
    Interspace := IIF(ACol = 0, 13, 3);

    Item := RowToItem(ARow);
    if Item <> nil then
      if Item.HasChanged and (Item.IsChanged(Item.Sequence) or (Item.Sequence = 0)) then
        FontStyle := [fsBold]
      else
        FontStyle := [];
  end;

end;

{function TJvgInspectorGrid.GetEditText(ACol, ARow: Longint): string;
var
  I: Integer;
begin
  inherited GetEditText(ACol, ARow);
  for I := 0 to Items.Count-1 do
    Items[I].Selected := False;
  Items[ARow].Selected := True;
end;}

procedure TJvgInspectorGrid.ItemsUpdate(Sender: TObject);
var
  I, J: Integer;
begin
  SendMessage(handle, WM_SETREDRAW, 0, 0);

  RowCount := 1;
  for I := 0 to Items.Count - 1 do
  begin
    Items[I].Row := RowCount - 1;
    Cells[0, RowCount - 1] := Items[I].Caption;
    if Items[I].Values.Count = 1 then
    begin
      Cells[1, RowCount - 1] := Items[I].Values[0];
      if ColCount > 2 then
        Cells[2, RowCount - 1] := Items[I].OriginalValues[0];
    end
    else
    if Items[I].Values.Count > 1 then
    begin
      if ColCount > 2 then
        Cells[2, RowCount - 1] := '';
      Cells[1, RowCount - 1] := Items[I].GetValue;
      if Items[I].Expanded then
        for J := 0 to Items[I].Values.Count - 1 do
        begin
          RowCount := RowCount + 1;
          Cells[0, RowCount - 1] := '';
          Cells[1, RowCount - 1] := Items[I].Values[J];
          if ColCount > 2 then
            Cells[2, RowCount - 1] := Items[I].OriginalValues[J];
        end;
    end
    else
      Cells[1, RowCount - 1] := '';
    RowCount := RowCount + 1;
  end;
  RowCount := RowCount - 1;

  SendMessage(handle, WM_SETREDRAW, 1, 0);
  Invalidate; //Rect(Handle, nil, False);
end;

procedure TJvgInspectorGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
type
  PClass = ^TClass;
var
  GridCoord: TGridCoord;
  Item: TJvgGridItem;
begin
  //...inherit grandfather
{  ClassOld := PClass(Self)^;
  PClass(Self)^ := Self.ClassParent.ClassParent;
  Self.MouseDown(Button, Shift, X, Y);
  PClass(Self)^ := ClassOld;}
  inherited MouseDown(Button, Shift, X, Y);

  GridCoord := MouseCoord(X, Y);
  if GridCoord.X = 0 then
  begin
    Item := RowToItem(GridCoord.Y);
    if Item = nil then
      Exit;
    if Item.Sequence = 0 then
      Item.Expanded := not Item.Expanded;
    Row := GridCoord.Y;
  end;
end;

function TJvgInspectorGrid.RowToItem(ARow: Integer): TJvgGridItem;
var
  I, Index: Integer;
begin
  Index := 0;
  //  I := 0;
  Result := nil;
  if Items.Count = 0 then
    Exit;
  for I := 0 to ARow - 1 do
  begin
    if Items[I].Expanded then
      Inc(Index, Items[I].Values.Count);
    Inc(Index);
    if Index > ARow then
      Break;
  end;

  Result := Items[I];
  Result.Sequence := ARow - ItemToRow(Result);
end;

function TJvgInspectorGrid.ItemToRow(Item: TJvgGridItem): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Item.Index - 1 do
  begin
    if Items[I].Expanded then
      Inc(Result, Items[I].Values.Count);
    Inc(Result);
  end;
end;

function TJvgInspectorGrid.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := True;
  if Assigned(OnSelectCell) then
    OnSelectCell(Self, ACol, ARow, Result);
  // if Items.FShowMultiValues and (Items[ARow].Values.Count > 1) then
 //   Result := False;
end;

function TJvgInspectorGrid.CanEditModify: Boolean;
var
  Item: TJvgGridItem;
begin
  Result := False;
  // (rom) deactivated  Result is already false
  //if (Col <> 1) or not (goEditing in Options) then
  //  Result := False;
  Item := RowToItem(Row);
  if Item <> nil then
    Result := not (Items.FShowMultiValues and (Item.Values.Count > 1) and (Item.Row = Row));
end;

procedure TJvgInspectorGrid.SetEditText(ACol, ARow: Integer; const Value: string);
var
  Item: TJvgGridItem;
begin
  inherited SetEditText(ACol, ARow, Value);
  if Assigned(OnSetEditText) then
    OnSetEditText(Self, ACol, ARow, Value);
  Item := RowToItem(ARow);
  if Item = nil then
    Exit;
  //  if Value <> Item.Values[Item.Sequence] then Item.Changed := True;// Values.Data :=
  Item.SetValue(Item.Sequence, Value);

  if Item.Sequence > 0 then
    Cells[1, Item.Row] := Item.GetValue;
  //  Item.Changed := Item.Changed or ;
    //Cells[1, ARow] := Value;
end;

procedure TJvgInspectorGrid.KeyPress(var Key: Char);
var
  Item: TJvgGridItem;
  I, OldRow: Integer;
begin
  inherited;
  Item := RowToItem(Row);
  if Item = nil then
    Exit;
  OldRow := Row;
  case Key of
    '+':
      if (Item.Values.Count > 0) and not Item.Expanded then
        Item.Expanded := True;
    '-':
      if (Item.Values.Count > 0) and Item.Expanded and (Row = ItemToRow(Item)) then
        Item.Expanded := False;
    '*':
      for I := 0 to Items.Count - 1 do
        Items[I].Expanded := True;
  end;
  Row := OldRow;
end;

procedure TJvgInspectorGrid.UndoAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].UndoAll;
end;

procedure TJvgInspectorGrid.UndoCurent;
begin
  RowToItem(Row).UndoAll;
end;

procedure TJvgInspectorGrid.WMSize(var Msg: TWMSize);
var
  I, FreeClientWidth: Integer;
begin
  inherited;
  FreeClientWidth := Width;
  I := GetScrollPos(handle, SB_VERT);
  if I <> 0 then
    Dec(FreeClientWidth, GetSystemMetrics(SM_CXHSCROLL) + 2);
  ColWidths[1] := FreeClientWidth - ColWidths[0];
end;

function TJvgInspectorGrid.GetEditMask(ACol, ARow: Integer): string;
var
  Item: TJvgGridItem;
begin
  Item := RowToItem(ARow);
  if Item = nil then
    Exit;
  if CanEditModify then
    Result := Item.EditMask
  else
    Result := '';
  if Assigned(OnGetEditMask) then
    OnGetEditMask(Self, ACol, ARow, Result);
end;

end.

