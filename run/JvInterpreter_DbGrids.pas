{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_DbGrids.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvInterpreter_DbGrids;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes, Graphics, Menus, Grids, Db, DbGrids,
  JvInterpreter_Windows;

{ TColumnTitle }

{ constructor Create(Column: TColumn) }

procedure TColumnTitle_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumnTitle.Create(V2O(Args.Values[0]) as TColumn));
end;

{ procedure Assign(Source: TPersistent); }

procedure TColumnTitle_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function DefaultAlignment: TAlignment; }

procedure TColumnTitle_DefaultAlignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).DefaultAlignment;
end;

{ function DefaultColor: TColor; }

procedure TColumnTitle_DefaultColor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).DefaultColor;
end;

{ function DefaultFont: TFont; }

procedure TColumnTitle_DefaultFont(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumnTitle(Args.Obj).DefaultFont);
end;

{ function DefaultCaption: string; }

procedure TColumnTitle_DefaultCaption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).DefaultCaption;
end;

{ procedure RestoreDefaults; }

procedure TColumnTitle_RestoreDefaults(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).RestoreDefaults;
end;

{ property Read Alignment: TAlignment }

procedure TColumnTitle_Read_Alignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).Alignment;
end;

{ property Write Alignment(Value: TAlignment) }

procedure TColumnTitle_Write_Alignment(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).Alignment := Value;
end;

{ property Read Caption: string }

procedure TColumnTitle_Read_Caption(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).Caption;
end;

{ property Write Caption(Value: string) }

procedure TColumnTitle_Write_Caption(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).Caption := Value;
end;

{ property Read Color: TColor }

procedure TColumnTitle_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumnTitle(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TColumnTitle_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).Color := Value;
end;

{ property Read Font: TFont }

procedure TColumnTitle_Read_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumnTitle(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }

procedure TColumnTitle_Write_Font(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumnTitle(Args.Obj).Font := V2O(Value) as TFont;
end;

{ TColumn }

{ constructor Create(Collection: TCollection) }

procedure TColumn_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn.Create(V2O(Args.Values[0]) as TCollection));
end;

{ procedure Assign(Source: TPersistent); }

procedure TColumn_Assign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Assign(V2O(Args.Values[0]) as TPersistent);
end;

{ function DefaultAlignment: TAlignment; }

procedure TColumn_DefaultAlignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultAlignment;
end;

{ function DefaultColor: TColor; }

procedure TColumn_DefaultColor(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultColor;
end;

{ function DefaultFont: TFont; }

procedure TColumn_DefaultFont(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).DefaultFont);
end;

{ function DefaultImeMode: TImeMode; }

procedure TColumn_DefaultImeMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultImeMode;
end;

{ function DefaultImeName: TImeName; }

procedure TColumn_DefaultImeName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultImeName;
end;

{ function DefaultReadOnly: Boolean; }

procedure TColumn_DefaultReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultReadOnly;
end;

{ function DefaultWidth: Integer; }

procedure TColumn_DefaultWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).DefaultWidth;
end;

{ procedure RestoreDefaults; }

procedure TColumn_RestoreDefaults(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).RestoreDefaults;
end;

{ property Read Grid: TCustomDBGrid }

procedure TColumn_Read_Grid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).Grid);
end;

{ property Read AssignedValues: TColumnValues }

procedure TColumn_Read_AssignedValues(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TColumn(Args.Obj).AssignedValues));
end;

{ property Read Field: TField }

procedure TColumn_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).Field);
end;

{ property Write Field(Value: TField) }

procedure TColumn_Write_Field(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Field := V2O(Value) as TField;
end;

{ property Read Alignment: TAlignment }

procedure TColumn_Read_Alignment(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).Alignment;
end;

{ property Write Alignment(Value: TAlignment) }

procedure TColumn_Write_Alignment(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Alignment := Value;
end;

{ property Read ButtonStyle: TColumnButtonStyle }

procedure TColumn_Read_ButtonStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).ButtonStyle;
end;

{ property Write ButtonStyle(Value: TColumnButtonStyle) }

procedure TColumn_Write_ButtonStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).ButtonStyle := Value;
end;

{ property Read Color: TColor }

procedure TColumn_Read_Color(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).Color;
end;

{ property Write Color(Value: TColor) }

procedure TColumn_Write_Color(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Color := Value;
end;

{ property Read DropDownRows: Cardinal }

procedure TColumn_Read_DropDownRows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Integer(TColumn(Args.Obj).DropDownRows);
end;

{ property Write DropDownRows(Value: Cardinal) }

procedure TColumn_Write_DropDownRows(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).DropDownRows := Value;
end;

{ property Read FieldName: String }

procedure TColumn_Read_FieldName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).FieldName;
end;

{ property Write FieldName(Value: String) }

procedure TColumn_Write_FieldName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).FieldName := Value;
end;

{ property Read Font: TFont }

procedure TColumn_Read_Font(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).Font);
end;

{ property Write Font(Value: TFont) }

procedure TColumn_Write_Font(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Font := V2O(Value) as TFont;
end;

{ property Read ImeMode: TImeMode }

procedure TColumn_Read_ImeMode(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).ImeMode;
end;

{ property Write ImeMode(Value: TImeMode) }

procedure TColumn_Write_ImeMode(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).ImeMode := Value;
end;

{ property Read ImeName: TImeName }

procedure TColumn_Read_ImeName(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).ImeName;
end;

{ property Write ImeName(Value: TImeName) }

procedure TColumn_Write_ImeName(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).ImeName := Value;
end;

{ property Read PickList: TStrings }

procedure TColumn_Read_PickList(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).PickList);
end;

{ property Write PickList(Value: TStrings) }

procedure TColumn_Write_PickList(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).PickList := V2O(Value) as TStrings;
end;

{ property Read PopupMenu: TPopupMenu }

procedure TColumn_Read_PopupMenu(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).PopupMenu);
end;

{ property Write PopupMenu(Value: TPopupMenu) }

procedure TColumn_Write_PopupMenu(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).PopupMenu := V2O(Value) as TPopupMenu;
end;

{ property Read ReadOnly: Boolean }

procedure TColumn_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TColumn_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).ReadOnly := Value;
end;

{ property Read Title: TColumnTitle }

procedure TColumn_Read_Title(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TColumn(Args.Obj).Title);
end;

{ property Write Title(Value: TColumnTitle) }

procedure TColumn_Write_Title(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Title := V2O(Value) as TColumnTitle;
end;

{ property Read Width: Integer }

procedure TColumn_Read_Width(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TColumn(Args.Obj).Width;
end;

{ property Write Width(Value: Integer) }

procedure TColumn_Write_Width(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TColumn(Args.Obj).Width := Value;
end;

{ TDBGridColumns }

{ constructor Create(Grid: TCustomDBGrid; ColumnClass: TColumnClass) }

procedure TDBGridColumns_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBGridColumns.Create(V2O(Args.Values[0]) as TCustomDBGrid, TColumnClass(V2O(Args.Values[1]))));
end;

{ function Add: TColumn; }

procedure TDBGridColumns_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBGridColumns(Args.Obj).Add);
end;

{ procedure LoadFromFile(const Filename: string); }

procedure TDBGridColumns_LoadFromFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).LoadFromFile(Args.Values[0]);
end;

{ procedure LoadFromStream(S: TStream); }

procedure TDBGridColumns_LoadFromStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).LoadFromStream(V2O(Args.Values[0]) as TStream);
end;

{ procedure RestoreDefaults; }

procedure TDBGridColumns_RestoreDefaults(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).RestoreDefaults;
end;

{ procedure RebuildColumns; }

procedure TDBGridColumns_RebuildColumns(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).RebuildColumns;
end;

{ procedure SaveToFile(const Filename: string); }

procedure TDBGridColumns_SaveToFile(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).SaveToFile(Args.Values[0]);
end;

{ procedure SaveToStream(S: TStream); }

procedure TDBGridColumns_SaveToStream(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).SaveToStream(V2O(Args.Values[0]) as TStream);
end;

{ property Read State: TDBGridColumnsState }

procedure TDBGridColumns_Read_State(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBGridColumns(Args.Obj).State;
end;

{ property Write State(Value: TDBGridColumnsState) }

procedure TDBGridColumns_Write_State(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).State := Value;
end;

{ property Read Grid: TCustomDBGrid }

procedure TDBGridColumns_Read_Grid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBGridColumns(Args.Obj).Grid);
end;

{ property Read Items[Integer]: TColumn }

procedure TDBGridColumns_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBGridColumns(Args.Obj).Items[Args.Values[0]]);
end;

{ property Write Items[Integer]: TColumn }

procedure TDBGridColumns_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBGridColumns(Args.Obj).Items[Args.Values[0]] := V2O(Value) as TColumn;
end;

{ TBookmarkList }

{ constructor Create(AGrid: TCustomDBGrid) }

procedure TBookmarkList_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TBookmarkList.Create(V2O(Args.Values[0]) as TCustomDBGrid));
end;

{ procedure Clear; }

procedure TBookmarkList_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBookmarkList(Args.Obj).Clear;
end;

{ procedure Delete; }

procedure TBookmarkList_Delete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TBookmarkList(Args.Obj).Delete;
end;

{ function Find(const Item: TBookmarkStr; var Index: Integer): Boolean; }

procedure TBookmarkList_Find(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).Find(Args.Values[0], TVarData(Args.Values[1]).vInteger);
end;

{ function IndexOf(const Item: TBookmarkStr): Integer; }

procedure TBookmarkList_IndexOf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).IndexOf(Args.Values[0]);
end;

{ function Refresh: Boolean; }

procedure TBookmarkList_Refresh(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).Refresh;
end;

{ property Read Count: Integer }

procedure TBookmarkList_Read_Count(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).Count;
end;

{ property Read CurrentRowSelected: Boolean }

procedure TBookmarkList_Read_CurrentRowSelected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).CurrentRowSelected;
end;

{ property Write CurrentRowSelected(Value: Boolean) }

procedure TBookmarkList_Write_CurrentRowSelected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TBookmarkList(Args.Obj).CurrentRowSelected := Value;
end;

{ property Read Items[Integer]: TBookmarkStr }

procedure TBookmarkList_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TBookmarkList(Args.Obj).Items[Args.Values[0]];
end;

{ TCustomDBGrid }

{ procedure DefaultDrawDataCell(const Rect: TRect; Field: TField; State: TGridDrawState); }

procedure TCustomDBGrid_DefaultDrawDataCell(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomDBGrid(Args.Obj).DefaultDrawDataCell(Var2Rect(Args.Values[0]), V2O(Args.Values[1]) as TField,
    TGridDrawState(Byte(V2S(Args.Values[2]))));
end;

{ procedure DefaultDrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); }

procedure TCustomDBGrid_DefaultDrawColumnCell(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomDBGrid(Args.Obj).DefaultDrawColumnCell(Var2Rect(Args.Values[0]), Args.Values[1], V2O(Args.Values[2]) as
    TColumn, TGridDrawState(Byte(V2S(Args.Values[3]))));
end;

{ function ValidFieldIndex(FieldIndex: Integer): Boolean; }

procedure TCustomDBGrid_ValidFieldIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomDBGrid(Args.Obj).ValidFieldIndex(Args.Values[0]);
end;

{ property Read FieldCount: Integer }

procedure TCustomDBGrid_Read_FieldCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomDBGrid(Args.Obj).FieldCount;
end;

{ property Read Fields[Integer]: TField }

procedure TCustomDBGrid_Read_Fields(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomDBGrid(Args.Obj).Fields[Args.Values[0]]);
end;

{ property Read SelectedField: TField }

procedure TCustomDBGrid_Read_SelectedField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomDBGrid(Args.Obj).SelectedField);
end;

{ property Write SelectedField(Value: TField) }

procedure TCustomDBGrid_Write_SelectedField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomDBGrid(Args.Obj).SelectedField := V2O(Value) as TField;
end;

{ property Read SelectedIndex: Integer }

procedure TCustomDBGrid_Read_SelectedIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomDBGrid(Args.Obj).SelectedIndex;
end;

{ property Write SelectedIndex(Value: Integer) }

procedure TCustomDBGrid_Write_SelectedIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomDBGrid(Args.Obj).SelectedIndex := Value;
end;

{ TDBGrid }

{ constructor Create(AOwner: TComponent) }

procedure TDBGrid_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBGrid.Create(V2O(Args.Values[0]) as TComponent));
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cDbGrids = 'DbGrids';
begin
  with JvInterpreterAdapter do
  begin
    { TColumnValue }
    AddConst(cDbGrids, 'cvColor', Ord(cvColor));
    AddConst(cDbGrids, 'cvWidth', Ord(cvWidth));
    AddConst(cDbGrids, 'cvFont', Ord(cvFont));
    AddConst(cDbGrids, 'cvAlignment', Ord(cvAlignment));
    AddConst(cDbGrids, 'cvReadOnly', Ord(cvReadOnly));
    AddConst(cDbGrids, 'cvTitleColor', Ord(cvTitleColor));
    AddConst(cDbGrids, 'cvTitleCaption', Ord(cvTitleCaption));
    AddConst(cDbGrids, 'cvTitleAlignment', Ord(cvTitleAlignment));
    AddConst(cDbGrids, 'cvTitleFont', Ord(cvTitleFont));
    AddConst(cDbGrids, 'cvImeMode', Ord(cvImeMode));
    AddConst(cDbGrids, 'cvImeName', Ord(cvImeName));
    { TColumnTitle }
    AddClass(cDbGrids, TColumnTitle, 'TColumnTitle');
    AddGet(TColumnTitle, 'Create', TColumnTitle_Create, 1, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'Assign', TColumnTitle_Assign, 1, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'DefaultAlignment', TColumnTitle_DefaultAlignment, 0, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'DefaultColor', TColumnTitle_DefaultColor, 0, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'DefaultFont', TColumnTitle_DefaultFont, 0, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'DefaultCaption', TColumnTitle_DefaultCaption, 0, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'RestoreDefaults', TColumnTitle_RestoreDefaults, 0, [varEmpty], varEmpty);
    AddGet(TColumnTitle, 'Alignment', TColumnTitle_Read_Alignment, 0, [varEmpty], varEmpty);
    AddSet(TColumnTitle, 'Alignment', TColumnTitle_Write_Alignment, 0, [varEmpty]);
    AddGet(TColumnTitle, 'Caption', TColumnTitle_Read_Caption, 0, [varEmpty], varEmpty);
    AddSet(TColumnTitle, 'Caption', TColumnTitle_Write_Caption, 0, [varEmpty]);
    AddGet(TColumnTitle, 'Color', TColumnTitle_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TColumnTitle, 'Color', TColumnTitle_Write_Color, 0, [varEmpty]);
    AddGet(TColumnTitle, 'Font', TColumnTitle_Read_Font, 0, [varEmpty], varEmpty);
    AddSet(TColumnTitle, 'Font', TColumnTitle_Write_Font, 0, [varEmpty]);
    { TColumnButtonStyle }
    AddConst(cDbGrids, 'cbsAuto', Ord(cbsAuto));
    AddConst(cDbGrids, 'cbsEllipsis', Ord(cbsEllipsis));
    AddConst(cDbGrids, 'cbsNone', Ord(cbsNone));
    { TColumn }
    AddClass(cDbGrids, TColumn, 'TColumn');
    AddGet(TColumn, 'Create', TColumn_Create, 1, [varEmpty], varEmpty);
    AddGet(TColumn, 'Assign', TColumn_Assign, 1, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultAlignment', TColumn_DefaultAlignment, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultColor', TColumn_DefaultColor, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultFont', TColumn_DefaultFont, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultImeMode', TColumn_DefaultImeMode, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultImeName', TColumn_DefaultImeName, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultReadOnly', TColumn_DefaultReadOnly, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'DefaultWidth', TColumn_DefaultWidth, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'RestoreDefaults', TColumn_RestoreDefaults, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'Grid', TColumn_Read_Grid, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'AssignedValues', TColumn_Read_AssignedValues, 0, [varEmpty], varEmpty);
    AddGet(TColumn, 'Field', TColumn_Read_Field, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Field', TColumn_Write_Field, 0, [varEmpty]);
    AddGet(TColumn, 'Alignment', TColumn_Read_Alignment, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Alignment', TColumn_Write_Alignment, 0, [varEmpty]);
    AddGet(TColumn, 'ButtonStyle', TColumn_Read_ButtonStyle, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'ButtonStyle', TColumn_Write_ButtonStyle, 0, [varEmpty]);
    AddGet(TColumn, 'Color', TColumn_Read_Color, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Color', TColumn_Write_Color, 0, [varEmpty]);
    AddGet(TColumn, 'DropDownRows', TColumn_Read_DropDownRows, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'DropDownRows', TColumn_Write_DropDownRows, 0, [varEmpty]);
    AddGet(TColumn, 'FieldName', TColumn_Read_FieldName, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'FieldName', TColumn_Write_FieldName, 0, [varEmpty]);
    AddGet(TColumn, 'Font', TColumn_Read_Font, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Font', TColumn_Write_Font, 0, [varEmpty]);
    AddGet(TColumn, 'ImeMode', TColumn_Read_ImeMode, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'ImeMode', TColumn_Write_ImeMode, 0, [varEmpty]);
    AddGet(TColumn, 'ImeName', TColumn_Read_ImeName, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'ImeName', TColumn_Write_ImeName, 0, [varEmpty]);
    AddGet(TColumn, 'PickList', TColumn_Read_PickList, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'PickList', TColumn_Write_PickList, 0, [varEmpty]);
    AddGet(TColumn, 'PopupMenu', TColumn_Read_PopupMenu, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'PopupMenu', TColumn_Write_PopupMenu, 0, [varEmpty]);
    AddGet(TColumn, 'ReadOnly', TColumn_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'ReadOnly', TColumn_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TColumn, 'Title', TColumn_Read_Title, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Title', TColumn_Write_Title, 0, [varEmpty]);
    AddGet(TColumn, 'Width', TColumn_Read_Width, 0, [varEmpty], varEmpty);
    AddSet(TColumn, 'Width', TColumn_Write_Width, 0, [varEmpty]);
    { TDBGridColumnsState }
    AddConst(cDbGrids, 'csDefault', Ord(csDefault));
    AddConst(cDbGrids, 'csCustomized', Ord(csCustomized));
    { TDBGridColumns }
    AddClass(cDbGrids, TDBGridColumns, 'TDBGridColumns');
    AddGet(TDBGridColumns, 'Create', TDBGridColumns_Create, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'Add', TDBGridColumns_Add, 0, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'LoadFromFile', TDBGridColumns_LoadFromFile, 1, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'LoadFromStream', TDBGridColumns_LoadFromStream, 1, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'RestoreDefaults', TDBGridColumns_RestoreDefaults, 0, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'RebuildColumns', TDBGridColumns_RebuildColumns, 0, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'SaveToFile', TDBGridColumns_SaveToFile, 1, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'SaveToStream', TDBGridColumns_SaveToStream, 1, [varEmpty], varEmpty);
    AddGet(TDBGridColumns, 'State', TDBGridColumns_Read_State, 0, [varEmpty], varEmpty);
    AddSet(TDBGridColumns, 'State', TDBGridColumns_Write_State, 0, [varEmpty]);
    AddGet(TDBGridColumns, 'Grid', TDBGridColumns_Read_Grid, 0, [varEmpty], varEmpty);
    AddIGet(TDBGridColumns, 'Items', TDBGridColumns_Read_Items, 1, [varEmpty], varEmpty);
    AddISet(TDBGridColumns, 'Items', TDBGridColumns_Write_Items, 1, [varNull]);
    AddIDGet(TDBGridColumns, TDBGridColumns_Read_Items, 1, [varEmpty], varEmpty);
    AddIDSet(TDBGridColumns, TDBGridColumns_Write_Items, 1, [varNull]);
    { TBookmarkList }
    AddClass(cDbGrids, TBookmarkList, 'TBookmarkList');
    AddGet(TBookmarkList, 'Create', TBookmarkList_Create, 1, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'Clear', TBookmarkList_Clear, 0, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'Delete', TBookmarkList_Delete, 0, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'Find', TBookmarkList_Find, 2, [varEmpty, varByRef], varEmpty);
    AddGet(TBookmarkList, 'IndexOf', TBookmarkList_IndexOf, 1, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'Refresh', TBookmarkList_Refresh, 0, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'Count', TBookmarkList_Read_Count, 0, [varEmpty], varEmpty);
    AddGet(TBookmarkList, 'CurrentRowSelected', TBookmarkList_Read_CurrentRowSelected, 0, [varEmpty], varEmpty);
    AddSet(TBookmarkList, 'CurrentRowSelected', TBookmarkList_Write_CurrentRowSelected, 0, [varEmpty]);
    AddIGet(TBookmarkList, 'Items', TBookmarkList_Read_Items, 1, [varEmpty], varEmpty);
    AddIDGet(TBookmarkList, TBookmarkList_Read_Items, 1, [varEmpty], varEmpty);
    { TDBGridOption }
    AddConst(cDbGrids, 'dgEditing', Ord(dgEditing));
    AddConst(cDbGrids, 'dgAlwaysShowEditor', Ord(dgAlwaysShowEditor));
    AddConst(cDbGrids, 'dgTitles', Ord(dgTitles));
    AddConst(cDbGrids, 'dgIndicator', Ord(dgIndicator));
    AddConst(cDbGrids, 'dgColumnResize', Ord(dgColumnResize));
    AddConst(cDbGrids, 'dgColLines', Ord(dgColLines));
    AddConst(cDbGrids, 'dgRowLines', Ord(dgRowLines));
    AddConst(cDbGrids, 'dgTabs', Ord(dgTabs));
    AddConst(cDbGrids, 'dgRowSelect', Ord(dgRowSelect));
    AddConst(cDbGrids, 'dgAlwaysShowSelection', Ord(dgAlwaysShowSelection));
    AddConst(cDbGrids, 'dgConfirmDelete', Ord(dgConfirmDelete));
    AddConst(cDbGrids, 'dgCancelOnExit', Ord(dgCancelOnExit));
    AddConst(cDbGrids, 'dgMultiSelect', Ord(dgMultiSelect));
    { TCustomDBGrid }
    AddClass(cDbGrids, TCustomDBGrid, 'TCustomDBGrid');
    AddGet(TCustomDBGrid, 'DefaultDrawDataCell', TCustomDBGrid_DefaultDrawDataCell, 3, [varEmpty, varEmpty, varEmpty],
      varEmpty);
    AddGet(TCustomDBGrid, 'DefaultDrawColumnCell', TCustomDBGrid_DefaultDrawColumnCell, 4, [varEmpty, varEmpty,
      varEmpty, varEmpty], varEmpty);
    AddGet(TCustomDBGrid, 'ValidFieldIndex', TCustomDBGrid_ValidFieldIndex, 1, [varEmpty], varEmpty);
    AddGet(TCustomDBGrid, 'FieldCount', TCustomDBGrid_Read_FieldCount, 0, [varEmpty], varEmpty);
    AddIGet(TCustomDBGrid, 'Fields', TCustomDBGrid_Read_Fields, 1, [varEmpty], varEmpty);
    AddGet(TCustomDBGrid, 'SelectedField', TCustomDBGrid_Read_SelectedField, 0, [varEmpty], varEmpty);
    AddSet(TCustomDBGrid, 'SelectedField', TCustomDBGrid_Write_SelectedField, 0, [varEmpty]);
    AddGet(TCustomDBGrid, 'SelectedIndex', TCustomDBGrid_Read_SelectedIndex, 0, [varEmpty], varEmpty);
    AddSet(TCustomDBGrid, 'SelectedIndex', TCustomDBGrid_Write_SelectedIndex, 0, [varEmpty]);
    { TDBGrid }
    AddClass(cDbGrids, TDBGrid, 'TDBGrid');
    AddGet(TDBGrid, 'Create', TDBGrid_Create, 1, [varEmpty], varEmpty);
  end;
  RegisterClasses([TDBGrid]);
end;

end.

