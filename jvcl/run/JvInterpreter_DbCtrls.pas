{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_DbCtrls.PAS, released on 2002-07-04.

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

unit JvInterpreter_DbCtrls;

{$I jvcl.inc}

interface

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes, Graphics, Db, DbCtrls;

{ TDBEdit }

{ constructor Create(AOwner: TComponent) }

procedure TDBEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBEdit_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBEdit(Args.Obj).Field);
end;

{ property Read DataField: string }

procedure TDBEdit_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBEdit(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBEdit_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBEdit(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBEdit_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBEdit(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBEdit_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBEdit(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBEdit_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBEdit(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBEdit_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBEdit(Args.Obj).ReadOnly := Value;
end;

{ TDBText }

{ constructor Create(AOwner: TComponent) }

procedure TDBText_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBText.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBText_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBText(Args.Obj).Field);
end;

{ property Read DataField: string }

procedure TDBText_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBText(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBText_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBText(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBText_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBText(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBText_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBText(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ TDBCheckBox }

{ constructor Create(AOwner: TComponent) }

procedure TDBCheckBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBCheckBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBCheckBox_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBCheckBox(Args.Obj).Field);
end;

{ property Read DataField: string }

procedure TDBCheckBox_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBCheckBox(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBCheckBox_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBCheckBox(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBCheckBox_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBCheckBox(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBCheckBox_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBCheckBox(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBCheckBox_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBCheckBox(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBCheckBox_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBCheckBox(Args.Obj).ReadOnly := Value;
end;

{ property Read ValueChecked: string }

procedure TDBCheckBox_Read_ValueChecked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBCheckBox(Args.Obj).ValueChecked;
end;

{ property Write ValueChecked(Value: string) }

procedure TDBCheckBox_Write_ValueChecked(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBCheckBox(Args.Obj).ValueChecked := Value;
end;

{ property Read ValueUnchecked: string }

procedure TDBCheckBox_Read_ValueUnchecked(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBCheckBox(Args.Obj).ValueUnchecked;
end;

{ property Write ValueUnchecked(Value: string) }

procedure TDBCheckBox_Write_ValueUnchecked(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBCheckBox(Args.Obj).ValueUnchecked := Value;
end;

{ TDBComboBox }

{ constructor Create(AOwner: TComponent) }

procedure TDBComboBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBComboBox_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBComboBox(Args.Obj).Field);
end;

{ property Read DataField: string }

procedure TDBComboBox_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBComboBox(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBComboBox_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBComboBox(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBComboBox_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBComboBox(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBComboBox_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBComboBox(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBComboBox_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBComboBox(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBComboBox_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBComboBox(Args.Obj).ReadOnly := Value;
end;

{ TDBListBox }

{ constructor Create(AOwner: TComponent) }

procedure TDBListBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBListBox_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBListBox(Args.Obj).Field);
end;

{ property Read DataField: string }

procedure TDBListBox_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBListBox(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBListBox_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBListBox(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBListBox_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBListBox(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBListBox_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBListBox(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBListBox_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBListBox(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBListBox_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBListBox(Args.Obj).ReadOnly := Value;
end;

{ TDBRadioGroup }

{ constructor Create(AOwner: TComponent) }

procedure TDBRadioGroup_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRadioGroup.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Field: TField }

procedure TDBRadioGroup_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRadioGroup(Args.Obj).Field);
end;

{ property Read Value: string }

procedure TDBRadioGroup_Read_Value(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRadioGroup(Args.Obj).Value;
end;

{ property Write Value(Value: string) }

procedure TDBRadioGroup_Write_Value(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRadioGroup(Args.Obj).Value := Value;
end;

{ property Read DataField: string }

procedure TDBRadioGroup_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRadioGroup(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBRadioGroup_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRadioGroup(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBRadioGroup_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRadioGroup(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBRadioGroup_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRadioGroup(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBRadioGroup_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRadioGroup(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBRadioGroup_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRadioGroup(Args.Obj).ReadOnly := Value;
end;

{ property Read Values: TStrings }

procedure TDBRadioGroup_Read_Values(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRadioGroup(Args.Obj).Values);
end;

{ property Write Values(Value: TStrings) }

procedure TDBRadioGroup_Write_Values(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRadioGroup(Args.Obj).Values := V2O(Value) as TStrings;
end;

{ TDBMemo }

{ constructor Create(AOwner: TComponent) }

procedure TDBMemo_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBMemo.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure LoadMemo; }

procedure TDBMemo_LoadMemo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBMemo(Args.Obj).LoadMemo;
end;

{ property Read Field: TField }

procedure TDBMemo_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBMemo(Args.Obj).Field);
end;

{ property Read AutoDisplay: Boolean }

procedure TDBMemo_Read_AutoDisplay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBMemo(Args.Obj).AutoDisplay;
end;

{ property Write AutoDisplay(Value: Boolean) }

procedure TDBMemo_Write_AutoDisplay(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBMemo(Args.Obj).AutoDisplay := Value;
end;

{ property Read DataField: string }

procedure TDBMemo_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBMemo(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBMemo_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBMemo(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBMemo_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBMemo(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBMemo_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBMemo(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBMemo_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBMemo(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBMemo_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBMemo(Args.Obj).ReadOnly := Value;
end;

{ TDBImage }

{ constructor Create(AOwner: TComponent) }

procedure TDBImage_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBImage.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure CopyToClipboard; }

procedure TDBImage_CopyToClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).CopyToClipboard;
end;

{ procedure CutToClipboard; }

procedure TDBImage_CutToClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).CutToClipboard;
end;

{ procedure LoadPicture; }

procedure TDBImage_LoadPicture(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).LoadPicture;
end;

{ procedure PasteFromClipboard; }

procedure TDBImage_PasteFromClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).PasteFromClipboard;
end;

{ property Read Field: TField }

procedure TDBImage_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBImage(Args.Obj).Field);
end;

{ property Read Picture: TPicture }

procedure TDBImage_Read_Picture(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBImage(Args.Obj).Picture);
end;

{ property Write Picture(Value: TPicture) }

procedure TDBImage_Write_Picture(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).Picture := V2O(Value) as TPicture;
end;

{ property Read AutoDisplay: Boolean }

procedure TDBImage_Read_AutoDisplay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).AutoDisplay;
end;

{ property Write AutoDisplay(Value: Boolean) }

procedure TDBImage_Write_AutoDisplay(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).AutoDisplay := Value;
end;

{ property Read BorderStyle: TBorderStyle }

procedure TDBImage_Read_BorderStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).BorderStyle;
end;

{ property Write BorderStyle(Value: TBorderStyle) }

procedure TDBImage_Write_BorderStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).BorderStyle := Value;
end;

{ property Read Center: Boolean }

procedure TDBImage_Read_Center(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).Center;
end;

{ property Write Center(Value: Boolean) }

procedure TDBImage_Write_Center(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).Center := Value;
end;

{ property Read DataField: string }

procedure TDBImage_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBImage_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBImage_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBImage(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBImage_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBImage_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBImage_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).ReadOnly := Value;
end;

{ property Read QuickDraw: Boolean }

procedure TDBImage_Read_QuickDraw(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).QuickDraw;
end;

{ property Write QuickDraw(Value: Boolean) }

procedure TDBImage_Write_QuickDraw(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).QuickDraw := Value;
end;

{ property Read Stretch: Boolean }

procedure TDBImage_Read_Stretch(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBImage(Args.Obj).Stretch;
end;

{ property Write Stretch(Value: Boolean) }

procedure TDBImage_Write_Stretch(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBImage(Args.Obj).Stretch := Value;
end;

{ TDBNavigator }

{ constructor Create(AOwner: TComponent) }

procedure TDBNavigator_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBNavigator.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); }

procedure TDBNavigator_SetBounds(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).SetBounds(Args.Values[0], Args.Values[1], Args.Values[2], Args.Values[3]);
end;

{ procedure BtnClick(Index: TNavigateBtn); }

procedure TDBNavigator_BtnClick(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).BtnClick(Args.Values[0]);
end;

{ property Read DataSource: TDataSource }

procedure TDBNavigator_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBNavigator(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBNavigator_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read VisibleButtons: TButtonSet }

procedure TDBNavigator_Read_VisibleButtons(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := S2V(Word(TDBNavigator(Args.Obj).VisibleButtons));
end;

{ property Write VisibleButtons(Value: TButtonSet) }

procedure TDBNavigator_Write_VisibleButtons(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).VisibleButtons := TButtonSet(Word(V2S(Value)))
end;

{ property Read Flat: Boolean }

procedure TDBNavigator_Read_Flat(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBNavigator(Args.Obj).Flat;
end;

{ property Write Flat(Value: Boolean) }

procedure TDBNavigator_Write_Flat(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).Flat := Value;
end;

{ property Read Hints: TStrings }

procedure TDBNavigator_Read_Hints(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBNavigator(Args.Obj).Hints);
end;

{ property Write Hints(Value: TStrings) }

procedure TDBNavigator_Write_Hints(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).Hints := V2O(Value) as TStrings;
end;

{ property Read ConfirmDelete: Boolean }

procedure TDBNavigator_Read_ConfirmDelete(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBNavigator(Args.Obj).ConfirmDelete;
end;

{ property Write ConfirmDelete(Value: Boolean) }

procedure TDBNavigator_Write_ConfirmDelete(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBNavigator(Args.Obj).ConfirmDelete := Value;
end;

{ TDBLookupListBox }

{ constructor Create(AOwner: TComponent) }

procedure TDBLookupListBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBLookupListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read SelectedItem: string }

procedure TDBLookupListBox_Read_SelectedItem(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupListBox(Args.Obj).SelectedItem;
end;

{ property Read BorderStyle: TBorderStyle }

procedure TDBLookupListBox_Read_BorderStyle(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupListBox(Args.Obj).BorderStyle;
end;

{ property Write BorderStyle(Value: TBorderStyle) }

procedure TDBLookupListBox_Write_BorderStyle(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupListBox(Args.Obj).BorderStyle := Value;
end;

{ property Read RowCount: Integer }

procedure TDBLookupListBox_Read_RowCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupListBox(Args.Obj).RowCount;
end;

{ property Write RowCount(Value: Integer) }

procedure TDBLookupListBox_Write_RowCount(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupListBox(Args.Obj).RowCount := Value;
end;

{ TDBLookupComboBox }

{ constructor Create(AOwner: TComponent) }

procedure TDBLookupComboBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBLookupComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure CloseUp(Accept: Boolean); }

procedure TDBLookupComboBox_CloseUp(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupComboBox(Args.Obj).CloseUp(Args.Values[0]);
end;

{ procedure DropDown; }

procedure TDBLookupComboBox_DropDown(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupComboBox(Args.Obj).DropDown;
end;

{ property Read ListVisible: Boolean }

procedure TDBLookupComboBox_Read_ListVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupComboBox(Args.Obj).ListVisible;
end;

{ property Read Text: string }

procedure TDBLookupComboBox_Read_Text(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupComboBox(Args.Obj).Text;
end;

{ property Read DropDownAlign: TDropDownAlign }

procedure TDBLookupComboBox_Read_DropDownAlign(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupComboBox(Args.Obj).DropDownAlign;
end;

{ property Write DropDownAlign(Value: TDropDownAlign) }

procedure TDBLookupComboBox_Write_DropDownAlign(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupComboBox(Args.Obj).DropDownAlign := Value;
end;

{ property Read DropDownRows: Integer }

procedure TDBLookupComboBox_Read_DropDownRows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupComboBox(Args.Obj).DropDownRows;
end;

{ property Write DropDownRows(Value: Integer) }

procedure TDBLookupComboBox_Write_DropDownRows(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupComboBox(Args.Obj).DropDownRows := Value;
end;

{ property Read DropDownWidth: Integer }

procedure TDBLookupComboBox_Read_DropDownWidth(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBLookupComboBox(Args.Obj).DropDownWidth;
end;

{ property Write DropDownWidth(Value: Integer) }

procedure TDBLookupComboBox_Write_DropDownWidth(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBLookupComboBox(Args.Obj).DropDownWidth := Value;
end;

{ TDBRichEdit }

{ constructor Create(AOwner: TComponent) }

procedure TDBRichEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRichEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure LoadMemo; }

procedure TDBRichEdit_LoadMemo(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRichEdit(Args.Obj).LoadMemo;
end;

{ property Read Field: TField }

procedure TDBRichEdit_Read_Field(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRichEdit(Args.Obj).Field);
end;

{ property Read AutoDisplay: Boolean }

procedure TDBRichEdit_Read_AutoDisplay(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRichEdit(Args.Obj).AutoDisplay;
end;

{ property Write AutoDisplay(Value: Boolean) }

procedure TDBRichEdit_Write_AutoDisplay(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRichEdit(Args.Obj).AutoDisplay := Value;
end;

{ property Read DataField: string }

procedure TDBRichEdit_Read_DataField(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRichEdit(Args.Obj).DataField;
end;

{ property Write DataField(Value: string) }

procedure TDBRichEdit_Write_DataField(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRichEdit(Args.Obj).DataField := Value;
end;

{ property Read DataSource: TDataSource }

procedure TDBRichEdit_Read_DataSource(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TDBRichEdit(Args.Obj).DataSource);
end;

{ property Write DataSource(Value: TDataSource) }

procedure TDBRichEdit_Write_DataSource(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRichEdit(Args.Obj).DataSource := V2O(Value) as TDataSource;
end;

{ property Read ReadOnly: Boolean }

procedure TDBRichEdit_Read_ReadOnly(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TDBRichEdit(Args.Obj).ReadOnly;
end;

{ property Write ReadOnly(Value: Boolean) }

procedure TDBRichEdit_Write_ReadOnly(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TDBRichEdit(Args.Obj).ReadOnly := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cDbCtrls = 'DbCtrls';
begin
  with JvInterpreterAdapter do
  begin
    { TDBEdit }
    AddClass(cDbCtrls, TDBEdit, 'TDBEdit');
    AddGet(TDBEdit, 'Create', TDBEdit_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBEdit, 'Field', TDBEdit_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBEdit, 'DataField', TDBEdit_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBEdit, 'DataField', TDBEdit_Write_DataField, 0, [varEmpty]);
    AddGet(TDBEdit, 'DataSource', TDBEdit_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBEdit, 'DataSource', TDBEdit_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBEdit, 'ReadOnly', TDBEdit_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBEdit, 'ReadOnly', TDBEdit_Write_ReadOnly, 0, [varEmpty]);
    { TDBText }
    AddClass(cDbCtrls, TDBText, 'TDBText');
    AddGet(TDBText, 'Create', TDBText_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBText, 'Field', TDBText_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBText, 'DataField', TDBText_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBText, 'DataField', TDBText_Write_DataField, 0, [varEmpty]);
    AddGet(TDBText, 'DataSource', TDBText_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBText, 'DataSource', TDBText_Write_DataSource, 0, [varEmpty]);
    { TDBCheckBox }
    AddClass(cDbCtrls, TDBCheckBox, 'TDBCheckBox');
    AddGet(TDBCheckBox, 'Create', TDBCheckBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBCheckBox, 'Field', TDBCheckBox_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBCheckBox, 'DataField', TDBCheckBox_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBCheckBox, 'DataField', TDBCheckBox_Write_DataField, 0, [varEmpty]);
    AddGet(TDBCheckBox, 'DataSource', TDBCheckBox_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBCheckBox, 'DataSource', TDBCheckBox_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBCheckBox, 'ReadOnly', TDBCheckBox_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBCheckBox, 'ReadOnly', TDBCheckBox_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TDBCheckBox, 'ValueChecked', TDBCheckBox_Read_ValueChecked, 0, [varEmpty], varEmpty);
    AddSet(TDBCheckBox, 'ValueChecked', TDBCheckBox_Write_ValueChecked, 0, [varEmpty]);
    AddGet(TDBCheckBox, 'ValueUnchecked', TDBCheckBox_Read_ValueUnchecked, 0, [varEmpty], varEmpty);
    AddSet(TDBCheckBox, 'ValueUnchecked', TDBCheckBox_Write_ValueUnchecked, 0, [varEmpty]);
    { TDBComboBox }
    AddClass(cDbCtrls, TDBComboBox, 'TDBComboBox');
    AddGet(TDBComboBox, 'Create', TDBComboBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBComboBox, 'Field', TDBComboBox_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBComboBox, 'DataField', TDBComboBox_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBComboBox, 'DataField', TDBComboBox_Write_DataField, 0, [varEmpty]);
    AddGet(TDBComboBox, 'DataSource', TDBComboBox_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBComboBox, 'DataSource', TDBComboBox_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBComboBox, 'ReadOnly', TDBComboBox_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBComboBox, 'ReadOnly', TDBComboBox_Write_ReadOnly, 0, [varEmpty]);
    { TDBListBox }
    AddClass(cDbCtrls, TDBListBox, 'TDBListBox');
    AddGet(TDBListBox, 'Create', TDBListBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBListBox, 'Field', TDBListBox_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBListBox, 'DataField', TDBListBox_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBListBox, 'DataField', TDBListBox_Write_DataField, 0, [varEmpty]);
    AddGet(TDBListBox, 'DataSource', TDBListBox_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBListBox, 'DataSource', TDBListBox_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBListBox, 'ReadOnly', TDBListBox_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBListBox, 'ReadOnly', TDBListBox_Write_ReadOnly, 0, [varEmpty]);
    { TDBRadioGroup }
    AddClass(cDbCtrls, TDBRadioGroup, 'TDBRadioGroup');
    AddGet(TDBRadioGroup, 'Create', TDBRadioGroup_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBRadioGroup, 'Field', TDBRadioGroup_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBRadioGroup, 'Value', TDBRadioGroup_Read_Value, 0, [varEmpty], varEmpty);
    AddSet(TDBRadioGroup, 'Value', TDBRadioGroup_Write_Value, 0, [varEmpty]);
    AddGet(TDBRadioGroup, 'DataField', TDBRadioGroup_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBRadioGroup, 'DataField', TDBRadioGroup_Write_DataField, 0, [varEmpty]);
    AddGet(TDBRadioGroup, 'DataSource', TDBRadioGroup_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBRadioGroup, 'DataSource', TDBRadioGroup_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBRadioGroup, 'ReadOnly', TDBRadioGroup_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBRadioGroup, 'ReadOnly', TDBRadioGroup_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TDBRadioGroup, 'Values', TDBRadioGroup_Read_Values, 0, [varEmpty], varEmpty);
    AddSet(TDBRadioGroup, 'Values', TDBRadioGroup_Write_Values, 0, [varEmpty]);
    { TDBMemo }
    AddClass(cDbCtrls, TDBMemo, 'TDBMemo');
    AddGet(TDBMemo, 'Create', TDBMemo_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBMemo, 'LoadMemo', TDBMemo_LoadMemo, 0, [varEmpty], varEmpty);
    AddGet(TDBMemo, 'Field', TDBMemo_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBMemo, 'AutoDisplay', TDBMemo_Read_AutoDisplay, 0, [varEmpty], varEmpty);
    AddSet(TDBMemo, 'AutoDisplay', TDBMemo_Write_AutoDisplay, 0, [varEmpty]);
    AddGet(TDBMemo, 'DataField', TDBMemo_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBMemo, 'DataField', TDBMemo_Write_DataField, 0, [varEmpty]);
    AddGet(TDBMemo, 'DataSource', TDBMemo_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBMemo, 'DataSource', TDBMemo_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBMemo, 'ReadOnly', TDBMemo_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBMemo, 'ReadOnly', TDBMemo_Write_ReadOnly, 0, [varEmpty]);
    { TDBImage }
    AddClass(cDbCtrls, TDBImage, 'TDBImage');
    AddGet(TDBImage, 'Create', TDBImage_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBImage, 'CopyToClipboard', TDBImage_CopyToClipboard, 0, [varEmpty], varEmpty);
    AddGet(TDBImage, 'CutToClipboard', TDBImage_CutToClipboard, 0, [varEmpty], varEmpty);
    AddGet(TDBImage, 'LoadPicture', TDBImage_LoadPicture, 0, [varEmpty], varEmpty);
    AddGet(TDBImage, 'PasteFromClipboard', TDBImage_PasteFromClipboard, 0, [varEmpty], varEmpty);
    AddGet(TDBImage, 'Field', TDBImage_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBImage, 'Picture', TDBImage_Read_Picture, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'Picture', TDBImage_Write_Picture, 0, [varEmpty]);
    AddGet(TDBImage, 'AutoDisplay', TDBImage_Read_AutoDisplay, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'AutoDisplay', TDBImage_Write_AutoDisplay, 0, [varEmpty]);
    AddGet(TDBImage, 'BorderStyle', TDBImage_Read_BorderStyle, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'BorderStyle', TDBImage_Write_BorderStyle, 0, [varEmpty]);
    AddGet(TDBImage, 'Center', TDBImage_Read_Center, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'Center', TDBImage_Write_Center, 0, [varEmpty]);
    AddGet(TDBImage, 'DataField', TDBImage_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'DataField', TDBImage_Write_DataField, 0, [varEmpty]);
    AddGet(TDBImage, 'DataSource', TDBImage_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'DataSource', TDBImage_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBImage, 'ReadOnly', TDBImage_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'ReadOnly', TDBImage_Write_ReadOnly, 0, [varEmpty]);
    AddGet(TDBImage, 'QuickDraw', TDBImage_Read_QuickDraw, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'QuickDraw', TDBImage_Write_QuickDraw, 0, [varEmpty]);
    AddGet(TDBImage, 'Stretch', TDBImage_Read_Stretch, 0, [varEmpty], varEmpty);
    AddSet(TDBImage, 'Stretch', TDBImage_Write_Stretch, 0, [varEmpty]);
    { TDBNavigator }
    AddClass(cDbCtrls, TDBNavigator, 'TDBNavigator');
    AddGet(TDBNavigator, 'Create', TDBNavigator_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBNavigator, 'SetBounds', TDBNavigator_SetBounds, 4, [varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TDBNavigator, 'BtnClick', TDBNavigator_BtnClick, 1, [varEmpty], varEmpty);
    AddGet(TDBNavigator, 'DataSource', TDBNavigator_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBNavigator, 'DataSource', TDBNavigator_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBNavigator, 'VisibleButtons', TDBNavigator_Read_VisibleButtons, 0, [varEmpty], varEmpty);
    AddSet(TDBNavigator, 'VisibleButtons', TDBNavigator_Write_VisibleButtons, 0, [varEmpty]);
    AddGet(TDBNavigator, 'Flat', TDBNavigator_Read_Flat, 0, [varEmpty], varEmpty);
    AddSet(TDBNavigator, 'Flat', TDBNavigator_Write_Flat, 0, [varEmpty]);
    AddGet(TDBNavigator, 'Hints', TDBNavigator_Read_Hints, 0, [varEmpty], varEmpty);
    AddSet(TDBNavigator, 'Hints', TDBNavigator_Write_Hints, 0, [varEmpty]);
    AddGet(TDBNavigator, 'ConfirmDelete', TDBNavigator_Read_ConfirmDelete, 0, [varEmpty], varEmpty);
    AddSet(TDBNavigator, 'ConfirmDelete', TDBNavigator_Write_ConfirmDelete, 0, [varEmpty]);
    { TDBLookupListBox }
    AddClass(cDbCtrls, TDBLookupListBox, 'TDBLookupListBox');
    AddGet(TDBLookupListBox, 'Create', TDBLookupListBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBLookupListBox, 'SelectedItem', TDBLookupListBox_Read_SelectedItem, 0, [varEmpty], varEmpty);
    AddGet(TDBLookupListBox, 'BorderStyle', TDBLookupListBox_Read_BorderStyle, 0, [varEmpty], varEmpty);
    AddSet(TDBLookupListBox, 'BorderStyle', TDBLookupListBox_Write_BorderStyle, 0, [varEmpty]);
    AddGet(TDBLookupListBox, 'RowCount', TDBLookupListBox_Read_RowCount, 0, [varEmpty], varEmpty);
    AddSet(TDBLookupListBox, 'RowCount', TDBLookupListBox_Write_RowCount, 0, [varEmpty]);
    { TDBLookupComboBox }
    AddClass(cDbCtrls, TDBLookupComboBox, 'TDBLookupComboBox');
    AddGet(TDBLookupComboBox, 'Create', TDBLookupComboBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBLookupComboBox, 'CloseUp', TDBLookupComboBox_CloseUp, 1, [varEmpty], varEmpty);
    AddGet(TDBLookupComboBox, 'DropDown', TDBLookupComboBox_DropDown, 0, [varEmpty], varEmpty);
    AddGet(TDBLookupComboBox, 'ListVisible', TDBLookupComboBox_Read_ListVisible, 0, [varEmpty], varEmpty);
    AddGet(TDBLookupComboBox, 'Text', TDBLookupComboBox_Read_Text, 0, [varEmpty], varEmpty);
    AddGet(TDBLookupComboBox, 'DropDownAlign', TDBLookupComboBox_Read_DropDownAlign, 0, [varEmpty], varEmpty);
    AddSet(TDBLookupComboBox, 'DropDownAlign', TDBLookupComboBox_Write_DropDownAlign, 0, [varEmpty]);
    AddGet(TDBLookupComboBox, 'DropDownRows', TDBLookupComboBox_Read_DropDownRows, 0, [varEmpty], varEmpty);
    AddSet(TDBLookupComboBox, 'DropDownRows', TDBLookupComboBox_Write_DropDownRows, 0, [varEmpty]);
    AddGet(TDBLookupComboBox, 'DropDownWidth', TDBLookupComboBox_Read_DropDownWidth, 0, [varEmpty], varEmpty);
    AddSet(TDBLookupComboBox, 'DropDownWidth', TDBLookupComboBox_Write_DropDownWidth, 0, [varEmpty]);
    { TDBRichEdit }
    AddClass(cDbCtrls, TDBRichEdit, 'TDBRichEdit');
    AddGet(TDBRichEdit, 'Create', TDBRichEdit_Create, 1, [varEmpty], varEmpty);
    AddGet(TDBRichEdit, 'LoadMemo', TDBRichEdit_LoadMemo, 0, [varEmpty], varEmpty);
    AddGet(TDBRichEdit, 'Field', TDBRichEdit_Read_Field, 0, [varEmpty], varEmpty);
    AddGet(TDBRichEdit, 'AutoDisplay', TDBRichEdit_Read_AutoDisplay, 0, [varEmpty], varEmpty);
    AddSet(TDBRichEdit, 'AutoDisplay', TDBRichEdit_Write_AutoDisplay, 0, [varEmpty]);
    AddGet(TDBRichEdit, 'DataField', TDBRichEdit_Read_DataField, 0, [varEmpty], varEmpty);
    AddSet(TDBRichEdit, 'DataField', TDBRichEdit_Write_DataField, 0, [varEmpty]);
    AddGet(TDBRichEdit, 'DataSource', TDBRichEdit_Read_DataSource, 0, [varEmpty], varEmpty);
    AddSet(TDBRichEdit, 'DataSource', TDBRichEdit_Write_DataSource, 0, [varEmpty]);
    AddGet(TDBRichEdit, 'ReadOnly', TDBRichEdit_Read_ReadOnly, 0, [varEmpty], varEmpty);
    AddSet(TDBRichEdit, 'ReadOnly', TDBRichEdit_Write_ReadOnly, 0, [varEmpty]);
  end;
  RegisterCLasses([TDBEdit, TDBText, TDBCheckBox, TDBComboBox, TDBListBox,
    TDBRadioGroup, TDBMemo, TDBImage, TDBNavigator, TDBLookupListBox,
      TDBLookupComboBox, TDBRichEdit]);
end;

end.

