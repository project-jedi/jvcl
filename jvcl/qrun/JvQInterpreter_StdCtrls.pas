{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_StdCtrls.PAS, released on 2002-07-04.

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

{$I jvcl.inc}

unit JvQInterpreter_StdCtrls;

interface

uses
  JvQInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  QWindows, Classes, QControls, QStdCtrls,
  JvQInterpreter_Windows;

{ TGroupBox }

{ constructor Create(AOwner: TComponent) }

procedure TGroupBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TGroupBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomLabel }

{ constructor Create(AOwner: TComponent) }

procedure TCustomLabel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomLabel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Canvas: TCanvas }

procedure TCustomLabel_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomLabel(Args.Obj).Canvas);
end;

{ TLabel }

{ constructor Create(AOwner: TComponent) }

procedure TLabel_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TLabel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomEdit }

{ constructor Create(AOwner: TComponent) }

procedure TCustomEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Clear; }

procedure TCustomEdit_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).Clear;
end;

{ procedure ClearSelection; }

procedure TCustomEdit_ClearSelection(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).ClearSelection;
end;

{ procedure CopyToClipboard; }

procedure TCustomEdit_CopyToClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).CopyToClipboard;
end;

{ procedure CutToClipboard; }

procedure TCustomEdit_CutToClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).CutToClipboard;
end;

{ procedure PasteFromClipboard; }

procedure TCustomEdit_PasteFromClipboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).PasteFromClipboard;
end;

{ function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; }

procedure TCustomEdit_GetSelTextBuf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomEdit(Args.Obj).GetSelTextBuf(PChar(string(Args.Values[0])), Args.Values[1]);
end;

{ procedure SelectAll; }

procedure TCustomEdit_SelectAll(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).SelectAll;
end;

{ procedure SetSelTextBuf(Buffer: PChar); }

procedure TCustomEdit_SetSelTextBuf(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).SetSelTextBuf(PChar(string(Args.Values[0])));
end;

{ property Read Modified: Boolean }

procedure TCustomEdit_Read_Modified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomEdit(Args.Obj).Modified;
end;

{ property Write Modified(Value: Boolean) }

procedure TCustomEdit_Write_Modified(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).Modified := Value;
end;

{ property Read SelLength: Integer }

procedure TCustomEdit_Read_SelLength(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomEdit(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: Integer) }

procedure TCustomEdit_Write_SelLength(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).SelLength := Value;
end;

{ property Read SelStart: Integer }

procedure TCustomEdit_Read_SelStart(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomEdit(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: Integer) }

procedure TCustomEdit_Write_SelStart(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).SelStart := Value;
end;

{ property Read SelText: string }

procedure TCustomEdit_Read_SelText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomEdit(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }

procedure TCustomEdit_Write_SelText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomEdit(Args.Obj).SelText := Value;
end;

{ TEdit }

{ constructor Create(AOwner: TComponent) }

procedure TEdit_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomMemo }

{ constructor Create(AOwner: TComponent) }

procedure TCustomMemo_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomMemo.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Lines: TStrings }

procedure TCustomMemo_Read_Lines(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomMemo(Args.Obj).Lines);
end;

{ property Write Lines(Value: TStrings) }

procedure TCustomMemo_Write_Lines(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomMemo(Args.Obj).Lines := V2O(Value) as TStrings;
end;

{ TMemo }

{ constructor Create(AOwner: TComponent) }

procedure TMemo_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TMemo.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomComboBox }

{ constructor Create(AOwner: TComponent) }

procedure TCustomComboBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Clear; }

procedure TCustomComboBox_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).Clear;
end;

{ procedure SelectAll; }

procedure TCustomComboBox_SelectAll(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).SelectAll;
end;

{ property Read Canvas: TCanvas }

procedure TCustomComboBox_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomComboBox(Args.Obj).Canvas);
end;

{ property Read DroppedDown: Boolean }

procedure TCustomComboBox_Read_DroppedDown(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomComboBox(Args.Obj).DroppedDown;
end;

{ property Write DroppedDown(Value: Boolean) }

procedure TCustomComboBox_Write_DroppedDown(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).DroppedDown := Value;
end;

{ property Read Items: TStrings }

procedure TCustomComboBox_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomComboBox(Args.Obj).Items);
end;

{ property Write Items(Value: TStrings) }

procedure TCustomComboBox_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).Items := V2O(Value) as TStrings;
end;

{ property Read ItemIndex: Integer }

procedure TCustomComboBox_Read_ItemIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomComboBox(Args.Obj).ItemIndex;
end;

{ property Write ItemIndex(Value: Integer) }

procedure TCustomComboBox_Write_ItemIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).ItemIndex := Value;
end;

{ property Read SelLength: Integer }

procedure TCustomComboBox_Read_SelLength(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: Integer) }

procedure TCustomComboBox_Write_SelLength(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).SelLength := Value;
end;

{ property Read SelStart: Integer }

procedure TCustomComboBox_Read_SelStart(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: Integer) }

procedure TCustomComboBox_Write_SelStart(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).SelStart := Value;
end;

{ property Read SelText: string }

procedure TCustomComboBox_Read_SelText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }

procedure TCustomComboBox_Write_SelText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomComboBox(Args.Obj).SelText := Value;
end;

{ TComboBox }

{ constructor Create(AOwner: TComponent) }

procedure TComboBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TButton }

{ constructor Create(AOwner: TComponent) }

procedure TButton_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TButton.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Click; }

procedure TButton_Click(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TButton(Args.Obj).Click;
end;

{ TCustomCheckBox }

{ constructor Create(AOwner: TComponent) }

procedure TCustomCheckBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomCheckBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCheckBox }

{ constructor Create(AOwner: TComponent) }

procedure TCheckBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCheckBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TRadioButton }

{ constructor Create(AOwner: TComponent) }

procedure TRadioButton_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TRadioButton.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TCustomListBox }

{ constructor Create(AOwner: TComponent) }

procedure TCustomListBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure Clear; }

procedure TCustomListBox_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListBox(Args.Obj).Clear;
end;

{ function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer; }

procedure TCustomListBox_ItemAtPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListBox(Args.Obj).ItemAtPos(Var2Point(Args.Values[0]), Args.Values[1]);
end;

{ function ItemRect(Index: Integer): TRect; }

procedure TCustomListBox_ItemRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TCustomListBox(Args.Obj).ItemRect(Args.Values[0]));
end;

{ property Read Canvas: TCanvas }

procedure TCustomListBox_Read_Canvas(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListBox(Args.Obj).Canvas);
end;

{ property Read Items: TStrings }

procedure TCustomListBox_Read_Items(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomListBox(Args.Obj).Items);
end;

{ property Write Items(Value: TStrings) }

procedure TCustomListBox_Write_Items(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListBox(Args.Obj).Items := V2O(Value) as TStrings;
end;

{ property Read ItemIndex: Integer }

procedure TCustomListBox_Read_ItemIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListBox(Args.Obj).ItemIndex;
end;

{ property Write ItemIndex(Value: Integer) }

procedure TCustomListBox_Write_ItemIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListBox(Args.Obj).ItemIndex := Value;
end;

{ property Read SelCount: Integer }

procedure TCustomListBox_Read_SelCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListBox(Args.Obj).SelCount;
end;

{ property Read Selected[Integer]: Boolean }

procedure TCustomListBox_Read_Selected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListBox(Args.Obj).Selected[Args.Values[0]];
end;

{ property Write Selected[Integer]: Boolean }

procedure TCustomListBox_Write_Selected(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListBox(Args.Obj).Selected[Args.Values[0]] := Value;
end;

{ property Read TopIndex: Integer }

procedure TCustomListBox_Read_TopIndex(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TCustomListBox(Args.Obj).TopIndex;
end;

{ property Write TopIndex(Value: Integer) }

procedure TCustomListBox_Write_TopIndex(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TCustomListBox(Args.Obj).TopIndex := Value;
end;

{ TListBox }

{ constructor Create(AOwner: TComponent) }

procedure TListBox_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TScrollBar }

{ constructor Create(AOwner: TComponent) }

procedure TScrollBar_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TScrollBar.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure SetParams(APosition, AMin, AMax: Integer); }

procedure TScrollBar_SetParams(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TScrollBar(Args.Obj).SetParams(Args.Values[0], Args.Values[1], Args.Values[2]);
end;



{ TCustomStaticText }

{ constructor Create(AOwner: TComponent) }

procedure TCustomStaticText_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TCustomStaticText.Create(V2O(Args.Values[0]) as TComponent));
end;

{ TStaticText }

{ constructor Create(AOwner: TComponent) }

procedure TStaticText_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TStaticText.Create(V2O(Args.Values[0]) as TComponent));
end;



type
  TJvInterpreterStdCtrlsEvent = class(TJvInterpreterEvent)
  private
    procedure DrawItemEvent(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure MeasureItemEvent(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  end;

procedure TJvInterpreterStdCtrlsEvent.DrawItemEvent(Control: TWinControl; Index: Integer; Rect: TRect; State:
  TOwnerDrawState);
begin
  CallFunction(nil, [O2V(Control), Index, Rect2Var(Rect), S2V(Word(State))]);
end;

procedure TJvInterpreterStdCtrlsEvent.MeasureItemEvent(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  CallFunction(nil, [O2V(Control), Index, Height]);
  Height := Args.Values[1];
end;

procedure TJvInterpreterStdCtrlsEvent.ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  CallFunction(nil, [O2V(Sender), S2V(Byte(ScrollCode)), ScrollPos]);
  ScrollPos := Args.Values[2];
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cStdCtrls = 'StdCtrls';
begin
  with JvInterpreterAdapter do
  begin
    { TGroupBox }
    AddClass(cStdCtrls, TGroupBox, 'TGroupBox');
    AddGet(TGroupBox, 'Create', TGroupBox_Create, 1, [varEmpty], varEmpty);
    
    { TTextLayout }
    AddConst(cStdCtrls, 'tlTop', Ord(tlTop));
    AddConst(cStdCtrls, 'tlCenter', Ord(tlCenter));
    AddConst(cStdCtrls, 'tlBottom', Ord(tlBottom));
    
    { TCustomLabel }
    AddClass(cStdCtrls, TCustomLabel, 'TCustomLabel');
    AddGet(TCustomLabel, 'Create', TCustomLabel_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomLabel, 'Canvas', TCustomLabel_Read_Canvas, 0, [varEmpty], varEmpty);
    { TLabel }
    AddClass(cStdCtrls, TLabel, 'TLabel');
    AddGet(TLabel, 'Create', TLabel_Create, 1, [varEmpty], varEmpty);
    { TEditCharCase }
    AddConst(cStdCtrls, 'ecNormal', Ord(ecNormal));
    AddConst(cStdCtrls, 'ecUpperCase', Ord(ecUpperCase));
    AddConst(cStdCtrls, 'ecLowerCase', Ord(ecLowerCase));
    { TCustomEdit }
    AddClass(cStdCtrls, TCustomEdit, 'TCustomEdit');
    AddGet(TCustomEdit, 'Create', TCustomEdit_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'Clear', TCustomEdit_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'ClearSelection', TCustomEdit_ClearSelection, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'CopyToClipboard', TCustomEdit_CopyToClipboard, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'CutToClipboard', TCustomEdit_CutToClipboard, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'PasteFromClipboard', TCustomEdit_PasteFromClipboard, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'GetSelTextBuf', TCustomEdit_GetSelTextBuf, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomEdit, 'SelectAll', TCustomEdit_SelectAll, 0, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'SetSelTextBuf', TCustomEdit_SetSelTextBuf, 1, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'Modified', TCustomEdit_Read_Modified, 0, [varEmpty], varEmpty);
    AddSet(TCustomEdit, 'Modified', TCustomEdit_Write_Modified, 0, [varEmpty]);
    AddGet(TCustomEdit, 'SelLength', TCustomEdit_Read_SelLength, 0, [varEmpty], varEmpty);
    AddSet(TCustomEdit, 'SelLength', TCustomEdit_Write_SelLength, 0, [varEmpty]);
    AddGet(TCustomEdit, 'SelStart', TCustomEdit_Read_SelStart, 0, [varEmpty], varEmpty);
    AddSet(TCustomEdit, 'SelStart', TCustomEdit_Write_SelStart, 0, [varEmpty]);
    AddGet(TCustomEdit, 'SelText', TCustomEdit_Read_SelText, 0, [varEmpty], varEmpty);
    AddSet(TCustomEdit, 'SelText', TCustomEdit_Write_SelText, 0, [varEmpty]);
    { TEdit }
    AddClass(cStdCtrls, TEdit, 'TEdit');
    AddGet(TEdit, 'Create', TEdit_Create, 1, [varEmpty], varEmpty);
    { TScrollStyle }
    AddConst(cStdCtrls, 'ssNone', Ord(ssNone));
    AddConst(cStdCtrls, 'ssHorizontal', Ord(ssHorizontal));
    AddConst(cStdCtrls, 'ssVertical', Ord(ssVertical));
    AddConst(cStdCtrls, 'ssBoth', Ord(ssBoth));
    { TCustomMemo }
    AddClass(cStdCtrls, TCustomMemo, 'TCustomMemo');
    AddGet(TCustomMemo, 'Create', TCustomMemo_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomMemo, 'Lines', TCustomMemo_Read_Lines, 0, [varEmpty], varEmpty);
    AddSet(TCustomMemo, 'Lines', TCustomMemo_Write_Lines, 0, [varEmpty]);
    { TMemo }
    AddClass(cStdCtrls, TMemo, 'TMemo');
    AddGet(TMemo, 'Create', TMemo_Create, 1, [varEmpty], varEmpty);
    { TComboBoxStyle }
    AddConst(cStdCtrls, 'csDropDown', Ord(csDropDown));
    AddConst(cStdCtrls, 'csSimple', Ord(csSimple));
    AddConst(cStdCtrls, 'csDropDownList', Ord(csDropDownList));
    AddConst(cStdCtrls, 'csOwnerDrawFixed', Ord(csOwnerDrawFixed));
    AddConst(cStdCtrls, 'csOwnerDrawVariable', Ord(csOwnerDrawVariable));
    { TOwnerDrawState }
    AddConst(cStdCtrls, 'odSelected', Ord(odSelected));
    AddConst(cStdCtrls, 'odGrayed', Ord(odGrayed));
    AddConst(cStdCtrls, 'odDisabled', Ord(odDisabled));
    AddConst(cStdCtrls, 'odChecked', Ord(odChecked));
    AddConst(cStdCtrls, 'odFocused', Ord(odFocused));
    { TCustomComboBox }
    AddClass(cStdCtrls, TCustomComboBox, 'TCustomComboBox');
    AddGet(TCustomComboBox, 'Create', TCustomComboBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomComboBox, 'Clear', TCustomComboBox_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCustomComboBox, 'SelectAll', TCustomComboBox_SelectAll, 0, [varEmpty], varEmpty);
    AddGet(TCustomComboBox, 'Canvas', TCustomComboBox_Read_Canvas, 0, [varEmpty], varEmpty);
    AddGet(TCustomComboBox, 'DroppedDown', TCustomComboBox_Read_DroppedDown, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'DroppedDown', TCustomComboBox_Write_DroppedDown, 0, [varEmpty]);
    AddGet(TCustomComboBox, 'Items', TCustomComboBox_Read_Items, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'Items', TCustomComboBox_Write_Items, 0, [varEmpty]);
    AddGet(TCustomComboBox, 'ItemIndex', TCustomComboBox_Read_ItemIndex, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'ItemIndex', TCustomComboBox_Write_ItemIndex, 0, [varEmpty]);
    AddGet(TCustomComboBox, 'SelLength', TCustomComboBox_Read_SelLength, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'SelLength', TCustomComboBox_Write_SelLength, 0, [varEmpty]);
    AddGet(TCustomComboBox, 'SelStart', TCustomComboBox_Read_SelStart, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'SelStart', TCustomComboBox_Write_SelStart, 0, [varEmpty]);
    AddGet(TCustomComboBox, 'SelText', TCustomComboBox_Read_SelText, 0, [varEmpty], varEmpty);
    AddSet(TCustomComboBox, 'SelText', TCustomComboBox_Write_SelText, 0, [varEmpty]);
    { TComboBox }
    AddClass(cStdCtrls, TComboBox, 'TComboBox');
    AddGet(TComboBox, 'Create', TComboBox_Create, 1, [varEmpty], varEmpty);
    { TButton }
    AddClass(cStdCtrls, TButton, 'TButton');
    AddGet(TButton, 'Create', TButton_Create, 1, [varEmpty], varEmpty);
    AddGet(TButton, 'Click', TButton_Click, 0, [varEmpty], varEmpty);
    { TCheckBoxState }
    AddConst(cStdCtrls, 'cbUnchecked', Ord(cbUnchecked));
    AddConst(cStdCtrls, 'cbChecked', Ord(cbChecked));
    AddConst(cStdCtrls, 'cbGrayed', Ord(cbGrayed));
    { TCustomCheckBox }
    AddClass(cStdCtrls, TCustomCheckBox, 'TCustomCheckBox');
    AddGet(TCustomCheckBox, 'Create', TCustomCheckBox_Create, 1, [varEmpty], varEmpty);
    { TCheckBox }
    AddClass(cStdCtrls, TCheckBox, 'TCheckBox');
    AddGet(TCheckBox, 'Create', TCheckBox_Create, 1, [varEmpty], varEmpty);
    { TRadioButton }
    AddClass(cStdCtrls, TRadioButton, 'TRadioButton');
    AddGet(TRadioButton, 'Create', TRadioButton_Create, 1, [varEmpty], varEmpty);
    { TListBoxStyle }
    AddConst(cStdCtrls, 'lbStandard', Ord(lbStandard));
    AddConst(cStdCtrls, 'lbOwnerDrawFixed', Ord(lbOwnerDrawFixed));
    AddConst(cStdCtrls, 'lbOwnerDrawVariable', Ord(lbOwnerDrawVariable));
    { TCustomListBox }
    AddClass(cStdCtrls, TCustomListBox, 'TCustomListBox');
    AddGet(TCustomListBox, 'Create', TCustomListBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Clear', TCustomListBox_Clear, 0, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'ItemAtPos', TCustomListBox_ItemAtPos, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListBox, 'ItemRect', TCustomListBox_ItemRect, 1, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Canvas', TCustomListBox_Read_Canvas, 0, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Items', TCustomListBox_Read_Items, 0, [varEmpty], varEmpty);
    AddSet(TCustomListBox, 'Items', TCustomListBox_Write_Items, 0, [varEmpty]);
    AddGet(TCustomListBox, 'ItemIndex', TCustomListBox_Read_ItemIndex, 0, [varEmpty], varEmpty);
    AddSet(TCustomListBox, 'ItemIndex', TCustomListBox_Write_ItemIndex, 0, [varEmpty]);
    AddGet(TCustomListBox, 'SelCount', TCustomListBox_Read_SelCount, 0, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Selected', TCustomListBox_Read_Selected, 1, [varEmpty], varEmpty);
    AddSet(TCustomListBox, 'Selected', TCustomListBox_Write_Selected, 1, [varNull]);
    AddGet(TCustomListBox, 'TopIndex', TCustomListBox_Read_TopIndex, 0, [varEmpty], varEmpty);
    AddSet(TCustomListBox, 'TopIndex', TCustomListBox_Write_TopIndex, 0, [varEmpty]);
    { TListBox }
    AddClass(cStdCtrls, TListBox, 'TListBox');
    AddGet(TListBox, 'Create', TListBox_Create, 1, [varEmpty], varEmpty);
    { TScrollCode }
    AddConst(cStdCtrls, 'scLineUp', Ord(scLineUp));
    AddConst(cStdCtrls, 'scLineDown', Ord(scLineDown));
    AddConst(cStdCtrls, 'scPageUp', Ord(scPageUp));
    AddConst(cStdCtrls, 'scPageDown', Ord(scPageDown));
    AddConst(cStdCtrls, 'scPosition', Ord(scPosition));
    AddConst(cStdCtrls, 'scTrack', Ord(scTrack));
    AddConst(cStdCtrls, 'scTop', Ord(scTop));
    AddConst(cStdCtrls, 'scBottom', Ord(scBottom));
    AddConst(cStdCtrls, 'scEndScroll', Ord(scEndScroll));
    { TScrollBar }
    AddClass(cStdCtrls, TScrollBar, 'TScrollBar');
    AddGet(TScrollBar, 'Create', TScrollBar_Create, 1, [varEmpty], varEmpty);
    AddGet(TScrollBar, 'SetParams', TScrollBar_SetParams, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    
    { TStaticBorderStyle }
    AddConst(cStdCtrls, 'sbsNone', Ord(sbsNone));
    AddConst(cStdCtrls, 'sbsSingle', Ord(sbsSingle));
    AddConst(cStdCtrls, 'sbsSunken', Ord(sbsSunken));
    { TCustomStaticText }
    AddClass(cStdCtrls, TCustomStaticText, 'TCustomStaticText');
    AddGet(TCustomStaticText, 'Create', TCustomStaticText_Create, 1, [varEmpty], varEmpty);
    { TStaticText }
    AddClass(cStdCtrls, TStaticText, 'TStaticText');
    AddGet(TStaticText, 'Create', TStaticText_Create, 1, [varEmpty], varEmpty);

    AddHandler(cStdCtrls, 'TDrawItemEvent', TJvInterpreterStdCtrlsEvent, @TJvInterpreterStdCtrlsEvent.DrawItemEvent);
    AddHandler(cStdCtrls, 'TMeasureItemEvent', TJvInterpreterStdCtrlsEvent,
      @TJvInterpreterStdCtrlsEvent.MeasureItemEvent);
    AddHandler(cStdCtrls, 'TScrollEvent', TJvInterpreterStdCtrlsEvent, @TJvInterpreterStdCtrlsEvent.ScrollEvent);
  end;
  RegisterClasses([TGroupBox, TCustomLabel, TLabel, TCustomEdit, TEdit,
    TCustomMemo, TMemo, TCustomComboBox, TComboBox, TButton, TCustomCheckBox,
      TCheckBox, TRadioButton, TCustomListBox, TListBox, TScrollBar
      , TCustomStaticText, TStaticText]);
end;

end.

