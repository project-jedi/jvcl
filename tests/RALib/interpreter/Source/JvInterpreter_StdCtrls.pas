{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : adapter unit - converts JvInterpreter calls to delphi calls

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvInterpreter_StdCtrls;

interface

uses JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses Windows, Classes, Controls, StdCtrls, JvInterpreter_Windows;


  { TGroupBox }

{ constructor Create(AOwner: TComponent) }
procedure TGroupBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TGroupBox.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCustomLabel }

{ constructor Create(AOwner: TComponent) }
procedure TCustomLabel_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomLabel.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Canvas: TCanvas }
procedure TCustomLabel_Read_Canvas(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomLabel(Args.Obj).Canvas);
end;

  { TLabel }

{ constructor Create(AOwner: TComponent) }
procedure TLabel_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TLabel.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCustomEdit }

{ constructor Create(AOwner: TComponent) }
procedure TCustomEdit_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Clear; }
procedure TCustomEdit_Clear(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).Clear;
end;

{  procedure ClearSelection; }
procedure TCustomEdit_ClearSelection(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).ClearSelection;
end;

{  procedure CopyToClipboard; }
procedure TCustomEdit_CopyToClipboard(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).CopyToClipboard;
end;

{  procedure CutToClipboard; }
procedure TCustomEdit_CutToClipboard(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).CutToClipboard;
end;

{  procedure PasteFromClipboard; }
procedure TCustomEdit_PasteFromClipboard(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).PasteFromClipboard;
end;

{  function GetSelTextBuf(Buffer: PChar; BufSize: Integer): Integer; }
procedure TCustomEdit_GetSelTextBuf(var Value: Variant; Args: TArgs);
begin
  Value := TCustomEdit(Args.Obj).GetSelTextBuf(PChar(string(Args.Values[0])), Args.Values[1]);
end;

{  procedure SelectAll; }
procedure TCustomEdit_SelectAll(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).SelectAll;
end;

{  procedure SetSelTextBuf(Buffer: PChar); }
procedure TCustomEdit_SetSelTextBuf(var Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).SetSelTextBuf(PChar(string(Args.Values[0])));
end;

{ property Read Modified: Boolean }
procedure TCustomEdit_Read_Modified(var Value: Variant; Args: TArgs);
begin
  Value := TCustomEdit(Args.Obj).Modified;
end;

{ property Write Modified(Value: Boolean) }
procedure TCustomEdit_Write_Modified(const Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).Modified := Value;
end;

{ property Read SelLength: Integer }
procedure TCustomEdit_Read_SelLength(var Value: Variant; Args: TArgs);
begin
  Value := TCustomEdit(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: Integer) }
procedure TCustomEdit_Write_SelLength(const Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).SelLength := Value;
end;

{ property Read SelStart: Integer }
procedure TCustomEdit_Read_SelStart(var Value: Variant; Args: TArgs);
begin
  Value := TCustomEdit(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: Integer) }
procedure TCustomEdit_Write_SelStart(const Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).SelStart := Value;
end;

{ property Read SelText: string }
procedure TCustomEdit_Read_SelText(var Value: Variant; Args: TArgs);
begin
  Value := TCustomEdit(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }
procedure TCustomEdit_Write_SelText(const Value: Variant; Args: TArgs);
begin
  TCustomEdit(Args.Obj).SelText := Value;
end;

  { TEdit }

{ constructor Create(AOwner: TComponent) }
procedure TEdit_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TEdit.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCustomMemo }

{ constructor Create(AOwner: TComponent) }
procedure TCustomMemo_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomMemo.Create(V2O(Args.Values[0]) as TComponent));
end;

{ property Read Lines: TStrings }
procedure TCustomMemo_Read_Lines(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomMemo(Args.Obj).Lines);
end;

{ property Write Lines(Value: TStrings) }
procedure TCustomMemo_Write_Lines(const Value: Variant; Args: TArgs);
begin
  TCustomMemo(Args.Obj).Lines := V2O(Value) as TStrings;
end;

  { TMemo }

{ constructor Create(AOwner: TComponent) }
procedure TMemo_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TMemo.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCustomComboBox }

{ constructor Create(AOwner: TComponent) }
procedure TCustomComboBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Clear; }
procedure TCustomComboBox_Clear(var Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).Clear;
end;

{  procedure SelectAll; }
procedure TCustomComboBox_SelectAll(var Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).SelectAll;
end;

{ property Read Canvas: TCanvas }
procedure TCustomComboBox_Read_Canvas(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomComboBox(Args.Obj).Canvas);
end;

{ property Read DroppedDown: Boolean }
procedure TCustomComboBox_Read_DroppedDown(var Value: Variant; Args: TArgs);
begin
  Value := TCustomComboBox(Args.Obj).DroppedDown;
end;

{ property Write DroppedDown(Value: Boolean) }
procedure TCustomComboBox_Write_DroppedDown(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).DroppedDown := Value;
end;

{ property Read Items: TStrings }
procedure TCustomComboBox_Read_Items(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomComboBox(Args.Obj).Items);
end;

{ property Write Items(Value: TStrings) }
procedure TCustomComboBox_Write_Items(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).Items := V2O(Value) as TStrings;
end;

{ property Read ItemIndex: Integer }
procedure TCustomComboBox_Read_ItemIndex(var Value: Variant; Args: TArgs);
begin
  Value := TCustomComboBox(Args.Obj).ItemIndex;
end;

{ property Write ItemIndex(Value: Integer) }
procedure TCustomComboBox_Write_ItemIndex(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).ItemIndex := Value;
end;

{ property Read SelLength: Integer }
procedure TCustomComboBox_Read_SelLength(var Value: Variant; Args: TArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: Integer) }
procedure TCustomComboBox_Write_SelLength(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).SelLength := Value;
end;

{ property Read SelStart: Integer }
procedure TCustomComboBox_Read_SelStart(var Value: Variant; Args: TArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: Integer) }
procedure TCustomComboBox_Write_SelStart(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).SelStart := Value;
end;

{ property Read SelText: string }
procedure TCustomComboBox_Read_SelText(var Value: Variant; Args: TArgs);
begin
  Value := TCustomComboBox(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }
procedure TCustomComboBox_Write_SelText(const Value: Variant; Args: TArgs);
begin
  TCustomComboBox(Args.Obj).SelText := Value;
end;

  { TComboBox }

{ constructor Create(AOwner: TComponent) }
procedure TComboBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TComboBox.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TButton }

{ constructor Create(AOwner: TComponent) }
procedure TButton_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TButton.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Click; }
procedure TButton_Click(var Value: Variant; Args: TArgs);
begin
  TButton(Args.Obj).Click;
end;

  { TCustomCheckBox }

{ constructor Create(AOwner: TComponent) }
procedure TCustomCheckBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomCheckBox.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCheckBox }

{ constructor Create(AOwner: TComponent) }
procedure TCheckBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCheckBox.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TRadioButton }

{ constructor Create(AOwner: TComponent) }
procedure TRadioButton_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TRadioButton.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TCustomListBox }

{ constructor Create(AOwner: TComponent) }
procedure TCustomListBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure Clear; }
procedure TCustomListBox_Clear(var Value: Variant; Args: TArgs);
begin
  TCustomListBox(Args.Obj).Clear;
end;

{  function ItemAtPos(Pos: TPoint; Existing: Boolean): Integer; }
procedure TCustomListBox_ItemAtPos(var Value: Variant; Args: TArgs);
begin
  Value := TCustomListBox(Args.Obj).ItemAtPos(Var2Point(Args.Values[0]), Args.Values[1]);
end;

{  function ItemRect(Index: Integer): TRect; }
procedure TCustomListBox_ItemRect(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(TCustomListBox(Args.Obj).ItemRect(Args.Values[0]));
end;

{ property Read Canvas: TCanvas }
procedure TCustomListBox_Read_Canvas(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomListBox(Args.Obj).Canvas);
end;

{ property Read Items: TStrings }
procedure TCustomListBox_Read_Items(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomListBox(Args.Obj).Items);
end;

{ property Write Items(Value: TStrings) }
procedure TCustomListBox_Write_Items(const Value: Variant; Args: TArgs);
begin
  TCustomListBox(Args.Obj).Items := V2O(Value) as TStrings;
end;

{ property Read ItemIndex: Integer }
procedure TCustomListBox_Read_ItemIndex(var Value: Variant; Args: TArgs);
begin
  Value := TCustomListBox(Args.Obj).ItemIndex;
end;

{ property Write ItemIndex(Value: Integer) }
procedure TCustomListBox_Write_ItemIndex(const Value: Variant; Args: TArgs);
begin
  TCustomListBox(Args.Obj).ItemIndex := Value;
end;

{ property Read SelCount: Integer }
procedure TCustomListBox_Read_SelCount(var Value: Variant; Args: TArgs);
begin
  Value := TCustomListBox(Args.Obj).SelCount;
end;

{ property Read Selected[Integer]: Boolean }
procedure TCustomListBox_Read_Selected(var Value: Variant; Args: TArgs);
begin
  Value := TCustomListBox(Args.Obj).Selected[Args.Values[0]];
end;

{ property Write Selected[Integer]: Boolean }
procedure TCustomListBox_Write_Selected(const Value: Variant; Args: TArgs);
begin
  TCustomListBox(Args.Obj).Selected[Args.Values[0]] := Value;
end;

{ property Read TopIndex: Integer }
procedure TCustomListBox_Read_TopIndex(var Value: Variant; Args: TArgs);
begin
  Value := TCustomListBox(Args.Obj).TopIndex;
end;

{ property Write TopIndex(Value: Integer) }
procedure TCustomListBox_Write_TopIndex(const Value: Variant; Args: TArgs);
begin
  TCustomListBox(Args.Obj).TopIndex := Value;
end;

  { TListBox }

{ constructor Create(AOwner: TComponent) }
procedure TListBox_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TListBox.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TScrollBar }

{ constructor Create(AOwner: TComponent) }
procedure TScrollBar_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TScrollBar.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure SetParams(APosition, AMin, AMax: Integer); }
procedure TScrollBar_SetParams(var Value: Variant; Args: TArgs);
begin
  TScrollBar(Args.Obj).SetParams(Args.Values[0], Args.Values[1], Args.Values[2]);
end;

{$IFDEF COMPILER3_UP}
  { TCustomStaticText }

{ constructor Create(AOwner: TComponent) }
procedure TCustomStaticText_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TCustomStaticText.Create(V2O(Args.Values[0]) as TComponent));
end;

  { TStaticText }

{ constructor Create(AOwner: TComponent) }
procedure TStaticText_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TStaticText.Create(V2O(Args.Values[0]) as TComponent));
end;
{$ENDIF COMPILER3_UP}


type
  
  TJvInterpreterStdCtrlsEvent = class(TJvInterpreterEvent)
  private
    procedure DrawItemEvent(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure MeasureItemEvent(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure ScrollEvent(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
  end;

procedure TJvInterpreterStdCtrlsEvent.DrawItemEvent(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
 {$IFDEF COMPILER5_UP}
  CallFunction(nil, [O2V(Control), Index, Rect2Var(Rect), S2V(Word(State))]);
 {$ELSE}
  CallFunction(nil, [O2V(Control), Index, Rect2Var(Rect), S2V(Byte(State))]);
 {$ENDIF}
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
begin
  with JvInterpreterAdapter do
  begin
   { TGroupBox }
    AddClass('StdCtrls', TGroupBox, 'TGroupBox');
    AddGet(TGroupBox, 'Create', TGroupBox_Create, 1, [varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
   { TTextLayout }
    AddConst('StdCtrls', 'tlTop', tlTop);
    AddConst('StdCtrls', 'tlCenter', tlCenter);
    AddConst('StdCtrls', 'tlBottom', tlBottom);
   {$ENDIF COMPILER3_UP}
   { TCustomLabel }
    AddClass('StdCtrls', TCustomLabel, 'TCustomLabel');
    AddGet(TCustomLabel, 'Create', TCustomLabel_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomLabel, 'Canvas', TCustomLabel_Read_Canvas, 0, [0], varEmpty);
   { TLabel }
    AddClass('StdCtrls', TLabel, 'TLabel');
    AddGet(TLabel, 'Create', TLabel_Create, 1, [varEmpty], varEmpty);
   { TEditCharCase }
    AddConst('StdCtrls', 'ecNormal', ecNormal);
    AddConst('StdCtrls', 'ecUpperCase', ecUpperCase);
    AddConst('StdCtrls', 'ecLowerCase', ecLowerCase);
   { TCustomEdit }
    AddClass('StdCtrls', TCustomEdit, 'TCustomEdit');
    AddGet(TCustomEdit, 'Create', TCustomEdit_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'Clear', TCustomEdit_Clear, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'ClearSelection', TCustomEdit_ClearSelection, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'CopyToClipboard', TCustomEdit_CopyToClipboard, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'CutToClipboard', TCustomEdit_CutToClipboard, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'PasteFromClipboard', TCustomEdit_PasteFromClipboard, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'GetSelTextBuf', TCustomEdit_GetSelTextBuf, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomEdit, 'SelectAll', TCustomEdit_SelectAll, 0, [0], varEmpty);
    AddGet(TCustomEdit, 'SetSelTextBuf', TCustomEdit_SetSelTextBuf, 1, [varEmpty], varEmpty);
    AddGet(TCustomEdit, 'Modified', TCustomEdit_Read_Modified, 0, [0], varEmpty);
    AddSet(TCustomEdit, 'Modified', TCustomEdit_Write_Modified, 0, [0]);
    AddGet(TCustomEdit, 'SelLength', TCustomEdit_Read_SelLength, 0, [0], varEmpty);
    AddSet(TCustomEdit, 'SelLength', TCustomEdit_Write_SelLength, 0, [0]);
    AddGet(TCustomEdit, 'SelStart', TCustomEdit_Read_SelStart, 0, [0], varEmpty);
    AddSet(TCustomEdit, 'SelStart', TCustomEdit_Write_SelStart, 0, [0]);
    AddGet(TCustomEdit, 'SelText', TCustomEdit_Read_SelText, 0, [0], varEmpty);
    AddSet(TCustomEdit, 'SelText', TCustomEdit_Write_SelText, 0, [0]);
   { TEdit }
    AddClass('StdCtrls', TEdit, 'TEdit');
    AddGet(TEdit, 'Create', TEdit_Create, 1, [varEmpty], varEmpty);
   { TScrollStyle }
    AddConst('StdCtrls', 'ssNone', ssNone);
    AddConst('StdCtrls', 'ssHorizontal', ssHorizontal);
    AddConst('StdCtrls', 'ssVertical', ssVertical);
    AddConst('StdCtrls', 'ssBoth', ssBoth);
   { TCustomMemo }
    AddClass('StdCtrls', TCustomMemo, 'TCustomMemo');
    AddGet(TCustomMemo, 'Create', TCustomMemo_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomMemo, 'Lines', TCustomMemo_Read_Lines, 0, [0], varEmpty);
    AddSet(TCustomMemo, 'Lines', TCustomMemo_Write_Lines, 0, [0]);
   { TMemo }
    AddClass('StdCtrls', TMemo, 'TMemo');
    AddGet(TMemo, 'Create', TMemo_Create, 1, [varEmpty], varEmpty);
   { TComboBoxStyle }
    AddConst('StdCtrls', 'csDropDown', csDropDown);
    AddConst('StdCtrls', 'csSimple', csSimple);
    AddConst('StdCtrls', 'csDropDownList', csDropDownList);
    AddConst('StdCtrls', 'csOwnerDrawFixed', csOwnerDrawFixed);
    AddConst('StdCtrls', 'csOwnerDrawVariable', csOwnerDrawVariable);
   { TOwnerDrawState }
    AddConst('StdCtrls', 'odSelected', odSelected);
    AddConst('StdCtrls', 'odGrayed', odGrayed);
    AddConst('StdCtrls', 'odDisabled', odDisabled);
    AddConst('StdCtrls', 'odChecked', odChecked);
    AddConst('StdCtrls', 'odFocused', odFocused);
   { TCustomComboBox }
    AddClass('StdCtrls', TCustomComboBox, 'TCustomComboBox');
    AddGet(TCustomComboBox, 'Create', TCustomComboBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomComboBox, 'Clear', TCustomComboBox_Clear, 0, [0], varEmpty);
    AddGet(TCustomComboBox, 'SelectAll', TCustomComboBox_SelectAll, 0, [0], varEmpty);
    AddGet(TCustomComboBox, 'Canvas', TCustomComboBox_Read_Canvas, 0, [0], varEmpty);
    AddGet(TCustomComboBox, 'DroppedDown', TCustomComboBox_Read_DroppedDown, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'DroppedDown', TCustomComboBox_Write_DroppedDown, 0, [0]);
    AddGet(TCustomComboBox, 'Items', TCustomComboBox_Read_Items, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'Items', TCustomComboBox_Write_Items, 0, [0]);
    AddGet(TCustomComboBox, 'ItemIndex', TCustomComboBox_Read_ItemIndex, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'ItemIndex', TCustomComboBox_Write_ItemIndex, 0, [0]);
    AddGet(TCustomComboBox, 'SelLength', TCustomComboBox_Read_SelLength, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'SelLength', TCustomComboBox_Write_SelLength, 0, [0]);
    AddGet(TCustomComboBox, 'SelStart', TCustomComboBox_Read_SelStart, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'SelStart', TCustomComboBox_Write_SelStart, 0, [0]);
    AddGet(TCustomComboBox, 'SelText', TCustomComboBox_Read_SelText, 0, [0], varEmpty);
    AddSet(TCustomComboBox, 'SelText', TCustomComboBox_Write_SelText, 0, [0]);
   { TComboBox }
    AddClass('StdCtrls', TComboBox, 'TComboBox');
    AddGet(TComboBox, 'Create', TComboBox_Create, 1, [varEmpty], varEmpty);
   { TButton }
    AddClass('StdCtrls', TButton, 'TButton');
    AddGet(TButton, 'Create', TButton_Create, 1, [varEmpty], varEmpty);
    AddGet(TButton, 'Click', TButton_Click, 0, [0], varEmpty);
   { TCheckBoxState }
    AddConst('StdCtrls', 'cbUnchecked', cbUnchecked);
    AddConst('StdCtrls', 'cbChecked', cbChecked);
    AddConst('StdCtrls', 'cbGrayed', cbGrayed);
   { TCustomCheckBox }
    AddClass('StdCtrls', TCustomCheckBox, 'TCustomCheckBox');
    AddGet(TCustomCheckBox, 'Create', TCustomCheckBox_Create, 1, [varEmpty], varEmpty);
   { TCheckBox }
    AddClass('StdCtrls', TCheckBox, 'TCheckBox');
    AddGet(TCheckBox, 'Create', TCheckBox_Create, 1, [varEmpty], varEmpty);
   { TRadioButton }
    AddClass('StdCtrls', TRadioButton, 'TRadioButton');
    AddGet(TRadioButton, 'Create', TRadioButton_Create, 1, [varEmpty], varEmpty);
   { TListBoxStyle }
    AddConst('StdCtrls', 'lbStandard', lbStandard);
    AddConst('StdCtrls', 'lbOwnerDrawFixed', lbOwnerDrawFixed);
    AddConst('StdCtrls', 'lbOwnerDrawVariable', lbOwnerDrawVariable);
   { TCustomListBox }
    AddClass('StdCtrls', TCustomListBox, 'TCustomListBox');
    AddGet(TCustomListBox, 'Create', TCustomListBox_Create, 1, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Clear', TCustomListBox_Clear, 0, [0], varEmpty);
    AddGet(TCustomListBox, 'ItemAtPos', TCustomListBox_ItemAtPos, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TCustomListBox, 'ItemRect', TCustomListBox_ItemRect, 1, [varEmpty], varEmpty);
    AddGet(TCustomListBox, 'Canvas', TCustomListBox_Read_Canvas, 0, [0], varEmpty);
    AddGet(TCustomListBox, 'Items', TCustomListBox_Read_Items, 0, [0], varEmpty);
    AddSet(TCustomListBox, 'Items', TCustomListBox_Write_Items, 0, [0]);
    AddGet(TCustomListBox, 'ItemIndex', TCustomListBox_Read_ItemIndex, 0, [0], varEmpty);
    AddSet(TCustomListBox, 'ItemIndex', TCustomListBox_Write_ItemIndex, 0, [0]);
    AddGet(TCustomListBox, 'SelCount', TCustomListBox_Read_SelCount, 0, [0], varEmpty);
    AddGet(TCustomListBox, 'Selected', TCustomListBox_Read_Selected, 1, [0], varEmpty);
    AddSet(TCustomListBox, 'Selected', TCustomListBox_Write_Selected, 1, [1]);
    AddGet(TCustomListBox, 'TopIndex', TCustomListBox_Read_TopIndex, 0, [0], varEmpty);
    AddSet(TCustomListBox, 'TopIndex', TCustomListBox_Write_TopIndex, 0, [0]);
   { TListBox }
    AddClass('StdCtrls', TListBox, 'TListBox');
    AddGet(TListBox, 'Create', TListBox_Create, 1, [varEmpty], varEmpty);
   { TScrollCode }
    AddConst('StdCtrls', 'scLineUp', scLineUp);
    AddConst('StdCtrls', 'scLineDown', scLineDown);
    AddConst('StdCtrls', 'scPageUp', scPageUp);
    AddConst('StdCtrls', 'scPageDown', scPageDown);
    AddConst('StdCtrls', 'scPosition', scPosition);
    AddConst('StdCtrls', 'scTrack', scTrack);
    AddConst('StdCtrls', 'scTop', scTop);
    AddConst('StdCtrls', 'scBottom', scBottom);
    AddConst('StdCtrls', 'scEndScroll', scEndScroll);
   { TScrollBar }
    AddClass('StdCtrls', TScrollBar, 'TScrollBar');
    AddGet(TScrollBar, 'Create', TScrollBar_Create, 1, [varEmpty], varEmpty);
    AddGet(TScrollBar, 'SetParams', TScrollBar_SetParams, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
   {$IFDEF COMPILER3_UP}
   { TStaticBorderStyle }
    AddConst('StdCtrls', 'sbsNone', sbsNone);
    AddConst('StdCtrls', 'sbsSingle', sbsSingle);
    AddConst('StdCtrls', 'sbsSunken', sbsSunken);
   { TCustomStaticText }
    AddClass('StdCtrls', TCustomStaticText, 'TCustomStaticText');
    AddGet(TCustomStaticText, 'Create', TCustomStaticText_Create, 1, [varEmpty], varEmpty);
   { TStaticText }
    AddClass('StdCtrls', TStaticText, 'TStaticText');
    AddGet(TStaticText, 'Create', TStaticText_Create, 1, [varEmpty], varEmpty);
   {$ENDIF COMPILER3_UP}

    AddHandler('StdCtrls', 'TDrawItemEvent', TJvInterpreterStdCtrlsEvent, @TJvInterpreterStdCtrlsEvent.DrawItemEvent);
    AddHandler('StdCtrls', 'TMeasureItemEvent', TJvInterpreterStdCtrlsEvent, @TJvInterpreterStdCtrlsEvent.MeasureItemEvent);
    AddHandler('StdCtrls', 'TScrollEvent', TJvInterpreterStdCtrlsEvent, @TJvInterpreterStdCtrlsEvent.ScrollEvent);
  end;    { with }
  RegisterClasses([TGroupBox, TCustomLabel, TLabel, TCustomEdit, TEdit,
    TCustomMemo, TMemo, TCustomComboBox, TComboBox, TButton, TCustomCheckBox,
    TCheckBox, TRadioButton, TCustomListBox, TListBox, TScrollBar
   {$IFDEF COMPILER3_UP}, TCustomStaticText, TStaticText {$ENDIF COMPILER3_UP} ]);
end;    { RegisterJvInterpreterAdapter }

end.
