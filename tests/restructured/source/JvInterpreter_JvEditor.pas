{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvInterpreter_JvEditor.PAS, released on 2002-07-04.

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

unit JvInterpreter_JvEditor;

interface

uses JvInterpreter;

  procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses Classes, JvEditor, JvHLEditor, JvInterpreter_Windows;


  { TJvKeyboard  }

{ constructor Create }
procedure TKeyboard_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TJvKeyboard .Create);
end;

{  procedure Add(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState); }
procedure TKeyboard_Add(var Value: Variant; Args: TArgs);
begin
  TJvKeyboard (Args.Obj).Add(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))));
end;

{  procedure Add2(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState); }
procedure TKeyboard_Add2(var Value: Variant; Args: TArgs);
begin
  TJvKeyboard (Args.Obj).Add2(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))), Args.Values[3], TShiftState(Byte(V2S(Args.Values[4]))));
end;

{  procedure Clear; }
procedure TKeyboard_Clear(var Value: Variant; Args: TArgs);
begin
  TJvKeyboard (Args.Obj).Clear;
end;

{  procedure SetDefLayot; }
procedure TKeyboard_SetDefLayot(var Value: Variant; Args: TArgs);
begin
  TJvKeyboard (Args.Obj).SetDefLayot;
end;

  { EJvEditorError  }

  { TJvCustomEditor }

{ constructor Create(AOwner: TComponent) }
procedure TRACustomEditor_Create(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TJvCustomEditor.Create(V2O(Args.Values[0]) as TComponent));
end;

{  procedure SetLeftTop(ALeftCol, ATopRow: integer); }
procedure TRACustomEditor_SetLeftTop(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SetLeftTop(Args.Values[0], Args.Values[1]);
end;

{  procedure ClipBoardCopy; }
procedure TRACustomEditor_ClipBoardCopy(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardCopy;
end;

{  procedure ClipBoardPaste; }
procedure TRACustomEditor_ClipBoardPaste(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardPaste;
end;

{  procedure ClipBoardCut; }
procedure TRACustomEditor_ClipBoardCut(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardCut;
end;

{  procedure DeleteSelected; }
procedure TRACustomEditor_DeleteSelected(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).DeleteSelected;
end;

{  function CalcCellRect(const X, Y: integer): TRect; }
procedure TRACustomEditor_CalcCellRect(var Value: Variant; Args: TArgs);
begin
  Value := Rect2Var(TJvCustomEditor(Args.Obj).CalcCellRect(Args.Values[0], Args.Values[1]));
end;

{  procedure SetCaret(X, Y: integer); }
procedure TRACustomEditor_SetCaret(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SetCaret(Args.Values[0], Args.Values[1]);
end;

{  procedure CaretFromPos(const Pos: integer; var X, Y: integer); }
procedure TRACustomEditor_CaretFromPos(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).CaretFromPos(Args.Values[0], TVarData(Args.Values[1]).vInteger, TVarData(Args.Values[2]).vInteger);
end;

{  function PosFromCaret(const X, Y: integer): integer; }
procedure TRACustomEditor_PosFromCaret(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).PosFromCaret(Args.Values[0], Args.Values[1]);
end;

{  procedure PaintCaret(const bShow: boolean); }
procedure TRACustomEditor_PaintCaret(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).PaintCaret(Args.Values[0]);
end;

{  function GetTextLen: Integer; }
procedure TRACustomEditor_GetTextLen(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetTextLen;
end;

{  function GetSelText: string; }
procedure TRACustomEditor_GetSelText(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetSelText;
end;

{  procedure SetSelText(const AValue: string); }
procedure TRACustomEditor_SetSelText(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SetSelText(Args.Values[0]);
end;

{  function GetWordOnCaret: string; }
procedure TRACustomEditor_GetWordOnCaret(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetWordOnCaret;
end;

{  procedure BeginUpdate; }
procedure TRACustomEditor_BeginUpdate(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).BeginUpdate;
end;

{  procedure EndUpdate; }
procedure TRACustomEditor_EndUpdate(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).EndUpdate;
end;

{  procedure MakeRowVisible(ARow: Integer); }
procedure TRACustomEditor_MakeRowVisible(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).MakeRowVisible(Args.Values[0]);
end;

{  procedure Command(ACommand: TEditCommand); }
procedure TRACustomEditor_Command(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).Command(Args.Values[0]);
end;

{  procedure PostCommand(ACommand: TEditCommand); }
procedure TRACustomEditor_PostCommand(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).PostCommand(Args.Values[0]);
end;

{  procedure InsertText(const Text: string); }
procedure TRACustomEditor_InsertText(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).InsertText(Args.Values[0]);
end;

{  procedure ReplaceWord(const NewString: string); }
procedure TRACustomEditor_ReplaceWord(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).ReplaceWord(Args.Values[0]);
end;

{  procedure ReplaceWord2(const NewString: string); }
procedure TRACustomEditor_ReplaceWord2(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).ReplaceWord2(Args.Values[0]);
end;

{  procedure BeginCompound; }
procedure TRACustomEditor_BeginCompound(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).BeginCompound;
end;

{  procedure EndCompound; }
procedure TRACustomEditor_EndCompound(var Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).EndCompound;
end;

{  function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint; }
procedure TRACustomEditor_GetText(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetText(Args.Values[0], PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ property Read LeftCol: integer }
procedure TRACustomEditor_Read_LeftCol(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LeftCol;
end;

{ property Read TopRow: integer }
procedure TRACustomEditor_Read_TopRow(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).TopRow;
end;

{ property Read VisibleColCount: integer }
procedure TRACustomEditor_Read_VisibleColCount(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).VisibleColCount;
end;

{ property Read VisibleRowCount: integer }
procedure TRACustomEditor_Read_VisibleRowCount(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).VisibleRowCount;
end;

{ property Read LastVisibleCol: integer }
procedure TRACustomEditor_Read_LastVisibleCol(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LastVisibleCol;
end;

{ property Read LastVisibleRow: integer }
procedure TRACustomEditor_Read_LastVisibleRow(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LastVisibleRow;
end;

{ property Read Cols: integer }
procedure TRACustomEditor_Read_Cols(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Cols;
end;

{ property Write Cols(Value: integer) }
procedure TRACustomEditor_Write_Cols(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).Cols := Value;
end;

{ property Read Rows: integer }
procedure TRACustomEditor_Read_Rows(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Rows;
end;

{ property Write Rows(Value: integer) }
procedure TRACustomEditor_Write_Rows(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).Rows := Value;
end;

{ property Read CaretX: integer }
procedure TRACustomEditor_Read_CaretX(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CaretX;
end;

{ property Write CaretX(Value: integer) }
procedure TRACustomEditor_Write_CaretX(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).CaretX := Value;
end;

{ property Read CaretY: integer }
procedure TRACustomEditor_Read_CaretY(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CaretY;
end;

{ property Write CaretY(Value: integer) }
procedure TRACustomEditor_Write_CaretY(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).CaretY := Value;
end;

{ property Read Modified: boolean }
procedure TRACustomEditor_Read_Modified(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Modified;
end;

{ property Write Modified(Value: boolean) }
procedure TRACustomEditor_Write_Modified(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).Modified := Value;
end;

{ property Read SelStart: integer }
procedure TRACustomEditor_Read_SelStart(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: integer) }
procedure TRACustomEditor_Write_SelStart(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SelStart := Value;
end;

{ property Read SelLength: integer }
procedure TRACustomEditor_Read_SelLength(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: integer) }
procedure TRACustomEditor_Write_SelLength(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SelLength := Value;
end;

{ property Read SelText: string }
procedure TRACustomEditor_Read_SelText(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }
procedure TRACustomEditor_Write_SelText(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).SelText := Value;
end;

(*
{ property Read BookMarks: TBookMarks }
procedure TRACustomEditor_Read_BookMarks(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks;
end;
*)

{ property Read Keyboard: TJvKeyboard  }
procedure TRACustomEditor_Read_Keyboard(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TJvCustomEditor(Args.Obj).Keyboard);
end;

(*
{ property Read CellRect: TCellRect }
procedure TRACustomEditor_Read_CellRect(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CellRect;
end;
*)

{ property Read UndoBuffer: TUndoBuffer }
procedure TRACustomEditor_Read_UndoBuffer(var Value: Variant; Args: TArgs);
begin
  Value := O2V(TJvCustomEditor(Args.Obj).UndoBuffer);
end;

{ property Read Recording: boolean }
procedure TRACustomEditor_Read_Recording(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Recording;
end;

procedure TRACustomEditor_Read_BookMarkX(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X;
end;

procedure TRACustomEditor_Read_BookMarkY(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y;
end;

procedure TRACustomEditor_Read_BookMarkValid(var Value: Variant; Args: TArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid;
end;

procedure TRACustomEditor_Write_BookMarkX(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X := Value;
end;

procedure TRACustomEditor_Write_BookMarkY(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y := Value;
end;

procedure TRACustomEditor_Write_BookMarkValid(const Value: Variant; Args: TArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid := Value;
end;


procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
begin
  with JvInterpreterAdapter do
  begin
   { TModifiedAction }
    AddConst('JvEditor', 'maInsert', maInsert);
    AddConst('JvEditor', 'maDelete', maDelete);
   { TJvKeyboard  }
    AddClass('JvEditor', TJvKeyboard , 'TJvKeyboard ');
    AddGet(TJvKeyboard , 'Create', TKeyboard_Create, 0, [0], varEmpty);
    AddGet(TJvKeyboard , 'Add', TKeyboard_Add, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvKeyboard , 'Add2', TKeyboard_Add2, 5, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvKeyboard , 'Clear', TKeyboard_Clear, 0, [0], varEmpty);
    AddGet(TJvKeyboard , 'SetDefLayot', TKeyboard_SetDefLayot, 0, [0], varEmpty);
   { EJvEditorError  }
    AddClass('JvEditor', EJvEditorError , 'EJvEditorError ');
   { TTabStop }
    AddConst('JvEditor', 'tsTabStop', tsTabStop);
    AddConst('JvEditor', 'tsAutoIndent', tsAutoIndent);
   { TJvCustomEditor }
    AddClass('JvEditor', TJvCustomEditor, 'TJvCustomEditor');
    AddGet(TJvCustomEditor, 'Create', TRACustomEditor_Create, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'SetLeftTop', TRACustomEditor_SetLeftTop, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'ClipBoardCopy', TRACustomEditor_ClipBoardCopy, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'ClipBoardPaste', TRACustomEditor_ClipBoardPaste, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'ClipBoardCut', TRACustomEditor_ClipBoardCut, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'DeleteSelected', TRACustomEditor_DeleteSelected, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'CalcCellRect', TRACustomEditor_CalcCellRect, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'SetCaret', TRACustomEditor_SetCaret, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'CaretFromPos', TRACustomEditor_CaretFromPos, 3, [varEmpty, varByRef, varByRef], varEmpty);
    AddGet(TJvCustomEditor, 'PosFromCaret', TRACustomEditor_PosFromCaret, 2, [varEmpty, varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'PaintCaret', TRACustomEditor_PaintCaret, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'GetTextLen', TRACustomEditor_GetTextLen, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'GetSelText', TRACustomEditor_GetSelText, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'SetSelText', TRACustomEditor_SetSelText, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'GetWordOnCaret', TRACustomEditor_GetWordOnCaret, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'BeginUpdate', TRACustomEditor_BeginUpdate, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'EndUpdate', TRACustomEditor_EndUpdate, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'MakeRowVisible', TRACustomEditor_MakeRowVisible, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'Command', TRACustomEditor_Command, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'PostCommand', TRACustomEditor_PostCommand, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'InsertText', TRACustomEditor_InsertText, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'ReplaceWord', TRACustomEditor_ReplaceWord, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'ReplaceWord2', TRACustomEditor_ReplaceWord2, 1, [varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'BeginCompound', TRACustomEditor_BeginCompound, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'EndCompound', TRACustomEditor_EndCompound, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'GetText', TRACustomEditor_GetText, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvCustomEditor, 'LeftCol', TRACustomEditor_Read_LeftCol, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'TopRow', TRACustomEditor_Read_TopRow, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'VisibleColCount', TRACustomEditor_Read_VisibleColCount, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'VisibleRowCount', TRACustomEditor_Read_VisibleRowCount, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'LastVisibleCol', TRACustomEditor_Read_LastVisibleCol, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'LastVisibleRow', TRACustomEditor_Read_LastVisibleRow, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'Cols', TRACustomEditor_Read_Cols, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'Cols', TRACustomEditor_Write_Cols, 0, [0]);
    AddGet(TJvCustomEditor, 'Rows', TRACustomEditor_Read_Rows, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'Rows', TRACustomEditor_Write_Rows, 0, [0]);
    AddGet(TJvCustomEditor, 'CaretX', TRACustomEditor_Read_CaretX, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'CaretX', TRACustomEditor_Write_CaretX, 0, [0]);
    AddGet(TJvCustomEditor, 'CaretY', TRACustomEditor_Read_CaretY, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'CaretY', TRACustomEditor_Write_CaretY, 0, [0]);
    AddGet(TJvCustomEditor, 'Modified', TRACustomEditor_Read_Modified, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'Modified', TRACustomEditor_Write_Modified, 0, [0]);
    AddGet(TJvCustomEditor, 'SelStart', TRACustomEditor_Read_SelStart, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'SelStart', TRACustomEditor_Write_SelStart, 0, [0]);
    AddGet(TJvCustomEditor, 'SelLength', TRACustomEditor_Read_SelLength, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'SelLength', TRACustomEditor_Write_SelLength, 0, [0]);
    AddGet(TJvCustomEditor, 'SelText', TRACustomEditor_Read_SelText, 0, [0], varEmpty);
    AddSet(TJvCustomEditor, 'SelText', TRACustomEditor_Write_SelText, 0, [0]);
   // AddGet(TJvCustomEditor, 'BookMarks', TRACustomEditor_Read_BookMarks, 0, [0], nil);
    AddGet(TJvCustomEditor, 'Keyboard', TRACustomEditor_Read_Keyboard, 0, [0], varEmpty);
   // AddGet(TJvCustomEditor, 'CellRect', TRACustomEditor_Read_CellRect, 0, [0], nil);
    AddGet(TJvCustomEditor, 'UndoBuffer', TRACustomEditor_Read_UndoBuffer, 0, [0], varEmpty);
    AddGet(TJvCustomEditor, 'Recording', TRACustomEditor_Read_Recording, 0, [0], varEmpty);

    AddIGet(TJvCustomEditor, 'BookMarkX', TRACustomEditor_Read_BookMarkX, 1, [varInteger], varEmpty);
    AddIGet(TJvCustomEditor, 'BookMarkY', TRACustomEditor_Read_BookMarkY, 1, [varInteger], varEmpty);
    AddIGet(TJvCustomEditor, 'BookMarkValid', TRACustomEditor_Read_BookMarkValid, 1, [varInteger], varEmpty);
    AddISet(TJvCustomEditor, 'BookMarkX', TRACustomEditor_Write_BookMarkX, 1, [varInteger]);
    AddISet(TJvCustomEditor, 'BookMarkY', TRACustomEditor_Write_BookMarkY, 1, [varInteger]);
    AddISet(TJvCustomEditor, 'BookMarkValid', TRACustomEditor_Write_BookMarkValid, 1, [varInteger]);

   { TCompletionList }
    AddConst('JvEditor', 'cmIdentifers', cmIdentifers);
    AddConst('JvEditor', 'cmTemplates', cmTemplates);

    AddConst('JvEditor', 'ecCharFirst', ecCharFirst);
    AddConst('JvEditor', 'ecCharLast', ecCharLast);
    AddConst('JvEditor', 'ecCommandFirst', ecCommandFirst);
    AddConst('JvEditor', 'ecUser', ecUser);
    AddConst('JvEditor', 'ecLeft', ecLeft);
    AddConst('JvEditor', 'ecUp', ecUp);
    AddConst('JvEditor', 'ecRight', ecRight);
    AddConst('JvEditor', 'ecDown', ecDown);
    AddConst('JvEditor', 'ecSelLeft', ecSelLeft);
    AddConst('JvEditor', 'ecSelUp', ecSelUp);
    AddConst('JvEditor', 'ecSelRight', ecSelRight);
    AddConst('JvEditor', 'ecSelDown', ecSelDown);
    AddConst('JvEditor', 'ecPrevWord', ecPrevWord);
    AddConst('JvEditor', 'ecNextWord', ecNextWord);
    AddConst('JvEditor', 'ecSelPrevWord', ecSelPrevWord);
    AddConst('JvEditor', 'ecSelNextWord', ecSelNextWord);
    AddConst('JvEditor', 'ecSelWord', ecSelWord);
    AddConst('JvEditor', 'ecWindowTop', ecWindowTop);
    AddConst('JvEditor', 'ecWindowBottom', ecWindowBottom);
    AddConst('JvEditor', 'ecPrevPage', ecPrevPage);
    AddConst('JvEditor', 'ecNextPage', ecNextPage);
    AddConst('JvEditor', 'ecSelPrevPage', ecSelPrevPage);
    AddConst('JvEditor', 'ecSelNextPage', ecSelNextPage);
    AddConst('JvEditor', 'ecBeginLine', ecBeginLine);
    AddConst('JvEditor', 'ecEndLine', ecEndLine);
    AddConst('JvEditor', 'ecBeginDoc', ecBeginDoc);
    AddConst('JvEditor', 'ecEndDoc', ecEndDoc);
    AddConst('JvEditor', 'ecSelBeginLine', ecSelBeginLine);
    AddConst('JvEditor', 'ecSelEndLine', ecSelEndLine);
    AddConst('JvEditor', 'ecSelBeginDoc', ecSelBeginDoc);
    AddConst('JvEditor', 'ecSelEndDoc', ecSelEndDoc);
    AddConst('JvEditor', 'ecSelAll', ecSelAll);
    AddConst('JvEditor', 'ecScrollLineUp', ecScrollLineUp);
    AddConst('JvEditor', 'ecScrollLineDown', ecScrollLineDown);
    AddConst('JvEditor', 'ecInsertPara', ecInsertPara);
    AddConst('JvEditor', 'ecBackspace', ecBackspace);
    AddConst('JvEditor', 'ecDelete', ecDelete);
    AddConst('JvEditor', 'ecChangeInsertMode', ecChangeInsertMode);
    AddConst('JvEditor', 'ecTab', ecTab);
    AddConst('JvEditor', 'ecBackTab', ecBackTab);
    AddConst('JvEditor', 'ecIndent', ecIndent);
    AddConst('JvEditor', 'ecUnindent', ecUnindent);
    AddConst('JvEditor', 'ecDeleteSelected', ecDeleteSelected);
    AddConst('JvEditor', 'ecClipboardCopy', ecClipboardCopy);
    AddConst('JvEditor', 'ecClipboardCut', ecClipboardCut);
    AddConst('JvEditor', 'ecClipBoardPaste', ecClipBoardPaste);
    AddConst('JvEditor', 'ecDeleteLine', ecDeleteLine);
    AddConst('JvEditor', 'ecDeleteWord', ecDeleteWord);
    AddConst('JvEditor', 'ecToUpperCase', ecToUpperCase);
    AddConst('JvEditor', 'ecToLowerCase', ecToLowerCase);
    AddConst('JvEditor', 'ecChangeCase', ecChangeCase);
    AddConst('JvEditor', 'ecUndo', ecUndo);
    AddConst('JvEditor', 'ecRedo', ecRedo);
    AddConst('JvEditor', 'ecBeginCompound', ecBeginCompound);
    AddConst('JvEditor', 'ecEndCompound', ecEndCompound);
    AddConst('JvEditor', 'ecBeginUpdate', ecBeginUpdate);
    AddConst('JvEditor', 'ecEndUpdate', ecEndUpdate);
    AddConst('JvEditor', 'ecSetBookmark0', ecSetBookmark0);
    AddConst('JvEditor', 'ecSetBookmark1', ecSetBookmark1);
    AddConst('JvEditor', 'ecSetBookmark2', ecSetBookmark2);
    AddConst('JvEditor', 'ecSetBookmark3', ecSetBookmark3);
    AddConst('JvEditor', 'ecSetBookmark4', ecSetBookmark4);
    AddConst('JvEditor', 'ecSetBookmark5', ecSetBookmark5);
    AddConst('JvEditor', 'ecSetBookmark6', ecSetBookmark6);
    AddConst('JvEditor', 'ecSetBookmark7', ecSetBookmark7);
    AddConst('JvEditor', 'ecSetBookmark8', ecSetBookmark8);
    AddConst('JvEditor', 'ecSetBookmark9', ecSetBookmark9);
    AddConst('JvEditor', 'ecGotoBookmark0', ecGotoBookmark0);
    AddConst('JvEditor', 'ecGotoBookmark1', ecGotoBookmark1);
    AddConst('JvEditor', 'ecGotoBookmark2', ecGotoBookmark2);
    AddConst('JvEditor', 'ecGotoBookmark3', ecGotoBookmark3);
    AddConst('JvEditor', 'ecGotoBookmark4', ecGotoBookmark4);
    AddConst('JvEditor', 'ecGotoBookmark5', ecGotoBookmark5);
    AddConst('JvEditor', 'ecGotoBookmark6', ecGotoBookmark6);
    AddConst('JvEditor', 'ecGotoBookmark7', ecGotoBookmark7);
    AddConst('JvEditor', 'ecGotoBookmark8', ecGotoBookmark8);
    AddConst('JvEditor', 'ecGotoBookmark9', ecGotoBookmark9);
    AddConst('JvEditor', 'ecCompletionIdentifers', ecCompletionIdentifers);
    AddConst('JvEditor', 'ecCompletionTemplates', ecCompletionTemplates);
    AddConst('JvEditor', 'ecRecordMacro', ecRecordMacro);
    AddConst('JvEditor', 'ecPlayMacro', ecPlayMacro);
    AddConst('JvEditor', 'ecBeginRecord', ecBeginRecord);
    AddConst('JvEditor', 'ecEndRecord', ecEndRecord);
  end;    { with }
  RegisterClasses([TJvEditor, TJvHLEditor]);
end;    { RegisterJvInterpreterAdapter }

end.
