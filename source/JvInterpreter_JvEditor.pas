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

uses
  JvInterpreter;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);

implementation

uses
  Classes,
  JvEditor, JvHLEditor, JvInterpreter_Windows;

{ TJvKeyboard }

{ constructor Create }

procedure TKeyboard_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TJvKeyboard.Create);
end;

{ procedure Add(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState); }

procedure TKeyboard_Add(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvKeyboard(Args.Obj).Add(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))));
end;

{ procedure Add2(const ACommand: TEditCommand; const AKey1: word; const AShift1: TShiftState; const AKey2: word; const AShift2: TShiftState); }

procedure TKeyboard_Add2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvKeyboard(Args.Obj).Add2(Args.Values[0], Args.Values[1], TShiftState(Byte(V2S(Args.Values[2]))), Args.Values[3],
    TShiftState(Byte(V2S(Args.Values[4]))));
end;

{ procedure Clear; }

procedure TKeyboard_Clear(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvKeyboard(Args.Obj).Clear;
end;

{ procedure SetDefLayout; }

procedure TKeyboard_SetDefLayout(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvKeyboard(Args.Obj).SetDefLayout;
end;

{ EJvEditorError  }

{ TJvCustomEditor }

{ constructor Create(AOwner: TComponent) }

procedure TRACustomEditor_Create(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TJvCustomEditor.Create(V2O(Args.Values[0]) as TComponent));
end;

{ procedure SetLeftTop(ALeftCol, ATopRow: integer); }

procedure TRACustomEditor_SetLeftTop(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SetLeftTop(Args.Values[0], Args.Values[1]);
end;

{ procedure ClipBoardCopy; }

procedure TRACustomEditor_ClipBoardCopy(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardCopy;
end;

{ procedure ClipBoardPaste; }

procedure TRACustomEditor_ClipBoardPaste(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardPaste;
end;

{ procedure ClipBoardCut; }

procedure TRACustomEditor_ClipBoardCut(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).ClipBoardCut;
end;

{ procedure DeleteSelected; }

procedure TRACustomEditor_DeleteSelected(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).DeleteSelected;
end;

{ function CalcCellRect(const X, Y: integer): TRect; }

procedure TRACustomEditor_CalcCellRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := Rect2Var(TJvCustomEditor(Args.Obj).CalcCellRect(Args.Values[0], Args.Values[1]));
end;

{ procedure SetCaret(X, Y: integer); }

procedure TRACustomEditor_SetCaret(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SetCaret(Args.Values[0], Args.Values[1]);
end;

{ procedure CaretFromPos(const Pos: integer; var X, Y: integer); }

procedure TRACustomEditor_CaretFromPos(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).CaretFromPos(Args.Values[0], TVarData(Args.Values[1]).vInteger,
    TVarData(Args.Values[2]).vInteger);
end;

{ function PosFromCaret(const X, Y: integer): integer; }

procedure TRACustomEditor_PosFromCaret(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).PosFromCaret(Args.Values[0], Args.Values[1]);
end;

{ procedure PaintCaret(const bShow: boolean); }

procedure TRACustomEditor_PaintCaret(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).PaintCaret(Args.Values[0]);
end;

{ function GetTextLen: Integer; }

procedure TRACustomEditor_GetTextLen(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetTextLen;
end;

{ function GetSelText: string; }

procedure TRACustomEditor_GetSelText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetSelText;
end;

{ procedure SetSelText(const AValue: string); }

procedure TRACustomEditor_SetSelText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SetSelText(Args.Values[0]);
end;

{ function GetWordOnCaret: string; }

procedure TRACustomEditor_GetWordOnCaret(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetWordOnCaret;
end;

{ procedure BeginUpdate; }

procedure TRACustomEditor_BeginUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).BeginUpdate;
end;

{ procedure EndUpdate; }

procedure TRACustomEditor_EndUpdate(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).EndUpdate;
end;

{ procedure MakeRowVisible(ARow: Integer); }

procedure TRACustomEditor_MakeRowVisible(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).MakeRowVisible(Args.Values[0]);
end;

{ procedure Command(ACommand: TEditCommand); }

procedure TRACustomEditor_Command(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).Command(Args.Values[0]);
end;

{ procedure PostCommand(ACommand: TEditCommand); }

procedure TRACustomEditor_PostCommand(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).PostCommand(Args.Values[0]);
end;

{ procedure InsertText(const Text: string); }

procedure TRACustomEditor_InsertText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).InsertText(Args.Values[0]);
end;

{ procedure ReplaceWord(const NewString: string); }

procedure TRACustomEditor_ReplaceWord(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).ReplaceWord(Args.Values[0]);
end;

{ procedure ReplaceWord2(const NewString: string); }

procedure TRACustomEditor_ReplaceWord2(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).ReplaceWord2(Args.Values[0]);
end;

{ procedure BeginCompound; }

procedure TRACustomEditor_BeginCompound(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).BeginCompound;
end;

{ procedure EndCompound; }

procedure TRACustomEditor_EndCompound(var Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).EndCompound;
end;

{ function GetText(Position: Longint; Buffer: PChar; Count: Longint): Longint; }

procedure TRACustomEditor_GetText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).GetText(Args.Values[0], PChar(string(Args.Values[1])), Args.Values[2]);
end;

{ property Read LeftCol: integer }

procedure TRACustomEditor_Read_LeftCol(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LeftCol;
end;

{ property Read TopRow: integer }

procedure TRACustomEditor_Read_TopRow(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).TopRow;
end;

{ property Read VisibleColCount: integer }

procedure TRACustomEditor_Read_VisibleColCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).VisibleColCount;
end;

{ property Read VisibleRowCount: integer }

procedure TRACustomEditor_Read_VisibleRowCount(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).VisibleRowCount;
end;

{ property Read LastVisibleCol: integer }

procedure TRACustomEditor_Read_LastVisibleCol(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LastVisibleCol;
end;

{ property Read LastVisibleRow: integer }

procedure TRACustomEditor_Read_LastVisibleRow(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).LastVisibleRow;
end;

{ property Read Cols: integer }

procedure TRACustomEditor_Read_Cols(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Cols;
end;

{ property Write Cols(Value: integer) }

procedure TRACustomEditor_Write_Cols(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).Cols := Value;
end;

{ property Read Rows: integer }

procedure TRACustomEditor_Read_Rows(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Rows;
end;

{ property Write Rows(Value: integer) }

procedure TRACustomEditor_Write_Rows(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).Rows := Value;
end;

{ property Read CaretX: integer }

procedure TRACustomEditor_Read_CaretX(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CaretX;
end;

{ property Write CaretX(Value: integer) }

procedure TRACustomEditor_Write_CaretX(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).CaretX := Value;
end;

{ property Read CaretY: integer }

procedure TRACustomEditor_Read_CaretY(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CaretY;
end;

{ property Write CaretY(Value: integer) }

procedure TRACustomEditor_Write_CaretY(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).CaretY := Value;
end;

{ property Read Modified: boolean }

procedure TRACustomEditor_Read_Modified(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Modified;
end;

{ property Write Modified(Value: boolean) }

procedure TRACustomEditor_Write_Modified(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).Modified := Value;
end;

{ property Read SelStart: integer }

procedure TRACustomEditor_Read_SelStart(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelStart;
end;

{ property Write SelStart(Value: integer) }

procedure TRACustomEditor_Write_SelStart(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SelStart := Value;
end;

{ property Read SelLength: integer }

procedure TRACustomEditor_Read_SelLength(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelLength;
end;

{ property Write SelLength(Value: integer) }

procedure TRACustomEditor_Write_SelLength(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SelLength := Value;
end;

{ property Read SelText: string }

procedure TRACustomEditor_Read_SelText(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).SelText;
end;

{ property Write SelText(Value: string) }

procedure TRACustomEditor_Write_SelText(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).SelText := Value;
end;

(*
{ property Read BookMarks: TBookMarks }
procedure TRACustomEditor_Read_BookMarks(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks;
end;
*)

{ property Read Keyboard: TJvKeyboard  }

procedure TRACustomEditor_Read_Keyboard(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TJvCustomEditor(Args.Obj).Keyboard);
end;

(*
{ property Read CellRect: TCellRect }
procedure TRACustomEditor_Read_CellRect(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).CellRect;
end;
*)

{ property Read UndoBuffer: TUndoBuffer }

procedure TRACustomEditor_Read_UndoBuffer(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := O2V(TJvCustomEditor(Args.Obj).UndoBuffer);
end;

{ property Read Recording: boolean }

procedure TRACustomEditor_Read_Recording(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).Recording;
end;

procedure TRACustomEditor_Read_BookMarkX(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X;
end;

procedure TRACustomEditor_Read_BookMarkY(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y;
end;

procedure TRACustomEditor_Read_BookMarkValid(var Value: Variant; Args: TJvInterpreterArgs);
begin
  Value := TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid;
end;

procedure TRACustomEditor_Write_BookMarkX(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].X := Value;
end;

procedure TRACustomEditor_Write_BookMarkY(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Y := Value;
end;

procedure TRACustomEditor_Write_BookMarkValid(const Value: Variant; Args: TJvInterpreterArgs);
begin
  TJvCustomEditor(Args.Obj).BookMarks[Integer(Args.Values[0])].Valid := Value;
end;

procedure RegisterJvInterpreterAdapter(JvInterpreterAdapter: TJvInterpreterAdapter);
const
  cJvEditor = 'JvEditor';
begin
  with JvInterpreterAdapter do
  begin
    { TModifiedAction }
    AddConst(cJvEditor, 'maInsert', maInsert);
    AddConst(cJvEditor, 'maDelete', maDelete);
    { TJvKeyboard  }
    AddClass(cJvEditor, TJvKeyboard, 'TJvKeyboard ');
    AddGet(TJvKeyboard, 'Create', TKeyboard_Create, 0, [0], varEmpty);
    AddGet(TJvKeyboard, 'Add', TKeyboard_Add, 3, [varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvKeyboard, 'Add2', TKeyboard_Add2, 5, [varEmpty, varEmpty, varEmpty, varEmpty, varEmpty], varEmpty);
    AddGet(TJvKeyboard, 'Clear', TKeyboard_Clear, 0, [0], varEmpty);
    AddGet(TJvKeyboard, 'SetDefLayout', TKeyboard_SetDefLayout, 0, [0], varEmpty);
    { EJvEditorError  }
    AddClass(cJvEditor, EJvEditorError, 'EJvEditorError ');
    { TTabStop }
//    AddConst(cJvEditor, 'tsTabStop', tsTabStop);
//    AddConst(cJvEditor, 'tsAutoIndent', tsAutoIndent);
    { TJvCustomEditor }
    AddClass(cJvEditor, TJvCustomEditor, 'TJvCustomEditor');
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
    AddConst(cJvEditor, 'cmIdentifiers', cmIdentifiers);
    AddConst(cJvEditor, 'cmTemplates', cmTemplates);

    AddConst(cJvEditor, 'ecCharFirst', ecCharFirst);
    AddConst(cJvEditor, 'ecCharLast', ecCharLast);
    AddConst(cJvEditor, 'ecCommandFirst', ecCommandFirst);
    AddConst(cJvEditor, 'ecUser', ecUser);
    AddConst(cJvEditor, 'ecLeft', ecLeft);
    AddConst(cJvEditor, 'ecUp', ecUp);
    AddConst(cJvEditor, 'ecRight', ecRight);
    AddConst(cJvEditor, 'ecDown', ecDown);
    AddConst(cJvEditor, 'ecSelLeft', ecSelLeft);
    AddConst(cJvEditor, 'ecSelUp', ecSelUp);
    AddConst(cJvEditor, 'ecSelRight', ecSelRight);
    AddConst(cJvEditor, 'ecSelDown', ecSelDown);
    AddConst(cJvEditor, 'ecPrevWord', ecPrevWord);
    AddConst(cJvEditor, 'ecNextWord', ecNextWord);
    AddConst(cJvEditor, 'ecSelPrevWord', ecSelPrevWord);
    AddConst(cJvEditor, 'ecSelNextWord', ecSelNextWord);
    AddConst(cJvEditor, 'ecSelWord', ecSelWord);
    AddConst(cJvEditor, 'ecWindowTop', ecWindowTop);
    AddConst(cJvEditor, 'ecWindowBottom', ecWindowBottom);
    AddConst(cJvEditor, 'ecPrevPage', ecPrevPage);
    AddConst(cJvEditor, 'ecNextPage', ecNextPage);
    AddConst(cJvEditor, 'ecSelPrevPage', ecSelPrevPage);
    AddConst(cJvEditor, 'ecSelNextPage', ecSelNextPage);
    AddConst(cJvEditor, 'ecBeginLine', ecBeginLine);
    AddConst(cJvEditor, 'ecEndLine', ecEndLine);
    AddConst(cJvEditor, 'ecBeginDoc', ecBeginDoc);
    AddConst(cJvEditor, 'ecEndDoc', ecEndDoc);
    AddConst(cJvEditor, 'ecSelBeginLine', ecSelBeginLine);
    AddConst(cJvEditor, 'ecSelEndLine', ecSelEndLine);
    AddConst(cJvEditor, 'ecSelBeginDoc', ecSelBeginDoc);
    AddConst(cJvEditor, 'ecSelEndDoc', ecSelEndDoc);
    AddConst(cJvEditor, 'ecSelAll', ecSelAll);
    AddConst(cJvEditor, 'ecScrollLineUp', ecScrollLineUp);
    AddConst(cJvEditor, 'ecScrollLineDown', ecScrollLineDown);
    AddConst(cJvEditor, 'ecInsertPara', ecInsertPara);
    AddConst(cJvEditor, 'ecBackspace', ecBackspace);
    AddConst(cJvEditor, 'ecDelete', ecDelete);
    AddConst(cJvEditor, 'ecChangeInsertMode', ecChangeInsertMode);
    AddConst(cJvEditor, 'ecTab', ecTab);
    AddConst(cJvEditor, 'ecBackTab', ecBackTab);
    AddConst(cJvEditor, 'ecIndent', ecIndent);
    AddConst(cJvEditor, 'ecUnindent', ecUnindent);
    AddConst(cJvEditor, 'ecDeleteSelected', ecDeleteSelected);
    AddConst(cJvEditor, 'ecClipboardCopy', ecClipboardCopy);
    AddConst(cJvEditor, 'ecClipboardCut', ecClipboardCut);
    AddConst(cJvEditor, 'ecClipBoardPaste', ecClipBoardPaste);
    AddConst(cJvEditor, 'ecDeleteLine', ecDeleteLine);
    AddConst(cJvEditor, 'ecDeleteWord', ecDeleteWord);
    AddConst(cJvEditor, 'ecToUpperCase', ecToUpperCase);
    AddConst(cJvEditor, 'ecToLowerCase', ecToLowerCase);
    AddConst(cJvEditor, 'ecChangeCase', ecChangeCase);
    AddConst(cJvEditor, 'ecUndo', ecUndo);
    AddConst(cJvEditor, 'ecRedo', ecRedo);
    AddConst(cJvEditor, 'ecBeginCompound', ecBeginCompound);
    AddConst(cJvEditor, 'ecEndCompound', ecEndCompound);
    AddConst(cJvEditor, 'ecBeginUpdate', ecBeginUpdate);
    AddConst(cJvEditor, 'ecEndUpdate', ecEndUpdate);
    AddConst(cJvEditor, 'ecSetBookmark0', ecSetBookmark0);
    AddConst(cJvEditor, 'ecSetBookmark1', ecSetBookmark1);
    AddConst(cJvEditor, 'ecSetBookmark2', ecSetBookmark2);
    AddConst(cJvEditor, 'ecSetBookmark3', ecSetBookmark3);
    AddConst(cJvEditor, 'ecSetBookmark4', ecSetBookmark4);
    AddConst(cJvEditor, 'ecSetBookmark5', ecSetBookmark5);
    AddConst(cJvEditor, 'ecSetBookmark6', ecSetBookmark6);
    AddConst(cJvEditor, 'ecSetBookmark7', ecSetBookmark7);
    AddConst(cJvEditor, 'ecSetBookmark8', ecSetBookmark8);
    AddConst(cJvEditor, 'ecSetBookmark9', ecSetBookmark9);
    AddConst(cJvEditor, 'ecGotoBookmark0', ecGotoBookmark0);
    AddConst(cJvEditor, 'ecGotoBookmark1', ecGotoBookmark1);
    AddConst(cJvEditor, 'ecGotoBookmark2', ecGotoBookmark2);
    AddConst(cJvEditor, 'ecGotoBookmark3', ecGotoBookmark3);
    AddConst(cJvEditor, 'ecGotoBookmark4', ecGotoBookmark4);
    AddConst(cJvEditor, 'ecGotoBookmark5', ecGotoBookmark5);
    AddConst(cJvEditor, 'ecGotoBookmark6', ecGotoBookmark6);
    AddConst(cJvEditor, 'ecGotoBookmark7', ecGotoBookmark7);
    AddConst(cJvEditor, 'ecGotoBookmark8', ecGotoBookmark8);
    AddConst(cJvEditor, 'ecGotoBookmark9', ecGotoBookmark9);
    AddConst(cJvEditor, 'ecCompletionIdentifiers', ecCompletionIdentifiers);
    AddConst(cJvEditor, 'ecCompletionTemplates', ecCompletionTemplates);
    AddConst(cJvEditor, 'ecRecordMacro', ecRecordMacro);
    AddConst(cJvEditor, 'ecPlayMacro', ecPlayMacro);
    AddConst(cJvEditor, 'ecBeginRecord', ecBeginRecord);
    AddConst(cJvEditor, 'ecEndRecord', ecEndRecord);
  end;
  RegisterClasses([TJvEditor, TJvHLEditor]);
end;

end.

