{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvSALCore.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvSALCore;

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QControls, QForms, QDialogs,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvSAL, JvTypes;

type
  TJvSALCore = class(TComponent)
  private
    FSal: TJvSAL;
  public
    procedure AddProcedures(ASal: TJvSAL);
    // SAL language
    procedure xIf;
    procedure xpIf;
    procedure xIfNot;
    procedure xpIfNot;
    procedure xElse;
    procedure xpElse;
    procedure xEndIf;
    procedure xpEndIf;
    procedure xRepeat;
    procedure xpRepeat;
    procedure xUntil;
    procedure xpUntil;
    procedure xSelect;
    procedure xCase;
    procedure xpCase;
    procedure xEndCase;
    procedure xpEndCase;
    procedure xEndSelect;
    procedure xExit;
    procedure xSet;
    procedure xGet;
    procedure xAsk;
    procedure xSay;
    procedure xTrue;
    procedure xFalse;
    procedure xAnd;
    procedure X_Or;
    procedure xXOr;
    procedure xNot;
    procedure xEq;
    procedure xNe;
    procedure xGe;
    procedure xLe;
    procedure xGt;
    procedure xLt;
    procedure xNeg;
    procedure xAbs;
    procedure xAdd;
    procedure xSub;
    procedure xMul;
    procedure xDiv;
    procedure xvAdd; // directly add to Variable
    procedure xvSub;
    procedure xvMul;
    procedure xvDiv;
    procedure xDec;
    procedure xInc;
    procedure xDecZero;
    procedure xCr;
    procedure xDup;
    procedure xDrop;
    procedure xSwap;
    procedure xCap;
  end;

implementation

uses
  Math,
  JvConsts, JvResources;

procedure TJvSALCore.AddProcedures(ASal: TJvSAL);
begin
  FSal := ASal;
  with FSal do
  begin
    // do not localize
    AddProcedure('if', xIf, xpIf);
    AddProcedure('ifnot', xIfNot, xpIfNot);
    AddProcedure('else', xElse, xpElse);
    AddProcedure('endif', xEndIf, xpEndIf);
    AddProcedure('repeat', xRepeat, xpRepeat);
    AddProcedure('until', xUntil, xpUntil);
    AddProcedure('select', xSelect, nil);
    AddProcedure('endselect', xEndSelect, nil);
    AddProcedure('case', xCase, xpCase);
    AddProcedure('endcase', xEndCase, xpEndCase);
    AddProcedure('exit', xExit, nil);
    AddProcedure('get', xGet, nil);
    AddProcedure('set', xSet, nil);
    AddProcedure('ask', xAsk, nil);
    AddProcedure('say', xSay, nil);
    AddProcedure('true', xTrue, nil);
    AddProcedure('false', xFalse, nil);
    AddProcedure('and', xAnd, nil);
    AddProcedure('or', x_Or, nil);
    AddProcedure('xor', xXor, nil);
    AddProcedure('not', xNot, nil);
    AddProcedure('=', xEq, nil);
    AddProcedure('<>', xNe, nil);
    AddProcedure('>=', xGe, nil);
    AddProcedure('<=', xLe, nil);
    AddProcedure('>', xGt, nil);
    AddProcedure('<', xLt, nil);
    AddProcedure('neg', xNeg, nil);
    AddProcedure('abs', xAbs, nil);
    AddProcedure('+', xAdd, nil);
    AddProcedure('-', xSub, nil);
    AddProcedure('*', xMul, nil);
    AddProcedure('/', xDiv, nil);
    AddProcedure('+=', xvAdd, nil);
    AddProcedure('-=', xvSub, nil);
    AddProcedure('*=', xvMul, nil);
    AddProcedure('/=', xvDiv, nil);
    AddProcedure('dec', xDec, nil);
    AddProcedure('inc', xInc, nil);
    AddProcedure('dec?', xDecZero, nil);
    AddProcedure('cr', xCr, nil);
    AddProcedure('dup', xDup, nil);
    AddProcedure('drop', xDrop, nil);
    AddProcedure('swap', xSwap, nil);
    AddProcedure('cap', xCap, nil);
  end;
end;

procedure TJvSALCore.X_Or;
begin
  FSal.BoolPush(FSal.BoolPop or FSal.BoolPop);
end;

procedure TJvSALCore.xAbs;
begin
  FSal.Push(Abs(FSal.Pop));
end;

procedure TJvSALCore.xAdd;
var
  V1, V2: Variant;
begin
  V2 := FSal.Pop;
  V1 := FSal.Pop;
  FSal.Push(V1 + V2);
end;

procedure TJvSALCore.xAnd;
begin
  FSal.BoolPush(FSal.BoolPop and FSal.BoolPop);
end;

procedure TJvSALCore.xAsk;
var
  S: string;
  V: Variant;
begin
  S := FSal.Pop;
  V := InputBox(FSal.Caption, S, '');
  if V <> '' then
    FSal.Push(V);
end;

procedure TJvSALCore.xCap;
begin
  FSal.Caption := FSal.Pop;
end;

procedure TJvSALCore.xCase;
var
  V1: Variant;
begin
  V1 := FSal.Pop;
  if V1 = FSal.TheSelect then
  begin
  end
  else
    FSal.PC := TJvAtom(FSal.Atoms.Objects[FSal.PcProc]).Value + 1;
end;

procedure TJvSALCore.xCr;
begin
  FSal.Push(Cr);
end;

procedure TJvSALCore.xDec;
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  FSal.Variable.Value := FSal.Variable.Value - 1;
end;

procedure TJvSALCore.xDecZero; // dec?  decrements a Variable and test for zero
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  FSal.Variable.Value := FSal.Variable.Value - 1;
  FSal.BoolPush(FSal.Variable.Value = 0);
end;

procedure TJvSALCore.xDiv;
var
  V1, V2: Double;
begin
  V2 := FSal.Pop;
  if V2 = 0.0 then
    raise EJVCLException.CreateRes(@RsEDivisionByZeroError);
  V1 := FSal.Pop;
  FSal.Push(V1 / V2);
end;

procedure TJvSALCore.xDrop;
begin
  FSal.Pop;
end;

procedure TJvSALCore.xDup;
var
  V1: Variant;
begin
  V1 := FSal.Pop;
  FSal.Push(V1);
  FSal.Push(V1);
end;

procedure TJvSALCore.xElse;
begin
  FSal.PC := TJvAtom(FSal.Atoms.Objects[FSal.PcProc]).Value + 1;
end;

procedure TJvSALCore.xEndCase;
// Removed Hint
//var
//  c: Integer;
begin
  //  c:=FSal.Atoms.Count;
  while FSal.PC < FSal.Atoms.Count do
  begin
    if FSal.Atoms[FSal.PC] = 'endselect' then // do not localize
    begin
      FSal.PC := FSal.PC + 1;
      Exit;
    end;
    FSal.PC := FSal.PC + 1;
  end;
  raise EJVCLException.CreateRes(@RsEMissingendselect);
end;

procedure TJvSALCore.xEndIf;
begin
  // do nothing
end;

procedure TJvSALCore.xEndSelect;
begin
  // do nothing
end;

procedure TJvSALCore.xEq;
begin
  FSal.BoolPush(FSal.Pop = FSal.Pop);
end;

procedure TJvSALCore.xExit;
begin
  FSal.PC := FSal.Atoms.Count;
end;

procedure TJvSALCore.xFalse;
begin
  FSal.BoolPush(False);
end;

procedure TJvSALCore.xGe;
begin
  FSal.BoolPush(FSal.Pop >= FSal.Pop);
end;

procedure TJvSALCore.xGet;
begin
  FSal.Push(FSal.Variable.Value);
end;

procedure TJvSALCore.xGt;
begin
  FSal.BoolPush(FSal.Pop > FSal.Pop);
end;

procedure TJvSALCore.xIf;
begin
  if not FSal.BoolPop then
    FSal.PC := TJvAtom(FSal.Atoms.Objects[FSal.PcProc]).Value + 1;
end;

procedure TJvSALCore.xIfNot;
begin
  if FSal.BoolPop then
    FSal.PC := TJvAtom(FSal.Atoms.Objects[FSal.PcProc]).Value + 1;
end;

procedure TJvSALCore.xInc;
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  FSal.Variable.Value := FSal.Variable.Value + 1;
end;

procedure TJvSALCore.xLe;
begin
  FSal.BoolPush(FSal.Pop <= FSal.Pop);
end;

procedure TJvSALCore.xLt;
begin
  FSal.BoolPush(FSal.Pop < FSal.Pop);
end;

procedure TJvSALCore.xMul;
var
  V1, V2: Double;
begin
  V2 := FSal.Pop;
  V1 := FSal.Pop;
  FSal.Push(V1 * V2);
end;

procedure TJvSALCore.xNe;
begin
  FSal.BoolPush(FSal.Pop <> FSal.Pop);
end;

procedure TJvSALCore.xNeg;
begin
  FSal.Push(0 - FSal.Pop);
end;

procedure TJvSALCore.xNot;
begin
  FSal.BoolPush(not FSal.BoolPop);
end;

procedure TJvSALCore.xRepeat;
begin
  // do nothing
end;

procedure TJvSALCore.xSay;
begin
  ShowMessage(FSal.Pop);
end;

procedure TJvSALCore.xSelect;
begin
  FSal.TheSelect := FSal.Pop;
end;

procedure TJvSALCore.xSet;
begin
  FSal.Variable.Value := FSal.Pop;
end;

procedure TJvSALCore.xSub;
var
  V1, V2: Double;
begin
  V2 := FSal.Pop;
  V1 := FSal.Pop;
  FSal.Push(V1 - V2);
end;

procedure TJvSALCore.xSwap;
var
  V1, V2: Variant;
begin
  V2 := FSal.Pop;
  V1 := FSal.Pop;
  FSal.Push(V2);
  FSal.Push(V1);
end;

procedure TJvSALCore.xTrue;
begin
  FSal.BoolPush(True);
end;

procedure TJvSALCore.xUntil;
begin
  if not FSal.BoolPop then
    FSal.PC := TJvAtom(FSal.Atoms.Objects[FSal.PcProc]).Value;
end;

procedure TJvSALCore.xvAdd; // +=
begin
  if VarIsEmpty(FSal.Variable.Value) then
    FSal.Variable.Value := FSal.Pop
  else
    FSal.Variable.Value := FSal.Variable.Value + FSal.Pop;
end;

procedure TJvSALCore.xvDiv; // /=
var
  V1: Variant;
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  V1 := FSal.Pop;
  if V1 = 0 then
    raise EJVCLException.CreateRes(@RsEDivisionByZeroError);
  FSal.Variable.Value := FSal.Variable.Value / V1;
end;

procedure TJvSALCore.xvMul; // *=
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  FSal.Variable.Value := FSal.Variable.Value * FSal.Pop;
end;

procedure TJvSALCore.xvSub; // -=
begin
  if VarIsEmpty(FSal.Variable.Value) then
    raise EJVCLException.CreateResFmt(@RsEVariablesIsNotInitialized, [FSal.VariableName]);
  FSal.Variable.Value := FSal.Variable.Value - FSal.Pop;
end;

procedure TJvSALCore.xXOr;
begin
  FSal.BoolPush(FSal.BoolPop xor FSal.BoolPop);
end;

procedure TJvSALCore.xpIf;
begin
  FSal.rPush(FSal.APO(FSal.Token, xIf))
end;

procedure TJvSALCore.xpEndCase;
begin
  TJvAtom(FSal.Atoms.Objects[FSal.rPop]).Value := FSal.APO(FSal.Token, xEndCase);
end;

procedure TJvSALCore.xpIfNot;
begin
  FSal.rPush(FSal.APO(FSal.Token, xIfNot));
end;

procedure TJvSALCore.xpEndIf;
begin
  TJvAtom(FSal.Atoms.Objects[FSal.rPop]).Value := FSal.APO(FSal.Token, xEndIf);
end;

procedure TJvSALCore.xpElse;
var
  I: Integer;
begin
  I := FSal.APO(FSal.Token, xElse);
  TJvAtom(FSal.Atoms.Objects[FSal.rPop]).Value := I;
  FSal.rPush(I);
end;

procedure TJvSALCore.xpCase;
begin
  FSal.rPush(FSal.APO(FSal.Token, xCase));
end;

procedure TJvSALCore.xpRepeat;
begin
  FSal.rPush(FSal.APO(FSal.Token, xRepeat))
end;

procedure TJvSALCore.xpUntil;
begin
  TJvAtom(FSal.Atoms.Objects[FSal.APO(FSal.Token, xUntil)]).Value := FSal.rPop;
end;

end.
