{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvSALCore.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit jvSALCore;

// SAL Core package
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, jvSAL,
  math{$IFDEF DELPHI6_UP}, Variants{$ENDIF};

type
  TJvSALCore = class(TComponent)
  private
    { Private declarations }
    sal: TJvSAL;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure AddProcedures(aSal: TJvSAL);
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
    procedure xvAdd; // directly add to variable
    procedure xvSub;
    procedure xvMul;
    procedure xvDiv;
    procedure xdec;
    procedure xinc;
    procedure xdeczero;
    procedure xCR;
    procedure xdup;
    procedure xdrop;
    procedure xswap;
    procedure xcap;

  published
    { Published declarations }
  end;

implementation

const
  cr = chr(13) + chr(10);
  tab = chr(9);

  { TJvSALCoreBasic }

procedure TJvSALCore.AddProcedures(aSal: TJvSAL);
begin
  sal := aSal;
  sal.AddProcedure('if', xif, xpif);
  sal.AddProcedure('ifnot', xifnot, xpifnot);
  sal.AddProcedure('else', xelse, xpelse);
  sal.AddProcedure('endif', xendif, xpendif);
  sal.AddProcedure('repeat', xrepeat, xprepeat);
  sal.AddProcedure('until', xuntil, xpuntil);
  sal.AddProcedure('select', xselect, nil);
  sal.AddProcedure('endselect', xendselect, nil);
  sal.AddProcedure('case', xcase, xpcase);
  sal.AddProcedure('endcase', xendcase, xpendcase);
  sal.AddProcedure('exit', xexit, nil);
  sal.AddProcedure('get', xget, nil);
  sal.AddProcedure('set', xset, nil);
  sal.AddProcedure('ask', xask, nil);
  sal.AddProcedure('say', xsay, nil);
  sal.AddProcedure('true', xtrue, nil);
  sal.AddProcedure('false', xfalse, nil);
  sal.AddProcedure('and', xand, nil);
  sal.AddProcedure('or', x_or, nil);
  sal.AddProcedure('xor', xxor, nil);
  sal.AddProcedure('not', xnot, nil);
  sal.AddProcedure('=', xeq, nil);
  sal.AddProcedure('<>', xne, nil);
  sal.AddProcedure('>=', xge, nil);
  sal.AddProcedure('<=', xle, nil);
  sal.AddProcedure('>', xgt, nil);
  sal.AddProcedure('<', xlt, nil);
  sal.AddProcedure('neg', xneg, nil);
  sal.AddProcedure('abs', xabs, nil);
  sal.AddProcedure('+', xadd, nil);
  sal.AddProcedure('-', xsub, nil);
  sal.AddProcedure('*', xmul, nil);
  sal.AddProcedure('/', xdiv, nil);
  sal.AddProcedure('+=', xvadd, nil);
  sal.AddProcedure('-=', xvsub, nil);
  sal.AddProcedure('*=', xvmul, nil);
  sal.AddProcedure('/=', xvdiv, nil);
  sal.AddProcedure('dec', xdec, nil);
  sal.AddProcedure('inc', xinc, nil);
  sal.AddProcedure('dec?', xdeczero, nil);
  sal.AddProcedure('cr', xcr, nil);
  sal.AddProcedure('dup', xdup, nil);
  sal.AddProcedure('drop', xdrop, nil);
  sal.AddProcedure('swap', xswap, nil);
  sal.AddProcedure('cap', xcap, nil);
end;

procedure TJvSALCore.X_Or;
begin
  sal.boolpush(sal.boolpop or sal.boolpop);
end;

procedure TJvSALCore.xAbs;
begin
  sal.push(abs(sal.pop));
end;

procedure TJvSALCore.xAdd;
var
  v1, v2: variant;
begin
  v2 := sal.pop;
  v1 := sal.pop;
  sal.push(v1 + v2);
end;

procedure TJvSALCore.xAnd;
begin
  sal.boolpush(sal.boolpop and sal.boolpop);
end;

procedure TJvSALCore.xAsk;
var
  s: string;
  v: variant;
begin
  s := sal.pop;
  v := inputbox(sal.Caption, s, '');
  if v = '' then exit;
  sal.push(v);
end;

procedure TJvSALCore.xcap;
begin
  sal.Caption := sal.pop;
end;

procedure TJvSALCore.xCase;
var
  v1: variant;
begin
  v1 := sal.pop;
  if v1 = sal.theSelect then
  begin
  end
  else
  begin
    sal.pc := TJvAtom(sal.atoms.objects[sal.pcproc]).value + 1;
  end;
end;

procedure TJvSALCore.xCR;
begin
  sal.push(cr);
end;

procedure TJvSALCore.xdec;
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  sal.variable.Value := sal.variable.value - 1;
end;

procedure TJvSALCore.xdeczero; // dec?  decrements a variable and test for zero
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  sal.variable.Value := sal.variable.value - 1;
  if sal.variable.value = 0 then
    sal.boolpush(true)
  else
    sal.boolpush(false);
end;

procedure TJvSALCore.xDiv;
var
  v1, v2: double;
begin
  v2 := sal.pop;
  if v2 = 0 then
    raise exception.create('division by zero error');
  v1 := sal.pop;
  sal.push(v1 / v2);
end;

procedure TJvSALCore.xdrop;
begin
  sal.pop;
end;

procedure TJvSALCore.xdup;
var
  v1: variant;
begin
  v1 := sal.pop;
  sal.push(v1);
  sal.push(v1);
end;

procedure TJvSALCore.xElse;
begin
  sal.pc := TJvAtom(sal.atoms.objects[sal.pcproc]).value + 1;
end;

procedure TJvSALCore.xEndCase;
// Removed Hint
//var
//  c:integer;
begin
  //  c:=sal.atoms.count;
  while sal.pc < sal.atoms.count do
  begin
    if sal.atoms[sal.pc] = 'endselect' then
    begin
      sal.pc := sal.pc + 1;
      exit;
    end;
    sal.pc := sal.pc + 1;
  end;
  raise exception.create('missing "endselect"');
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
  sal.boolpush(sal.pop = sal.pop);
end;

procedure TJvSALCore.xExit;
begin
  sal.pc := sal.atoms.Count;
end;

procedure TJvSALCore.xFalse;
begin
  sal.boolpush(false);
end;

procedure TJvSALCore.xGe;
begin
  sal.boolpush(sal.pop >= sal.pop);
end;

procedure TJvSALCore.xGet;
begin
  sal.push(sal.variable.Value);
end;

procedure TJvSALCore.xGt;
begin
  sal.boolpush(sal.pop > sal.pop);
end;

procedure TJvSALCore.xIf;
begin
  if not sal.boolpop then
  begin
    sal.pc := TJvAtom(sal.atoms.objects[sal.pcproc]).value + 1;
  end
end;

procedure TJvSALCore.xIfNot;
begin
  if sal.boolpop then
  begin
    sal.pc := TJvAtom(sal.atoms.objects[sal.pcproc]).value + 1;
  end
end;

procedure TJvSALCore.xinc;
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  sal.variable.Value := sal.variable.value + 1;
end;

procedure TJvSALCore.xLe;
begin
  sal.boolpush(sal.pop <= sal.pop);
end;

procedure TJvSALCore.xLt;
begin
  sal.boolpush(sal.pop < sal.pop);
end;

procedure TJvSALCore.xMul;
var
  v1, v2: double;
begin
  v2 := sal.pop;
  v1 := sal.pop;
  sal.push(v1 * v2);
end;

procedure TJvSALCore.xNe;
begin
  sal.boolpush(sal.pop <> sal.pop);
end;

procedure TJvSALCore.xNeg;
begin
  sal.push(0 - sal.pop);
end;

procedure TJvSALCore.xNot;
begin
  sal.boolpush(not sal.boolpop);
end;

procedure TJvSALCore.xRepeat;
begin
  // do nothing
end;

procedure TJvSALCore.xSay;
begin
  showmessage(sal.pop);
end;

procedure TJvSALCore.xSelect;
begin
  sal.theSelect := sal.pop;
end;

procedure TJvSALCore.xSet;
begin
  sal.Variable.Value := sal.pop;
end;

procedure TJvSALCore.xSub;
var
  v1, v2: double;
begin
  v2 := sal.pop;
  v1 := sal.pop;
  sal.push(v1 - v2);
end;

procedure TJvSALCore.xswap;
var
  v1, v2: variant;
begin
  v2 := sal.pop;
  v1 := sal.pop;
  sal.push(v2);
  sal.push(v1);
end;

procedure TJvSALCore.xTrue;
begin
  sal.boolpush(true);
end;

procedure TJvSALCore.xUntil;
begin
  if not sal.boolpop then
    sal.pc := TJvAtom(sal.atoms.objects[sal.pcproc]).value;
end;

procedure TJvSALCore.xvAdd; // +=
begin
  if VarIsEmpty(sal.variable.value) then
    sal.variable.Value := sal.pop
  else
    sal.variable.Value := sal.variable.value + sal.pop;
end;

procedure TJvSALCore.xvDiv; // /=
var
  v1: variant;
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  v1 := sal.pop;
  if v1 = 0 then
    raise exception.create('division by zero error');
  sal.variable.Value := sal.variable.value / v1;
end;

procedure TJvSALCore.xvMul; // *=
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  sal.variable.Value := sal.variable.value * sal.pop;
end;

procedure TJvSALCore.xvSub; // -=
begin
  if VarIsEmpty(sal.variable.value) then
    raise exception.Create('variable ' + sal.variablename + ' is not initialized');
  sal.variable.Value := sal.variable.value - sal.pop;
end;

procedure TJvSALCore.xXOr;
begin
  sal.boolpush(sal.boolpop xor sal.boolpop);
end;

procedure TJvSALCore.xpIf;
begin
  sal.rPush(sal.apo(sal.token, xif))
end;

procedure TJvSALCore.xpEndCase;
begin
  TJvAtom(sal.atoms.objects[sal.rpop]).value := sal.apo(sal.token, xendcase);
end;

procedure TJvSALCore.xpIfNot;
begin
  sal.rpush(sal.apo(sal.token, xifnot));
end;

procedure TJvSALCore.xpEndIf;
begin
  TJvAtom(sal.atoms.objects[sal.rpop]).value := sal.apo(sal.token, xendif);
end;

procedure TJvSALCore.xpElse;
var
  i: integer;
begin
  i := sal.apo(sal.token, xelse);
  TJvAtom(sal.atoms.objects[sal.rpop]).value := i;
  sal.rpush(i);
end;

procedure TJvSALCore.xpCase;
begin
  sal.rpush(sal.apo(sal.token, xcase));
end;

procedure TJvSALCore.xpRepeat;
begin
  sal.rpush(sal.apo(sal.token, xrepeat))
end;

procedure TJvSALCore.xpUntil;
begin
  TJvAtom(sal.atoms.objects[sal.apo(sal.token, xuntil)]).value := sal.rpop;
end;

end.
