(*
 *  Program type:  API Interface
 *
 *  Description:
 *        This program adds several departments with small default
 *        budgets, using 'execute immediate' statement.
 *        Then, a prepared statement, which doubles budgets for
 *        departments with low budgets, is executed.
 * The contents of this file are subject to the Interbase Public
 * License Version 1.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy
 * of the License at:
 * http://www.borland.com/devsupport/interbase/opensource/IPL.html
 *
 * Software distributed under the License is distributed on an
 * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express
 * or implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code was created by Borland Software Corporation
 * and its predecessors.  Portions created by Borland are Copyright (c)
 * 1994 - 2001 Borland Software Corporation.
 * All rights reserved.
 *
 * Contributor(s):
 *                 Aaron Ruddick InterBase QA, Borland Software Corp.
 *                 Dan Mikhayltsa  InterBase QA, Borland Software Corp.
 *)
program api2;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

var
  DB: IscDbHandle = nil; // database handle
  trans: IscTrHandle = nil; // transaction handle
  Db_name: string;

  n: Integer = 0;
  exec_str: string;
  prep_str: string;
  double_budget: IscStmtHandle = nil; // statement handle
  FLibrary: TUIBLibrary;

(*
 *  Delete old data.
 *)

procedure Cleanup;
begin
  FLibrary.TransactionStart(trans, DB);
  FLibrary.DSQLExecuteImmediate(DB, trans, 'DELETE FROM department WHERE dept_no IN ("117", "118", "119")', 1, nil);
  FLibrary.TransactionCommit(trans);
end;

(*
 *  Construct an 'insert' statement from the supplied parameters.
 *)

function getline(out line: string; Number: Integer): boolean;
type
  TDept = record
    Num: byte;
    Dep: string;
    Head: Byte;
  end;
const
  Data: array[0..2] of TDept =
  ((Num: 117; Dep: 'Field Office: Hong Kong'; Head: 110),
    (Num: 118; Dep: 'Field Office: Australia'; Head: 110),
    (Num: 119; Dep: 'Field Office: New Zealand'; Head: 110));
begin
  result := false;
  if not (Number in [0..2]) then Exit;

  with Data[Number] do
    line := Format('INSERT INTO DEPARTMENT (dept_no, department, head_dept) VALUES ("%d", "%s", "%d")', [Num, Dep, Head]);
  result := True;
end;


begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      Db_name := ParamStr(1) else
      Db_name := 'D:\Unified Interbase\demo\Database\employee.db';

    FLibrary.AttachDatabase(Db_name, DB, 'user_name=SYSDBA;password=masterkey');

    Cleanup;

  (*
   *  Prepare a statement, which may be executed more than once.
   *)

    prep_str := 'UPDATE DEPARTMENT SET budget = budget * 2 WHERE budget < 100000';

  (* Allocate a statement. *)
    FLibrary.DSQLAllocateStatement(DB, double_budget);

    FLibrary.TransactionStart(trans, DB);

  (* Prepare the statement. *)
    FLibrary.DSQLPrepare(trans, double_budget, prep_str, 1, nil);

  (*
   *  Add new departments, using 'execute immediate'.
   *  Build each 'insert' statement, using the supplied parameters.
   *  Since these statements will not be needed after they are executed,
   *  use 'execute immediate'.
   *)

    while (getline(exec_str, n)) do
    begin
      Writeln(format('Executing statement:'#13'%d: %s;', [n, exec_str]));
      FLibrary.DSQLExecuteImmediate(DB, trans, exec_str, 1, nil);
      inc(n);
    end;

  (*
   *    Execute a previously prepared statement.
   *)

    Writeln('Executing a prepared statement:'#13 + prep_str);

    FLibrary.DSQLExecute(trans, double_budget, 1);
    FLibrary.DSQLFreeStatement(double_budget, DSQL_drop);
    FLibrary.TransactionCommit(trans);
    FLibrary.DetachDatabase(DB);
    ReadLn;
  finally
    FLibrary.Free;
  end;
end.

