(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program performs a positioned update.
 *        Department budgets are examined and updated using some
 *        percent increase factor, determined at run-time.
 *
 *        The update statement is constructed using a dynamic cursor
 *        name.  The statement handle is freed and re-used by the
 *        update cursor, after being used by another statement.
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
program api6;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

const Dialect = 3;

(*
 *    Determine a percent increase for the department's budget.
 *)

function increase_factor(budget: Double): single;
begin
  if (budget < 100000) then result := 0.15 else
    if (budget < 500000) then result := 0.10 else
      result := 0.5;
end;

const
  DEPTLEN = 3;
  PROJLEN = 5;
  BUFLEN = 256;

//float increase_factor (double budget);

(*
 *  A cursor is declared on this select statement, allowing for
 *  the update of projected_budget field.
 *)
  sel_str = 'SELECT proj_id, dept_no, projected_budget ' +
    'FROM proj_dept_budget WHERE fiscal_year = 1994 ' +
    'FOR UPDATE OF projected_budget;';

(* This query is executed prior to the positioned update. *)
  tot_str =
    'SELECT SUM(projected_budget) FROM proj_dept_budget WHERE fiscal_year = 1994';

var
  upd_str: string;
  budget: double;
  DB: IscDbHandle = nil; // Database handle
  trans: IscTrHandle = nil; // transaction handle
  cursor: string = 'budget'; // dynamic cursor name
  stmt: IscStmtHandle = nil; // statement handle
  isqlda: TSQLParams;
  osqlda: TSQLResult;
  empdb: string;
  FLibrary: TUIBLibrary;
begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      empdb := ParamStr(1) else
      empdb := 'D:\Unified Interbase\demo\Database\employee.db';

    FLibrary.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

  (*
   *    Prepare and execute the first select statement.
   *    Free the statement handle, when done.
   *)

    FLibrary.DSQLAllocateStatement(DB, stmt);
    osqlda := TSQLResult.Create;
    try
      FLibrary.TransactionStart(trans, db);

      FLibrary.DSQLPrepare(trans, stmt, tot_str, Dialect, osqlda);

      FLibrary.DSQLExecute(trans, stmt, Dialect);

      FLibrary.DSQLFetch(stmt, Dialect, osqlda);

      Writeln(format('Total budget:  %16.2f', [osqlda.AsDouble[0]]));

      FLibrary.DSQLFreeStatement(stmt, DSQL_close);

      FLibrary.TransactionCommit(trans);

    (*
     *    Prepare and execute the positioned update.
     *    Re-use the statement handle as the select cursor.
     *)

      upd_str := format('UPDATE proj_dept_budget SET projected_budget = ? WHERE CURRENT OF %s', [cursor]);

    (* Allocate an input SQLDA for the update statement. *)
      isqlda := TSQLParams.Create;
      try
        isqlda.AddFieldType('0', uftFloat);
//!!!        isqlda.AddDouble;

        FLibrary.TransactionStart(trans, DB);
      (* Zero the statement handle. *)
        stmt := nil;

        FLibrary.DSQLAllocateStatement(DB, stmt);
        FLibrary.DSQLPrepare(trans, stmt, sel_str, Dialect, osqlda);

      (* Declare the cursor. *)

        FLibrary.DSQLSetCursorName(stmt, cursor);

        FLibrary.DSQLExecute(trans, stmt, Dialect);

        writeln(format('%-15s%-10s%-18s%-18s', ['PROJ', 'DEPT', ' CURRENT BUDGET', '  CHANGED TO']));

      (*
       *    Fetch and update department budgets.
       *)

        while FLibrary.DSQLFetch(stmt, Dialect, osqlda) do
        begin
          (* Determine the increase percentage. *)
          budget := osqlda.AsDouble[2];

          write(format('%-15s%-10s%15.2f', [osqlda.AsString[0], osqlda.AsString[1], budget]));
          budget := budget + budget * increase_factor(budget);
          isqlda.AsDouble[0] := budget;
          writeLN(format('%15.2f'#13, [budget]));

        (* Increase the budget. *)
          try
            FLibrary.DSQLExecImmed2(DB, trans, upd_str, Dialect, isqlda, nil);
          except
            on E: EUIBError do
            begin
              if (E.SQLCode = -625) then
              begin
                writeln('Exceeded budget limit -- not updated.');
                continue;
              end else
                raise;
            end;
          end;
        end;
        FLibrary.DSQLFreeStatement(stmt, DSQL_close);
        FLibrary.TransactionRollback(trans);
        FLibrary.DetachDatabase(DB);
      finally
        isqlda.Free;
      end;
    finally
      osqlda.Free;
    end;
    readln;
  finally
    FLibrary.Free;
  end;
end.

