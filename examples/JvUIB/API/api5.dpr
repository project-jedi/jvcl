(*
 *    Program type:  API Interface
 *
 *    Desription:
 *        This program demonstrates the reallocation of SQLDA
 *        and the 'isc_dsql_describe' statement.  After a query
 *        is examined with 'isc_dsql_describe', an SQLDA of correct
 *        size is reallocated, and some information is printed about
 *        the query:  its type (select, non-select), the number
 *        of columns, etc.
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

program api5;
uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

{$APPTYPE CONSOLE}

const
  sel_str = 'SELECT department, mngr_no, location, head_dept ' +
    'FROM department WHERE head_dept in ("100", "900", "600")';

var
  DB: IscDbHandle = nil; // database handle
  trans: IscTrHandle = nil; // transaction handle

  i: integer;
  stmt: IscStmtHandle = nil;
  sqlda: TSQLResult;
  empdb: string;
  FLibrary: TUIBLibrary;
begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      empdb := ParamStr(1) else
      empdb := 'D:\Unified Interbase\demo\Database\employee.db';

    FLibrary.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

  (* Allocate SQLDA of an arbitrary size. *)
    sqlda := TSQLResult.Create;
    try
      FLibrary.TransactionStart(trans, DB);

    (* Allocate a statement. *)
      FLibrary.DSQLAllocateStatement(DB, stmt);

    (* Prepare the statement. *)
      FLibrary.DSQLPrepare(trans, stmt, sel_str, 1, sqlda); // automaticaly change sqlda size  and describe if necessary

    (* This is a select statement, print more information about it. *)

      writeln('Query Type:  SELECT');

      writeln(format('Number of columns selected:  %d', [sqlda.FieldCount]));

    (* List column names, types, and lengths. *)
      for i := 0 to sqlda.FieldCount - 1 do
      begin
        writeln(format('Column SQL name  : %s', [sqlda.sqlname[i]]));
        writeln(format('Column Alias name: %s', [sqlda.AliasName[i]]));
        writeln(format('Column Rel name  : %s', [sqlda.RelName[i]]));
        writeln(format('Column Own name  : %s', [sqlda.OwnName[i]]));
        writeln(format('Column type      : %d', [sqlda.sqltype[i]]));
        writeln(format('Column length    : %d', [sqlda.sqllen[i]]));
        Writeln('');
      end;

      FLibrary.DSQLFreeStatement(stmt, DSQL_drop);

      FLibrary.TransactionCommit(trans);
      FLibrary.DetachDatabase(DB);
    finally
      sqlda.free;
    end;
    readln;
  finally
    FLibrary.Free;
  end;
end.

