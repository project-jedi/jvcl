(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program displays employee names and phone extensions.
 *
 *        It allocates an output SQLDA, prepares and executes a statement,
 *        and loops fetching multiple rows.
 *
 *        The SQLCODE returned by fetch is checked.
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

program api3;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

(* This macro is used to declare structures representing SQL VARCHAR types *)
//function SQL_VARCHAR(len) struct {short vary_length; char vary_string[(len)+1];}


var
  stmt: IscStmtHandle = nil; // statement handle
  DB: IscDbHandle = nil; // database handle
  trans: IscTrHandle = nil; // transaction handle
  sqlda: TSQLResult;
  empdb: string;
  sel_str: string =
  'SELECT last_name, first_name, phone_ext FROM phone_list WHERE location = "Monterey" ORDER BY last_name, first_name;';
  FLibrary: TUIBLibrary;
begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      empdb := paramstr(1) else
      empdb := 'D:\Unified Interbase\demo\Database\employee.db';

    FLibrary.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    FLibrary.TransactionStart(trans, DB);

    (* Allocate an output SQLDA. *)
    sqlda := TSQLResult.Create(3);

    (* Allocate a statement. *)
    FLibrary.DSQLAllocateStatement(DB, stmt);

    (* Prepare the statement. *)
    FLibrary.DSQLPrepare(trans, stmt, sel_str, 1, sqlda);

    (* Execute the statement. *)
    FLibrary.DSQLExecute(trans, stmt, 1, nil);

    (*
     *    Fetch and print the records.
     *)
    while FLibrary.DSQLFetch(stmt, 1, sqlda) do
      Writeln(format('%s %s %s', [sqlda.AsString[0], sqlda.AsString[1], sqlda.AsString[2]]));

    (* Free statement handle. *)
    FLibrary.DSQLFreeStatement(stmt, DSQL_close);
    FLibrary.TransactionCommit(trans);
    FLibrary.DetachDatabase(DB);
    sqlda.Free;
    Readln;
  finally
    FLibrary.Free;
  end;
end.

