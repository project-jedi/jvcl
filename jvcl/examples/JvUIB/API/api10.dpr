(*
 *    Program type:  API
 *
 *    Description:
 *        This program selects and updates an array type.
 *        Projected head count is displayed and updated for
 *        a set of projects.
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

program api10;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

const
  sel_str =
    'SELECT dept_no, quart_head_cnt FROM proj_dept_budget p ' +
    'WHERE fiscal_year = 1994 AND proj_id = ''VBASE'' ' +
    'FOR UPDATE of quart_head_cnt';

  upd_str =
    'UPDATE proj_dept_budget SET quart_head_cnt = ? WHERE CURRENT OF S';

var
  hcnt: array[0..3] of Integer;
  desc: TArrayDesc;
  len: Integer;
  DB: IscDbHandle = nil;
  trans: IscTrHandle = nil;
  stmt: IscStmtHandle = nil;
  ustmt: IscStmtHandle = nil;
  cursor: string = 'S';
  osqlda: TSQLResult;
  isqlda: TSQLParams;
  i: Smallint;
  empdb: string;
  FLibrary: TUIBLibrary;
  AQuad: GDS_QUAD;

begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      empdb := ParamStr(1) else
   // empdb := 'D:\Unified Interbase\demo\Database\employee.db';
      empdb := 'D:\employee.db';

    FLibrary.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    FLibrary.TransactionStart(trans, DB);

    // Set up the array description structure

    desc := FLibrary.ArrayLookupBounds(DB, trans, 'PROJ_DEPT_BUDGET', 'QUART_HEAD_CNT');

    // Set-up the select statement.

    osqlda := TSQLResult.Create(2);

    FLibrary.DSQLAllocateStatement(DB, stmt);
    FLibrary.DSQLAllocateStatement(DB, ustmt);

    (* Prepare and execute query *)
    FLibrary.DSQLPrepare(trans, stmt, sel_str, 1, osqlda);
    FLibrary.DSQLExecute(trans, stmt, 1);

    (* Needed for update current *)
    FLibrary.DSQLSetCursorName(stmt, cursor);

    // Set-up the update statement.

    isqlda := TSQLParams.Create;

    (* Use describe_bind to set up input sqlda *)

    FLibrary.DSQLPrepare(trans, ustmt, upd_str, 1);

//!!!    isqlda.AddArray;
    isqlda.AddFieldType('0', uftQuad);

    FLibrary.DSQLDescribeBind(ustmt, 1, isqlda);

    (*
     *    Fetch the head count for each department's 4 quarters;
     *    increase the head count by 1 for each quarter;
     *    and save the new head count.
     *)

    while FLibrary.DSQLFetch(stmt, 1, osqlda) do
    begin
        (* Get the current array values. *)
      if not osqlda.IsNull[1] then
      begin

        len := sizeof(hcnt);
        FLibrary.ArrayGetSlice(DB, trans, osqlda.AsQuad[1], desc, @hcnt, len);
            //dept_no [osqlda->sqlvar[0].sqllen] = '\0';
        writeln(format('Department #:  %s', [osqlda.AsString[0]]));

        writeln(format('Current counts: %d %d %d %d', [hcnt[0], hcnt[1], hcnt[2], hcnt[3]]));

            (* Add 1 to each count. *)
        for i := 0 to 3 do
          hcnt[i] := hcnt[i] + 1;

        isqlda.AsQuad[0] := osqlda.AsQuad[1];

       (* Save new array values. *)
        //!!!
        AQuad := isqlda.AsQuad[0];
        FLibrary.ArrayPutSlice(DB, trans, AQuad, desc, @hcnt, len);



            (* Update the array handle. *)
        FLibrary.DSQLExecute(trans, ustmt, 1, isqlda);

        writeln(format('New counts    : %d %d %d %d', [hcnt[0], hcnt[1], hcnt[2], hcnt[3]]));
      end;
    end;

    FLibrary.DSQLFreeStatement(stmt, DSQL_close);
    FLibrary.DSQLFreeStatement(ustmt, 0);

    (* Do a rollback to keep from updating the sample db *)

    FLibrary.TransactionRollback(trans);
    //TransactionCommit(trans);

    FLibrary.DetachDatabase(DB);

    osqlda.Free;
    isqlda.Free;

    Readln;
  finally
    FLibrary.Free;
  end;

end.

