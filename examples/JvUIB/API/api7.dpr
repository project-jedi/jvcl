(*
 *  Program type:  API Interface
 *
 *    Description:
 *      This program selects a blob data type.
 *      A set of project descriptions is printed.
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
program api7;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JvUIBase,
  JvUIBLib;

var
  sel_str      : string;
  blob_handle  : IscBlobHandle = nil;
  DB           : IscDbHandle = nil;    // database handle
  trans        : IscTrHandle = nil;    // transaction handle
  stmt         : IscStmtHandle = nil;  // statement handle
  sqlda        : TSQLResult;
  empdb        : string;
  str: string;
begin

  if (ParamCount > 1) then
    empdb := ParamStr(1) else
    empdb := 'D:\Unified Interbase\demo\Database\employee.db';

  sel_str := 'SELECT proj_name, proj_desc, product FROM project WHERE '+
     'product IN ("software", "hardware", "other") ORDER BY proj_name';

  AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

  TransactionStart(trans, DB);

  DSQLAllocateStatement(DB, stmt);

  sqlda := TSQLResult.Create(3);

  DSQLPrepare(trans, stmt, sel_str, 1, sqlda);

  DSQLExecute(trans, stmt, 1);

  (*
   *    For each project in the select statement, get and display
   *    project descriptions.
   *)

  while  DSQLFetch(stmt, 1, sqlda) do //sqlda
  begin
    writeln(format('PROJECT:  %s   TYPE:  %s', [sqlda.AsString[0], sqlda.AsString[2]]));
    BlobOpen(DB, trans, blob_handle, sqlda.AsQuad[1]);
    str := BlobReadString(blob_handle);
    WriteLn(str);
    BlobClose(blob_handle);
  end;
  DSQLFreeStatement(stmt, DSQL_close);
  TransactionCommit(trans);
  DetachDatabase(DB);
  sqlda.Free;

  readln;
end.
