(*
 *    Program type:  API Interface
 *
 *    Description:
 *        This program updates a blob data type.
 *        Project descriptions are added for a set of projects.
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

program api8;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  JvUIBase,
  JvUIBLib;

const
  Proj_data: array[0..21] of string =
  ('VBASE',
    'Design a video data base management system for ',
    'controlling on-demand video distribution.',
    '',
    'DGPII',
    'Develop second generation digital pizza maker ',
    'with flash-bake heating element and ',
    'digital ingredient measuring system.',
    '',
    'GUIDE',
    'Develop a prototype for the automobile version of ',
    'the hand-held map browsing device.',
    '',
    'MAPDB',
    'Port the map browsing database software to run ',
    'on the automobile model.',
    '',
    'HWRII',
    'Integrate the hand-writing recognition module into the ',
    'universal language translator.',
    '',
    '');

var
  Inp_ptr: Integer = 0;

function get_line: string;
begin
  result := Proj_data[Inp_ptr];
  inc(Inp_ptr);
end;

var
  upd_stmt: string;
  blob_handle: IscBlobHandle = nil;
  DB: IscDbHandle = nil; (* database handle *)
  trans: IscTrHandle = nil; (* transaction handle *)
  sqlda: TSQLParams;
  line: string;
  rec_cnt: Integer = 0;
  empdb: string;
  FLibrary: TUIBLibrary;

begin
  FLibrary := TUIBLibrary.Create;
  try
    if (ParamCount > 1) then
      empdb := ParamStr(1) else
      empdb := 'D:\Unified Interbase\demo\Database\employee.db';

    upd_stmt := 'UPDATE project SET proj_desc = ? WHERE proj_id = ?';

    FLibrary.AttachDatabase(empdb, DB, 'user_name=SYSDBA;password=masterkey');

    //  Set-up the SQLDA for the update statement.

    sqlda := TSQLParams.Create;
    {.$WARNING "Not supported anymore!"}
    sqlda.AddFieldType('1',uftBlob);
    sqlda.AddFieldType('2',uftVarChar);
//!!!    sqlda.AddBlob;
//!!!    sqlda.AddString;

    FLibrary.TransactionStart(trans, DB);

    // Get the next project id and update the project description.

    line := get_line;
    while (line <> '') do
    begin
      sqlda.AsString[1] := line;
      writeln(format('Updating description for project:  %s', [line]));

      blob_handle := nil;
      sqlda.AsQuad[0] := FLibrary.BlobCreate(DB, trans, blob_handle);

      line := get_line;
      while (line <> '') do
      begin
        writeln(format('  Inserting segment:  %s', [line]));
        FLibrary.BlobWriteString(blob_handle, line);
        line := get_line;
      end;
      FLibrary.BlobClose(blob_handle);
      FLibrary.DSQLExecuteImmediate(DB, trans, upd_stmt, 1, sqlda);
      inc(rec_cnt);
      line := get_line;
    end;

    FLibrary.TransactionRollback(trans); // change to TransactionCommit to apply updates
    writeln(format('Added %d project descriptions.', [rec_cnt]));

    FLibrary.DetachDatabase(DB);
    sqlda.Free;

    readln;
  finally
    FLibrary.Free;
  end;
end.

