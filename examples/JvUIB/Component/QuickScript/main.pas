{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

(*******************************************************************************
*  The TJvUIBQuery.QuickScript property is a way to execute more than one Update
*  SQL without trying to parse your script. If you uses the quickScript property
*  you must have only one query per line.
*******************************************************************************)

unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvComponent;

type
  TMainForm = class(TForm)
    Query: TJvUIBQuery;
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    btExecute: TButton;
    procedure btExecuteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btExecuteClick(Sender: TObject);
begin
  Query.SQL.Clear;
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test0'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test1'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test2'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test3'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test4'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test5'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test6'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test7'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test8'',''FFranc'')');
  Query.SQL.Add('INSERT INTO COUNTRY (COUNTRY,CURRENCY) VALUES (''Test9'',''FFranc'')');

  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test0''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test1''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test2''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test3''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test4''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test5''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test6''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test7''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test8''');
  Query.SQL.Add('DELETE FROM COUNTRY WHERE COUNTRY = ''Test9''');
  Query.ExecSQL;
  Query.Close(etmCommit);
end;

end.
