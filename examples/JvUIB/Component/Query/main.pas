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

unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
   SysUtils, Classes, JvUIB, JvComponent;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Button1: TButton;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Query.Params.ByNameAsInteger['Dept'] := 623;
  Query.Open;
  memo.Clear;
  while not Query.EOF do
    with Query, Fields do
    begin
      memo.Lines.Add(format('%s %s, Salary: %f',
        [ByNameAsString['FIRST_NAME'],
         ByNameAsString['LAST_NAME'],
         ByNameAsCurrency['SALARY']]));
      Next;
    end;
  memo.Lines.Add(Query.Plan);  
  Query.Close(etmCommit);

end;

end.
