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
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls,
{$ENDIF}
  SysUtils, Classes, JvUIB, JvComponent;

type
  TForm1 = class(TForm)
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    Button1: TButton;
    Memo: TMemo;
    Button2: TButton;
    UpdateQuery: TJvUIBQuery;
    Description: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var str: String;
begin
  Memo.Clear;
  Query.Open;
  while not Query.EOF do
  begin
    Memo.Lines.Add('Project Name: ' + Query.Fields.ByNameAsString['proj_name']);
    Memo.Lines.Add('Product: ' + Query.Fields.ByNameAsString['product']);
    Query.ReadBlob('proj_desc', str);

    Memo.Lines.Add('Description: ' + Str);
    Memo.Lines.Add('');
    Query.Next;
  end;
  Query.Close(etmCommit);
end;

procedure TForm1.Button2Click(Sender: TObject);
var Str: String;
begin
  Str := Description.Text;
  UpdateQuery.ParamsSetBlob('description', Str);
  UpdateQuery.ExecSQL;
  UpdateQuery.Close(etmCommit);

end;

end.
