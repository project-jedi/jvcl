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
{$I jvcl.inc}
unit main;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvUIB, StdCtrls, JvComponent;

type
  TMainForm = class(TForm)
    Go: TButton;
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Memo: TMemo;
    StoredProc: TJvUIBQuery;
    procedure GoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses JvUIBLib;

{$R *.dfm}

procedure TMainForm.GoClick(Sender: TObject);
var i: Integer;
begin
  Memo.Clear;
  StoredProc.BuildStoredProc('SUB_TOT_BUDGET');
  Memo.Lines.Add(StoredProc.SQL.Text);
  Memo.Lines.Add(StoredProc.Params.FieldName[0] + ': 100');
  Memo.Lines.Add('---');
  StoredProc.Params.ByNameAsString['HEAD_DEPT'] := '100';

  StoredProc.Open;
  with StoredProc.Fields do
  for i := 0 to FieldCount - 1 do
    memo.Lines.Add(AliasName[i] + ': ' + AsString[i]);
  StoredProc.Close(etmCommit);
end;

end.
