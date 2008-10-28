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

unit salary;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TSalary = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Salary: TSalary;

implementation

uses qr_salary;

{$R *.DFM}

var
  Form: TForm;

procedure TSalary.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Add('up to 20000');
  ComboBox1.Items.Add('20000 .. 40000');
  ComboBox1.Items.Add('40000 and over');
  ComboBox1.ItemIndex := 0;
end;

procedure SetParams(Index:integer);
begin
   // Form.Query1.ParamByName('DatBeg').Value := StrToDate('01.01.98');
   // Form.Query1.ParamByName('DatEnd').Value := StrToDate('01.01.99');
  if Index = 0 then
  begin
  	Form.Query1.SQL.Text :=
  		'select * from employee '+
  		'where salary <= 20000';
    Form.lblTitle.Caption := 'Salary up to 20000';
  end
  else
  if Index = 1 then
  begin
  	Form.Query1.SQL.Text :=
  		'select * from employee '+
  		'where 20000 < salary and salary <= 40000';
    Form.lblTitle.Caption := 'Salary 20000 .. 40000';
  end
  else
  if Index = 2 then
  begin
  	Form.Query1.SQL.Text :=
  		'select * from employee '+
  		'where salary > 40000';
    Form.lblTitle.Caption := 'Salary over 40000';
  end;
end;

procedure TSalary.Button1Click(Sender: TObject);
begin
 { Form := JvInterpreterMakeForm(ExtractFilePath(Application.ExeName) +
    'Reports\qr_salary'); }
  Form := TrSalary.Create(Application);
  try
   // Self.Hide;
    SetParams(ComboBox1.ItemIndex);
    Form.Query1.Open;
    Form.QuickRep1.Preview;
    Close;
  finally { wrap up }
    Form.Free;
  end;    { try/finally }
end;



{
/*
select Name, Descript, Kod from template
where :DatBeg <= Dat and Dat <= :DatEnd
*/
}
end.
