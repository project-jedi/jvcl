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
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    Query: TJvUIBQuery;
    go: TButton;
    procedure goClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TARecord = record
    COUNTRY: string;
    CURRENCY: string;
  end;



var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.goClick(Sender: TObject);
var
  i: Integer;
const
  Datas : array[1..10] of TARecord = (
    (COUNTRY: 'blabla0'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla1'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla2'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla3'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla4'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla5'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla6'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla7'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla8'; CURRENCY: 'blabla'),
    (COUNTRY: 'blabla9'; CURRENCY: 'blabla'));
begin
  for i := 1 to 10 do
  begin
    Query.Params.AsString[0] := Datas[i].COUNTRY;
    Query.Params.AsString[1] := Datas[i].CURRENCY;
    Query.Execute;
    // for better performance commit every 1000 records
    // Transaction.Commit;
  end;
  Query.Close(etmRollback); // change to etmCommit to apply.
end;

end.
