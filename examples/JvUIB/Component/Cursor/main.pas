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

// This sample demonstrate the use of a cursor
// Note:
//   SelectQuery.UseCursor := True
//   UpdateQuery.OnError   := etmStayIn
// With Firebird 1.5 you can write 'for update of xxx with lock'. 

interface

uses
  Windows, Messages, SysUtils, {$IFDEF COMPILER6_UP}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, JvUIB, JvUIBLib, StdCtrls, JvComponent;

type
  TMainForm = class(TForm)
    DataBase: TJvUIBDataBase;
    Transaction: TJvUIBTransaction;
    SelectQuery: TJvUIBQuery;
    UpdateQuery: TJvUIBQuery;
    Update: TButton;
    Log: TMemo;
    procedure UpdateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.UpdateClick(Sender: TObject);
  function IncreaseFactor(const budget: Double): single;
  begin
    if (budget < 100000) then result := 0.15 else
    if (budget < 500000) then result := 0.10 else
      result := 0.5;
  end;
var
  budget: double;
begin
  Log.Clear;
  Log.Lines.Add(format('%-15s%-10s%-18s%-18s',['PROJ', 'DEPT', ' CURRENT BUDGET',  '  CHANGED TO']));
  SelectQuery.Open;
  UpdateQuery.SQL.Text :=
    format('UPDATE proj_dept_budget SET projected_budget = ? WHERE CURRENT OF %s',
    [SelectQuery.CursorName]);
  while not SelectQuery.Eof do
  begin
    Budget := SelectQuery.Fields.AsDouble[2];
    UpdateQuery.Params.AsDouble[0] := Budget + Budget * IncreaseFactor(Budget);
    Log.Lines.Add(format('%-15s%-10s%15.2f%15.2f',
      [SelectQuery.Fields.AsString[0], SelectQuery.Fields.AsString[1], budget,
       UpdateQuery.Params.AsDouble[0]]));
    try
      UpdateQuery.Execute;
    except
      on E: EUIBError do
      begin
        if (E.SQLCode = -625) then
          Log.Lines.Add('Exceeded budget limit -- not updated.') else
        begin
          SelectQuery.Close(etmRollback);
          UpdateQuery.Close(etmRollback);
          raise;
        end;
      end;
    end;
    SelectQuery.Next;
  end;
  // Set etmCommit to apply changes
  SelectQuery.Close(etmRollback);
  UpdateQuery.Close(etmRollback);
end;

end.
