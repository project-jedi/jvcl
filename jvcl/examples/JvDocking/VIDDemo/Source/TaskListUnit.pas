{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit TaskListUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, Grids, JvComponent;

type
  TTaskListForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TaskListForm: TTaskListForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TTaskListForm.FormCreate(Sender: TObject);
begin
  StringGrid1.Cells[0, 0] := '!';
  StringGrid1.Cells[1, 0] := '';
  StringGrid1.Cells[2, 0] := '';
  StringGrid1.Cells[3, 0] := 'Description';
  StringGrid1.Cells[4, 0] := 'File';
  StringGrid1.Cells[5, 0] := 'line';
  StringGrid1.Cells[3, 1] := 'Click here to add a new task';
end;

end.
