unit TaskListUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, Grids;

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
