unit AutosUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TAutosForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    procedure PanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AutosForm: TAutosForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TAutosForm.PanelResize(Sender: TObject);
begin
  if Header.Width > 70 then
  begin
    Header.SectionWidth[2] := 66;
    Header.SectionWidth[1] := (Header.Width - 64) * 2 div 3;
    Header.SectionWidth[0] := (Header.Width - 64) div 3;
  end else
  begin
    Header.SectionWidth[0] := 2;
    Header.SectionWidth[1] := 2;
    Header.SectionWidth[2] := Header.Width - 4;
  end;
end;

end.
