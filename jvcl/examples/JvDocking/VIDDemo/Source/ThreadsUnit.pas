unit ThreadsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TThreadsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    ComboBoxPanel: TPanel;
    ComboBox: TComboBox;
    procedure PanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ThreadsForm: TThreadsForm;

implementation

uses MainFormUnit, Math;

{$R *.DFM}

procedure TThreadsForm.PanelResize(Sender: TObject);
begin
  ComboBox.Width := ComboBoxPanel.Width;
  Header.SectionWidth[0] := 18;
  Header.SectionWidth[1] := Max((Header.Width - Header.SectionWidth[0]) div 10, 3);
  Header.SectionWidth[2] := Max((Header.Width - Header.SectionWidth[0]) * 4 div 10, 3);
  Header.SectionWidth[3] := Max((Header.Width - Header.SectionWidth[0]) * 4 div 10, 3);
  Header.SectionWidth[4] := Max((Header.Width - Header.SectionWidth[0]) div 10, 3);
end;

end.
