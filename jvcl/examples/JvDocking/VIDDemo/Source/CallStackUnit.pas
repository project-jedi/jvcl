unit CallStackUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TCallStackForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    ComboBoxPanel: TPanel;
    ComboBox: TComboBox;
    procedure ComboBoxPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CallStackForm: TCallStackForm;

implementation

uses MainFormUnit, Math;

{$R *.DFM}

procedure TCallStackForm.ComboBoxPanelResize(Sender: TObject);
begin
  ComboBox.Width := ComboBoxPanel.Width;
  Header.SectionWidth[0] := 18;
  Header.SectionWidth[1] := Max(Header.Width - Header.SectionWidth[0] - 100, 3);
  Header.SectionWidth[2] := 100;
end;

end.
