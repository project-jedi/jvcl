unit LocalsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TLocalsForm = class(TForm)
    lbDockClient1: TJvDockClient;
    Panel1: TPanel;
    ComboBoxPanel: TPanel;
    ComboBox: TComboBox;
    Panel: TPanel;
    ListBox1: TListBox;
    Header: THeader;
    procedure ComboBoxPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LocalsForm: TLocalsForm;

implementation

uses MainFormUnit;

{$R *.DFM}

procedure TLocalsForm.ComboBoxPanelResize(Sender: TObject);
begin
  ComboBox.Width := ComboBoxPanel.Width;
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
