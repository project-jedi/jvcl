unit OutputUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, JvDockControlForm, StdCtrls;

type
  TOutputForm = class(TForm)
    lbDockClient1: TJvDockClient;
    ComboBoxPanel: TPanel;
    ComboBox: TComboBox;
    Shape1: TShape;
    procedure ComboBoxPanelResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OutputForm: TOutputForm;

implementation

uses MainFormUnit, Math;

{$R *.DFM}

procedure TOutputForm.ComboBoxPanelResize(Sender: TObject);
begin
  ComboBox.Width := ComboBoxPanel.Width;
end;

end.
