unit JvPanelDemoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, JvPanel, StdCtrls, ComCtrls, Mask, JvToolEdit,
  JvComponent, JvFormPlacement;

type
  TForm1 = class(TForm)
    JvPanel1: TJvPanel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    Animate1: TAnimate;
    JvFilenameEdit1: TJvFilenameEdit;
    Label1: TLabel;
    JvFormStorage1: TJvFormStorage;
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  JvPanel1.Transparent := CheckBox1.Checked;
  if JvPanel1.Transparent then
    Edit1.Top := 8
  else
    Edit1.Top := JvPanel1.Height - 35;
end;

end.

