unit fEdits;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, JvCalculatorBox, JvEdit,
  JvFloatEdit, JvButtonBox, JvImageBox, JvDirectoryBox,
  JvCustomBox, JvFileNameBox;

type
  TForm1 = class(TForm)
    JvFileNameBox1: TJvFileNameBox;
    JvDirectoryBox1: TJvDirectoryBox;
    JvImageBox1: TJvImageBox;
    JvButtonBox1: TJvButtonBox;
    JvCalculatorBox1: TJvCalculatorBox;
    procedure JvButtonBox1ButtonClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation
{$R *.DFM}

procedure TForm1.JvButtonBox1ButtonClick(Sender: TObject);
begin
  ShowMessage('Button clicked');
end;

end.
