unit fEdits;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, JvEdit, JvToolEdit, JvCurrEdit;

type
  TForm1 = class(TForm)
    JvFileNameBox1: TJvFilenameEdit;
    JvDirectoryBox1: TJvDirectoryEdit;
    JvButtonBox1: TJvComboEdit;
    JvDateEdit1: TJvDateEdit;
    JvCalcEdit1: TJvCalcEdit;
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
