unit JvEditsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvTypedEdit, JvEdit, JvFloatEdit, JvCurrEdit, JvToolEdit, Mask;

type
  TJvEdits = class(TFrame)
    JvFileNameBox1: TJvFilenameEdit;
    JvButtonBox1: TJvComboEdit;
    JvFloatEdit1: TJvFloatEdit;
    JvCalculatorBox1: TJvCalcEdit;
    JvCurrencyEdit1: TJvCurrencyEdit;
    JvFloatEdit21: TJvFloatEdit2;
    JvIntegerEdit1: TJvIntegerEdit;
    JvYearEdit1: TJvYearEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    JvDirectoryEdit1: TJvDirectoryEdit;
    procedure JvButtonBox1ButtonClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TJvEdits.JvButtonBox1ButtonClick(Sender: TObject);
begin
  ShowMessage('Here you can do something :)');
  jvButtonBox1.Text := 'Something done !';
end;

end.
