unit JvEditsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvTypedEdit, JvCalculatorBox, JvEdit, JvFloatEdit, JvButtonBox,
  JvImageBox, JvDirectoryBox, JvCustomBox, JvFileNameBox;

type
  TJvEdits = class(TFrame)
    JvFileNameBox1: TJvFileNameBox;
    JvDirectoryBox1: TJvDirectoryBox;
    JvImageBox1: TJvImageBox;
    JvButtonBox1: TJvButtonBox;
    JvFloatEdit1: TJvFloatEdit;
    JvCalculatorBox1: TJvCalculatorBox;
    JvCurrencyEdit1: TJvCurrencyEdit;
    JvFloatEdit21: TJvFloatEdit2;
    JvIntegerEdit1: TJvIntegerEdit;
    JvYearEdit1: TJvYearEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
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
  jvButtonBox1.Edit.Text := 'Something done !';  
end;

end.
