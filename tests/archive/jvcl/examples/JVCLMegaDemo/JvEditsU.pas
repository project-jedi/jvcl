unit JvEditsU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvTypedEdit, JvEdit, JvFloatEdit, JvCurrEdit, JvToolEdit, Mask,
  ExtCtrls, JvComponent, JvCaptionPanel, JvComCtrls, JvMaskEdit, JvSpin;

type
  TJvEdits = class(TFrame)
    JvCaptionPanel1: TJvCaptionPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    JvFileNameBox1: TJvFilenameEdit;
    JvButtonBox1: TJvComboEdit;
    JvFloatEdit1: TJvFloatEdit;
    JvCalculatorBox1: TJvCalcEdit;
    JvCurrencyEdit1: TJvCurrencyEdit;
    JvFloatEdit21: TJvFloatEdit2;
    JvIntegerEdit1: TJvIntegerEdit;
    JvYearEdit1: TJvYearEdit;
    JvDirectoryEdit1: TJvDirectoryEdit;
    Label3: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label8: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label28: TLabel;
    JvDateEdit1: TJvDateEdit;
    JvSpinEdit1: TJvSpinEdit;
    JvIpAddress1: TJvIpAddress;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
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
