unit JvExtEditsU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvCalculatorBox, JvCustomBox, JvButtonBox, StdCtrls, JvTypedEdit, JvEdit,
  JvPopupMemo, JvLinkLabel, JvAngleLabel, JvReversedLabel, JvLabel,
  JvBlinkingLabel, JvSpin, JvToolEdit, Mask, JvCurrEdit, JvComCtrls;

type
  TJvEditsFrm = class(TFrame)
    Label4: TLabel;
    JvFloatEdit21: TJvFloatEdit2;
    JvCurrencyEdit1: TJvCurrencyEdit;
    Label1: TLabel;
    JvCalcEdit1: TJvCalcEdit;
    Label2: TLabel;
    Label3: TLabel;
    JvComboEdit1: TJvComboEdit;
    JvFilenameEdit1: TJvFilenameEdit;
    JvDirectoryEdit1: TJvDirectoryEdit;
    JvDateEdit1: TJvDateEdit;
    JvxSpinEdit1: TJvxSpinEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label5: TLabel;
    JvIpAddress1: TJvIpAddress;
    Label6: TLabel;
    procedure JvComboEdit1ButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvEditsFrm.JvComboEdit1ButtonClick(Sender: TObject);
begin
 ShowMessage('You have clicked on the buttons!');
end;

end.
