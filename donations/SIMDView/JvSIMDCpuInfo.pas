unit JvSIMDCpuInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclSysInfo;

type
  TFormCpuInfo = class(TForm)
    LabelName: TLabel;
    EditName: TEdit;
    LabelVendor: TLabel;
    EditVendor: TEdit;
    LabelFrequency: TLabel;
    EditFrequency: TEdit;
    CheckBoxMMX: TCheckBox;
    CheckBoxExMMX: TCheckBox;
    CheckBox3DNow: TCheckBox;
    CheckBoxEx3DNow: TCheckBox;
    CheckBox64Bits: TCheckBox;
    CheckBoxSSE1: TCheckBox;
    CheckBoxSSE2: TCheckBox;
    CheckBoxSSE3: TCheckBox;
    ButtonClose: TButton;
  private
  public
    procedure Execute(const CpuInfo: TCPUInfo);
  end;

implementation

{$R *.dfm}

{ TFormCpuInfo }

procedure TFormCpuInfo.Execute(const CpuInfo: TCPUInfo);
begin
  EditName.Text := CpuInfo.CpuName;
  EditVendor.Text := CpuInfo.VendorIDString;
  EditFrequency.Text := IntToStr(CpuInfo.FrequencyInfo.NormFreq);
  CheckBoxMMX.Checked := CpuInfo.MMX;
  CheckBoxExMMX.Checked := CpuInfo.ExMMX;
  CheckBox3DNow.Checked := CpuInfo._3DNow;
  CheckBoxEx3DNow.Checked := CpuInfo.Ex3DNow;
  CheckBox64Bits.Checked := CpuInfo.Is64Bits;
  CheckBoxSSE1.Checked := CpuInfo.SSE >= 1;
  CheckBoxSSE2.Checked := CpuInfo.SSE >= 2;
  CheckBoxSSE3.Checked := CpuInfo.SSE >= 3;
  ShowModal;
end;

end.
