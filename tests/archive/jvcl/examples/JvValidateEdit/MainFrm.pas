{
 A demo for the JvValidateEdit component. You don't need to install the component
 to run this demo, but the JvValidateEdit and the JvCharStrEditor units
 must be in the same directory.
}
unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvValidateEdit, StdCtrls, ComCtrls, Spin, ExtCtrls, JvCombobox,
  JvColorCombo;

type
  TfrmValidateEditDemo = class(TForm)
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    cbDisplayFormat: TComboBox;
    Button2: TButton;
    chkHasMaxValue: TCheckBox;
    chkHasMinValue: TCheckBox;
    seDecimalPlaces: TSpinEdit;
    Label4: TLabel;
    chkZeroEmpty: TCheckBox;
    edCheckChars: TEdit;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    edSetTo: TEdit;
    rgSetToType: TRadioGroup;
    btnSetTo: TButton;
    chkValueChanged: TCheckBox;
    btnCheckChars: TButton;
    Label6: TLabel;
    Label7: TLabel;
    edDisplayPrefix: TEdit;
    edDisplaySuffix: TEdit;
    btnSetDisplayPrefix: TButton;
    btnSetDisplaySuffix: TButton;
    edMaxValue: TEdit;
    edMinValue: TEdit;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    edCPMaxValue: TEdit;
    Label9: TLabel;
    colCPAbove: TJvColorComboBox;
    Label10: TLabel;
    edCPMinValue: TEdit;
    Label11: TLabel;
    colCPBelow: TJvColorComboBox;
    Label12: TLabel;
    cbCPCheckPoints: TComboBox;
    chkAsVariant: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbDisplayFormatKeyPress(Sender: TObject; var Key: Char);
    procedure cbDisplayFormatChange(Sender: TObject);
    procedure seDecimalPlacesChange(Sender: TObject);
    procedure chkHasMaxValueClick(Sender: TObject);
    procedure chkHasMinValueClick(Sender: TObject);
    procedure btnSetToClick(Sender: TObject);
    procedure chkZeroEmptyClick(Sender: TObject);
    procedure btnCheckCharsClick(Sender: TObject);
    procedure btnSetDisplayPrefixClick(Sender: TObject);
    procedure btnSetDisplaySuffixClick(Sender: TObject);
    procedure edCPMaxValueExit(Sender: TObject);
    procedure edCPMinValueExit(Sender: TObject);
    procedure colCPAboveChange(Sender: TObject);
    procedure colCPBelowChange(Sender: TObject);
    procedure cbCPCheckPointsChange(Sender: TObject);
    procedure edMaxValueExit(Sender: TObject);
    procedure edMinValueExit(Sender: TObject);
  private
    procedure DoCustomValidate(Sender: TObject; Key: Char;
      const AText: string; const Pos: Integer; var IsValid: boolean);
    procedure ShowValueChange(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    FValidateEdit: TJvValidateEdit;
  end;

var
  frmValidateEditDemo: TfrmValidateEditDemo;

implementation
uses
  TypInfo,
  JvFunctions; // for D5
  
{$R *.DFM}

procedure TfrmValidateEditDemo.DoCustomValidate(Sender: TObject; Key: Char;
      const AText: string; const Pos: Integer; var IsValid: boolean);
function KeyOrAscii(Key: Char): string;
begin
  if Key < #32 then
    Result := Format('#%s', [Key])
  else
    Result := Key;
end;

begin
  IsValid := MessageBox(Handle, PChar(Format('Accept this key: %s?', [KeyOrAscii(Key)])), PChar('Validate'), MB_YESNO) = IDYES;
end;

procedure TfrmValidateEditDemo.FormCreate(Sender: TObject);
var
  df: TJvValidateEditDisplayFormat;
  cp: TJvValidateEditCriticalPointsCheck;
begin
  FValidateEdit := TJvValidateEdit.Create(self);
  FValidateEdit.Parent := Self;
  FValidateEdit.SetBounds(Label2.Left, Label2.Top + Label2.Height + 4, Self.ClientWidth - Label2.Left * 2, FValidateEdit.Height);
  FValidateEdit.Anchors := [akLeft, akTop, akRight];
  FValidateEdit.OnCustomValidate := DoCustomValidate;
  FValidateEdit.OnValueChanged := ShowValueChange;
  for df := Low(TJvValidateEditDisplayFormat) to High(TJvValidateEditDisplayFormat) do
    cbDisplayFormat.Items.Add(GetEnumName(TypeInfo(TJvValidateEditDisplayFormat), Ord(df)));
  cbDisplayFormat.ItemIndex := 0;
  cbDisplayFormatChange(Self);
  for cp := Low(TJvValidateEditCriticalPointsCheck) to High(TJvValidateEditCriticalPointsCheck) do
    cbCPCheckPoints.Items.Add(GetEnumName(TypeInfo(TJvValidateEditCriticalPointsCheck), Ord(cp)));
  cbCPCheckPoints.ItemIndex := 0;
  edCPMaxValue.Text := FloatToStr(FValidateEdit.CriticalPoints.MaxValue);
  colCPAbove.ColorValue := FValidateEdit.CriticalPoints.ColorAbove;
  edCPMinValue.Text := FloatToStr(FValidateEdit.CriticalPoints.MinValue);
  colCPBelow.ColorValue := FValidateEdit.CriticalPoints.ColorBelow;
  colCPAbove.OnChange := colCPAboveChange;
  colCPBelow.OnChange := colCPBelowChange;
end;

procedure TfrmValidateEditDemo.cbDisplayFormatKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    cbDisplayFormatChange(Sender);
end;

procedure TfrmValidateEditDemo.cbDisplayFormatChange(Sender: TObject);
begin
  FValidateEdit.DisplayFormat := TJvValidateEditDisplayFormat(cbDisplayFormat.ItemIndex);
  edCheckChars.Text := FValidateEdit.CheckChars;
  btnCheckChars.Enabled := (cbDisplayFormat.Text = 'dfCheckChars') or
    (cbDisplayFormat.Text = 'dfNonCheckChars');
end;

procedure TfrmValidateEditDemo.seDecimalPlacesChange(Sender: TObject);
begin
  FValidateEdit.DecimalPlaces := seDecimalPlaces.Value;
end;

procedure TfrmValidateEditDemo.chkHasMaxValueClick(Sender: TObject);
begin
  FValidateEdit.MaxValue := StrToFloatDef(edMaxValue.Text, 0);
  FValidateEdit.HasMaxValue := chkHasMaxValue.Checked;
end;

procedure TfrmValidateEditDemo.chkHasMinValueClick(Sender: TObject);
begin
  FValidateEdit.MinValue := StrToFloatDef(edMinValue.Text, 0);
  FValidateEdit.HasMinValue := chkHasMinValue.Checked;
end;

procedure TfrmValidateEditDemo.btnSetToClick(Sender: TObject);
begin
  case rgSetToType.ItemIndex of
    0:
      if chkAsVariant.Checked then
        FValidateEdit.Value := StrToCurrDef(edSetTo.Text, 0)
      else
        FValidateEdit.AsCurrency := StrToCurrDef(edSetTo.Text, 0);
    1:
      if chkAsVariant.Checked then
        FValidateEdit.Value := StrToFloatDef(edSetTo.Text, 0)
      else
        FValidateEdit.AsFloat := StrToFloatDef(edSetTo.Text, 0);
    2:
      if chkAsVariant.Checked then
        FValidateEdit.Value := StrToIntDef(edSetTo.Text, 0)
      else
        FValidateEdit.AsInteger := StrToIntDef(edSetTo.Text, 0);
    3:
      if chkAsVariant.Checked then
        FValidateEdit.Value := edSetTo.Text
      else
        FValidateEdit.Text := edSetTo.Text;
  end;
  edSetTo.SetFocus;
end;

procedure TfrmValidateEditDemo.ShowValueChange(Sender: TObject);
begin
  if chkValueChanged.Checked then
    MessageDlg('ValidateEdit Text changed to: ' + FValidateEdit.Text,
      mtInformation, [mbOK], 0);
end;

procedure TfrmValidateEditDemo.chkZeroEmptyClick(Sender: TObject);
begin
  FValidateEdit.ZeroEmpty := chkZeroEmpty.Checked;
end;

procedure TfrmValidateEditDemo.btnCheckCharsClick(Sender: TObject);
begin
  FValidateEdit.CheckChars := edCheckChars.Text;
end;

procedure TfrmValidateEditDemo.btnSetDisplayPrefixClick(Sender: TObject);
begin
  FValidateEdit.DisplayPrefix := edDisplayPrefix.Text;
end;

procedure TfrmValidateEditDemo.btnSetDisplaySuffixClick(Sender: TObject);
begin
  FValidateEdit.DisplaySuffix := edDisplaySuffix.Text;
end;

procedure TfrmValidateEditDemo.edCPMaxValueExit(Sender: TObject);
begin
  FValidateEdit.CriticalPoints.MaxValue := StrToFloatDef(edCPMaxValue.Text, 0);
  edCPMaxValue.Text := FloatToStr(FValidateEdit.CriticalPoints.MaxValue);
end;

procedure TfrmValidateEditDemo.edCPMinValueExit(Sender: TObject);
begin
  FValidateEdit.CriticalPoints.MinValue := StrToFloatDef(edCPMinValue.Text, 0);
  edCPMinValue.Text := FloatToStr(FValidateEdit.CriticalPoints.MinValue);
end;

procedure TfrmValidateEditDemo.colCPAboveChange(Sender: TObject);
begin
  FValidateEdit.CriticalPoints.ColorAbove := colCpAbove.ColorValue;
end;

procedure TfrmValidateEditDemo.colCPBelowChange(Sender: TObject);
begin
  FValidateEdit.CriticalPoints.ColorBelow := colCpBelow.ColorValue;
end;

procedure TfrmValidateEditDemo.cbCPCheckPointsChange(Sender: TObject);
begin
  FValidateEdit.CriticalPoints.CheckPoints :=
    TJvValidateEditCriticalPointsCheck(cbCPCheckPoints.ItemIndex);
end;


procedure TfrmValidateEditDemo.edMaxValueExit(Sender: TObject);
begin
  FValidateEdit.MaxValue := StrToFloatDef(edMaxValue.Text, 0);
end;

procedure TfrmValidateEditDemo.edMinValueExit(Sender: TObject);
begin
  FValidateEdit.MinValue := StrToFloatDef(edMinValue.Text, 0);
end;

end.

