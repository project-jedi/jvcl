{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author: Christopher Latta

 Contributor(s):

 You may retrieve the latest version of this file at the JEDI-JVCL
 home page, located at http://jvcl.sourceforge.net

 The contents of this file are used with permission, subject to
 the Mozilla Public License Version 1.1 (the "License"); you may
 not use this file except in compliance with the License. You may
 obtain a copy of the License at
 http://www.mozilla.org/MPL/MPL-1_1Final.html

 Software distributed under the License is distributed on an
 "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 implied. See the License for the specific language governing
 rights and limitations under the License.

******************************************************************}

{
 A demo for the JvValidateEdit component. You don't need to install the component
 to run this demo, but the JvValidateEdit and the JvCharStrEditor units
 must be in the same directory.
}
unit MainFrm;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms, QDialogs,
  JvQValidateEdit, QStdCtrls, QComCtrls, QExtCtrls, JvQCombobox,
  JvQColorCombo, JvQExStdCtrls, JvQEdit;

type
  TfrmValidateEditDemo = class(TForm)
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    cbDisplayFormat: TComboBox;
    chkHasMaxValue: TCheckBox;
    chkHasMinValue: TCheckBox;
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
    JvValidateEdit: TJvValidateEdit;
    seDecimalPlaces: TSpinEdit;
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
    procedure JvValidateEditCustomValidate(Sender: TObject; Key: Char;
      const AText: String; const Pos: Integer; var IsValid: Boolean);
    procedure JvValidateEditValueChanged(Sender: TObject);
    procedure seDecimalPlacesChanged(Sender: TObject; NewValue: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmValidateEditDemo: TfrmValidateEditDemo;

implementation
uses
  TypInfo,
  JvQJCLUtils, JvQJVCLUtils; // for D5

{$R *.xfm}


procedure TfrmValidateEditDemo.FormCreate(Sender: TObject);
var
  df: TJvValidateEditDisplayFormat;
  cp: TJvValidateEditCriticalPointsCheck;
begin
  for df := Low(TJvValidateEditDisplayFormat) to High(TJvValidateEditDisplayFormat) do
    cbDisplayFormat.Items.Add(GetEnumName(TypeInfo(TJvValidateEditDisplayFormat), Ord(df)));
  cbDisplayFormat.ItemIndex := 0;
  cbDisplayFormatChange(Self);
  for cp := Low(TJvValidateEditCriticalPointsCheck) to High(TJvValidateEditCriticalPointsCheck) do
    cbCPCheckPoints.Items.Add(GetEnumName(TypeInfo(TJvValidateEditCriticalPointsCheck), Ord(cp)));
  cbCPCheckPoints.ItemIndex := 0;
  edCPMaxValue.Text := FloatToStr(JvValidateEdit.CriticalPoints.MaxValue);
  colCPAbove.ColorValue := JvValidateEdit.CriticalPoints.ColorAbove;
  edCPMinValue.Text := FloatToStr(JvValidateEdit.CriticalPoints.MinValue);
  colCPBelow.ColorValue := JvValidateEdit.CriticalPoints.ColorBelow;
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
  JvValidateEdit.DisplayFormat := TJvValidateEditDisplayFormat(cbDisplayFormat.ItemIndex);
  edCheckChars.Text := JvValidateEdit.CheckChars;
  btnCheckChars.Enabled := (cbDisplayFormat.Text = 'dfCheckChars') or
    (cbDisplayFormat.Text = 'dfNonCheckChars');
end;

procedure TfrmValidateEditDemo.seDecimalPlacesChange(Sender: TObject);
begin
  JvValidateEdit.DecimalPlaces := seDecimalPlaces.Value;
end;

procedure TfrmValidateEditDemo.chkHasMaxValueClick(Sender: TObject);
begin
  JvValidateEdit.MaxValue := StrToFloatDef(edMaxValue.Text, 0);
  JvValidateEdit.HasMaxValue := chkHasMaxValue.Checked;
end;

procedure TfrmValidateEditDemo.chkHasMinValueClick(Sender: TObject);
begin
  JvValidateEdit.MinValue := StrToFloatDef(edMinValue.Text, 0);
  JvValidateEdit.HasMinValue := chkHasMinValue.Checked;
end;

procedure TfrmValidateEditDemo.btnSetToClick(Sender: TObject);
begin
  case rgSetToType.ItemIndex of
    0:
      if chkAsVariant.Checked then
        JvValidateEdit.Value := StrToCurrDef(edSetTo.Text, 0)
      else
        JvValidateEdit.AsCurrency := StrToCurrDef(edSetTo.Text, 0);
    1:
      if chkAsVariant.Checked then
        JvValidateEdit.Value := StrToFloatDef(edSetTo.Text, 0)
      else
        JvValidateEdit.AsFloat := StrToFloatDef(edSetTo.Text, 0);
    2:
      if chkAsVariant.Checked then
        JvValidateEdit.Value := StrToIntDef(edSetTo.Text, 0)
      else
        JvValidateEdit.AsInteger := StrToIntDef(edSetTo.Text, 0);
    3:
      if chkAsVariant.Checked then
        JvValidateEdit.Value := edSetTo.Text
      else
        JvValidateEdit.Text := edSetTo.Text;
  end;
  edSetTo.SetFocus;
end;

procedure TfrmValidateEditDemo.chkZeroEmptyClick(Sender: TObject);
begin
  JvValidateEdit.ZeroEmpty := chkZeroEmpty.Checked;
end;

procedure TfrmValidateEditDemo.btnCheckCharsClick(Sender: TObject);
begin
  JvValidateEdit.CheckChars := edCheckChars.Text;
end;

procedure TfrmValidateEditDemo.btnSetDisplayPrefixClick(Sender: TObject);
begin
  JvValidateEdit.DisplayPrefix := edDisplayPrefix.Text;
end;

procedure TfrmValidateEditDemo.btnSetDisplaySuffixClick(Sender: TObject);
begin
  JvValidateEdit.DisplaySuffix := edDisplaySuffix.Text;
end;

procedure TfrmValidateEditDemo.edCPMaxValueExit(Sender: TObject);
begin
  JvValidateEdit.CriticalPoints.MaxValue := StrToFloatDef(edCPMaxValue.Text, 0);
  edCPMaxValue.Text := FloatToStr(JvValidateEdit.CriticalPoints.MaxValue);
end;

procedure TfrmValidateEditDemo.edCPMinValueExit(Sender: TObject);
begin
  JvValidateEdit.CriticalPoints.MinValue := StrToFloatDef(edCPMinValue.Text, 0);
  edCPMinValue.Text := FloatToStr(JvValidateEdit.CriticalPoints.MinValue);
end;

procedure TfrmValidateEditDemo.colCPAboveChange(Sender: TObject);
begin
  JvValidateEdit.CriticalPoints.ColorAbove := colCpAbove.ColorValue;
end;

procedure TfrmValidateEditDemo.colCPBelowChange(Sender: TObject);
begin
  JvValidateEdit.CriticalPoints.ColorBelow := colCpBelow.ColorValue;
end;

procedure TfrmValidateEditDemo.cbCPCheckPointsChange(Sender: TObject);
begin
  JvValidateEdit.CriticalPoints.CheckPoints :=
    TJvValidateEditCriticalPointsCheck(cbCPCheckPoints.ItemIndex);
end;


procedure TfrmValidateEditDemo.edMaxValueExit(Sender: TObject);
begin
  JvValidateEdit.MaxValue := StrToFloatDef(edMaxValue.Text, 0);
end;

procedure TfrmValidateEditDemo.edMinValueExit(Sender: TObject);
begin
  JvValidateEdit.MinValue := StrToFloatDef(edMinValue.Text, 0);
end;

procedure TfrmValidateEditDemo.JvValidateEditCustomValidate(
  Sender: TObject; Key: Char; const AText: String; const Pos: Integer;
  var IsValid: Boolean);

  function KeyOrAscii(Key: Char): string;
  begin
    if Key < #32 then
      Result := Format('#%s', [Key])
    else
      Result := Key;
  end;

begin
  IsValid := QWindows.MessageBox(Handle, PChar(Format('Accept this key: %s?', [KeyOrAscii(Key)])), PChar('Validate'), MB_YESNO) = IDYES;
end;

procedure TfrmValidateEditDemo.JvValidateEditValueChanged(Sender: TObject);
begin
  if chkValueChanged.Checked then
    MessageDlg('ValidateEdit Text changed to: ' + JvValidateEdit.Text,
      mtInformation, [mbOK], 0);
end;

procedure TfrmValidateEditDemo.seDecimalPlacesChanged(Sender: TObject;
  NewValue: Integer);
begin
  JvValidateEdit.DecimalPlaces := seDecimalPlaces.Value;
end;

end.

