{******************************************************************

                       JEDI-VCL Demo

 Copyright (C) 2002 Project JEDI

 Original author:

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

{$I jvcl.inc}

unit JvSpecialProgressMainFormU;

interface

uses
  QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, Gauges, JvQSpecialProgress, QComCtrls, QExtCtrls, QStdCtrls, JvQPanel,
  JvQSpacer, JvQExControls, JvQComponent, JvQGauges;

type
  TJvSpecialProgressMainForm = class(TForm)
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    JvSpecialProgress1: TJvSpecialProgress;
    ProgressBar1: TProgressBar;
    Panel2: TPanel;
    rgrStartColor: TRadioGroup;
    rgrEndColor: TRadioGroup;
    rgrBackground: TRadioGroup;
    rgrPanel: TRadioGroup;
    chbSolid: TCheckBox;
    chbTextVisible: TCheckBox;
    chbTextCentered: TCheckBox;
    chbTransparent: TCheckBox;
    chbGradientBlocks: TCheckBox;
    rgrFont: TRadioGroup;
    Panel3: TPanel;
    lblMinimum: TLabel;
    edtMinimum: TEdit;
    lblMaximum: TLabel;
    edtMaximum: TEdit;
    lblStep: TLabel;
    edtStep: TEdit;
    btnApply: TButton;
    btnStepIt: TButton;
    chbPanelDoubleBuffered: TCheckBox;
    btnRandom: TButton;
    Gauge1: TJvGauge;
    procedure TrackBar1Change(Sender: TObject);
    procedure chbTextVisibleClick(Sender: TObject);
    procedure chbSolidClick(Sender: TObject);
    procedure rgrStartColorClick(Sender: TObject);
    procedure rgrEndColorClick(Sender: TObject);
    procedure chbTextCenteredClick(Sender: TObject);
    procedure chbGradientBlocksClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure rgrBackgroundClick(Sender: TObject);
    procedure rgrPanelClick(Sender: TObject);
    procedure rgrFontClick(Sender: TObject);
    procedure btnStepItClick(Sender: TObject);
    procedure chbPanelDoubleBufferedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRandomClick(Sender: TObject);
  public
    procedure RandomizeControls;
  end;

var
  JvSpecialProgressMainForm: TJvSpecialProgressMainForm;

implementation

{$R *.xfm}

procedure TJvSpecialProgressMainForm.TrackBar1Change(Sender: TObject);
begin
  JvSpecialProgress1.Position := TrackBar1.Position;
  Gauge1.Progress := TrackBar1.Position;
  ProgressBar1.Position := TrackBar1.Position;
end;

procedure TJvSpecialProgressMainForm.chbTextVisibleClick(Sender: TObject);
begin
  if chbTextVisible.Checked then
    JvSpecialProgress1.TextOption := toPercent
  else
    JvSpecialProgress1.TextOption := toNoText
end;

procedure TJvSpecialProgressMainForm.chbSolidClick(Sender: TObject);
begin
  JvSpecialProgress1.Solid := chbSolid.Checked;
end;

procedure TJvSpecialProgressMainForm.rgrStartColorClick(Sender: TObject);
begin
  JvSpecialProgress1.StartColor :=
    StringToColor(rgrStartColor.Items[rgrStartColor.ItemIndex]);
end;

procedure TJvSpecialProgressMainForm.rgrEndColorClick(Sender: TObject);
begin
  JvSpecialProgress1.EndColor :=
    StringToColor(rgrEndColor.Items[rgrEndColor.ItemIndex]);
end;

procedure TJvSpecialProgressMainForm.chbTextCenteredClick(Sender: TObject);
begin
  JvSpecialProgress1.TextCentered := chbTextCentered.Checked;
end;

procedure TJvSpecialProgressMainForm.chbGradientBlocksClick(Sender: TObject);
begin
  JvSpecialProgress1.GradientBlocks := chbGradientBlocks.Checked;
end;

procedure TJvSpecialProgressMainForm.btnApplyClick(Sender: TObject);
begin
  JvSpecialProgress1.Minimum := StrToIntDef(edtMinimum.Text, 0);
  JvSpecialProgress1.Maximum := StrToIntDef(edtMaximum.Text, 100);
  JvSpecialProgress1.Step := StrToIntDef(edtStep.Text, 10);
  edtMinimum.Text := IntToStr(JvSpecialProgress1.Minimum);
  edtMaximum.Text := IntToStr(JvSpecialProgress1.Maximum);
  edtStep.Text := IntToStr(JvSpecialProgress1.Step);
end;

procedure TJvSpecialProgressMainForm.rgrBackgroundClick(Sender: TObject);
begin
  JvSpecialProgress1.Color :=
    StringToColor(rgrBackground.Items[rgrBackground.ItemIndex]);
end;

procedure TJvSpecialProgressMainForm.rgrPanelClick(Sender: TObject);
begin
  Panel1.Color :=
    StringToColor(rgrPanel.Items[rgrPanel.ItemIndex]);
end;

procedure TJvSpecialProgressMainForm.rgrFontClick(Sender: TObject);
var
  TmpFont: TFont;
begin
  TmpFont := TFont.Create;
  try
    TmpFont.Assign(Font);
    with TmpFont do
      case rgrFont.ItemIndex of
        1:
          begin
            Color := clRed;
          end;
        2:
          begin
            Name := 'Tahoma';
            Size := 20;
          end;
        3:
          begin
            Style := [fsBold];
            Color := clGreen;
            Size := 12;
          end;
        4:
          begin
            Style := [fsItalic];
            Color := clYellow;
            Size := 16;
          end;
        5:
          begin
            Style := [fsUnderLine];
            Color := clWhite;
          end;
        6:
          begin
            Style := [fsStrikeOut];
            Size := 25;
          end;
        7:
          begin
            Name := 'Roman';
            Size := 20;
            Style := [fsBold, fsItalic];
            Color := clWhite;
          end;
      else
        begin
          JvSpecialProgress1.Font.Assign(Font);
          Exit;
        end;
      end;
    JvSpecialProgress1.Font.Assign(TmpFont);
  finally
    TmpFont.Free;
  end;
end;

procedure TJvSpecialProgressMainForm.btnStepItClick(Sender: TObject);
begin
  JvSpecialProgress1.StepIt;
  TrackBar1.Position := JvSpecialProgress1.Position;
end;

procedure TJvSpecialProgressMainForm.chbPanelDoubleBufferedClick(Sender: TObject);
begin
//  Panel2.DoubleBuffered := chbPanelDoubleBuffered.Checked;
end;

procedure TJvSpecialProgressMainForm.RandomizeControls;
begin
  { Changing RadioGroups ItemIndex will implicitly trigger the OnClick event }
  rgrStartColor.ItemIndex := Random(rgrStartColor.Items.Count);
  rgrEndColor.ItemIndex := Random(rgrEndColor.Items.Count);
  rgrBackground.ItemIndex := Random(rgrBackground.Items.Count);
  rgrPanel.ItemIndex := Random(rgrPanel.Items.Count);
  rgrFont.ItemIndex := Random(rgrFont.Items.Count);

  { Changing Checkboxs ItemIndex will implicitly trigger the OnClick event }
  chbSolid.Checked := Random > 0.5;
  chbTextVisible.Checked := Random > 0.5;
  chbTextCentered.Checked := Random > 0.5;
  chbTransparent.Checked := Random > 0.5;
  chbGradientBlocks.Checked := Random > 0.5;
  chbPanelDoubleBuffered.Checked := Random > 0.5;

  TrackBar1.Position := Random(101);
end;

procedure TJvSpecialProgressMainForm.FormShow(Sender: TObject);
begin
  Randomize;

  JvSpecialProgress1.Minimum := 0;
  JvSpecialProgress1.Maximum := 100;
  JvSpecialProgress1.Step := 10;
  edtMinimum.Text := IntToStr(JvSpecialProgress1.Minimum);
  edtMaximum.Text := IntToStr(JvSpecialProgress1.Maximum);
  edtStep.Text := IntToStr(JvSpecialProgress1.Step);

  RandomizeControls;
end;

procedure TJvSpecialProgressMainForm.btnRandomClick(Sender: TObject);
begin
  RandomizeControls;
end;

end.

