{$I JVCL.INC}
unit SpecialProgressTest;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6_UP}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, JvSpecialProgress, ComCtrls, ExtCtrls, StdCtrls, JvPanel,
  JvSpacer;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    JvSpecialProgress1: TJvSpecialProgress;
    Gauge1: TGauge;
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
    procedure TrackBar1Change(Sender: TObject);
    procedure chbTextVisibleClick(Sender: TObject);
    procedure chbSolidClick(Sender: TObject);
    procedure rgrStartColorClick(Sender: TObject);
    procedure rgrEndColorClick(Sender: TObject);
    procedure chbTextCenteredClick(Sender: TObject);
    procedure chbTransparentClick(Sender: TObject);
    procedure chbGradientBlocksClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure rgrBackgroundClick(Sender: TObject);
    procedure rgrPanelClick(Sender: TObject);
    procedure rgrFontClick(Sender: TObject);
    procedure btnStepItClick(Sender: TObject);
    procedure chbPanelDoubleBufferedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  JvSpecialProgress1.Position := TrackBar1.Position;
  Gauge1.Progress := TrackBar1.Position;
  ProgressBar1.Position := TrackBar1.Position;
end;

procedure TForm1.chbTextVisibleClick(Sender: TObject);
begin
  JvSpecialProgress1.TextVisible := chbTextVisible.Checked;
end;

procedure TForm1.chbSolidClick(Sender: TObject);
begin
  JvSpecialProgress1.Solid := chbSolid.Checked;
end;

procedure TForm1.rgrStartColorClick(Sender: TObject);
begin
  JvSpecialProgress1.StartColor :=
    StringToColor(rgrStartColor.Items[rgrStartColor.ItemIndex]);
end;

procedure TForm1.rgrEndColorClick(Sender: TObject);
begin
  JvSpecialProgress1.EndColor :=
    StringToColor(rgrEndColor.Items[rgrEndColor.ItemIndex]);
end;

procedure TForm1.chbTextCenteredClick(Sender: TObject);
begin
  JvSpecialProgress1.TextCentered := chbTextCentered.Checked;
end;

procedure TForm1.chbTransparentClick(Sender: TObject);
begin
  JvSpecialProgress1.Transparent := chbTransparent.Checked;
end;

procedure TForm1.chbGradientBlocksClick(Sender: TObject);
begin
  JvSpecialProgress1.GradientBlocks := chbGradientBlocks.Checked;
end;

procedure TForm1.btnApplyClick(Sender: TObject);
begin
  JvSpecialProgress1.Minimum := StrToIntDef(edtMinimum.Text, 0);
  JvSpecialProgress1.Maximum := StrToIntDef(edtMaximum.Text, 100);
  JvSpecialProgress1.Step := StrToIntDef(edtStep.Text, 10);
  edtMinimum.Text := IntToStr(JvSpecialProgress1.Minimum);
  edtMaximum.Text := IntToStr(JvSpecialProgress1.Maximum);
  edtStep.Text := IntToStr(JvSpecialProgress1.Step);
end;

procedure TForm1.rgrBackgroundClick(Sender: TObject);
begin
  JvSpecialProgress1.Color :=
    StringToColor(rgrBackground.Items[rgrBackground.ItemIndex]);
end;

procedure TForm1.rgrPanelClick(Sender: TObject);
begin
  Panel1.Color :=
    StringToColor(rgrPanel.Items[rgrPanel.ItemIndex]);
end;

procedure TForm1.rgrFontClick(Sender: TObject);
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
          JvSpecialProgress1.TextFont.Assign(Font);
          Exit;
        end;
      end;
    JvSpecialProgress1.TextFont.Assign(TmpFont);
  finally
    TmpFont.Free;
  end;
end;

procedure TForm1.btnStepItClick(Sender: TObject);
begin
  JvSpecialProgress1.StepIt;
  TrackBar1.Position := JvSpecialProgress1.Position;
end;

procedure TForm1.chbPanelDoubleBufferedClick(Sender: TObject);
begin
  Panel2.DoubleBuffered := chbPanelDoubleBuffered.Checked;
end;

end.

