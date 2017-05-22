unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, JvOfficeColorButton, JvOfficeColorPanel,  
  JvSpin, JvPanel,  JvExExtCtrls, JvComponent, JvExMask ;

type
  TForm1 = class(TForm)
    chkShowNone: TCheckBox;
    chkShowDefault: TCheckBox;
    chkShowSystemColors: TCheckBox;
    chkShowUserColors: TCheckBox;
    chkShowCustom: TCheckBox;
    chkHoldCustomColor: TCheckBox;
    grp1: TGroupBox;
    chkFlatBorder: TCheckBox;
    JvSpinEdit1: TJvSpinEdit;
    lbl1: TLabel;
    grp2: TGroupBox;
    rbStandard: TRadioButton;
    rbOfficeXP: TRadioButton;
    jvofcb1: TJvOfficeColorButton;
    jvocp1: TJvOfficeColorPanel;
    grp3: TGroupBox;
    rbColorPanel: TRadioButton;
    rbColorButton: TRadioButton;
    grp4: TGroupBox;
    pnlDemoColor: TPanel;
    chkEnableGlyph: TCheckBox;
    chkEnable: TCheckBox;
    chkShowDragBar: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure chkShowNoneClick(Sender: TObject);
    procedure rbOfficeXPClick(Sender: TObject);
    procedure rbStandardClick(Sender: TObject);
    procedure jvocp1ColorChange(Sender: TObject);
    procedure rbColorPanelClick(Sender: TObject);
  private
    { Private declarations }
    FBusy:Boolean;
    procedure UpdateColorPanelState(ATag:Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  JvJCLUtils
  ;

{$R Ico.res}

{$R *.dfm}

procedure TForm1.UpdateColorPanelState(ATag:Integer);
var
  lProps :TJvOfficeColorPanelProperties;
  c:TControl;
begin
  if FBusy then Exit;
  FBusy := True;
  if rbColorPanel.Checked then
  begin
    lProps := jvocp1.Properties;
    c := jvocp1;
  end
  else
  begin
    lProps := jvofcb1.Properties;
    c := jvofcb1;
  end;

  with lProps do
  begin
    case ATag of
      1:
      begin
        chkShowNone.Checked := ShowNoneColor;
        chkShowDefault.Checked := ShowDefaultColor;
        chkShowSystemColors.Checked := ShowSystemColors;
        chkShowUserColors.Checked := ShowUserColors;
        chkShowCustom.Checked := ShowCustomColor;
        chkHoldCustomColor.Checked := HoldCustomColor;

        chkEnableGlyph.Enabled := rbColorButton.Checked;
        chkEnableGlyph.Checked :=not jvofcb1.Glyph.Empty;

        chkShowDragBar.Enabled := rbColorButton.Checked;
        chkShowDragBar.Checked := jvofcb1.Properties.ShowDragBar;

        chkEnable.Checked := c.Enabled;

        chkFlatBorder.Enabled := rbColorPanel.Checked;
        chkFlatBorder.Checked := jvocp1.FlatBorder;
        JvSpinEdit1.Enabled := chkFlatBorder.Checked;
        JvSpinEdit1.Value := jvocp1.BorderWidth;
      end;
      2:
      begin
        ShowNoneColor := chkShowNone.Checked;
        ShowDefaultColor := chkShowDefault.Checked;
        ShowSystemColors := chkShowSystemColors.Checked;
        ShowUserColors := chkShowUserColors.Checked;
        ShowCustomColor := chkShowCustom.Checked;
        HoldCustomColor := chkHoldCustomColor.Checked;
        jvocp1.FlatBorder := chkFlatBorder.Checked;
        JvSpinEdit1.Enabled := chkFlatBorder.Checked;

        if rbColorButton.Checked then
        begin
          if chkEnableGlyph.Checked then
             jvofcb1.Glyph.LoadFromResourceName(hInstance, 'FONTCOLOR')
          else jvofcb1.Glyph := nil;

          with TJvOfficeColorButtonProperties(lProps) do
          begin
            ShowDragBar := chkShowDragBar.Checked;
          end;
        end;

        c.Enabled := chkEnable.Checked;

        jvocp1.BorderWidth := JvSpinEdit1.AsInteger;
      end;
    end;

  end;
  FBusy := False;
end;

procedure TForm1.chkShowNoneClick(Sender: TObject);
begin
   UpdateColorPanelState(2);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   UpdateColorPanelState(1);
end;

type
  THackControl = class(TControl);

procedure TForm1.rbOfficeXPClick(Sender: TObject);
begin
  if rbColorPanel.Checked then
  with jvocp1 do
  begin
    FlatBorder := true;
    BorderWidth := 1;
    BackColor := clWindow;
    HotTrack := True;
    HotTrackOptions.Enabled := true;
    HotTrackOptions.FrameVisible := True;
    UpdateColorPanelState(1);
  end else
  if rbColorButton.Checked then
  with jvofcb1 do
  begin
    DropingBgColor := clWindow;
    HotTrack := True;
    HotTrackOptions.Enabled := true;
    HotTrackOptions.FrameVisible := True;
    UpdateColorPanelState(1);
  end;
end;

procedure TForm1.rbStandardClick(Sender: TObject);
begin
  if rbColorPanel.Checked then
  with jvocp1 do
  begin
    FlatBorder := False;
    BorderWidth := 0;
    BackColor := clBtnFace;
    HotTrack := False;
    HotTrackOptions.Enabled := False;
    HotTrackOptions.FrameVisible := False;
  end else
  if rbColorButton.Checked then
  with jvofcb1 do
  begin
    DropingBgColor := clBtnFace;
    HotTrack := False;
    HotTrackOptions.Enabled := False;
    HotTrackOptions.FrameVisible := False;
  end;
end;

procedure TForm1.jvocp1ColorChange(Sender: TObject);
begin
  if Sender = jvocp1 then
    pnlDemoColor.Color := jvocp1.SelectedColor
  else   if Sender = jvofcb1 then
    pnlDemoColor.Color := jvofcb1.SelectedColor;

  grp4.Caption := 'Seleted Color: '+ ColorToPrettyName(pnlDemoColor.Color);

end;

procedure TForm1.rbColorPanelClick(Sender: TObject);
begin
  if Sender = rbColorButton then
  begin
    chkEnableGlyph.Enabled := True;
    chkShowDragBar.Enabled := True;
    chkFlatBorder.Enabled := False;
  end
  else
  begin
    chkEnableGlyph.Enabled := False;
    chkShowDragBar.Enabled := False;
    chkFlatBorder.Enabled := True;
  end;
  chkShowNoneClick(nil);
  if rbStandard.Checked then
    THackControl(rbStandard).Click;
  if rbOfficeXP.Checked then
    THackControl(rbOfficeXP).Click;
end;

end.
