unit MainDlg;

interface

uses
  Windows, Messages, Forms,JvButton, JvComponent, ImgList, Controls,
  StdCtrls, JvGradientCaption, JvListComb, JvBalloonHint, ExtCtrls, Classes;

type
  TfrmMain = class(TForm)
    ImageList2: TImageList;
    pnlDefaultValues: TPanel;
    lblDefaultHeader: TLabel;
    edtDefaultHeader: TEdit;
    rgrDefaultIcon: TRadioGroup;
    grbDefaultImageIndex: TGroupBox;
    ilbDefaultImageIndex: TJvImageListBox;
    rgrDefaultBalloonPosition: TRadioGroup;
    JvGradientCaption1: TJvGradientCaption;
    pnlBalloon: TPanel;
    JvGradientCaption3: TJvGradientCaption;
    grbOptions: TGroupBox;
    chbUseDefaultHeader: TCheckBox;
    chbUseDefaultIcon: TCheckBox;
    chbUseDefaultImageIndex: TCheckBox;
    chbShowCloseBtn: TCheckBox;
    chbCustomAnimation: TCheckBox;
    chbO_PlaySound: TCheckBox;
    btnLaunch: TButton;
    grbCustomAnimation: TGroupBox;
    lblCustomAnimationTime: TLabel;
    edtCustomAnimationTime: TEdit;
    rgrCustomAnimationStyle: TRadioGroup;
    lblMessage: TLabel;
    lblHeader: TLabel;
    edtHeader: TEdit;
    lblVisibleTime: TLabel;
    edtVisibleTime: TEdit;
    lblAnchorCtrl: TLabel;
    cmbAnchorCtrl: TComboBox;
    memMessage: TMemo;
    pnlApplicationHint: TPanel;
    chbShowHeaderInHint: TCheckBox;
    chbShowIconInHint: TCheckBox;
    chbUseBalloonAsHint: TCheckBox;
    chbPlaySound: TCheckBox;
    JvGradientCaption2: TJvGradientCaption;
    JvBalloonHint1: TJvBalloonHint;
    procedure edtDefaultHeaderChange(Sender: TObject);
    procedure chbShowHeaderInHintClick(Sender: TObject);
    procedure chbShowIconInHintClick(Sender: TObject);
    procedure chbUseBalloonAsHintClick(Sender: TObject);
    procedure chbPlaySoundClick(Sender: TObject);
    procedure rgrDefaultIconClick(Sender: TObject);
    procedure ilbDefaultImageIndexClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgrDefaultBalloonPositionClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure rgrCustomAnimationStyleClick(Sender: TObject);
    procedure edtCustomAnimationTimeChange(Sender: TObject);
    procedure chbUseDefaultHeaderClick(Sender: TObject);
    procedure chbUseDefaultIconClick(Sender: TObject);
    procedure chbUseDefaultImageIndexClick(Sender: TObject);
    procedure chbShowCloseBtnClick(Sender: TObject);
    procedure chbCustomAnimationClick(Sender: TObject);
    procedure chbO_PlaySoundClick(Sender: TObject);
  public
    procedure InitValues;
    procedure FillAnchors(Strings: TStrings);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  SysUtils;

{$R *.dfm}

procedure TfrmMain.edtDefaultHeaderChange(Sender: TObject);
begin
  JvBalloonHint1.DefaultHeader := edtDefaultHeader.Text;
end;

procedure TfrmMain.chbShowHeaderInHintClick(Sender: TObject);
begin
  if chbShowHeaderInHint.Checked then
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions + [ahShowHeaderInHint]
  else
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions - [ahShowHeaderInHint]
end;

procedure TfrmMain.chbShowIconInHintClick(Sender: TObject);
begin
  if chbShowIconInHint.Checked then
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions + [ahShowIconInHint]
  else
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions - [ahShowIconInHint];
end;

procedure TfrmMain.chbUseBalloonAsHintClick(Sender: TObject);
begin
  if chbUseBalloonAsHint.Checked then
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions + [ahUseBalloonAsHint]
  else
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions - [ahUseBalloonAsHint];
end;

procedure TfrmMain.chbPlaySoundClick(Sender: TObject);
begin
  if chbPlaySound.Checked then
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions + [ahPlaySound]
  else
    JvBalloonHint1.ApplicationHintOptions := JvBalloonHint1.ApplicationHintOptions - [ahPlaySound];
end;

procedure TfrmMain.rgrDefaultIconClick(Sender: TObject);
begin
  with rgrDefaultIcon do
    if ItemIndex >= 0 then
      JvBalloonHint1.DefaultIcon := TJvIconKind(ItemIndex);
end;

procedure TfrmMain.ilbDefaultImageIndexClick(Sender: TObject);
begin
  JvBalloonHint1.DefaultImageIndex := ilbDefaultImageIndex.ItemIndex;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  InitValues;
end;

procedure TfrmMain.InitValues;
begin
  with JvBalloonHint1 do
  begin
    chbShowHeaderInHint.Checked := ahShowHeaderInHint in ApplicationHintOptions;
    chbShowIconInHint.Checked := ahShowIconInHint in ApplicationHintOptions;
    chbUseBalloonAsHint.Checked := ahUseBalloonAsHint in ApplicationHintOptions;
    chbPlaySound.Checked := ahPlaySound in ApplicationHintOptions;
    rgrDefaultIcon.ItemIndex := Integer(DefaultIcon);
    rgrDefaultBalloonPosition.ItemIndex := Integer(DefaultBalloonPosition);

    chbUseDefaultHeader.Checked := boUseDefaultHeader in Options;
    chbUseDefaultIcon.Checked := boUseDefaultIcon in Options;
    chbUseDefaultImageIndex.Checked := boUseDefaultImageIndex in Options;
    chbShowCloseBtn.Checked := boShowCloseBtn in Options;
    chbCustomAnimation.Checked := boCustomAnimation in Options;
    chbO_PlaySound.Checked := boPlaySound in Options;
    rgrCustomAnimationStyle.ItemIndex := Integer(CustomAnimationStyle);
    edtCustomAnimationTime.Text := IntToStr(CustomAnimationTime);

    edtHeader.Text := 'Header';
    edtDefaultHeader.Text := DefaultHeader;
    memMessage.Text := 'Message';
    edtVisibleTime.Text := '10000';
  end;

  FillAnchors(cmbAnchorCtrl.Items);
  with cmbAnchorCtrl do
    ItemIndex := Items.IndexOf(btnLaunch.Name);
end;

procedure TfrmMain.rgrDefaultBalloonPositionClick(Sender: TObject);
begin
  with rgrDefaultBalloonPosition do
    if ItemIndex >= 0 then
      JvBalloonHint1.DefaultBalloonPosition := TJvBalloonPosition(ItemIndex);
end;

procedure TfrmMain.btnLaunchClick(Sender: TObject);
var
  LIcon: TJvIconKind;
  LImageIndex: Integer;
  LVisibleTime: Integer;
  LCtrl: TControl;
begin
  if (rgrDefaultIcon.ItemIndex >= 0) then
    LIcon := TJvIconKind(rgrDefaultIcon.ItemIndex)
  else
    LIcon := ikNone;
  LImageIndex := -1;
  if LIcon = ikCustom then
  begin
    LImageIndex := ilbDefaultImageIndex.ItemIndex;
    if LImageIndex < 0 then
      LIcon := ikNone
  end;

  with cmbAnchorCtrl do
    if ItemIndex >= 0 then
      LCtrl := TControl(Items.Objects[ItemIndex])
    else
      LCtrl := nil;

  LVisibleTime := StrToIntDef(edtVisibleTime.Text, 5000);

  case LIcon of
    ikNone:
      JvBalloonHint1.ActivateHint(LCtrl, memMessage.Text, edtHeader.Text, LVisibleTime);
    ikCustom:
      JvBalloonHint1.ActivateHint(LCtrl, memMessage.Text, LImageIndex, edtHeader.Text,
        LVisibleTime);
  else
    JvBalloonHint1.ActivateHint(LCtrl, memMessage.Text, LIcon, edtHeader.Text, LVisibleTime);
  end;
end;

procedure TfrmMain.rgrCustomAnimationStyleClick(Sender: TObject);
begin
  with rgrCustomAnimationStyle do
    if ItemIndex >= 0 then
      JvBalloonHint1.CustomAnimationStyle := TJvAnimationStyle(ItemIndex);
end;

procedure TfrmMain.edtCustomAnimationTimeChange(Sender: TObject);
begin
  JvBalloonHint1.CustomAnimationTime := StrToIntDef(edtCustomAnimationTime.Text, 0);
end;

procedure TfrmMain.chbUseDefaultHeaderClick(Sender: TObject);
begin
  if chbUseDefaultHeader.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boUseDefaultHeader]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boUseDefaultHeader];
end;

procedure TfrmMain.chbUseDefaultIconClick(Sender: TObject);
begin
  if chbUseDefaultIcon.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boUseDefaultIcon]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boUseDefaultIcon];
end;

procedure TfrmMain.chbUseDefaultImageIndexClick(Sender: TObject);
begin
  if chbUseDefaultImageIndex.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boUseDefaultImageIndex]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boUseDefaultImageIndex];
end;

procedure TfrmMain.chbShowCloseBtnClick(Sender: TObject);
begin
  if chbShowCloseBtn.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boShowCloseBtn]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boShowCloseBtn];
end;

procedure TfrmMain.chbCustomAnimationClick(Sender: TObject);
begin
  if chbCustomAnimation.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boCustomAnimation]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boCustomAnimation];
end;

procedure TfrmMain.chbO_PlaySoundClick(Sender: TObject);
begin
  if chbO_PlaySound.Checked then
    JvBalloonHint1.Options := JvBalloonHint1.Options + [boPlaySound]
  else
    JvBalloonHint1.Options := JvBalloonHint1.Options - [boPlaySound];
end;

procedure TfrmMain.FillAnchors(Strings: TStrings);

  procedure AddCtrl(ACtrl: TControl);
  begin
    if ACtrl.Name > '' then
      Strings.AddObject(ACtrl.Name, ACtrl);
  end;

  procedure AddControls(AWinCtrl: TWinControl);
  var
    I: Integer;
  begin
    with AWinCtrl do
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TWinControl then
          AddControls(TWinControl(Controls[I]))
        else
          AddCtrl(Controls[I]);
    AddCtrl(AWinCtrl);
  end;
begin
  with Strings do
  begin
    BeginUpdate;
    try
      Clear;
      AddControls(Self);
    finally
      EndUpdate;
    end;
  end;
end;

end.

