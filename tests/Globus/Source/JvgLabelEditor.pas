{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLabelEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Michael Beck [mbeck@bigfoot.com]
Portions created by Michael Beck are Copyright (C) 2003 Michael Beck
All Rights Reserved.

Contributor(s):

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
unit JvgLabelEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvCompEditorTemplate, Buttons, JvgSpeedButton, ComCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}

  JvgPage, JvgTypes,
  ExtCtrls, JvgSplit, JvComponent, JvgWizardHeader, JvgShadow, StdCtrls,
  JvgGroupBox, JvgLabel, JvgCheckBox, JvComCtrls,
  JvColorBox, JvColorBtn, JvDialogs, JvCombobox, JvColorCombo, ImgList,
  JvxSlider, JvImageWindow, JvBaseDlg, JvImageDlg, JvListComb, JvImage,
  ExtDlgs, JvgImage;

type
  TJvgLabelEditorDlg = class(TJvgCompEditorTemplate)
    pnlPanel1: TPanel;
    JvgLabelTest: TJvgLabel;
    splJvgSplitter1: TJvgSplitter;
    tbsTabSheet1: TTabSheet;
    OpenPictureDialog1: TOpenPictureDialog;
    cdlgJvColorDialog1: TJvColorDialog;
    gbxGradient: TJvgGroupBox;
    JvgLabel11: TJvgLabel;
    lblGradientPercentFilling: TJvgLabel;
    JvgLabel12: TJvgLabel;
    JvgLabel1: TJvgLabel;
    JvgLabel6: TJvgLabel;
    JvgLabel9: TJvgLabel;
    cbxGradientActive: TJvgCheckBox;
    cbxGradientBufferedDraw: TJvgCheckBox;
    cbxGradientOrientation: TComboBox;
    cbtnGradientColorTo: TJvColorButton;
    cbtnGradientColorFrom: TJvColorButton;
    cbxBrushStyle: TJvImageComboBox;
    sbarGradientPercentFilling: TScrollBar;
    JvgGroupBox3: TJvgGroupBox;
    JvgShadow1: TJvgShadow;
    sbtnBtnNewBackgroundImage: TSpeedButton;
    bvlBevel1: TBevel;
    imgBackground: TImage;
    cbxActiveBackground: TJvgCheckBox;
    JvgGroupBox6: TJvgGroupBox;
    JvgShadow2: TJvgShadow;
    sbtnBtnNewTextureImage: TSpeedButton;
    bvlBevel2: TBevel;
    imgTexture: TImage;
    cbxActiveTexture: TJvgCheckBox;
    JvgGroupBox2: TJvgGroupBox;
    JvgLabel20: TJvgLabel;
    JvgLabel19: TJvgLabel;
    JvgLabel18: TJvgLabel;
    JvgLabel17: TJvgLabel;
    JvgLabel16: TJvgLabel;
    JvgLabel15: TJvgLabel;
    JvgLabel14: TJvgLabel;
    JvgLabel13: TJvgLabel;
    JvgLabel10: TJvgLabel;
    cbtnText: TJvColorButton;
    cbtnTextActive: TJvColorButton;
    cbtnTextDisabled: TJvColorButton;
    cbtnHighlight: TJvColorButton;
    cbtnShadow: TJvColorButton;
    cbtnBackground: TJvColorButton;
    cbtnBackgroundActive: TJvColorButton;
    cbtnDelineate: TJvColorButton;
    cbtnDelineateActive: TJvColorButton;
    gbxPassiveStyle: TJvgGroupBox;
    cbxPassiveNormal: TJvgCheckBox;
    cbxPassivePushed: TJvgCheckBox;
    cbxPassiveRaised: TJvgCheckBox;
    cbxPassiveShadow: TJvgCheckBox;
    cbxPassiveRecessed: TJvgCheckBox;
    cbxPassiveVolumentric: TJvgCheckBox;
    gbxActiveStyle: TJvgGroupBox;
    cbxActiveNormal: TJvgCheckBox;
    cbxActivePushed: TJvgCheckBox;
    cbxActiveRaised: TJvgCheckBox;
    cbxActiveShadow: TJvgCheckBox;
    cbxActiveRecessed: TJvgCheckBox;
    cbxActiveVolumetric: TJvgCheckBox;
    gbxDisabledStyle: TJvgGroupBox;
    cbxDisabledNormal: TJvgCheckBox;
    cbxDisabledPushed: TJvgCheckBox;
    cbxDisabledRaised: TJvgCheckBox;
    cbxDisabledShadow: TJvgCheckBox;
    cbxDisabledRecessed: TJvgCheckBox;
    cbxDisabledVolumentric: TJvgCheckBox;
    JvgGroupBox1: TJvgGroupBox;
    cbxBold: TJvgCheckBox;
    cbxItalic: TJvgCheckBox;
    cbxUnderline: TJvgCheckBox;
    cbxStrikeOut: TJvgCheckBox;
    gbxLabelDirection: TJvgGroupBox;
    sbtnLabelDirectionDown: TJvgSpeedButton;
    sbtnLabelDirectionUp: TJvgSpeedButton;
    JvgLabel5: TJvgLabel;
    sbtnLabelDirectionLeft: TJvgSpeedButton;
    sbtnLabelDirectionRight: TJvgSpeedButton;
    JvgGroupBox4: TJvgGroupBox;
    lblFontSize: TJvgLabel;
    lblShadowDepth: TJvgLabel;
    JvgLabel2: TJvgLabel;
    JvgLabel3: TJvgLabel;
    sbarShadowDepth: TScrollBar;
    sbarFontSize: TScrollBar;
    cbxFont: TJvFontComboBox;
    imglBrushes: TImageList;
    procedure JvgCheckBox13AfterPaint(Sender: TObject);
    procedure tbarFontSizeChange(Sender: TObject);
    procedure cbxBoldClick(Sender: TObject);
    procedure cbxItalicClick(Sender: TObject);
    procedure cbxUnderlineClick(Sender: TObject);
    procedure cbxStrikeOutClick(Sender: TObject);
    procedure cbxActiveNormalClick(Sender: TObject);
    procedure cbxPassiveNormalClick(Sender: TObject);
    procedure cbxDisabledNormalClick(Sender: TObject);
    procedure cbtnDelineateActiveChange(Sender: TObject);
    procedure sbarGradientPercentFillingChange(Sender: TObject);
    procedure cbtnGradientColorFromChange(Sender: TObject);
    procedure cbxGradientActiveClick(Sender: TObject);
    procedure cbxGradientBufferedDrawClick(Sender: TObject);
    procedure cbxGradientOrientationChange(Sender: TObject);
    procedure sbtnLabelDirectionRightClick(Sender: TObject);
    procedure cbxBrushStyleChange(Sender: TObject);
    procedure btnNewTextureImageClick(Sender: TObject);
    procedure cbxActiveBackgroundClick(Sender: TObject);
    procedure sbarShadowDepthChange(Sender: TObject);
    procedure cbxFontChange(Sender: TObject);
  private
    { Private declarations }
    FLabelSource: TJvgLabel;
    FCurrentPassiveCheckBox: TJvgCheckBox;
    FCurrentActiveCheckBox: TJvgCheckBox;
    FCurrentDisabledCheckBox: TJvgCheckBox;
    procedure LabelChanged;
    procedure SetTextStyle(TextKind: Integer; TextStyle: TglTextStyle);
  protected
    function UpdateComponent: Boolean; override;
    procedure InitializeEditor; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; LabelSource: TJvgLabel); reintroduce; overload;
  end;

  TJvgLabelEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

var
  JvgLabelEditorDlg: TJvgLabelEditorDlg;

implementation

{$R *.dfm}
{$R ..\Resources\JvgLabelEditor.res}

procedure TJvgLabelEditor.ExecuteVerb(Index: Integer);
var
  EditorDlg: TJvgLabelEditorDlg;
begin

  inherited;
  case Index of
    0:
      begin
        EditorDlg := TJvgLabelEditorDlg.Create(Application, Component as TJvgLabel);
        try
          if EditorDlg.ShowModal = mrOK then
          begin
            Designer.Modified;
          end
          else
        finally
          EditorDlg.Free;
        end;
      end;
  end;
end;

function TJvgLabelEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Edit &Label...';
  end;
end;

function TJvgLabelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TJvgLabelEditorDlg.JvgCheckBox13AfterPaint(Sender: TObject);
begin
  inherited;
  if sender is TJvgCheckBox then
    if TJvgCheckBox(Sender).checked then
      TJvgCheckBox(Sender).TextStyles.Passive := fstNone
    else
      TJvgCheckBox(Sender).TextStyles.Passive := fstPushed;
end;

procedure TJvgLabelEditorDlg.tbarFontSizeChange(Sender: TObject);
begin
  JvgLabelTest.Font.Size := sbarFontSize.Position;
  lblFontSize.Caption := IntToStr(sbarFontSize.Position);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.LabelChanged;
begin
  JvgLabelTest.Invalidate;
end;

procedure TJvgLabelEditorDlg.cbxBoldClick(Sender: TObject);
begin
  if cbxBold.Checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsBold]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsBold];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxItalicClick(Sender: TObject);
begin
  inherited;
  if cbxItalic.Checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsItalic]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsItalic];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxUnderlineClick(Sender: TObject);
begin
  inherited;
  if cbxUnderline.Checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsUnderline]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsUnderline];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxStrikeOutClick(Sender: TObject);
begin
  inherited;
  if cbxStrikeout.Checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsStrikeOut]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsStrikeOut];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxActiveNormalClick(Sender: TObject);
var
  TextStyle: TglTextStyle;
begin
  inherited;
  if sender is TJvgCheckBox then
  begin
    if FCurrentActiveCheckBox <> nil then
    begin
      FCurrentActiveCheckBox.TextStyles.Passive := fstPushed;
      FCurrentActiveCheckBox.Invalidate;
    end;
    TJvgCheckBox(Sender).TextStyles.Passive := fstRaised;
    FCurrentActiveCheckBox := TJvgCheckBox(Sender);
  end;

  case TJvgCheckBox(Sender).tag of //
    0: TextStyle := fstNone;
    1: TextStyle := fstPushed;
    2: TextStyle := fstRaised;
    3: TextStyle := fstShadow;
    4: TextStyle := fstRecessed;
    5: TextStyle := fstVolumetric;
  end; // case

  SetTextStyle(0, TextStyle);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.SetTextStyle(TextKind: Integer; TextStyle: TglTextStyle);
begin
  case Integer(TextKind) of //
    0: JvgLabelTest.TextStyles.Active := TextStyle;
    1: JvgLabelTest.TextStyles.Disabled := TextStyle;
    2: JvgLabelTest.TextStyles.Passive := TextStyle;
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxPassiveNormalClick(Sender: TObject);
var
  TextStyle: TglTextStyle;
begin
  inherited;
  if sender is TJvgCheckBox then
  begin
    if FCurrentPassiveCheckBox <> nil then
    begin
      FCurrentPassiveCheckBox.TextStyles.Passive := fstPushed;
      FCurrentPassiveCheckBox.Invalidate;
    end;
    TJvgCheckBox(Sender).TextStyles.Passive := fstRaised;
    FCurrentPassiveCheckBox := TJvgCheckBox(Sender);
  end;

  case TJvgCheckBox(Sender).tag of //
    0: TextStyle := fstNone;
    1: TextStyle := fstPushed;
    2: TextStyle := fstRaised;
    3: TextStyle := fstShadow;
    4: TextStyle := fstRecessed;
    5: TextStyle := fstVolumetric;
  end; // case

  SetTextStyle(1, TextStyle);

  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxDisabledNormalClick(Sender: TObject);
var
  TextStyle: TglTextStyle;
begin
  inherited;
  if sender is TJvgCheckBox then
  begin
    if FCurrentDisabledCheckBox <> nil then
    begin
      FCurrentDisabledCheckBox.TextStyles.Passive := fstPushed;
      FCurrentDisabledCheckBox.Invalidate;
    end;
    TJvgCheckBox(Sender).TextStyles.Passive := fstRaised;
    FCurrentDisabledCheckBox := TJvgCheckBox(Sender);
  end;

  case TJvgCheckBox(Sender).tag of //
    0: TextStyle := fstNone;
    1: TextStyle := fstPushed;
    2: TextStyle := fstRaised;
    3: TextStyle := fstShadow;
    4: TextStyle := fstRecessed;
    5: TextStyle := fstVolumetric;
  end; // case

  SetTextStyle(2, TextStyle);

  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbtnDelineateActiveChange(Sender: TObject);
begin
  inherited;
  case TJvColorButton(Sender).tag of //
    0: JvgLabelTest.Colors.Text := TJvColorButton(Sender).color;
    1: JvgLabelTest.Colors.TextActive := TJvColorButton(Sender).color;
    2: JvgLabelTest.Colors.TextDisabled := TJvColorButton(Sender).color;
    3: JvgLabelTest.Colors.Highlight := TJvColorButton(Sender).color;
    4: JvgLabelTest.Colors.Shadow := TJvColorButton(Sender).color;
    5: JvgLabelTest.Colors.Background := TJvColorButton(Sender).color;
    6: JvgLabelTest.Colors.BackgroundActive := TJvColorButton(Sender).color;
    7: JvgLabelTest.Colors.Delineate := TJvColorButton(Sender).color;
    8: JvgLabelTest.Colors.DelineateActive := TJvColorButton(Sender).color;
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbarGradientPercentFillingChange(Sender: TObject);
begin
  inherited;
  if sender is TScrollBar then
  begin
    JvgLabelTest.Gradient.PercentFilling := TScrollBar(Sender).Position;
    lblGradientPercentFilling.Caption := IntToStr(TScrollBar(Sender).Position);
    LabelChanged;
  end;

end;

procedure TJvgLabelEditorDlg.cbtnGradientColorFromChange(Sender: TObject);
begin
  inherited;
  case TJvColorButton(Sender).tag of //
    1: JvgLabelTest.Gradient.FromColor := TJvColorButton(Sender).color;
    2: JvgLabelTest.Gradient.ToColor := TJvColorButton(Sender).color;
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientActiveClick(Sender: TObject);
begin
  inherited;
  JvgLabelTest.Gradient.Active := TJvgCheckBox(Sender).Checked;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientBufferedDrawClick(Sender: TObject);
begin
  inherited;
  JvgLabelTest.Gradient.BufferedDraw := TJvgCheckBox(Sender).Checked;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientOrientationChange(Sender: TObject);
begin
  inherited;

  case TComboBox(Sender).ItemIndex of //
    0: JvgLabelTest.Gradient.Orientation := fgdHorizontal;
    1: JvgLabelTest.Gradient.Orientation := fgdVertical;
    2: JvgLabelTest.Gradient.Orientation := fgdLeftBias;
    3: JvgLabelTest.Gradient.Orientation := fgdRightBias;
    4: JvgLabelTest.Gradient.Orientation := fgdRectangle;
    5: JvgLabelTest.Gradient.Orientation := fgdVertConvergent;
    6: JvgLabelTest.Gradient.Orientation := fgdVertConvergent;
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbtnLabelDirectionRightClick(Sender: TObject);
begin
  JvgLabelTest.Direction := TglLabelDir(TJvgSpeedButton(Sender).Tag);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxBrushStyleChange(Sender: TObject);
begin
  JvgLabelTest.Gradient.BrushStyle := TBrushStyle(TJvImageComboBox(Sender).ItemIndex);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.btnNewTextureImageClick(Sender: TObject);
begin
  inherited;
  if OpenPictureDialog1.Execute then
  begin
    case TSpeedButton(Sender).Tag of //
      0:
        begin
          imgBackground.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveBackground.Checked then
            JvgLabelTest.Background := imgBackground.Picture.Bitmap;
        end;
      1:
        begin
          imgTexture.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveTexture.Checked then
            JvgLabelTest.Texture := imgTexture.Picture.Bitmap;
        end;
    end; // case
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxActiveBackgroundClick(Sender: TObject);
begin
  case TJvgCheckBox(Sender).Tag of //
    0:
      if TJvgCheckBox(Sender).Checked then
      begin
        JvgLabelTest.Background := imgBackground.Picture.Bitmap;
      end
      else
        JvgLabelTest.Background := nil;
    1:
      if TJvgCheckBox(Sender).Checked then
      begin
        JvgLabelTest.Texture := imgTexture.Picture.Bitmap;
      end
      else
        JvgLabelTest.Texture := nil;
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbarShadowDepthChange(Sender: TObject);
begin
  JvgLabelTest.Illumination.ShadowDepth := sbarShadowDepth.Position;
  lblShadowDepth.Caption := IntToStr(sbarShadowDepth.Position);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxFontChange(Sender: TObject);
begin
  JvgLabelTest.Font.Name := TFontName(cbxFont.FontName);
  LabelChanged;
end;

function TJvgLabelEditorDlg.UpdateComponent: Boolean;
begin

  inherited UpdateComponent;
  Result := False;

  if (JvgLabelTest.Background <> nil) and (cbxActiveBackground.Checked) then
  begin
    FLabelSource.Background.Assign(JvgLabelTest.Background);
  end
  else
    FLabelSource.Background := nil;

  if (imgTexture.Picture <> nil) and (cbxActiveTexture.Checked) then
  begin
    FLabelSource.Texture.Assign(JvgLabelTest.Texture);
  end
  else
    FLabelSource.Texture := nil;

  FLabelSource.Colors.Background := JvgLabelTest.Colors.Background;
  FLabelSource.Colors.BackgroundActive := JvgLabelTest.Colors.BackgroundActive;
  FLabelSource.Colors.Delineate := JvgLabelTest.Colors.Delineate;
  FLabelSource.Colors.DelineateActive := JvgLabelTest.Colors.DelineateActive;
  FLabelSource.Colors.Highlight := JvgLabelTest.Colors.Highlight;
  FLabelSource.Colors.Shadow := JvgLabelTest.Colors.Shadow;
  FLabelSource.Colors.Text := JvgLabelTest.Colors.Text;
  FLabelSource.Colors.TextActive := JvgLabelTest.Colors.TextActive;
  FLabelSource.Colors.TextDisabled := JvgLabelTest.Colors.TextDisabled;

  FLabelSource.Direction := JvgLabelTest.Direction;

  FLabelSource.Font.Name := JvgLabelTest.Font.Name;
  FLabelSource.Font.Size := JvgLabelTest.Font.Size;
  FLabelSource.Font.Style := JvgLabelTest.Font.Style;

  FLabelSource.Gradient.Active := JvgLabelTest.Gradient.Active;
  FLabelSource.Gradient.BrushStyle := JvgLabelTest.Gradient.BrushStyle;
  FLabelSource.Gradient.BufferedDraw := JvgLabelTest.Gradient.BufferedDraw;
  FLabelSource.Gradient.FromColor := JvgLabelTest.Gradient.FromColor;
  FLabelSource.Gradient.Orientation := JvgLabelTest.Gradient.Orientation;
  FLabelSource.Gradient.PercentFilling := JvgLabelTest.Gradient.PercentFilling;
  FLabelSource.Gradient.ToColor := JvgLabelTest.Gradient.ToColor;

  FLabelSource.Illumination.ShadowDepth := JvgLabelTest.Illumination.ShadowDepth;
  FLabelSource.TextStyles.Active := JvgLabelTest.TextStyles.Active;
  FLabelSource.TextStyles.Disabled := JvgLabelTest.TextStyles.Disabled;
  FLabelSource.TextStyles.Passive := JvgLabelTest.TextStyles.Passive;
  Result := True;
end;

constructor TJvgLabelEditorDlg.Create(AOwner: TComponent; LabelSource: TJvgLabel);
begin
  inherited Create(AOwner);
  FLabelSource := LabelSource;

end;

procedure TJvgLabelEditorDlg.InitializeEditor;

  procedure LoadGlyph(CheckBoxGlyph: TBitMap; Glyph: string);
  begin
    CheckBoxGlyph.LoadFromResourceName(HInstance, Glyph);
  end;

  procedure LoadCheckBoxGlyph(CheckBox: TJvgCheckBox; GlyphKind: Integer);
  begin
    case GlyphKind of
      0: //Square
        begin
          LoadGlyph(CheckBox.GlyphDisabled, 'GLYPHDISABLEDSQUARE');
          LoadGlyph(CheckBox.GlyphOff, 'GLYPHOFFSQUARE');
          LoadGlyph(CheckBox.GlyphOn, 'GLYPHONSQUARE');
        end;
      1: //Circle
        begin
          LoadGlyph(CheckBox.GlyphDisabled, 'GLYPHDISABLED');
          LoadGlyph(CheckBox.GlyphOff, 'GLYPHOFF');
          LoadGlyph(CheckBox.GlyphOn, 'GLYPHON');
        end;

    end; // case
  end;

begin
  inherited;
  // loading from Resource file save ~ 200K in DFM file
  imglBrushes.ResInstLoad(HInstance, rtBitmap, 'BRUSH', clFuchsia);
  sbtnLabelDirectionDown.Glyph.LoadFromResourceName(HInstance, 'DOWN');
  sbtnLabelDirectionUp.Glyph.LoadFromResourceName(HInstance, 'UP');
  sbtnLabelDirectionRight.Glyph.LoadFromResourceName(HInstance, 'RIGHT');
  sbtnLabelDirectionLeft.Glyph.LoadFromResourceName(HInstance, 'LEFT');

  //Square Checkboxes

  LoadCheckBoxGlyph(cbxBold, 0);
  LoadCheckBoxGlyph(cbxItalic, 0);
  LoadCheckBoxGlyph(cbxUnderline, 0);
  LoadCheckBoxGlyph(cbxStrikeOut, 0);
  LoadCheckBoxGlyph(cbxGradientActive, 0);
  LoadCheckBoxGlyph(cbxGradientBufferedDraw, 0);
  LoadCheckBoxGlyph(cbxActiveBackground, 0);
  LoadCheckBoxGlyph(cbxActiveTexture, 0);

  //circle Checkboxes

  LoadCheckBoxGlyph(cbxPassiveNormal, 1);
  LoadCheckBoxGlyph(cbxPassivePushed, 1);
  LoadCheckBoxGlyph(cbxPassiveRaised, 1);
  LoadCheckBoxGlyph(cbxPassiveShadow, 1);
  LoadCheckBoxGlyph(cbxPassiveRecessed, 1);
  LoadCheckBoxGlyph(cbxPassiveVolumentric, 1);

  LoadCheckBoxGlyph(cbxActiveNormal, 1);
  LoadCheckBoxGlyph(cbxActivePushed, 1);
  LoadCheckBoxGlyph(cbxActiveRaised, 1);
  LoadCheckBoxGlyph(cbxActiveShadow, 1);
  LoadCheckBoxGlyph(cbxActiveRecessed, 1);
  LoadCheckBoxGlyph(cbxActiveVolumetric, 1);

  LoadCheckBoxGlyph(cbxDisabledNormal, 1);
  LoadCheckBoxGlyph(cbxDisabledPushed, 1);
  LoadCheckBoxGlyph(cbxDisabledRaised, 1);
  LoadCheckBoxGlyph(cbxDisabledShadow, 1);
  LoadCheckBoxGlyph(cbxDisabledRecessed, 1);
  LoadCheckBoxGlyph(cbxDisabledVolumentric, 1);

  sbarFontSize.Position := FLabelSource.Font.Size;
  sbarShadowDepth.Position := FLabelSource.Illumination.ShadowDepth;

  cbtnText.Color := FLabelSource.Colors.Text;
  cbtnTextActive.Color := FLabelSource.Colors.TextActive;
  cbtnTextDisabled.Color := FLabelSource.Colors.TextDisabled;
  cbtnHighlight.Color := FLabelSource.Colors.Highlight;
  cbtnShadow.Color := FLabelSource.Colors.Shadow;
  cbtnBackground.Color := FLabelSource.Colors.Background;
  cbtnBackgroundActive.Color := FLabelSource.Colors.BackgroundActive;
  cbtnDelineate.Color := FLabelSource.Colors.Delineate;
  cbtnDelineateActive.Color := FLabelSource.Colors.DelineateActive;

  cbxFont.FontName := FLabelSource.Font.Name;

  cbxBold.Checked := fsBold in FLabelSource.Font.Style;
  cbxItalic.Checked := fsItalic in FLabelSource.Font.Style;
  cbxUnderline.Checked := fsUnderline in FLabelSource.Font.Style;
  cbxStrikeOut.Checked := fsStrikeOut in FLabelSource.Font.Style;

  case Integer(FLabelSource.TextStyles.Active) of //
    0: cbxActiveNormal.Checked := True;
    3: cbxActivePushed.Checked := True;
    1: cbxActiveRaised.Checked := True;
    4: cbxActiveShadow.Checked := True;
    2: cbxActiveRecessed.Checked := True;
    5: cbxActiveVolumetric.Checked := True;
  end; // case

  case Integer(FLabelSource.TextStyles.Passive) of //
    0: cbxPassiveNormal.Checked := True;
    3: cbxPassivePushed.Checked := True;
    1: cbxPassiveRaised.Checked := True;
    4: cbxPassiveShadow.Checked := True;
    2: cbxPassiveRecessed.Checked := True;
    5: cbxPassiveVolumentric.Checked := True;
  end; // case

  case Integer(FLabelSource.TextStyles.Disabled) of //
    0: cbxDisabledNormal.Checked := True;
    3: cbxDisabledPushed.Checked := True;
    1: cbxDisabledRaised.Checked := True;
    4: cbxDisabledShadow.Checked := True;
    2: cbxDisabledRecessed.Checked := True;
    5: cbxDisabledVolumentric.Checked := True;
  end; // case

  cbxGradientActive.Checked := FLabelSource.Gradient.Active;
  cbxGradientBufferedDraw.Checked := FLabelSource.Gradient.BufferedDraw;
  cbxBrushStyle.ItemIndex := Integer(FLabelSource.Gradient.BrushStyle);
  cbtnGradientColorFrom.Color := FLabelSource.Gradient.FromColor;
  cbtnGradientColorTo.Color := FLabelSource.Gradient.ToColor;
  cbxGradientOrientation.ItemIndex := Integer(FLabelSource.Gradient.Orientation);
  sbarGradientPercentFilling.Position := Integer(FLabelSource.Gradient.PercentFilling);

  if FLabelSource.Background <> nil then
  begin
    imgBackground.Picture.Bitmap.Assign(FLabelSource.Background);
    JvgLabelTest.Background.Assign(imgBackground.Picture.Bitmap);
  end;

  if FLabelSource.Texture <> nil then
  begin
    imgTexture.Picture.Bitmap.Assign(FLabelSource.Texture);
    JvgLabelTest.Texture.Assign(imgTexture.Picture.Bitmap);
  end;

  LabelChanged;
end;

end.

