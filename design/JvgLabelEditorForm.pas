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
unit JvgLabelEditorForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ExtDlgs, ExtCtrls, StdCtrls,
  Buttons, ComCtrls,
{$IFDEF COMPILER6_UP}
  DesignIntf,
  DesignEditors,
  PropertyCategories,
{$ELSE}
  DsgnIntf,
{$ENDIF COMPILER6_UP}
  JvCompEditorTemplateForm, JvgSpeedButton, JvgPage, JvgTypes,
  JvgSplit, JvgWizardHeader, JvgShadow,
  JvgGroupBox, JvgLabel, JvgCheckBox,  JvgImage;

type
  TJvgLabelEditorDlg = class(TJvgCompEditorTemplate)
    pnlPanel1: TPanel;
    JvgLabelTest: TJvgLabel;
    splJvgSplitter1: TJvgSplitter;
    tbsTabSheet1: TTabSheet;
    OpenPictureDialog1: TOpenPictureDialog;
    JvColorDialog1: TColorDialog;
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
    cbtnGradientColorTo: TComboBox;
    cbtnGradientColorFrom: TComboBox;
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
    cbtnText: TComboBox;
    cbtnTextActive: TComboBox;
    cbtnTextDisabled: TComboBox;
    cbtnHighlight: TComboBox;
    cbtnShadow: TComboBox;
    cbtnBackground: TComboBox;
    cbtnBackgroundActive: TComboBox;
    cbtnDelineate: TComboBox;
    cbtnDelineateActive: TComboBox;
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
    cbxFont: TComboBox;
    imglBrushes: TImageList;
    cbxBrushStyle: TComboBox;
    procedure JvgCheckBox13AfterPaint(Sender: TObject);
    procedure tbarFontSizeChange(Sender: TObject);
    procedure cbxBoldClick(Sender: TObject);
    procedure cbxItalicClick(Sender: TObject);
    procedure cbxUnderlineClick(Sender: TObject);
    procedure cbxStrikeOutClick(Sender: TObject);
    procedure cbxActiveNormalClick(Sender: TObject);
    procedure cbxPassiveNormalClick(Sender: TObject);
    procedure cbxDisabledNormalClick(Sender: TObject);
    procedure cLabelColorsChange(Sender: TObject);
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
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FColorItems: TStrings;
    FLabelSource: TJvgLabel;
    FCurrentPassiveCheckBox: TJvgCheckBox;
    FCurrentActiveCheckBox: TJvgCheckBox;
    FCurrentDisabledCheckBox: TJvgCheckBox;
    procedure LabelChanged;
    procedure SetTextStyle(TextKind: integer; TextStyle: TglTextStyle);
    procedure GetColorValuesProc(const S: string);
    procedure SetColors(Items: TStrings);
    function SetItemsColor(Items: TStrings; AColor: TColor): integer;
    function GetItemsColor(Items: TStrings; ItemIndex: integer; ADefault: TColor): TColor;
  protected
    function UpdateComponent: boolean; override;
    procedure InitializeEditor; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent; LabelSource: TJvgLabel); reintroduce; overload;
    procedure AfterConstruction; override;
  end;

  TJvgLabelEditor = class(TComponentEditor)
    function GetVerbCount: integer; override;
    function GetVerb(Index: integer): string; override;
    procedure ExecuteVerb(Index: integer); override;
  end;

var
  JvgLabelEditorDlg: TJvgLabelEditorDlg;


resourcestring
  sEditLabel = 'Edit &Label...';

implementation

{$R *.dfm}
{$R ..\Resources\JvgLabelEditorForm.res}

procedure TJvgLabelEditor.ExecuteVerb(Index: integer);
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

function TJvgLabelEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0: Result := sEditLabel;
  end;
end;

function TJvgLabelEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

procedure TJvgLabelEditorDlg.JvgCheckBox13AfterPaint(Sender: TObject);
begin
  inherited;
  if Sender is TJvgCheckBox then
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
  if cbxBold.checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsBold]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsBold];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxItalicClick(Sender: TObject);
begin
  inherited;
  if cbxItalic.checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsItalic]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsItalic];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxUnderlineClick(Sender: TObject);
begin
  inherited;
  if cbxUnderline.checked then
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style + [fsUnderline]
  else
    JvgLabelTest.Font.Style := JvgLabelTest.Font.Style - [fsUnderline];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxStrikeOutClick(Sender: TObject);
begin
  inherited;
  if cbxStrikeOut.checked then
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
  if Sender is TJvgCheckBox then
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
  else
    TextStyle := fstNone;
  end; // case

  SetTextStyle(0, TextStyle);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.SetTextStyle(TextKind: integer; TextStyle: TglTextStyle);
begin
  case integer(TextKind) of //
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
  if Sender is TJvgCheckBox then
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
  else
    TextStyle := fstNone;
  end; // case

  SetTextStyle(1, TextStyle);

  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxDisabledNormalClick(Sender: TObject);
var
  TextStyle: TglTextStyle;
begin
  inherited;
  if Sender is TJvgCheckBox then
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
  else
    TextStyle := fstNone;
  end; // case

  SetTextStyle(2, TextStyle);

  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cLabelColorsChange(Sender: TObject);

begin
  inherited;
  with TComboBox(Sender) do
  begin
    case Tag of //
      1: JvgLabelTest.Colors.Text := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.Text);
      2: JvgLabelTest.Colors.TextActive := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.TextActive);
      3: JvgLabelTest.Colors.TextDisabled := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.TextDisabled);
      4: JvgLabelTest.Colors.HighLight := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.HighLight);
      5: JvgLabelTest.Colors.Shadow := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.Shadow);
      6: JvgLabelTest.Colors.Background := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.Background);
      7: JvgLabelTest.Colors.BackgroundActive := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.BackgroundActive);
      8: JvgLabelTest.Colors.Delineate := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.Delineate);
      9: JvgLabelTest.Colors.DelineateActive := GetItemsColor(Items, ItemIndex, JvgLabelTest.Colors.DelineateActive);
    end; // case
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbarGradientPercentFillingChange(Sender: TObject);
begin
  inherited;
  if Sender is TScrollBar then
  begin
    JvgLabelTest.Gradient.PercentFilling := TScrollBar(Sender).Position;
    lblGradientPercentFilling.Caption := IntToStr(TScrollBar(Sender).Position);
    LabelChanged;
  end;

end;

procedure TJvgLabelEditorDlg.cbtnGradientColorFromChange(Sender: TObject);
begin
  inherited;
  with TComboBox(Sender)do
    case Tag of //
    1: JvgLabelTest.Gradient.FromColor := GetItemsColor(Items, ItemIndex, JvgLabelTest.Gradient.FromColor);
    2: JvgLabelTest.Gradient.ToColor := GetItemsColor(Items, ItemIndex, JvgLabelTest.Gradient.ToColor);
  end; // case
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientActiveClick(Sender: TObject);
begin
  inherited;
  JvgLabelTest.Gradient.Active := TJvgCheckBox(Sender).checked;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientBufferedDrawClick(Sender: TObject);
begin
  inherited;
  JvgLabelTest.Gradient.BufferedDraw := TJvgCheckBox(Sender).checked;
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
  JvgLabelTest.Direction := TglLabelDir(TJvgSpeedButton(Sender).tag);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxBrushStyleChange(Sender: TObject);
begin
  JvgLabelTest.Gradient.BrushStyle := TBrushStyle(TComboBox(Sender).ItemIndex);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.btnNewTextureImageClick(Sender: TObject);
begin
  inherited;
  if OpenPictureDialog1.Execute then
  begin
    case TSpeedButton(Sender).tag of //
      0:
        begin
          imgBackground.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveBackground.checked then
            JvgLabelTest.Background := imgBackground.Picture.Bitmap;
        end;
      1:
        begin
          imgTexture.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveTexture.checked then
            JvgLabelTest.Texture := imgTexture.Picture.Bitmap;
        end;
    end; // case
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxActiveBackgroundClick(Sender: TObject);
begin
  case TJvgCheckBox(Sender).tag of //
    0:
      if TJvgCheckBox(Sender).checked then
      begin
        JvgLabelTest.Background := imgBackground.Picture.Bitmap;
      end
      else
        JvgLabelTest.Background := nil;
    1:
      if TJvgCheckBox(Sender).checked then
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
  JvgLabelTest.Font.Name := TFontName(cbxFont.Items[cbxFont.ItemIndex]);
  LabelChanged;
end;

function TJvgLabelEditorDlg.UpdateComponent: boolean;
begin

  inherited UpdateComponent;
  if (JvgLabelTest.Background <> nil) and (cbxActiveBackground.checked) then
  begin
    FLabelSource.Background.Assign(JvgLabelTest.Background);
  end
  else
    FLabelSource.Background := nil;

  if (imgTexture.Picture <> nil) and (cbxActiveTexture.checked) then
  begin
    FLabelSource.Texture.Assign(JvgLabelTest.Texture);
  end
  else
    FLabelSource.Texture := nil;

  FLabelSource.Colors.Background := JvgLabelTest.Colors.Background;
  FLabelSource.Colors.BackgroundActive := JvgLabelTest.Colors.BackgroundActive;
  FLabelSource.Colors.Delineate := JvgLabelTest.Colors.Delineate;
  FLabelSource.Colors.DelineateActive := JvgLabelTest.Colors.DelineateActive;
  FLabelSource.Colors.HighLight := JvgLabelTest.Colors.HighLight;
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
  Result := true;
end;

constructor TJvgLabelEditorDlg.Create(AOwner: TComponent; LabelSource: TJvgLabel);
begin
  inherited Create(AOwner);
  FLabelSource := LabelSource;

end;

procedure TJvgLabelEditorDlg.InitializeEditor;

  procedure LoadGlyph(CheckBoxGlyph: TBitmap; Glyph: string);
  begin
    CheckBoxGlyph.LoadFromResourceName(HInstance, Glyph);
  end;

  procedure LoadCheckBoxGlyph(CheckBox: TJvgCheckBox; GlyphKind: integer);
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

  // Square Checkboxes

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
  with FLabelSource.Colors do
  begin
    cbtnText.ItemIndex := SetItemsColor(cbtnText.Items, Text);
    cbtnTextActive.ItemIndex := SetItemsColor(cbtnTextActive.Items, TextActive);
    cbtnTextDisabled.ItemIndex := SetItemsColor(cbtnTextDisabled.Items, TextDisabled);
    cbtnHighlight.ItemIndex := SetItemsColor(cbtnHighlight.Items, HighLight);
    cbtnShadow.ItemIndex := SetItemsColor(cbtnShadow.Items, Shadow);
    cbtnBackground.ItemIndex := SetItemsColor(cbtnBackground.Items, Background);
    cbtnBackgroundActive.ItemIndex := SetItemsColor(cbtnBackgroundActive.Items, BackgroundActive);
    cbtnDelineate.ItemIndex := SetItemsColor(cbtnDelineate.Items, Delineate);
    cbtnDelineateActive.ItemIndex := SetItemsColor(cbtnDelineateActive.Items, DelineateActive);
  end;
  cbxBold.checked := fsBold in FLabelSource.Font.Style;
  cbxItalic.checked := fsItalic in FLabelSource.Font.Style;
  cbxUnderline.checked := fsUnderline in FLabelSource.Font.Style;
  cbxStrikeOut.checked := fsStrikeOut in FLabelSource.Font.Style;

  case integer(FLabelSource.TextStyles.Active) of //
    0: cbxActiveNormal.checked := true;
    3: cbxActivePushed.checked := true;
    1: cbxActiveRaised.checked := true;
    4: cbxActiveShadow.checked := true;
    2: cbxActiveRecessed.checked := true;
    5: cbxActiveVolumetric.checked := true;
  end; // case

  case integer(FLabelSource.TextStyles.Passive) of //
    0: cbxPassiveNormal.checked := true;
    3: cbxPassivePushed.checked := true;
    1: cbxPassiveRaised.checked := true;
    4: cbxPassiveShadow.checked := true;
    2: cbxPassiveRecessed.checked := true;
    5: cbxPassiveVolumentric.checked := true;
  end; // case

  case integer(FLabelSource.TextStyles.Disabled) of //
    0: cbxDisabledNormal.checked := true;
    3: cbxDisabledPushed.checked := true;
    1: cbxDisabledRaised.checked := true;
    4: cbxDisabledShadow.checked := true;
    2: cbxDisabledRecessed.checked := true;
    5: cbxDisabledVolumentric.checked := true;
  end; // case

  cbxGradientActive.checked := FLabelSource.Gradient.Active;
  cbxGradientBufferedDraw.checked := FLabelSource.Gradient.BufferedDraw;
  cbxBrushStyle.ItemIndex := integer(FLabelSource.Gradient.BrushStyle);

  cbtnGradientColorFrom.ItemIndex := SetItemsColor(cbtnGradientColorFrom.Items,FLabelSource.Gradient.FromColor);
  cbtnGradientColorTo.ItemIndex := SetItemsColor(cbtnGradientColorFrom.Items,FLabelSource.Gradient.ToColor);
  cbxGradientOrientation.ItemIndex := integer(FLabelSource.Gradient.Orientation);
  sbarGradientPercentFilling.Position := integer(FLabelSource.Gradient.PercentFilling);
  cbxFont.ItemIndex := cbxFont.Items.IndexOf(FLabelSource.Font.Name);

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

procedure TJvgLabelEditorDlg.FormCreate(Sender: TObject);
begin
  inherited;
  cbxFont.Items := Screen.Fonts;
  cbxFont.Sorted := true;
end;

procedure TJvgLabelEditorDlg.AfterConstruction;
var
  i: integer;
begin
  inherited;
  for i := 0 to ComponentCount - 1 do
    if (Components[i].Tag > 0) and (Components[i] is TComboBox) then
      SetColors(TComboBox(Components[i]).Items);
end;

procedure TJvgLabelEditorDlg.GetColorValuesProc(const S: string);
var
  AColor: Longint;
begin
  if FColorItems <> nil then
  begin
    IdentToColor(S, AColor);
    FColorItems.AddObject(Copy(S, 3, MaxInt), TObject(AColor));
  end;
end;

procedure TJvgLabelEditorDlg.SetColors(Items: TStrings);
begin
  FColorItems := Items;
  try
    GetColorValues(GetColorValuesProc);
  finally
    FColorItems := nil;
  end;
end;

function TJvgLabelEditorDlg.SetItemsColor(Items: TStrings;
  AColor: TColor): integer;
var
  AIdent: string;
begin
  Result := Items.IndexOfObject(TObject(AColor));
  if Result < 0 then
  begin
    ColorToIdent(AColor, AIdent);
    Result := Items.IndexOf(Copy(AIdent, 3, MaxInt));
  end;
end;

function TJvgLabelEditorDlg.GetItemsColor(Items: TStrings;
  ItemIndex: integer; ADefault: TColor): TColor;
begin
  if ItemIndex < 0 then
    Result := ADefault
  else
    Result := TColor(Items.Objects[ItemIndex]);
end;

end.

