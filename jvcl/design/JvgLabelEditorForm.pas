{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgLabelEditor.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Michael Beck [mbeck att bigfoot dott com]
Portions created by Michael Beck are Copyright (C) 2003 Michael Beck
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgLabelEditorForm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ExtDlgs, ExtCtrls, StdCtrls, Buttons, ComCtrls,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, PropertyCategories,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvgCompEditorTemplateForm, JvgTypes, JvgLabel;

type
  TJvgLabelEditorDlg = class(TJvgCompEditorTemplate)
    pnlPanel1: TPanel;
    splJvgSplitter1: TSplitter;
    tbsTabSheet1: TTabSheet;
    OpenPictureDialog1: TOpenPictureDialog;
    JvColorDialog1: TColorDialog;
    gbxGradient: TGroupBox;
    JvgLabel11: TLabel;
    lblGradientPercentFilling: TLabel;
    JvgLabel12: TLabel;
    JvgLabel1: TLabel;
    JvgLabel6: TLabel;
    JvgLabel9: TLabel;
    cbxGradientActive: TCheckBox;
    cbxGradientBufferedDraw: TCheckBox;
    cbxGradientOrientation: TComboBox;
    cbtnGradientColorTo: TComboBox;
    cbtnGradientColorFrom: TComboBox;
    sbarGradientPercentFilling: TScrollBar;
    JvgGroupBox3: TGroupBox;
    sbtnBtnNewBackgroundImage: TSpeedButton;
    bvlBevel1: TBevel;
    imgBackground: TImage;
    cbxActiveBackground: TCheckBox;
    JvgGroupBox6: TGroupBox;
    sbtnBtnNewTextureImage: TSpeedButton;
    bvlBevel2: TBevel;
    imgTexture: TImage;
    cbxActiveTexture: TCheckBox;
    JvgGroupBox2: TGroupBox;
    JvgLabel20: TLabel;
    JvgLabel19: TLabel;
    JvgLabel18: TLabel;
    JvgLabel17: TLabel;
    JvgLabel16: TLabel;
    JvgLabel15: TLabel;
    JvgLabel14: TLabel;
    JvgLabel13: TLabel;
    JvgLabel10: TLabel;
    cbtnText: TComboBox;
    cbtnTextActive: TComboBox;
    cbtnTextDisabled: TComboBox;
    cbtnHighlight: TComboBox;
    cbtnShadow: TComboBox;
    cbtnBackground: TComboBox;
    cbtnBackgroundActive: TComboBox;
    cbtnDelineate: TComboBox;
    cbtnDelineateActive: TComboBox;
    gbxPassiveStyle: TGroupBox;
    cbxPassiveNormal: TCheckBox;
    cbxPassivePushed: TCheckBox;
    cbxPassiveRaised: TCheckBox;
    cbxPassiveShadow: TCheckBox;
    cbxPassiveRecessed: TCheckBox;
    cbxPassiveVolumentric: TCheckBox;
    gbxActiveStyle: TGroupBox;
    cbxActiveNormal: TCheckBox;
    cbxActivePushed: TCheckBox;
    cbxActiveRaised: TCheckBox;
    cbxActiveShadow: TCheckBox;
    cbxActiveRecessed: TCheckBox;
    cbxActiveVolumetric: TCheckBox;
    gbxDisabledStyle: TGroupBox;
    cbxDisabledNormal: TCheckBox;
    cbxDisabledPushed: TCheckBox;
    cbxDisabledRaised: TCheckBox;
    cbxDisabledShadow: TCheckBox;
    cbxDisabledRecessed: TCheckBox;
    cbxDisabledVolumentric: TCheckBox;
    JvgGroupBox1: TGroupBox;
    cbxBold: TCheckBox;
    cbxItalic: TCheckBox;
    cbxUnderline: TCheckBox;
    cbxStrikeOut: TCheckBox;
    gbxLabelDirection: TGroupBox;
    sbtnLabelDirectionDown: TSpeedButton;
    sbtnLabelDirectionUp: TSpeedButton;
    JvgLabel5: TLabel;
    sbtnLabelDirectionLeft: TSpeedButton;
    sbtnLabelDirectionRight: TSpeedButton;
    JvgGroupBox4: TGroupBox;
    lblFontSize: TLabel;
    lblShadowDepth: TLabel;
    JvgLabel2: TLabel;
    JvgLabel3: TLabel;
    sbarShadowDepth: TScrollBar;
    sbarFontSize: TScrollBar;
    cbxFont: TComboBox;
    imglBrushes: TImageList;
    cbxBrushStyle: TComboBox;
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
    FColorItems: TStrings;
    FLabelSource: TJvgLabel;
    FLabel:TJvgLabel;
    procedure LabelChanged;
    procedure SetTextStyle(TextKind: Integer; TextStyle: TglTextStyle);
    procedure GetColorValuesProc(const S: string);
    procedure SetColors(Items: TStrings);
    function SetItemsColor(Items: TStrings; AColor: TColor): Integer;
    function GetItemsColor(Items: TStrings; ItemIndex: Integer; ADefault: TColor): TColor;
  protected
    function UpdateComponent: Boolean; override;
    procedure InitializeEditor; override;
  public
    constructor Create(AOwner: TComponent; LabelSource: TJvgLabel); reintroduce; overload;
    procedure AfterConstruction; override;
  end;

  TJvgLabelEditor = class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

var
  JvgLabelEditorDlg: TJvgLabelEditorDlg;

implementation

{$IFDEF USEJVCL}
uses
  JvDsgnConsts;
{$ENDIF USEJVCL}

{$R *.dfm}

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvgLabelEditorForm.res}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvgLabelEditorForm.res}
{$ENDIF LINUX}

{$IFNDEF USEJVCL}
resourcestring
  RsEditLabel = 'Edit &Label...';
{$ENDIF !USEJVCL}

function IntToTextStyle(Tag: Integer): TglTextStyle;
begin
  case Tag of
    0:
      Result := fstNone;
    1:
      Result := fstPushed;
    2:
      Result := fstRaised;
    3:
      Result := fstShadow;
    4:
      Result := fstRecessed;
    5:
      Result := fstVolumetric;
  else
    Result := fstNone;
  end;
end;

procedure TJvgLabelEditor.ExecuteVerb(Index: Integer);
var
  EditorDlg: TJvgLabelEditorDlg;
begin
  case Index of
    0:
      begin
        EditorDlg := TJvgLabelEditorDlg.Create(Application, Component as TJvgLabel);
        try
          if EditorDlg.ShowModal = mrOk then
            Designer.Modified;
        finally
          EditorDlg.Free;
        end;
      end;
  end;
end;

function TJvgLabelEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := RsEditLabel;
  end;
end;

function TJvgLabelEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TJvgLabelEditorDlg.tbarFontSizeChange(Sender: TObject);
begin
  FLabel.Font.Size := sbarFontSize.Position;
  lblFontSize.Caption := IntToStr(sbarFontSize.Position);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.LabelChanged;
begin
  FLabel.Invalidate;
end;

procedure TJvgLabelEditorDlg.cbxBoldClick(Sender: TObject);
begin
  if cbxBold.Checked then
    FLabel.Font.Style := FLabel.Font.Style + [fsBold]
  else
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxItalicClick(Sender: TObject);
begin
  if cbxItalic.Checked then
    FLabel.Font.Style := FLabel.Font.Style + [fsItalic]
  else
    FLabel.Font.Style := FLabel.Font.Style - [fsItalic];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxUnderlineClick(Sender: TObject);
begin
  if cbxUnderline.Checked then
    FLabel.Font.Style := FLabel.Font.Style + [fsUnderline]
  else
    FLabel.Font.Style := FLabel.Font.Style - [fsUnderline];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxStrikeOutClick(Sender: TObject);
begin
  if cbxStrikeOut.Checked then
    FLabel.Font.Style := FLabel.Font.Style + [fsStrikeOut]
  else
    FLabel.Font.Style := FLabel.Font.Style - [fsStrikeOut];
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxActiveNormalClick(Sender: TObject);
begin
  SetTextStyle(0, IntToTextStyle(TCheckBox(Sender).Tag));
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.SetTextStyle(TextKind: Integer; TextStyle: TglTextStyle);
begin
  case Integer(TextKind) of
    0:
      FLabel.TextStyles.Active := TextStyle;
    1:
      FLabel.TextStyles.Disabled := TextStyle;
    2:
      FLabel.TextStyles.Passive := TextStyle;
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxPassiveNormalClick(Sender: TObject);
begin
  SetTextStyle(1, IntToTextStyle(TCheckBox(Sender).Tag));
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxDisabledNormalClick(Sender: TObject);
begin
  SetTextStyle(2, IntToTextStyle(TCheckBox(Sender).Tag));
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cLabelColorsChange(Sender: TObject);

begin
  with TComboBox(Sender) do
  begin
    case Tag of
      1:
        FLabel.Colors.Text := GetItemsColor(Items, ItemIndex, FLabel.Colors.Text);
      2:
        FLabel.Colors.TextActive := GetItemsColor(Items, ItemIndex, FLabel.Colors.TextActive);
      3:
        FLabel.Colors.TextDisabled := GetItemsColor(Items, ItemIndex, FLabel.Colors.TextDisabled);
      4:
        FLabel.Colors.HighLight := GetItemsColor(Items, ItemIndex, FLabel.Colors.HighLight);
      5:
        FLabel.Colors.Shadow := GetItemsColor(Items, ItemIndex, FLabel.Colors.Shadow);
      6:
        FLabel.Colors.Background := GetItemsColor(Items, ItemIndex, FLabel.Colors.Background);
      7:
        FLabel.Colors.BackgroundActive := GetItemsColor(Items, ItemIndex, FLabel.Colors.BackgroundActive);
      8:
        FLabel.Colors.Delineate := GetItemsColor(Items, ItemIndex, FLabel.Colors.Delineate);
      9:
        FLabel.Colors.DelineateActive := GetItemsColor(Items, ItemIndex, FLabel.Colors.DelineateActive);
    end;
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbarGradientPercentFillingChange(Sender: TObject);
begin
  if Sender is TScrollBar then
  begin
    FLabel.Gradient.PercentFilling := TScrollBar(Sender).Position;
    lblGradientPercentFilling.Caption := IntToStr(TScrollBar(Sender).Position);
    LabelChanged;
  end;
end;

procedure TJvgLabelEditorDlg.cbtnGradientColorFromChange(Sender: TObject);
begin
  with TComboBox(Sender)do
    case Tag of
    1:
      FLabel.Gradient.FromColor := GetItemsColor(Items, ItemIndex, FLabel.Gradient.FromColor);
    2:
      FLabel.Gradient.ToColor := GetItemsColor(Items, ItemIndex, FLabel.Gradient.ToColor);
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientActiveClick(Sender: TObject);
begin
  FLabel.Gradient.Active := TCheckBox(Sender).Checked;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientBufferedDrawClick(Sender: TObject);
begin
  FLabel.Gradient.BufferedDraw := TCheckBox(Sender).Checked;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxGradientOrientationChange(Sender: TObject);
begin
  with FLabel.Gradient do
    case TComboBox(Sender).ItemIndex of
      0:
        Orientation := fgdHorizontal;
      1:
        Orientation := fgdVertical;
      2:
        Orientation := fgdLeftBias;
      3:
        Orientation := fgdRightBias;
      4:
        Orientation := fgdRectangle;
      5:
        Orientation := fgdVertConvergent;
      6:
        Orientation := fgdVertConvergent;
    end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbtnLabelDirectionRightClick(Sender: TObject);
begin
  FLabel.Direction := TglLabelDir(TSpeedButton(Sender).Tag);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxBrushStyleChange(Sender: TObject);
begin
  FLabel.Gradient.BrushStyle := TBrushStyle(TComboBox(Sender).ItemIndex);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.btnNewTextureImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    case TSpeedButton(Sender).Tag of
      0:
        begin
          imgBackground.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveBackground.Checked then
            FLabel.Background := imgBackground.Picture.Bitmap;
        end;
      1:
        begin
          imgTexture.Picture.LoadFromFile(OpenPictureDialog1.FileName);
          if cbxActiveTexture.Checked then
            FLabel.Texture := imgTexture.Picture.Bitmap;
        end;
    end;
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxActiveBackgroundClick(Sender: TObject);
begin
  case TCheckBox(Sender).Tag of
    0:
      if TCheckBox(Sender).Checked then
        FLabel.Background := imgBackground.Picture.Bitmap
      else
        FLabel.Background := nil;
    1:
      if TCheckBox(Sender).Checked then
        FLabel.Texture := imgTexture.Picture.Bitmap
      else
        FLabel.Texture := nil;
  end;
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.sbarShadowDepthChange(Sender: TObject);
begin
  FLabel.Illumination.ShadowDepth := sbarShadowDepth.Position;
  lblShadowDepth.Caption := IntToStr(sbarShadowDepth.Position);
  LabelChanged;
end;

procedure TJvgLabelEditorDlg.cbxFontChange(Sender: TObject);
begin
  FLabel.Font.Name := TFontName(cbxFont.Items[cbxFont.ItemIndex]);
  LabelChanged;
end;

function TJvgLabelEditorDlg.UpdateComponent: Boolean;
begin
  inherited UpdateComponent;
  with FLabelSource do
  begin
    if (FLabel.Background <> nil) and (cbxActiveBackground.Checked) then
      Background.Assign(FLabel.Background)
    else
      Background := nil;

    if (imgTexture.Picture <> nil) and (cbxActiveTexture.Checked) then
      Texture.Assign(FLabel.Texture)
    else
      Texture := nil;

    Colors.Background := FLabel.Colors.Background;
    Colors.BackgroundActive := FLabel.Colors.BackgroundActive;
    Colors.Delineate := FLabel.Colors.Delineate;
    Colors.DelineateActive := FLabel.Colors.DelineateActive;
    Colors.HighLight := FLabel.Colors.HighLight;
    Colors.Shadow := FLabel.Colors.Shadow;
    Colors.Text := FLabel.Colors.Text;
    Colors.TextActive := FLabel.Colors.TextActive;
    Colors.TextDisabled := FLabel.Colors.TextDisabled;

    Direction := FLabel.Direction;

    Font.Name := FLabel.Font.Name;
    Font.Size := FLabel.Font.Size;
    Font.Style := FLabel.Font.Style;

    Gradient.Active := FLabel.Gradient.Active;
    Gradient.BrushStyle := FLabel.Gradient.BrushStyle;
    Gradient.BufferedDraw := FLabel.Gradient.BufferedDraw;
    Gradient.FromColor := FLabel.Gradient.FromColor;
    Gradient.Orientation := FLabel.Gradient.Orientation;
    Gradient.PercentFilling := FLabel.Gradient.PercentFilling;
    Gradient.ToColor := FLabel.Gradient.ToColor;

    Illumination.ShadowDepth := FLabel.Illumination.ShadowDepth;
    TextStyles.Active := FLabel.TextStyles.Active;
    TextStyles.Disabled := FLabel.TextStyles.Disabled;
    TextStyles.Passive := FLabel.TextStyles.Passive;
  end;
  Result := True;
end;

constructor TJvgLabelEditorDlg.Create(AOwner: TComponent; LabelSource: TJvgLabel);
begin
  inherited Create(AOwner);
  FLabel := TJvgLabel.Create(Self);
  FLabel.Align := alClient;
  FLabel.Caption := 'JvLabelTest';
  FLabel.Parent := pnlPanel1;
  FLabelSource := LabelSource;
end;

procedure TJvgLabelEditorDlg.InitializeEditor;

  procedure LoadGlyph(CheckBoxGlyph: TBitmap; Glyph: string);
  begin
    CheckBoxGlyph.LoadFromResourceName(HInstance, Glyph);
  end;

begin
  inherited InitializeEditor;
  // loading from Resource file save ~ 200K in DFM file
  imglBrushes.ResInstLoad(HInstance, rtBitmap, 'BRUSH', clFuchsia);
  sbtnLabelDirectionDown.Glyph.LoadFromResourceName(HInstance, 'DOWN');
  sbtnLabelDirectionUp.Glyph.LoadFromResourceName(HInstance, 'UP');
  sbtnLabelDirectionRight.Glyph.LoadFromResourceName(HInstance, 'RIGHT');
  sbtnLabelDirectionLeft.Glyph.LoadFromResourceName(HInstance, 'LEFT');

  // Square Checkboxes


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
  cbxBold.Checked := fsBold in FLabelSource.Font.Style;
  cbxItalic.Checked := fsItalic in FLabelSource.Font.Style;
  cbxUnderline.Checked := fsUnderline in FLabelSource.Font.Style;
  cbxStrikeOut.Checked := fsStrikeOut in FLabelSource.Font.Style;

  case Integer(FLabelSource.TextStyles.Active) of
    0:
      cbxActiveNormal.Checked := True;
    1:
      cbxActiveRaised.Checked := True;
    2:
      cbxActiveRecessed.Checked := True;
    3:
      cbxActivePushed.Checked := True;
    4:
      cbxActiveShadow.Checked := True;
    5:
      cbxActiveVolumetric.Checked := True;
  end;

  case Integer(FLabelSource.TextStyles.Passive) of
    0:
      cbxPassiveNormal.Checked := True;
    1:
      cbxPassiveRaised.Checked := True;
    2:
      cbxPassiveRecessed.Checked := True;
    3:
      cbxPassivePushed.Checked := True;
    4:
      cbxPassiveShadow.Checked := True;
    5:
      cbxPassiveVolumentric.Checked := True;
  end;

  case Integer(FLabelSource.TextStyles.Disabled) of
    0:
      cbxDisabledNormal.Checked := True;
    1:
      cbxDisabledRaised.Checked := True;
    2:
      cbxDisabledRecessed.Checked := True;
    3:
      cbxDisabledPushed.Checked := True;
    4:
      cbxDisabledShadow.Checked := True;
    5:
      cbxDisabledVolumentric.Checked := True;
  end;

  cbxGradientActive.Checked := FLabelSource.Gradient.Active;
  cbxGradientBufferedDraw.Checked := FLabelSource.Gradient.BufferedDraw;
  cbxBrushStyle.ItemIndex := Integer(FLabelSource.Gradient.BrushStyle);

  cbtnGradientColorFrom.ItemIndex := SetItemsColor(cbtnGradientColorFrom.Items,FLabelSource.Gradient.FromColor);
  cbtnGradientColorTo.ItemIndex := SetItemsColor(cbtnGradientColorFrom.Items,FLabelSource.Gradient.ToColor);
  cbxGradientOrientation.ItemIndex := Integer(FLabelSource.Gradient.Orientation);
  sbarGradientPercentFilling.Position := Integer(FLabelSource.Gradient.PercentFilling);
  cbxFont.ItemIndex := cbxFont.Items.IndexOf(FLabelSource.Font.Name);

  if FLabelSource.Background <> nil then
  begin
    imgBackground.Picture.Bitmap.Assign(FLabelSource.Background);
    FLabel.Background.Assign(imgBackground.Picture.Bitmap);
  end;

  if FLabelSource.Texture <> nil then
  begin
    imgTexture.Picture.Bitmap.Assign(FLabelSource.Texture);
    FLabel.Texture.Assign(imgTexture.Picture.Bitmap);
  end;

  LabelChanged;
end;

procedure TJvgLabelEditorDlg.FormCreate(Sender: TObject);
begin
  cbxFont.Items := Screen.Fonts;
  cbxFont.Sorted := True;
end;

procedure TJvgLabelEditorDlg.AfterConstruction;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if (Components[I].Tag > 0) and (Components[I] is TComboBox) then
      SetColors(TComboBox(Components[I]).Items);
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
  AColor: TColor): Integer;
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
  ItemIndex: Integer; ADefault: TColor): TColor;
begin
  if ItemIndex < 0 then
    Result := ADefault
  else
    Result := TColor(Items.Objects[ItemIndex]);
end;

end.

