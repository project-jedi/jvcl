{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing Rights and limitations under the License.

The Original Code is: JvgCommClasses.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are CopyRight (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Rob den Braasem [rbraasem att xs4all dott nl]
Burov Dmitry, translation of russian text.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
{$I jvcl.inc}

unit JvgCommClasses;

interface

uses
  Windows, Graphics, Controls, Classes, ExtCtrls, JvgTypes;

type
  TJvgTwainColors = class;
  TJvgCustomGradient = class;
  TJvgGradient = class;
  TJvg3DGradient = class;
  TJvg2DAlign = class;
  TJvgPointClass = class;
  TJvgBevelOptions = class;
  TJvgExtBevelOptions = class;
  TJvgIllumination = class;
  TJvgLabelTextStyles = class;
  TJvgCustomTextColors = class;
  TJvgSimleLabelColors = class;
  TJvgCustomLabelColors = class;
  TJvgLabelColors = class;
  TJvgGroupBoxColors = class;
  TJvgListBoxItemStyle = class;
  TJvgAskListBoxItemStyle = class;
  TJvgCustomBoxStyle = class;
  TJvgCustomTextBoxStyle = class;
  TJvgTextBoxStyle = class;
  TJvgBevelLines = class;
  //*************************************{ . TJvgTwainColors . }
  TJvgTwainColors = class(TPersistent)
  private
    FFromColor: TColor;
    FToColor: TColor;
    FRGBFromColor: Longint;
    FRGBToColor: Longint;
    FOnChanged: TNotifyEvent;
    procedure SetFromColor(Value: TColor);
    procedure SetToColor(Value: TColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create; virtual;

    property RGBFromColor: Longint read FRGBFromColor write FRGBFromColor;
    property RGBToColor: Longint read FRGBToColor write FRGBToColor;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property FromColor: TColor read FFromColor write SetFromColor default clGray;
    property ToColor: TColor read FToColor write SetToColor default 0;
  end;
  //*************************************{ . TJvgCustomGradient . }
  TJvgCustomGradient = class(TJvgTwainColors)
  private
    FBufferedDraw: Boolean;
    FSteps: Integer;
    FPercentFilling: TPercentRange;
    FBrushStyle: TBrushStyle;

    FOrientation: TglGradientDir; //...public!
    FActive: Boolean;
    FReverse: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetOrientation(Value: TglGradientDir);
    procedure SetSteps(Value: Integer);
    procedure SetPercentFilling(Value: TPercentRange);
    procedure SetBrushStyle(Value: TBrushStyle);
  protected
    property Active: Boolean read FActive write SetActive;
    property BufferedDraw: Boolean read FBufferedDraw write FBufferedDraw default False;
    property Orientation: TglGradientDir read FOrientation write SetOrientation;
    property Steps: Integer read FSteps write SetSteps default 255;
    property PercentFilling: TPercentRange read FPercentFilling write SetPercentFilling default 100;
    property BrushStyle: TBrushStyle read FBrushStyle write SetBrushStyle default bsSolid;
  public
    constructor Create; override;
    procedure TextOut(DC: HDC; Str: string; TextR: TRect; X, Y: Integer);
    function GetColorFromGradientLine(GradientLineWidth, Position: Word): COLORREF;
  end;

  TJvgGradient = class(TJvgCustomGradient)
  public
    procedure Draw(DC: HDC; r: TRect; PenStyle, PenWidth: Integer);
  published
    property Active;
    property BufferedDraw;
    property Orientation;
    property Steps;
    property PercentFilling;
    property BrushStyle;
  end;
  //*************************************{ . TJvg3DGradient . }
  TJvg3DGradient = class(TJvgCustomGradient)
  private
    FDepth: Word;
    FGType: TThreeDGradientType;
    procedure SetDepth(Value: Word);
    procedure SetGType(Value: TThreeDGradientType);
  public
    constructor Create; override;
  published
    property Depth: Word read FDepth write SetDepth default 16;
    property GType: TThreeDGradientType read FGType write SetGType default fgtFlat;
  end;
  //*************************************{ . TJvg2DAlign . }
  TJvg2DAlign = class(TPersistent)
  private
    FHorizontal: TglHorAlign;
    FVertical: TglVertAlign;
    FOnChanged: TNotifyEvent;
    procedure SetHorizontal(Value: TglHorAlign);
    procedure SetVertical(Value: TglVertAlign);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Horizontal: TglHorAlign read FHorizontal write SetHorizontal
      default fhaLeft;
    property Vertical: TglVertAlign read FVertical write SetVertical
      default fvaTop;
  end;
  //*************************************{ . TJvgPointClass . }
  TJvgPointClass = class(TPersistent)
  private
    FX: Integer;
    FY: Integer;
    FOnChanged: TNotifyEvent;
    procedure SetX(Value: Integer);
    procedure SetY(Value: Integer);
  protected
    procedure Changed; virtual;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
  end;
  //*************************************{ . TJvgBevel . }
  TJvgBevelOptions = class(TPersistent)
  private
    FInner: TPanelBevel;
    FOuter: TPanelBevel;
    FSides: TglSides;
    FBold: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetInner(Value: TPanelBevel);
    procedure SetOuter(Value: TPanelBevel);
    procedure SetSides(Value: TglSides);
    procedure SetBold(Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    function BordersHeight: Integer;
    function BordersWidth: Integer;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Inner: TPanelBevel read FInner write SetInner stored True; //    default bvLowered;
    property Outer: TPanelBevel read FOuter write SetOuter stored True; //    default bvNone;
    property Sides: TglSides read FSides write SetSides stored True default ALLGLSIDES;
    property Bold: Boolean read FBold write SetBold stored True; //  default False;
  end;
  //*************************************{ . TJvgExtBevel . }
  TJvgExtBevelOptions = class(TJvgBevelOptions)
  private
    FActive: Boolean;
    FBevelPenStyle: TPenStyle;
    FBevelPenWidth: Word;
    FInteriorOffset: Word;
    procedure SetActive(Value: Boolean);
    procedure SetBevelPenStyle(Value: TPenStyle);
    procedure SetBevelPenWidth(Value: Word);
    procedure SetInteriorOffset(Value: Word);
  public
    constructor Create; override;
  published
    property Active: Boolean read FActive write SetActive default True;
    property BevelPenStyle: TPenStyle read FBevelPenStyle write SetBevelPenStyle default psSolid;
    property BevelPenWidth: Word read FBevelPenWidth write SetBevelPenWidth default 1;
    property InteriorOffset: Word read FInteriorOffset write SetInteriorOffset default 0;
  end;
  //*************************************{ . TJvgIllumination . }
  TJvgIllumination = class(TJvg2DAlign)
  private
    FShadowDepth: Integer;
    procedure SetShadowDepth(Value: Integer);
  public
    constructor Create;
  published
    property ShadowDepth: Integer read FShadowDepth write SetShadowDepth default 2;
  end;
  //*************************************{ . TJvgLabelTextStyles . }
  TJvgLabelTextStyles = class(TPersistent)
  private
    FPassive: TglTextStyle;
    FActive: TglTextStyle;
    FDisabled: TglTextStyle;
    FOnChanged: TNotifyEvent;
    procedure SetPassive(Value: TglTextStyle);
    procedure SetActive(Value: TglTextStyle);
    procedure SetDisabled(Value: TglTextStyle);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Passive: TglTextStyle read FPassive write SetPassive default fstRaised;
    property Active: TglTextStyle read FActive write SetActive default fstRaised;
    property Disabled: TglTextStyle read FDisabled write SetDisabled default fstPushed;
  end;
  //*************************************{ . TJvgCustomTextColors . }
  TJvgCustomTextColors = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FText: TColor;
    FTextDisabled: TColor;
    FDelineate: TColor;
    FBackground: TColor;
  {public}
    FHighlight: TColor;
    FShadow: TColor;
  private
    procedure SetText(Value: TColor);
    procedure SetTextDisabled(Value: TColor);
    procedure SetDelineate(Value: TColor);
    procedure SetBackground(Value: TColor);
    procedure SetHighlight(Value: TColor);
    procedure SetShadow(Value: TColor);
  protected
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  protected
    property Text: TColor read FText write SetText default clBlack;
    property TextDisabled: TColor read FTextDisabled write SetTextDisabled default clGray;
    property Delineate: TColor read FDelineate write SetDelineate default clWhite;
    property Shadow: TColor read FShadow write SetShadow default clBtnShadow;
    property Highlight: TColor read FHighlight write SetHighlight default clBtnHighlight;
    property Background: TColor read FBackground write SetBackground default clBtnFace;
  end;
  //*************************************{ . TJvgCustomLabelColors . }
  TJvgSimleLabelColors = class(TJvgCustomTextColors)
  published
    //  property Text stored True;
    property Delineate stored True;
    property Shadow stored True;
    property Highlight;
    property Background stored True;
  end;

  //*************************************{ . TJvgCustomLabelColors . }
  TJvgCustomLabelColors = class(TJvgCustomTextColors)
  private
    FTextActive: TColor;
    FDelineateActive: TColor;
    FAutoHighlight: Boolean;
    FAutoShadow: Boolean;
    FBackgroundActive: TColor;
  {public}
    FColorHighlightShift: Integer;
    FColorShadowShift: Integer;
  private
    procedure SetTextActive(Value: TColor);
    procedure SetDelineateActive(Value: TColor);
    procedure SetBackgroundActive(Value: TColor);
    procedure SetAutoHighlight(Value: Boolean);
    procedure SetAutoShadow(Value: Boolean);
    procedure SetColorHighlightShift(Value: Integer);
    procedure SetColorShadowShift(Value: Integer);
  public
    constructor Create; override;
  protected
    property TextActive: TColor read FTextActive write SetTextActive default clBlack;
    property DelineateActive: TColor read FDelineateActive write SetDelineateActive default clWhite;
    property AutoHighlight: Boolean read FAutoHighlight write SetAutoHighlight default False;
    property AutoShadow: Boolean read FAutoShadow write SetAutoShadow default False;
    property ColorHighlightShift: Integer read FColorHighlightShift write SetColorHighlightShift default 40;
    property ColorShadowShift: Integer read FColorShadowShift write SetColorShadowShift default 60;
    property BackgroundActive: TColor read FBackgroundActive write SetBackgroundActive default clBtnFace;
  end;
  //*************************************{ . TJvgLabelColors . }
  TJvgLabelColors = class(TJvgCustomLabelColors)
  published
    property Text;
    property TextDisabled;
    property Delineate;
    property Shadow;
    property Highlight;
    property Background;
    property TextActive;
    property DelineateActive;
    property AutoHighlight;
    property AutoShadow;
    property ColorHighlightShift;
    property ColorShadowShift;
    property BackgroundActive;
  end;
  //*************************************{ . TJvgGroupBoxColors . }
  TJvgGroupBoxColors = class(TJvgCustomLabelColors)
  private
    FCaption: TColor;
    FCaptionActive: TColor;
    FClient: TColor;
    FClientActive: TColor;
    procedure SetCaption(Value: TColor);
    procedure SetCaptionActive(Value: TColor);
    procedure SetClient(Value: TColor);
    procedure SetClientActive(Value: TColor);
  public
    constructor Create; override;
  published
    property Text;
    property Delineate;
    property Shadow;
    property Highlight;
    //  property Background;
    property TextActive;
    property DelineateActive;
    //  property AutoHighlight;
    //  property AutoShadow;
    //  property ColorHighlightShift;
    //  property ColorShadowShift;
    //  property BackgroundActive;
    property Caption: TColor read FCaption write SetCaption;
    property CaptionActive: TColor read FCaptionActive write SetCaptionActive;
    property Client: TColor read FClient write SetClient;
    property ClientActive: TColor read FClientActive write SetClientActive;
  end;
  //*************************************{ . TglListItemStyle . }
  TJvgCustomListBoxItemStyle = class(TPersistent)
  private
    FColor: TColor;
    FDelineateColor: TColor;
    FFont: TFont;
    FBevel: TJvgBevelOptions;
    FTextStyle: TglTextStyle;
    FOnChanged: TNotifyEvent;
    procedure SetColor(Value: TColor);
    procedure SetDelineateColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetTextStyle(Value: TglTextStyle);
  protected
    procedure SetOnChanged(Value: TNotifyEvent); virtual;
    procedure Changed;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function HighlightColor: TColor;
    function ShadowColor: TColor;
    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
    property Color: TColor read FColor write SetColor;
    property DelineateColor: TColor read FDelineateColor write SetDelineateColor;
    property Font: TFont read FFont write SetFont;
    property Bevel: TJvgBevelOptions read FBevel write FBevel;
    property TextStyle: TglTextStyle read FTextStyle write SetTextStyle;
  end;
  //*************************************{ . TglListItemStyle . }
  TJvgListBoxItemStyle = class(TJvgCustomListBoxItemStyle)
  private
    FGradient: TJvgGradient;
    FTextGradient: TJvgGradient;
  protected
    property TextGradient: TJvgGradient read FTextGradient write FTextGradient;
    procedure SetOnChanged(Value: TNotifyEvent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Gradient: TJvgGradient read FGradient write FGradient;
    property Color;
    property DelineateColor;
    property Font;
    property Bevel;
    property TextStyle;
  end;

  TJvgHintStyle = class(TJvgListBoxItemStyle)
  end;

  TJvgSpeedButtonStyle = class(TJvgListBoxItemStyle)
  published
    property TextGradient;
  end;
  //*************************************{ . TglListItemStyle . }
  TJvgAskListBoxItemStyle = class(TJvgCustomListBoxItemStyle)
  private
    FBtnColor: TColor;
    FBtnFont: TFont;
    FBtnTextStyle: TglTextStyle;
    procedure SetBtnColor(Value: TColor);
    procedure SetBtnFont(Value: TFont);
    procedure SetBtnTextStyle(Value: TglTextStyle);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property BtnColor: TColor read FBtnColor write SetBtnColor;
    property BtnFont: TFont read FBtnFont write SetBtnFont;
    property BtnTextStyle: TglTextStyle read FBtnTextStyle write SetBtnTextStyle;
    property Color;
    property DelineateColor;
    property Font;
    property Bevel;
    property TextStyle;
  end;
  //*************************************{ . TJvgCustomBoxStyle . }
  TJvgCustomBoxStyle = class(TJvgBevelOptions)
  private
    FPenStyle: TPenStyle;
    FHighlightColor: TColor;
    FShadowColor: TColor;
    procedure SetPenStyle(Value: TPenStyle);
    procedure SetHighlightColor(Value: TColor);
    procedure SetShadowColor(Value: TColor);
  protected
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle default psSolid;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clBtnHighlight;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
  public
    constructor Create; override;
  end;
  //*************************************{ . TJvgTextBoxStyle . }
  TJvgCustomTextBoxStyle = class(TJvgCustomBoxStyle)
  private
    FTextColor: TColor;
    FBackgroundColor: TColor;
    procedure SetTextColor(Value: TColor);
    procedure SetBackgroundColor(Value: TColor);
  protected
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWindow;
  public
    constructor Create; override;
  end;
  //*************************************{ . TJvgCustomBoxStyle . }
  TJvgTextBoxStyle = class(TJvgCustomTextBoxStyle)
  published
    property Inner;
    property Outer;
    property Sides;
    property Bold;
    property PenStyle;
    property TextColor;
    property BackgroundColor;
    property HighlightColor;
    property ShadowColor;
  end;
  //*************************************{ .TJvgBevelOptionsLines. }
  TJvgBevelLines = class(TPersistent)
  private
    FCount: cardinal;
    FStep: cardinal;
    FOrigin: TglOrigin;
    FStyle: TPanelBevel;
    FBold: Boolean;
    FThickness: Byte;
    FIgnoreBorder: Boolean;
    FOnChanged: TNotifyEvent;

    procedure SetCount(Value: cardinal);
    procedure SetStep(Value: cardinal);
    procedure SetOrigin(Value: TglOrigin);
    procedure SetStyle(Value: TPanelBevel);
    procedure SetBold(Value: Boolean);
    procedure SetThickness(Value: Byte);
    procedure SetIgnoreBorder(Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Count: cardinal read FCount write SetCount default 0;
    property Step: cardinal read FStep write SetStep default 0;
    property Origin: TglOrigin read FOrigin write SetOrigin default forLeftTop;
    property Style: TPanelBevel read FStyle write SetStyle default bvLowered;
    property Bold: Boolean read FBold write SetBold default False;
    property Thickness: Byte read FThickness write SetThickness default 1;
    property IgnoreBorder: Boolean read FIgnoreBorder write SetIgnoreBorder default False;
  end;

implementation

uses
  Math,
  JvgUtils;

procedure TJvgTwainColors.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TJvgTwainColors.Create;
begin
  inherited Create;
  //...set defaults
  FFromColor := clGray;
  FRGBFromColor := ColorToRGB(FFromColor);
  FToColor := clBlack;
  FRGBToColor := ColorToRGB(FToColor);
end;

procedure TJvgTwainColors.SetFromColor(Value: TColor);
begin
  if FFromColor <> Value then
  begin
    FFromColor := Value;
    FRGBFromColor := ColorToRGB(Value);
    Changed;
  end;
end;

procedure TJvgTwainColors.SetToColor(Value: TColor);
begin
  if FToColor <> Value then
  begin
    FToColor := Value;
    FRGBToColor := ColorToRGB(Value);
    Changed;
  end;
end;
//______________________________________{ . TJvgCustomGradient methods . }

constructor TJvgCustomGradient.Create;
begin
  inherited Create;
  //...set defaults
  FActive := False;
  FBufferedDraw := False;
  FOrientation := fgdHorizontal;
  FSteps := 255;
  FPercentFilling := 100;
  FBrushStyle := bsSolid;
end;

procedure TJvgCustomGradient.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TJvgCustomGradient.SetOrientation(Value: TglGradientDir);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Changed;
  end;
end;

procedure TJvgCustomGradient.SetSteps(Value: Integer);
begin
  if Value > 255 then
    Value := 255
  else
  if Value < 1 then
    Value := 1;
  if FSteps <> Value then
  begin
    FSteps := Value;
    Changed;
  end;
end;

procedure TJvgCustomGradient.SetPercentFilling(Value: TPercentRange);
begin
  if FPercentFilling <> Value then
  begin
    FPercentFilling := Value;
    Changed;
  end;
end;

procedure TJvgCustomGradient.SetBrushStyle(Value: TBrushStyle);
begin
  if Value <> FBrushStyle then
  begin
    FBrushStyle := Value;
    Changed;
  end;
end;

function TJvgCustomGradient.GetColorFromGradientLine
  (GradientLineWidth, Position: Word): COLORREF;
var
  c1F, c2F, c3F: Byte;
  c1T, c2T, c3T: Byte;
  Step1, Step2, Step3: Single;
begin
  c1F := Byte(Self.FRGBFromColor);
  c2F := Byte(Word(Self.FRGBFromColor) shr 8);
  c3F := Byte(Self.FRGBFromColor shr 16);
  c1T := Byte(Self.FRGBToColor);
  c2T := Byte(Word(Self.FRGBToColor) shr 8);
  c3T := Byte(Self.FRGBToColor shr 16);

  Step1 := (c1T - c1F) / GradientLineWidth;
  Step2 := (c2T - c2F) / GradientLineWidth;
  Step3 := (c3T - c3F) / GradientLineWidth;

  Result := RGB(trunc(c1F + Step1 * Position),
    trunc(c2F + Step2 * Position),
    trunc(c3F + Step3 * Position));
end;

procedure TJvgCustomGradient.TextOut(DC: HDC; Str: string; TextR: TRect; X, Y: Integer);
var
  I, Steps: Integer;
  r: TRect;
  c1F, c2F, c3F: Byte;
  c1T, c2T, c3T: Byte;
  c1, c2, c3: Single;
  Step1, Step2, Step3: Single;
  OldTextColor: TCOLORREF;
begin
  if (not Active) or (GetDeviceCaps(DC, BITSPIXEL) < 16) then
  begin
    Windows.TextOut(DC, X, Y, PChar(Str), Length(Str));
    Exit;
  end;
  r := TextR;
  c1F := Byte(FRGBFromColor);
  c2F := Byte(Word(FRGBFromColor) shr 8);
  c3F := Byte(FRGBFromColor shr 16);
  c1T := Byte(FRGBToColor);
  c2T := Byte(Word(FRGBToColor) shr 8);
  c3T := Byte(FRGBToColor shr 16);

  c1 := c1F;
  c2 := c2F;
  c3 := c3F;
  if FOrientation = fgdVertical then
    Steps := r.Right - r.Left
  else
    Steps := r.Bottom - r.Top;
  Step1 := (c1T - c1F) / Steps;
  Step2 := (c2T - c2F) / Steps;
  Step3 := (c3T - c3F) / Steps;

  OldTextColor := SetTextColor(DC, 0);
  Steps := MulDiv(Steps, PercentFilling, 100);
  for I := 0 to Steps do
  begin
    SetTextColor(DC, RGB(trunc(c1), trunc(c2), trunc(c3)));

    if FOrientation = fgdVertical then
    begin
      r.Left := I;
      r.Right := r.Left + 1;
    end
    else
    begin
      r.Top := I;
      r.Bottom := r.Top + 1;
    end;

    Windows.ExtTextOut(DC, X, Y, ETO_CLIPPED, @r,
      PChar(Str), Length(Str), nil);
    c1 := c1 + Step1;
    c2 := c2 + Step2;
    c3 := c3 + Step3;
  end;
  SetTextColor(DC, OldTextColor);
end;
//______________________________________{ . TJvg3DGradient methods . }

constructor TJvg3DGradient.Create;
begin
  inherited Create;
  Depth := 16;
  FGType := fgtFlat;
  FActive := True;
end;

procedure TJvg3DGradient.SetGType(Value: TThreeDGradientType);
begin
  if FGType <> Value then
  begin
    FGType := Value;
    Changed;
  end;
end;

procedure TJvg3DGradient.SetDepth(Value: Word);
begin
  if FDepth <> Value then
  begin
    FDepth := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvg2DAlign methods . }

constructor TJvg2DAlign.Create;
begin
  inherited Create;
  //...set defaults
  FHorizontal := fhaLeft;
  FVertical := fvaTop;
end;

procedure TJvg2DAlign.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvg2DAlign.SetHorizontal(Value: TglHorAlign);
begin
  if FHorizontal <> Value then
  begin
    FHorizontal := Value;
    Changed;
  end;
end;

procedure TJvg2DAlign.SetVertical(Value: TglVertAlign);
begin
  if FVertical <> Value then
  begin
    FVertical := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgPointClass methods . }

procedure TJvgPointClass.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvgPointClass.SetX(Value: Integer);
begin
  if FX <> Value then
  begin
    FX := Value;
    Changed;
  end;
end;

procedure TJvgPointClass.SetY(Value: Integer);
begin
  if FY <> Value then
  begin
    FY := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgBevelOptions methods . }

constructor TJvgBevelOptions.Create;
begin
  inherited;
  //..defaults
  FSides := ALLGLSIDES;
end;

procedure TJvgBevelOptions.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TJvgBevelOptions.SetOuter(Value: TPanelBevel);
begin
  if FOuter <> Value then
  begin
    FOuter := Value;
    Changed;
  end;
end;

procedure TJvgBevelOptions.SetInner(Value: TPanelBevel);
begin
  if FInner <> Value then
  begin
    FInner := Value;
    Changed;
  end;
end;

procedure TJvgBevelOptions.SetSides(Value: TglSides);
begin
  if FSides <> Value then
  begin
    FSides := Value;
    Changed;
  end;
end;

procedure TJvgBevelOptions.SetBold(Value: Boolean);
begin
  if FBold <> Value then
  begin
    FBold := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvgIllumination methods . }

constructor TJvgIllumination.Create;
begin
  inherited Create;
  //..defaults
  FShadowDepth := 2;
end;

procedure TJvgIllumination.SetShadowDepth(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgLabelTextStyles methods . }

constructor TJvgLabelTextStyles.Create;
begin
  inherited Create;
  //..defaults
  FActive := fstRaised;
  FPassive := fstRaised;
  FDisabled := fstPushed;
end;

procedure TJvgLabelTextStyles.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvgLabelTextStyles.SetPassive(Value: TglTextStyle);
begin
  if FPassive <> Value then
  begin
    FPassive := Value;
    Changed;
  end;
end;

procedure TJvgLabelTextStyles.SetActive(Value: TglTextStyle);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TJvgLabelTextStyles.SetDisabled(Value: TglTextStyle);
begin
  if FDisabled <> Value then
  begin
    FDisabled := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgCustomTextColors methods . }

constructor TJvgCustomTextColors.Create;
begin
  inherited Create;
  //..defaults
  FText := clBlack;
  FTextDisabled := clGray;
  FDelineate := clWhite;
  FHighlight := clBtnHighlight;
  FShadow := clBtnShadow;
  FBackground := clBtnFace;
end;

procedure TJvgCustomTextColors.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvgCustomTextColors.SetText(Value: TColor);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextColors.SetTextDisabled(Value: TColor);
begin
  if FTextDisabled <> Value then
  begin
    FTextDisabled := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextColors.SetDelineate(Value: TColor);
begin
  if FDelineate <> Value then
  begin
    FDelineate := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextColors.SetHighlight(Value: TColor);
begin
  if FHighlight <> Value then
  begin
    FHighlight := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextColors.SetShadow(Value: TColor);
begin
  if FShadow <> Value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextColors.SetBackground(Value: TColor);
begin
  if FBackground <> Value then
  begin
    FBackground := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgCustomLabelColors . }

constructor TJvgCustomLabelColors.Create;
begin
  inherited Create;
  //..defaults
  FTextActive := clBlack;
  FDelineateActive := clWhite;
  FAutoHighlight := False;
  FAutoShadow := False;
  FColorHighlightShift := 40;
  FColorShadowShift := 60;
  FBackgroundActive := clBtnFace;
end;

procedure TJvgCustomLabelColors.SetTextActive(Value: TColor);
begin
  if FTextActive <> Value then
  begin
    FTextActive := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetDelineateActive(Value: TColor);
begin
  if FDelineateActive <> Value then
  begin
    FDelineateActive := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetAutoHighlight(Value: Boolean);
begin
  if FAutoHighlight <> Value then
  begin
    FAutoHighlight := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetAutoShadow(Value: Boolean);
begin
  if FAutoShadow <> Value then
  begin
    FAutoShadow := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetColorHighlightShift(Value: Integer);
begin
  if FColorHighlightShift <> Value then
  begin
    FColorHighlightShift := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetColorShadowShift(Value: Integer);
begin
  if FColorShadowShift <> Value then
  begin
    FColorShadowShift := Value;
    Changed;
  end;
end;

procedure TJvgCustomLabelColors.SetBackgroundActive(Value: TColor);
begin
  if FBackgroundActive <> Value then
  begin
    FBackgroundActive := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgGroupBoxColors . }

constructor TJvgGroupBoxColors.Create;
begin
  inherited Create;
  //..defaults
  FCaption := clBtnFace;
  FCaptionActive := clBtnFace;
  FClient := clBtnFace;
  FClientActive := clBtnFace;
end;

procedure TJvgGroupBoxColors.SetCaption(Value: TColor);
begin
  if Value <> FCaption then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TJvgGroupBoxColors.SetCaptionActive(Value: TColor);
begin
  if Value <> FCaptionActive then
  begin
    FCaptionActive := Value;
    Changed;
  end;
end;

procedure TJvgGroupBoxColors.SetClient(Value: TColor);
begin
  if Value <> FClient then
  begin
    FClient := Value;
    Changed;
  end;
end;

procedure TJvgGroupBoxColors.SetClientActive(Value: TColor);
begin
  if Value <> FClientActive then
  begin
    FClientActive := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgExtBevelOptions . }

constructor TJvgExtBevelOptions.Create;
begin
  inherited Create;
  //..defaults
  FActive := True;
  FBevelPenStyle := psSolid;
  FBevelPenWidth := 1;
end;

procedure TJvgExtBevelOptions.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TJvgExtBevelOptions.SetBevelPenStyle(Value: TPenStyle);
begin
  if FBevelPenStyle <> Value then
  begin
    FBevelPenStyle := Value;
    Changed;
  end;
end;

procedure TJvgExtBevelOptions.SetBevelPenWidth(Value: Word);
begin
  if FBevelPenWidth <> Value then
  begin
    FBevelPenWidth := Value;
    Changed;
  end;
end;

procedure TJvgExtBevelOptions.SetInteriorOffset(Value: Word);
begin
  if FInteriorOffset <> Value then
  begin
    FInteriorOffset := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvgCustomListBoxItemStyle . }

constructor TJvgCustomListBoxItemStyle.Create;
begin
  inherited Create;
  FBevel := TJvgBevelOptions.Create;
  FFont := TFont.Create;
end;

destructor TJvgCustomListBoxItemStyle.Destroy;
begin
  FFont.Free;
  FBevel.Free;
  inherited Destroy;
end;

procedure TJvgCustomListBoxItemStyle.SetOnChanged(Value: TNotifyEvent);
begin
  FOnChanged := Value;
  FBevel.OnChanged := Value;
end;

procedure TJvgCustomListBoxItemStyle.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvgCustomListBoxItemStyle.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TJvgCustomListBoxItemStyle.SetDelineateColor(Value: TColor);
begin
  if FDelineateColor <> Value then
  begin
    FDelineateColor := Value;
    Changed;
  end;
end;

procedure TJvgCustomListBoxItemStyle.SetFont(Value: TFont);
begin
  if Value <> FFont then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TJvgCustomListBoxItemStyle.SetTextStyle(Value: TglTextStyle);
begin
  if Value <> FTextStyle then
  begin
    FTextStyle := Value;
    Changed;
  end;
end;
//______________________________________{ . TJvgListBoxItemStyle . }

constructor TJvgListBoxItemStyle.Create;
begin
  inherited Create;
  FGradient := TJvgGradient.Create;
  FTextGradient := TJvgGradient.Create;
end;

destructor TJvgListBoxItemStyle.Destroy;
begin
  FGradient.Free;
  FTextGradient.Free;
  inherited Destroy;
end;

procedure TJvgListBoxItemStyle.SetOnChanged(Value: TNotifyEvent);
begin
  inherited SetOnChanged(Value);
  FGradient.OnChanged := Value;
end;

//______________________________________{ . TJvgAskListBoxItemStyle . }

constructor TJvgAskListBoxItemStyle.Create;
begin
  inherited Create;
  FBtnFont := TFont.Create;
end;

destructor TJvgAskListBoxItemStyle.Destroy;
begin
  FBtnFont.Free;
  inherited Destroy; 
end;

procedure TJvgAskListBoxItemStyle.SetBtnColor(Value: TColor);
begin
  if FBtnColor <> Value then
  begin
    FBtnColor := Value;
    Changed;
  end;
end;

procedure TJvgAskListBoxItemStyle.SetBtnFont(Value: TFont);
begin
  if Value <> FBtnFont then
  begin
    FBtnFont.Assign(Value);
    Changed;
  end;
end;

procedure TJvgAskListBoxItemStyle.SetBtnTextStyle(Value: TglTextStyle);
begin
  if Value <> FBtnTextStyle then
  begin
    FBtnTextStyle := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvgCustomBoxStyle . }

constructor TJvgCustomBoxStyle.Create;
begin
  inherited Create;
  FPenStyle := psSolid;
  FHighlightColor := clBtnHighlight;
  FShadowColor := clBtnShadow;
end;

procedure TJvgCustomBoxStyle.SetPenStyle(Value: TPenStyle);
begin
  if Value <> FPenStyle then
  begin
    FPenStyle := Value;
    Changed;
  end;
end;

procedure TJvgCustomBoxStyle.SetHighlightColor(Value: TColor);
begin
  if Value <> FHighlightColor then
  begin
    FHighlightColor := Value;
    Changed;
  end;
end;

procedure TJvgCustomBoxStyle.SetShadowColor(Value: TColor);
begin
  if Value <> FShadowColor then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvgCustomTextBoxStyle . }

constructor TJvgCustomTextBoxStyle.Create;
begin
  inherited Create;
  FTextColor := clBlack;
  FBackgroundColor := clWindow;
end;

procedure TJvgCustomTextBoxStyle.SetTextColor(Value: TColor);
begin
  if Value <> FTextColor then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TJvgCustomTextBoxStyle.SetBackgroundColor(Value: TColor);
begin
  if Value <> FBackgroundColor then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

//______________________________________{ . TJvgBevelLines . }

constructor TJvgBevelLines.Create;
begin
  inherited Create;
  FStyle := bvLowered;
  FThickness := 1;
end;

procedure TJvgBevelLines.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TJvgBevelLines.SetCount(Value: cardinal);
begin
  if Value <> FCount then
  begin
    FCount := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetStep(Value: cardinal);
begin
  if Value <> FStep then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetOrigin(Value: TglOrigin);
begin
  if Value <> FOrigin then
  begin
    FOrigin := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetStyle(Value: TPanelBevel);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetBold(Value: Boolean);
begin
  if Value <> FBold then
  begin
    FBold := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetThickness(Value: Byte);
begin
  if Value <> FThickness then
  begin
    FThickness := Value;
    Changed;
  end;
end;

procedure TJvgBevelLines.SetIgnoreBorder(Value: Boolean);
begin
  if Value <> FIgnoreBorder then
  begin
    FIgnoreBorder := Value;
    Changed;
  end;
end;

//=== { TJvgGradient } =======================================================

// { paints the gradient; отрисовывает градиент }

procedure TJvgGradient.Draw(DC: HDC; r: TRect; PenStyle, PenWidth: Integer);
var
  I, J, X, Y, x2, y2, h, w, NumberOfColors: Integer;
  c1F, c2F, c3F: Byte;
  c1T, c2T, c3T: Byte;
  c1D, c2D, c3D: Integer;
  _R, _G, _B: Byte;
  Pen, OldPen: HPen;
  FillBrush: HBRUSH;
  BufferBmp, OldBMP: HBITMAP;
  BufferDC, TargetDC: HDC;
  ColorR: TRect;
  LOGBRUSH: TLOGBRUSH;

  procedure SwapColors;
  var
    TempColor: Longint;
  begin
    TempColor := FRGBFromColor;
    FRGBFromColor := FRGBToColor;
    FRGBToColor := TempColor;
  end;
begin
  if (not Active) or glGlobalData.fSuppressGradient then
    Exit;
  if (Steps = 1) or (GetDeviceCaps(DC, BITSPIXEL) < 16) then
  begin
    Exit;
    FillBrush := CreateSolidBrush(ColorToRGB(FromColor));
    FillRect(DC, r, FillBrush);
    DeleteObject(FillBrush);
    Exit;
  end;
  X := r.Left;
  Y := r.Top;
  h := r.Bottom - r.Top;
  w := r.Right - r.Left;
  x2 := 0;
  y2 := 0;
  Pen := 0;
  OldPen := 0;
  BufferDC := 0;

  if Orientation = fgdHorzConvergent then
  begin
    FOrientation := fgdHorizontal;
    Draw(DC, Rect(r.Left, r.Top, r.Right, r.Bottom - h div 2), PenStyle, PenWidth);
    SwapColors;
    Draw(DC, Rect(r.Left, r.Top + h div 2, r.Right, r.Bottom), PenStyle, PenWidth);
    SwapColors;
    FOrientation := fgdHorzConvergent;
    Exit;
  end;
  if Orientation = fgdVertConvergent then
  begin
    FOrientation := fgdVertical;
    Draw(DC, Rect(r.Left, r.Top, r.Right - w div 2, r.Bottom), PenStyle, PenWidth);
    SwapColors;
    Draw(DC, Rect(r.Left + w div 2, r.Top, r.Right, r.Bottom), PenStyle, PenWidth);
    SwapColors;
    FOrientation := fgdVertConvergent;
    Exit;
  end;

  //...r._ data no more useful
  c1F := Byte(FRGBFromColor);
  c2F := Byte(Word(FRGBFromColor) shr 8);
  c3F := Byte(FRGBFromColor shr 16);
  c1T := Byte(FRGBToColor);
  c2T := Byte(Word(FRGBToColor) shr 8);
  c3T := Byte(FRGBToColor shr 16);
  c1D := c1T - c1F;
  c2D := c2T - c2F;
  c3D := c3T - c3F;

  if BufferedDraw then
  begin
    BufferDC := CreateCompatibleDC(DC);
    BufferBmp := CreateBitmap(w, h, GetDeviceCaps(DC, Planes), GetDeviceCaps(DC, BITSPIXEL), nil);
    OldBMP := SelectObject(BufferDC, BufferBmp);
    SetMapMode(BufferDC, GetMapMode(DC));
    TargetDC := BufferDC;
  end
  else
    TargetDC := DC;

  case Orientation of
    fgdHorizontal:
      begin
        NumberOfColors := Min(Steps, h);
        ColorR.Left := r.Left;
        ColorR.Right := r.Right;
      end;
    fgdVertical:
      begin
        NumberOfColors := Min(Steps, w);
        ColorR.Top := r.Top;
        ColorR.Bottom := r.Bottom;
      end;
    fgdLeftBias, fgdRightBias:
      begin
        NumberOfColors := Min(Steps, w + h);
        if PenStyle = 0 then
          PenStyle := PS_SOLID;
        if PenWidth = 0 then
          PenWidth := 1;
        Pen := CreatePen(PenStyle, PenWidth, 0);
        OldPen := SelectObject(TargetDC, Pen);
        y2 := Y;
        if Orientation = fgdLeftBias then
          x2 := X
        else
        begin
          X := r.Right;
          x2 := r.Right;
        end;
      end;
  else {fgdRectangle}
    begin
      h := h div 2;
      w := w div 2;
      NumberOfColors := Min(Steps, Min(w, h));
    end;
  end;
  LOGBRUSH.lbStyle := BS_HATCHED;
  LOGBRUSH.lbHatch := Ord(BrushStyle) - Ord(bsHorizontal);
  for I := 0 to NumberOfColors - 1 do
  begin
    _R := c1F + MulDiv(I, c1D, NumberOfColors - 1);
    _G := c2F + MulDiv(I, c2D, NumberOfColors - 1);
    _B := c3F + MulDiv(I, c3D, NumberOfColors - 1);

    case Orientation of
      fgdHorizontal, fgdVertical, fgdRectangle:
        begin
          if BrushStyle = bsSolid then
            FillBrush := CreateSolidBrush(RGB(_R, _G, _B))
          else
          begin
            LOGBRUSH.lbColor := RGB(_R, _G, _B);
            FillBrush := CreateBrushIndirect(LOGBRUSH);
          end;

          case Orientation of
            fgdHorizontal:
              begin
                if FReverse then
                begin
                  ColorR.Top := r.Bottom - MulDiv(I, h, NumberOfColors);
                  ColorR.Bottom := r.Bottom - MulDiv(I + 1, h, NumberOfColors);
                end
                else
                begin
                  ColorR.Top := r.Top + MulDiv(I, h, NumberOfColors);
                  ColorR.Bottom := r.Top + MulDiv(I + 1, h, NumberOfColors);
                end;
              end;
            fgdVertical:
              begin
                if FReverse then
                begin
                  ColorR.Left := r.Right - MulDiv(I, w, NumberOfColors);
                  ColorR.Right := r.Right - MulDiv(I + 1, w, NumberOfColors);
                end
                else
                begin
                  ColorR.Left := r.Left + MulDiv(I, w, NumberOfColors);
                  ColorR.Right := r.Left + MulDiv(I + 1, w, NumberOfColors);
                end;
              end;
            fgdRectangle:
              begin
                ColorR.Top := r.Top + MulDiv(I, h, NumberOfColors);
                ColorR.Bottom := r.Bottom - MulDiv(I, h, NumberOfColors);
                ColorR.Left := r.Left + MulDiv(I, w, NumberOfColors);
                ColorR.Right := r.Right - MulDiv(I, w, NumberOfColors);
              end;
          end;
          FillRect(TargetDC, ColorR, FillBrush);
          DeleteObject(FillBrush);
        end;
    else {fgdLeftBias, fgdRightBias:}
      begin
        if Pen <> 0 then
          DeleteObject(SelectObject(TargetDC, OldPen)); //...cant delete selected!

        Pen := CreatePen(PenStyle, PenWidth, RGB(_R, _G, _B));

        OldPen := SelectObject(TargetDC, Pen);
        for J := 1 to MulDiv(I + 1, h + w, NumberOfColors) - MulDiv(I, h + w, NumberOfColors) do
        begin
          case Orientation of
            fgdLeftBias:
              begin
                if Y >= r.Bottom then
                  Inc(X, PenWidth)
                else
                  Y := Y + PenWidth;
                if x2 >= r.Right then
                  Inc(y2, PenWidth)
                else
                  x2 := x2 + PenWidth;
                MoveToEx(TargetDC, X, Y, nil);
                LineTo(TargetDC, x2, y2);
              end;
          else {fgdRightBias:}
            begin
              if X <= r.Left then
                Inc(Y, PenWidth)
              else
                X := X - PenWidth;
              if y2 >= r.Bottom then
                dec(x2, PenWidth)
              else
                y2 := y2 + PenWidth;
              MoveToEx(TargetDC, X, Y, nil);
              LineTo(TargetDC, x2, y2);
            end;
          end; {end case}
        end; {end for}
        DeleteObject(SelectObject(TargetDC, OldPen));
      end; {end case else}
    end; {end case}
    //    if NumberOfColors=0 then exit;
    if I / NumberOfColors * 100 > PercentFilling then
      Break;
  end; {end for}

  if BufferedDraw then
  begin
    BitBlt(DC, 0, 0, r.Right - r.Left, r.Bottom - r.Top, BufferDC, 0, 0, SRCCOPY);
    DeleteObject(SelectObject(BufferDC, OldBMP));
    DeleteDC(BufferDC);
  end;

end;

function TJvgBevelOptions.BordersHeight: Integer;
begin
  Result := 0;
  if Inner <> bvNone then
  begin
    if fsdTop in Sides then
      Inc(Result);
    if fsdBottom in Sides then
      if Bold then
        Inc(Result, 1)
      else
        Inc(Result);
  end;
  if Outer <> bvNone then
  begin
    if fsdTop in Sides then
      Inc(Result);
    if fsdBottom in Sides then
      if Bold then
        Inc(Result, 1)
      else
        Inc(Result);
  end;
end;

function TJvgBevelOptions.BordersWidth: Integer;
begin
  Result := 0;
  if Inner <> bvNone then
  begin
    if fsdLeft in Sides then
      Inc(Result);
    if fsdRight in Sides then
      if Bold then
        Inc(Result, 1)
      else
        Inc(Result);
  end;
  if Outer <> bvNone then
  begin
    if fsdLeft in Sides then
      Inc(Result);
    if fsdRight in Sides then
      if Bold then
        Inc(Result, 1)
      else
        Inc(Result);
  end;
end;

function TJvgCustomListBoxItemStyle.HighlightColor: TColor;
begin
  Result := IncColor(Color, 60);
end;

function TJvgCustomListBoxItemStyle.ShadowColor: TColor;
begin
  Result := DecColor(Color, 60);
end;

end.

