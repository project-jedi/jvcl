{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpacesStd.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorSpacesStd;

{$I jvcl.inc}

interface

uses
  ColorSpaces;

type
  // (rom) Should not be needed. TJvColorSpace is already with RTTI.
  {$M+}

  TJvRGBColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvHLSColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvCMYColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvYUVColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvHSVColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvPredefinedColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetName: string; override;
    function GetShortName: TJvShortNameString; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  {$M-}

const
  csHLS = TJvColorID(1);
  csCMY = TJvColorID(2);
  csYUV = TJvColorID(3);
  csHSV = TJvColorID(4);

  RGB_MAX = 255;
  CMY_MAX = 255;
  HLS_MAX = 240;
  HSV_MAX = 240;
  YUV_MIN = 16;
  YUV_MAX = 235;

function RGBToColor(const Color: TJvFullColor): TJvFullColor;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, Graphics;

resourcestring
  RsRed = 'Red';
  RsGreen = 'Green';
  RsBlue = 'Blue';
  RsTrueColor = 'True Color';
  RsRGB = 'RGB';
  RsHue = 'Hue';
  RsLightness = 'Lightness';
  RsSaturation = 'Saturation';
  RsChromaticVision = 'Chromatic Vision';
  RsHLS = 'HLS';
  RsCyan = 'Cyan';
  RsMagenta = 'Magenta';
  RsYellow = 'Yellow';
  RsSubstractiveVision = 'Substractive Vision';
  RsCMY = 'CMY';
  RsYValue = 'Y Value';
  RsUValue = 'U Value';
  RsVValue = 'V Value';
  RsPCVideo = 'PC Video';
  RsYUV = 'YUV';
  RsValue = 'Value';
  RsRotationVision = 'Rotation Vision';
  RsHSV = 'HSV';
  RsNoName= 'No Name';
  RsPredefinedColors = 'Predefined Colors';
  RsDEF = 'DEF';

const
  HLS_MAX_HALF = HLS_MAX / 2.0;
  HLS_MAX_ONE_THIRD = HLS_MAX / 3.0;
  HLS_MAX_TWO_THIRDS = (HLS_MAX * 2.0) / 3.0;
  HLS_MAX_SIXTH = HLS_MAX / 6.0;
  HLS_MAX_TWELVETH = HLS_MAX / 12.0;

function RGBToColor(const Color: TJvFullColor): TJvFullColor;
begin
  Result :=
    ((Color and $000000FF) shl 16) or
    ((Color and $0000FF00)) or
    ((Color and $00FF0000) shr 16);
end;

//=== { TJvRGBColorSpace } ===================================================

function TJvRGBColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := inherited ConvertFromRGB(AColor);
end;

function TJvRGBColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := inherited ConvertToRGB(AColor);
end;

function TJvRGBColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvRGBColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := RGB_MAX;
end;

function TJvRGBColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvRGBColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsRed;
    axIndex1:
      Result := RsGreen;
    axIndex2:
      Result := RsBlue;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvRGBColorSpace.GetName: string;
begin
  Result := RsTrueColor;
end;

function TJvRGBColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsRGB;
end;

//=== { TJvHLSColorSpace } ===================================================

function TJvHLSColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Hue, Lightness, Saturation: Double;
  Red, Green, Blue: Cardinal;
  ColorMax, ColorMin, ColorDiff, ColorSum: Double;
  RedDelta, GreenDelta, BlueDelta: Extended;
begin
  Red := (AColor) and $000000FF;
  Green := (AColor shr 8) and $000000FF;
  Blue := (AColor shr 16) and $000000FF;

  if Red > Green then
    ColorMax := Red
  else
    ColorMax := Green;
  if Blue > ColorMax then
    ColorMax := Blue;
  if Red < Green then
    ColorMin := Red
  else
    ColorMin := Green;
  if Blue < ColorMin then
    ColorMin := Blue;
  ColorDiff := ColorMax - ColorMin;
  ColorSum := ColorMax + ColorMin;

  Lightness := (ColorSum * HLS_MAX + RGB_MAX) / (2.0 * RGB_MAX);
  if ColorMax = ColorMin then
    AColor := (Round(Lightness) shl 8) or (2 * HLS_MAX div 3)
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Saturation := (ColorDiff * HLS_MAX + ColorSum / 2.0) / ColorSum
    else
      Saturation := (ColorDiff * HLS_MAX + ((2.0 * RGB_MAX - ColorMax - ColorMin) / 2.0)) /
        (2.0 * RGB_MAX - ColorMax - ColorMin);

    RedDelta := ((ColorMax - Red) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    GreenDelta := ((ColorMax - Green) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;
    BlueDelta := ((ColorMax - Blue) * HLS_MAX_SIXTH + ColorDiff / 2.0) / ColorDiff;

    if Red = ColorMax then
      Hue := BlueDelta - GreenDelta
    else
    if Green = ColorMax then
      Hue := HLS_MAX_ONE_THIRD + RedDelta - BlueDelta
    else
      Hue := 2.0 * HLS_MAX_ONE_THIRD + GreenDelta - RedDelta;

    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    AColor := (Cardinal(Round(Hue) and $000000FF)) or
      (Cardinal(Round(Lightness) and $000000FF) shl 8) or
      (Cardinal(Round(Saturation) and $000000FF) shl 16);
  end;
  Result := inherited ConvertFromRGB(AColor);
end;

function TJvHLSColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Red, Green, Blue: Double;
  Magic1, Magic2: Double;
  Hue, Lightness, Saturation: Cardinal;

  function HueToRGB(Lightness, Saturation, Hue: Double): Integer;
  var
    ResultEx: Double;
  begin
    if Hue < 0 then
      Hue := Hue + HLS_MAX;
    if Hue > HLS_MAX then
      Hue := Hue - HLS_MAX;

    if Hue < HLS_MAX_SIXTH then
      ResultEx := Lightness + ((Saturation - Lightness) * Hue + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
    if Hue < HLS_MAX_HALF then
      ResultEx := Saturation
    else
    if Hue < HLS_MAX_TWO_THIRDS then
      ResultEx := Lightness + ((Saturation - Lightness) * (HLS_MAX_TWO_THIRDS - Hue) + HLS_MAX_TWELVETH) / HLS_MAX_SIXTH
    else
      ResultEx := Lightness;
    Result := Round(ResultEx);
  end;

  function RoundColor(Value: Double): Integer;
  begin
    if Value > RGB_MAX then
      Result := RGB_MAX
    else
      Result := Round(Value);
  end;

begin
  Hue := (AColor) and $000000FF;
  Lightness := (AColor shr 8) and $000000FF;
  Saturation := (AColor shr 16) and $000000FF;

  if Saturation = 0 then
  begin
    Red := (Lightness * RGB_MAX) / HLS_MAX;
    Green := Red;
    Blue := Red;
  end
  else
  begin
    if Lightness <= HLS_MAX_HALF then
      Magic2 := (Lightness * (HLS_MAX + Saturation) + HLS_MAX_HALF) / HLS_MAX
    else
      Magic2 := Lightness + Saturation - ((Lightness * Saturation) + HLS_MAX_HALF) / HLS_MAX;

    Magic1 := 2 * Lightness - Magic2;

    Red := (HueToRGB(Magic1, Magic2, Hue + HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Green := (HueToRGB(Magic1, Magic2, Hue) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
    Blue := (HueToRGB(Magic1, Magic2, Hue - HLS_MAX_ONE_THIRD) * RGB_MAX + HLS_MAX_HALF) / HLS_MAX;
  end;

  Result := inherited ConvertToRGB(((RoundColor(Red) and $000000FF)) or
    ((RoundColor(Green) and $000000FF) shl 8) or
    ((RoundColor(Blue) and $000000FF) shl 16));
end;

function TJvHLSColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 120;
end;

function TJvHLSColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := HLS_MAX;
end;

function TJvHLSColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvHLSColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsHue;
    axIndex1:
      Result := RsLightness;
    axIndex2:
      Result := RsSaturation;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvHLSColorSpace.GetName: string;
begin
  Result := RsChromaticVision;
end;

function TJvHLSColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsHLS;
end;

//=== { TJvCMYColorSpace } ===================================================

function TJvCMYColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Red, Green, Blue: Integer;
  Cyan, Magenta, Yellow: Integer;
begin
  Red := (AColor) and $000000FF;
  Green := (AColor shr 8) and $000000FF;
  Blue := (AColor shr 16) and $000000FF;

  Cyan := CMY_MAX - Red;
  Magenta := CMY_MAX - Green;
  Yellow := CMY_MAX - Blue;

  Result := inherited ConvertFromRGB(((Cyan and $000000FF)) or
    ((Magenta and $000000FF) shl 8) or
    ((Yellow and $000000FF) shl 16));
end;

function TJvCMYColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Cyan, Magenta, Yellow: Integer;
  Red, Green, Blue: Integer;
begin
  Cyan := (AColor) and $000000FF;
  Magenta := (AColor shr 8) and $000000FF;
  Yellow := (AColor shr 16) and $000000FF;

  Red := CMY_MAX - Cyan;
  Green := CMY_MAX - Magenta;
  Blue := CMY_MAX - Yellow;
  Result := inherited ConvertToRGB(((Red and $000000FF)) or
    ((Green and $000000FF) shl 8) or
    ((Blue and $000000FF) shl 16));
end;

function TJvCMYColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 255;
end;

function TJvCMYColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := CMY_MAX;
end;

function TJvCMYColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvCMYColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsCyan;
    axIndex1:
      Result := RsMagenta;
    axIndex2:
      Result := RsYellow;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvCMYColorSpace.GetName: string;
begin
  Result := RsSubstractiveVision;
end;

function TJvCMYColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsCMY;
end;

//=== { TJvYUVColorSpace } ===================================================

function TJvYUVColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Y, U, V: Integer;
  Red, Green, Blue: Integer;
begin
  Red := (AColor) and $000000FF;
  Green := (AColor shr 8) and $000000FF;
  Blue := (AColor shr 16) and $000000FF;

  Y := Round((0.257 * Red) + (0.504 * Green) + (0.098 * Blue)) + 16;
  V := Round((0.439 * Red) - (0.368 * Green) - (0.071 * Blue)) + 128;
  U := Round(-(0.148 * Red) - (0.291 * Green) + (0.439 * Blue)) + 128;

  Y := EnsureRange(Y, YUV_MIN, YUV_MAX);
  U := EnsureRange(U, YUV_MIN, YUV_MAX);
  V := EnsureRange(V, YUV_MIN, YUV_MAX);

  Result := inherited ConvertFromRGB(Y or
    ((U and $000000FF) shl 8) or
    ((V and $000000FF) shl 16));
end;

function TJvYUVColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Red, Green, Blue: Integer;
  Y, U, V: Integer;
begin
  Y := (AColor) and $000000FF;
  U := (AColor shr 8) and $000000FF;
  V := (AColor shr 16) and $000000FF;

  Y := Y - 16;
  U := U - 128;
  V := V - 128;

  Red := Round((1.164 * Y) - (0.002 * U) + (1.596 * V));
  Green := Round((1.164 * Y) - (0.391 * U) - (0.813 * V));
  Blue := Round((1.164 * Y) + (2.018 * U) - (0.001 * V));

  Red := EnsureRange(Red, 0, RGB_MAX);
  Green := EnsureRange(Green, 0, RGB_MAX);
  Blue := EnsureRange(Blue, 0, RGB_MAX);

  Result := inherited ConvertToRGB(((Red and $000000FF)) or
    ((Green and $000000FF) shl 8) or
    ((Blue and $000000FF) shl 16));
end;

function TJvYUVColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 128;
end;

function TJvYUVColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := YUV_MAX;
end;

function TJvYUVColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := YUV_MIN;
end;

function TJvYUVColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsYValue;
    axIndex1:
      Result := RsUValue;
    axIndex2:
      Result := RsVValue;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvYUVColorSpace.GetName: string;
begin
  Result := RsPCVideo;
end;

function TJvYUVColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsYUV;
end;

//=== { TJvHSVColorSpace } ===================================================

function TJvHSVColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Hue, Saturation, Value: Integer;
  Red, Green, Blue: Byte;
  ColorMax, ColorMin, ColorDelta: Integer;
begin
  Red := AColor and $000000FF;
  Green := (AColor shr 8) and $000000FF;
  Blue := (AColor shr 16) and $000000FF;

  if Red > Green then
    ColorMax := Red
  else
    ColorMax := Green;
  if Blue > ColorMax then
    ColorMax := Blue;

  if Red < Green then
    ColorMin := Red
  else
    ColorMin := Green;
  if Blue < ColorMin then
    ColorMin := Blue;

  ColorDelta := ColorMax - ColorMin;
  Value := ColorMax;

  if Value = 0 then
    Saturation := 0
  else
    Saturation := (255 * ColorDelta) div Value;

  if Saturation = 0 then
    Hue := 0
  else
  begin
    Hue := 0;
    if Value = Red then
      Hue := (40 * (Green - Blue) div ColorDelta);
    if Value = Green then
      Hue := (HSV_MAX div 3) + (40 * (Blue - Red) div ColorDelta);
    if Value = Blue then
      Hue := ((HSV_MAX * 2) div 3) + (40 * (Red - Green) div ColorDelta);
  end;

  if Hue < 0 then
    Hue := Hue + HSV_MAX;
  if Hue > HSV_MAX then
    Hue := Hue - HSV_MAX;

  Result := inherited ConvertFromRGB(((Hue and $000000FF)) or
    ((Saturation and $000000FF) shl 8) or
    ((Value and $000000FF) shl 16));
end;

function TJvHSVColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Hue, Saturation, Value: Integer;
  Red, Green, Blue: Byte;
  P, Q, T, Summ, Rest: Integer;
begin
  Hue := AColor and $000000FF;
  Saturation := (AColor shr 8) and $000000FF;
  Value := (AColor shr 16) and $000000FF;

  if Saturation = 0 then
  begin
    Red := Value;
    Green := Value;
    Blue := Value;
  end
  else
  begin
    if Hue = HSV_MAX then
      Hue := 0;

    Rest := Hue mod (HSV_MAX div 6);
    Hue := Hue div (HSV_MAX div 6);

    Summ := Value * Saturation;

    P := Value - Summ div RGB_MAX;
    Q := Value - (Summ * Rest) div (RGB_MAX * (HSV_MAX div 6));
    T := Value - (Summ * ((HSV_MAX div 6) - Rest)) div (RGB_MAX * (HSV_MAX div 6));
    case Hue of
      0:
        begin
          Red := Value;
          Green := T;
          Blue := P;
        end;
      1:
        begin
          Red := Q;
          Green := Value;
          Blue := P;
        end;
      2:
        begin
          Red := P;
          Green := Value;
          Blue := T;
        end;
      3:
        begin
          Red := P;
          Green := Q;
          Blue := Value;
        end;
      4:
        begin
          Red := T;
          Green := P;
          Blue := Value;
        end;
    else
      Red := Value;
      Green := P;
      Blue := Q;
    end;
  end;

  Result := inherited ConvertToRGB(((Red and $000000FF)) or
    ((Green and $000000FF) shl 8) or
    ((Blue and $000000FF) shl 16));
end;

function TJvHSVColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  case Index of
    axIndex0:
      Result := 120;
    axIndex1:
      Result := 240;
  else
    Result := 150;
  end;
end;

function TJvHSVColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  case Index of
    axIndex0:
      Result := HSV_MAX;
  else
    Result := RGB_MAX;
  end;
end;

function TJvHSVColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvHSVColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsHue;
    axIndex1:
      Result := RsSaturation;
    axIndex2:
      Result := RsValue;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvHSVColorSpace.GetName: string;
begin
  Result := RsRotationVision;
end;

function TJvHSVColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsHSV;
end;

//=== { TJvPredefinedColorSpace } ============================================

function TJvPredefinedColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Index: Integer;
  NewColor: TColor;
  TestColor: Integer;
begin
  NewColor := clBtnFace;
  AColor := ColorToRGB(AColor);

  with ColorSpaceManager do
    for Index := (StandardColorsCount + ExtendedColorsCount) to PredefinedColorCount - 1 do
      if IdentToColor(PredefinedColorIndex[Index], TestColor) and
        (Integer(AColor) = ColorToRGB(TestColor)) then
      begin
        NewColor := TestColor;
        Break;
      end;

  Result := inherited ConvertFromRGB(NewColor);
end;

function TJvPredefinedColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := inherited ConvertToRGB(ColorToRGB(AColor));
end;

function TJvPredefinedColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvPredefinedColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  Result := RsNoName;
end;

function TJvPredefinedColorSpace.GetName: string;
begin
  Result := RsPredefinedColors;
end;

function TJvPredefinedColorSpace.GetShortName: TJvShortNameString;
begin
  Result := RsDEF;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

initialization
  ColorSpaceManager.RegisterColorSpace(TJvRGBColorSpace.Create(csRGB));
  ColorSpaceManager.RegisterColorSpace(TJvHLSColorSpace.Create(csHLS));
  ColorSpaceManager.RegisterColorSpace(TJvCMYColorSpace.Create(csCMY));
  ColorSpaceManager.RegisterColorSpace(TJvYUVColorSpace.Create(csYUV));
  ColorSpaceManager.RegisterColorSpace(TJvHSVColorSpace.Create(csHSV));
  ColorSpaceManager.RegisterColorSpace(TJvPredefinedColorSpace.Create(csPredefined));
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

