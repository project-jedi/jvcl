{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpaces.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorSpaces;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils,
  JvTypes;

type
  TJvAxisIndex = (axIndex0, axIndex1, axIndex2);
  TJvColorID = type Byte;
  TJvFullColor = type Cardinal;

const
  csRGB = TJvColorID(0);
  csHLS = TJvColorID(1);
  csCMY = TJvColorID(2);
  csYUV = TJvColorID(3);
  csHSV = TJvColorID(4);
  csDEF = TJvColorID(128);

  RGB_MIN = 0;
  RGB_MAX = 255;
  HLS_MIN = 0;
  HLS_MAX = 240;
  CMY_MIN = 0;
  CMY_MAX = 255;
  YUV_MIN = 16;
  YUV_MAX = 235;
  HSV_MIN = 0;
  HSV_MAX = 240;
  DEF_MIN = 0;
  DEF_MAX = 255;

type
  TJvColorSpace = class(TPersistent)
  private
    FID: TJvColorID;
  protected
    function GetAxisName(Index: TJvAxisIndex): string; virtual;
    function GetAxisMin(Index: TJvAxisIndex): Byte; virtual;
    function GetAxisMax(Index: TJvAxisIndex): Byte; virtual;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; virtual;
    function GetName: string; virtual;
    function GetShortName: string; virtual;
  public
    constructor Create(ColorID: TJvColorID); virtual;
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; virtual;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; virtual;
    property ID: TJvColorID read FID;
    property Name: string read GetName;
    property ShortName: string read GetShortName;
    property AxisName[Index: TJvAxisIndex]: string read GetAxisName;
    property AxisMin[Index: TJvAxisIndex]: Byte read GetAxisMin;
    property AxisMax[Index: TJvAxisIndex]: Byte read GetAxisMax;
    property AxisDefault[Index: TJvAxisIndex]: Byte read GetAxisDefault;
  end;

  TJvRGBColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: string; override;
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
    function GetShortName: string; override;
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
    function GetShortName: string; override;
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
    function GetShortName: string; override;
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
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvDEFColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromRGB(AColor: TJvFullColor): TJvFullColor; override;
    function ConvertToRGB(AColor: TJvFullColor): TJvFullColor; override;
  end;

  TJvColorSpaceManager = class(TPersistent)
  private
    FColorSpaceList: TList;
    FPredefinedColors: TStringList;
    function GetColorSpaceCount: Integer;
    function GetColorSpaceIndex(Index: Integer): TJvColorSpace;
    procedure AddPredefinedColor(const S: string);
    function GetPredefinedColorCount: Integer;
    function GetPredefinedColorIndex(Index: Integer): string;
  protected
    function GetColorSpace(ID: TJvColorID): TJvColorSpace; virtual;
  public
    procedure RegisterColorSpace(NewColorSpace: TJvColorSpace);
    constructor Create;
    destructor Destroy; override;
    function ConvertToID(AColor: TJvFullColor; NewID: TJvColorID): TJvFullColor;
    function GetColorID(AColor: TJvFullColor): TJvColorID;

    property ColorSpace[ID: TJvColorID]: TJvColorSpace read GetColorSpace;
    property ColorSpaceIndex[Index: Integer]: TJvColorSpace read GetColorSpaceIndex;
    property ColorSpaceCount: Integer read GetColorSpaceCount;

    property PredefinedColorIndex[Index: Integer]: string read GetPredefinedColorIndex;
    property PredefinedColorCount: Integer read GetPredefinedColorCount;
  end;

  EJvColorSpaceError = class(EJVCLException);

function ColorSpaceManager: TJvColorSpaceManager;
function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
function SetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex; NewValue: Byte): TJvFullColor;

function RGBToColor(const Color: TJvFullColor): TJvFullColor;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Math, Graphics;

var
  GlobalColorSpaceManager: TJvColorSpaceManager = nil;

resourcestring
  RsUnnamedColorAxis = 'Unnamed Color Axis';
  RsUnnamedColorSpace = 'Unnamed Color Space';
  RsUCS = 'UCS';
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
  RsEColorSpaceNotFound = 'Color Space not found: %d';
  RsEColorSpaceAlreadyExists = 'Color Space Already exists [ID: %d, Name: %s]';

const
  HLS_MAX_HALF = HLS_MAX / 2.0;
  HLS_MAX_ONE_THIRD = HLS_MAX / 3.0;
  HLS_MAX_TWO_THIRDS = (HLS_MAX * 2.0) / 3.0;
  HLS_MAX_SIXTH = HLS_MAX / 6.0;
  HLS_MAX_TWELVETH = HLS_MAX / 12.0;

function ColorSpaceManager: TJvColorSpaceManager;
begin
  if GlobalColorSpaceManager = nil then
    GlobalColorSpaceManager := TJvColorSpaceManager.Create;
  Result := GlobalColorSpaceManager;
end;

function SetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex;
  NewValue: Byte): TJvFullColor;
begin
  case AAxis of
    axIndex0:
      AColor := (AColor and $FFFFFF00) or (NewValue shl 0);
    axIndex1:
      AColor := (AColor and $FFFF00FF) or (NewValue shl 8);
    axIndex2:
      AColor := (AColor and $FF00FFFF) or (NewValue shl 16);
  end;
  Result := AColor;
end;

function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
begin
  case AAxis of
    axIndex0:
      Result := (AColor and $000000FF) shr 0;
    axIndex1:
      Result := (AColor and $0000FF00) shr 8;
    axIndex2:
      Result := (AColor and $00FF0000) shr 16;
  else
    Result := 0;
  end;
end;

function RGBToColor(const Color: TJvFullColor): TJvFullColor;
begin
  Result :=
    ((Color and $000000FF) shl 16) or
    ((Color and $0000FF00)) or
    ((Color and $00FF0000) shr 16);
end;

procedure SplitColorParts(AColor: TJvFullColor; var Part1, Part2, Part3: Cardinal);
begin
  Part1 := AColor and $000000FF;
  Part2 := (AColor shr 8) and $000000FF;
  Part3 := (AColor shr 16) and $000000FF;
end;

function JoinColorParts(Part1, Part2, Part3: Cardinal): TJvFullColor;
begin
  Result :=
    (Part1 and $000000FF) or
    ((Part2 and $000000FF) shl 8) or
    ((Part3 and $000000FF) shl 16);
end;

//=== { TJvColorSpace } ======================================================

function TJvColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := (AColor and $00FFFFFF) or (ID shl 24);
end;

function TJvColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := (AColor and $00FFFFFF) or (csRGB shl 24);
end;

constructor TJvColorSpace.Create(ColorID: TJvColorID);
begin
  inherited Create;
  FID := ColorID;
end;

function TJvColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TJvColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := High(Byte);
end;

function TJvColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := Low(Byte);
end;

function TJvColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  Result := RsUnnamedColorAxis;
end;

function TJvColorSpace.GetName: string;
begin
  Result := RsUnnamedColorSpace;
end;

function TJvColorSpace.GetShortName: string;
begin
  Result := RsUCS;
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
  Result := RGB_MIN;
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

function TJvRGBColorSpace.GetShortName: string;
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
  SplitColorParts(AColor, Red, Green, Blue);

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

    AColor :=
      JoinColorParts(Cardinal(Round(Hue)), Cardinal(Round(Lightness)), Cardinal(Round(Saturation)));
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
  SplitColorParts(AColor, Hue, Lightness, Saturation);

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

  Result := inherited ConvertToRGB(
    JoinColorParts(RoundColor(Red), RoundColor(Green), RoundColor(Blue)));
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
  Result := HLS_MIN;
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

function TJvHLSColorSpace.GetShortName: string;
begin
  Result := RsHLS;
end;

//=== { TJvCMYColorSpace } ===================================================

function TJvCMYColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Red, Green, Blue: Cardinal;
begin
  SplitColorParts(AColor, Red, Green, Blue);
  Result := inherited ConvertFromRGB(
    JoinColorParts(CMY_MAX - Red, CMY_MAX - Green, CMY_MAX - Blue));
end;

function TJvCMYColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Cyan, Magenta, Yellow: Cardinal;
begin
  SplitColorParts(AColor, Cyan, Magenta, Yellow);
  Result := inherited ConvertToRGB(JoinColorParts(CMY_MAX - Cyan, CMY_MAX - Magenta, CMY_MAX - Yellow));
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
  Result := CMY_MIN;
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

function TJvCMYColorSpace.GetShortName: string;
begin
  Result := RsCMY;
end;

//=== { TJvYUVColorSpace } ===================================================

function TJvYUVColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
var
  Y, U, V: Cardinal;
  Red, Green, Blue: Cardinal;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  Y := Round((0.257 * Red) + (0.504 * Green) + (0.098 * Blue)) + 16;
  V := Round((0.439 * Red) - (0.368 * Green) - (0.071 * Blue)) + 128;
  U := Round(-(0.148 * Red) - (0.291 * Green) + (0.439 * Blue)) + 128;

  Y := EnsureRange(Y, YUV_MIN, YUV_MAX);
  U := EnsureRange(U, YUV_MIN, YUV_MAX);
  V := EnsureRange(V, YUV_MIN, YUV_MAX);

  Result := inherited ConvertFromRGB(JoinColorParts(Y, U, V));
end;

function TJvYUVColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Red, Green, Blue: Cardinal;
  Y, U, V: Cardinal;
begin
  SplitColorParts(AColor, Y, U, V);

  Y := Y - 16;
  U := U - 128;
  V := V - 128;

  Red := Round((1.164 * Y) - (0.002 * U) + (1.596 * V));
  Green := Round((1.164 * Y) - (0.391 * U) - (0.813 * V));
  Blue := Round((1.164 * Y) + (2.018 * U) - (0.001 * V));

  Red := EnsureRange(Red, RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToRGB(JoinColorParts(Red, Green, Blue));
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

function TJvYUVColorSpace.GetShortName: string;
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

  Result := inherited ConvertFromRGB(JoinColorParts(Hue, Saturation, Value));
end;

function TJvHSVColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
var
  Hue, Saturation, Value: Cardinal;
  Red, Green, Blue: Cardinal;
  P, Q, T, Summ, Rest: Cardinal;
begin
  SplitColorParts(AColor, Hue, Saturation, Value);

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

  Result := inherited ConvertToRGB(JoinColorParts(Red, Green, Blue));
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

function TJvHSVColorSpace.GetShortName: string;
begin
  Result := RsHSV;
end;

//=== { TJvDEFColorSpace } ===================================================

function TJvDEFColorSpace.ConvertFromRGB(AColor: TJvFullColor): TJvFullColor;
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

function TJvDEFColorSpace.ConvertToRGB(AColor: TJvFullColor): TJvFullColor;
begin
  Result := inherited ConvertToRGB(ColorToRGB(AColor));
end;

function TJvDEFColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvDEFColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  Result := RsNoName;
end;

function TJvDEFColorSpace.GetName: string;
begin
  Result := RsPredefinedColors;
end;

function TJvDEFColorSpace.GetShortName: string;
begin
  Result := RsDEF;
end;

//=== { TJvColorSpaceManager } ===============================================

constructor TJvColorSpaceManager.Create;
begin
  inherited Create;
  FColorSpaceList := TList.Create;
  FPredefinedColors := TStringList.Create;
  GetColorValues(AddPredefinedColor);
end;

destructor TJvColorSpaceManager.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
    TJvColorSpace(FColorSpaceList.Items[Index]).Free;
  FColorSpaceList.Free;
  FPredefinedColors.Free;
  inherited Destroy;
end;

procedure TJvColorSpaceManager.AddPredefinedColor(const S: string);
begin
  FPredefinedColors.Add(S);
end;

function TJvColorSpaceManager.ConvertToID(AColor: TJvFullColor; NewID: TJvColorID): TJvFullColor;
var
  LColorID: TJvColorID;
begin
  LColorID := GetColorID(AColor);
  if LColorID = NewID then
    Result := AColor
  else
  begin
    if LColorID <> csRGB then
      AColor := ColorSpace[LColorID].ConvertToRGB(AColor);
    Result := ColorSpace[NewID].ConvertFromRGB(AColor);
  end;
end;

function TJvColorSpaceManager.GetColorID(AColor: TJvFullColor): TJvColorID;
var
  Index: Integer;
begin
  Result := TJvColorID(AColor shr 24);
  for Index := 0 to ColorSpaceCount - 1 do
    if ColorSpaceIndex[Index].ID = Result then
      Exit;
  Result := csDEF;
end;

function TJvColorSpaceManager.GetColorSpace(ID: TJvColorID): TJvColorSpace;
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TJvColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = ID then
    begin
      Result := LColorSpace;
      Exit;
    end;
  end;
  raise EJvColorSpaceError.CreateFmt(RsEColorSpaceNotFound, [ID]);
end;

function TJvColorSpaceManager.GetColorSpaceCount: Integer;
begin
  Result := FColorSpaceList.Count;
end;

function TJvColorSpaceManager.GetColorSpaceIndex(Index: Integer): TJvColorSpace;
begin
  Result := TJvColorSpace(FColorSpaceList.Items[Index]);
end;

function TJvColorSpaceManager.GetPredefinedColorCount: Integer;
begin
  Result := FPredefinedColors.Count;
end;

function TJvColorSpaceManager.GetPredefinedColorIndex(Index: Integer): string;
begin
  Result := FPredefinedColors.Strings[Index];
end;

procedure TJvColorSpaceManager.RegisterColorSpace(NewColorSpace: TJvColorSpace);
var
  Index: Integer;
  LColorSpace: TJvColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    LColorSpace := TJvColorSpace(FColorSpaceList.Items[Index]);
    if LColorSpace.ID = NewColorSpace.ID then
      with LColorSpace do
      begin
        EJvColorSpaceError.CreateFmt(RsEColorSpaceAlreadyExists, [ID, Name]);
        Exit;
      end;
  end;
  FColorSpaceList.Add(Pointer(NewColorSpace));
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
  ColorSpaceManager.RegisterColorSpace(TJvDEFColorSpace.Create(csDEF));
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(GlobalColorSpaceManager);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

