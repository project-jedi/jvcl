{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpaces.pas, released on 2004-09-11.

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
  Windows, Classes, SysUtils, Graphics,
  JvTypes;

type
  TJvAxisIndex = (axIndex0, axIndex1, axIndex2);
  TJvColorSpaceID = type Byte;
  TJvFullColor = type Cardinal;

const
  csRGB = TJvColorSpaceID(1 shl 2);
  csHLS = TJvColorSpaceID(2 shl 2);
  csCMY = TJvColorSpaceID(3 shl 2);
  csYUV = TJvColorSpaceID(4 shl 2);
  csHSV = TJvColorSpaceID(5 shl 2);
  csYIQ = TJvColorSpaceID(6 shl 2);
  csYCC = TJvColorSpaceID(7 shl 2);
  csXYZ = TJvColorSpaceID(8 shl 2);
  csLAB = TJvColorSpaceID(9 shl 2);
  csDEF = TJvColorSpaceID(10 shl 2);

  csMIN = csRGB;
  csMAX = csDEF;

  csID_MASK = $FC;

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
  YIQ_MIN = 0;
  YIQ_MAX = 255;
  YCC_MIN = 0;
  YCC_MAX = 255;
  XYZ_MIN = 0;
  XYZ_MAX = 255;
  LAB_MIN = 0;
  LAB_MAX = 255;
  DEF_MIN = 0;
  DEF_MAX = 255;

  fclRGBBlack = TJvFullColor((Ord(csRGB) shl 24) or $000000);
  fclRGBWhite = TJvFullColor((Ord(csRGB) shl 24) or $FFFFFF);
  fclRGBRed   = TJvFullColor((Ord(csRGB) shl 24) or $0000FF);
  fclRGBLime  = TJvFullColor((Ord(csRGB) shl 24) or $00FF00);
  fclRGBBlue  = TJvFullColor((Ord(csRGB) shl 24) or $FF0000);

  fclDEFWindowText = TJvFullColor((Ord(csDEF) shl 24) or COLOR_WINDOWTEXT);

type
  TJvColorSpace = class(TPersistent)
  private
    FID: TJvColorSpaceID;
  protected
    function GetAxisName(Index: TJvAxisIndex): string; virtual;
    function GetAxisMin(Index: TJvAxisIndex): Byte; virtual;
    function GetAxisMax(Index: TJvAxisIndex): Byte; virtual;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; virtual;
    function GetName: string; virtual;
    function GetShortName: string; virtual;
    function GetNumberOfColors: Cardinal; virtual;
  public
    constructor Create(ColorID: TJvColorSpaceID); virtual;
    function ConvertFromColor(AColor: TColor): TJvFullColor; virtual;
    function ConvertToColor(AColor: TJvFullColor): TColor; virtual;
    property ID: TJvColorSpaceID read FID;
    property NumberOfColors: Cardinal read GetNumberOfColors;
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
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
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
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
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
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
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
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
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
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
  end;

  TJvYIQColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
  end;

  TJvYCCColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
  end;

  TJvXYZColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
  end;

  TJvLABColorSpace = class(TJvColorSpace)
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetAxisMin(Index: TJvAxisIndex): Byte; override;
    function GetAxisMax(Index: TJvAxisIndex): Byte; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
  public
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
  end;
  
  TJvDEFColorSpace = class(TJvColorSpace)
  private
    FDelphiColors: TStringList;
    procedure AddDelphiColor(const S: string);
  protected
    function GetAxisName(Index: TJvAxisIndex): string; override;
    function GetName: string; override;
    function GetShortName: string; override;
    function GetAxisDefault(Index: TJvAxisIndex): Byte; override;
    function GetNumberOfColors: Cardinal; override;
  public
    constructor Create(ColorID: TJvColorSpaceID); override;
    destructor Destroy; override;
    function ConvertFromColor(AColor: TColor): TJvFullColor; override;
    function ConvertToColor(AColor: TJvFullColor): TColor; override;
    function ColorName(Index: Integer): string;
  end;

  TJvColorSpaceManager = class(TPersistent)
  private
    FColorSpaceList: TList;
    function GetCount: Integer;
    function GetColorSpaceByIndex(Index: Integer): TJvColorSpace;
  protected
    function GetColorSpace(ID: TJvColorSpaceID): TJvColorSpace; virtual;
  public
    procedure RegisterColorSpace(NewColorSpace: TJvColorSpace);
    constructor Create;
    destructor Destroy; override;
    function ConvertToID(AColor: TJvFullColor; DestID: TJvColorSpaceID): TJvFullColor;
    function ConvertToColor(AColor: TJvFullColor): TColor;
    function ConvertFromColor(AColor: TColor): TJvFullColor;
    function GetColorSpaceID(AColor: TJvFullColor): TJvColorSpaceID;

    property ColorSpace[ID: TJvColorSpaceID]: TJvColorSpace read GetColorSpace;
    property ColorSpaceByIndex[Index: Integer]: TJvColorSpace read GetColorSpaceByIndex;
    property Count: Integer read GetCount;
  end;

  EJvColorSpaceError = class(EJVCLException);

function ColorSpaceManager: TJvColorSpaceManager;
function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
function SetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex; NewValue: Byte): TJvFullColor;

function RGBToBGR(Value: Cardinal):Cardinal;

procedure SplitColorParts(AColor: TJvFullColor; var Part1, Part2, Part3: Integer);
function JoinColorParts(const Part1, Part2, Part3: Integer): TJvFullColor;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;

var
  GlobalColorSpaceManager: TJvColorSpaceManager = nil;

resourcestring
  RsErr_UnnamedAxis        = 'Unnamed Color Axis';
  RsErr_UnnamedSpace       = 'Unnamed Color Space';
  RsErr_UCS                = 'UCS';
  RsErr_NoName             = 'No Name';
  RsErr_CSNotFound         = 'Color Space not found: %d';
  RsErr_IllegalID          = 'Color Space ID %d is illegal';
  RsErr_CSAlreadyExists    = 'Color Space Already exists [ID: %d, Name: %s]';
  RsErr_InconvertibleColor = 'TColor value $%.8X cannot be converted to TJvFullColor';

  RsRGB_Red       = 'Red';
  RsRGB_Green     = 'Green';
  RsRGB_Blue      = 'Blue';
  RsRGB_FullName  = 'True Color';
  RsRGB_ShortName = 'RGB';

  RsHLS_Hue        = 'Hue';
  RsHLS_Lightness  = 'Lightness';
  RsHLS_Saturation = 'Saturation';
  RsHLS_FullName   = 'Chromatic Vision';
  RsHLS_ShortName  = 'HLS';

  RsCMY_Cyan      = 'Cyan';
  RsCMY_Magenta   = 'Magenta';
  RsCMY_Yellow    = 'Yellow';
  RsCMY_FullName  = 'Substractive Vision';
  RsCMY_ShortName = 'CMY';

  RsYUV_Y         = 'Y Value';
  RsYUV_U         = 'U Value';
  RsYUV_V         = 'V Value';
  RsYUV_FullName  = 'PC Video';
  RsYUV_ShortName = 'YUV';

  RsHSV_Hue        = 'Hue';
  RsHSV_Saturation = 'Saturation';
  RsHSV_Value      = 'Value';
  RsHSV_FullName   = 'Rotation Vision';
  RsHSV_ShortName  = 'HSV';

  RsYIQ_Y         = 'Y';
  RsYIQ_I         = 'I';
  RsYIQ_Q         = 'Q';
  RsYIQ_FullName  = 'NTSC US television standard';
  RsYIQ_ShortName = 'YIQ';

  RsYCC_Y         = 'Y';
  RsYCC_Cr        = 'Cr';
  RsYCC_Cb        = 'Cb';
  RsYCC_FullName  = 'YCrCb';
  RsYCC_ShortName = 'YCC';

  RsXYZ_X         = 'Y';
  RsXYZ_Y         = 'Y';
  RsXYZ_Z         = 'Z';
  RsXYZ_FullName  = 'CIE XYZ';
  RsXYZ_ShortName = 'XYZ';

  RsLAB_L         = 'L';
  RsLAB_A         = 'A';
  RsLAB_B         = 'B';
  RsLAB_FullName  = 'CIE LAB';
  RsLAB_ShortName = 'LAB';

  RsDEF_FullName  = 'Delphi predefined colors';
  RsDEF_ShortName = 'DEF';

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
      AColor := (AColor and $FFFFFF00) or  NewValue;
    axIndex1:
      AColor := (AColor and $FFFF00FF) or (NewValue shl  8);
    axIndex2:
      AColor := (AColor and $FF00FFFF) or (NewValue shl 16);
  end;
  Result := AColor;
end;

function GetAxisValue(AColor: TJvFullColor; AAxis: TJvAxisIndex): Byte;
begin
  case AAxis of
    axIndex0:
      Result := (AColor and $000000FF);
    axIndex1:
      Result := (AColor and $0000FF00) shr  8;
    axIndex2:
      Result := (AColor and $00FF0000) shr 16;
  else
    Result := 0;
  end;
end;

function RGBToBGR(Value: Cardinal): Cardinal;
begin
  Result :=
   ((Value and $00FF0000) shr 16) or
    (Value and $0000FF00) or
   ((Value and $000000FF) shl 16);
end;

procedure SplitColorParts(AColor: TJvFullColor; var Part1, Part2, Part3: Integer);
begin
  Part1 :=  AColor         and $000000FF;
  Part2 := (AColor shr 8)  and $000000FF;
  Part3 := (AColor shr 16) and $000000FF;
end;

function JoinColorParts(const Part1, Part2, Part3: Integer): TJvFullColor;
begin
  Result :=
     (Part1 and $000000FF) or
    ((Part2 and $000000FF) shl 8) or
    ((Part3 and $000000FF) shl 16);
end;

//=== { TJvColorSpace } ======================================================

constructor TJvColorSpace.Create(ColorID: TJvColorSpaceID);
begin
  inherited Create;
  if (ColorID >= csMIN) and (ColorID <= csMAX) then
    FID := ColorID
  else
    raise EJvColorSpaceError.CreateResFmt(@RsErr_IllegalID, [Ord(ColorID)]);
end;

function TJvColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
begin
  Result := (AColor and $00FFFFFF) or (ID shl 24);
end;

function TJvColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
begin
  Result := AColor and $00FFFFFF;
end;

function TJvColorSpace.GetNumberOfColors: Cardinal;
begin
  Result :=
    (AxisMax[axIndex0] - AxisMin[axIndex0] + 1) *
    (AxisMax[axIndex1] - AxisMin[axIndex1] + 1) *
    (AxisMax[axIndex2] - AxisMin[axIndex2] + 1);
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
  Result := RsErr_UnnamedAxis;
end;

function TJvColorSpace.GetName: string;
begin
  Result := RsErr_UnnamedSpace;
end;

function TJvColorSpace.GetShortName: string;
begin
  Result := RsErr_UCS;
end;

//=== { TJvRGBColorSpace } ===================================================

function TJvRGBColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
begin
  Result := inherited ConvertFromColor(AColor);
end;

function TJvRGBColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
begin
  Result := inherited ConvertToColor(AColor);
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
      Result := RsRGB_Red;
    axIndex1:
      Result := RsRGB_Green;
    axIndex2:
      Result := RsRGB_Blue;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvRGBColorSpace.GetName: string;
begin
  Result := RsRGB_FullName;
end;

function TJvRGBColorSpace.GetShortName: string;
begin
  Result := RsRGB_ShortName;
end;

//=== { TJvHLSColorSpace } ===================================================

function TJvHLSColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Hue, Lightness, Saturation: Double;
  Red, Green, Blue: Integer;
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
  Result := inherited ConvertFromColor(AColor);
end;

function TJvHLSColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Double;
  Magic1, Magic2: Double;
  Hue, Lightness, Saturation: Integer;

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

  Result := inherited ConvertToColor(
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
      Result := RsHLS_Hue;
    axIndex1:
      Result := RsHLS_Lightness;
    axIndex2:
      Result := RsHLS_Saturation;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvHLSColorSpace.GetName: string;
begin
  Result := RsHLS_FullName;
end;

function TJvHLSColorSpace.GetShortName: string;
begin
  Result := RsHLS_ShortName;
end;

//=== { TJvCMYColorSpace } ===================================================

function TJvCMYColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Red, Green, Blue: Integer;
  Cyan, Magenta, Yellow: Integer;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  Cyan    := ((RGB_MAX - Red  ) * (CMY_MAX-CMY_MIN+1) div (RGB_MAX-RGB_MIN+1)) + CMY_MIN;
  Magenta := ((RGB_MAX - Green) * (CMY_MAX-CMY_MIN+1) div (RGB_MAX-RGB_MIN+1)) + CMY_MIN;
  Yellow  := ((RGB_MAX - Blue ) * (CMY_MAX-CMY_MIN+1) div (RGB_MAX-RGB_MIN+1)) + CMY_MIN;

  Result := inherited ConvertFromColor(JoinColorParts(Cyan, Magenta, Yellow));
end;

function TJvCMYColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Cyan, Magenta, Yellow: Integer;
  Red, Green, Blue: Integer;
begin
  SplitColorParts(AColor, Cyan, Magenta, Yellow);

  Red   := ((CMY_MAX - Cyan   ) * (RGB_MAX-RGB_MIN+1) div (CMY_MAX-CMY_MIN+1)) + RGB_MIN;
  Green := ((CMY_MAX - Magenta) * (RGB_MAX-RGB_MIN+1) div (CMY_MAX-CMY_MIN+1)) + RGB_MIN;
  Blue  := ((CMY_MAX - Yellow ) * (RGB_MAX-RGB_MIN+1) div (CMY_MAX-CMY_MIN+1)) + RGB_MIN;

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
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
      Result := RsCMY_Cyan;
    axIndex1:
      Result := RsCMY_Magenta;
    axIndex2:
      Result := RsCMY_Yellow;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvCMYColorSpace.GetName: string;
begin
  Result := RsCMY_FullName;
end;

function TJvCMYColorSpace.GetShortName: string;
begin
  Result := RsCMY_ShortName;
end;

//=== { TJvYUVColorSpace } ===================================================

function TJvYUVColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Y, U, V: Integer;
  Red, Green, Blue: Integer;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  Y := Round(0.257*Red + 0.504*Green + 0.098*Blue) + 16;
  V := Round(0.439*Red - 0.368*Green - 0.071*Blue) + 128;
  U := Round(-0.148*Red - 0.291*Green + 0.439*Blue) + 128;

  Y := EnsureRange(Y, YUV_MIN, YUV_MAX);
  U := EnsureRange(U, YUV_MIN, YUV_MAX);
  V := EnsureRange(V, YUV_MIN, YUV_MAX);

  Result := inherited ConvertFromColor(JoinColorParts(Y, U, V));
end;

function TJvYUVColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Integer;
  Y, U, V: Integer;
begin
  SplitColorParts(AColor, Y, U, V);

  Y := Y - 16;
  U := U - 128;
  V := V - 128;

  Red := Round(1.164*Y - 0.002*U + 1.596*V);
  Green := Round(1.164*Y - 0.391*U - 0.813*V);
  Blue := Round(1.164*Y + 2.018*U - 0.001*V);

  Red := EnsureRange(Red , RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
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
      Result := RsYUV_Y;
    axIndex1:
      Result := RsYUV_U;
    axIndex2:
      Result := RsYUV_V;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvYUVColorSpace.GetName: string;
begin
  Result := RsYUV_FullName;
end;

function TJvYUVColorSpace.GetShortName: string;
begin
  Result := RsYUV_ShortName;
end;

//=== { TJvHSVColorSpace } ===================================================

function TJvHSVColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Hue, Saturation, Value: Integer;
  Red, Green, Blue: Integer;
  ColorMax, ColorMin, ColorDelta: Integer;
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

  Result := inherited ConvertFromColor(JoinColorParts(Hue, Saturation, Value));
end;

function TJvHSVColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Hue, Saturation, Value: Integer;
  Red, Green, Blue: Integer;
  P, Q, T, Summ, Rest: Integer;
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

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
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
  Result := HSV_MIN;
end;

function TJvHSVColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsHSV_Hue;
    axIndex1:
      Result := RsHSV_Saturation;
    axIndex2:
      Result := RsHSV_Value;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvHSVColorSpace.GetName: string;
begin
  Result := RsHSV_FullName;
end;

function TJvHSVColorSpace.GetShortName: string;
begin
  Result := RsHSV_ShortName;
end;

//=== { TJvYIQColorSpace } ===================================================

function TJvYIQColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Y, I, Q: Integer;
  Red, Green, Blue: Integer;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  Y := Round(0.299*Red + 0.587*Green + 0.114*Blue);
  I := Round(0.596*Red - 0.275*Green - 0.321*Blue) + 128;
  Q := Round(0.212*Red - 0.523*Green + 0.311*Blue) + 128;

  Y := EnsureRange(Y, YIQ_MIN, YIQ_MAX);
  I := EnsureRange(I, YIQ_MIN, YIQ_MAX);
  Q := EnsureRange(Q, YIQ_MIN, YIQ_MAX);

  Result := inherited ConvertFromColor(JoinColorParts(Y, I, Q));
end;

function TJvYIQColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Integer;
  Y, I, Q: Integer;
begin
  SplitColorParts(AColor, Y, I, Q);

  //Y := Y;
  I := I - 128;
  Q := Q - 128;

  Red := Round(Y + 0.956*I + 0.620*Q);
  Green := Round(Y - 0.272*I - 0.647*Q);
  Blue := Round(Y - 1.108*I + 1.705*Q);

  Red := EnsureRange(Red , RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
end;

function TJvYIQColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 128;
end;

function TJvYIQColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := YIQ_MAX;
end;

function TJvYIQColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := YIQ_MIN;
end;

function TJvYIQColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsYIQ_Y;
    axIndex1:
      Result := RsYIQ_I;
    axIndex2:
      Result := RsYIQ_Q;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvYIQColorSpace.GetName: string;
begin
  Result := RsYIQ_FullName;
end;

function TJvYIQColorSpace.GetShortName: string;
begin
  Result := RsYIQ_ShortName;
end;

//=== { TJvYCCColorSpace } ===================================================

function TJvYCCColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  Y, Cr, Cb: Integer;
  Red, Green, Blue: Integer;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  Y  := Round( 0.299*Red + 0.587*Green + 0.114*Blue);
  Cr := Round(-0.150*Red - 0.293*Green + 0.443*Blue) + 128;
  Cb := Round( 0.438*Red - 0.367*Green - 0.071*Blue) + 128;

  Y  := EnsureRange(Y,  YCC_MIN, YCC_MAX);
  Cr := EnsureRange(Cr, YCC_MIN, YCC_MAX);
  Cb := EnsureRange(Cb, YCC_MIN, YCC_MAX);

  Result := inherited ConvertFromColor(JoinColorParts(Y, Cr, Cb));
end;

function TJvYCCColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Integer;
  Y, Cr, Cb: Integer;
begin
  SplitColorParts(AColor, Y, Cr, Cb);

  Y  := Y;
  Cr := Cr - 128;
  Cb := Cb - 128;

  Red := Round(Y - 0.001*Cr + 1.600*Cb);
  Green := Round(Y - 0.388*Cr - 0.816*Cb);
  Blue := Round(Y + 2.000*Cr + 0.002*Cb);

  Red := EnsureRange(Red , RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
end;

function TJvYCCColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 128;
end;

function TJvYCCColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := YCC_MAX;
end;

function TJvYCCColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := YCC_MIN;
end;

function TJvYCCColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsYCC_Y;
    axIndex1:
      Result := RsYCC_Cr;
    axIndex2:
      Result := RsYCC_Cb;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvYCCColorSpace.GetName: string;
begin
  Result := RsYCC_FullName;
end;

function TJvYCCColorSpace.GetShortName: string;
begin
  Result := RsYCC_ShortName;
end;

//=== { TJvXYZColorSpace } ===================================================

function TJvXYZColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  X, Y, Z: Integer;
  Red, Green, Blue: Integer;
begin
  SplitColorParts(AColor, Red, Green, Blue);

  X := Round( 0.618*Red + 0.177*Green + 0.205*Blue);
  Y := Round( 0.299*Red + 0.587*Green + 0.114*Blue);
  Z := Round(             0.056*Green + 0.944*Blue);

  X := EnsureRange(X, XYZ_MIN, XYZ_MAX);
  Y := EnsureRange(Y, XYZ_MIN, XYZ_MAX);
  Z := EnsureRange(Z, XYZ_MIN, XYZ_MAX);

  Result := inherited ConvertFromColor(JoinColorParts(X, Y, Z));
end;

function TJvXYZColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Integer;
  X, Y, Z: Integer;
begin
  SplitColorParts(AColor, X, Y, Z);

  Red   := Round( 1.876*X - 0.533*Y - 0.343*Z);
  Green := Round(-0.967*X + 1.998*Y - 0.031*Z);
  Blue  := Round( 0.057*X - 0.118*Y + 1.061*Z);

  Red := EnsureRange(Red , RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
end;

function TJvXYZColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 128;
end;

function TJvXYZColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := XYZ_MAX;
end;

function TJvXYZColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := XYZ_MIN;
end;

function TJvXYZColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsXYZ_X;
    axIndex1:
      Result := RsXYZ_Y;
    axIndex2:
      Result := RsXYZ_Z;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvXYZColorSpace.GetName: string;
begin
  Result := RsXYZ_FullName;
end;

function TJvXYZColorSpace.GetShortName: string;
begin
  Result := RsXYZ_ShortName;
end;

//=== { TJvLABColorSpace } ===================================================

function TJvLABColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  X, Y, Z: Extended;
  L, A, B: Integer;
  Red, Green, Blue: Integer;

  function Calc(Value: Extended): Extended;
  begin
    if Value > 0.008856 then
      Result := Power(Value, 1.0 / 3.0)
    else
      Result := 7.7787 * Value + (16.0 / 116.0);
  end;

begin
  SplitColorParts(AColor, Red, Green, Blue);

  X := (0.618*Red + 0.177*Green + 0.205*Blue) / XYZ_MAX;
  Y := (0.299*Red + 0.587*Green + 0.114*Blue) / XYZ_MAX;
  Z := (            0.056*Green + 0.944*Blue) / XYZ_MAX;

  X := EnsureRange(X, 0.0, 1.0);
  Y := EnsureRange(Y, 0.0, 1.0);
  Z := EnsureRange(Z, 0.0, 1.0);

  if (Y > 0.008856) then
    L := Round(116.0 * Power(Y, 1.0 / 3.0) - 16.0)
  else
    L := Round(903.3 * Y);
  A := Round(500.0 *(Calc(X) - Calc(Y)));
  B := Round(200.0 *(Calc(Y) - Calc(Z)));

  L := EnsureRange(L, LAB_MIN, LAB_MAX);
  A := EnsureRange(A, LAB_MIN, LAB_MAX);
  B := EnsureRange(B, LAB_MIN, LAB_MAX);

  Result := inherited ConvertFromColor(JoinColorParts(L, A, B));
end;

function TJvLABColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
var
  Red, Green, Blue: Integer;
  X, Y, Z: Extended;
  L, A, B: Integer;

  function Calc(Value: Extended): Extended;
  begin
    if Value > 0.207 then
      Result := Power(Value, 3.0)
    else
      Result := ((116.0 * Value) - 16.0) / 903.3;
  end;

begin
  SplitColorParts(AColor, L, A, B);

  if L > 8 then
    Y := XYZ_MAX * Power((L + 16.0) / 116.0, 3.0)
  else
    Y := (XYZ_MAX * L) / 903.3;
  X := XYZ_MAX * Calc((A / 500.0) + ((L + 16.0) / 116.0));
  Z := XYZ_MAX * Calc(((L + 16.0) / 116.0) - (B / 200.0));

  X := EnsureRange(X, XYZ_MIN, XYZ_MAX);
  Y := EnsureRange(Y, XYZ_MIN, XYZ_MAX);
  Z := EnsureRange(Z, XYZ_MIN, XYZ_MAX);

  Red   := Round( 1.876*X - 0.533*Y - 0.343*Z);
  Green := Round(-0.967*X + 1.998*Y - 0.031*Z);
  Blue  := Round( 0.057*X - 0.118*Y + 1.061*Z);

  Red := EnsureRange(Red , RGB_MIN, RGB_MAX);
  Green := EnsureRange(Green, RGB_MIN, RGB_MAX);
  Blue := EnsureRange(Blue, RGB_MIN, RGB_MAX);

  Result := inherited ConvertToColor(JoinColorParts(Red, Green, Blue));
end;

function TJvLABColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 50;
end;

function TJvLABColorSpace.GetAxisMax(Index: TJvAxisIndex): Byte;
begin
  Result := LAB_MAX;
end;

function TJvLABColorSpace.GetAxisMin(Index: TJvAxisIndex): Byte;
begin
  Result := LAB_MIN;
end;

function TJvLABColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  case Index of
    axIndex0:
      Result := RsLAB_L;
    axIndex1:
      Result := RsLAB_A;
    axIndex2:
      Result := RsLAB_B;
  else
    Result := inherited GetAxisName(Index);
  end;
end;

function TJvLABColorSpace.GetName: string;
begin
  Result := RsLAB_FullName;
end;

function TJvLABColorSpace.GetShortName: string;
begin
  Result := RsLAB_ShortName;
end;

//=== { TJvDEFColorSpace } ===================================================

constructor TJvDEFColorSpace.Create(ColorID: TJvColorSpaceID);
begin
  inherited Create(ColorID);
  FDelphiColors := TStringList.Create;
  GetColorValues(AddDelphiColor);
  FDelphiColors.AddObject('clNone', TObject(clNone));
end;

destructor TJvDEFColorSpace.Destroy;
begin
  FDelphiColors.Free;
  inherited Destroy;
end;

procedure TJvDEFColorSpace.AddDelphiColor(const S: string);
var
  C: Integer;
begin
  if IdentToColor(S, C) then
    FDelphiColors.AddObject(S, TObject(C));
end;

function TJvDEFColorSpace.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  I: Integer;
  NewColor: TColor;
begin
  NewColor := clNone;
  for I := 0 to FDelphiColors.Count - 1 do
    if AColor = TColor(FDelphiColors.Objects[I]) then
    begin
      NewColor := AColor;
      Break;
    end;
  Result := inherited ConvertFromColor(NewColor);
  if NewColor = clNone then
    // mark it as clNone
    Result := Result or ($03 shl 24)
  else
  if (NewColor and ($80 shl 24)) <> 0 then
    // mark it as predefined color
    Result := Result or ($01 shl 24);
end;

function TJvDEFColorSpace.ConvertToColor(AColor: TJvFullColor): TColor;
begin
  Result := inherited ConvertToColor(AColor);
  case (AColor shr 24) and $03 of
    1:
      Result := Result or ($80 shl 24);
    3:
      if Result = (clNone and $FFFFFF) then
        Result := clNone;
  end;
end;

function TJvDEFColorSpace.ColorName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < FDelphiColors.Count) then
    Result := FDelphiColors[Index]
  else
    Result := '';
end;

function TJvDEFColorSpace.GetAxisDefault(Index: TJvAxisIndex): Byte;
begin
  Result := 0;
end;

function TJvDEFColorSpace.GetAxisName(Index: TJvAxisIndex): string;
begin
  Result := RsErr_NoName;
end;

function TJvDEFColorSpace.GetName: string;
begin
  Result := RsDEF_FullName;
end;

function TJvDEFColorSpace.GetShortName: string;
begin
  Result := RsDEF_ShortName;
end;

function TJvDEFColorSpace.GetNumberOfColors: Cardinal;
begin
  Result := FDelphiColors.Count;
end;

//=== { TJvColorSpaceManager } ===============================================

constructor TJvColorSpaceManager.Create;
begin
  inherited Create;
  FColorSpaceList := TList.Create;
end;

destructor TJvColorSpaceManager.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
    TJvColorSpace(FColorSpaceList.Items[Index]).Free;
  FColorSpaceList.Free;
  inherited Destroy;
end;

function TJvColorSpaceManager.ConvertToID(AColor: TJvFullColor; DestID: TJvColorSpaceID): TJvFullColor;
var
  SourceID: TJvColorSpaceID;
  Color: TColor;
begin
  SourceID := GetColorSpaceID(AColor);
  if SourceID = DestID then
    Result := AColor
  else
  begin
    Color := ColorSpace[SourceID].ConvertToColor(AColor);
    Result := ColorSpace[DestID].ConvertFromColor(Color);
  end;
end;

function TJvColorSpaceManager.ConvertToColor(AColor: TJvFullColor): TColor;
begin
  Result := ColorSpace[GetColorSpaceID(AColor)].ConvertToColor(AColor);
end;

function TJvColorSpaceManager.ConvertFromColor(AColor: TColor): TJvFullColor;
var
  ID: Byte;
begin
  if AColor = clDefault then
    AColor := clNone;
  ID := AColor shr 24;
  if AColor = clNone then
    Result := ColorSpace[csDEF].ConvertFromColor(AColor)
  else
    case ID of
      $00:
        Result := ColorSpace[csRGB].ConvertFromColor(AColor);
      $80:
        Result := ColorSpace[csDEF].ConvertFromColor(AColor);
    else
      raise EJvColorSpaceError.CreateResFmt(@RsErr_InconvertibleColor, [Cardinal(AColor)]);
    end;
end;

function TJvColorSpaceManager.GetColorSpaceID(AColor: TJvFullColor): TJvColorSpaceID;
var
  I: Integer;
begin
  Result := TJvColorSpaceID(AColor shr 24) and csID_MASK;
  for I := 0 to Count - 1 do
    if ColorSpaceByIndex[I].ID = Result then
      Exit;
  raise EJvColorSpaceError.CreateResFmt(@RsErr_IllegalID, [Ord(Result)]);
end;

function TJvColorSpaceManager.GetColorSpace(ID: TJvColorSpaceID): TJvColorSpace;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FColorSpaceList.Count - 1 do
  begin
    Result := TJvColorSpace(FColorSpaceList.Items[I]);
    if Result.ID = ID then
      Break;
  end;
  if Result = nil then
    raise EJvColorSpaceError.CreateResFmt(@RsErr_CSNotFound, [Ord(ID)]);
end;

function TJvColorSpaceManager.GetCount: Integer;
begin
  Result := FColorSpaceList.Count;
end;

function TJvColorSpaceManager.GetColorSpaceByIndex(Index: Integer): TJvColorSpace;
begin
  Result := TJvColorSpace(FColorSpaceList.Items[Index]);
end;

procedure TJvColorSpaceManager.RegisterColorSpace(NewColorSpace: TJvColorSpace);
var
  Index: Integer;
  CS: TJvColorSpace;
begin
  for Index := 0 to FColorSpaceList.Count - 1 do
  begin
    CS := TJvColorSpace(FColorSpaceList.Items[Index]);
    if CS.ID = NewColorSpace.ID then
      with CS do
      begin
        EJvColorSpaceError.CreateFmt(RsErr_CSAlreadyExists, [ID, Name]);
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
  ColorSpaceManager.RegisterColorSpace(TJvYIQColorSpace.Create(csYIQ));
  ColorSpaceManager.RegisterColorSpace(TJvYCCColorSpace.Create(csYCC));
  ColorSpaceManager.RegisterColorSpace(TJvXYZColorSpace.Create(csXYZ));
  ColorSpaceManager.RegisterColorSpace(TJvLABColorSpace.Create(csLAB));
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

