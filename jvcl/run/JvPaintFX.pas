{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPaintFX.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPaintFX;

interface

{$DEFINE USE_SCANLINE}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  // Type of a filter for use with Stretch()
  TFilterProc = function(Value: Single): Single;
  TLightBrush = (lbBrightness, lbContrast, lbSaturation,
    lbfisheye, lbrotate, lbtwist, lbrimple,
    mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound,
    mbround2, mbSplitRound, mbSplitWaste);

  TJvPaintFX = class(TComponent)
  public
    procedure Solarize(Src, Dst: TBitmap; Amount: Integer);
    procedure Posterize(Src, Dst: TBitmap; Amount: Integer);
    procedure Blend(Src1, Src2, Dst: TBitmap; Amount: Extended);
    procedure ExtractColor(Src: TBitmap; AColor: TColor);
    procedure ExcludeColor(Src: TBitmap; AColor: TColor);
    procedure Turn(Src, Dst: TBitmap);
    procedure TurnRight(Src, Dst: TBitmap);
    procedure HeightMap(Src: TBitmap; Amount: Integer);
    procedure TexturizeTile(Src: TBitmap; Amount: Integer);
    procedure TexturizeOverlap(Src: TBitmap; Amount: Integer);
    procedure RippleRandom(Src: TBitmap; Amount: Integer);
    procedure RippleTooth(Src: TBitmap; Amount: Integer);
    procedure RippleTriangle(Src: TBitmap; Amount: Integer);
    procedure Triangles(Src: TBitmap; Amount: Integer);
    procedure DrawMandelJulia(Src: TBitmap; x0, y0, x1, y1: Extended;
      Niter: Integer; Mandel: Boolean);
    procedure FilterXBlue(Src: TBitmap; Min, Max: Integer);
    procedure FilterXGreen(Src: TBitmap; Min, Max: Integer);
    procedure FilterXRed(Src: TBitmap; Min, Max: Integer);
    procedure FilterBlue(Src: TBitmap; Min, Max: Integer);
    procedure FilterGreen(Src: TBitmap; Min, Max: Integer);
    procedure FilterRed(Src: TBitmap; Min, Max: Integer);
    procedure Emboss(var Bmp: TBitmap);
    procedure Plasma(Src1, Src2, Dst: TBitmap; Scale, Turbulence: Extended);
    procedure Shake(Src, Dst: TBitmap; Factor: Extended);
    procedure ShakeDown(Src, Dst: TBitmap; Factor: Extended);
    procedure KeepBlue(Src: TBitmap; Factor: Extended);
    procedure KeepGreen(Src: TBitmap; Factor: Extended);
    procedure KeepRed(Src: TBitmap; Factor: Extended);
    procedure Mandelbrot(Src: TBitmap; Factor: Integer);
    procedure MaskMandelbrot(Src: TBitmap; Factor: Integer);
    procedure FoldRight(Src1, Src2, Dst: TBitmap; Amount: Extended);
    procedure QuartoOpaque(Src, Dst: TBitmap);
    procedure SemiOpaque(Src, Dst: TBitmap);
    procedure ShadowDownLeft(Src: TBitmap);
    procedure ShadowDownRight(Src: TBitmap);
    procedure ShadowUpLeft(Src: TBitmap);
    procedure ShadowUpRight(Src: TBitmap);
    procedure Darkness(const Clip: TBitmap; Amount: Integer);
    procedure Trace(Src: TBitmap; Intensity: Integer);
    procedure FlipRight(Src: TBitmap);
    procedure FlipDown(Src: TBitmap);
    procedure SpotLight(var Src: TBitmap; Amount: Integer; Spot: TRect);
    procedure SplitLight(var Clip: TBitmap; Amount: Integer);
    procedure MakeSeamlessClip(var Clip: TBitmap; Seam: Integer);
    procedure Wave(const Clip: TBitmap; Amount, Inference, Style: Integer);
    procedure Mosaic(const Bm: TBitmap; Size: Integer);
    procedure SmoothRotate(var Src, Dst: TBitmap; CX, CY: Integer;
      Angle: Extended);
    procedure SmoothResize(var Src, Dst: TBitmap);
    procedure Twist(var Bmp, Dst: TBitmap; Amount: Integer);
    procedure SplitBlur(const Clip: TBitmap; Amount: Integer);
    procedure GaussianBlur(const Clip: TBitmap; Amount: Integer);
    procedure Smooth(const Clip: TBitmap; Weight: Integer);
    procedure GrayScale(const Clip: TBitmap);
    procedure AddColorNoise(const Clip: TBitmap; Amount: Integer);
    procedure AddMonoNoise(const Clip: TBitmap; Amount: Integer);
    procedure Contrast(const Clip: TBitmap; Amount: Integer);
    procedure Lightness(const Clip: TBitmap; Amount: Integer);
    procedure Saturation(const Clip: TBitmap; Amount: Integer);
    procedure Spray(const Clip: TBitmap; Amount: Integer);
    procedure AntiAlias(const Clip: TBitmap);
    procedure AntiAliasRect(const Clip: TBitmap; XOrigin, YOrigin, XFinal, YFinal: Integer);
    procedure SmoothPoint(const Clip: TBitmap; XK, YK: Integer);
    procedure FishEye(var Bmp, Dst: TBitmap; Amount: Extended);
    procedure Marble(var Src, Dst: TBitmap; Scale: Extended; Turbulence: Integer);
    procedure Marble2(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble3(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble4(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble5(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble6(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble7(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure Marble8(var Src, Dst: TBitmap; Scale: Extended;
      Turbulence: Integer);
    procedure SqueezeHor(Src, Dst: TBitmap; Amount: Integer; Style: TLightBrush);
    procedure SplitRound(Src, Dst: TBitmap; Amount: Integer; Style: TLightBrush);

    procedure Tile(Src, Dst: TBitmap; Amount: Integer);
    // Interpolator
    // Src:	Source bitmap
    // Dst:	Destination bitmap
    // Filter:	Weight calculation filter
    // AWidth:	Relative sample radius
    procedure Strecth(Src, Dst: TBitmap; Filter: TFilterProc; AWidth: Single);
    procedure Grow(Src1, Src2, Dst: TBitmap; Amount: Extended; x, y: Integer);
    procedure Invert(Src: TBitmap);
    procedure MirrorRight(Src: TBitmap);
    procedure MirrorDown(Src: TBitmap);
  end;

// Sample filters for use with Stretch()
function SplineFilter(Value: Single): Single;
function BellFilter(Value: Single): Single;
function TriangleFilter(Value: Single): Single;
function BoxFilter(Value: Single): Single;
function HermiteFilter(Value: Single): Single;
function Lanczos3Filter(Value: Single): Single;
function MitchellFilter(Value: Single): Single;

const
  ResampleFilters: array [0..6] of record
    Name: string; // Filter name
    Filter: TFilterProc; // Filter implementation
    Width: Single; // Suggested sampling width/radius
  end = (
    (Name: 'Box'; Filter: BoxFilter; Width: 0.5),
    (Name: 'Triangle'; Filter: TriangleFilter; Width: 1.0),
    (Name: 'Hermite'; Filter: HermiteFilter; Width: 1.0),
    (Name: 'Bell'; Filter: BellFilter; Width: 1.5),
    (Name: 'B-Spline'; Filter: SplineFilter; Width: 2.0),
    (Name: 'Lanczos3'; Filter: Lanczos3Filter; Width: 3.0),
    (Name: 'Mitchell'; Filter: MitchellFilter; Width: 2.0)
    );

implementation

uses
  Math,
  JvJCLUtils, JvTypes;

type
  TFColor = record
    b, g, r: Byte;
  end;

function TrimInt(N, Min, Max: Integer): Integer;
begin
  if N > Max then
    Result := Max
  else
  if N < Min then
    Result := Min
  else
    Result := N;
end;

function IntToByte(N: Integer): Byte;
begin
  if N > 255 then
    Result := 255
  else
  if N < 0 then
    Result := 0
  else
    Result := N;
end;

// Just a small function to map the numbers to colors

function ConvertColor(Value: Integer): TColor;
begin
  case Value of
    0:
      Result := clBlack;
    1:
      Result := clNavy;
    2:
      Result := clGreen;
    3:
      Result := clAqua;
    4:
      Result := clRed;
    5:
      Result := clPurple;
    6:
      Result := clMaroon;
    7:
      Result := clSilver;
    8:
      Result := clGray;
    9:
      Result := clBlue;
    10:
      Result := clLime;
    11:
      Result := clOlive;
    12:
      Result := clFuchsia;
    13:
      Result := clTeal;
    14:
      Result := clYellow;
    15:
      Result := clWhite;
  else
    Result := clWhite;
  end;
end;

function BellFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 0.5 then
    Result := 0.75 - Sqr(Value)
  else
  if Value < 1.5 then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

// a.k.a. "Nearest Neighbour" filter
// anme: I have not been able to get acceptable
//       results with this filter for subsampling.

function BoxFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then
    Result := 1.0
  else
    Result := 0.0;
end;

function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

function Lanczos3Filter(Value: Single): Single;

function SinC(Value: Single): Single;
  begin
    if Value <> 0.0 then
    begin
      Value := Value * Pi;
      Result := Sin(Value) / Value
    end
    else
      Result := 1.0;
  end;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 3.0 then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B = 1.0 / 3.0;
  C = 1.0 / 3.0;
var
  tt: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  tt := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * tt) +
      (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt)) +
      ((6.0 * B + 30.0 * C) * tt) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

// B-spline filter

function SplineFilter(Value: Single): Single;
var
  tt: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2.0 / 3.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := 2.0 - Value;
    Result := 1.0 / 6.0 * Sqr(Value) * Value;
  end
  else
    Result := 0.0;
end;

// Triangle filter
// a.k.a. "Linear" or "Bilinear" filter

function TriangleFilter(Value: Single): Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

procedure TJvPaintFX.AddColorNoise(const Clip: TBitmap; Amount: Integer);
var
  Line: PJvRGBArray;
  X, Y: Integer;
  OPF: TPixelFormat;
begin
  Randomize;
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[Y];
    for X := 0 to Clip.Width - 1 do
    begin
      Line[X].rgbRed   := IntToByte(Line[X].rgbRed + (Random(Amount) - (Amount shr 1)));
      Line[X].rgbGreen := IntToByte(Line[X].rgbGreen + (Random(Amount) - (Amount shr 1)));
      Line[X].rgbBlue  := IntToByte(Line[X].rgbBlue + (Random(Amount) - (Amount shr 1)));
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.AddMonoNoise(const Clip: TBitmap; Amount: Integer);
var
  Line: PJvRGBArray;
  X, Y, A: Integer;
  OPF: TPixelFormat;
begin
  Randomize;
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[Y];
    for X := 0 to Clip.Width - 1 do
    begin
      A := Random(Amount) - (Amount shr 1);
      Line[X].rgbRed   := IntToByte(Line[X].rgbRed + A);
      Line[X].rgbGreen := IntToByte(Line[X].rgbGreen + A);
      Line[X].rgbBlue  := IntToByte(Line[X].rgbBlue + A);
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.AntiAlias(const Clip: TBitmap);
begin
  JvJCLUtils.AntiAlias(Clip);
end;

procedure TJvPaintFX.AntiAliasRect(const Clip: TBitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);
begin
  JvJCLUtils.AntiAliasRect(Clip, XOrigin, YOrigin, XFinal, YFinal);
end;

procedure TJvPaintFX.Contrast(const Clip: TBitmap; Amount: Integer);
var
  Line: PJvRGBArray;
  rg, gg, bg, r, g, b, x, y: Integer;
  OPF: TPixelFormat;
begin
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[y];
    for x := 0 to Clip.Width - 1 do
    begin
      r := Line[x].rgbRed;
      g := Line[x].rgbGreen;
      b := Line[x].rgbBlue;
      rg := (Abs(127 - r) * Amount) div 255;
      gg := (Abs(127 - g) * Amount) div 255;
      bg := (Abs(127 - b) * Amount) div 255;
      if r > 127 then
        r := r + rg
      else
        r := r - rg;
      if g > 127 then
        g := g + gg
      else
        g := g - gg;
      if b > 127 then
        b := b + bg
      else
        b := b - bg;
      Line[x].rgbRed   := IntToByte(r);
      Line[x].rgbGreen := IntToByte(g);
      Line[x].rgbBlue  := IntToByte(b);
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.FishEye(var Bmp, Dst: TBitmap; Amount: Extended);
var
  xmid, ymid: Single;
  fx, fy: Single;
  r1, r2: Single;
  ifx, ify: Integer;
  dx, dy: Single;
  rmax: Single;
  ty, tx: Integer;
  weight_x, weight_y: array [0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  sli, slo: PJvRGBArray;
begin
  xmid := Bmp.Width / 2;
  ymid := Bmp.Height / 2;
  rmax := Dst.Width * Amount;

  for ty := 0 to Dst.Height - 1 do
  begin
    for tx := 0 to Dst.Width - 1 do
    begin
      dx := tx - xmid;
      dy := ty - ymid;
      r1 := Sqrt(dx * dx + dy * dy);
      if r1 = 0 then
      begin
        fx := xmid;
        fy := ymid;
      end
      else
      begin
        r2 := rmax / 2 * (1 / (1 - r1 / rmax) - 1);
        fx := dx * r2 / r1 + xmid;
        fy := dy * r2 / r1 + ymid;
      end;
      ify := Trunc(fy);
      ifx := Trunc(fx);
      // Calculate the weights.
      if fy >= 0 then
      begin
        weight_y[1] := fy - ify;
        weight_y[0] := 1 - weight_y[1];
      end
      else
      begin
        weight_y[0] := -(fy - ify);
        weight_y[1] := 1 - weight_y[0];
      end;
      if fx >= 0 then
      begin
        weight_x[1] := fx - ifx;
        weight_x[0] := 1 - weight_x[1];
      end
      else
      begin
        weight_x[0] := -(fx - ifx);
        Weight_x[1] := 1 - weight_x[0];
      end;

      if ifx < 0 then
        ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
      else
      if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else
      if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.ScanLine[ify + iy]
          else
            sli := Bmp.ScanLine[Bmp.Height - ify - iy];
          if ifx + ix < Bmp.Width then
          begin
            new_red   := sli[ifx + ix].rgbRed;
            new_green := sli[ifx + ix].rgbGreen;
            new_blue  := sli[ifx + ix].rgbBlue;
          end
          else
          begin
            new_red   := sli[Bmp.Width - ifx - ix].rgbRed;
            new_green := sli[Bmp.Width - ifx - ix].rgbGreen;
            new_blue  := sli[Bmp.Width - ifx - ix].rgbBlue;
          end;
          weight := weight_x[ix] * weight_y[iy];
          total_red := total_red + new_red * weight;
          total_green := total_green + new_green * weight;
          total_blue := total_blue + new_blue * weight;
        end;
      end;
      slo := Dst.ScanLine[ty];
      slo[tx].rgbRed   := Round(total_red);
      slo[tx].rgbGreen := Round(total_green);
      slo[tx].rgbBlue  := Round(total_blue);
    end;
  end;
end;

procedure TJvPaintFX.GaussianBlur(const Clip: TBitmap; Amount: Integer);
var
  I: Integer;
begin
  for I := Amount downto 0 do
    SplitBlur(Clip, 3);
end;

procedure TJvPaintFX.GrayScale(const Clip: TBitmap);
var
  Line: PJvRGBArray;
  Gray, X, Y: Integer;
  OPF: TPixelFormat;
begin
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[y];
    for X := 0 to Clip.Width - 1 do
    begin
      Gray := Round(Line[X].rgbRed * 0.3 + Line[X].rgbGreen * 0.59 + Line[X].rgbBlue * 0.11);
      Line[X].rgbRed   := Gray;
      Line[X].rgbGreen := Gray;
      Line[X].rgbBlue  := Gray;
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.Lightness(const Clip: TBitmap; Amount: Integer);
var
  Line: PJvRGBArray;
  r, g, b, X, Y: Integer;
  OPF: TPixelFormat;
begin
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[Y];
    for X := 0 to Clip.Width - 1 do
    begin
      r := Line[X].rgbRed;
      g := Line[X].rgbGreen;
      b := Line[X].rgbBlue;
      Line[X].rgbRed   := IntToByte(r + ((255 - r) * Amount) div 255);
      Line[X].rgbGreen := IntToByte(g + ((255 - g) * Amount) div 255);
      Line[X].rgbBlue  := IntToByte(b + ((255 - b) * Amount) div 255);
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.Darkness(const Clip: TBitmap; Amount: Integer);
var
  Line: PJvRGBArray;
  r, g, b, X, Y: Integer;
  OPF: TPixelFormat;
begin
  OPF := Clip.PixelFormat;
  Clip.PixelFormat := pf24bit;
  for Y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[Y];
    for X := 0 to Clip.Width - 1 do
    begin
      r := Line[X].rgbRed;
      g := Line[X].rgbGreen;
      b := Line[X].rgbBlue;
      Line[X].rgbRed   := IntToByte(r - (r * Amount) div 255);
      Line[X].rgbGreen := IntToByte(g - (g * Amount) div 255);
      Line[X].rgbBlue  := IntToByte(b - (b * Amount) div 255);
    end;
  end;
  Clip.PixelFormat := OPF;
end;

procedure TJvPaintFX.Marble(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.Width := w;
  Dst.Height := h;
  Dst.Canvas.Draw(0, 0, Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Cos((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -Scale * Sin((x mod Turbulence) / Scale);
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble2(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Cos((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -Scale * Sin((x mod Turbulence) / Scale);
      xm := Round(Abs(x + xx - yy));
      ym := Round(Abs(y + yy - xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble3(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Cos((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -Scale * Sin((x mod Turbulence) / Scale);
      xm := Round(Abs(x - xx + yy));
      ym := Round(Abs(y - yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble4(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Sin((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -Scale * Cos((x mod Turbulence) / Scale);
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble5(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := h - 1 downto 0 do
  begin
    yy := Scale * Cos((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := w - 1 downto 0 do
    begin
      xx := -Scale * Sin((x mod Turbulence) / Scale);
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble6(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Cos((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -tan((x mod Turbulence) / Scale) / Scale;
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble7(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    yy := Scale * Sin((y mod Turbulence) / Scale);

    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := -tan((x mod Turbulence) / Scale) / (Scale * Scale);
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Marble8(var Src, Dst: TBitmap; Scale: Extended;
  Turbulence: Integer);
var
  x, xm, y, ym: Integer;
  xx, yy: Extended;
  p1, p2: pbytearray;
  w, h: Integer;
  ax: Extended;
begin
  h := Src.Height;
  w := Src.Width;
  Dst.assign(Src);
  for y := 0 to h - 1 do
  begin
    ax := (y mod Turbulence) / Scale;
    yy := Scale * Sin(ax) * Cos(1.5 * ax);
    p1 := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      ax := (x mod Turbulence) / Scale;
      xx := -Scale * Sin(2 * ax) * Cos(ax);
      xm := Round(Abs(x + xx + yy));
      ym := Round(Abs(y + yy + xx));
      if ym < h then
      begin
        p2 := Dst.ScanLine[ym];
        if xm < w then
        begin
          p2[xm * 3] := p1[x * 3];
          p2[xm * 3 + 1] := p1[x * 3 + 1];
          p2[xm * 3 + 2] := p1[x * 3 + 2];
        end;
      end;
    end;
  end;
end;

procedure TJvPaintFX.Saturation(const Clip: TBitmap; Amount: Integer);
var
  Line: pbytearray;
  Gray, r, g, b, x, y: Integer;
begin
  for y := 0 to Clip.Height - 1 do
  begin
    Line := Clip.ScanLine[y];
    for x := 0 to Clip.Width - 1 do
    begin
      r := Line[x * 3];
      g := Line[x * 3 + 1];
      b := Line[x * 3 + 2];
      Gray := (r + g + b) div 3;
      Line[x * 3] := IntToByte(Gray + (((r - Gray) * Amount) div 255));
      Line[x * 3 + 1] := IntToByte(Gray + (((g - Gray) * Amount) div 255));
      Line[x * 3 + 2] := IntToByte(Gray + (((b - Gray) * Amount) div 255));
    end;
  end;
end;

procedure TJvPaintFX.Smooth(const Clip: TBitmap; Weight: Integer);
begin
  //
end;

procedure TJvPaintFX.SmoothPoint(const Clip: TBitmap; XK, YK: Integer);
var
  Bleu, Vert, Rouge, w, h: Integer;
  color: TFColor;
  AColor: TColor;
  BB, GG, RR: array[1..5] of Integer;
begin
  w := Clip.Width;
  h := Clip.Height;
  if (XK > 0) and (YK > 0) and (XK < w - 1) and (YK < h - 1) then
    with Clip.Canvas do
    begin
      AColor := ColorToRGB(pixels[XK, YK - 1]);
      color.r := GetRValue(AColor);
      color.g := GetGValue(AColor);
      color.b := GetBValue(AColor);
      RR[1] := color.r;
      GG[1] := color.g;
      BB[1] := color.b;
      AColor := ColorToRGB(pixels[XK + 1, YK]);
      color.r := GetRValue(AColor);
      color.g := GetGValue(AColor);
      color.b := GetBValue(AColor);
      RR[2] := color.r;
      GG[2] := color.g;
      BB[2] := color.b;
      AColor := ColorToRGB(pixels[XK, YK + 1]);
      color.r := GetRValue(AColor);
      color.g := GetGValue(AColor);
      color.b := GetBValue(AColor);
      RR[3] := color.r;
      GG[3] := color.g;
      BB[3] := color.b;
      AColor := ColorToRGB(pixels[XK - 1, YK]);
      color.r := GetRValue(AColor);
      color.g := GetGValue(AColor);
      color.b := GetBValue(AColor);
      RR[4] := color.r;
      GG[4] := color.g;
      BB[4] := color.b;
      Bleu := (BB[1] + (BB[2] + BB[3] + BB[4])) div 4; (* Valeur moyenne *)
      Vert := (GG[1] + (GG[2] + GG[3] + GG[4])) div 4; (* en cours d'‚valuation        *)
      Rouge := (RR[1] + (RR[2] + RR[3] + RR[4])) div 4;
      color.r := rouge;
      color.g := vert;
      color.b := bleu;
      pixels[XK, YK] := rgb(color.r, color.g, color.b);
    end;
end;

procedure TJvPaintFX.SmoothResize(var Src, Dst: TBitmap);
var
  x, y, xP, yP,
    yP2, xP2: Integer;
  Read, Read2: PByteArray;
  t, z, z2, iz2: Integer;
  pc: PBytearray;
  w1, w2, w3, w4: Integer;
  Col1r, col1g, col1b, Col2r, col2g, col2b: Byte;
begin
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    Read := Src.ScanLine[yP shr 15];
    if yP shr 16 < Src.Height - 1 then
      Read2 := Src.ScanLine[yP shr 15 + 1]
    else
      Read2 := Src.ScanLine[yP shr 15];
    pc := Dst.ScanLine[y];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      Col1r := Read[t * 3];
      Col1g := Read[t * 3 + 1];
      Col1b := Read[t * 3 + 2];
      Col2r := Read2[t * 3];
      Col2g := Read2[t * 3 + 1];
      Col2b := Read2[t * 3 + 2];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[x * 3 + 2] :=
        (Col1b * w1 + Read[(t + 1) * 3 + 2] * w2 +
        Col2b * w3 + Read2[(t + 1) * 3 + 2] * w4) shr 15;
      pc[x * 3 + 1] :=
        (Col1g * w1 + Read[(t + 1) * 3 + 1] * w2 +
        Col2g * w3 + Read2[(t + 1) * 3 + 1] * w4) shr 15;
      pc[x * 3] :=
        (Col1r * w1 + Read2[(t + 1) * 3] * w2 +
        Col2r * w3 + Read2[(t + 1) * 3] * w4) shr 15;
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

procedure TJvPaintFX.SmoothRotate(var Src, Dst: TBitmap; CX, CY: Integer;
  Angle: Extended);
type
  TFColor = record b, g, r: Byte
  end;
var
  Top,
    Bottom,
    Left,
    Right,
    eww, nsw,
    fx, fy,
    wx, wy: Extended;
  cAngle,
    sAngle: Double;
  xDiff,
    yDiff,
    ifx, ify,
    px, py,
    ix, iy,
    x, y: Integer;
  nw, ne,
    sw, se: TFColor;
  P1, P2, P3: Pbytearray;
begin
  Angle := angle;
  Angle := -Angle * Pi / 180;
  sAngle := Sin(Angle);
  cAngle := Cos(Angle);
  xDiff := (Dst.Width - Src.Width) div 2;
  yDiff := (Dst.Height - Src.Height) div 2;
  for y := 0 to Dst.Height - 1 do
  begin
    P3 := Dst.ScanLine[y];
    py := 2 * (y - CY) + 1;
    for x := 0 to Dst.Width - 1 do
    begin
      px := 2 * (x - CX) + 1;
      fx := (((px * cAngle - py * sAngle) - 1) / 2 + CX) - xDiff;
      fy := (((px * sAngle + py * cAngle) - 1) / 2 + CY) - yDiff;
      ifx := Round(fx);
      ify := Round(fy);

      if (ifx > -1) and (ifx < Src.Width) and (ify > -1) and (ify < Src.Height) then
      begin
        eww := fx - ifx;
        nsw := fy - ify;
        iy := TrimInt(ify + 1, 0, Src.Height - 1);
        ix := TrimInt(ifx + 1, 0, Src.Width - 1);
        P1 := Src.ScanLine[ify];
        P2 := Src.ScanLine[iy];
        nw.r := P1[ifx * 3];
        nw.g := P1[ifx * 3 + 1];
        nw.b := P1[ifx * 3 + 2];
        ne.r := P1[ix * 3];
        ne.g := P1[ix * 3 + 1];
        ne.b := P1[ix * 3 + 2];
        sw.r := P2[ifx * 3];
        sw.g := P2[ifx * 3 + 1];
        sw.b := P2[ifx * 3 + 2];
        se.r := P2[ix * 3];
        se.g := P2[ix * 3 + 1];
        se.b := P2[ix * 3 + 2];

        Top := nw.b + eww * (ne.b - nw.b);
        Bottom := sw.b + eww * (se.b - sw.b);
        P3[x * 3 + 2] := IntToByte(Round(Top + nsw * (Bottom - Top)));

        Top := nw.g + eww * (ne.g - nw.g);
        Bottom := sw.g + eww * (se.g - sw.g);
        P3[x * 3 + 1] := IntToByte(Round(Top + nsw * (Bottom - Top)));

        Top := nw.r + eww * (ne.r - nw.r);
        Bottom := sw.r + eww * (se.r - sw.r);
        P3[x * 3] := IntToByte(Round(Top + nsw * (Bottom - Top)));
      end;
    end;
  end;
end;

procedure TJvPaintFX.SplitBlur(const Clip: TBitmap; Amount: Integer);
var
  p0, p1, p2: pbytearray;
  CX, x, y: Integer;
  Buf: array[0..3, 0..2] of Byte;
begin
  if Amount = 0 then
    Exit;
  for y := 0 to Clip.Height - 1 do
  begin
    p0 := Clip.ScanLine[y];
    if y - Amount < 0 then
      p1 := Clip.ScanLine[y]
    else {y-Amount>0}
      p1 := Clip.ScanLine[y - Amount];
    if y + Amount < Clip.Height then
      p2 := Clip.ScanLine[y + Amount]
    else {y+Amount>=Height}
      p2 := Clip.ScanLine[Clip.Height - y];

    for x := 0 to Clip.Width - 1 do
    begin
      if x - Amount < 0 then
        CX := x
      else {x-Amount>0}
        CX := x - Amount;
      Buf[0, 0] := p1[CX * 3];
      Buf[0, 1] := p1[CX * 3 + 1];
      Buf[0, 2] := p1[CX * 3 + 2];
      Buf[1, 0] := p2[CX * 3];
      Buf[1, 1] := p2[CX * 3 + 1];
      Buf[1, 2] := p2[CX * 3 + 2];
      if x + Amount < Clip.Width then
        CX := x + Amount
      else {x+Amount>=Width}
        CX := Clip.Width - x;
      Buf[2, 0] := p1[CX * 3];
      Buf[2, 1] := p1[CX * 3 + 1];
      Buf[2, 2] := p1[CX * 3 + 2];
      Buf[3, 0] := p2[CX * 3];
      Buf[3, 1] := p2[CX * 3 + 1];
      Buf[3, 2] := p2[CX * 3 + 2];
      p0[x * 3] := (Buf[0, 0] + Buf[1, 0] + Buf[2, 0] + Buf[3, 0]) shr 2;
      p0[x * 3 + 1] := (Buf[0, 1] + Buf[1, 1] + Buf[2, 1] + Buf[3, 1]) shr 2;
      p0[x * 3 + 2] := (Buf[0, 2] + Buf[1, 2] + Buf[2, 2] + Buf[3, 2]) shr 2;
    end;
  end;
end;

procedure TJvPaintFX.Spray(const Clip: TBitmap; Amount: Integer);
var
  i, j, x, y, w, h, Val: Integer;
begin
  h := Clip.Height;
  w := Clip.Width;
  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
    begin
      Val := Random(Amount);
      x := i + Val - Random(Val * 2);
      y := j + Val - Random(Val * 2);
      if (x > -1) and (x < w) and (y > -1) and (y < h) then
        Clip.Canvas.Pixels[i, j] := Clip.Canvas.Pixels[x, y];
    end;
end;

procedure TJvPaintFX.Mosaic(const Bm: TBitmap; Size: Integer);
var
  x, y, i, j: Integer;
  p1, p2: pbytearray;
  r, g, b: Byte;
begin
  y := 0;
  repeat
    p1 := bm.ScanLine[y];
    //   x:=0;
    repeat
      j := 1;
      repeat
        p2 := bm.ScanLine[y];
        x := 0;
        repeat
          r := p1[x * 3];
          g := p1[x * 3 + 1];
          b := p1[x * 3 + 2];
          i := 1;
          repeat
            p2[x * 3] := r;
            p2[x * 3 + 1] := g;
            p2[x * 3 + 2] := b;
            Inc(x);
            Inc(i);
          until (x >= bm.Width) or (i > Size);
        until x >= bm.Width;
        Inc(j);
        Inc(y);
      until (y >= bm.Height) or (j > Size);
    until (y >= bm.Height) or (x >= bm.Width);
  until y >= bm.Height;
end;

procedure TJvPaintFX.Twist(var Bmp, Dst: TBitmap; Amount: Integer);
var
  fxmid, fymid: Single;
  txmid, tymid: Single;
  fx, fy: Single;
  tx2, ty2: Single;
  r: Single;
  theta: Single;
  ifx, ify: Integer;
  dx, dy: Single;
  OFFSET: Single;
  ty, tx: Integer;
  weight_x, weight_y: array[0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  sli, slo: PBytearray;

  function ArcTan2(xt, yt: Single): Single;
  begin
    if xt = 0 then
      if yt > 0 then
        Result := Pi / 2
      else
        Result := -(Pi / 2)
    else
    begin
      Result := ArcTan(yt / xt);
      if xt < 0 then
        Result := Pi + ArcTan(yt / xt);
    end;
  end;

begin
  OFFSET := -(Pi / 2);
  dx := Bmp.Width - 1;
  dy := Bmp.Height - 1;
  r := Sqrt(dx * dx + dy * dy);
  tx2 := r;
  ty2 := r;
  txmid := (Bmp.Width - 1) / 2; //Adjust these to move center of rotation
  tymid := (Bmp.Height - 1) / 2; //Adjust these to move ......
  fxmid := (Bmp.Width - 1) / 2;
  fymid := (Bmp.Height - 1) / 2;
  if tx2 >= Bmp.Width then
    tx2 := Bmp.Width - 1;
  if ty2 >= Bmp.Height then
    ty2 := Bmp.Height - 1;

  for ty := 0 to Round(ty2) do
  begin
    for tx := 0 to Round(tx2) do
    begin
      dx := tx - txmid;
      dy := ty - tymid;
      r := Sqrt(dx * dx + dy * dy);
      if r = 0 then
      begin
        fx := 0;
        fy := 0;
      end
      else
      begin
        theta := ArcTan2(dx, dy) - r / Amount - OFFSET;
        fx := r * Cos(theta);
        fy := r * Sin(theta);
      end;
      fx := fx + fxmid;
      fy := fy + fymid;

      ify := Trunc(fy);
      ifx := Trunc(fx);
      // Calculate the weights.
      if fy >= 0 then
      begin
        weight_y[1] := fy - ify;
        weight_y[0] := 1 - weight_y[1];
      end
      else
      begin
        weight_y[0] := -(fy - ify);
        weight_y[1] := 1 - weight_y[0];
      end;
      if fx >= 0 then
      begin
        weight_x[1] := fx - ifx;
        weight_x[0] := 1 - weight_x[1];
      end
      else
      begin
        weight_x[0] := -(fx - ifx);
        Weight_x[1] := 1 - weight_x[0];
      end;

      if ifx < 0 then
        ifx := Bmp.Width - 1 - (-ifx mod Bmp.Width)
      else
      if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else
      if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.ScanLine[ify + iy]
          else
            sli := Bmp.ScanLine[Bmp.Height - ify - iy];
          if ifx + ix < Bmp.Width then
          begin
            new_red := sli[(ifx + ix) * 3];
            new_green := sli[(ifx + ix) * 3 + 1];
            new_blue := sli[(ifx + ix) * 3 + 2];
          end
          else
          begin
            new_red := sli[(Bmp.Width - ifx - ix) * 3];
            new_green := sli[(Bmp.Width - ifx - ix) * 3 + 1];
            new_blue := sli[(Bmp.Width - ifx - ix) * 3 + 2];
          end;
          weight := weight_x[ix] * weight_y[iy];
          total_red := total_red + new_red * weight;
          total_green := total_green + new_green * weight;
          total_blue := total_blue + new_blue * weight;
        end;
      end;
      slo := Dst.ScanLine[ty];
      slo[tx * 3] := Round(total_red);
      slo[tx * 3 + 1] := Round(total_green);
      slo[tx * 3 + 2] := Round(total_blue);
    end;
  end;
end;

procedure TJvPaintFX.Wave(const Clip: TBitmap; Amount, Inference, Style: Integer);
var
  x, y: Integer;
  Bitmap: TBitmap;
  P1, P2: PByteArray;
  b: Integer;
  fangle: real;
  wavex: Integer;
begin
  Bitmap := TBitmap.Create;
  Bitmap.assign(Clip);
  wavex := Style;
  fangle := Pi / 2 / Amount;
  for y := Bitmap.Height - 1 - (2 * Amount) downto Amount do
  begin
    P1 := Bitmap.ScanLine[y];
    b := 0;
    for x := 0 to Bitmap.Width - 1 do
    begin
      P2 := Clip.ScanLine[y + Amount + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      case wavex of
        0:
          b := Amount * Variant(Sin(fangle * x));
        1:
          b := Amount * Variant(Sin(fangle * x) * Cos(fangle * x));
        2:
          b := Amount * Variant(Sin(fangle * x) * Sin(Inference * fangle * x));
      end;
    end;
  end;
  Bitmap.Free;
end;

procedure TJvPaintFX.MakeSeamlessClip(var Clip: TBitmap; Seam: Integer);
var
  p0, p1, p2: pbytearray;
  h, w, i, j, sv, sh: Integer;
  f0, f1, f2: real;
begin
  h := Clip.Height;
  w := Clip.Width;
  sv := h div Seam;
  sh := w div Seam;
  p1 := Clip.ScanLine[0];
  p2 := Clip.ScanLine[h - 1];
  for i := 0 to w - 1 do
  begin
    p1[i * 3] := p2[i * 3];
    p1[i * 3 + 1] := p2[i * 3 + 1];
    p1[i * 3 + 2] := p2[i * 3 + 2];
  end;
  p0 := Clip.ScanLine[0];
  p2 := Clip.ScanLine[sv];
  for j := 1 to sv - 1 do
  begin
    p1 := Clip.ScanLine[j];
    for i := 0 to w - 1 do
    begin
      f0 := (p2[i * 3] - p0[i * 3]) / sv * j + p0[i * 3];
      p1[i * 3] := Round(f0);
      f1 := (p2[i * 3 + 1] - p0[i * 3 + 1]) / sv * j + p0[i * 3 + 1];
      p1[i * 3 + 1] := Round(f1);
      f2 := (p2[i * 3 + 2] - p0[i * 3 + 2]) / sv * j + p0[i * 3 + 2];
      p1[i * 3 + 2] := Round(f2);
    end;
  end;
  for j := 0 to h - 1 do
  begin
    p1 := Clip.ScanLine[j];
    p1[(w - 1) * 3] := p1[0];
    p1[(w - 1) * 3 + 1] := p1[1];
    p1[(w - 1) * 3 + 2] := p1[2];
    for i := 1 to sh - 1 do
    begin
      f0 := (p1[(w - sh) * 3] - p1[(w - 1) * 3]) / sh * i + p1[(w - 1) * 3];
      p1[(w - 1 - i) * 3] := Round(f0);
      f1 := (p1[(w - sh) * 3 + 1] - p1[(w - 1) * 3 + 1]) / sh * i + p1[(w - 1) * 3 + 1];
      p1[(w - 1 - i) * 3 + 1] := Round(f1);
      f2 := (p1[(w - sh) * 3 + 2] - p1[(w - 1) * 3 + 2]) / sh * i + p1[(w - 1) * 3 + 2];
      p1[(w - 1 - i) * 3 + 2] := Round(f2);
    end;
  end;
end;

procedure TJvPaintFX.SplitLight(var Clip: TBitmap; Amount: Integer);
var
  x, y, i: Integer;
  p1: pbytearray;

  function sinpixs(a: Integer): Integer;
  begin
    Result := Variant(Sin(a / 255 * Pi / 2) * 255);
  end;
begin
  for i := 1 to Amount do
    for y := 0 to Clip.Height - 1 do
    begin
      p1 := Clip.ScanLine[y];
      for x := 0 to Clip.Width - 1 do
      begin
        p1[x * 3] := sinpixs(p1[x * 3]);
        p1[x * 3 + 1] := sinpixs(p1[x * 3 + 1]);
        p1[x * 3 + 2] := sinpixs(p1[x * 3 + 2]);
      end;
    end;
end;

procedure TJvPaintFX.SqueezeHor(Src, Dst: TBitmap; Amount: Integer; Style: TLightBrush);
var
  dx, x, y, c, CX: Integer;
  R: TRect;
  bm: TBitmap;
  p0, p1: pbytearray;
begin
  if Amount > (Src.Width div 2) then
    Amount := Src.Width div 2;
  bm := TBitmap.Create;
  bm.PixelFormat := pf24bit;
  bm.Height := 1;
  bm.Width := Src.Width;
  CX := Src.Width div 2;
  p0 := bm.ScanLine[0];
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      p0[c] := p1[c];
      p0[c + 1] := p1[c + 1];
      p0[c + 2] := p1[c + 2];
    end;
    case Style of
      mbhor:
        begin
          dx := Amount;
          R := rect(dx, y, Src.Width - dx, y + 1);
        end;
      mbtop:
        begin
          dx := Round((Src.Height - 1 - y) / Src.Height * Amount);
          R := rect(dx, y, Src.Width - dx, y + 1);
        end;
      mbBottom:
        begin
          dx := Round(y / Src.Height * Amount);
          R := rect(dx, y, Src.Width - dx, y + 1);
        end;
      mbDiamond:
        begin
          dx := Round(Amount * Abs(Cos(y / (Src.Height - 1) * Pi)));
          R := rect(dx, y, Src.Width - dx, y + 1);
        end;
      mbWaste:
        begin
          dx := Round(Amount * Abs(Sin(y / (Src.Height - 1) * Pi)));
          R := rect(dx, y, Src.Width - dx, y + 1);
        end;
      mbRound:
        begin
          dx := Round(Amount * Abs(Sin(y / (Src.Height - 1) * Pi)));
          R := rect(CX - dx, y, CX + dx, y + 1);
        end;
      mbRound2:
        begin
          dx := Round(Amount * Abs(Sin(y / (Src.Height - 1) * Pi * 2)));
          R := rect(CX - dx, y, CX + dx, y + 1);
        end;
    end;
    Dst.Canvas.StretchDraw(R, bm);
  end;
  bm.Free;
end;

procedure TJvPaintFX.Tile(Src, Dst: TBitmap; Amount: Integer);
var
  w, h, w2, h2, i, j: Integer;
  bm: TBitmap;
begin
  w := Src.Width;
  h := Src.Height;
  Dst.Width := w;
  Dst.Height := h;
  Dst.Canvas.draw(0, 0, Src);
  if (Amount <= 0) or ((w div Amount) < 5) or ((h div Amount) < 5) then
    Exit;
  h2 := h div Amount;
  w2 := w div Amount;
  bm := TBitmap.Create;
  bm.Width := w2;
  bm.Height := h2;
  bm.PixelFormat := pf24bit;
  smoothresize(Src, bm);
  for j := 0 to Amount - 1 do
    for i := 0 to Amount - 1 do
      Dst.Canvas.Draw(i * w2, j * h2, bm);
  bm.Free;
end;

// -----------------------------------------------------------------------------
//
//			Interpolator
//
// -----------------------------------------------------------------------------
type
  // Contributor for a pixel
  TContributor = record
    pixel: Integer; // Source pixel
    weight: Single; // Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: Integer;
    p: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    r, g, b: Single;
  end;

  // Physical bitmap pixel
  TColorRGB = packed record
    r, g, b: BYTE;
  end;
  PColorRGB = ^TColorRGB;

  // Physical bitmap ScanLine (row)
  TRGBList = packed array[0..0] of TColorRGB;
  PRGBList = ^TRGBList;

procedure TJvPaintFX.Strecth(Src, Dst: TBitmap; Filter: TFilterProc;
  AWidth: Single);
var
  xscale, yscale: Single; // Zoom Scale factors
  i, j, k: Integer; // Loop variables
  center: Single; // Filter calculation variables
  Width, fscale, weight: Single; // Filter calculation variables
  left, right: Integer; // Filter calculation variables
  n: Integer; // Pixel number
  Work: TBitmap;
  contrib: PCListList;
  rgb: TRGB;
  color: TColorRGB;
  {$IFDEF USE_SCANLINE}
  SourceLine,
    DestLine: PRGBList;
  SourcePixel,
    DestPixel: PColorRGB;
  Delta,
    DestDelta: Integer;
  {$ENDIF}
  SrcWidth,
    SrcHeight,
    DstWidth,
    DstHeight: Integer;

  function Color2RGB(Color: TColor): TColorRGB;
  begin
    Result.r := Color and $000000FF;
    Result.g := (Color and $0000FF00) shr 8;
    Result.b := (Color and $00FF0000) shr 16;
  end;

  function RGB2Color(Color: TColorRGB): TColor;
  begin
    Result := Color.r or (Color.g shl 8) or (Color.b shl 16);
  end;

begin
  DstWidth := Dst.Width;
  DstHeight := Dst.Height;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then
    raise EJVCLException.Create('Source bitmap too small');

  // Create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.Height := SrcHeight;
    Work.Width := DstWidth;
    // xscale := DstWidth / SrcWidth;
    // yscale := DstHeight / SrcHeight;
    // Improvement suggested by David Ullrich:
    if (SrcWidth = 1) then
      xscale := DstWidth / SrcWidth
    else
      xscale := (DstWidth - 1) / (SrcWidth - 1);
    if (SrcHeight = 1) then
      yscale := DstHeight / SrcHeight
    else
      yscale := (DstHeight - 1) / (SrcHeight - 1);
    // This implementation only works on 24-bit images because it uses
    // TBitmap.ScanLine
    {$IFDEF USE_SCANLINE}
    Src.PixelFormat := pf24bit;
    Dst.PixelFormat := Src.PixelFormat;
    Work.PixelFormat := Src.PixelFormat;
    {$ENDIF}

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth * sizeof(TCList));
    // Horizontal sub-sampling
    // Scales from bigger to smaller Width
    if (xscale < 1.0) then
    begin
      Width := AWidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to DstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(Width * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - Width);
        // right := floor(center + Width);
        left := floor(center - Width);
        right := ceil(center + Width);
        for j := left to right do
        begin
          weight := Filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            Continue;
          if (j < 0) then
            n := -j
          else
          if (j >= SrcWidth) then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end
    else
      // Horizontal super-sampling
      // Scales from smaller to bigger Width
    begin
      for i := 0 to DstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(AWidth * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - AWidth);
        // right := floor(center + AWidth);
        left := floor(center - AWidth);
        right := ceil(center + AWidth);
        for j := left to right do
        begin
          weight := Filter(center - j);
          if (weight = 0.0) then
            Continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcWidth then
            n := SrcWidth - j + SrcWidth - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for k := 0 to SrcHeight - 1 do
    begin
      {$IFDEF USE_SCANLINE}
      SourceLine := Src.ScanLine[k];
      DestPixel := Work.ScanLine[k];
      {$ENDIF}
      for i := 0 to DstWidth - 1 do
      begin
        rgb.r := 0.0;
        rgb.g := 0.0;
        rgb.b := 0.0;
        for j := 0 to contrib^[i].n - 1 do
        begin
          {$IFDEF USE_SCANLINE}
          color := SourceLine^[contrib^[i].p^[j].pixel];
          {$ELSE}
          color := Color2RGB(Src.Canvas.Pixels[contrib^[i].p^[j].pixel, k]);
          {$ENDIF}
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            Continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if rgb.r > 255.0 then
          color.r := 255
        else
        if rgb.r < 0.0 then
          color.r := 0
        else
          color.r := Round(rgb.r);
        if rgb.g > 255.0 then
          color.g := 255
        else
        if rgb.g < 0.0 then
          color.g := 0
        else
          color.g := Round(rgb.g);
        if rgb.b > 255.0 then
          color.b := 255
        else
        if rgb.b < 0.0 then
          color.b := 0
        else
          color.b := Round(rgb.b);
        {$IFDEF USE_SCANLINE}
        // Set new pixel value
        DestPixel^ := color;
        // Move on to next column
        Inc(DestPixel);
        {$ELSE}
        Work.Canvas.Pixels[i, k] := RGB2Color(color);
        {$ENDIF}
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for i := 0 to DstWidth - 1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight * sizeof(TCList));
    // Vertical sub-sampling
    // Scales from bigger to smaller Height
    if (yscale < 1.0) then
    begin
      Width := AWidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to DstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(Width * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - Width);
        // right := floor(center + Width);
        left := floor(center - Width);
        right := ceil(center + Width);
        for j := left to right do
        begin
          weight := Filter((center - j) / fscale) / fscale;
          if weight = 0.0 then
            Continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcHeight then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end
    end
    else
      // Vertical super-sampling
      // Scales from smaller to bigger Height
    begin
      for i := 0 to DstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(AWidth * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - AWidth);
        // right := floor(center + AWidth);
        left := floor(center - AWidth);
        right := ceil(center + AWidth);
        for j := left to right do
        begin
          weight := Filter(center - j);
          if weight = 0.0 then
            Continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcHeight then
            n := SrcHeight - j + SrcHeight - 1
          else
            n := j;
          k := contrib^[i].n;
          contrib^[i].n := contrib^[i].n + 1;
          contrib^[i].p^[k].pixel := n;
          contrib^[i].p^[k].weight := weight;
        end;
      end;
    end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
    {$IFDEF USE_SCANLINE}
    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Dst.ScanLine[0];
    DestDelta := Integer(Dst.ScanLine[1]) - Integer(DestLine);
    {$ENDIF}
    for k := 0 to DstWidth - 1 do
    begin
      {$IFDEF USE_SCANLINE}
      DestPixel := pointer(DestLine);
      {$ENDIF}
      for i := 0 to DstHeight - 1 do
      begin
        rgb.r := 0;
        rgb.g := 0;
        rgb.b := 0;
        // weight := 0.0;
        for j := 0 to contrib^[i].n - 1 do
        begin
          {$IFDEF USE_SCANLINE}
          color := PColorRGB(Integer(SourceLine) + contrib^[i].p^[j].pixel * Delta)^;
          {$ELSE}
          color := Color2RGB(Work.Canvas.Pixels[k, contrib^[i].p^[j].pixel]);
          {$ENDIF}
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            Continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if rgb.r > 255.0 then
          color.r := 255
        else
        if rgb.r < 0.0 then
          color.r := 0
        else
          color.r := Round(rgb.r);
        if rgb.g > 255.0 then
          color.g := 255
        else
        if rgb.g < 0.0 then
          color.g := 0
        else
          color.g := Round(rgb.g);
        if rgb.b > 255.0 then
          color.b := 255
        else
        if rgb.b < 0.0 then
          color.b := 0
        else
          color.b := Round(rgb.b);
        {$IFDEF USE_SCANLINE}
        DestPixel^ := color;
        Inc(Integer(DestPixel), DestDelta);
        {$ELSE}
        Dst.Canvas.Pixels[k, i] := RGB2Color(color);
        {$ENDIF}
      end;
      {$IFDEF USE_SCANLINE}
      Inc(SourceLine, 1);
      Inc(DestLine, 1);
      {$ENDIF}
    end;

    // Free the memory allocated for vertical filter weights
    for i := 0 to DstHeight - 1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

  finally
    Work.Free;
  end;
end;

procedure TJvPaintFX.Grow(Src1, Src2, Dst: TBitmap; Amount: Extended; x, y: Integer);
var
  bm: TBitmap;
  h, w, hr, wr: Integer;
begin
  w := Src1.Width;
  h := Src1.Height;
  Dst.Width := w;
  Dst.Height := h;
  Dst.Canvas.Draw(0, 0, Src1);
  wr := Round(Amount * w);
  hr := Round(Amount * h);
  bm := TBitmap.Create;
  bm.Width := wr;
  bm.Height := hr;
  Strecth(Src2, bm, ResampleFilters[4].Filter, ResampleFilters[4].Width);
  Dst.Canvas.Draw(x, y, bm);
  bm.Free;
end;

procedure TJvPaintFX.SpotLight(var Src: TBitmap; Amount: Integer;
  Spot: TRect);
var
  bm: TBitmap;
  w, h: Integer;
begin
  Darkness(Src, Amount);
  w := Src.Width;
  h := Src.Height;
  bm := TBitmap.Create;
  bm.Width := w;
  bm.Height := h;
  bm.Canvas.Brush.color := clBlack;
  bm.Canvas.FillRect(rect(0, 0, w, h));
  bm.Canvas.brush.Color := clWhite;
  bm.Canvas.Ellipse(Spot.left, spot.top, spot.right, spot.bottom);
  bm.Transparent := True;
  bm.TransparentColor := clWhite;
  Src.Canvas.Draw(0, 0, bm);
  bm.Free;
end;

procedure TJvPaintFX.FlipDown(Src: TBitmap);
var
  dest: TBitmap;
  w, h, x, y: Integer;
  pd, ps: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  dest := TBitmap.Create;
  dest.Width := w;
  dest.Height := h;
  dest.PixelFormat := pf24bit;
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    pd := dest.ScanLine[y];
    ps := Src.ScanLine[h - 1 - y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := ps[x * 3];
      pd[x * 3 + 1] := ps[x * 3 + 1];
      pd[x * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
  Src.assign(dest);
  dest.Free;
end;

procedure TJvPaintFX.FlipRight(Src: TBitmap);
var
  dest: TBitmap;
  w, h, x, y: Integer;
  pd, ps: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  dest := TBitmap.Create;
  dest.Width := w;
  dest.Height := h;
  dest.PixelFormat := pf24bit;
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    pd := dest.ScanLine[y];
    ps := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := ps[(w - 1 - x) * 3];
      pd[x * 3 + 1] := ps[(w - 1 - x) * 3 + 1];
      pd[x * 3 + 2] := ps[(w - 1 - x) * 3 + 2];
    end;
  end;
  Src.assign(dest);
  dest.Free;
end;

procedure TJvPaintFX.Trace(Src: TBitmap; Intensity: Integer);
var
  x, y, i: Integer;
  P1, P2, P3, P4: PByteArray;
  tb, TraceB: Byte;
  hasb: Boolean;
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Src.Width;
  Bitmap.Height := Src.Height;
  Bitmap.Canvas.draw(0, 0, Src);
  Bitmap.PixelFormat := pf8bit;
  Src.PixelFormat := pf24bit;
  hasb := False;
  TraceB := $00;
  tb := 0;
  for i := 1 to Intensity do
  begin
    for y := 0 to Bitmap.Height - 2 do
    begin
      P1 := Bitmap.ScanLine[y];
      P2 := Bitmap.ScanLine[y + 1];
      P3 := Src.ScanLine[y];
      P4 := Src.ScanLine[y + 1];
      x := 0;
      repeat
        if p1[x] <> p1[x + 1] then
        begin
          if not hasb then
          begin
            tb := p1[x + 1];
            hasb := True;
            p3[x * 3] := TraceB;
            p3[x * 3 + 1] := TraceB;
            p3[x * 3 + 2] := TraceB;
          end
          else
          begin
            if p1[x] <> tb then
            begin
              p3[x * 3] := TraceB;
              p3[x * 3 + 1] := TraceB;
              p3[x * 3 + 2] := TraceB;
            end
            else
            begin
              p3[(x + 1) * 3] := TraceB;
              p3[(x + 1) * 3 + 1] := TraceB;
              p3[(x + 1) * 3 + 1] := TraceB;
            end;
          end;
        end;
        if p1[x] <> p2[x] then
        begin
          if not hasb then
          begin
            tb := p2[x];
            hasb := True;
            p3[x * 3] := TraceB;
            p3[x * 3 + 1] := TraceB;
            p3[x * 3 + 2] := TraceB;
          end
          else
          begin
            if p1[x] <> tb then
            begin
              p3[x * 3] := TraceB;
              p3[x * 3 + 1] := TraceB;
              p3[x * 3 + 2] := TraceB;
            end
            else
            begin
              p4[x * 3] := TraceB;
              p4[x * 3 + 1] := TraceB;
              p4[x * 3 + 2] := TraceB;
            end;
          end;
        end;
        Inc(x);
      until x >= (Bitmap.Width - 2);
    end;
    // do the same in the opposite direction
    // only when Intensity > 1
    if i > 1 then
      for y := Bitmap.Height - 1 downto 1 do
      begin
        P1 := Bitmap.ScanLine[y];
        P2 := Bitmap.ScanLine[y - 1];
        P3 := Src.ScanLine[y];
        P4 := Src.ScanLine[y - 1];
        x := Bitmap.Width - 1;
        repeat
          if p1[x] <> p1[x - 1] then
          begin
            if not hasb then
            begin
              tb := p1[x - 1];
              hasb := True;
              p3[x * 3] := TraceB;
              p3[x * 3 + 1] := TraceB;
              p3[x * 3 + 2] := TraceB;
            end
            else
            begin
              if p1[x] <> tb then
              begin
                p3[x * 3] := TraceB;
                p3[x * 3 + 1] := TraceB;
                p3[x * 3 + 2] := TraceB;
              end
              else
              begin
                p3[(x - 1) * 3] := TraceB;
                p3[(x - 1) * 3 + 1] := TraceB;
                p3[(x - 1) * 3 + 2] := TraceB;
              end;
            end;
          end;
          if p1[x] <> p2[x] then
          begin
            if not hasb then
            begin
              tb := p2[x];
              hasb := True;
              p3[x * 3] := TraceB;
              p3[x * 3 + 1] := TraceB;
              p3[x * 3 + 2] := TraceB;
            end
            else
            begin
              if p1[x] <> tb then
              begin
                p3[x * 3] := TraceB;
                p3[x * 3 + 1] := TraceB;
                p3[x * 3 + 2] := TraceB;
              end
              else
              begin
                p4[x * 3] := TraceB;
                p4[x * 3 + 1] := TraceB;
                p4[x * 3 + 2] := TraceB;
              end;
            end;
          end;
          Dec(x);
        until x <= 1;
      end;
  end;
  Bitmap.Free;
end;

procedure TJvPaintFX.ShadowUpLeft(Src: TBitmap);
var
  x, y: Integer;
  Bitmap: TBitmap;
  P1, P2: PByteArray;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Src.Width;
  Bitmap.Height := Src.Height;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.Canvas.draw(0, 0, Src);
  for y := 0 to Bitmap.Height - 5 do
  begin
    P1 := Bitmap.ScanLine[y];
    P2 := Bitmap.ScanLine[y + 4];
    for x := 0 to Bitmap.Width - 5 do
      if P1[x * 3] > P2[(x + 4) * 3] then
      begin
        P1[x * 3] := P2[(x + 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x + 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x + 4) * 3 + 2] + 1;
      end;
  end;
  Src.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TJvPaintFX.ShadowUpRight(Src: TBitmap);
var
  x, y: Integer;
  Bitmap: TBitmap;
  P1, P2: PByteArray;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Src.Width;
  Bitmap.Height := Src.Height;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.Canvas.draw(0, 0, Src);
  for y := 0 to Bitmap.Height - 5 do
  begin
    P1 := Bitmap.ScanLine[y];
    P2 := Bitmap.ScanLine[y + 4];
    for x := Bitmap.Width - 1 downto 4 do
      if P1[x * 3] > P2[(x - 4) * 3] then
      begin
        P1[x * 3] := P2[(x - 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x - 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x - 4) * 3 + 2] + 1;
      end;
  end;
  Src.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TJvPaintFX.ShadowDownLeft(Src: TBitmap);
var
  x, y: Integer;
  Bitmap: TBitmap;
  P1, P2: PByteArray;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Src.Width;
  Bitmap.Height := Src.Height;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.Canvas.draw(0, 0, Src);
  for y := Bitmap.Height - 1 downto 4 do
  begin
    P1 := Bitmap.ScanLine[y];
    P2 := Bitmap.ScanLine[y - 4];
    for x := 0 to Bitmap.Width - 5 do
      if P1[x * 3] > P2[(x + 4) * 3] then
      begin
        P1[x * 3] := P2[(x + 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x + 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x + 4) * 3 + 2] + 1;
      end;
  end;
  Src.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TJvPaintFX.ShadowDownRight(Src: TBitmap);
var
  x, y: Integer;
  Bitmap: TBitmap;
  P1, P2: PByteArray;
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := Src.Width;
  Bitmap.Height := Src.Height;
  Bitmap.PixelFormat := pf24bit;
  Bitmap.Canvas.draw(0, 0, Src);
  for y := Bitmap.Height - 1 downto 4 do
  begin
    P1 := Bitmap.ScanLine[y];
    P2 := Bitmap.ScanLine[y - 4];
    for x := Bitmap.Width - 1 downto 4 do
      if P1[x * 3] > P2[(x - 4) * 3] then
      begin
        P1[x * 3] := P2[(x - 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x - 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x - 4) * 3 + 2] + 1;
      end;
  end;
  Src.Assign(Bitmap);
  Bitmap.Free;
end;

procedure TJvPaintFX.SemiOpaque(Src, Dst: TBitmap);
var
  b: TBitmap;
  P: Pbytearray;
  x, y: Integer;
begin
  b := TBitmap.Create;
  b.Width := Src.Width;
  b.Height := Src.Height;
  b.PixelFormat := pf24bit;
  b.Canvas.draw(0, 0, Src);
  for y := 0 to b.Height - 1 do
  begin
    p := b.ScanLine[y];
    if (y mod 2) = 0 then
    begin
      for x := 0 to b.Width - 1 do
        if (x mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end
    else
    begin
      for x := 0 to b.Width - 1 do
        if ((x + 1) mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end;
  end;
  b.Transparent := True;
  b.TransparentColor := clWhite;
  Dst.Canvas.draw(0, 0, b);
  b.Free;

end;

procedure TJvPaintFX.QuartoOpaque(Src, Dst: TBitmap);
var
  b: TBitmap;
  P: Pbytearray;
  x, y: Integer;
begin
  b := TBitmap.Create;
  b.Width := Src.Width;
  b.Height := Src.Height;
  b.PixelFormat := pf24bit;
  b.Canvas.draw(0, 0, Src);
  for y := 0 to b.Height - 1 do
  begin
    p := b.ScanLine[y];
    if (y mod 2) = 0 then
    begin
      for x := 0 to b.Width - 1 do
        if (x mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end
    else
    begin
      for x := 0 to b.Width - 1 do
      begin
        p[x * 3] := $FF;
        p[x * 3 + 1] := $FF;
        p[x * 3 + 2] := $FF;
      end;

    end;
  end;
  b.Transparent := True;
  b.TransparentColor := clWhite;
  Dst.Canvas.draw(0, 0, b);
  b.Free;
end;

procedure TJvPaintFX.FoldRight(Src1, Src2, Dst: TBitmap; Amount: Extended);
var
  w, h, x, y, xf, xf0: Integer;
  ps1, ps2, pd: pbytearray;
begin
  Src1.PixelFormat := pf24bit;
  Src2.PixelFormat := pf24bit;
  w := Src1.Width;
  h := Src2.Height;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  xf := Round(Amount * w);
  for y := 0 to h - 1 do
  begin
    ps1 := Src1.ScanLine[y];
    ps2 := Src2.ScanLine[y];
    pd := Dst.ScanLine[y];
    for x := 0 to xf do
    begin
      xf0 := xf + (xf - x);
      if xf0 < w then
      begin
        pd[xf0 * 3] := ps1[x * 3];
        pd[xf0 * 3 + 1] := ps1[x * 3 + 1];
        pd[xf0 * 3 + 2] := ps1[x * 3 + 2];
        pd[x * 3] := ps2[x * 3];
        pd[x * 3 + 1] := ps2[x * 3 + 1];
        pd[x * 3 + 2] := ps2[x * 3 + 2];
      end;
    end;
    if (2 * xf) < w - 1 then
      for x := 2 * xf + 1 to w - 1 do
      begin
        pd[x * 3] := ps1[x * 3];
        pd[x * 3 + 1] := ps1[x * 3 + 1];
        pd[x * 3 + 2] := ps1[x * 3 + 2];
      end;
  end;
end;

procedure TJvPaintFX.Mandelbrot(Src: TBitmap; Factor: Integer);
const
  maxX = 1.25;
  minX = -2;
  maxY = 1.25;
  minY = -1.25;
var
  w, h, x, y : Integer;
  dx, dy: Extended;
  Line: pbytearray;
  color: Integer;

  function IsMandel(CA, CBi: Extended): Integer;
  const
    MAX_ITERATION = 64;

  var
    OLD_A: Extended; {just a variable to keep 'a' from being destroyed}
    A, B: Extended; {function Z divided in real and imaginary parts}
    LENGTH_Z: Extended; {length of Z, sqrt(length_z)>2 => Z->infinity}
    iteration: Integer;
  begin
    A := 0; {initialize Z(0) = 0}
    B := 0;

    ITERATION := 0; {initialize iteration}

    repeat
      OLD_A := A; {saves the 'a'  (Will be destroyed in next line}

      A := A * A - B * B + CA;
      B := 2 * OLD_A * B + CBi;
      ITERATION := ITERATION + 1;
      LENGTH_Z := A * A + B * B;
    until (LENGTH_Z >= 4) or (ITERATION > MAX_ITERATION);
    Result := iteration;
  end;

begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  dx := (MaxX - MinX) / w;
  dy := (Maxy - MinY) / h;
  for y := 0 to h - 1 do
  begin
    Line := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      color := IsMandel(MinX + x * dx, MinY + y * dy);
      if color > Factor then
        color := $FF
      else
        color := $00;
      Line[x * 3] := color;
      Line[x * 3 + 1] := color;
      Line[x * 3 + 2] := color;
    end;
  end;
end;

procedure TJvPaintFX.MaskMandelbrot(Src: TBitmap; Factor: Integer);
var
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  bm.Width := Src.Width;
  bm.Height := Src.Height;
  Mandelbrot(bm, Factor);
  bm.Transparent := True;
  bm.TransparentColor := clWhite;
  Src.Canvas.draw(0, 0, bm);
  bm.Free;
end;

procedure TJvPaintFX.KeepBlue(Src: TBitmap; Factor: Extended);
var
  x, y, w, h: Integer;
  Line: pbytearray;
begin
  Src.PixelFormat := pf24bit;
  w := Src.Width;
  h := Src.Height;
  for y := 0 to h - 1 do
  begin
    Line := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      Line[x * 3] := Round(Factor * Line[x * 3]);
      Line[x * 3 + 1] := 0;
      Line[x * 3 + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.KeepGreen(Src: TBitmap; Factor: Extended);
var
  x, y, w, h: Integer;
  Line: pbytearray;
begin
  Src.PixelFormat := pf24bit;
  w := Src.Width;
  h := Src.Height;
  for y := 0 to h - 1 do
  begin
    Line := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      Line[x * 3 + 1] := Round(Factor * Line[x * 3 + 1]);
      Line[x * 3] := 0;
      Line[x * 3 + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.KeepRed(Src: TBitmap; Factor: Extended);
var
  x, y, w, h: Integer;
  Line: pbytearray;
begin
  Src.PixelFormat := pf24bit;
  w := Src.Width;
  h := Src.Height;
  for y := 0 to h - 1 do
  begin
    Line := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      Line[x * 3 + 2] := Round(Factor * Line[x * 3 + 2]);
      Line[x * 3 + 1] := 0;
      Line[x * 3] := 0;
    end;
  end;
end;

procedure TJvPaintFX.Shake(Src, Dst: TBitmap; Factor: Extended);
var
  x, y, h, w, dx: Integer;
  p: pbytearray;
begin
  Dst.Canvas.draw(0, 0, Src);
  Dst.PixelFormat := pf24bit;
  w := Dst.Width;
  h := Dst.Height;
  dx := Round(Factor * w);
  if dx = 0 then
    Exit;
  if dx > (w div 2) then
    Exit;

  for y := 0 to h - 1 do
  begin
    p := Dst.ScanLine[y];
    if (y mod 2) = 0 then
      for x := dx to w - 1 do
      begin
        p[(x - dx) * 3] := p[x * 3];
        p[(x - dx) * 3 + 1] := p[x * 3 + 1];
        p[(x - dx) * 3 + 2] := p[x * 3 + 2];
      end
    else
      for x := w - 1 downto dx do
      begin
        p[x * 3] := p[(x - dx) * 3];
        p[x * 3 + 1] := p[(x - dx) * 3 + 1];
        p[x * 3 + 2] := p[(x - dx) * 3 + 2];
      end;
  end;

end;

procedure TJvPaintFX.ShakeDown(Src, Dst: TBitmap; Factor: Extended);
var
  x, y, h, w, dy: Integer;
  p, p2, p3: pbytearray;
begin
  Dst.Canvas.draw(0, 0, Src);
  Dst.PixelFormat := pf24bit;
  w := Dst.Width;
  h := Dst.Height;
  dy := Round(Factor * h);
  if dy = 0 then
    Exit;
  if dy > (h div 2) then
    Exit;

  for y := dy to h - 1 do
  begin
    p := Dst.ScanLine[y];
    p2 := Dst.ScanLine[y - dy];
    for x := 0 to w - 1 do
      if (x mod 2) = 0 then
      begin
        p2[x * 3] := p[x * 3];
        p2[x * 3 + 1] := p[x * 3 + 1];
        p2[x * 3 + 2] := p[x * 3 + 2];
      end;
  end;
  for y := h - 1 - dy downto 0 do
  begin
    p := Dst.ScanLine[y];
    p3 := Dst.ScanLine[y + dy];
    for x := 0 to w - 1 do
      if (x mod 2) <> 0 then
      begin
        p3[x * 3] := p[x * 3];
        p3[x * 3 + 1] := p[x * 3 + 1];
        p3[x * 3 + 2] := p[x * 3 + 2];
      end;
  end;
end;

procedure TJvPaintFX.Plasma(Src1, Src2, Dst: TBitmap; Scale, Turbulence: Extended);
var
  cval, sval: array[0..255] of Integer;
  i, x, y, w, h, xx, yy: Integer;
  Asin, Acos: Extended;
  ps1, ps2, pd: pbytearray;
begin
  w := Src1.Width;
  h := Src1.Height;
  if Turbulence < 10 then
    Turbulence := 10;
  if Scale < 5 then
    Scale := 5;
  for i := 0 to 255 do
  begin
    sincos(i / Turbulence, Asin, Acos);
    sval[i] := Round(-Scale * Asin);
    cval[i] := Round(Scale * Acos);
  end;
  for y := 0 to h - 1 do
  begin
    pd := Dst.ScanLine[y];
    ps2 := Src2.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      xx := x + sval[ps2[x * 3]];
      yy := y + cval[ps2[x * 3]];
      if (xx >= 0) and (xx < w) and (yy >= 0) and (yy < h) then
      begin
        ps1 := Src1.ScanLine[yy];
        pd[x * 3] := ps1[xx * 3];
        pd[x * 3 + 1] := ps1[xx * 3 + 1];
        pd[x * 3 + 2] := ps1[xx * 3 + 2];
      end;
    end;
  end;
  ;
end;

procedure TJvPaintFX.SplitRound(Src, Dst: TBitmap; Amount: Integer; Style: TLightBrush);
var
  x, y, w, c, c00, dx, CX: Integer;
  R, R00: TRect;
  bm, bm2: TBitmap;
  p0, p00, p1: pbytearray;
begin
  if Amount = 0 then
  begin
    Dst.Canvas.Draw(0, 0, Src);
    Exit;
  end;
  CX := Src.Width div 2;
  if Amount > CX then
    Amount := CX;
  w := Src.Width;
  bm := TBitmap.Create;
  bm.PixelFormat := pf24bit;
  bm.Height := 1;
  bm.Width := CX;
  bm2 := TBitmap.Create;
  bm2.PixelFormat := pf24bit;
  bm2.Height := 1;
  bm2.Width := CX;
  p0 := bm.ScanLine[0];
  p00 := bm2.ScanLine[0];
  dx := 0;
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to CX - 1 do
    begin
      c := x * 3;
      c00 := (CX + x) * 3;
      p0[c] := p1[c];
      p0[c + 1] := p1[c + 1];
      p0[c + 2] := p1[c + 2];
      p00[c] := p1[c00];
      p00[c + 1] := p1[c00 + 1];
      p00[c + 2] := p1[c00 + 2];
    end;
    case Style of
      mbSplitRound:
        dx := Round(Amount * Abs(Sin(y / (Src.Height - 1) * Pi)));
      mbSplitWaste:
        dx := Round(Amount * Abs(Cos(y / (Src.Height - 1) * Pi)));
    end;
    R := rect(0, y, dx, y + 1);
    Dst.Canvas.StretchDraw(R, bm);
    R00 := rect(w - 1 - dx, y, w - 1, y + 1);
    Dst.Canvas.StretchDraw(R00, bm2);
  end;
  bm.Free;
  bm2.Free;
end;

procedure TJvPaintFX.Emboss(var Bmp: TBitmap);
var
  x, y: Integer;
  p1, p2: Pbytearray;
begin
  for y := 0 to Bmp.Height - 2 do
  begin
    p1 := bmp.ScanLine[y];
    p2 := bmp.ScanLine[y + 1];
    for x := 0 to Bmp.Width - 4 do
    begin
      p1[x * 3] := (p1[x * 3] + (p2[(x + 3) * 3] xor $FF)) shr 1;
      p1[x * 3 + 1] := (p1[x * 3 + 1] + (p2[(x + 3) * 3 + 1] xor $FF)) shr 1;
      p1[x * 3 + 2] := (p1[x * 3 + 2] + (p2[(x + 3) * 3 + 2] xor $FF)) shr 1;
    end;
  end;

end;

procedure TJvPaintFX.FilterRed(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c + 2] > Min) and (p1[c + 2] < Max) then
        p1[c + 2] := $FF
      else
        p1[c + 2] := 0;
      p1[c] := 0;
      p1[c + 1] := 0;
    end;
  end;
end;

procedure TJvPaintFX.FilterGreen(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c + 1] > Min) and (p1[c + 1] < Max) then
        p1[c + 1] := $FF
      else
        p1[c + 1] := 0;
      p1[c] := 0;
      p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.FilterBlue(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c] > Min) and (p1[c] < Max) then
        p1[c] := $FF
      else
        p1[c] := 0;
      p1[c + 1] := 0;
      p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.FilterXRed(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c + 2] > Min) and (p1[c + 2] < Max) then
        p1[c + 2] := $FF
      else
        p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.FilterXGreen(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c + 1] > Min) and (p1[c + 1] < Max) then
        p1[c + 1] := $FF
      else
        p1[c + 1] := 0;
    end;
  end;
end;

procedure TJvPaintFX.FilterXBlue(Src: TBitmap; Min, Max: Integer);
var
  c, x, y: Integer;
  p1: pbytearray;

begin
  for y := 0 to Src.Height - 1 do
  begin
    p1 := Src.ScanLine[y];
    for x := 0 to Src.Width - 1 do
    begin
      c := x * 3;
      if (p1[c] > Min) and (p1[c] < Max) then
        p1[c] := $FF
      else
        p1[c] := 0;
    end;
  end;
end;

procedure TJvPaintFX.DrawMandelJulia(Src: TBitmap; x0, y0, x1, y1: Extended; Niter: Integer; Mandel: Boolean);
const
  //Number if colors. If this is changed, the number of mapped colors must also be changed
  nc = 16;
type
  TJvRGBTriplet = record
    r, g, b: Byte;
  end;
var
  X, XX, Y, YY, Cx, Cy, Dx, Dy, XSquared, YSquared: Double;
  Nx, Ny, Py, Px, I: Integer;
  Line: pbytearray;
  cc: array[0..15] of TJvRGBTriplet;
  AColor: TColor;
begin
  Src.PixelFormat := pf24bit;
  for i := 0 to 15 do
  begin
    AColor := ConvertColor(i);
    cc[i].b := GetBValue(ColorToRGB(AColor));
    cc[i].g := GetGValue(ColorToRGB(AColor));
    cc[i].r := GetRValue(ColorToRGB(AColor));
  end;
  if Niter < nc then
    Niter := nc;
  try
    Nx := Src.Width;
    Ny := Src.Height;
    Cx := 0;
    Cy := 1;
    Dx := (x1 - x0) / nx;
    Dy := (y1 - y0) / ny;
    Py := 0;
    while (PY < Ny) do
    begin
      Line := Src.ScanLine[py];
      PX := 0;
      while (Px < Nx) do
      begin
        x := x0 + px * dx;
        y := y0 + py * dy;
        if (mandel) then
        begin
          CX := x;
          CY := y;
          x := 0;
          y := 0;
        end;
        xsquared := 0;
        ysquared := 0;
        I := 0;
        while (I <= niter) and (xsquared + ysquared < (4)) do
        begin
          xsquared := x * x;
          ysquared := y * y;
          xx := xsquared - ysquared + CX;
          yy := (2 * x * y) + CY;
          x := xx;
          y := yy;
          I := I + 1;
        end;
        I := I - 1;
        if (i = niter) then
          i := 0
        else
          i := Round(i / (niter / nc));
        //        Canvas.Pixels[PX,PY] := ConvertColor(I);
        Line[px * 3] := cc[i].b;
        Line[px * 3 + 1] := cc[i].g;
        Line[px * 3 + 2] := cc[i].r;
        Px := Px + 1;
      end;
      Py := Py + 1;
    end;
  finally
  end;
end;

procedure TJvPaintFX.Invert(Src: TBitmap);
var
  w, h, x, y: Integer;
  p: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      p[x * 3] := not p[x * 3];
      p[x * 3 + 1] := not p[x * 3 + 1];
      p[x * 3 + 2] := not p[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.MirrorRight(Src: TBitmap);
var
  w, h, x, y: Integer;
  p: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := Src.ScanLine[y];
    for x := 0 to w div 2 do
    begin
      p[(w - 1 - x) * 3] := p[x * 3];
      p[(w - 1 - x) * 3 + 1] := p[x * 3 + 1];
      p[(w - 1 - x) * 3 + 2] := p[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.MirrorDown(Src: TBitmap);
var
  w, h, x, y: Integer;
  p1, p2: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  for y := 0 to h div 2 do
  begin
    p1 := Src.ScanLine[y];
    p2 := Src.ScanLine[h - 1 - y];
    for x := 0 to w - 1 do
    begin
      p2[x * 3] := p1[x * 3];
      p2[x * 3 + 1] := p1[x * 3 + 1];
      p2[x * 3 + 2] := p1[x * 3 + 2];
    end;
  end;
end;

// resample image as triangles

procedure TJvPaintFX.Triangles(Src: TBitmap; Amount: Integer);
type
  Ttriplet = record
    r, g, b: Byte;
  end;

var
  w, h, x, y, tb, tm, te: Integer;
  ps: pbytearray;
  T: ttriplet;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  if Amount < 5 then
    Amount := 5;
  Amount := (Amount div 2) * 2 + 1;
  tm := Amount div 2;
  for y := 0 to h - 1 do
  begin
    ps := Src.ScanLine[y];
    t.r := ps[0];
    t.g := ps[1];
    t.b := ps[2];
    tb := y mod (Amount - 1);
    if tb > tm then
      tb := 2 * tm - tb;
    if tb = 0 then
      tb := Amount;
    te := tm + Abs(tm - (y mod Amount));
    for x := 0 to w - 1 do
    begin
      if (x mod tb) = 0 then
      begin
        t.r := ps[x * 3];
        t.g := ps[x * 3 + 1];
        t.b := ps[x * 3 + 2];
      end;
      if ((x mod te) = 1) and (tb <> 0) then
      begin
        t.r := ps[x * 3];
        t.g := ps[x * 3 + 1];
        t.b := ps[x * 3 + 2];
      end;
      ps[x * 3] := t.r;
      ps[x * 3 + 1] := t.g;
      ps[x * 3 + 2] := t.b;
    end;
  end;
end;

procedure TJvPaintFX.RippleTooth(Src: TBitmap; Amount: Integer);
var
  x, y: Integer;
  P1, P2: PByteArray;
  b: Byte;

begin
  Src.PixelFormat := pf24bit;
  Amount := Min(Src.Height div 2, Amount);
  for y := Src.Height - 1 - Amount downto 0 do
  begin
    P1 := Src.ScanLine[y];
    b := 0;
    for x := 0 to Src.Width - 1 do
    begin
      P2 := Src.ScanLine[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      Inc(b);
      if b > Amount then
        b := 0;
    end;
  end;
end;

procedure TJvPaintFX.RippleTriangle(Src: TBitmap; Amount: Integer);
var
  x, y: Integer;
  P1, P2: PByteArray;
  b: Byte;
  doinc: Boolean;

begin
  Amount := Min(Src.Height div 2, Amount);
  for y := Src.Height - 1 - Amount downto 0 do
  begin
    P1 := Src.ScanLine[y];
    b := 0;
    doinc := True;
    for x := 0 to Src.Width - 1 do
    begin
      P2 := Src.ScanLine[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      if doinc then
      begin
        Inc(b);
        if b > Amount then
        begin
          doinc := False;
          b := Amount - 1;
        end;
      end
      else
      begin
        if b = 0 then
        begin
          doinc := True;
          b := 2;
        end;
        Dec(b);
      end;
    end;
  end;
end;

procedure TJvPaintFX.RippleRandom(Src: TBitmap; Amount: Integer);
var
  x, y: Integer;
  P1, P2: PByteArray;
  b: Byte;

begin
  Amount := Min(Src.Height div 2, Amount);
  Src.PixelFormat := pf24bit;
  Randomize;
  for y := Src.Height - 1 - Amount downto 0 do
  begin
    P1 := Src.ScanLine[y];
    b := 0;
    for x := 0 to Src.Width - 1 do
    begin
      P2 := Src.ScanLine[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      b := Random(Amount);
    end;
  end;
end;

procedure TJvPaintFX.TexturizeOverlap(Src: TBitmap; Amount: Integer);
var
  w, h, x, y, xo: Integer;
  bm: TBitmap;
  ARect: TRect;
begin
  bm := TBitmap.Create;
  Amount := Min(Src.Width div 2, Amount);
  Amount := Min(Src.Height div 2, Amount);
  xo := Round(Amount * 2 / 3);
  bm.Width := Amount;
  bm.Height := Amount;
  w := Src.Width;
  h := Src.Height;
  ARect := rect(0, 0, Amount, Amount);
  bm.Canvas.StretchDraw(ARect, Src);
  y := 0;
  repeat
    x := 0;
    repeat
      Src.Canvas.Draw(x, y, bm);
      x := x + xo;
    until x >= w;
    y := y + xo;
  until y >= h;
  bm.Free;
end;

procedure TJvPaintFX.TexturizeTile(Src: TBitmap; Amount: Integer);
var
  w, h, x, y: Integer;
  bm: TBitmap;
  ARect: TRect;
begin
  bm := TBitmap.Create;
  Amount := Min(Src.Width div 2, Amount);
  Amount := Min(Src.Height div 2, Amount);
  bm.Width := Amount;
  bm.Height := Amount;
  w := Src.Width;
  h := Src.Height;
  ARect := rect(0, 0, Amount, Amount);
  bm.Canvas.StretchDraw(ARect, Src);
  y := 0;
  repeat
    x := 0;
    repeat
      Src.Canvas.Draw(x, y, bm);
      x := x + bm.Width;
    until x >= w;
    y := y + bm.Height;
  until y >= h;
  bm.Free;
end;

procedure TJvPaintFX.HeightMap(Src: TBitmap; Amount: Integer);
var
  bm: TBitmap;
  w, h, x, y: Integer;
  pb, ps: pbytearray;
  c: Integer;
begin
  h := Src.Height;
  w := Src.Width;
  bm := TBitmap.Create;
  bm.Width := w;
  bm.Height := h;
  bm.PixelFormat := pf24bit;
  Src.PixelFormat := pf24bit;
  bm.Canvas.Draw(0, 0, Src);
  for y := 0 to h - 1 do
  begin
    pb := bm.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      c := Round((pb[x * 3] + pb[x * 3 + 1] + pb[x * 3 + 2]) / 3 / 255 * Amount);
      if (y - c) >= 0 then
      begin
        ps := Src.ScanLine[y - c];
        ps[x * 3] := pb[x * 3];
        ps[x * 3 + 1] := pb[x * 3 + 1];
        ps[x * 3 + 2] := pb[x * 3 + 2];
      end;
    end;
  end;
  bm.Free;
end;

procedure TJvPaintFX.Turn(Src, Dst: TBitmap);
var
  w, h, x, y: Integer;
  ps, pd: pbytearray;
begin
  h := Src.Height;
  w := Src.Width;
  Src.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  Dst.Height := w;
  Dst.Width := h;
  for y := 0 to h - 1 do
  begin
    ps := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd := Dst.ScanLine[w - 1 - x];
      pd[y * 3] := ps[x * 3];
      pd[y * 3 + 1] := ps[x * 3 + 1];
      pd[y * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.TurnRight(Src, Dst: TBitmap);
var
  w, h, x, y: Integer;
  ps, pd: pbytearray;
begin
  h := Src.Height;
  w := Src.Width;
  Src.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  Dst.Height := w;
  Dst.Width := h;
  for y := 0 to h - 1 do
  begin
    ps := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd := Dst.ScanLine[x];
      pd[(h - 1 - y) * 3] := ps[x * 3];
      pd[(h - 1 - y) * 3 + 1] := ps[x * 3 + 1];
      pd[(h - 1 - y) * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.ExtractColor(Src: TBitmap; AColor: TColor);
var
  w, h, x, y: Integer;
  p: pbytearray;
  EColor: TColor;
  r, g, b: Byte;
begin
  w := Src.Width;
  h := Src.Height;
  EColor := ColorToRGB(AColor);
  r := GetRValue(EColor);
  g := GetGValue(EColor);
  b := GetBValue(EColor);
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      if ((p[x * 3] <> b) or (p[x * 3 + 1] <> g) or (p[x * 3 + 2] <> r)) then
      begin
        p[x * 3] := $00;
        p[x * 3 + 1] := $00;
        p[x * 3 + 2] := $00;
      end;
    end
  end;
  Src.Transparent := True;
  Src.TransparentColor := clBlack;
end;

procedure TJvPaintFX.ExcludeColor(Src: TBitmap; AColor: TColor);
var
  w, h, x, y: Integer;
  p: pbytearray;
  EColor: TColor;
  r, g, b: Byte;
begin
  w := Src.Width;
  h := Src.Height;
  EColor := ColorToRGB(AColor);
  r := GetRValue(EColor);
  g := GetGValue(EColor);
  b := GetBValue(EColor);
  Src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := Src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      if ((p[x * 3] = b) and (p[x * 3 + 1] = g) and (p[x * 3 + 2] = r)) then
      begin
        p[x * 3] := $00;
        p[x * 3 + 1] := $00;
        p[x * 3 + 2] := $00;
      end;
    end
  end;
  Src.Transparent := True;
  Src.TransparentColor := clBlack;
end;

procedure TJvPaintFX.Blend(Src1, Src2, Dst: TBitmap; Amount: Extended);
var
  w, h, x, y: Integer;
  ps1, ps2, pd: pbytearray;
begin
  w := Src1.Width;
  h := Src1.Height;
  Dst.Width := w;
  Dst.Height := h;
  Src1.PixelFormat := pf24bit;
  Src2.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps1 := Src1.ScanLine[y];
    ps2 := Src2.ScanLine[y];
    pd := Dst.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := Round((1 - Amount) * ps1[x * 3] + Amount * ps2[x * 3]);
      pd[x * 3 + 1] := Round((1 - Amount) * ps1[x * 3 + 1] + Amount * ps2[x * 3 + 1]);
      pd[x * 3 + 2] := Round((1 - Amount) * ps1[x * 3 + 2] + Amount * ps2[x * 3 + 2]);
    end;
  end;
end;

procedure TJvPaintFX.Solarize(Src, Dst: TBitmap; Amount: Integer);
var
  w, h, x, y: Integer;
  ps, pd: pbytearray;
  c: Integer;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps := Src.ScanLine[y];
    pd := Dst.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      c := (ps[x * 3] + ps[x * 3 + 1] + ps[x * 3 + 2]) div 3;
      if c > Amount then
      begin
        pd[x * 3] := 255 - ps[x * 3];
        pd[x * 3 + 1] := 255 - ps[x * 3 + 1];
        pd[x * 3 + 2] := 255 - ps[x * 3 + 2];
      end
      else
      begin
        pd[x * 3] := ps[x * 3];
        pd[x * 3 + 1] := ps[x * 3 + 1];
        pd[x * 3 + 2] := ps[x * 3 + 2];
      end;
    end;
  end;
end;

procedure TJvPaintFX.Posterize(Src, Dst: TBitmap; Amount: Integer);
var
  w, h, x, y: Integer;
  ps, pd: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  Src.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps := Src.ScanLine[y];
    pd := Dst.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := Round(ps[x * 3] / Amount) * Amount;
      pd[x * 3 + 1] := Round(ps[x * 3 + 1] / Amount) * Amount;
      pd[x * 3 + 2] := Round(ps[x * 3 + 2] / Amount) * Amount;
    end;
  end;
end;

end.
