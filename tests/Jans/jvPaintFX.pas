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
{$I JEDI.INC}
unit JvPaintFX;

interface
{$DEFINE USE_SCANLINE}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  math;

type

  // Type of a filter for use with Stretch()
  TFilterProc = function(Value: Single): Single;
  TLightBrush = (lbBrightness, lbContrast, lbSaturation,
    lbfisheye, lbrotate, lbtwist, lbrimple,
    mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound,
    mbround2, mbsplitround, mbsplitwaste);

  TJvPaintFX = class(TComponent)
  private
    function ConvertColor(Value: Integer): TColor;

    { Private declarations }
  protected
    { Protected declarations }

  public
    { Public declarations }
    procedure Solorize(src, dst: tbitmap; amount: integer);
    procedure Posterize(src, dst: tbitmap; amount: integer);
    procedure Blend(src1, src2, dst: tbitmap; amount: extended);
    procedure ExtractColor(src: TBitmap; Acolor: tcolor);
    procedure ExcludeColor(src: TBitmap; Acolor: tcolor);
    procedure turn(src, dst: tbitmap);
    procedure turnRight(src, dst: Tbitmap);
    procedure HeightMap(src: Tbitmap; amount: integer);
    procedure TexturizeTile(src: TBitmap; amount: integer);
    procedure TexturizeOverlap(src: TBitmap; amount: integer);
    procedure RippleRandom(src: TBitmap; amount: integer);
    procedure RippleTooth(src: TBitmap; amount: integer);
    procedure RippleTriangle(src: TBitmap; amount: integer);
    procedure Triangles(src: TBitmap; amount: integer);
    procedure DrawMandelJulia(src: Tbitmap; x0, y0, x1, y1: extended;
      Niter: integer; Mandel: Boolean);
    procedure filterxblue(src: tbitmap; min, max: integer);
    procedure filterxgreen(src: tbitmap; min, max: integer);
    procedure filterxred(src: tbitmap; min, max: integer);
    procedure filterblue(src: tbitmap; min, max: integer);
    procedure filtergreen(src: tbitmap; min, max: integer);
    procedure filterred(src: tbitmap; min, max: integer);
    procedure Emboss(var Bmp: TBitmap);
    procedure Plasma(src1, src2, dst: Tbitmap; scale, turbulence: extended);
    procedure Shake(src, dst: Tbitmap; factor: extended);
    procedure ShakeDown(src, dst: Tbitmap; factor: extended);
    procedure KeepBlue(src: Tbitmap; factor: extended);
    procedure KeepGreen(src: Tbitmap; factor: extended);
    procedure KeepRed(src: Tbitmap; factor: extended);
    procedure MandelBrot(src: Tbitmap; factor: integer);
    procedure MaskMandelBrot(src: Tbitmap; factor: integer);
    procedure FoldRight(src1, src2, dst: Tbitmap; amount: extended);
    procedure QuartoOpaque(src, dst: tbitmap);
    procedure semiOpaque(src, dst: Tbitmap);
    procedure ShadowDownLeft(src: tbitmap);
    procedure ShadowDownRight(src: tbitmap);
    procedure shadowupleft(src: Tbitmap);
    procedure shadowupright(src: tbitmap);
    procedure Darkness(var src: tbitmap; Amount: integer);
    procedure Trace(src: Tbitmap; intensity: integer);
    procedure FlipRight(src: Tbitmap);
    procedure FlipDown(src: Tbitmap);
    procedure SpotLight(var src: Tbitmap; Amount: integer; Spot: TRect);
    procedure splitlight(var clip: tbitmap; amount: integer);
    procedure MakeSeamlessClip(var clip: tbitmap; seam: integer);
    procedure Wave(var clip: tbitmap; amount, inference, style: integer);
    procedure Mosaic(var Bm: TBitmap; size: Integer);
    function TrimInt(i, Min, Max: Integer): Integer;
    procedure SmoothRotate(var Src, Dst: TBitmap; cx, cy: Integer;
      Angle: Extended);
    procedure SmoothResize(var Src, Dst: TBitmap);
    procedure Twist(var Bmp, Dst: TBitmap; Amount: integer);
    procedure SplitBlur(var clip: tbitmap; Amount: integer);
    procedure GaussianBlur(var clip: tbitmap; Amount: integer);
    procedure Smooth(var clip: tbitmap; Weight: Integer);
    procedure GrayScale(var clip: tbitmap);
    procedure AddColorNoise(var clip: tbitmap; Amount: Integer);
    procedure AddMonoNoise(var clip: tbitmap; Amount: Integer);
    procedure Contrast(var clip: tbitmap; Amount: Integer);
    procedure Lightness(var clip: tbitmap; Amount: Integer);
    procedure Saturation(var clip: tbitmap; Amount: Integer);
    procedure Spray(var clip: tbitmap; Amount: Integer);
    procedure AntiAlias(clip: tbitmap);
    procedure AntiAliasRect(clip: tbitmap; XOrigin, YOrigin, XFinal, YFinal: Integer);
    procedure SmoothPoint(var clip: tbitmap; xk, yk: integer);
    procedure FishEye(var Bmp, Dst: TBitmap; Amount: Extended);
    procedure marble(var src, dst: tbitmap; scale: extended; turbulence: integer);
    procedure marble2(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble3(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble4(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble5(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble6(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble7(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure marble8(var src, dst: tbitmap; scale: extended;
      turbulence: integer);
    procedure squeezehor(src, dst: tbitmap; amount: integer; style: TLightBrush);
    procedure splitround(src, dst: tbitmap; amount: integer; style: TLightBrush);

    procedure tile(src, dst: TBitmap; amount: integer);
    // Interpolator
    // Src:	Source bitmap
    // Dst:	Destination bitmap
    // filter:	Weight calculation filter
    // fwidth:	Relative sample radius
    procedure Strecth(Src, Dst: TBitmap; filter: TFilterProc; fwidth: single);
    procedure Grow(Src1, Src2, Dst: TBitmap; amount: extended; x, y: integer);
    procedure Invert(src: tbitmap);
    procedure MirrorRight(src: Tbitmap);
    procedure MirrorDown(src: Tbitmap);
  published
    { Published declarations }
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
  MaxPixelCount = 32768;

  // -----------------------------------------------------------------------------
  //
  //			List of Filters
  //
  // -----------------------------------------------------------------------------

  ResampleFilters: array[0..6] of record
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

type
  TRGBTripleArray = array[0..MaxPixelCount - 1] of
    TRGBTriple;
  pRGBTripleArray = ^TRGBTripleArray;
  TFColor = record
    b, g, r: Byte;
  end;

  // Bell filter

function BellFilter(Value: Single): Single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 0.5) then
    Result := 0.75 - Sqr(Value)
  else if (Value < 1.5) then
  begin
    Value := Value - 1.5;
    Result := 0.5 * Sqr(Value);
  end
  else
    Result := 0.0;
end;

// Box filter
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

// Hermite filter

function HermiteFilter(Value: Single): Single;
begin
  // f(t) = 2|t|^3 - 3|t|^2 + 1, -1 <= t <= 1
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := (2.0 * Value - 3.0) * Sqr(Value) + 1.0
  else
    Result := 0.0;
end;

// Lanczos3 filter

function Lanczos3Filter(Value: Single): Single;

function SinC(Value: Single): Single;
  begin
    if (Value <> 0.0) then
    begin
      Value := Value * Pi;
      Result := sin(Value) / Value
    end
    else
      Result := 1.0;
  end;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 3.0) then
    Result := SinC(Value) * SinC(Value / 3.0)
  else
    Result := 0.0;
end;

function MitchellFilter(Value: Single): Single;
const
  B = (1.0 / 3.0);
  C = (1.0 / 3.0);
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  tt := Sqr(Value);
  if (Value < 1.0) then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt))
      + ((-18.0 + 12.0 * B + 6.0 * C) * tt)
      + (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else if (Value < 2.0) then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt))
      + ((6.0 * B + 30.0 * C) * tt)
      + ((-12.0 * B - 48.0 * C) * Value)
      + (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

// B-spline filter

function SplineFilter(Value: Single): Single;
var
  tt: single;
begin
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2.0 / 3.0;
  end
  else if (Value < 2.0) then
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
  if (Value < 0.0) then
    Value := -Value;
  if (Value < 1.0) then
    Result := 1.0 - Value
  else
    Result := 0.0;
end;

function IntToByte(i: Integer): Byte;
begin
  if i > 255 then
    Result := 255
  else if i < 0 then
    Result := 0
  else
    Result := i;
end;

procedure TJvPaintFX.AddColorNoise(var clip: tbitmap; Amount: Integer);
var
  p0: pbytearray;
  x, y, r, g, b: Integer;

begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.ScanLine[y];
    for x := 0 to clip.Width - 1 do
    begin
      r := p0[x * 3] + (Random(Amount) - (Amount shr 1));
      g := p0[x * 3 + 1] + (Random(Amount) - (Amount shr 1));
      b := p0[x * 3 + 2] + (Random(Amount) - (Amount shr 1));
      p0[x * 3] := IntToByte(r);
      p0[x * 3 + 1] := IntToByte(g);
      p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;

procedure TJvPaintFX.AddMonoNoise(var clip: tbitmap; Amount: Integer);
var
  p0: pbytearray;
  x, y, a, r, g, b: Integer;
begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    for x := 0 to clip.Width - 1 do
    begin
      a := Random(Amount) - (Amount shr 1);
      r := p0[x * 3] + a;
      g := p0[x * 3 + 1] + a;
      b := p0[x * 3 + 2] + a;
      p0[x * 3] := IntToByte(r);
      p0[x * 3 + 1] := IntToByte(g);
      p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;

procedure TJvPaintFX.AntiAlias(clip: tbitmap);
begin
  AntiAliasRect(clip, 0, 0, clip.width, clip.height);
end;

procedure TJvPaintFX.AntiAliasRect(clip: tbitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);
var
  Memo, x, y: Integer; (* Composantes primaires des points environnants *)
  p0, p1, p2: pbytearray;

begin
  if XFinal < XOrigin then
  begin
    Memo := XOrigin;
    XOrigin := XFinal;
    XFinal := Memo;
  end; (* Inversion des valeurs   *)
  if YFinal < YOrigin then
  begin
    Memo := YOrigin;
    YOrigin := YFinal;
    YFinal := Memo;
  end; (* si diff‚rence n‚gative*)
  XOrigin := max(1, XOrigin);
  YOrigin := max(1, YOrigin);
  XFinal := min(clip.width - 2, XFinal);
  YFinal := min(clip.height - 2, YFinal);
  clip.PixelFormat := pf24bit;
  for y := YOrigin to YFinal do
  begin
    p0 := clip.ScanLine[y - 1];
    p1 := clip.scanline[y];
    p2 := clip.ScanLine[y + 1];
    for x := XOrigin to XFinal do
    begin
      p1[x * 3] := (p0[x * 3] + p2[x * 3] + p1[(x - 1) * 3] + p1[(x + 1) * 3]) div 4;
      p1[x * 3 + 1] := (p0[x * 3 + 1] + p2[x * 3 + 1] + p1[(x - 1) * 3 + 1] + p1[(x + 1) * 3 + 1]) div 4;
      p1[x * 3 + 2] := (p0[x * 3 + 2] + p2[x * 3 + 2] + p1[(x - 1) * 3 + 2] + p1[(x + 1) * 3 + 2]) div 4;
    end;
  end;
end;

procedure TJvPaintFX.Contrast(var clip: tbitmap; Amount: Integer);
var
  p0: pbytearray;
  rg, gg, bg, r, g, b, x, y: Integer;
begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    for x := 0 to clip.Width - 1 do
    begin
      r := p0[x * 3];
      g := p0[x * 3 + 1];
      b := p0[x * 3 + 2];
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
      p0[x * 3] := IntToByte(r);
      p0[x * 3 + 1] := IntToByte(g);
      p0[x * 3 + 2] := IntToByte(b);
    end;
  end;
end;

procedure TJvPaintFX.FishEye(var Bmp, Dst: TBitmap; Amount: Extended);
var
  xmid, ymid: Single;
  fx, fy: Single;
  r1, r2: Single;
  ifx, ify: integer;
  dx, dy: Single;
  rmax: Single;
  ty, tx: Integer;
  weight_x, weight_y: array[0..1] of Single;
  weight: Single;
  new_red, new_green: Integer;
  new_blue: Integer;
  total_red, total_green: Single;
  total_blue: Single;
  ix, iy: Integer;
  sli, slo: PByteArray;
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
      else if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.scanline[ify + iy]
          else
            sli := Bmp.scanline[Bmp.Height - ify - iy];
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
      slo := Dst.scanline[ty];
      slo[tx * 3] := Round(total_red);
      slo[tx * 3 + 1] := Round(total_green);
      slo[tx * 3 + 2] := Round(total_blue);

    end;
  end;
end;

procedure TJvPaintFX.GaussianBlur(var clip: tbitmap; Amount: integer);
var
  i: Integer;
begin
  for i := Amount downto 0 do
    SplitBlur(clip, 3);
end;

procedure TJvPaintFX.GrayScale(var clip: tbitmap);
var
  p0: pbytearray;
  Gray, x, y: Integer;
begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    for x := 0 to clip.Width - 1 do
    begin
      Gray := Round(p0[x * 3] * 0.3 + p0[x * 3 + 1] * 0.59 + p0[x * 3 + 2] * 0.11);
      p0[x * 3] := Gray;
      p0[x * 3 + 1] := Gray;
      p0[x * 3 + 2] := Gray;
    end;
  end;
end;

procedure TJvPaintFX.Lightness(var clip: tbitmap; Amount: Integer);
var
  p0: pbytearray;
  r, g, b, x, y: Integer;
begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    for x := 0 to clip.Width - 1 do
    begin
      r := p0[x * 3];
      g := p0[x * 3 + 1];
      b := p0[x * 3 + 2];
      p0[x * 3] := IntToByte(r + ((255 - r) * Amount) div 255);
      p0[x * 3 + 1] := IntToByte(g + ((255 - g) * Amount) div 255);
      p0[x * 3 + 2] := IntToByte(b + ((255 - b) * Amount) div 255);
    end;
  end;
end;

procedure TJvPaintFX.Darkness(var src: tbitmap; Amount: integer);
var
  p0: pbytearray;
  r, g, b, x, y: Integer;
begin
  src.pixelformat := pf24bit;
  for y := 0 to src.Height - 1 do
  begin
    p0 := src.scanline[y];
    for x := 0 to src.Width - 1 do
    begin
      r := p0[x * 3];
      g := p0[x * 3 + 1];
      b := p0[x * 3 + 2];
      p0[x * 3] := IntToByte(r - ((r) * Amount) div 255);
      p0[x * 3 + 1] := IntToByte(g - ((g) * Amount) div 255);
      p0[x * 3 + 2] := IntToByte(b - ((b) * Amount) div 255);
    end;
  end;
end;

procedure TJvPaintFX.marble(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.width := w;
  dst.height := h;
  dst.canvas.Draw(0, 0, src);
  for y := 0 to h - 1 do
  begin
    yy := scale * cos((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -scale * sin((x mod turbulence) / scale);
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble2(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    yy := scale * cos((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -scale * sin((x mod turbulence) / scale);
      xm := round(abs(x + xx - yy));
      ym := round(abs(y + yy - xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble3(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    yy := scale * cos((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -scale * sin((x mod turbulence) / scale);
      xm := round(abs(x - xx + yy));
      ym := round(abs(y - yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble4(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    yy := scale * sin((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -scale * cos((x mod turbulence) / scale);
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble5(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := h - 1 downto 0 do
  begin
    yy := scale * cos((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := w - 1 downto 0 do
    begin
      xx := -scale * sin((x mod turbulence) / scale);
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble6(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    yy := scale * cos((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -tan((x mod turbulence) / scale) / scale;
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble7(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    yy := scale * sin((y mod turbulence) / scale);

    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := -tan((x mod turbulence) / scale) / (scale * scale);
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.marble8(var src, dst: tbitmap; scale: extended;
  turbulence: integer);
var
  x, xm, y, ym: integer;
  xx, yy: extended;
  p1, p2: pbytearray;
  w, h: integer;
  ax: extended;
begin
  h := src.height;
  w := src.width;
  dst.assign(src);
  for y := 0 to h - 1 do
  begin
    ax := (y mod turbulence) / scale;
    yy := scale * sin(ax) * cos(1.5 * ax);
    p1 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      ax := (x mod turbulence) / scale;
      xx := -scale * sin(2 * ax) * cos(ax);
      xm := round(abs(x + xx + yy));
      ym := round(abs(y + yy + xx));
      if ym < h then
      begin
        p2 := dst.scanline[ym];
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

procedure TJvPaintFX.Saturation(var clip: tbitmap; Amount: Integer);
var
  p0: pbytearray;
  Gray, r, g, b, x, y: Integer;
begin
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    for x := 0 to clip.Width - 1 do
    begin
      r := p0[x * 3];
      g := p0[x * 3 + 1];
      b := p0[x * 3 + 2];
      Gray := (r + g + b) div 3;
      p0[x * 3] := IntToByte(Gray + (((r - Gray) * Amount) div 255));
      p0[x * 3 + 1] := IntToByte(Gray + (((g - Gray) * Amount) div 255));
      p0[x * 3 + 2] := IntToByte(Gray + (((b - Gray) * Amount) div 255));
    end;
  end;
end;

procedure TJvPaintFX.Smooth(var clip: tbitmap; Weight: Integer);
begin
  //
end;

procedure TJvPaintFX.SmoothPoint(var clip: tbitmap; xk, yk: integer);
var
  Bleu, Vert, Rouge, w, h: Integer;
  color: TFColor;
  Acolor: tcolor;
  BB, GG, RR: array[1..5] of Integer;
begin
  w := clip.width;
  h := clip.height;
  if (xk > 0) and (yk > 0) and (xk < w - 1) and (yk < h - 1) then
    with clip.canvas do
    begin
      Acolor := colortorgb(pixels[xk, yk - 1]);
      color.r := getrvalue(Acolor);
      color.g := getgvalue(Acolor);
      color.b := getbvalue(Acolor);
      RR[1] := color.r;
      GG[1] := color.g;
      BB[1] := color.b;
      Acolor := colortorgb(pixels[xk + 1, yk]);
      color.r := getrvalue(Acolor);
      color.g := getgvalue(Acolor);
      color.b := getbvalue(Acolor);
      RR[2] := color.r;
      GG[2] := color.g;
      BB[2] := color.b;
      acolor := colortorgb(pixels[xk, yk + 1]);
      color.r := getrvalue(Acolor);
      color.g := getgvalue(Acolor);
      color.b := getbvalue(Acolor);
      RR[3] := color.r;
      GG[3] := color.g;
      BB[3] := color.b;
      acolor := colortorgb(pixels[xk - 1, yk]);
      color.r := getrvalue(Acolor);
      color.g := getgvalue(Acolor);
      color.b := getbvalue(Acolor);
      RR[4] := color.r;
      GG[4] := color.g;
      BB[4] := color.b;
      Bleu := (BB[1] + (BB[2] + BB[3] + BB[4])) div 4; (* Valeur moyenne *)
      Vert := (GG[1] + (GG[2] + GG[3] + GG[4])) div 4; (* en cours d'‚valuation        *)
      Rouge := (RR[1] + (RR[2] + RR[3] + RR[4])) div 4;
      color.r := rouge;
      color.g := vert;
      color.b := bleu;
      pixels[xk, yk] := rgb(color.r, color.g, color.b);
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
  Col1r, col1g, col1b, Col2r, col2g, col2b: byte;
begin
  xP2 := ((src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    Read := src.ScanLine[yP shr 15];
    if yP shr 16 < src.Height - 1 then
      Read2 := src.ScanLine[yP shr 15 + 1]
    else
      Read2 := src.ScanLine[yP shr 15];
    pc := Dst.scanline[y];
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

procedure TJvPaintFX.SmoothRotate(var Src, Dst: TBitmap; cx, cy: Integer;
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
    P3 := Dst.scanline[y];
    py := 2 * (y - cy) + 1;
    for x := 0 to Dst.Width - 1 do
    begin
      px := 2 * (x - cx) + 1;
      fx := (((px * cAngle - py * sAngle) - 1) / 2 + cx) - xDiff;
      fy := (((px * sAngle + py * cAngle) - 1) / 2 + cy) - yDiff;
      ifx := Round(fx);
      ify := Round(fy);

      if (ifx > -1) and (ifx < Src.Width) and (ify > -1) and (ify < Src.Height) then
      begin
        eww := fx - ifx;
        nsw := fy - ify;
        iy := TrimInt(ify + 1, 0, Src.Height - 1);
        ix := TrimInt(ifx + 1, 0, Src.Width - 1);
        P1 := Src.scanline[ify];
        P2 := Src.scanline[iy];
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

procedure TJvPaintFX.SplitBlur(var clip: tbitmap; Amount: integer);
var
  p0, p1, p2: pbytearray;
  cx, x, y: Integer;
  Buf: array[0..3, 0..2] of byte;
begin
  if Amount = 0 then Exit;
  for y := 0 to clip.Height - 1 do
  begin
    p0 := clip.scanline[y];
    if y - Amount < 0 then
      p1 := clip.scanline[y]
    else {y-Amount>0}
      p1 := clip.ScanLine[y - Amount];
    if y + Amount < clip.Height then
      p2 := clip.ScanLine[y + Amount]
    else {y+Amount>=Height}
      p2 := clip.ScanLine[clip.Height - y];

    for x := 0 to clip.Width - 1 do
    begin
      if x - Amount < 0 then
        cx := x
      else {x-Amount>0}
        cx := x - Amount;
      Buf[0, 0] := p1[cx * 3];
      Buf[0, 1] := p1[cx * 3 + 1];
      Buf[0, 2] := p1[cx * 3 + 2];
      Buf[1, 0] := p2[cx * 3];
      Buf[1, 1] := p2[cx * 3 + 1];
      Buf[1, 2] := p2[cx * 3 + 2];
      if x + Amount < clip.Width then
        cx := x + Amount
      else {x+Amount>=Width}
        cx := clip.Width - x;
      Buf[2, 0] := p1[cx * 3];
      Buf[2, 1] := p1[cx * 3 + 1];
      Buf[2, 2] := p1[cx * 3 + 2];
      Buf[3, 0] := p2[cx * 3];
      Buf[3, 1] := p2[cx * 3 + 1];
      Buf[3, 2] := p2[cx * 3 + 2];
      p0[x * 3] := (Buf[0, 0] + Buf[1, 0] + Buf[2, 0] + Buf[3, 0]) shr 2;
      p0[x * 3 + 1] := (Buf[0, 1] + Buf[1, 1] + Buf[2, 1] + Buf[3, 1]) shr 2;
      p0[x * 3 + 2] := (Buf[0, 2] + Buf[1, 2] + Buf[2, 2] + Buf[3, 2]) shr 2;
    end;
  end;
end;

procedure TJvPaintFX.Spray(var clip: tbitmap; Amount: Integer);
var
  i, j, x, y, w, h, Val: Integer;
begin
  h := clip.height;
  w := clip.Width;
  for i := 0 to w - 1 do
    for j := 0 to h - 1 do
    begin
      Val := Random(Amount);
      x := i + Val - Random(Val * 2);
      y := j + Val - Random(Val * 2);
      if (x > -1) and (x < w) and (y > -1) and (y < h) then
        clip.canvas.Pixels[i, j] := clip.canvas.Pixels[x, y];
    end;
end;

procedure TJvPaintFX.Mosaic(var Bm: TBitmap; size: Integer);
var
  x, y, i, j: integer;
  p1, p2: pbytearray;
  r, g, b: byte;
begin
  y := 0;
  repeat
    p1 := bm.scanline[y];
    //   x:=0;
    repeat
      j := 1;
      repeat
        p2 := bm.scanline[y];
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
            inc(x);
            inc(i);
          until (x >= bm.width) or (i > size);
        until x >= bm.width;
        inc(j);
        inc(y);
      until (y >= bm.height) or (j > size);
    until (y >= bm.height) or (x >= bm.width);
  until y >= bm.height;
end;

function TJvPaintFX.TrimInt(i, Min, Max: Integer): Integer;
begin
  if i > Max then
    Result := Max
  else if i < Min then
    Result := Min
  else
    Result := i;
end;

procedure TJvPaintFX.Twist(var Bmp, Dst: TBitmap; Amount: integer);
var
  fxmid, fymid: Single;
  txmid, tymid: Single;
  fx, fy: Single;
  tx2, ty2: Single;
  r: Single;
  theta: Single;
  ifx, ify: integer;
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
  if tx2 >= Bmp.Width then tx2 := Bmp.Width - 1;
  if ty2 >= Bmp.Height then ty2 := Bmp.Height - 1;

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
      else if ifx > Bmp.Width - 1 then
        ifx := ifx mod Bmp.Width;
      if ify < 0 then
        ify := Bmp.Height - 1 - (-ify mod Bmp.Height)
      else if ify > Bmp.Height - 1 then
        ify := ify mod Bmp.Height;

      total_red := 0.0;
      total_green := 0.0;
      total_blue := 0.0;
      for ix := 0 to 1 do
      begin
        for iy := 0 to 1 do
        begin
          if ify + iy < Bmp.Height then
            sli := Bmp.scanline[ify + iy]
          else
            sli := Bmp.scanline[Bmp.Height - ify - iy];
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
      slo := Dst.scanline[ty];
      slo[tx * 3] := Round(total_red);
      slo[tx * 3 + 1] := Round(total_green);
      slo[tx * 3 + 2] := Round(total_blue);
    end;
  end;
end;

procedure TJvPaintFX.Wave(var clip: tbitmap; amount, inference, style: integer);
var
  x, y: integer;
  BitMap: TBitMap;
  P1, P2: PByteArray;
  b: integer;
  fangle: real;
  wavex: integer;
begin
  BitMap := TBitMap.create;
  Bitmap.assign(clip);
  wavex := style;
  fangle := pi / 2 / amount;
  for y := BitMap.height - 1 - (2 * amount) downto amount do
  begin
    P1 := BitMap.ScanLine[y];
    b := 0;
    for x := 0 to Bitmap.width - 1 do
    begin
      P2 := clip.scanline[y + amount + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      case wavex of
        0: b := amount * variant(sin(fangle * x));
        1: b := amount * variant(sin(fangle * x) * cos(fangle * x));
        2: b := amount * variant(sin(fangle * x) * sin(inference * fangle * x));
      end;
    end;
  end;
  BitMap.free;
end;

procedure TJvPaintFX.MakeSeamlessClip(var clip: tbitmap; seam: integer);
var
  p0, p1, p2: pbytearray;
  h, w, i, j, sv, sh: integer;
  f0, f1, f2: real;
begin
  h := clip.height;
  w := clip.width;
  sv := h div seam;
  sh := w div seam;
  p1 := clip.scanline[0];
  p2 := clip.ScanLine[h - 1];
  for i := 0 to w - 1 do
  begin
    p1[i * 3] := p2[i * 3];
    p1[i * 3 + 1] := p2[i * 3 + 1];
    p1[i * 3 + 2] := p2[i * 3 + 2];
  end;
  p0 := clip.scanline[0];
  p2 := clip.scanline[sv];
  for j := 1 to sv - 1 do
  begin
    p1 := clip.scanline[j];
    for i := 0 to w - 1 do
    begin
      f0 := (p2[i * 3] - p0[i * 3]) / sv * j + p0[i * 3];
      p1[i * 3] := round(f0);
      f1 := (p2[i * 3 + 1] - p0[i * 3 + 1]) / sv * j + p0[i * 3 + 1];
      p1[i * 3 + 1] := round(f1);
      f2 := (p2[i * 3 + 2] - p0[i * 3 + 2]) / sv * j + p0[i * 3 + 2];
      p1[i * 3 + 2] := round(f2);
    end;
  end;
  for j := 0 to h - 1 do
  begin
    p1 := clip.scanline[j];
    p1[(w - 1) * 3] := p1[0];
    p1[(w - 1) * 3 + 1] := p1[1];
    p1[(w - 1) * 3 + 2] := p1[2];
    for i := 1 to sh - 1 do
    begin
      f0 := (p1[(w - sh) * 3] - p1[(w - 1) * 3]) / sh * i + p1[(w - 1) * 3];
      p1[(w - 1 - i) * 3] := round(f0);
      f1 := (p1[(w - sh) * 3 + 1] - p1[(w - 1) * 3 + 1]) / sh * i + p1[(w - 1) * 3 + 1];
      p1[(w - 1 - i) * 3 + 1] := round(f1);
      f2 := (p1[(w - sh) * 3 + 2] - p1[(w - 1) * 3 + 2]) / sh * i + p1[(w - 1) * 3 + 2];
      p1[(w - 1 - i) * 3 + 2] := round(f2);
    end;
  end;
end;

procedure TJvPaintFX.splitlight(var clip: tbitmap; amount: integer);
var
  x, y, i: integer;
  p1: pbytearray;

  function sinpixs(a: integer): integer;
  begin
    result := variant(sin(a / 255 * pi / 2) * 255);
  end;
begin
  for i := 1 to amount do
    for y := 0 to clip.height - 1 do
    begin
      p1 := clip.scanline[y];
      for x := 0 to clip.width - 1 do
      begin
        p1[x * 3] := sinpixs(p1[x * 3]);
        p1[x * 3 + 1] := sinpixs(p1[x * 3 + 1]);
        p1[x * 3 + 2] := sinpixs(p1[x * 3 + 2]);
      end;
    end;
end;

procedure TJvPaintFX.squeezehor(src, dst: tbitmap; amount: integer; style: TLightBrush);
var
  dx, x, y, c, cx: integer;
  R: trect;
  bm: tbitmap;
  p0, p1: pbytearray;
begin
  if amount > (src.width div 2) then
    amount := src.width div 2;
  bm := tbitmap.create;
  bm.PixelFormat := pf24bit;
  bm.height := 1;
  bm.width := src.width;
  cx := src.width div 2;
  p0 := bm.scanline[0];
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      p0[c] := p1[c];
      p0[c + 1] := p1[c + 1];
      p0[c + 2] := p1[c + 2];
    end;
    case style of
      mbhor:
        begin
          dx := amount;
          R := rect(dx, y, src.width - dx, y + 1);
        end;
      mbtop:
        begin
          dx := round((src.height - 1 - y) / src.height * amount);
          R := rect(dx, y, src.width - dx, y + 1);
        end;
      mbBottom:
        begin
          dx := round(y / src.height * amount);
          R := rect(dx, y, src.width - dx, y + 1);
        end;
      mbDiamond:
        begin
          dx := round(amount * abs(cos(y / (src.height - 1) * pi)));
          R := rect(dx, y, src.width - dx, y + 1);
        end;
      mbWaste:
        begin
          dx := round(amount * abs(sin(y / (src.height - 1) * pi)));
          R := rect(dx, y, src.width - dx, y + 1);
        end;
      mbRound:
        begin
          dx := round(amount * abs(sin(y / (src.height - 1) * pi)));
          R := rect(cx - dx, y, cx + dx, y + 1);
        end;
      mbRound2:
        begin
          dx := round(amount * abs(sin(y / (src.height - 1) * pi * 2)));
          R := rect(cx - dx, y, cx + dx, y + 1);
        end;
    end;
    dst.Canvas.StretchDraw(R, bm);
  end;
  bm.free;
end;

procedure TJvPaintFX.tile(src, dst: TBitmap; amount: integer);
var
  w, h, w2, h2, i, j: integer;
  bm: tbitmap;
begin
  w := src.width;
  h := src.height;
  dst.width := w;
  dst.height := h;
  dst.Canvas.draw(0, 0, src);
  if (amount <= 0) or ((w div amount) < 5) or ((h div amount) < 5) then exit;
  h2 := h div amount;
  w2 := w div amount;
  bm := tbitmap.create;
  bm.width := w2;
  bm.height := h2;
  bm.PixelFormat := pf24bit;
  smoothresize(src, bm);
  for j := 0 to amount - 1 do
    for i := 0 to amount - 1 do
      dst.canvas.Draw(i * w2, j * h2, bm);
  bm.free;
end;

// -----------------------------------------------------------------------------
//
//			Interpolator
//
// -----------------------------------------------------------------------------
type
  // Contributor for a pixel
  TContributor = record
    pixel: integer; // Source pixel
    weight: single; // Pixel weight
  end;

  TContributorList = array[0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: integer;
    p: PContributorList;
  end;

  TCListList = array[0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    r, g, b: single;
  end;

  // Physical bitmap pixel
  TColorRGB = packed record
    r, g, b: BYTE;
  end;
  PColorRGB = ^TColorRGB;

  // Physical bitmap scanline (row)
  TRGBList = packed array[0..0] of TColorRGB;
  PRGBList = ^TRGBList;

procedure TJvPaintFX.Strecth(Src, Dst: TBitmap; filter: TFilterProc;
  fwidth: single);
var
  xscale, yscale: single; // Zoom scale factors
  i, j, k: integer; // Loop variables
  center: single; // Filter calculation variables
  width, fscale, weight: single; // Filter calculation variables
  left, right: integer; // Filter calculation variables
  n: integer; // Pixel number
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
    DestDelta: integer;
  {$ENDIF}
  SrcWidth,
    SrcHeight,
    DstWidth,
    DstHeight: integer;

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
    raise Exception.Create('Source bitmap too small');

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
    // TBitmap.Scanline
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
    // Scales from bigger to smaller width
    if (xscale < 1.0) then
    begin
      width := fwidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to DstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
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
      // Scales from smaller to bigger width
    begin
      for i := 0 to DstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcWidth) then
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
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if (rgb.r > 255.0) then
          color.r := 255
        else if (rgb.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgb.r);
        if (rgb.g > 255.0) then
          color.g := 255
        else if (rgb.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgb.g);
        if (rgb.b > 255.0) then
          color.b := 255
        else if (rgb.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgb.b);
        {$IFDEF USE_SCANLINE}
        // Set new pixel value
        DestPixel^ := color;
        // Move on to next column
        inc(DestPixel);
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
    // Scales from bigger to smaller height
    if (yscale < 1.0) then
    begin
      width := fwidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to DstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
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
      // Scales from smaller to bigger height
    begin
      for i := 0 to DstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * sizeof(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if (weight = 0.0) then
            continue;
          if (j < 0) then
            n := -j
          else if (j >= SrcHeight) then
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
    Delta := integer(Work.ScanLine[1]) - integer(SourceLine);
    DestLine := Dst.ScanLine[0];
    DestDelta := integer(Dst.ScanLine[1]) - integer(DestLine);
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
          color := PColorRGB(integer(SourceLine) + contrib^[i].p^[j].pixel * Delta)^;
          {$ELSE}
          color := Color2RGB(Work.Canvas.Pixels[k, contrib^[i].p^[j].pixel]);
          {$ENDIF}
          weight := contrib^[i].p^[j].weight;
          if (weight = 0.0) then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if (rgb.r > 255.0) then
          color.r := 255
        else if (rgb.r < 0.0) then
          color.r := 0
        else
          color.r := round(rgb.r);
        if (rgb.g > 255.0) then
          color.g := 255
        else if (rgb.g < 0.0) then
          color.g := 0
        else
          color.g := round(rgb.g);
        if (rgb.b > 255.0) then
          color.b := 255
        else if (rgb.b < 0.0) then
          color.b := 0
        else
          color.b := round(rgb.b);
        {$IFDEF USE_SCANLINE}
        DestPixel^ := color;
        inc(integer(DestPixel), DestDelta);
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

procedure TJvPaintFX.Grow(Src1, Src2, Dst: TBitmap; amount: extended; x, y: integer);
var
  bm: tbitmap;
  h, w, hr, wr: integer;
begin
  w := src1.Width;
  h := src1.Height;
  Dst.Width := w;
  Dst.Height := h;
  Dst.Canvas.Draw(0, 0, Src1);
  wr := round(amount * w);
  hr := round(amount * h);
  bm := tbitmap.create;
  bm.width := wr;
  bm.height := hr;
  Strecth(Src2, bm, resamplefilters[4].filter, resamplefilters[4].width);
  Dst.Canvas.Draw(x, y, bm);
  bm.free;
end;

procedure TJvPaintFX.SpotLight(var src: Tbitmap; Amount: integer;
  Spot: TRect);
var
  bm: tbitmap;
  w, h: integer;
begin
  Darkness(src, amount);
  w := src.Width;
  h := src.Height;
  bm := tbitmap.create;
  bm.width := w;
  bm.height := h;
  bm.canvas.Brush.color := clblack;
  bm.canvas.FillRect(rect(0, 0, w, h));
  bm.canvas.brush.Color := clwhite;
  bm.canvas.Ellipse(Spot.left, spot.top, spot.right, spot.bottom);
  bm.transparent := true;
  bm.TransparentColor := clwhite;
  src.Canvas.Draw(0, 0, bm);
  bm.free;
end;

procedure TJvPaintFX.FlipDown(src: Tbitmap);
var
  dest: tbitmap;
  w, h, x, y: integer;
  pd, ps: pbytearray;
begin
  w := src.width;
  h := src.height;
  dest := tbitmap.create;
  dest.width := w;
  dest.height := h;
  dest.pixelformat := pf24bit;
  src.pixelformat := pf24bit;
  for y := 0 to h - 1 do
  begin
    pd := dest.scanline[y];
    ps := src.scanline[h - 1 - y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := ps[x * 3];
      pd[x * 3 + 1] := ps[x * 3 + 1];
      pd[x * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
  src.assign(dest);
  dest.free;
end;

procedure TJvPaintFX.FlipRight(src: Tbitmap);
var
  dest: tbitmap;
  w, h, x, y: integer;
  pd, ps: pbytearray;
begin
  w := src.width;
  h := src.height;
  dest := tbitmap.create;
  dest.width := w;
  dest.height := h;
  dest.pixelformat := pf24bit;
  src.pixelformat := pf24bit;
  for y := 0 to h - 1 do
  begin
    pd := dest.scanline[y];
    ps := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := ps[(w - 1 - x) * 3];
      pd[x * 3 + 1] := ps[(w - 1 - x) * 3 + 1];
      pd[x * 3 + 2] := ps[(w - 1 - x) * 3 + 2];
    end;
  end;
  src.assign(dest);
  dest.free;
end;

procedure TJvPaintFX.Trace(src: Tbitmap; intensity: integer);
var
  x, y, i: integer;
  P1, P2, P3, P4: PByteArray;
  tb, TraceB: byte;
  hasb: boolean;
  bitmap: tbitmap;
begin
  bitmap := tbitmap.create;
  bitmap.width := src.width;
  bitmap.height := src.height;
  bitmap.canvas.draw(0, 0, src);
  bitmap.PixelFormat := pf8bit;
  src.PixelFormat := pf24bit;
  hasb := false;
  TraceB := $00;
  for i := 1 to Intensity do
  begin
    for y := 0 to BitMap.height - 2 do
    begin
      P1 := BitMap.ScanLine[y];
      P2 := BitMap.scanline[y + 1];
      P3 := src.scanline[y];
      P4 := src.scanline[y + 1];
      x := 0;
      repeat
        if p1[x] <> p1[x + 1] then
        begin
          if not hasb then
          begin
            tb := p1[x + 1];
            hasb := true;
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
            hasb := true;
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
        inc(x);
      until x >= (BitMap.width - 2);
    end;
    // do the same in the opposite direction
    // only when intensity>1
    if i > 1 then
      for y := BitMap.height - 1 downto 1 do
      begin
        P1 := BitMap.ScanLine[y];
        P2 := BitMap.scanline[y - 1];
        P3 := src.scanline[y];
        P4 := src.scanline[y - 1];
        x := Bitmap.width - 1;
        repeat
          if p1[x] <> p1[x - 1] then
          begin
            if not hasb then
            begin
              tb := p1[x - 1];
              hasb := true;
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
              hasb := true;
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
          dec(x);
        until x <= 1;
      end;
  end;
  bitmap.free;
end;

procedure TJvPaintFX.shadowupleft(src: Tbitmap);
var
  c, c2, x, y: integer;
  BitMap: TBitMap;
  P1, P2: PByteArray;
begin
  BitMap := TBitMap.create;
  bitmap.width := src.width;
  bitmap.height := src.height;
  Bitmap.pixelformat := pf24bit;
  Bitmap.canvas.draw(0, 0, src);
  for y := 0 to BitMap.height - 5 do
  begin
    P1 := BitMap.ScanLine[y];
    P2 := BitMap.scanline[y + 4];
    for x := 0 to Bitmap.width - 5 do
      if P1[x * 3] > P2[(x + 4) * 3] then
      begin
        P1[x * 3] := P2[(x + 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x + 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x + 4) * 3 + 2] + 1;
      end;
  end;
  src.Assign(bitmap);
  BitMap.free;
end;

procedure TJvPaintFX.shadowupright(src: tbitmap);
var
  x, y: integer;
  BitMap: TBitMap;
  P1, P2: PByteArray;
begin
  BitMap := TBitMap.create;
  bitmap.width := src.width;
  bitmap.height := src.height;
  Bitmap.pixelformat := pf24bit;
  Bitmap.canvas.draw(0, 0, src);
  for y := 0 to bitmap.height - 5 do
  begin
    P1 := BitMap.ScanLine[y];
    P2 := BitMap.scanline[y + 4];
    for x := Bitmap.width - 1 downto 4 do
      if P1[x * 3] > P2[(x - 4) * 3] then
      begin
        P1[x * 3] := P2[(x - 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x - 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x - 4) * 3 + 2] + 1;
      end;
  end;
  src.Assign(bitmap);
  BitMap.free;
end;

procedure TJvPaintFX.ShadowDownLeft(src: tbitmap);
var
  x, y: integer;
  BitMap: TBitMap;
  P1, P2: PByteArray;
begin
  BitMap := TBitMap.create;
  bitmap.width := src.width;
  bitmap.height := src.height;
  Bitmap.pixelformat := pf24bit;
  Bitmap.canvas.draw(0, 0, src);
  for y := bitmap.height - 1 downto 4 do
  begin
    P1 := BitMap.ScanLine[y];
    P2 := BitMap.scanline[y - 4];
    for x := 0 to Bitmap.width - 5 do
      if P1[x * 3] > P2[(x + 4) * 3] then
      begin
        P1[x * 3] := P2[(x + 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x + 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x + 4) * 3 + 2] + 1;
      end;
  end;
  src.Assign(bitmap);
  BitMap.free;
end;

procedure TJvPaintFX.ShadowDownRight(src: tbitmap);
var
  x, y: integer;
  BitMap: TBitMap;
  P1, P2: PByteArray;
begin
  BitMap := TBitMap.create;
  bitmap.width := src.width;
  bitmap.height := src.height;
  Bitmap.pixelformat := pf24bit;
  Bitmap.canvas.draw(0, 0, src);
  for y := bitmap.height - 1 downto 4 do
  begin
    P1 := BitMap.ScanLine[y];
    P2 := BitMap.scanline[y - 4];
    for x := Bitmap.width - 1 downto 4 do
      if P1[x * 3] > P2[(x - 4) * 3] then
      begin
        P1[x * 3] := P2[(x - 4) * 3] + 1;
        P1[x * 3 + 1] := P2[(x - 4) * 3 + 1] + 1;
        P1[x * 3 + 2] := P2[(x - 4) * 3 + 2] + 1;
      end;
  end;
  src.Assign(bitmap);
  BitMap.free;
end;

procedure TJvPaintFX.semiOpaque(src, dst: Tbitmap);
var
  b: tbitmap;
  P: Pbytearray;
  x, y: integer;
begin
  b := tbitmap.create;
  b.width := src.width;
  b.height := src.height;
  b.PixelFormat := pf24bit;
  b.canvas.draw(0, 0, src);
  for y := 0 to b.height - 1 do
  begin
    p := b.scanline[y];
    if (y mod 2) = 0 then
    begin
      for x := 0 to b.width - 1 do
        if (x mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end
    else
    begin
      for x := 0 to b.width - 1 do
        if ((x + 1) mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end;
  end;
  b.transparent := true;
  b.transparentcolor := clwhite;
  dst.canvas.draw(0, 0, b);
  b.free;

end;

procedure TJvPaintFX.QuartoOpaque(src, dst: tbitmap);
var
  b: tbitmap;
  P: Pbytearray;
  x, y: integer;
begin
  b := tbitmap.create;
  b.width := src.width;
  b.height := src.height;
  b.PixelFormat := pf24bit;
  b.canvas.draw(0, 0, src);
  for y := 0 to b.height - 1 do
  begin
    p := b.scanline[y];
    if (y mod 2) = 0 then
    begin
      for x := 0 to b.width - 1 do
        if (x mod 2) = 0 then
        begin
          p[x * 3] := $FF;
          p[x * 3 + 1] := $FF;
          p[x * 3 + 2] := $FF;
        end;
    end
    else
    begin
      for x := 0 to b.width - 1 do
      begin
        p[x * 3] := $FF;
        p[x * 3 + 1] := $FF;
        p[x * 3 + 2] := $FF;
      end;

    end;
  end;
  b.transparent := true;
  b.transparentcolor := clwhite;
  dst.canvas.draw(0, 0, b);
  b.free;
end;

procedure TJvPaintFX.FoldRight(src1, src2, dst: Tbitmap; amount: extended);
var
  w, h, x, y, xf, xf0: integer;
  ps1, ps2, pd: pbytearray;
begin
  src1.PixelFormat := pf24bit;
  src2.PixelFormat := pf24bit;
  w := src1.width;
  h := src2.height;
  dst.width := w;
  dst.height := h;
  dst.PixelFormat := pf24bit;
  xf := round(amount * w);
  for y := 0 to h - 1 do
  begin
    ps1 := src1.ScanLine[y];
    ps2 := src2.scanline[y];
    pd := dst.scanline[y];
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

procedure TJvPaintFX.MandelBrot(src: Tbitmap; factor: integer);
const
  maxX = 1.25;
  minX = -2;
  maxY = 1.25;
  minY = -1.25;
var
  w, h, x, y, facx, facy: integer;
  Sa, Sbi, dx, dy: extended;
  p0: pbytearray;
  color: integer;
  xlo, xhi, ylo, yhi: extended;

  function IsMandel(CA, CBi: extended): integer;
  const
    MAX_ITERATION = 64;

  var
    OLD_A: extended; {just a variable to keep 'a' from being destroyed}
    A, B: extended; {function Z divided in real and imaginary parts}
    LENGTH_Z: extended; {length of Z, sqrt(length_z)>2 => Z->infinity}
    iteration: integer;
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
    result := iteration;
  end;

begin
  w := src.width;
  h := src.height;
  src.pixelformat := pf24bit;
  dx := (MaxX - MinX) / w;
  dy := (Maxy - MinY) / h;
  for y := 0 to h - 1 do
  begin
    p0 := src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      color := IsMandel(MinX + x * dx, MinY + y * dy);
      if color > factor then
        color := $FF
      else
        color := $00;
      p0[x * 3] := color;
      p0[x * 3 + 1] := color;
      p0[x * 3 + 2] := color;
    end;
  end;
end;

procedure TJvPaintFX.MaskMandelBrot(src: Tbitmap; factor: integer);
var
  bm: Tbitmap;
begin
  bm := tbitmap.create;
  bm.width := src.width;
  bm.height := src.height;
  MandelBrot(bm, factor);
  bm.transparent := true;
  bm.transparentcolor := clwhite;
  src.canvas.draw(0, 0, bm);
  bm.free;
end;

procedure TJvPaintFX.KeepBlue(src: Tbitmap; factor: extended);
var
  x, y, w, h: integer;
  p0: pbytearray;
begin
  src.PixelFormat := pf24bit;
  w := src.width;
  h := src.height;
  for y := 0 to h - 1 do
  begin
    p0 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      p0[x * 3] := round(factor * p0[x * 3]);
      p0[x * 3 + 1] := 0;
      p0[x * 3 + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.KeepGreen(src: Tbitmap; factor: extended);
var
  x, y, w, h: integer;
  p0: pbytearray;
begin
  src.PixelFormat := pf24bit;
  w := src.width;
  h := src.height;
  for y := 0 to h - 1 do
  begin
    p0 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      p0[x * 3 + 1] := round(factor * p0[x * 3 + 1]);
      p0[x * 3] := 0;
      p0[x * 3 + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.KeepRed(src: Tbitmap; factor: extended);
var
  x, y, w, h: integer;
  p0: pbytearray;
begin
  src.PixelFormat := pf24bit;
  w := src.width;
  h := src.height;
  for y := 0 to h - 1 do
  begin
    p0 := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      p0[x * 3 + 2] := round(factor * p0[x * 3 + 2]);
      p0[x * 3 + 1] := 0;
      p0[x * 3] := 0;
    end;
  end;
end;

procedure TJvPaintFX.Shake(src, dst: Tbitmap; factor: extended);
var
  x, y, h, w, dx: integer;
  p: pbytearray;
begin
  dst.canvas.draw(0, 0, src);
  dst.pixelformat := pf24bit;
  w := dst.Width;
  h := dst.height;
  dx := round(factor * w);
  if dx = 0 then exit;
  if dx > (w div 2) then exit;

  for y := 0 to h - 1 do
  begin
    p := dst.scanline[y];
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

procedure TJvPaintFX.ShakeDown(src, dst: Tbitmap; factor: extended);
var
  x, y, h, w, dy: integer;
  p, p2, p3: pbytearray;
begin
  dst.canvas.draw(0, 0, src);
  dst.pixelformat := pf24bit;
  w := dst.Width;
  h := dst.height;
  dy := round(factor * h);
  if dy = 0 then exit;
  if dy > (h div 2) then exit;

  for y := dy to h - 1 do
  begin
    p := dst.scanline[y];
    p2 := dst.scanline[y - dy];
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
    p := dst.scanline[y];
    p3 := dst.scanline[y + dy];
    for x := 0 to w - 1 do
      if (x mod 2) <> 0 then
      begin
        p3[x * 3] := p[x * 3];
        p3[x * 3 + 1] := p[x * 3 + 1];
        p3[x * 3 + 2] := p[x * 3 + 2];
      end;
  end;
end;

procedure TJvPaintFX.Plasma(src1, src2, dst: Tbitmap; scale, turbulence: extended);
var
  cval, sval: array[0..255] of integer;
  i, x, y, w, h, xx, yy: integer;
  Asin, Acos: extended;
  ps1, ps2, pd: pbytearray;
begin
  w := src1.width;
  h := src1.height;
  if turbulence < 10 then turbulence := 10;
  if scale < 5 then scale := 5;
  for i := 0 to 255 do
  begin
    sincos(i / turbulence, Asin, Acos);
    sval[i] := round(-scale * Asin);
    cval[i] := round(scale * Acos);
  end;
  for y := 0 to h - 1 do
  begin
    pd := dst.scanline[y];
    ps2 := src2.scanline[y];
    for x := 0 to w - 1 do
    begin
      xx := x + sval[ps2[x * 3]];
      yy := y + cval[ps2[x * 3]];
      if (xx >= 0) and (xx < w) and (yy >= 0) and (yy < h) then
      begin
        ps1 := src1.scanline[yy];
        pd[x * 3] := ps1[xx * 3];
        pd[x * 3 + 1] := ps1[xx * 3 + 1];
        pd[x * 3 + 2] := ps1[xx * 3 + 2];
      end;
    end;
  end;
  ;
end;

procedure TJvPaintFX.splitround(src, dst: tbitmap; amount: integer; style: TLightBrush);
var
  x, y, w, c, c00, dx, cx: integer;
  R, R00: trect;
  bm, bm2: tbitmap;
  p0, p00, p1: pbytearray;
begin
  if amount = 0 then
  begin
    dst.canvas.Draw(0, 0, src);
    exit;
  end;
  cx := src.width div 2;
  if amount > cx then
    amount := cx;
  w := src.width;
  bm := tbitmap.create;
  bm.PixelFormat := pf24bit;
  bm.height := 1;
  bm.width := cx;
  bm2 := tbitmap.create;
  bm2.PixelFormat := pf24bit;
  bm2.height := 1;
  bm2.width := cx;
  p0 := bm.scanline[0];
  p00 := bm2.scanline[0];
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to cx - 1 do
    begin
      c := x * 3;
      c00 := (cx + x) * 3;
      p0[c] := p1[c];
      p0[c + 1] := p1[c + 1];
      p0[c + 2] := p1[c + 2];
      p00[c] := p1[c00];
      p00[c + 1] := p1[c00 + 1];
      p00[c + 2] := p1[c00 + 2];
    end;
    case style of
      mbsplitround: dx := round(amount * abs(sin(y / (src.height - 1) * pi)));
      mbsplitwaste: dx := round(amount * abs(cos(y / (src.height - 1) * pi)));
    end;
    R := rect(0, y, dx, y + 1);
    dst.Canvas.StretchDraw(R, bm);
    R00 := rect(w - 1 - dx, y, w - 1, y + 1);
    dst.Canvas.StretchDraw(R00, bm2);
  end;
  bm.free;
  bm2.free;
end;

procedure TJvPaintFX.Emboss(var Bmp: TBitmap);
var
  x, y: Integer;
  p1, p2: Pbytearray;
begin
  for y := 0 to Bmp.Height - 2 do
  begin
    p1 := bmp.scanline[y];
    p2 := bmp.scanline[y + 1];
    for x := 0 to Bmp.Width - 4 do
    begin
      p1[x * 3] := (p1[x * 3] + (p2[(x + 3) * 3] xor $FF)) shr 1;
      p1[x * 3 + 1] := (p1[x * 3 + 1] + (p2[(x + 3) * 3 + 1] xor $FF)) shr 1;
      p1[x * 3 + 2] := (p1[x * 3 + 2] + (p2[(x + 3) * 3 + 2] xor $FF)) shr 1;
    end;
  end;

end;

procedure TJvPaintFX.filterred(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c + 2] > min) and (p1[c + 2] < max) then
        p1[c + 2] := $FF
      else
        p1[c + 2] := 0;
      p1[c] := 0;
      p1[c + 1] := 0;
    end;
  end;
end;

procedure TJvPaintFX.filtergreen(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c + 1] > min) and (p1[c + 1] < max) then
        p1[c + 1] := $FF
      else
        p1[c + 1] := 0;
      p1[c] := 0;
      p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.filterblue(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c] > min) and (p1[c] < max) then
        p1[c] := $FF
      else
        p1[c] := 0;
      p1[c + 1] := 0;
      p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.filterxred(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c + 2] > min) and (p1[c + 2] < max) then
        p1[c + 2] := $FF
      else
        p1[c + 2] := 0;
    end;
  end;
end;

procedure TJvPaintFX.filterxgreen(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c + 1] > min) and (p1[c + 1] < max) then
        p1[c + 1] := $FF
      else
        p1[c + 1] := 0;
    end;
  end;
end;

procedure TJvPaintFX.filterxblue(src: tbitmap; min, max: integer);
var
  c, x, y: integer;
  p1: pbytearray;

begin
  for y := 0 to src.height - 1 do
  begin
    p1 := src.scanline[y];
    for x := 0 to src.width - 1 do
    begin
      c := x * 3;
      if (p1[c] > min) and (p1[c] < max) then
        p1[c] := $FF
      else
        p1[c] := 0;
    end;
  end;
end;

//Just a small function to map the numbers to colors

function TJvPaintFX.ConvertColor(Value: Integer): TColor;
begin
  case Value of
    0: Result := clBlack;
    1: Result := clNavy;
    2: Result := clGreen;
    3: Result := clAqua;
    4: Result := clRed;
    5: Result := clPurple;
    6: Result := clMaroon;
    7: Result := clSilver;
    8: Result := clGray;
    9: Result := clBlue;
    10: Result := clLime;
    11: Result := clOlive;
    12: Result := clFuchsia;
    13: Result := clTeal;
    14: Result := clYellow;
    15: Result := clWhite;
  else
    Result := clWhite;
  end;
end;

procedure TJvPaintFX.DrawMandelJulia(src: Tbitmap; x0, y0, x1, y1: extended; Niter: integer; Mandel: Boolean);
const
  //Number if colors. If this is changed, the number of mapped colors must also be changed
  nc = 16;
type
  TJvRGBTriplet = record
    r, g, b: byte
  end;
var
  X, XX, Y, YY, Cx, Cy, Dx, Dy, XSquared, YSquared: Double;
  Nx, Ny, Py, Px, I: Integer;
  p0: pbytearray;
  cc: array[0..15] of TJvRGBTriplet;
  Acolor: Tcolor;
begin
  src.PixelFormat := pf24bit;
  for i := 0 to 15 do
  begin
    Acolor := convertcolor(i);
    cc[i].b := GetBValue(colortoRGB(Acolor));
    cc[i].g := GetGValue(colortoRGB(Acolor));
    cc[i].r := GetRValue(colortoRGB(Acolor));
  end;
  if Niter < nc then Niter := nc;
  try
    Nx := src.Width;
    Ny := src.Height;
    Cx := 0;
    Cy := 1;
    Dx := (x1 - x0) / nx;
    Dy := (y1 - y0) / ny;
    Py := 0;
    while (PY < Ny) do
    begin
      p0 := src.scanline[py];
      PX := 0;
      while (Px < Nx) do
      begin
        x := x0 + px * dx;
        y := y0 + py * dy;
        if (mandel) then
        begin
          cx := x;
          cy := y;
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
          xx := xsquared - ysquared + cx;
          yy := (2 * x * y) + cy;
          x := xx;
          y := yy;
          I := I + 1;
        end;
        I := I - 1;
        if (i = niter) then
          i := 0
        else
          i := round(i / (niter / nc));
        //        Canvas.Pixels[PX,PY] := ConvertColor(I);
        p0[px * 3] := cc[i].b;
        p0[px * 3 + 1] := cc[i].g;
        p0[px * 3 + 2] := cc[i].r;
        Px := Px + 1;
      end;
      Py := Py + 1;
    end;
  finally
  end;
end;

procedure TJvPaintFX.Invert(src: tbitmap);
var
  w, h, x, y: integer;
  p: pbytearray;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := src.scanline[y];
    for x := 0 to w - 1 do
    begin
      p[x * 3] := not p[x * 3];
      p[x * 3 + 1] := not p[x * 3 + 1];
      p[x * 3 + 2] := not p[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.MirrorRight(src: Tbitmap);
var
  w, h, x, y: integer;
  p: pbytearray;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := src.scanline[y];
    for x := 0 to w div 2 do
    begin
      p[(w - 1 - x) * 3] := p[x * 3];
      p[(w - 1 - x) * 3 + 1] := p[x * 3 + 1];
      p[(w - 1 - x) * 3 + 2] := p[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.MirrorDown(src: Tbitmap);
var
  w, h, x, y: integer;
  p1, p2: pbytearray;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  for y := 0 to h div 2 do
  begin
    p1 := src.scanline[y];
    p2 := src.scanline[h - 1 - y];
    for x := 0 to w - 1 do
    begin
      p2[x * 3] := p1[x * 3];
      p2[x * 3 + 1] := p1[x * 3 + 1];
      p2[x * 3 + 2] := p1[x * 3 + 2];
    end;
  end;
end;

// resample image as triangles

procedure TJvPaintFX.Triangles(src: TBitmap; amount: integer);
type
  Ttriplet = record
    r, g, b: byte;
  end;

var
  w, h, x, y, tb, tm, te: integer;
  ps: pbytearray;
  T: ttriplet;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  if amount < 5 then amount := 5;
  amount := (amount div 2) * 2 + 1;
  tm := amount div 2;
  for y := 0 to h - 1 do
  begin
    ps := src.scanline[y];
    t.r := ps[0];
    t.g := ps[1];
    t.b := ps[2];
    tb := y mod (amount - 1);
    if tb > tm then tb := 2 * tm - tb;
    if tb = 0 then tb := amount;
    te := tm + abs(tm - (y mod amount));
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

procedure TJvPaintFX.RippleTooth(src: TBitmap; amount: integer);
var
  x, y: integer;
  P1, P2: PByteArray;
  b: byte;

begin
  src.PixelFormat := pf24bit;
  amount := min(src.height div 2, amount);
  for y := src.height - 1 - amount downto 0 do
  begin
    P1 := src.ScanLine[y];
    b := 0;
    for x := 0 to src.width - 1 do
    begin
      P2 := src.scanline[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      inc(b);
      if b > amount then b := 0;
    end;
  end;
end;

procedure TJvPaintFX.RippleTriangle(src: TBitmap; amount: integer);
var
  c, x, y: integer;
  P1, P2: PByteArray;
  b: byte;
  doinc: boolean;

begin
  amount := min(src.height div 2, amount);
  for y := src.height - 1 - amount downto 0 do
  begin
    P1 := src.ScanLine[y];
    b := 0;
    doinc := true;
    for x := 0 to src.width - 1 do
    begin
      P2 := src.scanline[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      if doinc then
      begin
        inc(b);
        if b > amount then
        begin
          doinc := false;
          b := amount - 1;
        end;
      end
      else
      begin
        if b = 0 then
        begin
          doinc := true;
          b := 2;
        end;
        dec(b);
      end;
    end;
  end;
end;

procedure TJvPaintFX.RippleRandom(src: TBitmap; amount: integer);
var
  c, c2, x, y: integer;
  P1, P2: PByteArray;
  b: byte;

begin
  amount := min(src.height div 2, amount);
  src.PixelFormat := pf24bit;
  randomize;
  for y := src.height - 1 - amount downto 0 do
  begin
    P1 := src.ScanLine[y];
    b := 0;
    for x := 0 to src.width - 1 do
    begin
      P2 := src.scanline[y + b];
      P2[x * 3] := P1[x * 3];
      P2[x * 3 + 1] := P1[x * 3 + 1];
      P2[x * 3 + 2] := P1[x * 3 + 2];
      b := random(amount);
    end;
  end;
end;

procedure TJvPaintFX.TexturizeOverlap(src: TBitmap; amount: integer);
var
  w, h, x, y, xo: integer;
  bm: tbitmap;
  arect: trect;
begin
  bm := tbitmap.create;
  amount := min(src.width div 2, amount);
  amount := min(src.height div 2, amount);
  xo := round(amount * 2 / 3);
  bm.width := amount;
  bm.height := amount;
  w := src.width;
  h := src.height;
  arect := rect(0, 0, amount, amount);
  bm.Canvas.StretchDraw(arect, src);
  y := 0;
  repeat
    x := 0;
    repeat
      src.canvas.Draw(x, y, bm);
      x := x + xo;
    until x >= w;
    y := y + xo;
  until y >= h;
  bm.free;
end;

procedure TJvPaintFX.TexturizeTile(src: TBitmap; amount: integer);
var
  w, h, x, y: integer;
  bm: tbitmap;
  arect: trect;
begin
  bm := tbitmap.create;
  amount := min(src.width div 2, amount);
  amount := min(src.height div 2, amount);
  bm.width := amount;
  bm.height := amount;
  w := src.width;
  h := src.height;
  arect := rect(0, 0, amount, amount);
  bm.Canvas.StretchDraw(arect, src);
  y := 0;
  repeat
    x := 0;
    repeat
      src.canvas.Draw(x, y, bm);
      x := x + bm.width;
    until x >= w;
    y := y + bm.height;
  until y >= h;
  bm.free;
end;

procedure TJvPaintFX.HeightMap(src: Tbitmap; amount: integer);
var
  bm: tbitmap;
  w, h, x, y: integer;
  pb, ps: pbytearray;
  c: integer;
begin
  h := src.height;
  w := src.width;
  bm := tbitmap.create;
  bm.width := w;
  bm.height := h;
  bm.PixelFormat := pf24bit;
  src.PixelFormat := pf24bit;
  bm.Canvas.Draw(0, 0, src);
  for y := 0 to h - 1 do
  begin
    pb := bm.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      c := round((pb[x * 3] + pb[x * 3 + 1] + pb[x * 3 + 2]) / 3 / 255 * amount);
      if (y - c) >= 0 then
      begin
        ps := src.ScanLine[y - c];
        ps[x * 3] := pb[x * 3];
        ps[x * 3 + 1] := pb[x * 3 + 1];
        ps[x * 3 + 2] := pb[x * 3 + 2];
      end;
    end;
  end;
  bm.free;
end;

procedure TJvPaintFX.turn(src, dst: tbitmap);
var
  w, h, x, y: integer;
  ps, pd: pbytearray;
begin
  h := src.Height;
  w := src.width;
  src.PixelFormat := pf24bit;
  dst.PixelFormat := pf24bit;
  dst.Height := w;
  dst.Width := h;
  for y := 0 to h - 1 do
  begin
    ps := src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd := dst.ScanLine[w - 1 - x];
      pd[y * 3] := ps[x * 3];
      pd[y * 3 + 1] := ps[x * 3 + 1];
      pd[y * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.turnRight(src, dst: Tbitmap);
var
  w, h, x, y: integer;
  ps, pd: pbytearray;
begin
  h := src.Height;
  w := src.width;
  src.PixelFormat := pf24bit;
  dst.PixelFormat := pf24bit;
  dst.Height := w;
  dst.Width := h;
  for y := 0 to h - 1 do
  begin
    ps := src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd := dst.ScanLine[x];
      pd[(h - 1 - y) * 3] := ps[x * 3];
      pd[(h - 1 - y) * 3 + 1] := ps[x * 3 + 1];
      pd[(h - 1 - y) * 3 + 2] := ps[x * 3 + 2];
    end;
  end;
end;

procedure TJvPaintFX.ExtractColor(src: TBitmap; Acolor: tcolor);
var
  w, h, x, y: integer;
  p: pbytearray;
  Ecolor: TColor;
  r, g, b: byte;
begin
  w := src.width;
  h := src.height;
  Ecolor := colortorgb(Acolor);
  r := getRValue(Ecolor);
  g := getGValue(Ecolor);
  b := getBValue(Ecolor);
  src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := src.ScanLine[y];
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
  src.transparent := true;
  src.TransparentColor := clblack;
end;

procedure TJvPaintFX.ExcludeColor(src: TBitmap; Acolor: tcolor);
var
  w, h, x, y: integer;
  p: pbytearray;
  Ecolor: TColor;
  r, g, b: byte;
begin
  w := src.width;
  h := src.height;
  Ecolor := colortorgb(Acolor);
  r := getRValue(Ecolor);
  g := getGValue(Ecolor);
  b := getBValue(Ecolor);
  src.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p := src.ScanLine[y];
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
  src.transparent := true;
  src.TransparentColor := clblack;
end;

procedure TJvPaintFX.Blend(src1, src2, dst: tbitmap; amount: extended);
var
  w, h, x, y: integer;
  ps1, ps2, pd: pbytearray;
begin
  w := src1.Width;
  h := src1.Height;
  dst.Width := w;
  dst.Height := h;
  src1.PixelFormat := pf24bit;
  src2.PixelFormat := pf24bit;
  dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps1 := src1.ScanLine[y];
    ps2 := src2.ScanLine[y];
    pd := dst.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := round((1 - amount) * ps1[x * 3] + amount * ps2[x * 3]);
      pd[x * 3 + 1] := round((1 - amount) * ps1[x * 3 + 1] + amount * ps2[x * 3 + 1]);
      pd[x * 3 + 2] := round((1 - amount) * ps1[x * 3 + 2] + amount * ps2[x * 3 + 2]);
    end;
  end;
end;

procedure TJvPaintFX.Solorize(src, dst: tbitmap; amount: integer);
var
  w, h, x, y: integer;
  ps, pd: pbytearray;
  c: integer;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps := src.scanline[y];
    pd := dst.scanline[y];
    for x := 0 to w - 1 do
    begin
      c := (ps[x * 3] + ps[x * 3 + 1] + ps[x * 3 + 2]) div 3;
      if c > amount then
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

procedure TJvPaintFX.Posterize(src, dst: tbitmap; amount: integer);
var
  w, h, x, y: integer;
  ps, pd: pbytearray;
  c: integer;
begin
  w := src.width;
  h := src.height;
  src.PixelFormat := pf24bit;
  dst.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    ps := src.scanline[y];
    pd := dst.scanline[y];
    for x := 0 to w - 1 do
    begin
      pd[x * 3] := round(ps[x * 3] / amount) * amount;
      pd[x * 3 + 1] := round(ps[x * 3 + 1] / amount) * amount;
      pd[x * 3 + 2] := round(ps[x * 3 + 2] / amount) * amount;
    end;
  end;
end;

end.
