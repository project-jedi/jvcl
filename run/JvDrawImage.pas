{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDrawImage.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvDrawImage;

interface

uses
  Windows,
  {$IFDEF VCL}
  Messages,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QForms,
  {$ENDIF VisualCLX}
  Classes, Graphics, Controls, ExtCtrls,
  JvAirBrush, JvPaintFX;

type
  TSmartResizeMode = (rmWidth, rmHeight, rmSquare);
  TMorphBrush = (mbVerBox, mbHorBox, mbVerOval, mbHorOval);
  TDigitalFilter = array [0..4, 0..4] of Smallint;
  TColorPicked = procedure(Sender: TObject; AColor: TColor) of object;

  TJvDrawImage = class(TImage)
  private
    FGonio: array [0..180, 0..1] of Extended;
    FSinPixs: array [0..255] of Byte;
    FShape: string;
    FShapes: TStringList;
    FZoomClip: TBitmap;
    FAirBrush: TJvAirBrush;
    FPolygonChecked: Boolean;
    FOnColorPicked: TColorPicked;
    FBlocks: Integer;
    FStars: Integer;
    FStarPoints: Integer;
    FSpirals: Integer;
    function GetShapes: TStrings;
    procedure EscapePaint(X, Y: Integer; Shift: TShiftState);
    procedure CopyClip;
    procedure SetClip(AColor: TColor);
    procedure InitPlasma;
    function MixColors(Color1, Color2: TColor): TColor;
    function GetBlue(AColor: TColor): Byte;
    function GetGreen(AColor: TColor): Byte;
    function GetRed(AColor: TColor): Byte;
    procedure SetSyms(X, Y: Integer);
    function Rotate(Origin, Endpoint: TPoint; Angle: Real): TPoint;
    procedure DrawPlasma(X, Y: Integer; Amount: Extended);
    procedure DrawEffectBrush(X, Y, Radius: Integer; Amount: Extended;
      Style: TLightBrush);
    procedure Rimple(Src, Dst: TBitmap; Amount: Extended);
    procedure DrawStretchBrush(X, Y, Radius: Integer; Amount: Extended;
      Style: TMorphBrush);
    procedure SampleStretch(Src, Dst: TBitmap);
    procedure DrawLightBrush(X, Y, Radius, Amount: Integer;
      Style: TLightBrush);
    procedure DrawColorCircle(X, Y, Mode: Integer);
    procedure ColorCircle(var bm: TBitmap; center: TPoint; Radius,
      Mode: Integer);
    procedure DrawDarkerCircle(X, Y, Mode: Integer);
    procedure DrawLighterCircle(X, Y, Mode: Integer);
    procedure DrawGradientBrush(Color1, Color2: TColor; X1, X2,
      Y: Integer);
    procedure HorGradientLine(Bitmap: TBitmap; XOrigin, XFinal, Y: Integer;
      R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
    procedure SmoothPnt(Bitmap: TBitmap; xk, yk: Integer);
    procedure DrawVGradientBrush(Color1, Color2: TColor; Y1, Y2,
      X: Integer);
    procedure VerGradientLine(Bitmap: TBitmap; YOrigin, YFinal, X: Integer;
      R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
    procedure DrawCube;
    function PointToBlock(X, Y: Integer): TRect;
    procedure DrawSkew;
    procedure DrawTriangle;
    procedure PutClip(M: TRect);
    procedure DrawSyms(X, Y: Integer);
    procedure DrawTexLines(X0, Y0, X, Y: Integer);
    function BlendColors(const Color1, Color2: Integer;
      Opacity: Integer): Longint;
    function TexHighlight(Colr: Integer): Longint;
    function TexShadow(Colr: Integer): Longint;
    procedure DrawTexOvals(X0, Y0, X, Y: Integer);
    procedure DrawBlurOvals(X0, Y0, X, Y: Integer);
    procedure DrawTexCurves(X0, Y0, X, Y: Integer);
    procedure DrawBlurCurves(X0, Y0, X, Y: Integer);
    procedure DrawTexPoly(X0, Y0, X, Y: Integer);
    procedure DrawBlurPoly(X0, Y0, X, Y: Integer);
    procedure DrawTexRects(X0, Y0, X, Y: Integer);
    procedure DrawBlurRects(X0, Y0, X, Y: Integer);
    procedure DrawBlurLines(X0, Y0, X, Y: Integer);
    procedure InterpRect(X1, Y1, X2, Y2: Integer);
    procedure InterpolateRect(Bmp: TBitmap; X1, Y1, X2, Y2: Integer);
    procedure DrawColumn(X1, Y1, X2, Y2: Integer);
    procedure Column(Bitmap: TBitmap; XOrigin, XFinal, YOrigin,
      YFinal: Integer; R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
    procedure DrawSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
    procedure Sphere(Bitmap: TBitmap; xcenter, a, ycenter, b: Integer; R1,
      G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
    procedure DrawMultiSphere(Color1, Color2: TColor; X1, Y1, X2,
      Y2: Integer);
    procedure DrawDropletSphere(Color1, Color2: TColor; X1, Y1, X2,
      Y2: Integer);
    procedure DrawWaveSphere(Color1, Color2: TColor; X1, Y1, X2,
      Y2: Integer);
    procedure DrawRisingWaveSphere(Color1, Color2: TColor; X1, Y1, X2,
      Y2: Integer);
//    function GetAngle(Origin, Endpoint: TPoint): Integer;
//    procedure TextRotate(X, Y, Angle: Integer; AText: string; AFont: TFont);
    function ReduceVector(Origin, Endpoint: TPoint; Factor: Real): TPoint;
    procedure Star(X, Y: Integer);
    procedure SetPolygonChecked(const Value: Boolean);
    procedure DrawSpiro(center, Radius: TPoint);
    procedure DrawBars(X1, Y1, X2, Y2: Integer);
    procedure Drawborders(X1, Y1, X2, Y2: Integer);
    procedure SetonColorPicked(const Value: TColorPicked);
    procedure SetShape(const Value: string);
    procedure SetAirBrush(const Value: TJvAirBrush);
    procedure SetTransformer(const Value: TJvPaintFX);
    procedure BuildShapeList;
    procedure SetBlocks(const Value: Integer);
    procedure SetSpirals(const Value: Integer);
    procedure SetStarPoints(const Value: Integer);
    procedure SetStars(const Value: Integer);
    procedure FillGonio;
    procedure FillSinPixs;
    procedure Shear(ABitmap: TBitmap; Amount: Integer);
    procedure XFormA(Amount: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ColorPicked(AColor: TColor);
    procedure Loaded; override;
  public
    Clip: TBitmap;
    TraceB: Byte;
    FX: TJvPaintFX;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClipAll;
    procedure Effects;
    procedure Backgrounds;
    procedure Preview(ABitmap: TBitmap);
    procedure ApplyFilter(var Dst: TBitmap; DF: TDigitalFilter);
    procedure BlurBarChange(Sender: TObject);
    procedure ColorNoiseBarChange(Sender: TObject);
    procedure ContrastBarChange(Sender: TObject);
    procedure DrawBlend;
    procedure DrawMandelJulia(Mandel: Boolean);
    procedure DrawMap;
    procedure DrawSolarize;
    procedure DrawTriangles;
    procedure EmbossBarChange;
    procedure FilterBlueBarChange;
    procedure FilterGreenBarChange;
    procedure FilterRedBarChange;
    procedure FilterXBlueBarChange;
    procedure FilterXGreenBarChange;
    procedure FilterXRedBarChange;
    procedure FisheyeBarChange;
    procedure LightnessBarChange(Sender: TObject);
    procedure Marble2BarChange;
    procedure Marble3BarChange;
    procedure Marble4BarChange;
    procedure Marble5BarChange;
    procedure Marble6BarChange;
    procedure Marble7BarChange;
    procedure Marble8BarChange;
    procedure MarbleBarChange;
    procedure MonoNoiseBarChange(Sender: TObject);
    procedure MosaicBarChange;
    procedure PlasmaBarChange;
    procedure Posterize;
    procedure RippleRandom;
    procedure RippleTooth;
    procedure RippleTriangle;
    procedure RotateBar;
    procedure SaturationBarChange(Sender: TObject);
    procedure SeamBarChange;
    procedure ShearBarChange;
    procedure SmoothBarChange(Sender: TObject);
    procedure SplitBlurBarChange(Sender: TObject);
    procedure SplitRoundBarChange;
    procedure SplitWasteBarChange;
    procedure SqueezeBotBarChange;
    procedure SqueezeDiamondBarChange;
    procedure SqueezeHorBarChange;
    procedure SqueezeRound2BarChange;
    procedure SqueezeRoundBarChange;
    procedure SqueezeTopBarChange;
    procedure SqueezeWasteBarChange;
    procedure TexturizeOverlap;
    procedure TexturizeTile;
    procedure TwistBarChange;
    procedure WaveBarChange;
    procedure WaveExtraChange;
    procedure WaveInfChange;
    procedure XFormABarChange;
    procedure Trace;
    property AirBrush: TJvAirBrush read FAirBrush write SetAirBrush;
    property Transformer: TJvPaintFX read FX write SetTransformer;
    property Shapes: TStrings read GetShapes;
  published
    property Shape: string read FShape write SetShape;
    property PolygonChecked: Boolean read FPolygonChecked write SetPolygonChecked;
    property Stars: Integer read FStars write SetStars;
    property StarPoints: Integer read FStarPoints write SetStarPoints;
    property Blocks: Integer read FBlocks write SetBlocks;
    property Spirals: Integer read FSpirals write SetSpirals;
    property OnColorPicked: TColorPicked read FOnColorPicked write SetOnColorPicked;
  end;

implementation

uses
  SysUtils, Math, Dialogs, Clipbrd,
  JvResample, JvPainterEffectsForm, JvQuickPreviewForm, JvPainterQBForm,
  JvTypes, JvResources;

const
  // Texture constants
  DarkStrength = 0.82;
  StrongBlend = 52;
  WeakBlend = 36;

  BlurFilter: TDigitalFilter =
   ((1, 1, 1, 1, 1),
    (1, 0, 0, 0, 1),
    (1, 0, 0, 0, 1),
    (1, 0, 0, 0, 1),
    (1, 1, 1, 1, 1));

type
  TFColor = record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

var
  PainterEffectsF: TPainterEffectsForm;
  QuickPreviewF: TQuickPreviewForm;
  PainterQBF: TPainterQBForm;

  mycliprect: TRect;
  UserFilter: TDigitalFilter;

  RangeTransColor: TColor;

  NSpiro: Integer;
  Wavepen, Wavebrush: TColor;
  decoX, decoY: Integer;

  mybezier: array [0..3] of TPoint;
  myskew: array [0..4] of TPoint;
  mychord: array [1..8] of Integer;
  myorigin, myprevpoint: TPoint;
  myslinedir: string;
  myslinetol: Integer;
  myDraw: Boolean;
  mypen: TPenMode;
  mypenstyle: TPenStyle;
  myoldbrushstyle: TBrushStyle;
  myoldpenwidth: Integer;
  myround: Integer;

  clipcm: TCopyMode;

  pointarray: array [0..12] of TPoint;
  spiralfactor: Real;
  spiraldir: Integer;
  TargetPoint: TPoint;
  zoomrect: TRect;
  freepoly: array [0..100] of TPoint;
  freepolycount: Integer;
  bezierfix1, bezierfix2: Boolean;

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

constructor TJvDrawImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 256;
  Height := 256;
  Clip := TBitmap.Create;
  FZoomClip := TBitmap.Create;
  FAirBrush := TJvAirBrush.Create(Self);
  FX := TJvPaintFX.Create(Self);
  TargetPoint := Point(0, 0);
  NSpiro := 40;
  RangeTransColor := clWhite;
  zoomrect := Rect(0, 0, 50, 50);
  mycliprect := Rect(0, 0, 256, 256);
  //spiral number, direction and Factor
  Spirals := 3;
  spiralfactor := 0.99;
  spiraldir := 1;
  // number of points for Star shape
  StarPoints := 5;
  Stars := 1;
  // tolerance for straight line Drawing
  myslinetol := 5;

  mypenstyle := psSolid;
  // number of Blocks wide and heigh
  Blocks := 32;
  // rounding of roundrect
  myround := 10;
  // default Drawing Mode
  Shape := 'line';
  FillSinPixs;
  FillGonio;
  TraceB := $00;
  FShapes := TStringList.Create;
  BuildShapeList;
  PainterEffectsF := TPainterEffectsForm.Create(Self);
  PainterEffectsF.setDrawImage(Self);
  QuickPreviewF := TQuickPreviewForm.Create(Self);
  QuickPreviewF.SetDrawImage(Self);
  PainterQBF := TPainterQBForm.Create(Self);
  PainterQBF.setDrawImage(Self);
end;

destructor TJvDrawImage.Destroy;
begin
  FShapes.Free;
  Clip.Free;
  FZoomClip.Free;
  FAirBrush.Free;
  FX.Free;
  PainterEffectsF.Free;
  QuickPreviewF.Free;
  PainterQBF.Free;
  inherited Destroy;
end;

function TJvDrawImage.GetShapes: TStrings;
begin
  Result := FShapes;
end;

// Start of filter procedures

procedure TJvDrawImage.FillGonio;
var
  A0: Extended;
  I: Integer;
begin
  A0 := Pi / 180;
  for I := 0 to 180 do
    SinCos(A0 * (I - 90), FGonio[I, 0], FGonio[I, 1]);
end;

procedure TJvDrawImage.FillSinPixs;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FSinPixs[I] := Variant(Sin(I / 255 * Pi / 2) * 255);
end;

procedure TJvDrawImage.Shear(ABitmap: TBitmap; Amount: Integer);
var
  bm: TBitmap;
  p1, p2: PByteArray;
  X, dx, Y, h, w, c1, c2: Integer;
  f: Extended;
begin
  bm := TBitmap.Create;
  h := ABitmap.Height;
  w := ABitmap.Width;
  bm.Width := w;
  bm.Height := h;
  f := w / (w + (Amount / 100) * h);
  bm.PixelFormat := pf24bit;
  ABitmap.PixelFormat := pf24bit;
  for Y := 0 to h - 1 do
  begin
    p1 := ABitmap.ScanLine[Y];
    p2 := bm.ScanLine[Y];
    dx := Round(Amount / 100 * Y);
    for X := 0 to w - 1 do
    begin
      c1 := X * 3;
      c2 := Round(f * (X + dx)) * 3;
      p2[c2] := p1[c1];
      p2[c2 + 1] := p1[c1 + 1];
      p2[c2 + 2] := p1[c1 + 2];
    end;
  end;
  ABitmap.Assign(bm);
  bm.Free;
end;

procedure TJvDrawImage.XFormA(Amount: Integer);
var
  X, Y, i: Integer;
  p1: pbytearray;
begin
  for i := 1 to Amount do
    for Y := 0 to Clip.Height - 1 do
    begin
      p1 := Clip.ScanLine[Y];
      for X := 0 to Clip.Width - 1 do
      begin
        p1[X * 3] := FSinPixs[p1[X * 3]];
        p1[X * 3 + 1] := FSinPixs[p1[X * 3 + 1]];
        p1[X * 3 + 2] := FSinPixs[p1[X * 3 + 2]];
      end;
    end;
end;

procedure TJvDrawImage.Drawborders(X1, Y1, X2, Y2: Integer);
var
  h, w: Integer;
begin
  h := clientheight;
  w := clientwidth;
  Canvas.FillRect(Rect(0, 0, w, Y2 - Y1));
  Canvas.FillRect(Rect(0, h - (Y2 - Y1), w, h));
  Canvas.FillRect(Rect(0, 0, X2 - X1, h));
  Canvas.FillRect(Rect(w - (X2 - X1), 0, w, h));
end;

procedure TJvDrawImage.DrawBars(X1, Y1, X2, Y2: Integer);
var
  h, w: Integer;
begin
  h := clientheight;
  w := clientwidth;
  if Y1 < 10 then
    Y1 := 0;
  if Y2 > (h - 10) then
    Y2 := h;
  X1 := 0;
  X2 := w;
  Canvas.FillRect(Rect(X1, Y1, X2, Y2));
end;

procedure TJvDrawImage.DrawSpiro(center, Radius: TPoint);
var
  X0, X1, Y0, Y1, a0, a1, da0, da1: Real;
  xs, ys, X, Y, r0, R1: Integer;
  i: Integer;
begin
  xs := Picture.Bitmap.Width div 2;
  ys := Picture.Bitmap.Height div 2;
  if xs <> ys then
  begin
    ShowMessage(RsImageMustBeSquare);
    Exit;
  end;
  r0 := Variant(Sqrt(Sqr(center.X - xs) + Sqr(center.Y - ys)));
  R1 := Variant(Sqrt(Sqr(Radius.X - center.X) + Sqr(Radius.Y - center.Y)));
  if (r0 + R1) > xs then
  begin
    ShowMessage(RsSumOfRadiTolarge);
    Exit;
  end;
  if (r0 < 5) or (R1 < 5) then
  begin
    ShowMessage(Format(RsBothRadiMustBeGr, [5]));
    Exit;
  end;
  da1 := 2 * pi / 36;
  da0 := R1 / r0 * da1;
  a0 := 0;
  a1 := 0;
  Canvas.MoveTo(xs + r0 + R1, ys);
  for i := 1 to 36 * NSpiro do
  begin
    X1 := R1 * Cos(a1);
    Y1 := R1 * Sin(a1);
    a1 := a1 + da1;
    X0 := r0 * Cos(a0);
    Y0 := r0 * Sin(a0);
    a0 := a0 + da0;
    X := Variant(xs + X0 + X1);
    Y := Variant(ys + Y0 + Y1);
    Canvas.LineTo(X, Y)
  end;
end;

procedure TJvDrawImage.Star(X, Y: Integer);
var
  i, X0, Y0, damult: Integer;
  apoint: TPoint;
  da: Real;
begin
  X0 := myorigin.X;
  Y0 := myorigin.Y;
//777  d := Abs(Y - Y0);
  damult := 1;
  if not PolygonChecked then
  begin
    case StarPoints of
      5: damult := 2;
      7: damult := 3;
      9: damult := 4;
      11: damult := 5;
    end;
  end;
  da := damult * 2 * pi / StarPoints;
  with Canvas do
  begin
    pointarray[0] := Point(X, Y);
    //   MoveTo(X,Y);
    apoint := Point(X, Y);
    for i := 1 to StarPoints - 1 do
    begin
      //      apoint:=Rotate(Point(X0,Y0),apoint,da);
      //      LineTo(apoint.X,apoint.Y);
      apoint := Rotate(Point(X0, Y0), apoint, da);
      pointarray[i] := apoint;
    end;
    //      LineTo(X,Y);
    Polygon(Slice(PointArray, StarPoints))
  end;
end;

function TJvDrawImage.ReduceVector(Origin, Endpoint: TPoint;
  Factor: Real): TPoint;
var
  a, d, r: Real;
begin
  r := Sqrt(Sqr(Endpoint.X - Origin.X) + Sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := ArcTan2((Endpoint.Y - Origin.Y), d);
  r := r * Factor;
  Result.X := Origin.X + Variant(r * Cos(a));
  Result.Y := Origin.Y + Variant(r * Sin(a));
end;
(*)
procedure TJvDrawImage.TextRotate(X, Y, Angle: Integer; aText: string;
  afont: tfont);
var
  dc: hdc;
  fnt: LogFont;
  hfnt, hfntPrev: hfont;
  i: Integer;
  fname, s: string;
begin
  s := aText;
  fnt.lfEscapement := Angle * 10;
  fnt.lfOrientation := Angle * 10;
  if fsbold in afont.Style then
    fnt.lfWeight := FW_Bold
  else
    fnt.lfWeight := FW_NORMAL;
  if fsitalic in afont.Style then
    fnt.lfItalic := 1
  else
    fnt.lfItalic := 0;
  if fsunderline in afont.Style then
    fnt.lfUnderline := 1
  else
    fnt.lfUnderline := 0;
  fnt.lfStrikeOut := 0;
  fnt.lfHeight := Abs(afont.Height);
  fname := afont.Name;
  for i := 1 to length(fname) do
    fnt.lffacename[i - 1] := fname[i];
  fnt.lfFaceName[length(fname)] := #0;
  hfnt := CreateFontIndirect(fnt);
  dc := Canvas.handle;
  SetBkMode(dc, windows.TRANSPARENT);
  SetTextColor(dc, afont.Color);
  hfntPrev := SelectObject(dc, hfnt);
  //Textout(dc,X,Y,@aText[1],length(aText));
  Textout(dc, X, Y, @s[1], length(s));
  SelectObject(dc, hfntPrev);
  DeleteObject(hfnt);
  Repaint;
end;
(*)
(*)
function TJvDrawImage.GetAngle(Origin, Endpoint: TPoint): Integer;
var
  a, d: Real;
begin
//  r := Sqrt(Sqr(Endpoint.X - Origin.X) + Sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := ArcTan2((Endpoint.Y - Origin.Y), d);
  a := a * 360 / (2 * pi);
  Result := Variant(-a);
end;
(*)
procedure TJvDrawImage.DrawRisingWaveSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
var
  t, xcenter, a, ycenter, b: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  i, dx, dy, xo, yo, r, bl: Integer;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  a := (X2 - X1) div 2;
  b := (Y2 - Y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := X1 + a;
  ycenter := Y1 + b;

  dx := (X2 - X1) div bl;
  dy := (Y2 - Y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := Y1 + b;
    b := a;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := X1 + a;
    a := b;
  end;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := Abs(Round(a * Sin(pi * xo / (X2 - X1))));
      Sphere(Clip, X1 + xo, r, ycenter, r, R1, G1, B1, R2, G2, B2, True);
    end
    else
    begin
      yo := i * dy + b;
      r := Abs(Round(b * Sin(pi * yo / (Y2 - Y1) - pi / 2)));
      Sphere(Clip, xcenter, r, Y1 + yo, r, R1, G1, B1, R2, G2, B2, True);
    end;
  end;
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawWaveSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
var
  t, xcenter, a, ycenter, b: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  i, dx, dy, xo, yo, r, bl: Integer;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  a := (X2 - X1) div 2;
  b := (Y2 - Y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := X1 + a;
  ycenter := Y1 + b;

  dx := (X2 - X1) div bl;
  dy := (Y2 - Y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := Y1 + b;
    b := a;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := X1 + a;
    a := b;
  end;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := Abs(Round(a * Sin(pi * xo / (X2 - X1) - pi / 2)));
      Sphere(Clip, X1 + xo, r, ycenter, r, R1, G1, B1, R2, G2, B2, True);
    end
    else
    begin
      yo := i * dy + b;
      r := Abs(Round(b * Sin(pi * yo / (Y2 - Y1) - pi / 2)));
      Sphere(Clip, xcenter, r, Y1 + yo, r, R1, G1, B1, R2, G2, B2, True);
    end;
  end;
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawDropletSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
var
  t, xcenter, a, ycenter, b: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  i, dx, dy, bl: Integer;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  a := (X2 - X1) div 2;
  b := (Y2 - Y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := X1 + a;
  ycenter := Y1 + b;

  dx := (X2 - X1) div bl;
  dy := (Y2 - Y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := Y1 + b;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := X1 + a;
  end;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
      Sphere(Clip, X1 + i * dx + a, a, ycenter, a, R1, G1, B1, R2, G2, B2, True)
    else
      Sphere(Clip, xcenter, b, Y1 + i * dy + b, b, R1, G1, B1, R2, G2, B2, True);
  end;
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawMultiSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
var
  t, xcenter, a, ycenter, b: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  i, dx, dy, bl: Integer;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  a := (X2 - X1) div 2;
  b := (Y2 - Y1) div 2;
  xcenter := X1 + a;
  ycenter := Y1 + b;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);
  dx := (X2 - X1) div bl;
  dy := (Y2 - Y1) div bl;
  if dx > dy then
  begin
    a := dx div 2;
    ycenter := Y1 + b;
  end
  else
  begin
    b := dy div 2;
    xcenter := X1 + a;
  end;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
      Sphere(Clip, X1 + i * dx + a, a, ycenter, a, R1, G1, B1, R2, G2, B2, True)
    else
      Sphere(Clip, xcenter, b, Y1 + i * dy + b, b, R1, G1, B1, R2, G2, B2, True);
  end;
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.Sphere(Bitmap: TBitmap;
  xcenter, a, ycenter, b: Integer; R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
var (* Dessine un disque Color *)
  xx, yy: Integer; (* par remplissage avec Couleur1-2 *)
  compt, x_ll, y_ll, x_ray, y_ray: Longint;
begin
  xx := 0;
  yy := b;
  x_ray := 2 * a * a;
  y_ray := 2 * b * b;
  x_ll := 1;
  y_ll := x_ray * b - 1;
  compt := y_ll div 2;
  while yy >= 0 do
  begin
    HorGradientLine(Bitmap, xcenter - xx, xcenter + xx, ycenter + yy, R1, G1, B1, R2, G2, B2, Smooth);
    HorGradientLine(Bitmap, xcenter - xx, xcenter + xx, ycenter - yy, R1, G1, B1, R2, G2, B2, Smooth);
    if compt >= 0 then
    begin
      x_ll := x_ll + y_ray;
      compt := compt - x_ll - 1;
      xx := xx + 1;
    end;
    if compt < 0 then
    begin
      y_ll := y_ll - x_ray;
      compt := compt + y_ll - 1;
      yy := yy - 1;
    end;
  end;
end;

procedure TJvDrawImage.DrawSphere(Color1, Color2: TColor; X1, Y1, X2, Y2: Integer);
var
  t, xcenter, a, ycenter, b: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  a := ((X2 - X1) div 2);
  xcenter := X1 + a;
  b := ((Y2 - Y1) div 2);
  ycenter := Y1 + b;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  Sphere(Clip, xcenter, a, ycenter, b, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.Column(Bitmap: TBitmap; XOrigin, XFinal, YOrigin, YFinal: Integer; R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
var
  j: Integer;
begin
  for j := YOrigin to YFinal do
    HorGradientLine(Bitmap, XOrigin, XFinal, j, R1, G1, B1, R2, G2, B2, Smooth);
end;

procedure TJvDrawImage.DrawColumn(X1, Y1, X2, Y2: Integer);
var
  t: Integer;
  R1, G1, B1, R2, G2, B2: Byte;
  line: pbytearray;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  if X1 > X2 then
  begin
    t := X1;
    X1 := X2;
    X2 := t;
  end;
  if Y1 > Y2 then
  begin
    t := Y1;
    Y1 := Y2;
    Y2 := t;
  end;
  line := Clip.ScanLine[Y1];
  R1 := line[0];
  G1 := line[1];
  B1 := line[2];
  line := Clip.ScanLine[Y2];
  R2 := line[X2 * 3];
  G2 := line[X2 * 3 + 1];
  B2 := line[X2 * 3 + 2];
  Column(Clip, X1, X2, Y1, Y2, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.InterpolateRect(Bmp: TBitmap; X1, Y1, X2, Y2: Integer);
// Draws rectangle, which will have different Color in each corner and
// will blend from one Color to another
// ( c[0,0]    c[1,0]
//   c[0,1]    c[1,1] )
type
  TFColor = record b, g, r: Byte
  end;
var
  xCount, yCount,
    t, t2, z, iz,
    rp, rp2, gp,
    gp2, bp, bp2,
    xx: Integer;
  pb: PByteArray;
  c00, c10, c01, c11: TFColor;
begin
  t := 0;
  t2 := 0;
  if X2 < X1 then
  begin
    t := X2;
    X2 := X1;
    X1 := t;
  end;
  if Y2 < Y1 then
  begin
    t := Y2;
    Y2 := Y1;
    Y1 := t;
  end;
  if (X1 < 0) or (Y1 < 0) or (X2 > Bmp.Width - 1) or (Y2 > Bmp.Height - 1) then
    Exit;
  z := 0;
  iz := $100000;
  if X2 <> X1 then
    t := $100000 div (X2 - X1);
  if Y2 <> Y1 then
    t2 := $100000 div (Y2 - Y1);
/////  dx := X2 - X1;
  pb := bmp.ScanLine[Y1];
  c00.r := pb[X1 * 3];
  c00.g := pb[X1 * 3 + 1];
  c00.b := pb[X1 * 3 + 2];
  c01.r := pb[X2 * 3];
  c01.g := pb[X2 * 3 + 1];
  c01.b := pb[X2 * 3 + 2];
  pb := bmp.ScanLine[Y2];
  c10.r := pb[X1 * 3];
  c10.g := pb[X1 * 3 + 1];
  c10.b := pb[X1 * 3 + 2];
  c11.r := pb[X2 * 3];
  c11.g := pb[X2 * 3 + 1];
  c11.b := pb[X2 * 3 + 2];
  for yCount := Y1 to Y2 do
  begin
    xx := ((c00.r * iz + c01.r * z) shr 20);
    rp := xx shl 20;
    rp2 := (((c10.r * iz + c11.r * z) shr 20) - xx) * t;
    xx := ((c00.g * iz + c01.g * z) shr 20);
    gp := xx shl 20;
    gp2 := (((c10.g * iz + c11.g * z) shr 20) - xx) * t;
    xx := ((c00.b * iz + c01.b * z) shr 20);
    bp := xx shl 20;
    bp2 := (((c10.b * iz + c11.b * z) shr 20) - xx) * t;
    pb := bmp.ScanLine[ycount];
    //    pb:=@Bmp.Pixels[yCount,X1];
    for xCount := X1 to X2 do
    begin
      pb[xcount * 3 + 2] := bp shr 20;
      Inc(bp, bp2);
      pb[xcount * 3 + 1] := gp shr 20;
      Inc(gp, gp2);
      pb[xcount * 3] := rp shr 20;
      Inc(rp, rp2);
    end;
    Inc(z, t2);
    Dec(iz, t2);
  end;
end;

procedure TJvDrawImage.InterpRect(X1, Y1, X2, Y2: Integer);
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  Interpolaterect(Clip, X1, Y1, X2, Y2);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawBlurLines(X0, Y0, X, Y: Integer);
begin
  DrawTexLines(X0, Y0, X, Y);
  ClipAll;
  Clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(Clip, UserFilter);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawBlurRects(X0, Y0, X, Y: Integer);
begin
  DrawTexRects(X0, Y0, X, Y);
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.GaussianBlur(Clip, 4);
  UserFilter := Blurfilter;
  applyfilter(Clip, UserFilter);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawTexRects(X0, Y0, X, Y: Integer);
var
  dx, dy, xr, yr, X1, Y1, X2, Y2, i, w, h, xi, yi: Integer;
  bcolor, pcolor, hcolor, scolor: TColor;
begin
  w := Width;
  h := Height;
  pcolor := Canvas.Pen.Color;
  bcolor := Canvas.Brush.Color;
  Canvas.Brush.Color := pcolor;
  Canvas.Brush.Style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := Abs(Round(Sqrt(Sqr(X - X0) + Sqr(Y - Y0))));
  dx := Abs(X - X0);
  dy := Abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := Round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 3 do
        with Canvas do
        begin
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          Pen.Color := scolor;
          Brush.Color := scolor;
          Rectangle(X1, Y1, X2 + 2, Y2 + 2);
          Pen.Color := hcolor;
          Brush.Color := hcolor;
          Rectangle(X1 - 2, Y1 - 2, X2, Y2);
          Pen.Color := pcolor;
          Brush.Color := pcolor;
          Rectangle(X1, Y1, X2, Y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.Pen.Color := pcolor;
  Canvas.Brush.Color := bcolor;
end;

procedure TJvDrawImage.DrawBlurPoly(X0, Y0, X, Y: Integer);
begin
  DrawTexPoly(X0, Y0, X, Y);
  ClipAll;
  Clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(Clip, UserFilter);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawTexPoly(X0, Y0, X, Y: Integer);
var
  dx, dy, xr, yr, X1, Y1, X2, Y2, i, w, h, xi, yi: Integer;
  pcolor: TColor;
  points: array[0..3] of TPoint;
begin
  w := Width;
  h := Height;
  pcolor := Canvas.Pen.Color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := Abs(Round(Sqrt(Sqr(X - X0) + Sqr(Y - Y0))));
  dx := Abs(X - X0);
  dy := Abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := Round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with Canvas do
        begin
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[0] := Point(X1, Y1);
          points[3] := Point(X2, Y2);
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[1] := Point(X1, Y1);
          points[2] := Point(X2, Y2);
          Pen.Color := pcolor;
          polyline(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.Pen.Color := pcolor;
end;

procedure TJvDrawImage.DrawBlurCurves(X0, Y0, X, Y: Integer);
begin
  DrawTexCurves(X0, Y0, X, Y);
  ClipAll;
  Clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(Clip, UserFilter);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawTexCurves(X0, Y0, X, Y: Integer);
var
  dx, dy, xr, yr, X1, Y1, X2, Y2, i, w, h, xi, yi: Integer;
  pcolor: TColor;
  points: array[0..3] of TPoint;
begin
  w := Width;
  h := Height;
  pcolor := Canvas.Pen.Color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := Abs(Round(Sqrt(Sqr(X - X0) + Sqr(Y - Y0))));
  dx := Abs(X - X0);
  dy := Abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := Round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with Canvas do
        begin
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[0] := Point(X1, Y1);
          points[3] := Point(X2, Y2);
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[1] := Point(X1, Y1);
          points[2] := Point(X2, Y2);
          Pen.Color := pcolor;
          PolyBezier(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.Pen.Color := pcolor;
end;

procedure TJvDrawImage.ApplyFilter(var Dst: TBitmap; DF: TDigitalFilter);
var
  i, j, X, Y, tmpx, tmpy: Integer;
  Sum,
    Red,
    Green,
    Blue: Integer; //total value
  Tmp,
    Color: TFColor;
  Ptmp, Pcolor: pbytearray;
  bm: TBitmap;
  R: TRect;
begin
  bm := TBitmap.Create;
  bm.pixelformat := pf24bit;
  bm.Width := Dst.Width;
  bm.Height := Dst.Height;
  R := Rect(0, 0, bm.Width, bm.Height);
  bm.Canvas.CopyRect(R, Dst.Canvas, R);
  sum := 0;
  for Y := 0 to 4 do
    for X := 0 to 4 do
      sum := sum + DF[X, Y];
  if Sum = 0 then
    Sum := 1;
  for Y := 0 to Dst.Height - 1 do
  begin
    Pcolor := Dst.ScanLine[Y];
    for X := 0 to bm.Width - 1 do
    begin
      Red := 0;
      Green := 0;
      Blue := 0;
      for i := 0 to 4 do
        for j := 0 to 4 do
        begin
          Tmpy := TrimInt(Y + j - 2, 0, bm.Height - 1);
          Tmpx := TrimInt(X + i - 2, 0, bm.Width - 1);
          ptmp := bm.ScanLine[Tmpy];
          Tmp.r := ptmp[tmpx * 3];
          Tmp.g := ptmp[tmpx * 3 + 1];
          Tmp.b := ptmp[tmpx * 3 + 2];
          //          Tmp:=@Dst.Pixels[TrimInt(Y+j-1,0,Dst.Height-1),
          //                           TrimInt(X+i-1,0,Dst.Width-1)];
          Inc(Blue, DF[i, j] * Tmp.b);
          Inc(Green, DF[i, j] * Tmp.g);
          Inc(Red, DF[i, j] * Tmp.r);
        end;
      Color.b := IntToByte(Blue div Sum);
      Color.g := IntToByte(Green div Sum);
      Color.r := IntToByte(Red div Sum);
      PColor[X * 3] := Color.r;
      Pcolor[X * 3 + 1] := Color.g;
      Pcolor[X * 3 + 2] := Color.b;
    end;
  end;
  bm.Free;
end;

procedure TJvDrawImage.DrawBlurOvals(X0, Y0, X, Y: Integer);
begin
  DrawTexOvals(X0, Y0, X, Y);
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.GaussianBlur(Clip, 4);
  UserFilter := Blurfilter;
  applyfilter(Clip, UserFilter);
  Picture.Bitmap.Assign(Clip);

end;

procedure TJvDrawImage.DrawTexOvals(X0, Y0, X, Y: Integer);
var
  dx, dy, xr, yr, X1, Y1, X2, Y2, i, w, h, xi, yi: Integer;
  bcolor, pcolor, hcolor, scolor: TColor;
begin
  w := Width;
  h := Height;
  pcolor := Canvas.Pen.Color;
  bcolor := Canvas.Brush.Color;
  Canvas.Brush.Color := pcolor;
  Canvas.Brush.Style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := Abs(Round(Sqrt(Sqr(X - X0) + Sqr(Y - Y0))));
  dx := Abs(X - X0);
  dy := Abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := Round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 3 do
        with Canvas do
        begin
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          Pen.Color := scolor;
          Brush.Color := scolor;
          Ellipse(X1, Y1, X2 + 2, Y2 + 2);
          Pen.Color := hcolor;
          Brush.Color := hcolor;
          Ellipse(X1 - 2, Y1 - 2, X2, Y2);
          Pen.Color := pcolor;
          Brush.Color := pcolor;
          Ellipse(X1, Y1, X2, Y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.Pen.Color := pcolor;
  Canvas.Brush.Color := bcolor;
end;

function TJvDrawImage.BlendColors(const Color1, Color2: Longint; Opacity: Integer): Longint;
var
  R, R1, R2, G, G1, G2, B, B1, B2: Integer;
begin
  Opacity := Abs(Opacity);
  if Opacity > 100 then
    Opacity := 100;
  R1 := GetRValue(ColorToRGB(Color1));
  G1 := GetGValue(ColorToRGB(Color1));
  B1 := GetBValue(ColorToRGB(Color1));
  R2 := GetRValue(ColorToRGB(Color2));
  G2 := GetGValue(ColorToRGB(Color2));
  B2 := GetBValue(ColorToRGB(Color2));
  R := trunc(R1 * Opacity / 100) + trunc(R2 * (100 - Opacity) / 100);
  G := trunc(G1 * Opacity / 100) + trunc(G2 * (100 - Opacity) / 100);
  B := trunc(B1 * Opacity / 100) + trunc(B2 * (100 - Opacity) / 100);
  Result := RGB(R, G, B);
end; { BlendColors }

function TJvDrawImage.TexHighlight(Colr: Longint): Longint;
var
  avg, r, g, b: Integer;
  tmp: Longint;
begin
  r := GetRValue(Colr);
  g := GetGValue(Colr);
  b := GetBValue(Colr);
  avg := (r + g + b) div 3;
  r := (255 + 255 + avg + r) div 4;
  g := (255 + 255 + avg + g) div 4;
  b := (255 + 255 + avg + b) div 4;
  tmp := RGB(r, g, b);
  Result := BlendColors(Colr, tmp, WeakBlend);
end; { Highlight }

function TJvDrawImage.TexShadow(Colr: Longint): Longint;
var
  r, g, b: Integer;
  tmp: Longint;
begin
  r := GetRValue(Colr);
  g := GetGValue(Colr);
  b := GetBValue(Colr);
  tmp := RGB(trunc(DarkStrength * r), trunc(DarkStrength * g),
    trunc(DarkStrength * b));
  Result := BlendColors(Colr, tmp, StrongBlend);
end; { Shadow }

procedure TJvDrawImage.DrawTexLines(X0, Y0, X, Y: Integer);
var
  dx, dy, xr, yr, X1, Y1, X2, Y2, i, w, h, xi, yi: Integer;
  pcolor, hcolor, scolor: TColor;
begin
  w := Width;
  h := Height;
  pcolor := Canvas.Pen.Color;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := Abs(Round(Sqrt(Sqr(X - X0) + Sqr(Y - Y0))));
  dx := Abs(X - X0);
  dy := Abs(Y - Y0);
  if dy = 0 then
    dy := 1;
  if dx = 0 then
    dx := 1;
//  tx := w div dx;
//  ty := h div dy;
  yr := Round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with Canvas do
        begin
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          Pen.Color := pcolor;
          MoveTo(X1, Y1);
          LineTo(X2, Y2);
          Pen.Color := hcolor;
          MoveTo(X1 - 1, Y1 - 1);
          LineTo(X2 - 1, Y2 - 1);
          Pen.Color := scolor;
          MoveTo(X1 + 1, Y1 + 1);
          LineTo(X2 + 1, Y2 + 1);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.Pen.Color := pcolor;
end;

procedure TJvDrawImage.DrawSyms(X, Y: Integer);
var
  X0, Y0, i: Integer;
  da: Real;
  apoint: TPoint;
begin
  X0 := Picture.Bitmap.Width div 2;
  Y0 := Picture.Bitmap.Height div 2;
  da := 2 * pi / StarPoints;
  apoint := Point(X, Y);
  for i := 0 to StarPoints - 1 do
  begin
    with Canvas do
    begin
      MoveTo(pointarray[i].X, pointarray[i].Y);
      LineTo(apoint.X, apoint.Y);
      pointarray[i] := apoint;
      apoint := Rotate(Point(X0, Y0), apoint, da);
    end;
  end;
end;

procedure TJvDrawImage.PutClip(M: TRect);
var
  dest: TRect;
begin
  Clip.Width := (m.Right - m.Left + 1);
  Clip.Height := (m.Bottom - m.Top + 1);
  dest := Rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.CopyMode := cmsrccopy;
  Clip.pixelformat := Picture.Bitmap.pixelformat;
  Clip.Canvas.CopyRect(dest, Canvas, m);
end;

procedure TJvDrawImage.DrawTriangle;
begin
  with Canvas do
  begin
    MoveTo(myskew[0].X, myskew[0].Y);
    LineTo(myskew[1].X, myskew[1].Y);
    LineTo(myskew[2].X, myskew[2].Y);
    LineTo(myskew[0].X, myskew[0].Y);
  end;
end;

procedure TJvDrawImage.DrawSkew;
begin
  with Canvas do
  begin
    MoveTo(myskew[0].X, myskew[0].Y);
    LineTo(myskew[1].X, myskew[1].Y);
    LineTo(myskew[2].X, myskew[2].Y);
    LineTo(myskew[3].X, myskew[3].Y);
    LineTo(myskew[0].X, myskew[0].Y);
  end;
end;

function TJvDrawImage.PointToBlock(X, Y: Integer): TRect;
var
  xb, yb, w, h: Integer;
begin
  w := Picture.Bitmap.Width;
  h := Picture.Bitmap.Height;
  xb := w div Blocks;
  yb := h div Blocks;
  Result.Left := (X div xb) * xb;
  Result.Top := (Y div yb) * yb;
  Result.Right := Result.Left + xb;
  Result.Bottom := Result.Top + yb;
end;

procedure TJvDrawImage.DrawCube;
var
  dx, dy: Integer;
begin
  with Canvas do
  begin
    dx := myskew[4].X - myskew[2].X;
    dy := myskew[4].Y - myskew[2].Y;
    MoveTo(myskew[0].X, myskew[0].Y);
    LineTo(myskew[1].X, myskew[1].Y);
    LineTo(myskew[2].X, myskew[2].Y);
    LineTo(myskew[3].X, myskew[3].Y);
    LineTo(myskew[0].X, myskew[0].Y);
    if (dx >= 0) and (dy <= 0) then
    begin
      MoveTo(myskew[0].X, myskew[0].Y);
      LineTo(myskew[0].X + dx, myskew[0].Y + dy);
      LineTo(myskew[1].X + dx, myskew[1].Y + dy);
      LineTo(myskew[2].X + dx, myskew[2].Y + dy);
      LineTo(myskew[2].X, myskew[2].Y);
      MoveTo(myskew[1].X, myskew[1].Y);
      LineTo(myskew[1].X + dx, myskew[1].Y + dy);
    end
    else
    if (dx >= 0) and (dy > 0) then
    begin
      MoveTo(myskew[1].X, myskew[1].Y);
      LineTo(myskew[1].X + dx, myskew[1].Y + dy);
      LineTo(myskew[2].X + dx, myskew[2].Y + dy);
      LineTo(myskew[3].X + dx, myskew[3].Y + dy);
      LineTo(myskew[3].X, myskew[3].Y);
      MoveTo(myskew[2].X, myskew[2].Y);
      LineTo(myskew[2].X + dx, myskew[2].Y + dy);
    end
    else
    if (dx < 0) and (dy > 0) then
    begin
      MoveTo(myskew[0].X, myskew[0].Y);
      LineTo(myskew[0].X + dx, myskew[0].Y + dy);
      LineTo(myskew[3].X + dx, myskew[3].Y + dy);
      LineTo(myskew[2].X + dx, myskew[2].Y + dy);
      LineTo(myskew[2].X, myskew[2].Y);
      MoveTo(myskew[3].X, myskew[3].Y);
      LineTo(myskew[3].X + dx, myskew[3].Y + dy);
    end
    else
    if (dx < 0) and (dy < 0) then
    begin
      MoveTo(myskew[1].X, myskew[1].Y);
      LineTo(myskew[1].X + dx, myskew[1].Y + dy);
      LineTo(myskew[0].X + dx, myskew[0].Y + dy);
      LineTo(myskew[3].X + dx, myskew[3].Y + dy);
      LineTo(myskew[3].X, myskew[3].Y);
      MoveTo(myskew[0].X, myskew[0].Y);
      LineTo(myskew[0].X + dx, myskew[0].Y + dy);
    end;
  end;
end;

procedure TJvDrawImage.VerGradientLine(Bitmap: TBitmap;
  YOrigin, YFinal, X: Integer; R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
var
  r, g, b, i: Integer;
  valueR, ValueG, ValueB, advalR, advalB, advalG: single;
  Line: PByteArray;
begin
  if (X >= 0) and (X < Bitmap.Width) then
  begin
    if YOrigin > YFinal then
    begin
      i := YOrigin;
      YOrigin := YFinal;
      YFinal := i;
    end;
    if YFinal <> YOrigin then
    begin
      advalR := (R2 - R1) / (YFinal - YOrigin);
      advalG := (G2 - G1) / (YFinal - YOrigin);
      advalB := (B2 - B1) / (YFinal - YOrigin);
    end
    else
    begin
      advalR := 0;
      advalG := 0;
      advalB := 0;
    end;

    valueR := R1;
    valueG := G1;
    valueB := B1;

    for i := YOrigin to YFinal do
    begin
      Line := Bitmap.ScanLine[i];
      valueR := valueR + advalR;
      r := Round(ValueR);
      if r > 255 then
        r := 255;
      if r < 0 then
        r := 0;
      valueG := valueG + advalG;
      g := Round(ValueG);
      if g > 255 then
        g := 255;
      if g < 0 then
        g := 0;
      valueB := valueB + advalB;
      b := Round(ValueB);
      if b > 255 then
        b := 255;
      if b < 0 then
        b := 0;
      if (X >= 0) and (X < Bitmap.Width) then
      begin
        Line[X * 3] := b;
        Line[X * 3 + 1] := g;
        Line[X * 3 + 2] := r;
      end;
    end;
    if Smooth then
    begin
      SmoothPnt(Bitmap, X, YOrigin - 1);
      SmoothPnt(Bitmap, X, YFinal + 1);
    end;
  end;
end;

procedure TJvDrawImage.DrawVGradientBrush(Color1, Color2: TColor; Y1, Y2, X: Integer);
var
  R1, G1, B1, R2, G2, B2: Byte;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  vergradientline(Clip, Y1, Y2, X, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.SmoothPnt(Bitmap: TBitmap; xk, yk: Integer);
type
  TFColor = record b, g, r: Byte
  end;
var
  Bleu, Vert, Rouge: Integer;
  Color: TFColor;
  BB, GG, RR: array[1..5] of Integer;
  Line: pbytearray;
begin
  if (xk > 0) and (yk > 0) and (xk < Bitmap.Width - 1) and (yk < Bitmap.Height - 1) then
  begin
    line := Bitmap.ScanLine[yk - 1];
    Color.r := line[xk * 3];
    Color.g := line[xk * 3 + 1];
    Color.b := line[xk * 3 + 2];
    RR[1] := Color.r;
    GG[1] := Color.g;
    BB[1] := Color.b;
    line := Bitmap.ScanLine[yk];
    Color.r := line[(xk + 1) * 3];
    Color.g := line[(xk + 1) * 3 + 1];
    Color.b := line[(xk + 1) * 3 + 2];
    RR[2] := Color.r;
    GG[2] := Color.g;
    BB[2] := Color.b;
    line := Bitmap.ScanLine[yk + 1];
    Color.r := line[xk * 3];
    Color.g := line[xk * 3 + 1];
    Color.b := line[xk * 3 + 2];
    RR[3] := Color.r;
    GG[3] := Color.g;
    BB[3] := Color.b;
    line := Bitmap.ScanLine[yk];
    Color.r := line[(xk - 1) * 3];
    Color.g := line[(xk - 1) * 3 + 1];
    Color.b := line[(xk - 1) * 3 + 2];
    RR[4] := Color.r;
    GG[4] := Color.g;
    BB[4] := Color.b;
    Bleu := (BB[1] + (BB[2] + BB[3] + BB[4])) div 4; (* Valeur moyenne *)
    Vert := (GG[1] + (GG[2] + GG[3] + GG[4])) div 4; (* en cours d'‚valuation        *)
    Rouge := (RR[1] + (RR[2] + RR[3] + RR[4])) div 4;
    Color.r := rouge;
    Color.g := vert;
    Color.b := bleu;
    line := Bitmap.ScanLine[yk];
    line[xk * 3] := Color.r;
    line[xk * 3 + 1] := Color.g;
    line[xk * 3 + 2] := Color.b;
  end;
end;

procedure TJvDrawImage.HorGradientLine(Bitmap: TBitmap;
  XOrigin, XFinal, Y: Integer; R1, G1, B1, R2, G2, B2: Byte; Smooth: Boolean);
var
  r, g, b, i: Integer;
  valueR, ValueG, ValueB, advalR, advalB, advalG: single;
  Line: PByteArray;
begin
  if (Y >= 0) and (Y < Bitmap.Height) then
  begin
    if XOrigin > XFinal then
    begin
      i := XOrigin;
      XOrigin := XFinal;
      XFinal := i;
    end;
    if XFinal <> XOrigin then
    begin
      advalR := (R2 - R1) / (XFinal - XOrigin);
      advalG := (G2 - G1) / (XFinal - XOrigin);
      advalB := (B2 - B1) / (XFinal - XOrigin);
    end
    else
    begin
      advalR := 0;
      advalG := 0;
      advalB := 0;
    end;

    valueR := R1;
    valueG := G1;
    valueB := B1;
    Line := Bitmap.ScanLine[Y];
    for i := XOrigin to XFinal do
    begin
      valueR := valueR + advalR;
      r := Round(ValueR);
      if r > 255 then
        r := 255;
      if r < 0 then
        r := 0;
      valueG := valueG + advalG;
      g := Round(ValueG);
      if g > 255 then
        g := 255;
      if g < 0 then
        g := 0;
      valueB := valueB + advalB;
      b := Round(ValueB);
      if b > 255 then
        b := 255;
      if b < 0 then
        b := 0;
      if (i >= 0) and (i < Bitmap.Width) then
      begin
        Line[i * 3] := b;
        Line[i * 3 + 1] := g;
        Line[i * 3 + 2] := r;
      end;
    end;
    if Smooth then
    begin
      SmoothPnt(Bitmap, XOrigin - 1, Y);
      SmoothPnt(Bitmap, XFinal + 1, Y);
    end;
  end;
end;

procedure TJvDrawImage.DrawGradientBrush(Color1, Color2: TColor; X1, X2, Y: Integer);
var
  R1, G1, B1, R2, G2, B2: Byte;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  Color1 := ColorToRGB(Color1);
  R1 := GetRValue(Color1);
  G1 := GetGValue(Color1);
  B1 := GetBValue(Color1);
  Color2 := ColorToRGB(Color2);
  R2 := GetRValue(Color2);
  G2 := GetGValue(Color2);
  B2 := GetBValue(Color2);
  horgradientline(Clip, X1, X2, Y, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawLighterCircle(X, Y, Mode: Integer);
var
  r: Integer;
begin
  r := Canvas.Pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, Point(X, Y), r, Mode);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawDarkerCircle(X, Y, Mode: Integer);
var
  r: Integer;
begin
  r := Canvas.Pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, Point(X, Y), r, Mode);
  Picture.Bitmap.Assign(Clip);

end;

procedure TJvDrawImage.ColorCircle(var bm: TBitmap; center: TPoint; Radius, Mode: Integer);
var
  p, p0, p1: pbytearray;
  dx, X, Y, w, h, i, j, sum, c: Integer;
  cm, tm: TBitmap;
  Rs, Rd: TRect;
begin
  X := center.X;
  Y := center.Y;
  w := bm.Width;
  h := bm.Height;
  cm := TBitmap.Create;
  cm.Width := 2 * Radius;
  cm.Height := 2 * Radius;
  cm.PixelFormat := pf24bit;
  tm := TBitmap.Create;
  tm.Width := 2 * Radius;
  tm.Height := 2 * Radius;
  tm.PixelFormat := pf24bit;
  tm.Canvas.Brush.Color := clBlack;
  tm.Canvas.Ellipse(0, 0, tm.Width - 1, tm.Height - 1);
  tm.Transparent := True;
  tm.TransparentColor := clBlack;
  Rd := Rect(0, 0, cm.Width, cm.Height);
  Rs := Rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  cm.Canvas.CopyRect(Rd, bm.Canvas, RS);
  p0 := nil;
  p1 := nil;
  for j := 0 to cm.Height - 1 do
  begin
    p := cm.ScanLine[j];
    if j > 0 then
      p0 := cm.ScanLine[j - 1];
    if j < (h - 1) then
      p1 := cm.ScanLine[j + 1];
    for i := 0 to cm.Width - 1 do
    begin
      case Mode of
        0: //Blue
          begin
            p[i * 3 + 1] := 0;
            p[i * 3 + 2] := 0;
          end;
        1: //Green
          begin
            p[i * 3] := 0;
            p[i * 3 + 2] := 0;
          end;
        2: //Red
          begin
            p[i * 3] := 0;
            p[i * 3 + 1] := 0;
          end;
        3: //not Blue
          begin
            p[i * 3] := 0;
          end;
        4: //not Green
          begin
            p[i * 3 + 1] := 0;
          end;
        5: //not Red
          begin
            p[i * 3 + 2] := 0;
          end;
        6: //half Blue
          begin
            p[i * 3] := p[i * 3] * 9 div 10;
          end;
        7: //half Green
          begin
            p[i * 3 + 1] := p[i * 3 + 1] * 9 div 10;
          end;
        8: //half Red
          begin
            p[i * 3 + 2] := p[i * 3 + 2] * 9 div 10;
          end;
        9: // darker
          begin
            p[i * 3] := Round(p[i * 3] * 10 / 11);
            p[i * 3 + 1] := Round(p[i * 3 + 1] * 10 / 11);
            p[i * 3 + 2] := Round(p[i * 3 + 2] * 10 / 11);
          end;
        10: // lighter
          begin
            p[i * 3] := Round(p[i * 3] * 11 / 10);
            p[i * 3 + 1] := Round(p[i * 3 + 1] * 11 / 10);
            p[i * 3 + 2] := Round(p[i * 3 + 2] * 11 / 10);
          end;
        11: // gray
          begin
            sum := Round((p[i * 3] + p[i * 3 + 1] + p[i * 3 + 2]) / 3);
            p[i * 3] := sum;
            p[i * 3 + 1] := sum;
            p[i * 3 + 2] := sum;
          end;
        12: // mix
          begin
            c := p[i * 3];
            p[i * 3] := p[i * 3 + 1];
            p[i * 3 + 1] := p[i * 3 + 2];
            p[i * 3 + 2] := c;
          end;
        13: //Smooth
          begin
            if ((j > 0) and (j < (h - 1)) and (i > 0) and (i < (w - 1))) then
            begin
              p[i * 3] := Round((p[(i - 1) * 3] + p[(i + 1) * 3] + p0[i * 3] + p1[i * 3]) / 4);
              p[i * 3 + 1] := Round((p[(i - 1) * 3 + 1] + p[(i + 1) * 3 + 1] + p0[i * 3 + 1] + p1[i * 3 + 1]) / 4);
              p[i * 3 + 2] := Round((p[(i - 1) * 3 + 2] + p[(i + 1) * 3 + 2] + p0[i * 3 + 2] + p1[i * 3 + 2]) / 4);
            end;
          end;
      end;
    end;
  end;
  cm.Canvas.Draw(0, 0, tm);
  cm.Transparent := True;
  cm.transparentcolor := clWhite;
  bm.Canvas.Draw(X - Radius, Y - Radius, cm);
  cm.Free;
  tm.Free;
end;

procedure TJvDrawImage.DrawColorCircle(X, Y, Mode: Integer);
var
  r: Integer;
begin
  Picture.Bitmap.pixelformat := pf24bit;
  Clip.Assign(Picture.Bitmap);
  Clip.PixelFormat := pf24bit;
  r := Canvas.Pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, Point(X, Y), r, Mode);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawLightBrush(X, Y, Radius, Amount: Integer; Style: TLightBrush);
var
  Src, Dst: TBitmap;
  Rclip, Rsrc: TRect;
begin
  if X < Radius then
    X := Radius;
  if Y < Radius then
    Y := Radius;
  if (X + Radius) > Clip.Width - 1 then
    X := Clip.Width - 1 - Radius;
  if (Y + Radius) > Clip.Height - 1 then
    Y := Clip.Height - 1 - Radius;
  Src := TBitmap.Create;
  Src.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Rclip := Rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Src.Width := Rclip.Right - Rclip.Left;
  Src.Height := RClip.Bottom - Rclip.Top;
  Dst.Width := Src.Width;
  Dst.Height := Src.Height;
  Rsrc := Rect(0, 0, Src.Width, Src.Height);
  Dst.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  case Style of
    lbBrightness: FX.lightness(Dst, Amount);
    lbSaturation: FX.saturation(Dst, Amount);
    lbContrast: FX.contrast(Dst, Amount);
  end;
  // mask code
  Src.Canvas.Brush.Color := clWhite;
  Src.Canvas.FillRect(Rsrc);
  Src.Canvas.Brush.Style := bssolid;
  Src.Canvas.Brush.Color := clBlack;
  Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
  Src.Transparent := True;
  Src.TransparentColor := clBlack;
  Dst.Canvas.Draw(0, 0, Src);
  Dst.Transparent := True;
  Dst.TransparentColor := clWhite;
  Canvas.Draw(0, 0, Clip);
  Canvas.Draw(X - Radius, Y - Radius, Dst);
  Src.Free;
  Dst.Free;
end;

procedure TJvDrawImage.SampleStretch(Src, Dst: TBitmap);
begin
  // use mitchelfilter from resample unit
  ImgStretch(Src, Dst,
    ResampleFilters[6].Filter, ResampleFilters[6].Width);
end;

procedure TJvDrawImage.DrawStretchBrush(X, Y, Radius: Integer; Amount: Extended; Style: TMorphBrush);
var
  Src, Dst: TBitmap;
  Rclip, Rsrc: TRect;
  dr: Integer;
begin
  if X < Radius then
    X := Radius;
  if Y < Radius then
    Y := Radius;
  if (X + Radius) > Clip.Width - 1 then
    X := Clip.Width - 1 - Radius;
  if (Y + Radius) > Clip.Height - 1 then
    Y := Clip.Height - 1 - Radius;
  Src := TBitmap.Create;
  Src.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Rclip := Rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Dst.Width := Rclip.Right - Rclip.Left;
  Dst.Height := RClip.Bottom - Rclip.Top;
  // now Change to Reduce
  Amount := Abs(Amount);
  if Amount < 1 then
    Amount := 1;
  dr := Round(Radius * Amount / 180);
  if dr < 5 then
    dr := 5;
  if dr > Radius then
    dr := Radius;
  //(mbVerBox,mbHorBox,mbVerOval,mbHorOval);
  case Style of
    mbVerOval, mbVerbox: Rclip := Rect(X - Radius, Y - dr, X + Radius, Y + dr);
    mbHorOval, mbHorBox: Rclip := Rect(X - dr, Y - Radius, X + dr, Y + Radius);
  end;
  Src.Width := Rclip.Right - Rclip.Left;
  Src.Height := RClip.Bottom - Rclip.Top;
  Rsrc := Rect(0, 0, Src.Width, Src.Height);
  Src.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  SampleStretch(Src, Dst);
  // mask code
  // reset Src dimensions for masking
  if Style in [mbHorOval, mbVerOval] then
  begin
    Src.Width := Dst.Width;
    Src.Height := Dst.Height;
    Src.Canvas.Brush.Color := clWhite;
    Src.Canvas.FillRect(Rsrc);
    Src.Canvas.Brush.Style := bssolid;
    Src.Canvas.Brush.Color := clBlack;
    Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
    Src.Transparent := True;
    Src.TransparentColor := clBlack;
    Dst.Canvas.Draw(0, 0, Src);
    Dst.Transparent := True;
    Dst.TransparentColor := clWhite;
    Canvas.Draw(0, 0, Clip);
  end;
  Canvas.Draw(X - Radius, Y - Radius, Dst);
  Src.Free;
  Dst.Free;
end;

procedure TJvDrawImage.Rimple(Src, Dst: TBitmap; Amount: Extended);
var
  ca, sa, a, dx, dy, r, sr, fr: Extended;
  w, h, X, Y, cx, cy, i, j, c, ci: Integer;
  p1, p2: pbytearray;
begin
  w := Src.Width;
  h := Src.Height;
  cx := w div 2;
  cy := h div 2;
  if Amount < 1 then
    Amount := 1;
  fr := cx / Amount;
  for Y := 0 to h - 1 do
  begin
    p1 := Src.ScanLine[Y];
    for X := 0 to w - 1 do
    begin
      dx := X - cx;
      dy := -(Y - cx);
      r := Sqrt(Sqr(dx) + Sqr(dy));
      sr := fr * Sin(r / cx * Amount * 2 * pi);
      if (r + sr < cx) and (r + sr > 0) then
      begin
        a := ArcTan2(dy, dx);
        sincos(a, sa, ca);
        i := cx + Round((r + sr) * ca);
        j := cy + Round((r + sr) * sa);
        p2 := Dst.ScanLine[j];
        c := X * 3;
        ci := i * 3;
        p2[ci] := p1[c];
        p2[ci + 1] := p1[c + 1];
        p2[ci + 2] := p1[c + 2];
      end;
    end;
  end;
end;

procedure TJvDrawImage.DrawEffectBrush(X, Y, Radius: Integer; Amount: Extended; Style: TLightBrush);
var
  Src, Dst: TBitmap;
  Rclip, Rsrc: TRect;
begin
  if X < Radius then
    X := Radius;
  if Y < Radius then
    Y := Radius;
  if (X + Radius) > Clip.Width - 1 then
    X := Clip.Width - 1 - Radius;
  if (Y + Radius) > Clip.Height - 1 then
    Y := Clip.Height - 1 - Radius;
  Src := TBitmap.Create;
  Src.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Rclip := Rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Src.Width := Rclip.Right - Rclip.Left;
  Src.Height := RClip.Bottom - Rclip.Top;
  Dst.Width := Src.Width;
  Dst.Height := Src.Height;
  Rsrc := Rect(0, 0, Src.Width, Src.Height);
  Src.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  case Style of
    lbfisheye: FX.fisheye(Src, Dst, Amount);
    lbrotate: FX.smoothrotate(Src, Dst, Src.Width div 2, Src.Height div 2, Amount);
    lbtwist: FX.twist(Src, Dst, Round(Amount));
    lbrimple: Rimple(Src, Dst, Amount);
    mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound, mbRound2:
      FX.SqueezeHor(Src, Dst, Round(Amount), Style);
    mbSplitRound, mbSplitWaste:
      FX.SplitRound(Src, Dst, Round(Amount), Style);
  end;
  // mask code
  Src.Canvas.Brush.Color := clWhite;
  Src.Canvas.FillRect(Rsrc);
  Src.Canvas.Brush.Style := bssolid;
  Src.Canvas.Brush.Color := clBlack;
  Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
  Src.Transparent := True;
  Src.TransparentColor := clBlack;
  Dst.Canvas.Draw(0, 0, Src);
  Dst.Transparent := True;
  Dst.TransparentColor := clWhite;
  Canvas.Draw(0, 0, Clip);
  Canvas.Draw(X - Radius, Y - Radius, Dst);
  Src.Free;
  Dst.Free;
end;

procedure TJvDrawImage.DrawPlasma(X, Y: Integer; Amount: Extended);
var
  Src: TBitmap;
  Rs: TRect;
  h, w, ra: Integer;
begin
  Src := TBitmap.Create;
  ra := Round(Amount);
  zoomrect := Rect(X - ra, Y - ra, X + ra, Y + ra);
  if zoomrect.Left < 0 then
    zoomrect.Left := 0;
  if zoomrect.Top < 0 then
    zoomrect.Top := 0;
  if zoomrect.Right > (FZoomClip.Width - 1) then
    zoomrect.Right := FZoomClip.Width - 1;
  if zoomrect.Bottom > (FZoomClip.Height - 1) then
    zoomrect.Bottom := FZoomClip.Height - 1;
  w := zoomrect.Right - zoomrect.Left + 1;
  h := zoomrect.Bottom - zoomrect.Top + 1;
  Src.Width := w;
  Src.Height := h;
  Src.PixelFormat := pf24bit;
  Rs := Rect(0, 0, w, h);
  Src.Canvas.CopyRect(Rs, FZoomClip.Canvas, zoomrect);
  Canvas.stretchDraw(Rect(0, 0, FZoomClip.Width, FZoomClip.Height), Src);
  Src.Free;
end;

function TJvDrawImage.Rotate(Origin, Endpoint: TPoint; Angle: Real): TPoint;
var
  a, d, r: Real;
begin
  r := Sqrt(Sqr(Endpoint.X - Origin.X) + Sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := ArcTan2((Endpoint.Y - Origin.Y), d);
  a := a + Angle;
  Result.X := Origin.X + Variant(r * Cos(a));
  Result.Y := Origin.Y + Variant(r * Sin(a));
end;

procedure TJvDrawImage.SetSyms(X, Y: Integer);
var
  X0, Y0, i: Integer;
  da: Real;
  apoint: TPoint;
begin
  X0 := Picture.Bitmap.Width div 2;
  Y0 := Picture.Bitmap.Height div 2;
  da := 2 * pi / StarPoints;
  apoint := Point(X, Y);
  pointarray[0] := apoint;
  for i := 1 to StarPoints - 1 do
  begin
    apoint := Rotate(Point(X0, Y0), apoint, da);
    pointarray[i] := apoint;
  end;
end;

function TJvDrawImage.GetBlue(AColor: TColor): Byte;
begin
  Result := GetBValue(ColorToRGB(AColor));
end;

function TJvDrawImage.GetGreen(AColor: TColor): Byte;
begin
  Result := GetGValue(ColorToRGB(AColor));
end;

function TJvDrawImage.GetRed(AColor: TColor): Byte;
begin
  Result := GetRValue(ColorToRGB(AColor));
end;

function TJvDrawImage.MixColors(Color1, Color2: TColor): TColor;
var
  R1, G1, B1: Byte;
begin
  Color1 := ColorToRGB(Color1);
  Color2 := ColorToRGB(Color2);
  R1 := (GetRed(Color1) + GetRed(Color2)) div 2;
  G1 := (GetGreen(Color1) + GetGreen(Color2)) div 2;
  B1 := (GetBlue(Color1) + GetBlue(Color2)) div 2;
  Result := rgb(R1, G1, B1);
end;

procedure TJvDrawImage.InitPlasma;
var
  w, h: Integer;
begin
  with Picture.Bitmap do
  begin
    w := Width;
    h := Height;
    FZoomClip.Width := w;
    FZoomClip.Height := h;
  end;
  FZoomClip.PixelFormat := pf24bit;
  FZoomClip.Canvas.Draw(0, 0, Picture.Bitmap);
end;

procedure TJvDrawImage.CopyClip;
var
  m, dest: TRect;
begin
  m := mycliprect;
  Clip.Width := m.Right - m.Left + 1;
  Clip.Height := m.Bottom - m.Top + 1;
  dest := Rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.CopyMode := clipcm;
  Clip.Canvas.CopyRect(dest, Canvas, m);
  Clip.pixelformat := pf24bit;
end;

procedure TJvDrawImage.ClipAll;
begin
  mycliprect := Rect(0, 0, Picture.Bitmap.Width - 1, Picture.Bitmap.Height - 1);
  clipcm := cmsrccopy;
  SetClip(clWhite);
  CopyClip;
end;

procedure TJvDrawImage.SetClip(AColor: TColor);
var
  m, dest: TRect;
begin
  m := mycliprect;
  Clip.Width := (m.Right - m.Left) + 1;
  Clip.Height := (m.Bottom - m.Top) + 1;
  dest := Rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.Brush.Color := AColor;
  Clip.Canvas.FillRect(dest);
end;

procedure TJvDrawImage.EscapePaint(X, Y: Integer; Shift: TShiftState);
begin
  if Shape = 'Polygon' then
  begin
    if freepolycount > 2 then
      Canvas.Polygon(slice(freepoly, freepolycount));
    freepolycount := 0;
    myDraw := False;
  end;
  if Shape = 'polyline' then
  begin
    freepolycount := 0;
    myDraw := False;
  end;
  if Shape = 'polybezier' then
  begin
    bezierfix1 := False;
    bezierfix2 := False;
    myorigin := Point(X, Y);
    myprevpoint := myorigin;
    if ssAlt in Shift then
      myDraw := False;
  end;

  Canvas.Pen.Mode := mypen;
  TargetPoint := Point(X, Y);
end;

procedure TJvDrawImage.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Wavepen := Canvas.Pen.Color;
  Wavebrush := Canvas.Brush.Color;
  if button = mbright then
  begin
    EscapePaint(X, Y, Shift);
    Exit;
  end;
  if ((ssCtrl in Shift) and (ssShift in Shift)) then
  begin
    X := targetpoint.X;
    Y := targetpoint.Y;
    mouse.CursorPos := clienttoscreen(Point(X, Y));
  end;
  Canvas.MoveTo(X, Y);
  myorigin := Point(X, Y);
  myprevpoint := myorigin;
  myslinedir := 'none';
  myDraw := True;
  mypen := Canvas.Pen.Mode;
  if (Shape = 'rangemove') or (Shape = 'rangesmear') then
  begin
    clipcm := cmSrcInvert;
    SetClip(clWhite);
    CopyClip;
    with Canvas do
    begin
      copymode := cmSrcInvert;
      Draw(X, Y, Clip);
    end;
  end;

  if Shape = 'darkerbrush' then
    ClipAll;
  if Shape = 'mbHorOval' then
    ClipAll;
  if Shape = 'mbVerOval' then
    ClipAll;
  if Shape = 'mbVerBox' then
    ClipAll;
  if Shape = 'mbHorBox' then
    ClipAll;
  if Shape = 'mbHor' then
    ClipAll;
  if Shape = 'mbTop' then
    ClipAll;
  if Shape = 'mbBottom' then
    ClipAll;
  if Shape = 'mbDiamond' then
    ClipAll;
  if Shape = 'mbWaste' then
    ClipAll;
  if Shape = 'mbRound' then
    ClipAll;
  if Shape = 'mbRound2' then
    ClipAll;
  if Shape = 'mbSplitRound' then
    ClipAll;
  if Shape = 'mbSplitWaste' then
    ClipAll;
  if Shape = 'zoombrush' then
    InitPlasma;
  if Shape = 'zoomkeepbrush' then
    InitPlasma;
  if Shape = 'brightnessbrush' then
    ClipAll;
  if Shape = 'contrastbrush' then
    ClipAll;
  if Shape = 'saturationbrush' then
    ClipAll;
  if Shape = 'fisheyebrush' then
    ClipAll;
  if Shape = 'fisheyefixbrush' then
    ClipAll;
  if Shape = 'rotatebrush' then
    ClipAll;
  if Shape = 'twistbrush' then
    ClipAll;
  if Shape = 'rimplebrush' then
    ClipAll;
  if Shape = 'lighterbrush' then
    ClipAll;
  if Shape = 'graybrush' then
    ClipAll;
  if Shape = 'rollmixbrush' then
    ClipAll;
  if Shape = 'smoothbrush' then
    ClipAll;

  if Shape = 'gradientbrush' then
  begin
    Picture.Bitmap.PixelFormat := pf24bit;
  end;
  if Shape = 'mixbrush' then
  begin
    with Canvas do
    begin
      if ssAlt in Shift then
        Pen.Color := MixColors(Pixels[X, Y - Pen.Width], Pixels[X, Y + Pen.Width])
      else
        Pen.Color := MixColors(Pixels[X - Pen.Width, Y], Pixels[X + Pen.Width, Y]);
    end;
  end;
  if Shape = 'sym' then
    SetSyms(X, Y);
  if Shape = 'chord3' then
    Shape := 'chord';
  if Shape = 'pie3' then
    Shape := 'pie';
  if Shape = 'arc3' then
    Shape := 'arc';
  if Shape = 'bezier3' then
    Shape := 'bezier';
  if Shape = 'skewrect2' then
    Shape := 'skewrect';
  if Shape = 'triangle2' then
    Shape := 'triangle';
  if Shape = 'cube2' then
    Shape := 'cube';
  if (Shape = 'snapshot') then
  begin
    myoldbrushstyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    myoldpenwidth := Canvas.Pen.Width;
    Canvas.Pen.Width := 1;
  end;
  if (Shape = 'bezier1') then
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      PolyBezier(mybezier);
      mybezier[1] := Point(X, Y);
      PolyBezier(mybezier);
    end;
  if (Shape = 'bezier2') then
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      PolyBezier(mybezier);
      mybezier[2] := Point(X, Y);
      PolyBezier(mybezier);
    end;
  Canvas.Pen.Mode := mypen;
end;

procedure TJvDrawImage.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  xp, yp, i, j, X1, Y1, X2, Y2, h, w, pw, movex, movey: Integer;
  myrect: TRect;
  Color1, Color2: TColor;
  R1, G1, B1, R2, B2, G2: Byte;
  dx, dy, Angle: Extended;

  function rr: Integer;
  begin
    Result := Round(Sqrt(Sqr(X - myorigin.X) + Sqr(Y - myorigin.Y)));
    if Result < 10 then
      Result := 10;
  end;

  procedure MoveOrigin;
  begin
    myorigin.X := myorigin.X + movex;
    myorigin.Y := myorigin.Y + movey;
  end;

begin
  decoX := X;
  decoY := Y;
  movex := X - myprevpoint.X;
  movey := Y - myprevpoint.Y;
  // test for scripting
  if ((ssCtrl in Shift) and (ssAlt in Shift)) then
    Exit;
  mypen := Canvas.Pen.Mode;
  h := Abs(Y - myorigin.Y);
//  w := Abs(X - myorigin.X);
  if myDraw then
  begin
    if (Shape = 'rangemove') or (Shape = 'rangesmear') then
    begin
      with Canvas do
      begin
        copymode := cmSrcInvert;
        if Shape = 'rangemove' then
          Draw(myprevpoint.X, myprevpoint.Y, Clip);
        Draw(X, Y, Clip);
        myprevpoint := Point(X, Y);
      end;
    end;
    if Shape = 'airbrush' then
      if AirBrush.Air then
        AirBrush.Draw(Canvas, X, Y);
    if (Shape = 'zoombrush') or (Shape = 'zoomkeepbrush') then
    begin
      w := Canvas.Pen.Width;
      if w < 5 then
        w := 50;
      DrawPlasma(X, Y, w);
    end;
    if Shape = 'fisheyebrush' then
    begin
      w := Canvas.Pen.Width;
      if w < 5 then
        w := 50;
      DrawEffectBrush(X, Y, w, 0.9, lbfisheye);
    end;
    if Shape = 'fisheyefixbrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) / pi * 0.5 + 0.5;
      if Angle < 0.55 then
        Angle := 0.55;
      if Angle > 0.99 then
        Angle := 0.99;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbfisheye);
    end;
    if Shape = 'twistbrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := Abs(ArcTan2(dy, dx) * 25 / pi) + 1;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Round(Angle), lbtwist);
    end;
    if Shape = 'mbHorOval' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbHorOval);
    end;
    if Shape = 'mbHorBox' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbHorBox);
    end;

    if Shape = 'mbVerOval' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbVerOval);
    end;

    if Shape = 'mbVerBox' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbVerBox);
    end;

    if Shape = 'mbHor' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbHor);
    end;

    if Shape = 'mbTop' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbTop);
    end;

    if Shape = 'mbBottom' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbBottom);
    end;
    if Shape = 'mbWaste' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbWaste);
    end;
    if Shape = 'mbRound' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbRound);
    end;
    if Shape = 'mbRound2' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbRound2);
    end;

    if Shape = 'mbDiamond' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbDiamond);
    end;
    if Shape = 'mbSplitRound' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbSplitRound);
    end;
    if Shape = 'mbSplitWaste' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbSplitWaste);
    end;

    if Shape = 'rimplebrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 10 / pi + 1;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbrimple);
    end;

    if Shape = 'rotatebrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbrotate);
    end;
    if Shape = 'brightnessbrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, Round(Angle), lbBrightness);
    end;
    if Shape = 'contrastbrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, Round(Angle), lbContrast);
    end;
    if Shape = 'saturationbrush' then
    begin
      if ssAlt in Shift then
        MoveOrigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := ArcTan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, Round(Angle), lbContrast);
    end;

    if Shape = 'Bluebrush' then
      DrawColorCircle(X, Y, 0);
    if Shape = 'Greenbrush' then
      DrawColorCircle(X, Y, 1);
    if Shape = 'redbrush' then
      DrawColorCircle(X, Y, 2);
    if Shape = 'notBluebrush' then
      DrawColorCircle(X, Y, 3);
    if Shape = 'notGreenbrush' then
      DrawColorCircle(X, Y, 4);
    if Shape = 'notredbrush' then
      DrawColorCircle(X, Y, 5);
    if Shape = 'halfBluebrush' then
      DrawColorCircle(X, Y, 6);
    if Shape = 'halfGreenbrush' then
      DrawColorCircle(X, Y, 7);
    if Shape = 'halfredbrush' then
      DrawColorCircle(X, Y, 8);
    if Shape = 'darkerbrush' then
      DrawDarkerCircle(X, Y, 9);
    if Shape = 'lighterbrush' then
      DrawLighterCircle(X, Y, 10);
    if Shape = 'graybrush' then
      DrawLighterCircle(X, Y, 11);
    if Shape = 'rollmixbrush' then
      DrawLighterCircle(X, Y, 12);
    if Shape = 'smoothbrush' then
      DrawLighterCircle(X, Y, 13);

    if Shape = 'gradientbrush' then
    begin
      with Canvas do
      begin
        if ssAlt in Shift then
        begin
          Color1 := Pixels[X, Y - Pen.Width];
          Color2 := Pixels[X, Y + Pen.Width];
          DrawVGradientBrush(Color1, Color2, Y - Pen.Width, Y + Pen.Width, X);
          DrawVGradientBrush(Color1, Color2, Y - Pen.Width, Y + Pen.Width, X - 1);
          DrawVGradientBrush(Color1, Color2, Y - Pen.Width, Y + Pen.Width, X - 2);
        end
        else
        begin
          Color1 := Pixels[X - Pen.Width, Y];
          Color2 := Pixels[X + Pen.Width, Y];
          DrawGradientBrush(Color1, Color2, X - Pen.Width, X + Pen.Width, Y);
          DrawGradientBrush(Color1, Color2, X - Pen.Width, X + Pen.Width, Y + 1);
          DrawGradientBrush(Color1, Color2, X - Pen.Width, X + Pen.Width, Y + 2);
        end;
      end;
    end;

    if Shape = 'cube1' then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        DrawCube;
        myskew[4] := Point(X, Y);
        DrawCube;
      end;
    if (Shape = 'rectangle') or (Shape = 'cube') or (Shape = 'maze')
      or (Shape = 'Interprect') or (Shape = 'interColumn') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        Rectangle(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        Rectangle(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := Point(X, Y);
      end;

    if Shape = 'roundrect' then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        RoundRect(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y, myround, myround);
        RoundRect(myorigin.X, myorigin.Y, X, Y, myround, myround);
        myprevpoint := Point(X, Y);
      end;
    if Shape = 'Blocks' then
      Canvas.FillRect(PointToBlock(X, Y));

    if (Shape = 'ellipse') or (Shape = 'globe') or (Shape = 'interSphere')
      or (Shape = 'MultiSphere') or (Shape = 'DropletSphere')
      or (Shape = 'WaveSphere') or (Shape = 'RisingWaveSphere')
      or (Shape = 'decooval') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        Ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        Ellipse(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := Point(X, Y);
      end;
    if (Shape = 'chord') or (Shape = 'pie') or (Shape = 'arc') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        Ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        Ellipse(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := Point(X, Y);
      end;
    if Shape = 'skewrect1' then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        DrawSkew;
        myskew[2] := Point(X, Y);
        myskew[3].X := myskew[0].X + (myskew[2].X - myskew[1].X);
        myskew[3].Y := myskew[0].Y + (myskew[2].Y - myskew[1].Y);
        DrawSkew;
      end;

    if Shape = 'triangle1' then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        DrawTriangle;
        myskew[2] := Point(X, Y);
        DrawTriangle;
      end;
    if (Shape = 'polyline') or (Shape = 'Polygon') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        PenPos := Point(myorigin.X, myorigin.Y);
        LineTo(myprevpoint.X, myprevpoint.Y);
        PenPos := Point(myorigin.X, myorigin.Y);
        LineTo(X, Y);
        myprevpoint := Point(X, Y);
      end;

    if Shape = 'polybezier' then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        mybezier[0] := Point(myorigin.X, myorigin.Y);
        mybezier[3] := Point(myprevpoint.X, myprevpoint.Y);
        if not bezierfix1 then
        begin
          mybezier[1].X := mybezier[0].X;
          mybezier[1].Y := mybezier[3].Y;
        end;
        if not bezierfix2 then
        begin
          mybezier[2].X := mybezier[3].X;
          mybezier[2].Y := mybezier[0].Y;
        end;
        PolyBezier(mybezier);
        mybezier[3] := Point(X, Y);
        if (ssCtrl in Shift) then
        begin
          bezierfix1 := True;
          mybezier[1] := mybezier[3];
        end;
        if not bezierfix1 then
        begin
          mybezier[1].X := mybezier[0].X;
          mybezier[1].Y := mybezier[3].Y;
        end;
        if (ssShift in Shift) then
        begin
          bezierfix2 := True;
          mybezier[2] := mybezier[3];
        end;
        if not bezierfix2 then
        begin
          mybezier[2].X := mybezier[3].X;
          mybezier[2].Y := mybezier[0].Y;
        end;

        PolyBezier(mybezier);
        myprevpoint := Point(X, Y);
      end;
    if (Shape = 'line') or (Shape = 'rotateText') or (Shape = 'Star')
      or (Shape = 'spiral') or (Shape = 'skewrect') or (Shape = 'triangle')
      or (Shape = 'cone') or (Shape = 'Spiro') or
      (Shape = 'decobar') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        PenPos := Point(myorigin.X, myorigin.Y);
        LineTo(myprevpoint.X, myprevpoint.Y);
        PenPos := Point(myorigin.X, myorigin.Y);
        LineTo(X, Y);
        myprevpoint := Point(X, Y);
      end;
    if (Shape = 'bezier') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        mybezier[0] := Point(myorigin.X, myorigin.Y);
        mybezier[1] := mybezier[0];
        mybezier[3] := Point(myprevpoint.X, myprevpoint.Y);
        mybezier[2] := mybezier[3];
        PolyBezier(mybezier);
        mybezier[3] := Point(X, Y);
        mybezier[2] := mybezier[3];
        PolyBezier(mybezier);
        myprevpoint := Point(X, Y);
      end;
    if (Shape = 'bezier1') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        PolyBezier(mybezier);
        mybezier[1] := Point(X, Y);
        PolyBezier(mybezier);
      end;
    if (Shape = 'bezier2') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        PolyBezier(mybezier);
        mybezier[2] := Point(X, Y);
        PolyBezier(mybezier);
      end;
    if Shape = 'spray' then
      for i := 1 to 10 do
      begin
        xp := random(30) - 15;
        yp := random(30) - 15;
        Canvas.Pixels[X + xp, Y + yp] := Canvas.Brush.Color;
      end;

    if (Shape = 'Waveline') or (Shape = 'fastWaveline')
      or (Shape = 'colorWaveline') then
      with Canvas do
      begin
        Canvas.LineTo(X, Y);
        myprevpoint := Point(X, Y);
      end;

    if Shape = 'borderWaveline' then
    begin
      Canvas.MoveTo(myprevpoint.X, myprevpoint.Y);
      Canvas.LineTo(X, Y);
      Canvas.MoveTo(Width - myprevpoint.X, Height - myprevpoint.Y);
      Canvas.LineTo(Width - X, Height - Y);
      myprevpoint := Point(X, Y);
    end;

    if Shape = 'decoline' then
    begin
      with Canvas do
      begin
        pw := Pen.Width;
        Pen.Color := Wavepen;
        Pen.Mode := pmCopy;
        MoveTo(myprevpoint.X, myprevpoint.Y);
        LineTo(X, Y);
        Pen.Width := pw * 2;
        Pen.Mode := pmmasknotpen;
        MoveTo(myprevpoint.X, myprevpoint.Y);
        LineTo(X, Y);
        myprevpoint := Point(X, Y);
        Pen.Width := pw;
        Pen.Mode := pmCopy;
        Pen.Mode := mypen;
      end;
    end;

    if (Shape = 'freehand') or (Shape = 'mixbrush') then
      with Canvas do
      begin
        Canvas.LineTo(X, Y);
        myprevpoint := Point(X, Y)
      end;
    if Shape = 'cloneall' then
      with Canvas do
      begin
        X1 := myorigin.X - TargetPoint.X;
        Y1 := myorigin.Y - Targetpoint.Y;
        X2 := X - X1;
        Y2 := Y - Y1;
        copymode := cmsrccopy;
        i := Pen.Width;
        copyrect(Rect(X, Y, X + i, Y + i), Canvas,
          Rect(X2, Y2, X2 + i, Y2 + i));
      end;

    if Shape = 'clonenottarget' then
      with Canvas do
      begin
        X1 := myorigin.X - TargetPoint.X;
        Y1 := myorigin.Y - Targetpoint.Y;
        X2 := X - X1;
        Y2 := Y - Y1;
        i := Pen.Width;
        PutClip(Rect(X2, Y2, X2 + i, Y2 + i));
        Clip.Transparent := True;
        Clip.TransparentColor := Pixels[Targetpoint.X, Targetpoint.Y];
        Draw(X, Y, Clip);
        Clip.Transparent := False;
      end;

    if (Shape = 'paste') and (ssShift in Shift) then
    begin
      myrect := Rect(0, 0, 0, 0);
      myrect.Left := X;
      myrect.Top := Y;
      myrect.Right := X + mycliprect.Right - mycliprect.Left;
      myrect.Bottom := Y + mycliprect.Bottom - mycliprect.Top;
      Canvas.CopyRect(myrect, Canvas, mycliprect);
    end;
    if Shape = 'sym' then
      DrawSyms(X, Y);
    if Shape = 'sline' then
    begin
      if myslinedir = 'none' then
        if Abs(X - myorigin.X) >= Abs(Y - myorigin.Y) then
          myslinedir := 'h'
        else
          myslinedir := 'v';
      if (myslinedir = 'h') and (Abs(Y - myprevpoint.Y) > myslinetol) then
        myslinedir := 'v';
      if (myslinedir = 'v') and (Abs(X - myprevpoint.X) > myslinetol) then
        myslinedir := 'h';
      if myslinedir = 'h' then
      begin
        Canvas.LineTo(X, myprevpoint.Y);
        myprevpoint.X := X;
      end;
      if myslinedir = 'v' then
      begin
        Canvas.LineTo(myprevpoint.X, Y);
        myprevpoint.Y := Y;
      end;
    end;

    if Shape = 'vmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
//      Y2 := Height;
      Canvas.PenPos := Point(X2 - X1, Y1);
      Canvas.LineTo(X2 - X, Y);
      Canvas.PenPos := Point(X1, Y1);
      Canvas.LineTo(X, Y);
      myprevpoint := Point(X, Y)
    end;
    if Shape = 'cmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := Point(X2 - X1, Y2 - Y1);
      Canvas.LineTo(X2 - X, Y2 - Y);
      Canvas.PenPos := Point(X1, Y1);
      Canvas.LineTo(X, Y);
      myprevpoint := Point(X, Y)
    end;
    if Shape = 'mirror4' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := Point(X2 - X1, Y2 - Y1);
      Canvas.LineTo(X2 - X, Y2 - Y);
      Canvas.PenPos := Point(X2 - X1, Y1);
      Canvas.LineTo(X2 - X, Y);
      Canvas.PenPos := Point(X1, Y2 - Y1);
      Canvas.LineTo(X, Y2 - Y);
      Canvas.PenPos := Point(X1, Y1);
      Canvas.LineTo(X, Y);
      myprevpoint := Point(X, Y)
    end;
    if Shape = 'hmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
//      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := Point(X1, Y2 - Y1);
      Canvas.LineTo(X, Y2 - Y);
      Canvas.PenPos := Point(X1, Y1);
      Canvas.LineTo(X, Y);
      myprevpoint := Point(X, Y)
    end;
    if (Shape = 'snapshot') or (Shape = 'bars') or (Shape = 'border') then
      with Canvas do
      begin
        Pen.Mode := pmNotXor;
        Pen.Style := psDot;
        Rectangle(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        Rectangle(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := Point(X, Y);
        Pen.Style := psSolid;
      end;
  end;
  Canvas.Pen.Mode := mypen;
  myprevpoint := Point(X, Y);
end;

procedure TJvDrawImage.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  myrect: TRect;
  xs, ys, xt, yt, i, tangle: Integer;
  c: TColor;
  drw: string;
  apoint: TPoint;
  Bitmap: TBitmap;
  X1, Y1, X2, Y2: Integer;
  dcurve: array[0..3] of TPoint;
  rgn: HRGN;
  R1, G1, B1, R2, G2, B2: Byte;
  r, g, b, dr, dg, db: Extended;
  AColor, pcolor: TColor;
  pw: Integer;
begin
  //Canvas.Pen.Color:=Wavepen;
  //Canvas.Brush.Color:=Wavebrush;

  if ((ssCtrl in Shift) and (ssAlt in Shift)) then
    Exit;
  if button = mbright then
    Exit;
  mypen := Canvas.Pen.Mode;

  if Shape = 'zoombrush' then
    Canvas.Draw(0, 0, FZoomClip);
  if Shape = 'transcopy' then
  begin
    clipcm := cmSrcCopy;
    SetClip(clWhite);
    CopyClip;
    myrect := Rect(X, Y, X + Clip.Width - 1, Y + Clip.Height - 1);
    {$IFDEF VCL}
    Canvas.brushcopy(myrect, Clip,
      Rect(0, 0, Clip.Width, Clip.Height), RangeTransColor);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Clip.Transparent := True;
    Clip.TransparentColor := RangeTransColor;
    Canvas.Draw(X,Y, Clip);
    {$ENDIF VisualCLX}
    myDraw := False;
  end;
  if Shape = 'cube1' then
  begin
    if mypen <> pmNotXor then
      DrawCube;
    Shape := 'cube2';
  end;

  if Shape = 'TexLines' then
  begin
    DrawTexLines(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Texovals' then
  begin
    DrawTexOvals(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Blurovals' then
  begin
    DrawBlurOvals(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Texcurves' then
  begin
    DrawTexCurves(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Blurcurves' then
  begin
    DrawBlurCurves(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Texpoly' then
  begin
    DrawTexPoly(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'Blurpoly' then
  begin
    DrawBlurPoly(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'TexRects' then
  begin
    DrawTexRects(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'BlurRects' then
  begin
    DrawBlurRects(myorigin.X, myorigin.Y, X, Y);
  end;
  if Shape = 'BlurLines' then
  begin
    DrawBlurLines(myorigin.X, myorigin.Y, X, Y);
  end;

  if Shape = 'cube' then
  begin
    myskew[0] := myorigin;
    myskew[2] := Point(X, Y);
    myskew[4] := myskew[2];
    myskew[1].X := myskew[2].X;
    myskew[1].Y := myskew[0].Y;
    myskew[3].X := myskew[0].X;
    myskew[3].Y := myskew[2].Y;
    DrawCube;
    Shape := 'cube1';
  end;

  if Shape = 'Interprect' then
    InterpRect(myorigin.X, myorigin.Y, X, Y);

  if Shape = 'interColumn' then
    DrawColumn(myorigin.X, myorigin.Y, X, Y);

  if Shape = 'interSphere' then
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssAlt in Shift then
        DrawSphere(Canvas.Pixels[myorigin.X, myorigin.Y],
          Canvas.Pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;

  if Shape = 'MultiSphere' then
  begin
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.Pen.Mode := pmCopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssAlt in Shift then
        DrawMultiSphere(Canvas.Pixels[myorigin.X, myorigin.Y],
          Canvas.Pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawMultiSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'DropletSphere' then
  begin
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.Pen.Mode := pmCopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssAlt in Shift then
        DrawDropletSphere(Canvas.Pixels[myorigin.X, myorigin.Y],
          Canvas.Pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawDropletSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'WaveSphere' then
  begin
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.Pen.Mode := pmCopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssAlt in Shift then
        DrawWaveSphere(Canvas.Pixels[myorigin.X, myorigin.Y],
          Canvas.Pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawWaveSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'RisingWaveSphere' then
  begin
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.Pen.Mode := pmCopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssAlt in Shift then
        DrawRisingWaveSphere(Canvas.Pixels[myorigin.X, myorigin.Y],
          Canvas.Pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawRisingWaveSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'rectangle' then
  begin
    if mypen <> pmNotXor then
      Canvas.Rectangle(myorigin.X, myorigin.Y, X, Y);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.Rectangle(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys);
      end;
    end;
  end;

  if Shape = 'maze' then
  begin
    if mypen <> pmNotXor then
      Canvas.Rectangle(myorigin.X, myorigin.Y, X, Y);
    xs := (X - myorigin.X) div 10;
    ys := (Y - myorigin.Y) div 10;
    xt := myorigin.X;
    yt := myorigin.Y;
    for i := 1 to 10 do
    begin
      Canvas.MoveTo(xt + i * xs, Y);
      Canvas.LineTo(X, Y - i * ys);
      Canvas.MoveTo(X, Y - i * ys);
      Canvas.LineTo(X - i * xs, yt);
      Canvas.MoveTo(X - i * xs, yt);
      Canvas.LineTo(xt, yt + i * ys);
      Canvas.MoveTo(xt, yt + i * ys);
      Canvas.LineTo(xt + i * xs, Y);
    end;
  end;

  if Shape = 'roundrect' then
  begin
    if mypen <> pmNotXor then
      Canvas.RoundRect(myorigin.X, myorigin.Y, X, Y, myround, myround);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.RoundRect(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys, myround, myround);
      end;
    end;
  end;

  if Shape = 'Blocks' then
    Canvas.FillRect(PointToBlock(X, Y));

  if Shape = 'Star' then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      PenPos := Point(myorigin.X, myorigin.Y);
      LineTo(myprevpoint.X, myprevpoint.Y);
    end;
    Canvas.Pen.Mode := mypen;
    for i := 1 to Stars do
    begin
      apoint := ReduceVector(myorigin, Point(X, Y), i / Stars);
      Star(apoint.X, apoint.Y);
    end;
  end;

  if Shape = 'spiral' then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      PenPos := Point(myorigin.X, myorigin.Y);
      LineTo(myprevpoint.X, myprevpoint.Y);
    end;
    Canvas.Pen.Mode := mypen;
    apoint := Point(100 * X, 100 * Y);
    myorigin.X := 100 * myorigin.X;
    myorigin.Y := 100 * myorigin.Y;
    for i := 1 to Variant(Spirals * 36) do
    begin
      apoint := Rotate(myorigin, apoint, spiraldir * pi / 18);
      apoint := ReduceVector(myorigin, apoint, spiralfactor);
      Canvas.LineTo(apoint.X div 100, apoint.Y div 100);
    end;
  end;

  if Shape = 'ellipse' then
  begin
    if mypen <> pmNotXor then
      Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.Ellipse(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys);
      end;
    end;
  end;

  if Shape = 'globe' then
  begin
    if mypen <> pmNotXor then
      Canvas.Ellipse(myorigin.X, myorigin.Y, X, Y);
    xs := (X - myorigin.X) div 20;
    ys := (Y - myorigin.Y) div 20;
    for i := 1 to 10 do
    begin
      Canvas.Ellipse(myorigin.X + i * xs, myorigin.Y, X - i * xs, Y);
      Canvas.Ellipse(myorigin.X, myorigin.Y + i * ys, X, Y - i * ys);
    end;
  end;

  if Shape = 'chord2' then
  begin
    mychord[7] := X;
    mychord[8] := Y;
    Shape := 'chord3';
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.Pen.Mode := mypen;
    Canvas.Chord(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if Shape = 'chord1' then
  begin
    mychord[5] := X;
    mychord[6] := Y;
    Shape := 'chord2';
  end;

  if Shape = 'chord' then
  begin
    mychord[1] := myorigin.X;
    mychord[2] := myorigin.Y;
    mychord[3] := X;
    mychord[4] := Y;
    Shape := 'chord1';
  end;

  if Shape = 'arc2' then
  begin
    mychord[7] := X;
    mychord[8] := Y;
    Shape := 'arc3';
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.Pen.Mode := mypen;
    Canvas.Arc(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if Shape = 'arc1' then
  begin
    mychord[5] := X;
    mychord[6] := Y;
    Shape := 'arc2';
  end;

  if Shape = 'arc' then
  begin
    mychord[1] := myorigin.X;
    mychord[2] := myorigin.Y;
    mychord[3] := X;
    mychord[4] := Y;
    Shape := 'arc1';
  end;

  if Shape = 'pie2' then
  begin
    mychord[7] := X;
    mychord[8] := Y;
    Shape := 'pie3';
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.Pen.Mode := mypen;
    Canvas.Pie(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if Shape = 'pie1' then
  begin
    mychord[5] := X;
    mychord[6] := Y;
    Shape := 'pie2';
  end;

  if Shape = 'pie' then
  begin
    mychord[1] := myorigin.X;
    mychord[2] := myorigin.Y;
    mychord[3] := X;
    mychord[4] := Y;
    Shape := 'pie1';
  end;

  if Shape = 'skewrect1' then
  begin
    if mypen <> pmNotXor then
      DrawSkew;
    Shape := 'skewrect2';
  end;

  if Shape = 'skewrect' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    if mypen <> pmNotXor then
      Canvas.LineTo(X, Y);
    myskew[0] := myorigin;
    myskew[1] := Point(X, Y);
    myskew[2] := myskew[1];
    myskew[3] := myskew[0];
    Shape := 'skewrect1';
  end;

  if Shape = 'triangle1' then
  begin
    if mypen <> pmNotXor then
      DrawTriangle;
    Shape := 'triangle2';
  end;

  if Shape = 'triangle' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    if mypen <> pmNotXor then
      Canvas.LineTo(X, Y);
    myskew[0] := myorigin;
    myskew[1] := Point(X, Y);
    myskew[2] := myskew[1];
    Shape := 'triangle1';
  end;

  if Shape = 'decobar' then
  begin
    Picture.Bitmap.PixelFormat := pf24bit;
    with Canvas do
    begin
      pw := Pen.Width;
      pcolor := Pen.Color;
      AColor := ColorToRGB(Wavebrush);
      R1 := GetRed(AColor);
      r := R1;
      G1 := GetGreen(AColor);
      g := G1;
      B1 := GetBlue(AColor);
      b := B1;
      AColor := ColorToRGB(Pen.Color);
      R2 := GetRed(AColor);
      G2 := GetGreen(AColor);
      B2 := GetBlue(AColor);
      dr := (R1 - R2) / (pw / 3);
      dg := (G1 - G2) / (pw / 3);
      db := (B1 - B2) / (pw / 3);
      if pw < 30 then
        Pen.Width := 30;
      for i := 1 to Pen.Width div 3 do
      begin
        r := r - dr;
        g := g - dg;
        b := b - db;
        Pen.Color := rgb(Round(r), Round(g), Round(b));
        MoveTo(myorigin.X, myorigin.Y);
        LineTo(X, Y);
        Pen.Width := Pen.Width - 2;
      end;
      Pen.Width := pw;
      Pen.Color := pcolor;
    end;
  end;

  if Shape = 'decooval' then
  begin
    Picture.Bitmap.PixelFormat := pf24bit;
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      Ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
      Pen.Mode := pmCopy;
      pw := Pen.Width;
      Brush.Style := bsClear;
      AColor := ColorToRGB(Wavebrush);
      R1 := GetRed(AColor);
      r := R1;
      G1 := GetGreen(AColor);
      g := G1;
      B1 := GetBlue(AColor);
      b := B1;
      AColor := ColorToRGB(Wavepen);
      R2 := GetRed(AColor);
      G2 := GetGreen(AColor);
      B2 := GetBlue(AColor);
      dr := (R1 - R2) / (pw / 3);
      dg := (G1 - G2) / (pw / 3);
      db := (B1 - B2) / (pw / 3);
      if pw < 30 then
        Pen.Width := 30;
      for i := 1 to Pen.Width div 3 do
      begin
        Pen.Width := Pen.Width - 2;
        r := r - dr;
        g := g - dg;
        b := b - db;
        Pen.Color := rgb(Round(r), Round(g), Round(b));
        Ellipse(myorigin.X, myorigin.Y, X, Y);
      end;
      Pen.Width := pw;
    end;
  end;

  if (Shape = 'polyline') or (Shape = 'Polygon') then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    if mypen <> pmNotXor then
      Canvas.LineTo(X, Y);
    if freepolycount = 0 then
    begin
      freepoly[0] := myorigin;
      inc(freepolycount);
    end
    else
    begin
      freepoly[freepolycount] := Point(X, Y);
      if freepolycount < 100 then
        inc(freepolycount);
    end;
  end;

  if Shape = 'line' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    if mypen <> pmNotXor then
    begin
      Canvas.LineTo(X, Y);
    end;
  end;

  if Shape = 'Spiro' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    Canvas.Pen.Mode := pmNotXor;
    Canvas.LineTo(X, Y);
    Canvas.Pen.Mode := mypen;
    DrawSpiro(myorigin, Point(X, Y));
  end;

  if Shape = 'cone' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    Canvas.Pen.Mode := pmNotXor;
    Canvas.LineTo(X, Y);
    Canvas.Pen.Mode := mypen;
    xt := (Picture.Bitmap.Width - 2 * myorigin.X) div 20;
    xs := (Picture.Bitmap.Width - 2 * X) div 20;
    X := Picture.Bitmap.Width div 2;
    with Canvas do
    begin
      for i := 0 to 10 do
      begin
        MoveTo(X + i * xt, myorigin.Y);
        LineTo(X + i * xs, Y);
        MoveTo(X - i * xt, myorigin.Y);
        LineTo(X - i * xs, Y);
      end;
      MoveTo(X + 10 * xt, myorigin.Y);
      LineTo(X - 10 * xt, myorigin.Y);
      MoveTo(X + 10 * xs, Y);
      LineTo(X - 10 * xs, Y);
    end;
  end;

  {if Shape='polybezier' then
   begin
    Canvas.PenPos:=Point(myorigin.X,myorigin.Y);
     if mypen<>pmNotXor then
       begin
       mybezier[0]:=myorigin;
       mybezier[3]:=Point(X,Y);
       if not bezierfix1 then
       begin
         mybezier[1].X:=mybezier[0].X;
         mybezier[1].Y:=mybezier[3].Y;
         end;
       if not bezierfix2 then
       begin
         mybezier[2].X:=mybezier[3].X;
         mybezier[2].Y:=mybezier[0].Y;
         end;
       Canvas.PolyBezier(mybezier);
       bezierfix1:=False;
       bezierfix2:=False;
       end;
    end;}

  if Shape = 'bezier2' then
  begin
    Canvas.PenPos := Point(myorigin.X, myorigin.Y);
    if mypen <> pmNotXor then
    begin
      mybezier[2] := Point(X, Y);
      Canvas.PolyBezier(mybezier);
    end;
    Shape := 'bezier3';
  end;

  if Shape = 'bezier1' then
    Shape := 'bezier2';
  if Shape = 'bezier' then
    Shape := 'bezier1';
  if Shape = 'bezier3' then
    Shape := 'bezier';
  {$IFDEF VCL}
  if Shape = 'floodfill' then
  begin
    if ssAlt in Shift then
      Canvas.FloodFill(X, Y, Canvas.Pen.Color, fsborder)
    else
      Canvas.FloodFill(X, Y, Canvas.Pixels[X, Y], fssurface);
  end;
  {$ENDIF VCL}
  if Shape = 'snapshot' then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      Pen.Style := psDot;
      Rectangle(myorigin.X, myorigin.Y, X, Y);
      Pen.Style := psSolid;
    end;
    mycliprect := Rect(myorigin.X, myorigin.Y, X, Y);
    Canvas.Brush.Style := myoldbrushstyle;
    Canvas.Pen.Width := myoldpenwidth;
    Shape := '';
  end;

  if Shape = 'bars' then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      Pen.Style := psDot;
      Rectangle(myorigin.X, myorigin.Y, X, Y);
      Pen.Style := psSolid;
    end;
    DrawBars(myorigin.X, myorigin.Y, X, Y);
  end;

  if Shape = 'border' then
  begin
    with Canvas do
    begin
      Pen.Mode := pmNotXor;
      Pen.Style := psDot;
      Rectangle(myorigin.X, myorigin.Y, X, Y);
      Pen.Style := psSolid;
    end;
    Drawborders(myorigin.X, myorigin.Y, X, Y);
  end;

  if Shape = 'paste' then
  begin
    myrect := Rect(0, 0, 0, 0);
    myrect.Left := X;
    myrect.Top := Y;
    myrect.Right := X + mycliprect.Right - mycliprect.Left;
    myrect.Bottom := Y + mycliprect.Bottom - mycliprect.Top;
    Canvas.CopyRect(myrect, Canvas, mycliprect);
  end;

  if Shape = 'pastecolor' then
  begin
    clipcm := cmsrccopy;
    SetClip(clWhite);
    CopyClip;
    FX.ExtractColor(Clip, Canvas.Brush.Color);
    Canvas.Draw(X, Y, Clip);
    Clip.Transparent := False;
  end;

  if Shape = 'pastecolorx' then
  begin
    clipcm := cmsrccopy;
    SetClip(clWhite);
    CopyClip;
    FX.ExcludeColor(Clip, Canvas.Brush.Color);
    Canvas.Draw(X, Y, Clip);
    Clip.Transparent := False;
  end;

  if Shape = 'zoomkeepbrush' then
  begin
    Shape := 'freehand';
    Canvas.Pen.Width := 2;
  end;

  if Shape = 'paintpick' then
  begin
    if ssAlt in Shift then
      Canvas.Brush.Color := MixColors(
        Canvas.Pixels[X - 5, Y],
        Canvas.Pixels[X + 5, Y])
    else
      Canvas.Brush.Color := Canvas.Pixels[X, Y];
    ColorPicked(Canvas.Brush.Color);
    Shape := '';
  end;
  Canvas.Pen.Mode := mypen;
  if not ((Shape = 'Polygon') or
    (Shape = 'polyline') or
    (Shape = 'polybezier')) then
  begin
    myDraw := False;
  end;
end;

procedure TJvDrawImage.BuildShapeList;
const
  Names: array [0..99] of PChar =
   (
    'airbrush',
    'arc',
    'bars',
    'bezier',
    'Blocks',
    'Bluebrush',
    'Blurcurves',
    'BlurLines',
    'Blurovals',
    'Blurpoly',
    'BlurRects',
    'border',
    'brightnessbrush',
    'chord',
    'cloneall',
    'clonenottarget',
    'cmirror',
    'cone',
    'contrastbrush',
    'cube',
    'darkerbrush',
    'decobar',
    'decoval',
    'DropletSphere',
    'ellipse',
    'fisheyebrush',
    'fisheyefixbrush',
    'floodfill',
    'freehand',
    'globe',
    'gradientbrush',
    'graybrush',
    'Greenbrush',
    'halfBluebrush',
    'halfGreenbrush',
    'halfredbrush',
    'hmirror',
    'interColumn',
    'Interprect',
    'interSphere',
    'lighterbrush',
    'line',
    'maze',
    'mbBottom',
    'mbDiamond',
    'mbHor',
    'mbHorBox',
    'mbHorOval',
    'mbRound',
    'mbRound2',
    'mbSplitRound',
    'mbSplitWaste',
    'mbTop',
    'mbVerBox',
    'mbVerOval',
    'mbWaste',
    'mirror4',
    'mixbrush',
    'MultiSphere',
    'notBluebrush',
    'notGreenbrush',
    'notredbrush',
    'paintpick',
    'paste',
    'pastecolor',
    'pastecolorx',
    'pie',
    'polybezier',
    'Polygon',
    'polyline',
    'rangemove',
    'rangesmear',
    'rectangle',
    'redbrush',
    'rimplebrush',
    'RisingWaveSphere',
    'rollmixbrush',
    'rotatebrush',
    'roundrect',
    'saturationbrush',
    'skewrect',
    'sline',
    'smoothbrush',
    'snapshot',
    'spiral',
    'Spiro',
    'Star',
    'sym',
    'Texcurves',
    'TexLines',
    'Texovals',
    'Texpoly',
    'TexRects',
    'transcopy',
    'triangle',
    'twistbrush',
    'vmirror',
    'WaveSphere',
    'zoombrush',
    'zoomkeepbrush'
   );
var
  I: Integer;
begin
  for I := Low(Names) to High(Names) do
    Shapes.Append(Names[I]);
end;

procedure TJvDrawImage.SetPolygonChecked(const Value: Boolean);
begin
  FPolygonChecked := Value;
end;

procedure TJvDrawImage.ColorPicked(AColor: TColor);
begin
  if Assigned(FOnColorPicked) then
    FOnColorPicked(Self, AColor);
end;

procedure TJvDrawImage.SetOnColorPicked(const Value: TColorPicked);
begin
  FOnColorPicked := Value;
end;

procedure TJvDrawImage.SetShape(const Value: string);
begin
  FShape := Value;
end;

procedure TJvDrawImage.Loaded;
begin
  inherited;
  autosize := True;
  Picture.Bitmap.Height := 256;
  Picture.Bitmap.Width := 256;
  Canvas.Brush.Color := clWhite;
  Picture.Bitmap.Canvas.FillRect(Rect(0, 0, Picture.Bitmap.Width, Picture.Bitmap.Height));
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clWhite;
  Canvas.MoveTo(100, 100);
  Canvas.LineTo(128, 128);
  Canvas.Pen.Color := clBlack;
end;

procedure TJvDrawImage.SetAirBrush(const Value: TJvAirBrush);
begin
  FAirBrush.Assign(Value);
end;

procedure TJvDrawImage.SetTransformer(const Value: TJvPaintFX);
begin
  FX.Assign(Value);
end;

procedure TJvDrawImage.SetBlocks(const Value: Integer);
begin
  FBlocks := Value;
end;

procedure TJvDrawImage.SetSpirals(const Value: Integer);
begin
  FSpirals := Value;
end;

procedure TJvDrawImage.SetStarPoints(const Value: Integer);
begin
  FStarPoints := Value;
end;

procedure TJvDrawImage.SetStars(const Value: Integer);
begin
  FStars := Value;
end;

procedure TJvDrawImage.contrastBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.contrast(Clip, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.saturationBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.saturation(Clip, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.lightnessBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.lightness(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.BlurBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.GaussianBlur(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.splitBlurBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.SplitBlur(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.colornoiseBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.AddColorNoise(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.mononoiseBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.AddmonoNoise(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.smoothBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.Smooth(Clip, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.Effects;
begin
  with PainterEffectsF do
  begin
    cxbar.Max := Width;
    cybar.Max := Height;
    cxbar.Position := cxbar.Max div 2;
    cybar.Position := cybar.Max div 2;
    Show;
  end;
end;

procedure TJvDrawImage.seamBarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.MakeSeamlessClip(Clip, painterEffectsF.Ebar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.mosaicBarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.mosaic(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.twistBarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.twist(Clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.FisheyeBarChange;
var
  bm2: TBitmap;
  am: single;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position / 100;
  FX.Fisheye(Clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.WaveBarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.Wave(Clip, am, 0, 0);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.WaveExtraChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.EBar.Position;
  FX.Wave(Clip, am, 0, 1);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.WaveInfChange;
var
  wa, inf: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  inf := painterEffectsF.Ebar.Position;
  wa := paintereffectsF.ExtraBar.Position;
  FX.Wave(Clip, wa, inf, 2);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.RotateBar;
var
  am: Extended;
  Dst: TBitmap;
  dx, dy: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  Dst.pixelformat := pf24bit;
  with PainterEffectsF do
  begin
    am := Ebar.Position;
    dx := cxBar.Position;
    dy := cyBar.Position;
  end;
  FX.SmoothRotate(Clip, Dst, dx, dy, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.XFormABarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  XFormA(am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.MarbleBarChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble2BarChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble2(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble3BarChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble3(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble4BarChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble4(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble5BarChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.EBar.Position;
  FX.Marble5(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble6barChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble6(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble7barChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble7(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Marble8barChange;
var
  turbulence: Integer;
  Dst: TBitmap;
  scale: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  Dst := TBitmap.Create;
  Dst.PixelFormat := pf24bit;
  Dst.Width := Clip.Width;
  Dst.Height := Clip.Height;
  scale := painterEffectsF.ExtraBar.Position;
  turbulence := painterEffectsF.Ebar.Position;
  FX.Marble8(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.embossbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.Emboss(Clip);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.filterRedbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterRed(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.filterGreenbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterGreen(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.filterBluebarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterBlue(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.FilterXRedbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXRed(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.FilterXGreenbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXGreen(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.FilterXBluebarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXBlue(Clip, paintereffectsF.ExtraBar.Position, painterEffectsF.EBar.Position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.SqueezeHorbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbHor);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeTopbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbTop);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeBotbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbBottom);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeDiamondbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbDiamond);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeWastebarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbwaste);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeRoundbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbRound);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SqueezeRound2barChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SqueezeHor(Clip, bm2, am, mbround2);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SplitRoundbarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SplitRound(Clip, bm2, am, mbSplitRound);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.SplitWastebarChange;
var
  bm2: TBitmap;
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  bm2 := TBitmap.Create;
  bm2.Width := Clip.Width;
  bm2.Height := Clip.Height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.SplitRound(Clip, bm2, am, mbSplitWaste);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.Update;
  bm2.Free;
end;

procedure TJvDrawImage.ShearbarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  Shear(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.plasmabarChange;
var
  am, turb, w, h: Integer;
  src1, src2: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  w := Clip.Width;
  h := Clip.Height;
  src1 := TBitmap.Create;
  src1.Width := w;
  src1.Height := h;
  src1.PixelFormat := pf24bit;
  src1.Canvas.Draw(0, 0, Clip);
  src2 := TBitmap.Create;
  src2.Width := w;
  src2.Height := h;
  src2.PixelFormat := pf24bit;
  src2.Canvas.Draw(0, 0, Clip);
  am := painterEffectsF.Ebar.Position;
  turb := painterEffectsF.ExtraBar.Position;
  if turb < 10 then
  begin
    painterEffectsF.ExtraBar.Position := 10;
    turb := 10;
  end;
  FX.Plasma(src1, src2, Clip, am, turb);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
  src2.Free;
  src1.Free;
end;

procedure TJvDrawImage.DrawMandelJulia(Mandel: Boolean);
var
  xr, yr: Extended;
  X0, Y0, X1, Y1: Extended;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  xr := painterEffectsF.Ebar.Position * 0.028;
  yr := painterEffectsF.ExtraBar.Position * 0.009;
  X0 := -2.25 + xr;
  X1 := 0.75;
  Y0 := -1.5 + yr;
  Y1 := 1.5;
  FX.DrawMandelJulia(Clip, X0, Y0, X1, Y1, 16, Mandel);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.DrawTriangles;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.Triangles(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.RippleTooth;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.RippleTooth(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.RippleTriangle;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.RippleTooth(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.RippleRandom;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.RippleRandom(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.TexturizeTile;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.TexturizeTile(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.TexturizeOverlap;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.TexturizeOverlap(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.DrawMap;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  FX.HeightMap(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.Update;
end;

procedure TJvDrawImage.DrawBlend;
var
  am, w, h: Integer;
  src2, Dst: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  w := Clip.Width;
  h := Clip.Height;
  {$IFDEF VCL}
  if not Clipboard.HasFormat(CF_BITMAP) then
    Exit;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if not Clipboard.Provides('image/delphi.bitmap') then
    Exit;
  {$ENDIF VisualCLX}
  src2 := TBitmap.Create;
  src2.Assign(Clipboard);
  src2.PixelFormat := pf24bit;
  if ((src2.Width <> w) or (src2.Height <> h)) then
  begin
    src2.Free;
    Exit;
  end;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Blend(Clip, src2, Dst, am / 100);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  src2.Free;
  Dst.Free;
end;

procedure TJvDrawImage.DrawSolarize;
var
  am, w, h: Integer;
  Dst: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  w := Clip.Width;
  h := Clip.Height;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Solarize(Clip, Dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Posterize;
var
  am, w, h: Integer;
  Dst: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.Position;
  w := Clip.Width;
  h := Clip.Height;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Posterize(Clip, Dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.Update;
  Dst.Free;
end;

procedure TJvDrawImage.Backgrounds;
begin
  PainterQBF.Show;
  PainterQBF.BringToFront;
end;

procedure TJvDrawImage.Preview(ABitmap: TBitmap);
begin
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(abitmap);
end;

procedure TJvDrawImage.Trace;
var
  BitMap: TBitmap;
begin
  BitMap := TBitmap.Create;
  Bitmap.Assign(Picture.Bitmap);
  FX.Trace(BitMap, 1);
  Picture.Bitmap.Assign(Bitmap);
  BitMap.Free;
  Update;
end;

end.
