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
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, ExtCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QGraphics, QControls, QForms, QDialogs, QExtCtrls, QWindows,
  {$ENDIF VisualCLX}
  JvAirBrush, JvPaintFX, JvResample;

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
  {$IFDEF VCL}
  Clipbrd,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QClipbrd,
  {$ENDIF VisualCLX}
  Math,
  JvTypes, JvPainterEffectsForm, JvQuickPreviewForm, JvPainterQBForm,
  JvResources;

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
  mypen: tpenmode;
  mypenstyle: tpenstyle;
  myoldbrushstyle: tbrushstyle;
  myoldpenwidth: Integer;
  myround: Integer;

  clipcm: Tcopymode;

  pointarray: array[0..12] of TPoint;
  spiralfactor: Real;
  spiraldir: Integer;
  TargetPoint: TPoint;
  zoomrect: TRect;
  freepoly: array[0..100] of TPoint;
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
  TargetPoint := point(0, 0);
  NSpiro := 40;
  RangeTransColor := clwhite;
  zoomrect := rect(0, 0, 50, 50);
  mycliprect := rect(0, 0, 256, 256);
  //spiral number, direction and Factor
  Spirals := 3;
  spiralfactor := 0.99;
  spiraldir := 1;
  // number of points for Star shape
  StarPoints := 5;
  Stars := 1;
  // tolerance for straight line Drawing
  myslinetol := 5;

  mypenstyle := pssolid;
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
    p2 := bm.scanline[Y];
    dx := round(Amount / 100 * Y);
    for X := 0 to w - 1 do
    begin
      c1 := X * 3;
      c2 := round(f * (X + dx)) * 3;
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
      p1 := Clip.scanline[Y];
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
  Canvas.FillRect(rect(0, 0, w, Y2 - Y1));
  Canvas.FillRect(rect(0, h - (Y2 - Y1), w, h));
  Canvas.FillRect(rect(0, 0, X2 - X1, h));
  Canvas.FillRect(rect(w - (X2 - X1), 0, w, h));
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
  Canvas.FillRect(rect(X1, Y1, X2, Y2));
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
    showmessage(RsImageMustBeSquare);
    exit;
  end;
  r0 := Variant(sqrt(sqr(center.X - xs) + sqr(center.Y - ys)));
  R1 := Variant(sqrt(sqr(Radius.X - center.X) + sqr(Radius.Y - center.Y)));
  if (r0 + R1) > xs then
  begin
    showmessage(RsSumOfRadiTolarge);
    exit;
  end;
  if (r0 < 5) or (R1 < 5) then
  begin
    showmessage(Format(RsBothRadiMustBeGr, [5]));
    exit;
  end;
  da1 := 2 * pi / 36;
  da0 := R1 / r0 * da1;
  a0 := 0;
  a1 := 0;
  Canvas.moveto(xs + r0 + R1, ys);
  for i := 1 to 36 * NSpiro do
  begin
    X1 := R1 * cos(a1);
    Y1 := R1 * sin(a1);
    a1 := a1 + da1;
    X0 := r0 * cos(a0);
    Y0 := r0 * sin(a0);
    a0 := a0 + da0;
    X := Variant(xs + X0 + X1);
    Y := Variant(ys + Y0 + Y1);
    Canvas.lineto(X, Y)
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
//777  d := abs(Y - Y0);
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
    pointarray[0] := point(X, Y);
    //   moveto(X,Y);
    apoint := point(X, Y);
    for i := 1 to StarPoints - 1 do
    begin
      //      apoint:=Rotate(point(X0,Y0),apoint,da);
      //      lineto(apoint.X,apoint.Y);
      apoint := Rotate(point(X0, Y0), apoint, da);
      pointarray[i] := apoint;
    end;
    //      lineto(X,Y);
    Polygon(Slice(PointArray, StarPoints))
  end;

end;

function TJvDrawImage.ReduceVector(Origin, Endpoint: TPoint;
  Factor: Real): TPoint;
var
  a, d, r: Real;
begin
  r := sqrt(sqr(Endpoint.X - Origin.X) + sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := arctan2((Endpoint.Y - Origin.Y), d);
  r := r * Factor;
  result.X := Origin.X + Variant(r * cos(a));
  result.Y := Origin.Y + Variant(r * sin(a));
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
  fnt.lfHeight := abs(afont.Height);
  fname := afont.Name;
  for i := 1 to length(fname) do
    fnt.lffacename[i - 1] := fname[i];
  fnt.lfFaceName[length(fname)] := #0;
  hfnt := CreateFontIndirect(fnt);
  dc := Canvas.handle;
  SetBkMode(dc, windows.TRANSPARENT);
  SetTextColor(dc, afont.color);
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
//  r := sqrt(sqr(Endpoint.X - Origin.X) + sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := arctan2((Endpoint.Y - Origin.Y), d);
  a := a * 360 / (2 * pi);
  result := Variant(-a);
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := abs(round(a * sin(pi * xo / (X2 - X1))));
      Sphere(Clip, X1 + xo, r, ycenter, r, R1, G1, B1, R2, G2, B2, True);
    end
    else
    begin
      yo := i * dy + b;
      r := abs(round(b * sin(pi * yo / (Y2 - Y1) - pi / 2)));
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := abs(round(a * sin(pi * xo / (X2 - X1) - pi / 2)));
      Sphere(Clip, X1 + xo, r, ycenter, r, R1, G1, B1, R2, G2, B2, True);
    end
    else
    begin
      yo := i * dy + b;
      r := abs(round(b * sin(pi * yo / (Y2 - Y1) - pi / 2)));
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
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
var (* Dessine un disque color *)
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
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
  line := Clip.scanline[Y1];
  R1 := line[0];
  G1 := line[1];
  B1 := line[2];
  line := Clip.scanline[Y2];
  R2 := line[X2 * 3];
  G2 := line[X2 * 3 + 1];
  B2 := line[X2 * 3 + 2];
  Column(Clip, X1, X2, Y1, Y2, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.InterpolateRect(Bmp: TBitmap; X1, Y1, X2, Y2: Integer);
// Draws rectangle, which will have different color in each corner and
// will blend from one color to another
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
  pb := bmp.scanline[Y1];
  c00.r := pb[X1 * 3];
  c00.g := pb[X1 * 3 + 1];
  c00.b := pb[X1 * 3 + 2];
  c01.r := pb[X2 * 3];
  c01.g := pb[X2 * 3 + 1];
  c01.b := pb[X2 * 3 + 2];
  pb := bmp.scanline[Y2];
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
    pb := bmp.scanline[ycount];
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
  pcolor := Canvas.pen.color;
  bcolor := Canvas.brush.color;
  Canvas.brush.color := pcolor;
  Canvas.brush.Style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(X - X0) + sqr(Y - Y0))));
  dx := abs(X - X0);
  dy := abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
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
          pen.color := scolor;
          brush.color := scolor;
          rectangle(X1, Y1, X2 + 2, Y2 + 2);
          pen.color := hcolor;
          brush.color := hcolor;
          rectangle(X1 - 2, Y1 - 2, X2, Y2);
          pen.color := pcolor;
          brush.color := pcolor;
          rectangle(X1, Y1, X2, Y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.pen.color := pcolor;
  Canvas.brush.color := bcolor;
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
  pcolor := Canvas.pen.color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(X - X0) + sqr(Y - Y0))));
  dx := abs(X - X0);
  dy := abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
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
          points[0] := point(X1, Y1);
          points[3] := point(X2, Y2);
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[1] := point(X1, Y1);
          points[2] := point(X2, Y2);
          pen.color := pcolor;
          polyline(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.pen.color := pcolor;
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
  pcolor := Canvas.pen.color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(X - X0) + sqr(Y - Y0))));
  dx := abs(X - X0);
  dy := abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
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
          points[0] := point(X1, Y1);
          points[3] := point(X2, Y2);
          X1 := xi + random(xr);
          Y1 := yi + random(yr);
          X2 := xi + random(xr);
          Y2 := yi + random(yr);
          points[1] := point(X1, Y1);
          points[2] := point(X2, Y2);
          pen.color := pcolor;
          polybezier(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.pen.color := pcolor;
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
  R := rect(0, 0, bm.Width, bm.Height);
  bm.Canvas.CopyRect(R, Dst.Canvas, R);
  sum := 0;
  for Y := 0 to 4 do
    for X := 0 to 4 do
      sum := sum + DF[X, Y];
  if Sum = 0 then
    Sum := 1;
  for Y := 0 to Dst.Height - 1 do
  begin
    Pcolor := Dst.scanline[Y];
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
          ptmp := bm.scanline[Tmpy];
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
      PColor[X * 3] := color.r;
      Pcolor[X * 3 + 1] := color.g;
      Pcolor[X * 3 + 2] := color.b;
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
  pcolor := Canvas.pen.color;
  bcolor := Canvas.brush.color;
  Canvas.brush.color := pcolor;
  Canvas.brush.Style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(X - X0) + sqr(Y - Y0))));
  dx := abs(X - X0);
  dy := abs(Y - Y0);
  if dy < 3 then
    dy := 3;
  if dx < 3 then
    dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
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
          pen.color := scolor;
          brush.color := scolor;
          ellipse(X1, Y1, X2 + 2, Y2 + 2);
          pen.color := hcolor;
          brush.color := hcolor;
          ellipse(X1 - 2, Y1 - 2, X2, Y2);
          pen.color := pcolor;
          brush.color := pcolor;
          ellipse(X1, Y1, X2, Y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.pen.color := pcolor;
  Canvas.brush.color := bcolor;
end;

function TJvDrawImage.BlendColors(const Color1, Color2: Longint; Opacity: Integer): Longint;
var
  R, R1, R2, G, G1, G2, B, B1, B2: Integer;
begin
  Opacity := abs(Opacity);
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
  pcolor := Canvas.pen.color;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(X - X0) + sqr(Y - Y0))));
  dx := abs(X - X0);
  dy := abs(Y - Y0);
  if dy = 0 then
    dy := 1;
  if dx = 0 then
    dx := 1;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
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
          pen.color := pcolor;
          MoveTo(X1, Y1);
          LineTo(X2, Y2);
          pen.color := hcolor;
          moveto(X1 - 1, Y1 - 1);
          lineto(X2 - 1, Y2 - 1);
          pen.color := scolor;
          moveto(X1 + 1, Y1 + 1);
          lineto(X2 + 1, Y2 + 1);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  Canvas.pen.color := pcolor;
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
  apoint := point(X, Y);
  for i := 0 to StarPoints - 1 do
  begin
    with Canvas do
    begin
      moveto(pointarray[i].X, pointarray[i].Y);
      lineto(apoint.X, apoint.Y);
      pointarray[i] := apoint;
      apoint := Rotate(point(X0, Y0), apoint, da);
    end;
  end;
end;

procedure TJvDrawImage.PutClip(M: TRect);
var
  dest: TRect;
begin
  Clip.Width := (m.right - m.left + 1);
  Clip.Height := (m.bottom - m.top + 1);
  dest := rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.CopyMode := cmsrccopy;
  Clip.pixelformat := Picture.Bitmap.pixelformat;
  Clip.Canvas.CopyRect(dest, Canvas, m);
end;

procedure TJvDrawImage.DrawTriangle;
begin
  with Canvas do
  begin
    moveto(myskew[0].X, myskew[0].Y);
    lineto(myskew[1].X, myskew[1].Y);
    lineto(myskew[2].X, myskew[2].Y);
    lineto(myskew[0].X, myskew[0].Y);
  end;
end;

procedure TJvDrawImage.DrawSkew;
begin
  with Canvas do
  begin
    moveto(myskew[0].X, myskew[0].Y);
    lineto(myskew[1].X, myskew[1].Y);
    lineto(myskew[2].X, myskew[2].Y);
    lineto(myskew[3].X, myskew[3].Y);
    lineto(myskew[0].X, myskew[0].Y);
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
  result.left := (X div xb) * xb;
  result.top := (Y div yb) * yb;
  result.Right := result.left + xb;
  result.Bottom := result.top + yb;
end;

procedure TJvDrawImage.DrawCube;
var
  dx, dy: Integer;
begin
  with Canvas do
  begin
    dx := myskew[4].X - myskew[2].X;
    dy := myskew[4].Y - myskew[2].Y;
    moveto(myskew[0].X, myskew[0].Y);
    lineto(myskew[1].X, myskew[1].Y);
    lineto(myskew[2].X, myskew[2].Y);
    lineto(myskew[3].X, myskew[3].Y);
    lineto(myskew[0].X, myskew[0].Y);
    if (dx >= 0) and (dy <= 0) then
    begin
      moveto(myskew[0].X, myskew[0].Y);
      lineto(myskew[0].X + dx, myskew[0].Y + dy);
      lineto(myskew[1].X + dx, myskew[1].Y + dy);
      lineto(myskew[2].X + dx, myskew[2].Y + dy);
      lineto(myskew[2].X, myskew[2].Y);
      moveto(myskew[1].X, myskew[1].Y);
      lineto(myskew[1].X + dx, myskew[1].Y + dy);
    end
    else
    if (dx >= 0) and (dy > 0) then
    begin
      moveto(myskew[1].X, myskew[1].Y);
      lineto(myskew[1].X + dx, myskew[1].Y + dy);
      lineto(myskew[2].X + dx, myskew[2].Y + dy);
      lineto(myskew[3].X + dx, myskew[3].Y + dy);
      lineto(myskew[3].X, myskew[3].Y);
      moveto(myskew[2].X, myskew[2].Y);
      lineto(myskew[2].X + dx, myskew[2].Y + dy);
    end
    else
    if (dx < 0) and (dy > 0) then
    begin
      moveto(myskew[0].X, myskew[0].Y);
      lineto(myskew[0].X + dx, myskew[0].Y + dy);
      lineto(myskew[3].X + dx, myskew[3].Y + dy);
      lineto(myskew[2].X + dx, myskew[2].Y + dy);
      lineto(myskew[2].X, myskew[2].Y);
      moveto(myskew[3].X, myskew[3].Y);
      lineto(myskew[3].X + dx, myskew[3].Y + dy);
    end
    else
    if (dx < 0) and (dy < 0) then
    begin
      moveto(myskew[1].X, myskew[1].Y);
      lineto(myskew[1].X + dx, myskew[1].Y + dy);
      lineto(myskew[0].X + dx, myskew[0].Y + dy);
      lineto(myskew[3].X + dx, myskew[3].Y + dy);
      lineto(myskew[3].X, myskew[3].Y);
      moveto(myskew[0].X, myskew[0].Y);
      lineto(myskew[0].X + dx, myskew[0].Y + dy);
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
      Line := Bitmap.scanline[i];
      valueR := valueR + advalR;
      r := round(ValueR);
      if r > 255 then
        r := 255;
      if r < 0 then
        r := 0;
      valueG := valueG + advalG;
      g := round(ValueG);
      if g > 255 then
        g := 255;
      if g < 0 then
        g := 0;
      valueB := valueB + advalB;
      b := round(ValueB);
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
  vergradientline(Clip, Y1, Y2, X, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.SmoothPnt(Bitmap: TBitmap; xk, yk: Integer);
type
  TFColor = record b, g, r: Byte
  end;
var
  Bleu, Vert, Rouge: Integer;
  color: TFColor;
  BB, GG, RR: array[1..5] of Integer;
  Line: pbytearray;
begin
  if (xk > 0) and (yk > 0) and (xk < Bitmap.Width - 1) and (yk < Bitmap.Height - 1) then
  begin
    line := Bitmap.scanline[yk - 1];
    color.r := line[xk * 3];
    color.g := line[xk * 3 + 1];
    color.b := line[xk * 3 + 2];
    RR[1] := color.r;
    GG[1] := color.g;
    BB[1] := color.b;
    line := Bitmap.scanline[yk];
    color.r := line[(xk + 1) * 3];
    color.g := line[(xk + 1) * 3 + 1];
    color.b := line[(xk + 1) * 3 + 2];
    RR[2] := color.r;
    GG[2] := color.g;
    BB[2] := color.b;
    line := Bitmap.scanline[yk + 1];
    color.r := line[xk * 3];
    color.g := line[xk * 3 + 1];
    color.b := line[xk * 3 + 2];
    RR[3] := color.r;
    GG[3] := color.g;
    BB[3] := color.b;
    line := Bitmap.scanline[yk];
    color.r := line[(xk - 1) * 3];
    color.g := line[(xk - 1) * 3 + 1];
    color.b := line[(xk - 1) * 3 + 2];
    RR[4] := color.r;
    GG[4] := color.g;
    BB[4] := color.b;
    Bleu := (BB[1] + (BB[2] + BB[3] + BB[4])) div 4; (* Valeur moyenne *)
    Vert := (GG[1] + (GG[2] + GG[3] + GG[4])) div 4; (* en cours d'‚valuation        *)
    Rouge := (RR[1] + (RR[2] + RR[3] + RR[4])) div 4;
    color.r := rouge;
    color.g := vert;
    color.b := bleu;
    line := Bitmap.scanline[yk];
    line[xk * 3] := color.r;
    line[xk * 3 + 1] := color.g;
    line[xk * 3 + 2] := color.b;
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
    Line := Bitmap.scanline[Y];
    for i := XOrigin to XFinal do
    begin
      valueR := valueR + advalR;
      r := round(ValueR);
      if r > 255 then
        r := 255;
      if r < 0 then
        r := 0;
      valueG := valueG + advalG;
      g := round(ValueG);
      if g > 255 then
        g := 255;
      if g < 0 then
        g := 0;
      valueB := valueB + advalB;
      b := round(ValueB);
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
  Color1 := colortorgb(Color1);
  R1 := getrvalue(Color1);
  G1 := getgvalue(Color1);
  B1 := getbvalue(Color1);
  Color2 := colortorgb(Color2);
  R2 := getrvalue(Color2);
  G2 := getgvalue(Color2);
  B2 := getbvalue(Color2);
  horgradientline(Clip, X1, X2, Y, R1, G1, B1, R2, G2, B2, True);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawLighterCircle(X, Y, Mode: Integer);
var
  r: Integer;
begin
  r := Canvas.pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, point(X, Y), r, Mode);
  Picture.Bitmap.Assign(Clip);
end;

procedure TJvDrawImage.DrawDarkerCircle(X, Y, Mode: Integer);
var
  r: Integer;
begin
  r := Canvas.pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, point(X, Y), r, Mode);
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
  tm.Canvas.brush.color := clblack;
  tm.Canvas.Ellipse(0, 0, tm.Width - 1, tm.Height - 1);
  tm.transparent := True;
  tm.TransparentColor := clblack;
  Rd := rect(0, 0, cm.Width, cm.Height);
  Rs := rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  cm.Canvas.CopyRect(Rd, bm.Canvas, RS);
  p0 := nil;
  p1 := nil;
  for j := 0 to cm.Height - 1 do
  begin
    p := cm.scanline[j];
    if j > 0 then
      p0 := cm.scanline[j - 1];
    if j < (h - 1) then
      p1 := cm.scanline[j + 1];
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
            p[i * 3] := round(p[i * 3] * 10 / 11);
            p[i * 3 + 1] := round(p[i * 3 + 1] * 10 / 11);
            p[i * 3 + 2] := round(p[i * 3 + 2] * 10 / 11);
          end;
        10: // lighter
          begin
            p[i * 3] := round(p[i * 3] * 11 / 10);
            p[i * 3 + 1] := round(p[i * 3 + 1] * 11 / 10);
            p[i * 3 + 2] := round(p[i * 3 + 2] * 11 / 10);
          end;
        11: // gray
          begin
            sum := round((p[i * 3] + p[i * 3 + 1] + p[i * 3 + 2]) / 3);
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
              p[i * 3] := round((p[(i - 1) * 3] + p[(i + 1) * 3] + p0[i * 3] + p1[i * 3]) / 4);
              p[i * 3 + 1] := round((p[(i - 1) * 3 + 1] + p[(i + 1) * 3 + 1] + p0[i * 3 + 1] + p1[i * 3 + 1]) / 4);
              p[i * 3 + 2] := round((p[(i - 1) * 3 + 2] + p[(i + 1) * 3 + 2] + p0[i * 3 + 2] + p1[i * 3 + 2]) / 4);
            end;
          end;
      end;
    end;
  end;
  cm.Canvas.Draw(0, 0, tm);
  cm.transparent := True;
  cm.transparentcolor := clwhite;
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
  r := Canvas.pen.Width;
  if r < 5 then
    r := 5;
  ColorCircle(Clip, point(X, Y), r, Mode);
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
  Rclip := rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Src.Width := Rclip.right - Rclip.left;
  Src.Height := RClip.bottom - Rclip.top;
  Dst.Width := Src.Width;
  Dst.Height := Src.Height;
  Rsrc := rect(0, 0, Src.Width, Src.Height);
  Dst.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  case Style of
    lbBrightness: FX.lightness(Dst, Amount);
    lbSaturation: FX.saturation(Dst, Amount);
    lbContrast: FX.contrast(Dst, Amount);
  end;
  // mask code
  Src.Canvas.Brush.color := clwhite;
  Src.Canvas.FillRect(Rsrc);
  Src.Canvas.brush.Style := bssolid;
  Src.Canvas.Brush.color := clblack;
  Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
  Src.Transparent := True;
  Src.TransparentColor := clblack;
  Dst.Canvas.Draw(0, 0, Src);
  Dst.transparent := True;
  Dst.TransparentColor := clwhite;
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
  Rclip := rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Dst.Width := Rclip.right - Rclip.left;
  Dst.Height := RClip.bottom - Rclip.top;
  // now Change to Reduce
  Amount := abs(Amount);
  if Amount < 1 then
    Amount := 1;
  dr := round(Radius * Amount / 180);
  if dr < 5 then
    dr := 5;
  if dr > Radius then
    dr := Radius;
  //(mbVerBox,mbHorBox,mbVerOval,mbHorOval);
  case Style of
    mbVerOval, mbVerbox: Rclip := rect(X - Radius, Y - dr, X + Radius, Y + dr);
    mbHorOval, mbHorBox: Rclip := rect(X - dr, Y - Radius, X + dr, Y + Radius);
  end;
  Src.Width := Rclip.right - Rclip.left;
  Src.Height := RClip.bottom - Rclip.top;
  Rsrc := rect(0, 0, Src.Width, Src.Height);
  Src.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  SampleStretch(Src, Dst);
  // mask code
  // reset Src dimensions for masking
  if Style in [mbHorOval, mbVerOval] then
  begin
    Src.Width := Dst.Width;
    Src.Height := Dst.Height;
    Src.Canvas.Brush.color := clwhite;
    Src.Canvas.FillRect(Rsrc);
    Src.Canvas.brush.Style := bssolid;
    Src.Canvas.Brush.color := clblack;
    Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
    Src.Transparent := True;
    Src.TransparentColor := clblack;
    Dst.Canvas.Draw(0, 0, Src);
    Dst.transparent := True;
    Dst.TransparentColor := clwhite;
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
      r := sqrt(sqr(dx) + sqr(dy));
      sr := fr * sin(r / cx * Amount * 2 * pi);
      if (r + sr < cx) and (r + sr > 0) then
      begin
        a := arctan2(dy, dx);
        sincos(a, sa, ca);
        i := cx + round((r + sr) * ca);
        j := cy + round((r + sr) * sa);
        p2 := Dst.scanline[j];
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
  Rclip := rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
  Src.Width := Rclip.right - Rclip.left;
  Src.Height := RClip.bottom - Rclip.top;
  Dst.Width := Src.Width;
  Dst.Height := Src.Height;
  Rsrc := rect(0, 0, Src.Width, Src.Height);
  Src.Canvas.CopyRect(Rsrc, Clip.Canvas, Rclip);
  case Style of
    lbfisheye: FX.fisheye(Src, Dst, Amount);
    lbrotate: FX.smoothrotate(Src, Dst, Src.Width div 2, Src.Height div 2, Amount);
    lbtwist: FX.twist(Src, Dst, round(Amount));
    lbrimple: Rimple(Src, Dst, Amount);
    mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound, mbRound2:
      FX.SqueezeHor(Src, Dst, round(Amount), Style);
    mbSplitRound, mbSplitWaste:
      FX.SplitRound(Src, Dst, round(Amount), Style);
  end;
  // mask code
  Src.Canvas.Brush.color := clwhite;
  Src.Canvas.FillRect(Rsrc);
  Src.Canvas.brush.Style := bssolid;
  Src.Canvas.Brush.color := clblack;
  Src.Canvas.Ellipse(0, 0, Src.Width - 1, Src.Height - 1);
  Src.Transparent := True;
  Src.TransparentColor := clblack;
  Dst.Canvas.Draw(0, 0, Src);
  Dst.transparent := True;
  Dst.TransparentColor := clwhite;
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
  ra := round(Amount);
  zoomrect := rect(X - ra, Y - ra, X + ra, Y + ra);
  if zoomrect.left < 0 then
    zoomrect.left := 0;
  if zoomrect.top < 0 then
    zoomrect.top := 0;
  if zoomrect.right > (FZoomClip.Width - 1) then
    zoomrect.right := FZoomClip.Width - 1;
  if zoomrect.bottom > (FZoomClip.Height - 1) then
    zoomrect.bottom := FZoomClip.Height - 1;
  w := zoomrect.right - zoomrect.left + 1;
  h := zoomrect.bottom - zoomrect.top + 1;
  Src.Width := w;
  Src.Height := h;
  Src.PixelFormat := pf24bit;
  Rs := rect(0, 0, w, h);
  Src.Canvas.CopyRect(Rs, FZoomClip.Canvas, zoomrect);
  Canvas.stretchDraw(rect(0, 0, FZoomClip.Width, FZoomClip.Height), Src);
  Src.Free;
end;

function TJvDrawImage.Rotate(Origin, Endpoint: TPoint; Angle: Real): TPoint;
var
  a, d, r: Real;
begin
  r := sqrt(sqr(Endpoint.X - Origin.X) + sqr(Endpoint.Y - Origin.Y));
  d := Endpoint.X - Origin.X;
  if (d >= 0) and (d < 0.001) then
    d := 0.001;
  if (d < 0) and (d > -0.001) then
    d := -0.001;
  a := arctan2((Endpoint.Y - Origin.Y), d);
  a := a + Angle;
  result.X := Origin.X + Variant(r * cos(a));
  result.Y := Origin.Y + Variant(r * sin(a));
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
  apoint := point(X, Y);
  pointarray[0] := apoint;
  for i := 1 to StarPoints - 1 do
  begin
    apoint := Rotate(point(X0, Y0), apoint, da);
    pointarray[i] := apoint;
  end;
end;

function TJvDrawImage.GetBlue(AColor: TColor): Byte;
begin
  result := GetBValue(ColorToRGB(AColor));
end;

function TJvDrawImage.GetGreen(AColor: TColor): Byte;
begin
  result := GetGValue(ColorToRGB(AColor));
end;

function TJvDrawImage.GetRed(AColor: TColor): Byte;
begin
  result := GetRValue(ColorToRGB(AColor));
end;

function TJvDrawImage.MixColors(Color1, Color2: TColor): TColor;
var
  R1, G1, B1: Byte;
begin
  Color1 := colortorgb(Color1);
  Color2 := colortorgb(Color2);
  R1 := (getRed(Color1) + getRed(Color2)) div 2;
  G1 := (getGreen(Color1) + getGreen(Color2)) div 2;
  B1 := (getBlue(Color1) + getBlue(Color2)) div 2;
  result := rgb(R1, G1, B1);
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
  Clip.Width := m.right - m.left + 1;
  Clip.Height := m.bottom - m.top + 1;
  dest := rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.CopyMode := clipcm;
  Clip.Canvas.CopyRect(dest, Canvas, m);
  Clip.pixelformat := pf24bit;
end;

procedure TJvDrawImage.ClipAll;
begin
  mycliprect := rect(0, 0, Picture.Bitmap.Width - 1, Picture.Bitmap.Height - 1);
  clipcm := cmsrccopy;
  SetClip(clwhite);
  CopyClip;
end;

procedure TJvDrawImage.SetClip(AColor: TColor);
var
  m, dest: TRect;
begin
  m := mycliprect;
  Clip.Width := (m.right - m.left) + 1;
  Clip.Height := (m.bottom - m.top) + 1;
  dest := rect(0, 0, Clip.Width, Clip.Height);
  Clip.Canvas.brush.color := AColor;
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
    myorigin := point(X, Y);
    myprevpoint := myorigin;
    if ssalt in Shift then
      myDraw := False;
  end;

  Canvas.pen.Mode := mypen;
  TargetPoint := point(X, Y);
end;

procedure TJvDrawImage.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Wavepen := Canvas.pen.color;
  Wavebrush := Canvas.brush.color;
  if button = mbright then
  begin
    EscapePaint(X, Y, Shift);
    exit;
  end;
  if ((ssctrl in Shift) and (ssshift in Shift)) then
  begin
    X := targetpoint.X;
    Y := targetpoint.Y;
    mouse.CursorPos := clienttoscreen(point(X, Y));
  end;
  Canvas.MoveTo(X, Y);
  myorigin := point(X, Y);
  myprevpoint := myorigin;
  myslinedir := 'none';
  myDraw := True;
  mypen := Canvas.Pen.Mode;
  if (Shape = 'rangemove') or (Shape = 'rangesmear') then
  begin
    clipcm := cmSrcInvert;
    SetClip(clwhite);
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
      if ssalt in Shift then
        pen.color := MixColors(pixels[X, Y - pen.Width], pixels[X, Y + pen.Width])
      else
        pen.color := MixColors(pixels[X - pen.Width, Y], pixels[X + pen.Width, Y]);
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
    myoldbrushstyle := Canvas.brush.Style;
    Canvas.brush.Style := bsclear;
    myoldpenwidth := Canvas.Pen.Width;
    Canvas.Pen.Width := 1;
  end;
  if (Shape = 'bezier1') then
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      polybezier(mybezier);
      mybezier[1] := point(X, Y);
      polybezier(mybezier);
    end;
  if (Shape = 'bezier2') then
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      polybezier(mybezier);
      mybezier[2] := point(X, Y);
      polybezier(mybezier);
    end;
  Canvas.pen.Mode := mypen;
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
    result := round(sqrt(sqr(X - myorigin.X) + sqr(Y - myorigin.Y)));
    if result < 10 then
      result := 10;
  end;

  procedure moveorigin;
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
  if ((ssctrl in Shift) and (ssalt in Shift)) then
    exit;
  mypen := Canvas.pen.Mode;
  h := abs(Y - myorigin.Y);
//  w := abs(X - myorigin.X);
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
        myprevpoint := point(X, Y);
      end;
    end;
    if Shape = 'airbrush' then
      if AirBrush.Air then
        AirBrush.Draw(Canvas, X, Y);
    if (Shape = 'zoombrush') or (Shape = 'zoomkeepbrush') then
    begin
      w := Canvas.pen.Width;
      if w < 5 then
        w := 50;
      DrawPlasma(X, Y, w);
    end;
    if Shape = 'fisheyebrush' then
    begin
      w := Canvas.pen.Width;
      if w < 5 then
        w := 50;
      DrawEffectBrush(X, Y, w, 0.9, lbfisheye);
    end;
    if Shape = 'fisheyefixbrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) / pi * 0.5 + 0.5;
      if Angle < 0.55 then
        Angle := 0.55;
      if Angle > 0.99 then
        Angle := 0.99;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbfisheye);
    end;
    if Shape = 'twistbrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := abs(arctan2(dy, dx) * 25 / pi) + 1;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, round(Angle), lbtwist);
    end;
    if Shape = 'mbHorOval' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbHorOval);
    end;
    if Shape = 'mbHorBox' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbHorBox);
    end;

    if Shape = 'mbVerOval' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbVerOval);
    end;

    if Shape = 'mbVerBox' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.X, myorigin.Y, rr, Angle, mbVerBox);
    end;

    if Shape = 'mbHor' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbHor);
    end;

    if Shape = 'mbTop' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbTop);
    end;

    if Shape = 'mbBottom' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbBottom);
    end;
    if Shape = 'mbWaste' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbWaste);
    end;
    if Shape = 'mbRound' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbRound);
    end;
    if Shape = 'mbRound2' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbRound2);
    end;

    if Shape = 'mbDiamond' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbDiamond);
    end;
    if Shape = 'mbSplitRound' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbSplitRound);
    end;
    if Shape = 'mbSplitWaste' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, mbSplitWaste);
    end;

    if Shape = 'rimplebrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 10 / pi + 1;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbrimple);
    end;

    if Shape = 'rotatebrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.X, myorigin.Y, rr, Angle, lbrotate);
    end;
    if Shape = 'brightnessbrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, round(Angle), lbBrightness);
    end;
    if Shape = 'contrastbrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, round(Angle), lbContrast);
    end;
    if Shape = 'saturationbrush' then
    begin
      if ssalt in Shift then
        moveorigin;
      dx := X - myorigin.X;
      if dx = 0 then
        dx := 0.01;
      dy := Y - myorigin.Y;
      Angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.X, myorigin.Y,
        rr, round(Angle), lbContrast);
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
        if ssalt in Shift then
        begin
          Color1 := pixels[X, Y - pen.Width];
          Color2 := pixels[X, Y + pen.Width];
          DrawVGradientBrush(Color1, Color2, Y - pen.Width, Y + pen.Width, X);
          DrawVGradientBrush(Color1, Color2, Y - pen.Width, Y + pen.Width, X - 1);
          DrawVGradientBrush(Color1, Color2, Y - pen.Width, Y + pen.Width, X - 2);
        end
        else
        begin
          Color1 := pixels[X - pen.Width, Y];
          Color2 := pixels[X + pen.Width, Y];
          DrawGradientBrush(Color1, Color2, X - pen.Width, X + pen.Width, Y);
          DrawGradientBrush(Color1, Color2, X - pen.Width, X + pen.Width, Y + 1);
          DrawGradientBrush(Color1, Color2, X - pen.Width, X + pen.Width, Y + 2);
        end;
      end;
    end;

    if Shape = 'cube1' then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        DrawCube;
        myskew[4] := point(X, Y);
        DrawCube;
      end;
    if (Shape = 'rectangle') or (Shape = 'cube') or (Shape = 'maze')
      or (Shape = 'Interprect') or (Shape = 'interColumn') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        rectangle(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        rectangle(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := point(X, Y);
      end;

    if Shape = 'roundrect' then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        roundrect(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y, myround, myround);
        roundrect(myorigin.X, myorigin.Y, X, Y, myround, myround);
        myprevpoint := point(X, Y);
      end;
    if Shape = 'Blocks' then
      Canvas.FillRect(PointToBlock(X, Y));

    if (Shape = 'ellipse') or (Shape = 'globe') or (Shape = 'interSphere')
      or (Shape = 'MultiSphere') or (Shape = 'DropletSphere')
      or (Shape = 'WaveSphere') or (Shape = 'RisingWaveSphere')
      or (Shape = 'decooval') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        ellipse(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := point(X, Y);
      end;
    if (Shape = 'chord') or (Shape = 'pie') or (Shape = 'arc') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        ellipse(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := point(X, Y);
      end;
    if Shape = 'skewrect1' then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        DrawSkew;
        myskew[2] := point(X, Y);
        myskew[3].X := myskew[0].X + (myskew[2].X - myskew[1].X);
        myskew[3].Y := myskew[0].Y + (myskew[2].Y - myskew[1].Y);
        DrawSkew;
      end;

    if Shape = 'triangle1' then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        DrawTriangle;
        myskew[2] := point(X, Y);
        DrawTriangle;
      end;
    if (Shape = 'polyline') or (Shape = 'Polygon') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        penpos := point(myorigin.X, myorigin.Y);
        lineto(myprevpoint.X, myprevpoint.Y);
        penpos := point(myorigin.X, myorigin.Y);
        lineto(X, Y);
        myprevpoint := point(X, Y);
      end;

    if Shape = 'polybezier' then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        mybezier[0] := point(myorigin.X, myorigin.Y);
        mybezier[3] := point(myprevpoint.X, myprevpoint.Y);
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
        polybezier(mybezier);
        mybezier[3] := point(X, Y);
        if (ssctrl in Shift) then
        begin
          bezierfix1 := True;
          mybezier[1] := mybezier[3];
        end;
        if not bezierfix1 then
        begin
          mybezier[1].X := mybezier[0].X;
          mybezier[1].Y := mybezier[3].Y;
        end;
        if (ssshift in Shift) then
        begin
          bezierfix2 := True;
          mybezier[2] := mybezier[3];
        end;
        if not bezierfix2 then
        begin
          mybezier[2].X := mybezier[3].X;
          mybezier[2].Y := mybezier[0].Y;
        end;

        polybezier(mybezier);
        myprevpoint := point(X, Y);
      end;
    if (Shape = 'line') or (Shape = 'rotateText') or (Shape = 'Star')
      or (Shape = 'spiral') or (Shape = 'skewrect') or (Shape = 'triangle')
      or (Shape = 'cone') or (Shape = 'Spiro') or
      (Shape = 'decobar') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        penpos := point(myorigin.X, myorigin.Y);
        lineto(myprevpoint.X, myprevpoint.Y);
        penpos := point(myorigin.X, myorigin.Y);
        lineto(X, Y);
        myprevpoint := point(X, Y);
      end;
    if (Shape = 'bezier') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        mybezier[0] := point(myorigin.X, myorigin.Y);
        mybezier[1] := mybezier[0];
        mybezier[3] := point(myprevpoint.X, myprevpoint.Y);
        mybezier[2] := mybezier[3];
        polybezier(mybezier);
        mybezier[3] := point(X, Y);
        mybezier[2] := mybezier[3];
        polybezier(mybezier);
        myprevpoint := point(X, Y);
      end;
    if (Shape = 'bezier1') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        polybezier(mybezier);
        mybezier[1] := point(X, Y);
        polybezier(mybezier);
      end;
    if (Shape = 'bezier2') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        polybezier(mybezier);
        mybezier[2] := point(X, Y);
        polybezier(mybezier);
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
        Canvas.lineto(X, Y);
        myprevpoint := point(X, Y);
      end;

    if Shape = 'borderWaveline' then
    begin
      Canvas.moveto(myprevpoint.X, myprevpoint.Y);
      Canvas.lineto(X, Y);
      Canvas.MoveTo(Width - myprevpoint.X, Height - myprevpoint.Y);
      Canvas.lineto(Width - X, Height - Y);
      myprevpoint := point(X, Y);
    end;

    if Shape = 'decoline' then
    begin
      with Canvas do
      begin
        pw := pen.Width;
        pen.color := Wavepen;
        pen.Mode := pmcopy;
        moveto(myprevpoint.X, myprevpoint.Y);
        lineto(X, Y);
        pen.Width := pw * 2;
        pen.Mode := pmmasknotpen;
        MoveTo(myprevpoint.X, myprevpoint.Y);
        lineto(X, Y);
        myprevpoint := point(X, Y);
        pen.Width := pw;
        pen.Mode := pmcopy;
        pen.Mode := mypen;
      end;
    end;

    if (Shape = 'freehand') or (Shape = 'mixbrush') then
      with Canvas do
      begin
        Canvas.lineto(X, Y);
        myprevpoint := point(X, Y)
      end;
    if Shape = 'cloneall' then
      with Canvas do
      begin
        X1 := myorigin.X - TargetPoint.X;
        Y1 := myorigin.Y - Targetpoint.Y;
        X2 := X - X1;
        Y2 := Y - Y1;
        copymode := cmsrccopy;
        i := pen.Width;
        copyrect(rect(X, Y, X + i, Y + i), Canvas,
          rect(X2, Y2, X2 + i, Y2 + i));
      end;

    if Shape = 'clonenottarget' then
      with Canvas do
      begin
        X1 := myorigin.X - TargetPoint.X;
        Y1 := myorigin.Y - Targetpoint.Y;
        X2 := X - X1;
        Y2 := Y - Y1;
        i := pen.Width;
        PutClip(rect(X2, Y2, X2 + i, Y2 + i));
        Clip.transparent := True;
        Clip.TransparentColor := pixels[Targetpoint.X, Targetpoint.Y];
        Draw(X, Y, Clip);
        Clip.transparent := False;
      end;

    if (Shape = 'paste') and (ssshift in Shift) then
    begin
      myrect := rect(0, 0, 0, 0);
      myrect.left := X;
      myrect.top := Y;
      myrect.right := X + mycliprect.right - mycliprect.left;
      myrect.bottom := Y + mycliprect.bottom - mycliprect.Top;
      Canvas.CopyRect(myrect, Canvas, mycliprect);
    end;
    if Shape = 'sym' then
      DrawSyms(X, Y);
    if Shape = 'sline' then
    begin
      if myslinedir = 'none' then
        if abs(X - myorigin.X) >= abs(Y - myorigin.Y) then
          myslinedir := 'h'
        else
          myslinedir := 'v';
      if (myslinedir = 'h') and (abs(Y - myprevpoint.Y) > myslinetol) then
        myslinedir := 'v';
      if (myslinedir = 'v') and (abs(X - myprevpoint.X) > myslinetol) then
        myslinedir := 'h';
      if myslinedir = 'h' then
      begin
        Canvas.lineto(X, myprevpoint.Y);
        myprevpoint.X := X;
      end;
      if myslinedir = 'v' then
      begin
        Canvas.lineto(myprevpoint.X, Y);
        myprevpoint.Y := Y;
      end;
    end;

    if Shape = 'vmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
//      Y2 := Height;
      Canvas.PenPos := point(X2 - X1, Y1);
      Canvas.lineto(X2 - X, Y);
      Canvas.penpos := point(X1, Y1);
      Canvas.lineto(X, Y);
      myprevpoint := point(X, Y)
    end;
    if Shape = 'cmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := point(X2 - X1, Y2 - Y1);
      Canvas.lineto(X2 - X, Y2 - Y);
      Canvas.penpos := point(X1, Y1);
      Canvas.lineto(X, Y);
      myprevpoint := point(X, Y)
    end;
    if Shape = 'mirror4' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := point(X2 - X1, Y2 - Y1);
      Canvas.lineto(X2 - X, Y2 - Y);
      Canvas.PenPos := point(X2 - X1, Y1);
      Canvas.lineto(X2 - X, Y);
      Canvas.PenPos := point(X1, Y2 - Y1);
      Canvas.lineto(X, Y2 - Y);
      Canvas.penpos := point(X1, Y1);
      Canvas.lineto(X, Y);
      myprevpoint := point(X, Y)
    end;
    if Shape = 'hmirror' then
    begin
      X1 := myprevpoint.X;
      Y1 := myprevpoint.Y;
//      X2 := Width;
      Y2 := Height;
      Canvas.PenPos := point(X1, Y2 - Y1);
      Canvas.lineto(X, Y2 - Y);
      Canvas.penpos := point(X1, Y1);
      Canvas.lineto(X, Y);
      myprevpoint := point(X, Y)
    end;
    if (Shape = 'snapshot') or (Shape = 'bars') or (Shape = 'border') then
      with Canvas do
      begin
        pen.Mode := pmnotxor;
        pen.Style := psdot;
        rectangle(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
        rectangle(myorigin.X, myorigin.Y, X, Y);
        myprevpoint := point(X, Y);
        pen.Style := pssolid;
      end;
  end;
  Canvas.pen.Mode := mypen;
  myprevpoint := point(X, Y);
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
  //Canvas.pen.color:=Wavepen;
  //Canvas.brush.color:=Wavebrush;

  if ((ssctrl in Shift) and (ssalt in Shift)) then
    exit;
  if button = mbright then
    exit;
  mypen := Canvas.pen.Mode;

  if Shape = 'zoombrush' then
    Canvas.Draw(0, 0, FZoomClip);
  if Shape = 'transcopy' then
  begin
    clipcm := cmSrcCopy;
    SetClip(clwhite);
    CopyClip;
    myrect := rect(X, Y, X + Clip.Width - 1, Y + Clip.Height - 1);
    {$IFDEF VCL}
    Canvas.brushcopy(myrect, Clip,
      rect(0, 0, Clip.Width, Clip.Height), RangeTransColor);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Clip.Transparent := true;
    Clip.TransparentColor := RangeTransColor;
    Canvas.Draw(X,Y, Clip);
    {$ENDIF VisualCLX}
    myDraw := False;
  end;
  if Shape = 'cube1' then
  begin
    if mypen <> pmnotxor then
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
    myskew[2] := point(X, Y);
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
      if ssalt in Shift then
        DrawSphere(Canvas.pixels[myorigin.X, myorigin.Y],
          Canvas.pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;

  if Shape = 'MultiSphere' then
  begin
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.pen.Mode := pmcopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssalt in Shift then
        DrawMultiSphere(Canvas.pixels[myorigin.X, myorigin.Y],
          Canvas.pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawMultiSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'DropletSphere' then
  begin
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.pen.Mode := pmcopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssalt in Shift then
        DrawDropletSphere(Canvas.pixels[myorigin.X, myorigin.Y],
          Canvas.pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawDropletSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'WaveSphere' then
  begin
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.pen.Mode := pmcopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssalt in Shift then
        DrawWaveSphere(Canvas.pixels[myorigin.X, myorigin.Y],
          Canvas.pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawWaveSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'RisingWaveSphere' then
  begin
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    Canvas.pen.Mode := pmcopy;
    if ((myorigin.X <> X) and (myorigin.Y <> Y)) then
    begin
      if ssalt in Shift then
        DrawRisingWaveSphere(Canvas.pixels[myorigin.X, myorigin.Y],
          Canvas.pixels[X, Y], myorigin.X, myorigin.Y, X, Y)
      else
        DrawRisingWaveSphere(Wavepen, Wavebrush, myorigin.X, myorigin.Y, X, Y)
    end;
  end;

  if Shape = 'rectangle' then
  begin
    if mypen <> pmnotxor then
      Canvas.rectangle(myorigin.X, myorigin.Y, X, Y);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.rectangle(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys);
      end;
    end;
  end;

  if Shape = 'maze' then
  begin
    if mypen <> pmnotxor then
      Canvas.rectangle(myorigin.X, myorigin.Y, X, Y);
    xs := (X - myorigin.X) div 10;
    ys := (Y - myorigin.Y) div 10;
    xt := myorigin.X;
    yt := myorigin.Y;
    for i := 1 to 10 do
    begin
      Canvas.MoveTo(xt + i * xs, Y);
      Canvas.lineto(X, Y - i * ys);
      Canvas.MoveTo(X, Y - i * ys);
      Canvas.lineto(X - i * xs, yt);
      Canvas.MoveTo(X - i * xs, yt);
      Canvas.lineto(xt, yt + i * ys);
      Canvas.MoveTo(xt, yt + i * ys);
      Canvas.lineto(xt + i * xs, Y);
    end;
  end;

  if Shape = 'roundrect' then
  begin
    if mypen <> pmnotxor then
      Canvas.roundrect(myorigin.X, myorigin.Y, X, Y, myround, myround);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.roundrect(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys, myround, myround);
      end;
    end;
  end;

  if Shape = 'Blocks' then
    Canvas.FillRect(PointToBlock(X, Y));

  if Shape = 'Star' then
  begin
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      penpos := point(myorigin.X, myorigin.Y);
      lineto(myprevpoint.X, myprevpoint.Y);
    end;
    Canvas.pen.Mode := mypen;
    for i := 1 to Stars do
    begin
      apoint := ReduceVector(myorigin, point(X, Y), i / Stars);
      Star(apoint.X, apoint.Y);
    end;
  end;

  if Shape = 'spiral' then
  begin
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      penpos := point(myorigin.X, myorigin.Y);
      lineto(myprevpoint.X, myprevpoint.Y);
    end;
    Canvas.pen.Mode := mypen;
    apoint := point(100 * X, 100 * Y);
    myorigin.X := 100 * myorigin.X;
    myorigin.Y := 100 * myorigin.Y;
    for i := 1 to Variant(Spirals * 36) do
    begin
      apoint := Rotate(myorigin, apoint, spiraldir * pi / 18);
      apoint := ReduceVector(myorigin, apoint, spiralfactor);
      Canvas.lineto(apoint.X div 100, apoint.Y div 100);
    end;
  end;

  if Shape = 'ellipse' then
  begin
    if mypen <> pmnotxor then
      Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    if Stars > 1 then
    begin
      xs := (X - myorigin.X) div 2 div Stars;
      ys := (Y - myorigin.Y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        Canvas.ellipse(myorigin.X + i * xs, myorigin.Y + i * ys, X - i * xs, Y - i * ys);
      end;
    end;
  end;

  if Shape = 'globe' then
  begin
    if mypen <> pmnotxor then
      Canvas.ellipse(myorigin.X, myorigin.Y, X, Y);
    xs := (X - myorigin.X) div 20;
    ys := (Y - myorigin.Y) div 20;
    for i := 1 to 10 do
    begin
      Canvas.ellipse(myorigin.X + i * xs, myorigin.Y, X - i * xs, Y);
      Canvas.ellipse(myorigin.X, myorigin.Y + i * ys, X, Y - i * ys);
    end;
  end;

  if Shape = 'chord2' then
  begin
    mychord[7] := X;
    mychord[8] := Y;
    Shape := 'chord3';
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.pen.Mode := mypen;
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
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.pen.Mode := mypen;
    Canvas.arc(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
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
    Canvas.pen.Mode := pmnotxor;
    Canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    Canvas.pen.Mode := mypen;
    Canvas.pie(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
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
    if mypen <> pmnotxor then
      DrawSkew;
    Shape := 'skewrect2';
  end;

  if Shape = 'skewrect' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    if mypen <> pmnotxor then
      Canvas.lineto(X, Y);
    myskew[0] := myorigin;
    myskew[1] := point(X, Y);
    myskew[2] := myskew[1];
    myskew[3] := myskew[0];
    Shape := 'skewrect1';
  end;

  if Shape = 'triangle1' then
  begin
    if mypen <> pmnotxor then
      DrawTriangle;
    Shape := 'triangle2';
  end;

  if Shape = 'triangle' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    if mypen <> pmnotxor then
      Canvas.lineto(X, Y);
    myskew[0] := myorigin;
    myskew[1] := point(X, Y);
    myskew[2] := myskew[1];
    Shape := 'triangle1';
  end;

  if Shape = 'decobar' then
  begin
    Picture.Bitmap.PixelFormat := pf24bit;
    with Canvas do
    begin
      pw := pen.Width;
      pcolor := pen.color;
      AColor := colortoRGB(Wavebrush);
      R1 := getRed(AColor);
      r := R1;
      G1 := getGreen(AColor);
      g := G1;
      B1 := getBlue(AColor);
      b := B1;
      AColor := colortoRGB(pen.color);
      R2 := getred(AColor);
      G2 := getGreen(AColor);
      B2 := getBlue(AColor);
      dr := (R1 - R2) / (pw / 3);
      dg := (G1 - G2) / (pw / 3);
      db := (B1 - B2) / (pw / 3);
      if pw < 30 then
        pen.Width := 30;
      for i := 1 to pen.Width div 3 do
      begin
        r := r - dr;
        g := g - dg;
        b := b - db;
        pen.color := rgb(round(r), round(g), round(b));
        moveto(myorigin.X, myorigin.Y);
        lineto(X, Y);
        pen.Width := pen.Width - 2;
      end;
      pen.Width := pw;
      pen.color := pcolor;
    end;
  end;

  if Shape = 'decooval' then
  begin
    Picture.Bitmap.PixelFormat := pf24bit;
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      ellipse(myorigin.X, myorigin.Y, myprevpoint.X, myprevpoint.Y);
      pen.Mode := pmcopy;
      pw := pen.Width;
      brush.Style := bsclear;
      AColor := colortoRGB(Wavebrush);
      R1 := getred(AColor);
      r := R1;
      G1 := getGreen(AColor);
      g := G1;
      B1 := getBlue(AColor);
      b := B1;
      AColor := colortoRGB(Wavepen);
      R2 := getred(AColor);
      G2 := getGreen(AColor);
      B2 := getBlue(AColor);
      dr := (R1 - R2) / (pw / 3);
      dg := (G1 - G2) / (pw / 3);
      db := (B1 - B2) / (pw / 3);
      if pw < 30 then
        pen.Width := 30;
      for i := 1 to pen.Width div 3 do
      begin
        pen.Width := pen.Width - 2;
        r := r - dr;
        g := g - dg;
        b := b - db;
        pen.color := rgb(round(r), round(g), round(b));
        ellipse(myorigin.X, myorigin.Y, X, Y);
      end;
      pen.Width := pw;
    end;
  end;

  if (Shape = 'polyline') or (Shape = 'Polygon') then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    if mypen <> pmnotxor then
      Canvas.lineto(X, Y);
    if freepolycount = 0 then
    begin
      freepoly[0] := myorigin;
      inc(freepolycount);
    end
    else
    begin
      freepoly[freepolycount] := point(X, Y);
      if freepolycount < 100 then
        inc(freepolycount);
    end;
  end;

  if Shape = 'line' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    if mypen <> pmnotxor then
    begin
      Canvas.lineto(X, Y);
    end;
  end;

  if Shape = 'Spiro' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    Canvas.pen.Mode := pmnotxor;
    Canvas.lineto(X, Y);
    Canvas.pen.Mode := mypen;
    DrawSpiro(myorigin, point(X, Y));
  end;

  if Shape = 'cone' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    Canvas.pen.Mode := pmnotxor;
    Canvas.lineto(X, Y);
    Canvas.pen.Mode := mypen;
    xt := (Picture.Bitmap.Width - 2 * myorigin.X) div 20;
    xs := (Picture.Bitmap.Width - 2 * X) div 20;
    X := Picture.Bitmap.Width div 2;
    with Canvas do
    begin
      for i := 0 to 10 do
      begin
        moveto(X + i * xt, myorigin.Y);
        lineto(X + i * xs, Y);
        moveto(X - i * xt, myorigin.Y);
        lineto(X - i * xs, Y);
      end;
      moveto(X + 10 * xt, myorigin.Y);
      lineto(X - 10 * xt, myorigin.Y);
      moveto(X + 10 * xs, Y);
      lineto(X - 10 * xs, Y);
    end;
  end;

  {if Shape='polybezier' then
   begin
    Canvas.penpos:=point(myorigin.X,myorigin.Y);
     if mypen<>pmnotxor then
       begin
       mybezier[0]:=myorigin;
       mybezier[3]:=point(X,Y);
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
       Canvas.polybezier(mybezier);
       bezierfix1:=False;
       bezierfix2:=False;
       end;
    end;}

  if Shape = 'bezier2' then
  begin
    Canvas.penpos := point(myorigin.X, myorigin.Y);
    if mypen <> pmnotxor then
    begin
      mybezier[2] := point(X, Y);
      Canvas.polybezier(mybezier);
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
    if ssalt in Shift then
      Canvas.floodfill(X, Y, Canvas.pen.color, fsborder)
    else
      Canvas.floodfill(X, Y, Canvas.pixels[X, Y], fssurface);
  end;
  {$ENDIF VCL}
  if Shape = 'snapshot' then
  begin
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      pen.Style := psdot;
      rectangle(myorigin.X, myorigin.Y, X, Y);
      pen.Style := pssolid;
    end;
    mycliprect := rect(myorigin.X, myorigin.Y, X, Y);
    Canvas.brush.Style := myoldbrushstyle;
    Canvas.Pen.Width := myoldpenwidth;
    Shape := '';
  end;

  if Shape = 'bars' then
  begin
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      pen.Style := psdot;
      rectangle(myorigin.X, myorigin.Y, X, Y);
      pen.Style := pssolid;
    end;
    DrawBars(myorigin.X, myorigin.Y, X, Y);
  end;

  if Shape = 'border' then
  begin
    with Canvas do
    begin
      pen.Mode := pmnotxor;
      pen.Style := psdot;
      rectangle(myorigin.X, myorigin.Y, X, Y);
      pen.Style := pssolid;
    end;
    Drawborders(myorigin.X, myorigin.Y, X, Y);
  end;

  if Shape = 'paste' then
  begin
    myrect := rect(0, 0, 0, 0);
    myrect.left := X;
    myrect.top := Y;
    myrect.right := X + mycliprect.right - mycliprect.left;
    myrect.bottom := Y + mycliprect.bottom - mycliprect.Top;
    Canvas.CopyRect(myrect, Canvas, mycliprect);
  end;

  if Shape = 'pastecolor' then
  begin
    clipcm := cmsrccopy;
    SetClip(clwhite);
    CopyClip;
    FX.ExtractColor(Clip, Canvas.brush.color);
    Canvas.Draw(X, Y, Clip);
    Clip.transparent := False;
  end;

  if Shape = 'pastecolorx' then
  begin
    clipcm := cmsrccopy;
    SetClip(clwhite);
    CopyClip;
    FX.ExcludeColor(Clip, Canvas.brush.color);
    Canvas.Draw(X, Y, Clip);
    Clip.transparent := False;
  end;

  if Shape = 'zoomkeepbrush' then
  begin
    Shape := 'freehand';
    Canvas.pen.Width := 2;
  end;

  if Shape = 'paintpick' then
  begin
    if ssalt in Shift then
      Canvas.brush.color := MixColors(
        Canvas.pixels[X - 5, Y],
        Canvas.pixels[X + 5, Y])
    else
      Canvas.brush.color := Canvas.pixels[X, Y];
    ColorPicked(Canvas.brush.color);
    Shape := '';
  end;
  Canvas.pen.Mode := mypen;
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
  Canvas.Brush.color := clwhite;
  Picture.Bitmap.Canvas.fillrect(rect(0, 0, Picture.Bitmap.Width, Picture.Bitmap.Height));
  Canvas.brush.Style := bsclear;
  Canvas.pen.color := clwhite;
  Canvas.moveto(100, 100);
  Canvas.lineto(128, 128);
  Canvas.pen.color := clblack;
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
  FX.contrast(Clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.saturationBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.saturation(Clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.lightnessBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.lightness(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.BlurBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.GaussianBlur(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.splitBlurBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.SplitBlur(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.colornoiseBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.AddColorNoise(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.mononoiseBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.AddmonoNoise(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.smoothBarChange(Sender: TObject);
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.Smooth(Clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.Effects;
begin
  with PainterEffectsF do
  begin
    cxbar.Max := Width;
    cybar.max := Height;
    cxbar.position := cxbar.max div 2;
    cybar.position := cybar.max div 2;
    show;
  end;
end;

procedure TJvDrawImage.seamBarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.MakeSeamlessClip(Clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.mosaicBarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.mosaic(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.twist(Clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position / 100;
  FX.Fisheye(Clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
  bm2.Free;
end;

procedure TJvDrawImage.WaveBarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.Wave(Clip, am, 0, 0);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.WaveExtraChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.EBar.position;
  FX.Wave(Clip, am, 0, 1);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.WaveInfChange;
var
  wa, inf: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  inf := painterEffectsF.Ebar.position;
  wa := paintereffectsF.extraBar.Position;
  FX.Wave(Clip, wa, inf, 2);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
    am := Ebar.position;
    dx := cxBar.position;
    dy := cyBar.position;
  end;
  FX.SmoothRotate(Clip, Dst, dx, dy, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
  Dst.Free;
end;

procedure TJvDrawImage.XFormABarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  XFormA(am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble2(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble3(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble4(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.EBar.position;
  FX.Marble5(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble6(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble7(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.Marble8(Clip, Dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
  Dst.Free;
end;

procedure TJvDrawImage.embossbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.Emboss(Clip);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.filterRedbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterRed(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.filterGreenbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterGreen(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.filterBluebarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.filterBlue(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.FilterXRedbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXRed(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.FilterXGreenbarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXGreen(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.FilterXBluebarChange;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  FX.FilterXBlue(Clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbHor);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbTop);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbBottom);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbDiamond);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbwaste);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbRound);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(Clip, bm2, am, mbround2);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SplitRound(Clip, bm2, am, mbSplitRound);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  FX.SplitRound(Clip, bm2, am, mbSplitWaste);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(bm2);
  QuickPreviewF.PreviewImage.update;
  bm2.Free;
end;

procedure TJvDrawImage.ShearbarChange;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  Shear(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  turb := painterEffectsF.extrabar.position;
  if turb < 10 then
  begin
    painterEffectsF.extrabar.position := 10;
    turb := 10;
  end;
  FX.Plasma(src1, src2, Clip, am, turb);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
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
  xr := painterEffectsF.Ebar.position * 0.028;
  yr := painterEffectsF.ExtraBar.Position * 0.009;
  X0 := -2.25 + xr;
  X1 := 0.75;
  Y0 := -1.5 + yr;
  Y1 := 1.5;
  FX.DrawMandelJulia(Clip, X0, Y0, X1, Y1, 16, Mandel);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.DrawTriangles;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.Triangles(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.RippleTooth;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleTooth(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.RippleTriangle;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleTooth(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.RippleRandom;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleRandom(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.TexturizeTile;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.TexturizeTile(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.TexturizeOverlap;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.TexturizeOverlap(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.DrawMap;
var
  am: Integer;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.HeightMap(Clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Clip);
  QuickPreviewF.PreviewImage.update;
end;

procedure TJvDrawImage.DrawBlend;
var
  am, w, h: Integer;
  src2, Dst: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  w := Clip.Width;
  h := Clip.Height;
  {$IFDEF VCL}
  if not clipboard.HasFormat(CF_BITMAP) then
    exit;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  if not clipboard.Provides('image/delphi.bitmap') then
    exit;
  {$ENDIF VisualCLX}
  src2 := TBitmap.Create;
  src2.Assign(clipboard);
  src2.PixelFormat := pf24bit;
  if ((src2.Width <> w) or (src2.Height <> h)) then
  begin
    src2.Free;
    exit;
  end;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Blend(Clip, src2, Dst, am / 100);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
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
  am := painterEffectsF.Ebar.position;
  w := Clip.Width;
  h := Clip.Height;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Solarize(Clip, Dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
  Dst.Free;
end;

procedure TJvDrawImage.Posterize;
var
  am, w, h: Integer;
  Dst: TBitmap;
begin
  ClipAll;
  Clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  w := Clip.Width;
  h := Clip.Height;
  Dst := TBitmap.Create;
  Dst.Width := w;
  Dst.Height := h;
  Dst.PixelFormat := pf24bit;
  FX.Posterize(Clip, Dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.PreviewImage.Picture.Bitmap.Assign(Dst);
  QuickPreviewF.PreviewImage.update;
  Dst.Free;
end;

procedure TJvDrawImage.Backgrounds;
begin
  PainterQBF.show;
  PainterQBF.BringToFront;
end;

procedure TJvDrawImage.Preview(ABitmap: TBitmap);
begin
  QuickPreviewF.show;
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
