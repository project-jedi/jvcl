{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDrawImage.PAS, released on 2002-06-15.

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
unit JvDrawImage;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  math,
  JvAirBrush,
  JvPaintFX,
  JvResample,
  Clipbrd;

type
  TSmartResizeMode = (rmWidth, rmHeight, rmSquare);
  TMorphBrush = (mbVerBox, mbHorBox, mbVerOval, mbHorOval);
  TDigitalFilter = array[0..4, 0..4] of SmallInt;
  TColorPicked = procedure(sender: Tobject; Acolor: TColor) of object;

  TJvDrawImage = class(TImage)
  private
    gonio: array[0..180, 0..1] of extended;
    sinpixs: array[0..255] of byte;
    myshape: string;
    Fshapes: TStringlist;
    zoomclip: tbitmap;
    FAirBrush: TJvAirBrush;
    Fpolygonchecked: boolean;
    FonColorPicked: TColorPicked;
    Fblocks: integer;
    Fstars: integer;
    Fstarpoints: integer;
    Fspirals: integer;
    procedure MyPaintESC(x, y: integer; shift: tshiftstate);
    procedure CopyClip;
    procedure SetClip(acolor: tcolor);
    procedure InitPlasma;
    function MixColors(color1, color2: tcolor): tcolor;
    function GetBlue(acolor: Tcolor): byte;
    function GetGreen(acolor: Tcolor): byte;
    function GetRed(acolor: Tcolor): byte;
    procedure SetSyms(x, y: integer);
    function Rotate(origin, endpoint: tpoint; angle: real): tpoint;
    procedure DrawPlasma(x, y: integer; amount: extended);
    procedure DrawEffectBrush(x, y, radius: integer; amount: extended;
      style: TLightbrush);
    procedure Rimple(src, dst: tbitmap; amount: extended);
    procedure DrawStretchBrush(x, y, radius: integer; amount: extended;
      style: Tmorphbrush);
    procedure SampleStretch(src, dst: tbitmap);
    procedure DrawLightBrush(x, y, radius, amount: integer;
      style: TLightBrush);
    procedure DrawColorCircle(x, y, mode: integer);
    procedure ColorCircle(var bm: TBitmap; center: tpoint; radius,
      mode: integer);
    procedure DrawDarkerCircle(x, y, mode: integer);
    procedure DrawLighterCircle(x, y, mode: integer);
    procedure DrawGradientBrush(color1, color2: tcolor; x1, x2,
      y: integer);
    procedure HorGradientLine(bitmap: tbitmap; XOrigin, XFinal, y: integer;
      r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
    procedure SmoothPnt(bitmap: tbitmap; xk, yk: integer);
    procedure DrawVGradientBrush(color1, color2: tcolor; y1, y2,
      x: integer);
    procedure VerGradientLine(bitmap: tbitmap; YOrigin, YFinal, x: integer;
      r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
    procedure DrawCube;
    function PointToBlock(x, y: integer): Trect;
    procedure DrawSkew;
    procedure DrawTriangle;
    procedure PutClip(M: Trect);
    procedure DrawSyms(x, y: integer);
    procedure DrawTexLines(x0, y0, x, y: integer);
    function BlendColors(const Color1, Color2: Integer;
      Opacity: integer): longInt;
    function TexHighlight(Colr: Integer): longInt;
    function TexShadow(Colr: Integer): longInt;
    procedure DrawTexOvals(x0, y0, x, y: integer);
    procedure DrawBlurOvals(x0, y0, x, y: integer);
    procedure DrawTexCurves(x0, y0, x, y: integer);
    procedure DrawBlurCurves(x0, y0, x, y: integer);
    procedure DrawTexPoly(x0, y0, x, y: integer);
    procedure DrawBlurPoly(x0, y0, x, y: integer);
    procedure DrawTexRects(x0, y0, x, y: integer);
    procedure DrawBlurRects(x0, y0, x, y: integer);
    procedure DrawBlurLines(x0, y0, x, y: integer);
    procedure InterpRect(x1, y1, x2, y2: integer);
    procedure InterpolateRect(Bmp: TBitmap; x1, y1, x2, y2: Integer);
    procedure DrawColumn(x1, y1, x2, y2: integer);
    procedure Column(bitmap: tbitmap; XOrigin, XFinal, YOrigin,
      YFinal: integer; r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
    procedure DrawSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
    procedure Sphere(bitmap: tbitmap; xcenter, a, ycenter, b: integer; r1,
      g1, b1, r2, g2, b2: byte; smooth: boolean);
    procedure DrawMultiSphere(color1, color2: tcolor; x1, y1, x2,
      y2: integer);
    procedure DrawDropletSphere(color1, color2: tcolor; x1, y1, x2,
      y2: integer);
    procedure DrawWaveSphere(color1, color2: tcolor; x1, y1, x2,
      y2: integer);
    procedure DrawRisingWaveSphere(color1, color2: tcolor; x1, y1, x2,
      y2: integer);
//    function GetAngle(origin, endpoint: tpoint): integer;
//    procedure TextRotate(x, y, angle: integer; aText: string; afont: tfont);
    function ReduceVector(origin, endpoint: tpoint; factor: real): tpoint;
    procedure Star(x, y: integer);
    procedure SetPolygonChecked(const Value: boolean);
    procedure DrawSpiro(center, radius: tpoint);
    procedure DrawBars(x1, y1, x2, y2: integer);
    procedure Drawborders(x1, y1, x2, y2: integer);
    procedure SetonColorPicked(const Value: TColorPicked);
    procedure SetShape(const Value: string);
    procedure SetAirBrush(const Value: TJvAirBrush);
    procedure SetTransformer(const Value: TJvPaintFX);
    procedure BuildShapeList;
    procedure SetBlocks(const Value: integer);
    procedure SetSpirals(const Value: integer);
    procedure SetStarpoints(const Value: integer);
    procedure SetStars(const Value: integer);
    procedure FillGonio;
    procedure FillSinPixs;
    procedure Shear(Abitmap: Tbitmap; amount: integer);
    procedure XFormA(amount: integer);
    { Private declarations }
  protected
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure ColorPicked(AColor: Tcolor);
    procedure Loaded; override;
  public
    { Public declarations }
    clip: TBitmap;
    TraceB: byte;
    FX: TJvPaintFX;
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
    procedure ClipAll;
    procedure Effects;
    procedure Backgrounds;
    procedure Preview(aBitmap: Tbitmap);
    procedure ApplyFilter(var Dst: TBitmap; DF: TDigitalFilter);
    procedure BlurBarChange(Sender: TObject);
    procedure ColorNoiseBarChange(Sender: TObject);
    procedure ContrastBarChange(Sender: TObject);
    procedure DrawBlend;
    procedure DrawMandelJulia(mandel: boolean);
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
    procedure FisheyeBarchange;
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
    property Shapes: TStringlist read FShapes;

  published
    { Published declarations }
    property Shape: string read myshape write SetShape;
    property PolygonChecked: boolean read FPolygonChecked write SetPolygonChecked;
    property OnColorPicked: TColorPicked read FOnColorPicked write SetOnColorPicked;
    //  Stars,Starpoints:integer;
    //  Blocks:integer;
    //  Spirals:integer;
    property Stars: integer read FStars write SetStars;
    property Starpoints: integer read FStarpoints write SetStarpoints;
    property Blocks: integer read FBlocks write SetBlocks;
    property Spirals: integer read FSpirals write SetSpirals;
  end;

implementation

uses
  JvTypes,
  JvPainterEffectsForm,
  JvQuickPreviewForm,
  JvPainterQBForm;

resourcestring
  sImageMustBeSquare = 'image must be square for Spirographs';
  sSumOfRadiTolarge = 'sum of radi to large';
  sBothRadiMustBeGr = 'both radi must be >%d';

const
  tab = chr(9);

  // Texture constants
  DarkStrength = 0.82;
  StrongBlend = 52;
  WeakBlend = 36;

  BlurFilter: TDigitalFilter = (
    (1, 1, 1, 1, 1),
    (1, 0, 0, 0, 1),
    (1, 0, 0, 0, 1),
    (1, 0, 0, 0, 1),
    (1, 1, 1, 1, 1));

type

  TFColor = record
    b, g, r: Byte;
  end;

var
  PainterEffectsF: TPainterEffectsF;
  QuickPreviewF: TQuickPreviewF;
  PainterQBF: TPainterQBF;

  mycliprect: trect;
  UserFilter: TDigitalFilter;

  RangeTransColor: tcolor;

  NSpiro: integer;
  Wavepen, Wavebrush: tcolor;
  decoX, decoY: integer;

  mybezier: array[0..3] of tpoint;
  myskew: array[0..4] of tpoint;
  mychord: array[1..8] of integer;
  myorigin, myprevpoint: tpoint;
  myslinedir: string;
  myslinetol: integer;
  myDraw: boolean;
  mypen: tpenmode;
  mypenstyle: tpenstyle;
  myoldbrushstyle: tbrushstyle;
  myoldpenwidth: integer;
  myround: integer;

  clipcm: Tcopymode;

  pointarray: array[0..12] of Tpoint;
  spiralfactor: real;
  spiraldir: integer;
  TargetPoint: Tpoint;
  zoomrect: trect;
  freepoly: array[0..100] of tpoint;
  freepolycount: integer;
  bezierfix1, bezierfix2: boolean;

function TrimInt(i, Min, Max: Integer): Integer;
begin
  if i > Max then
    Result := Max
  else if i < Min then
    Result := Min
  else
    Result := i;
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

// Start of filter procedures

procedure TJvDrawImage.FillGonio;
var
  a0: extended;
  i: integer;
begin
  a0 := pi / 180;
  for i := 0 to 180 do
    sincos(a0 * (i - 90), gonio[i, 0], gonio[i, 1]);
end;

procedure TJvDrawImage.FillSinPixs;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    sinpixs[i] := variant(sin(i / 255 * pi / 2) * 255);
  end;
end;

procedure TJvDrawImage.Shear(Abitmap: Tbitmap; amount: integer);
var
  bm: tbitmap;
  p1, p2: pbytearray;
  x, dx, y, h, w, c1, c2: integer;
  f: extended;
begin
  bm := tbitmap.create;
  h := Abitmap.height;
  w := abitmap.width;
  bm.width := w;
  bm.height := h;
  f := w / (w + (amount / 100) * h);
  bm.PixelFormat := pf24bit;
  Abitmap.PixelFormat := pf24bit;
  for y := 0 to h - 1 do
  begin
    p1 := Abitmap.ScanLine[y];
    p2 := bm.scanline[y];
    dx := round(amount / 100 * y);
    for x := 0 to w - 1 do
    begin
      c1 := x * 3;
      c2 := round(f * (x + dx)) * 3;
      p2[c2] := p1[c1];
      p2[c2 + 1] := p1[c1 + 1];
      p2[c2 + 2] := p1[c1 + 2];
    end;
  end;
  Abitmap.Assign(bm);
  bm.free;
end;

procedure TJvDrawImage.XFormA(amount: integer);
var
  x, y, i: integer;
  p1: pbytearray;
begin
  for i := 1 to amount do
    for y := 0 to clip.height - 1 do
    begin
      p1 := clip.scanline[y];
      for x := 0 to clip.width - 1 do
      begin
        p1[x * 3] := sinpixs[p1[x * 3]];
        p1[x * 3 + 1] := sinpixs[p1[x * 3 + 1]];
        p1[x * 3 + 2] := sinpixs[p1[x * 3 + 2]];
      end;
    end;
end;

procedure TJvDrawImage.Drawborders(x1, y1, x2, y2: integer);
var
  h, w: integer;
begin
  h := clientheight;
  w := clientwidth;
  Canvas.FillRect(rect(0, 0, w, y2 - y1));
  Canvas.FillRect(rect(0, h - (y2 - y1), w, h));
  Canvas.FillRect(rect(0, 0, x2 - x1, h));
  Canvas.FillRect(rect(w - (x2 - x1), 0, w, h));
end;

procedure TJvDrawImage.DrawBars(x1, y1, x2, y2: integer);
var
  h, w: integer;
begin
  h := clientheight;
  w := clientwidth;
  if y1 < 10 then y1 := 0;
  if y2 > (h - 10) then y2 := h;
  x1 := 0;
  x2 := w;
  Canvas.FillRect(rect(x1, y1, x2, y2));
end;

procedure TJvDrawImage.DrawSpiro(center, radius: tpoint);
var
  x0, x1, y0, y1, a0, a1, da0, da1: real;
  xs, ys, x, y, r0, r1: integer;
  i: integer;
begin
  xs := picture.Bitmap.Width div 2;
  ys := picture.Bitmap.height div 2;
  if xs <> ys then
  begin
    showmessage(sImageMustBeSquare);
    exit;
  end;
  r0 := variant(sqrt(sqr(center.x - xs) + sqr(center.y - ys)));
  r1 := variant(sqrt(sqr(radius.x - center.x) + sqr(radius.y - center.y)));
  if (r0 + r1) > xs then
  begin
    showmessage(sSumOfRadiTolarge);
    exit;
  end;
  if (r0 < 5) or (r1 < 5) then
  begin
    showmessage(Format(sBothRadiMustBeGr, [5]));
    exit;
  end;
  da1 := 2 * pi / 36;
  da0 := r1 / r0 * da1;
  a0 := 0;
  a1 := 0;
  Canvas.moveto(xs + r0 + r1, ys);
  for i := 1 to 36 * NSpiro do
  begin
    x1 := r1 * cos(a1);
    y1 := r1 * sin(a1);
    a1 := a1 + da1;
    x0 := r0 * cos(a0);
    y0 := r0 * sin(a0);
    a0 := a0 + da0;
    x := variant(xs + x0 + x1);
    y := variant(ys + y0 + y1);
    canvas.lineto(x, y)
  end;
end;

procedure TJvDrawImage.Star(x, y: integer);
var
  i, x0, y0, damult: integer;
  apoint: tpoint;
  da: real;
begin
  x0 := myorigin.x;
  y0 := myorigin.y;
//777  d := abs(y - y0);
  damult := 1;
  if not PolygonChecked then
  begin
    case Starpoints of
      5: damult := 2;
      7: damult := 3;
      9: damult := 4;
      11: damult := 5;
    end;
  end;
  da := damult * 2 * pi / Starpoints;
  with canvas do
  begin
    pointarray[0] := point(x, y);
    //   moveto(x,y);
    apoint := point(x, y);
    for i := 1 to Starpoints - 1 do
    begin
      //      apoint:=Rotate(point(x0,y0),apoint,da);
      //      lineto(apoint.x,apoint.y);
      apoint := Rotate(point(x0, y0), apoint, da);
      pointarray[i] := apoint;
    end;
    //      lineto(x,y);
    Polygon(Slice(PointArray, Starpoints))
  end;

end;

function TJvDrawImage.ReduceVector(origin, endpoint: tpoint;
  factor: real): tpoint;
var
  a, d, r: real;
begin
  r := sqrt(sqr(endpoint.x - origin.x) + sqr(endpoint.y - origin.y));
  d := endpoint.x - origin.x;
  if (d >= 0) and (d < 0.001) then d := 0.001;
  if (d < 0) and (d > -0.001) then d := -0.001;
  a := arctan2((endpoint.y - origin.y), d);
  r := r * factor;
  result.x := origin.x + variant(r * cos(a));
  result.y := origin.y + variant(r * sin(a));
end;
(*)
procedure TJvDrawImage.TextRotate(x, y, angle: integer; aText: string;
  afont: tfont);
var
  dc: hdc;
  fnt: LogFont;
  hfnt, hfntPrev: hfont;
  i: integer;
  fname, s: string;
begin
  s := aText;
  fnt.lfEscapement := angle * 10;
  fnt.lfOrientation := angle * 10;
  if fsbold in afont.Style then
    fnt.lfWeight := FW_Bold
  else
    fnt.lfWeight := FW_NORMAL;
  if fsitalic in afont.style then
    fnt.lfItalic := 1
  else
    fnt.lfItalic := 0;
  if fsunderline in afont.style then
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
  dc := canvas.handle;
  SetBkMode(dc, windows.TRANSPARENT);
  SetTextColor(dc, afont.color);
  hfntPrev := SelectObject(dc, hfnt);
  //Textout(dc,x,y,@aText[1],length(aText));
  Textout(dc, x, y, @s[1], length(s));
  SelectObject(dc, hfntPrev);
  DeleteObject(hfnt);
  Repaint;
end;
(*)
(*)
function TJvDrawImage.GetAngle(origin, endpoint: tpoint): integer;
var
  a, d: real;
begin
//  r := sqrt(sqr(endpoint.x - origin.x) + sqr(endpoint.y - origin.y));
  d := endpoint.x - origin.x;
  if (d >= 0) and (d < 0.001) then d := 0.001;
  if (d < 0) and (d > -0.001) then d := -0.001;
  a := arctan2((endpoint.y - origin.y), d);
  a := a * 360 / (2 * pi);
  result := variant(-a);
end;
(*)
procedure TJvDrawImage.DrawRisingWaveSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
var
  t, xcenter, a, ycenter, b: integer;
  r1, g1, b1, r2, g2, b2: byte;
  i, dx, dy, xo, yo, r, bl: integer;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  a := (x2 - x1) div 2;
  b := (y2 - y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := x1 + a;
  ycenter := y1 + b;

  dx := (x2 - x1) div bl;
  dy := (y2 - y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := y1 + b;
    b := a;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := x1 + a;
    a := b;
  end;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := abs(round(a * sin(pi * xo / (x2 - x1))));
      Sphere(clip, x1 + xo, r, ycenter, r, r1, g1, b1, r2, g2, b2, true);
    end
    else
    begin
      yo := i * dy + b;
      r := abs(round(b * sin(pi * yo / (y2 - y1) - pi / 2)));
      Sphere(clip, xcenter, r, y1 + yo, r, r1, g1, b1, r2, g2, b2, true);
    end;
  end;
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.DrawWaveSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
var
  t, xcenter, a, ycenter, b: integer;
  r1, g1, b1, r2, g2, b2: byte;
  i, dx, dy, xo, yo, r, bl: integer;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  a := (x2 - x1) div 2;
  b := (y2 - y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := x1 + a;
  ycenter := y1 + b;

  dx := (x2 - x1) div bl;
  dy := (y2 - y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := y1 + b;
    b := a;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := x1 + a;
    a := b;
  end;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
    begin
      xo := i * dx + a;
      r := abs(round(a * sin(pi * xo / (x2 - x1) - pi / 2)));
      Sphere(clip, x1 + xo, r, ycenter, r, r1, g1, b1, r2, g2, b2, true);
    end
    else
    begin
      yo := i * dy + b;
      r := abs(round(b * sin(pi * yo / (y2 - y1) - pi / 2)));
      Sphere(clip, xcenter, r, y1 + yo, r, r1, g1, b1, r2, g2, b2, true);
    end;
  end;
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.DrawDropletSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
var
  t, xcenter, a, ycenter, b: integer;
  r1, g1, b1, r2, g2, b2: byte;
  i, dx, dy, bl: integer;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  a := (x2 - x1) div 2;
  b := (y2 - y1) div 2;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);

  xcenter := x1 + a;
  ycenter := y1 + b;

  dx := (x2 - x1) div bl;
  dy := (y2 - y1) div bl;
  if dx > dy then
  begin
    a := (dx div 2) * 4 div 5;
    ycenter := y1 + b;
  end
  else
  begin
    b := (dy div 2) * 4 div 5;
    xcenter := x1 + a;
  end;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
      Sphere(clip, x1 + i * dx + a, a, ycenter, a, r1, g1, b1, r2, g2, b2, true)
    else
      Sphere(clip, xcenter, b, y1 + i * dy + b, b, r1, g1, b1, r2, g2, b2, true);
  end;
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.DrawMultiSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
var
  t, xcenter, a, ycenter, b: integer;
  r1, g1, b1, r2, g2, b2: byte;
  i, dx, dy, bl: integer;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  a := (x2 - x1) div 2;
  b := (y2 - y1) div 2;
  xcenter := x1 + a;
  ycenter := y1 + b;
  if a > b then
    bl := a div (b + 1)
  else
    bl := b div (a + 1);
  dx := (x2 - x1) div bl;
  dy := (y2 - y1) div bl;
  if dx > dy then
  begin
    a := dx div 2;
    ycenter := y1 + b;
  end
  else
  begin
    b := dy div 2;
    xcenter := x1 + a;
  end;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  for i := 0 to bl - 1 do
  begin
    if dx > dy then
      Sphere(clip, x1 + i * dx + a, a, ycenter, a, r1, g1, b1, r2, g2, b2, true)
    else
      Sphere(clip, xcenter, b, y1 + i * dy + b, b, r1, g1, b1, r2, g2, b2, true);
  end;
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.Sphere(bitmap: tbitmap;
  xcenter, a, ycenter, b: integer; r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
var (* Dessine un disque color‚*)
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
    HorGradientLine(bitmap, xcenter - xx, xcenter + xx, ycenter + yy, r1, g1, b1, r2, g2, b2, smooth);
    HorGradientLine(bitmap, xcenter - xx, xcenter + xx, ycenter - yy, r1, g1, b1, r2, g2, b2, smooth);
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

procedure TJvDrawImage.DrawSphere(color1, color2: tcolor; x1, y1, x2, y2: integer);
var
  t, xcenter, a, ycenter, b: integer;
  r1, g1, b1, r2, g2, b2: byte;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  a := ((x2 - x1) div 2);
  xcenter := x1 + a;
  b := ((y2 - y1) div 2);
  ycenter := y1 + b;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  Sphere(clip, xcenter, a, ycenter, b, r1, g1, b1, r2, g2, b2, true);
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.Column(bitmap: tbitmap; XOrigin, XFinal, YOrigin, YFinal: integer; r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
var
  j: integer;
begin
  for j := YOrigin to YFinal do
    HorGradientLine(bitmap, XOrigin, XFinal, j, r1, g1, b1, r2, g2, b2, smooth);
end;

procedure TJvDrawImage.DrawColumn(x1, y1, x2, y2: integer);
var
  t: integer;
  r1, g1, b1, r2, g2, b2: byte;
  line: pbytearray;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  if x1 > x2 then
  begin
    t := x1;
    x1 := x2;
    x2 := t;
  end;
  if y1 > y2 then
  begin
    t := y1;
    y1 := y2;
    y2 := t;
  end;
  line := clip.scanline[y1];
  r1 := line[0];
  g1 := line[1];
  b1 := line[2];
  line := clip.scanline[y2];
  r2 := line[x2 * 3];
  g2 := line[x2 * 3 + 1];
  b2 := line[x2 * 3 + 2];
  Column(clip, x1, x2, y1, y2, r1, g1, b1, r2, g2, b2, true);
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.InterpolateRect(Bmp: TBitmap; x1, y1, x2, y2: Integer);
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
  if x2 < x1 then
  begin
    t := x2;
    x2 := x1;
    x1 := t;
  end;
  if y2 < y1 then
  begin
    t := y2;
    y2 := y1;
    y1 := t;
  end;
  if (x1 < 0) or (y1 < 0) or (x2 > Bmp.Width - 1) or (y2 > Bmp.Height - 1) then Exit;
  z := 0;
  iz := $100000;
  if x2 <> x1 then t := $100000 div (x2 - x1);
  if y2 <> y1 then t2 := $100000 div (y2 - y1);
/////  dx := x2 - x1;
  pb := bmp.scanline[y1];
  c00.r := pb[x1 * 3];
  c00.g := pb[x1 * 3 + 1];
  c00.b := pb[x1 * 3 + 2];
  c01.r := pb[x2 * 3];
  c01.g := pb[x2 * 3 + 1];
  c01.b := pb[x2 * 3 + 2];
  pb := bmp.scanline[y2];
  c10.r := pb[x1 * 3];
  c10.g := pb[x1 * 3 + 1];
  c10.b := pb[x1 * 3 + 2];
  c11.r := pb[x2 * 3];
  c11.g := pb[x2 * 3 + 1];
  c11.b := pb[x2 * 3 + 2];
  for yCount := y1 to y2 do
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
    //    pb:=@Bmp.Pixels[yCount,x1];
    for xCount := x1 to x2 do
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

procedure TJvDrawImage.InterpRect(x1, y1, x2, y2: integer);
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  Interpolaterect(clip, x1, y1, x2, y2);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawBlurLines(x0, y0, x, y: integer);
begin
  DrawTexLines(x0, y0, x, y);
  ClipAll;
  clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(clip, UserFilter);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawBlurRects(x0, y0, x, y: integer);
begin
  DrawTexRects(x0, y0, x, y);
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.GaussianBlur(clip, 4);
  UserFilter := Blurfilter;
  applyfilter(clip, UserFilter);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawTexRects(x0, y0, x, y: integer);
var
  dx, dy, xr, yr, x1, y1, x2, y2, i, w, h, xi, yi: integer;
  bcolor, pcolor, hcolor, scolor: tcolor;
begin
  w := width;
  h := height;
  pcolor := canvas.pen.color;
  bcolor := canvas.brush.color;
  canvas.brush.color := pcolor;
  canvas.brush.style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(x - x0) + sqr(y - y0))));
  dx := abs(x - x0);
  dy := abs(y - y0);
  if dy < 3 then dy := 3;
  if dx < 3 then dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 3 do
        with canvas do
        begin
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          pen.color := scolor;
          brush.color := scolor;
          rectangle(x1, y1, x2 + 2, y2 + 2);
          pen.color := hcolor;
          brush.color := hcolor;
          rectangle(x1 - 2, y1 - 2, x2, y2);
          pen.color := pcolor;
          brush.color := pcolor;
          rectangle(x1, y1, x2, y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  canvas.pen.color := pcolor;
  canvas.brush.color := bcolor;
end;

procedure TJvDrawImage.DrawBlurPoly(x0, y0, x, y: integer);
begin
  DrawTexPoly(x0, y0, x, y);
  ClipAll;
  clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(clip, UserFilter);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawTexPoly(x0, y0, x, y: integer);
var
  dx, dy, xr, yr, x1, y1, x2, y2, i, w, h, xi, yi: integer;
  pcolor: tcolor;
  points: array[0..3] of tpoint;
begin
  w := width;
  h := height;
  pcolor := canvas.pen.color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(x - x0) + sqr(y - y0))));
  dx := abs(x - x0);
  dy := abs(y - y0);
  if dy < 3 then dy := 3;
  if dx < 3 then dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with canvas do
        begin
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          points[0] := point(x1, y1);
          points[3] := point(x2, y2);
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          points[1] := point(x1, y1);
          points[2] := point(x2, y2);
          pen.color := pcolor;
          polyline(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  canvas.pen.color := pcolor;
end;

procedure TJvDrawImage.DrawBlurCurves(x0, y0, x, y: integer);
begin
  DrawTexCurves(x0, y0, x, y);
  ClipAll;
  clip.PixelFormat := pf24bit;
  //GaussianBlur(4);
  UserFilter := Blurfilter;
  applyfilter(clip, UserFilter);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawTexCurves(x0, y0, x, y: integer);
var
  dx, dy, xr, yr, x1, y1, x2, y2, i, w, h, xi, yi: integer;
  pcolor: tcolor;
  points: array[0..3] of tpoint;
begin
  w := width;
  h := height;
  pcolor := canvas.pen.color;
//  hcolor := Texhighlight(pcolor);
//  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(x - x0) + sqr(y - y0))));
  dx := abs(x - x0);
  dy := abs(y - y0);
  if dy < 3 then dy := 3;
  if dx < 3 then dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with canvas do
        begin
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          points[0] := point(x1, y1);
          points[3] := point(x2, y2);
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          points[1] := point(x1, y1);
          points[2] := point(x2, y2);
          pen.color := pcolor;
          polybezier(points);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  canvas.pen.color := pcolor;
end;

procedure TJvDrawImage.ApplyFilter(var Dst: TBitmap; DF: TDigitalFilter);
var
  i, j, x, y, tmpx, tmpy: Integer;
  Sum,
    Red,
    Green,
    Blue: integer; //total value
  Tmp,
    Color: TFColor;
  Ptmp, Pcolor: pbytearray;
  bm: tbitmap;
  R: trect;
begin
  bm := tbitmap.create;
  bm.pixelformat := pf24bit;
  bm.width := dst.width;
  bm.height := dst.height;
  R := rect(0, 0, bm.width, bm.height);
  bm.canvas.CopyRect(R, dst.canvas, R);
  sum := 0;
  for y := 0 to 4 do
    for x := 0 to 4 do
      sum := sum + DF[x, y];
  if Sum = 0 then Sum := 1;
  for y := 0 to Dst.Height - 1 do
  begin
    Pcolor := dst.scanline[y];
    for x := 0 to bm.Width - 1 do
    begin
      Red := 0;
      Green := 0;
      Blue := 0;
      for i := 0 to 4 do
        for j := 0 to 4 do
        begin
          Tmpy := TrimInt(y + j - 2, 0, bm.Height - 1);
          Tmpx := TrimInt(x + i - 2, 0, bm.Width - 1);
          ptmp := bm.scanline[Tmpy];
          Tmp.r := ptmp[tmpx * 3];
          Tmp.g := ptmp[tmpx * 3 + 1];
          Tmp.b := ptmp[tmpx * 3 + 2];
          //          Tmp:=@Dst.Pixels[TrimInt(y+j-1,0,Dst.Height-1),
          //                           TrimInt(x+i-1,0,Dst.Width-1)];
          Inc(Blue, DF[i, j] * Tmp.b);
          Inc(Green, DF[i, j] * Tmp.g);
          Inc(Red, DF[i, j] * Tmp.r);
        end;
      Color.b := IntToByte(Blue div Sum);
      Color.g := IntToByte(Green div Sum);
      Color.r := IntToByte(Red div Sum);
      PColor[x * 3] := color.r;
      Pcolor[x * 3 + 1] := color.g;
      Pcolor[x * 3 + 2] := color.b;
    end;
  end;
  bm.free;
end;

procedure TJvDrawImage.DrawBlurOvals(x0, y0, x, y: integer);
begin
  DrawTexOvals(x0, y0, x, y);
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.GaussianBlur(clip, 4);
  UserFilter := Blurfilter;
  applyfilter(clip, UserFilter);
  picture.bitmap.assign(clip);

end;

procedure TJvDrawImage.DrawTexOvals(x0, y0, x, y: integer);
var
  dx, dy, xr, yr, x1, y1, x2, y2, i, w, h, xi, yi: integer;
  bcolor, pcolor, hcolor, scolor: tcolor;
begin
  w := width;
  h := height;
  pcolor := canvas.pen.color;
  bcolor := canvas.brush.color;
  canvas.brush.color := pcolor;
  canvas.brush.style := bssolid;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(x - x0) + sqr(y - y0))));
  dx := abs(x - x0);
  dy := abs(y - y0);
  if dy < 3 then dy := 3;
  if dx < 3 then dx := 3;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 3 do
        with canvas do
        begin
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          pen.color := scolor;
          brush.color := scolor;
          ellipse(x1, y1, x2 + 2, y2 + 2);
          pen.color := hcolor;
          brush.color := hcolor;
          ellipse(x1 - 2, y1 - 2, x2, y2);
          pen.color := pcolor;
          brush.color := pcolor;
          ellipse(x1, y1, x2, y2);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  canvas.pen.color := pcolor;
  canvas.brush.color := bcolor;
end;

function TJvDrawImage.BlendColors(const Color1, Color2: longInt; Opacity: integer): longInt;
var
  R, R1, R2, G, G1, G2, B, B1, B2: integer;
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

function TJvDrawImage.TexHighlight(Colr: longInt): longInt;
var
  avg, r, g, b: integer;
  tmp: longInt;
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

function TJvDrawImage.TexShadow(Colr: longInt): longInt;
var
  r, g, b: integer;
  tmp: longInt;
begin
  r := GetRValue(Colr);
  g := GetGValue(Colr);
  b := GetBValue(Colr);
  tmp := RGB(trunc(DarkStrength * r), trunc(DarkStrength * g),
    trunc(DarkStrength * b));
  Result := BlendColors(Colr, tmp, StrongBlend);
end; { Shadow }

procedure TJvDrawImage.DrawTexLines(x0, y0, x, y: integer);
var
  dx, dy, xr, yr, x1, y1, x2, y2, i, w, h, xi, yi: integer;
  pcolor, hcolor, scolor: tcolor;
begin
  w := width;
  h := height;
  pcolor := canvas.pen.color;
  hcolor := Texhighlight(pcolor);
  scolor := TexShadow(pcolor);
  xr := abs(round(sqrt(sqr(x - x0) + sqr(y - y0))));
  dx := abs(x - x0);
  dy := abs(y - y0);
  if dy = 0 then dy := 1;
  if dx = 0 then dx := 1;
//  tx := w div dx;
//  ty := h div dy;
  yr := round(dy / dx * xr);
  yi := 0;
  repeat
    xi := 0;
    repeat
      for i := 1 to 10 do
        with canvas do
        begin
          x1 := xi + random(xr);
          y1 := yi + random(yr);
          x2 := xi + random(xr);
          y2 := yi + random(yr);
          pen.color := pcolor;
          MoveTo(x1, y1);
          LineTo(x2, y2);
          pen.color := hcolor;
          moveto(x1 - 1, y1 - 1);
          lineto(x2 - 1, y2 - 1);
          pen.color := scolor;
          moveto(x1 + 1, y1 + 1);
          lineto(x2 + 1, y2 + 1);
        end;
      inc(xi, dx);
    until xi > w - 1;
    inc(yi, dy);
  until yi > h - 1;
  canvas.pen.color := pcolor;
end;

procedure TJvDrawImage.DrawSyms(x, y: integer);
var
  x0, y0, i: integer;
  da: real;
  apoint: tpoint;
begin
  x0 := picture.bitmap.Width div 2;
  y0 := picture.Bitmap.Height div 2;
  da := 2 * pi / Starpoints;
  apoint := point(x, y);
  for i := 0 to Starpoints - 1 do
  begin
    with canvas do
    begin
      moveto(pointarray[i].x, pointarray[i].y);
      lineto(apoint.x, apoint.y);
      pointarray[i] := apoint;
      apoint := Rotate(point(x0, y0), apoint, da);
    end;
  end;
end;

procedure TJvDrawImage.PutClip(M: Trect);
var
  dest: trect;
begin
  clip.width := (m.right - m.left + 1);
  clip.height := (m.bottom - m.top + 1);
  dest := rect(0, 0, clip.width, clip.height);
  clip.Canvas.CopyMode := cmsrccopy;
  clip.pixelformat := picture.bitmap.pixelformat;
  clip.canvas.CopyRect(dest, canvas, m);
end;

procedure TJvDrawImage.DrawTriangle;
begin
  with canvas do
  begin
    moveto(myskew[0].x, myskew[0].y);
    lineto(myskew[1].x, myskew[1].y);
    lineto(myskew[2].x, myskew[2].y);
    lineto(myskew[0].x, myskew[0].y);
  end;
end;

procedure TJvDrawImage.DrawSkew;
begin
  with canvas do
  begin
    moveto(myskew[0].x, myskew[0].y);
    lineto(myskew[1].x, myskew[1].y);
    lineto(myskew[2].x, myskew[2].y);
    lineto(myskew[3].x, myskew[3].y);
    lineto(myskew[0].x, myskew[0].y);
  end;
end;

function TJvDrawImage.PointToBlock(x, y: integer): Trect;
var
  xb, yb, w, h: integer;
begin
  w := picture.bitmap.Width;
  h := Picture.bitmap.Height;
  xb := w div Blocks;
  yb := h div Blocks;
  result.left := (x div xb) * xb;
  result.top := (y div yb) * yb;
  result.Right := result.left + xb;
  result.Bottom := result.top + yb;
end;

procedure TJvDrawImage.DrawCube;
var
  dx, dy: integer;
begin
  with canvas do
  begin
    dx := myskew[4].x - myskew[2].x;
    dy := myskew[4].y - myskew[2].y;
    moveto(myskew[0].x, myskew[0].y);
    lineto(myskew[1].x, myskew[1].y);
    lineto(myskew[2].x, myskew[2].y);
    lineto(myskew[3].x, myskew[3].y);
    lineto(myskew[0].x, myskew[0].y);
    if (dx >= 0) and (dy <= 0) then
    begin
      moveto(myskew[0].x, myskew[0].y);
      lineto(myskew[0].x + dx, myskew[0].y + dy);
      lineto(myskew[1].x + dx, myskew[1].y + dy);
      lineto(myskew[2].x + dx, myskew[2].y + dy);
      lineto(myskew[2].x, myskew[2].y);
      moveto(myskew[1].x, myskew[1].y);
      lineto(myskew[1].x + dx, myskew[1].y + dy);
    end
    else if (dx >= 0) and (dy > 0) then
    begin
      moveto(myskew[1].x, myskew[1].y);
      lineto(myskew[1].x + dx, myskew[1].y + dy);
      lineto(myskew[2].x + dx, myskew[2].y + dy);
      lineto(myskew[3].x + dx, myskew[3].y + dy);
      lineto(myskew[3].x, myskew[3].y);
      moveto(myskew[2].x, myskew[2].y);
      lineto(myskew[2].x + dx, myskew[2].y + dy);
    end
    else if (dx < 0) and (dy > 0) then
    begin
      moveto(myskew[0].x, myskew[0].y);
      lineto(myskew[0].x + dx, myskew[0].y + dy);
      lineto(myskew[3].x + dx, myskew[3].y + dy);
      lineto(myskew[2].x + dx, myskew[2].y + dy);
      lineto(myskew[2].x, myskew[2].y);
      moveto(myskew[3].x, myskew[3].y);
      lineto(myskew[3].x + dx, myskew[3].y + dy);
    end
    else if (dx < 0) and (dy < 0) then
    begin
      moveto(myskew[1].x, myskew[1].y);
      lineto(myskew[1].x + dx, myskew[1].y + dy);
      lineto(myskew[0].x + dx, myskew[0].y + dy);
      lineto(myskew[3].x + dx, myskew[3].y + dy);
      lineto(myskew[3].x, myskew[3].y);
      moveto(myskew[0].x, myskew[0].y);
      lineto(myskew[0].x + dx, myskew[0].y + dy);
    end;
  end;
end;

procedure TJvDrawImage.VerGradientLine(bitmap: tbitmap;
  YOrigin, YFinal, x: integer; r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
var
  r, g, b, i: integer;
  valueR, ValueG, ValueB, advalR, advalB, advalG: single;
  Line: PByteArray;
begin
  if (x >= 0) and (x < bitmap.width) then
  begin
    if YOrigin > YFinal then
    begin
      i := YOrigin;
      YOrigin := YFinal;
      YFinal := i;
    end;
    if YFinal <> YOrigin then
    begin
      advalR := (r2 - r1) / (YFinal - YOrigin);
      advalG := (g2 - g1) / (YFinal - YOrigin);
      advalB := (b2 - b1) / (YFinal - YOrigin);
    end
    else
    begin
      advalR := 0;
      advalG := 0;
      advalB := 0;
    end;

    valueR := r1;
    valueG := g1;
    valueB := b1;

    for i := YOrigin to YFinal do
    begin
      Line := bitmap.scanline[i];
      valueR := valueR + advalR;
      r := round(ValueR);
      if r > 255 then r := 255;
      if r < 0 then r := 0;
      valueG := valueG + advalG;
      g := round(ValueG);
      if g > 255 then g := 255;
      if g < 0 then g := 0;
      valueB := valueB + advalB;
      b := round(ValueB);
      if b > 255 then b := 255;
      if b < 0 then b := 0;
      if (x >= 0) and (x < bitmap.width) then
      begin
        Line[x * 3] := b;
        Line[x * 3 + 1] := g;
        Line[x * 3 + 2] := r;
      end;
    end;
    if smooth then
    begin
      SmoothPnt(bitmap, x, YOrigin - 1);
      SmoothPnt(bitmap, x, YFinal + 1);
    end;
  end;
end;

procedure TJvDrawImage.DrawVGradientBrush(color1, color2: tcolor; y1, y2, x: integer);
var
  r1, g1, b1, r2, g2, b2: byte;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  vergradientline(clip, y1, y2, x, r1, g1, b1, r2, g2, b2, true);
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.SmoothPnt(bitmap: tbitmap; xk, yk: integer);
type
  TFColor = record b, g, r: Byte
  end;
var
  Bleu, Vert, Rouge: Integer;
  color: TFColor;
  BB, GG, RR: array[1..5] of Integer;
  Line: pbytearray;
begin
  if (xk > 0) and (yk > 0) and (xk < bitmap.width - 1) and (yk < bitmap.height - 1) then
  begin
    line := bitmap.scanline[yk - 1];
    color.r := line[xk * 3];
    color.g := line[xk * 3 + 1];
    color.b := line[xk * 3 + 2];
    RR[1] := color.r;
    GG[1] := color.g;
    BB[1] := color.b;
    line := bitmap.scanline[yk];
    color.r := line[(xk + 1) * 3];
    color.g := line[(xk + 1) * 3 + 1];
    color.b := line[(xk + 1) * 3 + 2];
    RR[2] := color.r;
    GG[2] := color.g;
    BB[2] := color.b;
    line := bitmap.scanline[yk + 1];
    color.r := line[xk * 3];
    color.g := line[xk * 3 + 1];
    color.b := line[xk * 3 + 2];
    RR[3] := color.r;
    GG[3] := color.g;
    BB[3] := color.b;
    line := bitmap.scanline[yk];
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
    line := bitmap.scanline[yk];
    line[xk * 3] := color.r;
    line[xk * 3 + 1] := color.g;
    line[xk * 3 + 2] := color.b;
  end;
end;

procedure TJvDrawImage.HorGradientLine(bitmap: tbitmap;
  XOrigin, XFinal, y: integer; r1, g1, b1, r2, g2, b2: byte; smooth: boolean);
var
  r, g, b, i: integer;
  valueR, ValueG, ValueB, advalR, advalB, advalG: single;
  Line: PByteArray;
begin
  if (y >= 0) and (y < bitmap.height) then
  begin
    if XOrigin > XFinal then
    begin
      i := XOrigin;
      XOrigin := XFinal;
      XFinal := i;
    end;
    if XFinal <> XOrigin then
    begin
      advalR := (r2 - r1) / (XFinal - XOrigin);
      advalG := (g2 - g1) / (XFinal - XOrigin);
      advalB := (b2 - b1) / (XFinal - XOrigin);
    end
    else
    begin
      advalR := 0;
      advalG := 0;
      advalB := 0;
    end;

    valueR := r1;
    valueG := g1;
    valueB := b1;
    Line := bitmap.scanline[y];
    for i := XOrigin to XFinal do
    begin
      valueR := valueR + advalR;
      r := round(ValueR);
      if r > 255 then r := 255;
      if r < 0 then r := 0;
      valueG := valueG + advalG;
      g := round(ValueG);
      if g > 255 then g := 255;
      if g < 0 then g := 0;
      valueB := valueB + advalB;
      b := round(ValueB);
      if b > 255 then b := 255;
      if b < 0 then b := 0;
      if (i >= 0) and (i < bitmap.width) then
      begin
        Line[i * 3] := b;
        Line[i * 3 + 1] := g;
        Line[i * 3 + 2] := r;
      end;
    end;
    if smooth then
    begin
      SmoothPnt(bitmap, XOrigin - 1, y);
      SmoothPnt(bitmap, XFinal + 1, y);
    end;
  end;
end;

procedure TJvDrawImage.DrawGradientBrush(color1, color2: tcolor; x1, x2, y: integer);
var
  r1, g1, b1, r2, g2, b2: byte;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  color1 := colortorgb(color1);
  r1 := getrvalue(color1);
  g1 := getgvalue(color1);
  b1 := getbvalue(color1);
  color2 := colortorgb(color2);
  r2 := getrvalue(color2);
  g2 := getgvalue(color2);
  b2 := getbvalue(color2);
  horgradientline(clip, x1, x2, y, r1, g1, b1, r2, g2, b2, true);
  picture.bitmap.Assign(clip);
end;

procedure TJvDrawImage.DrawLighterCircle(x, y, mode: integer);
var
  r: integer;
begin
  r := canvas.pen.width;
  if r < 5 then r := 5;
  ColorCircle(clip, point(x, y), r, mode);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawDarkerCircle(x, y, mode: integer);
var
  r: integer;
begin
  r := canvas.pen.width;
  if r < 5 then r := 5;
  ColorCircle(clip, point(x, y), r, mode);
  picture.bitmap.assign(clip);

end;

procedure TJvDrawImage.ColorCircle(var bm: TBitmap; center: tpoint; radius, mode: integer);
var
  p, p0, p1: pbytearray;
  dx, x, y, w, h, i, j, sum, c: integer;
  cm, tm: tbitmap;
  Rs, Rd: trect;
begin
  x := center.x;
  y := center.y;
  w := bm.width;
  h := bm.height;
  cm := tbitmap.create;
  cm.width := 2 * radius;
  cm.height := 2 * radius;
  cm.PixelFormat := pf24bit;
  tm := tbitmap.create;
  tm.width := 2 * radius;
  tm.height := 2 * radius;
  tm.PixelFormat := pf24bit;
  tm.canvas.brush.color := clblack;
  tm.canvas.Ellipse(0, 0, tm.width - 1, tm.height - 1);
  tm.transparent := true;
  tm.TransparentColor := clblack;
  Rd := rect(0, 0, cm.width, cm.height);
  Rs := rect(x - radius, y - radius, x + radius, y + radius);
  cm.canvas.CopyRect(Rd, bm.canvas, RS);
  p0 := nil;
  p1 := nil;
  for j := 0 to cm.height - 1 do
  begin
    p := cm.scanline[j];
    if j > 0 then p0 := cm.scanline[j - 1];
    if j < (h - 1) then p1 := cm.scanline[j + 1];
    for i := 0 to cm.width - 1 do
    begin
      case mode of
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
        13: //smooth
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
  cm.canvas.Draw(0, 0, tm);
  cm.transparent := true;
  cm.transparentcolor := clwhite;
  bm.Canvas.Draw(x - radius, y - radius, cm);
  cm.free;
  tm.free;
end;

procedure TJvDrawImage.DrawColorCircle(x, y, mode: integer);
var
  r: integer;
begin
  picture.bitmap.pixelformat := pf24bit;
  clip.assign(picture.bitmap);
  clip.PixelFormat := pf24bit;
  r := canvas.pen.width;
  if r < 5 then r := 5;
  ColorCircle(clip, point(x, y), r, mode);
  picture.bitmap.assign(clip);
end;

procedure TJvDrawImage.DrawLightBrush(x, y, radius, amount: integer; style: TLightBrush);
var
  src, dst: tbitmap;
  Rclip, Rsrc: trect;
begin
  if x < radius then x := radius;
  if y < radius then y := radius;
  if (x + radius) > clip.width - 1 then x := clip.width - 1 - radius;
  if (y + radius) > clip.height - 1 then y := clip.height - 1 - radius;
  src := tbitmap.create;
  src.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  Rclip := rect(x - radius, y - radius, x + radius, y + radius);
  src.width := Rclip.right - Rclip.left;
  src.height := RClip.bottom - Rclip.top;
  dst.width := src.width;
  dst.height := src.height;
  Rsrc := rect(0, 0, src.width, src.height);
  dst.Canvas.CopyRect(Rsrc, clip.canvas, Rclip);
  case style of
    lbBrightness: FX.lightness(dst, amount);
    lbSaturation: FX.saturation(dst, amount);
    lbContrast: FX.contrast(dst, amount);
  end;
  // mask code
  src.canvas.Brush.color := clwhite;
  src.canvas.FillRect(Rsrc);
  src.canvas.brush.style := bssolid;
  src.canvas.Brush.color := clblack;
  src.canvas.Ellipse(0, 0, src.width - 1, src.height - 1);
  src.Transparent := true;
  src.TransparentColor := clblack;
  dst.canvas.Draw(0, 0, src);
  dst.transparent := true;
  dst.TransparentColor := clwhite;
  canvas.Draw(0, 0, clip);
  canvas.Draw(x - radius, y - radius, dst);
  src.free;
  dst.free;
end;

procedure TJvDrawImage.SampleStretch(src, dst: tbitmap);
begin
  // use mitchelfilter from resample unit
  ImgStretch(src, dst,
    ResampleFilters[6].Filter, ResampleFilters[6].Width);
end;

procedure TJvDrawImage.DrawStretchBrush(x, y, radius: integer; amount: extended; style: Tmorphbrush);
var
  src, dst: tbitmap;
  Rclip, Rsrc: trect;
  dr: integer;
begin
  if x < radius then x := radius;
  if y < radius then y := radius;
  if (x + radius) > clip.width - 1 then x := clip.width - 1 - radius;
  if (y + radius) > clip.height - 1 then y := clip.height - 1 - radius;
  src := tbitmap.create;
  src.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  Rclip := rect(x - radius, y - radius, x + radius, y + radius);
  dst.width := Rclip.right - Rclip.left;
  dst.height := RClip.bottom - Rclip.top;
  // now Change to Reduce
  amount := abs(amount);
  if amount < 1 then amount := 1;
  dr := round(radius * amount / 180);
  if dr < 5 then dr := 5;
  if dr > radius then dr := radius;
  //(mbVerBox,mbHorBox,mbVerOval,mbHorOval);
  case style of
    mbVerOval, mbVerbox: Rclip := rect(x - radius, y - dr, x + radius, y + dr);
    mbHorOval, mbHorBox: Rclip := rect(x - dr, y - radius, x + dr, y + radius);
  end;
  src.width := Rclip.right - Rclip.left;
  src.height := RClip.bottom - Rclip.top;
  Rsrc := rect(0, 0, src.width, src.height);
  src.Canvas.CopyRect(Rsrc, clip.canvas, Rclip);
  SampleStretch(src, dst);
  // mask code
  // reset src dimensions for masking
  if style in [mbHorOval, mbVerOval] then
  begin
    src.width := dst.width;
    src.height := dst.height;
    src.canvas.Brush.color := clwhite;
    src.canvas.FillRect(Rsrc);
    src.canvas.brush.style := bssolid;
    src.canvas.Brush.color := clblack;
    src.canvas.Ellipse(0, 0, src.width - 1, src.height - 1);
    src.Transparent := true;
    src.TransparentColor := clblack;
    dst.canvas.Draw(0, 0, src);
    dst.transparent := true;
    dst.TransparentColor := clwhite;
    canvas.Draw(0, 0, clip);
  end;
  canvas.Draw(x - radius, y - radius, dst);
  src.free;
  dst.free;
end;

procedure TJvDrawImage.Rimple(src, dst: tbitmap; amount: extended);
var
  ca, sa, a, dx, dy, r, sr, fr: extended;
  w, h, x, y, cx, cy, i, j, c, ci: integer;
  p1, p2: pbytearray;
begin
  w := src.width;
  h := src.height;
  cx := w div 2;
  cy := h div 2;
  if amount < 1 then amount := 1;
  fr := cx / amount;
  for y := 0 to h - 1 do
  begin
    p1 := src.ScanLine[y];
    for x := 0 to w - 1 do
    begin
      dx := x - cx;
      dy := -(y - cx);
      r := sqrt(sqr(dx) + sqr(dy));
      sr := fr * sin(r / cx * amount * 2 * pi);
      if (r + sr < cx) and (r + sr > 0) then
      begin
        a := arctan2(dy, dx);
        sincos(a, sa, ca);
        i := cx + round((r + sr) * ca);
        j := cy + round((r + sr) * sa);
        p2 := dst.scanline[j];
        c := x * 3;
        ci := i * 3;
        p2[ci] := p1[c];
        p2[ci + 1] := p1[c + 1];
        p2[ci + 2] := p1[c + 2];
      end;
    end;
  end;
end;

procedure TJvDrawImage.DrawEffectBrush(x, y, radius: integer; amount: extended; style: TLightbrush);
var
  src, dst: tbitmap;
  Rclip, Rsrc: trect;
begin
  if x < radius then x := radius;
  if y < radius then y := radius;
  if (x + radius) > clip.width - 1 then x := clip.width - 1 - radius;
  if (y + radius) > clip.height - 1 then y := clip.height - 1 - radius;
  src := tbitmap.create;
  src.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  Rclip := rect(x - radius, y - radius, x + radius, y + radius);
  src.width := Rclip.right - Rclip.left;
  src.height := RClip.bottom - Rclip.top;
  dst.width := src.width;
  dst.height := src.height;
  Rsrc := rect(0, 0, src.width, src.height);
  src.Canvas.CopyRect(Rsrc, clip.canvas, Rclip);
  case style of
    lbfisheye: FX.fisheye(src, dst, amount);
    lbrotate: FX.smoothrotate(src, dst, src.width div 2, src.height div 2, amount);
    lbtwist: FX.twist(src, dst, round(amount));
    lbrimple: Rimple(src, dst, amount);
    mbHor, mbTop, mbBottom, mbDiamond, mbWaste, mbRound, mbRound2:
      FX.SqueezeHor(src, dst, round(amount), style);
    mbSplitRound, mbSplitWaste:
      FX.SplitRound(src, dst, round(amount), style);
  end;
  // mask code
  src.canvas.Brush.color := clwhite;
  src.canvas.FillRect(Rsrc);
  src.canvas.brush.style := bssolid;
  src.canvas.Brush.color := clblack;
  src.canvas.Ellipse(0, 0, src.width - 1, src.height - 1);
  src.Transparent := true;
  src.TransparentColor := clblack;
  dst.canvas.Draw(0, 0, src);
  dst.transparent := true;
  dst.TransparentColor := clwhite;
  canvas.Draw(0, 0, clip);
  canvas.Draw(x - radius, y - radius, dst);
  src.free;
  dst.free;
end;

procedure TJvDrawImage.DrawPlasma(x, y: integer; amount: extended);
var
  src: tbitmap;
  Rs: trect;
  h, w, ra: integer;
begin
  src := tbitmap.create;
  ra := round(amount);
  zoomrect := rect(x - ra, y - ra, x + ra, y + ra);
  if zoomrect.left < 0 then zoomrect.left := 0;
  if zoomrect.top < 0 then zoomrect.top := 0;
  if zoomrect.right > (zoomclip.width - 1) then zoomrect.right := zoomclip.width - 1;
  if zoomrect.bottom > (zoomclip.height - 1) then zoomrect.bottom := zoomclip.height - 1;
  w := zoomrect.right - zoomrect.left + 1;
  h := zoomrect.bottom - zoomrect.top + 1;
  src.width := w;
  src.height := h;
  src.PixelFormat := pf24bit;
  Rs := rect(0, 0, w, h);
  src.Canvas.CopyRect(Rs, zoomclip.canvas, zoomrect);
  canvas.stretchDraw(rect(0, 0, zoomclip.width, zoomclip.height), src);
  src.free;
end;

function TJvDrawImage.Rotate(origin, endpoint: tpoint; angle: real): tpoint;
var
  a, d, r: real;
begin
  r := sqrt(sqr(endpoint.x - origin.x) + sqr(endpoint.y - origin.y));
  d := endpoint.x - origin.x;
  if (d >= 0) and (d < 0.001) then d := 0.001;
  if (d < 0) and (d > -0.001) then d := -0.001;
  a := arctan2((endpoint.y - origin.y), d);
  a := a + angle;
  result.x := origin.x + variant(r * cos(a));
  result.y := origin.y + variant(r * sin(a));
end;

procedure TJvDrawImage.SetSyms(x, y: integer);
var
  x0, y0, i: integer;
  da: real;
  apoint: tpoint;
begin
  x0 := picture.bitmap.Width div 2;
  y0 := picture.Bitmap.Height div 2;
  da := 2 * pi / Starpoints;
  apoint := point(x, y);
  pointarray[0] := apoint;
  for i := 1 to Starpoints - 1 do
  begin
    apoint := Rotate(point(x0, y0), apoint, da);
    pointarray[i] := apoint;
  end;
end;

function TJvDrawImage.GetBlue(acolor: Tcolor): byte;
begin
  result := GetBValue(ColorToRGB(acolor));
end;

function TJvDrawImage.GetGreen(acolor: Tcolor): byte;
begin
  result := GetGValue(ColorToRGB(acolor));
end;

function TJvDrawImage.GetRed(acolor: Tcolor): byte;
begin
  result := GetRValue(ColorToRGB(acolor));
end;

function TJvDrawImage.MixColors(color1, color2: tcolor): tcolor;
var
  r1, g1, b1: byte;
begin
  color1 := colortorgb(color1);
  color2 := colortorgb(color2);
  r1 := (getRed(color1) + getRed(color2)) div 2;
  g1 := (getGreen(color1) + getGreen(color2)) div 2;
  b1 := (getBlue(color1) + getBlue(color2)) div 2;
  result := rgb(r1, g1, b1);
end;

procedure TJvDrawImage.InitPlasma;
var
  w, h: integer;
begin
  with picture.bitmap do
  begin
    w := width;
    h := height;
    zoomclip.width := w;
    zoomclip.height := h;
  end;
  zoomclip.PixelFormat := pf24bit;
  zoomclip.Canvas.Draw(0, 0, picture.bitmap);
end;

procedure TJvDrawImage.CopyClip;
var
  m, dest: trect;
begin
  m := mycliprect;
  clip.width := m.right - m.left + 1;
  clip.height := m.bottom - m.top + 1;
  dest := rect(0, 0, clip.width, clip.height);
  clip.Canvas.CopyMode := clipcm;
  clip.canvas.CopyRect(dest, canvas, m);
  clip.pixelformat := pf24bit;
end;

procedure TJvDrawImage.ClipAll;
begin
  mycliprect := rect(0, 0, picture.bitmap.width - 1, picture.bitmap.height - 1);
  clipcm := cmsrccopy;
  SetClip(clwhite);
  CopyClip;
end;

procedure TJvDrawImage.SetClip(acolor: tcolor);
var
  m, dest: trect;
begin
  m := mycliprect;
  clip.width := (m.right - m.left) + 1;
  clip.height := (m.bottom - m.top) + 1;
  dest := rect(0, 0, clip.width, clip.height);
  clip.canvas.brush.color := acolor;
  clip.Canvas.FillRect(dest);
end;

procedure TJvDrawImage.MyPaintESC(x, y: integer; shift: tshiftstate);
begin
  if myshape = 'Polygon' then
  begin
    if freepolycount > 2 then
      canvas.Polygon(slice(freepoly, freepolycount));
    freepolycount := 0;
    myDraw := false;
  end;
  if myshape = 'polyline' then
  begin
    freepolycount := 0;
    myDraw := false;
  end;
  if myshape = 'polybezier' then
  begin
    bezierfix1 := false;
    bezierfix2 := false;
    myorigin := point(x, y);
    myprevpoint := myorigin;
    if ssalt in shift then myDraw := false;
  end;

  canvas.pen.mode := mypen;
  TargetPoint := point(x, y);
end;

procedure TJvDrawImage.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Wavepen := canvas.pen.color;
  Wavebrush := canvas.brush.color;
  if button = mbright then
  begin
    MyPaintESC(x, y, shift);
    exit;
  end;
  if ((ssctrl in shift) and (ssshift in shift)) then
  begin
    x := targetpoint.x;
    y := targetpoint.y;
    mouse.CursorPos := clienttoscreen(point(x, y));
  end;
  canvas.MoveTo(x, y);
  myorigin := point(x, y);
  myprevpoint := myorigin;
  myslinedir := 'none';
  myDraw := true;
  mypen := canvas.Pen.Mode;
  if (myshape = 'rangemove') or (myshape = 'rangesmear') then
  begin
    clipcm := cmSrcInvert;
    SetClip(clwhite);
    CopyClip;
    with canvas do
    begin
      copymode := cmSrcInvert;
      Draw(x, y, clip);
    end;
  end;

  if myshape = 'darkerbrush' then ClipAll;
  if myshape = 'mbHorOval' then ClipAll;
  if myshape = 'mbVerOval' then ClipAll;
  if myshape = 'mbVerBox' then ClipAll;
  if myshape = 'mbHorBox' then ClipAll;
  if myshape = 'mbHor' then ClipAll;
  if myshape = 'mbTop' then ClipAll;
  if myshape = 'mbBottom' then ClipAll;
  if myshape = 'mbDiamond' then ClipAll;
  if myshape = 'mbWaste' then ClipAll;
  if myshape = 'mbRound' then ClipAll;
  if myshape = 'mbRound2' then ClipAll;
  if myshape = 'mbSplitRound' then ClipAll;
  if myshape = 'mbSplitWaste' then ClipAll;
  if myshape = 'zoombrush' then InitPlasma;
  if myshape = 'zoomkeepbrush' then InitPlasma;
  if myshape = 'brightnessbrush' then ClipAll;
  if myshape = 'contrastbrush' then ClipAll;
  if myshape = 'saturationbrush' then ClipAll;
  if myshape = 'fisheyebrush' then ClipAll;
  if myshape = 'fisheyefixbrush' then ClipAll;
  if myshape = 'rotatebrush' then ClipAll;
  if myshape = 'twistbrush' then ClipAll;
  if myshape = 'rimplebrush' then ClipAll;
  if myshape = 'lighterbrush' then ClipAll;
  if myshape = 'graybrush' then ClipAll;
  if myshape = 'rollmixbrush' then ClipAll;
  if myshape = 'smoothbrush' then ClipAll;

  if myshape = 'gradientbrush' then
  begin
    picture.bitmap.PixelFormat := pf24bit;
  end;
  if myshape = 'mixbrush' then
  begin
    with canvas do
    begin
      if ssalt in shift then
        pen.color := MixColors(pixels[x, y - pen.width], pixels[x, y + pen.width])
      else
        pen.color := MixColors(pixels[x - pen.width, y], pixels[x + pen.width, y]);
    end;
  end;
  if myshape = 'sym' then
    SetSyms(x, y);
  if myshape = 'chord3' then
    myshape := 'chord';
  if myshape = 'pie3' then
    myshape := 'pie';
  if myshape = 'arc3' then
    myshape := 'arc';
  if myshape = 'bezier3' then
    myshape := 'bezier';
  if myshape = 'skewrect2' then
    myshape := 'skewrect';
  if myshape = 'triangle2' then
    myshape := 'triangle';
  if myshape = 'cube2' then
    myshape := 'cube';
  if (myshape = 'snapshot') then
  begin
    myoldbrushstyle := canvas.brush.style;
    canvas.brush.style := bsclear;
    myoldpenwidth := canvas.Pen.width;
    canvas.Pen.Width := 1;
  end;
  if (myshape = 'bezier1') then
    with canvas do
    begin
      pen.mode := pmnotxor;
      polybezier(mybezier);
      mybezier[1] := point(x, y);
      polybezier(mybezier);
    end;
  if (myshape = 'bezier2') then
    with canvas do
    begin
      pen.mode := pmnotxor;
      polybezier(mybezier);
      mybezier[2] := point(x, y);
      polybezier(mybezier);
    end;
  canvas.pen.mode := mypen;
end;

procedure TJvDrawImage.MouseMove(Shift: TShiftState;
  X, Y: Integer);
var
  xp, yp, i, j, x1, y1, x2, y2, h, w, pw, movex, movey: integer;
  myrect: trect;
  color1, color2: tcolor;
  r1, g1, b1, r2, b2, g2: byte;
  dx, dy, angle: extended;

  function rr: integer;
  begin
    result := round(sqrt(sqr(x - myorigin.x) + sqr(y - myorigin.y)));
    if result < 10 then result := 10;
  end;

  procedure moveorigin;
  begin
    myorigin.x := myorigin.x + movex;
    myorigin.y := myorigin.y + movey;
  end;
begin
  decoX := x;
  decoY := y;
  movex := x - myprevpoint.x;
  movey := y - myprevpoint.y;
  // test for scripting
  if ((ssctrl in shift) and (ssalt in shift)) then exit;
  mypen := canvas.pen.mode;
  h := abs(y - myorigin.y);
//  w := abs(x - myorigin.x);
  if myDraw then
  begin
    if (myshape = 'rangemove') or (myshape = 'rangesmear') then
    begin
      with canvas do
      begin
        copymode := cmSrcInvert;
        if myshape = 'rangemove' then
          Draw(myprevpoint.x, myprevpoint.y, clip);
        Draw(x, y, clip);
        myprevpoint := point(x, y);
      end;
    end;
    if myshape = 'airbrush' then
      if AirBrush.Air then
        AirBrush.Draw(canvas, x, y);
    if (myshape = 'zoombrush') or (myshape = 'zoomkeepbrush') then
    begin
      w := canvas.pen.width;
      if w < 5 then w := 50;
      DrawPlasma(x, y, w);
    end;
    if myshape = 'fisheyebrush' then
    begin
      w := canvas.pen.width;
      if w < 5 then w := 50;
      DrawEffectBrush(x, y, w, 0.9, lbfisheye);
    end;
    if myshape = 'fisheyefixbrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) / pi * 0.5 + 0.5;
      if angle < 0.55 then angle := 0.55;
      if angle > 0.99 then angle := 0.99;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, lbfisheye);
    end;
    if myshape = 'twistbrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := abs(arctan2(dy, dx) * 25 / pi) + 1;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, round(angle), lbtwist);
    end;
    if myshape = 'mbHorOval' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.x, myorigin.y, rr, angle, mbHorOval);
    end;
    if myshape = 'mbHorBox' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.x, myorigin.y, rr, angle, mbHorBox);
    end;

    if myshape = 'mbVerOval' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.x, myorigin.y, rr, angle, mbVerOval);
    end;

    if myshape = 'mbVerBox' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawStretchBrush(myorigin.x, myorigin.y, rr, angle, mbVerBox);
    end;

    if myshape = 'mbHor' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbHor);
    end;

    if myshape = 'mbTop' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbTop);
    end;

    if myshape = 'mbBottom' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbBottom);
    end;
    if myshape = 'mbWaste' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbWaste);
    end;
    if myshape = 'mbRound' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbRound);
    end;
    if myshape = 'mbRound2' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbRound2);
    end;

    if myshape = 'mbDiamond' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbDiamond);
    end;
    if myshape = 'mbSplitRound' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbSplitRound);
    end;
    if myshape = 'mbSplitWaste' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, mbSplitWaste);
    end;

    if myshape = 'rimplebrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 10 / pi + 1;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, lbrimple);
    end;

    if myshape = 'rotatebrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 180 / pi;
      DrawEffectBrush(myorigin.x, myorigin.y, rr, angle, lbrotate);
    end;
    if myshape = 'brightnessbrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.x, myorigin.y,
        rr, round(angle), lbBrightness);
    end;
    if myshape = 'contrastbrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.x, myorigin.y,
        rr, round(angle), lbContrast);
    end;
    if myshape = 'saturationbrush' then
    begin
      if ssalt in shift then moveorigin;
      dx := x - myorigin.x;
      if dx = 0 then dx := 0.01;
      dy := y - myorigin.y;
      angle := arctan2(dy, dx) * 100 / pi;
      DrawLightBrush(myorigin.x, myorigin.y,
        rr, round(angle), lbContrast);
    end;

    if myshape = 'Bluebrush' then
      DrawColorCircle(x, y, 0);
    if myshape = 'Greenbrush' then
      DrawColorCircle(x, y, 1);
    if myshape = 'redbrush' then
      DrawColorCircle(x, y, 2);
    if myshape = 'notBluebrush' then
      DrawColorCircle(x, y, 3);
    if myshape = 'notGreenbrush' then
      DrawColorCircle(x, y, 4);
    if myshape = 'notredbrush' then
      DrawColorCircle(x, y, 5);
    if myshape = 'halfBluebrush' then
      DrawColorCircle(x, y, 6);
    if myshape = 'halfGreenbrush' then
      DrawColorCircle(x, y, 7);
    if myshape = 'halfredbrush' then
      DrawColorCircle(x, y, 8);
    if myshape = 'darkerbrush' then
      DrawDarkerCircle(x, y, 9);
    if myshape = 'lighterbrush' then
      DrawLighterCircle(x, y, 10);
    if myshape = 'graybrush' then
      DrawLighterCircle(x, y, 11);
    if myshape = 'rollmixbrush' then
      DrawLighterCircle(x, y, 12);
    if myshape = 'smoothbrush' then
      DrawLighterCircle(x, y, 13);

    if myshape = 'gradientbrush' then
    begin
      with canvas do
      begin
        if ssalt in shift then
        begin
          color1 := pixels[x, y - pen.width];
          color2 := pixels[x, y + pen.width];
          DrawVGradientBrush(color1, color2, y - pen.width, y + pen.width, x);
          DrawVGradientBrush(color1, color2, y - pen.width, y + pen.width, x - 1);
          DrawVGradientBrush(color1, color2, y - pen.width, y + pen.width, x - 2);
        end
        else
        begin
          color1 := pixels[x - pen.width, y];
          color2 := pixels[x + pen.width, y];
          DrawGradientBrush(color1, color2, x - pen.width, x + pen.width, y);
          DrawGradientBrush(color1, color2, x - pen.width, x + pen.width, y + 1);
          DrawGradientBrush(color1, color2, x - pen.width, x + pen.width, y + 2);
        end;
      end;
    end;

    if myshape = 'cube1' then
      with canvas do
      begin
        pen.mode := pmnotxor;
        DrawCube;
        myskew[4] := point(x, y);
        DrawCube;
      end;
    if (myshape = 'rectangle') or (myshape = 'cube') or (myshape = 'maze')
      or (myshape = 'Interprect') or (myshape = 'interColumn') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        rectangle(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y);
        rectangle(myorigin.x, myorigin.y, x, y);
        myprevpoint := point(x, y);
      end;

    if myshape = 'roundrect' then
      with canvas do
      begin
        pen.mode := pmnotxor;
        roundrect(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y, myround, myround);
        roundrect(myorigin.x, myorigin.y, x, y, myround, myround);
        myprevpoint := point(x, y);
      end;
    if myshape = 'Blocks' then
      canvas.FillRect(PointToBlock(x, y));

    if (myshape = 'ellipse') or (myshape = 'globe') or (myshape = 'interSphere')
      or (myshape = 'MultiSphere') or (myshape = 'DropletSphere')
      or (myshape = 'WaveSphere') or (myshape = 'RisingWaveSphere')
      or (myshape = 'decooval') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        ellipse(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y);
        ellipse(myorigin.x, myorigin.y, x, y);
        myprevpoint := point(x, y);
      end;
    if (myshape = 'chord') or (myshape = 'pie') or (myshape = 'arc') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        ellipse(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y);
        ellipse(myorigin.x, myorigin.y, x, y);
        myprevpoint := point(x, y);
      end;
    if myshape = 'skewrect1' then
      with canvas do
      begin
        pen.mode := pmnotxor;
        DrawSkew;
        myskew[2] := point(x, y);
        myskew[3].x := myskew[0].x + (myskew[2].x - myskew[1].x);
        myskew[3].y := myskew[0].y + (myskew[2].y - myskew[1].y);
        DrawSkew;
      end;

    if myshape = 'triangle1' then
      with canvas do
      begin
        pen.mode := pmnotxor;
        DrawTriangle;
        myskew[2] := point(x, y);
        DrawTriangle;
      end;
    if (myshape = 'polyline') or (myshape = 'Polygon') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        penpos := point(myorigin.x, myorigin.y);
        lineto(myprevpoint.x, myprevpoint.y);
        penpos := point(myorigin.x, myorigin.y);
        lineto(x, y);
        myprevpoint := point(x, y);
      end;

    if myshape = 'polybezier' then
      with canvas do
      begin
        pen.mode := pmnotxor;
        mybezier[0] := point(myorigin.x, myorigin.y);
        mybezier[3] := point(myprevpoint.x, myprevpoint.y);
        if not bezierfix1 then
        begin
          mybezier[1].x := mybezier[0].x;
          mybezier[1].y := mybezier[3].y;
        end;
        if not bezierfix2 then
        begin
          mybezier[2].x := mybezier[3].x;
          mybezier[2].y := mybezier[0].y;
        end;
        polybezier(mybezier);
        mybezier[3] := point(x, y);
        if (ssctrl in shift) then
        begin
          bezierfix1 := true;
          mybezier[1] := mybezier[3];
        end;
        if not bezierfix1 then
        begin
          mybezier[1].x := mybezier[0].x;
          mybezier[1].y := mybezier[3].y;
        end;
        if (ssshift in shift) then
        begin
          bezierfix2 := true;
          mybezier[2] := mybezier[3];
        end;
        if not bezierfix2 then
        begin
          mybezier[2].x := mybezier[3].x;
          mybezier[2].y := mybezier[0].y;
        end;

        polybezier(mybezier);
        myprevpoint := point(x, y);
      end;
    if (myshape = 'line') or (myshape = 'rotateText') or (myshape = 'Star')
      or (myshape = 'spiral') or (myshape = 'skewrect') or (myshape = 'triangle')
      or (myshape = 'cone') or (myshape = 'Spiro') or
      (myshape = 'decobar') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        penpos := point(myorigin.x, myorigin.y);
        lineto(myprevpoint.x, myprevpoint.y);
        penpos := point(myorigin.x, myorigin.y);
        lineto(x, y);
        myprevpoint := point(x, y);
      end;
    if (myshape = 'bezier') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        mybezier[0] := point(myorigin.x, myorigin.y);
        mybezier[1] := mybezier[0];
        mybezier[3] := point(myprevpoint.x, myprevpoint.y);
        mybezier[2] := mybezier[3];
        polybezier(mybezier);
        mybezier[3] := point(x, y);
        mybezier[2] := mybezier[3];
        polybezier(mybezier);
        myprevpoint := point(x, y);
      end;
    if (myshape = 'bezier1') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        polybezier(mybezier);
        mybezier[1] := point(x, y);
        polybezier(mybezier);
      end;
    if (myshape = 'bezier2') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        polybezier(mybezier);
        mybezier[2] := point(x, y);
        polybezier(mybezier);
      end;
    if myshape = 'spray' then
      for i := 1 to 10 do
      begin
        xp := random(30) - 15;
        yp := random(30) - 15;
        canvas.Pixels[x + xp, y + yp] := canvas.Brush.Color;
      end;

    if (myshape = 'Waveline') or (myshape = 'fastWaveline')
      or (myshape = 'colorWaveline') then
      with canvas do
      begin
        canvas.lineto(x, y);
        myprevpoint := point(x, y);
      end;

    if myshape = 'borderWaveline' then
    begin
      canvas.moveto(myprevpoint.x, myprevpoint.y);
      canvas.lineto(x, y);
      canvas.MoveTo(width - myprevpoint.x, height - myprevpoint.y);
      canvas.lineto(width - x, height - y);
      myprevpoint := point(x, y);
    end;

    if myshape = 'decoline' then
    begin
      with canvas do
      begin
        pw := pen.width;
        pen.color := Wavepen;
        pen.mode := pmcopy;
        moveto(myprevpoint.x, myprevpoint.y);
        lineto(x, y);
        pen.width := pw * 2;
        pen.mode := pmmasknotpen;
        MoveTo(myprevpoint.x, myprevpoint.y);
        lineto(x, y);
        myprevpoint := point(x, y);
        pen.width := pw;
        pen.mode := pmcopy;
        pen.mode := mypen;
      end;
    end;

    if (myshape = 'freehand') or (myshape = 'mixbrush') then
      with canvas do
      begin
        canvas.lineto(x, y);
        myprevpoint := point(x, y)
      end;
    if myshape = 'cloneall' then
      with canvas do
      begin
        x1 := myorigin.x - TargetPoint.x;
        y1 := myorigin.y - Targetpoint.y;
        x2 := x - x1;
        y2 := y - y1;
        copymode := cmsrccopy;
        i := pen.width;
        copyrect(rect(x, y, x + i, y + i), canvas,
          rect(x2, y2, x2 + i, y2 + i));
      end;

    if myshape = 'clonenottarget' then
      with canvas do
      begin
        x1 := myorigin.x - TargetPoint.x;
        y1 := myorigin.y - Targetpoint.y;
        x2 := x - x1;
        y2 := y - y1;
        i := pen.width;
        PutClip(rect(x2, y2, x2 + i, y2 + i));
        clip.transparent := true;
        clip.TransparentColor := pixels[Targetpoint.x, Targetpoint.y];
        Draw(x, y, clip);
        clip.transparent := false;
      end;

    if (myshape = 'paste') and (ssshift in shift) then
    begin
      myrect := rect(0, 0, 0, 0);
      myrect.left := x;
      myrect.top := y;
      myrect.right := x + mycliprect.right - mycliprect.left;
      myrect.bottom := y + mycliprect.bottom - mycliprect.Top;
      canvas.CopyRect(myrect, canvas, mycliprect);
    end;
    if myshape = 'sym' then
      DrawSyms(x, y);
    if myshape = 'sline' then
    begin
      if myslinedir = 'none' then
        if abs(x - myorigin.x) >= abs(y - myorigin.y) then
          myslinedir := 'h'
        else
          myslinedir := 'v';
      if (myslinedir = 'h') and (abs(y - myprevpoint.y) > myslinetol) then
        myslinedir := 'v';
      if (myslinedir = 'v') and (abs(x - myprevpoint.x) > myslinetol) then
        myslinedir := 'h';
      if myslinedir = 'h' then
      begin
        canvas.lineto(x, myprevpoint.y);
        myprevpoint.x := x;
      end;
      if myslinedir = 'v' then
      begin
        canvas.lineto(myprevpoint.x, y);
        myprevpoint.y := y;
      end;
    end;

    if myshape = 'vmirror' then
    begin
      x1 := myprevpoint.x;
      y1 := myprevpoint.y;
      x2 := width;
//      y2 := height;
      canvas.PenPos := point(x2 - x1, y1);
      canvas.lineto(x2 - x, y);
      canvas.penpos := point(x1, y1);
      canvas.lineto(x, y);
      myprevpoint := point(x, y)
    end;
    if myshape = 'cmirror' then
    begin
      x1 := myprevpoint.x;
      y1 := myprevpoint.y;
      x2 := width;
      y2 := height;
      canvas.PenPos := point(x2 - x1, y2 - y1);
      canvas.lineto(x2 - x, y2 - y);
      canvas.penpos := point(x1, y1);
      canvas.lineto(x, y);
      myprevpoint := point(x, y)
    end;
    if myshape = 'mirror4' then
    begin
      x1 := myprevpoint.x;
      y1 := myprevpoint.y;
      x2 := width;
      y2 := height;
      canvas.PenPos := point(x2 - x1, y2 - y1);
      canvas.lineto(x2 - x, y2 - y);
      canvas.PenPos := point(x2 - x1, y1);
      canvas.lineto(x2 - x, y);
      canvas.PenPos := point(x1, y2 - y1);
      canvas.lineto(x, y2 - y);
      canvas.penpos := point(x1, y1);
      canvas.lineto(x, y);
      myprevpoint := point(x, y)
    end;
    if myshape = 'hmirror' then
    begin
      x1 := myprevpoint.x;
      y1 := myprevpoint.y;
//      x2 := width;
      y2 := height;
      canvas.PenPos := point(x1, y2 - y1);
      canvas.lineto(x, y2 - y);
      canvas.penpos := point(x1, y1);
      canvas.lineto(x, y);
      myprevpoint := point(x, y)
    end;
    if (myshape = 'snapshot') or (myshape = 'bars') or (myshape = 'border') then
      with canvas do
      begin
        pen.mode := pmnotxor;
        pen.style := psdot;
        rectangle(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y);
        rectangle(myorigin.x, myorigin.y, x, y);
        myprevpoint := point(x, y);
        pen.style := pssolid;
      end;
  end;
  canvas.pen.mode := mypen;
  myprevpoint := point(x, y);
end;

procedure TJvDrawImage.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  myrect: trect;
  xs, ys, xt, yt, i, tangle: integer;
  c: tcolor;
  drw: string;
  apoint: tpoint;
  bitmap: tbitmap;
  x1, y1, x2, y2: integer;
  dcurve: array[0..3] of tpoint;
  rgn: HRGN;
  r1, g1, b1, r2, g2, b2: byte;
  r, g, b, dr, dg, db: extended;
  Acolor, pcolor: tcolor;
  pw: integer;
begin
  //canvas.pen.color:=Wavepen;
  //canvas.brush.color:=Wavebrush;

  if ((ssctrl in shift) and (ssalt in shift)) then exit;
  if button = mbright then exit;
  mypen := canvas.pen.mode;

  if myshape = 'zoombrush' then
    canvas.Draw(0, 0, zoomclip);
  if myshape = 'transcopy' then
  begin
    clipcm := cmSrcCopy;
    SetClip(clwhite);
    CopyClip;
    myrect := rect(x, y, x + clip.width - 1, y + clip.height - 1);
    canvas.brushcopy(myrect, clip,
      rect(0, 0, clip.width, clip.height), RangeTransColor);
    myDraw := false;
  end;
  if myshape = 'cube1' then
  begin
    if mypen <> pmnotxor then
      DrawCube;
    myshape := 'cube2';
  end;

  if myshape = 'TexLines' then
  begin
    DrawTexLines(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Texovals' then
  begin
    DrawTexOvals(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Blurovals' then
  begin
    DrawBlurOvals(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Texcurves' then
  begin
    DrawTexCurves(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Blurcurves' then
  begin
    DrawBlurCurves(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Texpoly' then
  begin
    DrawTexPoly(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'Blurpoly' then
  begin
    DrawBlurPoly(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'TexRects' then
  begin
    DrawTexRects(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'BlurRects' then
  begin
    DrawBlurRects(myorigin.x, myorigin.y, x, y);
  end;
  if myshape = 'BlurLines' then
  begin
    DrawBlurLines(myorigin.x, myorigin.y, x, y);
  end;

  if myshape = 'cube' then
  begin
    myskew[0] := myorigin;
    myskew[2] := point(x, y);
    myskew[4] := myskew[2];
    myskew[1].x := myskew[2].x;
    myskew[1].y := myskew[0].y;
    myskew[3].x := myskew[0].x;
    myskew[3].y := myskew[2].y;
    DrawCube;
    myshape := 'cube1';
  end;

  if myshape = 'Interprect' then
    InterpRect(myorigin.x, myorigin.y, x, y);

  if myshape = 'interColumn' then
    DrawColumn(myorigin.x, myorigin.y, x, y);

  if myshape = 'interSphere' then
    if ((myorigin.x <> x) and (myorigin.y <> y)) then
    begin
      if ssalt in shift then
        DrawSphere(canvas.pixels[myorigin.x, myorigin.y],
          canvas.pixels[x, y], myorigin.x, myorigin.y, x, y)
      else
        DrawSphere(Wavepen, Wavebrush, myorigin.x, myorigin.y, x, y)
    end;

  if myshape = 'MultiSphere' then
  begin
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(myorigin.x, myorigin.y, x, y);
    canvas.pen.mode := pmcopy;
    if ((myorigin.x <> x) and (myorigin.y <> y)) then
    begin
      if ssalt in shift then
        DrawMultiSphere(canvas.pixels[myorigin.x, myorigin.y],
          canvas.pixels[x, y], myorigin.x, myorigin.y, x, y)
      else
        DrawMultiSphere(Wavepen, Wavebrush, myorigin.x, myorigin.y, x, y)
    end;
  end;

  if myshape = 'DropletSphere' then
  begin
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(myorigin.x, myorigin.y, x, y);
    canvas.pen.mode := pmcopy;
    if ((myorigin.x <> x) and (myorigin.y <> y)) then
    begin
      if ssalt in shift then
        DrawDropletSphere(canvas.pixels[myorigin.x, myorigin.y],
          canvas.pixels[x, y], myorigin.x, myorigin.y, x, y)
      else
        DrawDropletSphere(Wavepen, Wavebrush, myorigin.x, myorigin.y, x, y)
    end;
  end;

  if myshape = 'WaveSphere' then
  begin
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(myorigin.x, myorigin.y, x, y);
    canvas.pen.mode := pmcopy;
    if ((myorigin.x <> x) and (myorigin.y <> y)) then
    begin
      if ssalt in shift then
        DrawWaveSphere(canvas.pixels[myorigin.x, myorigin.y],
          canvas.pixels[x, y], myorigin.x, myorigin.y, x, y)
      else
        DrawWaveSphere(Wavepen, Wavebrush, myorigin.x, myorigin.y, x, y)
    end;
  end;

  if myshape = 'RisingWaveSphere' then
  begin
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(myorigin.x, myorigin.y, x, y);
    canvas.pen.mode := pmcopy;
    if ((myorigin.x <> x) and (myorigin.y <> y)) then
    begin
      if ssalt in shift then
        DrawRisingWaveSphere(canvas.pixels[myorigin.x, myorigin.y],
          canvas.pixels[x, y], myorigin.x, myorigin.y, x, y)
      else
        DrawRisingWaveSphere(Wavepen, Wavebrush, myorigin.x, myorigin.y, x, y)
    end;
  end;

  if myshape = 'rectangle' then
  begin
    if mypen <> pmnotxor then
      canvas.rectangle(myorigin.x, myorigin.y, x, y);
    if Stars > 1 then
    begin
      xs := (x - myorigin.x) div 2 div Stars;
      ys := (y - myorigin.y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        canvas.rectangle(myorigin.x + i * xs, myorigin.y + i * ys, x - i * xs, y - i * ys);
      end;
    end;
  end;

  if myshape = 'maze' then
  begin
    if mypen <> pmnotxor then
      canvas.rectangle(myorigin.x, myorigin.y, x, y);
    xs := (x - myorigin.x) div 10;
    ys := (y - myorigin.y) div 10;
    xt := myorigin.x;
    yt := myorigin.y;
    for i := 1 to 10 do
    begin
      canvas.MoveTo(xt + i * xs, y);
      canvas.lineto(x, y - i * ys);
      canvas.MoveTo(x, y - i * ys);
      canvas.lineto(x - i * xs, yt);
      canvas.MoveTo(x - i * xs, yt);
      canvas.lineto(xt, yt + i * ys);
      canvas.MoveTo(xt, yt + i * ys);
      canvas.lineto(xt + i * xs, y);
    end;
  end;

  if myshape = 'roundrect' then
  begin
    if mypen <> pmnotxor then
      canvas.roundrect(myorigin.x, myorigin.y, x, y, myround, myround);
    if Stars > 1 then
    begin
      xs := (x - myorigin.x) div 2 div Stars;
      ys := (y - myorigin.y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        canvas.roundrect(myorigin.x + i * xs, myorigin.y + i * ys, x - i * xs, y - i * ys, myround, myround);
      end;
    end;
  end;

  if myshape = 'Blocks' then
    canvas.FillRect(PointToBlock(x, y));

  if myshape = 'Star' then
  begin
    with canvas do
    begin
      pen.mode := pmnotxor;
      penpos := point(myorigin.x, myorigin.y);
      lineto(myprevpoint.x, myprevpoint.y);
    end;
    canvas.pen.mode := mypen;
    for i := 1 to Stars do
    begin
      apoint := ReduceVector(myorigin, point(x, y), i / Stars);
      Star(apoint.x, apoint.y);
    end;
  end;

  if myshape = 'spiral' then
  begin
    with canvas do
    begin
      pen.mode := pmnotxor;
      penpos := point(myorigin.x, myorigin.y);
      lineto(myprevpoint.x, myprevpoint.y);
    end;
    canvas.pen.mode := mypen;
    apoint := point(100 * x, 100 * y);
    myorigin.x := 100 * myorigin.x;
    myorigin.y := 100 * myorigin.y;
    for i := 1 to variant(Spirals * 36) do
    begin
      apoint := Rotate(myorigin, apoint, spiraldir * pi / 18);
      apoint := ReduceVector(myorigin, apoint, spiralfactor);
      canvas.lineto(apoint.x div 100, apoint.y div 100);
    end;
  end;

  if myshape = 'ellipse' then
  begin
    if mypen <> pmnotxor then
      canvas.ellipse(myorigin.x, myorigin.y, x, y);
    if Stars > 1 then
    begin
      xs := (x - myorigin.x) div 2 div Stars;
      ys := (y - myorigin.y) div 2 div Stars;
      for i := 1 to Stars - 1 do
      begin
        canvas.ellipse(myorigin.x + i * xs, myorigin.y + i * ys, x - i * xs, y - i * ys);
      end;
    end;
  end;

  if myshape = 'globe' then
  begin
    if mypen <> pmnotxor then
      canvas.ellipse(myorigin.x, myorigin.y, x, y);
    xs := (x - myorigin.x) div 20;
    ys := (y - myorigin.y) div 20;
    for i := 1 to 10 do
    begin
      canvas.ellipse(myorigin.x + i * xs, myorigin.y, x - i * xs, y);
      canvas.ellipse(myorigin.x, myorigin.y + i * ys, x, y - i * ys);
    end;
  end;

  if myshape = 'chord2' then
  begin
    mychord[7] := x;
    mychord[8] := y;
    myshape := 'chord3';
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    canvas.pen.mode := mypen;
    canvas.Chord(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if myshape = 'chord1' then
  begin
    mychord[5] := x;
    mychord[6] := y;
    myshape := 'chord2';
  end;

  if myshape = 'chord' then
  begin
    mychord[1] := myorigin.x;
    mychord[2] := myorigin.y;
    mychord[3] := x;
    mychord[4] := y;
    myshape := 'chord1';
  end;

  if myshape = 'arc2' then
  begin
    mychord[7] := x;
    mychord[8] := y;
    myshape := 'arc3';
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    canvas.pen.mode := mypen;
    canvas.arc(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if myshape = 'arc1' then
  begin
    mychord[5] := x;
    mychord[6] := y;
    myshape := 'arc2';
  end;

  if myshape = 'arc' then
  begin
    mychord[1] := myorigin.x;
    mychord[2] := myorigin.y;
    mychord[3] := x;
    mychord[4] := y;
    myshape := 'arc1';
  end;

  if myshape = 'pie2' then
  begin
    mychord[7] := x;
    mychord[8] := y;
    myshape := 'pie3';
    canvas.pen.mode := pmnotxor;
    canvas.ellipse(mychord[1], mychord[2], mychord[3], mychord[4]);
    canvas.pen.mode := mypen;
    canvas.pie(mychord[1], mychord[2], mychord[3], mychord[4], mychord[5], mychord[6], mychord[7], mychord[8]);
  end;

  if myshape = 'pie1' then
  begin
    mychord[5] := x;
    mychord[6] := y;
    myshape := 'pie2';
  end;

  if myshape = 'pie' then
  begin
    mychord[1] := myorigin.x;
    mychord[2] := myorigin.y;
    mychord[3] := x;
    mychord[4] := y;
    myshape := 'pie1';
  end;

  if myshape = 'skewrect1' then
  begin
    if mypen <> pmnotxor then
      DrawSkew;
    myshape := 'skewrect2';
  end;

  if myshape = 'skewrect' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    if mypen <> pmnotxor then
      canvas.lineto(x, y);
    myskew[0] := myorigin;
    myskew[1] := point(x, y);
    myskew[2] := myskew[1];
    myskew[3] := myskew[0];
    myshape := 'skewrect1';
  end;

  if myshape = 'triangle1' then
  begin
    if mypen <> pmnotxor then
      DrawTriangle;
    myshape := 'triangle2';
  end;

  if myshape = 'triangle' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    if mypen <> pmnotxor then
      canvas.lineto(x, y);
    myskew[0] := myorigin;
    myskew[1] := point(x, y);
    myskew[2] := myskew[1];
    myshape := 'triangle1';
  end;

  if myshape = 'decobar' then
  begin
    picture.bitmap.PixelFormat := pf24bit;
    with canvas do
    begin
      pw := pen.width;
      pcolor := pen.color;
      Acolor := colortoRGB(Wavebrush);
      r1 := getRed(Acolor);
      r := r1;
      g1 := getGreen(acolor);
      g := g1;
      b1 := getBlue(acolor);
      b := b1;
      Acolor := colortoRGB(pen.color);
      r2 := getred(Acolor);
      g2 := getGreen(acolor);
      b2 := getBlue(acolor);
      dr := (r1 - r2) / (pw / 3);
      dg := (g1 - g2) / (pw / 3);
      db := (b1 - b2) / (pw / 3);
      if pw < 30 then
        pen.width := 30;
      for i := 1 to pen.width div 3 do
      begin
        r := r - dr;
        g := g - dg;
        b := b - db;
        pen.color := rgb(round(r), round(g), round(b));
        moveto(myorigin.x, myorigin.y);
        lineto(x, y);
        pen.width := pen.width - 2;
      end;
      pen.width := pw;
      pen.color := pcolor;
    end;
  end;

  if myshape = 'decooval' then
  begin
    picture.bitmap.PixelFormat := pf24bit;
    with canvas do
    begin
      pen.mode := pmnotxor;
      ellipse(myorigin.x, myorigin.y, myprevpoint.x, myprevpoint.y);
      pen.mode := pmcopy;
      pw := pen.width;
      brush.style := bsclear;
      Acolor := colortoRGB(Wavebrush);
      r1 := getred(Acolor);
      r := r1;
      g1 := getGreen(acolor);
      g := g1;
      b1 := getBlue(acolor);
      b := b1;
      Acolor := colortoRGB(Wavepen);
      r2 := getred(Acolor);
      g2 := getGreen(acolor);
      b2 := getBlue(acolor);
      dr := (r1 - r2) / (pw / 3);
      dg := (g1 - g2) / (pw / 3);
      db := (b1 - b2) / (pw / 3);
      if pw < 30 then
        pen.width := 30;
      for i := 1 to pen.width div 3 do
      begin
        pen.width := pen.width - 2;
        r := r - dr;
        g := g - dg;
        b := b - db;
        pen.color := rgb(round(r), round(g), round(b));
        ellipse(myorigin.x, myorigin.y, x, y);
      end;
      pen.width := pw;
    end;
  end;

  if (myshape = 'polyline') or (myshape = 'Polygon') then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    if mypen <> pmnotxor then
      canvas.lineto(x, y);
    if freepolycount = 0 then
    begin
      freepoly[0] := myorigin;
      inc(freepolycount);
    end
    else
    begin
      freepoly[freepolycount] := point(x, y);
      if freepolycount < 100 then
        inc(freepolycount);
    end;
  end;

  if myshape = 'line' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    if mypen <> pmnotxor then
    begin
      canvas.lineto(x, y);
    end;
  end;

  if myshape = 'Spiro' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    canvas.pen.mode := pmnotxor;
    canvas.lineto(x, y);
    canvas.pen.mode := mypen;
    DrawSpiro(myorigin, point(x, y));
  end;

  if myshape = 'cone' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    canvas.pen.mode := pmnotxor;
    canvas.lineto(x, y);
    canvas.pen.mode := mypen;
    xt := (picture.bitmap.Width - 2 * myorigin.x) div 20;
    xs := (picture.bitmap.Width - 2 * x) div 20;
    x := picture.bitmap.width div 2;
    with canvas do
    begin
      for i := 0 to 10 do
      begin
        moveto(x + i * xt, myorigin.y);
        lineto(x + i * xs, y);
        moveto(x - i * xt, myorigin.y);
        lineto(x - i * xs, y);
      end;
      moveto(x + 10 * xt, myorigin.y);
      lineto(x - 10 * xt, myorigin.y);
      moveto(x + 10 * xs, y);
      lineto(x - 10 * xs, y);
    end;
  end;

  {if myshape='polybezier' then begin
    canvas.penpos:=point(myorigin.x,myorigin.y);
     if mypen<>pmnotxor then
       begin
       mybezier[0]:=myorigin;
       mybezier[3]:=point(x,y);
       if not bezierfix1 then begin
         mybezier[1].x:=mybezier[0].x;
         mybezier[1].y:=mybezier[3].y;
         end;
       if not bezierfix2 then begin
         mybezier[2].x:=mybezier[3].x;
         mybezier[2].y:=mybezier[0].y;
         end;
       canvas.polybezier(mybezier);
       bezierfix1:=false;
       bezierfix2:=false;
       end;
    end;}

  if myshape = 'bezier2' then
  begin
    canvas.penpos := point(myorigin.x, myorigin.y);
    if mypen <> pmnotxor then
    begin
      mybezier[2] := point(x, y);
      canvas.polybezier(mybezier);
    end;
    myshape := 'bezier3';
  end;

  if myshape = 'bezier1' then myshape := 'bezier2';
  if myshape = 'bezier' then myshape := 'bezier1';
  if myshape = 'bezier3' then myshape := 'bezier';

  if myshape = 'floodfill' then
  begin
    if ssalt in shift then
      canvas.floodfill(x, y, canvas.pen.color, fsborder)
    else
      canvas.floodfill(x, y, canvas.pixels[x, y], fssurface);
  end;

  if myshape = 'snapshot' then
  begin
    with canvas do
    begin
      pen.mode := pmnotxor;
      pen.style := psdot;
      rectangle(myorigin.x, myorigin.y, x, y);
      pen.style := pssolid;
    end;
    mycliprect := rect(myorigin.x, myorigin.y, x, y);
    canvas.brush.Style := myoldbrushstyle;
    canvas.Pen.Width := myoldpenwidth;
    myshape := '';
  end;

  if myshape = 'bars' then
  begin
    with canvas do
    begin
      pen.mode := pmnotxor;
      pen.style := psdot;
      rectangle(myorigin.x, myorigin.y, x, y);
      pen.style := pssolid;
    end;
    DrawBars(myorigin.x, myorigin.y, x, y);
  end;

  if myshape = 'border' then
  begin
    with canvas do
    begin
      pen.mode := pmnotxor;
      pen.style := psdot;
      rectangle(myorigin.x, myorigin.y, x, y);
      pen.style := pssolid;
    end;
    Drawborders(myorigin.x, myorigin.y, x, y);
  end;

  if myshape = 'paste' then
  begin
    myrect := rect(0, 0, 0, 0);
    myrect.left := x;
    myrect.top := y;
    myrect.right := x + mycliprect.right - mycliprect.left;
    myrect.bottom := y + mycliprect.bottom - mycliprect.Top;
    canvas.CopyRect(myrect, canvas, mycliprect);
  end;

  if myshape = 'pastecolor' then
  begin
    clipcm := cmsrccopy;
    SetClip(clwhite);
    CopyClip;
    FX.ExtractColor(clip, canvas.brush.color);
    canvas.Draw(x, y, clip);
    clip.transparent := false;
  end;

  if myshape = 'pastecolorx' then
  begin
    clipcm := cmsrccopy;
    SetClip(clwhite);
    CopyClip;
    FX.ExcludeColor(clip, canvas.brush.color);
    canvas.Draw(x, y, clip);
    clip.transparent := false;
  end;

  if myshape = 'zoomkeepbrush' then
  begin
    myshape := 'freehand';
    canvas.pen.Width := 2;
  end;

  if myshape = 'paintpick' then
  begin
    if ssalt in shift then
      canvas.brush.color := MixColors(
        canvas.pixels[x - 5, y],
        canvas.pixels[x + 5, y])
    else
      canvas.brush.color := canvas.pixels[x, y];
    ColorPicked(canvas.brush.color);
    myshape := '';
  end;
  canvas.pen.mode := mypen;
  if not ((myshape = 'Polygon') or
    (myshape = 'polyline') or
    (myshape = 'polybezier')) then
  begin
    myDraw := false;
  end;
end; // end of mouseup

constructor TJvDrawImage.Create(AOwner: Tcomponent);
begin
  inherited;
  width := 256;
  height := 256;
  Clip := Tbitmap.create;
  ZoomClip := TBitmap.create;
  FAirBrush := TJvAirBrush.Create(self);
  FX := TJvPaintFX.Create(self);
  TargetPoint := point(0, 0);
  NSpiro := 40;
  RangeTransColor := clwhite;
  zoomrect := rect(0, 0, 50, 50);
  mycliprect := rect(0, 0, 256, 256);
  //spiral number, direction and factor
  Spirals := 3;
  spiralfactor := 0.99;
  spiraldir := 1;
  // number of points for Star shape
  Starpoints := 5;
  Stars := 1;
  // tolerance for straight line Drawing
  myslinetol := 5;

  mypenstyle := pssolid;
  // number of Blocks wide and heigh
  Blocks := 32;
  // rounding of roundrect
  myround := 10;
  // default Drawing mode
  myshape := 'line';
  FillSinPixs;
  FillGonio;
  TraceB := $00;
  Fshapes := TStringlist.Create;
  BuildShapeList;
  PainterEffectsF := TPainterEffectsF.Create(self);
  PainterEffectsF.setDrawImage(self);
  QuickPreviewF := TQuickPreviewF.Create(self);
  QuickPreviewF.setDrawImage(self);
  PainterQBF := TPainterQBF.create(self);
  PainterQBF.setDrawImage(self);

end;

procedure TJvDrawImage.BuildShapeList;
begin
  with Fshapes do
  begin
    append('airbrush');
    append('arc');
    append('bars');
    append('bezier');
    append('Blocks');
    append('Bluebrush');
    append('Blurcurves');
    append('BlurLines');
    append('Blurovals');
    append('Blurpoly');
    append('BlurRects');
    append('border');
    append('brightnessbrush');
    append('chord');
    append('cloneall');
    append('clonenottarget');
    append('cmirror');
    append('cone');
    append('contrastbrush');
    append('cube');
    append('darkerbrush');
    append('decobar');
    append('decoval');
    append('DropletSphere');
    append('ellipse');
    append('fisheyebrush');
    append('fisheyefixbrush');
    append('floodfill');
    append('freehand');
    append('globe');
    append('gradientbrush');
    append('graybrush');
    append('Greenbrush');
    append('halfBluebrush');
    append('halfGreenbrush');
    append('halfredbrush');
    append('hmirror');
    append('interColumn');
    append('Interprect');
    append('interSphere');
    append('lighterbrush');
    append('line');
    append('maze');
    append('mbBottom');
    append('mbDiamond');
    append('mbHor');
    append('mbHorBox');
    append('mbHorOval');
    append('mbRound');
    append('mbRound2');
    append('mbSplitRound');
    append('mbSplitWaste');
    append('mbTop');
    append('mbVerBox');
    append('mbVerOval');
    append('mbWaste');
    append('mirror4');
    append('mixbrush');
    append('MultiSphere');
    append('notBluebrush');
    append('notGreenbrush');
    append('notredbrush');
    append('paintpick');
    append('paste');
    append('pastecolor');
    append('pastecolorx');
    append('pie');
    append('polybezier');
    append('Polygon');
    append('polyline');
    append('rangemove');
    append('rangesmear');
    append('rectangle');
    append('redbrush');
    append('rimplebrush');
    append('RisingWaveSphere');
    append('rollmixbrush');
    append('rotatebrush');
    append('roundrect');
    append('saturationbrush');
    append('skewrect');
    append('sline');
    append('smoothbrush');
    append('snapshot');
    append('spiral');
    append('Spiro');
    append('Star');
    append('sym');
    append('Texcurves');
    append('TexLines');
    append('Texovals');
    append('Texpoly');
    append('TexRects');
    append('transcopy');
    append('triangle');
    append('twistbrush');
    append('vmirror');
    append('WaveSphere');
    append('zoombrush');
    append('zoomkeepbrush');
  end;
end;

destructor TJvDrawImage.Destroy;
begin
  Fshapes.free;
  Clip.free;
  ZoomClip.free;
  FAirBrush.free;
  FX.free;
  PainterEffectsF.free;
  QuickPreviewF.free;
  PainterQBF.free;
  inherited;
end;

procedure TJvDrawImage.SetPolygonChecked(const Value: boolean);
begin
  FPolygonChecked := Value;
end;

procedure TJvDrawImage.ColorPicked(AColor: Tcolor);
begin
  if assigned(OnColorPicked) then
    OnColorPicked(self, AColor);
end;

procedure TJvDrawImage.SetOnColorPicked(const Value: TColorPicked);
begin
  FOnColorPicked := Value;
end;

procedure TJvDrawImage.SetShape(const Value: string);
begin
  myshape := Value;
end;

procedure TJvDrawImage.Loaded;
begin
  inherited;
  autosize := true;
  picture.bitmap.height := 256;
  picture.bitmap.width := 256;
  canvas.Brush.color := clwhite;
  picture.bitmap.canvas.fillrect(rect(0, 0, picture.bitmap.width, picture.bitmap.height));
  canvas.brush.Style := bsclear;
  canvas.pen.color := clwhite;
  canvas.moveto(100, 100);
  canvas.lineto(128, 128);
  canvas.pen.color := clblack;
end;

procedure TJvDrawImage.SetAirBrush(const Value: TJvAirBrush);
begin
  FAirBrush.assign(Value);
end;

procedure TJvDrawImage.SetTransformer(const Value: TJvPaintFX);
begin
  FX.assign(Value);
end;

procedure TJvDrawImage.SetBlocks(const Value: integer);
begin
  FBlocks := Value;
end;

procedure TJvDrawImage.SetSpirals(const Value: integer);
begin
  FSpirals := Value;
end;

procedure TJvDrawImage.SetStarpoints(const Value: integer);
begin
  FStarpoints := Value;
end;

procedure TJvDrawImage.SetStars(const Value: integer);
begin
  FStars := Value;
end;

procedure TJvDrawImage.contrastBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.contrast(clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.saturationBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.saturation(clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.lightnessBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.lightness(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.BlurBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.GaussianBlur(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.splitBlurBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.SplitBlur(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.colornoiseBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.AddColorNoise(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.mononoiseBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.AddmonoNoise(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.smoothBarChange(Sender: TObject);
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.smooth(clip, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.Effects;
begin
  with PainterEffectsF do
  begin
    cxbar.Max := width;
    cybar.max := Height;
    cxbar.position := cxbar.max div 2;
    cybar.position := cybar.max div 2;
    show;
  end;
end;

procedure TJvDrawImage.seamBarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.MakeSeamlessClip(clip, painterEffectsF.Ebar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.mosaicBarChange;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.mosaic(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.twistBarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.twist(clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.fisheyeBarChange;
var
  bm2: tbitmap;
  am: single;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position / 100;
  FX.Fisheye(clip, bm2, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.WaveBarChange;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.Wave(clip, am, 0, 0);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.WaveExtraChange;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.EBar.position;
  FX.Wave(clip, am, 0, 1);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.WaveInfChange;
var
  wa, inf: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  inf := painterEffectsF.Ebar.position;
  wa := paintereffectsF.extraBar.Position;
  FX.Wave(clip, wa, inf, 2);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.RotateBar;
var
  am: extended;
  dst: tbitmap;
  dx, dy: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.width := clip.width;
  dst.height := clip.height;
  dst.pixelformat := pf24bit;
  with PainterEffectsF do
  begin
    am := Ebar.position;
    dx := cxBar.position;
    dy := cyBar.position;
  end;
  FX.SmoothRotate(Clip, Dst, dx, dy, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(Dst);
  QuickPreviewF.image1.update;
  Dst.free;
end;

procedure TJvDrawImage.XFormABarChange;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  XFormA(am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.marbleBarChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble2BarChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble2(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble3BarChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble3(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble4BarChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extraBar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble4(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble5BarChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.EBar.position;
  FX.marble5(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble6barChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble6(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble7barChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble7(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.marble8barChange;
var
  turbulence: integer;
  dst: tbitmap;
  scale: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  dst := tbitmap.create;
  dst.PixelFormat := pf24bit;
  dst.width := clip.width;
  dst.height := clip.height;
  scale := painterEffectsF.extrabar.position;
  turbulence := painterEffectsF.Ebar.position;
  FX.marble8(clip, dst, scale, turbulence);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.embossbarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.Emboss(clip);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.filterRedbarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.filterRed(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.filterGreenbarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.filterGreen(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.filterBluebarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.filterBlue(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.FilterXRedbarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.FilterXRed(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.FilterXGreenbarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.FilterXGreen(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.FilterXBluebarChange;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  FX.FilterXBlue(clip, paintereffectsF.extrabar.position, painterEffectsF.EBar.position);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.SqueezeHorbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbHor);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeTopbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbTop);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeBotbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbBottom);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeDiamondbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbDiamond);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeWastebarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbwaste);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeRoundbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbRound);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SqueezeRound2barChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SqueezeHor(clip, bm2, am, mbround2);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SplitRoundbarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SplitRound(clip, bm2, am, mbSplitRound);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.SplitWastebarChange;
var
  bm2: tbitmap;
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  bm2 := tbitmap.create;
  bm2.width := clip.width;
  bm2.height := clip.height;
  bm2.pixelformat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.SplitRound(clip, bm2, am, mbSplitWaste);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(bm2);
  QuickPreviewF.image1.update;
  bm2.free;
end;

procedure TJvDrawImage.ShearbarChange;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  Shear(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.plasmabarChange;
var
  am, turb, w, h: integer;
  src1, src2: tbitmap;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  w := clip.width;
  h := clip.height;
  src1 := Tbitmap.create;
  src1.width := w;
  src1.height := h;
  src1.PixelFormat := pf24bit;
  src1.canvas.Draw(0, 0, clip);
  src2 := Tbitmap.create;
  src2.width := w;
  src2.height := h;
  src2.PixelFormat := pf24bit;
  src2.canvas.Draw(0, 0, clip);
  am := painterEffectsF.Ebar.position;
  turb := painterEffectsF.extrabar.position;
  if turb < 10 then
  begin
    painterEffectsF.extrabar.position := 10;
    turb := 10;
  end;
  FX.Plasma(src1, src2, clip, am, turb);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
  src2.free;
  src1.free;
end;

procedure TJvDrawImage.DrawMandelJulia(mandel: boolean);
var
  xr, yr: extended;
  x0, y0, x1, y1: extended;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  xr := painterEffectsF.Ebar.position * 0.028;
  yr := painterEffectsF.ExtraBar.Position * 0.009;
  x0 := -2.25 + xr;
  x1 := 0.75;
  y0 := -1.5 + yr;
  y1 := 1.5;
  FX.DrawMandelJulia(clip, x0, y0, x1, y1, 16, mandel);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.DrawTriangles;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.Triangles(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.RippleTooth;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleTooth(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.RippleTriangle;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleTooth(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.RippleRandom;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.RippleRandom(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.TexturizeTile;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.TexturizeTile(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.TexturizeOverlap;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.TexturizeOverlap(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.DrawMap;
var
  am: integer;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  FX.HeightMap(clip, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(clip);
  QuickPreviewF.image1.update;
end;

procedure TJvDrawImage.DrawBlend;
var
  am, w, h: integer;
  src2, dst: tbitmap;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  w := clip.width;
  h := clip.height;
  if not clipboard.HasFormat(CF_BITMAP) then exit;
  src2 := tbitmap.Create;
  src2.assign(clipboard);
  src2.PixelFormat := pf24bit;
  if ((src2.width <> w) or (src2.height <> h)) then
  begin
    src2.free;
    exit;
  end;
  dst := tbitmap.create;
  dst.Width := w;
  dst.Height := h;
  dst.PixelFormat := pf24bit;
  FX.Blend(clip, src2, dst, am / 100);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  src2.free;
  dst.free;
end;

procedure TJvDrawImage.DrawSolarize;
var
  am, w, h: integer;
  dst: tbitmap;

begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  w := clip.width;
  h := clip.height;
  dst := tbitmap.create;
  dst.Width := w;
  dst.Height := h;
  dst.PixelFormat := pf24bit;
  FX.Solorize(clip, dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.Posterize;
var
  am, w, h: integer;
  dst: tbitmap;
begin
  ClipAll;
  clip.PixelFormat := pf24bit;
  am := painterEffectsF.Ebar.position;
  w := clip.width;
  h := clip.height;
  dst := tbitmap.create;
  dst.Width := w;
  dst.Height := h;
  dst.PixelFormat := pf24bit;
  FX.Posterize(clip, dst, am);
  QuickPreviewF.Show;
  QuickPreviewF.Image1.picture.bitmap.assign(dst);
  QuickPreviewF.image1.update;
  dst.free;
end;

procedure TJvDrawImage.Backgrounds;
begin
  PainterQBF.show;
  PainterQBF.BringToFront;
end;

procedure TJvDrawImage.Preview(aBitmap: Tbitmap);
begin
  QuickPreviewF.show;
  QuickPreviewF.image1.picture.bitmap.assign(abitmap);
end;

procedure TJvDrawImage.Trace;
var
  BitMap: TBitMap;
begin
  BitMap := TBitMap.create;
  Bitmap.Assign(picture.bitmap);
  FX.Trace(BitMap, 1);
  picture.Bitmap.assign(bitmap);
  BitMap.free;
  Update;
end;

end.
