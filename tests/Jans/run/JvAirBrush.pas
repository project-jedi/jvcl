{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAirBrush.PAS, released on 2002-06-15.

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
unit JvAirBrush;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TAirBrushShape = (absRound, absSquare, absLeftSlash, absRightSlash, absHorizontal, absVertical, absSpray);
  TJvAirBrush = class(TComponent)
  private
    Bitmap: TBitmap;
    FIntensity: Integer;
    FSize: Integer;
    FColor: TColor;
    FShape: TAirBrushShape;
    FInterval: integer;
    FCounter: DWord;
    procedure SetColor(const Value: TColor);
    procedure SetIntensity(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure MakeBrush;
    procedure Blend(src1, src2, dst: tbitmap; amount: extended);
    procedure SetShape(const Value: TAirBrushShape);
    function GetAir: boolean;
    procedure SetInterval(const Value: integer);
    procedure MakeSpray;

    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; x, y: integer);
    property Air: boolean read GetAir;
  published
    { Published declarations }
    property Size: Integer read FSize write SetSize;
    property Color: TColor read FColor write SetColor;
    property Intensity: Integer read FIntensity write SetIntensity;
    property Shape: TAirBrushShape read FShape write SetShape;
    property Interval: integer read FInterval write SetInterval;
  end;

implementation

{ TJvAirBrush }

constructor TJvAirBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := 40;
  FCounter := gettickcount;
  ;
  FInterval := 100;
  FIntensity := 10;
  FColor := clBlack;
  Bitmap := TBitmap.create;
  FShape := absRound;
end;

destructor TJvAirBrush.destroy;
begin
  Bitmap.free;
  inherited;
end;

procedure TJvAirBrush.SetColor(const Value: TColor);
begin
  FColor := Value;
  MakeBrush;
end;

procedure TJvAirBrush.SetIntensity(const Value: Integer);
begin
  if value <> FIntensity then
    if ((value >= 1) and (value <= 100)) then
    begin
      FIntensity := Value;
    end;
end;

procedure TJvAirBrush.SetSize(const Value: Integer);
begin
  if value <> FSize then
    if ((value >= 10) and (value <= 200)) then
    begin
      FSize := Value;
      MakeBrush;
    end;
end;

procedure TJvAirBrush.MakeBrush;
var
  pts: array[0..3] of Tpoint;
begin
  with Bitmap do
  begin
    width := FSize;
    height := FSize;
    canvas.brush.color := clwhite;
    canvas.fillrect(rect(0, 0, width, height));
    canvas.pen.style := psclear;
    canvas.brush.color := FColor;
    case FShape of
      absRound: canvas.Ellipse(0, 0, width, height);
      absSquare: canvas.Rectangle(0, 0, width, height);
      absRightSlash:
        begin
          pts[0] := point(0, height - 1);
          pts[1] := point(width div 4, height - 1);
          pts[2] := point(width - 1, 0);
          pts[3] := point(width - 1 - (width div 4), 0);
          canvas.Polygon(pts);
        end;
      absLeftSlash:
        begin
          pts[0] := point(0, 0);
          pts[1] := point(width div 4, 0);
          pts[2] := point(width - 1, height - 1);
          pts[3] := point(width - 1 - (width div 4), height - 1);
          canvas.Polygon(pts);
        end;
      absHorizontal: canvas.rectangle(0, height div 4, width - 1, height - 1 - (height div 4));
      absVertical: canvas.rectangle(width div 4, 0, width - 1 - (width div 4), height - 1);
      absSpray: MakeSpray;
    end;
    transparentcolor := clwhite;
    transparent := true;
  end;
end;

procedure TJvAirBrush.MakeSpray;
var
  x, y, x2, y2: integer;
begin
  x2 := Bitmap.Width div 2;
  y2 := Bitmap.Height div 2;
  with Bitmap.canvas do
    for y := 0 to Bitmap.height - 1 do
      for x := 0 to Bitmap.width - 1 do
        if (sqr(x - x2) + sqr(y - y2)) < sqr(x2) then
          if ((x mod 3) = 0) and ((y mod 3) = 0) then
            pixels[x, y] := FColor;
end;

procedure TJvAirBrush.Draw(ACanvas: TCanvas; x, y: integer);
var
  bm, dst: TBitmap;
  Rpaint, Rt: Trect;
  CLeft, Ctop: integer;
begin
  //  MakeBrush;
  CLeft := x - (FSize div 2);
  CTop := y - (FSize div 2);
  Rpaint := rect(CLeft, CTop, CLeft + FSize, CTop + FSize);
  bm := Tbitmap.create;
  bm.width := Bitmap.width;
  bm.height := bitmap.height;
  dst := Tbitmap.create;
  dst.width := bitmap.width;
  dst.height := bitmap.height;
  try
    Rt := rect(0, 0, bm.width, bm.height);
    bm.canvas.CopyRect(Rt, ACanvas, Rpaint);
    bm.PixelFormat := pf24bit;
    bitmap.PixelFormat := pf24bit;
    dst.PixelFormat := pf24bit;
    Blend(bm, bitmap, dst, FIntensity / 100);
    dst.TransparentColor := clwhite;
    dst.transparent := true;
    ACanvas.draw(CLeft, CTop, dst);
  finally
    bm.free;
    dst.free;
  end;

end;

procedure TJvAirBrush.Blend(src1, src2, dst: tbitmap; amount: extended);
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
      if ((ps2[x * 3] = $FF) and (ps2[x * 3 + 1] = $FF) and (ps2[x * 3 + 2] = $FF)) then
      begin
        pd[x * 3] := $FF;
        pd[x * 3 + 2] := $FF;
        pd[x * 3 + 2] := $FF;
      end
      else
      begin
        pd[x * 3] := round((1 - amount) * ps1[x * 3] + amount * ps2[x * 3]);
        pd[x * 3 + 1] := round((1 - amount) * ps1[x * 3 + 1] + amount * ps2[x * 3 + 1]);
        pd[x * 3 + 2] := round((1 - amount) * ps1[x * 3 + 2] + amount * ps2[x * 3 + 2]);
      end;
    end;
  end;
end;

procedure TJvAirBrush.SetShape(const Value: TAirBrushShape);
begin
  FShape := Value;
  MakeBrush;
end;

procedure TJvAirBrush.SetInterval(const Value: integer);
begin
  FInterval := Value;
end;

function TJvAirBrush.GetAir: boolean;
begin
  if integer(gettickcount - FCounter) > FInterval then
  begin
    result := true;
    Fcounter := gettickcount;
  end
  else
    result := false;
end;

end.
