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

Last Modified: 2003-10-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I JVCL.INC}
unit JvAirBrush;

interface

uses
  {$IFDEF COMPLIB_VCL}
  Windows, Graphics,
  {$ENDIF}
  {$IFDEF COMPLIB_CLX}
  QGraphics, JvJVCLUtils, Types,
  {$ENDIF}
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TAirBrushShape = (absRound, absSquare, absLeftSlash, absRightSlash, absHorizontal, absVertical, absSpray);
  TJvAirBrush = class(TJvComponent)
  private
    Bitmap: TBitmap;
    FIntensity: Integer;
    FSize: Integer;
    FColor: TColor;
    FShape: TAirBrushShape;
    FInterval: Integer;
    FCounter: LongWord;
    procedure SetColor(const Value: TColor);
    procedure SetIntensity(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure MakeBrush;
    procedure Blend(src1, src2, dst: TBitmap; amount: Extended);
    procedure SetShape(const Value: TAirBrushShape);
    function GetAir: boolean;
    procedure SetInterval(const Value: Integer);
    procedure MakeSpray;

    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; x, y: Integer);
    property Air: boolean read GetAir;
  published
    { Published declarations }
    property Size: Integer read FSize write SetSize;
    property Color: TColor read FColor write SetColor;
    property Intensity: Integer read FIntensity write SetIntensity;
    property Shape: TAirBrushShape read FShape write SetShape;
    property Interval: Integer read FInterval write SetInterval;
  end;

implementation

{ TJvAirBrush }

constructor TJvAirBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := 40;
  FCounter := GetTickCount;
  FInterval := 100;
  FIntensity := 10;
  FColor := clBlack;
  Bitmap := TBitmap.Create;
  FShape := absRound;
end;

destructor TJvAirBrush.destroy;
begin
  Bitmap.Free;
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
  pts: array[0..3] of TPoint;
begin
  with Bitmap do
  begin
    Width := FSize;
    Height := FSize;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Pen.style := psClear;
    Canvas.Brush.Color := FColor;
    case FShape of
      absRound: Canvas.Ellipse(0, 0, Width, Height);
      absSquare: Canvas.Rectangle(0, 0, Width, Height);
      absRightSlash:
        begin
          pts[0] := Point(0, Height - 1);
          pts[1] := Point(Width div 4, Height - 1);
          pts[2] := Point(Width - 1, 0);
          pts[3] := Point(Width - 1 - (Width div 4), 0);
          Canvas.Polygon(pts);
        end;
      absLeftSlash:
        begin
          pts[0] := Point(0, 0);
          pts[1] := Point(Width div 4, 0);
          pts[2] := Point(Width - 1, Height - 1);
          pts[3] := Point(Width - 1 - (Width div 4), Height - 1);
          Canvas.Polygon(pts);
        end;
      absHorizontal: Canvas.Rectangle(0, Height div 4, Width - 1, Height - 1 - (Height div 4));
      absVertical: Canvas.Rectangle(Width div 4, 0, Width - 1 - (Width div 4), Height - 1);
      absSpray: MakeSpray;
    end;
    transparentColor := clWhite;
    transparent := True;
  end;
end;

procedure TJvAirBrush.MakeSpray;
var
  x, y, x2, y2: Integer;
begin
  x2 := Bitmap.Width div 2;
  y2 := Bitmap.Height div 2;
  with Bitmap.Canvas do
    for y := 0 to Bitmap.Height - 1 do
      for x := 0 to Bitmap.Width - 1 do
        if (sqr(x - x2) + sqr(y - y2)) < sqr(x2) then
          if ((x mod 3) = 0) and ((y mod 3) = 0) then
            pixels[x, y] := FColor;
end;

procedure TJvAirBrush.Draw(ACanvas: TCanvas; x, y: Integer);
var
  bm, dst: TBitmap;
  Rpaint, Rt: TRect;
  CLeft, Ctop: Integer;
begin
  //  MakeBrush;
  CLeft := x - (FSize div 2);
  CTop := y - (FSize div 2);
  Rpaint := Rect(CLeft, CTop, CLeft + FSize, CTop + FSize);
  bm := TBitmap.Create;
  bm.Width := Bitmap.Width;
  bm.Height := Bitmap.Height;
  dst := TBitmap.Create;
  dst.Width := Bitmap.Width;
  dst.Height := Bitmap.Height;
  try
    Rt := Rect(0, 0, bm.Width, bm.Height);
    bm.Canvas.CopyRect(Rt, ACanvas, Rpaint);
    bm.PixelFormat := pf24bit;
    Bitmap.PixelFormat := pf24bit;
    dst.PixelFormat := pf24bit;
    Blend(bm, Bitmap, dst, FIntensity / 100);
    dst.TransparentColor := clWhite;
    dst.transparent := True;
    ACanvas.draw(CLeft, CTop, dst);
  finally
    bm.Free;
    dst.Free;
  end;
end;

procedure TJvAirBrush.Blend(src1, src2, dst: TBitmap; amount: Extended);
var
  w, h, x, y: Integer;
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

procedure TJvAirBrush.SetInterval(const Value: Integer);
begin
  FInterval := Value;
end;

function TJvAirBrush.GetAir: boolean;
begin
  if Integer(GetTickCount - FCounter) > FInterval then
  begin
    Result := True;
    Fcounter := GetTickCount;
  end
  else
    Result := False;
end;

end.
