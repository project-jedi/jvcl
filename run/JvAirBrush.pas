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
  {$IFDEF VCL}
  Windows, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, JvJVCLUtils, Types,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  TAirBrushShape = (absRound, absSquare, absLeftSlash, absRightSlash,
    absHorizontal, absVertical, absSpray);

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
    procedure Blend(Src1, Src2, Dst: TBitmap; Amount: Extended);
    procedure SetShape(const Value: TAirBrushShape);
    function GetAir: Boolean;
    procedure SetInterval(const Value: Integer);
    procedure MakeSpray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);
    property Air: Boolean read GetAir;
  published
    property Size: Integer read FSize write SetSize;
    property Color: TColor read FColor write SetColor;
    property Intensity: Integer read FIntensity write SetIntensity;
    property Shape: TAirBrushShape read FShape write SetShape;
    // (rom) Interval seems nonfunctional. Delete or reactivate for spray?
    property Interval: Integer read FInterval write SetInterval;
  end;

implementation

//=== TJvAirBrush ============================================================

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

destructor TJvAirBrush.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TJvAirBrush.SetColor(const Value: TColor);
begin
  FColor := Value;
  MakeBrush;
end;

procedure TJvAirBrush.SetIntensity(const Value: Integer);
begin
  if Value <> FIntensity then
    if (Value >= 1) and (Value <= 100) then
      FIntensity := Value;
end;

procedure TJvAirBrush.SetSize(const Value: Integer);
begin
  if Value <> FSize then
    if (Value >= 10) and (Value <= 200) then
    begin
      FSize := Value;
      MakeBrush;
    end;
end;

procedure TJvAirBrush.MakeBrush;
var
  Pts: array [0..3] of TPoint;
begin
  with Bitmap do
  begin
    Width := FSize;
    Height := FSize;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Color := FColor;
    case FShape of
      absRound:
        Canvas.Ellipse(0, 0, Width, Height);
      absSquare:
       Canvas.Rectangle(0, 0, Width, Height);
      absRightSlash:
        begin
          Pts[0] := Point(0, Height - 1);
          Pts[1] := Point(Width div 4, Height - 1);
          Pts[2] := Point(Width - 1, 0);
          Pts[3] := Point(Width - 1 - (Width div 4), 0);
          Canvas.Polygon(Pts);
        end;
      absLeftSlash:
        begin
          Pts[0] := Point(0, 0);
          Pts[1] := Point(Width div 4, 0);
          Pts[2] := Point(Width - 1, Height - 1);
          Pts[3] := Point(Width - 1 - (Width div 4), Height - 1);
          Canvas.Polygon(Pts);
        end;
      absHorizontal:
        Canvas.Rectangle(0, Height div 4, Width - 1, Height - 1 - (Height div 4));
      absVertical:
        Canvas.Rectangle(Width div 4, 0, Width - 1 - (Width div 4), Height - 1);
      absSpray:
        MakeSpray;
    end;
    TransparentColor := clWhite;
    Transparent := True;
  end;
end;

// (rom) better make Bitmap pf24bit here and use Scanline to speed this up

procedure TJvAirBrush.MakeSpray;
var
  X, Y, X2, Y2: Integer;
begin
  X2 := Bitmap.Width div 2;
  Y2 := Bitmap.Height div 2;
  with Bitmap.Canvas do
    for Y := 0 to Bitmap.Height - 1 do
      for X := 0 to Bitmap.Width - 1 do
        if (Sqr(X - X2) + Sqr(Y - Y2)) < Sqr(X2) then
          if ((X mod 3) = 0) and ((Y mod 3) = 0) then
            Pixels[X, Y] := FColor;
end;

procedure TJvAirBrush.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  Bmp, Dst: TBitmap;
  RPaint, Rt: TRect;
  CLeft, CTop: Integer;
begin
  //  MakeBrush;
  CLeft := X - (FSize div 2);
  CTop := Y - (FSize div 2);
  RPaint := Rect(CLeft, CTop, CLeft + FSize, CTop + FSize);
  Bmp := TBitmap.Create;
  Bmp.Width := Bitmap.Width;
  Bmp.Height := Bitmap.Height;
  Dst := TBitmap.Create;
  Dst.Width := Bitmap.Width;
  Dst.Height := Bitmap.Height;
  try
    Rt := Rect(0, 0, Bmp.Width, Bmp.Height);
    Bmp.Canvas.CopyRect(Rt, ACanvas, RPaint);
    Bmp.PixelFormat := pf24bit;
    Bitmap.PixelFormat := pf24bit;
    Dst.PixelFormat := pf24bit;
    Blend(Bmp, Bitmap, Dst, FIntensity / 100);
    Dst.TransparentColor := clWhite;
    Dst.Transparent := True;
    ACanvas.Draw(CLeft, CTop, Dst);
  finally
    Bmp.Free;
    Dst.Free;
  end;
end;

procedure TJvAirBrush.Blend(Src1, Src2, Dst: TBitmap; Amount: Extended);
var
  W, H, X, Y: Integer;
  Ps1, Ps2, Pd: PByteArray;
begin
  W := Src1.Width;
  H := Src1.Height;
  Dst.Width := W;
  Dst.Height := H;
  Src1.PixelFormat := pf24bit;
  Src2.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  for Y := 0 to H - 1 do
  begin
    Ps1 := Src1.ScanLine[Y];
    Ps2 := Src2.ScanLine[Y];
    Pd := Dst.ScanLine[Y];
    for X := 0 to W - 1 do
      if ((Ps2[X * 3] = $FF) and (Ps2[X * 3 + 1] = $FF) and (Ps2[X * 3 + 2] = $FF)) then
      begin
        Pd[X * 3] := $FF;
        Pd[X * 3 + 2] := $FF;
        Pd[X * 3 + 2] := $FF;
      end
      else
      begin
        Pd[X * 3] := Round((1 - Amount) * Ps1[X * 3] + Amount * Ps2[X * 3]);
        Pd[X * 3 + 1] := Round((1 - Amount) * Ps1[X * 3 + 1] + Amount * Ps2[X * 3 + 1]);
        Pd[X * 3 + 2] := Round((1 - Amount) * Ps1[X * 3 + 2] + Amount * Ps2[X * 3 + 2]);
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

function TJvAirBrush.GetAir: Boolean;
begin
  if Integer(GetTickCount - FCounter) > FInterval then
  begin
    Result := True;
    FCounter := GetTickCount;
  end
  else
    Result := False;
end;

end.
