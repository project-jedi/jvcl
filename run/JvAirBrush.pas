{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAirBrush.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAirBrush;

{$I jvcl.inc}

interface

uses
  Classes, Windows, Graphics,
  JvComponent;

type
  TJvAirBrushShape = (absRound, absSquare, absLeftSlash, absRightSlash,
    absHorizontal, absVertical, absSpray);

  TJvAirBrush = class(TJvComponent)
  private
    FBitmap: TBitmap;
    FIntensity: Integer;
    FSize: Integer;
    FColor: TColor;
    FShape: TJvAirBrushShape;
    FInterval: Integer;
    FCounter: Longword;
    procedure SetColor(const Value: TColor);
    procedure SetIntensity(const Value: Integer);
    procedure SetSize(const Value: Integer);
    procedure MakeBrush;
    procedure SetShape(const Value: TJvAirBrushShape);
    function GetAir: Boolean;
    procedure SetInterval(const Value: Integer);
    procedure MakeSpray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; X, Y: Integer);
    property Air: Boolean read GetAir;
  published
    property Size: Integer read FSize write SetSize default 40;
    property Color: TColor read FColor write SetColor default clBlack;
    property Intensity: Integer read FIntensity write SetIntensity default 10;
    property Shape: TJvAirBrushShape read FShape write SetShape default absRound;
    // (rom) Interval seems nonfunctional. Delete or reactivate for spray?
    property Interval: Integer read FInterval write SetInterval default 100;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvPaintFX;

constructor TJvAirBrush.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSize := 40;
  FCounter := GetTickCount;
  FInterval := 100;
  FIntensity := 10;
  FColor := clBlack;
  FBitmap := TBitmap.Create;
  FShape := absRound;
end;

destructor TJvAirBrush.Destroy;
begin
  FBitmap.Free;
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
  with FBitmap do
  begin
    Width := Size;
    Height := Size;
    Canvas.Brush.Color := clWhite;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Color := Color;
    case Shape of
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

// (rom) better make FBitmap pf24bit here and use Scanline to speed this up

procedure TJvAirBrush.MakeSpray;
var
  X, Y, X2, Y2: Integer;
begin
  X2 := FBitmap.Width div 2;
  Y2 := FBitmap.Height div 2;
  with FBitmap.Canvas do
    for Y := 0 to FBitmap.Height - 1 do
      for X := 0 to FBitmap.Width - 1 do
        if (Sqr(X - X2) + Sqr(Y - Y2)) < Sqr(X2) then
          if ((X mod 3) = 0) and ((Y mod 3) = 0) then
            Pixels[X, Y] := Color;
end;

procedure TJvAirBrush.Draw(ACanvas: TCanvas; X, Y: Integer);
var
  Bmp, Dst: TBitmap;
  RPaint, Rt: TRect;
  CLeft, CTop: Integer;
begin
  //  MakeBrush;
  CLeft := X - (Size div 2);
  CTop := Y - (Size div 2);
  RPaint := Rect(CLeft, CTop, CLeft + Size, CTop + Size);
  Bmp := TBitmap.Create;
  Bmp.Width := FBitmap.Width;
  Bmp.Height := FBitmap.Height;
  Dst := TBitmap.Create;
  Dst.Width := FBitmap.Width;
  Dst.Height := FBitmap.Height;
  try
    Rt := Rect(0, 0, Bmp.Width, Bmp.Height);
    Bmp.Canvas.CopyRect(Rt, ACanvas, RPaint);
    Bmp.PixelFormat := pf24bit;
    FBitmap.PixelFormat := pf24bit;
    Dst.PixelFormat := pf24bit;
    TJvPaintFX.Blend2(Bmp, FBitmap, Dst, Intensity / 100);
    Dst.TransparentColor := clWhite;
    Dst.Transparent := True;
    ACanvas.Draw(CLeft, CTop, Dst);
  finally
    Bmp.Free;
    Dst.Free;
  end;
end;

procedure TJvAirBrush.SetShape(const Value: TJvAirBrushShape);
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
  if Integer(GetTickCount - FCounter) > Interval then
  begin
    Result := True;
    FCounter := GetTickCount;
  end
  else
    Result := False;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

