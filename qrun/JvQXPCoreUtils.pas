{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXPCoreUtils.PAS, released on 2004-01-01.

The Initial Developer of the Original Code is Marc Hoffman.
Portions created by Marc Hoffman are Copyright (C) 2002 APRIORI business solutions AG.
Portions created by APRIORI business solutions AG are Copyright (C) 2002 APRIORI business solutions AG
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQXPCoreUtils;

interface

uses
  SysUtils, Classes, TypInfo,  
  Types, QGraphics, QControls, QTypes, QWindows, 
  JvQXPCore;

function JvXPMethodsEqual(const Method1, Method2: TMethod): Boolean;
procedure JvXPDrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer);
procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const Colors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
procedure JvXPAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TJvXPBoundLines; var Rect: TRect);
procedure JvXPDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TJvXPBoundLines;
  const AColor: TColor; const Rect: TRect);

//
// attic!
//

procedure JvXPConvertToGray2(Bitmap: TBitmap);
procedure JvXPRenderText(const AParent: TControl; const ACanvas: TCanvas;
  AText: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var Rect: TRect; Flags: Integer);
procedure JvXPFrame3D(const ACanvas: TCanvas; const Rect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);
procedure JvXPColorizeBitmap(Bitmap: TBitmap; const AColor: TColor);
procedure JvXPSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
procedure JvXPPlaceText(const AParent: TControl; const ACanvas: TCanvas;
  const AText: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  const AAlignment: TAlignment; const AWordWrap: Boolean; var Rect: TRect);

implementation

function JvXPMethodsEqual(const Method1, Method2: TMethod): Boolean;
begin
  Result := (Method1.Code = Method2.Code) and (Method1.Data = Method2.Data);
end;

procedure JvXPCreateGradientRect(const AWidth, AHeight: Integer; const StartColor,
  EndColor: TColor; const Colors: TJvXPGradientColors; const Style: TJvXPGradientStyle;
  const Dithered: Boolean; var Bitmap: TBitmap);
const
  PixelCountMax = 32768;
  DitherDepth = 16;
type
  TGradientBand = array [0..255] of TColor;
  TRGBMap = packed record
    case Boolean of
      True:
        (RGBVal: DWord);
      False:
        (R, G, B, D: Byte);
  end;
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [0..PixelCountMax-1] of TRGBTriple;
var
  iLoop, xLoop, yLoop, XX, YY: Integer;
  iBndS, iBndE: Integer;
  GBand: TGradientBand;
  Row: PRGBTripleArray;

  procedure CalculateGradientBand;
  var
    rR, rG, rB: Real;
    lCol, hCol: TRGBMap;
    iStp: Integer;
  begin
    if Style in [gsLeft, gsTop] then
    begin
      lCol.RGBVal := ColorToRGB(StartColor);
      hCol.RGBVal := ColorToRGB(EndColor);
    end
    else
    begin
      lCol.RGBVal := ColorToRGB(EndColor);
      hCol.RGBVal := ColorToRGB(StartColor);
    end;
    rR := (hCol.R - lCol.R) / (Colors - 1);
    rG := (hCol.G - lCol.G) / (Colors - 1);
    rB := (hCol.B - lCol.B) / (Colors - 1);
    for iStp := 0 to (Colors - 1) do
      GBand[iStp] := RGB(
        lCol.R + Round(rR * iStp),
        lCol.G + Round(rG * iStp),
        lCol.B + Round(rB * iStp));
  end;

begin
  Bitmap.Height := AHeight;
  Bitmap.Width := AWidth;

  Bitmap.PixelFormat := pf24bit;

  CalculateGradientBand;

  with Bitmap.Canvas do
  begin
    {$IFDEF LINUX}
    Start;
    {$ENDIF LINUX}
    Brush.Color := StartColor;
    FillRect(Bounds(0, 0, AWidth, AHeight));
    if Style in [gsLeft, gsRight] then
    begin
      for iLoop := 0 to Colors - 1 do
      begin
        iBndS := MulDiv(iLoop, AWidth, Colors);
        iBndE := MulDiv(iLoop + 1, AWidth, Colors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, iBndS, 0, iBndE, AHeight, PATCOPY);
        if (iLoop > 0) and Dithered then
          for yLoop := 0 to DitherDepth - 1 do
            if yLoop < AHeight  then
            begin
              Row := Bitmap.ScanLine[yLoop];
              for xLoop := 0 to AWidth div (Colors - 1) do
                begin
                  XX := iBndS + Random(xLoop);
                  if (XX < AWidth) and (XX > -1) then
                    with Row[XX] do
                    begin
                      rgbRed := GetRValue(GBand[iLoop - 1]);
                      rgbGreen := GetGValue(GBand[iLoop - 1]);
                      rgbBlue := GetBValue(GBand[iLoop - 1]);
                      rgbReserved := 0;
                    end;
                end;
            end;
      end;
      for yLoop := 1 to AHeight div DitherDepth do
        CopyRect(Bounds(0, yLoop * DitherDepth, AWidth, DitherDepth),
          Bitmap.Canvas, Bounds(0, 0, AWidth, DitherDepth));
    end
    else
    begin
      for iLoop := 0 to Colors - 1 do
      begin
        iBndS := MulDiv(iLoop, AHeight, Colors);
        iBndE := MulDiv(iLoop + 1, AHeight, Colors);
        Brush.Color := GBand[iLoop];
        PatBlt(Handle, 0, iBndS, AWidth, iBndE, PATCOPY);
        if (iLoop > 0) and Dithered then
          for yLoop := 0 to AHeight div (Colors - 1) do
          begin
            YY := iBndS + Random(yLoop);
            if (YY < AHeight) and (YY > -1) then
            begin
              Row := Bitmap.ScanLine[YY];
              for xLoop := 0 to DitherDepth - 1 do
              if xLoop < AWidth  then
                with Row[xLoop] do
                begin
                  rgbRed := GetRValue(GBand[iLoop - 1]);
                  rgbGreen := GetGValue(GBand[iLoop - 1]);
                  rgbBlue := GetBValue(GBand[iLoop - 1]);
                  rgbReserved := 0;
                end;
              end;
          end;
      end;
      for xLoop := 0 to AWidth div DitherDepth do
        CopyRect(Bounds(xLoop * DitherDepth, 0, DitherDepth, AHeight),
          Bitmap.Canvas, Bounds(0, 0, DitherDepth, AHeight));
    end;
    {$IFDEF LINUX}
    Stop;
    {$ENDIF LINUX}
  end;
end;

procedure JvXPDrawLine(const ACanvas: TCanvas; const X1, Y1, X2, Y2: Integer);
begin
  with ACanvas do
  begin
    MoveTo(X1, Y1);
    LineTo(X2, Y2);
  end;
end;

procedure JvXPAdjustBoundRect(const BorderWidth: Byte;
  const ShowBoundLines: Boolean; const BoundLines: TJvXPBoundLines;
  var Rect: TRect);
begin
  InflateRect(Rect, -BorderWidth, -BorderWidth);
  if not ShowBoundLines then
    Exit;
  if blLeft in BoundLines then
    Inc(Rect.Left);
  if blRight in BoundLines then
    Dec(Rect.Right);
  if blTop in BoundLines then
    Inc(Rect.Top);
  if blBottom in BoundLines then
    Dec(Rect.Bottom);
end;

procedure JvXPDrawBoundLines(const ACanvas: TCanvas; const BoundLines: TJvXPBoundLines;
  const AColor: TColor; const Rect: TRect);
begin
  with ACanvas do
  begin
    Pen.Color := AColor;
    Pen.Style := psSolid;
    if blLeft in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Left, Rect.Bottom - 1);
    if blTop in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Left, Rect.Top, Rect.Right, Rect.Top);
    if blRight in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Right - 1, Rect.Top, Rect.Right - 1, Rect.Bottom - 1);
    if blBottom in BoundLines then
      JvXPDrawLine(ACanvas, Rect.Top, Rect.Bottom - 1, Rect.Right, Rect.Bottom - 1);
  end;
end;

//
// attic
//

procedure JvXPConvertToGray2(Bitmap: TBitmap);
var
  x, y, c: Integer;
  PxlColor: TColor;
begin
  for x := 0 to Bitmap.Width - 1 do
    for y := 0 to Bitmap.Height - 1 do
    begin
      PxlColor := ColorToRGB(Bitmap.Canvas.Pixels[x, y]);
      c := (PxlColor shr 16 + ((PxlColor shr 8) and $00FF) + PxlColor and $0000FF) div 3 + 100;
      if c > 255 then
        c := 255;
      Bitmap.Canvas.Pixels[x, y] := RGB(c, c, c);
    end;
end;

procedure JvXPRenderText(const AParent: TControl; const ACanvas: TCanvas;
  AText: TCaption; const AFont: TFont; const AEnabled, AShowAccelChar: Boolean;
  var Rect: TRect; Flags: Integer); overload;

  procedure DoDrawText;
  begin  
    SetPenColor(ACanvas.Handle, ACanvas.Font.Color);
    DrawText(ACanvas.Handle, WideString(AText), -1, Rect, Flags); 
  end;

begin
  if (Flags and DT_CALCRECT <> 0) and ((AText = '') or AShowAccelChar and
    (AText[1] = '&') and (AText[2] = #0)) then
    AText := AText + ' ';
  if not AShowAccelChar then
    Flags := Flags or DT_NOPREFIX; 
  with ACanvas do
  begin
    Font.Assign(AFont);
    if not AEnabled then
      Font.Color := dxColor_Msc_Dis_Caption_WXP;
    if not AEnabled then
    begin
      OffsetRect(Rect, 1, 1);
      Font.Color := clBtnHighlight;
      DoDrawText;
      OffsetRect(Rect, -1, -1);
      Font.Color := clBtnShadow;
      DoDrawText;
    end
    else
      DoDrawText;
  end;
end;

procedure JvXPFrame3D(const ACanvas: TCanvas; const Rect: TRect;
  const TopColor, BottomColor: TColor; const Swapped: Boolean = False);
var
  ATopColor, ABottomColor: TColor;
begin
  ATopColor := TopColor;
  ABottomColor := BottomColor;
  if Swapped then
  begin
    ATopColor := BottomColor;
    ABottomColor := TopColor;
  end;
  with ACanvas do
  begin
    Pen.Color := ATopColor;
    Polyline([Point(Rect.Left, Rect.Bottom - 1),
      Point(Rect.Left, Rect.Top), Point(Rect.Right - 1, Rect.Top)]);
    Pen.Color := ABottomColor;
    Polyline([Point(Rect.Right - 1, Rect.Top + 1),
      Point(Rect.Right - 1 , Rect.Bottom - 1), Point(Rect.Left, Rect.Bottom - 1)]);
  end;
end;

procedure JvXPColorizeBitmap(Bitmap: TBitmap; const AColor: TColor);
var
  ColorMap: TBitmap;
  Rect: TRect;
begin
  Rect := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
  ColorMap := TBitmap.Create;
  try
    ColorMap.Assign(Bitmap);
    Bitmap.Dormant; 
    with ColorMap.Canvas do
    begin
      Brush.Color := AColor;  
      FillRect(Rect);
      Bitmap.TransparentColor := clBlack;
      Bitmap.Transparent := True;
      Draw(0,0, Bitmap); 
    end; 
    Bitmap.FreeImage;
    Bitmap.Assign(ColorMap);
    Bitmap.TransparentMode := tmAuto;  
  finally
    ColorMap.Free;
  end;
end;

procedure JvXPSetDrawFlags(const AAlignment: TAlignment; const AWordWrap: Boolean;
  var Flags: Integer);
begin
  Flags := DT_END_ELLIPSIS;
  case AAlignment of
    taLeftJustify:
      Flags := Flags or DT_LEFT;
    taCenter:
      Flags := Flags or DT_CENTER;
    taRightJustify:
      Flags := Flags or DT_RIGHT;
  end;
  if not AWordWrap then
    Flags := Flags or DT_SINGLELINE
  else
    Flags := Flags or DT_WORDBREAK;
end;

procedure JvXPPlaceText(const AParent: TControl; const ACanvas: TCanvas; const AText: TCaption;
  const AFont: TFont; const AEnabled, AShowAccelChar: Boolean; const AAlignment: TAlignment;
  const AWordWrap: Boolean; var Rect: TRect);
var
  Flags, DX, OH, OW: Integer;
begin
  OH := Rect.Bottom - Rect.Top;
  OW := Rect.Right - Rect.Left;
  JvXPSetDrawFlags(AAlignment, AWordWrap, Flags);
  JvXPRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect,
    Flags or DT_CALCRECT);
  if AAlignment = taRightJustify then
    DX := OW - (Rect.Right + Rect.Left)
  else
  if AAlignment = taCenter then
    DX := (OW - Rect.Right) div 2
  else
    DX := 0;
  OffsetRect(Rect, DX, (OH - Rect.Bottom) div 2);
  JvXPRenderText(AParent, ACanvas, AText, AFont, AEnabled, AShowAccelChar, Rect, Flags);
end;

end.

