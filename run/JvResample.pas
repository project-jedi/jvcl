{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvResample.PAS released on 1998-03-15

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Anders Melander are Copyright (C) 1998 Anders Melander.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

// -----------------------------------------------------------------------------
// Project: bitmap resampler
// Module: resample
// Description: Interpolated Bitmap Resampling using filters.
// Version: 01.02
// Release: 3
// Date: 15-MAR-1998
// Target: Win32, Delphi 2 & 3
// Author(s): anme: Anders Melander, anders att melander dott dk
// Copyright (c) 1997,98 by Anders Melander
// Formatting: 2 space indent, no tabs, 80 columns.
// -----------------------------------------------------------------------------
// This software is copyrighted as noted above.  It may be freely copied,
// modified, and redistributed, provided that the copyright notice(s) is
// preserved on all copies.
//
// There is no warranty or other guarantee of fitness for this software,
// it is provided solely "as is".  Bug reports or fixes may be sent
// to the author, who may or may not act on them as he desires.
//
// You may not include this software in a program or other software product
// without supplying the source, or without informing the end-user that the
// source is available for no extra charge.
//
// If you modify this software, you should include a notice in the "Revision
// history" section giving the name of the person performing the modification,
// the date of modification, and the reason for such modification.
// -----------------------------------------------------------------------------
// Here's some additional copyrights for you:
//
// From filter.c:
// The authors and the publisher hold no copyright restrictions
// on any of these files; this source code is public domain, and
// is freely available to the entire computer graphics community
// for study, use, and modification.  We do request that the
// comment at the top of each file, identifying the original
// author and its original publication in the book Graphics
// Gems, be retained in all programs that use these files.
//
// -----------------------------------------------------------------------------
// Revision history:
//
// 0100 110997  anme - Adapted from fzoom v0.20 by Dale Schumacher.
//
// 0101 110198 anme  - Added Lanczos3 and Mitchell filters.
//      - Fixed range bug.
//        Min value was not checked on conversion from Single to
//        byte.
//      - Numerous optimizations.
//      - Added TImage stretch on form resize.
//      - Added support for Delphi 2 via TCanvas.Pixels.
//      - Renamed module from stretch to resample.
//      - Moved demo code to separate module.
//
// 0102 150398 anme - Fixed a problem that caused all pixels to be shifted
//        1/2 pixel down and to the right (in source
//        coordinates). Thanks to David Ullrich for the
//        solution.
// -----------------------------------------------------------------------------
// Credits:
// The algorithms and methods used in this library are based on the article
// "General Filtered Image Rescaling" by Dale Schumacher which appeared in the
// book Graphics Gems III, published by Academic Press, Inc.
//
// The edge offset problem was fixed by:
//   * David Ullrich <ullrich att hardy dott math dott okstate dott edu>
// -----------------------------------------------------------------------------
// To do (in rough order of priority):
// * Implement Dale Schumacher's "Optimized Bitmap Scaling Routines".
// * Fix BoxFilter.
// * Optimize to use integer math instead of floating point where possible.
// -----------------------------------------------------------------------------

unit JvResample;

{$I jvcl.inc}

interface

uses
  {$IFDEF VCL}
  Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, QWindows,
  {$ENDIF VisualCLX}
  SysUtils, Classes;

type
  // Type of a filter for use with Stretch()
  TFilterProc = function(Value: Single): Single;

// Sample filters for use with Stretch()
function SplineFilter(Value: Single): Single;
function BellFilter(Value: Single): Single;
function TriangleFilter(Value: Single): Single;
function BoxFilter(Value: Single): Single;
function HermiteFilter(Value: Single): Single;
function Lanczos3Filter(Value: Single): Single;
function MitchellFilter(Value: Single): Single;

// Interpolator
// Src: Source bitmap
// Dst: Destination bitmap
// filter: weight calculation filter
// AWidth: Relative sample radius
procedure ImgStretch(Src, Dst: TBitmap; Filter: TFilterProc; AWidth: Single);

//----------------------------------------------------------------------------
// List of Filters
//----------------------------------------------------------------------------

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
  JvTypes, JvResources;

//----------------------------------------------------------------------------
// Filter functions
//----------------------------------------------------------------------------

// Hermite filter

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

// Bell filter

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

// B-spline filter

function SplineFilter(Value: Single): Single;
var
  TT: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    TT := Sqr(Value);
    Result := 0.5 * TT * Value - TT + 2.0 / 3.0;
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

// Lanczos3 filter

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
  TT: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  TT := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * TT)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * TT) +
      (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * TT)) +
      ((6.0 * B + 30.0 * C) * TT) +
      ((-12.0 * B - 48.0 * C) * Value) +
      (8.0 * B + 24 * C));
    Result := Value / 6.0;
  end
  else
    Result := 0.0;
end;

//----------------------------------------------------------------------------
// Interpolator
//----------------------------------------------------------------------------

type
  // Contributor for a pixel
  TContributor = record
    Pixel: Integer; // Source pixel
    Weight: Single; // Pixel Weight
  end;

  TContributorList = array [0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    N: Integer;
    P: PContributorList;
  end;

  TCListList = array [0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    R: Single;
    G: Single;
    B: Single;
  end;

  // Physical bitmap pixel
  TColorRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;
  PColorRGB = ^TColorRGB;

  // Physical bitmap scanline (row)
  TRGBList = packed array [0..0] of TColorRGB;
  PRGBList = ^TRGBList;

procedure ImgStretch(Src, Dst: TBitmap; Filter: TFilterProc; AWidth: Single);
var
  XScale, YScale: Single; // Zoom scale factors
  I, J, K: Integer; // Loop variables
  Center: Single; // Filter calculation variables
  Width, FScale, Weight: Single; // Filter calculation variables
  Left, Right: Integer; // Filter calculation variables
  N: Integer; // Pixel number
  Work: TBitmap;
  Contrib: PCListList;
  Rgb: TRGB;
  Color: TColorRGB;
  SourceLine, DestLine: PRGBList;
  SourcePixel, DestPixel: PColorRGB;
  Delta, DestDelta: Integer;
  SrcWidth, SrcHeight, DstWidth, DstHeight: Integer;

  function Color2RGB(Color: TColor): TColorRGB;
  begin
    Result.R := Color and $000000FF;
    Result.G := (Color and $0000FF00) shr 8;
    Result.B := (Color and $00FF0000) shr 16;
  end;

  function RGB2Color(Color: TColorRGB): TColor;
  begin
    Result := Color.R or (Color.G shl 8) or (Color.B shl 16);
  end;

begin
  DstWidth := Dst.Width;
  DstHeight := Dst.Height;
  SrcWidth := Src.Width;
  SrcHeight := Src.Height;
  if (SrcWidth < 1) or (SrcHeight < 1) then
    raise EJVCLException.CreateRes(@RsESourceBitmapTooSmall);

  // Create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.Height := SrcHeight;
    Work.Width := DstWidth;
    // XScale := DstWidth / SrcWidth;
    // YScale := DstHeight / SrcHeight;
    // Improvement suggested by David Ullrich:
    if SrcWidth = 1 then
      XScale := DstWidth / SrcWidth
    else
      XScale := (DstWidth - 1) / (SrcWidth - 1);
    if SrcHeight = 1 then
      YScale := DstHeight / SrcHeight
    else
      YScale := (DstHeight - 1) / (SrcHeight - 1);
    // This implementation only works on 24-bit images because it uses
    // TBitmap.Scanline
    Src.PixelFormat := pf24bit;
    Dst.PixelFormat := Src.PixelFormat;
    Work.PixelFormat := Src.PixelFormat;

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(Contrib, DstWidth * SizeOf(TCList));
    // Horizontal sub-sampling
    // Scales from bigger to smaller Width
    if XScale < 1.0 then
    begin
      Width := AWidth / XScale;
      FScale := 1.0 / XScale;
      for I := 0 to DstWidth - 1 do
      begin
        Contrib^[I].N := 0;
        GetMem(Contrib^[I].P, Trunc(Width * 2.0 + 1) * SizeOf(TContributor));
        Center := I / XScale;
        // Original code:
        // Left := Ceil(Center - Width);
        // Right := Floor(Center + Width);
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Filter((Center - J) / FScale) / FScale;
          if Weight = 0.0 then
            Continue;
          if J < 0 then
            N := -J
          else
          if J >= SrcWidth then
            N := SrcWidth - J + SrcWidth - 1
          else
            N := J;
          K := Contrib^[I].N;
          Contrib^[I].N := Contrib^[I].N + 1;
          Contrib^[I].P^[K].Pixel := N;
          Contrib^[I].P^[K].Weight := Weight;
        end;
      end;
    end
    else
      // Horizontal super-sampling
      // Scales from smaller to bigger Width
    begin
      for I := 0 to DstWidth - 1 do
      begin
        Contrib^[I].N := 0;
        GetMem(Contrib^[I].P, Trunc(AWidth * 2.0 + 1) * SizeOf(TContributor));
        Center := I / XScale;
        // Original code:
        // Left := Ceil(Center - AWidth);
        // Right := Floor(Center + AWidth);
        Left := Floor(Center - AWidth);
        Right := Ceil(Center + AWidth);
        for J := Left to Right do
        begin
          Weight := Filter(Center - J);
          if Weight = 0.0 then
            Continue;
          if J < 0 then
            N := -J
          else
          if J >= SrcWidth then
            N := SrcWidth - J + SrcWidth - 1
          else
            N := J;
          K := Contrib^[I].N;
          Contrib^[I].N := Contrib^[I].N + 1;
          Contrib^[I].P^[K].Pixel := N;
          Contrib^[I].P^[K].Weight := Weight;
        end;
      end;
    end;

    // ----------------------------------------------------
    // Apply filter to sample horizontally from Src to Work
    // ----------------------------------------------------
    for K := 0 to SrcHeight - 1 do
    begin
      SourceLine := Src.ScanLine[K];
      DestPixel := Work.ScanLine[K];
      for I := 0 to DstWidth - 1 do
      begin
        Rgb.R := 0.0;
        Rgb.G := 0.0;
        Rgb.B := 0.0;
        for J := 0 to Contrib^[I].N - 1 do
        begin
          Color := SourceLine^[Contrib^[I].P^[J].Pixel];
          Weight := Contrib^[I].P^[J].Weight;
          if Weight = 0.0 then
            Continue;
          Rgb.R := Rgb.R + Color.R * Weight;
          Rgb.G := Rgb.G + Color.G * Weight;
          Rgb.B := Rgb.B + Color.B * Weight;
        end;
        if Rgb.R > 255.0 then
          Color.R := 255
        else
        if Rgb.R < 0.0 then
          Color.R := 0
        else
          Color.R := Round(Rgb.R);
        if Rgb.G > 255.0 then
          Color.G := 255
        else
        if Rgb.G < 0.0 then
          Color.G := 0
        else
          Color.G := Round(Rgb.G);
        if Rgb.B > 255.0 then
          Color.B := 255
        else
        if Rgb.B < 0.0 then
          Color.B := 0
        else
          Color.B := Round(Rgb.B);
        // Set new Pixel value
        DestPixel^ := Color;
        // Move on to next column
        Inc(DestPixel);
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for I := 0 to DstWidth - 1 do
      FreeMem(Contrib^[I].P);

    FreeMem(Contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(Contrib, DstHeight * SizeOf(TCList));
    // Vertical sub-sampling
    // Scales from bigger to smaller height
    if YScale < 1.0 then
    begin
      Width := AWidth / YScale;
      FScale := 1.0 / YScale;
      for I := 0 to DstHeight - 1 do
      begin
        Contrib^[I].N := 0;
        GetMem(Contrib^[I].P, Trunc(Width * 2.0 + 1) * SizeOf(TContributor));
        Center := I / YScale;
        // Original code:
        // Left := Ceil(Center - Width);
        // Right := Floor(Center + Width);
        Left := Floor(Center - Width);
        Right := Ceil(Center + Width);
        for J := Left to Right do
        begin
          Weight := Filter((Center - J) / FScale) / FScale;
          if Weight = 0.0 then
            Continue;
          if J < 0 then
            N := -J
          else
          if J >= SrcHeight then
            N := SrcHeight - J + SrcHeight - 1
          else
            N := J;
          K := Contrib^[I].N;
          Contrib^[I].N := Contrib^[I].N + 1;
          Contrib^[I].P^[K].Pixel := N;
          Contrib^[I].P^[K].Weight := Weight;
        end;
      end
    end
    else
      // Vertical super-sampling
      // Scales from smaller to bigger height
    begin
      for I := 0 to DstHeight - 1 do
      begin
        Contrib^[I].N := 0;
        GetMem(Contrib^[I].P, Trunc(AWidth * 2.0 + 1) * SizeOf(TContributor));
        Center := I / YScale;
        // Original code:
        // Left := Ceil(Center - AWidth);
        // Right := Floor(Center + AWidth);
        Left := Floor(Center - AWidth);
        Right := Ceil(Center + AWidth);
        for J := Left to Right do
        begin
          Weight := Filter(Center - J);
          if Weight = 0.0 then
            Continue;
          if J < 0 then
            N := -J
          else
          if J >= SrcHeight then
            N := SrcHeight - J + SrcHeight - 1
          else
            N := J;
          K := Contrib^[I].N;
          Contrib^[I].N := Contrib^[I].N + 1;
          Contrib^[I].P^[K].Pixel := N;
          Contrib^[I].P^[K].Weight := Weight;
        end;
      end;
    end;

    // --------------------------------------------------
    // Apply filter to sample vertically from Work to Dst
    // --------------------------------------------------
    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Dst.ScanLine[0];
    DestDelta := Integer(Dst.ScanLine[1]) - Integer(DestLine);
    for K := 0 to DstWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      for I := 0 to DstHeight - 1 do
      begin
        Rgb.R := 0;
        Rgb.G := 0;
        Rgb.B := 0;
        // Weight := 0.0;
        for J := 0 to Contrib^[I].N - 1 do
        begin
          Color := PColorRGB(Integer(SourceLine) + Contrib^[I].P^[J].Pixel * Delta)^;
          Weight := Contrib^[I].P^[J].Weight;
          if Weight = 0.0 then
            Continue;
          Rgb.R := Rgb.R + Color.R * Weight;
          Rgb.G := Rgb.G + Color.G * Weight;
          Rgb.B := Rgb.B + Color.B * Weight;
        end;
        if Rgb.R > 255.0 then
          Color.R := 255
        else
        if Rgb.R < 0.0 then
          Color.R := 0
        else
          Color.R := Round(Rgb.R);
        if Rgb.G > 255.0 then
          Color.G := 255
        else
        if Rgb.G < 0.0 then
          Color.G := 0
        else
          Color.G := Round(Rgb.G);
        if Rgb.B > 255.0 then
          Color.B := 255
        else
        if Rgb.B < 0.0 then
          Color.B := 0
        else
          Color.B := Round(Rgb.B);
        DestPixel^ := Color;
        Inc(Integer(DestPixel), DestDelta);
      end;
      Inc(SourceLine, 1);
      Inc(DestLine, 1);
    end;

    // Free the memory allocated for vertical filter weights
    for I := 0 to DstHeight - 1 do
      FreeMem(Contrib^[I].P);

    FreeMem(Contrib);
  finally
    Work.Free;
  end;
end;

end.
