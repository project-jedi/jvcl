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

{$I jvcl.inc}

unit JvQResample;

// -----------------------------------------------------------------------------
// Project: bitmap resampler
// Module: resample
// Description: Interpolated Bitmap Resampling using filters.
// Version: 01.02
// Release: 3
// Date: 15-MAR-1998
// Target: Win32, Delphi 2 & 3
// Author(s): anme: Anders Melander, anders@melander.dk
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
//   * David Ullrich <ullrich@hardy.math.okstate.edu>
// -----------------------------------------------------------------------------
// To do (in rough order of priority):
// * Implement Dale Schumacher's "Optimized Bitmap Scaling Routines".
// * Fix BoxFilter.
// * Optimize to use integer math instead of floating point where possible.
// -----------------------------------------------------------------------------

interface

uses  
  QGraphics, QWindows, 
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
// filter: Weight calculation filter
// fwidth: Relative sample radius
procedure ImgStretch(Src, Dst: TBitmap; filter: TFilterProc; fwidth: single);

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
  JvQTypes, JvQResources;

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
  tt: single;
begin
  if Value < 0.0 then
    Value := -Value;
  if Value < 1.0 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2.0 / 3.0;
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
  tt: Single;
begin
  if Value < 0.0 then
    Value := -Value;
  tt := Sqr(Value);
  if Value < 1.0 then
  begin
    Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * tt)) +
      ((-18.0 + 12.0 * B + 6.0 * C) * tt) +
      (6.0 - 2 * B));
    Result := Value / 6.0;
  end
  else
  if Value < 2.0 then
  begin
    Value := (((-1.0 * B - 6.0 * C) * (Value * tt)) +
      ((6.0 * B + 30.0 * C) * tt) +
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
    pixel: Integer; // Source pixel
    weight: Single; // Pixel weight
  end;

  TContributorList = array [0..0] of TContributor;
  PContributorList = ^TContributorList;

  // List of source pixels contributing to a destination pixel
  TCList = record
    n: Integer;
    p: PContributorList;
  end;

  TCListList = array [0..0] of TCList;
  PCListList = ^TCListList;

  TRGB = packed record
    r, g, b: Single;
  end;

  // Physical bitmap pixel
  TColorRGB = packed record
    r, g, b: Byte;
  end;
  PColorRGB = ^TColorRGB;

  // Physical bitmap scanline (row)
  TRGBList = packed array [0..0] of TColorRGB;
  PRGBList = ^TRGBList;

procedure ImgStretch(Src, Dst: TBitmap; filter: TFilterProc; fwidth: Single);
var
  xscale, yscale: Single; // Zoom scale factors
  i, j, k: Integer; // Loop variables
  center: single; // Filter calculation variables
  width, fscale, weight: single; // Filter calculation variables
  left, right: Integer; // Filter calculation variables
  n: Integer; // Pixel number
  Work: TBitmap;
  contrib: PCListList;
  rgb: TRGB;
  color: TColorRGB;
  SourceLine,
    DestLine: PRGBList;
  SourcePixel,
    DestPixel: PColorRGB;
  Delta,
    DestDelta: Integer;
  SrcWidth,
    SrcHeight,
    DstWidth,
    DstHeight: Integer;

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
    raise EJVCLException.CreateRes(@RsESourceBitmapTooSmall);

  // Create intermediate image to hold horizontal zoom
  Work := TBitmap.Create;
  try
    Work.Height := SrcHeight;
    Work.Width := DstWidth;
    // xscale := DstWidth / SrcWidth;
    // yscale := DstHeight / SrcHeight;
    // Improvement suggested by David Ullrich:
    if SrcWidth = 1 then
      xscale := DstWidth / SrcWidth
    else
      xscale := (DstWidth - 1) / (SrcWidth - 1);
    if SrcHeight = 1 then
      yscale := DstHeight / SrcHeight
    else
      yscale := (DstHeight - 1) / (SrcHeight - 1);
    // This implementation only works on 24-bit images because it uses
    // TBitmap.Scanline
    Src.PixelFormat := pf24bit;
    Dst.PixelFormat := Src.PixelFormat;
    Work.PixelFormat := Src.PixelFormat;

    // --------------------------------------------
    // Pre-calculate filter contributions for a row
    // -----------------------------------------------
    GetMem(contrib, DstWidth * SizeOf(TCList));
    // Horizontal sub-sampling
    // Scales from bigger to smaller width
    if xscale < 1.0 then
    begin
      width := fwidth / xscale;
      fscale := 1.0 / xscale;
      for i := 0 to DstWidth - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * SizeOf(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if weight = 0.0 then
            continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcWidth then
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
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * SizeOf(TContributor));
        center := i / xscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if weight = 0.0 then
            continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcWidth then
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
      SourceLine := Src.ScanLine[k];
      DestPixel := Work.ScanLine[k];
      for i := 0 to DstWidth - 1 do
      begin
        rgb.r := 0.0;
        rgb.g := 0.0;
        rgb.b := 0.0;
        for j := 0 to contrib^[i].n - 1 do
        begin
          color := SourceLine^[contrib^[i].p^[j].pixel];
          weight := contrib^[i].p^[j].weight;
          if weight = 0.0 then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if rgb.r > 255.0 then
          color.r := 255
        else
        if rgb.r < 0.0 then
          color.r := 0
        else
          color.r := round(rgb.r);
        if rgb.g > 255.0 then
          color.g := 255
        else
        if rgb.g < 0.0 then
          color.g := 0
        else
          color.g := round(rgb.g);
        if rgb.b > 255.0 then
          color.b := 255
        else
        if rgb.b < 0.0 then
          color.b := 0
        else
          color.b := round(rgb.b);
        // Set new pixel value
        DestPixel^ := color;
        // Move on to next column
        Inc(DestPixel);
      end;
    end;

    // Free the memory allocated for horizontal filter weights
    for i := 0 to DstWidth - 1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);

    // -----------------------------------------------
    // Pre-calculate filter contributions for a column
    // -----------------------------------------------
    GetMem(contrib, DstHeight * SizeOf(TCList));
    // Vertical sub-sampling
    // Scales from bigger to smaller height
    if yscale < 1.0 then
    begin
      width := fwidth / yscale;
      fscale := 1.0 / yscale;
      for i := 0 to DstHeight - 1 do
      begin
        contrib^[i].n := 0;
        GetMem(contrib^[i].p, trunc(width * 2.0 + 1) * SizeOf(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - width);
        // right := floor(center + width);
        left := floor(center - width);
        right := ceil(center + width);
        for j := left to right do
        begin
          weight := filter((center - j) / fscale) / fscale;
          if weight = 0.0 then
            continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcHeight then
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
        GetMem(contrib^[i].p, trunc(fwidth * 2.0 + 1) * SizeOf(TContributor));
        center := i / yscale;
        // Original code:
        // left := ceil(center - fwidth);
        // right := floor(center + fwidth);
        left := floor(center - fwidth);
        right := ceil(center + fwidth);
        for j := left to right do
        begin
          weight := filter(center - j);
          if weight = 0.0 then
            continue;
          if j < 0 then
            n := -j
          else
          if j >= SrcHeight then
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
    SourceLine := Work.ScanLine[0];
    Delta := Integer(Work.ScanLine[1]) - Integer(SourceLine);
    DestLine := Dst.ScanLine[0];
    DestDelta := Integer(Dst.ScanLine[1]) - Integer(DestLine);
    for k := 0 to DstWidth - 1 do
    begin
      DestPixel := Pointer(DestLine);
      for i := 0 to DstHeight - 1 do
      begin
        rgb.r := 0;
        rgb.g := 0;
        rgb.b := 0;
        // weight := 0.0;
        for j := 0 to contrib^[i].n - 1 do
        begin
          color := PColorRGB(Integer(SourceLine) + contrib^[i].p^[j].pixel * Delta)^;
          weight := contrib^[i].p^[j].weight;
          if weight = 0.0 then
            continue;
          rgb.r := rgb.r + color.r * weight;
          rgb.g := rgb.g + color.g * weight;
          rgb.b := rgb.b + color.b * weight;
        end;
        if rgb.r > 255.0 then
          color.r := 255
        else
        if rgb.r < 0.0 then
          color.r := 0
        else
          color.r := round(rgb.r);
        if rgb.g > 255.0 then
          color.g := 255
        else
        if rgb.g < 0.0 then
          color.g := 0
        else
          color.g := round(rgb.g);
        if rgb.b > 255.0 then
          color.b := 255
        else
        if rgb.b < 0.0 then
          color.b := 0
        else
          color.b := round(rgb.b);
        DestPixel^ := color;
        Inc(Integer(DestPixel), DestDelta);
      end;
      Inc(SourceLine, 1);
      Inc(DestLine, 1);
    end;

    // Free the memory allocated for vertical filter weights
    for i := 0 to DstHeight - 1 do
      FreeMem(contrib^[i].p);

    FreeMem(contrib);
  finally
    Work.Free;
  end;
end;

end.
