{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUnicodeCanvas.PAS, released on 2003-09-21

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas.Hausladen@gmx.de>
Copyright (c) 2003 Andreas Hausladen
All Rights Reserved.

Contributor(s):

Last Modified: 2003-10-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}
unit JvUnicodeCanvas;
interface

uses
  SysUtils,
{$IFDEF VCL}
  Windows, Graphics,
{$ENDIF}
{$IFDEF VisualCLX}
  Qt, Types, QGraphics,
{$ENDIF}
  Classes, JvClxUtils, JvJCLUtils;

type
  TExtTextOutOptionsType = (etoClipped, etoOpaque);
  TExtTextOutOptions = Set of TExtTextOutOptionsType;

 { This Canvas has no new fields and can be type-casted form every TCanvas
   derived class. }
  TUnicodeCanvas = class(TCanvas)
  public
    function TextExtentW(const Text: WideString): TSize;
    function TextWidthW(const Text: WideString): Integer;
    function TextHeightW(const Text: WideString): Integer;
    procedure TextOutW(X, Y: Integer; const Text: WideString);
    procedure TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);
    function ExtTextOutW(X, Y: Integer; Options: TExtTextOutOptions; Rect: PRect;
      const Text: WideString; lpDx: Pointer): Boolean;

    function ExtTextOut(X, Y: Integer; Options: TExtTextOutOptions; Rect: PRect;
      const Text: String; lpDx: Pointer): Boolean; overload;
  {$IFDEF COMPILER_6UP}
    function ExtTextOut(X, Y: Integer; Options: TExtTextOutOptions;
      Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean; overload;
  {$ENDIF}

  {$IFDEF VCL}
   {$IFDEF COMPILER_6UP}
    function TextExtent(const Text: WideString): TSize; overload;
    function TextWidth(const Text: WideString): Integer; overload;
    function TextHeight(const Text: WideString): Integer; overload;
    procedure TextOut(X, Y: Integer; const Text: WideString); overload;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: WideString); overload;
   {$ENDIF}
  {$ENDIF}

  {$IFDEF VisualCLX}
    procedure TextOutVCL(X, Y: Integer; const Text: WideString);
    procedure TextRectVCL(Rect: TRect; X, Y: Integer;
      const Text: WideString; TextFlags: Integer = 0);
  {$ENDIF}
  end;

implementation

{$IFDEF VCL}
function ExtTextOutOptionsToInt(Options: TExtTextOutOptions): Integer;
begin
  Result := 0;
  if etoClipped in Options then Result := Result or ETO_CLIPPED;
  if etoOpaque in Options then Result := Result or ETO_OPAQUE;
end;

function TUnicodeCanvas.TextExtentW(const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  Windows.GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result);
end;

procedure TUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
var W: Integer;
begin
  Changing;
  W := TextWidth(Text);
  if CanvasOrientation = coRightToLeft then
    Inc(X, W + 1);
  Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
    Length(Text), nil);
  MoveTo(X + W, Y);
  Changed;
end;

procedure TUnicodeCanvas.TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);
var
  Options: Longint;
begin
  Changing;
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
    Length(Text), nil);
  Changed;
end;

function TUnicodeCanvas.TextWidthW(const Text: WideString): Integer;
begin
  Result := TextExtent(Text).cx;
end;

function TUnicodeCanvas.TextHeightW(const Text: WideString): Integer;
begin
  Result := TextExtent(Text).cy;
end;

// ------------

 {$IFDEF COMPILER_6UP}
function TUnicodeCanvas.TextExtent(const Text: WideString): TSize;
begin
  Result := TextExtentW(Text);
end;

function TUnicodeCanvas.TextHeight(const Text: WideString): Integer;
begin
  Result := TextHeightW(Text);
end;

procedure TUnicodeCanvas.TextOut(X, Y: Integer; const Text: WideString);
begin
  TextOutW(X, Y, Text);
end;

procedure TUnicodeCanvas.TextRect(Rect: TRect; X, Y: Integer;
  const Text: WideString);
begin
  TextRectW(Rect, X, Y, Text);
end;

function TUnicodeCanvas.TextWidth(const Text: WideString): Integer;
begin
  Result := TextWidthW(Text);
end;

function TUnicodeCanvas.ExtTextOut(X, Y: Integer; Options: TExtTextOutOptions; Rect: PRect;
  const Text: WideString; lpDx: Pointer): Boolean;
begin
  Result := ExtTextOutW(X, Y, Options, Rect, Text, lpDx);
end;
 {$ENDIF COMPILER_6UP}

{$ENDIF VCL}

{$IFDEF VisualCLX}

function TUnicodeCanvas.TextExtentW(const Text: WideString): TSize;
begin
  Result := TextExtent(Text);
end;

function TUnicodeCanvas.TextHeightW(const Text: WideString): Integer;
begin
  Result := TextHeight(Text);
end;

procedure TUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
begin
  TextOutVCL( X, Y, Text);
end;

procedure TUnicodeCanvas.TextRectW(Rect: TRect; X, Y: Integer;
  const Text: WideString);
begin
  TextRectVCL(Rect, X, Y, Text);
end;

function TUnicodeCanvas.TextWidthW(const Text: WideString): Integer;
begin
  Result := TextWidth(Text);
end;

procedure TUnicodeCanvas.TextOutVCL(X, Y: Integer; const Text: WideString);
var
  R: TRect;
begin
  if Brush.Style = bsSolid then
  begin
    R := Rect(0, 0, MaxLongint, MaxLongint);
    TextExtent(Text, R);
    OffsetRect(R, X, Y);
    FillRect(R);
  end;
  TextOut(X, Y, Text);
end;

procedure TUnicodeCanvas.TextRectVCL(Rect: TRect; X, Y: Integer;
  const Text: WideString; TextFlags: Integer = 0);
begin
  if Brush.Style = bsSolid then
    FillRect(Rect);
  TextRect(Rect, X, Y, Text, TextFlags);
end;

{$ENDIF VisualCLX}

function TUnicodeCanvas.ExtTextOut(X, Y: Integer; Options: TExtTextOutOptions; Rect: PRect;
  const Text: String; lpDx: Pointer): Boolean;
begin
  Result := ClxExtTextOut(Self, X, Y, ExtTextOutOptionsToInt(Options), Rect, Text, lpDx);
end;

function TUnicodeCanvas.ExtTextOutW(X, Y: Integer; Options: TExtTextOutOptions;
  Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;
begin
  Result := ClxExtTextOutW(Self, X, Y, ExtTextOutOptionsToInt(Options), Rect, Text, lpDx);
end;

end.
