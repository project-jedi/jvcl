{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUnicodeCanvas.PAS, released on 2003-09-20

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
Andreas Hausladen

Last Modified: 2003-09-20

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvUnicodeCanvas;
{$I JVCL.INC}
interface
{$IFDEF COMPLIB_VCL}
 {$DEFINE VCL}
{$ENDIF}
{$IFDEF LINUX}
 {$UNDEF VCL}
 {$DEFINE VisualCLX}
{$ENDIF}

uses
  SysUtils, Classes,
{$IFDEF VCL}
  Windows, Graphics;
{$ENDIF}
{$IFDEF VisualCLX}
  QGraphics;
{$ENDIF}

type
 { This Canvas has no new fields and can be type-casted form every TCanvas
   derived class. }
  TUnicodeCanvas = class(TCanvas)
  public
    function TextExtentW(const Text: WideString): TSize;
    function TextWidthW(const Text: WideString): Integer;
    function TextHeightW(const Text: WideString): Integer;
    procedure TextOutW(X, Y: Integer; const Text: WideString);
    procedure TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);

  {$IFDEF VCL}
    function TextExtent(const Text: WideString): TSize; overload;
    function TextWidth(const Text: WideString): Integer; overload;
    function TextHeight(const Text: WideString): Integer; overload;
    procedure TextOut(X, Y: Integer; const Text: WideString); overload;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: WideString); overload;
  {$ENDIF}
  end;


implementation

{$IFDEF VCL}
function TUnicodeCanvas.TextExtentW(const Text: WideString): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  Windows.GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result);
end;

procedure TUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
var W: Integer;
begin
  W := TextWidth(Text);
  if CanvasOrientation = coRightToLeft then
    Inc(X, W + 1);
  Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text),
    Length(Text), nil);
  MoveTo(X + W, Y);
end;

procedure TUnicodeCanvas.TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);
var
  Options: Longint;
begin
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text),
    Length(Text), nil);
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
{$ENDIF}

{$IFDEF VisualCLX}
function TUnicodeCanvas.TextExtentW(const Text: WideChar): TSize;
begin
  Result := TextExtentW(Text);
end;

function TUnicodeCanvas.TextHeightW(const Text: WideChar): Integer;
begin
  Result := TextHeightW(Text);
end;

procedure TUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
begin
  TextOutW(X, Y, Text);
end;

procedure TUnicodeCanvas.TextRectW(Rect: TRect; X, Y: Integer;
  const Text: WideString);
begin
  TextRectW(Rect, X, Y, Text);
end;

function TUnicodeCanvas.TextWidthW(const Text: WideString): Integer;
begin
  Result := TextWidth(Text);
end;
{$ENDIF}
end.
