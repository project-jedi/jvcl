{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUnicodeCanvas.PAS, released on 2003-09-21

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
Copyright (c) 2003 Andreas Hausladen
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvUnicodeCanvas;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, Windows, Graphics,
  JvJCLUtils;

type
  TJvExtTextOutOptionsKind = (etoClipped, etoOpaque);
  TJvExtTextOutOptions = set of TJvExtTextOutOptionsKind;

  { This Canvas has no new fields and can be type-casted form every TCanvas
    derived class. }
  TJvUnicodeCanvas = class(TCanvas)
  public
    function TextExtentW(const Text: WideString): TSize;
    function TextWidthW(const Text: WideString): Integer;
    function TextHeightW(const Text: WideString): Integer;
    procedure TextOutW(X, Y: Integer; const Text: WideString);
    procedure TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);
    function ExtTextOutW(X, Y: Integer; Options: TJvExtTextOutOptions;
      Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;

    function ExtTextOut(X, Y: Integer; Options: TJvExtTextOutOptions;
      Rect: PRect; const Text: string; lpDx: Pointer): Boolean;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation


function ExtTextOutOptionsToInt(Options: TJvExtTextOutOptions): Integer;
begin
  Result := 0;
  if etoClipped in Options then
    Result := Result or ETO_CLIPPED;
  if etoOpaque in Options then
    Result := Result or ETO_OPAQUE;
end;

function TJvUnicodeCanvas.TextExtentW(const Text: WideString): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  Windows.GetTextExtentPoint32W(Handle, PWideChar(Text), Length(Text), Result);
end;

procedure TJvUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
var
  W: Integer;
begin
  Changing;
  W := TextWidthW(Text);
  if CanvasOrientation = coRightToLeft then
    Inc(X, W + 1);
  Windows.ExtTextOutW(Handle, X, Y, TextFlags, nil, PWideChar(Text), Length(Text), nil);
  MoveTo(X + W, Y);
  Changed;
end;

procedure TJvUnicodeCanvas.TextRectW(Rect: TRect; X, Y: Integer; const Text: WideString);
var
  Options: Longint;
begin
  Changing;
  Options := ETO_CLIPPED or TextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((TextFlags and ETO_RTLREADING) <> 0) and (CanvasOrientation = coRightToLeft) then
    Inc(X, TextWidthW(Text) + 1);
  Windows.ExtTextOutW(Handle, X, Y, Options, @Rect, PWideChar(Text), Length(Text), nil);
  Changed;
end;

function TJvUnicodeCanvas.TextWidthW(const Text: WideString): Integer;
begin
  Result := TextExtentW(Text).cx;
end;

function TJvUnicodeCanvas.TextHeightW(const Text: WideString): Integer;
begin
  Result := TextExtentW(Text).cy;
end;

function TJvUnicodeCanvas.ExtTextOut(X, Y: Integer; Options: TJvExtTextOutOptions;
  Rect: PRect; const Text: string; lpDx: Pointer): Boolean;
begin
  Result := Windows.ExtTextOut(Handle, X, Y, ExtTextOutOptionsToInt(Options),
    Rect, PChar(Text), Length(Text), lpDx);
end;

function TJvUnicodeCanvas.ExtTextOutW(X, Y: Integer; Options: TJvExtTextOutOptions;
  Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;
begin
  Result := Windows.ExtTextOutW(Handle, X, Y, ExtTextOutOptionsToInt(Options),
    Rect, PWideChar(Text), Length(Text), lpDx);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.