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

Last Modified: 2003-09-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvUnicodeCanvas;
{$I JVCL.INC}
interface

uses
  SysUtils, Classes,
{$IFDEF COMPLIB_VCL}
  Windows, Graphics;
{$ENDIF}
{$IFDEF COMPLIB_CLX}
  QGraphics;
{$ENDIF}

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

  {$IFDEF COMPLIB_VCL}
   {$IFDEF COMPILER_6UP}
    function TextExtent(const Text: WideString): TSize; overload;
    function TextWidth(const Text: WideString): Integer; overload;
    function TextHeight(const Text: WideString): Integer; overload;
    procedure TextOut(X, Y: Integer; const Text: WideString); overload;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: WideString); overload;
   {$ENDIF}
  {$ENDIF}

  {$IFDEF COMPLIB_CLX}
    procedure TextOutVCL(Canvas: TCanvas; X, Y: Integer; const Text: WideString);
    procedure TextRectVCL(Canvas: TCanvas; Rect: TRect; X, Y: Integer;
      const Text: WideString; TextFlags: Integer = 0);
  {$ENDIF}
  end;

implementation

{$IFDEF MSWINDOWS}
function ExtTextOutOptionsToInt(Options: TExtTextOutOptions): Integer;
begin
  Result := 0;
  if etoClipped in Options then Result := Result or ETO_CLIPPED;
  if etoOpaque in Options then Result := Result or ETO_OPAQUE;
end;
{$ENDIF}

{$IFDEF COMPLIB_VCL}
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

{$ENDIF COMPLIB_VCL}


{$IFDEF COMPLIB_CLX}
function TUnicodeCanvas.TextExtentW(const Text: WideChar): TSize;
begin
  Result := TextExtent(Text);
end;

function TUnicodeCanvas.TextHeightW(const Text: WideChar): Integer;
begin
  Result := TextHeight(Text);
end;

procedure TUnicodeCanvas.TextOutW(X, Y: Integer; const Text: WideString);
begin
  TextOutVCL(X, Y, Text);
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
  if Canvas.Brush.Style = bsSolid then
  begin
    R := Rect(0, 0, MaxLongint, MaxLongint);
    Canvas.TextExtent(Text, R);
    OffsetRect(R, X, Y);
    Canvas.FillRect(R);
  end;
  Canvas.TextOut(X, Y, Text);
end;

procedure TUnicodeCanvas.TextRectVCL(Rect: TRect; X, Y: Integer;
  const Text: WideString; TextFlags: Integer = 0);
begin
  if Canvas.Brush.Style = bsSolid then
    Canvas.FillRect(Rect);
  Canvas.TextRect(Rect, X, Y, Text, TextFlags);
end;
{$ENDIF COMPLIB_CLX}

function TUnicodeCanvas.ExtTextOut(X, Y: Integer; Options: TExtTextOutOptions; Rect: PRect;
  const Text: String; lpDx: Pointer): Boolean;
begin
{$IFDEF MSWINDOWS}
  Changing;
  Result := Windows.ExtTextOut(Handle, X, Y, ExtTextOutOptionsToInt(Options),
    Rect, PChar(Text), Length(Text), lpDx);
  Changed;
{$ELSE}
  Result := ExtTextOutW(X, Y; Options, Rect, WideString(Text), lpDx);
{$ENDIF}
end;

function TUnicodeCanvas.ExtTextOutW(X, Y: Integer; Options: TExtTextOutOptions;
  Rect: PRect; const Text: WideString; lpDx: Pointer): Boolean;
{$IFDEF MSWINDOWS}
begin
  Changing;
  Result := Windows.ExtTextOutW(Handle, X, Y, ExtTextOutOptionsToInt(Options),
    Rect, PWideChar(Text), Length(Text), lpDx);
  Changed;
end;
{$ELSE}

{ missing feature: horizontal text alignment }
var
  RecallBrush: TBrush;
  RecallPenPos: TPoint;
  Ch: WideChar;
  Index, Width: Integer;
  Dx: PInteger;
  R, CellRect: TRect;
  TextLen: Integer;
begin
  Result := False;
  if (Text = '') then
    Exit;
  if (etoClipped in Options) and (Rect = nil) then
    Exclude(Options, etoClipped);

  RecallPenPos := PenPos;
  Result := True;
  Changing;
  RecallBrush := nil;
  try
    if etoOpaque in Options then
    begin
      if Brush.Style <> bsSolid then
      begin
        RecallBrush := TBrush.Create;
        RecallBrush.Assign(Brush);
        Brush.Style := bsSolid;
      end;
      if Rect <> nil then
        FillRect(Rect^);
    end
    else
      if (Brush.Style = bsSolid) then
      begin
        RecallBrush := TBrush.Create;
        RecallBrush.Assign(Brush);
        Brush.Style := bsClear;
      end;

    if lpDx = nil then
    begin
      if (etoClipped in Options) then
        TextRectW(Rect^, X, Y, Text)
      else
        TextOutW(X, Y, Text);
    end
    else
    begin
     // put each char in its cell
      TextLen := Length(Text);
      if (etoOpaque in Options) and (Rect = nil) then
      begin
        Dx := lpDx;
        Width := 0;
        for Index := 1 to TextLen do
        begin
          Inc(Width, Dx^);
          Inc(Dx);
        end;
        R.Left := X;
        R.Right := X + Width;
        R.Top := Y;
        R.Bottom := Y + TextHeightW(Text);
        FillRect(R);
      end;

      Dx := lpDx;
      for Index := 1 to TextLen do
      begin
        if (Rect <> nil) and (X >= Rect^.Right) then
          Break;

        Ch := Text[Index];
        if etoClipped in Options then
        begin
          CellRect.Left := X;
          CellRect.Right := X + Dx^;
          CellRect.Top := Rect^.Top;
          CellRect.Bottom := Rect^.Bottom;
          if CellRect.Right > Rect^.Right then
            CellRect.Right := Rect^.Right;
          TextRectW(R, X, Y, Ch);
        end
        else
          TextOutW(X, Y, Ch);

        if Index = TextLen then
          Break;

        Inc(X, Dx^);
        Inc(Dx);
      end;
    end;
  finally
    if Assigned(RecallBrush) then
    begin
      Brush.Assign(RecallBrush);
      RecallBrush.Free;
    end;
  end;
  Changed;
  PenPos := RecallPenPos;
end;
{$ENDIF MSWINDOWS}

end.
