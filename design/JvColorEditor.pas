{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvColors.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvColorEditor;

{$C PRELOAD}

interface

uses
  SysUtils, Classes,
  {$IFDEF VCL}
  Windows, Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics, Types,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors,
  {$IFDEF VCL}
  VCLEditors,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  ClxEditors,
  {$ENDIF VisualCLX}
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts, JvJVCLUtils;

type
  TJvColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
      {$IFDEF COMPILER5} override {$ELSE} virtual {$ENDIF};
  end;

function JvIdentToColor(const Ident: string; var Color: Longint): Boolean;
function JvColorToString(Color: TColor): string;
function JvStringToColor(S: string): TColor;
procedure JvGetColorValues(Proc: TGetStrProc);
function JvColorToBorderColor(AColor: TColor; ASelected: Boolean): TColor;

implementation

type
  TColorEntry = record
    Value: TColor;
    Name: PChar;
  end;

const
  clInfoBk16 = TColor($02E1FFFF);
  clNone16 = TColor($02FFFFFF);
  ColorCount = 3;
  Colors: array [0..ColorCount - 1] of TColorEntry = (
    (Value: clCream; Name: 'clCream'),
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue'));

function JvColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then
  begin
    for I := Low(Colors) to High(Colors) do
      if Colors[I].Value = Color then
      begin
        Result := Colors[I].Name;
        Exit;
      end;
    Result := Format('$%.8x', [Color]);
  end;
end;

function JvIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
  Text: array [0..63] of Char;
begin
  StrPLCopy(Text, Ident, SizeOf(Text) - 1);
  for I := Low(Colors) to High(Colors) do
    if StrIComp(Colors[I].Name, Text) = 0 then
    begin
      Color := Colors[I].Value;
      Result := True;
      Exit;
    end;
  Result := IdentToColor(Ident, Color);
end;

function JvStringToColor(S: string): TColor;
begin
  if not JvIdentToColor(S, Longint(Result)) then
    Result := StringToColor(S);
end;

procedure JvGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(Colors) to High(Colors) do
    Proc(StrPas(Colors[I].Name));
end;

function JvColorToBorderColor(AColor: TColor; ASelected: Boolean): TColor;
const
  cBlackLevel = 192;
type
  TColorQuad = record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
    Alpha: Byte;
  end;
begin
  // (rom) > or >= ?
  if (TColorQuad(AColor).Red > cBlackLevel) or
    (TColorQuad(AColor).Green > cBlackLevel) or
    (TColorQuad(AColor).Blue > cBlackLevel) then
    Result := clBlack
  else
  if ASelected then
    Result := clWhite
  else
    Result := AColor;
end;

// === TJvColorProperty ======================================================

function TJvColorProperty.GetValue: string;
var
  Color: TColor;
begin
  Color := TColor(GetOrdValue);
  if Color = clNone16 then
    Color := clNone
  else
    if Color = clInfoBk16 then
      Color := clInfoBk;
  Result := JvColorToString(Color);
end;

procedure TJvColorProperty.GetValues(Proc: TGetStrProc);
begin
  JvGetColorValues(Proc);
end;

procedure TJvColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(JvStringToColor(Value));
end;

procedure TJvColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  Rght: Integer;
  OldPenColor, OldBrushColor: TColor;
begin
  Rght := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  try
    OldPenColor := Pen.Color;
    OldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, Rght, ARect.Bottom);
    Brush.Color := JvStringToColor(Value);
    Pen.Color := JvColorToBorderColor(ColorToRGB(Brush.Color), ASelected);
    Rectangle(ARect.Left + 1, ARect.Top + 1, Rght - 1, ARect.Bottom - 1);
    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
  finally
    ACanvas.TextRect(Rect(Rght, ARect.Top, ARect.Right, ARect.Bottom),
      Rght + 1, ARect.Top + 1, Value);
  end;
end;

end.

