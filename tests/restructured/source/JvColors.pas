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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvColors;

{$C PRELOAD}


interface

uses Classes, Controls, Graphics, Forms, JvVCLUtils;

function RxIdentToColor(const Ident: string; var Color: Longint): Boolean;
function RxColorToString(Color: TColor): string;
function RxStringToColor(S: string): TColor;
procedure RxGetColorValues(Proc: TGetStrProc);

procedure RegisterRxColors;

implementation

uses Windows,
 {$IFDEF Delphi6_Up}RTLConsts,DesignIntf, VCLEditors, DesignEditors,{$ELSE}DsgnIntf,{$ENDIF}
SysUtils
;

type
  TColorEntry = record
    Value: TColor;
    Name: PChar;
  end;

const
{$IFDEF WIN32}
  clInfoBk16 = TColor($02E1FFFF);
  clNone16 = TColor($02FFFFFF);
  ColorCount = 3;
{$ELSE}
  ColorCount = 5;
{$ENDIF}
  Colors: array[0..ColorCount - 1] of TColorEntry = (
{$IFNDEF WIN32}
    (Value: clInfoBk;      Name: 'clInfoBk'),
    (Value: clNone;        Name: 'clNone'),
{$ENDIF}
    (Value: clCream;       Name: 'clCream'),
    (Value: clMoneyGreen;  Name: 'clMoneyGreen'),
    (Value: clSkyBlue;     Name: 'clSkyBlue'));

function RxColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then begin
    for I := Low(Colors) to High(Colors) do
      if Colors[I].Value = Color then
      begin
        Result := StrPas(Colors[I].Name);
        Exit;
      end;
    FmtStr(Result, '$%.8x', [Color]);
  end;
end;

function RxIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
  I: Integer;
  Text: array[0..63] of Char;
begin
  StrPLCopy(Text, Ident, SizeOf(Text) - 1);
  for I := Low(Colors) to High(Colors) do
    if StrIComp(Colors[I].Name, Text) = 0 then begin
      Color := Colors[I].Value;
      Result := True;
      Exit;
    end;
  Result := IdentToColor(Ident, Color);
end;

function RxStringToColor(S: string): TColor;
begin
  if not RxIdentToColor(S, Longint(Result)) then
    Result := StringToColor(S);
end;

procedure RxGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(Colors) to High(Colors) do Proc(StrPas(Colors[I].Name));
end;

{ TJvColorProperty }

type
  TJvColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
  end;

function TJvColorProperty.GetValue: string;
var
  Color: TColor;
begin
  Color := TColor(GetOrdValue);
{$IFDEF WIN32}
  if Color = clNone16 then Color := clNone
  else if Color = clInfoBk16 then Color := clInfoBk;
{$ENDIF}
  Result := RxColorToString(Color);
end;

procedure TJvColorProperty.GetValues(Proc: TGetStrProc);
begin
  RxGetColorValues(Proc);
end;

procedure TJvColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(RxStringToColor(Value));
end;

procedure TJvColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);

  function ColorToBorderColor(AColor: TColor): TColor;
  type
    TColorQuad = record
      Red, Green, Blue, Alpha: Byte;
    end;
  begin
    if (TColorQuad(AColor).Red > 192) or (TColorQuad(AColor).Green > 192) or
       (TColorQuad(AColor).Blue > 192) then
      Result := clBlack
    else if ASelected then
      Result := clWhite
    else
      Result := AColor;
  end;

var
  vRight: Integer;
  vOldPenColor, vOldBrushColor: TColor;
begin
  vRight := (ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  try
    vOldPenColor := Pen.Color;
    vOldBrushColor := Brush.Color;
    Pen.Color := Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);
    Brush.Color := RxStringToColor(Value);
    Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);
    Brush.Color := vOldBrushColor;
    Pen.Color := vOldPenColor;
  finally
    ACanvas.TextRect(Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom),
      vRight + 1, ARect.Top + 1, Value);
  end;
end;


procedure RegisterRxColors;
begin
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TJvColorProperty);
end;

end.
