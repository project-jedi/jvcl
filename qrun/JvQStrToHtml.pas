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

The Original Code is: JvStrToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                Andreas Hausladen [Andreas dott Hausladen att gmx dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQStrToHtml;

interface

uses
  SysUtils, Classes,
  JvQComponent;

type
  TJvStrToHtml = class(TJvComponent)
  private
    FHtml: string;
    FValue: string;
    procedure SetHtml(const Value: string);
    procedure SetValue(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    function TextToHtml(const Text: string): string;
    function HtmlToText(const Text: string): string;
  published
    property Text: string read FValue write SetValue;
    property Html: string read FHtml write SetHtml;
  end;

function StringToHtml(const Value: string): string;
function HtmlToString(const Value: string): string;
function CharToHtml(Ch: Char): string;

implementation

type
  TJvHtmlCodeRec = packed record
    Ch: Char;
    Html: PChar;
  end;

const
  Conversions: array [1..79] of TJvHtmlCodeRec = (
    (Ch: '"'; Html: '&quot;'),
    (Ch: 'à'; Html: '&agrave;'),
    (Ch: 'ç'; Html: '&ccedil;'),
    (Ch: 'é'; Html: '&eacute;'),
    (Ch: 'è'; Html: '&egrave;'),
    (Ch: 'ê'; Html: '&ecirc;'),
    (Ch: 'ù'; Html: '&ugrave;'),
    (Ch: 'ë'; Html: '&euml;'),
    (Ch: '<'; Html: '&lt;'),
    (Ch: '>'; Html: '&gt;'),
    (Ch: '^'; Html: '&#136;'),
    (Ch: '~'; Html: '&#152;'),
    (Ch: '£'; Html: '&#163;'),
    (Ch: '§'; Html: '&#167;'),
    (Ch: '°'; Html: '&#176;'),
    (Ch: '²'; Html: '&#178;'),
    (Ch: '³'; Html: '&#179;'),
    (Ch: 'µ'; Html: '&#181;'),
    (Ch: '·'; Html: '&#183;'),
    (Ch: '¼'; Html: '&#188;'),
    (Ch: '½'; Html: '&#189;'),
    (Ch: '¿'; Html: '&#191;'),
    (Ch: 'À'; Html: '&#192;'),
    (Ch: 'Á'; Html: '&#193;'),
    (Ch: 'Â'; Html: '&#194;'),
    (Ch: 'Ã'; Html: '&#195;'),
    (Ch: 'Ä'; Html: '&#196;'),
    (Ch: 'Å'; Html: '&#197;'),
    (Ch: 'Æ'; Html: '&#198;'),
    (Ch: 'Ç'; Html: '&#199;'),
    (Ch: 'È'; Html: '&#200;'),
    (Ch: 'É'; Html: '&#201;'),
    (Ch: 'Ê'; Html: '&#202;'),
    (Ch: 'Ë'; Html: '&#203;'),
    (Ch: 'Ì'; Html: '&#204;'),
    (Ch: 'Í'; Html: '&#205;'),
    (Ch: 'Î'; Html: '&#206;'),
    (Ch: 'Ï'; Html: '&#207;'),
    (Ch: 'Ñ'; Html: '&#209;'),
    (Ch: 'Ò'; Html: '&#210;'),
    (Ch: 'Ó'; Html: '&#211;'),
    (Ch: 'Ô'; Html: '&#212;'),
    (Ch: 'Õ'; Html: '&#213;'),
    (Ch: 'Ö'; Html: '&#214;'),
    (Ch: 'Ù'; Html: '&#217;'),
    (Ch: 'Ú'; Html: '&#218;'),
    (Ch: 'Û'; Html: '&#219;'),
    (Ch: 'Ü'; Html: '&#220;'),
    (Ch: 'Ý'; Html: '&#221;'),
    (Ch: 'ß'; Html: '&#223;'),
    (Ch: 'à'; Html: '&#224;'),
    (Ch: 'á'; Html: '&#225;'),
    (Ch: 'â'; Html: '&#226;'),
    (Ch: 'ã'; Html: '&#227;'),
    (Ch: 'ä'; Html: '&#228;'),
    (Ch: 'å'; Html: '&#229;'),
    (Ch: 'æ'; Html: '&#230;'),
    (Ch: 'ç'; Html: '&#231;'),
    (Ch: 'è'; Html: '&#232;'),
    (Ch: 'é'; Html: '&#233;'),
    (Ch: 'ê'; Html: '&#234;'),
    (Ch: 'ë'; Html: '&#235;'),
    (Ch: 'ì'; Html: '&#236;'),
    (Ch: 'í'; Html: '&#237;'),
    (Ch: 'î'; Html: '&#238;'),
    (Ch: 'ï'; Html: '&#239;'),
    (Ch: 'ñ'; Html: '&#241;'),
    (Ch: 'ò'; Html: '&#242;'),
    (Ch: 'ó'; Html: '&#243;'),
    (Ch: 'ô'; Html: '&#244;'),
    (Ch: 'õ'; Html: '&#245;'),
    (Ch: 'ö'; Html: '&#246;'),
    (Ch: '÷'; Html: '&#247;'),
    (Ch: 'ù'; Html: '&#249;'),
    (Ch: 'ú'; Html: '&#250;'),
    (Ch: 'û'; Html: '&#251;'),
    (Ch: 'ü'; Html: '&#252;'),
    (Ch: 'ý'; Html: '&#253;'),
    (Ch: 'ÿ'; Html: '&#255;')
    );

constructor TJvStrToHtml.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValue := '';
  FHtml := '';
end;

function TJvStrToHtml.HtmlToText(const Text: string): string;
begin
  Result := HtmlToString(Text);
end;

procedure TJvStrToHtml.SetHtml(const Value: string);
begin
  FValue := HtmlToText(Value);
end;

procedure TJvStrToHtml.SetValue(const Value: string);
begin
  FHtml := TextToHtml(Value);
end;

function TJvStrToHtml.TextToHtml(const Text: string): string;
begin
  Result := StringToHtml(Text);
end;

function StringToHtml(const Value: string): string;
var
  I, J: Integer;
  Len, AddLen, HtmlLen: Integer;
  P: PChar;
  Ch: Char;
begin
  Len := Length(Value);
  // number of chars to add
  AddLen := 0;
  for I := 1 to Len do
    for J := Low(Conversions) to High(Conversions) do
      if Value[I] = Conversions[J].Ch then
      begin
        Inc(AddLen, StrLen(Conversions[J].Html) - 1);
        Break;
      end;

  if AddLen = 0 then
    Result := Value
  else
  begin
    SetLength(Result, Len + AddLen);
    P := Pointer(Result);
    for I := 1 to Len do
    begin
      Ch := Value[I];
      for J := Low(Conversions) to High(Conversions) do
        if Ch = Conversions[J].Ch then
        begin
          HtmlLen := StrLen(Conversions[J].Html);
          Move(Conversions[J].Html[0], P[0], HtmlLen); // Conversions[].Html is a PChar
          Inc(P, HtmlLen);
          Ch := #0;
          Break;
        end;
      if Ch <> #0 then
      begin
        P[0] := Ch;
        Inc(P);
      end;
    end;
  end;
end;

function HtmlToString(const Value: string): string;
var
  I, Index, Len: Integer;
  Start, J: Integer;
  Ch: Char;
  ReplStr: string;
begin
  Len := Length(Value);
  SetLength(Result, Len); // worst case
  Index := 0;
  I := 1;
  while I <= Len do
  begin
    Ch := Value[I];
   // html entitiy
    if Ch = '&' then
    begin
      Start := I;
      Inc(I);
      while (I <= Len) and (Value[I] <> ';') and (I < Start + 20) do
        Inc(I);
      if Value[I] <> ';' then
        I := Start
      else
      begin
        Ch := #0;
        ReplStr := LowerCase(Copy(Value, Start, I - Start + 1));
        for J := Low(Conversions) to High(Conversions) do
          if Conversions[J].Html = ReplStr then
          begin
            Ch := Conversions[J].Ch;
            Break;
          end;
        if Ch = #0 then
        begin
          I := Start;
          Ch := Value[I];
        end;
      end;
    end;

    Inc(I);
    Inc(Index);
    Result[Index] := Ch;
  end;
  if Index <> Len then
    SetLength(Result, Index);
end;

function CharToHtml(Ch: Char): string;
var
  I: Integer;
begin
  for I := Low(Conversions) to High(Conversions) do
    if Conversions[I].Ch = Ch then
    begin
      Result := Conversions[I].Html;
      Exit;
    end;
  Result := Ch;
end;

end.

