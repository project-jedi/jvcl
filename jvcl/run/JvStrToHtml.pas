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

unit JvStrToHtml;

interface

uses
  SysUtils, Classes,
  JvComponent;

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
  Conversions: array [1..72] of TJvHtmlCodeRec = (
    (Ch: '"'; Html: '&quot;'),
    (Ch: '<'; Html: '&lt;'),
    (Ch: '>'; Html: '&gt;'),
    (Ch: '^'; Html: '&circ;'),
    (Ch: '~'; Html: '&tild;'),
    (Ch: '£'; Html: '&pound;'),
    (Ch: '§'; Html: '&sect;'),
    (Ch: '°'; Html: '&deg;'),
    (Ch: '²'; Html: '&sup2;'),
    (Ch: '³'; Html: '&sup3;'),
    (Ch: 'µ'; Html: '&micro;'),
    (Ch: '·'; Html: '&middot;'),
    (Ch: '¼'; Html: '&frac14;'),
    (Ch: '½'; Html: '&frac12;'),
    (Ch: '¿'; Html: '&iquest;'),
    (Ch: 'À'; Html: '&Agrave;'),
    (Ch: 'Á'; Html: '&Aacute;'),
    (Ch: 'Â'; Html: '&Acirc;'),
    (Ch: 'Ã'; Html: '&Atilde;'),
    (Ch: 'Ä'; Html: '&Auml;'),
    (Ch: 'Å'; Html: '&Aring;'),
    (Ch: 'Æ'; Html: '&AElig;'),
    (Ch: 'Ç'; Html: '&Ccedil;'),
    (Ch: 'È'; Html: '&Egrave;'),
    (Ch: 'É'; Html: '&Eacute;'),
    (Ch: 'Ê'; Html: '&Ecirc;'),
    (Ch: 'Ë'; Html: '&Euml;'),
    (Ch: 'Ì'; Html: '&Igrave;'),
    (Ch: 'Í'; Html: '&Iacute;'),
    (Ch: 'Î'; Html: '&Icirc;'),
    (Ch: 'Ï'; Html: '&Iuml;'),
    (Ch: 'Ñ'; Html: '&Ntilde;'),
    (Ch: 'Ò'; Html: '&Ograve;'),
    (Ch: 'Ó'; Html: '&Oacute;'),
    (Ch: 'Ô'; Html: '&Ocirc;'),
    (Ch: 'Õ'; Html: '&Otilde;'),
    (Ch: 'Ö'; Html: '&Ouml;'),
    (Ch: 'Ù'; Html: '&Ugrave;'),
    (Ch: 'Ú'; Html: '&Uacute;'),
    (Ch: 'Û'; Html: '&Ucirc;'),
    (Ch: 'Ü'; Html: '&Uuml;'),
    (Ch: 'Ý'; Html: '&Yacute;'),
    (Ch: 'ß'; Html: '&szlig;'),
    (Ch: 'à'; Html: '&aacute;'),
    (Ch: 'á'; Html: '&agrave;'),
    (Ch: 'â'; Html: '&acirc;'),
    (Ch: 'ã'; Html: '&atilde;'),
    (Ch: 'ä'; Html: '&auml;'),
    (Ch: 'å'; Html: '&aring;'),
    (Ch: 'æ'; Html: '&aelig;'),
    (Ch: 'ç'; Html: 'ccedil;'),
    (Ch: 'é'; Html: '&eacute;'),
    (Ch: 'è'; Html: '&egrave;'),
    (Ch: 'ê'; Html: '&ecirc;'),
    (Ch: 'ë'; Html: '&euml;'),
    (Ch: 'ì'; Html: '&igrave;'),
    (Ch: 'í'; Html: '&iacute;'),
    (Ch: 'î'; Html: '&icirc;'),
    (Ch: 'ï'; Html: '&iuml;'),
    (Ch: 'ñ'; Html: '&ntilde;'),
    (Ch: 'ò'; Html: '&ograve;'),
    (Ch: 'ó'; Html: '&oacute;'),
    (Ch: 'ô'; Html: '&ocirc;'),
    (Ch: 'õ'; Html: '&otilde;'),
    (Ch: 'ö'; Html: '&ouml;'),
    (Ch: '÷'; Html: '&divide;'),
    (Ch: 'ù'; Html: '&ugrave;'),
    (Ch: 'ú'; Html: '&uacute;'),
    (Ch: 'û'; Html: '&ucirc;'),
    (Ch: 'ü'; Html: '&uuml;'),
    (Ch: 'ý'; Html: '&yacute;'),
    (Ch: 'ÿ'; Html: '&yuml;')
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

        // if no conversion was found, it may actually be a number
        if Ch = #0 then
        begin
          if StrToIntDef(ReplStr, -1) <> -1 then
          begin
            Ch := Chr(StrToInt(ReplStr));
          end
          else
          begin
            I := Start;
            Ch := Value[I];
          end;
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

