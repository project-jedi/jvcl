{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrToHtml.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStrToHtml;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, JvComponent;

type
  TJvStrToHtml = class(TJvComponent)
  private
    FHtml: string;
    FValue: string;
    procedure SetHtml(const Value: string);
    procedure SetValue(const Value: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Text: string read FValue write SetValue;
    property Html: string read FHtml write SetHtml;
    function CharToHtml(Ch: Char): string;
    function TextToHtml(Text: string): string;
    function HtmlToText(Text: string): string;
  end;

function StringToHtml(Value: string): string;
function HtmlToString(Value: string): string;

implementation

type
  TJvHtmlCodeRec = record
    Ch: Char;
    Html: string;
  end;

const
  Conversions: array[1..79] of TJvHtmlCodeRec = (
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

  {**************************************************}

function TJvStrToHtml.CharToHtml(Ch: Char): string;
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

{**************************************************}

constructor TJvStrToHtml.Create(AOwner: TComponent);
begin
  inherited;
  FValue := '';
  FHtml := '';
end;

{**************************************************}

function TJvStrToHtml.HtmlToText(Text: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Text) do
    Result := Result + CharToHtml(Text[I]);
end;

{**************************************************}

procedure TJvStrToHtml.SetHtml(const Value: string);
begin
  FValue := HtmlToText(Value);
end;

{**************************************************}

procedure TJvStrToHtml.SetValue(const Value: string);
begin
  FHtml := TextToHtml(Value);
end;

{**************************************************}

function TJvStrToHtml.TextToHtml(Text: string): string;
var
  i: Integer;
begin
  Result := Text;
  for i := Low(Conversions) to High(Conversions) do
    Result := StringReplace(Result, Conversions[i].Html, Conversions[i].Ch,
      [rfReplaceAll, rfIgnoreCase]);
end;

{**************************************************}

// (rom) this is silly. Better base the component methods on the functions.

function StringToHtml(Value: string): string;
begin
  with TJvStrToHtml.Create(nil) do
  begin
    Result := TextToHtml(Value);
    Free;
  end;
end;

{**************************************************}

function HtmlToString(Value: string): string;
begin
  with TJvStrToHtml.Create(nil) do
  begin
    Result := HtmlToText(Value);
    Free;
  end;
end;

end.
