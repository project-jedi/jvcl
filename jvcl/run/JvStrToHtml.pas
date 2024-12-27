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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStrToHtml;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  JvComponentBase;

type
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
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

{$IFNDEF UNICODE}
uses
  Windows;
{$ENDIF ~UNICODE}

type
  TJvHtmlCodeRec = record
    Ch: Word;
    Html: string;
  end;

const
  { References:
      http://www.w3.org/TR/REC-html40/charset.html#h-5.3
      http://www.w3.org/TR/REC-html40/sgml/entities.html#h-24.2.1
      http://www.w3.org/TR/REC-html40/sgml/entities.html#h-24.4.1
  }
  Conversions: array [0..240] of TJvHtmlCodeRec = (
    (Ch:   34; Html: '&quot;'),
    (Ch:   38; Html: '&amp;'),
    (Ch:   39; Html: '&apos;'),
    (Ch:   60; Html: '&lt;'),
    (Ch:   62; Html: '&gt;'),
    (Ch:  160; Html: '&nbsp;'),
    (Ch:  161; Html: '&iexcl;'),
    (Ch:  162; Html: '&cent;'),
    (Ch:  163; Html: '&pound;'),
    (Ch:  164; Html: '&curren;'),
    (Ch:  165; Html: '&yen;'),
    (Ch:  166; Html: '&brvbar;'),
    (Ch:  167; Html: '&sect;'),
    (Ch:  168; Html: '&uml;'),
    (Ch:  169; Html: '&copy;'),
    (Ch:  170; Html: '&ordf;'),
    (Ch:  171; Html: '&laquo;'),
    (Ch:  172; Html: '&not;'),
    (Ch:  173; Html: '&shy;'),
    (Ch:  174; Html: '&reg;'),
    (Ch:  175; Html: '&macr;'),
    (Ch:  176; Html: '&deg;'),
    (Ch:  177; Html: '&plusmn;'),
    (Ch:  178; Html: '&sup2;'),
    (Ch:  179; Html: '&sup3;'),
    (Ch:  180; Html: '&acute;'),
    (Ch:  181; Html: '&micro;'),
    (Ch:  182; Html: '&para;'),
    (Ch:  183; Html: '&middot;'),
    (Ch:  184; Html: '&cedil;'),
    (Ch:  185; Html: '&sup1;'),
    (Ch:  186; Html: '&ordm;'),
    (Ch:  187; Html: '&raquo;'),
    (Ch:  188; Html: '&frac14;'),
    (Ch:  189; Html: '&frac12;'),
    (Ch:  190; Html: '&frac34;'),
    (Ch:  191; Html: '&iquest;'),
    (Ch:  192; Html: '&Agrave;'),
    (Ch:  193; Html: '&Aacute;'),
    (Ch:  194; Html: '&Acirc;'),
    (Ch:  195; Html: '&Atilde;'),
    (Ch:  196; Html: '&Auml;'),
    (Ch:  197; Html: '&Aring;'),
    (Ch:  198; Html: '&AElig;'),
    (Ch:  199; Html: '&Ccedil;'),
    (Ch:  200; Html: '&Egrave;'),
    (Ch:  201; Html: '&Eacute;'),
    (Ch:  202; Html: '&Ecirc;'),
    (Ch:  203; Html: '&Euml;'),
    (Ch:  204; Html: '&Igrave;'),
    (Ch:  205; Html: '&Iacute;'),
    (Ch:  206; Html: '&Icirc;'),
    (Ch:  207; Html: '&Iuml;'),
    (Ch:  208; Html: '&ETH;'),
    (Ch:  209; Html: '&Ntilde;'),
    (Ch:  210; Html: '&Ograve;'),
    (Ch:  211; Html: '&Oacute;'),
    (Ch:  212; Html: '&Ocirc;'),
    (Ch:  213; Html: '&Otilde;'),
    (Ch:  214; Html: '&Ouml;'),
    (Ch:  215; Html: '&times;'),
    (Ch:  216; Html: '&Oslash;'),
    (Ch:  217; Html: '&Ugrave;'),
    (Ch:  218; Html: '&Uacute;'),
    (Ch:  219; Html: '&Ucirc;'),
    (Ch:  220; Html: '&Uuml;'),
    (Ch:  221; Html: '&Yacute;'),
    (Ch:  222; Html: '&THORN;'),
    (Ch:  223; Html: '&szlig;'),
    (Ch:  224; Html: '&agrave;'),
    (Ch:  225; Html: '&aacute;'),
    (Ch:  226; Html: '&acirc;'),
    (Ch:  227; Html: '&atilde;'),
    (Ch:  228; Html: '&auml;'),
    (Ch:  229; Html: '&aring;'),
    (Ch:  230; Html: '&aelig;'),
    (Ch:  231; Html: '&ccedil;'),
    (Ch:  232; Html: '&egrave;'),
    (Ch:  233; Html: '&eacute;'),
    (Ch:  234; Html: '&ecirc;'),
    (Ch:  235; Html: '&euml;'),
    (Ch:  236; Html: '&igrave;'),
    (Ch:  237; Html: '&iacute;'),
    (Ch:  238; Html: '&icirc;'),
    (Ch:  239; Html: '&iuml;'),
    (Ch:  240; Html: '&eth;'),
    (Ch:  241; Html: '&ntilde;'),
    (Ch:  242; Html: '&ograve;'),
    (Ch:  243; Html: '&oacute;'),
    (Ch:  244; Html: '&ocirc;'),
    (Ch:  245; Html: '&otilde;'),
    (Ch:  246; Html: '&ouml;'),
    (Ch:  247; Html: '&divide;'),
    (Ch:  248; Html: '&oslash;'),
    (Ch:  249; Html: '&ugrave;'),
    (Ch:  250; Html: '&uacute;'),
    (Ch:  251; Html: '&ucirc;'),
    (Ch:  252; Html: '&uuml;'),
    (Ch:  253; Html: '&yacute;'),
    (Ch:  254; Html: '&thorn;'),
    (Ch:  255; Html: '&yuml;'),
    (Ch:  338; Html: '&OElig;'),
    (Ch:  339; Html: '&oelig;'),
    (Ch:  352; Html: '&Scaron;'),
    (Ch:  353; Html: '&scaron;'),
    (Ch:  376; Html: '&Yuml;'),
    (Ch:  402; Html: '&fnof;'),
    (Ch:  710; Html: '&circ;'),
    (Ch:  732; Html: '&tilde;'),
    (Ch:  913; Html: '&Alpha;'),
    (Ch:  914; Html: '&Beta;'),
    (Ch:  915; Html: '&Gamma;'),
    (Ch:  916; Html: '&Delta;'),
    (Ch:  917; Html: '&Epsilon;'),
    (Ch:  918; Html: '&Zeta;'),
    (Ch:  919; Html: '&Eta;'),
    (Ch:  920; Html: '&Theta;'),
    (Ch:  921; Html: '&Iota;'),
    (Ch:  922; Html: '&Kappa;'),
    (Ch:  923; Html: '&Lambda;'),
    (Ch:  924; Html: '&Mu;'),
    (Ch:  925; Html: '&Nu;'),
    (Ch:  926; Html: '&Xi;'),
    (Ch:  927; Html: '&Omicron;'),
    (Ch:  928; Html: '&Pi;'),
    (Ch:  929; Html: '&Rho;'),
    (Ch:  931; Html: '&Sigma;'),
    (Ch:  932; Html: '&Tau;'),
    (Ch:  933; Html: '&Upsilon;'),
    (Ch:  934; Html: '&Phi;'),
    (Ch:  935; Html: '&Chi;'),
    (Ch:  936; Html: '&Psi;'),
    (Ch:  937; Html: '&Omega;'),
    (Ch:  945; Html: '&alpha;'),
    (Ch:  946; Html: '&beta;'),
    (Ch:  947; Html: '&gamma;'),
    (Ch:  948; Html: '&delta;'),
    (Ch:  949; Html: '&epsilon;'),
    (Ch:  950; Html: '&zeta;'),
    (Ch:  951; Html: '&eta;'),
    (Ch:  952; Html: '&theta;'),
    (Ch:  953; Html: '&iota;'),
    (Ch:  954; Html: '&kappa;'),
    (Ch:  955; Html: '&lambda;'),
    (Ch:  956; Html: '&mu;'),
    (Ch:  957; Html: '&nu;'),
    (Ch:  958; Html: '&xi;'),
    (Ch:  959; Html: '&omicron;'),
    (Ch:  960; Html: '&pi;'),
    (Ch:  961; Html: '&rho;'),
    (Ch:  962; Html: '&sigmaf;'),
    (Ch:  963; Html: '&sigma;'),
    (Ch:  964; Html: '&tau;'),
    (Ch:  965; Html: '&upsilon;'),
    (Ch:  966; Html: '&phi;'),
    (Ch:  967; Html: '&chi;'),
    (Ch:  968; Html: '&psi;'),
    (Ch:  969; Html: '&omega;'),
    (Ch:  977; Html: '&thetasym;'),
    (Ch:  978; Html: '&upsih;'),
    (Ch:  982; Html: '&piv;'),
    (Ch: 8194; Html: '&ensp;'),
    (Ch: 8195; Html: '&emsp;'),
    (Ch: 8201; Html: '&thinsp;'),
    (Ch: 8204; Html: '&zwnj;'),
    (Ch: 8205; Html: '&zwj;'),
    (Ch: 8206; Html: '&lrm;'),
    (Ch: 8207; Html: '&rlm;'),
    (Ch: 8211; Html: '&ndash;'),
    (Ch: 8212; Html: '&mdash;'),
    (Ch: 8216; Html: '&lsquo;'),
    (Ch: 8217; Html: '&rsquo;'),
    (Ch: 8218; Html: '&sbquo;'),
    (Ch: 8220; Html: '&ldquo;'),
    (Ch: 8221; Html: '&rdquo;'),
    (Ch: 8222; Html: '&bdquo;'),
    (Ch: 8224; Html: '&dagger;'),
    (Ch: 8225; Html: '&Dagger;'),
    (Ch: 8226; Html: '&bull;'),
    (Ch: 8230; Html: '&hellip;'),
    (Ch: 8240; Html: '&permil;'),
    (Ch: 8242; Html: '&prime;'),
    (Ch: 8243; Html: '&Prime;'),
    (Ch: 8249; Html: '&lsaquo;'),
    (Ch: 8250; Html: '&rsaquo;'),
    (Ch: 8254; Html: '&oline;'),
    (Ch: 8364; Html: '&euro;'),
    (Ch: 8482; Html: '&trade;'),
    (Ch: 8592; Html: '&larr;'),
    (Ch: 8593; Html: '&uarr;'),
    (Ch: 8594; Html: '&rarr;'),
    (Ch: 8595; Html: '&darr;'),
    (Ch: 8596; Html: '&harr;'),
    (Ch: 8629; Html: '&crarr;'),
    (Ch: 8704; Html: '&forall;'),
    (Ch: 8706; Html: '&part;'),
    (Ch: 8707; Html: '&exist;'),
    (Ch: 8709; Html: '&empty;'),
    (Ch: 8711; Html: '&nabla;'),
    (Ch: 8712; Html: '&isin;'),
    (Ch: 8713; Html: '&notin;'),
    (Ch: 8715; Html: '&ni;'),
    (Ch: 8719; Html: '&prod;'),
    (Ch: 8721; Html: '&sum;'),
    (Ch: 8722; Html: '&minus;'),
    (Ch: 8727; Html: '&lowast;'),
    (Ch: 8730; Html: '&radic;'),
    (Ch: 8733; Html: '&prop;'),
    (Ch: 8734; Html: '&infin;'),
    (Ch: 8736; Html: '&ang;'),
    (Ch: 8743; Html: '&and;'),
    (Ch: 8744; Html: '&or;'),
    (Ch: 8745; Html: '&cap;'),
    (Ch: 8746; Html: '&cup;'),
    (Ch: 8747; Html: '&int;'),
    (Ch: 8756; Html: '&there4;'),
    (Ch: 8764; Html: '&sim;'),
    (Ch: 8773; Html: '&cong;'),
    (Ch: 8776; Html: '&asymp;'),
    (Ch: 8800; Html: '&ne;'),
    (Ch: 8801; Html: '&equiv;'),
    (Ch: 8804; Html: '&le;'),
    (Ch: 8805; Html: '&ge;'),
    (Ch: 8834; Html: '&sub;'),
    (Ch: 8835; Html: '&sup;'),
    (Ch: 8836; Html: '&nsub;'),
    (Ch: 8838; Html: '&sube;'),
    (Ch: 8839; Html: '&supe;'),
    (Ch: 8853; Html: '&oplus;'),
    (Ch: 8855; Html: '&otimes;'),
    (Ch: 8869; Html: '&perp;'),
    (Ch: 8901; Html: '&sdot;'),
    (Ch: 8968; Html: '&lceil;'),
    (Ch: 8969; Html: '&rceil;'),
    (Ch: 8970; Html: '&lfloor;'),
    (Ch: 8971; Html: '&rfloor;'),
    (Ch: 9674; Html: '&loz;'),
    (Ch: 9824; Html: '&spades;'),
    (Ch: 9827; Html: '&clubs;'),
    (Ch: 9829; Html: '&hearts;'),
    (Ch: 9830; Html: '&diams;')
  );

var
  ConversionsHash: array of Word;

{$IFNDEF UNICODE}
const
  MB_ERR_INVALID_CHARS = 8;
{$ENDIF ~UNICODE}

{ TJvStrToHtml }

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

function GetHtmlHash(const S: string): Word;
var
  I: Integer;
begin
  Result := Length(S);
  for I := 1 to Length(S) do
    Result := Word(Result + Ord(S[I]) shl (I mod 4));
end;

procedure InitConversionsHash;
var
  I: Integer;
begin
  SetLength(ConversionsHash, Length(Conversions));
  for I := 0 to High(ConversionsHash) do
    ConversionsHash[I] := GetHtmlHash(Conversions[I].Html);
end;

function StringToHtml(const Value: string): string;
const
  Nbsp = '&nbsp;';
var
  I, J: Integer;
  Len, AddLen, HtmlLen: Integer;
  P: PChar;
  Ch: Char;
  W: Word;
  Even: Boolean;
begin
  Len := Length(Value);
  // number of chars to add
  AddLen := 0;
  I := 1;
  while I <= Len do
  begin
    Ch := Value[I];
    if Ch = ' ' then
    begin
      Even := False;
      repeat
        if Even then
          Inc(AddLen, 5 {Length(Nbsp) - 1});
        Even := not Even;
        Inc(I);
      until (I > Len) or (Value[I] <> ' ');
      Continue;
    end
    else
    if (Ord(Ch) >= 128) or not (AnsiChar(Ch) in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
    begin
      W := Word(Ch);
      {$IFNDEF UNICODE}
      if W >= 128 then
        if MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, @Ch, 1, PWideChar(@W), 1) = 0 then
          W := Word(Ch);
      {$ENDIF ~UNICODE}
      for J := Low(Conversions) to High(Conversions) do
        if W = Conversions[J].Ch then
        begin
          Inc(AddLen, Length(Conversions[J].Html) - 1);
          Break;
        end;
    end;
    Inc(I);
  end;

  if AddLen = 0 then
    Result := Value
  else
  begin
    SetLength(Result, Len + AddLen);
    P := Pointer(Result);
    I := 1;
    while I <= Len do
    begin
      Ch := Value[I];
      if Ch = ' ' then
      begin
        Even := False;
        repeat
          if Even then
          begin
            HtmlLen := 6 {Length(Nbsp)};
            Move(Nbsp[1], P[0], HtmlLen * SizeOf(Char));
            Inc(P, HtmlLen);
          end
          else
          begin
            P[0] := ' ';
            Inc(P);
          end;
          Even := not Even;
          Inc(I);
        until (I > Len) or (Value[I] <> ' ');
        Continue;
      end
      else
      if (Ord(Ch) >= 128) or not (AnsiChar(Ch) in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
      begin
        W := Word(Ch);
        {$IFNDEF UNICODE}
        if W >= 128 then
          if MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, @Ch, 1, PWideChar(@W), 1) = 0 then
            W := Word(Ch);
        {$ENDIF ~UNICODE}
        for J := Low(Conversions) to High(Conversions) do
          if W = Conversions[J].Ch then
          begin
            HtmlLen := Length(Conversions[J].Html);
            Move(Conversions[J].Html[1], P[0], HtmlLen * SizeOf(Char)); // Conversions[].Html is a PChar
            Inc(P, HtmlLen);
            Ch := #0;
            Break;
          end;
      end;
      if Ch <> #0 then
      begin
        P[0] := Ch;
        Inc(P);
      end;
      Inc(I);
    end;
  end;
end;

function HtmlToString(const Value: string): string;
var
  I, Index, Len: Integer;
  Start, J: Integer;
  Ch: Char;
  W, Hash: Word;
  ReplStr: string;
begin
  if ConversionsHash = nil then
    InitConversionsHash;

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
        ReplStr := Copy(Value, Start, I - Start + 1);
        if ReplStr = '&nbsp;' then // special treatment for &nbsp
          Ch := ' '
        else
        begin
          Hash := GetHtmlHash(ReplStr);
          J := 0;
          while (J < Length(Conversions)) do
          begin
            while (J < Length(Conversions)) and (ConversionsHash[J] <> Hash) do
              Inc(J);
            if (J < Length(Conversions)) and (Conversions[J].Html = ReplStr) then
            begin
              W := Conversions[J].Ch;
              Ch := Char(W);
              {$IFNDEF UNICODE}
              if W >= 128 then
                if WideCharToMultiByte(CP_ACP, MB_ERR_INVALID_CHARS, @W, 1, PAnsiChar(@Ch), 1, nil, nil) = 0 then
                  Ch := Char(W);
              {$ENDIF ~UNICODE}
              Break;
            end;
            Inc(J);
          end;
        end;

        // if no conversion was found, it may actually be a number
        if Ch = #0 then
        begin
          ReplStr := Copy(ReplStr, 2, MaxInt);
          if ReplStr <> '' then
          begin
            if (ReplStr[1] = '#') and (Length(ReplStr) > 1) then
            begin
              Delete(ReplStr, 1, 1);
              if ReplStr[1] = 'x' then // hex value
                ReplStr[1] := '$'; // prepare for StrToInt
            end;
            if StrToIntDef(ReplStr, -1) <> -1 then
              Ch := Chr(StrToInt(ReplStr))
            else
            begin
              I := Start;
              Ch := Value[I];
            end;
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
  W: Word;
begin
  if (Ord(Ch) >= 128) or not (AnsiChar(Ch) in ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
  begin
    W := Word(Ch);
    {$IFNDEF UNICODE}
    if (W < 128) or (MultiByteToWideChar(CP_ACP, MB_ERR_INVALID_CHARS, @Ch, 1, PWideChar(@W), 1) <> 0) then
    {$ENDIF ~UNICODE}
    begin
      I := 0;
      while (I < Length(Conversions)) and (Conversions[I].Ch <> W) do
        Inc(I);
      if I < Length(Conversions) then
      begin
        Result := Conversions[I].Html;
        Exit;
      end;
    end;
  end;
  Result := Ch;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
