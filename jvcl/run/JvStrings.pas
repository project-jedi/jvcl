{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrings.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1 dott verhoeven att wxs dott nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove att slcdug dott org].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Should be merged with JCL
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

{$IFDEF COMPILER6_UP}
{$IFDEF MSWINDOWS}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF MSWINDOWS}
{$ENDIF COMPILER6_UP}

unit JvStrings;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Graphics,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QGraphics,
  {$ENDIF VisualCLX}
  SysUtils, Classes;

{regular expressions}

{template functions}
function ReplaceFirst(sourceStr, findStr, replaceStr: string): string;
function ReplaceLast(sourceStr, findStr, replaceStr: string): string;
function InsertLastBlock(var sourceStr: string; blockStr: string): Boolean;
function removeMasterBlocks(sourceStr: string): string;
function removeFields(sourceStr: string): string;

{http functions}
function URLEncode(Value: string): string; // Converts string To A URLEncoded string
function URLDecode(Value: string): string; // Converts string From A URLEncoded string

{set functions}
procedure SplitSet(AText: string; AList: TStringList);
function JoinSet(AList: TStringList): string;
function FirstOfSet(AText: string): string;
function LastOfSet(AText: string): string;
function CountOfSet(AText: string): Integer;
function SetRotateRight(AText: string): string;
function SetRotateLeft(AText: string): string;
function SetPick(AText: string; aIndex: Integer): string;
function SetSort(AText: string): string;
function SetUnion(set1, set2: string): string;
function SetIntersect(set1, set2: string): string;
function SetExclude(set1, set2: string): string;

{replace any <,> etc by &lt; &gt;}
function XMLSafe(AText: string): string;

{simple hash, Result can be used in Encrypt}
function Hash(AText: string): Integer;

{ Base64 encode and decode a string }
function B64Encode(const S: string): string;
function B64Decode(const S: string): string;

{Basic encryption from a Borland Example}
function Encrypt(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
function Decrypt(const Instring: string; StartKey, MultKey, AddKey: Integer): string;

{Using Encrypt and Decrypt in combination with B64Encode and B64Decode}
function EncryptB64(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
function DecryptB64(const Instring: string; StartKey, MultKey, AddKey: Integer): string;

procedure csv2tags(src, dst: TStringList);
// converts a csv list to a tagged string list

procedure tags2csv(src, dst: TStringList);
// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure ListSelect(src, dst: TStringList; AKey, AValue: string);
{selects akey=avalue from src and returns recordset in dst}

procedure ListFilter(src: TStringList; AKey, AValue: string);
{filters src for akey=avalue}

procedure ListOrderBy(src: TStringList; AKey: string; Numeric: Boolean);
{orders a tagged src list by akey}

function PosStr(const Findstring, Sourcestring: string;
  StartPos: Integer = 1): Integer;
{ PosStr searches the first occurrence of a substring Findstring in a string
  given by Sourcestring with case sensitivity (upper and lower case characters
  are differed). This function returns the index value of the first character
  of a specified substring from which it occurs in a given string starting with
  StartPos character index. If a specified substring is not found Q_PosStr
  returns zero. The author of algorithm is Peter Morris (UK) (Faststrings unit
  from www.torry.ru). }

function PosStrLast(const Findstring, Sourcestring: string): Integer;
{finds the last occurance}

function LastPosChar(const FindChar: Char; Sourcestring: string): Integer;

function PosText(const Findstring, Sourcestring: string;
  StartPos: Integer = 1): Integer;
{ PosText searches the first occurrence of a substring Findstring in a string
  given by Sourcestring without case sensitivity (upper and lower case
  characters are not differed). This function returns the index value of the
  first character of a specified substring from which it occurs in a given
  string starting with StartPos character index. If a specified substring is
  not found Q_PosStr returns zero. The author of algorithm is Peter Morris
  (UK) (Faststrings unit from www.torry.ru). }

function PosTextLast(const Findstring, Sourcestring: string): Integer;
{finds the last occurance}

function NameValuesToXML(AText: string): string;
{$IFDEF MSWINDOWS}
procedure LoadResourceFile(AFile: string; ms: TMemoryStream);
{$ENDIF MSWINDOWS}
procedure DirFiles(ADir, AMask: string; AFileList: TStringList);
procedure RecurseDirFiles(myDir: string; var AFileList: TStringList);
procedure RecurseDirProgs(myDir: string; var AFileList: TStringList);
procedure Savestring(AFile, AText: string);
function Loadstring(AFile: string): string;
function HexToColor(AText: string): TColor;
function UppercaseHTMLTags(AText: string): string;
function LowercaseHTMLTags(AText: string): string;
procedure GetHTMLAnchors(AFile: string; AList: TStringList);
function relativepath(aSrc, aDst: string): string;
function GetToken(var start: Integer; SourceText: string): string;
function PosNonSpace(Start: Integer; SourceText: string): Integer;
function PosEscaped(Start: Integer; SourceText, FindText: string; escapeChar: Char): Integer;
function DeleteEscaped(SourceText: string; escapeChar: Char): string;
function BeginOfAttribute(Start: Integer; SourceText: string): Integer;
// parses the beginning of an attribute: space + alpha character
function ParseAttribute(var Start: Integer; SourceText: string; var aName: string; var AValue: string): Boolean;
// parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
procedure ParseAttributes(SourceText: string; var Attributes: TStringList);
// parses all name=value attributes to the attributes TStringList
function HasStrValue(AText, aName: string; var AValue: string): Boolean;
// checks if a name="value" pair exists and returns any value
function GetStrValue(AText, aName, aDefault: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1 dott verhoeven att wxs dott nl"
// returns aDefault when not found
function GetHTMLColorValue(AText, aName: string; aDefault: TColor): TColor;
// same for a color
function GetIntValue(AText, aName: string; aDefault: Integer): Integer;
// same for an Integer
function GetFloatValue(AText, aName: string; aDefault: extended): extended;
// same for a float
function GetBoolValue(AText, aName: string): Boolean;
// same for Boolean but without default
function GetValue(AText, aName: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1 dott verhoeven att wxs dott nl"
procedure SetValue(var AText: string; aName, AValue: string);
// sets a string value in a line
procedure DeleteValue(var AText: string; aName: string);
// deletes a aName="value" pair from AText

procedure GetNames(AText: string; AList: TStringList);
// get a list of names from a string with name="value" pairs
function GetHTMLColor(aColor: TColor): string;
// converts a color value to the HTML hex value
function BackPosStr(start: Integer; Findstring, Sourcestring: string): Integer;
// finds a string backward case sensitive
function BackPosText(start: Integer; Findstring, Sourcestring: string): Integer;
// finds a string backward case insensitive
function PosRangeStr(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range, e.g. <TD>....</TD> case sensitive
function PosRangeText(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range, e.g. <TD>....</td> case insensitive
function BackPosRangeStr(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range backward, e.g. <TD>....</TD> case sensitive
function BackPosRangeText(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range backward, e.g. <TD>....</td> case insensitive
function PosTag(Start: Integer; Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a HTML or XML tag:  <....>
function Innertag(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds the innertext between opening and closing tags
function Easter(nYear: Integer): TDateTime;
// returns the easter date of a year.
function getWeekNumber(today: Tdatetime): string;
//gets a datecode. Returns year and weeknumber in format: YYWW

function parseNumber(s: string): Integer;
// parse number returns the last position, starting from 1
function ParseDate(s: string): Integer;
// parse a SQL style data string from positions 1,
// starts and ends with #

implementation

uses
  JvConsts, JvResources, JvTypes;

const
  B64Table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  ValidURLChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$-_@.&+-!*"''(),;/#?:';

  ToUpperChars: array [0..255] of Char =
  (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
    #$40, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$5B, #$5C, #$5D, #$5E, #$5F,
    #$60, #$41, #$42, #$43, #$44, #$45, #$46, #$47, #$48, #$49, #$4A, #$4B, #$4C, #$4D, #$4E, #$4F,
    #$50, #$51, #$52, #$53, #$54, #$55, #$56, #$57, #$58, #$59, #$5A, #$7B, #$7C, #$7D, #$7E, #$7F,
    #$80, #$81, #$82, #$81, #$84, #$85, #$86, #$87, #$88, #$89, #$8A, #$8B, #$8C, #$8D, #$8E, #$8F,
    #$80, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$8A, #$9B, #$8C, #$8D, #$8E, #$8F,
    #$A0, #$A1, #$A1, #$A3, #$A4, #$A5, #$A6, #$A7, #$A8, #$A9, #$AA, #$AB, #$AC, #$AD, #$AE, #$AF,
    #$B0, #$B1, #$B2, #$B2, #$A5, #$B5, #$B6, #$B7, #$A8, #$B9, #$AA, #$BB, #$A3, #$BD, #$BD, #$AF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF,
    #$C0, #$C1, #$C2, #$C3, #$C4, #$C5, #$C6, #$C7, #$C8, #$C9, #$CA, #$CB, #$CC, #$CD, #$CE, #$CF,
    #$D0, #$D1, #$D2, #$D3, #$D4, #$D5, #$D6, #$D7, #$D8, #$D9, #$DA, #$DB, #$DC, #$DD, #$DE, #$DF);

(* make Delphi 5 compiler happy // andreas
  ToLowerChars: array[0..255] of Char =
  (#$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$09, #$0A, #$0B, #$0C, #$0D, #$0E, #$0F,
    #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1A, #$1B, #$1C, #$1D, #$1E, #$1F,
    #$20, #$21, #$22, #$23, #$24, #$25, #$26, #$27, #$28, #$29, #$2A, #$2B, #$2C, #$2D, #$2E, #$2F,
    #$30, #$31, #$32, #$33, #$34, #$35, #$36, #$37, #$38, #$39, #$3A, #$3B, #$3C, #$3D, #$3E, #$3F,
    #$40, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$5B, #$5C, #$5D, #$5E, #$5F,
    #$60, #$61, #$62, #$63, #$64, #$65, #$66, #$67, #$68, #$69, #$6A, #$6B, #$6C, #$6D, #$6E, #$6F,
    #$70, #$71, #$72, #$73, #$74, #$75, #$76, #$77, #$78, #$79, #$7A, #$7B, #$7C, #$7D, #$7E, #$7F,
    #$90, #$83, #$82, #$83, #$84, #$85, #$86, #$87, #$88, #$89, #$9A, #$8B, #$9C, #$9D, #$9E, #$9F,
    #$90, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$9A, #$9B, #$9C, #$9D, #$9E, #$9F,
    #$A0, #$A2, #$A2, #$BC, #$A4, #$B4, #$A6, #$A7, #$B8, #$A9, #$BA, #$AB, #$AC, #$AD, #$AE, #$BF,
    #$B0, #$B1, #$B3, #$B3, #$B4, #$B5, #$B6, #$B7, #$B8, #$B9, #$BA, #$BB, #$BC, #$BE, #$BE, #$BF,
    #$E0, #$E1, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF,
    #$E0, #$E1, #$E2, #$E3, #$E4, #$E5, #$E6, #$E7, #$E8, #$E9, #$EA, #$EB, #$EC, #$ED, #$EE, #$EF,
    #$F0, #$F1, #$F2, #$F3, #$F4, #$F5, #$F6, #$F7, #$F8, #$F9, #$FA, #$FB, #$FC, #$FD, #$FE, #$FF);
*)

procedure Savestring(AFile, AText: string);
begin
  with TFileStream.Create(AFile, fmCreate) do
  try
    WriteBuffer(AText[1], Length(AText));
  finally
    Free;
  end;
end;

function Loadstring(AFile: string): string;
var
  s: string;
begin
  with TFileStream.Create(AFile, fmOpenRead) do
  try
    SetLength(s, Size);
    ReadBuffer(s[1], Size);
  finally
    Free;
  end;
  Result := s;
end;

procedure DeleteValue(var AText: string; aName: string);
var
  p, p2, L: Integer;
begin
  L := Length(aName) + 2;
  p := PosText(aName + '="', AText);
  if p = 0 then
    Exit;
  p2 := PosStr('"', AText, p + L);
  if p2 = 0 then
    Exit;
  if p > 1 then
    Dec(p); // include the preceeding space if not the first one
  Delete(AText, p, p2 - p + 1);
end;

function GetValue(AText, aName: string): string;
var
  p, p2, L: Integer;
begin
  Result := '';
  L := Length(aName) + 2;
  p := PosText(aName + '="', AText);
  if p = 0 then
    Exit;
  p2 := PosStr('"', AText, p + L);
  if p2 = 0 then
    Exit;
  Result := Copy(AText, p + L, p2 - (p + L));
  Result := StringReplace(Result, '~~', Cr, [rfReplaceAll]);
end;

function HasStrValue(AText, aName: string; var AValue: string): Boolean;
var
  p, p2, L: Integer;
  s: string;
begin
  Result := False;
  L := Length(aName) + 2;
  p := PosText(aName + '="', AText);
  if p = 0 then
    Exit;
  p2 := PosStr('"', AText, p + L);
  if p2 = 0 then
    Exit;
  s := Copy(AText, p + L, p2 - (p + L));
  AValue := StringReplace(s, '~~', cr, [rfReplaceAll]);
end;

function GetStrValue(AText, aName, aDefault: string): string;
var
  s: string;
begin
  s := '';
  if hasStrValue(AText, aName, s) then
    Result := s
  else
    Result := aDefault;
end;

function GetIntValue(AText, aName: string; aDefault: Integer): Integer;
var
  s: string;
begin
  s := getValue(AText, aName);
  try
    Result := StrToInt(s);
  except
    Result := adefault;
  end;
end;

function GetFloatValue(AText, aName: string; aDefault: extended): extended;
var
  s: string;
begin
  s := '';
  if hasStrValue(AText, aName, s) then
  try
    Result := strtofloat(s);
  except
    Result := aDefault;
  end
  else
    Result := aDefault;
end;

function GetHTMLColorValue(AText, aName: string; aDefault: TColor): TColor;
var
  s: string;
begin
  s := '';
  if hasStrValue(AText, aName, s) then
  begin
    if Copy(s, 1, 1) = '#' then
    begin
      s := '$' + Copy(s, 6, 2) + Copy(s, 4, 2) + Copy(s, 2, 2);
    end
    else
      s := 'cl' + s;
    try
      Result := stringtocolor(s);
    except
      Result := aDefault;
    end;
  end
  else
    Result := aDefault;
end;

procedure SetValue(var AText: string; aName, AValue: string);
var
  p, p2, L: Integer;
begin
  l := Length(aName) + 2;
  if AText = '' then
  begin
    AText := aName + '="' + AValue + '"';
  end
  else
  begin
    p := PosText(aName + '="', AText);
    if p = 0 then
    begin
      AText := AText + ' ' + aName + '="' + AValue + '"';
    end
    else
    begin
      p2 := PosStr('"', AText, p + L);
      if p2 = 0 then
        Exit;
      Delete(AText, p + L, p2 - (p + L));
      insert(AValue, AText, p + L);
    end;
  end;
end;

function GetHTMLColor(aColor: TColor): string;
begin
  Result := Format('%6.6x', [colortorgb(acolor)]);
  Result := '="#' + Copy(Result, 5, 2) + Copy(Result, 3, 2) + Copy(Result, 1, 2) + '"';
end;

function BackPosStr(start: Integer; Findstring, Sourcestring: string): Integer;
var
  p, L: Integer;
begin
  Result := 0;
  L := Length(Findstring);
  if (L = 0) or (Sourcestring = '') or (start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    p := PosStr(Findstring, Sourcestring, Start);
    if p < Start then
    begin
      Result := p;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosText(start: Integer; Findstring, Sourcestring: string): Integer;
var
  p, L, from: Integer;
begin
  Result := 0;
  L := Length(Findstring);
  if (L = 0) or (Sourcestring = '') or (start < 2) then
    Exit;
  from := Start - L;
  if from < 1 then
    Exit;
  repeat
    p := PosText(Findstring, Sourcestring, from);
    if p < Start then
    begin
      Result := p;
      Exit;
    end;
    from := from - L;
  until from < 1;
end;

function PosRangeStr(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosStr(Headstring, Sourcestring, Start);
  if RangeBegin = 0 then
    Exit;
  RangeEnd := PosStr(Tailstring, Sourcestring, RangeBegin + Length(Headstring));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd + Length(Tailstring) - 1;
  Result := True;
end;

function PosRangeText(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosText(Headstring, Sourcestring, Start);
  if RangeBegin = 0 then
    Exit;
  RangeEnd := PosText(Tailstring, Sourcestring, RangeBegin + Length(Headstring));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd + Length(Tailstring) - 1;
  Result := True;
end;

function Innertag(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosText(Headstring, Sourcestring, Start);
  if RangeBegin = 0 then
    Exit;
  RangeBegin := RangeBegin + Length(Headstring);
  RangeEnd := PosText(Tailstring, Sourcestring, RangeBegin + Length(Headstring));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd - 1;
  Result := True;
end;

function PosTag(Start: Integer; Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := PosRangeStr(Start, '<', '>', Sourcestring, RangeBegin, RangeEnd);
end;

function BackPosRangeStr(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
var
  L: Integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case sensitive
  Result := False;
  L := Length(Headstring);
  if (L = 0) or (start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    if not PosRangeStr(Start, Headstring, Tailstring, Sourcestring, RangeBegin, RangeEnd) then
      Exit;
    if RangeBegin < Start then
    begin
      Result := True;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosRangeText(Start: Integer; Headstring, Tailstring, Sourcestring: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
var
  L: Integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case insensitive
  Result := False;
  L := Length(Headstring);
  if (L = 0) or (start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    if not PosRangeText(Start, Headstring, Tailstring, Sourcestring, RangeBegin, RangeEnd) then
      Exit;
    if RangeBegin < Start then
    begin
      Result := True;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function PosNonSpace(Start: Integer; SourceText: string): Integer;
var
  p, L: Integer;
begin
  Result := 0;
  L := Length(SourceText);
  p := Start;
  if L = 0 then
    Exit;
  while (p < L) and (SourceText[p] = ' ') do
    Inc(p);
  if SourceText[p] <> ' ' then
    Result := p;
end;

function BeginOfAttribute(Start: Integer; SourceText: string): Integer;
var
  p, L: Integer;
begin
  // parses the beginning of an attribute: space + alpha character
  Result := 0;
  L := Length(SourceText);
  if L = 0 then
    Exit;
  p := PosStr(' ', Sourcetext, start);
  if p = 0 then
    Exit;
  p := PosNonSpace(p, SourceText);
  if p = 0 then
    Exit;
  if (SourceText[p] in ['a'..'z', 'A'..'Z']) then
    Result := p;
end;

function ParseAttribute(var Start: Integer; SourceText: string; var aName: string; var AValue: string): Boolean;
var
  pn, pv, p: Integer;
begin
  // parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
  Result := False;
  pn := BeginOfAttribute(Start, SourceText);
  if pn = 0 then
    Exit;
  p := PosStr('="', SourceText, pn);
  if p = 0 then
    Exit;
  aName := trim(Copy(SourceText, pn, p - pn));
  pv := p + 2;
  p := PosStr('"', SourceText, pv);
  if p = 0 then
    Exit;
  AValue := Copy(SourceText, pv, p - pv);
  start := p + 1;
  Result := True;
end;

procedure ParseAttributes(SourceText: string; var Attributes: TStringList);
var
  aName, AValue: string;
  start: Integer;
begin
  Attributes.Clear;
  start := 1;
  while ParseAttribute(Start, SourceText, aName, AValue) do
    Attributes.Append(aName + '=' + AValue);
end;

function GetToken(var start: Integer; SourceText: string): string;
var
  p1, p2: Integer;
begin
  Result := '';
  if start > Length(sourceText) then
    Exit;
  p1 := posNonSpace(Start, SourceText);
  if p1 = 0 then
    Exit;
  if SourceText[p1] = '"' then
  begin // quoted token
    p2 := PosStr('"', SourceText, p1 + 1);
    if p2 = 0 then
      Exit;
    Result := Copy(SourceText, p1 + 1, p2 - p1 - 1);
    start := p2 + 1;
  end
  else
  begin
    p2 := PosStr(' ', SourceText, p1 + 1);
    if p2 = 0 then
      p2 := Length(sourcetext) + 1;
    Result := Copy(SourceText, p1, p2 - p1);
    start := p2;
  end;
end;

function Easter(nYear: Integer): TDateTime;
var
  nMonth, nDay, nMoon, nEpact, nSunday, nGold, nCent, nCorx, nCorz: Integer;
begin

  { The Golden Number of the year in the 19 year Metonic Cycle }
  nGold := ((nYear mod 19) + 1);

  { Calculate the Century }
  nCent := ((nYear div 100) + 1);

  { No. of Years in which leap year was dropped in order to keep in step
    with the sun }
  nCorx := ((3 * nCent) div 4 - 12);

  { Special Correction to Syncronize Easter with the moon's orbit }
  nCorz := ((8 * nCent + 5) div 25 - 5);

  { Find Sunday }
  nSunday := ((5 * nYear) div 4 - nCorx - 10);

  { Set Epact (specifies occurance of full moon }
  nEpact := ((11 * nGold + 20 + nCorz - nCorx) mod 30);

  if (nEpact < 0) then
    nEpact := nEpact + 30;

  if ((nEpact = 25) and (nGold > 11)) or (nEpact = 24) then
    nEpact := nEpact + 1;

  { Find Full Moon }
  nMoon := 44 - nEpact;

  if (nMoon < 21) then
    nMoon := nMoon + 30;

  { Advance to Sunday }
  nMoon := (nMoon + 7 - ((nSunday + nMoon) mod 7));

  if (nMoon > 31) then
  begin
    nMonth := 4;
    nDay := (nMoon - 31);
  end
  else
  begin
    nMonth := 3;
    nDay := nMoon;
  end;

  Result := EncodeDate(nYear, nMonth, nDay);

end;

//gets a datecode. Returns year and weeknumber in format: YYWW

function getWeekNumber(today: Tdatetime): string;

{dayOfWeek function returns Integer 1..7 equivalent to Sunday..Saturday.
ISO 8601 weeks start with Monday and the first week of a year is the one which
includes the first Thursday - Fiddle takes care of all this}

const
  Fiddle: array[1..7] of Byte = (6, 7, 8, 9, 10, 4, 5);

var
  present, startOfYear: Tdatetime;
  firstDayOfYear, weekNumber, numberOfDays: Integer;
  year, month, day: word;
  YearNumber: string;

begin
  present := trunc(today); //truncate to remove hours, mins and secs
  decodeDate(present, year, month, day); //decode to find year
  startOfYear := encodeDate(year, 1, 1); //encode 1st Jan of the year

  //find what day of week 1st Jan is, then add days according to rule
  firstDayOfYear := Fiddle[dayOfWeek(startOfYear)];

  //calc number of days since beginning of year + additional according to rule
  numberOfDays := trunc(present - startOfYear) + firstDayOfYear;

  //calc number of weeks
  weekNumber := trunc(numberOfDays / 7);

  //Format year, needed to prevent millenium bug and keep the Fluffy Spangle happy
  YearNumber := formatDateTime('yyyy', present);

  YearNumber := YearNumber + 'W';

  if weekNumber < 10 then
    YearNumber := YearNumber + '0'; //add leading zero for week

  //create datecode string
  Result := YearNumber + inttostr(weekNumber);

  if weekNumber = 0 then //recursive call for year begin/end...
    //see if previous year end was week 52 or 53
    Result := getWeekNumber(encodeDate(year - 1, 12, 31))

  else
  if weekNumber = 53 then
    //if 31st December less than Thursday then must be week 01 of next year
    if dayOfWeek(encodeDate(year, 12, 31)) < 5 then
    begin
      YearNumber := formatDateTime('yyyy', encodeDate(year + 1, 1, 1));
      Result := YearNumber + 'W01';
    end;

end;

function relativepath(aSrc, aDst: string): string;
var
  doc, sdoc, pardoc, img, simg, parimg, rel: string;
  pdoc, pimg: Integer;
begin
  doc := aSrc;
  img := aDst;
  repeat
    pdoc := Pos('\', doc);
    if pdoc > 0 then
    begin
      pardoc := Copy(doc, 1, pdoc);
      pardoc[Length(pardoc)] := '/';
      sdoc := sdoc + pardoc;
      Delete(doc, 1, pdoc);
    end;
    pimg := Pos('\', img);
    if pimg > 0 then
    begin
      parimg := Copy(img, 1, pimg);
      parimg[Length(parimg)] := '/';
      simg := simg + parimg;
      Delete(img, 1, pimg);
    end;
    if (pdoc > 0) and (pimg > 0) and (sdoc <> simg) then
      rel := '../' + rel + parimg;
    if (pdoc = 0) and (pimg <> 0) then
    begin
      rel := rel + parimg + img;
      if Pos(':', rel) > 0 then
        rel := '';
      Result := rel;
      Exit;
    end;
    if (pdoc > 0) and (pimg = 0) then
    begin
      rel := '../' + rel;
    end;
  until (pdoc = 0) and (pimg = 0);
  rel := rel + extractfilename(img);
  if Pos(':', rel) > 0 then
    rel := '';
  Result := rel;
end;

procedure GetHTMLAnchors(AFile: string; AList: TStringList);
var
  s, sa: string;
  p1, p2: Integer;
begin
  s := Loadstring(AFile);
  p1 := 1;
  repeat
    p1 := PosText('<a name="', s, p1);
    if p1 <> 0 then
    begin
      p2 := PosText('"', s, p1 + 9);
      if p2 <> 0 then
      begin
        sa := Copy(s, p1 + 9, p2 - p1 - 9);
        AList.Append(sa);
        p1 := p2;
      end
      else
        p1 := 0;
    end;
  until p1 = 0;
end;

function UppercaseHTMLTags(AText: string): string;
var
  p, p2: Integer;

begin
  Result := '';
  p2 := 1;
  repeat
    p := PosStr('<', AText, p2);
    if p > 0 then
    begin
      Result := Result + Copy(AText, p2, p - p2);
      p2 := p;
      if Copy(AText, p, 4) = '<!--' then
      begin
        p := PosStr('-->', AText, p);
        if p > 0 then
        begin
          Result := Result + Copy(AText, p2, p + 3 - p2);
          p2 := p + 3;
        end
        else
          Result := Result + Copy(AText, p2, Length(AText));
      end
      else
      begin
        p := PosStr('>', AText, p);
        if p > 0 then
        begin
          Result := Result + UpperCase(Copy(AText, p2, p - p2 + 1));
          p2 := p + 1;
        end
        else
          Result := Result + Copy(AText, p2, Length(AText));
      end;
    end
    else
    begin
      Result := Result + Copy(AText, p2, Length(AText));
    end;
  until p = 0;
end;

function LowercaseHTMLTags(AText: string): string;
var
  p, p2: Integer;

begin
  Result := '';
  p2 := 1;
  repeat
    p := PosStr('<', AText, p2);
    if p > 0 then
    begin
      Result := Result + Copy(AText, p2, p - p2);
      p2 := p;
      // now check for comments
      if Copy(AText, p, 4) = '<!--' then
      begin
        p := PosStr('-->', AText, p);
        if p > 0 then
        begin
          Result := Result + Copy(AText, p2, p + 3 - p2);
          p2 := p + 3;
        end
        else
          Result := Result + Copy(AText, p2, Length(AText));
      end
      else
      begin
        p := PosStr('>', AText, p);
        if p > 0 then
        begin
          Result := Result + LowerCase(Copy(AText, p2, p - p2 + 1));
          p2 := p + 1;
        end
        else
          Result := Result + Copy(AText, p2, Length(AText));
      end;
    end
    else
    begin
      Result := Result + Copy(AText, p2, Length(AText));
    end;
  until p = 0;
end;

function HexToColor(AText: string): TColor;
begin
  Result := clblack;
  if Length(AText) <> 7 then
    Exit;
  if AText[1] <> '#' then
    Exit;
  AText := '$' + Copy(AText, 6, 2) + Copy(AText, 4, 2) + Copy(AText, 2, 2);
  try
    Result := stringtocolor(AText);
  except
    Result := clblack;
  end;

end;

function PosEscaped(Start: Integer; SourceText, FindText: string; escapeChar: Char): Integer;
begin
  Result := PosText(FindText, SourceText, Start);
  if Result = 0 then
    Exit;
  if Result = 1 then
    Exit;
  if SourceText[Result - 1] <> escapeChar then
    Exit;
  repeat
    Result := PosText(FindText, SourceText, Result + 1);
    if Result = 0 then
      Exit;
  until SourceText[Result - 1] <> escapeChar;
end;

function DeleteEscaped(SourceText: string; escapeChar: Char): string;
var
  i: Integer;
begin
  i := 1;
  repeat
    if SourceText[i] = escapeChar then
      Delete(SourceText, i, 1);
    i := i + 1;
  until i > Length(SourceText);
  Result := SourceText;
end;

procedure RecurseDirFiles(myDir: string; var AFileList: TStringList);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faAnyFile or faDirectory;
  if FindFirst(myDir + PathDelim + AllFilePattern, FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
    begin
      if (sr.Attr and faDirectory) <> 0 then
      begin
        if (sr.name <> '.') and (sr.name <> '..') then
          RecurseDirFiles(myDir + PathDelim + sr.Name, AFileList);
      end
      else
        AFileList.Append(myDir + PathDelim + sr.Name);
    end;
  FindClose(sr);
end;

procedure RecurseDirProgs(myDir: string; var AFileList: TStringList);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  e: string;
  {$IFDEF LINUX}
  st: TStatBuf;
  {$ENDIF LINUX}
begin
  FileAttrs := faAnyFile or faDirectory;
  if FindFirst(myDir + PathDelim + AllFilePattern, FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
    begin
      if (sr.Attr and faDirectory) <> 0 then
      begin
        if (sr.name <> '.') and (sr.name <> '..') then
          RecurseDirProgs(myDir + PathDelim + sr.Name, AFileList);
      end
      {$IFDEF MSWINDOWS}
      else
      begin
        e := LowerCase(ExtractFileExt(sr.name));
        if e = '.exe' then
          AFileList.Append(myDir + PathDelim + sr.Name);
      end;
      {$ENDIF MSWINDOWS}
      {$IFDEF LINUX}
      else
      begin
        if stat(PChar(myDir + PathDelim + sr.Name), st) = 0 then
        begin
          if st.st_mode and (S_IXUSR or S_IXGRP or S_IXOTH) <> 0 then
            AFileList.Append(myDir + PathDelim + sr.Name);
        end;
      end;
      {$ENDIF LINUX}
    end;
  FindClose(sr);
end;

procedure LoadResourceFile(AFile: string; ms: TMemoryStream);
var
  HResInfo: HRSRC;
  HGlobal: THandle;
  Buffer, GoodType: PChar;
  Ext: string;
begin
  ext := UpperCase(ExtractFileExt(AFile));
  ext := Copy(ext, 2, Length(ext));
  if ext = 'HTM' then
    ext := 'HTML';
  Goodtype := PChar(ext);
  AFile := changefileext(afile, '');
  HResInfo := FindResource(HInstance, PChar(AFile), GoodType);
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal = 0 then
    raise EResNotFound.CreateResFmt(@RsECannotLoadResource, [AFile]);
  Buffer := LockResource(HGlobal);
  ms.Clear;
  ms.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
  ms.Seek(0, 0);
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
end;

procedure GetNames(AText: string; AList: TStringList);
var
  p: Integer;
  s: string;
begin
  AList.Clear;
  repeat
    AText := Trim(AText);
    p := Pos('="', AText);
    if p > 0 then
    begin
      s := Copy(AText, 1, p - 1);
      AList.Append(s);
      Delete(AText, 1, p + 1);
      p := Pos('"', AText);
      if p > 0 then
        Delete(AText, 1, p);
    end;
  until p = 0;
end;

function NameValuesToXML(AText: string): string;
var
  AList: TStringList;
  i, c: Integer;
  iname, ivalue, xml: string;
begin
  Result := '';
  if AText = '' then
    Exit;
  AList := TStringList.Create;
  GetNames(AText, AList);
  c := AList.Count;
  if c = 0 then
  begin
    AList.Free;
    Exit
  end;
  xml := '<accountdata>' + cr;
  for i := 0 to c - 1 do
  begin
    iname := AList[i];
    ivalue := GetValue(AText, iname);
    ivalue := StringReplace(ivalue, '~~', cr, [rfReplaceAll]);
    xml := xml + '<' + iname + '>' + cr;
    xml := xml + '  ' + ivalue + cr;
    xml := xml + '</' + iname + '>' + cr;
  end;
  xml := xml + '</accountdata>' + cr;
  AList.Free;
  Result := xml;
end;

function LastPosChar(const FindChar: Char; Sourcestring: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := Length(Sourcestring);
  if i = 0 then
    Exit;
  while (i > 0) and (Sourcestring[i] <> Findchar) do
    Dec(i);
  Result := i;
end;

function PosStr(const Findstring, Sourcestring: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        PUSH    EDX
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        SUB     EDX,ECX
        JNG     @@qt0
        MOV     EBX,EAX
        XCHG    EAX,EDX
        NOP
        ADD     EDI,ECX
        MOV     ECX,EAX
        MOV     AL,BYTE PTR [ESI]
@@lp1:  CMP     AL,BYTE PTR [EDI]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNZ     @@lp1
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOV     AL,BYTE PTR [ESI]
        MOV     EBX,EDX
        JMP     @@fr
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     AL,BYTE PTR [ESI+EBX]
        XOR     AL,BYTE PTR [EDI+EBX]
        JNE     @@ms
        DEC     EBX
        JNE     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        SUB     EAX,[ESP]
@@qt:   POP     ECX
        POP     EBX
        POP     EDI
        POP     ESI
end;

function PosText(const Findstring, Sourcestring: string; StartPos: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        NOP
        TEST    EAX,EAX
        JE      @@qt
        TEST    EDX,EDX
        JE      @@qt0
        MOV     ESI,EAX
        MOV     EDI,EDX
        PUSH    EDX
        MOV     EAX,[EAX-4]
        MOV     EDX,[EDX-4]
        DEC     EAX
        SUB     EDX,EAX
        DEC     ECX
        PUSH    EAX
        SUB     EDX,ECX
        JNG     @@qtx
        ADD     EDI,ECX
        MOV     ECX,EDX
        MOV     EDX,EAX
        MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
@@lp1:  MOVZX   EBX,BYTE PTR [EDI]
        CMP     AL,BYTE PTR [EBX+ToUpperChars]
        JE      @@uu
@@fr:   INC     EDI
        DEC     ECX
        JNE     @@lp1
@@qtx:  ADD     ESP,$08
@@qt0:  XOR     EAX,EAX
        JMP     @@qt
@@ms:   MOVZX   EBX,BYTE PTR [ESI]
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOV     EDX,[ESP]
        JMP     @@fr
        NOP
@@uu:   TEST    EDX,EDX
        JE      @@fd
@@lp2:  MOV     BL,BYTE PTR [ESI+EDX]
        MOV     AH,BYTE PTR [EDI+EDX]
        CMP     BL,AH
        JE      @@eq
        MOV     AL,BYTE PTR [EBX+ToUpperChars]
        MOVZX   EBX,AH
        XOR     AL,BYTE PTR [EBX+ToUpperChars]
        JNE     @@ms
@@eq:   DEC     EDX
        JNZ     @@lp2
@@fd:   LEA     EAX,[EDI+1]
        POP     ECX
        SUB     EAX,[ESP]
        POP     ECX
@@qt:   POP     EBX
        POP     EDI
        POP     ESI
end;

function GetBoolValue(AText, aName: string): Boolean;
begin
  Result := LowerCase(GetValue(AText, aName)) = 'yes';
end;

procedure ListSelect(src, dst: TStringList; AKey, AValue: string);
var
  i, c: Integer;
begin
  dst.Clear;
  c := src.Count;
  if c = 0 then
    Exit;
  for i := 0 to c - 1 do
  begin
    if GetValue(src[i], AKey) = AValue then
      dst.Append(src[i]);
  end;
end;

procedure ListFilter(src: TStringList; AKey, AValue: string);
var
  i, c: Integer;
  dst: TStringList;
begin
  c := src.Count;
  if c = 0 then
    Exit;
  dst := TStringList.Create;
  for i := 0 to c - 1 do
  begin
    if GetValue(src[i], AKey) = AValue then
      dst.Append(src[i]);
  end;
  src.Assign(dst);
  dst.Free;
end;

procedure ListOrderBy(src: TStringList; AKey: string; Numeric: Boolean);
var
  i, c, index: Integer;
  lit, dst: TStringList;
  s: string;
  ivalue: Integer;
begin
  c := src.Count;
  if c < 2 then
    Exit; // nothing to sort
  lit := TStringList.Create;
  dst := TStringList.Create;
  for i := 0 to c - 1 do
  begin
    s := GetValue(src[i], AKey);
    if Numeric then
    try
      ivalue := StrToInt(s);
      // format to 5 decimal places for correct string sorting
      // e.g. 5 becomes 00005
      s := Format('%5.5d', [ivalue]);
    except
      // just use the unformatted value
    end;
    lit.AddObject(s, TObject(i));
  end;
  lit.Sort;
  for i := 0 to c - 1 do
  begin
    index := Integer(lit.Objects[i]);
    dst.Append(src[index]);
  end;
  lit.Free;
  src.Assign(dst);
  dst.Free;
end;

// converts a csv list to a tagged string list

procedure csv2tags(src, dst: TStringList);
var
  i, c, fi, fc: Integer;
  Names: TStringList;
  rec: TStringList;
  s: string;
begin
  dst.Clear;
  c := src.Count;
  if c < 2 then
    Exit;
  Names := TStringList.Create;
  rec := TStringList.Create;
  try
    Names.CommaText := src[0];
    fc := Names.Count;
    if fc > 0 then
      for i := 1 to c - 1 do
      begin
        rec.CommaText := src[i];
        s := '';
        for fi := 0 to fc - 1 do
          s := s + Names[fi] + '="' + rec[fi] + '" ';
        dst.Append(s);
      end;
  finally
    rec.Free;
    Names.Free;
  end;
end;

// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure tags2csv(src, dst: TStringList);
var
  i, c, fi, fc: Integer;
  Names: TStringList;
  rec: TStringList;
  s: string;
begin
  dst.Clear;
  c := src.Count;
  if c < 1 then
    Exit;
  Names := TStringList.Create;
  rec := TStringList.Create;
  try
    GetNames(src[0], Names);
    fc := Names.Count;
    if fc > 0 then
    begin
      dst.Append(Names.CommaText);
      for i := 0 to c - 1 do
      begin
        s := '';
        rec.Clear;
        for fi := 0 to fc - 1 do
          rec.Append(GetValue(src[i], Names[fi]));
        dst.Append(rec.CommaText);
      end;
    end;
  finally
    rec.Free;
    Names.Free;
  end;
end;

function B64Encode;
var
  i: Integer;
  InBuf: array[0..2] of byte;
  OutBuf: array[0..3] of Char;
begin
  SetLength(Result, ((Length(S) + 2) div 3) * 4);
  for i := 1 to ((Length(S) + 2) div 3) do
  begin
    if Length(S) < (i * 3) then
      Move(S[(i - 1) * 3 + 1], InBuf, Length(S) - (i - 1) * 3)
    else
      Move(S[(i - 1) * 3 + 1], InBuf, 3);
    OutBuf[0] := B64Table[((InBuf[0] and $FC) shr 2) + 1];
    OutBuf[1] := B64Table[(((InBuf[0] and $03) shl 4) or ((InBuf[1] and $F0) shr 4)) + 1];
    OutBuf[2] := B64Table[(((InBuf[1] and $0F) shl 2) or ((InBuf[2] and $C0) shr 6)) + 1];
    OutBuf[3] := B64Table[(InBuf[2] and $3F) + 1];
    Move(OutBuf, Result[(i - 1) * 4 + 1], 4);
  end;
  if (Length(S) mod 3) = 1 then
  begin
    Result[Length(Result) - 1] := '=';
    Result[Length(Result)] := '=';
  end
  else
  if (Length(S) mod 3) = 2 then
    Result[Length(Result)] := '=';
end;

function B64Decode(const S: string): string;
var
  i: Integer;
  InBuf: array[0..3] of byte;
  OutBuf: array[0..2] of byte;
  RetValue: string;
begin
  if ((Length(S) mod 4) <> 0) or (S = '') then
    raise EJVCLException.CreateRes(@RsEIncorrectStringFormat);

  SetLength(RetValue, ((Length(S) div 4) - 1) * 3);
  for i := 1 to ((Length(S) div 4) - 1) do
  begin
    Move(S[(i - 1) * 4 + 1], InBuf, 4);
    if (InBuf[0] > 64) and (InBuf[0] < 91) then
      Dec(InBuf[0], 65)
    else
    if (InBuf[0] > 96) and (InBuf[0] < 123) then
      Dec(InBuf[0], 71)
    else
    if (InBuf[0] > 47) and (InBuf[0] < 58) then
      Inc(InBuf[0], 4)
    else
    if InBuf[0] = 43 then
      InBuf[0] := 62
    else
      InBuf[0] := 63;
    if (InBuf[1] > 64) and (InBuf[1] < 91) then
      Dec(InBuf[1], 65)
    else
    if (InBuf[1] > 96) and (InBuf[1] < 123) then
      Dec(InBuf[1], 71)
    else
    if (InBuf[1] > 47) and (InBuf[1] < 58) then
      Inc(InBuf[1], 4)
    else
    if InBuf[1] = 43 then
      InBuf[1] := 62
    else
      InBuf[1] := 63;
    if (InBuf[2] > 64) and (InBuf[2] < 91) then
      Dec(InBuf[2], 65)
    else
    if (InBuf[2] > 96) and (InBuf[2] < 123) then
      Dec(InBuf[2], 71)
    else
    if (InBuf[2] > 47) and (InBuf[2] < 58) then
      Inc(InBuf[2], 4)
    else
    if InBuf[2] = 43 then
      InBuf[2] := 62
    else
      InBuf[2] := 63;
    if (InBuf[3] > 64) and (InBuf[3] < 91) then
      Dec(InBuf[3], 65)
    else
    if (InBuf[3] > 96) and (InBuf[3] < 123) then
      Dec(InBuf[3], 71)
    else
    if (InBuf[3] > 47) and (InBuf[3] < 58) then
      Inc(InBuf[3], 4)
    else
    if InBuf[3] = 43 then
      InBuf[3] := 62
    else
      InBuf[3] := 63;
    OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
    OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2] := (InBuf[2] shl 6) or (InBuf[3] and $3F);
    Move(OutBuf, RetValue[(i - 1) * 3 + 1], 3);
  end;
  if S <> '' then
  begin
    Move(S[Length(S) - 3], InBuf, 4);
    if InBuf[2] = 61 then
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else
      if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else
      if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else
      if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else
      if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else
      if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else
      if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      RetValue := RetValue + Char(OutBuf[0]);
    end
    else
    if InBuf[3] = 61 then
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else
      if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else
      if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else
      if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else
      if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else
      if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else
      if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      if (InBuf[2] > 64) and (InBuf[2] < 91) then
        Dec(InBuf[2], 65)
      else
      if (InBuf[2] > 96) and (InBuf[2] < 123) then
        Dec(InBuf[2], 71)
      else
      if (InBuf[2] > 47) and (InBuf[2] < 58) then
        Inc(InBuf[2], 4)
      else
      if InBuf[2] = 43 then
        InBuf[2] := 62
      else
        InBuf[2] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      RetValue := RetValue + Char(OutBuf[0]) + Char(OutBuf[1]);
    end
    else
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else
      if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else
      if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else
      if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else
      if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else
      if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else
      if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      if (InBuf[2] > 64) and (InBuf[2] < 91) then
        Dec(InBuf[2], 65)
      else
      if (InBuf[2] > 96) and (InBuf[2] < 123) then
        Dec(InBuf[2], 71)
      else
      if (InBuf[2] > 47) and (InBuf[2] < 58) then
        Inc(InBuf[2], 4)
      else
      if InBuf[2] = 43 then
        InBuf[2] := 62
      else
        InBuf[2] := 63;
      if (InBuf[3] > 64) and (InBuf[3] < 91) then
        Dec(InBuf[3], 65)
      else
      if (InBuf[3] > 96) and (InBuf[3] < 123) then
        Dec(InBuf[3], 71)
      else
      if (InBuf[3] > 47) and (InBuf[3] < 58) then
        Inc(InBuf[3], 4)
      else
      if InBuf[3] = 43 then
        InBuf[3] := 62
      else
        InBuf[3] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      OutBuf[2] := (InBuf[2] shl 6) or (InBuf[3] and $3F);
      RetValue := RetValue + Char(OutBuf[0]) + Char(OutBuf[1]) + Char(OutBuf[2]);
    end;
  end;
  Result := RetValue;
end;

{*******************************************************
 * Standard Encryption algorithm - Copied from Borland *
 *******************************************************}

function Encrypt(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Instring) do
  begin
    Result := Result + CHAR(Byte(Instring[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}

function Decrypt(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Instring) do
  begin
    Result := Result + CHAR(Byte(Instring[I]) xor (StartKey shr 8));
    StartKey := (Byte(Instring[I]) + StartKey) * MultKey + AddKey;
  end;
end;

function EncryptB64(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := B64Encode(Encrypt(Instring, StartKey, MultKey, AddKey));
end;

function DecryptB64(const Instring: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := Decrypt(B64Decode(Instring), StartKey, MultKey, AddKey);
end;

function Hash(AText: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AText = '' then
    Exit;
  Result := Ord(AText[1]);
  for I := 2 to Length(AText) do
    Result := (Result * Ord(AText[i])) xor Result;
end;

{replace any <,> etc by &lt; &gt;}

function XMLSafe(AText: string): string;
var
  i, c: Integer;
begin
  c := Length(AText);
  if c = 0 then
  begin
    Result := AText;
    Exit;
  end;
  Result := '';
  for i := 1 to c do
  begin
    if AText[i] = '<' then
      Result := Result + '&lt;'
    else
    if AText[i] = '>' then
      Result := Result + '&gt;'
    else
    if AText[i] = '&' then
      Result := Result + '&amp;'
    else
    if (Ord(AText[i]) >= 32) and (Ord(AText[i]) < 128) then
      Result := Result + AText[i]
    else
    if Ord(AText[i]) > 127 then
      Result := Result + '&#' + inttostr(Ord(AText[i])) + ';'
    else
      Result := Result + ' ';
  end;
end;

function FirstOfSet(AText: string): string;
var
  p: Integer;
begin
  Result := Trim(AText);
  if Result = '' then
    Exit;
  if Result[1] = '"' then
  begin
    p := posStr('"', Result, 2);
    Result := Copy(Result, 2, p - 2);
  end
  else
  begin
    p := Pos(' ', Result);
    Result := Copy(Result, 1, p - 1);
  end;
end;

function LastOfSet(AText: string): string;
var
  c: Integer;
begin
  Result := Trim(AText);
  c := Length(Result);
  if c = 0 then
    Exit;
  if Result[c] = '"' then
  begin
    while (c > 1) and (Result[c - 1] <> '"') do
      Dec(c);
    Result := Copy(Result, c, Length(Result) - c);
  end
  else
  begin
    while (c > 1) and (Result[c - 1] <> ' ') do
      Dec(c);
    Result := Copy(Result, c, Length(Result));
  end;
end;

function CountOfSet(AText: string): Integer;
var
  lit: TStringList;
begin
  lit := TStringList.Create;
  splitset(AText, lit);
  Result := lit.Count;
  lit.Free;
end;

function SetRotateRight(AText: string): string;
var
  lit: TStringList;
  c: Integer;
begin
  lit := TStringList.Create;
  splitset(AText, lit);
  c := lit.Count;
  if c > 0 then
  begin
    lit.Move(c - 1, 0);
    Result := joinSet(lit);
  end
  else
    Result := '';
  lit.Free;
end;

function SetRotateLeft(AText: string): string;
var
  lit: TStringList;
  c: Integer;
begin
  lit := TStringList.Create;
  splitset(AText, lit);
  c := lit.Count;
  if c > 0 then
  begin
    lit.Move(0, c - 1);
    Result := joinSet(lit);
  end
  else
    Result := '';
  lit.Free;
end;

procedure SplitSet(AText: string; AList: TStringList);
var
  p: Integer;
begin
  AList.Clear;
  if AText = '' then
    Exit;
  AText := trim(AText);
  while AText <> '' do
  begin
    if AText[1] = '"' then
    begin
      Delete(AText, 1, 1);
      p := Pos('"', AText);
      if p <> 0 then
      begin
        AList.Append(Copy(AText, 1, p - 1));
        Delete(AText, 1, p);
      end;
    end
    else
    begin
      p := Pos(' ', AText);
      if p = 0 then
      begin
        AList.Append(AText);
        AText := '';
      end
      else
      begin
        AList.Append(Copy(AText, 1, p - 1));
        Delete(AText, 1, p);
      end;
    end;
    AText := trim(AText);
  end;

end;

function JoinSet(AList: TStringList): string;
var
  i, c: Integer;
begin
  Result := '';
  c := AList.Count;
  if c = 0 then
    Exit;
  for i := 0 to c - 1 do
    Result := Result + AList[i] + ' ';
  Delete(Result, Length(Result), 1);
end;

function SetPick(AText: string; aIndex: Integer): string;
var
  lit: TStringList;
  c: Integer;
begin
  lit := TStringList.Create;
  splitset(AText, lit);
  c := lit.Count;
  if (c > 0) and (aIndex < c) then
    Result := lit[aIndex]
  else
    Result := '';
  lit.Free;
end;

function SetSort(AText: string): string;
var
  lit: TStringList;
  c: Integer;
begin
  lit := TStringList.Create;
  splitset(AText, lit);
  c := lit.Count;
  if c > 0 then
  begin
    lit.Sort;
    Result := joinSet(lit);
  end
  else
    Result := '';
  lit.Free;
end;

function SetUnion(set1, set2: string): string;
var
  lit1, lit2, lit3: TStringList;
  i, c: Integer;
begin
  lit1 := TStringList.Create;
  lit2 := TStringList.Create;
  lit3 := TStringList.Create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.Count;
  if c <> 0 then
  begin
    lit2.Addstrings(lit1);
    for i := 0 to lit2.Count - 1 do
      if lit3.IndexOf(lit2[i]) = -1 then
        lit3.Append(lit2[i]);
    Result := JoinSet(lit3);
  end
  else
  begin
    Result := JoinSet(lit1);
  end;
  lit1.Free;
  lit2.Free;
  lit3.Free;
end;

function SetIntersect(set1, set2: string): string;
var
  lit1, lit2, lit3: TStringList;
  i, c: Integer;
begin
  lit1 := TStringList.Create;
  lit2 := TStringList.Create;
  lit3 := TStringList.Create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.Count;
  if c <> 0 then
  begin
    for i := 0 to c - 1 do
      if lit1.IndexOf(lit2[i]) <> -1 then
        lit3.Append(lit2[i]);
    Result := JoinSet(lit3);
  end
  else
  begin
    Result := '';
  end;
  lit1.Free;
  lit2.Free;
  lit3.Free;
end;

function SetExclude(set1, set2: string): string;
var
  lit1, lit2: TStringList;
  i, c, index: Integer;
begin
  lit1 := TStringList.Create;
  lit2 := TStringList.Create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.Count;
  if c <> 0 then
  begin
    for i := 0 to c - 1 do
    begin
      index := lit1.IndexOf(lit2[i]);
      if index <> -1 then
        lit1.Delete(index);
    end;
    Result := JoinSet(lit1);
  end
  else
  begin
    Result := JoinSet(lit1);
  end;
  lit1.Free;
  lit2.Free;
end;

// This function converts a string into a RFC 1630 compliant URL

function URLEncode(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
  begin
    if Pos(UpperCase(Value[I]), ValidURLChars) > 0 then
      Result := Result + Value[I]
    else
    begin
      if Value[I] = ' ' then
        Result := Result + '+'
      else
      begin
        Result := Result + '%';
        Result := Result + IntToHex(Byte(Value[I]), 2);
      end;
    end;
  end;
end;

function URLDecode(Value: string): string;
const
  HexChars = '0123456789ABCDEF';
var
  I: Integer;
  Ch, H1, H2: Char;
begin
  Result := '';
  I := 1;
  while I <= Length(Value) do
  begin
    Ch := Value[I];
    case Ch of
      '%':
        begin
          H1 := Value[I + 1];
          H2 := Value[I + 2];
          Inc(I, 2);
          Result := Result + Chr(((Pos(H1, HexChars) - 1) * 16) + (Pos(H2, HexChars) - 1));
        end;
      '+':
        Result := Result + ' ';
      '&':
        Result := Result + CrLf;
    else
      Result := Result + Ch;
    end;
    Inc(I);
  end;
end;

{template functions}

function ReplaceFirst(sourceStr, findStr, replaceStr: string): string;
var
  p: Integer;
begin
  Result := sourceStr;
  p := PosText(findstr, sourcestr, 1);
  if p = 0 then
    Exit;
  Result := Copy(sourcestr, 1, p - 1) + replacestr + Copy(sourceStr, p + Length(findStr), Length(sourceStr));
end;

function ReplaceLast(sourceStr, findStr, replaceStr: string): string;
var
  p: Integer;
begin
  Result := sourceStr;
  p := posTextLast(findstr, sourcestr);
  if p = 0 then
    Exit;
  Result := Copy(sourcestr, 1, p - 1) + replacestr + Copy(sourceStr, p + Length(findStr), Length(sourceStr));
end;

// insert a block template
// the last occurance of {block:aBlockname}
// the block template is marked with {begin:aBlockname} and {end:aBlockname}

function InsertLastBlock(var sourceStr: string; blockStr: string): Boolean;
var
  // phead:Integer;
  pblock, pe, pb: Integer;
  sbb, sbe, sb, sbr: string;
  sbbL, sbeL: Integer;
begin
  Result := False;
  //  phead:= PosStr('</head>',sourceStr,1);
  //  If phead = 0 Then Exit;
  //  phead:= phead + 7;
  sb := '{block:' + blockstr + '}';
  //  sbL:=Length(sb);
  sbb := '{begin:' + BlockStr + '}';
  sbbL := Length(sbb);
  sbe := '{end:' + BlockStr + '}';
  sbeL := Length(sbe);
  pblock := PosTextLast(sb, sourceStr);
  if pblock = 0 then
    Exit;
  pb := PosText(sbb, sourceStr, 1);
  if pb = 0 then
    Exit;
  pe := PosText(sbe, sourceStr, pb);
  if pe = 0 then
    Exit;
  pe := pe + sbeL - 1;
  // now replace
  sbr := Copy(SourceStr, pb + sbbL, pe - pb - sbbL - sbeL + 1);
  SourceStr := Copy(SourceStr, 1, pblock - 1) + sbr + Copy(SourceStr, pblock, Length(sourceStr));
  Result := True;
end;

// removes all  {begin:somefield} to {end:somefield} from aSource

function removeMasterBlocks(sourceStr: string): string;
var
  s, src: string;
  pb: Integer;
  pe: Integer;
  pee: Integer;
begin
  s := '';
  src := sourceStr;
  repeat
    pb := PosText('{begin:', src);
    if pb > 0 then
    begin
      pe := PosText('{end:', src, pb);
      if pe > 0 then
      begin
        pee := PosStr('}', src, pe);
        if pee > 0 then
        begin
          s := s + Copy(src, 1, pb - 1);
          Delete(src, 1, pee);
        end;
      end;
    end;
  until pb = 0;
  Result := s + src;
end;

// removes all {field} entries in a template

function removeFields(sourceStr: string): string;
var
  src, s: string;
  pb: Integer;
  pe: Integer;
begin
  s := '';
  src := sourceStr;
  repeat
    pb := Pos('{', src);
    if pb > 0 then
    begin
      pe := Pos('}', src);
      if pe > 0 then
      begin
        s := s + Copy(src, 1, pb - 1);
        Delete(src, 1, pe);
      end;
    end;
  until pb = 0;
  Result := s + src;
end;

{finds the last occurance}

function PosStrLast(const Findstring, Sourcestring: string): Integer;
var
  i, L: Integer;
begin
  Result := 0;
  L := Length(Findstring);
  if L = 0 then
    Exit;
  i := Length(Sourcestring);
  if i = 0 then
    Exit;
  i := i - L + 1;
  while i > 0 do
  begin
    Result := posStr(Findstring, Sourcestring, i);
    if Result > 0 then
      Exit;
    i := i - L;
  end;
end;

{finds the last occurance}

function PosTextLast(const Findstring, Sourcestring: string): Integer;
var
  i, L: Integer;
begin
  Result := 0;
  L := Length(Findstring);
  if L = 0 then
    Exit;
  i := Length(Sourcestring);
  if i = 0 then
    Exit;
  i := i - L + 1;
  while i > 0 do
  begin
    Result := PosText(Findstring, Sourcestring, i);
    if Result > 0 then
      Exit;
    i := i - L;
  end;
end;

procedure DirFiles(ADir, AMask: string; AFileList: TStringList);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faArchive + faDirectory;
  if FindFirst(ADir + AMask, FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
      if (sr.Attr and faArchive) <> 0 then
        AFileList.Append(ADir + sr.Name);
  FindClose(sr);
end;

// parse number returns the last position, starting from 1

function parseNumber(s: string): Integer;
var
  i, e, e2, c: Integer;
begin
  Result := 0;
  i := 0;
  c := Length(s);
  if c = 0 then
    Exit;
  while (i + 1 <= c) and (s[i + 1] in (DigitChars + [',', '.'])) do
    Inc(i);
  if (i + 1 <= c) and (s[i + 1] in ['e', 'E']) then
  begin
    e := i;
    Inc(i);
    if (i + 1 <= c) and (s[i + 1] in ['+', '-']) then
      Inc(i);
    e2 := i;
    while (i + 1 <= c) and (s[i + 1] in DigitChars) do
      Inc(i);
    if i = e2 then
      i := e;
  end;
  Result := i;
end;

// parse a SQL style data string from positions 1,
// starts and ends with #

function ParseDate(s: string): Integer;
var
  p: Integer;
begin
  Result := 0;
  if Length(s) < 2 then
    Exit;
  p := PosStr('#', s, 2);
  if p = 0 then
    Exit;
  try
    strtodate(Copy(s, 2, p - 2));
    Result := p;
  except
    Result := 0;
  end;
end;

end.
