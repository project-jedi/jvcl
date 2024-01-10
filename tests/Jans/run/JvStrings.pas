{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrings.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
  Should be merged with JCL
-----------------------------------------------------------------------------}
{$I JEDI.INC}
unit JvStrings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, dialogs;

{regular expressions}

{template functions}
function ReplaceFirst(sourceStr, findStr, replaceStr: string): string;
function ReplaceLast(sourceStr, findStr, replaceStr: string): string;
function InsertLastBlock(var sourceStr: string; blockStr: string): boolean;
function removeMasterBlocks(sourceStr: string): string;
function removeFields(sourceStr: string): string;

{http functions}
function URLEncode(Value: string): string; // Converts String To A URLEncoded String
function URLDecode(Value: string): string; // Converts String From A URLEncoded String

{set functions}
procedure SplitSet(aText: string; aList: TStringList);
function JoinSet(aList: TstringList): string;
function FirstOfSet(aText: string): string;
function LastOfSet(aText: string): string;
function CountOfSet(aText: string): integer;
function SetRotateRight(aText: string): string;
function SetRotateLeft(aText: string): string;
function SetPick(aText: string; aIndex: integer): string;
function SetSort(aText: string): string;
function SetUnion(set1, set2: string): string;
function SetIntersect(set1, set2: string): string;
function SetExclude(set1, set2: string): string;

{replace any <,> etc by &lt; &gt;}
function XMLSafe(aText: string): string;

{simple hash, result can be used in Encrypt}
function Hash(aText: string): integer;

{ Base64 encode and decode a string }
function B64Encode(const S: string): string;
function B64Decode(const S: string): string;

{Basic encryption from a Borland Example}
function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;

{Using Encrypt and Decrypt in combination with B64Encode and B64Decode}
function EncryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function DecryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;

procedure csv2tags(src, dst: TStringList);
// converts a csv list to a tagged string list

procedure tags2csv(src, dst: TStringList);
// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure ListSelect(src, dst: TStringList; aKey, aValue: string);
{selects akey=avalue from src and returns recordset in dst}

procedure ListFilter(src: TStringList; aKey, aValue: string);
{filters src for akey=avalue}

procedure ListOrderBy(src: TstringList; aKey: string; numeric: boolean);
{orders a tagged src list by akey}

function PosStr(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;
{ PosStr searches the first occurrence of a substring FindString in a string
  given by SourceString with case sensitivity (upper and lower case characters
  are differed). This function returns the index value of the first character
  of a specified substring from which it occurs in a given string starting with
  StartPos character index. If a specified substring is not found Q_PosStr
  returns zero. The author of algorithm is Peter Morris (UK) (FastStrings unit
  from www.torry.ru). }

function PosStrLast(const FindString, SourceString: string): integer;
{finds the last occurrence}

function PosText(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;
{ PosText searches the first occurrence of a substring FindString in a string
  given by SourceString without case sensitivity (upper and lower case
  characters are not differed). This function returns the index value of the
  first character of a specified substring from which it occurs in a given
  string starting with StartPos character index. If a specified substring is
  not found Q_PosStr returns zero. The author of algorithm is Peter Morris
  (UK) (FastStrings unit from www.torry.ru). }

function PosTextLast(const FindString, SourceString: string): integer;
{finds the last occurrence}

function NameValuesToXML(aText: string): string;
procedure LoadResourceFile(aFile: string; ms: TMemoryStream);
procedure DirFiles(aDir, amask: string; aFileList: TStringlist);
procedure RecurseDirFiles(myDir: string; var aFileList: TStringlist);
procedure RecurseDirProgs(myDir: string; var aFileList: TStringlist);
procedure SaveString(aFile, aText: string);
function LoadString(aFile: string): string;
function HexToColor(aText: string): Tcolor;
function UppercaseHTMLTags(aText: string): string;
function LowercaseHTMLTags(aText: string): string;
procedure GetHTMLAnchors(aFile: string; aList: TStringList);
function relativepath(aSrc, aDst: string): string;
function GetToken(var start: integer; SourceText: string): string;
function PosNonSpace(Start: integer; SourceText: string): integer;
function PosEscaped(Start: integer; SourceText, FindText: string; escapeChar: char): integer;
function DeleteEscaped(SourceText: string; escapeChar: char): string;
function BeginOfAttribute(Start: integer; SourceText: string): integer;
// parses the beginning of an attribute: space + alpha character
function ParseAttribute(var Start: integer; SourceText: string; var aName: string; var aValue: string): boolean;
// parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
procedure ParseAttributes(SourceText: string; var Attributes: TStringList);
// parses all name=value attributes to the attributes TStringlist
function HasStrValue(aText, aName: string; var aValue: string): boolean;
// checks if a name="value" pair exists and returns any value
function GetStrValue(aText, aName, aDefault: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1.verhoeven@wxs.nl"
// returns aDefault when not found
function GetHTMLColorValue(aText, aName: string; aDefault: Tcolor): TColor;
// same for a color
function GetIntValue(aText, aName: string; aDefault: Integer): integer;
// same for an integer
function GetFloatValue(aText, aName: string; aDefault: extended): extended;
// same for a float
function GetBoolValue(aText, aName: string): boolean;
// same for boolean but without default
function GetValue(aText, aName: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1.verhoeven@wxs.nl"
procedure SetValue(var aText: string; aName, aValue: string);
// sets a string value in a line
procedure DeleteValue(var aText: string; aName: string);
// deletes a aName="value" pair from aText

procedure GetNames(aText: string; aList: TStringList);
// get a list of names from a string with name="value" pairs
function GetHTMLColor(aColor: TColor): string;
// converts a color value to the HTML hex value
function BackPosStr(start: integer; FindString, SourceString: string): integer;
// finds a string backward case sensitive
function BackPosText(start: integer; FindString, SourceString: string): integer;
// finds a string backward case insensitive
function PosRangeStr(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds a text range, e.g. <TD>....</TD> case sensitive
function PosRangeText(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds a text range, e.g. <TD>....</td> case insensitive
function BackPosRangeStr(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds a text range backward, e.g. <TD>....</TD> case sensitive
function BackPosRangeText(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds a text range backward, e.g. <TD>....</td> case insensitive
function PosTag(Start: integer; SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds a HTML or XML tag:  <....>
function Innertag(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
// finds the innertext between opening and closing tags
function Easter(nYear: Integer): TDateTime;
// returns the easter date of a year.
function getWeekNumber(today: Tdatetime): string;
//gets a datecode. Returns year and weeknumber in format: YYWW
implementation

const
  cr = chr(13) + chr(10);
  tab = chr(9);

  B64Table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  ValidURLChars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$-_@.&+-!*"''(),;/#?:';

  ToUpperChars: array[0..255] of Char =
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

procedure SaveString(aFile, aText: string);
begin
  with TFileStream.Create(aFile, fmCreate) do
  try
    writeBuffer(aText[1], length(aText));
  finally free;
  end;
end;

function LoadString(aFile: string): string;
var
  s: string;
begin
  with TFileStream.Create(aFile, fmOpenRead) do
  try
    SetLength(s, Size);
    ReadBuffer(s[1], Size);
  finally free;
  end;
  result := s;
end;

procedure DeleteValue(var aText: string; aName: string);
var
  p, p2, L: integer;
begin
  L := length(aName) + 2;
  p := PosText(aName + '="', aText);
  if p = 0 then exit;
  p2 := PosStr('"', aText, p + L);
  if p2 = 0 then exit;
  if p > 1 then dec(p); // include the preceding space if not the first one
  delete(aText, p, p2 - p + 1);
end;

function GetValue(aText, aName: string): string;
var
  p, p2, L: integer;
begin
  result := '';
  L := length(aName) + 2;
  p := PosText(aName + '="', aText);
  if p = 0 then exit;
  p2 := PosStr('"', aText, p + L);
  if p2 = 0 then exit;
  result := copy(atext, p + L, p2 - (p + L));
  result := stringreplace(result, '~~', cr, [rfreplaceall]);
end;

function HasStrValue(aText, aName: string; var aValue: string): boolean;
var
  p, p2, L: integer;
  s: string;
begin
  result := false;
  L := length(aName) + 2;
  p := PosText(aName + '="', aText);
  if p = 0 then exit;
  p2 := PosStr('"', aText, p + L);
  if p2 = 0 then exit;
  s := copy(atext, p + L, p2 - (p + L));
  aValue := stringreplace(s, '~~', cr, [rfreplaceall]);
end;

function GetStrValue(aText, aName, aDefault: string): string;
var
  s: string;
begin
  s := '';
  if hasStrValue(aText, aName, s) then
    result := s
  else
    result := aDefault;
end;

function GetIntValue(aText, aName: string; aDefault: Integer): integer;
var
  s: string;
begin
  s := getValue(aText, aName);
  try
    result := strtoint(s);
  except
    result := adefault;
  end;
end;

function GetFloatValue(aText, aName: string; aDefault: extended): extended;
var
  s: string;
begin
  s := '';
  if hasStrValue(aText, aName, s) then
  try
    result := strtofloat(s);
  except
    result := aDefault;
  end
  else
    result := aDefault;
end;

function GetHTMLColorValue(aText, aName: string; aDefault: Tcolor): TColor;
var
  s: string;
begin
  s := '';
  if hasStrValue(aText, aName, s) then
  begin
    if copy(s, 1, 1) = '#' then
    begin
      s := '$' + copy(s, 6, 2) + copy(s, 4, 2) + copy(s, 2, 2);
    end
    else
      s := 'cl' + s;
    try
      result := stringtocolor(s);
    except
      result := aDefault;
    end;
  end
  else
    result := aDefault;
end;

procedure SetValue(var aText: string; aName, aValue: string);
var
  p, p2, L: integer;
begin
  l := length(aName) + 2;
  if aText = '' then
  begin
    aText := aName + '="' + aValue + '"';
  end
  else
  begin
    p := PosText(aName + '="', aText);
    if p = 0 then
    begin
      aText := aText + ' ' + aName + '="' + aValue + '"';
    end
    else
    begin
      p2 := PosStr('"', aText, p + L);
      if p2 = 0 then exit;
      Delete(aText, p + L, p2 - (p + L));
      insert(aValue, aText, p + L);
    end;
  end;
end;

function GetHTMLColor(aColor: TColor): string;
begin
  result := format('%6.6x', [colortorgb(acolor)]);
  result := '="#' + copy(result, 5, 2) + copy(result, 3, 2) + copy(result, 1, 2) + '"';
end;

function BackPosStr(start: integer; FindString, SourceString: string): integer;
var
  p, L: integer;
begin
  result := 0;
  L := length(FindString);
  if (L = 0) or (SourceString = '') or (start < 2) then exit;
  Start := Start - L;
  if Start < 1 then exit;
  repeat
    p := PosStr(FindString, SourceString, Start);
    if p < Start then
    begin
      result := p;
      exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosText(start: integer; FindString, SourceString: string): integer;
var
  p, L, from: integer;
begin
  result := 0;
  L := length(FindString);
  if (L = 0) or (SourceString = '') or (start < 2) then exit;
  from := Start - L;
  if from < 1 then exit;
  repeat
    p := PosText(FindString, SourceString, from);
    if p < Start then
    begin
      result := p;
      exit;
    end;
    from := from - L;
  until from < 1;
end;

function PosRangeStr(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
begin
  result := false;
  RangeBegin := PosStr(HeadString, SourceString, Start);
  if RangeBegin = 0 then exit;
  RangeEnd := PosStr(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then exit;
  RangeEnd := RangeEnd + length(TailString) - 1;
  result := true;
end;

function PosRangeText(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
begin
  result := false;
  RangeBegin := PosText(HeadString, SourceString, Start);
  if RangeBegin = 0 then exit;
  RangeEnd := PosText(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then exit;
  RangeEnd := RangeEnd + length(TailString) - 1;
  result := true;
end;

function Innertag(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
begin
  result := false;
  RangeBegin := PosText(HeadString, SourceString, Start);
  if RangeBegin = 0 then exit;
  RangeBegin := RangeBegin + length(HeadString);
  RangeEnd := PosText(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then exit;
  RangeEnd := RangeEnd - 1;
  result := true;
end;

function PosTag(Start: integer; SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
begin
  result := PosRangeStr(Start, '<', '>', SourceString, RangeBegin, RangeEnd);
end;

function BackPosRangeStr(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
var
  L: integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case sensitive
  result := false;
  L := length(HeadString);
  if (L = 0) or (start < 2) then exit;
  Start := Start - L;
  if Start < 1 then exit;
  repeat
    if not PosRangeStr(Start, HeadString, TailString, SourceString, RangeBegin, RangeEnd) then exit;
    if RangeBegin < Start then
    begin
      result := true;
      exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosRangeText(Start: integer; HeadString, TailString, SourceString: string; var RangeBegin: integer; var RangeEnd: integer): boolean;
var
  L: integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case insensitive
  result := false;
  L := length(HeadString);
  if (L = 0) or (start < 2) then exit;
  Start := Start - L;
  if Start < 1 then exit;
  repeat
    if not PosRangeText(Start, HeadString, TailString, SourceString, RangeBegin, RangeEnd) then exit;
    if RangeBegin < Start then
    begin
      result := true;
      exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function PosNonSpace(Start: integer; SourceText: string): integer;
var
  p, L: integer;
begin
  result := 0;
  L := length(SourceText);
  p := Start;
  if L = 0 then exit;
  while (p < L) and (SourceText[p] = ' ') do
    inc(p);
  if SourceText[p] <> ' ' then result := p;
end;

function BeginOfAttribute(Start: integer; SourceText: string): integer;
var
  p, L: integer;
begin
  // parses the beginning of an attribute: space + alpha character
  result := 0;
  L := length(SourceText);
  if L = 0 then exit;
  p := PosStr(' ', Sourcetext, start);
  if p = 0 then exit;
  p := PosNonSpace(p, SourceText);
  if p = 0 then exit;
  if (SourceText[p] in ['a'..'z', 'A'..'Z']) then
    result := p;
end;

function ParseAttribute(var Start: integer; SourceText: string; var aName: string; var aValue: string): boolean;
var
  pn, pv, p: integer;
begin
  // parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
  result := false;
  pn := BeginOfAttribute(Start, SourceText);
  if pn = 0 then exit;
  p := PosStr('="', SourceText, pn);
  if p = 0 then exit;
  aName := trim(copy(SourceText, pn, p - pn));
  pv := p + 2;
  p := PosStr('"', SourceText, pv);
  if p = 0 then exit;
  aValue := copy(SourceText, pv, p - pv);
  start := p + 1;
  result := true;
end;

procedure ParseAttributes(SourceText: string; var Attributes: TStringList);
var
  aName, aValue: string;
  start: integer;
begin
  Attributes.Clear;
  start := 1;
  while ParseAttribute(Start, SourceText, aName, aValue) do
    Attributes.Append(aName + '=' + aValue);
end;

function GetToken(var start: integer; SourceText: string): string;
var
  p1, p2: integer;
begin
  result := '';
  if start > length(sourceText) then exit;
  p1 := posNonSpace(Start, SourceText);
  if p1 = 0 then exit;
  if SourceText[p1] = '"' then
  begin // quoted token
    p2 := PosStr('"', SourceText, p1 + 1);
    if p2 = 0 then exit;
    result := copy(SourceText, p1 + 1, p2 - p1 - 1);
    start := p2 + 1;
  end
  else
  begin
    p2 := PosStr(' ', SourceText, p1 + 1);
    if p2 = 0 then p2 := length(sourcetext) + 1;
    result := copy(SourceText, p1, p2 - p1);
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

  { Special Correction to Synchronize Easter with the moon's orbit }
  nCorz := ((8 * nCent + 5) div 25 - 5);

  { Find Sunday }
  nSunday := ((5 * nYear) div 4 - nCorx - 10);

  { Set Epact (specifies occurrence of full moon }
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

{dayOfWeek function returns integer 1..7 equivalent to Sunday..Saturday.
ISO 8601 weeks start with Monday and the first week of a year is the one which
includes the first Thursday - Fiddle takes care of all this}

const
  Fiddle: array[1..7] of Byte = (6, 7, 8, 9, 10, 4, 5);

var
  present, startOfYear: Tdatetime;
  firstDayOfYear, weekNumber, numberOfDays: integer;
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

  //Format year, needed to prevent millennium bug and keep the Fluffy Spangle happy
  YearNumber := formatDateTime('yyyy', present);

  YearNumber := YearNumber + 'W';

  if weekNumber < 10 then
    YearNumber := YearNumber + '0'; //add leading zero for week

  //create datecode string
  result := YearNumber + inttostr(weekNumber);

  if weekNumber = 0 then //recursive call for year begin/end...
    //see if previous year end was week 52 or 53
    result := getWeekNumber(encodeDate(year - 1, 12, 31))

  else if weekNumber = 53 then
    //if 31st December less than Thursday then must be week 01 of next year
    if dayOfWeek(encodeDate(year, 12, 31)) < 5 then
    begin
      YearNumber := formatDateTime('yyyy', encodeDate(year + 1, 1, 1));
      result := YearNumber + 'W01';
    end;

end;

function relativepath(aSrc, aDst: string): string;
var
  doc, sdoc, pardoc, img, simg, parimg, rel: string;
  pdoc, pimg: integer;
begin
  doc := aSrc;
  img := aDst;
  repeat
    pdoc := pos('\', doc);
    if pdoc > 0 then
    begin
      pardoc := copy(doc, 1, pdoc);
      pardoc[length(pardoc)] := '/';
      sdoc := sdoc + pardoc;
      delete(doc, 1, pdoc);
    end;
    pimg := pos('\', img);
    if pimg > 0 then
    begin
      parimg := copy(img, 1, pimg);
      parimg[length(parimg)] := '/';
      simg := simg + parimg;
      delete(img, 1, pimg);
    end;
    if (pdoc > 0) and (pimg > 0) and (sdoc <> simg) then
      rel := '../' + rel + parimg;
    if (pdoc = 0) and (pimg <> 0) then
    begin
      rel := rel + parimg + img;
      if pos(':', rel) > 0 then rel := '';
      result := rel;
      exit;
    end;
    if (pdoc > 0) and (pimg = 0) then
    begin
      rel := '../' + rel;
    end;
  until (pdoc = 0) and (pimg = 0);
  rel := rel + extractfilename(img);
  if pos(':', rel) > 0 then rel := '';
  result := rel;
end;

procedure GetHTMLAnchors(aFile: string; aList: TStringList);
var
  s, sa: string;
  p1, p2: integer;
begin
  s := LoadString(aFile);
  p1 := 1;
  repeat
    p1 := PosText('<a name="', s, p1);
    if p1 <> 0 then
    begin
      p2 := PosText('"', s, p1 + 9);
      if p2 <> 0 then
      begin
        sa := copy(s, p1 + 9, p2 - p1 - 9);
        aList.Append(sa);
        p1 := p2;
      end
      else
        p1 := 0;
    end;
  until p1 = 0;
end;

function UppercaseHTMLTags(aText: string): string;
var
  p, p2: integer;

begin
  result := '';
  p2 := 1;
  repeat
    p := PosStr('<', AText, p2);
    if p > 0 then
    begin
      result := result + copy(AText, p2, p - p2);
      p2 := p;
      if copy(AText, p, 4) = '<!--' then
      begin
        p := PosStr('-->', AText, p);
        if p > 0 then
        begin
          result := result + copy(AText, p2, p + 3 - p2);
          p2 := p + 3;
        end
        else
          result := result + copy(AText, p2, length(AText));
      end
      else
      begin
        p := PosStr('>', AText, p);
        if p > 0 then
        begin
          result := result + uppercase(copy(AText, p2, p - p2 + 1));
          p2 := p + 1;
        end
        else
          result := result + copy(AText, p2, length(AText));
      end;
    end
    else
    begin
      result := result + copy(AText, p2, length(AText));
    end;
  until p = 0;
end;

function LowercaseHTMLTags(aText: string): string;
var
  p, p2: integer;

begin
  result := '';
  p2 := 1;
  repeat
    p := PosStr('<', AText, p2);
    if p > 0 then
    begin
      result := result + copy(AText, p2, p - p2);
      p2 := p;
      // now check for comments
      if copy(AText, p, 4) = '<!--' then
      begin
        p := PosStr('-->', AText, p);
        if p > 0 then
        begin
          result := result + copy(AText, p2, p + 3 - p2);
          p2 := p + 3;
        end
        else
          result := result + copy(AText, p2, length(AText));
      end
      else
      begin
        p := PosStr('>', AText, p);
        if p > 0 then
        begin
          result := result + lowercase(copy(AText, p2, p - p2 + 1));
          p2 := p + 1;
        end
        else
          result := result + copy(AText, p2, length(AText));
      end;
    end
    else
    begin
      result := result + copy(AText, p2, length(AText));
    end;
  until p = 0;
end;

function HexToColor(aText: string): Tcolor;
begin
  result := clblack;
  if length(aText) <> 7 then exit;
  if aText[1] <> '#' then exit;
  aText := '$' + copy(AText, 6, 2) + copy(AText, 4, 2) + copy(AText, 2, 2);
  try
    result := stringtocolor(aText);
  except
    result := clblack;
  end;

end;

function PosEscaped(Start: integer; SourceText, FindText: string; escapeChar: char): integer;
begin
  result := PosText(FindText, SourceText, Start);
  if result = 0 then exit;
  if result = 1 then exit;
  if SourceText[result - 1] <> escapeChar then exit;
  repeat
    result := PosText(FindText, SourceText, result + 1);
    if result = 0 then exit;
  until SourceText[result - 1] <> escapeChar;
end;

function DeleteEscaped(SourceText: string; escapeChar: char): string;
var
  i: integer;
begin
  i := 1;
  repeat
    if SourceText[i] = escapeChar then
      delete(SourceText, i, 1);
    i := i + 1;
  until i > length(SourceText);
  result := SourceText;
end;

procedure RecurseDirFiles(myDir: string; var aFileList: TStringlist);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faArchive + faDirectory;
  if FindFirst(myDir + '\*.*', FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
    begin
      if (sr.Attr and faDirectory) <> 0 then
      begin
        if (sr.name <> '.') and (sr.name <> '..') then
          RecurseDirFiles(myDir + '\' + sr.Name, aFileList);
      end
      else if (sr.Attr and faArchive) <> 0 then
      begin
        aFileList.append(myDir + '\' + sr.Name);
      end;
    end;
  FindClose(sr);
end;

procedure RecurseDirProgs(myDir: string; var aFileList: TStringlist);
var
  sr: TSearchRec;
  FileAttrs: Integer;
  e: string;
begin
  FileAttrs := faArchive + faDirectory;
  if FindFirst(myDir + '\*.*', FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
    begin
      if (sr.Attr and faDirectory) <> 0 then
      begin
        if (sr.name <> '.') and (sr.name <> '..') then
          RecurseDirProgs(myDir + '\' + sr.Name, aFileList);
      end
      else if (sr.Attr and faArchive) <> 0 then
      begin
        e := lowercase(extractfileext(sr.name));
        if e = '.exe' then
          aFileList.append(myDir + '\' + sr.Name);
      end;
    end;
  FindClose(sr);
end;

procedure LoadResourceFile(aFile: string; ms: TMemoryStream);
var
  HResInfo: HRSRC;
  HGlobal: THandle;
  Buffer, GoodType: pchar;
  Ext: string;
begin
  ext := uppercase(extractfileext(aFile));
  ext := copy(ext, 2, length(ext));
  if ext = 'HTM' then ext := 'HTML';
  Goodtype := pchar(ext);
  aFile := changefileext(afile, '');
  HResInfo := FindResource(HInstance, pchar(aFile), GoodType);
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal = 0 then
    raise EResNotFound.Create('Can''t load resource: ' + aFile);
  Buffer := LockResource(HGlobal);
  ms.clear;
  ms.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
  ms.Seek(0, 0);
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
end;

procedure GetNames(aText: string; aList: TStringList);
var
  p: integer;
  s: string;
begin
  alist.clear;
  repeat
    aText := Trim(aText);
    p := pos('="', aText);
    if p > 0 then
    begin
      s := copy(aText, 1, p - 1);
      alist.append(s);
      delete(aText, 1, p + 1);
      p := pos('"', atext);
      if p > 0 then
      begin
        delete(aText, 1, p);
      end;
    end;
  until p = 0;
end;

function NameValuesToXML(aText: string): string;
var
  alist: TStringlist;
  i, c: integer;
  iname, ivalue, xml: string;
begin
  result := '';
  if aText = '' then exit;
  aList := tstringlist.create;
  GetNames(aText, aList);
  c := alist.count;
  if c = 0 then
  begin
    alist.free;
    exit
  end;
  xml := '<accountdata>' + cr;
  for i := 0 to c - 1 do
  begin
    iname := alist[i];
    ivalue := getvalue(aText, iname);
    ivalue := stringreplace(ivalue, '~~', cr, [rfreplaceall]);
    xml := xml + '<' + iname + '>' + cr;
    xml := xml + '  ' + ivalue + cr;
    xml := xml + '</' + iname + '>' + cr;
  end;
  xml := xml + '</accountdata>' + cr;
  alist.free;
  result := xml;
end;

function PosStr(const FindString, SourceString: string; StartPos: Integer): Integer;
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

function PosText(const FindString, SourceString: string; StartPos: Integer): Integer;
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

function GetBoolValue(aText, aName: string): boolean;
begin
  result := lowercase(GetValue(aText, aName)) = 'yes';
end;

procedure ListSelect(src, dst: TStringList; aKey, aValue: string);
var
  i, c: integer;
begin
  dst.Clear;
  c := src.count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
  begin
    if getvalue(src[i], aKey) = aValue then
      dst.Append(src[i]);
  end;
end;

procedure ListFilter(src: TStringList; aKey, aValue: string);
var
  i, c: integer;
  dst: Tstringlist;
begin
  c := src.count;
  if c = 0 then exit;
  dst := TStringList.create;
  for i := 0 to c - 1 do
  begin
    if getvalue(src[i], aKey) = aValue then
      dst.Append(src[i]);
  end;
  src.Assign(dst);
  dst.free;
end;

procedure ListOrderBy(src: TstringList; aKey: string; numeric: boolean);
var
  i, c, index: integer;
  lit, dst: TStringlist;
  s: string;
  ivalue: integer;
begin
  c := src.count;
  if c < 2 then exit; // nothing to sort
  lit := TStringList.create;
  dst := TStringList.create;
  for i := 0 to c - 1 do
  begin
    s := getvalue(src[i], aKey);
    if numeric then
    try
      ivalue := strtoint(s);
      // format to 5 decimal places for correct string sorting
      // e.g. 5 becomes 00005
      s := format('%5.5d', [ivalue]);
    except
      // just use the unformatted value
    end;
    lit.AddObject(s, TObject(i));
  end;
  lit.Sort;
  for i := 0 to c - 1 do
  begin
    index := integer(lit.Objects[i]);
    dst.Append(src[index]);
  end;
  lit.free;
  src.Assign(dst);
  dst.free;
end;

// converts a csv list to a tagged string list

procedure csv2tags(src, dst: TStringList);
var
  i, c, fi, fc: integer;
  names: TstringList;
  rec: TstringList;
  s: string;
begin
  dst.clear;
  c := src.count;
  if c < 2 then exit;
  names := TStringList.create;
  rec := TStringList.create;
  try
    names.CommaText := src[0];
    fc := names.count;
    if fc > 0 then
      for i := 1 to c - 1 do
      begin
        rec.CommaText := src[i];
        s := '';
        for fi := 0 to fc - 1 do
          s := s + names[fi] + '="' + rec[fi] + '" ';
        dst.Append(s);
      end;
  finally
    rec.free;
    names.free;
  end;
end;

// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure tags2csv(src, dst: TStringList);
var
  i, c, fi, fc: integer;
  names: TstringList;
  rec: TstringList;
  s: string;
begin
  dst.clear;
  c := src.count;
  if c < 1 then exit;
  names := TStringList.create;
  rec := TStringList.Create;
  try
    GetNames(src[0], names);
    fc := names.count;
    if fc > 0 then
    begin
      dst.append(names.commatext);
      for i := 0 to c - 1 do
      begin
        s := '';
        rec.clear;
        for fi := 0 to fc - 1 do
          rec.append(getvalue(src[i], names[fi]));
        dst.Append(rec.commatext);
      end;
    end;
  finally
    rec.free;
    names.free;
  end;
end;

function B64Encode;
var
  i: integer;
  InBuf: array[0..2] of byte;
  OutBuf: array[0..3] of char;
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
  else if (Length(S) mod 3) = 2 then
    Result[Length(Result)] := '=';
end;

function B64Decode(const S: string): string;
var
  i: integer;
  InBuf: array[0..3] of byte;
  OutBuf: array[0..2] of byte;
begin
  if (Length(S) mod 4) <> 0 then
  begin
    raise Exception.Create('Base64: Incorrect string format');
  end;
  SetLength(Result, ((Length(S) div 4) - 1) * 3);
  for i := 1 to ((Length(S) div 4) - 1) do
  begin
    Move(S[(i - 1) * 4 + 1], InBuf, 4);
    if (InBuf[0] > 64) and (InBuf[0] < 91) then
      Dec(InBuf[0], 65)
    else if (InBuf[0] > 96) and (InBuf[0] < 123) then
      Dec(InBuf[0], 71)
    else if (InBuf[0] > 47) and (InBuf[0] < 58) then
      Inc(InBuf[0], 4)
    else if InBuf[0] = 43 then
      InBuf[0] := 62
    else
      InBuf[0] := 63;
    if (InBuf[1] > 64) and (InBuf[1] < 91) then
      Dec(InBuf[1], 65)
    else if (InBuf[1] > 96) and (InBuf[1] < 123) then
      Dec(InBuf[1], 71)
    else if (InBuf[1] > 47) and (InBuf[1] < 58) then
      Inc(InBuf[1], 4)
    else if InBuf[1] = 43 then
      InBuf[1] := 62
    else
      InBuf[1] := 63;
    if (InBuf[2] > 64) and (InBuf[2] < 91) then
      Dec(InBuf[2], 65)
    else if (InBuf[2] > 96) and (InBuf[2] < 123) then
      Dec(InBuf[2], 71)
    else if (InBuf[2] > 47) and (InBuf[2] < 58) then
      Inc(InBuf[2], 4)
    else if InBuf[2] = 43 then
      InBuf[2] := 62
    else
      InBuf[2] := 63;
    if (InBuf[3] > 64) and (InBuf[3] < 91) then
      Dec(InBuf[3], 65)
    else if (InBuf[3] > 96) and (InBuf[3] < 123) then
      Dec(InBuf[3], 71)
    else if (InBuf[3] > 47) and (InBuf[3] < 58) then
      Inc(InBuf[3], 4)
    else if InBuf[3] = 43 then
      InBuf[3] := 62
    else
      InBuf[3] := 63;
    OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
    OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
    OutBuf[2] := (InBuf[2] shl 6) or (InBuf[3] and $3F);
    Move(OutBuf, Result[(i - 1) * 3 + 1], 3);
  end;
  if Length(S) <> 0 then
  begin
    Move(S[Length(S) - 3], InBuf, 4);
    if InBuf[2] = 61 then
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      Result := Result + char(OutBuf[0]);
    end
    else if InBuf[3] = 61 then
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      if (InBuf[2] > 64) and (InBuf[2] < 91) then
        Dec(InBuf[2], 65)
      else if (InBuf[2] > 96) and (InBuf[2] < 123) then
        Dec(InBuf[2], 71)
      else if (InBuf[2] > 47) and (InBuf[2] < 58) then
        Inc(InBuf[2], 4)
      else if InBuf[2] = 43 then
        InBuf[2] := 62
      else
        InBuf[2] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      Result := Result + char(OutBuf[0]) + char(OutBuf[1]);
    end
    else
    begin
      if (InBuf[0] > 64) and (InBuf[0] < 91) then
        Dec(InBuf[0], 65)
      else if (InBuf[0] > 96) and (InBuf[0] < 123) then
        Dec(InBuf[0], 71)
      else if (InBuf[0] > 47) and (InBuf[0] < 58) then
        Inc(InBuf[0], 4)
      else if InBuf[0] = 43 then
        InBuf[0] := 62
      else
        InBuf[0] := 63;
      if (InBuf[1] > 64) and (InBuf[1] < 91) then
        Dec(InBuf[1], 65)
      else if (InBuf[1] > 96) and (InBuf[1] < 123) then
        Dec(InBuf[1], 71)
      else if (InBuf[1] > 47) and (InBuf[1] < 58) then
        Inc(InBuf[1], 4)
      else if InBuf[1] = 43 then
        InBuf[1] := 62
      else
        InBuf[1] := 63;
      if (InBuf[2] > 64) and (InBuf[2] < 91) then
        Dec(InBuf[2], 65)
      else if (InBuf[2] > 96) and (InBuf[2] < 123) then
        Dec(InBuf[2], 71)
      else if (InBuf[2] > 47) and (InBuf[2] < 58) then
        Inc(InBuf[2], 4)
      else if InBuf[2] = 43 then
        InBuf[2] := 62
      else
        InBuf[2] := 63;
      if (InBuf[3] > 64) and (InBuf[3] < 91) then
        Dec(InBuf[3], 65)
      else if (InBuf[3] > 96) and (InBuf[3] < 123) then
        Dec(InBuf[3], 71)
      else if (InBuf[3] > 47) and (InBuf[3] < 58) then
        Inc(InBuf[3], 4)
      else if InBuf[3] = 43 then
        InBuf[3] := 62
      else
        InBuf[3] := 63;
      OutBuf[0] := (InBuf[0] shl 2) or ((InBuf[1] shr 4) and $03);
      OutBuf[1] := (InBuf[1] shl 4) or ((InBuf[2] shr 2) and $0F);
      OutBuf[2] := (InBuf[2] shl 6) or (InBuf[3] and $3F);
      Result := Result + Char(OutBuf[0]) + Char(OutBuf[1]) + Char(OutBuf[2]);
    end;
  end;
end;

{*******************************************************
 * Standard Encryption algorithm - Copied from Borland *
 *******************************************************}

function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}

function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + CHAR(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(InString[I]) + StartKey) * MultKey + AddKey;
  end;
end;

function EncryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  result := B64Encode(Encrypt(InString, StartKey, MultKey, AddKey));
end;

function DecryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  result := Decrypt(B64Decode(Instring), StartKey, MultKey, AddKey);
end;

function Hash(aText: string): integer;
var
  i: integer;
begin
  result := 0;
  if aText = '' then exit;
  result := ord(aText[1]);
  for I := 2 to Length(aText) do
    result := (result * ord(aText[i])) xor result;
end;

{replace any <,> etc by &lt; &gt;}

function XMLSafe(aText: string): string;
var
  i, c: integer;
begin
  c := length(aText);
  if c = 0 then
  begin
    result := aText;
    exit;
  end;
  result := '';
  for i := 1 to c do
  begin
    if aText[i] = '<' then
      result := result + '&lt;'
    else if aText[i] = '>' then
      result := result + '&gt;'
    else if aText[i] = '&' then
      result := result + '&amp;'
    else if (ord(aText[i]) >= 32) and (ord(aText[i]) < 128) then
      result := result + aText[i]
    else if ord(aText[i]) > 127 then
      result := result + '&#' + inttostr(ord(aText[i])) + ';'
    else
      result := result + ' ';
  end;
end;

function FirstOfSet(aText: string): string;
var
  p: integer;
begin
  result := Trim(aText);
  if result = '' then exit;
  if result[1] = '"' then
  begin
    p := posStr('"', result, 2);
    result := copy(result, 2, p - 2);
  end
  else
  begin
    p := pos(' ', result);
    result := copy(result, 1, p - 1);
  end;
end;

function LastOfSet(aText: string): string;
var
  c: integer;
begin
  result := Trim(aText);
  c := length(result);
  if c = 0 then exit;
  if result[c] = '"' then
  begin
    while (c > 1) and (result[c - 1] <> '"') do
      dec(c);
    result := copy(result, c, length(result) - c);
  end
  else
  begin
    while (c > 1) and (result[c - 1] <> ' ') do
      dec(c);
    result := copy(result, c, length(result));
  end;
end;

function CountOfSet(aText: string): integer;
var
  lit: TStringlist;
begin
  lit := TstringList.create;
  splitset(aText, lit);
  result := lit.count;
  lit.free;
end;

function SetRotateRight(aText: string): string;
var
  lit: TStringlist;
  c: integer;
begin
  lit := TstringList.create;
  splitset(aText, lit);
  c := lit.count;
  if c > 0 then
  begin
    lit.Move(c - 1, 0);
    result := joinSet(lit);
  end
  else
    result := '';
  lit.free;
end;

function SetRotateLeft(aText: string): string;
var
  lit: TStringlist;
  c: integer;
begin
  lit := TstringList.create;
  splitset(aText, lit);
  c := lit.count;
  if c > 0 then
  begin
    lit.Move(0, c - 1);
    result := joinSet(lit);
  end
  else
    result := '';
  lit.free;
end;

procedure SplitSet(aText: string; aList: TStringList);
var
  p: integer;
begin
  aList.Clear;
  if aText = '' then exit;
  aText := trim(aText);
  while aText <> '' do
  begin
    if aText[1] = '"' then
    begin
      delete(aText, 1, 1);
      p := pos('"', aText);
      if p <> 0 then
      begin
        aList.append(copy(aText, 1, p - 1));
        delete(aText, 1, p);
      end;
    end
    else
    begin
      p := pos(' ', atext);
      if p = 0 then
      begin
        aList.Append(aText);
        atext := '';
      end
      else
      begin
        aList.append(copy(aText, 1, p - 1));
        delete(aText, 1, p);
      end;
    end;
    aText := trim(aText);
  end;

end;

function JoinSet(aList: TstringList): string;
var
  i, c: integer;
begin
  result := '';
  c := aList.count;
  if c = 0 then exit;
  for i := 0 to c - 1 do
    result := result + aList[i] + ' ';
  delete(result, length(result), 1);
end;

function SetPick(aText: string; aIndex: integer): string;
var
  lit: TStringlist;
  c: integer;
begin
  lit := TstringList.create;
  splitset(aText, lit);
  c := lit.count;
  if (c > 0) and (aIndex < c) then
    result := lit[aIndex]
  else
    result := '';
  lit.free;
end;

function SetSort(aText: string): string;
var
  lit: TStringlist;
  c: integer;
begin
  lit := TstringList.create;
  splitset(aText, lit);
  c := lit.count;
  if c > 0 then
  begin
    lit.Sort;
    result := joinSet(lit);
  end
  else
    result := '';
  lit.free;
end;

function SetUnion(set1, set2: string): string;
var
  lit1, lit2, lit3: Tstringlist;
  i, c: integer;
begin
  lit1 := tStringList.create;
  lit2 := tStringList.create;
  lit3 := tStringList.create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.count;
  if c <> 0 then
  begin
    lit2.AddStrings(lit1);
    for i := 0 to lit2.count - 1 do
      if lit3.IndexOf(lit2[i]) = -1 then
        lit3.Append(lit2[i]);
    result := JoinSet(lit3);
  end
  else
  begin
    result := JoinSet(lit1);
  end;
  lit1.free;
  lit2.free;
  lit3.free;
end;

function SetIntersect(set1, set2: string): string;
var
  lit1, lit2, lit3: Tstringlist;
  i, c: integer;
begin
  lit1 := tStringList.create;
  lit2 := tStringList.create;
  lit3 := tStringList.create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.count;
  if c <> 0 then
  begin
    for i := 0 to c - 1 do
      if lit1.IndexOf(lit2[i]) <> -1 then
        lit3.Append(lit2[i]);
    result := JoinSet(lit3);
  end
  else
  begin
    result := '';
  end;
  lit1.free;
  lit2.free;
  lit3.free;
end;

function SetExclude(set1, set2: string): string;
var
  lit1, lit2: Tstringlist;
  i, c, index: integer;
begin
  lit1 := tStringList.create;
  lit2 := tStringList.create;
  SplitSet(set1, lit1);
  SplitSet(set2, lit2);
  c := lit2.count;
  if c <> 0 then
  begin
    for i := 0 to c - 1 do
    begin
      index := lit1.IndexOf(lit2[i]);
      if index <> -1 then
        lit1.Delete(index);
    end;
    result := JoinSet(lit1);
  end
  else
  begin
    result := JoinSet(lit1);
  end;
  lit1.free;
  lit2.free;
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
      '+': Result := Result + ' ';
      '&': Result := Result + #13 + #10;
    else
      Result := Result + Ch;
    end;
    Inc(I);
  end;
end;

{template functions}

function ReplaceFirst(sourceStr, findStr, replaceStr: string): string;
var
  p: integer;
begin
  result := sourceStr;
  p := posText(findstr, sourcestr, 1);
  if p = 0 then exit;
  result := copy(sourcestr, 1, p - 1) + replacestr + copy(sourceStr, p + length(findStr), length(sourceStr));
end;

function ReplaceLast(sourceStr, findStr, replaceStr: string): string;
var
  p: integer;
begin
  result := sourceStr;
  p := posTextLast(findstr, sourcestr);
  if p = 0 then exit;
  result := copy(sourcestr, 1, p - 1) + replacestr + copy(sourceStr, p + length(findStr), length(sourceStr));
end;

// insert a block template
// the last occurrence of {block:aBlockname}
// the block template is marked with {begin:aBlockname} and {end:aBlockname}

function InsertLastBlock(var sourceStr: string; blockStr: string): boolean;
var
  // phead:integer;
  pblock, pe, pb: integer;
  sbb, sbe, sb, sbr: string;
  sbbL, sbeL: integer;
begin
  result := false;
  //  phead:= posstr('</head>',sourceStr,1);
  //  If phead = 0 Then Exit;
  //  phead:= phead + 7;
  sb := '{block:' + blockstr + '}';
  //  sbL:=length(sb);
  sbb := '{begin:' + BlockStr + '}';
  sbbL := Length(sbb);
  sbe := '{end:' + BlockStr + '}';
  sbeL := Length(sbe);
  pblock := posTextlast(sb, sourceStr);
  if pblock = 0 then Exit;
  pb := posText(sbb, sourceStr, 1);
  if pb = 0 then Exit;
  pe := postext(sbe, sourceStr, pb);
  if pe = 0 then Exit;
  pe := pe + sbeL - 1;
  // now replace
  sbr := copy(SourceStr, pb + sbbL, pe - pb - sbbL - sbeL + 1);
  SourceStr := copy(SourceStr, 1, pblock - 1) + sbr + copy(SourceStr, pblock, length(sourceStr));
  result := true;
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
    pb := postext('{begin:', src);
    if pb > 0 then
    begin
      pe := postext('{end:', src, pb);
      if pe > 0 then
      begin
        pee := posstr('}', src, pe);
        if pee > 0 then
        begin
          s := s + copy(src, 1, pb - 1);
          delete(src, 1, pee);
        end;
      end;
    end;
  until pb = 0;
  result := s + src;
end;

// removes all {field} entries in a template

function removeFields(sourceStr: string): string;
var
  src, s: string;
  pb: integer;
  pe: integer;
begin
  s := '';
  src := sourceStr;
  repeat
    pb := pos('{', src);
    if pb > 0 then
    begin
      pe := pos('}', src);
      if pe > 0 then
      begin
        s := s + copy(src, 1, pb - 1);
        delete(src, 1, pe);
      end;
    end;
  until pb = 0;
  result := s + src;
end;

{finds the last occurrence}

function PosStrLast(const FindString, SourceString: string): integer;
var
  i, L: integer;
begin
  result := 0;
  L := length(FindString);
  if L = 0 then exit;
  i := length(SourceString);
  if i = 0 then exit;
  i := i - L + 1;
  while i > 0 do
  begin
    result := posStr(FindString, SourceString, i);
    if result > 0 then exit;
    i := i - L;
  end;
end;

{finds the last occurrence}

function PosTextLast(const FindString, SourceString: string): integer;
var
  i, L: integer;
begin
  result := 0;
  L := length(FindString);
  if L = 0 then exit;
  i := length(SourceString);
  if i = 0 then exit;
  i := i - L + 1;
  while i > 0 do
  begin
    result := posText(FindString, SourceString, i);
    if result > 0 then exit;
    i := i - L;
  end;
end;

procedure DirFiles(aDir, amask: string; aFileList: TStringlist);
var
  sr: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faArchive + faDirectory;
  if FindFirst(aDir + amask, FileAttrs, sr) = 0 then
    while FindNext(sr) = 0 do
      if (sr.Attr and faArchive) <> 0 then
        aFileList.append(aDir + sr.Name);
  FindClose(sr);
end;

end.
