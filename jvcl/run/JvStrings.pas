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

unit JvStrings;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  Graphics,
  SysUtils, Classes;

{regular expressions}

{template functions}
function ReplaceFirst(const SourceStr, FindStr, ReplaceStr: string): string;
function ReplaceLast(const SourceStr, FindStr, ReplaceStr: string): string;
function InsertLastBlock(var SourceStr: string; BlockStr: string): Boolean;
function RemoveMasterBlocks(const SourceStr: string): string;
function RemoveFields(const SourceStr: string): string;

{http functions}
function URLEncode(const Value: string): string; // Converts string To A URLEncoded string
function URLDecode(const Value: string): string; // Converts string From A URLEncoded string

{set functions}
procedure SplitSet(AText: string; AList: TStringList);
function JoinSet(AList: TStringList): string;
function FirstOfSet(const AText: string): string;
function LastOfSet(const AText: string): string;
function CountOfSet(const AText: string): Integer;
function SetRotateRight(const AText: string): string;
function SetRotateLeft(const AText: string): string;
function SetPick(const AText: string; AIndex: Integer): string;
function SetSort(const AText: string): string;
function SetUnion(const Set1, Set2: string): string;
function SetIntersect(const Set1, Set2: string): string;
function SetExclude(const Set1, Set2: string): string;

{replace any <,> etc by &lt; &gt;}
function XMLSafe(const AText: string): string;

{simple hash, Result can be used in Encrypt}
function Hash(const AText: string): Integer;

{ Base64 encode and decode a string }
function B64Encode(const S: string): string;
function B64Decode(const S: string): string;

{Basic encryption from a Borland Example}
function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;

{Using Encrypt and Decrypt in combination with B64Encode and B64Decode}
function EncryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
function DecryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;

procedure CSVToTags(Src, Dst: TStringList);
// converts a csv list to a tagged string list

procedure TagsToCSV(Src, Dst: TStringList);
// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure ListSelect(Src, Dst: TStringList; const AKey, AValue: string);
{selects akey=avalue from Src and returns recordset in Dst}

procedure ListFilter(Src: TStringList; const AKey, AValue: string);
{filters Src for akey=avalue}

procedure ListOrderBy(Src: TStringList; const AKey: string; Numeric: Boolean);
{orders a tagged Src list by akey}

function PosStr(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;
{ PosStr searches the first occurrence of a substring FindString in a string
  given by SourceString with case sensitivity (upper and lower case characters
  are differed). This function returns the index value of the first character
  of a specified substring from which it occurs in a given string starting with
  StartPos character index. If a specified substring is not found Q_PosStr
  returns zero. The author of algorithm is Peter Morris (UK) (Faststrings unit
  from www.torry.ru). }

function PosStrLast(const FindString, SourceString: string): Integer;
{finds the last occurance}

function LastPosChar(const FindChar: Char; SourceString: string): Integer;

function PosText(const FindString, SourceString: string;
  StartPos: Integer = 1): Integer;
{ PosText searches the first occurrence of a substring FindString in a string
  given by SourceString without case sensitivity (upper and lower case
  characters are not differed). This function returns the index value of the
  first character of a specified substring from which it occurs in a given
  string starting with StartPos character index. If a specified substring is
  not found Q_PosStr returns zero. The author of algorithm is Peter Morris
  (UK) (Faststrings unit from www.torry.ru). }

function PosTextLast(const FindString, SourceString: string): Integer;
{finds the last occurance}

function NameValuesToXML(const AText: string): string;
{$IFDEF MSWINDOWS}
procedure LoadResourceFile(AFile: string; MemStream: TMemoryStream);
{$ENDIF MSWINDOWS}
procedure DirFiles(const ADir, AMask: string; AFileList: TStringList);
procedure RecurseDirFiles(const ADir: string; var AFileList: TStringList);
procedure RecurseDirProgs(const ADir: string; var AFileList: TStringList);
procedure SaveString(const AFile, AText: string);
function LoadString(const AFile: string): string;
function HexToColor(const AText: string): TColor;
function UppercaseHTMLTags(const AText: string): string;
function LowercaseHTMLTags(const AText: string): string;
procedure GetHTMLAnchors(const AFile: string; AList: TStringList);
function RelativePath(const ASrc, ADst: string): string;
function GetToken(var Start: Integer; const SourceText: string): string;
function PosNonSpace(Start: Integer; const SourceText: string): Integer;
function PosEscaped(Start: Integer; const SourceText, FindText: string; EscapeChar: Char): Integer;
function DeleteEscaped(const SourceText: string; EscapeChar: Char): string;
function BeginOfAttribute(Start: Integer; const SourceText: string): Integer;
// parses the beginning of an attribute: space + alpha character
function ParseAttribute(var Start: Integer; const SourceText: string; var AName, AValue: string): Boolean;
// parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
procedure ParseAttributes(const SourceText: string; Attributes: TStrings);
// parses all name=value attributes to the attributes TStringList
function HasStrValue(const AText, AName: string; var AValue: string): Boolean;
// checks if a name="value" pair exists and returns any value
function GetStrValue(const AText, AName, ADefault: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1 dott verhoeven att wxs dott nl"
// returns ADefault when not found
function GetHTMLColorValue(const AText, AName: string; ADefault: TColor): TColor;
// same for a color
function GetIntValue(const AText, AName: string; ADefault: Integer): Integer;
// same for an Integer
function GetFloatValue(const AText, AName: string; ADefault: Extended): Extended;
// same for a float
function GetBoolValue(const AText, AName: string): Boolean;
// same for Boolean but without default
function GetValue(const AText, AName: string): string;
// retrieves string value from a line like:
//  name="jan verhoeven" email="jan1 dott verhoeven att wxs dott nl"
procedure SetValue(var AText: string; const AName, AValue: string);
// sets a string value in a line
procedure DeleteValue(var AText: string; const AName: string);
// deletes a AName="value" pair from AText

procedure GetNames(AText: string; AList: TStringList);
// get a list of names from a string with name="value" pairs
function GetHTMLColor(AColor: TColor): string;
// converts a color value to the HTML hex value
function BackPosStr(Start: Integer; const FindString, SourceString: string): Integer;
// finds a string backward case sensitive
function BackPosText(Start: Integer; const FindString, SourceString: string): Integer;
// finds a string backward case insensitive
function PosRangeStr(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range, e.g. <TD>....</TD> case sensitive
function PosRangeText(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range, e.g. <TD>....</td> case insensitive
function BackPosRangeStr(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range backward, e.g. <TD>....</TD> case sensitive
function BackPosRangeText(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds a text range backward, e.g. <TD>....</td> case insensitive
function PosTag(Start: Integer; SourceString: string; var RangeBegin: Integer;
  var RangeEnd: Integer): Boolean;
// finds a HTML or XML tag:  <....>
function InnerTag(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
// finds the innertext between opening and closing tags
function Easter(NYear: Integer): TDateTime;
// returns the easter date of a year.
function GetWeekNumber(Today: TDateTime): string;
//gets a datecode. Returns year and weeknumber in format: YYWW

function ParseNumber(const S: string): Integer;
// parse number returns the last position, starting from 1
function ParseDate(const S: string): Integer;
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

procedure SaveString(const AFile, AText: string);
begin
  with TFileStream.Create(AFile, fmCreate) do
  try
    WriteBuffer(AText[1], Length(AText));
  finally
    Free;
  end;
end;

function LoadString(const AFile: string): string;
var
  S: string;
begin
  with TFileStream.Create(AFile, fmOpenRead) do
  try
    SetLength(S, Size);
    ReadBuffer(S[1], Size);
  finally
    Free;
  end;
  Result := S;
end;

procedure DeleteValue(var AText: string; const AName: string);
var
  P, P2, L: Integer;
begin
  L := Length(AName) + 2;
  P := PosText(AName + '="', AText);
  if P = 0 then
    Exit;
  P2 := PosStr('"', AText, P + L);
  if P2 = 0 then
    Exit;
  if P > 1 then
    Dec(P); // include the preceding space if not the first one
  Delete(AText, P, P2 - P + 1);
end;

function GetValue(const AText, AName: string): string;
var
  P, P2, L: Integer;
begin
  Result := '';
  L := Length(AName) + 2;
  P := PosText(AName + '="', AText);
  if P = 0 then
    Exit;
  P2 := PosStr('"', AText, P + L);
  if P2 = 0 then
    Exit;
  Result := Copy(AText, P + L, P2 - (P + L));
  Result := StringReplace(Result, '~~', Cr, [rfReplaceAll]);
end;

function HasStrValue(const AText, AName: string; var AValue: string): Boolean;
var
  P, P2, L: Integer;
  S: string;
begin
  Result := False;
  L := Length(AName) + 2;
  P := PosText(AName + '="', AText);
  if P = 0 then
    Exit;
  P2 := PosStr('"', AText, P + L);
  if P2 = 0 then
    Exit;
  S := Copy(AText, P + L, P2 - (P + L));
  AValue := StringReplace(S, '~~', Cr, [rfReplaceAll]);
end;

function GetStrValue(const AText, AName, ADefault: string): string;
var
  S: string;
begin
  S := '';
  if HasStrValue(AText, AName, S) then
    Result := S
  else
    Result := ADefault;
end;

function GetIntValue(const AText, AName: string; ADefault: Integer): Integer;
var
  S: string;
begin
  S := GetValue(AText, AName);
  try
    Result := StrToInt(S);
  except
    Result := ADefault;
  end;
end;

function GetFloatValue(const AText, AName: string; ADefault: Extended): Extended;
var
  S: string;
begin
  S := '';
  if HasStrValue(AText, AName, S) then
  try
    Result := StrToFloat(S);
  except
    Result := ADefault;
  end
  else
    Result := ADefault;
end;

function GetHTMLColorValue(const AText, AName: string; ADefault: TColor): TColor;
var
  S: string;
begin
  S := '';
  if HasStrValue(AText, AName, S) then
  begin
    if Copy(S, 1, 1) = '#' then
      S := '$' + Copy(S, 6, 2) + Copy(S, 4, 2) + Copy(S, 2, 2)
    else
      S := 'cl' + S;
    try
      Result := StringToColor(S);
    except
      Result := ADefault;
    end;
  end
  else
    Result := ADefault;
end;

procedure SetValue(var AText: string; const AName, AValue: string);
var
  P, P2, L: Integer;
begin
  L := Length(AName) + 2;
  if AText = '' then
    AText := AName + '="' + AValue + '"'
  else
  begin
    P := PosText(AName + '="', AText);
    if P = 0 then
      AText := AText + ' ' + AName + '="' + AValue + '"'
    else
    begin
      P2 := PosStr('"', AText, P + L);
      if P2 = 0 then
        Exit;
      Delete(AText, P + L, P2 - (P + L));
      Insert(AValue, AText, P + L);
    end;
  end;
end;

function GetHTMLColor(AColor: TColor): string;
begin
  Result := Format('%6.6x', [ColorToRGB(AColor)]);
  Result := '="#' + Copy(Result, 5, 2) + Copy(Result, 3, 2) + Copy(Result, 1, 2) + '"';
end;

function BackPosStr(Start: Integer; const FindString, SourceString: string): Integer;
var
  P, L: Integer;
begin
  Result := 0;
  L := Length(FindString);
  if (L = 0) or (SourceString = '') or (Start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    P := PosStr(FindString, SourceString, Start);
    if P < Start then
    begin
      Result := P;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosText(Start: Integer; const FindString, SourceString: string): Integer;
var
  P, L, From: Integer;
begin
  Result := 0;
  L := Length(FindString);
  if (L = 0) or (SourceString = '') or (Start < 2) then
    Exit;
  From := Start - L;
  if From < 1 then
    Exit;
  repeat
    P := PosText(FindString, SourceString, From);
    if P < Start then
    begin
      Result := P;
      Exit;
    end;
    From := From - L;
  until From < 1;
end;

function PosRangeStr(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosStr(HeadString, SourceString, Start);
  if RangeBegin = 0 then
    Exit;
  RangeEnd := PosStr(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd + Length(TailString) - 1;
  Result := True;
end;

function PosRangeText(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosText(HeadString, SourceString, Start);
  if RangeBegin = 0 then
    Exit;
  RangeEnd := PosText(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd + Length(TailString) - 1;
  Result := True;
end;

function InnerTag(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := False;
  RangeBegin := PosText(HeadString, SourceString, Start);
  if RangeBegin = 0 then
    Exit;
  RangeBegin := RangeBegin + Length(HeadString);
  RangeEnd := PosText(TailString, SourceString, RangeBegin + Length(HeadString));
  if RangeEnd = 0 then
    Exit;
  RangeEnd := RangeEnd - 1;
  Result := True;
end;

function PosTag(Start: Integer; SourceString: string; var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
begin
  Result := PosRangeStr(Start, '<', '>', SourceString, RangeBegin, RangeEnd);
end;

function BackPosRangeStr(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
var
  L: Integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case sensitive
  Result := False;
  L := Length(HeadString);
  if (L = 0) or (Start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    if not PosRangeStr(Start, HeadString, TailString, SourceString, RangeBegin, RangeEnd) then
      Exit;
    if RangeBegin < Start then
    begin
      Result := True;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function BackPosRangeText(Start: Integer; const HeadString, TailString, SourceString: string;
  var RangeBegin: Integer; var RangeEnd: Integer): Boolean;
var
  L: Integer;
begin
  // finds a text range backward, e.g. <TD>....</TD> case insensitive
  Result := False;
  L := Length(HeadString);
  if (L = 0) or (Start < 2) then
    Exit;
  Start := Start - L;
  if Start < 1 then
    Exit;
  repeat
    if not PosRangeText(Start, HeadString, TailString, SourceString, RangeBegin, RangeEnd) then
      Exit;
    if RangeBegin < Start then
    begin
      Result := True;
      Exit;
    end;
    Start := Start - L;
  until Start < 1;
end;

function PosNonSpace(Start: Integer; const SourceText: string): Integer;
var
  P, L: Integer;
begin
  Result := 0;
  L := Length(SourceText);
  P := Start;
  if L = 0 then
    Exit;
  while (P < L) and (SourceText[P] = ' ') do
    Inc(P);
  if SourceText[P] <> ' ' then
    Result := P;
end;

function BeginOfAttribute(Start: Integer; const SourceText: string): Integer;
var
  P, L: Integer;
begin
  // parses the beginning of an attribute: space + alpha character
  Result := 0;
  L := Length(SourceText);
  if L = 0 then
    Exit;
  P := PosStr(' ', SourceText, Start);
  if P = 0 then
    Exit;
  P := PosNonSpace(P, SourceText);
  if P = 0 then
    Exit;
  if SourceText[P] in ['a'..'z', 'A'..'Z'] then
    Result := P;
end;

function ParseAttribute(var Start: Integer; const SourceText: string;
  var AName, AValue: string): Boolean;
var
  PN, PV, P: Integer;
begin
  // parses a name="value" attribute from Start; returns 0 when not found or else the position behind the attribute
  Result := False;
  PN := BeginOfAttribute(Start, SourceText);
  if PN = 0 then
    Exit;
  P := PosStr('="', SourceText, PN);
  if P = 0 then
    Exit;
  AName := Trim(Copy(SourceText, PN, P - PN));
  PV := P + 2;
  P := PosStr('"', SourceText, PV);
  if P = 0 then
    Exit;
  AValue := Copy(SourceText, PV, P - PV);
  Start := P + 1;
  Result := True;
end;

procedure ParseAttributes(const SourceText: string; Attributes: TStrings);
var
  Name, Value: string;
  Start: Integer;
begin
  Attributes.BeginUpdate;
  try
    Attributes.Clear;
    Start := 1;
    while ParseAttribute(Start, SourceText, Name, Value) do
      Attributes.Add(Name + '=' + Value);
  finally
    Attributes.EndUpdate;
  end;
end;

function GetToken(var Start: Integer; const SourceText: string): string;
var
  P1, P2: Integer;
begin
  Result := '';
  if Start > Length(SourceText) then
    Exit;
  P1 := PosNonSpace(Start, SourceText);
  if P1 = 0 then
    Exit;
  if SourceText[P1] = '"' then
  begin // quoted token
    P2 := PosStr('"', SourceText, P1 + 1);
    if P2 = 0 then
      Exit;
    Result := Copy(SourceText, P1 + 1, P2 - P1 - 1);
    Start := P2 + 1;
  end
  else
  begin
    P2 := PosStr(' ', SourceText, P1 + 1);
    if P2 = 0 then
      P2 := Length(SourceText) + 1;
    Result := Copy(SourceText, P1, P2 - P1);
    Start := P2;
  end;
end;

function Easter(NYear: Integer): TDateTime;
var
  NMonth, NDay, NMoon, NEpact, NSunday, NGold, NCent, NCorX, NCorZ: Integer;
begin

  { The Golden Number of the year in the 19 year Metonic Cycle }
  NGold := ((NYear mod 19) + 1);

  { Calculate the Century }
  NCent := ((NYear div 100) + 1);

  { No. of Years in which leap year was dropped in order to keep in step
    with the sun }
  NCorX := ((3 * NCent) div 4 - 12);

  { Special Correction to Syncronize Easter with the moon's orbit }
  NCorZ := ((8 * NCent + 5) div 25 - 5);

  { Find Sunday }
  NSunday := ((5 * NYear) div 4 - NCorX - 10);

  { Set Epact (specifies occurance of full moon }
  NEpact := ((11 * NGold + 20 + NCorZ - NCorX) mod 30);

  if (NEpact < 0) then
    NEpact := NEpact + 30;

  if ((NEpact = 25) and (NGold > 11)) or (NEpact = 24) then
    NEpact := NEpact + 1;

  { Find Full Moon }
  NMoon := 44 - NEpact;

  if (NMoon < 21) then
    NMoon := NMoon + 30;

  { Advance to Sunday }
  NMoon := (NMoon + 7 - ((NSunday + NMoon) mod 7));

  if (NMoon > 31) then
  begin
    NMonth := 4;
    NDay := (NMoon - 31);
  end
  else
  begin
    NMonth := 3;
    NDay := NMoon;
  end;

  Result := EncodeDate(NYear, NMonth, NDay);
end;

//gets a datecode. Returns year and weeknumber in format: YYWW

{DayOfWeek function returns Integer 1..7 equivalent to Sunday..Saturday.
ISO 8601 weeks Start with Monday and the first week of a year is the one which
includes the first Thursday - Fiddle takes care of all this}

function GetWeekNumber(Today: TDateTime): string;
const
  Fiddle: array [1..7] of Byte = (6, 7, 8, 9, 10, 4, 5);
var
  Present, StartOfYear: TDateTime;
  FirstDayOfYear, WeekNumber, NumberOfDays: Integer;
  Year, Month, Day: Word;
  YearNumber: string;
begin
  Present := Trunc(Today); //truncate to remove hours, mins and secs
  DecodeDate(Present, Year, Month, Day); //decode to find year
  StartOfYear := EncodeDate(Year, 1, 1); //encode 1st Jan of the year

  //find what day of week 1st Jan is, then add days according to rule
  FirstDayOfYear := Fiddle[DayOfWeek(StartOfYear)];

  //calc number of days since beginning of year + additional according to rule
  NumberOfDays := Trunc(Present - StartOfYear) + FirstDayOfYear;

  //calc number of weeks
  WeekNumber := Trunc(NumberOfDays / 7);

  //Format year, needed to prevent millenium bug and keep the Fluffy Spangle happy
  YearNumber := FormatDateTime('yyyy', Present);

  YearNumber := YearNumber + 'W';

  if WeekNumber < 10 then
    YearNumber := YearNumber + '0'; //add leading zero for week

  //create datecode string
  Result := YearNumber + IntToStr(WeekNumber);

  if WeekNumber = 0 then //recursive call for year begin/end...
    //see if previous year end was week 52 or 53
    Result := GetWeekNumber(EncodeDate(Year - 1, 12, 31))
  else
  if WeekNumber = 53 then
    //if 31st December less than Thursday then must be week 01 of next year
    if DayOfWeek(EncodeDate(Year, 12, 31)) < 5 then
    begin
      YearNumber := FormatDateTime('yyyy', EncodeDate(Year + 1, 1, 1));
      Result := YearNumber + 'W01';
    end;
end;

function RelativePath(const ASrc, ADst: string): string;
var
  Doc, SDoc, ParDoc, Img, SImg, ParImg, Rel: string;
  PDoc, PImg: Integer;
begin
  Doc := ASrc;
  Img := ADst;
  repeat
    PDoc := Pos('\', Doc);
    if PDoc > 0 then
    begin
      ParDoc := Copy(Doc, 1, PDoc);
      ParDoc[Length(ParDoc)] := '/';
      SDoc := SDoc + ParDoc;
      Delete(Doc, 1, PDoc);
    end;
    PImg := Pos('\', Img);
    if PImg > 0 then
    begin
      ParImg := Copy(Img, 1, PImg);
      ParImg[Length(ParImg)] := '/';
      SImg := SImg + ParImg;
      Delete(Img, 1, PImg);
    end;
    if (PDoc > 0) and (PImg > 0) and (SDoc <> SImg) then
      Rel := '../' + Rel + ParImg;
    if (PDoc = 0) and (PImg <> 0) then
    begin
      Rel := Rel + ParImg + Img;
      if Pos(':', Rel) > 0 then
        Rel := '';
      Result := Rel;
      Exit;
    end;
    if (PDoc > 0) and (PImg = 0) then
    begin
      Rel := '../' + Rel;
    end;
  until (PDoc = 0) and (PImg = 0);
  Rel := Rel + ExtractFileName(Img);
  if Pos(':', Rel) > 0 then
    Rel := '';
  Result := Rel;
end;

procedure GetHTMLAnchors(const AFile: string; AList: TStringList);
var
  S, SA: string;
  P1, P2: Integer;
begin
  S := LoadString(AFile);
  P1 := 1;
  repeat
    P1 := PosText('<a name="', S, P1);
    if P1 <> 0 then
    begin
      P2 := PosText('"', S, P1 + 9);
      if P2 <> 0 then
      begin
        SA := Copy(S, P1 + 9, P2 - P1 - 9);
        AList.Add(SA);
        P1 := P2;
      end
      else
        P1 := 0;
    end;
  until P1 = 0;
end;

function UppercaseHTMLTags(const AText: string): string;
var
  P, P2: Integer;
begin
  Result := '';
  P2 := 1;
  repeat
    P := PosStr('<', AText, P2);
    if P > 0 then
    begin
      Result := Result + Copy(AText, P2, P - P2);
      P2 := P;
      if Copy(AText, P, 4) = '<!--' then
      begin
        P := PosStr('-->', AText, P);
        if P > 0 then
        begin
          Result := Result + Copy(AText, P2, P + 3 - P2);
          P2 := P + 3;
        end
        else
          Result := Result + Copy(AText, P2, Length(AText));
      end
      else
      begin
        P := PosStr('>', AText, P);
        if P > 0 then
        begin
          Result := Result + UpperCase(Copy(AText, P2, P - P2 + 1));
          P2 := P + 1;
        end
        else
          Result := Result + Copy(AText, P2, Length(AText));
      end;
    end
    else
    begin
      Result := Result + Copy(AText, P2, Length(AText));
    end;
  until P = 0;
end;

function LowercaseHTMLTags(const AText: string): string;
var
  P, P2: Integer;
begin
  Result := '';
  P2 := 1;
  repeat
    P := PosStr('<', AText, P2);
    if P > 0 then
    begin
      Result := Result + Copy(AText, P2, P - P2);
      P2 := P;
      // now check for comments
      if Copy(AText, P, 4) = '<!--' then
      begin
        P := PosStr('-->', AText, P);
        if P > 0 then
        begin
          Result := Result + Copy(AText, P2, P + 3 - P2);
          P2 := P + 3;
        end
        else
          Result := Result + Copy(AText, P2, Length(AText));
      end
      else
      begin
        P := PosStr('>', AText, P);
        if P > 0 then
        begin
          Result := Result + LowerCase(Copy(AText, P2, P - P2 + 1));
          P2 := P + 1;
        end
        else
          Result := Result + Copy(AText, P2, Length(AText));
      end;
    end
    else
    begin
      Result := Result + Copy(AText, P2, Length(AText));
    end;
  until P = 0;
end;

function HexToColor(const AText: string): TColor;
begin
  Result := clBlack;
  if Length(AText) <> 7 then
    Exit;
  if AText[1] <> '#' then
    Exit;
  try
    Result := StringToColor('$' + Copy(AText, 6, 2) + Copy(AText, 4, 2) + Copy(AText, 2, 2));
  except
    Result := clBlack;
  end;
end;

function PosEscaped(Start: Integer; const SourceText, FindText: string; EscapeChar: Char): Integer;
begin
  Result := PosText(FindText, SourceText, Start);
  if Result = 0 then
    Exit;
  if Result = 1 then
    Exit;
  if SourceText[Result - 1] <> EscapeChar then
    Exit;
  repeat
    Result := PosText(FindText, SourceText, Result + 1);
    if Result = 0 then
      Exit;
  until SourceText[Result - 1] <> EscapeChar;
end;

function DeleteEscaped(const SourceText: string; EscapeChar: Char): string;
var
  I: Integer;
  RealLen: Integer;
begin
  RealLen := 0;
  SetLength(Result, Length(SourceText));
  for I := 1 to Length(SourceText) do
    if SourceText[I] <> EscapeChar then
    begin
      Inc(RealLen);
      Result[RealLen] := SourceText[I];
    end;
  SetLength(Result, RealLen);
end;

procedure RecurseDirFiles(const ADir: string; var AFileList: TStringList);
var
  SR: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faAnyFile or faDirectory;
  if FindFirst(ADir + PathDelim + AllFilePattern, FileAttrs, SR) = 0 then
    while FindNext(SR) = 0 do
      if (SR.Attr and faDirectory) <> 0 then
      begin
        if (SR.Name <> '.') and (SR.Name <> '..') then
          RecurseDirFiles(ADir + PathDelim + SR.Name, AFileList);
      end
      else
        AFileList.Add(ADir + PathDelim + SR.Name);
  FindClose(SR);
end;

procedure RecurseDirProgs(const ADir: string; var AFileList: TStringList);
var
  SR: TSearchRec;
  FileAttrs: Integer;
  E: string;
  {$IFDEF UNIX}
  ST: TStatBuf;
  {$ENDIF UNIX}
begin
  FileAttrs := faAnyFile or faDirectory;
  if FindFirst(ADir + PathDelim + AllFilePattern, FileAttrs, SR) = 0 then
    while FindNext(SR) = 0 do
    begin
      if (SR.Attr and faDirectory) <> 0 then
      begin
        if (SR.Name <> '.') and (SR.Name <> '..') then
          RecurseDirProgs(ADir + PathDelim + SR.Name, AFileList);
      end
      {$IFDEF MSWINDOWS}
      else
      begin
        E := LowerCase(ExtractFileExt(SR.Name));
        if E = '.exe' then
          AFileList.Add(ADir + PathDelim + SR.Name);
      end;
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      else
      begin
        if stat(PChar(ADir + PathDelim + SR.Name), ST) = 0 then
        begin
          if ST.st_mode and (S_IXUSR or S_IXGRP or S_IXOTH) <> 0 then
            AFileList.Add(ADir + PathDelim + SR.Name);
        end;
      end;
      {$ENDIF UNIX}
    end;
  FindClose(SR);
end;

procedure LoadResourceFile(AFile: string; MemStream: TMemoryStream);
var
  HResInfo: HRSRC;
  HGlobal: THandle;
  Buffer, GoodType: PChar;
  Ext: string;
begin
  Ext := UpperCase(ExtractFileExt(AFile));
  Ext := Copy(Ext, 2, Length(Ext));
  if Ext = 'HTM' then
    Ext := 'HTML';
  GoodType := PChar(Ext);
  AFile := ChangeFileExt(AFile, '');
  HResInfo := FindResource(HInstance, PChar(AFile), GoodType);
  HGlobal := LoadResource(HInstance, HResInfo);
  if HGlobal = 0 then
    raise EResNotFound.CreateResFmt(@RsECannotLoadResource, [AFile]);
  Buffer := LockResource(HGlobal);
  MemStream.Clear;
  MemStream.WriteBuffer(Buffer[0], SizeOfResource(HInstance, HResInfo));
  MemStream.Seek(0, 0);
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
end;

procedure GetNames(AText: string; AList: TStringList);
var
  P: Integer;
  S: string;
begin
  AList.Clear;
  repeat
    AText := Trim(AText);
    P := Pos('="', AText);
    if P > 0 then
    begin
      S := Copy(AText, 1, P - 1);
      AList.Add(S);
      Delete(AText, 1, P + 1);
      P := Pos('"', AText);
      if P > 0 then
        Delete(AText, 1, P);
    end;
  until P = 0;
end;

function NameValuesToXML(const AText: string): string;
var
  AList: TStringList;
  I, C: Integer;
  IName, IValue, Xml: string;
begin
  Result := '';
  if AText = '' then
    Exit;
  AList := TStringList.Create;
  GetNames(AText, AList);
  C := AList.Count;
  if C = 0 then
  begin
    AList.Free;
    Exit
  end;
  Xml := '<accountdata>' + Cr;
  for I := 0 to C - 1 do
  begin
    IName := AList[I];
    IValue := GetValue(AText, IName);
    IValue := StringReplace(IValue, '~~', Cr, [rfReplaceAll]);
    Xml := Xml + '<' + IName + '>' + Cr;
    Xml := Xml + '  ' + IValue + Cr;
    Xml := Xml + '</' + IName + '>' + Cr;
  end;
  Xml := Xml + '</accountdata>' + Cr;
  AList.Free;
  Result := Xml;
end;

function LastPosChar(const FindChar: Char; SourceString: string): Integer;
var
  I: Integer;
begin
  I := Length(SourceString);
  while (I > 0) and (SourceString[I] <> FindChar) do
    Dec(I);
  Result := I;
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

function GetBoolValue(const AText, AName: string): Boolean;
begin
  Result := LowerCase(GetValue(AText, AName)) = 'yes';
end;

procedure ListSelect(Src, Dst: TStringList; const AKey, AValue: string);
var
  I: Integer;
begin
  Dst.Clear;
  for I := 0 to Src.Count - 1 do
  begin
    if GetValue(Src[I], AKey) = AValue then
      Dst.Add(Src[I]);
  end;
end;

procedure ListFilter(Src: TStringList; const AKey, AValue: string);
var
  I: Integer;
  Dst: TStringList;
begin
  Dst := TStringList.Create;
  for I := 0 to Src.Count - 1 do
  begin
    if GetValue(Src[I], AKey) = AValue then
      Dst.Add(Src[I]);
  end;
  Src.Assign(Dst);
  Dst.Free;
end;

procedure ListOrderBy(Src: TStringList; const AKey: string; Numeric: Boolean);
var
  I, Index: Integer;
  Lit, Dst: TStringList;
  S: string;
  IValue: Integer;
begin
  if Src.Count < 2 then
    Exit; // nothing to sort
  Lit := TStringList.Create;
  Dst := TStringList.Create;
  for I := 0 to Src.Count - 1 do
  begin
    S := GetValue(Src[I], AKey);
    if Numeric then
    try
      IValue := StrToInt(S);
      // format to 5 decimal places for correct string sorting
      // e.g. 5 becomes 00005
      S := Format('%5.5d', [IValue]);
    except
      // just use the unformatted value
    end;
    Lit.AddObject(S, TObject(I));
  end;
  Lit.Sort;
  for I := 0 to Src.Count - 1 do
  begin
    Index := Integer(Lit.Objects[I]);
    Dst.Add(Src[Index]);
  end;
  Lit.Free;
  Src.Assign(Dst);
  Dst.Free;
end;

// converts a csv list to a tagged string list

procedure CSVToTags(Src, Dst: TStringList);
var
  I, FI, FC: Integer;
  Names: TStringList;
  Rec: TStringList;
  S: string;
begin
  Dst.Clear;
  if Src.Count < 2 then
    Exit;
  Names := TStringList.Create;
  Rec := TStringList.Create;
  try
    Names.CommaText := Src[0];
    FC := Names.Count;
    if FC > 0 then
      for I := 1 to Src.Count - 1 do
      begin
        Rec.CommaText := Src[I];
        S := '';
        for FI := 0 to FC - 1 do
          S := S + Names[FI] + '="' + Rec[FI] + '" ';
        Dst.Add(S);
      end;
  finally
    Rec.Free;
    Names.Free;
  end;
end;

// converts a tagged string list to a csv list
// only fieldnames from the first record are scanned ib the other records

procedure TagsToCSV(Src, Dst: TStringList);
var
  I, FI, FC: Integer;
  Names: TStringList;
  Rec: TStringList;
  S: string;
begin
  Dst.Clear;
  if Src.Count < 1 then
    Exit;
  Names := TStringList.Create;
  Rec := TStringList.Create;
  try
    GetNames(Src[0], Names);
    FC := Names.Count;
    if FC > 0 then
    begin
      Dst.Add(Names.CommaText);
      for I := 0 to Src.Count - 1 do
      begin
        S := '';
        Rec.Clear;
        for FI := 0 to FC - 1 do
          Rec.Add(GetValue(Src[I], Names[FI]));
        Dst.Add(Rec.CommaText);
      end;
    end;
  finally
    Rec.Free;
    Names.Free;
  end;
end;

function B64Encode;
var
  I: Integer;
  InBuf: array [0..2] of Byte;
  OutBuf: array [0..3] of Char;
begin
  SetLength(Result, ((Length(S) + 2) div 3) * 4);
  for I := 1 to ((Length(S) + 2) div 3) do
  begin
    if Length(S) < (I * 3) then
      Move(S[(I - 1) * 3 + 1], InBuf, Length(S) - (I - 1) * 3)
    else
      Move(S[(I - 1) * 3 + 1], InBuf, 3);
    OutBuf[0] := B64Table[((InBuf[0] and $FC) shr 2) + 1];
    OutBuf[1] := B64Table[(((InBuf[0] and $03) shl 4) or ((InBuf[1] and $F0) shr 4)) + 1];
    OutBuf[2] := B64Table[(((InBuf[1] and $0F) shl 2) or ((InBuf[2] and $C0) shr 6)) + 1];
    OutBuf[3] := B64Table[(InBuf[2] and $3F) + 1];
    Move(OutBuf, Result[(I - 1) * 4 + 1], 4);
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
  I: Integer;
  InBuf: array [0..3] of Byte;
  OutBuf: array [0..2] of Byte;
  RetValue: string;
begin
  if ((Length(S) mod 4) <> 0) or (S = '') then
    raise EJVCLException.CreateRes(@RsEIncorrectStringFormat);

  SetLength(RetValue, ((Length(S) div 4) - 1) * 3);
  for I := 1 to ((Length(S) div 4) - 1) do
  begin
    Move(S[(I - 1) * 4 + 1], InBuf, 4);
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
    Move(OutBuf, RetValue[(I - 1) * 3 + 1], 3);
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

function Encrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + Char(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(Result[I]) + StartKey) * MultKey + AddKey;
  end;
end;
{*******************************************************
 * Standard Decryption algorithm - Copied from Borland *
 *******************************************************}

function Decrypt(const InString: string; StartKey, MultKey, AddKey: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(InString) do
  begin
    Result := Result + Char(Byte(InString[I]) xor (StartKey shr 8));
    StartKey := (Byte(InString[I]) + StartKey) * MultKey + AddKey;
  end;
end;

function EncryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := B64Encode(Encrypt(InString, StartKey, MultKey, AddKey));
end;

function DecryptB64(const InString: string; StartKey, MultKey, AddKey: Integer): string;
begin
  Result := Decrypt(B64Decode(InString), StartKey, MultKey, AddKey);
end;

function Hash(const AText: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AText = '' then
    Exit;
  Result := Ord(AText[1]);
  for I := 2 to Length(AText) do
    Result := (Result * Ord(AText[I])) xor Result;
end;

{replace any <,> etc by &lt; &gt;}

function XMLSafe(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    if AText[I] = '<' then
      Result := Result + '&lt;'
    else
    if AText[I] = '>' then
      Result := Result + '&gt;'
    else
    if AText[I] = '&' then
      Result := Result + '&amp;'
    else
    if (Ord(AText[I]) >= 32) and (Ord(AText[I]) < 128) then
      Result := Result + AText[I]
    else
    if Ord(AText[I]) > 127 then
      Result := Result + '&#' + IntToStr(Ord(AText[I])) + ';'
    else
      Result := Result + ' ';
end;

function FirstOfSet(const AText: string): string;
var
  P: Integer;
begin
  Result := Trim(AText);
  if Result = '' then
    Exit;
  if Result[1] = '"' then
  begin
    P := PosStr('"', Result, 2);
    Result := Copy(Result, 2, P - 2);
  end
  else
  begin
    P := Pos(' ', Result);
    Result := Copy(Result, 1, P - 1);
  end;
end;

function LastOfSet(const AText: string): string;
var
  C: Integer;
begin
  Result := Trim(AText);
  if Result = '' then
    Exit;
  C := Length(Result);
  if Result[C] = '"' then
  begin
    while (C > 1) and (Result[C - 1] <> '"') do
      Dec(C);
    Result := Copy(Result, C, Length(Result) - C);
  end
  else
  begin
    while (C > 1) and (Result[C - 1] <> ' ') do
      Dec(C);
    Result := Copy(Result, C, Length(Result));
  end;
end;

function CountOfSet(const AText: string): Integer;
var
  Lit: TStringList;
begin
  Lit := TStringList.Create;
  SplitSet(AText, Lit);
  Result := Lit.Count;
  Lit.Free;
end;

function SetRotateRight(const AText: string): string;
var
  Lit: TStringList;
  C: Integer;
begin
  Lit := TStringList.Create;
  SplitSet(AText, Lit);
  C := Lit.Count;
  if C > 0 then
  begin
    Lit.Move(C - 1, 0);
    Result := JoinSet(Lit);
  end
  else
    Result := '';
  Lit.Free;
end;

function SetRotateLeft(const AText: string): string;
var
  Lit: TStringList;
  C: Integer;
begin
  Lit := TStringList.Create;
  SplitSet(AText, Lit);
  C := Lit.Count;
  if C > 0 then
  begin
    Lit.Move(0, C - 1);
    Result := JoinSet(Lit);
  end
  else
    Result := '';
  Lit.Free;
end;

procedure SplitSet(AText: string; AList: TStringList);
var
  P: Integer;
begin
  AList.Clear;
  if AText = '' then
    Exit;
  AText := Trim(AText);
  while AText <> '' do
  begin
    if AText[1] = '"' then
    begin
      Delete(AText, 1, 1);
      P := Pos('"', AText);
      if P <> 0 then
      begin
        AList.Add(Copy(AText, 1, P - 1));
        Delete(AText, 1, P);
      end;
    end
    else
    begin
      P := Pos(' ', AText);
      if P = 0 then
      begin
        AList.Add(AText);
        AText := '';
      end
      else
      begin
        AList.Add(Copy(AText, 1, P - 1));
        Delete(AText, 1, P);
      end;
    end;
    AText := Trim(AText);
  end;
end;

function JoinSet(AList: TStringList): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AList.Count - 1 do
    Result := Result + AList[I] + ' ';
  Delete(Result, Length(Result), 1);
end;

function SetPick(const AText: string; AIndex: Integer): string;
var
  Lit: TStringList;
  C: Integer;
begin
  Lit := TStringList.Create;
  SplitSet(AText, Lit);
  C := Lit.Count;
  if (C > 0) and (AIndex < C) then
    Result := Lit[AIndex]
  else
    Result := '';
  Lit.Free;
end;

function SetSort(const AText: string): string;
var
  Lit: TStringList;
begin
  Lit := TStringList.Create;
  SplitSet(AText, Lit);
  if Lit.Count > 0 then
  begin
    Lit.Sort;
    Result := JoinSet(Lit);
  end
  else
    Result := '';
  Lit.Free;
end;

function SetUnion(const Set1, Set2: string): string;
var
  Lit1, Lit2, Lit3: TStringList;
  I, C: Integer;
begin
  Lit1 := TStringList.Create;
  Lit2 := TStringList.Create;
  Lit3 := TStringList.Create;
  SplitSet(Set1, Lit1);
  SplitSet(Set2, Lit2);
  C := Lit2.Count;
  if C <> 0 then
  begin
    Lit2.Addstrings(Lit1);
    for I := 0 to Lit2.Count - 1 do
      if Lit3.IndexOf(Lit2[I]) = -1 then
        Lit3.Add(Lit2[I]);
    Result := JoinSet(Lit3);
  end
  else
  begin
    Result := JoinSet(Lit1);
  end;
  Lit1.Free;
  Lit2.Free;
  Lit3.Free;
end;

function SetIntersect(const Set1, Set2: string): string;
var
  Lit1, Lit2, Lit3: TStringList;
  I: Integer;
begin
  Lit1 := TStringList.Create;
  Lit2 := TStringList.Create;
  Lit3 := TStringList.Create;
  SplitSet(Set1, Lit1);
  SplitSet(Set2, Lit2);
  if Lit2.Count <> 0 then
  begin
    for I := 0 to Lit2.Count - 1 do
      if Lit1.IndexOf(Lit2[I]) <> -1 then
        Lit3.Add(Lit2[I]);
    Result := JoinSet(Lit3);
  end
  else
    Result := '';
  Lit1.Free;
  Lit2.Free;
  Lit3.Free;
end;

function SetExclude(const Set1, Set2: string): string;
var
  Lit1, Lit2: TStringList;
  I, Index: Integer;
begin
  Lit1 := TStringList.Create;
  Lit2 := TStringList.Create;
  SplitSet(Set1, Lit1);
  SplitSet(Set2, Lit2);
  if Lit2.Count <> 0 then
  begin
    for I := 0 to Lit2.Count - 1 do
    begin
      Index := Lit1.IndexOf(Lit2[I]);
      if Index <> -1 then
        Lit1.Delete(Index);
    end;
    Result := JoinSet(Lit1);
  end
  else
    Result := JoinSet(Lit1);
  Lit1.Free;
  Lit2.Free;
end;

// This function converts a string into a RFC 1630 compliant URL

function URLEncode(const Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Value) do
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

function URLDecode(const Value: string): string;
const
  HexChars = '0123456789ABCDEF';
var
  I: Integer;
  Ch, H1, H2: Char;
  Len: Integer;
begin
  Result := '';
  Len := Length(Value);
  I := 1;
  while I <= Len do
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

function ReplaceFirst(const SourceStr, FindStr, ReplaceStr: string): string;
var
  P: Integer;
begin
  Result := SourceStr;
  P := PosText(FindStr, SourceStr, 1);
  if P <> 0 then
    Result := Copy(SourceStr, 1, P - 1) + ReplaceStr + Copy(SourceStr, P + Length(FindStr), Length(SourceStr));
end;

function ReplaceLast(const SourceStr, FindStr, ReplaceStr: string): string;
var
  P: Integer;
begin
  Result := SourceStr;
  P := PosTextLast(FindStr, SourceStr);
  if P <> 0 then
    Result := Copy(SourceStr, 1, P - 1) + ReplaceStr + Copy(SourceStr, P + Length(FindStr), Length(SourceStr));
end;

// insert a block template
// the last occurance of {block:aBlockname}
// the block template is marked with {begin:aBlockname} and {end:aBlockname}

function InsertLastBlock(var SourceStr: string; BlockStr: string): Boolean;
var
  // phead: Integer;
  PBlock, PE, PB: Integer;
  SBB, SBE, SB, SBR: string;
  SBBL, SBEL: Integer;
begin
  Result := False;
  //  phead:= PosStr('</head>',SourceStr,1);
  //  If phead = 0 Then Exit;
  //  phead:= phead + 7;
  SB := '{block:' + BlockStr + '}';
  //  sbL:=Length(SB);
  SBB := '{begin:' + BlockStr + '}';
  SBBL := Length(SBB);
  SBE := '{end:' + BlockStr + '}';
  SBEL := Length(SBE);
  PBlock := PosTextLast(SB, SourceStr);
  if PBlock = 0 then
    Exit;
  PB := PosText(SBB, SourceStr, 1);
  if PB = 0 then
    Exit;
  PE := PosText(SBE, SourceStr, PB);
  if PE = 0 then
    Exit;
  PE := PE + SBEL - 1;
  // now replace
  SBR := Copy(SourceStr, PB + SBBL, PE - PB - SBBL - SBEL + 1);
  SourceStr := Copy(SourceStr, 1, PBlock - 1) + SBR + Copy(SourceStr, PBlock, Length(SourceStr));
  Result := True;
end;

// removes all  {begin:somefield} to {end:somefield} from ASource

function RemoveMasterBlocks(const SourceStr: string): string;
var
  S, Src: string;
  PB: Integer;
  PE: Integer;
  PEE: Integer;
begin
  S := '';
  Src := SourceStr;
  repeat
    PB := PosText('{begin:', Src);
    if PB > 0 then
    begin
      PE := PosText('{end:', Src, PB);
      if PE > 0 then
      begin
        PEE := PosStr('}', Src, PE);
        if PEE > 0 then
        begin
          S := S + Copy(Src, 1, PB - 1);
          Delete(Src, 1, PEE);
        end;
      end;
    end;
  until PB = 0;
  Result := S + Src;
end;

// removes all {field} entries in a template

function RemoveFields(const SourceStr: string): string;
var
  Src, S: string;
  PB: Integer;
  PE: Integer;
begin
  S := '';
  Src := SourceStr;
  repeat
    PB := Pos('{', Src);
    if PB > 0 then
    begin
      PE := Pos('}', Src);
      if PE > 0 then
      begin
        S := S + Copy(Src, 1, PB - 1);
        Delete(Src, 1, PE);
      end;
    end;
  until PB = 0;
  Result := S + Src;
end;

{finds the last occurance}

function PosStrLast(const FindString, SourceString: string): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  L := Length(FindString);
  if L = 0 then
    Exit;
  I := Length(SourceString);
  if I = 0 then
    Exit;
  I := I - L + 1;
  while I > 0 do
  begin
    Result := PosStr(FindString, SourceString, I);
    if Result > 0 then
      Exit;
    I := I - L;
  end;
end;

{finds the last occurance}

function PosTextLast(const FindString, SourceString: string): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  L := Length(FindString);
  if L = 0 then
    Exit;
  I := Length(SourceString);
  if I = 0 then
    Exit;
  I := I - L + 1;
  while I > 0 do
  begin
    Result := PosText(FindString, SourceString, I);
    if Result > 0 then
      Exit;
    I := I - L;
  end;
end;

procedure DirFiles(const ADir, AMask: string; AFileList: TStringList);
var
  SR: TSearchRec;
  FileAttrs: Integer;
begin
  FileAttrs := faArchive + faDirectory;
  if FindFirst(ADir + AMask, FileAttrs, SR) = 0 then
    while FindNext(SR) = 0 do
      if (SR.Attr and faArchive) <> 0 then
        AFileList.Add(ADir + SR.Name);
  FindClose(SR);
end;

// parse number returns the last position, starting from 1

function ParseNumber(const S: string): Integer;
var
  I, E, E2, C: Integer;
begin
  Result := 0;
  I := 0;
  C := Length(S);
  if C = 0 then
    Exit;
  while (I + 1 <= C) and (S[I + 1] in (DigitChars + [',', '.'])) do
    Inc(I);
  if (I + 1 <= C) and (S[I + 1] in ['e', 'E']) then
  begin
    E := I;
    Inc(I);
    if (I + 1 <= C) and (S[I + 1] in ['+', '-']) then
      Inc(I);
    E2 := I;
    while (I + 1 <= C) and (S[I + 1] in DigitChars) do
      Inc(I);
    if I = E2 then
      I := E;
  end;
  Result := I;
end;

// parse a SQL style data string from positions 1,
// starts and ends with #

function ParseDate(const S: string): Integer;
var
  P: Integer;
begin
  Result := 0;
  if Length(S) < 2 then
    Exit;
  P := PosStr('#', S, 2);
  if P <> 0 then
    try
      StrToDate(Copy(S, 2, P - 2));
      Result := P;
    except
      Result := 0;
    end;
end;

end.
