{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

               Changed StrSplit Function (has one new parameter).

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  Internal pchar-manipulation functions required by TJvCsvDataSet data access component.

  Useful extra functions for parsing strings using pascal,
  not present in your basic vanilla Pascal/Delphi standard
  libraries.

  MOST use PChars and char buffers, not the String type.

  These functions are used to implement the
  CsvDataSource component but are generally reuseable in
  any string parsing code.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCsvParse;

interface

uses
  Classes;

const
  MaxInitStrNum = 9;

  { String Class Functions - uses Delphi String objects instead of Pascal PChars }

  {new 2003}
function StrSplit(const inString: String; const splitChar, quoteChar: Char;
  var OutStrings: array of String; MaxSplit: Integer): Integer;
  {new 2004}
function StrSplitStrings( const inString:String; const splitChar,quoteChar:Char; OutStrings:TStrings):Integer;


  { circa 1998-2001 classic functions }
function StrStrip(s: String): String; // Strip whitespace, carriage returns, linefeeds.
function GetString(var Source: String; seperator: String): String;
  // Iteratively split off a piece of a string. Modifies original string.
function PadString(s: String; len: Integer; padchar: Char): String;
  //procedure Gibble(var S: string); // Deprecated. With a name like Gibble, are you surprised?
function BuildPathName(PathName, FileName: String): String;
function StrEatWhiteSpace(s: String): String;
function HexToAscii(s: String): String;
function AsciiToHex(s: String): String;
function StripQuotes(s1: String): String;

{ TStrings helper functions }
function GetIntValueFromResultString(VarName: String; ResultStrings: TStrings;
  DefVal: Integer): Integer;
function GetValueFromResultString(VarName: string; ResultStrings: TStrings): string;

{ Pascal Low Level PChar Functions }
function ValidNumericLiteral(s1: PChar): Boolean;
function ValidIntLiteral(s1: PChar): Boolean;
function ValidHexLiteral(s1: PChar): Boolean;
function HexPcharToInt(s1: PChar): Integer;
function ValidStringLiteral(s1: PChar): Boolean;
function StripPCharQuotes(s1: PChar): String;

function ValidIdentifier(s1: PChar): Boolean;
function EndChar(x: Char): Boolean;
procedure GetToken(s1, s2: PChar);
function IsExpressionKeyword(s1: PChar): Boolean;
function IsKeyword(s1: PChar): Boolean;
function ValidVarReference(s1: PChar): Boolean;
function GetParenthesis(s1, s2: PChar): Boolean;
procedure GetVarReference(s1, s2, sidx: PChar);
procedure PcharEatWhiteChars(s1: PChar);

  { Debugging functions related to GetToken function. }
function GetTokenCount: Integer;
procedure ResetTokenCount;

implementation

uses
  SysUtils,
  JvConsts, JvResources, JvTypes;

var
  TokenCount: Integer = 0;

  { Returns true for literals like '123.456', '78', or '-35.1231231' }
function ValidNumericLiteral(s1: PChar): Boolean;
var
  l, x, x1: Integer;
  DecimalFlag: Boolean;
begin
  l := strlen(s1);
  DecimalFlag := False;
  x1 := 0;

  if (l <= 0) then 
  begin
    Result := False;
    Exit;
  end;
  
  { detect leading minus }
  if (s1[0] = '-') then
    Inc(x1); // skip the minus, as it's okay as a leading character
     
  { Detect a decimal number or integer number }
  for x := x1 to l - 1 do 
  begin
    if (s1[x] = '.') then 
    begin
      if (DecimalFlag) then  
      begin
        Result := False; // two decimal places is invalid.
        Exit;
      end;
      DecimalFlag := True;
    end
    else if not (s1[x] in DigitSymbols) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }
function ValidIntLiteral(s1: PChar): Boolean;
var
  l, x, x1: Integer;
begin
  l := strlen(s1);
  x1 := 0;
  if (l <= 0) then 
  begin
    Result := False;
    Exit;
  end;
  { detect leading minus }
  if (s1[0] = '-') then
    Inc(x1); // skip the minus, as it's okay as a leading character

  { Detect a decimal number or integer number }
  for x := x1 to l - 1 do 
  begin
    if not (s1[x] in DigitSymbols) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }
function ValidHexLiteral(s1: PChar): Boolean;
var
  l, x: Integer;
begin
  l := strlen(s1);
  //  x1 := 0;

  { detect hex code type indicator }
  if (l < 2) or (s1[0] <> '$') then 
  begin
    Result := False;
    Exit;
  end;

  { Detect hex digits }
  for x := 1 to l - 2 do 
  begin
    if not (s1[x] in HexadecimalSymbols) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function HexPcharToInt(s1: PChar): Integer;
var
  x, l: Integer;
  digit, val: Integer;
begin
  l := strlen(s1);
  if (l < 2) or (l > 9) then
    raise EJVCLException.Create(RsEInvalidHexLiteral);
  if s1[0] <> '$' then
    raise EJVCLException.Create(RsEInvalidHexLiteral);
  val := 0;
  for x := 1 to l - 2 do
  begin
    val := val * 16; { shift right four bits at a time }
    if s1[x] in DigitSymbols then
      digit := Ord(s1[x]) - Ord('0')
    else if s1[x] in HexadecimalLowercaseLetters then
      digit := Ord(s1[x]) - Ord('a') + 10
    else if s1[x] in HexadecimalUppercaseLetters then
      digit := Ord(s1[x]) - Ord('A') + 10
    else
      raise EJVCLException.Create(RsEInvalidHexLiteral);
    val := val + digit;
  end;
  Result := val;
end;



function ValidStringLiteral(s1: PChar): Boolean;
var
  l: Integer;
begin
  l := strlen(s1);
  if (s1[0] = '"') and (s1[l - 1] = '"') then
    Result := True
  else
    Result := False;
end;

{ Strip quotes and return as a real Delphi String }
function StripQuotes(s1: String): String;
begin
  if ValidStringLiteral(PChar(s1)) then
    Result := Copy(s1, 2,Length(s1) - 2)
  else
    Result := s1;
end;


// This function is limited to 1 to 254 characters:
function StripPcharQuotes(s1: PChar): String;
var
  tempbuf: array [0..256] of Char;
  l: Integer;
begin
  l := strlen(s1);
  if (l > 255) then
    l := 255;
  if ValidStringLiteral(s1) then 
  begin
    StrLCopy(tempbuf, s1 + 1,l - 2);
  end;
  Result := String(tempbuf);
end;


{ Prevent confusion between expression-keywords and variable identifiers }
function IsExpressionKeyword(s1: PChar): Boolean;
begin
  if (StrIComp(s1, 'AND') = 0) then
    Result := True
  else if (StrIComp(s1, 'OR') = 0) then
    Result := True
  else if (StrIComp(s1, 'XOR') = 0) then
    Result := True
  else if (StrIComp(s1, 'NOT') = 0) then
    Result := True
  else if (StrIComp(s1, 'DIV') = 0) then
    Result := True
  else if (StrIComp(s1, 'SHR') = 0) then
    Result := True
  else if (StrIComp(s1, 'SHL') = 0) then
    Result := True
  else
    Result := False;
end;

function IsKeyword(s1: PChar): Boolean;
begin
  Result := (StrIComp(s1, 'SET') = 0) or (StrIComp(s1, 'LET') = 0) or
    (StrIComp(s1, 'DIM') = 0) or (StrIComp(s1, 'ARRAYCOPY') = 0) or
    (StrIComp(s1, 'STRCOPY') = 0) or (StrIComp(s1, 'STRPAD') = 0) or
    (StrIComp(s1, 'STRSTRIP') = 0) or (StrIComp(s1, 'END') = 0) or
    (StrIComp(s1, 'INC') = 0) or (StrIComp(s1, 'DEC') = 0) or
    (StrIComp(s1, 'PARAM') = 0) or (StrIComp(s1, 'JUMP') = 0) or
    (StrIComp(s1, 'SLEEP') = 0) or (StrIComp(s1, 'GOTO') = 0) or
    (StrIComp(s1, 'IF') = 0) or (StrIComp(s1, 'CALL') = 0) or
    (StrIComp(s1, 'STOP') = 0) or (StrIComp(s1, 'CONST') = 0);
end;



{ ValidIdentifier:

  Valid identifier must start with a-z or A-Z or _, and can have alphanumeric or underscore(_)
  as subsequent characters, no spaces, punctuation, or other characters allowed.  Same rules as
  most programming languages, Cobol being one particularly nasty exception! <grin>

    --Warren.
}
function ValidIdentifier(s1: PChar): Boolean;
var
  x, y: Integer;
  pass: Boolean;
begin
  Pass := True;

  if IsExpressionKeyword(s1) then 
  begin
    Result := False;
    Exit;
  end;
 
  x := strlen(s1);
  if (x < 1) or (x > 32) then 
  begin
    Result := False;
    Exit;
  end;
 
  if not (s1[0] in IdentifierFirstSymbols) then
    Pass := False;

  if Pass and (x > 1) then
    for y := 1 to x - 1 do 
    begin
      if not (s1[y] in IdentifierSymbols) then
      begin
        Pass := False;
        Result := Pass;
        Exit;
      end;
    end;

  Result := Pass;
end;



function EndChar(x: Char): Boolean;
begin
  if (x = ',') or (x = ';') or (x = ':') or (x = '[') or (x = ']') or
    (x = '(') or (x = ')') or (x = '#') or (x = '<') or (x = '>') or (x = '=') or
    (x = '*') or (x = '/') or (x = '+') or (x = Chr(0)) then
    Result := True
  else
    Result := False;
end;


procedure GetToken(s1, s2: PChar);
var
  w, x, y: Integer;
  InQuotes: Boolean;
begin
  x := 0;
  w := 0;

  { Empty in, Empty Out }
  if (strlen(s1) = 0) then
  begin
    s2[0] := Chr(0);
  end;

  InQuotes := False;

  { skip leading space }
  while (s1[x] = ' ') or (s1[x] = Chr(9)) do
    Inc(x);

  while True do
  begin
    if EndChar(s1[x]) and (not InQuotes) then
    begin
      { return punctuation one symbol at a time }
      if (w < 1) then
      begin
        s2[w] := s1[x];
        Inc(w);
        Inc(x);
      end;

      Break;
    end;

    if (s1[x] = '"') then
      InQuotes := not InQuotes;

    { Break if space found and not in quotes }
    if (s1[x] = ' ') and (not InQuotes) then
      Break
    else
    begin
      s2[w] := s1[x];
      Inc(w);
    end;

    Inc(x);
  end;
  // s2[x] := Chr(0);

  { detect not-equal, less-than-or-equal and greater-than-or-equal operators }
  if (w = 1) then
  begin
    if (s2[0] = '<') and (s1[x] = '>') then
    begin
      s2[w] := '>';
      Inc(x);
      Inc(w);   // char literal
    end
    else if (s2[0] = '<') and (s1[x] = '=') then
    begin
      s2[w] := '=';
      Inc(x);
      Inc(w);
    end
    else if (s2[0] = '>') and (s1[x] = '=') then
    begin
      s2[w] := '=';
      Inc(x);
      Inc(w);
    end;
  end;

  { remove token from initial buffer, move to second buffer }
  y := Integer(strlen(s1)) - x; //Cardinal removes warning.
  if (y > 0) then
    StrLCopy(s1, s1 + x, y) { copy remaining characters }
  else
    s1[0] := Chr(0); { just erase all of old string }

  s2[w] := Chr(0); { Terminate new string }
  Inc(TokenCount);
end;


function StrEatWhiteSpace(s: String): String;
var
  c: array [0..1024] of Char;
begin
  if Length(s) > 1024 then
  begin
    Result := s;
    Exit;
  end;
  StrCopy(c, PChar(s));
  PcharEatWhiteChars(c);
  Result := String(c);
end;

{ strip whitespace from pchar - space or tab }
procedure PcharEatWhiteChars(s1: PChar);
var
  T, U, L: Integer;
begin
  L := strlen(s1);
  // U := L;
  if (L <= 0) then Exit;
  { skip spaces starting at the beginning }
  for T := 0 to L do 
  begin
    if (s1[T] <> ' ') and (s1[T] <> Chr(9)) then
      Break;
  end;
  { skip spaces starting at the end }
  for U := L - 1 downto T do 
  begin
    if (s1[U] <> ' ') and (s1[U] <> Chr(9)) then
      Break;
  end;
  if (T > 0) or (U < L - 1) then 
  begin
    if (T > U) then 
    begin // was T>=U (test me!)
      s1[0] := Chr(0);
    end 
    else
      StrLCopy(s1, s1 + T, (U - T) + 1);
  end;
end;

function GetParenthesis(s1, s2: PChar): Boolean;
var
  token, tempbuf: array [0..128] of Char;
  brackets: Integer;
begin
  { make temporary copy of s1, check for parenthesis }
  StrCopy(tempbuf, s1);
  GetToken(tempbuf, s2);
  if (strcomp(s2, '(') = 0) then 
  begin
    brackets := 1;
    s2[0] := Chr(0);
    repeat
      GetToken(tempbuf, token);
      if (strcomp(token, ')') = 0) then Dec(brackets);
      if (brackets > 0) then 
      begin
        strcat(s2, token);
        strcat(s2, ' ');
      end;
      if (strcomp(token, '(') = 0) then Inc(brackets);
    until (strlen(s1) = 0) or (brackets = 0);
    if (brackets <> 0) then 
    begin
      s2[0] := Chr(0);
      Result := False;
      Exit;
    end;
    strcopy(s1, tempbuf); { remainder back into s1 }
    Result := True; { true }
  end 
  else 
  begin { not parenthesis }
    s2[0] := Chr(0);
    Result := False;
    Exit;
  end;
end;


{ Gets a single token like ABC, or gets ABC[X] type reference if present }
procedure GetVarReference(s1, s2, sidx: PChar);
var
  tempbuf: array [0..128] of Char;
  brackets: Integer;
begin
  GetToken(s1, s2);
  sidx[0] := Chr(0);
  PcharEatWhiteChars(s1);
  if (s1[0] = '[') then
  begin
    brackets := 0;
    repeat
      GetToken(s1, tempbuf);
      StrCat(sidx, tempbuf);
      if (StrComp(tempbuf, ']') = 0) then
        Dec(brackets);
      if (StrComp(tempbuf, '[') = 0) then
        Inc(brackets);

      if (strlen(s1) = 0) then
        Break;
    until brackets <= 0;

    { Remove outermost brackets }
    StrLCopy(sidx, sidx + 1, strlen(sidx) - 2);
  end;
end;




{ Expects ABC or ABC[X] type of reference }
function ValidVarReference(s1: PChar): Boolean;
var
  len1: Integer;
  tempbuf1, tempbuf2: array [0..128] of Char;
begin
  StrCopy(s1, tempbuf1);
  GetToken(tempbuf1, tempbuf2);
  if strlen(tempbuf1) = 0 then 
  begin
    Result := ValidIdentifier(s1);
  end 
  else
  begin
    len1 := strlen(tempbuf1);
    if (tempbuf1[0] = '[') and (tempbuf1[len1 - 1] = ']') then 
    begin
      Result := ValidIdentifier(s1);
    end 
    else
      Result := False;
  end;
end;

{ debugging and performance tuning information }

function GetTokenCount: Integer;
begin
  Result := TokenCount;
end;

procedure ResetTokenCount;
begin
  TokenCount := 0;
end;

function PadString(s: String; len: Integer; padchar: Char): String;
var
  x: String;
begin
  x := s;
  while Length(x) < len do 
  begin
    x := x + padchar;
  end;
  Result := x;
end;

{ Encoding function named in honor of Dennis Forbes' favourite word }
{procedure Gibble(var S: string);
var
 t,l,c1: integer;
 lo,hi:byte;
 x: array[0..255] of char;
begin
 l := length(s);
 for t:= 0 to l-1 do begin
     c1 := Ord(s[t+1] );
     if (c1  >= 32 ) AND (c1 <= 231) then begin
        c1 := c1 - 32;
        lo := (c1 MOD 25);
        hi := c1 div 25;
        lo := 24-lo;
        c1 := ((hi*25)+lo ) +32;
        x[t] := Chr(c1);
     end else
        x[t] := Chr(c1);
 end;
 x[l] := Chr(0);
 s := String(x);
end;
 }

function BuildPathName(PathName, FileName: String): String;
var
  l: Integer;
begin
  l := Length(PathName);
  if (l = 0) then
    Result := FileName
  else 
  begin
    if (PathName[l] = '\') then 
    begin
      Result := PathName + FileName;
    end 
    else 
    begin
      Result := PathName + '\' + FileName;
    end;
  end;
end;


function HexDigitVal(c: Char): Integer;
begin
  if c in DigitSymbols then
    Result := Ord(c) - Ord('0')
  else if c in HexadecimalLowercaseLetters then
    Result := Ord(c) - Ord('a') + 10
  else if c in HexadecimalUppercaseLetters then
    Result := Ord(c) - Ord('A') + 10
  else
    Result := 0;
end;


function HexToAscii(s: String): String;
var
  t, y, L: Integer;
  c: array[0..256] of Char;
begin
  L := (Length(s) div 2);
  for t := 0 to L - 1 do 
  begin
    y := (t * 2) + 1;
    c[t] := Char(HexDigitVal(s[y]) * 16 + HexDigitVal(s[y + 1]));
  end;
  c[L] := Chr(0);
  Result := String(c);
end;

function AsciiToHex(s: String): String;
var
  t: Integer;
  s2: String;
begin
  for t := 1 to Length(s) do 
  begin
    s2 := s2 + IntToHex(Ord(s[t]), 2);
  end;
  Result := s2;
end;



//-----------------------------------------------------------------------------
// GetIntValueFromResultString
//
// Retrieve an integer value from a result string, Formats that are valid
// include:
//
// VariableName: Value  - usual format for status results
// VariableName = Value  - usual format in ini files
// Label Name = Value    - labels names can contain spaces.
//-----------------------------------------------------------------------------
function GetIntValueFromResultString(VarName: String; ResultStrings: TStrings;
  DefVal: Integer): Integer;
var
  s: String;
begin
  s := GetValueFromResultString(VarName, ResultStrings);
  Result := StrToIntDef(s, DefVal);
end;

//-----------------------------------------------------------------------------
// GetValueFromResultString
//
// Retrieve a value from a result string, Formats that are valid include:
// VariableName: Value  - usual format for status results
// VariableName = Value  - usual format in ini files
// Label Name = Value    - labels names can contain spaces.
//-----------------------------------------------------------------------------
function GetValueFromResultString(VarName: String; ResultStrings: TStrings): String;
var
  label1, value1: String;
  len1, pos1, t, Count: Integer;
begin
  if not Assigned(ResultStrings) then 
  begin
    Result := 'NIL';
    Exit;
  end;

  Count := ResultStrings.Count;
  for t := 0 to Count - 1 do 
  begin
    len1 := Length(ResultStrings[t]);
    pos1 := Pos(':', ResultStrings[t]);
    if (pos1 = 0) then
      pos1 := Pos('=', ResultStrings[t]);
    // found a value to extract:
    if (pos1 > 0) then 
    begin
      Label1 := Copy(ResultStrings[t], 1, pos1 - 1);
      Value1 := Copy(ResultStrings[t], pos1 + 1, len1);

      if VarName = Label1 then 
      begin // found it!
        Result := Value1;
        Exit;
      end;
    end;
  end;
end;

function StrStrip(s: String): String;
var
  len, i: Integer;
begin
  len := Length(s);
  i := 1;
  while (len >= i) and ((s[i] = ' ') or (s[i] = #9)) do
    i := i + 1;
  if (i > len) then 
  begin
    Result := '';
    Exit;
  end;
  s := Copy(s, i, len);
  len := len - i + 1;
  i := len;
  while (i > 0) and ((s[i] = ' ') or (s[i] = #9)) do
    i := i - 1;
  Result := Copy(s, 1,i);
end;

function GetString(var Source: String; seperator: String): String;
var
  i, j, len: Integer;
begin
  //source:=StrStrip(source);
  len := Length(Source);
  i := 0;
  for j := 1 to len do
    if Pos(Source[j], seperator) > 0 then 
    begin
      i := j;
      Break;
    end;
  if (i > 0) then 
  begin
    Result := StrStrip(Copy(Source, 1,i - 1));
    Source := Copy(Source, i + 1,Length(Source) - i);
    //Source:=StrStrip(source); //???
  end
  else 
  begin
    Result := StrStrip(Source);
    Source := '';
  end;
end;

//------------------------------------------------------------------------------------------
// StrSplit
//   Given aString='Blah,Blah,Blah', splitChar=',', writes to OutStrings an Array
//   ie ('blah','blah','blah ) and returns the integer count of how many items are in
//   the resulting array, or -1 if more than MaxSplit items were found in the input 
//   string.
//
// XXX READ THESE NOTES! XXX
//
// XXX DOES NOT HANDLE QUOTING (YOU CAN'T HAVE A COMMA INSIDE QUOTES, AT LEAST NOT YET.) XXX
//
// XXX OutStrings array must be dimensioned to start at element ZERO,
//     if it starts at element 1, then you'll get exceptions XXX
//------------------------------------------------------------------------------------------
function StrSplit(const inString: String; const splitChar, quoteChar: Char;
  var OutStrings: array of String; MaxSplit: Integer): Integer;
var
  t, Len, SplitCounter: Integer;
  Ch: Char;
  inQuotes: Boolean;
begin
  inQuotes := False;
  Len := Length(inString);
  for t := Low(OutStrings) to High(OutStrings) do
  begin // clear array that is passed in!
    OutStrings[t] := ''; // Array
  end;

  SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

  for t := 1 to Len do 
  begin
    Ch := inString[t];
    if (Ch = splitChar) and (not inQuotes) then 
    begin
      Inc(SplitCounter);
      if SplitCounter > MaxSplit then 
      begin
        Result := -1; // Error!
        Exit;
      end;
    end 
    else 
    begin
      OutStrings[SplitCounter] := OutStrings[SplitCounter] + ch;
      if (ch = quoteChar) then
        inQuotes := not inQuotes;
    end;
  end;
  Inc(SplitCounter);
  Result := SplitCounter;
end;
// NEW 2004 WP
function StrSplitStrings( const inString:String; const splitChar,quoteChar:Char; OutStrings:TStrings):Integer;
var
  t,Len,SplitCounter:Integer;
  Ch:Char;
  inQuotes:Boolean;
  OutString:String;
begin
   inQuotes := false;
   Len := Length(inString);
   OutStrings.Clear;
   SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

   for t := 1 to Len do begin
        Ch := inString[t];
        if (Ch = splitChar) and (not inQuotes) then begin
                OutStrings.Add(OutString);
                OutString:='';
                Inc(SplitCounter);
        end else begin
          OutString := OutString + ch;
          if (ch = quoteChar) then
              inQuotes := not inQuotes;
        end;
   end;
   OutStrings.Add(OutString);
   Inc(SplitCounter);
   result := SplitCounter;
end;
//--end NEW--

end.

