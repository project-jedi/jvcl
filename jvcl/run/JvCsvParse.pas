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
function StrSplit(const InString: string; const SplitChar, QuoteChar: Char;
  var OutStrings: array of string; MaxSplit: Integer): Integer;
{new 2004}
function StrSplitStrings(const InString: string; const SplitChar, QuoteChar: Char; OutStrings: TStrings): Integer;

{ circa 1998-2001 classic functions }
function StrStrip(S: string): string; // Strip whitespace, carriage returns, linefeeds.
function GetString(var Source: string; const Separator: string): string;
// Iteratively split off a piece of a string. Modifies original string.
function PadString(const S: string; Len: Integer; PadChar: Char): string;
//procedure Gibble(var S: string); // Deprecated. With a name like Gibble, are you surprised?
function BuildPathName(const PathName, FileName: string): string;
function StrEatWhiteSpace(const S: string): string;
function HexToAscii(const S: string): string;
function AsciiToHex(const S: string): string;
function StripQuotes(const S1: string): string;

{ TStrings helper functions }
function GetIntValueFromResultString(const VarName: string; ResultStrings: TStrings;
  DefVal: Integer): Integer;
function GetValueFromResultString(const VarName: string; ResultStrings: TStrings): string;

{ Pascal Low Level PChar Functions }
function ValidNumericLiteral(S1: PChar): Boolean;
function ValidIntLiteral(S1: PChar): Boolean;
function ValidHexLiteral(S1: PChar): Boolean;
function HexPCharToInt(S1: PChar): Integer;
function ValidStringLiteral(S1: PChar): Boolean;
function StripPCharQuotes(S1: PChar): string;

function ValidIdentifier(S1: PChar): Boolean;
function EndChar(X: Char): Boolean;
procedure GetToken(S1, S2: PChar);
function IsExpressionKeyword(S1: PChar): Boolean;
function IsKeyword(S1: PChar): Boolean;
function ValidVarReference(S1: PChar): Boolean;
function GetParenthesis(S1, S2: PChar): Boolean;
procedure GetVarReference(S1, S2, SIdx: PChar);
procedure PCharEatWhiteChars(S1: PChar);

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

function ValidNumericLiteral(S1: PChar): Boolean;
var
  L, X, X1: Integer;
  DecimalFlag: Boolean;
begin
  L := StrLen(S1);
  DecimalFlag := False;
  X1 := 0;

  if L <= 0 then
  begin
    Result := False;
    Exit;
  end;

  { detect leading minus }
  if S1[0] = '-' then
    Inc(X1); // skip the minus, as it's okay as a leading character

  { Detect a decimal number or integer number }
  for X := X1 to L - 1 do
    if S1[X] = '.' then
    begin
      if DecimalFlag then
      begin
        Result := False; // two decimal places is invalid.
        Exit;
      end;
      DecimalFlag := True;
    end
    else
    if not (S1[X] in DigitSymbols) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }

function ValidIntLiteral(S1: PChar): Boolean;
var
  L, X, X1: Integer;
begin
  L := StrLen(S1);
  X1 := 0;
  if L <= 0 then
  begin
    Result := False;
    Exit;
  end;
  { detect leading minus }
  if S1[0] = '-' then
    Inc(X1); // skip the minus, as it's okay as a leading character

  { Detect a decimal number or integer number }
  for X := X1 to L - 1 do
    if not (S1[X] in DigitSymbols) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

{ Returns true for integer literals only, like -35 or 199, but not
  for values like '123.45' }

function ValidHexLiteral(S1: PChar): Boolean;
var
  L, X: Integer;
begin
  L := StrLen(S1);
  //  X1 := 0;

  { detect hex code type indicator }
  if (L < 2) or (S1[0] <> '$') then
  begin
    Result := False;
    Exit;
  end;

  { Detect hex digits }
  for X := 1 to L - 2 do
    if not (S1[X] in HexadecimalSymbols) then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

function HexPCharToInt(S1: PChar): Integer;
var
  X, L: Integer;
  Digit, Val: Integer;
begin
  L := StrLen(S1);
  if (L < 2) or (L > 9) then
    raise EJVCLException.CreateRes(@RsEInvalidHexLiteral);
  if S1[0] <> '$' then
    raise EJVCLException.CreateRes(@RsEInvalidHexLiteral);
  Val := 0;
  for X := 1 to L - 2 do
  begin
    Val := Val * 16; { shift right four bits at a time }
    if S1[X] in DigitSymbols then
      Digit := Ord(S1[X]) - Ord('0')
    else
    if S1[X] in HexadecimalLowercaseLetters then
      Digit := Ord(S1[X]) - Ord('a') + 10
    else
    if S1[X] in HexadecimalUppercaseLetters then
      Digit := Ord(S1[X]) - Ord('A') + 10
    else
      raise EJVCLException.CreateRes(@RsEInvalidHexLiteral);
    Val := Val + Digit;
  end;
  Result := Val;
end;

function ValidStringLiteral(S1: PChar): Boolean;
begin
  Result := (S1[0] = '"') and (S1[StrLen(S1) - 1] = '"');
end;

{ Strip quotes and return as a real Delphi String }

function StripQuotes(const S1: string): string;
begin
  if ValidStringLiteral(PChar(S1)) then
    Result := Copy(S1, 2, Length(S1) - 2)
  else
    Result := S1;
end;

// This function is limited to 1 to 254 characters:

function StripPCharQuotes(S1: PChar): string;
var
  TempBuf: array [0..256] of Char;
  L: Integer;
begin
  L := StrLen(S1);
  if L > 255 then
    L := 255;
  if ValidStringLiteral(S1) then
    StrLCopy(TempBuf, S1 + 1, L - 2);
  Result := string(TempBuf);
end;

{ Prevent confusion between expression-keywords and variable identifiers }

function IsExpressionKeyword(S1: PChar): Boolean;
begin
  if StrIComp(S1, 'AND') = 0 then
    Result := True
  else
  if StrIComp(S1, 'OR') = 0 then
    Result := True
  else
  if StrIComp(S1, 'XOR') = 0 then
    Result := True
  else
  if StrIComp(S1, 'NOT') = 0 then
    Result := True
  else
  if StrIComp(S1, 'DIV') = 0 then
    Result := True
  else
  if StrIComp(S1, 'SHR') = 0 then
    Result := True
  else
  if StrIComp(S1, 'SHL') = 0 then
    Result := True
  else
    Result := False;
end;

function IsKeyword(S1: PChar): Boolean;
begin
  Result := (StrIComp(S1, 'SET') = 0) or (StrIComp(S1, 'LET') = 0) or
    (StrIComp(S1, 'DIM') = 0) or (StrIComp(S1, 'ARRAYCOPY') = 0) or
    (StrIComp(S1, 'STRCOPY') = 0) or (StrIComp(S1, 'STRPAD') = 0) or
    (StrIComp(S1, 'STRSTRIP') = 0) or (StrIComp(S1, 'END') = 0) or
    (StrIComp(S1, 'INC') = 0) or (StrIComp(S1, 'DEC') = 0) or
    (StrIComp(S1, 'PARAM') = 0) or (StrIComp(S1, 'JUMP') = 0) or
    (StrIComp(S1, 'SLEEP') = 0) or (StrIComp(S1, 'GOTO') = 0) or
    (StrIComp(S1, 'IF') = 0) or (StrIComp(S1, 'CALL') = 0) or
    (StrIComp(S1, 'STOP') = 0) or (StrIComp(S1, 'CONST') = 0);
end;

{ ValidIdentifier:

  Valid identifier must start with a-z or A-Z or _, and can have alphanumeric or underscore(_)
  as subsequent characters, no spaces, punctuation, or other characters allowed.  Same rules as
  most programming languages, Cobol being one particularly nasty exception! <grin>

    --Warren.
}

function ValidIdentifier(S1: PChar): Boolean;
var
  X, Y: Integer;
  Pass: Boolean;
begin
  Pass := True;

  if IsExpressionKeyword(S1) then
  begin
    Result := False;
    Exit;
  end;

  X := StrLen(S1);
  if (X < 1) or (X > 32) then
  begin
    Result := False;
    Exit;
  end;

  if not (S1[0] in IdentifierFirstSymbols) then
    Pass := False;

  if Pass and (X > 1) then
    for Y := 1 to X - 1 do
      if not (S1[Y] in IdentifierSymbols) then
      begin
        Pass := False;
        Result := Pass;
        Exit;
      end;

  Result := Pass;
end;

function EndChar(X: Char): Boolean;
begin
  Result := (X = ',') or (X = ';') or (X = ':') or (X = '[') or (X = ']') or
    (X = '(') or (X = ')') or (X = '#') or (X = '<') or (X = '>') or (X = '=') or
    (X = '*') or (X = '/') or (X = '+') or (X = Chr(0));
end;

procedure GetToken(S1, S2: PChar);
var
  W, X, Y: Integer;
  InQuotes: Boolean;
begin
  X := 0;
  W := 0;

  { Empty in, Empty Out }
  if StrLen(S1) = 0 then
    S2[0] := Chr(0);

  InQuotes := False;

  { skip leading space }
  while (S1[X] = ' ') or (S1[X] = Tab) do
    Inc(X);

  while True do
  begin
    if EndChar(S1[X]) and not InQuotes then
    begin
      { return punctuation one symbol at a time }
      if W < 1 then
      begin
        S2[W] := S1[X];
        Inc(W);
        Inc(X);
      end;
      Break;
    end;

    if S1[X] = '"' then
      InQuotes := not InQuotes;

    { Break if space found and not in quotes }
    if (S1[X] = ' ') and not InQuotes then
      Break
    else
    begin
      S2[W] := S1[X];
      Inc(W);
    end;

    Inc(X);
  end;
  // S2[X] := Chr(0);

  { detect not-equal, less-than-or-equal and greater-than-or-equal operators }
  if W = 1 then
    if (S2[0] = '<') and (S1[X] = '>') then
    begin
      S2[W] := '>';
      Inc(X);
      Inc(W); // char literal
    end
    else
    if (S2[0] = '<') and (S1[X] = '=') then
    begin
      S2[W] := '=';
      Inc(X);
      Inc(W);
    end
    else
    if (S2[0] = '>') and (S1[X] = '=') then
    begin
      S2[W] := '=';
      Inc(X);
      Inc(W);
    end;

  { remove token from initial buffer, move to second buffer }
  Y := Integer(StrLen(S1)) - X;
  if Y > 0 then
    StrLCopy(S1, S1 + X, Y) { copy remaining characters }
  else
    S1[0] := Chr(0); { just erase all of old string }

  S2[W] := Chr(0); { Terminate new string }
  Inc(TokenCount);
end;

function StrEatWhiteSpace(const S: string): string;
var
  Buf: array [0..1024] of Char;
begin
  if Length(S) > 1024 then
  begin
    Result := S;
    Exit;
  end;
  StrCopy(Buf, PChar(S));
  PCharEatWhiteChars(Buf);
  Result := string(Buf);
end;

{ strip whitespace from pchar - space or tab }

procedure PCharEatWhiteChars(S1: PChar);
var
  T, U, L: Integer;
begin
  L := StrLen(S1);
  // U := L;
  if L <= 0 then
    Exit;
  { skip spaces starting at the beginning }
  for T := 0 to L do
    if (S1[T] <> ' ') and (S1[T] <> Tab) then
      Break;
  { skip spaces starting at the end }
  for U := L - 1 downto T do
    if (S1[U] <> ' ') and (S1[U] <> Tab) then
      Break;
  if (T > 0) or (U < L - 1) then
    if T > U then  // was T>=U (test me!)
      S1[0] := Chr(0)
    else
      StrLCopy(S1, S1 + T, (U - T) + 1);
end;

function GetParenthesis(S1, S2: PChar): Boolean;
var
  Token, TempBuf: array [0..128] of Char;
  Brackets: Integer;
begin
  { make temporary copy of S1, check for parenthesis }
  StrCopy(TempBuf, S1);
  GetToken(TempBuf, S2);
  if StrComp(S2, '(') = 0 then
  begin
    Brackets := 1;
    S2[0] := Chr(0);
    repeat
      GetToken(TempBuf, Token);
      if StrComp(Token, ')') = 0 then
        Dec(Brackets);
      if Brackets > 0 then
      begin
        StrCat(S2, Token);
        StrCat(S2, ' ');
      end;
      if StrComp(Token, '(') = 0 then
        Inc(Brackets);
    until (StrLen(S1) = 0) or (Brackets = 0);
    if Brackets <> 0 then
    begin
      S2[0] := Chr(0);
      Result := False;
      Exit;
    end;
    StrCopy(S1, TempBuf); { remainder back into S1 }
    Result := True;
  end
  else
  begin { not parenthesis }
    S2[0] := Chr(0);
    Result := False;
    Exit;
  end;
end;

{ Gets a single token like ABC, or gets ABC[X] type reference if present }

procedure GetVarReference(S1, S2, SIdx: PChar);
var
  TempBuf: array [0..128] of Char;
  Brackets: Integer;
begin
  GetToken(S1, S2);
  SIdx[0] := Chr(0);
  PCharEatWhiteChars(S1);
  if S1[0] = '[' then
  begin
    Brackets := 0;
    repeat
      GetToken(S1, TempBuf);
      StrCat(SIdx, TempBuf);
      if StrComp(TempBuf, ']') = 0 then
        Dec(Brackets);
      if StrComp(TempBuf, '[') = 0 then
        Inc(Brackets);

      if StrLen(S1) = 0 then
        Break;
    until Brackets <= 0;

    { Remove outermost brackets }
    StrLCopy(SIdx, SIdx + 1, StrLen(SIdx) - 2);
  end;
end;

{ Expects ABC or ABC[X] type of reference }

function ValidVarReference(S1: PChar): Boolean;
var
  Len1: Integer;
  TempBuf1, TempBuf2: array [0..128] of Char;
begin
  StrCopy(S1, TempBuf1);
  GetToken(TempBuf1, TempBuf2);
  if StrLen(TempBuf1) = 0 then
    Result := ValidIdentifier(S1)
  else
  begin
    Len1 := StrLen(TempBuf1);
    if (TempBuf1[0] = '[') and (TempBuf1[Len1 - 1] = ']') then
      Result := ValidIdentifier(S1)
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

function PadString(const S: string; Len: Integer; PadChar: Char): string;
begin
  Result := S;
  while Length(Result) < Len do
    Result := Result + PadChar;
end;

{ Encoding function named in honor of Dennis Forbes' favourite word }
{procedure Gibble(var S: string);
var
 I, L, c1: Integer;
 lo, hi: Byte;
 X: array [0..255] of Char;
begin
 L := Length(S);
 for I:= 0 to L-1 do
 begin
     c1 := Ord(S[I+1] );
     if (c1  >= 32 ) AND (c1 <= 231) then
     begin
        c1 := c1 - 32;
        lo := (c1 MOD 25);
        hi := c1 div 25;
        lo := 24-lo;
        c1 := ((hi*25)+lo ) +32;
        X[I] := Chr(c1);
     end
     else
        X[I] := Chr(c1);
 end;
 X[L] := Chr(0);
 S := String(X);
end;
 }

function BuildPathName(const PathName, FileName: string): string;
var
  L: Integer;
begin
  L := Length(PathName);
  if L = 0 then
    Result := FileName
  else
  if PathName[L] = '\' then
    Result := PathName + FileName
  else
    Result := PathName + '\' + FileName;
end;

function HexDigitVal(C: Char): Integer;
begin
  if C in DigitSymbols then
    Result := Ord(C) - Ord('0')
  else
  if C in HexadecimalLowercaseLetters then
    Result := Ord(C) - Ord('a') + 10
  else
  if C in HexadecimalUppercaseLetters then
    Result := Ord(C) - Ord('A') + 10
  else
    Result := 0;
end;

function HexToAscii(const S: string): string;
var
  I, Y, L: Integer;
  C: array [0..256] of Char;
begin
  L := Length(S) div 2;
  for I := 0 to L - 1 do
  begin
    Y := (I * 2) + 1;
    C[I] := Char(HexDigitVal(S[Y]) * 16 + HexDigitVal(S[Y + 1]));
  end;
  C[L] := Chr(0);
  Result := C;
end;

function AsciiToHex(const S: string): string;
var
  I: Integer;
  S2: string;
begin
  for I := 1 to Length(S) do
    S2 := S2 + IntToHex(Ord(S[I]), 2);
  Result := S2;
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

function GetIntValueFromResultString(const VarName: string;
  ResultStrings: TStrings; DefVal: Integer): Integer;
var
  S: string;
begin
  S := GetValueFromResultString(VarName, ResultStrings);
  Result := StrToIntDef(S, DefVal);
end;

//-----------------------------------------------------------------------------
// GetValueFromResultString
//
// Retrieve a value from a result string, Formats that are valid include:
// VariableName: Value  - usual format for status results
// VariableName = Value  - usual format in ini files
// Label Name = Value    - labels names can contain spaces.
//-----------------------------------------------------------------------------

function GetValueFromResultString(const VarName: string; ResultStrings: TStrings): string;
var
  Label1, Value1: string;
  Len1, Pos1, I, Count: Integer;
begin
  if not Assigned(ResultStrings) then
  begin
    Result := 'NIL';
    Exit;
  end;

  Count := ResultStrings.Count;
  for I := 0 to Count - 1 do
  begin
    Len1 := Length(ResultStrings[I]);
    Pos1 := Pos(':', ResultStrings[I]);
    if Pos1 = 0 then
      Pos1 := Pos('=', ResultStrings[I]);
    // found a value to extract:
    if Pos1 > 0 then
    begin
      Label1 := Copy(ResultStrings[I], 1, Pos1 - 1);
      Value1 := Copy(ResultStrings[I], Pos1 + 1, Len1);

      if VarName = Label1 then
      begin // found it!
        Result := Value1;
        Exit;
      end;
    end;
  end;
end;

function StrStrip(S: string): string;
var
  Len, I: Integer;
begin
  Len := Length(S);
  I := 1;
  while (Len >= I) and ((S[I] = ' ') or (S[I] = Tab)) do
    I := I + 1;
  if I > Len then
  begin
    Result := '';
    Exit;
  end;
  S := Copy(S, I, Len);
  Len := Len - I + 1;
  I := Len;
  while (I > 0) and ((S[I] = ' ') or (S[I] = Tab)) do
    I := I - 1;
  Result := Copy(S, 1, I);
end;

function GetString(var Source: string; const Separator: string): string;
var
  I, J, Len: Integer;
begin
  //Source := StrStrip(Source);
  Len := Length(Source);
  I := 0;
  for J := 1 to Len do
    if Pos(Source[J], Separator) > 0 then
    begin
      I := J;
      Break;
    end;
  if I > 0 then
  begin
    Result := StrStrip(Copy(Source, 1, I - 1));
    Source := Copy(Source, I + 1, Length(Source) - I);
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
//   Given aString='Blah,Blah,Blah', SplitChar=',', writes to OutStrings an Array
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

function StrSplit(const InString: string; const SplitChar, QuoteChar: Char;
  var OutStrings: array of string; MaxSplit: Integer): Integer;
var
  I, Len, SplitCounter: Integer;
  Ch: Char;
  InQuotes: Boolean;
begin
  InQuotes := False;
  Len := Length(InString);
  for I := Low(OutStrings) to High(OutStrings) do // clear array that is passed in!
    OutStrings[I] := '';

  SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

  for I := 1 to Len do
  begin
    Ch := InString[I];
    if (Ch = SplitChar) and not InQuotes then
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
      OutStrings[SplitCounter] := OutStrings[SplitCounter] + Ch;
      if Ch = QuoteChar then
        InQuotes := not InQuotes;
    end;
  end;
  Inc(SplitCounter);
  Result := SplitCounter;
end;

// NEW 2004 WP

function StrSplitStrings(const InString: string; const SplitChar, QuoteChar: Char; OutStrings: TStrings): Integer;
var
  I, Len, SplitCounter: Integer;
  Ch: Char;
  InQuotes: Boolean;
  OutString: string;
begin
  InQuotes := False;
  Len := Length(InString);
  OutStrings.Clear;
  SplitCounter := 0; // ALWAYS ASSUME THAT ZERO IS VALID IN THE OUTGOING ARRAY.

  for I := 1 to Len do
  begin
    Ch := InString[I];
    if (Ch = SplitChar) and not InQuotes then
    begin
      OutStrings.Add(OutString);
      OutString := '';
      Inc(SplitCounter);
    end
    else
    begin
      OutString := OutString + Ch;
      if Ch = QuoteChar then
        InQuotes := not InQuotes;
    end;
  end;
  OutStrings.Add(OutString);
  Inc(SplitCounter);
  Result := SplitCounter;
end;

//--end NEW--

end.

