{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrUtil.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):
  Roman Kovbasiouk <roko@users.sourceforge.net>

Last Modified: 2003-03-10

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : String utilities

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStrUtil;

interface

uses
  SysUtils, Classes;

const
  Separators: set of Char =
    [#0, ' ', '-', #13, #10, '.', ',', '/', '\', '#', '"', '''',
    ':', '+', '%', '*', '(', ')', ';', '=', '{', '}', '[', ']', '{', '}', '<', '>'];

{$IFDEF DELPHI}
type
  TSetOfChar = set of Char;
{$ENDIF DELPHI}
{$IFDEF BCB}
type
  TSetOfChar = string;
{$ENDIF BCB}

function FindNotBlankCharPos(const S: string): Integer;
function AnsiChangeCase(const S: string): string;
{ GetWordOnPos returns Word from string, S, on the cursor position, P}
function GetWordOnPos(const S: string; const P: Integer): string;
{ GetWordOnPosEx working like GetWordOnPos function, but
  also returns Word position in iBeg, iEnd variables }
function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string;
{ SubWord returns next Word from string, P, and offsets Pointer to the end of Word, P2 }
function SubWord(P: PChar; var P2: PChar): string;
{ NumberByWord returns the text representation of
  the number, N, in normal russian language. Was typed from Monitor magazine }
function NumberByWord(const N: Longint): string;
{ GetLineByPos returns the Line number, there
  the symbol Pos is pointed. Lines separated with #13 symbol }
function GetLineByPos(const S: string; const Pos: Integer): Integer;
{ GetXYByPos is same to previous function, but returns X position in line too}
procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
function Cmp(const S1, S2: string): Boolean;
{ StringCat add S2 string to S1 and returns this string }
function StringCat(var S1: string; S2: string): string;
{ Spaces returns string consists on N space chars }
function Spaces(const N: Integer): string;
{ AddSpaces add spaces to string, S, if it Length is smaller than N }
function AddSpaces(const S: string; const N: Integer): string;
{ HasChar returns True, if char, Ch, contains in string, S }
function HasChar(const Ch: Char; const S: string): Boolean;
function HasAnyChar(const Chars: string; const S: string): Boolean;
function CountOfChar(const Ch: Char; const S: string): Integer;
{ CurrencyToStr format currency, Cur, using ffCurrency float format}
function CurrencyToStr(const Cur: currency): string;
{ SubStr returns substring from string, S,
  separated with Separator string}
function SubStr(const S: string; const Index: Integer; const Separator: string): string;
{ SubStrEnd same to previous function but Index numerated
  from the end of string }
function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
{ GetSubStr is full equal to SubStr function
  - only for compatibility only - don't use }
function GetSubStr(const S: string; const Index: Integer; const Separator: Char): string; {$IFDEF COMPILER6_UP} deprecated; {$ENDIF}

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
{ ReplaceSokr1 is full equal to ReplaceString function
  - for compatibility only - don't use }
function ReplaceSokr1(S: string; const Word, Phrase: string): string; {$IFDEF COMPILER6_UP} deprecated; {$ENDIF}
{ ReplaceAllStrings searches for all substrings, Words,
  in a string, S, and replaces them with Phrases with the same Index.}
function ReplaceAllStrings(S: string; Words, Phrases: TStrings): string;
{ ReplaceStrings searches the Word in a string, S, on PosBeg position,
  in the list, Words, and if founds, replaces this Word
  with string from another list, Phrases, with the same Index,
  and then update NewSelStart variable }
function ReplaceStrings(S: string; PosBeg, Len: Integer; Words, Phrases: TStrings; var NewSelStart: Integer): string;
{ CountOfLines calculates the lines count in a string, S,
  each line must be separated from another with CrLf sequence }
function CountOfLines(const S: string): Integer;
{ DeleteEmptyLines deletes all empty lines from strings, Ss.
  Lines contained only spaces also deletes. }
procedure DeleteEmptyLines(Ss: TStrings);
{ SQLAddWhere addes or modifies existing where-statement, where,
  to the strings, SQL.
  Note: If strings SQL allready contains where-statement,
  it must be started on the begining of any line }
procedure SQLAddWhere(SQL: TStrings; const Where: string);

function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean;
function LoadTextFile(const FileName: TFileName): string;
procedure SaveTextFile(const FileName: TFileName; const Source: string);
{ ConcatSep concatenate S and S2 strings with Separator.
  if S = '', separator isn't included }
function ConcatSep(const S, S2, Separator: string): string;
{ ConcatLeftSep is same to previous function, but
  strings concatenate right to left }
function ConcatLeftSep(const S, S2, Separator: string): string;
{ MinimizeString trunactes long string, S, and appends
  '...' symbols, if Length of S is more than MaxLen }
function MinimizeString(const S: string; const MaxLen: Integer): string;
{ Next 5 function for russian chars transliterating.
  This functions are needed because Oem2Ansi and Ansi2Oem functions
  sometimes works sucks }
procedure Dos2Win(var S: string);
procedure Win2Dos(var S: string);
function Dos2WinRes(const S: string): string;
function Win2DosRes(const S: string): string;
function Win2Koi(const S: string): string;
{ function LastDate for russian users only }
//  { returns date relative to current date: 'два дн€ назад' }
function LastDate(const Dat: TDateTime): string;
function DefStr(const S: string; Default: string): string;
function StringEndsWith(const Str, SubStr: string): Boolean;

function ExtractFilePath2(const FileName: string): string;
{$IFNDEF COMPILER3_UP}
function AnsiStrIComp(S1, S2: PChar): Integer;
function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
{$ENDIF COMPILER3_UP}

implementation

{$IFNDEF COMPILER3_UP}

function AnsiStrIComp(S1, S2: PChar): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;

{$ENDIF COMPILER3_UP}

function FindNotBlankCharPos(const S: string): Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 1 to Length(S) do
    if S[I] <> ' ' then
      Exit;
end;

// (rom) reimplemented

function AnsiChangeCase(const S: string): string;
var
  I: Integer;
  Up: string;
  Down: string;
begin
  Result := S;
  Up := AnsiUpperCase(S);
  Down := AnsiLowerCase(S);
  for I := 1 to Length(Result) do
    if Result[I] = Up[I] then
      Result[I] := Down[I]
    else
      Result[I] := Up[I];
end;

function GetWordOnPos(const S: string; const P: Integer): string;
var
  I, Beg: Integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  for I := P downto 1 do
    // add by patofan
    if (S[I] in Separators) {$IFDEF DELPHI3_UP} and (StrByteType(PChar(s), I - 1) <> mbTrailByte) {$ENDIF} then
      // ending add by patofan
      Break;
  Beg := I + 1;
  for I := P to Length(S) do
    // add by patofan
    if (S[I] in Separators) {$IFDEF DELPHI3_UP} and (StrByteType(PChar(s), I - 1) <> mbTrailByte) {$ENDIF} then
      // ending add by patofan
      Break;
  if I > Beg then
    Result := Copy(S, Beg, I - Beg)
  else
    Result := S[P];
end;

// (rom) DELPHI3_UP or COMPILER3_UP?

function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then
    Exit;
  iBeg := P;
  if P > 1 then
    // add by patofan
    if (S[P] in Separators) {$IFDEF Delphi3_Up} and (StrByteType(PChar(s), p - 1) <> mbTrailByte) {$ENDIF} then
      // ending add by patofan
      if (P < 1) or ((P - 1 > 0) and (S[P - 1] in Separators)) then
        Inc(iBeg)
      else
      if not ((P - 1 > 0) and (S[P - 1] in Separators)) then
        Dec(iBeg);
  if iBeg > Length(S) then
    iBeg := P - 1; {+RWare}
  while iBeg >= 1 do
    // add by patofan
    if (S[iBeg] in Separators) {$IFDEF DELPHI3_UP} and (StrByteType(PChar(s), iBeg - 1) <> mbTrailByte) {$ENDIF} then
    // ending add by patofan
      Break
    else
      Dec(iBeg);
  Inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if (S[iEnd] in Separators) {$IFDEF COMPILER3_UP} and (StrByteType(PChar(s), iEnd - 1) <> mbTrailByte) {$ENDIF}
    then
      Break
    else
      Inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg)
  else
    Result := S[P];
end;

function SubWord(P: PChar; var P2: PChar): string;
var
  I: Integer;
begin
  I := 0;
  while not (P[I] in Separators) do
    Inc(I);
  SetString(Result, P, I);
  P2 := P + I;
end;

function NumberByWord(const N: Longint): string;
const
  Ten: array [0..9] of string =
    ('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine');
  Hun: array [1..9] of string =
    ('onehundred', 'twohundred', 'threehundred', 'fourhundred', 'fivehundred',
     'sixhundred', 'sevenhundred', 'eighthundred', 'ninehundred');
  OnTen: array [10..19] of string =
    ('ten', 'eleven', 'twelve', 'thirteen', 'fourteen',
     'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen');
  HunIn: array [2..9] of string =
    ('twothousand', 'threethousand', 'fourthousand', 'fivethousand',
     'sixthousand', 'seventhousand', 'eightthousand', 'ninethousand');
var
  StrVsp: string;
  NumStr: string;
  StrVsp2: string;
  I: Byte;

  function IndNumber(Stri: string; Place: Byte): Byte;
  begin
    IndNumber := Ord(Stri[Place]) - 48;
  end;

  function Back(Stri: string): Longint;
  var
    Code: Integer;
    LI: Longint;
  begin
    Result := 0;
    Val(Stri, LI, Code);
    if Code = 0 then
      Result := LI;
  end;

begin
  NumStr := IntToStr(N);
  if Length(NumStr) > 9 then
  begin
    Result := '*****';
    Exit;
  end;
  case Length(NumStr) of
    1:
      StrVsp := Ten[N];
    2: case NumStr[1] of
        '1':
          StrVsp := OnTen[N];
        '0':
          StrVsp := NumberByWord(IndNumber(NumStr, 2));
        '2'..'9':
          begin
            StrVsp := HunIn[IndNumber(NumStr, 1)];
            if NumStr[2] <> '0' then
              StrVsp := StrVsp + ' ' + NumberByWord(IndNumber(NumStr, 2));
          end;
      end;
    3:
      begin
        StrVsp := Hun[IndNumber(NumStr, 1)];
        StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 2)));
      end;
    4:
      begin
        StrVsp := Ten[IndNumber(NumStr, 1)];
        // (rom) needs translation
        case NumStr[1] of
          '1':
            StrVsp := 'одна тыс€ча';
          '2':
            StrVsp := 'две тыс€чи';
          '3', '4':
            StrVsp := StrVsp + ' тыс€чи';
          '5'..'9':
            StrVsp := StrVsp + ' тыс€ч';
        end;
        StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 3)));
      end;
    5:
      begin
        StrVsp2 := NumberByWord(Back(Copy(NumStr, 1, 2)));
        I := Pos(' два', StrVsp2);
        if Pos(' два', StrVsp2) = I then
          I := 0;
        if I <> 0 then
          StrVsp2[I + 3] := 'e';
        I := Pos(' один', StrVsp2);
        if Pos(' одинн', StrVsp2) = I then
          I := 0;
        if I <> 0 then
        begin
          StrVsp2[I + 3] := 'н';
          StrVsp2[I + 4] := 'а';
        end;
        if NumStr[1] <> '1' then
          case NumStr[2] of
            '1':
              StrVsp := ' тыс€ча ';
            '2'..'4':
              StrVsp := ' тыс€чи ';
            '5'..'9':
              StrVsp := ' тыс€ч ';
          end
        else
          StrVsp := ' тыс€ч ';
        StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 3, 3)));
      end;
    6:
      begin
        StrVsp2 := NumberByWord(Back(Copy(NumStr, 1, 3)));
        I := Pos(' два', StrVsp2);
        if Pos(' двад', StrVsp2) = I then
          I := 0;
        if I <> 0 then
          StrVsp2[I + 3] := 'е';
        I := Pos(' один', Strvsp2);
        if Pos(' одинн', StrVsp2) = I then
          I := 0;
        if I <> 0 then
        begin
          StrVsp2[I + 3] := 'н';
          StrVsp2[I + 4] := 'а';
        end;
        if NumStr[2] <> '1' then
          case NumStr[3] of
            '1':
              StrVsp := ' тыс€ча ';
            '2'..'4':
              StrVsp := ' тыс€чи ';
            '5'..'9':
              StrVsp := ' тыс€ч ';
          end
        else
          StrVsp := ' тыс€ч ';
        StrVsp := StrVsp2 + StrVsp + NumberByWord(Back(Copy(NumStr, 4, 3)));
      end;
    7:
      begin
        StrVsp := Ten[IndNumber(NumStr, 1)];
        case NumStr[1] of
          '1':
            StrVsp := 'один миллион';
          '2'..'4':
            StrVsp := StrVsp + ' миллиона';
          '5'..'9':
            StrVsp := StrVsp + ' миллионов';
        end;
        StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 2, 6)));
      end;
    8:
      begin
        StrVsp := NumberByWord(Back(Copy(NumStr, 1, 2)));
        StrVsp := StrVsp + ' миллион';
        if NumStr[1] <> '1' then
          case NumStr[2] of
            '2'..'4':
              StrVsp := StrVsp + 'а';
            '0', '5'..'9':
              StrVsp := StrVsp + 'ов';
          end
        else
          StrVsp := StrVsp + 'ов';
        StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 3, 6)));
      end;
    9:
      begin
        StrVsp := NumberByWord(Back(Copy(Numstr, 1, 3)));
        StrVsp := StrVsp + ' миллион';
        if NumStr[2] <> '1' then
          case NumStr[3] of
            '2'..'4':
              StrVsp := StrVsp + 'а';
            '0', '5'..'9':
              StrVsp := StrVsp + 'ов';
          end
        else
          StrVsp := StrVsp + 'ов';
        StrVsp := StrVsp + ' ' + NumberByWord(Back(Copy(NumStr, 4, 6)));
      end;
  end;
  if (Length(StrVsp) > 4) and (Copy(StrVsp, Length(StrVsp) - 3, 4) = Ten[0]) then
    StrVsp := Copy(StrVsp, 1, Length(StrVsp) - 4);
  Result := StrVsp;
end;

function GetLineByPos(const S: string; const Pos: Integer): Integer;
var
  I: Integer;
begin
  if Length(S) < Pos then
    Result := -1
  else
  begin
    I := 1;
    Result := 0;
    while I <= Pos do
    begin
      if S[I] = #13 then
        Inc(Result);
      Inc(I);
    end;
  end;
end;

procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
var
  I, iB: Integer;
begin
  X := -1;
  Y := -1;
  iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then
  begin
    I := 1;
    Y := 0;
    while I <= Pos do
    begin
      if S[I] = #13 then
      begin
        Inc(Y);
        iB := I + 1;
      end;
      Inc(I);
    end;
    X := Pos - iB;
  end;
end;

function Cmp(const S1, S2: string): Boolean;
begin
  Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0;
end;

function StringCat(var S1: string; S2: string): string;
begin
  S1 := S1 + S2;
  Result := S1;
end;

{ Spaces returns string consists on N space chars }

function Spaces(const N: Integer): string;
begin
  // (rom) reimplemented
  Result := AddSpaces('', N);
end;

function AddSpaces(const S: string; const N: Integer): string;
begin
  // (rom) SLOOOOW implementation
  Result := S;
  while Length(Result) < N do
    Result := Result + ' ';
end;

{ HasChar returns True, if char, Ch, contains in string, S }

function HasChar(const Ch: Char; const S: string): Boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function HasAnyChar(const Chars: string; const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(Chars) do
    if HasChar(Chars[I], S) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function CountOfChar(const Ch: Char; const S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if S[I] = Ch then
      Inc(Result);
end;

function CurrencyToStr(const Cur: currency): string;
begin
  Result := CurrToStrF(Cur, ffCurrency, CurrencyDecimals)
end;

function GetSubStr(const S: string; const Index: Integer; const Separator: Char): string;
begin
  Result := SubStr(S, Index, Separator);
end;

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
{ Returns a substring. Substrings are divided by Sep character [translated] }
var
  I: Integer;
  pB, pE: PChar;
begin
  Result := '';
  if ((Index < 0) or ((Index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then
    Exit;
  pB := PChar(S);
  for I := 1 to Index do
  begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then
      Exit;
    pB := pB + Length(Separator);
    if pB[0] = #0 then
      Exit;
  end;
  pE := StrPos(pB + 1, PChar(Separator));
  if pE = nil then
    pE := PChar(S) + Length(S);
  if not (AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE - pB);
end;

function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
var
  MaxIndex: Integer;
  pB: PChar;
begin
// Not optimal implementation [translated]
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do
  begin
    Inc(MaxIndex);
    pB := StrPos(pB + Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - Index, Separator);
end;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }

function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
var
  LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do
  begin
    Sm := P - PChar(S);
    S := Copy(S, 1, Sm) + NewPattern + Copy(S, Sm + LW + 1, Length(S));
    P := StrPos(PChar(S) + Sm + Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function ReplaceSokr1(S: string; const Word, Phrase: string): string;
begin
  Result := ReplaceString(S, Word, Phrase);
end;

{
    with memEdit do begin
      Text := ReplaceStrings(Text, SelStart+1, SelLength, memWords.Lines, memPhrases.Lines, NewSelStart);
      SelStart := NewSelStart-1;
    end; }

function ReplaceStrings(S: string; PosBeg, Len: Integer; Words, Phrases: TStrings;
  var NewSelStart: Integer): string;
var
  I, Beg, Ent, LS, F: Integer;
  Word: string;
begin
  NewSelStart := PosBeg;
  Result := S;
  LS := Length(S);
  if Len = 0 then
  begin
    if PosBeg < 1 then
      Exit;
    if PosBeg = 1 then
      PosBeg := 2;
    for I := PosBeg - 1 downto 1 do
      if S[I] in Separators then
        Break;
    Beg := I + 1;
    for Ent := PosBeg to LS do
      if S[Ent] in Separators then
        Break;
    if Ent > Beg then
      Word := Copy(S, Beg, Ent - Beg)
    else
      Word := S[PosBeg];
  end
  else
  begin
    Word := Copy(S, PosBeg, Len);
    Beg := PosBeg;
    Ent := PosBeg + Len;
  end;
  if Word = '' then
    Exit;
  F := Words.IndexOf(Word);
  if (F > -1) and (F < Phrases.Count) then
  begin
    Result := Copy(S, 1, Beg - 1) + Phrases[F] + Copy(S, Ent, LS);
    NewSelStart := Beg + Length(Phrases[F]);
  end;
end;

{
    with memEdit do
      Text := ReplaceAllStrings(Text, memWords.Lines, memPhrases.Lines);
}

function ReplaceAllStrings(S: string; Words, Phrases: TStrings): string;
var
  I, LW: Integer;
  P: PChar;
  Sm: Integer;
begin
  for I := 0 to Words.Count - 1 do
  begin
    LW := Length(Words[I]);
    P := StrPos(PChar(S), PChar(Words[I]));
    while P <> nil do
    begin
      Sm := P - PChar(S);
      S := Copy(S, 1, Sm) + Phrases[I] + Copy(S, Sm + LW + 1, Length(S));
      P := StrPos(PChar(S) + Sm + Length(Phrases[I]), PChar(Words[I]));
    end;
  end;
  Result := S;
end;

// Todo: optimize this
function CountOfLines(const S: string): Integer;
begin
  with TStringList.Create do
  try
    Text := S;
    Result := Count;
  finally
    Free;
  end;
end;

procedure DeleteEmptyLines(Ss: TStrings);
var
  I: Integer;
begin
  I := 0;
  while I < Ss.Count do
    if Trim(Ss[I]) = '' then
      Ss.Delete(I)
    else
      Inc(I);
end;

procedure SQLAddWhere(SQL: TStrings; const Where: string);
var
  I, J: Integer;
begin
  J := SQL.Count - 1;
  for I := 0 to SQL.Count - 1 do
    // (rom) does this always work? Think of a fieldname "grouporder"
    if StrLIComp(PChar(SQL[I]), 'where ', 6) = 0 then
    begin
      J := I + 1;
      while J < SQL.Count do
      begin
        if (StrLIComp(PChar(SQL[J]), 'order ', 6) = 0) or
          (StrLIComp(PChar(SQL[J]), 'group ', 6) = 0) then
          Break;
        Inc(J);
      end;
    end;
  SQL.Insert(J, 'and ' + Where);
end;

function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean;
begin
  {$IFDEF DELPHI}
  Result := Ch in SetOfChar;
  {$ENDIF DELPHI}
  {$IFDEF BCB}
  Result := Pos(Ch, SetOfChar) > 0;
  {$ENDIF BCB}
end;

function LoadTextFile(const FileName: TFileName): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    Result := Text;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName: TFileName; const Source: string);
begin
  with TStringList.Create do
  try
    Text := Source;
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

function ConcatSep(const S, S2, Separator: string): string;
begin
  if S <> '' then
    if S2 <> '' then
      Result := S + Separator + S2
    else
      Result := S
  else
    Result := S2;
end;

function ConcatLeftSep(const S, S2, Separator: string): string;
begin
  Result := S;
  if Result <> '' then
    Result := Separator + Result;
  Result := S2 + Result;
end;

function MinimizeString(const S: string; const MaxLen: Integer): string;
begin
  if Length(S) > MaxLen then
    if MaxLen < 3 then
      Result := Copy(S, 1, MaxLen)
    else
      Result := Copy(S, 1, MaxLen - 3) + '...'
  else
    Result := S;
end;

procedure Dos2Win(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$80..#$AF:
        S[I] := Char(Byte(S[I]) + (192 - $80));
      #$E0..#$EF:
        S[I] := Char(Byte(S[I]) + (240 - $E0));
    end;
end;

procedure Win2Dos(var S: string);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    case S[I] of
      #$C0..#$EF:
        S[I] := Char(Byte(S[I]) - (192 - $80));
      #$F0..#$FF:
        S[I] := Char(Byte(S[I]) - (240 - $E0));
    end;
end;

function Dos2WinRes(const S: string): string;
begin
  Result := S;
  Dos2Win(Result);
end;

function Win2DosRes(const S: string): string;
begin
  Result := S;
  Win2Dos(Result);
end;

function Win2Koi(const S: string): string;
const
  W = 'абвгдеЄжзийклмнопрстуфхчцшщьыъэю€јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’„÷Ўў№џЏЁёя';
  K = 'Ѕ¬„«ƒ≈£÷Џ… ЋћЌќѕ–“”‘’∆»ё√џЁЎўя№ј—бвчзде≥цъйклмнопртуфхжиюгыэшщ€ьас';
var
  I, J: Integer;
begin
  Result := S;
  for I := 1 to Length(Result) do
  begin
    J := Pos(Result[I], W);
    if J > 0 then
      Result[I] := K[J];
  end;
end;

function LastDate(const Dat: TDateTime): string;
const
  D2D: array [0..9] of 1..3 = (3, 1, 2, 2, 2, 3, 3, 3, 3, 3);
  Day: array [1..3] of string = ('день', 'дн€', 'дней');
  Month: array [1..3] of string = ('мес€ц', 'мес€ца', 'мес€цев');
  Year: array [1..3] of string = ('год', 'года', 'лет');
  Week: array [1..4] of string = ('неделю', '2 недели', '3 недели', 'мес€ц');
var
  Y, M, D: Integer;
begin
  if Date = Dat then
    Result := 'сегодн€'
  else
  if Dat = Date - 1 then
    Result := 'вчера'
  else
  if Dat = Date - 2 then
    Result := 'позавчера'
  else
  if Dat > Date then
    Result := 'в будущем'
  else
  begin
    D := Trunc(Date - Dat);
    Y := Round(D / 365);
    M := Round(D / 30);
    if Y > 0 then
      Result := IntToStr(Y) + ' ' + Year[D2D[StrToInt(IntToStr(Y)[Length(IntToStr(Y))])]] + ' назад'
    else
    if M > 0 then
      Result := IntToStr(M) + ' ' + Month[D2D[StrToInt(IntToStr(M)[Length(IntToStr(M))])]] + ' назад'
    else
    if D > 6 then
      Result := Week[D div 7] + ' назад'
    else
    if D > 0 then
      Result := IntToStr(D) + ' ' + Day[D2D[StrToInt(IntToStr(D)[Length(IntToStr(D))])]] + ' назад'
  end;
end;

function DefStr(const S: string; Default: string): string;
begin
  if S <> '' then
    Result := S
  else
    Result := Default;
end;

function StringEndsWith(const Str, SubStr: string): Boolean;
begin
  Result := Copy(Str, Length(Str) - Length(SubStr) + 1, Length(SubStr)) = SubStr;
end;

function ExtractFilePath2(const FileName: string): string;
var
  P, P1, P2, PP: PChar;
begin
  P := PChar(FileName);
  P1 := StrRScan(P, '\');
  P2 := StrRScan(P, '/');
  if P1 <> nil then
    if P2 <> nil then
      if P2 > P1 then
        PP := P2
      else
        PP := P1
    else
      PP := P1
  else
    PP := P2;

  if PP = nil then
    Result := ''
  else
    SetString(Result, P, PP - P + 1);
end;

end.

