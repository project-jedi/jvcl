{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStrUtil.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov
All Rights Reserved.

Contributor(s):

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : String utilities

Known Issues:
  Some russian comments were translated to english; these comments are marked
  with [translated]
-----------------------------------------------------------------------------}

{$I JVCL.INC}

{history
  (JVCL Library versions) :
   2.10.2
     - changed: GetXYByPos counts #13 instead of #10. It was to windows related
     - fixed bug: FindNotBlankCharPos always returns 1
}

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
function GetWordOnPos(const S: string; const P: Integer): string;
function GetWordOnPosEx(const S: string; const P: Integer; var iBeg, iEnd: Integer): string;
function Cmp(const S1, S2: string): Boolean;
{ Spaces returns string consists on N space chars }
function Spaces(const N: Integer): string;
{ HasChar returns True, if char, Ch, contains in string, S }
function HasChar(const Ch: Char; const S: string): Boolean;
function HasAnyChar(const Chars: string; const S: string): Boolean;
{ SubStr returns substring from string, S,
  separated with Separator string}
function SubStr(const S: string; const Index: Integer; const Separator: string): string;
{ SubStrEnd same to previous function but Index numerated
  from the end of string }
function SubStrEnd(const S: string; const Index: Integer; const Separator: string): string;
{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
function ReplaceString(S: string; const OldPattern, NewPattern: string): string;
function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean;
{ GetXYByPos is same to previous function, but
  returns X position in line too}
procedure GetXYByPos(const S: string; const Pos: Integer; var X, Y: Integer);
{ AddSlash returns string with added slash char to Dir parameter, if needed }
function AddSlash2(const Dir: TFileName): string;
{ AddPath returns FileName with Path, if FileName not contain any path }
function AddPath(const FileName, Path: TFileName): TFileName;
{ ExePath returns ExtractFilePath(ParamStr(0)) }
function ExePath: TFileName;
function LoadTextFile(const FileName: TFileName): string;
procedure SaveTextFile(const FileName: TFileName; const Source: string);
{ ConcatSep concatenate S and S2 strings with Separator.
  if S = '', separator don't included }
function ConcatSep(const S, S2, Separator: string): string;
{ FileEquMask returns True if file, FileName,
  is compatible with given dos file mask, Mask }
function FileEquMask(FileName, Mask: TFileName): Boolean;
{ FileEquMasks returns True if file, FileName,
  is compatible with given Masks.
  Masks must be separated with comma (';') }
function FileEquMasks(FileName, Masks: TFileName): Boolean;
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
begin
  for Result := 1 to Length(S) do
    if S[Result] <> ' ' then
      Exit;
  Result := Length(S) + 1;
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

function Cmp(const S1, S2: string): Boolean;
begin
  Result := AnsiStrIComp(PChar(S1), PChar(S2)) = 0;
end;

{ Spaces returns string consists on N space chars }

function Spaces(const N: Integer): string;
begin
  SetLength(Result, N);
  if N > 0 then
    FillChar(Result[1], N, ' ');
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

{ SubStr returns substring from string, S,
  separated with Separator string}

function SubStr(const S: string; const Index: Integer; const Separator: string): string;
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

{ GetXYByPos is same to previous function, but
  returns X position in line too}

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
    while (I <= Pos) do
    begin
      if S[I] = #10 then
      begin
        Inc(Y);
        iB := I + 1
      end;
      Inc(I);
    end;
    X := Pos - iB;
  end;
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

function CharInSet(const Ch: Char; const SetOfChar: TSetOfChar): Boolean;
begin
  {$IFDEF DELPHI}
  Result := Ch in SetOfChar;
  {$ENDIF DELPHI}
  {$IFDEF BCB}
  Result := Pos(Ch, SetOfChar) > 0;
  {$ENDIF BCB}
end;

function AddSlash2(const Dir: TFileName): string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir + '\';
end;

function AddPath(const FileName, Path: TFileName): TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
  else
    Result := FileName;
end;

function ExePath: TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
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

function FileEquMask(FileName, Mask: TFileName): Boolean;
var
  I: Integer;
  C: char;
  P: PChar;
begin
  FileName := AnsiUpperCase(ExtractFileName(FileName));
  Mask := AnsiUpperCase(Mask);
  Result := False;
  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';
  I := 1;
  P := PChar(FileName);
  while (I <= Length(Mask)) do
  begin
    C := Mask[I];
    if (P[0] = #0) and (C <> '*') then
      Exit;
    case C of
      '*':
        if I = Length(Mask) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          P := StrScan(P, Mask[I + 1]);
          if P = nil then
            Exit;
        end;
      '?':
       Inc(P);
    else
      if C = P[0] then
        Inc(P)
      else
        Exit;
    end;
    Inc(I);
  end;
  if P[0] = #0 then
    Result := True;
end;

function FileEquMasks(FileName, Masks: TFileName): Boolean;
var
  I: Integer;
  Mask: string;
begin
  Result := False;
  I := 0;
  Mask := Trim(SubStr(Masks, I, ';'));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask) then
    begin
      Result := True;
      Break;
    end
    else
    begin
      Inc(I);
      Mask := Trim(SubStr(Masks, I, ';'));
    end;
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

