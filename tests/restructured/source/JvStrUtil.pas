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

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : String utilities

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvStrUtil;

interface

uses Windows, SysUtils, Classes;

const
  Separators : set of char = [#00,' ','-',#13, #10,'.',',','/','\', '#', '"', '''',
    ':','+','%','*','(',')',';','=','{','}','[',']', '{', '}', '<', '>'];

{$IFDEF Delphi}
type
  TSetOfChar = set of char;
{$ENDIF Delphi}
{$IFDEF BCB}
type
  TSetOfChar = string;
{$ENDIF BCB}

  function FindNotBlankCharPos(const S: string): Integer;

  function ANSIChangeCase(const S: string): string;

  function GetWordOnPos(const S : string; const P : integer) : string;

  function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;

  function Cmp(const S1, S2 : string) : boolean;

  { Spaces returns string consists on N space chars }
  function Spaces(const N : integer) : string;

  { HasChar returns true, if char, Ch, contains in string, S }
  function HasChar(const Ch : Char; const S : string) : boolean;

  function HasAnyChar(const Chars : string; const S : string) : boolean;

  { SubStr returns substring from string, S,
    separated with Separator string}
  function SubStr(const S : string; const index : integer; const Separator : string) : string;

  { SubStrEnd same to previous function but index numerated
    from the end of string }

  function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;

  { ReplaceString searches for all substrings, OldPattern,
    in a string, S, and replaces them with NewPattern }
  function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;

  function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;

  { GetXYByPos is same to previous function, but
    returns X position in line too}
  procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);

  { AddSlash returns string with added slash char to Dir parameter, if needed }
  function AddSlash2(const Dir : TFileName) : string;

  { AddPath returns FileName with Path, if FileName not contain any path }
  function AddPath(const FileName, Path : TFileName) : TFileName;

  { ExePath returns ExtractFilePath(ParamStr(0)) }
  function ExePath : TFileName;

  function LoadTextFile(const FileName : TFileName): string;

  procedure SaveTextFile(const FileName : TFileName; const Source : string);

  { ConcatSep concatenate S and S2 strings with Separator.
    if S = '', separator don't included }
  function ConcatSep(const S, S2, Separator : string) : string;

  { FileEquMask returns true if file, FileName,
    is compatible with given dos file mask, Mask }
  function FileEquMask(FileName, Mask : TFileName) : boolean;

  { FileEquMasks returns true if file, FileName,
    is compatible with given Masks.
    Masks must be separated with comma (';') }
  function FileEquMasks(FileName, Masks : TFileName) : boolean;

  function StringEndsWith(const Str, SubStr: String): Boolean;

  function ExtractFilePath2(const FileName: String): String;

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
  i: Integer;
begin
  Result := 1;
  for i := 1 to Length(S) do
    if S[i] <> ' ' then Exit;
end;

function ANSIChangeCase(const S: string): string;
var
  i: Integer;
  Up: ANSIChar;
begin
  Result := S;
  for i := 1 to Length(Result) do
  begin
    Up := ANSIUpperCase(Result[i])[1];
    if Result[i] = Up then
      Result[i] := ANSILowerCase(Result[i])[1]
    else
      Result[i] := Up;
  end;
end;

function GetWordOnPos(const S : string; const P : integer) : string;
var
  i, Beg : integer;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  for i := P downto 1 do
    // add by patofan
    if (S[i] in Separators ) {$IFDEF Delphi3_Up}and ( StrByteType( pchar(s) , i-1 ) <> mbTrailByte ){$ENDIF} then
    // ending add by patofan
      break;
  Beg := i + 1;
  for i := P to Length(S) do
    // add by patofan
    if (S[i] in Separators ) {$IFDEF Delphi3_Up}and ( StrByteType( pchar(s) , i-1 ) <> mbTrailByte ){$ENDIF} then
    // ending add by patofan
      break;
  if i > Beg then
    Result := Copy(S, Beg, i-Beg) else
  Result := S[P];
end;

function GetWordOnPosEx(const S : string; const P : integer; var iBeg, iEnd : integer) : string;
begin
  Result := '';
  if (P > Length(S)) or (P < 1) then exit;
  iBeg := P;
  if P > 1 then
    // add by patofan
    if (S[P] in Separators ) {$IFDEF Delphi3_Up}and ( StrByteType( pchar(s) , p-1 ) <> mbTrailByte ){$ENDIF} then
    // ending add by patofan
      if (P < 1) or ((P - 1 > 0) and (S[P-1] in Separators)) then
        inc(iBeg)
      else if not ((P - 1 > 0) and (S[P-1] in Separators)) then
        dec(iBeg);
  if iBeg > Length(S) then iBeg := P-1; {+RWare}
  while iBeg >= 1 do
    // add by patofan
    if ( S[iBeg] in Separators ) {$IFDEF Delphi3_Up}and ( StrByteType( pchar(s) , iBeg-1 ) <> mbTrailByte ){$ENDIF} then
    // ending add by patofan
      break
    else
      dec(iBeg);
  inc(iBeg);
  iEnd := P;
  while iEnd <= Length(S) do
    if ( S[iEnd] in Separators ) {$IFDEF COMPILER3_UP} and ( StrByteType( pchar(s) , iEnd-1 ) <> mbTrailByte ) {$ENDIF}
    then break else inc(iEnd);
  if iEnd > iBeg then
    Result := Copy(S, iBeg, iEnd - iBeg) else
    Result := S[P];
end;

function Cmp(const S1, S2 : string) : boolean;
begin
  Result := ANSIStrIComp(PChar(S1), PChar(S2)) = 0;
end;

{ Spaces returns string consists on N space chars }
function Spaces(const N : integer) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to N do Result := Result+' ';
end;

{ HasChar returns true, if char, Ch, contains in string, S }
function HasChar(const Ch : Char; const S : string) : boolean;
begin
  Result := Pos(Ch, S) > 0;
end;

function HasAnyChar(const Chars : string; const S : string) : boolean;
var
  i : integer;
begin
  for i := 1 to Length(Chars) do
    if HasChar(Chars[i], S) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

{ SubStr returns substring from string, S,
  separated with Separator string}
function SubStr(const S : string; const index : integer; const Separator : string) : string;
 {Вырезает подстроку. Подстроки разделяются символом Sep}
var
  i : integer;
  pB, pE : PChar;
begin
  Result := '';
  if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or
    (Length(S) = 0) then exit;
  pB := PChar(S);
  for i := 1 to index do begin
    pB := StrPos(pB, PChar(Separator));
    if pB = nil then exit;
    pB := pB+Length(Separator);
    if pB[0] = #0 then exit;
  end;
  pE := StrPos(pB+1, PChar(Separator));
  if pE = nil then pE := PChar(S)+Length(S);
  if not (ANSIStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
    SetString(Result, pB, pE-pB);
end;

function SubStrEnd(const S : string; const index : integer; const Separator : string) : string;
 {то же что и SubStr, но подстроки нумеруются с конца}
var
  MaxIndex : integer;
  pB : PChar;
begin
 {неоптимальная реализация}
  MaxIndex := 0;
  pB := StrPos(PChar(S), PChar(Separator));
  while pB <> nil do begin
    inc(MaxIndex);
    pB := StrPos(pB+Length(Separator), PChar(Separator));
  end;
  Result := SubStr(S, MaxIndex - index, Separator);
end;

{ GetXYByPos is same to previous function, but
  returns X position in line too}
procedure GetXYByPos(const S : string; const Pos : integer; var X, Y : integer);
var
  i, iB : integer;
begin
  X := -1; Y := -1; iB := 0;
  if (Length(S) >= Pos) and (Pos >= 0) then begin
    i := 1;
    Y := 0;
    while (i <= Pos) do begin
      if S[i] = #13 then begin inc(Y); iB := i+1 end;
      inc(i);
    end;
    X := Pos - iB;
  end;
end;

{ ReplaceString searches for all substrings, OldPattern,
  in a string, S, and replaces them with NewPattern }
function ReplaceString(S : string; const OldPattern, NewPattern : string) : string;
var
  LW : integer;
  P : PChar;
  Sm : integer;
begin
  LW := Length(OldPattern);
  P := StrPos(PChar(S), PChar(OldPattern));
  while P <> nil do begin
    Sm := P-PChar(S);
    S := Copy(S, 1, Sm)+NewPattern+Copy(S, Sm+LW+1, Length(S));
    P := StrPos(PChar(S)+Sm+Length(NewPattern), PChar(OldPattern));
  end;
  Result := S;
end;

function CharInSet(const Ch : Char; const SetOfChar : TSetOfChar) : boolean;
begin
{$IFDEF Delphi}
  Result := Ch in SetOfChar;
{$ENDIF Delphi}
{$IFDEF BCB}
  Result := Pos(Ch, SetOfChar) > 0;
{$ENDIF BCB}
end;

function AddSlash2(const Dir : TFileName) : string;
begin
  Result := Dir;
  if (Length(Dir) > 0) and (Dir[Length(Dir)] <> '\') then
    Result := Dir +'\';
end;

function AddPath(const FileName, Path : TFileName) : TFileName;
begin
  if ExtractFileDrive(FileName) = '' then
    Result := AddSlash2(Path) + FileName
  else
    Result := FileName;
end;

function ExePath : TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function LoadTextFile(const FileName : TFileName): string;
begin
  with TStringList.Create do
  try
  LoadFromFile(FileName);
  Result := Text;
  finally
    Free;
  end;
end;

procedure SaveTextFile(const FileName : TFileName; const Source : string);
begin
  with TStringList.Create do
  try
  Text := Source;
  SaveToFile(FileName);
  finally
    Free;
  end;
end;

function ConcatSep(const S, S2, Separator : string) : string;
begin
  if S <> '' then
    if S2 <> '' then
      Result := S + Separator + S2
    else
      Result := S
  else
    Result := S2;
end;

function FileEquMask(FileName, Mask : TFileName) : boolean;
var
  i : integer;
  C : char;
  P : PChar;
begin
  FileName := ANSIUpperCase(ExtractFileName(FileName));
  Mask := ANSIUpperCase(Mask);
  Result := false;
  if Pos('.', FileName) = 0 then FileName := FileName+'.';
  i := 1; P := PChar(FileName);
  while (i <= length(Mask)) do begin
    C := Mask[i];
    if (P[0] = #0) and (C <> '*') then exit;
    case C of
      '*' :
        if i = length(Mask) then begin
          Result := true;
          exit;
        end else begin
          P := StrScan(P, Mask[i+1]);
          if P = nil then exit;
        end;
      '?' : inc(P);
      else if C = P[0] then inc(P) else exit;
    end;
    inc(i);
  end;
  if P[0] = #0 then Result := true;
end;

function FileEquMasks(FileName, Masks : TFileName) : boolean;
var
  i : integer;
  Mask : string;
begin
  Result := false;
  i := 0;
  Mask := Trim(SubStr(Masks, i, ';'));
  while Length(Mask) <> 0 do
    if FileEquMask(FileName, Mask) then begin
      Result := true;
      break;
    end else begin
      inc(i);
      Mask := Trim(SubStr(Masks, i, ';'));
    end;
end;

function StringEndsWith(const Str, SubStr: String): Boolean;
begin
  Result := Copy(Str, Length(Str) - Length(SubStr) + 1, Length(SubStr)) = SubStr;
end;

function ExtractFilePath2(const FileName: String): String;
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
  begin
    SetString(Result, P, PP - P + 1);
  end;
end;

end.
