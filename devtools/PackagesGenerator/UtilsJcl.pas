{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: UtilsJcl.pas, released on 2004-03-24.

The Initial Developer of the Original Code is Andreas Hausladen
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen
All Rights Reserved.

Contributor(s):
  JEDI Code library

Last Modified: 2004-03-24

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}
{$I windowsonly.inc}

unit UtilsJcl;

interface

uses
  Windows, SysUtils, Classes;

{ JclStrings emulation }
const
  {$IFDEF COMPILER6_UP}
  PathSeparator = PathDelim;
  {$ELSE}
  PathSeparator = '\';
  {$ENDIF MSWINDOWS}

{$IFDEF COMPILER5}
function SameText(const S1, S2: string): Boolean;
{$ENDIF COMPILER5}

procedure StrToStrings(S: string; const Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
function StringsToStr(const List: TStrings; const Sep: AnsiString; const AllowEmptyString: Boolean = True): AnsiString;
function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
function CharUpper(const C: AnsiChar): AnsiChar;
function StrPrefixIndex(const S: AnsiString; const Prefixes: array of string): Integer;
function StrHasPrefix(const S: AnsiString; const Prefixes: array of string): Boolean;


{$IFDEF COMPILER5}
type
  TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);
{$ENDIF COMPIELR5}
procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags = []);


{ JclFileUtils emulation }
function PathExtractFileNameNoExt(const Path: string): string;
function PathIsAbsolute(const Path: string): Boolean;
function DirectoryExists(const Dir: string): Boolean;


{ JclDateUtils }

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;

{ JclSysUtils }

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;

implementation

{$IFDEF COMPILER5}
function SameText(const S1, S2: string): Boolean;
begin
  Result := CompareText(S1, S2) = 0;
end;
{$ENDIF COMPILER5}

{ JclStrings emulation }

procedure StrToStrings(S: string; const Sep: AnsiString; const List: TStrings; const AllowEmptyString: Boolean = True);
var
  I, L: Integer;
  Left: AnsiString;
begin
  Assert(List <> nil);
  List.Clear;
  L := Length(Sep);
  I := Pos(Sep, S);
  while (I > 0) do
  begin
    Left := Copy(S, 1, I - 1);
    if (Left <> '') or AllowEmptyString then
      List.Add(Left);
    Delete(S, 1, I + L - 1);
    I := Pos(Sep, S);
  end;
  if S <> '' then
    List.Add(S);  // Ignore empty strings at the end.
end;

function StringsToStr(const List: TStrings; const Sep: AnsiString; const AllowEmptyString: Boolean): AnsiString;
var
  I, L: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
  begin
    if (List[I] <> '') or AllowEmptyString then
    begin
      // don't combine these into one addition, somehow it hurts performance
      Result := Result + List[I];
      Result := Result + Sep;
    end;
  end;
  // remove terminating separator
  if List.Count <> 0 then
  begin
    L := Length(Sep);
    Delete(Result, Length(Result) - L + 1, L);
  end;
end;

function StrEnsureSuffix(const Suffix, Text: AnsiString): AnsiString;
var
  SuffixLen: Integer;
begin
  SuffixLen := Length(Suffix);
  if Copy(Text, Length(Text) - SuffixLen + 1, SuffixLen) = Suffix then
    Result := Text
  else
    Result := Text + Suffix;
end;

function StrRepeat(const S: AnsiString; Count: Integer): AnsiString;
var
  L: Integer;
  P: PChar;
begin
  L := Length(S);
  SetLength(Result, Count * L);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Move(Pointer(S)^, P^, L);
    P := P + L;
    Dec(Count);
  end;
end;

function CharUpper(const C: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(Windows.CharUpper(PAnsiChar(C)));
end;

function StrPrefixIndex(const S: AnsiString; const Prefixes: array of string): Integer;
var
  I: Integer;
  Test: string;
begin
  Result := -1;
  for I := Low(Prefixes) to High(Prefixes) do
  begin
    Test := Copy(S, 1, Length(Prefixes[I]));
    if AnsiSameText(Test, Prefixes[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function StrHasPrefix(const S: AnsiString; const Prefixes: array of string): Boolean;
begin
  Result := StrPrefixIndex(S, Prefixes) > -1;
end;


function StrEqualText(Text: PChar; SearchText: PChar; MaxLen: Integer;
  IgnoreCase: Boolean): Boolean;
var
  i: Integer;
begin
  if IgnoreCase then
    Result := StrLIComp(Text, SearchText, MaxLen) = 0
  else
  begin
    Result := False;
    for i := 0 to MaxLen - 1 do
      if (Text[i] = #0) or {(SearchText[i] = #0) or}
         (Text[i] <> SearchText[i]) then Exit;
    Result := True;
  end;
end;

function FastStringReplace(const Text, SearchText, ReplaceText: string;
  ReplaceAll, IgnoreCase: Boolean): string;
var
  LenSearchText, LenReplaceText, LenText: Integer;
  Index, Len, StartIndex: Integer;
begin
  LenSearchText := Length(SearchText);
  LenReplaceText := Length(ReplaceText);
  LenText := Length(Text);
  if LenSearchText = 0 then
  begin
    Result := Text;
    Exit;
  end;

  if ReplaceAll then
  begin
    if LenReplaceText - LenSearchText > 0 then
      SetLength(Result, LenText +
        (LenReplaceText - LenSearchText) * (LenText div LenSearchText))
    else
      SetLength(Result, LenText);
  end
  else
    SetLength(Result, LenText + (LenReplaceText - LenSearchText));


  Len := 0;
  StartIndex := 1;
  for Index := 1 to LenText do
  begin
    if StrEqualText(PChar(Pointer(Text)) + Index - 1, Pointer(SearchText),
                   LenSearchText, IgnoreCase) then
    begin
      if Index > StartIndex then
      begin 
        Move(Text[StartIndex], Result[Len + 1], Index - StartIndex); 
        Inc(Len, Index - StartIndex);
      end; 
      StartIndex := Index + LenSearchText; 

      if LenReplaceText > 0 then
      begin 
        Move(ReplaceText[1], Result[Len + 1], LenReplaceText); 
        Inc(Len, LenReplaceText); 
      end; 

      if not ReplaceAll then Break; 
    end; 
  end; 

  Index := LenText + 1;
  if Index > StartIndex then
  begin
    Move(Text[StartIndex], Result[Len + 1], Index - StartIndex);
    Inc(Len, Index - StartIndex);
  end;

  SetLength(Result, Len);
end;

procedure StrReplace(var S: AnsiString; const Search, Replace: AnsiString; Flags: TReplaceFlags = []);
begin
  S := FastStringReplace(S, Search, Replace, rfReplaceAll in Flags, rfIgnoreCase in Flags);
end;


{ JclFileUtils emulation }

function PathExtractFileNameNoExt(const Path: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(Path), '');
end;

function PathIsAbsolute(const Path: string): Boolean;
const
  DriveLetters     = ['a'..'z', 'A'..'Z'];
var
  I: Integer;
begin
  Result := False;
  if Path <> '' then
  begin
    I := 0;
    if Copy(Path, 1, 4) = '\\.\' then
      I := 4
    else
    if Copy(Path, 1, 2) = '\\' then
      I := 2;
    Result := (Length(Path) > I + 2) and (Path[I + 1] in DriveLetters) and
      (Path[I + 2] = ':') and (Path[I + 3] = PathSeparator);
  end;
end;

function DirectoryExists(const Dir: string): Boolean;
var
  Attr: Cardinal;
begin
  Attr := GetFileAttributes(PChar(Dir));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

{ JclDateUtils }

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
const
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day
  FileTimeBase = -109205.0;
begin
  Result := Int64(FileTime) / FileTimeStep;
  Result := Result + FileTimeBase;
end;

{ JclSysUtils }

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

end.
