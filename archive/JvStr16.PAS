{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvJvStr16.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvStr16;

interface

{$IFNDEF WIN32}

type
  ShortString = string;
  PShortString = ^ShortString;
  AnsiChar = Char;
  PAnsiChar = ^AnsiChar;

{ System32 unit functions and procedures }
procedure SetLength(var S: string; NewLength: Byte);
procedure SetString(var S: string; Buffer: PChar; Len: Byte);
procedure UniqueString(var S: string);

{ SysUtils32 unit functions and procedures }
function Trim(const S: string): string;
function TrimLeft(const S: string): string;
function TrimRight(const S: string): string;
function QuotedStr(const S: string): string;

{$ENDIF WIN32}

implementation

{$IFNDEF WIN32}

uses
  SysUtils;

procedure SetLength(var S: string; NewLength: Byte);
begin
  S[0] := Char(NewLength);
end;

procedure SetString(var S: string; Buffer: PChar; Len: Byte);
begin
  S[0] := Char(Len);
  if Buffer <> nil then
  begin
    if StrLen(Buffer) < Len then
      Len := StrLen(Buffer);
    Move(Buffer^, S[1], Len);
  end;
end;

procedure UniqueString(var S: string);
begin
end;

function Trim(const S: string): string;
var
  I, L: Byte;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] <= ' ' do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeft(const S: string): string;
var
  I, L: Byte;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do
    Inc(I);
  Result := Copy(S, I, 255);
end;

function TrimRight(const S: string): string;
var
  I: Byte;
begin
  I := Length(S);
  while (I > 0) and (S[I] <= ' ') do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function QuotedStr(const S: string): string;
var
  I: Byte;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if Result[I] = '''' then
      Insert('''', Result, I);
  Result := '''' + Result + '''';
end;

{$ENDIF WIN32}

end.
