{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaesarCipher.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvCaesarCipher;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvTypes, JvComponent;

type
  TJvCaesarCipher = class(TJvComponent)
  private
    Fn: Byte;
    FDecoded: string;
    FEncoded: string;
    procedure SetDecoded(st: string);
    procedure SetEncoded(st: string);
    function Crypt(St: string; n: Byte): string;
    function CryptByte(Ch, n: Byte): Byte;
  published
    property N: Byte read Fn write Fn;
    property Encoded: string read Fencoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
    procedure Decode(It: Tstrings);
    procedure Encode(It: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  end;

implementation

{***********************************************************}

function TJvCaesarCipher.CryptByte(Ch, n: Byte): Byte;
var
  j: Integer;
begin
  j := Ch + n;
  if j < 0 then
    j := 256 - j
  else if j > 256 then
    j := j - 256;
  Result := j;
end;

{***********************************************************}

function TJvCaesarCipher.Crypt(St: string; n: Byte): string;
var
  i: Integer;
begin
  // (rom) optimized for speed
  SetLength(Result, Length(St));
  for i := 1 to Length(st) do
    Result[i] := Char(CryptByte(Byte(St[i]), n));
end;

{***********************************************************}

procedure TJvCaesarCipher.SetDecoded(st: string);
begin
  FDecoded := st;
  FEncoded := Crypt(st, Fn);
end;

{***********************************************************}

procedure TJvCaesarCipher.SetEncoded(st: string);
begin
  FEncoded := st;
  FDecoded := Crypt(st, -Fn);
end;

{***********************************************************}

procedure TJvCaesarCipher.Decode(It: TStrings);
var
  i: Integer;
begin
  // (rom) fixed loop to start at 0 instead of 1
  for i := 0 to It.Count - 1 do
    It[i] := Crypt(It[i], -Fn);
end;

{***********************************************************}

procedure TJvCaesarCipher.Encode(it: Tstrings);
var
  i: Integer;
begin
  // (rom) fixed loop to start at 0 instead of 1
  for i := 0 to It.Count - 1 do
    It[i] := Crypt(It[i], Fn);
end;

{***********************************************************}

function TJvCaesarCipher.DecodeStream(Value: TStream): TStream;
var
  buffer: array[0..1024] of Byte;
  i, count: Integer;
begin
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    count := Value.Read(buffer, 1024);
    for i := 0 to count - 1 do
      buffer[i] := CryptByte(buffer[i], -Fn);
    Result.Write(buffer, count);
  end;
end;

{***********************************************************}

function TJvCaesarCipher.EncodeStream(Value: TStream): TStream;
var
  buffer: array[0..1024] of Byte;
  i, count: Integer;
begin
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    count := Value.Read(buffer, 1024);
    for i := 0 to count - 1 do
      buffer[i] := CryptByte(buffer[i], Fn);
    Result.Write(buffer, count);
  end;
end;

end.
