{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaesarCipher.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvCaesarCipher;

interface

uses
  SysUtils, Classes,
  JvComponent;

type
  TJvCaesarCipher = class(TJvComponent)
  private
    Fn: Byte;
    FDecoded: string;
    FEncoded: string;
    procedure SetDecoded(St: string);
    procedure SetEncoded(St: string);
    function Crypt(St: string; N: Byte): string;
    function CryptByte(Ch, N: Byte): Byte;
  published
    property N: Byte read Fn write Fn;
    property Encoded: string read FEncoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
    procedure Decode(It: TStrings);
    procedure Encode(It: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  end;

implementation

const
  cBufferSize = 1024;

function TJvCaesarCipher.CryptByte(Ch, N: Byte): Byte;
var
  J: Integer;
begin
  J := Ch + N;
  if J < 0 then
    J := 256 - J
  else
  if J > 256 then
    J := J - 256;
  Result := J;
end;

function TJvCaesarCipher.Crypt(St: string; N: Byte): string;
var
  I: Integer;
begin
  // (rom) optimized for speed
  SetLength(Result, Length(St));
  for I := 1 to Length(St) do
    Result[I] := Char(CryptByte(Byte(St[I]), N));
end;

procedure TJvCaesarCipher.SetDecoded(St: string);
begin
  FDecoded := St;
  FEncoded := Crypt(St, Fn);
end;

procedure TJvCaesarCipher.SetEncoded(St: string);
begin
  FEncoded := St;
  FDecoded := Crypt(St, -Fn);
end;

procedure TJvCaesarCipher.Decode(It: TStrings);
var
  I: Integer;
begin
  It.BeginUpdate;
  try
    // (rom) fixed loop to start at 0 instead of 1
    for I := 0 to It.Count-1 do
      It[I] := Crypt(It[I], -Fn);
  finally
    It.EndUpdate;
  end;
end;

procedure TJvCaesarCipher.Encode(It: TStrings);
var
  I: Integer;
begin
  It.BeginUpdate;
  try
    // (rom) fixed loop to start at 0 instead of 1
    for I := 0 to It.Count - 1 do
      It[I] := Crypt(It[I], Fn);
  finally
    It.EndUpdate;
  end;
end;

function TJvCaesarCipher.DecodeStream(Value: TStream): TStream;
var
  Buffer: array [0..cBufferSize] of Byte;
  I, Count: Integer;
begin
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    Count := Value.Read(Buffer, cBufferSize);
    for I := 0 to Count - 1 do
      Buffer[I] := CryptByte(Buffer[I], -Fn);
    Result.Write(Buffer, Count);
  end;
end;

function TJvCaesarCipher.EncodeStream(Value: TStream): TStream;
var
  Buffer: array [0..cBufferSize] of Byte;
  I, Count: Integer;
begin
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    Count := Value.Read(Buffer, cBufferSize);
    for I := 0 to Count - 1 do
      Buffer[I] := CryptByte(Buffer[I], Fn);
    Result.Write(Buffer, Count);
  end;
end;

end.

