{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVigenereCipher.PAS, released on 2001-02-28.

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

unit JvVigenereCipher;

interface

uses
  SysUtils, Classes,
  JvComponent;

type
  { (rb) Construct a abstract base class for encoders/decoders }
  TJvVigenereCipher = class(TJvComponent)
  private
    FKey: string;
    FDecoded: string;
    FEncoded: string;
    procedure SetDecoded(S: string);
    procedure SetEncoded(S: string);
    function Trans(Ch: Char; K: Byte): Char;
    function Decrypt(const S, AKey: string): string;
    function Crypt(const S, AKey: string): string;
  public
    procedure Decode(Strings: TStrings);
    procedure Encode(Strings: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  published
    { (rb) Only property Key should be stored to the stream? }
    property Key: string read FKey write FKey;
    property Encoded: string read FEncoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
  end;

implementation

function TJvVigenereCipher.Trans(Ch: Char; K: Byte): Char;
begin
  Result := Char((256 + Ord(Ch) + K) mod 256);
end;

function TJvVigenereCipher.Decrypt(const S, AKey: string): string;
var
  I, J: Byte;
begin
  if AKey = '' then
  begin
    Result := '';
    Exit;
  end;

  J := 1;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
  begin
    Result[I] := Trans(S[I], -Ord(AKey[J]));
    J := (J mod Length(AKey)) + 1;
  end;
end;

function TJvVigenereCipher.Crypt(const S, AKey: string): string;
var
  I, J: Byte;
begin
  if AKey = '' then
  begin
    Result := '';
    Exit;
  end;

  J := 1;
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
  begin
    Result[I] := Trans(S[I], Ord(AKey[J]));
    J := (J mod Length(AKey)) + 1;
  end;
end;

procedure TJvVigenereCipher.SetDecoded(S: string);
begin
  FDecoded := S;
  FEncoded := Crypt(S, FKey);
end;

procedure TJvVigenereCipher.SetEncoded(S: string);
begin
  FEncoded := S;
  FDecoded := Decrypt(S, FKey);
end;

procedure TJvVigenereCipher.Decode(Strings: TStrings);
begin
  Strings.Text := Decrypt(Strings.Text, FKey);
end;

procedure TJvVigenereCipher.Encode(Strings: TStrings);
begin
  Strings.Text := Crypt(Strings.Text, FKey);
end;

function TJvVigenereCipher.DecodeStream(Value: TStream): TStream;
var
  Buffer: array [0..1023] of Byte;
  I, J, Count: Integer;
begin
  { (RB) Letting this function create a stream is not a good idea; }
  Result := TMemoryStream.Create;
  J := 1;
  while Value.Position < Value.Size do
  begin
    Count := Value.Read(Buffer, SizeOf(Buffer));
    for I := 0 to Count - 1 do
    begin
      Buffer[I] := Ord(Trans(Char(Buffer[I]), -Ord(FKey[J])));
      J := (J mod Length(FKey)) + 1;
    end;
    Result.Write(Buffer, Count);
  end;
end;

function TJvVigenereCipher.EncodeStream(Value: TStream): TStream;
var
  Buffer: array[0..1023] of Byte;
  I, J, Count: Integer;
begin
  { (RB) Letting this function create a stream is not a good idea; }
  Result := TMemoryStream.Create;
  J := 1;
  while Value.Position < Value.Size do
  begin
    Count := Value.Read(Buffer, SizeOf(Buffer));
    for I := 0 to Count - 1 do
    begin
      Buffer[I] := Ord(Trans(Char(Buffer[I]), Ord(FKey[J])));
      J := (J mod Length(FKey)) + 1;
    end;
    Result.Write(Buffer, Count);
  end;
end;

end.

