{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvVigenereCipher.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvVigenereCipher;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvTypes, JvComponent;

type
  TJvVigenereCipher = class(TJvComponent)
  private
    FKey: string;
    FDecoded: string;
    FEncoded: string;
    procedure SetDecoded(st: string);
    procedure SetEncoded(st: string);
    function Trans(Ch: Char; k: Byte): Char;
    function Decrypt(St: string; Ke: string): string;
    function Crypt(St: string; Ke: string): string;
  protected
  public
  published
    property Key: string read FKey write FKey;
    property Encoded: string read Fencoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
    procedure Decode(It: TStrings);
    procedure Encode(It: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  end;

implementation

{***********************************************************}

function TJvVigenereCipher.Trans(Ch: Char; k: Byte): Char;
var
  i: Integer;
begin
  i := Ord(ch) + k;
  if i < 0 then
    i := i + 256
  else if i > 255 then
    i := i - 256;
  Result := Char(i);
end;

{***********************************************************}

function TJvVigenereCipher.Decrypt(St: string; Ke: string): string;
var
  i, j: Byte;
begin
  Result := '';
  j := 1;
  if Length(Ke) > 0 then
    for i := 1 to Length(St) do
    begin
      Result := Result + Trans(St[i], -Ord(Ke[j]));
      if j = Length(Ke) then
        j := 1
      else
        Inc(j);
    end;
end;

{***********************************************************}

function TJvVigenereCipher.Crypt(St: string; Ke: string): string;
var
  i, j: Byte;
begin
  Result := '';
  j := 1;
  if Length(Ke) > 0 then
    for i := 1 to Length(St) do
    begin
      Result := Result + Trans(St[i], Ord(Ke[j]));
      if j = Length(Ke) then
        j := 1
      else
        Inc(j);
    end;
end;

{***********************************************************}

procedure TJvVigenereCipher.SetDecoded(st: string);
begin
  FDecoded := st;
  FEncoded := Crypt(st, FKey);
end;

{***********************************************************}

procedure TJvVigenereCipher.SetEncoded(st: string);
begin
  FEncoded := st;
  FDecoded := Decrypt(st, FKey);
end;

{***********************************************************}

procedure TJvVigenereCipher.Decode(It: TStrings);
begin
  It.Text := Decrypt(It.Text, FKey);
end;

{***********************************************************}

procedure TJvVigenereCipher.Encode(it: TStrings);
begin
  It.Text := Crypt(It.Text, FKey);
end;

{***********************************************************}

function TJvVigenereCipher.DecodeStream(Value: TStream): TStream;
var
  buffer: array[0..1024] of Byte;
  i, j, count: Integer;
begin
  Result := TMemoryStream.Create;
  j := 1;
  while Value.Position < Value.Size do
  begin
    count := Value.Read(buffer, 1024);
    for i := 0 to count - 1 do
    begin
      buffer[i] := Ord(Trans(Char(buffer[i]), -Ord(FKey[j])));
      if j = Length(FKey) then
        j := 1
      else
        Inc(j);
    end;
    Result.Write(buffer, count);
  end;
end;

{***********************************************************}

function TJvVigenereCipher.EncodeStream(Value: TStream): TStream;
var
  buffer: array[0..1024] of Byte;
  i, j, count: Integer;
begin
  Result := TMemoryStream.Create;
  j := 1;
  while Value.Position < Value.Size do
  begin
    count := Value.Read(buffer, 1024);
    for i := 0 to count - 1 do
    begin
      buffer[i] := Ord(Trans(Char(buffer[i]), Ord(FKey[j])));
      if j = Length(FKey) then
        j := 1
      else
        Inc(j);
    end;
    Result.Write(buffer, count);
  end;
end;

end.
