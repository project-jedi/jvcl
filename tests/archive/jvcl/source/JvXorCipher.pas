{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvXorCipher.PAS, released on 2001-02-28.

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

unit JvXorCipher;

interface

uses
  SysUtils, Classes,
  JvTypes, JvComponent;

type
  { (RB) Construct a abstract base class for encoders/decoders }
  TJvXorCipher = class(TJvComponent)
  private
    FDecoded: string;
    FEncoded: string;
    FPivot: Byte;
    procedure SetDecoded(S: string);
    procedure SetEncoded(S: string);
    function Crypt(S: string): string;
  public
    procedure Decode(Strings: TStrings);
    procedure Encode(Strings: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  published
    { (RB) At most one of Encoded, Decoded should be stored, ie not both }
    { (RB) Pivot should be read before Encoded/Decoded from the stream; otherwise
           Encoded = Decoded after loading values from the stream because
           Pivot = 0
    }
    property Encoded: string read FEncoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
    property Pivot: Byte read FPivot write FPivot;
  end;

implementation

function TJvXorCipher.Crypt(S: string): string;
var
  I: Byte;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + Char(Ord(S[I]) xor FPivot);
end;

procedure TJvXorCipher.SetDecoded(S: string);
begin
  FDecoded := S;
  FEncoded := Crypt(S);
end;

procedure TJvXorCipher.SetEncoded(S: string);
begin
  FEncoded := S;
  FDecoded := Crypt(S);
end;

procedure TJvXorCipher.Decode(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Strings[I] := Crypt(Strings[I]);
end;

procedure TJvXorCipher.Encode(Strings: TStrings);
begin
  Decode(Strings);
end;

function TJvXorCipher.DecodeStream(Value: TStream): TStream;
var
  Buffer: array [0..1023] of Byte;
  I, Count: Integer;
begin
  { (RB) Letting this function create a stream is not a good idea; }
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    Count := Value.Read(Buffer, SizeOf(Buffer));
    for I := 0 to Count - 1 do
      Buffer[I] := Buffer[I] xor FPivot;
    Result.Write(Buffer, Count);
  end;
end;

function TJvXorCipher.EncodeStream(Value: TStream): TStream;
begin
  Result := DecodeStream(Value);
end;

end.

