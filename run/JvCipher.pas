{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCaesarCipher.PAS, JvXORCipher.PAS,
                      JvVigenereCipher.PAS, released on 2001-02-28.

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

unit JvCipher;

interface

uses
  SysUtils, Classes,
  JvComponent;

type
  // abstract base class for simple ciphers
  // which do not change the length of the data
  TJvCipher = class(TJvComponent)
  private
    FEncoded: string;
    FKey: string;
    function GetDecoded: string;
    procedure SetDecoded(const Value: string);
  protected
    procedure Decode(const Key: string; Buf: PChar; Size: Cardinal); virtual; abstract;
    procedure Encode(const Key: string; Buf: PChar; Size: Cardinal); virtual; abstract;
  public
    procedure DecodeList(const Key: string; List: TStrings);
    procedure EncodeList(const Key: string; List: TStrings);
    procedure DecodeStream(const Key: string; Stream: TStream);
    procedure EncodeStream(const Key: string; Stream: TStream);
    procedure DecodeFile(const Key: string; const FileName: string);
    procedure EncodeFile(const Key: string; const FileName: string);
    function DecodeString(const Key: string; const Value: string): string;
    function EncodeString(const Key: string; const Value: string): string;
  published
    property Key: string read FKey write FKey;
    property Encoded: string read FEncoded write FEncoded;
    property Decoded: string read GetDecoded write SetDecoded stored False;
  end;

  TJvCaesarCipher = class(TJvCipher)
  public
    procedure Decode(const Key: string; Buf: PChar; Size: Cardinal); override;
    procedure Encode(const Key: string; Buf: PChar; Size: Cardinal); override;
  end;

  TJvXORCipher = class(TJvCipher)
  public
    procedure Decode(const Key: string; Buf: PChar; Size: Cardinal); override;
    procedure Encode(const Key: string; Buf: PChar; Size: Cardinal); override;
  end;

  TJvVigenereCipher = class(TJvCipher)
  private
    function Trans(Ch: Char; K: Byte): Char;
  public
    procedure Decode(const Key: string; Buf: PChar; Size: Cardinal); override;
    procedure Encode(const Key: string; Buf: PChar; Size: Cardinal); override;
  end;

implementation

//=== TJvCipher ==============================================================

procedure TJvCipher.DecodeList(const Key: string; List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    for I := 0 to List.Count - 1 do
      if List[I] <> '' then
        Decode(Key, @(List[I])[1], Length(List[I]));
  finally
    List.EndUpdate;
  end;
end;

procedure TJvCipher.EncodeList(const Key: string; List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    for I := 0 to List.Count - 1 do
      if List[I] <> '' then
        Encode(Key, @(List[I])[1], Length(List[I]));
  finally
    List.EndUpdate;
  end;
end;

procedure TJvCipher.DecodeStream(const Key: string; Stream: TStream);
var
  MemStream: TMemoryStream;
  Count: Cardinal;
  Pos: Int64;
begin
  MemStream := TMemoryStream.Create;
  try
    Pos := Stream.Position;
    Count := Cardinal(Stream.Size - Pos);
    MemStream.SetSize(Count);
    if Count <> 0 then
    begin
      Stream.ReadBuffer(MemStream.Memory^, Count);
      Decode(Key, MemStream.Memory, Count);
      Stream.Position := Pos;
      Stream.WriteBuffer(MemStream.Memory^, Count);
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TJvCipher.EncodeStream(const Key: string; Stream: TStream);
var
  MemStream: TMemoryStream;
  Count: Cardinal;
  Pos: Int64;
begin
  MemStream := TMemoryStream.Create;
  try
    Pos := Stream.Position;
    Count := Cardinal(Stream.Size - Pos);
    MemStream.SetSize(Count);
    if Count <> 0 then
    begin
      Stream.ReadBuffer(MemStream.Memory^, Count);
      Encode(Key, MemStream.Memory, Count);
      Stream.Position := Pos;
      Stream.WriteBuffer(MemStream.Memory^, Count);
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TJvCipher.DecodeFile(const Key: string; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);
  try
    DecodeStream(Key, Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvCipher.EncodeFile(const Key: string; const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareExclusive);
  try
    EncodeStream(Key, Stream);
  finally
    Stream.Free;
  end;
end;

function TJvCipher.GetDecoded: string;
begin
  Result := DecodeString(FKey, FEncoded);
end;

procedure TJvCipher.SetDecoded(const Value: string);
begin
  FEncoded := EncodeString(FKey, Value);
end;

function TJvCipher.DecodeString(const Key, Value: string): string;
var
  Tmp : PChar;
begin
  GetMem(Tmp, Length(Value)+1);
  try
    StrPCopy(Tmp, Value);
    Decode(Key, Tmp, Length(Value));
    Result := Tmp;
  finally
    FreeMem(Tmp);
  end;
end;

function TJvCipher.EncodeString(const Key, Value: string): string;
var
  Tmp : PChar;
begin
  GetMem(Tmp, Length(Value)+1);
  try
    StrPCopy(Tmp, Value);
    Encode(Key, Tmp, Length(Value));
    Result := Tmp;
  finally
    FreeMem(Tmp);
  end;
end;

//=== TJvCaesarCipher ========================================================

procedure TJvCaesarCipher.Decode(const Key: string; Buf: PChar; Size: Cardinal);
var
  N: Integer;
  I: Cardinal;
begin
  if Size > 0 then
  begin
    N := StrToIntDef(Key, 13);
    if (N <= 0) or (N >= 256) then
      N := 13;
    for I := 0 to Size - 1 do
      Buf[I] := Char(Cardinal(Buf[I]) - Cardinal(N));
  end;
end;

procedure TJvCaesarCipher.Encode(const Key: string; Buf: PChar; Size: Cardinal);
var
  N: Integer;
  I: Cardinal;
begin
  if Size > 0 then
  begin
    N := StrToIntDef(Key, 13);
    if (N <= 0) or (N >= 256) then
      N := 13;
    for I := 0 to Size - 1 do
      Buf[I] := Char(Cardinal(Buf[I]) + Cardinal(N));
  end;
end;

//=== TJvXORCipher ===========================================================

procedure TJvXORCipher.Decode(const Key: string; Buf: PChar; Size: Cardinal);
var
  I: Cardinal;
  J: Cardinal;
begin
  if Key <> '' then
  begin
    J := 1;
    for I := 1 to Size do
    begin
      Buf[I-1] := Char(Ord(Buf[I-1]) xor Ord(Key[J]));
      J := (J mod Cardinal(Length(Key))) + 1;
    end;
  end;
end;

procedure TJvXORCipher.Encode(const Key: string; Buf: PChar; Size: Cardinal);
begin
  Decode(Key, Buf, Size);
end;

//=== TJvVigenereCipher ======================================================

function TJvVigenereCipher.Trans(Ch: Char; K: Byte): Char;
begin
  Result := Char((256 + Ord(Ch) + K) mod 256);
end;

procedure TJvVigenereCipher.Decode(const Key: string; Buf: PChar; Size: Cardinal);
var
  I: Cardinal;
  J: Cardinal;
begin
  if (Key <> '') and (Size > 0) then
  begin
    J := 1;
    for I := 0 to Size - 1 do
    begin
      Buf[I] := Trans(Buf[I], -Ord(Key[J]));
      J := (J mod Cardinal(Length(Key))) + 1;
    end;
  end;
end;

procedure TJvVigenereCipher.Encode(const Key: string; Buf: PChar; Size: Cardinal);
var
  I: Cardinal;
  J: Cardinal;
begin
  if (Key <> '') and (Size > 0) then
  begin
    J := 1;
    for I := 0 to Size - 1 do
    begin
      Buf[I] := Trans(Buf[I], Ord(Key[J]));
      J := (J mod Cardinal(Length(Key))) + 1;
    end;
  end;
end;

end.

