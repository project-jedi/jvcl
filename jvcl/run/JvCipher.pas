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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCipher;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  JvComponentBase;

type
  // abstract base class for simple ciphers
  // which do not change the length of the data
  TJvCustomCipher = class(TJvComponent)
  private
    FEncoded: AnsiString;
    FKey: AnsiString;
    FIsStored: Boolean;
    function GetDecoded: AnsiString;
    procedure SetDecoded(const Value: AnsiString);
    function GetIsStored: Boolean;
  protected
    procedure Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); virtual; abstract;
    procedure Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); virtual; abstract;
  public
    { TODO -oahuser -cUnicode : Is there a TAnsStringList ? }
    procedure DecodeList(const Key: AnsiString; List: TStrings);
    procedure EncodeList(const Key: AnsiString; List: TStrings);
    procedure DecodeStream(const Key: AnsiString; Stream: TStream);
    procedure EncodeStream(const Key: AnsiString; Stream: TStream);
    procedure DecodeFile(const Key: AnsiString; const FileName: string);
    procedure EncodeFile(const Key: AnsiString; const FileName: string);
    function DecodeString(const Key: AnsiString; const Value: AnsiString): AnsiString;
    function EncodeString(const Key: AnsiString; const Value: AnsiString): AnsiString;
    constructor Create(AOwner: TComponent); override;
  published
    property Key: AnsiString read FKey write FKey stored GetIsStored;
    property Encoded: AnsiString read FEncoded write FEncoded stored GetIsStored;
    property Decoded: AnsiString read GetDecoded write SetDecoded stored False;
    property IsStored: Boolean read GetIsStored write FIsStored default True;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCaesarCipher = class(TJvCustomCipher)
  public
    procedure Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
    procedure Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvXORCipher = class(TJvCustomCipher)
  public
    procedure Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
    procedure Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvVigenereCipher = class(TJvCustomCipher)
  private
    function Trans(Ch: AnsiChar; K: Byte): AnsiChar;
  public
    procedure Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
    procedure Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal); override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

//=== { TJvCustomCipher } ====================================================

constructor TJvCustomCipher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsStored := True;
end;

procedure TJvCustomCipher.DecodeList(const Key: AnsiString; List: TStrings);
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

procedure TJvCustomCipher.EncodeList(const Key: AnsiString; List: TStrings);
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

procedure TJvCustomCipher.DecodeStream(const Key: AnsiString; Stream: TStream);
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

procedure TJvCustomCipher.EncodeStream(const Key: AnsiString; Stream: TStream);
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

procedure TJvCustomCipher.DecodeFile(const Key: AnsiString; const FileName: string);
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

procedure TJvCustomCipher.EncodeFile(const Key: AnsiString; const FileName: string);
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

function TJvCustomCipher.GetDecoded: AnsiString;
begin
  Result := DecodeString(FKey, FEncoded);
end;

procedure TJvCustomCipher.SetDecoded(const Value: AnsiString);
begin
  FEncoded := EncodeString(FKey, Value);
end;

function TJvCustomCipher.DecodeString(const Key, Value: AnsiString): AnsiString;
begin
  Result := Value;
  UniqueString(Result);
  Decode(Key, PAnsiChar(Result), Length(Value));
end;

function TJvCustomCipher.EncodeString(const Key, Value: AnsiString): AnsiString;
begin
  Result := Value;
  UniqueString(Result);
  Encode(Key, PAnsiChar(Result), Length(Value));
end;

function TJvCustomCipher.GetIsStored: Boolean;
begin
  Result := FIsStored;
end;

//=== { TJvCaesarCipher } ====================================================

procedure TJvCaesarCipher.Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
var
  N: Integer;
  I: Cardinal;
begin
  if Size > 0 then
  begin
    N := StrToIntDef(string(Key), 13);
    if (N <= 0) or (N >= 256) then
      N := 13;
    for I := 0 to Size - 1 do
      Buf[I] := AnsiChar(Cardinal(Buf[I]) - Cardinal(N));
  end;
end;

procedure TJvCaesarCipher.Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
var
  N: Integer;
  I: Cardinal;
begin
  if Size > 0 then
  begin
    N := StrToIntDef(string(Key), 13);
    if (N <= 0) or (N >= 256) then
      N := 13;
    for I := 0 to Size - 1 do
      Buf[I] := AnsiChar(Cardinal(Buf[I]) + Cardinal(N));
  end;
end;

//=== { TJvXORCipher } =======================================================

procedure TJvXORCipher.Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
var
  I: Cardinal;
  J: Cardinal;
begin
  if Key <> '' then
  begin
    J := 1;
    for I := 1 to Size do
    begin
      Buf[I-1] := AnsiChar(Ord(Buf[I-1]) xor Ord(Key[J]));
      J := (J mod Cardinal(Length(Key))) + 1;
    end;
  end;
end;

procedure TJvXORCipher.Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
begin
  Decode(Key, Buf, Size);
end;

//=== { TJvVigenereCipher } ==================================================

function TJvVigenereCipher.Trans(Ch: AnsiChar; K: Byte): AnsiChar;
begin
  Result := AnsiChar((256 + Ord(Ch) + K) mod 256);
end;

procedure TJvVigenereCipher.Decode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
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

procedure TJvVigenereCipher.Encode(const Key: AnsiString; Buf: PAnsiChar; Size: Cardinal);
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

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
