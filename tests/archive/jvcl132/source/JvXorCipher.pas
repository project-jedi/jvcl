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
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvXorCipher;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvTypes, JvComponent;

type
  TJvXorCipher = class(TJvComponent)
  private
    FDecoded: string;
    FEncoded: string;
    FPivot: Byte;
    procedure SetDecoded(st: string);
    procedure SetEncoded(st: string);
    function Crypt(St: string): string;
  published
    property Encoded: string read Fencoded write SetEncoded;
    property Decoded: string read FDecoded write SetDecoded;
    property Pivot: Byte read FPivot write FPivot;
    procedure Decode(It: TStrings);
    procedure Encode(It: TStrings);
    function EncodeStream(Value: TStream): TStream;
    function DecodeStream(Value: TStream): TStream;
  end;

implementation

{***********************************************************}

function TJvXorCipher.Crypt(St: string): string;
var
  i: Byte;
begin
  Result := '';
  for i := 1 to Length(st) do
    Result := Result + Char((Ord(st[i])) xor FPivot);
end;

{***********************************************************}

procedure TJvXorCipher.SetDecoded(st: string);
begin
  FDecoded := st;
  FEncoded := Crypt(st);
end;

{***********************************************************}

procedure TJvXorCipher.SetEncoded(st: string);
begin
  FEncoded := st;
  FDecoded := Crypt(st);
end;

{***********************************************************}

procedure TJvXorCipher.Decode(It: TStrings);
var
  j: Integer;
begin
  for j := 1 to It.Count - 1 do
    It[j] := Crypt(It[j]);
end;

{***********************************************************}

procedure TJvXorCipher.Encode(It: TStrings);
begin
  Decode(It);
end;

{***********************************************************}

function TJvXorCipher.DecodeStream(Value: TStream): TStream;
var
  buffer: array[0..1024] of Byte;
  i, count: Integer;
begin
  Result := TMemoryStream.Create;
  while Value.Position < Value.Size do
  begin
    count := Value.Read(buffer, 1024);
    for i := 0 to count - 1 do
      buffer[i] := buffer[i] xor FPivot;
    Result.Write(buffer, count);
  end;
end;

{***********************************************************}

function TJvXorCipher.EncodeStream(Value: TStream): TStream;
begin
  Result := DecodeStream(Value);
end;
end.
