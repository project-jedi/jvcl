{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRle.PAS, released on 2001-02-28.

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

unit JvRle;

{$OBJEXPORTALL On}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvComponent;

type
  TJvRle = class(TJvComponent)
  private
  published
    function Compress(Stream: TStream): TStream;
    function Decompress(Stream: TStream): TStream;
  end;

implementation

{******************************************************************************}

function TJvRle.Compress(Stream: TStream): TStream;
var
  count, count2, count3, i: Integer;
  buf: array[0..1024] of Byte;
  buf2: array[0..60000] of Byte;
  b: Byte;
begin
  Result := TMemoryStream.Create;

  count := 1024;
  while count = 1024 do
  begin
    count := Stream.Read(buf, 1024);
    count2 := 0;
    i := 0;
    while i < count do
    begin
      b := buf[i];
      count3 := 0;
      while (buf[i] = b) and (i < count) and (count3 < $30) do
      begin
        Inc(i);
        Inc(count3);
      end;
      if (i = count) and (count3 in [2..$2F]) and (count = 1024) then
        Stream.Position := Stream.Position - count3
      else
      begin
        if count3 = 1 then
        begin
          if (b and $C0) = $C0 then
          begin
            buf2[count2] := $C1;
            buf2[count2 + 1] := b;
            Inc(count2, 2);
          end
          else
          begin
            buf2[count2] := b;
            Inc(count2);
          end;
        end
        else
        begin
          buf2[count2] := count3 or $C0;
          buf2[count2 + 1] := b;
          Inc(count2, 2);
        end;
      end;
    end;
    Result.Write(buf2, count2);
  end;

  Result.Position := 0;
end;

{******************************************************************************}

function TJvRle.Decompress(Stream: TStream): TStream;
var
  count, count2, count3, i: Integer;
  buf: array[0..1024] of Byte;
  buf2: array[0..60000] of Byte;
  b: Byte;
begin
  Result := TMemoryStream.Create;

  count := 1024;
  while count = 1024 do
  begin
    count := Stream.Read(buf, 1024);
    count2 := 0;
    i := 0;
    while i < count do
    begin
      if (buf[i] and $C0) = $C0 then
      begin
        if i = count - 1 then
          Stream.Position := Stream.Position - 1
        else
        begin
          b := buf[i] and $3F;
          Inc(i);
          for count3 := count2 to count2 + b - 1 do
            buf2[count3] := buf[i];
          count2 := count2 + b;
        end;
      end
      else
      begin
        buf2[count2] := buf[i];
        Inc(count2);
      end;
      Inc(i);
    end;
    Result.Write(buf2, count2);
  end;

  Result.Position := 0;
end;

end.
