{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvButtonUtils.PAS, released on 2002-06-15.

The Initial Developer of the Original Code is Jan Verhoeven [jan1.verhoeven@wxs.nl]
Portions created by Jan Verhoeven are Copyright (C) 2002 Jan Verhoeven.
All Rights Reserved.

Contributor(s): Robert Love [rlove@slcdug.org].

Last Modified: 2000-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvButtonUtils;

interface

uses
  Windows, SysUtils, Classes, Graphics;

procedure AntiAlias(Clip: TBitmap);
procedure AntiAliasRect(Clip: TBitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);

implementation

uses
  Math;

procedure AntiAlias(Clip: TBitmap);
begin
  AntiAliasRect(Clip, 0, 0, Clip.Width, Clip.Height);
end;

procedure AntiAliasRect(Clip: TBitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);
var
  Memo, X, Y: Integer; (* Composantes primaires des points environnants *)
  P0, P1, P2: PByteArray;

begin
  if XFinal < XOrigin then
  begin
    // swap values
    Memo := XOrigin;
    XOrigin := XFinal;
    XFinal := Memo;
  end;
  if YFinal < YOrigin then
  begin
    Memo := YOrigin;
    YOrigin := YFinal;
    YFinal := Memo;
  end; (* si diff‚rence n‚gative *)
  XOrigin := Max(1, XOrigin);
  YOrigin := Max(1, YOrigin);
  XFinal := Min(Clip.Width - 2, XFinal);
  YFinal := Min(Clip.Height - 2, YFinal);
  Clip.PixelFormat := pf24bit;
  for Y := YOrigin to YFinal do
  begin
    P0 := Clip.ScanLine[Y - 1];
    P1 := Clip.ScanLine[Y];
    P2 := Clip.ScanLine[Y + 1];
    for X := XOrigin to XFinal do
    begin
      P1[X * 3] := (P0[X * 3] + P2[X * 3] + P1[(X - 1) * 3] + P1[(X + 1) * 3]) div 4;
      P1[X * 3 + 1] := (P0[X * 3 + 1] + P2[X * 3 + 1] + P1[(X - 1) * 3 + 1] + P1[(X + 1) * 3 + 1]) div 4;
      P1[X * 3 + 2] := (P0[X * 3 + 2] + P2[X * 3 + 2] + P1[(X - 1) * 3 + 2] + P1[(X + 1) * 3 + 2]) div 4;
    end;
  end;
end;

end.
