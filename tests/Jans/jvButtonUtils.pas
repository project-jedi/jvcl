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
{$I JEDI.INC}
unit JvButtonUtils;

interface
uses
  Windows, SysUtils, Classes, Graphics, math;

procedure AntiAlias(clip: tbitmap);
procedure AntiAliasRect(clip: tbitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);

implementation

procedure AntiAlias(clip: tbitmap);
begin
  AntiAliasRect(clip, 0, 0, clip.width, clip.height);
end;

procedure AntiAliasRect(clip: tbitmap; XOrigin, YOrigin,
  XFinal, YFinal: Integer);
var
  Memo, x, y: Integer; (* Composantes primaires des points environnants *)
  p0, p1, p2: pbytearray;

begin
  if XFinal < XOrigin then
  begin
    Memo := XOrigin;
    XOrigin := XFinal;
    XFinal := Memo;
  end; (* Inversion des valeurs   *)
  if YFinal < YOrigin then
  begin
    Memo := YOrigin;
    YOrigin := YFinal;
    YFinal := Memo;
  end; (* si diff‚rence n‚gative*)
  XOrigin := max(1, XOrigin);
  YOrigin := max(1, YOrigin);
  XFinal := min(clip.width - 2, XFinal);
  YFinal := min(clip.height - 2, YFinal);
  clip.PixelFormat := pf24bit;
  for y := YOrigin to YFinal do
  begin
    p0 := clip.ScanLine[y - 1];
    p1 := clip.scanline[y];
    p2 := clip.ScanLine[y + 1];
    for x := XOrigin to XFinal do
    begin
      p1[x * 3] := (p0[x * 3] + p2[x * 3] + p1[(x - 1) * 3] + p1[(x + 1) * 3]) div 4;
      p1[x * 3 + 1] := (p0[x * 3 + 1] + p2[x * 3 + 1] + p1[(x - 1) * 3 + 1] + p1[(x + 1) * 3 + 1]) div 4;
      p1[x * 3 + 2] := (p0[x * 3 + 2] + p2[x * 3 + 2] + p1[(x - 1) * 3 + 2] + p1[(x + 1) * 3 + 2]) div 4;
    end;
  end;
end;

end.
