{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvRegion.PAS, released on 2001-02-28.

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

unit JvRegion;

{$OBJEXPORTALL On}

interface

// (rom) the JCL has a function for this job

uses
  Windows, Messages, SysUtils, Classes, Graphics, JvTypes, JvComponent;

type
  TJvRegion = class(TJvComponent)
  published
    function RegionFromBitmap(Image: TBitmap): HRGN;
  end;

implementation

{*******************************************************}

function TJvRegion.RegionFromBitmap(Image: TBitmap): HRGN;
var
  rgn1, rgn2: HRGN;
  startx, endx, x, y: Integer;
  TransparentColor: TRGBTriple;
  bmp: TBitmap;
  p: PRGBArray;
begin
  rgn1 := 0;

  bmp := TBitmap.Create;
  bmp.Assign(Image);
  bmp.PixelFormat := pf24Bit;

  if (bmp.Height > 0) and (bmp.Width > 0) then
  begin
    p := bmp.ScanLine[0];
    TransparentColor := p[0];
  end;

  for y := 0 to bmp.Height - 1 do
  begin
    x := 0;
    p := bmp.ScanLine[y];
    repeat
      while (x < bmp.Width) and (CompareMem(@p[x], @TransparentColor, 3)) do
        Inc(x);
      Inc(x);
      startx := x;
      while (x < bmp.Width) and (not (CompareMem(@p[x], @TransparentColor, 3))) do
        Inc(x);
      endx := x;

      // do we have some pixels?
      if startx < bmp.Width then
      begin
        if rgn1 = 0 then
          // Create a region to start with
          rgn1 := CreateRectRgn(startx + 1, y, endx, y + 1)
        else
        begin
          rgn2 := CreateRectRgn(startx + 1, y, endx, y + 1);
          if rgn2 <> 0 then
            CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
          DeleteObject(rgn2);
        end;
      end;
    until x >= Image.Width;
  end;

  bmp.Free;
  Result := rgn1;
end;

end.
