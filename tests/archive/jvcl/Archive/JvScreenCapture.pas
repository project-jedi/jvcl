{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvScreenCapture.PAS, released on 2001-02-28.

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

unit JvScreenCapture;

{$OBJEXPORTALL On}

interface

// (rom) the JCL has a fucntion for this

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms, JvComponent;

type
  TJvScreenCapture = class(TJvComponent)
  private
  public
    // (rom) changed to overload
    function CaptureScreen: TBitmap; overload;
    function CaptureScreen(Rec: TRect): TBitmap; overload;
  end;

implementation

{*********************************************************}

function TJvScreenCapture.CaptureScreen(Rec: TRect): TBitmap;
const
  NumColors = 256;
var
  R: TRect;
  C: TCanvas;
  LP: PLogPalette;
  Size: Integer;
  img: TImage;
begin
  img := TImage.Create(Self);
  img.Width := rec.Right - rec.Left;
  img.Height := rec.Bottom - rec.Top;
  R := Rec;
  C := TCanvas.Create;
  C.Handle := GetDC(HWND_DESKTOP);
  try
    Img.Canvas.CopyRect(Rect(0, 0, Rec.Right - Rec.Left, Rec.Bottom - Rec.Top), C, R);
    Size := SizeOf(TLogPalette) + (Pred(NumColors) * SizeOf(TPaletteEntry));
    LP := AllocMem(Size);
    try
      LP^.palVersion := $300;
      LP^.palNumEntries := NumColors;
      GetSystemPaletteEntries(C.Handle, 0, NumColors, LP^.palPalEntry);
      Img.Picture.Bitmap.Palette := CreatePalette(LP^);
    finally
      FreeMem(LP, Size);
    end
  finally
    ReleaseDC(HWND_DESKTOP, C.Handle);
    C.Free;
  end;
  Result := TBitmap.Create;
  Result.Assign(img.Picture.Bitmap);
  img.Free;
end;

{*********************************************************}

function TJvScreenCapture.CaptureScreen: TBitmap;
begin
  Result := CaptureScreen(Rect(0, 0, Screen.Width, Screen.Height));
end;

end.
