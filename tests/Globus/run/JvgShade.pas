{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgShade.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

UNIT JvgShade;

INTERFACE

USES
   Windows,
   Messages,
   SysUtils,
   Classes,
   Graphics,
   Controls,
   Forms,
   Dialogs,
   ExtCtrls,
   JvgTypes,
   JvgUtils,
   JVComponent,
   JvgCommClasses;

TYPE

   TJvgShade = CLASS(TJvCustomPanel)
   PRIVATE
      FImage: TBitmap;
      fLoaded: boolean;
      fNeedRebuildImage: boolean;

   PROTECTED
      PROPERTY Color;                   //...hide
      PROCEDURE Paint; OVERRIDE;
      PROCEDURE WMSize(VAR Msg: TMessage); MESSAGE WM_SIZE;
   PUBLIC
      PROPERTY Canvas;
      CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;
      DESTRUCTOR Destroy; OVERRIDE;

      PROCEDURE RemakeBackground;       //...for users
   PUBLISHED
      PROPERTY Align;
      PROPERTY Enabled;
      PROPERTY Visible;
      PROPERTY Image: TBitmap READ FImage WRITE FImage;
   END;

PROCEDURE Register;

IMPLEMENTATION
//uses test;

PROCEDURE Register;
BEGIN
END;
//*****************************************_____________LowLevel METHODS
//________________________________________________________

CONSTRUCTOR TJvgShade.Create(AOwner: TComponent);
BEGIN
   INHERITED Create(AOwner);

   Width := 105;
   Height := 105;
   Image := TBitmap.create;
   fLoaded := true;
   //...defaults
   fNeedRebuildImage := (csDesigning IN ComponentState) AND NOT (csLoading IN
      ComponentState);
END;
//________________________________________________________

DESTRUCTOR TJvgShade.Destroy;
BEGIN
   Image.Free;
   INHERITED Destroy;
END;
//________________________________________________________

PROCEDURE TJvgShade.WMSize(VAR Msg: TMessage);
BEGIN
   IF (csDesigning IN ComponentState) AND NOT (csLoading IN ComponentState) THEN
      RemakeBackground;
END;
//________________________________________________________

PROCEDURE TJvgShade.Paint;
VAR
   i, j, o                    : integer;
   RGB                        : COLORREF;
   R, G, B                    : byte;
CONST
   SHIFTCOLOR                 = $003939;
BEGIN
   IF fNeedRebuildImage THEN
   BEGIN
      Image.Width := Width;
      Image.Height := Height;
      //..prepare tabula rasa :)
      Image.Canvas.Brush.Color := Parent.Brush.Color;
      Image.Canvas.Brush.Style := bsSolid;
      Image.Canvas.FillRect(ClientRect);
      GetParentImageRect(self, Bounds(Left, Top, Width, Height),
         Image.Canvas.Handle);
      FOR j := 0 TO Height DO
         FOR i := 0 TO Width DO
            //	if Image.Canvas.Pixels[i,j] > SHIFTCOLOR then
         BEGIN
            IF o <> Image.Canvas.Pixels[i, j] THEN
            BEGIN
               //o := Image.Canvas.Pixels[i,j];
               //Form1.Memo1.Lines.Add(Format('%x',[o]));
            END;
            //	  if Image.Canvas.Pixels[i,j] = $C8B8A0 then
            RGB := Image.Canvas.Pixels[i, j];
            R := byte(RGB SHR 16);
            G := byte(RGB SHR 8);
            B := byte(RGB);
            //	  RShift := $
            Image.Canvas.Pixels[i, j] := Image.Canvas.Pixels[i, j] + SHIFTCOLOR;
         END;
      fNeedRebuildImage := false
   END;

   BitBlt(Canvas.Handle, 0, 0, Width, Height, Image.Canvas.Handle, 0, 0,
      SRCCOPY);

   IF csDesigning IN ComponentState THEN
      WITH Canvas DO
      BEGIN
         Pen.Color := clBlack;
         Pen.Style := psDash;
         Brush.Style := bsClear;
         Rectangle(0, 0, width, height);
      END;

END;

PROCEDURE TJvgShade.RemakeBackground;   //...for users
BEGIN
   fNeedRebuildImage := true;
   Repaint;
END;
//________________________________________________________

END.

