{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRuler.PAS, released on 2003-01-15.

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

UNIT JvgRuler;

INTERFACE
USES
   Windows,
   Messages,
   Classes,
   Controls,
   Graphics,
   JvgTypes,
   JvgCommClasses,
   JvgUtils,
   Forms,
   OleCtnrs,
   ExtCtrls,
   JVComponent,
   SysUtils;

TYPE
   TglSizeUnit = (fsuSantimetres, fsuInches, fsuPixels);

   TJvgRuler = CLASS(TJvGraphicControl)
   PRIVATE
      FUseUnit: TglSizeUnit;
      FOrientation: TglOrientation;
      LOGPIXELSX_,
         LOGPIXELSY_: integer;
      FPosition: integer;
      PROCEDURE SetPosition(CONST Value: integer);
   PROTECTED
      PROCEDURE Paint; OVERRIDE;
   PUBLIC
      PROPERTY Position: integer READ FPosition WRITE SetPosition;
   PUBLISHED
      PROPERTY Align;
      PROPERTY Font;
      PROPERTY Orientation: TglOrientation READ FOrientation WRITE FOrientation
         DEFAULT goHorizontal;
      PROPERTY UseUnit: TglSizeUnit READ FUseUnit WRITE FUseUnit
         DEFAULT fsuSantimetres;
   END;

PROCEDURE Register;

IMPLEMENTATION
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE Register;
BEGIN
END;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

PROCEDURE TJvgRuler.Paint;
CONST
   Offset                     : ARRAY[boolean] OF integer = (8, 3);
VAR
   x, y                       : single;
   pt                         : TPoint;
   str                        : STRING;
   R                          : TRect;
BEGIN
   LOGPIXELSX_ := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
   LOGPIXELSY_ := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
   Canvas.Font.Assign(Font);
   x := 0;
   y := 0;
   REPEAT
      x := x + 0.5;
      y := y + 0.5;
      //  FUseUnit := fsuSantimetres;
      CASE FUseUnit OF
         {fsuPixels://...points
         begin
           //PosLabel.Caption := Format( '%u'+#13+'%u', [pt.x,pt.y] );
         end;}
         fsuInches:                     //...inchs
            BEGIN
               pt.x := round(x * LOGPIXELSX_ * 1.541 / 10);
               pt.y := round(y * LOGPIXELSY_ * 1.541 / 10);
            END;
         fsuSantimetres:                //...santimetres
            BEGIN
               pt.x := round(x * LOGPIXELSX_ * 1.541 * 2.54 / 10);
               pt.y := round(y * LOGPIXELSY_ * 1.541 * 2.54 / 10);
            END;
         fsuPixels:                     //...pixels
            BEGIN
               pt.x := round(x * 50);
               pt.y := round(y * 50);
            END;
      END;

      WITH Canvas DO
         IF Orientation = goHorizontal THEN
         BEGIN
            IF pt.x > Width THEN
               break;
            IF x = trunc(x) THEN
            BEGIN
               R := Rect(pt.x - 10, 0, pt.x + 10, Height);
               SetBkMode(Handle, TRANSPARENT);
               IF UseUnit = fsuPixels THEN
                  str := IntToStr(pt.x)
               ELSE
                  str := IntToStr(trunc(X));
               DrawText(Handle, PChar(str), Length(str), R, DT_SINGLELINE OR
                  DT_CENTER);
            END;
            MoveTo(pt.x, Height - Offset[x = trunc(x)]);
            LineTo(pt.x, Height - 1);
         END
         ELSE
         BEGIN
            IF pt.y > Height THEN
               break;
            IF y = trunc(y) THEN
            BEGIN
               R := Rect(0, pt.y - 10, Width, pt.y + 10);
               SetBkMode(Handle, TRANSPARENT);
               IF UseUnit = fsuPixels THEN
                  str := IntToStr(pt.y)
               ELSE
                  str := IntToStr(trunc(Y));
               DrawText(Handle, PChar(str), Length(str), R, DT_SINGLELINE OR
                  DT_CENTER OR DT_VCENTER);
            END;
            MoveTo(Width - Offset[y = trunc(y)], pt.y);
            LineTo(Width - 1, pt.y);
         END;

   UNTIL false;

   IF Position > 0 THEN
      WITH Canvas DO
         IF Orientation = goHorizontal THEN
         BEGIN
            MoveTo(Position - 2, Height - 4);
            LineTo(Position + 2, Height - 4);
            LineTo(Position, Height);
            LineTo(Position - 2, Height - 4);
         END
         ELSE
         BEGIN
            MoveTo(Width - 4, Position - 2);
            LineTo(Width - 4, Position + 2);
            LineTo(Width, Position);
            LineTo(Width - 4, Position - 2);
         END;
END;

PROCEDURE TJvgRuler.SetPosition(CONST Value: integer);
BEGIN
   IF FPosition = Value THEN
      exit;
   FPosition := Value;
   Invalidate;
END;

END.

