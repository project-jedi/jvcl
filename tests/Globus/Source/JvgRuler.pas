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

unit JvgRuler;

interface
uses
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

type
  TglSizeUnit = (fsuSantimetres, fsuInches, fsuPixels);

  TJvgRuler = class(TJvGraphicControl)
  private
    FUseUnit: TglSizeUnit;
    FOrientation: TglOrientation;
    LOGPIXELSX_,
      LOGPIXELSY_: integer;
    FPosition: integer;
    procedure SetPosition(const Value: integer);
  protected
    procedure Paint; override;
  public
    property Position: integer read FPosition write SetPosition;
  published
    property Align;
    property Font;
    property Orientation: TglOrientation read FOrientation write FOrientation
      default goHorizontal;
    property UseUnit: TglSizeUnit read FUseUnit write FUseUnit
      default fsuSantimetres;
  end;

procedure Register;

implementation
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure Register;
begin
end;
{~~~~~~~~~~~~~~~~~~~~~~~~~}

procedure TJvgRuler.Paint;
const
  Offset: array[boolean] of integer = (8, 3);
var
  x, y: single;
  pt: TPoint;
  str: string;
  R: TRect;
begin
  LOGPIXELSX_ := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
  LOGPIXELSY_ := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  Canvas.Font.Assign(Font);
  x := 0;
  y := 0;
  repeat
    x := x + 0.5;
    y := y + 0.5;
    //  FUseUnit := fsuSantimetres;
    case FUseUnit of
      {fsuPixels://...points
      begin
        //PosLabel.Caption := Format( '%u'+#13+'%u', [pt.x,pt.y] );
      end;}
      fsuInches: //...inchs
        begin
          pt.x := round(x * LOGPIXELSX_ * 1.541 / 10);
          pt.y := round(y * LOGPIXELSY_ * 1.541 / 10);
        end;
      fsuSantimetres: //...santimetres
        begin
          pt.x := round(x * LOGPIXELSX_ * 1.541 * 2.54 / 10);
          pt.y := round(y * LOGPIXELSY_ * 1.541 * 2.54 / 10);
        end;
      fsuPixels: //...pixels
        begin
          pt.x := round(x * 50);
          pt.y := round(y * 50);
        end;
    end;

    with Canvas do
      if Orientation = goHorizontal then
      begin
        if pt.x > Width then
          break;
        if x = trunc(x) then
        begin
          R := Rect(pt.x - 10, 0, pt.x + 10, Height);
          SetBkMode(Handle, TRANSPARENT);
          if UseUnit = fsuPixels then
            str := IntToStr(pt.x)
          else
            str := IntToStr(trunc(X));
          DrawText(Handle, PChar(str), Length(str), R, DT_SINGLELINE or
            DT_CENTER);
        end;
        MoveTo(pt.x, Height - Offset[x = trunc(x)]);
        LineTo(pt.x, Height - 1);
      end
      else
      begin
        if pt.y > Height then
          break;
        if y = trunc(y) then
        begin
          R := Rect(0, pt.y - 10, Width, pt.y + 10);
          SetBkMode(Handle, TRANSPARENT);
          if UseUnit = fsuPixels then
            str := IntToStr(pt.y)
          else
            str := IntToStr(trunc(Y));
          DrawText(Handle, PChar(str), Length(str), R, DT_SINGLELINE or
            DT_CENTER or DT_VCENTER);
        end;
        MoveTo(Width - Offset[y = trunc(y)], pt.y);
        LineTo(Width - 1, pt.y);
      end;

  until false;

  if Position > 0 then
    with Canvas do
      if Orientation = goHorizontal then
      begin
        MoveTo(Position - 2, Height - 4);
        LineTo(Position + 2, Height - 4);
        LineTo(Position, Height);
        LineTo(Position - 2, Height - 4);
      end
      else
      begin
        MoveTo(Width - 4, Position - 2);
        LineTo(Width - 4, Position + 2);
        LineTo(Width, Position);
        LineTo(Width - 4, Position - 2);
      end;
end;

procedure TJvgRuler.SetPosition(const Value: integer);
begin
  if FPosition = Value then
    exit;
  FPosition := Value;
  Invalidate;
end;

end.
