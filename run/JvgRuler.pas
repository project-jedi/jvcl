{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgRuler.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin att yandex dott ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvgRuler;

{$I jvcl.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics,
  Forms, OleCtnrs, ExtCtrls, SysUtils,
  JvComponent, JvgCommClasses, JvgUtils;

type
  TJvgSizeUnit = (fsuCentimeters, fsuInches, fsuPixels);
  TJvgOrientation = (goHorizontal, goVertical);

  TJvgRuler = class(TJvGraphicControl)
  private
    FUseUnit: TJvgSizeUnit;
    FOrientation: TJvgOrientation;
    FPosition: Integer;
    procedure SetPosition(const Value: Integer);
    procedure SetOrientation(Value: TJvgOrientation);
    procedure SetUseUnit(Value: TJvgSizeUnit);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Position: Integer read FPosition write SetPosition;
  published
    property Align;
    property Font;
    property Orientation: TJvgOrientation read FOrientation write SetOrientation  default goHorizontal;
    property UseUnit: TJvgSizeUnit read FUseUnit write SetUseUnit default fsuCentimeters;
  end;

implementation

constructor TJvgRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation := goHorizontal;
  FUseUnit := fsuCentimeters;
end;

procedure TJvgRuler.Paint;
const
  Offset: array [Boolean] of Integer = (8, 3);
var
  X, Y: Single;
  Pt: TPoint;
  S: string;
  R: TRect;
begin
  Canvas.Font.Assign(Font);
  X := 0;
  Y := 0;
  repeat
    X := X + 0.5;
    Y := Y + 0.5;
    case FUseUnit of
      fsuInches:
        begin
          Pt.X := InchesToPixels(Canvas.Handle, X, True);
          Pt.Y := InchesToPixels(Canvas.Handle, Y, False);
        end;
      fsuCentimeters:
        begin
          Pt.X := CentimetersToPixels(Canvas.Handle, X, True);
          Pt.Y := CentimetersToPixels(Canvas.Handle, Y, False);
        end;
      fsuPixels:
        begin
          Pt.X := Round(X * 50);
          Pt.Y := Round(Y * 50);
        end;
    end;

    with Canvas do
      if Orientation = goHorizontal then
      begin
        if Pt.X > Width then
          Break;
        if X = Trunc(X) then
        begin
          R := Rect(Pt.X - 10, 0, Pt.X + 10, Height);
          SetBkMode(Handle, TRANSPARENT);
          if UseUnit = fsuPixels then
            S := IntToStr(Pt.X)
          else
            S := IntToStr(Trunc(X));
          Windows.DrawText(Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_CENTER);
        end;
        MoveTo(Pt.X, Height - Offset[X = Trunc(X)]);
        LineTo(Pt.X, Height - 1);
      end
      else
      begin
        if Pt.Y > Height then
          Break;
        if Y = Trunc(Y) then
        begin
          R := Rect(0, Pt.Y - 10, Width, Pt.Y + 10);
          SetBkMode(Handle, TRANSPARENT);
          if UseUnit = fsuPixels then
            S := IntToStr(Pt.Y)
          else
            S := IntToStr(Trunc(Y));
          Windows.DrawText(Handle, PChar(S), Length(S), R, DT_SINGLELINE or
            DT_CENTER or DT_VCENTER);
        end;
        MoveTo(Width - Offset[Y = Trunc(Y)], Pt.Y);
        LineTo(Width - 1, Pt.Y);
      end;
  until False;

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

procedure TJvgRuler.SetPosition(const Value: Integer);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TJvgRuler.SetOrientation(Value: TJvgOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    //Paint;
    Invalidate;
  end;
end;

procedure TJvgRuler.SetUseUnit(Value: TJvgSizeUnit);
begin
  if FUseUnit <> Value then
  begin
    FUseUnit := Value;
    //Paint;
    Invalidate;
  end;
end;

end.

