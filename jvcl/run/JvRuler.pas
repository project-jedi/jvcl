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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvRuler;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, SysUtils,
  JvComponent;

type
  TJvRulerUnit = (ruCentimeters, ruInches, ruPixels);
  TJvRulerOrientation = (roHorizontal, roVertical);

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvRuler = class(TJvGraphicControl)
  private
    FUseUnit: TJvRulerUnit;
    FOrientation: TJvRulerOrientation;
    FPosition: Double;
    procedure SetPosition(const Value: Double);
    procedure SetOrientation(Value: TJvRulerOrientation);
    procedure SetUseUnit(Value: TJvRulerUnit);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Font;
    property Height default 25;
    property Width default 300;
    property Orientation: TJvRulerOrientation read FOrientation write SetOrientation  default roHorizontal;
    property Position: Double read FPosition write SetPosition;
    property UseUnit: TJvRulerUnit read FUseUnit write SetUseUnit default ruCentimeters;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

const
  LogPixels: array [Boolean] of Integer = (LOGPIXELSY, LOGPIXELSX);

function InchesToPixels(DC: HDC; Value: Double; IsHorizontal: Boolean): Integer;
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal]));
end;

function CentimetersToPixels(DC: HDC; Value: Double; IsHorizontal: Boolean): Integer;
begin
  Result := Round(Value * GetDeviceCaps(DC, LogPixels[IsHorizontal]) / 2.54);
end;

//=== { TJvRuler } ===========================================================

constructor TJvRuler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOrientation := roHorizontal;
  FUseUnit := ruCentimeters;
  Height := 25;
  Width := 300;
end;

procedure TJvRuler.Paint;
const
  Offset: array [Boolean] of Integer = (8, 3);
var
  X, Y: Double;
  PX, PY, Pos: Integer;
  S: string;
  R: TRect;
begin
  Canvas.Font := Font;
  X := 0;
  Y := 0;
  repeat
    X := X + 0.5;
    Y := Y + 0.5;
    case FUseUnit of
      ruInches:
        begin
          PX := InchesToPixels(Canvas.Handle, X, True);
          PY := InchesToPixels(Canvas.Handle, Y, False);
          Pos := InchesToPixels(Canvas.Handle, Position, Orientation = roHorizontal);
        end;
      ruCentimeters:
        begin
          PX := CentimetersToPixels(Canvas.Handle, X, True);
          PY := CentimetersToPixels(Canvas.Handle, Y, False);
          Pos := CentimetersToPixels(Canvas.Handle, Position, Orientation = roHorizontal);
        end;
    else // ruPixels
      PX := Round(X * 50);
      PY := Round(Y * 50);
      Pos := Round(Position);
    end;

    SetBkMode(Canvas.Handle, TRANSPARENT);
    if (PX < Width) or (PY < Height) then
      with Canvas do
        if Orientation = roHorizontal then
        begin
          if X = Trunc(X) then
          begin
            R := Rect(PX - 10, 0, PX + 10, Height);
            if UseUnit = ruPixels then
              S := IntToStr(PX)
            else
              S := IntToStr(Trunc(X));
            R := Rect(PX - TextWidth(S), 0, PX + TextWidth(S), Height);
            Windows.DrawText(Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_CENTER);
          end;
          MoveTo(PX, Height - Offset[X = Trunc(X)]);
          LineTo(PX, Height);
        end
        else
        begin
          if Y = Trunc(Y) then
          begin
            if UseUnit = ruPixels then
              S := IntToStr(PY)
            else
              S := IntToStr(Trunc(Y));
            R := Rect(0, PY - TextHeight(S), Width, PY + TextHeight(S));
            Windows.DrawText(Handle, PChar(S), Length(S), R, DT_SINGLELINE or DT_VCENTER);
          end;
          MoveTo(Width - Offset[Y = Trunc(Y)], PY);
          LineTo(Width, PY);
        end;
  until ((Orientation = roHorizontal) and (PX > Width)) or
    ((Orientation = roVertical) and (PY > Height));

  if Position > 0.0 then
    with Canvas do
      if Orientation = roHorizontal then
      begin
        MoveTo(Pos - 2, Height - 4);
        LineTo(Pos + 2, Height - 4);
        LineTo(Pos, Height);
        LineTo(Pos - 2, Height - 4);
      end
      else
      begin
        MoveTo(Width - 4, Pos - 2);
        LineTo(Width - 4, Pos + 2);
        LineTo(Width, Pos);
        LineTo(Width - 4, Pos - 2);
      end;
end;

procedure TJvRuler.SetPosition(const Value: Double);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Invalidate;
  end;
end;

procedure TJvRuler.SetOrientation(Value: TJvRulerOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if csDesigning in ComponentState then
      SetBounds(Left, Top, Height, Width);
    Invalidate;
  end;
end;

procedure TJvRuler.SetUseUnit(Value: TJvRulerUnit);
begin
  if FUseUnit <> Value then
  begin
    FUseUnit := Value;
    Invalidate;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
