{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorRotate.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorRotate;

{$I jvcl.inc}

interface

uses
  Classes, Graphics,
  ColorSpaces;

type
  TRotateColor = (rcCommon, rcRed, rcGreen, rcBlue);

  TSaturationMethod = (smRange, smLoop);

  TRotateValue = record
    Value: -255..255;
    SaturationMethod: TSaturationMethod;
  end;

  TAxisDelta = array [TAxisIndex] of TRotateValue;

  TColorDelta = record
    ColorID: TColorID;
    AxisRed: TAxisDelta;
    AxisGreen: TAxisDelta;
    AxisBlue: TAxisDelta;
  end;

function ChangeColorDeltaSpace(ColorDelta: TColorDelta;
  NewID: TColorID): TColorDelta;
function RotateColor(AColor: TFullColor;
  AColorDelta: TColorDelta): TFullColor;
procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap;
  AColorDelta: TColorDelta);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;

function ChangeColorDeltaSpace(ColorDelta: TColorDelta;
  NewID: TColorID): TColorDelta;
var
  I: TAxisIndex;
  SourceColorSpace, DestColorSpace: TColorSpace;

  function GetAxisDelta(AColor: TColor): TAxisDelta;
  var
    I: TAxisIndex;
    SourceColor, DestColor: TFullColor;
  begin
    SourceColor := SourceColorSpace.ConvertFromRGB(TFullColor(AColor));
    DestColor := DestColorSpace.ConvertFromRGB(TFullColor(AColor));
    SourceColor := RotateColor(SourceColor, ColorDelta);
    SourceColor := SourceColorSpace.ConvertToRGB(SourceColor);
    SourceColor := DestColorSpace.ConvertFromRGB(SourceColor);
    for I := Low(TAxisIndex) to High(TAxisIndex) do
    begin
      Result[I].Value := Integer(SourceColor and $000000FF) - Integer(DestColor and $000000FF);
      SourceColor := SourceColor shr 8;
      DestColor := DestColor shr 8;
    end;
  end;

begin
  with ColorSpaceManager do
  begin
    SourceColorSpace := ColorSpace[ColorDelta.ColorID];
    DestColorSpace := ColorSpace[NewID];
  end;
  Result.AxisRed := GetAxisDelta(clRed);
  Result.AxisGreen := GetAxisDelta(clLime);
  Result.AxisBlue := GetAxisDelta(clBlue);
  for I := Low(TAxisIndex) to High(TAxisIndex) do
  begin
    Result.AxisRed[I].SaturationMethod := ColorDelta.AxisRed[I].SaturationMethod;
    Result.AxisGreen[I].SaturationMethod := ColorDelta.AxisGreen[I].SaturationMethod;
    Result.AxisBlue[I].SaturationMethod := ColorDelta.AxisBlue[I].SaturationMethod;
  end;
end;

// (rom) reworked for loops
function RotateColor(AColor: TFullColor; AColorDelta: TColorDelta): TFullColor;
var
  I: TAxisIndex;
  MinAxis: array [TAxisIndex] of Byte;
  MaxAxis: array [TAxisIndex] of Byte;
  ValueAxis: array [TAxisIndex] of Integer;
  ValueRed, ValueGreen, ValueBlue: Integer;
  SourceColorSpace, DeltaColorSpace: TColorSpace;
  LColorID: TColorID;

  function DoRotate(AValue: Cardinal; AAxisDelta: TAxisDelta): Cardinal;
  var
    I: TAxisIndex;
    Range: Integer;
  begin
    for I := Low(TAxisIndex) to High(TAxisIndex) do
    begin
      ValueAxis[I] := Integer(AValue and $000000FF) + AAxisDelta[I].Value;
      AValue := AValue shr 8;
      if AAxisDelta[I].SaturationMethod = smRange then
      begin
        if ValueAxis[I] > MaxAxis[I] then
          ValueAxis[I] := MaxAxis[I];
        if ValueAxis[I] < MinAxis[I] then
          ValueAxis[I] := MinAxis[I];
      end
      else
      begin
        Range := MaxAxis[I] - MinAxis[I] + 1;
        while ValueAxis[I] < MinAxis[I] do
          Inc(ValueAxis[I], Range);
        while ValueAxis[I] > MaxAxis[I] do
          Dec(ValueAxis[I], Range);
      end;
    end;
    Result :=
      (ValueAxis[axIndex0]) or (ValueAxis[axIndex1] shl 8) or (ValueAxis[axIndex2] shl 16);
  end;

begin
  with ColorSpaceManager do
  begin
    LColorID := GetColorID(AColor);
    SourceColorSpace := ColorSpace[LColorID];
    if LColorID <> csRGB then
      AColor := SourceColorSpace.ConvertToRGB(AColor);

    DeltaColorSpace := ColorSpace[AColorDelta.ColorID];

    with DeltaColorSpace do
    begin
      for I := Low(TAxisIndex) to High(TAxisIndex) do
      begin
        MinAxis[I] := AxisMin[I];
        MaxAxis[I] := AxisMax[I];
      end;
      ValueRed := ConvertFromRGB((AColor and $000000FF) or (MinAxis[axIndex1] shl 8) or (MinAxis[axIndex2] shl 16));
      ValueGreen := ConvertFromRGB((MinAxis[axIndex0]) or (AColor and $0000FF00) or (MinAxis[axIndex2] shl 16));
      ValueBlue := ConvertFromRGB((MinAxis[axIndex0]) or (MinAxis[axIndex1] shl 8) or (AColor and $00FF0000));

      ValueRed := ConvertToRGB(DoRotate(ValueRed, AColorDelta.AxisRed));
      ValueGreen := ConvertToRGB(DoRotate(ValueGreen, AColorDelta.AxisGreen));
      ValueBlue := ConvertToRGB(DoRotate(ValueBlue, AColorDelta.AxisBlue));

      for I := Low(TAxisIndex) to High(TAxisIndex) do
      begin
        ValueAxis[I] := (ValueRed and $000000FF) + (ValueGreen and $000000FF) + (ValueBlue and $000000FF);
        // (rom) the test was wrong in the original implementation
        if ValueAxis[I] > 255 then
          ValueAxis[I] := 255;
        ValueRed := ValueRed shr 8;
        ValueGreen := ValueGreen shr 8;
        ValueBlue := ValueBlue shr 8;
      end;

      AColor := ValueAxis[axIndex0] or (ValueAxis[axIndex1] shl 8) or (ValueAxis[axIndex2] shl 16);
    end;

    if LColorID <> csRGB then
      Result := SourceColorSpace.ConvertFromRGB(AColor)
    else
      Result := AColor;
  end;
end;

procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap; AColorDelta: TColorDelta);
begin
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

