{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorRotate.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorRotate;

{$I jvcl.inc}

interface

uses
  Classes, Graphics,
  JvFullColorSpaces;

type
  TJvRotateColor = (rcCommon, rcRed, rcGreen, rcBlue);

  TJvSaturationMethod = (smRange, smLoop);

  TJvRotateValue = record
    Value: -255..255;
    SaturationMethod: TJvSaturationMethod;
  end;

  TJvAxisDelta = array [TJvAxisIndex] of TJvRotateValue;

  TJvColorDelta = record
    ColorID: TJvFullColorSpaceID;
    AxisRed: TJvAxisDelta;
    AxisGreen: TJvAxisDelta;
    AxisBlue: TJvAxisDelta;
  end;

function ChangeColorDeltaSpace(ColorDelta: TJvColorDelta;
  NewID: TJvFullColorSpaceID): TJvColorDelta;
function RotateColor(AColor: TJvFullColor;
  AColorDelta: TJvColorDelta): TJvFullColor;
procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap;
  AColorDelta: TJvColorDelta);

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math;

function ChangeColorDeltaSpace(ColorDelta: TJvColorDelta;
  NewID: TJvFullColorSpaceID): TJvColorDelta;
var
  I: TJvAxisIndex;
  SourceColorSpace, DestColorSpace: TJvColorSpace;

  function GetAxisDelta(AColor: TColor): TJvAxisDelta;
  var
    I: TJvAxisIndex;
    SourceColor, DestColor: TJvFullColor;
  begin
    SourceColor := SourceColorSpace.ConvertFromColor(AColor);
    DestColor := DestColorSpace.ConvertFromColor(AColor);
    SourceColor := RotateColor(SourceColor, ColorDelta);
    SourceColor := SourceColorSpace.ConvertToColor(SourceColor);
    SourceColor := DestColorSpace.ConvertFromColor(SourceColor);
    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
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
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    Result.AxisRed[I].SaturationMethod := ColorDelta.AxisRed[I].SaturationMethod;
    Result.AxisGreen[I].SaturationMethod := ColorDelta.AxisGreen[I].SaturationMethod;
    Result.AxisBlue[I].SaturationMethod := ColorDelta.AxisBlue[I].SaturationMethod;
  end;
end;

// (rom) reworked for loops
function RotateColor(AColor: TJvFullColor; AColorDelta: TJvColorDelta): TJvFullColor;
var
  I: TJvAxisIndex;
  MinAxis: array [TJvAxisIndex] of Byte;
  MaxAxis: array [TJvAxisIndex] of Byte;
  ValueAxis: array [TJvAxisIndex] of Integer;
  ValueRed, ValueGreen, ValueBlue: TJvFullColor;
  ColorRed, ColorGreen, ColorBlue: TJvFullColor;
  SourceColorSpace, DeltaColorSpace: TJvColorSpace;
  LColorID: TJvFullColorSpaceID;
  LColor: TColor;

  function DoRotate(AValue: TJvFullColor; AAxisDelta: TJvAxisDelta): TColor;
  var
    I: TJvAxisIndex;
    Range: Integer;
  begin
    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      ValueAxis[I] := Integer(GetAxisValue(AValue, I)) + AAxisDelta[I].Value;
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
    LColorID := GetColorSpaceID(AColor);
    SourceColorSpace := ColorSpace[LColorID];
    LColor := SourceColorSpace.ConvertToColor(AColor);

    DeltaColorSpace := ColorSpace[AColorDelta.ColorID];

    with DeltaColorSpace do
    begin
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        MinAxis[I] := AxisMin[I];
        MaxAxis[I] := AxisMax[I];
      end;
      ValueRed := ConvertFromColor((LColor and $000000FF) or (MinAxis[axIndex1] shl 8) or (MinAxis[axIndex2] shl 16));
      ValueGreen := ConvertFromColor((MinAxis[axIndex0]) or (LColor and $0000FF00) or (MinAxis[axIndex2] shl 16));
      ValueBlue := ConvertFromColor((MinAxis[axIndex0]) or (MinAxis[axIndex1] shl 8) or (LColor and $00FF0000));

      ColorRed := DoRotate(ValueRed, AColorDelta.AxisRed);
      ColorGreen := DoRotate(ValueGreen, AColorDelta.AxisGreen);
      ColorBlue := DoRotate(ValueBlue, AColorDelta.AxisBlue);

      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        ValueAxis[I] := (ColorRed and $000000FF) + (ColorGreen and $000000FF) + (ColorBlue and $000000FF);
        // (rom) the test was wrong in the original implementation
        if ValueAxis[I] > 255 then
          ValueAxis[I] := 255;
        ColorRed := ColorRed shr 8;
        ColorGreen := ColorGreen shr 8;
        ColorBlue := ColorBlue shr 8;
      end;

      LColor := ValueAxis[axIndex0] or (ValueAxis[axIndex1] shl 8) or (ValueAxis[axIndex2] shl 16);
    end;

    Result := SourceColorSpace.ConvertFromColor(LColor);
  end;
end;

procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap; AColorDelta: TJvColorDelta);
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

