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
  Windows, Classes, Graphics,
  JvFullColorSpaces;

type
  TJvRotateColor = (rcCommon, rcRed, rcGreen, rcBlue);

  TJvSaturationMethod = (smRange, smLoop);

  TJvRotateValueType = -255..255;

  TJvRotateValue = class(TPersistent)
  private
    FValue: TJvRotateValueType;
    FSaturationMethod: TJvSaturationMethod;
  public
    property Value: TJvRotateValueType read FValue write FValue default 0;
    property SaturationMethod: TJvSaturationMethod read FSaturationMethod write FSaturationMethod default smRange;

    procedure Assign(Value: TJvRotateValue); reintroduce;
  end;

  TJvAxisDelta = class(TPersistent)
  private
    FConstituents: array [TJvAxisIndex] of TJvRotateValue;

    function GetConstituents(Index: TJvAxisIndex): TJvRotateValue;
    procedure SetConstituents(Index: TJvAxisIndex;
      const Value: TJvRotateValue);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Value: TJvAxisDelta); reintroduce;

    property Constituents[Index: TJvAxisIndex]:TJvRotateValue read GetConstituents write SetConstituents; default;
  end;

  TJvColorDelta = class(TPersistent)
  private
    FColorID: TJvFullColorSpaceID;
    FAxisRed: TJvAxisDelta;
    FAxisGreen: TJvAxisDelta;
    FAxisBlue: TJvAxisDelta;
    procedure SetAxisBlue(const Value: TJvAxisDelta);
    procedure SetAxisGreen(const Value: TJvAxisDelta);
    procedure SetAxisRed(const Value: TJvAxisDelta);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Value: TJvColorDelta); reintroduce;

    property ColorID: TJvFullColorSpaceID read FColorID write FColorID default csRGB;
    property AxisRed: TJvAxisDelta read FAxisRed write SetAxisRed;
    property AxisGreen: TJvAxisDelta read FAxisGreen write SetAxisGreen;
    property AxisBlue: TJvAxisDelta read FAxisBlue write SetAxisBlue;
  end;

function RotateColor(AColor: TJvFullColor;
  AColorDelta: TJvColorDelta): TJvFullColor;
procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap;
  AColorDelta: TJvColorDelta);

implementation

  {$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
  {$ENDIF UNITVERSIONING}

// (rom) reworked for loops
function RotateColor(AColor: TJvFullColor; AColorDelta: TJvColorDelta): TJvFullColor;
var
  I: TJvAxisIndex;
  MinAxis: array [TJvAxisIndex] of Byte;
  MaxAxis: array [TJvAxisIndex] of Byte;
  ValueAxis: array [TJvAxisIndex] of SmallInt;
  ColorRed, ColorGreen, ColorBlue: TColor;
  MaxColorAxis:Integer;
  SourceColorSpace, DeltaColorSpace: TJvColorSpace;
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
      end else begin
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
    SourceColorSpace := ColorSpace[GetColorSpaceID(AColor)];
    LColor := SourceColorSpace.ConvertToColor(AColor);
    DeltaColorSpace := ColorSpace[AColorDelta.ColorID];

    with DeltaColorSpace do
    begin
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        MinAxis[I] := AxisMin[I];
        MaxAxis[I] := AxisMax[I];
      end;
      ColorRed := ConvertToColor(DoRotate(ConvertFromColor(LColor and $000000FF), AColorDelta.AxisRed));
      ColorGreen := ConvertToColor(DoRotate(ConvertFromColor(LColor and $0000FF00), AColorDelta.AxisGreen));
      ColorBlue := ConvertToColor(DoRotate(ConvertFromColor(LColor and $00FF0000), AColorDelta.AxisBlue));

      MaxColorAxis:=255;
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        ValueAxis[I] := (ColorRed and $FF) + (ColorGreen and $FF) + (ColorBlue and $FF);
        if ValueAxis[I] > MaxColorAxis then
          MaxColorAxis := ValueAxis[I];
        ColorRed:=ColorRed shr 8;
        ColorGreen:=ColorGreen shr 8;
        ColorBlue:=ColorBlue shr 8;
      end;

      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        ValueAxis[I] := ValueAxis[I] + 255 - MaxColorAxis;
        if ValueAxis[I] < 0 then
          ValueAxis[I] := 0;
      end;
      LColor := ValueAxis[axIndex0] or (ValueAxis[axIndex1] shl 8) or (ValueAxis[axIndex2] shl 16);
    end;
    Result := SourceColorSpace.ConvertFromColor(LColor);
  end;
end;

procedure RotateBitmap(SourceBitmap, DestBitmap: TBitmap; AColorDelta: TJvColorDelta);
type
  TFullColorValue = array [TJvAxisIndex] of SmallInt;
  PFullColorValue = ^TFullColorValue;
  {$IFNDEF COMPILER6_UP}
  PCardinal = ^Cardinal;
  {$ENDIF COMPILER6_UP}
var
  OriginalPixelFormat: TPixelFormat;
  Colors: array [TJvAxisIndex,Byte] of TFullColorValue;
  ColorR, ColorB, ColorG, ColorFusion: TFullColorValue;
  I: TJvAxisIndex;
  J: Byte;
  X, Y: Integer;
  MinAxis: array [TJvAxisIndex] of SmallInt;
  MaxAxis: array [TJvAxisIndex] of SmallInt;
  MaxColorAxis: SmallInt;
  DeltaColorSpace: TJvColorSpace;
  DestLine, SourceLine: PCardinal;
  procedure DoRotate(Color:TColor; AAxisDelta: TJvAxisDelta; out DestColor:TFullColorValue);
  var
    I: TJvAxisIndex;
    Range: Integer;
    FullColor: TJvFullColor;
    ColorValue: TFullColorValue;
  begin
    FullColor := DeltaColorSpace.ConvertFromColor(Color);
    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      ColorValue[I] := Integer(GetAxisValue(FullColor, I)) + AAxisDelta[I].Value;
      if AAxisDelta[I].SaturationMethod = smRange then
      begin
        if ColorValue[I] > MaxAxis[I] then
          ColorValue[I] := MaxAxis[I];
        if ColorValue[I] < MinAxis[I] then
          ColorValue[I] := MinAxis[I];
      end else begin
        Range := MaxAxis[I] - MinAxis[I] + 1;
        while ColorValue[I] < MinAxis[I] do
          Inc(ColorValue[I], Range);
        while ColorValue[I] > MaxAxis[I] do
          Dec(ColorValue[I], Range);
      end;
    end;
    Color := DeltaColorSpace.ConvertToColor(ColorValue[axIndex0] or
      (ColorValue[axIndex1] shl 8) or (ColorValue[axIndex2] shl 16));
    DestColor[axIndex0] := Color and $FF;
    DestColor[axIndex1] := (Color shr 8) and $FF;
    DestColor[axIndex2] := (Color shr 16) and $FF;
  end;
begin
  DestBitmap.Width := SourceBitmap.Width;
  DestBitmap.Height := SourceBitmap.Height;
  OriginalPixelFormat := SourceBitmap.PixelFormat;
  SourceBitmap.PixelFormat := pf32bit;
  DestBitmap.PixelFormat := pf32bit;
  with ColorSpaceManager do
  begin
    DeltaColorSpace := ColorSpace[AColorDelta.ColorID];
    with DeltaColorSpace do
    begin
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        MinAxis[I] := AxisMin[I];
        MaxAxis[I] := AxisMax[I];
      end;
      for J := Low(Byte) to High(Byte) do
      begin
        DoRotate(TColor(J),AColorDelta.AxisRed,Colors[axIndex0,J]);
        DoRotate(TColor(J shl 8),AColorDelta.AxisGreen,Colors[axIndex1,J]);
        DoRotate(TColor(J shl 16),AColorDelta.AxisBlue,Colors[axIndex2,J]);
      end;

      for Y := 0 to DestBitmap.Height-1 do
      begin
        SourceLine := SourceBitmap.ScanLine[Y];
        DestLine := DestBitmap.ScanLine[Y];
        for X := 0 to DestBitmap.Width-1 do
        begin
          ColorR := Colors[axIndex0,(SourceLine^ shr 16) and $FF];       //
          ColorG := Colors[axIndex1,(SourceLine^ shr 8) and $FF];        // Bitmap Color Format is
          ColorB := Colors[axIndex2,(SourceLine^) and $FF];              // (MSB)0RGB(LSB)
          ColorFusion[axIndex0] := ColorR[axIndex0] + ColorG[axIndex0] + ColorB[axIndex0];
          ColorFusion[axIndex1] := ColorR[axIndex1] + ColorG[axIndex1] + ColorB[axIndex1];
          ColorFusion[axIndex2] := ColorR[axIndex2] + ColorG[axIndex2] + ColorB[axIndex2];
          MaxColorAxis := 255;
          if ColorFusion[axIndex0] > MaxColorAxis then
            MaxColorAxis := ColorFusion[axIndex0];
          if ColorFusion[axIndex1] > MaxColorAxis then
            MaxColorAxis := ColorFusion[axIndex1];
          if ColorFusion[axIndex2] > MaxColorAxis then
            MaxColorAxis := ColorFusion[axIndex2];
          ColorFusion[axIndex0] := ColorFusion[axIndex0] + 255 - MaxColorAxis;
          if ColorFusion[axIndex0] < 0 then
            ColorFusion[axIndex0] := 0;
          ColorFusion[axIndex1] := ColorFusion[axIndex1] + 255 - MaxColorAxis;
          if ColorFusion[axIndex1] < 0 then
            ColorFusion[axIndex1] := 0;
          ColorFusion[axIndex2] := ColorFusion[axIndex2] + 255 - MaxColorAxis;
          if ColorFusion[axIndex2] < 0 then
            ColorFusion[axIndex2] := 0;
          DestLine^ :=            // Bitmap Color Format is (MSB)0RGB(LSB)
              (ColorFusion[axIndex0] shl 16) or (ColorFusion[axIndex1] shl 8) or (ColorFusion[axIndex2]);
          Inc(SourceLine);
          Inc(DestLine);
        end;
      end;
    end;
  end;
  SourceBitmap.PixelFormat := OriginalPixelFormat;
  DestBitmap.PixelFormat := OriginalPixelFormat;
end;

{ TJvAxisDelta }

procedure TJvAxisDelta.Assign(Value: TJvAxisDelta);
var
  Index: TJvAxisIndex;
begin
  for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    FConstituents[Index].Assign(Value[Index]);
end;

constructor TJvAxisDelta.Create;
var
  Index: TJvAxisIndex;
begin
  inherited Create;

  for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    FConstituents[Index] := TJvRotateValue.Create;
end;

destructor TJvAxisDelta.Destroy;
var
  Index: TJvAxisIndex;
begin
  for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    FConstituents[Index].Free;

  inherited Destroy;
end;

function TJvAxisDelta.GetConstituents(Index: TJvAxisIndex): TJvRotateValue;
begin
  Result := FConstituents[Index];
end;

procedure TJvAxisDelta.SetConstituents(Index: TJvAxisIndex;
  const Value: TJvRotateValue);
begin
  FConstituents[Index].Assign(Value);
end;

{ TJvColorDelta }

procedure TJvColorDelta.Assign(Value: TJvColorDelta);
begin
  AxisRed.Assign(Value.AxisRed);
  AxisGreen.Assign(Value.AxisGreen);
  AxisBlue.Assign(Value.AxisBlue);

  ColorID := Value.ColorID;
end;

constructor TJvColorDelta.Create;
begin
  inherited Create;
  FColorID := csRGB;
  FAxisRed := TJvAxisDelta.Create;
  FAxisGreen := TJvAxisDelta.Create;
  FAxisBlue := TJvAxisDelta.Create;
end;

destructor TJvColorDelta.Destroy;
begin
  FAxisRed.Free;
  FAxisGreen.Free;
  FAxisBlue.Free;

  inherited Destroy;
end;

procedure TJvColorDelta.SetAxisBlue(const Value: TJvAxisDelta);
begin
  FAxisBlue.Assign(Value);
end;

procedure TJvColorDelta.SetAxisGreen(const Value: TJvAxisDelta);
begin
  FAxisGreen.Assign(Value);
end;

procedure TJvColorDelta.SetAxisRed(const Value: TJvAxisDelta);
begin
  FAxisRed.Assign(Value);
end;

{ TJvRotateValue }

procedure TJvRotateValue.Assign(Value: TJvRotateValue);
begin
  FValue := Value.Value;
  FSaturationMethod := Value.SaturationMethod;
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

