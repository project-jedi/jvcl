{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpacesEditors.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorSpacesEditors;

{$I jvcl.inc}

interface

uses
  Windows, Classes, DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  Graphics,
  ColorSpaces;

type
  TJvPredefinedFamily = (pfConstant, pfSystem);

  TJvFullColorProperty = class(TPropertyEditor, ICustomPropertyDrawing)
  private
    function GetIsColorProperty: Boolean;
    procedure DialogApply(Sender: TObject; AFullColor: TJvFullColor);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    procedure EditSpace(AColorID: TJvColorID);
    procedure SetColor(AFullColor: TJvFullColor);
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    property IsColorProperty: Boolean read GetIsColorProperty;
  end;

  TJvFullColorSpace = class(TNestedProperty)
  private
    FColorSpace: TJvColorSpace;
    FParent: TJvFullColorProperty;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TJvFullColorProperty; AColorSpace: TJvColorSpace;
      AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    procedure Edit; override;
    property ColorSpace: TJvColorSpace read FColorSpace;
    property Parent: TJvFullColorProperty read FParent;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TJvFullColorAxis = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FColorSpace: TJvColorSpace;
    FAxisIndex: TJvAxisIndex;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TPropertyEditor; AColorSpace: TJvColorSpace;
      AAxisIndex: TJvAxisIndex; AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); virtual;
    property AxisIndex: TJvAxisIndex read FAxisIndex;
    property ColorSpace: TJvColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TJvPredefinedColorSpace = class(TNestedProperty)
  private
    FParent: TJvFullColorProperty;
    FColorSpace: TJvColorSpace;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TJvFullColorProperty; AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    property Parent: TJvFullColorProperty read FParent;
    property ColorSpace: TJvColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TJvPredefinedIndentColorSpace = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FPredefinedFamily: TJvPredefinedFamily;
    FParent: TJvFullColorProperty;
    FColorSpace: TJvColorSpace;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TJvFullColorProperty; AColorSpace: TJvColorSpace;
      APredefinedFamily: TJvPredefinedFamily; AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
    // ICustomPropertyListDrawing
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); virtual;
    property Parent: TJvFullColorProperty read FParent;
    property PredefinedFamily: TJvPredefinedFamily read FPredefinedFamily;
    property ColorSpace: TJvColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TJvColorSelection = class(TSelectionEditor)
  protected
    procedure RequireClass(Proc: TGetStrProc; AClass: TClass);
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure RequiresUnits(Proc: TGetStrProc); override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  TJvFullColorDialogSelection = class(TJvColorSelection)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TJvColorCircleDialogSelection = class(TJvColorSelection)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, SysUtils, GraphUtil, DesignConst, TypInfo,
  ColorDialogs, ColorCtrls;

const
  COLOR_PREVIEW_RECT = 16;
  COLOR_PREVIEW_LINE = 1;
  COLOR_PREVIEW_SIZE = COLOR_PREVIEW_RECT + 2 * COLOR_PREVIEW_LINE;

function HexToInt(Value: string): Byte;
begin
  if Value = '' then
    Result := 0
  else
  begin
    if Length(Value) > 3 then
      Value := Copy(Value, 1, 3);
    Result := StrToInt(Value);
  end;
end;

procedure ColorPropertyListDrawValue(AColor: TColor; AValue: string;
  ACanvas: TCanvas; ARect: TRect; ASelected: Boolean);
var
  OldBrushColor: TColor;
  OldPenColor: TColor;
begin
  with ACanvas do
  begin
    OldBrushColor := Brush.Color;
    OldPenColor := Pen.Color;

    Pen.Color := Brush.Color;
    Rectangle(ARect);

    Brush.Color := AColor;
    Pen.Color := clBlack;
    Rectangle(ARect.Left + COLOR_PREVIEW_LINE,
      ARect.Top + COLOR_PREVIEW_LINE,
      ARect.Left + COLOR_PREVIEW_LINE + COLOR_PREVIEW_RECT,
      ARect.Top + COLOR_PREVIEW_LINE + COLOR_PREVIEW_RECT);

    Pen.Color := OldPenColor;
    Brush.Color := OldBrushColor;
    TextOut(ARect.Left + COLOR_PREVIEW_SIZE, ARect.Top, AValue);
  end;
end;

//=== { TJvFullColorProperty } ===============================================

procedure TJvFullColorProperty.DialogApply(Sender: TObject; AFullColor: TJvFullColor);
begin
  SetColor(AFullColor);
end;

procedure TJvFullColorProperty.Edit;
begin
  EditSpace(ColorSpaceManager.GetColorID(GetOrdValue));
end;

procedure TJvFullColorProperty.EditSpace(AColorID: TJvColorID);
var
  AColor: TJvFullColor;
begin
  with ColorSpaceManager, TJvFullColorDialog.Create(nil) do
    try
      AColor := GetOrdValue;
      if GetColorID(AColor) <> AColorID then
        AColor := ConvertToID(AColor, AColorID);

      FullColor := AColor;
      Title := '';
      OnApply := DialogApply;
      Options := [foFullOpen, foAllowSpaceChange, foShowNewPreview, foShowOldPreview,
        foShowPredefined, foAllowVariable, foNoneAndDefault, foShowApply];

      if Execute then
        SetColor(FullColor);
    finally
      Free;
    end;
end;

function TJvFullColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paMultiSelect, paRevertable,
    paVolatileSubProperties, paVCL];
  if IsColorProperty then
    Result := Result + [paReadOnly]
  else
    Result := Result + [paValueList, paSortList];
end;

function TJvFullColorProperty.GetIsColorProperty: Boolean;
begin
  Result := (GetPropType = TypeInfo(TColor));
end;

procedure TJvFullColorProperty.GetProperties(Proc: TGetPropProc);
var
  Index: Integer;
  AColorSpace: TJvColorSpace;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
    begin
      AColorSpace := ColorSpaceIndex[Index];
      if AColorSpace.ID <> csDEF then
        Proc(TJvFullColorSpace.Create(Self, AColorSpace, IsColorProperty));
    end;
  Proc(TJvPredefinedColorSpace.Create(Self, IsColorProperty));
end;

function TJvFullColorProperty.GetValue: string;
var
  AValue: TJvFullColor;
  AColorID: TJvColorID;
  ColorStr: string;
begin
  AValue := GetOrdValue;

  with ColorSpaceManager do
  begin
    AColorID := GetColorID(AValue);

    if IsColorProperty and not (AColorID in [csRGB, csDEF]) then
      ColorStr := SInvalidFormat
    else
      ColorStr := ColorSpace[GetColorID(AValue)].ShortName;
  end;

  Result := Format('%s [%s]', [GetPropType^.Name, ColorStr]);
end;

procedure TJvFullColorProperty.GetValues(Proc: TGetStrProc);
var
  Index: Integer;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
      Proc(ColorSpaceIndex[Index].ShortName);
end;

procedure TJvFullColorProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TJvFullColorProperty.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  OldPenColor, OldBrushColor: TColor;
  Right: Integer;
begin
  with ACanvas do
  begin
    Rectangle(ARect);

    OldBrushColor := Brush.Color;
    Brush.Color := TColor(ColorSpaceManager.ConvertToID(GetOrdValue, csRGB));
    OldPenColor := Pen.Color;
    Pen.Color := clBlack;

    Right := (ARect.Bottom - ARect.Top) + ARect.Left;

    Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

    Pen.Color := OldPenColor;
    Brush.Color := OldBrushColor;
  end;
  DefaultPropertyDrawValue(Self, ACanvas, Rect(Right, ARect.Top, ARect.Right, ARect.Bottom));
end;

procedure TJvFullColorProperty.SetColor(AFullColor: TJvFullColor);
var
  AColorID: TJvColorID;
begin
  with ColorSpaceManager do
  begin
    AColorID := GetColorID(AFullColor);
    if (not (AColorID in [csRGB, csDEF])) and IsColorProperty then
      AFullColor := ConvertToID(AFullColor, csRGB);
  end;
  SetOrdValue(AFullColor);
end;

procedure TJvFullColorProperty.SetValue(const Value: string);
var
  Index: Integer;
  LColor: TJvFullColor;
  LColorSpace: TJvColorSpace;
begin
  LColor := GetOrdValue;
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
    begin
      LColorSpace := ColorSpaceIndex[Index];
      if LColorSpace.ShortName = Value then
      begin
        LColor := ColorSpaceManager.ConvertToID(LColor, LColorSpace.ID);
        SetOrdValue(LColor);
        Break;
      end;
    end;
end;

//=== { TJvFullColorSpace } ==================================================

constructor TJvFullColorSpace.Create(AParent: TJvFullColorProperty;
  AColorSpace: TJvColorSpace; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := AColorSpace;
  FIsColorProperty := AIsColorProperty;
end;

procedure TJvFullColorSpace.Edit;
begin
  Parent.EditSpace(ColorSpace.ID);
end;

function TJvFullColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paMultiSelect, paReadOnly,
    paRevertable, paVolatileSubProperties, paVCL];
end;

function TJvFullColorSpace.GetName: string;
begin
  Result := ColorSpace.ShortName;
end;

procedure TJvFullColorSpace.GetProperties(Proc: TGetPropProc);
var
  Index: TJvAxisIndex;
begin
  for Index := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    Proc(TJvFullColorAxis.Create(Parent, ColorSpace, Index, IsColorProperty));
end;

function TJvFullColorSpace.GetValue: string;
begin
  Result := ColorSpace.Name;
end;

//=== { TJvFullColorAxis } ===================================================

constructor TJvFullColorAxis.Create(AParent: TPropertyEditor;
  AColorSpace: TJvColorSpace; AAxisIndex: TJvAxisIndex; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FColorSpace := AColorSpace;
  FAxisIndex := AAxisIndex;
  FIsColorProperty := AIsColorProperty;
end;

function TJvFullColorAxis.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvFullColorAxis.GetName: string;
begin
  Result := ColorSpace.AxisName[AxisIndex];
end;

function TJvFullColorAxis.GetValue: string;
begin
  Result := Format('$%.2x', [
    GetAxisValue(ColorSpaceManager.ConvertToID(GetOrdValue, ColorSpace.ID), AxisIndex)]);
end;

procedure TJvFullColorAxis.GetValues(Proc: TGetStrProc);
var
  I: Byte;
begin
  for I := ColorSpace.AxisMin[AxisIndex] to ColorSpace.AxisMax[AxisIndex] do
    Proc(Format('$%.2x', [I]));
end;

procedure TJvFullColorAxis.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  LColor: Cardinal;
begin
  LColor := ColorSpaceManager.ConvertToID(GetOrdValue, ColorSpace.ID);
  LColor := SetAxisValue(LColor, AxisIndex, HexToInt(Value));
  LColor := ColorSpace.ConvertToRGB(LColor);
  ColorPropertyListDrawValue(LColor, Value, ACanvas, ARect, ASelected);
end;

procedure TJvFullColorAxis.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TJvFullColorAxis.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TJvFullColorAxis.SetValue(const Value: string);
var
  AxisValue: Byte;
  LColor: Cardinal;
  LColorID: TJvColorID;
begin
  AxisValue := HexToInt(Value);
  LColor := GetOrdValue;
  LColorID := ColorSpaceManager.GetColorID(LColor);
  LColor := ColorSpaceManager.ConvertToID(LColor, ColorSpace.ID);
  LColor := SetAxisValue(LColor, AxisIndex, AxisValue);
  if not IsColorProperty then
    LColor := ColorSpaceManager.ConvertToID(LColor, LColorID)
  else
    LColor := ColorSpaceManager.ConvertToID(LColor, csRGB);
  SetOrdValue(LColor);
end;

//=== { TJvPredefinedColorSpace } ============================================

constructor TJvPredefinedColorSpace.Create(AParent: TJvFullColorProperty; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := ColorSpaceManager.ColorSpace[csDEF];
  FIsColorProperty := AIsColorProperty;
end;

function TJvPredefinedColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result :=
    [paSubProperties, paMultiSelect, paReadOnly, paFullWidthName, paVolatileSubProperties];
end;

function TJvPredefinedColorSpace.GetName: string;
begin
  Result := ColorSpace.Name;
end;

procedure TJvPredefinedColorSpace.GetProperties(Proc: TGetPropProc);
begin
  Proc(TJvPredefinedIndentColorSpace.Create(Parent, ColorSpace, pfConstant, IsColorProperty));
  Proc(TJvPredefinedIndentColorSpace.Create(Parent, ColorSpace, pfSystem, IsColorProperty));
end;

//=== { TJvPredefinedIndentColorSpace } ======================================

constructor TJvPredefinedIndentColorSpace.Create(AParent: TJvFullColorProperty;
  AColorSpace: TJvColorSpace; APredefinedFamily: TJvPredefinedFamily; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := AColorSpace;
  FPredefinedFamily := APredefinedFamily;
  FIsColorProperty := AIsColorProperty;
end;

function TJvPredefinedIndentColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paDialog, paMultiSelect, paRevertable, paVCL];
end;

function TJvPredefinedIndentColorSpace.GetName: string;
begin
  Result := GetEnumName(TypeInfo(TJvPredefinedFamily), Integer(PredefinedFamily));
  Result := Copy(Result, 3, Length(Result) - 2);
end;

function TJvPredefinedIndentColorSpace.GetValue: string;
var
  TempValue: Integer;
  TempResult: string;
begin
  TempValue := GetOrdValue;
  if (((TempValue and $FF000000) <> 0) xor (PredefinedFamily = pfConstant)) and
    ColorToIdent(GetOrdValue, TempResult) then
    Result := TempResult
  else
    Result := '';
end;

procedure TJvPredefinedIndentColorSpace.GetValues(Proc: TGetStrProc);
var
  Index: Integer;
  Start, Stop: Integer;
begin
  if PredefinedFamily = pfConstant then
  begin
    Start := 0;
    Stop := StandardColorsCount + ExtendedColorsCount - 1;
  end
  else
  begin
    Start := StandardColorsCount + ExtendedColorsCount;
    Stop := ColorSpaceManager.PredefinedColorCount - 1;
  end;
  with ColorSpaceManager do
    for Index := Start to Stop do
      Proc(PredefinedColorIndex[Index]);
end;

procedure TJvPredefinedIndentColorSpace.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  ColorPropertyListDrawValue(StringToColor(Value), Value, ACanvas, ARect, ASelected);
end;

procedure TJvPredefinedIndentColorSpace.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TJvPredefinedIndentColorSpace.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TJvPredefinedIndentColorSpace.SetValue(const Value: string);
var
  TempValue, NewValue: Integer;
  AColorID: TJvColorID;
begin
  if IdentToColor(Value, NewValue) and (((NewValue and $FF000000) <> 0) xor (PredefinedFamily = pfConstant)) then
  begin
    TempValue := GetOrdValue;
    AColorID := ColorSpaceManager.GetColorID(TempValue);
    if not IsColorProperty then
      NewValue := ColorSpaceManager.ConvertToID(NewValue, AColorID);
    SetOrdValue(NewValue);
  end;
end;

//=== { TJvColorSelection } ==================================================

procedure TJvColorSelection.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  // No execution
  inherited ExecuteVerb(Index, List);
end;

function TJvColorSelection.GetVerb(Index: Integer): string;
begin
  // No menu item
  Result := '';
end;

function TJvColorSelection.GetVerbCount: Integer;
begin
  // No menu item
  Result := 0;
end;

procedure TJvColorSelection.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  // No menu item
  inherited PrepareItem(Index, AItem);
end;

procedure TJvColorSelection.RequireClass(Proc: TGetStrProc; AClass: TClass);
var
  LTypInfo: PTypeInfo;
begin
  LTypInfo := AClass.ClassInfo;
  if LTypInfo <> nil then
    Proc(GetTypeData(LTypInfo).UnitName);
end;

procedure TJvColorSelection.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited RequiresUnits(Proc);

  RequireClass(Proc, TJvColorSpace);

  with ColorSpaceManager do
    for I := 0 to ColorSpaceCount - 1 do
      RequireClass(Proc, ColorSpaceIndex[I].ClassType);
end;

//=== { TJvFullColorDialogSelection } ========================================

procedure TJvFullColorDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if FullColorFormClass <> nil then
    RequireClass(Proc, FullColorFormClass);
end;

//=== { TJvColorCircleDialogSelection } ======================================

procedure TJvColorCircleDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if ColorCircleBaseFormClass <> nil then
    RequireClass(Proc, ColorCircleBaseFormClass);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TJvFullColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvFullColor), nil, '', TJvFullColorProperty);
  RegisterSelectionEditor(TJvFullColorDialog, TJvFullColorDialogSelection);
  RegisterSelectionEditor(TJvColorCircleDialog, TJvColorCircleDialogSelection);
  RegisterSelectionEditor(TJvColorPanel, TJvColorSelection);
  RegisterSelectionEditor(TJvFullColorTrackBar, TJvColorSelection);
  RegisterSelectionEditor(TJvColorCircle, TJvColorSelection);
  RegisterSelectionEditor(TJvColorLabel, TJvColorSelection);
  RegisterSelectionEditor(TJvColorSpaceCombo, TJvColorSelection);
  RegisterSelectionEditor(TJvColorAxisConfigCombo, TJvColorSelection);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\design'
    );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

