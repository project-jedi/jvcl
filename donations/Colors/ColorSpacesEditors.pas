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
  TPredefinedFamily = (pfConstant, pfSystem);

  TFullColorProperty = class(TPropertyEditor, ICustomPropertyDrawing)
  private
    function GetIsColorProperty: Boolean;
    procedure DialogApply(Sender: TObject; AFullColor: TFullColor);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;
    procedure EditSpace(AColorID: TColorID);
    procedure SetColor(AFullColor: TFullColor);
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    property IsColorProperty: Boolean read GetIsColorProperty;
  end;

  TFullColorSpace = class(TNestedProperty)
  private
    FColorSpace: TColorSpace;
    FParent: TFullColorProperty;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TFullColorProperty; AColorSpace: TColorSpace;
      AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    procedure Edit; override;
    property ColorSpace: TColorSpace read FColorSpace;
    property Parent: TFullColorProperty read FParent;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TFullColorAxis = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FColorSpace: TColorSpace;
    FAxisIndex: TAxisIndex;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TPropertyEditor; AColorSpace: TColorSpace;
      AAxisIndex: TAxisIndex; AIsColorProperty: Boolean);
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
    property AxisIndex: TAxisIndex read FAxisIndex;
    property ColorSpace: TColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TPredefinedColorSpace = class(TNestedProperty)
  private
    FParent: TFullColorProperty;
    FColorSpace: TColorSpace;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TFullColorProperty; AIsColorProperty: Boolean);
    function GetName: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    property Parent: TFullColorProperty read FParent;
    property ColorSpace: TColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TPredefinedIndentColorSpace = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FPredefinedFamily: TPredefinedFamily;
    FParent: TFullColorProperty;
    FColorSpace: TColorSpace;
    FIsColorProperty: Boolean;
  public
    constructor Create(AParent: TFullColorProperty; AColorSpace: TColorSpace;
      APredefinedFamily: TPredefinedFamily; AIsColorProperty: Boolean);
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
    property Parent: TFullColorProperty read FParent;
    property PredefinedFamily: TPredefinedFamily read FPredefinedFamily;
    property ColorSpace: TColorSpace read FColorSpace;
    property IsColorProperty: Boolean read FIsColorProperty;
  end;

  TColorSelection = class(TSelectionEditor)
  protected
    procedure RequireClass(Proc: TGetStrProc; AClass: TClass);
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure RequiresUnits(Proc: TGetStrProc); override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  TFullColorDialogSelection = class(TColorSelection)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TColorCircleDialogSelection = class(TColorSelection)
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

//=== { TFullColorProperty } =================================================

procedure TFullColorProperty.DialogApply(Sender: TObject; AFullColor: TFullColor);
begin
  SetColor(AFullColor);
end;

procedure TFullColorProperty.Edit;
begin
  EditSpace(ColorSpaceManager.GetColorID(GetOrdValue));
end;

procedure TFullColorProperty.EditSpace(AColorID: TColorID);
var
  AColor: TFullColor;
begin
  with ColorSpaceManager, TFullColorDialog.Create(nil) do
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

function TFullColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paMultiSelect, paRevertable,
    paVolatileSubProperties, paVCL];
  if IsColorProperty then
    Result := Result + [paReadOnly]
  else
    Result := Result + [paValueList, paSortList];
end;

function TFullColorProperty.GetIsColorProperty: Boolean;
begin
  Result := (GetPropType = TypeInfo(TColor));
end;

procedure TFullColorProperty.GetProperties(Proc: TGetPropProc);
var
  Index: Integer;
  AColorSpace: TColorSpace;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
    begin
      AColorSpace := ColorSpaceIndex[Index];
      if AColorSpace.ID <> csPredefined then
        Proc(TFullColorSpace.Create(Self, AColorSpace, IsColorProperty));
    end;
  Proc(TPredefinedColorSpace.Create(Self, IsColorProperty));
end;

function TFullColorProperty.GetValue: string;
var
  AValue: TFullColor;
  AColorID: TColorID;
  ColorStr: string;
begin
  AValue := GetOrdValue;

  with ColorSpaceManager do
  begin
    AColorID := GetColorID(AValue);

    if IsColorProperty and not (AColorID in [csRGB, csPredefined]) then
      ColorStr := SInvalidFormat
    else
      ColorStr := ColorSpace[GetColorID(AValue)].ShortName;
  end;

  Result := Format('%s [%s]', [GetPropType^.Name, ColorStr]);
end;

procedure TFullColorProperty.GetValues(Proc: TGetStrProc);
var
  Index: Integer;
begin
  with ColorSpaceManager do
    for Index := 0 to ColorSpaceCount - 1 do
      Proc(ColorSpaceIndex[Index].ShortName);
end;

procedure TFullColorProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TFullColorProperty.PropDrawValue(ACanvas: TCanvas;
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

procedure TFullColorProperty.SetColor(AFullColor: TFullColor);
var
  AColorID: TColorID;
begin
  with ColorSpaceManager do
  begin
    AColorID := GetColorID(AFullColor);
    if (not (AColorID in [csRGB, csPredefined])) and IsColorProperty then
      AFullColor := ConvertToID(AFullColor, csRGB);
  end;
  SetOrdValue(AFullColor);
end;

procedure TFullColorProperty.SetValue(const Value: string);
var
  Index: Integer;
  LColor: TFullColor;
  LColorSpace: TColorSpace;
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

//=== { TFullColorSpace } ====================================================

constructor TFullColorSpace.Create(AParent: TFullColorProperty;
  AColorSpace: TColorSpace; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := AColorSpace;
  FIsColorProperty := AIsColorProperty;
end;

procedure TFullColorSpace.Edit;
begin
  Parent.EditSpace(ColorSpace.ID);
end;

function TFullColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paMultiSelect, paReadOnly,
    paRevertable, paVolatileSubProperties, paVCL];
end;

function TFullColorSpace.GetName: string;
begin
  Result := ColorSpace.ShortName;
end;

procedure TFullColorSpace.GetProperties(Proc: TGetPropProc);
var
  Index: TAxisIndex;
begin
  for Index := Low(TAxisIndex) to High(TAxisIndex) do
    Proc(TFullColorAxis.Create(Parent, ColorSpace, Index, IsColorProperty));
end;

function TFullColorSpace.GetValue: string;
begin
  Result := ColorSpace.Name;
end;

//=== { TFullColorSub } ======================================================

constructor TFullColorAxis.Create(AParent: TPropertyEditor;
  AColorSpace: TColorSpace; AAxisIndex: TAxisIndex; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FColorSpace := AColorSpace;
  FAxisIndex := AAxisIndex;
  FIsColorProperty := AIsColorProperty;
end;

function TFullColorAxis.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TFullColorAxis.GetName: string;
begin
  Result := ColorSpace.AxisName[AxisIndex];
end;

function TFullColorAxis.GetValue: string;
begin
  Result := Format('$%.2x', [
    GetAxisValue(ColorSpaceManager.ConvertToID(GetOrdValue, ColorSpace.ID), AxisIndex)]);
end;

procedure TFullColorAxis.GetValues(Proc: TGetStrProc);
var
  I: Byte;
begin
  for I := ColorSpace.AxisMin[AxisIndex] to ColorSpace.AxisMax[AxisIndex] do
    Proc(Format('$%.2x', [I]));
end;

procedure TFullColorAxis.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  LColor: Cardinal;
begin
  LColor := ColorSpaceManager.ConvertToID(GetOrdValue, ColorSpace.ID);
  LColor := SetAxisValue(LColor, AxisIndex, HexToInt(Value));
  LColor := ColorSpace.ConvertToRGB(LColor);
  ColorPropertyListDrawValue(LColor, Value, ACanvas, ARect, ASelected);
end;

procedure TFullColorAxis.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TFullColorAxis.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TFullColorAxis.SetValue(const Value: string);
var
  AxisValue: Byte;
  LColor: Cardinal;
  LColorID: TColorID;
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

//=== { TPredefinedColorSpace } ==============================================

constructor TPredefinedColorSpace.Create(AParent: TFullColorProperty; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := ColorSpaceManager.ColorSpace[csPredefined];
  FIsColorProperty := AIsColorProperty;
end;

function TPredefinedColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result :=
    [paSubProperties, paMultiSelect, paReadOnly, paFullWidthName, paVolatileSubProperties];
end;

function TPredefinedColorSpace.GetName: string;
begin
  Result := ColorSpace.Name;
end;

procedure TPredefinedColorSpace.GetProperties(Proc: TGetPropProc);
begin
  Proc(TPredefinedIndentColorSpace.Create(Parent, ColorSpace, pfConstant, IsColorProperty));
  Proc(TPredefinedIndentColorSpace.Create(Parent, ColorSpace, pfSystem, IsColorProperty));
end;

//=== { TPredefinedIndentColorSpace } ========================================

constructor TPredefinedIndentColorSpace.Create(AParent: TFullColorProperty;
  AColorSpace: TColorSpace; APredefinedFamily: TPredefinedFamily; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := AColorSpace;
  FPredefinedFamily := APredefinedFamily;
  FIsColorProperty := AIsColorProperty;
end;

function TPredefinedIndentColorSpace.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paDialog, paMultiSelect, paRevertable, paVCL];
end;

function TPredefinedIndentColorSpace.GetName: string;
begin
  Result := GetEnumName(TypeInfo(TPredefinedFamily), Integer(PredefinedFamily));
  Result := Copy(Result, 3, Length(Result) - 2);
end;

function TPredefinedIndentColorSpace.GetValue: string;
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

procedure TPredefinedIndentColorSpace.GetValues(Proc: TGetStrProc);
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

procedure TPredefinedIndentColorSpace.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  ColorPropertyListDrawValue(StringToColor(Value), Value, ACanvas, ARect, ASelected);
end;

procedure TPredefinedIndentColorSpace.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TPredefinedIndentColorSpace.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TPredefinedIndentColorSpace.SetValue(const Value: string);
var
  TempValue, NewValue: Integer;
  AColorID: TColorID;
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

//=== { TFullColorDialogSelection } ==========================================

procedure TColorSelection.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  // No execution
  inherited ExecuteVerb(Index, List);
end;

function TColorSelection.GetVerb(Index: Integer): string;
begin
  // No menu item
  Result := '';
end;

function TColorSelection.GetVerbCount: Integer;
begin
  // No menu item
  Result := 0;
end;

procedure TColorSelection.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  // No menu item
  inherited PrepareItem(Index, AItem);
end;

procedure TColorSelection.RequireClass(Proc: TGetStrProc; AClass: TClass);
var
  LTypInfo: PTypeInfo;
begin
  LTypInfo := AClass.ClassInfo;
  if LTypInfo <> nil then
    Proc(GetTypeData(LTypInfo).UnitName);
end;

procedure TColorSelection.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited RequiresUnits(Proc);

  RequireClass(Proc, TColorSpace);

  with ColorSpaceManager do
    for I := 0 to ColorSpaceCount - 1 do
      RequireClass(Proc, ColorSpaceIndex[I].ClassType);
end;

//=== { TFullColorDialogSelection } ==========================================

procedure TFullColorDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if FullColorFormClass <> nil then
    RequireClass(Proc, FullColorFormClass);
end;

//=== { TColorCircleDialogSelection } ========================================

procedure TColorCircleDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if ColorCircleBaseFormClass <> nil then
    RequireClass(Proc, ColorCircleBaseFormClass);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TFullColorProperty);
  RegisterPropertyEditor(TypeInfo(TFullColor), nil, '', TFullColorProperty);
  RegisterSelectionEditor(TFullColorDialog, TFullColorDialogSelection);
  RegisterSelectionEditor(TColorCircleDialog, TColorCircleDialogSelection);
  RegisterSelectionEditor(TColorPanel, TColorSelection);
  RegisterSelectionEditor(TColorTrackBar, TColorSelection);
  RegisterSelectionEditor(TColorCircle, TColorSelection);
  RegisterSelectionEditor(TColorLabel, TColorSelection);
  RegisterSelectionEditor(TColorSpaceCombo, TColorSelection);
  RegisterSelectionEditor(TAxisConfigCombo, TColorSelection);
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

