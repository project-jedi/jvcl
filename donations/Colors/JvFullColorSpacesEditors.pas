{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorSpacesEditors.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorSpacesEditors;

{$I jvcl.inc}

interface

uses
  Windows, Classes, DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  Graphics,
  JvFullColorSpaces;

type
  TJvDEFFamily = (pfConstant, pfSystem);

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
    procedure EditSpace(AColorID: TJvFullColorSpaceID);
    procedure SetColor(AFullColor: TJvFullColor);
    // ICustomPropertyDrawing
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    property IsColorProperty: Boolean read GetIsColorProperty;
  end;

  TJvFullColorSpaceProperty = class(TNestedProperty)
  private
    FColorSpace: TJvColorSpace;
    FParent: TJvFullColorProperty;
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
  end;

  TJvFullColorSpacePropertyClass = class of TJvFullColorSpaceProperty;

  TJvFullColorAxisProperty = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FAxisIndex: TJvAxisIndex;
    FParent:TJvFullColorSpaceProperty;
  public
    constructor Create(AParent: TJvFullColorSpaceProperty;
      AAxisIndex: TJvAxisIndex);
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
    property Parent: TJvFullColorSpaceProperty read FParent;
    property AxisIndex: TJvAxisIndex read FAxisIndex;
  end;

  TJvDEFColorSpaceProperty = class(TJvFullColorSpaceProperty)
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  TJvDEFColorSpaceIndentProperty = class(TNestedProperty, ICustomPropertyListDrawing)
  private
    FPredefinedFamily: TJvDEFFamily;
    FParent: TJvDEFColorSpaceProperty;
  public
    constructor Create(AParent: TJvDEFColorSpaceProperty;
      APredefinedFamily: TJvDEFFamily);
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
    property Parent: TJvDEFColorSpaceProperty read FParent;
    property PredefinedFamily: TJvDEFFamily read FPredefinedFamily;
  end;

  TJvFullColorSelection = class(TSelectionEditor)
  protected
    procedure RequireClass(Proc: TGetStrProc; AClass: TClass);
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure RequiresUnits(Proc: TGetStrProc); override;
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  TJvFullColorDialogSelection = class(TJvFullColorSelection)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TJvFullColorCircleDialogSelection = class(TJvFullColorSelection)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TJvFullColorListEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function AllEqual: Boolean; override;
    procedure FormApply(Sender: TObject);
  end;

procedure Register;
procedure RegisterFullColorSpaceEditor (AColorSpaceID: TJvFullColorSpaceID;
  AEditorClass: TJvFullColorSpacePropertyClass);
function FindFullColorSpaceEditor (AColorSpaceID: TJvFullColorSpaceID)
  :TJvFullColorSpacePropertyClass;

var
  DefaultFullColorSpacePropertyClass :
                 TJvFullColorSpacePropertyClass = TJvFullColorSpaceProperty;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Math, SysUtils, GraphUtil, DesignConst, TypInfo, Forms,
  JvFullColorDialogs, JvFullColorCtrls, JvFullColorListFrm;

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
  if IsColorProperty then
    // a dummy param
    EditSpace(csRGB)
  else
    EditSpace(ColorSpaceManager.GetColorSpaceID(GetOrdValue));
end;

procedure TJvFullColorProperty.EditSpace(AColorID: TJvFullColorSpaceID);
var
  LColor: TJvFullColor;
begin
  with ColorSpaceManager, TJvFullColorDialog.Create(nil) do
    try
      if IsColorProperty then
      begin
        LColor := ColorSpaceManager.ConvertFromColor(TColor(GetOrdValue));
        // reset dummy param
        AColorID := ColorSpaceManager.GetColorSpaceID(LColor);
      end
      else
        LColor := TJvFullColor(GetOrdValue);
      if GetColorSpaceID(LColor) <> AColorID then
        LColor := ConvertToID(LColor, AColorID);

      FullColor := LColor;
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
  I: Integer;
  CS: TJvColorSpace;
begin
  with ColorSpaceManager do
    for I := 0 to Count - 1 do
    begin
      CS := ColorSpaceByIndex[I];
      Proc(FindFullColorSpaceEditor(CS.ID).Create(Self,CS,IsColorProperty));
    end;
end;

function TJvFullColorProperty.GetValue: string;
var
  Value: TJvFullColor;
begin
  Value := GetOrdValue;

  with ColorSpaceManager do
    if IsColorProperty then
      Result := GetPropType^.Name
    else
      Result := Format('%s [%s]', [GetPropType^.Name, ColorSpace[GetColorSpaceID(Value)].ShortName]);
end;

procedure TJvFullColorProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with ColorSpaceManager do
    for I := 0 to Count - 1 do
      Proc(ColorSpaceByIndex[I].ShortName);
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
    if IsColorProperty then
      Brush.Color := GetOrdValue
    else
      Brush.Color := ColorSpaceManager.ConvertToColor(TJvFullColor(GetOrdValue));
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
begin
  with ColorSpaceManager do
    if IsColorProperty then
      SetOrdValue(Ord(ConvertToColor(AFullColor)))
    else
      SetOrdValue(Ord(AFullColor));
end;

procedure TJvFullColorProperty.SetValue(const Value: string);
var
  I: Integer;
  LColor: TJvFullColor;
  LColorSpace: TJvColorSpace;
begin
  LColor := GetOrdValue;
  with ColorSpaceManager do
    for I := 0 to Count - 1 do
    begin
      LColorSpace := ColorSpaceByIndex[I];
      if LColorSpace.ShortName = Value then
      begin
        LColor := ColorSpaceManager.ConvertToID(LColor, LColorSpace.ID);
        SetOrdValue(Ord(LColor));
        Break;
      end;
    end;
end;

//=== { TJvFullColorSpaceProperty } ==========================================

constructor TJvFullColorSpaceProperty.Create(AParent: TJvFullColorProperty;
  AColorSpace: TJvColorSpace; AIsColorProperty: Boolean);
begin
  inherited Create(AParent);
  FParent := AParent;
  FColorSpace := AColorSpace;
end;

procedure TJvFullColorSpaceProperty.Edit;
begin
  Parent.EditSpace(ColorSpace.ID);
end;

function TJvFullColorSpaceProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paSubProperties, paDialog, paMultiSelect, paReadOnly,
    paRevertable, paVolatileSubProperties, paVCL];
end;

function TJvFullColorSpaceProperty.GetName: string;
begin
  Result := ColorSpace.ShortName;
end;

procedure TJvFullColorSpaceProperty.GetProperties(Proc: TGetPropProc);
var
  I: TJvAxisIndex;
begin
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    Proc(TJvFullColorAxisProperty.Create(Self, I));
end;

function TJvFullColorSpaceProperty.GetValue: string;
begin
  Result := ColorSpace.Name;
end;

//=== { TJvFullColorAxisProperty } ===========================================

constructor TJvFullColorAxisProperty.Create(AParent: TJvFullColorSpaceProperty;
  AAxisIndex: TJvAxisIndex);
begin
  inherited Create(AParent.Parent);
  FParent := AParent;
  FAxisIndex := AAxisIndex;
end;

function TJvFullColorAxisProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvFullColorAxisProperty.GetName: string;
begin
  Result := Parent.ColorSpace.AxisName[AxisIndex];
end;

function TJvFullColorAxisProperty.GetValue: string;
begin
  Result := Format('$%.2x', [
    GetAxisValue(ColorSpaceManager.ConvertToID(GetOrdValue, Parent.ColorSpace.ID), AxisIndex)]);
end;

procedure TJvFullColorAxisProperty.GetValues(Proc: TGetStrProc);
var
  I: Byte;
begin
  with Parent.ColorSpace do
    for I := AxisMin[AxisIndex] to AxisMax[AxisIndex] do
    Proc(Format('$%.2x', [I]));
end;

procedure TJvFullColorAxisProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  FullColor: TJvFullColor;
  LColor: TColor;
begin
  FullColor := ColorSpaceManager.ConvertToID(GetOrdValue, Parent.ColorSpace.ID);
  FullColor := SetAxisValue(FullColor, AxisIndex, HexToInt(Value));
  LColor := Parent.ColorSpace.ConvertToColor(FullColor);
  ColorPropertyListDrawValue(LColor, Value, ACanvas, ARect, ASelected);
end;

procedure TJvFullColorAxisProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TJvFullColorAxisProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TJvFullColorAxisProperty.SetValue(const Value: string);
var
  AxisValue: Byte;
  LColor: TJvFullColor;
  LColorID: TJvFullColorSpaceID;
begin
  AxisValue := HexToInt(Value);
  LColor := TJvFullColor(GetOrdValue);
  LColorID := ColorSpaceManager.GetColorSpaceID(LColor);
  LColor := ColorSpaceManager.ConvertToID(LColor, Parent.ColorSpace.ID);
  LColor := SetAxisValue(LColor, AxisIndex, AxisValue);
  if not Parent.Parent.IsColorProperty then
    SetOrdValue(Ord(ColorSpaceManager.ConvertToID(LColor, LColorID)))
  else
    SetOrdValue(Ord(ColorSpaceManager.ConvertToColor(LColor)));
end;

//=== { TJvDEFColorSpaceProperty } ===========================================

procedure TJvDEFColorSpaceProperty.GetProperties(Proc: TGetPropProc);
begin
  Proc(TJvDEFColorSpaceIndentProperty.Create(Self, pfConstant));
  Proc(TJvDEFColorSpaceIndentProperty.Create(Self, pfSystem));
end;

//=== { TJvDEFColorSpaceIndentProperty } =====================================

constructor TJvDEFColorSpaceIndentProperty.Create(AParent: TJvDEFColorSpaceProperty;
      APredefinedFamily: TJvDEFFamily);
begin
  inherited Create(AParent.Parent);
  FParent := AParent;
  FPredefinedFamily := APredefinedFamily;
end;

function TJvDEFColorSpaceIndentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paMultiSelect, paRevertable];
end;

function TJvDEFColorSpaceIndentProperty.GetName: string;
begin
  Result := GetEnumName(TypeInfo(TJvDEFFamily), Ord(PredefinedFamily));
  Result := Copy(Result, 3, Length(Result) - 2);
end;

function TJvDEFColorSpaceIndentProperty.GetValue: string;
var
  AColor: TColor;
begin
  if Parent.Parent.IsColorProperty then
    AColor := GetOrdValue
  else
    AColor := ColorSpaceManager.ConvertToColor(TJvFullColor(GetOrdValue));
  if ((AColor and $FF000000) <> 0) xor (PredefinedFamily = pfConstant) then
    ColorToIdent(AColor, Result)
  else
    Result := '';
end;

procedure TJvDEFColorSpaceIndentProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AColor:TColor;
  CS: TJvDEFColorSpace;
begin
  CS := TJvDEFColorSpace(Parent.ColorSpace);
  for I:=0 to CS.ColorCount-1 do
  begin
    AColor:=CS.ColorValue[I];
    if ((AColor and $FF000000) <> 0) xor (PredefinedFamily = pfConstant)
      then Proc(CS.ColorName[I]);
  end;
end;

procedure TJvDEFColorSpaceIndentProperty.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  ColorPropertyListDrawValue(StringToColor(Value), Value, ACanvas, ARect, ASelected);
end;

procedure TJvDEFColorSpaceIndentProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := Max(ACanvas.TextHeight(Value), COLOR_PREVIEW_SIZE);
end;

procedure TJvDEFColorSpaceIndentProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLOR_PREVIEW_SIZE;
end;

procedure TJvDEFColorSpaceIndentProperty.SetValue(const Value: string);
var
  I:Integer;
  CS: TJvDEFColorSpace;
begin
  CS := TJvDEFColorSpace(Parent.ColorSpace);
  for I:=0 to CS.ColorCount-1 do
    if (CompareText(Value,CS.ColorName[I])=0) then
  begin
    if Parent.Parent.IsColorProperty then
      SetOrdValue(CS.ColorValue[I])
    else if ((CS.ColorValue[I] and JvSystemColorMask)=JvSystemColorMask) then
      SetOrdValue(CS.ConvertFromColor(CS.ColorValue[I]))
    else with ColorSpaceManager do
      SetOrdValue(ConvertToID(ConvertFromColor(CS.ColorValue[I]),
                              GetColorSpaceID(GetOrdValue)));
  end;
end;

//=== { TJvColorSelection } ==================================================

procedure TJvFullColorSelection.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  // No execution
  inherited ExecuteVerb(Index, List);
end;

function TJvFullColorSelection.GetVerb(Index: Integer): string;
begin
  // No menu item
  Result := '';
end;

function TJvFullColorSelection.GetVerbCount: Integer;
begin
  // No menu item
  Result := 0;
end;

procedure TJvFullColorSelection.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  // No menu item
  inherited PrepareItem(Index, AItem);
end;

procedure TJvFullColorSelection.RequireClass(Proc: TGetStrProc; AClass: TClass);
var
  LTypInfo: PTypeInfo;
begin
  LTypInfo := AClass.ClassInfo;
  if LTypInfo <> nil then
    Proc(GetTypeData(LTypInfo).UnitName);
end;

procedure TJvFullColorSelection.RequiresUnits(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited RequiresUnits(Proc);

  RequireClass(Proc, TJvColorSpace);

  with ColorSpaceManager do
    for I := 0 to Count - 1 do
      RequireClass(Proc, ColorSpaceByIndex[I].ClassType);
end;

//=== { TJvFullColorDialogSelection } ========================================

procedure TJvFullColorDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if FullColorFormClass <> nil then
    RequireClass(Proc, FullColorFormClass);
end;

//=== { TJvColorCircleDialogSelection } ======================================

procedure TJvFullColorCircleDialogSelection.RequiresUnits(Proc: TGetStrProc);
begin
  inherited RequiresUnits(Proc);

  if ColorCircleBaseFormClass <> nil then
    RequireClass(Proc, ColorCircleBaseFormClass);
end;

//=== { TJvFullColorListEditor } =============================================

function TJvFullColorListEditor.AllEqual: Boolean;
var
  IndexList, IndexColor: Integer;
  FullColorList:TJvFullColorList;
begin
  Result := False;
  if PropCount > 1 then
  begin
    FullColorList:=TJvFullColorList.Create;
    FullColorList.Assign(TJvFullColorList(GetOrdValue));

    for IndexList := 1 to PropCount - 1 do
      with TJvFullColorList(GetOrdValueAt(IndexList)) do
        for IndexColor:=0 to Count-1 do
          if (FullColorList.Items[IndexColor]<>Items[IndexColor]) then
            Exit;
  end;
  Result := True;
end;

procedure TJvFullColorListEditor.Edit;
var
  FullColorListForm: TJvFullColorListForm;
  FullColorList:TJvFullColorList;
  IndexList, IndexColor:Integer;
begin
  FullColorListForm := TJvFullColorListForm.Create(Application);
  try
    FullColorList:=TJvFullColorList.Create;
    try
      for IndexList:=0 to PropCount-1 do
        with TJvFullColorList(GetOrdValueAt(IndexList)) do
          for IndexColor:=0 to Count-1 do
            if FullColorList.IndexOf(Items[IndexColor])=-1 then
              FullColorList.Add(Items[IndexColor]);

      FullColorListForm.OnApply := FormApply;
      FullColorListForm.ColorList := FullColorList;
      if FullColorListForm.Execute then
        FormApply(FullColorListForm);
    finally
      FullColorList.Free;
    end;
  finally
    FullColorListForm.Free;
  end;
end;   

procedure TJvFullColorListEditor.FormApply(Sender: TObject);
begin
  SetOrdValue(Cardinal((Sender as TJvFullColorListForm).ColorList));
end;

function TJvFullColorListEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paVCL, paMultiSelect];
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TColor), nil, '', TJvFullColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvFullColor), nil, '', TJvFullColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvFullColorList), nil, '', TJvFullColorListEditor);
  RegisterSelectionEditor(TJvFullColorDialog, TJvFullColorDialogSelection);
  RegisterSelectionEditor(TJvFullColorCircleDialog, TJvFullColorCircleDialogSelection);
  RegisterSelectionEditor(TJvFullColorPanel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorTrackBar, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorCircle, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorLabel, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorSpaceCombo, TJvFullColorSelection);
  RegisterSelectionEditor(TJvFullColorAxisCombo, TJvFullColorSelection);
end;

var
  FullColorSpaceEditorList:TList = nil;

procedure ModuleUnloadProc(HInstance: Integer);
var
  Index:Integer;
  AEditorClass:TJvFullColorSpacePropertyClass;
begin
  for Index:=FullColorSpaceEditorList.Count-1 downto 0 do
  begin
    AEditorClass:=TJvFullColorSpacePropertyClass(FullColorSpaceEditorList.Items[Index]);
    if   (AEditorClass<>nil) and (AEditorClass<>TJvFullColorSpaceProperty)
      and(IsClassInModule(HInstance,AEditorClass))
      then FullColorSpaceEditorList.Items[Index]:=nil;
  end;
end;

procedure InitFullColorSpaceEditorList;
var
  ACount:Integer;
  Index:Integer;
begin
  ACount:=(High(TJvFullColorSpaceID)-Low(TJvFullColorSpaceID)+1) shr 2;
  FullColorSpaceEditorList.Count:=ACount;
  for Index:=0 to FullColorSpaceEditorList.Count-1 do
    FullColorSpaceEditorList.Items[Index]:=nil;
end;

procedure RegisterFullColorSpaceEditor (AColorSpaceID: TJvFullColorSpaceID;
  AEditorClass: TJvFullColorSpacePropertyClass);
begin
  FullColorSpaceEditorList.Items[AColorSpaceID shr 2]:=AEditorClass;
  // todo (outchy) notification for changing
end;

function FindFullColorSpaceEditor (AColorSpaceID: TJvFullColorSpaceID)
  :TJvFullColorSpacePropertyClass;
begin
  Result:=TJvFullColorSpacePropertyClass(FullColorSpaceEditorList.Items[AColorSpaceID shr 2]);
  if (Result=nil)
    then Result:=DefaultFullColorSpacePropertyClass;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\design'
    );
{$ENDIF UNITVERSIONING}

initialization
  FullColorSpaceEditorList:=TList.Create;
  InitFullColorSpaceEditorList;
  AddModuleUnloadProc(ModuleUnloadProc);

  RegisterFullColorSpaceEditor(csDEF, TJvDEFColorSpaceProperty);
{$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
{$ENDIF UNITVERSIONING}

finalization
  RemoveModuleUnloadProc(ModuleUnloadProc);
  FreeAndNil(FullColorSpaceEditorList);
{$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

