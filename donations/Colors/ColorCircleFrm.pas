{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ColorCircleFrm.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit ColorCircleFrm;

{$I jvcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ExtCtrls, StdCtrls, Spin,
  ColorDialogs, ColorCtrls, ColorSpaces, ColorRotate;

type
  TJvColorCircleForm = class(TJvBaseColorCircleForm)
    JvColorCircle: TJvColorCircle;
    RadioButtonCommonSettings: TRadioButton;
    RadioButtonAxisSettings: TRadioButton;
    PanelAxisSettings: TPanel;
    PanelCommonSettings: TPanel;
    ImageOld: TImage;
    ImageNew: TImage;
    JvFullColorTrackBarCommon: TJvFullColorTrackBar;
    JvFullColorTrackBarBlue: TJvFullColorTrackBar;
    JvFullColorTrackBarGreen: TJvFullColorTrackBar;
    JvFullColorTrackBarRed: TJvFullColorTrackBar;
    SpinEditComAxis0: TSpinEdit;
    SpinEditRedAxis0: TSpinEdit;
    SpinEditGreenAxis0: TSpinEdit;
    SpinEditBlueAxis0: TSpinEdit;
    SpinEditComAxis1: TSpinEdit;
    SpinEditComAxis2: TSpinEdit;
    SpinEditRedAxis1: TSpinEdit;
    SpinEditRedAxis2: TSpinEdit;
    SpinEditGreenAxis1: TSpinEdit;
    SpinEditGreenAxis2: TSpinEdit;
    SpinEditBlueAxis1: TSpinEdit;
    SpinEditBlueAxis2: TSpinEdit;
    LabelOld: TLabel;
    LabelNew: TLabel;
    LabelColorSpace: TLabel;
    LabelAxisSettings: TLabel;
    LabelComAxis0: TLabel;
    LabelComAxis1: TLabel;
    LabelComAxis2: TLabel;
    LabelAxis0: TLabel;
    LabelAxis1: TLabel;
    LabelAxis2: TLabel;
    LabelCommon: TLabel;
    LabelRed: TLabel;
    LabelGreen: TLabel;
    LabelBlue: TLabel;
    PanelGraphics: TPanel;
    ButtonGraphics: TButton;
    ButtonCancel: TButton;
    ButtonOK: TButton;
    ButtonApply: TButton;
    CheckBoxCom0: TCheckBox;
    CheckBoxCom1: TCheckBox;
    CheckBoxCom2: TCheckBox;
    CheckBoxRed0: TCheckBox;
    CheckBoxRed1: TCheckBox;
    CheckBoxRed2: TCheckBox;
    CheckBoxGreen0: TCheckBox;
    CheckBoxGreen1: TCheckBox;
    CheckBoxGreen2: TCheckBox;
    CheckBoxBlue0: TCheckBox;
    CheckBoxBlue1: TCheckBox;
    CheckBoxBlue2: TCheckBox;
    BevelOld: TBevel;
    BevelNew: TBevel;
    JvColorSpaceCombo: TJvColorSpaceCombo;
    JvColorAxisConfigCombo: TJvColorAxisConfigCombo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonGraphicsClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure JvColorSpaceComboChange(Sender: TObject);
    procedure CheckBoxSettingsClick(Sender: TObject);
    procedure JvColorCircleBlueColorChange(Sender: TObject);
    procedure JvColorCircleColorChange(Sender: TObject);
    procedure JvColorCircleGreenColorChange(Sender: TObject);
    procedure JvColorCircleRedColorChange(Sender: TObject);
    procedure JvColorAxisConfigComboChange(Sender: TObject);
    procedure JvColorCircleColorSpaceChange(Sender: TObject);
    procedure RadioButtonAxisClick(Sender: TObject);
    procedure SpinEditSettingsValueChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FExpanded: Boolean;
    FUpdating: Boolean;
    FExpandedWidth: Integer;
    FRedAxis: array [TJvAxisIndex] of Byte;
    FGreenAxis: array [TJvAxisIndex] of Byte;
    FBlueAxis: array [TJvAxisIndex] of Byte;
    FComAxis: array [TJvAxisIndex] of Byte;
    FAxisMin: array [TJvAxisIndex] of Byte;
    FAxisMax: array [TJvAxisIndex] of Byte;
    FSpinEditComAxes: array [TJvAxisIndex] of TSpinEdit;
    FSpinEditRedAxes: array [TJvAxisIndex] of TSpinEdit;
    FSpinEditGreenAxes: array [TJvAxisIndex] of TSpinEdit;
    FSpinEditBlueAxes: array [TJvAxisIndex] of TSpinEdit;
    FLabelComAxes: array [TJvAxisIndex] of TLabel;
    FLabelAxes: array [TJvAxisIndex] of TLabel;
    FCheckBoxCom: array [TJvAxisIndex] of TCheckBox;
    FCheckBoxRed: array [TJvAxisIndex] of TCheckBox;
    FCheckBoxGreen: array [TJvAxisIndex] of TCheckBox;
    FCheckBoxBlue: array [TJvAxisIndex] of TCheckBox;
    FFilled: Boolean;
    procedure FillInternalArrays;
  protected
    procedure Loaded; override;
    procedure SetOptions(const Value: TJvColorCircleDialogOptions); override;
    procedure SetRedDelta(const Value: TJvAxisDelta); override;
    procedure SetGreenDelta(const Value: TJvAxisDelta); override;
    procedure SetBlueDelta(const Value: TJvAxisDelta); override;
    procedure SetDelta(const Value: TJvColorDelta); override;
  public
    procedure UpdateDeltaValue;
    procedure CalcDeltaValue(ARotateColor: TJvRotateColor);
    procedure UpdateColorSpace;
    procedure UpdateCheckBoxStates;
    procedure UpdateAxisSettings;

    procedure Expand;
    procedure Collapse;

    property Expanded: Boolean read FExpanded;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  TypInfo, Math;

{$R *.dfm}

var
  GlobalLoop, GlobalRange: string;

function AxisIndexFromTag(ATag: Integer): TJvAxisIndex;
begin
  Result := TJvAxisIndex(ATag and $03);
end;

procedure TJvColorCircleForm.FormCreate(Sender: TObject);
begin
  Options := JvDefaultColorCircleDialogOptions - [roShowSaturation];
  JvColorAxisConfigCombo.Selected := JvColorCircle.AxisConfig;
end;

procedure TJvColorCircleForm.FillInternalArrays;
begin
  if not FFilled then
  begin
    FSpinEditComAxes[axIndex0] := SpinEditComAxis0;
    FSpinEditComAxes[axIndex1] := SpinEditComAxis1;
    FSpinEditComAxes[axIndex2] := SpinEditComAxis2;
    FSpinEditRedAxes[axIndex0] := SpinEditRedAxis0;
    FSpinEditRedAxes[axIndex1] := SpinEditRedAxis1;
    FSpinEditRedAxes[axIndex2] := SpinEditRedAxis2;
    FSpinEditGreenAxes[axIndex0] := SpinEditGreenAxis0;
    FSpinEditGreenAxes[axIndex1] := SpinEditGreenAxis1;
    FSpinEditGreenAxes[axIndex2] := SpinEditGreenAxis2;
    FSpinEditBlueAxes[axIndex0] := SpinEditBlueAxis0;
    FSpinEditBlueAxes[axIndex1] := SpinEditBlueAxis1;
    FSpinEditBlueAxes[axIndex2] := SpinEditBlueAxis2;
    FLabelComAxes[axIndex0] := LabelComAxis0;
    FLabelComAxes[axIndex1] := LabelComAxis1;
    FLabelComAxes[axIndex2] := LabelComAxis2;
    FLabelAxes[axIndex0] := LabelAxis0;
    FLabelAxes[axIndex1] := LabelAxis1;
    FLabelAxes[axIndex2] := LabelAxis2;
    FCheckBoxCom[axIndex0] := CheckBoxCom0;
    FCheckBoxCom[axIndex1] := CheckBoxCom1;
    FCheckBoxCom[axIndex2] := CheckBoxCom2;
    FCheckBoxRed[axIndex0] := CheckBoxRed0;
    FCheckBoxRed[axIndex1] := CheckBoxRed1;
    FCheckBoxRed[axIndex2] := CheckBoxRed2;
    FCheckBoxGreen[axIndex0] := CheckBoxGreen0;
    FCheckBoxGreen[axIndex1] := CheckBoxGreen1;
    FCheckBoxGreen[axIndex2] := CheckBoxGreen2;
    FCheckBoxBlue[axIndex0] := CheckBoxBlue0;
    FCheckBoxBlue[axIndex1] := CheckBoxBlue1;
    FCheckBoxBlue[axIndex2] := CheckBoxBlue2;
    FFilled := True;
  end;
end;

procedure TJvColorCircleForm.Loaded;
begin
  inherited Loaded;
  FExpandedWidth := Width;
end;

procedure TJvColorCircleForm.Collapse;
begin
  Width := PanelGraphics.Left - 1;
  PanelGraphics.Visible := False;
  ButtonGraphics.Caption := RsCollapsedCaption;
  FExpanded := False;
end;

procedure TJvColorCircleForm.Expand;
begin
  Width := FExpandedWidth;
  PanelGraphics.Visible := True;
  ButtonGraphics.Caption := RsExpandedCaption;
  FExpanded := True;
end;

procedure TJvColorCircleForm.SetOptions(const Value: TJvColorCircleDialogOptions);
var
  I: TJvAxisIndex;
  EnabledA, EnabledB: Boolean;
  LVisible: Boolean;

  procedure UpdateCheckBox(ACheckBox: TCheckBox);
  begin
    ACheckBox.Visible := EnabledA;
    ACheckBox.Checked := EnabledB;
  end;

begin
  inherited SetOptions(Value);
  FillInternalArrays;

  if roFullOpen in Options then
    Expand
  else
    Collapse;

  ButtonGraphics.Enabled := not (roPreventExpand in Options);

  ButtonApply.Visible := (roShowApply in Options);

  if roShowHelp in Options then
    BorderIcons := BorderIcons + [biHelp]
  else
    BorderIcons := BorderIcons - [biHelp];

  EnabledA := roAllowSpaceChange in Options;
  LabelColorSpace.Enabled := EnabledA;
  JvColorSpaceCombo.Enabled := EnabledA;

  LVisible := roShowOldPreview in Options;
  ImageOld.Visible := LVisible;
  LabelOld.Visible := LVisible;

  LVisible := roShowNewPreview in Options;
  ImageNew.Visible := LVisible;
  LabelNew.Visible := LVisible;

  EnabledA := roShowSaturation in Options;
  EnabledB := roDefaultRange in Options;

  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    UpdateCheckBox(FCheckBoxCom[I]);
    UpdateCheckBox(FCheckBoxRed[I]);
    UpdateCheckBox(FCheckBoxGreen[I]);
    UpdateCheckBox(FCheckBoxBlue[I]);
  end;

  if RadioButtonCommonSettings.Enabled and RadioButtonAxisSettings.Enabled then
    RadioButtonAxisSettings.Checked := True;

  UpdateAxisSettings;
  UpdateCheckBoxStates;
  UpdateColorSpace;
end;

procedure TJvColorCircleForm.UpdateColorSpace;
var
  I: TJvAxisIndex;
  LCaption: string;
  LColor: TJvFullColor;
begin
  if FUpdating then
    Exit;
  FillInternalArrays;

  FUpdating := True;

  with JvColorSpaceCombo.SelectedSpace do
  begin
    JvColorAxisConfigCombo.ColorID := ID;

    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      LCaption := AxisName[I];
      FLabelComAxes[I].Caption := LCaption;
      FLabelAxes[I].Caption := LCaption;

      FAxisMax[I] := AxisMax[I];
      FAxisMin[I] := AxisMin[I];
      FSpinEditComAxes[I].MaxValue := FAxisMax[I];
      FSpinEditComAxes[I].MinValue := -FAxisMax[I];
      FSpinEditRedAxes[I].MaxValue := FAxisMax[I];
      FSpinEditRedAxes[I].MinValue := -FAxisMax[I];
      FSpinEditGreenAxes[I].MaxValue := FAxisMax[I];
      FSpinEditGreenAxes[I].MinValue := -FAxisMax[I];
      FSpinEditBlueAxes[I].MaxValue := FAxisMax[I];
      FSpinEditBlueAxes[I].MinValue := -FAxisMax[I];

      LColor := ConvertFromRGB(TJvFullColor(clRed));
      FRedAxis[I] := GetAxisValue(LColor, I);

      LColor := ConvertFromRGB(TJvFullColor(clLime));
      FGreenAxis[I] := GetAxisValue(LColor, I);

      LColor := ConvertFromRGB(TJvFullColor(clBlue));
      FBlueAxis[I] := GetAxisValue(LColor, I);

      LColor := ConvertFromRGB(TJvFullColor(clDkGray));
      FComAxis[I] := GetAxisValue(LColor, I);
    end;

    JvColorCircle.ConvertColorToColorID(ID shl 24);

    FUpdating := False;
  end;
  CalcDeltaValue(rcRed);
  CalcDeltaValue(rcGreen);
  CalcDeltaValue(rcBlue);
end;

procedure TJvColorCircleForm.UpdateDeltaValue;
var
  I: TJvAxisIndex;
  ComAxis: array [TJvAxisIndex] of Integer;
  LColorID: TJvColorID;

  function CheckRange(Value: Integer; AMin: Byte; AMax: Byte): Byte;
  begin
    while Value < AMin do
      Inc(Value, AMax - AMin + 1);
    while Value > AMax do
      Dec(Value, AMax - AMin + 1);
    Result := Value;
  end;

begin
  if FUpdating then
    Exit;
  FillInternalArrays;

  FUpdating := True;

  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    ComAxis[I] := (RedDelta[I].Value + GreenDelta[I].Value + BlueDelta[I].Value) div 3;
    FSpinEditRedAxes[I].Value := RedDelta[I].Value;
    FSpinEditGreenAxes[I].Value := GreenDelta[I].Value;
    FSpinEditBlueAxes[I].Value := BlueDelta[I].Value;
    FSpinEditComAxes[I].Value := ComAxis[I];
  end;

  LColorID := JvColorSpaceCombo.ColorSpaceID;

  JvColorCircle.RedColor := LColorID shl 24;
  JvColorCircle.GreenColor := LColorID shl 24;
  JvColorCircle.BlueColor := LColorID shl 24;
  JvColorCircle.FullColor := LColorID shl 24;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    JvColorCircle.RedColor := JvColorCircle.RedColor or
      (CheckRange(RedDelta[I].Value + FRedAxis[I], FAxisMin[I], FAxisMax[I]) shl (Ord(I)*8));
    JvColorCircle.GreenColor := JvColorCircle.GreenColor or
      (CheckRange(GreenDelta[I].Value + FGreenAxis[I], FAxisMin[I], FAxisMax[I]) shl (Ord(I)*8));
    JvColorCircle.BlueColor := JvColorCircle.BlueColor or
      (CheckRange(BlueDelta[I].Value + FBlueAxis[I], FAxisMin[I], FAxisMax[I]) shl (Ord(I)*8));
    JvColorCircle.FullColor := JvColorCircle.FullColor or
      (CheckRange(ComAxis[I] + FComAxis[I], FAxisMin[I], FAxisMax[I]) shl (Ord(I)*8));
  end;

  FUpdating := False;
end;

procedure TJvColorCircleForm.RadioButtonAxisClick(Sender: TObject);
begin
  UpdateAxisSettings;
end;

procedure TJvColorCircleForm.UpdateAxisSettings;
var
  I: TJvAxisIndex;
  LColor: TColor;
  EnabledA, EnabledB: Boolean;
begin
  FillInternalArrays;
  EnabledA := roCommon in Options;

  if not EnabledA then
    RadioButtonAxisSettings.Checked := True;

  RadioButtonCommonSettings.Enabled := EnabledA;
  EnabledA := EnabledA and RadioButtonCommonSettings.Checked;
  PanelCommonSettings.Enabled := EnabledA;

  if EnabledA then
    LColor := clWindow
  else
    LColor := clBtnFace;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    FSpinEditComAxes[I].Enabled := EnabledA;
    FSpinEditComAxes[I].Color := LColor;
    FCheckBoxCom[I].Enabled := EnabledA;
    FLabelComAxes[I].Enabled := EnabledA;
  end;
  LabelCommon.Enabled := EnabledA;
  JvFullColorTrackBarCommon.Visible := EnabledA;
  if EnabledA then
    JvColorCircle.Styles := JvColorCircle.Styles + [crShowCommon]
  else
    JvColorCircle.Styles := JvColorCircle.Styles - [crShowCommon];

  EnabledA := (roRed in Options) or (roBlue in Options) or (roGreen in Options);

  if not EnabledA then
    RadioButtonCommonSettings.Checked := True;

  RadioButtonAxisSettings.Enabled := EnabledA;
  EnabledA := EnabledA and RadioButtonAxisSettings.Checked;
  PanelAxisSettings.Enabled := EnabledA;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    FLabelAxes[I].Enabled := EnabledA;

  EnabledB := EnabledA and (roRed in Options);
  if EnabledB then
    LColor := clWindow
  else
    LColor := clBtnFace;
  LabelRed.Enabled := EnabledB;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    FSpinEditRedAxes[I].Enabled := EnabledB;
    FSpinEditRedAxes[I].Color := LColor;
    FCheckBoxRed[I].Enabled := EnabledB;
  end;
  JvFullColorTrackBarRed.Visible := EnabledB;

  EnabledB := EnabledA and (roGreen in Options);
  if EnabledB then
    LColor := clWindow
  else
    LColor := clBtnFace;
  LabelGreen.Enabled := EnabledB;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    FSpinEditGreenAxes[I].Enabled := EnabledB;
    FSpinEditGreenAxes[I].Color := LColor;
    FCheckBoxGreen[I].Enabled := EnabledB;
  end;
  JvFullColorTrackBarGreen.Visible := EnabledB;

  EnabledB := EnabledA and (roBlue in Options);
  if EnabledB then
    LColor := clWindow
  else
    LColor := clBtnFace;
  LabelBlue.Enabled := EnabledB;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    FSpinEditBlueAxes[I].Enabled := EnabledB;
    FSpinEditBlueAxes[I].Color := LColor;
    FCheckBoxBlue[I].Enabled := EnabledB;
  end;
  JvFullColorTrackBarBlue.Visible := EnabledB;
end;

procedure TJvColorCircleForm.CheckBoxSettingsClick(Sender: TObject);
var
  AxisDelta: TJvAxisDelta;
  SaturationMethod: TJvSaturationMethod;
begin
  if FUpdating then
    Exit;

  FUpdating := True;

  with Sender as TCheckBox do
  begin
    if Checked then
      SaturationMethod := smLoop
    else
      SaturationMethod := smRange;

    case Tag and $30 of
      $00:
        begin
          AxisDelta := RedDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          RedDelta := AxisDelta;
        end;
      $10:
        begin
          AxisDelta := GreenDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          GreenDelta := AxisDelta;
        end;
      $20:
        begin
          AxisDelta := BlueDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          BlueDelta := AxisDelta;
        end;
      $30:
        begin
          AxisDelta := RedDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          RedDelta := AxisDelta;

          AxisDelta := GreenDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          GreenDelta := AxisDelta;

          AxisDelta := BlueDelta;
          AxisDelta[AxisIndexFromTag(Tag)].SaturationMethod := SaturationMethod;
          BlueDelta := AxisDelta;
        end;
    end;
  end;

  FUpdating := False;

  UpdateCheckBoxStates;
end;

procedure TJvColorCircleForm.UpdateCheckBoxStates;
var
  I: TJvAxisIndex;

  procedure UpdateCheckBox(ACheckBox: TCheckBox);
  var
    SaturationMethod: TJvSaturationMethod;
  begin
    SaturationMethod := smRange;

    case ACheckBox.Tag and $30 of
      $00:
        SaturationMethod := RedDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod;
      $10:
        SaturationMethod := GreenDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod;
      $20:
        SaturationMethod := BlueDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod;
      $30:
        if (RedDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod = smLoop) and
          (GreenDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod = smLoop) and
          (BlueDelta[AxisIndexFromTag(ACheckBox.Tag)].SaturationMethod = smLoop) then
          SaturationMethod := smLoop
        else
          SaturationMethod := smRange;
    end;

    if SaturationMethod = smLoop then
    begin
      ACheckBox.Caption := GlobalLoop;
      ACheckBox.Checked := True;
    end
    else
    begin
      ACheckBox.Caption := GlobalRange;
      ACheckBox.Checked := False;
    end;
  end;

begin
  if FUpdating then
    Exit;
  FillInternalArrays;

  FUpdating := True;
  for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
  begin
    UpdateCheckBox(FCheckBoxCom[I]);
    UpdateCheckBox(FCheckBoxRed[I]);
    UpdateCheckBox(FCheckBoxGreen[I]);
    UpdateCheckBox(FCheckBoxBlue[I]);
  end;
  FUpdating := False;
end;

procedure TJvColorCircleForm.ButtonGraphicsClick(Sender: TObject);
begin
  if Expanded then
    Collapse
  else
    Expand;
end;

procedure TJvColorCircleForm.ButtonApplyClick(Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Self);
end;

procedure TJvColorCircleForm.JvColorSpaceComboChange(Sender: TObject);
begin
  if not FUpdating then
    UpdateColorSpace;
end;

procedure TJvColorCircleForm.SetBlueDelta(const Value: TJvAxisDelta);
begin
  inherited SetBlueDelta(Value);
  if not FUpdating then
  begin
    UpdateDeltaValue;
    UpdateCheckBoxStates;
  end;
end;

procedure TJvColorCircleForm.SetDelta(const Value: TJvColorDelta);
begin
  inherited SetDelta(Value);
  if not FUpdating then
  begin
    UpdateDeltaValue;
    UpdateCheckBoxStates;
  end;
end;

procedure TJvColorCircleForm.SetGreenDelta(const Value: TJvAxisDelta);
begin
  inherited SetGreenDelta(Value);
  if not FUpdating then
  begin
    UpdateDeltaValue;
    UpdateCheckBoxStates;
  end;
end;

procedure TJvColorCircleForm.SetRedDelta(const Value: TJvAxisDelta);
begin
  inherited SetRedDelta(Value);
  if not FUpdating then
  begin
    UpdateDeltaValue;
    UpdateCheckBoxStates;
  end;
end;

procedure TJvColorCircleForm.CalcDeltaValue(ARotateColor: TJvRotateColor);
var
  I: TJvAxisIndex;
  AxisDelta: TJvAxisDelta;

  function GetDelta(OldValue: Integer; ColorAxisValue: Integer;
    InitAxisValue, AxisMin, AxisMax: Byte): Integer;
  var
    AxisLength: Integer;
    Offset1, Offset2, Offset3: Integer;
  begin
    AxisLength := AxisMax - AxisMin + 1;
    Offset1 := Abs(ColorAxisValue - AxisLength - OldValue - InitAxisValue);
    Offset2 := Abs(ColorAxisValue - OldValue - InitAxisValue);
    Offset3 := Abs(ColorAxisValue + AxisLength - OldValue - InitAxisValue);
    if (Offset1 < Offset2) and (Offset1 < Offset3) then
      Result := (ColorAxisValue - AxisLength) - InitAxisValue
    else
    if Offset2 < Offset3 then
      Result := ColorAxisValue - InitAxisValue
    else
      Result := (ColorAxisValue + AxisLength) - InitAxisValue;

    Result := EnsureRange(Result, -AxisLength, AxisLength);
  end;

begin
  if FUpdating then
    Exit;

  FUpdating := True;

  if ARotateColor = rcCommon then
  begin
    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      AxisDelta[I].Value := GetDelta(RedDelta[I].Value, GetAxisValue(JvColorCircle.FullColor, I),
        FComAxis[I], FAxisMin[I], FAxisMax[I]);
      AxisDelta[I].SaturationMethod := RedDelta[I].SaturationMethod;
    end;
    RedDelta := AxisDelta;
    GreenDelta := RedDelta;
    BlueDelta := RedDelta;
  end
  else
  begin
    if ARotateColor = rcRed then
    begin
      //RedDelta:=GetDelta(RedDelta,JvColorCircle.RedColor,FRedAxis0,FRedAxis1,FRedAxis2);
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        AxisDelta[I].Value := GetDelta(RedDelta[I].Value, GetAxisValue(JvColorCircle.RedColor, I),
          FRedAxis[I], FAxisMin[I], FAxisMax[I]);
        AxisDelta[I].SaturationMethod := RedDelta[I].SaturationMethod;
      end;
      RedDelta := AxisDelta;
    end;

    if ARotateColor = rcGreen then
    begin
      //GreenDelta:=GetDelta(GreenDelta,JvColorCircle.GreenColor,FGreenAxis0,FGreenAxis1,FGreenAxis2);
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        AxisDelta[I].Value := GetDelta(GreenDelta[I].Value, GetAxisValue(JvColorCircle.GreenColor, I),
          FGreenAxis[I], FAxisMin[I], FAxisMax[I]);
        AxisDelta[I].SaturationMethod := GreenDelta[I].SaturationMethod;
      end;
      GreenDelta := AxisDelta;
    end;

    if ARotateColor = rcBlue then
    begin
      //BlueDelta:=GetDelta(BlueDelta,JvColorCircle.BlueColor,FBlueAxis0,FBlueAxis1,FBlueAxis2);
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        AxisDelta[I].Value := GetDelta(BlueDelta[I].Value, GetAxisValue(JvColorCircle.BlueColor, I),
          FBlueAxis[I], FAxisMin[I], FAxisMax[I]);
        AxisDelta[I].SaturationMethod := BlueDelta[I].SaturationMethod;
      end;
      BlueDelta := AxisDelta;
    end;
  end;

  FUpdating := False;

  UpdateDeltaValue;
end;

procedure TJvColorCircleForm.JvColorCircleBlueColorChange(Sender: TObject);
begin
  if not FUpdating then
    CalcDeltaValue(rcBlue);
end;

procedure TJvColorCircleForm.JvColorCircleColorChange(Sender: TObject);
begin
  if not FUpdating then
    CalcDeltaValue(rcCommon);
end;

procedure TJvColorCircleForm.JvColorCircleGreenColorChange(Sender: TObject);
begin
  if not FUpdating then
    CalcDeltaValue(rcGreen);
end;

procedure TJvColorCircleForm.JvColorCircleRedColorChange(Sender: TObject);
begin
  if not FUpdating then
    CalcDeltaValue(rcRed);
end;

procedure TJvColorCircleForm.JvColorAxisConfigComboChange(Sender: TObject);
begin
  if not FUpdating then
  begin
    FUpdating := True;
    JvColorCircle.AxisConfig := (Sender as TJvColorAxisConfigCombo).Selected;
    FUpdating := False;
  end;
end;

procedure TJvColorCircleForm.JvColorCircleColorSpaceChange(Sender: TObject);
begin
  CalcDeltaValue(rcRed);
  CalcDeltaValue(rcGreen);
  CalcDeltaValue(rcBlue);
end;

procedure TJvColorCircleForm.SpinEditSettingsValueChange(Sender: TObject);
var
  AxisDelta: TJvAxisDelta;
begin
  if FUpdating then
    Exit;

  FUpdating := True;

  with Sender as TSpinEdit do
    case Tag and $30 of
      $00:
        begin
          AxisDelta := RedDelta;
          AxisDelta[AxisIndexFromTag(Tag)].Value := Value;
          RedDelta := AxisDelta;
        end;
      $10:
        begin
          AxisDelta := GreenDelta;
          AxisDelta[AxisIndexFromTag(Tag)].Value := Value;
          GreenDelta := AxisDelta;
        end;
      $20:
        begin
          AxisDelta := BlueDelta;
          AxisDelta[AxisIndexFromTag(Tag)].Value := Value;
          BlueDelta := AxisDelta;
        end;
      $30:
        begin
          AxisDelta := RedDelta;
          AxisDelta[AxisIndexFromTag(Tag)].Value := Value;
          RedDelta := AxisDelta;
          GreenDelta := AxisDelta;
          BlueDelta := AxisDelta;
        end;
    end;

  FUpdating := False;

  UpdateDeltaValue;
end;

procedure TJvColorCircleForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    with Sender as TCustomForm do
      if not ((ActiveControl is TCustomComboBox) and
        TCustomComboBox(ActiveControl).DroppedDown) then
        Close;
end;

procedure InitializeStrings;
var
  LTypInfo: PTypeInfo;
  LString: string;
begin
  LTypInfo := TypeInfo(TJvSaturationMethod);
  LString := GetEnumName(LTypInfo, Integer(smLoop));
  GlobalLoop := Copy(LString, 3, Length(LString) - 2);
  LString := GetEnumName(LTypInfo, Integer(smRange));
  GlobalRange := Copy(LString, 3, Length(LString) - 2);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

initialization
  InitializeStrings;
  ColorCircleBaseFormClass := TJvColorCircleForm;
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

