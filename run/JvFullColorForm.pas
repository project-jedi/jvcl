{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FullColorFrm.pas, released on 2004-09-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvFullColorForm;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  Dialogs, StdCtrls, ExtCtrls, Mask,
  JvFullColorCtrls, JvFullColorSpaces, JvFullColorDialogs, JvExMask,
  JvSpin, JvExStdCtrls, JvCombobox, JvColorCombo, JvComponent;

type
  TJvFullColorFrm = class(TJvForm)
    LabelColorSpace: TLabel;
    GroupBoxSettings: TGroupBox;
    ScrollBarAxis0: TScrollBar;
    ScrollBarAxis1: TScrollBar;
    ScrollBarAxis2: TScrollBar;
    SpinEditAxis0: TJvSpinEdit;
    SpinEditAxis1: TJvSpinEdit;
    SpinEditAxis2: TJvSpinEdit;
    LabelAxis0: TLabel;
    LabelAxis1: TLabel;
    LabelAxis2: TLabel;
    LabelPredefined: TLabel;
    PanelGraphic: TPanel;
    JvColorPanel: TJvFullColorPanel;
    JvFullColorTrackBar: TJvFullColorTrackBar;
    ButtonGraphics: TButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    LabelDrawOld: TLabel;
    LabelDrawNew: TLabel;
    LabelOld: TLabel;
    LabelNew: TLabel;
    LabelAxis: TLabel;
    ButtonApply: TButton;
    JvColorAxisConfigCombo: TJvFullColorAxisCombo;
    JvColorSpaceCombo: TJvFullColorSpaceCombo;
    ColorBox: TJvColorComboBox;
    JvFullColorGroup: TJvFullColorGroup;
    procedure ButtonGraphicsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure JvComboBoxColorSpaceSelect(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure JvColorPanelColorChange(Sender: TObject);
    procedure ComboBoxAxisChange(Sender: TObject);
    procedure ComboBoxPredefinedSelect(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure LabelDrawOldClick(Sender: TObject);
    procedure JvFullColorGroupChange(Sender: TObject);
  private
    FUpdating: Boolean;
    FExpanded: Boolean;
    FExpandedWidth: Integer;
    FFullColor: TJvFullColor;
    FOptions: TJvFullColorDialogOptions;
    FOnApply: TNotifyEvent;
    FScrollBarAxes: array [TJvAxisIndex] of TScrollBar;
    FSpinEditAxes: array [TJvAxisIndex] of TJvSpinEdit;
    FLabelAxes: array [TJvAxisIndex] of TLabel;
    FFilled: Boolean;
    procedure FillInternalArrays;
  protected
    procedure UpdateColorValue;
    procedure UpdateColorSpace;
    procedure SetFullColor(const Value: TJvFullColor);
    procedure SetOptions(const Value: TJvFullColorDialogOptions);
    procedure Loaded; override;
    property Expanded: Boolean read FExpanded;
  public
    constructor Create(AOwner: TComponent; AFullColor: TJvFullColor;
      AOptions: TJvFullColorDialogOptions); reintroduce;
    procedure Expand;
    procedure Collapse;
    property Options: TJvFullColorDialogOptions read FOptions write SetOptions;
    property FullColor: TJvFullColor read FFullColor write SetFullColor;
    property OnApply: TNotifyEvent read FOnApply write FOnApply;
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

uses
  JvJCLUtils,   // for TryStrToInt
  JvResources;

{$R *.dfm}

function AxisIndexFromTag(ATag: Integer): TJvAxisIndex;
begin
  Result := TJvAxisIndex(ATag and $03);
end;

constructor TJvFullColorFrm.Create(AOwner: TComponent;
  AFullColor: TJvFullColor; AOptions: TJvFullColorDialogOptions);
begin
  inherited Create(AOwner);
  FOptions := AOptions;
  FFullColor := AFullColor;
end;

procedure TJvFullColorFrm.FormCreate(Sender: TObject);
var
  CS: TJvDEFColorSpace;
  Index:Integer;
begin
  with ColorSpaceManager do
  begin
    CS := TJvDEFColorSpace(ColorSpace[csDEF]);
    for Index := 0 to CS.ColorCount - 1 do
      JvFullColorGroup.Items.Add(CS.ConvertFromColor(CS.ColorValue[Index]));
    LabelDrawOld.Color := ConvertToColor(FullColor);
    JvColorSpaceCombo.ColorSpaceID := GetColorSpaceID(FullColor);
  end;
  SetFullColor(FullColor);
  SetOptions(Options);
end;

procedure TJvFullColorFrm.Loaded;
begin
  inherited Loaded;
  FExpandedWidth := Width;
end;

procedure TJvFullColorFrm.FillInternalArrays;
begin
  if not FFilled then
  begin
    FScrollBarAxes[axIndex0] := ScrollBarAxis0;
    FScrollBarAxes[axIndex1] := ScrollBarAxis1;
    FScrollBarAxes[axIndex2] := ScrollBarAxis2;
    FSpinEditAxes[axIndex0] := SpinEditAxis0;
    FSpinEditAxes[axIndex1] := SpinEditAxis1;
    FSpinEditAxes[axIndex2] := SpinEditAxis2;
    FLabelAxes[axIndex0] := LabelAxis0;
    FLabelAxes[axIndex1] := LabelAxis1;
    FLabelAxes[axIndex2] := LabelAxis2;
    FFilled := True;
  end;
end;

procedure TJvFullColorFrm.ButtonGraphicsClick(Sender: TObject);
begin
  if Expanded then
    Collapse
  else
    Expand;
end;

procedure TJvFullColorFrm.Expand;
begin
  PanelGraphic.Visible := True;
  Width := FExpandedWidth;
  ButtonGraphics.Caption := RsExpandedCaption;
  FExpanded := True;
end;

procedure TJvFullColorFrm.Collapse;
begin
  Width := PanelGraphic.Left - 1;
  PanelGraphic.Visible := False;
  ButtonGraphics.Caption := RsCollapsedCaption;
  FExpanded := False;
end;

procedure TJvFullColorFrm.SpinEditChange(Sender: TObject);
var
  IntValue:Integer;
begin
  if FUpdating then
    Exit;
  FUpdating := True;

  with Sender as TJvSpinEdit do
    if TryStrToInt(Text,IntValue) then
      FullColor := SetAxisValue(FullColor, AxisIndexFromTag(Tag), IntValue);

  FUpdating := False;
  UpdateColorValue;
end;

procedure TJvFullColorFrm.ScrollBarChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  with Sender as TScrollBar do
    FullColor := SetAxisValue(FullColor, AxisIndexFromTag(Tag), Position);
  FUpdating := False;
  UpdateColorValue;
end;

procedure TJvFullColorFrm.JvColorPanelColorChange(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  FullColor := (Sender as TJvFullColorPanel).FullColor;
  FUpdating := False;
  UpdateColorValue;
end;

procedure TJvFullColorFrm.JvFullColorGroupChange(Sender: TObject);
begin
  if FUpdating then
    Exit;

  FUpdating := True;
  with (Sender as TJvFullColorGroup), ColorSpaceManager do
    if (SelectedIndex>-1) then
  begin
    JvColorSpaceCombo.ColorSpaceID := GetColorSpaceID(Selected);
    FullColor := Selected;
  end;
  FUpdating := False;

  UpdateColorSpace;
end;

procedure TJvFullColorFrm.JvComboBoxColorSpaceSelect(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  with Sender as TJvFullColorSpaceCombo do
    FullColor := ColorSpaceManager.ConvertToID(FullColor, ColorSpaceID);
  FUpdating := False;
  UpdateColorSpace;
end;

procedure TJvFullColorFrm.ComboBoxPredefinedSelect(Sender: TObject);
begin
  if FUpdating then
    Exit;
  FUpdating := True;
  with Sender as TJvColorComboBox, ColorSpaceManager do
    FullColor := ConvertToID(ConvertFromColor(Colors[ItemIndex]),
                             GetColorSpaceID(FullColor));
  FUpdating := False;
  UpdateColorSpace;
end;

procedure TJvFullColorFrm.ComboBoxAxisChange(Sender: TObject);
begin
  JvColorPanel.AxisConfig := (Sender as TJvFullColorAxisCombo).Selected;
end;

procedure TJvFullColorFrm.UpdateColorValue;
var
  I: TJvAxisIndex;
  C: TColor;
  NewIndex: Integer;
  ValueAxes: array [TJvAxisIndex] of Byte;
  J: Integer;
  LColorID: TJvFullColorSpaceID;
  DefColorSpace: TJvColorSpace;
begin
  if FUpdating then
    Exit;
  FillInternalArrays;

  FUpdating := True;

  LabelDrawNew.Color := ColorSpaceManager.ConvertToColor(FullColor);
  LabelDrawNew.Update;

  LColorID := ColorSpaceManager.GetColorSpaceID(FullColor);

  if (LColorID=csDEF) then
  begin
    JvFullColorGroup.Selected := FullColor;

    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      FScrollBarAxes[I].Enabled := False;
      FScrollBarAxes[I].Position := 0;
      FSpinEditAxes[I].Enabled := False;
      FSpinEditAxes[I].Value := 0;
    end;
  end
  else
  begin
    JvColorPanel.FullColor := FullColor;

    for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
    begin
      FScrollBarAxes[I].Enabled := True;
      FSpinEditAxes[I].Enabled := True;
      ValueAxes[I] := GetAxisValue(FullColor, I);
      FScrollBarAxes[I].Position := ValueAxes[I];
      FSpinEditAxes[I].Value := ValueAxes[I];
    end;
  end;

  JvColorSpaceCombo.ColorSpaceID := LColorID;

  NewIndex := -1;
  DefColorSpace := ColorSpaceManager.ColorSpace[csDEF];
  with ColorBox, Items, ColorSpaceManager do
  begin
    for J := 0 to Items.Count - 1 do
    begin
      C := DefColorSpace.ConvertFromColor(Colors[J]);
      if ConvertToID(C, LColorID) = FullColor then
      begin
        NewIndex := J;
        Break;
      end;
    end;
    ItemIndex := NewIndex;
  end;

  FUpdating := False;
end;

procedure TJvFullColorFrm.UpdateColorSpace;
var
  I: TJvAxisIndex;
  AxisMin, AxisMax: Byte;
  LColorSpace: TJvColorSpace;
begin
  if FUpdating then
    Exit;
  FillInternalArrays;

  FUpdating := True;

  LColorSpace := JvColorSpaceCombo.SelectedSpace;

  if Assigned(LColorSpace) then
  begin
    if (LColorSpace.ID = csDEF) then
    begin
      PanelGraphic.Visible := False;
      JvFullColorGroup.Visible := Expanded;

      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        FScrollBarAxes[I].Enabled := False;
        FScrollBarAxes[I].Position := 0;
        FSpinEditAxes[I].Enabled := False;
        FSpinEditAxes[I].Value := 0;
      end;
    end
    else
    begin
      PanelGraphic.Visible := Expanded;
      JvFullColorGroup.Visible := False;

      JvColorAxisConfigCombo.ColorID := LColorSpace.ID;
      for I := Low(TJvAxisIndex) to High(TJvAxisIndex) do
      begin
        FLabelAxes[I].Caption := LColorSpace.AxisName[I];
        AxisMin := LColorSpace.AxisMin[I];
        AxisMax := LColorSpace.AxisMax[I];
        FScrollBarAxes[I].Min := AxisMin;
        FScrollBarAxes[I].Max := AxisMax;
        FSpinEditAxes[I].MinValue := AxisMin;
        FSpinEditAxes[I].MaxValue := AxisMax;
      end;

      JvColorPanel.FullColor := FullColor;
      JvFullColorTrackBar.FullColor := FullColor;
    end;
  end;

  FUpdating := False;

  UpdateColorValue;
end;

procedure TJvFullColorFrm.ButtonApplyClick(Sender: TObject);
begin
  if Assigned(OnApply) then
    OnApply(Sender);
end;

procedure TJvFullColorFrm.SetFullColor(const Value: TJvFullColor);
begin
  FFullColor := Value;
  if not FUpdating then
  begin
    with ColorSpaceManager do
      JvColorSpaceCombo.ColorSpaceID := GetColorSpaceID(Value);
    UpdateColorSpace;
  end;
end;

procedure TJvFullColorFrm.SetOptions(const Value: TJvFullColorDialogOptions);
var
  LVisible: Boolean;
  LColor: TColor;
begin
  FOptions := Value;

  if foFullOpen in Options then
    Expand
  else
    Collapse;

  ButtonGraphics.Enabled := not (foPreventExpand in Options);

  ButtonApply.Visible := (foShowApply in Options);

  if foShowHelp in Options then
    BorderIcons := BorderIcons + [biHelp]
  else
    BorderIcons := BorderIcons - [biHelp];

  JvColorSpaceCombo.Enabled := foAllowSpaceChange in Options;

  LVisible := foShowOldPreview in Options;
  LabelDrawOld.Visible := LVisible;
  LabelOld.Visible := LVisible;

  LVisible := foShowNewPreview in Options;
  LabelDrawNew.Visible := LVisible;
  LabelNew.Visible := LVisible;

  LVisible := foShowPredefined in Options;
  ColorBox.Visible := LVisible;
  LabelPredefined.Visible := LVisible;

  JvColorSpaceCombo.AllowVariable := foAllowVariable in Options;
  if foAllowVariable in Options then
    ColorBox.Options := ColorBox.Options + [coSysColors]
  else
    ColorBox.Options := ColorBox.Options - [coSysColors];
{  if foNoneAndDefault in Options then
    ColorBox.Style := ColorBox.Style + [cbIncludeNone, cbIncludeDefault]
  else
    ColorBox.Style := ColorBox.Style - [cbIncludeNone, cbIncludeDefault];}

  UpdateColorSpace;

  LColor := ColorSpaceManager.ConvertToColor(FullColor);

  LabelDrawNew.Color := LColor;
  LabelDrawOld.Color := LColor;
end;

procedure TJvFullColorFrm.LabelDrawOldClick(Sender: TObject);
begin
  with ColorSpaceManager do
    FullColor := ConvertToID(ConvertFromColor(LabelDrawOld.Color),
                             GetColorSpaceID(FullColor));
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
