{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens.fudickar@oratool.de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens.fudickar@oratool.de]

Last Modified: 2003-11-03

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDynControlEngine_JVCL;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvMaskEdit, JvDateTimePicker, JvBitBtn, JvCheckBox, JvBaseEdits,
  JvLabel, JvListBox, JvMemo, JvPanel, JvRadioGroup, JvToolEdit,
  JvScrollBox, JvStaticText, JvComboBox, JvImage, JvSpin,
  JvDynControlEngine, JvDynControlEngine_Interface;

type
  TJvDynControlJVCLMaskEdit = class (TJvMaskEdit, IUnknown, IJvDynControl,
    IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlJVCLCalcEdit = class (TJvCalcEdit, IUnknown, IJvDynControl,
    IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlJVCLSpinEdit = class (TJvSpinEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlSpin)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlSpin
    procedure ControlSetIncrement(Value: integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: boolean);
  end;

  TJvDynControlJVCLFileNameEdit = class (TJvFileNameEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlFileName)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlFileName
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDefaultExt(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(Value: string);
    procedure ControlSetFilterIndex(Value: integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  TJvDynControlJVCLDirectoryEdit = class (TJvDirectoryEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDirectory)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(Value: string);
    procedure ControlSetDialogTitle(Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlJVCLDateTimeEdit = class (TJvDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlJVCLDateEdit = class (TJvDynControlJVCLDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlJVCLTimeEdit = class (TJvDynControlJVCLDateTimeEdit)
  public
    procedure ControlSetDefaultProperties;
  end;

  TJvDynControlJVCLCheckBox = class (TJvCheckBox, IUnknown,
    IJvDynControl, IJvDynControlData)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;
  end;

  TJvDynControlJVCLMemo = class (TJvMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: boolean);
    procedure ControlSetWantReturns(Value: boolean);
    procedure ControlSetWordWrap(Value: boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
  end;

  TJvDynControlJVCLRadioGroup = class (TJvRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: integer);
  end;

  TJvDynControlJVCLListBox = class (TJvListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLComboBox = class (TJvComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    procedure ControlSetSorted(Value: boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: boolean);
  end;

  TJvDynControlJVCLPanel = class (TJvPanel, IUnknown, IJvDynControl,
    IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle; ABorderWidth: integer);
  end;

  TJvDynControlJVCLImage = class (TJvImage, IUnknown, IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetAutoSize(Value: boolean);
    procedure ControlSetIncrementalDisplay(Value: boolean);
    procedure ControlSetCenter(Value: boolean);
    procedure ControlSetProportional(Value: boolean);
    procedure ControlSetStretch(Value: boolean);
    procedure ControlSetTransparent(Value: boolean);
    procedure ControlSetPicture(Value: TPicture);
    function ControlGetPicture: TPicture;
  end;

  TJvDynControlJVCLScrollBox = class (TJvScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

  end;

  TJvDynControlJVCLLabel = class (TJvLabel, IUnknown, IJvDynControl,
    IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);
  end;

  TJvDynControlJVCLStaticText = class (TJvStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLButton = class (TJvBitBtn, IUnknown, IJvDynControl,
    IJvDynControlButton)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

function DynControlEngine_JVCL: TJvDynControlEngine;

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  SysUtils;

var
  IntDynControlEngine_JVCL: TJvDynControlEngine;

//=== TJvDynControlJVCLMaskEdit ==============================================

procedure TJvDynControlJVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlJVCLMaskEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLMaskEdit.ControlGetValue: variant;
begin
  Result := Text;
end;


//=== TJvDynControlJVCLCalcEdit ==============================================

procedure TJvDynControlJVCLCalcEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlJVCLCalcEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLCalcEdit.ControlGetValue: variant;
begin
  Result := Text;
end;

//=== TJvDynControlJVCLSpinEdit ==============================================

procedure TJvDynControlJVCLSpinEdit.ControlSetDefaultProperties;
begin
  ButtonKind := bkDiagonal;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlJVCLSpinEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLSpinEdit.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetIncrement(Value: integer);
begin
  Increment := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMinValue(Value: double);
begin
  MinValue      := Value;
  CheckMinValue := (MaxValue <> 0) and (MinValue <> 0);
  CheckMaxValue := CheckMinValue;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMaxValue(Value: double);
begin
  MaxValue      := Value;
  CheckMinValue := (MaxValue <> 0) and (MinValue <> 0);
  CheckMaxValue := CheckMinValue;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetUseForInteger(Value: boolean);
begin
  if Value then
    ValueType := vtInteger
  else
    ValueType := vtFloat;
end;

//=== TJvDynControlJVCLFileNameEdit ==========================================

procedure TJvDynControlJVCLFileNameEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLFileNameEdit.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetInitialDir(Value: string);
begin
  InitialDir := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDefaultExt(Value: string);
begin
  DefaultExt := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDialogTitle(Value: string);
begin
  DialogTitle := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  DialogOptions := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetFilter(Value: string);
begin
  Filter := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetFilterIndex(Value: integer);
begin
  FilterIndex := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  case Value of
    jdkOpen:
      DialogKind := dkOpen;
    jdkOpenPicture:
      DialogKind := dkOpenPicture;
    jdkSave:
      DialogKind := dkSave;
    jdkSavePicture:
      DialogKind := dkSavePicture;
  end;
end;

//=== TJvDynControlJVCLDirectoryEdit =========================================

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDefaultProperties;
begin
  DialogKind := dkWin32;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDirectoryEdit.ControlGetValue: variant;
begin
  Result := Text;
end;


procedure TJvDynControlJVCLDirectoryEdit.ControlSetInitialDir(Value: string);
begin
  InitialDir := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDialogTitle(Value: string);
begin
  DialogText := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  DialogOptions := Value;
end;


//=== TJvDynControlJVCLDateTimeEdit ==========================================

procedure TJvDynControlJVCLDateTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetReadOnly(Value: boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDateTimeEdit.ControlGetValue: variant;
begin
  Result := Text;
end;

//=== TJvDynControlJVCLDateEdit ==============================================

procedure TJvDynControlJVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

//=== TJvDynControlJVCLTimeEdit ==============================================

procedure TJvDynControlJVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmUpDown;
  Kind := dtkTime;
end;

//=== TJvDynControlJVCLCheckBox ==============================================

procedure TJvDynControlJVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCheckBox.ControlSetReadOnly(Value: boolean);
begin
end;

procedure TJvDynControlJVCLCheckBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetValue(Value: variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := UpperCase(Value) = 'TRUE';
end;

function TJvDynControlJVCLCheckBox.ControlGetValue: variant;
begin
  Result := Checked;
end;

//=== TJvDynControlJVCLMemo ==================================================

procedure TJvDynControlJVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetReadOnly(Value: boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLMemo.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLMemo.ControlSetSorted(Value: boolean);
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlJVCLMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlJVCLMemo.ControlSetWantTabs(Value: boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetWantReturns(Value: boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetWordWrap(Value: boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

//=== TJvDynControlJVCLRadioGroup ============================================

procedure TJvDynControlJVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetReadOnly(Value: boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetValue(Value: variant);
begin
  if VarType(Value) in [varSmallint, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on E: Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlJVCLRadioGroup.ControlGetValue: variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetSorted(Value: boolean);
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLRadioGroup.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetColumns(Value: integer);
begin
  Columns := Value;
end;

//=== TJvDynControlJVCLListBox ===============================================

procedure TJvDynControlJVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLListBox.ControlSetReadOnly(Value: boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLListBox.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetValue(Value: variant);
begin
  if VarType(Value) in [varSmallint, varInteger] then
    ItemIndex := Value
  else
    try
      ItemIndex := Value
    except
      on E: Exception do
        ItemIndex := Items.IndexOf(Value);
    end;
end;

function TJvDynControlJVCLListBox.ControlGetValue: variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLListBox.ControlSetSorted(Value: boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

//=== TJvDynControlJVCLComboBox ==============================================

procedure TJvDynControlJVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLComboBox.ControlSetReadOnly(Value: boolean);
begin
//  ReadOnly := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetCaption(Value: string);
begin
end;

procedure TJvDynControlJVCLComboBox.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetValue(Value: variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLComboBox.ControlGetValue: variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLComboBox.ControlSetSorted(Value: boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLComboBox.ControlSetNewEntriesAllowed(Value: boolean);
begin
  if Value then
    Style := csDropDown
  else
    Style := csDropDownList;
end;

//=== TJvDynControlJVCLPanel =================================================

procedure TJvDynControlJVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlJVCLPanel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: integer; ABorderStyle: TBorderStyle; ABorderWidth: integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner  := ABevelInner;
  BevelOuter  := ABevelOuter;
  BevelWidth  := ABevelWidth;
end;

 //****************************************************************************
 // TJvDynControlJVCLImage
 //****************************************************************************

procedure TJvDynControlJVCLImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLImage.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetTabOrder(Value: integer);
begin
//  TabOrder := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
//  OnEnter := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetOnExit(Value: TNotifyEvent);
begin
//  OnExit := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetAutoSize(Value: boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetIncrementalDisplay(Value: boolean);
begin
  IncrementalDisplay := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetCenter(Value: boolean);
begin
  Center := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetProportional(Value: boolean);
begin
  Proportional := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetStretch(Value: boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetTransparent(Value: boolean);
begin
  Transparent := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

function TJvDynControlJVCLImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

//=== TJvDynControlJVCLScrollBox =============================================

procedure TJvDynControlJVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLScrollBox.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== TJvDynControlJVCLLabel =================================================

procedure TJvDynControlJVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetTabOrder(Value: integer);
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

//=== TJvDynControlJVCLStaticText ============================================

procedure TJvDynControlJVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetTabOrder(Value: integer);
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== TJvDynControlJVCLButton ================================================

procedure TJvDynControlJVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLButton.ControlSetCaption(Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetTabOrder(Value: integer);
begin
end;

procedure TJvDynControlJVCLButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLButton.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetGlyph(Value: TBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlJVCLButton.ControlSetNumGlyphs(Value: integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;

function DynControlEngine_JVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngine_JVCL;
end;

initialization
  IntDynControlEngine_JVCL := TJvDynControlEngine.Create;
  IntDynControlEngine_JVCL.RegisterControl(jctLabel, TJvDynControlJVCLLabel);
  IntDynControlEngine_JVCL.RegisterControl(jctStaticText, TJvDynControlJVCLStaticText);
  IntDynControlEngine_JVCL.RegisterControl(jctButton, TJvDynControlJVCLButton);
  IntDynControlEngine_JVCL.RegisterControl(jctScrollBox, TJvDynControlJVCLScrollBox);
  IntDynControlEngine_JVCL.RegisterControl(jctPanel, TJvDynControlJVCLPanel);
  IntDynControlEngine_JVCL.RegisterControl(jctImage, TJvDynControlJVCLImage);
  IntDynControlEngine_JVCL.RegisterControl(jctCheckBox, TJvDynControlJVCLCheckBox);
  IntDynControlEngine_JVCL.RegisterControl(jctComboBox, TJvDynControlJVCLComboBox);
  IntDynControlEngine_JVCL.RegisterControl(jctListBox, TJvDynControlJVCLListBox);
  IntDynControlEngine_JVCL.RegisterControl(jctRadioGroup, TJvDynControlJVCLRadioGroup);
  IntDynControlEngine_JVCL.RegisterControl(jctDateTimeEdit, TJvDynControlJVCLDateTimeEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctTimeEdit, TJvDynControlJVCLTimeEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctDateEdit, TJvDynControlJVCLDateEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctEdit, TJvDynControlJVCLMaskEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctCalculateEdit, TJvDynControlJVCLCalcEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctSpinEdit, TJvDynControlJVCLSpinEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctDirectoryEdit, TJvDynControlJVCLDirectoryEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctFileNameEdit, TJvDynControlJVCLFileNameEdit);
  IntDynControlEngine_JVCL.RegisterControl(jctMemo, TJvDynControlJVCLMemo);
  SetDefaultDynControlEngine(IntDynControlEngine_JVCL);

finalization
  FreeAndNil(IntDynControlEngine_JVCL);

end.
