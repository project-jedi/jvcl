{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudickar att oratool dott de]
All Rights Reserved.

Contributor(s):
Jens Fudickar [jens dott fudickar att oratool dott de]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I vclonly.inc}

unit JvDynControlEngineJVCL;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl,
  JvMaskEdit, JvDateTimePicker, JvBitBtn, JvCheckBox, JvBaseEdits,
  JvLabel, JvListBox, JvMemo, JvPanel, JvRadioGroup, JvToolEdit,
  JvScrollBox, JvStaticText, JvComboBox, JvImage, JvSpin, JvCheckListBox,
  JvDynControlEngine, JvDynControlEngineIntf, JvGroupBox;

type
  TJvDynControlJVCLMaskEdit = class(TJvMaskEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);
  end;

  TJvDynControlJVCLButtonEdit = class (TJvPanel, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlButtonEdit,
    IJvDynControlButton)
  private
    FEditControl: TJvMaskEdit;
    FButton: TJvBitBtn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: variant);
    function ControlGetValue: variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlButtonEdit
    procedure ControlSetOnButtonClick(Value: TNotifyEvent);
    procedure ControlSetButtonCaption(const Value: string);

    //IJvDynControlButton
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

  TJvDynControlJVCLCalcEdit = class(TJvCalcEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
  end;

  TJvDynControlJVCLSpinEdit = class(TJvSpinEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlSpin, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlSpin
    procedure ControlSetIncrement(Value: Integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: Boolean);
  end;

  TJvDynControlJVCLFileNameEdit = class(TJvFileNameEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlFileName, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlFileName
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDefaultExt(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(const Value: string);
    procedure ControlSetFilterIndex(Value: Integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  TJvDynControlJVCLDirectoryEdit = class(TJvDirectoryEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDirectory, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlJVCLDateTimeEdit = class(TJvPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  private
    FDatePicker: TJvDateTimePicker;
    FTimePicker: TJvDateTimePicker;
  protected
    procedure ControlResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLDateEdit = class(TJvDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLTimeEdit = class(TJvDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLCheckBox = class(TJvCheckBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetReadonly(Value: Boolean);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
  end;

  TJvDynControlJVCLMemo = class(TJvMemo, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlMemo, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetReadonly(Value: Boolean);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
  end;

  TJvDynControlJVCLRadioGroup = class(TJvRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlRadioGroup, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlJVCLListBox = class(TJvListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLCheckListBox = class (TJvCheckListBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick,
    IJvDynControlCheckListBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    //IJvDynControlCheckListBox = interface
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetChecked(Index: Integer; Value: Boolean);
    procedure ControlSetItemEnabled(Index: Integer; Value: Boolean);
    procedure ControlSetHeader(Index: Integer; Value: Boolean);
    procedure ControlSetState(Index: Integer; Value: TCheckBoxState);
    function ControlGetChecked(Index: Integer): Boolean;
    function ControlGetItemEnabled(Index: Integer): Boolean;
    function ControlGetHeader(Index: Integer): Boolean;
    function ControlGetState(Index: Integer): TCheckBoxState;
  end;

  TJvDynControlJVCLComboBox = class(TJvComboBox, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    //procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlJVCLGroupBox = class (TJvGroupBox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLPanel = class(TJvPanel, IUnknown, IJvDynControl,
    IJvDynControlPanel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
  end;

  TJvDynControlJVCLImage = class(TJvImage, IUnknown, IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetAutoSize(Value: Boolean);
    procedure ControlSetIncrementalDisplay(Value: Boolean);
    procedure ControlSetCenter(Value: Boolean);
    procedure ControlSetProportional(Value: Boolean);
    procedure ControlSetStretch(Value: Boolean);
    procedure ControlSetTransparent(Value: Boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
  end;

  TJvDynControlJVCLScrollBox = class(TJvScrollbox, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLLabel = class(TJvLabel, IUnknown, IJvDynControl,
    IJvDynControlLabel)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: Boolean);
  end;

  TJvDynControlJVCLStaticText = class(TJvStaticText, IUnknown, IJvDynControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
  end;

  TJvDynControlJVCLButton = class(TJvBitBtn, IUnknown, IJvDynControl,
    IJvDynControlButton)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
  end;

function DynControlEngineJVCL: TJvDynControlEngine;

implementation

uses
  SysUtils,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  JvDynControlEngineVCL,
  JvJCLUtils;

var
  IntDynControlEngineJVCL: TJvDynControlEngine = nil;

//=== { TJvDynControlJVCLMaskEdit } ==========================================

procedure TJvDynControlJVCLMaskEdit.ControlSetDefaultProperties;
begin
  Button.Visible := False;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetPasswordChar(Value: Char);
begin
  PasswordChar := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetEditMask(const Value: string);
begin
  EditMask := Value;
end;

//=== { TJvDynControlJVCLButtonEdit } ============================================

constructor TJvDynControlJVCLButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TJvMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton     := TJvBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.Caption := '...';
  Height      := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner  := bvNone;
  BevelOuter  := bvNone;
end;

destructor TJvDynControlJVCLButtonEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetReadOnly(Value: boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetTabOrder(Value: integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  FEditControl.OnClick := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetValue(Value: variant);
begin
  FEditControl.Text := Value;
end;

function TJvDynControlJVCLButtonEdit.ControlGetValue: variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  FEditControl.PasswordChar := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetEditMask(const Value: string);
begin
  FEditControl.EditMask := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick:= Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetNumGlyphs(Value: integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;


//=== { TJvDynControlJVCLCalcEdit } ==========================================

procedure TJvDynControlJVCLCalcEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLCalcEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLCalcEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

//=== { TJvDynControlJVCLSpinEdit } ==========================================

procedure TJvDynControlJVCLSpinEdit.ControlSetDefaultProperties;
begin
  ButtonKind := bkDiagonal;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLSpinEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLSpinEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetIncrement(Value: Integer);
begin
  Increment := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMinValue(Value: double);
begin
  MinValue := Value;
  CheckMinValue := (MaxValue <> 0) and (MinValue <> 0);
  CheckMaxValue := CheckMinValue;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMaxValue(Value: double);
begin
  MaxValue := Value;
  CheckMinValue := (MaxValue <> 0) and (MinValue <> 0);
  CheckMaxValue := CheckMinValue;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetUseForInteger(Value: Boolean);
begin
  if Value then
    {$IFDEF BCB}
    ValueType := TValueType(vtInteger)
    {$ELSE}
    ValueType := vtInteger
    {$ENDIF BCB}
  else
    ValueType := vtFloat;
end;

//=== { TJvDynControlJVCLFileNameEdit } ======================================

procedure TJvDynControlJVCLFileNameEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLFileNameEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLFileNameEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  InitialDir := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  DefaultExt := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  DialogTitle := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  DialogOptions := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetFilter(const Value: string);
begin
  Filter := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetFilterIndex(Value: Integer);
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

//=== { TJvDynControlJVCLDirectoryEdit } =====================================

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDefaultProperties;
begin
  DialogKind := dkWin32;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLDirectoryEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDirectoryEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;


procedure TJvDynControlJVCLDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  InitialDir := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  DialogText := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  DialogOptions := Value;
end;

//=== { TJvDynControlJVCLDateTimeEdit } ======================================

constructor TJvDynControlJVCLDateTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDatePicker := TJvDateTimePicker.Create(Self);
  FDatePicker.Parent := Self;
  FDatePicker.Align := alLeft;
  FDatePicker.Top := 0;
  FDatePicker.Left := 0;
  FTimePicker := TJvDateTimePicker.Create(Self);
  FTimePicker.Align := alClient;
  FTimePicker.Parent := Self;
  FTimePicker.Top := 0;
  FTimePicker.Left := 0;
  Height := FDatePicker.Height;
  Width := FDatePicker.Width + FTimePicker.Width;
  OnResize := ControlResize;
  ControlResize(nil);
  FDatePicker.DateFormat := dfShort;
  FDatePicker.DateMode := dmComboBox;
  FDatePicker.Kind := dtkDate;
  FTimePicker.DateFormat := dfShort;
  FTimePicker.DateMode := dmUpDown;
  FTimePicker.Kind := dtkTime;
end;

destructor TJvDynControlJVCLDateTimeEdit.Destroy;
begin
  FreeAndNil(FDatePicker);
  FreeAndNil(FTimePicker);
  inherited Destroy;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlResize(Sender: TObject);
begin
  FDatePicker.Height := Height div 2;
  FTimePicker.Height := Height;
  FDatePicker.Width := Width div 2;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetDefaultProperties;
begin
end;


procedure TJvDynControlJVCLDateTimeEdit.ControlSetCaption(const Value: string);
begin
  //Caption := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FDatePicker.OnEnter := Value;
  FTimePicker.OnEnter := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FDatePicker.OnExit := Value;
  FTimePicker.OnExit := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FDatePicker.OnChange := Value;
  FTimePicker.OnChange := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetValue(Value: Variant);
begin
  FDatePicker.Date := Value;
  FTimePicker.Time := Value;
end;

function TJvDynControlJVCLDateTimeEdit.ControlGetValue: Variant;
begin
  Result := trunc(FDatePicker.Date) + (trunc(FTimePicker.Time) - FTimePicker.Time);
end;

// IJvDynControlDate

procedure TJvDynControlJVCLDateTimeEdit.ControlSetMinDate(Value: TDateTime);
begin
  FDatePicker.MinDate := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetMaxDate(Value: TDateTime);
begin
  FDatePicker.MaxDate := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  FDatePicker.Format := Value;
  FTimePicker.Format := Value;
  {$ENDIF COMPILER6_UP}
end;


//=== { TJvDynControlJVCLDateEdit } ==========================================

procedure TJvDynControlJVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLDateEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLDateEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLDateEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

// IJvDynControlDate

procedure TJvDynControlJVCLDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  MinDate := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  MaxDate := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== { TJvDynControlJVCLTimeEdit } ==========================================

procedure TJvDynControlJVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind := dtkTime;
  DateMode := dmUpDown;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLTimeEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetFormat(const Value: string);
begin
  {$IFDEF COMPILER6_UP}
  Format := Value;
  {$ENDIF COMPILER6_UP}
end;

//=== { TJvDynControlJVCLCheckBox } ==========================================

procedure TJvDynControlJVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCheckBox.ControlSetReadOnly(Value: Boolean);
begin
  Enabled := False;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetTabOrder(Value: Integer);
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
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetValue(Value: Variant);
begin
  if VarType(Value) = varBoolean then
    Checked := Value
  else
    Checked := UpperCase(Value) = 'TRUE';
end;

function TJvDynControlJVCLCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

//=== { TJvDynControlJVCLMemo } ==============================================

procedure TJvDynControlJVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLMemo.ControlSetValue(Value: Variant);
begin
  Text := Value;
end;

function TJvDynControlJVCLMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLMemo.ControlSetSorted(Value: Boolean);
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

procedure TJvDynControlJVCLMemo.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

//=== { TJvDynControlJVCLRadioGroup } ========================================

procedure TJvDynControlJVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetTabOrder(Value: Integer);
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
  OnClick := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlJVCLRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetSorted(Value: Boolean);
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

procedure TJvDynControlJVCLRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

//=== { TJvDynControlJVCLListBox } ===========================================

procedure TJvDynControlJVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLListBox.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlJVCLListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLListBox.ControlSetSorted(Value: Boolean);
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

//=== { TJvDynControlJVCLCheckListBox } ============================================

procedure TJvDynControlJVCLCheckListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlJVCLCheckListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLCheckListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

//IJvDynControlCheckListBox = interface
procedure TJvDynControlJVCLCheckListBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetChecked(Index: Integer; Value: Boolean);
begin
  Checked[Index] := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetItemEnabled(Index: Integer; Value: Boolean);
begin
  ItemEnabled[Index] := Value;
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetHeader(Index: Integer; Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  Header[Index] := Value;
  {$ENDIF VCL}
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetState(Index: Integer; Value: TCheckBoxState);
begin
  State[Index] := Value;
end;

function TJvDynControlJVCLCheckListBox.ControlGetChecked(Index: Integer): Boolean;
begin
  Result := Checked[Index];
end;

function TJvDynControlJVCLCheckListBox.ControlGetItemEnabled(Index: Integer): Boolean;
begin
  Result := ItemEnabled[Index];
end;

function TJvDynControlJVCLCheckListBox.ControlGetHeader(Index: Integer): Boolean;
begin
  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  Result := Header[Index];
  {$ELSE}
  Result := False;
  {$ENDIF VCL}
  {$ELSE}
  Result := False;
  {$ENDIF COMPILER6_UP}
end;

function TJvDynControlJVCLCheckListBox.ControlGetState(Index: Integer): TCheckBoxState;
begin
  Result := State[Index];
end;

//=== { TJvDynControlJVCLComboBox } ==========================================

procedure TJvDynControlJVCLComboBox.ControlSetDefaultProperties;
begin
end;

 //procedure TJvDynControlJVCLComboBox.ControlSetReadOnly(Value: Boolean);
 //begin
 //  ReadOnly := Value;
 //end;

procedure TJvDynControlJVCLComboBox.ControlSetCaption(const Value: string);
begin
end;

procedure TJvDynControlJVCLComboBox.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLComboBox.ControlSetValue(Value: Variant);
begin
  if Style = csDropDownList then
    ItemIndex := Items.IndexOf(Value)
  else
    Text := Value;
end;

function TJvDynControlJVCLComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLComboBox.ControlSetSorted(Value: Boolean);
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

procedure TJvDynControlJVCLComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Style := csDropDown
  else
    Style := csDropDownList;
end;

//=== { TJvDynControlJVCLGroupBox } ==============================================

procedure TJvDynControlJVCLGroupBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLGroupBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLGroupBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLGroupBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLGroupBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLGroupBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

//=== { TJvDynControlJVCLPanel } =============================================

procedure TJvDynControlJVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlJVCLPanel.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel;
  ABevelOuter: TPanelBevel; ABevelWidth: Integer;
  ABorderStyle: TBorderStyle; ABorderWidth: Integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner := ABevelInner;
  BevelOuter := ABevelOuter;
  BevelWidth := ABevelWidth;
end;

//=== { TJvDynControlJVCLImage } =============================================

procedure TJvDynControlJVCLImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLImage.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
  IncrementalDisplay := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetCenter(Value: Boolean);
begin
  Center := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetProportional(Value: Boolean);
begin
  {$IFDEF COMPILER6_UP}
  Proportional := Value;
  {$ENDIF COMPILER6_UP}
end;

procedure TJvDynControlJVCLImage.ControlSetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TJvDynControlJVCLImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlJVCLImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlJVCLImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

//=== { TJvDynControlJVCLScrollBox } =========================================

procedure TJvDynControlJVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLScrollBox.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetTabOrder(Value: Integer);
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

//=== { TJvDynControlJVCLLabel } =============================================

procedure TJvDynControlJVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLLabel.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

//=== { TJvDynControlJVCLStaticText } ========================================

procedure TJvDynControlJVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetTabOrder(Value: Integer);
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

//=== { TJvDynControlJVCLButton } ============================================

procedure TJvDynControlJVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLButton.ControlSetCaption(const Value: string);
begin
  Caption := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLButton.ControlSetNumGlyphs(Value: Integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;

function DynControlEngineJVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngineJVCL;
end;

type
  TJvDynControlEngineJVCL = class(TJvDynControlEngine)
    procedure RegisterControls; override;
  end;

procedure TJvDynControlEngineJVCL.RegisterControls;
begin
  RegisterControl(jctLabel, TJvDynControlJVCLLabel);
  RegisterControl(jctStaticText, TJvDynControlJVCLStaticText);
  RegisterControl(jctButton, TJvDynControlJVCLButton);
  RegisterControl(jctScrollBox, TJvDynControlJVCLScrollBox);
  RegisterControl(jctGroupBox, TJvDynControlJVCLGroupBox);
  RegisterControl(jctPanel, TJvDynControlJVCLPanel);
  RegisterControl(jctImage, TJvDynControlVCLImage);
  RegisterControl(jctCheckBox, TJvDynControlJVCLCheckBox);
  RegisterControl(jctComboBox, TJvDynControlJVCLComboBox);
  RegisterControl(jctListBox, TJvDynControlJVCLListBox);
  RegisterControl(jctCheckListBox, TJvDynControlJVCLCheckListBox);
  RegisterControl(jctRadioGroup, TJvDynControlJVCLRadioGroup);
  RegisterControl(jctDateTimeEdit, TJvDynControlJVCLDateTimeEdit);
  RegisterControl(jctTimeEdit, TJvDynControlJVCLTimeEdit);
  RegisterControl(jctDateEdit, TJvDynControlJVCLDateEdit);
  RegisterControl(jctEdit, TJvDynControlJVCLMaskEdit);
  RegisterControl(jctCalculateEdit, TJvDynControlJVCLCalcEdit);
  RegisterControl(jctSpinEdit, TJvDynControlJVCLSpinEdit);
  RegisterControl(jctDirectoryEdit, TJvDynControlJVCLDirectoryEdit);
  RegisterControl(jctFileNameEdit, TJvDynControlJVCLFileNameEdit);
  RegisterControl(jctMemo, TJvDynControlJVCLMemo);
  RegisterControl(jctButtonEdit, TJvDynControlJVCLButtonEdit);
end;

initialization
  IntDynControlEngineJVCL := TJvDynControlEngineJVCL.Create;
  SetDefaultDynControlEngine(IntDynControlEngineJVCL);

finalization
  FreeAndNil(IntDynControlEngineJVCL);

end.
