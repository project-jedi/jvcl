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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDynControlEngineJVCL;

{$I jvcl.inc}

{$IFDEF SUPPORTS_PLATFORM_WARNINGS}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF SUPPORTS_PLATFORM_WARNINGS}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  ActnList, Graphics, ComCtrls, ImgList,
  Classes,
  Controls, StdCtrls, ExtCtrls, Mask, Forms,
  Buttons, Dialogs, FileCtrl,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvMaskEdit, JvDateTimePicker, JvBitBtn, JvCheckBox, JvBaseEdits,
  JvLabel, JvListBox, JvMemo, JvRichEdit, JvPanel, JvRadioGroup, JvToolEdit,
  JvScrollBox, JvStaticText, JvCombobox, JvImage, JvSpin, JvCheckListBox,
  JvDynControlEngine, JvDynControlEngineIntf, JvGroupBox, JvComCtrls,
  JvProgressBar, JvStringGrid, Grids;

type
  TJvDynControlJVCLMaskEdit = class(TJvMaskEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);
  end;

  TJvDynControlJVCLButtonEdit = class(TJvPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
    IJvDynControlButtonEdit, IJvDynControlButton)
  private
    FEditControl: TJvMaskEdit;
    FButton: TJvBitBtn;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);

    //IJvDynControlButtonEdit
    procedure ControlSetOnButtonClick(Value: TNotifyEvent);
    procedure ControlSetButtonCaption(const Value: string);

    //IJvDynControlButton
    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
    procedure ControlSetDefault(Value: Boolean);
    procedure ControlSetCancel(Value: Boolean);
  end;

  TJvDynControlJVCLCalcEdit = class(TJvCalcEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlJVCLSpinEdit = class(TJvSpinEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlSpin, IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    // IJvDynControlSpin
    procedure ControlSetIncrement(Value: Integer);
    procedure ControlSetMinValue(Value: Double);
    procedure ControlSetMaxValue(Value: Double);
    procedure ControlSetUseForInteger(Value: Boolean);
  end;

  TJvDynControlJVCLFileNameEdit = class(TJvFileNameEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlFileName,
    IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

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
    IJvDynControl, IJvDynControlData, IJvDynControlDirectory,
    IJvDynControlReadOnly)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

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

    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLDateEdit = class(TJvDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLTimeEdit = class(TJvDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlJVCLCheckBox = class(TJvCheckBox, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData, IJvDynControlReadOnly,
    IJvDynControlCheckBox, IJvDynControlFont)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetReadOnly(Value: Boolean);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlCheckBox
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetState(Value: TCheckBoxState);
    function ControlGetState: TCheckBoxState;
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlJVCLMemo = class(TJvMemo, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo,
    IJvDynControlReadOnly,
    IJvDynControlAlignment, IJvDynControlFont)
  public
    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetReadOnly(Value: Boolean);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlJVCLRichEdit = class(TJvRichEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo,
    IJvDynControlReadOnly, IJvDynControlFont)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);
    procedure ControlSetReadOnly(Value: Boolean);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlJVCLRadioGroup = class(TJvRadioGroup, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData, IJvDynControlItems,
    IJvDynControlRadioGroup, IJvDynControlReadOnly)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlJVCLListBox = class(TJvListBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlItemIndex, IJvDynControlDblClick,
    IJvDynControlKey, IJvDynControlMouse)
  public
    function ControlGetItemIndex: Integer;
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    function ControlGetOnKeyDown: TKeyEvent;
    function ControlGetOnKeyPress: TKeyPressEvent;
    function ControlGetOnKeyUp: TKeyEvent;
    procedure ControlSetOnKeyDown(const Value: TKeyEvent);
    procedure ControlSetOnKeyPress(const Value: TKeyPressEvent);
    procedure ControlSetOnKeyUp(const Value: TKeyEvent);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetItemIndex(const Value: Integer);

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    function ControlGetOnMouseDown: TMouseEvent;
    function ControlGetOnMouseEnter: TNotifyEvent;
    function ControlGetOnMouseLeave: TNotifyEvent;
    function ControlGetOnMouseMove: TMouseMoveEvent;
    function ControlGetOnMouseUp: TMouseEvent;
    procedure ControlSetOnMouseDown(const Value: TMouseEvent);
    procedure ControlSetOnMouseEnter(const Value: TNotifyEvent);
    procedure ControlSetOnMouseLeave(const Value: TNotifyEvent);
    procedure ControlSetOnMouseMove(const Value: TMouseMoveEvent);
    procedure ControlSetOnMouseUp(const Value: TMouseEvent);
  end;

  TJvDynControlJVCLCheckListBox = class(TJvCheckListBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlDblClick,
    IJvDynControlCheckListBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

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
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlJVCLComboBox = class(TJvComboBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlJVCLGroupBox = class(TJvGroupBox, IUnknown,
    IJvDynControl, IJvDynControlCaption)
  public
    function ControlGetCaption: string;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
  end;

  TJvDynControlJVCLPanel = class(TJvPanel, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlPanel, IJvDynControlAlign,
    IJvDynControlAutoSize, IJvDynControlBevelBorder, IJvDynControlColor,
    IJvDynControlAlignment)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel;
      ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);

    // IJvDynControlAlign
    procedure ControlSetAlign(Value: TAlign);

    // IJvDynControlAutoSize
    procedure ControlSetAutoSize(Value: Boolean);

    // IJvDynControlBevelBorder
    procedure ControlSetBevelInner(Value: TBevelCut);
    procedure ControlSetBevelKind(Value: TBevelKind);
    procedure ControlSetBevelOuter(Value: TBevelCut);
    procedure ControlSetBorderStyle(Value: TBorderStyle);
    procedure ControlSetBorderWidth(Value: Integer);
    // IJvDynControlColor
    procedure ControlSetColor(Value: TColor);
    procedure ControlSetParentColor(Value: Boolean);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);
  end;

  TJvDynControlJVCLImage = class(TJvImage, IUnknown,
    IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetAutoSize(Value: Boolean);
    procedure ControlSetIncrementalDisplay(Value: Boolean);
    procedure ControlSetCenter(Value: Boolean);
    procedure ControlSetProportional(Value: Boolean);
    procedure ControlSetStretch(Value: Boolean);
    procedure ControlSetTransparent(Value: Boolean);
    procedure ControlSetPicture(Value: TPicture);
    procedure ControlSetGraphic(Value: TGraphic);
    function ControlGetPicture: TPicture;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlJVCLScrollBox = class(TJvScrollbox, IUnknown,
    IJvDynControl, IJvDynControlCaption)
  public
    function ControlGetCaption: string;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
  end;

  TJvDynControlJVCLLabel = class(TJvLabel, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlLabel, IJvDynControlAlign,
    IJvDynControlAutoSize,
    IJvDynControlAlignment, IJvDynControlFont)
  public
    function ControlGetCaption: string;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: Boolean);
    // IJvDynControlAlign
    procedure ControlSetAlign(Value: TAlign);

    // IJvDynControlAutoSize
    procedure ControlSetAutoSize(Value: Boolean);
    // IJvDynControlColor
    procedure ControlSetColor(Value: TColor);
    procedure ControlSetParentColor(Value: Boolean);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlJVCLStaticText = class(TJvStaticText, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlAlign,
    IJvDynControlAutoSize, IJvDynControlColor,
    IJvDynControlAlignment, IJvDynControlFont)
  public
    function ControlGetCaption: string;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    // IJvDynControlAlign
    procedure ControlSetAlign(Value: TAlign);

    // IJvDynControlAutoSize
    procedure ControlSetAutoSize(Value: Boolean);
    // IJvDynControlColor
    procedure ControlSetColor(Value: TColor);
    procedure ControlSetParentColor(Value: Boolean);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlJVCLButton = class(TJvBitBtn, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlButton, IJvDynControlAction)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
    procedure ControlSetDefault(Value: Boolean);
    procedure ControlSetCancel(Value: Boolean);

    // IJvDynControlAction
    procedure ControlSetAction(Value: TCustomAction);
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlJVCLRadioButton = class(TRadioButton, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    // IJvDynControlData
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
    procedure ControlSetAnchors(Value: TAnchors);
  end;

  TJvDynControlJVCLTreeView = class(TJvTreeView, IUnknown, IJvDynControl,
      IJvDynControlTreeView, IJvDynControlReadOnly, IJvDynControlDblClick,
      IJvDynControlMouse)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    // IJvDynControlReadOnly
    procedure ControlSetReadOnly(Value: Boolean);

    // IJvDynControlTreeView
    procedure ControlSetAutoExpand(Value: Boolean);
    procedure ControlSetHotTrack(Value: Boolean);
    procedure ControlSetShowHint(Value: Boolean);
    procedure ControlSetShowLines(Value: Boolean);
    procedure ControlSetShowRoot(Value: Boolean);
    procedure ControlSetToolTips(Value: Boolean);
    procedure ControlSetItems(Value: TTreeNodes);
    function ControlGetItems: TTreeNodes;
    procedure ControlSetImages(Value: TCustomImageList);
    procedure ControlSetStateImages(Value: TCustomImageList);
    function ControlGetSelected: TTreeNode;
    procedure ControlSetSelected(const Value: TTreeNode);
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetOnChange(Value: TTVChangedEvent);
    procedure ControlSetOnChanging(Value: TTVChangingEvent);
    procedure ControlSetSortType(Value: TSortType);
    procedure ControlSortItems;

    //IJvDynControlDblClick
    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    function ControlGetOnMouseDown: TMouseEvent;
    function ControlGetOnMouseEnter: TNotifyEvent;
    function ControlGetOnMouseLeave: TNotifyEvent;
    function ControlGetOnMouseMove: TMouseMoveEvent;
    function ControlGetOnMouseUp: TMouseEvent;
    procedure ControlSetOnMouseDown(const Value: TMouseEvent);
    procedure ControlSetOnMouseEnter(const Value: TNotifyEvent);
    procedure ControlSetOnMouseLeave(const Value: TNotifyEvent);
    procedure ControlSetOnMouseMove(const Value: TMouseMoveEvent);
    procedure ControlSetOnMouseUp(const Value: TMouseEvent);
  end;

  TJvDynControlJVCLProgressBar = class(TJvProgressBar, IUnknown, IJvDynControl,
      IJvDynControlCaption, IJvDynControlAlign, IJvDynControlProgressBar)
  public
    function ControlGetCaption: string;
    procedure ControlSetAlign(Value: TAlign);
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetDefaultProperties;
    procedure ControlSetHint(const Value: string);
    //IJvDynControlProgressBar
    procedure ControlSetMarquee(Value: Boolean);
    procedure ControlSetMax(Value: Integer);
    procedure ControlSetMin(Value: Integer);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOrientation(Value: TProgressBarOrientation);
    procedure ControlSetPosition(Value: Integer);
    procedure ControlSetSmooth(Value: Boolean);
    procedure ControlSetStep(Value: Integer);
    procedure ControlSetTabOrder(Value: Integer);
  end;

type
  TJvDynControlJVCLTabControl = class(TJvTabControl, IUnknown, IJvDynControl,
      IJvDynControlTabControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlTabControl
    procedure ControlCreateTab(const AName: string);
    procedure ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
    procedure ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
    procedure ControlSetTabIndex(Index: Integer);
    function ControlGetTabIndex: Integer;
    procedure ControlSetMultiLine(Value: Boolean);
    procedure ControlSetScrollOpposite(Value: Boolean);
    procedure ControlSetHotTrack(Value: Boolean);
    procedure ControlSetRaggedRight(Value: Boolean);
  end;

  TJvDynControlJVCLPageControl = class(TJvPageControl, IUnknown,
      IJvDynControl, IJvDynControlTabControl, IJvDynControlPageControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlTabControl
    procedure ControlCreateTab(const AName: string);
    procedure ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
    procedure ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
    procedure ControlSetTabIndex(Index: Integer);
    function ControlGetTabIndex: Integer;
    procedure ControlSetMultiLine(Value: Boolean);
    procedure ControlSetScrollOpposite(Value: Boolean);
    procedure ControlSetHotTrack(Value: Boolean);
    procedure ControlSetRaggedRight(Value: Boolean);

    //IJvDynControlPageControl
    function ControlGetPage(const PageName: string): TWinControl;
  end;

type
  TJvDynControlJVCLCheckedComboBox = class(TJvCheckedComboBox, IUnknown, IJvDynControl, IJvDynControlData,
      IJvDynControlItems, IJvDynControlDblClick, IJvDynControlCheckComboBox)
  public
    function ControlGetDelimiter: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDelimiter(Value: string);
  end;

  TJvDynControlJVCLStringGrid = class(TJvStringGrid, IUnknown, IJvDynControl, IJvDynControlStringGrid)
  public
    procedure ControlSetAnchors(Value: TAnchors);
    // IJvDynControl
    procedure ControlSetDefaultProperties;
    procedure ControlSetHint(const Value: string);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetTabOrder(Value: Integer);
    // IJvDynControlStringGrid
    function GetControlCells(ACol, ARow: Integer): string;
    function GetControlCol: Integer;
    function GetControlColCount: Integer; stdcall;
    function GetControlColWidths(Index: Integer): Integer;
    function GetControlFixedCols: Integer;
    function GetControlFixedRows: Integer;
    function GetControlObjects(ACol, ARow: Integer): TObject;
    function GetControlOptions: TGridOptions;
    function GetControlRow: Integer;
    function GetControlRowCount: Integer; stdcall;
    function GetControlRowHeights(Index: Integer): Integer;
    procedure SetControlCells(ACol, ARow: Integer; const Value: string);
    procedure SetControlCol(const Value: Integer);
    procedure SetControlColCount(const Value: Integer);
    procedure SetControlColWidths(Index: Integer; const Value: Integer);
    procedure SetControlFixedCols(const Value: Integer);
    procedure SetControlFixedRows(const Value: Integer);
    procedure SetControlObjects(ACol, ARow: Integer; Value: TObject);
    procedure SetControlOptions(const Value: TGridOptions);
    procedure SetControlRow(const Value: Integer);
    procedure SetControlRowCount(const Value: Integer); stdcall;
    procedure SetControlRowHeights(Index: Integer; const Value: Integer);
  end;


function DynControlEngineJVCL: TJvDynControlEngine;
procedure SetDynControlEngineJVCLDefault;

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
  SysUtils, Variants,
  JvDynControlEngineTools, JvDynControlEngineVCL, JvJCLUtils;

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

procedure TJvDynControlJVCLMaskEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetPasswordChar(Value: Char);
begin
  PasswordChar := Value;
end;

procedure TJvDynControlJVCLMaskEdit.ControlSetEditMask(const Value: string);
begin
  EditMask := Value;
end;

//=== { TJvDynControlJVCLButtonEdit } ========================================

constructor TJvDynControlJVCLButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TJvMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TJvBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.Caption := '...';
  Height := FEditControl.Height;
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
  Self.Caption := ' ';
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetTabOrder(Value: Integer);
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

procedure TJvDynControlJVCLButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := VarToStr(Value);
end;

function TJvDynControlJVCLButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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
  FButton.OnClick := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetDefault(Value: Boolean);
begin
  FButton.Default := Value;
end;

procedure TJvDynControlJVCLButtonEdit.ControlSetCancel(Value: Boolean);
begin
  FButton.Cancel := Value;
end;


//=== { TJvDynControlJVCLCalcEdit } ==========================================

procedure TJvDynControlJVCLCalcEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
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

procedure TJvDynControlJVCLCalcEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLCalcEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLCalcEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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

procedure TJvDynControlJVCLSpinEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLSpinEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetIncrement(Value: Integer);
begin
  Increment := Value;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMinValue(Value: Double);
begin
  MinValue := Value;
  CheckMinValue := (MaxValue <> 0) and (MinValue <> 0);
  CheckMaxValue := CheckMinValue;
end;

procedure TJvDynControlJVCLSpinEdit.ControlSetMaxValue(Value: Double);
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

procedure TJvDynControlJVCLFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLFileNameEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLFileNameEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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

procedure TJvDynControlJVCLDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLDirectoryEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLDirectoryEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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

procedure TJvDynControlJVCLDateTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetValue(Value: Variant);
begin
  FDatePicker.Date := Value;
  FTimePicker.Time := Value;
end;

function TJvDynControlJVCLDateTimeEdit.ControlGetValue: Variant;
begin
  Result := Trunc(FDatePicker.Date) + (Trunc(FTimePicker.Time) - FTimePicker.Time);
end;

procedure TJvDynControlJVCLDateTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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
  FDatePicker.Format := Value;
  FTimePicker.Format := Value;
end;

//=== { TJvDynControlJVCLDateEdit } ==========================================

procedure TJvDynControlJVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
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

procedure TJvDynControlJVCLDateEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetValue(Value: Variant);
begin
  Date := Value;
end;

function TJvDynControlJVCLDateEdit.ControlGetValue: Variant;
begin
  Result := Date;
end;

procedure TJvDynControlJVCLDateEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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
  Format := Value;
end;

//=== { TJvDynControlJVCLTimeEdit } ==========================================

procedure TJvDynControlJVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind := dtkTime;
  DateMode := dmUpDown;
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

procedure TJvDynControlJVCLTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetValue(Value: Variant);
begin
  Time := Value;
end;

function TJvDynControlJVCLTimeEdit.ControlGetValue: Variant;
begin
  Result := Time;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLTimeEdit.ControlSetFormat(const Value: string);
begin
  Format := Value;
end;

//=== { TJvDynControlJVCLCheckBox } ===========================================

function TJvDynControlJVCLCheckBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCheckBox.ControlSetReadOnly(Value: Boolean);
begin
  Enabled := False;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlJVCLCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

//IJvDynControlCheckBox
procedure TJvDynControlJVCLCheckBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetState(Value: TCheckBoxState);
begin
  State := Value;
end;

function TJvDynControlJVCLCheckBox.ControlGetState: TCheckBoxState;
begin
  Result := State;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLCheckBox.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlJVCLCheckBox.ControlGetFont: TFont;
begin
  Result := Font;
end;


function TJvDynControlJVCLMemo.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlJVCLMemo } ==============================================

procedure TJvDynControlJVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
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

procedure TJvDynControlJVCLMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
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

procedure TJvDynControlJVCLMemo.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
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

procedure TJvDynControlJVCLMemo.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TJvDynControlJVCLMemo.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlJVCLRichEdit.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlJVCLRichEdit } ==========================================

procedure TJvDynControlJVCLRichEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLRichEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLRichEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlJVCLRichEdit.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlJVCLRichEdit.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TJvDynControlJVCLRichEdit.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlJVCLRichEdit.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

//=== { TJvDynControlJVCLRadioGroup } ===========================================

function TJvDynControlJVCLRadioGroup.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
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

procedure TJvDynControlJVCLRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
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

procedure TJvDynControlJVCLRadioGroup.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

function TJvDynControlJVCLListBox.ControlGetItemIndex: Integer;
begin
  Result := ItemIndex;
end;

//=== { TJvDynControlJVCLListBox } ===========================================

procedure TJvDynControlJVCLListBox.ControlSetDefaultProperties;
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

procedure TJvDynControlJVCLListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
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

procedure TJvDynControlJVCLListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetItemIndex(const Value: Integer);
begin
  ItemIndex := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

function TJvDynControlJVCLListBox.ControlGetOnKeyDown: TKeyEvent;
begin
  Result := OnKeyDown;
end;

function TJvDynControlJVCLListBox.ControlGetOnKeyPress: TKeyPressEvent;
begin
  Result := OnKeyPress;
end;

function TJvDynControlJVCLListBox.ControlGetOnKeyUp: TKeyEvent;
begin
  Result := OnKeyUp;
end;

function TJvDynControlJVCLListBox.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlJVCLListBox.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlJVCLListBox.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlJVCLListBox.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlJVCLListBox.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnKeyDown(const Value: TKeyEvent);
begin
  OnKeyDown := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnKeyPress(const Value: TKeyPressEvent);
begin
  OnKeyPress := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnKeyUp(const Value: TKeyEvent);
begin
  OnKeyUp := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlJVCLListBox.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlJVCLListBox.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlJVCLListBox.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

//=== { TJvDynControlJVCLCheckListBox } ======================================

procedure TJvDynControlJVCLCheckListBox.ControlSetDefaultProperties;
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

procedure TJvDynControlJVCLCheckListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
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
  Header[Index] := Value;
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
  Result := Header[Index];
end;

function TJvDynControlJVCLCheckListBox.ControlGetState(Index: Integer): TCheckBoxState;
begin
  Result := State[Index];
end;

procedure TJvDynControlJVCLCheckListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlJVCLComboBox } ==========================================

procedure TJvDynControlJVCLComboBox.ControlSetDefaultProperties;
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

procedure TJvDynControlJVCLComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetValue(Value: Variant);
begin
  if (Style = csDropDownList) then
    if VarIsInt(Value) then
      ItemIndex := VarToInt(Value)
    else
      ItemIndex := Items.IndexOf(VarToStr(Value))
  else
    Text := VarToStr(Value);
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

procedure TJvDynControlJVCLComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Style := csDropDown
  else
    Style := csDropDownList;
end;

//=== { TJvDynControlJVCLGroupBox } ===========================================

function TJvDynControlJVCLGroupBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlJVCLGroupBox } ==========================================

procedure TJvDynControlJVCLGroupBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLGroupBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLGroupBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLGroupBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

//=== { TJvDynControlJVCLPanel } ===========================================

function TJvDynControlJVCLPanel.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlJVCLPanel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLPanel.ControlSetHint(const Value: string);
begin
  Hint := Value;
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


procedure TJvDynControlJVCLPanel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetBevelInner(Value: TBevelCut);
begin
  BevelInner:= Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetBevelKind(Value: TBevelKind);
begin
  BevelKind := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetBevelOuter(Value: TBevelCut);
begin
  BevelOuter:= Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle:= Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetBorderWidth(Value: Integer);
begin
  BorderWidth := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlJVCLPanel.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

//=== { TJvDynControlJVCLImage } =============================================

procedure TJvDynControlJVCLImage.ControlSetDefaultProperties;
begin
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

procedure TJvDynControlJVCLImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
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
  Proportional := Value;
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

procedure TJvDynControlJVCLImage.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlJVCLScrollBox } =========================================

procedure TJvDynControlJVCLScrollBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLScrollBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLLabel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLLabel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLLabel.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlJVCLLabel.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

//=== { TJvDynControlJVCLScrollBox } ===========================================

function TJvDynControlJVCLScrollBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLScrollBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

//=== { TJvDynControlJVCLLabel } ===========================================

function TJvDynControlJVCLLabel.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLLabel.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlJVCLLabel.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlJVCLStaticText } ===========================================

function TJvDynControlJVCLStaticText.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlJVCLStaticText } ========================================

procedure TJvDynControlJVCLStaticText.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLStaticText.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLStaticText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TJvDynControlJVCLStaticText.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlJVCLStaticText.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlJVCLButton } ===========================================

function TJvDynControlJVCLButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlJVCLButton } ============================================

procedure TJvDynControlJVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
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

procedure TJvDynControlJVCLButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
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

procedure TJvDynControlJVCLButton.ControlSetDefault(Value: Boolean);
begin
  Default := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetCancel(Value: Boolean);
begin
  Cancel := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetAction(Value: TCustomAction);
begin
  Action := Value;
end;

procedure TJvDynControlJVCLButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlJVCLRadioButton } ===========================================

function TJvDynControlJVCLRadioButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlJVCLRadioButton } =======================================

procedure TJvDynControlJVCLRadioButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLRadioButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

// IJvDynControlData
procedure TJvDynControlJVCLRadioButton.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlJVCLRadioButton.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlJVCLRadioButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlJVCLTreeView } ==========================================

procedure TJvDynControlJVCLTreeView.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLTreeView.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetAutoExpand(Value: Boolean);
begin
  AutoExpand := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetShowLines(Value: Boolean);
begin
  ShowLines := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetShowRoot(Value: Boolean);
begin
  ShowRoot := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetToolTips(Value: Boolean);
begin
  ToolTips := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetItems(Value: TTreeNodes);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLTreeView.ControlGetItems: TTreeNodes;
begin
  Result := Items;
end;

function TJvDynControlJVCLTreeView.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlJVCLTreeView.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlJVCLTreeView.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlJVCLTreeView.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlJVCLTreeView.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlJVCLTreeView.ControlSetImages(Value: TCustomImageList);
begin
  Images.Assign(Value);
end;

procedure TJvDynControlJVCLTreeView.ControlSetStateImages(Value: TCustomImageList);
begin
  StateImages.Assign(Value);
end;

function TJvDynControlJVCLTreeView.ControlGetSelected: TTreeNode;
begin
  Result := Selected;
end;

procedure TJvDynControlJVCLTreeView.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnChange(Value: TTVChangedEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnChanging(Value:
    TTVChangingEvent);
begin
  OnChanging := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetSortType(Value: TSortType);
begin
  SortType := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSetSelected(const Value: TTreeNode);
begin
  Selected := Value;
end;

procedure TJvDynControlJVCLTreeView.ControlSortItems;
begin
  AlphaSort;
end;

//=== { TJvDynControlJVCLProgressBar } ===========================================

function TJvDynControlJVCLProgressBar.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlJVCLProgressBar.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

//=== { TJvDynControlJVCLProgressbar } =======================================

procedure TJvDynControlJVCLProgressbar.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLProgressbar.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLProgressBar.ControlSetMarquee(Value: Boolean);
begin
  {$IFDEF DELPHI2009_UP}
  if Value then
    Style := pbstMarquee
  else
    Style := pbstNormal;
  {$ENDIF DELPHI2009_UP}
end;

procedure TJvDynControlJVCLProgressbar.ControlSetMax(Value: Integer);
begin
  Max := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetMin(Value: Integer);
begin
  Min := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetOrientation(Value: TProgressBarOrientation);
begin
  Orientation:= Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetSmooth(Value: Boolean);
begin
  Smooth := Value;
end;

procedure TJvDynControlJVCLProgressbar.ControlSetStep(Value: Integer);
begin
  Step := Value;
end;


procedure TJvDynControlJVCLTabControl.ControlCreateTab(const AName: string);
begin
  Tabs.Add(AName);
end;

function TJvDynControlJVCLTabControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlJVCLTabControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

//=== { TJvDynControlJVCLPageControl } =======================================

procedure TJvDynControlJVCLPageControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLPageControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlCreateTab(const AName: string);
var
  TabSheet: TTabSheet;
begin
  TabSheet := TTabSheet.Create(Self);
  TabSheet.Caption := AName;
  TabSheet.PageControl := Self;
  TabSheet.Parent := Self;
end;

procedure TJvDynControlJVCLPageControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlJVCLPageControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlJVCLPageControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

function TJvDynControlJVCLPageControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlJVCLPageControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlJVCLPageControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

function TJvDynControlJVCLPageControl.ControlGetPage(const PageName: string): TWinControl;
var
  i: Integer;
begin
  i := Tabs.IndexOf(PageName);
  if (i >= 0) and (i < PageCount) then
    Result := TWinControl(Pages[i])
  else
    Result := nil;
end;

//=== { TJvDynControlJVCLTabControl } ========================================

procedure TJvDynControlJVCLTabControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLTabControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlJVCLTabControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlJVCLTabControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLTabControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

procedure TJvDynControlJVCLTabControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

//=== { TJvDynControlJVCLCheckedComboBox } ======================================

procedure TJvDynControlJVCLCheckedComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
//  OnChange := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlJVCLCheckedComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlJVCLCheckedComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

function TJvDynControlJVCLCheckedComboBox.ControlGetDelimiter: string;
begin
  Result := Delimiter;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLCheckedComboBox.ControlSetDelimiter(Value: string);
begin
  if Value <> '' then
    Delimiter:= Value[1]
  else
    Delimiter := chr(0);
end;

procedure TJvDynControlJVCLStringGrid.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlJVCLStringGrid.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlJVCLStringGrid.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlJVCLStringGrid.ControlSetOnChange(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlJVCLStringGrid.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlJVCLStringGrid.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlJVCLStringGrid.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlJVCLStringGrid.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

function TJvDynControlJVCLStringGrid.GetControlCells(ACol, ARow: Integer): string;
begin
  Result := Cells[ACol, ARow];
end;

function TJvDynControlJVCLStringGrid.GetControlCol: Integer;
begin
  Result := Col;
end;

function TJvDynControlJVCLStringGrid.GetControlColCount: Integer;
begin
  Result := ColCount;
end;

function TJvDynControlJVCLStringGrid.GetControlColWidths(Index: Integer): Integer;
begin
  Result := ColWidths[Index];
end;

function TJvDynControlJVCLStringGrid.GetControlFixedCols: Integer;
begin
  Result := FixedCols;
end;

function TJvDynControlJVCLStringGrid.GetControlFixedRows: Integer;
begin
  Result := FixedRows;
end;

function TJvDynControlJVCLStringGrid.GetControlObjects(ACol, ARow: Integer): TObject;
begin
  Result := Objects[ACol, ARow];
end;

function TJvDynControlJVCLStringGrid.GetControlOptions: TGridOptions;
begin
  Result := Options;
end;

function TJvDynControlJVCLStringGrid.GetControlRow: Integer;
begin
  Result := Row;
end;

function TJvDynControlJVCLStringGrid.GetControlRowCount: Integer;
begin
  Result := RowCount;
end;

function TJvDynControlJVCLStringGrid.GetControlRowHeights(Index: Integer): Integer;
begin
  Result := RowHeights[Index];
end;

procedure TJvDynControlJVCLStringGrid.SetControlCells(ACol, ARow: Integer; const Value: string);
begin
  Cells[ACol, ARow] := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlCol(const Value: Integer);
begin
  Col := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlColCount(const Value: Integer);
begin
  ColCount := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlColWidths(Index: Integer; const Value: Integer);
begin
  ColWidths[Index] := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlFixedCols(const Value: Integer);
begin
  FixedCols := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlFixedRows(const Value: Integer);
begin
  FixedRows := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlObjects(ACol, ARow: Integer; Value: TObject);
begin
  Objects[ACol, ARow] := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlOptions(const Value: TGridOptions);
begin
  Options := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlRow(const Value: Integer);
begin
  Row := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlRowCount(const Value: Integer);
begin
  RowCount := Value;
end;

procedure TJvDynControlJVCLStringGrid.SetControlRowHeights(Index: Integer; const Value: Integer);
begin
  RowHeights [Index] := Value;
end;

//=== { TJvDynControlEngineJVCL } ============================================

type
  TJvDynControlEngineJVCL = class(TJvDynControlEngine)
  public
    procedure RegisterControls; override;
  end;

function DynControlEngineJVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngineJVCL;
end;

procedure SetDynControlEngineJVCLDefault;
begin
  SetDefaultDynControlEngine(IntDynControlEngineJVCL);
end;

procedure TJvDynControlEngineJVCL.RegisterControls;
begin
  RegisterControlType(jctLabel, TJvDynControlJVCLLabel);
  RegisterControlType(jctStaticText, TJvDynControlJVCLStaticText);
  RegisterControlType(jctButton, TJvDynControlJVCLButton);
  RegisterControlType(jctRadioButton, TJvDynControlJVCLRadioButton);
  RegisterControlType(jctScrollBox, TJvDynControlJVCLScrollBox);
  RegisterControlType(jctGroupBox, TJvDynControlJVCLGroupBox);
  RegisterControlType(jctPanel, TJvDynControlJVCLPanel);
  RegisterControlType(jctImage, TJvDynControlVCLImage);
  RegisterControlType(jctCheckBox, TJvDynControlJVCLCheckBox);
  RegisterControlType(jctComboBox, TJvDynControlJVCLComboBox);
  RegisterControlType(jctListBox, TJvDynControlJVCLListBox);
  RegisterControlType(jctCheckListBox, TJvDynControlJVCLCheckListBox);
  RegisterControlType(jctCheckComboBox, TJvDynControlJVCLCheckedComboBox);
  RegisterControlType(jctRadioGroup, TJvDynControlJVCLRadioGroup);
  RegisterControlType(jctDateTimeEdit, TJvDynControlJVCLDateTimeEdit);
  RegisterControlType(jctTimeEdit, TJvDynControlJVCLTimeEdit);
  RegisterControlType(jctDateEdit, TJvDynControlJVCLDateEdit);
  RegisterControlType(jctEdit, TJvDynControlJVCLMaskEdit);
  RegisterControlType(jctCalculateEdit, TJvDynControlJVCLCalcEdit);
  RegisterControlType(jctSpinEdit, TJvDynControlJVCLSpinEdit);
  RegisterControlType(jctDirectoryEdit, TJvDynControlJVCLDirectoryEdit);
  RegisterControlType(jctFileNameEdit, TJvDynControlJVCLFileNameEdit);
  RegisterControlType(jctMemo, TJvDynControlJVCLMemo);
  RegisterControlType(jctRichEdit, TJvDynControlJVCLRichEdit);
  RegisterControlType(jctButtonEdit, TJvDynControlJVCLButtonEdit);
  RegisterControlType(jctTreeView, TJvDynControlJVCLTreeView);
  RegisterControlType(jctProgressbar, TJvDynControlJVCLProgressbar);
  RegisterControlType(jctTabControl, TJvDynControlJVCLTabControl);
  RegisterControlType(jctPageControl, TJvDynControlJVCLPageControl);
  {$IFDEF DELPHI7_UP}
  RegisterControlType(jctColorComboBox, TJvDynControlVCLColorComboBox);
  {$ENDIF}
  RegisterControlType(jctStringGrid, TJvDynControlJVCLStringGrid);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  IntDynControlEngineJVCL := TJvDynControlEngineJVCL.Create;
  SetDefaultDynControlEngine(IntDynControlEngineJVCL);

finalization
  FreeAndNil(IntDynControlEngineJVCL);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
