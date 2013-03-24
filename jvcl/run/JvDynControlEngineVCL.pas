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

unit JvDynControlEngineVCL;

{$I jvcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  ActnList, Graphics, ComCtrls, ImgList,
  {$ENDIF MSWINDOWS}
  Variants, Classes, Controls, StdCtrls, ExtCtrls, Mask, Forms,
  Buttons, Dialogs, FileCtrl, ExtDlgs, CheckLst,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  JvDynControlEngine, JvDynControlEngineIntf, Grids;

type
  TJvDynControlEngineVCL = class(TJvDynControlEngine)
  protected
    procedure RegisterControls; override;
  end;

  TJvDynControlVCLMaskEdit = class(TMaskEdit, IUnknown,
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
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);
  end;

  TJvDynControlVCLButtonEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlReadOnly, IJvDynControlEdit,
    IJvDynControlButtonEdit, IJvDynControlButton)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
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
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

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

  TJvDynControlVCLFileNameEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlFileName,
    IJvDynControlReadOnly)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    FFilterIndex: Integer;
    FFilter: string;
    FDialogOptions: TOpenOptions;
    FDialogKind: TJvDynControlFileNameDialogKind;
    FDialogTitle: string;
    FDefaultExt: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultOnButtonClick(Sender: TObject);

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

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

  TJvDynControlVCLDirectoryEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDirectory,
    IJvDynControlReadOnly)
  private
    FEditControl: TMaskEdit;
    FButton: TBitBtn;
    FInitialDir: string;
    FDialogOptions: TSelectDirOpts;
    FDialogTitle: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DefaultOnButtonClick(Sender: TObject);

    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlVCLDateTimeEdit = class(TPanel, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  private
    FDatePicker: TDateTimePicker;
    FTimePicker: TDateTimePicker;
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
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlVCLDateEdit = class(TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlDate)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlVCLTimeEdit = class(TDateTimePicker, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlTime)
  public
    procedure ControlSetDefaultProperties;

    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlVCLCheckBox = class(TCheckBox, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData, IJvDynControlCheckBox, IJvDynControlFont)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlCheckBox
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetState(Value: TCheckBoxState);
    function ControlGetState: TCheckBoxState;

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlVCLMemo = class(TMemo, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo,
    IJvDynControlReadOnly, IJvDynControlAlignment,IJvDynControlFont)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);
    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlVCLRichEdit = class(TRichEdit, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlMemo,
    IJvDynControlReadOnly,IJvDynControlFont)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetReadOnly(Value: Boolean);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);

    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlVCLRadioGroup = class(TRadioGroup, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData, IJvDynControlItems,
    IJvDynControlRadioGroup)
  public
    function ControlGetCaption: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetCaption(const Value: string);
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlVCLListBox = class(TListBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlItemIndex, IJvDynControlDblClick,
    IJvDynControlKey, IJvDynControlMouse)
  public
    function ControlGetItemIndex: Integer;
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetItemIndex(const Value: Integer);

    procedure ControlSetOnDblClick(Value: TNotifyEvent);
    function ControlGetOnKeyDown: TKeyEvent;
    function ControlGetOnKeyPress: TKeyPressEvent;
    function ControlGetOnKeyUp: TKeyEvent;
    procedure ControlSetOnKeyDown(const Value: TKeyEvent);
    procedure ControlSetOnKeyPress(const Value: TKeyPressEvent);

    function ControlGetOnMouseDown: TMouseEvent;
    function ControlGetOnMouseEnter: TNotifyEvent;
    function ControlGetOnMouseLeave: TNotifyEvent;
    function ControlGetOnMouseMove: TMouseMoveEvent;
    function ControlGetOnMouseUp: TMouseEvent;
    procedure ControlSetOnKeyUp(const Value: TKeyEvent);
    procedure ControlSetOnMouseDown(const Value: TMouseEvent);
    procedure ControlSetOnMouseEnter(const Value: TNotifyEvent);
    procedure ControlSetOnMouseLeave(const Value: TNotifyEvent);
    procedure ControlSetOnMouseMove(const Value: TMouseMoveEvent);
    procedure ControlSetOnMouseUp(const Value: TMouseEvent);
  end;

  TJvDynControlVCLCheckListBox = class(TCheckListBox, IUnknown,
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
    procedure ControlSetAnchors(Value: TAnchors);

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
    procedure ControlSetState(Index: Integer; Value: TCheckBoxState);
    function ControlGetChecked(Index: Integer): Boolean;
    function ControlGetItemEnabled(Index: Integer): Boolean;
    function ControlGetState(Index: Integer): TCheckBoxState;
    procedure ControlSetHeader(Index: Integer; Value: Boolean);
    function ControlGetHeader(Index: Integer): Boolean;
  end;

  TJvDynControlVCLComboBox = class(TComboBox, IUnknown,
    IJvDynControl, IJvDynControlData, IJvDynControlItems, IJvDynControlComboBox)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlVCLGroupBox = class(TGroupBox, IUnknown,
    IJvDynControl, IJvDynControlCaption)
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
  end;

  TJvDynControlVCLPanel = class(TPanel, IUnknown,
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

  TJvDynControlVCLImage = class(TImage, IUnknown,
    IJvDynControl, IJvDynControlImage)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

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

  TJvDynControlVCLScrollBox = class(TScrollbox, IUnknown,
    IJvDynControl, IJvDynControlCaption)
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
  end;

  TJvDynControlVCLLabel = class(TLabel, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlLabel, IJvDynControlAlign,
    IJvDynControlAutoSize, IJvDynControlColor,
    IJvDynControlAlignment, IJvDynControlFont)
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

  TJvDynControlVCLStaticText = class(TStaticText, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlAlign,
    IJvDynControlAutoSize, IJvDynControlColor,
    IJvDynControlAlignment, IJvDynControlFont)
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

  TJvDynControlVCLButton = class(TBitBtn, IUnknown,
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
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetGlyph(Value: TBitmap);
    procedure ControlSetNumGlyphs(Value: Integer);
    procedure ControlSetLayout(Value: TButtonLayout);
    procedure ControlSetDefault(Value: Boolean);
    procedure ControlSetCancel(Value: Boolean);

    // IJvDynControlAction
    procedure ControlSetAction(Value: TCustomAction);
  end;

  TJvDynControlVCLRadioButton = class(TRadioButton, IUnknown,
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
    procedure ControlSetAnchors(Value: TAnchors);

    // IJvDynControlData
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;
  end;

  TJvDynControlVCLTreeView = class(TTreeView, IUnknown,
    IJvDynControl, IJvDynControlTreeView, IJvDynControlReadOnly, IJvDynControlDblClick,
    IJvDynControlMouse)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

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
    procedure ControlSetOnChange(Value: TTVChangedEvent);
    procedure ControlSetOnChanging(Value: TTVChangingEvent);
    procedure ControlSetSortType(Value: TSortType);
    procedure ControlSortItems;

    //IJvDynControlDblClick = interface
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

  TJvDynControlVCLProgressBar = class(TProgressBar, IUnknown, IJvDynControl,
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

  TJvDynControlVCLTabControl = class(TTabControl, IUnknown,
    IJvDynControl, IJvDynControlTabControl)
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

  TJvDynControlVCLPageControl = class(TPageControl, IUnknown,
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

  TJvDynControlVCLColorComboBox = class(TColorBox, IUnknown, IJvDynControl,
      IJvDynControlColorComboBoxControl)
  public
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnChange(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetValue(Value: Variant);
    function ControlGetValue: Variant;

    //IJvDynControlColorComboBoxControl
    function ControlGetColorName(AColor: TColor): string;
    function ControlGetSelectedColor: TColor;
    procedure ControlSetSelectedColor(const Value: TColor);
    function GetControlDefaultColor: TColor; stdcall;
    procedure SetControlDefaultColor(const Value: TColor); stdcall;
  end;

  TJvDynControlVCLStringGrid = class(TStringGrid, IUnknown, IJvDynControl, IJvDynControlStringGrid)
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



function DynControlEngineVCL: TJvDynControlEngine;
procedure SetDynControlEngineVCLDefault;

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
  SysUtils,
  JvDynControlEngineTools, JvJCLUtils;

var
  IntDynControlEngineVCL: TJvDynControlEngine = nil;

//=== { TJvDynControlVCLMaskEdit } ===========================================

procedure TJvDynControlVCLMaskEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLMaskEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlVCLMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetPasswordChar(Value: Char);
begin
  PasswordChar := Value;
end;

procedure TJvDynControlVCLMaskEdit.ControlSetEditMask(const Value: string);
begin
  EditMask := Value;
end;

//=== { TJvDynControlVCLButtonEdit } =========================================

constructor TJvDynControlVCLButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TJvDynControlVCLButtonEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlVCLButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  FEditControl.OnClick := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := VarToStr(Value);
end;

function TJvDynControlVCLButtonEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;



procedure TJvDynControlVCLButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  FEditControl.PasswordChar := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetEditMask(const Value: string);
begin
  FEditControl.EditMask := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  FButton.Caption := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  FButton.Glyph.Assign(Value);
end;

procedure TJvDynControlVCLButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
  FButton.NumGlyphs := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
  FButton.Layout := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetDefault(Value: Boolean);
begin
  FButton.Default := Value;
end;

procedure TJvDynControlVCLButtonEdit.ControlSetCancel(Value: Boolean);
begin
  FButton.Cancel := Value;
end;

//=== { TJvDynControlVCLFileNameEdit } =======================================

constructor TJvDynControlVCLFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  FDialogOptions := [ofHideReadOnly, ofEnableSizing];
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDialogKind := jdkOpen;
end;

destructor TJvDynControlVCLFileNameEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLFileNameEdit.DefaultOnButtonClick(Sender: TObject);
begin
  case FDialogKind of
    jdkOpen:
      with TOpenDialog.Create(Self) do
      try
        Options := FDialogOptions;
        Title := FDialogTitle;
        Filter := FFilter;
        FilterIndex := FFilterIndex;
        InitialDir := FInitialDir;
        DefaultExt := FDefaultExt;
        FileName := ControlGetValue;
        if Execute then
          ControlSetValue(FileName);
      finally
        Free;
      end;
    jdkOpenPicture:
      with TOpenPictureDialog.Create(Self) do
      try
        Options := FDialogOptions;
        Title := FDialogTitle;
        Filter := FFilter;
        FilterIndex := FFilterIndex;
        InitialDir := FInitialDir;
        DefaultExt := FDefaultExt;
        FileName := ControlGetValue;
        if Execute then
          ControlSetValue(FileName);
      finally
        Free;
      end;
    jdkSave:
      with TSaveDialog.Create(Self) do
      try
        Options := FDialogOptions;
        Title := FDialogTitle;
        Filter := FFilter;
        FilterIndex := FFilterIndex;
        InitialDir := FInitialDir;
        DefaultExt := FDefaultExt;
        FileName := ControlGetValue;
        if Execute then
          ControlSetValue(FileName);
      finally
        Free;
      end;
    jdkSavePicture:
      with TSavePictureDialog.Create(Self) do
      try
        Options := FDialogOptions;
        Title := FDialogTitle;
        Filter := FFilter;
        FilterIndex := FFilterIndex;
        InitialDir := FInitialDir;
        DefaultExt := FDefaultExt;
        FileName := ControlGetValue;
        if Execute then
          ControlSetValue(FileName);
      finally
        Free;
      end;
  end;
  if FEditControl.CanFocus then
    FEditControl.SetFocus;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDefaultProperties;
begin
  Caption := ' ';
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := VarToStr(Value);
end;

function TJvDynControlVCLFileNameEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

// IJvDynControlFileName

procedure TJvDynControlVCLFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlVCLFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

//=== { TJvDynControlVCLDirectoryEdit } ======================================

constructor TJvDynControlVCLDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditControl := TMaskEdit.Create(AOwner);
  FEditControl.Parent := Self;
  FButton := TBitBtn.Create(AOwner);
  FButton.Parent := Self;
  FButton.Align := alRight;
  FButton.OnClick := DefaultOnButtonClick;
  FButton.Caption := '...';
  Height := FEditControl.Height;
  FButton.Width := Height;
  FEditControl.Align := alClient;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TJvDynControlVCLDirectoryEdit.Destroy;
begin
  FreeAndNil(FEditControl);
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TJvDynControlVCLDirectoryEdit.DefaultOnButtonClick(Sender: TObject);
var
  Opt: TSelectDirOpts;
  Dir: string;
begin
  Dir := ControlGetValue;
  if Dir = '' then
    if FInitialDir <> '' then
      Dir := FInitialDir
    else
      Dir := PathDelim;
  if not DirectoryExists(Dir) then
    Dir := PathDelim;
  if SelectDirectory(Dir, Opt, HelpContext) then
    ControlSetValue(Dir);
  if FEditControl.CanFocus then
    FEditControl.SetFocus;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  FEditControl.ReadOnly := Value;
  FButton.Enabled := not Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FEditControl.OnEnter := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FEditControl.OnExit := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FEditControl.OnChange := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetValue(Value: Variant);
begin
  FEditControl.Text := VarToStr(Value);
end;

function TJvDynControlVCLDirectoryEdit.ControlGetValue: Variant;
begin
  Result := FEditControl.Text;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;


procedure TJvDynControlVCLDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;



//=== { TJvDynControlVCLDateTimeEdit } =======================================

constructor TJvDynControlVCLDateTimeEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  FDatePicker := TDateTimePicker.Create(Self);
  FDatePicker.Parent := Self;
  FDatePicker.Align := alLeft;
  FDatePicker.Top := 0;
  FDatePicker.Left := 0;
  FTimePicker := TDateTimePicker.Create(Self);
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

destructor TJvDynControlVCLDateTimeEdit.Destroy;
begin
  FreeAndNil(FDatePicker);
  FreeAndNil(FTimePicker);
  inherited Destroy;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlResize(Sender: TObject);
begin
  FDatePicker.Height := Round(Height / 2);
  FTimePicker.Height := Height;
  FDatePicker.Width := Round(Width / 2);
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetDefaultProperties;
begin
  Self.Caption := ' ';
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  FDatePicker.OnEnter := Value;
  FTimePicker.OnEnter := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  FDatePicker.OnExit := Value;
  FTimePicker.OnExit := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  FDatePicker.OnChange := Value;
  FTimePicker.OnChange := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetValue(Value: Variant);
begin
  FDatePicker.Date := Value;
  FTimePicker.Time := Value;
end;

function TJvDynControlVCLDateTimeEdit.ControlGetValue: Variant;
begin
  { TODO -oAHUser : Delphi.NET workaround }
  Result := Trunc(FDatePicker.Date) + (Trunc(FTimePicker.Time) - FTimePicker.Time);
end;

// IJvDynControlDate

procedure TJvDynControlVCLDateTimeEdit.ControlSetMinDate(Value: TDateTime);
begin
  FDatePicker.MinDate := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetMaxDate(Value: TDateTime);
begin
  FDatePicker.MaxDate := Value;
end;

procedure TJvDynControlVCLDateTimeEdit.ControlSetFormat(const Value: string);
begin
  FDatePicker.Format := Value;
  FTimePicker.Format := Value;
end;

//=== { TJvDynControlVCLDateEdit } ===========================================

procedure TJvDynControlVCLDateEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  DateMode := dmComboBox;
  Kind := dtkDate;
end;

procedure TJvDynControlVCLDateEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlVCLDateEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetValue(Value: Variant);
begin
  Date := Value;
end;

function TJvDynControlVCLDateEdit.ControlGetValue: Variant;
begin
  Result := Date;
end;

// IJvDynControlDate

procedure TJvDynControlVCLDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  MinDate := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  MaxDate := Value;
end;

procedure TJvDynControlVCLDateEdit.ControlSetFormat(const Value: string);
begin
  Format := Value;
end;

//=== { TJvDynControlVCLTimeEdit } ===========================================

procedure TJvDynControlVCLTimeEdit.ControlSetDefaultProperties;
begin
  DateFormat := dfShort;
  Kind := dtkTime;
  DateMode := dmUpDown;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetValue(Value: Variant);
begin
  Time := Value;
end;

function TJvDynControlVCLTimeEdit.ControlGetValue: Variant;
begin
  Result := Time;
end;

procedure TJvDynControlVCLTimeEdit.ControlSetFormat(const Value: string);
begin
  Format := Value;
end;



//=== { TJvDynControlVCLCheckBox } ===========================================

function TJvDynControlVCLCheckBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlVCLCheckBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlVCLCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlVCLCheckBox.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlVCLCheckBox.ControlGetFont: TFont;
begin
  Result := Font;
end;

//IJvDynControlCheckBox

procedure TJvDynControlVCLCheckBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlVCLCheckBox.ControlSetState(Value: TCheckBoxState);
begin
  State := Value;
end;

function TJvDynControlVCLCheckBox.ControlGetState: TCheckBoxState;
begin
  Result := State;
end;

function TJvDynControlVCLMemo.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlVCLMemo } ===============================================

procedure TJvDynControlVCLMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLMemo.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlVCLMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlVCLMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlVCLMemo.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TJvDynControlVCLMemo.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlVCLRichEdit.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlVCLRichEdit } ===========================================

procedure TJvDynControlVCLRichEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLRichEdit.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlVCLRichEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLRichEdit.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLRichEdit.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlVCLRichEdit.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlVCLRichEdit.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TJvDynControlVCLRichEdit.ControlSetWantTabs(Value: Boolean);
begin
  WantTabs := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetWantReturns(Value: Boolean);
begin
  WantReturns := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLRichEdit.ControlSetScrollBars(Value: TScrollStyle);
begin
  ScrollBars := Value;
end;

//=== { TJvDynControlVCLRadioGroup } ===========================================

procedure TJvDynControlVCLRadioGroup.ControlSetDefaultProperties;
begin
end;

function TJvDynControlVCLRadioGroup.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(VarToStr(Value));
end;

function TJvDynControlVCLRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlVCLRadioGroup.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLRadioGroup.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLRadioGroup.ControlSetColumns(Value: Integer);
begin
  Columns := Value;
end;

function TJvDynControlVCLListBox.ControlGetItemIndex: Integer;
begin
  Result := ItemIndex;
end;

//=== { TJvDynControlVCLListBox } ============================================

procedure TJvDynControlVCLListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  //  OnChange := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(VarToStr(Value));
end;

function TJvDynControlVCLListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLListBox.ControlSetItemIndex(const Value: Integer);
begin
  ItemIndex := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

function TJvDynControlVCLListBox.ControlGetOnKeyDown: TKeyEvent;
begin
  Result := OnKeyDown;
end;

function TJvDynControlVCLListBox.ControlGetOnKeyPress: TKeyPressEvent;
begin
  Result := OnKeyPress;
end;

function TJvDynControlVCLListBox.ControlGetOnKeyUp: TKeyEvent;
begin
  Result := OnKeyUp;
end;

function TJvDynControlVCLListBox.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlVCLListBox.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlVCLListBox.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlVCLListBox.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlVCLListBox.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlVCLListBox.ControlSetOnKeyDown(const Value: TKeyEvent);
begin
  OnKeyDown := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnKeyPress(const Value: TKeyPressEvent);
begin
  OnKeyPress := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnKeyUp(const Value: TKeyEvent);
begin
  OnKeyUp := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlVCLListBox.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlVCLListBox.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlVCLListBox.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

//=== { TJvDynControlVCLCheckListBox } =======================================

procedure TJvDynControlVCLCheckListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLCheckListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  //  OnChange := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(VarToStr(Value));
end;

function TJvDynControlVCLCheckListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLCheckListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

//IJvDynControlCheckListBox = interface

procedure TJvDynControlVCLCheckListBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetChecked(Index: Integer; Value: Boolean);
begin
  Checked[Index] := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetItemEnabled(Index: Integer; Value: Boolean);
begin
  ItemEnabled[Index] := Value;
end;

procedure TJvDynControlVCLCheckListBox.ControlSetHeader(Index: Integer; Value: Boolean);
begin
  Header[Index] := Value;
end;

function TJvDynControlVCLCheckListBox.ControlGetHeader(Index: Integer): Boolean;
begin
  Result := Header[Index];
end;

procedure TJvDynControlVCLCheckListBox.ControlSetState(Index: Integer; Value: TCheckBoxState);
begin
  State[Index] := Value;
end;

function TJvDynControlVCLCheckListBox.ControlGetChecked(Index: Integer): Boolean;
begin
  Result := Checked[Index];
end;

function TJvDynControlVCLCheckListBox.ControlGetItemEnabled(Index: Integer): Boolean;
begin
  Result := ItemEnabled[Index];
end;

function TJvDynControlVCLCheckListBox.ControlGetState(Index: Integer): TCheckBoxState;
begin
  Result := State[Index];
end;

//=== { TJvDynControlVCLComboBox } ===========================================

procedure TJvDynControlVCLComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetValue(Value: Variant);
begin
  if (Style = csDropDownList) then
    if VarIsInt(Value) then
      ItemIndex := VarToInt(Value)
    else
      ItemIndex := Items.IndexOf(VarToStr(Value))
  else
    Text := VarToStr(Value);
end;

function TJvDynControlVCLComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLComboBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlVCLComboBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLComboBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlVCLComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
const
  Styles: array[Boolean] of TComboBoxStyle =
    (csDropDownList, csDropDown);
begin
  Style := Styles[Value];
end;

function TJvDynControlVCLGroupBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLGroupBox } ===========================================

procedure TJvDynControlVCLGroupBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLGroupBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLGroupBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLGroupBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

function TJvDynControlVCLPanel.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLPanel } ==============================================

procedure TJvDynControlVCLPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

procedure TJvDynControlVCLPanel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLPanel.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel;
  ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
begin
  BorderWidth := ABorderWidth;
  BorderStyle := ABorderStyle;
  BevelInner := ABevelInner;
  BevelOuter := ABevelOuter;
  BevelWidth := ABevelWidth;
end;

procedure TJvDynControlVCLPanel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBevelInner(Value: TBevelCut);
begin
  BevelInner := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBevelKind(Value: TBevelKind);
begin
  BevelKind := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBevelOuter(Value: TBevelCut);
begin
  BevelOuter := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBorderStyle(Value: TBorderStyle);
begin
  BorderStyle := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetBorderWidth(Value: Integer);
begin
  BorderWidth := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlVCLPanel.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

//=== { TJvDynControlVCLImage } ==============================================

procedure TJvDynControlVCLImage.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLImage.ControlSetTabOrder(Value: Integer);
begin
  //  TabOrder := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
  //  OnEnter := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnExit(Value: TNotifyEvent);
begin
  //  OnExit := Value;
end;

procedure TJvDynControlVCLImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLImage.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
  IncrementalDisplay := Value;
end;

procedure TJvDynControlVCLImage.ControlSetCenter(Value: Boolean);
begin
  Center := Value;
end;



procedure TJvDynControlVCLImage.ControlSetProportional(Value: Boolean);
begin
  Proportional := Value;
end;


procedure TJvDynControlVCLImage.ControlSetStretch(Value: Boolean);
begin
  Stretch := Value;
end;

procedure TJvDynControlVCLImage.ControlSetTransparent(Value: Boolean);
begin
  Transparent := Value;
end;

procedure TJvDynControlVCLImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlVCLImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlVCLImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

function TJvDynControlVCLScrollBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLScrollBox } ==========================================

procedure TJvDynControlVCLScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLScrollBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLScrollBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLScrollBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

function TJvDynControlVCLLabel.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLLabel } ==============================================

procedure TJvDynControlVCLLabel.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLLabel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLLabel.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetWordWrap(Value: Boolean);
begin
  WordWrap := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TJvDynControlVCLLabel.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlVCLLabel.ControlGetFont: TFont;
begin
  Result := Font;
end;

function TJvDynControlVCLStaticText.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLStaticText } =========================================


procedure TJvDynControlVCLStaticText.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStaticText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetColor(Value: TColor);
begin
  Color := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetAlignment(Value: TAlignment);
begin
  Alignment := Value;
end;

procedure TJvDynControlVCLStaticText.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlVCLStaticText.ControlGetFont: TFont;
begin
  Result := Font;
end;

function TJvDynControlVCLButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLButton } =============================================

procedure TJvDynControlVCLButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLButton.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLButton.ControlSetGlyph(Value: TBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlVCLButton.ControlSetNumGlyphs(Value: Integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlVCLButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;

procedure TJvDynControlVCLButton.ControlSetDefault(Value: Boolean);
begin
  Default := Value;
end;

procedure TJvDynControlVCLButton.ControlSetCancel(Value: Boolean);
begin
  Cancel := Value;
end;

procedure TJvDynControlVCLButton.ControlSetAction(Value: TCustomAction);
begin
  Action := Value;
end;

function TJvDynControlVCLRadioButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

//=== { TJvDynControlVCLRadioButton } ========================================

procedure TJvDynControlVCLRadioButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLRadioButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

// IJvDynControlData

procedure TJvDynControlVCLRadioButton.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLRadioButton.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlVCLRadioButton.ControlGetValue: Variant;
begin
  Result := Checked;
end;

//=== { TJvDynControlVCLTreeView } ===========================================

procedure TJvDynControlVCLTreeView.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLTreeView.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetAutoExpand(Value: Boolean);
begin
  AutoExpand := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetShowLines(Value: Boolean);
begin
  ShowLines := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetShowRoot(Value: Boolean);
begin
  ShowRoot := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetToolTips(Value: Boolean);
begin
  ToolTips := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetItems(Value: TTreeNodes);
begin
  Items.Assign(Value);
end;

function TJvDynControlVCLTreeView.ControlGetItems: TTreeNodes;
begin
  Result := Items;
end;

function TJvDynControlVCLTreeView.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlVCLTreeView.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlVCLTreeView.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlVCLTreeView.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlVCLTreeView.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlVCLTreeView.ControlSetImages(Value: TCustomImageList);
begin
  Images.Assign(Value);
end;

procedure TJvDynControlVCLTreeView.ControlSetStateImages(Value: TCustomImageList);
begin
  StateImages.Assign(Value);
end;

function TJvDynControlVCLTreeView.ControlGetSelected: TTreeNode;
begin
  Result := Selected;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnChange(Value: TTVChangedEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnChanging(Value:
    TTVChangingEvent);
begin
  OnChanging := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetSortType(Value: TSortType);
begin
  SortType := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlVCLTreeView.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlVCLTreeView.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

procedure TJvDynControlVCLTreeView.ControlSortItems;
begin
  AlphaSort;
end;

procedure TJvDynControlVCLTreeView.ControlSetSelected(const Value: TTreeNode);
begin
  Selected := Value;
end;

function TJvDynControlVCLProgressBar.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlVCLProgressBar.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

//=== { TJvDynControlVCLProgressbar } ========================================

procedure TJvDynControlVCLProgressbar.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLProgressbar.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLProgressBar.ControlSetMarquee(Value: Boolean);
begin
  {$IFDEF DELPHI2009_UP}
  if Value then
    Style := pbstMarquee
  else
    Style := pbstNormal;
  {$ENDIF DELPHI2009_UP}
end;

procedure TJvDynControlVCLProgressbar.ControlSetMax(Value: Integer);
begin
  Max := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetMin(Value: Integer);
begin
  Min := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetOrientation(Value: TProgressBarOrientation);
begin
  Orientation := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetSmooth(Value: Boolean);
begin
  Smooth := Value;
end;

procedure TJvDynControlVCLProgressbar.ControlSetStep(Value: Integer);
begin
  Step := Value;
end;

//=== { TJvDynControlVCLTabControl } =========================================

procedure TJvDynControlVCLTabControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLTabControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLTabControl.ControlCreateTab(const AName: string);
begin
  Tabs.Add(AName);
end;

procedure TJvDynControlVCLTabControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlVCLTabControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlVCLTabControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

function TJvDynControlVCLTabControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlVCLTabControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlVCLTabControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

//=== { TJvDynControlVCLPageControl } ========================================

procedure TJvDynControlVCLPageControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLPageControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLPageControl.ControlCreateTab(const AName: string);
var
  TabSheet: TTabSheet;
begin
  TabSheet := TTabSheet.Create(Self);
  TabSheet.Caption := AName;
  TabSheet.PageControl := Self;
  TabSheet.Parent := Self;
end;

procedure TJvDynControlVCLPageControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlVCLPageControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlVCLPageControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

function TJvDynControlVCLPageControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlVCLPageControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlVCLPageControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

function TJvDynControlVCLPageControl.ControlGetPage(const PageName: string): TWinControl;
var
  I: Integer;
begin
  I := Tabs.IndexOf(PageName);
  if (I >= 0) and (I < PageCount) then
    Result := TWinControl(Pages[I])
  else
    Result := nil;
end;

//=== { TJvDynControlVCLColorComboBox } ===========================================

Type TAccessCustomColorBox = class(TCustomColorBox);

function TJvDynControlVCLColorComboBox.ControlGetColorName(AColor: TColor):
    string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to TAccessCustomColorBox(Self).ItemCount - 1 do
    if Colors[i] = AColor then
      Result := ColorNames[i];
end;

function TJvDynControlVCLColorComboBox.ControlGetSelectedColor: TColor;
begin
  Result := Selected;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLColorComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLColorComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlVCLColorComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlVCLColorComboBox.ControlSetSelectedColor(const Value:
    TColor);
begin
  Selected := Value;
end;

function TJvDynControlVCLColorComboBox.GetControlDefaultColor: TColor;
begin
  Result := DefaultColorColor;
end;

procedure TJvDynControlVCLColorComboBox.SetControlDefaultColor(const Value:
    TColor);
begin
  DefaultColorColor := Value;
end;


procedure TJvDynControlVCLStringGrid.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlVCLStringGrid.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlVCLStringGrid.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlVCLStringGrid.ControlSetOnChange(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlVCLStringGrid.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlVCLStringGrid.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlVCLStringGrid.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlVCLStringGrid.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

function TJvDynControlVCLStringGrid.GetControlCells(ACol, ARow: Integer): string;
begin
  Result := Cells[ACol, ARow];
end;

function TJvDynControlVCLStringGrid.GetControlCol: Integer;
begin
  Result := Col;
end;

function TJvDynControlVCLStringGrid.GetControlColCount: Integer;
begin
  Result := ColCount;
end;

function TJvDynControlVCLStringGrid.GetControlColWidths(Index: Integer): Integer;
begin
  Result := ColWidths[Index];
end;

function TJvDynControlVCLStringGrid.GetControlFixedCols: Integer;
begin
  Result := FixedCols;
end;

function TJvDynControlVCLStringGrid.GetControlFixedRows: Integer;
begin
  Result := FixedRows;
end;

function TJvDynControlVCLStringGrid.GetControlObjects(ACol, ARow: Integer): TObject;
begin
  Result := Objects[ACol, ARow];
end;

function TJvDynControlVCLStringGrid.GetControlOptions: TGridOptions;
begin
  Result := Options;
end;

function TJvDynControlVCLStringGrid.GetControlRow: Integer;
begin
  Result := Row;
end;

function TJvDynControlVCLStringGrid.GetControlRowCount: Integer;
begin
  Result := RowCount;
end;

function TJvDynControlVCLStringGrid.GetControlRowHeights(Index: Integer): Integer;
begin
  Result := RowHeights[Index];
end;

procedure TJvDynControlVCLStringGrid.SetControlCells(ACol, ARow: Integer; const Value: string);
begin
  Cells[ACol, ARow] := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlCol(const Value: Integer);
begin
  Col := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlColCount(const Value: Integer);
begin
  ColCount := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlColWidths(Index: Integer; const Value: Integer);
begin
  ColWidths[Index] := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlFixedCols(const Value: Integer);
begin
  FixedCols := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlFixedRows(const Value: Integer);
begin
  FixedRows := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlObjects(ACol, ARow: Integer; Value: TObject);
begin
  Objects[ACol, ARow] := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlOptions(const Value: TGridOptions);
begin
  Options := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlRow(const Value: Integer);
begin
  Row := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlRowCount(const Value: Integer);
begin
  RowCount := Value;
end;

procedure TJvDynControlVCLStringGrid.SetControlRowHeights(Index: Integer; const Value: Integer);
begin
  RowHeights [Index] := Value;
end;

//=== { TJvDynControlEngineVCL } =============================================

procedure SetDynControlEngineVCLDefault;
begin
  SetDefaultDynControlEngine(IntDynControlEngineVCL);
end;

function DynControlEngineVCL: TJvDynControlEngine;
begin
  Result := IntDynControlEngineVCL;
end;

procedure TJvDynControlEngineVCL.RegisterControls;
begin
  RegisterControlType(jctLabel, TJvDynControlVCLLabel);
  RegisterControlType(jctStaticText, TJvDynControlVCLStaticText);
  RegisterControlType(jctButton, TJvDynControlVCLButton);
  RegisterControlType(jctRadioButton, TJvDynControlVCLRadioButton);
  RegisterControlType(jctScrollBox, TJvDynControlVCLScrollBox);
  RegisterControlType(jctGroupBox, TJvDynControlVCLGroupBox);
  RegisterControlType(jctPanel, TJvDynControlVCLPanel);
  RegisterControlType(jctImage, TJvDynControlVCLImage);
  RegisterControlType(jctCheckBox, TJvDynControlVCLCheckBox);
  RegisterControlType(jctComboBox, TJvDynControlVCLComboBox);
  RegisterControlType(jctListBox, TJvDynControlVCLListBox);
  RegisterControlType(jctCheckListBox, TJvDynControlVCLCheckListBox);
  RegisterControlType(jctRadioGroup, TJvDynControlVCLRadioGroup);
  RegisterControlType(jctDateTimeEdit, TJvDynControlVCLDateTimeEdit);
  RegisterControlType(jctTimeEdit, TJvDynControlVCLTimeEdit);
  RegisterControlType(jctDateEdit, TJvDynControlVCLDateEdit);
  RegisterControlType(jctEdit, TJvDynControlVCLMaskEdit);
  //  RegisterControlType(jctCalculateEdit, TJvDynControlVCLMaskEdit);
  //  RegisterControlType(jctSpinEdit, TJvDynControlVCLMaskEdit);
  RegisterControlType(jctDirectoryEdit, TJvDynControlVCLDirectoryEdit);
  RegisterControlType(jctFileNameEdit, TJvDynControlVCLFileNameEdit);
  RegisterControlType(jctMemo, TJvDynControlVCLMemo);
  RegisterControlType(jctRichEdit, TJvDynControlVCLRichEdit);
  RegisterControlType(jctButtonEdit, TJvDynControlVCLButtonEdit);
  RegisterControlType(jctTreeView, TJvDynControlVCLTreeView);
  RegisterControlType(jctProgressbar, TJvDynControlVCLProgressbar);
  RegisterControlType(jctTabControl, TJvDynControlVCLTabControl);
  RegisterControlType(jctPageControl, TJvDynControlVCLPageControl);
  {$IFDEF DELPHI7_UP}
  RegisterControlType(jctColorComboBox, TJvDynControlVCLColorComboBox);
  {$ENDIF}
  RegisterControlType(jctStringGrid, TJvDynControlVCLStringGrid);
end;



initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  IntDynControlEngineVCL := TJvDynControlEngineVCL.Create;
  SetDynControlEngineVCLDefault;

finalization
  FreeAndNil(IntDynControlEngineVCL);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
