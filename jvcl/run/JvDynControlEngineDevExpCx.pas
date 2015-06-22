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
// $Id: jvcl/run/JvDynControlEngineDevExpCx.pas jfudickar date $

unit JvDynControlEngineDevExpCx;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

{$IFNDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning, JvDynControlEngineIntf, Graphics, ComCtrls, Classes,
  ExtCtrls, Grids;
{$ENDIF UNITVERSIONING}

{$ELSE}
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  Classes, Controls, StdCtrls, ExtCtrls, ComCtrls, Mask, Forms, Graphics,
  Buttons, Dialogs, FileCtrl, ActnList, ImgList,
  cxLookAndFeels, cxMaskEdit, cxLabel, cxButtons, cxListBox, cxDropDownEdit,
  cxButtonEdit, cxCalendar, cxCheckBox, cxMemo, cxRadioGroup, cxImage, cxTreeView,
  cxEdit, cxCalc, cxSpinEdit, cxTimeEdit, cxCheckListBox, cxGroupBox, cxRichEdit,
  cxProgressBar, cxPC, cxColorComboBox, cxGraphics, cxCheckComboBox, dxTaskbarProgress,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  cxOi, cxVGrid, cxVGridViewInfo,
  {$ENDIF}
  JvDynControlEngine, JvDynControlEngineIntf;

type

  TCxDynControlWrapper = class(TPersistent)
  private
    FLookAndFeel: TcxLookAndFeel;
    FStyleController: TcxEditStyleController;
  protected
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetStyleController(Value: TcxEditStyleController);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property StyleController: TcxEditStyleController read FStyleController write SetStyleController;
  end;

  IJvDynControlDevExpCx = interface
    ['{13F812FE-9F75-4529-8452-45F2D9DE5A91}']
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxMaskEdit = class(TcxMaskEdit, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlDevExpCx, IJvDynControlReadOnly, IJvDynControlEdit)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlEdit
    procedure ControlSetPasswordChar(Value: Char);
    procedure ControlSetEditMask(const Value: string);
  end;

  TJvDynControlCxButtonEdit = class(TcxButtonEdit, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlDevExpCx, IJvDynControlReadOnly, IJvDynControlEdit, IJvDynControlButtonEdit,
    IJvDynControlButton)
  private
    FIntOnButtonClick: TNotifyEvent;
  protected
    procedure IntOnButtonClick(Sender: TObject; AButtonIndex: Integer);
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

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

  TJvDynControlCxCalcEdit = class(TcxCalcEdit, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlDevExpCx, IJvDynControlReadOnly)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxSpinEdit = class(TcxSpinEdit, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlDevExpCx, IJvDynControlSpin, IJvDynControlReadOnly)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    // IJvDynControlSpin
    procedure ControlSetIncrement(Value: Integer);
    procedure ControlSetMinValue(Value: double);
    procedure ControlSetMaxValue(Value: double);
    procedure ControlSetUseForInteger(Value: Boolean);
  end;

  TJvDynControlCxFileNameEdit = class(TcxButtonEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlFileName, IJvDynControlReadOnly)
  private
    FInitialDir: string;
    FFilterIndex: Integer;
    FFilter: string;
    FDialogOptions: TOpenOptions;
    FDialogKind: TJvDynControlFileNameDialogKind;
    FDialogTitle: string;
    FDefaultExt: string;
  public
    procedure DefaultOnButtonClick(Sender: TObject; AButtonIndex: Integer);

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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    // IJvDynControlFileName
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDefaultExt(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TOpenOptions);
    procedure ControlSetFilter(const Value: string);
    procedure ControlSetFilterIndex(Value: Integer);
    procedure ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
  end;

  TJvDynControlCxDirectoryEdit = class(TcxButtonEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlDirectory, IJvDynControlReadOnly)
  private
    FInitialDir: string;
    FDialogOptions: TSelectDirOpts;
    FDialogTitle: string;
  public
    procedure DefaultOnButtonClick(Sender: TObject; AButtonIndex: Integer);

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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    // IJvDynControlDirectory
    procedure ControlSetInitialDir(const Value: string);
    procedure ControlSetDialogTitle(const Value: string);
    procedure ControlSetDialogOptions(Value: TSelectDirOpts);
  end;

  TJvDynControlCxDateTimeEdit = class(TcxDateEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlDate, IJvDynControlReadOnly)
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

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxDateEdit = class(TcxDateEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlDate, IJvDynControlReadOnly)
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

    // IJvDynControlDate
    procedure ControlSetMinDate(Value: TDateTime);
    procedure ControlSetMaxDate(Value: TDateTime);
    procedure ControlSetFormat(const Value: string);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxTimeEdit = class(TcxTimeEdit, IUnknown, IJvDynControl,
    IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlTime, IJvDynControlReadOnly)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    procedure ControlSetFormat(const Value: string);
  end;

  TJvDynControlCxCheckBox = class(TcxCheckBox, IUnknown, IJvDynControl,
    IJvDynControlCaption, IJvDynControlData, IJvDynControlDevExpCx, IJvDynControlReadOnly,
    IJvDynControlCheckBox, IJvDynControlFont)
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
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlCheckBox
    procedure ControlSetAllowGrayed(Value: Boolean);
    procedure ControlSetState(Value: TCheckBoxState);
    function ControlGetState: TCheckBoxState;

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlFont
    procedure ControlSetFont(Value: TFont);
    function ControlGetFont: TFont;
  end;

  TJvDynControlCxMemo = class(TcxMemo, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlMemo, IJvDynControlDevExpCx, IJvDynControlReadOnly,
    IJvDynControlAlignment, IJvDynControlFont)
  public
    //IJvDynControlFont
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
    //IJvDynControlAlignment
    procedure ControlSetAlignment(Value: TAlignment);
    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlCxRichEdit = class(TcxRichEdit, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlMemo, IJvDynControlDevExpCx, IJvDynControlReadOnly, IJvDynControlFont)
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetWantTabs(Value: Boolean);
    procedure ControlSetWantReturns(Value: Boolean);
    procedure ControlSetWordWrap(Value: Boolean);
    procedure ControlSetScrollBars(Value: TScrollStyle);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlFont
    function ControlGetFont: TFont;
    procedure ControlSetFont(Value: TFont);
  end;

  TJvDynControlCxRadioGroup = class(TcxRadioGroup, IUnknown, IJvDynControl,
    IJvDynControlCaption, IJvDynControlData, IJvDynControlItems, IJvDynControlDevExpCx,
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    procedure ControlSetColumns(Value: Integer);
  end;

  TJvDynControlCxListBox = class(TcxListBox, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlItemIndex, IJvDynControlDblClick, IJvDynControlDevExpCx, IJvDynControlReadOnly,
    IJvDynControlKey, IJvDynControlMouse)
  public
    function ControlGetItemIndex: Integer;
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
    procedure ControlSetItemIndex(const Value: Integer);

    function ControlGetOnKeyDown: TKeyEvent;
    function ControlGetOnKeyPress: TKeyPressEvent;
    function ControlGetOnKeyUp: TKeyEvent;
    procedure ControlSetOnKeyDown(const Value: TKeyEvent);
    procedure ControlSetOnKeyPress(const Value: TKeyPressEvent);
    procedure ControlSetOnKeyUp(const Value: TKeyEvent);

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

  TJvDynControlCxCheckListBox = class(TcxCheckListBox, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlDblClick, IJvDynControlDevExpCx, IJvDynControlReadOnly,
    IJvDynControlCheckListBox)
  private
    FIntItems: TStrings;
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

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

  TJvDynControlCxComboBox = class(TcxComboBox, IUnknown, IJvDynControl, IJvDynControlData,
    IJvDynControlItems, IJvDynControlDevExpCx, IJvDynControlComboBox, IJvDynControlReadOnly)
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;
    procedure ControlSetAnchors(Value: TAnchors);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    procedure ControlSetNewEntriesAllowed(Value: Boolean);
  end;

  TJvDynControlCxGroupBox = class(TcxGroupBox, IUnknown, IJvDynControl,
    IJvDynControlCaption, IJvDynControlColor)
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

    // IJvDynControlColor
    procedure ControlSetColor(Value: TColor);
    procedure ControlSetParentColor(Value: Boolean);
  end;

  TJvDynControlCxPanel = class(TcxGroupBox, IUnknown, IJvDynControl, IJvDynControlPanel,
    IJvDynControlAlign, IJvDynControlAutoSize, IJvDynControlBevelBorder, IJvDynControlColor,
    IJvDynControlCaption, IJvDynControlAlignment)
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

    procedure ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);

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

  TJvDynControlCxImage = class(TcxImage, IUnknown, IJvDynControl,
    IJvDynControlImage, IJvDynControlDevExpCx)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  // (rom) TScrollBox or TcxScrollBox?
  TJvDynControlCxScrollBox = class(TScrollBox, IJvDynControl, IJvDynControlCaption)
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

  TJvDynControlCxLabel = class(TcxLabel, IUnknown, IJvDynControl, IJvDynControlLabel,
    IJvDynControlCaption, IJvDynControlDevExpCx, IJvDynControlAlign,
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

    procedure ControlSetFocusControl(Value: TWinControl);
    procedure ControlSetWordWrap(Value: Boolean);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

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

  // (rom) Warning! TStaticText and TLabel are very different.
  TJvDynControlCxStaticText = class(TcxLabel, IUnknown, IJvDynControl, IJvDynControlDevExpCx,
    IJvDynControlCaption, IJvDynControlAlign, IJvDynControlAutoSize, IJvDynControlColor,
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

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

  TJvDynControlCxButton = class(TcxButton, IUnknown, IJvDynControl, IJvDynControlButton,
    IJvDynControlCaption, IJvDynControlDevExpCx, IJvDynControlAction)
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

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxRadioButton = class(TCxRadioButton, IUnknown,
    IJvDynControl, IJvDynControlCaption, IJvDynControlData, IJvDynControlDevExpCx)
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

    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxTreeView = class(TcxTreeView, IUnknown,
    IJvDynControl, IJvDynControlTreeView,
    IJvDynControlDevExpCx, IJvDynControlReadOnly, IJvDynControlDblClick,
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
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetOnChange(Value: TTVChangedEvent);
    procedure ControlSetSortType(Value: TSortType);
    procedure ControlSortItems;
    function ControlGetSelected: TTreeNode;
    procedure ControlSetSelected(const Value: TTreeNode);
    procedure ControlSetOnChanging(Value: TTVChangingEvent);

    //IJvDynControlDblClick
    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

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

  TJvDynControlCxProgressBar = class(TcxProgressBar, IUnknown, IJvDynControl,
      IJvDynControlProgressBar, IJvDynControlAlign, IJvDynControlDevExpCx)
  private
    fTaskbarProgress: TdxTaskbarProgress ;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlSetAlign(Value: TAlign);
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetCaption(const Value: string);
    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
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


  TJvDynControlCxTabControl = class(TcxTabControl, IUnknown, IJvDynControl,
      IJvDynControlTabControl, IJvDynControlDevExpCx)
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
    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  TJvDynControlCxPageControl = class(TcxPageControl, IUnknown,
      IJvDynControl, IJvDynControlTabControl, IJvDynControlPageControl, IJvDynControlDevExpCx)
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
    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
  end;

  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  TJvDynControlCxRTTIInspectorControl = class(TcxRTTIInspector, IUnknown,
      IJvDynControl, IJvDynControlRTTIInspectorControl, IJvDynControlDevExpCx)
  private
    fControlOnPropertyChange: TJvDynControlInspectorControlOnPropertyChangeEvent;
    fOnDisplayProperty: TJvDynControlInspectorControlOnDisplayPropertyEvent;
    fOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    OldPropertyName: string;
    procedure InspectorOnFilterProperty(Sender: TObject; const PropertyName:
        string; var Accept: Boolean);
    procedure InspectorOnItemChanged(Sender: TObject; AOldRow: TcxCustomRow;
        AOldCellIndex: Integer);
    procedure ReplaceOnDrawRowHeader(Sender: TObject; ACanvas: TcxCanvas; APainter:
        TcxvgPainter; AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
  protected
    //IJvDynControlRTTIInspectorControl
    function ControlGetOnDisplayProperty:
        TJvDynControlInspectorControlOnDisplayPropertyEvent;
    function ControlGetOnTranslatePropertyName:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
    procedure ControlSetOnDisplayProperty(const Value:
        TJvDynControlInspectorControlOnDisplayPropertyEvent); overload;
    procedure ControlSetOnTranslatePropertyName(const Value:
        TJvDynControlInspectorControlOnTranslatePropertyNameEvent);
    function GetControlDividerWidth: Integer;
    procedure SetControlDividerWidth(const Value: Integer);
  public
    function ControlGetCurrentPropertyName: string;
    procedure ControlSetDefaultProperties;
    procedure ControlSetTabOrder(Value: Integer);

    procedure ControlSetOnEnter(Value: TNotifyEvent);
    procedure ControlSetOnExit(Value: TNotifyEvent);
    procedure ControlSetOnClick(Value: TNotifyEvent);
    procedure ControlSetHint(const Value: string);
    procedure ControlSetAnchors(Value: TAnchors);

    //IJvDynControlRTTIInspectorControl
    function ControlGetInspectedObject: TObject;
    function ControlGetVisibleItemsCount: Integer;
    function ControlIsPropertySupported(const aPropertyName : string): Boolean;
    procedure ControlSaveEditorValues;
    procedure ControlSetInspectedObject(const Value: TObject);

    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);
    function GetControlOnPropertyChange:
        TJvDynControlInspectorControlOnPropertyChangeEvent;
    procedure SetControlOnPropertyChange(const Value:
        TJvDynControlInspectorControlOnPropertyChangeEvent);
  end;

  {$ENDIF}

  TJvDynControlCxColorComboBox = class(TcxColorComboBox, IUnknown, IJvDynControl,
      IJvDynControlColorComboBoxControl, IJvDynControlDevExpCx)
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

    // IJvDynControlDevExpCx
    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    //IJvDynControlColorComboBoxControl
    function ControlGetColorName(AColor: TColor): string;
    function ControlGetSelectedColor: TColor;
    procedure ControlSetSelectedColor(const Value: TColor);
    function GetControlDefaultColor: TColor; stdcall;
    procedure SetControlDefaultColor(const Value: TColor); stdcall;
  end;

  TJvDynControlEngineDevExpCx = class(TJvDynControlEngine)
  private
    FCxProperties: TCxDynControlWrapper;
  protected
    procedure SetcxProperties(Value: TCxDynControlWrapper);
    procedure RegisterControls; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CreateControlClass(AControlClass: TControlClass; AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl; override;
  published
    property CxProperties: TCxDynControlWrapper read FCxProperties write FCxProperties;
  end;

  TJvDynControlCxCheckComboBox = class(TcxCheckComboBox, IUnknown, IJvDynControl, IJvDynControlData, IJvDynControlItems,
      IJvDynControlDblClick, IJvDynControlDevExpCx, IJvDynControlReadOnly, IJvDynControlCheckComboBox)
  private
    FIntItems: TStrings;
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

    procedure ControlSetSorted(Value: Boolean);
    procedure ControlSetItems(Value: TStrings);
    function ControlGetItems: TStrings;

    procedure ControlSetOnDblClick(Value: TNotifyEvent);

    procedure ControlSetCxProperties(Value: TCxDynControlWrapper);

    function ControlGetDelimiter: string;
    procedure ControlSetAnchors(Value: TAnchors);
    procedure ControlSetDelimiter(Value: string);
  end;



procedure SetDynControlEngineDevExpCxDefault;
function DynControlEngineDevExpCx: TJvDynControlEngineDevExpCx;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: jvcl/run/JvDynControlEngineDevExpCx.pas $';
    Revision: '$Revision: 3c2d95f4dca5add4bf68ec3879a79a241f867aee $';
    Date: '$Date: 2013-08-11 21:01:11 +0200 $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

uses
  SysUtils, ExtDlgs, Variants,
  {$IFNDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  JvDynControlEngineJVCL,
  {$ENDIF}
  cxTextEdit, cxControls,
  JvDynControlEngineVCL,
  JvJCLUtils, JvBrowseFolder, JvDynControlEngineTools,
  cxLookAndFeelPainters, TypInfo;

var
  IntDynControlEngineDevExpCx: TJvDynControlEngineDevExpCx = nil;

//=== { TCxDynControlWrapper } ===============================================

constructor TCxDynControlWrapper.Create;
begin
  inherited Create;
  FLookAndFeel := TcxLookAndFeel.Create(nil);
  FStyleController := TcxEditStyleController.Create(nil);
end;

destructor TCxDynControlWrapper.Destroy;
begin
  FreeAndNil(FStyleController);
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TCxDynControlWrapper.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TCxDynControlWrapper.SetStyleController(Value: TcxEditStyleController);
begin
  FStyleController := Value;
end;

//=== { TJvDynControlCxMaskEdit } ============================================

procedure TJvDynControlCxMaskEdit.ControlSetDefaultProperties;
begin
  Properties.MaskKind := emkStandard;
end;

procedure TJvDynControlCxMaskEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlCxMaskEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxMaskEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxMaskEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxMaskEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxMaskEdit.ControlSetPasswordChar(Value: Char);
begin
  if Value <> #0 then
    Properties.EchoMode := eemPassword
  else
    Properties.EchoMode := eemNormal;
end;

procedure TJvDynControlCxMaskEdit.ControlSetEditMask(const Value: string);
begin
  Properties.EditMask := Value;
  Properties.MaskKind := emkStandard;
end;

//=== { TJvDynControlCxButtonEdit } ==========================================

procedure TJvDynControlCxButtonEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := IntOnButtonClick;
  Properties.MaskKind := emkStandard;
end;

procedure TJvDynControlCxButtonEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlCxButtonEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxButtonEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxButtonEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxButtonEdit.ControlSetPasswordChar(Value: Char);
begin
  if Value <> #0 then
    Properties.EchoMode := eemPassword
  else
    Properties.EchoMode := eemNormal;
end;

procedure TJvDynControlCxButtonEdit.ControlSetEditMask(const Value: string);
begin
  Properties.EditMask := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetOnButtonClick(Value: TNotifyEvent);
begin
  FIntOnButtonClick := Value;;
end;

procedure TJvDynControlCxButtonEdit.ControlSetButtonCaption(const Value: string);
begin
  Properties.Buttons[0].DisplayName := Value;
end;

procedure TJvDynControlCxButtonEdit.ControlSetGlyph(Value: TBitmap);
begin
  Properties.Buttons[0].Glyph.Assign(Value);
end;

procedure TJvDynControlCxButtonEdit.ControlSetNumGlyphs(Value: Integer);
begin
end;

procedure TJvDynControlCxButtonEdit.ControlSetLayout(Value: TButtonLayout);
begin
end;

procedure TJvDynControlCxButtonEdit.ControlSetDefault(Value: Boolean);
begin
end;

procedure TJvDynControlCxButtonEdit.ControlSetCancel(Value: Boolean);
begin
end;


procedure TJvDynControlCxButtonEdit.IntOnButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  if Assigned(FIntOnButtonClick) then
    FIntOnButtonClick(Sender);
end;

//=== { TJvDynControlCxCalcEdit } ============================================

procedure TJvDynControlCxCalcEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxCalcEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlCxCalcEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetValue(Value: Variant);
begin
  Self.Value := Value;
end;

function TJvDynControlCxCalcEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxCalcEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxCalcEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxSpinEdit } ============================================

procedure TJvDynControlCxSpinEdit.ControlSetDefaultProperties;
begin
  Text := '0';
end;

procedure TJvDynControlCxSpinEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetOnClick(Value: TNotifyEvent);
begin

end;

procedure TJvDynControlCxSpinEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetValue(Value: Variant);
begin
  Self.Value := Value;
end;

function TJvDynControlCxSpinEdit.ControlGetValue: Variant;
begin
  Result := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetIncrement(Value: Integer);
begin
  Properties.Increment := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetMinValue(Value: double);
begin
  Properties.MinValue := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetMaxValue(Value: double);
begin
  Properties.MaxValue := Value;
end;

procedure TJvDynControlCxSpinEdit.ControlSetUseForInteger(Value: Boolean);
begin
  if Value then
    Properties.ValueType := vtInt
  else
    Properties.ValueType := vtFloat;
end;

procedure TJvDynControlCxSpinEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxFileNameEdit } ========================================

procedure TJvDynControlCxFileNameEdit.DefaultOnButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if not Properties.ReadOnly then
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
    if CanFocus then
      SetFocus;
  end;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := DefaultOnButtonClick;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxFileNameEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxFileNameEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDefaultExt(const Value: string);
begin
  FDefaultExt := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDialogOptions(Value: TOpenOptions);
begin
  FDialogOptions := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetFilter(const Value: string);
begin
  FFilter := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetFilterIndex(Value: Integer);
begin
  FFilterIndex := Value;
end;

procedure TJvDynControlCxFileNameEdit.ControlSetDialogKind(Value: TJvDynControlFileNameDialogKind);
begin
  FDialogKind := Value;
end;

//=== { TJvDynControlCxDirectoryEdit } =======================================

procedure TJvDynControlCxDirectoryEdit.DefaultOnButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  Dir: string;
begin
  if not Properties.ReadOnly then
  begin
    Dir := ControlGetValue;
    if Dir = '' then
    begin
      if FInitialDir <> '' then
        Dir := FInitialDir
      else
        Dir := '\';
    end;
    if not DirectoryExists(Dir) then
      Dir := '\';
    if BrowseForFolder('', True, Dir, HelpContext) then
//    if SelectDirectory(Dir, FDialogOptions, HelpContext) then
      ControlSetValue(Dir);
    if CanFocus then
      SetFocus;
  end;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetDefaultProperties;
begin
  Properties.OnButtonClick := DefaultOnButtonClick;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxDirectoryEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetInitialDir(const Value: string);
begin
  FInitialDir := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetDialogTitle(const Value: string);
begin
  FDialogTitle := Value;
end;

procedure TJvDynControlCxDirectoryEdit.ControlSetDialogOptions(Value: TSelectDirOpts);
begin
  FDialogOptions := Value;
end;

//=== { TJvDynControlCxDateTimeEdit } ========================================

procedure TJvDynControlCxDateTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime  := True;
  Properties.SaveTime  := False;
  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetValue(Value: Variant);
begin
  if VarIsStr(Value) then
    Date := StrToDateTime(Value)
  else
    Date := Value;
end;

function TJvDynControlCxDateTimeEdit.ControlGetValue: Variant;
begin
  if Text = '' then
    Result := Null
  else
    Result := Date;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

// IJvDynControlDate
procedure TJvDynControlCxDateTimeEdit.ControlSetMinDate(Value: TDateTime);
begin
  Properties.MinDate := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetMaxDate(Value: TDateTime);
begin
  Properties.MaxDate := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetFormat(const Value: string);
begin
//  Format := Value;
end;

procedure TJvDynControlCxDateTimeEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxDateEdit } ============================================

procedure TJvDynControlCxDateEdit.ControlSetDefaultProperties;
begin
  Properties.ShowTime  := False;
  Properties.SaveTime  := False;
  Properties.InputKind := ikStandard;
end;

procedure TJvDynControlCxDateEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxDateEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetValue(Value: Variant);
begin
  if VarIsStr(Value) then
    Date := StrToDateTime(Value)
  else
    Date := Value;
end;

function TJvDynControlCxDateEdit.ControlGetValue: Variant;
begin
  if Text = '' then
    Result := Null
  else
    Result := Date;
end;

procedure TJvDynControlCxDateEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

// IJvDynControlDate
procedure TJvDynControlCxDateEdit.ControlSetMinDate(Value: TDateTime);
begin
  Properties.MinDate := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetMaxDate(Value: TDateTime);
begin
  Properties.MaxDate := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetFormat(const Value: string);
begin
//  Format := Value;
end;

procedure TJvDynControlCxDateEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//=== { TJvDynControlCxTimeEdit } ============================================

procedure TJvDynControlCxTimeEdit.ControlSetDefaultProperties;
begin
  Properties.ShowDate := False;
  Properties.UseCtrlIncrement := True;
end;

procedure TJvDynControlCxTimeEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxTimeEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetValue(Value: Variant);
begin
  if VarIsStr(Value) then
    Time := StrToTime(Value)
  else
    Time := Value;
end;

function TJvDynControlCxTimeEdit.ControlGetValue: Variant;
begin
  if Text = '' then
    Result := Null
  else
    Result := Time;
end;

procedure TJvDynControlCxTimeEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxTimeEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxTimeEdit.ControlSetFormat(const Value: string);
begin
//  Properties.Format := Value;
  Properties.Use24HourFormat := (Pos('H', Value) > 0);
  if (Pos('s', Value) > 0) then
    Properties.TimeFormat := tfHourMinSec
  else
  if (Pos('m', Value) > 0) then
    Properties.TimeFormat := tfHourMin
  else
    Properties.TimeFormat := tfHour;
end;

//=== { TJvDynControlCxCheckBox } ===========================================

function TJvDynControlCxCheckBox.ControlGetCaption: string;
begin
  Result := Properties.Caption;
end;

procedure TJvDynControlCxCheckBox.ControlSetDefaultProperties;
begin
  Transparent := True;
  AutoSize := False;
end;

procedure TJvDynControlCxCheckBox.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetCaption(const Value: string);
begin
  if Properties.Caption <> Value then
    Properties.Caption := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlCxCheckBox.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlCxCheckBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetAllowGrayed(Value: Boolean);
begin
  Properties.AllowGrayed := Value;
end;

procedure TJvDynControlCxCheckBox.ControlSetState(Value: TCheckBoxState);
begin
  case Value of
    cbUnchecked:
      State := cbsUnchecked;
    cbChecked:
      State := cbsChecked;
    cbGrayed:
      State := cbsGrayed;
  end;
end;

function TJvDynControlCxCheckBox.ControlGetState: TCheckBoxState;
begin
  case State of
    cbsUnchecked:
      Result := cbUnchecked;
    cbsChecked:
      Result := cbChecked;
  else
    Result := cbGrayed;
  end;
end;


procedure TJvDynControlCxCheckBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxCheckBox.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlCxCheckBox.ControlGetFont: TFont;
begin
  Result := Font;
end;

function TJvDynControlCxMemo.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlCxMemo } ================================================

procedure TJvDynControlCxMemo.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxMemo.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxMemo.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxMemo.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxMemo.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxMemo.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxMemo.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxMemo.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxMemo.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlCxMemo.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlCxMemo.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWantTabs(Value: Boolean);
begin
  Properties.WantTabs := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWantReturns(Value: Boolean);
begin
  Properties.WantReturns := Value;
end;

procedure TJvDynControlCxMemo.ControlSetWordWrap(Value: Boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxMemo.ControlSetScrollBars(Value: TScrollStyle);
begin
  Properties.ScrollBars := Value;
end;

procedure TJvDynControlCxMemo.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxMemo.ControlSetAlignment(Value: TAlignment);
begin
  Properties.Alignment := Value;
end;

procedure TJvDynControlCxMemo.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;


function TJvDynControlCxRichEdit.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlCxRichEdit } ============================================

procedure TJvDynControlCxRichEdit.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxRichEdit.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxRichEdit.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxRichEdit.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxRichEdit.ControlSetItems(Value: TStrings);
begin
  Lines.Assign(Value);
end;

function TJvDynControlCxRichEdit.ControlGetItems: TStrings;
begin
  Result := Lines;
end;

procedure TJvDynControlCxRichEdit.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetWantTabs(Value: Boolean);
begin
  Properties.WantTabs := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetWantReturns(Value: Boolean);
begin
  Properties.WantReturns := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetWordWrap(Value: Boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetScrollBars(Value: TScrollStyle);
begin
  Properties.ScrollBars := Value;
end;

procedure TJvDynControlCxRichEdit.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxRichEdit.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

//=== { TJvDynControlCxRadioGroup } ===========================================

function TJvDynControlCxRadioGroup.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxRadioGroup.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetValue(Value: Variant);
var
  I: Integer;
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
  begin
    ItemIndex := -1;
    for I := 0 to Properties.Items.Count - 1 do
      if TcxRadioGroupItem(Properties.Items[I]).Caption = Value then
      begin
        ItemIndex := I;
        Break;
      end;
  end;
end;

function TJvDynControlCxRadioGroup.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxRadioGroup.ControlSetSorted(Value: Boolean);
begin
end;

procedure TJvDynControlCxRadioGroup.ControlSetItems(Value: TStrings);
var
  I: Integer;
  Item: TcxRadioGroupItem;
begin
  Properties.Items.Clear;
  for I := 0 to Value.Count - 1 do
  begin
    Item := TcxRadioGroupItem(Properties.Items.Add);
    Item.Caption := Value[I];
  end;
end;

function TJvDynControlCxRadioGroup.ControlGetItems: TStrings;
begin
//  Result := TStrings(Properties.Items);
  Result := nil;
end;

procedure TJvDynControlCxRadioGroup.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxRadioGroup.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxRadioGroup.ControlSetColumns(Value: Integer);
begin
  Properties.Columns := Value;
end;

function TJvDynControlCxListBox.ControlGetItemIndex: Integer;
begin
  Result := ItemIndex;
end;

//=== { TJvDynControlCxListBox } =============================================

procedure TJvDynControlCxListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxListBox.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlCxListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlCxListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlCxListBox.ControlSetItems(Value: TStrings);
begin
  Items.Assign(Value);
end;

function TJvDynControlCxListBox.ControlGetItems: TStrings;
begin
  Result := Items;
end;

procedure TJvDynControlCxListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxListBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxListBox.ControlSetItemIndex(const Value: Integer);
begin
  ItemIndex := Value;
end;

function TJvDynControlCxListBox.ControlGetOnKeyDown: TKeyEvent;
begin
  Result := OnKeyDown;
end;

function TJvDynControlCxListBox.ControlGetOnKeyPress: TKeyPressEvent;
begin
  Result := OnKeyPress;
end;

function TJvDynControlCxListBox.ControlGetOnKeyUp: TKeyEvent;
begin
  Result := OnKeyUp;
end;

function TJvDynControlCxListBox.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlCxListBox.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlCxListBox.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlCxListBox.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlCxListBox.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlCxListBox.ControlSetOnKeyDown(const Value: TKeyEvent);
begin
  OnKeyDown := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnKeyPress(const Value: TKeyPressEvent);
begin
  OnKeyPress := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnKeyUp(const Value: TKeyEvent);
begin
  OnKeyUp := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlCxListBox.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlCxListBox.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlCxListBox.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

//=== { TJvDynControlCxCheckListBox } ========================================

constructor TJvDynControlCxCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIntItems := TStringList.Create;
end;

destructor TJvDynControlCxCheckListBox.Destroy;
begin
  FIntItems.Free;
  Inherited Destroy;
end;

procedure TJvDynControlCxCheckListBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxCheckListBox.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetOnChange(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxCheckListBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetValue(Value: Variant);
begin
  if VarIsInt(Value) then
    ItemIndex := Value
  else
    ItemIndex := Items.IndexOf(Value);
end;

function TJvDynControlCxCheckListBox.ControlGetValue: Variant;
begin
  Result := ItemIndex;
end;

procedure TJvDynControlCxCheckListBox.ControlSetSorted(Value: Boolean);
begin
  Sorted := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetItems(Value: TStrings);
var
  I: Integer;
begin
  FIntItems.Assign(Value);
  Items.Clear;
  for I := 0 to FIntItems.Count-1 do
    with Items.Add do
      Text := FIntItems[I];
end;

function TJvDynControlCxCheckListBox.ControlGetItems: TStrings;
var
  I: Integer;
begin
  FIntItems.Clear;
  for I := 0 to Items.Count-1 do
    FIntItems.Add(Items[I].Text);
  Result := FIntItems;
end;

procedure TJvDynControlCxCheckListBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

//IJvDynControlCheckListBox = interface
procedure TJvDynControlCxCheckListBox.ControlSetAllowGrayed(Value: Boolean);
begin
  AllowGrayed := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetChecked(Index: Integer; Value: Boolean);
begin
  Items[Index].Checked := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetItemEnabled(Index: Integer; Value: Boolean);
begin
  Items[Index].Enabled := Value;
end;

procedure TJvDynControlCxCheckListBox.ControlSetHeader(Index: Integer; Value: Boolean);
begin
end;

procedure TJvDynControlCxCheckListBox.ControlSetState(Index: Integer; Value: TCheckBoxState);
begin
  case Value of
    cbUnchecked:
      Items[Index].State := cbsUnchecked;
    cbChecked:
      Items[Index].State := cbsChecked;
    cbGrayed:
      Items[Index].State := cbsGrayed;
  end;
end;

function TJvDynControlCxCheckListBox.ControlGetChecked(Index: Integer): Boolean;
begin
  Result := Items[Index].Checked;
end;

function TJvDynControlCxCheckListBox.ControlGetItemEnabled(Index: Integer): Boolean;
begin
  Result := Items[Index].Enabled;
end;

function TJvDynControlCxCheckListBox.ControlGetHeader(Index: Integer): Boolean;
begin
  Result := False;
end;

function TJvDynControlCxCheckListBox.ControlGetState(Index: Integer): TCheckBoxState;
begin
  case Items[Index].State of
    cbsUnchecked:
      Result := cbUnchecked;
    cbsChecked:
      Result := cbChecked;
  else
    Result := cbGrayed;
  end;
end;

procedure TJvDynControlCxCheckListBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

//=== { TJvDynControlCxComboBox } ============================================

procedure TJvDynControlCxComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxComboBox.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxComboBox.ControlSetSorted(Value: Boolean);
begin
  Properties.Sorted := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetItems(Value: TStrings);
begin
  Properties.Items.Assign(Value);
end;

function TJvDynControlCxComboBox.ControlGetItems: TStrings;
begin
  Result := Properties.Items;
end;

procedure TJvDynControlCxComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxComboBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxComboBox.ControlSetNewEntriesAllowed(Value: Boolean);
begin
  if Value then
    Properties.DropDownListStyle := lsEditList
  else
    Properties.DropDownListStyle := lsEditFixedList;
end;

//=== { TJvDynControlCxGroupBox } ===========================================

function TJvDynControlCxGroupBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxGroupBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxGroupBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetColor(Value: TColor);
begin
  Style.Color := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxGroupBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxGroupBox.ControlSetParentColor(Value: Boolean);
begin
  Parentcolor := Value;
end;

//=== { TJvDynControlCxPanel } ===========================================

function TJvDynControlCxPanel.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxPanel.ControlSetDefaultProperties;
begin
  BevelInner := bvNone;
  BevelOuter := bvNone;
  PanelStyle.Active := True;
  PanelStyle.BorderWidth := 0;
  Style.BorderStyle := ebsNone;
  Style.TransparentBorder := False;
end;

procedure TJvDynControlCxPanel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxPanel.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxPanel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxPanel.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxPanel.ControlSetBorder(ABevelInner: TPanelBevel; ABevelOuter: TPanelBevel; ABevelWidth: Integer; ABorderStyle: TBorderStyle; ABorderWidth: Integer);
begin
  ControlSetBorderWidth (ABorderWidth);
  ControlSetBorderStyle (ABorderStyle);
  ControlSetBevelInner  (ABevelInner);
  ControlSetBevelOuter  (ABevelOuter);
  BevelWidth := ABevelWidth;
end;


procedure TJvDynControlCxPanel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlCxPanel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxPanel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlCxPanel.ControlSetBevelInner(Value: TBevelCut);
begin
  BevelInner:= Value;
end;

procedure TJvDynControlCxPanel.ControlSetBevelKind(Value: TBevelKind);
begin
  BevelKind := Value;
end;

procedure TJvDynControlCxPanel.ControlSetBevelOuter(Value: TBevelCut);
begin
  BevelOuter:= Value;
end;

procedure TJvDynControlCxPanel.ControlSetBorderStyle(Value: TBorderStyle);
begin
  if value = bsNone then
    BorderStyle := cxcbsNone
  else
    BorderStyle := cxcbsDefault;
  if BorderStyle = cxcbsNone then
  begin
    if Style.BorderStyle <> ebsNone then
      Style.BorderStyle := ebsNone;
  end
  else
  begin
    if svBorderStyle in Style.AssignedValues then
      Style.AssignedValues := Style.AssignedValues - [ svBorderStyle ];
  end;
end;

procedure TJvDynControlCxPanel.ControlSetBorderWidth(Value: Integer);
begin
  PanelStyle.BorderWidth := Value;
end;

procedure TJvDynControlCxPanel.ControlSetColor(Value: TColor);
begin
  Style.Color := Value;
end;

procedure TJvDynControlCxPanel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlCxPanel.ControlSetAlignment(Value: TAlignment);
begin
  Properties.Alignment.Horz := Value;
end;


//=== { TJvDynControlCxImage } ===============================================

procedure TJvDynControlCxImage.ControlSetDefaultProperties;
begin
  Properties.GraphicTransparency := gtDefault;
  ParentColor := True;
end;

procedure TJvDynControlCxImage.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxImage.ControlSetOnEnter(Value: TNotifyEvent);
begin
//  OnEnter := Value;
end;

procedure TJvDynControlCxImage.ControlSetOnExit(Value: TNotifyEvent);
begin
//  OnExit := Value;
end;

procedure TJvDynControlCxImage.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxImage.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxImage.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlCxImage.ControlSetIncrementalDisplay(Value: Boolean);
begin
//  Properties.IncrementalDisplay := Value;
end;

procedure TJvDynControlCxImage.ControlSetCenter(Value: Boolean);
begin
  Properties.Center := Value;
end;

procedure TJvDynControlCxImage.ControlSetProportional(Value: Boolean);
begin
//  Properties.Proportional := Value;
end;

procedure TJvDynControlCxImage.ControlSetStretch(Value: Boolean);
begin
  Properties.Stretch := Value;
end;

procedure TJvDynControlCxImage.ControlSetTransparent(Value: Boolean);
begin
  if Value then
    Properties.GraphicTransparency := gtDefault
  else
    Properties.GraphicTransparency := gtTransparent;
end;

procedure TJvDynControlCxImage.ControlSetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TJvDynControlCxImage.ControlSetGraphic(Value: TGraphic);
begin
  Picture.Assign(Value);
end;

function TJvDynControlCxImage.ControlGetPicture: TPicture;
begin
  Result := Picture;
end;

procedure TJvDynControlCxImage.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxImage.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Properties.Center := True;
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Properties.ShowFocusRect := False;
  if Assigned(Style.StyleController) then
  begin
    Style.StyleController := Value.StyleController;
    Style.StyleController.Style.BorderStyle := ebsNone;
  end;
end;

//=== { TJvDynControlCxScrollBox } ===========================================

function TJvDynControlCxScrollBox.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxScrollBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxScrollBox.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxScrollBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxScrollBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

//=== { TJvDynControlCxLabel } ===========================================

function TJvDynControlCxLabel.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxLabel.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxLabel.ControlSetDefaultProperties;
begin
  AutoSize := False;
  Transparent := True;
  Style.BorderStyle := ebsNone;
end;

procedure TJvDynControlCxLabel.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxLabel.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxLabel.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxLabel.ControlSetFocusControl(Value: TWinControl);
begin
  FocusControl := Value;
end;

procedure TJvDynControlCxLabel.ControlSetWordWrap(Value: Boolean);
begin
  Properties.WordWrap := Value;
end;

procedure TJvDynControlCxLabel.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxLabel.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlCxLabel.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlCxLabel.ControlSetColor(Value: TColor);
begin
  Style.Color := Value;
end;

procedure TJvDynControlCxLabel.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlCxLabel.ControlSetAlignment(Value: TAlignment);
begin
  Properties.Alignment.Horz := Value;
end;

procedure TJvDynControlCxLabel.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlCxLabel.ControlGetFont: TFont;
begin
  Result := Font;
end;

//=== { TJvDynControlCxStaticText } ===========================================

function TJvDynControlCxStaticText.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxStaticText.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetDefaultProperties;
begin
  AutoSize := False;
  Transparent := True;
  Style.BorderStyle := ebsNone;
end;

procedure TJvDynControlCxStaticText.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxStaticText.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

procedure TJvDynControlCxStaticText.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetAutoSize(Value: Boolean);
begin
  AutoSize := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetColor(Value: TColor);
begin
  Style.Color := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetParentColor(Value: Boolean);
begin
  ParentColor := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetAlignment(Value: TAlignment);
begin
  Properties.Alignment.Horz := Value;
end;

procedure TJvDynControlCxStaticText.ControlSetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

function TJvDynControlCxStaticText.ControlGetFont: TFont;
begin
  Result := Font;
end;


//=== { TJvDynControlCxButton } ===========================================

function TJvDynControlCxButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxButton.ControlSetTabOrder(Value: Integer);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnExit(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxButton.ControlSetGlyph(Value: TBitmap);
begin
  Glyph.Assign(Value);
end;

procedure TJvDynControlCxButton.ControlSetNumGlyphs(Value: Integer);
begin
  NumGlyphs := Value;
end;

procedure TJvDynControlCxButton.ControlSetLayout(Value: TButtonLayout);
begin
  Layout := Value;
end;

procedure TJvDynControlCxButton.ControlSetDefault(Value: Boolean);
begin
  Default := Value;
end;

procedure TJvDynControlCxButton.ControlSetCancel(Value: Boolean);
begin
  Cancel := Value;
end;

procedure TJvDynControlCxButton.ControlSetAction(Value: TCustomAction);
begin
  Action := Value;
end;

procedure TJvDynControlCxButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;


procedure TJvDynControlCxButton.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

//=== { TJvDynControlCxTreeView } ============================================

procedure TJvDynControlCxTreeView.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxTreeView.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetReadOnly(Value: Boolean);
begin
  ReadOnly := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetAutoExpand(Value: Boolean);
begin
  AutoExpand := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetShowHint(Value: Boolean);
begin
  ShowHint := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetShowLines(Value: Boolean);
begin
  ShowLines := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetShowRoot(Value: Boolean);
begin
  ShowRoot := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetToolTips(Value: Boolean);
begin
  ToolTips := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetItems(Value: TTreeNodes);
begin
//  Items.Assign(Value);
  Items := Value;
end;

function TJvDynControlCxTreeView.ControlGetItems: TTreeNodes;
begin
  Result := Items;
end;

function TJvDynControlCxTreeView.ControlGetOnMouseDown: TMouseEvent;
begin
  Result := OnMouseDown;
end;

function TJvDynControlCxTreeView.ControlGetOnMouseEnter: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseEnter;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlCxTreeView.ControlGetOnMouseLeave: TNotifyEvent;
begin
  {$IFDEF DELPHI2007_UP}
  Result := OnMouseLeave;
  {$ENDIF DELPHI2007_UP}
end;

function TJvDynControlCxTreeView.ControlGetOnMouseMove: TMouseMoveEvent;
begin
  Result := OnMouseMove;
end;

function TJvDynControlCxTreeView.ControlGetOnMouseUp: TMouseEvent;
begin
  Result := OnMouseUp;
end;

procedure TJvDynControlCxTreeView.ControlSetImages(Value: TCustomImageList);
begin
  Images.Assign(Value);
end;

procedure TJvDynControlCxTreeView.ControlSetStateImages(Value: TCustomImageList);
begin
  StateImages.Assign(Value);
end;

procedure TJvDynControlCxTreeView.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnChange(Value: TTVChangedEvent);
begin
  OnChange := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetSortType(Value: TSortType);
begin
  SortType := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxTreeView.ControlSetOnChanging(Value: TTVChangingEvent);
begin
  OnChanging := Value;
end;

procedure TJvDynControlCxTreeView.ControlSortItems;
begin
  AlphaSort;
end;

function TJvDynControlCxTreeView.ControlGetSelected: TTreeNode;
begin
  Result := Selected;
end;

procedure TJvDynControlCxTreeView.ControlSetOnMouseDown(const Value: TMouseEvent);
begin
  OnMouseDown := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnMouseEnter(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlCxTreeView.ControlSetOnMouseLeave(const Value: TNotifyEvent);
begin
  {$IFDEF DELPHI2007_UP}
  OnMouseEnter := Value;
  {$ENDIF DELPHI2007_UP}
end;

procedure TJvDynControlCxTreeView.ControlSetOnMouseMove(const Value: TMouseMoveEvent);
begin
  OnMouseMove := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetOnMouseUp(const Value: TMouseEvent);
begin
  OnMouseUp := Value;
end;

procedure TJvDynControlCxTreeView.ControlSetSelected(const Value: TTreeNode);
begin
  Selected := Value;
end;

constructor TJvDynControlCxProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTaskbarProgress  := TdxTaskbarProgress .Create(AOwner);
  fTaskbarProgress.LinkedComponent := Self;
end;

destructor TJvDynControlCxProgressBar.Destroy;
begin
  fTaskbarProgress.State := tbpsNoProgress;
  fTaskbarProgress.Active := False;
  FreeAndNil(fTaskbarProgress );
  inherited Destroy;
end;

procedure TJvDynControlCxProgressBar.ControlSetAlign(Value: TAlign);
begin
  Align := Value;
end;

//=== { TJvDynControlCxProgressbar } =========================================

procedure TJvDynControlCxProgressbar.ControlSetDefaultProperties;
begin
  Properties.ShowText := False;
  Properties.AnimationSpeed := 3;
end;

procedure TJvDynControlCxProgressbar.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetMax(Value: Integer);
begin
  Properties.Max := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetMin(Value: Integer);
begin
  Properties.Min := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetOrientation(Value: TProgressBarOrientation);
begin
  if Value = pbHorizontal then
    Properties.Orientation:= cxorHorizontal
  else
    Properties.Orientation:= cxorVertical;
end;

procedure TJvDynControlCxProgressbar.ControlSetPosition(Value: Integer);
begin
  Position := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetSmooth(Value: Boolean);
begin
  //Properties.Smooth := Value;
  if Value then
    Properties.BarStyle := cxbsSolid
  else
    Properties.BarStyle := cxbsLEDs;
end;

procedure TJvDynControlCxProgressbar.ControlSetStep(Value: Integer);
begin
//  Step := Value;
end;

procedure TJvDynControlCxProgressbar.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
  fTaskbarProgress.Active := True;
end;

procedure TJvDynControlCxProgressBar.ControlSetMarquee(Value: Boolean);
begin
  Properties.Marquee := Value;
end;



//=== { TJvDynControlCxRadioButton } ===========================================

function TJvDynControlCxRadioButton.ControlGetCaption: string;
begin
  Result := Caption;
end;

procedure TJvDynControlCxRadioButton.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxRadioButton.ControlSetCaption(const Value: string);
begin
  if Caption <> Value then
    Caption := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

// IJvDynControlData
procedure TJvDynControlCxRadioButton.ControlSetOnChange(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetValue(Value: Variant);
begin
  Checked := JvDynControlVariantToBoolean(Value);
end;

function TJvDynControlCxRadioButton.ControlGetValue: Variant;
begin
  Result := Checked;
end;

procedure TJvDynControlCxRadioButton.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxRadioButton.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;


//=== { TJvDynControlCxTabControl } ==========================================

procedure TJvDynControlCxTabControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxTabControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlCxTabControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlCxTabControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

procedure TJvDynControlCxTabControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxTabControl.ControlCreateTab(const AName: string);
begin
  Tabs.Add(AName);
end;

function TJvDynControlCxTabControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlCxTabControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxTabControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlCxTabControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;


//=== { TJvDynControlCxPageControl } =========================================

procedure TJvDynControlCxPageControl.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxPageControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxPageControl.ControlCreateTab(const AName: string);
var
  TabSheet: TcxTabSheet;
begin
  TabSheet := TcxTabSheet.Create(Self);
  TabSheet.Caption := AName;
  TabSheet.PageControl := Self;
  TabSheet.Parent := Self;
end;

procedure TJvDynControlCxPageControl.ControlSetOnChangeTab(OnChangeEvent: TNotifyEvent);
begin
  OnChange := OnChangeEvent;
end;

procedure TJvDynControlCxPageControl.ControlSetOnChangingTab(OnChangingEvent: TTabChangingEvent);
begin
  OnChanging := OnChangingEvent;
end;

procedure TJvDynControlCxPageControl.ControlSetTabIndex(Index: Integer);
begin
  TabIndex := Index;
end;

function TJvDynControlCxPageControl.ControlGetTabIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TJvDynControlCxPageControl.ControlSetMultiLine(Value: Boolean);
begin
  MultiLine := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetScrollOpposite(Value: Boolean);
begin
  ScrollOpposite := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetHotTrack(Value: Boolean);
begin
  HotTrack := Value;
end;

procedure TJvDynControlCxPageControl.ControlSetRaggedRight(Value: Boolean);
begin
  RaggedRight := Value;
end;

function TJvDynControlCxPageControl.ControlGetPage(const PageName: string): TWinControl;
var
  I: Integer;
begin
  I := Tabs.IndexOf(PageName);
  if (I >= 0) and (I < PageCount) then
    Result := TWinControl(Pages[I])
  else
    Result := nil;
end;

procedure TJvDynControlCxPageControl.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;


function TJvDynControlEngineDevExpCx.CreateControlClass(AControlClass: TControlClass; AOwner: TComponent; AParentControl: TWinControl; AControlName: string): TControl;
var
  Control: TControl;
begin
  Control := inherited CreateControlClass(AControlClass, AOwner, AParentControl, AControlName);
  if Supports(Control, IJvDynControlDevExpCx) then
    with Control as IJvDynControlDevExpCx do
      ControlSetCxProperties(cxProperties);
  Result := Control;
end;

//=== { DynControlEngineDevExpCx } ===========================================

procedure SetDynControlEngineDevExpCxDefault;
begin
  SetDefaultDynControlEngine(IntDynControlEngineDevExpCx);
end;

function DynControlEngineDevExpCx: TJvDynControlEngineDevExpCx;
begin
  Result := IntDynControlEngineDevExpCx;
end;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}

//=== { TJvDynControlCxRTTIInspectorControl } ========================================

procedure TJvDynControlCxRTTIInspectorControl.ControlSetDefaultProperties;
begin
  OnFilterProperty := InspectorOnFilterProperty;
  OnItemChanged := InspectorOnItemChanged;
  OnDrawRowHeader := ReplaceOnDrawRowHeader;
end;

function TJvDynControlCxRTTIInspectorControl.ControlGetCurrentPropertyName:
    string;
begin
  if Assigned (FocusedRow) and Assigned(TcxPropertyRow(FocusedRow).PropertyEditor) then
    Result := TcxPropertyRow(FocusedRow).PropertyEditor.GetName
  else
    Result := '';
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

function TJvDynControlCxRTTIInspectorControl.ControlGetInspectedObject: TObject;
begin
  Result := InspectedObject;
end;

function TJvDynControlCxRTTIInspectorControl.ControlGetOnDisplayProperty:
    TJvDynControlInspectorControlOnDisplayPropertyEvent;
begin
  Result := fOnDisplayProperty;
end;

function TJvDynControlCxRTTIInspectorControl.ControlGetOnTranslatePropertyName:
    TJvDynControlInspectorControlOnTranslatePropertyNameEvent;
begin
  Result := fOnTranslatePropertyName;
end;

function TJvDynControlCxRTTIInspectorControl.ControlGetVisibleItemsCount: Integer;
begin
  Result := Rows.Count;
end;

function TJvDynControlCxRTTIInspectorControl.ControlIsPropertySupported(const
    aPropertyName : string): Boolean;
begin
  if Assigned(InspectedObject) then
    if IsPublishedProp(InspectedObject, aPropertyName) then
      if PropIsType(InspectedObject, aPropertyName, tkClass) then
        Result := GetObjectProp(InspectedObject, aPropertyName) is TStringList
      else
        Result := True
    else
      Result := False
  else
    Result := True;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSaveEditorValues;
begin
  HideEdit;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetCxProperties(Value:
    TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetInspectedObject(const
    Value: TObject);
begin
  if Value is TPersistent then
    InspectedObject := TPersistent(Value)
  else
    InspectedObject := nil;
  OldPropertyName := '';

end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetOnDisplayProperty(const
    Value: TJvDynControlInspectorControlOnDisplayPropertyEvent);
begin
  fOnDisplayProperty := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.ControlSetOnTranslatePropertyName(
    const Value: TJvDynControlInspectorControlOnTranslatePropertyNameEvent);
begin
  fOnTranslatePropertyName := Value;
end;

function TJvDynControlCxRTTIInspectorControl.GetControlDividerWidth: Integer;
begin
  Result := OptionsView.RowHeaderWidth;
end;

procedure TJvDynControlCxRTTIInspectorControl.InspectorOnItemChanged(Sender:
    TObject; AOldRow: TcxCustomRow; AOldCellIndex: Integer);
var
  NewPropertyName: string;
begin
  NewPropertyName := ControlGetCurrentPropertyName;
  if NewPropertyName = ''  then
    Exit;
  if Assigned(fControlOnPropertyChange) then
    fControlOnPropertyChange(OldPropertyName, NewPropertyName);
  OldPropertyName := NewPropertyName;
end;

function TJvDynControlCxRTTIInspectorControl.GetControlOnPropertyChange:
    TJvDynControlInspectorControlOnPropertyChangeEvent;
begin
  Result := fControlOnPropertyChange;
end;

procedure TJvDynControlCxRTTIInspectorControl.InspectorOnFilterProperty(Sender:
    TObject; const PropertyName: string; var Accept: Boolean);
begin
  if Assigned(fonDisplayProperty) And IsPublishedProp(InspectedObject, PropertyName) then
    Accept := fOnDisplayProperty(PropertyName) and ControlIsPropertySupported(PropertyName);
end;

procedure TJvDynControlCxRTTIInspectorControl.ReplaceOnDrawRowHeader(Sender:
    TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter; AHeaderViewInfo:
    TcxCustomRowHeaderInfo; var Done: Boolean);
begin
  if (AHeaderViewInfo is TcxEditorRowHeaderInfo) and Assigned(fOnTranslatePropertyName)then
    TcxEditorRowHeaderInfo(AHeaderViewInfo).CaptionsInfo[0].Caption := fOnTranslatePropertyName(TcxEditorRowHeaderInfo(AHeaderViewInfo).CaptionsInfo[0].Caption);
end;

procedure TJvDynControlCxRTTIInspectorControl.SetControlDividerWidth(const
    Value: Integer);
begin
  OptionsView.RowHeaderWidth := Value;
end;

procedure TJvDynControlCxRTTIInspectorControl.SetControlOnPropertyChange(const
    Value: TJvDynControlInspectorControlOnPropertyChangeEvent);
begin
  fControlOnPropertyChange := Value;
end;

{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}

function TJvDynControlCxColorComboBox.ControlGetColorName(AColor: TColor):
    string;
begin
  Result := '';
end;

function TJvDynControlCxColorComboBox.ControlGetSelectedColor: TColor;
begin
  Result := ColorValue;
end;

procedure TJvDynControlCxColorComboBox.ControlSetDefaultProperties;
begin
end;

procedure TJvDynControlCxColorComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
end;

procedure TJvDynControlCxColorComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxColorComboBox.ControlSetValue(Value: Variant);
begin
  Text := VarToStr(Value);
end;

function TJvDynControlCxColorComboBox.ControlGetValue: Variant;
begin
  Result := Text;
end;

procedure TJvDynControlCxColorComboBox.ControlSetCxProperties(Value:
    TCxDynControlWrapper);
begin
  LookAndFeel.Assign(Value.LookAndFeel);
end;

procedure TJvDynControlCxColorComboBox.ControlSetSelectedColor(const Value:
    TColor);
begin
  ColorValue := Value;
end;

function TJvDynControlCxColorComboBox.GetControlDefaultColor: TColor;
begin
  Result := Properties.DefaultColor;
end;

procedure TJvDynControlCxColorComboBox.SetControlDefaultColor(const Value:
    TColor);
begin
  Properties.DefaultColor := Value;
end;

//=== { TJvDynControlCxCheckComboBox } ========================================

constructor TJvDynControlCxCheckComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIntItems := TStringList.Create;
end;

destructor TJvDynControlCxCheckComboBox.Destroy;
begin
  FIntItems.Free;
  Inherited Destroy;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetDefaultProperties;
begin
  Properties.EditValueFormat := cvfCaptions;
  Properties.ShowEmptyText := False;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetReadOnly(Value: Boolean);
begin
  Properties.ReadOnly := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetTabOrder(Value: Integer);
begin
  TabOrder := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetOnEnter(Value: TNotifyEvent);
begin
  OnEnter := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetOnExit(Value: TNotifyEvent);
begin
  OnExit := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetOnChange(Value: TNotifyEvent);
begin
  Properties.OnChange := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetOnClick(Value: TNotifyEvent);
begin
  OnClick := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetHint(const Value: string);
begin
  Hint := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetValue(Value: Variant);
var
  ACheckStates: TcxCheckStates;
  I: Integer;
  st : tStringList;
begin
//  Self.Value := Value;
  st := tStringList.Create;
  Properties.Items.BeginUpdate;
  try
    st.Duplicates := dupIgnore;
    if Properties.Delimiter <> '' then
      st.Delimiter := Properties.Delimiter[1]
    else
      st.Delimiter := chr(0);
    {$IFDEF DELPHI2009_UP}
    st.StrictDelimiter := True;
    {$ENDIF DELPHI2009_UP}
    st.DelimitedText := Value;

    SetLength(ACheckStates, Properties.Items.Count);
    for i := 0 to Properties.Items.Count - 1 do
      if st.IndexOf(Properties.Items[I].Description) >= 0 then
        aCheckStates[i] :=  cbsChecked
      else
        aCheckStates[i] :=  cbsUnChecked;
    Self.Value := CalculateCheckStatesValue (aCheckStates, Properties.Items, Properties.EditValueFormat);
  finally
    Properties.Items.EndUpdate;
    St.Free;
  end;
end;

function TJvDynControlCxCheckComboBox.ControlGetValue: Variant;
var
  APCheckStates: ^TcxCheckStates;
  I: Integer;
begin
  New(APCheckStates);
  try
    CalculateCheckStates(Value, Properties.Items, Properties.EditValueFormat , APCheckStates^);
    for i := 0 to Properties.Items.Count - 1 do
      if APCheckStates^[I] = cbsChecked then
        if Result = '' then
          Result := Properties.Items[I].Description
        else
          Result := Result+Properties.Delimiter+Properties.Items[I].Description;
  finally
    Dispose(APCheckStates)
  end;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetSorted(Value: Boolean);
begin
  Properties.Sorted := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetItems(Value: TStrings);
var
  I: Integer;
begin
  FIntItems.Assign(Value);
  Properties.Items.Clear;
  for I := 0 to FIntItems.Count-1 do
    Properties.Items.AddCheckItem (FIntItems[I]);
end;

function TJvDynControlCxCheckComboBox.ControlGetItems: TStrings;
var
  I: Integer;
begin
  FIntItems.Clear;
  for I := 0 to Properties.Items.Count-1 do
    FIntItems.Add(Properties.Items[I].Description);
  Result := FIntItems;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetOnDblClick(Value: TNotifyEvent);
begin
  OnDblClick := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetCxProperties(Value: TCxDynControlWrapper);
begin
  Style.LookAndFeel.Assign(Value.LookAndFeel);
  Style.StyleController := Value.StyleController;
end;

function TJvDynControlCxCheckComboBox.ControlGetDelimiter: string;
begin
  Result := Properties.Delimiter;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetAnchors(Value: TAnchors);
begin
  Anchors := Value;
end;

procedure TJvDynControlCxCheckComboBox.ControlSetDelimiter(Value: string);
begin
  Properties.Delimiter:= Value;
end;


//=== { TJvDynControlEngineDevExpCx } ========================================

constructor TJvDynControlEngineDevExpCx.Create;
begin
  inherited Create;
  FCxProperties := TCxDynControlWrapper.Create;
end;

destructor TJvDynControlEngineDevExpCx.Destroy;
begin
  FreeAndNil(FCxProperties);
  inherited Destroy;
end;

procedure TJvDynControlEngineDevExpCx.SetcxProperties(Value: TCxDynControlWrapper);
begin
  if Value is TCxDynControlWrapper then
  begin
    FCxProperties.LookAndFeel := Value.LookAndFeel;
    FCxProperties.StyleController := Value.StyleController;
  end;
end;

procedure TJvDynControlEngineDevExpCx.RegisterControls;
begin
  RegisterControlType(jctLabel, TJvDynControlCxLabel);
  RegisterControlType(jctStaticText, TJvDynControlCxStaticText);
  RegisterControlType(jctButton, TJvDynControlCxButton);
  RegisterControlType(jctRadioButton, TJvDynControlCxRadioButton);
  RegisterControlType(jctScrollBox, TJvDynControlCxScrollBox);
  RegisterControlType(jctGroupBox, TJvDynControlCxGroupBox);
  RegisterControlType(jctPanel, TJvDynControlCxPanel);
  RegisterControlType(jctImage, TJvDynControlCxImage);
  RegisterControlType(jctCheckBox, TJvDynControlCxCheckBox);
  RegisterControlType(jctComboBox, TJvDynControlCxComboBox);
  RegisterControlType(jctListBox, TJvDynControlCxListBox);
  RegisterControlType(jctCheckListBox, TJvDynControlCxCheckListBox);
  RegisterControlType(jctCheckComboBox, TJvDynControlCxCheckComboBox);
  RegisterControlType(jctRadioGroup, TJvDynControlCxRadioGroup);
  RegisterControlType(jctDateTimeEdit, TJvDynControlCxDateTimeEdit);
  RegisterControlType(jctTimeEdit, TJvDynControlCxTimeEdit);
  RegisterControlType(jctDateEdit, TJvDynControlCxDateEdit);
  RegisterControlType(jctEdit, TJvDynControlCxMaskEdit);
  RegisterControlType(jctCalculateEdit, TJvDynControlCxCalcEdit);
  RegisterControlType(jctSpinEdit, TJvDynControlCxSpinEdit);
  RegisterControlType(jctDirectoryEdit, TJvDynControlCxDirectoryEdit);
  RegisterControlType(jctFileNameEdit, TJvDynControlCxFileNameEdit);
  RegisterControlType(jctMemo, TJvDynControlCxMemo);
  RegisterControlType(jctRichEdit, TJvDynControlCxRichEdit);
  RegisterControlType(jctButtonEdit, TJvDynControlCxButtonEdit);
  RegisterControlType(jctTreeVIew, TJvDynControlCxTreeView);
  RegisterControlType(jctProgressbar, TJvDynControlCxProgressbar);
  RegisterControlType(jctTabControl, TJvDynControlCxTabControl);
  RegisterControlType(jctPageControl, TJvDynControlCxPageControl);
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXVERTICALGRID}
  RegisterControlType(jctRTTIInspector, TJvDynControlCxRTTIInspectorControl);
  {$ELSE}
  //RegisterControlType(jctRTTIInspector, TJvDynControlCxRTTIInspectorControl);
  {$ENDIF}
  RegisterControlType(jctColorComboBox, TJvDynControlCxColorComboBox);
  RegisterControlType(jctStringGrid, TJvDynControlVCLStringGrid);
end;


{$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  IntDynControlEngineDevExpCx := TJvDynControlEngineDevExpCx.Create;
  SetDefaultDynControlEngine(IntDynControlEngineDevExpCx);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}

finalization
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  FreeAndNil(IntDynControlEngineDevExpCx);
  {$ENDIF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
