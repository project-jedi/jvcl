{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvStdCtrlsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvStdCtrlsReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, Controls,
  FiltEdit,
  ImgList,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF HAS_UNIT_SYSTEM_UITYPES}
  DesignEditors, DesignIntf,
  JvDsgnConsts, JvTypes,
  JvRichEdit,
  JvDateTimePicker, JvDatePickerEdit, JvCalendar, JvxSlider, JvTextListBox,
  JvxCheckListBox, JvCoolBar,
  JvHotKey, JvMemo, JvMenus, JvSystemPopup, JvToolBar, JvUpDown,
  JvMonthCalendar, JvListBox, JvScrollBox,
  JvControlPanelButton, JvStartMenuButton, JvRecentMenuButton,
  JvFavoritesButton, JvCheckTreeView,  JvListView,
  JvBrowseFolder,
  JvCombobox, JvColorCombo, JvComCtrls,
  JvSpin, JvEdit, JvProgressBar, JvMaskEdit, JvBaseEdits, JvCalc,
  JvToolEdit, JvBevel, JvCheckBox, JvSpeedButton, JvSecretPanel,
  JvCheckListBox, JvControlBar, JvCtrls, JvGroupBox, JvHeaderControl,
  JvImage, JvLabel, JvRadioButton, JvRadioGroup, JvScrollBar, JvShape,
  JvStaticText, JvStatusBar, JvGrids, JvStringGrid, JvBitBtn, JvPanel, JvImageList,
  JvCheckedItemsForm, JvProgressEditor, JvDsgnEditors, JvCheckedMaskEdit,
  JvIPAddressEditor;

{$R JvStdCtrlsReg.dcr}

procedure Register;
const
  BaseClass: TClass = TComponent;
  cText = 'Text';
  cOwnerDraw = 'OwnerDraw';
begin
  RegisterComponents(RsPaletteVisual, [TJvShape]);
  RegisterComponents(RsPaletteNonVisual, [
    TJvMainMenu, TJvPopupMenu, TJvOfficeMenuItemPainter,TJvBtnMenuItemPainter,
    TJvStandardMenuItemPainter, TJvOwnerDrawMenuItemPainter, TJvXPMenuItemPainter,
    TJvSystemPopup, TJvCalculator]);
  RegisterComponents(RsPaletteDialog, [TJvBrowseForFolderDialog]);
  RegisterComponents(RsPaletteButton, [TJvBitBtn, TJvImgBtn, TJvSpeedButton,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup,
    TJvUpDown, TJvDomainUpDown, TJvControlPanelButton, TJvStartMenuButton,
    TJvRecentMenuButton, TJvFavoritesButton, TJvSpinButton]);
  RegisterComponents(RsPaletteEdit, [TJvEdit,
    TJvMemo, TJvRichEdit, TJvMaskEdit, TJvCheckedMaskEdit, TJvComboEdit, TJvCalcEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvSpinEdit, TJvDatePickerEdit, TJvDateEdit, TJvTimeEdit,
    TJvHotKey, TJvIPAddress]);
  RegisterComponents(RsPaletteImageAnimator, [TJvImage, TJvImageList]);
  RegisterComponents(RsPaletteBarPanel, [TJvPageControl,
    TJvTabControl, TJvTabDefaultPainter,
    TJvProgressBar, TJvGradientProgressBar, TJvStatusBar, TJvToolBar, TJvCoolBar,
    TJvControlBar, TJvGroupBox, TJvHeaderControl, TJvPanel, TJvBevel,
    TJvSecretPanel]);
  RegisterComponents(RsPaletteLabel, [TJvStaticText, TJvLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvComboBox, TJvCheckedComboBox,
    TJvListBox, TJvCheckListBox, TJvTreeView, TJvListView, TJvCheckTreeView,
    TJvColorComboBox, TJvFontComboBox, TJvTextListBox, TJvxCheckListBox,
    TJvDateTimePicker, TJvMonthCalendar, TJvMonthCalendar2,
    TJvDrawGrid, TJvStringGrid]);
  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollBox, TJvScrollBar]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvTrackBar]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvxSlider]);

  RegisterComponentEditor(TJvImageList, TJvImageListEditor);

  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(LongWord), TJvIPAddress, 'Address', TJvIPAddressProperty);
end;

end.