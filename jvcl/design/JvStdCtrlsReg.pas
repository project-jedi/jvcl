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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvStdCtrlsReg;

interface

procedure Register;

implementation

uses
  Classes, Controls,
  FiltEdit, ImgList, 
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts, JvTypes,
  JvSpin, JvEdit, JvRichEdit, JvProgressBar, JvDateTimePicker, JvDatePickerEdit,
  JvCheckedMaskEdit, JvMaskEdit, JvCalendar, JvBaseEdits, JvCalc, JvToolEdit,
  JvxSlider, JvBevel, JvCheckBox, JvSpeedButton, JvTextListBox, JvSecretPanel, 
  JvxCheckListBox, JvCheckListBox, JvCombobox, JvCheckTreeView, JvComCtrls,
  JvControlBar, JvCoolBar, JvCtrls, JvGroupBox, JvHeaderControl, JvHotKey,
  JvImage, JvLabel, JvListView, JvMemo, JvMenus, JvRadioButton, JvRadioGroup,
  JvScrollBar, JvScrollBox, JvShape, JvStaticText, JvStatusBar, JvGrids,
  JvStringGrid, JvSystemPopup, JvToolBar, JvUpDown, JvBitBtn,
  JvPanel, JvMonthCalendar, JvControlPanelButton, JvStartMenuButton,
  JvRecentMenuButton, JvFavoritesButton, JvImageList, JvListBox, JvBrowseFolder,
  JvTransparentPanel, JvCheckedItemsForm, JvColorCombo, 
  JvProgressEditor, JvDsgnEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvStdCtrlsReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvStdCtrlsReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  BaseClass: TClass = TComponent;
  cText = 'Text';
  cOwnerDraw = 'OwnerDraw';
begin
  RegisterComponents(RsPaletteVisual, [TJvShape]);
  RegisterComponents(RsPaletteNonVisual, [TJvMainMenu, TJvPopupMenu,
    TJvOfficeMenuItemPainter,TJvBtnMenuItemPainter, TJvStandardMenuItemPainter,
    TJvOwnerDrawMenuItemPainter, TJvXPMenuItemPainter,
    TJvSystemPopup, TJvCalculator]);
  RegisterComponents(RsPaletteDialog, [TJvBrowseForFolderDialog]);
  RegisterComponents(RsPaletteButton, [TJvBitBtn, TJvImgBtn, TJvSpeedButton,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup, TJvUpDown, TJvDomainUpDown,
    TJvControlPanelButton, TJvStartMenuButton, TJvRecentMenuButton,
    TJvFavoritesButton, TJvSpinButton]);
  RegisterComponents(RsPaletteEdit, [TJvEdit, TJvMemo, TJvRichEdit,
    TJvCheckedMaskEdit, TJvMaskEdit, TJvHotKey, TJvCalcEdit, TJvComboEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvDatePickerEdit,
    TJvSpinEdit, TJvIPAddress]);
  RegisterComponents(RsPaletteImageAnimator, [TJvImage, TJvImageList]);
  RegisterComponents(RsPaletteBarPanel, [
    TJvPageControl, TJvTabControl, TJvTabDefaultPainter,   
    TJvProgressBar, TJvStatusBar, TJvToolBar, TJvControlBar, TJvCoolBar,
    TJvGroupBox, TJvHeaderControl, TJvPanel, TJvBevel,
    TJvSecretPanel {, TJvTransparentPanel}]);
  RegisterComponents(RsPaletteLabel, [TJvLabel, TJvStaticText]);
  RegisterComponents(RsPaletteListComboTree, [TJvComboBox,TJvListBox,
    TJvCheckListBox, TJvTreeView, TJvListView, TJvCheckTreeView,
    TJvColorComboBox, TJvFontComboBox, TJvTextListBox, TJvxCheckListBox,
    TJvDateTimePicker, TJvMonthCalendar, {TJvMonthCalendar2,}
    TJvDrawGrid, TJvStringGrid]);
  RegisterComponents(RsPaletteScrollerTracker, [TJvScrollBar, TJvScrollBox]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvTrackBar,TJvxSlider]);

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
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
end;

end.
