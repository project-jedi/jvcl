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

unit JvStdCtrlsReg;

{$I jvcl.inc}

{$IFDEF MSWINDOWS}
{$DEFINE USEWINDOWS}
{$ENDIF MSWINDOWS}

interface

procedure Register;

implementation

uses
  Classes, Controls,
  {$IFDEF VCL}
  FiltEdit,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QTypes, // type TCaption
  {$ENDIF VisualCLX}
  ImgList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDsgnConsts, JvTypes,
  {$IFDEF VCL}
  JvRichEdit, JvStaticText,
  JvDateTimePicker, JvDatePickerEdit, JvCalendar, JvxSlider, JvTextListBox,
  JvxCheckListBox, JvCoolBar,
  JvHotKey, JvMemo, JvMenus, JvSystemPopup, JvToolBar, JvUpDown,
  JvMonthCalendar, JvListBox, JvScrollBox,
  JvControlPanelButton, JvStartMenuButton, JvRecentMenuButton,
  JvFavoritesButton, JvCheckTreeView,  JvListView,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  JvQGauges, JvQStdDsgnEditors, QComCtrlsEx,
  {$ENDIF VisualCLX}
  {$IFDEF USEWINDOWS}
  JvBrowseFolder,
  {$ENDIF USEWINDOWS}
  JvCombobox, JvColorCombo, JvComCtrls,
  JvSpin, JvEdit, JvProgressBar, JvMaskEdit, JvBaseEdits, JvCalc,
  JvToolEdit, JvBevel, JvCheckBox, JvSpeedButton, JvSecretPanel,
  JvCheckListBox, JvControlBar, JvCtrls, JvGroupBox, JvHeaderControl,
  JvImage, JvLabel, JvRadioButton, JvRadioGroup, JvScrollBar, JvShape,
  JvStatusBar, JvGrids, JvStringGrid, JvBitBtn, JvPanel, JvImageList,
  JvTransparentPanel, JvCheckedItemsForm, JvProgressEditor, JvDsgnEditors,
  JvCheckedMaskEdit;

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
  RegisterComponents(RsPaletteNonVisual, [
    {$IFDEF VCL}
    TJvMainMenu, TJvPopupMenu,
    TJvOfficeMenuItemPainter,TJvBtnMenuItemPainter, TJvStandardMenuItemPainter,
    TJvOwnerDrawMenuItemPainter, TJvXPMenuItemPainter,
    TJvSystemPopup,
    {$ENDIF VCL}
    TJvCalculator]);
  RegisterComponents(RsPaletteDialog, [TJvBrowseForFolderDialog]);
  RegisterComponents(RsPaletteButton, [TJvBitBtn, TJvImgBtn, TJvSpeedButton,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup,
    {$IFDEF VCL}
    TJvUpDown, TJvDomainUpDown, TJvControlPanelButton, TJvStartMenuButton,
    TJvRecentMenuButton, TJvFavoritesButton,
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    TUpDown,
    {$ENDIF VisualCLX}
    TJvSpinButton]);
  RegisterComponents(RsPaletteEdit, [TJvEdit,
    {$IFDEF VCL}
    TJvMemo,
    TJvRichEdit,
    {$ENDIF VCL}
    TJvMaskEdit, TJvCheckedMaskEdit, TJvComboEdit, TJvCalcEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvSpinEdit,
    {$IFDEF VCL}
    TJvHotKey,
    TJvIPAddress,
    TJvDatePickerEdit,
    {$ENDIF VCL}
    TJvDateEdit]);
  RegisterComponents(RsPaletteImageAnimator, [TJvImage, TJvImageList]);
  RegisterComponents(RsPaletteBarPanel, [TJvPageControl,
    TJvTabControl, TJvTabDefaultPainter,
    {$IFDEF VisualCLX}
    TJvGauge,
    {$ENDIF VisualCLX}
    TJvProgressBar, TJvStatusBar,
    {$IFDEF VCL}
    TJvToolBar, TJvCoolBar,
    {$ENDIF VCL}
    TJvControlBar,
    TJvGroupBox, TJvHeaderControl, TJvPanel, TJvBevel,
    TJvSecretPanel {, TJvTransparentPanel}]);
  RegisterComponents(RsPaletteLabel, [
    {$IFDEF VCL}
    TJvStaticText,
    {$ENDIF VCL}
    TJvLabel]);
  RegisterComponents(RsPaletteListComboTree, [TJvComboBox, TJvCheckedComboBox, 
    {$IFDEF VCL}
    TJvListBox,
    {$ENDIF VCL}
    TJvCheckListBox,
    {$IFDEF VCL}
    TJvTreeView, TJvListView, TJvCheckTreeView,
    {$ENDIF VCL}
    TJvColorComboBox,
    {$IFDEF VCL}
    TJvFontComboBox, TJvTextListBox, TJvxCheckListBox,
    TJvDateTimePicker,
    TJvMonthCalendar, {TJvMonthCalendar2,}
    {$ENDIF VCL}
    TJvDrawGrid, TJvStringGrid]);
  RegisterComponents(RsPaletteScrollerTracker, [
    {$IFDEF VCL}
    TJvScrollBox,
    {$ENDIF VCL}
    TJvScrollBar]);
  RegisterComponents(RsPaletteSliderSplitter, [TJvTrackBar]);
  {$IFDEF VCL}
  RegisterComponents(RsPaletteSliderSplitter, [TJvxSlider]);
  {$ENDIF VCL}

  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, cText, nil);
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, cText, TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, cText, TStringProperty);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  {$ENDIF VCL}
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, cText, TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, cOwnerDraw, nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, cOwnerDraw, nil);
  {$ENDIF VCL}
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvCustomLabel, 'ImageIndex',TJvDefaultImageIndexProperty);
end;

end.
