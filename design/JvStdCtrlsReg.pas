{$I JVCL.INC}

unit JvStdCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes,
  JvSpin, JvEdit, JvRichEdit, JvProgressBar, JvDateTimePicker, JvDatePickerEdit, JvCheckedMaskEdit,
  JvMaskEdit, JvCalendar, JvBaseEdits, JvCalc, JvToolEdit, JvxCtrls,
  JvxSlider, JvBevel, JvCheckBox, JvCheckListBox, JvCombobox,
  JvComCtrls, JvControlBar, JvCoolBar, JvCtrls, JvGroupBox, JvGroupHeader, JvHeaderControl,
  JvHotKey, JvImage, JvLabel, JvListView, JvMemo, JvMenus, JvRadioButton, JvRadioGroup,
  JvScrollBar, JvScrollBox, JvShape, JvStaticText, JvStatusBar, JvStringGrid,
  JvSystemPopup, JvTabControl, JvToolBar, JvUpDown, JvBitBtn, JvPanel, JvMonthCalendar,
  JvFindReplace, JvControlPanelButton, JvStartMenuButton, JvRecentMenuButton, JvFavoritesButton,
  
  JvBrowseFolder, JvTransparentPanel, JvColorCombo;


{.$R ..\resources\JvStdCtrlsReg.dcr}

procedure Register;
begin
  RegisterComponents('Jv Standard',[
    TJvMainMenu, TJvPopupMenu, TJvSystemPopup, TJvEdit, TJvCheckedMaskEdit,
    TJvMaskEdit, TJvCalcEdit, TJvCalculator, TJvComboEdit, TJvFileDirEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvSpinEdit,
    TJvMemo, TJvPanel, {TJvTransparentPanel, }TJvxSlider, TJvBevel, TJvStringGrid,
    // TJvTextListBox, TJvxCheckListBox,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup, TJvCheckListBox, TJvComboBox,
    TJvColorComboBox, TJvFontComboBox, TJvListBox, TJvBitBtn, TJvImgBtn, TJvScrollBox, TJvShape,

    TJvStaticText, TJvIPAddress, TJvPageControl, TJvTreeView, TJvTrackBar,
    TJvControlBar, TJvCoolBar, TJvHotKey, TJvListView, TJvRichEdit, TJvProgressBar,
    TJvDateTimePicker, TJvMonthCalendar, {TJvMonthCalendar2,} TJvStatusBar, TJvTabControl, TJvToolBar,
    TJvUpDown, TJvDomainUpDown, TJvBrowseForFolderDialog,

    TJvControlPanelButton, TJvStartMenuButton, TJvRecentMenuButton, TJvFavoritesButton,
    TJvGroupBox, TJvGroupHeader, TJvHeaderControl, TJvImage, TJvLabel, TJvFindReplace
    ]);
end;

end.
