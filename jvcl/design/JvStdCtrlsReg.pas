{$I JVCL.INC}

unit JvStdCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, Controls, DesignIntf, DesignEditors, FiltEdit,  
  JvSpin, JvEdit, JvRichEdit, JvProgressBar, JvDateTimePicker, JvDatePickerEdit, JvCheckedMaskEdit,
  JvMaskEdit, JvCalendar, JvBaseEdits, JvCalc, JvToolEdit, JvxCtrls,
  JvxSlider, JvBevel, JvCheckBox, JvCheckListBox, JvCombobox,
  JvComCtrls, JvControlBar, JvCoolBar, JvCtrls, JvGroupBox, JvHeaderControl,
  JvHotKey, JvImage, JvLabel, JvListView, JvMemo, JvMenus, JvRadioButton, JvRadioGroup,
  JvScrollBar, JvScrollBox, JvShape, JvStaticText, JvStatusBar, JvStringGrid,
  JvSystemPopup, JvTabControl, JvToolBar, JvUpDown, JvBitBtn, JvPanel, JvMonthCalendar,
  JvFindReplace, JvControlPanelButton, JvStartMenuButton, JvRecentMenuButton, JvFavoritesButton,
  
  JvBrowseFolder, JvTransparentPanel, JvCheckedItemsForm, JvColorCombo,

  JvProgressEditor,
  JvDsgnEditors;


{.$R ..\resources\JvStdCtrlsReg.dcr}

procedure Register;
const
  BaseClass:TClass = TComponent;
begin
  RegisterComponents('Jv Standard',[
    TJvMainMenu, TJvPopupMenu, TJvSystemPopup, TJvEdit, TJvCheckedMaskEdit,
    TJvMaskEdit, TJvCalcEdit, TJvCalculator, TJvComboEdit, TJvFileDirEdit,
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvSpinEdit,
    TJvMemo, TJvPanel, TJvxSlider, TJvBevel, TJvStringGrid,
    // TJvTextListBox, TJvxCheckListBox, TJvTransparentPanel,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup, TJvCheckListBox, TJvComboBox,
    TJvColorComboBox, TJvFontComboBox, TJvListBox, TJvBitBtn, TJvImgBtn, TJvScrollBox, TJvShape,

    TJvStaticText, TJvIPAddress, TJvPageControl, TJvTreeView, TJvTrackBar,
    TJvControlBar, TJvCoolBar, TJvHotKey, TJvListView, TJvRichEdit, TJvProgressBar,
    TJvDateTimePicker, TJvMonthCalendar, {TJvMonthCalendar2,} TJvStatusBar, TJvTabControl, TJvToolBar,
    TJvUpDown, TJvDomainUpDown, TJvBrowseForFolderDialog,

    TJvControlPanelButton, TJvStartMenuButton, TJvRecentMenuButton, TJvFavoritesButton,
    TJvGroupBox, TJvHeaderControl, TJvImage, TJvLabel, TJvFindReplace
    ]);

  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'Gauge', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(TControl), BaseClass, 'ProgressBar', TJvProgressControlProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomNumEdit, 'Text', nil);
  RegisterPropertyEditor(TypeInfo(string), TJvFileDirEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomDateEdit, 'Text', TStringProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFilenameEdit, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDirectoryEdit, 'Text', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomComboEdit, 'ButtonHint', TJvHintProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxCheckListBox, 'Items', TJvCheckItemsProperty);

end;

end.
