{$I JVCL.INC}

unit JvStdCtrlsReg;

interface

procedure Register;

implementation
uses
  Classes, Controls,
  FiltEdit,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvConsts,
  JvSpin, JvEdit, JvRichEdit, JvProgressBar, JvDateTimePicker, JvDatePickerEdit, JvCheckedMaskEdit,
  JvMaskEdit, JvCalendar, JvBaseEdits, JvCalc, JvToolEdit, JvxCtrls,
  JvxSlider, JvBevel, JvCheckBox, JvCheckListBox, JvCombobox, JvCheckTreeView,
  JvComCtrls, JvControlBar, JvCoolBar, JvCtrls, JvGroupBox, JvHeaderControl,
  JvHotKey, JvImage, JvLabel, JvListView, JvMemo, JvMenus, JvRadioButton, JvRadioGroup,
  JvScrollBar, JvScrollBox, JvShape, JvStaticText, JvStatusBar, JvGrids, JvStringGrid,
  JvSystemPopup, JvTabControl, JvToolBar, JvUpDown, JvBitBtn, JvPanel, JvMonthCalendar,
  JvControlPanelButton, JvStartMenuButton, JvRecentMenuButton, JvFavoritesButton,
  
  JvBrowseFolder, JvTransparentPanel, JvCheckedItemsForm, JvColorCombo,

  JvProgressEditor,
  JvDsgnEditors;


{$R ..\resources\JvStdCtrlsReg.dcr}

procedure Register;
const
  BaseClass:TClass = TComponent;
begin
  RegisterComponents(SPaletteVisual,[TJvShape]);

  RegisterComponents(SPaletteNonVisual,[
    TJvMainMenu, TJvPopupMenu,TJvSystemPopup, TJvCalculator
    ]);
  RegisterComponents(SPaletteDialog,[
    TJvBrowseForFolderDialog
    ]);

  RegisterComponents(SPaletteButton,[
    TJvBitBtn, TJvImgBtn,
    TJvCheckBox, TJvRadioButton, TJvRadioGroup,
    TJvUpDown, TJvDomainUpDown,
    TJvControlPanelButton, TJvStartMenuButton, TJvRecentMenuButton, TJvFavoritesButton
    ]);
  RegisterComponents(SPaletteEdit,[
    TJvEdit, TJvMemo, TJvRichEdit, TJvCheckedMaskEdit,
    TJvMaskEdit, TJvHotKey, TJvCalcEdit, TJvComboEdit, 
    TJvFilenameEdit, TJvDirectoryEdit, TJvDateEdit, TJvSpinEdit,
    TJvIPAddress]);

  RegisterComponents(SPaletteImageAnimator,
    [TJvImage]);

  RegisterComponents(SPaletteBarPanel,[
    TJvPageControl,  TJvProgressBar,
    TJvStatusBar, TJvTabControl, TJvToolBar,
    TJvControlBar, TJvCoolBar,
    TJvGroupBox, TJvHeaderControl,
    TJvPanel, TJvBevel {TJvTransparentPanel}
    ]);

  RegisterComponents(SPaletteLabel,[
    TJvLabel, TJvStaticText]);

  RegisterComponents(SPaletteListComboTree,[
    TJvComboBox,TJvListBox, TJvCheckListBox,
    TJvTreeView, TJvListView, TJvCheckTreeView,
    TJvColorComboBox, TJvFontComboBox,
    { TJvTextListBox, TJvxCheckListBox,}
    TJvDateTimePicker, TJvMonthCalendar, {TJvMonthCalendar2,}
    TJvDrawGrid, TJvStringGrid
    ]);
  RegisterComponents(SPaletteScrollerTracker,[
    TJvScrollBox, TJvTrackBar
    ]);
  RegisterComponents(SPaletteSliderSplitter,[
    TJvxSlider
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
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedButton, 'Caption', TJvHintProperty);

end;

end.
