{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCatReg.pas, released on 2000-09-22.

The Initial Developer of the Original Code is Peter Thörnqvist (peter3@peter3.com) .
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

Last Modified: 2002-08-25

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Registration unit for OI category support in D6

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCatReg;

interface

procedure Register;

implementation
uses
  DesignIntf, JvTypes, JvAlarms, JVCLVer, JvComponent,
  JvTransBtn,JvTransBtn2,
  JvMemo, JvVCLUtils, JvBlinkingLabel, JvBitBtn,
  JvItemsSearchs, JvButton, JvButtonPersistent,
  JvPopupMemo, JvVerInf, JvCommonDialogD,
  JvCaptionPanel, JvTimer, JvCurrEdit, JvColorBox,
  JvCntScr, JvDialogs, JvLinkLabel, JvCmdEdit, JvSpecialImage,
  JvDateTimePicker, JvBouncingLabel, JvBMPListBox, JvCheckListBox,
  JvInstallLabel, JvDrawGrid, JvLabel, JvStringGrid, JvRealLabel,
  JvTabControl, JvAppInfo, JvMenus, JvFloatEdit,
  JvProgressBar, JvPlaylist, JvCheckBox,
  JvWinampLabel, JvGroupBox, JvImage, JvListComb, JvMovableBevel, Jvgrids, JvToolEdit, JvRadioCtl,
  JvComCtrls, JvMLButton, JvMaskEdit, JvHotLink, JvMaxPixel, JvUCB, JvMousePanel,
  JvMultiselectChecklistbox, JvMultilineListbox, JvObservibleCheckBox, JvButtonShaped,
  JvArrowBtn, JvRegTV, JvSpinEdit, JvPlacemnt,
  JvHotkeyEx, JvSyncSplitter, JvPropAutoSave, JvRadioButton, JvRadioGroup,
  {**}
  JvUpDown, JvScrollBox, JvRichEdit, JvControlBar, JvxCtrls, JvScrollBar,
  JvScrollingLabel, JvListView, JvColorCombo,
  JvSpacer, JvSpeedButton, JvSplitter, JvStaticText,
  JvRichEd, JvxAnimate, JvSpecialLabel,
  JvTypedEdit, JvAppearingLabel, JvSpin, JvxSlider, JvCombobox, JvCtrls,
  JvEdit, JvDriveCtrls, JvSplit,
  {$IFNDEF D6PersonalEdition}
    JvDBSpinEdit ,JvDBDateTimePicker, JvDBProgressBar, JvDBCtrl,
    JvDBQBE, JvDBRichEd, JvDBComb, JvDBIndex;
  {$ENDIF}


const
  cJediCat = 'JEDI VCL';

procedure Register;
begin
  // global
  RegisterPropertiesInCategory(cJediCat, ['HintColor', 'OnMouseEnter', 'OnMouseLeave', 'OnCtl3DChanged', 'OnParentColorChange']);
  // JvVCLVer
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJVCLAboutInfo), ['']);
  // JvAlarms
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvTriggerKind), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvOnAlarm), ['']);
  // JvMemo
  RegisterPropertiesInCategory(cJediCat, TJvCustomMemo, ['AutoSize','HideCaret', 'MaxLines', 'HotTrack',
    'HintColor', 'Lines', 'ReadOnly', 'OnVerticalScroll', 'OnHorizontalScroll']);
  // JvVCLUtils
  RegisterPropertiesInCategory(cJediCat, typeinfo(TFillDirection), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TVertAlignment), ['']);
  // JvBlinkingLabel
  RegisterPropertiesInCategory(cJediCat, TJvBlinkingLabel, ['Blinking', 'BlinkingDelay', 'BlinkingTime']);
  // JvBitBtn
  RegisterPropertiesInCategory(cJediCat, TJvBitBtn, ['HotTrack', 'HotTrackFont', 'HotGlyph', 'DropDownMenu']);
  // JvMaxPixel
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvMaxPixel), ['']);
  // JvPropAutoSave
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvAutoSave), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvRegAutoSave), ['']);
  // JvItemsSearchs
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvItemsSearchs), ['']);
  // JvButton
  RegisterPropertiesInCategory(cJediCat, TJvButton, ['DropDownMenu', 'HotTrack', 'HotTrackFont']);
  // JvButtonPersistent
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvButtonPersistent), ['']);
  // JvPopUpMemo
  RegisterPropertiesInCategory(cJediCat, TJvPopupMemo, ['MaximumHeight']);
  // JvVerInf
  RegisterPropertiesInCategory(cJediCat, typeinfo(TVersionLanguage), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TVersionCharSet), ['']);
  // JvRichEd
  RegisterPropertiesInCategory(cJediCat, TJvCustomRichEdit,['AllowInPlace','AllowObjects','AutoURLDetect','AutoVerbMenu',
    'HideScrollBars','Title','LangOptions','SelectionBar','StreamFormat',
    'StreamMode','UndoLimit','WordSelection','OnSaveClipboard','OnSelectionChange','OnProtectChange',
    'OnProtectChangeEx','OnResizeRequest','OnURLClick','OnTextNotFound','OnCloseFindDialog',
    'DefaultJvConverter','WordAttributes','PageRect','Paragraph','SelectionType']);
  // JvCommonDialogD
  RegisterPropertiesInCategory(cJediCat, TJvCommonDialogD,['DialogTitle']);
  // JVCaptionPanel
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCapBtnStyle),['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCapBtnStyles),['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvDrawPosition),['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCapBtnEvent),['']);
  RegisterPropertiesInCategory(cJediCat, TJvCaptionPanel,['AutoDrag','Buttons','CaptionColor','CaptionPosition','CaptionFont',
    'FlatButtons','OnButtonClick','OnEndAutoDrag']);
  // JvTimer
  RegisterPropertiesInCategory(cJediCat, TJvTimer,['SyncEvent','Threaded','ThreadPriority']);
  // JvCurrEdit
  RegisterPropertiesInCategory(cJediCat, TJvCustomNumEdit,['Formatting','Alignment','BeepOnError','CheckOnExit',
    'GlyphKind','ButtonWidth','DecimalPlaces','DisplayFormat','MaxValue','MinValue','FormatOnEditing',
    'ZeroEmpty','AsInteger','DisplayText','Value']);
  // JvColorBox
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvColorClickEvent),['']);
  // JvCntScr
  RegisterPropertiesInCategory(cJediCat, TJvContentScroller,['Active','ScrollAmount','ScrollIntervall','ScrollLength',
    'ScrollDirection','MediaFile','LoopMedia','LoopCount','OnAfterScroll','OnBeforeScroll']);
  // JVDialogs
  RegisterPropertiesInCategory(cJediCat, TJvOpenDialog,['ActiveControl','ActiveStyle','AutoSize','DefBtnCaption',
    'FilterLabelCaption','Height','ShowPlacesBar','UseUserSize','Width','OnShareViolation']);
  RegisterPropertiesInCategory(cJediCat, TJvColorDialog,['OnQueryColor']);
  // JvLinkLabel
  RegisterPropertiesInCategory(cJediCat, TJvLinkLabel,['OnDynamicTagInit','OnCaptionChanged','OnLinkClick',
    'Caption','Text','LinkColor','LinkColorClicked','LinkColorHot','LinkStyle','HotLinks','AutoHeight',
    'MarginWidth','MarginHeight']);
  // JvCmdEdit
  RegisterPropertiesInCategory(cJediCat, TJvCustomCommandEdit,['CurrCmd','Prompt','History','Commands','ExecuteKey',
    'OnCommand','ArrowKeys']);
  // JvSpecialImage
  RegisterPropertiesInCategory(cJediCat, TJvSpecialImage,['Brightness','Inverted','Flipped','Mirrored','Picture']);
  // JvDateTimePicker
  RegisterPropertiesInCategory(cJediCat, TJvDateTimePicker,['DropDownDate','NullDate','NullText']);
  // JvBouncingLabel
  RegisterPropertiesInCategory(cJediCat, TJvBouncingLabel,['Bouncing','Interval']);
  // JvBMPListBox
  RegisterPropertiesInCategory(cJediCat, TJvBMPListBox,['Background','BackgroundFillmode']);
  // JvCheckListBox
  RegisterPropertiesInCategory(cJediCat, TJvCheckListBox,['HotTrack','HorScrollbar','OnSelectCancel','OnVerticalScroll','OnHorizontalScroll']);
  // JvInstallLabel
  RegisterPropertiesInCategory(cJediCat, TJvInstallLabel,['DefaultImage','ImageList','Lines','LineSpacing','TextOffset','ImageOffset']);
  // JvDrawGrid
  RegisterPropertiesInCategory(cJediCat, TJvDrawGrid,['OnVerticalScroll','OnHorizontalScroll']);
  // JvLabel
  RegisterPropertiesInCategory(cJediCat, TJvLabel,['HotTrack','HotTrackFont']);
  // JvStringGrid
  RegisterPropertiesInCategory(cJediCat, TJvStringGrid,['InplaceEditor','OnExitCell','Alignment','OnSetCanvasProperties','OnGetCellAlignment',
    'OnCaptionClick','OnLoadProgress','OnSaveProgress','OnVerticalScroll','OnHorizontalScroll']);
  // JvRealLabel
  RegisterPropertiesInCategory(cJediCat, TJvRealLabel,['Text','SleepTime','MakeErrors']);
  // JvTabControl
  RegisterPropertiesInCategory(cJediCat, TJvTabControl,['Color']);
  // JvAppInfo
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvAppInfo),['']);
  // JvMenus
  RegisterPropertiesInCategory(cJediCat, TJvMainMenu,['Cursor','MinTextOffset','Style','ShowCheckMarks','OwnerDraw','Images',
    'OnGetImageIndex','OnDrawItem','OnGetItemParams','OnMeasureItem']);
  RegisterPropertiesInCategory(cJediCat, TJvPopupMenu,['Cursor','LeftMargin','MinTextOffset','Style','ShowCheckMarks',
    'OwnerDraw','Images','OnGetImageIndex','OnDrawItem','OnDrawMargin','OnGetItemParams','OnMeasureItem']);
  // JvFloatEdit
  RegisterPropertiesInCategory(cJediCat, TJvFloatEdit,['Value']);
  // JvProgressBar
  RegisterPropertiesInCategory(cJediCat, TJvProgressBar,['BarColor']);
  // JvPlayList
  RegisterPropertiesInCategory(cJediCat, TJvPlayList,['ShowNumbers','ShowExtension','Items']);
  // JvCheckBox
  RegisterPropertiesInCategory(cJediCat, TJvCheckBox,['Associated','AutoSize','HotTrack','HotTrackFont','OnRestored']);
  // JvWinampLabel
  RegisterPropertiesInCategory(cJediCat, TJvWinampLabel,['Active','Stretch','ScrollBy','ScrollInterval','WaitOnEnd','Skin']);
  // JvGroupBox
  RegisterPropertiesInCategory(cJediCat, TJvGroupBox,['PropagateEnable','OnHotkey']);
  // JvImage
  RegisterPropertiesInCategory(cJediCat, TJvImage,['Pictures','Picture','State','OnStateChanged']);
  // JvListComb
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvButtonColors),['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvListItems),['']);
  RegisterPropertiesInCategory(cJediCat, TJvImageComboBox,['DefaultIndent','DroppedWidth','ButtonFrame','ButtonStyle',
    'ColorHighLight','ColorHighLightText','ImageList','OnChange']);
  RegisterPropertiesInCategory(cJediCat, TJvImageListBox,['Alignment','ButtonFrame','ButtonStyle','ColorHighLight',
    'ColorHighLightText','ImageList']);
  // JvMovableBevel
  RegisterPropertiesInCategory(cJediCat, TJvMovableBevel,['BorderSize']);
  // JvGrids
  RegisterPropertiesInCategory(cJediCat, TJvxDrawGrid,['IniStorage','FixedButtons','OnAcceptEditKey','OnCancelEdit',
    'OnCheckButton','OnChangeFocus','OnFixedCellClick','OnColumnSized','OnRowSized','OnGetEditLimit','OnEditChange',
    'OnShowEditor','OnGetEditAlign','OnGetEditStyle','OnGetPicklist','OnEditButtonClick']);
  // JvRadioCtl
  RegisterPropertiesInCategory(cJediCat, TJvCustomRadioControl,['Angle','BorderStyle','ButtonEdge','DefaultPos','Frequency',
    'LargeChange','Max','MaxAngle','Min','MinAngle','PointerColor','PointerSize','PointerShape','Position','Radius',
    'RepeatDelay','RepeatRate','SmallChange','TickStyle','OnChange','OnDrawPointer','Bitmap','Center']);
  // JvComCtrls
  RegisterPropertiesInCategory(cJediCat, TJvPageControl,['ClientBorderWidth','DrawTabShadow','HideAllTabs']);
  RegisterPropertiesInCategory(cJediCat, TJvTrackBar,['ShowRange','ToolTips','ToolTipSide','OnChanged','OnToolTip']);
  RegisterPropertiesInCategory(cJediCat, TJvTreeView,['ScrollDirection','Checkboxes','OnVerticalScroll','OnHorizontalScroll',
    'PageControl','OnPageChanged','AutoDragScroll','MultiSelect','OnCustomDrawItem','OnEditCancelled','OnSelectionChange']);
  // JvMLButton
  RegisterPropertiesInCategory(cJediCat, TJvMultilineButton,['Multiline']);
  // JvMaskEdit
  RegisterPropertiesInCategory(cJediCat, TJvMaskEdit,['HotTrack']);
  // JvHotLink
  RegisterPropertiesInCategory(cJediCat, TJvHotLink,['URL']);
  // JvUCB
  RegisterPropertiesInCategory(cJediCat, TJvUninstallComboBox,['Section','UninstallString','DisplayName','ShowAll','DisplayMode']);
  RegisterPropertiesInCategory(cJediCat, TJvUninstallListBox,['Section','UninstallString','DisplayName','ShowAll','DisplayMode']);
  // JvMousePanel
  RegisterPropertiesInCategory(cJediCat, TJvMousePanel,['MouseOverColor']);
  // JvMultiselectChecklistbox
  RegisterPropertiesInCategory(cJediCat, TJvMultiselectChecklistbox,['Multiselect']);
  // JvMultilineListbox
  RegisterPropertiesInCategory(cJediCat, TJvMultilineListbox,['Alignment','Multiline','SelectedColor','SelectedTextColor',
    'DisabledTextColor','ShowFocusRect']);
  // JvObservibleCheckBox
  RegisterPropertiesInCategory(cJediCat, TJvObservibleCheckBox,['Observer']);
  // JvButtonShaped
  RegisterPropertiesInCategory(cJediCat, TJvButtonShaped,['Shape']);
  // JvArrowBtn
  RegisterPropertiesInCategory(cJediCat, TJvArrowButton,['ArrowWidth','DropDown','FillFont','PressBoth','OnDrop']);
  // JvRegTV
  RegisterPropertiesInCategory(cJediCat, TJvRegistryTreeView,['CurrentPath','ShortPath','CurrentKey','RegistryKeys','ListView',
    'RootCaption','DefaultCaption','DefaultNoValueCaption']);
  // JvSpinEdit
  RegisterPropertiesInCategory(cJediCat, TJvSpinEdit,['Max','Min','Value','Thousands','Alignment','HotTrack']);


  // JvToolEdit
  RegisterPropertiesInCategory(cJediCat, typeinfo(TCloseUpEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TFileExt), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TPopupAlign), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TGlyphKind), ['']);
  RegisterPropertiesInCategory(cJediCat, TJvCustomComboEdit, ['Alignment', 'AlwaysEnable', 'Button', 'ClickKey',
    'Glyph', 'GlyphKind', 'ButtonWidth', 'NumGlyphs', 'ButtonHint', 'DirectInput', 'ReadOnly', 'PopupAlign', 'PopupVisible',
      'OnButtonClick', 'ButtonFlat']);
  RegisterPropertiesInCategory(cJediCat, TJvCustomDateEdit, ['BlanksChar', 'CalendarHints', 'CheckOnExit', 'DefaultToday',
    'DialogTitle', 'EditMask', 'Formatting', 'GlyphKind', 'PopupColor', 'CalendarStyle', 'StartOfWeek', 'Weekends', 'WeekendColor',
      'YearDigits', 'OnAcceptDate', 'MaxLength', 'Text', 'Date', 'PopupVisible']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDirDialogKind), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TFileDialogKind), ['']);
  // JvPlacemnt
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvFormPlacement), ['']);
  // JvTransBtn
  RegisterPropertiesInCategory(cJediCat, TJvTransparentButton,['AutoGray','BorderWidth','FrameStyle',
    'Offset','DropDownMenu','ShowPressed','TextAlign','WordWrap','OnMouseExit']);

  // JvTransBtn2
  RegisterPropertiesInCategory(cJediCat, TJvTransparentButton2,['AutoGray','BorderWidth','FrameStyle',
    'Offset','DropDownMenu','ShowPressed','TextAlign','WordWrap',
    'HiFont','FrameStyle','ActiveImage','ActiveIndex','GrayImage','GrayIndex',
    'DisabledImage','DisabledIndex','DownImage','DownIndex','ShowPressed','TextAlign',
    'WordWrap','OnMouseExit']);

  // JvComponent
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvComponent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvGraphicControl), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCustomPanel), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCustomControl), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvWinControl), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCustomMemo), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvCaret), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvClipBoardCommand), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvClipBoardCommands), ['']);

  // JvTransBtn
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvFrameStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvButtonState), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvTextAlign), ['']);
  // JvHotkeyEx
  RegisterPropertiesInCategory(cJediCat, TJvHotKeyEx,['Shortcut']);
  // JvSyncSplitter
  RegisterPropertiesInCategory(cJediCat, TJvSyncSplitter,['Partner']);
  // JvRadioButton
  RegisterPropertiesInCategory(cJediCat, TJvRadioButton,['HotTrack','HotTrackFont','OnRestored']);
  // JvRadioGroup
  RegisterPropertiesInCategory(cJediCat, TJvRadioGroup,['OnRestored']);

  // JvUpDown
  RegisterPropertiesInCategory(cJediCat, TJvUpDown,['AlignButton','Color','HotTrack']);
  // JvScrollBox
  RegisterPropertiesInCategory(cJediCat, TJvScrollBox,['HotTrack','OnVerticalScroll','OnHorizontalScroll','OnKeyDown','OnKeyPress','OnKeyUp','TabStop']);
  // JvRichEdit
  RegisterPropertiesInCategory(cJediCat, TJvRichEdit,['OnVerticalScroll','OnHorizontalScroll']);
  // JvControlBar
  RegisterPropertiesInCategory(cJediCat, TJvControlBar,['PopupControl','PopupNames']);

  // JvxCtrls
  RegisterPropertiesInCategory(cJediCat, TJvTextListBox,['TabWidth']);
  RegisterPropertiesInCategory(cJediCat, TJvxCheckListBox,['AllowGrayed','CheckKind','CheckedIndex',
    'AutoScroll','GraySelection','OnGetItemWidth','TabStop','OnStateChange','OnClickCheck']);
  RegisterPropertiesInCategory(cJediCat, TJvxLabel,['ShadowColor','ShadowSize','ShadowPos','ShowFocus']);
  RegisterPropertiesInCategory(cJediCat, TJvSecretPanel,['Active','Cycled','Glyph','GlyphLayout',
    'Interval','Lines','ScrollDirection','TextStyle','OnPaintClient','OnStartPlay','OnStopPlay']);
  RegisterPropertiesInCategory(cJediCat, TJvxSpeedButton,['AllowTimer','DropDownMenu','MenuPosition',
    'GrayedInactive','InitPause','MarkDropDown','ModalResult','RepeatInterval','WordWrap']);
  // JvScrollBar
  RegisterPropertiesInCategory(cJediCat, TJvScrollBar,['HotTrack']);
  // JvScrollingLabel
  RegisterPropertiesInCategory(cJediCat, TJvScrollingLabel,['ScrollInterval','Scrolling','NoGrap','Text','ScrollDirection']);
  // JvListView
  RegisterPropertiesInCategory(cJediCat, TJvListView,['AutoClipboardCopy','AutoClipboardCopy','OnLoadProgress',
    'OnSaveProgress','OnAutoSort','OnVerticalScroll','OnHorizontalScroll']);
  // JvColorCombo
  RegisterPropertiesInCategory(cJediCat, TJvColorComboBox,['ColorValue','ColorDialogText','ColorWidth',
    'NewColorText','Options','DroppedDownWidth','HiliteColor','HiliteText','OnNewColor']);
  RegisterPropertiesInCategory(cJediCat, TJvFontComboBox,['FontName','Device','HiliteColor','HiliteText',
    'Options','UseImages']);
  // JvSpacer
  RegisterPropertiesInCategory(cJediCat, TJvSpacer,['Spacing']);
  // JvSpeedButton
  RegisterPropertiesInCategory(cJediCat, TJvSpeedButton,['HotTrack','HotTrackFont','HotGlyph','DropDownMenu',
    'ModalResult']);
  // JvSplitter
  RegisterPropertiesInCategory(cJediCat, TJvSplitter,['ShowHint']);
  // JvStaticText
  RegisterPropertiesInCategory(cJediCat, TJvStaticText,['HotTrack','HotTrackFont']);
  // JvRichEd
  RegisterPropertiesInCategory(cJediCat, TJvxRichEdit,['AutoURLDetect','AutoVerbMenu','AllowObjects',
    'AllowInPlace','Title','LangOptions','SelectionBar','StreamFormat','StreamMode','UndoLimit',
    'WordSelection','OnProtectChangeEx','OnSaveClipboard','OnSelectionChange','OnTextNotFound',
    'OnCloseFindDialog','OnURLClick']);
  // JvControlBar
  RegisterPropertiesInCategory(cJediCat, TJvControlBar,['PopupControl','PopupNames']);
  // JvSpecialLabel
  RegisterPropertiesInCategory(cJediCat, TJvSpecialLabel,['Text','SleepTime']);
  // JvTypedEdit
  RegisterPropertiesInCategory(cJediCat, TJvIntegerEdit,['Value','MaxValue','MinValue','HasMaxValue','HasMinValue']);
  // JvAppearingLabel
  RegisterPropertiesInCategory(cJediCat, TJvAppearingLabel,['FirstInterval','Interval','ScrollPixel',
    'AutoStart','AppearFrom','OnAppeared']);
  // JvSpin
  RegisterPropertiesInCategory(cJediCat, TJvxSpinEdit,['Alignment','ArrowKeys','ButtonKind','Decimal',
    'EditorEnabled','Increment','MaxValue','MinValue','ValueType','Value','OnBottomClick','OnTopClick']);
  // JvxSlider
  RegisterPropertiesInCategory(cJediCat, TJvxSlider,['ThumbOffset','SliderRect','BevelStyle','ImageHThumb',
    'ImageHRuler','ImageVThumb','ImageVRuler','NumThumbStates','Orientation','EdgeSize',
    'Options','ReadOnly','OnDrawPoints']);
  // JvCombobox
  RegisterPropertiesInCategory(cJediCat, TJvCustomCombobox,['MaxPixel','OnRestored']);
  // JvCtrls
  RegisterPropertiesInCategory(cJediCat, TJvListBox,['Align','HorzExtent','ScrollBars','Alignment',
    'HotTrack','OnGetText','OnSelectCancel','OnDeleteString','OnAddString','OnVerticalScroll','OnHorizontalScroll']);
  RegisterPropertiesInCategory(cJediCat, TJvImgBtn,['Alignment','Animate','AnimateFrames','AnimateInterval','Color',
    'Images','ImageIndex','ImageVisible','Kind','Layout','Margin','OwnerDraw','Spacing','OnButtonDraw','OnGetAnimateIndex']);
  // JvEdit
  RegisterPropertiesInCategory(cJediCat, TJvCustomEdit,['Alignment','DisabledTextColor','DisabledColor',
    'HotTrack','OnRestored','SelLength','SelStart','SelText','DisabledColor','DisabledTextColor']);
  // JvDriveCtrls
  RegisterPropertiesInCategory(cJediCat, TJvDriveCombo,['Drive','DriveTypes','Offset','ImageSize',
    'DisplayName']);
  RegisterPropertiesInCategory(cJediCat, TJvDriveList,['ImageAlign','Drive','DriveTypes','ImageSize','OnChange']);
  RegisterPropertiesInCategory(cJediCat, TJvDirectoryListBox,['AutoExpand','Directory']);
  // JvSplit
  RegisterPropertiesInCategory(cJediCat, TJvxSplitter,['TopLeftLimit','BottomRightLimit',
    'OnPosChanged','OnPosChanging']);

  // JvTypes
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnUrlClick), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnLinkClick), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnChangeKey), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TLabelDirection), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TAngle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDirection), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvOutputMode), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnDoneFile), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnDoneStream), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnProgress), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnFtpProgress), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnError), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TWallpaperStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TTransformationKind), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvWaveLocation), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyCap), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyCaps), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyButtonDown), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyMove), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyZMove), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyErrorMsg), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TBright), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TPopupPosition), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvDirMask), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvDirMasks), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJoyErrorMsg), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TListEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnPrnProgress), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnNextPage), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TBitmapStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnOpened), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnOpenCanceled), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnKeyFound), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TGradStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnDelete), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnParent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnImage), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnText), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvRestart), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvRunOption), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvRunOptions), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvFileKind), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TFormatOption), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TButtonStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TButtonDisplay), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDefault), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TModality), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TButtonOption), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TButtonOptions), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TButtonResult), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TMsgStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskStyles), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDeleteStyle), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDeleteStyles), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnOk), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TCoordChanged), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TNotifyEventParams), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvAnimation), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TJvAnimations), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDropEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnFound), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnChangedDir), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnAlarm), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TOnChangeColor), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDiskRes), ['']);

{$IFNDEF D6PersonalEdition}
  RegisterPropertiesInCategory(cJediCat, TJvDBSpinEdit,['DataField','DataSource']);
  // JvDBDateTimePicker
  RegisterPropertiesInCategory(cJediCat, TJvDateTimePicker, ['DataField', 'DataSource']);
  // JvDBProgressBar
  RegisterPropertiesInCategory(cJediCat, TJvDBProgressBar, ['DataField', 'DataSource']);
  // JvDBCtrl
  RegisterPropertiesInCategory(cJediCat, typeinfo(TTitleClickEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TCheckTitleBtnEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TGetCellParamsEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TSortMarker), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TGetBtnParamsEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TGetCellPropsEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, typeinfo(TDBEditShowEvent), ['']);
  RegisterPropertiesInCategory(cJediCat, TJvDBGrid, ['Options', 'FixedCols', 'ClearSelection', 'DefaultDrawing', 'MultiSelect',
    'ShowGlyphs', 'TitleButtons', 'RowsHeight']);
  RegisterPropertiesInCategory(cJediCat, TJvDBComboEdit, ['DataField', 'DataSource']);
  RegisterPropertiesInCategory(cJediCat, TJvDBDateEdit, ['DataField', 'DataSource', 'ReadOnly']);
  // JvDBQBE
  RegisterPropertiesInCategory(cJediCat, TJvQBEQuery, ['Local', 'ParamCount', 'Prepared', 'StmtHandle', 'Text', 'RowsAffected',
    'AutoRefresh', 'AuxiliaryTables', 'ParamCheck', 'StartParam', 'QBE', 'BlankAsZero', 'Params', 'RequestLive',
      'Constrained']);
  // JvDbRichEd
  RegisterPropertiesInCategory(cJediCat, TJvDBRichEdit,['Field','DataField','DataSource']);
  // JvDBComb
  RegisterPropertiesInCategory(cJediCat, TJvDBComboBox,['EnableValues','Values']);
  // JvDBIndex
  RegisterPropertiesInCategory(cJediCat, TJvDBIndexCombo,['NoIndexItem','EnableNoIndex',
    'DisplayMode','DataSource']);
{$ENDIF}

end;

end.

