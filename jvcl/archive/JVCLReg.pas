{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLReg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JVCLReg;

interface

procedure Register;

implementation

{$R ..\resources\JVCLReg.dcr}

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls,
  ToolsApi, ActnList, Dialogs, ExptIntf,
  {$IFNDEF COMPILER6_UP}
  DsgnIntf,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ENDIF}
  FiltEdit,
  JvDSADialogs, JvComponent,

  // About JVCL
  JvJVCLAbout, JvJVCLAboutProperty, JVCLVer,

  // Additional
  JvImage, JvBitBtn, JvMaskEdit, JvStaticText, JvCheckListBox,
  JvBevel, JvScrollBox, JvStringGrid, JvShape, JvControlBar,
  JvSplitter,

  // Standard
  JvEdit, JvCombobox, JvCheckBox, JvRadioButton, JvButton,
  JvLabel, JvScrollBar, JvRadioGroup, JvPanel, JvMemo, JvValidateEdit,

  // Edits
  {  JvCustomBox, JvFileNameBox, JvDirectoryBox, JvImageBox, JvButtonBox,
  JvCalculatorBox, }
//  JvTypedEdit, JvFloatEdit,

  // Controls
  JvPlaylist, JvFavoritesButton, JvClock, JvStartMenuBtn,
  JvScrollText, JvRecentMenuBtn,
  {$IFNDEF DelphiPersonalEdition}
  JvControlPanel,
  {$ENDIF}
  JvGradientCaption,
  JvZoom, JvWaitingGradient, JvSpecialProgress, JvWaitingProgress,
  JvGammaPanel, JvSlider, JvSpin,

  // Mp3
  JvBreatheSkin, JvVisualId3v2, JvWinampApi, JvVisualId3v1,

  // Multimedia
  JvSoundControl, JvDeviceChanged, JvImageTransform, JvImageRotate,
  JvWavePlayer, JvStarfield, JvJoystick, JvSpecialImage,
  JvThumbImage, JvThumbnails, JvThumbViews, JvMovableBevel,
  JvID3v2, JvID3v2Base, JvID3v2Editor, JvID3v1, 

  // Labels
  JvWinampLabel, JvLinkLabel,
  {JvHotLink, JvAngleLabel, JvBlinkingLabel, JvScrollingLabel, JvReversedLabel,
  JvRealLabel, JvSpecialLabel, JvBouncingLabel, JvAppearingLabel, }


  // Forms
  JvFormWallpaper, JvAnimTitle, JvPerforated,
  JvTransparentForm, JvTrayIcon, JvFormAnimatedIcon,
  JvAppAnimatedIcon, JvFormAnimation,
  JvAutoSizeCompo, JvMagnet, JvGradient,

  // Convert
  JvStringListToHtml, JvRgbToHtml, JvStrToHtml, JvRichEditToHtml, JvFormToHtml,

  // Internet
  JvHttpGrabber, JvFtpGrabber, JvHtmlParser, JvMultiHttpGrabber,

  JvSimpleXml,

  // Utils
  JvDirectories, JvMemoryInfos, JvSerialMaker, JvThread, JvRegistry,
  JvAlarms, JvKeyboardStates, JvDragDrop,

  // Utils 2
  JvMru, JvRas32, JvSystemPopup, JvMousePositionner,
  JvWinHelp, JvEasterEgg, JvPrint, JvThreadTimer,
  JvTimeLimit, JvScreenSaver, JvSystemColors,
  JvPatchFile, JvComputerInfo, JvCommStatus, JvLogFile,
  JvCabFile, JvDataEmbedded,

  // Utils 3
  JvCaesarCipher, JvVigenereCipher, JvXorCipher, JvGenetic,
  JvTranslator,

  // Dialogs
  JvCommonDialogD, JvBaseDlg,
  JvSelectDirectory,
  JvNagScreen, JvTipOfDay, JvImageDlg, JvDiskPrompt,
  JvCopyError, JvDeleteError, JvRenameError, JvPageSetupTitled, JvPageSetup,

  // WinDialogs
  JvBrowseFolder, JvObjPickerComp, JvConnectNetwork,
  JvAddPrinter, JvWinDialogs, JvxLogin, JvProgressDialog,

  // Win32
  JvStatusBar, JvProgressBar,
  JvTabControl, {JvRichEdit,}
  JvUpDown, JvHotKey, JvAnimate, JvDateTimePicker,
  JvMonthCalendar, JvListView, JvHeaderControl, JvToolBar,
  JvCoolBar, JvPageScroller,

  // Image types
  JvPcx, JvAni,

  // Shapes
  JvArrow,

  // Peter Below Goodies
  JvCoupler, JvHighlighter, JvGroupBox, JvPopupMemo, JvTransparentPanel, JvSpacer, JvSyncSplitter,

  // Petr Vones Components
  JvPerfStatEditor, JvMailEditor, 
  JvComCtrls, JvCtrls, JvDdeCmd, JvDialogs, JvMail, JvPerfMon95, JvSysComp,


  // JvBands
  JvBandForms, JvBandObjectDLLWizard,

  {$IFNDEF DelphiPersonalEdition}
  // DB-Aware components
  JvDBDateTimePicker,
  JvDBProgressBar,
  JvDBSpinEdit,
  {$ENDIF}

  // Editors
  JvDataEmbeddedEditor, JvFormWallpaperEditor, JvPatcherEditor,
  JvHtmlParserEditor, JvAlarmsEditor,
  JvBaseDlgEditor, JvCommonDialogDEditor, JvAppletProperty,

  JvTypes, JvArrowBtn, JvBmpAnim, JvCaptionButton,
  JvColorCombo, JvDriveCtrls, JvFindReplace, JvInstallLabel, JvRollOut, JvScrollPanel,
  JvTimeLine, JvTimeLineEdit, JvShFileOp, JvAppHotKey, JvBalloonHint,

  JvCaptionPanel, JvColorBtn, JvColorBox, JvColorForm, JvOutEdit,
  JvImagewindow, JvListComb, JvLookout, JvProfiler32, JvRegTV,
  JvSearchFiles, JvTransBtn, ImgList, JvTipOfDayProp,

  JvChangeNotify, JvEnterTab, JvFindFiles,
  JvFileInfo, JvItemsPanel, JvCntScr, JvCmdEdit,
  JvTMTL, JvCalendar, JvUCB, JvChNtfyProperty, JvOLBar, JvOLBEditor,
  JvInspector, JvHidControllerClass, JvAnalogClock, JvRadioCtl,

  JvDsgnEditors,

  // Fernando Silva
  JvFooter, JvGroupHeader, JvNTEventLog,
  JvGroupHeaderEditor, JvFooterEditor,  

  //  JvPlugin
  JvPlugin,
  JvPluginMan,
  JvPluginWizard,

  // Actions
  JvActions,

  // (rom) added
  JvShellHook,

  // palette names
  JvxDConst;

procedure RegPropEds;
begin
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, 'CurrentDirectory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvSearchFiles, 'RootDirectory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem, 'PerfStatKey', TJvPerfStatProperty);
  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded, 'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TPicture), TJvFormWallpaper, 'Image', TJvFormWallpaperEditor);
  RegisterPropertyEditor(TypeInfo(TstringList), TJvPatchFile, 'Differences', TJvPatcherEditor);
  RegisterPropertyEditor(TypeInfo(TstringList), TJvAlarms, 'Alarms', TJvAlarmsEditor);
  RegisterPropertyEditor(TypeInfo(TJvParserInfoList), TJvHtmlParser, 'Parser', TJvHtmlParserEditor);
  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookoutButton, 'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton, 'ImageIndex', TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvChangeItem, 'Directory', TJvDirectoryProperty);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(TDate), nil, '', TJvDateExProperty);
  RegisterPropertyEditor(TypeInfo(TTime), nil, '', TJvTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime), nil, '', TJvDateTimeExProperty);
  {$ENDIF}
  RegisterPropertyEditor(TypeInfo(TDateTime),TJvAlarmInfo,'Date',TJvDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDateTime),TJvAnalogClock,'Time',TJvTimeExProperty);

  {Thumbview.filter editor}
  RegisterPropertyEditor(TypeInfo(string), TJvThumbView, 'Filter', TFilterProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvAppletDialog, 'AppletName', TJvAppletNameProperty);
  RegisterPropertyEditor(TypeInfo(integer), TJvAppletDialog, 'AppletIndex', TJvAppletIndexProperty);
  RegisterPropertyEditor(TypeInfo(TShortCut), TJvComponent, '', TJvShortCutProperty);


// JvPlugin
//   RegisterPropertyEditor(TypeInfo(string), TJvPlugin, 'Version', TVersionEditor);
//   RegisterPropertyEditor(TypeInfo(string), TJvPluginManager, 'Version', TVersionEditor);
end;

procedure RegCompEds;
begin
  RegisterComponentEditor(TJvMail, TJvMailEditor);
//  RegisterComponentEditor(TCommonDialog, TJvOpenDialogEditor);
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterComponentEditor(TCommonDialog, TJvBaseDlgEditor);
  {$ENDIF}
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvOpenDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvSaveDialog, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogF, TJvBaseDlgEditor);
  RegisterComponentEditor(TJvCommonDialogD, TJvCommonDialogDEditor);
  RegisterComponentEditor(TJvTipOfDay, TJvTipOfDayEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvImageListBox, TJvStringsEditor);
  RegisterComponentEditor(TJvImageComboBox, TJvStringsEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
  RegisterComponentEditor(TJvChangeNotify, TJvChangeNotifyEditor);
  RegisterComponentEditor(TJvGroupHeader, TJvGroupHeaderEditor);
  RegisterComponentEditor(TJvFooter, TJvFooterEditor);
  RegisterComponentEditor(TJvID3Controller, TJvID3ControllerEditor);
end;

procedure RegComps;
begin
  // Jv Standard
  RegisterComponents(srJvStandardPalette,
    [TJvLabel, TJvEdit, TJvMemo, TJvCheckBox, TJvRadioButton,
     TJvListBox, TJvCombobox, TJvScrollBar, TJvGroupBox, TJvRadioGroup,
     TJvPanel, TJvJVCLAboutComponent, TJvValidateEdit]);

  // Jv Additional
  RegisterComponents(srJvAdditionalPalette,
    [TJvBitBtn, TJvImgBtn, TJvArrowButton, TJvTransparentButton, TJvTransparentButton2,

     TJvPopupMemo, TJvStringGrid,

     TJvCheckListBox, TJvImageListBox, TJvFontComboBox,
     TJvColorComboBox, TJvImageComboBox,

     TJvScrollBox, TJvImage, TJvImageWindow, TJvImageSquare,
     TJvShape, TJvBevel, TJvSyncSplitter,

     TJvControlBar,

     TJvStaticText, TJvScrollText, TJvContentScroller, TJvZoom,

     TJvMaskEdit, TJvCommandEdit,
     TJvSpinEdit, TJvSpinButton, TJvCoupler,

     TJvInstallLabel,

     TJvTransparentPanel, TJvCaptionPanel, TJvItemsPanel, TJvRollout,

     JvSpacer.TJvSpacer, TJvDivider, TJvArrow, TJvHighlighter, TJvRadioControl,

     TJvGroupHeader, TJvFooter]);

  // Jv Win32
  RegisterComponents(srJvWin32Palette,
    [TJvTabControl, TJvPageControl, {TJvRichEdit, }TJvTrackBar,
     {$IFNDEF DelphiPersonalEdition}
     TJvProgressBar,
     {$ENDIF}
     TJvUpDown, TJvDomainUpDown, TJvHotKey, TJvApplicationHotKey,
     TJvAnimate, TJvDateTimePicker, TJvMonthCalendar, TJvMonthCalendar2,
     TJvTreeView, TJvListView, TJvHeaderControl, TJvStatusBar, TJvToolBar,
     TJvCoolBar, TJvPageScroller, TJvScrollingWindow, TJvIpAddress,
     TJvSHFileOperation, TJvTrayIcon, TJvHidDeviceController]);

  // Jv System
  RegisterComponents(srJvSystemPalette,
    [TJvDriveCombo, TJvDriveList, TJvDirectoryListBox, TJvFileListBox,
     TJvCaptionButton, TJvColorButton, TJvColorBox, TJvColorSquare,
     TJvRegistryTreeView, TJvUninstallListBox, TJvUninstallComboBox,

     TJvDragDrop, TJvAppDdeCmd, TJvPerfStat95, TJvCreateProcess,
     TJvChangeNotify, TJvFileInfo, TJvComputerInfo, TJvDirectories,
     TJvSystemColors, TJvSystemPopup, TJvNTEventLog, TJvShellHook]);

  {$IFNDEF DelphiPersonalEdition}
  // Jv Data Controls
  RegisterComponents(srJvDataControlsPalette,
    [TJvDBDateTimePicker, TJvDBProgressBar, TJvDBSpinEdit]);
  {$ENDIF}

  // Jv Internet
  RegisterComponents(srJvInternetPalette,
    [{TJvHotLink, }TJvHtmlParser, TJvHttpGrabber, TJvMultiHttpGrabber,
     TJvFtpGrabber, TJvSimpleXml, TJvStringListToHtml, TJvRichEditToHtml,
     TJvRgbToHtml, TJvStrToHtml, TJvFormToHtml, TJvMail, TJvRas32,
     TJvCommStatus]);

  // Jv Dialogs
  RegisterComponents(srJvDialogsPalette,
    [TJvBrowseForFolderDialog, TJvSelectDirectory, TJvOpenDialog, TJvSaveDialog,
     {TJvOpenDialog2000, TJvSaveDialog2000,}
     TJvConnectNetwork,
     TJvDisconnectNetwork, TJvPageSetupDialog, TJvPageSetupTitledDialog,
     TJvAddPrinterDialog, TJvFindFilesDialog, TJvFormatDriveDialog,
     TJvColorDialog, TJvOrganizeFavoritesDialog, TJvComputerNameDialog,
     TJvAppletDialog, TJvChangeIconDialog,
     TJvShellAboutDialog, TJvRunDialog, TJvObjectPropertiesDialog,
     TJvNewLinkDialog, TJvAddHardwareDialog, TJvOpenWithDialog,
     TJvDiskFullDialog, TJvExitWindowsDialog, TJvOutOfMemoryDialog,
     TJvObjectPickerDialog,

     TJvNagScreen, TJvTipOfDay, TJvFindReplace,
     TJvImageDlg, TJvDiskPrompt, TJvCopyError, TJvDeleteError,
     TJvRenameError, TJvDSADialog, TJvLoginDialog, TJvProgressDialog]);

  // Jv Custom
  RegisterComponents(srJvCustomPalette,
    [TJvLinkLabel, TJvGammaPanel, TJvOutlookBar, TJvLookOut, TJvLookOutButton,
     TJvExpress, TJvExpressButton, TJvTimeLine, TJvTMTimeline, TJvInspector,
     TJvInspectorBorlandPainter, TJvInspectorDotNetPainter, TJvBalloonHint]);

  // Jv Labels
{  RegisterComponents(srJvLabelsPalette,
    [TJvAngleLabel, TJvBlinkingLabel, TJvScrollingLabel, TJvReversedLabel, TJvRealLabel,
     TJvSpecialLabel, TJvBouncingLabel, TJvAppearingLabel]);}

  // Jv Multimedia
  RegisterComponents(srJvMultimediaPalette,
    [TJvPlaylist, TJvSoundControl, TJvDeviceChanged, TJvJoystick,
     TJvWavePlayer, TJvBmpAnimator,

     TJvFormWallpaper, TJvStarfield, TJvImageTransform,
     TJvImageRotate, TJvSpecialImage, TJvSlider,

     TJvGradient, TJvGradientCaption, TJvWaitingGradient,
     TJvSpecialProgress, TJvWaitingProgress,

     TJvId3v1, TJvId3v2, TJvVisualId3v1, TJvVisualId3v2,

     TJvWinampLabel, TJvBreatheSkin, TJvWinampApi,

     TJvThumbImage, TJvThumbNail, TJvThumbView, TJvMovableBevel]);

  // Jv Forms
  RegisterComponents(srJvFormsPalette,
    [TJvFormMagnet, TJvAppAnimatedIcon, TJvFormAnimatedIcon, TJvAnimTitle,
     TJvTransparentForm, TJvPerforated,
     TJvFormAnimation, TJvAutoSizeCompo]);

  // Jv Utils
  RegisterComponents(srJvUtilsPalette,
    [TJvTranslator, TJvTranslatorStrings,

     TJvFavoritesButton, TJvStartMenuBtn, TJvRecentMenuBtn,
     {$IFNDEF DelphiPersonalEdition}
     TJvControlPanel,
     {$ENDIF}

     TJvSerialMaker, TJvTimeLimit,

     TJvScreenSaver, TJvPatchFile, TJvThread, TJvThreadTimer, TJvAlarms,
     TJvClock, TJvAnalogClock,

     TJvMruList, TJvWinHelp, TJvEasterEgg, TJvPrint, TJvMousePositionner,

     TJvDataEmbedded,

     TJvCaesarCipher, TJvVigenereCipher, TJvXorCipher, TJvGenetic,

     TJvSearchFiles, TJvLogFile, TJvCabFile, TJvProfiler,
     TJvEnterAsTab, TJvKeyboardStates ]);

  // Jv Convert
//  RegisterComponents(srJvConvertPalette,
//    [TJvFloatEdit, TJvFloatEdit2, TJvCurrencyEdit, TJvIntegerEdit, TJvYearEdit]);

  // JvBands
  RegisterCustomModule(TJvBandForm, TCustomModule);
  RegisterPackageWizard(TJvBandObjectDLLWizard.Create);


  //JvPlugin
  RegisterComponents(srJvPluginPalette,
    [TJvPluginManager]);

  // Jv Composites
{  RegisterComponents(srJvCompositesPalette,
     [TJvDirectoryBox, TJvFileNameBox, TJvImageBox,
      TJvButtonBox, TJvCalculatorBox]); }
end;

procedure RegExperts;
begin
  RegisterLibraryExpert(TJvPluginWizard.Create);
end;

procedure RegActions;
begin
  RegisterActions(srJVCLActions, [TJvSendMail, TJvWebAction], nil);
end;

procedure Register;
begin
  RegComps;
  RegPropEds;
  RegCompEds;
  RegExperts;
  RegActions;
  RegisterClass(TJvLookOutPage);
  RegisterClass(TJvFooterBtn);
end;

end.

