{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLReg.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JVCLReg;

{$OBJEXPORTALL On}

interface

procedure Register;

implementation

{$R JVCLReg.dcr}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, Dlgs, 

  //About JVCL
  JvJVCLAbout, JVCLVer,

  //Additional
  JvImage, JvBitBtn, JvSpeedButton, JvMaskEdit, JvStaticText, JvCheckListBox,
  JvBevel, JvScrollBox, JvStringGrid, JvDrawGrid, JvShape, JvControlBar,
  JvSplitter,

  //Standard
  JvListbox2, JvEdit, JvCombobox, JvCheckBox, JvRadioButton, JvButton,
  JvLabel, JvScrollBar, JvGroupBox2, JvRadioGroup, JvPanel, JvMemo,JvMemoEx,

  //Edits
  JvCustomBox, JvFileNameBox, JvDirectoryBox, JvImageBox, JvButtonBox,
  JvIpBox, JvFloatEdit, JvCalculatorBox, JvFontBox,
  JvTypedEdit,

  //Controls
  JvPlaylist, JvButtonShaped, JvFavoritesButton, JvClock, JvStartMenuBtn,
  JvScrollText, JvFontCombobox, JvRecentMenuBtn, JvControlPanel,
  JvRegistryViewer, JvGradientCaption, JvImageListBox, JvImageCombobox,
  JvZoom, JvWaitingGradient, JvSpecialProgress, JvWaitingProgress,
  JvGammaPanel, JvSlider, JvOutlookPanel, JvSpinEdit,

  //Mp3
  JvBreatheSkin, JvId3v2, JvVisualId3v2, JvWinampApi, JvId3v1, JvVisualId3v1,

  //Multimedia
  JvCreatedWith, JvSoundControl, JvCdUtils, JvScreenCapture,
  JvWallPaper, JvDeviceChanged, JvImageTransform, JvImageRotate,
  JvWavePlayer, JvImageSplit, JvStarfield, JvJoystick, JvScreenResolution,
  JvScreenCanvas, JvSpecialImage,

  //Labels
  JvHotLink, JvBlinkingLabel, JvScrollingLabel, JvMailTo, JvReversedLabel,
  JvRealLabel, JvSpecialLabel, JvWinampLabel, JvAngleLabel, JvBouncingLabel,
  JvAppearingLabel, JvLinkLabel,

  //Forms
  JvFormWallpaper, JvAnimTitle, JvPerforated, JvFlashForm,
  JvTransparentForm, JvTrayIcon, JvTransForm, JvFormAnimatedIcon,
  JvAppAnimatedIcon, JvFormPlace, JvNotitle, JvFormAnimation,
  JvAutoSizeCompo, JvFlashApplication, JvFormButton, JvMagnet, JvGradient,

  //Convert
  JvStringListToHtml, JvRgbToHtml, JvStrToHtml, JvRichEditToHtml,
  JvFormToHtml,

  //Internet
  JvHttpGrabber, JvFtpGrabber, JvHtmlParser, JvSurfTo, JvImageMailto,
  JvImageSurfTo, JvMultiHttpGrabber,

  JvSimpleXml,

  //Utils
  JvChrono, JvComplex, JvCplLauncher, JvCreateShortcut,
  JvExecute, JvExitWindows, JvDirectories, JvFileUtils,
  JvDrivePresent, JvInstances, JvMemoryInfos, JvTrayBar,
  JvSerialMaker, JvThread, JvSafeMode, JvRegistry, JvSendKey,
  JvApplication, JvFileInformations, JvRecentDocs, JvAlarms,
  JvKeyboardStates, JvAssociateExtension, JvDragDrop, JvSearchFile,

  //Utils 2
  JvMru, JvRas32, JvSystemPopup, JvDirectorySpy, JvMousePositionner,
  JvWinHelp, JvEasterEgg, JvWindowsTitle, JvPrint, JvThreadTimer,
  JvTimeLimit, JvScreenSaver, JvClipboardViewer, JvSystemColors,
  JvPatchFile, JvRunOnStartup, JvComputerInfo, JvCommStatus, JvLogFile,
  JvCabFile, JvRle, JvDataEmbedded,

  //Utils 3
  JvRegion, JvCaesarCipher, JvVigenereCipher, JvXorCipher, JvGenetic,
  JvTranslator,


  //Dialogs
  JvCommonDialogD, JvBaseDlg, JvFindFiles,
  JvSelectDirectory, JvInputBox, JvPasswordForm, JvMessageBox, JvMessageBeep,
  JvFatalAppExit, JvExchListboxes, JvLoginDlg, JvSerialDlg, JvNagScreen,
  JvTipsOfDay, JvImageDlg, JvCalculator, JvProgressDlg, JvDiskPrompt,
  JvCopyError, JvDeleteError, JvRenameError,  JvPageSetupTitled, JvPageSetup,


  //WinDialogs
  JvBrowseFolder,


  //Win32
   JvStatusBar,
  JvTabControl, JvPageControl2, JvRichEdit, JvTrackBar2,
  JvProgressBar, JvUpDown, JvHotKey, JvAnimate, JvDateTimePicker,
  JvMonthCalendar, JvTreeView2, JvListView, JvHeaderControl, JvToolBar,
  JvCoolBar, JvPageScroller,
  JvHotKeyEx,

  //Image types
  JvPcx, JvAni,

  //Shapes
  JvArrow,

  //Composites
  JvDateTimeNullPicker, JvYesNoPicker,

  //Peter Below Goodies
  JvAlignedEdit, JvAlignListbox, JvBMPListBox, JvButtonDrawGrid, JvButtonPageControl,
  JvCaretEdit, JvControlStringgrid, JvCoupler, JvDisplayMemo, JvExLabel, JvEXLIST,
  JvExTabcontrol, JvFixedCheckListBox, JvFixedTrackbar, JvHighlighter, JvKeyScrollBox,
  JvMLButton, JvMousePanel, JvMultilineListbox, JvMultiselectChecklistbox, JvObserverLabel,
  JvObservibleCheckBox, JvOneSizeFitsAllButton, JvExEdit, JvExStringgrid, JvGroupBox,
  JvOwnerDrawPageControl, JvReorderListBox, JvPopupMemo, JvScrollEvStringGrid,
  JvSizeablePanel, JvSpacer, JvSyncSplitter, JvTextcontainer, JvTransparentPanel,

  //Petr Vones Components
  JVCLMiscal,
  JvComCtrls, JvCtrls, JvDdeCmd, JvDialogs, JvMail, JvPerfMon95, JvSysComp,

{$IFNDEF D6PersonalEdition}
//DB-Aware components
  JvDBDateTimePicker,
{$ENDIF}

  //Editors
  JvDataEmbeddedEditor, JvFormWallpaperEditor, JvPatcherEditor,
  JvHtmlParserEditor, JvAlarmsEditor,
   JvBaseDlgEditor, JvCommonDialogDEditor,

  Dialogs, ExptIntf, ToolIntf, ExtDlgs, StdCtrls, Buttons,
{$IFDEF DELPHI5}DsgnIntf, {$ENDIF}{$IFDEF DELPHI6_UP}DesignEditors, DesignIntf, {$ENDIF}
  JvTypes;


{******************************************************************************}


procedure Register;
begin
  //Total: 267 components



  //Registering Standard components - 14
  RegisterComponents('Jv Standard', [TJvLabel, TJvEdit, TJvMemo, TJvButton, TJvCheckBox,
    TJvRadioButton, TJvListbox2, TJvCombobox, TJvScrollBar, TJvGroupBox2, TJvRadioGroup,
      TJvPanel, TJvMemoEx, TJvSingleLineMemo]);

  //Registering Additional components - 13
  RegisterComponents('Jv Additional', [TJvBitBtn, TJvSpeedButton, TJvMaskEdit,
    TJvStringGrid, TJvDrawGrid, TJvImage, TJvShape, TJvBevel, TJvScrollBox,
      TJvCheckListBox, TJvSplitter, TJvStaticText, TJvControlBar]);

  //Registering Labels components - 12
  RegisterComponents('Jv Labels', [TJvHotLink, TJvBlinkingLabel, TJvScrollingLabel,
    TJvMailTo, TJvReversedLabel, TJvRealLabel, TJvSpecialLabel, TJvWinampLabel,
      TJvAngleLabel, TJvBouncingLabel, TJvAppearingLabel, TJvLinkLabel]);

  //Registering Multimedia components - 15
  RegisterComponents('Jv Multimedia', [TJvSoundControl, TJvCdUtils,
    TJvScreenCapture, TJvWallpaper, TJvDeviceChanged, TJvImageTransform,
      TJvImageRotate, TJvWavePlayer, TJvCreatedWith, TJvImageSplit,
      TJvStarfield, TJvJoystick, TJvScreenResolution, TJvScreenCanvas,
      TJvSpecialImage]);

  //Registering Forms enhancement components - 14
  RegisterComponents('Jv Forms', [TJvFormWallpaper, TJvAnimtitle, TJvPerforated,
    TJvFlashForm, TJvTransparentForm, TJvTrayIcon, TJvTransForm, TJvFormAnimatedIcon,
      TJvAppAnimatedIcon, TJvFormPlace, TJvNoTitle, TJvFormAnimation, TJvAutoSizeCompo,
      TJvFlashApplication, TJvFormButton, TJvFormMagnet,TJvGradient]);

  //Registering Utils components - 23
  RegisterComponents('Jv Utils', [TJvChrono, TJvComplex, TJvCplLauncher,
    TJvCreateShortcut, TJvExecute, TJvExitWindows, TJvAlarms,
      TJvDirectories, TJvFilesUtils, TJvDrivePresent, TJvInstances,
      TJvTraybar, TJvSerialMaker, TJvThread, TJvSafeMode, TJvSendKey,
      TJvApplication, TJvRecentDocs, TJvFileInformations, TJvKeyboardStates,
      TJvAssociateExtension, TJvDragDrop, TJvSearchFile]);

  //Registering Utils2 components - 22
  RegisterComponents('Jv Utils 2', [TJvTimeLimit, TJvScreenSaver, TJvClipboardViewer,
    TJvSystemColors, TJvPatchFile, TJvRunOnStartup, TJvRas32, TJvComputerInfo,
      TJvCommStatus, TJvThreadTimer, TJvSystemPopup, TJvDirectorySpy,
      TJvMousePositionner, TJvMruList, TJvWinHelp, TJvEasterEgg, TJvWindowsTitle,
      TJvPrint, TJvLogFile, TJvCabFile, TJvRle, TJvDataEmbedded]);

  //Registering Utils2 components - 7
  RegisterComponents('Jv Utils 3', [TJvRegion, TJvCaesarCipher, TJvVigenereCipher,
    TJvXorCipher, TJvGenetic ,TJvTranslator, TJvTranslatorStrings]);

  //Registering Dialogs components - 20
  RegisterComponents('Jv Dialogs', [TJvFindFiles,TJvInputBox,  TJvPasswordForm, TJvMessageBox,
      TJvMessageBeep, TJvFatalAppExit, TJvExchListboxes, TJvLoginDlg,
      TJvSerialDlg, TJvNagScreen, TJvTipsOfDay, TJvImageDlg, TJvCalculator,
      TJvProgressDlg, TJvDiskPrompt, TJvCopyError, TJvDeleteError, TJvRenameError, TJvPageSetupDialog,
      TJvPageSetupTitledDialog]);

  //Register WinDialogs components  -  2
  RegisterComponents('Jv WinDialogs', [TJvBrowseFolder, TJvSelectDirectory ]);


  //Registering Controls components - 20
  RegisterComponents('Jv Controls', [TJvPlaylist, TJvButtonShaped, TJvFavoritesButton,
    TJvStartMenuBtn, TJvClock, TJvGammaPanel, TJvScrollText, TJvFontCombobox,
      TJvRecentMenuBtn, TJvControlPanel, TJvRegistryViewer, TJvSlider, TJvGradientCaption,
      TJvImageListBox, TJvImageCombobox, TJvZoom, TJvWaitingGradient, TJvSpecialProgress,
      TJvWaitingProgress, TJvOutlookPanel, TJvSpinEdit]);

  //Register Win32 components - 18
  RegisterComponents('Jv Win32', [TJvStatusBar, TJvTabControl, TJvPageControl2,
    TJvRichEdit, TJvTrackBar2, TJvProgressBar, TJvUpDown, TJvHotKey, TJvHotKeyEx,
      TJvAnimate, TJvDateTimePicker, TJvMonthCalendar, TJvTreeView2, TJvListView,
      TJvHeaderControl, TJvToolBar, TJvCoolBar, TJvPageScroller]);

  //Register Net components - 8
  RegisterComponents('Jv Net', [TJvHtmlParser, TJvSurfTo, TJvImageSurfTo, TJvImageMailTo,
    TJvHttpGrabber, TJvMultiHttpGrabber, TJvFtpGrabber{, TJvSimpleXml}]);

  //Register Edits Components - 12
  RegisterComponents('Jv Edits', [TJvFileNameBox, TJvDirectoryBox, TJvImageBox,
    TJvButtonBox, TJvIpBox, TJvFloatEdit, TJvCalculatorBox, TJvFontBox,
    TJvCurrencyEdit,TJvFloatEdit2,TJvIntegerEdit,TJvYearEdit]);


  //Register Convert Components - 5
  RegisterComponents('Jv Convert', [TJvStringListToHtml, TJvRichEditToHtml,
    TJvRgbToHtml, TJvStrToHtml, TJvFormToHtml]);

  //Register MP3 Components - 6
  RegisterComponents('Jv MP3', [TJvId3v1, TJvVisualId3v1, TJvId3v2, TJvBreatheSkin,
    TJvWinampApi, TJvVisualId3v2]);

  // Register Shapes Components - 1
  RegisterComponents('Jv Shapes', [TJvArrow]);

  //Composites - 2
  RegisterComponents('Jv Composites', [TJvYesNoPicker, TJvDateTimeNullPicker]);


  //Register Peter Below Components - 34
  RegisterComponents('JEDI-VCL2', [TJvAlignedEdit, TJvBMPListBox, TJvAlignListbox, TJvButtonDrawGrid,
    TJvButtonPageControl, TJvControlStringgrid, TJvCoupler, TJvDisplayMemo, TJvExLabel,
      TJvExListbox, TJvExTabcontrol, TJvFixedCheckListBox, TJvHighlighter, TJvKeyScrollBox, TJvMultilineButton,
      TJvMousePanel, TJvMultilineListbox, TJvObserverLabel, TJvObservibleCheckBox, TJvOneSizeFitsAllButton,
      TJvOwnerDrawPageControl, TJvExEdit, TJvExStringgrid, TJvGroupBox, TJvReorderListBox, TJvSyncSplitter,
      TJvPopupMemo, TJvScrollEvStringGrid, TJvSizeablePanel, TJvSpacer, TJvTextContainer,
      TJvTransparentPanel, TJvCaretEdit, TJvCaretMemo]);


  //Petr Vones Components  - 15
  RegisterComponents('JEDI-VCL', [TJvIpAddress, TJvPageControl, TJvTrackBar, TJvTreeView]);
  RegisterComponents('JEDI-VCL', [TJvListBox, TJvImgBtn]);
  RegisterPropertyEditor(TypeInfo(TJvImgBtnKind), TJvImgBtn, 'Kind', TJvNosortEnumProperty);
  RegisterComponents('JEDI-VCL', [TJvAppDdeCmd]);
  RegisterComponents('JEDI-VCL', [TJvOpenDialog, TJvSaveDialog, TJvColorDialog]);
  RegisterComponentEditor(TCommonDialog, TJvOpenDialogEditor);
  RegisterComponents('JEDI-VCL', [TJvMail]);
  RegisterComponentEditor(TJvMail, TJvMailEditor);
  RegisterComponents('JEDI-VCL', [TJvPerfStat95]);
  RegisterPropertyEditor(TypeInfo(string), TJvPerfStatItem, 'PerfStatKey', TJvPerfStatProperty);
  RegisterComponents('JEDI-VCL', [{TJvProcessList,} TJvCreateProcess, TJvFileTreeScan]);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, '', TJvExeNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCreateProcess, 'CurrentDirectory', TJvDirectoryProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvFileTreeScan, 'RootDirectory', TJvDirectoryProperty);

{$IFNDEF D6PersonalEdition}
  //DB-Aware components
  RegisterComponents('JEDI-DB', [TJvDBDateTimePicker]);
{$ENDIF}

  //Register property editors - 5
  RegisterPropertyEditor(TypeInfo(TStream), TJvDataEmbedded, 'Data', TJvDataEmbeddedEditor);
  RegisterPropertyEditor(TypeInfo(TPicture), TJvFormWallpaper, 'Image', TJvFormWallpaperEditor);
  RegisterPropertyEditor(TypeInfo(TstringList), TJvPatchFile, 'Differences', TJvPatcherEditor);
  RegisterPropertyEditor(TypeInfo(TstringList), TJvAlarms, 'Alarms', TJvAlarmsEditor);
  RegisterPropertyEditor(TypeInfo(TParserInfos), TJvHtmlParser, 'Parser', TJvHtmlParserEditor);
  RegisterPropertyEditor(TypeInfo(TJVCLAboutInfo), nil, 'AboutJVCL', TJVCLAboutDialogProperty);


  //Register component editors - 3
  RegisterComponentEditor(TJvCommonDialog, TJvBaseDlgEditor);
//  RegisterComponentEditor(TJvCommonDialogP, TJvBaseDlgEditorP);
  RegisterComponentEditor(TJvCommonDialogD, TJvCommonDialogDEditor);
//  RegisterComponentEditor(TCommonDialog, TJvBrowseDialogEditor);

end;

end.

