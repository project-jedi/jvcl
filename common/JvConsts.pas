{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvConsts;

interface

uses
  SysUtils;

const
  { JvEditor }
  JvEditorCompletionChars = #8+'0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';

  { various units }
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];

  {$IFDEF RAINTER}
  {RAInter}
  RAIIdSymbols      = StIdSymbols;
  RAIIdFirstSymbols = StIdFirstSymbols;
  {$ENDIF RAINTER}

  { JvDlg }
  SOk = 'OK';
  SCancel = 'Cancel';

  { Menu Designer }
  SMDMenuDesigner       = 'Menu &Designer';
  SMDInsertItem         = '&Insert';
  SMDDeleteItem         = '&Delete';
  SMDCreateSubmenuItem  = 'Create &SubMenu';

  SCantGetShortCut      = 'Target FileName for ShortCut %s not available';

  { RALib 1.23 }
  SPropertyNotExists    = 'Property "%s" does not exist';
  SInvalidPropertyType  = 'Property "%s" has invalid type';

  { RALib 1.55 }

resourcestring
  { JvAni.pas, JvAniFile.pas }
  RC_AniExtension = 'ani';
  RC_AniFilterName = 'ANI Image';
  SInvalidAnimatedIconImage = 'Animated icon image is not valid';

  { JvHLEdPropDlg }
  SHLEdPropDlg_Caption = 'Editor Properties';
  SHLEdPropDlg_tsEditor = 'Editor';
  SHLEdPropDlg_tsColors = 'Colors';
  SHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  SHLEdPropDlg_cbKeyboardLayoutDefault = 'Default keymapping';
  SHLEdPropDlg_gbEditor = 'Editor options:';
  SHLEdPropDlg_cbAutoIndent = '&Auto indent mode';
  SHLEdPropDlg_cbSmartTab = 'S&mart tab';
  SHLEdPropDlg_cbBackspaceUnindents = 'Backspace &unindents';
  SHLEdPropDlg_cbGroupUndo = '&Group undo';
  SHLEdPropDlg_cbCursorBeyondEOF = 'Cursor beyond &EOF';
  SHLEdPropDlg_cbUndoAfterSave = '&Undo after sa&ve';
  SHLEdPropDlg_cbKeepTrailingBlanks = '&Keep trailing blanks';
  SHLEdPropDlg_cbDoubleClickLine = '&Double click line';
  SHLEdPropDlg_cbSytaxHighlighting = 'Use &syntax highlight';
  SHLEdPropDlg_lblTabStops = '&Tab stops:';
  SHLEdPropDlg_lblColorSpeedSettingsFor = 'Color SpeedSettings for';
  SHLEdPropDlg_lblElement = '&Element:';
  SHLEdPropDlg_lblColor = '&Color:';
  SHLEdPropDlg_gbTextAttributes = 'Text attributes:';
  SHLEdPropDlg_gbUseDefaultsFor = 'Use defaults for:';
  SHLEdPropDlg_cbBold = '&Bold';
  SHLEdPropDlg_cbItalic = '&Italic';
  SHLEdPropDlg_cbUnderline = '&Underline';
  SHLEdPropDlg_cbDefForeground = '&Foreground';
  SHLEdPropDlg_cbDefBackground = '&Background';
  SHLEdPropDlg_OptionCantBeChanged = 'This option cannot be changed. Sorry.';

  SHLEdPropDlg_RAHLEditorNotAssigned = 'JvHLEditor property is not assigned';
  SHLEdPropDlg_RegAutoNotAssigned = 'RegAuto property is not assigned';

const
  {$IFDEF DELPHI2}
  SDelphiKey = 'Software\Borland\Delphi\2.0';
  {$ENDIF}
  {$IFDEF BCB1}
  SDelphiKey = 'Software\Borland\C++Builder\1.0';
  {$ENDIF}
  {$IFDEF DELPHI3}
  SDelphiKey = 'Software\Borland\Delphi\3.0';
  {$ENDIF}
  {$IFDEF BCB3}
  SDelphiKey = 'Software\Borland\C++Builder\3.0';
  {$ENDIF}
  {$IFDEF DELPHI4}
  SDelphiKey = 'Software\Borland\Delphi\4.0';
  {$ENDIF}
  {$IFDEF BCB4}
  SDelphiKey = 'Software\Borland\C++Builder\4.0';
  {$ENDIF}
  {$IFDEF DELPHI5}
  SDelphiKey = 'Software\Borland\Delphi\5.0';
  {$ENDIF}
  {$IFDEF BCB5}
  SDelphiKey = 'Software\Borland\C++Builder\5.0';
  {$ENDIF}
  {$IFDEF DELPHI6}
  SDelphiKey = 'Software\Borland\Delphi\6.0';
  {$ENDIF}
  {$IFDEF BCB6}
  SDelphiKey = 'Software\Borland\C++Builder\6.0';
  {$ENDIF}
  {$IFDEF DELPHI7}
  SDelphiKey = 'Software\Borland\Delphi\7.0';
  {$ENDIF}
  {$IFDEF BCB7} // will it ever be released?
  SDelphiKey = 'Software\Borland\C++Builder\7.0';
  {$ENDIF}
  {$IFDEF DELPHI8}
  SDelphiKey = 'Software\Borland\Delphi\8.0';
  {$ENDIF}

resourcestring
  { JvToolEdit }
  SBrowse                = 'Browse';
  {$IFDEF MSWINDOWS}
  SDefaultFilter         = 'All files (*.*)|*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  SDefaultFilter         = 'All files (*)|*';
  {$ENDIF LINUX}

  { JvPickDate }
  SDateDlgTitle          = 'Select a Date';
  SNextYear              = 'Next Year|';
  SNextMonth             = 'Next Month|';
  SPrevYear              = 'Previous Year|';
  SPrevMonth             = 'Previous Month|';

  { JvVCLUtils }
  SNotImplemented        = 'Function not yet implemented';
  SFileNotExec           = 'File specified is not an executable file, dynamic-link library, or icon file';
  SLoadLibError          = 'Could not load library ''%s''';
  SDetails               = 'Details';

  // JvTConst
  { JvDualList }
  SDualListSrcCaption         = '&Source';
  SDualListDestCaption        = '&Destination';

  { JvClipView }
  SClipbrdUnknown             = 'Cannot display. Data in Clipboard is in an unknown format.';
  SClipbrdEmpty               = 'Clipboard is empty';

  { JvSpeedbar }
  SCustomizeSpeedbar          = 'Customize Speedbar';
  SAvailButtons               = '&Available buttons:';
  SSpeedbarCategories         = '&Categories:';
  SSpeedbarEditHint           = 'To add command buttons, drag and drop buttons onto the SpeedBar. To remove command buttons, drag them off of the SpeedBar.';

  { MathParser }
  SParseSyntaxError           = 'Syntax error';
  SParseNotCramp              = 'Invalid condition (no cramp)';
  SParseDivideByZero          = 'Divide by zero';
  SParseSqrError              = 'Invalid floating operation';
  SParseLogError              = 'Invalid floating operation';
  SParseInvalidFloatOperation = 'Invalid floating operation';

  // JvGConst
  SGIFImage            = 'CompuServe GIF Image';
  SChangeGIFSize       = 'Cannot change the Size of a GIF image';
  SNoGIFData           = 'No GIF Data to write';
  SUnrecognizedGIFExt  = 'Unrecognized extension block: %.2x';
  SWrongGIFColors      = 'Wrong number of colors; must be a power of 2';
  SBadGIFCodeSize      = 'GIF code size not in range 2 to 9';
  SGIFDecodeError      = 'GIF encoded data is corrupt';
  SGIFEncodeError      = 'GIF image encoding error';
  SGIFVersion          = 'Unknown GIF version';

  // JvDConst
  { JvDBLists }
  SLocalDatabase          = 'Cannot perform this operation on a local database';

  { JvDBUtils }
  SRetryLogin             = 'Do you wish to retry the connect to database?';

  { JvDBFilter }
  SExprNotBoolean         = 'Field ''%s'' is not of type Boolean';
  SExprBadNullTest        = 'NULL only allowed with ''='' and ''<>''';
  SExprBadField           = 'Field ''%s'' cannot be used in a filter expression';
  SCaptureFilter          = 'Cannot perform this operation when controls are captured';
  SNotCaptureFilter       = 'Cannot perform this operation when controls are not captured';

  { JvDBCtrl }
  SInactiveData           = 'Closed';
  SBrowseData             = 'Browse';
  SEditData               = 'Edit';
  SInsertData             = 'Insert';
  SSetKeyData             = 'Search';
  SCalcFieldsData         = 'Calculate';

  { LoginDlg }
  SRegistration           = 'Registration';
  SAppTitleLabel          = 'Application "%s"';
  SHintLabel              = 'Type your user name and password';
  SUserNameLabel          = '&User name:';
  SPasswordLabel          = '&Password:';
  SInvalidUserName        = 'Invalid user name or password';

  { JvChPswDlg }
  SChangePassword         = 'Change password';
  SOldPasswordLabel       = '&Old password:';
  SNewPasswordLabel       = '&New password:';
  SConfirmPasswordLabel   = '&Confirm password:';
  SPasswordChanged        = 'Password has been changed';
  SPasswordNotChanged     = 'Password has not been changed';
  SPasswordsMismatch      = 'The new and confirmed passwords do not match';

  { JvDbExcpt }
  SDBExceptCaption        = 'Database Engine Error';
  SBDEErrorLabel          = 'BDE Error';
  SServerErrorLabel       = 'Server Error';
  SErrorMsgLabel          = 'Error message';
  SNextButton             = '&Next';
  SPrevButton             = '&Prev';

  { JvDBFilter expression parser }
  SExprIncorrect          = 'Incorrectly formed filter expression';
  SExprTermination        = 'Filter expression incorrectly terminated';
  SExprNameError          = 'Unterminated field name';
  SExprStringError        = 'Unterminated string constant';
  SExprInvalidChar        = 'Invalid filter expression character: ''%s''';
  SExprNoRParen           = ''')'' expected but %s found';
  SExprExpected           = 'Expression expected but %s found';
  SExprBadCompare         = 'Relational operators require a field and a constant';

  { JvDBUtils }
  SConfirmSave            = 'The data were changed. Save them?';
  SDatabaseName           = 'Database name: %s';

  { LoginDlg }
  SUnlockCaption          = 'Unlock application';
  SUnlockHint             = 'Type your password';

  { JvDBCtrl }
  SDeleteMultipleRecords  = 'Delete all selected records?';

  { Polaris patch }
  SDateMinLimit           = 'Enter a date before "%s"';
  SDateMaxLimit           = 'Enter a date after "%s"';
  SDateOutOfRange         = '%s - Enter a date between "%s" and "%s"';
  SDateOutOfMin           = '%s - Enter a date after "%s"';
  SDateOutOfMax           = '%s - Enter a date before "%s"';

  { JvID3_ }

  SID3FrameNotFound          = 'Frame not found';
  SID3UnknownEncoding        = 'Unknown encoding';
  SID3UnknownVersion         = 'Unknown version';
  SID3DuplicateFrame         = 'Frame is a duplicate of another frame in the tag.';
  SID3AlreadyContainsFrame   = 'Tag already contains a ''%s'' frame.';
  SID3ControllerNotActive    = 'Controller is not active';
  SID3EncodingNotSupported   = 'Encoding not supported in this version.';
  SID3VersionNotSupported    = 'Version not supported.';
  SID3NoController           = 'No controller specified.';
  SID3InvalidLanguageValue   = '''%s'' is an invalid language value.';
  SID3InvalidPartInSetValue  = '''%s'' is an invalid ''part in set'' value.';
  SID3InvalidTimeValue       = '''%s'' is an invalid time value.'#13'Value must be of format ''HHMM''.';
  SID3InvalidDateValue       = '''%s'' is an invalid date value.'#13'Value must be of format ''DDMM''.';
  SID3ValueTooBig            = '''%d'' is an invalid value. Value is too big.';
  SID3StringTooLong          = '''%s'' is an invalid value. String is too long.';
  SID3InvalidCharinList      = 'Invalid char ''%s'' in string ''%s'' in list.';
  SID3InvalidFrameClass      = 'Frame class ''%s'' can''t be used to represent frame ID ''%s''';
  SID3FrameIDNotSupported    = 'Frame ID ''%s'' not supported by this frame';
  SID3FrameIDStrNotSupported = 'Frame ID string ''%s'' not supported by this frame';

  srJvHLEdPropDlgIni = 'JvHLEdPropDlg.ini';

  { see JvCtlReg }
  {$IFDEF COMPILER3_UP}
  srSamplesPalette = 'Samples';
  {$ENDIF}

  { JVCL IDE palettes }
  SPaletteSystem = 'Jv System';
  SPaletteDialog = 'Jv Dialogs';
  SPaletteButton = 'Jv Buttons';
  SPaletteEdit = 'Jv Edits';
  SPaletteCustom = 'Jv Custom';
  SPaletteBarPanel = 'Jv Bars and Panels';
  SPaletteLabel = 'Jv Labels';
  SPaletteListComboTree = 'Jv Lists, Combos and Trees';
  SPaletteScrollerTracker = 'Jv Scrollers and Trackers';
  SPaletteSliderSplitter = 'Jv Sliders and Splitters';
  SPaletteImageAnimator = 'Jv Images and Animators';
  SPaletteVisual = 'Jv Visual';
  SPaletteNonVisual = 'Jv Non-Visual';
  SPaletteAppForm = 'Jv Application and Forms';
  SPaletteInterNetWork = 'Jv Internet and Network';
  SPaletteEncryptCompress = 'Jv Encrypt and Compress';
  SPaletteDBVisual = 'Jv Data Controls';
  SPaletteDBNonVisual = 'Jv Data Access';
  SPaletteHMIIndicator = 'Jv HMI Indicators';
  SPaletteHMINonVisual = 'Jv HMI Non-Visual';
  SPaletteHMIControls = 'Jv HMI Controls';
  SPaletteBDE = 'Jv BDE Components';
  SPaletteMTThreads = 'Jv Threading';
  SPalettePrintPreview = 'Jv Print Preview';
  SPaletteTimeFramework = 'Jv TimeFrameWork';
  SPaletteUIB = 'Jv UIB';
  SPaletteInterpreter = 'Jv Interpreter';
  SPaletteGlobusComponents1 = 'JVCL Globus Components 1';
  SPaletteGlobusComponents2 = 'JVCL Globus Components 2';
  SPaletteValidators = 'Jv Validators';
  SPaletteWizard = 'Jv Wizard';

  { for RegisterActions }
  srJVCLActions     = 'JVCL';
  srJvDialogActions = 'Jv Dialog';

  { TImageList component editor }
  srSaveImageList      = 'Save to bitmap...';

  { TJvFormStorage component editor }
  srStorageDesigner    = 'Form Storage Designer...';

  { TJvPageManager component editor }
  srProxyEditor        = 'Edit Proxies...';
  srPageProxies        = '%s Page Proxies';
  srProxyName          = 'Page Proxy Name';
  srPageName           = 'Page Name';

  { TJvSpeedBar component editor }
  srSBItemNotCreate    = 'Cannot create a new Speedbar button';
  srConfirmSBDelete    = 'Are you sure you want to delete current section?';
  srSpeedbarDesigner   = 'Speedbar designer...';
  srNewSectionName     = 'Untitled (%d)';

  { TJvTimerList component editor }
  srEventNotCreate     = 'Cannot create a new event';
  srTimerDesigner      = 'Edit Events...';
  srTimerEvents        = '%s.Events';

  { TJvAnimatedImage component editor }
  srAniCurFilter       = 'Animated Cursors (*.ani)|*.ani|Any files (*.*)|*.*';
  srEditPicture        = 'Edit picture...';
  srLoadAniCursor      = 'Load from ANI...';

  { TJvIconList property editor }
  srLoadIcon           = 'Load Icon';

  { TJvxGradientCaption component editor }
  srCaptionDesigner    = 'Edit Captions...';
  srGradientCaptions   = 'Captions';

  { TJvMemoryTable & TJvMemoryData component editor }
  srBorrowStructure    = 'Borrow structure...';

  { various editors }
  SJvEditorString = 'Click to edit...';

  { JvID3_ component editor }
  SID3CommitTag = '&Commit';
  SID3FileInfoTag = 'File &info';
  SID3FrameEditorTag = 'Frame edi&tor';
  SID3RemoveTag = '&Remove tag...';
  SID3RemoveTagConfirmation = 'Remove tag?';
  SID3Err_FileDoesNotExists = 'File %s does not exists';
  SID3Err_NoFileSpecified = 'No file specified';
  SID3Err_NoValidMPEGTag = 'This file has not a valid MPEG tag';

const
  { JvDataProvider constants }
  { Consumer attributes }
  DPA_RenderDisabledAsGrayed = 1;
  DPA_RendersSingleItem      = 2;
  DPA_ConsumerDisplaysList   = 3;

resourcestring
  SExtensibleIntObjDuplicateClass = 'Implementation of that class already exists.';
  SExtensibleIntObjCollectionExpected = 'Expected collection.';
  SExtensibleIntObjClassNameExpected = 'Missing ClassName property';
  SExtensibleIntObjInvalidClass = 'Invalid class type.';
  SDataItemRenderHasNoText = '(item doesn''t support the IJvDataItemText interface)';
  SDataProviderNeedsItemsImpl = 'Can''t create a data provider without an IJvDataItems implementation.';

  { JvUrlListGrabber }
  sENoGrabberForUrl = 'There is no grabber capable of handling URL: %s';

implementation

end.
