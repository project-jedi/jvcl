{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDsgnConsts.pas, released on 2003-6-27.

The Initial Developers of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Project JEDI
All Rights Reserved.

Contributor(s): -

Last Modified: 2003-06-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvDsgnConsts;

interface

//=== strings used by several editors ========================================
resourcestring
  SPreviewEllipsis = 'Preview...';
  SDesignerEllipsis = 'Designer...';
  SItemsEditorEllipsis = 'Items Editor...';
  SNone = '(none)';
  SHelp = 'Help';
  SConfirm = 'Confirm?';
  SAllFilesFilter = 'All Files (*.*)|*.*';
  SNextPage = 'Next Page';
  SPreviousPage = 'Previous Page';
  SJVCLActionsCategory = 'JVCL';
  SPropertyEditors = '\Property Editors';
  SJvEditorString = 'Click to edit...';

//=== JVCL IDE palettes ======================================================
resourcestring
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

//=== JvAnimatedEditor.pas ===================================================
resourcestring
  RsEditPicture = 'Edit picture...';
  RsLoadAniCursor = 'Load from ANI...';

//=== JvAVICaptureEditors.pas ================================================
resourcestring
  SDisconnected = 'Disconnected';
  SdIsNotWithinTheValidRangeOfdd = '%d is not within the valid range of %d..%d';

//=== JvBandObjectDLLWizard.pas ==============================================
resourcestring
  SCreatesABandObjectDLLProject = 'Creates a Band Object DLL Project.';
  SBandObjectDLLWizard = 'Band Object DLL Wizard';

//=== JvBandObjectDLLWizardForm.pas ==========================================
resourcestring
  SBandNameHasToBeAValidIdentifier = 'Band name has to be a valid identifier!';
  SPleaseEnterBandDescription = 'Please enter band description!';
  SHelpText = sLineBreak +
    'Band Name' + sLineBreak +
    'Enter a band name, e.g. MyBand.' + sLineBreak +
    'This will be the class name of the band object.' + sLineBreak +
    'No need to prefix it with ''T'' as it will be generated.' + sLineBreak + sLineBreak +
    'Description' + sLineBreak +
    'Enter a menuitem text, e.g. &My Band' + sLineBreak +
    'This text will appear in the Explorer Bar or Toolbars menu.' + sLineBreak + sLineBreak +
    'Band Type' + sLineBreak +
    'Select the type of band object to create.';

//=== JvChangeNotifyEditor.pas ===============================================
resourcestring
  SNotificationsEllipsis = 'Notifications...';

//=== JvCheckedItemsForm.pas =================================================
resourcestring
  SItemEditor = 'Item editor';
  SEnabled = 'Enabled';

//=== JvColorProviderDesignerForm.pas ========================================
resourcestring
  SSystemColors = 'System colors';
  SStandardColors = 'Standard colors';
  SCustomColorsEllipsis = 'Custom colors...';
  SColorMsg = 'Copy standard and system colors from the default context?';

//=== JvColorProviderEditors.pas =============================================
resourcestring
  SMappingDoesNotExistForThisColorProv = 'Mapping does not exist for this color provider.';
  SInvalidPropertyValue = 'Invalid property value';

//=== JvCsvDataEditor.pas ====================================================
resourcestring
  SJvCsvDataSetSelectCSVFileToOpen = 'JvCsvDataSet - Select CSV File to Open';
  SCsvFilter = '*.csv';

//=== JvCsvDataForm.pas ======================================================
resourcestring
  SMustTypeAValidFieldNameAndSelectAFi =
    'Must type a valid field name and select a field type. ' +
    'Field name must start with a letter A-Z and consist of letters and numbers only. ' +
    'All field names will be converted to uppercase before being used.';
  SAddFailed = 'Add Failed';
  SFieldNameIsNotAValidIdentifier = '%s: Field name is not a valid identifier';
  SCantAddTwoFieldsWithTheSameNameSele =
    'Can''t add two fields with the same name! Select existing item and click ''Modify'' button to change its properties.';
  SUpdateFailed = 'Update Failed';
  SNoItemIsSelectedInTheFieldsListYouC = 'No item is selected in the fields list. You can''t update nothing.';
  SModifyingTheCurrentlySelectedItemWo =
    'Modifying the currently selected item would create two items with the same name.';
  SYouHaventActuallyChangedAnythingIfY = 'You haven''t actually changed anything. If you ' +
    'made changes and didn''t click Modify, the changes have ' +
    'not been made yet. (Click no, to go back.) ' + sLineBreak +
    'Are you sure you want to close the CSV Fields editor? ';

//=== JvDataConsumerContextSelectForm.pas ====================================
resourcestring
  SConsumerDoesNotSupportContextSelect = 'Consumer does not support context selection.';
  SIJvDataConsumerProviderIsNotSupported = 'IJvDataConsumerProvider is not supported by the specified consumer.';

//=== JvDataConsumerItemSelectForm.pas =======================================
resourcestring
  SDataProviderItemSelector = 'DataProvider Item Selector';

//=== JvDataContextManagerForm.pas ===========================================
resourcestring
  SDataProviderContextManager = 'DataProvider Context Manager';

//=== JvDataProviderDesignerForm.pas =========================================
resourcestring
  SDataProviderDesigner = 'DataProvider Designer';
  SInternalErrorUnableToRetrieveContext = 'Internal error: unable to retrieve context list';

//=== JvDataProviderEditors.pas ==============================================
resourcestring
  SSpecifiedProviderIsNotATComponentDe = 'Specified provider is not a TComponent descendant';
  STreeDesignerEllipsis = 'Tree designer...';
  SContextManagerEllipsis = 'Context manager...';
  SInvalidVerbd = 'Invalid verb#: %d';

//=== JvDataProviderItemDesign.pas ===========================================
resourcestring
  SUnknown = '<unknown>';
  SNoItem = '<no item>';

//=== JvDateTimeForm.pas =====================================================
resourcestring
  SSelectDate = 'Select Date';
  SSelectTime = 'Select Time';
  SSelectDateTime = 'Select Date and Time';
  SMaxInt = 'MaxInt';
  SMinInt = 'MinInt';
  SMaxLong = 'MaxLong';
  SMinLong = 'MinLong';
  SMaxShort = 'MaxShort';
  SMinShort = 'MinShort';
  SMaxWord = 'MaxWord';
  SFileName = '(Filename)';
  SDirectory = '(Directory)';

//=== JvDBMemDatasetEditor.pas ===============================================
resourcestring
  RsBorrowStructure = 'Borrow structure...';

//=== JvDsgnEditors.pas ======================================================
resourcestring
  SStripFilePath = '&Strip file path';
  SExecutableFilesExeExeAllFiles = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
  SItems = 'Items';
  SFmtEditEllipsis = '%s Editor...';
  RsSaveImageList = 'Save to bitmap...';

//=== JvFooterEditor.pas =====================================================
resourcestring
  SAddButtonText = 'Add button';
  SMSOffice = 'MS Office 2000';
  SMSEnterpriseManagerWizard = 'MS Enterprise Manager Wizard';
  SDialogMode = 'Dialog Mode';
  SPrevious = 'Previous';
  SNext = 'Next';

//=== JvFormPropertiesForm.pas ===============================================
resourcestring
  RsStorageDesigner = 'Form Storage Designer...';

//=== JvgComponentListEditorForm.pas =========================================
resourcestring
  SEditComponentListEllipsis = 'Edit component list...';

//=== JvgHelpPanelEditor.pas =================================================
resourcestring
  SRTFAndTextFilesrtftxtrtftxt = 'RTF and Text files (*.rtf,*.txt)|*.rtf;*.txt';
  SLoadRTFFile = 'Load RTF file';
  SPreviewRTFText = 'Preview RTF text';

//=== JvID3v2EditorForm.pas ==================================================
resourcestring
  SfiErrorFrame = 'Error';
  SfiPaddingFrame = 'Padding';
  SfiNoFrame = 'No known frame';
  SfiAudioCrypto = 'Audio encryption';
  SfiPicture = 'Attached picture';
  SfiAudioSeekPoint = 'Audio seek point index';
  SfiComment = 'Comments';
  SfiCommercial = 'Commercial frame';
  SfiCryptoReg = 'Encryption method registration';
  SfiEqualization2 = 'Equalization (2)';
  SfiEqualization = 'Equalization';
  SfiEventTiming = 'Event timing codes';
  SfiGeneralObject = 'General encapsulated object';
  SfiGroupingReg = 'Group identification registration';
  SfiInvolvedPeople = 'Involved people list';
  SfiLinkedInfo = 'Linked information';
  SfiCDID = 'Music CD identifier';
  SfiMPEGLookup = 'MPEG location lookup table';
  SfiOwnership = 'Ownership frame';
  SfiPrivate = 'Private frame';
  SfiPlayCounter = 'Play counter';
  SfiPopularimeter = 'Popularimeter';
  SfiPositionsync = 'Position synchronisation frame';
  SfiBufferSize = 'Recommended buffer size';
  SfiVolumeAdj2 = 'Relative volume adjustment (2)';
  SfiVolumeAdj = 'Relative volume adjustment';
  SfiReverb = 'Reverb';
  SfiSeekFrame = 'Seek frame';
  SfiSignature = 'Signature frame';
  SfiSyncedLyrics = 'Synchronized lyric/text';
  SfiSyncedTempo = 'Synchronized tempo codes';
  SfiAlbum = 'Album/Movie/Show title';
  SfiBPM = 'BPM (beats per minute)';
  SfiComposer = 'Composer';
  SfiContentType = 'Content type';
  SfiCopyright = 'Copyright message';
  SfiDate = 'Date';
  SfiEncodingTime = 'Encoding time';
  SfiPlaylistDelay = 'Playlist delay';
  SfiOrigReleaseTime = 'Original release time';
  SfiRecordingTime = 'Recording time';
  SfiReleaseTime = 'Release time';
  SfiTaggingTime = 'Tagging time';
  SfiInvolvedPeople2 = 'Involved people list';
  SfiEncodedBy = 'Encoded by';
  SfiLyricist = 'Lyricist/Text writer';
  SfiFileType = 'File type';
  SfiTime = 'Time';
  SfiContentGroup = 'Content group description';
  SfiTitle = 'Title/songname/content description';
  SfiSubTitle = 'Subtitle/Description refinement';
  SfiInitialKey = 'Initial key';
  SfiLanguage = 'Language(s)';
  SfiSongLen = 'Length';
  SfiMusicianCreditList = 'Musician credits list';
  SfiMediaType = 'Media type';
  SfiMood = 'Mood';
  SfiOrigAlbum = 'Original album/movie/show title';
  SfiOrigFileName = 'Original filename';
  SfiOrigLyricist = 'Original lyricist(s)/text writer(s)';
  SfiOrigArtist = 'Original artist(s)/performer(s)';
  SfiOrigYear = 'Original release year';
  SfiFileOwner = 'File owner/licensee';
  SfiLeadArtist = 'Lead performer(s)/Soloist(s)';
  SfiBand = 'Band/orchestra/accompaniment';
  SfiConductor = 'Conductor/performer refinement';
  SfiMixArtist = 'Interpreted, remixed, or otherwise modified by';
  SfiPartInSet = 'Part of a set';
  SfiProducedNotice = 'Produced notice';
  SfiPublisher = 'Publisher';
  SfiTrackNum = 'Track number/Position in set';
  SfiRecordingDates = 'Recording dates';
  SfiNetRadioStation = 'Internet radio station name';
  SfiNetRadioOwner = 'Internet radio station owner';
  SfiSize = 'Size';
  SfiAlbumSortOrder = 'Album sort order';
  SfiPerformerSortOrder = 'Performer sort order';
  SfiTitleSortOrder = 'Title sort order';
  SfiISRC = 'ISRC (international standard recording code)';
  SfiEncoderSettings = 'Software/Hardware and settings used for encoding';
  SfiSetSubTitle = 'Set subtitle';
  SfiUserText = 'User defined text information';
  SfiYear = 'Year';
  SfiUniqueFileID = 'Unique file identifier';
  SfiTermsOfUse = 'Terms of use';
  SfiUnsyncedLyrics = 'Unsynchronized lyric/text transcription';
  SfiWWWCommercialInfo = 'Commercial information';
  SfiWWWCopyright = 'Copyright/Legal information';
  SfiWWWAudioFile = 'Official audio file webpage';
  SfiWWWArtist = 'Official artist/performer webpage';
  SfiWWWAudioSource = 'Official audio source webpage';
  SfiWWWRadioPage = 'Official internet radio station homepage';
  SfiWWWPayment = 'Payment';
  SfiWWWPublisher = 'Official publisher webpage';
  SfiWWWUser = 'User defined URL link';
  SfiMetaCrypto = 'Encrypted meta frame';
  SfiMetaCompression = 'Compressed meta frame';

  SMPEG10 = 'MPEG 1.0';
  SMPEG20 = 'MPEG 2.0';
  SMPEGUnknown = '??';
  SMPEG25 = 'MPEG 2.5';

  SLayerUnknown = '??';
  SLayer1 = 'Layer 1';
  SLayer2 = 'Layer 2';
  SLayer3 = 'Layer 3';

  SChannelModeStereo = 'Stereo';
  SChannelModeJointStereo = 'Joint Stereo';
  SChannelModeDualChannel = 'Dual Channel';
  SChannelModeMono = 'Mono';

  SEmphasisNone = 'None';
  SEmphasisMicrosec = '50/15 microsec';
  SEmphasisUnknown = '??';
  SEmphasisCCITTJ17 = 'CCIT J.17';

  SBoolNo = 'No';
  SBoolYes = 'Yes';

  SVbrNo = '';
  SVbrVbr = ' (VBR)';

  SIDV2FileInfo =
    'Size: %d bytes' + sLineBreak +
    'Header found at: %d bytes' + sLineBreak +
    'Length: %d seconds' + sLineBreak +
    '%s %s' + sLineBreak +
    '%dkbit%s, %d frames' + sLineBreak +
    '%dHz %s' + sLineBreak +
    'CRCs: %s' + sLineBreak +
    'Copyrighted: %s' + sLineBreak +
    'Original: %s' + sLineBreak +
    'Emphasis: %s';

  SIDV2FileInfoCaption = 'File info';

  // (rom) from JvConsts.pas
  SID3CommitTag = '&Commit';
  SID3FileInfoTag = 'File &info';
  SID3FrameEditorTag = 'Frame edi&tor';
  SID3RemoveTag = '&Remove tag...';
  SID3RemoveTagConfirmation = 'Remove tag?';
  SID3Err_FileDoesNotExists = 'File %s does not exists';
  SID3Err_NoFileSpecified = 'No file specified';
  SID3Err_NoValidMPEGTag = 'This file has not a valid MPEG tag';

//=== JvgLabelEditorForm.pas =================================================
resourcestring
  SEditLabel = 'Edit &Label...';

//=== JvgLogicItemEditorForm.pas =============================================
resourcestring
  SLogicElements = 'Logic Element: %s';
  SResult = '[RESULT]';
  SNotDefined = '[ not defined ]';

//=== JvgLogicsEditorForm.pas ================================================
resourcestring
  SCaption = 'Caption';
  SComments = 'Comments';
  SEditComponentEllipsis = 'Edit component...';

//=== JvGradientCaptionForm.pas ==============================================
resourcestring
  RsCaptionDesigner = 'Edit Captions...';
  RsGradientCaptions = 'Captions';

//=== JvgReportEditorForm.pas ================================================
resourcestring
  SEditReport = 'Edit report...';
  SPreviewReportEllipsis = 'Preview report...';
  SDeleteObject = 'Delete object?';
  SPagePreview = 'Page Preview';

//=== JvgReportParamsForm.pas ================================================
resourcestring
  SEditParamsEllipsis = 'Edit params...';

//=== JvGroupHeaderEditor.pas ================================================
resourcestring
  SStandardFlat = 'Standard/Flat';
  SWeb = 'Web';

//=== JvgShadowEditor.pas ====================================================
resourcestring
  SUpdateAllEditControl = 'Update all edit control';

//=== JVHLEditEditor.pas =====================================================
resourcestring
  SHLEditorMsg = 'Please select "JvHLEditor" first';
  SHLEditorMsgTitle = 'Cannot edit';
  SExecute = 'Execute';
  RsJvHLEdPropDlgIni = 'JvHLEdPropDlg.ini';

//=== TJvIconListForm.pas ====================================================
resourcestring
  RsLoadIcon = 'Load Icon';

//=== JvID3v2EditorForm.pas ==================================================
resourcestring
  SCommit = 'Commit?';

//=== JvIDEZoom.pas ==========================================================
resourcestring
  SZoomEditWindow = 'Zoom Edit Window';

//=== JvImagePreviewForm.pas =================================================
resourcestring
  SPreview = 'Preview';

//=== JvJVCLAboutEditor.pas ==================================================
resourcestring
  SVersions = 'Version %s';

//=== JvLookoutEditor.pas ====================================================
resourcestring
  SAddPage = 'Add page';
  SActivate = 'Activate';
  SAddButton = 'Add Button';
  SScrollUp = 'Scroll Up';
  SScrollDown = 'Scroll Down';
  SAddPage_ = 'Add Page';

//=== JvMailEditor.pas =======================================================
resourcestring
  SSend = 'Send';
  SAddress = 'Address';

//=== JvOutlookBarEditors.pas ================================================
resourcestring
  SOLEditor = 'OutlookBar Editor...';

//=== JvOutlookBarForm.pas ===================================================
resourcestring
  SDesignerIsNilInFormClosed = 'Designer is nil in FormClosed';
  SFmtCaption = 'Editing %s';
  SOutlookBarCaption = 'OutlookBar Editor';

//=== JvPageLinkEditor.pas ===================================================
resourcestring
  SCreateLinkToPaged = 'Create link to page %d';

//=== JvPageListTreeViewReg.pas ==============================================
resourcestring
  SFmtInterfaceNotSupported = '%s does not support the required interface (%s)';
  SNextPageAmp = 'Ne&xt Page';
  SPrevPage = '&Previous Page';
  SNewPage = '&New Page';
  SDelPage = '&Delete Page';

//=== JvPageManagerForm.pas ==================================================
resourcestring
  RsProxyEditor = 'Edit Proxies...';
  RsPageProxies = '%s Page Proxies';
  RsProxyName = 'Page Proxy Name';
  RsPageName = 'Page Name';

//=== JvPictureEditForm.pas ==================================================
resourcestring
  SLoadPicture = 'Load picture';
  SSavePictureAs = 'Save picture as';

//=== JvPluginParamsForm.pas =================================================
resourcestring
  SPluginParamsFormInfoText =
    'The settings above will create the following project:' +
    sLineBreak + sLineBreak +
    '* A project called Plg%0:s.%1:s' + sLineBreak +
    '* A unit called Plugin%0:s, containing the data module T%0:s.';

//=== JvPluginWizard.pas =====================================================
resourcestring
  SJvPluginWizard = 'Jv Plugin Wizard';
  SProjects = 'Projects';
  SNewPlugin = 'New Plugin';
  SPrivateDeclarations = '{ Private declarations }';
  SPublicDeclarations = '{ Public declarations }';
  SIMPORTANTNOTEIfYouChangeTheNameOfTh =
    '// IMPORTANT NOTE: If you change the name of the Plugin container,' + sLineBreak +
    '// you must set the type below to the same type. (Delphi changes' + sLineBreak +
    '// the declaration, but not the procedure itself. Both the return' + sLineBreak +
    '// type and the type created must be the same as the declared type above.';
  SJediPuginWizard = 'JEDI Plugin Wizard';

//=== JvPreviewReg.pas =======================================================
resourcestring
  SCreatePreview = 'Create Preview';
  SClearPreview = 'Clear Preview';

//=== JvScheduleEditorForm.pas ===============================================
resourcestring
  SInvalidScheduleSettingsFound = 'Invalid schedule settings found.';
  SStop = 'Stop';
  SRun = 'Run';

//=== JvScheduleEditors.pas ==================================================
resourcestring
  SEventEditor = 'Event editor...';

//=== JvScrollMaxEditor.pas ==================================================
resourcestring
  SAddBand = 'Add Band';

//=== JvSegmentedLEDDisplayEditors.pas =======================================
resourcestring
  SAddDigit = 'Add digit';
  SRemoveDigit = 'Remove digit';
  SEditMappingEllipsis = 'Edit mapping...';

//=== JvSegmentedLEDDisplayMappingForm.pas ===================================
resourcestring
  SSegmentedLEDDisplayMappingEditor = 'Segmented LED Display Mapping Editor';

//=== JvSegmentedLEDDisplayMappingForm.pas ===================================
resourcestring
  STextFilter =
    'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|Batch files (*.BAT)|*.BAT|All files (*.*)|*.*';
  //  STextFilter = 'Text files (*.txt)|*.txt|Config files (*.sys;*.ini)|*.sys;*.ini|Batch files (*.bat)|*.bat|All files (*.*)|*.*';
  SSingleLine = 'Line';
  SMultipleLines = 'Lines';

//=== TJvSpeedbarForm.pas ====================================================
  RsESBItemNotCreate = 'Cannot create a new Speedbar button';
  RsConfirmSBDelete = 'Are you sure you want to delete current section?';
  RsSpeedbarDesigner = 'Speedbar designer...';
  RsNewSectionName = 'Untitled (%d)';

//=== JvTimeFrameworkReg.pas =================================================
resourcestring
  SGridLayout = 'Grid Layout';
  SCustomDraw = 'Custom Draw';

//=== JvTimerListEditor.pas ==================================================
resourcestring
  SEventsEllipsis = 'Events...';

//=== JvTreeItemsEditorForm.pas ==============================================
resourcestring
  SLinksEditorEllipsis = 'Links Editor...';

//=== JvValidatorsEditorForm.pas =============================================
resourcestring
  SJvValidatorsItemsEditorEllipsis = 'JvValidators Items Editor...';
  SJvValidatorItemsEditorEllipsis = 'JvValidator Items Editor';

//=== JvWizardEditorForm.pas =================================================
resourcestring
  SPageListEllipsis = 'Page List...';
  SNewWelcomePage = 'New Welcome Page';
  SNewInteriorPage = 'New Interior Page';
  SDeletePage = 'Delete Page';

//=== DataProvider design time constants =====================================
resourcestring
  SDataProviderDesignerCaption = 'Editing %s%s...';
  SDataProviderContextManCaption = 'Editing contexts for ''%s''...';
  SDataItemRootID = 'ROOT';
  SDataItemRootCaption = 'Root';
  SDataItemNoTextIntf = 'Item has no text support.';
  SDataItemIDNotFound = 'Item ID "%s" not found!';
  SDataItemNotFound = 'Item not found.';
  SDataProviderAddFailed = 'Failed to add a new item.';
  SDataProviderAddErrorReason = 'unable to add new item; %s.';
  SDataProviderDeleteErrorReason = 'Unable to delete item; %s.';

  SDataProviderNoManOrDsgn = 'neither IJvDataItemsManagement nor IJvDataItemsDesigner are supported';
  SDataProviderNoSubItems = 'item does not support IJvDataItems';
  SDataProviderNoMan = 'IJvDataItemsManagement is not supported';

//=== not taken into JVCL ====================================================
{
resourcestring
  // TJvTimerList component editor
  srEventNotCreate = 'Cannot create a new event';
  srTimerDesigner = 'Edit Events...';
  srTimerEvents = '%s.Events';
}

implementation

end.

