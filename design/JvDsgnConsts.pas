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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDsgnConsts;

{$I jvcl.inc}

interface

uses
  JvConsts;

//=== strings used by several editors ========================================
resourcestring
  RsPreviewEllipsis = 'Preview...';
  RsDesignerEllipsis = 'Designer...';
  RsItemsEditorEllipsis = 'Items Editor...';
  RsNone = '(none)';
  RsHelp = 'Help';
  RsConfirm = 'Confirm?';
  {$IFDEF MSWINDOWS}
  RsAllFilesFilter = 'All Files (*.*)|*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  RsAllFilesFilter = 'All Files (*)|*';
  {$ENDIF LINUX}
  RsNextPage = 'Next Page';
  RsPreviousPage = 'Previous Page';
  RsJVCLActionsCategory = 'JVCL';
  RsPropertyEditors = '\Property Editors';
  RsJvEditorString = 'Click to edit...';
  RsSystemColors = 'System colors';
  RsStandardColors = 'Standard colors';

  RsEInvalidPropertyValue = 'Invalid property value';

//=== JVCL IDE palettes ======================================================
resourcestring
  RsPaletteSystem = 'Jv System';
  RsPaletteDialog = 'Jv Dialogs';
  RsPaletteButton = 'Jv Buttons';
  RsPaletteEdit = 'Jv Edits';
  RsPaletteCustom = 'Jv Custom';
  RsPaletteBarPanel = 'Jv Bars, Panels';
  RsPaletteLabel = 'Jv Labels';
  RsPaletteListComboTree = 'Jv Lists, Combos, Trees';
  RsPaletteScrollerTracker = 'Jv Scrollers';
  RsPaletteSliderSplitter = 'Jv Trackers, Sliders, Splitters';
  RsPaletteImageAnimator = 'Jv Images, Animators';
  RsPaletteVisual = 'Jv Visual';
  RsPaletteNonVisual = 'Jv Non-Visual';
  RsPalettePersistence = 'Jv Persistence';
  RsPaletteAppForm = 'Jv Application, Forms';
  RsPaletteInterNetWork = 'Jv Internet, Network';
  RsPaletteEncryptCompress = 'Jv Encrypt, Compress';
  RsPaletteDBVisual = 'Jv Data Controls';
  RsPaletteDBNonVisual = 'Jv Data Access';
  RsPaletteHMIComponents = 'Jv HMI';
  RsPaletteBDE = 'Jv BDE';
  RsPalettePrintPreview = 'Jv Print Preview';
  RsPaletteInterpreter = 'Jv Interpreter';
  RsPaletteGlobusComponents1 = 'JVCL Globus 1';
  RsPaletteGlobusComponents2 = 'JVCL Globus 2';
  RsPaletteValidators = 'Jv Validators';
  RsPaletteEDI = 'Jv EDI';
  RsPaletteJans = 'Jv Jans';
  RsPaletteJansSim = 'Jv Jans SIM';
  RsPaletteJansCsv = 'Jv Jans CSV';
  RsPalettePlugin = 'Jv Plugin';
  RsPaletteNavPane = 'Jv NavPane';
  {$IFDEF USEJVCL}
  RsPaletteUIB = 'Jv UIB';
  RsPaletteMTThreads = 'Jv Threading';
  RsPaletteTimeFramework = 'Jv TimeFrameWork';
  RsPaletteWizard = 'Jv Wizard';
  RsPaletteXPControls = 'Jv XP Controls';
  RsPaletteDocking = 'Jv Docking';
  RsPaletteDotNet = 'Jv DotNet';
  RsPaletteDotNetDB = 'Jv DotNet DB';
  {$ENDIF USEJVCL}

//=== JvAnimatedEditor.pas ===================================================
resourcestring
  RsEditPicture = 'Edit picture...';
  RsLoadAniCursor = 'Load from ANI...';

//=== JvAVICaptureEditors.pas ================================================
resourcestring
  RsGetValueFmt = '%d - %s';
  RsDisconnected = 'Disconnected';
  RsEdIsNotWithinTheValidRangeOfdd = '%d is not within the valid range of %d..%d';

//=== JvBandObjectDLLWizard.pas ==============================================
resourcestring
  RsCreatesABandObjectDLLProject = 'Creates a Band Object DLL Project.';
  RsBandObjectDLLWizard = 'Band Object DLL Wizard';

//=== JvBandObjectDLLWizardForm.pas ==========================================
resourcestring
  RsBandHelpCaptionFmt = '%s %s';
  RsEBandNameHasToBeAValidIdentifier = 'Band name has to be a valid identifier!';
  RsEPleaseEnterBandDescription = 'Please enter band description!';
  RsHelpText = sLineBreak +
    'Band Name' + sLineBreak +
    'Enter a band name, e.g. MyBand.' + sLineBreak +
    'This will be the class name of the band object.' + sLineBreak +
    'No need to prefix it with ''T'' as it will be generated.' + sLineBreak + sLineBreak +
    'Description' + sLineBreak +
    'Enter a menuitem text, e.g. &My Band' + sLineBreak +
    'This text will appear in the Explorer Bar or Toolbars menu.' + sLineBreak + sLineBreak +
    'Band Type' + sLineBreak +
    'Select the type of band object to create.';

//=== JvBaseDsgnForm.pas =====================================================
resourcestring
  RsBaseDesignFormName = 'JEDI-VCL Editor';

//=== JvChangeNotifyEditor.pas ===============================================
resourcestring
  RsNotificationsEllipsis = 'Notifications...';

//=== JvCheckedItemsForm.pas =================================================
resourcestring
  RsItemEditor = 'Item editor';
  RsEnabled = 'Enabled';

//=== JvColorProviderAddDialogForm.pas =======================================
resourcestring
  RsCustomColors = 'Custom colors';

//=== JvColorProviderDesignerForm.pas ========================================
resourcestring
  RsCustomColorsEllipsis = 'Custom colors...';
  RsColorMsg = 'Copy standard and system colors from the default context?';
  RsDesigning = 'Designing %s';

//=== JvColorProviderEditors.pas =============================================
resourcestring
  RsEMappingDoesNotExistForThisColorProv = 'Mapping does not exist for this color provider.';

//=== JvCsvDataEditor.pas ====================================================
resourcestring
  RsJvCsvDataSetSelectCSVFileToOpen = 'JvCsvDataSet - Select CSV File to Open';
  RsCsvFilter = '*.csv';

//=== JvCsvDataForm.pas ======================================================
resourcestring
  RsMustTypeAValidFieldNameAndSelectAFi =
    'Must type a valid field name and select a field type. ' +
    'Field name must start with a letter A-Z and consist of letters and numbers only. ' +
    'All field names will be converted to uppercase before being used.';
  RsAddFailed = 'Add Failed';
  RsFieldNameIsNotAValidIdentifier = '%s: Field name is not a valid identifier';
  RsCantAddTwoFieldsWithTheSameNameSele =
    'Can''t add two fields with the same name! Select existing item and click ''Modify'' button to change its properties.';
  RsUpdateFailed = 'Update Failed';
  RsNoItemIsSelectedInTheFieldsListYouC = 'No item is selected in the fields list. You can''t update nothing.';
  RsModifyingTheCurrentlySelectedItemWo =
    'Modifying the currently selected item would create two items with the same name.';
  RsYouHaventActuallyChangedAnythingIfY = 'You haven''t actually changed anything. If you ' +
    'made changes and didn''t click Modify, the changes have ' +
    'not been made yet. (Click no, to go back.) ' + sLineBreak +
    'Are you sure you want to close the CSV Fields editor? ';

//=== JvDataConsumerContextSelectForm.pas ====================================
resourcestring
  RsEConsumerDoesNotSupportContextSelect = 'Consumer does not support context selection.';
  RsEIJvDataConsumerProviderIsNotSupported = 'IJvDataConsumerProvider is not supported by the specified consumer.';

//=== JvDataConsumerItemSelectForm.pas =======================================
resourcestring
  RsDataProviderItemSelector = 'DataProvider Item Selector';

//=== JvDataContextManagerForm.pas ===========================================
resourcestring
  RsDataProviderContextManager = 'DataProvider Context Manager';

//=== JvDataEmbeddedEditor.pas ===============================================
resourcestring
   RsLoadFromFileEllipsis = 'Load From File...';
   RsClearEmbeddedData = 'Clear data';
   RsViewEmbeddedDataAsText = 'View As Text...';
   

//=== JvDataProviderDesignerForm.pas =========================================
resourcestring
  RsDefault = '<Default>';
  RsDataProviderDesigner = 'DataProvider Designer';
  RsEInternalErrorUnableToRetrieveContext = 'Internal error: unable to retrieve context list';

//=== JvDataProviderEditors.pas ==============================================
resourcestring
  RsESpecifiedProviderIsNotATComponentDe = 'Specified provider is not a TComponent descendant';
  RsTreeDesignerEllipsis = 'Tree designer...';
  RsContextManagerEllipsis = 'Context manager...';
  RsInvalidVerbd = 'Invalid verb#: %d';

//=== JvDataProviderItemDesign.pas ===========================================
resourcestring
  RsUnknown = '<unknown>';
  RsNoItem = '<no item>';

//=== JvDateTimeForm.pas =====================================================
resourcestring
  RsSelectDate = 'Select Date';
  RsSelectTime = 'Select Time';
  RsSelectDateTime = 'Select Date and Time';
  RsMaxInt = 'MaxInt';
  RsMinInt = 'MinInt';
  RsMaxLong = 'MaxLong';
  RsMinLong = 'MinLong';
  RsMaxShort = 'MaxShort';
  RsMinShort = 'MinShort';
  RsMaxWord = 'MaxWord';
  RsFileName = '(Filename)';
  RsDirectory = '(Directory)';

//=== JvDBGridProp.pas =======================================================
resourcestring
  RsEJvDBGridDataSourceNeeded = 'DataSource property must be set before selecting controls.';
  RsEJvDBGridDataSetNeeded = 'A dataset must be linked to the grid datasource before selecting controls.';
  RsJvDBGridAlreadyAdded = 'The field "%s" has already been added.';

//=== JvDBMemDatasetEditor.pas ===============================================
resourcestring
  RsBorrowStructure = 'Borrow structure...';

//=== JvDockPropertyEditors.pas ==============================================
{$IFDEF USEJVCL}
resourcestring
  RsDockNewPage = 'Ne&w Page';
  RsDockNextPage = 'Ne&xt Page';
  RsDockPreviousPage = '&Previous Page';
  RsDockDeletePage = '&Delete Page';
{$ENDIF USEJVCL}

//=== JvDsgnEditors.pas ======================================================
resourcestring
  RsStripFilePath = '&Strip file path';
  {$IFDEF MSWINDOWS}
  RsExecutableFilesExeExeAllFiles = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  RsExecutableFilesExeExeAllFiles = 'Executable files (*)|*';
  {$ENDIF LINUX}
  RsItems = 'Items';
  RsFmtEditEllipsis = '%s Editor...';
  RsSaveImageList = 'Save to bitmap...';

//=== JvFooterEditor.pas =====================================================
resourcestring
  RsAddButtonText = 'Add button';
  RsMSOffice = 'MS Office 2000';
  RsMSEnterpriseManagerWizard = 'MS Enterprise Manager Wizard';
  RsDialogMode = 'Dialog Mode';
  RsPrevious = 'Previous';
  RsNext = 'Next';

//=== JvFormPropertiesForm.pas ===============================================
resourcestring
  RsStorageDesigner = 'Form Storage Designer...';

//=== JvgComponentListEditorForm.pas =========================================
resourcestring
  RsEditComponentListEllipsis = 'Edit component list...';

//=== JvgHelpPanelEditor.pas =================================================
resourcestring
  RsRTFAndTextFilesrtftxtrtftxt = 'RTF and Text files (*.rtf,*.txt)|*.rtf;*.txt';
  RsLoadRTFFile = 'Load RTF file';
  RsPreviewRTFText = 'Preview RTF text';

//=== JvgLabelEditorForm.pas =================================================
resourcestring
  RsEditLabel = 'Edit &Label...';

//=== JvgLogicItemEditorForm.pas =============================================
resourcestring
  RsLogicElements = 'Logic Element: %s';
  RsResult = '[RESULT]';
  RsNotDefined = '[ not defined ]';

//=== JvgLogicsEditorForm.pas ================================================
resourcestring
  RsCaption = 'Caption';
  RsComments = 'Comments';
  RsEditComponentEllipsis = 'Edit component...';

//=== JvgMultiResourceEditorForm.pas =========================================
resourcestring
  RsCellControlCaption = 'Control';
  RsCellDefaultCaption = 'Default';

//=== JvGradientCaptionForm.pas ==============================================
resourcestring
  RsCaptionDesigner = 'Edit Captions...';
  RsGradientCaptions = 'Captions';

//=== JvgReportEditorForm.pas ================================================
resourcestring
  RsEditReport = 'Edit report...';
  RsPreviewReportEllipsis = 'Preview report...';
  RsDeleteObject = 'Delete object?';
  RsPagePreview = 'Page Preview';

//=== JvgReportParamsForm.pas ================================================
resourcestring
  RsEditParamsEllipsis = 'Edit params...';

//=== JvGroupHeaderEditor.pas ================================================
resourcestring
  RsStandardFlat = 'Standard/Flat';
  RsWeb = 'Web';

//=== JvgShadowEditor.pas ====================================================
resourcestring
  RsUpdateAllEditControl = 'Update all edit control';

//=== JVHLEditEditor.pas =====================================================
resourcestring
  RsHLEditorMsg = 'Please select "JvHLEditor" first';
  RsHLEditorMsgTitle = 'Cannot edit';
  RsExecute = 'Execute';
  RsJvHLEdPropDlgIni = 'JvHLEdPropDlg.ini';

//=== { TJvIconListForm.pas } ================================================
resourcestring
  RsLoadIcon = 'Load Icon';

//=== JvID3v2EditorForm.pas ==================================================
resourcestring
  RsfiErrorFrame = 'Error';
  RsfiPaddingFrame = 'Padding';
  RsfiNoFrame = 'No known frame';
  RsfiAudioCrypto = 'Audio encryption';
  RsfiPicture = 'Attached picture';
  RsfiAudioSeekPoint = 'Audio seek point index';
  RsfiComment = 'Comments';
  RsfiCommercial = 'Commercial frame';
  RsfiCryptoReg = 'Encryption method registration';
  RsfiEqualization2 = 'Equalization (2)';
  RsfiEqualization = 'Equalization';
  RsfiEventTiming = 'Event timing codes';
  RsfiGeneralObject = 'General encapsulated object';
  RsfiGroupingReg = 'Group identification registration';
  RsfiInvolvedPeople = 'Involved people list';
  RsfiLinkedInfo = 'Linked information';
  RsfiCDID = 'Music CD identifier';
  RsfiMPEGLookup = 'MPEG location lookup table';
  RsfiOwnership = 'Ownership frame';
  RsfiPrivate = 'Private frame';
  RsfiPlayCounter = 'Play counter';
  RsfiPopularimeter = 'Popularimeter';
  RsfiPositionsync = 'Position synchronisation frame';
  RsfiBufferSize = 'Recommended buffer size';
  RsfiVolumeAdj2 = 'Relative volume adjustment (2)';
  RsfiVolumeAdj = 'Relative volume adjustment';
  RsfiReverb = 'Reverb';
  RsfiSeekFrame = 'Seek frame';
  RsfiSignature = 'Signature frame';
  RsfiSyncedLyrics = 'Synchronized lyric/text';
  RsfiSyncedTempo = 'Synchronized tempo codes';
  RsfiAlbum = 'Album/Movie/Show title';
  RsfiBPM = 'BPM (beats per minute)';
  RsfiComposer = 'Composer';
  RsfiContentType = 'Content type';
  RsfiCopyright = 'Copyright message';
  RsfiDate = 'Date';
  RsfiEncodingTime = 'Encoding time';
  RsfiPlaylistDelay = 'Playlist delay';
  RsfiOrigReleaseTime = 'Original release time';
  RsfiRecordingTime = 'Recording time';
  RsfiReleaseTime = 'Release time';
  RsfiTaggingTime = 'Tagging time';
  RsfiInvolvedPeople2 = 'Involved people list';
  RsfiEncodedBy = 'Encoded by';
  RsfiLyricist = 'Lyricist/Text writer';
  RsfiFileType = 'File type';
  RsfiTime = 'Time';
  RsfiContentGroup = 'Content group description';
  RsfiTitle = 'Title/songname/content description';
  RsfiSubTitle = 'Subtitle/Description refinement';
  RsfiInitialKey = 'Initial key';
  RsfiLanguage = 'Language(s)';
  RsfiSongLen = 'Length';
  RsfiMusicianCreditList = 'Musician credits list';
  RsfiMediaType = 'Media type';
  RsfiMood = 'Mood';
  RsfiOrigAlbum = 'Original album/movie/show title';
  RsfiOrigFileName = 'Original filename';
  RsfiOrigLyricist = 'Original lyricist(s)/text writer(s)';
  RsfiOrigArtist = 'Original artist(s)/performer(s)';
  RsfiOrigYear = 'Original release year';
  RsfiFileOwner = 'File owner/licensee';
  RsfiLeadArtist = 'Lead performer(s)/Soloist(s)';
  RsfiBand = 'Band/orchestra/accompaniment';
  RsfiConductor = 'Conductor/performer refinement';
  RsfiMixArtist = 'Interpreted, remixed, or otherwise modified by';
  RsfiPartInSet = 'Part of a set';
  RsfiProducedNotice = 'Produced notice';
  RsfiPublisher = 'Publisher';
  RsfiTrackNum = 'Track number/Position in set';
  RsfiRecordingDates = 'Recording dates';
  RsfiNetRadioStation = 'Internet radio station name';
  RsfiNetRadioOwner = 'Internet radio station owner';
  RsfiSize = 'Size';
  RsfiAlbumSortOrder = 'Album sort order';
  RsfiPerformerSortOrder = 'Performer sort order';
  RsfiTitleSortOrder = 'Title sort order';
  RsfiISRC = 'ISRC (international standard recording code)';
  RsfiEncoderSettings = 'Software/Hardware and settings used for encoding';
  RsfiSetSubTitle = 'Set subtitle';
  RsfiUserText = 'User defined text information';
  RsfiYear = 'Year';
  RsfiUniqueFileID = 'Unique file identifier';
  RsfiTermsOfUse = 'Terms of use';
  RsfiUnsyncedLyrics = 'Unsynchronized lyric/text transcription';
  RsfiWWWCommercialInfo = 'Commercial information';
  RsfiWWWCopyright = 'Copyright/Legal information';
  RsfiWWWAudioFile = 'Official audio file webpage';
  RsfiWWWArtist = 'Official artist/performer webpage';
  RsfiWWWAudioSource = 'Official audio source webpage';
  RsfiWWWRadioPage = 'Official internet radio station homepage';
  RsfiWWWPayment = 'Payment';
  RsfiWWWPublisher = 'Official publisher webpage';
  RsfiWWWUser = 'User defined URL link';
  RsfiMetaCrypto = 'Encrypted meta frame';
  RsfiMetaCompression = 'Compressed meta frame';

  RsMPEG10 = 'MPEG 1.0';
  RsMPEG20 = 'MPEG 2.0';
  RsMPEGUnknown = '??';
  RsMPEG25 = 'MPEG 2.5';

  RsLayerUnknown = '??';
  RsLayer1 = 'Layer 1';
  RsLayer2 = 'Layer 2';
  RsLayer3 = 'Layer 3';

  RsChannelModeStereo = 'Stereo';
  RsChannelModeJointStereo = 'Joint Stereo';
  RsChannelModeDualChannel = 'Dual Channel';
  RsChannelModeMono = 'Mono';

  RsEmphasisNone = 'None';
  RsEmphasisMicrosec = '50/15 microsec';
  RsEmphasisUnknown = '??';
  RsEmphasisCCITTJ17 = 'CCIT J.17';

  RsBoolNo = 'No';
  RsBoolYes = 'Yes';

  RsVbrNo = '';
  RsVbrVbr = ' (VBR)';

  RsIDV2FileInfoFmt =
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

  RsIDV2FileInfoCaption = 'File info';
  RsCommit = 'Commit?';

  // (rom) from JvConsts.pas
  RsID3CommitTag = '&Commit';
  RsID3FileInfoTag = 'File &info';
  RsID3FrameEditorTag = 'Frame edi&tor';
  RsID3RemoveTag = '&Remove tag...';
  RsID3RemoveTagConfirmation = 'Remove tag?';
  RsID3Err_FileDoesNotExists = 'File %s does not exists';
  RsID3Err_NoFileSpecified = 'No file specified';
  RsID3Err_NoValidMPEGTag = 'This file has not a valid MPEG tag';

//=== JvIDEZoom.pas ==========================================================
resourcestring
  RsZoomEditWindow = 'Zoom Edit Window';

//=== JvImagePreviewForm.pas =================================================
resourcestring
  RsPreview = 'Preview';

//=== JvJVCLAboutEditor.pas ==================================================
resourcestring
  RsVersions = 'Version %s';

//=== JvLookoutEditor.pas ====================================================
resourcestring
  RsAddPage = 'Add page';
  RsActivate = 'Activate';
  RsAddButton = 'Add Button';
  RsScrollUp = 'Scroll Up';
  RsScrollDown = 'Scroll Down';
  RsAddPage_ = 'Add Page';

//=== JvMailEditor.pas =======================================================
resourcestring
  RsSend = 'Send';
  RsAddress = 'Address';

//=== JvOutlookBarEditors.pas ================================================
resourcestring
  RsOLEditor = 'OutlookBar Editor...';

//=== JvOutlookBarForm.pas ===================================================
resourcestring
  RsDesignerIsNilInFormClosed = 'Designer is nil in FormClosed';
  RsFmtCaption = 'Editing %s';
  RsOutlookBarCaption = 'OutlookBar Editor';

//=== JvPageLinkEditor.pas ===================================================
resourcestring
  RsCreateLinkToPaged = 'Create link to page %d';

//=== JvPageListTreeViewReg.pas ==============================================
resourcestring
  RsEFmtInterfaceNotSupported = '%s does not support the required interface (%s)';
  RsPageListEditorEllipsis = 'Page List Editor...';
  RsNextPageAmp = 'Ne&xt Page';
  RsPrevPage = '&Previous Page';
  RsNewPage = '&New Page';
  RsDelPage = '&Delete Page';

//=== JvPageManagerForm.pas ==================================================
resourcestring
  RsProxyEditor = 'Edit Proxies...';
  RsPageProxies = '%s Page Proxies';
  RsProxyName = 'Page Proxy Name';
  RsPageName = 'Page Name';

//=== JvPatcherEditor.pas ====================================================
resourcestring
  RsDiff = '(diff)';
  RsEqual = '(equal)';

//=== JvPictureEditForm.pas ==================================================
resourcestring
  RsLoadPicture = 'Load picture';
  RsSavePictureAs = 'Save picture as';

//=== JvPluginParamsForm.pas =================================================
resourcestring
  RsPluginParamsFormInfoText =
    'The settings above will create the following project:' +
    sLineBreak + sLineBreak +
    '* A project called Plg%0:s.%1:s' + sLineBreak +
    '* A unit called Plugin%0:s, containing the data module T%0:s.';

//=== JvPluginWizard.pas =====================================================
resourcestring
  RsJvPluginWizard = 'Jv Plugin Wizard';
  RsProjects = 'Projects';
  RsNewPlugin = 'New Plugin';
  RsPrivateDeclarations = '{ Private declarations }';
  RsPublicDeclarations = '{ Public declarations }';
  RsIMPORTANTNOTEIfYouChangeTheNameOfTh =
    '// IMPORTANT NOTE: If you change the name of the Plugin container,' + sLineBreak +
    '// you must set the type below to the same type. (Delphi changes' + sLineBreak +
    '// the declaration, but not the procedure itself. Both the return' + sLineBreak +
    '// type and the type created must be the same as the declared type above.';
  RsJediPuginWizard = 'JEDI Plugin Wizard';

//=== JvPreviewReg.pas =======================================================
resourcestring
  RsCreatePreview = 'Create Preview';
  RsClearPreview = 'Clear Preview';

//=== JvScheduleEditorForm.pas ===============================================
resourcestring
  RsEInvalidScheduleSettingsFound = 'Invalid schedule settings found.';
  RsStop = 'Stop';
  RsRun = 'Run';

//=== JvScheduleEditors.pas ==================================================
resourcestring
  RsEventEditor = 'Event editor...';

//=== JvScrollMaxEditor.pas ==================================================
resourcestring
  RsAddBand = 'Add Band';

//=== JvSegmentedLEDDisplayEditors.pas =======================================
resourcestring
  RsAddDigit = 'Add digit';
  RsRemoveDigit = 'Remove digit';
  RsEditMappingEllipsis = 'Edit mapping...';

//=== JvSegmentedLEDDisplayMappingForm.pas ===================================
resourcestring
  RsSegmentedLEDDisplayMappingEditor = 'Segmented LED Display Mapping Editor';

//=== JvSegmentedLEDDisplayMappingForm.pas ===================================
resourcestring
  {$IFDEF MSWINDOWS}
  RsTextFilter =
    'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|Batch files (*.BAT)|*.BAT|All files (*.*)|*.*';
  //  RsTextFilter = 'Text files (*.txt)|*.txt|Config files (*.sys;*.ini)|*.sys;*.ini|Batch files (*.bat)|*.bat|All files (*.*)|*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  RsTextFilter =
    'Text files (*.txt)|*.txt|All files (*)|*';
  {$ENDIF LINUX}
  RsSingleLine = 'Line';
  RsMultipleLines = 'Lines';

//=== { TJvSpeedbarForm.pas } ================================================
  RsESBItemNotCreate = 'Cannot create a new Speedbar button';
  RsConfirmSBDelete = 'Are you sure you want to delete current section?';
  RsSpeedbarDesigner = 'Speedbar designer...';
  RsNewSectionName = 'Untitled (%d)';

//=== JvTimeFrameworkReg.pas =================================================
resourcestring
  RsGridLayout = 'Grid Layout';
  RsCustomDraw = 'Custom Draw';

//=== JvTimerListEditor.pas ==================================================
resourcestring
  RsEventsEllipsis = 'Events...';

//=== JvTreeItemsEditorForm.pas ==============================================
resourcestring
  RsLinksEditorEllipsis = 'Links Editor...';

//=== JvValidatorsEditorForm.pas =============================================
resourcestring
  RsJvValidatorsItemsEditorEllipsis = 'JvValidators Items Editor...';
  RsJvValidatorItemsEditorEllipsis = 'JvValidator Items Editor...';

//=== JvWizardEditorForm.pas =================================================
{$IFDEF USEJVCL}
resourcestring
  RsPageListEllipsis = 'Page List...';
  RsNewWelcomePage = 'New Welcome Page';
  RsNewInteriorPage = 'New Interior Page';
  RsDeletePage = 'Delete Page';
{$ENDIF USEJVCL}

//=== JvXPPropertyEditors.pas ================================================
{$IFDEF USEJVCL}
resourcestring
  RsItemEditorEllipsis = 'Item Editor...';
  RsDefaultColorItem = 'Restore Default Colors';
  RsDefaultFontsItem = 'Restore Default Fonts';
{$ENDIF USEJVCL}

//=== DataProvider design time constants =====================================
resourcestring
  RsDataProviderDesignerCaption = 'Editing %s%s...';
  RsDataProviderContextManCaption = 'Editing contexts for ''%s''...';
  RsDataItemRootID = 'ROOT';
  RsDataItemRootCaption = 'Root';
  RsDataItemNoTextIntf = 'Item has no text support.';
  RsDataItemIDNotFound = 'Item ID "%s" not found!';
  RsEDataItemNotFound = 'Item not found.';
  RsEDataProviderAddFailed = 'Failed to add a new item.';
  RsEDataProviderAddErrorReason = 'unable to add new item; %s.';
  RsEDataProviderDeleteErrorReason = 'Unable to delete item; %s.';
  RsEDataProviderNoManOrDsgn = 'neither IJvDataItemsManagement nor IJvDataItemsDesigner are supported';
  RsEDataProviderNoSubItems = 'item does not support IJvDataItems';
  RsEDataProviderNoMan = 'IJvDataItemsManagement is not supported';

//=== not taken into JVCL ====================================================
{
resourcestring
  // TJvTimerList component editor
  RsEventNotCreate = 'Cannot create a new event';
  RsTimerDesigner = 'Edit Events...';
  RsTimerEvents = '%s.Events';
}

implementation

end.

