{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvResources.PAS, released on 2003-12-10.

The Initial Developer of the Original Code is: Robert Marquardt (robert_marquardt att gmx dott de)
Copyright (c) 2003 Robert Marquardt
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  unit to centralize all resourcestrings of the JVCL for easier translation

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvResources;

{$I jvcl.inc}

interface

uses
  JvConsts;

//=== used in several files ==================================================
resourcestring
  RsButtonOKCaption = '&OK';
  RsButtonCancelCaption = 'Cancel';
  RsBackButtonCaption = '< &Back';
  RsPrevButtonCaption = '< &Prev';
  RsNextButtonCaption = '&Next >';
  RsDateDlgCaption = 'Select a Date';
  RsDetailsLeftCaption = '<< &Details';
  RsDetailsRightCaption = '&Details >>';

  RsUndoItem = '&Undo';
  RsCutItem = 'Cu&t';
  RsCopyItem = '&Copy';
  RsPasteItem = '&Paste';
  RsDeleteItem = '&Delete';
  RsSelectAllItem = 'Select &All';
  {
  SWEDISH:
  RsUndoItem = '&Еngra';
  RsCutItem = '&Klipp ut';
  RsCopyItem = 'K&opiera';
  RsPasteItem = 'Kl&istra in';
  RsDeleteItem = '&Ta bort';
  RsSelectAllItem = '&Markera allt';

  GERMAN:
  RsUndoItem = '&Rьckgдngig';
  RsCutItem = '&Ausschneiden';
  RsCopyItem = '&Kopieren';
  RsPasteItem = 'E&infьgen';
  RsDeleteItem = '&Lцschen';
  RsSelectAllItem = 'Alles &markieren';

  DUTCH:
  RsUndoItem = '&Ongedaan maken';
  RsCutItem = 'K&nippen';
  RsCopyItem = '&Kopiлren';
  RsPasteItem = '&Plakken';
  RsDeleteItem = '&Wissen';
  RsSelectAllItem = '&Alles selecteren';
  }

  RsEmptyItem = '<Empty>';
  RsNoName = '(unnamed)';

  RsDatabaseName = 'Database name: %s';
  RsDataItemRenderHasNoText = '(item does not support the IJvDataItemText interface)';
  RsError = 'Error';
  RsFalse = 'False';
  RsTrue = 'True';

  RsEErrorSetupDll = 'SetupApi.dll not found';
  RsEInternalError = 'internal error';
  RsEUnterminatedStringNears = 'unterminated string near %s';
  RsEStackOverflow = 'stack overflow';
  RsEStackUnderflow = 'stack underflow';
  RsEReturnStackUnderflow = 'return stack underflow';
  RsENotImplemented = 'not implemented';
  RsEDelSubTreeNotImplemented = 'DeleteSubTreeInt has not been implemented yet';
  { Polaris patch }
  RsEDateOutOfRange = '%s - Enter a date between "%s" and "%s"';
  RsEDateOutOfMin = '%s - Enter a date after "%s"';
  RsEDateOutOfMax = '%s - Enter a date before "%s"';
  RsEID3NoController = 'No controller specified';
  RsEReturnStackOverflow = 'return stack overflow';
  RsESorryForOneDimensionalArraysOnly = 'Sorry, for one-dimensional arrays only';
  RsELocalDatabase = 'Cannot perform this operation on a local database';

  RsEInterfaceNotSupported = '%s does not support the %s interface';
  RsECircularReference = 'Circular reference not allowed';

  RsESourceBitmapTooSmall = 'Source bitmap too small';

//=== JvAni.pas ==============================================================
resourcestring
  RsAniExtension = 'ani';
  RsAniFilterName = 'ANI Image';
  RsAniCurFilter = 'Animated Cursors (*.ani)|*.ani|Any files (*.*)|*.*';

  RsEInvalidAnimatedIconImage = 'Invalid animated icon image';

//=== JvAppDBStorage.pas =====================================================
resourcestring
  RsENotSupported = 'Method not supported';
  RsEBufTooSmallFmt = 'Buffer too small (%d bytes required)';

//=== JvAppIniStorage.pas ====================================================
resourcestring
  RsEReadValueFailed = 'TJvAppIniFileStorage.ReadValue: Section undefined';
  RsEWriteValueFailed = 'TJvAppIniFileStorage.WriteValue: Section undefined';

//=== JvAppRegistryStorage.pas ===============================================
resourcestring
  RsEUnableToCreateKey = 'Unable to create key ''%s''';
  RsEEnumeratingRegistry = 'Error enumerating registry';

//=== JvAppStorage.pas =======================================================
resourcestring
  RsEInvalidType = 'Invalid type';
  RsEUnknownBaseType = 'Unknown base type for given set';
  RsEInvalidPath = 'Invalid path';
  RsENotAUniqueRootPath = '''%s'' is not a unique root path';
  RsECircularReferenceOfStorages = 'Circular reference of storages';

//=== JvAppStorageSelectList.pas =============================================
resourcestring
  RsLoadSettings = 'Load Settings';
  RsSaveSettings = 'Save Settings';
  RsDeleteSettings = 'Delete Settings';
  RsLoadCaption = '&Load';
  RsSaveCaption = '&Save';
  RsDeleteCaption = '&Delete';

  RsEDynControlEngineNotDefined = 'TJvAppStorageSelectList.CreateDialog: DynControlEngine not defined!';
  RsEDynAppStorageNotDefined = 'TJvAppStorageSelectList.GetSelectListPath: No AppStorage assigned';

//=== JvAppXMLStorage.pas ====================================================
resourcestring
  RsENodeCannotBeEmpty = 'The node must be given a name';
  RsEPathDoesntExists = 'Path ''%s'' does not exists';
  RsENotABooleanValue = '''%s'' is not a valid boolean value';
  RsENodeNameCannotContainSpaces = 'XML Node names cannot contain white space and the WhiteSpaceReplacement property is empty. Please set the WhiteSpaceReplacementProperty to a non empty value.';
  RsEWhiteSpaceReplacementCannotContainSpaces =
    'The WhiteSpaceReplacement property cannot contain any white spaces.';

//=== JvAVICapture.pas =======================================================
resourcestring
  RsNotConnected = 'Not connected';
  RsErrorMessagePrefix = 'Error #';

  RsEInvalidDriverIndex = '%d is an invalid driver index. The maximum value is %d'#13#10+
                          'This may also happen if the device could not be initialized properly.';

//=== JvBackgrounds.pas ======================================================
resourcestring
  SChainError = 'Message from %s.%s:'#13#10#13#10'Oops... Messing up %s''s window procedure chain.%s';
  SWorkaround = #13#10#13#10'To avoid this, $DEFINE the NO_DESIGNHOOK conditional compilation symbol and rebuild.';

//=== JvBalloonHint.pas ======================================================
resourcestring
  RsEParentRequired = 'Control ''%s'' has no parent window';
  RsEParentGivenNotAParent = 'Parent given is not a parent of ''%s''';

//=== JvBaseEdits.pas ========================================================
resourcestring
  RsEOutOfRangeXFloat = 'Value must be between %.*f and %.*f';

//=== JvBDECheckPasswordForm.pas =============================================
resourcestring
  RsChangePassword = 'Change password';
  RsOldPasswordLabel = '&Old password:';
  RsNewPasswordLabel = '&New password:';
  RsConfirmPasswordLabel = '&Confirm password:';
  RsPasswordChanged = 'Password has been changed';
  RsPasswordNotChanged = 'Password has not been changed';
  RsPasswordsMismatch = 'The new and confirmed passwords do not match';

//=== JvBDEExceptionForm.pas =================================================
resourcestring
  RsDBExceptCaption = 'Database Engine Error';
  RsBDEErrorLabel = 'BDE Error';
  RsServerErrorLabel = 'Server Error';
  RsErrorMsgLabel = 'Error message';

//=== JvBDEFilter.pas ========================================================
resourcestring
  RsECaptureFilter = 'Cannot perform this operation when controls are captured';
  RsENotCaptureFilter = 'Cannot perform this operation when controls are not captured';

//=== JvBDELoginDialog.pas ===================================================
resourcestring
  RsEInvalidUserName = 'Invalid user name or password';
  RsLastLoginUserName = 'Last Login User';  // Also used by JvLoginForm
  RsSelectDatabase = 'SelectDatabase'; { dialog never writes this value }
  RsLastAliasName = 'LastAlias'; { used if SelectDatabase = True  }

//=== JvBDEMove.pas ==========================================================
resourcestring
  RsEInvalidReferenceDescriptor = 'Invalid reference descriptor';

//=== JvBdeUtils.pas =========================================================
resourcestring
  RsRetryLogin = 'Do you wish to retry the connect to database?';

  RsETableNotInExclusiveMode = 'Table must be opened in exclusive mode to add passwords';
  RsETableNotOpen = 'Table must be opened to pack';
  RsETableNotOpenExclusively = 'Table must be opened exclusively to pack';
  RsENoParadoxDBaseTable = 'Table must be either of Paradox or dBASE type to pack';

//=== JvBehaviorLabel.pas ====================================================
resourcestring
  RsENeedBehaviorLabel = 'Cannot call %s.Create with ALabel = nil';
  RsENoOwnerLabelParent = 'OwnerLabel.Parent is nil in %s.Start';

//=== JvBrowseFolder.pas =====================================================
resourcestring
  RsEShellNotCompatible = 'Shell not compatible with BrowseForFolder';

//=== JvButtons.pas ==========================================================
resourcestring
  RsEOwnerMustBeForm = '%s owner must be a TForm';

//=== JvCalc.pas =============================================================
resourcestring
  RsCalculatorCaption = 'Calculator';

//=== JvCalendar.pas =========================================================
resourcestring
  RsEInvalidDateStr = 'Invalid date specification to TMonthCalStrings (%s)';
  RsECannotAssign = 'Cannot assign %s to a %s';
  RsEInvalidArgumentToSetDayStates = 'Invalid argument to SetDayStates';
  RsEInvalidAppearance = 'TJvCustomMonthCalendar.CreateWithAppearance: cannot be created without valid Appearance';

//=== JvCaptionButton.pas ====================================================
resourcestring
  RsEOwnerMustBeTCustomForm = 'TJvCaptionButton owner must be a TCustomForm';

//=== JvCaret.pas ============================================================
resourcestring
  RsEInvalidCaretOwner = '%s: cannot be created without a valid Owner';

//=== JvChangeNotify.pas =====================================================
resourcestring
  RsFileNameChange = 'Filename Change';
  RsDirectoryNameChange = 'Directory Name Change';
  RsAttributesChange = 'Attributes Change';
  RsSizeChange = 'Size Change';
  RsWriteChange = 'Write Change';
  RsSecurityChange = 'Security Change';

  RsEFmtCannotChangeName = 'Cannot change %s when active';
  RsEFmtInvalidPath = 'Invalid or empty path (%s)';
  RsEFmtMaxCountExceeded = 'Maximum of %d items exceeded';
  RsEFmtInvalidPathAtIndex = 'Invalid or empty path ("%s") at index %d';
  RsENotifyErrorFmt = '%s:' + sLineBreak + '%s';

//=== JvChart.pas ============================================================
resourcestring
  RsChartDesigntimeLabel = ': JEDI JVCL Charting Component';
  RsNoData = 'No data. (Data.ValueCount=0)';
  RsGraphHeader = 'Graph Header';
  RsCurrentHeaders = 'Current Header: %s';
  RsXAxisHeaders = 'X Axis Header: %s';
  RsGraphScale = 'Graph Scale';
  RsYAxisScales = 'Y Axis Scale: %s';
  RsNoValuesHere = 'No values here!';
  RsNA = ' n/a ';

  RsEDataIndexCannotBeNegative = 'Data: index cannot be negative';
  RsEDataIndexTooLargeProbablyAnInternal = 'Data: index too large. Probably an internal error';
  RsEGetAverageValueIndexNegative = 'GetAverageValue: Index negative';
  RsESetAverageValueIndexNegative = 'SetAverageValue: Index negative';
  RsEChartOptionsPenCountPenCountOutOf = 'JvChart.Options.PenCount - PenCount out of range';
  RsEChartOptionsXStartOffsetValueOutO = 'JvChart.Options.XStartOffset  - value out of range';
  RsEUnableToGetCanvas = 'Unable to get canvas';

//=== JvCheckedMaskEdit.pas ==================================================
resourcestring
  RsEBeginUnsupportedNestedCall = 'TJvCustomCheckedMaskEdit.BeginInternalChange: Unsupported nested call!';
  RsEEndUnsupportedNestedCall = 'TJvCustomCheckedMaskEdit.EndInternalChange: Unsupported nested call!';

//=== JvClipboardViewer.pas ==================================================
  RsClipboardUnknown = 'Cannot display. Data in Clipboard is in an unknown format.';
  RsClipboardEmpty = 'Clipboard is empty';

//=== JvClipbrd.pas ==========================================================
resourcestring
  RsENoRenderFormatEventGiven = 'No OnRenderFormat was given';

//=== JvColorButton.pas ======================================================
resourcestring
  RsOtherCaption = '&Other...';

//=== JvColorCombo.pas =======================================================
resourcestring
  RsCustomCaption = 'Custom...';
  RsNewColorPrefix = 'Custom';

//=== JvColorProvider.pas ====================================================
resourcestring
  RsDelphiConstantNames = 'Delphi constant names';
  RsEnglishNames = 'English names';
  RsCustomColors = 'Custom colors';
  RsStandardColors = 'Standard colors';
  RsSystemColors = 'System colors';
  RsNoSettings = '(no settings)';

  RsESpecifiedMappingError = 'Specified mapping does not belong to the current provider';
  RsEAlreadyRegistered = '''%s'' is already registered';
  RsENoICR = 'Component does not support IInterfaceComponentReference';
  RsENoColProv = 'Component does not support IJvColorProvider';
  RsEMappingCollectionExpected = 'Mapping collection expected';
  RsEExpectedMappingName = 'Expected mapping name';
  RsEExpectedNameMappings = 'Expected name mappings';
  RsEInvalidNameMappingSpecification = 'Invalid name mapping specification';
  RsEUnknownColor = 'Unknown color ''%s''';
  RsEInvalidColor = 'Invalid color (%d)';
  RsEItemNotForList = 'Item does not belong to this list';

//=== JvCombobox.pas =========================================================
resourcestring
  RsCapSelAll = '&Select all';
  RsCapDeselAll = '&Deselect all';
  RsENoMoreLength = 'Too many items selected';

//=== JvComputerInfoEx.pas ===================================================
resourcestring
  RsEReadOnlyProperty = 'This value is read-only and cannot be changed.';

//=== JvContextProvider.pas ==================================================
resourcestring
  RsContextItemEmptyCaption = '(no context assigned to this item)';
  RsENoContextAssigned = 'No context has been assigned to this item';

  RsENoContextItem = 'Specified item is not a context item';
  RsENotSupportedIInterfaceComponentReference = 'Component does not support IInterfaceComponentReference';
  RsENotSupportedIJvDataProvider = 'Component does not support IJvDataProvider';

//=== JvCreateProcess.pas ====================================================
resourcestring
  RsIdle = 'Idle';
  RsNormal = 'Normal';
  RsHigh = 'High';
  RsRealTime = 'RealTime';

  RsEProcessIsRunning = 'Cannot perform this operation when process is running';
  RsEProcessNotRunning = 'Process is not running';

//=== JvCSVBaseControls.pas ==================================================
resourcestring
  RsReplaceExistingDatabase = 'Replace existing database?';
  RsCVSDatabase = 'CSV Database';
  RsFindText = 'Find Text:';
  RsFirstHint = 'First';
  RsPreviousHint = 'Previous';
  RsFindHint = 'Find';
  RsNextHint = 'Next';
  RsLastHint = 'Last';
  RsAddHint = 'Add';
  RsDeleteHint = 'Delete';
  RsPostHint = 'Post';
  RsRefreshHint = 'Refresh';
  RsENoFieldsDefined = 'No fields defined';

  
//=== JvCsvData.pas ==========================================================
resourcestring
  RsErrorRowItem = '<ERROR>';

  RsECsvErrFormat = '%s: %s';
  RsECsvInvalidSeparatorFmt = 'Invalid separator character (%s)'; 
  RsEProblemReadingRow = 'Problem reading row %d';
  RsENoRecord = 'No records';
  RsENoFieldNamesMatch = 'No field names match in these datasets. CopyFromDataset failed.';
  RsETimeTConvError = 'SetFieldData Error - TimeT-to-DateTime conversion error';
  RsEFieldTypeNotHandled = 'SetFieldData Error - Field type not handled';
  RsEUnableToLocateCSVFileInfo = 'Unable to locate CSV file information for field %s';
  RsEPhysicalLocationOfCSVField = 'Physical location of CSV field %s unknown';
  RsEInvalidFieldTypeCharacter = 'Invalid field type character: %s';
  RsECsvNoRecord = 'No database record';
  RsEUnexpectedError = 'Unexpected error parsing CSV Field Definitions';
  RsEFieldDefinitionError = 'Field Definition Error. CsvFieldDef, FieldDefs, and file contents must match';
  RsEInvalidCsvKeyDef = 'Invalid CsvKeyDef property. InternalInitFieldDefs failed';
  RsEInternalErrorParsingCsvKeyDef = 'Internal Error parsing CsvKeyDef. InternalInitFieldDefs failed';
  RsEContainsField = 'CsvKeyDef contains field ''%s'' which is not defined. InternalInitFieldDefs failed';
  RsEInsertBlocked = 'InternalAddRecord cannot Add. Insert blocked';
  RsEPostingHasBeenBlocked = 'Posting to this database has been blocked';
  RsEKeyNotUnique = '%s - Key is not unique ';
  RsECannotInsertNewRow = 'Cannot insert new row. Insert blocked';
  RsECannotPost = 'Cannot post. Not in dsEdit or dsInsert mode';
  RsESortFailedCommaSeparated = 'Sort failed. You must give a comma separated list of field names';
  RsESortFailedFieldNames = 'Sort failed. Unable to parse field names. ';
  RsESortFailedInvalidFieldNameInList = 'Sort failed. Invalid field name in list: %s';
  RsEDataSetNotOpen = 'AppendRowString: DataSet is not open (active not set to true)';
  RsEErrorProcessingFirstLine = 'Error processing first line of CSV file';
  RsEFieldInFileButNotInDefinition = 'ProcessCsvHeaderRow: Field %s found in file, but not in field definitions';
  RsECsvFieldLocationError = 'CSV field location error: %s';
  RsEFieldNotFound = 'Field %s not found in the data file';
  RsECsvStringTooLong = 'CSV string is too long: %s...';
  RsEInternalLimit = 'JvCsvData - Internal Limit of MAXCOLUMNS (%d) reached. CSV Data has too many columns';
  RsETableNameNotSet = 'TableName not specified';
  RsEGetMode = 'Invalid option to GetMode';
  RsENoTableName = 'TableName not specified';
  RsETableNameRequired = 'LoadFromFile=True, so a TableName is required';
  RsEInternalCompare = 'InternalCompare. Nil value detected';

//=== JvCsvParse.pas =========================================================
resourcestring
  RsEInvalidHexLiteral = 'HexStrToInt: Invalid hex literal';

//=== JvCursor.pas ===========================================================
resourcestring
  RsCurExtension = 'cur';
  RsCurDescription = 'Cursor files';

  RsECursorLoadFromClipboardFormat = 'LoadFromClipboardFormat not supported';
  RsECursorLoadFromStream = 'LoadFromStream not supported';
  RsECursorSaveToClipboardFormat = 'SaveToClipboardFormat not supported';
  RsECursorSaveToStream = 'SaveToStream not supported';

//=== JvDataProvider.pas =====================================================
resourcestring
  RsEItemsMayNotBeMovedInTheMainTree = 'Items may not be moved in the main tree';
  RsEInvalidIndex = 'Invalid index';
  RsEItemCanNotBeDeleted = 'Item cannot be deleted';
  RsEContextNameExpected = 'Context name expected';
  RsEConsumerStackIsEmpty = 'Consumer stack is empty';
  RsEContextStackIsEmpty = 'Context stack is empty';
  RsEAContextWithThatNameAlreadyExists = 'A context with that name already exists';
  RsECannotCreateAContextWithoutAContext = 'Cannot create a context without a context list owner';
  RsEComponentDoesNotSupportTheIJvDataPr = 'Component does not support the IJvDataProvider interface';
  RsEComponentDoesNotSupportTheIInterfac = 'Component does not support the IInterfaceComponentReference interface';
  RsEYouMustSpecifyAProviderBeforeSettin = 'You must specify a provider before setting the context';
  RsEProviderHasNoContextNameds = 'Provider has no context named "%s"';
  RsEProviderDoesNotSupportContexts = 'Provider does not support contexts';
  RsETheSpecifiedContextIsNotPartOfTheSa = 'The specified context is not part of the same provider';
  RsEYouMustSpecifyAProviderBeforeSettin_ = 'You must specify a provider before setting the item';
  RsEItemNotFoundInTheSelectedContext = 'Item not found in the selected context';
  RsEViewListOutOfSync = 'ViewList out of sync';

  RsEProviderIsNoIJvDataConsumer = 'Provider property of ''%s'' does not point to a IJvDataConsumer';
  RsEComponentIsNotDataConsumer = 'Component ''%s'' is not a data consumer';
  RsECannotAddNil = 'Cannot add a nil pointer';
  RsEConsumerNoSupportIJvDataConsumerClientNotify =
    'Consumer does not support the ''IJvDataConsumerClientNotify'' interface';
  RsENotifierNoSupprtIJvDataConsumer = 'Notifier does not support the ''IJvDataConsumer'' interface';

  RsEExtensibleIntObjDuplicateClass = 'Implementation of that class already exists';
  RsEExtensibleIntObjCollectionExpected = 'Expected collection';
  RsEExtensibleIntObjClassNameExpected = 'Missing ClassName property';
  RsEExtensibleIntObjInvalidClass = 'Invalid class type';
  RsEDataProviderNeedsItemsImpl = 'Cannot create a data provider without an IJvDataItems implementation';

//=== JvDatePickerEdit.pas ===================================================
resourcestring
  RsDefaultNoDateShortcut = 'Alt+Del';

  RsEMustHaveADate = '%s must have a date!';

//=== JvDateTimePicker.pas ===================================================
resourcestring
  RsNoneCaption = '(none)';

//=== JvDBControls.pas =======================================================
resourcestring
  RsInactiveData = 'Closed';
  RsBrowseData = 'Browse';
  RsEditData = 'Edit';
  RsInsertData = 'Insert';
  RsSetKeyData = 'Search';
  RsCalcFieldsData = 'Calculate';

//=== JvDBGrid.pas ===========================================================
resourcestring
  RsJvDBGridSelectTitle = 'Select columns';
  RsJvDBGridSelectOption = '[With the real field name]';
  RsJvDBGridSelectOK = '&OK';
  RsJvDBGridSelectWarning = 'At least one column must be visible!';
  RsEJvDBGridControlPropertyNotAssigned = 'JvDBGrid.EditControls: property Control not assigned';

//=== JvDBUltimGrid.pas ======================================================
resourcestring
  RsEJvDBGridBadFieldKind = 'Cannot sort a binary or special field';
  RsEJvDBGridIndexPropertyMissing = 'Cannot sort. An index property is missing';
  RsEJvDBGridIndexMissing  = 'Cannot sort. The corresponding index is missing';
  RsEJvDBGridUserSortNotAssigned = 'Cannot sort. OnUserSort is not assigned';

//=== JvDBGridExport.pas =====================================================
resourcestring
  RsHTMLExportDocTitle = 'Grid to HTML Export';
  RsExportWord = 'Exporting to MS Word...';
  RsExportExcel = 'Exporting to MS Excel...';
  RsExportHTML = 'Exporting to HTML...';
  RsExportFile = 'Exporting to CSV/Text...';
  RsExportClipboard = 'Exporting to Clipboard...';
  RsEDataSetDataSourceIsUnassigned = 'Dataset or DataSource unassigned';
  RsEGridIsUnassigned = 'No grid assigned';

//=== JvDBImage.pas ==========================================================
resourcestring
  RsEBadGraphicSignature = 'Bad graphic signature';

//=== JvDBLookup.pas =========================================================
resourcestring
  RsEInvalidFormatNotAllowed = 'Invalid format: % not allowed';
  RsEInvalidFormatsNotAllowed = 'Invalid format: %s not allowed';

//=== JvDBQueryParamsForm.pas ================================================
resourcestring
  // (p3) copied from bdeconst so we don't have to include the entire BDE for three strings...
  RsDataTypes =
    ';String;SmallInt;Integer;Word;Boolean;Float;Currency;BCD;Date;Time;DateTime;;;;Blob;Memo;Graphic;;;;;Cursor;';
  RsParamEditor = '%s%s%s Parameters';

  RsEInvalidParamFieldType = 'Must have a valid field type selected';

//=== JvDBRemoteLogin.pas ====================================================
resourcestring
  RsKeyLoginSection = 'Remote Login';
  RsKeyLastLoginUserName = 'Last User';

//=== JvDBTreeView.pas =======================================================
resourcestring
  RsDeleteNode = 'Delete %s ?';
  RsDeleteNode2 = 'Delete %s (with all children) ?';
  RsMasterFieldError = '"MasterField" must be integer type';
  RsDetailFieldError = '"DetailField" must be integer type';
  RsItemFieldError = '"ItemField" must be string, date or integer type';
  RsIconFieldError = '"IconField" must be integer type';
  RsMasterFieldEmpty = '"MasterField" property must be filled';
  RsDetailFieldEmpty = '"DetailField" property must be filled';
  RsItemFieldEmpty = '"ItemField" property must be filled';

  RsEMoveToModeError = 'Invalid move mode for JvDBTreeNode';
  RsMasterDetailFieldError = '"MasterField" and "DetailField" must be of same type';
  RsEDataSetNotActive = 'DataSet not active';
  RsEErrorValueForDetailValue = 'error value for DetailValue';

//=== JvDBUtils.pas ==========================================================
resourcestring
  RsConfirmSave = 'The data has changed. Save it?';

//=== JvDdeCmd.pas ===========================================================
resourcestring
  RsEErrorCommandStart = 'Invalid command start format';
  RsEErrorCommandFormat = 'Invalid command format: %s';

//=== JvDrawImage.pas ========================================================
resourcestring
  RsImageMustBeSquare = 'image must be square for Spirographs';
  RsSumOfRadiTolarge = 'sum of radi too large';
  RsBothRadiMustBeGr = 'both radi must be >%d';

//=== JvDropDownForm.pas =====================================================
resourcestring
  RsETJvCustomDropDownFormCreateOwnerMus = 'TJvCustomDropDownForm.Create: Owner must be a TCustomEdit';

//=== JvDSADialogs.pas =======================================================
resourcestring
  RsInTheCurrentQueue = 'in the current queue';

  RsDSActkShowText = 'Do not show this dialog again';
  RsDSActkAskText = 'Do not ask me again';
  RsDSActkWarnText = 'Do not warn me again';

  RsCntdownText = 'This dialog is closing in %d %s.';
  RsCntdownSecText = 'second';
  RsCntdownSecsText = 'seconds';

  RsECannotEndCustomReadIfNotInCustomRea = 'Cannot end custom read if not in custom read mode';
  RsECannotEndCustomWriteIfNotInCustomWr = 'Cannot end custom write if not in custom write mode';
  RsECannotEndReadIfNotInReadMode = 'Cannot end read if not in read mode';
  RsECannotEndWriteIfNotInWriteMode = 'Cannot end write if not in write mode';
  RsEJvDSADialogPatchErrorJvDSADialogCom = 'JvDSADialog patch error: JvDSADialog component not found';

  RsEDSARegKeyCreateError = 'Unable to create key %s';
  RsEDSADuplicateID = 'DSA dialog with ID ''%d'' is already assigned to another dialog name';
  RsEDSADuplicateName = 'DSA dialog named ''%s'' is already assigned to another dialog ID';
  RsEDSADialogIDNotFound = 'DSA dialog %d does not exist';
  RsEDSADuplicateCTK_ID = 'CheckMarkText ID %d already registered';
  RsEDSADialogIDNotStored = 'DSA dialog %d has not been stored';
  RsEDSAKeyNotFound = 'Key %s does not exist';
  RsEDSAKeyNoAccessAs = 'Key %s cannot be accessed as %s';

  RsECtrlHasNoCheckedProp = 'The specified control has no "Checked" property';
  RsECtrlHasNoCaptionProp = 'The specified control has no "Caption" property';
  RsEDialogIDChangeOnlyInDesign = 'The dialog ID can only be changed at design time';
  RsEOnlyAllowedOnForms = 'TJvDSADialog is only allowed on forms';
  RsEAlreadyDSADialog = 'The form already has a TJvDSADialog component';

  RsEDSAAccessBool = 'Boolean';
  RsEDSAAccessFloat = 'Float';
  RsEDSAAccessInt64 = 'Int64';
  RsEDSAAccessInt = 'Integer';
  RsEDSAAccessString = 'string';

//=== JvDualList.pas =========================================================
resourcestring
  RsDualListSrcCaption = '&Source';
  RsDualListDestCaption = '&Destination';

//=== JvDynControlEngine.pas =================================================
resourcestring
  RsEIntfCastError = 'SIntfCastError';
  RsEUnsupportedControlClass = 'TJvDynControlEngine.RegisterControl: Unsupported ControlClass';
  RsENoRegisteredControlClass = 'TJvDynControlEngine.CreateControl: No Registered ControlClass';

//=== JvEDIDBBuffering.pas ===================================================
resourcestring
  RsENoProfileDatasets = 'Not all profile datasets have been assigned.';

//=== JvEditor.pas, JvUnicodeEditor.pas ======================================
resourcestring
  RsERedoNotYetImplemented = 'Redo not yet implemented';
  RsEInvalidCompletionMode = 'Invalid JvEditor Completion Mode';

//=== JvEmbeddedForms.pas ====================================================
resourcestring
  RsEFormLinkSingleInstanceOnly = 'You only need one form link per form.';
  RsELinkCircularRef = 'Circular references not allowed.';

//=== JvErrorIndicator.pas ===================================================
resourcestring
  RsEControlNotFoundInGetError = 'Control not found in GetError';
  RsEControlNotFoundInGetImageAlignment = 'Control not found in GetImageAlignment';
  RsEControlNotFoundInGetImagePadding = 'Control not found in GetImagePadding';
  RsEUnableToAddControlInSetError = 'Unable to add control in SetError';
  RsEUnableToAddControlInSetImageAlignme = 'Unable to add control in SetImageAlignment';
  RsEUnableToAddControlInSetImagePadding = 'Unable to add control in SetImagePadding';

//=== JvExceptionForm.pas ====================================================
resourcestring
  RsCodeError = '%s.' + sLineBreak + 'Error Code: %.8x (%1:d).';
  RsModuleError = 'Exception in module %s.' + sLineBreak + '%s';

//=== JvFindReplace.pas ======================================================
resourcestring
  RsNotFound = 'Search string ''%s'' not found';
  RsXOccurencesReplaced = '%d occurence(s) of ''%s'' were replaced';
  RsReplaceCaption = 'Replace';
  RsFindCaption = 'Find';

  RsENoEditAssigned = 'No edit control assigned!';

//=== JvFooter.pas ===========================================================
resourcestring
  RsETJvFooterBtnCanOnlyBePlacedOnATJvFo = 'TJvFooterBtn can only be placed on a TJvFooter';

//=== JvForth.pas ============================================================
resourcestring
  RsEInvalidNumbers = 'invalid number %s';
  RsEUnrecognizedDataTypeInSetOperation = 'unrecognized data type in set operation';
  RsEUnterminatedBlockNear = 'unterminated block near ';
  RsEParserTimedOutAfterdSecondsYouMayHa = 'parser timed out after %d seconds; you may have circular includes';
  RsEUnterminatedIncludeNears = 'unterminated include near %s';
  RsEIllegalSpaceCharacterInTheIncludeFi = 'illegal space character in the include file: %s';
  RsECanNotFindIncludeFiles = 'Can not find include file: %s';
  RsEOnIncludeHandlerNotAssignedCanNotHa = 'OnInclude handler not assigned, can not handle include file: %s';
  RsEMissingCommentTerminatorNears = 'missing "}" comment terminator near %s';
  RsEMissingXmlMethodSpecifierNears = 'missing XML method specifier near %s';
  RsEMissingDataSourceMethodSpecifierNea = 'missing data source method specifier near %s';
  RsEMissingSystemMethodSpecifierNears = 'missing system method specifier near %s';
  RsEMissingExternalVariableMethodSpecif = 'missing external variable method specifier near %s';
  RsEMissingInternalVariableMethodSpecif = 'missing internal variable method specifier near %s';
  RsEUndefinedWordsNears = 'undefined word "%s" near %s';
  RsEScriptTimedOutAfterdSeconds = 'Script timed out after %d seconds';
  RsECanNotAssignVariables = 'can not assign variable %s';
  RsEVariablesNotDefined = 'Variable %s not defined';
  RsEProceduresNotDefined = 'procedure %s not defined';
  RsEVariablesNotDefined_ = 'variable %s not defined';
  RsESystemsNotDefined = 'System %s not defined';
  RsECanNotAssignSystems = 'can not assign System %s';
  RsEUnrecognizeExternalVariableMethodss = 'unrecognize external variable method %s.%s';
  RsEUnrecognizeInternalVariableMethodss = 'unrecognize internal variable method %s.%s';
  RsEUnrecognizeSystemMethodss = 'unrecognize system method %s.%s';
  RsEFilesDoesNotExist = 'File %s does not exist';
  RsECanNotSaveToFiles = 'Can not save to file %s';
  RsEXMLSelectionIsEmpty = 'XML selection is empty';
  RsENoXMLSelectionSelected = 'no XML selection selected';
  RsEXMLSelectionOutOfRange = 'XML selection out of range';
  RsEInvalidXmlMethodSpecifiers = 'invalid XML method specifier %s';
  RsEIncrementIndexExpectedIns = 'Increment Index: "[" expected in %s';
  RsEIncrementIndexExpectedIns_ = 'Increment Index: "]" expected in %s';
  RsEIncrementIndexExpectedIntegerBetwee = 'Increment Index: expected integer between "[..]" in %s';
  RsEDSOIndexOutOfRanged = 'DSO index out of range %d';
  RsEDSOUnknownKeys = 'DSO unknown key %s';

//=== Jvg3DColors.pas ========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEOnlyOneInstanceOfTJvg3DLocalColors = 'Cannot create more than one instance of TJvg3DLocalColors component';
{$ENDIF USEJVCL}

//=== JvGammaPanel.pas =======================================================
resourcestring
  RsRedFormat = 'R : %3D';
  RsGreenFormat = 'G : %3D';
  RsBlueFormat = 'B : %3D';

  RsHint1 = 'Background Color';
  RsHint2 = 'Foreground Color';
  RsLabelCaption = 'X';
  RsLabelHint = 'Exchange colors';

  RsDefaultB = 'B : ---';
  RsDefaultG = 'G : ---';
  RsDefaultR = 'R : ---';

//=== JvgAskListBox.pas ======================================================
{$IFDEF USEJVCL}
resourcestring
  RsYes = 'yes';
  RsNo = 'no';
{$ENDIF USEJVCL}

//=== JvgButton.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEErrorDuringAccessGlyphsListOrGlyphP = 'Error during access GlyphsList or Glyph property';
{$ENDIF USEJVCL}

//=== JvgCaption.pas =========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEOnlyOneInstanceOfTJvgCaption = 'Cannot create more than one instance of TJvgCaption component';
{$ENDIF USEJVCL}

//=== JvgCheckVersionInfoForm.pas ============================================
{$IFDEF USEJVCL}
resourcestring
  RsNoNewerVersionOfProgramAvailable = 'No newer version of program available';
{$ENDIF USEJVCL}

//=== JvGenetic.pas ==========================================================
resourcestring
  RsENoTest = 'TJvGenetic: OnTestMember must be assigned';

//=== JvgExportComponents.pas ================================================
{$IFDEF USEJVCL}
resourcestring
  RsEDataSetIsUnassigned = 'DataSet is unassigned';
  RsESaveToFileNamePropertyIsEmpty = 'SaveToFileName property is empty';
{$ENDIF USEJVCL}

//=== JvgHelpPanel.pas =======================================================
{$IFDEF USEJVCL}
resourcestring
  RsHelp = ' help ';
  RsOpenContextMenuToLoadRTFTextControl = 'Open context menu to load RTF text. Control shows text at runtime only.';
{$ENDIF USEJVCL}

//=== JvgHint.pas ============================================================
{$IFDEF USEJVCL}
resourcestring
  RsEOnlyOneInstanceOfTJvgHint = 'Cannot create more than one instance of TJvgHint component';
{$ENDIF USEJVCL}

//=== JvgHTTPVersionInfo.pas =================================================
{$IFDEF USEJVCL}
resourcestring
  RsEUnknownURLPropertyVersionDataURLIs = 'Unknown URL: property VersionDataURL is empty';
{$ENDIF USEJVCL}

//=== JvGIF.pas ==============================================================
resourcestring
  RsGIFImage = 'CompuServe GIF Image';

  RsEChangeGIFSize = 'Cannot change the Size of a GIF image';
  RsENoGIFData = 'No GIF Data to write';
  RsEUnrecognizedGIFExt = 'Unrecognized extension block: %.2x';
  RsEWrongGIFColors = 'Wrong number of colors; must be a power of 2';
  RsEBadGIFCodeSize = 'GIF code size not in range 2 to 9';
  RsEGIFDecodeError = 'GIF encoded data is corrupt';
  RsEGIFEncodeError = 'GIF image encoding error';
  RsEGIFVersion = 'Unknown GIF version';

//=== JvgLogics.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEqualTo = 'equal to';
  RsStartingWith = 'starting with';
  RsEndsWith = 'ends with';
  RsContains = 'contains';
  RsIsContainedWithin = 'is contained within';
  RsNotEmpty = 'not empty';
  RsStep = 'Step ';
  RsComments = 'Comments';
{$ENDIF USEJVCL}

//=== JvgMailSlots.pas =======================================================
{$IFDEF USEJVCL}
resourcestring
  RsETJvgMailSlotServerErrorCreatingChan = 'TJvgMailSlotServer: Error creating channel!';
  RsETJvgMailSlotServerErrorGatheringInf = 'TJvgMailSlotServer: Error gathering information!';
  RsETJvgMailSlotServerErrorReadingMessa = 'TJvgMailSlotServer: Error reading message!';
{$ENDIF USEJVCL}

//=== JvgProgress.pas ========================================================
{$IFDEF USEJVCL}
resourcestring
  RsProgressCaption = 'Progress...[%d%%]';
{$ENDIF USEJVCL}

//=== JvgQPrintPreviewForm.pas ===============================================
{$IFDEF USEJVCL}
resourcestring
  RsPageOfPages = 'Page %d of %d';
{$ENDIF USEJVCL}

//=== JvGradientHeaderPanel.pas ==============================================
resourcestring
  RsYourTextHereCaption = 'Put your text here ...';

//=== JvgReport.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsOLELinkedObjectNotFound = 'OLE: Linked object not found.';
  RsErrorText = 'Error';
  RsErrorReadingComponent = 'Error reading component';
{$ENDIF USEJVCL}

//=== JvGridPreviewForm.pas ==================================================
resourcestring
  RsOfd = 'of %d';
  RsPaged = 'Page %d';
  RsNoPrinterIsInstalled = 'No Printer is installed';

//=== JvGridPrinter.pas ======================================================
resourcestring
  RsPrintOptionsPageFooter = 'date|time|page';
  RsPrintOptionsDateFormat = 'd-mmm-yyyy';
  RsPrintOptionsTimeFormat = 'h:nn am/pm';

//=== JvgSingleInstance.pas ==================================================
{$IFDEF USEJVCL}
resourcestring
  RsOneInstanceOfThisProgramIsAlreadyRu =
    'One instance of this program is already running. A second instance launch is not allowed.';
  RsSecondInstanceLaunchOfs = 'Second instance launch of %s';
{$ENDIF USEJVCL}

//=== JvgSmallFontsDefense.pas ===============================================
{$IFDEF USEJVCL}
resourcestring
  RsTJvgSmallFontsDefenseCannotBeUsedWi = 'TJvgSmallFontsDefense cannot be used with large fonts.';
{$ENDIF USEJVCL}

//=== JvgSysRequirements.pas =================================================
{$IFDEF USEJVCL}
resourcestring
  { RUSSIAN
  RsVideoVRefreshRate = 'Частота обновления экрана должна быть %d герц или выше. Измените частоту обновления в свойствах экрана.';
  RsGraphicResolution = 'Разрешение экрана должно быть %s точек или выше. Измените разрешение в свойствах экрана.';
  RsColorDepth = 'Количество цветов экрана должно быть %s цветов или выше. Измените число цветов в свойствах экрана.';
  RsSystemFont = 'В системе должен быть установлен %s шрифт. Измените вид шрифта в свойствах экрана.';
  RsOSPlatform = 'Для работы программы необходима операционная система %s.';
  }
  RsVideoVRefreshRate =
    'The monitor refresh rate should be %d Hertz or higher. Change monitor refresh rate in Monitor Control Panel.';
  RsGraphicResolution =
    'The screen resolution should be equal %s pixels or higher. Change screen resolution in Monitor Control Panel.';
  RsColorDepth =
    'The number of colors of the screen should be equal to %s colors or higher. Change screen colors in Monitor Control Panel.';
  RsSystemFont = 'In system small fonts should be established. Change to small fonts in Monitor Control Panel.';
  RsOSPlatform = 'The program requires %s or better.';
{$ENDIF USEJVCL}

//=== JvgUtils.pas ===========================================================
{$IFDEF USEJVCL}
resourcestring
  RsERightBracketsNotFound = 'Right brackets not found';
  RsERightBracketHavntALeftOnePosd = 'Right bracket does not have a left one. Pos: %d';
  RsEDivideBy = 'Divide by 0';
  RsEDuplicateSignsAtPos = 'Duplicate signs at Pos: %d';
  RsEExpressionStringIsEmpty = 'Expression string is empty';
  {$IFDEF glDEBUG}
  RsEObjectMemoryLeak = 'object memory leak';
  {$ENDIF glDEBUG}
{$ENDIF USEJVCL}

//=== JvgXMLSerializer.pas ===================================================
{$IFDEF USEJVCL}
resourcestring
  { RUSSIAN
  RsOpenXMLTagNotFound = 'Открывающий тег не найден: <%s>';
  RsCloseXMLTagNotFound = 'Закрывающий тег не найден: </%s>';
  RsUncknownProperty = 'Uncknown property: %s'
  }
  RsOpenXMLTagNotFound = 'Open tag not found: <%s>';
  RsCloseXMLTagNotFound = 'Close tag not found: </%s>';
  RsUnknownProperty = 'Unknown property: %s';
{$ENDIF USEJVCL}

//=== JvHidControllerClass.pas ===============================================
{$IFDEF USEJVCL}
resourcestring
  RsUnknownLocaleIDFmt = 'Unknown Locale ID $%.4x';
  RsHIDP_STATUS_NULL = 'Device not plugged in';
  RsHIDP_STATUS_INVALID_PREPARSED_DATA = 'Invalid preparsed data';
  RsHIDP_STATUS_INVALID_REPORT_TYPE = 'Invalid report type';
  RsHIDP_STATUS_INVALID_REPORT_LENGTH = 'Invalid report length';
  RsHIDP_STATUS_USAGE_NOT_FOUND = 'Usage not found';
  RsHIDP_STATUS_VALUE_OUT_OF_RANGE = 'Value out of range';
  RsHIDP_STATUS_BAD_LOG_PHY_VALUES = 'Bad logical or physical values';
  RsHIDP_STATUS_BUFFER_TOO_SMALL = 'Buffer too small';
  RsHIDP_STATUS_INTERNAL_ERROR = 'Internal error';
  RsHIDP_STATUS_I8042_TRANS_UNKNOWN = '8042 key translation impossible';
  RsHIDP_STATUS_INCOMPATIBLE_REPORT_ID = 'Incompatible report ID';
  RsHIDP_STATUS_NOT_VALUE_ARRAY = 'Not a value array';
  RsHIDP_STATUS_IS_VALUE_ARRAY = 'Is a value array';
  RsHIDP_STATUS_DATA_INDEX_NOT_FOUND = 'Data index not found';
  RsHIDP_STATUS_DATA_INDEX_OUT_OF_RANGE = 'Data index out of range';
  RsHIDP_STATUS_BUTTON_NOT_PRESSED = 'Button not pressed';
  RsHIDP_STATUS_REPORT_DOES_NOT_EXIST = 'Report does not exist';
  RsHIDP_STATUS_NOT_IMPLEMENTED = 'Not implemented';
  RsUnknownHIDFmt = 'Unknown HID error %x';
  RsHIDErrorPrefix = 'HID Error: ';

  RsEDirectThreadCreationNotAllowed = 'Direct creation of a TJvDeviceReadThread object is not allowed';
  RsEDirectHidDeviceCreationNotAllowed = 'Direct creation of a TJvHidDevice object is not allowed';
  RsEDeviceCannotBeIdentified = 'Device cannot be identified';
  RsEDeviceCannotBeOpened = 'Device cannot be opened';
  RsEOnlyOneControllerPerProgram = 'Only one TJvHidDeviceController allowed per program';
  RsEHIDBooleanError = 'HID Error: a boolean function failed';
{$ENDIF USEJVCL}

//=== JvHint.pas =============================================================
resourcestring
  RsHintCaption = 'Hint';

//=== JvHLEditorPropertyForm.pas =============================================
resourcestring
  RsHLEdPropDlg_Caption = 'Editor Properties';
  RsHLEdPropDlg_tsEditor = 'Editor';
  RsHLEdPropDlg_tsColors = 'Colors';
  RsHLEdPropDlg_lblEditorSpeedSettings = 'Editor SpeedSettings';
  RsHLEdPropDlg_cbKeyboardLayoutDefault = 'Default keymapping';
  RsHLEdPropDlg_gbEditor = 'Editor options:';
  RsHLEdPropDlg_cbAutoIndent = '&Auto indent mode';
  RsHLEdPropDlg_cbSmartTab = 'S&mart tab';
  RsHLEdPropDlg_cbBackspaceUnindents = 'Backspace &unindents';
  RsHLEdPropDlg_cbGroupUndo = '&Group undo';
  RsHLEdPropDlg_cbCursorBeyondEOF = 'Cursor beyond &EOF';
  RsHLEdPropDlg_cbUndoAfterSave = '&Undo after sa&ve';
  RsHLEdPropDlg_cbKeepTrailingBlanks = '&Keep trailing blanks';
  RsHLEdPropDlg_cbDoubleClickLine = '&Double click line';
  RsHLEdPropDlg_cbSytaxHighlighting = 'Use &syntax highlight';
  RsHLEdPropDlg_lblTabStops = '&Tab stops:';
  RsHLEdPropDlg_lblColorSpeedSettingsFor = 'Color SpeedSettings for';
  RsHLEdPropDlg_lblElement = '&Element:';
  RsHLEdPropDlg_lblColor = '&Color:';
  RsHLEdPropDlg_gbTextAttributes = 'Text attributes:';
  RsHLEdPropDlg_gbUseDefaultsFor = 'Use defaults for:';
  RsHLEdPropDlg_cbBold = '&Bold';
  RsHLEdPropDlg_cbItalic = '&Italic';
  RsHLEdPropDlg_cbUnderline = '&Underline';
  RsHLEdPropDlg_cbDefForeground = '&Foreground';
  RsHLEdPropDlg_cbDefBackground = '&Background';
  RsHLEdPropDlg_OptionCantBeChanged = 'This option cannot be changed. Sorry.';

  RsEHLEdPropDlg_RAHLEditorNotAssigned = 'JvHLEditor property is not assigned';
  RsEHLEdPropDlg_RegAutoNotAssigned = 'RegAuto property is not assigned';
  RsEHLEdPropDlg_GridCellNotFound = 'Grid cell not found';

//=== JvHTTPGrabber.pas ======================================================
resourcestring
  RsAgent = 'TJvHTTPGrabber Delphi Component';

//=== JvId3v1.pas ============================================================
resourcestring
  RsENotActive = 'Not active';

//=== JvID3v2Base.pas ========================================================
resourcestring
  RsENameMsgFormat = '%s: %s';
  RsEAllowedEncodingsIsEmpty = 'FAllowedEncodings is empty';
  RsEAlreadyReadingWriting = 'Already reading or writing';
  RsEAlreadyReadingWritingFrame = 'Already reading/writing frame';
  RsEAlreadyUsingTempStream = 'Already using temp stream';
  RsECannotCallCanRead = 'Cannot call CanRead while writing';
  RsEControllerDoesNotSupportCompression = 'Controller does not support compression';
  RsEControllerDoesNotSupportCRC = 'Controller does not support CRC';
  RsEControllerDoesNotSupportEncryption = 'Controller does not support encryption';
  RsEControllerDoesNotSupportFooter = 'Controller does not support footer';
  RsECouldNotFindAllowableEncoding = 'Could not find allowable encoding';
  RsECouldNotReadData = 'Could not read data from stream';
  RsEErrorInFrame = 'Error in frame %s (%s), %s';
  RsEFrameSizeDiffers = 'Frame size differs from actually amount of data written';
  RsEFrameSizeTooBig = 'Frame size is too big';
  RsELanguageNotOfLength3 = 'Language is not of length 3';
  RsENoTempStream = 'No temp stream';
  RsENotReadingFrame = 'Not reading frame';
  RsENotUsingTempStream = 'Not using temp stream';
  RsENotWriting = 'Not writing';
  RsENotWritingFrame = 'Not writing frame';
  RsETagTooBig = 'Tag is too big';
  RsEValueTooBig = 'Cannot write value in v2.2; too big';
  RsENotReading = 'Not reading';

  RsEID3FrameNotFound = 'Frame not found';
  RsEID3UnknownEncoding = 'Unknown encoding';
  RsEID3UnknownVersion = 'Unknown version';
  RsEID3DuplicateFrame = 'Frame is a duplicate of another frame in the tag';
  RsEID3AlreadyContainsFrame = 'Tag already contains a ''%s'' frame';
  RsEID3ControllerNotActive = 'Controller is not active';
  RsEID3EncodingNotSupported = 'Encoding not supported in this version';
  RsEID3VersionNotSupported = 'Version not supported';
  RsEID3InvalidLanguageValue = '''%s'' is an invalid language value';
  RsEID3InvalidPartInSetValue = '''%s'' is an invalid ''part in set'' value';
  RsEID3InvalidTimeValue = '''%s'' is an invalid time value.'#13'Value must be of format ''HHMM''';
  RsEID3InvalidDateValue = '''%s'' is an invalid date value.'#13'Value must be of format ''DDMM''';
  RsEID3ValueTooBig = '''%d'' is an invalid value. Value is too big';
  RsEID3StringTooLong = '''%s'' is an invalid value. String is too long';
  RsEID3InvalidCharinList = 'Invalid char ''%s'' in string ''%s'' in list';
  RsEID3InvalidFrameClass = 'Frame class ''%s'' cannot be used to represent frame ID ''%s''';
  RsEID3FrameIDNotSupported = 'Frame ID ''%s'' not supported by this frame';
  RsEID3FrameIDStrNotSupported = 'Frame ID string ''%s'' not supported by this frame';

//=== JvId3v2Types.pas =======================================================
resourcestring
  RsEFrameIDSizeCanOnlyBe34 = 'Frame ID size can only be 3 or 4';

//=== JvImageDlg.pas =========================================================
resourcestring
  RsImageTitle = 'Image Viewer';

//=== JvImageList.pas ========================================================
resourcestring
  RsResource = 'Resource %s';
  RsMappedResource = 'Mapped Resource %s';
  RsBitmap = 'Bitmap %s';
  RsEWrongImageListMode = 'Wrong image list mode. For this function the mode must be %s';

//=== JvImageWindow.pas ======================================================
resourcestring
  RsEImagesNotAssigned = 'Images not Assigned!';

//=== JvInspector.pas ========================================================
resourcestring
  RsJvInspItemValueException = 'Exception ';
  RsJvInspItemUnInitialized = '(uninitialized)';
  RsJvInspItemUnassigned = '(unassigned)';
  RsJvInspItemNoValue = '(no value)';

  RsStringListEditorCaption = 'String list editor';
  RsXLinesCaption = ' lines';
  RsOneLineCaption = '1 line';

  RsEJvInspItemHasParent = 'Item already assigned to another parent';
  RsEJvInspItemNotAChild = 'Specified Item is not a child of this item';
  RsEJvInspItemColNotFound = 'Specified column does not belong to this compound item';
  RsEJvInspItemItemIsNotCol = 'Specified item is not a column of this compound item';
  RsEJvInspItemInvalidPropValue = 'Invalid property value %s';
  RsEJvInspDataNoAccessAs = 'Data cannot be accessed as %s';
  RsEJvInspDataNotInit = 'Data not initialized';
  RsEJvInspDataNotAssigned = 'Data not assigned';
  RsEJvInspDataNoValue = 'Data has no value';
  RsEJvInspDataStrTooLong = 'String too long';
  RsEJvInspRegNoCompare = 'Cannot compare %s to %s';
  RsEJvInspNoGenReg = 'Unable to create generic item registration list';
  RsEJvInspPaintNotActive = 'Painter is not the active painter of the specified inspector';
  RsEJvInspPaintOnlyUsedOnce = 'Inspector painter can only be linked to one inspector';

  RsEInspectorInternalError = 'Internal error: two data instances pointing to the same data are registered';
  RsESpecifierBeforeSeparator = 'A specifier should be placed before and after a separator';
  RsEDOrDDOnlyOnce = '''d'' or ''dd'' should appear only once';
  RsEMOrMMOnlyOnce = '''m'' or ''mm'' should appear only once';
  RsEYYOrYYYYOnlyOnce = '''yy'' or ''yyyy'' should appear only once';
  RsEOnlyDOrDDAllowed = 'Only ''d'' or ''dd'' are allowed';
  RsEOnlyMOrMMAllowed = 'Only ''m'' or ''mm'' are allowed';
  RsEOnlyYYOrYYYYAllowed = 'Only ''yy'' or ''yyyy'' are allowed';
  RsEOnlyTwoSeparators = 'Only two separators are allowed';
  RsEOnlyDMYSAllowed = 'Only ''d'', ''m'', ''y'' and ''%s'' are allowed';
  RsEDOrDDRequired = '''d'' or ''dd'' are required';
  RsEMOrMMRequired = '''m'' or ''mm'' are required';
  RsEYYOrYYYYRequired = '''yy'' or ''yyyy'' are required';
  RsEInstanceAlreadyExists = 'Instance already exists with another name';
  RsENameAlreadyExistsForInstance = 'Name already exists for another instance';
  RsEInstanceNonexistent = 'Instance does not exist';
  RsEMethodAlreadyExists = 'Method already exists with another name';
  RsENameAlreadyExistsForMethod = 'Name already exists for another method';
  RsENamedInstanceNonexistent = 'Instance named ''%s'' does not exist';
  RsEMethodNonexistent = 'Method does not exist';
  RsENamedMethodNonexistent = 'Method named ''%s'' does not exist';
  RsENotSeparately = '%s cannot be created separately';
  RsENoNewInstance = '%s does not allow a new instance to be created';

  // (rom) converted assertions
  RsEJvAssertSetTopIndex = 'TJvCustomInspector.SetTopIndex: unexpected MaxIdx <= -1';
  RsEJvAssertInspectorPainter = 'TJvInspectorCustomCompoundItem.DivideRect: unexpected Inspector.Painter = nil';
  RsEJvAssertDataParent = 'TJvInspectorSetMemberData.New: unexpected ADataParent = nil';
  RsEJvAssertParent = 'TJvInspectorSetMemberData.New: unexpected AParent = nil';
  RsEJvAssertPropInfo = 'TJvInspectorPropData.New: unexpected PropInfo = nil';
  RsEJvAssertINIFile = 'TJvInspectorINIFileData.New: unexpected AINIFile = nil';

//=== JvInspXVCL.pas =========================================================
resourcestring
  RsENoNodeSpecified = 'TJvInspectorxNodeData.New: No node specified';

//=== JvInstallLabel.pas =====================================================
resourcestring
  RsEListOutOfBounds = 'List index out of bounds (%d)';

//=== JvInterpreter.pas ======================================================
resourcestring
  RsNotImplemented = 'Function not yet implemented';
  RsOleAutomationCall = 'Ole automation call';

  RsESorryDynamicArraysSupportIsMadeForO = 'Sorry. Dynamic arrays support is made for one-dimensional arrays only';
  RsEUnknownRecordType = 'Unknown RecordType';
  RsERangeCheckError = 'Range check error';

//=== JvInterpreter_Quickrpt.pas =============================================
resourcestring
  RsENoQuickReportFound = 'TQuickRep component not found on the form';

//=== JvInterpreter_System.pas ===============================================
resourcestring
  RsESizeMustBeEven = 'The size of bounds array must be even!';

//=== JvInterpreterConst.pas =================================================
resourcestring
  RsEInterpreter0 = 'Ok';
  RsEInterpreter1 = 'Unknown error';
  RsEInterpreter2 = 'Internal interpreter error: %s';
  RsEInterpreter3 = 'User break';
  RsEInterpreter4 = 'Re-raising an exception only allowed in exception handler';
  RsEInterpreter5 = 'Error in unit ''%s'' on line %d : %s';
  RsEInterpreter6 = 'External error in unit ''%s'' on line %d : %s';
  RsEInterpreter7 = 'Access denied to ''%s''';
  RsEInterpreter8 = 'Expression is too complex - overflow';
  RsEInterpreter31 = 'Record ''%s'' not defined';

  RsEInterpreter52 = 'Stack overflow';
  RsEInterpreter53 = 'Type mismatch';
  RsEInterpreter55 = 'Function ''main'' undefined';
  RsEInterpreter56 = 'Unit ''%s'' not found';
  RsEInterpreter57 = 'Event ''%s'' not registered';
  RsEInterpreter58 = 'DFM ''%s'' not found';

  RsEInterpreter101 = 'Error in remark'; // (rom) in comment?
  RsEInterpreter103 = '%s expected but %s found';
  RsEInterpreter104 = 'Undeclared Identifier ''%s''';
  RsEInterpreter105 = 'Type of expression must be boolean';
  RsEInterpreter106 = 'Class type required';
  RsEInterpreter107 = ' not allowed before else';
  RsEInterpreter108 = 'Type of expression must be integer';
  RsEInterpreter109 = 'Record, object or class type required';
  RsEInterpreter110 = 'Missing operator or semicolon';
  RsEInterpreter111 = 'Identifier redeclared: ''%s''';

  RsEInterpreter171 = 'Array index out of bounds';
  RsEInterpreter172 = 'Too many array bounds';
  RsEInterpreter173 = 'Not enough array bounds';
  RsEInterpreter174 = 'Invalid array dimension';
  RsEInterpreter175 = 'Invalid array range';
  RsEInterpreter176 = 'Array type required';

  RsEInterpreter181 = 'Too many actual parameters';
  RsEInterpreter182 = 'Not enough parameters';
  RsEInterpreter183 = 'Incompatible types: ''%s'' and ''%s''';
  RsEInterpreter184 = 'Error loading library ''%s''';
  RsEInterpreter185 = 'Invalid type of argument in call to function ''%s''';
  RsEInterpreter186 = 'Invalid type of result in call to function ''%s''';
  RsEInterpreter187 = 'Can''t get proc address for function ''%s''';
  RsEInterpreter188 = 'Invalid type of argument in call to function ''%s''';
  RsEInterpreter189 = 'Invalid type of result in call to function ''%s''';
  RsEInterpreter190 = 'Invalid calling convention for function ''%s''';

  RsEInterpreter201 = 'Calling ''%s'' failed: ''%s''';

  RsEInterpreter301 = 'Expression';
  RsEInterpreter302 = 'Identifier';
  RsEInterpreter303 = 'Declaration';
  RsEInterpreter304 = 'End of File';
  RsEInterpreter305 = 'Class Declaration';
  RsEInterpreter306 = 'Integer Constant''';
  RsEInterpreter307 = 'Integer Value';
  RsEInterpreter308 = 'String Constant';
  RsEInterpreter309 = 'Statement';

  RsEInterpreter401 = 'Implementation of unit not found';
  RsEInterpreter402 = 'Array and Record types are not allowed as procedure/function parameter';

  RsEXOrX = ''' or ''';

//=== JvInterpreterFm.pas ====================================================
resourcestring
  RsENoReportProc = 'Procedure "JvInterpreterRunReportPreview" not found';
  RsENoReportProc2 = 'Procedure "JvInterpreterRunReportPreview2" not found';

//=== JvJanTreeView.pas ======================================================
resourcestring
  RsSaveCurrentTree = 'Save Current Tree';
  RsSearch = 'Search';
  RsSearchFor = 'Search for:';
  RsNoMoresFound = 'No more %s found';

  RsEInvalidReduction = 'Invalid reduction';
  RsEBadTokenState = 'Bad token state';
  RsTreeViewFiles = 'TreeView Files';
  RsNewNode = 'new node';
  RsNew = 'new';
  RsRecalculateErr = 'Error in: %s';

//=== JvJoystick.pas =========================================================
resourcestring
  RsNoJoystickDriver = 'The joystick driver is not present.';
  RsCannotCaptureJoystick = 'Cannot capture the joystick';
  RsJoystickUnplugged = 'The specified joystick is not connected to the system.';
  RsJoystickErrorParam = 'The specified joystick device identifier is invalid.';

  RsEJoystickError = 'Unable to initialize joystick driver';

//=== JvJVCLUtils.pas ========================================================
resourcestring
  RsENotForMdi = 'MDI forms are not allowed';
  RsEPixelFormatNotImplemented = 'BitmapToMemoryStream: pixel format not implemented';
  RsEBitCountNotImplemented = 'BitmapToMemoryStream: bit count not implemented';
  RsECantGetShortCut = 'Target FileName for ShortCut %s not available';

//=== JvLinkedControls.pas ===================================================
resourcestring
  RsEOwnerLinkError = 'Cannot link to owner control';

//=== JvLinkLabel.pas ========================================================
resourcestring
  RsEUnableToLocateMode = 'Unable to locate specified node';
  RsETagNotFound = 'TJvCustomLinkLabel.UpdateDynamicTag: Tag not found';

//=== JvLinkLabelParser.pas ==================================================
resourcestring
  RsENoMoreElementsToReturn = 'TElementEnumerator.GetNextElement: No more elements to return';
  RsEUnsupportedState = 'TDefaultParser.ParseNode: Unsupported state';

//=== JvLinkLabelTextHandler.pas =============================================
resourcestring
  RsENoMoreWords = 'TWordEnumerator.GetNext: No more words to return';
  RsEUnsupported = 'TTextHandler.EmptyBuffer: Unsupported TParentTextElement descendant encountered';

//=== JvLinkLabelTools.pas ===================================================
resourcestring
  RsECannotBeInstantiated = 'This class cannot be instantiated';

//=== JvLinkLabelTree.pas ====================================================
resourcestring
  RsETNodeGetNodeTypeUnknownClass = 'TNode.GetNodeType: Unknown class';
  RsENoMoreNodesToReturn = 'No more nodes to return';
  RsENoMoreRecordsToReturn = 'No more records to return';
  RsEWordInfoIndexOutOfBounds = 'TStringNode.GetWordInfo: Index out of bounds';

//=== JvListView.pas =========================================================
resourcestring
  RsETooManyColumns = 'TJvListView.GetColumnsOrder: too many columns';

//=== JvLoginForm.pas ========================================================
resourcestring
  RsRegistrationCaption = 'Registration';
  RsAppTitleLabel = 'Application "%s"';
  RsHintLabel = 'Type your user name and password to enter the application';
  RsUserNameLabel = '&User name:';
  RsPasswordLabel = '&Password:';
  RsUnlockCaption = 'Unlock application';
  RsUnlockHint = 'Type your password to unlock the application';

//=== JvMail.pas =============================================================
resourcestring
  RsAttachmentNotFound = 'Attached file "%s" not found';
  RsRecipNotValid = 'Recipient %s has invalid address';
  RsNoClientInstalled = 'There is no MAPI-enabled client on the machine';
  RsNoUserLogged = 'There must be a user logged before call this function';

//=== JvMemoryDataset.pas ====================================================
resourcestring
  RsEMemNoRecords = 'No data found';
  //----------------- Added by CFZ ------------------
  // 'Registro ya existente.';
  RsERecordDuplicate = 'Record already exists.';
  // 'Registro no encontrado.';
  RsERecordInexistent = 'Record not found.';
  // 'No se pudo agregar el registro.';
  RsEInsertError = 'Unable to append the record.';
  // 'No se pudo modificar el registro.';
  RsEUpdateError = 'Unable to modify the record.';
  // 'No se pudo eliminar el registro.';
  RsEDeleteError = 'Unable to erase the record.';
  //-------------------------------------------------

//=== JvMouseGesture.pas =====================================================
resourcestring
  RsECannotHookTwice = 'JvMouseGesture Fatal: You cannot hook this event twice';

//=== JvMRUList.pas ==========================================================
resourcestring
  RsEErrorMruCreating = 'Unable to create MRU';
  RsEErrorMruUnicode = 'Windows NT required for Unicode in MRU';

//=== JvMRUManager.pas =======================================================
resourcestring
  RsEDuplicatesNotAllowedInMRUList = 'Duplicates not allowed in MRU list';

//=== JvMTComponents.pas =====================================================
{$IFDEF USEJVCL}
resourcestring
  RsENoThreadManager = 'No ThreadManager specified';
  RsEOperatorNotAvailable = 'Operation not available while thread is active';
  RsECannotChangePropertySection = 'Cannot change property of active section';
  RsECannotChangePropertyBuffer = 'Cannot change property of active buffer';
{$ENDIF USEJVCL}

//=== JvMTData.pas ===========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEMethodOnlyForMainThread = '%s method can only be used by the main VCL thread';
{$ENDIF USEJVCL}

//=== JvMTSync.pas ===========================================================
{$IFDEF USEJVCL}
resourcestring
  RsESemaphoreFailure = 'Semaphore failure (%d)';
  RsESemaphoreAbandoned = 'Semaphore was abandoned';
  RsEThreadAbandoned = 'Thread was abandoned';
{$ENDIF USEJVCL}

//=== JvMTThreading.pas ======================================================
{$IFDEF USEJVCL}
resourcestring
  RsECurThreadIsPartOfManager = 'Current MTThread is part of the MTManager';
  RsECheckTerminateCalledByWrongThread = 'CheckTerminate can only be called by the same thread';
  RsEThreadNotInitializedOrWaiting = 'Cannot run: thread is not Initializing or Waiting';
  RsECannotChangeNameOfOtherActiveThread = 'Cannot change name of other active thread';
  RsEReleaseOfUnusedTicket = 'Release of unused ticket';
{$ENDIF USEJVCL}

//=== JvMultiHttpGrabber.pas =================================================
resourcestring
  RsErrorConnection = 'Unable to connect';
  RsMultiAgent = 'TJvMultiHTTPGrabber Delphi Component';

//=== JvNavigationPane.pas ===================================================
resourcestring
  RsEUnsupportedButtonType = 'ButtonType not supported';

//=== JvNTEventLog.pas =======================================================
resourcestring
  RsLogError = 'Error';
  RsLogWarning = 'Warning';
  RsLogInformation = 'Information';
  RsLogSuccessAudit = 'Success Audit';
  RsLogFailureAudit = 'Failure Audit';

//=== JvObjectPickerDialog.pas ===============================================
resourcestring
  RsEAttributeIndexOutOfBounds = '%d is not a valid attribute index';
  RsESelectionIndexOutOfBounds = '%d is not a valid selection index';

//=== JvOfficeColorButton.pas ================================================
resourcestring
  RsDragToFloating = 'Drag to floating';

//=== JvOfficeColorForm.pas ==================================================
resourcestring
  RsColorWindow = 'Color Window';
  // (rom) probably the same as RsDragToFloating
  RsDragToFloat = 'Drag to float';

//=== JvOfficeColorPanel.pas =================================================
resourcestring
  RsAutoCaption = 'Automatic';
  RsOtherColorCaption = 'Other Colors...';

//=== JvPageSetup.pas ========================================================
resourcestring
  RsEInvalidValue = 'Value must be greater than zero';

//=== JvPainterQBForm.pas ====================================================
resourcestring
  RsPainterQuickBackdrops = 'Painter Quick Backdrops';
  RsEnterName = 'Enter Name:';
  RsNoItemSelected = 'No item selected!';
  RsErrorInPresets = 'Error in Presets';

//=== JvParameterList.pas ====================================================
resourcestring
  RsErrParameterMustBeEntered = 'Parameter "%s" must be entered!';

  RsHistorySelectPath = 'History';

  RsDialogCaption = '';
  RsOkButton = '&OK';
  RsCancelButton = '&Cancel';
  RsHistoryLoadButton = '&Load';
  RsHistorySaveButton = '&Save';
  RsHistoryClearButton = 'Cl&ear';
  RsHistoryLoadCaption = 'Load Parameter Settings';
  RsHistorySaveCaption = 'Save Parameter Settings';
  RsHistoryClearCaption = 'Manage Parameter Settings';

  RsENoParametersDefined = 'TJvParameterList.ShowParameterDialog: No Parameters defined';
  RsEAddObjectWrongObjectType = 'TJvParameterList.AddObject: Wrong object type';
  RsEAddObjectSearchNameNotDefined = 'TJvParameterList.AddObject: SearchName not defined';
  RsEAddObjectDuplicateSearchNamesNotAllowed = 'TJvParameterList.AddObject: Duplicate SearchNames ("%s") not allowed';

//=== JvParameterListParameter.pas ===========================================
resourcestring
  // RsErrParameterMustBeEntered = 'Parameter %s must be entered!';
  RsErrParameterIsNotAValidNumber = 'Parameter %s: %s is not a valid number value!';
  RsErrParameterMustBeBetween = 'Parameter %s: %s must be between %s and %s!';
  RsErrParameterFileDoesNotExist = 'Parameter %s: The file "%s" does not exist!';
  RsErrParameterFileExistOverwrite = 'Parameter %s: The file "%s" exists! Overwrite?';
  RsErrParameterDirectoryNotExist = 'Parameter %s: The directory "%s" does not exist!';

//=== JvParameterListTools.pas ===============================================
resourcestring
  RsSelectCaption = 'Select...';

//=== JvParserForm.pas =======================================================
resourcestring
  RsNewObject = 'New';

//=== JvPatchForm.pas ========================================================
resourcestring
  RsJvPatcherEditorComparingFilesd = 'Jv - Patcher Editor: Comparing files %d%%';
  RsJvPatcherEditorEndStep = 'Jv - Patcher Editor: end step ...';
  RsErrJvPatcherEditorInvalidFilename = 'Invalid filename(s). Please specify valid filenames for both source and destination and try again.';

//=== JvPcx.pas ==============================================================
resourcestring
  RsPcxExtension = 'pcx';
  RsPcxFilterName = 'PCX Image';

  RsEPcxUnknownFormat = 'PCX: Unknown format';
  RsEPcxPaletteProblem = 'PCX: Unable to retrieve palette';
  RsEPcxInvalid = 'PCX: Invalid PCX file';

//=== JvPerfMon95.pas ========================================================
resourcestring
  RsWrongOS = 'TJvPerfMon95 component is intended for Win95/98 only';

  RsECantOpenPerfKey = 'Performance registry key not found';
  RsECantStart = 'Cannot start performance statistics (%s)';
  RsECantStop = 'Cannot stop performance statistics (%s)';
  RsEKeyNotExist = 'Specified key "%s" does not exist';

//=== JvPickDate.pas =========================================================
resourcestring
  RsNextYearHint = 'Next Year|';
  RsNextMonthHint = 'Next Month|';
  RsPrevYearHint = 'Previous Year|';
  RsPrevMonthHint = 'Previous Month|';

//=== JvPlugin.pas ===========================================================
resourcestring
  RsEFmtResNotFound = 'Resource not found: %s';

//=== JvPluginManager.pas ====================================================
resourcestring
  RsEErrEmptyExt = 'Extension may not be empty';
  RsEPluginPackageNotFound = 'Plugin package not found: %s';
  RsERegisterPluginNotFound = 'Plugin function %s not found in %s';
  RsERegisterPluginFailed = 'Calling %s in %s failed';

//=== JvProfilerForm.pas =====================================================
resourcestring
  RsTotalElapsedTimedms = '%s -  total elapsed time: %d (ms)';
  RsTextFormatsasctxtinfdocAllFiles = 'Text formats|*.asc;*.txt;*.inf;*.doc|All files|*.*';
  RsDefCaption = 'Profiler 32 Report';
  RsDefHeader = 'Profiler 32 run %s by "%s" (machine %s).';

  RsEMaxNumberOfIDsExceededd = 'Max number of ID''s exceeded (%d)';
  RsEMaxStackSizeExceededd = 'Max stack size exceeded (%d)';

//=== JvPrvwRender.pas =======================================================
resourcestring
  RsEAPrintPreviewComponentMustBeAssigne = 'A PrintPreview component must be assigned in CreatePreview!';
  RsEARichEditComponentMustBeAssignedInC = 'A RichEdit component must be assigned in CreatePreview!';
  RsECannotPerfromThisOperationWhilePrin = 'Cannot perfrom this operation while printing!';
  RsEPrinterNotAssigned = 'Printer not assigned!';
  RsENoPrintPreviewAssigned = 'No PrintPreview assigned!';

//=== JvRas32.pas ============================================================
resourcestring
  RsRasDllName = 'RASAPI32.DLL';

  RsERasError = 'RAS: Unable to find RasApi32.dll';

//=== JvRegistryTreeview.pas =================================================
resourcestring
  RsDefaultCaption = '(Default)';
  RsMyComputer = 'My Computer';
  RsDefaultNoValue = '(value not set)';
  RsUnknownCaption = '(Unknown)';

//=== JvRichEdit.pas =========================================================
resourcestring
  RsRTFFilter = 'Rich Text Format (*.rtf)|*.rtf';
  RsTextFilter = 'Plain text (*.txt)|*.txt';

  RsEConversionError = 'Conversion error %.8x';
  RsEConversionBusy = 'Cannot execute multiple conversions';
  RsECouldNotInitConverter = 'Could not initialize converter';
  RsEDiskFull = 'Out of space on output';
  RsEDocTooLarge = 'Conversion document too large for target';
  RsEInvalidDoc = 'Invalid document';
  RsEInvalidFile = 'Invalid data in conversion file';
  RsENoMemory = 'Out of memory';
  RsEOpenConvErr = 'Error opening conversion file';
  RsEOpenExceptErr = 'Error opening exception file';
  RsEOpenInFileErr = 'Could not open input file';
  RsEOpenOutFileErr = 'Could not open output file';
  RsEReadErr = 'Error during read';
  RsEUserCancel = 'Conversion cancelled by user';
  RsEWriteErr = 'Error during write';
  RsEWriteExceptErr = 'Error writing exception file';
  RsEWrongFileType = 'Wrong file type for this converter';

//=== JvSAL.pas ==============================================================
resourcestring
  RsEBooleanStackOverflow = 'Boolean stack overflow';
  RsEBooleanStackUnderflow = 'Boolean stack underflow';
  RsEProgramStopped = 'Program stopped';
  RsEUnterminatedIncludeDirectiveNears = 'Unterminated include directive near %s';
  RsEOngetUnitEventHandlerIsNotAssigned = 'OngetUnit event handler is not assigned';
  RsECouldNotIncludeUnits = 'Could not include unit %s';
  RsEUnterminatedCommentNears = 'Unterminated comment near %s';
  RsEUnterminatedProcedureNears = 'Unterminated procedure near %s';
  RsEVariablesAllreadyDefineds = 'Variable %s allready defined;%s';
  RsEVariablesIsNotYetDefineds = 'Variable %s is not yet defined;%s';
  RsEProceduresNears = 'Procedure %s near %s';
  RsEUndefinedProcedures = 'Undefined procedure %s';
  RsECouldNotFindEndOfProcedure = 'Could not find end of procedure';

//=== JvSALCore.pas ==========================================================
resourcestring
  RsEVariablesIsNotInitialized = 'Variable %s is not initialized';
  RsEDivisionByZeroError = 'Division by zero error';
  RsEMissingendselect = 'Missing "endselect"';

//=== JvSchedEvtStore.pas ====================================================
resourcestring
  RsEStructureStackIsEmpty = 'Structure stack is empty';
  RsEScheduleIsActiveReadingANewSchedule =
    'Schedule is active. Reading a new schedule can only be done on inactive schedules';
  RsEScheduleIsActiveStoringOfAScheduleC =
    'Schedule is active. Storing of a schedule can only be done on inactive schedules';
  RsENotImplemented_ = 'not implemented';
  RsENotASchedule = 'Not a schedule';
  RsEUnknownScheduleVersions = 'Unknown schedule version ($%s)';
  RsEUnexpectedStructure = 'Unexpected structure';
  RsEIncorrectIdentifierFound = 'Incorrect identifier found';
  RsEIncorrectStructure = 'Incorrect structure found';

//=== JvScheduledEvents.pas ==================================================
resourcestring
  RsECannotRestart = 'Cannot restart: Event is being triggered or is executing';

//=== JvScrollMax.pas ========================================================
resourcestring
  RsRightClickAndChooseAddBand = 'Right click and choose "Add band"';

  { (rom) deactivated  see DefineCursor in JvScrollMax.pas
  RsECannotLoadCursorResource = 'Cannot load cursor resource';
  RsETooManyUserdefinedCursors = 'Too many user-defined cursors';
  }
  RsETJvScrollMaxBandCanBePutOnlyIntoTJv = 'TJvScrollMaxBand can be put only into TJvScrollMax component';
  RsETJvScrollMaxCanContainOnlyTJvScroll = 'TJvScrollMax can contain only TJvScrollMaxBand components';
  RsEControlsNotAChildOfs = 'Control %s not a child of %s';

//=== JvSegmentedLEDDisplay.pas ==============================================
resourcestring
  RsEInvalidClass = 'Invalid class';
  RsEInvalidMappingFile = 'Invalid mapping file';
  RsEDuplicateDigitClass = 'Duplicate DigitClass registered';

//=== JvSegmentedLEDDisplayMapperFrame.pas ===================================
resourcestring
  RsTheCurrentCharacterHasBeenModifiedA = 'The current character has been modified. Apply changes?';
  RsTheCurrentMappingHasBeenModifiedSav = 'The current mapping has been modified. Save changes to file?';
  RsSegmentedLEDDisplayMappingFilessdms = 'Segmented LED display mapping files (*.sdm)|*.sdm|All files (*.*)|*.*';
  RsSelectCharacter = 'Select character...';
  RsSpecifyANewCharacter = 'Specify a new character';

//=== JvSHFileOperation.pas ==================================================
resourcestring
  RsENoFilesSpecifiedToTJvSHFileOperatio = 'No files specified to TJvSHFileOperation Execute function';

//=== JvSimpleXml.pas ========================================================
resourcestring
  {$IFNDEF COMPILER6_UP}
  // RsEInvalidBoolean = '''%s'' is not a valid Boolean value'; make Delphi 5 compiler happy // andreas
  {$ENDIF COMPILER6_UP}
  RsEInvalidXMLElementUnexpectedCharacte =
    'Invalid XML Element: Unexpected character in property declaration ("%s" found)';
  RsEInvalidXMLElementUnexpectedCharacte_ =
    'Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but "%s"  found';
  RsEUnexpectedValueForLPos = 'Unexpected value for lPos';
  RsEInvalidXMLElementExpectedBeginningO = 'Invalid XML Element: Expected beginning of tag but "%s" found';
  RsEInvalidXMLElementExpectedEndOfTagBu = 'Invalid XML Element: Expected end of tag but "%s" found';
  RsEInvalidXMLElementMalformedTagFoundn = 'Invalid XML Element: malformed tag found (no valid name)';
  RsEInvalidXMLElementErroneousEndOfTagE =
    'Invalid XML Element: Erroneous end of tag, expecting </%s> but </%s> found';
  RsEInvalidCommentExpectedsButFounds = 'Invalid Comment: expected "%s" but found "%s"';
  RsEInvalidCommentNotAllowedInsideComme = 'Invalid Comment: "--" not allowed inside comments';
  RsEInvalidCommentUnexpectedEndOfData = 'Invalid Comment: Unexpected end of data';
  RsEInvalidCDATAExpectedsButFounds = 'Invalid CDATA: expected "%s" but found "%s"';
  RsEInvalidCDATAUnexpectedEndOfData = 'Invalid CDATA: Unexpected end of data';
  RsEInvalidHeaderExpectedsButFounds = 'Invalid Header: expected "%s" but found "%s"';
  RsEInvalidStylesheetExpectedsButFounds = 'Invalid Stylesheet: expected "%s" but found "%s"';
  RsEInvalidStylesheetUnexpectedEndOfDat = 'Invalid Stylesheet: Unexpected end of data';
  RsEInvalidDocumentUnexpectedTextInFile = 'Invalid Document: Unexpected text in file prolog';

//=== JvSpeedbar.pas =========================================================
resourcestring
  RsEAutoSpeedbarMode = 'Cannot set this property value while Position is bpAuto';

//=== JvSpeedbarSetupForm.pas ================================================
resourcestring
  RsCustomizeSpeedbar = 'Customize Speedbar';
  RsAvailButtons = '&Available buttons:';
  RsSpeedbarCategories = '&Categories:';
  RsSpeedbarEditHint = 'To add command buttons, drag and drop buttons onto the SpeedBar.' +
    ' To remove command buttons, drag them off the SpeedBar.';

//=== JvSpellChecker.pas =====================================================
resourcestring
  RsENoSpellCheckerAvailable = 'No IJvSpellChecker implementation available!';

//=== JvSpellerForm.pas ======================================================
resourcestring
  RsENoDictionaryLoaded = 'No dictionary loaded';

//=== JvSpin.pas =============================================================
resourcestring
  RsEOutOfRangeFloat = 'Value must be between %g and %g';

//=== JvStatusBar.pas ========================================================
resourcestring
  RsEInvalidControlSelection = 'Invalid control selection';

//=== JvSticker.pas ==========================================================
resourcestring
  RsEditStickerCaption = 'Edit sticker';

//=== JvStringHolder.pas =====================================================
resourcestring
  RsENoItemFoundWithName = 'No item found with name "%s"';

//=== JvStrings.pas ==========================================================
resourcestring
  RsECannotLoadResource = 'Cannot load resource: %s';
  RsEIncorrectStringFormat = 'Base64: Incorrect string format';

//=== JvSyncSplitter.pas =====================================================
resourcestring
  RsEInvalidPartner = 'TJvSyncSplitter.SetPartner: cannot set Partner to Self!';

//=== JvSystemPopup.pas ======================================================
resourcestring
  RsEAlreadyHooked = 'TJvSystemPopup.Hook: already hooked';

//=== JvTFDays.pas ===========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEInvalidPrimeTimeStartTime = 'Invalid PrimeTime StartTime';
  RsEInvalidPrimeTimeEndTime = 'Invalid PrimeTime EndTime';
  RsEColumnIndexOutOfBounds = 'Column index out of bounds';
  RsERowIndexOutOfBounds = 'Row index out of bounds';
  RsEMapColNotFoundForAppointment = 'Map column not found for appointment';
  RsECorruptAppointmentMap = 'Corrupt appointment map';
  RsEGridGranularityCannotBeGreater = 'Grid granularity cannot be greater ' +
    'than the time block granularity';
  RsETimeBlockGranularityMustBeEvenly = 'Time block granularity must be evenly ' +
    'divisible by the grid granularity';
  RsETimeBlocksMustBeginExactlyOn = 'Time blocks must begin exactly on ' +
    'a grid time division';
  RsEGridEndTimeCannotBePriorToGridStart = 'GridEndTime cannot be prior to GridStartTime';
  RsEGridStartTimeCannotBeAfterGridEndTi = 'GridStartTime cannot be after GridEndTime';
  RsEInvalidRowd = 'Invalid row (%d)';
  RsEThereIsNoDataToPrint = 'There is no data to print';
  RsENoPageInfoExists = 'No page info exists.  ' +
    'Document must be prepared';
  RsEATimeBlockNameCannotBeNull = 'A time block name cannot be null';
  RsEAnotherTimeBlockWithTheName = 'Another time block with the name ' +
    '"%s" already exists';
  RsEATimeBlockWithTheNamesDoesNotExist = 'A time block with the name "%s" does not exist';
{$ENDIF USEJVCL}

//=== JvTFGantt.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsThisIsTheMajorScale = 'This is the Major Scale';
  RsThisIsTheMinorScale = 'This is the Minor Scale';
{$ENDIF USEJVCL}

//=== JvTFGlance.pas =========================================================
{$IFDEF USEJVCL}
resourcestring
  RsECellDatesCannotBeChanged = 'Cell Dates cannot be changed';
  RsECellMapHasBeenCorrupteds = 'Cell map has been corrupted %s';
  RsECellObjectNotAssigned = 'Cell object not assigned';
  RsEInvalidColIndexd = 'Invalid col index (%d)';
  RsEInvalidRowIndexd = 'Invalid row index (%d)';
  RsEApptIndexOutOfBoundsd = 'Appt index out of bounds (%d)';
  RsECellCannotBeSplit = 'Cell cannot be split';
  RsEASubcellCannotBeSplit = 'A subcell cannot be split';
  RsGlanceMainTitle = '(Title)';
{$ENDIF USEJVCL}

//=== JvTFGlanceTextViewer.pas ===============================================
{$IFDEF USEJVCL}
resourcestring
  RsEGlanceControlNotAssigned = 'GlanceControl not assigned';
{$ENDIF USEJVCL}

//=== JvTFManager.pas ========================================================
{$IFDEF USEJVCL}
resourcestring
  RsECouldNotCreateCustomImageMap = 'Could not create CustomImageMap.  ' +
    'Appointment not assigned';
  RsECouldNotCreateAppointmentObject = 'Could not create Appointment object.  ' +
    'ScheduleManager not assigned';
  RsEScheduleManagerNotificationFailedSc = 'ScheduleManager notification failed.  ScheduleManager not assigned';
  RsEScheduleNotificationFailed = 'Schedule notification failed.  ' +
    'Schedule not assigned';
  RsEInvalidStartAndEndTimes = 'Invalid start and end times';
  RsEInvalidStartAndEndDates = 'Invalid start and end dates';
  RsEAppointmentNotificationFailed = 'Appointment notification failed.  ' +
    'Appointment not assigned';
  RsECouldNotCreateNewAppointment = 'Could not create new appointment. ' +
    'Appointment with given ID already exists';
  RsEInvalidTriggerForRefreshControls = 'Invalid Trigger for RefreshControls';
  RsEInvalidScopeInReconcileRefresh = 'Invalid Scope in ReconcileRefresh';
  RsECouldNotRetrieveSchedule = 'Could not retrieve schedule.  ' +
    'ScheduleManager not assigned';
  RsECouldNotReleaseSchedule = 'Could not release schedule.  ' +
    'ScheduleManager not assigned';
  RsECouldNotCreateADocumentBecauseA = 'Could not create a document because a ' +
    'document already exists';
  RsECouldNotFinishDocumentBecauseNo = 'Could not finish document because no ' +
    'document has been created';
  RsEDocumentDoesNotExist = 'Document does not exist';
  RsEDocumentPagesCannotBeAccessedIf = 'Document pages cannot be accessed if ' +
    'printing directly to the printer';
  RsEDocumentPagesAreInaccessibleUntil = 'Document pages are inaccessible until ' +
    'the document has been finished';
  RsECouldNotRetrievePageCount = 'Could not retrieve page count ' +
    'because document does not exist';
  RsEOnlyAFinishedDocumentCanBePrinted = 'Only a finished document can be printed';
  RsEThereAreNoPagesToPrint = 'There are no pages to print';
  RsEDocumentMustBeFinishedToSaveToFile = 'Document must be Finished to save to file';
  RsEThisPropertyCannotBeChangedIfA = 'This property cannot be changed if a ' +
    'document exists';
  RsECouldNotCreateTJvTFPrinterPageLayou = 'Could not create TJvTFPrinterPageLayout ' +
    'because aPrinter must be assigned';
  RsEInvalidFooterHeightd = 'Invalid Footer Height (%d)';
  RsEInvalidHeaderHeightd = 'Invalid Header Height (%d)';
{$ENDIF USEJVCL}

//=== JvTFSparseMatrix.pas ===================================================
{$IFDEF USEJVCL}
resourcestring
  RsEMatrixMustBeEmpty = 'Matrix must be empty before setting null value';
{$ENDIF USEJVCL}

//=== JvTFUtils.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsEResultDoesNotFallInMonth = 'Result does not fall in given month';
  RsEInvalidMonthValue = 'Invalid Month Value (%d)';
  RsEInvalidDayOfWeekValue = 'Invalid value for day of week (%d)';
{$ENDIF USEJVCL}

//=== JvTFWeeks.pas ==========================================================
{$IFDEF USEJVCL}
resourcestring
  RsWeekOf = 'Week of %s';
{$ENDIF USEJVCL}

//=== JvThumbImage.pas =======================================================
resourcestring
  RsEUnknownFileExtension = 'Unknown file extension %s';
  RsFileFilters = '|PCX Files(*.pcx)|*.pcx|Targa Files(*.tga)|*.tga';
  RsPcxTga = '*.pcx;*.tga;';

//=== JvThumbnails.pas =======================================================
resourcestring
  RsUnknown = 'Unknown';

//=== JvTimeLimit.pas ========================================================
resourcestring
  RsExpired = 'The test period has expired, please register this application';

//=== JvTimeList.pas =========================================================
resourcestring
  RsEOwnerMustBeTJvTimerList = 'Owner of TJvTimerEvents must be a TJvTimerList';

//=== JvTipOfDay.pas =========================================================
resourcestring
  RsCloseCaption = '&Close';
  RsNextCaption = '&Next Tip';
  RsTipsTitle = 'Tips and Tricks';
  RsTipsHeaderText = 'Did you know...';
  RsTipsCheckBoxText = '&Show Tips on Startup';
  RsStoreShowOnStartUp = 'Show_On_Startup'; // (p3) Spaces in XML node names is not valid XML...

//=== JvToolEdit.pas =========================================================
resourcestring
  RsBrowseCaption = 'Browse';
  {$IFDEF MSWINDOWS}
  RsDefaultFilter = 'All files (*.*)|*.*';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  RsDefaultFilter = 'All files (*)|*';
  {$ENDIF UNIX}

  { Polaris patch }
  RsEDateMinLimit = 'Enter a date before "%s"';
  RsEDateMaxLimit = 'Enter a date after "%s"';

//=== JvTurtle.pas ===========================================================
resourcestring
  RsErrorCanvasNotAssigned = 'Canvas not assigned';
  RsEmptyScript = 'Empty script';
  RsInvalidIntegerIns = 'Invalid integer in %s';
  RsInvalidColorIns = 'Invalid color in %s';
  RsInvalidCopyMode = 'Invalid copy mode';
  RsInvalidPenMode = 'Invalid pen mode';
  RsInvalidTextIns = 'Invalid text in %s';
  RsMissingFontname = 'Missing fontname';
  RsNumberExpectedIns = 'Number expected in %s';
  RsNumberStackUnderflow = 'Number stack underflow';
  RsNumberStackOverflow = 'Number stack overflow';
  RsMissingAfterComment = 'Missing "}" after comment';
  RsErrorIns = 'Error in %s';
  RsDivisionByZero = 'Division by zero';
  RsInvalidParameterIns = 'Invalid parameter in %s';
  RsSymbolsIsNotDefined = 'Symbol %s is not defined';
  RsMissingAfterBlock = 'Missing "]" after block';
  RsStackUnderflowIns = 'Stack underflow in %s';
  RsSymbolExpectedAfterIf = 'Symbol expected after if';
  RsCanNotTakeSqrtOf = 'Can not take sqrt of 0';
  RsNotAllowedIns = '0 not allowed in %s';
  RsNeedMinimumOfSidesIns = 'Need minimum of 3 sides in %s';
  RsMaximumSidesExceededIns = 'Maximum 12 sides exceeded in %s';
  RsTokenExpected = 'Token expected';
  RssDoesNotExist = '%s does not exist';
  RsDivisionByZeroNotAllowedInIn = 'Division by zero not allowed in in-';
  RsStackOverflow = 'Stack overflow';
  RsStackUnderflow = 'Stack underflow';

//=== JvTypes.pas ============================================================
resourcestring
  RsClBlack = 'Black';
  RsClMaroon = 'Maroon';
  RsClGreen = 'Green';
  RsClOlive = 'Olive green';
  RsClNavy = 'Navy blue';
  RsClPurple = 'Purple';
  RsClTeal = 'Teal';
  RsClGray = 'Gray';
  RsClSilver = 'Silver';
  RsClRed = 'Red';
  RsClLime = 'Lime';
  RsClYellow = 'Yellow';
  RsClBlue = 'Blue';
  RsClFuchsia = 'Fuchsia';
  RsClAqua = 'Aqua';
  RsClWhite = 'White';
  RsClMoneyGreen = 'Money green';
  RsClSkyBlue = 'Sky blue';
  RsClCream = 'Cream';
  RsClMedGray = 'Medium gray';
  {$IFDEF VCL}
  // windows system colors
  RsClScrollBar = 'Scrollbar';
  RsClBackground = 'Desktop background';
  RsClActiveCaption = 'Active window title bar';
  RsClInactiveCaption = 'Inactive window title bar';
  RsClMenu = 'Menu background';
  RsClWindow = 'Window background';
  RsClWindowFrame = 'Window frame';
  RsClMenuText = 'Menu text';
  RsClWindowText = 'Window text';
  RsClCaptionText = 'Active window title bar text';
  RsClActiveBorder = 'Active window border';
  RsClInactiveBorder = 'Inactive window border';
  RsClAppWorkSpace = 'Application workspace';
  RsClHighlight = 'Selection background';
  RsClHighlightText = 'Selection text';
  RsClBtnFace = 'Button face';
  RsClBtnShadow = 'Button shadow';
  RsClGrayText = 'Dimmed text';
  RsClBtnText = 'Button text';
  RsClInactiveCaptionText = 'Inactive window title bar text';
  RsClBtnHighlight = 'Button highlight';
  RsCl3DDkShadow = 'Dark shadow 3D elements';
  RsCl3DLight = 'Highlight 3D elements';
  RsClInfoText = 'Tooltip text';
  RsClInfoBk = 'Tooltip background';
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  // VisualCLX system colors
  RsClText = 'Edit text';
  RsClForeground = 'Window text';
  RsClBrightText = 'Bright text';
  RsClButtonText = 'Button text';
  RsClHighlightedText = 'Selection text';
  RsClHighlight = 'Selection background';
  RsClBackground = 'Window background';
  RsClBase = 'Edit background';
  RsClLight = 'Light 3D elements';
  RsClMidlight = 'Medium Light 3D elements';
  RsClButton = 'Button face';
  RsClMid  = 'Light Shadow 3D elements';
  RsClShadow = 'Shadow 3D elements';
  RsClDark = 'Dark Shadow 3D elements';

  RsClNormalText ='Normal edit text';
  RsClNormalForeground ='Normal window text';
  RsClNormalBrightText ='Normal bright text';
  RsClNormalButtonText ='Normal button text';
  RsClNormalHighlightedText ='Normal selection text';
  RsClNormalHighlight ='Normal selection background';
  RsClNormalBackground ='Normal window background';
  RsClNormalBase ='Normal edit background';
  RsClNormalLight ='Normal light 3D elements';
  RsClNormalMidlight ='Normal medium light 3D elements';
  RsClNormalButton ='Normal button face';
  RsClNormalMid  ='Normal light shadow 3D elements';
  RsClNormalShadow ='Normal shadow 3D elements';
  RsClNormalDark ='Normal dark shadow 3D elements';

  RsClDisabledText ='Disabled edit text';
  RsClDisabledForeground ='Disabled window text';
  RsClDisabledBrightText ='Disabled bright text';
  RsClDisabledButtonText ='Disabled button text';
  RsClDisabledHighlightedText ='Disabled selection text';
  RsClDisabledHighlight ='Disabled selection background';
  RsClDisabledBackground ='Disabled window background';
  RsClDisabledBase ='Disabled edit background';
  RsClDisabledLight ='Disabled light 3D elements';
  RsClDisabledMidlight ='Disabled medium light 3D elements';
  RsClDisabledButton ='Disabled button face';
  RsClDisabledMid  ='Disabled light shadow 3D elements';
  RsClDisabledShadow ='Disabled shadow 3D elements';
  RsClDisabledDark ='Disabled dark shadow 3D elements';

  RsClActiveText ='Active edit text';
  RsClActiveForeground ='Active window text';
  RsClActiveBrightText ='Active bright text';
  RsClActiveButtonText ='Active button text';
  RsClActiveHighlightedText ='Active selection text';
  RsClActiveHighlight ='Active selection background';
  RsClActiveBackground ='Active window background';
  RsClActiveBase ='Active edit background';
  RsClActiveLight ='Active light 3D elements';
  RsClActiveMidlight ='Active medium light 3D elements';
  RsClActiveButton ='Active button face';
  RsClActiveMid  ='Active light shadow 3D elements';
  RsClActiveShadow ='Active shadow 3D elements';
  RsClActiveDark ='Active dark shadow 3D elements';
  // non standard colors introduced in QWindows.pas
  RsClDesktop = 'Desktop background';
  RsClInfoBk = 'Tooltip background';
  {$ENDIF VisualCLX}

//=== JvUrlGrabbers.pas ======================================================
resourcestring
  RsFileNotFoundFmt = 'File "%s" not found';

//=== JvUrlListGrabber.pas ===================================================
resourcestring
  RsENoGrabberForUrl = 'There is no grabber capable of handling URL: %s';
  RsEGrabberNotStopped = 'The grabber is not stopped, you cannot change its URL.';

//=== JvValidators.pas =======================================================
resourcestring
  RsEInsertNilValidator = 'Cannot insert nil validator';
  RsERemoveNilValidator = 'Cannot remove nil validator';
  RsEValidatorNotChild = 'Validator is not owned by this component';
  RsEInvalidIndexd = 'Invalid index (%d)';

//=== JvVirtualKeySelectionFrame.pas =========================================
resourcestring
  RsNoValidKeyCode = 'This is not a valid key code';
  RsInvalidKeyCode = 'Invalid key code';

//=== JvWinampLabel.pas ======================================================
resourcestring
  RsWinampRC = 'WINAMP1';

  RsEInvalidSkin = 'Invalid skin';

//=== JvWinDialogs.pas =======================================================
resourcestring
  //SDiskFullError =
  //  'TJvDiskFullDialog does not support removable media or network drives.';
  RsEFunctionNotSupported = 'This function is not supported by your version of Windows';
  RsEInvalidDriveChar = 'Invalid drive (%s)';
  { make Delphi 5 compiler happy // andreas
    RsEUnsupportedDisk = 'Unsupported drive (%s): JvDiskFullDialog only supports fixed drives';}

//=== JvWinHelp.pas ==========================================================
resourcestring
  RsEOwnerForm = 'Owner must be of type TCustomForm';

//=== JvWizard.pas ===========================================================
// (rom) no IFDEF USEJVCL because of CLX version
resourcestring
  RsFirstButtonCaption = 'To &Start Page';
  RsLastButtonCaption = 'To &Last Page';
  RsFinishButtonCaption = '&Finish';
  RsWelcome = 'Welcome';
  RsTitle = 'Title';
  RsSubtitle = 'Subtitle';

  RsEInvalidParentControl = 'The Parent should be TJvWizard or a descendant';
  RsEInvalidWizardPage = 'The pages belong to another wizard';

//=== JvWizardCommon.pas =====================================================
{$IFDEF USEJVCL}
resourcestring
  RsETilingError = 'Tiling only works on images with dimensions > 0';
{$ENDIF USEJVCL}

//=== JvWizardRouteMapSteps.pas ==============================================
{$IFDEF USEJVCL}
resourcestring
  RsActiveStepFormat = 'Step %d of %d';
  RsBackTo = 'Back to';
  RsNextStep = 'Next Step';
{$ENDIF USEJVCL}

//=== JvXmlDatabase.pas ======================================================
resourcestring
  RsEUnknownInstruction = 'Unknown Instruction %s';
  RsEUnexpectedEndOfQuery = 'Unexpected end of query';
  RsEUnexpectedStatement = 'Unexpected statement %s';

//=== JvXPBar.pas ============================================================
{$IFDEF USEJVCL}
resourcestring
  RsUntitled = 'untitled';
{$ENDIF USEJVCL}

//=== JvXPCore.pas ===========================================================
{$IFDEF USEJVCL}
resourcestring
  RsCopyright = 'Design eXperience. (c) 2002 M. Hoffmann Version ';
  RsCopyright2 = 'Design eXperience II - (c) 2002 M. Hoffmann Version ';
  RsVersion = '2.0.1'; // always increase version number on new releases!
{$ENDIF USEJVCL}

//=== JvYearGrid.pas =========================================================
{$IFDEF USEJVCL}
resourcestring
  RsYearGrid = 'YearGrid';
  RsEnterYear = 'Enter year (1999-2050):';
  RsInvalidYear = 'invalid year';
  RsYear = '&Year...';
  RsEdit = '&Edit';
  RsColor = '&Color...';
  RsNoColor = '&No Color';
  RsSaveAllInfo = '&Save All Info';
  RsSaveFoundInfo = 'Save Found Info';
  RsBorderColor = '&Border Color...';
  RsBookMarkColor = 'Book&Mark Color...';
  RsFindItem = '&Find...';
  RsClearFind = 'Clear Find';
  RsYearGridFind = 'YearGrid Find';
  RsEnterSeachText = 'Enter seach text:';
  RsFounds = 'Found %s';
  RsToday = 'Today ';
{$ENDIF USEJVCL}
  
//=== not taken into JVCL ====================================================
{
resourcestring
  // MathParser
  SParseSyntaxError = 'Syntax error';
  SParseNotCramp = 'Invalid condition (no cramp)';
  SParseDivideByZero = 'Divide by zero';
  SParseSqrError = 'Invalid floating operation';
  SParseLogError = 'Invalid floating operation';
  SParseInvalidFloatOperation = 'Invalid floating operation';
  // JvDBFilter
  SExprNotBoolean = 'Field ''%s'' is not of type Boolean';
  SExprBadNullTest = 'NULL only allowed with ''='' and ''<>''';
  SExprBadField = 'Field ''%s'' cannot be used in a filter expression';
  // JvDBFilter expression parser
  SExprIncorrect = 'Incorrectly formed filter expression';
  SExprTermination = 'Filter expression incorrectly terminated';
  SExprNameError = 'Unterminated field name';
  SExprStringError = 'Unterminated string constant';
  SExprInvalidChar = 'Invalid filter expression character: ''%s''';
  SExprNoRParen = ''')'' expected but %s found';
  SExprExpected = 'Expression expected but %s found';
  SExprBadCompare = 'Relational operators require a field and a constant';
}

{$IFNDEF USEJVCL}
  {$UNDEF UNITVERSIONING}
{$ENDIF ~USEJVCL}

implementation

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
