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

{$IFNDEF COMPILER6_UP}
uses
  JvTypes;
{$ENDIF COMPILER6_UP}


resourcestring
  //=== strings used by several editors ======================================
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

  //=== JvAVICaptureEditors.pas ==============================================
  SDisconnected = 'Disconnected';
  SdIsNotWithinTheValidRangeOfdd = '%d is not within the valid range of %d..%d';

  //=== JvBandObjectDLLWizard.pas ============================================
  SCreatesABandObjectDLLProject = 'Creates a Band Object DLL Project.';
  SBandObjectDLLWizard = 'Band Object DLL Wizard';

  //=== JvBandObjectDLLWizardForm.pas ========================================
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

  //=== JvChangeNotifyEditor.pas =============================================
  SNotificationsEllipsis = 'Notifications...';

  //=== JvCheckedItemsForm.pas ===============================================
  SItemEditor = 'Item editor';
  SEnabled = 'Enabled';

  //=== JvColorProviderDesignerForm.pas ======================================
  SSystemColors = 'System colors';
  SStandardColors = 'Standard colors';
  SCustomColorsEllipsis = 'Custom colors...';
  SColorMsg = 'Copy standard and system colors from the default context?';

  //=== JvColorProviderEditors.pas ===========================================
  SMappingDoesNotExistForThisColorProv = 'Mapping does not exist for this color provider.';

  //=== JvCsvDataEditor.pas ==================================================
  SJvCsvDataSetSelectCSVFileToOpen = 'JvCsvDataSet - Select CSV File to Open';

  //=== JvCsvDataForm.pas ====================================================
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

  //=== JvDataConsumerContextSelectForm.pas ==================================
  SConsumerDoesNotSupportContextSelect = 'Consumer does not support context selection.';
  SIJvDataConsumerProviderIsNotSupported = 'IJvDataConsumerProvider is not supported by the specified consumer.';

  //=== JvDataConsumerItemSelectForm.pas =====================================
  SDataProviderItemSelector = 'DataProvider Item Selector';

  //=== JvDataContextManagerForm.pas =========================================
  SDataProviderContextManager = 'DataProvider Context Manager';

  //=== JvDataProviderDesignerForm.pas =======================================
  SDataProviderDesigner = 'DataProvider Designer';
  SInternalErrorUnableToRetrieveContext = 'Internal error: unable to retrieve context list';

  //=== JvDataProviderEditors.pas ============================================
  SSpecifiedProviderIsNotATComponentDe = 'Specified provider is not a TComponent descendant';
  STreeDesignerEllipsis = 'Tree designer...';
  SContextManagerEllipsis = 'Context manager...';
  SInvalidVerbd = 'Invalid verb#: %d';

  //=== JvDataProviderItemDesign.pas =========================================
  SUnknown = '<unknown>';
  SNoItem = '<no item>';

  //=== JvDateTimeForm.pas ===================================================
  SSelectDate = 'Select Date';
  SSelectTime = 'Select Time';
  SSelectDateTime = 'Select Date and Time';

  //=== JvDsgnEditors.pas ====================================================
  SStripFilePath = '&Strip file path';
  SExecutableFilesExeExeAllFiles = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
  SItems = 'Items';
  SFmtEditEllipsis = '%s Editor...';

  //=== JvFooterEditor.pas ===================================================
  SAddButtonText = 'Add button';
  SMSOffice = 'MS Office 2000';
  SMSEnterpriseManagerWizard = 'MS Enterprise Manager Wizard';
  SDialogMode = 'Dialog Mode';
  SPrevious = 'Previous';
  SNext = 'Next';

  //=== JvgComponentListEditorForm.pas =======================================
  SEditComponentListEllipsis = 'Edit component list...';

  //=== JvgHelpPanelEditor.pas ===============================================
  SRTFAndTextFilesrtftxtrtftxt = 'RTF and Text files (*.rtf,*.txt)|*.rtf;*.txt';
  SLoadRTFFile = 'Load RTF file';
  SPreviewRTFText = 'Preview RTF text';

  //=== JvgLabelEditorForm.pas ===============================================
  SEditLabel = 'Edit &Label...';

  //=== JvgLogicItemEditorForm.pas ===========================================
  SLogicElements = 'Logic Element: %s';
  SResult = '[RESULT]';
  SNotDefined = '[ not defined ]';

  //=== JvgLogicsEditorForm.pas ==============================================
  SCaption = 'Caption';
  SComments = 'Comments';
  SEditComponentEllipsis = 'Edit component...';

  //=== JvgReportEditorForm.pas ==============================================
  SEditReport = 'Edit report...';
  SPreviewReportEllipsis = 'Preview report...';
  SDeleteObject = 'Delete object?';
  SPagePreview = 'Page Preview';

  //=== JvgReportParamsForm.pas ==============================================
  SEditParamsEllipsis = 'Edit params...';

  //=== JvGroupHeaderEditor.pas ==============================================
  SStandardFlat = 'Standard/Flat';
  SWeb = 'Web';

  //=== JvgShadowEditor.pas ==================================================
  SUpdateAllEditControl = 'Update all edit control';

  //=== JVHLEditEditor.pas ===================================================
  SHLEditorMsg = 'Please select "JvHLEditor" first';
  SHLEditorMsgTitle = 'Cannot edit';
  SExecute = 'Execute';

  //=== JvID3v2EditorForm.pas ================================================
  SCommit = 'Commit?';

  //=== JvIDEZoom.pas ========================================================
  SZoomEditWindow = 'Zoom Edit Window';

  //=== JvImagePreviewForm.pas ===============================================
  SPreview = 'Preview';

  //=== JvJVCLAboutEditor.pas ================================================
  SVersions = 'Version %s';

  //=== JvLookoutEditor.pas ==================================================
  SAddPage = 'Add page';
  SActivate = 'Activate';
  SAddButton = 'Add Button';
  SScrollUp = 'Scroll Up';
  SScrollDown = 'Scroll Down';
  SAddPage_ = 'Add Page';

  //=== JvMailEditor.pas =====================================================
  SSend = 'Send';
  SAddress = 'Address';

  //=== JvOutlookBarEditors.pas ==============================================
  SOLEditor = 'OutlookBar Editor...';

  //=== JvOutlookBarForm.pas =================================================
  SDesignerIsNilInFormClosed = 'Designer is nil in FormClosed';
  SFmtCaption = 'Editing %s';
  SOutlookBarCaption = 'OutlookBar Editor';

  //=== JvPageLinkEditor.pas =================================================
  SCreateLinkToPaged = 'Create link to page %d';

  //=== JvPageListTreeViewReg.pas ============================================
  SFmtInterfaceNotSupported = '%s does not support the required interface (%s)';
  SNextPageAmp = 'Ne&xt Page';
  SPrevPage = '&Previous Page';
  SNewPage = '&New Page';
  SDelPage = '&Delete Page';

  //=== JvPictureEditForm.pas ================================================
  SLoadPicture = 'Load picture';
  SSavePictureAs = 'Save picture as';

  //=== JvPluginParamsForm.pas ===============================================
  SPluginParamsFormInfoText =
    'The settings above will create the following project:' +
    sLineBreak + sLineBreak +
    '* A project called Plg%0:s.%1:s' + sLineBreak +
    '* A unit called Plugin%0:s, containing the data module T%0:s.';

  //=== JvPluginWizard.pas ===================================================
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

  //=== JvPreviewReg.pas =====================================================
  SCreatePreview = 'Create Preview';
  SClearPreview = 'Clear Preview';

  //=== JvScheduleEditorForm.pas =============================================
  SInvalidScheduleSettingsFound = 'Invalid schedule settings found.';
  SStop = 'Stop';
  SRun = 'Run';

  //=== JvScheduleEditors.pas ================================================
  SEventEditor = 'Event editor...';

  //=== JvScrollMaxEditor.pas ================================================
  SAddBand = 'Add Band';

  //=== JvSegmentedLEDDisplayEditors.pas =====================================
  SAddDigit = 'Add digit';
  SRemoveDigit = 'Remove digit';
  SEditMappingEllipsis = 'Edit mapping...';

  //=== JvSegmentedLEDDisplayMappingForm.pas =================================
  SSegmentedLEDDisplayMappingEditor = 'Segmented LED Display Mapping Editor';

  //=== JvSegmentedLEDDisplayMappingForm.pas =================================
  STextFilter =
    'Text files (*.TXT)|*.TXT|Config files (*.SYS;*.INI)|*.SYS;*.INI|Batch files (*.BAT)|*.BAT|All files (*.*)|*.*';
  //  STextFilter = 'Text files (*.txt)|*.txt|Config files (*.sys;*.ini)|*.sys;*.ini|Batch files (*.bat)|*.bat|All files (*.*)|*.*';
  SSingleLine = 'Line';
  SMultipleLines = 'Lines';

  //=== JvTimeFrameworkReg.pas ===============================================
  SGridLayout = 'Grid Layout';
  SCustomDraw = 'Custom Draw';

  //=== JvTimerListEditor.pas ================================================
  SEventsEllipsis = 'Events...';

  //=== JvTreeItemsEditorForm.pas ============================================
  SLinksEditorEllipsis = 'Links Editor...';

  //=== JvValidatorsEditorForm.pas ===========================================
  SJvValidatorsItemsEditorEllipsis = 'JvValidators Items Editor...';
  SJvValidatorItemsEditorEllipsis = 'JvValidator Items Editor';

  //=== JvWizardEditorForm.pas ===============================================
  SPageListEllipsis = 'Page List...';
  SNewWelcomePage = 'New Welcome Page';
  SNewInteriorPage = 'New Interior Page';
  SDeletePage = 'Delete Page';

  //=== DataProvider design time constants ===================================
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

implementation

end.
