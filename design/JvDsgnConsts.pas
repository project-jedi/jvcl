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

resourcestring
  //=== strings used by several editors ======================================
  SPreview = 'Preview...';
  SDesigner = 'Designer...';
  SNone = '(none)';
  SHelp = 'Help';
  SConfirm = 'Confirm?';
  SAllFilesFilter = 'All Files (*.*)|*.*';

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
  SEditProperty = 'Notifications...';

  //=== JvCheckedItemsForm.pas ===============================================
  SItemEditor = 'Item editor';
  SEnabled = 'Enabled';

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
  SsFieldNameIsNotAValidIdentifier = '%s: Field name is not a valid identifier';
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
  STreeDesigner = 'Tree designer...';
  SContextManager = 'Context manager...';
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
  SFmtEditProperty = '%s Editor...';

  //=== JvFooterEditor.pas ===================================================
  SAddButtonText = 'Add button';
  SMSOffice = 'MS Office 2000';
  SMSEnterpriseManagerWizard = 'MS Enterprise Manager Wizard';
  SDialogMode = 'Dialog Mode';
  SPrevious = 'Previous';
  SNext = 'Next';

  //=== JvgComponentListEditorForm.pas =======================================
  SEditComponentList = 'Edit component list...';

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
  SEditComponent = 'Edit component...';

  //=== JvgReportEditorForm.pas ==============================================
  SEditReport = 'Edit report...';
  SPreviewReport = 'Preview report...';
  SDeleteObject = 'Delete object?';
  SPagePreview = 'Page Preview';

  //=== JvgReportParamsForm.pas ==============================================
  SEditParams = 'Edit params...';

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
  SPreviewText = 'Preview';

  //=== JvJVCLAboutEditor.pas ================================================
  SVersions = 'Version %s';

  //=== JvLookoutEditor.pas ==================================================
  SAddPage = 'Add page';
  SActivate = 'Activate';
  SAddButton = 'Add Button';
  SScrollUp = 'Scroll Up';
  SScrollDown = 'Scroll Down';
  SAddPage_ = 'Add Page';
  SNextPage = 'Next Page';
  SPreviousPage = 'Previous Page';

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
