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

  //=== JvAVICaptureEditors.pas ==============================================
  SDisconnected = 'Disconnected';
  SdIsNotWithinTheValidRangeOfdd = '%d is not within the valid range of %d..%d';

  //=== JvBandObjectDLLWizard.pas ============================================
  SCreatesABandObjectDLLProject = 'Creates a Band Object DLL Project.';
  SBandObjectDLLWizard = 'Band Object DLL Wizard';

  //=== JvBandObjectDLLWizardForm.pas ========================================
  SBandNameHasToBeAValidIdentifier = 'Band name has to be a valid identifier!';
  SPleaseEnterBandDescription = 'Please enter band description!';

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

  //=== JvDataConsumerContextSelectForm.pas ==================================
  SConsumerDoesNotSupportContextSelect = 'Consumer does not support context selection.';
  SIJvDataConsumerProviderIsNotSupported = 'IJvDataConsumerProvider is not supported by the specified consumer.';

  //=== JvDataConsumerItemSelectForm.pas =====================================
  SDataProviderItemSelector = 'DataProvider Item Selector';

  //=== JvDataContextManagerForm.pas =========================================
  SDataProviderContextManager = 'DataProvider Context Manager';

  //=== JvDataEmbeddedEditor.pas =============================================
  SAllFilesFilter = 'All Files (*.*)|*.*';

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
  SAllFiles = 'All files (*.*)|*.*';
  SStripFilePath = '&Strip file path';
  SExecutableFilesExeExeAllFiles = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*';
  SItems = 'Items';
  SFmtEditProperty = '%s Editor...';

  //=== JvFooterEditor.pas ===================================================
  SAddButton = 'Add button';
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
