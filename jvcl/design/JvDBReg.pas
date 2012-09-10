{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  {$IFDEF HAS_UNIT_SYSTEM_ACTIONS}
  System.Actions,
  {$ENDIF HAS_UNIT_SYSTEM_ACTIONS}
  Classes, ComCtrls, ActnList,
  DesignEditors, DesignIntf,
  JvDsgnConsts,
  {$IFDEF JV_MIDAS}
  JvDBRemoteLogin,
  {$ENDIF JV_MIDAS}
  {$IFDEF USE_3RDPARTY_DOA}
  JvOracleDataset,
  JvDBPasswordDialogDoa,
  JvDBLogonDialogDoa,
  {$ENDIF USE_3RDPARTY_DOA}
  {$IFDEF USE_3RDPARTY_DEVART_ODAC} 
  JvDBPasswordDialogOdac,
  JvDBLogonDialogOdac,
  JvOdacSmartQuery,
  {$ENDIF USE_3RDPARTY_DEVART_ODAC}
  {$IFDEF USE_3RDPARTY_DEVART_UNIDAC} 
  JvDBLogonDialogUniDac,
  JvUniDacQuery,
  {$ENDIF USE_3RDPARTY_DEVART_UNIDAC}
  JvADOQuery,
  JvMemoryDataset, JvDBDatePickerEdit, JvDBDateTimePicker, JvDBLookupTreeView,
  JvDBProgressBar, JvDBRichEdit, JvDBSpinEdit, JvDBTreeView, JvDBLookup,
  JvCsvData, JvDBCombobox, JvDBControls, JvDBGrid, JvDBUltimGrid, JvDBGridFooter,
  JvDBRadioPanel, JvDBGridExport, JvDBLookupComboEdit, JvDBHTLabel, JvDBSearchEdit,
  JvDBSearchComboBox, JvAppDBStorage, JvDBFindEdit, JvDBImage, JvDBEditors,
  JvDBMemDatasetEditor, JvDBGridExportEditors, JvDBGridEditors, JvCsvDataEditor,
  JvDBActionsEngine, JvDBActions, JvDBCheckBox,
  JvDBActnResForm, JvDataSource, JvDataSourceIntf,
  JvDynControlEngineVCL // This prevents runtime error messages about not initialized JvDynControlEngine
  ;

{$R JvDBReg.dcr}

procedure Register;
const
  cDataField = 'DataField';
  cKeyField = 'KeyField';
  cListField = 'ListField';
  cDisplayField = 'DisplayField';
  cListKeyField = 'ListKeyField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
  cLookupField = 'LookupField';
  cSectionField = 'SectionField';
  cValueField = 'ValueField';
  cEditControls = 'EditControls';
  cSortedField = 'SortedField';
  cSortMarker = 'SortMarker';
  cPanels = 'Panels';
begin
  RegisterComponents(RsPaletteDBNonVisual, [TJvDataSource, TJvMemoryData,
    TJvCsvDataSet {$IFDEF JV_MIDAS}, TJvDBRemoteLogin {$ENDIF},
    {$IFDEF USE_3RDPARTY_DOA} 
    TJvOracleDataset, TJvDBDoaLogonDialog, TJvDBDoaPasswordDialog,
    {$ENDIF USE_3RDPARTY_DOA}
    {$IFDEF USE_3RDPARTY_DEVART_ODAC} 
    TJvDBOdacConnectDialog, TJvDBOdacPasswordDialog, TjvOdacSmartQuery,
    TjvOdacOraTable, TjvOdacOraQuery,
    {$ENDIF USE_3RDPARTY_DEVART_ODAC}
    {$IFDEF USE_3RDPARTY_DEVART_UNIDAC} 
    TJvDBUniDacConnectDialog, 
    TjvUniDacUniTable, TjvUniDacUniQuery,
    {$ENDIF USE_3RDPARTY_DEVART_UNIDAC}
    TJvADOQuery, TJvADODataSet,
    TJvDBGridWordExport, TJvDBGridExcelExport, TJvDBGridHTMLExport,
    TJvDBGridCSVExport, TJvDBGridXMLExport, TJvDatabaseActionList]);
  RegisterComponents(RsPaletteDBVisual, [TJvDBDatePickerEdit,
    TJvDBDateTimePicker, TJvDBProgressBar, TJvDBRichEdit, TJvDBSpinEdit,
    TJvDBLookupList, TJvDBLookupCombo, TJvDBLookupEdit, TJvDBRadioPanel,
    TJvDBCombobox, TJvDBTreeView, TJvDBLookupTreeViewCombo, TJvDBLookupTreeView,
    TJvDBGrid, TJvDBUltimGrid, TJvDBGridFooter, TJvDBComboEdit, TJvDBDateEdit, TJvDBCalcEdit,
    TJvDBMaskEdit, TJvDBStatusLabel, TJvDBLookupComboEdit, TJvDBHTLabel,
    TJvDBSearchEdit, TJvDBSearchComboBox, TJvDBFindEdit, TJvDBImage, TJvDBNavigator,
    TJvDBCheckBox]);
  RegisterComponents(RsPalettePersistence, [TJvAppDBStorage]);

  RegisterPropertyEditor(TypeInfo(TDataFieldString), TJvDataConnector, cDataField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(TDataFieldString), TJvDataConnector, cKeyField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(TDataFieldString), TJvDataConnector, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(TDataFieldString), TJvDataConnector, cListKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(TDataFieldString), TJvDataConnector, cLookupField, TJvLookupSourceProperty);

  RegisterPropertyEditor(TypeInfo(string), TJvDBComboBoxListSettings, cKeyField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBComboBoxListSettings, cDisplayField, TJvDataFieldProperty);

  RegisterPropertyEditor(TypeInfo(string), TJvLookupControl, cLookupField, TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupEdit, cLookupField, TJvLookupSourceProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cDetailField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cIconField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeViewCombo, cIconField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cKeyField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cListField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cMasterField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cDetailField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupTreeView, cIconField, TJvListFieldProperty);
  RegisterPropertyEditor(TypeInfo(TJvWordGridFormat), TJvDBGridWordExport, '', TJvDBGridExportWordFormatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cSectionField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cKeyField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cValueField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCsvDataSet, 'CsvFieldDef', TJvCsvDefStrProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCsvDataSet, 'FileName', TJvFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TJvDBGridControls), TJvDBGrid, cEditControls, TJvDBGridControlsProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBUltimGrid, cSortedField, nil);
  RegisterPropertyEditor(TypeInfo(TSortMarker), TJvDBUltimGrid, cSortMarker, nil);
  RegisterPropertyEditor(TypeInfo(TStatusPanels), TJvDBGridFooter, cPanels, nil);

  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);

  RegisterComponentEditor(TJvCsvDataSet, TJvCSVDataSetComponentEditor);


  RegisterActions(RsJVCLDBActionsCategory, [TJvDatabaseFirstAction,
    TJvDatabaseLastAction, TJvDatabaseNextAction, TJvDatabasePriorAction,
    TJvDatabaseNextBlockAction, TJvDatabasePriorBlockAction,
    TJvDatabasePositionAction, TJvDatabaseRefreshAction, TJvDatabaseRefreshRecordAction,
    TJvDatabaseInsertAction, TJvDatabaseCopyAction, TJvDatabaseEditAction,
    TJvDatabaseDeleteAction, TJvDatabasePostAction, TJvDatabaseCancelAction,
    TJvDatabaseSingleRecordWindowAction, TJvDatabaseSimpleAction,
    {$IFDEF USE_3RDPARTY_SMEXPORT}
    TJvDatabaseSMExportAction,
    {$ENDIF USE_3RDPARTY_SMEXPORT}
    {$IFDEF USE_3RDPARTY_SMIMPORT}
    TJvDatabaseSMImportAction,
    {$ENDIF USE_3RDPARTY_SMIMPORT}
    TJvDatabaseOpenAction, TJvDatabaseCloseAction, TJvDatabaseModifyAllAction,
    TJvDatabaseShowSQLStatementAction], TJvDBActions);
end;

end.