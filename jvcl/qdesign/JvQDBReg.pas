{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvQDBReg;

interface

procedure Register;

implementation

uses
  Classes, 
  DesignEditors, DesignIntf, 
  JvQDsgnConsts,
  JvQMemoryDataset, JvQDBDatePickerEdit, JvQDBDateTimePicker, JvQDBLookupTreeView,
  JvQDBProgressBar, JvQDBRichEdit, JvQDBSpinEdit, JvQDBTreeView, JvQDBLookup,
  JvQCsvData, JvQDBCombobox, JvQDBControls, JvQDBGrid, JvQDBRadioPanel,
  JvQDBGridExport, JvQDBLookupComboEdit, JvQDBHTLabel, JvQDBSearchEdit,
  JvQDBSearchComboBox, JvQAppDBStorage, JvQDBFindEdit, JvQDBImage,
  {$IFDEF JV_MIDAS}
  JvQDBRemoteLogin,
  {$ENDIF JV_MIDAS}
  JvQDBEditors, JvQDBMemDatasetEditor, JvQDBGridExportEditors;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDBReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvDBReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  cKeyField = 'KeyField';
  cListField = 'ListField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
  cLookupField = 'LookupField';
  cSectionField = 'SectionField';
  cValueField = 'ValueField';
  cRowsHeight = 'RowsHeight';
  //cStartMasterValue = 'StartMasterValue';
begin
  RegisterComponents(RsPaletteDBNonVisual, [TJvMemoryData,
    TJvCSVDataSet {$IFDEF JV_MIDAS}, TJvDBRemoteLogin {$ENDIF},
    TJvDBGridWordExport, TJvDBGridExcelExport, TJvDBGridHTMLExport, TJvDBGridCSVExport, TJvDBGridXMLExport]);
  RegisterComponents(RsPaletteDBVisual, [TJvDBDatePickerEdit,
    TJvDBDateTimePicker, TJvDBProgressBar, TJvDBRichEdit, TJvDBSpinEdit,
    TJvDBLookupList, TJvDBLookupCombo, TJvDBLookupEdit, TJvDBRadioPanel,
    TJvDBCombobox, TJvDBTreeView, TJvDBLookupTreeViewCombo, TJvDBLookupTreeView,
    TJvDBGrid, TJvDBComboEdit, TJvDBDateEdit, TJvDBCalcEdit, TJvDBMaskEdit,
    TJvDBStatusLabel, TJvDBLookupComboEdit, TJvDBHTLabel, TJvDBSearchEdit,
    TJvDBSearchComboBox, TJvDBFindEdit, TJvDBImage]);
  RegisterComponents(RsPalettePersistence, [TJvAppDBStorage]);

  RegisterPropertyEditor(TypeInfo(Integer), TJvDBGrid, cRowsHeight, nil);
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
  RegisterPropertyEditor(TypeInfo(TWordGridFormat), TJvDBGridWordExport, '', TDBGridExportWordFormatProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cSectionField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cKeyField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cValueField, TJvDataFieldProperty);

  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);
end;

end.

