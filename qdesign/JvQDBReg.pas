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

unit JvQDBReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes, QComCtrls, QControls,
  DesignEditors, DesignIntf, 
  JvQDsgnConsts,
  {$IFDEF JV_MIDAS}
  JvQDBRemoteLogin,
  {$ENDIF JV_MIDAS}
  JvQDBHTLabel, JvQDBProgressBar, JvQMemoryDataset,
  JvQCsvData, JvQCsvDataEditor, JvQAppDBStorage, JvQDBFindEdit;
  (*
  JvQDBDatePickerEdit, JvQDBDateTimePicker, JvQDBLookupTreeView,
  JvDBSpinEdit, JvQDBTreeView, JvQDBLookup,

  JvQDBCombobox, JvQDBControls, JvQDBGrid, JvQDBUltimGrid, JvQDBGridFooter,
  JvQDBRadioPanel, JvQDBGridExport, JvQDBLookupComboEdit, JvQDBSearchEdit,
  JvQDBSearchComboBox, JvQDBImage, JvQDBEditors,
  JvQDBGridExportEditors, JvQDBGridEditors;
  *)

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvDBReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvDBReg.dcr}
{$ENDIF UNIX}

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
  cEditControls = 'EditControls';
  cSortedField = 'SortedField';
  cSortMarker = 'SortMarker';
  cPanels = 'Panels';
begin
  GroupDescendentsWith(TJvMemoryData, TControl);
  GroupDescendentsWith(TJvCSVDataSet, TControl);
  GroupDescendentsWith(TJvCustomCSVDataSet, TControl);
  RegisterComponents(RsPaletteDBNonVisual, [TJvMemoryData,
    TJvCSVDataSet {$IFDEF JV_MIDAS}, TJvDBRemoteLogin {$ENDIF}]);

//    TJvDBGridWordExport, TJvDBGridExcelExport, TJvDBGridHTMLExport,
//    TJvDBGridCSVExport, TJvDBGridXMLExport, );
  RegisterComponents(RsPaletteDBVisual, [TJvDBProgressBar, TJvDBFindEdit,
    TJvDBHTLabel]);
(*
    TJvDBRichEdit, TJvDBSpinEdit,
    TJvDBLookupList, TJvDBLookupCombo, TJvDBLookupEdit, TJvDBRadioPanel,
    TJvDBCombobox, TJvDBTreeView, TJvDBLookupTreeViewCombo, TJvDBLookupTreeView,
    TJvDBGrid, TJvDBUltimGrid, TJvDBGridFooter, TJvDBComboEdit, TJvDBDateEdit, TJvDBCalcEdit,
    TJvDBMaskEdit, TJvDBStatusLabel, TJvDBLookupComboEdit,
    TJvDBSearchEdit, TJvDBSearchComboBox,  TJvDBImage]);
*)
  RegisterComponents(RsPalettePersistence, [TJvAppDBStorage]);
  (*
//  RegisterPropertyEditor(TypeInfo(string), TJvLookupControl, cLookupField, TJvLookupSourceProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvDBLookupEdit, cLookupField, TJvLookupSourceProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TJvDataFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TJvDataFieldProperty);
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
  *)
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cSectionField, TJvDataFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cKeyField, TJvDataFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cValueField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCsvDataSet, 'CsvFieldDef', TJvCsvDefStrProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCsvDataSet, 'FileName', TJvFilenameProperty);
//  RegisterPropertyEditor(TypeInfo(TJvDBGridControls), TJvDBGrid, cEditControls, TJvDBGridControlsEditor);
//  RegisterPropertyEditor(TypeInfo(string), TJvDBUltimGrid, cSortedField, nil);
//  RegisterPropertyEditor(TypeInfo(TSortMarker), TJvDBUltimGrid, cSortMarker, nil);
//  RegisterPropertyEditor(TypeInfo(TStatusPanels), TJvDBGridFooter, cPanels, nil);
//  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);
end;

end.
