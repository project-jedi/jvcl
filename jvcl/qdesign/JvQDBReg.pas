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
  DesignEditors, DesignIntf, QControls,
  JvQDsgnConsts,
  JvQMemoryDataset,  JvQCsvData, JvQAppDBStorage; //, JvQDBMemDatasetEditor;
  //JvQDBDatePickerEdit, JvQDBDateTimePicker, JvQDBLookupTreeView,
//  JvQDBProgressBar, JvQDBRichEdit, JvQDBSpinEdit, JvQDBTreeView, JvQDBLookup,

//  JvQDBCombobox, JvQDBControls, JvQDBGrid, JvQDBRadioPanel,
//  JvQDBGridExport, JvQDBLookupComboEdit, JvQDBHTLabel, JvQDBSearchEdit,
//  JvQDBSearchComboBox,

  //JvQDBFindEdit, JvQDBImage,
  {$IFDEF JV_MIDAS}
//  JvQDBRemoteLogin,
  {$ENDIF JV_MIDAS}
//  JvQDBEditors, JvQDBGridExportEditors;

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
  GroupDescendentsWith(TJvMemoryData, TControl);
  GroupDescendentsWith(TJvAppDBStorage, TControl);

  RegisterComponents(RsPaletteDBNonVisual, [TJvMemoryData,
    TJvCSVDataSet ]);
  RegisterComponents(RsPalettePersistence, [TJvAppDBStorage]);

//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cSectionField, TJvDataFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cKeyField, TJvDataFieldProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvCustomAppDBStorage, cValueField, TJvDataFieldProperty);

//  RegisterComponentEditor(TJvMemoryData, TJvMemDataSetEditor);
end;

end.

