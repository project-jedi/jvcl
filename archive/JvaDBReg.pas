{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDBReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a dott prygounkov att gmx dott de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : Register db-aware components

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvaDBReg;

interface


procedure Register;

implementation

uses
  Classes,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvDBEditors, JvBDEEditors,
  JvDBTreeView, JvDBLookupTreeView, JvDBMove, JvSQLS, JvxDConst;

{$R ..\resources\radb.dcr}

procedure Register;
const
  cKeyField = 'KeyField';
  cListField = 'ListField';
  cMasterField = 'MasterField';
  cDetailField = 'DetailField';
  cIconField = 'IconField';
  cItemField = 'ItemField';
  cStartMasterValue = 'StartMasterValue';
  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  cDatabase = 'Database';
  cUseFilter = 'UseFilter';
  {$ENDIF}
  {$ENDIF}
begin
  {RADBCt unit}
  RegisterComponents(srJvBDEPalette, [TJvaSQLScript]);
  { JvDBMove unit }
  RegisterComponents(srJvBDEPalette, [TJvDBMove]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Source', TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Destination', TJvDatabaseNameProperty);

  {JvDBTreeView unit}
  RegisterComponents(srJvDataControlsPalette, [TJvDBTreeView]);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cItemField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cMasterField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cDetailField, TJvDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBTreeView, cIconField, TJvDataFieldProperty);

  {JvDBLookupTreeView unit}
  RegisterComponents(srJvDataControlsPalette, [TJvDBLookupTreeView, TJvDBLookupTreeViewCombo]);
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

  {$IFDEF COMPILER6_UP}
  {$IFDEF VCL}
  RegisterPropertiesInCategory(cDatabase, TJvDBTreeView,
    [cItemField, cMasterField, cDetailField, cIconField, cStartMasterValue, cUseFilter]);
  RegisterPropertiesInCategory(cDatabase, TJvDBLookupTreeView,
    [cMasterField, cDetailField, cIconField, cStartMasterValue, cKeyField, cListField, cUseFilter]);
  RegisterPropertiesInCategory(cDatabase, TJvDBLookupTreeViewCombo,
    [cMasterField, cDetailField, cIconField, cStartMasterValue, cKeyField, cListField, cUseFilter]);
  {$ENDIF}
  {$ENDIF}
end;

end.

