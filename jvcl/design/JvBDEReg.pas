{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvBDEReg;

interface

procedure Register;

implementation

uses
  SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JvBDEEditors, JvConsts, 
  JvBdeUtils, JvBDEFilter, JvBDEIndex, JvBDELists, JvBDEMove,
  JvBDEProgress, JvBDEQBE, JvBDESecurity, JvBDEMemTable,
  JvBDEQuery, JvBDESQLScript,
  JvBDEMemTableEditor, JvBDESecurityEditor;


{$R ..\resources\JvBDEReg.dcr}

procedure Register;
begin
  RegisterComponents(SPaletteBDE, [TJvDBFilter, TJvDBIndexCombo,
    TJvDatabaseItems, TJvTableItems,
    // TJvDBListDataSet, TJvDatabaseList, TJvLangDrivList, TJvTableList,
    // TJvStoredProcList, TJvFieldList,  TJvIndexList,
    TJvDBMove, TJvDBProgress, TJvQBEQuery, TJvDBSecurity,
    TJvBDEMemoryTable, TJvQuery, TJvBDESQLScript, TJvSQLScript]);

  RegisterPropertyEditor(TypeInfo(TFileName), TJvCustomTableItems, 'TableName',
    TJvTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvDBSecurity, 'UsersTableName',
    TJvUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBSecurity, 'LoginNameField',
    TLoginNameFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, 'DatabaseName',
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomBDEItems, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBProgress, 'SessionName',
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Source',
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, 'Destination',
    TJvDatabaseNameProperty);

  RegisterComponentEditor(TJvBDEMemoryTable, TJvBDEMemoryTableEditor);
end;

end.

