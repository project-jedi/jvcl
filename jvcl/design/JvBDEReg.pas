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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDEReg;

{$I jvcl.inc}

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
  JvDsgnConsts,
  JvBDEEditors, JvBdeUtils, JvBDEFilter, JvBDEIndex, JvBDELists, JvBDEMove,
  JvBDEProgress, JvBDEQBE, JvBDESecurity, JvBDEMemTable, JvBDEQuery,
  JvBDESQLScript, JvBDEMemTableEditor, JvBDESecurityEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvBDEReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvBDEReg.dcr}
{$ENDIF UNIX}

procedure Register;
const
  cTableName = 'TableName';
  cUsersTableName = 'UsersTableName';
  cLoginNameField = 'LoginNameField';
  cDatabaseName = 'DatabaseName';
  cSessionName = 'SessionName';
  cSource = 'Source';
  cDestination = 'Destination';
begin
  RegisterComponents(RsPaletteBDE, [TJvDBFilter, TJvDBIndexCombo,
    TJvDatabaseItems, TJvBDEItems, TJvTableItems, TJvDBListDataSet, 
    // TJvDatabaseList, TJvLangDrivList, TJvTableList,
    // TJvStoredProcList, TJvFieldList,  TJvIndexList,
    TJvDBMove, TJvDBProgress, TJvQBEQuery, TJvDBSecurity,
    TJvBDEMemoryTable, TJvQuery, TJvBDESQLScript, TJvSQLScript]);

  RegisterPropertyEditor(TypeInfo(TFileName), TJvCustomTableItems, cTableName,
    TJvTableNameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TJvDBSecurity, cUsersTableName,
    TJvUserTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBSecurity, cLoginNameField,
    TLoginNameFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, cDatabaseName,
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvCustomBDEItems, cSessionName,
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvBDESQLScript, cSessionName,
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBProgress, cSessionName,
    TJvSessionNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, cSource,
    TJvDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvDBMove, cDestination,
    TJvDatabaseNameProperty);

  RegisterComponentEditor(TJvBDEMemoryTable, TJvBDEMemoryTableEditor);
end;

end.

