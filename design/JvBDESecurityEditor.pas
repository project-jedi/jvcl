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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBDESecurityEditor;

{$I jvcl.inc}

interface

uses
  Classes, SysUtils, DB, DBTables,
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  JvDBEditors, JvBDESecurity;

type
  { For TJvDBSecurity component }
  TJvUserTableNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TJvLoginNameFieldProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation

//=== { TJvUserTableNameProperty } ===========================================

procedure TJvUserTableNameProperty.GetValueList(List: TStrings);
var
  Security: TJvDBSecurity;
begin
  Security := GetComponent(0) as TJvDBSecurity;
  if Security.Database <> nil then
    Security.Database.Session.GetTableNames(Security.Database.DatabaseName,
      '*.*', True, False, List);
end;

//=== { TJvLoginNameFieldProperty } ==========================================

procedure TJvLoginNameFieldProperty.GetValueList(List: TStrings);
var
  Security: TJvDBSecurity;
  Table: TTable;
begin
  Security := GetComponent(0) as TJvDBSecurity;
  if (Security.Database <> nil) and (Security.UsersTableName <> '') then
  begin
    Table := TTable.Create(Security);
    try
      Table.DatabaseName := Security.Database.DatabaseName;
      Table.TableName := Security.UsersTableName;
      {$IFDEF COMPILER10_UP}
      {$WARN SYMBOL_DEPRECATED OFF}
      Table.GetFieldNames(List);
      {$WARN SYMBOL_DEPRECATED ON}
      {$ELSE}
      Table.GetFieldNames(List);
      {$ENDIF COMPILER10_UP}
    finally
      Table.Free;
    end;
  end;
end;

end.