{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandsReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvBDEEditors;

interface

uses
  Classes,
  JvDBEditors;

type
  TJvDatabaseNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  TJvSessionNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  { For TJvFieldList, TJvIndexList components }
  TJvTableNameProperty = class(TJvDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

implementation

uses
  DB, DBTables;
  
//=== { TJvDatabaseNameProperty } ============================================

procedure TJvDatabaseNameProperty.GetValueList(List: TStrings);
begin
  if (GetComponent(0) is TDBDataSet) then
    (GetComponent(0) as TDBDataSet).DBSession.GetDatabaseNames(List)
  else
  if Session <> nil then
    Session.GetDatabaseNames(List);
end;

//=== { TJvSessionNameProperty } =============================================

procedure TJvSessionNameProperty.GetValueList(List: TStrings);
begin
  Sessions.GetSessionNames(List);
end;

//=== { TJvTableNameProperty } ===============================================

procedure TJvTableNameProperty.GetValueList(List: TStrings);
begin
  (GetComponent(0) as TDBDataSet).DBSession.GetTableNames((GetComponent(0)
    as TDBDataSet).DatabaseName, '', True, False, List);
end;

end.
