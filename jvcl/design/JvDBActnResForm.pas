{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDialogActnResForm.PAS, released on 2004-04-01.

The Initial Developers of the Original Code are:
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActnResForm;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, StdActns, ActnList, ImgList, Controls, Forms,
  JvActions, JvDBActions, JvActionsEngine;

type
  TJvDBActions = class(TDataModule)
    ActionList1: TActionList;
    JvDatabaseInsertAction1: TJvDatabaseInsertAction;
    JvDatabaseCopyAction1: TJvDatabaseCopyAction;
    JvDatabaseEditAction1: TJvDatabaseEditAction;
    JvDatabaseDeleteAction1: TJvDatabaseDeleteAction;
    JvDatabasePostAction1: TJvDatabasePostAction;
    JvDatabaseCancelAction1: TJvDatabaseCancelAction;
    JvDatabaseSingleRecordWindowAction1: TJvDatabaseSingleRecordWindowAction;
    JvDatabaseFirstAction1: TJvDatabaseFirstAction;
    JvDatabaseLastAction1: TJvDatabaseLastAction;
    JvDatabaseNextAction1: TJvDatabaseNextAction;
    JvDatabasePriorAction1: TJvDatabasePriorAction;
    JvDatabaseNextBlockAction1: TJvDatabaseNextBlockAction;
    JvDatabasePriorBlockAction1: TJvDatabasePriorBlockAction;
    JvDatabasePositionAction1: TJvDatabasePositionAction;
    JvDatabaseRefreshAction1: TJvDatabaseRefreshAction;
    JvDatabaseSimpleAction1: TJvDatabaseSimpleAction;
    JvDatabaseOpenAction1: TJvDatabaseOpenAction;
    JvDatabaseCloseAction1: TJvDatabaseCloseAction;
    JvDatabaseModifyAllAction1: TJvDatabaseModifyAllAction;
    JvDatabaseShowSQLStatementAction1: TJvDatabaseShowSQLStatementAction;
  public
  end;

implementation

{$R *.dfm}

end.