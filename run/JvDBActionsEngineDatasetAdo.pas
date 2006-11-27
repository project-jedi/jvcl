{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineDatasetAdo.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngineDatasetAdo;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, DB,
  JvDBActionsEngine;

type
  TJvDatabaseActionAdoDatasetEngine = class(TJvDatabaseActionBaseDatasetEngine)
  public
    function GetSQL: string; override;
    function Supports(ADataComponent: TComponent): Boolean; override;
    function SupportsGetSQL: Boolean; override;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  AdoDb;

function TJvDatabaseActionAdoDatasetEngine.GetSQL: string;
begin
  if Dataset is TADOQuery then
    Result := TADOQuery(Dataset).SQL.Text
  else
  if Dataset is TAdoTable then
    Result := 'SELECT * FROM ' + TADOTable(Dataset).TableName;
end;

function TJvDatabaseActionAdoDatasetEngine.Supports(ADataComponent: TComponent): Boolean;
begin
  Result := (ADataComponent is TADOQuery) or (ADataComponent is TADOTable);
end;

function TJvDatabaseActionAdoDatasetEngine.SupportsGetSQL: Boolean;
begin
  Result := Assigned(Dataset);
end;

procedure InitActionEngineList;
begin
  RegisterActionEngine(TJvDatabaseActionAdoDatasetEngine);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

