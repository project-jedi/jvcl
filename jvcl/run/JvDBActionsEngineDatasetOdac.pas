{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActionsEngineDatasetOdac.Pas, released on 2004-12-30.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngineDatasetOdac;

{$I jvcl.inc}

interface

uses
{$IFDEF UNITVERSIONING}
  JclUnitVersioning,
{$ENDIF UNITVERSIONING}
  Classes, DB,
  JvDBActionsEngine;



{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
type
  TJvDatabaseActionOdacDatasetEngine = class(TJvDatabaseActionBaseDatasetEngine)
  public
    function GetSQL: string; override;
    function Supports(ADataComponent: TComponent): boolean; override;
    function SupportsGetSQL: Boolean; override;
  end;
{$ENDIF USE_3RDPARTY_CORELAB_ODAC}


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
{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
  DBAccess;
{$ENDIF USE_3RDPARTY_CORELAB_ODAC}



{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
function TJvDatabaseActionOdacDatasetEngine.GetSQL: string;
begin
  Result := TCustomDADataSet(Dataset).SQL.Text;
end;

function TJvDatabaseActionOdacDatasetEngine.Supports(ADataComponent:
    TComponent): boolean;
begin
  Result := (ADataComponent is TCustomDADataSet);
end;

function TJvDatabaseActionOdacDatasetEngine.SupportsGetSQL: Boolean;
begin
  Result := Assigned(Dataset);
end;


{$ENDIF USE_3RDPARTY_CORELAB_ODAC}


procedure InitActionEngineList;
begin
{$IFDEF USE_3RDPARTY_CORELAB_ODAC}
  RegisterActionEngine(TJvDatabaseActionOdacDatasetEngine);
{$ENDIF USE_3RDPARTY_CORELAB_ODAC}
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

