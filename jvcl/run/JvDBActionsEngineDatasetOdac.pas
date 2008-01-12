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
    function GetSQL(AActionComponent : TComponent): string; override;
    procedure RefreshRecord(AActionComponent : TComponent); override;
    function SupportsComponent(AActionComponent : TComponent): Boolean; override;
    function SupportsGetSQL(AActionComponent : TComponent): Boolean; override;
    function SupportsRefreshRecord(AActionComponent : TComponent): Boolean;
        override;
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

{$IFDEF USE_3RDPARTY_CORELAB_ODAC}

uses
  DBAccess;

function TJvDatabaseActionOdacDatasetEngine.GetSQL(AActionComponent :
    TComponent): string;
var Dataset : TCustomDADataSet;
    i : integer;
begin
  if GetDataset(AActionComponent) is TCustomDADataSet then
  Begin
    Dataset := TCustomDADataSet(GetDataset(AActionComponent));
    Result := TCustomDADataSet(GetDataset(AActionComponent)).FinalSQL;
    if Dataset.ParamCount > 0 then
      Result := Result + #13#10+#13#10+'Bind Variables : ';
    for i := 0 to Dataset.ParamCount - 1 do
      Result := Result + #13#10+Dataset.Params[i].Name+' : "'+Dataset.Params[i].AsString+'"';
  end;
end;

procedure TJvDatabaseActionOdacDatasetEngine.RefreshRecord(AActionComponent :
    TComponent);
begin
  inherited;
  if GetDataset(AActionComponent) is TCustomDADataSet then
    TCustomDADataSet(GetDataset(AActionComponent)).RefreshRecord;
end;

function TJvDatabaseActionOdacDatasetEngine.SupportsComponent(AActionComponent
    : TComponent): Boolean;
begin
  Result := (GetDataset(AActionComponent) is TCustomDADataSet);
end;

function TJvDatabaseActionOdacDatasetEngine.SupportsGetSQL(AActionComponent :
    TComponent): Boolean;
begin
  Result := True;
end;

function TJvDatabaseActionOdacDatasetEngine.SupportsRefreshRecord(
    AActionComponent : TComponent): Boolean;
begin
  Result := True;
end;

{$ENDIF USE_3RDPARTY_CORELAB_ODAC}

procedure InitActionEngineList;
begin
  {$IFDEF USE_3RDPARTY_CORELAB_ODAC}
  RegisterDatabaseActionEngine(TJvDatabaseActionOdacDatasetEngine);
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

