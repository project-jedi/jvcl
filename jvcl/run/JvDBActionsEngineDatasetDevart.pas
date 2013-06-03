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
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvDBActionsEngineDatasetDevart;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, DB,
  JvDBActionsEngine;

{$IFDEF USE_3RDPARTY_DEVART_DAC}
type
  TJvDatabaseActionDevartDatasetEngine = class(TJvDatabaseActionBaseDatasetEngine)
  public
    function GetSQL(AActionComponent : TComponent): string; override;
    procedure RefreshRecord(AActionComponent : TComponent); override;
    function SupportsComponent(AActionComponent : TComponent): Boolean; override;
    function SupportsGetSQL(AActionComponent : TComponent): Boolean; override;
    function SupportsRefreshRecord(AActionComponent : TComponent): Boolean; override;
  end;
{$ENDIF USE_3RDPARTY_DEVART_DAC}

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

{$IFDEF USE_3RDPARTY_DEVART_DAC}

uses
  DBAccess, SysUtils, Variants;

function TJvDatabaseActionDevartDatasetEngine.GetSQL(AActionComponent :
    TComponent): string;
var Dataset : TCustomDADataSet;
    s : String;
    i : integer;
begin
  if GetDataset(AActionComponent) is TCustomDADataSet then
  Begin
    Dataset := TCustomDADataSet(GetDataset(AActionComponent));
    Result := TCustomDADataSet(GetDataset(AActionComponent)).FinalSQL;
    if Dataset.ParamCount > 0 then
      Result := Result + #13#10+#13#10+'Bind Variables : ';
    for i := 0 to Dataset.ParamCount - 1 do
    begin
      Result := Result + #13#10' :'+Dataset.Params[i].Name+' : ';
      if Dataset.Params[i].isNull then
        Result := Result + 'NULL'
      else
      begin
        Result := Result +''''+Dataset.Params[i].AsString+'''';
        case Dataset.Params[i].DataType of
          ftDate,
          ftDateTime,
          ftTimeStamp,
          ftOraTimeStamp :
          begin
            DateTimeToString(s, 'dd.mm.yyyy hh:nn:ss', Dataset.Params[i].AsDateTime);
            Result := Result + ' - TO_DATE('''+s+''', ''DD.MM.YYYY HH24:MI:SS'')';
          end;
        end;
      end;
    end;
  end;
end;

procedure TJvDatabaseActionDevartDatasetEngine.RefreshRecord(AActionComponent :
    TComponent);
begin
  inherited;
  if GetDataset(AActionComponent) is TCustomDADataSet then
    TCustomDADataSet(GetDataset(AActionComponent)).RefreshRecord;
end;

function TJvDatabaseActionDevartDatasetEngine.SupportsComponent(AActionComponent
    : TComponent): Boolean;
begin
  Result := (GetDataset(AActionComponent) is TCustomDADataSet);
end;

function TJvDatabaseActionDevartDatasetEngine.SupportsGetSQL(AActionComponent :
    TComponent): Boolean;
begin
  Result := True;
end;

function TJvDatabaseActionDevartDatasetEngine.SupportsRefreshRecord(AActionComponent : TComponent): Boolean;
begin
  Result := True;
end;

{$ENDIF USE_3RDPARTY_DEVART_DAC}

procedure InitActionEngineList;
begin
  {$IFDEF USE_3RDPARTY_DEVART_DAC}
  RegisterDatabaseActionEngine(TJvDatabaseActionDevartDatasetEngine);
  {$ENDIF USE_3RDPARTY_DEVART_DAC}
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
