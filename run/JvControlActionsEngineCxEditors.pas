{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBActions.Pas, released on 2007-03-11.

The Initial Developer of the Original Code is Jens Fudickar [jens dott fudicker  att oratool dott de]
Portions created by Jens Fudickar are Copyright (C) 2002 Jens Fudickar.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvControlActionsEngineCxEditors;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Forms, Controls, Classes,
  {$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
  cxTreeView,
  {$ENDIF}
  JvControlActionsEngine;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
type
  TJvControlActioncxTreeViewEngine = class(TJvControlActionEngine)
  protected
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const aActionControl: TControl): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
  end;
{$ENDIF}

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
  SysUtils, Grids, TypInfo, StrUtils, Variants, Dialogs, StdCtrls, Clipbrd;

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
procedure InitActionEngineList;
begin
  RegisterControlActionEngine (TJvControlActioncxTreeViewEngine);
end;

function TJvControlActioncxTreeViewEngine.ExecuteOperation(const aOperation: TJvControlActionOperation; const
    aActionControl: TControl): Boolean;
begin
  Result := true;
  if Assigned(aActionControl) and (aActionControl is TcxCustomTreeView) then
    Case aOperation of
      caoCollapse : TcxCustomTreeView(aActionControl).FullCollapse;
      caoExpand : TcxCustomTreeView(aActionControl).FullExpand;
    else
      Result := false;
    End
  else
    Result := false;
end;

function TJvControlActioncxTreeViewEngine.GetSupportedOperations:
    TJvControlActionOperations;
begin
  Result := [caoCollapse, caoExpand];
end;

function TJvControlActioncxTreeViewEngine.SupportsComponent(aActionComponent:
    TComponent): Boolean;
begin
  Result := aActionComponent is TcxCustomTreeView;
end;
{$ENDIF}

{$IFDEF USE_3RDPARTY_DEVEXPRESS_CXEDITOR}
initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
{$ENDIF}

end.
