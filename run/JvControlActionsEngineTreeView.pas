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

unit JvControlActionsEngineTreeView;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Graphics, ComCtrls,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  QWindows, QImgList, QGraphics, QComCtrls,
  {$ENDIF UNIX}
  Forms, Controls, Classes, JvControlActionsEngine;

type


  TJvControlActionTreeViewEngine = class(TJvControlActionEngine)
  private
  protected
    function GetSupportedOperations: TJvControlActionOperations; override;
  public
    function ExecuteOperation(const aOperation: TJvControlActionOperation; const
        aActionControl: TControl): Boolean; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
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
  SysUtils, Variants, Dialogs;


procedure InitActionEngineList;
begin
  RegisterControlActionEngine (TJvControlActionTreeViewEngine);
end;

function TJvControlActionTreeViewEngine.ExecuteOperation(const aOperation:
    TJvControlActionOperation; const aActionControl: TControl): Boolean;
begin
  Result := true;
  if Assigned(aActionControl) and (aActionControl is TCustomTreeView) then
    Case aOperation of
      caoCollapse : TCustomTreeView(aActionControl).FullCollapse;
      caoExpand : TCustomTreeView(aActionControl).FullExpand;
    else
      Result := false;
    End
  else
    Result := false;
end;

function TJvControlActionTreeViewEngine.GetSupportedOperations:
    TJvControlActionOperations;
begin
  Result := [caoCollapse, caoExpand];
end;

function TJvControlActionTreeViewEngine.SupportsComponent(aActionComponent:
    TComponent): Boolean;
begin
  Result := aActionComponent is TCustomTreeView;
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
