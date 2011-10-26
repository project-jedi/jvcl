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

unit JvVersionControlActionsEngine;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Graphics,
  Forms, Controls, Classes, JvActionsEngine;

type
  TjvVersionControlActionEngine = class(TJvActionBaseEngine)
  private
    FOnChangeActionComponent: TJvChangeActionComponent;
  protected
    function GetEngineList: TJvActionEngineList; virtual; abstract;
    property EngineList: TJvActionEngineList read GetEngineList;
  public
    constructor Create(AOwner: TComponent); override;
    function GetFilename(aActionComponent: TComponent): string; virtual;
    function SaveFile(aActionComponent: TComponent;const aFilename: string): Boolean; virtual;
    function NeedsSaveFile(aActionComponent: TComponent): Boolean; virtual;
    function SupportsAction(AAction: TJvActionEngineBaseAction): Boolean; override;
    function SupportsGetFileName(aActionComponent: TComponent): Boolean; virtual;
    function SupportsSaveFile(aActionComponent: TComponent): Boolean; virtual;
    function SupportsNeedsSaveFile(aActionComponent: TComponent): Boolean; virtual;
    property OnChangeActionComponent: TJvChangeActionComponent read FOnChangeActionComponent write FOnChangeActionComponent;
  end;

  TjvVersionControlActionEngineClass = class of TjvVersionControlActionEngine;
  TjvVersionControlActionEngineList = class(TJvActionEngineList)
  public
    procedure RegisterEngine(AEngineClass: TjvVersionControlActionEngineClass);
  end;

procedure RegisterVersionControlActionEngine(AEngineClass: TjvVersionControlActionEngineClass);

function RegisteredVersionControlActionEngineList: TjvVersionControlActionEngineList;

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
  SysUtils,
  Variants, Dialogs,
  JvVersionControlActions;


var
  IntRegisteredActionEngineList: TjvVersionControlActionEngineList;

procedure RegisterVersionControlActionEngine(AEngineClass: TjvVersionControlActionEngineClass);
begin
  if Assigned(IntRegisteredActionEngineList) then
    IntRegisteredActionEngineList.RegisterEngine(AEngineClass);
end;

function RegisteredVersionControlActionEngineList: TjvVersionControlActionEngineList;
begin
  Result := IntRegisteredActionEngineList;
end;

procedure CreateActionEngineList;
begin
  IntRegisteredActionEngineList := TjvVersionControlActionEngineList.Create;
end;

procedure DestroyActionEngineList;
begin
  IntRegisteredActionEngineList.Free;
  IntRegisteredActionEngineList := nil;
end;

procedure InitActionEngineList;
begin
  CreateActionEngineList;
end;

constructor TjvVersionControlActionEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TjvVersionControlActionEngine.GetFilename(aActionComponent:
    TComponent): string;
begin
  Result := '';
end;

function TjvVersionControlActionEngine.SaveFile(aActionComponent: TComponent;const aFilename: string): Boolean;
begin
  Result := True;
end;

function TjvVersionControlActionEngine.NeedsSaveFile(aActionComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TjvVersionControlActionEngine.SupportsAction(AAction: TJvActionEngineBaseAction): Boolean;
begin
  Result := (AAction is TJvVersionControlBaseAction) ;
end;

function TjvVersionControlActionEngine.SupportsGetFileName(aActionComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TjvVersionControlActionEngine.SupportsSaveFile(aActionComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TjvVersionControlActionEngine.SupportsNeedsSaveFile(aActionComponent: TComponent): Boolean;
begin
  Result := False;
end;


procedure TjvVersionControlActionEngineList.RegisterEngine(AEngineClass: TjvVersionControlActionEngineClass);
begin
  Add(AEngineClass.Create(nil));
end;


initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  InitActionEngineList;

finalization
  DestroyActionEngineList;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.