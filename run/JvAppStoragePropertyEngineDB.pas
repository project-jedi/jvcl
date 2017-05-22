{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppStoragePropertyEngineDB.pas, released on 2005-01-13.

The Initial Developer of the Original Code is Jens Fudickar
Portions created by Jens Fudickar are Copyright (C) 2005 Jens Fudickar
All Rights Reserved.

Contributor(s):
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Description:

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppStoragePropertyEngineDB;

{$I jvcl.inc}

interface

{$IFDEF UNITVERSIONING}
uses
  JclUnitVersioning;
{$ENDIF UNITVERSIONING}

procedure RegisterAppStoragePropertyEngines;

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
  Classes, DBGrids,
  JvJCLUtils, JvAppStorage;

type
  TJvAppStoragePropertyDBGridColumnsEngine = class(TJvAppStoragePropertyBaseEngine)
  public
    function Supports(AObject: TObject; AProperty: TObject): Boolean; override;
    procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const Recursive,
      ClearFirst: Boolean; const IgnoreProperties: TStrings = nil); override;
    procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject: TObject; AProperty: TObject; const
      Recursive: Boolean; const IgnoreProperties: TStrings = nil); override;
  end;

//=== { TJvAppStoragePropertyDBGridColumnsEngine } ===========================

function TJvAppStoragePropertyDBGridColumnsEngine.Supports(AObject: TObject; AProperty: TObject): Boolean;
begin
  Result := Assigned(AProperty) and (AProperty is TDBGridColumns);
end;

type
  TAccessCustomDBGrid = class(TCustomDBGrid);

procedure TJvAppStoragePropertyDBGridColumnsEngine.ReadProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject:
  TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean; const IgnoreProperties: TStrings = nil);
begin
  if Assigned(AObject) and (AObject is TCustomDBGrid) then
    TAccessCustomDBGrid(AObject).BeginLayout;
  try
    if Assigned(AProperty) and (AProperty is TDBGridColumns) then
      AStorage.ReadCollection(APath, TCollection(AProperty), ClearFirst);
  finally
    if Assigned(AObject) and (AObject is TCustomDBGrid) then
      TAccessCustomDBGrid(AObject).EndLayout;
  end;
end;

procedure TJvAppStoragePropertyDBGridColumnsEngine.WriteProperty(AStorage: TJvCustomAppStorage; const APath: string; AObject:
  TObject; AProperty: TObject; const Recursive: Boolean; const IgnoreProperties: TStrings = nil);
begin
  if Assigned(AProperty) and (AProperty is TDBGridColumns) then
    AStorage.WriteCollection(APath, TCollection(AProperty));
end;

//=== Global =================================================================

procedure RegisterAppStoragePropertyEngines;
begin
  RegisterAppStoragePropertyEngine(TJvAppStoragePropertyDBGridColumnsEngine);
end;

procedure UnregisterAppStoragePropertyEngines;
begin
  UnregisterAppStoragePropertyEngine(TJvAppStoragePropertyDBGridColumnsEngine);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  RegisterAppStoragePropertyEngines;

finalization
  UnregisterAppStoragePropertyEngines;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
