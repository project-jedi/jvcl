{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvAppStorage.pas, released on --.

The Initial Developer of the Original Code is Marcel Bestebroer
Portions created by Marcel Bestebroer are Copyright (C) 2002 - 2003 Marcel
Bestebroer
All Rights Reserved.

Contributor(s):
  Jens Fudickar
  Olivier Sannier

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description:
  General storage unit - provides with a basic storage backend component to store application
  specific data. Descendants can provide specific backends for registry, INI-files, DB, XML,
  etc. Should be used to provide a common interface for storing data as is done in some of
  the JVCL components (eg. JvFormPlacement/JvFormStorage).

  This was requested in one of the comments of the JVCL 3.0 Survey Results.

  Paths
  =====
  Paths are relative to the current path. Paths are specified using backslashes (\) between
  individual folders and the value. Paths starting with a backslash are always relative to the root
  storage (application specific root, absolute root path).

  Dots (.) are used to reference parent folders with the following rules:
  * a single dot (.) refers to the current folder
  * each additional dot moves up a level in the folder hierarchie, ie. "....\Here" refers to a
    folder three levels up from the current where a sub folder/value name "Here" is searched. Of
    course the normal (OS path) specification can be used as well ("..\..\..\Here" would be the
    same as the first example).

  Multiple backslashes without names between them are ignored ("Root\\Here" is the same as
  "Root\Here").

  Storage hierarchies
  ===================
  Each storage allows you add an unlimited number of sub storages. A sub storage is a symbolic
  link between a path in a storage to another storage (which in turn can also provide sub storages).

  Suppose you want to store both generic as well as user specific settings. This can be accomplished
  with two stores, one for the generic settings and one specific for the current user. The generic
  store (referred to as 'asRegBackend' from now on) will link to the user specific store (referred
  to as 'asUserIniBackend' from now on) using asRegBackend.SubStorages. The RootPath for the
  asUserIniBackend sub-store link will be set to 'UserSettings'. From that point on, any reference
  to a sub path of '\UserSettings' from the asRegBackend storage will be handed over to the
  asUserIniBackend storage. Examples:

  Path                          Target
  ====                          ======
  \WinPath                      asRegBackend:'\WinPath'
  \Generic\UserSettings\Me      asRegBackend:'\Generic\UserSettings\Me'
  \UserSettings                 asRegBackend:'\UserSettings'
  \UserSettings\FirstName       asUserIniBackend:'\FirstName'
  \UserSettings\Sub1\Sub1.1     asUserIniBackend:'\Sub1\Sub1.1'

  Because all settings can be read from a single store (from the application's perspective) you have
  created the option to keep your settings storage and retrieval code simple and easy to understand.
  Upon startup you can set asUserIniBackend to the correct INI file for the user that has logged on,
  and you are ready to read in the settings of that user.

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvAppStoragePropertyEngineDB;

{$I jvcl.inc}

interface

procedure RegisterAppStoragePropertyEngines;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes, DBGrids,
  JvAppStorage;

type
  TJvAppStoragePropertyDBGridColumnsEngine = class(TJvAppStoragePropertyBaseEngine)
  public
    function Supports(AObject: TObject; AProperty: TObject): Boolean; override;
    procedure ReadProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean); override;
    procedure WriteProperty(AStorage: TJvCustomAppStorage; const APath: string;
      AObject: TObject; AProperty: TObject; const Recursive: Boolean); override;
  end;

//=== { TJvAppStoragePropertyDBGridColumnsEngine } ===========================

function TJvAppStoragePropertyDBGridColumnsEngine.Supports(AObject: TObject; AProperty: TObject): Boolean;
begin
  Result := Assigned(AProperty) and (AProperty is TDBGridColumns);
end;

type
  TAccessCustomDBGrid = class(TCustomDBGrid);

procedure TJvAppStoragePropertyDBGridColumnsEngine.ReadProperty(AStorage: TJvCustomAppStorage;
  const APath: string; AObject: TObject; AProperty: TObject; const Recursive, ClearFirst: Boolean);
begin
  if Assigned(AObject) and (AObject is TCustomDBGrid) then
    TAccessCustomDBGrid(AObject).BeginLayout;
  try
    if Assigned(AProperty) and (AProperty is TDBGridColumns) then
      AStorage.ReadCollection(APath, TCollection(AProperty), ClearFirst);
  finally
    if Assigned(AObject) and (AObject is TDBGrid) then
      TAccessCustomDBGrid(AObject).EndLayout;
  end;
end;

procedure TJvAppStoragePropertyDBGridColumnsEngine.WriteProperty(AStorage: TJvCustomAppStorage;
  const APath: string; AObject: TObject; AProperty: TObject; const Recursive: Boolean);
begin
  if Assigned(AProperty) and (AProperty is TCustomDBGrid) then
    AStorage.WriteCollection(APath, TCollection(AProperty));
end;

//=== Global =================================================================

procedure RegisterAppStoragePropertyEngines;
begin
  RegisterAppStoragePropertyEngine(TJvAppStoragePropertyDBGridColumnsEngine);
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  RegisterAppStoragePropertyEngines;

{$IFDEF UNITVERSIONING}
finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

