{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uilPluginMan.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [tim@uil.net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s): Ralf Steinhaeusser [ralfiii@gmx.net].

Last Modified: 2002-09-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

 PluginManager loads Plugins

 detailed Versionhistory see JvPlugCommon .pas

 changed 26.7.2001, by Ralf Steinhaeusser, Changes marked with !

 Events :
 When loading plugins (LoadPlugins) the events are called in the following order:
 fOnBeforeLoad(Sender, Name, CanLoad)
     Plugin -> Register
     Plugin.Initialize
   fOnNewCommand (times Nr. of commands)
 fOnAfterLoad
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPluginMan;

interface

uses JvPlugin,
  Windows, SysUtils, Classes, Graphics, JvComponent; // reduced to the min

type
  TNewCommandEvent = procedure(Sender: TObject; ACaption, AHint, AData: string;
    ABitmap: TBitmap; AEvent: TNotifyEvent) of object;

  // Exceptions
type
  EJvPluginError = class(Exception);
  EJvLoadPluginError = class(EJvPluginError);

  // Event Types
type
  TJvBeforeLoadEvent = procedure(Sender: TObject; Filename: string; var AllowLoad: boolean) of object;
  TJvAfterLoadEvent = procedure(Sender: TObject; Filename: string) of object;

type
  TPluginKind = (plgDLL, plgPackage, plgCustom);

type
  TPluginInfo = class(TObject)
  public
    PluginKind: TPluginKind;
    Handle: HINST;
    Plugin: TJvPlugin;
  end;

type
  // TJvPluginManager definition
  TJvPluginManager = class(TJvComponent)
  private
    // Private declarations
    fExtension: string;
    fPluginFolder: string;
    fPluginKind: TPluginKind;

    fPluginInfos: TList;
    fOnBeforeLoad: TJvBeforeLoadEvent;
    fOnAfterLoad: TJvAfterLoadEvent;
    fOnNewCommand: TNewCommandEvent;
    procedure SetPluginKind(const Value: TPluginKind);
  protected
    // Protected declarations
    procedure SetExtension(newValue: string);
    function GetPlugin(index: integer): TJvPlugin;
//    function GetVersion: string;
    function GetPluginCount: integer;
//    procedure SetVersion(newValue: string);
  public
    // Public declarations
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPlugin(Filename: string; PlgKind: TPluginKind);
    procedure LoadPlugins;
    procedure UnloadPlugin(index: integer);
    procedure GetLoadedPlugins(PluginList: TStrings);
    property Plugins[index: integer]: TJvPlugin read GetPlugin;
    property PluginCount: integer read GetPluginCount;
    procedure SendMessage(PluginMessage: longint; PluginParams: string);
    procedure AddCustomPlugin(Plugin: TJvPlugin);
  published
    // Published properties and events
    property PluginFolder: string read fPluginFolder write fPluginFolder;
    property Extension: string read fExtension write SetExtension;
    property PluginKind: TPluginKind read fPluginKind write SetPluginKind;
//    property Version: string read GetVersion write SetVersion;

    // Events
    property OnBeforeLoad: TJvBeforeLoadEvent read fOnBeforeLoad write fOnBeforeLoad;
    property OnAfterLoad: TJvAfterLoadEvent read fOnAfterLoad write fOnAfterLoad;
    property OnNewCommand: TNewCommandEvent read fOnNewCommand write fOnNewCommand;
  end; // TJvPluginManager

implementation

uses JvPlugCommon,
  Forms;

const
  C_Extensions: array[plgDLL..plgPackage] of string = ('dll', 'bpl');

  // ###################################
  // ######   Create, Free ...
  // ###################################

  // initializes properties

constructor TJvPluginManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPluginInfos := TList.Create;
  fPluginKind := plgDLL;
  fExtension := C_Extensions[fPluginKind];
end;

destructor TJvPluginManager.Destroy;
begin
  // Free the loaded plugins
  while fPluginInfos.Count > 0 do // !change as suggested in forum
    UnloadPlugin(0);

  fPluginInfos.Free;

  inherited Destroy;
end;

// ###################################
// ######  setting of properties
// ###################################

procedure TJvPluginManager.SetExtension(newValue: string);
// Sets data member FExtension to newValue.
begin
  if (fExtension <> newValue) then
  begin
    if (length(newValue) > 3) or (length(newValue) < 1) then
      raise Exception.Create('Extension must be between 1 and 3 characters')
    else
      fExtension := newValue;
  end;
end;

procedure TJvPluginManager.SetPluginKind(const Value: TPluginKind);
begin
  if fPluginKind = Value then
    Exit;

  if fExtension = C_Extensions[fPluginKind] then
    fExtension := C_Extensions[Value];

  fPluginKind := Value;
end;

{function TJvPluginManager.GetVersion: string;
begin
  result := C_VersionString;
end;}

{procedure TJvPluginManager.SetVersion(newValue: string);
begin
end;}

function TJvPluginManager.GetPluginCount: integer;
begin
  Result := fPluginInfos.Count;
end;

// ###################################
// ######   Pluin in/out
// ###################################

// Returns plugin with given index

function TJvPluginManager.GetPlugin(index: integer): TJvPlugin;
var
  PlgI: TPluginInfo;
begin
  PlgI := fPluginInfos.Items[index];
  result := PlgI.Plugin;
end;

// Get a list of the names of all loaded Plugins

procedure TJvPluginManager.GetLoadedPlugins(PluginList: TStrings);
var
  j: integer;
begin
  PlugInList.Clear;
  for j := 0 to fPluginInfos.Count - 1 do
    PluginList.Add(Plugins[j].Name);
end;

// ###################################
// ######  Loading plugins
// ###################################

procedure TJvPluginManager.AddCustomPlugin(Plugin: TJvPlugin);
var
  PlgInfo: TPluginInfo;
  counter: integer;
begin
  if not Plugin.Initialize(Self, Application, 'CustomPlugin') then
    raise EJvLoadPluginError.Create('CustomPlugin : failed initialization.');

  PlgInfo := TPluginInfo.create;
  PlgInfo.PluginKind := PlgCustom;
  PlgInfo.Plugin := Plugin;

  fPluginInfos.Add(PlgInfo);

  // Events for all new commands
  if assigned(fOnNewCommand) then
  begin
    for counter := 0 to Plugin.Commands.Count - 1 do
      with TJvPluginCommand(Plugin.Commands.Items[counter]) do
        fOnNewCommand(Self, Caption, Hint, Data, Bitmap, OnExecute);
  end;
end;

// Load a Plugin - either DLL or package

procedure TJvPluginManager.LoadPlugin(Filename: string; PlgKind: TPluginKind);
type
  TSxRegisterPlugin = function: TJvPlugin; stdcall;
var
  counter: integer;
  LibHandle: integer;
  RegisterProc: TSxRegisterPlugin;
  Plugin: TJvPlugin;
  NumCopies: integer;
  PlgInfo: TPluginInfo;
begin
  LibHandle := 0;
  case PlgKind of
    plgDLL: LibHandle := LoadLibrary(Pchar(FileName));
    plgPackage: LibHandle := LoadPackage(FileName);
  end;

  if LibHandle = 0 then
    raise EJvLoadPluginError.Create('Error loading Plug-in "' + Filename + '"');

  try
    // Load the registration procedure
    RegisterProc := GetProcAddress(LibHandle, C_REGISTER_PLUGIN);
    if not Assigned(RegisterProc) then
      raise EJvLoadPluginError.Create('"' + Filename + '" is not a valid Plug-in. Export-function not found');

    // register the plugin
    Plugin := RegisterProc;
    if Plugin = nil then
      Exit;

    // make sure we don't load more copies of the plugin than allowed
    if Plugin.InstanceCount > 0 then // 0 = unlimited
    begin
      NumCopies := 0;
      for counter := 0 to fPluginInfos.Count - 1 do
      begin
        if Plugins[counter].PluginID = Plugin.PluginID then
          Inc(NumCopies);
      end;

      if NumCopies >= Plugin.InstanceCount then
        Exit; // Todo : Don't know what Skipload does here
    end;

    // initialize the plugin
    if not Plugin.Initialize(Self, Application, Filename) then
      raise EJvLoadPluginError.Create('"' + Filename + '" failed initialization.');

    PlgInfo := TPluginInfo.create;
    PlgInfo.PluginKind := PlgKind;
    PlgInfo.Plugin := Plugin;
    PlgInfo.Handle := LibHandle;
    LibHandle := 0;

    fPluginInfos.Add(PlgInfo);

    // Events for all new commands
    if assigned(fOnNewCommand) then
    begin
      for counter := 0 to Plugin.Commands.Count - 1 do
        with TJvPluginCommand(Plugin.Commands.Items[counter]) do
          fOnNewCommand(Self, Caption, Hint, Data, Bitmap, OnExecute);
    end;
  finally
    // if - for whatever reason - method has been ended - free library
    if LibHandle <> 0 then
      case PlgKind of
        plgDLL: FreeLibrary(LibHandle);
        plgPackage: UnloadPackage(LibHandle);
      end;
  end;
end;

// Load all plugins in the plugin-folder

procedure TJvPluginManager.LoadPlugins;
var
  AllowLoad: boolean;
  Filename: string;
  Found: Integer;
  path: string;
  sr: TSearchRec;
begin
  // if the PluginPath is blank, we load from the app's folder.
  if FPluginFolder = '' then
    path := ExtractFilePath(Application.Exename)
  else
    path := FPluginFolder;

  if path[length(path)] <> '\' then
    path := path + '\';

  try
    Found := FindFirst(path + '*.' + FExtension, 0, sr);
    while Found = 0 do
    begin
      Filename := sr.Name;
      AllowLoad := true;

      if (assigned(fOnBeforeLoad)) then
        fOnBeforeLoad(Self, Filename, AllowLoad);

      if AllowLoad then
      begin
        try //! If one plugin made problems -> no other plugins where loaded
          //! To avoid that the try-except block was wrapped around here...
          LoadPlugin(Path + Filename, PluginKind);

          if (assigned(fOnAfterLoad)) then
            fOnAfterLoad(Self, Filename);
        except
        end;
      end;
      Found := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;
end; // LoadPlugins

// ###################################
// ######  Unloading plugins
// ###################################

procedure TJvPluginManager.UnloadPlugin(index: integer);
var
  PlgI: TPluginInfo;
begin
  PlgI := fPluginInfos.Items[index];
  PlgI.Plugin.Free;

  case PlgI.PluginKind of
    plgDLL: FreeLibrary(PlgI.Handle);
    plgPackage: UnloadPackage(PlgI.Handle);
  end;

  PlgI.Free;
  fPluginInfos.Delete(index);
end;

// ###################################
// ######
// ###################################

procedure TJvPluginManager.SendMessage(PluginMessage: longint; PluginParams: string);
var
  j: integer;
begin
  for j := 0 to fPluginInfos.Count - 1 do
    Plugins[j].SendPluginMessage(PluginMessage, PluginParams);
end;

end.

