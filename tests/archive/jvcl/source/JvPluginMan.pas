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

Contributor(s):
Ralf Steinhaeusser [ralfiii@gmx.net].
Gustavo Bianconi

Last Modified: 2003-05-01

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

 PluginManager loads Plugins

 changed 26.7.2001, by Ralf Steinhaeusser, Changes marked with !

 Events :
 When loading plugins (LoadPlugins) the events are called in the following order:
 FOnBeforeLoad(Sender, Name, CanLoad)
     Plugin -> Register
     Plugin.Initialize
   FOnNewCommand (times Nr. of commands)
 FOnAfterLoad

Versionhistory:

 BaseVersion 5 :
 V 11 : When loading packages -> except instead of finally -> //!11
        New event : OnErrorLoading
 V 10 : Now handles custom Plugins (only their destructors are called when unloading)
 V 09 : Pluginmanager : Extension automatically follows plugintype
        First version to share with "rest of the world"
 V 08 : Problems with $ImplicitBuild
 V 07 : fixed file-creation bug: linebreaks were done with #10#13 instead of
              the other way round, what caused the IDE-navigation do show
              erroneous behaviour
 V 06 : fixed Memory leak when loading of not supported DLL's is skipped
        inserted credits to About-box
 V 05 : started adding Package-functionality
        PluginManager : Loined 2 TLists to one,
        Record with info on Plugins introduced
        fixed buggy Instance-count check
        Added PluginKind-Property
        changed : PluginName also contains path
 V 04 : cleaned Plugin-Manager :
        Removed OnBefore- and OnAfterLoading (REALLY unnecessary - OnBeforeLoad,
                and OnAfterLoad are still here !)
        Removed Trigger-routines. Were only called once -> moved into code
 V 03 : removed unnecessary Set/Get-routines for most properties
 V 02 : new about-dialog, removed unnecessary CDK-auto-generated comments
        stupid fPluginHandles from TStringList -> TList
 V 01 : renamed objects, files, ressources
        fixed several Memory-leaks, fixed unload-bug, minimized uses-list
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPluginMan;

interface

uses
  Windows, SysUtils, Classes, Graphics,
  JvComponent, JvPlugin; // reduced to the min

const
  C_VersionString = '5.10';

type
  TNewCommandEvent = procedure(Sender: TObject; ACaption, AHint, AData: string;
    AShortCut: TShortCut; ABitmap: TBitmap;
    AEvent: TNotifyEvent) of object;
  // Bianconi
  // Removed
  // TJvNotifyStrEvent = procedure(Sender: TObject; S: string) of object;
  // End of Removed

  TJvBeforeLoadEvent = procedure(Sender: TObject; Filename: string; var AllowLoad: boolean) of object;
  TJvAfterLoadEvent = procedure(Sender: TObject; Filename: string;
    const ALibHandle: Thandle; var AllowLoad: Boolean) of object;
  TJvBeforeCommandsEvent = procedure(Sender: TObject; APlugIn: TJvPlugIn) of object;
  TJvAfterCommandsEvent = procedure(Sender: TObject; APlugIn: TJvPlugIn) of object;
  TJvPlgInErrorEvent = procedure(Sender: TObject; AError: Exception) of object;
  // End of Bianconi

  EJvPluginError = class(Exception);
  EJvLoadPluginError = class(EJvPluginError);
  // Bianconi
  EJvExtensionPlugInError = class(EJvPlugInError);
  EJvInitializationPlugInError = class(EJvPlugInError);
  EJvInitializationCustomPlugInError = class(EJvPlugInError);
  EJvCantRegisterPlugInError = class(EJvPlugInError);
  // End of Bianconi

  TPluginKind = (plgDLL, plgPackage, plgCustom);

  TPluginInfo = class(TObject)
  public
    PluginKind: TPluginKind;
    Handle: HINST;
    Plugin: TJvPlugin;
  end;

  TJvPluginManager = class(TJvComponent)
  private
    FExtension: string;
    FPluginFolder: string;
    FPluginKind: TPluginKind;
    FPluginInfos: TList;
    FOnBeforeLoad: TJvBeforeLoadEvent;
    FOnAfterLoad: TJvAfterLoadEvent;
    FOnNewCommand: TNewCommandEvent;

// Bianconi
    // Removed
    // FOnErrorLoading: TJvNotifyStrEvent;
    // End of removed
    fOnBeforeNewCommand: TJvBeforeCommandsEvent;
    fOnAfterNewCommand: TJvAfterCommandsEvent;
    FOnPlugInError: TJvPlgInErrorEvent;
// End of Bianconi

    procedure SetPluginKind(const Value: TPluginKind);
  protected
    procedure SetExtension(NewValue: string);
    function GetPlugin(Index: Integer): TJvPlugin;
    //    function GetVersion: string;
    function GetPluginCount: Integer;
    //    procedure SetVersion(newValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPlugin(FileName: string; PlgKind: TPluginKind);
    procedure LoadPlugins;
    procedure UnloadPlugin(Index: Integer);
    procedure GetLoadedPlugins(PluginList: TStrings);
    property Plugins[Index: Integer]: TJvPlugin read GetPlugin;
    property PluginCount: Integer read GetPluginCount;
    procedure SendMessage(PluginMessage: Longint; PluginParams: string);
    function AddCustomPlugin(Plugin: TJvPlugin): Boolean;
  published
    property PluginFolder: string read FPluginFolder write FPluginFolder;
    property Extension: string read FExtension write SetExtension;
    property PluginKind: TPluginKind read FPluginKind write SetPluginKind;
    //    property Version: string read GetVersion write SetVersion;
    property OnBeforeLoad: TJvBeforeLoadEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnNewCommand: TNewCommandEvent read FOnNewCommand write FOnNewCommand;
// Bianconi
    // Removed
    // property OnErrorLoading: TJvNotifyStrEvent read FOnErrorLoading write FOnErrorLoading;
    // End of removed
    property OnAfterLoad: TJvAfterLoadEvent read fOnAfterLoad write fOnAfterLoad;
    property OnBeforeNewCommand: TJvBeforeCommandsEvent read fOnBeforeNewCommand write fOnBeforeNewCommand;
    property OnAfterNewCommand: TJvAfterCommandsEvent read fOnAfterNewCommand write fOnAfterNewCommand;
    property OnPlugInError: TJvPlgInErrorEvent read FOnPlugInError write FOnPlugInError;
// End of Bianconi
  end;

implementation

uses
  {$IFNDEF COMPILER6_UP}
  JvFunctions, // for IncludeTrailingPathDelimiter (only <D6)
  {$ENDIF}
  Forms;

const
  C_REGISTER_PLUGIN = 'RegisterPlugin';
  C_Extensions: array[plgDLL..plgPackage] of PChar = ('dll', 'bpl');

constructor TJvPluginManager.Create(AOwner: TComponent);
begin
  try
    inherited Create(AOwner);
    FPluginInfos := TList.Create;
    FPluginKind := plgDLL;
    FExtension := C_Extensions[FPluginKind];
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPluginError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end; // on E : Exception
  end; // try .. except
end;

destructor TJvPluginManager.Destroy;
begin
  // Free the loaded plugins
  while FPluginInfos.Count > 0 do // !change as suggested in forum
    UnloadPlugin(0);
  FPluginInfos.Free;
  inherited Destroy;
end;

procedure TJvPluginManager.SetExtension(NewValue: string);
begin
  try
    if (FExtension <> NewValue) then
    begin
      // (rb) No reason to block this
      if {(Length(newValue) > 3) or} Length(NewValue) < 1 then
        raise Exception.Create('Extension may not be empty')
      else
        FExtension := NewValue;
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPluginError) then
       FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
end;

procedure TJvPluginManager.SetPluginKind(const Value: TPluginKind);
begin
  if FPluginKind <> Value then
  begin
    if FExtension = C_Extensions[FPluginKind] then
      FExtension := C_Extensions[Value];
    FPluginKind := Value;
  end;
end;

{function TJvPluginManager.GetVersion: string;
begin
  result := C_VersionString;
end;}

{procedure TJvPluginManager.SetVersion(newValue: string);
begin
end;}

function TJvPluginManager.GetPluginCount: Integer;
begin
  Result := FPluginInfos.Count;
end;

function TJvPluginManager.GetPlugin(Index: Integer): TJvPlugin;
var
  PlgI: TPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  Result := PlgI.Plugin;
end;

procedure TJvPluginManager.GetLoadedPlugins(PluginList: TStrings);
var
  J: Integer;
begin
  PlugInList.Clear;
  for J := 0 to FPluginInfos.Count - 1 do
    PluginList.Add(Plugins[J].Name);
end;

// Create and add plugin - if error occurs, the Plugin is not added to list

function TJvPluginManager.AddCustomPlugin(Plugin: TJvPlugin): Boolean;
var
  PlgInfo: TPluginInfo;
  Counter: Integer;
begin
  Result := false;
  try
    Result := Plugin.Initialize(Self, Application, 'CustomPlugin');
    if not Result then Exit;

    PlgInfo := TPluginInfo.create;
    PlgInfo.PluginKind := PlgCustom;
    PlgInfo.Plugin := Plugin;

    fPluginInfos.Add(PlgInfo);

    try
      if (Assigned(fOnBeforeNewCommand)) then
      begin
        fOnBeforeNewCommand(Self, PlugIn);
      end;

      // Events for all new commands
      if Assigned(fOnNewCommand) then
      begin
        for counter := 0 to Plugin.Commands.Count - 1 do
          with TJvPluginCommand(Plugin.Commands.Items[counter]) do
            fOnNewCommand(Self, Caption, Hint, Data, ShortCut, Bitmap, OnExecute);
      end;
    finally
      if (Assigned(fOnAfterNewCommand)) then
        fOnAfterNewCommand(Self, PlugIn);
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPluginError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end; // On E : Exception
  end; // try .. except
end;

// Load a Plugin - either DLL or package

procedure TJvPluginManager.LoadPlugin(FileName: string; PlgKind: TPluginKind);
type
  TSxRegisterPlugin = function: TJvPlugin; stdcall;
var
  Counter: Integer;
  LibHandle: Integer;
  RegisterProc: TSxRegisterPlugin;
  Plugin: TJvPlugin;
  NumCopies: Integer;
  PlgInfo: TPluginInfo;
  AllowLoad: Boolean;
begin
  LibHandle := 0;
  AllowLoad := true;
  if (assigned(fOnBeforeLoad)) then
    fOnBeforeLoad(Self, Filename, AllowLoad);

  if AllowLoad then
  begin
    try
      LibHandle := 0;
      Plugin := nil;
      case PlgKind of
        plgDLL: LibHandle := LoadLibrary(PChar(FileName));
        plgPackage: LibHandle := LoadPackage(FileName);
      end;

      if LibHandle = 0 then
        raise EJvLoadPluginError.Create(FileName);

      AllowLoad := True;
      if (Assigned(FOnAfterLoad)) then
      begin
        FOnAfterLoad(Self, FileName, LibHandle, AllowLoad);
      end;

      if not AllowLoad then
      begin
        case PluginKind of
          plgDLL: FreeLibrary(LibHandle);
          plgPackage: UnloadPackage(LibHandle);
        end;
        Exit;
      end; // if NOT AllowLoad

      // Load the registration procedure
      RegisterProc := GetProcAddress(LibHandle, C_REGISTER_PLUGIN);
      if not Assigned(RegisterProc) then
        raise EJvLoadPluginError.Create(FileName);

      // register the plugin
      Plugin := RegisterProc;
      if Plugin = nil then
        raise EJvCantRegisterPlugInError.Create(FileName);

      // make sure we don't load more copies of the plugin than allowed
      if Plugin.InstanceCount > 0 then // 0 = unlimited
      begin
        NumCopies := 0;
        for Counter := 0 to FPluginInfos.Count - 1 do
        begin
          if Plugins[Counter].PluginID = Plugin.PluginID then
            Inc(NumCopies);
        end;

        if NumCopies >= Plugin.InstanceCount then
        begin
          Plugin.Free;
          Exit; // Todo : Don't know what Skipload does here
        end;
      end; // if Plugin.InstanceCount > 0

      // initialize the plugin and add to list
      if AddCustomPlugin(Plugin) then
      begin
        PlgInfo := FPluginInfos.Last;
        PlgInfo.PluginKind := PlgKind;
        PlgInfo.Handle := LibHandle;
      end;
    except //!11    if - for whatever reason - an exception has occurred
      //            free Plugin and library
      // (rom) statements used twice could be wrapped in method
      on E: Exception do
      begin
        FreeAndNil(Plugin);
        if (LibHandle <> 0) then
        begin
          case PlgKind of
            plgDLL: FreeLibrary(LibHandle);
            plgPackage: UnloadPackage(LibHandle);
          end;
        end; // if( LibHandle <> 0 )
        if not (csDesigning in ComponentState) and Assigned(FOnPluginError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end; // On E : Exception
    end; // try .. except
  end; // if AllowLoad
end;

// Load all plugins in the plugin-folder
// exceptions can only be seen through the OnErrorLoading-Event

procedure TJvPluginManager.LoadPlugins;
var
  FileName: string;
  Found: Integer;
  Path: string;
  Sr: TSearchRec;
begin
  // if the PluginPath is blank, we load from the app's folder.
  if FPluginFolder = '' then
    Path := ExtractFilePath(Application.ExeName)
  else
    Path := FPluginFolder;

  Path := IncludeTrailingPathDelimiter(Path);

  try
    try
      Found := FindFirst(Path + '*.' + FExtension, 0, Sr);
      while Found = 0 do
      begin
        FileName := Sr.Name;
        try
        //! If one plugin made problems -> no other plugins where loaded
        //! To avoid that the try-except block was wrapped around here...
          LoadPlugin(Path + FileName, PluginKind);
        except
        end;
        Found := FindNext(Sr);
      end;
    except
      on E: Exception do
      begin
        if not (csDesigning in ComponentState) and Assigned(FOnPluginError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end; // On E : Exception
    end; // try .. except
  finally
    FindClose(Sr);
  end; // try .. finally
end;

procedure TJvPluginManager.UnloadPlugin(Index: Integer);
var
  PlgI: TPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  PlgI.Plugin.Free;
  case PlgI.PluginKind of
    plgDLL: FreeLibrary(PlgI.Handle);
    plgPackage: UnloadPackage(PlgI.Handle);
  end;

  PlgI.Free;
  FPluginInfos.Delete(Index);
end;

procedure TJvPluginManager.SendMessage(PluginMessage: Longint; PluginParams: string);
var
  J: Integer;
begin
  for J := 0 to FPluginInfos.Count - 1 do
    Plugins[J].SendPluginMessage(PluginMessage, PluginParams);
end;

end.

