{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uilPluginMan.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [tim att uil dott net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s):
Ralf Steinhaeusser [ralfiii att gmx dott net].
Gustavo Bianconi
Steefan Lesage - converted to use new OTA


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
 V 03 : removed unecessary Set/Get-routines for most properties
 V 02 : new about-dialog, removed unnecessary CDK-auto-generated comments
        stupid fPluginHandles from TStringList -> TList
 V 01 : renamed objects, files, ressources
        fixed several Memory-leaks, fixed unload-bug, minimized uses-list
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPluginManager;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, // (ahuser) do not move to VCL
  {$ENDIF MSWINDOWS}
  {$IFDEF VCL}
  Graphics, Forms,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Types, QWindows, QGraphics, QForms,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
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

  TJvBeforeLoadEvent = procedure(Sender: TObject; FileName: string; var AllowLoad: Boolean) of object;
  TJvAfterLoadEvent = procedure(Sender: TObject; FileName: string;
    const ALibHandle: THandle; var AllowLoad: Boolean) of object;
  TJvBeforeCommandsEvent = procedure(Sender: TObject; APlugIn: TJvPlugIn) of object;
  TJvAfterCommandsEvent = procedure(Sender: TObject; APlugIn: TJvPlugIn) of object;
  TJvPlgInErrorEvent = procedure(Sender: TObject; AError: Exception) of object;
  // End of Bianconi

  EJvPlugInError = class(Exception);
  EJvLoadPluginError = class(EJvPlugInError);
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
    PlugIn: TJvPlugIn;
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
    FOnBeforeNewCommand: TJvBeforeCommandsEvent;
    FOnAfterNewCommand: TJvAfterCommandsEvent;
    FOnPlugInError: TJvPlgInErrorEvent;
    // End of Bianconi
    procedure SetPluginKind(const Value: TPluginKind);
    procedure UnloadLibrary(Kind: TPluginKind; LibHandle: Integer);
  protected
    procedure SetExtension(NewValue: string);
    function GetPlugin(Index: Integer): TJvPlugIn;
    // function GetVersion: string;
    function GetPluginCount: Integer;
    // procedure SetVersion(newValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadPlugin(FileName: string; PlgKind: TPluginKind);
    procedure LoadPlugins;
    procedure UnloadPlugin(Index: Integer);
    procedure GetLoadedPlugins(PlugInList: TStrings);
    property Plugins[Index: Integer]: TJvPlugIn read GetPlugin;
    property PluginCount: Integer read GetPluginCount;
    procedure SendMessage(PluginMessage: Longint; PluginParams: string);
    function AddCustomPlugin(PlugIn: TJvPlugIn): Boolean;
  published
    property PluginFolder: string read FPluginFolder write FPluginFolder;
    property Extension: string read FExtension write SetExtension;
    property PluginKind: TPluginKind read FPluginKind write SetPluginKind;
    // property Version: string read GetVersion write SetVersion;
    property OnBeforeLoad: TJvBeforeLoadEvent read FOnBeforeLoad write FOnBeforeLoad;
    property OnNewCommand: TNewCommandEvent read FOnNewCommand write FOnNewCommand;
    // Bianconi
    // Removed
    // property OnErrorLoading: TJvNotifyStrEvent read FOnErrorLoading write FOnErrorLoading;
    // End of removed
    property OnAfterLoad: TJvAfterLoadEvent read FOnAfterLoad write FOnAfterLoad;
    property OnBeforeNewCommand: TJvBeforeCommandsEvent read FOnBeforeNewCommand write FOnBeforeNewCommand;
    property OnAfterNewCommand: TJvAfterCommandsEvent read FOnAfterNewCommand write FOnAfterNewCommand;
    property OnPlugInError: TJvPlgInErrorEvent read FOnPlugInError write FOnPlugInError;
    // End of Bianconi
  end;

implementation

uses
  {$IFNDEF COMPILER6_UP}
  JvJCLUtils, // for IncludeTrailingPathDelimiter (only <D6)
  {$ENDIF COMPILER6_UP}
  JvResources;

const
  C_REGISTER_PLUGIN = 'RegisterPlugin';
  C_Extensions: array [plgDLL..plgCustom] of PChar = ('dll', 'bpl','xxx');

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
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
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
    if FExtension <> NewValue then
    begin
      // (rb) No reason to block this
      if {(Length(newValue) > 3) or} Length(NewValue) < 1 then
        raise EJvPlugInError.CreateRes(@RsEErrEmptyExt)
      else
        FExtension := NewValue;
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
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

function TJvPluginManager.GetPlugin(Index: Integer): TJvPlugIn;
var
  PlgI: TPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  Result := PlgI.PlugIn;
end;

procedure TJvPluginManager.GetLoadedPlugins(PlugInList: TStrings);
var
  I: Integer;
begin
  PlugInList.BeginUpdate;
  try
    PlugInList.Clear;
    for I := 0 to FPluginInfos.Count - 1 do
      PlugInList.Add(Plugins[I].Name);
  finally
    PlugInList.EndUpdate;
  end;
end;

// Create and add plugin - if error occurs, the Plugin is not added to list

function TJvPluginManager.AddCustomPlugin(PlugIn: TJvPlugIn): Boolean;
var
  PlgInfo: TPluginInfo;
  Counter: Integer;
begin
  Result := False;
  try
    Result := PlugIn.Initialize(Self, Application, 'CustomPlugin');
    if not Result then
      Exit;

    PlgInfo := TPluginInfo.Create;
    PlgInfo.PluginKind := plgCustom;
    PlgInfo.PlugIn := PlugIn;

    FPluginInfos.Add(PlgInfo);

    try
      if Assigned(FOnBeforeNewCommand) then
        FOnBeforeNewCommand(Self, PlugIn);

      // Events for all new commands
      if Assigned(FOnNewCommand) then
        for Counter := 0 to PlugIn.Commands.Count - 1 do
          with TJvPluginCommand(PlugIn.Commands.Items[Counter]) do
            FOnNewCommand(Self, Caption, Hint, Data, ShortCut, Bitmap, OnExecute);
    finally
      if Assigned(FOnAfterNewCommand) then
        FOnAfterNewCommand(Self, PlugIn);
    end;
  except
    on E: Exception do
    begin
      if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
        FOnPlugInError(Self, E)
      else
        raise;
    end;
  end;
end;

// Load a Plugin - either DLL or package

procedure TJvPluginManager.LoadPlugin(FileName: string; PlgKind: TPluginKind);
type
  TSxRegisterPlugin = function: TJvPlugIn; stdcall;
var
  Counter: Integer;
  LibHandle: Integer;
  RegisterProc: TSxRegisterPlugin;
  PlugIn: TJvPlugIn;
  NumCopies: Integer;
  PlgInfo: TPluginInfo;
  AllowLoad: Boolean;
begin
  LibHandle := 0;
  AllowLoad := True;
  if Assigned(FOnBeforeLoad) then
    FOnBeforeLoad(Self, FileName, AllowLoad);

  if AllowLoad then
  begin
    try
      LibHandle := 0;
      PlugIn := nil;
      case PlgKind of
        plgDLL:
          LibHandle := LoadLibrary(PChar(FileName));
        plgPackage:
          LibHandle := LoadPackage(FileName);
      end;

      if LibHandle = 0 then
        raise EJvLoadPluginError.CreateResFmt(@RsEPluginPackageNotFound, [FileName]);

      AllowLoad := True;
      if Assigned(FOnAfterLoad) then
        FOnAfterLoad(Self, FileName, LibHandle, AllowLoad);

      if not AllowLoad then
      begin
        UnloadLibrary(PluginKind, LibHandle);
        Exit;
      end;

      // Load the registration procedure
      RegisterProc := GetProcAddress(LibHandle, C_REGISTER_PLUGIN);
      if not Assigned(RegisterProc) then
        raise EJvLoadPluginError.CreateResFmt(@RsERegisterPluginNotFound, [C_REGISTER_PLUGIN, FileName]);

      // register the plugin
      PlugIn := RegisterProc;
      if PlugIn = nil then
        raise EJvCantRegisterPlugInError.CreateResFmt(@RsERegisterPluginFailed, [C_REGISTER_PLUGIN, FileName]);

      // make sure we don't load more copies of the plugin than allowed
      if PlugIn.InstanceCount > 0 then // 0 = unlimited
      begin
        NumCopies := 0;
        for Counter := 0 to FPluginInfos.Count - 1 do
          if Plugins[Counter].PluginID = PlugIn.PluginID then
            Inc(NumCopies);

        if NumCopies >= PlugIn.InstanceCount then
        begin
          PlugIn.Free;
          Exit; // Todo : Don't know what Skipload does here
        end;
      end; // if Plugin.InstanceCount > 0

      // initialize the plugin and add to list
      if AddCustomPlugin(PlugIn) then
      begin
        PlgInfo := FPluginInfos.Last;
        PlgInfo.PluginKind := PlgKind;
        PlgInfo.Handle := LibHandle;
      end;
    except
      //!11    if - for whatever reason - an exception has occurred
      //            free Plugin and library
      // (rom) statements used twice could be wrapped in method
      on E: Exception do
      begin
        FreeAndNil(PlugIn);
        if LibHandle <> 0 then
          UnLoadLibrary(PlgKind, LibHandle);
        if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end;
    end;
  end;
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
        //! If one plugin made problems -> no other plugins where loaded
        //! To avoid that the try-except block was wrapped around here...
        try
          LoadPlugin(Path + FileName, PluginKind);
        except
        end;
        Found := FindNext(Sr);
      end;
    except
      on E: Exception do
      begin
        if not (csDesigning in ComponentState) and Assigned(FOnPlugInError) then
          FOnPlugInError(Self, E)
        else
          raise;
      end;
    end;
  finally
    FindClose(Sr);
  end;
end;

procedure TJvPluginManager.UnloadPlugin(Index: Integer);
var
  PlgI: TPluginInfo;
begin
  PlgI := FPluginInfos.Items[Index];
  PlgI.PlugIn.Free;
  UnloadLibrary(PlgI.PluginKind, PlgI.Handle);
  PlgI.Free;
  FPluginInfos.Delete(Index);
end;

procedure TJvPluginManager.SendMessage(PluginMessage: Longint; PluginParams: string);
var
  I: Integer;
begin
  for I := 0 to FPluginInfos.Count - 1 do
    Plugins[I].SendPluginMessage(PluginMessage, PluginParams);
end;

procedure TJvPluginManager.UnloadLibrary(Kind: TPluginKind;
  LibHandle: Integer);
begin
  case Kind of
    plgDLL:
      FreeLibrary(LibHandle);
    plgPackage:
      UnloadPackage(LibHandle);
  end;
end;

end.

