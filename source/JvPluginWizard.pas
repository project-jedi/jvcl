{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPluginWizard.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [tim@uil.net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s): Ralf Steinhaeusser [ralfiii@gmx.net].

Last Modified: 2002-09-02

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

 Todo : I don't know why but IDE does not not navigate correctly here...
 Todo : Creates code with #10#13 instead of the other way ound (#13#10 is correct)

 Versionhistory :

 V 09 : inserted TYPE-decaration, IDE navigation works now
        rewrote ProjectCreate (100 times), seems to work now except a AV when
        creating a DLL-project. May have to do with ressources
 V 08 : Setting the pluginname works fine after A LOT of trying...
 V 07 : #10#13 -> #13#10 corrected
 V 06 : Wizard-Form added, lets select Plugin-Type and Object-name
 V 05 : uses-list minimized

 -----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvPluginWizard;

interface

uses
  Windows, ExptIntf, EditIntf,
  ToolsApi; // minimized list !

type
  TJvPluginWizard = class(TIExpert) // found in exptintf.pas
  public
    function GetIDString: string; override; // overridden abstract methods
    function GetStyle: TExpertStyle; override;
    function GetState: TExpertState; override;
    function GetMenuText: string; override;
    function GetPage: string; override;
    function GetName: string; override;
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetGlyph: HICON; override;
    procedure Execute; override;
  end;

  TJvPluginProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator) // both interfaces needed !!!!
  public
    { 0 = dll; 1 = dpk }
    PlugType: Integer;
    PlugName: string;
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  end;

  {$IFDEF COMPILER3}
  TSxPluginModuleCreator = class(TIModuleCreator)
  {$ELSE}
  TSxPluginModuleCreator = class(TIModuleCreatorEx) // found in editintf.pas
  {$ENDIF}
  public
    { 0 = dll; 1 = dpk }
    PlugType: Integer;
    PlugName: string;
    constructor Create;
    function Existing: Boolean; override; // overridden abstract methods
    function GetAncestorName: string; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function GetFormName: string; override;
    {$IFNDEF COMPILER3}
    function GetIntfName: string; override;
    function NewIntfSource(const UnitIdent, Form, Ancestor: string): string; override;
    function NewModuleSource(const UnitIdent, Form, Ancestor: string): string; override;
    {$ELSE}
    function NewModuleSource(UnitIdent, FormIdent, AncestorIdent: string): string; override;
    {$ENDIF}
    procedure FormCreated(Form: TIFormInterface); override;
  end;

  // from http://www.gexperts.org/opentools/GXModuleCreator.pas
  TJvOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: string;
  public
    constructor Create(const Source: string);
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses
  ToolIntf,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  Controls, SysUtils,
  JvPlugin, JvPluginParams, JvTypes;

{$R JvPluginWiz.res}

const
  cJediPuginWizard = 'JEDI Plugin Wizard';
  cPlgPrefix = 'Plg';
  cPluginPrefix = 'Plugin';
  cDirSep = '\';

//=== TJvPluginWizard ========================================================

function TJvPluginWizard.GetIDString: string;
begin
  Result := cJediPuginWizard;
end;

function TJvPluginWizard.GetStyle: TExpertStyle;
begin
  Result := esProject;
end;

function TJvPluginWizard.GetState: TExpertState;
begin
  Result := [esEnabled];
end;

function TJvPluginWizard.GetMenuText: string;
begin
  Result := cJediPuginWizard;
end;

function TJvPluginWizard.GetName: string;
begin
  Result := cJediPuginWizard;
end;

function TJvPluginWizard.GetPage: string;
begin
  Result := 'Projects';
end;

function TJvPluginWizard.GetAuthor: string;
begin
  Result := 'MPL';
end;

function TJvPluginWizard.GetComment: string;
begin
  Result := 'New Plugin';
end;

function TJvPluginWizard.GetGlyph: HICON;
begin
  Result := LoadIcon(HInstance, 'JvPLUGINWIZ');
end;

procedure TJvPluginWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectCreator: TJvPluginProjectCreator;
begin
  with TfrmPluginParams.Create(nil) do
    try
      if ShowModal = mrOk then
      begin
        if Assigned(BorlandIDEServices) and
          (BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) = S_OK) then
        begin
          ProjectCreator := TJvPluginProjectCreator.Create;
          { rbDll checked     => dll     => PlugType = 0 = Ord(False)
            rbPackage checked => package => PlugType = 1 = Ord(True)
          }
          ProjectCreator.PlugType := Ord(rbPackage.Checked); //  radPluginType.ItemIndex;
          ProjectCreator.PlugName := Trim(edName.Text);
          ModuleServices.CreateModule(ProjectCreator);
        end;
      end;
    finally
      Free;
    end;
end;

//=== TJvPluginProjectCreator ================================================

// left empty this makes problems !!

function TJvPluginProjectCreator.GetFileName: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    Result := GetCurrentDir + cDirSep + cPlgPrefix + PlugName + '.dpr'
  else
    Result := GetCurrentDir + cDirSep + cPlgPrefix + PlugName + '.dpk';
end;

function TJvPluginProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TJvPluginProjectCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

procedure TJvPluginProjectCreator.NewDefaultModule;
var
  ModuleCreator: TSxPluginModuleCreator;
begin
  ModuleCreator := TSxPluginModuleCreator.Create;
  ModuleCreator.PlugType := PlugType;
  ModuleCreator.PlugName := PlugName;
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    ToolServices.ModuleCreate(ModuleCreator,
      [cmAddToProject, cmMainForm, cmMarkModified, cmShowSource, cmShowForm, cmUnNamed])
  else
    ToolServices.ModuleCreate(ModuleCreator,
      [cmAddToProject, cmMainForm, cmMarkModified, cmShowSource, cmShowForm, cmUnNamed]);
  ModuleCreator.Free;
end;

function TJvPluginProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TJvPluginProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TJvPluginProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
var
  S: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    S := 'library ' + ProjectName + ';' + CrLf +
      CrLf +
      'uses' + CrLf +
      '  ShareMem;' + CrLf +
      CrLf +
      'exports' + CrLf +
      '  RegisterPlugin;' + CrLf +
      CrLf +
      'begin' + CrLf +
      'end.'
  else // Package-Library
    S := 'package ' + ProjectName + ';' + CrLf + CrLf +
      '{$DESCRIPTION ''JEDI Plugin Package''}' + CrLf +
      '{$RUNONLY}' + CrLf +
      '{$IMPLICITBUILD ON}' + CrLf + CrLf +
      'requires' + CrLf +
    {$IFDEF COMPILER5}
    '  vcl50,' + CrLf + '  JVCL200_R50;' +
    {$ENDIF COMPILER5}
    {$IFDEF COMPILER6}
    '  vcl,' + CrLf + '  JVCL200_R60;' +
    {$ENDIF COMPILER6}
    {$IFDEF COMPILER7}
    '  vcl,' + CrLf + '  JVCL200_R70;' +
    {$ENDIF COMPILER7}

    CrLf + CrLf + 'end.';

  Result := TJvOTAFile.Create(S);
end;

function TJvPluginProjectCreator.GetCreatorType: string;
begin
  { 0 = dll; 1 = dpk }
  if PlugType = 0 then
    Result := sLibrary
  else
    Result := sPackage;
end;

function TJvPluginProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJvPluginProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

// from http://www.gexperts.org/opentools/ProjectCreator.pas

function GetCurrentProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  I: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[I];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
    begin
      Result := IProjectGroup;
      Break;
    end;
  end;
end;

function TJvPluginProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProjectGroup; // nil
end;

function TJvPluginProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

//=== TSxPluginModuleCreator =================================================

constructor TSxPluginModuleCreator.Create;
begin
  inherited Create;
end;

function TSxPluginModuleCreator.Existing: Boolean;
begin
  Result := False;
end;

function TSxPluginModuleCreator.GetAncestorName: string;
begin
  Result := 'JvPlugin';
end;

function TSxPluginModuleCreator.GetFileName: string;
begin
  //  Result := '';
  Result := GetCurrentDir + cDirSep + cPluginPrefix + PlugName + '.pas'
end;

function TSxPluginModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TSxPluginModuleCreator.GetFormName: string;
begin
  Result := PlugName; //!
end;

{$IFNDEF COMPILER3}
function TSxPluginModuleCreator.GetIntfName: string;
begin
  Result := '';
end;

function TSxPluginModuleCreator.NewIntfSource(const UnitIdent, Form, Ancestor: string): string;
begin
  Result := '';
end;
{$ENDIF}

{$IFNDEF COMPILER3}
function TSxPluginModuleCreator.NewModuleSource(const UnitIdent, Form, Ancestor: string): string;
var
  TypeName: string;
begin
  TypeName := Form;
{$ELSE}
function TSxPluginModuleCreator.NewModuleSource(UnitIdent, FormIdent, AncestorIdent: string): string;
var
  TypeName: string;
  Ancestor: string;
begin
  TypeName := FormIdent;
  Ancestor := AncestorIdent;
{$ENDIF}

  //TypeName := PlugName;

  Result := 'unit ' + UnitIdent + ';' + CrLf + CrLf +

  'interface' + CrLf + CrLf +

  'uses' + CrLf +
    '  Windows, Messages, SysUtils, Classes, Dialogs, Forms,' + CrLf +
    '  JvPlugin;' + CrLf + CrLf +

  'type' + CrLf +
    '  T' + TypeName + ' = class(T' + Ancestor + ')' + CrLf +
    '  private' + CrLf +
    '    { Private declarations }' + CrLf +
    '  public' + CrLf +
    '    { Public declarations }' + CrLf +
    '  end;' + CrLf + CrLf +

  'function RegisterPlugin: T' + TypeName + '; stdcall;' + CrLf + CrLf;

  { 0 = dll; 1 = dpk }
  if PlugType <> 0 then
    Result := Result + 'exports RegisterPlugin;' + CrLf + CrLf;

  Result := Result +
    'implementation' + CrLf + CrLf +

  '{$R *.DFM}' + CrLf + CrLf +

  '// IMPORTANT NOTE: If you change the name of the Plugin container,' + CrLf +
    '// you must set the type below to the same type. (Delphi changes' + CrLf +
    '// the declaration, but not the procedure itself. Both the return' + CrLf +
    '// type and the type created must be the same as the declared type above.' + CrLf +
    'function RegisterPlugin: T' + TypeName + ';' + CrLf + CrLf +
    'begin' + CrLf +
    '  Result := T' + TypeName + '.Create(nil);' + CrLf +
    'end;' + CrLf + CrLf +

  'end.'
end;

procedure TSxPluginModuleCreator.FormCreated(Form: TIFormInterface);
begin
end;

//=== TJvOTAFile =============================================================

// TJvOTAFile - from Stefaan Lesage

constructor TJvOTAFile.Create(const Source: string);
begin
  FSource := Source;
end;

function TJvOTAFile.GetAge: TDateTime;
begin
  Result := -1; // new
end;

function TJvOTAFile.GetSource: string;
begin
  Result := FSource;
end;

procedure RegisterContainerModule;
begin
  RegisterCustomModule(TJvPlugin, TCustomModule);
end;

initialization
  RegisterContainerModule;

end.

