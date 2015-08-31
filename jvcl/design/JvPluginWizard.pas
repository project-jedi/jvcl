{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPluginWizard.PAS, released on 1999-09-06.

The Initial Developer of the Original Code is Tim Sullivan [timatt uil dott net]
Portions created by Tim Sullivan are Copyright (C) 1999 Tim Sullivan.
All Rights Reserved.

Contributor(s):
Ralf Steinhaeusser [ralfiii att gmx dott net].
Steefan Lesage - converted to use new OTA
Ramzi Haidar - fix for RAD Studio 2007
Florent Ouchet - updated OTA

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

 Todo : I don't know why but IDE does not not navigate correctly here...
 Todo : Creates code with #10#13 instead of the other way ound (#13#10 is correct)

 History:

 V 09 : inserted TYPE-decaration, IDE navigation works now
        rewrote ProjectCreate (100 times), seems to work now except a AV when
        creating a DLL-project. May have to do with ressources
 V 08 : Setting the pluginname works fine after A LOT of trying...
 V 07 : #10#13 -> #13#10 corrected
 V 06 : Wizard-Form added, lets select Plugin-Type and Object-name
 V 05 : uses-list minimized

 -----------------------------------------------------------------------------}
// $Id$

unit JvPluginWizard;

{$I jvcl.inc}

interface

uses
  Windows, ToolsAPI,
  JvTypes;

const
  PLUGIN_PACKAGE_TEMPLATE_BCB =
  '//---------------------------------------------------------------------------' + #13#10 +
  '' + #13#10 +
  '#include <basepch.h>                                                         ' + #13#10 +
  '#pragma hdrstop                                                              ' + #13#10 +
  '#pragma package(smart_init)                                                  ' + #13#10 +
  '//---------------------------------------------------------------------------' + #13#10 +
  '                                                                             ' + #13#10 +
  '//   Package source.                                                         ' + #13#10 +
  '//---------------------------------------------------------------------------' + #13#10 +
  '' + #13#10 +
  '' + #13#10 +
  '#pragma argsused                                                             ' + #13#10 +
  'int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)       ' + #13#10 +
  '{                                                                            ' + #13#10 +
  '    return 1;                                                                ' + #13#10 +
  '}                                                                            ' + #13#10 +
  '//---------------------------------------------------------------------------';

  PLUGIN_DLL_TEMPLATE_BCB =
  '//---------------------------------------------------------------------------' + #13#10 +
  '' + #13#10 +
  '#include <vcl.h>                                                         ' + #13#10 +
  '#include <windows.h>                                                         ' + #13#10 +
  '#pragma hdrstop                                                              ' + #13#10 +
  '//---------------------------------------------------------------------------' + #13#10 +
  '' + #13#10 +
  '#pragma argsused                                                             ' + #13#10 +
  'int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void* lpReserved)' + #13#10 +
  '{                                                                            ' + #13#10 +
  '    return 1;                                                                ' + #13#10 +
  '}                                                                            ' + #13#10 +
  '//---------------------------------------------------------------------------';

  BCB6_OPTIONS_TEMPLATE =
  '<?xml version=''1.0'' encoding=''utf-8'' ?>' + #13#10 + 
  '<!-- C++Builder XML Project -->' + #13#10 + 
  '<PROJECT>' + #13#10 + 
  '  <MACROS>' + #13#10 + 
  '    <VERSION value="BCB.06.00"/>' + #13#10 + 
  '    <PROJECT value="%ModuleIdent.bpl"/>' + #13#10 + 
  '    <OBJFILES value=""/>' + #13#10 + 
  '    <RESFILES value="%ModuleIdent.res"/>' + #13#10 + 
  '    <DEFFILE value=""/>' + #13#10 + 
  '    <RESDEPEN value="$(RESFILES)"/>' + #13#10 + 
  '    <LIBFILES value=""/>' + #13#10 + 
  '    <LIBRARIES value=""/>' + #13#10 + 
  '    <SPARELIBS value=""/>' + #13#10 + 
  '    <PACKAGES value="vcl.bpi rtl.bpi"/>' + #13#10 + 
  '    <PATHCPP value=".;"/>' + #13#10 + 
  '    <PATHPAS value=".;"/>' + #13#10 + 
  '    <PATHRC value=".;"/>' + #13#10 + 
  '    <PATHASM value=".;"/>' + #13#10 + 
  '    <DEBUGLIBPATH value="$(BCB)\lib\debug"/>' + #13#10 + 
  '    <RELEASELIBPATH value="$(BCB)\lib\release"/>' + #13#10 + 
  '    <LINKER value="ilink32"/>' + #13#10 + 
  '    <USERDEFINES value="_DEBUG"/>' + #13#10 + 
  '    <SYSDEFINES value="_RTLDLL;NO_STRICT;USEPACKAGES"/>' + #13#10 + 
  '    <MAINSOURCE value="%ModuleIdent.cpp"/>' + #13#10 + 
  '    <INCLUDEPATH value="$(BCB)\include;$(BCB)\include\vcl"/>' + #13#10 + 
  '    <LIBPATH value="$(BCB)\Projects\Lib;$(BCB)\lib\obj;$(BCB)\lib"/>' + #13#10 + 
  '    <WARNINGS value="-w-par"/>' + #13#10 + 
  '    <OTHERFILES value=""/>' + #13#10 + 
  '  </MACROS>' + #13#10 + 
  '  <OPTIONS>' + #13#10 + 
  '    <CFLAG1 value="-Od -H=$(BCB)\lib\vcl60.csm -Hc -Vx -Ve -X- -r- -a8 -b- -k -y -v -vi- -c ' + #13#10 + 
  '      -tWM"/>' + #13#10 + 
  '    <PFLAGS value="-$YD -$W -$O- -$A8 -v -JPHNE -M"/>' + #13#10 + 
  '    <RFLAGS value=""/>' + #13#10 + 
  '    <AFLAGS value="/mx /w2 /zd"/>' + #13#10 + 
  '    <LFLAGS value="-D&quot;&quot; -aa -Tpp -x -Gn -Gl -Gi -v"/>' + #13#10 + 
  '    <OTHERFILES value=""/>' + #13#10 + 
  '  </OPTIONS>' + #13#10 + 
  '  <LINKER>' + #13#10 + 
  '    <ALLOBJ value="c0pkg32.obj $(PACKAGES) Memmgr.Lib sysinit.obj $(OBJFILES)"/>' + #13#10 + 
  '    <ALLRES value="$(RESFILES)"/>' + #13#10 + 
  '    <ALLLIB value="$(LIBFILES) $(LIBRARIES) import32.lib cp32mti.lib"/>' + #13#10 + 
  '    <OTHERFILES value=""/>' + #13#10 + 
  '  </LINKER>' + #13#10 + 
  '  <FILELIST>' + #13#10 + 
  '      <FILE FILENAME="%ModuleIdent.res" FORMNAME="" UNITNAME="%ModuleIdent.res" CONTAINERID="ResTool" DESIGNCLASS="" LOCALCOMMAND=""/>' + #13#10 + 
  '      <FILE FILENAME="%ModuleIdent.cpp" FORMNAME="" UNITNAME="%ModuleIdent" CONTAINERID="CCompiler" DESIGNCLASS="" LOCALCOMMAND=""/>' + #13#10 + 
  '  </FILELIST>' + #13#10 + 
  '  <BUILDTOOLS>' + #13#10 + 
  '  </BUILDTOOLS>' + #13#10 + 
  '' + #13#10 + 
  '  <IDEOPTIONS>' + #13#10 + 
  '[Version Info]' + #13#10 + 
  'IncludeVerInfo=1' + #13#10 + 
  'AutoIncBuild=0' + #13#10 + 
  'MajorVer=1' + #13#10 + 
  'MinorVer=0' + #13#10 + 
  'Release=0' + #13#10 + 
  'Build=0' + #13#10 + 
  'Debug=0' + #13#10 + 
  'PreRelease=0' + #13#10 + 
  'Special=0' + #13#10 + 
  'Private=0' + #13#10 + 
  'DLL=0' + #13#10 + 
  'Locale=3081' + #13#10 + 
  'CodePage=1252' + #13#10 + 
  '' + #13#10 + 
  '[Version Info Keys]' + #13#10 + 
  'CompanyName=' + #13#10 + 
  'FileDescription=' + #13#10 + 
  'FileVersion=1.0.0.0' + #13#10 + 
  'InternalName=' + #13#10 + 
  'LegalCopyright=' + #13#10 + 
  'LegalTrademarks=' + #13#10 + 
  'OriginalFilename=' + #13#10 + 
  'ProductName=' + #13#10 + 
  'ProductVersion=1.0.0.0' + #13#10 + 
  'Comments=' + #13#10 + 
  '' + #13#10 + 
  '[Debugging]' + #13#10 + 
  'DebugSourceDirs=$(BCB)\source\vcl' + #13#10 + 
  '' + #13#10 + 
  '[Parameters]' + #13#10 + 
  'RunParams=' + #13#10 + 
  'Launcher=' + #13#10 + 
  'UseLauncher=0' + #13#10 + 
  'DebugCWD=' + #13#10 + 
  'HostApplication=' + #13#10 + 
  'RemoteHost=' + #13#10 + 
  'RemotePath=' + #13#10 + 
  'RemoteLauncher=' + #13#10 + 
  'RemoteCWD=' + #13#10 + 
  'RemoteDebug=0' + #13#10 + 
  '' + #13#10 + 
  '[Compiler]' + #13#10 + 
  'ShowInfoMsgs=0' + #13#10 + 
  'LinkDebugVcl=0' + #13#10 + 
  'LinkCGLIB=0' + #13#10 + 
  '' + #13#10 + 
  '[Language]' + #13#10 + 
  'ActiveLang=' + #13#10 + 
  'ProjectLang=' + #13#10 + 
  'RootDir=' + #13#10 + 
  '' + #13#10 + 
  '[Linker]' + #13#10 + 
  'LibPrefix=' + #13#10 + 
  'LibSuffix=' + #13#10 + 
  'LibVersion=' + #13#10 + 
  '' + #13#10 + 
  '</IDEOPTIONS>' + #13#10 + 
  '</PROJECT>';
    

type
  TJvPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTARepositoryWizard60,
    {$IFDEF COMPILER8_UP} IOTARepositoryWizard80, {$ENDIF COMPILER8_UP}
    {$IFDEF COMPILER10_UP} IOTAProjectWizard100, {$ENDIF COMPILER10_UP}
    IOTAMenuWizard, IOTAProjectWizard)
  private
    FUniqueID: string;
    FCaption: string;

    function GetIsCBuilder: Boolean;
  public
    PluginMainMenu: IOTAComponent;

    constructor Create;
    procedure InitializeWizard; virtual;

    { IOTAWizard Methods }
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual;

    { IOTARepositoryWizard Methods }
    function GetAuthor: string; virtual;
    function GetComment: string; virtual;
    function GetPage: string; virtual;
    function GetGlyph: Cardinal; virtual;

    { IOTAMenuWizard methods }
    function GetMenuText: string; virtual;

    { IOTARepositoryWizard60 }
    function GetDesigner: string;

    {$IFDEF COMPILER8_UP}
    { IOTARepositoryWizard80 }
    function GetGalleryCategory: IOTAGalleryCategory; virtual;
    function GetPersonality: string; virtual;
    {$ENDIF COMPILER8_UP}

    {$IFDEF COMPILER10_UP}
    { IOTAProjectWizard100 }
    function IsVisible(Project: IOTAProject): Boolean;
    {$ENDIF COMPILER10_UP}
    
    {$IFDEF COMPILER8_UP}
    property Personality: string read GetPersonality;
    {$ENDIF}
    
    property Caption: string read GetName write FCaption;
    property UniqueID: string read GetIDString write FUniqueID;  
    property IsCBuilder: Boolean read GetIsCBuilder;
  end;

  
  TJvPluginWizardDelphi = class(TJvPluginWizard, IOTAWizard, IOTARepositoryWizard,
    IOTARepositoryWizard60,
    {$IFDEF COMPILER8_UP} IOTARepositoryWizard80, {$ENDIF COMPILER8_UP}
    {$IFDEF COMPILER10_UP} IOTAProjectWizard100, {$ENDIF COMPILER10_UP}
    IOTAMenuWizard, IOTAProjectWizard)  
  public
    {$IFDEF COMPILER8_UP}
    { IOTARepositoryWizard80 }
    function GetPersonality: string; override;
    {$ENDIF COMPILER8_UP}
  end;
  
  TJvPluginWizardBuilder = class(TJvPluginWizard, IOTAWizard, IOTARepositoryWizard,
    IOTARepositoryWizard60,
    {$IFDEF COMPILER8_UP} IOTARepositoryWizard80, {$ENDIF COMPILER8_UP}
    {$IFDEF COMPILER10_UP} IOTAProjectWizard100, {$ENDIF COMPILER10_UP}
    IOTAMenuWizard, IOTAProjectWizard)  
  public
    {$IFDEF COMPILER8_UP}
    { IOTARepositoryWizard80 }
    function GetPersonality: string; override;
    {$ENDIF COMPILER8_UP}
  end;
  

  TJvPlugType = ( ptPackage, ptDLL );

  TJvPluginProjectCreator = class(TInterfacedObject, IOTACreator,
    IOTAProjectCreator50,
    {$IFDEF COMPILER8_UP} IOTAProjectCreator80, {$ENDIF COMPILER8_UP}
    IOTAProjectCreator)
  public
    Wizard: TJvPluginWizard;
    PlugType: TJvPlugType;
    { Private variables which will be used to store some properties for
      the TJvPlugIn }
    PlugName: string;
    PlugDesc: string;
    PlugAuth: string;
    PlugCopy: string;
    PlugUID: string;
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
    
    {--- IOTAProjectCreator50 ---}
    procedure NewDefaultProjectModule(const Project: IOTAProject);

    {--- IOTAProjectCreator80 ---}
    {$IFDEF COMPILER8_UP}
    function GetProjectPersonality: string;
    {$ENDIF COMPILER8_UP}
  end;
  
  TJvPluginModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    Wizard: TJvPluginWizard;
    PlugType: TJvPlugType;
    Project: IOTAModule;
    { Private variables which will be used to store some properties for
      the TJvPlugIn }
    PlugName: string;
    PlugDesc: string;
    PlugAuth: string;
    PlugCopy: string;
    PlugUID: string;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TJvPluginDLLModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    Wizard: TJvPluginWizard;
    Project: IOTAModule;
    PlugType: TJvPlugType;
    
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;

    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TJvOTAFile = class(TNotifierObject, IOTAFile)
  private
    FSource : string;
    FAge    : TDateTime;
  protected
    {--- IOTAFile ---}
    function GetSource: string;
    function GetAge: TDateTime;
  public
    constructor Create(aSource: string; aAge: TDateTime);
  end;

implementation

uses
  Controls, SysUtils, Dialogs, Classes, ActnList, Menus,
  DesignIntf, DesignEditors,
  JclFileUtils, JclIDEUtils,
  JvPlugin, JvPluginParamsForm, JvConsts, JvDsgnConsts;

{$R JvPluginWiz.res}

const
  CrLf = sLineBreak;
  CrLf2 = CrLf + CrLf;
  cPlgPrefix = 'Plg';
  cPluginPrefix = 'PlugIn';

(* make Delphi 5 compiler happy // andreas
function GetFormEditorFromModule(
  IModule: IOTAModule): IOTAFormEditor;
var
  i: Integer;
  IEditor: IOTAEditor;
begin
  Result := nil;
  if IModule = nil then
     Exit;
  for i := 0 to IModule.GetModuleFileCount - 1 do
  begin
    IEditor := IModule.GetModuleFileEditor(i);
    if Supports(IEditor, IOTAFormEditor, Result) then
      Break;
  end;
end;
*)

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

//=== { TJvPluginWizard } ====================================================

constructor TJvPluginWizard.Create;
begin
  inherited Create;
  FCaption := RsJediPluginWizard;
  InitializeWizard;
end;

procedure TJvPluginWizard.InitializeWizard;
begin
  // Override in descendent
end;

function TJvPluginWizard.GetIDString: string;
begin
  Result := FUniqueID; //RsPluginWizardIDString;
end;

function TJvPluginWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TJvPluginWizard.GetMenuText: string;
begin
  Result := FCaption; //RsJediPluginWizard;
end;

function TJvPluginWizard.GetName: string;
begin
  Result := FCaption; //RsJvPluginWizard;
end;

function TJvPluginWizard.GetPage: string;
var
  Repository : TJclBorRadToolRepository;
  Installations : TJclBorRADToolInstallations;
begin
  Result := RsProjects;
  Installations := TJclBorRADToolInstallations.Create;
  try
    {$IFDEF BCB}
    Repository := Installations.BCBInstallationFromVersion[
      {$IFDEF BCB6} 6 {$ENDIF}
      ].Repository;
    {$ENDIF BCB}
    {$IFDEF DELPHI}
    Repository := Installations.DelphiInstallationFromVersion[
      {$IFDEF DELPHI23} 23 {$ENDIF} 
      {$IFDEF DELPHI22} 22 {$ENDIF} 
      {$IFDEF DELPHI21} 21 {$ENDIF} 
      {$IFDEF DELPHI20} 20 {$ENDIF} 
      {$IFDEF DELPHI19} 19 {$ENDIF} 
      {$IFDEF DELPHI18} 18 {$ENDIF}
      {$IFDEF DELPHI17} 17 {$ENDIF}
      {$IFDEF DELPHI16} 16 {$ENDIF}
      {$IFDEF DELPHI15} 15 {$ENDIF}
      {$IFDEF DELPHI14} 14 {$ENDIF}
      {$IFDEF DELPHI12} 12 {$ENDIF}
      {$IFDEF DELPHI11} 11 {$ENDIF}
      {$IFDEF DELPHI10} 10 {$ENDIF}
      {$IFDEF DELPHI9}   9 {$ENDIF}
      {$IFDEF DELPHI7}   7 {$ENDIF}
      {$IFDEF DELPHI6}   6 {$ENDIF}
      ].Repository;
    {$ENDIF DELPHI}
    
    Result := Repository.FindPage(RsProjects, 2);
  finally
    Installations.Free;
  end;
end;

function TJvPluginWizard.GetAuthor: string;
begin
  Result := 'MPL';
end;

function TJvPluginWizard.GetComment: string;
begin
  Result := RsNewPlugin;
end;

function TJvPluginWizard.GetGlyph: Cardinal; 
begin
  Result := LoadIcon(HInstance, 'XJVPLUGINWIZ');
end;

procedure TJvPluginWizard.Execute;
var
  ModuleServices: IOTAModuleServices;
  ProjectCreator: TJvPluginProjectCreator;
  frmPluginParams: TfrmPluginParams;
begin
  frmPluginParams := TfrmPluginParams.Create(nil);
  try
    if (frmPluginParams.ShowModal = mrOk) and Assigned(BorlandIDEServices) and
      (BorlandIDEServices.QueryInterface(IOTAModuleServices, ModuleServices) = S_OK) then
    begin
      ProjectCreator := TJvPluginProjectCreator.Create;
      ProjectCreator.Wizard := Self;

      if frmPluginParams.rbPackage.Checked then
        ProjectCreator.PlugType := ptPackage
      else
        ProjectCreator.PlugType := ptDLL;
      ProjectCreator.PlugName := Trim(frmPluginParams.edtPluginName.Text);
      ProjectCreator.PlugAuth := Trim(frmPluginParams.edtPluginAuthor.Text);
      ProjectCreator.PlugCopy := Trim(frmPluginParams.edtPluginCopyright.Text);
      ProjectCreator.PlugDesc := Trim(frmPluginParams.mmoDescripton.Text);
      ProjectCreator.PlugUID := Trim(frmPluginParams.edtPluginUID.Text);

      ModuleServices.CreateModule(ProjectCreator);
    end;
  finally
    frmPluginParams.Free;
  end;
end;

function TJvPluginWizard.GetIsCBuilder: Boolean;
begin
  {$IFDEF COMPILER8_UP}
  Result := Personality = sCBuilderPersonality;
  {$ELSE}
    {$IFDEF BCB}
    Result := True;
    {$ELSE}
    Result := False;
    {$ENDIF BCB}
  {$ENDIF COMPILER8_UP}
end;

{$IFDEF COMPILER8_UP}
function TJvPluginWizardDelphi.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;

function TJvPluginWizardBuilder.GetPersonality: string;
begin
  Result := sCBuilderPersonality;
end;
{$ENDIF COMPILER8_UP}

function TJvPluginWizard.GetDesigner: string;
begin
  Result := dVCL;
end;

{$IFDEF COMPILER8_UP}
function TJvPluginWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := (BorlandIDEServices as IOTAGalleryCategoryManager).FindCategory('Borland.Delphi.New.Expert');
end;

function TJvPluginWizard.GetPersonality: string;
begin
  Result := sDelphiPersonality;
end;
{$ENDIF COMPILER8_UP}

{$IFDEF COMPILER10_UP}
function TJvPluginWizard.IsVisible(Project: IOTAProject): Boolean;
begin
  Result := True;
end;
{$ENDIF COMPILER10_UP}

//=== { TJvPluginProjectCreator } ============================================

// left empty this makes problems !!

function TJvPluginProjectCreator.GetFileName: string;
var
  sTmp: string;
  sExt: string;
begin
  if Wizard <> nil then
  begin
    sTmp := '';

    if Wizard.IsCBuilder then
    begin
      sExt := '.cpp';
      sTmp := GetCurrentDir + DirDelimiter + cPlgPrefix + PlugName + sExt
    end
    else
    begin
      if PlugType = ptDLL then
        sExt := '.dpr'
      else // PlugType = ptPackage
        sExt := '.dpk';

      sTmp := GetCurrentDir + DirDelimiter + cPlgPrefix + PlugName + sExt
    end
  end;
  
  Result := sTmp;
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
begin
  // deprecated
end;

function TJvPluginProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
var
  sTmp: string;
begin
  sTmp := '';
  
  {$IFDEF COMPILER10_UP}
  Result := nil;
  {$ELSE ~COMPILER10_UP}
    if PlugType = ptPackage then
    begin
    {$IFDEF BCB6}
      sTmp := BCB6_OPTIONS_TEMPLATE;
      sTmp := StringReplace(sTmp, '%ModuleIdent', ProjectName, [rfIgnoreCase, rfReplaceAll]);
      Result := TJvOTAFile.Create(sTmp, -1);
    {$ELSE ~BCB6}
      // Anything else is not supported
      Result := nil;
    {$ENDIF BCB6}
    end
  {$ENDIF COMPILER10_UP}
end;

procedure TJvPluginProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  // nothing
end;

function TJvPluginProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    if Wizard.IsCBuilder then
    begin
      if PlugType = ptDLL then
        Lines.Add(PLUGIN_DLL_TEMPLATE_BCB)
      else // PlugType = ptPackage
        Lines.Add(PLUGIN_PACKAGE_TEMPLATE_BCB)
    end
    else
    begin
      if PlugType = ptDLL then
      begin
        Lines.Add('library ' + ProjectName + ';');
        Lines.Add('');    
        Lines.Add('uses');    
        Lines.Add('  ShareMem;');
        //, ' + cPluginPrefix + PlugName + ';'); // will be added by nested CreateModule
        Lines.Add('');    
        Lines.Add('{$R *.res}');    
        Lines.Add('');    
        Lines.Add('exports');    
        Lines.Add('  RegisterPlugin;');    
        Lines.Add('');    
        Lines.Add('begin');    
        Lines.Add('end.');
      end    
      else // PlugType = ptPackage
      begin
        Lines.Add('package ' + ProjectName + ';');
        Lines.Add('');
        Lines.Add('{$DESCRIPTION ''JEDI Plugin Package''}');
        Lines.Add('{$RUNONLY}');
        Lines.Add('{$IMPLICITBUILD ON}');
        Lines.Add('');
        Lines.Add('end.');
      end
    end;
    
    Result := TJvOTAFile.Create(Lines.Text, -1);
  finally
    Lines.Free;
  end;
end;

function TJvPluginProjectCreator.GetCreatorType: string;
begin
  {$IFDEF COMPILER11_UP}
  {$IF declared(sCppDynamicLibrary)} // a "Delphi only" installation does not declare the C++Builder constants
  if Wizard.IsCBuilder then
  begin
    // Required for C++ Builder 2007, so it creates the correct project structure.
    // NOTE: There seems to be a display bug in C++ Builder 2007 (or maybe it's just my installation?).
    // When you create a package or DLL, the created output file extension is set to "exe" instead of
    // "bpl"/"dll".  If you try to compile it will also generate a BPL or DLL file, not an EXE file
    // as is displayed.... strange :\
    // If you save the project and re-open it, and it will display "bpl" or "dll" correctly.
    if PlugType = ptDLL then
      Result := sCppDynamicLibrary
    else // PlugType = ptPackage
      Result := sCppPackage
  end
  else
  {$IFEND}
  {$ENDIF COMPILER11_UP}
  begin
    if PlugType = ptDLL then
      Result := sLibrary
    else // PlugType = ptPackage
      Result := sPackage
  end;
end;

function TJvPluginProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJvPluginProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TJvPluginProjectCreator.GetOwner: IOTAModule;
begin
  Result := GetCurrentProjectGroup; // nil
end;

function TJvPluginProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TJvPluginProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
var
  Module: IOTAModule;
  ModuleCreator: TJvPluginModuleCreator;
  {$IFDEF COMPILER10_UP}
  DllModuleCreator: TJvPluginDLLModuleCreator;
  {$ENDIF COMPILER10_UP}
begin
  if Wizard.IsCBuilder then
  begin
    if PlugType = ptPackage then
    begin
      {$IFDEF COMPILER8_UP}
      Project.AddFile('rtl.bpi', False);
      {$ENDIF COMPILER8_UP}
      Project.AddFile('JvCore.bpi', False)
    end
  end
  else
  begin
    // add requires
    if PlugType = ptPackage then
    begin
      Project.AddFile('JvCore.dcp', False);
    end
  end;

  // create a new unit
  ModuleCreator := TJvPluginModuleCreator.Create;
  ModuleCreator.Wizard := Wizard;
  ModuleCreator.PlugType := PlugType;
  ModuleCreator.PlugName := PlugName;
  ModuleCreator.PlugAuth := PlugAuth;
  ModuleCreator.PlugDesc := PlugDesc;
  ModuleCreator.PlugCopy := PlugCopy;
  ModuleCreator.PlugUID := PlugUID;
  ModuleCreator.Project := Project;
  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
  
  if Wizard.IsCBuilder then
  begin
    if PlugType = ptDLL then
    begin
      // I have no idea why we have to do this for the newer Borland/CodeGear IDEs, but it is not required
      // for older versions (such as BCB6), since they create their own project source file for DLLs
      {$IFDEF COMPILER10_UP}
      DllModuleCreator := TJvPluginDLLModuleCreator.Create;
      DllModuleCreator.Wizard := Wizard;
      DllModuleCreator.Project := Project;
      DllModuleCreator.PlugType := PlugType;
      Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(DllModuleCreator);
      {$ENDIF COMPILER10_UP}
    end
  end;
end;

{$IFDEF COMPILER8_UP}
function TJvPluginProjectCreator.GetProjectPersonality: string;
begin
  Result := Wizard.Personality;
end;
{$ENDIF COMPILER8_UP}

//=== { TJvOTAFile } =========================================================

constructor TJvOTAFile.Create(aSource: string; aAge: TDateTime);
begin
  inherited Create;
  FSource := aSource;
  FAge := aAge;
end;

function TJvOTAFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TJvOTAFile.GetSource: string;
begin
  Result := FSource;
end;

procedure RegisterContainerModule;
begin
  RegisterCustomModule(TJvPlugIn, TCustomModule);
end;

//=== { TJvPluginModuleCreator } =============================================

{*****************************************************************************
  Name           : TJvPluginModuleCreator.FormCreated
  Author         : Stefaan Lesage
  Arguments      : FormEditor - Interface to the IOTAFormEditor for the form
                                that has been created.
  Return Values  : None
  Exceptions     : None
  Description    : This method will be executed Called the new
                   form/datamodule/custom module is created.  We will use it
                   to initialise some properties on the TJvPluginDataModule.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

procedure TJvPluginModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
var
  Plugin: TJvPlugIn;
begin
  Plugin := INTAComponent(FormEditor.GetRootComponent).GetComponent as TJvPlugIn;
  Plugin.Author := PlugAuth;
  Plugin.Description := PlugDesc;
  Plugin.Copyright := PlugCopy;
  Plugin.PluginID := PlugUID;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetAncestorName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : The name of the Ancestor.
  Exceptions     : None
  Description    : Property Getter for the AncestorName property
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetAncestorName: string;
begin
  Result := 'JvPlugIn';
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetCreatorType
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the type of the creator as a string.  In our case
                   it returns sForm since we create a DataModule / CustomForm.
  Exceptions     : None
  Description    : Property Getter for the CreatorType property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetExisting
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns a Boolean indicating if this is an existing
                   module.  We return False since this is a new Module.
  Exceptions     : None
  Description    : Property Getter for the Existing property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetFileSystem
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Return the File system IDString that this module uses for
                   reading/writing.  We return an empty string since our module
                   doesn't use a virtual file system.
  Exceptions     : None
  Description    : Property Getter for the FileSystem property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetFormName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the name of the form ( not the class, but its
                   actual name property ).  We will make sure that each
                   plugin module always gets the Plugin prefix before its
                   name.
  Exceptions     : None
  Description    : Property getter for the FormName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetFormName: string;
begin
  Result := PlugName;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetImplFileName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the complete path to the implementation ( source )
                   file name, e.g. C:\dir\Unit1.pas. If GetUnnamed returns
                   True, the file name is just a placeholder, and the user
                   will be prompted for a file name when the file is saved.
  Exceptions     : None
  Description    : Property getter for the ImplFileName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetImplFileName: string;
var
  sExt: string;
begin
  if Wizard <> nil then
  begin
    if Wizard.IsCBuilder then
      sExt := '.cpp'
    else
      sExt := '.pas';
  end;
  
  Result := GetCurrentDir + DirDelimiter + cPluginPrefix + PlugName + sExt;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetIntfFileName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Return the interface filename, or blank to have the IDE
                   create a new unique one.
  Exceptions     : None
  Description    : Property getter for the IntfFileName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetMainForm
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if the newly created form is to be the
                   applications main form. It returns False if the form
                   is not necessarily the main form.
  Exceptions     : None
  Description    : Property getter for the MainForm property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetOwner
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the module interface of the new modules owner,
                   that is, it returns the project interface for a new source
                   file or the project group interface for a new project.
                   You can create a module that does not have an owner by
                   returning 0.
                   In our case we will return the Active ProjectGroup's
                   Active Project.
  Exceptions     : None
  Description    : Property getter for the Owner property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetOwner: IOTAModule;
begin
  Result := Project;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetShowForm
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if you want the IDE to show the form editor
                   after the form is created. Have GetShowForm returns False
                   to keep the form hidden.
  Exceptions     : None
  Description    : Property getter for the ShowForm property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetShowSource
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if you want the IDE to show the source file
                   in the source editor. Have GetShowSource return False to
                   keep the source file hidden.
  Exceptions     : None
  Description    : Property getter for the ShowSource property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.GetUnnamed
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if the new module has not been saved to a
                   file and therefore does not have a file name yet. If the
                   user saves the module, the user will be prompted for a
                   file name. GetUnnamed returns False if the module has a
                   file name.
  Exceptions     : None
  Description    : Property Getter for the Unnamed property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.NewFormFile
  Author         : Stefaan Lesage
  Arguments      : FormIdent     - The name of the form. Use this to
                                   parameterize the form description.
                   AncestorIdent - The name of the ancestor form. Use this to
                                   parameterize the form description.
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewFormFile returns the new form description or 0 to use
                   the default form description. The form description must be
                   binary or text form resource.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.NewImplSource
  Author         : Stefaan Lesage
  Arguments      : ModuleIdent   - The ModuleIdent parameter is the name of
                                   the unit or module, e.g., Unit1. Use
                                   this to parameterize the file contents.
                   FormIdent     - The FormIdent parameter is the name of
                                   the form. Use this to parameterize the
                                   file contents.
                   AncestorIdent - The AncestorIdent parameter is the name
                                   of the ancestor form. Use this to
                                   parameterize the file contents
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewImplSource returns the source code for the new
                   modules implementation or 0 for a default unit.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
  TypeName: string;
  Ancestor: string;
  ClassNameOfPlugin: string;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    ClassNameOfPlugin := 'T' + FormIdent;

    TypeName := FormIdent;
    Ancestor := AncestorIdent;

    TypeName := PlugName;
    if Wizard.IsCBuilder then
    begin
      Lines.Add('//---------------------------------------------------------------------------');
      Lines.Add('');
      Lines.Add('#include <vcl.h>');
      Lines.Add('#pragma hdrstop');
      Lines.Add('');
      Lines.Add('#include "JvPlugin.hpp"');
      Lines.Add('#include "' + ModuleIdent + '.h"');
      Lines.Add('//---------------------------------------------------------------------------');
      Lines.Add('#pragma package(smart_init)');
      Lines.Add('#pragma resource "*.dfm"');
      Lines.Add(ClassNameOfPlugin + ' *' + FormIdent + ';');
      Lines.Add('');
      Lines.Add('//---------------------------------------------------------------------------');
      Lines.Add('__fastcall ' + ClassNameOfPlugin + '::' + ClassNameOfPlugin + '(TComponent* Owner)');
      Lines.Add('    : TJvPlugIn(Owner)');
      Lines.Add('{');
      Lines.Add('}');
      Lines.Add('//---------------------------------------------------------------------------');
      Lines.Add(RsIMPORTANTNOTEIfYouChangeTheNameOfTh);
      
      if PlugType = ptDLL then
      begin
        Lines.Add('extern "C" __declspec(dllexport)')
      end;
      
      Lines.Add('TJvPlugIn* __stdcall RegisterPlugin(void)');
      Lines.Add('{');
      Lines.Add('  ' + FormIdent + ' = new ' + ClassNameOfPlugin + '(NULL);');
      Lines.Add('  return ' + FormIdent + ';');
      Lines.Add('}');
      Lines.Add('//---------------------------------------------------------------------------');
      Lines.Add('')
    end
    else
    begin
      Lines.Add('unit ' + ModuleIdent + ';');
      Lines.Add('');
      Lines.Add('interface');
      Lines.Add('');
      Lines.Add('uses');
      Lines.Add('  Windows, Messages, SysUtils, Classes, Dialogs, Forms, Controls,');
      Lines.Add('  JvPlugin;');
      Lines.Add('');
      Lines.Add('type');
      Lines.Add('  ' + ClassNameOfPlugin + ' = class(T' + Ancestor + ')');
      //Lines.Add('  T' + TypeName + ' = class(T' + Ancestor + ')');
      Lines.Add('  private');
      Lines.Add('    ' + RsPrivateDeclarations);
      Lines.Add('  public');
      Lines.Add('    ' + RsPublicDeclarations);
      Lines.Add('end;');
      Lines.Add('');
      //Lines.Add('function RegisterPlugin: T' + TypeName + '; stdcall;');
      Lines.Add('function RegisterPlugin: TJvPlugIn; stdcall;');
      Lines.Add('');
  
      if PlugType = ptPackage then
      begin
        Lines.Add('exports RegisterPlugin;');
        Lines.Add('');
      end;
      
      Lines.Add('implementation');
      Lines.Add('');
      Lines.Add('{$R *.dfm}');
      Lines.Add('');
      Lines.Add(RsIMPORTANTNOTEIfYouChangeTheNameOfTh);
      Lines.Add('function RegisterPlugin: TJvPlugIn;');
      Lines.Add('begin');
      Lines.Add('  Result := ' + ClassNameOfPlugin + '.Create(nil);');
      //    '  Result := T' + TypeName + '.Create(nil);' + CrLf +
      Lines.Add('end;');
      Lines.Add('');
      Lines.Add('end.');
    end;
    
    Result := TJvOTAFile.Create(Lines.Text, -1);
  finally
    Lines.Free;
  end;
end;

{*****************************************************************************
  Name           : TJvPluginModuleCreator.NewIntfSource
  Author         : Stefaan Lesage
  Arguments      : ModuleIdent   - The ModuleIdent parameter is the name of
                                   the unit or module, e.g., Unit1. Use
                                   this to parameterize the file contents.
                   FormIdent     - The FormIdent parameter is the name of
                                   the form. Use this to parameterize the
                                   file contents.
                   AncestorIdent - The AncestorIdent parameter is the name
                                   of the ancestor form. Use this to
                                   parameterize the file contents
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewIntfSource returns the source code for the new modules
                   interface or 0 for a default header.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;


//=== { TJvPluginDLLModuleCreator } =============================================

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.FormCreated
  Author         : Stefaan Lesage
  Arguments      : FormEditor - Interface to the IOTAFormEditor for the form
                                that has been created.
  Return Values  : None
  Exceptions     : None
  Description    : This method will be executed Called the new
                   form/datamodule/custom module is created. 
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

procedure TJvPluginDLLModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  // Nothing to do
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetAncestorName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : The name of the Ancestor.
  Exceptions     : None
  Description    : Property Getter for the AncestorName property
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetCreatorType
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the type of the creator as a string.  In our case
                   it returns sUnit since we should create the main DLL entry
                   point in a separate file.
  Exceptions     : None
  Description    : Property Getter for the CreatorType property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetCreatorType: string;
begin
  Result := sText;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetExisting
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns a Boolean indicating if this is an existing
                   module.  We return False since this is a new Module.
  Exceptions     : None
  Description    : Property Getter for the Existing property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetFileSystem
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Return the File system IDString that this module uses for
                   reading/writing.  We return an empty string since our module
                   doesn't use a virtual file system.
  Exceptions     : None
  Description    : Property Getter for the FileSystem property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetFormName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the name of the form ( not the class, but its
                   actual name property ).  We will make sure that each
                   plugin module always gets the Plugin prefix before its
                   name.
  Exceptions     : None
  Description    : Property getter for the FormName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetFormName: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetImplFileName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the complete path to the implementation ( source )
                   file name, e.g. C:\dir\Unit1.pas. If GetUnnamed returns
                   True, the file name is just a placeholder, and the user
                   will be prompted for a file name when the file is saved.
  Exceptions     : None
  Description    : Property getter for the ImplFileName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetImplFileName: string;
var
  sTmp: string;
begin
  sTmp := '';
  if Wizard <> nil then
  begin
    if Wizard.IsCBuilder then
      sTmp := GetCurrentDir + DirDelimiter + 'DllMainUnit.cpp';
  end;
  Result := sTmp;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetIntfFileName
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Return the interface filename, or blank to have the IDE
                   create a new unique one.
  Exceptions     : None
  Description    : Property getter for the IntfFileName property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetMainForm
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if the newly created form is to be the
                   applications main form. It returns False if the form
                   is not necessarily the main form.
  Exceptions     : None
  Description    : Property getter for the MainForm property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetOwner
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns the module interface of the new modules owner,
                   that is, it returns the project interface for a new source
                   file or the project group interface for a new project.
                   You can create a module that does not have an owner by
                   returning 0.
                   In our case we will return the Active ProjectGroup's
                   Active Project.
  Exceptions     : None
  Description    : Property getter for the Owner property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetOwner: IOTAModule;
begin
  Result := Project;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetShowForm
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if you want the IDE to show the form editor
                   after the form is created. Have GetShowForm returns False
                   to keep the form hidden.
  Exceptions     : None
  Description    : Property getter for the ShowForm property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetShowSource
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if you want the IDE to show the source file
                   in the source editor. Have GetShowSource return False to
                   keep the source file hidden.
  Exceptions     : None
  Description    : Property getter for the ShowSource property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.GetUnnamed
  Author         : Stefaan Lesage
  Arguments      : None
  Return Values  : Returns True if the new module has not been saved to a
                   file and therefore does not have a file name yet. If the
                   user saves the module, the user will be prompted for a
                   file name. GetUnnamed returns False if the module has a
                   file name.
  Exceptions     : None
  Description    : Property Getter for the Unnamed property.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.NewFormFile
  Author         : Stefaan Lesage
  Arguments      : FormIdent     - The name of the form. Use this to
                                   parameterize the form description.
                   AncestorIdent - The name of the ancestor form. Use this to
                                   parameterize the form description.
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewFormFile returns the new form description or 0 to use
                   the default form description. The form description must be
                   binary or text form resource.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.NewImplSource
  Author         : Stefaan Lesage
  Arguments      : ModuleIdent   - The ModuleIdent parameter is the name of
                                   the unit or module, e.g., Unit1. Use
                                   this to parameterize the file contents.
                   FormIdent     - The FormIdent parameter is the name of
                                   the form. Use this to parameterize the
                                   file contents.
                   AncestorIdent - The AncestorIdent parameter is the name
                                   of the ancestor form. Use this to
                                   parameterize the file contents
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewImplSource returns the source code for the new
                   modules implementation or 0 for a default unit.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    if Wizard.IsCBuilder then
    begin
      if PlugType = ptDLL then
      begin
        Lines.Add(PLUGIN_DLL_TEMPLATE_BCB);
        Result := TJvOTAFile.Create(Lines.Text, -1)
      end
      else
        Result := nil
    end
    else
      Result := nil;

  finally
    Lines.Free;
  end;
end;

{*****************************************************************************
  Name           : TJvPluginDLLModuleCreator.NewIntfSource
  Author         : Stefaan Lesage
  Arguments      : ModuleIdent   - The ModuleIdent parameter is the name of
                                   the unit or module, e.g., Unit1. Use
                                   this to parameterize the file contents.
                   FormIdent     - The FormIdent parameter is the name of
                                   the form. Use this to parameterize the
                                   file contents.
                   AncestorIdent - The AncestorIdent parameter is the name
                                   of the ancestor form. Use this to
                                   parameterize the file contents
  Return Values  : Returns an instance of a file creator class that you must
                   write, deriving from IOTAFile. If you return 0,
                   a default form will be created.

  Exceptions     : None
  Description    : NewIntfSource returns the source code for the new modules
                   interface or 0 for a default header.
  History        :

  Date         By                   Description
  ----         --                   -----------
  11/07/2003   slesage              Initial creation of the Method.
 *****************************************************************************}

function TJvPluginDLLModuleCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;


initialization
  RegisterContainerModule;

end.
