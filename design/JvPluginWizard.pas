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
located at http://jvcl.sourceforge.net

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

type
  TJvPluginWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    {$IFDEF COMPILER6_UP} IOTARepositoryWizard60, {$ENDIF COMPILER6_UP}
    {$IFDEF COMPILER8_UP} IOTARepositoryWizard80, {$ENDIF COMPILER8_UP}
    {$IFDEF COMPILER10_UP} IOTAProjectWizard100, {$ENDIF COMPILER10_UP}
    IOTAMenuWizard, IOTAProjectWizard)
  public
    PluginMainMenu: IOTAComponent;

    { IOTAWizard Methods }
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
    procedure Execute; virtual;

    { IOTARepositoryWizard Methods }
    function GetAuthor: string; virtual;
    function GetComment: string; virtual;
    function GetPage: string; virtual;
    function GetGlyph: {$IFDEF COMPILER6_UP} Cardinal; {$ELSE} HICON; {$ENDIF} virtual;

    { IOTAMenuWizard methods }
    function GetMenuText: string; virtual;

    {$IFDEF COMPILER6_UP}
    { IOTARepositoryWizard60 }
    function GetDesigner: string;
    {$ENDIF COMPILER6_UP}

    {$IFDEF COMPILER8_UP}
    { IOTARepositoryWizard80 }
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
    {$ENDIF COMPILER8_UP}

    {$IFDEF COMPILER10_UP}
    { IOTAProjectWizard100 }
    function IsVisible(Project: IOTAProject): Boolean;
    {$ENDIF COMPILER10_UP}
  end;

  TJvPlugType = ( ptPackage, ptDLL );

  TJvPluginProjectCreator = class(TInterfacedObject, IOTACreator,
    {$IFDEF COMPILER5_UP} IOTAProjectCreator50, {$ENDIF COMPILER5_UP}
    {$IFDEF COMPILER8_UP} IOTAProjectCreator80, {$ENDIF COMPILER8_UP}
    IOTAProjectCreator)
  public
    Wizard: TJvPluginWizard;
    PlugType: TJvPlugType;
    { Private variables which will be used to store some properties for
      the TJvPlugin }
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
    function GetProjectPersonality: string;
  end;

  TJvPluginModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  public
    Wizard: TJvPluginWizard;
    PlugType: TJvPlugType;
    Project: IOTAModule;
    { Private variables which will be used to store some properties for
      the TJvPlugin }
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
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  JclFileUtils, JclBorlandTools,
  JvPlugin, JvPluginParamsForm, JvConsts, JvDsgnConsts;

{$R JvPluginWiz.res}

const
  CrLf = sLineBreak;
  CrLf2 = CrLf + CrLf;
  cPlgPrefix = 'Plg';
  cPluginPrefix = 'Plugin';

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

function TJvPluginWizard.GetIDString: string;
begin
  Result := RsPluginWizardIDString;
end;

function TJvPluginWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TJvPluginWizard.GetMenuText: string;
begin
  Result := RsJediPluginWizard;
end;

function TJvPluginWizard.GetName: string;
begin
  Result := RsJvPluginWizard;
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
      {$IFDEF BCB5} 5 {$ENDIF}
      ].Repository;
    {$ENDIF BCB}
    {$IFDEF DELPHI}
    Repository := Installations.DelphiInstallationFromVersion[
      {$IFDEF DELPHI11} 11 {$ENDIF}
      {$IFDEF DELPHI10} 10 {$ENDIF}
      {$IFDEF DELPHI9}   9 {$ENDIF}
      {$IFDEF DELPHI7}   7 {$ENDIF}
      {$IFDEF DELPHI6}   6 {$ENDIF}
      {$IFDEF DELPHI5}   5 {$ENDIF}
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

function TJvPluginWizard.GetGlyph: {$IFDEF COMPILER6_UP} Cardinal; {$ELSE} HICON; {$ENDIF}
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

{$IFDEF COMPILER6_UP}
function TJvPluginWizard.GetDesigner: string;
begin
  Result := dVCL;
end;
{$ENDIF COMPILER6_UP}

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
begin
  if PlugType = ptDLL then
    Result := GetCurrentDir + DirDelimiter + cPlgPrefix + PlugName + '.dpr'
  else // PlugType = ptPackage
    Result := GetCurrentDir + DirDelimiter + cPlgPrefix + PlugName + '.dpk';
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
begin
  Result := nil;
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
    end;

    Result := TJvOTAFile.Create(Lines.Text, -1);
  finally
    Lines.Free;
  end;
end;

function TJvPluginProjectCreator.GetCreatorType: string;
begin
  if PlugType = ptDLL then
    Result := sLibrary
  else // PlugType = ptPackage
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
begin
  // add requires
  if PlugType = ptPackage then
  begin
    {$IFDEF COMPILER5}
    // added by default
    //Project.AddFile('vcl50.dcp', False);
    Project.AddFile('JvCoreD5R.dcp', False);
    //{$ELSE ~COMPILER5}
    // added by default
    //Project.AddFile('vcl.dcp', False);
    {$ENDIF COMPILER5}
    {$IFDEF COMPILER6}
    Project.AddFile('JvCoreD6R.dcp', False);
    {$ENDIF COMPILER6}
    {$IFDEF COMPILER7}
    Project.AddFile('JvCoreD7R.dcp', False);
    {$ENDIF COMPILER7}
    {$IFDEF COMPILER9}
    Project.AddFile('JvCoreD9R.dcp', False);
    {$ENDIF COMPILER9}
    {$IFDEF COMPILER10}
    Project.AddFile('JvCoreD10R.dcp', False);
    {$ENDIF COMPILER10}
    {$IFDEF COMPILER11}
    Project.AddFile('JvCoreD11R.dcp', False);
    {$ENDIF COMPILER11}
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
end;

function TJvPluginProjectCreator.GetProjectPersonality: string;
begin
  {$IFDEF COMPILER8_UP}
  Result := sDelphiPersonality;
  {$ELSE}
  Result := '';
  {$ENDIF COMPILER8_UP}
end;

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
  Plugin := INTAComponent(FormEditor.GetRootComponent).GetComponent as TJvPlugin;
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
  Result := 'JvPlugin';
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
begin
  Result := GetCurrentDir + DirDelimiter + cPluginPrefix + PlugName + '.pas'
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
    Lines.Add('function RegisterPlugin: TJvPlugin; stdcall;');
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
    Lines.Add('function RegisterPlugin: TJvPlugin;');
    Lines.Add('begin');
    Lines.Add('  Result := ' + ClassNameOfPlugin + '.Create(nil);');
    //    '  Result := T' + TypeName + '.Create(nil);' + CrLf +
    Lines.Add('end;');
    Lines.Add('');
    Lines.Add('end.');
  
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

initialization
  RegisterContainerModule;

end.

