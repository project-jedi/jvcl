{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvBandObjectDLLWizard.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs att ctzen dott com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBandObjectDLLWizard;

{$I jvcl.inc}

interface

uses
  Windows, ToolsAPI;

type
  TJvBandType = (zbtInfoBand, zbtCommBand, zbtToolBand, zbtDeskBand);

  TJvBandObjectDLLWizard = class(TInterfacedObject,
    IOTANotifier, IOTAWizard, IOTARepositoryWizard, IOTAProjectWizard)
  public
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;
  end;

  TJvBandObjectProjectCreator = class(TInterfacedObject,
    IOTACreator, IOTAProjectCreator, IOTAProjectCreator50
    {$IFDEF COMPILER8_UP}, IOTAProjectCreator80{$ENDIF COMPILER8_UP})
  private
    FOwner: IOTAProjectGroup;
    FBandName: string;
    FBandDesc: string;
    FBandType: TJvBandType;
  public
    constructor Create(const AOwner: IOTAProjectGroup; const ABandName: string;
      const ABandDesc: string; ABandType: TJvBandType);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAProjectCreator
    function GetFileName: string;
    function GetOptionFileName: string;
    function GetShowSource: Boolean;
    procedure NewDefaultModule;
    function NewOptionSource(const ProjectName: string): IOTAFile;
    procedure NewProjectResource(const Project: IOTAProject);
    function NewProjectSource(const ProjectName: string): IOTAFile;
    // IOTAProjectCreator50
    procedure NewDefaultProjectModule(const Project: IOTAProject);
    {$IFDEF COMPILER8_UP}
    // IOTAProjectCreator80
    function GetProjectPersonality: string;
    {$ENDIF COMPILER8_UP}
  end;

  TJvBandObjectProjectSource = class(TInterfacedObject, IOTAFile)
  private
    FProjectName: string;
  public
    constructor Create(const ProjectName: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TJvBandObjectModuleCreator = class(TInterfacedObject,
    IOTACreator, IOTAModuleCreator)
  private
    FOwner: IOTAProject;
    FBandName: string;
    FBandDesc: string;
    FBandType: TJvBandType;
  public
    constructor Create(const AOwner: IOTAProject; const BandName, BandDesc: string;
      const BandType: TJvBandType);
    // IOTACreator
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
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

  TJvBandObjectModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FBandName: string;
    FBandDesc: string;
    FBandType: TJvBandType;
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  public
    constructor Create(const BandName, BandDesc: string;
      const BandType: TJvBandType;
      const ModuleIdent, FormIdent, AncestorIdent: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;


implementation

uses
  SysUtils, Forms, Controls, ComObj,
  JvBandObjectDLLWizardForm, JvConsts, JvDsgnConsts;

const
  CrLf2 = #13#10#13#10;
  BAND_TYPE_DESC: array [0..3] of PChar = ('Info', 'Comm', 'Tool', 'Desk');

//=== { TJvBandObjectDLLWizard } =============================================

procedure TJvBandObjectDLLWizard.AfterSave;
begin
end;

procedure TJvBandObjectDLLWizard.BeforeSave;
begin
end;

procedure TJvBandObjectDLLWizard.Destroyed;
begin
end;

function GetActiveProjectGroup(const ModuleServices: IOTAModuleServices): IOTAProjectGroup;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ModuleServices.ModuleCount - 1 do
    if Supports(ModuleServices.Modules[I], IOTAProjectGroup, Result) then
      Break;
end;

procedure TJvBandObjectDLLWizard.Execute;
var
  WizardForm: TzWizardForm;
  ModuleServices: IOTAModuleServices;
  ProjectGroup: IOTAProjectGroup;
begin
  WizardForm := TzWizardForm.Create(Application);
  try
    if WizardForm.ShowModal <> mrOk then
      Exit;
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    ProjectGroup := GetActiveProjectGroup(ModuleServices);
    ModuleServices.CreateModule(TJvBandObjectProjectCreator.Create(ProjectGroup,
      WizardForm.EditBandName.Text, WizardForm.EditBandDesc.Text,
      TJvBandType(WizardForm.RgBandType.ItemIndex)));
  finally
    WizardForm.Free;
  end;
end;

function TJvBandObjectDLLWizard.GetAuthor: string;
begin
  Result := 'Chiang Seng Chang <cs@ctzen.com>';
end;

function TJvBandObjectDLLWizard.GetComment: string;
begin
  Result := RsCreatesABandObjectDLLProject;
end;

function TJvBandObjectDLLWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TJvBandObjectDLLWizard.GetIDString: string;
begin
  Result := RsBandObjectDLLWizardIDString;
end;

function TJvBandObjectDLLWizard.GetName: string;
begin
  Result := RsBandObjectDLLWizard;
end;

function TJvBandObjectDLLWizard.GetPage: string;
begin
  Result := '';
end;

function TJvBandObjectDLLWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TJvBandObjectDLLWizard.Modified;
begin
end;

//=== { TJvBandObjectProjectCreator } ========================================

constructor TJvBandObjectProjectCreator.Create(const AOwner: IOTAProjectGroup;
  const ABandName: string; const ABandDesc: string; ABandType: TJvBandType);
begin
  inherited Create;
  FOwner := AOwner;
  FBandName := ABandName;
  FBandDesc := ABandDesc;
  FBandType := ABandType;
end;

function TJvBandObjectProjectCreator.GetCreatorType: string;
begin
  Result := sLibrary;
end;

function TJvBandObjectProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJvBandObjectProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TJvBandObjectProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TJvBandObjectProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TJvBandObjectProjectCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

{$IFDEF COMPILER8_UP}
function TJvBandObjectProjectCreator.GetProjectPersonality: string;
begin
  Result := sDelphiPersonality;
end;
{$ENDIF COMPILER8_UP}

function TJvBandObjectProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TJvBandObjectProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TJvBandObjectProjectCreator.NewDefaultModule;
begin
end;

procedure TJvBandObjectProjectCreator.NewDefaultProjectModule(
  const Project: IOTAProject);
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  ModuleServices.CreateModule(TJvBandObjectModuleCreator.Create(Project, FBandName, FBandDesc, FBandType));
end;

function TJvBandObjectProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TJvBandObjectProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
end;

function TJvBandObjectProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TJvBandObjectProjectSource.Create(ProjectName);
end;

//=== { TJvBandObjectProjectSource } =========================================

constructor TJvBandObjectProjectSource.Create(const ProjectName: string);
begin
  inherited Create;
  FProjectName := ProjectName;
end;

function TJvBandObjectProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TJvBandObjectProjectSource.GetSource: string;
begin
  Result :=
    'library ' + FProjectName + ';' + CrLf2 +

    'uses' + CrLf +
    '  ComServ;' + CrLf2 +

    'exports' + CrLf +
    '  DllGetClassObject,' + CrLf +
    '  DllCanUnloadNow,' + CrLf +
    '  DllRegisterServer,' + CrLf +
    '  DllUnregisterServer;' + CrLf2 +

    '{$R *.res}' + CrLf2 +

    'begin' + CrLf +
    'end.';
end;

//=== { TJvBandObjectModuleCreator } =========================================

constructor TJvBandObjectModuleCreator.Create(const AOwner: IOTAProject;
  const BandName, BandDesc: string; const BandType: TJvBandType);
begin
  inherited Create;
  FOwner := AOwner;
  FBandName := BandName;
  FBandDesc := BandDesc;
  FBandType := BandType;
end;

procedure TJvBandObjectModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TJvBandObjectModuleCreator.GetAncestorName: string;
begin
  Result := 'JvBandForm';
end;

function TJvBandObjectModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TJvBandObjectModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TJvBandObjectModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TJvBandObjectModuleCreator.GetFormName: string;
begin
  Result := FBandName + 'Form';
end;

function TJvBandObjectModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TJvBandObjectModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TJvBandObjectModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TJvBandObjectModuleCreator.GetOwner: IOTAModule;
begin
  Result := FOwner;
end;

function TJvBandObjectModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TJvBandObjectModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TJvBandObjectModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TJvBandObjectModuleCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TJvBandObjectModuleCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TJvBandObjectModuleSource.Create(FBandName, FBandDesc, FBandType,
    ModuleIdent, FormIdent, AncestorIdent);
end;

function TJvBandObjectModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

//=== { TJvBandObjectModuleSource } ==========================================

constructor TJvBandObjectModuleSource.Create(const BandName, BandDesc: string;
  const BandType: TJvBandType; const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FBandName := BandName;
  FBandDesc := BandDesc;
  FBandType := BandType;
  FModuleIdent := ModuleIdent;
  FFormIdent := FormIdent;
  FAncestorIdent := AncestorIdent;
end;

function TJvBandObjectModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TJvBandObjectModuleSource.GetSource: string;
const
  cSource =
    'unit %3:s;' + CrLf2 +

    'interface' + CrLf2 +

    'uses' + CrLf +
    '  Windows,' + CrLf +
    '  JvBandObject, JvBandForms;' + CrLf2 +

    'type' + CrLf +
    '  // Band Object Factory' + CrLf +
    '  T%0:sFactory = class(Tz%2:sBandObjectFactory)' + CrLf +
    '  end;' + CrLf2 +
    '  // Band Object' + CrLf +
    '  T%0:s = class(Tz%2:sBandObject)' + CrLf +
    '  protected' + CrLf +
    '    function CreateBandForm(const ParentWnd: THandle): TJvBandForm; override;' + CrLf +
    '  end;' + CrLf2 +

    '  // Band Form' + CrLf +
    '  T%4:s = class(T%5:s)' + CrLf +
    '  private' + CrLf +
    '    function GetBandObject: T%0:s;' + CrLf +
    '  public' + CrLf +
    '    property BandObject: T%0:s read GetBandObject;' + CrLf +
    '  end;' + CrLf2 +

    'const' + CrLf +
    '  Class_%0:s: TGUID = ''%6:s'';' + CrLf2 +

    'implementation' + CrLf2 +

    '{$R *.dfm}' + CrLf2 +

    'uses' + CrLf +
    '  ComObj, ComServ;' + CrLf2 +

    '{ T%0:s }' + CrLf2 +

    'function T%0:s.CreateBandForm(const ParentWnd: THandle): TJvBandForm;' + CrLf +
    'begin' + CrLf +
    '  Result := T%4:s.CreateBandForm(ParentWnd, Self);' + CrLf +
    'end;' + CrLf2 +

    '{ T%4:s }' + CrLf2 +

    'function T%4:s.GetBandObject: T%0:s;' + CrLf +
    'begin' + CrLf +
    '  Result := T%0:s(_BandObject);' + CrLf +
    'end;' + CrLf2 +

    'initialization' + CrLf +
    '  T%0:sFactory.Create(ComServer, T%0:s, Class_%0:s,' + CrLf +
    '    ''%0:s'', ''%1:s'', ciMultiInstance, tmApartment);' + CrLf2 +

    'end.';
begin
  Result := Format(cSource, [FBandName, FBandDesc, BAND_TYPE_DESC[Ord(FBandType)],
    FModuleIdent, FFormIdent, FAncestorIdent, CreateClassID]);
end;

end.

