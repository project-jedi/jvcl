{:jvBandObject wizard.  }
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvBandObjectDLLWizard.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs@ctzen.com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2001-mm-dd

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit jvBandObjectDLLWizard;

interface

uses
  ToolsApi, Windows;

type
  TjvBandObjectDLLWizard = class(TInterfacedObject,
      IOTANotifier,
      IOTAWizard,
      IOTARepositoryWizard,
      IOTAProjectWizard)
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
    {$IFDEF COMPILER6_Up}
    function GetGlyph: Cardinal;
    {$ELSE}
    function GetGlyph: HICON;
    {$ENDIF}

  end;

  TjvBandObjectProjectCreator = class(TInterfacedObject,
      IOTACreator,
      IOTAProjectCreator)
  public
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
  end;

  TjvBandObjectProjectSource = class(TInterfacedObject,
      IOTAFile)
  private
    FProjectName: string;
  public
    constructor Create(const ProjectName: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TjvBandType = (zbtInfoBand, zbtCommBand, zbtToolBand, zbtDeskBand);

  TjvBandObjectModuleCreator = class(TInterfacedObject,
      IOTACreator,
      IOTAModuleCreator)
  private
    FBandName: string;
    FBandDesc: string;
    FBandType: TjvBandType;
  public
    constructor Create(const BandName, BandDesc: string;
      const BandType: TjvBandType);
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

  TjvBandObjectModuleSource = class(TInterfacedObject,
      IOTAFile)
  private
    FBandName: string;
    FBandDesc: string;
    FBandType: TjvBandType;
    FModuleIdent: string;
    FFormIdent: string;
    FAncestorIdent: string;
  public
    constructor Create(const BandName, BandDesc: string;
      const BandType: TjvBandType;
      const ModuleIdent, FormIdent, AncestorIdent: string);
    // IOTAFile
    function GetSource: string;
    function GetAge: TDateTime;
  end;

implementation

uses
  SysUtils, Forms, Controls, ComObj,
  jvBandObjectDLLWizardForm;

const
  CRLF = #13#10;
  CRLF2 = #13#10#13#10;
  BAND_TYPE_DESC: array[0..3] of string = ('Info', 'Comm', 'Tool', 'Desk');

var
  ProjectModule: IOTAModule;

  { TjvBandObjectDLLWizard }

procedure TjvBandObjectDLLWizard.AfterSave;
begin
end;

procedure TjvBandObjectDLLWizard.BeforeSave;
begin
end;

procedure TjvBandObjectDLLWizard.Destroyed;
begin
end;

procedure TjvBandObjectDLLWizard.Execute;
begin
  with TzWizardForm.Create(Application) do
  try
    if ShowModal <> mrOK then
      Exit;
    with (BorlandIDEServices as IOTAModuleServices) do
    begin
      ProjectModule := CreateModule(TjvBandObjectProjectCreator.Create);
      CreateModule(TjvBandObjectModuleCreator.Create(EditBandName.Text,
        EditBandDesc.Text, TjvBandType(RgBandType.ItemIndex)));
    end;
  finally
    Free;
  end;
end;

function TjvBandObjectDLLWizard.GetAuthor: string;
begin
  Result := 'Chiang Seng Chang <cs@ctzen.com>';
end;

function TjvBandObjectDLLWizard.GetComment: string;
begin
  Result := 'Creates a Band Object DLL Project.';
end;

{$IFDEF COMPILER6_Up}

function TjvBandObjectDLLWizard.GetGlyph: Cardinal;
{$ELSE}

function TjvBandObjectDLLWizard.GetGlyph: HICON;
{$ENDIF}
begin
  Result := 0;
end;

function TjvBandObjectDLLWizard.GetIDString: string;
begin
  Result := 'ctZen.jvBandObjectDLLWizard';
end;

function TjvBandObjectDLLWizard.GetName: string;
begin
  Result := 'Band Object DLL Wizard';
end;

function TjvBandObjectDLLWizard.GetPage: string;
begin
  Result := '';
end;

function TjvBandObjectDLLWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TjvBandObjectDLLWizard.Modified;
begin
end;

{ TjvBandObjectProjectCreator }

function TjvBandObjectProjectCreator.GetCreatorType: string;
begin
  Result := '';
end;

function TjvBandObjectProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TjvBandObjectProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TjvBandObjectProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TjvBandObjectProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TjvBandObjectProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TjvBandObjectProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TjvBandObjectProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TjvBandObjectProjectCreator.NewDefaultModule;
begin
end;

function TjvBandObjectProjectCreator.NewOptionSource(
  const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TjvBandObjectProjectCreator.NewProjectResource(
  const Project: IOTAProject);
begin
end;

function TjvBandObjectProjectCreator.NewProjectSource(
  const ProjectName: string): IOTAFile;
begin
  Result := TjvBandObjectProjectSource.Create(ProjectName);
end;

{ TjvBandObjectProjectSource }

constructor TjvBandObjectProjectSource.Create(const ProjectName: string);
begin
  inherited Create;
  FProjectName := ProjectName;
end;

function TjvBandObjectProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TjvBandObjectProjectSource.GetSource: string;
const
  SOURCE =
    'library %s;' + CRLF2 +
    'uses' + CRLF +
    '  ComServ;' + CRLF2 +
    'exports' + CRLF +
    '  DllGetClassObject,' + CRLF +
    '  DllCanUnloadNow,' + CRLF +
    '  DllRegisterServer,' + CRLF +
    '  DllUnregisterServer;' + CRLF2 +
    '{$R *.RES}' + CRLF2 +
    'begin' + CRLF +
    'end.';
begin
  Result := Format(SOURCE, [FProjectName]);
end;

{ TjvBandObjectModuleCreator }

constructor TjvBandObjectModuleCreator.Create(const BandName, BandDesc: string;
  const BandType: TjvBandType);
begin
  inherited Create;
  FBandName := BandName;
  FBandDesc := BandDesc;
  FBandType := BandType;
end;

procedure TjvBandObjectModuleCreator.FormCreated(
  const FormEditor: IOTAFormEditor);
begin
end;

function TjvBandObjectModuleCreator.GetAncestorName: string;
begin
  Result := 'jvBandForm';
end;

function TjvBandObjectModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TjvBandObjectModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TjvBandObjectModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TjvBandObjectModuleCreator.GetFormName: string;
begin
  Result := FBandName + 'Form';
end;

function TjvBandObjectModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TjvBandObjectModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TjvBandObjectModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TjvBandObjectModuleCreator.GetOwner: IOTAModule;
begin
  Result := ProjectModule;
end;

function TjvBandObjectModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TjvBandObjectModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TjvBandObjectModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TjvBandObjectModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TjvBandObjectModuleCreator.NewImplSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TjvBandObjectModuleSource.Create(FBandName, FBandDesc, FBandType,
    ModuleIdent, FormIdent, AncestorIdent);
end;

function TjvBandObjectModuleCreator.NewIntfSource(const ModuleIdent,
  FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TjvBandObjectModuleSource }

constructor TjvBandObjectModuleSource.Create(const BandName, BandDesc: string;
  const BandType: TjvBandType; const ModuleIdent, FormIdent, AncestorIdent: string);
begin
  inherited Create;
  FBandName := BandName;
  FBandDesc := BandDesc;
  FBandType := BandType;
  FModuleIdent := ModuleIdent;
  FFormIdent := FormIdent;
  FAncestorIdent := AncestorIdent;
end;

function TjvBandObjectModuleSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TjvBandObjectModuleSource.GetSource: string;
const
  SOURCE =
    'unit %3:s;' + CRLF2 +
    'interface' + CRLF2 +
    'uses' + CRLF +
    '  Windows,' + CRLF +
    '  jvBandObject, jvBandForms;' + CRLF2 +
    'type' + CRLF +
    '  // Band Object Factory' + CRLF +
    '  T%0:sFactory = class(Tz%2:sBandObjectFactory)' + CRLF +
    '  end;' + CRLF2 +
    '  // Band Object' + CRLF +
    '  T%0:s = class(Tz%2:sBandObject)' + CRLF +
    '  protected' + CRLF +
    '    function CreateBandForm(const ParentWnd: HWnd): TjvBandForm; override;' + CRLF +
    '  end;' + CRLF2 +
    '  // Band Form' + CRLF +
    '  T%4:s = class(T%5:s)' + CRLF +
    '  private' + CRLF +
    '    function GetBandObject: T%0:s;' + CRLF +
    '  public' + CRLF +
    '    property BandObject: T%0:s read GetBandObject;' + CRLF +
    '  end;' + CRLF2 +
    'const' + CRLF +
    '  Class_%0:s: TGUID = ''%6:s'';' + CRLF2 +
    'implementation' + CRLF2 +
    '{$R *.DFM}' + CRLF2 +
    'uses' + CRLF +
    '  ComObj, ComServ;' + CRLF2 +
    '{ T%0:s }' + CRLF2 +
    'function T%0:s.CreateBandForm(const ParentWnd: HWnd): TjvBandForm;' + CRLF +
    'begin' + CRLF +
    '  Result := T%4:s.CreateBandForm(ParentWnd, Self);' + CRLF +
    'end;' + CRLF2 +
    '{ T%4:s }' + CRLF2 +
    'function T%4:s.GetBandObject: T%0:s;' + CRLF +
    'begin' + CRLF +
    '  Result := T%0:s(_BandObject);' + CRLF +
    'end;' + CRLF2 +
    'initialization' + CRLF +
    '  T%0:sFactory.Create(ComServer, T%0:s, Class_%0:s,' + CRLF +
    '    ''%0:s'', ''%1:s'', ciMultiInstance, tmApartment);' + CRLF2 +
    'end.';

begin
  Result := Format(SOURCE, [FBandName, FBandDesc, BAND_TYPE_DESC[Ord(FBandType)],
    FModuleIdent, FFormIdent, FAncestorIdent,
      CreateClassID]);
end;

end.

