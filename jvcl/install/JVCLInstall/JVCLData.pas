{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCLData.pas, released on 2004-03-29.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JVCLData;

interface

uses
  Windows, Registry, SysUtils, Classes, Contnrs,
  JVCLConfiguration, DelphiData, PackageUtils, Intf, GenerateUtils,
  IniFiles;

const
  PackageGeneratorFile = 'devtools\bin\pgEdit.xml';

type
  TJVCLData = class;
  TTargetConfig = class;

  TInstallMode = set of TPackageGroupKind;

  TDeinstallProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer) of object;
  TDeleteFilesEvent = procedure(TargetConfig: TTargetConfig) of object;

  TTargetConfig = class(TComponent, ITargetConfig) // TComponent <-> TInterfacedObject
  private
    FOwner: TJVCLData;
    FTarget: TCompileTarget;
    FInstalledJVCLVersion: Integer;
    FJCLDir: string;
    FMissingJCL: Boolean;
    FCompiledJCL: Boolean;
    FInstallJVCL: Boolean;
    FHppDir: string;

    FDeveloperInstall: Boolean;
    FCleanPalettes: Boolean;
    FBuild: Boolean;
    FCompileOnly: Boolean;
    FDebugUnits: Boolean;
    FAutoDependencies: Boolean;

    FInstallMode: TInstallMode;
    FFrameworks: TJVCLFrameworks;
    FDcpDir: string;
    FBplDir: string;

    procedure SetInstallMode(const Value: TInstallMode);
    function GetFrameworkCount: Integer;
    function GetDxgettextDir: string;
  private
    { ITargetConfig }
    function GetInstance: TObject;
    function GetJVCLPackagesXmlDir: string;
    function GetJVCLDir: string;
    function GetJVCLPackagesDir: string;

    function GetTargetSymbol: string;
    function GetAutoDependencies: Boolean;
    function GetBuild: Boolean;
    function GetUnitOutDir: string;
    function GetDebugUnits: Boolean;
    function GetCompileOnly: Boolean;

    function GetTarget: TCompileTarget;
    function GetJCLDir: string;
    function GetHppDir: string;
    function GetBplDir: string;
    function GetDcpDir: string;
  protected
    procedure Init; virtual;
    procedure DoCleanPalette(reg: TRegistry; const Name: string;
      RemoveEmptyPalettes: Boolean);
    function RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup): Boolean;
  public
    property Target: TCompileTarget read GetTarget;
    property Owner: TJVCLData read FOwner;
  public
    constructor Create(AOwner: TJVCLData; ATarget: TCompileTarget); reintroduce;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
    procedure DeinstallJVCL(Progress: TDeinstallProgressEvent;
      DeleteFiles: TDeleteFilesEvent);
    function RegisterToIDE: Boolean;

    function CanInstallJVCL: Boolean;
      // CanInstallJVCL returns False when the target is not up to date or
      // no JCL was found.

    function IsUpToDate: Boolean;
      // IsUpToDate returns False when the (Borland) updates for this target
      // are not installed.

    function IsOldVersionInstalled: Boolean;
      // IsOldVersionInstalled returns true if an older JVCL version is
      // installed.

    function GetBpgFilename(Personal: Boolean; Kind: TPackageGroupKind): string;
      // BpgFilename returns the filename of the ProjectGroup that is used

    procedure ResetPackagesSettings(ProjectGroup: TProjectGroup);
      // ResetPackagesSettings sets the install property for each package target
      // to its registry setting of the current IDE target.
    procedure SavePackagesSettings(ProjectGroup: TProjectGroup);
      // SavePackagesSettings saves the runtime packages state to an .ini file.

    property TargetSymbol: string read GetTargetSymbol;
      // TargetSymbol returns the symbol that is used in the xml files for this
      // target.

    property UnitOutDir: string read GetUnitOutDir;
      // UnitOutDir specifies the JVCL directory where the .dcu should go.
    property BplDir: string read GetBplDir write FBplDir;
      // BPL directory for this target
    property DcpDir: string read GetDcpDir write FDcpDir;
      // DCP directory for this target

    property DxgettextDir: string read GetDxgettextDir;
      // Directory where dxgettext is installed or ''. (special handling for Delphi/BCb 5)

    property InstalledJVCLVersion: Integer read FInstalledJVCLVersion;
      // InstalledJVCLVersion returns the version of the installed JVCL.

    property MissingJCL: Boolean read FMissingJCL;
      // MissingJCL is True when no JCL is installed and no JCL directoy was
      // found that could be installed.

    property CompiledJCL: Boolean read FCompiledJCL;
      // CompiledJCL is True when D/CJcl.dcp and D/CJclVcl.dcp exist for this
      // target.

    property Frameworks: TJVCLFrameworks read FFrameworks;
      // Frameworks contains all possible package groups.

    property FrameworkCount: Integer read GetFrameworkCount;
      // FrameworkCount returns the number of available frameworks for this
      // target.
  public
    property InstallJVCL: Boolean read FInstallJVCL write FInstallJVCL;
      // InstallJVCL specifies if the JVCL should be installed on this target.

    property InstallMode: TInstallMode read FInstallMode write SetInstallMode;
      // InstallMode specifies if the JVCL only,  JVCL and JVCLX or JVCLX only
      // should be installed.

    property DebugUnits: Boolean read GetDebugUnits write FDebugUnits;
      // if DebugUnits is True the units will be compiled in debug mode, too.
      // (Delphi only) [NOT USED IN THE JVCL DUE TO jvcl.inc SETTINGS]

    property Build: Boolean read GetBuild write FBuild;
      // if Build is True the packages are built instead of make.

    property CompileOnly: Boolean read GetCompileOnly write FCompileOnly;
      // if CompileOnly is True the desigtime packages are not registered to the
      // IDE.

    property AutoDependencies: Boolean read GetAutoDependencies write FAutoDependencies;
      // if AutoDependencies it True the make file for the project groups will
      // contain auto dependency information for faster compilation.

    property DeveloperInstall: Boolean read FDeveloperInstall write FDeveloperInstall;
      // DevelopInstall: add the \run directory to the library path.

    property CleanPalettes: Boolean read FCleanPalettes write FCleanPalettes;
      // CleanPalettes specifies if the JVCL components should be removed from
      // the component palettes before installation.

    property JCLDir: string read GetJCLDir write FJCLDir;
      // JCLDir specifies the directory where the JCL is.

    property HppDir: string read GetHppDir write FHppDir;
      // HppDir: (for BCB installation) specifies where the generated .hpp files
      // should go.
  end;

  TJVCLData = class(TObject)
  private
    FConfigs: array of TTargetConfig;
    FTargets: TCompileTargetList;
    FIsDxgettextInstalled: Boolean;
    FDxgettextDir: string;
    FJVCLConfig: TJVCLConfig;
    FJVCLDir: string;
    FDeleteFilesOnUninstall: Boolean;
    FCompileJclDcp: Boolean;

    function GetTargetConfig(Index: Integer): TTargetConfig;
    function GetJVCLDir: string;
    function GetCleanPalettes: Integer;
    procedure SetCleanPalettes(Value: Integer);
    function GetDeveloperInstall: Integer;
    procedure SetDeveloperInstall(Value: Integer);
    function GetJVCLPackagesDir: string;
    function GetJVCLPackagesXmlDir: string;
    function GetBuild: Integer;
    procedure SetBuild(Value: Integer);
    function GetCompileOnly: Integer;
    procedure SetCompileOnly(const Value: Integer);
    function GetOptionState(Index: Integer): Integer;
  protected
    function JvclIncFilename: string;
    procedure Init; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveTargetConfigs;
    function FindTargetConfig(const TargetSymbol: string): TTargetConfig;
    function IsJVCLInstalledAnywhere(MinVersion: Integer): Boolean;

    property DxgettextDir: string read FDxgettextDir;
    property IsDxgettextInstalled: Boolean read FIsDxgettextInstalled;

    property JVCLConfig: TJVCLConfig read FJVCLConfig;
    property JVCLDir: string read GetJVCLDir;
    property JVCLPackagesDir: string read GetJVCLPackagesDir;
    property JVCLPackagesXmlDir: string read GetJVCLPackagesXmlDir;

    property DeveloperInstall: Integer read GetDeveloperInstall write SetDeveloperInstall;
    property CleanPalettes: Integer read GetCleanPalettes write SetCleanPalettes;
    property Build: Integer read GetBuild write SetBuild;
    property CompileOnly: Integer read GetCompileOnly write SetCompileOnly;

    property DeleteFilesOnUninstall: Boolean read FDeleteFilesOnUninstall write FDeleteFilesOnUninstall;
    property CompileJclDcp: Boolean read FCompileJclDcp write FCompileJclDcp;

    property TargetConfig[Index: Integer]: TTargetConfig read GetTargetConfig;
    property Targets: TCompileTargetList read FTargets;
  end;

implementation

uses
  Utils, CmdLineUtils;

resourcestring
  RsComponentPalettePrefix = 'TJv';
  RsNoJVCLFound = 'No JVCL directory found. Application terminated.';
  RsJVCLInstaller = 'JVCL Installer';

  RsCleaningPalette = 'Cleaning Palette...';
  RsCleaningPathLists = 'Cleaning Path lists...';
  RsUnregisteringPackages = 'Unregistering packages...';
  RsDeletingFiles = 'Deleting files...';
  RsComplete = 'Complete.';

const
  sDxgettextRegKey = '\bplfile\Shell\Extract strings\Command';
  sJvclIncFile = '%s\common\jvcl.inc';
  sBCBIncludeDir = '%s\Include\Vcl';


function ReadRegString(RootKey: HKEY; const Key, Name: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKeyReadOnly(Key) then
    begin
      if Reg.ValueExists(Name) then
        Result := Reg.ReadString(Name);
    end;
  finally
    Reg.Free;
  end;
end;

{ TJVCLData }

constructor TJVCLData.Create;
var
  I: Integer;
  ErrMsg: string;
begin
  inherited Create;
  FCompileJclDcp := True; 
  FJVCLConfig := TJVCLConfig.Create;
  FJVCLConfig.LoadFromFile(JvclIncFilename);

  ErrMsg := '';
  LoadConfig(JVCLDir + '\' + PackageGeneratorFile, 'JVCL', ErrMsg);

  FTargets := TCompileTargetList.Create;
  SetLength(FConfigs, Targets.Count);
  for I := 0 to High(FConfigs) do
    FConfigs[I] := TTargetConfig.Create(Self, Targets[I]);
  Init;
end;

destructor TJVCLData.Destroy;
var
  i: Integer;
begin
  FJVCLConfig.Free;
  for i := 0 to High(FConfigs) do
    FConfigs[I].Free;
  FTargets.Free;
  inherited Destroy;
end;

function TJVCLData.FindTargetConfig(const TargetSymbol: string): TTargetConfig;
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
  begin
    Result := TargetConfig[i];
    if CompareText(TargetSymbol, Result.TargetSymbol) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TJVCLData.GetOptionState(Index: Integer): Integer;
var
  i: Integer;
  b: Boolean;
begin
  Result := 0; // false
  for i := 0 to Targets.Count - 1 do
  begin
    if TargetConfig[i].InstallJVCL then
    begin
      case Index of
        0: b := TargetConfig[i].Build;
        1: b := TargetConfig[i].CleanPalettes;
        2: b := TargetConfig[i].CompileOnly;
        3: b := TargetConfig[i].DeveloperInstall;
      else
        b := False;
      end;
      if b then
      begin
        if Result = 3 then
        begin
          Result := 2;
          Exit;
        end;
        Result := 1 // true
      end
      else
      begin
        if Result = 1 then
        begin
          Result := 2; // mixed
          Exit;
        end;
        Result := 3;
      end;
    end;
  end;
  if Result = 3 then
    Result := 0;
end;

function TJVCLData.GetBuild: Integer;
begin
  Result := GetOptionState(0);
end;

function TJVCLData.GetCleanPalettes: Integer;
begin
  Result := GetOptionState(1);
end;

function TJVCLData.GetCompileOnly: Integer;
begin
  Result := GetOptionState(2);
end;

function TJVCLData.GetDeveloperInstall: Integer;
begin
  Result := GetOptionState(3);
end;

function TJVCLData.GetJVCLDir: string;
begin
  if FJVCLDir = '' then
  begin
    FJVCLDir := ExtractFileDir(ParamStr(0));
    while not DirectoryExists(JVCLPackagesDir) do
    begin
      if Length(FJVCLDir) < 4 then
      begin
        MessageBox(0, PChar(RsNoJVCLFound), PChar(RsJVCLInstaller), MB_ICONERROR or MB_OK);
        Halt(1);
        Break;
      end;
      FJVCLDir := ExtractFileDir(JVCLDir);
    end;
  end;
  Result := FJVCLDir;
end;

function TJVCLData.GetJVCLPackagesDir: string;
begin
  Result := JVCLDir + '\packages';
end;

function TJVCLData.GetJVCLPackagesXmlDir: string;
begin
  Result := JVCLPackagesDir + '\xml';
end;

function TJVCLData.GetTargetConfig(Index: Integer): TTargetConfig;
begin
  Result := FConfigs[Index];
end;

procedure TJVCLData.Init;
var
  i: Integer;
  S: string;
begin
  FDeleteFilesOnUninstall := True;

 // dxgettext detection
  S := ReadRegString(HKEY_CLASSES_ROOT, PChar(sDxgettextRegKey), '');
  FIsDxgettextInstalled := S <> '';
  if FIsDxgettextInstalled then
  begin
    if S[1] = '"' then
    begin
      Delete(S, 1, 1);
      i := 1;
      while (i <= Length(S)) and (S[i] <> '"') do
        Inc(i);
      SetLength(S, i - 1);
    end;
    FDxgettextDir := ExtractFileDir(S);
  end;
end;

function TJVCLData.IsJVCLInstalledAnywhere(MinVersion: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Targets.Count - 1 do
    if TargetConfig[i].InstalledJVCLVersion >= MinVersion then
    begin
      Result := True;
      Break;
    end;
end;

function TJVCLData.JvclIncFilename: string;
begin
  Result := Format(sJvclIncFile, [JVCLDir]);
end;

procedure TJVCLData.SaveTargetConfigs;
var
  i: Integer;
begin
  if FJVCLConfig.Modified then
    JVCLConfig.SaveToFile(JVCLConfig.Filename);
  for i := 0 to Targets.Count - 1 do
    TargetConfig[i].Save;
end;

procedure TJVCLData.SetBuild(Value: Integer);
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[I].Build := Value <> 0;
end;

procedure TJVCLData.SetCleanPalettes(Value: Integer);
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[I].CleanPalettes := Value <> 0;
end;

procedure TJVCLData.SetCompileOnly(const Value: Integer);
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[i].CompileOnly := Value <> 0;
end;

procedure TJVCLData.SetDeveloperInstall(Value: Integer);
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[i].DeveloperInstall := Value <> 0;
end;

{ TTargetConfig }

constructor TTargetConfig.Create(AOwner: TJVCLData; ATarget: TCompileTarget);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FTarget := ATarget;

  FInstallMode := [pkVcl];
  FHppDir := Format(sBCBIncludeDir, [Target.RootDir]);
  FCleanPalettes := True;
  FDeveloperInstall := False;
  FAutoDependencies := True;
  FBplDir := Target.BplDir;
  FDcpDir := Target.DcpDir;
  FJCLDir := CmdOptions.JclPath;
  Init;
  FInstallJVCL := CanInstallJVCL;

  FFrameworks := TJVCLFrameworks.Create(Self);
  Load;
end;

destructor TTargetConfig.Destroy;
begin
  FFrameworks.Free;
  inherited Destroy;
end;

procedure TTargetConfig.Init;
 // Memory allocations must go to the constructor because Init could be called
 // more the once.

  function FindJCL(List: TStrings): string;
  var
    i: Integer;
    S: string;
  begin
    Result := '';
    for i := 0 to List.Count - 1 do
    begin
      S := Target.ExpandDirMacros(List[i]);
      if EndsWith(S, '\jcl\source\common', True) then
      begin
        if FileExists(S + '\JclBase.pas') then
        begin
          Result := ExtractFileDir(ExtractFileDir(S));
          FMissingJCL := False;
          Break;
        end;
      end
      else
      if EndsWith(S, '\jcl\lib\', True) then
      begin
        if FileExists(ExtractFileDir(S) + '\source\common\JclBase.pas') then
        begin
          Result := ExtractFileDir(S);
          FMissingJCL := False;
          Break;
        end;
      end;
    end;
  end;

var
  S: string;
begin
  FInstallMode := [];
  FCompiledJCL := False;

 // identify JVCL version
  FInstalledJVCLVersion := 0;
  if Target.FindPackageEx('JvPack1') <> nil then
    FInstalledJVCLVersion := 1
  else if Target.FindPackageEx('jvcl2') <> nil then
    FInstalledJVCLVersion := 2
  else if Target.FindPackageEx('JvCore') <> nil then // VCL
  begin
    Include(FInstallMode, pkVCL);
    FInstalledJVCLVersion := 3;
  end;
  if Target.FindPackageEx('JvQCore') <> nil then // CLX
  begin
    Include(FInstallMode, pkCLX);
    FInstalledJVCLVersion := 3;
  end;
  if FInstallMode = [] then // if no VCL and no CLX that it is CLX
    Include(FInstallMode, pkVCL);

 // identify JCL version
  FMissingJCL := True;
  if FJCLDir = '' then
  begin
    FJCLDir := FindJCL(Target.BrowsingPaths);
    if FJCLDir = '' then
      FJCLDir := FindJCL(Target.SearchPaths);
  end;

  // are C/DJcl.dcp and C/DJclVcl.dcp available
  if Target.Version = 5 then S := '50' else S := '';

  if (Target.IsBCB and
      FileExists(Format('%s\CJcl%s.dcp', [BplDir, S])) and
      FileExists(Format('%s\CJclVcl%s.dcp', [BplDir, S]))) or

     (not Target.IsBCB and
      FileExists(Format('%s\DJcl%s.dcp', [BplDir, S])) and
      FileExists(Format('%s\DJclVcl%s.dcp', [BplDir, S]))) then
  begin
    FCompiledJCL := True;

    { (ahuser) Removed because some files require JCL source }
    {if FJCLDir = '' then // replace JCL directory
      FJCLDir := BplDir;

    if Target.IsBCB then
      FMissingJCL := False
    else
    begin
      // Delphi requires .bpl files
      if FileExists(Format('%s\DJcl%s.bpl', [BplDir, S])) and
         FileExists(Format('%s\DJclVcl%s.bpl', [BplDir, S])) then
        FMissingJCL := False;
    end;}
  end;
end;

function TTargetConfig.CanInstallJVCL: Boolean;
begin
  Result := IsUpToDate and not MissingJCL;
end;

function TTargetConfig.IsUpToDate: Boolean;
begin
  if Target.IsBCB then
  begin
    case Target.Version of
      5: Result := Target.LatestUpdate >= 2;
      6: Result := Target.LatestUpdate >= 1;
    else
     // unknown Delphi/BCB version
      Result := False;
    end;
  end
  else
  begin
    case Target.Version of
      5: Result := Target.LatestUpdate >= 1;
      6: Result := Target.LatestUpdate >= 2;
      7: Result := True;
    else
     // unknown Delphi/BCB version
      Result := False;
    end;
  end;
end;

function TTargetConfig.IsOldVersionInstalled: Boolean;
begin
  Result := InstalledJVCLVersion < 3;
end;

/// <summary>
/// GetBpgFilename returns the file name of the Borland Package Group (.bpg)
/// file that is used for this target.
/// </summary>
function TTargetConfig.GetBpgFilename(Personal: Boolean; Kind: TPackageGroupKind): string;
const
  TargetTypes: array[Boolean] of string = ('D', 'C');
var
  Pers, Clx: string;
begin
  if Personal then
  begin
    if Target.Version <= 5 then
      Pers := 'Std'
    else
      Pers := 'Per';
  end;

  if Kind = pkClx then
    Clx := 'Clx';

  Result := Owner.JVCLPackagesDir + Format('\%s%d%s%s Packages.bpg',
    [TargetTypes[Target.IsBCB], Target.Version, Pers, Clx]);
end;

procedure TTargetConfig.SavePackagesSettings(ProjectGroup: TProjectGroup);
var
  i: Integer;
  Ini: TMemIniFile;
begin
 // save to ini
  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.EraseSection(ProjectGroup.BpgName);
    for i := 0 to ProjectGroup.Count - 1 do
      with ProjectGroup.Packages[i] do
        if not Info.IsDesign then
          Ini.WriteBool(ProjectGroup.BpgName, Info.Name, Compile);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TTargetConfig.ResetPackagesSettings(ProjectGroup: TProjectGroup);
var
  PkgIndex, i: Integer;
  BplName: string;
  IsInstalled: Boolean;
  Ini: TMemIniFile;
begin
 // Set Compile to False for each package.
  for PkgIndex := 0 to ProjectGroup.Count - 1 do
    ProjectGroup.Packages[PkgIndex].Compile := False;

 // read from ini
  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    for i := 0 to ProjectGroup.Count - 1 do
      if not ProjectGroup.Packages[i].Info.IsDesign then
        ProjectGroup.Packages[i].Compile := Ini.ReadBool(ProjectGroup.BpgName, ProjectGroup.Packages[i].Info.Name, False);
  finally
    Ini.Free;
  end;

  IsInstalled := False;
 // Set Install to the registry setting. The dependency check will activate the
 // required (runtime) packages.
  for PkgIndex := 0 to ProjectGroup.Count - 1 do
  begin
    BplName := ProjectGroup.Packages[PkgIndex].Info.BplName;

    for i := 0 to Target.KnownPackages.Count - 1 do
      if CompareText(Target.KnownPackages[i].Name, BplName) = 0 then
      begin
        ProjectGroup.Packages[PkgIndex].Install := True;
        IsInstalled := True;
        Break;
      end;

    for i := 0 to Target.DisabledPackages.Count - 1 do
      if CompareText(Target.DisabledPackages[i].Name, BplName) = 0 then
      begin
        ProjectGroup.Packages[PkgIndex].Install := True;
        IsInstalled := True;
        Break;
      end;
  end;
  if not IsInstalled then
  begin
    // No package of the project group is installed, so it must be a new
    // installation.
    for PkgIndex := 0 to ProjectGroup.Count - 1 do
      ProjectGroup.Packages[PkgIndex].Install := True;
  end;
end;

function TTargetConfig.GetTargetSymbol: string;
const
  TargetTypes: array[Boolean] of string = ('D', 'C');
var
  Pers: string;
begin
  if Target.IsPersonal {or (Kind = pkPersonal)} then
  begin
    if Target.Version = 5 then
      Pers := 's'
    else
      Pers := 'p';
  end;
  Result := Format('%s%d%s', [TargetTypes[Target.IsBCB], Target.Version, Pers]);
end;

function TTargetConfig.GetUnitOutDir: string;
const
  TargetTypes: array[Boolean] of string = ('D', 'C');
begin
  Result := GetJVCLDir + Format('\lib\%s%d', [TargetTypes[Target.IsBCB], Target.Version]);
end;

procedure TTargetConfig.SetInstallMode(const Value: TInstallMode);
begin
  if Value <> FInstallMode then
  begin
    if Value = [] then
      FInstallMode := [pkVcl]
    else
      FInstallMode := Value;
  end;
end;

function TTargetConfig.GetInstance: TObject;
begin
  Result := Self;
end;

function TTargetConfig.GetTarget: TCompileTarget;
begin
  Result := FTarget;
end;

function TTargetConfig.GetJVCLPackagesXmlDir: string;
begin
  Result := Owner.JVCLPackagesXmlDir;
end;

function TTargetConfig.GetJVCLDir: string;
begin
  Result := Owner.JVCLDir;
end;

function TTargetConfig.GetJVCLPackagesDir: string;
begin
  Result := Owner.JVCLPackagesDir;
end;

function TTargetConfig.GetJCLDir: string;
begin
  Result := FJCLDir;
end;

function TTargetConfig.GetHppDir: string;
begin
  Result := FHppDir;
end;

function TTargetConfig.GetDebugUnits: Boolean;
begin
  Result := FDebugUnits;
end;

function TTargetConfig.GetAutoDependencies: Boolean;
begin
  Result := FAutoDependencies and not Build;
end;

function TTargetConfig.GetBuild: Boolean;
begin
  Result := FBuild;
end;

function TTargetConfig.GetCompileOnly: Boolean;
begin
  Result := FCompileOnly;
end;

function TTargetConfig.GetBplDir: string;
begin
  Result := FBplDir;
end;

function TTargetConfig.GetDcpDir: string;
begin
  Result := FDcpDir;
end;

function TTargetConfig.GetFrameworkCount: Integer;
var
  Kind: TPackageGroupKind;
begin
  Result := 0;
  for Kind := pkFirst to pkLast do
  begin
    if Frameworks.Items[Target.IsPersonal, Kind] <> nil then
      Inc(Result);
  end;
end;

function TTargetConfig.GetDxgettextDir: string;
begin
  Result := Owner.DxgettextDir;
  if Result <> '' then
    if Target.Version = 5 then
      Result := Result + '\delphi5';
end;

procedure TTargetConfig.Save;
var
  Kind: TPackageGroupKind;
  Ini: TMemIniFile;
begin
  for Kind := pkFirst to pkLast do
  begin
    if Frameworks.Items[False, Kind] <> nil then
      SavePackagesSettings(Frameworks.Items[False, Kind]);
    if Frameworks.Items[True, Kind] <> nil then
      SavePackagesSettings(Frameworks.Items[True, Kind]);
  end;

  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.WriteString(Target.DisplayName, 'JCLDir', JCLDir); // do not localize
    Ini.WriteString(Target.DisplayName, 'HPPDir', HppDir); // do not localize
    Ini.WriteString(Target.DisplayName, 'BPLDir', BplDir); // do not localize
    Ini.WriteString(Target.DisplayName, 'DCPDir', DcpDir); // do not localize
    Ini.WriteBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall); // do not localize
    Ini.WriteBool(Target.DisplayName, 'CleanPalettes', CleanPalettes); // do not localize
    for Kind := pkFirst to pkLast do
      Ini.WriteBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode); // do not localize
    Ini.WriteBool(Target.DisplayName, 'AutoDependencies', AutoDependencies); // do not localize

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TTargetConfig.Load;
var
  Kind: TPackageGroupKind;
  Ini: TMemIniFile;
  Mode: TInstallMode;
begin
  for Kind := pkFirst to pkLast do
  begin
    if Frameworks.Items[False, Kind] <> nil then
      ResetPackagesSettings(Frameworks.Items[False, Kind]);
    if Frameworks.Items[True, Kind] <> nil then
      ResetPackagesSettings(Frameworks.Items[True, Kind]);
  end;

  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    if JCLDir = '' then
      JCLDir := Ini.ReadString(Target.DisplayName, 'JCLDir', JCLDir); // do not localize
    HppDir := Ini.ReadString(Target.DisplayName, 'HPPDir', HppDir);   // do not localize
    BplDir := Ini.ReadString(Target.DisplayName, 'BPLDir', BplDir);   // do not localize
    DcpDir := Ini.ReadString(Target.DisplayName, 'DCPDir', DcpDir);   // do not localize
    DeveloperInstall := Ini.ReadBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall); // do not localize
    CleanPalettes := Ini.ReadBool(Target.DisplayName, 'CleanPalettes', CleanPalettes); // do not localize
    Mode := [];
    for Kind := pkFirst to pkLast do
      if Ini.ReadBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode) then // do not localize
        Include(Mode, Kind);
    InstallMode := Mode;
    //AutoDependencies := Ini.ReadBool(Target.DisplayName, 'AutoDependencies', AutoDependencies);
  finally
    Ini.Free;
  end;
end;

function TTargetConfig.RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup): Boolean;
var
  PackageIndex, i: Integer;
  KnownPackages, DisabledPackages: TDelphiPackageList;
  Target: TCompileTarget;
begin
  Target := ProjectGroup.Target;
  KnownPackages := Target.KnownPackages;
  DisabledPackages := Target.DisabledPackages;

  // remove JVCL packages
  for i := DisabledPackages.Count - 1 downto 0 do
    if StartsWith(DisabledPackages.Items[i].Name, 'Jv', True) then
      DisabledPackages.Delete(i);

  for i := KnownPackages.Count - 1 downto 0 do
    if StartsWith(KnownPackages.Items[i].Name, 'Jv', True) then
      KnownPackages.Delete(i);


  for PackageIndex := 0 to ProjectGroup.Count - 1 do
  begin
    if ProjectGroup.Packages[PackageIndex].Install and
       ProjectGroup.Packages[PackageIndex].Info.IsDesign then
    begin
      KnownPackages.Add(
        ProjectGroup.TargetConfig.BplDir + '\' + ProjectGroup.Packages[PackageIndex].TargetName,
        ProjectGroup.Packages[PackageIndex].Info.Description
      );
    end;
  end;

  ProjectGroup.Target.SavePaths;
  ProjectGroup.Target.SavePackagesLists;
  Result := True;
end;

function TTargetConfig.RegisterToIDE: Boolean;
var
  Kind: TPackageGroupKind;
  i: Integer;
  AllPackages, PackageGroup: TProjectGroup;
begin
  if InstalledJVCLVersion < 3 then
    DeinstallJVCL(nil, nil);

 // remove old
  AddPaths(Target.BrowsingPaths, False, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qrun']);
  AddPaths(Target.SearchPaths, False, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qrun']);


 // common
  AddPaths(Target.BrowsingPaths, True, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common']);
  AddPaths(Target.SearchPaths, True, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common', Target.InsertDirMacros(UnitOutDir)]);

 // add
  if pkVCL in InstallMode then
  begin
    AddPaths(Target.BrowsingPaths, True, ExtractFileDir(Owner.JVCLDir),
      ['jvcl3\run']);
    AddPaths(Target.SearchPaths, {Add:=}DeveloperInstall, ExtractFileDir(Owner.JVCLDir),
      ['jvcl3\run']);
  end;
  if pkCLX in InstallMode then
  begin
    AddPaths(Target.BrowsingPaths, True, ExtractFileDir(Owner.JVCLDir),
      ['jvcl3\qcommon', 'jvcl3\qrun']);
    AddPaths(Target.SearchPaths, {Add:=}DeveloperInstall, ExtractFileDir(Owner.JVCLDir),
      ['jvcl3\qcommon', 'jvcl3\qrun']);
  end;

  AllPackages := TProjectGroup.Create(Self, '');
  try
    for Kind := pkFirst to pkLast do
    begin
      if Kind in InstallMode then
      begin
        PackageGroup := Frameworks.Items[Target.IsPersonal, Kind];
        if PackageGroup <> nil then
        begin
          for i := 0 to PackageGroup.Count - 1 do
            if PackageGroup.Packages[i].Install then
              AllPackages.AddPackage(PackageGroup.Packages[i]);
        end;
      end;
    end;
    Result := RegisterProjectGroupToIDE(AllPackages);
  finally
    AllPackages.Free;
  end;
end;

procedure TTargetConfig.DoCleanPalette(reg: TRegistry; const Name: string;
  RemoveEmptyPalettes: Boolean);
var
  Entries, S: string;
  List: TStrings;
  i, ps: Integer;
begin
  Entries := reg.ReadString(Name);
  List := TStringList.Create;
  try
    ps := 0;
    for i := 1 to Length(Entries) do
      if Entries[i] = ';' then
      begin
        List.Add(SubStr(Entries, ps + 1, i - 1));
        ps := i;
      end;
    if ps < Length(Entries) then
      List.Add(SubStr(Entries, ps + 1, Length(Entries)));

    for i := List.Count - 1 downto 0 do
    begin
      ps := Pos('.', List[i]); // for Delphi 6, 7 and BCB 6
      if Copy(List[i], ps + 1, 3) = RsComponentPalettePrefix then
        List.Delete(i);
    end;

    S := '';
    for i := 0 to List.Count - 1 do
      S := S + List[i] + ';';
    // last char is ';'

    if (S <> '') or (not RemoveEmptyPalettes) then
    begin
      if S <> Entries then
        reg.WriteString(Name, S)
    end
    else
      reg.DeleteValue(Name);
      
  finally
    List.Free;
  end;
end;

procedure TTargetConfig.CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
var
  i: Integer;
  reg: TRegistry;
  List: TStrings;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    if reg.OpenKey(Target.RegistryKey + '\Palette', False) then
    begin
      List := TStringList.Create;
      try
        reg.GetValueNames(List);
        for i := 0 to List.Count - 1 do
          DoCleanPalette(reg, List[i], RemoveEmptyPalettes);
      finally
        List.Free;
      end;
    end;
  finally
    reg.Free;
  end;
end;

procedure TTargetConfig.DeinstallJVCL(Progress: TDeinstallProgressEvent;
  DeleteFiles: TDeleteFilesEvent);

  procedure DoProgress(const Text: string; Position, Max: Integer);
  begin
    if Assigned(Progress) then
      Progress(Self, Text, Position, Max);
  end;

var
  MaxSteps: Integer;
  i: Integer;
  Ini: TMemIniFile;
  Kind: TPackageGroupKind;
begin
  MaxSteps := 4;
  if not Assigned(DeleteFiles) then
    Dec(MaxSteps); 

{**}DoProgress(RsCleaningPalette, 0, MaxSteps);
  CleanJVCLPalette(True);

{**}DoProgress(RsCleaningPathLists, 1, MaxSteps);

 // remove JVCL 1 and 2 directories
  for i := Target.BrowsingPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.BrowsingPaths[i])) <> 0 then
      Target.BrowsingPaths.Delete(i);

  for i := Target.SearchPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.SearchPaths[i])) <> 0 then
      Target.SearchPaths.Delete(i);


 // remove JVCL 3 directories
  AddPaths(Target.BrowsingPaths, {Add:=}False, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common', 'jvcl3\design', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qdesign', 'jvcl3\qrun']);
  AddPaths(Target.SearchPaths, {Add:=}False, ExtractFileDir(Owner.JVCLDir),
    ['jvcl3\common', 'jvcl3\design', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qdesign', 'jvcl3\qrun',
    Target.InsertDirMacros(UnitOutDir), UnitOutDir]);
  Target.SavePaths;

{**}DoProgress(RsUnregisteringPackages, 2, MaxSteps);
  // remove JVCL packages
  with Target do
  begin
    for i := DisabledPackages.Count - 1 downto 0 do
      if StartsWith(DisabledPackages.Items[i].Name, 'Jv', True) then
        DisabledPackages.Delete(i);

    for i := KnownPackages.Count - 1 downto 0 do
      if StartsWith(KnownPackages.Items[i].Name, 'Jv', True) then
        KnownPackages.Delete(i);
  end;
  Target.SavePackagesLists;

 // clean ini file
  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    Ini.EraseSection(Target.DisplayName);
    for Kind := pkFirst to pkLast do
    begin
      if Frameworks.Items[False, Kind] <> nil then
        Ini.EraseSection(Frameworks.Items[False, Kind].BpgName);
      if Frameworks.Items[True, Kind] <> nil then
        Ini.EraseSection(Frameworks.Items[True, Kind].BpgName);
    end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;

  if Assigned(DeleteFiles) then
  begin
{**}DoProgress(RsDeletingFiles, 3, MaxSteps);
    DeleteFiles(Self);
  end;

{**}DoProgress(RsComplete, MaxSteps, MaxSteps);
end;

end.
