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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

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
  TInstallMode = set of TPackageGroupKind;
  TDeinstallProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer) of object;

  TTargetConfig = class(TComponent, ITargetConfig) // TComponent <-> TInterfacedObject
  private
    FOwner: TJVCLData;
    FTarget: TCompileTarget;
    FInstalledJVCLVersion: Integer;
    FJCLDir: string;
    FMissingJCL: Boolean;
    FCompiledJCL: Boolean;
    FInstallJCL: Boolean;
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
  public
    property Target: TCompileTarget read GetTarget;
    property Owner: TJVCLData read FOwner;
  public
    constructor Create(AOwner: TJVCLData; ATarget: TCompileTarget); reintroduce;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
    procedure DeinstallJVCL(Progress: TDeinstallProgressEvent);

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
    property DcpDir: string read GetDcpDir write FDcpDir;

    property InstalledJVCLVersion: Integer read FInstalledJVCLVersion;
      // InstalledJVCLVersion returns the version of the installed JVCL.

    property MissingJCL: Boolean read FMissingJCL;
      // MissingJCL is True when no JCL is installed and no JCL directoy was
      // found that could be installed.

    property CompiledJCL: Boolean read FCompiledJCL;
      // CompiledJCL is True when CJcl.dcp and CJclVcl.dcp exist for this
      // target.

    property Frameworks: TJVCLFrameworks read FFrameworks;
      // Frameworks contains all possible package groups.

    property FrameworkCount: Integer read GetFrameworkCount;
      // FrameworkCount returns the number of available frameworks for this
      // target.
  public
    property InstallJCL: Boolean read FInstallJCL write FInstallJCL;
      // InstallJCL specifies if the required JCL should be installed on this
      // target.

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
  protected
    function JvclIncFilename: string;
    procedure Init; virtual;
    function RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveTargetConfigs;
    function RegisterToIDE(TargetConfig: TTargetConfig): Boolean;

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

    property TargetConfig[Index: Integer]: TTargetConfig read GetTargetConfig;
    property Targets: TCompileTargetList read FTargets;
  end;

implementation

uses
  Utils;

resourcestring
  RsComponentPalettePrefix = 'TJv';

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

function TJVCLData.GetBuild: Integer;
var
  i: Integer;
begin
  Result := 0; // false
  for i := 0 to Targets.Count - 1 do
  begin
    if TargetConfig[i].Build then
      Result := 1 // true
    else
      if Result = 1 then
      begin
        Result := 2; // mixed
        Exit;
      end;
  end;
end;

function TJVCLData.GetCleanPalettes: Integer;
var
  i: Integer;
begin
  Result := 0; // false
  for i := 0 to Targets.Count - 1 do
  begin
    if TargetConfig[i].CleanPalettes then
      Result := 1 // true
    else
      if Result = 1 then
      begin
        Result := 2; // mixed
        Exit;
      end;
  end;
end;

function TJVCLData.GetCompileOnly: Integer;
var
  i: Integer;
begin
  Result := 0; // false
  for i := 0 to Targets.Count - 1 do
  begin
    if TargetConfig[i].CompileOnly then
      Result := 1 // true
    else
      if Result = 1 then
      begin
        Result := 2; // mixed
        Exit;
      end;
  end;
end;

function TJVCLData.GetDeveloperInstall: Integer;
var
  i: Integer;
begin
  Result := 0; // false
  for i := 0 to Targets.Count - 1 do
  begin
    if TargetConfig[i].DeveloperInstall then
      Result := 1 // true
    else
      if Result = 1 then
      begin
        Result := 2; // mixed
        Exit;
      end;
  end;
end;

function TJVCLData.GetJVCLDir: string;
begin
  if FJVCLDir = '' then
  begin
    FJVCLDir := ExtractFileDir(ParamStr(0));
    while not DirectoryExists(FJVCLDir + '\packages') do
    begin
      if Length(FJVCLDir) < 4 then
      begin
        MessageBox(0, 'No JVCL directory found. Application terminated.',
                   'JVCL Installer', MB_ICONERROR or MB_OK);
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
  Result := JVCLDir + '\packages\xml';
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
 // dxgettext detection
  S := ReadRegString(HKEY_CLASSES_ROOT, '\bplfile\Shell\Extract strings\Command', '');
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
  Result := JVCLDir + '\common\jvcl.inc';
end;

function TJVCLData.RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup): Boolean;
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
  ProjectGroup.Target.SaveKnownPackages;
  Result := True;
end;

function TJVCLData.RegisterToIDE(TargetConfig: TTargetConfig): Boolean;
var
  Kind: TPackageGroupKind;
  i: Integer;
  AllPackages, PackageGroup: TProjectGroup;
begin
  if TargetConfig.InstalledJVCLVersion < 3 then
    TargetConfig.DeinstallJVCL(nil);

 // remove old
  AddPaths(TargetConfig.Target.BrowsingPaths, False, ExtractFileDir(JVCLDir),
    ['jvcl3\common', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qrun']);
  AddPaths(TargetConfig.Target.SearchPaths, False, ExtractFileDir(JVCLDir),
    ['jvcl3\common', 'jvcl3\run', 'jvcl3\qcommon', 'jvcl3\qrun']);


 // common
  AddPaths(TargetConfig.Target.BrowsingPaths, True, ExtractFileDir(JVCLDir),
    ['jvcl3\common']);
  AddPaths(TargetConfig.Target.SearchPaths, True, ExtractFileDir(JVCLDir),
    ['jvcl3\common', TargetConfig.Target.InsertDirMacros(TargetConfig.UnitOutDir)]);

 // add
  if pkVCL in TargetConfig.InstallMode then
  begin
    AddPaths(TargetConfig.Target.BrowsingPaths, True, ExtractFileDir(JVCLDir),
      ['jvcl3\run']);
    AddPaths(TargetConfig.Target.SearchPaths, {Add:=}TargetConfig.DeveloperInstall, ExtractFileDir(JVCLDir),
      ['jvcl3\run']);
  end;
  if pkCLX in TargetConfig.InstallMode then
  begin
    AddPaths(TargetConfig.Target.BrowsingPaths, True, ExtractFileDir(JVCLDir),
      ['jvcl3\qcommon', 'jvcl3\qrun']);
    AddPaths(TargetConfig.Target.SearchPaths, {Add:=}TargetConfig.DeveloperInstall, ExtractFileDir(JVCLDir),
      ['jvcl3\qcommon', 'jvcl3\qrun']);
  end;

  AllPackages := TProjectGroup.Create(TargetConfig, '');
  try
    for Kind := pkFirst to pkLast do
    begin
      if Kind in TargetConfig.InstallMode then
      begin
        PackageGroup := TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, Kind];
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
  FHppDir := Target.RootDir + '\Include\Vcl';
  FCleanPalettes := True;
  FDeveloperInstall := False;
  FAutoDependencies := True;
  FBplDir := Target.BplDir;
  FDcpDir := Target.DcpDir;
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
var
  i: Integer;
  S: string;
begin
  FCompiledJCL := False;

 // parse command line arguments
  for i := 1 to ParamCount do
  begin
    S := ParamStr(i);
    if StartsWith(S, '--jcl-path=', True) then
    begin
      Delete(S, 1, 11);
      if DirectoryExists(S) then
        FJCLDir := S;
    end;
  end;

 // identify JVCL version
  FInstalledJVCLVersion := 0;
  if Target.FindPackageEx('JvPack1') <> nil then
    FInstalledJVCLVersion := 1
  else if Target.FindPackageEx('jvcl2') <> nil then
    FInstalledJVCLVersion := 2
  else if Target.FindPackageEx('JvCore') <> nil then
    FInstalledJVCLVersion := 3;

 // identify JCL version
  FMissingJCL := True;
  if FJCLDir = '' then
  begin
    for i := 0 to Target.BrowsingPaths.Count - 1 do
    begin
      if Pos('jcl\source', LowerCase(Target.BrowsingPaths[i])) <> 0 then
      begin
        FJCLDir := Target.ExpandDirMacros(Target.BrowsingPaths[i]);
        if CompareText(ExtractFileName(FJCLDir), 'source') = 0 then
          FJCLDir := ExtractFileDir(FJCLDir)
        else
          FJCLDir := ExtractFileDir(ExtractFileDir(FJCLDir));
        FInstallJCL := False;
        Break;
      end;
    end;
  end;

  if FJCLDir = '' then
  begin
    for i := 0 to Target.SearchPaths.Count - 1 do
    begin
      if Pos('jcl\lib', LowerCase(Target.SearchPaths[i])) <> 0 then
      begin
        FJCLDir := Target.ExpandDirMacros(Target.SearchPaths[i]);
        if CompareText(ExtractFileName(FJCLDir), 'lib') = 0 then
          FJCLDir := ExtractFileDir(FJCLDir)
        else
          FJCLDir := ExtractFileDir(ExtractFileDir(FJCLDir));
        FInstallJCL := False;
        Break;
      end;
    end;
  end;

  if FJCLDir <> '' then
  begin
    if DirectoryExists(JCLDir + '\source\common') then
      FMissingJCL := False;
  end;

  // are CJcl.dcp and CJclVcl.dcp available
  if FileExists(BplDir + '\CJcl.dcp') and
     FileExists(BplDir + '\CJclVcl.dcp') then
  begin
    if FJCLDIr = '' then
      FJCLDir := BplDir;
    FMissingJCL := False;
    FCompiledJCL := True;
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
    Ini.WriteString(Target.DisplayName, 'JCLDir', JCLDir);
    Ini.WriteString(Target.DisplayName, 'HPPDir', HppDir);
    Ini.WriteBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall);
    Ini.WriteBool(Target.DisplayName, 'CleanPalettes', CleanPalettes);
    for Kind := pkFirst to pkLast do
      Ini.WriteBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode);
    Ini.WriteBool(Target.DisplayName, 'AutoDependencies', AutoDependencies);

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
      JCLDir := Ini.ReadString(Target.DisplayName, 'JCLDir', JCLDir);
    if HppDir = '' then
      HppDir := Ini.ReadString(Target.DisplayName, 'HPPDir', HppDir);
    DeveloperInstall := Ini.ReadBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall);
    CleanPalettes := Ini.ReadBool(Target.DisplayName, 'CleanPalettes', CleanPalettes);
    Mode := [];
    for Kind := pkFirst to pkLast do
      if Ini.ReadBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode) then
        Include(Mode, Kind);
    InstallMode := Mode;
    //AutoDependencies := Ini.ReadBool(Target.DisplayName, 'AutoDependencies', AutoDependencies);
  finally
    Ini.Free;
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

procedure TTargetConfig.DeinstallJVCL(Progress: TDeinstallProgressEvent);
var
  i: Integer;
  Kind: TPackageGroupKind;
  ProjectGroup: TProjectGroup;
begin
  CleanJVCLPalette(True);

  for i := Target.BrowsingPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.BrowsingPaths[i])) <> 0 then
      Target.BrowsingPaths.Delete(i);

  for i := Target.SearchPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.SearchPaths[i])) <> 0 then
      Target.SearchPaths.Delete(i);

  for Kind := pkFirst to pkLast do
  begin
    ProjectGroup := Frameworks.Items[Target.IsPersonal, Kind];
    if ProjectGroup <> nil then
    begin

      for i := Target.KnownPackages.Count - 1 downto 0 do
      begin

      end;

    end;
  end;

end;

end.
