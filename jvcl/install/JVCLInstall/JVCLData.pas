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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JVCLData;

{$I jvcl.inc}

interface

uses
  Windows, Registry, SysUtils, Classes, Contnrs,
  JVCLConfiguration, DelphiData, PackageUtils, Intf, GenerateUtils, PackageGenerator,
  IniFiles, JCLData, JVCLVer, RegConfig,
  JclDebug;

const
  sPackageGeneratorFile = 'devtools\bin\pgEdit.xml';
  sDxgettextRegKey = '\bplfile\Shell\Extract strings\Command';
  sJvclIncFile = '%s\common\jvcl.inc';
  sBCBIncludeDir = '%s\Include\Vcl';

type
  TJVCLData = class;
  TTargetConfig = class;

  TInstallMode = set of TPackageGroupKind;

  TTargetConfig = class(TComponent, ITargetConfig) // TComponent <-> TInterfacedObject
  private
    FOwner: TJVCLData;
    FTarget: TCompileTarget;
    FInstalledJVCLVersion: Integer;
    FDefaultHppDir: string;
    FBplDir: string;
    FHppDir: string;

    FJclDir: string;
    FJclDcpDir: string;
    FJclBplDir: string;
    FJclVersion: string;
    FMissingJCL: Boolean;
    FOutdatedJcl: Boolean;

    FJVCLVersion: string;
    FInstallJVCL: Boolean;

    FDeveloperInstall: Boolean;
    FCleanPalettes: Boolean;
    FBuild: Boolean;
    FCompileOnly: Boolean;
    FDebugUnits: Boolean;
    FAutoDependencies: Boolean;
    FAddBplDirToPath: Boolean;

    FInstallMode: TInstallMode;
    FFrameworks: TFrameworks;
    FGenerateMapFiles: Boolean;
    FLinkMapFiles: Boolean;
    FCreateJdbgFiles: Boolean;
    FDeleteMapFiles: Boolean;
    FJVCLConfig: TJVCLConfig;
    FJVCLRegistryConfig: TJVCLRegistryConfig;

    FInstallSuccess: Boolean;

    procedure SetInstallMode(Value: TInstallMode);
    function GetFrameworkCount: Integer;
    function GetDxgettextDir: string;
    function GetDeveloperInstall: Boolean;
    function GetGenerateMapFiles: Boolean;
    function GetLinkMapFiles: Boolean;
    function GetCreateJdbgFiles: Boolean;
    function GetDeleteMapFiles: Boolean;
    function GetCleanPalettes: Boolean;
    function GetJVCLConfig: TJVCLConfig;
    function GetJVCLRegistryConfig: TJVCLRegistryConfig;
  private
    { ITargetConfig }
    function GetInstance: TObject;
    function GetJVCLPackagesXmlDir: string;
    function GetJVCLDir: string;
    function GetJVCLPackagesDir: string;

    function GetTargetSymbol: string;
    function GetMainTargetSymbol: string;
    function GetAutoDependencies: Boolean;
    function GetBuild: Boolean;
    function GetUnitOutDir: string;
    function GetDebugUnitOutDir: string;
    function GetDebugUnits: Boolean;
    function GetCompileOnly: Boolean;
    function GetAddBplDirToPath: Boolean;

    function GetTarget: TCompileTarget;
    function GetJclDir: string;
    function GetJclDcpDir: string;
    function GetJclDcuDir: string;
    function GetJclBplDir: string;
    function GetHppDir: string;
    function GetBplDir: string;
    function GetDcpDir: string;
    function GetDebugHppDir: string;
    function GetDebugBplDir: string;
    function GetDebugDcpDir: string;
    function GetLogFileName: string;
  protected
    procedure Init; virtual;
    procedure DoCleanPalette(Reg: TRegistry; const Name: string;
      RemoveEmptyPalettes: Boolean);
    procedure ClearPackageCache(const Key: string; const AStartsWith: string);
    procedure RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup);

    procedure UpdateOptions;
    procedure EnableOption(const Name: string; Enable: Boolean; ForceEnable: Boolean = False);
  public
    property Target: TCompileTarget read GetTarget;
    property Owner: TJVCLData read FOwner;

    procedure _SetBuildSuccess(Value: Boolean);
  public
    constructor Create(AOwner: TJVCLData; ATarget: TCompileTarget); reintroduce;
    destructor Destroy; override;
    procedure Reinit;
    procedure Load;
    procedure Save;
    procedure CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
    procedure GetPackageBinariesForDeletion(List: TStrings);
    procedure DeinstallJVCL(Progress: TDeinstallProgressEvent;
      DeleteFiles: TDeleteFilesEvent; RealUninstall: Boolean);
    procedure AddPathsToIDE;
    procedure RegisterDesigntimePackages;
    procedure RegisterJVCLVersionInfo;

    function GetOutputDirs(DebugUnits: Boolean): TOutputDirs;
      // GetOutputDirs returns the output directory set dependingg on DebugUnits.
      // If DebugUnits=True the returned directories are all debug output
      // directories. Otherwise the "normal" output directories are returned.

    function GetPathEnvVar: string;
      // GetPathEnvVar returns a proper PATH environment variable for the target.

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

    function VersionedJclDcp(const Name: string): string;
      // VersionedJclDcp inserts the suffix for JCL .dcp files; wants "Jcl.dcp"

    function VersionedJclBpl(const Name: string): string;
      // VersionedJclBpl inserts the suffix for JCL .bpl files; wants "Jcl.bpl"

    function VersionedJVCLXmlDcp(const Name: string): string;
      // VersionedVCLDcp inserts the suffix for JVCL .dcp files; wants "JvPack-R"

    function VersionedJVCLXmlBpl(const Name: string): string;
      // VersionedVCLBpl inserts the suffix for JVCL .bpl files; wants "JvPack-R"

    function LinkMapFile(const BinaryFileName, MapFileName: string;
      var MapFileSize, JclDebugDataSize: Integer): Boolean;
      // link the map file in the binary file

    function CompressMapFileToJdbg(const MapFileName: string): Boolean;
      // compresses the map file to a jdbg file

    property TargetSymbol: string read GetTargetSymbol;
      // TargetSymbol returns the symbol that is used in the xml files for this
      // target.
    property MainTargetSymbol: string read GetMainTargetSymbol;
      // MainTargetSymbol returns the symbol that is used in the xml files for this
      // target's main version. It does not include the personal/standard flag

    property UnitOutDir: string read GetUnitOutDir;
      // UnitOutDir specifies the JVCL directory where the .dcu should go.
    property BplDir: string read GetBplDir write FBplDir;
      // BPL directory for this target
    property DcpDir: string read GetDcpDir;
      // DCP directory for this target
    property HppDir: string read GetHppDir write FHppDir;
      // HppDir: (for BCB installation) specifies where the generated .hpp files
      // should go.
    property DebugUnitOutDir: string read GetDebugUnitOutDir;
      // DebugUnitOutDir specifies the JVCL directory where the .dcu should go.
    property DebugBplDir: string read GetBplDir;
      // Debug BPL directory for this target
    property DebugDcpDir: string read GetDebugDcpDir;
      // Debug DCP directory for this target
    property DebugHppDir: string read GetDebugHppDir;
      // Debbug HppDir: (for BCB installation) specifies where the generated .hpp files
      // should go.

    property DxgettextDir: string read GetDxgettextDir;
      // Directory where dxgettext is installed or ''. (special handling for Delphi/BCB 5)

    property InstalledJVCLVersion: Integer read FInstalledJVCLVersion; // major version: 1, 2, 3, 4
      // InstalledJVCLVersion returns the version of the installed JVCL.

    property JVCLVersion: string read FJVCLVersion;
      // JVCLVersion returns version of the installed JVCL: '3.30';

    property JclVersion: string read FJclVersion;
      // JclVersion returns version of the installed JCL.

    property MissingJCL: Boolean read FMissingJCL;
      // MissingJCL is True when no JCL is installed and no JCL directoy was
      // found that could be installed.

    property OutdatedJcl: Boolean read FOutdatedJcl;
      // OutdatedJcl is True if no jcl\source\common\windows\win32api directory
      // exists which means that the JCL is too old for the JVCL.

    property Frameworks: TFrameworks read FFrameworks;
      // Frameworks contains all possible package groups.

    property FrameworkCount: Integer read GetFrameworkCount;
      // FrameworkCount returns the number of available frameworks for this
      // target.

    property JVCLConfig: TJVCLConfig read GetJVCLConfig;
      // JVCLConfig returns the confiuration

    property JVCLRegistryConfig: TJVCLRegistryConfig read GetJVCLRegistryConfig;
      // JVCLRegistryConfig returns the configuration that does not affect the JVCL.inc
  public
    property InstallJVCL: Boolean read FInstallJVCL write FInstallJVCL;
      // InstallJVCL specifies if the JVCL should be installed on this target.

    property InstallMode: TInstallMode read FInstallMode write SetInstallMode;
      // InstallMode specifies if the JVCL only,  JVCL and JVCLX or JVCLX only
      // should be installed.

    property DebugUnits: Boolean read GetDebugUnits write FDebugUnits;
      // if DebugUnits is True the units will be compiled in debug mode, too.
      // (Delphi only) [NOT USED IN THE JVCL DUE TO jvcl.inc SETTINGS]

    property GenerateMapFiles: Boolean read GetGenerateMapFiles write FGenerateMapFiles;
      // if GenerateMapFiles is True the compiler generates .map files for each package

    property LinkMapFiles: Boolean read GetLinkMapFiles write FLinkMapFiles;
      // if LinkMapFiles is True the the map files are linked as a resource of the binary

    property CreateJdbgFiles: Boolean read GetCreateJdbgFiles write FCreateJdbgFiles;
      // if CreateJdbgFiles is True the map files will be compressed to jdbg files

    property DeleteMapFiles: Boolean read GetDeleteMapFiles write FDeleteMapFiles;
      // if DeleteMapFiles is True the the map files are deleted after they
      // were linked in the binaries or compressed to jdbg files

    property Build: Boolean read GetBuild write FBuild;
      // if Build is True the packages are built instead of make.

    property CompileOnly: Boolean read GetCompileOnly write FCompileOnly;
      // if CompileOnly is True the desigtime packages are not registered to the
      // IDE.

    property AutoDependencies: Boolean read GetAutoDependencies write FAutoDependencies;
      // if AutoDependencies it True the make file for the project groups will
      // contain auto dependency information for faster compilation.

    property AddBplDirToPath: Boolean read GetAddBplDirToPath write FAddBplDirToPath;
      // AddBplDirToPath: adds the BplDir to the Target.EnvPath variable. This
      // allows the IDE to find the runtime packages.

    property DeveloperInstall: Boolean read GetDeveloperInstall write FDeveloperInstall;
      // DevelopInstall: add the \run directory to the library path.

    property CleanPalettes: Boolean read GetCleanPalettes write FCleanPalettes;
      // CleanPalettes specifies if the JVCL components should be removed from
      // the component palettes before installation.

    property JclDir: string read GetJclDir;
      // JclDir specifies the directory where the JCL is.

    property JclDcpDir: string read GetJclDcpDir;
      // JclDcpDir returns the directory where the Jcl.dcp/JclVcl.dcp files are.

    property JclDcuDir: string read GetJclDcuDir;
      // JclDcuDir specifies the directory where the JCL .dcu files are, depending on the target.

    property JclBplDir: string read GetJclBplDir;
      // JclBplDir returns the directory where the JclXx.bpl/JclVclXX.bpl files are.

    property InstallSuccess: Boolean read FInstallSuccess;

    property LogFileName: string read GetLogFileName;
  end;

  TJclLinkMapFile = function(ExecutableFileName, MapFileName: PChar;
    var MapFileSize, JclDebugDataSize: Integer): Boolean;

  TJVCLData = class(TObject)
  private
    FConfigs: array of TTargetConfig;
    FTargets: TCompileTargetList;
    FIsDxgettextInstalled: Boolean;
    FDxgettextDir: string;
    FJVCLDir: string;
    FDeleteFilesOnUninstall: Boolean;
    FVerbose: Boolean;
    FIgnoreMakeErrors: Boolean;
    FJclLibrary: HModule;
    FJclLinkMapFile: TJclLinkMapFile;
    FPackageGenerator: TPackageGenerator;

    function GetTargetConfig(Index: Integer): TTargetConfig;
    function GetJVCLDir: string;
    function GetJVCLIncludeDir: string;
    function GetJVCLSourceDir: string;
    function GetJVCLPackagesDir: string;
    function GetJVCLPackagesXmlDir: string;
    function GetOptionState(Index: Integer): Integer;
    procedure SetOptionState(Index: Integer; const Value: Integer);
  protected
    function JvclIncFilename: string;
    procedure Init; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reinit;

    procedure SaveTargetConfigs;
    function FindTargetConfig(const TargetSymbol: string): TTargetConfig;
    function IsJVCLInstalledAnywhere(MinVersion: Integer): Boolean;
    function LinkMapFile(const BinaryFileName, MapFileName: string;
      var MapFileSize, JclDebugDataSize: Integer): Boolean;
    function CompressMapFileToJdbg(const MapFileName: string): Boolean;

    property DxgettextDir: string read FDxgettextDir;
    property IsDxgettextInstalled: Boolean read FIsDxgettextInstalled;

    property JVCLDir: string read GetJVCLDir;
    property JVCLSourceDir: string read GetJVCLSourceDir;
    property JVCLIncludeDir: string read GetJVCLIncludeDir;
    property JVCLPackagesDir: string read GetJVCLPackagesDir;
    property JVCLPackagesXmlDir: string read GetJVCLPackagesXmlDir;

    property DeveloperInstall: Integer index 3 read GetOptionState write SetOptionState;
    property DebugUnits: Integer index 4 read GetOptionState write SetOptionState;
    property CleanPalettes: Integer index 1 read GetOptionState write SetOptionState;
    property Build: Integer index 0 read GetOptionState write SetOptionState;
    property CompileOnly: Integer index 2 read GetOptionState write SetOptionState;
    property GenerateMapFiles: Integer index 5 read GetOptionState write SetOptionState;
    property LinkMapFiles: Integer index 6 read GetOptionState write SetOptionState;
    property CreateJdbgFiles: Integer index 8 read GetOptionState write SetOptionState;
    property DeleteMapFiles: Integer index 7 read GetOptionState write SetOptionState;

    property DeleteFilesOnUninstall: Boolean read FDeleteFilesOnUninstall write FDeleteFilesOnUninstall default True;
    property Verbose: Boolean read FVerbose write FVerbose default False;
    property IgnoreMakeErrors: Boolean read FIgnoreMakeErrors write FIgnoreMakeErrors default False;

    property TargetConfig[Index: Integer]: TTargetConfig read GetTargetConfig;
    property Targets: TCompileTargetList read FTargets;

    property PackageGenerator: TPackageGenerator read FPackageGenerator;
  end;

implementation

uses
  JclFileUtils, JclStrings,
  Utils, CmdLineUtils, PackageInformation, JediRegInfo;

resourcestring
  RsComponentPalettePrefix = 'TJv';
  RsNoJVCLFound = 'No JVCL directory found. Application terminated.';
  RsJVCLInstaller = 'JVCL Installer';

  RsCleaningPalette = 'Cleaning Palette...';
  RsCleaningPathLists = 'Cleaning Path lists...';
  RsUnregisteringPackages = 'Unregistering packages...';
  RsDeletingFiles = 'Deleting files...';
  RsComplete = 'Complete.';

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
  FPackageGenerator := TPackageGenerator.Create;
  FDeleteFilesOnUninstall := True;
  FVerbose := False;

  ErrMsg := '';
  FPackageGenerator.LoadConfig(JVCLDir + '\' + sPackageGeneratorFile, 'JVCL', ErrMsg);

  FTargets := TCompileTargetList.Create;
  SetLength(FConfigs, Targets.Count);
  for I := 0 to High(FConfigs) do
    FConfigs[I] := TTargetConfig.Create(Self, Targets[I]);

  { Don't fail during startup, because MapFile linking isn't that important that
    it should block the whole JVCL installer. }
  FJclLibrary := HModule(-1);

  Init;
end;

destructor TJVCLData.Destroy;
var
  i: Integer;
begin
  FJclLinkMapFile := nil;
  if (FJclLibrary <> 0) and (FJclLibrary <> HModule(-1)) then
    FreeLibrary(FJclLibrary);

  for i := 0 to High(FConfigs) do
    FConfigs[I].Free;
  FTargets.Free;
  FPackageGenerator.Free;
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

function TJVCLData.GetJVCLSourceDir: string;
begin
  Result := JVCLDir + '\run';
end;

function TJVCLData.GetJVCLIncludeDir: string;
begin
  Result := JVCLDir + '\common';
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
    if not FileExists(FDxgettextDir + '\msgfmt.exe') then
    begin
      FIsDxgettextInstalled := False;
      FDxgettextDir := '';
    end;
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

function TJVCLData.LinkMapFile(const BinaryFileName, MapFileName: string;
  var MapFileSize, JclDebugDataSize: Integer): Boolean;
var
  LinkerBugUnit: string;
begin
  try
    Result := InsertDebugDataIntoExecutableFile(BinaryFileName, MapFileName,
      LinkerBugUnit, MapFileSize, JclDebugDataSize);
  except
    Result := False;
  end;
end;

function TJVCLData.CompressMapFileToJdbg(const MapFileName: string): Boolean;
begin
  try
    Result := ConvertMapFileToJdbgFile(MapFileName);
  except
    Result := False;
  end;
end;

procedure TJVCLData.Reinit;
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[i].Reinit;
end;

procedure TJVCLData.SaveTargetConfigs;
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
    TargetConfig[i].Save;
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
        4: b := TargetConfig[i].DebugUnits;
        5: b := TargetConfig[i].GenerateMapFiles;
        6: b := TargetConfig[i].LinkMapFiles;
        7: b := TargetConfig[i].DeleteMapFiles;
        8: b := TargetConfig[i].CreateJdbgFiles;
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

procedure TJVCLData.SetOptionState(Index: Integer; const Value: Integer);
var
  i: Integer;
begin
  for i := 0 to Targets.Count - 1 do
  begin
    case Index of
      0: TargetConfig[i].Build := Value <> 0;
      1: TargetConfig[i].CleanPalettes := Value <> 0;
      2: TargetConfig[i].CompileOnly := Value <> 0;
      3: TargetConfig[i].DeveloperInstall := Value <> 0;
      4: TargetConfig[i].DebugUnits := Value <> 0;
      5: TargetConfig[i].GenerateMapFiles := Value <> 0;
      6: TargetConfig[i].LinkMapFiles := Value <> 0;
      7: TargetConfig[i].DeleteMapFiles := Value <> 0;
      8: TargetConfig[i].CreateJdbgFiles := Value <> 0;
    end;
  end;
end;

{ TTargetConfig }

constructor TTargetConfig.Create(AOwner: TJVCLData; ATarget: TCompileTarget);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FTarget := ATarget;
  FBuild := CmdOptions.RebuildPackages;

  FInstallMode := [pkVcl];
  if Target.IsBDS and (Target.IDEVersion >= 5) then // Delphi 2007+ have global include path
  begin
    if Target.IDEVersion >= 11 then  // bcc64 appeared with XE3, we must distinguish output directories
      FDefaultHppDir := GetJVCLDir + Format('\include\%s%d\%s', [Target.TargetType, Target.Version, LowerCase(Target.PlatformName)]) // do not localize
    else
      FDefaultHppDir := GetJVCLDir + Format('\include\%s%d', [Target.TargetType, Target.Version]) // do not localize
    //FDefaultHppDir := Target.ExpandDirMacros('$(BDSCOMMONDIR)\Hpp') // do not localize
  end
  else
    FDefaultHppDir := Format(sBCBIncludeDir, [Target.RootDir]);
  FHppDir := FDefaultHppDir;

  FCleanPalettes := True;
  FDeveloperInstall := False;
  FAutoDependencies := True;
  FGenerateMapFiles := True;
  FLinkMapFiles := Target.Platform = ctpWin32;  // linking map files into anything but x86 binaries is not yet supported
  FDeleteMapFiles := True;
  FCompileOnly := Target.Platform <> ctpWin32;  // by default, only install under Win32

  FBplDir := Target.BplDir;
  // DcpDir matches the DCU output directory.
  // The DCU output path will be added to the SearchPath and the IDE can then
  // access the DCP files.

  Init;
  FInstallJVCL := CanInstallJVCL;

  FFrameworks := TFrameworks.Create(Self);
  FJVCLConfig := TJVCLConfig.Create;
  FJVCLRegistryConfig := TJVCLRegistryConfig.Create;
  Load;
end;

destructor TTargetConfig.Destroy;
begin
  FJVCLRegistryConfig.Free;
  FJVCLConfig.Free;
  FFrameworks.Free;
  inherited Destroy;
end;

procedure TTargetConfig.Init;
  // Memory allocations must go to the constructor because Init could be called
  // more the once.
var
  i, FindCount: Integer;
  JclRegInfo: TJediInformation;
begin
  FInstallMode := [];
  FOutdatedJcl := False;

  // identify JVCL version
  FInstalledJVCLVersion := 0;
  if Target.FindPackageEx('JvPack1') <> nil then // do not localize
    FInstalledJVCLVersion := 1
  else if Target.FindPackageEx('jvcl2') <> nil then // do not localize
    FInstalledJVCLVersion := 2
  else if Target.FindPackageEx('JvCore') <> nil then // VCL // do not localize
  begin
    Include(FInstallMode, pkVCL);
    FInstalledJVCLVersion := 3;
  end;
  Include(FInstallMode, pkVCL);

  // find JCL by looking into the (new) JEDI Registry key
  FOutdatedJcl := False;
  FMissingJCL := True;
  FJclDir := '';
  FJclVersion := '';
  FJclBplDir := '';
  FJclDcpDir := '';

  JclRegInfo := ReadJediRegInformation(Target.RegistryKey, 'JCL', Target.PlatformName); // do not localize
  FJclDir := FixBackslashBackslash(ExcludeTrailingPathDelimiter(Target.ExpandDirMacros(JclRegInfo.RootDir)));
  FJclDcpDir := FixBackslashBackslash(ExcludeTrailingPathDelimiter(Target.ExpandDirMacros(JclRegInfo.DcpDir)));
  FJclBplDir := FixBackslashBackslash(ExcludeTrailingPathDelimiter(Target.ExpandDirMacros(JclRegInfo.BplDir)));
  FJclVersion := JclRegInfo.Version;

  FJVCLVersion := ReadJediRegInformation(Target.RegistryKey, 'JVCL', Target.PlatformName).Version;
  if (FInstalledJVCLVersion = 0) and (FJVCLVersion <> '') then
    FInstalledJVCLVersion := ParseVersionNumber(FJVCLVersion) shr 24
  else
  if (FJVCLVersion = '') and (FInstalledJVCLVersion = 3) then
    FJVCLVersion := '3';

  FMissingJCL := (FJclDir = '') or (FJclVersion = '') or (FJclDcpDir = '');
  if not FMissingJCL then
  begin
    // check version number
    FOutdatedJcl := ParseVersionNumber(FJclVersion) < ParseVersionNumber(JCLMinVersion);

    if not FOutdatedJcl then
    begin
      // check for the JCL's .dcp files
      FindCount := Length(JCLDcpFiles);
      for i := 0 to High(JCLDcpFiles) do
      begin
        if FileExists(FJclDcpDir + PathDelim + VersionedJclDcp(JCLDcpFiles[i])) then
        begin
          Dec(FindCount);
          Break;
        end;
      end;
      FMissingJCL := FindCount > 0;
    end;
  end;
end;

function TTargetConfig.GetJVCLConfig: TJVCLConfig;
begin
  Result := FJVCLConfig;
end;

function TTargetConfig.GetJVCLRegistryConfig: TJVCLRegistryConfig;
begin
  Result := FJVCLRegistryConfig;
end;

procedure TTargetConfig.AddPathsToIDE;
begin
  if InstalledJVCLVersion < 3 then
    DeinstallJVCL(nil, nil, True);

  RegisterJVCLVersionInfo;

  if AddBplDirToPath then
  begin
    if not Target.IsInEnvPath(BplDir) then
      Target.EnvPath := Target.EnvPath + ';' + BplDir;
  end;

  // remove old paths
  AddPaths(Target.BrowsingPaths, False, Owner.JVCLDir,
    ['common', 'run', 'Resources', 'qcommon', 'qrun']); // do not localize
  AddPaths(Target.SearchPaths, False, Owner.JVCLDir,
    ['common', 'run', 'Resources', 'qcommon', 'qrun']); // do not localize
  AddPaths(Target.DebugDcuPaths, {Add:=}False, Owner.JVCLDir,
    [Target.InsertDirMacros(DebugUnitOutDir), DebugUnitOutDir]); // do not localize
  if Target.SupportsPersonalities([persBCB]) then
    AddPaths(Target.GlobalCppBrowsingPaths, {Add:=}False, Owner.JVCLDir,
      ['Resources', Target.InsertDirMacros(UnitOutDir)]); // do not localize, clean up because we had browsing and library path wrong

  // update paths
  AddPaths(Target.BrowsingPaths, True, Owner.JVCLDir, // Resources directory must not be in browse-paths
    ['common']); // do not localize
  AddPaths(Target.SearchPaths, True, Owner.JVCLDir,
    [Target.InsertDirMacros(UnitOutDir), 'common', 'Resources']); // do not localize
  if DebugUnits and not DeveloperInstall then
    AddPaths(Target.DebugDcuPaths, True, Owner.JVCLDir,
      [Target.InsertDirMacros(DebugUnitOutDir)]); // do not localize
  if Target.SupportsPersonalities([persBCB]) then
  begin
    AddPaths(Target.GlobalIncludePaths, True, Owner.JVCLDir,
     [Target.InsertDirMacros(HppDir)]);
    AddPaths(Target.GlobalCppBrowsingPaths, True, Owner.JVCLDir,
      ['run', 'common']); // do not localize
    AddPaths(Target.GlobalCppLibraryPaths, True, Owner.JVCLDir,
      ['Resources', Target.InsertDirMacros(UnitOutDir)]); // do not localize
  end;

  // add
  if pkVCL in InstallMode then
  begin
    AddPaths(Target.BrowsingPaths, True, Owner.JVCLDir,
      ['run']); // do not localize
    AddPaths(Target.SearchPaths, {Add:=}DeveloperInstall, Owner.JVCLDir,
      ['run']); // do not localize
  end;

  Target.SavePaths;
end;

function TTargetConfig.CanInstallJVCL: Boolean;
begin
  Result := IsUpToDate and not MissingJCL;
end;

function TTargetConfig.IsUpToDate: Boolean;
begin
  Result := True;

  if Target.IsDelphi then
  begin
    if Target.Version = 6 then
    begin
      Result := Target.LatestUpdate >= 2; // Update 2 required
    end;
  end;
{
  if Target.IsBCB then
  begin
    case Target.Version of
      5: Result := Target.LatestUpdate >= 1;
      6: Result := Target.LatestUpdate >= 1;
    end;
  end
  else
  begin
    case Target.Version of
      5: Result := Target.LatestUpdate >= 1;
      6: Result := Target.LatestUpdate >= 2;
      7: Result := True;
      8: Result := False; // not supported
    end;
  end;}

  // The IDE is up to date because the JCL Installer guarantees this for us.
  // The JVCL requires an installed JCL, so this is no problem.
//  Result := True;
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
var
  Suffix: string;
begin
  Suffix := '';
  if Personal then
  begin
    if Target.Version <= 5 then
      Suffix := 'Std'  // do not localize
    else
      Suffix := 'Per'; // do not localize
  end;

  // Use a specific project group for x64
  if (Target.Version >= 16) then
    case Target.Platform of
      ctpWin32:
        ;  // no suffix needed
      ctpWin64:
        Suffix := Suffix + '_x64';
      else
        raise Exception.Create('JVCLData.TTargetConfig.GetBpgFilename: Unsupported platform');
    end;

  if Target.IsBDS then
  begin
    if Target.Version >= 11 then  // Delphi 2007 and upper use groupproj files
      Result := Owner.JVCLPackagesDir + Format('\%s%d%s Packages.groupproj', // do not localize
        [Target.TargetType, Target.Version, Suffix])
    else
      Result := Owner.JVCLPackagesDir + Format('\%s%d%s Packages.bdsgroup', // do not localize
        [Target.TargetType, Target.Version, Suffix]);
  end
  else
  begin
    Result := Owner.JVCLPackagesDir + Format('\%s%d%s Packages.bpg', // do not localize
      [Target.TargetType, Target.Version, Suffix]);
  end;
end;

function TTargetConfig.VersionedJclDcp(const Name: string): string;
begin
  Result := Name;
end;

function TTargetConfig.VersionedJclBpl(const Name: string): string;
var
  Suffix: string;
begin
  { TODO : Keep in sync with JCL naming schema }
  // Compute the JCL bpl file name
  if Target.Version >= 7 then
    Suffix := Format('%d0', [Target.Version])
  else if Target.IsBCB then
    Suffix := Format('C%d0', [Target.Version])
  else
    Suffix := Format('D%d0', [Target.Version]);
  Result := ChangeFileExt(Name, '') + Suffix + ExtractFileExt(Name);
end;

function TTargetConfig.VersionedJVCLXmlDcp(const Name: string): string;
begin
  { TODO : Keep in sync with JVCL naming schema }
  if EndsWith(Name, '-R', True) or EndsWith(Name, '-D', True) then
    Result := Copy(Name, 1, Length(Name) - 2) + '.dcp'
  else
    Result := Name;
end;

procedure TTargetConfig._SetBuildSuccess(Value: Boolean);
begin
  FInstallSuccess := Value;
end;

function TTargetConfig.VersionedJVCLXmlBpl(const Name: string): string;
var
  Suffix: string;
begin
  { TODO : Keep in sync with JVCL naming schema }
  if Target.Version >= 7 then
    Suffix := Format('%d0', [Target.Version])
  else if Target.IsBCB then
    Suffix := Format('C%d0', [Target.Version])
  else
    Suffix := Format('D%d0', [Target.Version]);

  if EndsWith(Name, '-R', True) or EndsWith(Name, '-D', True) then
    Result := Copy(Name, 1, Length(Name) - 2) + Suffix + '.bpl'
  else
    Result := Name;
end;

function TTargetConfig.LinkMapFile(const BinaryFileName, MapFileName: string;
  var MapFileSize, JclDebugDataSize: Integer): Boolean;
begin
  Result := Owner.LinkMapFile(BinaryFileName, MapFileName, MapFileSize,
    JclDebugDataSize);
end;

function TTargetConfig.CompressMapFileToJdbg(const MapFileName: string): Boolean;
begin
  Result := Owner.CompressMapFileToJdbg(MapFileName);
end;

procedure TTargetConfig.RegisterJVCLVersionInfo;
begin
  InstallJediRegInformation(Target.RegistryKey, 'JVCL', Target.PlatformName,
    Format('%d.%d.%d.%d', [JVCLVersionMajor, JVCLVersionMinor, JVCLVersionRelease, JVCLVersionBuild]),
    DcpDir, BplDir, GetJVCLDir);
end;

procedure TTargetConfig.SavePackagesSettings(ProjectGroup: TProjectGroup);
var
  i: Integer;
  Ini: TMemIniFile;
  IniFileName: string;
begin
  // save to ini
  IniFileName := ChangeFileExt(ParamStr(0), '.ini'); // do not localize
  FileSetReadOnly(IniFileName, False);
  Ini := TMemIniFile.Create(IniFileName);
  try
    Ini.EraseSection(ProjectGroup.BpgName);
    for i := 0 to ProjectGroup.Count - 1 do
      with ProjectGroup.Packages[i] do
        if not ProjectTypeIsDesign(Info.ProjectType) then
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
  Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini')); // do not localize
  try
    for i := 0 to ProjectGroup.Count - 1 do
      if not ProjectTypeIsDesign(ProjectGroup.Packages[i].Info.ProjectType) then
        // Ini read defaults to True, to compile and install newly created packages
        ProjectGroup.Packages[i].Compile := Ini.ReadBool(ProjectGroup.BpgName, ProjectGroup.Packages[i].Info.Name, True)
      else
      begin
        if Ini.ReadBool(ProjectGroup.BpgName + ' Designtime', ProjectGroup.Packages[i].Info.Name, False) then // do not localize
          ProjectGroup.Packages[i].Install := True;
      end;
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
        // shouldn't the following value be False?
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
var
  Pers: string;
begin
  if Target.IsPersonal {or (Kind = pkPersonal)} then
  begin
    if Target.Version = 5 then
      Pers := 's' // do not localize
    else
      Pers := 'p'; // do not localize
  end;
  Result := Format('%s%s', [MainTargetSymbol, Pers]); // do not localize
end;

function TTargetConfig.GetMainTargetSymbol: string;
var
  PlatformPostfix: string;
begin
  PlatformPostfix := '';
  case Target.Platform of
    ctpWin32:
      PlatformPostfix := '';
    ctpWin64:
      PlatformPostfix := '_x64'; // do not localize
  end;
  Result := Format('%s%d%s', [Target.TargetType, Target.Version, PlatformPostfix]); // do not localize
end;

function TTargetConfig.GetUnitOutDir: string;
var
  PlatformStr: string;
begin
  PlatformStr := AnsiLowerCase(Target.PlatformName);
  if PlatformStr <> '' then
    PlatformStr := '\' + PlatformStr;
  Result := GetJVCLDir + Format('\lib\%s%d%s', [Target.TargetType, Target.Version, PlatformStr]); // do not localize
end;

procedure TTargetConfig.SetInstallMode(Value: TInstallMode);
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

function TTargetConfig.GetJclDir: string;
begin
  Result := FJclDir;
end;

function TTargetConfig.GetJclDcpDir: string;
begin
  Result := FJclDcpDir;
end;

function TTargetConfig.GetJclDcuDir: string;
var
  PlatformStr: string;
begin
  { TODO : Keep in sync with JCL naming schema }
  // Note: if the JCL changes its naming convention, a table of equivalences
  // would need to be built. Right now (2006/02/09), this is not necessary
  // as the format is always %type%version.
  PlatformStr := AnsiLowerCase(Target.PlatformName);
  if PlatformStr <> '' then
    PlatformStr := '\' + PlatformStr;
  Result := JclDir + Format('\lib\%s%d%s', [Target.TargetType, Target.Version, PlatformStr]);
end;

function TTargetConfig.GetJclBplDir: string;
begin
  Result := FJclBplDir;
end;

function TTargetConfig.GetHppDir: string;
begin
  Result := FHppDir;
end;

function TTargetConfig.GetDebugUnits: Boolean;
begin
  Result := FDebugUnits;
end;
//----------- Debug directories -------------
function TTargetConfig.GetDebugUnitOutDir: string;
begin
  Result := UnitOutDir + PathDelim + 'debug';
end;

function TTargetConfig.GetDebugBplDir: string;
begin
  Result := UnitOutDir + PathDelim + 'debug';
end;

function TTargetConfig.GetDebugDcpDir: string;
begin
  Result := UnitOutDir + PathDelim + 'debug';
end;

function TTargetConfig.GetDebugHppDir: string;
begin
  Result := HppDir;
end;

function TTargetConfig.GetOutputDirs(DebugUnits: Boolean): TOutputDirs;
begin
  if DebugUnits then
  begin
    Result.UnitOutDir := DebugUnitOutDir;
    Result.BplDir := DebugBplDir;
    Result.DcpDir := DebugDcpDir;
    Result.HppDir := DebugHppDir;
  end
  else
  begin
    Result.UnitOutDir := UnitOutDir;
    Result.BplDir := BplDir;
    Result.DcpDir := DcpDir;
    Result.HppDir := HppDir;
  end;
end;
//-------------------------------------------
function TTargetConfig.GetAutoDependencies: Boolean;
begin
  Result := FAutoDependencies and not Build;
end;

function TTargetConfig.GetBuild: Boolean;
begin
  Result := FBuild;
end;

function TTargetConfig.GetCleanPalettes: Boolean;
begin
  Result := FCleanPalettes;
end;

function TTargetConfig.GetCompileOnly: Boolean;
begin
  Result := FCompileOnly;
end;

function TTargetConfig.GetAddBplDirToPath: Boolean;
begin
  Result := FAddBplDirToPath;
end;

function TTargetConfig.GetGenerateMapFiles: Boolean;
begin
  Result := FGenerateMapFiles;
end;

function TTargetConfig.GetLinkMapFiles: Boolean;
begin
  Result := FLinkMapFiles;
end;

function TTargetConfig.GetLogFileName: string;
begin
  Result := PathAddSeparator(ExtractFilePath(ParamStr(0))) + 'jvcl_install_' + LowerCase(GetMainTargetSymbol) + '.log'; 
end;

function TTargetConfig.GetCreateJdbgFiles: Boolean;
begin
  Result := FCreateJdbgFiles;
end;

function TTargetConfig.GetDeleteMapFiles: Boolean;
begin
  Result := FDeleteMapFiles;
end;

function TTargetConfig.GetBplDir: string;
begin
  Result := FBplDir;
  if Result = '' then
    Result := Target.BplDir;
  if Result = '' then
    Result := '.';
end;

function TTargetConfig.GetDcpDir: string;
begin
  Result := GetUnitOutDir;
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

function TTargetConfig.GetDeveloperInstall: Boolean;
begin
  Result := FDeveloperInstall;
end;

function TTargetConfig.GetDxgettextDir: string;
begin
  Result := Owner.DxgettextDir;
end;

procedure TTargetConfig.Save;
var
  Kind: TPackageGroupKind;
  Ini: TMemIniFile;
  IniFileName: string;
begin
  if JVCLConfig.Modified then
  begin
    FileSetReadOnly(JVCLConfig.Filename, False);
    JVCLConfig.SaveToFile(JVCLConfig.Filename);
  end;
  if JVCLRegistryConfig.Modified then
    JVCLRegistryConfig.SaveToRegistry(Target.RegistryKey + '\Jedi\JVCL');

  for Kind := pkFirst to pkLast do
  begin
    if Frameworks.Items[False, Kind] <> nil then
      SavePackagesSettings(Frameworks.Items[False, Kind]);
    if Frameworks.Items[True, Kind] <> nil then
      SavePackagesSettings(Frameworks.Items[True, Kind]);
  end;

  IniFileName := ChangeFileExt(ParamStr(0), '.ini'); // do not localize
  FileSetReadOnly(IniFileName, False);
  Ini := TMemIniFile.Create(IniFileName);
  try
    if not Target.SupportsPersonalities([persBCB]) or (HppDir = FDefaultHppDir) then
      Ini.DeleteKey(Target.DisplayName, 'HPPDir') // do not localize
    else
      Ini.WriteString(Target.DisplayName, 'HPPDir', HppDir); // do not localize

    if BplDir = Target.BplDir then
      Ini.DeleteKey(Target.DisplayName, 'BPLDir') // do not localize
    else
      Ini.WriteString(Target.DisplayName, 'BPLDir', BplDir); // do not localize

    // delete unused value
    Ini.DeleteKey(Target.DisplayName, 'DCPDir'); // do not localize

    Ini.WriteBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall); // do not localize
    Ini.WriteBool(Target.DisplayName, 'CleanPalettes', CleanPalettes); // do not localize
    for Kind := pkFirst to pkLast do
      Ini.WriteBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode); // do not localize
    Ini.WriteBool(Target.DisplayName, 'AutoDependencies', AutoDependencies); // do not localize
    Ini.WriteBool(Target.DisplayName, 'GenerateMapFiles', GenerateMapFiles); // do not localize
    Ini.WriteBool(Target.DisplayName, 'LinkMapFiles', LinkMapFiles); // do not localize
    Ini.WriteBool(Target.DisplayName, 'DeleteMapFiles', DeleteMapFiles); // do not localize
    Ini.WriteBool(Target.DisplayName, 'DebugUnits', DebugUnits); // do not localize

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
  Filename: string;
  Version: Integer;
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
    HppDir := ExcludeTrailingPathDelimiter(Ini.ReadString(Target.DisplayName, 'HPPDir', HppDir));   // do not localize
    BplDir := ExcludeTrailingPathDelimiter(Ini.ReadString(Target.DisplayName, 'BPLDir', BplDir));   // do not localize
    DeveloperInstall := Ini.ReadBool(Target.DisplayName, 'DeveloperInstall', DeveloperInstall); // do not localize
    CleanPalettes := Ini.ReadBool(Target.DisplayName, 'CleanPalettes', CleanPalettes); // do not localize
    GenerateMapFiles := Ini.ReadBool(Target.DisplayName, 'GenerateMapFiles', GenerateMapFiles); // do not localize
    LinkMapFiles := Ini.ReadBool(Target.DisplayName, 'LinkMapFiles', LinkMapFiles); // do not localize
    DeleteMapFiles := Ini.ReadBool(Target.DisplayName, 'DeleteMapFiles', DeleteMapFiles); // do not localize
    DebugUnits := Ini.ReadBool(Target.DisplayName, 'DebugUnits', DebugUnits); // do not localize
    Mode := [];
    for Kind := pkFirst to pkLast do
      if Ini.ReadBool(Target.DisplayName, 'InstallMode_' + IntToStr(Integer(Kind)), Kind in InstallMode) then // do not localize
        Include(Mode, Kind);

    InstallMode := Mode;
    //AutoDependencies := Ini.ReadBool(Target.DisplayName, 'AutoDependencies', AutoDependencies);

    // fix "Delphi\\Projects" bug
    HppDir := FixBackslashBackslash(HppDir);
    BplDir := FixBackslashBackslash(BplDir);

    // Load jvcl%t.inc. Or the jvclbase.inc if no jvcl%t.inc exists
    if Target.IsBDS then
    begin
      Version := Target.Version;
      if Target.PlatForm = ctpWin32 then
        Filename := GetJVCLDir + '\common\' + Format('jvcl%s%d%s.inc', // do not localize
            [LowerCase(Target.TargetType), Version, ''])
      else if Target.PlatForm = ctpWin64 then
        Filename := GetJVCLDir + '\common\' + Format('jvcl%s%d%s.inc', // do not localize
            [LowerCase(Target.TargetType), Version, '_x64'])
      else
        Filename := GetJVCLDir + '\common\' + Format('jvcl%s%d%s.inc', // do not localize
            [LowerCase(Target.TargetType), Version, AnsiLowerCase(Target.PlatformName)]);
    end
    else
      Filename := GetJVCLDir + '\common\' + Format('jvcl%s%d.inc', // do not localize
          [LowerCase(Target.TargetType), Target.Version]);
    if not FileExists(Filename) then
    begin
      JVCLConfig.LoadFromFile(GetJVCLDir + '\common\jvclbase.inc'); // do not localize
      JVCLConfig.Filename := Filename;
      JVCLConfig.Modified := True; // must be stored
    end
    else
      JVCLConfig.LoadFromFile(Filename);

    // set (hidden) configurations
    JVCLConfig.Enabled['DelphiPersonalEdition'] := Target.IsPersonal; // do not localize
    JVCLConfig.Enabled['JVCL_GENERATE_CPP_PACKAGE_FILES'] := Target.IsBDS and (persBCB in Target.SupportedPersonalities);
    JVCLConfig.Enabled['USE_BDE'] := Target.HasBDE;

    JVCLRegistryConfig.LoadFromRegistry(Target.RegistryKey + '\Jedi\JVCL');

    UpdateOptions;
  finally
    Ini.Free;
  end;
end;

procedure TTargetConfig.EnableOption(const Name: string; Enable: Boolean; ForceEnable: Boolean);
begin
  if Name <> '' then
  begin
    if not ForceEnable then
      JVCLConfig.Enabled[Name] := Enable and JVCLConfig.Enabled[Name]
    else
      JVCLConfig.Enabled[Name] := Enable;
  end;
end;

procedure TTargetConfig.ClearPackageCache(const Key: string; const AStartsWith: string);
var
  Reg: TRegistry;
  Names: TStrings;
  i: Integer;
begin
  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(Target.RegistryKey + '\' + Key, False) then
    begin
      Names := TStringList.Create;
      try
        Reg.GetKeyNames(Names);
        Reg.CloseKey;
        for i := 0 to Names.Count - 1 do
          if StartsWith(Names[i], AStartsWith, True) then
            Reg.DeleteKey(Target.RegistryKey + '\' + Key + '\' + Names[i]);
      finally
        Names.Free;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TTargetConfig.RegisterProjectGroupToIDE(ProjectGroup: TProjectGroup);
var
  PackageIndex, i: Integer;
  KnownPackages, DisabledPackages: TDelphiPackageList;
  Target: TCompileTarget;
  BplFilename: string;
begin
  Target := ProjectGroup.Target;
  KnownPackages := Target.KnownPackages;
  DisabledPackages := Target.DisabledPackages;

  // remove JVCL packages
  for i := DisabledPackages.Count - 1 downto 0 do
    if StartsWith(DisabledPackages.Items[i].Name, 'Jv', True) then // do not localize
      DisabledPackages.Delete(i);

  for i := KnownPackages.Count - 1 downto 0 do
    if StartsWith(KnownPackages.Items[i].Name, 'Jv', True) then // do not localize
      KnownPackages.Delete(i);

  if Target.IsBDS then
    ClearPackageCache('Package Cache', 'Jv'); // do not localize

  for PackageIndex := 0 to ProjectGroup.Count - 1 do
  begin
    BplFilename := ProjectGroup.TargetConfig.BplDir + PathDelim +
                   ProjectGroup.Packages[PackageIndex].TargetName;
    if ProjectGroup.Packages[PackageIndex].Install and
       ProjectTypeIsDesign(ProjectGroup.Packages[PackageIndex].Info.ProjectType) and
       FileExists(BplFilename) then
    begin
      KnownPackages.Add(BplFilename, ProjectGroup.Packages[PackageIndex].Info.Description);
    end;
  end;

  ProjectGroup.Target.SavePackagesLists;
end;

procedure TTargetConfig.RegisterDesigntimePackages;
var
  Kind: TPackageGroupKind;
  i: Integer;
  AllPackages, PackageGroup: TProjectGroup;
begin
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
    RegisterProjectGroupToIDE(AllPackages);
  finally
    AllPackages.Free;
  end;
end;

procedure TTargetConfig.Reinit;
begin
  Init;
end;

procedure TTargetConfig.DoCleanPalette(Reg: TRegistry; const Name: string;
  RemoveEmptyPalettes: Boolean);
var
  Entries, S: string;
  List: TStrings;
  i, ps: Integer;
begin
  Entries := Reg.ReadString(Name);
  List := TStringList.Create;
  try
    ps := 0;
    for i := 1 to Length(Entries) do
      if Entries[i] = ';' then // do not localize
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
      S := S + List[i] + ';'; // do not localize
    // last char is ';'

    if (S <> '') or (not RemoveEmptyPalettes) then
    begin
      if S <> Entries then
        Reg.WriteString(Name, S)
    end
    else
    if StartsWith(Name, 'Jv', True) or StartsWith(Name, 'JEDI', True) then
      Reg.DeleteValue(Name);

  finally
    List.Free;
  end;
end;

procedure TTargetConfig.CleanJVCLPalette(RemoveEmptyPalettes: Boolean);
var
  i: Integer;
  Reg: TRegistry;
  List: TStrings;
begin
  reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(Target.RegistryKey + '\Palette', False) then // do not localize
    begin
      List := TStringList.Create;
      try
        Reg.GetValueNames(List);
        for i := 0 to List.Count - 1 do
          DoCleanPalette(reg, List[i], RemoveEmptyPalettes);
      finally
        List.Free;
      end;
    end;

    if Target.IsBDS then
      ClearPackageCache('Palette\Cache', 'Jv'); // do not localize
  finally
    Reg.Free;
  end;
end;

procedure TTargetConfig.DeinstallJVCL(Progress: TDeinstallProgressEvent;
  DeleteFiles: TDeleteFilesEvent; RealUninstall: Boolean);

  procedure DoProgress(const Text: string; Position, Max: Integer);
  begin
    if Assigned(Progress) then
      Progress(Self, Text, Position, Max);
  end;

var
  MaxSteps: Integer;
  i: Integer;
//  Ini: TMemIniFile;
//  Kind: TPackageGroupKind;
begin
  MaxSteps := 4;
  if not Assigned(DeleteFiles) then
    Dec(MaxSteps);

{**}DoProgress(RsCleaningPalette, 0, MaxSteps);
  if RealUninstall then
    CleanJVCLPalette(True);

  if Target.IsBDS then
    ClearPackageCache('Package Cache', 'Jv'); // do not localize

{**}DoProgress(RsCleaningPathLists, 1, MaxSteps);

  // remove JVCL 1 and 2 directories
  for i := Target.BrowsingPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.BrowsingPaths[i])) <> 0 then // do not localize
      Target.BrowsingPaths.Delete(i);

  for i := Target.SearchPaths.Count - 1 downto 0 do
    if Pos('\jvpack\', AnsiLowerCase(Target.SearchPaths[i])) <> 0 then // do not localize
      Target.SearchPaths.Delete(i);


  // remove JVCL 3 directories
  AddPaths(Target.BrowsingPaths, {Add:=}False, Owner.JVCLDir,
    ['common', 'design', 'run', 'Resources', 'qcommon', 'qdesign', 'qrun']); // do not localize
  AddPaths(Target.SearchPaths, {Add:=}False, Owner.JVCLDir,
    ['common', 'design', 'run', 'Resources', 'qcommon', 'qdesign', 'qrun', // do not localize
    Target.InsertDirMacros(UnitOutDir), UnitOutDir]);
  AddPaths(Target.DebugDcuPaths, {Add:=}False, Owner.JVCLDir,
    [Target.InsertDirMacros(DebugUnitOutDir), DebugUnitOutDir]); // do not localize
  if Target.SupportsPersonalities([persBCB]) then
  begin
    AddPaths(Target.GlobalIncludePaths, {Add:=}False, Owner.JVCLDir,
     [Target.InsertDirMacros(HppDir)]);
    AddPaths(Target.GlobalCppBrowsingPaths, {Add:=}False, Owner.JVCLDir,
      ['run', 'common', 'Resources', Target.InsertDirMacros(UnitOutDir)]); // do not localize, clean up because we had browsing and library path wrong
    AddPaths(Target.GlobalCppLibraryPaths, {Add:=}False, Owner.JVCLDir,
      ['Resources', Target.InsertDirMacros(UnitOutDir)]); // do not localize

    // remove non suffixed hppdir as there was a time when the JVCL did not create the win32/win64 subdirs for HPP files
    if Target.IDEVersion >= 11 then
      AddPaths(Target.GlobalIncludePaths, {Add:=}False, Owner.JVCLDir,
       [Target.InsertDirMacros(StrEnsureNoSuffix(DirDelimiter + LowerCase(Target.PlatformName), HppDir))]);
  end;
  Target.SavePaths;

{**}DoProgress(RsUnregisteringPackages, 2, MaxSteps);
  // remove JVCL packages
  with Target do
  begin
    for i := DisabledPackages.Count - 1 downto 0 do
      if StartsWith(DisabledPackages.Items[i].Name, 'Jv', True) then // do not localize
        DisabledPackages.Delete(i);

    for i := KnownPackages.Count - 1 downto 0 do
      if StartsWith(KnownPackages.Items[i].Name, 'Jv', True) then // do not localize
        KnownPackages.Delete(i);
  end;
  Target.SavePackagesLists;

  if RealUninstall then
  begin
    RemoveJediRegInformation(Target.RegistryKey, 'JVCL', Target.PlatformName);

    // clean ini file
{    Ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini')); // do not localize
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
    end;}

    if Assigned(DeleteFiles) then
    begin
  {**}DoProgress(RsDeletingFiles, 3, MaxSteps);
      DeleteFiles(Self);
    end;
  end;

{**}DoProgress(RsComplete, MaxSteps, MaxSteps);
end;

procedure TTargetConfig.GetPackageBinariesForDeletion(List: TStrings);
var
  Mask: string;
begin
  if Target.Version = 6 then
  begin
    if Target.IsBCB then
      Mask := 'Jv*C' + IntToStr(Target.Version) + '0.*'   // do not localize
    else
      Mask := 'Jv*D' + IntToStr(Target.Version) + '0.*';  // do not localize
  end
  else
    Mask := 'Jv*' + IntToStr(Target.Version) + '0.*';  // do not localize

  FindFiles(BplDir, Mask, False, List,
    ['.bpl', '.dcp', '.lib', '.bpi', '.tds', '.map']);  // do not localize
  if AnsiCompareText(DcpDir, BplDir) <> 0 then
    FindFiles(DcpDir, 'Jv*.*', False, List,             // do not localize
      ['.dcp', '.lib', '.bpi', '.lsp']);                // do not localize
  // in Default directories
  if AnsiCompareText(BplDir, Target.BplDir) <> 0 then
    FindFiles(Target.BplDir, Mask, False, List,
      ['.bpl', '.dcp', '.lib', '.bpi', '.tds', '.map']); // do not localize
  if (AnsiCompareText(DcpDir, BplDir) <> 0) and
     (AnsiCompareText(DcpDir, Target.DcpDir) <> 0) then
    FindFiles(Target.DcpDir, 'Jv*.*', False, List,       // do not localize
      ['.dcp', '.lib', '.bpi', '.lsp']);                 // do not localize
end;

function TTargetConfig.GetPathEnvVar: string;

  function LongName(const Filename: string): string;
  begin
    Result := Filename;
    if Pos('~', Filename) > 0 then
      Result := ExcludeTrailingPathDelimiter(PathGetLongName(Filename));
  end;

  function CompareDir(Dir: string; const LongNameDir: string): Boolean;
  begin
    Dir := LongName(Dir);
    Result := AnsiSameText(Dir, LongNameDir);
  end;

var
  List: TStrings;
  i, k: Integer;
  Dir: string;
begin
  List := TStringList.Create;
  try
    StrToPathList(Target.EnvPath, List);
    for i := List.Count - 1 downto 0 do
    begin
      if not DirectoryExists(ExcludeTrailingPathDelimiter(List[i])) then
        List.Delete(i)
      else
      begin
        Dir := LongName(List[i]);
        for k := 0 to Owner.Targets.Count - 1 do
        begin
          if CompareDir(Owner.Targets[k].BplDir, Dir) or
             CompareDir(Owner.Targets[k].DcpDir, Dir) then
          begin
            if not CompareDir(Target.BplDir, Dir) and
               not CompareDir(Target.DcpDir, Dir) and
               not CompareDir(BplDir, Dir) and
               not CompareDir(DcpDir, Dir) then
            begin
              List.Delete(i);
              Break;
            end;
          end;
        end;
      end;
    end;
    Result := PathListToStr(List);
  finally
    List.Free;
  end;
end;

{$I InstalledPackages.inc}

end.
