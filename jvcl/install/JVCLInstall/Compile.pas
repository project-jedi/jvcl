{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: Compile.pas, released on 2004-03-29.

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

unit Compile;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, SysUtils, Classes, CapExec, JVCLData, DelphiData,
  GenerateUtils, PackageUtils, Intf, PackageInformation, ConditionParser;

type
  TProgressKind = (
    pkTarget,           // progress of all targets
    pkProject,          // |- progress of the parts of one target compilation
    pkResource,         //    |- progress of the resource compilation
    pkPackage,          //    |- progress of the package compilation
    pkOther             //    |- progress for copy/delete and other things
  );

  TTargetProgressEvent = procedure(Sender: TObject; Current: TTargetConfig;
    Position, Max: Integer) of object;
  TPackageProgressEvent = procedure(Sender: TObject; Current: TPackageTarget;
    const Text: string; Position, Max: Integer) of object;
  TResourceProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer) of object;
  TProjectProgressEvent = TResourceProgressEvent;
  TProgressEvent = procedure(Sender: TObject; const Text: string;
    Position, Max: Integer; Kind: TProgressKind) of object;

  TCompiler = class(TObject)
  private
    FData: TJVCLData;
    FCurrentProjectGroup: TProjectGroup;
    FAborted: Boolean;
    FOutput: TStrings;

    FCount: Integer;

    FPkgCount: Integer; // number of packages to compile
    FPkgIndex: Integer;

    FResCount: Integer;
    FResIndex: Integer;

    FOnCaptureLine: TCaptureLine;
    FOnTargetProgress: TTargetProgressEvent;
    FOnPackageProgress: TPackageProgressEvent;
    FOnResourceProgress: TResourceProgressEvent;
    FOnProjectProgress: TProjectProgressEvent;
    FOnProgress: TProgressEvent;
    FOnIdle: TNotifyEvent;

    FAbortReason: string;
    FQuiet: string;

    function IsPackageUsed(ProjectGroup: TProjectGroup;
      RequiredPackage: TRequiredPackage): Boolean;
    function IsFileUsed(ProjectGroup: TProjectGroup;
      ContainedFile: TContainedFile): Boolean;
  protected
    function Make(TargetConfig: ITargetConfig; Args: string;
      CaptureLine: TCaptureLine; StartDir: string = ''): Integer;
    procedure DoIdle(Sender: TObject);

    procedure CaptureLine(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineClean(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
    procedure CaptureLinePackageCompilation(const Line: string; var Aborted: Boolean);
    procedure CaptureLineResourceCompilation(const Line: string; var Aborted: Boolean);

    procedure DoTargetProgress(Current: TTargetConfig; Position, Max: Integer); virtual;
    procedure DoProjectProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoResourceProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoPackageProgress(Current: TPackageTarget; const Text: string;
      Position, Max: Integer); virtual;

    procedure DoProgress(const Text: string; Position, Max: Integer;
      Kind: TProgressKind); virtual;
      // DoProgress is called by every DoXxxProgress method

    function CompileProjectGroup(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
    procedure CreateProjectGroupMakefile(ProjectGroup: TProjectGroup; AutoDepend: Boolean);
    function GenerateResources(TargetConfig: ITargetConfig): Boolean;
    function DeleteFormDataFiles(ProjectGroup: TProjectGroup): Boolean;
    function CopyFormDataFiles(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;

    function IsCondition(const Condition: string; TargetConfig: ITargetConfig): Boolean;
  public
    constructor Create(AData: TJVCLData);
    destructor Destroy; override;

    function GeneratePackages(const Group, Targets, PackagesPath: string): Boolean; overload;
    function GenerateAllPackages: Boolean; overload;
    function Compile: Boolean;
    function CompileTarget(TargetConfig: TTargetConfig; DoClx: Boolean): Boolean;

    procedure Abort; // abort compile process

    property AbortReason: string read FAbortReason write FAbortReason;
    property Data: TJVCLData read FData;
    property Output: TStrings read FOutput;

    property OnCaptureLine: TCaptureLine read FOnCaptureLine write FOnCaptureLine;
    property OnTargetProgress: TTargetProgressEvent read FOnTargetProgress write FOnTargetProgress;
    property OnPackageProgress: TPackageProgressEvent read FOnPackageProgress write FOnPackageProgress;
    property OnResourceProgress: TResourceProgressEvent read FOnResourceProgress write FOnResourceProgress;
    property OnProjectProgress: TProjectProgressEvent read FOnProjectProgress write FOnProjectProgress;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;

const
  ProjectMax = 7;

var
  Compiler: TCompiler = nil;
  StartupEnvVarPath: string;

resourcestring
  RsPackagesAreUpToDate = 'Packages are up to date';

implementation

uses
  CmdLineUtils, JvConsts, Utils, Core;

resourcestring
  RsGeneratingTemplates = 'Generating templates...';
  RsGeneratingPackages = 'Generating packages...';
  RsGeneratingResources = 'Generating resources...';
  RsCompilingPackages = 'Compiling packages...';
  RsCopyingFiles = 'Copying files...';
  RsCopyingFile = 'Copying %s';
  RsFinished = 'Finished.';

  RsAbortedByUser = 'Aborted by User';
  RsErrorLoadingPackageGeneratorConfigFile = 'Error loading devtools\bin\pgEdit.xml';
  RsErrorGeneratingPackages = 'Error while generating packages for %s';
  RsErrorGeneratingTemplates = 'Error while generating templates.';
  RsErrorCompilingResources = 'Error while compiling resources.';
  RsErrorGeneratingTemplatesForDir = 'Error generating templates for the %s directory.';
  RsErrorCompilingPackages = 'An error occured while compiling the packages.'; // this must not be the doubt of the installer

  RsCommandNotFound = 'Command could not be executed.'#10#10#10'Cmdline: %s'#10#0'Start directory: %s';

const
  RsGeneratePackages = '[Generating: Packages]'; // do not localize

const
  CommonDependencyFiles: array[0..5] of string = (
    'jvcl.inc', 'jvclbase.inc', 'jvcl%t.inc', 'jedi.inc', 'linuxonly.inc', 'windowsonly.inc'
  );

{ TCompiler }

constructor TCompiler.Create(AData: TJVCLData);
begin
  inherited Create;
  FData := AData;
  FOutput := TStringList.Create;
end;

destructor TCompiler.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

procedure TCompiler.DoTargetProgress(Current: TTargetConfig; Position,
  Max: Integer);
begin
  if Assigned(FOnTargetProgress) then
    FOnTargetProgress(Self, Current, Position, Max);
  DoProgress(Current.Target.DisplayName, Position, Max, pkTarget);
end;

procedure TCompiler.DoProjectProgress(const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnProjectProgress) then
    FOnProjectProgress(Self, Text, Position, Max);
  DoProgress(Text, Position, Max, pkProject);
end;

procedure TCompiler.DoResourceProgress(const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnResourceProgress) then
    FOnResourceProgress(Self, Text, Position, Max);
  DoProgress(Text, Position, Max, pkResource);
end;

procedure TCompiler.DoPackageProgress(Current: TPackageTarget;
  const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnPackageProgress) then
    FOnPackageProgress(Self, Current, Text, Position, Max);
  DoProgress(Text, Position, Max, pkPackage);
end;

procedure TCompiler.DoProgress(const Text: string; Position, Max: Integer;
  Kind: TProgressKind);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Text, Position, Max, Kind);
end;

procedure TCompiler.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  FOutput.Add(Line);
  if Assigned(FOnCaptureLine) then
    FOnCaptureLine(Line, FAborted);
  Aborted := FAborted;
end;

procedure TCompiler.CaptureLineClean(const Line: string; var Aborted: Boolean);
begin
  if StartsWith('[', Line) then
    CaptureLine(Line, Aborted);
end;

procedure TCompiler.CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
begin
  if StartsWith(Trim(Line), 'echo [Compiling: ', True) then
    Inc(FCount)
  else if (Line <> '') and (Line[1] <> #9) then
    CaptureLine(Line, FAborted);
  Aborted := FAborted;
end;

procedure TCompiler.CaptureLinePackageCompilation(const Line: string; var Aborted: Boolean);
var
  S: string;
  i: Integer;
begin
  CaptureLine(Line, Aborted);
  if (Line <> '') and (Line[1] = '[') then
  begin
    if StartsWith(Line, '[Compiling: ', True) then
    begin
      Inc(FPkgIndex);
      S := Trim(Copy(Line, 13, Length(Line) - 13));

      for i := 0 to FCurrentProjectGroup.Count - 1 do
        if CompareText(FCurrentProjectGroup.Packages[i].TargetName, S) = 0 then
        begin
          S := S + '  (' + FCurrentProjectGroup.Packages[i].Info.Description + ')';
          DoPackageProgress(FCurrentProjectGroup.Packages[i], S, FPkgIndex - 1, FPkgCount);
          Exit;
        end;
      DoPackageProgress(nil, S, FPkgIndex, FPkgCount);
    end;
  end;
end;

procedure TCompiler.CaptureLineResourceCompilation(const Line: string;
  var Aborted: Boolean);
var
  S: string;
begin
  CaptureLine(Line, Aborted);
  if (Line <> '') and (Line[1] = '[') then
  begin
    if StartsWith(Line, '[Compiling: ', True) then
    begin
      Inc(FResIndex);
      S := Trim(Copy(Line, 15, Length(Line) - 15));
      DoResourceProgress(S, FResIndex, FResCount);
    end;
  end;
end;

procedure TCompiler.Abort;
begin
  FAborted := True;
end;

procedure WriteMsg(const Text: string); // used by TCompiler.GeneratePackages
begin
  Compiler.CaptureLine(Text, Compiler.FAborted);
end;

/// <summary>
/// Make calls the make.exe of the given TargetConfig. If the StartDir is empty
/// the JVCLPackageDir\bin directory is used. If the command could not be
/// executed a message dialog is shown with the complete command line.
/// </summary>
function TCompiler.Make(TargetConfig: ITargetConfig; Args: string;
  CaptureLine: TCaptureLine; StartDir: string): Integer;
begin
  if StartDir = '' then
    StartDir := Data.JVCLPackagesDir + '\bin';

  if Data.IgnoreMakeErrors then
    Args := Trim('-i ' + Args);

  if Data.Verbose then
  begin
    // output command line
    if Assigned(CaptureLine) then
      CaptureLine(#1 + '"' + TargetConfig.Target.Make + '" ' + Args, FAborted);
  end;

  Result := CaptureExecute('"' + TargetConfig.Target.Make + '"', Args,
                           StartDir, CaptureLine, DoIdle);
  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.Make + '"' + Args, StartDir])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

procedure TCompiler.DoIdle(Sender: TObject);
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

/// <summary>
/// GeneratePackages generates the packages in
/// PackagesPath for the Group (JVCL) for the Targets (comma separated
/// pg.exe target list).
/// </summary>
function TCompiler.GeneratePackages(const Group, Targets, PackagesPath: string): Boolean;
var
  ErrMsg: string;
  List, TargetList: TStrings;
begin
  Result := False;

  FAborted := False;
  CaptureLine(RsGeneratePackages, FAborted);
  if FAborted then
  begin
    AbortReason := RsAbortedByUser;
    Exit;
  end;

  try
    if not LoadConfig(Data.JVCLDir + '\' + sPackageGeneratorFile, Group, ErrMsg) then
    begin
      CaptureLine(ErrMsg, FAborted);
      AbortReason := RsErrorLoadingPackageGeneratorConfigFile;
      Exit;
    end;
  except
    on E: Exception do
    begin
      AbortReason := RsErrorLoadingPackageGeneratorConfigFile + #10#10 + E.Message;
      Exit;
    end;
  end;

  List := TStringList.Create;
  TargetList := TStringList.Create;
  try
    TargetList.CommaText := Targets;
    ExpandTargetsNoPerso(TargetList);
    EnumeratePackages(PackagesPath, List);
    if not Generate(List, TargetList, WriteMsg, Data.JVCLDir + '\' + sPackageGeneratorFile,
                    Group, ErrMsg, PackagesPath, '', '', Data.JVCLDir + '\common\jvcl%t.inc') then
    begin
      CaptureLine(ErrMsg, FAborted);
      AbortReason := Format(RsErrorGeneratingPackages, [TargetList.CommaText]);
      Exit;
    end;
  finally
    List.Free;
    TargetList.Free;
  end;
  Result := True;
end;

/// <summary>
/// GenerateAllPackages generates all JVCL packages
/// </summary>
function TCompiler.GenerateAllPackages: Boolean;
begin
  Result := GeneratePackages('JVCL', 'all', Data.JVCLPackagesDir);
end;

function TCompiler.Compile: Boolean;
var
  i, Index: Integer;
  Frameworks, Count: Integer;
  TargetConfigs: array of TTargetConfig;
  SysInfo: string;
begin
  Result := True;
  if Data.Verbose then
    FQuiet := ''
  else
    FQuiet := ' -s';

  SysInfo := '';
  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        case Win32MinorVersion of
          0..9: SysInfo := 'Windows 95';
          10..89: SysInfo := 'Windows 98';
          90: SysInfo := 'Windows ME';
        end;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        case Win32MajorVersion of
          4: SysInfo := 'Windows NT4';
          5:
            begin
              case Win32MinorVersion of
                0: SysInfo := 'Windows 2000';
                1: SysInfo := 'Windows XP';
              end;
            end;
        end;
      end;
  end;

  if SysInfo <> '' then
  begin
    SysInfo := #1 + SysInfo + Format(' %s (%d.%d.%d)',
      [Win32CSDVersion, Win32MajorVersion, Win32MinorVersion, Win32BuildNumber]);

    CaptureLine(SysInfo, FAborted);
    CaptureLine('', FAborted);
  end;

  AbortReason := '';
  // read target configs that should be compiled
  Count := 0;
  Frameworks := 0;
  SetLength(TargetConfigs, Data.Targets.Count);
  for i := 0 to High(TargetConfigs) do
  begin
    if Data.TargetConfig[i].InstallJVCL then
    begin
      with Data.TargetConfig[i] do
      begin
        if not Target.IsBCB then
        begin
          // Delphi requires .bpl files
          if not FileExists(Format('%s\DJcl%d0.bpl', [BplDir, Target.Version])) and
             not FileExists(Format('%s\DJcl%d0.bpl', [Target.BplDir, Target.Version])) then
            Continue; // do not install JVCL when no JCL is installed
        end;
      end;
      TargetConfigs[Count] := Data.TargetConfig[i];
      Inc(Count);
      if pkVCL in Data.TargetConfig[i].InstallMode then
        Inc(Frameworks);
      if pkCLX in Data.TargetConfig[i].InstallMode then
        Inc(Frameworks);
    end;
  end;
  SetLength(TargetConfigs, Count);

  // compile all targets
  Index := 0;
  for i := 0 to Count - 1 do
  begin
    DoTargetProgress(TargetConfigs[i], Index, Frameworks);
    if pkVCL in TargetConfigs[i].InstallMode then
    begin
      Result := CompileTarget(TargetConfigs[i], {CLX:=}False);
      if not Result then
        Break;
      Inc(Index);
    end;
    DoTargetProgress(TargetConfigs[i], Index, Frameworks);
    if pkClx in TargetConfigs[i].InstallMode then
    begin
      Result := CompileTarget(TargetConfigs[i], {CLX:=}True);
      if not Result then
        Break;
      Inc(Index);
    end;
    DoTargetProgress(TargetConfigs[i], Index, Frameworks);
  end;
end;

/// <summary>
/// CompileTarget starts CompileProjectGroup for all sub targets of the
/// given target IDE.
/// </summary>
function TCompiler.CompileTarget(TargetConfig: TTargetConfig; DoClx: Boolean): Boolean;
var
  ObjFiles: TStrings;
  i: Integer;
  Aborted: Boolean;
begin
  Result := True;
  Aborted := False;
  FOutput.Clear;

  if TargetConfig.Target.IsBCB and TargetConfig.Build then // CLX for BCB is not supported
  begin
    // Delete all .obj and .dcu files because dcc32.exe -JPHNE does not create new .obj
    // files if they already exist. And as a result interface changes in a unit
    // let the bcc32.exe compiler fail.
    ObjFiles := TStringList.Create;
    try
      FindFiles(TargetConfig.UnitOutDir, '*.*', True, ObjFiles, ['.obj', '.dcu']);
      for i := 0 to ObjFiles.Count - 1 do
        DeleteFile(ObjFiles[i]);
    finally
      ObjFiles.Free;
    end;
  end;

  // VCL
  if Result and (pkVCL in TargetConfig.InstallMode) and not DoClx then
  begin
    if not TargetConfig.DeveloperInstall then
    begin
      // debug units
      if (not TargetConfig.Target.IsBCB) and TargetConfig.DebugUnits then
        Result := CompileProjectGroup(
          TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], True);
    end;

    if Result then
      // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], False);

    if Result or not CmdOptions.KeepFiles then
      Make(TargetConfig, FQuiet + ' Clean', CaptureLineClean);
    if Result then
      CaptureLine('[Finished JVCL for VCL installation]', Aborted); // do not localize
  end;

  // CLX
  if Result and (pkClx in TargetConfig.InstallMode) and DoClx then
  begin
    if not FileExists(TargetConfig.BplDir + '\clxdesigner.dcp') then
    begin
      // Delphi 7 has no clxdesigner.dcp so we compile it from Delphi's property
      // editor source.
      CaptureExecute(Data.JVCLPackagesDir + '\bin\MakeClxDesigner.bat', '',
        Data.JVCLPackagesDir + '\bin', CaptureLine, nil, False);
    end;

    if not TargetConfig.DeveloperInstall then
    begin
      // debug units
      if (not TargetConfig.Target.IsBCB) and TargetConfig.DebugUnits then
        Result := CompileProjectGroup(
          TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], True);
    end;

    if Result then
      // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], False);

    if Result or not CmdOptions.KeepFiles then
      Make(TargetConfig, FQuiet + ' Clean', CaptureLineClean);
    if Result then
      CaptureLine('[Finished JVCL for CLX installation]', Aborted); // do not localize
  end;
end;

/// <summary>
/// GenerateResources starts the make file for the resource file compilation.
/// </summary>
function TCompiler.GenerateResources(TargetConfig: ITargetConfig): Boolean;
begin
  Result := False;

  // get number of resources to compile
  FCount := 0;
  if Make(TargetConfig, '-f makefile.mak -n', CaptureLineGetCompileCount, TargetConfig.JVCLDir + '\images') <> 0 then
  begin
    AbortReason := RsErrorCompilingResources;
    Exit;
  end;
  // update FResCount with the number of resources that MAKE will compile
  FResCount := FCount;

  FResIndex := 0;
  if FCount > 0 then
  begin
    DoResourceProgress('', 0, FResCount);
    // generate .res and .dcr files
    if Make(TargetConfig, '-f makefile.mak', CaptureLineResourceCompilation, TargetConfig.JVCLDir + '\images') <> 0 then
    begin
      AbortReason := RsErrorCompilingResources;
      Exit;
    end;
    DoResourceProgress('', FResCount, FResCount);
  end;
  Result := True;
end;

/// <summary>
/// DeleteFormDataFiles deletes the .dfm, .xfm files from the lib-path.
/// </summary>
function TCompiler.DeleteFormDataFiles(ProjectGroup: TProjectGroup): Boolean;
var
  Files: TStrings;
  i: Integer;
  Dir: string;
begin
  Result := True;
  Files := TStringList.Create;
  try
    Dir := ProjectGroup.TargetConfig.UnitOutDir;
    FindFiles(Dir, '*.*', False, Files, ['.dfm', '.xfm']);
    for i := 0 to Files.Count - 1 do
      DeleteFile(Files[i]);
  finally
    Files.Free;
  end;
end;

/// <summary>
/// CopyFormDataFiles copies the .dfm, .xfm files to the lib-path.
/// This function is only called for non developer installations.
/// </summary>
function TCompiler.CopyFormDataFiles(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
var
  Files: TStrings;
  i: Integer;
  Dir, DestDir, DestFile, DebugDestDir: string;
begin
  Result := True;
  Files := TStringList.Create;
  try
{**}DoProgress('', 0, 100, pkOther);

    DestDir := ProjectGroup.TargetConfig.UnitOutDir;
    DebugDestDir := DestDir + PathDelim + 'debug';

    if ProjectGroup.IsVCLX then
    begin
      Dir := ProjectGroup.TargetConfig.JVCLDir + PathDelim + 'qrun';
      FindFiles(Dir, '*.xfm', False, Files, ['.xfm']);
    end
    else
    begin
      Dir := ProjectGroup.TargetConfig.JVCLDir + PathDelim + 'run';
      FindFiles(Dir, '*.dfm', False, Files, ['.dfm']);
    end;

    CaptureLine(RsCopyingFiles, FAborted);
    for i := 0 to Files.Count - 1 do
    begin
      DestFile := DestDir + PathDelim + ExtractFileName(Files[i]);
      if FileAge(Files[i]) > FileAge(DestFile) then
      begin
{**}    DoProgress(ExtractFileName(Files[i]), i, Files.Count, pkOther);
        CopyFile(PChar(Files[i]), PChar(DestFile), False);
      end;
      if DebugUnits then
      begin
        DestFile := DebugDestDir + PathDelim + ExtractFileName(Files[i]);
        if FileAge(Files[i]) > FileAge(DestFile) then
          CopyFile(PChar(Files[i]), PChar(DestFile), False);
      end;
    end;
{**}  DoProgress('', 0, Files.Count, pkOther);
  finally
    Files.Free;
  end;
end;

function GetWindowsDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetWindowsDirectory(PChar(Result), Length(Result)));
end;

function GetSystemDir: string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(Result), Length(Result)));
end;

/// <summary>
/// CompileProjectGroup starts the make file for the templates, starts the
/// packages generator, calls the GenerateResource method and compiles all
/// selected packages of the project group.
/// </summary>
function TCompiler.CompileProjectGroup(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
var
  AProjectIndex, i: Integer;
  Args: string;
  Edition, PkgDir, JVCLPackagesDir: string;
  AutoDepend: Boolean;
  TargetConfig: ITargetConfig;
  DccOpt: string;
  Path: string;
  PathList, BplPaths: TStringList;
  S, SearchPaths: string;

  function GetProjectIndex: Integer;
  begin
    Result := AProjectIndex;
    Inc(AProjectIndex);
  end;

begin
  // ClearEnvironment; // remove almost all environment variables for "make.exe long command line"
  // ahuser (2005-01-22): make.exe fails only if a path with spaces is in the PATH envvar

  SetEnvironmentVariable('MAKEOPTIONS', nil);
  Result := False;
  FCurrentProjectGroup := ProjectGroup;
  try
    TargetConfig := ProjectGroup.TargetConfig;
    AutoDepend := TargetConfig.AutoDependencies;

    // obtain information for progress bar
    FPkgCount := 0;
    for i := 0 to ProjectGroup.Count - 1 do
      if ProjectGroup.Packages[i].Compile then
        Inc(FPkgCount);

    Edition := TargetConfig.TargetSymbol;
    if ProjectGroup.IsVCLX then
      Edition := Edition + 'clx';
    JVCLPackagesDir := TargetConfig.JVCLPackagesDir;
    Args := '-f Makefile.mak';

    PkgDir := Edition;
    if not ProjectGroup.IsVCLX then // only PRO and ENT versions have CLX support
    begin
      if PkgDir[3] in ['p', 'P', 's', 'S'] then
      begin
        if PkgDir[2] = '5' then
          PkgDir := Copy(PkgDir, 1, 2) + 'std'
        else
          PkgDir := Copy(PkgDir, 1, 2) + 'per';
      end;
    end;

    { dcc32.exe crashes if the path is too long (> 100 + line number, ...}
    if Length(JVCLPackagesDir) < 100 then DccOpt := '-Q- -M' else DccOpt := '-Q -M';
    // setup environment variables
    if TargetConfig.Build then
      if Length(JVCLPackagesDir) < 100 then DccOpt := '-Q- -M -B' else DccOpt := '-Q -M -B';
    if TargetConfig.GenerateMapFiles then
      DccOpt := DccOpt + ' -GD';

    if (not DebugUnits) then
      DccOpt := DccOpt + ' -DJVCL_NO_DEBUGINFO'
    else
      { DeveloperInstall always use Debug units in the Jvcl\Lib\xx directory }
    if not TargetConfig.DeveloperInstall then
      CreateDir(TargetConfig.UnitOutDir + '\debug');

    { set PATH envvar and add all directories that contain .bpl files }
    PathList := TStringList.Create;
    try
      PathList.Duplicates := dupIgnore;

      PathList.Add(GetWindowsDir);
      PathList.Add(GetSystemDir);
      PathList.Add(GetWindowsDir + '\Command'); // Win9x

      PathList.Add(ExtractShortPathName(TargetConfig.Target.RootDir));
      PathList.Add(ExtractShortPathName(TargetConfig.BplDir));
      PathList.Add(ExtractShortPathName(TargetConfig.DcpDir));
      { Add original BPL directory for "common" BPLs, but add it as the very
        last path to prevent collisions between packages in TargetConfig.BplDir
        and Target.BplDir. }
      PathList.Add(ExtractShortPathName(TargetConfig.Target.BplDir));

      { Add paths with .bpl files from the PATH environment variable }
      BplPaths := TStringList.Create;
      try
        StrToPathList(StartupEnvVarPath, BplPaths);
        for i := 0 to BplPaths.Count - 1 do
          if DirContainsFiles(ExcludeTrailingPathDelimiter(BplPaths[i]), '*.bpl') then
            PathList.Add(ExtractShortPathName(ExcludeTrailingPathDelimiter(BplPaths[i])));
      finally
        BplPaths.Free;
      end;

      Path := PathListToStr(PathList);
    finally
      PathList.Free;
    end;

    if TargetConfig.Target.Version > 6 then // Overcome make.exe "command line too long" bug
    begin
      SearchPaths := '';
      for i := 0 to TargetConfig.Target.SearchPaths.Count - 1 do
      begin
        S := ExtractShortPathName(ExcludeTrailingPathDelimiter(TargetConfig.Target.ExpandDirMacros(TargetConfig.Target.SearchPaths[i])));
        if SearchPaths <> '' then
          SearchPaths := SearchPaths + ';' + S
        else
          SearchPaths := S;
      end;
    end
    else
      SearchPaths := '.';

    SetEnvironmentVariable('PATH', PChar(Path));
    SetEnvironmentVariable('DCCOPT', Pointer(DccOpt));
    // especially for BCB generated make file
    SetEnvironmentVariable('DCC', PChar('"' + TargetConfig.Target.RootDir + '\bin\dcc32.exe" ' + DccOpt));
    SetEnvironmentVariable('UNITDIRS', PChar(SearchPaths));

    SetEnvironmentVariable('QUIET', Pointer(Copy(FQuiet, 2, MaxInt))); // make command line option " -s"

    SetEnvironmentVariable('TARGETS', nil); // we create our own makefile so do not allow a user defined TARGETS envvar
    SetEnvironmentVariable('MASTEREDITION', nil);

    SetEnvironmentVariable('ROOT', Pointer(TargetConfig.Target.RootDir));
    SetEnvironmentVariable('JCLROOT', Pointer(TargetConfig.JCLDir));
    SetEnvironmentVariable('JVCLROOT', Pointer(TargetConfig.JVCLDir));
    SetEnvironmentVariable('VERSION', Pointer(IntToStr(TargetConfig.Target.Version)));
    if DebugUnits then
      SetEnvironmentVariable('UNITOUTDIR', Pointer(TargetConfig.UnitOutDir + '\debug'))
    else
      SetEnvironmentVariable('UNITOUTDIR', Pointer(TargetConfig.UnitOutDir));
    SetEnvironmentVariable('MAINBPLDIR', Pointer(TargetConfig.Target.BplDir));
    SetEnvironmentVariable('MAINDCPDIR', Pointer(TargetConfig.Target.DcpDir));
    SetEnvironmentVariable('MAINLIBDIR', Pointer(TargetConfig.Target.DcpDir)); // for BCB
    SetEnvironmentVariable('BPLDIR', Pointer(TargetConfig.BplDir));
    SetEnvironmentVariable('DCPDIR', Pointer(TargetConfig.DcpDir));
    SetEnvironmentVariable('LIBDIR', Pointer(TargetConfig.DcpDir));  // for BCB
    SetEnvironmentVariable('HPPDIR', Pointer(TargetConfig.HppDir)); // for BCB
    SetEnvironmentVariable('BPILIBDIR', Pointer(TargetConfig.DcpDir)); // for BCB

    // add dxgettext unit directory
    { Dxgettext is now included in the JVCL as JvGnugettext
    if Data.JVCLConfig.Enabled['USE_DXGETTEXT'] then
      SetEnvironmentVariable('EXTRAUNITDIRS', Pointer(TargetConfig.DxgettextDir))
    else}
      SetEnvironmentVariable('EXTRAUNITDIRS', nil);
    SetEnvironmentVariable('EXTRAINCLUDEDIRS', nil);
    SetEnvironmentVariable('EXTRARESDIRS', nil);

    // *****************************************************************

{**}DoProjectProgress(RsGeneratingPackages, GetProjectIndex, ProjectMax);
    if ProjectGroup.Target.IsPersonal then
    begin
      // generate template.cfg for the "master" PkgDir
      SetEnvironmentVariable('EDITION', PChar(Copy(Edition, 1, 2)));
      SetEnvironmentVariable('PKGDIR', PChar(Copy(PkgDir, 1, 2)));
      SetEnvironmentVariable('PKGDIR_MASTEREDITION', PChar(Copy(PkgDir, 1, 2)));
      if Make(TargetConfig, Args + ' Templates', CaptureLine) <> 0 then
      begin
        AbortReason := Format(RsErrorGeneratingTemplatesForDir, [Copy(PkgDir, 1, 2)]);
        Exit;
      end;
    end;

    // generate template.cfg file for PkgDir
    SetEnvironmentVariable('EDITION', PChar(Edition));
    SetEnvironmentVariable('PKGDIR', PChar(PkgDir));
    SetEnvironmentVariable('PKGDIR_MASTEREDITION', PChar(PkgDir));
    if Make(TargetConfig, Args + ' Templates', CaptureLine) <> 0 then
    begin
      AbortReason := Format(RsErrorGeneratingTemplatesForDir, [PkgDir]);
      Exit;
    end;

    // *****************************************************************

{**}DoProjectProgress(RsGeneratingPackages, GetProjectIndex, ProjectMax);
    if ProjectGroup.Target.IsPersonal then
    begin
      // generate the packages and .cfg files for the "master" PkgDir
      if not GeneratePackages('JVCL', Copy(Edition, 1, 2),
                              TargetConfig.JVCLPackagesDir) then
        Exit; // AbortReason is set in GeneratePackages
    end;

    // generate the packages and .cfg files for PkgDir
    if not GeneratePackages('JVCL', Edition, TargetConfig.JVCLPackagesDir) then
      Exit; // AbortReason is set in GeneratePackages

   // *****************************************************************

{**}DoProjectProgress(RsGeneratingResources, GetProjectIndex, ProjectMax);
    if not GenerateResources(TargetConfig) then
      Exit; // AbortReason is set in GenerateResources

   // *****************************************************************

{**}DoProjectProgress(RsCompilingPackages, GetProjectIndex, ProjectMax);
    FPkgIndex := 0;
    if FPkgCount > 0 then
    begin
      // ==== changed Resources ====
      // As long as no dependency information about resources is in the .xml
      // files we let the Delphi compiler decide if he wants to compile the
      // package.
      if (FResCount > 0) then
        AutoDepend := False;
      // ===========================

      { Now it is time to write the "xx Packages.mak" file. }
      CreateProjectGroupMakefile(ProjectGroup, AutoDepend);

      { Are there any packages that have to be compiled? Ask make.exe for this
        information. }
      if AutoDepend then
      begin
        if not TargetConfig.DeveloperInstall and TargetConfig.DebugUnits and not DebugUnits then
          SetEnvironmentVariable('MAKEOPTIONS', '-B -n') { make a complete make pass when the release units
                                                           should be compiled while TargetConfig.DebugUnits are active }
        else
          SetEnvironmentVariable('MAKEOPTIONS', '-n');

        // get the number of packages that needs compilation
        FCount := 0;
        if Make(TargetConfig, Args + ' CompilePackages', CaptureLineGetCompileCount) <> 0 then
        begin
          AbortReason := RsErrorCompilingPackages;
          Exit;
        end;
        // update FPkgCount with the number of packages that MAKE will compile
        FPkgCount := FCount;
      end;
      SetEnvironmentVariable('MAKEOPTIONS', nil);
      if not TargetConfig.DeveloperInstall and TargetConfig.DebugUnits and not DebugUnits then
        SetEnvironmentVariable('MAKEOPTIONS', '-B'); { make a complete make pass when the release units
                                                       should be compiled while TargetConfig.DebugUnits are active }

      if FPkgCount > 0 then
      begin
        { Remove .dfm/.xfm files from the lib directory so the compiler takes the
          correct one and we do not have unused files in the lib directory. }
        DeleteFormDataFiles(ProjectGroup);

        { Now compile the packages }
        DoPackageProgress(nil, '', 0, FPkgCount);
        // compile packages
        if Make(TargetConfig, Args + FQuiet + ' CompilePackages', CaptureLinePackageCompilation) <> 0 then
        begin
          AbortReason := RsErrorCompilingPackages;
          Exit;
        end;
        DoPackageProgress(nil, '', FPkgCount, FPkgCount);
      end
      else
        CaptureLine(RsPackagesAreUpToDate, FAborted);
    end;

    // *****************************************************************

    if (FPkgCount > 0) and
       ((not ProjectGroup.TargetConfig.DeveloperInstall) or
        (TargetConfig.Target.IsBCB)) then
    begin
{**}  DoProjectProgress(RsCopyingFiles, GetProjectIndex, ProjectMax);
      { The .dfm/.xfm files are deleted from the lib directory in the
        resource generation section in this method.
        The files are only copied for a non-developer installation and for
        BCB. }
      CopyFormDataFiles(ProjectGroup, DebugUnits);
    end
    else
{**}  GetProjectIndex; // increase progress

  finally
{**}DoProjectProgress(RsFinished, ProjectMax, ProjectMax);
    FCurrentProjectGroup := nil;
  end;
  Result := True;

  { Delete the generated "xx Package.mak" file. }
  DeleteFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
end;

function ReplaceTargetMacros(const S: string; TargetConfig: ITargetConfig): string;
var
  ps: Integer;
begin
  Result := S;
  ps := Pos('%t', Result);
  if ps > 0 then
  begin
    Delete(Result, ps, 2);
    Insert(Format('%s%d', [LowerCase(TargetTypes[TargetConfig.Target.IsBCB]), TargetConfig.Target.Version]),
      Result, ps);
  end;
end;

/// <summary>
/// CreateProjectGroupMakefile creates the make file for the project group.
/// If AutoDepend is true, this function will add dependency information into
/// the make file for a faster compilation process.
/// </summary>
procedure TCompiler.CreateProjectGroupMakefile(ProjectGroup: TProjectGroup;
  AutoDepend: Boolean);
var
  Lines: TStrings;
  i, depI: Integer;
  Pkg: TPackageTarget;
  Dependencies, S, PasFile, DcuFile, ObjFile, FormFile: string;
  FilenameOnly: string;
  DeleteFiles: Boolean;
  BplFilename, MapFilename: string;
  PasFileSearchDirs: string;
begin
  BplFilename := ProjectGroup.TargetConfig.BplDir + '\' + ProjectGroup.BpgName;

  Lines := TStringList.Create;
  try
    Lines.Add('!ifndef ROOT');
    Lines.Add('ROOT = $(MAKEDIR)\..');
    Lines.Add('!endif');
    Lines.Add('!ifndef DCCOPT');
    if Length(Data.JVCLPackagesDir) < 100 then Lines.Add('DCCOPT = -Q- -M') else Lines.Add('DCCOPT = -Q -M'); { dcc32.exe bug }
    Lines.Add('!endif');
    Lines.Add('');
    Lines.Add('BPR2MAK = "$(ROOT)\bin\bpr2mak" -t..\BCB.bmk');
    Lines.Add('MAKE = "$(ROOT)\bin\make"'{-$(MAKEFLAGS)'});
    Lines.Add('DCC = "$(ROOT)\bin\dcc32.exe" $(DCCOPT)');
    Lines.Add('');

    // for JCL .dcp files
    Lines.Add(Format('.path.dcp = "%s";"%s";"%s";"%s"',
      [ExtractShortPathName(ProjectGroup.TargetConfig.BplDir),
       ExtractShortPathName(ProjectGroup.TargetConfig.DcpDir),
       ExtractShortPathName(ProjectGroup.Target.BplDir),
       ExtractShortPathName(ProjectGroup.Target.DcpDir)]));

    if AutoDepend then
    begin
      S := ExtractShortPathName(ProjectGroup.TargetConfig.JVCLDir);
      PasFileSearchDirs :=
        Format('"%s\common";"%s\run";"%s\design";"%s\qcommon";"%s\qrun";"%s\qdesign"',//;"%s"',
               [S, S, S, S, S, S{, ProjectGroup.TargetConfig.DxgettextDir}]);
      Lines.Add('.path.pas = ' + PasFileSearchDirs);
      Lines.Add(Format('.path.dfm = "%s\run";"%s\design"',
        [S, S]));
      Lines.Add(Format('.path.xfm = "%s\qrun";"%s\qdesign"',
        [S, S]));
      Lines.Add(Format('.path.inc = "%s\common"', [S]));
      Lines.Add(Format('.path.res = "%s\Resources"', [S]));
      Lines.Add(Format('.path.bpl = "%s";"%s"',
        [ProjectGroup.TargetConfig.BplDir, ProjectGroup.TargetConfig.DcpDir]));
      Lines.Add('');

      // add files like jvcl.inc
      Dependencies := '';
      for depI := 0 to High(CommonDependencyFiles) do
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
          ExtractFileName(ReplaceTargetMacros(CommonDependencyFiles[depI], ProjectGroup.TargetConfig));

      Lines.Add('CommonDependencies = ' + Dependencies);
      Lines.Add('');
    end;
    Lines.Add('');

    Lines.Add('default: \');
    for i := 0 to ProjectGroup.Count - 1 do
    begin
      Pkg := ProjectGroup.Packages[i];
      if Pkg.Compile then
        Lines.Add('  ' + Pkg.TargetName + '\');
    end;
    Lines.Add(''); // for last "\"

    Lines.Add('');
    for i := 0 to ProjectGroup.Count - 1 do
    begin
      Pkg := ProjectGroup.Packages[i];
      // add package dependency lists
      Dependencies := '';
      for depI := 0 to Pkg.JvDependencies.Count - 1 do
      begin
        if IsPackageUsed(ProjectGroup, Pkg.JvDependenciesReqPkg[depI]) then
        begin
          if not ProjectGroup.TargetConfig.GenerateMapFiles then
          begin
            // delete the old .map file
            MapFilename := ProjectGroup.TargetConfig.BplDir + PathDelim +
              ChangeFileExt(ExtractFileName(Pkg.TargetName), '.map');
            if FileExists(MapFilename) then
              DeleteFile(MapFilename);
          end;

          Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
             ProjectGroup.FindPackageByXmlName(Pkg.JvDependencies[depI]).TargetName;
        end;
      end;

      // add JCL dependencies
      for depI := 0 to Pkg.JclDependencies.Count - 1 do
      begin
        if IsPackageUsed(ProjectGroup, Pkg.JclDependenciesReqPkg[depI]) then
          Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
             Pkg.JclDependencies[depI] + '.dcp';
      end;

      if AutoDepend then
      begin
        // Add all contained files and test for their condition.
        for depI := 0 to Pkg.Info.ContainCount - 1 do
        begin
          if IsFileUsed(ProjectGroup, Pkg.Info.Contains[depI]) then
          begin
            PasFile := Pkg.Info.Contains[depI].Name;
            FilenameOnly := ExtractFileName(PasFile);
            PasFile := FollowRelativeFilename(Data.JVCLPackagesXmlDir, PasFile);
            if not FileExists(PasFile) then
              PasFile := FindFilename(PasFileSearchDirs, FilenameOnly);

            if FileExists(PasFile) then // add the file only if it exists
            begin
              Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
                FilenameOnly;

              { Check for a .dfm/.xfm file }
              if Pkg.Info.Contains[depI].FormName <> '' then
              begin
                if ProjectGroup.IsVCLX then
                  FormFile := ChangeFileExt(PasFile, '.xfm')
                else
                  FormFile := ChangeFileExt(PasFile, '.dfm');
                if FileExists(FormFile) then
                  Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
                    ExtractFileName(FormFile);
              end;

            end;
          end;
        end;
        Dependencies := Dependencies + '\' + sLineBreak + #9#9'$(CommonDependencies)';
      end;

      Lines.Add(Pkg.TargetName + ': ' + Pkg.SourceName + ' ' + Dependencies);
      Lines.Add(#9'@echo [Compiling: ' + Pkg.TargetName + ']');
      Lines.Add(#9'@cd ' + Pkg.RelSourceDir);
      if ProjectGroup.Target.IsBCB then
      begin
        if not ProjectGroup.TargetConfig.Build then
        begin
          // dcc32.exe does not recreate the .obj files when they already exist.
          // So we must delete them before compilation. This is not needed when
          // building the JVCL for BCB because all .obj files will be deleted by
          // the Installer before entering the compilation process.
          DeleteFiles := False;
          for depI := 0 to Pkg.Info.ContainCount - 1 do
          begin
            if IsFileUsed(ProjectGroup, Pkg.Info.Contains[depI]) then
            begin
              FilenameOnly := ExtractFileName(Pkg.Info.Contains[depI].Name);
              PasFile := FollowRelativeFilename(Data.JVCLPackagesXmlDir, Pkg.Info.Contains[depI].Name);
              if CompareText(ExtractFileExt(FilenameOnly), '.pas') = 0 then
              begin
                ObjFile := ProjectGroup.TargetConfig.UnitOutDir + '\obj\' + ChangeFileExt(FilenameOnly, '.obj');
                if not FileExists(PasFile) then
                  PasFile := FindFilename(PasFileSearchDirs, FilenameOnly);

                {
                if FileExists(ObjFile) and not FileExists(PasFile) then
                  Continue; // a little optimization: foreign units should not force the package to be built.
                }

                if not FileExists(ObjFile) or // dcc32.exe will not create the missing .obj file if the other files exist
                   not FileExists(PasFile) or // unknown directory for the .pas file
                   (CompareFileAge(ObjFile, [], PasFile, []) < 0) or
                   (FileExists(BplFilename) and (
                    (CompareFileAge(ObjFile, [], BplFilename, []) < 0) or
                    (CompareFileAge(PasFile, [], BplFilename, []) < 0))
                   ) then
                begin
                  DeleteFiles := True;
                  Break;
                end;
              end;
            end;
          end;

          if DeleteFiles then
          begin
            for depI := 0 to Pkg.Info.ContainCount - 1 do
            begin
              if IsFileUsed(ProjectGroup, Pkg.Info.Contains[depI]) then
              begin
                FilenameOnly := ExtractFileName(Pkg.Info.Contains[depI].Name);
                if CompareText(ExtractFileExt(FilenameOnly), '.pas') = 0 then
                begin
                  ObjFile := ProjectGroup.TargetConfig.UnitOutDir + '\obj\' + ChangeFileExt(FilenameOnly, '.obj');
                  if FileExists(ObjFile) then
                  begin
                    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
                      Lines.Add(#9'-@del "' + ObjFile + '" >NUL')
                    else
                      Lines.Add(#9'-@del /f /q "' + ObjFile + '" 2>NUL');
                  end;
                  DcuFile := ProjectGroup.TargetConfig.UnitOutDir + '\obj\' + ChangeFileExt(FilenameOnly, '.dcu');
                  if FileExists(DcuFile) then
                  begin
                    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
                      Lines.Add(#9'-@del "' + DcuFile + '" >NUL')
                    else
                      Lines.Add(#9'-@del /f /q "' + DcuFile + '" 2>NUL');
                  end;
                end;
              end;
            end;
          end;
        end;
        Lines.Add(#9'$(BPR2MAK) $&.bpk');
        Lines.Add(#9'@echo.');                 // prevent "......Borland De"
        Lines.Add(#9'$(MAKE) -f $&.mak');
      end
      else
        Lines.Add(#9'$(DCC) $&.dpk');
      Lines.Add(#9'@cd ' + GetReturnPath(Pkg.RelSourceDir));
      Lines.Add('');
    end;

    FileSetReadOnly(ChangeFileExt(ProjectGroup.Filename, '.mak'), False);
    Lines.SaveToFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
  finally
    Lines.Free;
  end;
end;

function TCompiler.IsFileUsed(ProjectGroup: TProjectGroup;
  ContainedFile: TContainedFile): Boolean;
begin
  Result := ContainedFile.IsUsedByTarget(ProjectGroup.TargetConfig.TargetSymbol) and
            IsCondition(ContainedFile.Condition, ProjectGroup.TargetConfig);
end;

function TCompiler.IsPackageUsed(ProjectGroup: TProjectGroup;
  RequiredPackage: TRequiredPackage): Boolean;
begin
  Result := RequiredPackage.IsRequiredByTarget(ProjectGroup.TargetConfig.TargetSymbol) and
            IsCondition(RequiredPackage.Condition, ProjectGroup.TargetConfig);
end;

type
  { TListConditionParser searches for the idents in the List. If an ident is in
    the list the ident is returned as True. }
  TListConditionParser = class(TConditionParser)
  private
    FTargetConfig: ITargetConfig;
  protected
    procedure MissingRightParenthesis; override;
    function GetIdentValue(const Ident: String): Boolean; override;
  public
    constructor Create(ATargetConfig: ITargetConfig);
  end;


function TCompiler.IsCondition(const Condition: string; TargetConfig: ITargetConfig): Boolean;
var
  Parser: TListConditionParser;
begin
  Result := True;
  if Condition <> '' then
  begin
    Parser := TListConditionParser.Create(TargetConfig);
    try
      Result := Parser.Parse(Condition);
    finally
      Parser.Free;
    end;
  end;
end;

{ TListConditionParser }

constructor TListConditionParser.Create(ATargetConfig: ITargetConfig);
begin
  inherited Create;
  FTargetConfig := ATargetConfig;
end;

function TListConditionParser.GetIdentValue(const Ident: String): Boolean;
begin
  Result := FTargetConfig.JVCLConfig.Enabled[Ident];
end;

procedure TListConditionParser.MissingRightParenthesis;
begin
  raise Exception.Create('Missing ")" in conditional expression');
end;


initialization
  StartupEnvVarPath := GetEnvironmentVariable('PATH');

end.

