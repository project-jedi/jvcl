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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit Compile;

interface

uses
  Windows, SysUtils, Classes, CapExec, JVCLData, DelphiData,
  GenerateUtils, PackageUtils, Utils, Intf;

type
  TProgressKind = (
    tkTarget,           // progress of all targets
    tkProject,          // |- progress of the parts of one target compilation
    tkResource,         //    |- progress of the resource compilation
    tkPackage           //    |- progress of the package compilation
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

  TJVCLCompiler = class(TObject)
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

  protected
    procedure CaptureLine(const Line: string; var Aborted: Boolean); virtual;
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
  public
    constructor Create(AData: TJVCLData);
    destructor Destroy; override;

    function GeneratePackages(const Group, Targets, PackagesPath: string): Boolean; overload;
    function PrepareJCL(Force: Boolean): Boolean;
    function GenerateAllPackages: Boolean; overload;
    function Compile(Force: Boolean = False): Boolean;
    function CompileTarget(TargetConfig: TTargetConfig): Boolean;

    procedure Abort; // abort compile process

    property Data: TJVCLData read FData;
    property Output: TStrings read FOutput;

    property OnCaptureLine: TCaptureLine read FOnCaptureLine write FOnCaptureLine;
    property OnTargetProgress: TTargetProgressEvent read FOnTargetProgress write FOnTargetProgress;
    property OnPackageProgress: TPackageProgressEvent read FOnPackageProgress write FOnPackageProgress;
    property OnResourceProgress: TResourceProgressEvent read FOnResourceProgress write FOnResourceProgress;
    property OnProjectProgress: TProjectProgressEvent read FOnProjectProgress write FOnProjectProgress;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

const
  ProjectMax = 5;
  
var
  Compiler: TJVCLCompiler = nil;

implementation

uses
  JvConsts;

resourcestring
  RsGeneratingTemplates = 'Generating templates...';
  RsGeneratingPackages = 'Generating packages...';
  RsGeneratingResources = 'Generating resources...';
  RsCompilingPackages = 'Compiling packages...';
  RsFinished = 'Finished.';
  RsCompilingJCL = 'Compiling JCL dcp files...';

const
  CommonDependencyFiles: array[0..3] of string = (
    'jvcl.inc', 'jedi.inc', 'linuxonly.inc', 'windowsonly.inc'
  );

resourcestring
  RsGeneratePackages = '[Generating: Packages]';

{ TJVCLCompiler }

constructor TJVCLCompiler.Create(AData: TJVCLData);
begin
  inherited Create;
  FData := AData;
  FOutput := TStringList.Create;
end;

destructor TJVCLCompiler.Destroy;
begin
  FOutput.Free;
  inherited Destroy;
end;

procedure TJVCLCompiler.DoTargetProgress(Current: TTargetConfig; Position,
  Max: Integer);
begin
  if Assigned(FOnTargetProgress) then
    FOnTargetProgress(Self, Current, Position, Max);
  DoProgress(Current.Target.DisplayName, Position, Max, tkTarget);
end;

procedure TJVCLCompiler.DoProjectProgress(const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnProjectProgress) then
    FOnProjectProgress(Self, Text, Position, Max);
  DoProgress(Text, Position, Max, tkProject);
end;

procedure TJVCLCompiler.DoResourceProgress(const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnResourceProgress) then
    FOnResourceProgress(Self, Text, Position, Max);
  DoProgress(Text, Position, Max, tkResource);
end;

procedure TJVCLCompiler.DoPackageProgress(Current: TPackageTarget;
  const Text: string; Position, Max: Integer);
begin
  if Assigned(FOnPackageProgress) then
    FOnPackageProgress(Self, Current, Text, Position, Max);
  DoProgress(Text, Position, Max, tkPackage);
end;

procedure TJVCLCompiler.DoProgress(const Text: string; Position, Max: Integer;
  Kind: TProgressKind);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Text, Position, Max, Kind);
end;

procedure TJVCLCompiler.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  FOutput.Add(Line);
  if Assigned(FOnCaptureLine) then
    FOnCaptureLine(Line, FAborted);
  Aborted := FAborted;
end;

procedure TJVCLCompiler.CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
begin
  if StartsWith(Trim(Line), 'echo [Compiling: ', True) then
    Inc(FCount);
  Aborted := FAborted;
end;

procedure TJVCLCompiler.CaptureLinePackageCompilation(const Line: string; var Aborted: Boolean);
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
          DoPackageProgress(FCurrentProjectGroup.Packages[i], S, FPkgIndex - 1, FPkgCount);
          Exit;
        end;
      DoPackageProgress(nil, S, FPkgIndex, FPkgCount);
    end;
  end;
end;

procedure TJVCLCompiler.CaptureLineResourceCompilation(const Line: string;
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

procedure TJVCLCompiler.Abort;
begin
  FAborted := True;
end;

procedure WriteMsg(const Text: string); // used by TJVCLCompiler.GeneratePackages
begin
  Compiler.CaptureLine(Text, Compiler.FAborted);
end;

/// <summary>
/// GeneratePackages generates the packages in
/// PackagesPath for the Group (JVCL, JCL) for the Targets (comma separated
/// pg.exe target list).
/// </summary>
function TJVCLCompiler.GeneratePackages(const Group, Targets, PackagesPath: string): Boolean;
var
  ErrMsg: string;
  List, TargetList: TStrings;
begin
  Result := False;

  FAborted := False;
  CaptureLine(RsGeneratePackages, FAborted);
  if FAborted then
    Exit;

  if not LoadConfig(Data.JVCLDir + '\' + PackageGeneratorFile, Group, ErrMsg) then
  begin
    CaptureLine(ErrMsg, FAborted);
    Exit;
  end;

  List := TStringList.Create;
  TargetList := TStringList.Create;
  try
    TargetList.CommaText := Targets;
    ExpandTargetsNoPerso(TargetList);
    EnumeratePackages(PackagesPath, List);
    if not Generate(List, TargetList, WriteMsg, Data.JVCLDir + '\' + PackageGeneratorFile,
                    Group, ErrMsg, False, PackagesPath, '', '', Data.JVCLConfig.Filename) then
    begin
      CaptureLine(ErrMsg, FAborted);
      Exit;
    end;
  finally
    List.Free;
    TargetList.Free;
  end;
  Result := True;
end;

/// <summary>
/// PrepareJCL compiles the .dcp files for C++Builder targets where the JVCL
/// should be installed.
/// </summary>
function TJVCLCompiler.PrepareJCL(Force: Boolean): Boolean;
var
  Args: string;
  i, LineNum: Integer;
  ErrorFileName, TargetPkgDir, TargetXmlDir: string;
  ErrorLines: TStrings;
begin
  Result := False;
  for i := 0 to Data.Targets.Count - 1 do
  begin
    if Data.Targets[i].IsBCB and Data.TargetConfig[i].InstallJVCL then
    begin
      TargetPkgDir := Format('%s\packages\c%d',
                             [Data.TargetConfig[i].JCLDir, Data.Targets[i].Version]);
      TargetXmlDir := Format('%s\packages\xml', [Data.TargetConfig[i].JCLDir]);
      ErrorFileName := TargetPkgDir + '\error.log';
      DeleteFile(ErrorFileName);

      // Are the .dcp files newer than the .bpk files
      if (not Force) and (not Data.TargetConfig[i].Build) and
          FileExists(Data.TargetConfig[i].BplDir + '\CJcl.dcp') and
          FileExists(Data.TargetConfig[i].BplDir + '\CJclVcl.dcp') and
         (FileAge(Data.TargetConfig[i].BplDir + '\CJcl.dcp') >= FileAge(TargetXmlDir + '\Jcl-R.xml')) then
        Continue;

      SetEnvironmentVariable('JCLROOT', PChar(Data.TargetConfig[i].JCLDir));
      Args := Format('-f MakeJCLDcp4BCB.mak -DVERSION=%d', [Data.Targets[i].Version]);

      DoTargetProgress(Data.TargetConfig[i], 0, 100);
      DoPackageProgress(nil, RsCompilingJCL, 0, 3);

     // copy template for PackageGenerator
      if CaptureExecute('"' + Data.Targets[i].Make + '"', Args + ' -s Templates',
                        Data.JVCLPackagesDir + '\bin', CaptureLine) <> 0 then
        Exit;
      DoPackageProgress(nil, RsCompilingJCL, 1, 3);

     // generate packages
      Result := GeneratePackages('JCL', 'c' + IntToStr(Data.Targets[i].Version),
        Data.TargetConfig[i].JCLDir + '\packages');
      DoPackageProgress(nil, RsCompilingJCL, 2, 3);

     // compile dcp files
      if CaptureExecute('"' + Data.Targets[i].Make + '"', Args + ' -s Compile',
                        Data.JVCLPackagesDir + '\bin', CaptureLine) <> 0 then
      begin
        if FileExists(ErrorFileName) then
        begin
          ErrorLines := TStringList.Create;
          try
            ErrorLines.LoadFromFile(ErrorFileName);
            for LineNum := 0 to ErrorLines.Count - 1 do
              CaptureLine(ErrorLines[LineNum], FAborted);
          finally
            ErrorLines.Free;
          end;
        end;
        Exit;
      end;
      DeleteFile(ErrorFileName);
      DoTargetProgress(Data.TargetConfig[i], 0, 100);
      DoPackageProgress(nil, '', 0, 100);
    end;
  end;
  Result := True;
end;

/// <summary>
/// GenerateAllPackages generates all JVCL packages
/// </summary>
function TJVCLCompiler.GenerateAllPackages: Boolean;
begin
  Result := GeneratePackages('JVCL', 'all', Data.JVCLPackagesDir);
end;

function TJVCLCompiler.Compile(Force: Boolean = False): Boolean;
var
  i: Integer;
  Count: Integer;
  TargetConfigs: array of TTargetConfig;
begin
  Result := PrepareJCL(Force);
  if not Result then
    Exit;

 // read target configs that should be compiled
  Count := 0;
  SetLength(TargetConfigs, Data.Targets.Count);
  for i := 0 to High(TargetConfigs) do
  begin
    if Data.TargetConfig[i].InstallJVCL then
    begin
      TargetConfigs[Count] := Data.TargetConfig[i];
      Inc(Count);
    end;
  end;
  SetLength(TargetConfigs, Count);

 // compile all targets 
  for i := 0 to Count - 1 do
  begin
    DoTargetProgress(TargetConfigs[i], i, Count);
    Result := CompileTarget(TargetConfigs[i]);
    DoTargetProgress(TargetConfigs[i], i + 1, Count);
    if not Result then
      Break;
  end;
end;

/// <summary>
/// CompileTarget starts CompileProjectGroup for all sub targets of the
/// given target IDE.
/// </summary>
function TJVCLCompiler.CompileTarget(TargetConfig: TTargetConfig): Boolean;
begin
  Result := True;
  FOutput.Clear;

 // VCL
  if Result and (pkVCL in TargetConfig.InstallMode) then
  begin
   // debug units
    if (not TargetConfig.Target.IsBCB) and TargetConfig.DebugUnits then
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], True);

    if Result then
     // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], False);
  end;

 // Clx
  if Result and (pkClx in TargetConfig.InstallMode) then
  begin
   // debug units
    if (not TargetConfig.Target.IsBCB) and TargetConfig.DebugUnits then
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], True);

    if Result then
     // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], False);
  end;

  if Result then
    CaptureExecute('"' + TargetConfig.Target.Make + '"', '-s Clean',
                    Data.JVCLPackagesDir + '\bin', CaptureLine);
end;

/// <summary>
/// GenerateResources starts the make file for the resource file compilation.
/// </summary>
function TJVCLCompiler.GenerateResources(TargetConfig: ITargetConfig): Boolean;
begin
  Result := False;

 // get number of resources to compile
  FCount := 0;
  if CaptureExecute('"' + TargetConfig.Target.Make + '"', '-n',
                    TargetConfig.JVCLDir + '\images', CaptureLineGetCompileCount) <> 0 then
    Exit;
 // update FResCount with the number of resources that MAKE will compile
  FResCount := FCount;

  FResIndex := 0;
  if FCount > 0 then
  begin
    DoResourceProgress('', 0, FResCount);
   // generate .res and .dcr files
    if CaptureExecute('"' + TargetConfig.Target.Make + '"', '',
                      TargetConfig.JVCLDir + '\images', CaptureLineResourceCompilation) <> 0 then
      Exit;
    DoResourceProgress('', FResCount, FResCount);
  end;
  Result := True;
end;

/// <summary>
/// CompileProjectGroup starts the make file for the templates, starts the
/// packages generator, calls the GenerateResource method and compiles all
/// selected packages of the project group.
/// </summary>
function TJVCLCompiler.CompileProjectGroup(ProjectGroup: TProjectGroup;
  DebugUnits: Boolean): Boolean;
var
  AProjectIndex, i: Integer;
  Args: string;
  Edition, PkgDir, JVCLPackagesDir: string;
  AutoDepend: Boolean;

  function GetProjectIndex: Integer;
  begin
    Result := AProjectIndex;
    Inc(AProjectIndex);
  end;

begin
  Result := False;
  FCurrentProjectGroup := ProjectGroup;
  try
    AutoDepend := ProjectGroup.TargetConfig.AutoDependencies;
    // obtain information for progress bar
    FPkgCount := 0;
    for i := 0 to ProjectGroup.Count - 1 do
      if ProjectGroup.Packages[i].Compile then
        Inc(FPkgCount);

    Edition := ProjectGroup.TargetConfig.TargetSymbol;
    JVCLPackagesDir := ProjectGroup.TargetConfig.JVCLPackagesDir;
    Args := '-f Makefile.mak';

    PkgDir := Edition;
    if PkgDir[3] in ['p', 'P', 's', 'S'] then
    begin
      if PkgDir[2] = '5' then
        PkgDir := Copy(PkgDir, 1, 2) + 'std'
      else
        PkgDir := Copy(PkgDir, 1, 2) + 'per';
    end;

   // setup environment variables
    if ProjectGroup.TargetConfig.Build then
      SetEnvironmentVariable('DCCOPT', '-Q -M -B')
    else
      SetEnvironmentVariable('DCCOPT', '-Q -M');

    SetEnvironmentVariable('TARGETS', nil); // we create our own makefile so do not allow a user defined TARGETS envvar
    SetEnvironmentVariable('MASTEREDITION', nil);

    SetEnvironmentVariable('ROOT', PChar(ProjectGroup.TargetConfig.Target.RootDir));
    SetEnvironmentVariable('JCLROOT', PChar(ProjectGroup.TargetConfig.JCLDir));
    SetEnvironmentVariable('JVCLROOT', PChar(ProjectGroup.TargetConfig.JVCLDir));
    SetEnvironmentVariable('VERSION', PChar(IntToStr(ProjectGroup.TargetConfig.Target.Version)));
    if DebugUnits then
      SetEnvironmentVariable('UNITOUTDIR', PChar(ProjectGroup.TargetConfig.UnitOutDir + '\debug'))
    else
      SetEnvironmentVariable('UNITOUTDIR', PChar(ProjectGroup.TargetConfig.UnitOutDir));
    SetEnvironmentVariable('BPLDIR', PChar(ProjectGroup.TargetConfig.BplDir));
    SetEnvironmentVariable('DCPDIR', PChar(ProjectGroup.TargetConfig.DcpDir));
    SetEnvironmentVariable('LIBDIR', PChar(ProjectGroup.TargetConfig.DcpDir));
    SetEnvironmentVariable('HPPDIR', PChar(ProjectGroup.TargetConfig.HPPDir));

{**}DoProjectProgress(RsGeneratingPackages, GetProjectIndex, ProjectMax);
    if ProjectGroup.Target.IsPersonal then
    begin
     // generate template.cfg for the "master" PkgDir
      SetEnvironmentVariable('EDITION', PChar(Copy(Edition, 1, 2)));
      SetEnvironmentVariable('PKGDIR', PChar(Copy(PkgDir, 1, 2)));
      SetEnvironmentVariable('PKGDIR_MASTEREDITION', PChar(Copy(PkgDir, 1, 2)));
      if CaptureExecute('"' + ProjectGroup.Target.Make + '"', Args + ' Templates',
                        JVCLPackagesDir + '\bin', CaptureLine) <> 0 then
        Exit;
    end;

   // generate temnplate.cfg file for PkgDir
    SetEnvironmentVariable('EDITION', PChar(Edition));
    SetEnvironmentVariable('PKGDIR', PChar(PkgDir));
    SetEnvironmentVariable('PKGDIR_MASTEREDITION', PChar(PkgDir));
    if CaptureExecute('"' + ProjectGroup.Target.Make + '"', Args + ' Templates',
                      JVCLPackagesDir + '\bin', CaptureLine) <> 0 then
      Exit;

{**}DoProjectProgress(RsGeneratingPackages, GetProjectIndex, ProjectMax);
    if ProjectGroup.Target.IsPersonal then
    begin
     // generate the packages and .cfg files for the "master" PkgDir
      if not GeneratePackages('JVCL', Copy(Edition, 1, 2),
                              ProjectGroup.TargetConfig.JVCLPackagesDir) then
        Exit;
    end;

   // generate the packages and .cfg files for PkgDir
    if not GeneratePackages('JVCL', Edition, ProjectGroup.TargetConfig.JVCLPackagesDir) then
      Exit;

{**}DoProjectProgress(RsGeneratingResources, GetProjectIndex, ProjectMax);
    if not GenerateResources(ProjectGroup.TargetConfig) then
      Exit;

{**}DoProjectProgress(RsCompilingPackages, GetProjectIndex, ProjectMax);
    FPkgIndex := 0;
    if FPkgCount > 0 then
    begin
      // ==== Resources have changed ====
      // As long as no dependency information about resources is in the .xml
      // files we let the Delphi compiler decide if he wants to compile the
      // package.
      if (FResCount > 0) then
        AutoDepend := False;
      // =================================

      CreateProjectGroupMakefile(ProjectGroup, AutoDepend);
      if AutoDepend then
      begin
        SetEnvironmentVariable('MAKEOPTIONS', '-n');
        try
          // get number of packages that will be compiled
          FCount := 0;
          if CaptureExecute('"' + ProjectGroup.Target.Make + '"', Args + ' CompilePackages',
                            ProjectGroup.TargetConfig.JVCLPackagesDir + '\bin',
                            CaptureLineGetCompileCount) <> 0 then
            Exit;
         // update FPkgCount with the number of packages that MAKE will compile
          FPkgCount := FCount;
        finally
          SetEnvironmentVariable('MAKEOPTIONS', nil);
        end;
      end;

      if FPkgCount > 0 then
      begin
        DoPackageProgress(nil, '', 0, FPkgCount);
        // compile packages
        if CaptureExecute('"' + ProjectGroup.Target.Make + '"', Args + ' CompilePackages',
                          ProjectGroup.TargetConfig.JVCLPackagesDir + '\bin',
                          CaptureLinePackageCompilation) <> 0 then
          Exit;
        DoPackageProgress(nil, '', FPkgCount, FPkgCount);
      end;
    end;
  finally
{**}DoProjectProgress(RsFinished, ProjectMax, ProjectMax);
    FCurrentProjectGroup := nil;
  end;
  Result := True;
  DeleteFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
  DeleteFile(ProjectGroup.TargetConfig.JVCLPackagesDir + '\tmp.bat');
end;

/// <summary>
/// CreateProjectGroupMakefile creates the make file for the project group.
/// If AutoDepend is true, this function will add dependency information into
/// the make file for a faster compilation process.
/// </summary>
procedure TJVCLCompiler.CreateProjectGroupMakefile(ProjectGroup: TProjectGroup;
  AutoDepend: Boolean);
var
  Lines: TStrings;
  i, depI: Integer;
  Pkg: TPackageTarget;
  Dependencies, S: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('!ifndef ROOT');
    Lines.Add('ROOT = $(MAKEDIR)\..');
    Lines.Add('!endif');
    Lines.Add('!ifndef DCCOPT');
    Lines.Add('DCCOPT = -Q -M');
    Lines.Add('!endif');
    Lines.Add('');
    Lines.Add('BPR2MAK = "$(ROOT)\bin\bpr2mak" -t..\BCB.bmk');
    Lines.Add('MAKE = "$(ROOT)\bin\make" -$(MAKEFLAGS)');
    Lines.Add('DCC = "$(ROOT)\bin\dcc32.exe" $(DCCOPT)');
    Lines.Add('');
    if {(not ProjectGroup.Target.IsBCB) and} AutoDepend then
    begin
      S := ProjectGroup.TargetConfig.JVCLDir;
      Lines.Add(Format('.path.pas = %s\common;%s\run;%s\design;%s\qcommon;%s\qrun;%s\qdesign',
        [S, S, S, S, S, S]));
      Lines.Add(Format('.path.dfm = %s\run;%s\design;%s\qrun;%s\qdesign',
        [S, S, S, S]));
      Lines.Add(Format('.path.inc = %s\common', [S]));
      Lines.Add(Format('.path.res = %s\Resources', [S]));
      Lines.Add(Format('.path.bpl = %s;%s',
        [ProjectGroup.TargetConfig.BplDir, ProjectGroup.TargetConfig.DcpDir]));
      Lines.Add('');
     // add files like jvcl.inc
      Dependencies := '';
      for depI := 0 to High(CommonDependencyFiles) do
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
          ExtractFileName(CommonDependencyFiles[depI]);

      Lines.Add('CommonDependencies = ' + Dependencies);
      Lines.Add('');
      Lines.Add('');
    end;

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
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
           ProjectGroup.FindPackagebyXmlName(Pkg.JvDependencies[depI]).TargetName;

      if {(not ProjectGroup.Target.IsBCB) and} AutoDepend then
      begin
       // Add all contained files even if the condition and target is not
       // correct. This does not make any difference because the Compiler has
       // the last decission.
        for depI := 0 to Pkg.Info.ContainCount - 1 do
          Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
            ExtractFileName(Pkg.Info.Contains[depI].Name);
        Dependencies := Dependencies + '\' + sLineBreak + #9#9'$(CommonDependencies)';
      end;

      Lines.Add(Pkg.TargetName + ': ' + Pkg.SourceName + ' ' + Dependencies);
      Lines.Add(#9'@echo [Compiling: ' + Pkg.TargetName + ']');
      Lines.Add(#9'@cd ' + Pkg.RelSourceDir);
      if ProjectGroup.Target.IsBCB then
      begin
        Lines.Add(#9'@$(BPR2MAK) $&.bpk');
        Lines.Add(#9'@echo.');
        Lines.Add(#9'@$(MAKE) -f $&.mak');
      end
      else
        Lines.Add(#9'@$(DCC) $&.dpk');
      Lines.Add(#9'@cd ' + GetReturnPath(Pkg.RelSourceDir));
      Lines.Add('');
    end;
    Lines.SaveToFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
  finally
    Lines.Free;
  end;
end;

end.
