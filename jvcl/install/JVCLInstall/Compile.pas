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

{$I jvcl.inc}
{$I windowsonly.inc}

unit Compile;

interface

uses
  Windows, SysUtils, Classes, CapExec, JVCLData, DelphiData,
  GenerateUtils, PackageUtils, Intf;

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

    FAbortReason: string;
    FQuiet: string;
  protected
    function Make(TargetConfig: ITargetConfig; const Args: string;
      CaptureLine: TCaptureLine; StartDir: string = ''): Integer;

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

    function IsCondition(const Condition: string): Boolean;
  public
    constructor Create(AData: TJVCLData);
    destructor Destroy; override;

    function GeneratePackages(const Group, Targets, PackagesPath: string): Boolean; overload;
    function PrepareJCL(Force: Boolean): Boolean;
    function GenerateAllPackages: Boolean; overload;
    function Compile(Force: Boolean = False): Boolean;
    function CompileTarget(TargetConfig: TTargetConfig): Boolean;

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
  end;

const
  ProjectMax = 5;

var
  Compiler: TJVCLCompiler = nil;

resourcestring
  RsPackagesAreUpToDate = 'Packages are up to date';

implementation

uses
  CmdLineUtils, JvConsts, Utils;

resourcestring
  RsGeneratingTemplates = 'Generating templates...';
  RsGeneratingPackages = 'Generating packages...';
  RsGeneratingResources = 'Generating resources...';
  RsCompilingPackages = 'Compiling packages...';
  RsFinished = 'Finished.';
  RsCompilingJCL = 'Compiling JCL dcp files...';

  RsAbortedByUser = 'Aborted by User';
  RsErrorLoadingPackageGeneratorConfigFile = 'Error loading devtools\bin\pgEdit.xml';
  RsErrorGeneratingPackages = 'Error while generating packages for %s';
  RsErrorGeneratingTemplates = 'Error while generating templates.';
  RsErrorCompilingJclDcpFiles = 'Error while compiling .dcp files for JCL.';
  RsErrorCompilingResources = 'Error while compiling resources.';
  RsErrorGeneratingTemplatesForDir = 'Error generating templates for the %s directory.';
  RsErrorCompilingPackages = 'An error occured while compiling the packages.'; // this must not be the doubt of the installer

  RsCommandNotFound = 'Command could not be executed.'#10#10#10'Cmdline: %s'#10#0'Start directory: %s';

const
  RsGeneratePackages = '[Generating: Packages]'; // do not localize

const
  CommonDependencyFiles: array[0..3] of string = (
    'jvcl.inc', 'jedi.inc', 'linuxonly.inc', 'windowsonly.inc'
  );

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

procedure TJVCLCompiler.CaptureLineClean(const Line: string; var Aborted: Boolean);
begin
  if StartsWith('[', Line) then
    CaptureLine(Line, Aborted);
end;

procedure TJVCLCompiler.CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
begin
  if StartsWith(Trim(Line), 'echo [Compiling: ', True) then
    Inc(FCount)
  else if (Line <> '') and (Line[1] <> #9) then
    CaptureLine(Line, FAborted);
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
          S := S + '  (' + FCurrentProjectGroup.Packages[i].Info.Description + ')';
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
/// Make calls the make.exe of the given TargetConfig. If the StartDir is empty
/// the JVCLPackageDir\bin directory is used. If the command could not be
/// executed a message dialog is shown with the complete command line.
/// </summary>
function TJVCLCompiler.Make(TargetConfig: ITargetConfig; const Args: string;
  CaptureLine: TCaptureLine; StartDir: string): Integer;
begin
  if StartDir = '' then
    StartDir := Data.JVCLPackagesDir + '\bin';

  if Data.Verbose then
  begin
    // output command line
    if Assigned(CaptureLine) then
      CaptureLine(#1 + '"' + TargetConfig.Target.Make + '" -l+ ' + Args, FAborted);
  end;

  Result := CaptureExecute('"' + TargetConfig.Target.Make + '"', '-l+ ' + Args,
                           StartDir, CaptureLine);
  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.Make + '"' + Args, StartDir])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
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
  begin
    AbortReason := RsAbortedByUser;
    Exit;
  end;

  try
    if not LoadConfig(Data.JVCLDir + '\' + PackageGeneratorFile, Group, ErrMsg) then
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
    if not Generate(List, TargetList, WriteMsg, Data.JVCLDir + '\' + PackageGeneratorFile,
                    Group, ErrMsg, PackagesPath, '', '', Data.JVCLConfig.Filename) then
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
/// PrepareJCL compiles the .dcp files for C++Builder targets where the JVCL
/// should be installed.
/// </summary>
function TJVCLCompiler.PrepareJCL(Force: Boolean): Boolean;
var
  Args: string;
  i, LineNum: Integer;
  ErrorFileName, TargetPkgDir, TargetXmlDir, S: string;
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
      with Data.TargetConfig[i] do
      begin
        if Target.Version = 5 then S := '50' else S := '';
        if (not Force) and (not Build) and CompiledJCL and
           FileExists(Format('%s\CJcl%s.dcp', [BplDir, S])) and
           FileExists(Format('%s\CJclVcl%s.dcp', [BplDir, S])) then
        begin
           if (CompareFileAge('%s\CJcl%s.dcp', [BplDir, S],
                          '%s\CJcl.lib', [DcpDir]) > 0) and
              (CompareFileAge('%s\CJclVcl%s.dcp', [BplDir, S],
                          '%s\CJclVcl.lib', [DcpDir]) > 0) then
              Continue;
        end;

       // let the compiler rebuild the dcp files
        DeleteFile(Format('%s\CJcl%s.dcp', [BplDir, S]));
        DeleteFile(Format('%s\CJclVcl%s.dcp', [BplDir, S]));
        DeleteFile(Format('%s\CJclVClx%s.dcp', [BplDir, S]));

       // sometimes the files are in the wrong directory
        DeleteFile(Format('%s\CJcl%s.dcp', [DcpDir, S]));
        DeleteFile(Format('%s\CJclVcl%s.dcp', [DcpDir, S]));
        DeleteFile(Format('%s\CJclVClx%s.dcp', [DcpDir, S]));
      end;

      SetEnvironmentVariable('JCLROOT', PChar(Data.TargetConfig[i].JCLDir));
      SetEnvironmentVariable('DCPDIR', PChar(Data.TargetConfig[i].BplDir)); // not DcpDir!!!
      Args := Format('-f MakeJCLDcp4BCB.mak -DVERSION=%d', [Data.Targets[i].Version]);

      DoTargetProgress(Data.TargetConfig[i], 0, 100);
      DoPackageProgress(nil, RsCompilingJCL, 0, 3);

      try
       // copy template for PackageGenerator
        if Make(Data.TargetConfig[i], Args + FQuiet + ' Templates', CaptureLine) <> 0 then
        begin
          AbortReason := RsErrorGeneratingTemplates;
          Exit;
        end;
        DoPackageProgress(nil, RsCompilingJCL, 1, 3);

       // generate packages
        Result := GeneratePackages('JCL', 'c' + IntToStr(Data.Targets[i].Version),
          Data.TargetConfig[i].JCLDir + '\packages');
        DoPackageProgress(nil, RsCompilingJCL, 2, 3);

       // compile dcp files
        if Make(Data.TargetConfig[i], Args + FQuiet + ' Compile', CaptureLine) <> 0 then
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
          AbortReason := RsErrorCompilingJclDcpFiles;
          Exit;
        end;
      finally
       // clean
        Make(Data.TargetConfig[i], Args + FQuiet + ' CleanJcl', CaptureLine);

        DeleteFile(ErrorFileName);
      end;

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
  Result := True;
  if Data.Verbose then
    FQuiet := ''
  else
    FQuiet := ' -s';
    
  AbortReason := '';
  if Data.CompileJclDcp then
    Result := PrepareJCL(Force);
  if not Result then
    Exit; // AbortReason was set in PrepareJCL

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
var
  ObjFiles: TStrings;
  i: Integer;
  Aborted: Boolean;
begin
  Result := True;
  Aborted := False;
  FOutput.Clear;

  if TargetConfig.Target.IsBCB and TargetConfig.Build then
  begin
    // Delete all .obj files because dcc32.exe -JPHNE does not create new .obj
    // files if they already exist. And as a result interface changes in a unit
    // let the bcc32.exe compiler fail.
    ObjFiles := TStringList.Create;
    try
      FindFiles(TargetConfig.UnitOutDir, '*.*', True, ObjFiles, ['.obj']);
      for i := 0 to ObjFiles.Count - 1 do
        DeleteFile(ObjFiles[i]);
    finally
      ObjFiles.Free;
    end;
  end;

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

    if Result or not CmdOptions.KeepFiles then
      Make(TargetConfig, FQuiet + ' Clean', CaptureLineClean);
    if Result then
      CaptureLine('[Finished JVCL for VCL installation]', Aborted);
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

    if Result or not CmdOptions.KeepFiles then
      Make(TargetConfig, FQuiet + ' Clean', CaptureLineClean);
    if Result then
      CaptureLine('[Finished JVCL for CLX installation]', Aborted);
  end;
end;

/// <summary>
/// GenerateResources starts the make file for the resource file compilation.
/// </summary>
function TJVCLCompiler.GenerateResources(TargetConfig: ITargetConfig): Boolean;
begin
  Result := False;

 // get number of resources to compile
  FCount := 0;
  if Make(TargetConfig, '-n', CaptureLineGetCompileCount, TargetConfig.JVCLDir + '\images') <> 0 then
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
    if Make(TargetConfig, '', CaptureLineResourceCompilation, TargetConfig.JVCLDir + '\images') <> 0 then
    begin
      AbortReason := RsErrorCompilingResources;
      Exit;
    end;
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
  TargetConfig: ITargetConfig;

  function GetProjectIndex: Integer;
  begin
    Result := AProjectIndex;
    Inc(AProjectIndex);
  end;

begin
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
    JVCLPackagesDir := TargetConfig.JVCLPackagesDir;
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
    if TargetConfig.Build then
    begin
      SetEnvironmentVariable('DCCOPT', '-Q -M -B');
     // especially for BCB generated make file
      SetEnvironmentVariable('DCC', PChar('"' + TargetConfig.Target.RootDir + '\bin\dcc32.exe" -Q -M -B'));
    end
    else
    begin
      SetEnvironmentVariable('DCCOPT', '-Q -M');
     // especially for BCB generated make file
      SetEnvironmentVariable('DCC', PChar('"' + TargetConfig.Target.RootDir + '\bin\dcc32.exe" -Q -M'));
    end;
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
    SetEnvironmentVariable('BPLDIR', Pointer(TargetConfig.BplDir));
    SetEnvironmentVariable('DCPDIR', Pointer(TargetConfig.DcpDir));
    SetEnvironmentVariable('LIBDIR', Pointer(TargetConfig.DcpDir));  // for BCB
    SetEnvironmentVariable('HPPDIR', Pointer(TargetConfig.HppDir)); // for BCB
    SetEnvironmentVariable('BPILIBDIR', Pointer(TargetConfig.DcpDir)); // for BCB


   // add dxgettext unit directory
    if Data.JVCLConfig.Enabled['USE_DXGETTEXT'] then
      SetEnvironmentVariable('EXTRAUNITDIRS', Pointer(TargetConfig.DxgettextDir))
    else
      SetEnvironmentVariable('EXTRAUNITDIRS', nil);
    SetEnvironmentVariable('EXTRAINCLUDEDIRS', nil);
    SetEnvironmentVariable('EXTRARESDIRS', nil);


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

   // generate temnplate.cfg file for PkgDir
    SetEnvironmentVariable('EDITION', PChar(Edition));
    SetEnvironmentVariable('PKGDIR', PChar(PkgDir));
    SetEnvironmentVariable('PKGDIR_MASTEREDITION', PChar(PkgDir));
    if Make(TargetConfig, Args + ' Templates', CaptureLine) <> 0 then
    begin
      AbortReason := Format(RsErrorGeneratingTemplatesForDir, [PkgDir]);
      Exit;
    end;

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

{**}DoProjectProgress(RsGeneratingResources, GetProjectIndex, ProjectMax);
    if not GenerateResources(TargetConfig) then
      Exit; // AbortReason is set in GenerateResources

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
          // get number of packages that will be compiled
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

      if FPkgCount > 0 then
      begin
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
  finally
{**}DoProjectProgress(RsFinished, ProjectMax, ProjectMax);
    FCurrentProjectGroup := nil;
  end;
  Result := True;
  DeleteFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
  DeleteFile(TargetConfig.JVCLPackagesDir + '\tmp.bat');
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
  Dependencies, S, PasFile: string;
  DeleteFiles: Boolean;
  BplFilename: string;
begin
  BplFilename := ProjectGroup.TargetConfig.BplDir + '\' + ProjectGroup.BpgName;

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
    Lines.Add('MAKE = "$(ROOT)\bin\make" -l+'{-$(MAKEFLAGS)'});
    Lines.Add('DCC = "$(ROOT)\bin\dcc32.exe" $(DCCOPT)');
    Lines.Add('');

   // for JCL .dcp files
    Lines.Add(Format('.path.dcp = "%s";"%s";"%s";"%s"',
      [ProjectGroup.TargetConfig.BplDir, ProjectGroup.TargetConfig.DcpDir,
       ProjectGroup.Target.BplDir, ProjectGroup.Target.DcpDir]));
    if AutoDepend then
    begin
      S := ProjectGroup.TargetConfig.JVCLDir;
      Lines.Add(Format('.path.pas = "%s\common";"%s\run";"%s\design";"%s\qcommon";"%s\qrun";"%s\qdesign";"%s"',
        [S, S, S, S, S, S,
         ProjectGroup.TargetConfig.DxgettextDir]));
      Lines.Add(Format('.path.dfm = "%s\run";"%s\design";"%s\qrun";"%s\qdesign"',
        [S, S, S, S]));
      Lines.Add(Format('.path.xfm = "%s\run";"%s\design";"%s\qrun";"%s\qdesign"',
        [S, S, S, S]));
      Lines.Add(Format('.path.inc = "%s\common"', [S]));
      Lines.Add(Format('.path.res = "%s\Resources"', [S]));
      Lines.Add(Format('.path.bpl = "%s";"%s"',
        [ProjectGroup.TargetConfig.BplDir, ProjectGroup.TargetConfig.DcpDir]));
      Lines.Add('');
      
     // add files like jvcl.inc
      Dependencies := '';
      for depI := 0 to High(CommonDependencyFiles) do
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
          ExtractFileName(CommonDependencyFiles[depI]);

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
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
           ProjectGroup.FindPackagebyXmlName(Pkg.JvDependencies[depI]).TargetName;

     // add JCL dependencies
      for depI := 0 to Pkg.JclDependencies.Count - 1 do
        Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
           Pkg.JclDependencies[depI] + '.dcp';


      if AutoDepend then
      begin
       // Add all contained files and test for their condition.
        for depI := 0 to Pkg.Info.ContainCount - 1 do
          if IsCondition(Pkg.Info.Contains[depI].Condition) then
          begin
            Dependencies := Dependencies + '\' + sLineBreak + #9#9 +
              ExtractFileName(Pkg.Info.Contains[depI].Name);
            {if Pkg.Info.Contains[depI].FormName <> '' then
            begin
              if ProjectGroup.IsVCLX then
                S := ChangeFileExt(ExtractFileName(Pkg.Info.Contains[depI].Name), '.xfm')
              else
                S := ChangeFileExt(ExtractFileName(Pkg.Info.Contains[depI].Name), '.dfm');
              Dependencies := Dependencies + '\' + sLineBreak + #9#9 + S;
            end;}
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
         // So we must delete them before compilation.
         // This is not needed when building the JVCL for BCB because all .obj
         // files will be deleted by the installer before entering compilation
         // process.
          DeleteFiles := False;
          for depI := 0 to Pkg.Info.ContainCount - 1 do
          begin
            PasFile := FollowRelativeFilename(Data.JVCLPackagesXmlDir, Pkg.Info.Contains[depI].Name);
            S := ExtractFileName(Pkg.Info.Contains[depI].Name);
            if CompareText(ExtractFileExt(S), '.pas') = 0 then
            begin
              S := ProjectGroup.TargetConfig.UnitOutDir + '\obj\' + ChangeFileExt(S, '.obj');
              if FileExists(S) then
                if not FileExists(PasFile) or // unknown directory for the .pas file
                   (CompareFileAge(S, [], PasFile, []) < 0) or
                   (FileExists(BplFilename) and (
                    (CompareFileAge(S, [], BplFilename, []) < 0) or
                    (CompareFileAge(PasFile, [], BplFilename, []) < 0))
                   ) then
                begin
                  DeleteFiles := True;
                  Break;
                end;
            end;
          end;

          if DeleteFiles then
          begin
            for depI := 0 to Pkg.Info.ContainCount - 1 do
            begin
              S := ExtractFileName(Pkg.Info.Contains[depI].Name);
              if CompareText(ExtractFileExt(S), '.pas') = 0 then
              begin
                S := ProjectGroup.TargetConfig.UnitOutDir + '\obj\' + ChangeFileExt(S, '.obj');
                if FileExists(S) then
                  Lines.Add(#9'-@del "' + S + '" >NUL');
              end;
            end;
          end;
        end;
        Lines.Add(#9'$(BPR2MAK) $&.bpk');
        Lines.Add(#9'@echo.');                 // prevent the "......Borland De"
        Lines.Add(#9'$(MAKE) -f $&.mak');
      end
      else
        Lines.Add(#9'$(DCC) $&.dpk');
      Lines.Add(#9'@cd ' + GetReturnPath(Pkg.RelSourceDir));
      Lines.Add('');
    end;
    Lines.SaveToFile(ChangeFileExt(ProjectGroup.Filename, '.mak'));
  finally
    Lines.Free;
  end;
end;

function TJVCLCompiler.IsCondition(const Condition: string): Boolean;
begin
  Result := True;
  if Condition <> '' then
  begin
    if Condition[1] = '!' then
      Result := not Data.JVCLConfig.Enabled[Copy(Condition, 2, MaxInt)]
    else
      Result := Data.JVCLConfig.Enabled[Condition];
  end;
end;

end.
