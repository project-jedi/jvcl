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
  GenerateUtils, PackageUtils, Intf, PackageInformation, ConditionParser,
  JvVCL5Utils;

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

    function IsPackageUsed(ProjectGroup: TProjectGroup;
      RequiredPackage: TRequiredPackage): Boolean;
    function IsFileUsed(ProjectGroup: TProjectGroup;
      ContainedFile: TContainedFile): Boolean;
    procedure SortProjectGroup(Group: TProjectGroup; List: TList);
  protected
    function Dcc32(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt: string; DebugUnits: Boolean; Files, ObjFiles: TStrings): Integer;
    function Bcc32(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const BccOpt: string; DebugUnits: Boolean; Files: TStrings; ObjFiles: TStrings): Integer;
    function Ilink32(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const IlinkOpt: string; DebugUnits: Boolean; ObjFiles, LibFiles, ResFiles: TStrings): Integer;
    function Tlib(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const TlibOpt: string; DebugUnits: Boolean; ObjFiles: TStrings): Integer;
    function Make(TargetConfig: ITargetConfig; Args: string;
      CaptureLine: TCaptureLine; StartDir: string = ''): Integer;
    function CompileCppPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt, BccOpt, IlinkOpt: string; DebugUnits: Boolean): Integer;
    function CompileDelphiPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt: string; DebugUnits: Boolean): Integer;

    function WriteDcc32Cfg(const Directory: string; TargetConfig: ITargetConfig;
      const DccOpt: string; DebugUnits: Boolean): string; // returns the dcc32.cfg filename
    procedure DoIdle(Sender: TObject);

    procedure LinkMapFile(TargetConfig: ITargetConfig; Project: TPackageTarget;
      DebugUnits: Boolean);

    procedure CaptureLine(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineClean(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
    procedure CaptureLinePackageCompilation(const Line: string; var Aborted: Boolean);
    procedure CaptureLineResourceCompilation(const Line: string; var Aborted: Boolean);
    procedure CaptureStatusLineDcc32(const Line: string; var Aborted: Boolean);

    procedure DoTargetProgress(Current: TTargetConfig; Position, Max: Integer); virtual;
    procedure DoProjectProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoResourceProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoPackageProgress(Current: TPackageTarget; const Text: string;
      Position, Max: Integer); virtual;

    procedure DoProgress(const Text: string; Position, Max: Integer;
      Kind: TProgressKind); virtual;
      // DoProgress is called by every DoXxxProgress method

    function CompileProjectGroup(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
    function GenerateResources(TargetConfig: ITargetConfig): Boolean;
    function DeleteFormDataFiles(ProjectGroup: TProjectGroup): Boolean;
    function CopyFormDataFiles(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;

    function IsCondition(const Condition: string; TargetConfig: ITargetConfig): Boolean;
    function GeneratePackages(const Group, Targets, PackagesPath: string): Boolean; overload;
    function GenerateAllPackages: Boolean; overload;
    function CompileTarget(TargetConfig: TTargetConfig; PackageGroupKind: TPackageGroupKind): Boolean;
  public
    constructor Create(AData: TJVCLData);
    destructor Destroy; override;

    function IsDcc32BugDanger: Boolean;

    function Compile: Boolean;
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
  ProjectMaxProgress = 3;
  MaxDcc32PathLen = 80;

var
  Compiler: TCompiler = nil;

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
  RsPostCompilationOperations = 'Post-compilation operations...';
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
  RsErrorLinkingMapFiles = 'An error occured while linking the map files into binaries.';
  RsErrorDeletingMapFiles = 'An error occured while deleting the map files after the linking.';

  RsCommandNotFound = 'Command could not be executed.'#10#10#10'Cmdline: %s'#10#0'Start directory: %s';

const
  sGeneratePackages = '[Generating: Packages]'; // do not localize
  sLinkingMapFiles = '[Linking: map files]'; // do not localize

const
  CommonDependencyFiles: array[0..5] of string = (
    'jvcl.inc', 'jvclbase.inc', 'jvcl%t.inc', 'jedi.inc', 'linuxonly.inc', 'windowsonly.inc'
  );

function CutPersEdition(const Edition: string): string;
var
  i: Integer;
begin
  Result := Edition;
  for i := 2 to Length(Result) do
    if not (Result[i] in ['0'..'9']) then
    begin
      Result := Copy(Result, 1, i - 1);
      Exit;
    end;
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
    Insert(Format('%s%d', [LowerCase(TargetConfig.Target.TargetType), TargetConfig.Target.Version]),
      Result, ps);
  end;
end;

{ TCompiler }

constructor TCompiler.Create(AData: TJVCLData);
begin
  inherited Create;
  FData := AData;
  FOutput := TStringList.Create;
  CaptureStatusLine := CaptureStatusLineDcc32;
end;

destructor TCompiler.Destroy;
begin
  CaptureStatusLine := nil;
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
  //Aborted := FAborted;
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
  //Aborted := FAborted;
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

procedure TCompiler.CaptureStatusLineDcc32(const Line: string; var Aborted: Boolean);
begin
  CaptureLine(Line, Aborted);
end;

procedure TCompiler.Abort;
begin
  FAborted := True;
end;

procedure WriteMsg(const Text: string); // used by TCompiler.GeneratePackages
begin
  Compiler.CaptureLine(Text, Compiler.FAborted);
end;

procedure TCompiler.DoIdle(Sender: TObject);
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
end;

/// <summary>
/// Make calls the make.exe of the given TargetConfig. If the StartDir is empty
/// the JVCLPackageDir\bin directory is used. If the command could not be
/// executed a message dialog is shown with the complete command line. Returns
/// the ExitCode of the last/failed command.
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
                           StartDir, CaptureLine, DoIdle,
                           False, TargetConfig.GetPathEnvVar);
  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.Make + '"' + Args, StartDir])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// WriteDcc32Cfg() writes the dcc32.cfg file to the directory
/// </summary>
function TCompiler.WriteDcc32Cfg(const Directory: string; TargetConfig: ITargetConfig;
  const DccOpt: string; DebugUnits: Boolean): string;
var
  Lines: TStrings;
  SearchPaths, S: string;
  i: Integer;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  Lines := TStringList.Create;
  try
    // opts
    Lines.Add(DccOpt);

    // search paths
    SearchPaths := '';
    with TargetConfig do
      for i := 0 to TargetConfig.Target.SearchPaths.Count - 1 do
      begin
        S := ExcludeTrailingPathDelimiter(Target.ExpandDirMacros(Target.SearchPaths[i]));
        if DirectoryExists(S) then
        begin
          if SearchPaths <> '' then
            SearchPaths := SearchPaths + ';' + S
          else
            SearchPaths := S;
        end;
      end;
    Lines.Add('-U"' + SearchPaths + '"');
    Lines.Add('-I"' + SearchPaths + '"');
    Lines.Add('-R"' + SearchPaths + '"');
    Lines.Add('-O"' + SearchPaths + '"');

    SearchPaths := TargetConfig.Target.ExpandDirMacros(
      TargetConfig.Target.RootLibDir + ';' +
      TargetConfig.Target.RootLibDir + PathDelim + 'obj;' +
      TargetConfig.JclDcpDir + ';' +
      TargetConfig.JclDcuDir + ';' +
      OutDirs.DcpDir + ';' +
      OutDirs.UnitOutDir
    );
    Lines.Add('-U"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Common' + '"');
    Lines.Add('-I"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Common' + '"');
    Lines.Add('-R"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Resources' + '"');
    Lines.Add('-O"' + SearchPaths + '"');

    // output directories
    Lines.Add('-LE"' + OutDirs.BplDir + '"'); // .exe output
    Lines.Add('-LN"' + OutDirs.DcpDir + '"'); // .dcp output
    Lines.Add('-N0"' + OutDirs.UnitOutDir + '"'); // .dcu output
    Lines.Add('-N1"' + OutDirs.HppDir + '"'); // .hpp output
//    Lines.Add('-NH"' + OutDirs.HppDir + '"'); // .hpp output
    Lines.Add('-N2"' + OutDirs.UnitOutDir + '"'); // .obj output
//    Lines.Add('-NO"' + OutDirs.UnitOutDir + '"'); // .obj output
    Lines.Add('-NB"' + OutDirs.DcpDir + '"'); // .bpi output

    { dcc32.exe crashes if the path is too long }
    if IsDcc32BugDanger then
      Lines.Add('-Q');
    if TargetConfig.Target.IsPersonal then
      Lines.Add('-DDelphiPersonalEdition');

    Result := Directory + '\dcc32.cfg';
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

/// <summary>
/// Dcc32() compiles a Delphi.Win32 package. If the command could not be
/// executed a message dialog is shown with the complete command line. Returns
/// the ExitCode of the last/failed command.
/// </summary>
function TCompiler.Dcc32(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const DccOpt: string; DebugUnits: Boolean; Files, ObjFiles: TStrings): Integer;
const
  MaxCmdLineLength = 2048 - 1;
var
  Dcc32Cfg, PrjFilename: string;
  Filename, Args, CmdLine, S: string;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);
  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  if Files.Count > 0 then
    Dcc32Cfg := WriteDcc32Cfg(ExtractFileDir(PrjFilename), TargetConfig, DccOpt, DebugUnits);

  CmdLine := '';
  Result := 0;
  try
    // collect files, limited by the MaxCmdLineLength
    while Files.Count > 0 do
    begin
      if FAborted then
        Break;

      Filename := Files[0];
      if Pos(' ', Filename) > 0 then
        S := ' "' + Filename + '"'
      else
        S := ' ' + Filename;
      Files.Delete(0);
      CmdLine := '"' + TargetConfig.Target.Dcc32 + '"' + S;
      Args := S;

      while Files.Count > 0 do
      begin
        Filename := Files[0];
        if Pos(' ', Filename) > 0 then
          S := ' "' + Filename + '"'
        else
          S := ' ' + Filename;
        if Length(CmdLine + S) > MaxCmdLineLength then
          Break;

        CmdLine := CmdLine + S;
        Files.Delete(0);
        if Assigned(ObjFiles) then
          ObjFiles.Add(OutDirs.UnitOutDir + PathDelim + ChangeFileExt(ExtractFileName(Filename), '.obj'));
      end;

      if Data.Verbose then
      begin
        // output command line
        CaptureLinePackageCompilation(#1 + CmdLine, FAborted);
      end;

      Result := CaptureExecute('"' + TargetConfig.Target.Dcc32 + '"', Args,
                               ExtractFileDir(PrjFilename), CaptureLinePackageCompilation, DoIdle,
                               False, TargetConfig.GetPathEnvVar);
      if Result <> 0 then
        Break;
    end;
  finally
    if not CmdOptions.KeepFiles then
      DeleteFile(Dcc32Cfg);
  end;

  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      [CmdLine,
                       ExtractFileDir(PrjFilename)])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// Bcc32() compiles C++ files. It adds the compiles .obj file names to ObjFiles
/// If the command could not be executed a message dialog is shown with the
/// complete command line. Returns the ExitCode of the last/failed command.
/// </summary>
function TCompiler.Bcc32(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const BccOpt: string; DebugUnits: Boolean; Files: TStrings; ObjFiles: TStrings): Integer;
var
  Lines: TStrings;
  i: Integer;
  RspFilename: string;
  ObjFilename: string;
  NothingToDo: Boolean;
  ObjAge: Integer;
  CsmAge: Integer;
  OutDirs: TOutputDirs;
begin
  Result := 0;
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);
  RspFilename := Project.SourceDir + PathDelim + ChangeFileExt(ExtractFileName(Project.SourceName), '.@@@');

  CsmAge := FileAge(Format('%s\lib\vcl%d0.csm', [TargetConfig.Target.RootDir, TargetConfig.Target.Version]));

  NothingToDo := True;
  Lines := TStringList.Create;
  try
    Lines.Add(BccOpt);
    Lines.Add(Format('-I"%s\include;%s\include\vcl"',
                     [TargetConfig.Target.RootDir, TargetConfig.Target.RootDir]));
    if DebugUnits then
      Lines.Add('-D_DEBUG -y -v');
    Lines.Add('-D_RTLDLL;NO_STRICT;USEPACKAGES');
    if TargetConfig.Target.IsPersonal then
      Lines.Add('-DDelphiPersonalEdition');
    Lines.Add(Format('-O2 -H="%s\lib\vcl%d0.csm" -Hu -Vx -Ve -r -a8 -b- -k- -vi- -c -tWM',
                     [TargetConfig.Target.RootDir, TargetConfig.Target.Version]));
    Lines.Add('-n"' + OutDirs.UnitOutDir + '"');
    // add files
    for i := 0 to Files.Count - 1 do
    begin
      ObjFilename := OutDirs.UnitOutDir + PathDelim + ExtractFileName(ChangeFileExt(Files[i], '.obj'));
      ObjAge := FileAge(ObjFilename);
      if Assigned(ObjFiles) then
        ObjFiles.AddObject(ObjFilename, TObject(ObjAge));
      if not TargetConfig.AutoDependencies or
         (ObjAge < CsmAge) or (ObjAge < FileAge(ExtractFilePath(RspFilename) + Files[i])) then
      begin
        Lines.Add('"' + Files[i] + '"');
        NothingToDo := False;
      end;
    end;
    if NothingToDo then
    begin
      Exit;
    end;
    Lines.SaveToFile(RspFilename);
  finally
    Lines.Free;
  end;

  if Data.Verbose then
  begin
    // output command line
    CaptureLinePackageCompilation(#1 + '"' + TargetConfig.Target.Bcc32 + '" @' + ExtractFileName(RspFilename), FAborted);
  end;
  try
    Result := CaptureExecute('"' + TargetConfig.Target.Bcc32 + '"', '@' + ExtractFileName(RspFilename),
                             ExtractFileDir(RspFilename), CaptureLinePackageCompilation, DoIdle,
                             False, TargetConfig.GetPathEnvVar);
  finally
    if not CmdOptions.KeepFiles then
      DeleteFile(RspFilename);
  end;

  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.Bcc32 + '" @' + ExtractFileName(RspFilename),
                       ExtractFileDir(RspFilename)])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// Ilink32() links .obj, .lib and .res files. If the command could not be
/// executed a message dialog is shown with the complete command line. Returns
/// the ExitCode of the last/failed command.
/// </summary>
function TCompiler.Ilink32(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const IlinkOpt: string; DebugUnits: Boolean; ObjFiles, LibFiles, ResFiles: TStrings): Integer;
var
  Lines: TStrings;
  RspFilename: string;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);
  RspFilename := Project.SourceDir + PathDelim + ChangeFileExt(ExtractFileName(Project.SourceName), '.@@@');

  Lines := TStringList.Create;
  try
    Lines.Add(IlinkOpt + ' +');
    if DebugUnits then
      Lines.Add(Format('-L"%s\lib\debug" +', [TargetConfig.Target.RootDir]))
    else
      Lines.Add(Format('-L"%s\lib\release" +', [TargetConfig.Target.RootDir]));
    Lines.Add(Format('-L"%s\lib\obj;%s\lib" +',
                     [TargetConfig.Target.RootDir, TargetConfig.Target.RootDir]));
    Lines.Add(Format('-I"%s" +', [OutDirs.UnitOutDir])); // intermediate output dir
    Lines.Add(Format('-j"%s" +', [OutDirs.UnitOutDir])); // .obj search path
    Lines.Add(Format('-D"%s" +', [Project.Info.Description]));
    Lines.Add(Format('-L"%s\common;%s\Resources;%s\design;%s\run" +', // resource files
                     [TargetConfig.JVCLDir, TargetConfig.JVCLDir, TargetConfig.JVCLDir, TargetConfig.JVCLDir]));
    if DebugUnits then
      Lines.Add('-v +');
    if not TargetConfig.GenerateMapFiles then
      Lines.Add('-x +');

    Lines.Add('-aa -Tpp -Gn -Gl -Gi +');
    if Project.Info.XmlInfo.ImageBase <> '' then
      Lines.Add('-b:0x' + Project.Info.XmlInfo.ImageBase + ' +');
    case Project.Info.ProjectType of
      ptPackageRun:
        Lines.Add('-Gpr +');
      ptPackageDesign:
        Lines.Add('-Gpd +');
      ptPackage: ;
      ptLibrary: ;
      ptProgram: ;
    end;

    Lines.Add(Format('-L"%s;%s" -l"%s" +', [TargetConfig.JCLDcpDir, OutDirs.DcpDir, OutDirs.DcpDir]));

    // add .obj files
    Lines.Add(ConcatPaths(ObjFiles, ' ') + ', +');
    Lines.Add('"' + OutDirs.BplDir + PathDelim + Project.TargetName + '", +');
    Lines.Add('"' + OutDirs.BplDir + PathDelim + ChangeFileExt(Project.TargetName, '.map') + '", +');
    Lines.Add(ConcatPaths(LibFiles, ' ') + ',, +');
    Lines.Add(ConcatPaths(ResFiles, ' '));

    Lines.SaveToFile(RspFilename);
  finally
    Lines.Free;
  end;

  if Data.Verbose then
  begin
    // output command line
    CaptureLinePackageCompilation(#1 + '"' + TargetConfig.Target.Ilink32 + '" @' + ExtractFileName(RspFilename), FAborted);
  end;
  try
    Result := CaptureExecute('"' + TargetConfig.Target.Ilink32 + '"', '@' + ExtractFileName(RspFilename),
                             ExtractFileDir(RspFilename), CaptureLinePackageCompilation, DoIdle,
                             False, TargetConfig.GetPathEnvVar);
  finally
    if not CmdOptions.KeepFiles then
      DeleteFile(RspFilename);
  end;

  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.Ilink32 + '" @' + ExtractFileName(RspFilename),
                       ExtractFileDir(RspFilename)])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// Tlib() creates a .lib file. If the command could not be executed a message
/// dialog is shown with the complete command line. Returns the ExitCode of the
/// last/failed command.
/// </summary>
function TCompiler.Tlib(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const TlibOpt: string; DebugUnits: Boolean; ObjFiles: TStrings): Integer;
var
  Lines: TStrings;
  RspFilename: string;
  LibFilename: string;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  RspFilename := Project.SourceDir + PathDelim + ChangeFileExt(ExtractFileName(Project.SourceName), '.@@@');
  LibFilename := OutDirs.DcpDir + PathDelim + ChangeFileExt(ExtractFileName(Project.SourceName), '.lib');
  DeleteFile(LibFilename);

  Lines := TStringList.Create;
  try
    Lines.Add(TLibOpt + ' &');
    // add .obj files
    if ObjFiles.Count > 0 then
      Lines.Add(' +"' + ConcatPaths(ObjFiles, '" &'#13#10 + ' +"') + '"');

    Lines.SaveToFile(RspFilename);
  finally
    Lines.Free;
  end;

  if Data.Verbose then
  begin
    // output command line
    CaptureLinePackageCompilation(#1 + '"' + TargetConfig.Target.Tlib + '" "' + LibFilename + '" @' + ExtractFileName(RspFilename), FAborted);
  end;
  try
    Result := CaptureExecute('"' + TargetConfig.Target.Tlib + '"', '"' + LibFilename + '" @' + ExtractFileName(RspFilename),
                             ExtractFileDir(RspFilename), CaptureLinePackageCompilation, DoIdle,
                             False, TargetConfig.GetPathEnvVar);
  finally
    if not CmdOptions.KeepFiles then
      DeleteFile(RspFilename);
  end;

  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound,
                      ['"' + TargetConfig.Target.TLib + '" "' + LibFilename + '" @' + ExtractFileName(RspFilename),
                       ExtractFileDir(RspFilename)])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// CompileCppPackage() compiles a BCB.Win32 package. If one of the commands
/// could not be executed a message dialog is shown with the complete command
/// line of the failed command. Returns the ExitCode of the last/failed command.
/// </summary>
function TCompiler.CompileCppPackage(TargetConfig: ITargetConfig;
  Project: TPackageTarget; const DccOpt, BccOpt, IlinkOpt: string;
  DebugUnits: Boolean): Integer;
var
  PrjFilename, PkgFilename, ResFilename: string;
  PasFiles, CppFiles, ObjFiles, LibFiles, ResFiles: TStrings;
  i, ObjAge, OldestObjAge, AgeIndex: Integer;
  Dcc32Packages: string;
  BplFilename, DcpFilename, ObjFilename: string;
  BplAge, DcpAge, LibAge, BpiAge: Integer;
  Changed: Boolean;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  BplFilename := OutDirs.BplDir + PathDelim + Project.TargetName;
  DcpFilename := OutDirs.DcpDir + PathDelim + Project.DcpName;
  BplAge := FileAge(BplFilename);
  DcpAge := FileAge(DcpFilename);
  LibAge := FileAge(ChangeFileExt(DcpFilename, '.lib'));
  BpiAge := FileAge(ChangeFileExt(DcpFilename, '.bpi'));

  PasFiles := nil;
  CppFiles := nil;
  ObjFiles := nil;
  LibFiles := nil;
  ResFiles := nil;
  try
    PasFiles := TStringList.Create;
    CppFiles := TStringList.Create;
    ObjFiles := TStringList.Create;
    LibFiles := TStringList.Create;
    ResFiles := TStringList.Create;

    CppFiles.Add(ChangeFileExt(PrjFilename, '.cpp')); // <project>.cpp
    ResFiles.Add(ChangeFileExt(PrjFilename, '.res')); // <project>.res
    for i := 0 to Project.ContainCount - 1 do
    begin
      if IsFileUsed(Project.Owner, Project.Contains[i]) then
      begin
        PasFiles.Add(Project.Contains[i].Name);
        if Project.Contains[i].FormName <> '' then
        begin
          ResFilename := ChangeFileExt(Project.Contains[i].Name, '.xfm');
          if not FileExists(ResFilename) then
            ResFilename := ChangeFileExt(Project.Contains[i].Name, '.dfm');
          ResFiles.Add(ResFilename);
        end;
      end;
    end;

    ObjFiles.Add('c0pkg32.obj');
    Dcc32Packages := '';
    for i := 0 to Project.RequireCount - 1 do
    begin
      if IsPackageUsed(Project.Owner, Project.Requires[i]) then
      begin
        // obtain DCP filename
        { TODO : Change this if the .dcp filename convention changes }
        PkgFilename := ChangeFileExt(Project.Requires[i].GetBplName(Project.Owner), '');
        ObjFiles.Add(PkgFilename + '.bpi');
        Dcc32Packages := Dcc32Packages + ';' + PkgFilename;
      end;
    end;
    if Dcc32Packages <> '' then
      Dcc32Packages := ' -LU"' + Copy(Dcc32Packages, 2, MaxInt) + '"';
    ObjFiles.Add('Memmgr.lib');
    ObjFiles.Add('sysinit.obj');

    LibFiles.Add('import32.lib');
    LibFiles.Add('cp32mti.lib');

    // add additional .lib files
    if TargetConfig.Target.Version = 5 then
      LibFiles.AddStrings(Project.Info.XmlInfo.C5Libs)
    else
    if TargetConfig.Target.Version = 6 then
      LibFiles.AddStrings(Project.Info.XmlInfo.C6Libs)
    else
    if TargetConfig.Target.Version = 10 then // not used
      LibFiles.AddStrings(Project.Info.XmlInfo.C10Libs);

    AgeIndex := ObjFiles.Count;
    // add .pas.obj files
    OldestObjAge := 0;
    for i := 0 to PasFiles.Count - 1 do
    begin
      ObjFilename := OutDirs.UnitOutDir + PathDelim + ChangeFileExt(ExtractFileName(PasFiles[i]), '.obj');
      if TargetConfig.AutoDependencies then
        ObjAge := FileAge(ObjFilename)
      else
        ObjAge := -1;
      ObjFiles.AddObject(ObjFilename, TObject(ObjAge));
      if ObjAge > OldestObjAge then
        OldestObjAge := ObjAge;
    end;

    // compile Delphi files (creates only .dcu, .obj and .hpp files)
    Result := CompileDelphiPackage(TargetConfig, Project, DccOpt + ' -JPHNE --BCB', DebugUnits);
    if Result <> 0 then
      Exit;

    Changed := not TargetConfig.AutoDependencies or
               HaveFilesChanged(ObjFiles, AgeIndex) or
               (DcpAge < OldestObjAge);
    if Changed then
    begin
      // compile Delphi package (only modified) to get .dcp file (.bpl is also created)
      Result := CompileDelphiPackage(TargetConfig, Project,
                     StringReplace(' ' + DccOpt + ' ', ' -B ', '', [rfReplaceAll]) + ' --BCB',
                     DebugUnits);
      if Result <> 0 then
        Exit;
    end;

    { Delete the dcc32 generated .lsp file which is not used at all. }
    DeleteFile(Project.SourceDir + PathDelim +
               ExtractFileName(ChangeFileExt(Project.SourceName, '.lsp')));

    AgeIndex := ObjFiles.Count;
    // compile C++ files
    Result := Bcc32(TargetConfig, Project, BccOpt, DebugUnits, CppFiles, ObjFiles);
    if Result <> 0 then
      Exit;

    Changed := not TargetConfig.AutoDependencies or
               HaveFilesChanged(ObjFiles, AgeIndex) or
               (BplAge < OldestObjAge) or (LibAge < OldestObjAge) or (BpiAge < OldestObjAge);
    if Changed then
    begin
      // link files (create .lib, .bpi and .bpl)
      Result := Ilink32(TargetConfig, Project, IlinkOpt, DebugUnits, ObjFiles,
                        LibFiles, ResFiles);
      if Result <> 0 then
        Exit;
    end;
  finally
    PasFiles.Free;
    CppFiles.Free;
    ObjFiles.Free;
    LibFiles.Free;
    ResFiles.Free;
  end;
end;

/// <summary>
/// CompileDelphiPackage() compiles a Delphi.Win32 package. If one of the commands
/// could not be executed a message dialog is shown with the complete command
/// line of the failed command. Returns the ExitCode of the last/failed command.
/// </summary>
function TCompiler.CompileDelphiPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const DccOpt: string; DebugUnits: Boolean): Integer;
var
  Files: TStrings;
  i: Integer;
  DcpAge, FormAge, PasAge, DcuAge: Integer;
  PrjFilename: string;
  Filename: string;
  Changed: Boolean;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  Result := 0;
  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  DcpAge := FileAge(OutDirs.DcpDir + PathDelim + Project.DcpName);

  // .dpk/.bpk
  Changed := not TargetConfig.AutoDependencies or
             (DcpAge < FileAge(PrjFilename)) or
             (DcpAge < FileAge(ChangeFileExt(PrjFilename, '.res')));
  if not Changed then
  begin
    for i := 0 to High(CommonDependencyFiles) do
      if DcpAge < FileAge(TargetConfig.JVCLDir + '\common\' + ReplaceTargetMacros(CommonDependencyFiles[i], TargetConfig)) then
      begin
        Changed := True;
        Break;
      end;
  end;

  if not Changed then
  begin
    // required JVCL package
    for i := 0 to Project.JvDependencies.Count - 1 do
    begin
      if IsPackageUsed(Project.Owner, Project.JvDependenciesReqPkg[i]) then
      begin
        Filename := OutDirs.DcpDir + PathDelim +
                    TargetConfig.VersionedJVCLXmlDcp(Project.JvDependenciesReqPkg[i].Name);
        if FileAge(Filename) > DcpAge then
        begin
          Changed := True;
          Break;
        end;
      end;
    end;
  end;
  if not Changed then
  begin
    // required JVCL package
    for i := 0 to Project.JclDependencies.Count - 1 do
    begin
      if IsPackageUsed(Project.Owner, Project.JclDependenciesReqPkg[i]) then
      begin
        Filename := TargetConfig.JclBplDir + PathDelim +
                    TargetConfig.VersionedJclDcp(Project.JclDependenciesReqPkg[i].Name);
        if FileAge(Filename) > DcpAge then
        begin
          Changed := True;
          Break;
        end;
      end;
    end;
  end;

  if not Changed then
  begin
    // files
    for i := 0 to Project.ContainCount - 1 do
    begin
      if IsFileUsed(Project.Owner, Project.Contains[i]) then
      begin
        // .pas
        PasAge := FileAge(Project.SourceDir + PathDelim + Project.Contains[i].Name);
        DcuAge := FileAge(OutDirs.UnitOutDir + PathDelim +
                          ChangeFileExt(ExtractFileName(Project.Contains[i].Name), '.dcu'));
        if (PasAge > DcuAge) or (DcpAge < PasAge) then 
        begin
          Changed := True;
          Break;
        end;
        // .dfm/.xfm
        if Project.Contains[i].FormName <> '' then
        begin
          FormAge := FileAge(ChangeFileExt(Project.SourceDir + PathDelim +
                             Project.Contains[i].Name, '.dfm'));
          if FormAge = -1 then
            FormAge := FileAge(ChangeFileExt(Project.SourceDir + PathDelim +
                               Project.Contains[i].Name, '.xfm'));
          if (FormAge <> -1) and (DcpAge < FormAge) then
          begin
            Changed := True;
            Break;
          end;
        end;
      end;
    end;
  end;

  if Changed then
  begin
    Files := TStringList.Create;
    try
      Files.Add(ExtractFileName(ChangeFileExt(Project.SourceName, '.dpk'))); // force .dpk
      Result := Dcc32(TargetConfig, Project, DccOpt, DebugUnits, Files, nil);
    finally
      Files.Free;
    end;
  end;
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

  CaptureLine(sGeneratePackages, FAborted);
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

  if FAborted then
  begin
    AbortReason := RsAbortedByUser;
    Exit;
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

/// <summary>
/// Compile() prepares for compiling and decides if the VCL or CLX framework
/// should be compiled.
/// </summary>
function TCompiler.Compile: Boolean;
var
  i, Index: Integer;
  Frameworks, Count: Integer;
  TargetConfigs: array of TTargetConfig;
  SysInfo: string;
begin
  Result := True;
  FAborted := False;

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
  CaptureLine(Format('JVCL %d.%d.%d.%d', 
    [JVCLVersionMajor, JVCLVersionMinor, JVCLVersionRelease, JVCLVersionBuild]), 
	FAborted);
  CaptureLine('', FAborted);

  AbortReason := '';
  // read target configs that should be compiled
  Count := 0;
  Frameworks := 0;
  SetLength(TargetConfigs, Data.Targets.Count);
  for i := 0 to High(TargetConfigs) do
  begin
    if Data.TargetConfig[i].InstallJVCL then
    begin
      TargetConfigs[Count] := Data.TargetConfig[i];
      Inc(Count);
      if pkVCL in Data.TargetConfig[i].InstallMode then
        Inc(Frameworks);
      if pkCLX in Data.TargetConfig[i].InstallMode then
        Inc(Frameworks);
    end;
  end;
  SetLength(TargetConfigs, Count);

  // compile all selected targets
  Index := 0;
  for i := 0 to Count - 1 do
  begin
    DoTargetProgress(TargetConfigs[i], Index, Frameworks);
    if pkVCL in TargetConfigs[i].InstallMode then
    begin
      Result := CompileTarget(TargetConfigs[i], pkVCL);
      if not Result then
        Break;
      Inc(Index);
    end;
    DoTargetProgress(TargetConfigs[i], Index, Frameworks);
    if pkClx in TargetConfigs[i].InstallMode then
    begin
      Result := CompileTarget(TargetConfigs[i], pkCLX);
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
function TCompiler.CompileTarget(TargetConfig: TTargetConfig; PackageGroupKind: TPackageGroupKind): Boolean;
var
  //ObjFiles: TStrings;
  //i: Integer;
  Aborted: Boolean;
  DoClx: Boolean;
begin
  DoClx := PackageGroupKind = pkClx;
  Result := True;
  Aborted := False;
  FOutput.Clear;

  // VCL
  if Result and (pkVCL in TargetConfig.InstallMode) and not DoClx then
  begin
    if not TargetConfig.DeveloperInstall then
    begin
      // debug units
      if TargetConfig.Target.SupportsPersonalities([persDelphi]) and
        TargetConfig.DebugUnits then
        Result := CompileProjectGroup(
          TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], True);
    end;

    if Result then
      // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], False);

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
      if TargetConfig.Target.SupportsPersonalities([persDelphi]) and TargetConfig.DebugUnits then
        Result := CompileProjectGroup(
          TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], True);
    end;

    if Result then
      // compile
      Result := CompileProjectGroup(
        TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkClx], False);

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

  if FAborted then
  begin
    AbortReason := RsAbortedByUser;
    Exit;
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
    DebugDestDir := ProjectGroup.TargetConfig.DebugUnitOutDir;

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
{**}DoProgress('', 0, Files.Count, pkOther);
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

procedure TCompiler.SortProjectGroup(Group: TProjectGroup; List: TList);

  procedure AddProject(Project: TPackageTarget);
  var
    i, Idx: Integer;
    Dep: TPackageTarget;
  begin
    for i := 0 to Project.RequireCount - 1 do
    begin
      Dep := Group.FindPackageByXmlName(Project.Requires[i].Name);
      if Dep <> nil then
      begin
        Idx := List.IndexOf(Dep);
        if Idx = -1 then
          List.Add(Dep);
      end;
    end;
  end;

var
  i, k, Idx: Integer;
  Project, DepProject: TPackageTarget;
begin
  for i := 0 to Group.Count - 1 do
    if Group[i].Compile then
      List.Add(Group[i]);

  // sort
  i := 0;
  while i < List.Count do
  begin
    Project := List[i];
    if Project.Compile then
    begin
      for k := 0 to Project.RequireCount - 1 do
      begin
        if IsPackageUsed(Group, Project.Requires[k]) then
        begin
          DepProject := Group.FindPackageByXmlName(Project.Requires[k].Name);
          if DepProject <> nil then
          begin
            Idx := List.IndexOf(DepProject);
            if Idx = -1 then // must insert the
            begin
              List.Insert(i, DepProject);
              Dec(i);
            end
            else
            if Idx > i then
            begin
              List.Move(idx, i);
              Dec(i);
            end;
          end;
        end;
      end;
    end;
    Inc(i);
  end;
end;

/// <summary>
/// CompileProjectGroup starts the make file for the templates, starts the
/// packages generator, calls the GenerateResource method and compiles all
/// selected packages of the project group.
/// </summary>
function TCompiler.CompileProjectGroup(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
var
  AProjectIndex, i: Integer;
  TargetConfig: ITargetConfig;
  DccOpt: string;
  Edition, PkgDir, JVCLPackagesDir: string;
  Files: TStrings;

  ProjectOrder: TList;
  Project: TPackageTarget;
  DebugProgress: string;
  
  function GetProjectIndex: Integer;
  begin
    Result := AProjectIndex;
    Inc(AProjectIndex);
  end;

begin
  Result := False;
  if FAborted then
    Exit;
  DebugProgress := '';
  if DebugUnits then
     DebugProgress := ' (Debug)';

  FCurrentProjectGroup := ProjectGroup;
  try
    TargetConfig := ProjectGroup.TargetConfig;

    { remove current JVCL but keep the "Installation tag" valid }
    TargetConfig.DeinstallJVCL(nil, nil, {RealUninstall:=}False);

    // obtain information for progress bar
    FPkgCount := 0;
    for i := 0 to ProjectGroup.Count - 1 do
      if ProjectGroup.Packages[i].Compile then
        Inc(FPkgCount);
    FPkgIndex := 0;

    Edition := TargetConfig.TargetSymbol;
    if ProjectGroup.IsVCLX then
      Edition := Edition + 'clx';
    JVCLPackagesDir := TargetConfig.JVCLPackagesDir;

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

    DccOpt := '-M'; // make modified units, output 'never build' DCPs

    if TargetConfig.Build then
      DccOpt := DccOpt + ' -B';
    if TargetConfig.GenerateMapFiles then
      DccOpt := DccOpt + ' -GD';
    if TargetConfig.Target.IsBDS and (persBCB in TargetConfig.Target.SupportedPersonalities) then
      // Dual packages, bpi and lib files for BDS 2006
      DccOpt := DccOpt + ' -JL'; // for BCB 5/6 the -JPHNE is set during compilation

    if not DebugUnits and not TargetConfig.DeveloperInstall then
      DccOpt := DccOpt + ' -DJVCL_NO_DEBUGINFO';

    if DebugUnits then
    begin
      ForceDirectories(TargetConfig.DebugUnitOutDir);
      ForceDirectories(TargetConfig.DebugBplDir);
      ForceDirectories(TargetConfig.DebugDcpDir);
      ForceDirectories(TargetConfig.DebugHppDir);
    end;

    { Create include directory if necessary }
    if TargetConfig.Target.SupportsPersonalities([persBCB]) then
      ForceDirectories(TargetConfig.Target.ExpandDirMacros(TargetConfig.HppDir));

   // *****************************************************************

{**}DoProjectProgress(RsGeneratingPackages + DebugProgress, GetProjectIndex, ProjectMaxProgress);
    // generate the packages and .cfg files for the "master" PkgDir
    if not GeneratePackages('JVCL', CutPersEdition(Edition),
                            TargetConfig.JVCLPackagesDir) then
      Exit; // AbortReason is set in GeneratePackages

   // *****************************************************************

{**}{DoProjectProgress(RsGeneratingResources, GetProjectIndex, ProjectMaxProgress);
    if not GenerateResources(TargetConfig) then
      Exit; // AbortReason is set in GenerateResources}

   // *****************************************************************

{**}DoProjectProgress(RsCompilingPackages + DebugProgress, GetProjectIndex, ProjectMaxProgress);
    { Remove .dfm/.xfm files from the lib directory so the compiler takes the
      correct one and we do not have unused files in the lib directory. }
    DeleteFormDataFiles(ProjectGroup);

    Files := TStringList.Create;
    try
      { .bpl and .dcp files meight be at the wrong location. So delete them from
        wrong locations. }
      TargetConfig.GetPackageBinariesForDeletion(Files);
      for i := 0 to Files.Count - 1 do
        if not StartsWith(Files[i], TargetConfig.BplDir + PathDelim, False) and
           not StartsWith(Files[i], TargetConfig.DcpDir + PathDelim, False) then
        begin
          if TargetConfig.DebugUnits then
          begin
            if StartsWith(Files[i], TargetConfig.DebugBplDir + PathDelim, False) and
               StartsWith(Files[i], TargetConfig.DebugDcpDir + PathDelim, False) then
              Continue;
          end;
          DeleteFile(Files[i]);
        end;
    finally
      Files.Free;
    end;

    { Now compile the packages }
    DoPackageProgress(nil, '', 0, FPkgCount);

    ProjectOrder := TList.Create;
    try
      SortProjectGroup(ProjectGroup, ProjectOrder);
      if ProjectOrder.Count > 0 then
        CaptureLinePackageCompilation('[Compiling: Packages]', FAborted);
      for i := 0 to ProjectOrder.Count - 1 do
      begin
        Project := ProjectOrder[i];
        CaptureLinePackageCompilation('[Compiling: ' + Project.TargetName + ']', FAborted);

        if TargetConfig.Target.IsBCB then
        begin
          if CompileCppPackage(TargetConfig, Project, DccOpt, '', '', DebugUnits) <> 0 then
          begin
            if FAborted then
              Exit;
            AbortReason := RsErrorCompilingPackages;
            Exit;
          end;
        end
        else
        begin
          { Create .dcp and .bpl }
          if CompileDelphiPackage(TargetConfig, Project, DccOpt, DebugUnits) <> 0 then
          begin
            if FAborted then
              Exit;
            AbortReason := RsErrorCompilingPackages;
            Exit;
          end;
        end;
        if FAborted then
          Exit;
      end;
    finally
      ProjectOrder.Free;
    end;
    DoPackageProgress(nil, '', FPkgCount, FPkgCount);

   // *****************************************************************

{**}DoProjectProgress(RsPostCompilationOperations + DebugProgress, GetProjectIndex, ProjectMaxProgress);
    if TargetConfig.GenerateMapFiles and TargetConfig.LinkMapFiles then
    begin
      CaptureLine(sLinkingMapFiles, FAborted);
      for i := 0 to ProjectGroup.Count - 1 do
        if ProjectGroup.Packages[i].Compile then
          LinkMapFile(TargetConfig, ProjectGroup.Packages[i], DebugUnits);
    end;

   // *****************************************************************

    if not ProjectGroup.TargetConfig.DeveloperInstall {or
       (TargetConfig.Target.SupportsPersonalities([persBCB], True))} then // only BCB
    begin
{**}  DoProjectProgress(RsCopyingFiles + DebugProgress, GetProjectIndex, ProjectMaxProgress);
      { The .dfm/.xfm files are deleted from the lib directory in the
        resource generation section in this method.
        The files are only copied for a non-developer installation and for
        BCB. }
      CopyFormDataFiles(ProjectGroup, DebugUnits);
    end
    else
{**}  GetProjectIndex; // increase progress

    if not FAborted then
    begin
      if TargetConfig.CleanPalettes then
        TargetConfig.CleanJVCLPalette(False);
      TargetConfig.RegisterToIDE;
    end;
  finally
{**}DoProjectProgress(RsFinished, ProjectMaxProgress, ProjectMaxProgress);
    FCurrentProjectGroup := nil;
  end;

  Result := True;
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

procedure TCompiler.LinkMapFile(TargetConfig: ITargetConfig;
  Project: TPackageTarget; DebugUnits: Boolean);
var
  BplFilename, MapFilename: string;
  MapFileSize, JclDebugDataSize: Integer;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  BplFileName := OutDirs.BplDir + PathDelim + Project.TargetName;
  MapFileName := ChangeFileExt(BplFileName, '.map');
  if FileExists(BplFilename) and FileExists(MapFileName) then
  begin
    CaptureLine(Format('Linking %s inside %s',
      [ExtractFileName(MapFileName), ExtractFileName(BplFileName)]), FAborted);
    if not TargetConfig.LinkMapFile(BplFileName, MapFileName,
      MapFileSize, JclDebugDataSize) then
    begin
      CaptureLine(Format('Error: Unable to link %s', [ExtractFileName(MapFileName)]), FAborted);
      AbortReason := RsErrorLinkingMapFiles;
      Exit;
    end;

    if TargetConfig.DeleteMapFiles then
    begin
      CaptureLine(Format('Deleting file %s', [ExtractFileName(MapFileName)]), FAborted);
      if not DeleteFile(MapFileName) then
      begin
        CaptureLine(Format('Error: Unable to delete %s', [MapFileName]), FAborted);
        AbortReason := RsErrorDeletingMapFiles;
        Exit;
      end;
    end;
  end;
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

function TCompiler.IsDcc32BugDanger: Boolean;
begin
  Result := Length(Data.JVCLDir) > MaxDcc32PathLen;
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


end.

