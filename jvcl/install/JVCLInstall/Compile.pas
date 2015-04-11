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
home page, located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit Compile;

{$I jvcl.inc}
{$I windowsonly.inc}

{$IF CompilerVersion >= 20.0} // Delphi 2009+
  {$STRINGCHECKS OFF} // who needs this performance killer, so turn it off
{$IFEND}

interface

uses
  Windows, SysUtils, Classes, CapExec, JVCLData, DelphiData,
  GenerateUtils, PackageUtils, Intf, PackageInformation, ConditionParser,
  JclBase, JclSysInfo, JVCLVer;

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
    procedure SortProject(Group: TProjectGroup; List: TList; Project: TPackageTarget; var ProjectIndex: Integer);
    procedure SortProjectGroup(Group: TProjectGroup; List: TList);
  protected
    function Dcc(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt: string; DebugUnits: Boolean; Files, ObjFiles: TStrings): Integer;
    function Bcc32(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const BccOpt: string; DebugUnits: Boolean; Files: TStrings; ObjFiles: TStrings): Integer;
    function Ilink32(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const IlinkOpt: string; DebugUnits: Boolean; ObjFiles, LibFiles, ResFiles: TStrings): Integer;
    function Tlib(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const TlibOpt: string; DebugUnits: Boolean; ObjFiles: TStrings): Integer;
    {function Make(TargetConfig: ITargetConfig; Args: string;
      CaptureLine: TCaptureLine; StartDir: string = ''): Integer;}
    function CompileCppPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt, BccOpt, IlinkOpt: string; DebugUnits: Boolean; var FilesCompiled: Boolean): Integer;
    function CompileDelphiPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
      const DccOpt: string; DebugUnits: Boolean; var FilesCompiled: Boolean): Integer;

    function WriteDccCfg(const Directory: string; TargetConfig: ITargetConfig;
      const DccOpt: string; DebugUnits: Boolean): string; // returns the dcc32.cfg filename
    procedure DoIdle(Sender: TObject);

    procedure LinkMapFile(TargetConfig: ITargetConfig; Project: TPackageTarget;
      DebugUnits: Boolean);

    procedure LogModify(const Name: string; const FilenameA: string; AgeA: Integer;
      const FilenameB: string; AgeB: Integer);
    procedure CaptureLine(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineClean(const Line: string; var Aborted: Boolean); virtual;
    procedure CaptureLineGetCompileCount(const Line: string; var Aborted: Boolean);
    procedure CaptureLinePackageCompilation(const Line: string; var Aborted: Boolean);
    procedure CaptureLineResourceCompilation(const Line: string; var Aborted: Boolean);
    procedure CaptureStatusLineDcc32(const Line: string; var Aborted: Boolean);

    procedure DoTargetProgress(Current: TTargetConfig; Position, Max: Integer); virtual;
    procedure DoProjectProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoResourceProgress(const Text: string; Position, Max: Integer); virtual;
    procedure DoPackageProgress(Current: TPackageTarget; const Text: string; Position, Max: Integer); virtual;

    procedure DoProgress(const Text: string; Position, Max: Integer;
      Kind: TProgressKind); virtual;
      // DoProgress is called by every DoXxxProgress method

    function CompileProjectGroup(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;
    //function GenerateResources(TargetConfig: ITargetConfig): Boolean;
    function DeleteFormDataFiles(ProjectGroup: TProjectGroup): Boolean;
    function CopyFormDataFiles(ProjectGroup: TProjectGroup; DebugUnits: Boolean): Boolean;

    function IsCondition(const Condition: string; TargetConfig: ITargetConfig): Boolean;
    function GeneratePackages(const Group, Targets, PackagesPath: string): Boolean; overload;
    function GenerateAllPackages: Boolean; overload;
    function CompileTarget(TargetConfig: TTargetConfig; PackageGroupKind: TPackageGroupKind): Boolean;
    procedure DeleteRemovedFiles(TargetConfig: TTargetConfig);
    procedure ClearSourceDirectory;

    //procedure AlterHppFiles(TargetConfig: ITargetConfig; Project: TPackageTarget);
  public
    constructor Create(AData: TJVCLData);
    destructor Destroy; override;

    function IsDcc32BugDangerous(TargetConfig: ITargetConfig): Boolean;

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
  {$IFNDEF COMPILER12_UP}
  JvJCLUtils,
  {$ENDIF ~COMPILER12_UP}
  JclSimpleXML, JclStreams, JclSysUtils, JclFileUtils, JclStrings,
  CmdLineUtils, JvConsts, Utils, Core, Dcc32FileAgePatch;

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
  CommonDependencyFiles: array[0..4] of string = (
    '%jvcl%\common\jvcl.inc',
    '%jvcl%\common\jvclbase.inc',
    '%jvcl%\common\jvcl%t%.inc',
    '%jvcl%\common\windowsonly.inc',
    '%jcl%\source\include\jedi\jedi.inc'
  );

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

{----------------------------------------------------------------------}

function ReplaceTargetMacros(const S: string; TargetConfig: ITargetConfig): string;
var
  ps: Integer;
begin
  Result := S;
  ps := Pos('%jcl%', Result);
  if ps > 0 then
  begin
    Delete(Result, ps, 5);
    Insert(TargetConfig.JclDir, Result, ps);
  end;
  ps := Pos('%jvcl%', Result);
  if ps > 0 then
  begin
    Delete(Result, ps, 6);
    Insert(TargetConfig.JVCLDir, Result, ps);
  end;

  ps := Pos('%t%', Result);
  if ps > 0 then
  begin
    Delete(Result, ps, 3);
    Insert(Format('%s%d', [LowerCase(TargetConfig.Target.TargetType), TargetConfig.Target.Version]),
      Result, ps);
  end;
end;

procedure DeleteFiles(Files: TStrings);
var
  I: Integer;
begin
  for I := 0 to Files.Count - 1 do
    DeleteFile(Files[I]);
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

procedure TCompiler.LogModify(const Name: string; const FilenameA: string; AgeA: Integer;
  const FilenameB: string; AgeB: Integer);
begin
{  if AgeA = -1 then
    CaptureLine(Format('%s: %s (%s) > %s (%s)',
      [Name,
       FilenameA, '-1',
       FilenameB, DateTimeToStr(FileDateToDateTime(AgeB))]), FAborted)
  else if AgeB = -1 then
    CaptureLine(Format('%s: %s (%s) > %s (%s)',
      [Name,
       FilenameA, DateTimeToStr(FileDateToDateTime(AgeA)),
       FilenameB, '-1']), FAborted)
  else
    CaptureLine(Format('%s: %s (%s) > %s (%s)',
      [Name,
       FilenameA, DateTimeToStr(FileDateToDateTime(AgeA)),
       FilenameB, DateTimeToStr(FileDateToDateTime(AgeB))]), FAborted);}
end;

procedure TCompiler.CaptureLine(const Line: string; var Aborted: Boolean);
begin
  FOutput.Add(Line);
  if Assigned(FOnCaptureLine) then
    FOnCaptureLine(Line, FAborted);
  //Aborted := FAborted;   We abort when the compiler has finished its work.
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
  //Aborted := FAborted;   We abort when the compiler has finished its work.
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
(*function TCompiler.Make(TargetConfig: ITargetConfig; Args: string;
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
end;*)

/// <summary>
/// WriteDccCfg() writes the dcc32.cfg or dcc64.cfg file to the directory
/// </summary>
function TCompiler.WriteDccCfg(const Directory: string; TargetConfig: ITargetConfig;
  const DccOpt: string; DebugUnits: Boolean): string;
var
  Lines: TStrings;
  SearchPaths, S: string;
  I: Integer;
  OutDirs: TOutputDirs;
  Target: TCompileTarget;
  BDSLibDir: string;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  Lines := TStringList.Create;
  try
    // opts
    Lines.Add(DccOpt);

    // default paths

    {if TargetConfig.DebugUnits then
      BDSLibDir := TargetConfig.Target.RootLibDebugDir
    else}
      BDSLibDir := TargetConfig.Target.RootLibReleaseDir;

    if not DirectoryExists(BDSLibDir) then
      raise Exception.CreateFmt('The unit directory "%s" does not exist.'#10'Please contact the JVCL team about this.', [BDSLibDir]);

    SearchPaths := TargetConfig.Target.ExpandDirMacros(
      BDSLibDir + ';' +
      BDSLibDir + PathDelim + 'obj;' +
      TargetConfig.JclDcpDir + ';' +
      TargetConfig.JclDcuDir + ';' +
      OutDirs.DcpDir + ';' +
      OutDirs.UnitOutDir
    );
    SearchPaths := RemoveInvalidPaths(SearchPaths);
    Lines.Add('-U"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Common' + '"');
    Lines.Add('-I"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Common' + '"');
    Lines.Add('-R"' + SearchPaths + ';' + TargetConfig.JVCLDir + PathDelim + 'Resources' + '"');
    Lines.Add('-O"' + SearchPaths + '"');

    // search paths
    SearchPaths := '';
    Target := TargetConfig.Target;
    for i := 0 to Target.SearchPaths.Count - 1 do
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

    // output directories
    Lines.Add('-LE"' + OutDirs.BplDir + '"'); // .exe output
    Lines.Add('-LN"' + OutDirs.DcpDir + '"'); // .dcp output
    Lines.Add('-N0"' + OutDirs.UnitOutDir + '"'); // .dcu output
    Lines.Add('-N1"' + OutDirs.HppDir + '"'); // .hpp output
    if TargetConfig.Target.IsBDS then
      Lines.Add('-NH"' + OutDirs.HppDir + '"'); // .hpp output
    Lines.Add('-N2"' + OutDirs.UnitOutDir + '"'); // .obj output
    if TargetConfig.Target.IsBDS then
      Lines.Add('-NO"' + OutDirs.UnitOutDir + '"'); // .obj output
    Lines.Add('-NB"' + OutDirs.DcpDir + '"'); // .bpi output

    { dcc32.exe crashes if the path is too long }
    if IsDcc32BugDangerous(TargetConfig) then
      Lines.Add('-Q');
    if TargetConfig.Target.IsPersonal then
      Lines.Add('-DDelphiPersonalEdition');
    if TargetConfig.Target.IsBDS and (TargetConfig.Target.Version >= 16) then
      Lines.Add('-nsSystem;System.Win;WinAPI;Vcl;Vcl.Imaging;Data;Data.Win;BDE');

    if TargetConfig.Target.Platform = ctpWin64 then
      Result := Directory + '\dcc64.cfg'
    else
      Result := Directory + '\dcc32.cfg';
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

/// <summary>
/// Dcc() compiles a Delphi.Win32 or Win64 package. If the command could not be
/// executed a message dialog is shown with the complete command line. Returns
/// the ExitCode of the last/failed command.
/// </summary>
function TCompiler.Dcc(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const DccOpt: string; DebugUnits: Boolean; Files, ObjFiles: TStrings): Integer;
const
  MaxCmdLineLength = 2048 - 1;
var
  DccCfg, PrjFilename, DccBinary: string;
  BplFilename, BplBakFilename, Filename, Args, CmdLine, S, PathEnvVar: string;
  OutDirs: TOutputDirs;
  ExistingBplRenamed: Boolean;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);
  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  if Files.Count > 0 then
    DccCfg := WriteDccCfg(ExtractFileDir(PrjFilename), TargetConfig, DccOpt, DebugUnits);

  PathEnvVar := TargetConfig.GetPathEnvVar;
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
      if TargetConfig.Target.Platform = ctpWin64 then
        DccBinary := TargetConfig.Target.Dcc64
      else
        DccBinary := TargetConfig.Target.Dcc32;
      CmdLine := '"' + DccBinary + '"' + S;
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

      { Get the target file out of the way, in case it is still used by the IDE or
        an application. }
      BplFilename := OutDirs.BplDir + PathDelim + Project.TargetName;
      BplBakFilename := BplFilename + '.bak';
      ExistingBplRenamed := False;
      if FileExists(BplFilename) then
      begin
        if SetFileAttributes(PChar(BplBakFilename), FILE_ATTRIBUTE_NORMAL) then
          DeleteFile(BplBakFilename);
        ExistingBplRenamed := RenameFile(BplFilename, BplBakFilename);
      end;
      Result := -1;
      try
        { Compile the project }
        if TargetConfig.Target.Version <= 9 then
          Result := CaptureExecute('"' + DccBinary + '"', Args,
                                   ExtractFileDir(PrjFilename), CaptureLinePackageCompilation, DoIdle,
                                   False, PathEnvVar, Dcc32SpeedInjection)
        else
          Result := CaptureExecute('"' + DccBinary + '"', Args,
                                   ExtractFileDir(PrjFilename), CaptureLinePackageCompilation, DoIdle,
                                   False, PathEnvVar, nil);
      finally
        { Restore original file if there was an error or an exception }
        if ExistingBplRenamed then
        begin
          if Result <> 0 then
            RenameFile(BplBakFilename, BplFilename);
          if FileExists(BplBakFilename) then
          begin
            SetFileAttributes(PChar(BplBakFilename), FILE_ATTRIBUTE_NORMAL);
            if not DeleteFile(BplBakFilename) then
              MoveFileEx(PChar(BplBakFilename), nil, MOVEFILE_DELAY_UNTIL_REBOOT);
          end;
        end;
      end;
      if Result <> 0 then
        Break;
    end;
  finally
    if not CmdOptions.KeepFiles then
      DeleteFile(DccCfg);
  end;

  if Result < 0 then // command not found
    MessageBox(0, PChar(Format(RsCommandNotFound, [CmdLine, ExtractFileDir(PrjFilename)])),
               'JVCL Installer', MB_OK or MB_ICONERROR);
end;

/// <summary>
/// Bcc32() compiles C++ files. It adds the compiled .obj file names to ObjFiles
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

  CsmAge := FileAgeEx(Format('%s\lib\vcl%d0.csm', [TargetConfig.Target.RootDir, TargetConfig.Target.Version]));

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
      ObjAge := FileAgeEx(ObjFilename);
      if Assigned(ObjFiles) then
        ObjFiles.AddObject(ObjFilename, TObject(ObjAge));
      if not TargetConfig.AutoDependencies or
         (ObjAge < CsmAge) or (ObjAge < FileAgeEx(ExtractFilePath(RspFilename) + Files[i])) then
      begin
        Lines.Add('"' + Files[i] + '"');
        NothingToDo := False;
      end;
    end;
    if NothingToDo then
      Exit;
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
  DebugUnits: Boolean; var FilesCompiled: Boolean): Integer;
var
  PrjFilename, PkgFilename, ResFilename: string;
  PasFiles, CppFiles, ObjFiles, LibFiles, ResFiles: TStrings;
  i, ObjAge, NewestObjAge, AgeIndex: Integer;
  BplFilename, DcpFilename, BpiFilename, ObjFilename: string;
  {BplAge, }DcpAge, {LibAge,} BpiAge: Integer;
  Changed: Boolean;
  OutDirs: TOutputDirs;
  FilesPackedTogether: Boolean;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  BplFilename := OutDirs.BplDir + PathDelim + Project.TargetName;
  DcpFilename := OutDirs.DcpDir + PathDelim + Project.DcpName;
  BpiFilename := ChangeFileExt(DcpFilename, '.bpi');
  {BplAge := FileAgeEx(BplFilename);}
  DcpAge := FileAgeEx(DcpFilename);
  {LibAge := FileAgeEx(ChangeFileExt(DcpFilename, '.lib'));}
  BpiAge := FileAgeEx(BpiFilename);

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
    for i := 0 to Project.RequireCount - 1 do
    begin
      if IsPackageUsed(Project.Owner, Project.Requires[i]) then
      begin
        // obtain DCP filename
        { TODO : Change this if the .dcp filename convention changes }
        PkgFilename := ChangeFileExt(Project.Requires[i].GetBplName(Project.Owner), '');
        ObjFiles.Add(PkgFilename + '.bpi');
      end;
    end;
    ObjFiles.Add('Memmgr.lib');
    ObjFiles.Add('sysinit.obj');

    LibFiles.Add('import32.lib');
    LibFiles.Add('cp32mti.lib');

    // add additional .lib files
    if TargetConfig.Target.Version = 6 then
      LibFiles.AddStrings(Project.Info.XmlInfo.C6Libs);

    AgeIndex := ObjFiles.Count;
    // add .pas.obj files
    NewestObjAge := 0;
    for i := 0 to PasFiles.Count - 1 do
    begin
      ObjFilename := OutDirs.UnitOutDir + PathDelim + ChangeFileExt(ExtractFileName(PasFiles[i]), '.obj');
      if TargetConfig.AutoDependencies then
        ObjAge := FileAgeEx(ObjFilename)
      else
        ObjAge := -1;
      ObjFiles.AddObject(ObjFilename, TObject(ObjAge));
      if ObjAge > NewestObjAge then
        NewestObjAge := ObjAge;
    end;

    // compile Delphi files (creates only .dcu, .obj and .hpp files)
    Result := CompileDelphiPackage(TargetConfig, Project, DccOpt + ' -JPHNE --BCB', DebugUnits, FilesCompiled);
    if Result <> 0 then
      Exit;

    FilesPackedTogether := False;
    Changed := not TargetConfig.AutoDependencies or
               HaveFilesChanged(ObjFiles, AgeIndex) or
               (DcpAge < NewestObjAge) or
               (BpiAge <> FileAgeEx(BpiFilename));
    if Changed then
    begin
      // compile Delphi package (only modified) to get .dcp file (.bpl is also created)
      // Note: in order to be REALLY safe, we should also pass --BCB here to get the
      // compiler in BCB mode and ensure that should a unit be recompiled, it is in the
      // same state as it was when the dcu/obj/hpp files were created. However, this
      // does not work with BCB5, as it won't generate the dcp file with --BCB. Defining
      // BCB via -DBCB is also not working here. So we have to "hope" the compiler
      // is intelligent enough to see that it just needs to pack the dcu files into
      // the final dcp file.
      Result := CompileDelphiPackage(TargetConfig, Project,
                     StringReplace(' ' + DccOpt + ' ', ' -B ', '', [rfReplaceAll]),
                     DebugUnits, FilesPackedTogether);
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
               FilesCompiled or
               FilesPackedTogether;
               {(BplAge < NewestObjAge) or (LibAge < BpiAge) or (BpiAge < NewestObjAge)};
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
/// CompileDelphiPackage() compiles a Delphi package. If one of the commands
/// could not be executed a message dialog is shown with the complete command
/// line of the failed command. Returns the ExitCode of the last/failed command.
/// </summary>
function TCompiler.CompileDelphiPackage(TargetConfig: ITargetConfig; Project: TPackageTarget;
  const DccOpt: string; DebugUnits: Boolean; var FilesCompiled: Boolean): Integer;
var
  Files: TStrings;
  i: Integer;
  DcpAge, FormAge, PasAge, DcuAge, BplAge, DepAge: Integer;
  PrjFilename, DcpFilename, BplFilename, ResFilename: string;
  Filename, DcuFilename: string;
  Changed: Boolean;
  OutDirs: TOutputDirs;
begin
  OutDirs := TargetConfig.GetOutputDirs(DebugUnits);

  Result := 0;
  PrjFilename := Project.SourceDir + PathDelim + ExtractFileName(Project.SourceName);
  DcpFilename := OutDirs.DcpDir + PathDelim + Project.DcpName;
  BplFilename := OutDirs.BplDir + PathDelim + Project.TargetName;
  ResFilename := ChangeFileExt(PrjFilename, '.res');
  DcpAge := FileAgeEx(DcpFilename);
  BplAge := FileAgeEx(BplFilename);

  // .dpk/.bpk
  Changed := not TargetConfig.AutoDependencies or
             (DcpAge < FileAgeEx(PrjFilename)) or
             (DcpAge < FileAgeEx(ResFilename)){ or
             (DcpAge > BplAge)}; // this happens if the .dcp is not for the .bpl [BDS 2006 makes this non-function]
  if Changed then
  begin
    if DcpAge < FileAgeEx(PrjFilename) then
      LogModify('Project', PrjFilename, FileAgeEx(PrjFilename), DcpFilename, DcpAge);
    if DcpAge < FileAgeEx(ResFilename) then
      LogModify('Resource', ResFilename, FileAgeEx(ResFilename), DcpFilename, DcpAge);
    {if DcpAge > BplAge then
      LogModify('Bpl', BplFilename, BplAge, DcpFilename, DcpAge);}
  end;

  if DcpAge > BplAge then
    DcpAge := BplAge; // compile units that are newer than the bpl if the bpl is older than the dcp
  if not Changed then
  begin
    for i := 0 to High(CommonDependencyFiles) do
    begin
      Filename := ReplaceTargetMacros(CommonDependencyFiles[i], TargetConfig);
      DepAge := FileAgeEx(Filename);
      if (DcpAge < DepAge) or (DepAge = -1) then
      begin
        Changed := True;
        LogModify('Common', Filename, DepAge, DcpFilename, DcpAge);
        Break;
      end;
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
        DepAge := FileAgeEx(Filename);
        if (DepAge > DcpAge) or (DepAge = -1) then
        begin
          Changed := True;
          LogModify('JVCL', Filename, DepAge, DcpFilename, DcpAge);
          Break;
        end;
      end;
    end;
  end;
  if not Changed then
  begin
    // required JCL package
    for i := 0 to Project.JclDependencies.Count - 1 do
    begin
      if IsPackageUsed(Project.Owner, Project.JclDependenciesReqPkg[i]) then
      begin
        Filename := TargetConfig.JclDcpDir + PathDelim +
                    TargetConfig.VersionedJclDcp(Project.JclDependenciesReqPkg[i].Name + '.dcp');
        DepAge := FileAgeEx(Filename);
        if (DepAge > DcpAge) or (DepAge = -1) then
        begin
          Changed := True;
          LogModify('JCL', Filename, DepAge, DcpFilename, DcpAge);
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
        Filename := Project.SourceDir + PathDelim + Project.Contains[i].Name;
        DcuFilename := OutDirs.UnitOutDir + PathDelim + ChangeFileExt(ExtractFileName(Project.Contains[i].Name), '.dcu');
        PasAge := FileAgeEx(Filename);
        DcuAge := FileAgeEx(DcuFilename);
        if (PasAge > DcuAge) or (PasAge = -1) then
        begin
          Changed := True;
          LogModify('Source', Filename, PasAge, DcuFilename, DcuAge);
          Break;
        end;
        if (DcuAge > DcpAge) or (DcuAge = -1) then
        begin
          Changed := True;
          LogModify('Unit', DcuFilename, DcuAge, DcpFilename, DcpAge);
          Break;
        end;

        // .dfm/.xfm
        if Project.Contains[i].FormName <> '' then
        begin
          Filename := ChangeFileExt(Filename, '.dfm');
          FormAge := FileAgeEx(Filename);
          if FormAge = -1 then
          begin
            Filename := ChangeFileExt(Filename, '.xfm');
            FormAge := FileAgeEx(Filename);
          end;
          if (FormAge <> -1) and (FormAge > DcpAge) then
          begin
            Changed := True;
            LogModify('Form', Filename, FormAge, DcpFilename, DcpAge);
            Break;
          end;
        end;
      end;
    end;
  end;

  FilesCompiled := False;
  if Changed then
  begin
    Files := TStringList.Create;
    try
      Files.Add(ExtractFileName(ChangeFileExt(Project.SourceName, '.dpk'))); // force .dpk
      Result := Dcc(TargetConfig, Project, DccOpt, DebugUnits, Files, nil);
      if Result = 0 then
        FilesCompiled := True;
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
    if not Data.PackageGenerator.LoadConfig(Data.JVCLDir + '\' + sPackageGeneratorFile, Group, ErrMsg) then
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
    Data.PackageGenerator.ExpandTargetsNoPerso(TargetList);
    EnumeratePackages(PackagesPath, List);
    if not Data.PackageGenerator.Generate(List, TargetList, WriteMsg, Data.JVCLDir + '\' + sPackageGeneratorFile,
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
/// Compile() prepares for compiling and decides if the VCL framework
/// should be compiled.
/// </summary>
function TCompiler.Compile: Boolean;
var
  i, Index: Integer;
  Frameworks, Count: Integer;
  TargetConfigs: array of TTargetConfig;
  InstallAttempted: array of Boolean;
  SysInfo: string;
  XML: TJclSimpleXML;
  AConfig: TTargetConfig;
  AConfigElem: TJclSimpleXMLElem;
  CompiledConfigIndex: Integer;
  LogContent: TStringList;
begin
  Result := True;
  FAborted := False;
  try
    SysInfo := GetWindowsProductString;

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
    Frameworks := 0;
    Count := 0;
    SetLength(TargetConfigs, Data.Targets.Count);
    for i := 0 to High(TargetConfigs) do
    begin
      if Data.TargetConfig[i].InstallJVCL then
      begin
        TargetConfigs[Count] := Data.TargetConfig[i];
        Inc(Count);
        if pkVCL in Data.TargetConfig[i].InstallMode then
          Inc(Frameworks);
      end;
    end;
    SetLength(TargetConfigs, Count);
    SetLength(InstallAttempted, Count);

    // Delete all units from the JVCL source directory where they shouldn't be
    ClearSourceDirectory;

    // Remove previous log files if asked to
    for I := 0 to Data.Targets.Count - 1 do
      if CmdOptions.DeletePreviousLogFiles then
        SysUtils.DeleteFile(Data.TargetConfig[I].LogFileName);

    // compile all selected targets
    Index := 0;
    for i := 0 to Count - 1 do
    begin
      InstallAttempted[i] := False;
      DoTargetProgress(TargetConfigs[i], Index, Frameworks);
      if pkVCL in TargetConfigs[i].InstallMode then
      begin
        if not FAborted then
          InstallAttempted[i] := True;
        Result := CompileTarget(TargetConfigs[i], pkVCL);
        if not Result and not CmdOptions.ContinueOnError then
          Break;
        Inc(Index);
      end;
      DoTargetProgress(TargetConfigs[i], Index, Frameworks);
    end;
  finally
    if CmdOptions.XMLResultFileName <> '' then
    begin
      XML := TJclSimpleXML.Create;
      try
        XML.Options := [sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
        XML.Root.Name := 'JvclInstall';
        CompiledConfigIndex := 0;
        for I := 0 to Data.Targets.Count - 1 do
        begin
          AConfig := Data.TargetConfig[I];
          AConfigElem := XML.Root.Items.Add('Installation');

          AConfigElem.Properties.Add('Target', AConfig.MainTargetSymbol);

          if AConfig.Target.IsBDS and (AConfig.Target.IDEVersion >= 9) then
            AConfigElem.Properties.Add('TargetName', Format('%s %s %s', [AConfig.Target.Name, AConfig.Target.VersionStr, AConfig.Target.PlatformName])) // do not localize
          else
            AConfigElem.Properties.Add('TargetName', Format('%s %s', [AConfig.Target.Name, AConfig.Target.VersionStr])); // do not localize

          AConfigElem.Properties.Add('Enabled', pkVCL in AConfig.InstallMode);
          AConfigElem.Properties.Add('InstallAttempted', InstallAttempted[CompiledConfigIndex]);
          AConfigElem.Properties.Add('InstallSuccess', AConfig.InstallSuccess);
          AConfigElem.Properties.Add('LogFileName', Iff(FileExists(AConfig.LogFileName), AConfig.LogFileName, ''));
          if CmdOptions.IncludeLogFilesInXML and FileExists(AConfig.LogFileName) then
          begin
            LogContent := TStringList.Create;
            try
              LogContent.LoadFromFile(AConfig.LogFileName{$IFDEF UNICODE}, TEncoding.UTF8{$ENDIF UNICODE});
              AConfigElem.Items.Add('LogFile').Items.AddCData('', {$IFNDEF UNICODE}UTF8Decode{$ENDIF UNICODE}(LogContent.Text));
            finally
              LogContent.Free;
            end;
          end;

          if AConfig = TargetConfigs[CompiledConfigIndex] then
            Inc(CompiledConfigIndex);
        end;
        XML.SaveToFile(CmdOptions.XMLResultFileName, JclStreams.seUTF8);
      finally
        XML.Free;
      end;
    end;
  end;
end;

/// <summary>
/// DeleteRemovedFiles deletes the removed packages and units from the output directories.
/// </summary>
procedure TCompiler.DeleteRemovedFiles(TargetConfig: TTargetConfig);

  procedure CollectExistingOutputFiles(Files: TStrings; const Dirs: array of string);
  const
    OutputFileExts: array[0..11] of string = (
      '.dcu', '.dfm', '.hpp', '.obj', '.dcp', '.lib', '.bpi', '.bpl', '.jdbg', '.map', '.tds', '.lsp'
    );
  var
    ProcessedDirs: TStrings;
    I: Integer;
  begin
    ProcessedDirs := TStringList.Create;
    try
      for I := 0 to High(Dirs) do
      begin
        if ProcessedDirs.IndexOf(Dirs[I]) = -1 then
        begin
          ProcessedDirs.Add(Dirs[I]);
          FindFiles(Dirs[I], 'Jv*.*', False, Files, OutputFileExts); // do not remove the leading "Jv" here otherwise the directories will be empty
        end;
      end;
    finally
      ProcessedDirs.Free;
    end;
  end;

  function GetHash(const Filename: string): SizeInt;
  var
    I: Integer;
  begin
    Result := Length(Filename);
    for I := 1 to Length(Filename) do
      Result := Result + Ord(Filename[I]);
  end;

var
  UsedFiles: TStringList;

  procedure UsedFilesAdd(const Filename: string);
  begin
    UsedFiles.AddObject(Filename, Pointer(GetHash(AnsiLowerCase(ExtractFileName(Filename)))));
  end;

var
  PackageIndex: Integer;
  Filename: string;
  Files: TStrings;
  OutputDirs, DebugOutputDirs: TOutputDirs;
  Group: TProjectGroup;
  Package: TPackageTarget;
  FileIndex: Integer;
  Found: Boolean;
  I: Integer;
  HashValue: Pointer;
  HasDebugUnits: Boolean;
begin
  // Delete the files that aren't part of the installation
  UsedFiles := TStringList.Create;
  Files := TStringList.Create;
  try
    HasDebugUnits := not TargetConfig.DeveloperInstall and TargetConfig.DebugUnits;
    OutputDirs := TargetConfig.GetOutputDirs(False);
    DebugOutputDirs := TargetConfig.GetOutputDirs(True);

    Group := nil;
    if pkVCL in TargetConfig.InstallMode then
      Group := TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL];

    if Group <> nil then
    begin
      // Get all destination file names that are part of this distribution for the target
      for PackageIndex := 0 to Group.Count - 1 do
      begin
        Package := Group.Packages[PackageIndex];
        if Package.Compile then
        begin
          { DCP }
          FileName := ExtractFileName(Package.DcpName);

          UsedFilesAdd(OutputDirs.DcpDir + PathDelim + Filename);
          UsedFilesAdd(OutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.lib'));
          UsedFilesAdd(OutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.bpi'));
          UsedFilesAdd(OutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.lsp'));
          UsedFilesAdd(OutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dcu'));
          UsedFilesAdd(OutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.obj'));
          UsedFilesAdd(OutputDirs.HppDir + PathDelim + ChangeFileExt(Filename, '.hpp'));

          if HasDebugUnits then
          begin
            UsedFilesAdd(DebugOutputDirs.DcpDir + PathDelim + Filename);
            UsedFilesAdd(DebugOutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.lib'));
            UsedFilesAdd(DebugOutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.lsp'));
            UsedFilesAdd(DebugOutputDirs.DcpDir + PathDelim + ChangeFileExt(Filename, '.bpi'));
            UsedFilesAdd(DebugOutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dcu'));
            UsedFilesAdd(DebugOutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.obj'));
            UsedFilesAdd(DebugOutputDirs.HppDir + PathDelim + ChangeFileExt(Filename, '.hpp'));
          end;

          { BPL }
          Filename := ExtractFileName(Package.TargetName);

          UsedFilesAdd(OutputDirs.BplDir + PathDelim + Filename);
          UsedFilesAdd(OutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.map'));
          UsedFilesAdd(OutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.jdbg'));
          UsedFilesAdd(OutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.tds'));

          if HasDebugUnits then
          begin
            UsedFilesAdd(DebugOutputDirs.BplDir + PathDelim + Filename);
            UsedFilesAdd(DebugOutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.map'));
            UsedFilesAdd(DebugOutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.jdbg'));
            UsedFilesAdd(DebugOutputDirs.BplDir + PathDelim + ChangeFileExt(Filename, '.tds'));
          end;

          { Contained units }
          for FileIndex := 0 to Package.ContainCount - 1 do
          begin
            if IsFileUsed(Group, Package.Contains[FileIndex]) then
            begin
              { PAS }
              Filename := ExtractFileName(Package.Contains[FileIndex].Name);

              if Package.Contains[FileIndex].FormName <> '' then
                UsedFilesAdd(OutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dfm'));
              UsedFilesAdd(OutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dcu'));
              UsedFilesAdd(OutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.obj'));
              UsedFilesAdd(OutputDirs.HppDir + PathDelim + ChangeFileExt(Filename, '.hpp'));

              if HasDebugUnits then
              begin
                if Package.Contains[FileIndex].FormName <> '' then
                  UsedFilesAdd(DebugOutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dfm'));
                UsedFilesAdd(DebugOutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.dcu'));
                UsedFilesAdd(DebugOutputDirs.UnitOutDir + PathDelim + ChangeFileExt(Filename, '.obj'));
                UsedFilesAdd(DebugOutputDirs.HppDir + PathDelim + ChangeFileExt(Filename, '.hpp'));
              end;
            end;
          end;

        end;
      end;

      { Collect all existing files that are in the output directories }
      CollectExistingOutputFiles(Files,
        [OutputDirs.UnitOutDir, OutputDirs.BplDir, OutputDirs.DcpDir, OutputDirs.HppDir,
         DebugOutputDirs.UnitOutDir, DebugOutputDirs.BplDir, DebugOutputDirs.DcpDir, DebugOutputDirs.HppDir]);


      { Delete all files that aren't created by the compiler/installer }
      for FileIndex := 0 to Files.Count - 1 do
      begin
        Filename := Files[FileIndex];

        // Using the "hashed-compare" is faster than using binary search because many
        // filenames start with the same sub-string (aka directory name). So IndexOf
        // has to compare a lot of matching characters until it reaches the filename.
        HashValue := Pointer(GetHash(AnsiLowerCase(ExtractFileName(Filename))));
        Found := False;
        for I := 0 to UsedFiles.Count - 1 do
        begin
          if (UsedFiles.Objects[I] = HashValue) and (AnsiCompareText(Filename, UsedFiles[I]) = 0) then
          begin
            Found := True;
            Break;
          end;
        end;

        if not Found then
          DeleteFile(Filename);
      end;
    end;

    // remove non suffixed hppdir content as there was a time when the JVCL did not create the win32/win64 subdirs for HPP files
    if TargetConfig.Target.IDEVersion >= 11 then
    begin
      Files.Clear;
      BuildFileList(StrEnsureNoSuffix(LowerCase(TargetConfig.Target.PlatformName), OutputDirs.HppDir) + '*.hpp', faAnyFile, Files, True);
      for FileIndex := 0 to Files.Count - 1 do
        DeleteFile(Files[FileIndex]);
    end;
  finally
    Files.Free;
    UsedFiles.Free;
  end;
end;

/// <summary>
/// ClearSourceDirectory deletes all binary unit files from the source and
/// include directory to get a clean installation.
/// </summary>
procedure TCompiler.ClearSourceDirectory;
var
  Files: TStrings;
begin
  Files := TStringList.Create;
  try
    FindFiles(Data.JVCLSourceDir, '*.*', False, Files, ['.dcu', '.dcp', '.obj', '.bpi', '.lib']);
    FindFiles(Data.JVCLIncludeDir, '*.*', False, Files, ['.dcu', '.dcp', '.obj', '.bpi', '.lib']);
    DeleteFiles(Files);
  finally
    Files.Free;
  end;
end;


/// <summary>
/// CompileTarget starts CompileProjectGroup for all sub targets of the
/// given target IDE.
/// </summary>
function TCompiler.CompileTarget(TargetConfig: TTargetConfig; PackageGroupKind: TPackageGroupKind): Boolean;
var
  Aborted: Boolean;
begin
  Result := True;
  Aborted := False;
  FOutput.Clear;

  try
    try
      DeleteRemovedFiles(TargetConfig);

      // VCL
      if Result and (pkVCL in TargetConfig.InstallMode) then
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
        begin
          // compile
          Result := CompileProjectGroup(
            TargetConfig.Frameworks.Items[TargetConfig.Target.IsPersonal, pkVCL], False);
        end;

        if Result then
          CaptureLine('[Finished JVCL for VCL installation]', Aborted); // do not localize
      end;
    finally
      TargetConfig._SetBuildSuccess(Result);
    end;
  except
    on E: Exception do
    begin
      if Assigned(ApplicationHandleException) then
        ApplicationHandleException(nil);
      AbortReason := E.Message;
      Result := False;
    end;
  end;
end;

/// <summary>
/// GenerateResources starts the make file for the resource file compilation.
/// </summary>
(*
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
*)

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

    Dir := ProjectGroup.TargetConfig.JVCLDir + PathDelim + 'run';
    FindFiles(Dir, '*.dfm', False, Files, ['.dfm']);

    CaptureLine(RsCopyingFiles, FAborted);
    for i := 0 to Files.Count - 1 do
    begin
      DestFile := DestDir + PathDelim + ExtractFileName(Files[i]);
      if FileAgeEx(Files[i]) > FileAgeEx(DestFile) then
      begin
{**}    DoProgress(ExtractFileName(Files[i]), i, Files.Count, pkOther);
        CopyFile(PChar(Files[i]), PChar(DestFile), False);
      end;
      if DebugUnits then
      begin
        DestFile := DebugDestDir + PathDelim + ExtractFileName(Files[i]);
        if FileAgeEx(Files[i]) > FileAgeEx(DestFile) then
          CopyFile(PChar(Files[i]), PChar(DestFile), False);
      end;
    end;
{**}DoProgress('', 0, Files.Count, pkOther);
  finally
    Files.Free;
  end;
end;

procedure TCompiler.SortProject(Group: TProjectGroup; List: TList; Project: TPackageTarget; var ProjectIndex: Integer);
var
  ReqProjectIndex: Integer;
  ReqPackage: TRequiredPackage;
  ReqProject: TPackageTarget;
  ListIndex: Integer;
begin
  for ReqProjectIndex := 0 to Project.RequireCount - 1 do
  begin
    ReqPackage := Project.Requires[ReqProjectIndex];
    if IsPackageUsed(Group, ReqPackage) then
    begin
      // Two cases: the required project is not in the list so we need to add,
      // or it's already there and not in front so we need to move it.
      // In both cases, we need to sort the moved/added package recursively
      // and increment the ProjectIndex only after the recursive sort is done
      // so that packages required by the current required package are put
      // in front of it as well.
      ReqProject := Group.FindPackageByXmlName(ReqPackage.Name);
      if Assigned(ReqProject) then
      begin
        ListIndex := List.IndexOf(ReqProject);
        if ListIndex = -1 then
        begin
          List.Insert(ProjectIndex, ReqProject);
          SortProject(Group, List, ReqProject, ProjectIndex);
          Inc(ProjectIndex);
        end
        else
        if ListIndex > ProjectIndex then
        begin
          List.Move(ListIndex, ProjectIndex);
          SortProject(Group, List, ReqProject, ProjectIndex);
          Inc(ProjectIndex);
        end;
      end;
    end;
  end;
end;

procedure TCompiler.SortProjectGroup(Group: TProjectGroup; List: TList);
var
  CurProject: TPackageTarget;
  CurProjectIndex: Integer;
begin
  // Add all projects to be compiled into list.
  for CurProjectIndex := 0 to Group.Count - 1 do
    if Group[CurProjectIndex].Compile then
      List.Add(Group[CurProjectIndex]);

  // Sort according to dependency list:
  // For each package that must be compiled, put those it requires in front
  // of it, and this recursively.
  CurProjectIndex := 0;
  while CurProjectIndex < List.Count do
  begin
    CurProject := List[CurProjectIndex];

    SortProject(Group, List, CurProject, CurProjectIndex);

    Inc(CurProjectIndex);
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
  JVCLPackagesDir: string;
  Files: TStrings;

  ProjectOrder: TList;
  Project: TPackageTarget;
  DebugProgress: string;
  FilesCompiled: Boolean;
  
  function GetProjectIndex: Integer;
  begin
    Result := AProjectIndex;
    Inc(AProjectIndex);
  end;

begin
  Assert(ProjectGroup <> nil, 'The project group file wasn''t found');

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
    if not TargetConfig.CompileOnly then
      TargetConfig.DeinstallJVCL(nil, nil, {RealUninstall:=}False);

    // obtain information for progress bar
    FPkgCount := 0;
    for i := 0 to ProjectGroup.Count - 1 do
      if ProjectGroup.Packages[i].Compile then
        Inc(FPkgCount);
    FPkgIndex := 0;

    JVCLPackagesDir := TargetConfig.JVCLPackagesDir;

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

    ForceDirectoriesEx(TargetConfig.UnitOutDir);
    ForceDirectoriesEx(TargetConfig.BplDir);
    ForceDirectoriesEx(TargetConfig.DcpDir);
    //ForceDirectoriesEx(TargetConfig.HppDir);
    if DebugUnits then
    begin
      ForceDirectoriesEx(TargetConfig.DebugUnitOutDir);
      ForceDirectoriesEx(TargetConfig.DebugBplDir);
      ForceDirectoriesEx(TargetConfig.DebugDcpDir);
      ForceDirectoriesEx(TargetConfig.DebugHppDir);
    end;

    { Create include directory if necessary }
    if TargetConfig.Target.SupportsPersonalities([persBCB]) then
      ForceDirectoriesEx(TargetConfig.Target.ExpandDirMacros(TargetConfig.HppDir));

   // *****************************************************************

{**}DoProjectProgress(RsGeneratingPackages + DebugProgress, GetProjectIndex, ProjectMaxProgress);
    // generate the packages and .cfg files
    if not GeneratePackages('JVCL', {CutPersEdition(TargetSymbol)} TargetConfig.MainTargetSymbol,
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
      { .bpl and .dcp files may be in the wrong directory. So delete them. }
      TargetConfig.GetPackageBinariesForDeletion(Files);
      for i := 0 to Files.Count - 1 do
      begin
        if not StartsWith(Files[i], TargetConfig.BplDir + PathDelim, True) and
           not StartsWith(Files[i], TargetConfig.DcpDir + PathDelim, True) then
        begin
          if TargetConfig.DebugUnits then
          begin
            if StartsWith(Files[i], TargetConfig.DebugBplDir + PathDelim, True) and
               StartsWith(Files[i], TargetConfig.DebugDcpDir + PathDelim, True) then
              Continue;
          end;
          DeleteFile(Files[i]);
        end;
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

        FilesCompiled := False;
        if TargetConfig.Target.IsBCB and not TargetConfig.Target.IsBDS then
        begin
          if CompileCppPackage(TargetConfig, Project, DccOpt, '', '', DebugUnits, FilesCompiled) <> 0 then
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
          if CompileDelphiPackage(TargetConfig, Project, DccOpt, DebugUnits, FilesCompiled) <> 0 then
          begin
            if FAborted then
              Exit;
            AbortReason := RsErrorCompilingPackages;
            Exit;
          end;
        end;
        {if FilesCompiled and (TargetConfig.Target.SupportedPersonalities * [persBCB] <> []) then
          AlterHppFiles(TargetConfig, Project);}

        if FAborted then
          Exit;
      end;
    finally
      ProjectOrder.Free;
    end;
    DoPackageProgress(nil, '', FPkgCount, FPkgCount);

   // *****************************************************************

{**}DoProjectProgress(RsPostCompilationOperations + DebugProgress, GetProjectIndex, ProjectMaxProgress);
    if TargetConfig.GenerateMapFiles and (TargetConfig.LinkMapFiles or TargetConfig.CreateJdbgFiles) then
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
      TargetConfig.AddPathsToIDE;
      if not TargetConfig.CompileOnly then
      begin
        if TargetConfig.CleanPalettes then
          TargetConfig.CleanJVCLPalette(False);
        TargetConfig.RegisterDesigntimePackages;
      end;
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
    if TargetConfig.LinkMapFiles and not TargetConfig.LinkMapFile(BplFileName, MapFileName,
      MapFileSize, JclDebugDataSize) then
    begin
      CaptureLine(Format('Error: Unable to link %s', [ExtractFileName(MapFileName)]), FAborted);
      AbortReason := RsErrorLinkingMapFiles;
      Exit;
    end;

    if TargetConfig.CreateJdbgFiles and not TargetConfig.CompressMapFileToJdbg(MapFileName) then
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

/// <summary>
/// AlterHppFiles corrects the *.hpp files that the dcc32.exe compiler has generated.
///  - Converts every '#define constant "string"' to "static const char
/// </summary>
(*procedure TCompiler.AlterHppFiles(TargetConfig: ITargetConfig; Project: TPackageTarget);

  function IsValidTypeName(const Name: string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 1 to Length(Name) do
      if not (Name[I] in ['A'..'Z', 'a'..'z', '0'..'9', ':', '_']) then
        Exit;
    Result := True;
  end;

  function AlterDefine(var f: TextFile; Lines: TStrings): Boolean;
  var
    S, Name, TypeName: string;
    ps: Integer;
    Value: Integer;
  begin
    Result := False;
    S := Trim(Copy(Lines[Lines.Count - 1], 9, MaxInt));
    ps := Pos(' ', S);
    if (ps > 2) and (ps < Length(S)) then
    begin
      { #define name "string"   =>   static const char* name = "string"; }
      if S[ps + 1] = '"' then
      begin
        Name := Copy(S, 1, ps - 1);
        Lines[Lines.Count - 1] := Format('static const char* %s = %s', [Name, Copy(S, ps + 1, MaxInt)]);
        while not Eof(f) and (S[Length(S)] = '\') do // line continuation
        begin
          ReadLn(f, S);
          Lines.Add(S);
        end;
        Lines[Lines.Count - 1] := Lines[Lines.Count - 1] + ';';
        Result := True;
      end
      else
      { #define name (type)(value)   =>   static const type name = (type)(value); }
      if S[ps + 1] = '(' then
      begin
        Name := Copy(S, 1, ps - 1);
        Delete(S, 1, ps + 1);
        ps := Pos(')', S);
        if ps > 2 then
        begin
          TypeName := Copy(S, 1, ps - 1);
          if IsValidTypeName(TypeName) then
          begin
            Delete(S, 1, ps);
            if (S <> '') and (S[1] = '(') and (S[Length(S)] = ')') and TryStrToInt(Copy(S, 2, Length(S) - 2), Value) then
            begin
              Lines[Lines.Count - 1] := Format('static const %s %s = (%s)(%d);', [TypeName, Name, TypeName, Value]);
              Result := True;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure AlterHppFile(const Filename: string);
  var
    Lines: TStrings;
    Modified: Boolean;
    S: string;
    f: TextFile;
  begin
    Lines := TStringList.Create;
    try
      AssignFile(f, Filename);
      Reset(f);
      try
        Modified := False;
        while not Eof(f) do
        begin
          ReadLn(f, S);
          Lines.Add(S);
          if StartsWith(S, '#define ', False) then
          begin
            if AlterDefine(f, Lines) then
              Modified := True;
          end;
        end;
      finally
        CloseFile(f);
      end;
      if Modified then
        Lines.SaveToFile(Filename);
    finally
      Lines.Free;
    end;
  end;

var
  I: Integer;
  HppDir, Filename: string;
begin
  HppDir := TargetConfig.HppDir;
  for I := 0 to Project.ContainCount - 1 do
  begin
    Filename := HppDir + PathDelim + ChangeFileExt(ExtractFileName(Project.Contains[I].Name), '.hpp');
    if FileExists(Filename) then
      AlterHppFile(Filename);
  end;
end;*)

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

function TCompiler.IsDcc32BugDangerous(TargetConfig: ITargetConfig): Boolean;
begin
  if TargetConfig.Target.Version <= 7 then
    Result := Length(Data.JVCLDir) > MaxDcc32PathLen
  else
    Result := False;
end;

end.
