unit ZipExec;

interface

uses
  Classes,
  JVCLHelpUtils, JvCreateProcess;

type
  TZipper = class
  private
    FTmpListName: string;
    FZipFileName: string;
    FZipExecFileName: string;
    FSourceFileNames: TStrings;
    FOnRead: TJvCPSReadEvent;
    function GetTmpListName: string;
    procedure SetSourceFileNames(Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function Execute: Boolean;
    property ZipFileName: string read FZipFileName write FZipFileName;
    property ZipExecFileName: string read FZipExecFileName write FZipExecFileName;
    property SourceFileNames: TStrings read FSourceFileNames write SetSourceFileNames;
    property TmpListName: string read GetTmpListName;
    property OnRead: TJvCPSReadEvent read FOnRead write FOnRead;
  end;

  TZipTask = class(TTask)
  private
    FZipper: TZipper;
    FOldMsg: string;
    procedure HandleRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property Zipper: TZipper read FZipper;
  end;

  THtmlHelpZipFileGenerator = class(TTask)
  private
    FDestDir: string;
    FHtmlHelpDir: string;
    FDoxFileName: string;
    FZipExecFileName: string;
    FOldMsg: string;
    function GetCHMFileName: string;
    function GetDestFileName: string;
    procedure HandleRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;

    property CHMFileName: string read GetCHMFileName;
    property DestFileName: string read GetDestFileName;
  public
    property ZipExecFileName: string read FZipExecFileName write FZipExecFileName;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property HtmlHelpDir: string read FHtmlHelpDir write FHtmlHelpDir;
    property DestDir: string read FDestDir write FDestDir;
  end;

  TWinHelpZipFileGenerator = class(TTask)
  private
    FDestDir: string;
    FWinHelpDir: string;
    FDoxFileName: string;
    FZipExecFileName: string;
    FOldMsg: string;
    function GetAlsFileName: string;
    function GetCntFileName: string;
    function GetHLPFileName: string;
    function GetDestFileName: string;
    procedure HandleRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;

    property AlsFileName: string read GetAlsFileName;
    property CntFileName: string read GetCntFileName;
    property HLPFileName: string read GetHLPFileName;
    property DestFileName: string read GetDestFileName;
  public
    property ZipExecFileName: string read FZipExecFileName write FZipExecFileName;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property WinHelpDir: string read FWinHelpDir write FWinHelpDir;
    property DestDir: string read FDestDir write FDestDir;
  end;

  TOnlineHelpZipFilesGenerator = class(TTask)
  private
    FOldMsg: string;
    FMaxFilesInZip: Integer;
    FDestDirectory: string;
    FSrcDirectory: string;
    FSrcExtension: string;
    FSrcDateAfter: TDateTime;

    FZipExecFileName: string;
    procedure HandleRead(Sender: TObject; const S: string;
      const StartsOnNewLine: Boolean);
  protected
    function CollectFiles(AFiles: TStrings): Boolean;
    function Zip(const ZipIndex: Integer; AFiles: TStrings): Boolean;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property SrcDirectory: string read FSrcDirectory write FSrcDirectory;
    property SrcExtension: string read FSrcExtension write FSrcExtension;
    property SrcDateAfter: TDateTime read FSrcDateAfter write FSrcDateAfter;
    property DestDirectory: string read FDestDirectory write FDestDirectory;
    property MaxFilesInZip: Integer read FMaxFilesInZip write FMaxFilesInZip;
    property ZipExecFileName: string read FZipExecFileName write FZipExecFileName;
  end;

implementation

uses
  Forms,
  Windows, SysUtils, JvSearchFiles, JclFileUtils;

{ TOnlineHelpZipFilesGenerator }

function TOnlineHelpZipFilesGenerator.CanStart: Boolean;
begin
  Result := CheckDir(SrcDirectory) and
    CheckDir(DestDirectory) and
    CheckFile(ZipExecFileName);
end;

function TOnlineHelpZipFilesGenerator.CollectFiles(AFiles: TStrings): Boolean;
begin
  StatusMsg('Collecting files');

  with TJvSearchFiles.Create(nil) do
  try
    DirOption := doExcludeSubDirs;
    RootDirectory := SrcDirectory;
    Options := [soSearchFiles];
    FileParams.FileMask := SrcExtension;

    FileParams.SearchTypes := [stFileMask];
    if SrcDateAfter > 0 then
    begin
      FileParams.LastChangeAfter := SrcDateAfter;
      FileParams.SearchTypes := FileParams.SearchTypes + [stLastChangeAfter];
    end;

    Result := Search;
    if Result then
      AFiles.Assign(Files);
  finally
    Free;
  end;
end;

function TOnlineHelpZipFilesGenerator.DoExecute: Boolean;
var
  AllFiles: TStringList;
  WorkFiles: TStringList;
  I: Integer;
  ZipCount: Integer;
begin
  AllFiles := TStringList.Create;
  try
    Result := CollectFiles(AllFiles);
    if not Result then
      Exit;

    StatusMsg('Generating zips');

    WorkFiles := TStringList.Create;
    try
      I := 0;
      ZipCount := 0;
      while I < AllFiles.Count do
      begin
        Progress(I, AllFiles.Count);
        WorkFiles.Add(AllFiles[i]);
        if WorkFiles.Count >= MaxFilesInZip then
        begin
          Result := Zip(ZipCount, WorkFiles);
          if not Result then
            Exit;

          Inc(ZipCount);
          WorkFiles.Clear;
        end;

        Inc(I);
      end;

      if WorkFiles.Count >= 0 then
      begin
        Result := Zip(ZipCount, WorkFiles);
        if not Result then
          Exit;
      end;
    finally
      WorkFiles.Free;
    end;
  finally
    AllFiles.Free;
  end;
end;

function TOnlineHelpZipFilesGenerator.GetTaskDescription: string;
begin
  Result := 'Generating zip files..';
end;

function TOnlineHelpZipFilesGenerator.Zip(const ZipIndex: Integer; AFiles: TStrings): Boolean;
var
  Zipper: TZipper;
begin
  Zipper := TZipper.Create;
  try
    Zipper.SourceFileNames := AFiles;
    Zipper.ZipFileName := IncludeTrailingPathDelimiter(DestDirectory) +
      Format('File%.3d.zip', [ZipIndex]);
    Zipper.ZipExecFileName := Self.ZipExecFileName;
    Zipper.OnRead := HandleRead;
    Result := Zipper.Execute;
  finally
    Zipper.Free;
  end;
end;

{ TZipper }

constructor TZipper.Create;
begin
  inherited Create;
  FSourceFileNames := TStringList.Create;
end;

destructor TZipper.Destroy;
begin
  FSourceFileNames.Free;
  inherited Destroy;
end;

function TZipper.Execute: Boolean;
var
//  ZipFileName: string;
  Process: TJvCreateProcess;
begin
  Result := True;

  if not Assigned(SourceFileNames) or (SourceFileNames.Count = 0) then
    Exit;

  SourceFileNames.SaveToFile(TmpListName);
  try
    Process := TJvCreateProcess.Create(nil);
    try
      Process.ApplicationName := ZipExecFileName;
      Process.CommandLine :=
        Format('%s %s %s', [
        AnsiQuotedStr(ZipExecFileName, '"'),
          AnsiQuotedStr(ZipFileName, '"'),
          AnsiQuotedStr('@' + TmpListName, '"')]);
      Process.ConsoleOptions := [coOwnerData, coRedirect];
      Process.StartupInfo.ShowWindow := swHide;
      Process.StartupInfo.DefaultWindowState := False;
      Process.WaitForTerminate := True;
      Process.OnRead := Self.OnRead;
      Process.Run;
      repeat
        Application.HandleMessage;
      until Process.State = psReady
    finally
      Process.Free;
    end;
  finally
    DeleteFile(TmpListName);
  end;
end;

function TZipper.GetTmpListName: string;
begin
  if FTmpListName = '' then
    FTmpListName := FileGetTempName('Zip');

  Result := FTmpListName;
end;

procedure TOnlineHelpZipFilesGenerator.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine and (FOldMsg > '') then
    HintMsg(FOldMsg);
  FOldMsg := S;
end;

procedure TZipper.SetSourceFileNames(Value: TStrings);
begin
  FSourceFileNames.Assign(Value);
end;

{ TWinHelpZipFileGenerator }

function TWinHelpZipFileGenerator.CanStart: Boolean;
begin
  Result :=
    CheckFile(AlsFileName) and
    CheckFile(CntFileName) and
    CheckFile(HLPFileName) and
    CheckDir(DestDir);
end;

function TWinHelpZipFileGenerator.DoExecute: Boolean;
var
  Zipper: TZipper;
begin
  Zipper := TZipper.Create;
  try
    Zipper.SourceFileNames.Add(HLPFileName);
    Zipper.SourceFileNames.Add(CntFileName);
    Zipper.SourceFileNames.Add(AlsFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := Self.ZipExecFileName;
    Zipper.OnRead := HandleRead;
    Result := Zipper.Execute;
  finally
    Zipper.Free;
  end;
end;

function TWinHelpZipFileGenerator.GetAlsFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(DoxFileName, '.als');
end;

function TWinHelpZipFileGenerator.GetCntFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(DoxFileName, '.cnt');
end;

function TWinHelpZipFileGenerator.GetDestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-WinHelp.zip
  Result := IncludeTrailingPathDelimiter(DestDir) +
    ChangeFileExt(DoxFileName, '') + 'Help-WinHelp.zip';
end;

function TWinHelpZipFileGenerator.GetHLPFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(DoxFileName, '.HLP');
end;

function TWinHelpZipFileGenerator.GetTaskDescription: string;
begin
  Result := 'Generating Win Help zip file..';
end;

procedure TWinHelpZipFileGenerator.HandleRead(Sender: TObject;
  const S: string; const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine and (FOldMsg > '') then
    HintMsg(FOldMsg);
  FOldMsg := S;
end;

{ THtmlHelpZipFileGenerator }

function THtmlHelpZipFileGenerator.CanStart: Boolean;
begin
  Result := CheckFile(CHMFileName) and CheckDir(DestDir);
end;

function THtmlHelpZipFileGenerator.DoExecute: Boolean;
var
  Zipper: TZipper;
begin
  Zipper := TZipper.Create;
  try
    Zipper.SourceFileNames.Add(CHMFileName);
    Zipper.ZipFileName := DestFileName;
    Zipper.ZipExecFileName := Self.ZipExecFileName;
    Zipper.OnRead := HandleRead;
    Result := Zipper.Execute;
  finally
    Zipper.Free;
  end;
end;

function THtmlHelpZipFileGenerator.GetCHMFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(HtmlHelpDir) +
    ChangeFileExt(DoxFileName, '.chm');
end;

function THtmlHelpZipFileGenerator.GetDestFileName: string;
begin
  // JVCL330.dox -> JVCL330Help-HTML.zip
  Result := IncludeTrailingPathDelimiter(DestDir) +
    ChangeFileExt(DoxFileName, '') + 'Help-HTML.zip';
end;

function THtmlHelpZipFileGenerator.GetTaskDescription: string;
begin
  Result := 'Generating HTML help zip file..';
end;

procedure THtmlHelpZipFileGenerator.HandleRead(Sender: TObject;
  const S: string; const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine and (FOldMsg > '') then
    HintMsg(FOldMsg);
  FOldMsg := S;
end;

function TZipTask.CanStart: Boolean;
var
  I: Integer;
begin
  Result := CheckDir(ExtractFilePath(Zipper.ZipFileName)) and
    CheckFile(Zipper.ZipExecFileName);
  if Result then
  begin
    for I := 0 to Zipper.SourceFileNames.Count-1 do
    begin
      Result := CheckFile(Zipper.SourceFileNames[i]);
      if not Result then Exit;
    end;
  end;
end;

constructor TZipTask.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FZipper := TZipper.Create;
  FZipper.OnRead :=HandleRead;
end;

destructor TZipTask.Destroy;
begin
  FZipper.Free;
  inherited;
end;

function TZipTask.DoExecute: Boolean;
begin
  Result := FZipper.Execute;
end;

function TZipTask.GetTaskDescription: string;
begin
  Result := 'Zipping';
end;

procedure TZipTask.HandleRead(Sender: TObject;
  const S: string; const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine and (FOldMsg > '') then
    HintMsg(FOldMsg);
  FOldMsg := S;
end;

end.

