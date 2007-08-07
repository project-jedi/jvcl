unit DoxFileBuilder;

interface

uses
  Windows, JVCLHelpUtils, JvCreateProcess,
  Classes;

type
  TDoxFileBuilder = class(TTask)
  private
    FConfiguration: string;
    FDMCCFileName: string;
    FDoxFileName: string;
    FCreateProcess: TJvCreateProcess;
    FLastLine: string;
    procedure HandleRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    procedure HandleTerminate(Sender: TObject; ExitCode: DWORD);
    function Compile: Boolean;
    function TerminateWinHelpCompilerProcess: Boolean;
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;

    property DMCCFileName: string read FDMCCFileName write FDMCCFileName;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property Configuration: string read FConfiguration write FConfiguration;
  end;

implementation

uses Forms, SysUtils, JclSysInfo;

{ TDoxFileBuilder }

function TDoxFileBuilder.CanStart: Boolean;
begin
  Result := CheckFile(DMCCFileName) and CheckFile(DoxFileName);
end;

function GetCommandLine(
  const DMCCFileName: string;
  const Configuration: string;
  const CheckForErrorsOnly: Boolean;
  const DoNotShowDocumentationAfterBuildIsComplete: Boolean;
  const WarningLevel: Integer;
  const DoxFileName: string
  ): string;
begin
  //dmcc.exe [-config "<NAME>"] [-chk] [-all] [-noshow] [-w1|w2|w3] <PROJECT>
  //dmcc.exe -sym <SYMBOLFILE> <PROJECT>

  Result := AnsiQuotedStr(DMCCFileName, '"');
  if Configuration <> '' then
    Result := Result + ' -config ' + AnsiQuotedStr(Configuration, '"');
  if CheckForErrorsOnly then
    Result := Result + ' -chk';
  if DoNotShowDocumentationAfterBuildIsComplete then
    Result := Result + ' -noshow';
  case WarningLevel of
    1: Result := Result + ' -w1';
    2: Result := Result + ' -w2';
    3: Result := Result + ' -w3';
  end;
  Result := Result + ' ' + DoxFilename;
end;

function TDoxFileBuilder.Compile: Boolean;
begin
  Result := True;

  FCreateProcess.CurrentDirectory := ExtractFilePath(DoxFilename);
  FCreateProcess.CommandLine := GetCommandLine(
    DMCCFileName, Configuration, False, True, 1, DoxFileName);
  FCreateProcess.ConsoleOptions := [coOwnerData, coRedirect];
  FCreateProcess.StartupInfo.ShowWindow := swHide;
  FCreateProcess.StartupInfo.DefaultWindowState := False;
  FCreateProcess.WaitForTerminate := True;
  FCreateProcess.OnRead := HandleRead;
  FCreateProcess.OnTerminate := HandleTerminate;

  FCreateProcess.Run;
  repeat
    Application.HandleMessage;
  until FCreateProcess.State = psReady;
  // last line
  HandleRead(Self, '', True);
end;

constructor TDoxFileBuilder.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FCreateProcess := TJvCreateProcess.Create(nil);
end;

destructor TDoxFileBuilder.Destroy;
begin
  FCreateProcess.Free;
  inherited Destroy;
end;

function TDoxFileBuilder.DoExecute: Boolean;
begin
  Result :=
    Compile and TerminateWinHelpCompilerProcess;
end;

function GetGrindClassProcessID: DWORD;

  function EnumWinProc(Wnd: HWnd; var ProcessID: Integer): Bool; stdcall;
  var
    Buf: array[Byte] of Char;
  begin
    Result := True;
    GetClassName(wnd, Buf, sizeof(Buf));
    if (StrIComp(Buf, 'GrindClass') = 0) then
    begin
      Result := False;
      GetWindowThreadProcessId(Wnd, @ProcessID);
    end;
  end;

begin
  EnumWindows(@EnumWinProc, Integer(@Result));
end;

function TDoxFileBuilder.TerminateWinHelpCompilerProcess: Boolean;
var
  ProcessID: DWORD;
begin
  Result := True;

  StatusMsg('Stopping WinHelp compiler');

  ProcessID := GetGrindClassProcessID;
  if ProcessID <> 0 then
  begin
    case TerminateApp(ProcessID, 1000) of
      taClean: HintMsg('Process terminated clean');
      taKill: WarningMsg('Process killed');
    else
      WarningMsg('Error');
    end;
  end
  else
  begin
    WarningMsg('No GrindClass found');
  end;
end;

function TDoxFileBuilder.GetTaskDescription: string;
begin
  Result := 'Building help';
end;

procedure TDoxFileBuilder.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine then
    if FLastLine <> '' then
      HintMsg(FLastLine);
  FLastLine := S;
end;

procedure TDoxFileBuilder.HandleTerminate(Sender: TObject;
  ExitCode: DWORD);
begin
  StatusMsg('Process finished');

  case ExitCode of
    0:   HintMsg('No error.');
    20:  WarningMsg('Warnings (such as broken links).');
    30:  ErrorMsg('QA checks set up in the project failed.');
    40:  ErrorMsg('Errors during the output phase.');
    41:  ErrorMsg('Fatal errors during output (usually indicates bugs).');
    50:  ErrorMsg('Output file cannot be created (locked by other application).');
    100: ErrorMsg('Invalid command line (eg. wrong command line switch).');
    110: ErrorMsg('The given project cannot be found or loaded.');
  else
    WarningMsgFmt('Unknown ExitCode %d', [ExitCode]);
  end;
end;

end.

