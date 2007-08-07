unit DoxFileBuiler;

interface

uses
  JVCLHelpUtils, JvCreateProcess,
  Classes;

type
  TDoxFileBuiler = class(TTask)
  private
    FConfiguration: string;
    FDMCCFileName: string;
    FDoxFileName: string;
    FCreateProcess: TJvCreateProcess;
    FLastLine: string;
    procedure HandleRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
    function Compile: Boolean;
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

uses Forms, SysUtils;

{ TDoxFileBuiler }

function TDoxFileBuiler.CanStart: Boolean;
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

function TDoxFileBuiler.Compile: Boolean;
begin
  Result := True;

  FCreateProcess.CurrentDirectory := ExtractFilePath(DoxFilename);
  FCreateProcess.CommandLine := GetCommandLine(
    DMCCFileName, Configuration, False, False, 1, DoxFileName);
  FCreateProcess.ConsoleOptions := [coOwnerData, coRedirect];
  FCreateProcess.StartupInfo.ShowWindow := swHide;
  FCreateProcess.StartupInfo.DefaultWindowState := False;
  FCreateProcess.WaitForTerminate := True;
  FCreateProcess.OnRead := HandleRead;
  FCreateProcess.Run;
  repeat
    Application.HandleMessage;
  until FCreateProcess.State = psReady;
  // last line
  HandleRead(Self, '', True);
end;

constructor TDoxFileBuiler.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FCreateProcess := TJvCreateProcess.Create(nil);
end;

destructor TDoxFileBuiler.Destroy;
begin
  FCreateProcess.Free;
  inherited Destroy;
end;

function TDoxFileBuiler.DoExecute: Boolean;
begin
  Result := Compile;
end;

function TDoxFileBuiler.GetTaskDescription: string;
begin
  Result := 'Building help';
end;

procedure TDoxFileBuiler.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine then
    if FLastLine <> '' then
      StatusMsg(FLastLine);
  FLastLine := S;
end;

end.

