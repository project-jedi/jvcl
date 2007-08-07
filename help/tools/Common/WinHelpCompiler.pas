unit WinHelpCompiler;

interface

uses
  JVCLHelpUtils, JvCreateProcess,
  Classes;

type
  TWinHelpCompiler = class(TTask)
  private
    FWinHelpDir: string;
    FDoxFileName: string;
    FCreateProcess: TJvCreateProcess;
    FHCRTFFileName: string;
    function GetHPJFileName: string;
    procedure HandleRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
  protected
    function Compile: Boolean;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property WinHelpDir: string read FWinHelpDir write FWinHelpDir;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property HPJFileName: string read GetHPJFileName;
    property HCRTFFileName: string read FHCRTFFileName write FHCRTFFileName;
  end;

implementation

uses
  Forms, SysUtils;

//=== { TWinHelpCompiler } ===================================================

constructor TWinHelpCompiler.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FCreateProcess := TJvCreateProcess.Create(nil);
end;

destructor TWinHelpCompiler.Destroy;
begin
  FCreateProcess.Free;
  inherited Destroy;
end;

function TWinHelpCompiler.CanStart: Boolean;
begin
  Result := CheckFile(HPJFileName) and CheckFile(HCRTFFileName);
end;

function TWinHelpCompiler.Compile: Boolean;
begin
  Result := True;

  FCreateProcess.CurrentDirectory := WinHelpDir;
  //HCRTF [/B path] [/O HLP-filename] [/X[CHNRT]] [RTF-filename] [/NC] [/XT] [HPJ-filename]
  FCreateProcess.ApplicationName := HCRTFFileName;
  FCreateProcess.CommandLine := Format('%s -xn %s', [
    AnsiQuotedStr(HCRTFFileName, '"'), ExtractFileName(HPJFileName)]);
  FCreateProcess.ConsoleOptions := [coOwnerData, coRedirect];
  FCreateProcess.StartupInfo.ShowWindow := swHide;
  FCreateProcess.StartupInfo.DefaultWindowState := False;
  FCreateProcess.WaitForTerminate := True;
  FCreateProcess.OnRead := HandleRead;
  FCreateProcess.Run;
  repeat
    Application.HandleMessage;
  until FCreateProcess.State = psReady
end;

function TWinHelpCompiler.DoExecute: Boolean;
begin
  Result := Compile;
end;

function TWinHelpCompiler.GetHPJFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(WinHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.HPJ');
end;

function TWinHelpCompiler.GetTaskDescription: string;
begin
  Result := 'Compiling Windows help';
end;

procedure TWinHelpCompiler.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  StatusMsg(S);
end;

end.
