unit HtmlHelpCompiler;

interface

uses
  JVCLHelpUtils, JvCreateProcess,
  Classes;

type
  THtmlHelpCompiler = class(TTask)
  private
    FHtmlHelpDir: string;
    FDoxFileName: string;
    FCreateProcess: TJvCreateProcess;
    FHHCFileName: string;
    function GetHHPFileName: string;
    procedure HandleRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);

    function Compile: Boolean;
  protected
    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property HtmlHelpDir: string read FHtmlHelpDir write FHtmlHelpDir;
    property DoxFileName: string read FDoxFileName write FDoxFileName;
    property HHPFileName: string read GetHHPFileName;
    property HHCFileName: string read FHHCFileName write FHHCFileName;
  end;

implementation

uses
  Forms, SysUtils;

//=== { THtmlHelpCompiler } ==================================================

constructor THtmlHelpCompiler.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FCreateProcess := TJvCreateProcess.Create(nil);
end;

destructor THtmlHelpCompiler.Destroy;
begin
  FCreateProcess.Free;
  inherited Destroy;
end;

function THtmlHelpCompiler.CanStart: Boolean;
begin
  Result := CheckFile(HHPFileName) and CheckFile(HHCFileName);
end;

function THtmlHelpCompiler.Compile: Boolean;
begin
  Result := True;

  FCreateProcess.CurrentDirectory := HtmlHelpDir;
  FCreateProcess.CommandLine := Format('%s %s', [AnsiQuotedStr(HHCFileName, '"'), ExtractFileName(HHPFileName)]);
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

function THtmlHelpCompiler.DoExecute: Boolean;
begin
  Result := Compile;
end;

function THtmlHelpCompiler.GetHHPFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(HtmlHelpDir) +
    ChangeFileExt(ExtractFileName(DoxFileName), '.hhp');
end;

function THtmlHelpCompiler.GetTaskDescription: string;
begin
  Result := 'Compiling Html help';
end;

procedure THtmlHelpCompiler.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  StatusMsg(S);
end;

end.
