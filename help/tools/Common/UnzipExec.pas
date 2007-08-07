unit UnzipExec;

interface

uses
  Classes, JvCreateProcess,
  JVCLHelpUtils;

type
  TUnzipExec = class(TTask)
  private
    FSrcFileName: string;
    FDestDirectory: string;

    FCreateProcess: TJvCreateProcess;
    FUnZipExec: string;
    FOldMsg: string;

    procedure HandleRead(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);
  protected
    function UnZip: Boolean;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    constructor Create(ATaskManager: ITaskManager); override;
    destructor Destroy; override;
    property SrcFileName: string read FSrcFileName write FSrcFileName;
    property DestDirectory: string read FDestDirectory write FDestDirectory;
    property UnZipExec: string read FUnZipExec write FUnZipExec;
  end;

implementation

uses
  Forms,
  SysUtils;

{ TUnzipExec }

function TUnzipExec.CanStart: Boolean;
begin
  Result := CheckFile(SrcFileName) and
    CheckDir(DestDirectory) and
    CheckFile(UnZipExec);
end;

constructor TUnzipExec.Create(ATaskManager: ITaskManager);
begin
  inherited Create(ATaskManager);
  FCreateProcess := TJvCreateProcess.Create(nil);
  FOldMsg := '';
end;

destructor TUnzipExec.Destroy;
begin
  FCreateProcess.Free;
  inherited Destroy;
end;

function TUnzipExec.DoExecute: Boolean;
begin
  Result := UnZip;
end;

function TUnzipExec.GetTaskDescription: string;
begin
  Result := 'Unzipping file..';
end;

procedure TUnzipExec.HandleRead(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  if StartsOnNewLine and (FOldMsg > '') then
    HintMsg(FOldMsg);
  FOldMsg := S;
end;

function TUnzipExec.UnZip: Boolean;
begin
  Result := True;
  StatusMsg('Unzipping..');
  HintMsgFmt('Unzipping %s to %s', [SrcFileName, DestDirectory]);

  FCreateProcess.ApplicationName := UnZipExec;
  FCreateProcess.CommandLine :=
    Format('%s %s %s', [
      AnsiQuotedStr(UnZipExec, '"'),
      AnsiQuotedStr(SrcFileName, '"'), AnsiQuotedStr(DestDirectory, '"')]);
  FCreateProcess.ConsoleOptions := [coOwnerData, coRedirect];
  FCreateProcess.StartupInfo.ShowWindow := swHide;
  FCreateProcess.StartupInfo.DefaultWindowState := False;
  FCreateProcess.WaitForTerminate := True;
  FCreateProcess.OnRead := HandleRead;
  FCreateProcess.Run;
  repeat
    Application.HandleMessage;
  until FCreateProcess.State = psReady;

  HandleRead(Self, '', True);
end;

end.

