unit glProcs;//...simple process managment

interface

uses Windows, Messages, Classes, Forms, dialogs;

type

TglProcess = class ( TComponent )
private
  FResult: boolean;
  FFileName: string;
  FOnTermainated: TNotifyEvent;
  si: TStartupInfo;
public
  pi: TProcessInformation;
  function Run: boolean;
  function Kill: boolean;
  destructor Destroy; override;
published
  property FileName: string read FFileName write FFileName;
  property Result: boolean read FResult stored false;
  property OnTermainated: TNotifyEvent read FOnTermainated write FOnTermainated;
end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Proba', [TglProcess]);
end;


destructor TglProcess.Destroy;
begin
  Kill;
  inherited;
end;

function TglProcess.Run: boolean;
begin
  GetStartupInfo(si);
  si.wShowWindow := SW_NORMAL;
  FResult := CreateProcess( PChar(FFileName), nil, nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil, si, pi);
  Run := FResult;
  if Result then
  begin
    while WaitForSingleObject(pi.hProcess, 100) = WAIT_TIMEOUT do Application.ProcessMessages;
    if Assigned(OnTermainated) then OnTermainated(self);
  end;
end;

function TglProcess.Kill: boolean;
begin
  if FResult {and(WaitForSingleObject(pi.hProcess, 100) <> WAIT_TIMEOUT)}
  then Kill := TerminateProcess( pi.hProcess, 0 ) else Kill := false;
end;

end.
