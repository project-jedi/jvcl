unit glSingleInstance;

interface

uses
  Windows, Messages, Classes, Forms, Dialogs, syncobjs, SysUtils;

type
  TglSingleInstance = class(TComponent)
  private
    CheckEvent: TEvent;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Gl Controls', [TglSingleInstance]);
end;

{ semaphore

Var hs:THandle;
begin
  hs:=CreateSemaphore(Nil,0,2,'MyProgSemaphore');
  If GetLastError=ERROR_ALREADY_EXISTS Then halt;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  ReleaseSemaphore(hs,2,0);
end.
}

{ mutex

const
  SAppMutex = 'SomeName';

var
  Mtx: THandle;
  Wait: Longint;

begin
  Application.Initialize;
  Mtx := CreateMutex(nil, True, PChar(SAppMutex));
  try
    Wait := WaitForSingleObject(Mtx, 0);
    if (Wait <> WAIT_TIMEOUT) then
    begin
      OpenMutex(SYNCHRONIZE, False, PChar(SAppMutex));
      try
        Application.CreateForm(TfrmMain, frmMain);
        Application.Run;
      finally
        ReleaseMutex(Mtx);
      end;
    end else
      Exit;
  finally
    CloseHandle(Mtx);
  end;
end.
}
{ TglSingleInstance }

constructor TglSingleInstance.Create(AOwner: TComponent);
begin
  inherited;
  if csDesigning in ComponentState then exit;
  CheckEvent := TEvent.Create( nil, false, true, ExtractFileName(ParamStr(0)));
  if CheckEvent.WaitFor(10) <> wrSignaled then
  begin
    Application.MessageBox('Копия данной программы уже запущена. Повторный запуск программы не разрешен.', PChar('Повторный запуск программы '+ ExtractFileName(ParamStr(0))), MB_ICONSTOP or MB_OK);
    halt;
  end;
end;

destructor TglSingleInstance.Destroy;
begin
  inherited;
  if Assigned(CheckEvent) then CheckEvent.Free;
end;

end.
