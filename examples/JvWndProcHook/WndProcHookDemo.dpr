program WndProcHookDemo;

uses
  Forms,
  JvWndProcHookDemoMainFormU in 'JvWndProcHookDemoMainFormU.pas' {JvWndProcHookDemoMainForm};

{$R *.res}

begin
  Application.CreateForm(TJvWndProcHookDemoMainForm, JvWndProcHookDemoMainForm);
  Application.Initialize;
  Application.Run;
end.
