program JvShellHookDemo;

uses
  Forms,
  JvShellHookDemoMainFormU in 'JvShellHookDemoMainFormU.pas' {JvShellHookDemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvShellHookDemoMainForm, JvShellHookDemoMainForm);
  Application.Run;
end.
