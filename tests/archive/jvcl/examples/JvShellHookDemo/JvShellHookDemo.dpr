program JvShellHookDemo;

uses
  Forms, JvShellHookDemoMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvShellHookDemoMainForm, JvShellHookDemoMainForm);
  Application.Run;
end.
