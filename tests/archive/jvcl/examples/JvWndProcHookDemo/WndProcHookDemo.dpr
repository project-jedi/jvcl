program WndProcHookDemo;

uses
  Forms, JvWndProcHookDemoMainFormU;  

{$R *.res}

begin
  Application.CreateForm(TJvWndProcHookDemoMainForm, JvWndProcHookDemoMainForm);
  Application.Initialize;
  Application.Run;
end.
