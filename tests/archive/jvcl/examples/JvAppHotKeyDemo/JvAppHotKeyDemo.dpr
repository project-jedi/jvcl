program JvAppHotKeyDemo;

uses
  Forms, JvAppHotKeyDemoMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAppHotKeyDemoMainForm, JvAppHotKeyDemoMainForm);
  Application.Run;
end.
