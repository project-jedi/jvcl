program JvAppHotKeyDemo;

uses
  Forms,
  JvAppHotKeyDemoMainFormU in 'JvAppHotKeyDemoMainFormU.pas' {JvAppHotKeyDemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAppHotKeyDemoMainForm, JvAppHotKeyDemoMainForm);
  Application.Run;
end.
