program JvInterpretertest;

uses
  QForms,
  main in 'main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm , MainForm);
  Application.Run;
end.
