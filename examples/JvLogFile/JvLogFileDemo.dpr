program JvLogFileDemo;

uses
  Forms,
  JvLogFileMainFormU in 'JvLogFileMainFormU.pas' {JvLogFileMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvLogFileMainForm, JvLogFileMainForm);
  Application.Run;
end.
