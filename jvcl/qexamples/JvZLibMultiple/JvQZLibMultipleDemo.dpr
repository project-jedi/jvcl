program JvQZLibMultipleDemo;

uses
  QForms,
  JvQZLibMultipleMainFormU in 'JvQZLibMultipleMainFormU.pas' {JvZLibMultipleMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvZLibMultipleMainForm, JvZLibMultipleMainForm);
  Application.Run;
end.
