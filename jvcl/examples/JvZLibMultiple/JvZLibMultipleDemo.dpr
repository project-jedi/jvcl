program JvZLibMultipleDemo;

uses
  Forms,
  JvZLibMultipleMainFormU in 'JvZLibMultipleMainFormU.pas' {JvZLibMultipleMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvZLibMultipleMainForm, JvZLibMultipleMainForm);
  Application.Run;
end.
