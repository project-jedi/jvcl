program JvZLibMultipleDemo;

uses
  Forms, JvZLibMultipleMainFormU;  

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvZLibMultipleMainForm, JvZLibMultipleMainForm);
  Application.Run;
end.
