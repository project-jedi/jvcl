program JvLogFileDemo;

uses
  Forms, JvLogFileMainFormU;  

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvLogFileMainForm, JvLogFileMainForm);
  Application.Run;
end.
