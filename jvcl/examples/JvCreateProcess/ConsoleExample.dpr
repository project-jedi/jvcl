program ConsoleExample;

uses
  Forms,
  ConsoleExampleMainFormU in 'ConsoleExampleMainFormU.pas' {ConsoleExampleMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TConsoleExampleMainForm, ConsoleExampleMainForm);
  Application.Run;
end.
