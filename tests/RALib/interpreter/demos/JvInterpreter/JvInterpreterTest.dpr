program JvInterpreterTest;

uses
  Forms,
  fJvInterpreterTest in 'fJvInterpreterTest.pas' {Test};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTest, Test);
  Application.Run;
end.
