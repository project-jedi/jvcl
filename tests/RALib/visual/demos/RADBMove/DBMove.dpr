program DBMove;

uses
  Forms,
  fDBMove in 'fDBMove.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TRADBMove Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
