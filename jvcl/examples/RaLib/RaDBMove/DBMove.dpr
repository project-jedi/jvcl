program DBMove;

uses
  Forms,
  fJvDBMove in 'fJvDBMove.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TJvDBMove Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
