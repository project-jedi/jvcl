program JvMruList;

uses
  Forms,
  fMru in 'fMru.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
