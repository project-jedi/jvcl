program TracerTest;

uses
  Forms,
  fJvTracerTest in 'fJvTracerTest.pas' {Form1},
  iJvMTracer in 'iJvMTracer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
