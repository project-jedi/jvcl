program TracerTest;

uses
  Forms,
  fTracerTest in 'fTracerTest.pas' {Form1},
  iMTracer in 'iMTracer.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
