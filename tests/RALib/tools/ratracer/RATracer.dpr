program RATracer;

uses
  Forms,
  Dialogs,
  fTracer in 'fTracer.pas' {TracerMain},
  iMTracer in 'iMTracer.pas';

{$R *.RES}


begin
  Application.Initialize;
  Application.Title := 'JVCL Tracer';
  Application.CreateForm(TTracerMain, TracerMain);
  Application.Run;
end.
