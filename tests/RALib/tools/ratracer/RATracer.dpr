program RATracer;

uses
  Forms,
  Dialogs,
  fJvTracer in 'fJvTracer.pas' {TracerMain},
  iJvMTracer in 'iJvMTracer.pas';

{$R *.RES}


begin
  Application.Initialize;
  Application.Title := 'JVCL Tracer';
  Application.CreateForm(TJvTracerMain , TracerMain);
  Application.Run;
end.
