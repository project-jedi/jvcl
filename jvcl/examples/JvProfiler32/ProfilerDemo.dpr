program ProfilerDemo;

uses
  Forms,
  Profiler32MainFormU in 'Profiler32MainFormU.pas' {Profiler32MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TProfiler32MainForm, Profiler32MainForm);
  Application.Run;
end.
