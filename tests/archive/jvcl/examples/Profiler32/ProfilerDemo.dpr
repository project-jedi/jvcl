program ProfilerDemo;

uses
  Forms, Profiler32MainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TProfiler32MainForm, Profiler32MainForm);
  Application.Run;
end.
