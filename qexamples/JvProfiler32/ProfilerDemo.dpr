{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program ProfilerDemo;

uses
  QForms,
  Profiler32MainFormU in 'Profiler32MainFormU.pas' {Profiler32MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TProfiler32MainForm, Profiler32MainForm);
  Application.Run;
end.
