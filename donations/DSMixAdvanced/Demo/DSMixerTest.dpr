program DSMixerTest;

{%ToDo 'DSMixerTest.todo'}

uses
  Forms,
  unitChannel in 'unitChannel.pas' {FormChannel},
  unitDSMixerTest in 'unitDSMixerTest.pas' {FormDSMixer},
  cbDSMixer in 'cbDSMixer.pas',
  cbAudioFileRead in 'cbAudioFileRead.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDSMixer, FormDSMixer);
  Application.Run;
end.
