program RAControls;

{$R 'RaControls.res' 'RaControls.rc'}

uses
  Forms,
  fJvControls in 'fJvControls.pas' {MainForm};


begin
  Application.Initialize;
  Application.CreateForm(TMainForm , MainForm);
  Application.Run;
end.
