program RAControls;

uses
  Forms,
  fJvControls in 'fJvControls.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm , MainForm);
  Application.Run;
end.
