program RAControls;

uses
  Forms,
  fRAControls in 'fRAControls.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
