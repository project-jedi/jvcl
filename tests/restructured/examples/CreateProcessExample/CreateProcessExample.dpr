program CreateProcessExample;

uses
  Forms,
  CreateProcessMain in 'CreateProcessMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
