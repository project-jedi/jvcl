program BasicDemo;

uses
  Forms,
  BasicMain in 'BasicMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
