program Mdiapp;

uses
  Forms,
  Main in 'MAIN.PAS' {MainForm},
  Childwin in 'CHILDWIN.PAS' {MDIChild},
  About in 'about.pas' {AboutBox};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm , MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
