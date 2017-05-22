program SimpleDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JVCL XP Controls - Simple Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
