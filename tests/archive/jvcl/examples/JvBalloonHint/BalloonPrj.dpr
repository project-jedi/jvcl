program BalloonPrj;

uses
  Forms,
  MainDlg in 'MainDlg.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
