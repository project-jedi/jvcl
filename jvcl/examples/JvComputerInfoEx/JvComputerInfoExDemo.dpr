program JvComputerInfoExDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
