program JvZLibMultipleDemo;

uses
  Forms,
  ZLibMainFrm in 'ZLibMainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
