program JvPlgMainApp;

uses
  Forms,
  ufrmMain in 'ufrmMain.pas' {frmMain},
  JvPlgIntf in '..\JvPlgIntf.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
