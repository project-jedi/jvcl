program GenDox;

uses
  Forms,
  GenDoxMain in 'GenDoxMain.pas' {frmMain},
  GenDoxEngine in 'GenDoxEngine.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
