program JvInterpreterEndUser;

uses
  Forms,
  DM_RaInterpreterEndUser in 'DM_RaInterpreterEndUser.pas' {DMRaIntrEndUsr: TDataModule},
  RaInterpreterEndUserMainFormU in 'RaInterpreterEndUserMainFormU.pas' {RaInterpreterEndUserMainForm},
  fReports in 'fReports.pas' {Reports};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TRaInterpreterEndUserMainForm, RaInterpreterEndUserMainForm);
  Application.CreateForm(TDMRaIntrEndUsr, DMRaIntrEndUsr);
  Application.Run;
end.
