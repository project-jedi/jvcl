program GenGroupOverviews;

uses
  Forms,
  main_grpoverview in 'main_grpoverview.pas' {frmGrpOverviewGen};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGrpOverviewGen, frmGrpOverviewGen);
  Application.Run;
end.
