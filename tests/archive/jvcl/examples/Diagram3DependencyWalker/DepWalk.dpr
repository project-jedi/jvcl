program DepWalk;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Dependency Walker';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
