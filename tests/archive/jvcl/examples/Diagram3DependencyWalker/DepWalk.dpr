program DepWalk;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  StatsFrm in 'StatsFrm.pas' {frmUnitStats},
  PrintFrm in 'PrintFrm.pas' {frmPrint},
  DepWalkConsts in 'DepWalkConsts.pas',
  OptionsFrm in 'OptionsFrm.pas' {frmOptions},
  PersistForm in 'PersistForm.pas' {frmPersistable},
  DepWalkUtils in 'DepWalkUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Dependency Walker';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
