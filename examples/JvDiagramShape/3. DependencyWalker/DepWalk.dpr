program DepWalk;

uses
  Forms,
  DependencyWalkerDemoMainForm in 'DependencyWalkerDemoMainForm.pas' {DependencyWalkerDemoMainFrm},
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
  Application.CreateForm(TDependencyWalkerDemoMainFrm, DependencyWalkerDemoMainFrm);
  Application.Run;
end.
