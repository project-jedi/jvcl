program JvValidatorsDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvValidators in '..\..\source\JvValidators.pas',
  JvErrProvider in '..\..\source\JvErrProvider.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
