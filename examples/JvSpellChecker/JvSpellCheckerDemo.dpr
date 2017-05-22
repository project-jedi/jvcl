program JvSpellCheckerDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSpellCheckerForm in 'JvSpellCheckerForm.pas' {frmSpellChecker};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSpellChecker, frmSpellChecker);
  Application.Run;
end.
