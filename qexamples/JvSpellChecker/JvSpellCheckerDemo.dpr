{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvSpellCheckerDemo;

uses
  QForms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSpellCheckerForm in 'JvSpellCheckerForm.pas' {frmSpellChecker};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSpellChecker, frmSpellChecker);
  Application.Run;
end.
