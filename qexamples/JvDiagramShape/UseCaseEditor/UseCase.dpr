{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program UseCase;

uses
  QForms,
  MainForm in 'MainForm.pas' {MainDlg},
  CaptionEditForm in 'CaptionEditForm.pas' {CaptionEditDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
