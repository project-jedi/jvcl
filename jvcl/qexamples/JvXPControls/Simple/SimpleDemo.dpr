{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program SimpleDemo;

uses
  QForms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JVCL XP Controls - Simple Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
