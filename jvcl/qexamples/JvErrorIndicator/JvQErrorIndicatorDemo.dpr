{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQErrorIndicatorDemo;

uses
  QForms,
  MainFrm in 'MainFrm.pas' {frmErrIndicatorDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvErrorIndicator Demo';
  Application.CreateForm(TfrmErrIndicatorDemo, frmErrIndicatorDemo);
  Application.Run;
end.
