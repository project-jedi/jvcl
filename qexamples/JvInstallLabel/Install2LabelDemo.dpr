{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program Install2LabelDemo;

uses
  QForms,
  InstallLabelMainFormU in 'InstallLabelMainFormU.pas' {InstallLabelMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TInstallLabelMainForm, InstallLabelMainForm);
  Application.Run;
end.
