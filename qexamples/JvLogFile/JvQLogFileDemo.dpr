{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQLogFileDemo;

uses
  QForms,
  JvQLogFileMainFormU in 'JvQLogFileMainFormU.pas' {JvLogFileMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvLogFileMainForm, JvLogFileMainForm);
  Application.Run;
end.
