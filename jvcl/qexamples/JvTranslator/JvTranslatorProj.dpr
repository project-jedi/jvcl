{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvTranslatorProj;

uses
  QForms,
  JvTranslatorMainFormU in 'JvTranslatorMainFormU.pas' {JvTranslatorMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvTranslatorMainForm, JvTranslatorMainForm);
  Application.Run;
end.
