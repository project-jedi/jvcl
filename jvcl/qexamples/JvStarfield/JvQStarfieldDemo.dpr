{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQStarfieldDemo;

uses
  QForms,
  StarFieldMain in 'StarFieldMain.pas' {StarfieldMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TStarfieldMainForm, StarfieldMainForm);
  Application.Run;
end.
