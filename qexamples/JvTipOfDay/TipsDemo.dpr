{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program TipsDemo;

uses
  QForms,
  TipOfDayMainFormU in 'TipOfDayMainFormU.pas' {TipOfDayMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTipOfDayMainForm, TipOfDayMainForm);
  Application.Run;
end.
