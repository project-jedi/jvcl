{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program OLBarDemo;

uses
  QForms,
  OLBarMainFormU in 'OLBarMainFormU.pas' {OLBarMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TOLBarMainForm, OLBarMainForm);
  Application.Run;
end.
