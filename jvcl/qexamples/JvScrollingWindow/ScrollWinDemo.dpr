{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program ScrollWinDemo;

uses
  QForms,
  ScrollWinMainFormU in 'ScrollWinMainFormU.pas' {JvScrollingWindowMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvScrollingWindowMainForm, JvScrollingWindowMainForm);
  Application.Run;
end.
