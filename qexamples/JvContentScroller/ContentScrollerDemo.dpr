{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program ContentScrollerDemo;

uses
  QForms,
  ContentScrollerMainFormU in 'ContentScrollerMainFormU.pas' {ContentScrollerMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TContentScrollerMainForm, ContentScrollerMainForm);
  Application.Run;
end.
