{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQDataEmbeddedProj;

uses
  QForms,
  JvQDataEmbeddedMainFormU in 'JvQDataEmbeddedMainFormU.pas' {JvDataEmbeddedMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDataEmbeddedMainForm, JvDataEmbeddedMainForm);
  Application.Run;
end.
