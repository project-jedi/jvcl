{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQZoomProj;

uses
  QForms,
  JvQZoomMainFormU in 'JvQZoomMainFormU.pas' {JvZoomMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvZoomMainForm, JvZoomMainForm);
  Application.Run;
end.
