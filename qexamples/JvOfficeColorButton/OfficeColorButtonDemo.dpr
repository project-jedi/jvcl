{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program OfficeColorButtonDemo;

uses
  QForms,
  Main in 'Main.pas' {ColorDemoMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TColorDemoMainForm, ColorDemoMainForm);
  Application.Run;
end.
