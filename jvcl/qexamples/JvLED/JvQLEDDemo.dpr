{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQLEDDemo;

uses
  QForms,
  LEDMain in 'LEDMain.pas' {LEDDemoMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TLEDDemoMain, LEDDemoMain);
  Application.Run;
end.
