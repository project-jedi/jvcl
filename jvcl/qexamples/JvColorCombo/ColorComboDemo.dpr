{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program ColorComboDemo;

uses
  QForms,
  JvColorComboDemoMainFormU in 'JvColorComboDemoMainFormU.pas' {JvColorComboDemoMainForm},
  JvQExStdCtrls in '..\..\qrun\JvQExStdCtrls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvColorComboDemoMainForm, JvColorComboDemoMainForm);
  Application.Run;
end.
