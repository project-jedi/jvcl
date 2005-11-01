{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program FindReplaceDemo;

uses
  QForms,
  FindReplaceMainFormU in 'FindReplaceMainFormU.pas' {FindReplaceMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFindReplaceMainForm, FindReplaceMainForm);
  Application.Run;
end.
