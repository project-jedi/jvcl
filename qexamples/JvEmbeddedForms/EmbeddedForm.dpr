{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program EmbeddedForm;

uses
  QForms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  EmbeddedFormUnit in 'EmbeddedFormUnit.pas' {FirstEmbeddedForm},
  DeepEmbeddedFormUnit in 'DeepEmbeddedFormUnit.pas' {DeepEmbeddedForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvEmbedded Forms Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDeepEmbeddedForm, DeepEmbeddedForm);
  Application.CreateForm(TFirstEmbeddedForm, FirstEmbeddedForm);
  Application.Run;
end.

