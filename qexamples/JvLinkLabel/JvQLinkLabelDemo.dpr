{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQLinkLabelDemo;

uses
  QForms,
  InfoStrings in 'InfoStrings.pas',
  Play in 'Play.pas' {frmPlay},
  JvQLinkLabelMainFormU in 'JvQLinkLabelMainFormU.pas' {JvLinkLabelMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'TJvLinkLabel Demo';
  Application.CreateForm(TJvLinkLabelMainForm, JvLinkLabelMainForm);
  Application.Run;
end.
