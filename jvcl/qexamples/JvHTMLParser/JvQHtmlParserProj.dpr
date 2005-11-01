{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program JvQHtmlParserProj;

uses
  QForms,
  JvQHTMLParserMainFormU in 'JvQHTMLParserMainFormU.pas' {JvQHTMLParserMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvHtmlParserDemo';
  Application.CreateForm(TJvHTMLParserMainForm, JvHTMLParserMainForm);
  Application.Run;
end.

