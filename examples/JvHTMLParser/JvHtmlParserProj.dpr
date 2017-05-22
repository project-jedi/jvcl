program JvHtmlParserProj;

uses
  Forms,
  JvHTMLParserMainFormU in 'JvHTMLParserMainFormU.pas' {JvHTMLParserMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvHtmlParserDemo';
  Application.CreateForm(TJvHTMLParserMainForm, JvHTMLParserMainForm);
  Application.Run;
end.

