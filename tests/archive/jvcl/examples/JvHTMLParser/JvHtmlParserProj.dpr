program JvHtmlParserProj;

uses
  Forms, JvHTMLParserMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvHtmlParserDemo';
  Application.CreateForm(TJvHTMLParserMainForm, JvHTMLParserMainForm);
  Application.Run;
end.

