program JvHtmlParserProj;

uses
  Forms,
  fHTMLParser in 'fHTMLParser.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvHtmlParserDemo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

