program JvInterpreterSimpleExpression;

uses
  Forms,
  JvInterpreterSimpleExpressionFm in 'JvInterpreterSimpleExpressionFm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
