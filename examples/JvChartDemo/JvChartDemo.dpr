program JvChartDemo;
{ JEDI Demo App }
uses
  Forms,
  JvChartDemoFm in 'JvChartDemoFm.pas' {JvGraphDemoForm};

{$R *.RES}


begin
  Application.Title := 'JvChart Demo';
  Application.CreateForm(TJvChartDemoForm, JvChartDemoForm);
  Application.Run;
end.
