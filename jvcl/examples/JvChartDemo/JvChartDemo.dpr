program JvChartDemo;

uses
  Forms,
  JvChartDemoFm in 'JvChartDemoFm.pas' {JvChartDemoForm};

{$R *.res}


begin
  Application.Title := 'JvChart Demo';
  Application.CreateForm(TJvChartDemoForm, JvChartDemoForm);
  Application.Run;
end.
