program JvChartDemo;

uses
  QForms,
  JvChartDemoFm in 'JvChartDemoFm.pas' {JvChartDemoForm},
  StatsClasses in 'StatsClasses.pas';

{$R *.res}


begin
  Application.Title := 'JvChart Demo';
  Application.CreateForm(TJvChartDemoForm, JvChartDemoForm);
  Application.Run;
end.
