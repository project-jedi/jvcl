program JvChartDemo;

uses
  Forms,
  JvChartDemoFm in 'JvChartDemoFm.pas' {JvChartDemoForm},
  StatsClasses in 'StatsClasses.pas',
  JvChart in '..\..\run\JvChart.pas';

{$R *.res}


begin
  Application.Title := 'JvChart Demo';
  Application.CreateForm(TJvChartDemoForm, JvChartDemoForm);
  Application.Run;
end.
