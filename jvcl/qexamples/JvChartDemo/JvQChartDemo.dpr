program JvQChartDemo;

uses
  QForms,
  JvQChartDemoFm in 'JvQChartDemoFm.pas' {JvChartDemoForm},
  StatsClasses in 'StatsClasses.pas',
  JvQChart in '..\..\qrun\JvQChart.pas';

{$R *.res}


begin
  Application.Title := 'JvChart Demo';
  Application.CreateForm(TJvChartDemoForm, JvChartDemoForm);
  Application.Run;
end.
