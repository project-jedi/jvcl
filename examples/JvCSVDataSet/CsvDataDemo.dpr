program CsvDataDemo;

uses
  Forms,
  CsvDataSourceDemoFm in 'CsvDataSourceDemoFm.pas' {CsvDataSourceForm},
  JvCsvData in '..\JvCsvData.Pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCsvDataSourceForm, CsvDataSourceForm);
  Application.Run;
end.
