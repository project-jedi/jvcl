program CsvDataDemo;

uses
  Forms,
  CsvDataSourceDemoFm in 'CsvDataSourceDemoFm.pas' {CsvDataSourceForm},
  JvCsvData in '..\..\run\JvCsvData.pas',
  JvCsvParse in '..\..\run\JvCsvParse.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCsvDataSourceForm, CsvDataSourceForm);
  Application.Run;
end.
