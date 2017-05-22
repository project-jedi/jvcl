program CsvDataDemo_Unicode;

uses
  Forms,
  CsvDataSourceUnicodeDemoFm in 'CsvDataSourceUnicodeDemoFm.pas' {CsvDataSourceForm},
  JvCsvData in '..\..\run\JvCsvData.pas',
  JvCsvParse in '..\..\run\JvCsvParse.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCsvDataSourceForm, CsvDataSourceForm);
  Application.Run;
end.
