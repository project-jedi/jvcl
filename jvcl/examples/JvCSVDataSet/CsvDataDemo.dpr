program CsvDataDemo;

uses
  Forms,
  CsvDataSourceDemoFm in 'CsvDataSourceDemoFm.pas' {CsvDataSourceForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCsvDataSourceForm, CsvDataSourceForm);
  Application.Run;
end.
