{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

program CsvDataDemo;

uses
  QForms,
  CsvDataSourceDemoFm in 'CsvDataSourceDemoFm.pas' {CsvDataSourceForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCsvDataSourceForm, CsvDataSourceForm);
  Application.Run;
end.
