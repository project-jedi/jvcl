program JvID3v1Demo;

uses
  Forms,
  JvID3v1MainFormU in 'JvID3v1MainFormU.pas' {JvID3v1MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvID3v1MainForm, JvID3v1MainForm);
  Application.Run;
end.
