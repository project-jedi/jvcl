program JvDotNetDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JVCL DotNet Controls Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

