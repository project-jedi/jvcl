program JvInstallProj;

uses
  Forms,
  fInstall in 'fInstall.pas' {Form1},
  JvInstallerPage in '..\..\Source\JvInstallerPage.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
