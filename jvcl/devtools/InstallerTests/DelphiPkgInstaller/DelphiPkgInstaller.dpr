program DelphiPkgInstaller;

uses
  JvGnugettext,
  Windows,
  SysUtils,
  Forms,
  Main in 'Main.pas' {FormMain},
  FrmStartup in 'FrmStartup.pas' {FormStartup},
  DataModuleMain in 'DataModuleMain.pas' {DMMain: TDataModule},
  Configuration in 'Configuration.pas',
  Helpers in 'Helpers.pas',
  Packages in 'Packages.pas',
  Utils in '..\JVCLInstall\Utils.pas',
  CmdLineUtils in '..\JVCLInstall\CmdLineUtils.pas',
  DelphiData in '..\JVCLInstall\DelphiData.pas',
  Logging in 'Logging.pas';

{$R *.res}

begin
  if Config.Target = nil then
  begin
    MessageBox(0, PChar(Format(_('%s is not installed.'), [VersionNames[Config.Version]])),
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;
  if not CheckValidDelphiInstallation then
  begin
    MessageBox(0, PChar(Format(_('No valid Delphi installation. You must have started Delphi at least once before you can install packages.'), [])),
      'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;
  DMMain := TDMMain.Create(Application);
  FormStartup := TFormStartup.Create(Application);
  FormStartup.Show;
  Application.ProcessMessages;
  Application.Initialize;
  Application.Title := 'Delphi Package Installer';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
