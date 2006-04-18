program Installer;

uses
  Compiler5MissingPropertyFix in 'D5Workarounds\Compiler5MissingPropertyFix.pas',
  Forms,
  D5CheckLst in 'D5Workarounds\D5CheckLst.pas',
  CmdLineUtils in 'Common\CmdLineUtils.pas',
  Utils in 'Common\Utils.pas',
  Main in 'Main.pas' {FormMain},
  DelphiData in 'Common\DelphiData.pas',
  JCLDetectConsts in 'Common\JCLDetectConsts.pas',
  ConfigurationBase in 'Configurations\ConfigurationBase.pas',
  JVCLConfiguration in 'Configurations\JVCLConfiguration.pas',
  InstallerConsts in 'Translation\InstallerConsts.pas',
  ConfigOptions in 'Configurations\ConfigOptions.pas',
  FrmeDirectoryEdit in 'StdFrames\FrmeDirectoryEdit.pas' {FrameDirectoryEdit: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
