program DelphiPkgUninstall;

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  Utils in '..\..\JVCLInstall\Utils.pas',
  CmdLineUtils in '..\..\JVCLInstall\CmdLineUtils.pas',
  DelphiData in '..\..\JVCLInstall\DelphiData.pas';

{$E bin}

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Uninstall';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
