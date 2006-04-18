program DelphiPkgInstallerCreator;

uses
  Forms,
  Main in 'Main.pas' {FormMain},
  PDPackageLoader in 'PackageLoading\PDPackageLoader.pas',
  PDHelpers in 'PackageLoading\PDHelpers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
