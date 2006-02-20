program PackageCheck;

uses
  Forms,
  MainForm in 'MainForm.pas' {PackageCheckForm},
  DefineForm in 'DefineForm.pas' {DefForm},
  TargetInfo in 'TargetInfo.pas',
  UsesParser in 'UsesParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPackageCheckForm, PackageCheckForm);
  Application.Run;
end.
