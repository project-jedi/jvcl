program pm;

uses
  Forms,
  PackageModifierMainForm in 'PackageModifierMainForm.pas' {PackageModifierMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPackageModifierMainFrm, PackageModifierMainFrm);
  Application.Run;
end.
