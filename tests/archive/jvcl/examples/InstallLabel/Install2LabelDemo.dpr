program Install2LabelDemo;

uses
  Forms, InstallLabelMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TInstallLabelMainForm, InstallLabelMainForm);
  Application.Run;
end.
