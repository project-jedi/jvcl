program Install2LabelDemo;

uses
  Forms,
  InstallLabelMainFormU in 'InstallLabelMainFormU.pas' {InstallLabelMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TInstallLabelMainForm, InstallLabelMainForm);
  Application.Run;
end.
