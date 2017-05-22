program BkgndDemo;

uses
  Forms,
  MDIBkgndDemoMain in 'MDIBkgndDemoMain.pas' {MDIMainForm},
  MDIBkgndDemoSettings in 'MDIBkgndDemoSettings.pas' {BkgndImageSettings},
  MDIBkgndDemoFrame in 'MDIBkgndDemoFrame.pas' {DemoFrame: TFrame},
  MDIBkgndDemoChld in 'MDIBkgndDemoChld.pas' {MDIChildForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MDI Wallpaper demo';
  Application.CreateForm(TMDIMainForm, MDIMainForm);
  Application.CreateForm(TBkgndImageSettings, BkgndImageSettings);
  Application.CreateForm(TMDIChildForm, MDIChildForm);
  Application.Run;
end.
