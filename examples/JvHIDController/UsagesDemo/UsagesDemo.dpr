program UsagesDemo;

uses
  Forms,
  UsagesMain in 'UsagesMain.pas' {UsagesForm},
  Info in 'Info.pas' {InfoForm},
  UsagesInfo in 'UsagesInfo.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TUsagesForm, UsagesForm);
  Application.Run;
end.
