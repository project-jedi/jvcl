program JvErrProviderDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmErrProviderDemo},
  JvErrProvider in '..\..\source\JvErrProvider.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvErrorProvider Demo';
  Application.CreateForm(TfrmErrProviderDemo, frmErrProviderDemo);
  Application.Run;
end.
