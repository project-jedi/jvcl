program JvErrorIndicatorDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmErrIndicatorDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JvErrorIndicator Demo';
  Application.CreateForm(TfrmErrIndicatorDemo, frmErrIndicatorDemo);
  Application.Run;
end.
