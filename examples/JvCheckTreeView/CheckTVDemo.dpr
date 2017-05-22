program CheckTVDemo;

uses
  Forms,
  CheckTVDemoFrm in 'CheckTVDemoFrm.pas' {frmCheckTVDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCheckTVDemo, frmCheckTVDemo);
  Application.Run;
end.
