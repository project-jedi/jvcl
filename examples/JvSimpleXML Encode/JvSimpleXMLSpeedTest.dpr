program JvSimpleXMLSpeedTest;

uses
  Forms,
  MainSpeedFrm in 'MainSpeedFrm.pas' {frmSpeedTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSpeedTest, frmSpeedTest);
  Application.Run;
end.
