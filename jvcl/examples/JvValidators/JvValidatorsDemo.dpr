program JvValidatorsDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
