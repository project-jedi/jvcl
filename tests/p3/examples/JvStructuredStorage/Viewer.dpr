program Viewer;

uses
  Forms,
  ViewMain in 'ViewMain.pas' {frmMain},
  PropsFrm in 'PropsFrm.pas' {frmProps},
  JvStructStore in '..\..\source\JvStructStore.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Compound Document Editor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
