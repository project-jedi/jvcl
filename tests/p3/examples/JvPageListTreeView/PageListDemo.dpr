program PageListDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvPageListTreeView in '..\..\source\JvPageListTreeView.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
