program JvItemViewerDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  ViewerFrm in 'ViewerFrm.pas' {frmImageViewer};

{$R *.res}

begin
//  MemChk;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
