program JvShellHookDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvShellHook in '..\..\source\JvShellHook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
