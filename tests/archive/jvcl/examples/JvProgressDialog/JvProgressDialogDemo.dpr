program JvProgressDialogDemo;

uses
  Forms,
  JvProgressFrm in '..\..\source\JvProgressFrm.pas' {frmProgress},
  JvProgressDialogMain in 'JvProgressDialogMain.pas' {frmProgressDialogDemo},
  JvProgressDialog in '..\..\source\JvProgressDialog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmProgressDialogDemo, frmProgressDialogDemo);
  Application.Run;
end.
