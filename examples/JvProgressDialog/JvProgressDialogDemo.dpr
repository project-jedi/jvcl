program JvProgressDialogDemo;

uses
  Forms,
  JvProgressDialogMain in 'JvProgressDialogMain.pas' {frmProgressDialogDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmProgressDialogDemo, frmProgressDialogDemo);
  Application.Run;
end.
