program JvFullColorCircleDialogPrj;

uses
  Forms,
  JvFullColorCircleDialogMainForm in 'JvFullColorCircleDialogMainForm.pas' {JvFullColorCircleDlgMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvFullColorCircleDlgMainFrm, JvFullColorCircleDlgMainFrm);
  Application.Run;
end.
