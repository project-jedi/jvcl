program JvFullColorCircleDialogPrj;

uses
  Forms,
  JvFullColorCircleDialogMainForm in 'JvFullColorCircleDialogMainForm.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
