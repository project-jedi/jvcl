program JvFullColorDialogPrj;

uses
  Forms,
  JvFullColorDialogMainForm in 'JvFullColorDialogMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
