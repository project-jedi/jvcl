program JvDialogsDemo;

uses
  Forms,
  fDialogs in 'fDialogs.pas' {JvDialogsDemoFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDialogsDemoFrm, JvDialogsDemoFrm);
  Application.Run;
end.
