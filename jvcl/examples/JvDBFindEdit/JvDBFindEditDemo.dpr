program JvDBFindEditDemo;

uses
  Forms,
  JvDBFindEditDemoForm in 'JvDBFindEditDemoForm.pas' {JvDBFindEditDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBFindEditDemoFrm, JvDBFindEditDemoFrm);
  Application.Run;
end.
