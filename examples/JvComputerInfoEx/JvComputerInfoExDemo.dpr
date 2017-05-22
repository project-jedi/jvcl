program JvComputerInfoExDemo;

uses
  Forms,
  JvComputerInfoExDemoForm in 'JvComputerInfoExDemoForm.pas' {JvComputerInfoExDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvComputerInfoExDemoFrm, JvComputerInfoExDemoFrm);
  Application.Run;
end.
