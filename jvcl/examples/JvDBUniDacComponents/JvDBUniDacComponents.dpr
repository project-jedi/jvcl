program JvDBUniDacComponents;

uses
  Forms,
  JvDBUniDacComponentsMainForm in 'JvDBUniDacComponentsMainForm.pas' {Form1},
  JvDynControlEngineJVCL,
  JvDynControlEngineJVCLDB;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
