program JvAviCapDemo;

uses
  Forms,
  JvAviCapDemoFrmU in 'JvAviCapDemoFrmU.pas' {JvAviCapDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAviCapDemoFrm, JvAviCapDemoFrm);
  Application.Run;
end.
