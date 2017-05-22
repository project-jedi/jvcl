program JvDBImageDemo;

uses
  Forms,
  JvDBImageDemoMainForm in 'JvDBImageDemoMainForm.pas' {JvDBImageDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBImageDemoMainFrm, JvDBImageDemoMainFrm);
  Application.Run;
end.
