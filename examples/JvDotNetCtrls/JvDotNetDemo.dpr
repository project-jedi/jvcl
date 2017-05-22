program JvDotNetDemo;

uses
  Forms,
  JvDotNetDemoMainForm in 'JvDotNetDemoMainForm.pas' {JvDotNetDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JVCL DotNet Controls Demo';
  Application.CreateForm(TJvDotNetDemoMainFrm, JvDotNetDemoMainFrm);
  Application.Run;
end.

