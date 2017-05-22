program JvDBSearchDemo;

uses
  Forms,
  JvDBSearchDemoMainForm in 'JvDBSearchDemoMainForm.pas' {JvDBSearchDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBSearchDemoMainFrm, JvDBSearchDemoMainFrm);
  Application.Run;
end.
