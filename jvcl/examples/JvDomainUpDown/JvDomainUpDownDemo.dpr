program JvDomainUpDownDemo;

uses
  Forms,
  JvDomainUpDownDemoMainForm in 'JvDomainUpDownDemoMainForm.pas' {JvDomainUpDownDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDomainUpDownDemoMainFrm, JvDomainUpDownDemoMainFrm);
  Application.Run;
end.
