program JvParameterListDemo;

uses
  Forms,
  JvParameterListMainForm in 'JvParameterListMainForm.pas' {JvParameterListDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvParameterListDemoMainFrm, JvParameterListDemoMainFrm);
  Application.Run;
end.
