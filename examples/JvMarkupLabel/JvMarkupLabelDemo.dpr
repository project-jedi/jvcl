program JvMarkupLabelDemo;

uses
  Forms,
  JvMarkupLabelDemoMainForm in 'JvMarkupLabelDemoMainForm.pas' {JvMarkupLabelDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvMarkupLabelDemoMainFrm, JvMarkupLabelDemoMainFrm);
  Application.Run;
end.
