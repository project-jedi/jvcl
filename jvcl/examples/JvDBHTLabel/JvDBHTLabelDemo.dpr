program JvDBHTLabelDemo;

uses
  Forms,
  JvDBHTLabelDemoMainForm in 'JvDBHTLabelDemoMainForm.pas' {JvDBHTLabelDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvDBHTLabelDemoMainFrm, JvDBHTLabelDemoMainFrm);
  Application.Run;
end.
