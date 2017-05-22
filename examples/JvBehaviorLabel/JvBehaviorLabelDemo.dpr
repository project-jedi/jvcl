program JvBehaviorLabelDemo;

uses
  Forms,
  JvBehaviorLblMainFrmU in 'JvBehaviorLblMainFrmU.pas' {JvBehaviorLblMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvBehaviorLblMainFrm, JvBehaviorLblMainFrm);
  Application.Run;
end.
