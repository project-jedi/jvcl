program JvPanelDemo;

uses
  Forms,
  JvPanelDemoFrm in 'JvPanelDemoFrm.pas' {JvPanelDemoMainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvPanelDemoMainFrm, JvPanelDemoMainFrm);
  Application.Run;
end.

