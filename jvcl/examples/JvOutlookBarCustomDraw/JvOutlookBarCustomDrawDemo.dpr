program JvOutlookBarCustomDrawDemo;

uses
  Forms,
  JvOutlookBarCustomDrawDemoMainForm in 'JvOutlookBarCustomDrawDemoMainForm.pas' {JvOutlookBarCustomDrawDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvOutlookBarCustomDrawDemoMainFrm, JvOutlookBarCustomDrawDemoMainFrm);
  Application.Run;
end.
