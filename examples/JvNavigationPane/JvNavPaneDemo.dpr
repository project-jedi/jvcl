program JvNavPaneDemo;

uses
  Forms,
  JvNavPaneDemoMainForm in 'JvNavPaneDemoMainForm.pas' {JvNavPaneDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvNavPaneDemoMainFrm, JvNavPaneDemoMainFrm);
  Application.Run;
end.
