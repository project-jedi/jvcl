program Client;

uses
{$IFDEF LINUX}QForms,{$ELSE}Forms,{$ENDIF}
  main in 'main.pas' {MainForm},
  RemoteObject_UIB in '../Server/RemoteObject_UIB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
