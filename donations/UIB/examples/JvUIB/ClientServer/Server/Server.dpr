program Server;

uses
{$IFDEF LINUX}QForms,{$ELSE}Forms,{$ENDIF}
  Main in 'Main.pas' {MainForm},
  RemoteObject in 'RemoteObject.pas',
  RemoteObject_UIB in 'RemoteObject_UIB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
