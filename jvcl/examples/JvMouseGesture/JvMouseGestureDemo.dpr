program JvMouseGestureDemo;

uses
  Forms,
  uJvMouseGesture in 'uJvMouseGesture.pas' {JvMouseGestureDemoMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvMouseGestureDemoMainFrm, JvMouseGestureDemoMainFrm);
  Application.Run;
end.
