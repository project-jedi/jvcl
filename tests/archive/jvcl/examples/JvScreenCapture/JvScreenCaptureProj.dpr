program JvScreenCaptureProj;

uses
  Forms,
  JvScreenCaptureMainFormU in 'JvScreenCaptureMainFormU.pas' {JvScreenCaptureMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvScreenCaptureMainForm, JvScreenCaptureMainForm);
  Application.Run;
end.
