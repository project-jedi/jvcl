program JvScreenCaptureProj;

uses
  Forms, JvScreenCaptureMainFormU; 

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvScreenCaptureMainForm, JvScreenCaptureMainForm);
  Application.Run;
end.
