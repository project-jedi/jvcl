program JvZoomProj;

uses
  Forms, JvZoomMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvZoomMainForm, JvZoomMainForm);
  Application.Run;
end.
