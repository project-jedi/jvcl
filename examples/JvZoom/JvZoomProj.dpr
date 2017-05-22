program JvZoomProj;

uses
  Forms,
  JvZoomMainFormU in 'JvZoomMainFormU.pas' {JvZoomMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvZoomMainForm, JvZoomMainForm);
  Application.Run;
end.
