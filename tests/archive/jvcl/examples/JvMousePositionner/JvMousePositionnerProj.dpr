program JvMousePositionnerProj;

uses
  Forms, JvMousePositionnerMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvMousePositionnerMainForm, JvMousePositionnerMainForm);
  Application.Run;
end.
