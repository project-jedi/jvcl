program JvMousePositionnerProj;

uses
  Forms,
  JvMousePositionnerMainFormU in 'JvMousePositionnerMainFormU.pas' {JvMousePositionnerMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvMousePositionnerMainForm, JvMousePositionnerMainForm);
  Application.Run;
end.
