program JvMousePositionnerProj;

uses
  Forms,
  fMouse in 'fMouse.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
