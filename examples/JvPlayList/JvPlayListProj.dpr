program JvPlayListProj;

uses
  Forms,
  JvPlayListMainFormU in 'JvPlayListMainFormU.pas';

{$R *.RES}

 begin
  Application.Initialize;
  Application.CreateForm(TJvPlayListMainForm, JvPlayListMainForm);
  Application.Run;
end.
