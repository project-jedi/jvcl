program JvAniProj;

uses
  Forms, JvAniMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvAniMainForm, JvAniMainForm);
  Application.Run;
end.
