program JvSearchFileProj;

uses
  Forms, JvSearchFileMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvSearchFileMainForm, JvSearchFileMainForm);
  Application.Run;
end.
