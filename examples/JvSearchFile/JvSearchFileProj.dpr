program JvSearchFileProj;

uses
  Forms,
  JvSearchFileMainFormU in 'JvSearchFileMainFormU.pas' {JvSearchFileMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvSearchFileMainForm, JvSearchFileMainForm);
  Application.Run;
end.
