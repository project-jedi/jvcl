program JvAniProj;

uses
  Forms,
  JvAniMainFormU in 'JvAniMainFormU.pas' {JvAniMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvAniMainForm, JvAniMainForm);
  Application.Run;
end.
