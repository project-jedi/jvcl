program JvSearchFileProj;

uses
  Forms,
  fSearchFile in 'fSearchFile.pas' {MainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.
