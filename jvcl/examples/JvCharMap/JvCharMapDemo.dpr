program JvCharMapDemo;

uses
  Forms,
  JvCharMapMainFrmU in 'JvCharMapMainFrmU.pas' {JvCharMapMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvCharMapMainFrm, JvCharMapMainFrm);
  Application.Run;
end.
