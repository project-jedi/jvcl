program JvTranslatorProj;

uses
  Forms,
  JvTranslatorMainFormU in 'JvTranslatorMainFormU.pas' {JvTranslatorMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvTranslatorMainForm, JvTranslatorMainForm);
  Application.Run;
end.
