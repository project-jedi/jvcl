program JvTranslatorProj;

uses
  Forms, JvTranslatorMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvTranslatorMainForm, JvTranslatorMainForm);
  Application.Run;
end.
