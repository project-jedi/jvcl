program JvStarfieldDemo;

uses
  Forms,
  StarFieldMain in 'StarFieldMain.pas' {StarfieldMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TStarfieldMainForm, StarfieldMainForm);
  Application.Run;
end.
