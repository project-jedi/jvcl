program JvDataEmbeddedProj;

uses
  Forms,
  JvDataEmbeddedMainFormU in 'JvDataEmbeddedMainFormU.pas' {JvDataEmbeddedMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDataEmbeddedMainForm, JvDataEmbeddedMainForm);
  Application.Run;
end.
