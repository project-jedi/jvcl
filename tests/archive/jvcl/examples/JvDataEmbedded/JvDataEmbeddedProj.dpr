program JvDataEmbeddedProj;

uses
  Forms, JvDataEmbeddedMainFormU;
  
{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvDataEmbeddedMainForm, JvDataEmbeddedMainForm);
  Application.Run;
end.
