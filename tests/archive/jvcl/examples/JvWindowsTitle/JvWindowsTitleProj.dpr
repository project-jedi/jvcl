program JvWindowsTitleProj;

uses
  Forms, JvWindowsTitleMainFomU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvWindowsTitleMainForm, JvWindowsTitleMainForm);
  Application.Run;
end.
