program InspectorSimpleExample;

uses
  Forms,
  InspectorSimpleExampleMain in 'InspectorSimpleExampleMain.pas' {SimpleMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSimpleMainForm, SimpleMainForm);
  Application.Run;
end.
