program InspectorDBExample;

uses
  Forms, JvInspectorDBDemoMainFormU;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvInspectorDBDemoMainForm, JvInspectorDBDemoMainForm);
  Application.Run;
end.
