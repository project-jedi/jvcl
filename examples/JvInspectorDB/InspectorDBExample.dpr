program InspectorDBExample;

uses
  Forms,
  JvInspectorDBDemoMainFormU in 'JvInspectorDBDemoMainFormU.pas' {JvInspectorDBDemoMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvInspectorDBDemoMainForm, JvInspectorDBDemoMainForm);
  Application.Run;
end.
