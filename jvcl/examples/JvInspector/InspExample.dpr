program InspExample;

uses
  Forms,
  InspectorExampleMain in 'InspectorExampleMain.pas' {frmInspector},
  InspectorExampleTestForm in 'InspectorExampleTestForm.pas' {frmTest};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmInspector, frmInspector);
  Application.Run;
end.
