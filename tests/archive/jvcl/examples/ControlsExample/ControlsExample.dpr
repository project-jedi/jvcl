program ControlsExample;

uses
  Forms, ControlsExampleMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TControlsExampleMainForm, ControlsExampleMainForm);
  Application.Run;
end.
