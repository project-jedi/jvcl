program ControlsExample;

uses
  Forms,
  ControlsExampleMainFormU in 'ControlsExampleMainFormU.pas' {ControlsExampleMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TControlsExampleMainForm, ControlsExampleMainForm);
  Application.Run;
end.
