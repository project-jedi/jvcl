program ControlsExample;

uses
  Forms,
  ControlsMain in 'ControlsMain.pas' {MainFrom};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrom, MainFrom);
  Application.Run;
end.
