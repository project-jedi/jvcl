program ExampleJVSegmentedLEDDisplay;

uses
  Forms,
  ExampleJVSegmentedLEDDisplayMain in 'ExampleJVSegmentedLEDDisplayMain.pas' {frmExampleSegmentedLEDDisplayMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmExampleSegmentedLEDDisplayMain, frmExampleSegmentedLEDDisplayMain);
  Application.Run;
end.
