program ExampleJVSegmentedLEDDisplay;

uses
  Forms,
  ExampleJVSegmentedLEDDisplayMain in 'ExampleJVSegmentedLEDDisplayMain.pas' {frmExampleSegmentedLEDDisplayMain},
  ExampleJVSegmentedLEDDisplay7SegMain in 'ExampleJVSegmentedLEDDisplay7SegMain.pas' {fme7SegExamples: TFrame},
  ExampleJVSegmentedLEDDisplay14SegMain in 'ExampleJVSegmentedLEDDisplay14SegMain.pas' {fme14SegExamples: TFrame},
  ExampleJVSegmentedLEDDisplay16SegMain in 'ExampleJVSegmentedLEDDisplay16SegMain.pas' {fme16SegExamples: TFrame};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmExampleSegmentedLEDDisplayMain, frmExampleSegmentedLEDDisplayMain);
  Application.Run;
end.
