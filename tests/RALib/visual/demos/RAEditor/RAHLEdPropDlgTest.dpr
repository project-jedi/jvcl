program RAHLEdPropDlgTest;

uses
  Forms,
  fJvHLEdPropDlgTestMain in 'fJvHLEdPropDlgTestMain.pas' {Form1},
  fJvHLEdPropDlgTestParams in 'fJvHLEdPropDlgTestParams.pas' {MyParams};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
