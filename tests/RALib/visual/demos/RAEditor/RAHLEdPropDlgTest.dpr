program RAHLEdPropDlgTest;

uses
  Forms,
  fRAHLEdPropDlgTestMain in 'fRAHLEdPropDlgTestMain.pas' {Form1},
  fRAHLEdPropDlgTestParams in 'fRAHLEdPropDlgTestParams.pas' {MyParams};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
