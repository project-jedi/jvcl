program DSAExamples;

uses
  Forms,
  DSAExamplesMain in 'DSAExamplesMain.pas' {frmDSAExamples},
  DSAExamplesCustom1 in 'DSAExamplesCustom1.pas' {frmDSAExamplesCustomDlg1},
  DSAExamplesCustom2 in 'DSAExamplesCustom2.pas' {frmDSAExamplesCustomDlg2},
  DSAExamplesProgressDlg in 'DSAExamplesProgressDlg.pas' {frmDSAExamplesProgressDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Don''t Show Again (DSA) Examples and tests';
  Application.CreateForm(TfrmDSAExamples, frmDSAExamples);
  Application.Run;
end.
