program DSAExamples;

uses
  Forms,
  DSAExamplesCustom1 in 'DSAExamplesCustom1.pas' {frmDSAExamplesCustomDlg1},
  DSAExamplesCustom2 in 'DSAExamplesCustom2.pas' {frmDSAExamplesCustomDlg2},
  DSAExamplesProgressDlg in 'DSAExamplesProgressDlg.pas' {frmDSAExamplesProgressDlg},
  DSADialogsMainFormU in 'DSADialogsMainFormU.pas' {DSADialogsMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Don''t Show Again (DSA) Examples and tests';
  Application.CreateForm(TDSADialogsMainForm, DSADialogsMainForm);
  Application.Run;
end.
