program UseCase;

uses
  Forms,
  UseCaseDemoMainForm in 'UseCaseDemoMainForm.pas' {UseCaseDemoMainFrm},
  CaptionEditForm in 'CaptionEditForm.pas' {CaptionEditDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TUseCaseDemoMainFrm, UseCaseDemoMainFrm);
  Application.Run;
end.
