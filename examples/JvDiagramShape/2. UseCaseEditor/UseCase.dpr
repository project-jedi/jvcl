program UseCase;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainDlg},
  CaptionEditForm in 'CaptionEditForm.pas' {CaptionEditDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
