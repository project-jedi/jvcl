program jsr;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSurveyImpl in '..\common\JvSurveyImpl.pas',
  CommentsFrm in 'CommentsFrm.pas' {frmComments};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'JEDI Survey Reporter';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

