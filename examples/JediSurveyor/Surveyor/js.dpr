program Js;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSurveyImpl in '..\common\JvSurveyImpl.pas',
  CommentFrm in 'CommentFrm.pas' {frmComment};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JEDI Surveyor';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
