program jsr;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSurveyImpl in '..\common\JvSurveyImpl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'JEDI Survey Reporter';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

