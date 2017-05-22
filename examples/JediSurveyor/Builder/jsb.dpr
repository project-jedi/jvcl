program jsb;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  JvSurveyImpl in '..\common\JvSurveyImpl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

