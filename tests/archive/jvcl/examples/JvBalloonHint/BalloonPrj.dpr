program BalloonPrj;

uses
  Forms, JvBalloonHintMainFormU;
  
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvBalloonHintMainForm, JvBalloonHintMainForm);
  Application.Run;
end.
