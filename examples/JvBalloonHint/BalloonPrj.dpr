program BalloonPrj;

uses
  Forms,
  JvBalloonHintMainFormU in 'JvBalloonHintMainFormU.pas' {JvBalloonHintMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvBalloonHintMainForm, JvBalloonHintMainForm);
  Application.Run;
end.
