program Balls;

uses
  Forms,
  fBalls in 'fBalls.pas' {fBouncingBalls};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfBouncingBalls, fBouncingBalls);
  Application.Run;
end.
