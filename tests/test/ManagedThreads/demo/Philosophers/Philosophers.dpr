program Philosophers;

uses
  Forms,
  fPhilosophers in 'fPhilosophers.pas' {frmDiningPhilosophers};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDiningPhilosophers, frmDiningPhilosophers);
  Application.Run;
end.
