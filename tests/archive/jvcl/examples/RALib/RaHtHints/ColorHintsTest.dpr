program ColorHintsTest;

uses
  Forms,
  RaHtHintsMainFormU in 'RaHtHintsMainFormU.pas' {RaHtHintsMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TRaHtHintsMainForm, RaHtHintsMainForm);
  Application.Run;
end.
