program GenDonators;

uses
  Forms,
  main_donators in 'main_donators.pas' {frmGenDonate};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGenDonate, frmGenDonate);
  Application.Run;
end.
