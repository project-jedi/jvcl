program ProviderTest1;

uses
  Forms,
  ProviderTest1Main in 'ProviderTest1Main.pas' {frmTestProviders};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTestProviders, frmTestProviders); 
  Application.Run;
end.                                                                              
