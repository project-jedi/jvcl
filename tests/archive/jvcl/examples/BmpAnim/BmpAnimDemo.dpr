program BmpAnimDemo;

uses
  Forms, BmpAnimMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TBmpAnimMainForm, BmpAnimMainForm);
  Application.Run;
end.
