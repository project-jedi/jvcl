program ScrollWinDemo;

uses
  Forms, ScrollWinMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvScrollingWindowMainForm, JvScrollingWindowMainForm);
  Application.Run;
end.
