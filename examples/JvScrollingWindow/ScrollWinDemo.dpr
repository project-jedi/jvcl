program ScrollWinDemo;

uses
  Forms,
  ScrollWinMainFormU in 'ScrollWinMainFormU.pas' {JvScrollingWindowMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvScrollingWindowMainForm, JvScrollingWindowMainForm);
  Application.Run;
end.
