program ContentScrollerDemo;

uses
  Forms, ContentScrollerMainFormU;  

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TContentScrollerMainForm, ContentScrollerMainForm);
  Application.Run;
end.
