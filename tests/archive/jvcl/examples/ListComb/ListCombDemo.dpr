program ListCombDemo;

uses
  Forms, ListCombMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TListCombMainForm, ListCombMainForm);
  Application.Run;
end.
