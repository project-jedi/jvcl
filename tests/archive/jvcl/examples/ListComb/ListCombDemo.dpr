program ListCombDemo;

uses
  Forms,
  ListCombMainFormU in 'ListCombMainFormU.pas' {ListCombMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TListCombMainForm, ListCombMainForm);
  Application.Run;
end.
