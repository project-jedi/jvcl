program ListCombDemo;

uses
  Forms,
  ListCombMainFormU in 'ListCombMainFormU.pas' {ListCombMainForm},
  ListBoxFormU in 'ListBoxFormU.pas' {fmListBox};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TListCombMainForm, ListCombMainForm);
  Application.CreateForm(TfmListBox, fmListBox);
  Application.Run;
end.
