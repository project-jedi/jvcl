program JvMruList;

uses
  Forms, JvMruListMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvMruListMainForm, JvMruListMainForm);
  Application.Run;
end.
