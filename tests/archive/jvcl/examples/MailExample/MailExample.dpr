program MailExample;

uses
  Forms, MailExampleMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMailExampleMainForm, MailExampleMainForm);
  Application.Run;
end.
