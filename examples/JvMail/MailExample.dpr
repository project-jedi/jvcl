program MailExample;

uses
  Forms,
  MailExampleMainFormU in 'MailExampleMainFormU.pas' {MailExampleMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMailExampleMainForm, MailExampleMainForm);
  Application.Run;
end.
