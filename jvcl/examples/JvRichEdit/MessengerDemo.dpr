program MessengerDemo;

uses
  Forms,
  MessengerMainFormU in 'MessengerMainFormU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMessengerMainForm, MessengerMainForm);
  Application.Run;
end.
