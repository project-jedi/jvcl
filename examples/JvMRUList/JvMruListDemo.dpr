program JvMruListDemo;

uses
  Forms,
  JvMruListMainFormU in 'JvMruListMainFormU.pas' {JvMruListMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvMruListMainForm, JvMruListMainForm);
  Application.Run;
end.
