program JvDynControlDatabase;

uses
  Forms,
  JvDynControlDatabaseMainForm in 'JvDynControlDatabaseMainForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
