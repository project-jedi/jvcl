program JvAppStoreDemoSubStorage;

uses
  Forms,
  JvAppStorageSubStorageMainFrm in 'JvAppStorageSubStorageMainFrm.pas' {JvAppStorageSubStorageMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAppStorageSubStorageMainForm, JvAppStorageSubStorageMainForm);
  Application.Run;
end.
