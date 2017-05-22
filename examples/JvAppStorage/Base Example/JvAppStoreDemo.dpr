program JvAppStoreDemo;

uses
  Forms,
  JvAppStorageBaseMainFrmU in 'JvAppStorageBaseMainFrmU.pas' {JvAppStorageBaseMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAppStorageBaseMainFrm, JvAppStorageBaseMainFrm);
  Application.Run;
end.
