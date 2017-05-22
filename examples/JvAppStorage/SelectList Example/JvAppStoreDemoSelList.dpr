program JvAppStoreDemoSelList;

uses
  Forms,
  JvAppStorageSelListMainFrmU in 'JvAppStorageSelListMainFrmU.pas' {JvAppStorageSelListMainFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJvAppStorageSelListMainFrm, JvAppStorageSelListMainFrm);
  Application.Run;
end.
