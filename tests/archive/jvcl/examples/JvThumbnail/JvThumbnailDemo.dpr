program JvThumbnailDemo;

uses
  Forms, JvThumbnailMainFormU, JvThumbnailChildFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvThumbnailMainForm, JvThumbnailMainForm);
  Application.Run;
end.
