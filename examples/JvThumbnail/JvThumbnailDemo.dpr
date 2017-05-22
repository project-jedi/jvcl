program JvThumbnailDemo;

uses
  Forms,
  JvThumbnailMainFormU in 'JvThumbnailMainFormU.pas' {JvThumbnailMainForm},
  JvThumbnailChildFormU in 'JvThumbnailChildFormU.pas' {JvThumbnailChildForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvThumbnailMainForm, JvThumbnailMainForm);
  Application.CreateForm(TJvThumbnailChildForm, JvThumbnailChildForm);
  Application.Run;
end.
