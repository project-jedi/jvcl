program JvBrowserFolder;

uses
  Forms,
  JvBrowseFolderMainFormU in 'JvBrowseFolderMainFormU.pas' {JvBrowseFolderMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvBrowseFolderMainForm, JvBrowseFolderMainForm);
  Application.Run;
end.
