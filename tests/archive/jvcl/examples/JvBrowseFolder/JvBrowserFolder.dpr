program JvBrowserFolder;

uses
  Forms, JvBrowseFolderMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TJvBrowseFolderMainForm, JvBrowseFolderMainForm);
  Application.Run;
end.
