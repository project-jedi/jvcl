program FileDirDemo;

uses
  Forms, FileListBoxMainFormU;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFileListBoxMainForm, FileListBoxMainForm);
  Application.Run;
end.
