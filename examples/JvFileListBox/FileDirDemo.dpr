program FileDirDemo;

uses
  Forms,
  FileListBoxMainFormU in 'FileListBoxMainFormU.pas' {FileListBoxMainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFileListBoxMainForm, FileListBoxMainForm);
  Application.Run;
end.
