program DtxEdit;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  FileWrapper in 'FileWrapper.pas',
  FileItem in 'FileItem.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
