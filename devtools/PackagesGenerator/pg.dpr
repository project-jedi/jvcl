program pg;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  FileUtils in 'FileUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
