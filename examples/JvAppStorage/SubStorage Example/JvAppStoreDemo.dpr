program JvAppStoreDemo;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainFormDlg};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFormDlg, MainFormDlg);
  Application.Run;
end.
