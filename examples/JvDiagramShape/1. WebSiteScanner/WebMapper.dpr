program WebMapper;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainDlg},
  JimParse;

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
