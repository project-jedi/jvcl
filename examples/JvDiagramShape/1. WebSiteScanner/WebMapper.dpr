program WebMapper;

uses
  Forms,
  MainForm in 'MainForm.pas' {MainDlg},
  JimParse in 'JimParse.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainDlg, MainDlg);
  Application.Run;
end.
