program GenDtx;

uses
  Forms,
  MainDlg in 'MainDlg.pas' {Form1},
  DelphiParser in 'DelphiParser.pas',
  Settings in 'Settings.pas',
  SettingsDlg in 'SettingsDlg.pas' {frmSettings},
  ParserTypes in 'ParserTypes.pas',
  MainCtrl in 'MainCtrl.pas',
  InputDlg in 'InputDlg.pas' {frmInput},
  EditNiceNameDlg in 'EditNiceNameDlg.pas' {frmEditNiceName},
  UnitStatusDlg in 'UnitStatusDlg.pas' {frmUnitStatus};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
