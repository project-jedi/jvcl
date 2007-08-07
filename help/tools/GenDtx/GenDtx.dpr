program GenDtx;

uses
  Forms,
  MainDlg in 'MainDlg.pas' {frmMain},
  DelphiParser in 'DelphiParser.pas',
  Settings in 'Settings.pas',
  SettingsDlg in 'SettingsDlg.pas' {frmSettings},
  ParserTypes in 'ParserTypes.pas',
  MainCtrl in 'MainCtrl.pas',
  InputDlg in 'InputDlg.pas' {frmInput},
  EditNiceNameDlg in 'EditNiceNameDlg.pas' {frmEditNiceName},
  UnitStatusDlg in 'UnitStatusDlg.pas' {frmUnitStatus},
  DirectoriesDlg in 'DirectoriesDlg.pas' {frmDirectories},
  FilterDlg in 'FilterDlg.pas' {frmFilter},
  VisibilityDlg in 'VisibilityDlg.pas' {frmVisibility},
  ClassStructureDlg in 'ClassStructureDlg.pas' {frmClassStructure},
  EditPasCleanOptionsDlg in 'EditPasCleanOptionsDlg.pas' {frmEditPasCleanOptions},
  ItemFilter in 'ItemFilter.pas',
  DtxRenameU in 'DtxRenameU.pas' {frmDtxRename},
  AskDtxCheckU in 'AskDtxCheckU.pas' {frmAskDtxCheck};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
