program JvComboListBoxDemo;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {frmMain},
  DropFrm in 'DropFrm.pas' {frmDrop};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.

