unit DirectoriesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, StdCtrls,

  Settings, JvComponent, JvBaseDlg, JvBrowseFolder;

type
  TfrmDirectories = class(TForm)
    lblRunTimePasDirDesc: TLabel;
    edtRunTimePasDir: TEdit;
    btnRunTimePasDir: TButton;
    lblDesignTimePasDir: TLabel;
    edtDesignTimePasDir: TEdit;
    btnDesignTimePasDir: TButton;
    lblOutDirDesc: TLabel;
    edtGeneratedDtxDir: TEdit;
    btnGeneratedDtxDir: TButton;
    Label1: TLabel;
    edtRealDtxDir: TEdit;
    btnRealDtxDir: TButton;
    Label2: TLabel;
    edtPackageDir: TEdit;
    btnPackageDir: TButton;
    chbOverwriteExisting: TCheckBox;
    btnApply: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    ActionList1: TActionList;
    actCancel: TAction;
    actOK: TAction;
    actApply: TAction;
    actRealDtxDir: TAction;
    actRunTimePasDir: TAction;
    actDesignTimePasDir: TAction;
    actGeneratedDtxDir: TAction;
    actPackageDir: TAction;
    JvBrowseForFolderDialog1: TJvBrowseForFolderDialog;
    Label3: TLabel;
    edtRootDir: TEdit;
    chbUseRootDir: TCheckBox;
    btnRootDir: TButton;
    actRootDir: TAction;
    Label4: TLabel;
    edtRootDelphiDir: TEdit;
    btnRootDelphiDir: TButton;
    actRootDelphiDir: TAction;
    procedure actCancelExecute(Sender: TObject);
    procedure actOKExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure actRunTimePasDirExecute(Sender: TObject);
    procedure actDesignTimePasDirExecute(Sender: TObject);
    procedure actGeneratedDtxDirExecute(Sender: TObject);
    procedure actRealDtxDirExecute(Sender: TObject);
    procedure actPackageDirExecute(Sender: TObject);
    procedure actRootDirExecute(Sender: TObject);
    procedure chbUseRootDirClick(Sender: TObject);
    procedure edtRootDirChange(Sender: TObject);
    procedure actRootDelphiDirExecute(Sender: TObject);
  private
    FSettings: TSettings;
  protected
    procedure Init;
    procedure Final;
    procedure Uitvoeren;

    procedure SaveFiles;
    procedure UpdateUseRootDir;
    procedure UpdateDirs;
  public
    class procedure Execute;
  end;

implementation

{$R *.dfm}
//=== TfrmDirectories ========================================================

procedure TfrmDirectories.actApplyExecute(Sender: TObject);
begin
  Uitvoeren;
end;

procedure TfrmDirectories.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDirectories.actDesignTimePasDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtDesignTimePasDir.Text;
    if Execute then
      edtDesignTimePasDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actGeneratedDtxDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtGeneratedDtxDir.Text;
    if Execute then
      edtGeneratedDtxDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actOKExecute(Sender: TObject);
begin
  Uitvoeren;
  Close;
end;

procedure TfrmDirectories.actPackageDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtPackageDir.Text;
    if Execute then
      edtPackageDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actRealDtxDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtRealDtxDir.Text;
    if Execute then
      edtRealDtxDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actRootDelphiDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtRootDelphiDir.Text;
    if Execute then
      edtRootDelphiDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actRootDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtRootDir.Text;
    if Execute then
      edtRootDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.actRunTimePasDirExecute(Sender: TObject);
begin
  with JvBrowseForFolderDialog1 do
  begin
    Directory := edtRunTimePasDir.Text;
    if Execute then
      edtRunTimePasDir.Text := Directory;
  end;
end;

procedure TfrmDirectories.chbUseRootDirClick(Sender: TObject);
begin
  UpdateUseRootDir;
end;

procedure TfrmDirectories.edtRootDirChange(Sender: TObject);
begin
  if chbUseRootDir.Checked then
    UpdateDirs;
end;

class procedure TfrmDirectories.Execute;
begin
  with TfrmDirectories.Create(Application) do
  try
    Init;
    try
      ShowModal;
    finally
      Final;
    end;
  finally
    Free;
  end;
end;

procedure TfrmDirectories.Final;
begin
  FSettings.Free;
end;

procedure TfrmDirectories.Init;
begin
  FSettings := TSettings.Create;
  FSettings.Assign(TSettings.Instance);

  with FSettings do
  begin
    edtRunTimePasDir.Text := RunTimePasDir;
    edtDesignTimePasDir.Text := DesignTimePasDir;
    edtGeneratedDtxDir.Text := GeneratedDtxDir;
    edtRealDtxDir.Text := RealDtxDir;
    edtPackageDir.Text := PackageDir;
    edtRootDir.Text := RootDir;
    edtRootDelphiDir.Text := DelphiRootSourceDir;
    chbOverwriteExisting.Checked := OverwriteExisting;
    chbUseRootDir.Checked := UseRootDir;
  end;

  UpdateUseRootDir;
end;

procedure TfrmDirectories.SaveFiles;
begin
  with FSettings do
  begin
    DelphiRootSourceDir := edtRootDelphiDir.Text;
    DesignTimePasDir := edtDesignTimePasDir.Text;
    GeneratedDtxDir := edtGeneratedDtxDir.Text;
    OverwriteExisting := chbOverwriteExisting.Checked;
    PackageDir := edtPackageDir.Text;
    RealDtxDir := edtRealDtxDir.Text;
    RootDir := edtRootDir.Text;
    RunTimePasDir := edtRunTimePasDir.Text;
    UseRootDir := chbUseRootDir.Checked;
  end;
end;

procedure TfrmDirectories.Uitvoeren;
begin
  SaveFiles;

  with TSettings.Instance do
  begin
    Assign(FSettings);
    SaveSettings;
  end;
end;

procedure TfrmDirectories.UpdateDirs;
var
  RootDir: string;
begin
  RootDir := IncludeTrailingPathDelimiter(edtRootDir.Text);
  edtRunTimePasDir.Text := RootDir + 'dev\JVCL3\run';
  edtDesignTimePasDir.Text := RootDir + 'dev\JVCL3\design';
  edtGeneratedDtxDir.Text := RootDir + 'dev\help\Gen';
  edtRealDtxDir.Text := RootDir + 'dev\help';
  edtPackageDir.Text := RootDir + 'dev\JVCL3\packages\d7';
end;

procedure TfrmDirectories.UpdateUseRootDir;
var
  UseRootDir: Boolean;
begin
  UseRootDir := chbUseRootDir.Checked;

  edtRunTimePasDir.Enabled := not UseRootDir;
  edtDesignTimePasDir.Enabled := not UseRootDir;
  edtGeneratedDtxDir.Enabled := not UseRootDir;
  edtRealDtxDir.Enabled := not UseRootDir;
  edtPackageDir.Enabled := not UseRootDir;
  btnRunTimePasDir.Enabled := not UseRootDir;
  btnDesignTimePasDir.Enabled := not UseRootDir;
  btnGeneratedDtxDir.Enabled := not UseRootDir;
  btnRealDtxDir.Enabled := not UseRootDir;
  btnPackageDir.Enabled := not UseRootDir;

  if UseRootDir then
    UpdateDirs;
end;

end.
