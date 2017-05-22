unit JvVersionControlActionsMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl, JvVersionControlActions, ActnList,
  JvVersionControlActionsEngine, Menus, Buttons, ImgList, JvActionsEngine, Mask, JvExMask, JvToolEdit, ExtCtrls,
  JvExExtCtrls, JvExtComponent, JvPanel, JvExStdCtrls, JvCombobox, JvDriveCtrls, JvListBox;

type
  TjvVersionControlActionMemoEngine = class(TjvVersionControlActionEngine)
  private
  protected
  public
    function GetFilename(aActionComponent: TComponent): string; override;
    function SupportsComponent(aActionComponent: TComponent): Boolean; override;
    function SupportsGetFileName(aActionComponent: TComponent): Boolean; override;
  end;
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    VersionControl1: TMenuItem;
    UpdateMenu: TMenuItem;
    CommitSandboxMenu: TMenuItem;
    CommitMenu: TMenuItem;
    UpdateSandboxMenu: TMenuItem;
    N1: TMenuItem;
    t1menu: TMenuItem;
    t2menu: TMenuItem;
    CommitButton: TBitBtn;
    ImageList1: TImageList;
    JvVersionControlActionList: TJvVersionControlActionList;
    JvVersionControlAddAction1: TJvVersionControlAddAction;
    JvVersionControlAddSandboxAction1: TJvVersionControlAddSandboxAction;
    JvVersionControlExploreAction1: TJvVersionControlExploreAction;
    JvVersionControlDiffAction1: TJvVersionControlDiffAction;
    JvVersionControlContextMenuAction1: TJvVersionControlContextMenuAction;
    JvVersionControlCommitSandboxAction1: TJvVersionControlCommitSandboxAction;
    JvVersionControlCommitAction1: TJvVersionControlCommitAction;
    JvVersionControlCheckoutSandboxAction1: TJvVersionControlCheckoutSandboxAction;
    JvVersionControlBranchSandboxAction1: TJvVersionControlBranchSandboxAction;
    JvVersionControlBranchAction1: TJvVersionControlBranchAction;
    JvVersionControlBlameAction1: TJvVersionControlBlameAction;
    JvVersionControlGraphAction1: TJvVersionControlGraphAction;
    JvVersionControlLogAction1: TJvVersionControlLogAction;
    JvVersionControlLogSandboxAction1: TJvVersionControlLogSandboxAction;
    JvVersionControlExploreSandboxAction1: TJvVersionControlExploreSandboxAction;
    JvVersionControlLockAction1: TJvVersionControlLockAction;
    JvVersionControlRenameAction1: TJvVersionControlRenameAction;
    JvVersionControlRepoBrowserAction1: TJvVersionControlRepoBrowserAction;
    JvVersionControlRevertAction1: TJvVersionControlRevertAction;
    JvVersionControlStatusAction1: TJvVersionControlStatusAction;
    JvVersionControlTagAction1: TJvVersionControlTagAction;
    JvVersionControlUnlockAction1: TJvVersionControlUnlockAction;
    JvVersionControlUpdateToAction1: TJvVersionControlUpdateToAction;
    JvVersionControlUpdateAction1: TJvVersionControlUpdateAction;
    JvVersionControlMergeAction1: TJvVersionControlMergeAction;
    JvVersionControlPropertiesAction1: TJvVersionControlPropertiesAction;
    JvVersionControlLockSandboxAction1: TJvVersionControlLockSandboxAction;
    JvVersionControlMergeSandboxAction1: TJvVersionControlMergeSandboxAction;
    JvVersionControlPropertiesSandboxAction1: TJvVersionControlPropertiesSandboxAction;
    JvVersionControlRenameSandboxAction1: TJvVersionControlRenameSandboxAction;
    JvVersionControlRevertSandboxAction1: TJvVersionControlRevertSandboxAction;
    JvVersionControlStatusSandboxAction1: TJvVersionControlStatusSandboxAction;
    JvVersionControlTagSandboxAction1: TJvVersionControlTagSandboxAction;
    JvVersionControlUpdateSandboxAction1: TJvVersionControlUpdateSandboxAction;
    JvVersionControlUnlockSandboxAction1: TJvVersionControlUnlockSandboxAction;
    JvVersionControlUpdateSandboxToAction1: TJvVersionControlUpdateSandboxToAction;
    Branch1: TMenuItem;
    Checkout1: TMenuItem;
    Commit1: TMenuItem;
    Commit2: TMenuItem;
    ContextMenurightclick1: TMenuItem;
    Diff1: TMenuItem;
    Explore1: TMenuItem;
    Explore2: TMenuItem;
    RevisionGraph1: TMenuItem;
    Lock1: TMenuItem;
    Lock2: TMenuItem;
    Log1: TMenuItem;
    Log2: TMenuItem;
    Merge1: TMenuItem;
    Merge2: TMenuItem;
    Properties1: TMenuItem;
    Properties2: TMenuItem;
    Rename1: TMenuItem;
    RenameSandbox1: TMenuItem;
    RepositoryBrowser1: TMenuItem;
    Revert1: TMenuItem;
    Revert2: TMenuItem;
    Status1: TMenuItem;
    Status2: TMenuItem;
    ag1: TMenuItem;
    ag2: TMenuItem;
    Unlock1: TMenuItem;
    Unlock2: TMenuItem;
    Update1: TMenuItem;
    Updateto1: TMenuItem;
    Updateto2: TMenuItem;
    Updateto3: TMenuItem;
    CommitSandboxButton: TBitBtn;
    UpdateButton: TBitBtn;
    UpdateSandboxButton: TBitBtn;
    ButtonPanel: TJvPanel;
    Panel1: TPanel;
    JvDirectoryEdit: TJvDirectoryEdit;
    Memo1: TMemo;
    DirectoryListBox1: TJvDirectoryListBox;
    DriveComboBox1: TJvDriveCombo;
    Label1: TLabel;
    FileListBox1: TFileListBox;
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure DriveComboBox1Change(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JvDirectoryEditChange(Sender: TObject);
  private
    { Private declarations }
  protected
  public
    procedure CreateButtons;
    { Public declarations }
  end;


var
  Form1: TForm1;

implementation

uses JclVersionControl,
  JclVersionCtrlSVNImpl,
  JclVersionCtrlcvsImpl,
  JclVersionCtrlGITImpl,
  JvVersionControlActionsEngineFileListBox;

{$R *.dfm}

procedure TForm1.CreateButtons;
var
  Btn: TButton;
  I: Integer;
begin
  for I := 0 to JvVersionControlActionList.ActionCount-1 do
  begin
    Btn := TButton.Create(self);
    Btn.Parent := ButtonPanel;
    Btn.Action := JvVersionControlActionList.Actions[i];
  end;
end;

procedure TForm1.DirectoryListBox1Change(Sender: TObject);
begin
  FileListBox1.Directory := DirectoryListBox1.Directory;
  JvDirectoryEdit.Directory := DirectoryListBox1.Directory;
end;

procedure TForm1.DriveComboBox1Change(Sender: TObject);
begin
//  DirectoryListBox1.Drive := DriveComboBox1.Drive;
//  FileListBox1.Drive := DriveComboBox1.Drive;
//FileListBox1.Directory := DirectoryListBox1.Directory;
//JvDirectoryEdit.Directory := DirectoryListBox1.Directory;
end;

procedure TForm1.FileListBox1Change(Sender: TObject);
begin
  if FileExists(FileListBox1.FileName) and (ExtractFileExt (FileListBox1.FileName) = '.pas') then
    Memo1.Lines.LoadFromFile(FileListBox1.FileName)
  else
    Memo1.Clear;
  if Assigned(JvVersionControlActionList) and Assigned(JvVersionControlActionList.Images) and Assigned(JvVersionControlExploreSandboxAction1.CurrentPlugin) then
    Label1.Caption := JvVersionControlExploreSandboxAction1.CurrentPlugin.Name+' Repository - Action Count : '+Inttostr(JvVersionControlActionList.Images.Count)
  else
    Label1.Caption := 'No Version Control System Assigned';
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  CreateButtons;
  DirectoryListBox1Change(nil);
end;

procedure TForm1.JvDirectoryEditChange(Sender: TObject);
begin
  DirectoryListBox1.Directory := JvDirectoryEdit.Directory;
end;

function TjvVersionControlActionMemoEngine.SupportsComponent(aActionComponent:
    TComponent): Boolean;
begin
  Result := aActionComponent is TFilelistBox;
end;

function TjvVersionControlActionMemoEngine.GetFilename(aActionComponent:
    TComponent): string;
begin
  Result := TFilelistBox(aActionComponent).FileName;
end;

function TjvVersionControlActionMemoEngine.SupportsGetFileName(
    aActionComponent: TComponent): Boolean;
begin
  Result := True;
end;


end.
