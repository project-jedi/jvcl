unit JvSearchFileU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, StdCtrls, JvListBox, JvCtrls, JvSearchFiles, Mask, JvToolEdit;

type
  TJvSearchFileFrm = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edDirectory: TJvDirectoryEdit;
    chkRecursive: TCheckBox;
    edFileMask: TEdit;
    btnSearch: TButton;
    GroupBox2: TGroupBox;
    lbFoundFiles: TJvListBox;
    JvSearchFile1: TJvSearchFiles;
    procedure btnSearchClick(Sender: TObject);
    procedure JvSearchFile1FindFile(Sender: TObject; const AName: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvSearchFileFrm.btnSearchClick(Sender: TObject);
begin
  lbFoundFiles.Clear;
  JvSearchFile1.RootDirectory := edDirectory.Text;
  JvSearchFile1.FileParams.FileMasks.Text := edFileMask.Text;
  if chkRecursive.Checked then
    JvSearchFile1.DirOption := doIncludeSubDirs
  else
    JvSearchFile1.DirOption := doExcludeSubDirs;
  JvSearchFile1.Search;
end;

procedure TJvSearchFileFrm.JvSearchFile1FindFile(Sender: TObject;
  const AName: string);
begin
  lbFoundFiles.Items.Add(AName);
end;

end.

