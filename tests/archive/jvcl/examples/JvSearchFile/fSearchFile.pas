unit fSearchFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvListbox, JvComponent, JvCtrls, JvSearchFiles, Mask, JvToolEdit;

type
  TForm1 = class(TForm)
    JvSearchFile1: TJvSearchFiles;
    GroupBox1: TGroupBox;
    JvDirectoryBox1: TJvDirectoryEdit;
    btnSearch: TButton;
    Label1: TLabel;
    chkRecursive: TCheckBox;
    Label2: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    lbFoundFiles: TJvListBox;
    btnCancel: TButton;
    procedure btnSearchClick(Sender: TObject);
    procedure JvSearchFile1FindFile(Sender: TObject; const AName: string);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnSearchClick(Sender: TObject);
begin
  btnSearch.Enabled := false;
  btnCancel.Enabled := true;
  Screen.Cursor := crHourGlass;
  try
    lbFoundFiles.Clear;
    JvSearchFile1.FileParams.FileMasks.Text := Edit1.Text;
    if chkRecursive.Checked then
      JvSearchFile1.DirOption := doIncludeSubDirs
    else
      JvSearchFile1.DirOption := doExcludeInvalidDirs;
    JvSearchFile1.RootDirectory := JvDirectoryBox1.EditText;
    JvSearchFile1.Search;
  finally
    btnSearch.Enabled := true;
    btnCancel.Enabled := false;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.JvSearchFile1FindFile(Sender: TObject;
  const AName: string);
begin
  lbFoundFiles.Items.Add(AName);
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
  JvSearchFile1.Abort;
  btnCancel.Enabled := false;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  btnCancel.Click;
end;

end.

