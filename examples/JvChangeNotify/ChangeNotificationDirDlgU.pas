unit ChangeNotificationDirDlgU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvChangeNotify;

type
  TChangeNotificationDirDlg = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    cbAttributes: TCheckBox;
    cbDirNames: TCheckBox;
    cbFileNames: TCheckBox;
    cbSize: TCheckBox;
    cbWrite: TCheckBox;
    cbSubTrees: TCheckBox;
    btnOK: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
  public
    class function Execute(var Directory: string; var Options: TJvChangeActions; var IncludeSubDirs: boolean): boolean;
  end;


implementation
uses
  FileCtrl;
  
{$R *.DFM}

procedure TChangeNotificationDirDlg.Button1Click(Sender: TObject);
var S: string;
begin
  S := GetCurrentDir;
  if SelectDirectory(S, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    Edit1.Text := S;
end;

class function TChangeNotificationDirDlg.Execute(var Directory: string;
  var Options: TJvChangeActions; var IncludeSubDirs: boolean): boolean;
var f: TChangeNotificationDirDlg;
begin
  f := self.Create(Application);
  with f do
  try
    Edit1.Text := Directory;
    cbFileNames.Checked := caChangeFileName in Options;
    cbAttributes.Checked := caChangeAttributes in Options;
    cbDirNames.Checked := caChangeDirName in Options;
    cbSize.Checked := caChangeSize in Options;
    cbWrite.Checked := caChangeLastWrite in Options;
    cbSubTrees.Checked := IncludeSubDirs;
    Result := ShowModal = mrOK;
    if Result then
    begin
      Directory := Edit1.Text;
      Options := [];
      if cbFileNames.Checked then
        Include(Options, caChangeFileName);
      if cbAttributes.Checked then
        Include(Options, caChangeAttributes);
      if cbDirNames.Checked then
        Include(Options, caChangeDirName);
      if cbSize.Checked then
        Include(Options, caChangeSize);
      if cbWrite.Checked then
        Include(Options, caChangeLastWrite);
      IncludeSubDirs := cbSubTrees.Checked;
    end;
  finally
    f.Free;
  end;

end;

end.

