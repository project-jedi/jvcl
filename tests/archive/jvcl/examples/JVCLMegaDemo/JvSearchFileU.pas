unit JvSearchFileU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvSearchFile, StdCtrls, JvListBox, JvCtrls, JvCustomBox,
  JvDirectoryBox;

type
  TJvSearchFileFrm = class(TFrame)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    JvDirectoryBox1: TJvDirectoryBox;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Button1: TButton;
    GroupBox2: TGroupBox;
    JvListBox1: TJvListBox;
    JvSearchFile1: TJvSearchFile;
    procedure Button1Click(Sender: TObject);
    procedure JvSearchFile1Found(Sender: TObject; Path: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvSearchFileFrm.Button1Click(Sender: TObject);
begin
  JvListBox1.Clear;
  JvSearchFile1.Mask := Edit1.Text;
  JvSearchFile1.Recursive := CheckBox1.Checked;
  JvSearchFile1.Execute(JvDirectoryBox1.Directory);
end;

procedure TJvSearchFileFrm.JvSearchFile1Found(Sender: TObject; Path: String);
begin
  JvListBox1.Items.Add(Path);
end;

end.
