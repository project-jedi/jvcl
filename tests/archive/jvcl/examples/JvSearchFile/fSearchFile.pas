unit fSearchFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvListbox, JvCustomBox, JvDirectoryBox, JvSearchFile,
  JvComponent, JvCtrls
  //, JvCtrls
  ;

type
  TForm1 = class(TForm)
    JvSearchFile1: TJvSearchFile;
    GroupBox1: TGroupBox;
    JvDirectoryBox1: TJvDirectoryBox;
    Button1: TButton;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Label2: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    JvListBox1: TJvListBox;
    procedure JvSearchFile1Found(Sender: TObject; Path: String);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvSearchFile1Found(Sender: TObject; Path: String);
begin
  JvListBox1.Items.Add(Path);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvListBox1.Clear;
  JvSearchFile1.Mask := Edit1.Text;
  JvSearchFile1.Recursive := CheckBox1.Checked;
  JvSearchFile1.Execute(JvDirectoryBox1.Directory);
end;

end.
