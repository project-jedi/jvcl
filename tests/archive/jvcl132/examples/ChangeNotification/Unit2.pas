unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm2 = class(TForm)
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  FileCtrl;
{$R *.DFM}

procedure TForm2.Button1Click(Sender: TObject);
var S:String;
begin
 S := GetCurrentDir;
 if SelectDirectory(S,[sdAllowCreate, sdPerformCreate, sdPrompt],0) then
   Edit1.Text := S;
end;

end.
