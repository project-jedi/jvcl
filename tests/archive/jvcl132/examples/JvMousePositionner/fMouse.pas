unit fMouse;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, JvMousePositionner, JvComponent;
type
  TForm1 = class(TForm)
    JvMousePositionner1: TJvMousePositionner;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Button7: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button7Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
implementation
{$R *.DFM}
procedure TForm1.Button7Click(Sender: TObject);
begin
   self.JvMousePositionner1.ExecuteEx(Point(self.spinedit1.value,self.spinedit2.value));
end;
procedure TForm1.Button4Click(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button1;
   self.JvMousePositionner1.Execute(100,10);
end;
procedure TForm1.Button5Click(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button2;
   self.JvMousePositionner1.Execute(100,10);
end;
procedure TForm1.Button6Click(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button3;
   self.JvMousePositionner1.Execute(100,10);
end;
end.
