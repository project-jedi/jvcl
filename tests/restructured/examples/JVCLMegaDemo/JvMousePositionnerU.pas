unit JvMousePositionnerU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvMousePositionner, StdCtrls, Spin, ExtCtrls, JvCaptionPanel;

type
  TJvMousePositionnerFrm = class(TFrame)
    JvMousePositionner1: TJvMousePositionner;
    JvCaptionPanel1: TJvCaptionPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Button7: TButton;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    JvMouse: TButton;
    procedure Button7Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure JvMouseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvMousePositionnerFrm.Button7Click(Sender: TObject);
begin
   self.JvMousePositionner1.ExecuteEx(Point(self.spinedit1.value,self.spinedit2.value));
end;

procedure TJvMousePositionnerFrm.Button4Click(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button1;
   self.JvMousePositionner1.Execute(100,10);
end;

procedure TJvMousePositionnerFrm.Button5Click(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button2;
   self.JvMousePositionner1.Execute(100,10);
end;

procedure TJvMousePositionnerFrm.JvMouseClick(Sender: TObject);
begin
   self.JvMousePositionner1.Control:=Button3;
   self.JvMousePositionner1.Execute(100,10);
end;

end.
