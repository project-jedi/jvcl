unit fMru;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvMru, Spin, JvComponent;

type
  TForm1 = class(TForm)
    JvMruList1: TJvMruList;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label2: TLabel;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure JvMruList1EnumText(Sender: TObject; Value: String;
      Index: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
   self.JvMruList1.SubKey:=self.Edit1.text;
   self.JvMruList1.MaxItems:=self.SpinEdit1.Value;
   self.button2.enabled:=true;
   self.button3.enabled:=true;
   self.button4.enabled:=true;
   self.button5.enabled:=true;
   Button2Click(self);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   self.ListBox1.Clear;
   self.JvMruList1.EnumItems;
end;

procedure TForm1.JvMruList1EnumText(Sender: TObject; Value: String;
  Index: Integer);
begin
   self.ListBox1.items.add(Value);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   self.ListBox1.clear;
   self.JvMruList1.GetMostRecentItem;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
   self.JvMruList1.DeleteString;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  st:string;
begin
   st:=InputBox('','Enter text','');
   if st<>'' then
      self.JvMruList1.AddString(st);
end;

end.
