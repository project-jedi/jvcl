unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, JvComCtrls, JvComponent, JvMarkupLabel, StdCtrls,
  JvExControls;

type
  TForm1 = class(TForm)
    JvMarkupLabel1: TJvMarkupLabel;
    JvTrackBar1: TJvTrackBar;
    Label1: TLabel;
    Edit1: TMemo;
    Button1: TButton;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    JvTrackBar2: TJvTrackBar;
    Label4: TLabel;
    procedure JvTrackBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1Click(nil);
end;

procedure TForm1.JvTrackBarChange(Sender: TObject);
begin
  case (Sender as TJvTrackBar).Tag of
  0: JvMarkupLabel1.Width := JvTrackBar1.Position;
  1: JvMarkupLabel1.Height := JvTrackBar2.Position;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvMarkupLabel1.Text := Edit1.Lines.Text;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  JvMarkupLabel1.AutoSize := CheckBox1.Checked;
  if CheckBox1.Checked then
  begin
    JvTrackBar1.Position := JvMarkupLabel1.Width;
    JvTrackBar2.Position := JvMarkupLabel1.Height;
  end;
end;

end.
