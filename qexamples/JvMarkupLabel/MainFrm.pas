{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

unit MainFrm;

interface

uses
  Types, QWindows, QMessages, SysUtils, Classes, QGraphics, QControls, QForms,
  QDialogs, QComCtrls, JvQComCtrls, JvQComponent, JvQMarkupLabel, QStdCtrls,
  JvQExControls, JvQExComCtrls, JvQExStdCtrls, JvQGroupBox;

type
  TForm1 = class(TForm)
    JvMarkupLabel1: TJvMarkupLabel;
    Label1: TLabel;
    Edit1: TMemo;
    Button1: TButton;
    Label4: TLabel;
    JvGroupBox1: TJvGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    JvTrackBar1: TJvTrackBar;
    CheckBox1: TCheckBox;
    JvTrackBar2: TJvTrackBar;
    procedure JvTrackBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
    procedure RecalibrateTrackbars;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

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
//  JvMarkupLabel1.AutoSize := CheckBox1.Checked;
  if not CheckBox1.Checked then
  begin
    JvMarkupLabel1.Width := Self.Width-27;
    JvMarkupLabel1.Height := Self.Height - Label4.Top - 64;
  end;
  RecalibrateTrackbars;
end;

procedure TForm1.RecalibrateTrackbars;
begin
  JvTrackBar1.Max := JvMarkupLabel1.Width;
  JvTrackBar1.Position := JvMarkupLabel1.Width;
  JvTrackBar2.Max := JvMarkupLabel1.Height;
  JvTrackBar2.Position := JvMarkupLabel1.Height;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  RecalibrateTrackbars;
end;

end.

