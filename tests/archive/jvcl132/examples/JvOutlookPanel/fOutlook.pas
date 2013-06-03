unit fOutlook;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvOutlookPanel, Menus;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Panels1: TMenuItem;
    Panel11: TMenuItem;
    Panel21: TMenuItem;
    Panel31: TMenuItem;
    JvOutlookPanel1: TJvOutlookPanel;
    JvOutlookPanel3: TJvOutlookPanel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    JvOutlookPanel2: TJvOutlookPanel;
    Button1: TButton;
    procedure Exit1Click(Sender: TObject);
    procedure Panels1Click(Sender: TObject);
    procedure Panel11Click(Sender: TObject);
    procedure Panel21Click(Sender: TObject);
    procedure Panel31Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.Panels1Click(Sender: TObject);
begin
  Panel11.Checked := JvOutlookPanel1.Visible;
  Panel21.Checked := JvOutlookPanel2.Visible;
  Panel31.Checked := JvOutlookPanel3.Visible;
end;

procedure TForm1.Panel11Click(Sender: TObject);
begin
  JvOutlookPanel1.Visible := not(JvOutlookPanel1.Visible);
end;

procedure TForm1.Panel21Click(Sender: TObject);
begin
  JvOutlookPanel2.Visible := not(JvOutlookPanel2.Visible);
end;

procedure TForm1.Panel31Click(Sender: TObject);
begin
  JvOutlookPanel3.Visible := not(JvOutlookPanel3.Visible);
end;

end.
