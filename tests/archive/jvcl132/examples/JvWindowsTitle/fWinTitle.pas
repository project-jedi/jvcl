unit fWinTitle;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvWindowsTitle, JvComponent;

type
  TForm1 = class(TForm)
    JvWindowsTitle1: TJvWindowsTitle;
    Button1: TButton;
    ListBox1: TListBox;
    procedure JvWindowsTitle1List(Sender: TObject; Title: String;
      Handle: Cardinal);
    procedure JvWindowsTitle1BeforeList(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.JvWindowsTitle1List(Sender: TObject; Title: String;
  Handle: Cardinal);
begin
  ListBox1.Items.add(Format('%s : %d',[Title,Handle]));
end;

procedure TForm1.JvWindowsTitle1BeforeList(Sender: TObject);
begin
  ListBox1.clear;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvWindowsTitle1.Update;
end;

end.
