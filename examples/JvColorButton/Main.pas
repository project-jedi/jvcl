unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvColorBox, Menus, Buttons,ExtCtrls, JvColorBtn,
  JvComponent;

type
  TForm2 = class(TForm)
    ColorButton1: TJvColorButton;
    procedure ColorBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ColorBox1ColorClick(Sender: TObject; Button: TMouseButton;
      Color: TColor);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses JvColorForm;

{$R *.DFM}


procedure TForm2.ColorBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
     if Button = mbLeft then
       Caption := Caption + ' Left '
     else
       Caption := Caption + ' Right '
end;


procedure TForm2.ColorBox1ColorClick(Sender: TObject; Button: TMouseButton;
  Color: TColor);
begin
  Caption := ColorToString(Color);

end;

end.
