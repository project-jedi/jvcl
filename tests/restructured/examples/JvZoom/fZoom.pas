unit fZoom;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, JvZoom;
  
type
  TForm1 = class(TForm)
    JvZoom1: TJvZoom;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  JvZoom1.active := checkbox1.checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel div 2;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  JvZoom1.ZoomLevel := JvZoom1.ZoomLevel*2;
end;

end.
