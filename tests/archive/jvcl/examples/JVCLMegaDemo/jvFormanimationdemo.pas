unit jvFormanimationdemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvFormAnimation, StdCtrls;

type
  TfrFormAnimation = class(TForm)
    JvFormAnimation1: TJvFormAnimation;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


implementation

{$R *.DFM}

procedure TfrFormAnimation.Button1Click(Sender: TObject);
begin
  JvFormAnimation1.AppearEllipse;
end;

procedure TfrFormAnimation.Button2Click(Sender: TObject);
begin
  JvFormAnimation1.AppearRectangle;
end;

procedure TfrFormAnimation.Button3Click(Sender: TObject);
begin
  JvFormAnimation1.AppearRoundedRectangle(20, 20);
end;

procedure TfrFormAnimation.Button4Click(Sender: TObject);
begin
  JvFormAnimation1.AppearHorizontally;
end;

procedure TfrFormAnimation.Button5Click(Sender: TObject);
begin
  JvFormAnimation1.AppearVertically;
end;

procedure TfrFormAnimation.Button6Click(Sender: TObject);
begin
  JvFormAnimation1.AppearTelevision;
end;

procedure TfrFormAnimation.Button7Click(Sender: TObject);
begin
  JvFormAnimation1.AppearToTop;
end;

procedure TfrFormAnimation.Button8Click(Sender: TObject);
begin
  JvFormAnimation1.AppearToBottom;
end;

end.

