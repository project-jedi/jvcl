unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  ExtCtrls, StdCtrls, JvCntScr, JvComponent;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ContentScroller1: TJvContentScroller;
    Image11: TImage;
    chkGoDown: TCheckBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if chkGoDown.Checked then
    ContentScroller1.ScrollDirection := sdDown
  else
    ContentScroller1.ScrollDirection := sdUp;
  ContentScroller1.Active := not ContentScroller1.Active;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ContentScroller1.Active := false;
end;

end.
