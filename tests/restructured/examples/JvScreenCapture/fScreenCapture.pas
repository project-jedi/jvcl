unit fScreenCapture;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, clipbrd;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  JvFunctions;
{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var b:TBitmap;
begin
   b := CaptureScreen;
   self.image1.picture.bitmap.assign(b);
   b.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  MyFormat : Word;
  AData: THandle;
  APalette:HPalette;
begin
   self.image1.picture.bitmap.SaveToClipboardFormat(MyFormat,AData,APalette);
   ClipBoard.SetAsHandle(MyFormat,AData);
end;

end.
