unit JvScreenCaptureMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, clipbrd;

type
  TJvScreenCaptureMainForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  JvScreenCaptureMainForm: TJvScreenCaptureMainForm;

implementation

{$R *.DFM}

uses
  JvFunctions;

procedure TJvScreenCaptureMainForm.Button1Click(Sender: TObject);
var
  b : TBitmap;
begin
   b := CaptureScreen;
   self.image1.picture.bitmap.assign(b);
   b.Free;
end;

procedure TJvScreenCaptureMainForm.Button2Click(Sender: TObject);
var
  MyFormat : Word;
  AData: THandle;
  APalette:HPalette;
begin
   self.image1.picture.bitmap.SaveToClipboardFormat(MyFormat,AData,APalette);
   ClipBoard.SetAsHandle(MyFormat,AData);
end;

end.
