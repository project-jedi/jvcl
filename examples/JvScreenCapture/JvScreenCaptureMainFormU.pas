unit JvScreenCaptureMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ClipBrd;

type
  TJvScreenCaptureMainForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    TaskBar: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  end;

var
  JvScreenCaptureMainForm: TJvScreenCaptureMainForm;

implementation

{$R *.DFM}

uses
  JclGraphics;

procedure TJvScreenCaptureMainForm.Button1Click(Sender: TObject);
var
  B: TBitmap;
begin
   b := TBitmap.Create;
   ScreenShot(B, TaskBar.Checked);
   Self.Image1.Picture.Bitmap.Assign(B);
   B.Free;
end;

procedure TJvScreenCaptureMainForm.Button2Click(Sender: TObject);
var
  AFormat: Word;
  AData: THandle;
  APalette: HPALETTE;
begin
   Self.Image1.Picture.Bitmap.SaveToClipboardFormat(AFormat, AData, APalette);
   ClipBoard.SetAsHandle(AFormat, AData);
end;

end.
