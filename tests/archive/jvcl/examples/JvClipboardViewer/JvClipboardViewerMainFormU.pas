unit JvClipboardViewerMainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, JvClipView, JvComponent;

type
  TJvClipboardViewerMainForm = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Label1: TLabel;
    Memo1: TMemo;
    Image1: TImage;
    Label2: TLabel;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; AText: String);
  end;

var
  JvClipboardViewerMainForm: TJvClipboardViewerMainForm;

implementation

{$R *.DFM}

procedure TJvClipboardViewerMainForm.JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
begin
   self.Image1.Picture.Bitmap.Assign(Image);
end;

procedure TJvClipboardViewerMainForm.JvClipboardViewer1Text(Sender: TObject; AText: String);
begin
   self.Memo1.Lines.text:=Text;
end;

end.
