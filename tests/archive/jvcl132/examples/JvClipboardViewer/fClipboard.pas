unit fClipboard;
interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, JvClipboardViewer, JvComponent;
type
  TForm1 = class(TForm)
    JvClipboardViewer1: TJvClipboardViewer;
    Label1: TLabel;
    Memo1: TMemo;
    Image1: TImage;
    Label2: TLabel;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; Text: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;
implementation
{$R *.DFM}
procedure TForm1.JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
begin
   self.Image1.Picture.Bitmap.Assign(Image);
end;
procedure TForm1.JvClipboardViewer1Text(Sender: TObject; Text: String);
begin
   self.Memo1.Lines.text:=Text;  
end;
end.
