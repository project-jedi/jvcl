unit JvClipboardViewerU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvComponent, JvClipboardViewer, StdCtrls, ExtCtrls, JvCaptionPanel;

type
  TJvClipboardViewerFrm = class(TFrame)
    JvCaptionPanel1: TJvCaptionPanel;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Memo1: TMemo;
    JvClipboardViewer1: TJvClipboardViewer;
    procedure JvClipboardViewer1Image(Sender: TObject; Image: TBitmap);
    procedure JvClipboardViewer1Text(Sender: TObject; Text: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvClipboardViewerFrm.JvClipboardViewer1Image(Sender: TObject;
  Image: TBitmap);
begin
   self.Image1.Picture.Bitmap.Assign(Image);
end;

procedure TJvClipboardViewerFrm.JvClipboardViewer1Text(Sender: TObject;
  Text: String);
begin
   self.Memo1.Lines.text:=Text;
end;

end.
