unit MainFrm2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, jpeg, ImgList, JvItemViewer;

type
  TForm1 = class(TForm)
    JvImageFilesViewer1: TJvImageFilesViewer;
    JvImageListViewer1: TJvImageListViewer;
    ImageList1: TImageList;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  JvImageFilesViewer1.LoadImages;
end;

end.
