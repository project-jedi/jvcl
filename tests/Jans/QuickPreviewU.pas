unit QuickPreviewU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, janDrawImage;

type
  TQuickPreviewF = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    btnUse: TSpeedButton;
    procedure btnUseClick(Sender: TObject);
  private
    { Private declarations }
    PainterF:TjanDrawImage;
  public
    { Public declarations }
    procedure setDrawImage(ADrawImage:TjanDrawImage);
  end;

var
  QuickPreviewF: TQuickPreviewF;
  anifile:string;
  aniframe:integer;

implementation

{$R *.DFM}



procedure TQuickPreviewF.btnUseClick(Sender: TObject);

begin
  PainterF.canvas.draw(0,0,image1.picture.bitmap);
  //PainterF.mypaint.Update ;
  close;
end;



procedure TQuickPreviewF.setDrawImage(ADrawImage: TjanDrawImage);
begin
  PainterF:=aDrawImage;
end;

end.
