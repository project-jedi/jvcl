unit QuickPreviewU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, StdCtrls, JvDrawImage;

type
  TQuickPreviewF = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Panel1: TPanel;
    btnUse: TSpeedButton;
    procedure btnUseClick(Sender: TObject);
  private
    { Private declarations }
    PainterF: TJvDrawImage;
  public
    { Public declarations }
    procedure setDrawImage(ADrawImage: TJvDrawImage);
  end;

var
  QuickPreviewF: TQuickPreviewF;
  anifile: string;
  aniframe: integer;

implementation

{$R *.DFM}

procedure TQuickPreviewF.btnUseClick(Sender: TObject);

begin
  PainterF.canvas.draw(0, 0, image1.picture.bitmap);
  //PainterF.mypaint.Update ;
  close;
end;

procedure TQuickPreviewF.setDrawImage(ADrawImage: TJvDrawImage);
begin
  PainterF := aDrawImage;
end;

end.
