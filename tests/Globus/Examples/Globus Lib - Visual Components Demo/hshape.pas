unit hshape;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, JvgShape, JvgHoleShape, JvgBevel;

type
  TForm2 = class(TForm)
    FreeHoleShape2: TJvgHoleShape;
    FreeHoleShape3: TJvgHoleShape;
    FreeHoleShape4: TJvgHoleShape;
    FreeHoleShape5: TJvgHoleShape;
    FreeHoleShape6: TJvgHoleShape;
    FreeHoleShape10: TJvgHoleShape;
    FreeHoleShape7: TJvgHoleShape;
    FreeHoleShape9: TJvgHoleShape;
    FreeHoleShape11: TJvgHoleShape;
    FreeHoleShape12: TJvgHoleShape;
    FreeHoleShape14: TJvgHoleShape;
    FreeHoleShape15: TJvgHoleShape;
    FreeHoleShape16: TJvgHoleShape;
    glHoleShape1: TJvgHoleShape;
    glHoleShape3: TJvgHoleShape;
    FreeHoleShape13: TJvgHoleShape;
    glHoleShape2: TJvgHoleShape;
    glHoleShape4: TJvgHoleShape;
    glHoleShape5: TJvgHoleShape;
    glBevel1: TJvgBevel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
implementation

{$R *.DFM}

procedure TForm2.FormShow(Sender: TObject);
var    RGN, RGN2, oldRGN: HRGN;
begin
//  glHoleShape2.ShapeBitmap:=Image.Picture.Bitmap;
//  RGN := CreateRectRgn( 0, 0, 100, 100 );
//  CombineRgn(RGN, glHoleShape2.RGNInner, glHoleShape1.RGNInner,  RGN_OR );
end;

end.
