unit hshape;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, StdCtrls, glShape, glHShape, glBevel;

type
  TForm2 = class(TForm)
    FreeHoleShape2: TglHoleShape;
    FreeHoleShape3: TglHoleShape;
    FreeHoleShape4: TglHoleShape;
    FreeHoleShape5: TglHoleShape;
    FreeHoleShape6: TglHoleShape;
    FreeHoleShape10: TglHoleShape;
    FreeHoleShape7: TglHoleShape;
    FreeHoleShape9: TglHoleShape;
    FreeHoleShape11: TglHoleShape;
    FreeHoleShape12: TglHoleShape;
    FreeHoleShape14: TglHoleShape;
    FreeHoleShape15: TglHoleShape;
    FreeHoleShape16: TglHoleShape;
    glHoleShape1: TglHoleShape;
    glHoleShape3: TglHoleShape;
    FreeHoleShape13: TglHoleShape;
    glHoleShape2: TglHoleShape;
    glHoleShape4: TglHoleShape;
    glHoleShape5: TglHoleShape;
    glBevel1: TglBevel;
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
