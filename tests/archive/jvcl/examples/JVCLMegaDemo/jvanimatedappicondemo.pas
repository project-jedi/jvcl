unit jvanimatedappicondemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvAppAnimatedIcon;

type
  TfrAnimatedApplicationicon = class(TForm)
    JvAppAnimatedIcon1: TJvAppAnimatedIcon;
    ImageList1: TImageList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;


implementation

{$R *.DFM}

end.
