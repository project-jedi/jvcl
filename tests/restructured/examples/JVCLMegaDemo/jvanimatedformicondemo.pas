unit jvanimatedformicondemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvFormAnimatedIcon;

type
  TfrAnimatedFormIcon = class(TForm)
    JvFormAnimatedIcon1: TJvFormAnimatedIcon;
    ImageList1: TImageList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

end.
