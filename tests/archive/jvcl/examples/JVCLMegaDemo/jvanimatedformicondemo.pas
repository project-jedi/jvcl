unit jvanimatedformicondemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvFormAnimatedIcon;

type
  TfrAnimatedFormIcon = class(TForm)
    JvFormAnimatedIcon1: TJvFormAnimatedIcon;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.DFM}

procedure TfrAnimatedFormIcon.FormCreate(Sender: TObject);
begin
 JvFormAnimatedIcon1.Active := true;
end;

end.
