unit jvanimatedappicondemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, JvComponent, JvAppAnimatedIcon, StdCtrls;

type
  TfrAnimatedApplicationicon = class(TForm)
    JvAppAnimatedIcon1: TJvAppAnimatedIcon;
    ImageList1: TImageList;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;


implementation

{$R *.DFM}

procedure TfrAnimatedApplicationicon.Button1Click(Sender: TObject);
begin
 if JvAppAnimatedIcon1.Tag = 0 then
 begin
   JvAppAnimatedIcon1.Active := true;
   JvAppAnimatedIcon1.Tag := 1;
   Button1.Caption := 'stop that now!'
 end
 else
 begin
   JvAppAnimatedIcon1.Active := false;
   JvAppAnimatedIcon1.Tag := 0;
   Button1.Caption := 'start the animation!'
 end;
end;

end.
