unit JvButtonsU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvGIF, ExtCtrls, JvImage, ImgList, ComCtrls, JvArrowBtn, JvComponent,
  JvTransBtn, StdCtrls, JvButton, Buttons, JvBitBtn;

type
  TJvButtons = class(TForm)
    JvTransparentButton21: TJvTransparentButton2;
    JvArrowButton1: TJvArrowButton;
    ilTreeview: TImageList;
    JvImageJEDI: TJvImage;
    CheckBoxImage: TCheckBox;
    JvBitBtn1: TJvBitBtn;
    Label1: TLabel;
    procedure CheckBoxImageClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

implementation

uses
  Unitmain;

{$R *.DFM}

procedure TJvButtons.CheckBoxImageClick(Sender: TObject);
begin
 JvImageJEDI.visible := CheckBoxImage.Checked;
end;

procedure TJvButtons.Button1Click(Sender: TObject);
begin
  Mainform.CreateDemoForm(63);
end;

end.
