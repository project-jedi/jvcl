unit JvButtonsU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JvGIF, ExtCtrls, JvImage, ImgList, ComCtrls, JvArrowBtn, JvComponent,
  JvTransBtn2, StdCtrls;

type
  TJvButtons = class(TFrame)
    JvTransparentButton21: TJvTransparentButton2;
    JvArrowButton1: TJvArrowButton;
    ListView1: TListView;
    ilTreeview: TImageList;
    JvImageJEDI: TJvImage;
    CheckBoxImage: TCheckBox;
    procedure CheckBoxImageClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TJvButtons.CheckBoxImageClick(Sender: TObject);
begin

 JvImageJEDI.visible := CheckBoxImage.Checked;
end;

end.
