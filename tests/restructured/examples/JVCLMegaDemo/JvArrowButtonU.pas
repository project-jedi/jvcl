unit JvArrowButtonU;

interface               

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, JvComponent, JvArrowBtn, ExtCtrls, JvCaptionPanel;

type
  TJvArrowButtonFrm = class(TFrame)
    JvCaptionPanel1: TJvCaptionPanel;
    ArrowButton1: TJvArrowButton;
    ArrowButton2: TJvArrowButton;
    PopupMenu1: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    Replace1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
