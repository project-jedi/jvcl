unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, Buttons, JvArrowBtn, ComCtrls, StdCtrls, ImgList,
  JvComponent;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    Edit1: TMenuItem;
    Replace1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    ArrowButton1: TJvArrowButton;
    ArrowButton2: TJvArrowButton;
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
{$R *.DFM}




end.
